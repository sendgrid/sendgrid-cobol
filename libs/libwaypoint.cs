using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Globalization;
using System.Linq;
using System.Net;
using Branch.Core.Api.Authentication;
using Branch.Core.Storage;
using Branch.Models.Authentication;
using Branch.Models.Services.Branch;
using Branch.Models.Services.Halo4;
using Branch.Models.Services.Halo4.Branch;
using Branch.Models.Services.Halo4._343;
using Branch.Models.Services.Halo4._343.Responses;
using EasyHttp.Http;
using EasyHttp.Infrastructure;
using Microsoft.WindowsAzure.Storage.Blob;
using Newtonsoft.Json;
using RestSharp.Contrib;
using Branch343DataModels = Branch.Models.Services.Halo4._343.DataModels;

namespace Branch.Core.Api.Halo4
{
	public class WaypointManager
	{
		private const string RegisterWebAppLocation =
			"https://settings.svc.halowaypoint.com/RegisterClientService.svc/register/webapp/AE5D20DCFA0347B1BCE0A5253D116752";

		private const string Language = "en-US";
		private const string Game = "h4";
		private readonly AzureStorage _storage;

		public RegisterWebApp RegisteredWebApp { get; private set; }
		public Metadata Metadata { get; private set; }
		public Playlist Playlists { get; private set; }
		public Challenge Challenges { get; private set; }


		public WaypointManager(AzureStorage storage, bool updateAuthentication = false)
		{
			_storage = storage;
			RegisterWebApp();

			if (updateAuthentication)
				I343.UpdateAuthentication(_storage);

			GetMetadata();
			GetPlaylists();
			GetChallenges();
		}


		#region Setup Waypoint Manager

		/// <summary>
		/// 
		/// </summary>
		public void RegisterWebApp()
		{
			var response = UnauthorizedRequest(RegisterWebAppLocation);

			if (response.StatusCode == HttpStatusCode.OK && !String.IsNullOrEmpty(response.RawText))
			{
				try
				{
					RegisteredWebApp = JsonConvert.DeserializeObject<RegisterWebApp>(response.RawText);
				}
				catch (JsonReaderException jsonReaderException)
				{
					Trace.TraceError(jsonReaderException.ToString());
#if DEBUG
					throw;
#endif
				}
			}
			else
			{
				Trace.TraceError("Unable to register web application.");
#if DEBUG
				throw new HttpException(response.StatusCode, "fuk, can't register web app.");
#endif
			}
		}

		/// <summary>
		/// 
		/// </summary>
		public void GetMetadata()
		{
			var metadataBlob = _storage.Blob.GetBlob(_storage.Blob.H4BlobContainer,
				GenerateBlobContainerPath(BlobType.Other, "metadata"));
			var metadata = _storage.Blob.FindAndDownloadBlob<Metadata>(_storage.Blob.H4BlobContainer,
				GenerateBlobContainerPath(BlobType.Other, "metadata"));

			if (metadataBlob == null || metadata == null || 
				metadataBlob.Properties.LastModified + TimeSpan.FromMinutes(14) < DateTime.UtcNow)
				UpdateMetadata();
			else
				Metadata = metadata;
		}

		/// <summary>
		/// 
		/// </summary>
		public void GetPlaylists()
		{
			var playlists = _storage.Blob.FindAndDownloadBlob<Playlist>(_storage.Blob.H4BlobContainer,
				GenerateBlobContainerPath(BlobType.Other, "playlists"));

			if (playlists == null)
				UpdatePlaylists();
			else
				Playlists = playlists;
		}

		/// <summary>
		/// 
		/// </summary>
		public void GetChallenges()
		{
			var challenges = _storage.Blob.FindAndDownloadBlob<Challenge>(_storage.Blob.H4BlobContainer,
				GenerateBlobContainerPath(BlobType.Other, "challenges"));

			if (challenges == null)
				UpdateChallenges();
			else
				Challenges = challenges;
		}

		#endregion


		#region Player Endpoints

		/// <summary>
		/// Gets a Players Halo 4 Service Record
		/// </summary>
		/// <param name="gamertag">The players Xbox 360 Gamertag.</param>
		/// <returns>The raw JSON of their Service Record</returns>
		public ServiceRecord GetServiceRecord(string gamertag)
		{
			const BlobType blobType = BlobType.PlayerServiceRecord;
			var escapedGamertag = EscapeGamertag(gamertag);
			var blobContainerPath = GenerateBlobContainerPath(blobType, escapedGamertag);
			var blob = _storage.Blob.GetBlob(_storage.Blob.H4BlobContainer, blobContainerPath);
			var blobValidity = CheckBlobValidity<ServiceRecord>(blob, new TimeSpan(0, 8, 0));

			// Check if blob exists & expire date
			if (blobValidity.Item1) return blobValidity.Item2;

			// Try and get new blob
			var url = PopulateUrl(UrlFromIds(EndpointType.ServiceList, "GetServiceRecord"),
				new Dictionary<string, string> {{"gamertag", gamertag}});
			var serviceRecordRaw = ValidateResponseAndGetRawText(UnauthorizedRequest(url));
			var serviceRecord = ParseText<ServiceRecord>(serviceRecordRaw);
			if (serviceRecord == null) return blobValidity.Item2;

			_storage.Blob.UploadBlob(_storage.Blob.H4BlobContainer,
				GenerateBlobContainerPath(blobType, escapedGamertag), serviceRecordRaw);

			var serviceRecordEntity = JsonConvert.DeserializeObject<ServiceRecordEntity>(serviceRecordRaw);
			if (serviceRecordEntity.PartitionKey == null || serviceRecordEntity.RowKey == null) serviceRecordEntity.SetKeys(null, gamertag);

			var warGamesMode = serviceRecord.GameModes.FirstOrDefault(m => m.Id == Branch343DataModels.Enums.GameMode.WarGames);
			if (warGamesMode != null)
			{
				serviceRecordEntity.WarGamesKills = warGamesMode.TotalKills;
				serviceRecordEntity.WarGamesDeaths = warGamesMode.TotalDeaths;
				serviceRecordEntity.WarGamesGames = warGamesMode.TotalGamesStarted;
				serviceRecordEntity.WarGamesMedals = warGamesMode.TotalMedals ?? 0;
				serviceRecordEntity.WarGamesDuration = warGamesMode.TotalDuration ?? 
					TimeSpan.FromTicks(0).ToString();
			}

			_storage.Table.InsertOrReplaceSingleEntity(serviceRecordEntity, _storage.Table.Halo4CloudTable);

			AddPlayerToStorage(gamertag);

			return serviceRecord;
		}

		/// <summary>
		/// 
		/// </summary>
		/// <param name="gamertag"></param>
		/// <param name="startIndex"></param>
		/// <param name="count"></param>
		/// <param name="mode"></param>
		/// <param name="chapterId"></param>
		/// <returns></returns>
		public GameHistory<T> GetPlayerGameHistory<T>(string gamertag, int startIndex = 0, int count = 5,
			Branch343DataModels.Enums.GameMode mode = Branch343DataModels.Enums.GameMode.WarGames, int chapterId = -1)
			where T : Branch343DataModels.GameHistoryModel.Base
		{
			const BlobType blobType = BlobType.PlayerGameHistory;
			var escapedGamertag = EscapeGamertag(gamertag);
			var gameHistoryNameFormat = string.Format("{0}-{1}-{2}-{3}-{4}", escapedGamertag, startIndex, count, (int) mode,
				chapterId);
			var blobContainerPath = GenerateBlobContainerPath(blobType, gameHistoryNameFormat);
			var blob = _storage.Blob.GetBlob(_storage.Blob.H4BlobContainer, blobContainerPath);
			var blobValidity = CheckBlobValidity <GameHistory<T>>(blob, new TimeSpan(0, 8, 0));

			// Check if blob exists & expire date
			if (blobValidity.Item1) return blobValidity.Item2;

			// Try and get new blob
			var customUrlParams = new Dictionary<string, string> {{"gamertag", gamertag}};
			var customQueryParams = new Dictionary<string, string>
			{
				{"gamemodeid", ((int) mode).ToString(CultureInfo.InvariantCulture)},
				{"count", count.ToString(CultureInfo.InvariantCulture)},
				{"startat", startIndex.ToString(CultureInfo.InvariantCulture)}
			};
			if (chapterId != -1) customQueryParams.Add("chapterid", chapterId.ToString(CultureInfo.InvariantCulture));
			var url = PopulateUrl(UrlFromIds(EndpointType.ServiceList, "GetGameHistory"), customUrlParams, customQueryParams);
			var gameHistoryRaw = ValidateResponseAndGetRawText(AuthorizedRequest(url, AuthType.Spartan));
			var gameHistory = ParseText<GameHistory<T>>(gameHistoryRaw);
			if (gameHistory == null) return blobValidity.Item2;

			_storage.Blob.UploadBlob(_storage.Blob.H4BlobContainer,
				GenerateBlobContainerPath(blobType, gameHistoryNameFormat), gameHistoryRaw);

			AddPlayerToStorage(gamertag);

			return gameHistory;
		}

		/// <summary>
		/// 
		/// </summary>
		/// <param name="gamertag"></param>
		/// <param name="id"></param>
		/// <returns></returns>
		public ElapsedGame GetPlayerGame(string gamertag, string id)
		{
			const BlobType blobType = BlobType.PlayerGame;
			var gameHistoryNameFormat = id;
			var blobContainerPath = GenerateBlobContainerPath(blobType, gameHistoryNameFormat);
			var blob = _storage.Blob.GetBlob(_storage.Blob.H4BlobContainer, blobContainerPath);
			var blobValidity = CheckBlobValidity<ElapsedGame>(blob, new TimeSpan(36500, 0, 0, 0));

			// Check if blob exists & expire date
			if (blobValidity.Item1) return blobValidity.Item2;

			var customUrlParams = new Dictionary<string, string> { { "gamertag", gamertag} , {"gameid", id } };
			var url = PopulateUrl(UrlFromIds(EndpointType.ServiceList, "GetGameDetails"), customUrlParams);
			var gameRaw = ValidateResponseAndGetRawText(AuthorizedRequest(url, AuthType.Spartan));
			var game = ParseText<ElapsedGame>(gameRaw);
			if (game == null) return blobValidity.Item2;

			_storage.Blob.UploadBlob(_storage.Blob.H4BlobContainer,
				GenerateBlobContainerPath(blobType, gameHistoryNameFormat), gameRaw);

			AddPlayerToStorage(gamertag);

			return game;
		}

		/// <summary>
		/// 
		/// </summary>
		/// <param name="gamertag"></param>
		/// <returns></returns>
		public Commendation GetPlayerCommendations(string gamertag)
		{
			const BlobType blobType = BlobType.PlayerCommendation;
			var escapedGamertag = EscapeGamertag(gamertag);
			var blobContainerPath = GenerateBlobContainerPath(blobType, escapedGamertag);
			var blob = _storage.Blob.GetBlob(_storage.Blob.H4BlobContainer, blobContainerPath);
			var blobValidity = CheckBlobValidity<Commendation>(blob, new TimeSpan(0, 8, 0));

			// Check if blob exists & expire date
			if (blobValidity.Item1) return blobValidity.Item2;

			var url = PopulateUrl(UrlFromIds(EndpointType.ServiceList, "GetCommendations"),
				new Dictionary<string, string> {{"gamertag", gamertag}});
			var commendationRaw = ValidateResponseAndGetRawText(AuthorizedRequest(url, AuthType.Spartan));
			var commendation = ParseText<Commendation>(commendationRaw);
			if (commendation == null) return blobValidity.Item2;

			_storage.Blob.UploadBlob(_storage.Blob.H4BlobContainer,
				GenerateBlobContainerPath(blobType, escapedGamertag), commendationRaw);

			AddPlayerToStorage(gamertag);

			return commendation;
		}

		#endregion

		#region Misc Endpoints

		/// <summary>
		/// 
		/// </summary>
		public void UpdateMetadata()
		{
			Metadata = UpdateOther<Metadata>("metadata", "GetGameMetadata");
		}

		/// <summary>
		/// 
		/// </summary>
		public void UpdatePlaylists()
		{
			Playlists = UpdateOther<Playlist>("playlists", "GetPlaylists");
		}

		/// <summary>
		/// 
		/// </summary>
		public void UpdateChallenges()
		{
			Challenges = UpdateOther<Challenge>("challenges", "GetGlobalChallenges");
		}

		/// <summary>
		/// 
		/// </summary>
		/// <typeparam name="T"></typeparam>
		/// <param name="blobFileName"></param>
		/// <param name="serviceListName"></param>
		/// <returns></returns>
		private T UpdateOther<T>(string blobFileName, string serviceListName)
			where T : WaypointResponse
		{
			var blobPath = GenerateBlobContainerPath(BlobType.Other, blobFileName);
			var otherData = _storage.Blob.FindAndDownloadBlob<T>(_storage.Blob.H4BlobContainer, blobPath);

			if (otherData == null)
			{
				var otherDataString =
					ValidateResponseAndGetRawText(
						AuthorizedRequest(PopulateUrl(UrlFromIds(EndpointType.ServiceList, serviceListName)), AuthType.Spartan));

				if (otherDataString == null)
					return null;

				// Save
				_storage.Blob.UploadBlob(_storage.Blob.H4BlobContainer, GenerateBlobContainerPath(BlobType.Other, blobFileName),
					otherDataString);

				otherData = ParseText<T>(otherDataString);
			}
			return otherData;
		}

		#endregion

		#region Unauthorized Request

		private HttpResponse UnauthorizedRequest(String url)
		{
// ReSharper disable once IntroduceOptionalParameters.Local
			return UnauthorizedRequest(url, HttpMethod.GET);
		}

		private HttpResponse UnauthorizedRequest(String url, HttpMethod requestType)
		{
			return UnauthorizedRequest(url, requestType, new Dictionary<String, String>());
		}

		private HttpResponse UnauthorizedRequest(String url, HttpMethod requestType, Dictionary<String, String> headers)
		{
			if (headers == null)
				headers = new Dictionary<string, string>();

			var httpClient = new HttpClient();
			httpClient.Request.Accept = "application/json";

			foreach (var header in headers)
				httpClient.Request.AddExtraHeader(header.Key, header.Value);

			switch (requestType)
			{
				case HttpMethod.GET:
					return httpClient.Get(url);

				default:
					throw new ArgumentException();
			}
		}

		#endregion

		#region Authorized Request

		private HttpResponse AuthorizedRequest(String url, AuthType authType)
		{
// ReSharper disable once IntroduceOptionalParameters.Local
			return AuthorizedRequest(url, authType, HttpMethod.GET);
		}

		private HttpResponse AuthorizedRequest(String url, AuthType authType, HttpMethod requestType)
		{
			return AuthorizedRequest(url, authType, requestType, new Dictionary<string, string>());
		}

		private HttpResponse AuthorizedRequest(String url, AuthType authType, HttpMethod requestType,
			Dictionary<String, String> headers)
		{
			if (headers == null)
				headers = new Dictionary<string, string>();

			// get auth
			var auth = _storage.Table.RetrieveSingleEntity<WaypointTokenEntity>("Authentication",
				WaypointTokenEntity.FormatRowKey(),
				_storage.Table.AuthenticationCloudTable);

			switch (authType)
			{
				case AuthType.Spartan:
					headers.Add("X-343-Authorization-Spartan", auth == null ? "" : auth.SpartanToken); // error catch
					break;

				default:
					throw new ArgumentException();
			}

			return UnauthorizedRequest(url, requestType, headers);
		}

		#endregion

		#region Api Helpers

		/// <summary>
		///     Returns a URL from a key and endpoint type.
		/// </summary>
		/// <param name="endpointType">The type of endpoint you need to call (ie; ServiceList)</param>
		/// <param name="key">The key url in that endpoint.</param>
		/// <returns>A string representation of the url.</returns>
		private string UrlFromIds(EndpointType endpointType, string key)
		{
			switch (endpointType)
			{
				case EndpointType.ServiceList:
					return RegisteredWebApp.ServiceList[key];

				case EndpointType.Settings:
					return RegisteredWebApp.Settings[key];

				default:
					throw new ArgumentException();
			}
		}

		/// <summary>
		///     Populates a url with the default params populated.
		/// </summary>
		/// <param name="url">The url to populate.</param>
		/// <returns>A string representation of the populated url</returns>
		private static string PopulateUrl(string url)
		{
			return PopulateUrl(url, new Dictionary<string, string>());
		}

		/// <summary>
		///     Populates a url with the default params populated.
		/// </summary>
		/// <param name="url">The url to populate.</param>
		/// <param name="customDefaults">Custom params to populate the url with, auto wrapped in the {} brackets.</param>
		/// <returns>A string representation of the populated url</returns>
		private static string PopulateUrl(string url, Dictionary<string, string> customDefaults)
		{
			return PopulateUrl(url, customDefaults, new Dictionary<string, string>());
		}

		/// <summary>
		///     Populates a url with the default params populated, and also populates custom params.
		/// </summary>
		/// <param name="url">The url to populate.</param>
		/// <param name="customDefaults">Custom params to populate the url with, auto wrapped in the {} brackets.</param>
		/// <param name="queryParams"></param>
		/// <returns>A string representation of the populated url</returns>
		private static string PopulateUrl(string url, Dictionary<string, string> customDefaults,
			Dictionary<string, string> queryParams)
		{
			url = url.Replace("{language}", Language);
			url = url.Replace("{game}", Game);

			if (customDefaults == null)
				throw new ArgumentException("Custom Defaults can't be null");
			if (queryParams == null)
				throw new ArgumentException("Query Params can't be null");

			var startOfParams = !url.Contains("?");
			foreach (var queryParam in queryParams)
			{
				url += startOfParams ? "?" : "&";
				url += string.Format("{0}={1}", queryParam.Key, HttpUtility.HtmlEncode(queryParam.Value));

				startOfParams = false;
			}

			return customDefaults.Aggregate(url,
				(current, customDefault) => current.Replace("{" + customDefault.Key + "}", customDefault.Value));
		}

		/// <summary>
		///     Checks is a HttpResponse is valid or not.
		/// </summary>
		/// <param name="response">The HttpResponse</param>
		/// <returns>Boolean representation of the validity of the response.</returns>
		private static bool ValidateResponse(HttpResponse response)
		{
			if (response == null || response.StatusCode != HttpStatusCode.OK || String.IsNullOrEmpty(response.RawText))
				return false;

			var parsedResponse = JsonConvert.DeserializeObject<WaypointResponse>(response.RawText);
			return (parsedResponse != null &&
			        (parsedResponse.StatusCode == ResponseCode.Okay ||
			         parsedResponse.StatusCode == ResponseCode.PlayerFound));
		}

		/// <summary>
		///     Checks is a HttpResponse is valid or not, and if not returns the Raw Text.
		/// </summary>
		/// <param name="response">The HttpResponse</param>
		private static string ValidateResponseAndGetRawText(HttpResponse response)
		{
			return !ValidateResponse(response) ? null : response.RawText;
		}

		/// <summary>
		///     Checks is a HttpResponse is valid or not, and parses it into a model
		/// </summary>
		/// <param name="response">The HttpResponse we are checking and parsing</param>
		/// <returns>Returns null if the response is not valid, and the parsed model if it is.</returns>
		// ReSharper disable once UnusedMember.Local
		private static TModelType ValidateAndParseResponse<TModelType>(HttpResponse response)
			where TModelType : WaypointResponse
		{
			if (!ValidateResponse(response))
				return null;

			try
			{
				return JsonConvert.DeserializeObject<TModelType>(response.RawText);
			}
			catch (JsonReaderException jsonReaderException)
			{
				Trace.TraceError(jsonReaderException.ToString());
			}

			return null;
		}

		/// <summary>
		/// </summary>
		/// <returns></returns>
		public bool CheckApiValidity()
		{
			var auth = _storage.Table.RetrieveSingleEntity<WaypointTokenEntity>("Authentication",
				WaypointTokenEntity.FormatRowKey(),
				_storage.Table.AuthenticationCloudTable);

			if (auth == null)
				return false;

			return (auth.SpartanToken != null);
		}

		#endregion

		#region Enums

		public enum AuthType
		{
			Spartan
		}

		public enum EndpointType
		{
			ServiceList,
			Settings
		}

		public enum BlobType
		{
			Other,
			PlayerServiceRecord,
			PlayerGameHistory,
			PlayerGame,
			PlayerCommendation
		}

		#endregion

		#region Public Helpers

		public string GetPlayerModelUrl(string gamertag, string size = "large", string pose = "fullbody")
		{
			return PopulateUrl(RegisteredWebApp.ServiceList["GetSpartanImage"], new Dictionary<string, string>
			{
				{"gamertag", gamertag},
				{"size", size},
				{"pose", pose}
			});
		}

		#endregion

		#region General Helpers

		/// <summary>
		/// 
		/// </summary>
		/// <typeparam name="TBlam"></typeparam>
		/// <param name="jsonData"></param>
		/// <returns></returns>
		public TBlam ParseText<TBlam>(string jsonData)
			where TBlam : WaypointResponse
		{
			if (jsonData == null) return null;

#if DEBUG
			return JsonConvert.DeserializeObject<TBlam>(jsonData);
#else
			try
			{
				return JsonConvert.DeserializeObject<TBlam>(jsonData);
			}
			catch (JsonReaderException jsonReaderException)
			{
				return null;
			}
#endif
		}

		/// <summary>
		/// 
		/// </summary>
		/// <param name="blobType"></param>
		/// <param name="fileName"></param>
		/// <returns></returns>
		public string GenerateBlobContainerPath(BlobType blobType, string fileName)
		{
			string path;

			switch (blobType)
			{
				case BlobType.Other:
					path = "other";
					break;

				case BlobType.PlayerCommendation:
					path = "player-commendation";
					break;

				case BlobType.PlayerGame:
					path = "player-game";
					break;

				case BlobType.PlayerGameHistory:
					path = "player-game-history";
					break;

				case BlobType.PlayerServiceRecord:
					path = "player-service-record";
					break;

				default:
					throw new ArgumentException("Invalid/Unknown Blob Type");
			}

			return string.Format("{0}/{1}.json", path, fileName);
		}

		/// <summary>
		/// 
		/// </summary>
		/// <param name="gamertag"></param>
		/// <returns></returns>
		public string EscapeGamertag(string gamertag)
		{
			gamertag = gamertag.ToLower();
			gamertag = gamertag.Replace(" ", "-");
			return gamertag;
		}

		/// <summary>
		/// 
		/// </summary>
		/// <typeparam name="TDataModel"></typeparam>
		/// <param name="blob"></param>
		/// <param name="expireLength"></param>
		/// <returns></returns>
		public Tuple<bool, TDataModel> CheckBlobValidity<TDataModel>(ICloudBlob blob, TimeSpan expireLength)
			where TDataModel : WaypointResponse
		{
			if (blob == null || !blob.Exists())
				return new Tuple<bool, TDataModel>(false, null);

			var blobData = _storage.Blob.DownloadBlob<TDataModel>(blob);
			if (blobData == null) return new Tuple<bool, TDataModel>(false, null);

			if (blob.Properties.LastModified == null || DateTime.UtcNow > blob.Properties.LastModified + expireLength)
				return new Tuple<bool, TDataModel>(false, null);

			return new Tuple<bool, TDataModel>(true, blobData);
		}

		#endregion

		#region Branch Data Management

		/// <summary>
		/// 
		/// </summary>
		/// <param name="gamertag"></param>
		private void AddPlayerToStorage(string gamertag)
		{
			_storage.Table.InsertOrReplaceSingleEntity(
				new GamerIdEntity(gamertag, Models.Services.Branch.Enums.GamerId.X360XblGamertag), _storage.Table.BranchCloudTable);
		}

		/// <summary>
		/// 
		/// </summary>
		/// <param name="playlistName"></param>
		/// <param name="playlistId"></param>
		/// <param name="isTeam"></param>
		public void SetPlaylistOrientation(string playlistName, int playlistId, bool isTeam)
		{
			var playlistOrientationEntity = new PlaylistOrientationEntity(playlistName, playlistId, isTeam);
			_storage.Table.InsertOrReplaceSingleEntity(playlistOrientationEntity, _storage.Table.Halo4CloudTable);
		}

		/// <summary>
		/// 
		/// </summary>
		/// <param name="playlistId"></param>
		/// <returns></returns>
		public PlaylistOrientationEntity GetPlaylistOrientation(int playlistId)
		{
			return _storage.Table.RetrieveSingleEntity<PlaylistOrientationEntity>(PlaylistOrientationEntity.PartitionKeyString,
				string.Format(PlaylistOrientationEntity.RowKeyString, playlistId), _storage.Table.Halo4CloudTable);
		}

		/// <summary>
		/// 
		/// </summary>
		/// <returns></returns>
		public IEnumerable<PlaylistOrientationEntity> GetPlaylistOrientations()
		{
			return
				_storage.Table.RetrieveMultipleEntities<PlaylistOrientationEntity>(PlaylistOrientationEntity.PartitionKeyString,
					_storage.Table.Halo4CloudTable);
		}

	#endregion
	}
}