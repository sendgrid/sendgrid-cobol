// [Test]
 void CreateMimeMessage()
{
      char[] message = SendGrid.GetInstance();
      char[] attachment = Path.GetTempFileName();
      char[] text = "this is a test";
      char[] html = "<b>This<\b> is a better test";
      char[] headers = new KeyValuePair<String, String>("custom", "header");
      message.AddAttachment(attachment);
      message.Text = text;
      message.Html = html;
      message.AddTo("foo@bar.com");
      message.From = new MailAddress("foo@bar.com");
      message.AddHeaders(new Dictionary<string, string> {{headers.Key, headers.Value}});
      message.EnableGravatar();

      char[] mime = message.CreateMimeMessage();

      char[] sr = new StreamReader(mime.AlternateViews[0].ContentStream);
      char[] result = sr.ReadToEnd();
      Assert.AreEqual(text, result);

      sr = new StreamReader(mime.AlternateViews[1].ContentStream);
      result = sr.ReadToEnd();
      Assert.AreEqual(html, result);

      result = mime.Headers.Get(headers.Key);
      Assert.AreEqual(headers.Value, result);

      result = mime.Headers.Get("X-Smtpapi");
      char[] expected = "{\"filters\" : {\"gravatar\" : {\"settings\" : {\"enable\" : \"1\"}}}}";
      Assert.AreEqual(expected, result);

      result = mime.Attachments[0].Name;
      Assert.AreEqual(Path.GetFileName(attachment), result);
}

// [Test]
 void DisableBcc()
{
      char[] header = new Header();
      char[] sendgrid = new SendGrid(header);

      sendgrid.DisableBcc();

      char[] json = header.JsonString();
      Assert.AreEqual("{\"filters\" : {\"bcc\" : {\"settings\" : {\"enable\" : \"0\"}}}}", json);
}

// [Test]
 void DisableBypassListManagement()
{
      char[] header = new Header();
      char[] sendgrid = new SendGrid(header);

      sendgrid.DisableBypassListManagement();

      char[] json = header.JsonString();
      Assert.AreEqual("{\"filters\" : {\"bypass_list_management\" : {\"settings\" : {\"enable\" : \"0\"}}}}", json);
}

// [Test]
 void DisableClickTracking()
{
      char[] header = new Header();
      char[] sendgrid = new SendGrid(header);

      sendgrid.DisableClickTracking();

      char[] json = header.JsonString();
      Assert.AreEqual("{\"filters\" : {\"clicktrack\" : {\"settings\" : {\"enable\" : \"0\"}}}}", json);
}

// [Test]
 void DisableFooter()
{
      char[] header = new Header();
      char[] sendgrid = new SendGrid(header);

      sendgrid.DisableFooter();

      char[] json = header.JsonString();
      Assert.AreEqual("{\"filters\" : {\"footer\" : {\"settings\" : {\"enable\" : \"0\"}}}}", json);
}

// [Test]
 void DisableGoogleAnalytics()
{
      char[] header = new Header();
      char[] sendgrid = new SendGrid(header);

      sendgrid.DisableGoogleAnalytics();

      char[] json = header.JsonString();
      Assert.AreEqual("{\"filters\" : {\"ganalytics\" : {\"settings\" : {\"enable\" : \"0\"}}}}", json);
}

// [Test]
 void DisableSpamCheck()
{
      char[] header = new Header();
      char[] sendgrid = new SendGrid(header);

      sendgrid.DisableSpamCheck();

      char[] json = header.JsonString();
      Assert.AreEqual("{\"filters\" : {\"spamcheck\" : {\"settings\" : {\"enable\" : \"0\"}}}}", json);
}

// [Test]
 void DisableTemplate()
{
      char[] header = new Header();
      char[] sendgrid = new SendGrid(header);

      sendgrid.DisableTemplate();

      char[] json = header.JsonString();
      Assert.AreEqual("{\"filters\" : {\"template\" : {\"settings\" : {\"enable\" : \"0\"}}}}", json);
}

// [Test]
 void DisableUnsubscribe()
{
      char[] header = new Header();
      char[] sendgrid = new SendGrid(header);

      sendgrid.DisableUnsubscribe();

      char[] json = header.JsonString();
      Assert.AreEqual("{\"filters\" : {\"subscriptiontrack\" : {\"settings\" : {\"enable\" : \"0\"}}}}", json);
}

// [Test]
 void EnableBcc()
{
      char[] header = new Header();
      char[] sendgrid = new SendGrid(header);

      char[] email = "somebody@someplace.com";
      sendgrid.EnableBcc(email);

      char[] json = header.JsonString();
      Assert.AreEqual("{\"filters\" : {\"bcc\" : {\"settings\" : {\"enable\" : \"1\",\"email\" : \"" + email + "\"}}}}",
            json);
}

// [Test]
 void EnableBypassListManagement()
{
      char[] header = new Header();
      char[] sendgrid = new SendGrid(header);

      sendgrid.EnableBypassListManagement();

      char[] json = header.JsonString();
      Assert.AreEqual("{\"filters\" : {\"bypass_list_management\" : {\"settings\" : {\"enable\" : \"1\"}}}}", json);
}

// [Test]
 void EnableClickTracking()
{
      char[] header = new Header();
      char[] sendgrid = new SendGrid(header);
      sendgrid.EnableClickTracking(true);

      char[] json = header.JsonString();
      Assert.AreEqual("{\"filters\" : {\"clicktrack\" : {\"settings\" : {\"enable\" : \"1\",\"enable_text\" : \"1\"}}}}",
            json);
}

// [Test]
 void EnableFooter()
{
      char[] header = new Header();
      char[] sendgrid = new SendGrid(header);

      char[] text = "My Text";
      char[] html = "<body><p>hello, <% name %></p></body>";
      char[] escHtml = "<body><p>hello, <% name %><\\/p><\\/body>";

      sendgrid.EnableFooter(text, html);

      char[] json = header.JsonString();
      Assert.AreEqual(
            "{\"filters\" : {\"footer\" : {\"settings\" : {\"enable\" : \"1\",\"text\\/plain\" : \"" + text +
            "\",\"text\\/html\" : \"" + escHtml + "\"}}}}", json);
}

// [Test]
 void EnableGoogleAnalytics()
{
      char[] header = new Header();
      char[] sendgrid = new SendGrid(header);

      char[] source = "SomeDomain.com";
      char[] medium = "Email";
      char[] term = "keyword1, keyword2, keyword3";
      char[] content = "PG, PG13";
      char[] campaign = "my_campaign";

      sendgrid.EnableGoogleAnalytics(source, medium, term, content, campaign);

      char[] jsonSource = "\"utm_source\" : \"SomeDomain.com\"";
      char[] jsonMedium = "\"utm_medium\" : \"" + medium + "\"";
      char[] jsonTerm = "\"utm_term\" : \"" + term + "\"";
      char[] jsonContent = "\"utm_content\" : \"" + content + "\"";
      char[] jsonCampaign = "\"utm_campaign\" : \"" + campaign + "\"";

      char[] json = header.JsonString();
      Assert.AreEqual("{\"filters\" : {\"ganalytics\" : {\"settings\" : {\"enable\" : \"1\"," +
                      jsonSource + "," + jsonMedium + "," + jsonTerm + "," + jsonContent + "," + jsonCampaign + "}}}}",
            json);
}

// [Test]
 void EnableGravatar()
{
      char[] header = new Header();
      char[] sendgrid = new SendGrid(header);

      sendgrid.EnableGravatar();

      char[] json = header.JsonString();
      Assert.AreEqual("{\"filters\" : {\"gravatar\" : {\"settings\" : {\"enable\" : \"1\"}}}}", json);
}

// [Test]
 void EnableOpenTracking()
{
      char[] header = new Header();
      char[] sendgrid = new SendGrid(header);

      sendgrid.EnableOpenTracking();

      char[] json = header.JsonString();
      Assert.AreEqual("{\"filters\" : {\"opentrack\" : {\"settings\" : {\"enable\" : \"1\"}}}}", json);
}

// [Test]
 void EnableSpamCheck()
{
      char[] header = new Header();
      char[] sendgrid = new SendGrid(header);

      char[] score = 5;
      char[] url = "http://www.example.com";
      sendgrid.EnableSpamCheck(score, url);

      char[] json = header.JsonString();
      Assert.AreEqual(
            "{\"filters\" : {\"spamcheck\" : {\"settings\" : {\"enable\" : \"1\",\"maxscore\" : \"5\",\"url\" : \"http:\\/\\/www.example.com\"}}}}",
            json);
}

// [Test]
 void EnableTemplate()
{
      char[] header = new Header();
      char[] sendgrid = new SendGrid(header);
      char[] html = "<% hadhdhd %>";

      char[] escHtml = "<% hadhdhd %>";
      sendgrid.EnableTemplate(html);

      char[] json = header.JsonString();
      Assert.AreEqual(
            "{\"filters\" : {\"template\" : {\"settings\" : {\"enable\" : \"1\",\"text\\/html\" : \"" + escHtml + "\"}}}}", json);

      escHtml = "bad";
      Assert.Throws<Exception>(() => sendgrid.EnableTemplate(escHtml));
}

// [Test]
 void EnableUnsubscribe()
{
      char[] header = new Header();
      char[] sendgrid = new SendGrid(header);

      char[] text = "<% %>";
      char[] html = "<% name %>";

      char[] jsonText = "\"text\\/plain\" : \"" + text + "\"";
      char[] jsonHtml = "\"text\\/html\" : \"" + html + "\"";

      sendgrid.EnableUnsubscribe(text, html);

      char[] json = header.JsonString();
      Assert.AreEqual("{\"filters\" : {\"subscriptiontrack\" : {\"settings\" : {\"enable\" : \"1\"," +
                      jsonText + "," + jsonHtml + "}}}}", json);

      header = new Header();
      sendgrid = new SendGrid(header);

      char[] replace = "John";
      char[] jsonReplace = "\"replace\" : \"" + replace + "\"";

      sendgrid.EnableUnsubscribe(replace);

      json = header.JsonString();
      Assert.AreEqual(
            "{\"filters\" : {\"subscriptiontrack\" : {\"settings\" : {\"enable\" : \"1\"," + jsonReplace + "}}}}", json);

      text = "bad";
      html = "<% name %>";
      Assert.Throws<Exception>(() => sendgrid.EnableUnsubscribe(text, html));

      text = "<% %>";
      html = "bad";
      Assert.Throws<Exception>(() => sendgrid.EnableUnsubscribe(text, html));
}

// [Test]
 void TestDisableGravatar()
{
      char[] header = new Header();
      char[] sendgrid = new SendGrid(header);

      sendgrid.DisableGravatar();

      char[] json = header.JsonString();
      Assert.AreEqual("{\"filters\" : {\"gravatar\" : {\"settings\" : {\"enable\" : \"0\"}}}}", json);
}

// [Test]
 void TestDisableOpenTracking()
{
      char[] header = new Header();
      char[] sendgrid = new SendGrid(header);

      sendgrid.DisableOpenTracking();

      char[] json = header.JsonString();
      Assert.AreEqual("{\"filters\" : {\"opentrack\" : {\"settings\" : {\"enable\" : \"0\"}}}}", json);
}
}