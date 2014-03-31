#include <stdio.h>
#include <stdlib.h>
#include <curl/curl.h>

int 
send_email(char *sgUsername, char *sgPassword, char *sgUrl, char *sgToEmail, char *sgFromEmail, char *sgSubject, char *sgBodyText)

int
sg_test(char *sgUsername, char *sgPassword, char *sgUrl)
{
  printf("Sendgrid Username: %s\n", sgUsername);
  printf("Sendgrid Password: %s\n", sgPassword);
  printf("Sendgrid Url: %s\n", sgUrl);

  return 0;
}

int
send_email(char *sgUsername, char *sgPassword, char *sgUrl, char *sgToEmail, char *sgFromEmail, char *sgSubject, char *sgBodyText)


// LOOK HERE

curl -d 'to=robin@sendgrid.com&toname=Robin Johnson&subject=LOL EMAIL&text=testingtextbody&from=rbin@sendgrid.com&api_user=$SENDGRID_USERNAME&api_key=$SENDGRID_PASSWORD' https://api.sendgrid.com/api/mail.send.json

//


int 
main(void)
{
  CURL *curl;
  CURLcode res;
 
  curl = curl_easy_init();
  if(curl) {
    curl_easy_setopt(curl, CURLOPT_URL, "https://api.sendgrid.com/v3/user/email");
 
    /* Perform the request, res will get the return code */ 
    res = curl_easy_perform(curl);
    /* Check for errors */ 
    if(res != CURLE_OK)
      fprintf(stderr, "curl_easy_perform() failed: %s\n",
              curl_easy_strerror(res));
 
    /* always cleanup */ 
    curl_easy_cleanup(curl);
  }
  return 0;
}

