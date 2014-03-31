#include <stdio.h>
#include <stdlib.h>
#include <curl/curl.h>

static const char* sgUrl = "https://api.sendgrid.com/api/mail.send.json";

int 
send_email(char *sgUsername, char *sgPassword, char *sgToEmail, char *sgFromEmail, char *sgSubject, char *sgBodyText)
{

  CURL *curl;
  curl_global_init();
  curl_easy_setopt(curl, CURLOPT_VERBOSE, 1);
  curl_easy_setopt(curl, CURLOPT_URL, sgUrl);
  curl_easy_setopt(curl, CURLOPT_POST, 1);
  curl_easy_setopt(curl, CURLOPT_USERNAME, sgUsername);
  curl_easy_setopt(curl, CURLOPT_PASSWORD, sgPassword);
  curl_easy_setopt(curl, CURLOPT_POSTFIELDS, /* sgToEmail, sgFromEmail, sgSubject, sgBodyText */  );
  curl_easy_perform(curl);
  curl_easy_cleanup(curl);

  return 0;
}

