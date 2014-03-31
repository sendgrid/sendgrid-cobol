#include <stdio.h>
#include <stdlib.h>
#include <curl/curl.h>

const char[] sgUrl = 'https://api.sendgrid.com/api/mail.send.json'

int 
send_email(char *sgUsername, char *sgPassword, char *sgToEmail, char *sgFromEmail, char *sgSubject, char *sgBodyText)



// LOOK HERE




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

