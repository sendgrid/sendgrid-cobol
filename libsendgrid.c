#include <stdio.h>
#include <stdlib.h>
#include <curl/curl.h>

int sg_test(char *sgUsername, char *sgPassword, char *sgUrl);

int
sg_test(char *sgUsername, char *sgPassword, char *sgUrl)
{
  printf("Sendgrid Username: %s\n", sgUsername);
  printf("Sendgrid Password: %s\n", sgPassword);
  printf("Sendgrid Url: %s\n", sgUrl);

  return 0;
}

function_pt(void *ptr, size_t size, size_t nmemb, void *stream)
{
  printf("%d", atoi(ptr));
}

