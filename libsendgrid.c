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
