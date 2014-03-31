       IDENTIFICATION DIVISION.
       PROGRAM-ID.   sendgrid.
       AUTHOR.       Robin Johnson. Alex Reed.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
      * SendGrid Authentication
           01 SG-AUTH-USERNAME    PIC X(100)  VALUE SPACES.
           01 SG-AUTH-PASSWORD    PIC X(50)   VALUE SPACES.

      * SendGrid Api Url
           01 URL-PROTOCOL        PIC X(8)    VALUE 'https://'.
           01 URL-HOST            PIC X(16)   VALUE 'api.sendgrid.com'.
           01 URL-VERSION         PIC X(3)    VALUE '/v3'.
           01 URL-BUILT           PIC X(100)  VALUE SPACES.
           01 URL-ENDPOINT        PIC X(20)   VALUE SPACES.

      * SendGrid Api Endpoints
           01 URL-EMAIL-ENDPOINT  PIC X(11)   VALUE '/user/email'.

       DATA DIVISION.
       WORKING-STORAGE SECTION.


       PROCEDURE DIVISION.

      *LET'S GET THIS DONE !!
       DISPLAY "THIS IS GETTING DONE, YO."

       END PROGRAM
