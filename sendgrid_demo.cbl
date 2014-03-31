       IDENTIFICATION DIVISION.
       PROGRAM-ID.   sendgrid.
       AUTHOR.       Robin Johnson. Alex Reed.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *    SendGrid Authentication
           01 SG-AUTH-USERNAME   PIC X(100)    VALUE SPACES.
           01 SG-AUTH-PASSWORD   PIC X(50)     VALUE SPACES.

      *    Email params
           01 SG-MAIL-TO         PIC X(100)    VALUE "test@gmail.com".
           01 SG-MAIL-FROM       PIC X(100)    VALUE "me@gmail.com".
           01 SG-MAIL-SUBJECT    PIC X(100)    VALUE "Test Email".
           01 SG-MAIL-BODY-TEXT  PIC X(10000)  VALUE "Welcome Guys! xo".


       PROCEDURE DIVISION.
          ACCEPT SG-AUTH-USERNAME FROM ENVIRONMENT "SENDGRID_USERNAME".
          ACCEPT SG-AUTH-PASSWORD FROM ENVIRONMENT "SENDGRID_PASSWORD".

      *   This calls the send_email function in libsendgrid
          CALL "send_email" USING SG-AUTH-USERNAME SG-AUTH-PASSWORD 
                                  SG-MAIL-TO SG-MAIL-FROM 
                                  SG-MAIL-SUBJECT SG-MAIL-BODY-TEXT.
