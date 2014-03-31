# SendGrid Cobol Library
The easiest way to send emails in Cobol!


## How to use

Installing Cobol, is now as easy as installing `open-cobol` in brew:

```
$ brew install open-cobol
```

In our COBOL example, we are using Environment Variables.  You can set your own Environment Variables but following this link:

<a href="http://stackoverflow.com/questions/7501678/set-environment-variables-on-mac-os-x-lion">http://stackoverflow.com/questions/7501678/set-environment-variables-on-mac-os-x-lion</a>

Set some SendGrid Credentials.  $SENDGRID_USERNAME  &&  $SENDGRID_PASSWORD.

In Cobol, we get Environment Variables and put them into variables, like so:

```
    ACCEPT SG-AUTH-USERNAME FROM ENVIRONMENT "SENDGRID_USERNAME".
    ACCEPT SG-AUTH-PASSWORD FROM ENVIRONMENT "SENDGRID_PASSWORD".
```

To send the email; call the function in libsendgrid `send_email` from cobol, and pass in the arguments. For an example on how to call this in the context of a complete Cobol application, look at the `sendgrid_demo.cbl` file in the root of the repo.

```
    CALL "send_email" USING SG-AUTH-USERNAME SG-AUTH-PASSWORD 
                            SG-MAIL-TO SG-MAIL-FROM 
                            SG-MAIL-SUBJECT SG-MAIL-BODY-TEXT.
```

Save the cobol source, and open terminal and navigate to the directory of the source code. Then call these functions:

```
$ cobc -c -static sendgrid_demo.cbl
$ cobc -x -o sendgrid_demo sendgrid_demo.o libsendgrid.o
```

This will now produce a executable binary called sendgrid_demo in the same folder as your source code. To execute it, just run:

```
$ ./sendgrid_demo
```

If your email was sent, the output should look like this:

```
$ ./sendgrid_demo
Creating POST Payload...  Done.
Sending POST Payload to SendGrid WebApi...  Done. Email Sent.
```

## Maintained By

- Robin Johnson [@rbin](http://twitter.com/rbin)
- Alex Reed [@alexerax](http://twitter.com/alexerax)