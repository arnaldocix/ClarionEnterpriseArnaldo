!ABCIncludeFile(ABC)

  OMIT('_EndOfInclude_',_ClaMailPresent_)
_ClaMailPresent_  EQUATE(1)
  INCLUDE('systemstring.inc'), ONCE

                    MAP
                      MODULE('ClaMail.clw')
CreateEmailList         PROCEDURE(EmailAddressList inList, SystemStringClass outStr),DLL(_ABCDllMode_)

!region functions do the same as the classes below, only in a non-OOP way
!region SMS
!!! <summary>
!!! Create an SMS Client
!!! </summary>
!!! <returns>A handle to an SMSClient</returns>
MakeSMSClient           PROCEDURE(STRING SMTPServerHost, STRING SMTPLoginUserName, STRING SMTPLoginPassword, STRING fromAddress, LONG SMTPEnableSsl, <LONG SMTPPort>),LONG,DLL(_ABCDllMode_)
!!! <summary>
!!! Send an SMS Message
!!! </summary>
!!! <param name="SMSClient">a handle to an SMS client obtained by calling MakeSMSClient</param>
!!! <returns>0 if the SMS was successfully sent, 1 if SMSClient is 0, 2 if there was a format exception, 1 if there was an SMPT exception, 99 for other types of exceptions.</returns>
SendSMS                 PROCEDURE(LONG SMSClientHandle, STRING toPhoneNumber, STRING  toCarrier, STRING  subject, STRING  textMessage, <*STRING errMessage>),LONG,PROC,DLL(_ABCDllMode_)
!!! <summary>
!!! Frees up the resources allocated by MakeSMSClient
!!! </summary>
!!! <param name="SMSClient">a handle to an SMS client obtained by calling MakeSMSClient</param>
FreeSMSClient           PROCEDURE(LONG SMSClientHandle),DLL(_ABCDllMode_)
!endregion
!region Email
!!! <summary>
!!! Create an Email Client
!!! </summary>
!!! <returns>A handle to a MailClient</returns>
MakeMailClient           PROCEDURE(STRING SMTPServerHost, STRING SMTPLoginUserName, STRING SMTPLoginPassword, STRING fromAddress, LONG SMTPEnableSsl, <LONG SMTPPort>),LONG,DLL(_ABCDllMode_)
!!! <param name="MailClient">a handle to a Mail client obtained by calling MakeMailClient</param>
SetEncoding           PROCEDURE(LONG MailClientHandle, STRING encoding),DLL(_ABCDllMode_)
!!! <param name="MailClient">a handle to a Mail client obtained by calling MakeMailClient</param>
SetReceivedRequest    PROCEDURE(LONG MailClientHandle, BOOL wantReceivedNotice),DLL(_ABCDllMode_)
!!! <param name="MailClient">a handle to a Mail client obtained by calling MakeMailClient</param>
SetReplyTo            PROCEDURE(LONG MailClientHandle, STRING replyAddress),DLL(_ABCDllMode_)
!!! <param name="MailClient">a handle to a Mail client obtained by calling MakeMailClient</param>
!!! <returns>0 if the Email was successfully sent, 1 if MailClient is 0, 2 if there was a format exception, 1 if there was an SMPT exception, 99 for other types of exceptions.</returns>
SendHTMLMail          PROCEDURE(LONG MailClientHandle, STRING toAddress, STRING subject, STRING htmlMessage, <STRING embeddedImageFileNames>, <STRING attachedFileNames>, <EmailAddressList ccList>, <EmailAddressList bccList>, <*STRING errMessage>),LONG,PROC,DLL(_ABCDllMode_)
!!! <param name="MailClient">a handle to a Mail client obtained by calling MakeMailClient</param>
!!! <returns>0 if the Email was successfully sent, 1 if MailClient is 0, 2 if there was a format exception, 1 if there was an SMPT exception, 99 for other types of exceptions.</returns>
SendMail              PROCEDURE(LONG MailClientHandle, STRING toAddress, STRING subject, STRING message, <STRING attachedFileNames>, <EmailAddressList ccList>, <EmailAddressList bccList>, <*STRING errMessage>),LONG,PROC,DLL(_ABCDllMode_)
!!! <summary>
!!! Frees up the resources allocated by MakeMailClient
!!! </summary>
!!! <param name="MailClient">a handle to a Mail client obtained by calling MakeMailClient</param>
FreeMailClient           PROCEDURE(LONG MailClientHandle),DLL(_ABCDllMode_)
!endregion
!!! <summary>
!!! Indicates if error messages should be displayed when sending SMS or Email messages
!!! </summary>
!!! <param name="SMSMailClient">a handle to an SMS or Email client obtained by calling MakeSMSClient or MakeMailClient</param>
SetErrorReporting       PROCEDURE(LONG SMSMailClientHandle, BOOL showErrorMessage),DLL(_ABCDllMode_)
!endregion
                      END
                    END

!!! <summary>
!!! Queue used to pass a list of email addresses
!!! </summary>
EmailAddressList  QUEUE,TYPE
!!! <summary>
!!! Display name of the email address.  Can be blank
!!! </summary>
displayName           STRING(200)
!!! <summary>
!!! email address.  Can be in the format address@host or <address@host>
!!! </summary>
emailAddress          STRING(100)
                    END

!!! <summary>
!!! Base class for SMSClient and MailClient
!!! </summary>
SMSMailClient     CLASS,TYPE,MODULE('ClaMail.clw'),LINK('ClaMail.clw',_ABCLinkMode_),DLL(_ABCDllMode_)
!region Protected
claTalk               &ClaRunExtClass,PROTECTED
errorReporting        BOOL,PROTECTED
host                  &SystemStringClass,PROTECTED
user                  &SystemStringClass,PROTECTED
pwd                   &SystemStringClass,PROTECTED
from                  &SystemStringClass,PROTECTED
ssl                   BOOL,PROTECTED
port                  LONG,PROTECTED

Construct             PROCEDURE(),PROTECTED
Destruct              PROCEDURE(),VIRTUAL

!!! <summary>
!!! Set up the mail server
!!! </summary>
!!! <param name="SMTPServerHost">the server name to use to send the email</param>
!!! <param name="SMTPLoginUserName">user name to login to the server</param>
!!! <param name="SMTPLoginPassword">password to login to the server</param>
!!! <param name="fromAddress">address of who is sending messages</param>
!!! <param name="SMTPEnableSsl">(true/false) if the server uses SSL</param>
!!! <param name="SMTPPort">port number, if needed, for the server</param>
Setup                 PROCEDURE(STRING SMTPServerHost, STRING SMTPLoginUserName, STRING SMTPLoginPassword, STRING fromAddress, LONG SMTPEnableSsl, <LONG SMTPPort>),PROTECTED
!endregion
!region Public Methods
!!! <summary>
!!! Set if the send methods should display a message when they encounter an error.
!!! The default state is to not display a message.
!!! </summary>
SetErrorReporting     PROCEDURE(BOOL showErrorMessage)
!endregion
                    END

!!! <summary>
!!! Instances of this class allow you to send SMS messages to an SMTP Server that supports SMS
!!! </summary>
SMSClient         CLASS(SMSMailClient),TYPE,MODULE('ClaMail.clw'),LINK('ClaMail.clw',_ABCLinkMode_),DLL(_ABCDllMode_)
!region Public Methods
!!! <summary>
!!! Set up the SMTP server
!!! </summary>
!!! <param name="SMTPServerHost">the server name to use to send the email</param>
!!! <param name="SMTPLoginUserName">user name to login to the server</param>
!!! <param name="SMTPLoginPassword">password to login to the server</param>
!!! <param name="fromAddress">address of who is sending messages</param>
!!! <param name="SMTPEnableSsl">(true/false) if the server uses SSL</param>
!!! <param name="SMTPPort">port number, if needed, for the server</param>
Init                  PROCEDURE(STRING SMTPServerHost, STRING SMTPLoginUserName, STRING SMTPLoginPassword, STRING fromAddress, LONG SMTPEnableSsl, <LONG SMTPPort>)

!!! <summary>
!!! Send an SMS to the toPhoneNumber via the toCarrier using the SMTP server information
!!! </summary>
!!! <param name="toPhoneNumber">
!!! The telephone number to send the SMS to
!!! </param>
!!! <param name="toCarrier">
!!! The name of the carrier who the toPhoneNumber is with.  The format of toCarrier defines how the
!!! address of the receiver is constructed:
!!! Format                 Address
!!! ======                 =======
!!! name                   <toPhoneNumber>@name
!!! @name                  <toPhoneNumber>@name
!!! text{number}more       text<toPhoneNumber>more
!!! The last format gives you complete control over the name.  An example of this is
!!! toPhoneNumber = 123456
!!! toCarrier = 'sms.{number}@carrier.com
!!! The SMS would be sent to sms.123456@carrier.com
!!! </param>
!!! <param name="subject">subject of the SMS</param>
!!! <param name="textMessage">content of the SMS</param>
!!! <param name="errMessage">
!!! An optional string that will be filled with the error message if the send fails
!!! </param>
!!! <returns>0 if the SMS was successfully sent, 2 if there was a format exception, 1 if there was an SMPT exception, 99 for other types of exceptions.</returns>
SendSMS               PROCEDURE(STRING toPhoneNumber, STRING  toCarrier, STRING  subject, STRING  textMessage, <*STRING errMessage>),LONG,PROC
!endregion
                    END

!!! <summary>
!!! Instances of this class allow you to send emails to an SMTP server
!!! </summary>
MailClient        CLASS(SMSMailClient),TYPE,MODULE('ClaMail.clw'),LINK('ClaMail.clw',_ABCLinkMode_),DLL(_ABCDllMode_)
!region Private
wantAReceivedNotice   BOOL,PRIVATE
encoding              &SystemStringClass,PRIVATE
replyTo               &SystemStringClass,PRIVATE

Construct             PROCEDURE(),PROTECTED
Destruct              PROCEDURE(),VIRTUAL
!endregion
!region Public Methods
!region Initialization
!!! <summary>
!!! Set up the mail server
!!! </summary>
!!! <param name="SMTPServerHost">the server name to use to send the email</param>
!!! <param name="SMTPLoginUserName">user name to login to the server</param>
!!! <param name="SMTPLoginPassword">password to login to the server</param>
!!! <param name="fromAddress">
!!! address of the sender of the message.  The address must be in one of these formats:
!!! "display name" <address@host>,
!!! display name <address@host>,
!!! <address@host>, or
!!! address@host
!!! If the display name is not in quotes, then there cannot be a comma in the display
!!! name.  It is recommended therefore to use either "display name" or no display name
!!! </param>
!!! <param name="SMTPEnableSsl">(true/false) if the server uses SSL</param>
!!! <param name="SMTPPort">port number, if needed, for the server</param>
Init                  PROCEDURE(STRING SMTPServerHost, STRING SMTPLoginUserName, STRING SMTPLoginPassword, STRING fromAddress, LONG SMTPEnableSsl, <LONG SMTPPort>)

!!! <summary>
!!! Set the character encoding that the message is sent in.  The default is
!!! ASCII.  To use a different encoding you need to set encoding to the name 
!!! of the encoding.  See clatalk.tpl for a list of encoding names
!!! </summary>
SetEncoding           PROCEDURE(STRING encoding)

!!! <summary>
!!! Set if a message should be sent if an email has been received.  Even if
!!! this is set to TRUE, it is not guaranteed that a receipt will be sent as
!!! lots of email clients do not support this option.  The default state is to not
!!! to request a receipt.
!!! </summary>
SetReceivedRequest    PROCEDURE(BOOL wantReceivedNotice)

!!! <summary>
!!! Set the email address that replies will be sent to.  If not set, then
!!! replies will go to the From address set with SetFrom
!!! </summary>
SetReplyTo            PROCEDURE(STRING replyAddress)

!endregion

!!! <summary>
!!! Send an HTML formatted email to the toAddress
!!! </summary>
!!! <param name="toAddress">
!!! address to send the message to.  The address must be in one of these formats:
!!! "display name" <address@host>,
!!! display name <address@host>,
!!! <address@host>, or
!!! address@host
!!! If the display name is not in quotes, then there cannot be a comma in the display
!!! name.  It is recommended therefore to use either "display name" or no display name
!!! </param>
!!! <param name="subject">subject of the message</param>
!!! <param name="htmlMessage">content of the message in html format</param>
!!! <param name="embeddedImageFileNames">
!!! Image file names separated by a semi-colon (;)
!!! To reference the images in your html message you use '<img src="cid:imageId' & n & '">'
!!! where n indicates the image's position in your list of names starting at 1.  For example
!!! if you set embeddedImageFileNames to 'C:\Images\Flowers.png;C:\Images\Cats.png', then in your
!!! htmlMessage you reference Flowers.png with the code '<img src="cid:imageId1">' and Cats.png
!!! with the code '<img src="cid:imageId2">'
!!! </param>
!!! <param name="attachedFileNames">attachement files separated by a semi-colon (;)</param>
!!! <param name="ccList">
!!! A list of email addresses that should be in the CC list in the email
!!! </param>
!!! <param name="bccList">
!!! A list of email addresses that should be in the BCC list in the email
!!! </param>
!!! <param name="errMessage">
!!! An optional string that will be filled with the error message if the send fails
!!! </param>
!!! <returns>0 if the email was successfully sent, 2 if there was a format exception, 1 if there was an SMPT exception, 99 for other types of exceptions.</returns>
SendHTMLMail          PROCEDURE(STRING toAddress, STRING subject, STRING htmlMessage, <STRING embeddedImageFileNames>, <STRING attachedFileNames>, <EmailAddressList ccList>, <EmailAddressList bccList>, <*STRING errMessage>),LONG,PROC

!!! <summary>
!!! Send an email to the toAddress
!!! </summary>
!!! <param name="toAddress">
!!! address to send the message to.  The address must be in one of these formats:
!!! "display name" <address@host>,
!!! display name <address@host>,
!!! <address@host>, or
!!! address@host
!!! If the display name is not in quotes, then there cannot be a comma in the display
!!! name.  It is recommended therefore to use either "display name" or no display name
!!! </param>
!!! <param name="subject">subject of the message</param>
!!! <param name="message">content of the message as plain text</param>
!!! <param name="attachedFileNames">attachement files separated by a semi-colon (;)</param>
!!! <param name="ccList">
!!! A list of email addresses that should be in the CC list in the email
!!! </param>
!!! <param name="bccList">
!!! A list of email addresses that should be in the BCC list in the email
!!! </param>
!!! <param name="errMessage">
!!! An optional string that will be filled with the error message if the send fails
!!! </param>
!!! <returns>0 if the email was successfully sent, 2 if there was a format exception, 1 if there was an SMPT exception, 99 for other types of exceptions.</returns>
SendMail              PROCEDURE(STRING toAddress, STRING subject, STRING message, <STRING attachedFileNames>, <EmailAddressList ccList>, <EmailAddressList bccList>, <*STRING errMessage>),LONG,PROC
!endregion
                    END

!_EndOfInclude_
