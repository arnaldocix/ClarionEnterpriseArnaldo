!ABCIncludeFile(ABC)

  OMIT('_EndOfInclude_',_ClaRunExtPresent_)
_ClaRunExtPresent_  EQUATE(1)

                    MAP
                    END

IAWSTransfering     INTERFACE,COM
Transfering           PROCEDURE(LONG percDone, LONG finished)
                    END

RotateFlipType:RotateNoneFlipNone   EQUATE(0)!Specifies no clockwise rotation and no flipping.
RotateFlipType:Rotate90FlipNone EQUATE(1)!Specifies a 90-degree clockwise rotation without flipping.
RotateFlipType:Rotate180FlipNone    EQUATE(2)!Specifies a 180-degree clockwise rotation without flipping.
RotateFlipType:Rotate270FlipNone    EQUATE(3)!Specifies a 270-degree clockwise rotation without flipping.
RotateFlipType:RotateNoneFlipX  EQUATE(4)!Specifies no clockwise rotation followed by a horizontal flip.
RotateFlipType:Rotate90FlipX    EQUATE(5)!Specifies a 90-degree clockwise rotation followed by a horizontal flip.
RotateFlipType:Rotate180FlipX   EQUATE(6)!Specifies a 180-degree clockwise rotation followed by a horizontal flip.
RotateFlipType:Rotate270FlipX   EQUATE(7)!Specifies a 270-degree clockwise rotation followed by a horizontal flip.
RotateFlipType:RotateNoneFlipY  EQUATE(8)!Specifies no clockwise rotation followed by a vertical flip.
RotateFlipType:Rotate90FlipY    EQUATE(9)!Specifies a 90-degree clockwise rotation followed by a vertical flip.
RotateFlipType:Rotate180FlipY   EQUATE(10)!Specifies a 180-degree clockwise rotation followed by a vertical flip.
RotateFlipType:Rotate270FlipY   EQUATE(11)!Specifies a 270-degree clockwise rotation followed by a vertical flip.
RotateFlipType:RotateNoneFlipXY EQUATE(12)!Specifies no clockwise rotation followed by a horizontal and vertical flip.
RotateFlipType:Rotate90FlipXY   EQUATE(13)!Specifies a 90-degree clockwise rotation followed by a horizontal and vertical flip.
RotateFlipType:Rotate180FlipXY  EQUATE(14)!Specifies a 180-degree clockwise rotation followed by a horizontal and vertical flip.
RotateFlipType:Rotate270FlipXY  EQUATE(15)!Specifies a 270-degree clockwise rotation followed by a horizontal and vertical flip.

PNGImageResolution:Original EQUATE(0)
PNGImageResolution:Draft    EQUATE(200)
PNGImageResolution:Normal   EQUATE(400)
PNGImageResolution:Good     EQUATE(600)
PNGImageResolution:Detail   EQUATE(1200)

HttpVerb:GET          EQUATE(0)
HttpVerb:POST         EQUATE(1)
HttpVerb:PUT          EQUATE(2)
HttpVerb:DELETE       EQUATE(3)
ClaRunExtClass      CLASS,TYPE,MODULE('ClaRunExt.clw'),LINK('ClaRunExt.clw',_ABCLinkMode_),DLL(_ABCDllMode_)
Construct             PROCEDURE()
Destruct              PROCEDURE()

!region Public Methods

!region Image Methods
!!! <summary>
!!! Convert any Clarion supported format image file to a PNG file, changing the fileName extension to PNG.
!!! </summary>
!!! <param name="fileNameIn">the name of the file to convert</param>
!!! <returns>Return Zero if no error and Error Number if there is an error.</returns>
ImageToPNG            PROCEDURE(STRING fileNameIn),LONG

!!! <summary>
!!! Convert any Clarion supported format image file to a PNG with the name fileNameOut.
!!! </summary>
!!! <param name="fileNameIn">the name of the file to convert</param>
!!! <param name="fileNameOut">the name of the PNG output file</param>
!!! <returns>Return Zero if no error and Error Number if there is an error.</returns>
ImageToPNG            PROCEDURE(STRING fileNameIn, STRING fileNameOut),LONG

!!! <summary>
!!! Convert any Clarion supported format image file to a PNG with the name fileNameOut.
!!! </summary>
!!! <param name="fileNameIn">the name of the file to convert</param>
!!! <param name="fileNameOut">the name of the PNG output file</param>
!!! <param name="imageRes">the resolution used to convert the image Ie: 200,400,600,1200. 0 use the default input image resolution</param>
!!! <returns>Return Zero if no error and Error Number if there is an error.</returns>
ImageToPNG            PROCEDURE(STRING fileNameIn, STRING fileNameOut, USHORT imageRes),LONG

!!! <summary>
!!! Modify a file applying rotation or flip and store the result in a new file
!!! </summary>
!!! <param name="fileNameIn">the name of the file to rotate</param>
!!! <param name="fileNameOut">the name of the modified file</param>
!!! <param name="RotateFlipType">Type of rotation to by applied to the image. equate of type RotateFlipType:</param>
!!! <returns>Return Zero if no error and Error Number if there is an error.</returns>
ImageRotateFlip       PROCEDURE(STRING fileNameIn, STRING fileNameOut, LONG RotateFlipType),LONG

!!! <summary>
!!! Create a new image reduced in size based on the percentage parameter
!!! </summary>
!!! <param name="fileNameIn">the name of the original file to reduce</param>
!!! <param name="fileNameOut">the name of the modified file</param>
!!! <param name="percentage">percentage to reduce the file</param>
!!! <returns>Return Zero if no error and Error Number if there is an error.</returns>
ImageSaveThumbnail    PROCEDURE(STRING fileNameIn, STRING fileNameOut, LONG percentage),LONG

!endregion

!region SMTP Methods
!!! <summary>
!!! Send an SMS to the toPhoneNumber via the toCarrier using the SMTP server information
!!! </summary>
!!! <param name="SMTPServerHost">the server name to use to send the SMS</param>
!!! <param name="SMTPLoginUserName">user name to login to the server</param>
!!! <param name="SMTPLoginPassword">password to login to the server</param>
!!! <param name="SMTPPort">port number, if needed, for the server</param>
!!! <param name="SMTPEnableSsl">(true/false) if the server uses SSL</param>
!!! <param name="showErrorMessage">(true/false) if the function will show the full error in a message or just return the error value</param>
!!! <param name="fromAddress">
!!! address of the sender of the message.  The address must be in one of these formats:
!!! "display name" <address@host>,
!!! display name <address@host>,
!!! <address@host>, or
!!! address@host
!!! If the display name is not in quotes, then there cannot be a comma in the display
!!! name.  It is recommended therefore to use either "display name" or no display name
!!! </param>
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

SendSMS               PROCEDURE(STRING SMTPServerHost, STRING SMTPLoginUserName, STRING SMTPLoginPassword, LONG SMTPPort, LONG SMTPEnableSsl, LONG showErrorMessage, STRING fromAddress, STRING toPhoneNumber, STRING toCarrier, STRING subject, STRING textMessage, <*STRING errMessage>),LONG
!!! <summary>
!!! Send an email to the toAddress using the SMTP server information
!!! </summary>
!!! <param name="SMTPServerHost">the server name to use to send the email</param>
!!! <param name="SMTPLoginUserName">user name to login to the server</param>
!!! <param name="SMTPLoginPassword">password to login to the server</param>
!!! <param name="SMTPPort">port number, if needed, for the server</param>
!!! <param name="SMTPEnableSsl">(true/false) if the server uses SSL</param>
!!! <param name="showErrorMessage">(true/false) if the function will show the full error in a message or just return the error value</param>
!!! <param name="receiveRequest">(true/false) if a confirmation of received is requested (not all clients will answer)</param>
!!! <param name="messageBodyEncoding">the encoding for the message empty or defualt will be ASCII</param>
!!! <param name="fromAddress">
!!! address of the sender of the message.  The address must be in one of these formats:
!!! "display name" <address@host>,
!!! display name <address@host>,
!!! <address@host>, or
!!! address@host
!!! If the display name is not in quotes, then there cannot be a comma in the display
!!! name.  It is recommended therefore to use either "display name" or no display name
!!! </param>
!!! <param name="toAddress">
!!! address to send the message to.  The address must be in one of these formats:
!!! "display name" <address@host>,
!!! display name <address@host>,
!!! <address@host>, or
!!! address@host
!!! If the display name is not in quotes, then there cannot be a comma in the display
!!! name.  It is recommended therefore to use either "display name" or no display name
!!! </param>
!!! <param name="replyToAddress">the address to get the reply from the message</param>
!!! <param name="subject">subject of the message</param>
!!! <param name="htmlMessage">content of the message if it is html formated</param>
!!! <param name="embeddedImageFileNames">
!!! Image file names separated by a semi-colon (;)
!!! To reference the images in your html message you use '<img src="cid:imageId' & n & '">'
!!! where n indicates the image's position in your list of names starting at 1.  For example
!!! if you set embeddedImageFileNames to 'C:\Images\Flowers.png;C:\Images\Cats.png', then in your
!!! htmlMessage you reference Flowers.png with the code '<img src="cid:imageId1">' and Cats.png
!!! with the code '<img src="cid:imageId2">'
!!! </param>
!!! <param name="textMessage">content of the message if it is plain text</param>
!!! <param name="attachedFileNames">attachment files separated by a semi-colon (;)</param>
!!! <param name="ccList">
!!! A comma separated list of email addresses that will be added into the CC list in the email
!!! The addresses must be in one of these formats:
!!! "display name" <address@host>,
!!! display name <address@host>,
!!! <address@host>, or
!!! address@host
!!! If the display name is not encloded in quotes, then there cannot be a comma in the display
!!! name.  It is recommended therefore to use either "display name" or no display name
!!! </param>
!!! <param name="bccList">
!!! A comma separated list of email addresses that will be added into the BCC list in the email
!!! The addresses must be in one of these formats:
!!! "display name" <address@host>,
!!! display name <address@host>,
!!! <address@host>, or
!!! address@host
!!! If the display name is not in quotes, then there cannot be a comma in the display
!!! name.  It is recommended therefore to use either "display name" or no display name
!!! </param>
!!! <param name="errMessage">
!!! An optional string that will be filled with the error message if the send fails
!!! </param>
!!! <returns>0 if the email was successfully sent, 2 if there was a format exception, 1 if there was an SMTP exception, 99 for other types of exceptions.</returns>
SendMail              PROCEDURE(STRING SMTPServerHost, STRING SMTPLoginUserName, STRING SMTPLoginPassword, LONG SMTPPort, LONG SMTPEnableSsl, LONG showErrorMessage, LONG receiveRequest, STRING messageBodyEncoding, STRING fromAddress, STRING toAddress, STRING replyToAddress, STRING subject, STRING htmlMessage, STRING embeddedImageFileNames, STRING textMessage, STRING attachedFileNames, STRING ccList, STRING bccList, <*STRING errMessage>),LONG

!endregion

!region WebRequest Methods
!!! <summary>
!!! Executes a WebRequest with the passed parameters and stores the response in the responseOut buffer
!!! </summary>
!!! <param name="httpWebAddress"> the address to send the webRequest</param>
!!! <param name="httpVerbMethod"> the verb method used in the webRequest, it can be HttpVerb:GET, POST, PUT, DELETE</param>
!!! <param name="postData"> data passed to the webRequest when using the POST verb</param>
!!! <param name="requestParameters"> parameters passed to the the webRequest appended to the http address after the ?</param>
!!! <param name="responseOut"> the buffer to be filled with the response, the buffer needs to be big enough to contain the response value</param>
!!! <returns> Zero if the request was successful, if the buffer is not big enough the return value will be the numeric value of the required buffer size. If the value is negative an exception occurred, and the description of the exception is in the responseOut</returns>
HttpWebRequest          PROCEDURE(CONST *CSTRING httpWebAddress,LONG httpVerbMethod,CONST *CSTRING postData,CONST *CSTRING requestParameters,*CSTRING responseOut),LONG

!!! <summary>
!!! Executes a WebRequest with the passed parameters and stores the response in the responseOut buffer
!!! </summary>
!!! <param name="httpWebAddress"> the HTTP address to send the webRequest</param>
!!! <param name="httpVerbMethod"> the verb method used in the webRequest, it can be HttpVerb:GET, POST, PUT, DELETE</param>
!!! <param name="postData"> data passed to the webRequest when using the POST verb</param>
!!! <param name="requestParameters"> the parameters passed to the the webRequest appended to the HTTP address after the ?</param>
!!! <param name="contentType"> the request content type, default is "text/xml"</param>
!!! <param name="headers"> a string containing the headers and values separated by pipes name1|value|name2|value2</param>
!!! <param name="responseOut"> the buffer to be filled with the response, the buffer needs to be big enough to contain the response value</param>
!!! <returns> Zero if the request was successful, if the buffer is not big enough the return value will be the numeric value of the required buffer size. If the value is negative an exception occurred, and the description of the exception is in the responseOut</returns>
HttpWebRequest          PROCEDURE(CONST *CSTRING httpWebAddress,LONG httpVerbMethod,CONST *CSTRING postData,CONST *CSTRING requestParameters,CONST *CSTRING contentType, CONST *CSTRING headers,*CSTRING responseOut),LONG

!!! <summary>
!!! Executes a WebRequest with the passed parameters and stores the response in a file created with the name in outputFilename
!!! </summary>
!!! <param name="httpWebAddress"> the HTTP address to send the webRequest</param>
!!! <param name="httpVerbMethod"> the verb method used in the webRequest, it can be HttpVerb:GET, POST, PUT, DELETE</param>
!!! <param name="postData"> data passed to the webRequest when using the POST verb</param>
!!! <param name="requestParameters"> the parameters passed to the the webRequest appended to the HTTP address after the ?</param>
!!! <param name="errorOut"> the text of the error message if an error occurred</param>
!!! <param name="outputFilename"> the name of the file to store the response in </param>
!!! <returns> Zero if the request was successful, if the buffer is not big enough the return value will be the numeric value of the required buffer size. If the value is negative an exception occurred, and the description of the exception is in the responseOut</returns>
HttpWebRequestToFile    PROCEDURE(CONST *CSTRING httpWebAddress,LONG httpVerbMethod,CONST *CSTRING postData,CONST *CSTRING requestParameters,*CSTRING errorOut, CONST *CSTRING outputFilename),LONG

!!! <summary>
!!! Executes a WebRequest with the passed parameters and stores the response in a file created with the name in outputFilename
!!! </summary>
!!! <param name="httpWebAddress"> the HTTP address to send the webRequest</param>
!!! <param name="httpVerbMethod"> the verb method used in the webRequest, it can be HttpVerb:GET, POST, PUT, DELETE</param>
!!! <param name="postData"> data passed to the webRequest when using the POST verb</param>
!!! <param name="requestParameters"> the parameters passed to the the webRequest appended to the HTTP address after the ?</param>
!!! <param name="contentType"> the request content type, defaults to "text/xml"</param>
!!! <param name="headers> a string containing the headers and values separated by pipes name1|value|name2|value2</param>
!!! <param name="errorOut"> the text of the error if an error occurred</param>
!!! <param name="outputFilename"> the name of the file to store the response</param>
!!! <returns> Zero if the request was successful, if the buffer is not big enough the return value will be the numeric value of the required buffer size. If the value is negative an exception occurred, and the description of the exception is in the responseOut</returns>
HttpWebRequestToFile    PROCEDURE(CONST *CSTRING httpWebAddress,LONG httpVerbMethod,CONST *CSTRING postData,CONST *CSTRING requestParameters,CONST *CSTRING contentType, CONST *CSTRING headers,*CSTRING errorOut, CONST *CSTRING outputFilename),LONG

!endregion

!region AWS Methods
! The AWS methods are not expected to be used directly by Clarion programmers.  Use the procedures and
! classes in claaws*.inc for a higher level interface

GetAWSRegion          PROCEDURE(*STRING displayName, *STRING systemName, <*STRING errorMessage>, <*STRING errorCode>),BOOL
ConnectToS3           PROCEDURE(STRING AccessKey, STRING SecretKey, <*STRING ErrorMessage>, <*STRING errorCode>),LONG
DisconnectFromS3      PROCEDURE(LONG s3)
!region container methods
GetContainer          PROCEDURE(LONG ptr, *STRING containerName, *STRING regionDisplayName, *STRING regionSystemName, *STRING errorMessage, *STRING errorCode),LONG
CreateContainer       PROCEDURE(LONG ptr, STRING containerName, STRING region, *STRING errorMessage, *STRING errorCode),LONG
DeleteContainer       PROCEDURE(LONG ptr, STRING containerName, *STRING errorMessage, *STRING errorCode),LONG
!endregion
!region folder methods
CreateFolder          PROCEDURE(LONG ptr, STRING containerName, STRING folderName, *STRING errorMessage, *STRING errorCode),LONG
DeleteFolder          PROCEDURE(LONG ptr, STRING containerName, STRING folderName, *STRING errorMessage, *STRING errorCode),LONG
!endregion
!region file methods
GetNextFile           PROCEDURE(LONG ptr, STRING containerName, STRING parentFolder, *STRING fileName, *BOOL isFile, *DATE lmDate, *TIME lmTime, *INT64 size, *STRING errorMessage, *STRING errorCode),LONG
FileExists            PROCEDURE(LONG ptr, STRING containerName, STRING fileName, *STRING errorMessage, *STRING errorCode),LONG
UploadFile            PROCEDURE(LONG ptr, STRING sourceFile, STRING containerName, STRING destinationFile, BOOL asynchronous, BOOL encrypted, IAWSTransfering callback, *STRING errorMessage, *STRING errorCode),LONG,PROC
DownloadFile          PROCEDURE(LONG ptr, STRING containerName, STRING sourceFile, STRING destinationFile, BOOL asynchronous, IAWSTransfering callback, *STRING errorMessage, *STRING errorCode),LONG,PROC
DeleteFile            PROCEDURE(LONG ptr, STRING containerName, STRING fileName, *STRING errorMessage, *STRING errorCode),LONG,PROC
!region Asynchronous transfer methods
CancelTransfer        PROCEDURE(LONG ptr, *STRING errorMessage, *STRING errorCode),LONG,PROC
!endregion
!region public access to file
GetTemporaryURL       PROCEDURE(LONG ptr, STRING containerName, STRING fileName, LONG days, LONG hours, LONG minutes, *STRING errorMessage, *STRING errorCode),STRING
GetPermanentURL       PROCEDURE(LONG ptr, STRING containerName, STRING fileName, *STRING errorMessage, *STRING errorCode),STRING
AllowPublicAccess     PROCEDURE(LONG ptr, STRING containerName, STRING fileName, BOOL public, *STRING errorMessage, *STRING errorCode),LONG,PROC
HasPublicAccess       PROCEDURE(LONG ptr, STRING containerName, STRING fileName, *STRING errorMessage, *STRING errorCode),BOOL,PROC
!endregion
!region Meta Data
GetNextMetaData       PROCEDURE(LONG ptr, STRING containerName, STRING fileName, *STRING key, *STRING value, *STRING errorMessage, *STRING errorCode),BOOL
GetMetaData           PROCEDURE(LONG ptr, STRING containerName, STRING fileName, STRING key, *STRING errorMessage, *STRING errorCode),STRING
SetMetaData           PROCEDURE(LONG ptr, STRING containerName, STRING fileName, STRING metaData, *STRING errorMessage, *STRING errorCode),BOOL,PROC
SetMetaData           PROCEDURE(LONG ptr, STRING containerName, STRING fileName, STRING key, STRING value, *STRING errorMessage, *STRING errorCode),LONG,PROC
RemoveMetaData        PROCEDURE(LONG ptr, STRING containerName, STRING fileName, STRING key, *STRING errorMessage, *STRING errorCode),LONG,PROC
!endregion
!endregion
!endregion
!endregion
!endregion
                    END
!_EndOfInclude_
