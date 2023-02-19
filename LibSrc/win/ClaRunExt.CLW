                    MEMBER()

  INCLUDE('ClaRunExt.INC'),ONCE
  INCLUDE('CWSYNCHC.INC'),ONCE

COMPILE('***', _C100_)
  PRAGMA ('project(#file copy clarunext.dll)')
!***

  MAP
TestFnPtr   PROCEDURE(LONG fnPtr, STRING functionName, BOOL displayMessage, <*STRING errMessage>),BOOL
    MODULE('Win32Api')
      ClaRunExt_LoadLibrary(CONST *CSTRING pszModuleFileName), UNSIGNED, PASCAL, NAME('LoadLibraryA')
      ClaRunExt_FreeLibrary(UNSIGNED hModule), BOOL, PASCAL, PROC, NAME('FreeLibrary')
      ClaRunExt_GetModuleHandle(CONST *CSTRING pszModuleName), UNSIGNED, PASCAL, NAME('GetModuleHandleA')
      ClaRunExt_GetProcAddress(UNSIGNED hModule, CONST *CSTRING pszProcName), LONG, PASCAL, NAME('GetProcAddress')
      ClaRunExt_GetLastError(), ULONG, PASCAL, NAME('GetLastError')
    END
    MODULE('ClaRunExtDLL')
      ImageToPNG1_ClaRunExt (CONST *STRING fileNameIn, LONG inLen),LONG,C,RAW,DLL(1)!Save image to a PNG file, changing the fileName extension to PNG. Return 0 if no error and Error Number if there is an error
      ImageToPNG2_ClaRunExt (CONST *STRING fileNameIn, LONG inLen, CONST *STRING fileNameOut, LONG outLen),LONG,C,RAW,DLL(1)!Save image to a PNG file with the name fileNameOut. Return 0 if no error and Error Number if there is an error
      ImageToPNG3_ClaRunExt (CONST *STRING fileNameIn, LONG inLen, CONST *STRING fileNameOut, LONG outLen, LONG imageRes),LONG,C,RAW,DLL(1)!Save image to a PNG file with the name fileNameOut using the imageRes resolution. Return 0 if no error and Error Number if there is an error
      ImageRotateFlip_ClaRunExt (CONST *STRING fileNameIn, LONG inLen, CONST *STRING fileNameOut, LONG outLen, LONG RotateFlipType),LONG,C,RAW,DLL(1)!Save image rotated or fliped. Return 0 if no error and Error Number if there is an error
      ImageSaveThumbnail_ClaRunExt (CONST *STRING fileNameIn, LONG inLen, CONST *STRING fileNameOut, LONG outLen, LONG percentage),LONG,C,RAW,DLL(1)!Save image reduced in size. Return 0 if no error and Error Number if there is an error
      SendSMS_ClaRunExt (CONST *STRING SMTPServerHost, LONG hostLen, CONST *STRING SMTPLoginUserName, LONG nameLen, CONST *STRING SMTPLoginPassword, LONG pwdLen, LONG SMTPPort, LONG SMTPEnableSsl, LONG showErrorMessage, | 
        CONST *STRING  fromAddress, LONG fromLen, CONST *STRING  toPhoneNumber, LONG toLen, CONST *STRING  toCarrier, LONG carrierLen, CONST *STRING  subject, LONG subjectLen, CONST *STRING  textMessage, LONG messageLen, | 
        *STRING errMessage, LONG errMessageLen),LONG,C,RAW,DLL(1)!Send a SMS message
      SendMail_ClaRunExt (CONST *STRING SMTPServerHost, LONG hostLen, CONST *STRING SMTPLoginUserName, LONG nameLen, CONST *STRING SMTPLoginPassword, LONG pwdLen, LONG SMTPPort, LONG SMTPEnableSsl, LONG showErrorMessage, | 
        LONG receiveRequest, CONST *STRING messageBodyEncoding, LONG encodingLen, CONST *STRING fromAddress, LONG fromLen, CONST *STRING toAddress, LONG toLen, CONST *STRING replyToAddress, LONG replyLen, | 
        CONST *STRING subject, LONG subjectLen, CONST *STRING htmlMessage, LONG htmlLen, CONST *STRING embeddedImageFileNames, LONG imagesLen, CONST *STRING textMessage, LONG textLen, CONST *STRING attachedFileNames, |
        LONG attachmentsLen, CONST *STRING ccList, LONG ccLen, CONST *STRING bccList, LONG bccLen, *STRING errMessage, LONG errMessageLen),LONG,C,RAW,DLL
      HttpWebRequest_ClaRunExt (CONST *CSTRING requestEndpoint,LONG httpVerbMethod,CONST *CSTRING postData,CONST *CSTRING requestParameters,*CSTRING responseOut),LONG,C,DLL(1)!HttpWebRequest
      HttpWebRequestExt_ClaRunExt (CONST *CSTRING requestEndpoint,LONG httpVerbMethod,CONST *CSTRING postData,CONST *CSTRING requestParameters,CONST *CSTRING contentType, CONST *CSTRING headers,*CSTRING responseOut),LONG,C,DLL(1)!HttpWebRequest
      HttpWebRequestToFile_ClaRunExt (CONST *CSTRING requestEndpoint,LONG httpVerbMethod,CONST *CSTRING postData,CONST *CSTRING requestParameters,*CSTRING errorOut, CONST *CSTRING outputFilename),LONG,C,DLL(1)!HttpWebRequest
      HttpWebRequestToFileExt_ClaRunExt (CONST *CSTRING requestEndpoint,LONG httpVerbMethod,CONST *CSTRING postData,CONST *CSTRING requestParameters,CONST *CSTRING contentType, CONST *CSTRING headers,*CSTRING errorOut, CONST *CSTRING outputFilename),LONG,C,DLL(1)!HttpWebRequest
      GetAWSRegion_ClaRunExt(*STRING displayName, LONG displayNameLen, *STRING systemName, LONG systemNameLen, *STRING errorMessage, LONG errMessageLen, *STRING errorCode, LONG errorCodeLen),LONG,C,RAW,DLL
      ConnectToS3_ClaRunExt (*STRING AccessKey, LONG AccessKeyLen, *STRING SecretKey, LONG SecretKeyLen, *STRING ErrorMessage, LONG errMessageLen, *STRING errorCode, LONG errorCodeLen),LONG,C,RAW,DLL
      DisconnectFromS3_ClaRunExt(LONG netPtr),C,RAW,DLL
      GetContainer_ClaRunExt(LONG netPtr, *STRING container, LONG containerLen, *STRING regionDisplayName, LONG regionDisplayNameLen, *STRING regionSystemName, LONG regionSystemNameLen, *STRING ErrorMessage, LONG errMessageLen, *STRING errorCode, LONG errorCodeLen),LONG,C,RAW,DLL
      CreateContainer_ClaRunExt(LONG netPtr, *STRING container, LONG containerLen, *STRING region, LONG regionLen, *STRING ErrorMessage, LONG errMessageLen, *STRING errorCode, LONG errorCodeLen),LONG,C,RAW,DLL
      DeleteContainer_ClaRunExt(LONG netPtr, *STRING container, LONG containerLen, *STRING ErrorMessage, LONG errMessageLen, *STRING errorCode, LONG errorCodeLen),LONG,C,RAW,DLL
      CreateFolder_ClaRunExt(LONG netPtr, *STRING container, LONG containerLen, *STRING folder, LONG folderLen, *STRING ErrorMessage, LONG errMessageLen, *STRING errorCode, LONG errorCodeLen),LONG,C,RAW,DLL
      DeleteFolder_ClaRunExt(LONG netPtr, *STRING container, LONG containerLen, *STRING folder, LONG folderLen, *STRING ErrorMessage, LONG errMessageLen, *STRING errorCode, LONG errorCodeLen),LONG,C,RAW,DLL
      GetNextFile_ClaRunExt(LONG netPtr, *STRING container, LONG containerLen, *STRING parentFolder, LONG parentFolderLen, *STRING fileName, LONG fileNameLen, *BOOL isFile, *DATE lmDate, *TIME lmTime, *ULONG sizeLo, *LONG sizeHi, *STRING errorMessage, LONG errorMessageLen, *STRING errorCode, LONG errorCodeLen),LONG,C,RAW,DLL
      UploadFile_ClaRunExt (LONG netPtr, *STRING sourceFile, LONG sourceFileLen, *STRING container, LONG containerLen, *STRING destinationFile, LONG destinationFileLen, BOOL asynchronous, BOOL encrypted, IAWSTransfering callback, *STRING ErrorMessage, LONG errMessageLen, *STRING errorCode, LONG errorCodeLen),LONG,C,RAW,DLL
      DownloadFile_ClaRunExt (LONG netPtr, *STRING container, LONG containerLen, *STRING sourceFile, LONG sourceFileLen, *STRING destinationFile, LONG destinationFileLen, BOOL asynchronous, IAWSTransfering callback, *STRING ErrorMessage, LONG errMessageLen, *STRING errorCode, LONG errorCodeLen),LONG,C,RAW,DLL
      DeleteFile_ClaRunExt (LONG netPtr, *STRING container, LONG containerLen, *STRING fileName, LONG fileNameLen, *STRING ErrorMessage, LONG errMessageLen, *STRING errorCode, LONG errorCodeLen),LONG,C,RAW,DLL
      FileExists_ClaRunExt (LONG netPtr, *STRING container, LONG containerLen, *STRING fileName, LONG fileNameLen, *STRING ErrorMessage, LONG errMessageLen, *STRING errorCode, LONG errorCodeLen),LONG,C,RAW,DLL
      CancelTransfer_ClaRunExt(LONG netPtr, *STRING ErrorMessage, LONG errMessageLen, *STRING errorCode, LONG errorCodeLen),LONG,C,RAW,DLL
      GetTemporaryURL_ClaRunExt (LONG netPtr, *STRING container, LONG containerLen, *STRING fileName, LONG fileNameLen, LONG days, LONG hours, LONG minutes, *STRING ret, LONG retLen, *STRING ErrorMessage, LONG errMessageLen, *STRING errorCode, LONG errorCodeLen),C,RAW,DLL
      GetPermanentURL_ClaRunExt (LONG netPtr, *STRING container, LONG containerLen, *STRING fileName, LONG fileNameLen, *STRING ret, LONG retLen, *STRING ErrorMessage, LONG errMessageLen, *STRING errorCode, LONG errorCodeLen),C,RAW,DLL
      AllowPublicAccess_ClaRunExt (LONG netPtr, *STRING container, LONG containerLen, *STRING fileName, LONG fileNameLen, LONG public, *STRING ErrorMessage, LONG errMessageLen, *STRING errorCode, LONG errorCodeLen),LONG,C,RAW,DLL
      HasPublicAccess_ClaRunExt (LONG netPtr, *STRING container, LONG containerLen, *STRING fileName, LONG fileNameLen, *STRING ErrorMessage, LONG errMessageLen, *STRING errorCode, LONG errorCodeLen),LONG,C,RAW,DLL
      GetNextMetaData_ClaRunExt (LONG netPtr, *STRING container, LONG containerLen, *STRING fileName, LONG fileNameLen, *STRING key, LONG keyLen, *STRING value, LONG valueLen, *STRING ErrorMessage, LONG errMessageLen, *STRING errorCode, LONG errorCodeLen),LONG,C,RAW,DLL
      GetMetaData_ClaRunExt (LONG netPtr, *STRING container, LONG containerLen, *STRING fileName, LONG fileNameLen, STRING key, LONG keyLen, *STRING ret, LONG retLen, *STRING ErrorMessage, LONG errMessageLen, *STRING errorCode, LONG errorCodeLen),C,RAW,DLL
      SetAllMetaData_ClaRunExt (LONG netPtr, *STRING container, LONG containerLen, *STRING fileName, LONG fileNameLen, STRING metaData, LONG metaDataLen, *STRING ErrorMessage, LONG errMessageLen, *STRING errorCode, LONG errorCodeLen),LONG,C,RAW,DLL
      SetMetaData_ClaRunExt (LONG netPtr, *STRING container, LONG containerLen, *STRING fileName, LONG fileNameLen, STRING key, LONG keyLen, STRING value, LONG valueLen, *STRING ErrorMessage, LONG errMessageLen, *STRING errorCode, LONG errorCodeLen),LONG,C,RAW,DLL
      RemoveMetaData_ClaRunExt (LONG netPtr, *STRING container, LONG containerLen, *STRING fileName, LONG fileNameLen, STRING key, LONG keyLen, *STRING ErrorMessage, LONG errMessageLen, *STRING errorCode, LONG errorCodeLen),LONG,C,RAW,DLL
    END
  END


!SendSMS and SendMail
!return error values:
!0  = No Error
!1  = SMTP Error
!2  = Parameter Format Error
!3  = No ClaRunExt.dll
!99 = Unknow Error


!Note:
!To add more functions supported in ClaRunExt.DLL:
!1) Add the new function to the MODULE('ClaRunExtDLL') in the MAP
!2) Add new LONG with the External Name with the exact same name as the function in the map
!3) Add a new CSTRING with the function name from Libmaker as PRIVATE
!4) Add a Method to the ClaRunExtLibClass for that new function
!5) Add a Method to the ClaRunExtClass for that new function
!6) Add the code to the ClaRunExtLibClass.Init method to assign the LONG variable created on step 2 using the value of the CSTRING created on step 3
!7) Create a method on ClaRunExtLibClass with the name of the function that does the Init verification and the call to the Function declared in the MAP
!8) Create a method on ClaRunExtClass that calls the method to the ClaRunExtLibClass

!Class Static Data

ImageToPNG1_ClaRunExt_fp    LONG,NAME('ImageToPNG1_ClaRunExt')
ImageToPNG2_ClaRunExt_fp    LONG,NAME('ImageToPNG2_ClaRunExt')
ImageToPNG3_ClaRunExt_fp    LONG,NAME('ImageToPNG3_ClaRunExt')
ImageRotateFlip_ClaRunExt_fp    LONG,NAME('ImageRotateFlip_ClaRunExt')
ImageSaveThumbnail_ClaRunExt_fp LONG,NAME('ImageSaveThumbnail_ClaRunExt')
SendSMS_ClaRunExt_fp    LONG,NAME('SendSMS_ClaRunExt')
SendMail_ClaRunExt_fp   LONG,NAME('SendMail_ClaRunExt')
HttpWebRequest_ClaRunExt_fp     LONG,NAME('HttpWebRequest_ClaRunExt')
HttpWebRequestExt_ClaRunExt_fp  LONG,NAME('HttpWebRequestExt_ClaRunExt')
HttpWebRequestToFile_ClaRunExt_fp LONG,NAME('HttpWebRequestToFile_ClaRunExt')
HttpWebRequestToFileExt_ClaRunExt_fp LONG,NAME('HttpWebRequestToFileExt_ClaRunExt')
GetAWSRegion_ClaRunExt_fp   LONG,NAME('GetAWSRegion_ClaRunExt')
ConnectToS3_ClaRunExt_fp   LONG,NAME('ConnectToS3_ClaRunExt')
DisconnectFromS3_ClaRunExt_fp   LONG,NAME('DisconnectFromS3_ClaRunExt')
GetContainer_ClaRunExt_fp   LONG,NAME('GetContainer_ClaRunExt')
CreateContainer_ClaRunExt_fp   LONG,NAME('CreateContainer_ClaRunExt')
DeleteContainer_ClaRunExt_fp   LONG,NAME('DeleteContainer_ClaRunExt')
GetNextFile_ClaRunExt_fp   LONG,NAME('GetNextFile_ClaRunExt')
CreateFolder_ClaRunExt_fp    LONG,NAME('CreateFolder_ClaRunExt')
DeleteFolder_ClaRunExt_fp   LONG,NAME('DeleteFolder_ClaRunExt')
UploadFile_ClaRunExt_fp LONG,NAME('UploadFile_ClaRunExt')
DownloadFile_ClaRunExt_fp   LONG,NAME('DownloadFile_ClaRunExt')
DeleteFile_ClaRunExt_fp LONG,NAME('DeleteFile_ClaRunExt')
FileExists_ClaRunExt_fp LONG,NAME('FileExists_ClaRunExt')
CancelTransfer_ClaRunExt_fp LONG,NAME('CancelTransfer_ClaRunExt')
GetTemporaryURL_ClaRunExt_fp    LONG,NAME('GetTemporaryURL_ClaRunExt')
GetPermanentURL_ClaRunExt_fp    LONG,NAME('GetPermanentURL_ClaRunExt')
AllowPublicAccess_ClaRunExt_fp  LONG,NAME('AllowPublicAccess_ClaRunExt')
HasPublicAccess_ClaRunExt_fp  LONG,NAME('HasPublicAccess_ClaRunExt')
GetNextMetaData_ClaRunExt_fp  LONG,NAME('GetNextMetaData_ClaRunExt')
GetMetaData_ClaRunExt_fp  LONG,NAME('GetMetaData_ClaRunExt')
SetAllMetaData_ClaRunExt_fp  LONG,NAME('SetAllMetaData_ClaRunExt')
SetMetaData_ClaRunExt_fp  LONG,NAME('SetMetaData_ClaRunExt')
RemoveMetaData_ClaRunExt_fp  LONG,NAME('RemoveMetaData_ClaRunExt')

ClaRunExtLibraryModuleName  CSTRING('ClaRunExt.DLL'),PRIVATE
ImageToPNG1_ClaRunExt_fn    CSTRING('?ImageToPNG1@@YAJPBDJ@Z'),PRIVATE
ImageToPNG2_ClaRunExt_fn    CSTRING('?ImageToPNG2@@YAJPBDJ0J@Z'),PRIVATE
ImageToPNG3_ClaRunExt_fn    CSTRING('?ImageToPNG3@@YAJPBDJ0JJ@Z'),PRIVATE
ImageRotateFlip_ClaRunExt_fn    CSTRING('?RotateFlip@@YAJPBDJ0JJ@Z'),PRIVATE
ImageSaveThumbnail_ClaRunExt_fn CSTRING('?SaveThumbnail@@YAJPBDJ0JJ@Z'),PRIVATE
SendSMS_ClaRunExt_fn    CSTRING('?SendSMS@@YAJPBDJ0J0JJJJ0J0J0J0J0JPADJ@Z'),PRIVATE
SendMail_ClaRunExt_fn   CSTRING('?SendMail@@YAJPBDJ0J0JJJJJ0J0J0J0J0J0J0J0J0J0J0JPADJ@Z'),PRIVATE
HttpWebRequest_ClaRunExt_fn     CSTRING('?MakeHttpWebRequest@@YAHPBDH00IPAD@Z'),PRIVATE
HttpWebRequestExt_ClaRunExt_fn  CSTRING('?MakeHttpWebRequestExt@@YAHPBDH0000IPAD@Z'),PRIVATE
HttpWebRequestToFile_ClaRunExt_fn CSTRING('?MakeHttpWebRequestToFile@@YAHPBDH00IPAD0@Z'),PRIVATE
HttpWebRequestToFileExt_ClaRunExt_fn CSTRING('?MakeHttpWebRequestToFileExt@@YAHPBDH0000IPAD0@Z'),PRIVATE
GetAWSRegion_ClaRunExt_fn   CSTRING('?GetAWSRegion@@YAJPADJ0J0J@Z'),PRIVATE
ConnectToS3_ClaRunExt_fn   CSTRING('?ConnectToS3@@YAJPBDJ0JPADJ1J@Z'),PRIVATE
DisconnectFromS3_ClaRunExt_fn   CSTRING('?DisconnectFromS3@@YAXJ@Z'),PRIVATE
GetContainer_ClaRunExt_fn   CSTRING('?GetContainer@@YAJJPADJ0J0J0J0J@Z'),PRIVATE
CreateContainer_ClaRunExt_fn   CSTRING('?CreateContainer@@YAJJPBDJ0JPADJ1J@Z'),PRIVATE
DeleteContainer_ClaRunExt_fn   CSTRING('?DeleteContainer@@YAJJPBDJPADJ1J@Z'),PRIVATE
CreateFolder_ClaRunExt_fn    CSTRING('?CreateFolder@@YAJJPBDJ0JPADJ1J@Z'),PRIVATE
DeleteFolder_ClaRunExt_fn   CSTRING('?DeleteFolder@@YAJJPBDJ0JPADJ1J@Z'),PRIVATE
GetNextFile_ClaRunExt_fn    CSTRING('?GetNextFile@@YAJJPADJ0J0JPAJ11PAK10J0J@Z'),PRIVATE
UploadFile_ClaRunExt_fn CSTRING('?UploadFile@@YAJJPADJ0J0JJJPAVIAWSTransfer@@0J0J@Z'),PRIVATE
DownloadFile_ClaRunExt_fn   CSTRING('?DownloadFile@@YAJJPADJ0J0JJPAVIAWSTransfer@@0J0J@Z'),PRIVATE
DeleteFile_ClaRunExt_fn CSTRING('?DeleteFile@@YAJJPADJ0J0J0J@Z'),PRIVATE
FileExists_ClaRunExt_fn CSTRING('?FileExists@@YAJJPADJ0J0J0J@Z'),PRIVATE
CancelTransfer_ClaRunExt_fn CSTRING('?CancelTransfer@@YAJJPADJ0J@Z'),PRIVATE
GetTemporaryURL_ClaRunExt_fn    CSTRING('?GetTemporaryURL@@YAXJPADJ0JJJJ0J0J0J@Z'),PRIVATE
GetPermanentURL_ClaRunExt_fn    CSTRING('?GetPermanentURL@@YAXJPADJ0J0J0J0J@Z'),PRIVATE
AllowPublicAccess_ClaRunExt_fn  CSTRING('?AllowPublicAccess@@YAJJPADJ0JJ0J0J@Z'),PRIVATE
HasPublicAccess_ClaRunExt_fn  CSTRING('?HasPublicAccess@@YAJJPADJ0J0J0J@Z'),PRIVATE
GetNextMetaData_ClaRunExt_fn  CSTRING('?GetNextMetaData@@YAJJPADJ0J0J0J0J0J@Z'),PRIVATE
GetMetaData_ClaRunExt_fn  CSTRING('?GetMetaData@@YAJJPADJ0JPBDJ0J0J0J@Z'),PRIVATE
SetAllMetaData_ClaRunExt_fn  CSTRING('?SetAllMetaData@@YAJJPADJ0JPBDJ0J0J@Z'),PRIVATE
SetMetaData_ClaRunExt_fn  CSTRING('?SetMetaData@@YAJJPADJ0JPBDJ1J0J0J@Z'),PRIVATE
RemoveMetaData_ClaRunExt_fn  CSTRING('?RemoveMetaData@@YAJJPADJ0JPBDJ0J0J@Z'),PRIVATE

!-------------------------------------------------------------
!This class is used to keep one instance of the ClaRunExt.DLL Library per application
!
ClaRunExtLibClass   CLASS,PRIVATE
hLib                  UNSIGNED,PRIVATE
Users                 UNSIGNED,PRIVATE
Inited                BYTE,PRIVATE
cs                    &CriticalSection

Construct             PROCEDURE()
Destruct              PROCEDURE()

Init                  PROCEDURE(BOOL displayMessage, <*STRING errMessage>),BYTE,PRIVATE
Use                   PROCEDURE()
Kill                  PROCEDURE()
Disconnect            PROCEDURE()

ImageToPNG            PROCEDURE(STRING fileNameIn),LONG!Save image to a PNG file, changing the fileName extension to PNG. Return 0 if no error and Error Number if there is an error
ImageToPNG            PROCEDURE(STRING fileNameIn, STRING fileNameOut),LONG!Save image to a PNG file with the name fileNameOut. Return 0 if no error and Error Number if there is an error
ImageToPNG            PROCEDURE(STRING fileNameIn, STRING fileNameOut, USHORT imageRes),LONG!Save image to a PNG file with the name fileNameOut using the imageRes resolution. Return 0 if no error and Error Number if there is an error
ImageRotateFlip       PROCEDURE(STRING fileNameIn, STRING fileNameOut, LONG RotateFlipType),LONG!Save image rotated or fliped. Return 0 if no error and Error Number if there is an error
ImageSaveThumbnail    PROCEDURE(STRING fileNameIn, STRING fileNameOut, LONG porcentage),LONG!Save image reduced in size. Return 0 if no error and Error Number if there is an error
SendSMS               PROCEDURE(STRING SMTPServerHost, STRING SMTPLoginUserName, STRING SMTPLoginPassword, LONG SMTPPort, LONG SMTPEnableSsl, LONG showErrorMessage, STRING  fromAddress, STRING  toPhoneNumber, STRING  toCarrier, STRING  subject, STRING  textMessage, <*STRING errMessage>),LONG
SendMail              PROCEDURE(STRING SMTPServerHost, STRING SMTPLoginUserName, STRING SMTPLoginPassword, LONG SMTPPort, LONG SMTPEnableSsl, LONG showErrorMessage, LONG receiveRequest, STRING messageBodyEncoding, STRING fromAddress, STRING toAddress, STRING replyToAddress, STRING subject, STRING htmlMessage, STRING embeddedImageFileNames, STRING textMessage, STRING attachedFileNames, STRING ccList, STRING bccList, <*STRING errMessage>),LONG
HttpWebRequest          PROCEDURE(CONST *CSTRING requestEndpoint,LONG httpVerbMethod,CONST *CSTRING postData,CONST *CSTRING requestParameters,*CSTRING responseOut),LONG
HttpWebRequest          PROCEDURE(CONST *CSTRING requestEndpoint,LONG httpVerbMethod,CONST *CSTRING postData,CONST *CSTRING requestParameters,CONST *CSTRING contentType, CONST *CSTRING headers,*CSTRING responseOut),LONG
HttpWebRequestToFile    PROCEDURE(CONST *CSTRING requestEndpoint,LONG httpVerbMethod,CONST *CSTRING postData,CONST *CSTRING requestParameters,*CSTRING errorOut, CONST *CSTRING outputFilename),LONG
HttpWebRequestToFile    PROCEDURE(CONST *CSTRING requestEndpoint,LONG httpVerbMethod,CONST *CSTRING postData,CONST *CSTRING requestParameters,CONST *CSTRING contentType, CONST *CSTRING headers,*CSTRING errorOut, CONST *CSTRING outputFilename),LONG
GetAWSRegion          PROCEDURE(*STRING displayName, *STRING systemName, <*STRING errorMessage>, <*STRING errorCode>),LONG
ConnectToS3           PROCEDURE(STRING AccessKey, STRING SecretKey, <*STRING ErrorMessage>, <*STRING errorCode>),LONG
DisconnectFromS3      PROCEDURE(LONG service)
GetContainer          PROCEDURE(LONG ptr, *STRING containerName, *STRING regionDisplayName, *STRING regionSystemName, *STRING errorMessage, *STRING errorCode),LONG
CreateContainer       PROCEDURE(LONG ptr, STRING containerName, STRING region, *STRING errorMessage, *STRING errorCode),LONG
DeleteContainer       PROCEDURE(LONG ptr, STRING containerName, *STRING errorMessage, *STRING errorCode),LONG
CreateFolder          PROCEDURE(LONG ptr, STRING containerName, STRING folder, *STRING errorMessage, *STRING errorCode),LONG
DeleteFolder          PROCEDURE(LONG ptr, STRING containerName, STRING folder, *STRING errorMessage, *STRING errorCode),LONG
GetNextFile           PROCEDURE(LONG ptr, STRING containerName, STRING parentFolder, *STRING fileName, *BOOL isFile, *DATE lmDate, *TIME lmTime, *INT64 size, *STRING errorMessage, *STRING errorCode),LONG
UploadFile            PROCEDURE(LONG ptr, STRING sourceFile, STRING containerName, STRING destinationFile, BOOL asynchronous, BOOL encrypted, IAWSTransfering callback, *STRING errorMessage, *STRING errorCode),LONG
DownloadFile          PROCEDURE(LONG ptr, STRING containerName, STRING sourceFile, STRING destinationFile, BOOL asynchronous, IAWSTransfering callback, *STRING errorMessage, *STRING errorCode),LONG
DeleteFile            PROCEDURE(LONG ptr, STRING containerName, STRING fileName, *STRING errorMessage, *STRING errorCode),LONG
FileExists            PROCEDURE(LONG ptr, STRING containerName, STRING fileName, *STRING errorMessage, *STRING errorCode),LONG
CancelTransfer        PROCEDURE(LONG ptr, *STRING errorMessage, *STRING errorCode),LONG
GetTemporaryURL       PROCEDURE(LONG ptr, STRING containerName, STRING fileName, LONG days, LONG hours, LONG minutes, *STRING errorMessage, *STRING errorCode),STRING
GetPermanentURL       PROCEDURE(LONG ptr, STRING containerName, STRING fileName, *STRING errorMessage, *STRING errorCode),STRING
AllowPublicAccess     PROCEDURE(LONG ptr, STRING containerName, STRING fileName, BOOL public, *STRING errorMessage, *STRING errorCode),LONG
HasPublicAccess       PROCEDURE(LONG ptr, STRING containerName, STRING fileName, *STRING errorMessage, *STRING errorCode),BOOL
GetNextMetaData       PROCEDURE(LONG ptr, STRING containerName, STRING fileName, *STRING key, *STRING value, *STRING errorMessage, *STRING errorCode),BOOL
GetMetaData           PROCEDURE(LONG ptr, STRING containerName, STRING fileName, STRING key, *STRING errorMessage, *STRING errorCode),STRING
SetMetaData           PROCEDURE(LONG ptr, STRING containerName, STRING fileName, STRING metaData, *STRING errorMessage, *STRING errorCode),BOOL
SetMetaData           PROCEDURE(LONG ptr, STRING containerName, STRING fileName, STRING key, STRING value, *STRING errorMessage, *STRING errorCode),LONG
RemoveMetaData        PROCEDURE(LONG ptr, STRING containerName, STRING fileName, STRING key, *STRING errorMessage, *STRING errorCode),LONG
                    END
!-------------------------------------------------------------
TestFnPtr           PROCEDURE(LONG fnPtr, STRING functionName, BOOL displayMessage, <*STRING errMessage>)
errorDetails          STRING(50)
  CODE
  IF fnPtr = 0
    errorDetails = functionName & ' was not found in ClaRunExt.DLL'
    IF NOT OMITTED(errMessage)
      errMessage = errorDetails
    END
    IF displayMessage
      MESSAGE(errorDetails)
    END
    RETURN FALSE
  END
  RETURN TRUE

!region ClaRunExtLibClass
!region Initialisation/Destruction
ClaRunExtLibClass.Construct PROCEDURE()
  CODE
  SELF.hLib   = 0
  SELF.Users  = 0
  SELF.Inited = FALSE
  RETURN

ClaRunExtLibClass.Destruct  PROCEDURE()
  CODE
  SELF.Kill()
  RETURN

ClaRunExtLibClass.Init  PROCEDURE(BOOL displayMessage, <*STRING errMessage>)
errorText                     STRING('ClaRunExt.DLL Could not be found')
functionsExist                BOOL
  CODE
  IF NOT SELF.Inited
    SELF.hLib = ClaRunExt_LoadLibrary (ClaRunExtLibraryModuleName)
    IF SELF.hLib = 0
      IF NOT OMITTED(errMessage)
        errMessage = errorText
      END
      IF displayMessage
        MESSAGE(errorText)
      END
      RETURN FALSE
    END

    ! LONG Function pointer = GetProcAddress(ModuleHandle, CSTRING Function Name)
    ImageToPNG1_ClaRunExt_fp = ClaRunExt_GetProcAddress (SELF.hLib, ImageToPNG1_ClaRunExt_fn)
    functionsExist = TestFnPtr(ImageToPNG1_ClaRunExt_fp, 'ImageToPNG1', displayMessage, errMessage)
    ImageToPNG2_ClaRunExt_fp = ClaRunExt_GetProcAddress (SELF.hLib, ImageToPNG2_ClaRunExt_fn)
    functionsExist = CHOOSE(functionsExist AND TestFnPtr(ImageToPNG2_ClaRunExt_fp, 'ImageToPNG2', displayMessage, errMessage))
    ImageToPNG3_ClaRunExt_fp = ClaRunExt_GetProcAddress (SELF.hLib, ImageToPNG3_ClaRunExt_fn)
    functionsExist = CHOOSE(functionsExist AND TestFnPtr(ImageToPNG3_ClaRunExt_fp, 'ImageToPNG3', displayMessage, errMessage))
    ImageRotateFlip_ClaRunExt_fp = ClaRunExt_GetProcAddress (SELF.hLib, ImageRotateFlip_ClaRunExt_fn)
    functionsExist = CHOOSE(functionsExist AND TestFnPtr(ImageRotateFlip_ClaRunExt_fp, 'ImageRotateFlip', displayMessage, errMessage))
    ImageSaveThumbnail_ClaRunExt_fp = ClaRunExt_GetProcAddress (SELF.hLib, ImageSaveThumbnail_ClaRunExt_fn)
    functionsExist = CHOOSE(functionsExist AND TestFnPtr(ImageSaveThumbnail_ClaRunExt_fp, 'ImageSaveThumbnail', displayMessage, errMessage))
    SendSMS_ClaRunExt_fp = ClaRunExt_GetProcAddress (SELF.hLib, SendSMS_ClaRunExt_fn)
    functionsExist = CHOOSE(functionsExist AND TestFnPtr(SendSMS_ClaRunExt_fp, 'SendSMS', displayMessage, errMessage))
    SendMail_ClaRunExt_fp = ClaRunExt_GetProcAddress (SELF.hLib, SendMail_ClaRunExt_fn)
    functionsExist = CHOOSE(functionsExist AND TestFnPtr(SendMail_ClaRunExt_fp, 'SendMail', displayMessage, errMessage))
    HttpWebRequest_ClaRunExt_fp    = ClaRunExt_GetProcAddress (SELF.hLib, HttpWebRequest_ClaRunExt_fn)
    functionsExist = CHOOSE(functionsExist AND TestFnPtr(HttpWebRequest_ClaRunExt_fp, 'HttpWebRequest', displayMessage, errMessage))
    HttpWebRequestExt_ClaRunExt_fp = ClaRunExt_GetProcAddress (SELF.hLib, HttpWebRequestExt_ClaRunExt_fn)
    functionsExist = CHOOSE(functionsExist AND TestFnPtr(HttpWebRequestExt_ClaRunExt_fp, 'HttpWebRequestExt', displayMessage, errMessage))
    HttpWebRequestToFile_ClaRunExt_fp    = ClaRunExt_GetProcAddress (SELF.hLib, HttpWebRequestToFile_ClaRunExt_fn)
    functionsExist = CHOOSE(functionsExist AND TestFnPtr(HttpWebRequestToFile_ClaRunExt_fp, 'HttpWebRequestToFile', displayMessage, errMessage))
    HttpWebRequestToFileExt_ClaRunExt_fp = ClaRunExt_GetProcAddress (SELF.hLib, HttpWebRequestToFileExt_ClaRunExt_fn)
    functionsExist = CHOOSE(functionsExist AND TestFnPtr(HttpWebRequestToFileExt_ClaRunExt_fp, 'HttpWebRequestToFileExt', displayMessage, errMessage))
    GetAWSRegion_ClaRunExt_fp = ClaRunExt_GetProcAddress (SELF.hLib, GetAWSRegion_ClaRunExt_fn)
    functionsExist = CHOOSE(functionsExist AND TestFnPtr(GetAWSRegion_ClaRunExt_fp, 'GetAWSRegion', displayMessage, errMessage))
    ConnectToS3_ClaRunExt_fp = ClaRunExt_GetProcAddress (SELF.hLib, ConnectToS3_ClaRunExt_fn)
    functionsExist = CHOOSE(functionsExist AND TestFnPtr(ConnectToS3_ClaRunExt_fp, 'ConnectToS3', displayMessage, errMessage))
    DisconnectFromS3_ClaRunExt_fp = ClaRunExt_GetProcAddress (SELF.hLib, DisconnectFromS3_ClaRunExt_fn)
    functionsExist = CHOOSE(functionsExist AND TestFnPtr(DisconnectFromS3_ClaRunExt_fp, 'DisconnectFromS3', displayMessage, errMessage))
    GetContainer_ClaRunExt_fp = ClaRunExt_GetProcAddress (SELF.hLib, GetContainer_ClaRunExt_fn)
    functionsExist = CHOOSE(functionsExist AND TestFnPtr(GetContainer_ClaRunExt_fp, 'GetContainer', displayMessage, errMessage))
    CreateContainer_ClaRunExt_fp = ClaRunExt_GetProcAddress (SELF.hLib, CreateContainer_ClaRunExt_fn)
    functionsExist = CHOOSE(functionsExist AND TestFnPtr(CreateContainer_ClaRunExt_fp, 'CreateContainer', displayMessage, errMessage))
    DeleteContainer_ClaRunExt_fp = ClaRunExt_GetProcAddress (SELF.hLib, DeleteContainer_ClaRunExt_fn)
    functionsExist = CHOOSE(functionsExist AND TestFnPtr(DeleteContainer_ClaRunExt_fp, 'DeleteContainer', displayMessage, errMessage))
    GetNextFile_ClaRunExt_fp = ClaRunExt_GetProcAddress (SELF.hLib, GetNextFile_ClaRunExt_fn)
    functionsExist = CHOOSE(functionsExist AND TestFnPtr(GetNextFile_ClaRunExt_fp, 'GetNextFile', displayMessage, errMessage))
    CreateFolder_ClaRunExt_fp = ClaRunExt_GetProcAddress (SELF.hLib, CreateFolder_ClaRunExt_fn)
    functionsExist = CHOOSE(functionsExist AND TestFnPtr(CreateFolder_ClaRunExt_fp, 'CreateFolder', displayMessage, errMessage))
    DeleteFolder_ClaRunExt_fp = ClaRunExt_GetProcAddress (SELF.hLib, DeleteFolder_ClaRunExt_fn)
    functionsExist = CHOOSE(functionsExist AND TestFnPtr(DeleteFolder_ClaRunExt_fp, 'DeleteFolder', displayMessage, errMessage))
    UploadFile_ClaRunExt_fp = ClaRunExt_GetProcAddress (SELF.hLib, UploadFile_ClaRunExt_fn)
    functionsExist = CHOOSE(functionsExist AND TestFnPtr(UploadFile_ClaRunExt_fp, 'UploadFile', displayMessage, errMessage))
    DownloadFile_ClaRunExt_fp = ClaRunExt_GetProcAddress (SELF.hLib, DownloadFile_ClaRunExt_fn)
    functionsExist = CHOOSE(functionsExist AND TestFnPtr(DownloadFile_ClaRunExt_fp, 'DownloadFile', displayMessage, errMessage))
    DeleteFile_ClaRunExt_fp = ClaRunExt_GetProcAddress (SELF.hLib, DeleteFile_ClaRunExt_fn)
    functionsExist = CHOOSE(functionsExist AND TestFnPtr(DeleteFile_ClaRunExt_fp, 'DeleteFile', displayMessage, errMessage))
    FileExists_ClaRunExt_fp = ClaRunExt_GetProcAddress (SELF.hLib, FileExists_ClaRunExt_fn)
    functionsExist = CHOOSE(functionsExist AND TestFnPtr(FileExists_ClaRunExt_fp, 'FileExists', displayMessage, errMessage))
    CancelTransfer_ClaRunExt_fp = ClaRunExt_GetProcAddress (SELF.hLib, CancelTransfer_ClaRunExt_fn)
    functionsExist = CHOOSE(functionsExist AND TestFnPtr(CancelTransfer_ClaRunExt_fp, 'CancelTransfer', displayMessage, errMessage))
    GetTemporaryURL_ClaRunExt_fp = ClaRunExt_GetProcAddress (SELF.hLib, GetTemporaryURL_ClaRunExt_fn)
    functionsExist = CHOOSE(functionsExist AND TestFnPtr(GetTemporaryURL_ClaRunExt_fp, 'GetTemporaryURL', displayMessage, errMessage))
    GetPermanentURL_ClaRunExt_fp = ClaRunExt_GetProcAddress (SELF.hLib, GetPermanentURL_ClaRunExt_fn)
    functionsExist = CHOOSE(functionsExist AND TestFnPtr(GetPermanentURL_ClaRunExt_fp, 'GetPermanentURL', displayMessage, errMessage))
    AllowPublicAccess_ClaRunExt_fp = ClaRunExt_GetProcAddress (SELF.hLib, AllowPublicAccess_ClaRunExt_fn)
    functionsExist = CHOOSE(functionsExist AND TestFnPtr(AllowPublicAccess_ClaRunExt_fp, 'AllowPublicAccess', displayMessage, errMessage))
    HasPublicAccess_ClaRunExt_fp = ClaRunExt_GetProcAddress (SELF.hLib, HasPublicAccess_ClaRunExt_fn)
    functionsExist = CHOOSE(functionsExist AND TestFnPtr(HasPublicAccess_ClaRunExt_fp, 'HasPublicAccess', displayMessage, errMessage))
    GetNextMetaData_ClaRunExt_fp = ClaRunExt_GetProcAddress (SELF.hLib, GetNextMetaData_ClaRunExt_fn)
    functionsExist = CHOOSE(functionsExist AND TestFnPtr(GetNextMetaData_ClaRunExt_fp, 'GetNextMetaData', displayMessage, errMessage))
    GetMetaData_ClaRunExt_fp = ClaRunExt_GetProcAddress (SELF.hLib, GetMetaData_ClaRunExt_fn)
    functionsExist = CHOOSE(functionsExist AND TestFnPtr(GetMetaData_ClaRunExt_fp, 'GetMetaData', displayMessage, errMessage))
    SetAllMetaData_ClaRunExt_fp = ClaRunExt_GetProcAddress (SELF.hLib, SetAllMetaData_ClaRunExt_fn)
    functionsExist = CHOOSE(functionsExist AND TestFnPtr(SetAllMetaData_ClaRunExt_fp, 'SetAllMetaData', displayMessage, errMessage))
    SetMetaData_ClaRunExt_fp = ClaRunExt_GetProcAddress (SELF.hLib, SetMetaData_ClaRunExt_fn)
    functionsExist = CHOOSE(functionsExist AND TestFnPtr(SetMetaData_ClaRunExt_fp, 'SetMetaData', displayMessage, errMessage))
    RemoveMetaData_ClaRunExt_fp = ClaRunExt_GetProcAddress (SELF.hLib, RemoveMetaData_ClaRunExt_fn)
    functionsExist = CHOOSE(functionsExist AND TestFnPtr(RemoveMetaData_ClaRunExt_fp, 'RemoveMetaData', displayMessage, errMessage))

    IF NOT functionsExist
      SELF.Disconnect()
      RETURN FALSE
    END
    SELF.Inited = TRUE
  END
  RETURN TRUE

ClaRunExtLibClass.Use   PROCEDURE()
  CODE
  IF SELF.cs &= NULL
    SELF.cs &= NEW CriticalSection()
  END
  SELF.Users += 1

ClaRunExtLibClass.Kill  PROCEDURE()
disposeCs BOOL
  CODE
  IF SELF.Users
    SELF.cs.Wait()
    SELF.Users -= 1
    IF SELF.Users = 0
      SELF.Disconnect()
      disposeCs = TRUE
    END
    SELF.cs.Release()
    IF disposeCs
      DISPOSE(SELF.cs)
      SELF.cs &= NULL
    END
  END

ClaRunExtLibClass.Disconnect    PROCEDURE()
  CODE
  IF SELF.hLib <> 0
    ClaRunExt_FreeLibrary (SELF.hLib)
    SELF.hLib = 0
  END
!endregion
!region ClaRunExtLibClass function pointer calling methods
ClaRunExtLibClass.ImageToPNG    PROCEDURE(STRING fileNameIn)
  CODE
  IF NOT SELF.Init(TRUE)
    RETURN FALSE
  END
  RETURN ImageToPNG1_ClaRunExt (fileNameIn, LEN(fileNameIn))

ClaRunExtLibClass.ImageToPNG    PROCEDURE(STRING fileNameIn, STRING fileNameOut)
  CODE
  IF NOT SELF.Init(TRUE)
    RETURN FALSE
  END
  RETURN ImageToPNG2_ClaRunExt (fileNameIn, LEN(fileNameIn), fileNameOut, LEN(fileNameOut))

ClaRunExtLibClass.ImageToPNG    PROCEDURE(STRING fileNameIn, STRING fileNameOut, USHORT imageRes)
  CODE
  IF NOT SELF.Init(TRUE)
    RETURN FALSE
  END
  RETURN ImageToPNG3_ClaRunExt (fileNameIn, LEN(fileNameIn), fileNameOut, LEN(fileNameOut), imageRes)

ClaRunExtLibClass.ImageRotateFlip   PROCEDURE(STRING fileNameIn, STRING fileNameOut, LONG RotateFlipType)
  CODE
  IF NOT SELF.Init(TRUE)
    RETURN FALSE
  END
  RETURN ImageRotateFlip_ClaRunExt (fileNameIn, LEN(fileNameIn), fileNameOut, LEN(fileNameOut), RotateFlipType)

ClaRunExtLibClass.ImageSaveThumbnail    PROCEDURE(STRING fileNameIn, STRING fileNameOut, LONG percentage)
  CODE
  IF NOT SELF.Init(TRUE)
    RETURN FALSE
  END
  RETURN ImageSaveThumbnail_ClaRunExt(fileNameIn, LEN(fileNameIn), fileNameOut, LEN(fileNameOut), percentage)


ClaRunExtLibClass.SendSMS   PROCEDURE(STRING SMTPServerHost, STRING SMTPLoginUserName, STRING SMTPLoginPassword, LONG SMTPPort, LONG SMTPEnableSsl, LONG showErrorMessage, STRING  fromAddress, STRING  toPhoneNumber, STRING  toCarrier, STRING  subject, STRING  textMessage, <*STRING errMessage>)
errMsgLen LONG
  CODE
  IF NOT SELF.Init(showErrorMessage, errMessage)
    RETURN 3
	END
	IF NOT OMITTED(errMessage)
		errMsgLen = LEN(errMessage)
	END
  RETURN SendSMS_ClaRunExt (SMTPServerHost, LEN(SMTPServerHost), SMTPLoginUserName, LEN(SMTPLoginUserName), SMTPLoginPassword, LEN(SMTPLoginPassword), | 
    SMTPPort, SMTPEnableSsl, showErrorMessage, fromAddress, LEN(fromAddress), toPhoneNumber, LEN(toPhoneNumber), toCarrier, LEN(toCarrier), subject, | 
    LEN(subject), textMessage, LEN(textMessage), errMessage, errMsgLen)

ClaRunExtLibClass.SendMail  PROCEDURE(STRING SMTPServerHost, STRING SMTPLoginUserName, STRING SMTPLoginPassword, LONG SMTPPort, LONG SMTPEnableSsl, LONG showErrorMessage, LONG receiveRequest, STRING messageBodyEncoding, STRING fromAddress, STRING toAddress, STRING replyToAddress, STRING subject, STRING htmlMessage, STRING embeddedImageFileNames, STRING textMessage, STRING attachedFileNames, STRING ccList, STRING bccList, <*STRING errMessage>)
errMsgLen LONG
  CODE
  IF NOT SELF.Init(showErrorMessage, errMessage)
    RETURN 3
  END
  IF NOT OMITTED(errMessage)
    errMsgLen = LEN(errMessage)
  END
  RETURN SendMail_ClaRunExt (SMTPServerHost, LEN(SMTPServerHost), SMTPLoginUserName, LEN(SMTPLoginUserName), SMTPLoginPassword, LEN(SMTPLoginPassword), | 
    SMTPPort, SMTPEnableSsl, showErrorMessage, receiveRequest, messageBodyEncoding, LEN(messageBodyEncoding), fromAddress, LEN(fromAddress), toAddress, |
    LEN(toAddress), replyToAddress, LEN(replyToAddress), subject, LEN(subject), htmlMessage, LEN(htmlMessage), embeddedImageFileNames, LEN(embeddedImageFileNames), |
    textMessage, LEN(textMessage), attachedFileNames, LEN(attachedFileNames), ccList, LEN(ccList), bccList, LEN(bccList), errMessage, errMsgLen)

ClaRunExtLibClass.HttpWebRequest    PROCEDURE(CONST *CSTRING requestEndpoint,LONG httpVerbMethod,CONST *CSTRING postData,CONST *CSTRING requestParameters,*CSTRING responseOut)
  CODE
  IF NOT SELF.Init(TRUE)
    RETURN FALSE
  END
  RETURN HttpWebRequest_ClaRunExt (requestEndpoint,httpVerbMethod,postData,requestParameters,responseOut)


ClaRunExtLibClass.HttpWebRequest    PROCEDURE(CONST *CSTRING requestEndpoint,LONG httpVerbMethod,CONST *CSTRING postData,CONST *CSTRING requestParameters,CONST *CSTRING contentType, CONST *CSTRING headers,*CSTRING responseOut)
  CODE
  IF NOT SELF.Init(TRUE)
    RETURN FALSE
  END
  RETURN HttpWebRequestExt_ClaRunExt (requestEndpoint,httpVerbMethod,postData,requestParameters,contentType, headers,responseOut)

ClaRunExtLibClass.HttpWebRequestToFile      PROCEDURE(CONST *CSTRING requestEndpoint,LONG httpVerbMethod,CONST *CSTRING postData,CONST *CSTRING requestParameters,*CSTRING errorOut, CONST *CSTRING outputFilename)
  CODE
  IF NOT SELF.Init(TRUE)
    RETURN FALSE
  END
  RETURN HttpWebRequestToFile_ClaRunExt (requestEndpoint,httpVerbMethod,postData,requestParameters,errorOut, outputFilename)

ClaRunExtLibClass.HttpWebRequestToFile      PROCEDURE(CONST *CSTRING requestEndpoint,LONG httpVerbMethod,CONST *CSTRING postData,CONST *CSTRING requestParameters,CONST *CSTRING contentType, CONST *CSTRING headers,*CSTRING errorOut, CONST *CSTRING outputFilename)
  CODE
  IF NOT SELF.Init(TRUE)
    RETURN FALSE
  END
  RETURN HttpWebRequestToFileExt_ClaRunExt (requestEndpoint,httpVerbMethod,postData,requestParameters,contentType, headers,errorOut, outputFilename)

ClaRunExtLibClass.GetAWSRegion      PROCEDURE(*STRING displayName, *STRING systemName, <*STRING errorMessage>, <*STRING errorCode>)
errMsgLen                         LONG
errCodeLen                        LONG
  CODE
  IF NOT SELF.Init(FALSE, errorMessage)
    RETURN FALSE
  END
  IF NOT OMITTED(errorMessage)
    errMsgLen = LEN(errorMessage)
  END
  IF NOT OMITTED(errorCode)
    errCodeLen = LEN(errorCode)
  END
  RETURN GetAWSRegion_ClaRunExt (displayName, LEN(displayName), systemName, LEN(systemName), errorMessage, errMsgLen, errorCode, errCodeLen)

ClaRunExtLibClass.ConnectToS3  PROCEDURE(STRING AccessKey, STRING SecretKey, <*STRING ErrorMessage>, <*STRING errorCode>)
errMsgLen                         LONG
errCodeLen                        LONG
  CODE
  IF NOT SELF.Init(FALSE, errorMessage)
    RETURN 0
  END
  IF NOT OMITTED(errorMessage)
    errMsgLen = LEN(errorMessage)
  END
  IF NOT OMITTED(errorCode)
    errCodeLen = LEN(errorCode)
  END
  RETURN ConnectToS3_ClaRunExt (AccessKey, LEN(AccessKey), SecretKey, LEN(SecretKey), errorMessage, errMsgLen, errorCode, errCodeLen)
ClaRunExtLibClass.DisconnectFromS3    PROCEDURE(LONG service)
  CODE
  IF NOT SELF.Init(TRUE)
    RETURN
  END
  DisconnectFromS3_ClaRunExt (service)
ClaRunExtLibClass.GetContainer  PROCEDURE(LONG ptr, *STRING containerName, *STRING regionDisplayName, *STRING regionSystemName, *STRING errorMessage, *STRING errorCode)
  CODE
  IF NOT SELF.Init(FALSE, errorMessage)
    RETURN FALSE
  END
  RETURN GetContainer_ClaRunExt (ptr, containerName, LEN(containerName), regionDisplayName, LEN(regionDisplayName), regionSystemName, LEN(regionSystemName), errorMessage, LEN(errorMessage), errorCode, LEN(errorCode))
ClaRunExtLibClass.CreateContainer  PROCEDURE(LONG ptr, STRING containerName, STRING region, *STRING errorMessage, *STRING errorCode)
  CODE
  IF NOT SELF.Init(FALSE, errorMessage)
    RETURN 3
  END
  RETURN CreateContainer_ClaRunExt (ptr, containerName, LEN(containerName), region, LEN(region), errorMessage, LEN(errorMessage), errorCode, LEN(errorCode))
ClaRunExtLibClass.DeleteContainer  PROCEDURE(LONG ptr, STRING containerName, *STRING errorMessage, *STRING errorCode)
  CODE
  IF NOT SELF.Init(FALSE, errorMessage)
    RETURN 3
  END
  RETURN DeleteContainer_ClaRunExt (ptr, containerName, LEN(containerName), errorMessage, LEN(errorMessage), errorCode, LEN(errorCode))
ClaRunExtLibClass.GetNextFile   PROCEDURE(LONG ptr, STRING containerName, STRING parentFolder, *STRING fileName, *BOOL isFile, *DATE lmDate, *TIME lmTime, *INT64 size, *STRING errorMessage, *STRING errorCode)
  CODE
  IF NOT SELF.Init(FALSE, errorMessage)
    RETURN FALSE
  END
  RETURN GetNextFile_ClaRunExt (ptr, containerName, LEN(containerName), parentFolder, LEN(parentFolder), fileName, LEN(fileName), isFile, lmDate, lmTime, size.lo, size.hi, errorMessage, LEN(errorMessage), errorCode, LEN(errorCode))
ClaRunExtLibClass.CreateFolder   PROCEDURE(LONG ptr, STRING containerName, STRING folder, *STRING errorMessage, *STRING errorCode)
  CODE
  IF NOT SELF.Init(FALSE, errorMessage)
    RETURN 3
  END
  RETURN CreateFolder_ClaRunExt (ptr, containerName, LEN(containerName), folder, LEN(folder), errorMessage, LEN(errorMessage), errorCode, LEN(errorCode))
ClaRunExtLibClass.DeleteFolder   PROCEDURE(LONG ptr, STRING containerName, STRING folder, *STRING errorMessage, *STRING errorCode)
  CODE
  IF NOT SELF.Init(FALSE, errorMessage)
    RETURN 3
  END
  RETURN DeleteFolder_ClaRunExt (ptr, containerName, LEN(containerName), folder, LEN(folder), errorMessage, LEN(errorMessage), errorCode, LEN(errorCode))
ClaRunExtLibClass.UploadFile   PROCEDURE(LONG ptr, STRING sourceFile, STRING containerName, STRING destinationFile, BOOL asynchronous, BOOL encrypted, IAWSTransfering callback, *STRING errorMessage, *STRING errorCode)
  CODE
  IF NOT SELF.Init(FALSE, errorMessage)
    RETURN 3
  END
  RETURN UploadFile_ClaRunExt (ptr, sourceFile, LEN(sourceFile), containerName, LEN(containerName), destinationFile, LEN(destinationFile), asynchronous, encrypted, callback, errorMessage, LEN(errorMessage), errorCode, LEN(errorCode))
ClaRunExtLibClass.DownloadFile  PROCEDURE(LONG ptr, STRING containerName, STRING sourceFile, STRING destinationFile, BOOL asynchronous, IAWSTransfering callback, *STRING errorMessage, *STRING errorCode)
  CODE
  IF NOT SELF.Init(FALSE, errorMessage)
    RETURN 3
  END
  RETURN DownloadFile_ClaRunExt (ptr, containerName, LEN(containerName), sourceFile, LEN(sourceFile), destinationFile, LEN(destinationFile), asynchronous, callback, errorMessage, LEN(errorMessage), errorCode, LEN(errorCode))
ClaRunExtLibClass.DeleteFile    PROCEDURE(LONG ptr, STRING containerName, STRING fileName, *STRING errorMessage, *STRING errorCode)
  CODE
  IF NOT SELF.Init(FALSE, errorMessage)
    RETURN 3
  END
  RETURN DeleteFile_ClaRunExt (ptr, containerName, LEN(containerName), fileName, LEN(fileName), errorMessage, LEN(errorMessage), errorCode, LEN(errorCode))
ClaRunExtLibClass.FileExists    PROCEDURE(LONG ptr, STRING containerName, STRING fileName, *STRING errorMessage, *STRING errorCode)
  CODE
  IF NOT SELF.Init(FALSE, errorMessage)
    RETURN FALSE
  END
  RETURN FileExists_ClaRunExt (ptr, containerName, LEN(containerName), fileName, LEN(fileName), errorMessage, LEN(errorMessage), errorCode, LEN(errorCode))
ClaRunExtLibClass.CancelTransfer    PROCEDURE(LONG ptr, *STRING errorMessage, *STRING errorCode)
  CODE
  IF NOT SELF.Init(FALSE, errorMessage)
    RETURN 3
  END
  RETURN CancelTransfer_ClaRunExt(ptr, errorMessage, LEN(errorMessage), errorCode, LEN(errorCode))
ClaRunExtLibClass.GetTemporaryURL   PROCEDURE(LONG ptr, STRING containerName, STRING fileName, LONG days, LONG hours, LONG minutes, *STRING errorMessage, *STRING errorCode)
ret                                   STRING(1000)
  CODE
  IF NOT SELF.Init(FALSE, errorMessage)
    RETURN ''
  END
  GetTemporaryURL_ClaRunExt (ptr, containerName, LEN(containerName), fileName, LEN(fileName), days, hours, minutes, ret, LEN(ret), errorMessage, LEN(errorMessage), errorCode, LEN(errorCode))
  RETURN ret
ClaRunExtLibClass.GetPermanentURL   PROCEDURE(LONG ptr, STRING containerName, STRING fileName, *STRING errorMessage, *STRING errorCode)
ret                                   STRING(1000)
  CODE
  IF NOT SELF.Init(FALSE, errorMessage)
    RETURN ''
  END
  GetPermanentURL_ClaRunExt(ptr, containerName, LEN(containerName), fileName, LEN(fileName), ret, LEN(ret), errorMessage, LEN(errorMessage), errorCode, LEN(errorCode))
  RETURN ret
ClaRunExtLibClass.AllowPublicAccess PROCEDURE(LONG ptr, STRING containerName, STRING fileName, BOOL public, *STRING errorMessage, *STRING errorCode)
  CODE
  IF NOT SELF.Init(FALSE, errorMessage)
    RETURN 3
  END
  RETURN AllowPublicAccess_ClaRunExt (ptr, containerName, LEN(containerName), fileName, LEN(fileName), public, errorMessage, LEN(errorMessage), errorCode, LEN(errorCode))
ClaRunExtLibClass.HasPublicAccess PROCEDURE(LONG ptr, STRING containerName, STRING fileName, *STRING errorMessage, *STRING errorCode)
  CODE
  IF NOT SELF.Init(FALSE, errorMessage)
    RETURN FALSE
  END
  RETURN HasPublicAccess_ClaRunExt (ptr, containerName, LEN(containerName), fileName, LEN(fileName), errorMessage, LEN(errorMessage), errorCode, LEN(errorCode))
ClaRunExtLibClass.GetNextMetaData    PROCEDURE(LONG ptr, STRING containerName, STRING fileName, *STRING key, *STRING value, *STRING errorMessage, *STRING errorCode)
  CODE
  IF NOT SELF.Init(FALSE, errorMessage)
    RETURN 3
  END
  RETURN GetNextMetaData_ClaRunExt (ptr, containerName, LEN(containerName), fileName, LEN(fileName), key, LEN(key), value, LEN(value), errorMessage, LEN(errorMessage), errorCode, LEN(errorCode))
ClaRunExtLibClass.GetMetaData   PROCEDURE(LONG ptr, STRING containerName, STRING fileName, STRING key, *STRING errorMessage, *STRING errorCode)
ret                               STRING(1000)
  CODE
  IF NOT SELF.Init(FALSE, errorMessage)
    RETURN ''
  END
  GetMetaData_ClaRunExt(ptr, containerName, LEN(containerName), fileName, LEN(fileName), key, LEN(key), ret, LEN(ret), errorMessage, LEN(errorMessage), errorCode, LEN(errorCode))
  RETURN ret
ClaRunExtLibClass.SetMetaData   PROCEDURE(LONG ptr, STRING containerName, STRING fileName, STRING metaData, *STRING errorMessage, *STRING errorCode)
  CODE
  IF NOT SELF.Init(FALSE, errorMessage)
    RETURN 3
  END
  RETURN SetAllMetaData_ClaRunExt (ptr, containerName, LEN(containerName), fileName, LEN(fileName), metaData, LEN(metaData), errorMessage, LEN(errorMessage), errorCode, LEN(errorCode))
ClaRunExtLibClass.SetMetaData   PROCEDURE(LONG ptr, STRING containerName, STRING fileName, STRING key, STRING value, *STRING errorMessage, *STRING errorCode)
  CODE
  IF NOT SELF.Init(FALSE, errorMessage)
    RETURN 3
  END
  RETURN SetMetaData_ClaRunExt (ptr, containerName, LEN(containerName), fileName, LEN(fileName), key, LEN(key), value, LEN(value), errorMessage, LEN(errorMessage), errorCode, LEN(errorCode))
ClaRunExtLibClass.RemoveMetaData    PROCEDURE(LONG ptr, STRING containerName, STRING fileName, STRING key, *STRING errorMessage, *STRING errorCode)
  CODE
  IF NOT SELF.Init(FALSE, errorMessage)
    RETURN 3
  END
  RETURN RemoveMetaData_ClaRunExt (ptr, containerName, LEN(containerName), fileName, LEN(fileName), key, LEN(key), errorMessage, LEN(errorMessage), errorCode, LEN(errorCode))
!endregion
!endregion
!region ClaRunExtClass
ClaRunExtClass.Construct    PROCEDURE()
  CODE
  ClaRunExtLibClass.Use()

ClaRunExtClass.Destruct PROCEDURE()
  CODE
  ClaRunExtLibClass.Kill()
!region Image Manipulation
ClaRunExtClass.ImageToPNG   PROCEDURE(STRING fileNameIn)
  CODE
  RETURN ClaRunExtLibClass.ImageToPNG(fileNameIn)
ClaRunExtClass.ImageToPNG   PROCEDURE(STRING fileNameIn, STRING fileNameOut)
  CODE
  RETURN ClaRunExtLibClass.ImageToPNG(fileNameIn, fileNameOut)
ClaRunExtClass.ImageToPNG   PROCEDURE(STRING fileNameIn, STRING fileNameOut, USHORT imageRes)
  CODE
  RETURN ClaRunExtLibClass.ImageToPNG(fileNameIn, fileNameOut, imageRes)
ClaRunExtClass.ImageRotateFlip  PROCEDURE(STRING fileNameIn, STRING fileNameOut, LONG RotateFlipType)
  CODE
  RETURN ClaRunExtLibClass.ImageRotateFlip(fileNameIn, fileNameOut, RotateFlipType)
ClaRunExtClass.ImageSaveThumbnail   PROCEDURE(STRING fileNameIn, STRING fileNameOut, LONG percentage)
  CODE
  RETURN ClaRunExtLibClass.ImageSaveThumbnail(fileNameIn, fileNameOut, percentage)
!endregion
!region Email/SMS
ClaRunExtClass.SendSMS  PROCEDURE(STRING SMTPServerHost, STRING SMTPLoginUserName, STRING SMTPLoginPassword, LONG SMTPPort, LONG SMTPEnableSsl, LONG showErrorMessage, STRING  fromAddress, STRING  toPhoneNumber, STRING  toCarrier, STRING  subject, STRING  textMessage, <*STRING errMessage>)
  CODE
  RETURN ClaRunExtLibClass.SendSMS(SMTPServerHost, SMTPLoginUserName, SMTPLoginPassword, SMTPPort, SMTPEnableSsl, showErrorMessage, fromAddress, toPhoneNumber, toCarrier, subject, textMessage, errMessage)
ClaRunExtClass.SendMail PROCEDURE(STRING SMTPServerHost, STRING SMTPLoginUserName, STRING SMTPLoginPassword, LONG SMTPPort, LONG SMTPEnableSsl, LONG showErrorMessage, LONG receiveRequest, STRING messageBodyEncoding, STRING fromAddress, STRING toAddress, STRING replyToAddress, STRING subject, STRING htmlMessage, STRING embeddedImageFileNames, STRING textMessage, STRING attachedFileNames, STRING ccList, STRING bccList, <*STRING errMessage>)
  CODE
  RETURN ClaRunExtLibClass.SendMail(SMTPServerHost, SMTPLoginUserName, SMTPLoginPassword, SMTPPort, SMTPEnableSsl, showErrorMessage, receiveRequest, messageBodyEncoding, fromAddress, toAddress, replyToAddress, subject, htmlMessage, embeddedImageFileNames, textMessage, attachedFileNames, ccList, bccList, errMessage)
!endregion
!region WebRequest    
ClaRunExtClass.HttpWebRequest PROCEDURE(CONST *CSTRING requestEndpoint,LONG httpVerbMethod,CONST *CSTRING postData,CONST *CSTRING requestParameters,*CSTRING responseOut)
 CODE
 RETURN ClaRunExtLibClass.HttpWebRequest(requestEndpoint, httpVerbMethod, postData, requestParameters, responseOut)
ClaRunExtClass.HttpWebRequest          PROCEDURE(CONST *CSTRING requestEndpoint,LONG httpVerbMethod,CONST *CSTRING postData,CONST *CSTRING requestParameters,CONST *CSTRING contentType, CONST *CSTRING headers,*CSTRING responseOut)
 CODE
 RETURN ClaRunExtLibClass.HttpWebRequest(requestEndpoint, httpVerbMethod, postData, requestParameters, contentType, headers, responseOut)

ClaRunExtClass.HttpWebRequestToFile    PROCEDURE(CONST *CSTRING requestEndpoint,LONG httpVerbMethod,CONST *CSTRING postData,CONST *CSTRING requestParameters,*CSTRING errorOut, CONST *CSTRING outputFilename)
 CODE
 RETURN ClaRunExtLibClass.HttpWebRequestToFile(requestEndpoint, httpVerbMethod, postData, requestParameters, errorOut, outputFilename)

ClaRunExtClass.HttpWebRequestToFile PROCEDURE(CONST *CSTRING requestEndpoint,LONG httpVerbMethod,CONST *CSTRING postData,CONST *CSTRING requestParameters,CONST *CSTRING contentType, CONST *CSTRING headers,*CSTRING errorOut, CONST *CSTRING outputFilename)
 CODE
 RETURN ClaRunExtLibClass.HttpWebRequestToFile(requestEndpoint, httpVerbMethod, postData, requestParameters, contentType, headers, errorOut, outputFilename)
!endregion
!region AWS Methods
ClaRunExtClass.GetAWSRegion PROCEDURE(*STRING displayName, *STRING systemName, <*STRING errorMessage>, <*STRING errorCode>)
  CODE
  RETURN ClaRunExtLibClass.GetAWSRegion(displayName, systemName, errorMessage, errorCode)
ClaRunExtClass.ConnectToS3 PROCEDURE(STRING AccessKey, STRING SecretKey, <*STRING ErrorMessage>, <*STRING errorCode>)
  CODE
  RETURN ClaRunExtLibClass.ConnectToS3(AccessKey, SecretKey, ErrorMessage, errorCode)
ClaRunExtClass.DisconnectFromS3    PROCEDURE(LONG service)
  CODE
  ClaRunExtLibClass.DisconnectFromS3(service)
ClaRunExtClass.GetContainer  PROCEDURE(LONG ptr, *STRING containerName, *STRING regionDisplayName, *STRING regionSystemName, *STRING errorMessage, *STRING errorCode)
  CODE
  RETURN ClaRunExtLibClass.GetContainer (ptr, containerName, regionDisplayName, regionSystemName, errorMessage, errorCode)
ClaRunExtClass.CreateContainer PROCEDURE(LONG ptr, STRING containerName, STRING region, *STRING errorMessage, *STRING errorCode)
  CODE
  RETURN ClaRunExtLibClass.CreateContainer (ptr, containerName, region, errorMessage, errorCode)
ClaRunExtClass.DeleteContainer PROCEDURE(LONG ptr, STRING containerName, *STRING errorMessage, *STRING errorCode)
  CODE
  RETURN ClaRunExtLibClass.DeleteContainer (ptr, containerName, errorMessage, errorCode)
ClaRunExtClass.CreateFolder PROCEDURE(LONG ptr, STRING containerName, STRING folder, *STRING errorMessage, *STRING errorCode)
  CODE
  RETURN ClaRunExtLibClass.CreateFolder (ptr, containerName, folder, errorMessage, errorCode)
ClaRunExtClass.DeleteFolder  PROCEDURE(LONG ptr, STRING containerName, STRING folder, *STRING errorMessage, *STRING errorCode)
  CODE
  RETURN ClaRunExtLibClass.DeleteFolder (ptr, containerName, folder, errorMessage, errorCode)
ClaRunExtClass.GetNextFile  PROCEDURE(LONG ptr, STRING containerName, STRING parentFolder, *STRING fileName, *BOOL isFile, *DATE lmDate, *TIME lmTime, *INT64 size, *STRING errorMessage, *STRING errorCode)
  CODE
  RETURN ClaRunExtLibClass.GetNextFile(ptr, containerName, parentFolder, fileName, isFile, lmDate, lmTime, size, errorMessage, errorCode)
ClaRunExtClass.UploadFile   PROCEDURE(LONG ptr, STRING sourceFile, STRING containerName, STRING destinationFile, BOOL asynchronous, BOOL encrypted, IAWSTransfering callback, *STRING errorMessage, *STRING errorCode)
  CODE
  RETURN ClaRunExtLibClass.UploadFile(ptr, sourceFile, containerName, destinationFile, asynchronous, encrypted, callback, errorMessage, errorCode)
ClaRunExtClass.DownloadFile PROCEDURE(LONG ptr, STRING containerName, STRING sourceFile, STRING destinationFile, BOOL asynchronous, IAWSTransfering callback, *STRING errorMessage, *STRING errorCode)
  CODE
  RETURN ClaRunExtLibClass.DownloadFile(ptr, containerName, sourceFile, destinationFile, asynchronous, callback, errorMessage, errorCode)
ClaRunExtClass.DeleteFile   PROCEDURE(LONG ptr, STRING containerName, STRING fileName, *STRING errorMessage, *STRING errorCode)
  CODE
  RETURN ClaRunExtLibClass.DeleteFile(ptr, containerName, fileName, errorMessage, errorCode)
ClaRunExtClass.FileExists   PROCEDURE(LONG ptr, STRING containerName, STRING fileName, *STRING errorMessage, *STRING errorCode)
  CODE
  RETURN ClaRunExtLibClass.FileExists(ptr, containerName, fileName, errorMessage, errorCode)
ClaRunExtClass.CancelTransfer   PROCEDURE(LONG ptr, *STRING errorMessage, *STRING errorCode)
  CODE
  RETURN ClaRunExtLibClass.CancelTransfer(ptr, errorMessage, errorCode)
ClaRunExtClass.GetTemporaryURL  PROCEDURE(LONG ptr, STRING containerName, STRING fileName, LONG days, LONG hours, LONG minutes, *STRING errorMessage, *STRING errorCode)
  CODE
  RETURN ClaRunExtLibClass.GetTemporaryURL(ptr, containerName, fileName, days, hours, minutes, errorMessage, errorCode)
ClaRunExtClass.GetPermanentURL  PROCEDURE(LONG ptr, STRING containerName, STRING fileName, *STRING errorMessage, *STRING errorCode)
  CODE
  RETURN ClaRunExtLibClass.GetPermanentURL(ptr, containerName, fileName, errorMessage, errorCode)
ClaRunExtClass.AllowPublicAccess    PROCEDURE(LONG ptr, STRING containerName, STRING fileName, BOOL public, *STRING errorMessage, *STRING errorCode)
  CODE
  RETURN ClaRunExtLibClass.AllowPublicAccess(ptr, containerName, fileName, public, errorMessage, errorCode)
ClaRunExtClass.HasPublicAccess    PROCEDURE(LONG ptr, STRING containerName, STRING fileName, *STRING errorMessage, *STRING errorCode)
  CODE
  RETURN ClaRunExtLibClass.HasPublicAccess(ptr, containerName, fileName, errorMessage, errorCode)
ClaRunExtClass.GetNextMetaData   PROCEDURE(LONG ptr, STRING containerName, STRING fileName, *STRING key, *STRING value, *STRING errorMessage, *STRING errorCode)
  CODE
  RETURN ClaRunExtLibClass.GetNextMetaData(ptr, containerName, fileName, key, value, errorMessage, errorCode)
ClaRunExtClass.GetMetaData  PROCEDURE(LONG ptr, STRING containerName, STRING fileName, STRING key, *STRING errorMessage, *STRING errorCode)
  CODE
  RETURN ClaRunExtLibClass.GetMetaData(ptr, containerName, fileName, key, errorMessage, errorCode)
ClaRunExtClass.SetMetaData  PROCEDURE(LONG ptr, STRING containerName, STRING fileName, STRING metaData, *STRING errorMessage, *STRING errorCode)
  CODE
  RETURN ClaRunExtLibClass.SetMetaData(ptr, containerName, fileName, metaData, errorMessage, errorCode)
ClaRunExtClass.SetMetaData  PROCEDURE(LONG ptr, STRING containerName, STRING fileName, STRING key, STRING value, *STRING errorMessage, *STRING errorCode)
  CODE
  RETURN ClaRunExtLibClass.SetMetaData(ptr, containerName, fileName, key, value, errorMessage, errorCode)
ClaRunExtClass.RemoveMetaData   PROCEDURE(LONG ptr, STRING containerName, STRING fileName, STRING key, *STRING errorMessage, *STRING errorCode)
  CODE
  RETURN ClaRunExtLibClass.RemoveMetaData(ptr, containerName, fileName, key, errorMessage, errorCode)
!endregion
!endregion
