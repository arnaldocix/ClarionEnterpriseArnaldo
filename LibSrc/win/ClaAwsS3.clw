  MEMBER()

  INCLUDE('ClaAwsS3.inc'),ONCE
  INCLUDE('ClaRunExt.INC'),ONCE
  INCLUDE('ClaMail.INC'),ONCE

  MAP
    MODULE('RTL')
AttachThreadToClarion PROCEDURE(BOOL),PASCAL
    END
MetaDataToString    PROCEDURE(AWSMetaData inList, SystemStringClass outStr)
  END

ClaRunExt           CLASS
clarunext             &ClaRunExtClass,PRIVATE
DotNet                PROCEDURE(),*ClaRunExtClass
Destruct              PROCEDURE()
                    END

AWSS3Client         CLASS,IMPLEMENTS(IAWSS3Client),IMPLEMENTS(IAWSTransfering),TYPE
lastError             STRING(1000),PRIVATE
lastErrorCode         STRING(50),PRIVATE
netCode               LONG,PRIVATE
notificationCode      LONG,PRIVATE
percDone              LONG,PRIVATE
transferState         LONG,PRIVATE
threadId              LONG,PRIVATE
transferingFile       STRING(255),PRIVATE
s3TransferingFile     STRING(1024),PRIVATE

Construct             PROCEDURE()
Connect               PROCEDURE(STRING AccessKey, STRING SecretKey),BOOL
TransferInProgress    PROCEDURE(LONG percentage, LONG state)
                    END

ListRegions         PROCEDURE(RegionList regions, <*STRING errorMessage>)
dotnet                &ClaRunExtClass
errMessage            &STRING
noReporting           STRING(1)
  CODE
  dotnet &= ClaRunExt.DotNet()
  IF OMITTED(errorMessage)
    errMessage &= noReporting
  ELSE
    errMessage &= errorMessage
  END
  FREE(regions)
  LOOP WHILE dotnet.GetAWSRegion(regions.DisplayName, regions.SystemName, errMessage)
    ADD(regions)
  END
  RETURN CHOOSE(errMessage = '')

MetaDataToString    PROCEDURE(AWSMetaData inList, SystemStringClass outStr)
i                     LONG,AUTO
recs                  LONG,AUTO
  CODE
  recs = RECORDS(inList)
  LOOP i = 1 TO recs
    IF i <> 1
      outStr.Append('<02>')
    END
    GET(inList, i)
    outStr.Append(CLIP(inList.key))
    outStr.Append('<02>')
    outStr.Append(CLIP(inList.value))
  END

!region ClaRunExt
ClaRunExt.DotNet    PROCEDURE()
  CODE
  IF SELF.clarunext &= NULL
    SELF.clarunext &= NEW ClaRunExtClass()
  END
  RETURN SELF.clarunext

ClaRunExt.Destruct  PROCEDURE()
  CODE
  IF NOT SELF.clarunext &= NULL
    DISPOSE(SELF.clarunext)
  END
!endregion

!region AWSS3Client
AWSS3Client.Construct  PROCEDURE()
  CODE
  SELF.notificationCode = AWSNotification:Processing

AWSS3Client.Connect PROCEDURE(STRING AccessKey, STRING SecretKey)
ptr                   LONG,AUTO
dotnet                &ClaRunExtClass
ret                   &AWSS3Client
  CODE
  dotnet &= ClaRunExt.DotNet()
  SELF.netCode = dotnet.ConnectToS3(AccessKey, SecretKey, SELF.lastError)
  RETURN CHOOSE(SELF.netCode <> 0)

AWSS3Client.TransferInProgress PROCEDURE(LONG percentage, LONG state)
  CODE
  IF state
    SELF.transferState = CHOOSE(state, AWSTransfer:Completed, AWSTransfer:Cancelled, AWSTransfer:ERROR)
    SELF.percDone = 100
  ELSE
    SELF.percDone = percentage
  END
  NOTIFY(SELF.notificationCode, SELF.threadId, ADDRESS(SELF))
  
!region IAWSS3Client
AWSS3Client.IAWSS3Client.Disconnect    PROCEDURE()
dotnet                                              &ClaRunExtClass
  CODE
  IF SELF.netCode <> 0
    dotnet &= ClaRunExt.DotNet()
    dotnet.DisconnectFromS3(SELF.netCode)
    SELF.netCode = 0
  END
  DISPOSE(SELF)
  
AWSS3Client.IAWSS3Client.Error PROCEDURE()
  CODE
  RETURN SELF.lastError

AWSS3Client.IAWSS3Client.ErrorCode PROCEDURE()
  CODE
  RETURN SELF.lastErrorCode

AWSS3Client.IAWSS3Client.ListContainers   PROCEDURE(AWSContainerList containers)
dotnet                &ClaRunExtClass
  CODE
  SELF.lastError = ''
  SELF.lastErrorCode = ''
  dotnet &= ClaRunExt.DotNet()
  FREE(containers)
  LOOP WHILE dotnet.GetContainer(SELF.netCode, containers.name, containers.RegionDisplayName, containers.RegionSystemName, SELF.lastError, SELF.lastErrorCode)
    ADD(containers)
  END
  RETURN CHOOSE(SELF.lastError = '')

AWSS3Client.IAWSS3Client.CreateContainer   PROCEDURE(STRING containerName, STRING region)
dotnet                &ClaRunExtClass
  CODE
  SELF.lastError = ''
  SELF.lastErrorCode = ''
  dotnet &= ClaRunExt.DotNet()
  RETURN dotnet.CreateContainer(SELF.netCode, containerName, region, SELF.lastError, SELF.lastErrorCode)

AWSS3Client.IAWSS3Client.DeleteContainer       PROCEDURE(STRING containerName)
dotnet                &ClaRunExtClass
  CODE
  SELF.lastError = ''
  SELF.lastErrorCode = ''
  dotnet &= ClaRunExt.DotNet()
  RETURN dotnet.DeleteContainer(SELF.netCode, containerName, SELF.lastError, SELF.lastErrorCode)
!region Folder methods
AWSS3Client.IAWSS3Client.CreateFolder        PROCEDURE(STRING containerName, STRING folderName)
dotnet                &ClaRunExtClass
  CODE
  SELF.lastError = ''
  SELF.lastErrorCode = ''
  dotnet &= ClaRunExt.DotNet()
  RETURN dotnet.CreateFolder(SELF.netCode, containerName, folderName, SELF.lastError, SELF.lastErrorCode)
AWSS3Client.IAWSS3Client.DeleteFolder        PROCEDURE(STRING containerName, STRING folderName)
dotnet                &ClaRunExtClass
  CODE
  SELF.lastError = ''
  SELF.lastErrorCode = ''
  dotnet &= ClaRunExt.DotNet()
  RETURN dotnet.DeleteFolder(SELF.netCode, containerName, folderName, SELF.lastError, SELF.lastErrorCode)
!endregion
!region File methods
AWSS3Client.IAWSS3Client.ListFiles    PROCEDURE(STRING container, STRING parentFolder, *AWSObjectList folders)
dotnet                                            &ClaRunExtClass
  CODE
  SELF.lastError = ''
  SELF.lastErrorCode = ''
  dotnet &= ClaRunExt.DotNet()
  FREE(folders)
  LOOP WHILE dotnet.GetNextFile(SELF.netCode, container, parentFolder, folders.name, folders.isFolder, folders.LastModified.DATE, folders.LastModified.TIME, folders.SIZE, SELF.lastError, SELF.lastErrorCode)
    ADD(folders)
  END
  RETURN CHOOSE(SELF.lastError = '')
AWSS3Client.IAWSS3Client.FileExists  PROCEDURE(STRING container, STRING fileName)
dotnet                                &ClaRunExtClass
  CODE
  SELF.lastError = ''
  SELF.lastErrorCode = ''
  dotnet &= ClaRunExt.DotNet()
  RETURN dotnet.FileExists(SELF.netCode, container, fileName, SELF.lastError, SELF.lastErrorCode)

AWSS3Client.IAWSS3Client.UploadFile PROCEDURE(STRING sourceFile, STRING containerName, STRING destinationFile, BOOL asynchronous, BOOL encrypted)
dotnet                                            &ClaRunExtClass
  CODE
  SELF.transferingFile = sourceFile
  SELF.s3TransferingFile = destinationFile
  SELF.lastError = ''
  SELF.lastErrorCode = ''
  dotnet &= ClaRunExt.DotNet()
  SELF.transferState = AWSTransfer:Transfering
  SELF.percDone = 0
  SELF.threadId = THREAD()
  RETURN dotnet.UploadFile(SELF.netCode, sourceFile, containerName, destinationFile, asynchronous, encrypted, SELF.IAWSTransfering, SELF.lastError, SELF.lastErrorCode)
AWSS3Client.IAWSS3Client.DownloadFile PROCEDURE(STRING containerName, STRING sourceFile, STRING destinationFile, BOOL asynchronous)
dotnet                                            &ClaRunExtClass
  CODE
  SELF.transferingFile = destinationFile
  SELF.s3TransferingFile = sourceFile
  SELF.lastError = ''
  SELF.lastErrorCode = ''
  dotnet &= ClaRunExt.DotNet()
  SELF.transferState = AWSTransfer:Transfering
  SELF.percDone = 0
  SELF.threadId = THREAD()
  RETURN dotnet.DownloadFile(SELF.netCode, containerName, sourceFile, destinationFile, asynchronous, SELF.IAWSTransfering, SELF.lastError, SELF.lastErrorCode)
AWSS3Client.IAWSS3Client.DeleteFile   PROCEDURE(STRING containerName, STRING fileName)
dotnet                                            &ClaRunExtClass
  CODE
  SELF.lastError = ''
  SELF.lastErrorCode = ''
  dotnet &= ClaRunExt.DotNet()
  RETURN dotnet.DeleteFile(SELF.netCode, containerName, fileName, SELF.lastError, SELF.lastErrorCode)
!region Asynchronous transfer methods
AWSS3Client.IAWSS3Client.CancelTransfer      PROCEDURE()
dotnet                                                    &ClaRunExtClass
  CODE
  SELF.lastError = ''
  SELF.lastErrorCode = ''
  dotnet &= ClaRunExt.DotNet()
  RETURN dotnet.CancelTransfer(SELF.netCode, SELF.lastError, SELF.lastErrorCode)

AWSS3Client.IAWSS3Client.TransferStatus   PROCEDURE(*LONG state, *LONG percentage)
  CODE
  state = SELF.transferState
  percentage = SELF.percDone

AWSS3Client.IAWSS3Client.TransferingFile    PROCEDURE()
  CODE
  RETURN CLIP(SELF.transferingFile)

AWSS3Client.IAWSS3Client.TransferingS3File  PROCEDURE()
  CODE
  RETURN CLIP(SELF.s3TransferingFile)

AWSS3Client.IAWSS3Client.SetNotificationCode  PROCEDURE(LONG notificationCode)
  CODE
  SELF.notificationCode = notificationCode
!endregion
!region public access to file
AWSS3Client.IAWSS3Client.GetTemporaryURL     PROCEDURE(STRING containerName, STRING fileName, LONG days, LONG hours, LONG minutes)
dotnet                                                    &ClaRunExtClass
  CODE
  SELF.lastError = ''
  SELF.lastErrorCode = ''
  dotnet &= ClaRunExt.DotNet()
  RETURN dotnet.GetTemporaryURL(SELF.netCode, containerName, fileName, days, hours, minutes, SELF.lastError, SELF.lastErrorCode)
AWSS3Client.IAWSS3Client.GetPermanentURL  PROCEDURE(STRING containerName, STRING fileName)
dotnet                                                &ClaRunExtClass
  CODE
  SELF.lastError = ''
  SELF.lastErrorCode = ''
  dotnet &= ClaRunExt.DotNet()
  RETURN dotnet.GetPermanentURL(SELF.netCode, containerName, fileName, SELF.lastError, SELF.lastErrorCode)
AWSS3Client.IAWSS3Client.AllowPublicAccess    PROCEDURE(STRING containerName, STRING fileName, BOOL public)
dotnet                                                    &ClaRunExtClass
  CODE
  SELF.lastError = ''
  SELF.lastErrorCode = ''
  dotnet &= ClaRunExt.DotNet()
  RETURN dotnet.AllowPublicAccess(SELF.netCode, containerName, fileName, public, SELF.lastError, SELF.lastErrorCode)
AWSS3Client.IAWSS3Client.HasPublicAccess    PROCEDURE(STRING containerName, STRING fileName)
dotnet                                                    &ClaRunExtClass
  CODE
  SELF.lastError = ''
  SELF.lastErrorCode = ''
  dotnet &= ClaRunExt.DotNet()
  RETURN dotnet.HasPublicAccess(SELF.netCode, containerName, fileName, SELF.lastError, SELF.lastErrorCode)
!endregion
!region Meta Data
AWSS3Client.IAWSS3Client.GetMetaData         PROCEDURE(STRING containerName, STRING fileName, *AWSMetaData metaData)
dotnet                                                    &ClaRunExtClass
ret                                                       LONG
  CODE
  SELF.lastError = ''
  SELF.lastErrorCode = ''
  dotnet &= ClaRunExt.DotNet()
  FREE(metaData)
  LOOP WHILE dotnet.GetNextMetaData(SELF.netCode, containerName, fileName, metaData.key, metaData.value, SELF.lastError, SELF.lastErrorCode)
    ADD(metaData)
  END
  RETURN CHOOSE(SELF.lastError = '')
AWSS3Client.IAWSS3Client.GetMetaData  PROCEDURE(STRING containerName, STRING fileName, STRING key)
dotnet                                            &ClaRunExtClass
  CODE
  SELF.lastError = ''
  SELF.lastErrorCode = ''
  dotnet &= ClaRunExt.DotNet()
  RETURN dotnet.GetMetaData(SELF.netCode, containerName, fileName, key, SELF.lastError, SELF.lastErrorCode)
AWSS3Client.IAWSS3Client.SetMetaData  PROCEDURE(STRING containerName, STRING fileName, *AWSMetaData metaData)
dotnet                                            &ClaRunExtClass
metaDataList                                      SystemStringClass
  CODE
  SELF.lastError = ''
  SELF.lastErrorCode = ''
  MetaDataToString(metaData, metaDataList)
  dotnet &= ClaRunExt.DotNet()
  RETURN dotnet.GetMetaData(SELF.netCode, containerName, fileName, metaDataList.Str(), SELF.lastError, SELF.lastErrorCode)
AWSS3Client.IAWSS3Client.SetMetaData  PROCEDURE(STRING containerName, STRING fileName, STRING key, STRING value)
dotnet                                            &ClaRunExtClass
  CODE
  SELF.lastError = ''
  SELF.lastErrorCode = ''
  dotnet &= ClaRunExt.DotNet()
  RETURN dotnet.SetMetaData(SELF.netCode, containerName, fileName, key, value, SELF.lastError, SELF.lastErrorCode)
AWSS3Client.IAWSS3Client.RemoveMetaData   PROCEDURE(STRING containerName, STRING fileName, STRING key)
dotnet                                                &ClaRunExtClass
  CODE
  SELF.lastError = ''
  SELF.lastErrorCode = ''
  dotnet &= ClaRunExt.DotNet()
  RETURN dotnet.RemoveMetaData(SELF.netCode, containerName, fileName, key, SELF.lastError, SELF.lastErrorCode)
!endregion
!endregion
!endregion
!region IAWSTransfering
AWSS3Client.IAWSTransfering.Transfering         PROCEDURE(LONG percDone, LONG state)
  CODE
  AttachThreadToClarion(FALSE)
  SELF.TransferInProgress(percDone, state)
!endregion
!endregion
ConnectToS3    PROCEDURE(STRING AccessKey, STRING SecretKey, <*STRING errorMessage>)
S3                    &AWSS3Client
  CODE
  S3 &= NEW AWSS3Client
  IF NOT S3.Connect(AccessKey, SecretKey)
    IF NOT OMITTED(errorMessage)
      errorMessage = S3.IAWSS3Client.ERROR()
    END
    DISPOSE(S3)
    RETURN NULL
  END
  RETURN S3.IAWSS3Client
_EndOfInclude_
