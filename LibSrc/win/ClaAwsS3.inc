!ABCIncludeFile


  OMIT('_EndOfInclude_',_AWSS3Present_)  ! Omit this if already compiled
_AWSS3Present_      EQUATE(1)

DisplayNameLen      EQUATE(40)
SystemNameLen       EQUATE(40)

!!! <summary>
!!! Queue used to get the list of regions(servers) where data can be stored
!!! </summary>
RegionList          QUEUE,TYPE
!!! <summary>
!!! Name that can be displayed to a user for them to pick where data should be stored
!!! </summary>
DisplayName           STRING(DisplayNameLen)
!!! <summary>
!!! Name passed to methods that raise connections to an AWS S3 service
!!! </summary>
SystemName            STRING(SystemNameLen)
                    END

!region List System Names of currently available regions
!!! <summary>
!!! The Asia Pacific (Tokyo) region.
!!! </summary>
Region:APNortheast1 EQUATE('ap-northeast-1')
!!! <summary>
!!! The Asia Pacific (Singapore) region.
!!! </summary>
Region:APSoutheast1 EQUATE('ap-southeast-1')
!!! <summary>
!!! The Asia Pacific (Sydney) region.
!!! </summary>
Region:APSoutheast2 EQUATE('ap-southeast-2')
!!! <summary>
!!! The EU Central (Frankfurt) region.
!!! </summary>
Region:EUCentral1   EQUATE('eu-central-1')
!!! <summary>
!!! The EU West (Ireland) region.
!!! </summary>
Region:EUWest1      EQUATE('eu-west-1')
!!! <summary>
!!! The South America (Sao Paulo) region.
!!! </summary>
Region:SAEast1      EQUATE('sa-east-1')
!!! <summary>
!!! The US East (Virginia) region.
!!! </summary>
Region:USEast1      EQUATE('us-east-1')
!!! <summary>
!!! The US West (N. California) region.
!!! </summary>
Region:USWest1      EQUATE('us-west-1')
!!! <summary>
!!! The US West (Oregon) region.
!!! </summary>
Region:USWest2      EQUATE('us-west-2')
!endregion

  MAP
    MODULE('ClaAwsS3.clw')
!!! <summary>
!!! Populates regions with all current AWS regions.
!!! The AWS API supplied by Amazon does not have any system where a program can
!!! automatically become aware of a new region.  You must replace the awssdk.dll and
!!! normally make coding changes for a program to recognise a new region.
!!! The ClaAWS system is more flexible and allows you to dynamically add new regions
!!! without needing to ship new binaries.  To do this you need to add a key to the
!!! 32bit registry HKLM\Software\SoftVelocity\AWS\Regions\<regions system name>.  
!!! For example, to add a server called ap-southeast-3 in Seoul you would add 
!!! HKLM\Software\SoftVelocity\AWS\Regions\ap-southeast-3 and set the Default value
!!! to the name of the server (Eg Asia Pacific (Seoul))
!!! The ClaAWS system assumes that the URL of any new server is in the form
!!! s3-<server_name>.amazonaws.com.  If the new server does not follow this pattern
!!! you can add the string value URL and set the value to the URL of the
!!! new server.  Eg s3.eu-central-1.amazonaws.com.  The URL for the new server
!!! should be published at http://docs.aws.amazon.com/general/latest/gr/rande.html#s3_region
!!! </summary>
ListRegions   PROCEDURE(RegionList regions, <*STRING errorMessage>),BOOL,PROC,DLL(_ABCDllMode_)

!!! <summary>
!!! Raises a connection to an AWS simple storage service (S3)
!!! </summary>
!!! <param name="AccessKey">
!!! The Access key that the user obtained when they signed up to AWS.  This should
!!! be an IAM account key
!!! </param>
!!! <param name="SecretKey">
!!! The Secret key for the AccessKey
!!! </param>
!!! <param name="errorMessage">
!!! optional string that will contain details about why the connection could not
!!! be raised
!!! </param>
!!! <returns>
!!! an IAWSS3Client if the connection is raised. NULL otherwise.  If NULL is returned,
!!! and errorMessage is passed, then errorMessage will contain information on why the connection
!!! failed
!!! </returns>
ConnectToS3  PROCEDURE(STRING AccessKey, STRING SecretKey, <*STRING errorMessage>),*IAWSS3Client,DLL(_ABCDllMode_)
    END
  END

!!! <summary>
!!! This class is here simply to get ClaAws.clw automatically linked into the appropriate app
!!! </summary>
AWSS3DummyClass     CLASS,TYPE,MODULE('ClaAwsS3.CLW'),LINK('ClaAwsS3.CLW',_ABCLinkMode_),DLL(_ABCDllMode_)
END

!!! <summary>
!!! code returned when EVENT:Notify is raised when asynchronously uploading or downloading a file to an AWS service
!!! </summary>
AWSNotification:Processing   EQUATE(0F42C13D3H)

AWSTransfer         ITEMIZE,PRE(AWSTransfer)
Transfering           EQUATE
Completed             EQUATE
Cancelled             EQUATE
Error                 EQUATE
                    END

!!! <summary>
!!! Queue used to get the list of buckets/vaults
!!! </summary>
AWSContainerList       QUEUE,TYPE
!!! <summary>
!!! The name of the bucket
!!! </summary>
Name                  STRING(FILE:MaxFilePath)
!!! <summary>
!!! The display name of the region where the bucket is.  Same as RegionList.DisplayName
!!! </summary>
RegionDisplayName         STRING(DisplayNameLen)
!!! <summary>
!!! The system name of the region where the bucket is.  Same as RegionList.SystemName
!!! </summary>
RegionSystemName          STRING(SystemNameLen)
                        END

!!! <summary>
!!! Queue used to get the list of folders or files
!!! </summary>
AWSObjectList       QUEUE,TYPE
!!! <summary>
!!! The name of the folder or file
!!! </summary>
Name                  STRING(FILE:MaxFilePath)
!!! <summary>
!!! True if the object is a folder.
!!! </summary>
IsFolder              BOOL
!!! <summary>
!!! When this file was last modified
!!! </summary>
LastModified          GROUP
Date                    DATE
Time                    TIME
                      END
!!! <summary>
!!! The size of the file
!!! </summary>
Size                  GROUP(INT64).
                    END

!!! <summary>
!!! Queue used to set and get meta data about a file
!!! </summary>
AWSMetaData         QUEUE,TYPE
!!! <summary>
!!! The name of the meta data.  Legal values are:
!!! Cache-Control       Specifies caching behavior along the request/reply chain.
!!! Content-Disposition Specifies presentational information for the object.
!!! Content-Encoding    Specifies what content encodings have been applied to the object
!!!                     and thus what decoding mechanisms must be applied to obtain
!!!                     the media-type referenced by the Content-Type header field.
!!! Content-Type        A standard MIME type describing the format of the object data.
!!! Expires             The date and time at which the object is no longer cacheable in the format @D10T@T4.
!!! x-amz-meta-<name>   <name> can be any name (without spaces).  Use these meta data to store your own
!!!                     information about the file
!!! 
!!! </summary>
key                   STRING(500)
value                 STRING(1000)
                    END

!!! <summary>
!!! Raises a connection to the S3 service on the passed region/server
!!! </summary>
IAWSS3Client   INTERFACE
!region Cleanup code
!!! <summary>
!!! Drops connection to the service and cleans up all memory used by the underlying
!!! class.
!!! Calling any methods in the interface after calling this method is very likely to
!!! cause a GPF
!!! </summary>
Disconnect            PROCEDURE()
!endregion
!region Error handling
!!! <summary>
!!! Get the message associated with the last error that occured for this client
!!! </summary>
Error                 PROCEDURE(),STRING
!!! <summary>
!!! Get the AWS error code.
!!! See http://docs.aws.amazon.com/AmazonS3/latest/API/ErrorResponses.html and
!!! http://docs.aws.amazon.com/amazonglacier/latest/dev/api-error-responses.html for                   a
!!! lists of possible error codes
!!! </summary>
ErrorCode                 PROCEDURE(),STRING
!endregion
!region Bucket methods
!!! <summary>
!!! Get a list of all buckets owned by this user
!!! </summary>
!!! <param name="containers">
!!! On success containers will contain a list of containers available to the user.
!!! Only containers.Name contains any information.
!!! </param>
!!! <returns>
!!! TRUE if the call worked.  If the call failed, you can use IAWSStorageClient.Error to get more information
!!! on the cause of the error.
!!! </returns>
ListContainers           PROCEDURE(*AWSContainerList containers),BOOL,PROC
!!! <summary>
!!! Create a bucket in the region.  Note that buckets must have a unique name across all regions and all users.
!!! You cannot have the same bucket name on the S3 European region as someone else has on the S3 US East (Virginia) region.
!!! </summary>
!!! <param name="Region">
!!! The System name of the region/server where the bucket should be created
!!! See the region equates for predefined servers or use the RegionList.SystemName
!!! returned from the ListRegion function
!!! </param>
!!! <returns>
!!! 0 = Success
!!! NoPathErr(3) = ClaRunExt.dll could not be found
!!! BadNameErr(45) = An invalid bucket name
!!! IsOpenErr(52) = Bucket already exists
!!! FileSystemErr(90) = Other error.  See IAWSStorageClient.Error to get more information
!!! </returns>
CreateContainer          PROCEDURE(STRING containerName, STRING region),LONG,PROC
!!! <summary>
!!! Deletes a bucket.  This function will fail if there are files in the container
!!! </summary>
!!! <returns>
!!! 0 = Success
!!! NoFileErr(2) = Bucket does not exist
!!! NoPathErr(3) = ClaRunExt.dll could not be found
!!! NoAccessErr(5) = Attempt to delete a bucket owned by another user 
!!! BadNameErr(45) = An invalid bucket name
!!! FileSystemErr(90) = Other error.  See IAWSStorageClient.Error to get more information
!!! </returns>
DeleteContainer       PROCEDURE(STRING containerName),LONG,PROC
!endregion
!region Folder methods
!!! <summary>
!!! Create a folder in the bucket.
!!! </summary>
!!! <param name="containerName">
!!! The Name of the bucket where the folder should be created
!!! </param>
!!! <param name="folderName">
!!! The name of the folder to create.  To nest folders you seperate them by slashes (/).
!!! Note that you do not need a parent folder to create a child folder.  So doing
!!! CreateContainer('MyTestContainer');CreateFolder('MyTestContainer', 'outerfolder/childfolder')
!!! is legal
!!! </param>
!!! <returns>
!!! 0 = Success
!!! NoPathErr(3) = ClaRunExt.dll could not be found
!!! BadNameErr(45) = An invalid bucket name
!!! IsOpenErr(52) = folder already exists
!!! FileSystemErr(90) = Other error.  See IAWSStorageClient.Error to get more information
!!! </returns>
CreateFolder       PROCEDURE(STRING containerName, STRING folderName),LONG,PROC
!!! <summary>
!!! Deletes an empty folder.  If there are files in the folder, the function will
!!! return no error, but the folder will not have been fully removed.  See notes.
!!! </summary>
!!! <notes>
!!! The AWS storage systems are actually flat.  There are no folders, only files.  However,
!!! there is a convention that most client tools use to make it appear that there are
!!! folders.  Files are stored in the format folder/folder/.../file.  Client tools 
!!! (like claawsstorage) will create a folder hierachy from the names.  A Folder is
!!! simply stored as a file with a name in the format folder/folder/.../.  Deleting
!!! the folder simply removes this file, but not any child files.
!!! </notes>
!!! <returns>
!!! 0 = Success
!!! NoFileErr(2) = folder does not exist
!!! NoPathErr(3) = ClaRunExt.dll could not be found
!!! NoAccessErr(5) = Attempt to delete a folder owned by another user 
!!! BadNameErr(45) = An invalid bucket name
!!! FileSystemErr(90) = Other error.  See IAWSStorageClient.Error to get more information
!!! </returns>
DeleteFolder       PROCEDURE(STRING containerName, STRING folderName),LONG,PROC
!endregion
!region File methods
!!! <summary>
!!! Get a list of all files and folders in a folder
!!! </summary>
!!! <param name="container">
!!! The Name of the bucket where the folder is
!!! </param>
!!! <param name="parentFolder">
!!! The name of the parent folder.  Pass an empty string to get a list of all files and folders in a container.
!!! Use / to separate nested folders.
!!! </param>
!!! <param name="files">
!!! A queue that will contain the list of files and folders in the parentFolder when
!!! ListFiles succeeds
!!! </param>
!!! <returns>
!!! TRUE if the call worked.  If the call failed, you can use IAWSStorageClient.Error to get more information
!!! on the cause of the error.
!!! </returns>
ListFiles         PROCEDURE(STRING container, STRING parentFolder, *AWSObjectList files),BOOL,PROC
!!! <summary>
!!! Returns TRUE if fileName exists in the container.  If FALSE is returned you should check
!!! ErrorCode() to see if the file does not exist or if an error occured trying to find the file
!!! </summary>
!!! <remarks>
!!! File names are case sensitive.
!!! To check if a folder exists add a trailing /
!!! </remarks>
FileExists        PROCEDURE(STRING container, STRING fileName),BOOL
!!! <summary>
!!! Uploads a file from the local machine to the AWS service.  If the file already exists on the service, it will
!!! be overwritten.  Note: You can only have one asynchronous upload or download active at one time.  If you 
!!! want to do multiple asynchronous uploads and downloads simultaneously then would will need multiple 
!!! IAWSStorageClient instances.  It is most efficient if these instances are each running on seperate threads.
!!! </summary>
!!! <param name="sourceFile">
!!! The fully qualified path to the file that is to be uploaded
!!! </param>
!!! <param name="containerName">
!!! The name of the container where the file is to be located
!!! </param>
!!! <param name="destinationFile">
!!! The name of the file including all folders where the file should be located
!!! </param>
!!! <param name="asynchronous">
!!! If false UploadFile will only return once the upload is complete.  You can check the return code to
!!! see if the upload was successful.  If true, UploadFile will return immediately.
!!! The IAWSStorageClient will generate regular EVENT:Notify events during the upload.  The third parameter
!!! of NOTIFICATION will hold the address of the IAWSStorageClient that generated the notification.  Use
!!! TransferStatus to get more details on the status of the transfer.
!!! </param>
!!! <param name="encrypted">
!!! Set to TRUE if you want the data to be encrypted on the service
!!! </param>
!!! <returns>
!!! 0 = Success
!!! NoFileErr(2) = file does not exist
!!! NoPathErr(3) = ClaRunExt.dll could not be found
!!! NoAccessErr(5) = Attempt to delete a folder owned by another user 
!!! BadNameErr(45) = An invalid bucket name
!!! FileSystemErr(90) = Other error.  See IAWSStorageClient.Error to get more information
!!! </returns>
UploadFile            PROCEDURE(STRING sourceFile, STRING containerName, STRING destinationFile, BOOL asynchronous = FALSE, BOOL encrypted = FALSE),LONG,PROC
!!! <summary>
!!! Downloads a file from the AWS service to the local machine.  If the file already exists, it will be overwritten.
!!! Note:  You can only have one asynchronous upload or download active at one time.  If you want to do multiple 
!!! asynchronous uploads and downloads simultaneously then would will need multiple IAWSStorageClient instances.
!!! It is most efficient if these instances are each running on seperate threads.
!!! </summary>
!!! <param name="containerName">
!!! The name of the container where the file is located
!!! </param>
!!! <param name="sourceFile">
!!! The name of the file including all folders
!!! </param>
!!! <param name="destinationFile">
!!! The fully qualified path where the file should be downloaded to.  This is the full name, not just
!!! the directory where the file should end up.
!!! </param>
!!! <param name="metaData">
!!! A list of extra information about the file.  A common type of meta data is ContentType which associates
!!! a MIME type with the file allowing browsers to call the appropriate application when downloading the file
!!! via it's URL.  For example metaData.key='ContentType' metaData.value='application/vnd.ms-excel' indicates
!!! that the uploaded file is an Excel spreadsheet.
!!! </param>
!!! <param name="asynchronous">
!!! If false DownloadFile will only return once the download is complete.  You can check the return code to
!!! see if the download was successful.  If true, DowloadFile will return immediately.
!!! The IAWSStorageClient will generate regular EVENT:Notify events during the upload.  The third parameter
!!! of NOTIFICATION will hold the address of the IAWSStorageClient that generated the notification.  Use
!!! TransferStatus to get more details on the status of the transfer.
!!! </param>
!!! <returns>
!!! 0 = Success
!!! NoFileErr(2) = file does not exist
!!! NoPathErr(3) = ClaRunExt.dll could not be found
!!! NoAccessErr(5) = Attempt to delete a folder owned by another user 
!!! BadNameErr(45) = An invalid bucket name
!!! FileSystemErr(90) = Other error.  See IAWSStorageClient.Error to get more information
!!! </returns>
DownloadFile          PROCEDURE(STRING containerName, STRING sourceFile, STRING destinationFile, BOOL asynchronous = FALSE),LONG,PROC
!!! <summary>
!!! Deletes a file 
!!! </summary>
!!! <param name="containerName">
!!! The name of the container where the file is located
!!! </param>
!!! <param name="fileName">
!!! The name of the file including all folders
!!! </param>
!!! <returns>
!!! 0 = Success
!!! NoFileErr(2) = file does not exist
!!! NoPathErr(3) = ClaRunExt.dll could not be found
!!! NoAccessErr(5) = Attempt to delete a folder owned by another user 
!!! BadNameErr(45) = An invalid bucket name
!!! FileSystemErr(90) = Other error.  See IAWSStorageClient.Error to get more information
!!! </returns>
DeleteFile            PROCEDURE(STRING containerName, STRING fileName),LONG,PROC
!region Asynchronous transfer methods
!!! <summary>
!!! Cancels the currently running asynchronous upload or download.  You may get EVENT:Notify events after this call
!!! as cancelation may not be instantaneous.
!!! </summary>
CancelTransfer        PROCEDURE(),LONG,PROC
!!! <summary>
!!! Gets the state of the current asynchronous upload or download
!!! </summary>
!!! <param name="state">
!!! Indicates what state the transfer is in.  Possible values are:
!!! AWSTransfer:Transfering indicates the file is still being transfered.  Use percentage to find out how much of the transfer is completed
!!! AWSTransfer:Completed   indicates the file has been transfered
!!! AWSTransfer:Cancelled   indicates that CancelTransfer() has been called
!!! AWSTransfer:Error       indicates an error occured during transfer.  See IAWSStorageClient.Error and IAWSStorageClient.ErrorCode for more information 
!!! </param>
!!! <param name="percentage">
!!! How much of the transfer has been completed
!!! </param>
TransferStatus    PROCEDURE(*LONG state, *LONG percentage)
!!! <summary>
!!! Gets the local file that is involved in the asynchronous upload or download
!!! </summary>
TransferingFile   PROCEDURE(),STRING
!!! <summary>
!!! Gets the name of the file on the S3 server that is involved in the asynchronous upload or download
!!! </summary>
TransferingS3File   PROCEDURE(),STRING
!!! <summary>
!!! During asynchronous uploads and downloads the IAWSStorageClient instance will generate regular EVENT:Notify events.
!!! When you call NOTIFICATION the first parameter will be set to AWSNotification:Processing.  This may clash with
!!! existing notifications in your code.  In this case you can use SetNotificationCode to configure the IAWSStorageClient instance
!!! to generate the passed notification code rather than AWSNotification:Processing
!!! </summary>
SetNotificationCode   PROCEDURE(LONG notificationCode)
!endregion
!region public access to file
!!! <summary>
!!! Get a URL that anyone can use to download the file.  This URL is only valid for a limited time period
!!! </summary>
!!! <param name="days">
!!! How many days the URL is valid for
!!! </param>
!!! <param name="hours">
!!! How many hours the URL is valid for
!!! </param>
!!! <param name="minutes">
!!! How many minutes the URL is valid for
!!! </param>
!!! <returns>
!!! The temporary URL 
!!! </returns>
GetTemporaryURL       PROCEDURE(STRING containerName, STRING fileName, LONG days, LONG hours, LONG minutes),STRING
!!! <summary>
!!! Get a URL that anyone can use to download the file.  This return a valid URL even if public access
!!! is not allowed
!!! </summary>
GetPermanentURL       PROCEDURE(STRING containerName, STRING fileName),STRING
!!! <summary>
!!! Allow or disallow public access to the file
!!! </summary>
AllowPublicAccess     PROCEDURE(STRING containerName, STRING fileName, BOOL public),LONG,PROC
!!! <summary>
!!! returns TRUE if public access is allowed for this file
!!! </summary>
HasPublicAccess       PROCEDURE(STRING containerName, STRING fileName),BOOL
!endregion
!region Meta Data
!!! <summary>
!!! Get all meta data associated with a file
!!! </summary>
!!! <param name="containerName">
!!! The name of the container where the file is located
!!! </param>
!!! <param name="fileName">
!!! The name of the file including all folders
!!! </param>
!!! <param name="metaData">
!!! Queue that contains a list of all meta data for the file on return
!!! </param>
!!! <returns>
!!! 0 = Success
!!! NoFileErr(2) = file does not exist
!!! NoPathErr(3) = ClaRunExt.dll could not be found
!!! FileSystemErr(90) = Other error.  See IAWSStorageClient.Error to get more information
!!! </returns>
GetMetaData           PROCEDURE(STRING containerName, STRING fileName, *AWSMetaData metaData),BOOL,PROC
!!! <summary>
!!! Get the value of the specified meta data
!!! </summary>
!!! <param name="containerName">
!!! The name of the container where the file is located
!!! </param>
!!! <param name="fileName">
!!! The name of the file including all folders
!!! </param>
!!! <param name="key">
!!! The meta data's name
!!! </param>
GetMetaData           PROCEDURE(STRING containerName, STRING fileName, STRING key),STRING
!!! <summary>
!!! Set the meta data for the file.  This replaces all existing meta data with the passed list
!!! </summary>
!!! <param name="containerName">
!!! The name of the container where the file is located
!!! </param>
!!! <param name="fileName">
!!! The name of the file including all folders
!!! </param>
!!! <param name="metaData">
!!! Queue that contains a list of all meta data that should be set.
!!! </param>
!!! <returns>
!!! 0 = Success
!!! NoFileErr(2) = file does not exist
!!! NoPathErr(3) = ClaRunExt.dll could not be found
!!! FileSystemErr(90) = Other error.  See IAWSStorageClient.Error to get more information
!!! </returns>
SetMetaData           PROCEDURE(STRING containerName, STRING fileName, *AWSMetaData metaData),BOOL,PROC
!!! <summary>
!!! Add/Change meta data for a file
!!! </summary>
!!! <param name="containerName">
!!! The name of the container where the file is located
!!! </param>
!!! <param name="fileName">
!!! The name of the file including all folders
!!! </param>
!!! <param name="key">
!!! The meta data key that should be set to the passed value
!!! Note that if the key is not a pre-defined key (See AWSMetaData for list) and
!!! the key does not start with x-amz-meta-, then AWS will automatically prepend
!!! x-amz-meta- to the key
!!! </param>
!!! <param name="value">
!!! The value that the meta data key should be set to
!!! </param>
!!! <returns>
!!! 0 = Success
!!! NoFileErr(2) = file does not exist
!!! NoPathErr(3) = ClaRunExt.dll could not be found
!!! FileSystemErr(90) = Other error.  See IAWSStorageClient.Error to get more information
!!! </returns>
SetMetaData           PROCEDURE(STRING containerName, STRING fileName, STRING key, STRING value),LONG,PROC
!!! <summary>
!!! Deletes the meta data key for a file.  Deleting non-existant meta data does not raise an error
!!! </summary>
!!! <param name="containerName">
!!! The name of the container where the file is located
!!! </param>
!!! <param name="fileName">
!!! The name of the file including all folders
!!! </param>
!!! <param name="key">
!!! The meta data key that should be removed.
!!! </param>
!!! <returns>
!!! 0 = Success
!!! NoFileErr(2) = file does not exist
!!! NoPathErr(3) = ClaRunExt.dll could not be found
!!! FileSystemErr(90) = Other error.  See IAWSStorageClient.Error to get more information
!!! </returns>
RemoveMetaData           PROCEDURE(STRING containerName, STRING fileName, STRING key),LONG,PROC
!endregion
!endregion
                END

_EndOfInclude_
