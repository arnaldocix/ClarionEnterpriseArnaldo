
!ABCIncludeFile

OMIT('_EndOfInclude_',_ErrorClassPresent_)
_ErrorClassPresent_ EQUATE(1)

  INCLUDE('ABWINDOW.INC'),ONCE
  INCLUDE('ERRORS.CLW'),ONCE

BaseButtonFeq EQUATE(100)
ListFeq       EQUATE(200)
ImageFeq      EQUATE(300)

! Message numbers for predefined error messages in ABERROR.TRN
  ITEMIZE(0),PRE(Msg)
None                      EQUATE
AbortReading              EQUATE
AccessDenied              EQUATE
AddAnother                EQUATE
AddFailed                 EQUATE
AddNewRecord              EQUATE
CloseFailed               EQUATE
ConcurrencyFailed         EQUATE
ConcurrencyFailedFromForm EQUATE
ConcurrencyFailedFromBrowse EQUATE
ConfirmCancel             EQUATE
ConfirmDelete             EQUATE
CreateFailed              EQUATE
DeleteFailed              EQUATE
DeleteIllegal             EQUATE
DuplicateKey              EQUATE
ExecutionOfSqlFailed      EQUATE
FetchOfSqlFailed          EQUATE
FieldNotInFile            EQUATE
FieldNotInList            EQUATE
FieldOutOfRange           EQUATE
FieldOutOfRangeHigh       EQUATE
FieldOutOfRangeLow        EQUATE
FileLoadFailed            EQUATE
GetRecordFailed           EQUATE
InsertIllegal             EQUATE
InvalidFile               EQUATE
LogoutFailed              EQUATE
NoRecords                 EQUATE
OpenFailed                EQUATE
ProcedureToDo             EQUATE
PutFailed                 EQUATE
RebuildFailed             EQUATE
RebuildKey                EQUATE
RecordHeld                EQUATE
RecordLimit               EQUATE
RestrictDelete            EQUATE
RestrictUpdate            EQUATE
RetryAutoInc              EQUATE
RetryDelete               EQUATE
RetrySave                 EQUATE
SaveRecord                EQUATE
SearchReachedBeginning    EQUATE
SearchReachedEnd          EQUATE
UpdateIllegal             EQUATE
UseClosedFile             EQUATE
ViewOpenFailed            EQUATE
QBEColumnNotSupported     EQUATE
SMTPError                 EQUATE
NNTPError                 EQUATE
WinsockError              EQUATE
WMFAddQueueFailed         EQUATE
WMFFileNotValid           EQUATE
WMFInvalidControl         EQUATE
WMFObjectIdNotFound       EQUATE
RWPPoorComment            EQUATE
RWPUnknownProperty        EQUATE
  END

ErrorEntryGroup               GROUP,TYPE         ! List of all translated error messages
Id                              UNSIGNED         ! Error message identifier
Message                         &STRING          ! Message text
Title                           &STRING          ! Error window caption bar text
Fatality                        BYTE             ! Severity of error
Category                        ASTRING          ! Optional category for this error, if blank uses CurrentCategory
                              END
ErrorEntry                    QUEUE(ErrorEntryGroup),TYPE         ! List of all translated error messages
                              END


ErrorHistoryListGroup         GROUP,TYPE
Category                        ASTRING
Txt                             CSTRING(1024)   ! Fully expanded text of message (neeed because macros reflect 'current state')
Id                              SHORT
                              END
ErrorHistoryList              QUEUE(ErrorHistoryListGroup),TYPE
                              END


ErrorBlock                    GROUP,TYPE         ! Parameter to AddErrors procedure
Number                          USHORT           ! Current error message identifier
                              END

MessageMaxlen                 EQUATE(1024)


ErrorLogInterface             INTERFACE                         !All methods should return Level:Benign (everything ok) or Level:Notify (couldn't complete action)
Close                           PROCEDURE(BYTE Force = False),BYTE,PROC
Open                            PROCEDURE(BYTE Force = False),BYTE,PROC
Take                            PROCEDURE(STRING ErrText),BYTE,PROC
                              END


StandardErrorLogClass         CLASS,TYPE,IMPLEMENTS(ErrorLogInterface),MODULE('ABERROR.CLW'),LINK('ABERROR.CLW',_ABCLinkMode_),DLL(_ABCDllMode_)
critSect                        &ICriticalSection,PRIVATE
Usage                           LONG,PRIVATE

Construct                       PROCEDURE
Destruct                        PROCEDURE

Open                            PROCEDURE(BYTE Force = False),BYTE,PROC,PROTECTED
Close                           PROCEDURE(BYTE Force = False),BYTE,PROC,PROTECTED
                              END

  ITEMIZE(0)
ErrClarion  EQUATE
ErrFile     EQUATE
  END

ErrorStatusGroup              GROUP,TYPE
FieldName                       CSTRING(MessageMaxlen)   ! name of field that produced the error
FileName                        CSTRING(MessageMaxlen)   ! name of file that produced the error
KeyName                         CSTRING(MessageMaxlen)   ! name of key that produced the error
MessageText                     CSTRING(MessageMaxlen)   ! message text
SaveError                       CSTRING(255)               ! Clarion error message
SaveErrorCode                   LONG                       ! Clarion error code
SaveFileError                   CSTRING(255)               ! File system's error message
SaveFileErrorCode               CSTRING(255)               ! File system's error code
ErrorBuffer                     LIKE(ErrorEntryGroup)
PrevErrorID                     UNSIGNED
                              END


ErrorStatusClass              CLASS,TYPE,MODULE('ABERROR.CLW'),LINK('ABERROR.CLW',_ABCLinkMode_),DLL(_ABCDllMode_)
V                               LIKE(ErrorStatusGroup),PRIVATE
Construct                       PROCEDURE
Destruct                        PROCEDURE
                              END

ErrorClass                    CLASS,TYPE,MODULE('ABERROR.CLW'),LINK('ABERROR.CLW',_ABCLinkMode_),DLL(_ABCDllMode_)
DefaultCategory                 ASTRING,PRIVATE                    ! The default category associated with errors that don't have their own
Errors                          &ErrorEntry,PRIVATE                ! Queue to hold all translated error messages
ErrorLog                        &ErrorLogInterface,PROTECTED       ! Interface to error log
History                         &ErrorHistoryList,PROTECTED

ProcNames                       &PNQ,PRIVATE
StdErrorLog                     &StandardErrorLogClass,PRIVATE     ! The standard error log handler
Silent                          BYTE,PRIVATE                       ! Set true to force silent error handling
LogErrors                       BYTE,PRIVATE                       ! Set true to send errors to the error log
HistoryThreshold                LONG,PRIVATE                       ! Set number of items to 'store' in history setting to -1 means keep all errors, setting to 0 switches off history
HistoryViewLevel                LONG,PRIVATE                       ! Sets the error level which triggers history viewing, only valid with HistoryThreshold <> 0
HistoryResetOnView              BYTE,PRIVATE                       ! Set true to auto clear history after viewing, only valid with HistoryThreshold <> 0
critSect                        &ICriticalSection,PRIVATE
Status                          &ErrorStatusClass,PRIVATE
MsgStyle                        SHORT,PRIVATE

Construct                       PROCEDURE
Destruct                        PROCEDURE,VIRTUAL

AddError                        PROCEDURE(USHORT Id,*STRING Message,*STRING Title,BYTE Fatality,ASTRING Category),VIRTUAL
AddErrors                       PROCEDURE(ErrorBlock EB),VIRTUAL
AddHistory                      PROCEDURE,VIRTUAL
GetCategory                     PROCEDURE(SHORT Id = -1),ASTRING   ! Gets the category of a specific eror, or it -1 the default category
GetError                        PROCEDURE(UNSIGNED errType = ErrClarion),STRING
GetErrorcode                    PROCEDURE(UNSIGNED errType = ErrClarion),LONG
GetProcedureName                PROCEDURE,STRING
HistoryMsg                      PROCEDURE(STRING Caption, LONG Icon, LONG Buttons, LONG DefaultButton),LONG,PROC,VIRTUAL
Init                            PROCEDURE()   !,EXTENDS
Init                            PROCEDURE(ErrorStatusClass pErrorStatusClass)   !,EXTENDS
Kill                            PROCEDURE
Message                         PROCEDURE(SHORT Id,LONG Buttons,LONG Default),LONG
Msg                             PROCEDURE(STRING Txt, <STRING Caption>, <STRING Icon>, LONG Buttons = Button:Ok, LONG DefaultButton = 0, LONG Style = 0),LONG,PROC,VIRTUAL,PROTECTED
MessageBox                      PROCEDURE(BYTE Level = Level:Benign, STRING Txt, <STRING Caption>, <STRING Icon>, LONG Buttons, LONG DefaultButton, LONG Style),LONG,VIRTUAL,PROTECTED
RemoveErrors                    PROCEDURE(ErrorBlock EB)
RemoveError                     PROCEDURE(USHORT Id),VIRTUAL
ResetHistory                    PROCEDURE                          ! Reset(empty) the current history state
SetCategory                     PROCEDURE(SHORT Id = -1, ASTRING Category)  ! If id = -1, sets default category, otherwise sets the category of a specific error
SetErrorLog                     PROCEDURE(ErrorLogInterface ErrLog),VIRTUAL
SetErrors                       PROCEDURE
SetFatality                     PROCEDURE(SHORT Id,BYTE Level),BYTE,PROC
SetField                        PROCEDURE(STRING Text)
SetFile                         PROCEDURE(STRING Text)
SetKey                          PROCEDURE(STRING Text)
SetId                           PROCEDURE(USHORT Id),BYTE,PROTECTED
SetId                           PROCEDURE(USHORT Id,UNSIGNED StartPos),BYTE,PRIVATE
GetErrorEntry                   PROCEDURE(USHORT Id,*ErrorEntryGroup GErrorEntry),BYTE,PROC,PROTECTED
GetErrorEntry                   PROCEDURE(USHORT Id,*ErrorEntryGroup GErrorEntry,UNSIGNED StartPos),BYTE,PRIVATE
SetErrorEntry                   PROCEDURE(USHORT Id,*ErrorEntryGroup GErrorEntry,UNSIGNED StartPos),BYTE,PROC,PRIVATE
SetProcedureName                PROCEDURE(<STRING Name>)           ! Omit when leaving scope
SubsString                      PROCEDURE,STRING,PROTECTED
TakeBenign                      PROCEDURE,BYTE,VIRTUAL,PROC,PROTECTED
TakeError                       PROCEDURE(SHORT Id),BYTE,PROC
TakeFatal                       PROCEDURE,BYTE,VIRTUAL,PROC,PROTECTED
TakeNotify                      PROCEDURE,VIRTUAL,PROTECTED
TakeOther                       PROCEDURE,BYTE,VIRTUAL,PROC,PROTECTED
TakeProgram                     PROCEDURE,BYTE,VIRTUAL,PROC,PROTECTED
TakeUser                        PROCEDURE,BYTE,VIRTUAL,PROC,PROTECTED
Throw                           PROCEDURE(SHORT Id),BYTE,PROC  !,EXTENDS
ThrowFile                       PROCEDURE(SHORT Id,STRING FileName),BYTE,PROC  !,EXTENDS
ThrowMessage                    PROCEDURE(SHORT Id,STRING Text),BYTE,PROC  !,EXTENDS
ViewHistory                     PROCEDURE,VIRTUAL                  ! View the current error history state
!Access to non threaded properties
GetDefaultCategory              PROCEDURE(),ASTRING,PROTECTED!,FINAL ! The default category associated with errors that don't have their own
SetDefaultCategory              PROCEDURE(ASTRING pDefaultCategory),PROTECTED!,FINAL ! The default category associated with errors that don't have their own
SetSilent                       PROCEDURE(BYTE pSilent=1)!,FINAL   ! Set true to force silent error handling
GetSilent                       PROCEDURE(),BYTE!,FINAL            ! Get silent error handling status
SetLogErrors                    PROCEDURE(BYTE pLogErrors=1)!,FINAL ! Set true to send errors to the error log
GetLogErrors                    PROCEDURE(),BYTE!,FINAL            ! Get the error log status
SetHistoryThreshold             PROCEDURE(LONG pHThreshold=-1)!,FINAL ! Set number of items to 'store' in history setting to -1 means keep all errors, setting to 0 switches off history
GetHistoryThreshold             PROCEDURE(),LONG!,FINAL            ! Set number of items to 'store' in history setting to -1 means keep all errors, setting to 0 switches off history
SetHistoryViewLevel             PROCEDURE(LONG pHViewLevel)!,FINAL ! Sets the error level which triggers history viewing, only valid with HistoryThreshold <> 0
GetHistoryViewLevel             PROCEDURE(),LONG !,FINAL           ! Gets the error level which triggers history viewing, only valid with HistoryThreshold <> 0
SetHistoryResetOnView           PROCEDURE(BYTE pHResetOnView=1)!,FINAL ! Set true to auto clear history after viewing, only valid with HistoryThreshold <> 0
GetHistoryResetOnView           PROCEDURE(),BYTE!,FINAL            ! Get auto clear history status
!Access to the threaded class
StoreErrorEntry                 PROCEDURE(USHORT Id),PRIVATE!,FINAL
GetFileName                     PROCEDURE(),STRING,PROTECTED!,FINAL
SetFileName                     PROCEDURE(STRING pFileName),PROTECTED!,FINAL
GetFieldName                    PROCEDURE(),STRING,PROTECTED!,FINAL
SetFieldName                    PROCEDURE(STRING pFieldName),PROTECTED!,FINAL
GetKeyName                      PROCEDURE(),STRING,PROTECTED!,FINAL
SetKeyName                      PROCEDURE(STRING pKeyName),PROTECTED!,FINAL
GetMessageText                  PROCEDURE(),STRING,PROTECTED!,FINAL
SetMessageText                  PROCEDURE(STRING pMessageText),PROTECTED!,FINAL
GetSaveError                    PROCEDURE(),STRING,PROTECTED!,FINAL
SetSaveError                    PROCEDURE(STRING pSaveError),PROTECTED!,FINAL
GetSaveErrorCode                PROCEDURE(),LONG,PROTECTED!,FINAL
SetSaveErrorCode                PROCEDURE(LONG pSaveErrorCode),PROTECTED!,FINAL
GetSaveFileError                PROCEDURE(),STRING,PROTECTED!,FINAL
SetSaveFileError                PROCEDURE(STRING pSaveFileError),PROTECTED!,FINAL
GetSaveFileErrorCode            PROCEDURE(),STRING,PROTECTED!,FINAL
SetSaveFileErrorCode            PROCEDURE(STRING pSaveFileErrorCode),PROTECTED!,FINAL
GetMsgStyle                     PROCEDURE(),SHORT!,FINAL
SetMsgStyle                     PROCEDURE(SHORT MsgStyle)!,FINAL
SetPrevErrorID                  PROCEDURE(UNSIGNED pErrorID),PROTECTED!,FINAL
GetPrevErrorID                  PROCEDURE(),UNSIGNED,PROTECTED!,FINAL
SetErrorBufferId                PROCEDURE(UNSIGNED pErrorID),PROTECTED!,FINAL
GetErrorBufferId                PROCEDURE(),UNSIGNED,PROTECTED!,FINAL
SetErrorBufferMessage           PROCEDURE(STRING pMessage),PROTECTED!,FINAL
GetErrorBufferMessage           PROCEDURE(),STRING,PROTECTED!,FINAL
SetErrorBufferTitle             PROCEDURE(STRING pTitle),PROTECTED!,FINAL
GetErrorBufferTitle             PROCEDURE(),STRING,PROTECTED!,FINAL
SetErrorBufferFatality          PROCEDURE(BYTE pFatality),PROTECTED!,FINAL
GetErrorBufferFatality          PROCEDURE(),STRING,PROTECTED!,FINAL
SetErrorBufferCategory          PROCEDURE(ASTRING pCategory),PROTECTED!,FINAL
GetErrorBufferCategory          PROCEDURE(),ASTRING,PROTECTED!,FINAL
GetAllStatusVariables           PROCEDURE(*ErrorStatusGroup pErrorStatusGroup),PROTECTED!,FINAL

                              END

MsgBoxClass                   CLASS(WindowManager),TYPE,MODULE('ABERROR.CLW'),LINK('ABERROR.CLW',_ABCLinkMode_),DLL(_ABCDllMode_)
ButtonTypes                     LONG,PROTECTED
Caption                         &STRING,PROTECTED
Err                             &ErrorClass,PROTECTED
Icon                            LONG,PROTECTED
HistoryHandler                  &WindowComponent
MsgButtons                      LONG,PRIVATE
MsgButtonID                     LONG,DIM(8),PRIVATE
MsgRVal                         LONG
Style                           LONG,PROTECTED
Win                             &WINDOW,PROTECTED

FetchFeq                        PROCEDURE(BYTE Btn),SHORT
FetchStdButton                  PROCEDURE(SHORT Feq),BYTE
Init                            PROCEDURE(WINDOW Win, ErrorClass Err, <STRING Caption>, LONG Icon, LONG Buttons = Button:Ok, LONG DefaultButton = 0, LONG Style = 0)
Init                            PROCEDURE,BYTE,PROC,DERIVED
Kill                            PROCEDURE,BYTE,PROC,DERIVED
SetupAdditionalFeqs             PROCEDURE,VIRTUAL,PROTECTED
TakeAccepted                    PROCEDURE,BYTE,PROC,DERIVED
                              END


HistHandlerClass              CLASS,TYPE,IMPLEMENTS(WindowComponent),MODULE('ABERROR.CLW'),LINK('ABERROR.CLW',_ABCLinkMode_),DLL(_ABCDllMode_)
Err                             &ErrorClass,PROTECTED
History                         &ErrorHistoryList
LBColumns                       SHORT,PROTECTED
Win                             &WINDOW,PROTECTED

_VLBProc                        PROCEDURE(LONG RowNum, SHORT ColNum),STRING,PRIVATE

Init                            PROCEDURE(WINDOW Win, ErrorClass Err, ErrorHistoryList History)
TakeEvent                       PROCEDURE,BYTE,VIRTUAL
VLBProc                         PROCEDURE(LONG RowNum, SHORT ColNum),STRING,VIRTUAL,PROTECTED
                              END


  _EndOfInclude_



