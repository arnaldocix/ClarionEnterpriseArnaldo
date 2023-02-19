!ABCIncludeFile(ABC)

  OMIT('_EndOfInclude_',_FileTriggerClassPresent_)  ! Omit this if already compiled
_FileTriggerClassPresent_         EQUATE(1)

  INCLUDE('FILECB.INC'),ONCE

!---------------------------------------------------------

TriggerManager               CLASS,IMPLEMENTS(FileCallBackInterface),TYPE,MODULE('TRIGGER.CLW'),LINK('TRIGGER.CLW',_ABCLinkMode_),DLL(_ABCDllMode_)

Construct                       PROCEDURE !,FINAL
Destruct                        PROCEDURE !,FINAL
Buffer                          &GROUP,PROTECTED           ! The record buffer
File                            &File                      ! Reference to the object FILE
PreviousBuffer                  &STRING,PROTECTED          ! Store the saved buffer to be used in the Post trigger methods, the buffer is stored on every read action
SavePreviousBuffer              BYTE                       ! Tell to the FileManager to store a copy of the buffer

InView                          BYTE,PRIVATE               ! Used by File Callback code
StopOperation                   BYTE,PROTECTED             ! Set this to true in the PreXXXX Trigger procedures to stop the file operation from being executed

Setup                           PROCEDURE(),VIRTUAL
Setup                           PROCEDURE(*FILE TheFile,*GROUP TheBuffer)!,FINAL
FunctionCalled                  PROCEDURE(SIGNED opCode, *Params Parameters, *CSTRING ErrCode, *CSTRING ErrMsg), BYTE,PROTECTED,VIRTUAL
FunctionDone                    PROCEDURE(SIGNED opCode, *Params Parameters, *CSTRING ErrCode, *CSTRING ErrMsg), BYTE,PROTECTED,VIRTUAL
! Client Triggers
PreDelete                       PROCEDURE(*CSTRING ErrCode, *CSTRING ErrMsg),BYTE,VIRTUAL
PostDelete                      PROCEDURE(*CSTRING ErrCode, *CSTRING ErrMsg),BYTE,VIRTUAL
PreInsert                       PROCEDURE(SIGNED OpCode, UNSIGNED AddLen, *CSTRING ErrCode, *CSTRING ErrMsg),BYTE,VIRTUAL
PostInsert                      PROCEDURE(*CSTRING ErrCode, *CSTRING ErrMsg),BYTE,VIRTUAL
PreUpdate                       PROCEDURE(LONG Pntr, UNSIGNED PutLen, *CSTRING ErrCode, *CSTRING ErrMsg),BYTE,VIRTUAL
PostUpdate                      PROCEDURE(*CSTRING ErrCode, *CSTRING ErrMsg),BYTE,VIRTUAL
PreFetch                        PROCEDURE(SIGNED OpCode, KEY key, STRING positionBuffer, LONG pointer, UNSIGNED recLen, *CSTRING ErrCode, *CSTRING ErrMsg),BYTE,VIRTUAL
PostFetch                       PROCEDURE(SIGNED OpCode, KEY key, STRING positionBuffer, LONG pointer, UNSIGNED recLen, *CSTRING ErrCode, *CSTRING ErrMsg),BYTE,VIRTUAL
PreCreate                       PROCEDURE(*CSTRING ErrCode, *CSTRING ErrMsg),BYTE,VIRTUAL
PostCreate                      PROCEDURE(*CSTRING ErrCode, *CSTRING ErrMsg),BYTE,VIRTUAL
                              END
_EndOfInclude_
