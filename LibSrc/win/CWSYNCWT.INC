!ABCIncludeFile
  OMIT('__EndOfInclude__',__CWSYNCWT_INC)

__CWSYNCWT_INC  EQUATE(1)

    INCLUDE('CWSYNCH.INT'),ONCE
  
WorkingThreadManager CLASS,MODULE('CWSYNCWT.CLW'),LINK('CWSYNCWT.CLW',1),DLL(0),TYPE
_Finish                 BYTE,PRIVATE
_IsCancelled            BYTE,PRIVATE
_IsPaused               BYTE,PRIVATE
_UseNotification        BYTE,PRIVATE
_CallingThreadNumber    SIGNED,PRIVATE
_WorkingThreadNumber    SIGNED,PRIVATE
_StackSize              UNSIGNED,PRIVATE
_Lock_WorkingThread     &ICriticalSection,PRIVATE
_AccessTo_IsCancelled   &IReaderWriterLock,PRIVATE
_AccessTo_IsPaused      &IReaderWriterLock,PRIVATE
_AccessTo_StackSize     &IReaderWriterLock,PRIVATE
_AccessTo_ClassData     &IReaderWriterLock,PRIVATE
_ThreadProcAddress      LONG,PRIVATE
_WhenFinishProcAddress  LONG,PRIVATE
_CallerUpdateAddress    LONG,PRIVATE
Construct               PROCEDURE() !,FINAL
Destruct                PROCEDURE() !,FINAL

!!! <summary>
!!!	 StartProcess() calls the virtual method WorkingThreadProcess
!!!  WorkingThreadProcess is where you place the code that starts your PROCEDURE on a background thread
!!! </summary>
!!! <param name="pUseNotification">If the value is true (default) when the background Procedure is finished 
!!!                                the virtual WhenProcessFinish method is called </param>
StartProcess            PROCEDURE(BYTE pUseNotification = 1)
StartProcess            PROCEDURE(BYTE pUseNotification = 1, LONG pThreadProcAddress, LONG pWhenFinishProcAddress, LONG pCallerUpdateAddress)
StartProcessThreaded    PROCEDURE()!Should be private to the module
PauseProcess            PROCEDURE()
ResumeProcess           PROCEDURE()
CancelProcess           PROCEDURE()
ResetProcess            PROCEDURE()
InitializeProcess       PROCEDURE(),VIRTUAL
DoWorkingThreadProcess  PROCEDURE(),PRIVATE
WorkingThreadProcess    PROCEDURE(),VIRTUAL,PROTECTED
TakeEvent               PROCEDURE()
DoWhenProcessFinish     PROCEDURE(),PRIVATE
WhenProcessFinish       PROCEDURE(),VIRTUAL,PROTECTED
DoCallerUpdate          PROCEDURE(),PRIVATE
UpdatingCaller          PROCEDURE(),VIRTUAL,PROTECTED
UpdateCaller            PROCEDURE(),PROTECTED
IsPaused                PROCEDURE(),BYTE
IsCancelled             PROCEDURE(),BYTE
IsStarted               PROCEDURE(),BYTE
IsProcessFinish         PROCEDURE(),BYTE
AssignToClass           PROCEDURE(BYTE pVal=0)
AssignFromClass         PROCEDURE(BYTE pVal=0)
DoAssignToClassTmp      PROCEDURE(BYTE pVal=0),VIRTUAL,PROTECTED
DoAssignFromClassTmp    PROCEDURE(BYTE pVal=0),VIRTUAL,PROTECTED
WriteToClassTmp         PROCEDURE(BYTE pVal=0),PROTECTED
ReadFromClassTmp        PROCEDURE(BYTE pVal=0),PROTECTED
DoWriteToClassTmp       PROCEDURE(BYTE pVal=0),VIRTUAL,PROTECTED
DoReadFromClassTmp      PROCEDURE(BYTE pVal=0),VIRTUAL,PROTECTED

GetWorkingThreadNumber  PROCEDURE(),LONG
GetStackSize            PROCEDURE(),UNSIGNED
SetStackSize            PROCEDURE(UNSIGNED pValue)

 END
! __EndOfInclude__    