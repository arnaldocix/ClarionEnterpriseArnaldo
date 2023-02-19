OMIT('_EndOfInclude_',_H5ServerPresent_)
_H5ServerPresent_ EQUATE(1)

                     INCLUDE('H5CLIENT.INC'),ONCE
                     INCLUDE('H5BROKER.INC'),ONCE
                     INCLUDE('H5FILES.INC'),ONCE

! Internet classes and types

NET:Unknown          EQUATE(0)
NET:Terminate        EQUATE(1)
NET:Request          EQUATE(2)

SubmitItemClass      CLASS,MODULE('H5SERVER.CLW'),TYPE,DLL(BaseClassDllMode)
Reset                  PROCEDURE(STRING)

Event                  SIGNED
Extra                  ANY
Feq                    SIGNED
Name                   ANY
NewValue               ANY
                     END


ShutDownClass        CLASS,MODULE('ABSTRACT'),TYPE
Close                  PROCEDURE,VIRTUAL
                     END


WebServerClass       CLASS,MODULE('H5SERVER.CLW'),LINK('H5SERVER',LinkBaseClasses),TYPE,DLL(BaseClassDllMode)
Connect                PROCEDURE
GetInternetEnabled     PROCEDURE,BYTE
GetReadyForPage        PROCEDURE,BYTE
GetRequestedWholePage  PROCEDURE,BYTE
GetSendWholePage       PROCEDURE,BYTE
Halt                   PROCEDURE
Init                   PROCEDURE(*BrokerClass Broker, *ShutDownClass ShutDownManager, <STRING PageToReturnTo>, SIGNED TimeOut, <STRING JQueryLibraryPath>, WebFilesClass Files)
Kill                   PROCEDURE
Quit                   PROCEDURE,VIRTUAL
SetDialogPageBackground    PROCEDURE(LONG=COLOR:None, <STRING>)
SetDialogWindowBackground  PROCEDURE(LONG=COLOR:None, <STRING>)
SetSendWholePage       PROCEDURE(BYTE)
SetNewPageDisable      PROCEDURE(SIGNED)        ! Call to disable sending a new page, until an external event has occured.
SetNextAction          PROCEDURE,*SubmitItemClass
TakeEvent              PROCEDURE,SIGNED
TakeRequest            PROCEDURE(STRING),SIGNED,PRIVATE
TakeRequest            PROCEDURE,PRIVATE,NAME('WebServer$TakeRequest')  ! called from RPC
TakePageSent           PROCEDURE

Aborting               BYTE,PRIVATE
Active                 SIGNED(FALSE)            ! Was this program started via the Application Broker?
ArgIndex               SIGNED
Arguments              ANY,PRIVATE
Broker                 &BrokerClass
CurSubmit              &SubmitItemClass,PROTECTED
Client                 &WebClientManagerClass
CommandLine            ANY                      ! Program options e.g. /../myprog.exe.0/?args
DialogPageBackColor    LONG
DialogPageImage        CSTRING(FILE:MaxFileName)
DialogWinBackColor     LONG
DialogWinImage         CSTRING(FILE:MaxFileName)
Disabled               BYTE,PRIVATE
Files                  &WebFilesClass
GotCommandLine         BYTE
IgnoreRequest          BYTE,PRIVATE
InRequest              BYTE,PRIVATE
JQueryLibraryPath      CSTRING(FILE:MaxFileName)
LastRequest            LONG,PRIVATE
PageToReturnTo         CSTRING(FILE:MaxFileName)
RequestPending         BYTE,PRIVATE
RequestedWholePage     BYTE,PRIVATE
SendWholePage          BYTE,PRIVATE
ShutDownManager        &ShutDownClass,PRIVATE
TimeOut                SIGNED
                     END



_EndOfInclude_
