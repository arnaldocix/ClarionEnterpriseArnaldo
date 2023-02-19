!ABCIncludeFile(QuickSocket)

  OMIT('_EndDefine_',_QuickSocket_)
_QuickSocket_  EQUATE(1)

 INCLUDE('WINSOCK.INC'),ONCE
 INCLUDE ('JSON.INC'),ONCE

QuickSocketCmd ITEMIZE,PRE
Unknown              EQUATE(0)
Login                EQUATE(1) !login to the server
SendMessage          EQUATE(2) !Send a text message to a user
SendData             EQUATE(3) !TBD: Send a message to a user with data instead of text message, the data will be base64 encoded
SendNotify           EQUATE(4) !Like message but with also a Notification Code and the text
BroadcastMessage     EQUATE(5) !Like Notify but for all
BroadcastNotify      EQUATE(6) !Like Notify but for all
BroadcastShutdown    EQUATE(7) !specific brodcasting that the users shoudl answer shuting down
NotifyCleanConnected EQUATE(8)
NotifyBatchConnected EQUATE(9)
NotifyConnected      EQUATE(10)
NotifyDisconnected   EQUATE(11)
CmdAnswer            EQUATE(12) !answer returned by the server to the client containing the original cmd, a retval and an optional message
               END

QuickSocketCmdGroup GROUP,TYPE
Cmd                     LONG
CmdData                 STRING(1001)
    END

QuickSocket_ClientsQueue QUEUE,TYPE                          
socket                    SOCKET                            ! Client socket
id                        CSTRING(64)                       ! Human readable ID/name
                        END

QuickSocketClass        CLASS,TYPE,MODULE('QuickSocket.CLW' ),LINK('QuickSocket.CLW',_ABCLinkMode_),DLL(_ABCDllMode_)
wm                        LONG,PROTECTED                              ! WM registered for this object
serverRunning             BYTE,PROTECTED
connectionSocket          SOCKET,PROTECTED                            ! This is the listenning socket
serverIP                  CSTRING(64),PROTECTED
serverPort                LONG,PROTECTED
clientId                  CSTRING(64)                     ! Human readable ID/name
bufferReceived            STRING(1024),PROTECTED !Data received from the socket
bytesReceived             LONG,PROTECTED
WinRef                    &WINDOW,PROTECTED
_OldUserData              ULONG,PROTECTED ! Subclass user data
_OldWndProc               ULONG,PROTECTED ! Subclass window procedure specific to this object
Clients                   &QuickSocket_ClientsQueue,PROTECTED

CONSTRUCT                 PROCEDURE()!,FINAL
DESTRUCT                  PROCEDURE()!,FINAL
WindowSubClassEvents      PROCEDURE(UNSIGNED hWnd, SIGNED wMsg, UNSIGNED wParam, LONG lParam),LONG
InitSubClass              PROCEDURE(*WINDOW winRef)
KillSubClass              PROCEDURE()
GetMachineName            PROCEDURE(),STRING
GetMachineIpAddress       PROCEDURE(),STRING

_UseServerAsClient        BYTE,PROTECTED
SetUseServerAsClient      PROCEDURE(BYTE value)
GetUseServerAsClient      PROCEDURE(),BYTE
StartServer               PROCEDURE(*WINDOW winRef,STRING pServerIp,LONG pPortNumber, <STRING serverName>), LONG,PROC
StartServer               PROCEDURE(STRING pServerIp,LONG pPortNumber, <STRING serverName>), LONG,PROC
StopServer                PROCEDURE()
IsServerRunning           PROCEDURE(),BYTE
ConnectTo                 PROCEDURE(*WINDOW winRef,STRING pServerIp,LONG pPortNumber),LONG
ConnectTo                 PROCEDURE(STRING pServerIp,LONG pPortNumber),LONG
IsConnected               PROCEDURE(),BYTE
ClientExistBySocket       PROCEDURE(SOCKET client),BYTE
ClientExistById           PROCEDURE(STRING client),BYTE
GetClientId               PROCEDURE(SOCKET client),STRING
GetClientSocket           PROCEDURE(STRING client),SOCKET
SendToServer              PROCEDURE(STRING strMsg),LONG
Send                      PROCEDURE(STRING strMsg, SOCKET destination),LONG,PROC
Broadcast                 PROCEDURE(STRING strMsg),LONG,PROC

DoTakeWindowMessage       PROCEDURE(LONG client,LONG request),VIRTUAL,PROTECTED !,FINAL
TakeWindowMessage         PROCEDURE(LONG client,LONG request),VIRTUAL
OnClientClosed            PROCEDURE(LONG client),VIRTUAL,PROTECTED !,FINAL
OnClientConnect           PROCEDURE(LONG client),VIRTUAL,PROTECTED !,FINAL
DisconnectFrom            PROCEDURE()
DisconnectClient          PROCEDURE(SOCKET client),VIRTUAL !,FINAL
_CommandToString          PROCEDURE(LONG pCommand),STRING
                        END

QuickAppConnectClass    CLASS(QuickSocketClass),TYPE,MODULE('QuickSocket.CLW' ),LINK('QuickSocket.CLW',_ABCLinkMode_),DLL(_ABCDllMode_)
JSON                        &JSONDataClass,PROTECTED
JsonCmd                     LIKE(QuickSocketCmdGroup)
_NotifyOnConnection         BYTE,PROTECTED

CONSTRUCT                   PROCEDURE()!,FINAL
DESTRUCT                    PROCEDURE()!,FINAL

ConnectTo                   PROCEDURE(STRING pServerIp,LONG pPortNumber, STRING pUserName, STRING pUserPassword),LONG
Login                       PROCEDURE(STRING pUserName, STRING pUserPassword),LONG
SendServerResponse          PROCEDURE(LONG pToClientSocket, LONG pOriginalCommand, LONG pRetVal, STRING pRetMsg),LONG,PROC
RefreshNewConnectedUsers    PROCEDURE(SOCKET newClientSocket),LONG,PROC,PROTECTED
SendMessage                 PROCEDURE(STRING pToUserName, STRING pMsgText),LONG
SendNotify                  PROCEDURE(STRING pToUserName, LONG pNotificationCode, STRING pMsgText),LONG
SendNotify                  PROCEDURE(STRING pToUserName, LONG pNotificationCode, *GROUP pMsgContent),LONG
BroadcastNotify             PROCEDURE(LONG pNotificationCode, STRING pMsgText),LONG
BroadcastNotify             PROCEDURE(LONG pNotificationCode, *GROUP pMsgContent),LONG
BroadcastShutdown           PROCEDURE(LONG pNotificationCode, STRING pMsgText),LONG

DoTakeWindowMessage         PROCEDURE(LONG client,LONG request),DERIVED,PROTECTED !,FINAL

FromJSON                    PROCEDURE(*STRING pJSONString,*GROUP pJSONObject),PROTECTED !Shortcut to the internal json class method to be called inside the Event_Notification to parse the group in the message.
Event_ServerResponse        PROCEDURE(LONG pOriginalCommand, LONG pRetVal, STRING pRetMsg),PROTECTED,VIRTUAL !Called in the client after a call to methods like Login, Message, Broadcast, etc. the info come from the server response
Event_LoginOnServer         PROCEDURE(STRING pUserName, STRING pUserPassword, *BYTE pCancelOperation),PROTECTED,VIRTUAL
Event_Message               PROCEDURE(STRING pFromUserName, STRING pToUserName, STRING pMsgText),PROTECTED,VIRTUAL
Event_Notification          PROCEDURE(STRING pFromUserName, STRING pToUserName, LONG pNotificationCode, STRING pMsgText),PROTECTED,VIRTUAL
Event_BroadcastNotification PROCEDURE(STRING pFromUserName, LONG pNotificationCode, STRING pMsgText),PROTECTED,VIRTUAL
Event_BroadcastShutdown     PROCEDURE(STRING pFromUserName, LONG pNotificationCode, STRING pMsgText),PROTECTED,VIRTUAL
Event_NotifyCleanConnected  PROCEDURE(),PROTECTED,VIRTUAL
Event_NotifyDisconnected    PROCEDURE(STRING pUserName),PROTECTED,VIRTUAL
Event_NotifyConnected       PROCEDURE(STRING pUserName),PROTECTED,VIRTUAL
                        END

!_EndDefine_  