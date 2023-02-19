  MEMBER

  MAP
     QuickSocketWindowSubClass(UNSIGNED hWnd, UNSIGNED wMsg, UNSIGNED wParam, LONG lParam), LONG, PASCAL,PRIVATE
     _TRACE(STRING sDbgMessage)
     MODULE('')
       SLEEP(LONG),PASCAL
     END
     MODULE('C API')
         LStrCpy(*CString DestinationPtr,LONG SourcePtr),LONG,PASCAL,RAW,PROC
         MemCpy(Long,Long,Unsigned),Name('_memcpy'),Dll(dll_mode)
     END
     MODULE('winapi')
         __SetWindowLong(ulong hwnd, signed Index, long dwNewLong), long, pascal, raw, name('SetWindowLongA'), proc
         __GetWindowLong(ulong hwnd, signed Index), long, pascal, raw, name('GetWindowLongA')
         __OutPutDebugString( *CSTRING szMsg ), RAW, PASCAL, DLL(TRUE), NAME('OutPutDebugStringA')        
     END
  END

__GWL_WNDPROC equate(-4)
__GWL_USERDATA equate(-21)
  
  INCLUDE('QuickSock.inc'),ONCE
  INCLUDE ('JSON.INC'),ONCE

QuickSocketCmdLoginGroup GROUP,TYPE
UID              CSTRING(21)
UPWD             CSTRING(17)
    END

QuickSocketUsersQueue QUEUE,TYPE
UID              CSTRING(21)
    END


QuickSocketCmdMsgGroup GROUP,TYPE
MsgFrom          CSTRING(21)
MsgTo            CSTRING(21)
MsgText          CSTRING(501)
    END

QuickSocketCmdNotifyGroup GROUP,TYPE
MsgFrom          CSTRING(21)
MsgTo            CSTRING(21)
MsgNotify        LONG
MsgText          CSTRING(501)
    END

QuickSocketCmdBroadcastGroup GROUP,TYPE
MsgFrom          CSTRING(21)
MsgNotify        LONG
MsgText          CSTRING(501)
    END

QuickSocketCmdDataGroup GROUP,TYPE
MsgFrom          CSTRING(21)
MsgTo            CSTRING(21)
MsgTotal         LONG
MsgCurrent       LONG
MsgData          CSTRING(501)
    END

QuickSocketCmdAnswerGroup GROUP,TYPE
OrgCmd                     LONG
RetVal                     LONG
RetMsg                     CSTRING(501)
    END

_TRACE                    PROCEDURE(STRING sDbgMessage)
szMessage         CSTRING(2048)
lMsgLen           LONG
szOutPut          CSTRING(124)
lMsgPtr           LONG
 CODE
    szMessage = CLIP( sDbgMessage )
    lMsgLen = LEN( CLIP( szMessage ) )
    IF lMsgLen <= 120
      szMessage = CLIP( szMessage ) & '<10><13>'
      __OutPutDebugString( szMessage )
    ELSE
      szOutPut = szMessage[ 1 : 120 ] & '<10><13>'
      __OutPutDebugString( szOutPut )
      lMsgPtr = 121
      LOOP
        IF lMsgPtr >= lMsgLen
          BREAK
        END
        IF lMsgPtr + 120 > lMsgLen
          szOutPut = szMessage[ lMsgPtr : lMsgLen ] & '<10><13>'
          __OutPutDebugString( szOutPut )
          BREAK
        ELSE
          szOutPut = szMessage[ lMsgPtr : lMsgPtr + 120 ] & '<10><13>'
          __OutPutDebugString( szOutPut )
        END
        lMsgPtr += 121
      END
      szOutPut = '<10><13>'
      __OutPutDebugString( szOutPut )
    END
                      
QuickSocketClass.CONSTRUCT   PROCEDURE()
 CODE
    SELF.Clients &= NEW QuickSocket_ClientsQueue
    SELF._UseServerAsClient = true

QuickSocketClass.DESTRUCT    PROCEDURE()
lIndex LONG
 CODE
  LOOP lIndex = 1 TO RECORDS(SELF.Clients)
    GET(SELF.Clients, lIndex)
    SELF.DisconnectClient(SELF.Clients.socket)
  END
  FREE(SELF.Clients)
  DISPOSE(SELF.Clients)
  SELF.StopServer()
  SELF.DisconnectFrom()

QuickSocketClass.GetMachineName            PROCEDURE()
lMachineName CSTRING(101)
wsaDataBlock            LIKE(WSAData)
 CODE
    IF WSAStartup(s32_Version2_0, wsaDataBlock)           ! Tries to initialize it
       RETURN ''                                            ! If still an error, give up
    END
    CLEAR(lMachineName)
    IF GetHostName(lMachineName,100) <> 0
       CLEAR(lMachineName)
    END
    RETURN CLIP(lMachineName)
    
QuickSocketClass.GetMachineIpAddress       PROCEDURE()
lMachineName         CSTRING(101)
lIPAddress           CSTRING(101)
llpHostBuf           LONG
lPtr5                LONG
lStrPtr              LONG
lIPAddr              LONG
 CODE
    lMachineName = SELF.GetMachineName()
    IF LEN(CLIP(lMachineName)) > 0
       llpHostBuf = GetHostByName(ADDRESS(lMachineName))
       IF llpHostBuf <> 0
          Memcpy(ADDRESS(lPtr5),llpHostBuf+12,4)
          IF lPtr5 <> 0
             Memcpy(ADDRESS(lStrPtr),lPtr5,4)
             IF lStrPtr <> 0
                Memcpy(ADDRESS(lIPAddr),lStrPtr,4)
                lStrPtr = WS_INet_NToA(lIPAddr)
                IF lStrPtr <> 0
                   LStrCpy(lIPAddress,lStrPtr)
                END
             END
          END
       END
    END
    RETURN CLIP(lIPAddress)


QuickSocketClass.InitSubClass PROCEDURE(*WINDOW winRef)
  CODE
  SELF.WinRef &= winRef
  SELF._OldUserData = SetWindowLong(SELF.WinRef{PROP:Handle}, __GWL_USERDATA, ADDRESS(SELF))
  SELF._OldWndProc  = SELF.WinRef{PROP:WndProc}
  SELF.WinRef{PROP:WndProc} = ADDRESS(QuickSocketWindowSubClass) ! Transfer WM handling for subclassing Windows Messages
  RETURN

QuickSocketClass.KillSubClass PROCEDURE()
  CODE
  SELF.WinRef{PROP:WndProc} = SELF._OldWndProc
  __SetWindowLong(SELF.WinRef{PROP:Handle}, __GWL_USERDATA, SELF._OldUserData)
  RETURN

QuickSocketClass.WindowSubClassEvents          PROCEDURE(UNSIGNED hWnd, SIGNED wMsg, UNSIGNED wParam, LONG lParam)
sLocalReceived          STRING(1024)
nErr            LONG
 CODE
  ! wMsg    Registered Windows message
  ! wParam  Socket that originated the message
  ! lParam  Message value (FD_ACCEPT, FD_READ, or FD_CLOSE)

  IF SELF.wm = wMsg
     CASE lParam                                               ! What is the message content?
     OF FD_ACCEPT                                              ! CONNECTION REQUEST
       CLEAR(SELF.Clients)
       SELF.Clients.socket = WS_ACCEPT(wParam)
       IF SELF.Clients.socket <> SOCKET_ERROR
          ADD(SELF.Clients)
          SELF.OnClientConnect(wParam)
       END

     OF FD_READ                                                ! OTHER PARTY SENDING DATA
       CLEAR(SELF.bufferReceived)
       CLEAR(sLocalReceived)
       SELF.bytesReceived = 0
       nErr = WS_RECV(wParam, ADDRESS(sLocalReceived), SIZE(sLocalReceived), 0)
       IF nErr <> SOCKET_ERROR
          SELF.bytesReceived = nErr
          SELF.bufferReceived=''
          SELF.bufferReceived = CLIP(sLocalReceived)     ! Store the read message for further processing
          SELF.DoTakeWindowMessage(wParam, lParam)       ! Opportunity to process the message, for the programmer
          CLEAR(SELF.bufferReceived)
          SELF.bufferReceived=''
          SELF.bytesReceived = 0
       END

     OF FD_CLOSE                                               ! OTHER PARTY CLOSING CONNECTION
       SELF.OnClientClosed(wParam)                    ! Let the server handle the disconnection event
       WS_CLOSESOCKET(wParam)                                ! Disconnect only after methods, this way we can send a final message
     END
  END
  RETURN (wsCallWindowProc(SELF._OldWndProc, hWnd, wMsg, wParam, lParam))

QuickSocketClass.IsServerRunning           PROCEDURE()
 CODE
    IF SELF.serverRunning
       RETURN TRUE
    ELSE
       RETURN FALSE
    END
    
QuickSocketClass.StopServer PROCEDURE()
 CODE
  IF NOT SELF.IsServerRunning() THEN RETURN END
  
  WS_CLOSESOCKET(SELF.connectionSocket)

  SELF.KillSubClass()
  
  SELF.wm = 0
  SELF.connectionSocket = SOCKET_ERROR
  SELF.serverIP = ''
  SELF.serverPort = ''
  SELF.serverRunning = False
  
QuickSocketClass.StartServer      PROCEDURE(STRING pServerIp,LONG pPortNumber, <STRING serverName>)
W &WINDOW
 CODE    
    W &= SYSTEM{PROP:Target}
    RETURN SELF.StartServer(W,pServerIp,pPortNumber,serverName)

QuickSocketClass.StartServer PROCEDURE(*WINDOW pWinRef,STRING pServerIp,LONG pPortNumber, <STRING serverName>)
szIP                    CSTRING(32)
s                       SOCKET
inetAddr                LONG
lWM                     LONG
lEvent                  LONG
wsaDataBlock            LIKE(WSAData)
local                   LIKE(SOCKADDR_IN)

 CODE
  IF SELF.IsServerRunning() THEN RETURN 1 END                     ! Can't call more than once per object

  s = WS_SOCKET(AF_INET, SOCK_STREAM, IPPROTO_TCP)
  IF s = SOCKET_ERROR
    IF WSAGetLastError() = WSANOTINITIALISED                ! Was it that the socket layer wasn't initialised?
      IF WSAStartup(s32_Version2_0, wsaDataBlock)           ! Tries to initialize it
        RETURN 1                                            ! If still an error, give up
      END
      s = WS_SOCKET(AF_INET, SOCK_STREAM, IPPROTO_TCP)
      IF s = SOCKET_ERROR THEN RETURN 1 END                 ! Still error, give up
    END
  END

  szIP = pServerIp
  inetAddr = WS_INET_ADDR(szIP)
  IF inetAddr = INADDR_NONE                                 ! Can't accept connection requests from NONE
    RETURN 2
  END

  local.sin_family = AF_INET
  local.sin_addr.s_un.s_addr = inetAddr
  local.sin_port = ws_htons(pPortNumber)

  IF WS_BIND(s, ADDRESS(local), SIZE(local) ) = SOCKET_ERROR
    RETURN 3! Usually happen when the IP and PORT are not valid or the PORT is already open or the OS is not giving access to the PORT
  END

  IF WS_LISTEN(s, SOMAXCONN) = SOCKET_ERROR               ! Creates a listenning socket
    WS_CLOSESOCKET(s)
    RETURN 4
  END

  lEvent = BOR(lEvent, FD_ACCEPT)
  lEvent = BOR(lEvent, FD_READ)
  lEvent = BOR(lEvent, FD_CLOSE)

  lWM = WS_INBOUND + THREAD() !only one server can listen to a port per thread
  
  SELF.InitSubClass(pWinRef)  
  
  IF WSAAsyncSelect(s, SELF.winRef{PROP:Handle}, lWM, lEvent) = SOCKET_ERROR
    SELF.KillSubClass()
    RETURN 5
  END

  SELF.serverPort = pPortNumber
  SELF.serverIP = pServerIp
  SELF.connectionSocket = s
  SELF.serverRunning = TRUE
  IF OMITTED(serverName)
     SELF.clientId = SELF.serverIP
  ELSE
     SELF.clientId = serverName
  END
  
  SELF.wm = lWM
  RETURN 0

QuickSocketClass.DoTakeWindowMessage       PROCEDURE(LONG client,LONG request)
 CODE
    SELF.TakeWindowMessage(client,request)
    
QuickSocketClass.TakeWindowMessage PROCEDURE(LONG client, LONG request)
 CODE

QuickSocketClass.OnClientClosed            PROCEDURE(LONG client)
 CODE

QuickSocketClass.OnClientConnect           PROCEDURE(LONG client)
 CODE

QuickSocketClass.IsConnected               PROCEDURE()
 CODE
    IF SELF.connectionSocket<>0 AND SELF.connectionSocket<>SOCKET_ERROR AND SELF.IsServerRunning() = false
       RETURN True
    END
    RETURN False

QuickSocketClass.GetClientId               PROCEDURE(SOCKET client)
 CODE
    IF SELF.ClientExistBySocket(client)
       RETURN SELF.Clients.id
    ELSE
       RETURN ''
    END

QuickSocketClass.GetClientSocket           PROCEDURE(STRING client)
 CODE
    IF SELF.ClientExistById(client)
       RETURN SELF.Clients.socket
    ELSE
       RETURN 0
    END

QuickSocketClass.ClientExistBySocket               PROCEDURE(SOCKET client)
lIndex LONG
 CODE
    LOOP lIndex = 1 TO RECORDS(SELF.Clients)
         GET(SELF.Clients,lIndex)
         IF ERRORCODE()
            CLEAR(SELF.Clients)
            BREAK
         END
         IF SELF.Clients.socket = client
            RETURN TRUE
         END
    END
    CLEAR(SELF.Clients)
    RETURN False
  
QuickSocketClass.ClientExistById               PROCEDURE(STRING client)
lIndex LONG
 CODE
    LOOP lIndex = 1 TO RECORDS(SELF.Clients)
         GET(SELF.Clients,lIndex)
         IF ERRORCODE()
            CLEAR(SELF.Clients)
            BREAK
         END
         IF SELF.Clients.id = client
            RETURN TRUE
         END
    END
    CLEAR(SELF.Clients)
    RETURN False
    
QuickSocketClass.SendToServer        PROCEDURE(STRING strMsg)!Will send a message to the default server
 CODE
    RETURN SELF.Send(strMsg,SELF.connectionSocket)

QuickSocketClass.Send        PROCEDURE(STRING strMsg,SOCKET destination)
strSend                 STRING(1024)
s                       SOCKET
lError SIGNED
 CODE
  s = destination
  strSend = strMsg                                             ! Moves the passed in parameter to local scope
  IF WS_SEND(s, ADDRESS(strSend), LEN(CLIP(strSend) ), 0) = SOCKET_ERROR
    lError = WSAGetLastError()
    RETURN lError
  END
  RETURN 0
  
QuickSocketClass.Broadcast                 PROCEDURE(STRING strMsg)
strSend                 STRING(1024)
lIndex LONG
lError LONG
 CODE
    strSend = strMsg                                             ! Moves the passed in parameter to local scope
    LOOP lIndex = 1 TO RECORDS(SELF.Clients)
         GET(SELF.Clients, lIndex)
         IF ERRORCODE()
            BREAK
         END
         lError = SELF.Send(strMsg,SELF.Clients.socket)
         IF lError<>0
            RETURN lError
         END
    END
    RETURN 0

QuickSocketClass.ConnectTo PROCEDURE(STRING pServerIp,LONG pPortNumber)
W &WINDOW
 CODE    
    W &= SYSTEM{PROP:Target}
    RETURN SELF.ConnectTo(W, pServerIp, pPortNumber)
    
QuickSocketClass.ConnectTo   PROCEDURE(*WINDOW pWinRef, STRING pServerIp, LONG pPortNumber)
s                       SOCKET
inetAddr                ULONG
lBytesSent              LONG
szIP                    CSTRING(64)
strID                   CSTRING(64)
strHello                STRING(1)
dest                    LIKE(SOCKADDR_IN)
wsaDataBlock            LIKE(WSAData)
lEvent                  LONG
lWM                     LONG

 CODE
  SELF.connectionSocket = SOCKET_ERROR
  SELF.serverIP = ''
  s = WS_SOCKET(AF_INET, SOCK_STREAM, IPPROTO_TCP)
  IF s = SOCKET_ERROR
    IF WSAGetLastError() = WSANOTINITIALISED                ! Was it that the socket layer wasn't initialised?
      IF WSAStartup(s32_Version2_0, wsaDataBlock)         ! Tries to initialize it
        RETURN 1                                            ! If still an error, give up
      END
      s = WS_SOCKET(AF_INET, SOCK_STREAM, IPPROTO_TCP)
      IF s = SOCKET_ERROR THEN RETURN 1 END                 ! Still error, give up
    END
  END

  szIP = pServerIp
  inetAddr = WS_INET_ADDR(szIP)
  IF inetAddr = INADDR_NONE OR inetAddr = INADDR_ANY        ! Can't connect to "ANY" address
    RETURN 3
  END

  dest.sin_family = AF_INET
  dest.sin_addr.s_un.s_addr = inetAddr
  dest.sin_port = WS_HTONS(pPortNumber)

  IF WS_CONNECT(s, ADDRESS(dest), SIZE(dest) ) = SOCKET_ERROR
    RETURN 4
  END

  IF WS_SEND(s, ADDRESS(strHello), LEN(strHello), 0) = SOCKET_ERROR
    RETURN 5                                                ! Return error if not confirmed
  END

  SELF.connectionSocket = s
  SELF.serverIP = pServerIp

  lEvent = BOR(lEvent, FD_READ)
  lEvent = BOR(lEvent, FD_CLOSE)
  lWM = WS_INBOUND + THREAD()

  SELF.wm = lWM
  
  SELF.InitSubClass(pWinRef)
  
  
  IF WSAAsyncSelect(s, SELF.winRef{PROP:Handle}, lWM, lEvent) = SOCKET_ERROR
    SELF.KillSubClass()
  END

  RETURN 0

QuickSocketClass.DisconnectFrom  PROCEDURE()
 CODE
    IF SELF.connectionSocket<>0 AND SELF.connectionSocket<>SOCKET_ERROR
       WS_CLOSESOCKET(SELF.connectionSocket)
       SELF.connectionSocket = 0
       SELF.KillSubClass()
       SELF.wm = 0
    END
    
QuickSocketClass.DisconnectClient  PROCEDURE(SOCKET client)
 CODE
    SELF.Clients.socket = client
    GET(SELF.Clients, SELF.Clients.socket)
    IF NOT ERRORCODE()
       DELETE(SELF.Clients)       
    END
    WS_CLOSESOCKET(client)                                      ! It is safe to request a close on a passed socket
    
QuickSocketClass._CommandToString          PROCEDURE(LONG pCommand)
 CODE
    CASE pCommand
    OF QuickSocketCmd:Unknown            
       RETURN 'Cmd:Unknown'
    OF QuickSocketCmd:Login              
       RETURN 'Cmd:Login'
    OF QuickSocketCmd:SendMessage        
       RETURN 'Cmd:SendMessage'
    OF QuickSocketCmd:SendData           
       RETURN 'Cmd:SendData'
    OF QuickSocketCmd:SendNotify         
       RETURN 'Cmd:SendNotify'
    OF QuickSocketCmd:BroadcastNotify    
       RETURN 'Cmd:BroadcastNotify'
    OF QuickSocketCmd:BroadcastShutdown  
       RETURN 'Cmd:BroadcastShutdown'       
    OF QuickSocketCmd:NotifyCleanConnected
       RETURN 'Cmd:NotifyCleanConnected'
    OF QuickSocketCmd:NotifyBatchConnected
       RETURN 'Cmd:NotifyBatchConnected'
    OF QuickSocketCmd:NotifyConnected    
       RETURN 'Cmd:NotifyConnected'
    OF QuickSocketCmd:NotifyDisconnected 
       RETURN 'Cmd:NotifyDisconnected'
    OF QuickSocketCmd:CmdAnswer          
       RETURN 'Cmd:CmdAnswer'
    END

QuickSocketClass.SetUseServerAsClient     PROCEDURE(BYTE value)
 CODE
    IF value > 0 
       SELF._UseServerAsClient = TRUE
    ELSE
       SELF._UseServerAsClient = false
    END 
    
QuickSocketClass.GetUseServerAsClient     PROCEDURE()
 CODE
    RETURN SELF._UseServerAsClient

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
QuickAppConnectClass.CONSTRUCT                 PROCEDURE()
 CODE
    SELF.JSON &= NEW JSONDataClass
    SELF.JSON.SetClipValues(true)
 
QuickAppConnectClass.DESTRUCT                  PROCEDURE()
 CODE
    DISPOSE(SELF.JSON)

QuickAppConnectClass.ConnectTo                 PROCEDURE(STRING pServerIp,LONG pPortNumber, STRING pUserName, STRING pUserPassword)
lError LONG
 CODE
    lError = SELF.ConnectTo(pServerIp,pPortNumber)
    IF NOT lError
       SLEEP(50)
       lError = SELF.Login(pUserName, pUserPassword)
    END
    RETURN lError
 
QuickAppConnectClass.Login                     PROCEDURE(STRING pUserName, STRING pUserPassword)
lLoginGroup LIKE(QuickSocketCmdLoginGroup)
 CODE    
    SELF.JsonCmd.Cmd = QuickSocketCmd:Login

    lLoginGroup.UID = CLIP(pUserName)
    lLoginGroup.UPWD = CLIP(pUserPassword)
    SELF.JsonCmd.CmdData = SELF.JSON.ToJSON(lLoginGroup) 
    SELF.clientId = pUserName
    RETURN SELF.SEND(SELF.JSON.ToJSON(SELF.JsonCmd),SELF.connectionSocket)

QuickAppConnectClass.SendServerResponse        PROCEDURE(LONG pToClientSocket, LONG pOriginalCommand, LONG pRetVal, STRING pRetMsg)
lAnsGroup    LIKE(QuickSocketCmdAnswerGroup)
 CODE
    SELF.JsonCmd.Cmd = QuickSocketCmd:CmdAnswer
    lAnsGroup.OrgCmd = pOriginalCommand
    lAnsGroup.RetVal = pRetVal
    lAnsGroup.RetMsg = CLIP(pRetMsg)
    SELF.JsonCmd.CmdData = SELF.JSON.ToJSON(lAnsGroup)    
    RETURN SELF.SEND(SELF.JSON.ToJSON(SELF.JsonCmd) ,pToClientSocket)
 
QuickAppConnectClass.SendMessage               PROCEDURE(STRING pToUserName, STRING pMsgText)
lMsgGroup   LIKE(QuickSocketCmdMsgGroup)
lDestination SOCKET
lError LONG
 CODE
    IF SELF.IsServerRunning()
       lDestination = SELF.GetClientSocket(pToUserName)
       lMsgGroup.MsgFrom = SELF.clientId
    ELSE
       lDestination = SELF.connectionSocket
    END
    IF lDestination = SOCKET_ERROR OR lDestination = 0
       RETURN SOCKET_ERROR
    END

    lMsgGroup.MsgTo    = CLIP(pToUserName)
    lMsgGroup.MsgText  = CLIP(pMsgText)
    
    SELF.JsonCmd.Cmd = QuickSocketCmd:SendMessage
    SELF.JsonCmd.CmdData = SELF.JSON.ToJSON(lMsgGroup)
    lError = SELF.SEND(SELF.JSON.ToJSON(SELF.JsonCmd),lDestination)
    IF NOT lError
       SLEEP(100)
    END
    RETURN lError
    
QuickAppConnectClass.SendNotify                PROCEDURE(STRING pToUserName, LONG pNotificationCode, *GROUP pMsgContent)
 CODE
    RETURN SELF.SendNotify(pToUserName, pNotificationCode,SELF.JSON.ToJSON(pMsgContent))
    
QuickAppConnectClass.SendNotify                PROCEDURE(STRING pToUserName, LONG pNotificationCode, STRING pMsgText)
lNotifyGroup LIKE(QuickSocketCmdNotifyGroup)
lDestination SOCKET
lError       LONG
 CODE
    IF SELF.IsServerRunning()
       lDestination = SELF.GetClientSocket(pToUserName)
    ELSE
       lDestination = SELF.connectionSocket
    END
    IF lDestination = SOCKET_ERROR OR lDestination = 0
       RETURN SOCKET_ERROR
    END
    
    lNotifyGroup.MsgTo     = CLIP(pToUserName)
    lNotifyGroup.MsgNotify = pNotificationCode
    lNotifyGroup.MsgText   = CLIP(pMsgText)

    SELF.JsonCmd.Cmd = QuickSocketCmd:SendNotify
    SELF.JsonCmd.CmdData = SELF.JSON.ToJSON(lNotifyGroup)

    lError = SELF.SEND(SELF.JSON.ToJSON(SELF.JsonCmd),lDestination)
    IF NOT lError
       SLEEP(100)
    END
    RETURN lError

QuickAppConnectClass.FromJSON                  PROCEDURE(*STRING pJSONString,*GROUP pJSONObject)
 CODE
    SELF.JSON.FromJSON(pJSONString, pJSONObject)
      
QuickAppConnectClass.BroadcastNotify           PROCEDURE(LONG pNotificationCode, *GROUP pMsgContent)
 CODE
    RETURN SELF.BroadcastNotify(pNotificationCode,SELF.JSON.ToJSON(pMsgContent))

QuickAppConnectClass.BroadcastNotify           PROCEDURE(LONG pNotificationCode, STRING pMsgText)
lBroadGroup   LIKE(QuickSocketCmdBroadcastGroup)
 CODE
    IF SELF.IsServerRunning()
       lBroadGroup.MsgFrom = SELF.clientId
    END
    lBroadGroup.MsgNotify= pNotificationCode
    lBroadGroup.MsgText  = CLIP(pMsgText)

    SELF.JsonCmd.Cmd = QuickSocketCmd:BroadcastNotify
    SELF.JsonCmd.CmdData = SELF.JSON.ToJSON(lBroadGroup)

    IF SELF.IsServerRunning()
       RETURN SELF.Broadcast(SELF.JSON.ToJSON(SELF.JsonCmd))
    ELSE
       RETURN SELF.SEND(SELF.JSON.ToJSON(SELF.JsonCmd),SELF.connectionSocket)
    END
    
QuickAppConnectClass.BroadcastShutdown         PROCEDURE(LONG pNotificationCode, STRING pMsgText)
lBroadGroup   LIKE(QuickSocketCmdBroadcastGroup)
 CODE
    IF SELF.IsServerRunning()
       lBroadGroup.MsgFrom = SELF.clientId
    END
    lBroadGroup.MsgNotify= pNotificationCode
    lBroadGroup.MsgText  = CLIP(pMsgText)

    SELF.JsonCmd.Cmd = QuickSocketCmd:BroadcastShutdown
    SELF.JsonCmd.CmdData = SELF.JSON.ToJSON(lBroadGroup)

    IF SELF.IsServerRunning()
       RETURN SELF.Broadcast(SELF.JSON.ToJSON(SELF.JsonCmd))
    ELSE
       RETURN SELF.SEND(SELF.JSON.ToJSON(SELF.JsonCmd),SELF.connectionSocket)
    END
    
QuickAppConnectClass.RefreshNewConnectedUsers   PROCEDURE(SOCKET newClientSocket)
lIndex LONG
lError LONG
lMessage STRING(200)
lUsers   QUEUE(QuickSocketUsersQueue)
         END
 CODE
    !First send its name to everyone except it self or any one that is not fully loged in (Clients.id='')
    SELF.JsonCmd.Cmd = QuickSocketCmd:NotifyConnected
    SELF.JsonCmd.CmdData = SELF.GetClientId(newClientSocket)
    lMessage = SELF.JSON.ToJSON(SELF.JsonCmd)
    LOOP lIndex = 1 TO RECORDS(SELF.Clients)
         GET(SELF.Clients, lIndex)
         IF ERRORCODE()
            BREAK
         END
         !Do not send the connection if it is itself or 
         IF newClientSocket = SELF.Clients.socket             
            CYCLE
         END         
         IF SELF.Clients.id=''
            CYCLE
         END
         lError = SELF.Send(lMessage,SELF.Clients.socket)
         IF lError<>0
            !RETURN lError
         END
    END
       
    !Send all the others names to it self
    SELF.JsonCmd.Cmd = QuickSocketCmd:NotifyConnected
    !Send server ID to be a client too
    IF SELF.IsServerRunning()
       IF SELF.GetUseServerAsClient()
          IF SELF.clientId<>''
             lUsers.UID = SELF.clientId
             ADD(lUsers)
             !SELF.JsonCmd.CmdData = 
             !lError = SELF.Send(SELF.JSON.ToJSON(SELF.JsonCmd),newClientSocket)
             !IF lError<>0
             !   RETURN lError
             !END
          END
       END
    END

    LOOP lIndex = 1 TO RECORDS(SELF.Clients)
         GET(SELF.Clients, lIndex)
         IF ERRORCODE()
            BREAK
         END
         IF newClientSocket = SELF.Clients.socket OR SELF.Clients.id=''
            CYCLE
         END         
         lUsers.UID = SELF.Clients.id
         ADD(lUsers)       
    END
    !TBD: slice the records to not max the SELF.JsonCmd.CmdData string size
    IF RECORDS(lUsers)
       SELF.JsonCmd.Cmd = QuickSocketCmd:NotifyBatchConnected
       SELF.JsonCmd.CmdData = SELF.JSON.ToJSON(lUsers)
       IF SELF.Send(SELF.JSON.ToJSON(SELF.JsonCmd),newClientSocket)
       END
    END
    RETURN 0
 
QuickAppConnectClass.DoTakeWindowMessage       PROCEDURE(LONG client,LONG request)
lLoginGroup  LIKE(QuickSocketCmdLoginGroup)
lMsgGroup    LIKE(QuickSocketCmdMsgGroup)
lBroadGroup  LIKE(QuickSocketCmdBroadcastGroup)
lNotifyGroup LIKE(QuickSocketCmdNotifyGroup)
lAnsGroup    LIKE(QuickSocketCmdAnswerGroup)
lSocket      SOCKET
lCancelled   BYTE
lError       LONG
lIndex       LONG
lUsers       QUEUE(QuickSocketUsersQueue)
 END
 CODE    
    IF CLIP(SELF.bufferReceived)=''
       RETURN
    END
    CLEAR(SELF.JsonCmd)
    SELF.JSON.FromJSON(SELF.bufferReceived,SELF.JsonCmd)
    IF SELF.IsServerRunning()
       !Server side
       CASE SELF.JsonCmd.Cmd
       OF QuickSocketCmd:Login          
          SELF.JSON.FromJSON(SELF.JsonCmd.CmdData,lLoginGroup)          
          IF SELF.ClientExistBySocket(client) AND SELF.Clients.id = ''
             !Must exist because was added at the connection event
             !And the ID must be null. not allowed to exist two times now or change names
             SELF.Clients.socket = client
             SELF.Clients.id = lLoginGroup.UID
             PUT(SELF.Clients)
             
             lCancelled = false
             SELF.Event_LoginOnServer(lLoginGroup.UID, lLoginGroup.UPWD, lCancelled)
             IF lCancelled
                SELF.SendServerResponse(client,QuickSocketCmd:Login,1,'Login fail. Disconnected from server')
                SELF.DisconnectClient(client)
             ELSE
                SELF.SendServerResponse(client,QuickSocketCmd:Login,0,'Login OK')
                SLEEP(500)!The just connected client may not be ready yet
                SELF.RefreshNewConnectedUsers(client)
                IF SELF.GetUseServerAsClient()
                   SELF.Event_NotifyConnected(CLIP(lLoginGroup.UID))
                END
             END
          END
       OF QuickSocketCmd:SendMessage        
          SELF.JSON.FromJSON(SELF.JsonCmd.CmdData,lMsgGroup)
          IF CLIP(lMsgGroup.MsgTo) <> '' AND lMsgGroup.MsgTo <> SELF.clientId AND lMsgGroup.MsgTo <> SELF.serverIP
             lSocket = SELF.GetClientSocket(lMsgGroup.MsgTo)             
             IF lSocket
                lMsgGroup.MsgFrom = SELF.GetClientId(client)
                SELF.JsonCmd.CmdData = SELF.JSON.ToJSON(lMsgGroup)
                lError = SELF.Send(SELF.JSON.ToJSON(SELF.JsonCmd), lSocket)
                IF lError
                   MESSAGE('1Error number:('&lError&')')
                END
             ELSE
                MESSAGE('Socket didn''t exist.|From:'&lMsgGroup.MsgFrom&'|To:'&lMsgGroup.MsgTo&'|'&SELF.JsonCmd.CmdData,'Send Error')
             END
          ELSE
             IF SELF.GetUseServerAsClient()
                SELF.Event_Message(SELF.GetClientId(client), lMsgGroup.MsgTo, lMsgGroup.MsgText)      
             END
          END
       OF QuickSocketCmd:SendNotify         
          SELF.JSON.FromJSON(SELF.JsonCmd.CmdData,lNotifyGroup)
          IF CLIP(lNotifyGroup.MsgTo) <> '' AND lNotifyGroup.MsgTo <> SELF.clientId AND lNotifyGroup.MsgTo <> SELF.serverIP
             lSocket = SELF.GetClientSocket(lNotifyGroup.MsgTo)             
             IF lSocket
                lNotifyGroup.MsgFrom = SELF.GetClientId(client)
                SELF.JsonCmd.CmdData = SELF.JSON.ToJSON(lNotifyGroup)
                lError = SELF.Send(SELF.JSON.ToJSON(SELF.JsonCmd), lSocket)
                IF lError
                   MESSAGE('2Error number:('&lError&')')
                END
             ELSE
                MESSAGE('Socket didn''t exist.|From:'&lNotifyGroup.MsgFrom&'|To:'&lNotifyGroup.MsgTo&'|'&SELF.JsonCmd.CmdData,'Notification Error')
             END
          ELSE
             IF SELF.GetUseServerAsClient()
                SELF.Event_Notification(SELF.GetClientId(client), lNotifyGroup.MsgTo, lNotifyGroup.MsgNotify, lNotifyGroup.MsgText)      
             END
          END
       OF QuickSocketCmd:BroadcastNotify    
          SELF.JSON.FromJSON(SELF.JsonCmd.CmdData,lBroadGroup)
          lBroadGroup.MsgFrom = SELF.GetClientId(client)
          SELF.JsonCmd.Cmd = QuickSocketCmd:BroadcastNotify
          SELF.JsonCmd.CmdData = SELF.JSON.ToJSON(lBroadGroup)    
          SELF.Broadcast(SELF.JSON.ToJSON(SELF.JsonCmd))
          IF SELF.GetUseServerAsClient()
             SELF.Event_BroadcastNotification(lBroadGroup.MsgFrom, lBroadGroup.MsgNotify, lBroadGroup.MsgText)          
          END
       OF QuickSocketCmd:BroadcastShutdown
          SELF.JSON.FromJSON(SELF.JsonCmd.CmdData,lBroadGroup)
          lBroadGroup.MsgFrom = SELF.GetClientId(client)
          SELF.JsonCmd.Cmd = QuickSocketCmd:BroadcastShutdown
          SELF.JsonCmd.CmdData = SELF.JSON.ToJSON(lBroadGroup)    
          SELF.Broadcast(SELF.JSON.ToJSON(SELF.JsonCmd))
          IF SELF.GetUseServerAsClient()
             SELF.Event_BroadcastShutdown(lBroadGroup.MsgFrom, lBroadGroup.MsgNotify, lBroadGroup.MsgText)
          END
       OF QuickSocketCmd:NotifyConnected    
          IF SELF.GetUseServerAsClient()
             SELF.Event_NotifyConnected(CLIP(SELF.JsonCmd.CmdData))
          END
       OF QuickSocketCmd:NotifyDisconnected
          IF SELF.GetUseServerAsClient()
             SELF.Event_NotifyDisconnected(CLIP(SELF.JsonCmd.CmdData))
          END
       ELSE
       END             
    ELSE
       !Client side       
       CASE SELF.JsonCmd.Cmd
       OF QuickSocketCmd:SendMessage        
          SELF.JSON.FromJSON(SELF.JsonCmd.CmdData,lMsgGroup)
          SELF.Event_Message(lMsgGroup.MsgFrom, lMsgGroup.MsgTo, lMsgGroup.MsgText)      
       OF QuickSocketCmd:SendNotify         
          SELF.JSON.FromJSON(SELF.JsonCmd.CmdData,lNotifyGroup)
          SELF.Event_Notification(lNotifyGroup.MsgFrom, lNotifyGroup.MsgTo, lNotifyGroup.MsgNotify, lNotifyGroup.MsgText)      
       OF QuickSocketCmd:BroadcastNotify    
          SELF.JSON.FromJSON(SELF.JsonCmd.CmdData,lBroadGroup)
          SELF.Event_BroadcastNotification(lBroadGroup.MsgFrom, lBroadGroup.MsgNotify, lBroadGroup.MsgText)
       OF QuickSocketCmd:BroadcastShutdown  
          SELF.JSON.FromJSON(SELF.JsonCmd.CmdData,lBroadGroup)
          SELF.Event_BroadcastShutdown(lBroadGroup.MsgFrom, lBroadGroup.MsgNotify, lBroadGroup.MsgText)
       OF QuickSocketCmd:NotifyBatchConnected
          SELF.JSON.FromJSON(SELF.JsonCmd.CmdData,lUsers)
          LOOP lIndex = 1 to RECORDS(lUsers)
               GET(lUsers, lIndex)
               IF NOT ERRORCODE()
                  SELF.Event_NotifyConnected(CLIP(lUsers.UID))
               END
          END
       OF QuickSocketCmd:NotifyCleanConnected
          SELF.Event_NotifyCleanConnected()
       OF QuickSocketCmd:NotifyConnected    
          SELF.Event_NotifyConnected(CLIP(SELF.JsonCmd.CmdData))
       OF QuickSocketCmd:NotifyDisconnected 
          SELF.Event_NotifyDisconnected(CLIP(SELF.JsonCmd.CmdData))
       OF QuickSocketCmd:CmdAnswer
          SELF.JSON.FromJSON(SELF.JsonCmd.CmdData,lAnsGroup)
          SELF.Event_ServerResponse(lAnsGroup.OrgCmd, lAnsGroup.RetVal, lAnsGroup.RetMsg)
       ELSE
       END
    END
    SELF.TakeWindowMessage(client,request)
    SELF.bufferReceived = ''

QuickAppConnectClass.Event_ServerResponse      PROCEDURE(LONG pOriginalCommand, LONG pRetVal, STRING pRetMsg)
 CODE
    !This is a Virtual method that expect to be derived in the client to add
    !functionality to the response in case of each action (Events)
    !Will be call by the class, 
    !the user can derive the method to use the parameters   
QuickAppConnectClass.Event_LoginOnServer        PROCEDURE(STRING pUserName, STRING pUserPassword, *BYTE pCancelOperation)
 CODE
    !to cancel the login operation just set the pCancelOperation=true
    
QuickAppConnectClass.Event_Message             PROCEDURE(STRING pFromUserName, STRING pToUserName, STRING pMsgText)
 CODE
QuickAppConnectClass.Event_Notification        PROCEDURE(STRING pFromUserName, STRING pToUserName, LONG pNotificationCode, STRING pMsgText)
 CODE
QuickAppConnectClass.Event_BroadcastNotification PROCEDURE(STRING pFromUserName, LONG pNotificationCode, STRING pMsgText)
 CODE
QuickAppConnectClass.Event_BroadcastShutdown   PROCEDURE(STRING pFromUserName, LONG pNotificationCode, STRING pMsgText)
 CODE
QuickAppConnectClass.Event_NotifyDisconnected  PROCEDURE(STRING pUserName)
 CODE
QuickAppConnectClass.Event_NotifyConnected     PROCEDURE(STRING pUserName)
 CODE
QuickAppConnectClass.Event_NotifyCleanConnected PROCEDURE()
 CODE
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

QuickSocketWindowSubClass          PROCEDURE(UNSIGNED hWnd, SIGNED wMsg, UNSIGNED wParam, LONG lParam)
lQuickSocketSubClassWindow &QuickSocketClass,AUTO
  CODE
  lQuickSocketSubClassWindow &= __GetWindowLong(hWnd, __GWL_USERDATA)
? ASSERT(NOT lQuickSocketSubClassWindow &= null)
  RETURN lQuickSocketSubClassWindow.WindowSubClassEvents(hWnd, wMsg, wParam, lParam)

