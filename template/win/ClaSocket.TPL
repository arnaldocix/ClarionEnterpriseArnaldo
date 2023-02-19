#TEMPLATE(SVQuickAppConnectSupport, 'Socket Communication Support'),FAMILY('ABC','CW20')
#!
#EXTENSION(SVQuickAppConnectGlobal, 'Socket Communication Support - Global'),HLP('~TCP_Template_Support.htm#SockGloExtension'),APPLICATION
#DISPLAY
#DISPLAY(' Socket Communication Support'),AT(3,,187), PROP(PROP:FontColor, 0FFFFFFH), PROP(PROP:Color,0808080H)
#DISPLAY
#PROMPT('Disable support for Socket Communications?',CHECK),%GlobalDisable,DEFAULT(%False),AT(10)
#!
#AT(%CustomGlobalDeclarations),WHERE(NOT %GlobalDisable AND %GlobalUseABCClasess)
  #ADD(%CustomGlobalDeclarationIncludes,'QuickSocket.INC' )
#ENDAT
#!
#EXTENSION(SVQuickAppConnect,'Enable Socket Communication Support'),HLP('~TCP_Template_Support.htm#SockProcExtension'),PROCEDURE,REQ(SVQuickAppConnectGlobal),DESCRIPTION('Socket Communication Support')
#PREPARE
  #CALL(%ReadABCFiles)
  #CALL(%SetClassDefaults, 'Default', 'QuickAppConnect', 'QuickAppConnectClass')
  #SET(%QuickAppConnectObjectName,%ThisObjectName)
#ENDPREPARE
    #BOXED('Default QuickAppConnectClass prompts'),HIDE,AT(0,0)
      #INSERT(%OOPHiddenPrompts)
      #PROMPT('Local Ob&ject Name:',@S255),%QuickAppConnectObjectName
      #PROMPT('Local Disable Socket support?',CHECK),%LocalEnable,DEFAULT(%True),AT(10)
    #ENDBOXED
  #SHEET
    #TAB('General'),HLP('~TCP_Template_Support.htm#SockProcExtension')
      #BOXED,SECTION
        #BOXED,WHERE(%GlobalDisable OR NOT %GlobalUseABCClasess),AT(0,0)
          #DISPLAY('This option is disabled on the Global')
          #DISPLAY('level and is not available.')
        #ENDBOXED
        #BOXED,WHERE(NOT %GlobalDisable AND %GlobalUseABCClasess),AT(0,0)
          #PROMPT('Disable support for Sockets?',CHECK),%PromptLocalDisable,DEFAULT(%False),AT(10)
          #BOXED,WHERE(NOT %PromptLocalDisable)
            #PROMPT('Implement Server?',CHECK),%SupportServer,DEFAULT(%True),AT(10)
            #PROMPT('Implement Client?',CHECK),%SupportClient,DEFAULT(%True),AT(10)
              #SHEET
                #TAB('Server')
                    #ENABLE(%SupportServer)
                      #PROMPT('Server IP:',FIELD),%LocalServerIP,REQ
                      #PROMPT('Server Port:',FIELD),%LocalServerPort,REQ
                      #PROMPT('Server Name:',FIELD),%LocalServerName
                      #PROMPT('Get IP and Machine name at runtime?',CHECK),%InitializeServerVariables,DEFAULT(%True),AT(10)
                    #ENDENABLE
                #ENDTAB
                #TAB('Client')
                    #DISPLAY('Server to connect to:')
                    #ENABLE(%SupportClient)
                        #PROMPT('Use the Server settings',CHECK),%LocalServerClientAsServer,DEFAULT(%True),AT(10)
                        #ENABLE(NOT %LocalServerClientAsServer)
                            #PROMPT('Server IP:',FIELD),%LocalServerIPCli,REQ
                            #PROMPT('Server Port:',FIELD),%LocalServerPortCli,REQ
                            #PROMPT('Server Name:',FIELD),%LocalServerNameCli
                        #ENDENABLE
                        #DISPLAY
                        #PROMPT('Client Name:',FIELD),%LocalClientName,REQ
                        #PROMPT('Server Password:',OPTFIELD),%LocalClientPwd
                    #ENDENABLE
                #ENDTAB
              #ENDSHEET
              #BOXED('Error Managment')
                 #DISPLAY('Specify variables for connection or action fails')
                 #PROMPT('Variable to store error code:',FIELD),%ErrorVar,REQ             
                 #DISPLAY('Specify Code to execute on Actions:')
                 #PROMPT('On Action Failed:',EDIT),%ExecuteCodeError,WHENACCEPTED(%SVExpresionEditor(%ExecuteCodeError)),DEFAULT('MESSAGE(''Error:''&LOC:ErrorCode,''AppConnect Error'')')
                 #PROMPT('On Action Succeeds:',EDIT),%ExecuteCodeSucceeded,WHENACCEPTED(%SVExpresionEditor(%ExecuteCodeSucceeded))
                 #PROMPT('When not connected:',EDIT),%ExecuteCodeNotConnected,WHENACCEPTED(%SVExpresionEditor(%ExecuteCodeNotConnected)),DEFAULT('MESSAGE(''The Client is not connected'',''AppConnect Error'')')
                 #DISPLAY('The code can be disabled locally at each routine.')
                 #DISPLAY('Code can also be added on each routine''s embeds.')
              #ENDBOXED
          #ENDBOXED
        #ENDBOXED
      #ENDBOXED
    #ENDTAB
    #TAB('Classes')
      #WITH(%ClassItem,'Default')
        #INSERT(%ClassPrompts)
      #ENDWITH
    #ENDTAB
  #ENDSHEET
#!
#ATSTART
  #SET(%LocalEnable,NOT ((%GlobalDisable OR NOT %GlobalUseABCClasess) OR %PromptLocalDisable))
  #IF(%LocalEnable)
    #CALL(%ReadABCFiles)
    #CALL(%SetClassDefaults, 'Default', 'QuickAppConnect', 'QuickAppConnectClass')
    #SET(%QuickAppConnectObjectName,%ThisObjectName)
  #ENDIF
  #IF(%SupportServer AND %SupportClient AND %LocalServerClientAsServer)
    #SET(%LocalServerIPCli,%LocalServerIP)
    #SET(%LocalServerPortCli,%LocalServerPort)
    #SET(%LocalServerNameCli,%LocalServerName)
  #ENDIF 
#ENDAT
#!
#AT(%GatherObjects),WHERE(%LocalEnable)
#CALL(%AddObjectList, 'Default')
#ENDAT
#!
#AT(%LocalProcedures),WHERE(%LocalEnable)
  #CALL(%GenerateVirtuals(ABC), 'Default', 'Local Objects|Abc Objects|QuickAppConnect|' & %QuickAppConnectObjectName, '%QuickAppConnectVirtuals(SVQuickAppConnectSupport)')
#ENDAT
#!
#AT(%LocalDataClasses),WHERE(%LocalEnable)
#INSERT(%GenerateClass, 'Default')
#ENDAT
#!
#AT(%QuickAppConnectMethodCodeSection,%ActiveTemplateInstance),PRIORITY(5000),DESCRIPTION('Parent Call'),WHERE(%LocalEnable AND %ParentCallValid())
  #CALL(%GenerateParentCall(ABC))
#ENDAT
#!
#AT(%BeforeWindowOpening),DESCRIPTION('AppConnect Server - InitializeServerInfo'),WHERE(%LocalEnable AND %SupportServer AND %InitializeServerVariables)
#IF(%LocalEnable AND %SupportServer AND %InitializeServerVariables)
Do %QuickAppConnectObjectName_InitializeServerInfo
#ENDIF
#ENDAT
#!
#AT(%ProcedureRoutines),PRIORITY(5000),WHERE(%LocalEnable AND %SupportServer)
#EMBED(%SVQuickAppConnectServerInitializeServerInfo,'Server Routines'),%ActiveTemplateInstance,TREE('Procedure Routines|'&%QuickAppConnectObjectName&'|Server Routines|InitializeServerInfo')
#EMBED(%SVQuickAppConnectServerStartServer,'Server Routines'),%ActiveTemplateInstance,TREE('Procedure Routines|'&%QuickAppConnectObjectName&'|Server Routines|StartServer|Before Routine Declaration{{priority(1)}')
#EMBED(%SVQuickAppConnectServerStopServer,'Server Routines'),%ActiveTemplateInstance,TREE('Procedure Routines|'&%QuickAppConnectObjectName&'|Server Routines|StopServer|Before Routine Declaration{{priority(1)}')
#ENDAT
#AT(%SVQuickAppConnectServerInitializeServerInfo,%ActiveTemplateInstance),PRIORITY(5000),DESCRIPTION('Code'),WHERE(%LocalEnable AND %SupportServer AND %InitializeServerVariables)
%QuickAppConnectObjectName_InitializeServerInfo ROUTINE
  IF CLIP(%LocalServerIP)=''
     %LocalServerName = %QuickAppConnectObjectName.GetMachineName()
     %LocalServerIP   = %QuickAppConnectObjectName.GetMachineIpAddress()
     %LocalServerPort = 8080
  END
#ENDAT
#AT(%SVQuickAppConnectServerStartServer,%ActiveTemplateInstance),PRIORITY(5000),WHERE(%LocalEnable AND %SupportServer)
%QuickAppConnectObjectName_StartServer          ROUTINE
#EMBED(%SVQuickAppConnectServerStartServerData,'Server Routines'),%ActiveTemplateInstance,TREE('Procedure Routines|'&%QuickAppConnectObjectName&'|Server Routines|StartServer|Before Executing Code{{priority(2)}'),LABEL,DATA
 %ErrorVar = 0
 IF %QuickAppConnectObjectName.IsServerRunning()= false
    %ErrorVar = %QuickAppConnectObjectName.StartServer(%LocalServerIP,%LocalServerPort,%LocalServerName)
    IF %ErrorVar
       #EMBED(%SVQuickAppConnectServerStartServerError,'Server Routines'),%ActiveTemplateInstance,TREE('Procedure Routines|'&%QuickAppConnectObjectName&'|Server Routines|StartServer|If Error Code Returned - Start Server Fail{{priority(3)}')
    ELSE
       #EMBED(%SVQuickAppConnectServerStartServerOk,'Server Routines'),%ActiveTemplateInstance,TREE('Procedure Routines|'&%QuickAppConnectObjectName&'|Server Routines|StartServer|Server Start Succeeded{{priority(4)}')
    END
 ELSE
    #EMBED(%SVQuickAppConnectServerStartServerAlready,'Server Routines'),%ActiveTemplateInstance,TREE('Procedure Routines|'&%QuickAppConnectObjectName&'|Server Routines|StartServer|It was already connected{{priority(5)}')
 END
#ENDAT
#AT(%SVQuickAppConnectServerStopServer,%ActiveTemplateInstance),PRIORITY(5000),WHERE(%LocalEnable AND %SupportServer)
%QuickAppConnectObjectName_StopServer           ROUTINE
#EMBED(%SVQuickAppConnectServerStopServerData,'Server Routines'),%ActiveTemplateInstance,TREE('Procedure Routines|'&%QuickAppConnectObjectName&'|Server Routines|StopServer|Before Executing Code{{priority(2)}'),LABEL,DATA
    IF %QuickAppConnectObjectName.IsServerRunning()
       %QuickAppConnectObjectName.StopServer()
       #EMBED(%SVQuickAppConnectServerStopServerAfter,'Server Routines'),%ActiveTemplateInstance,TREE('Procedure Routines|'&%QuickAppConnectObjectName&'|Server Routines|StopServer|After Stop Server Succeeded{{priority(3)}')
    ELSE
       #EMBED(%SVQuickAppConnectServerStopServerNot,'Server Routines'),%ActiveTemplateInstance,TREE('Procedure Routines|'&%QuickAppConnectObjectName&'|Server Routines|StopServer|Server was not Running{{priority(4)}')
    END
#ENDAT
#!
#!
#!
#AT(%ProcedureRoutines),PRIORITY(5100),WHERE(%LocalEnable AND %SupportClient)
#EMBED(%SVQuickAppConnectClientConnect,'Client Routines'),%ActiveTemplateInstance,TREE('Procedure Routines|'&%QuickAppConnectObjectName&'|Client Routines|Connection|ConnectToServer|Before Routine Declaration{{priority(1)}')
#EMBED(%SVQuickAppConnectClientDisconnect,'Client Routines'),%ActiveTemplateInstance,TREE('Procedure Routines|'&%QuickAppConnectObjectName&'|Client Routines|Connection|DisconnectFromServer|Before Routine Declaration{{priority(1)}')
#ENDAT
#AT(%SVQuickAppConnectClientConnect,%ActiveTemplateInstance),PRIORITY(5000),WHERE(%LocalEnable AND %SupportClient)
%QuickAppConnectObjectName_ConnectToServer      ROUTINE
#EMBED(%SVQuickAppConnectClientConnectData,'Client Routines'),%ActiveTemplateInstance,TREE('Procedure Routines|'&%QuickAppConnectObjectName&'|Client Routines|Connection|ConnectToServer|Before Executing Code{{priority(2)}'),LABEL,DATA
    %ErrorVar = 0
    IF %QuickAppConnectObjectName.IsServerRunning() = FALSE
       %ErrorVar = %QuickAppConnectObjectName.ConnectTo(%LocalServerIPCli ,%LocalServerPortCli)
       IF %ErrorVar
          #EMBED(%SVQuickAppConnectClientConnectFail,'ClientRoutines'),%ActiveTemplateInstance,TREE('Procedure Routines|'&%QuickAppConnectObjectName&'|Client Routines|Connection|ConnectToServer|Connection To Server Fail{{priority(6)}')
       ELSE
          #EMBED(%SVQuickAppConnectClientConnectOk,'ClientRoutines'),%ActiveTemplateInstance,TREE('Procedure Routines|'&%QuickAppConnectObjectName&'|Client Routines|Connection|ConnectToServer|After Connect To Server{{priority(3)}')
          %ErrorVar = %QuickAppConnectObjectName.Login(%LocalClientName,%LocalClientPwd)
          IF %ErrorVar
             #EMBED(%SVQuickAppConnectClientConnectLoginFail,'ClientRoutines'),%ActiveTemplateInstance,TREE('Procedure Routines|'&%QuickAppConnectObjectName&'|Client Routines|Connection|ConnectToServer|Login Fail{{priority(5)}')
          ELSE
             #EMBED(%SVQuickAppConnectClientConnectLoginOk,'ClientRoutines'),%ActiveTemplateInstance,TREE('Procedure Routines|'&%QuickAppConnectObjectName&'|Client Routines|Connection|ConnectToServer|Login Succeed{{priority(4)}')
          END
       END       
    ELSE
       #EMBED(%SVQuickAppConnectClientConnectIServer,'Client Routines'),%ActiveTemplateInstance,TREE('Procedure Routines|'&%QuickAppConnectObjectName&'|Client Routines|Connection|ConnectToServer|Connection Is a Server{{priority(7)}')
    END
#ENDAT
#!
#AT(%SVQuickAppConnectClientDisconnect,%ActiveTemplateInstance),PRIORITY(5100),WHERE(%LocalEnable AND %SupportClient)
%QuickAppConnectObjectName_DisconnectFromServer ROUTINE
#EMBED(%SVQuickAppConnectClientDisconnectData,'Client Routines'),%ActiveTemplateInstance,TREE('Procedure Routines|'&%QuickAppConnectObjectName&'|Client Routines|Connection|DisconnectFromServer|Before Executing Code{{priority(2)}'),LABEL,DATA
    IF %QuickAppConnectObjectName.IsServerRunning() = FALSE
       %QuickAppConnectObjectName.DisconnectFrom()
       #EMBED(%SVQuickAppConnectClientDisconnectOk,'ClientRoutines'),%ActiveTemplateInstance,TREE('Procedure Routines|'&%QuickAppConnectObjectName&'|Client Routines|Connection|DisconnectFromServer|After Disconnect From Server{{priority(3)}')
    ELSE
       #EMBED(%SVQuickAppConnectClientDisconnectIServer,'Client Routines'),%ActiveTemplateInstance,TREE('Procedure Routines|'&%QuickAppConnectObjectName&'|Client Routines|Connection|DisconnectFromServer|Connection Is a Server{{priority(4)}')
    END
#ENDAT
#AT(%QuickAppConnectMethodCodeSection,%ActiveTemplateInstance,'Event_LoginOnServer','(STRING pUserName,STRING pUserPassword,*BYTE pCancelOperation)'),PRIORITY(5001)
#$    !Verify that the pUserName and pUserPassword are correct
#$    !To cancel the login set the parameter pCancelOperation = true 
#$    
#$    !Example:
#$    
#$    !IF UPPER(pUserName) = 'GUEST' !Do not allow a GUEST user
#$    !   pCancelOperation = true
#$    !END
#ENDAT
#!--------------------------------------------------------------------------
#CODE(SVQuickAppConnectClient,'Execute Socket Action (Client Connection)'),REQ(SVQuickAppConnect),DESCRIPTION('Execute Socket Action - '&%ConnectionAction),HLP('~TCP_Template_Support.htm#SockConDiscon')
#ENABLE(%LocalEnable AND %SupportClient)
#PROMPT('Action:',DROP('Connect to Server[Connect]|Disconnect from Server[Disconnect]')),%ConnectionAction,DEFAULT('Connect')
#ENDENABLE
#IF(%LocalEnable AND %SupportClient)
  #IF(%ConnectionAction = 'Connect')
Do %QuickAppConnectObjectName_ConnectToServer
  #ELSE
Do %QuickAppConnectObjectName_DisconnectFromServer
  #ENDIF
#ENDIF
#!--------------------------------------------------------------------------
#CODE(SVQuickAppConnectServer,'Execute Socket Action (Server)'),REQ(SVQuickAppConnect),DESCRIPTION('Execute Socket Action - Server '&%ConnectionAction),HLP('~TCP_Template_Support.htm#SockServerAction')
#ENABLE(%LocalEnable AND %SupportServer)
#PROMPT('Action:',DROP('Start Server[Start]|Stop Server[Stop]|Initialize IP and Name[Initialize Information]')),%ConnectionAction,DEFAULT('Start')
#ENDENABLE
#IF(%LocalEnable AND %SupportServer)
  #CASE(%ConnectionAction)
  #OF('Start')
Do %QuickAppConnectObjectName_StartServer
  #OF('Stop')
Do %QuickAppConnectObjectName_StopServer
  #OF('InitializeInformation')
Do %QuickAppConnectObjectName_InitializeServerInfo
  #ENDCASE
#ENDIF
#!--------------------------------------------------------------------------
#CODE(SVQuickAppConnectClientAction,'Execute Socket Action (Client)'),REQ(SVQuickAppConnect),DESCRIPTION('Execute Socket Action - '&%ChatAction&' ('&%ActiveTemplateInstance&')'),HLP('~TCP_Template_Support.htm#SockSendMsg')
#!
#ENABLE(%LocalEnable AND (%SupportServer OR %SupportClient))
#PROMPT('Action:',DROP('Send Message[Message]|Send Notification[Notify]|Broadcast Notification[BroadcastNotification]|Broadcast Shutdown[Shutdown]')),%ChatAction,DEFAULT('Message')
#BOXED,SECTION
  #BOXED,WHERE(%ChatAction = 'Message'),AT(0,0)
    #DISPLAY('The message is sent only to selected recipient')
    #DISPLAY('')
    #DISPLAY('The client executes the "Event_Message" method')
    #DISPLAY('with the content of the message.')
    #DISPLAY('')
    #DISPLAY('')    
  #ENDBOXED
  #BOXED,WHERE(%ChatAction = 'Notify'),AT(0,0)
    #DISPLAY('The message is sent only to selected recipient')
    #DISPLAY('Also a numeric value can be sent with the message.')
    #DISPLAY('On the client the "Event_Notification" method is executed')
    #DISPLAY('with the content of the message.')
    #DISPLAY('')    
    #DISPLAY('The message can be either a STRING or a GROUP')
  #ENDBOXED
  #BOXED,WHERE(%ChatAction = 'BroadcastNotification'),AT(0,0)
    #DISPLAY('The message is broadcast to all connected clients')
    #DISPLAY('Also a numeric value is sent with the message.')
    #DISPLAY('The client executes the "Event_BroadcastNotification"')
    #DISPLAY('method with the content of the message.')
    #DISPLAY('')    
    #DISPLAY('The message can be either a STRING or a GROUP')
  #ENDBOXED
  #BOXED,WHERE(%ChatAction = 'Shutdown'),AT(0,0)
    #DISPLAY('A Shutdown message is broadcast to all connected clients')
    #DISPLAY('Also a numeric value is sent with the message.')
    #DISPLAY('The client executes the "Event_BroadcastShutdown"')
    #DISPLAY('method with the content of the message.')
    #DISPLAY('')    
    #DISPLAY('')    
  #ENDBOXED
#ENDBOXED
#PROMPT('Message content:',FIELD),%MessageContent
#ENABLE(%ChatAction='Notify' OR %ChatAction='BroadcastNotification' OR %ChatAction='Shutdown')
#DISPLAY('')    
#DISPLAY('The numeric code sent with the notifications')
#PROMPT('Notification Code:',FIELD),%NotificationCode
#ENDENABLE
#DISPLAY('')    
#ENABLE(%ChatAction='Message' OR %ChatAction='Notify')
#DISPLAY('The recipient of the message or notification')
#PROMPT('User Recipient:',FIELD),%UseRecipient
#ENDENABLE
#DISPLAY('The code to execute defined in the AppConnect Extension')
#PROMPT('Disable Error Message',CHECK),%DisableGlobalCodeError,DEFAULT(%False),AT(10)
#PROMPT('Disable Not Connected Message',CHECK),%DisableGlobalCodeNotConnected,DEFAULT(%False),AT(10)
#PROMPT('Disable Action Succeeded',CHECK),%DisableGlobalCodeActionSucceeded,DEFAULT(%True),AT(10)
#ENDENABLE
#!
#!
Do %QuickAppConnectObjectName_ExecuteAction_%ChatAction_%ActiveTemplateInstance
#AT(%ProcedureRoutines),PRIORITY(4000),WHERE(%LocalEnable)
#EMBED(%SVQuickAppConnectAction,'Execute AppConnect Action'),%ActiveTemplateInstance,TREE('Procedure Routines|'&%QuickAppConnectObjectName&'|Client Routines|Actions|'&%ChatAction&'_'&%ActiveTemplateInstance&'|Before Routine Declaration{{priority(1)}')
#ENDAT
#!
#AT(%SVQuickAppConnectAction,%ActiveTemplateInstance),PRIORITY(5000),WHERE(%LocalEnable)
%QuickAppConnectObjectName_ExecuteAction_%ChatAction_%ActiveTemplateInstance ROUTINE
#EMBED(%SVQuickAppConnectActionData,'Execute AppConnect Action'),%ActiveTemplateInstance,TREE('Procedure Routines|'&%QuickAppConnectObjectName&'|Client Routines|Actions|'&%ChatAction&'_'&%ActiveTemplateInstance&'|Before Executing Code{{priority(2)}'),LABEL,DATA
 %ErrorVar = 0
 IF %QuickAppConnectObjectName.IsServerRunning() OR %QuickAppConnectObjectName.IsConnected()
    #CASE(%ChatAction)
    #OF('Shutdown')
    %ErrorVar = %QuickAppConnectObjectName.BroadcastShutdown(%NotificationCode,%MessageContent)
    #OF('Notify')
    %ErrorVar = %QuickAppConnectObjectName.SendNotify(%UseRecipient,%NotificationCode,%MessageContent)
    #OF('BroadcastNotification')
    %ErrorVar = %QuickAppConnectObjectName.BroadcastNotify(%NotificationCode,%MessageContent)
    #OF('Message')
    %ErrorVar = %QuickAppConnectObjectName.SendMessage(%UseRecipient,%MessageContent)
    #END
    IF %ErrorVar
       #EMBED(%SVQuickAppConnectActionError,'Execute AppConnect Action'),%ActiveTemplateInstance,TREE('Procedure Routines|'&%QuickAppConnectObjectName&'|Client Routines|Actions|'&%ChatAction&'_'&%ActiveTemplateInstance&'|If Error Code Returned - Connection Fail{{priority(3)}')
       #IF(NOT %DisableGlobalCodeError)
       %ExecuteCodeError
       #ENDIF
    ELSE
       #EMBED(%SVQuickAppConnectActionOk,'Execute AppConnect Action'),%ActiveTemplateInstance,TREE('Procedure Routines|'&%QuickAppConnectObjectName&'|Client Routines|Actions|'&%ChatAction&'_'&%ActiveTemplateInstance&'|When Action Succeeded{{priority(4)}')
       #IF(NOT %DisableGlobalCodeActionSucceeded)
       %ExecuteCodeSucceeded
       #ENDIF
    END
 ELSE
    #EMBED(%SVQuickAppConnectActionServerNot,'Execute AppConnect Action'),%ActiveTemplateInstance,TREE('Procedure Routines|'&%QuickAppConnectObjectName&'|Client Routines|Actions|'&%ChatAction&'_'&%ActiveTemplateInstance&'|If it is not connected{{priority(5)}')
    #IF(NOT %DisableGlobalCodeNotConnected)
    %ExecuteCodeNotConnected
    #ENDIF
 END
#ENDAT
#!--------------------------------------------------------------------------
#GROUP(%QuickAppConnectVirtuals, %TreeText, %DataText, %CodeText)
#EMBED(%QuickAppConnectMethodDataSection,'QuickAppConnect Method Data Section'),%ActiveTemplateInstance,%pClassMethod,%pClassMethodPrototype,LABEL,DATA,PREPARE(%FixClassName(%FixBaseClassToUse('Default'))),TREE(%TreeText & %DataText)
  #?CODE
  #EMBED(%QuickAppConnectMethodCodeSection,'QuickAppConnect Method Executable Code Section'),%ActiveTemplateInstance,%pClassMethod,%pClassMethodPrototype,PREPARE(%FixClassName(%FixBaseClassToUse('Default'))),TREE(%TreeText & %CodeText)
#!--------------------------------------------------------------------------
#!
#INCLUDE('SVFnGrp.TPW')
#INCLUDE('ABOOP.TPW')