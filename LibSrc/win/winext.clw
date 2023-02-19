  MEMBER
  INCLUDE('svapi.INC'),ONCE
  MAP
    INCLUDE('svapifnc.inc')
    MODULE('C runtime')
        __pathSplit (*CSTRING Path, *CSTRING Drv, *CSTRING Dir, *CSTRING Nme, *CSTRING Ext),SIGNED,PROC,RAW,NAME('_fnsplit')
    END
    MODULE('Win32API')
      _CreateToolHelp32Snapshot  (ULONG, ULONG),UNSIGNED,PASCAL,RAW,NAME('CreateToolHelp32Snapshot')
      _Process32First            (UNSIGNED, UNSIGNED),BOOL,PASCAL,RAW,NAME('Process32First')
      _Process32Next             (UNSIGNED, UNSIGNED),BOOL,PASCAL,RAW,NAME('Process32Next')
      _Module32First             (UNSIGNED hSnapshot, UNSIGNED),BOOL,PASCAL,RAW,NAME('Module32First')
      _CloseHandle               (UNSIGNED),BOOL,PASCAL,RAW,NAME('CloseHandle')
      _GetLastError              (),ULONG,PASCAL,RAW,NAME('GetLastError')      
    END  
    MODULE('')
      _EnumWindows(LONG lpEnumFunc, LONG lParam),BOOL,PASCAL,NAME('EnumWindows')
      _GetWindowThreadProcessId (UNSIGNED hWnd, <*LONG pid>),LONG,PASCAL,NAME('GetWindowThreadProcessId')
    END
    WinExtSubClassFunc(UNSIGNED,UNSIGNED,UNSIGNED,LONG),LONG,PASCAL
    MyEnumWindowsProc(UNSIGNED hwnd, LONG lParam),BOOL,PASCAL 
  END
  INCLUDE('WinExt.INC'),ONCE

!------------------------------------------------------------------------------
GloCurrentWinExt            &WindowExtenderClass,THREAD

WindowExtenderClass.Construct                   PROCEDURE()
 CODE

WindowExtenderClass.Destruct                    PROCEDURE()
 CODE
    IF SELF.Inited = true
       SELF.Inited = false
       CASE STATUS (SELF.MainWindow)
       OF WINDOW:OK
       OROF WINDOW:ClosePending
       OROF WINDOW:InDestroy
         SELF.MainWindow {PROP:WndProc} = SELF.OrigWndProc
       END
       GloCurrentWinExt &= SELF.PrvWinExt
    END

WindowExtenderClass.Init                        PROCEDURE(WINDOW TheMainWindow,BYTE AllowShutDown,BYTE AllowTrayIcon,STRING TrayIconName,<STRING ToolTip>)
 CODE
    SELF.Inited = true
    SELF.MainWindow &= TheMainWindow
    SELF.AllowTrayIcon = AllowTrayIcon
    SELF.AllowShutDown = AllowShutDown
    SELF.ToTrayOnLosefocus = False
    SELF.PrvWinExt &= GloCurrentWinExt
    GloCurrentWinExt &= SELF
    SELF.OrigWndProc = TheMainWindow{PROP:WndProc}
    TheMainWindow{PROP:WndProc} = ADDRESS(WinExtSubClassFunc)
    SELF.SetIconName(TrayIconName)
    IF NOT OMITTED(6)
       SELF.SetToolTip(ToolTip)
    END

WindowExtenderClass.GetEXEFileName              PROCEDURE()
locFileName   CSTRING(file:MaxFileName+1), auto
locDrv        CSTRING(file:MaxFileName+1)
locDir        CSTRING(file:MaxFileName+1)
LocName       CSTRING(file:MaxFileName+1)
locExt        CSTRING(file:MaxFileName+1)
 CODE
    IF SELF._EXEFileName = ''
       locFileName = SELF.GetEXEFullFileName()
       __pathSplit(locFileName, locDrv, locDir, LocName, locExt)       
       SELF._EXEFileName = LocName&locExt
    END
    RETURN SELF._EXEFileName

WindowExtenderClass.GetEXEFullFileName          PROCEDURE()
retLen        LONG,AUTO
!runningPID    LONG,AUTO
!hProcess      LONG,AUTO
!szExeFile      CSTRING(file:MaxFileName+1)
!szExeFileSize  LONG(file:MaxFileName)
 CODE
!    IF pProcessID = 0
       IF SELF._EXEFullFileName = ''
          retLen = GetModuleFileName(0, SELF._EXEFullFileName, 500)
       END
       RETURN SELF._EXEFullFileName
!    ELSE
!       This code is on hold because the support of XP
!       runningPID = pProcessID
!       hProcess = OpenProcess(PROCESS_QUERY_INFORMATION + PROCESS_VM_READ, false, runningPID);
!       IF hProcess
!          IF GetModuleFileNameEx(hProcess,0 , szExeFile, szExeFileSize);
!             CloseHandle(hProcess)
!             RETURN szExeFile
!          END
!       END
!       CloseHandle(hProcess)
!       RETURN ''
!    END

WindowExtenderClass.IsInstanceRunning           PROCEDURE(<STRING processExeName>)
runningPID LONG
currentPID LONG
currentSID LONG
TH32CS_SNAPPROCESS          equate(02H)
INVALID_HANDLE_VALUE        equate(-1)
retVal               LONG
locProcessName       CSTRING(file:MaxFileName+1)
hProcessSnap         UNSIGNED                              
th32sessionsID       LONG                                
pe32                 GROUP,PRE()                           
dwSize                ULONG                                
cntUsage              ULONG                                
th32ProcessID         ULONG                                
th32DefaultHeapID     ULONG                                
th32ModuleID          ULONG                                
cntThreads            ULONG                                
th32ParentProcessID   ULONG                                
pcPriClassBase        LONG                                 
dwFlags               ULONG                                
szExeFile             CSTRING(260)                         
                     END                                   
 CODE 
    IF OMITTED(processExeName) OR CLIP(processExeName)=''
       currentPID = GetCurrentProcessID()
       locProcessName    = UPPER(CLIP(SELF.GetEXEFileName()))
       IF ProcessIdToSessionId(currentPID,  currentSID)=0
          currentSID = 0
       END
    ELSE
       currentPID = 0
       locProcessName = UPPER(CLIP(processExeName))
    END

    retVal = 0

    hProcessSnap      = _CreateToolHelp32Snapshot(TH32CS_SNAPPROCESS,0)
    if hProcessSnap = INVALID_HANDLE_VALUE
?       MESSAGE('Failed to create process snapshot','Error!')
    else
       pe32.dwSize = SIZE(pe32)
       if _Process32First(hProcessSnap, ADDRESS(pe32))
          !- on first process
          IF UPPER(pe32.szExeFile) = locProcessName AND currentPID<>pe32.th32ProcessID
             runningPID = pe32.th32ProcessID
             IF ProcessIdToSessionId(runningPID, th32sessionsID)<>0
                IF th32sessionsID = currentSID
                   retVal = pe32.th32ProcessID
                END
             END
          END
          LOOP WHILE retVal = 0
               pe32.dwSize = size(pe32)
               IF _Process32Next(hProcessSnap, ADDRESS(pe32))
                  !- on next process
                  IF UPPER(pe32.szExeFile) = locProcessName AND currentPID<>pe32.th32ProcessID
                     runningPID = pe32.th32ProcessID
                     IF ProcessIdToSessionId(runningPID, th32sessionsID)<>0
                        IF th32sessionsID = currentSID
                           retVal = pe32.th32ProcessID
                        END
                     END       
                  END
               ELSE
                  BREAK
               END
          END
       ELSE
?          MESSAGE('Error ' & _GetLastError() & ' retrieving first process.','Error!')
       END
       IF _CloseHandle(hProcessSnap) = 0
?          MESSAGE('Error ' & _GetLastError() & ': closing process snapshot handle', 'Error!', ICON:Exclamation)
       END
    END
    RETURN retVal

WindowExtenderClass.RestoreInstanceRunning      PROCEDURE(<STRING processExeName>)
runningPID LONG
lRetVal BYTE
 CODE
    lRetVal = false
    runningPID = SELF.IsInstanceRunning(processExeName)
    IF runningPID > 0
       lRetVal = true
       IF _EnumWindows(ADDRESS(MyEnumWindowsProc), runningPID)
       END
    END
    RETURN lRetVal

WindowExtenderClass.SetIconName                 PROCEDURE(STRING TrayIconName)
 CODE
    SELF.IconName = TrayIconName
    IF SELF.IconName
       IF SELF.IconName[LEN(SELF.IconName)-3]='.'
          SELF.IconName[LEN(SELF.IconName)-3]='_'
       END
    END
    SELF.IconHandle = LoadImage(SYSTEM{PROP:AppInstance},ADDRESS(SELF.IconName),1,16,16,0)
    IF SELF.OnTray AND SELF.AllowTrayIcon
       SELF.ModifyTrayIcon()
    END

WindowExtenderClass.SetToolTip                  PROCEDURE(STRING ToolTip)
 CODE
    SELF.ToolTip = ToolTip
    IF SELF.OnTray AND SELF.AllowTrayIcon
       SELF.ModifyTrayIcon()
    END

WindowExtenderClass.SetAllowTrayIcon            PROCEDURE(BYTE OnOff)
 CODE
    IF NOT SELF.OnTray
       SELF.AllowTrayIcon = OnOff
    END

WindowExtenderClass.SetAllowShutDown            PROCEDURE(BYTE OnOff)
 CODE
    IF NOT SELF.OnShutDown
       SELF.AllowShutDown = OnOff
    END

WindowExtenderClass.SetToTrayOnLosefocus        PROCEDURE(BYTE OnOff)
 CODE
    SELF.ToTrayOnLosefocus = OnOff

WindowExtenderClass.GetToTrayOnLosefocus        PROCEDURE()
 CODE
    RETURN SELF.ToTrayOnLosefocus

WindowExtenderClass.GetAllowTrayIcon            PROCEDURE()
 CODE
    RETURN SELF.AllowTrayIcon

WindowExtenderClass.GetOnTray                   PROCEDURE()
 CODE
    RETURN SELF.OnTray

WindowExtenderClass.GetAllowShutDown            PROCEDURE()
 CODE
    RETURN SELF.AllowShutDown

WindowExtenderClass.TakeEvent                   PROCEDURE()
NotifyCode      UNSIGNED
NotifyThread    SIGNED
NotifyParameter LONG
lRetVal         BYTE
 CODE
    CASE EVENT()
    OF EVENT:CloseDown
    OF EVENT:CloseWindow
       IF SELF.OnTray
          SELF.DeleteTrayIcon()
          SELF.MainWindow{PROP:HIDE}=False
          SELF.MainWindow{PROP:Active}=true
          SELF.OnTray = False
       END
    OF EVENT:Maximize
       IF SELF.OnTray
          SELF.RestoreFromTray()
       END
    OF EVENT:Iconized
       IF NOT SELF.SendToTray()
          POST(EVENT:Maximize)
       END
    OF EVENT:Notify
       IF NOTIFICATION(NotifyCode,NotifyThread,NotifyParameter)
          IF NotifyThread=THREAD()
             IF SELF.OnTray
                CASE NotifyCode
                OF Notify:TrayIconMouseLeft
                   SELF.TrayIconMouseLeft()
                OF Notify:TrayIconMouseRight
                   SELF.TrayIconMouseRight()
                OF Notify:TrayIconMouseLeft2
                   SELF.TrayIconMouseLeft2()
                OF Notify:TrayIconMouseRight2
                   SELF.TrayIconMouseRight2()
                END
             ELSE
                CASE NotifyCode
                OF EVENT:Losefocus
                   IF SELF.ToTrayOnLosefocus
                      lRetVal = SELF.SendToTray()
                   END
                END
             END
          END
          CASE NotifyCode
          OF Notify:RestoreFromTray
             SELF.RestoreFromTray()
          OF Event:CloseDown
             POST(EVENT:CloseDown)
          END
       END
    END

WindowExtenderClass.SendToTray                   PROCEDURE()
RetVal  BYTE
 CODE
    IF SELF.AllowTrayIcon
       IF SELF.OnTray=False
          IF SELF.AddTrayIcon()
             SELF.MainWindow{PROP:HIDE}=true
             SELF.MainWindow{PROP:Active}=false
             SELF.OnTray = True
             RetVal = True
          ELSE
             SELF.OnTray = False
             RetVal = False
          END
       END
    END
    RETURN SELF.OnTray

WindowExtenderClass.RestoreFromTray              PROCEDURE()
 CODE
    IF SELF.OnTray = True
       SELF.DeleteTrayIcon()
       SELF.MainWindow{PROP:HIDE}=False
    END
    SELF.MainWindow{PROP:Active}=true
    DISPLAY(0)
    ShowWindow(SELF.MainWindow{Prop:Handle},SW_RESTORE)
    SetForegroundWindow(SELF.MainWindow{Prop:Handle})
    SELF.OnTray = False
    
WindowExtenderClass.AddTrayIcon PROCEDURE()
lError      LONG
IconImage   LONG
 CODE
    lError=0
    SELF.NID.uId = 1
    SELF.NID.hIcon = SELF.IconHandle
    SELF.NID.ToolTip = SELF.ToolTip
    SELF.NID.cbSize = SIZE(SELF.NID)
    SELF.NID.hWnd = SELF.MainWindow{Prop:Handle}
    SELF.NID.uFlags = NIF_ICON + NIF_MESSAGE + NIF_TIP
    SELF.NID.uCBmessage = EVENT:TrayIcon
    lError=Shell_NotifyIcon(NIM_ADD,ADDRESS(SELF.NID))
    IF lError=0
       IF GetLastError()
          RETURN False
       ELSE
          RETURN True
       END
    ELSE
       RETURN True
    END

WindowExtenderClass.ModifyTrayIcon PROCEDURE()
lError  LONG
 CODE
    lError=0
    SELF.NID.uId = 1
    SELF.NID.hIcon = SELF.IconHandle
    SELF.NID.ToolTip = SELF.ToolTip
    SELF.NID.cbSize = SIZE(SELF.NID)
    SELF.NID.uFlags = NIF_ICON + NIF_TIP
    SELF.NID.hWnd = SELF.MainWindow{Prop:Handle}
    SELF.NID.uCBmessage = EVENT:TrayIcon
    lError=Shell_NotifyIcon(NIM_MODIFY,ADDRESS(SELF.NID))

WindowExtenderClass.DeleteTrayIcon PROCEDURE()
lError  LONG
 CODE
    lError=0
    SELF.NID.uId = 1
    SELF.NID.hIcon = SELF.IconHandle
    SELF.NID.ToolTip = SELF.ToolTip
    SELF.NID.cbSize = SIZE(SELF.NID)
    SELF.NID.hWnd = SELF.MainWindow{Prop:Handle}
    SELF.NID.uFlags = NIF_ICON + NIF_MESSAGE + NIF_TIP
    SELF.NID.uCBmessage = EVENT:TrayIcon
    lError=Shell_NotifyIcon(NIM_DELETE,ADDRESS(SELF.NID))

WindowExtenderClass.ProcessShutDown                PROCEDURE()
 CODE
    RETURN True
WindowExtenderClass.TrayIconMouseLeft              PROCEDURE()
 CODE
WindowExtenderClass.TrayIconMouseRight             PROCEDURE()
 CODE
WindowExtenderClass.TrayIconMouseLeft2             PROCEDURE()
 CODE
WindowExtenderClass.TrayIconMouseRight2            PROCEDURE()
 CODE
WindowExtenderClass.GetMainWindowThread         PROCEDURE()
 CODE
    IF SELF.MainWindow &= NULL
       RETURN 0
    ELSE
       RETURN SELF.MainWindow{PROP:Thread}
    END
WinExtSubClassFunc       FUNCTION (UNSIGNED hWnd,UNSIGNED wMsg,UNSIGNED wParam,LONG lParam)
lIconID  LONG
  CODE                                                     ! Begin processed code
    CASE wMsg
    OF EVENT:TrayIcon        
           CASE BAND(lParam, 0FFFFh)
           OF WM_LBUTTONDOWN
           OF WM_LBUTTONUP
              lIconID = wParam
              NOTIFY (Notify:TrayIconMouseLeft,, lIconID)
           OF WM_LBUTTONDBLCLK
               lIconID = wParam
               NOTIFY (Notify:TrayIconMouseLeft2,, lIconID)
           OF WM_RBUTTONDOWN
           OF WM_RBUTTONUP
               lIconID = wParam
               NOTIFY (Notify:TrayIconMouseRight,, lIconID)
           OF WM_RBUTTONDBLCLK
               lIconID = wParam
               NOTIFY (Notify:TrayIconMouseRight2,, lIconID)
           END
        RETURN(0)
    OF WM_QUERYENDSESSION
      IF GloCurrentWinExt.AllowShutDown
         GloCurrentWinExt.OnShutDown=GloCurrentWinExt.ProcessShutDown()
         IF GloCurrentWinExt.OnShutDown
            RETURN(GloCurrentWinExt.OnShutDown)
         ELSE
            RETURN(False)
         END
      ELSE
         RETURN(False)
      END
    OF WM_ENDSESSION
      IF GloCurrentWinExt.AllowShutDown
         NOTIFY (Event:CloseDown)
         RETURN(True)
      ELSE
         RETURN(False)
      END
    OF WM_ACTIVATEAPP
       IF wParam=0
          NOTIFY(EVENT:Losefocus,GloCurrentWinExt.GetMainWindowThread(), 0)
          RETURN(true)
       END
    OF WM_SYSCOMMAND
       !IF GloCurrentWinExt.OnTray
          IF wParam = SC_RESTORE
               NOTIFY (Notify:RestoreFromTray,GloCurrentWinExt.GetMainWindowThread(), wParam)
               RETURN(true)
          END
       !END
    END
    RETURN(CallWindowProc(GloCurrentWinExt.OrigWndProc,hWnd,wMsg,wParam,lParam))

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! called form RestoreInstanceRunning
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
MyEnumWindowsProc PROCEDURE(UNSIGNED hwnd, LONG lParam)
bContinueEnum    BOOL
windowPId LONG
tId LONG
  CODE
  !lParam was called with runningPID
  tId = _GetWindowThreadProcessId(hwnd, windowPId)
  IF windowPId = lParam  
     SendMessage(hwnd,WM_SYSCOMMAND,SC_RESTORE,0);   
     SetForegroundWindow(hwnd)
     bContinueEnum = True!False !Stop the enumeration    
  ELSE
    bContinueEnum = True
  END
  RETURN bContinueEnum 

