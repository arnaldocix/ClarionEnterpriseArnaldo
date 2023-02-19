!ABCIncludeFile

!Declarations of SortHeader Class and Type
OMIT('_EndOfInclude_',_WindowExtender_)
_WindowExtender_ EQUATE(1)

INCLUDE('svapi.inc'),ONCE


!Shell_NotifyIcon Events
EVENT:TrayIcon               EQUATE(660H)
Notify:TrayIconMouseLeft     EQUATE(661H)
Notify:TrayIconMouseRight    EQUATE(662H)
Notify:TrayIconMouseLeft2    EQUATE(663H)
Notify:TrayIconMouseRight2   EQUATE(664H)
Notify:RestoreFromTray       EQUATE(664H)


WindowExtenderClass    CLASS,TYPE,MODULE('WinExt.CLW'),LINK('WinExt.CLW',_ABCLinkMode_),DLL(_ABCDllMode_)
Inited                      BYTE,PROTECTED
MainWindow                  &WINDOW,PROTECTED
PrvWinExt                   &WindowExtenderClass,PROTECTED
OrigWndProc                 LONG,PROTECTED
AllowTrayIcon               BYTE,PROTECTED
ToTrayOnLosefocus           BYTE,PROTECTED
AllowShutDown               BYTE,PROTECTED
OnTray                      BYTE,PROTECTED
OnShutDown                  BYTE,PROTECTED
NID                         LIKE(NotifyIconData),PROTECTED
IconName                    CSTRING(128),PROTECTED
IconHandle                  LONG,PROTECTED
ToolTip                     CSTRING(64),PROTECTED
ToolTipHandle               LONG,PROTECTED
Init                        PROCEDURE(WINDOW TheMainWindow,BYTE AllowShutDown,BYTE AllowTrayIcon,STRING TrayIconName,<STRING ToolTip>)
GetMainWindowThread         PROCEDURE(),SIGNED
SetAllowTrayIcon            PROCEDURE(BYTE OnOff)
SetAllowShutDown            PROCEDURE(BYTE OnOff)
SetToTrayOnLosefocus        PROCEDURE(BYTE OnOff)
GetAllowTrayIcon            PROCEDURE(),BYTE
GetAllowShutDown            PROCEDURE(),BYTE
GetToTrayOnLosefocus        PROCEDURE(),BYTE
GetOnTray                   PROCEDURE(),BYTE
SetIconName                 PROCEDURE(STRING TrayIconName)
SetToolTip                  PROCEDURE(STRING ToolTip)
Construct                   PROCEDURE()!,FINAL
Destruct                    PROCEDURE()!,FINAL
TakeEvent                   PROCEDURE()
RestoreFromTray             PROCEDURE()         !if the app is on the icon tray will remove the icon , unhide the window and show the window
SendToTray                  PROCEDURE(),BYTE    !If return true the process of adding the icon to the tray and hiding the window was successful
TrayIconMouseLeft           PROCEDURE(),VIRTUAL        !Virtual method to process the mouse event onthe tray
TrayIconMouseRight          PROCEDURE(),VIRTUAL
TrayIconMouseLeft2          PROCEDURE(),VIRTUAL
TrayIconMouseRight2         PROCEDURE(),VIRTUAL        
ProcessShutDown             PROCEDURE(),BYTE,VIRTUAL   !Virtual method to process the Window Shutdown, if return true the shutdown will continue, if false it will be stoped.
AddTrayIcon                 PROCEDURE(),BYTE,PROTECTED
ModifyTrayIcon              PROCEDURE(),PROTECTED
DeleteTrayIcon              PROCEDURE(),PROTECTED
_EXEFileName                CSTRING(500),PROTECTED
_EXEFullFileName            CSTRING(500),PROTECTED
GetEXEFileName              PROCEDURE(),STRING
GetEXEFullFileName          PROCEDURE(),STRING
IsInstanceRunning           PROCEDURE(<STRING processExeName>),LONG !Return the instance running other than this one
RestoreInstanceRunning      PROCEDURE(<STRING processExeName>),BYTE !If the process is ommited it refere to the current process name returned by GetEXEFileName() processExeName must be Filename.Ext
                        END

_EndOfInclude_
