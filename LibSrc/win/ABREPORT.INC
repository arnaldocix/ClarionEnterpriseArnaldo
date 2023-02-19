! Process server header module

!ABCIncludeFile

  OMIT('_EndOfInclude_',_ReportsPresent_)
_ReportsPresent_   EQUATE(1)

  INCLUDE('ABWINDOW.INC'),ONCE
  INCLUDE('ABUTIL.INC'),ONCE
  INCLUDE('ABWMFPAR.INC'),ONCE
  INCLUDE('ABRPTGEN.INT'),ONCE
  INCLUDE('ABRPATMG.INC'),ONCE
  INCLUDE('ABRPPSEL.INC'),ONCE
  INCLUDE('ABBreak.INC'),ONCE
  INCLUDE('PRNPROP.CLW'),ONCE

NoZoom            EQUATE(-2)
PageWidth         EQUATE(-1)

PreviewQueue      EQUATE(PrintPreviewFileQueue) !Declared on the equates.clw contain a fiel Filename of type STRING(FILE:MaxFileName)


ProcessClass      CLASS(ViewManager),TYPE,MODULE('ABREPORT.CLW'),LINK('ABREPORT.CLW',_ABCLinkMode_),DLL(_ABCDllMode_)
BytesRead           LONG,PRIVATE
FileSize            LONG,PRIVATE
Children            &ChildList,PRIVATE
Percentile          &BYTE,PROTECTED
PText               SIGNED,PROTECTED
Query               &QueryClass
RecordsProcessed    LONG
RecordsToProcess    LONG
StepMgr             &StepClass,PRIVATE
ValueField          ANY,PRIVATE
ChildRead           USHORT             ! Must have the UNSIGNED type
ChildEOF            BYTE
CaseSensitiveValue  BYTE

AddItem             PROCEDURE(ViewManager VM,BYTE Level=0),USHORT
Init                PROCEDURE(VIEW V,RelationManager RM,SIGNED ProgressText = 0,<*BYTE PercentProgress>,LONG GuessRecords=0)   !,EXTENDS
Init                PROCEDURE(VIEW V,RelationManager RM,SIGNED ProgressText = 0,<*BYTE PercentProgress>,StepClass SC,*? ValueField)   !,EXTENDS
Kill                PROCEDURE,VIRTUAL
Next                PROCEDURE(BYTE ProcessRecords),VIRTUAL,BYTE
Reset               PROCEDURE,VIRTUAL
SetProgressLimits   PROCEDURE
SetProgressLimits   PROCEDURE(STRING Low,STRING High)
TakeAccepted        PROCEDURE,VIRTUAL
TakeLocate          PROCEDURE,VIRTUAL
TakeRecord          PROCEDURE,BYTE,VIRTUAL,PROC
UpdateDisplay       PROCEDURE,VIRTUAL
ChildProcessingStart PROCEDURE(USHORT ChildRead),VIRTUAL
ChildProcessingEnd   PROCEDURE(USHORT ChildRead),VIRTUAL
               END


MinXSeperation    EQUATE(18)                     !thumbnail minimum seperations in X and Y directions
MinYSeperation    EQUATE(18)                     ! in pixels
ZoomItem          EQUATE(200)                    !Base feq for zoom menu items
BaseFeq           EQUATE(500)                    !Base feq for dynamically created controls

SaveSizeType      GROUP,TYPE
Set                 BYTE
XPos                SIGNED
YPos                SIGNED
Width               SIGNED
Height              SIGNED
                  END

PrintPreviewClass CLASS(WindowManager),TYPE,MODULE('ABREPORT.CLW'),LINK('ABREPORT.CLW',_ABCLinkMode_),DLL(_ABCDllMode_)
ShowOutputProgress  BYTE(1)
AllowUserZoom       BYTE                                      !Set to false to remove User Zoom feature - backward compatibility
AllowPrintCurrent   BYTE(1)                                   !Set to false to remove Print Current Page button and menu
AllowSendReport     BYTE(0)                                   !Enable the Email report menu item 
OnePageReportJobName CSTRING(250)                             !Text to show when the one page get printed in the Printed Job
ConfirmPages        BYTE                                      !Set true to force 'pages to print' confirmation before printing
CurrentPage         LONG
MDIWindow           BOOL                                      !DEPRECATED !Set true to open the previewer window as an MDI child window
FirstPage           LONG,PROTECTED
ImageQueue          &PreviewQueue,PROTECTED
OutputFileQueue     &OutputFileQueue,PROTECTED
INIMgr              &INIClass,PRIVATE
Maximize            BYTE                                      !Set true to maximize Viewer Window
mUse                BYTE,DIM(32),PRIVATE                      !Use variables for dynamically created menu items
PageManager         &PageManagerClass,PRIVATE                 !Pointer to last page manager in linked list
PagesAcross         USHORT
PagesDown           USHORT
PagesToPrint        CSTRING(256),PROTECTED
Popup               &PopupClass,PROTECTED
PreviewWindow       &WINDOW,PROTECTED
PrintEngine         &PrintEngineClass,PROTECTED
PrintOK             BYTE,PROTECTED
RTLLayout           BYTE,PROTECTED
TargetSelector      &ReportTargetSelectorClass,PROTECTED
SaveSize            LIKE(SaveSizeType),PRIVATE
ThumbnailsPresent   UNSIGNED,PRIVATE
UserPercentile      SIGNED(0)                                 !Allows user to set their own percentile zoom factor
InitZoom            SIGNED,PROTECTED
WindowPosSet        BYTE(False)                               !DEPRECATED !Flags prefered initial position set
WindowSizeSet       BYTE(False)                               !DEPRECATED !Flags prefered initial size set
WinHeight           SIGNED,PRIVATE
WinWidth            SIGNED,PRIVATE
WinXPos             SIGNED,PRIVATE
WinYPos             SIGNED,PRIVATE
WMFParser           &WMFDocumentParser,PROTECTED
ZoomQueue           &ZoomItemQueue,PRIVATE
ReportTarget        &IReportGenerator,PROTECTED
ZoomIndex           BYTE                                      !index number of zoom factor in ZoomPresets group or 0 if user zoom active
ZoomList            STRING(16),PROTECTED                      !Use variable for drop Combo, zoom list

PrtPrev:TBarZoom    SIGNED,PROTECTED                          !These variables hold the feq's of various preview window components
PrtPrev:TBarPrint   SIGNED,PROTECTED
PrtPrev:TBarPrintCurrent SIGNED,PROTECTED
PrtPrev:TBarExit    SIGNED,PROTECTED
PrtPrev:MenuPages   SIGNED,PROTECTED
PrtPrev:MenuPrint   SIGNED,PROTECTED
PrtPrev:MenuPrintCurrent SIGNED,PROTECTED
PrtPrev:MenuSaveAs  SIGNED,PROTECTED
PrtPrev:MenuProcFiles SIGNED,PROTECTED
PrtPrev:MenuExit    SIGNED,PROTECTED
PrtPrev:ZoomList    SIGNED,PROTECTED
PrtPrev:ZoomPrompt  SIGNED,PROTECTED
PrtPrev:PagePrompt  SIGNED,PROTECTED
PrtPrev:CurrentPage SIGNED,PROTECTED
PrtPrev:PagesAcross SIGNED,PROTECTED
PrtPrev:PagesDown   SIGNED,PROTECTED
PrtPrev:Next        SIGNED,PROTECTED
PrtPrev:Previous    SIGNED,PROTECTED
PrtPrev:Jump        SIGNED,PROTECTED
PrtPrev:ChangeDisplay SIGNED,PROTECTED
PrtPrev:ViewMenu    SIGNED,PROTECTED
PrtPrev:ZoomMenu    SIGNED,PROTECTED
PrtPrev:LastCtrl    SIGNED,PROTECTED
PrtPrev:LastDynCtrl SIGNED,PROTECTED

AskPage             PROCEDURE(),BYTE,PROC,VIRTUAL,PROTECTED
AskPrintPages       PROCEDURE(),BYTE,PROC,VIRTUAL,PROTECTED   !Returns false if user cancelled
AskThumbnails       PROCEDURE(),VIRTUAL,PROTECTED
ConfigMenuChecks    PROCEDURE(),PRIVATE
DeleteImageQueue    PROCEDURE(LONG Idx),BYTE,PROC,VIRTUAL
Display             PROCEDURE(SHORT InitZoomFactor=0,LONG InitCurrentPage=1,USHORT InitPagesAcross=1,USHORT InitPagesDown=1),BYTE,VIRTUAL
DrawPage            PROCEDURE(),PRIVATE
Init                PROCEDURE(*PreviewQueue ImageQueue),VIRTUAL
Init                PROCEDURE(*PreviewQueue ImageQueue, *ReportTargetSelectorClass pTargetSelector, WMFDocumentParser WMFDocParser),VIRTUAL
InitWindowPosition  PROCEDURE(WINDOW win),PROTECTED
InPageList          PROCEDURE(LONG PageNumber),BYTE
Kill                PROCEDURE(),BYTE,PROC,VIRTUAL
NewZoom             PROCEDURE(),PRIVATE
Open                PROCEDURE(),VIRTUAL
OnSaveAs            PROCEDURE(),BYTE,VIRTUAL
OnProcessFiles      PROCEDURE(),BYTE,VIRTUAL
ParseImages         PROCEDURE(BOOL force),BYTE,PRIVATE
ProcessResultFiles  PROCEDURE(*OutputFileQueue OutputFile),BYTE,VIRTUAL
ResetUserZoom       PROCEDURE(),PRIVATE
SetDefaultPages     PROCEDURE(),VIRTUAL
SetINIManager       PROCEDURE(*INIClass INI)                !Allows Previewer to use programs INI file for setting restoration between calls
SetRightToLeft      PROCEDURE()
SetPosition         PROCEDURE(<SIGNED XPos>,<SIGNED YPos>,<SIGNED Width>,<SIGNED Height>)
SetZoomPercentile   PROCEDURE(UNSIGNED Percentile)          !Sets zoom to a given percentage
SyncImageQueue      PROCEDURE(),VIRTUAL
TakeAccepted        PROCEDURE(),BYTE,PROC,VIRTUAL
TakeEvent           PROCEDURE(),BYTE,PROC,VIRTUAL
TakeWindowEvent     PROCEDURE(),BYTE,PROC,VIRTUAL
TakeFieldEvent      PROCEDURE(),BYTE,PROC,VIRTUAL
TakeCurrentPage     PROCEDURE(),VIRTUAL
                  END

ReportManager CLASS(WindowManager),TYPE,MODULE('ABREPORT.CLW'),LINK('ABREPORT.CLW',_ABCLinkMode_),DLL(_ABCDllMode_)
DeferWindow         SIGNED,PROTECTED
Processors          &ProcessorQueue,PRIVATE
Preview             &PrintPreviewClass,PROTECTED
PreviewQueue        &PreviewQueue,PROTECTED
OutputFileQueue     &OutputFileQueue,PROTECTED
Process             &ProcessClass,PROTECTED
Attribute           &ReportAttributeManager,PROTECTED
ReportTarget        &IReportGenerator,PROTECTED
DataOutputTarget    &IDataOutputGenerator,PROTECTED
TargetSelector      &ReportTargetSelectorClass,PROTECTED
BreakMan            &BreakManagerClass,PROTECTED
RecordsPerCycle     LONG,PRIVATE
QueryControl        SIGNED
Report              &WINDOW
StartTime           LONG,PRIVATE
TimeSlice           LONG
WMFParser           &WMFDocumentParser,PROTECTED
Zoom                UNSIGNED
DeferOpenReport     BYTE,PROTECTED
KeepVisible         BYTE,PROTECTED
OpenFailed          BYTE,PRIVATE
OpenOutputWhenFinish BYTE,PROTECTED
TargetSelectorCreated BYTE,PROTECTED
SkipPreview         BYTE
ShowOutputProgress  BYTE(1)
UseMRP              BYTE,PROTECTED
WaitCursor          BYTE,PROTECTED

SetUseMRP           PROCEDURE(BYTE UseMRP=TRUE),VIRTUAL
GetUseMRP           PROCEDURE(),BYTE,VIRTUAL
AddItem             PROCEDURE(*RecordProcessor RC)
AddItem             PROCEDURE(*ReportTargetSelectorClass pTargetSelector)
AddItem             PROCEDURE(*BreakManagerClass pBreakMan)
SetReportTarget     PROCEDURE(*IReportGenerator pReportTarget)
SetOpenOutputWhenFinish PROCEDURE(BYTE value=TRUE),VIRTUAL
GetOpenOutputWhenFinish PROCEDURE(),BYTE,VIRTUAL
Ask                 PROCEDURE(),VIRTUAL
AskPreview          PROCEDURE(),VIRTUAL
EndReport           PROCEDURE(),BYTE,PROC,VIRTUAL
PrintReport         PROCEDURE(),VIRTUAL
CancelPrintReport   PROCEDURE(),VIRTUAL
Next                PROCEDURE(),BYTE,PROC,VIRTUAL
Init                PROCEDURE(ProcessClass PC,<REPORT R>,<PrintPreviewClass PV>)   !,EXTENDS
Kill                PROCEDURE(),BYTE,PROC,VIRTUAL
Open                PROCEDURE,VIRTUAL
OpenReport          PROCEDURE,BYTE,PROC,PROTECTED,VIRTUAL
ProcessResultFiles  PROCEDURE(OutputFileQueue OutputFile),BYTE,PROC,VIRTUAL
SetStaticControlsAttributes PROCEDURE(),VIRTUAL
SetDynamicControlsAttributes PROCEDURE(),VIRTUAL
TakeAccepted        PROCEDURE(),BYTE,PROC,PROTECTED,DERIVED
TakeCloseEvent      PROCEDURE(),BYTE,PROC,VIRTUAL
TakeNoRecords       PROCEDURE(),VIRTUAL
TakeRecord          PROCEDURE(),BYTE,PROC,VIRTUAL
TakeWindowEvent     PROCEDURE(),BYTE,PROC,VIRTUAL
  END

  _EndOfInclude_

