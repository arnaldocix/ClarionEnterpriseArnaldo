!ABCIncludeFile(ABC)

OMIT('_EndOfInclude_',_ReportTargetSelectorClassPresent_)
_ReportTargetSelectorClassPresent_ EQUATE(1)

 INCLUDE('abrptgen.int'),ONCE

TargetOutputGeneratorQueue    QUEUE(),TYPE
OutputGenerator                &IOutputGeneratorTarget
ReportGenerator                &IReportGenerator
DataOutputGenerator            &IDataOutputGenerator
EnableOnPreview                BYTE
                              END

ReportTargetSelectorClass    CLASS,MODULE('ABRPPSEL.CLW'),TYPE,LINK('ABRPPSEL.CLW',_ABCLinkMode_),DLL(_ABCDllMode_)
Horizontal              BYTE
Stretch                 BYTE
WithPrinter             BYTE,PROTECTED
PrintSelected           BYTE,PROTECTED
SelectedItemIndex       BYTE,PRIVATE
DefaultItemIndex        BYTE,PROTECTED
OutputSelected          &IOutputGeneratorTarget,PRIVATE!only is valid if the ASK return true
QOutputGen              &TargetOutputGeneratorQueue,PROTECTED
SetSize                 PROCEDURE(BYTE pHorizontal=6,BYTE pStretch=1) ! The dialog resize it self, the pHorizontal is the max number of option to display before create a second option's row
AddItem                 PROCEDURE(IOutputGeneratorTarget pPlugIn,BYTE pEnableOnPreview=1),PROTECTED,VIRTUAL ! Add a plugin to the object
AddItem                 PROCEDURE(IReportGenerator pPlugIn,BYTE pEnableOnPreview=1) ! Add a plugin to the object
AddItem                 PROCEDURE(IDataOutputGenerator pPlugIn) ! Add a plugin to the object
Ask                     PROCEDURE(BYTE pWithPrinter=0,BYTE pFromPreview=0),BYTE,VIRTUAL !Return true if the user click OK and false if the user click Cancel. If pWithPrinter=True then the printer Option is showed on the dialog
Items                   PROCEDURE(BYTE pFromPreview=0),BYTE,VIRTUAL ! Return the number of plugin registered into the object
Item                    PROCEDURE(SHORT GeneratorPos,BYTE pFromPreview=0),*IOutputGeneratorTarget,PROTECTED,VIRTUAL
GetPrintSelected        PROCEDURE(),BYTE ! After the Ask it Return true if the print option was supported and selected.
FillQueue               PROCEDURE(QUEUE QueueToFill,BYTE ColumnToFill=1),VIRTUAL
GetOutputGeneratorName  PROCEDURE(SHORT GeneratorPos,BYTE pFromPreview=0),STRING,VIRTUAL !If the Pos is not valid it return ''
GetOutputGenerator      PROCEDURE(STRING GeneratorName),*IOutputGeneratorTarget,PROTECTED,VIRTUAL
SetSelectedOutputGenerator PROCEDURE(STRING GeneratorNameOrIndex),BYTE,PROTECTED,VIRTUAL !It return the index if found or 0 if not
GetOutputGeneratorIndex PROCEDURE(STRING GeneratorNameOrIndex),BYTE,VIRTUAL !It return the index if found or 0 if not
GetOutputSelected       PROCEDURE(),*IOutputGeneratorTarget,PROTECTED !It is only valid to call this method if GetPrintSelected=False
SetDefaultOutputGenerator PROCEDURE(STRING GeneratorNameOrIndex),VIRTUAL !It return the index if found or 0 if not

GetSelectedItemIndex    PROCEDURE(),BYTE
GetDefaultItemIndex     PROCEDURE(),BYTE

GetReportGenerator      PROCEDURE(STRING GeneratorName),*IReportGenerator
GetReportSelected       PROCEDURE(),*IReportGenerator !It is only valid to call this method if GetPrintSelected=False
GetReportDefault        PROCEDURE(),*IReportGenerator !It is only valid to call this method if GetPrintSelected=False
GetDataOutputGenerator  PROCEDURE(STRING GeneratorName),*IDataOutputGenerator
GetDataOutputSelected   PROCEDURE(),*IDataOutputGenerator !It is only valid to call this method if GetPrintSelected=False
GetDataOutputDefault    PROCEDURE(),*IDataOutputGenerator !It is only valid to call this method if GetPrintSelected=False

IsSelectedAReportOuput  PROCEDURE(),BYTE
IsSelectedADataOuput    PROCEDURE(),BYTE

CONSTRUCT               PROCEDURE()
DESTRUCT                PROCEDURE()
                      END

 _EndOfInclude_
