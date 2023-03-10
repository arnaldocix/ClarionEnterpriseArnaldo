!ABCIncludeFile

OMIT('_EndOfInclude_',_HTMLReportGeneratorPresent_)
_HTMLReportGeneratorPresent_ EQUATE(1)

 INCLUDE('ABPRTARG.INC'),ONCE   ! TargetClass
 INCLUDE('ABRPATMG.INC'),ONCE   ! Attr Manager
 INCLUDE('ABRPTGEN.INT'),ONCE   ! IReportGenerator Interface
 INCLUDE('ABPRNAME.INC'),ONCE   ! NameGenerator
 INCLUDE('ABERROR.INC'),ONCE

TextStyleQueue    QUEUE,TYPE
Name                CSTRING(20)
Color               LONG
Face                CSTRING(32)
Size                SIGNED
Style               SIGNED
                  END  

RectStyleQueue    QUEUE,TYPE
Name                CSTRING(20)
Color               LONG
BorderColor         LONG
BorderWidth         DECIMAL(9,4)
IsLine              BYTE
                  END

PosQueue     QUEUE,TYPE
Name           CSTRING(20)
XPos           DECIMAL(9,4)
YPos           DECIMAL(9,4)
Width          DECIMAL(9,4)
Height         DECIMAL(9,4)
             END
                        
HTMLGenerator   CLASS,TYPE,MODULE('ABPRHTML.CLW'),LINK('ABPRHTML.CLW',_ABCLinkMode_),DLL(_ABCDllMode_)
Output                  &TargetGenerator,PRIVATE
CSSOutput               &TargetGenerator,PRIVATE
PosQ                    &PosQueue,PRIVATE
TextStyleQ              &TextStyleQueue,PRIVATE
RectStyleQ              &RectStyleQueue,PRIVATE
OutNameG                &NameGenerator,PRIVATE
ImageNameG              &NameGenerator,PRIVATE
Attribute               &ReportAttributeManager,PROTECTED
DocumentName            CSTRING(255),PRIVATE
DocumentOpen            BYTE,PRIVATE
PageNo                  UNSIGNED,PRIVATE
TotalPageNo             UNSIGNED,PRIVATE
PageOpen                BYTE,PRIVATE
PosCnt                  UNSIGNED,PRIVATE             
TextStyleCnt            UNSIGNED,PRIVATE             
RectStyleCnt            UNSIGNED,PRIVATE             
SkipControl             BYTE,PRIVATE
BoxLeft                 LONG,PRIVATE
BoxTop                  LONG,PRIVATE
BoxRight                LONG,PRIVATE
BoxBottom               LONG,PRIVATE
HyperLinkTarget         STRING(MaxAttributeSize),PRIVATE
HyperLink               STRING(MaxAttributeSize),PRIVATE
IsHyperLink             BYTE, PRIVATE
Errors                  &ErrorClass,PRIVATE
NavFirst                STRING(15)
NavLast                 STRING(15)
NavNext                 STRING(15)
NavPrior                STRING(15)
NavPage0                STRING(15)
NavPageText             STRING(15)
NavGoPage               STRING(15)
CreateSubDirectory      BYTE
SingleFileContent       BYTE
ImagesSubDirectory      CSTRING(FILE:MaxFilePath)
FilesSubDirectory       CSTRING(FILE:MaxFilePath)
CONSTRUCT               PROCEDURE()
DESTRUCT                PROCEDURE()
Init                    PROCEDURE(<STRING FileName>)
SetUp                   PROCEDURE(),VIRTUAL
SetFileName             PROCEDURE(STRING pFileName)
SetNavigationText       PROCEDURE(STRING pNavFirst,STRING pNavLast,STRING pNavNext,STRING pNavLast,STRING pNavPage0,STRING pNavPageText,STRING pNavGoPage)
SetSubDirectory         PROCEDURE(BYTE pCreateSubDirectory,<STRING pFilesSubDirectory>,<STRING pImagesSubDirectory>)
SetSingleFile           PROCEDURE(BYTE pSingleFile = 1)
SetDocumentName         PROCEDURE(STRING pDocName)
ProcessCheck            PROCEDURE(*CheckFormatGrp, STRING Text),PRIVATE
ProcessComment          PROCEDURE(STRING Text,STRING pAttribute)
ProcessHiddenControlSize PROCEDURE(*PosGrp pPosition,STRING pAttribute),PROC,PRIVATE !If the control was hidden in runtime it replace the width and height with the real
ProcessHiddenControlSize PROCEDURE(*StringFormatGrp pStrFormat,STRING pAttribute),PROC,PRIVATE
ProcessImage            PROCEDURE(*ImageFormatGrp, STRING iName),PRIVATE
ProcessLine             PROCEDURE(*LineFormatGrp),PRIVATE
ProcessLocation         PROCEDURE(*PosGrp,USHORT BorderWidth=0),PRIVATE
ProcessGroup            PROCEDURE(*GroupFormatGrp gfg, STRING Text),PRIVATE
ProcessRadio            PROCEDURE(*RadioFormatGrp, STRING Text),PRIVATE
ProcessRect             PROCEDURE(*RectFormatGrp, BYTE),PRIVATE
ProcessString           PROCEDURE(*StringFormatGrp, STRING),PRIVATE

FlushStyleSheet         PROCEDURE(),PRIVATE
FlushNavigation         PROCEDURE(),PRIVATE
                 END

HTMLReportGenerator    CLASS(HTMLGenerator),IMPLEMENTS(IReportGenerator),MODULE('ABPRHTML.CLW'),TYPE,LINK('ABPRHTML.CLW',_ABCLinkMode_),DLL(_ABCDllMode_)
OutputFileQueue         &OutputFileQueue,PROTECTED
ImageFileQueue          &OutputFileQueue,PROTECTED
CONSTRUCT               PROCEDURE()
DESTRUCT                PROCEDURE()
                      END
                 
 _EndOfInclude_
                 

