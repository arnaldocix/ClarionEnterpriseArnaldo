!ABCIncludeFile(ABC)

OMIT('_EndOfInclude_',_XMLReportGeneratorPresent_)
_XMLReportGeneratorPresent_ EQUATE(1)

 INCLUDE('ABPRTARG.INC'),ONCE   ! TargetClass
 INCLUDE('ABRPATMG.INC'),ONCE   ! Attr Manager
 INCLUDE('ABRPTGEN.INT'),ONCE   ! IReportGenerator Interface
 INCLUDE('ABERROR.INC'),ONCE
MaxXMLGeneratorNameLen  EQUATE(500)
MaxXMLGeneratorValueLen EQUATE(500)

XMLTagAttribute      GROUP
Name                    STRING(MaxXMLGeneratorNameLen)
Value                   STRING(MaxXMLGeneratorValueLen)
                     END
QueueTagAttributes   QUEUE(XMLTagAttribute),TYPE
                     END

TagType:Tag             EQUATE(1)  !<   />  OR <   >  </   >
TagType:HeaderAttribute EQUATE(2)  !<?  ?>
TagType:Comment         EQUATE(3)  !<!--   -->
TagType:TagAttribute    EQUATE(4)  !INTERNAL USE
TagType:TagValue        EQUATE(5)  !INTERNAL USE

QueueXMLComments     QUEUE,TYPE
Value                   STRING(MaxXMLGeneratorValueLen)
                     END
XMLTag               GROUP
Name                    STRING(MaxXMLGeneratorNameLen)
Value                   &STRING !STRING(MaxXMLGeneratorValueLen)
TagType                 BYTE !TagType:Tag , TagType:HtmlComment OR TagType:XMLComment
Attrs                   &QueueTagAttributes
Child                   &QueueTags
IsCData                 BYTE
                     END
QueueTags            QUEUE(XMLTag),TYPE
                     END
QueueProcessedTags   QUEUE,TYPE
FEQ                     USHORT
                     END
                     
XMLWriter       CLASS,MODULE('ABPRXML.CLW'),TYPE,LINK('ABPRXML.CLW',_ABCLinkMode_),DLL(_ABCDllMode_)
Output                  &TargetGenerator,PROTECTED

QTag                    &QueueTags
QHeaderAttr             &QueueTagAttributes

RootTag                 STRING(MaxXMLGeneratorNameLen),PROTECTED
RootTagAttributes       STRING(MaxXMLGeneratorNameLen),PROTECTED
Encoding                STRING(MaxXMLGeneratorValueLen),PROTECTED
XSL                     STRING(MaxXMLGeneratorValueLen),PROTECTED
SupportNameSpaces       BYTE,PROTECTED
ForceXMLHeader          BYTE,PROTECTED
RootAdded               BYTE,PROTECTED
XMLHeaderAdded          BYTE,PROTECTED
XMLVersion              STRING(5),PROTECTED
ppScapedString          &STRING,PROTECTED
UseXMLasXSLName         BYTE,PROTECTED
UseCRLF                 BYTE,PROTECTED

CONSTRUCT               PROCEDURE()
DESTRUCT                PROCEDURE()
ChildDESTRUCT           PROCEDURE(QueueTags pQTag)

Init                    PROCEDURE(<STRING TargetFileName>),VIRTUAL
Empty                   PROCEDURE()

SetFileName             PROCEDURE(STRING pFileName)
GetFileName             PROCEDURE(),STRING
SetForceXMLHeader       PROCEDURE(BYTE pTrue=1)
GetForceXMLHeader       PROCEDURE(),BYTE
SetSupportNameSpaces    PROCEDURE(BYTE pTrue=1)
GetSupportNameSpaces    PROCEDURE(),BYTE
SetRootTag              PROCEDURE(STRING pRootTag)
GetRootTag              PROCEDURE(),STRING
SetEncoding             PROCEDURE(STRING pEncoding)
GetEncoding             PROCEDURE(),STRING
SetXSL                  PROCEDURE(STRING pXSLFileName)
GetXSL                  PROCEDURE(),STRING
SetUseXMLasXSLName      PROCEDURE(BYTE pValue)
GetUseXMLasXSLName      PROCEDURE(),BYTE
SetUseCRLF              PROCEDURE(BYTE pTrue=1)
GetUseCRLF              PROCEDURE(),BYTE

AddXMLHeaderAttribute   PROCEDURE(STRING pName,STRING pValue)
RemoveXMLHeaderAttribute PROCEDURE(STRING pName)
AddComment              PROCEDURE(STRING pComment)
RemoveComments          PROCEDURE()
AddTag                  PROCEDURE(STRING pName,STRING pValue,BYTE pIsCData=0,<STRING pParent>),VIRTUAL
SetTagValue             PROCEDURE(STRING pTagName,STRING pValue,BYTE pIsCData=0)
AddAttribute            PROCEDURE(STRING pName,STRING pValue,STRING pTagName)

SetTagValueToParent     PROCEDURE(QueueTags pQTag,STRING pTagName,STRING pValue,BYTE pIsCData=0),BYTE,PROTECTED
AddTagToParent          PROCEDURE(QueueTags pQTag,STRING pName,STRING pValue,BYTE pIsCData=0,STRING pParent),BYTE,PROTECTED
AddAttributeToParent    PROCEDURE(QueueTags pQTag,STRING pName,STRING pValue,STRING pTagName),BYTE,PROC,PROTECTED
MakeValidName           PROCEDURE(STRING pName),STRING,PROTECTED
EscapeValues            PROCEDURE(STRING pValue),STRING,PROTECTED

FlushTags               PROCEDURE(SHORT pIndent,QueueTags pQTag),PROTECTED
FlushRootOpen           PROCEDURE(),PROTECTED,VIRTUAL
FlushRootClose          PROCEDURE(),PROTECTED,VIRTUAL
WriteLine               PROCEDURE(STRING pLine),PROTECTED

Save                    PROCEDURE()
SaveAs                  PROCEDURE(STRING pFileName)
                END

XMLGenerator    CLASS(XMLWriter),MODULE('ABPRXML.CLW'),TYPE,LINK('ABPRXML.CLW',_ABCLinkMode_),DLL(_ABCDllMode_)
PagesAsDifferentFile    BYTE,PRIVATE
PagesAsParentTag        BYTE,PRIVATE
BasePageTagName         STRING(15),PROTECTED
PageNo                  SHORT(0),PROTECTED
TotalPageNo             SHORT(0),PROTECTED
PageOpen                BYTE,PRIVATE
PageTagAdded            BYTE,PROTECTED

Init                    PROCEDURE(<STRING TargetFileName>),VIRTUAL
Setup                   PROCEDURE(),VIRTUAL
SetPagesAsParentTag     PROCEDURE(BYTE pTrue=1)
SetPagesAsDifferentFile PROCEDURE(BYTE pTrue=1)
OpenDocument            PROCEDURE(<STRING pDocumentName>)
CloseDocument           PROCEDURE()
StartPage               PROCEDURE()
EndPage                 PROCEDURE()
FlushRootOpen           PROCEDURE(),PROTECTED,VIRTUAL,DERIVED
FlushRootClose          PROCEDURE(),PROTECTED,VIRTUAL,DERIVED
FlushPages              PROCEDURE(),PROTECTED
AddTag                  PROCEDURE(STRING pName,STRING pValue,BYTE pIsCData=0,<STRING pParent>),VIRTUAL,DERIVED

                      END

XMLReportGenerator    CLASS(XMLGenerator),IMPLEMENTS(IReportGenerator),MODULE('ABPRXML.CLW'),TYPE,LINK('ABPRXML.CLW',_ABCLinkMode_),DLL(_ABCDllMode_)
ProcessedTag            &QueueProcessedTags,PROTECTED
OutputFileQueue         &OutputFileQueue,PROTECTED
Attribute               &ReportAttributeManager,PROTECTED
CONSTRUCT               PROCEDURE()
DESTRUCT                PROCEDURE()
                      END

 _EndOfInclude_
