!
!INTERFACE: IXmlNotify
!
!FoundNode           PROCEDURE( STRING name, STRING  attributes )
!**
!* The FoundNode method is called by the parser when a node </> is found
!**
!
!
!CloseNode           PROCEDURE( STRING name)
!**
!* The CloseNode method is called by the parser after calling the FoundNode
!**
!
!
!FoundElement        PROCEDURE( STRING name, STRING  value, STRING  attributes )
!**
!* The FoundElement method is called by the parser when a <>data<\> is found
!* The data is a value, not other tags
!* The data will be clipped on left and right
!**
!
!
!CloseElement        PROCEDURE( STRING name)
!**
!* The CloseElement method is called by the parser after calling the FoundElement,
!* in between those calls the FoundAttribute can be called if the tag has any attributes
!**
!
!
!StartElement        PROCEDURE( STRING name, STRING  value, STRING  attributes )
!**
!* The StartElement method is called by the parser when a <>data<\> is found
!* The data are other child Tags
!**
!
!
!EndElement          PROCEDURE( STRING name, STRING  value, STRING  attributes )
!**
!* The EndElement method is called by the parser after calling the StartElement,
!* in between those calls the FoundAttribute can be called if the tag has any attributes
!* Also any method related with the child Tags will be called
!**
!
!
!FoundComment        PROCEDURE( STRING Comment)
!**
!* The FoundComment method is called by the parser when a <!-- --!> is found
!**
!
!
!FoundHeader         PROCEDURE( STRING attributes)
!**
!* The FoundHeader method is called by the parser when a <? ?> is found
!**
!
!
!CloseHeader         PROCEDURE()
!**
!* The CloseHeader method is called by the parser after calling the FoundHeader
!* in between those calls the FoundAttribute can be called if the tag has any attributes
!**
!
!
!FoundAttribute      PROCEDURE( STRING tagname, STRING name, STRING  value )
!**
!* The FoundAttribute method is called by the parser when an attribute is found in a Tag or Header
!* and attribute will be in the form of name="value" the value must be between double quotes
!*
!**
!
!
IXmlNotify      INTERFACE
FoundNode           PROCEDURE( STRING name, STRING  attributes )
CloseNode           PROCEDURE( STRING name)
FoundElement        PROCEDURE( STRING name, STRING  value, STRING  attributes )
CloseElement        PROCEDURE( STRING name)
StartElement        PROCEDURE( STRING name, STRING  value, STRING  attributes )
EndElement          PROCEDURE( STRING name, STRING  value, STRING  attributes )
FoundComment        PROCEDURE( STRING Comment)
FoundHeader         PROCEDURE( STRING attributes)
CloseHeader         PROCEDURE()
FoundAttribute      PROCEDURE( STRING tagname, STRING name, STRING  value )
                END


! CLASS: XMLParser
!This is a none validate XML parser
!It is fast and use very low memory
!It can be very handy to parse trusted files that are valid
!The parser does not store or create any structure with the XML data
!instead it use an interface and call each method of that interface
!passing the XML Data.
!To use the parser the IXmlNotify need to be implemented.
!
!Parse               PROCEDURE(*STRING xmlStream,IXmlNotify XmlNotify),BYTE,PROC
!**
!* The Parse method initialize the parsing.
!* All the tags will be processed and passed to the IXmlNotify interface
!* This method requiere and stream containing the XML data.
!* To use a file that file need to be read in memory first
!**
!
!
!SetProgressControl  PROCEDURE(SIGNED ProgressControl)
!**
!* Allow to pass a FEQ for a ProgressControl
!**
!
!GetProgressControl  PROCEDURE(),SIGNED
!**
!* Return the stored ProgressControl FEQ
!**
!
!SetPassParameters   PROCEDURE(BYTE PassParemeters)
!**
!* Allow to set the PassParemeters property.
!* If the PassParemeters = true then the value and attributes parameters will be passed to the interface.
!* PassParemeters = false only values not child Tags will be passed and the attributes will be only passed to the FoundAttribute method
!* The PassParemeters = false use less resources and only for debug PassParemeters = true should be used, once the
!* Interfaces is implemented properly there are no reason to use PassParemeters = true
!**
!
!GetPassParameters   PROCEDURE(),BYTE
!**
!* Return the value of PassParameters property
!**
!
QNamePositions QUEUE,TYPE
TokenStart           LONG
TokenEnd             LONG
 END

XMLQueueOfTextType   QUEUE,TYPE
Text            STRING(400)
Level           LONG
               END



XMLParser       CLASS,IMPLEMENTS(IXmlNotify),TYPE,MODULE('QuickXMLParser.CLW'),LINK('QuickXMLParser')
_NameStack             &QNamePositions,PROTECTED
_NameStackAutoDispose  BYTE,PRIVATE
_trace                 BYTE,PROTECTED
_buffer                &STRING,PROTECTED
XmlNotify              &IXmlNotify,PROTECTED
    ! parse buffer and offsets of where we are parsing
_parseLength           LONG,PROTECTED
    ! current position in parse buffer
_current               LONG,PROTECTED

    ! tag, attributes and data position information
_firstTagStart         LONG,PROTECTED
_firstTagEnd           LONG,PROTECTED
   
_lastTagStart          LONG,PROTECTED
_lastTagEnd            LONG,PROTECTED
   
_nameStart             LONG,PROTECTED
_nameEnd               LONG,PROTECTED
_attrStart             LONG,PROTECTED
_attrEnd               LONG,PROTECTED
_valueStart            LONG,PROTECTED
_valueEnd              LONG,PROTECTED
_tagType               BYTE,PROTECTED
_ProgressControl       SIGNED,PROTECTED
_OverPassParameters    BYTE,PROTECTED
_CaselessName          BYTE,PROTECTED

Construct              PROCEDURE
Destruct               PROCEDURE
Parse                  PROCEDURE(*STRING xmlStream,IXmlNotify XmlNotify),BYTE,PROC
SetTrace               PROCEDURE(BYTE trueFalse = 1)
Trace                  PROCEDURE(STRING methodName, STRING parameters)
SetCaseLessName        PROCEDURE(BYTE trueFalse = 1)
GetCaseLessName        PROCEDURE(),BYTE
SetProgressControl     PROCEDURE(SIGNED ProgressControl)
GetProgressControl     PROCEDURE(),SIGNED
SetPassParameters      PROCEDURE(BYTE PassParemeters)
GetPassParameters      PROCEDURE(),BYTE
NotifyProgressControl  PROCEDURE(),PROTECTED
Parse                  PROCEDURE(),BYTE,PROTECTED
Parse                  PROCEDURE(*STRING xmlStream,IXmlNotify XmlNotify,LONG parseStart, LONG parseLength, QNamePositions qpos),BYTE,PROTECTED
ParseNodes             PROCEDURE(),BYTE,PROC,PROTECTED
Reset                  PROCEDURE(),PROTECTED
ResetTagPositions      PROCEDURE(LONG Start=-1),PROTECTED
ClearPos               PROCEDURE(),PROTECTED
getTagLength           PROCEDURE(),LONG,PROTECTED
getLastTagLength       PROCEDURE(),LONG,PROTECTED
getNameLength          PROCEDURE(),LONG,PROTECTED
getAttributesLength    PROCEDURE(),LONG,PROTECTED
getValueLength         PROCEDURE(),LONG,PROTECTED
hasTag                 PROCEDURE(),BYTE,PROTECTED
isClosedTag            PROCEDURE(LONG tagStart,LONG endTagStart),BYTE,PROTECTED
GetTagType             PROCEDURE(),BYTE,PROTECTED
SetTagType             PROCEDURE(),PROTECTED
hasNullTag             PROCEDURE(),BYTE,PROTECTED
hasLastTag             PROCEDURE(),BYTE,PROTECTED
hasName                PROCEDURE(),BYTE,PROTECTED
hasAttributes          PROCEDURE(),BYTE,PROTECTED
hasValue               PROCEDURE(),BYTE,PROTECTED
getTag                 PROCEDURE(),STRING,PROTECTED
getName                PROCEDURE(),STRING,PROTECTED
getOriginalName        PROCEDURE(),STRING,PROTECTED
getValue               PROCEDURE(),STRING,PROTECTED
getAttributes          PROCEDURE(),STRING,PROTECTED
valueHasTag            PROCEDURE(),BYTE,PROTECTED
ParseName              PROCEDURE(),BYTE,PROTECTED
ParseAttributes        PROCEDURE(),BYTE,PROC,PROTECTED
ParseValue             PROCEDURE(),BYTE,PROC,PROTECTED
NotifyFoundNode        PROCEDURE(),PROTECTED
NotifyFoundElement     PROCEDURE(),PROTECTED
NotifyStartElement     PROCEDURE(),PROTECTED
NotifyEndElement       PROCEDURE(),PROTECTED
NotifyFoundAttributes  PROCEDURE(),PROTECTED
NotifyFoundComment     PROCEDURE(),PROTECTED
NotifyFoundHeader      PROCEDURE(),PROTECTED
GetCurrentElementName  PROCEDURE(),STRING
PushCurrentElementName PROCEDURE(),PROTECTED
PopCurrentElementName  PROCEDURE(),PROTECTED
!!utility methods
getLength              PROCEDURE( long startPos,long endPos ),LONG,PROTECTED
find                   PROCEDURE( string srchStr, long offset=0, long length = 0 ),LONG,PROTECTED
                END

XMLFileParser       CLASS,IMPLEMENTS(IXmlNotify),TYPE,MODULE('QuickXMLParser.CLW'),LINK('QuickXMLParser')
_QOfText                     &XMLQueueOfTextType,PROTECTED
_XMLParser                   &XMLParser,PRIVATE
_trace                       BYTE,PROTECTED
_CaselessName                BYTE,PROTECTED
Level                        LONG,PROTECTED
autoClear                    BYTE(1),PROTECTED
SetAutoClear                 PROCEDURE(BYTE autoClear=1)
xmlStream                    &STRING,PROTECTED
GetXmlStream                 PROCEDURE(),STRING
ClearXmlStream               PROCEDURE()
ProgressControl              USHORT,PROTECTED
_FileName                    STRING(256)
GetFileName                  PROCEDURE(),STRING
_PassParemeters              BYTE,PROTECTED
SetPassParameters            PROCEDURE(BYTE PassParemeters)
GetPassParameters            PROCEDURE(),BYTE
SetProgressControl           PROCEDURE(USHORT progressControl)
Parse                        PROCEDURE(STRING fileName)
ShowXMLFile                  PROCEDURE(STRING fileName)
ToQueue                      PROCEDURE(XMLQueueOfTextType Q,STRING fileName)
SetTrace                     PROCEDURE(BYTE trueFalse = 1)
Trace                        PROCEDURE(STRING methodName, STRING parameters),PROTECTED
Construct                    PROCEDURE
Destruct                     PROCEDURE
SetCaseLessName              PROCEDURE(BYTE trueFalse = 1)
GetCaseLessName              PROCEDURE(),BYTE
GetCurrentElementName        PROCEDURE(),STRING,PROTECTED
ToQueueXmlNotifyFoundNode           PROCEDURE( STRING name, STRING  attributes ),PROTECTED
ToQueueXmlNotifyFoundElement        PROCEDURE( STRING name, STRING  value, STRING  attributes ),PROTECTED
ToQueueXmlNotifyStartElement        PROCEDURE( STRING name, STRING  value, STRING  attributes ),PROTECTED
ToQueueXmlNotifyEndElement          PROCEDURE( STRING name, STRING  value, STRING  attributes ),PROTECTED
ToQueueXmlNotifyFoundComment        PROCEDURE( STRING Comment),PROTECTED
ToQueueXmlNotifyFoundHeader         PROCEDURE( STRING attributes),PROTECTED
ToQueueXmlNotifyFoundAttribute      PROCEDURE( STRING tagname, STRING name, STRING  value ),PROTECTED
XmlNotifyFoundNode           PROCEDURE(STRING name, STRING  attributes),VIRTUAL
XmlNotifyCloseNode           PROCEDURE(STRING name),VIRTUAL
XmlNotifyFoundElement        PROCEDURE(STRING name, STRING  value, STRING  attributes),VIRTUAL
XmlNotifyCloseElement        PROCEDURE(STRING name),VIRTUAL
XmlNotifyStartElement        PROCEDURE(STRING name, STRING  value, STRING  attributes),VIRTUAL
XmlNotifyEndElement          PROCEDURE(STRING name, STRING  value, STRING  attributes),VIRTUAL
XmlNotifyFoundComment        PROCEDURE(STRING Comment),VIRTUAL
XmlNotifyFoundHeader         PROCEDURE(STRING attributes),VIRTUAL
XmlNotifyCloseHeader         PROCEDURE(),VIRTUAL
XmlNotifyFoundAttribute      PROCEDURE(STRING tagname, STRING name, STRING  value),VIRTUAL
                    END



