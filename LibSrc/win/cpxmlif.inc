
  omit('_EndOfInclude_', _cpXMPifIncluded_)
_cpXMPifIncluded_ equate(1)


cint                    equate(signed)
cbool                   equate(byte)


DOMStyle                equate(unsigned)


                        itemize, pre(DOMStyle)
ADO_26                    equate
ADO_NET                   equate
                        end




  map
    module('')
!===  DOM  ======
CreateDOMImplementation     procedure, *DOMImplementation, name('_CreateDOMImplementation@0'), pascal, raw
DestroyDOMImplementation    procedure(*DOMImplementation pDOMImplementation), name('_DestroyDOMImplementation@4'), pascal, raw
CreateDocumentFromFile      procedure(const *cstring uri, ULONG poolSize = 251), *Document, name('_CreateDocument@8'), pascal, raw
CreateDocumentFromXML       procedure(const *cstring xml, ULONG poolSize = 251), *Document, name('_CreateDocument2@8'), pascal, raw
CreateDOMWriter             procedure, *DOMWriter, name('_CreateDOMWriter@0'), pascal, raw
DestroyDOMWriter            procedure(*DOMWriter pDOMWriter), name('_DestroyDOMWriter@4'), pascal, raw
!===  SAX  ======
XMLStringToSAX              procedure(const *cstring xml, *ISAXCallback saxcallback), byte, proc, name('_XMLStringToSAX@8'), pascal, raw
XMLFileToSAX                procedure(const *cstring path, *ISAXCallback saxcallback), byte, proc, name('_XMLFileToSAX@8'), pascal, raw
    end
  end


                        itemize, pre(Format)
AS_IS                     equate(1)
CANONICAL                 equate(2)
REFORMATTED               equate(4)
                        end

NEWLINE_DEFAULT         cstring('')
NEWLINE_CR              cstring('CR')
NEWLINE_CRLF            cstring('CR-LF')
NEWLINE_LF              cstring('LF')

DOMWriter               interface                                                                                                                       .;map;module('DOMWriter')
setEncoding               procedure(*DOMWriter, const *cstring encoding), cbool                                                                         , name('_setEncoding@8'), pascal, raw, proc
setEncoding               procedure(*DOMWriter, const *cstring encoding, bool assumeISO88591), cbool                                                    , name('_setEncoding@12'), pascal, raw, proc
getEncoding               procedure(*DOMWriter), *cstring                                                                                               , name('_getEncoding@4'), pascal, raw
getLastEncoding           procedure(*DOMWriter), *cstring                                                                                               , name('_getLastEncoding@4'), pascal, raw
setFormat                 procedure(*DOMWriter, unsigned format), cbool                                                                                 , name('_setFormat@8'), pascal, raw, proc
getFormat                 procedure(*DOMWriter), unsigned                                                                                               , name('_getFormat@4'), pascal, raw
setNewLine                procedure(*DOMWriter, const *cstring newLine), cbool                                                                          , name('_setNewLine@8'), pascal, raw, proc
getNewLine                procedure(*DOMWriter), *cstring                                                                                               , name('_getNewLine@4'), pascal, raw
writeNode                 procedure(*DOMWriter, *Node pNode), *cstring                                                                                  , name('_writeNode@8'), pascal, raw
writeNode                 procedure(*DOMWriter, const *cstring systemId, *Node pNode), cbool                                                            , name('_writeNode2@12'), pascal, raw, proc
                        end                                                                                                                             ;end
                        
DOMImplementation       interface                                                                                                                       .;map;module('DOMImplementation')
hasFeature                procedure(*DOMImplementation, const *cstring feature, const *cstring version), cbool                                          , name('_hasFeature@12'), pascal, raw
! DOM Level 2      
createDocumentType        procedure(*DOMImplementation, const *cstring qualifiedName, const *cstring publicId, const *cstring systemId), *DocumentType  , name('_createDocumentType@16'), pascal, raw
createDocument            procedure(*DOMImplementation, const *cstring namespaceURI, const *cstring qualifiedName, *DocumentType doctype, ULONG poolSize = 251), *Document    , name('_createDocument@20'), pascal, raw
                        end                                                                                                                             ;end

DOMObject               interface                                                                                                                       .;map;module('DOMObject');
addRef                    procedure(*DOMObject), cbool                                                                                                  , name('_addRef@4'), pascal, raw, proc
duplicate                 procedure(*DOMObject), cbool                                                                                                  , name('_duplicate@4'), pascal, raw, proc
release                   procedure(*DOMObject), cbool                                                                                                  , name('_release@4'), pascal, raw, proc
countReferences           procedure(*DOMObject), cint                                                                                                   , name('_countReferences@4'), pascal, raw
                        end                                                                                                                             ;end

NodeList                interface(DOMObject)                                                                                                            .;map;module('NodeList')
item                      procedure(*NodeList, unsigned index), *Node                                                                                   , name('_item@8'), pascal, raw
getLength                 procedure(*NodeList), unsigned                                                                                                , name('_getLength2@4'), pascal, raw
                        end                                                                                                                             ;end

NamedNodeMap            interface(DOMObject)                                                                                                            .;map;module('NamedNodeMap')
getNamedItem              procedure(*NamedNodeMap, const *cstring name), *Node                                                                          , name('_getNamedItem@8'), pascal, raw
setNamedItem              procedure(*NamedNodeMap, *Node arg), *Node                                                                                    , name('_setNamedItem@8'), pascal, raw
removeNamedItem           procedure(*NamedNodeMap, const *cstring name), *Node                                                                          , name('_removeNamedItem@8'), pascal, raw
item                      procedure(*NamedNodeMap, unsigned index), *Node                                                                               , name('_item2@8'), pascal, raw
getLength                 procedure(*NamedNodeMap), unsigned                                                                                            , name('_getLength3@4'), pascal, raw
! DOM Level 2
getNamedItemNS            procedure(*NamedNodeMap, const *cstring namespaceURI, const *cstring localName), *Node                                        , name('_getNamedItemNS@12'), pascal, raw
setNamedItemNS            procedure(*NamedNodeMap, *Node arg), *Node                                                                                    , name('_setNamedItemNS@8'), pascal, raw
removeNamedItemNS         procedure(*NamedNodeMap, const *cstring namespaceURI, const *cstring localName), *Node                                        , name('_removeNamedItemNS@12'), pascal, raw
                        end                                                                                                                             ;end

                        itemize(1), pre(Node)
ELEMENT                   equate
ATTRIBUTE                 equate
TEXT                      equate
CDATA_SECTION             equate
ENTITY_REFERENCE          equate
ENTITY                    equate
PROCESSING_INSTRUCTION    equate
COMMENT                   equate
DOCUMENT                  equate
DOCUMENT_TYPE             equate
DOCUMENT_FRAGMENT         equate
NOTATION                  equate
                        end

Node                    interface(DOMObject)                                                                                                            ;end;map;module('Node')
getNodeName               procedure(*Node), *cstring                                                                                                    , name('_getNodeName@4'), pascal, raw
getNodeValue              procedure(*Node), *cstring                                                                                                    , name('_getNodeValue@4'), pascal, raw
setNodeValue              procedure(*Node, const *cstring value), cbool                                                                                 , name('_setNodeValue@8'), pascal, raw, proc
getNodeType               procedure(*Node), unsigned                                                                                                    , name('_getNodeType@4'), pascal, raw
getParentNode             procedure(*Node), *Node                                                                                                       , name('_getParentNode@4'), pascal, raw
getChildNodes             procedure(*Node), *NodeList                                                                                                   , name('_getChildNodes@4'), pascal, raw
getFirstChild             procedure(*Node), *Node                                                                                                       , name('_getFirstChild@4'), pascal, raw
getLastChild              procedure(*Node), *Node                                                                                                       , name('_getLastChild@4'), pascal, raw
getPreviousSibling        procedure(*Node), *Node                                                                                                       , name('_getPreviousSibling@4'), pascal, raw
getNextSibling            procedure(*Node), *Node                                                                                                       , name('_getNextSibling@4'), pascal, raw
getAttributes             procedure(*Node), *NamedNodeMap                                                                                               , name('_getAttributes@4'), pascal, raw
getOwnerDocument          procedure(*Node), *Document                                                                                                   , name('_getOwnerDocument@4'), pascal, raw
insertBefore              procedure(*Node, *Node newChild, *Node refChild), *Node, proc                                                                 , name('_insertBefore@12'), pascal, raw
replaceChild              procedure(*Node, *Node newChild, *Node oldChild), *Node                                                                       , name('_replaceChild@12'), pascal, raw
removeChild               procedure(*Node, *Node oldChild), *Node, proc                                                                                 , name('_removeChild@8'), pascal, raw
appendChild               procedure(*Node, *Node newChild), *Node                                                                                       , name('_appendChild@8'), pascal, raw, proc
hasChildNodes             procedure(*Node), cbool                                                                                                       , name('_hasChildNodes@4'), pascal, raw
cloneNode                 procedure(*Node, cbool deep), *Node                                                                                           , name('_cloneNode@8'), pascal, raw
! DOM Level 2
normalize                 procedure(*Node), cbool                                                                                                       , name('_normalize@4'), pascal, raw, proc
isSupported               procedure(*Node, const *cstring feature, const *cstring version), cbool                                                       , name('_isSupported@12'), pascal, raw
getNamespaceURI           procedure(*Node), *cstring                                                                                                    , name('_getNamespaceURI@4'), pascal, raw
getPrefix                 procedure(*Node), *cstring                                                                                                    , name('_getPrefix@4'), pascal, raw
getLocalName              procedure(*Node), *cstring                                                                                                    , name('_getLocalName@4'), pascal, raw
hasAttributes             procedure(*Node), cbool                                                                                                       , name('_hasAttributes@4'), pascal, raw
                        end                                                                                                                             ; end

Attr                    interface(Node)                                                                                                                 .;map;module('Attr')
getName                   procedure(*Attr), *cstring                                                                                                    , name('_getName@4'), pascal, raw
getSpecified              procedure(*Attr), cbool                                                                                                       , name('_getSpecified@4'), pascal, raw
getValue                  procedure(*Attr), *cstring                                                                                                    , name('_getValue@4'), pascal, raw
setValue                  procedure(*Attr, const *cstring value), cbool                                                                                 , name('_setValue@8'), pascal, raw, proc
getOwnerElement           procedure(*Attr), *Element                                                                                                    , name('_getOwnerElement@4'), pascal, raw
                        end                                                                                                                             ;end

CharacterData           interface(Node)                                                                                                                 .;map;module('CharacterData')
getData                   procedure(*CharacterData), *cstring                                                                                           , name('_getData@4'), pascal, raw
setData                   procedure(*CharacterData, const *cstring data), cbool                                                                         , name('_setData@8'), pascal, raw, proc
getLength                 procedure(*CharacterData), unsigned                                                                                           , name('_getLength@4'), pascal, raw
substringData             procedure(*CharacterData, unsigned offset, unsigned count), *cstring                                                          , name('_substringData@12'), pascal, raw
appendData                procedure(*CharacterData, const *cstring arg), cbool                                                                          , name('_appendData@8'), pascal, raw, proc
insertData                procedure(*CharacterData, unsigned offset, const *cstring arg), cbool                                                         , name('_insertData@12'), pascal, raw, proc
deleteData                procedure(*CharacterData, unsigned offset, unsigned count), cbool                                                             , name('_deleteData@12'), pascal, raw, proc
replaceData               procedure(*CharacterData, unsigned offset, unsigned count, const *cstring arg), cbool                                         , name('_replaceData@16'), pascal, raw, proc
                        end                                                                                                                             ;end

Notation                interface(Node)                                                                                                                 .;map;module('Notation')
getPublicId               procedure, *cstring                                                                                                           , name('_getPublicID@4'), pascal, raw
getSystemId               procedure, *cstring                                                                                                           , name('_getSystemID@4'), pascal, raw
                        end                                                                                                                             ;end

Text                    interface(CharacterData)                                                                                                        .;map;module('Text')
splitText                 procedure(*Text, unsigned offset), *Text                                                                                      , name('_splitText@8'), pascal, raw
                        end                                                                                                                             ;end

Comment                 interface(CharacterData)
                        end

CDATASection            interface(Text)
                        end

EntityReference         interface(Node)                                                                                                                 .;map;module('EntityReference')
                        end                                                                                                                             ;end

Entity                  interface(Node)                                                                                                                 .;map;module('Entity')
getPublicId               procedure(*Entity), *cstring                                                                                                  , name('_getPublicId3@4'), pascal, raw
getSystemId               procedure(*Entity), *cstring                                                                                                  , name('_getSystemId3@4'), pascal, raw
getNotationName           procedure(*Entity), *cstring                                                                                                  , name('_getNotationName@4'), pascal, raw
                        end                                                                                                                             ;end

ProcessingInstruction   interface(Node)                                                                                                                 .;map;module('ProcessingInstruction')
getTarget                 procedure(*ProcessingInstruction), *cstring                                                                                   , name('_getTarget@4'), pascal, raw
getData                   procedure(*ProcessingInstruction), *cstring                                                                                   , name('_getData2@4'), pascal, raw
setData                   procedure(*ProcessingInstruction, const *cstring data), cbool                                                                 , name('_setData@8'), pascal, raw, proc
                        end                                                                                                                             ;end

DocumentType            interface(Node)                                                                                                                 .;map;module('DocumentType')
getName                   procedure(*DocumentType), *cstring                                                                                            , name('_getName2@4'), pascal, raw
getEntities               procedure(*DocumentType), *NamedNodeMap                                                                                       , name('_getEntities@4'), pascal, raw
getNotations              procedure(*DocumentType), *NamedNodeMap                                                                                       , name('_getNotations@4'), pascal, raw
! DOM Level 2
getPublicId               procedure(*DocumentType), *cstring                                                                                            , name('_getPublicId2@4'), pascal, raw
getSystemId               procedure(*DocumentType), *cstring                                                                                            , name('_getSystemId2@4'), pascal, raw
getInternalSubset         procedure(*DocumentType), *cstring                                                                                            , name('_getInternalSubset@4'), pascal, raw
                        end                                                                                                                             ;end

Element                 interface(Node)                                                                                                                 .;map;module('Element')
getTagName                procedure(*Element), *cstring                                                                                                 , name('_getTagName@4'), pascal, raw
getAttribute              procedure(*Element, const *cstring name), *cstring                                                                            , name('_getAttribute@8'), pascal, raw
setAttribute              procedure(*Element, const *cstring name, const *cstring value), cbool                                                         , name('_setAttribute@12'), pascal, raw, proc
removeAttribute           procedure(*Element, const *cstring name), cbool                                                                               , name('_removeAttribute@8'), pascal, raw, proc
getAttributeNode          procedure(*Element, const *cstring name), *Attr                                                                               , name('_getAttributeNode@8'), pascal, raw
setAttributeNode          procedure(*Element, *Attr newAttr), *Attr                                                                                     , name('_setAttributeNode@8'), pascal, raw, proc
removeAttributeNode       procedure(*Element, *Attr oldAttr), *Attr                                                                                     , name('_removeAttributeNode@8'), pascal, raw
getElementsByTagName      procedure(*Element, const *cstring name), *NodeList                                                                           , name('_getElementsByTagName@8'), pascal, raw
! DOM Level 2
getAttributeNS            procedure(*Element, const *cstring namespaceURI, const *cstring localName), *cstring                                          , name('_getAttributeNS@12'), pascal, raw
setAttributeNS            procedure(*Element, const *cstring namespaceURI, const *cstring qualifiedName, const *cstring value), cbool                   , name('_setAttributeNS@16'), pascal, raw, proc
removeAttributeNS         procedure(*Element, const *cstring namespaceURI, const *cstring localName), cbool                                             , name('_removeAttributeNS@12'), pascal, raw, proc
getAttributeNodeNS        procedure(*Element, const *cstring namespaceURI, const *cstring localName), *Attr                                             , name('_getAttributeNodeNS@12'), pascal, raw
setAttributeNodeNS        procedure(*Element, *Attr newAttr), *Attr                                                                                     , name('_setAttributeNodeNS@8')                                                      , pascal, raw
hasAttribute              procedure(*Element, const *cstring name), cbool                                                                               , name('_hasAttribute@8'), pascal, raw
hasAttributeNS            procedure(*Element, const *cstring namespaceURI, const *cstring localName), cbool                                             , name('_hasAttributeNS@12'), pascal, raw
                        end                                                                                                                             ;end

Document                interface(Node)                                                                                                                 .;map;module('Document')
getDoctype                procedure, *DocumentType                                                                                                      , name('_getDocumentType@4'), pascal, raw
getImplementation         procedure, *DOMImplementation                                                                                                 , name('_getImplementation@4'), pascal, raw
getDocumentElement        procedure(*Document), *Element                                                                                                , name('_getDocumentElement@4'), pascal, raw
createElement             procedure(*Document, const *cstring tagName), *Element                                                                        , name('_createElement@8'), pascal, raw
createDocumentFragment    procedure(*Document), *DocumentFragment                                                                                       , name('_createDocumentFragment@4'), pascal, raw
createTextNode            procedure(*Document, const *cstring data), *Text                                                                              , name('_createTextNode@8'), pascal, raw
createComment             procedure(*Document, const *cstring data), *Comment                                                                           , name('_createComment@8'), pascal, raw
createCDATASection        procedure(*Document, const *cstring data), *CDATASection                                                                      , name('_createCDATASection@8'), pascal, raw
createProcessingInstruction procedure(*Document, const *cstring target, const *cstring data), *ProcessingInstruction                                    , name('_createProcessingInstruction@12'), pascal, raw
createAttribute           procedure(*Document, const *cstring name), *Attr                                                                              , name('_createAttribute@8'), pascal, raw
createEntityReference     procedure(*Document, const *cstring name), *EntityReference                                                                   , name('_createEntityReference@8'), pascal, raw
getElementsByTagName      procedure(*Document, const *cstring name), *NodeList                                                                          , name('_getElementsByTagName2@8'), pascal, raw
importNode                procedure(*Document, *Node importedNode, cbool deep), *Node                                                                   , name('_importNode@12'), pascal, raw
createElementNS           procedure(*Document, const *cstring namespaceURI, const *cstring qualifiedName), *Element                                     , name('_createElementNS@12'), pascal, raw
createAttributeNS         procedure(*Document, const *cstring namespaceURI, const *cstring qualifiedName), *Attr                                        , name('_createAttributeNS@12'), pascal, raw
getElementsByTagNameNS    procedure(*Document, const *cstring namespaceURI, const *cstring localName), *NodeList                                        , name('_getElementsByTagNameNS@12'), pascal, raw
getElementById            procedure(*Document, const *cstring elementId), *Element                                                                      , name('_getElementById@8'), pascal, raw
                        end                                                                                                                             ;end

DocumentFragment        interface(Node)
                        end

!===  SAX  ========
ISAXCallback            interface, com
SetDocumentLocator        procedure(const *cstring PublicID, const *cstring SystemID)
NotationDecl              procedure(const *cstring name, const *cstring publicId, const *cstring systemId)
UnparsedEntityDecl        procedure(const *cstring name, const *cstring publicId, const *cstring systemId, const *cstring notationName)
StartDocument             procedure
EndDocument               procedure
StartElement              procedure(const *cstring name)
Attribute                 procedure(const *cstring name, const *cstring type, const *cstring value)
EndElement                procedure(const *cstring name)
Characters                procedure(const *cstring chars)
IgnorableWhitespace       procedure(const *cstring chars)
ProcessingInstruction2    procedure(const *cstring target, const *cstring data)
Warning                   procedure(const *cstring warning)
Error                     procedure(const *cstring error)
FatalError                procedure(const *cstring fatalError)
StartDTD                  procedure(const *cstring name, const *cstring publicId, const *cstring systemId)
EndDTD                    procedure
StartEntity               procedure(const *cstring name)
EndEntity                 procedure(const *cstring name)
StartCDATA                procedure
EndCDATA                  procedure
Comment                   procedure(const *cstring commentText)
StartNamespaceDeclScope   procedure(const *cstring prefix, const *cstring uri)
EndNamespaceDeclScope     procedure(const *cstring prefix)
                        end




  _EndOfInclude_

