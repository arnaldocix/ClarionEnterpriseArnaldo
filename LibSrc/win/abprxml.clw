    MEMBER
INCLUDE('ABPRXML.INC'),ONCE
INCLUDE('ABERROR.INC'),ONCE
    PRAGMA('link(EXP_XML.ICO)')
    MAP
      INCLUDE('CWUTIL.INC'),ONCE
    END

XMLWriter.WriteLine               PROCEDURE(STRING pLine)
 CODE
    IF SELF.UseCRLF
       SELF.Output.WriteLine(pLine)
    ELSE
       SELF.Output.WriteStr(pLine)
    END

XMLWriter.CONSTRUCT               PROCEDURE()
 CODE
    SELF.QTag &= NEW(QueueTags)
    SELF.Output &= NEW(TargetGenerator)
    SELF.QHeaderAttr &= NEW(QueueTagAttributes)
    SELF.UseCRLF = True
    SELF.RootAdded = False
    SELF.SetForceXMLHeader(True)
    SELF.Encoding = ''
    SELF.Output.Init()

XMLWriter.DESTRUCT                PROCEDURE()
 CODE
    SELF.Empty()
    DISPOSE(SELF.QHeaderAttr)
    DISPOSE(SELF.QTag)
    DISPOSE(SELF.Output)
    IF NOT SELF.ppScapedString &= NULL
       DISPOSE(SELF.ppScapedString)
    END


XMLWriter.ChildDESTRUCT                PROCEDURE(QueueTags pQTag)
lIndex  LONG
 CODE
 LOOP lIndex=RECORDS(pQTag) TO 1 BY -1
      GET(pQTag,lIndex)
      IF NOT ERRORCODE() THEN
         IF NOT pQTag.Value &=NULL
            DISPOSE(pQTag.Value)  
         END
         IF pQTag.TagType = TagType:Tag THEN
            SELF.ChildDESTRUCT(pQTag.Child)
            DISPOSE(pQTag.Child)
            FREE(pQTag.Attrs)
            DISPOSE(pQTag.Attrs)
         END
         DELETE(pQTag)
      END
 END

XMLWriter.Init                    PROCEDURE(<STRING TargetFileName>)
 CODE
    SELF.Empty()
    IF NOT OMITTED(2) THEN
       SELF.Output.Init(TargetFileName)
    END
    
XMLWriter.Empty                   PROCEDURE()
 CODE
    FREE(SELF.QHeaderAttr)
    SELF.ChildDESTRUCT(SELF.QTag)
    FREE(SELF.QTag)   
    SELF.Output.Init()
    SELF.RootAdded = False
    SELF.SetForceXMLHeader(True)
    SELF.Encoding = ''
    
XMLWriter.MakeValidName           PROCEDURE(STRING pName)
lIndex  SHORT
 CODE
    LOOP lIndex=1 TO LEN(pName)
      CASE VAL(pName[lIndex])
      OF  VAL('.')
      OROF VAL('[')
      OROF VAL(']')
      OROF VAL(' ')
      OROF VAL('{{') 
      OROF VAL('}')
      OROF VAL('+')
      OROF VAL('=')
      OROF VAL('''') 
      OROF VAL('"')
      OROF VAL('!')
      OROF VAL('@')
      OROF VAL('#')
      OROF VAL('$')
      OROF VAL('%')
      OROF VAL('^')
      OROF VAL('&')
      OROF VAL('*')
      OROF VAL('(')
      OROF VAL(')')
      OROF VAL(';')
      OROF VAL(',')
      OROF VAL('<<')
      OROF VAL('>')
      OROF VAL('?')
      OROF VAL('/')
      OROF VAL('|')
      OROF VAL('\')
      OROF VAL('~')
      OROF VAL('`')
        pName[lIndex] = '_'
      OF VAL(':')
        IF NOT SELF.SupportNameSpaces
          pName[lIndex] = '_'
        END
      END
    END
    IF LEN(pName)
       CASE VAL(pName[1])
       OF VAL(0) TO VAL(9)
          RETURN '_' & pName
       ELSE
          RETURN pName
       END
    ELSE
       RETURN pName
    END

XMLWriter.EscapeValues            PROCEDURE(STRING pValue)
CharToReplace   STRING(1)
ReplacedBy      STRING(10)
LenReplacedBy   SHORT
lIndex          SHORT
lLastIndex      SHORT

lSLen           SHORT
lOriginalSLen   SHORT
lReplaced       BYTE
SAux1           &STRING
SAux2           &STRING

 CODE

    SAux1 &= SELF.ppScapedString

    IF NOT SAux1 &= NULL
       DISPOSE(SAux1)
    END

    lSLen = LEN(pValue)
    lOriginalSLen = lSLen
    SAux1 &= NEW(STRING(lSLen+20))
    SAux1 = pValue

    CharToReplace= '&'
    ReplacedBy   = '&amp;'
    LenReplacedBy= 5
    Do ReplaceOneChar

    CharToReplace= '>'
    ReplacedBy   = '&gt;'
    LenReplacedBy= 4
    Do ReplaceOneChar

    CharToReplace= '<'
    ReplacedBy   = '&lt;'
    LenReplacedBy= 4
    Do ReplaceOneChar

    CharToReplace= ''''
    ReplacedBy   = '&apos;'
    LenReplacedBy= 6
    Do ReplaceOneChar

    CharToReplace= '"'
    ReplacedBy   = '&quot;'
    LenReplacedBy= 6
    Do ReplaceOneChar

    SELF.ppScapedString &= SAux1
    RETURN SAux1[1:lSLen]

ReplaceOneChar   ROUTINE
    lLastIndex = 1
    LOOP
       lIndex = INSTRING(CharToReplace,SAux1,1,lLastIndex)
       IF lIndex THEN
          lReplaced = True
          IF CharToReplace='&'
             IF lSLen-lIndex>=4
                IF SAux1[(lIndex):(lIndex+3)]='&gt;' OR SAux1[(lIndex):(lIndex+3)]='&lt;'
                   lReplaced = False
                   lLastIndex=lIndex+4
                END
             END
             IF lSLen-lIndex>=5
                IF SAux1[(lIndex):(lIndex+4)]='&amp;'
                   lReplaced = False
                   lLastIndex=lIndex+5
                END
             END
             IF lSLen-lIndex>=6
                IF SAux1[(lIndex):(lIndex+5)]='&apos;' OR SAux1[(lIndex):(lIndex+5)]='&quot;'
                   lReplaced = False
                   lLastIndex=lIndex+6
                END
             END
          END
          IF lReplaced
             IF (lSLen+LenReplacedBy)>(lOriginalSLen+20) THEN
                SAux2 &= NEW(STRING(lSLen+500))
                SAux2 = ''
                SAux2 = SAux1[1:lSLen]
                DISPOSE(SAux1)
                SAux1 &= SAux2[1:lSLen]
             END
             SAux1 = SAux1[1:(lIndex-1)] & ReplacedBy[1:LenReplacedBy] & SAux1[(lIndex+1):(lSLen)]
             lSLen += LenReplacedBy
             lLastIndex=lIndex+LenReplacedBy
          END
       ELSE
          BREAK
       END
    END

XMLWriter.SetFileName              PROCEDURE(STRING pFileName)
 CODE
    SELF.Output.SetOriginalFileName(pFileName)

XMLWriter.GetFileName              PROCEDURE()
 CODE
    RETURN SELF.Output.GetOriginalFileName()

XMLWriter.SetRootTag              PROCEDURE(STRING pRootTag)
 CODE
    SELF.RootTag = SELF.MakeValidName(pRootTag)

XMLWriter.GetRootTag              PROCEDURE()
 CODE
    RETURN SELF.RootTag

XMLWriter.SetSupportNameSpaces    PROCEDURE(BYTE pTrue=1)
 CODE
    SELF.SupportNameSpaces   = pTrue

XMLWriter.GetSupportNameSpaces    PROCEDURE()
 CODE
    RETURN SELF.SupportNameSpaces

XMLWriter.SetForceXMLHeader       PROCEDURE(BYTE pTrue=1)
 CODE
    SELF.ForceXMLHeader   = pTrue

XMLWriter.GetForceXMLHeader       PROCEDURE()
 CODE
    RETURN SELF.ForceXMLHeader

XMLWriter.SetEncoding             PROCEDURE(STRING pEncoding)
 CODE
    SELF.Encoding = pEncoding

XMLWriter.GetEncoding             PROCEDURE()
 CODE
    RETURN SELF.Encoding

XMLWriter.SetXSL                  PROCEDURE(STRING pXSLFileName)
 CODE
    SELF.XSL = pXSLFileName

XMLWriter.GetXSL                  PROCEDURE()
 CODE
    RETURN SELF.XSL

XMLWriter.SetUseXMLasXSLName          PROCEDURE(BYTE pValue)
 CODE
    SELF.UseXMLasXSLName = pValue

XMLWriter.GetUseXMLasXSLName          PROCEDURE()
 CODE
    RETURN SELF.UseXMLasXSLName

XMLWriter.SetUseCRLF              PROCEDURE(BYTE pTrue=1)
 CODE
    SELF.UseCRLF = pTrue

XMLWriter.GetUseCRLF              PROCEDURE()
 CODE
    RETURN SELF.UseCRLF

XMLWriter.AddComment              PROCEDURE(STRING pComment)
 CODE
       SELF.QTag.Name     = pComment
       SELF.QTag.TagType  = TagType:Comment

       SELF.QTag.Value   &= NEW(STRING(1))           ! GJx added
       SELF.QTag.Value    = ' '                      ! GJx added
       ADD(SELF.QTag)

XMLWriter.RemoveComments          PROCEDURE()
lIndex  LONG
 CODE
    lIndex = RECORDS(SELF.QTag)
    LOOP     
         IF lIndex = 0 THEN BREAK.
         GET(SELF.QTag,lIndex)
         IF ERRORCODE() THEN BREAK.
         IF SELF.QTag.TagType  = TagType:Comment
            DELETE(SELF.QTag)
         END
         lIndex -= 1
    END

XMLWriter.AddXMLHeaderAttribute       PROCEDURE(STRING pName,STRING pValue)
lIndex  LONG
lFound  BYTE
 CODE
    IF NOT CLIP(pName) THEN RETURN.
    lFound = False
    LOOP lIndex = 1 to RECORDS(SELF.QHeaderAttr)
         GET(SELF.QHeaderAttr,lIndex)
         IF ERRORCODE() THEN BREAK.
         IF SELF.QHeaderAttr.Name = pName THEN
            lFound = True
            SELF.QHeaderAttr.Value= pValue
            PUT(SELF.QHeaderAttr)
            BREAK
         END
    END
    IF lFound = False
       SELF.QHeaderAttr.Name = pName
       SELF.QHeaderAttr.Value= pValue
       ADD(SELF.QHeaderAttr)
    END

XMLWriter.RemoveXMLHeaderAttribute PROCEDURE(STRING pName)
lIndex  LONG
 CODE
    IF NOT CLIP(pName) THEN RETURN.
    LOOP lIndex = 1 to RECORDS(SELF.QHeaderAttr)
         GET(SELF.QHeaderAttr,lIndex)
         IF ERRORCODE() THEN BREAK.
         IF SELF.QHeaderAttr.Name = pName THEN
            DELETE(SELF.QHeaderAttr)
            BREAK
         END
    END

XMLWriter.AddTag                  PROCEDURE(STRING pName,STRING pValue,BYTE pIsCData=0,<STRING pParent>)
lAdded  BYTE
 CODE
    IF NOT CLIP(pName) THEN RETURN.
    IF OMITTED(5) OR CLIP(pParent)='' THEN
       SELF.QTag.Name     = pName
       SELF.QTag.Value   &= NEW(STRING(LEN(pValue)))
       SELF.QTag.Value    = pValue
       SELF.QTag.IsCData  = pIsCData
       SELF.QTag.TagType  = TagType:Tag
     
       SELF.QTag.Attrs   &= NEW(QueueTagAttributes)
       SELF.QTag.Child   &= NEW(QueueTags)
       ADD(SELF.QTag)
    ELSE
       IF RECORDS(SELF.QTag) THEN
          lAdded = SELF.AddTagToParent(SELF.QTag,pName,pValue,pIsCData,pParent)
          ASSERT(lAdded,'Parent tag not found ParentTag('&pParent&') TagName('&pName&')')
       END
    END

XMLWriter.AddTagToParent    PROCEDURE(QueueTags pQTag,STRING pName,STRING pValue,BYTE pIsCData=0,STRING pParent)
lIndex  LONG
lAdded  BYTE
 CODE
 IF NOT CLIP(pName) THEN RETURN False.
 lAdded = False
 LOOP lIndex=RECORDS(pQTag) TO 1 BY -1
      GET(pQTag,lIndex)
      IF NOT ERRORCODE() THEN
         IF pQTag.TagType = TagType:Tag THEN
            lAdded = SELF.AddTagToParent(pQTag.Child,pName,pValue,pIsCData,pParent)
            IF lAdded=True THEN
               BREAK
            ELSE
               IF CLIP(UPPER(pQTag.Name))=CLIP(UPPER(pParent))
                  lAdded = True
                  pQTag.Child.Name     = pName
                  pQTag.Child.Value    &= NEW(STRING(LEN(pValue)))
                  pQTag.Child.Value    = pValue
                  pQTag.Child.IsCData  = pIsCData
                  pQTag.Child.TagType  = TagType:Tag
                  pQTag.Child.Attrs   &= NEW(QueueTagAttributes)
                  pQTag.Child.Child   &= NEW(QueueTags)
                  ADD(pQTag.Child)
                  BREAK
               END
            END
         END
      END
 END
 RETURN lAdded

XMLWriter.SetTagValue             PROCEDURE(STRING pTagName,STRING pValue,BYTE pIsCData=0)
lAdded  BYTE
 CODE
     IF NOT CLIP(pTagName) THEN RETURN.
     IF RECORDS(SELF.QTag) THEN
        lAdded = SELF.SetTagValueToParent(SELF.QTag,pTagName,pValue,pIsCData)
     END

XMLWriter.SetTagValueToParent     PROCEDURE(QueueTags pQTag,STRING pTagName,STRING pValue,BYTE pIsCData=0)
lIndex  LONG
lAdded  BYTE
 CODE
 IF NOT CLIP(pTagName) THEN RETURN False.
 lAdded = False
 LOOP lIndex=RECORDS(pQTag) TO 1 BY -1
      GET(pQTag,lIndex)
      IF NOT ERRORCODE() THEN
         IF pQTag.TagType = TagType:Tag THEN
            lAdded = SELF.SetTagValueToParent(pQTag.Child,pTagName,pValue,pIsCData)
            IF lAdded=True THEN
               BREAK
            ELSE
               IF CLIP(UPPER(pQTag.Name))=CLIP(UPPER(pTagName))
                  lAdded = True
                  IF NOT pQTag.Value &= NULL THEN
                     DISPOSE(pQTag.Value)
                  END
                  pQTag.Value &= NEW(STRING(LEN(pValue)))
                  pQTag.Value  = pValue
                  pQTag.IsCData= pIsCData
                  PUT(pQTag)
                  BREAK
               END
            END
         END
      END
 END
 RETURN lAdded

XMLWriter.AddAttribute            PROCEDURE(STRING pName,STRING pValue,STRING pTagName)
 CODE
     IF NOT CLIP(pName) THEN RETURN.
     IF RECORDS(SELF.QTag) THEN
        SELF.AddAttributeToParent(SELF.QTag,pName,pValue,pTagName)
     END
 
XMLWriter.AddAttributeToParent    PROCEDURE(QueueTags pQTag,STRING pName,STRING pValue,STRING pTagName)
lIndex  LONG
lAdded  BYTE
 CODE
 IF NOT CLIP(pName) THEN RETURN False.
 lAdded = False
 LOOP lIndex=RECORDS(pQTag) TO 1 BY -1
      GET(pQTag,lIndex)
      IF NOT ERRORCODE() THEN
         IF pQTag.TagType = TagType:Tag THEN
            lAdded = SELF.AddAttributeToParent(pQTag.Child,pName,pValue,pTagName)
            IF lAdded=True THEN
               BREAK
            ELSE
               IF CLIP(UPPER(pQTag.Name))=CLIP(UPPER(pTagName))
                  lAdded = True
                  pQTag.Attrs.Name = pName
                  pQTag.Attrs.Value= pValue
                  ADD(pQTag.Attrs)
                  BREAK
               END
            END
         END
      END
 END
 RETURN lAdded

XMLWriter.FlushRootOpen           PROCEDURE()
 CODE
    IF SELF.RootAdded = False THEN
       SELF.RootAdded = True
       IF CLIP(SELF.RootTagAttributes) <> ''
         SELF.WriteLine('<'&SELF.MakeValidName(CLIP(SELF.RootTag))&' '&CLIP(SELF.RootTagAttributes)&'>')
       ELSE
         SELF.WriteLine('<'&SELF.MakeValidName(CLIP(SELF.RootTag))&'>')
       END
    END

XMLWriter.FlushRootClose          PROCEDURE()
 CODE
    IF SELF.RootAdded = True THEN
       SELF.RootAdded = False
       SELF.WriteLine('</'&SELF.MakeValidName(CLIP(SELF.RootTag))&'>')
    END

XMLWriter.FlushTags              PROCEDURE(SHORT pIndent,QueueTags pQTag)
lIndex  SHORT
lIndexAttr  SHORT
lIndexHAttr SHORT
lXMLVersionFound BYTE(0)
lXMLEncodingFound BYTE(0)
 CODE
    IF NOT RECORDS(pQTag) THEN
       RETURN
    END
    LOOP lIndex=1 to RECORDS(pQTag)
         GET(pQTag,lIndex)
         IF ERRORCODE() THEN BREAK.
         IF SELF.XMLHeaderAdded = False THEN
            SELF.XMLHeaderAdded = True
            IF NOT RECORDS(SELF.QHeaderAttr) THEN
               IF SELF.ForceXMLHeader THEN
                  SELF.Output.WriteStr('<?xml version="'&CLIP(SELF.XMLVersion)&'"')
                  IF CLIP(SELF.Encoding)
                     SELF.Output.WriteStr(' encoding="'&CLIP(SELF.Encoding)&'"')
                  END
                  SELF.WriteLine(' ?>')
               END
            ELSE
               SELF.Output.WriteStr('<?xml')
            END
            lXMLVersionFound = False
            lXMLEncodingFound = False
            !ADD XMLVersion
            LOOP lIndexHAttr = 1 to RECORDS(SELF.QHeaderAttr)
                 GET(SELF.QHeaderAttr,lIndexHAttr)
                 IF ERRORCODE() THEN BREAK.
                 IF CLIP(SELF.QHeaderAttr.Name)='version' THEN
                    SELF.Output.WriteStr(' '&SELF.MakeValidName(CLIP(SELF.QHeaderAttr.Name))&'="'&CLIP(SELF.QHeaderAttr.Value)&'"')
                    lXMLVersionFound = True
                 END
                 IF CLIP(SELF.QHeaderAttr.Name)='encoding' THEN
                    lXMLEncodingFound = True
                 END
            END
            IF RECORDS(SELF.QHeaderAttr) THEN
               IF lXMLVersionFound = False THEN
                  SELF.Output.WriteStr(' '&'version="'&CLIP(SELF.XMLVersion)&'"')
               END
               IF lXMLEncodingFound = False AND CLIP(SELF.Encoding)
                  SELF.Output.WriteStr(' encoding="'&CLIP(SELF.Encoding)&'"')
               END
            END
            LOOP lIndexHAttr = 1 to RECORDS(SELF.QHeaderAttr)
                 GET(SELF.QHeaderAttr,lIndexHAttr)
                 IF ERRORCODE() THEN BREAK.
                 IF CLIP(SELF.QHeaderAttr.Name)<>'version' THEN
                    SELF.Output.WriteStr(' '&SELF.MakeValidName(CLIP(SELF.QHeaderAttr.Name))&'="'&CLIP(SELF.QHeaderAttr.Value)&'"')
                 END
            END
            IF RECORDS(SELF.QHeaderAttr) THEN
               SELF.WriteLine(' ?>')
            END
            IF RECORDS(SELF.QHeaderAttr) OR SELF.ForceXMLHeader THEN
               IF CLIP(SELF.XSL) OR SELF.UseXMLasXSLName
                  SELF.Output.WriteStr('<?xml-stylesheet type="text/xsl" href="')
                  IF SELF.UseXMLasXSLName
                     SELF.Output.WriteStr(SELF.Output.GetNameNoExt(SELF.Output.GetFileName())&'.XSL')
                  ELSE
                     SELF.Output.WriteStr(CLIP(SELF.XSL))
                  END
                  SELF.WriteLine('" ?>')
               END
            END
         END

         SELF.FlushRootOpen()
         
         CASE pQTag.TagType
         OF TagType:Comment
            SELF.WriteLine('<!-- '&CLIP(pQTag.Name)&' -->')
         OF TagType:Tag
            IF SELF.UseCRLF
               SELF.Output.WriteStr(ALL(' ',pIndent)&'<'&SELF.MakeValidName(CLIP(pQTag.Name)))
            ELSE
               SELF.Output.WriteStr('<'&SELF.MakeValidName(CLIP(pQTag.Name)))
            END
            LOOP lIndexAttr=1 TO RECORDS(pQTag.Attrs)
                 GET(pQTag.Attrs,lIndexAttr)
                 IF ERRORCODE() THEN BREAK.
                 SELF.Output.WriteStr(' '&SELF.MakeValidName(CLIP(pQTag.Attrs.Name))&'="'&CLIP(pQTag.Attrs.Value)&'"')
            END
            IF RECORDS(pQTag.Child) OR CLIP(pQTag.Value) THEN
                  SELF.WriteLine('>')
                  IF (CLIP(pQTag.Value)) THEN
                     IF pQTag.IsCData
                        SELF.Output.WriteStr('<![CDATA[')
                     END
                     IF pQTag.IsCData
                        SELF.Output.WriteStr(CLIP(pQTag.Value))
                     ELSE
                        SELF.WriteLine(SELF.EscapeValues(CLIP(pQTag.Value)))
                     END
                     IF pQTag.IsCData
                        SELF.WriteLine(']]>')
                     END
                  END
                  SELF.FlushTags(pIndent+1,pQTag.Child)
                  IF SELF.UseCRLF
                     SELF.WriteLine(ALL(' ',pIndent)&'</'&SELF.MakeValidName(CLIP(pQTag.Name))&'>')
                  ELSE
                     SELF.WriteLine('</'&SELF.MakeValidName(CLIP(pQTag.Name))&'>')
                  END
            ELSE
                  SELF.WriteLine('/>')
            END
         END
    END
    !SELF.ChildDESTRUCT(pQtag)

XMLWriter.SaveAs                  PROCEDURE(STRING pFileName)
 CODE
    SELF.SetFileName(pFileName)
    SELf.Save()
    
XMLWriter.Save                    PROCEDURE()
 CODE
    ASSERT(CLIP(SELF.Output.GetFileName())<>'','The file name for the output file is empty.')
    SELF.Output.OpenFile()

    IF NOT RECORDS(SELF.QTag) THEN
       RETURN
    END
    IF NOT CLIP(SELF.RootTag) THEN
       SELF.SetRootTag('Clarion_XML_Document')
    END
    SELF.RootAdded      = False
    SELF.XMLHeaderAdded = False
    SELF.FlushTags(1,SELF.QTag)
    SELF.FlushRootClose()
    SELF.Output.CloseFile()
    SELF.Output.SetFileName('')
    
!****************************************************************************
!****************************************************************************
!****************************************************************************
!****************************************************************************


XMLGenerator.Init                    PROCEDURE(<STRING TargetFileName>)
 CODE
    IF OMITTED(2) THEN
       PARENT.Init()
    ELSE
       PARENT.Init(TargetFileName)
    END
    SELF.PageOpen = False
    SELF.PageNo   = 0
    SELF.TotalPageNo   = 0
    SELF.SetPagesAsParentTag(False)
    SELF.SetPagesAsDifferentFile(False)
    SELF.BasePageTagName = 'Page_'
    SELF.Setup()
    
    
XMLGenerator.SetUp                   PROCEDURE()
 CODE

XMLGenerator.OpenDocument            PROCEDURE(<STRING pDocumentName>)
 CODE
    SELF.PageNo = 0
    SELF.RootAdded = False
    SELF.ChildDESTRUCT(SELF.Qtag)
    IF NOT OMITTED(2) THEN
       SELF.SetRootTag(pDocumentName)
    END
    IF SELF.PagesAsDifferentFile = False THEN
       SELF.Output.OpenFile()
    END

XMLGenerator.CloseDocument           PROCEDURE()
 CODE
    IF SELF.PagesAsDifferentFile = False THEN
       SELF.FlushPages()
       SELF.Output.CloseFile()
       SELF.Output.SetFileName('')
    END

XMLGenerator.StartPage               PROCEDURE()
lIndex  SHORT
 CODE
    SELF.PageNo+=1
    IF SELF.PagesAsDifferentFile = True THEN
       SELF.RootAdded = False
       SELF.ChildDESTRUCT(SELF.Qtag)
       !GENERATE FILE NAME
       SELF.Output.SetFileNameFromOriginal(SELF.PageNo,SELF.TotalPageNo)
       SELF.Output.OpenFile()
    ELSE
       IF SELF.PagesAsParentTag = True THEN
          SELF.AddTag(CLIP(SELF.BasePageTagName)&SELF.PageNo,'')
       END
    END
    SELF.PageOpen = True

XMLGenerator.EndPage                 PROCEDURE()
 CODE
    IF SELF.PagesAsDifferentFile = True THEN
       SELF.FlushPages()
       SELF.Output.CloseFile()
       SELF.Output.SetFileName('')
    END
    SELF.PageOpen = False

XMLGenerator.FlushPages              PROCEDURE()
lIndex  SHORT
 CODE
    IF NOT RECORDS(SELF.QTag) THEN
       RETURN
    END
    IF NOT CLIP(SELF.RootTag) THEN
       SELF.SetRootTag('Clarion_XML_Document')
    END
    SELF.RootAdded      = False
    SELF.XMLHeaderAdded = False
    SELF.PageTagAdded        = False
    SELF.FlushTags(1,SELF.QTag)
    SELF.FlushRootClose()

XMLGenerator.FlushRootOpen           PROCEDURE()
 CODE
    PARENT.FlushRootOpen()
    IF SELF.PagesAsDifferentFile = True THEN
       IF SELF.PagesAsParentTag = True THEN
          IF SELF.PageTagAdded = False THEN
             SELF.PageTagAdded = True
             SELF.WriteLine('<'&SELF.MakeValidName(CLIP(SELF.BasePageTagName)&SELF.PageNo)&'>')
          END
       END
    END

XMLGenerator.FlushRootClose          PROCEDURE()
 CODE
    IF SELF.PageTagAdded = True THEN
       SELF.PageTagAdded = False
       SELF.WriteLine('</'&SELF.MakeValidName(CLIP(SELF.BasePageTagName)&SELF.PageNo)&'>')
    END
    PARENT.FlushRootClose()

XMLGenerator.SetPagesAsParentTag     PROCEDURE(BYTE pTrue=1)
 CODE
    SELF.PagesAsParentTag = pTrue

XMLGenerator.SetPagesAsDifferentFile  PROCEDURE(BYTE pTrue=1)
 CODE
    SELF.PagesAsDifferentFile = pTrue

XMLGenerator.AddTag                  PROCEDURE(STRING pName,STRING pValue,BYTE pIsCData=0,<STRING pParent>)
lAdded  BYTE
 CODE
    IF NOT CLIP(pName) THEN RETURN.
    IF OMITTED(5) OR CLIP(pParent)=''
       IF SELF.PagesAsDifferentFile = False AND SELF.PagesAsParentTag = True
          IF CLIP(pName)<>CLIP(CLIP(SELF.BasePageTagName)&SELF.PageNo)
             SELF.AddTag(pName,pValue,pIsCData,CLIP(SELF.BasePageTagName)&SELF.PageNo)
             RETURN
          END
       END
    END
    PARENT.AddTag(pName,pValue,pIsCData,pParent)

!**********************************************************************
!**********************************************************************
!**********************************************************************
!**********************************************************************

XMLReportGenerator.CONSTRUCT            PROCEDURE()
 CODE
    SELF.Attribute  &=NEW ReportAttributeManager
    SELF.ProcessedTag  &= NEW(QueueProcessedTags)
    SORT(SELF.ProcessedTag,SELF.ProcessedTag.FEQ)
    SELF.XMLVersion='1.0'

XMLReportGenerator.DESTRUCT                             PROCEDURE()
 CODE
    FREE(SELF.ProcessedTag)
    DISPOSE(SELF.ProcessedTag)
    DISPOSE(SELF.Attribute)

XMLReportGenerator.IReportGenerator.Init                PROCEDURE(<ErrorClass EC>)
  CODE
XMLReportGenerator.IReportGenerator.ProcessBand         PROCEDURE(STRING type, BYTE start)
  CODE
    !NOT SUPPORTED
XMLReportGenerator.IReportGenerator.OpenDocument        PROCEDURE(UNSIGNED TotalPages)
 CODE
     SELF.OpenDocument()
     SELF.TotalPageNo = TotalPages
     RETURN Level:Benign
XMLReportGenerator.IReportGenerator.CloseDocument       PROCEDURE()
 CODE
     IF SELF.PagesAsDifferentFile = False THEN
        IF NOT SELF.OutputFileQueue &= NULL THEN
           SELF.OutputFileQueue.FileName = SELF.Output.GetFileName()
           ADD(SELF.OutputFileQueue)
        END
     END
     SELF.CloseDocument()
     RETURN Level:Benign
XMLReportGenerator.IReportGenerator.SupportPageProcessing PROCEDURE()
 CODE
    RETURN TRUE
XMLReportGenerator.IReportGenerator.OpenPage PROCEDURE(STRING PageName)
 CODE
    RETURN Level:Benign
XMLReportGenerator.IReportGenerator.StartPageProcess  PROCEDURE(SHORT pBoxLeft,SHORT pBoxTop,SHORT pBoxRight,SHORT pBoxBottom,STRING pPageName)
 CODE
     SELF.StartPage()
     RETURN Level:Benign
XMLReportGenerator.IReportGenerator.ClosePage           PROCEDURE()
 CODE
     IF SELF.PagesAsDifferentFile = True THEN
        IF NOT SELF.OutputFileQueue &= NULL THEN
           SELF.OutputFileQueue.FileName = SELF.Output.GetFileName()
           ADD(SELF.OutputFileQueue)
        END
     END
     SELF.EndPage()
     RETURN Level:Benign

XMLReportGenerator.IReportGenerator.AskProperties       PROCEDURE(BYTE Force=0)
ReturnValue  BYTE
LOC:FileName CSTRING(FILE:MaxFilePath+1),AUTO
LExt         CSTRING(128),AUTO
 CODE
     ReturnValue = Level:Benign
     SELF.Init()
     IF NOT CLIP(SELF.Output.GetFileName()) OR Force THEN
        LOC:FileName = ''
        IF NOT FILEDIALOG('Save as XML File',LOC:FileName,'XML|*.XML|Text|*.TXT|All|*.*',FILE:LongName+FILE:Save+FILE:KeepDir) THEN
           ReturnValue = Level:Notify
        ELSE
           Lext=''
           PathSplit(LOC:FileName, , , , Lext)
           IF NOT Lext THEN
              LOC:FileName=LOC:FileName&'.XML'
           END
           SELF.Output.SetOriginalFileName(LOC:FileName)
        END
     END
     RETURN ReturnValue
XMLReportGenerator.IReportGenerator.WhoAmI              PROCEDURE()
 CODE
     RETURN RepGen:XML

XMLReportGenerator.IReportGenerator.DisplayIcon         PROCEDURE()
 CODE
     RETURN '~EXP_XML.ICO'

XMLReportGenerator.IReportGenerator.DisplayName         PROCEDURE()
 CODE
     RETURN 'XML'

XMLReportGenerator.IReportGenerator.GetFileName              PROCEDURE()
 CODE
    RETURN SELF.Output.GetOriginalFileName()

XMLReportGenerator.IReportGenerator.ProcessString         PROCEDURE(*StringFormatGrp pFormatGrp, STRING Text, STRING pExtendControlAttr)
lName    STRING(MaxXMLGeneratorNameLen)
lValue   STRING(MaxXMLGeneratorValueLen)
lParent  STRING(MaxXMLGeneratorNameLen)
lTagType BYTE
lFEQ     USHORT
lNameFromText  BYTE
lValueFromText BYTE
lIsCData       BYTE
 CODE
    IF SELF.Attribute.Extract(RepGen:ALL,TargetAttr:SKIP,pExtendControlAttr) THEN
       RETURN
    END
    IF SELF.Attribute.Extract(RepGen:XML,TargetAttr:SKIP,pExtendControlAttr) THEN
       RETURN
    END
    lFEQ = SELF.Attribute.Extract(RepGen:XML,TargetAttr:TagProcessOnce,pExtendControlAttr)

    IF lFEQ THEN
       SELF.ProcessedTag.FEQ = lFEQ
       GET(SELF.ProcessedTag,SELF.ProcessedTag.FEQ)
       IF NOT ERRORCODE() THEN
          IF SELF.ProcessedTag.FEQ = lFEQ THEN
             RETURN
          END
       ELSE
          SELF.ProcessedTag.FEQ = lFEQ
          ADD(SELF.ProcessedTag)
       END
    END
    
    lTagType       = SELF.Attribute.Extract(RepGen:XML,TargetAttr:TagType,pExtendControlAttr)
    lNameFromText  = SELF.Attribute.Extract(RepGen:XML,TargetAttr:TagNameFromText,pExtendControlAttr)
    lValueFromText = SELF.Attribute.Extract(RepGen:XML,TargetAttr:TagValueFromText,pExtendControlAttr)
    lIsCData       = SELF.Attribute.Extract(RepGen:XML,TargetAttr:TagIsCData,pExtendControlAttr)

    CASE lTagType
    OF TagType:Tag
       IF lNameFromText THEN
          lName = Text
       ELSE
          lName = SELF.Attribute.Extract(RepGen:XML,TargetAttr:TagName,pExtendControlAttr)
       END
       IF lValueFromText THEN
          lValue = Text
       ELSE
          lValue = SELF.Attribute.Extract(RepGen:XML,TargetAttr:TagValue,pExtendControlAttr)
       END
       lParent= SELF.Attribute.Extract(RepGen:XML,TargetAttr:TagParentName,pExtendControlAttr)
       IF NOT CLIP(lValue)
          lIsCData = False
       END
       SELF.AddTag(CLIP(lName),CLIP(lValue),lIsCData,CLIP(lParent))
    OF TagType:TagValue
       lParent= SELF.Attribute.Extract(RepGen:XML,TargetAttr:TagParentName,pExtendControlAttr)
       IF CLIP(lParent) THEN
          SELF.SetTagValue(CLIP(lParent),Text,lIsCData)
       END
    OF TagType:TagAttribute
       IF lNameFromText THEN
          lName = Text
       ELSE
          lName = SELF.Attribute.Extract(RepGen:XML,TargetAttr:TagName,pExtendControlAttr)
       END
       IF lValueFromText THEN
          lValue = Text
       ELSE
          lValue = SELF.Attribute.Extract(RepGen:XML,TargetAttr:TagValue,pExtendControlAttr)
       END
       lParent= SELF.Attribute.Extract(RepGen:XML,TargetAttr:TagParentName,pExtendControlAttr)
       IF lParent THEN
          SELF.AddAttribute(CLIP(lName),CLIP(lValue),CLIP(lParent))
       END
    OF TagType:HeaderAttribute
       IF lNameFromText THEN
          lName = Text
       ELSE
          lName = SELF.Attribute.Extract(RepGen:XML,TargetAttr:TagName,pExtendControlAttr)
       END
       IF lValueFromText THEN
          lValue = Text
       ELSE
          lValue = SELF.Attribute.Extract(RepGen:XML,TargetAttr:TagValue,pExtendControlAttr)
       END
       SELF.AddXMLHeaderAttribute(lName,lValue)
    OF TagType:Comment
       IF lValueFromText THEN
          lValue = Text
       ELSE
          lValue = SELF.Attribute.Extract(RepGen:XML,TargetAttr:TagValue,pExtendControlAttr)
       END
       SELF.AddComment(lValue)
    ELSE
       !IS not a valid TagType
    END

XMLReportGenerator.IReportGenerator.ProcessLine        PROCEDURE(*LineFormatGrp pFormatGrp, STRING pExtendControlAttr)
 CODE
    !NOT SUPPORTED
XMLReportGenerator.IReportGenerator.ProcessEllipse     PROCEDURE(*EllipseFormatGrp pFormatGrp, STRING pExtendControlAttr)
 CODE
    !NOT SUPPORTED

XMLReportGenerator.IReportGenerator.ProcessArc      PROCEDURE(*ArcFormatGrp pFormatGrp, STRING pExtendControlAttr)
 CODE
 !NOT SUPPORTED

XMLReportGenerator.IReportGenerator.ProcessChord      PROCEDURE(*ChordFormatGrp pFormatGrp, STRING pExtendControlAttr)
 CODE
 !NOT SUPPORTED

XMLReportGenerator.IReportGenerator.ProcessRectangle   PROCEDURE(*RectFormatGrp pFormatGrp, STRING pExtendControlAttr)
 CODE
    !NOT SUPPORTED

XMLReportGenerator.IReportGenerator.ProcessImage       PROCEDURE(*ImageFormatGrp pFormatGrp, STRING iName, STRING pExtendControlAttr)
 CODE
    !NOT SUPPORTED

XMLReportGenerator.IReportGenerator.ProcessCheck       PROCEDURE(*CheckFormatGrp pFormatGrp, STRING Text, STRING pExtendControlAttr)
 CODE
    SELF.IReportGenerator.ProcessString(pFormatGrp.Prompt, Text,pExtendControlAttr)

XMLReportGenerator.IReportGenerator.ProcessRadio       PROCEDURE(*RadioFormatGrp pFormatGrp, STRING Text, STRING pExtendControlAttr)
 CODE
    !NOT SUPPORTED
XMLReportGenerator.IReportGenerator.ProcessPie          PROCEDURE(SliceFormatQueue pSliceFormatQueue, *PosGrp pPosGroup, STRING pExtendControlAttr)
 CODE
    !NOT SUPPORTED

XMLReportGenerator.IReportGenerator.ProcessPolygon  PROCEDURE(PointQueue pPointQueue, *StyleGrp pStyleGrp, STRING pExtendControlAttr)
  CODE
  !NOT SUPPORTED

XMLReportGenerator.IReportGenerator.ProcessGroup        PROCEDURE(*GroupFormatGrp pFormatGrp, STRING Text, STRING pExtendControlAttr)
 CODE
    SELF.IReportGenerator.ProcessString(pFormatGrp.header, Text,pExtendControlAttr)

XMLReportGenerator.IReportGenerator.ProcessText     PROCEDURE(TextFormatQueue TextFormatQ, STRING pExtendControlAttr)
lIndex      USHORT
lName       STRING(MaxXMLGeneratorNameLen)
lValue      &STRING 
lParent     STRING(MaxXMLGeneratorNameLen)
lTagType    BYTE
  CODE
    IF SELF.Attribute.Extract(RepGen:ALL,TargetAttr:SKIP,pExtendControlAttr) THEN
       RETURN
    END
    IF SELF.Attribute.Extract(RepGen:XML,TargetAttr:SKIP,pExtendControlAttr) THEN
       RETURN
    END
    lValue&=NEW(STRING(SIZE(TextFormatQ.Text)*RECORDS(TextFormatQ)))
    lValue = ''

    lTagType = SELF.Attribute.Extract(RepGen:XML,TargetAttr:TagType,pExtendControlAttr)
    CASE lTagType
    OF TagType:Tag          
       lName = SELF.Attribute.Extract(RepGen:XML,TargetAttr:TagName,pExtendControlAttr)
       IF NOT CLIP(lName) THEN
          RETURN
       END
       lParent= SELF.Attribute.Extract(RepGen:XML,TargetAttr:TagParentName,pExtendControlAttr)
       LOOP lIndex = 1 TO RECORDS(TextFormatQ)
         GET(TextFormatQ, lIndex)
         lValue = CLIP(lValue)&CLIP(TextFormatQ.Text)&'<13,10>'
       END
       SELF.AddTag(CLIP(lName),CLIP(lValue),True,CLIP(lParent))
    OF TagType:TagAttribute
       LOOP lIndex = 1 TO RECORDS(TextFormatQ)
         GET(TextFormatQ, lIndex)
         lValue = CLIP(lValue)&CLIP(TextFormatQ.Text)
       END
       lName = SELF.Attribute.Extract(RepGen:XML,TargetAttr:TagName,pExtendControlAttr)
       IF NOT CLIP(lName) THEN
          RETURN
       END
       lParent= SELF.Attribute.Extract(RepGen:XML,TargetAttr:TagParentName,pExtendControlAttr)
       IF lParent THEN
          SELF.AddAttribute(CLIP(lName),CLIP(lValue),CLIP(lParent))
       END
    OF TagType:Comment
       LOOP lIndex = 1 TO RECORDS(TextFormatQ)
         GET(TextFormatQ, lIndex)
         SELF.AddComment(TextFormatQ.Text)
       END  
    ELSE
       !IS not a valid TagType
?      ASSERT(False,'Not a valid tag type on the Text control. '&pExtendControlAttr)
    END
    DISPOSE(lValue)

XMLReportGenerator.IReportGenerator.SetResultQueue      PROCEDURE(OutputFileQueue pOutputFile)
 CODE
    SELF.OutputFileQueue &= pOutputFile
    FREE(SELF.OutputFileQueue)

XMLReportGenerator.IReportGenerator.SupportResultQueue  PROCEDURE()
 CODE
 RETURN True

XMLReportGenerator.IReportGenerator.SupportPageParsing  PROCEDURE()
 CODE
 RETURN True
