  MEMBER

  INCLUDE('ABWMFPAR.INC'),ONCE
  MAP
    INCLUDE('CWUTIL.INC'),ONCE
    MODULE('Windows')
      MulDiv (LONG,LONG,LONG),LONG,PASCAL
    END
  END

  ITEMIZE,PRE(Type)
Brush  EQUATE
Font   EQUATE
Pen    EQUATE
  END

QueueObjects     QUEUE,TYPE
ID                 USHORT
Type               BYTE
                 END

QueueFonts       QUEUE,TYPE
ID                 USHORT
Color              LONG
Size               USHORT
Style              USHORT
Face               CSTRING(32)
Angle              USHORT
CharSet            BYTE
                 END

QueuePens        QUEUE,TYPE
ID                 USHORT
Color              LONG
Width              LONG
Style              USHORT
                 END

QueueBrush       QUEUE,TYPE
ID                 USHORT
Color              LONG
Style              USHORT
Hatch              USHORT
                 END

QueueHoles       QUEUE,TYPE
Id                 USHORT
                 END

QueueDC          QUEUE,TYPE
CurrentBrush       LONG
CurrentFont        LONG
CurrentPen         LONG
BKColor            LONG
BKMode             USHORT
TextAlign          USHORT
TextColor          LONG
                 END

  OMIT('=== DO LINK', lib_mode)
 PRAGMA('link(C%V%DOS%X%%L%.LIB)')
! === DO LINK

WMFInFile               FILE,DRIVER('DOS'),THREAD,PRE(WMFF)
                          RECORD
Buffer                     STRING(5000)
                          END
                        END

WMFParser.CONSTRUCT     PROCEDURE
  CODE
    SELF.PointQ &= NEW(PointQueue)
    SELF.SliceFormatQ &= NEW(SliceFormatQueue)
    SELF.TextFormatQ &= NEW(TextFormatQueue)
    SELF.Fonts &= NEW(QueueFonts)
    SELF.Brush &= NEW(QueueBrush)
    SELF.Pens &= NEW(QueuePens)
    SELF.Holes &= NEW(QueueHoles)
    SELF.Objects &= NEW(QueueObjects)
    SELF.DC &= NEW(QueueDC)
    SELF.CurrentObject  = 0
    CLEAR(SELF.DC)
    SELF.DC.BKColor     = COLOR:None
    SELF.DC.CurrentFont = -1
    SELF.DC.CurrentPen  = -1
    SELF.DC.CurrentBrush= -1
    SELF.InsideBox    = 0
    SELF.InsideCheck  = 0
    SELF.InsideImage  = 0
    SELF.InsideGroup  = 0
    SELF.InsideRadio  = 0
    SELF.InsideString = 0
    SELF.InsideText   = 0

WMFParser.DESTRUCT      PROCEDURE
  CODE
    IF SELF.FileOpen=True THEN
      SELF.CloseFile()
    END
    SELF.Kill()
    DISPOSE(SELF.PointQ)
    DISPOSE(SELF.SliceFormatQ)
    DISPOSE(SELF.TextFormatQ)
    DISPOSE(SELF.Fonts)
    DISPOSE(SELF.Brush)
    DISPOSE(SELF.Pens)
    DISPOSE(SELF.Holes)
    DISPOSE(SELF.DC)
    DISPOSE(SELF.Objects)

WMFParser.Init          PROCEDURE(STRING fname, IReportGenerator rptgen, ErrorClass EC)
  CODE
    SELF.FileName     = CLIP(fname)
    SELF.FileOpen     = False
    SELF.WMFFPos      = 1
    SELF.Errors      &= EC
    SELF.ReportGenerator &= rptgen
    CLEAR(SELF.DC)
    SELF.CurrentObject  = 0
    SELF.DC.BKColor     = COLOR:None
    SELF.DC.CurrentFont = -1
    SELF.DC.CurrentPen  = -1
    SELF.DC.CurrentBrush= -1
    SELF.WindowOriginX  = 0
    SELF.WindowOriginY  = 0
    SELF._SkipInvalidExternalImages = True

WMFParser.Kill          PROCEDURE
  CODE
    FREE(SELF.PointQ)
    FREE(SELF.SliceFormatQ)
    FREE(SELF.TextFormatQ)
    FREE(SELF.Fonts)
    FREE(SELF.Brush)
    FREE(SELF.Pens)
    FREE(SELF.Holes)
    FREE(SELF.DC)
    FREE(SELF.Objects)

WMFParser.AddObject     PROCEDURE(BYTE type)
  CODE
    IF RECORDS(SELF.Holes) THEN
      GET(SELF.Holes,1)
      SELF.Objects.ID = SELF.Holes.Id
      DELETE(SELF.Holes)
    ELSE
      SELF.Objects.ID = SELF.CurrentObject
      SELF.CurrentObject += 1
    END
    SELF.Objects.Type = type
    GET(SELF.Objects, SELF.Objects.ID)
?   ASSERT(ERRORCODE(),'On WMFParser.AddObject: object repeated '&SELF.Objects.ID)
    ADD(SELF.Objects)
    RETURN SELF.Objects.ID

WMFParser.OpenFile      PROCEDURE
ret BYTE,AUTO
  CODE
    IF SELF.FileOpen = True
      IF UPPER(WMFInFile{PROP:Name}) <> UPPER(SELF.FileName)
        ret = SELF.CloseFile()
        IF ret <> Level:Benign
          RETURN ret
        END
      END
    END
    SELF.Errors.SetFile(SELF.FileName)
    WMFInFile{PROP:Name} = SELF.FileName
    OPEN(WMFInFile)
    IF ERRORCODE() THEN RETURN SELF.Errors.Throw(Msg:OpenFailed).
    SELF.FileOpen = True
    RETURN Level:Benign

WMFParser.CloseFile     PROCEDURE
  CODE
    IF SELF.FileOpen = True
      CLOSE(WMFInFile)
      IF ERRORCODE() THEN RETURN SELF.Errors.Throw(Msg:CloseFailed).
      SELF.FileOpen=False
    END
    RETURN Level:Benign

WMFParser.ProcessHeader  PROCEDURE()

METAFILEHEADER       GROUP,OVER(WMFF:Buffer)
key                    LONG
hmf                    USHORT
bbox:left              SHORT
bbox:top               SHORT
bbox:right             SHORT
bbox:bottom            SHORT
inch                   USHORT
reserved               LONG
checksum               USHORT
                     END

METAHEADER           GROUP,OVER(WMFF:Buffer)
Type                   USHORT
HeaderSize             USHORT
Version                USHORT
Size                   ULONG
NoObjects              USHORT
MaxRecord              ULONG
NoParameters           USHORT
                     END

  CODE
  GET(WMFInFile, SELF.WMFFPos, SIZE(METAFILEHEADER))
  IF ERRORCODE() THEN RETURN SELF.Errors.Throw(Msg:OpenFailed).
  IF MetaFileHeader.Key <> 9ac6cdd7H THEN RETURN SELF.Errors.Throw(Msg:WMFFileNotValid).
  SELF.PixelsPerInch = MetaFileHeader.Inch
  SELF.ToInch        = 1000/SELF.PixelsPerInch
  SELF.BoxLeft       = MetaFileHeader.bbox:left*SELF.ToInch
  SELF.BoxTop        = MetaFileHeader.bbox:top*SELF.ToInch
  SELF.BoxRight      = MetaFileHeader.bbox:right*SELF.ToInch
  SELF.BoxBottom     = MetaFileHeader.bbox:bottom*SELF.ToInch
  SELF.WMFFPos += SIZE(METAFILEHEADER)
  GET(WMFInFile, SELF.WMFFPos, SIZE(METAHEADER))
  IF ERRORCODE() THEN RETURN Level:Fatal.
  SELF.WMFFPos += SIZE(MetaHeader)
  RETURN Level:Benign

WMFParser.GetBoxLeft      PROCEDURE()
  CODE
  RETURN SELF.BoxLeft

WMFParser.GetBoxTop      PROCEDURE()
  CODE
  RETURN SELF.BoxTop

WMFParser.GetBoxRight      PROCEDURE()
  CODE
  RETURN SELF.BoxRight

WMFParser.GetBoxBottom      PROCEDURE()
  CODE
  RETURN SELF.BoxBottom
 
WMFParser.SetSkipInvalidExternalImages PROCEDURE(BYTE value)
  CODE
     IF value>0
        SELF._SkipInvalidExternalImages = True
     ELSE
        SELF._SkipInvalidExternalImages = False
     END

WMFParser.GetSkipInvalidExternalImages PROCEDURE()
  CODE
  RETURN SELF._SkipInvalidExternalImages

WMFParser.Run           PROCEDURE()
  CODE
  RETURN SELF.ProcessFile()

WMFParser.ProcessFile   PROCEDURE
ret     BYTE,AUTO
  CODE
  LOOP
    ret = SELF.TakeRecord()
  WHILE ret = Level:Benign
  RETURN CHOOSE (ret = Level:Notify, Level:Benign, ret)


WMFParser.TakeRecord           PROCEDURE()

METARECORD           GROUP,OVER(WMFF:Buffer)
Size                   ULONG
Funct                  USHORT
Params                 USHORT,DIM(1000)
BrushIndirect          GROUP,OVER(Params)
Style                    USHORT
Color                    LONG
Hatch                    USHORT
                       END

StretchDIBitsGrp GROUP,OVER(Params)
dwRop   ULONG!   Raster operation to be performed
Usag    USHORT!  Flag indicating whether the bitmap color table contains RGB values or indexes to the currently realized logical palette
srcYExt SHORT!   Height of the source in the bitmap
srcXExt SHORT!   Width of the source in the bitmap
srcY    SHORT!   Y-coordinate of the origin of the source in the bitmap
srcX    SHORT!   X-coordinate of the origin of the source in the bitmap
dstYExt SHORT!   Height of the destination rectangle
dstXExt SHORT!   Width of the destination rectangle
dstY    SHORT!   Y-coordinate of the origin of the destination rectangle
dstX    SHORT!   X-coordinate of the origin of the destination rectangle
 END
                      
LongGrp                GROUP,OVER(Params)
Param1                   LONG
                       END
ShortGrp               GROUP,OVER(Params)
Param1                   SHORT
Param2                   SHORT
Param3                   SHORT
Param4                   SHORT
Param5                   SHORT
Param6                   SHORT
Param7                   SHORT
Param8                   SHORT
                       END
PenIndirect            GROUP,OVER(Params)
Style                    USHORT
Width                    LONG
Color                    LONG
                       END
FontIndirect           GROUP,OVER(Params)
lfHeight                 SHORT
lfWidth                  SHORT
lfEscapement             SHORT
lfOrientation            SHORT
lfWeight                 SHORT
lfItalic                 BYTE
lfUnderline              BYTE
lfStrikeOut              BYTE
lfCharSet                BYTE
lfOutPrecision           BYTE
lfClipPrecision          BYTE
lfQuality                BYTE
lfPitchAndFamily         BYTE
                       END
EscapeGrp              GROUP,OVER(Params)
P1                       USHORT
P2                       USHORT
Magic                    USHORT
Type                     USHORT
CtlName                  CSTRING(20)
slen                     USHORT
isSplit                  USHORT
                       END
                     END

INCLUDE('ABWMFPAR.EQU'),ONCE

I                    UNSIGNED,AUTO

ViaSign              SHORT,AUTO

TextStart            USHORT,AUTO
TextLen              USHORT,AUTO
TmpCString           CSTRING(FILE:MaxFilePath),AUTO

BandName             CSTRING(10),AUTO
LOC:RecordStart      LONG
  CODE
  GET (WMFInFile, SELF.WMFFPos, SIZE(MetaRecord.Size))
  CASE ERRORCODE()
  OF 0
  OF BadRecErr
    RETURN Level:Notify
  ELSE
    RETURN SELF.Errors.Throw(Msg:GetRecordFailed)
  END
  IF NOT MetaRecord.Size THEN RETURN SELF.Errors.Throw(Msg:WMFFileNotValid).

  LOC:RecordStart = SELF.WMFFPos
  GET (WMFInFile, SELF.WMFFPos, MetaRecord.Size * 2)
  SELF.WMFFPos += MetaRecord.Size * 2

  CASE MetaRecord.Funct
  OF META:ABORTDOC
  OF META:ANIMATEPALETTE
  OF META:ARC
    SELF.ArcFormat.StartX = (SELF.WindowOriginY + MetaRecord.ShortGrp.Param4)*SELF.ToInch
    SELF.ArcFormat.StartY = (SELF.WindowOriginX + MetaRecord.ShortGrp.Param3)*SELF.ToInch
    SELF.ArcFormat.EndX = (SELF.WindowOriginY + MetaRecord.ShortGrp.Param2)*SELF.ToInch
    SELF.ArcFormat.EndY = (SELF.WindowOriginX + MetaRecord.ShortGrp.Param1)*SELF.ToInch
    SELF.ArcFormat.Pos.Top = (SELF.WindowOriginY + MetaRecord.ShortGrp.Param7)*SELF.ToInch
    SELF.ArcFormat.Pos.Left = (SELF.WindowOriginX + MetaRecord.ShortGrp.Param8)*SELF.ToInch
    SELF.ArcFormat.Pos.Bottom = (SELF.WindowOriginY + MetaRecord.ShortGrp.Param5)*SELF.ToInch
    SELF.ArcFormat.Pos.Right = (SELF.WindowOriginX + MetaRecord.ShortGrp.Param4)*SELF.ToInch
    SELF.Pens.ID = SELF.DC.CurrentPen
    GET(SELF.Pens, SELF.Pens.ID)
    IF ERRORCODE()
      SELF.Errors.SetField('Pen')
      RETURN SELF.Errors.Throw(Msg:WMFObjectIdNotFound)
    END
    SELF.ArcFormat.Style.BorderStyle = SELF.Pens.Style
    SELF.ArcFormat.Style.BorderColor = SELF.Pens.Color
    SELF.ArcFormat.Style.BorderWidth = SELF.Pens.Width
  OF META:BITBLT
  OF META:CHORD
    SELF.ArcFormat.StartX = (SELF.WindowOriginY + MetaRecord.ShortGrp.Param4)*SELF.ToInch
    SELF.ArcFormat.StartY = (SELF.WindowOriginX + MetaRecord.ShortGrp.Param3)*SELF.ToInch
    SELF.ArcFormat.EndX = (SELF.WindowOriginY + MetaRecord.ShortGrp.Param2)*SELF.ToInch
    SELF.ArcFormat.EndY = (SELF.WindowOriginX + MetaRecord.ShortGrp.Param1)*SELF.ToInch
    SELF.ArcFormat.Pos.Top = (SELF.WindowOriginY + MetaRecord.ShortGrp.Param7)*SELF.ToInch
    SELF.ArcFormat.Pos.Left = (SELF.WindowOriginX + MetaRecord.ShortGrp.Param8)*SELF.ToInch
    SELF.ArcFormat.Pos.Bottom = (SELF.WindowOriginY + MetaRecord.ShortGrp.Param5)*SELF.ToInch
    SELF.ArcFormat.Pos.Right = (SELF.WindowOriginX + MetaRecord.ShortGrp.Param4)*SELF.ToInch
    SELF.Brush.ID = SELF.DC.CurrentBrush
    GET(SELF.Brush, SELF.Brush.ID)
    IF ERRORCODE()
      SELF.Errors.SetField('Brush')
      RETURN SELF.Errors.Throw(Msg:WMFObjectIdNotFound)
    END
    SELF.ArcFormat.Style.BackgroundColor = SELF.Brush.Color
    SELF.ArcFormat.Style.BackgroundStyle = SELF.Brush.Style
    SELF.Pens.ID = SELF.DC.CurrentPen
    GET(SELF.Pens, SELF.Pens.ID)
    IF ERRORCODE()
      SELF.Errors.SetField('Pen')
      RETURN SELF.Errors.Throw(Msg:WMFObjectIdNotFound)
    END
    SELF.ArcFormat.Style.BorderStyle = SELF.Pens.Style
    SELF.ArcFormat.Style.BorderColor = SELF.Pens.Color
    SELF.ArcFormat.Style.BorderWidth = SELF.Pens.Width
  OF META:CREATEBITMAP
  OF META:CREATEBITMAPINDIRECT
  OF META:CREATEBRUSH
  OF META:CREATEBRUSHINDIRECT
    SELF.Brush.ID    = SELF.AddObject(Type:Brush)
    SELF.Brush.Style = MetaRecord.BrushIndirect.Style
    CASE SELF.Brush.Style
    OF BRUSH:Hollow
    OROF BRUSH:Null
    OROF BRUSH:Pattern
      SELF.Brush.Color = COLOR:None
    ELSE
      SELF.Brush.Color = MetaRecord.BrushIndirect.Color
    END
    SELF.Brush.Hatch = MetaRecord.BrushIndirect.Hatch
    ADD(SELF.Brush)
    SELF.DC.CurrentBrush = SELF.Brush.ID
  OF META:CREATEFONTINDIRECT
    SELF.Fonts.Id = SELF.AddObject(Type:Font)
    ViaSign = MetaRecord.Params[1]
    SELF.Fonts.Size  = MulDiv (ABS(Metarecord.FontIndirect.lfHeight), 72, SELF.PixelsPerInch)
    SELF.Fonts.Color = SELF.DC.TextColor
    SELF.Fonts.Style = METARECORD.FontIndirect.lfWeight !BAND(MetaRecord.Params[5],FONT:Weight)
    SELF.Fonts.Angle = METARECORD.FontIndirect.lfEscapement/10
    IF METARECORD.FontIndirect.lfItalic THEN
       SELF.Fonts.Style += FONT:Italic
    END
    IF METARECORD.FontIndirect.lfUnderline THEN
       SELF.Fonts.Style += FONT:Underline
    END
    IF METARECORD.FontIndirect.lfStrikeOut THEN
       SELF.Fonts.Style += FONT:StrikeOut
    END
    SELF.Fonts.CharSet = METARECORD.FontIndirect.lfCharSet
    TextStart = 6 + 9 * 2 + 1
    ASSERT( MetaRecord.Size * 2 > TextStart )
    SELF.Fonts.Face = WMFInFile.Buffer[TextStart : MetaRecord.Size * 2]
    ADD(SELF.Fonts)
    SELF.DC.CurrentFont = SELF.Fonts.ID
  OF META:CREATEPALETTE
  OF META:CREATEPATTERNBRUSH
  OF META:CREATEPENINDIRECT
    SELF.Pens.ID = SELF.AddObject(Type:Pen)
    SELF.Pens.Width = MetaRecord.PenIndirect.Width
    SELF.Pens.Style = MetaRecord.PenIndirect.Style
    IF SELF.Pens.Style = PEN:Null
      SELF.Pens.Color = COLOR:None
    ELSE
      SELF.Pens.Color = MetaRecord.PenIndirect.Color
    END
    ADD(SELF.Pens)
    SELF.DC.CurrentPen = SELF.Pens.ID
  OF META:CREATEREGION
  OF META:DELETEOBJECT
    SELF.Objects.ID = MetaRecord.Params[1]
    GET(SELF.Objects, SELF.Objects.ID)
    IF ERRORCODE()
      SELF.Errors.SetField('Object')
      RETURN SELF.Errors.Throw(Msg:WMFObjectIdNotFound)
    END
    DELETE(SELF.Objects)
    SELF.Holes.Id = MetaRecord.Params[1]
    ADD(SELF.Holes,SELF.Holes.Id)
    IF ERRORCODE() THEN RETURN SELF.Errors.Throw(Msg:WMFAddQueueFailed).
    SELF.Fonts.Id = MetaRecord.Params[1]
    GET(SELF.Fonts,SELF.Fonts.Id)
    IF ~ERRORCODE() THEN
      DELETE(SELF.Fonts)
    ELSE
      SELF.Pens.Id = MetaRecord.Params[1]
      GET(SELF.Pens,SELF.Pens.Id)
      IF ~ERRORCODE() THEN
        DELETE(SELF.Pens)
      ELSE
        SELF.Brush.Id = MetaRecord.Params[1]
        GET(SELF.Brush,SELF.Brush.Id)
        IF ~ERRORCODE() THEN
          DELETE(SELF.Brush)
        ELSE
          SELF.Errors.SetField('Brush')
          RETURN SELF.Errors.Throw(Msg:WMFObjectIdNotFound)
        END
      END
    END
  OF META:DIBBITBLT
  OF META:DIBCREATEPATTERNBRUSH
  OF META:DIBSTRETCHBLT
  OF META:DRAWTEXT
  OF META:ELLIPSE
    SELF.EllipseFormat.Pos.Top = (SELF.WindowOriginY + MetaRecord.ShortGrp.Param3)*SELF.ToInch
    SELF.EllipseFormat.Pos.Left = (SELF.WindowOriginX + MetaRecord.ShortGrp.Param4)*SELF.ToInch
    SELF.EllipseFormat.Pos.Bottom = (SELF.WindowOriginY + MetaRecord.ShortGrp.Param1)*SELF.ToInch
    SELF.EllipseFormat.Pos.Right = (SELF.WindowOriginX + MetaRecord.ShortGrp.Param2)*SELF.ToInch
    SELF.Brush.ID = SELF.DC.CurrentBrush
    GET(SELF.Brush, SELF.Brush.ID)
    IF ERRORCODE()
      SELF.Errors.SetField('Brush')
      RETURN SELF.Errors.Throw(Msg:WMFObjectIdNotFound)
    END
    SELF.EllipseFormat.Style.BackgroundColor = SELF.Brush.Color
    SELF.EllipseFormat.Style.BackgroundStyle = SELF.Brush.Style
    SELF.Pens.ID = SELF.DC.CurrentPen
    GET(SELF.Pens, SELF.Pens.ID)
    IF ERRORCODE()
      SELF.Errors.SetField('Pen')
      RETURN SELF.Errors.Throw(Msg:WMFObjectIdNotFound)
    END
    SELF.EllipseFormat.Style.BorderStyle = SELF.Pens.Style
    SELF.EllipseFormat.Style.BorderColor = SELF.Pens.Color
    SELF.EllipseFormat.Style.BorderWidth = SELF.Pens.Width
    CASE SELF.InsideRadio
    OF 1
      SELF.RadioFormat.Outer = SELF.EllipseFormat
      SELF.InsideRadio += 1
    OF 2
      SELF.RadioFormat.Inner = SELF.EllipseFormat
      SELF.RadioFormat.Checked = TRUE
    END
  OF META:ENDDOC
  OF META:ENDPAGE
  OF META:ESCAPE
    IF MetaRecord.EscapeGrp.Magic = 1319
      CASE MetaRecord.EscapeGrp.Type
      OF ESCType:ImageName
        SELF.TempText = WMFF:Buffer[39 : 39 + MetaRecord.EscapeGrp.slen - 1]
        SELF.TempTextLen = MetaRecord.EscapeGrp.slen
        DO ResolveFilePath
      OF ESCType:ImageInfo
        TmpCString = WMFF:Buffer[25 : 25 + FILE:MaxFilePath]
        SELF.TempText = TmpCString
        SELF.TempTextLen = LEN(CLIP(SELF.TempText))
        DO ResolveFilePath
      OF ESCType:StartControl
        SELF.Comment = WMFF:Buffer[39 : 39 + MetaRecord.EscapeGrp.slen - 1]
        SELF.TempText = ''
        SELF.TempTextLen = 0
        CASE MetaRecord.EscapeGrp.CtlName
        OF 'ARC'
          SELF.InsideArc = TRUE
          CLEAR(SELF.ArcFormat)
        OF 'BOX'
          SELF.InsideBox = TRUE
          CLEAR(SELF.RectFormat)
        OF 'CHECK'
          SELF.InsideCheck = TRUE
          CLEAR(SELF.CheckFormat)
        OF 'CHORD'
          SELF.InsideChord = TRUE
          CLEAR(SELF.ArcFormat)
        OF 'ELLIPSE'
          SELF.InsideEllipse = TRUE
          CLEAR(SELF.EllipseFormat)
        OF 'LINE'
          SELF.InsideLine = TRUE
          CLEAR(SELF.LineFormat)
        OF 'IMAGE'
          SELF.InsideImage = TRUE
          CLEAR(SELF.ImageFormat)
        OF 'OPTION'
        OROF 'GROUP'
          SELF.InsideGroup = TRUE
          CLEAR(SELF.GroupFormat)
        OF 'RADIO'
          SELF.InsideRadio = TRUE
          CLEAR(SELF.RadioFormat)
        OF 'STRING'
        OROF 'SMARTSTRING'
          SELF.InsideString = TRUE
          CLEAR(SELF.StringFormat)
        OF 'TEXT'
        OROF 'RTF'
        OROF 'SPLITTEXT'
          SELF.InsideText = TRUE
          IF MetaRecord.EscapeGrp.isSplit
            SELF.Comment = SELF.PreserveComment
          END
        OF 'PIE'
          SELF.InsidePie = TRUE
          SELF.FirstSlice = TRUE
        OF 'POLYGON'
          SELF.InsidePolygon = TRUE
        END
      OF ESCType:EndControl
        CASE MetaRecord.EscapeGrp.CtlName
        OF 'ARC'
          IF SELF.InsideArc
            SELF.ReportGenerator.ProcessArc(SELF.ArcFormat, SELF.Comment)
            SELF.InsideArc = FALSE
          ELSE
            SELF.Errors.SetField(MetaRecord.EscapeGrp.CtlName)
            RETURN SELF.Errors.Throw(Msg:WMFInvalidControl)
          END
        OF 'BOX'
          IF SELF.InsideBox
            SELF.ReportGenerator.ProcessRectangle(SELF.RectFormat, SELF.Comment)
            SELF.InsideBox = FALSE
          ELSE
            SELF.Errors.SetField(MetaRecord.EscapeGrp.CtlName)
            RETURN SELF.Errors.Throw(Msg:WMFInvalidControl)
          END
        OF 'CHECK'
          IF SELF.InsideCheck
            SELF.ReportGenerator.ProcessCheck(SELF.CheckFormat, SELF.TempText[1:SELF.TempTextLen], SELF.Comment)
            SELF.InsideCheck = FALSE
          ELSE
            SELF.Errors.SetField(MetaRecord.EscapeGrp.CtlName)
            RETURN SELF.Errors.Throw(Msg:WMFInvalidControl)
          END
        OF 'CHORD'
          IF SELF.InsideChord
            SELF.ReportGenerator.ProcessChord(SELF.ArcFormat, SELF.Comment)
            SELF.InsideChord = FALSE
          ELSE
            SELF.Errors.SetField(MetaRecord.EscapeGrp.CtlName)
            RETURN SELF.Errors.Throw(Msg:WMFInvalidControl)
          END
        OF 'ELLIPSE'
          IF SELF.InsideEllipse
            SELF.ReportGenerator.ProcessEllipse(SELF.EllipseFormat, SELF.Comment)
            SELF.InsideEllipse = FALSE
          ELSE
            SELF.Errors.SetField(MetaRecord.EscapeGrp.CtlName)
            RETURN SELF.Errors.Throw(Msg:WMFInvalidControl)
          END
        OF 'LINE'
          IF SELF.InsideLine
            SELF.ReportGenerator.ProcessLine(SELF.LineFormat, SELF.Comment)
            SELF.InsideLine = FALSE
          ELSE
            SELF.Errors.SetField(MetaRecord.EscapeGrp.CtlName)
            RETURN SELF.Errors.Throw(Msg:WMFInvalidControl)
          END
        OF 'IMAGE'
          IF SELF.InsideImage
            IF CLIP(SELF.TempText[1 : SELF.TempTextLen])
               IF SELF.ImageFormat.EmbedStart>0 OR SELF._SkipInvalidExternalImages=False OR (SELF.ImageFormat.EmbedStart<=0 AND EXISTS(CLIP(SELF.TempText[1 : SELF.TempTextLen])))
                  SELF.ReportGenerator.ProcessImage(SELF.ImageFormat, SELF.TempText, SELF.Comment)
               END     
            END
            SELF.InsideImage = FALSE
          ELSE
            SELF.Errors.SetField(MetaRecord.EscapeGrp.CtlName)
            RETURN SELF.Errors.Throw(Msg:WMFInvalidControl)
          END
        OF 'OPTION'
        OROF 'GROUP'
          IF SELF.InsideGroup
            SELF.ReportGenerator.ProcessGroup(SELF.GroupFormat, SELF.TempText[1:SELF.TempTextLen], SELF.Comment)
            SELF.InsideGroup = FALSE
          ELSE
            SELF.Errors.SetField(MetaRecord.EscapeGrp.CtlName)
            RETURN SELF.Errors.Throw(Msg:WMFInvalidControl)
          END
        OF 'RADIO'
          IF SELF.InsideRadio
            SELF.ReportGenerator.ProcessRadio(SELF.RadioFormat, SELF.TempText[1:SELF.TempTextLen], SELF.Comment)
            SELF.InsideRadio = FALSE
          ELSE
            SELF.Errors.SetField(MetaRecord.EscapeGrp.CtlName)
            RETURN SELF.Errors.Throw(Msg:WMFInvalidControl)
          END
        OF 'STRING'
        OROF 'SMARTSTRING'
          IF SELF.InsideString
            SELF.ReportGenerator.ProcessString(SELF.StringFormat, SELF.TempText[1:SELF.TempTextLen], SELF.Comment)
            SELF.InsideString = FALSE
          ELSE
            SELF.Errors.SetField(MetaRecord.EscapeGrp.CtlName)
            RETURN SELF.Errors.Throw(Msg:WMFInvalidControl)
          END
        OF 'TEXT'
        OROF 'RTF'
        OROF 'SPLITTEXT'
          IF SELF.InsideText
            IF MetaRecord.EscapeGrp.isSplit
              SELF.PreserveComment = SELF.Comment
            END
            SELF.ReportGenerator.ProcessText(SELF.TextFormatQ, SELF.Comment)
            FREE(SELF.TextFormatQ)
            SELF.InsideText = FALSE
          ELSE
            IF NOT MetaRecord.EscapeGrp.isSplit
              SELF.Errors.SetField(MetaRecord.EscapeGrp.CtlName)
              RETURN SELF.Errors.Throw(Msg:WMFInvalidControl)
            END
          END
        OF 'PIE'
          IF SELF.InsidePie
            SELF.ReportGenerator.ProcessPie(SELF.SliceFormatQ, SELF.Pos, SELF.Comment)
            FREE(SELF.SliceFormatQ)
            SELF.InsidePie = FALSE
          ELSE
            SELF.Errors.SetField(MetaRecord.EscapeGrp.CtlName)
            RETURN SELF.Errors.Throw(Msg:WMFInvalidControl)
          END
        OF 'POLYGON'
          IF SELF.InsidePolygon
            SELF.ReportGenerator.ProcessPolygon(SELF.PointQ, SELF.Style, SELF.Comment)
            FREE(SELF.PointQ)
            SELF.InsidePolygon = FALSE
          ELSE
            SELF.Errors.SetField(MetaRecord.EscapeGrp.CtlName)
            RETURN SELF.Errors.Throw(Msg:WMFInvalidControl)
          END
        END
      OF ESCType:StartBand
        BandName = WMFF:Buffer[15 : 24]
        SELF.ReportGenerator.ProcessBand(BandName, TRUE);
      OF ESCType:EndBand
        BandName = WMFF:Buffer[15 : 24]
        SELF.ReportGenerator.ProcessBand(BandName, FALSE);
      END
    END
  OF META:EXCLUDECLIPRECT
  OF META:EXTFLOODFILL
  OF META:EXTTEXTOUT
    CLEAR(SELF.StringFormat)
    TextStart = 15
    TextLen = MetaRecord.Params[3]
    SELF.StringFormat.topText = (SELF.WindowOriginY + MetaRecord.ShortGrp.Param1)*SELF.ToInch
    SELF.StringFormat.leftText = (SELF.WindowOriginX + MetaRecord.ShortGrp.Param2)*SELF.ToInch
    IF MetaRecord.Params[4]
      TextStart += 8
    END
    IF BAND(MetaRecord.Params[4],ETO:CLIPPED+ETO:OPAQUE)
      SELF.StringFormat.Pos.Left   = (SELF.WindowOriginX + MetaRecord.ShortGrp.Param5)*SELF.ToInch
      SELF.StringFormat.Pos.Top    = (SELF.WindowOriginY + MetaRecord.ShortGrp.Param6)*SELF.ToInch
      SELF.StringFormat.Pos.Right  = (SELF.WindowOriginX + MetaRecord.ShortGrp.Param7)*SELF.ToInch
      SELF.StringFormat.Pos.Bottom = (SELF.WindowOriginY + MetaRecord.ShortGrp.Param8)*SELF.ToInch
    ELSE
      SELF.StringFormat.Pos.Left   = (SELF.WindowOriginX + MetaRecord.ShortGrp.Param2)*SELF.ToInch
      SELF.StringFormat.Pos.Top    = (SELF.WindowOriginY + MetaRecord.ShortGrp.Param1)*SELF.ToInch
      SELF.StringFormat.Pos.Right  = (SELF.WindowOriginX + MetaRecord.ShortGrp.Param2 + (TextLen * SELF.Fonts.Size * 5.8))*SELF.ToInch
      SELF.StringFormat.Pos.Bottom = (SELF.WindowOriginY + MetaRecord.ShortGrp.Param1 + (SELF.Fonts.Size * 9.5))*SELF.ToInch
    END
    IF TextLen > 0 AND MetaRecord.Size * 2 >= TextStart
      SELF.Fonts.ID = SELF.DC.CurrentFont
      GET(SELF.Fonts,SELF.Fonts.ID)
      IF ERRORCODE()
        SELF.Errors.SetField('Fonts')
        RETURN SELF.Errors.Throw(Msg:WMFObjectIdNotFound)
      END
      SELF.StringFormat.Size   = SELF.Fonts.Size
      SELF.StringFormat.Style  = SELF.Fonts.Style
      SELF.StringFormat.CharSet= SELF.Fonts.CharSet
      SELF.StringFormat.Face   = SELF.Fonts.Face
      SELF.StringFormat.Angle  = SELF.Fonts.Angle
      IF SELF.DC.TextColor <> COLOR:None
        SELF.StringFormat.Color = SELF.DC.TextColor
      ELSE
        SELF.StringFormat.Color = SELF.Fonts.Color
      END
      IF SELF.DC.BKMode = TRANSPARENT
        SELF.StringFormat.BackgroundColor = COLOR:None
      ELSE
        SELF.StringFormat.BackgroundColor = SELF.DC.BKColor
      END
      SELF.TempText = WMFF:Buffer[TextStart : TextStart + TextLen - 1]
      SELF.TempTextLen = TextLen
      IF SELF.InsideGroup
        SELF.GroupFormat.Header = SELF.StringFormat
      ELSIF SELF.InsideCheck
        SELF.CheckFormat.Prompt = SELF.StringFormat
      ELSIF SELF.InsideRadio
        SELF.RadioFormat.Prompt = SELF.StringFormat
      ELSIF SELF.InsideText
        SELF.TextFormatQ.Text = WMFF:Buffer[TextStart : TextStart + TextLen - 1]
        SELF.TextFormatQ.Format = SELF.StringFormat
        ADD(SELF.TextFormatQ)
      END
    END
  OF META:FILLREGION
  OF META:FLOODFILL
  OF META:FRAMEREGION
  OF META:INTERSECTCLIPRECT
  OF META:INVERTREGION
  OF META:LINETO
    CASE SELF.InsideGroup
    OF 1
      SELF.Pens.ID = SELF.DC.CurrentPen
      GET(SELF.Pens, SELF.Pens.ID)
      IF ERRORCODE()
        SELF.Errors.SetField('Pen')
        RETURN SELF.Errors.Throw(Msg:WMFObjectIdNotFound)
      END
      SELF.GroupFormat.LineStyle = SELF.Pens.Style
      SELF.GroupFormat.LineColor = SELF.Pens.Color
      SELF.GroupFormat.LineWidth = SELF.Pens.Width
      SELF.GroupFormat.Pos.Left = (SELF.WindowOriginX + MetaRecord.ShortGrp.Param2)*SELF.ToInch
      IF SELF.GroupFormat.Pos.Left = SELF.GroupFormat.X1
        SELF.GroupFormat.Pos.Bottom = (SELF.WindowOriginY + MetaRecord.ShortGrp.Param1)*SELF.ToInch
        SELF.InsideGroup += 2
      ELSE
        SELF.InsideGroup += 1
      END
    OF 2
      SELF.GroupFormat.Pos.Bottom = (SELF.WindowOriginY + MetaRecord.ShortGrp.Param1)*SELF.ToInch
      SELF.InsideGroup += 1
    OF 3
      SELF.GroupFormat.Pos.Right = (SELF.WindowOriginX + MetaRecord.ShortGrp.Param2)*SELF.ToInch
      SELF.InsideGroup += 1
    OF 4
      SELF.InsideGroup += 1
    OF 5
      SELF.GroupFormat.X2 = (SELF.WindowOriginX + MetaRecord.ShortGrp.Param2)*SELF.ToInch
    ELSE
      IF SELF.InsideCheck
        SELF.CheckFormat.Checked = TRUE
      ELSE
        SELF.LineFormat.Pos.Left = SELF.LineStartXPos
        SELF.LineFormat.Pos.Top = SELF.LineStartYPos
        SELF.LineFormat.Pos.Right = (SELF.WindowOriginX + MetaRecord.ShortGrp.Param2)*SELF.ToInch
        SELF.LineFormat.Pos.Bottom = (SELF.WindowOriginY + MetaRecord.ShortGrp.Param1)*SELF.ToInch
        SELF.Pens.ID = SELF.DC.CurrentPen
        GET(SELF.Pens, SELF.Pens.ID)
        IF ERRORCODE()
          SELF.Errors.SetField('Pen')
          RETURN SELF.Errors.Throw(Msg:WMFObjectIdNotFound)
        END
        SELF.LineFormat.Style = SELF.Pens.Style
        SELF.LineFormat.Color = SELF.Pens.Color
        SELF.LineFormat.Width = SELF.Pens.Width
      END
      SELF.LineStartXPos = (SELF.WindowOriginX + MetaRecord.ShortGrp.Param2)*SELF.ToInch
      SELF.LineStartYPos = (SELF.WindowOriginY + MetaRecord.ShortGrp.Param1)*SELF.ToInch
    END
  OF META:MOVETO
    IF SELF.InsideGroup
      SELF.GroupFormat.X1 = (SELF.WindowOriginX + MetaRecord.ShortGrp.Param2)*SELF.ToInch
      SELF.GroupFormat.Pos.Top = (SELF.WindowOriginY + MetaRecord.ShortGrp.Param1)*SELF.ToInch
    ELSE
      SELF.LineStartXPos = (SELF.WindowOriginX + MetaRecord.ShortGrp.Param2)*SELF.ToInch
      SELF.LineStartYPos = (SELF.WindowOriginY + MetaRecord.ShortGrp.Param1)*SELF.ToInch
    END
  OF META:OFFSETCLIPRGN
  OF META:OFFSETVIEWPORTORG
  OF META:OFFSETWINDOWORG
  OF META:PAINTREGION
  OF META:PATBLT
  OF META:PIE
    IF SELF.InsidePie
      SELF.Brush.ID = SELF.DC.CurrentBrush
      GET(SELF.Brush, SELF.Brush.ID)
      IF ERRORCODE()
        SELF.Errors.SetField('Brush')
        RETURN SELF.Errors.Throw(Msg:WMFObjectIdNotFound)
      END
      SELF.SliceFormatQ.Format.Style.BackgroundColor = SELF.Brush.Color
      SELF.SliceFormatQ.Format.Style.BackgroundStyle = SELF.Brush.Style
      SELF.Pens.ID = SELF.DC.CurrentPen
      GET(SELF.Pens, SELF.Pens.ID)
      IF ERRORCODE()
        SELF.Errors.SetField('Pen')
        RETURN SELF.Errors.Throw(Msg:WMFObjectIdNotFound)
      END
      SELF.SliceFormatQ.Format.Style.BorderStyle = SELF.Pens.Style
      SELF.SliceFormatQ.Format.Style.BorderColor = SELF.Pens.Color
      SELF.SliceFormatQ.Format.Style.BorderWidth = SELF.Pens.Width
      IF SELF.FirstSlice
        SELF.Pos.Top = (SELF.WindowOriginY + MetaRecord.ShortGrp.Param7)*SELF.ToInch
        SELF.Pos.Left = (SELF.WindowOriginX + MetaRecord.ShortGrp.Param8)*SELF.ToInch
        SELF.Pos.Bottom = (SELF.WindowOriginY + MetaRecord.ShortGrp.Param5)*SELF.ToInch
        SELF.Pos.Right = (SELF.WindowOriginX + MetaRecord.ShortGrp.Param6)*SELF.ToInch
        SELF.FirstSlice = FALSE
      END
      SELF.SliceFormatQ.Format.StartX = (SELF.WindowOriginX + MetaRecord.ShortGrp.Param4)*SELF.ToInch
      SELF.SliceFormatQ.Format.StartY = (SELF.WindowOriginY + MetaRecord.ShortGrp.Param3)*SELF.ToInch
      SELF.SliceFormatQ.Format.EndX = (SELF.WindowOriginX + MetaRecord.ShortGrp.Param2)*SELF.ToInch
      SELF.SliceFormatQ.Format.EndY = (SELF.WindowOriginY + MetaRecord.ShortGrp.Param1)*SELF.ToInch
      ADD(SELF.SliceFormatQ)
    END
  OF META:POLYGON
    IF SELF.InsidePolygon
      SELF.Brush.ID = SELF.DC.CurrentBrush
      GET(SELF.Brush, SELF.Brush.ID)
      IF ERRORCODE()
        SELF.Errors.SetField('Brush')
        RETURN SELF.Errors.Throw(Msg:WMFObjectIdNotFound)
      END
      SELF.Style.BackgroundColor = SELF.Brush.Color
      SELF.Style.BackgroundStyle = SELF.Brush.Style
      SELF.Pens.ID = SELF.DC.CurrentPen
      GET(SELF.Pens, SELF.Pens.ID)
      IF ERRORCODE()
        SELF.Errors.SetField('Pen')
        RETURN SELF.Errors.Throw(Msg:WMFObjectIdNotFound)
      END
      SELF.Style.BorderStyle = SELF.Pens.Style
      SELF.Style.BorderColor = SELF.Pens.Color
      SELF.Style.BorderWidth = SELF.Pens.Width
      LOOP I = 1 TO MetaRecord.Params[1]
        SELF.PointQ.XPos = (SELF.WindowOriginX + MetaRecord.Params[I*2])*SELF.ToInch
        SELF.PointQ.YPos = (SELF.WindowOriginY + MetaRecord.Params[I*2+1])*SELF.ToInch
        ADD(SELF.PointQ)
      END
    END
  OF META:POLYLINE
  OF META:POLYPOLYGON
  OF META:REALIZEPALETTE
  OF META:RECTANGLE
    IF SELF.InsideCheck
      SELF.CheckFormat.Prompt = SELF.StringFormat
      SELF.CheckFormat.Pos.Top = (SELF.WindowOriginY + MetaRecord.ShortGrp.Param3)*SELF.ToInch
      SELF.CheckFormat.Pos.Left = (SELF.WindowOriginX + MetaRecord.ShortGrp.Param4)*SELF.ToInch
      SELF.CheckFormat.Pos.Bottom = (SELF.WindowOriginY + MetaRecord.ShortGrp.Param1)*SELF.ToInch
      SELF.CheckFormat.Pos.Right = (SELF.WindowOriginX + MetaRecord.ShortGrp.Param2)*SELF.ToInch
      SELF.Brush.ID = SELF.DC.CurrentBrush
      GET(SELF.Brush, SELF.Brush.ID)
      IF ERRORCODE()
        SELF.Errors.SetField('Brush')
        RETURN SELF.Errors.Throw(Msg:WMFObjectIdNotFound)
      END
      SELF.CheckFormat.Style.BackgroundColor = SELF.Brush.Color
      SELF.CheckFormat.Style.BackgroundStyle = SELF.Brush.Style
      SELF.Pens.ID = SELF.DC.CurrentPen
      GET(SELF.Pens, SELF.Pens.ID)
      IF ERRORCODE()
        SELF.Errors.SetField('Pen')
        RETURN SELF.Errors.Throw(Msg:WMFObjectIdNotFound)
      END
      SELF.CheckFormat.Style.BorderStyle = SELF.Pens.Style
      SELF.CheckFormat.Style.BorderColor = SELF.Pens.Color
      SELF.CheckFormat.Style.BorderWidth = SELF.Pens.Width
    ELSIF SELF.InsideBox
       CLEAR(SELF.RectFormat)
       SELF.RectFormat.ell_width = 0
       SELF.RectFormat.ell_height = 0
       SELF.RectFormat.Pos.Top = (SELF.WindowOriginY + MetaRecord.ShortGrp.Param3)*SELF.ToInch
       SELF.RectFormat.Pos.Left = (SELF.WindowOriginX + MetaRecord.ShortGrp.Param4)*SELF.ToInch
       SELF.RectFormat.Pos.Bottom = (SELF.WindowOriginY + MetaRecord.ShortGrp.Param1)*SELF.ToInch
       SELF.RectFormat.Pos.Right = (SELF.WindowOriginX + MetaRecord.ShortGrp.Param2)*SELF.ToInch
       SELF.Brush.ID = SELF.DC.CurrentBrush
       GET(SELF.Brush, SELF.Brush.ID)
       IF ERRORCODE()
         SELF.Errors.SetField('Brush')
         RETURN SELF.Errors.Throw(Msg:WMFObjectIdNotFound)
       END
       SELF.RectFormat.Style.BackgroundColor = SELF.Brush.Color
       SELF.RectFormat.Style.BackgroundStyle = SELF.Brush.Style
       SELF.Pens.ID = SELF.DC.CurrentPen
       GET(SELF.Pens, SELF.Pens.ID)
       IF ERRORCODE()
         SELF.Errors.SetField('Pen')
         RETURN SELF.Errors.Throw(Msg:WMFObjectIdNotFound)
       END
       SELF.RectFormat.Style.BorderStyle = SELF.Pens.Style
       SELF.RectFormat.Style.BorderColor = SELF.Pens.Color
       SELF.RectFormat.Style.BorderWidth = SELF.Pens.Width
    END
  OF META:RESETDC
  OF META:RESIZEPALETTE
  OF META:RESTOREDC
    GET(SELF.DC, RECORDS(SELF.DC))
    IF ERRORCODE()
      SELF.Errors.SetField('Device Context')
      RETURN SELF.Errors.Throw(Msg:WMFObjectIdNotFound)
    END
    DELETE(SELF.DC)
  OF META:ROUNDRECT
    CLEAR(SELF.RectFormat)
    SELF.RectFormat.ell_width = MetaRecord.Params[1]*SELF.ToInch
    SELF.RectFormat.ell_height = MetaRecord.Params[2]*SELF.ToInch
    SELF.RectFormat.Pos.Top = (SELF.WindowOriginY + MetaRecord.ShortGrp.Param5)*SELF.ToInch
    SELF.RectFormat.Pos.Left = (SELF.WindowOriginX + MetaRecord.ShortGrp.Param6)*SELF.ToInch
    SELF.RectFormat.Pos.Bottom = (SELF.WindowOriginY + MetaRecord.ShortGrp.Param3)*SELF.ToInch
    SELF.RectFormat.Pos.Right = (SELF.WindowOriginX + MetaRecord.ShortGrp.Param4)*SELF.ToInch
    SELF.Brush.ID = SELF.DC.CurrentBrush
    GET(SELF.Brush, SELF.Brush.ID)
    IF ERRORCODE()
      SELF.Errors.SetField('Brush')
      RETURN SELF.Errors.Throw(Msg:WMFObjectIdNotFound)
    END
    SELF.RectFormat.Style.BackgroundColor = SELF.Brush.Color
    SELF.RectFormat.Style.BackgroundStyle = SELF.Brush.Style
    SELF.Pens.ID = SELF.DC.CurrentPen
    GET(SELF.Pens, SELF.Pens.ID)
    IF ERRORCODE()
      SELF.Errors.SetField('Pen')
      RETURN SELF.Errors.Throw(Msg:WMFObjectIdNotFound)
    END
    SELF.RectFormat.Style.BorderStyle = SELF.Pens.Style
    SELF.RectFormat.Style.BorderColor = SELF.Pens.Color
    SELF.RectFormat.Style.BorderWidth = SELF.Pens.Width
  OF META:SAVEDC
    ADD(SELF.DC)
  OF META:SCALEVIEWPORTEXT
  OF META:SCALEWINDOWEXT
  OF META:SELECTCLIPREGION
  OF META:SELECTOBJECT
    SELF.Objects.ID = MetaRecord.Params[1]
    GET(SELF.Objects, SELF.Objects.ID)
    IF ERRORCODE()
      SELF.Errors.SetField('object')
      RETURN SELF.Errors.Throw(Msg:WMFObjectIdNotFound)
    END
    CASE SELF.Objects.Type
    OF Type:Font
      SELF.DC.CurrentFont = SELF.Objects.ID
    OF Type:Pen
      SELF.DC.CurrentPen = SELF.Objects.ID
    OF Type:Brush
      SELF.DC.CurrentBrush = SELF.Objects.ID
    END
  OF META:SELECTPALETTE
  OF META:SETBKCOLOR
    SELF.DC.BKColor = MetaRecord.LongGrp.Param1
  OF META:SETBKMODE
    SELF.DC.BKMode = MetaRecord.LongGrp.Param1
  OF META:SETDIBTODEV
  OF META:SETMAPMODE
  OF META:SETMAPPERFLAGS
  OF META:SETPALENTRIES
  OF META:SETPIXEL
  OF META:SETPOLYFILLMODE
  OF META:SETRELABS
  OF META:SETROP2
  OF META:SETSTRETCHBLTMODE
    SELF.InsideImage = TRUE
    SELF.ImageFormat.StretchMode = MetaRecord.Params[1]
  OF META:SETTEXTALIGN
    SELF.DC.TextAlign = MetaRecord.LongGrp.Param1
  OF META:SETTEXTCHAREXTRA
  OF META:SETTEXTCOLOR
    SELF.DC.TextColor = MetaRecord.LongGrp.Param1
  OF META:SETTEXTJUSTIFICATION
  OF META:SETVIEWPORTEXT
  OF META:SETVIEWPORTORG
  OF META:SETWINDOWEXT
  OF META:SETWINDOWORG
     SELF.WindowOriginX = -MetaRecord.ShortGrp.Param2
     SELF.WindowOriginY = -MetaRecord.ShortGrp.Param1
  OF META:STARTDOC
  OF META:STARTPAGE
  OF META:STRETCHBLT
  OF META:STRETCHDIB
    SELF.ImageFormat.Pos.Top    = (SELF.WindowOriginY + MetaRecord.StretchDIBitsGrp.dstY)*SELF.ToInch
    SELF.ImageFormat.Pos.Bottom = (SELF.WindowOriginY + MetaRecord.StretchDIBitsGrp.dstY + MetaRecord.StretchDIBitsGrp.dstYExt)*SELF.ToInch

    SELF.ImageFormat.Pos.Left   = (SELF.WindowOriginX + MetaRecord.StretchDIBitsGrp.dstX)*SELF.ToInch
    SELF.ImageFormat.Pos.Right  = (SELF.WindowOriginX + MetaRecord.StretchDIBitsGrp.dstX + MetaRecord.StretchDIBitsGrp.dstXExt)*SELF.ToInch
    SELF.ImageFormat.EmbedStart = LOC:RecordStart+28
  OF META:TEXTOUT
  ELSE
  END
  RETURN Level:Benign


ResolveFilePath  ROUTINE
  DATA
LOC:Path             CSTRING(FILE:MaxFilePath),AUTO
LOC:Drive            CSTRING(FILE:MaxFilePath),AUTO
LOC:Dir              CSTRING(FILE:MaxFilePath),AUTO
LOC:Name             CSTRING(FILE:MaxFilePath),AUTO
LOC:Extension        CSTRING(FILE:MaxFilePath),AUTO
  CODE
  LOC:Path = SELF.TempText[1:SELF.TempTextLen]
  PathSplit(LOC:Path, LOC:Drive, LOC:Dir, LOC:Name, LOC:Extension)
  IF LOC:Drive = '' AND LOC:Dir = ''
    LOC:Path = SYSTEM{PROP:TempImagePath} & LOC:Name & CLIP(LOC:Extension)
    SELF.TempText = LOC:Path
    SELF.TempTextLen = LEN (LOC:Path)
  END
  EXIT

WMFDocumentParser.OpenDocument   PROCEDURE
ret BYTE,AUTO
  CODE
    ret = TRUE
    IF SELF.ReportGenerator.SupportPageProcessing()
       SELF.Parser.Init(SELF.WMFQueue.Filename, SELF.ReportGenerator, SELF.Errors)
       ret = SELF.Parser.OpenFile()
    END
    IF ret = Level:Benign
       ret = SELF.ReportGenerator.OpenPage(SELF.WMFQueue.Filename)
    END
    RETURN ret

WMFDocumentParser.CloseDocument   PROCEDURE
ret BYTE,AUTO
  CODE
    ret = SELF.ReportGenerator.ClosePage()
    IF SELF.ReportGenerator.SupportPageProcessing()
       IF ret = Level:Benign
         ret = SELF.Parser.CloseFile()
       END
       SELF.Parser.Kill()
    END
    RETURN ret

WMFDocumentParser.ProcessDocument   PROCEDURE
ret BYTE,AUTO
  CODE
     IF NOT SELF.ReportGenerator.SupportPageProcessing()
        RETURN Level:Benign
     END
     ret = SELF.Parser.ProcessHeader()
     IF ret = Level:Benign
        ret = SELF.ReportGenerator.StartPageProcess(SELF.Parser.GetBoxLeft(),SELF.Parser.GetBoxTop(),SELF.Parser.GetBoxRight(),SELF.Parser.GetBoxBottom(),SELF.WMFQueue.Filename)
        IF ret = Level:Benign
           IF SELF.ReportGenerator.SupportPageParsing()
              ret = SELF.Parser.Run()
           END
        END
     END
    RETURN ret

WMFDocumentParser.GenerateReport PROCEDURE(BYTE ShowProgress)
idx          UNSIGNED(0)
ret          BYTE,AUTO
TotalPages   UNSIGNED
Progress:Thermometer BYTE

ProgressWindow WINDOW('Progress...'),AT(,,160,47),FONT('Arial',8,,),CENTER,TIMER(1), |
         GRAY,DOUBLE
       IMAGE(),AT(6,7),USE(?ImageGenerator)
       STRING('Exporting to'),AT(33,8,114,10),USE(?Progress:UserString),LEFT
       PROGRESS,USE(Progress:Thermometer),SMOOTH,AT(4,31,152,7),RANGE(0,100)
       STRING('Page 0 of '),AT(33,17,114,10),USE(?Progress:PctText),TRN,LEFT
     END

  CODE
    SELF.Parser &= NEW(WMFParser)
    TotalPages = RECORDS(SELF.WMFQueue)
    ret = SELF.ReportGenerator.OpenDocument(TotalPages)
    IF ret = Level:Benign THEN
      IF ShowProgress THEN
        OPEN(ProgressWindow)
        ?ImageGenerator{PROP:TEXT}=SELF.ReportGenerator.DisplayIcon()
        ?Progress:UserString{PROP:TEXT}='Exporting to '&SELF.ReportGenerator.DisplayName()
        ?Progress:PctText{PROP:TEXT}='Page 0 of '&TotalPages
        ACCEPT
          CASE EVENT()
          OF Event:OpenWindow
          OF EVENT:Timer
            idx += 1
            ProgressWindow{PROP:Timer} = 0
            GET(SELF.WMFQueue, idx)
            IF ERRORCODE() OR TotalPages < idx THEN
              POST(Event:CloseWindow)
            END
            ?Progress:PctText{PROP:Text} = 'Page ' & idx & ' of ' & TotalPages
            Progress:Thermometer = MulDiv (idx, 100, TotalPages)
            DISPLAY(?Progress:Thermometer,?Progress:PctText)
            DO ProcessDocument
            IF ret <> Level:Benign THEN
              POST(Event:CloseWindow)
            END
            IF TotalPages = idx THEN
              ret = SELF.ReportGenerator.CloseDocument()
              POST(Event:CloseWindow)
            ELSE
              ProgressWindow{PROP:Timer}=1
            END
          END
        END
        CLOSE(ProgressWindow)
      ELSE
        LOOP idx = 1 TO TotalPages
          GET(SELF.WMFQueue, idx)
          IF ERRORCODE() THEN BREAK.
          DO ProcessDocument
          IF ret <> Level:Benign THEN BREAK.
          IF TotalPages = idx THEN
            ret = SELF.ReportGenerator.CloseDocument()
          END
        END
      END
    END
!   SELF.Parser.DESTRUCT()
    DISPOSE(SELF.Parser)
    RETURN ret

ProcessDocument ROUTINE
    ret = SELF.OpenDocument()
    IF ret = Level:Benign
      ret = SELF.ProcessDocument()
      IF ret = Level:Benign
        ret = SELF.CloseDocument()
      END
    END

WMFDocumentParser.Init PROCEDURE(PreviewQueue WMFQueue, IReportGenerator rptgen, <ErrorClass EC>)
  CODE    
    SELF.WMFQueue &= WMFQueue
    SELF.ReportGenerator &= rptgen
    IF NOT OMITTED(4)
       IF SELF.ErrorsCreated
         SELF.Errors.Kill()
         DISPOSE(SELF.Errors)
         SELF.ErrorsCreated = FALSE
       END
       SELF.Errors &= EC
    ELSE
       IF SELF.ErrorsCreated = FALSE
         SELF.ErrorsCreated = TRUE
         SELF.Errors &= NEW(ErrorClass)
         SELF.StatusErrors &=NEW(ErrorStatusClass)
         SELF.Errors.Init(SELF.StatusErrors)
       END
    END

WMFDocumentParser.Destructor          PROCEDURE
  CODE
    IF SELF.ErrorsCreated
       SELF.Errors.Kill()
       DISPOSE(SELF.Errors)
       DISPOSE(SELF.StatusErrors)
    END
