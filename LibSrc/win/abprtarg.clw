    MEMBER

    INCLUDE('ABPRTARG.INC'),ONCE
    PRAGMA('link(C%V%DOS%X%%L%.LIB)')

    MAP
    END


ReportTarget_InFile     FILE,DRIVER('DOS'),PRE(RT_IN),THREAD
                          RECORD
Buffer                     STRING(64000)
                          END
                        END
ReportTarget_OutFile    FILE,DRIVER('DOS'),CREATE,PRE(RT_OUT),THREAD
                          RECORD
Buffer                     STRING(64000)
                          END
                        END

!********************************************

TargetGenerator.Init   PROCEDURE(<STRING TargetFileName>)
  CODE
  IF NOT OMITTED(2)
     SELF.TargetFileName = TargetFileName
  END
  SELF.FPos = 0
  RETURN

TargetGenerator.SetFileNameFromOriginal PROCEDURE (SHORT pOrdinal, SHORT pTotal=0)
  CODE
  ASSERT (CLIP(SELF.OriginalTargetFileName)<>'', 'SetFileNameFromOriginal: The original file name is empty')
  SELF.TargetFileName = SELF.GetOriginalFileName (pOrdinal, pTotal)
  RETURN

TargetGenerator.SetFileNameFromOriginal PROCEDURE()
  CODE
  ASSERT (CLIP(SELF.OriginalTargetFileName)<>'', 'SetFileNameFromOriginal: The original file name is empty')
  SELF.TargetFileName = SELF.GetOriginalFileName()
  RETURN

TargetGenerator.SetOriginalFileName  PROCEDURE (STRING TargetFileName)
  CODE
  SELF.OriginalTargetFileName = TargetFileName
  SELF.SetFileName (TargetFileName)
  RETURN

TargetGenerator.GetExtension   PROCEDURE (STRING FileName)
lLen      UNSIGNED,AUTO
lIndex    UNSIGNED,AUTO
lChar     BYTE,AUTO
  CODE
  lLen = LEN (CLIP (FileName))

  ASSERT (lLen <> 0, 'GetExtension: The file name is empty')

  lIndex = lLen

  LOOP WHILE lIndex <> 0
    lChar = VAL (FileName[lIndex])

    IF lChar = VAL('.')
      RETURN FileName [lIndex + 1 : lLen]
    END

    IF lChar = VAL('\') OR lChar = VAL(':')
      BREAK
    END

    lIndex -= 1
  END

  RETURN ''

TargetGenerator.GetPath   PROCEDURE (STRING FileName)
lIndex    UNSIGNED,AUTO
lChar     BYTE,AUTO
  CODE
  lIndex = LEN (CLIP (FileName))

  ASSERT (lIndex <> 0, 'GetPath: The file name is empty')

  LOOP WHILE lIndex <> 0
    lChar = VAL (FileName[lIndex])

    IF lChar = VAL('\')
      RETURN FileName [1 : lIndex]
    END

    IF lChar = VAL(':')
      RETURN FileName [1 : lIndex] & '.\'
    END

    lIndex -= 1
  END

  RETURN ''


TargetGenerator.GetFileNameNoExt  PROCEDURE(STRING FileName)
lLen      UNSIGNED,AUTO
lIndex    UNSIGNED,AUTO
lChar     BYTE,AUTO
  CODE
  lLen = LEN (CLIP (FileName))

  ASSERT (lLen <> 0, 'GetFileNameNoExt: The file name is empty')

  lIndex = lLen

  LOOP WHILE lIndex <> 0
    lChar = VAL (FileName[lIndex])

    IF lChar = VAL('.')
      RETURN FileName [1 : lIndex - 1]
    END

    IF lChar = VAL('\') OR lChar = VAL(':')
      BREAK
    END

    lIndex -= 1
  END

  RETURN FileName [1 : lLen]

TargetGenerator.GetNameNoExt  PROCEDURE (STRING FileName)
lLen      UNSIGNED,AUTO
Dot       UNSIGNED,AUTO
lIndex    UNSIGNED,AUTO
lChar     BYTE,AUTO
  CODE
  lLen = LEN (CLIP (FileName))

  ASSERT (lLen <> 0, 'GetNameNoExt: The file name is empty')

  Dot = lLen + 1
  lIndex   = lLen

  LOOP WHILE lIndex <> 0
    lChar = VAL (FileName[lIndex])

    IF lChar = VAL('.')
      Dot = lIndex
    END

    IF lChar = VAL('\') OR lChar = VAL(':')
      BREAK
    END

    lIndex -= 1
  END

  RETURN FileName [lIndex + 1 : Dot - 1]

TargetGenerator.GetOriginalFileName  PROCEDURE()
  CODE
  RETURN SELF.OriginalTargetFileName

TargetGenerator.GetOriginalFileName  PROCEDURE (SHORT pOrdinal, SHORT pTotal=0)
lFileName   CSTRING(FILE:MaxFileName)
lFileExt    CSTRING(FILE:MaxFileName)
lLen        UNSIGNED,AUTO
lIndex      UNSIGNED,AUTO
lChar       BYTE,AUTO
  CODE
  lLen = LEN (CLIP (SELF.OriginalTargetFileName))

  ASSERT (lLen <> 0, 'GetOriginalFileName: The original file name is empty')

  IF pOrdinal = 0 THEN
    RETURN SELF.GetOriginalFileName()
  END

  lIndex = lLen

  LOOP WHILE lIndex <> 0
    lChar = VAL (SELF.OriginalTargetFileName[lIndex])

    IF lChar = VAL('.')
      BREAK
    END

    IF lChar = VAL ('\') OR lChar = VAL (':')
      lIndex = 1
    END

    lIndex -= 1
  END

  IF lIndex = 0
    lFileName = CLIP(SELF.OriginalTargetFileName)
    lFileExt  = '.TXT' !If the file has not an extension by default it take the TXT
  ELSIF lIndex = 1
    lFileName = ''
    lFileExt  = CLIP(SELF.OriginalTargetFileName)
  ELSE
    lFileName = SELF.OriginalTargetFileName [1 : lIndex - 1]
    lFileExt  = SELF.OriginalTargetFileName [lIndex : lLen]
  END

  IF pTotal = 0
    RETURN lFileName & '_' & pOrdinal & lFileExt
  ELSE
    RETURN lFileName & '_' & |
           ALL ('0', LEN(LEFT(CLIP(pTotal))) - LEN(LEFT(CLIP(pOrdinal)))) & |
           LEFT (CLIP(pOrdinal)) & lFileExt
  END

TargetGenerator.SetFileName  PROCEDURE (STRING TargetFileName)
  CODE
  SELF.TargetFileName = TargetFileName
  RETURN

TargetGenerator.GetFileName   PROCEDURE()
  CODE
  RETURN SELF.TargetFileName

TargetGenerator.OpenFile   PROCEDURE (BYTE pCreate=1)
  CODE
  SELF.TargetFileHandle = 0
  ReportTarget_OutFile{PROP:Name} = CLIP(SELF.TargetFileName)

  IF pCreate
    CLOSE (ReportTarget_OutFile)
    CREATE(ReportTarget_OutFile)

    IF ERRORCODE() <> 0
      MESSAGE ('ERROR:(' & ERRORCODE() & ') ' & ERROR() & '|Can''t create target file ' & CLIP(SELF.TargetFileName),'Target OpenFile')
      RETURN FALSE
    END
  END

  OPEN (ReportTarget_OutFile, 1)

  IF ERRORCODE() <> 0
    MESSAGE ('ERROR:(' & ERRORCODE() & ') ' & ERROR() & '|Can''t open target file ' & CLIP(SELF.TargetFileName),'Target OpenFile')
    RETURN FALSE
  ELSE
    SELF.TargetFileHandle = 1
    RETURN TRUE
  END

TargetGenerator.CloseFile   PROCEDURE()
  CODE
  IF SELF.TargetFileHandle <> 0
    CLOSE (ReportTarget_OutFile)

    IF ERRORCODE() <> 0
      MESSAGE ('ERROR:(' & ERRORCODE() & ') ' & ERROR() & '|Can''t close target file ' & CLIP(SELF.TargetFileName),'Target CloseFile')
      RETURN FALSE
    END

    SELF.TargetFileHandle = 0
  END
  RETURN TRUE

TargetGenerator.WriteLine  PROCEDURE (STRING str, BYTE pClipStr=0)
  CODE
  RETURN SELF.WriteStr (str & '<13,10>', pClipStr)

TargetGenerator.WriteLong  PROCEDURE (LONG v)
  CODE
  RETURN SELF.WriteULong (v)

TargetGenerator.WriteShort PROCEDURE (SHORT v)
  CODE
  RETURN SELF.WriteUShort (v)

TargetGenerator.WriteULong  PROCEDURE (ULONG v)
LOC:UShort  GROUP,OVER(v)
L              USHORT
H              USHORT
            END
  CODE
  RETURN CHOOSE (SELF.WriteUShort (LOC:UShort.L) AND SELF.WriteUShort (LOC:UShort.H))

TargetGenerator.WriteUShort  PROCEDURE (USHORT v)
LOC:Byte    GROUP,OVER(v)
L              BYTE
H              BYTE
            END
  CODE
  RETURN CHOOSE (SELF.WriteByte (LOC:Byte.L) AND SELF.WriteByte (LOC:Byte.H))

TargetGenerator.DumpStr  PROCEDURE (STRING str, LONG len)
  CODE
  ReportTarget_OutFile.Buffer = str
  APPEND (ReportTarget_OutFile, len)
  RETURN CHOOSE (ERRORCODE() = 0)

TargetGenerator.WriteByte PROCEDURE (BYTE v)
LOC:Buffer           STRING(1),AUTO
LOC:RetCode          BYTE,AUTO
  CODE
  LOC:Buffer[1] = CHR (v)

  IF SELF.StoreStr (LOC:Buffer, SIZE (LOC:Buffer), LOC:RetCode)
    RETURN LOC:RetCode
  END

  SELF.FPos += SIZE (LOC:Buffer)

  IF SELF.TargetFileHandle <> 0
    RETURN SELF.DumpStr (LOC:Buffer, SIZE (LOC:Buffer))
  END

  RETURN FALSE !File is not open

TargetGenerator.StoreStr  PROCEDURE (STRING str, LONG strLen, *BYTE retCode)
  CODE
  RETURN FALSE

TargetGenerator.WriteStr PROCEDURE (STRING str, BYTE pClipStr=0)
LOC:Buffer           LIKE(ReportTarget_OutFile.Buffer),AUTO
LOC:BufferLen        SIGNED,AUTO
LOC:BytesWritten     SIGNED
LOC:RetCode          BYTE,AUTO
  CODE
  LOC:Buffer = str
  LOC:BufferLen = LEN (CHOOSE (NOT pClipStr, str, CLIP (str)))

  IF SELF.StoreStr (LOC:Buffer, LOC:BufferLen, LOC:RetCode)
    RETURN LOC:RetCode
  END

  SELF.FPos += LOC:BufferLen

  IF SELF.TargetFileHandle <> 0
    RETURN SELF.DumpStr (LOC:Buffer, LOC:BufferLen)
  END

  RETURN FALSE !File is not open

TargetGenerator.WriteFileStream  PROCEDURE (STRING pFileName, LONG pStart=1, LONG pLen=0,BYTE pAllowedCharOnly=0)
LOC:FileName         CSTRING(FILE:MaxFileName)
LOC:FileHandle       LONG
LOC:Buffer           LIKE(ReportTarget_OutFile.Buffer),AUTO
LOC:Buffer2          STRING(2)
LOC:BufferLen        SIGNED,AUTO
LOC:BytesRead        SIGNED,AUTO
LOC:BytesWritten     SIGNED
LOC:TotalBytesRead   LONG,AUTO
lIndex               UNSIGNED,AUTO
ReturnValue          LONG
LOC:SFPos            LONG,AUTO
LOC:FilePos          LONG,AUTO
LOC:PrevFileName     CSTRING(FILE:MaxFileName)
LOC:LocalOpen        BYTE,AUTO
  CODE
  LOC:LocalOpen = FALSE

  IF SELF.SourceFileHandle <> 0
    LOC:SFPos = SELF.GetSourceFPos()

    IF UPPER(CLIP(pFileName)) <> UPPER(CLIP(SELF.SourceFileName))
      LOC:PrevFileName = CLIP (SELF.SourceFileName)
      SELF.CloseSourceFile()

      IF NOT SELF.OpenSourceFile (pFileName)
        SELF.OpenSourceFile (LOC:PrevFileName)
        SELF.SetSourceFPos (LOC:SFPos)
        RETURN 0
      END

      LOC:LocalOpen = TRUE
    END
  ELSE
    IF NOT SELF.OpenSourceFile (pFileName)
      RETURN 0
    END

    LOC:SFPos = 0
    LOC:LocalOpen = TRUE
  END

  LOC:BufferLen = SIZE (ReportTarget_OutFile.Buffer)!64000

  IF pLen <> 0 AND pLen < LOC:BufferLen
    LOC:BufferLen = pLen
  END

  LOC:BytesRead = 0
  LOC:TotalBytesRead = 0
  LOC:FilePos = pStart

  SET (ReportTarget_InFile)

  LOOP
    GET (ReportTarget_InFile, LOC:FilePos, LOC:BufferLen)
    IF ERRORCODE() = 0
      LOC:BytesRead = BYTES (ReportTarget_InFile)
      LOC:Buffer [1 : LOC:BytesRead] = ReportTarget_InFile.Buffer [1 : LOC:BytesRead]
      LOC:FilePos += LOC:BytesRead
    ELSE
      LOC:BytesRead = 0
    END
    IF LOC:BytesRead = 0
      BREAK
    END

    LOC:TotalBytesRead += LOC:BytesRead
    IF pLen <> 0
      IF LOC:TotalBytesRead > pLen
        LOC:BytesRead = LOC:BytesRead - (LOC:TotalBytesRead-pLen)
      END
    END

    IF pAllowedCharOnly
      LOOP lIndex = 1 TO LOC:BytesRead
        CASE VAL (LOC:Buffer[lIndex])
        OF VAL('\')
        OROF VAL('(')
        OROF VAL(')')
          SELF.WriteStr ('\')
        END

        LOC:Buffer2 [1] = LOC:Buffer[lIndex]
        SELF.WriteStr (LOC:Buffer [lIndex])
      END
    ELSE
      SELF.WriteStr (LOC:Buffer [1 : LOC:BytesRead])
    END

    IF pLen <> 0
      IF LOC:TotalBytesRead > pLen
        LOC:TotalBytesRead = pLen
        BREAK
      END
    END
  UNTIL LOC:BytesRead < LOC:BufferLen

  IF LOC:TotalBytesRead > 0
    SELF.SetSourceFPos (LOC:SFPos + LOC:TotalBytesRead)
  END

  IF SELF.SourceFileHandle <> 0
    IF LOC:LocalOpen
      SELF.CloseSourceFile()

      IF LOC:SFPos <> 0
        IF SELF.OpenSourceFile (LOC:PrevFileName)
          SELF.SetSourceFPos (LOC:SFPos)
        END
      END
    END
  END

  RETURN LOC:TotalBytesRead

TargetGenerator.OpenSourceFile  PROCEDURE (STRING SourceFileName)
  CODE
  IF SELF.SourceFileHandle <> 0
    SELF.CloseSourceFile()
    SELF.SourceFileHandle = 0
  END

  SELF.SFPos = 0
  SELF.SourceFileName = SourceFileName

  ReportTarget_InFile{PROP:Name} = CLIP(SELF.SourceFileName)

  OPEN (ReportTarget_InFile, 0+32) !Read Only - Deny Write

  IF ERRORCODE() <> 0
    MESSAGE ('ERROR:(' & ERRORCODE() & ') ' & ERROR() & '|Can''t open source file ' & CLIP(SELF.SourceFileName),'Target OpenSourceFile')
    CLEAR (SELF.SourceFileName)
    RETURN FALSE
  END

  SELF.SourceFileHandle = 1
  RETURN TRUE

TargetGenerator.CloseSourceFile   PROCEDURE()
  CODE
  SELF.SFPos = 0

  IF SELF.SourceFileHandle <> 0
    CLOSE (ReportTarget_InFile)
    IF ERRORCODE() <> 0
      RETURN FALSE
    END

    SELF.SourceFileHandle = 0
  END
  RETURN TRUE

TargetGenerator.GetSourceFPos  PROCEDURE()
  CODE
  RETURN SELF.SFPos

TargetGenerator.SetSourceFPos  PROCEDURE (LONG pFPos)
  CODE
  SELF.SFPos = pFPos
  RETURN

TargetGenerator.GetSourceSAutoAdvance  PROCEDURE()
  CODE
  RETURN SELF.SAutoAdvance

TargetGenerator.SetSourceSAutoAdvance  PROCEDURE (BYTE pParam)
  CODE
  SELF.SAutoAdvance = CHOOSE (pParam <> 0)
  RETURN

TargetGenerator.ReadStr  PROCEDURE (LONG pLength, *STRING pBuffer)
LOC:BytesRead   LONG
  CODE
  IF SELF.SourceFileHandle <> 0 THEN
     SET (ReportTarget_InFile, SELF.SFPos)

     IF ERRORCODE() = 0
        GET (ReportTarget_InFile, SELF.SFPos, pLength)

        IF ERRORCODE() = 0
           LOC:BytesRead = BYTES (ReportTarget_InFile)

           IF SELF.SAutoAdvance THEN
              SELF.SFPos += LOC:BytesRead
           END

           pBuffer[1:LOC:BytesRead] = ReportTarget_InFile.Buffer[1:LOC:BytesRead]
           RETURN LOC:BytesRead
        END
     END
  END

  pBuffer = ''
  RETURN 0

TargetGenerator.GetStr   PROCEDURE (LONG pLength)
LOC:Buffer      LIKE(ReportTarget_InFile.Buffer),AUTO
LOC:BytesRead   LONG
  CODE
  IF SELF.SourceFileHandle = 0 THEN
     RETURN ''
  END
  LOC:BytesRead = SELF.ReadStr (pLength, LOC:Buffer)
  IF LOC:BytesRead <> 0
     RETURN LOC:Buffer [1: LOC:BytesRead]
  ELSE
     RETURN ''
  END

TargetGenerator.ReadULong   PROCEDURE(*ULONG pBuffer)
LOC:Buffer      STRING(4),AUTO
G               GROUP,OVER(LOC:Buffer)
A                 BYTE
B                 BYTE
C                 BYTE
D                 BYTE
                END
LOC:NBuffer     ULONG,OVER(G)
BAUX            BYTE,AUTO
  CODE
  IF SELF.ReadStr (SIZE (LOC:Buffer) ,LOC:Buffer) <> SIZE (LOC:Buffer)
     RETURN FALSE
  END

  BAUX = G.A
  G.A  = G.D
  G.D  = BAUX
  BAUX = G.C
  G.C  = G.B
  G.B  = BAUX
  pBuffer = LOC:NBuffer
  RETURN TRUE

TargetGenerator.ReadLong   PROCEDURE(*LONG pBuffer)
LOC:Buffer     STRING(4),AUTO
G              GROUP,OVER(LOC:Buffer)
A                 BYTE
B                 BYTE
C                 BYTE
D                 BYTE
               END
LOC:NBuffer    LONG,OVER(G)
BAUX           BYTE,AUTO
  CODE
  IF SELF.ReadStr (SIZE (LOC:Buffer), LOC:Buffer) <> SIZE (LOC:Buffer)
     RETURN FALSE
  END

  BAUX = G.A
  G.A  = G.D
  G.D  = BAUX
  BAUX = G.C
  G.C  = G.B
  G.B  = BAUX
  pBuffer = LOC:NBuffer
  RETURN TRUE

TargetGenerator.ReadUShort   PROCEDURE (*USHORT pBuffer)
LOC:Buffer     STRING(2),AUTO
G              GROUP,OVER(LOC:Buffer)
A                 BYTE
B                 BYTE
               END
LOC:NBuffer    USHORT,OVER(G)
BAUX           BYTE,AUTO
  CODE
  IF SELF.ReadStr (SIZE (LOC:Buffer), LOC:Buffer) <> SIZE (LOC:Buffer)
     RETURN FALSE
  END

  BAUX = G.A
  G.A  = G.B
  G.B  = BAUX
  pBuffer = LOC:NBuffer
  RETURN TRUE

TargetGenerator.ReadShort   PROCEDURE (*SHORT pBuffer)
LOC:Buffer     STRING(2),AUTO
G              GROUP,OVER(LOC:Buffer)
A                 BYTE
B                 BYTE
               END
LOC:NBuffer    SHORT,OVER(G)
BAUX           BYTE,AUTO
  CODE
  IF SELF.ReadStr (SIZE (LOC:Buffer), LOC:Buffer) <> SIZE (LOC:Buffer)
     RETURN FALSE
  END

  BAUX = G.A
  G.A  = G.B
  G.B  = BAUX
  pBuffer = LOC:NBuffer
  RETURN TRUE

TargetGenerator.ReadByte   PROCEDURE (*BYTE pBuffer)
LOC:Buffer     STRING(1)
LOC:NBuffer    BYTE,OVER(LOC:Buffer)
 CODE
  IF SELF.ReadStr (SIZE (LOC:Buffer), LOC:Buffer) <> SIZE (LOC:Buffer)
     RETURN FALSE
  END

  pBuffer = LOC:NBuffer
  RETURN TRUE

!********************************************
