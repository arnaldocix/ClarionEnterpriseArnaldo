    MEMBER

    INCLUDE('ABPRTARY.INC'),ONCE
    PRAGMA('link(C%V%ENC%X%.LIB)')

    MAP
     MODULE('C%V%ENC%X%.LIB')
       CryptClose(), NAME('_CPE_Close')
       CryptGetLastError(), SIGNED, NAME('_CPE_GetLastError')
       CryptInit(*CSTRING pPassword, ULONG nPswdLen, *CSTRING lpId, ULONG nIdLen, ULONG P), SIGNED, RAW, NAME('_CPE_Initialize')
       CryptGetOU(*CSTRING lpO, *ULONG LenO, *CSTRING lpU, *ULONG LenU), SIGNED, RAW, NAME('_CPE_GetOU')
       CryptNext(*CSTRING pData, ULONG nLen), SIGNED, RAW, NAME('_CPE_EncryptNext')
       CryptEncrypt(*STRING pData, ULONG nLen, ULONG nObjNum, ULONG nGenNum), SIGNED, RAW, NAME('_CPE_EncryptData')
       CryptConvert(*STRING pData, ULONG nLen, *STRING pStr, *ULONG nStrLen), SIGNED, RAW, NAME('_CPE_DataToString')
     END
    END

!********************************************
PDFTargetGenerator.Construct   PROCEDURE()
 CODE
   SELF.TextBuffer &= new STRING(1)
   SELF.StreamBuffer &= new STRING(1)
   SELF.FlateStream &= new STRING(1)
   SELF.EscapedStream &= new STRING(1)


PDFTargetGenerator.Destruct    PROCEDURE()
 CODE
   DISPOSE(SELF.TextBuffer)
   DISPOSE(SELF.StreamBuffer)
   DISPOSE(SELF.FlateStream)
   DISPOSE(SELF.EscapedStream)

PDFTargetGenerator.Init        PROCEDURE(<STRING TargetFileName>)
 CODE
   IF NOT OMITTED(2) THEN
      PARENT.Init(TargetFileName)
   ELSE
      PARENT.Init()
   END

   SELF.EncryptInited = False
   SELF.Password      = ''
   SELF.Encrypting    = False
   SELF.Encrypt       = False

   SELF.Flate       = FALSE
   SELF.Flated      = FALSE
   SELF.BufferLen   = 0
   SELF.TextBuffer  = ''
   SELF.StreamBuffer= ''
   SELF.FlateStream = ''
   SELF.AccessPermissions = 0

PDFTargetGenerator.StoreStr  PROCEDURE(STRING str, LONG strLen, *BYTE retCode)
LOC:TempBuffer       &STRING
LOC:NewLen           ULONG
 CODE
   IF SELF.Buffering = True
      LOC:NewLen = SELF.BufferLen + strLen
      LOC:TempBuffer &= new STRING(LOC:NewLen + 1)
      LOC:TempBuffer = SELF.TextBuffer[1:(SELF.BufferLen)] & str[1:strLen]
      DISPOSE(SELF.TextBuffer)
      SELF.TextBuffer &= LOC:TempBuffer
      SELF.BufferLen = LOC:NewLen
      retCode = True
      RETURN True
   ELSE
      retCode = False
      RETURN False
   END

PDFTargetGenerator.StartBuffering       PROCEDURE()
  CODE
     SELF.Buffering = True

PDFTargetGenerator.StopBuffering        PROCEDURE()
  CODE
     SELF.Buffering = False

PDFTargetGenerator.GetBufferSize        PROCEDURE()
  CODE
     RETURN SELF.BufferLen

PDFTargetGenerator.StartFlate PROCEDURE()
  CODE
  SELF.Flate  = TRUE
  SELF.Flated = FALSE
  SELF.StartBuffering()

PDFTargetGenerator.StopFlate PROCEDURE()
  CODE
  SELF.Flate = False
  SELF.StopBuffering()

PDFTargetGenerator.GetFlateStatus PROCEDURE()
  CODE
  RETURN SELF.Flated

!This method need to be called before stop flate mode
PDFTargetGenerator.CompressBuffer PROCEDURE()
LOC:Result      SIGNED,AUTO
  CODE
    IF SELF.Flate
       DISPOSE (SELF.FlateStream)
       SELF.FlateStream &= COMPRESS (SELF.TextBuffer [1 : SELF.BufferLen], -1, LOC:Result)
       IF LOC:Result >= 0
         SELF.Flated = TRUE
         SELF.BufferLen = SIZE (SELF.FlateStream)
       ELSE
         SELF.Flated = FALSE
       END
    END
    RETURN SELF.BufferLen

PDFTargetGenerator.FlushBuffer PROCEDURE(BYTE pEcapeCharacters=0)
LOC:Result      LONG
LOC:BlockLen    ULONG
LOC:pSize       &ULONG
LOC:I           LONG
LOC:BufferLen   LONG
LOC:TempStream  &STRING

  CODE
    IF SELF.TargetFileHandle
       LOC:BlockLen = SELF.BufferLen
       IF SELF.Flated = TRUE THEN
          LOC:TempStream &= SELF.FlateStream
       ELSE
          LOC:TempStream &= SELF.TextBuffer
       END
       IF SELF.Encrypt
          LOC:Result = CryptEncrypt(LOC:TempStream, LOC:BlockLen, SELF.ObjNum, 0)
          IF pEcapeCharacters
             DO EscapeChars
          END
       END
       LOC:I = 1
       LOC:BufferLen = 4096
       IF LOC:BlockLen < LOC:BufferLen THEN
          LOC:BufferLen = LOC:BlockLen
       END
       LOOP
         SELF.DumpStr(LOC:TempStream[LOC:I : LOC:I+LOC:BufferLen-1], LOC:BufferLen)
         SELF.FPos += LOC:BufferLen
         LOC:I += LOC:BufferLen
         IF LOC:BlockLen <= LOC:I THEN
           BREAK
         END
         IF (LOC:BlockLen - LOC:I + 1) <= LOC:BufferLen THEN
           LOC:BufferLen = LOC:BlockLen - LOC:I + 1
         END
       END
       SELF.BufferLen  = 0
       LOC:TempStream  = ''
    END
    SELF.Flated = False

EscapeChars     ROUTINE
 DATA
CharToReplace   STRING(1),DIM(8)
ReplacedBy      STRING(2),DIM(8)

lIndex          SHORT
lIndexAux       SHORT
lIndexReplace   SHORT
lReplaced       BYTE


HowManyReplaced LONG
 CODE
    SELF.EscapedStream &= NEW(STRING(LOC:BlockLen*2))

    CharToReplace[1]= '\'
    ReplacedBy[1]   = '\\'

    CharToReplace[2]= '('
    ReplacedBy[2]   = '\('

    CharToReplace[3]= ')'
    ReplacedBy[3]   = '\)'

    CharToReplace[4]= CHR(10)!Line Feed
    ReplacedBy[4]   = '\n'

    CharToReplace[5]= CHR(13)!Carriage return
    ReplacedBy[5]   = '\r'

    CharToReplace[6]= CHR(9)!Horizontal tab
    ReplacedBy[6]   = '\t'

    CharToReplace[7]= CHR(8)!Backspace
    ReplacedBy[7]   = '\b'

    CharToReplace[8]= CHR(12)!Form feed
    ReplacedBy[8]   = '\f'

    lIndexAux = 0
    HowManyReplaced=0
    LOOP lIndex=1 TO LOC:BlockLen
         lReplaced = False
         lIndexAux+=1
         LOOP lIndexReplace=1 TO 8
            IF LOC:TempStream[lIndex]=CharToReplace[lIndexReplace]
               HowManyReplaced+=1
               SELF.EscapedStream[(lIndexAux):(lIndexAux+1)]=ReplacedBy[lIndexReplace]
               lIndexAux+=1
               lReplaced = True
               BREAK
            END
         END
         IF lReplaced = False
            SELF.EscapedStream[lIndexAux]=LOC:TempStream[lIndex]
         END
    END
    IF HowManyReplaced>0
       LOC:BlockLen   = lIndexAux
       SELF.BufferLen = lIndexAux
       IF SELF.Flated = TRUE THEN
          DISPOSE(SELF.FlateStream)
          SELF.FlateStream&=SELF.EscapedStream
          LOC:TempStream &= SELF.EscapedStream
       ELSE
          DISPOSE(SELF.TextBuffer)
          SELF.TextBuffer&=SELF.EscapedStream
          LOC:TempStream &= SELF.EscapedStream
       END
    END

PDFTargetGenerator.SetEncryption        PROCEDURE(BYTE Encrypt,<STRING pPassword>,<ULONG pAccessPermissions>)
 CODE
    SELF.Encrypt = Encrypt
    IF NOT OMITTED(3)
       IF NOT OMITTED(4)
          SELF.SetPassword(pPassword,pAccessPermissions)
       ELSE
          SELF.SetPassword(pPassword)
       END
    ELSE
       SELF.Encrypt = False
    END

PDFTargetGenerator.GetEncryption        PROCEDURE()
 CODE
    RETURN SELF.Encrypt

PDFTargetGenerator.SetPassword          PROCEDURE(STRING pPassword,<ULONG pAccessPermissions>)
 CODE
    IF CLIP(pPassword)
       SELF.Password = pPassword
       IF NOT OMITTED(3)
          SELF.AccessPermissions = pAccessPermissions
       ELSE
          SELF.AccessPermissions = 0
       END
    ELSE
       SELF.Encrypt = False
       SELF.Password = ''
    END

PDFTargetGenerator.EncryptGetID      PROCEDURE()
 CODE
    RETURN SELF.EncryptID

PDFTargetGenerator.EncryptSetID      PROCEDURE(STRING pID)
 CODE
    SELF.EncryptID = pID

PDFTargetGenerator.EncryptGetHexID      PROCEDURE()
ID:Hex        STRING(256)
lIndex        SHORT
lHexByteH     BYTE
lHexByteL     BYTE
 CODE
    IF NOT SELF.EncryptID
       SELF.EncryptID = 'b94f171e8c6ad92a8dc45b559e759d39'
    END
    IF LEN(SELF.EncryptID)%2 > 0
       SELF.EncryptID = SELF.EncryptID & '0'
    END
    SELF.EncryptID = UPPER(SELF.EncryptID)
    ID:Hex = ''
    LOOP lIndex=1 TO LEN(SELF.EncryptID)/2
         lHexByteH=INLIST(SELF.EncryptID[(lIndex*2)-1],'1','2','3','4','5','6','7','8','9','A','B','C','D','E','F')
         lHexByteL=INLIST(SELF.EncryptID[(lIndex*2)],'1','2','3','4','5','6','7','8','9','A','B','C','D','E','F')
         ID:Hex[lIndex]=CHR((lHexByteH*16)+lHexByteL)
    END
    RETURN ID:Hex[1:(LEN(SELF.EncryptID)/2)]

PDFTargetGenerator.EncryptInit          PROCEDURE()
ID:Hex        CSTRING(256)
RetVal        LONG
lPassord      CSTRING(256)
 CODE
     IF SELF.EncryptInited THEN RETURN True.
     IF NOT SELF.Password THEN RETURN False.
     IF NOT SELF.Encrypt THEN RETURN False.
     ID:Hex = SELF.EncryptGetHexID()
     lPassord = CLIP(SELF.Password)
     RetVal = CryptInit(lPassord, LEN(SELF.Password), ID:Hex, (LEN(SELF.EncryptID)/2), -SELF.AccessPermissions)
     IF RetVal
        SELF.EncryptInited = True
     ELSE
        SELF.EncryptInited = False
     END
     RETURN RetVal

PDFTargetGenerator.EncryptStart         PROCEDURE(ULONG nObjNum)
 CODE
    IF NOT SELF.EncryptInit() THEN RETURN.
    SELF.ObjNum     = nObjNum
    SELF.Encrypting = True
    SELF.StartBuffering()

PDFTargetGenerator.EncryptStop          PROCEDURE()
 CODE
    SELF.Encrypting = False
    SELF.StopBuffering()

PDFTargetGenerator.EncryptGetLastError  PROCEDURE()
 CODE
    RETURN CryptGetLastError()

PDFTargetGenerator.EncryptGetOwnerString PROCEDURE()
LOC:O         CSTRING(256)
LOC:U         CSTRING(256)
LOC:LenO      ULONG
LOC:LenU      ULONG
 CODE
    IF NOT SELF.EncryptInit() THEN RETURN ''.
    LOC:LenO = 256
    LOC:LenU = 256
    IF CryptGetOU(LOC:O, LOC:LenO, LOC:U, LOC:LenU)
       RETURN LOC:O[1:(LOC:LenO)]
    ELSE
       RETURN ''
    END

PDFTargetGenerator.EncryptGetUserString PROCEDURE()
LOC:O         CSTRING(256)
LOC:U         CSTRING(256)
LOC:LenO      ULONG
LOC:LenU      ULONG
 CODE
    IF NOT SELF.EncryptInit() THEN RETURN ''.
    LOC:LenO = 256
    LOC:LenU = 256
    IF CryptGetOU(LOC:O, LOC:LenO, LOC:U, LOC:LenU)
       RETURN LOC:U[1:(LOC:LenU)]
    ELSE
       RETURN ''
    END

PDFTargetGenerator.EncryptEnd           PROCEDURE()
 CODE
    IF NOT SELF.EncryptInit() THEN RETURN.
    SELF.EncryptInited = False
    SELF.Encrypting    = False
    CryptClose()

PDFTargetGenerator.EncryptWriteStr PROCEDURE(ULONG nObjNum,STRING str,BYTE pClipStr=0)
RetVal  BYTE
 CODE
    IF SELF.Encrypt THEN
       SELF.EncryptStart(nObjNum)
       RetVal = SELF.WriteStr(str,pClipStr)
       SELF.FlushBuffer(True)
       SELF.EncryptStop()
    ELSE
       RetVal = SELF.WriteStr(str,pClipStr)
    END
    RETURN RetVal

!********************************************
