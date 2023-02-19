 MEMBER

 INCLUDE ('ERRORS.CLW')
 INCLUDE ('SystemString.INC')
  MAP
  END


!**Base64 Data

SystemStringClass_Base64Encode STRING('ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=')


SystemStringClass.Construct PROCEDURE()
  CODE
     IF NOT SELF.s &= NULL
 	   DISPOSE(SELF.s)
 	 END  
     SELF.EOLMarker = '<13,10>'
     SELF.TokensDelimiters = ' ,;.-()][}{{_:<<>\/@#¦¬^?+í!|"'''
     SELF.lines &= new SystemStringQueue()
     SELF.ResetToken()

SystemStringClass.Destruct PROCEDURE()
  CODE
  	SELF.DisposeIt()
 	IF NOT SELF.cs &=NULL
 	   DISPOSE(SELF.cs)
 	END
  	SELF.CleanLines()
  	DISPOSE(SELF.lines)

SystemStringClass.DisposeIt PROCEDURE()
 CODE
 	IF NOT SELF.s &= NULL
 	   DISPOSE(SELF.s)
 	   SELF.ResetToken()
 	END

SystemStringClass.DisposeItCString PROCEDURE()
 CODE
 	IF NOT SELF.cs &= NULL
 	   DISPOSE(SELF.cs)
 	END

SystemStringClass.CleanLines PROCEDURE()
 CODE
    SELF.CleanSplitQueue(SELF.lines)

SystemStringClass.CleanSplitQueue PROCEDURE(*SystemStringQueue collection)
idx         LONG
 CODE
    IF RECORDS(collection)
       LOOP idx=1 TO RECORDS(collection) 
            GET(collection,idx)
            IF NOT ERRORCODE()
               DISPOSE(collection.Str)
            END
       END
       FREE(collection)
    END
 
SystemStringClass.Clean PROCEDURE()
 CODE    
    SELF.FromString('')
    SELF.CleanLines()

SystemStringClass.FromString PROCEDURE(STRING svalue)
 CODE
    SELF.DisposeIt()
    SELF.s &= NEW STRING(LEN(svalue))
    SELF.s = svalue
    
SystemStringClass.FromString PROCEDURE(*CSTRING svalue)
 CODE
    SELF.DisposeIt()
    SELF.s &= NEW STRING(LEN(svalue))
    SELF.s = svalue

SystemStringClass.Str PROCEDURE(STRING s)
  CODE
  SELF.FromString(s)

SystemStringClass.Str PROCEDURE()
  CODE
     IF SELF.s &= NULL
        RETURN ''
     ELSE
        RETURN SELF.s
     END

SystemStringClass.ToCString PROCEDURE()
 CODE
    SELF.DisposeItCString()
    SELF.cs &= new CSTRING(LEN(SELF.s)+1)
    SELF.cs = SELF.s
    RETURN SELF.cs

SystemStringClass.FromCString PROCEDURE()
 CODE
    SELF.DisposeIt()
    IF NOT SELF.cs &= NULL
       SELF.FromString(SELF.cs)
    ELSE
       SELF.FromString('')
    END
    
SystemStringClass.Length PROCEDURE()
 CODE
     IF SELF.s &= NULL 
        RETURN 0
     ELSE
        RETURN LEN(SELF.s)
     END

SystemStringClass.IndexOf PROCEDURE(STRING svalue)
 CODE
    RETURN SELF.IndexOf(svalue,1)
    
SystemStringClass.IndexOf PROCEDURE(STRING svalue, ULONG startIndex)
 CODE
    RETURN INSTRING(svalue, SELF.s, 1, startIndex)

SystemStringClass.LastIndexOf PROCEDURE(STRING svalue)
 CODE
    RETURN SELF.LastIndexOf(svalue, 1)
    
SystemStringClass.LastIndexOf PROCEDURE(STRING svalue, ULONG startIndex)
idx        LONG,AUTO
lastFoundIdx LONG,AUTO
 CODE
    lastFoundIdx = 0
    IF startIndex < SELF.Length()
       idx = startIndex
       LOOP
          idx = INSTRING(svalue, SELF.s, 1, idx)
          IF idx = 0
             BREAK
          END
          lastFoundIdx = idx
          idx += 1          
       END
    END
    RETURN lastFoundIdx
    
SystemStringClass.Append PROCEDURE(STRING s)
  CODE
  SELF.Str(SELF.Str() & s)
  
SystemStringClass.AppendLine PROCEDURE(STRING s)
 CODE
  SELF.Str(SELF.Str() & s & SELF.EOLMarker)

SystemStringClass.ToString PROCEDURE()
 CODE
 	RETURN SELF.Str()
	
SystemStringClass.ToLower PROCEDURE()
 CODE
    RETURN LOWER(SELF.Str())

SystemStringClass.ToUpper PROCEDURE()
 CODE
    RETURN UPPER(SELF.Str())

SystemStringClass.ToCapitalize PROCEDURE()
 CODE
    IF SELF.s &= NULL
       RETURN ''
    ELSE
       RETURN SELF.Capitalize(SELF.Str())
    END
    
SystemStringClass.Capitalize PROCEDURE(STRING cs)
idx        LONG
sLength    LONG
 CODE
       idx = 1
       sLength = LEN(cs)
       IF sLength > 0
          cs[idx] = UPPER(cs[idx])
          IF sLength > 1
             LOOP idx = 2 to sLength
                  IF cs[idx-1] = ' '
                     cs[idx] = UPPER(cs[idx])
                  END
             END
          END
       END
       RETURN cs

SystemStringClass.Contains PROCEDURE(STRING svalue)
 CODE
    IF INSTRING(svalue, SELF.Str(),1,1)
       RETURN TRUE
    ELSE
       RETURN FALSE  
    END

SystemStringClass.StartsWith PROCEDURE(STRING svalue)
 CODE
    RETURN SELF.StartsWith(svalue,true)
    
SystemStringClass.StartsWith PROCEDURE(STRING svalue,BYTE IgnoreCase)
csLen LONG
svLEN LONG
 CODE
    csLen = SELF.Length()
    svLEN = LEN(svalue)
    IF svLEN > csLen
       RETURN FALSE
    ELSE
       IF IgnoreCase
          IF UPPER(svalue) = UPPER(SUB(SELF.Str(),1,svLen))
             RETURN TRUE
          ELSE
             RETURN FALSE
          END
       ELSE
          IF svalue = SUB(SELF.Str(),1,svLen)
             RETURN TRUE
          ELSE
             RETURN FALSE
          END
       END
    END

SystemStringClass.EndsWith PROCEDURE(STRING svalue)
 CODE
    RETURN SELF.EndsWith(svalue,true)
    
SystemStringClass.EndsWith PROCEDURE(STRING svalue,BYTE IgnoreCase)
csLen LONG
svLEN LONG
 CODE
    csLen = SELF.Length()
    svLEN = LEN(svalue)
    IF svLEN > csLen
       RETURN FALSE
    ELSE
       IF IgnoreCase
          IF UPPER(svalue) = UPPER(SUB(SELF.Str(),csLen - svLen ,svLen))
             RETURN TRUE
          ELSE
             RETURN FALSE
          END
       ELSE
          IF svalue = SUB(SELF.Str(),csLen - svLen ,svLen)
             RETURN TRUE
          ELSE
             RETURN FALSE
          END
       END
    END

SystemStringClass.ReplaceInContent          PROCEDURE(STRING oldString,STRING newString)
sstr         SystemStringClass
idxFound     LONG
prevIdxFound LONG
newStringLen LONG
oldStringLen LONG
retVal       LONG
 CODE
    retVal = 0
    IF oldString<>newString
       newStringLen = LEN(newString)
       oldStringLen = LEN(oldString)
       
       idxFound = INSTRING(oldString, SELF.ToString(), 1, 1)
       prevIdxFound = 1
       LOOP
          IF idxFound > 0
             IF prevIdxFound < idxFound
                sstr.Append(SUB(SELF.ToString(),prevIdxFound,idxFound - prevIdxFound  ))
             END
             sstr.Append(newString)
             prevIdxFound = idxFound + oldStringLen
             retVal+=1
          ELSE
             BREAK
          END
          idxFound = INSTRING(oldString, SELF.ToString(), 1, prevIdxFound)
       END
       IF idxFound = 0
          IF prevIdxFound < SELF.Length()
             retVal+=1
             sstr.Append(SUB(SELF.ToString(), prevIdxFound, LEN(SELF.ToString()) - prevIdxFound + 1))
          END          
       END
       SELF.FromString(sstr.ToString())
    END
    RETURN retVal
    
SystemStringClass.Replace PROCEDURE(STRING oldString,STRING newString) !return the number of oldstring replaced
 CODE
    !sstr.FromString(SELF.ToString())
    RETURN SELF.ReplaceSubString(SELF.ToString(), oldString, newString)
    !RETURN sstr.ToString()
    
SystemStringClass.ReplaceSubString          PROCEDURE(STRING originalString, STRING oldString,STRING newString)!execute a replace in the internal string, does not return anything
sstr         SystemStringClass
idxFound     LONG
prevIdxFound LONG
newStringLen LONG
oldStringLen LONG
 CODE
    IF oldString<>newString
       newStringLen = LEN(newString)
       oldStringLen = LEN(oldString)
       
       idxFound = INSTRING(oldString, originalString, 1, 1)
       prevIdxFound = 1
       LOOP
          IF idxFound > 0
             IF prevIdxFound < idxFound
                sstr.Append(SUB(originalString,prevIdxFound,idxFound - prevIdxFound  ))
             END
             sstr.Append(newString)
             prevIdxFound = idxFound + oldStringLen
          ELSE
             BREAK
          END
          idxFound = INSTRING(oldString, originalString, 1, prevIdxFound)
       END
       IF idxFound = 0
          IF prevIdxFound < LEN(originalString)
             sstr.Append(SUB(originalString, prevIdxFound, LEN(originalString) - prevIdxFound + 1))
          END          
       END
       RETURN sstr.ToString()
    ELSE
       RETURN originalString
    END
    
SystemStringClass.Insert PROCEDURE(ULONG startIndex, STRING svalue)
csLen LONG
 CODE
    csLen = SELF.Length()
    IF startIndex < csLen
       RETURN SELF.s[1 : (startIndex)] & svalue & SELF.s[(startIndex) : (csLen)]
    ELSE
       RETURN SELF.s & svalue
    END

SystemStringClass.Remove PROCEDURE(ULONG startIndex)
csLen LONG
 CODE
    csLen = SELF.Length()
    IF startIndex < csLen
       RETURN SELF.s[1 : (startIndex - 1)]
    ELSE
       RETURN SELF.s
    END
 
SystemStringClass.Remove PROCEDURE(ULONG startIndex, ULONG subLength)
csLen LONG
 CODE
    csLen = SELF.Length()
    IF startIndex < csLen
       IF csLen <= (subLength + startIndex)
          RETURN SELF.s[1 : (startIndex - 1)]
       ELSE
          RETURN SELF.s[1 : (startIndex - 1)] & SELF.s[(startIndex + subLength) : (csLen)]
       END
    ELSE
       RETURN SELF.s
    END

SystemStringClass.Substring PROCEDURE(ULONG startIndex)
csLen LONG
 CODE
    csLen = SELF.Length()
    IF startIndex < csLen
       RETURN SELF.s[(startIndex):(csLen)]
    ELSE
       RETURN ''
    END
 
SystemStringClass.Substring PROCEDURE(ULONG startIndex, ULONG subLength)
csLen LONG
 CODE
    csLen = SELF.Length()
    IF startIndex < csLen
       IF csLen > (subLength + startIndex-1)
          RETURN SELF.s[(startIndex):(startIndex + subLength-1)]
       ELSE
          RETURN SELF.s[(startIndex):(csLen)]
       END
    ELSE
       RETURN ''
    END

SystemStringClass.SubstringLeft             PROCEDURE(ULONG startIndex)
 CODE
    RETURN SELF.SubstringLeft(startIndex, startIndex)
    
SystemStringClass.SubstringLeft             PROCEDURE(ULONG startIndex, ULONG subLength)
 CODE
    IF startIndex > 1 AND startIndex < SELF.Length()
       IF startIndex = subLength OR subLength > startIndex
          RETURN SELF.s[1:(startIndex - 1)]
       ELSE
          RETURN SELF.s[(startIndex - subLength):(startIndex - 1)]
       END
    ELSE
       RETURN ''
    END
 
SystemStringClass.PadLeft                   PROCEDURE(ULONG totalWidth)
 CODE
    RETURN SELF.PadLeft(totalWidth, ' ')
    
SystemStringClass.PadLeft                   PROCEDURE(ULONG totalWidth, STRING paddingChar)
idx LONG,AUTO
 CODE
    IF SELF.Length() > totalWidth
       RETURN SELF.ToString()
    END
    RETURN ALL(paddingChar,totalWidth  - SELF.Length()) & SELF.ToString()

SystemStringClass.PadRight PROCEDURE(ULONG totalWidth)
 CODE
    RETURN SELF.PadRight(totalWidth, ' ')

SystemStringClass.PadRight PROCEDURE(ULONG totalWidth, STRING paddingChar)
 CODE
    IF SELF.Length() > totalWidth
       RETURN SELF.ToString()
    END
    RETURN SELF.ToString() & ALL(paddingChar,totalWidth - SELF.Length())

SystemStringClass.PadCenter PROCEDURE(ULONG totalWidth)
 CODE
    RETURN SELF.PadCenter(totalWidth, ' ')

SystemStringClass.PadCenter PROCEDURE(ULONG totalWidth, STRING paddingChar)
leftPadLen LONG,AUTO
rightPadLen LONG,AUTO
 CODE
    IF SELF.Length() > totalWidth
       RETURN SELF.ToString()
    END
    leftPadLen  = INT((totalWidth  - SELF.Length()) / 2)
    rightPadLen = totalWidth  - SELF.Length() - leftPadLen
    RETURN ALL(paddingChar,leftPadLen) & SELF.ToString() & ALL(paddingChar,rightPadLen)

SystemStringClass.Trim PROCEDURE()
 CODE
    RETURN CLIP(LEFT(SELF.Str()))

SystemStringClass.Trim PROCEDURE(BYTE trimType,STRING char1,<STRING char2>,<STRING char3>,<STRING char4>)
idx        LONG
sLength    LONG
isIdxToTrimStart LONG
isIdxToTrimEnd LONG
 CODE
    sLength = SELF.Length()
    isIdxToTrimStart = 0
    isIdxToTrimEnd = sLength + 1
    IF trimType = 0 OR trimType = 1
       LOOP idx = 1 to sLength
            IF SELF.s[idx] = char1[1]
               isIdxToTrimStart = idx
            ELSE
               IF NOT OMITTED(3) !char2
                  IF SELF.s[idx] = char2[1]
                     isIdxToTrimStart = idx
                  END
               END
               IF NOT OMITTED(4) !char3
                  IF SELF.s[idx] = char3[1]
                     isIdxToTrimStart = idx
                  END
               END
               IF NOT OMITTED(5) !char4
                  IF SELF.s[idx] = char4[1]
                     isIdxToTrimStart = idx
                  END
               END
            END
            IF isIdxToTrimStart <> idx
               BREAK
            END
       END
    END
    IF trimType = 0 OR trimType = 2
       LOOP idx = sLength to 1 BY -1
            IF SELF.s[idx] = char1[1]
               isIdxToTrimEnd = idx
            ELSE
               IF NOT OMITTED(3) !char2
                  IF SELF.s[idx] = char2[1]
                     isIdxToTrimEnd = idx
                  END
               END
               IF NOT OMITTED(4) !char3
                  IF SELF.s[idx] = char3[1]
                     isIdxToTrimEnd = idx
                  END
               END
               IF NOT OMITTED(5) !char4
                  IF SELF.s[idx] = char4[1]
                     isIdxToTrimEnd = idx
                  END
               END
            END
            IF isIdxToTrimEnd <> idx
               BREAK
            END
       END
    END
    RETURN SELF.s[(isIdxToTrimStart+1):(isIdxToTrimEnd-1)]

SystemStringClass.Trim PROCEDURE(STRING char1,<STRING char2>,<STRING char3>,<STRING char4>)
 CODE
    RETURN SELF.Trim(0,char1,char2,char3,char4)
SystemStringClass.TrimStart PROCEDURE(STRING char1,<STRING char2>,<STRING char3>,<STRING char4>)
 CODE
    RETURN SELF.Trim(1,char1,char2,char3,char4)
SystemStringClass.TrimEnd PROCEDURE(STRING char1,<STRING char2>,<STRING char3>,<STRING char4>)
 CODE
    RETURN SELF.Trim(2,char1,char2,char3,char4)

SystemStringClass.Split PROCEDURE(STRING token,BYTE includeEmptyResults = false)
idxFound     LONG
tokenStringLen LONG
prevIdxFound LONG
 CODE
    SELF.CleanSplitQueue(SELF.lines)
    tokenStringLen = LEN(token)
    idxFound = INSTRING(token, SELF.Str(), 1, 1)
    prevIdxFound = 1
    LOOP
       IF idxFound > 0
          IF prevIdxFound < idxFound
             SELF.AddLine(SUB(SELF.Str(),prevIdxFound,idxFound - prevIdxFound  ))
          ELSE
             IF includeEmptyResults AND prevIdxFound = idxFound
                SELF.AddLine('')
             END
          END
          prevIdxFound = idxFound + tokenStringLen
       ELSE
          BREAK
       END
       idxFound = INSTRING(token, SELF.Str(), 1, prevIdxFound)
    END
    IF idxFound = 0
       IF prevIdxFound < SELF.Length()
          SELF.AddLine(SUB(SELF.Str(),prevIdxFound,SELF.Length() - prevIdxFound + 1))
       ELSE
          IF includeEmptyResults AND prevIdxFound = SELF.Length()
             SELF.AddLine('')
          END
       END          
    END

 !OMIT ('=== DO LINK DOS', lib_mode)
    PRAGMA('link(C%V%DOS%X%%L%.LIB)')
! === DO LINK DOS

SystemStringClass.ToFile PROCEDURE(STRING fileName)
SystemStringClass_OutFile FILE,DRIVER('DOS'),CREATE
          RECORD
buffer      STRING(32768)
          END
        END
sz      LONG,AUTO
start   LONG,AUTO
amount  LONG,AUTO
CurErr  SIGNED,AUTO
 CODE
  IF fileName = ''
    RETURN BadFileErr
  END
  sz = SELF.Length()
  IF sz = 0
    RETURN 0
  END
  SystemStringClass_OutFile{PROP:Name} = fileName
  CREATE (SystemStringClass_OutFile)
  IF ERRORCODE() THEN RETURN ERRORCODE().
  OPEN (SystemStringClass_OutFile)
  IF ERRORCODE() THEN RETURN ERRORCODE().
  SEND (SystemStringClass_OutFile, 'FILEBUFFERS=' & ROUND(sz/512, 1))

  CurErr = 0
  start  = 1
  LOOP WHILE sz <> 0
    amount = SIZE (SystemStringClass_OutFile.buffer)
    IF amount > sz
      amount = sz
    END
    SystemStringClass_OutFile.buffer [1 : amount] = SELF.s [start : start + amount - 1]
    ADD (SystemStringClass_OutFile, amount)
    CurErr = ERRORCODE()
    IF CurErr <> 0
      BREAK
    END
    start += amount
    sz    -= amount
  END

  CLOSE (SystemStringClass_OutFile)
  RETURN CurErr


SystemStringClass.FromFile PROCEDURE(STRING fileName)
SystemStringClass_InFile  FILE,DRIVER('DOS')
          RECORD
buffer      STRING(32768)
          END
        END
sz      LONG,AUTO
start   LONG,AUTO
fetch   LONG,AUTO
CurErr  SIGNED,AUTO
  CODE
  IF fileName = ''
    RETURN BadFileErr
  END
  SystemStringClass_InFile{PROP:Name} = fileName
  OPEN (SystemStringClass_InFile,40h)
  IF ERRORCODE() THEN
     SELF.Str('')
     RETURN ERRORCODE()
  END
  sz = BYTES(SystemStringClass_InFile)
  IF sz = 0
    SELF.Str('')
    CurErr = BadFileErr
  ELSE
    SEND (SystemStringClass_InFile, 'FILEBUFFERS=' & ROUND(sz/512, 1))
    SELF.DisposeIt()
    SELF.s &= NEW STRING(sz)
    CurErr = 0
    start  = 1
    LOOP WHILE sz <> 0
      fetch = SIZE (SystemStringClass_InFile.buffer)
      IF fetch > sz
        fetch = sz
      END
      GET (SystemStringClass_InFile, start , fetch)
      CurErr = ERRORCODE()
      IF CurErr <> 0
        BREAK
      END
      SELF.s [start : start + fetch - 1] = SystemStringClass_InFile.buffer [1 : fetch]
      start += fetch
      sz    -= fetch
    END
  END

  CLOSE (SystemStringClass_InFile)
  RETURN CurErr

SystemStringClass.FromBlob PROCEDURE(*BLOB b)
sz      LONG,AUTO
  CODE
  sz = b{PROP:Size}
  SELF.DisposeIt()
  IF sz = 0
     SELF.FromString('')
  ELSE
    SELF.s &= new STRING(sz)
    SELF.s[1 : sz] = b [0 : sz - 1]
  END

SystemStringClass.ToBlob PROCEDURE(*BLOB b)
sz      LONG,AUTO
  CODE
  sz = SELF.Length()
  IF sz = 0
    b{PROP:Size} = sz
  ELSE
    b{PROP:Size} = sz
    b [0 : sz - 1] = SELF.s[1 : sz]
  END

SystemStringClass.Take24 PROCEDURE(byte h, byte m, byte l, *STRING Into)
B6 BYTE,AUTO
   CODE
   ! First 6 bits? What does the 'high bit is counted first' expression mean?
   ! I'm assuming top 6 bits of h
   B6 = BSHIFT(h,-2)
   Into[1] = SystemStringClass_Base64Encode[B6+1]
   ! Second 6 bits become bottom 2 of h (up 4) and top 4 of m (down 4)
   B6 = BOR(BAND(BSHIFT(h,4),030H),BSHIFT(m,-4))
   Into[2] = SystemStringClass_Base64Encode[B6+1]
   ! Third 6 bits are bottom 4 of m (up two) and top 2 of l (down 6)
   B6 = BOR(BAND(BSHIFT(m,2),03CH),BSHIFT(l,-6))
   Into[3] = SystemStringClass_Base64Encode[B6+1]
   ! Last 6 come from bottom 6 of l
   Into[4] = SystemStringClass_Base64Encode[BAND(l,03FH)+1]

SystemStringClass.Take32 PROCEDURE(*byte h, *byte m, *byte l, *STRING SFrom)
Buff BYTE,DIM(4),AUTO
idx BYTE,AUTO
  CODE
  LOOP idx = 1 TO 4
    Buff[idx] = INSTRING(SFrom[idx],SystemStringClass_Base64Encode)
?   ASSERT(Buff[idx])
    Buff[idx] -= 1
  END
? ASSERT(Buff[1]<>64)
? ASSERT(Buff[2]<>64)
  ! Whole of first 6 bits up two and first two of second (down 4)
  h = BOR(BSHIFT(Buff[1],2),BSHIFT(Buff[2],-4))
  IF Buff[3] = 64 THEN RETURN 1 .
  ! Middle is bottom 4 bits of second (up 4) and top 4 bits of third (down 2)
  m = BOR(BSHIFT(Buff[2],4),BSHIFT(Buff[3],-2))
  IF Buff[4] = 64 THEN RETURN 2 .
  ! Bottom is bottom two bits of third (up 6) and whole of fourth
  l = BOR(BSHIFT(Buff[3],6),Buff[4])
  RETURN 3

SystemStringClass.ToBase64 PROCEDURE()
idx SIGNED,AUTO
Blk SIGNED,AUTO
Outv &CSTRING
SOutv SystemStringClass
outBlk LONG
  CODE
  Outv &= new CSTRING((SELF.Length() * 4)/3)
  Blk = SELF.Length()/3
  LOOP idx = 1 TO Blk
    SELF.Take24(VAL(SELF.s[idx*3-2]),VAL(SELF.s[idx*3-1]),VAL(SELF.s[idx*3]),Outv[idx*4-3:idx*4])
  END
  IF Blk * 3 < LEN(SELF.s)
    IF Blk *3 + 1 = LEN(SELF.s)
      SELF.Take24(VAL(SELF.s[LEN(SELF.s)]),0,0,Outv[Blk*4+1:Blk*4+4])
      Outv[Blk*4+3] = '='
      Outv[Blk*4+4] = '='
    ELSE
      SELF.Take24(VAL(SELF.s[LEN(SELF.s)-1]),VAL(SELF.s[LEN(SELF.s)]),0,Outv[Blk*4+1:Blk*4+4])
      Outv[Blk*4+4] = '='
    END
    outBlk = Blk*4+4
  ELSE
    outBlk = Blk * 4
  END
  SOutv.FromString(Outv[1:outBlk])
  Outv = ''
  DISPOSE(Outv)
  RETURN SOutv.ToString()

SystemStringClass.FromBase64 PROCEDURE(STRING svalue)
F SIGNED(1)
Store STRING(4)
SH BYTE(0)
OutF SIGNED(1)
B  BYTE,DIM(3)
N  BYTE,AUTO
newSize LONG
  CODE
   SELF.DisposeIt()
   newSize = INT(LEN(svalue) * 3/4) !+ 1
   SELF.s &= NEW STRING(newSize)
  LOOP WHILE F <= LEN(svalue)
    IF INSTRING(svalue[F],SystemStringClass_Base64Encode)
      SH += 1
      Store[SH] = svalue[F]
      IF Sh = 4
        N = SELF.Take32(B[1],B[2],B[3],Store)
        SELF.s[OutF] = CHR(B[1])
        OutF += 1
        IF N = 1 THEN BREAK .
        SELF.s[OutF] = CHR(B[2])
        OutF += 1
        IF N = 2 THEN BREAK .
        SELF.s[OutF] = CHR(B[3])
        OutF += 1
        Sh = 0
      END
    END
    F += 1
  END

SystemStringClass.EncodeBase64 PROCEDURE()
idx SIGNED,AUTO
Blk SIGNED,AUTO
Outv &CSTRING
outBlk LONG
  CODE
  Outv &= new CSTRING((SELF.Length() * 4)/3)
  Blk = SELF.Length()/3
  LOOP idx = 1 TO Blk
    SELF.Take24(VAL(SELF.s[idx*3-2]),VAL(SELF.s[idx*3-1]),VAL(SELF.s[idx*3]),Outv[idx*4-3:idx*4])
  END
  IF Blk * 3 < LEN(SELF.s)
    IF Blk *3 + 1 = LEN(SELF.s)
      SELF.Take24(VAL(SELF.s[LEN(SELF.s)]),0,0,Outv[Blk*4+1:Blk*4+4])
      Outv[Blk*4+3] = '='
      Outv[Blk*4+4] = '='
    ELSE
      SELF.Take24(VAL(SELF.s[LEN(SELF.s)-1]),VAL(SELF.s[LEN(SELF.s)]),0,Outv[Blk*4+1:Blk*4+4])
      Outv[Blk*4+4] = '='
    END
    outBlk = Blk*4+4
  ELSE
    outBlk = Blk * 4
  END
  SELF.FromString(Outv[1: (outBlk)])
  Outv = ''
  DISPOSE(Outv)
  
SystemStringClass.DecodeBase64 PROCEDURE()
F SIGNED(1)
Store STRING(4)
SH BYTE(0)
OutF SIGNED(1)
B  BYTE,DIM(3)
N  BYTE,AUTO
newSize LONG
newCS &STRING
  CODE   
   newSize = INT(LEN(SELF.s) * 3/4) !+ 1
   newCS &= new STRING(newSize)
   LOOP WHILE F <= LEN(SELF.s)
    IF INSTRING(SELF.s[F],SystemStringClass_Base64Encode)
      SH += 1
      Store[SH] = SELF.s[F]
      IF Sh = 4
        N = SELF.Take32(B[1],B[2],B[3],Store)
        newCS[OutF] = CHR(B[1])
        OutF += 1
        IF N = 1 THEN BREAK .
        newCS[OutF] = CHR(B[2])
        OutF += 1
        IF N = 2 THEN BREAK .
        newCS[OutF] = CHR(B[3])
        OutF += 1
        Sh = 0
      END
    END
    F += 1
  END
  SELF.DisposeIt()
  SELF.s &= newCS

SystemStringClass.Compress PROCEDURE()
compressedBuffer &STRING
result LONG
 CODE
    compressedBuffer &= COMPRESS(SELF.s,-1,result)
    IF result >0
       SELF.DisposeIt()
       SELF.s &= compressedBuffer
    END

SystemStringClass.Decompress PROCEDURE()
decompressedBuffer &STRING
result LONG
 CODE
    DECOMPRESS(decompressedBuffer,SELF.s,result)
    IF result > 0
       SELF.DisposeIt()
       SELF.s &= decompressedBuffer
    END
!**********************
!Lines support
!**********************

!region Line Support
SystemStringClass.SplitToLines PROCEDURE(LONG lineLength)
strLen LONG,AUTO
start   LONG,AUTO
amount  LONG,AUTO
CurErr  SIGNED,AUTO
 CODE
    SELF.CleanSplitQueue(SELF.lines)
    strLen = SELF.Length()
    start  = 1
    LOOP WHILE strLen <> 0
       amount = lineLength
       IF amount > strLen
         amount = strLen
       END
       SELF.AddLine(SELF.s [start : start + amount - 1])
       start += amount
       strLen-= amount
    END
 
SystemStringClass.SplitToLines PROCEDURE()
 CODE
    SELF.Split(SELF.EOLMarker)

SystemStringClass.CountLines PROCEDURE()
 CODE
    RETURN RECORDS(SELF.lines)
    
SystemStringClass.AddLine PROCEDURE(STRING svalue)
 CODE
    SELF.InsertLine(0, svalue)

SystemStringClass.InsertLine PROCEDURE(ULONG startIndex, STRING svalue) 
splitStr    &SystemStringClass
 CODE
    splitStr &= new SystemStringClass()
    splitStr.Str(svalue)
    SELF.lines.Str &= splitStr
    ADD(SELF.lines, startIndex)
 
SystemStringClass.GetLineValue PROCEDURE(ULONG lineNumber)
 CODE
    IF lineNumber <= RECORDS(SELF.lines)
       GET(SELF.lines,lineNumber)
       RETURN SELF.lines.Str.ToString()
    ELSE
       RETURN ''
    END

SystemStringClass.GetLineTrimValue PROCEDURE(ULONG lineNumber)
 CODE
    IF lineNumber <= RECORDS(SELF.lines)
       GET(SELF.lines,lineNumber)
       RETURN CLIP(LEFT(SELF.ReplaceSubString(SELF.lines.Str.ToString(),SELF.EOLMarker,'')))
    ELSE
       RETURN ''
    END

SystemStringClass.GetLine PROCEDURE(ULONG lineNumber)
 CODE
    IF lineNumber <= RECORDS(SELF.lines)
       GET(SELF.lines,lineNumber)
       RETURN SELF.lines.Str
    ELSE
       RETURN 0
    END
    
SystemStringClass.GetLines                  PROCEDURE()
 CODE
    RETURN SELF.lines

SystemStringClass.DeleteLine                PROCEDURE(ULONG lineNumber)
 CODE
    IF lineNumber <= RECORDS(SELF.lines)       
       GET(SELF.lines,lineNumber)
       IF NOT ERRORCODE()
          DISPOSE(SELF.lines.Str)
          DELETE(SELF.lines)
       END
    END

SystemStringClass.FromLines PROCEDURE()
 CODE
    SELF.FromLines('')
    
SystemStringClass.FromLines PROCEDURE(STRING delimiter)
 CODE
    SELF.FromLines('',delimiter)
    
SystemStringClass.FromLinesWithEOL PROCEDURE()
 CODE
    SELF.FromLines('',SELF.EOLMarker)

SystemStringClass.FromLines PROCEDURE(STRING leftDelimiter, STRING rigthDelimiter)
idx LONG
 CODE
    SELF.FromString('')
    LOOP idx = 1 TO RECORDS(SELF.lines)
       GET(SELF.lines,idx)
       SELF.Append(leftDelimiter&SELF.lines.Str.ToString()&rigthDelimiter)
    END

SystemStringClass.GetEOLMarker PROCEDURE()
 CODE
    RETURN SELF.EOLMarker

SystemStringClass.SetEOLMarker PROCEDURE(STRING EOLMarker)
 CODE
    SELF.EOLMarker = EOLMarker
    
SystemStringClass.GetTokensDelimiters       PROCEDURE()
 CODE
    RETURN SELF.TokensDelimiters
    
SystemStringClass.SetTokensDelimiters       PROCEDURE(STRING delimiters)
 CODE
    SELF.TokensDelimiters = delimiters

SystemStringClass.ResetToken                PROCEDURE()
 CODE
    SELF.TokenSelStart = 0
    SELF.TokenSelEnd   = 0

SystemStringClass.ResetTokenToEnd           PROCEDURE()
 CODE
    SELF.TokenSelStart = SELF.Length()
    SELF.TokenSelEnd   = SELF.TokenSelStart

SystemStringClass.IsTokenNull               PROCEDURE()
 CODE
    IF SELF.s &= NULL OR SELF.TokenSelStart = SELF.TokenSelEnd OR SELF.TokenSelStart > SELF.TokenSelEnd OR SELF.TokenSelEnd > SELF.Length() OR SELF.TokenSelStart < 1
       RETURN TRUE
    ELSE
       RETURN FALSE
    END

SystemStringClass.IsTokenInited             PROCEDURE()
 CODE
    IF SELF.TokenSelStart = 0 AND SELF.TokenSelEnd = 0
       RETURN FALSE
    ELSE
       RETURN TRUE
    END

SystemStringClass.GetCurrentToken           PROCEDURE()
 CODE
    IF NOT SELF.IsTokenNull()
       RETURN SELF.s[(SELF.TokenSelStart):(SELF.TokenSelEnd)]
    ELSE
       RETURN ''
    END

SystemStringClass.FoundToken PROCEDURE(STRING svalue)
 CODE
    RETURN SELF.FoundToken(svalue,1)
    
SystemStringClass.FoundToken PROCEDURE(STRING svalue, ULONG startIndex)
idx        LONG,AUTO
startIdx   LONG,AUTO
endIdx     LONG,AUTO
startIdxOK   BYTE,AUTO
endIdxOK     BYTE,AUTO
 CODE
    idx = SELF.IndexOf(svalue, startIndex)
    IF idx>0
       startIdx = idx
       endIdx = startIdx + LEN(svalue) - 1
       !Check the what if was found is a token
       !to be a token it must be delimited by the delimiters
       startIdxOK = true
       IF startIdx > 1
          IF NOT INSTRING(SELF.s[(startIdx-1):(startIdx-1)], SELF.TokensDelimiters, 1, 1)
             !The left limit NOT OK
             startIdxOK = false
          END
       END
       endIdxOK = false
       IF startIdxOK = true
          endIdxOK = true
          IF endIdx < SELF.Length()
             IF NOT INSTRING(SELF.s[(endIdx+1):(endIdx+1)], SELF.TokensDelimiters, 1, 1)
                !The right limit NOT OK
                endIdxOK = false
             END
          END
       END
       IF startIdxOK AND endIdxOK       
          SELF.TokenSelStart = startIdx
          SELF.TokenSelEnd   = endIdx
          RETURN startIdx
       END
    END
    SELF.ResetToken()
    RETURN 0

SystemStringClass.NextToken                 PROCEDURE()
idx1 LONG
countIdx LONG
subLength LONG,AUTO
 CODE
    IF SELF.s &= NULL
       SELF.ResetToken()
       RETURN ''
    END
    subLength = SELF.Length()
    !remove the leading delimeters
    countIdx = SELF.TokenSelEnd + 1
    LOOP idx1 = SELF.TokenSelEnd + 1 TO subLength - 1
        IF NOT INSTRING(SELF.s[(idx1):(idx1)], SELF.TokensDelimiters, 1, 1)
           BREAK
        END
        countIdx += 1
    END
    SELF.TokenSelStart = countIdx
    !find the first char that is in the TokensDelimiters
    LOOP idx1 = countIdx TO subLength - 1
        IF INSTRING(SELF.s[(idx1):(idx1)], SELF.TokensDelimiters, 1, 1)
           BREAK
        END
        countIdx += 1
    END    
    SELF.TokenSelEnd = countIdx - 1
    RETURN SELF.GetCurrentToken()

SystemStringClass.PrevToken                 PROCEDURE()
idx1 LONG
countIdx LONG
 CODE
    IF SELF.s &= NULL
       SELF.ResetToken()
       RETURN ''
    END

    !find the first previous char the is not in the TokensDelimiters to get the end position of the token
    countIdx = SELF.TokenSelStart - 1
    LOOP idx1 = countIdx TO 1 BY -1
        IF NOT INSTRING(SELF.s[(idx1):(idx1)], SELF.TokensDelimiters, 1, 1)
           BREAK
        END
        countIdx -= 1
    END

    SELF.TokenSelEnd = countIdx
    
    !find the first previous char the is not in the TokensDelimiters to get the first position of the token
    LOOP idx1 = countIdx TO 1 BY -1
        IF INSTRING(SELF.s[(idx1):(idx1)], SELF.TokensDelimiters, 1, 1)
           BREAK
        END
        countIdx -= 1
    END

    SELF.TokenSelStart = countIdx + 1
    RETURN SELF.GetCurrentToken()

SystemStringClass.PushToken                 PROCEDURE(ULONG startIndex, STRING svalue)
 CODE
    SELF.FromString(SELF.Insert(startIndex, svalue))
    SELF.TokenSelStart = startIndex
    SELF.TokenSelEnd   = startIndex + LEN(svalue)
    
SystemStringClass.PopToken                  PROCEDURE()
 CODE
    IF NOT SELF.IsTokenNull()
       SELF.FromString(SELF.SubstringLeft(SELF.TokenSelStart)&SELF.Substring(SELF.TokenSelEnd+1))
       SELF.TokenSelEnd = SELF.TokenSelStart
       RETURN TRUE
    ELSE
       RETURN FALSE
    END
!endregion  

SystemStringClass.FormatString PROCEDURE(STRING objectsString, <STRING objectsString1>, <STRING objectsString2>, <STRING objectsString3>, <STRING objectsString4>, <STRING objectsString5>)
! CODE  
!    IF OMITTED(objectsString1)
!       RETURN SELF.FormatString(SELF.ToString(), objectsString)
!    END
!    IF OMITTED(objectsString2)
!       RETURN SELF.FormatString(SELF.ToString(), objectsString, objectsString1)
!    END
!    IF OMITTED(objectsString3)
!       RETURN SELF.FormatString(SELF.ToString(), objectsString, objectsString1, objectsString2)
!    END
!    IF OMITTED(objectsString4)
!       RETURN SELF.FormatString(SELF.ToString(), objectsString, objectsString1, objectsString2, objectsString3)
!    END
!    IF OMITTED(objectsString5)
!       RETURN SELF.FormatString(SELF.ToString(), objectsString, objectsString1, objectsString2, objectsString3, objectsString4)
!    END
!    RETURN SELF.FormatString(SELF.ToString(), objectsString, objectsString1, objectsString2, objectsString3, objectsString4, objectsString5)
!    
!SystemStringClass.FormatString PROCEDURE(STRING formatString, STRING objectsString, <STRING objectsString1>, <STRING objectsString2>, <STRING objectsString3>, <STRING objectsString4>, <STRING objectsString5>)
formatStringRef         SystemStringClass
indxPar LONG
 CODE
    formatStringRef.FromString(SELF.Replace('{{0}',objectsString))
    IF NOT OMITTED(objectsString1)
       formatStringRef.FromString(formatStringRef.Replace('{{1}',objectsString1))
    END
    IF NOT OMITTED(objectsString2)
       formatStringRef.FromString(formatStringRef.Replace('{{2}',objectsString2))
    END
    IF NOT OMITTED(objectsString3)
       formatStringRef.FromString(formatStringRef.Replace('{{3}',objectsString3))
    END
    IF NOT OMITTED(objectsString4)
       formatStringRef.FromString(formatStringRef.Replace('{{4}',objectsString4))
    END
    IF NOT OMITTED(objectsString5)
       formatStringRef.FromString(formatStringRef.Replace('{{5}',objectsString5))
    END
    RETURN formatStringRef.ToString()
