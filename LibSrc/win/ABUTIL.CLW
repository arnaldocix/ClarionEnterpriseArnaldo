  MEMBER

  MAP
AddDollar     PROCEDURE(STRING Src),STRING
RemoveDollar  PROCEDURE(STRING Src),STRING
  END

  INCLUDE('CWSYNCHM.INC'),ONCE
  INCLUDE('CWSYNCHC.INC'),ONCE
  INCLUDE('ABUTIL.INC'),ONCE
  INCLUDE('ABUTIL.TRN'),ONCE
  INCLUDE('ABFILE.INC'),ONCE
  INCLUDE('KEYCODES.CLW'),ONCE


ConstDescriptorList QUEUE,TYPE
ItemType              BYTE
Destination           ANY
                    END

MaxTLen EQUATE(1024)

TranslatorQueue     QUEUE,TYPE
TextProp              CSTRING(MaxTLen)
Replacement           CSTRING(MaxTLen)
                    END


TypeMappingQueue    QUEUE,TYPE
ControlType           USHORT
Property              USHORT
                    END

INIUnknown          EQUATE('---Unknown---')

! --- ConstantClass

ConstantClass.Construct PROCEDURE

  CODE
  SELF.Complete = False
  SELF.TerminatorField = 1                  ! C5 default
  SELF.TerminatorInclude = False            ! C5 default
  RETURN

ConstantClass.Init PROCEDURE(BYTE Termination)

  CODE
  SELF.Str &= NULL
  SELF.Descriptor &= NEW ConstDescriptorList
  SELF.Termination=Termination
  CLEAR(SELF.TerminatorValue)
  SELF.Complete = False
  SELF.Reset
  RETURN

ConstantClass.Kill PROCEDURE

i        LONG,AUTO

  CODE
  SELF.Complete = True
  LOOP i = 1 TO RECORDS (SELF.Descriptor)
    GET (SELF.Descriptor, i)
    SELF.Descriptor.Destination &= NULL
  END

  DISPOSE (SELF.Descriptor)
  DISPOSE (SELF.Str)
  RETURN

ConstantClass.Reset PROCEDURE

  CODE
  SELF.RecordPnt= 1
  SELF.CharPnt  = 1
  SELF.Complete = False

  IF SELF.Str &= NULL
    SELF.Items = 0
  ELSE
    CASE SELF.Termination
    OF Term:Byte
      SELF.Items = SELF.GetByte()
    OF Term:UShort
      SELF.Items = SELF.GetUShort()
    END
  END
  RETURN

ConstantClass.Set PROCEDURE(*STRING Src)

  CODE
  DISPOSE (SELF.Str)
  SELF.SourceSize = LEN (Src)
  SELF.Str &= NEW STRING (SELF.SourceSize)
  SELF.Str = Src
  SELF.Reset
  RETURN

ConstantClass.AddItem PROCEDURE (BYTE ItemType, *? Dest)

  CODE
  CLEAR (SELF.Descriptor)
  SELF.Descriptor.ItemType     = ItemType
  SELF.Descriptor.Destination &= Dest
  ADD(SELF.Descriptor)
? ASSERT(~ERRORCODE())
  RETURN

ConstantClass.Next PROCEDURE()

i        LONG,AUTO

  CODE
  IF SELF.Complete
    RETURN Level:Notify
  END

  CASE SELF.Termination
  OF Term:Byte
  OROF Term:UShort
    IF SELF.RecordPnt > SELF.Items
      SELF.Complete = True
      RETURN Level:Notify
    END
  OF Term:EndGroup
    IF SELF.CharPnt > SELF.SourceSize
      SELF.Complete = True
      RETURN Level:Notify
    END
  END

  LOOP i = 1 TO RECORDS (SELF.Descriptor)
    GET (SELF.Descriptor, i)

    CASE SELF.Descriptor.ItemType
    OF ConstType:CString
      SELF.Descriptor.Destination = SELF.GetCString()
    OF ConstType:PString
      SELF.Descriptor.Destination = SELF.GetPString()
    OF ConstType:Byte
      SELF.Descriptor.Destination = SELF.GetByte()
    OF ConstType:Short
      SELF.Descriptor.Destination = SELF.GetShort()
    OF ConstType:UShort
      SELF.Descriptor.Destination = SELF.GetUShort()
    OF ConstType:Long
      SELF.Descriptor.Destination = SELF.GetLong()
    ELSE
?     ASSERT(False)
    END

    IF i = SELF.TerminatorField AND SELF.Termination = Term:FieldValue AND SELF.Descriptor.Destination = SELF.TerminatorValue
      SELF.Complete = TRUE
      IF ~SELF.TerminatorInclude
        RETURN Level:Notify
      END
    END
  END

  IF SELF.Termination = Term:FieldValue AND SELF.Complete AND ~SELF.TerminatorInclude
    RETURN Level:Notify
  END

  SELF.RecordPnt += 1
  RETURN Level:Benign


ConstantClass.Next PROCEDURE (QUEUE Q)

  CODE
  SELF.Reset
  LOOP WHILE SELF.Next() = Level:Benign
    ADD (Q)
?   ASSERT(~ERRORCODE())
  END


ConstantClass.Next PROCEDURE (FILE F)

  CODE
  SELF.Reset
  LOOP WHILE SELF.Next() = Level:Benign
    ADD (F)
?   ASSERT(~ERRORCODE())
  END


ConstantClass.GetByte PROCEDURE()

RVal  BYTE,AUTO

  CODE
? ASSERT(SELF.CharPnt <= LEN(SELF.Str))     !Exceeded limit of source string, probable descriptor/group structure layout mismatch
  RVal = VAL(SELF.Str[SELF.CharPnt])
  SELF.CharPnt += 1
  RETURN RVal


ConstantClass.GetUShort PROCEDURE()

RVal USHORT,AUTO

  CODE
  RVal = SELF.GetByte()
  RETURN RVal + BSHIFT (SELF.GetByte(), 8)


ConstantClass.GetShort PROCEDURE()

RVal  SHORT,AUTO

  CODE
  RVal = SELF.GetUShort()            !Req'd for type conversion
  RETURN RVal


ConstantClass.GetLong PROCEDURE()

RVal  LONG,AUTO

  CODE
  RVal = SELF.GetUShort()
  RETURN RVal + BSHIFT (SELF.GetShort(), 16)


ConstantClass.GetPString PROCEDURE()

L     BYTE,AUTO
i     BYTE,AUTO
RVal  STRING(256),AUTO

  CODE
  L = SELF.GetByte()
  IF L = 0
    RETURN ''
  END

  LOOP i = 1 TO L
    RVal[i] = CHR(SELF.GetByte())
  END
  RETURN RVal [1 : L]


ConstantClass.GetCString PROCEDURE()

b     BYTE,AUTO
Cnt   UNSIGNED(0)
RVal  STRING(1024),AUTO

  CODE
  LOOP
    b = SELF.GetByte()
    IF b = 0
      BREAK
    ELSE
      Cnt += 1
      RVal[Cnt] = CHR(b)
    END
  WHILE Cnt < SIZE (RVal)

  IF Cnt = 0
    RETURN ''
  END
  RETURN RVal [1 : Cnt]

! --- FieldPairs class. Manages a queue with two pairs of fields

FieldPairsClass.AddItem PROCEDURE (*? Left)
  CODE
? ASSERT(~(SELF.List &= NULL))
  CLEAR(SELF.List)
  SELF.List.Left &= Left
  SELF.List.Right = Left
  ADD(SELF.List)
  RETURN

FieldPairsClass.AddPair PROCEDURE (*? Left, *? Right)
  CODE
? ASSERT(~(SELF.List &= NULL))
  CLEAR(SELF.List)
  SELF.List.Left &= Left
  SELF.List.Right &= Right
  ADD(SELF.List)
  RETURN

FieldPairsClass.AssignLeftToRight PROCEDURE
I UNSIGNED,AUTO
  CODE
  LOOP I = 1 TO RECORDS(SELF.List)
    GET(SELF.List,I)
    SELF.List.Right = SELF.List.Left
    PUT(SELF.List)
  END
  RETURN

FieldPairsClass.AssignRightToLeft PROCEDURE
I UNSIGNED,AUTO
  CODE
  LOOP I = 1 TO RECORDS(SELF.List)
    GET(SELF.List,I)
    SELF.List.Left = SELF.List.Right
    PUT(SELF.List)
  END
  RETURN

FieldPairsClass.ClearLeft PROCEDURE
I UNSIGNED,AUTO
  CODE
  LOOP I = 1 TO RECORDS(SELF.List)
    GET(SELF.List,I)
    CLEAR(SELF.List.Left)
  END
  RETURN

FieldPairsClass.ClearRight PROCEDURE
I UNSIGNED,AUTO
  CODE
  LOOP I = 1 TO RECORDS(SELF.List)
    GET(SELF.List,I)
    CLEAR(SELF.List.Right)
  END
  RETURN

FieldPairsClass.EqualLeftRight PROCEDURE
I UNSIGNED,AUTO
  CODE
  LOOP I = 1 TO RECORDS(SELF.List)
    GET(SELF.List,I)
    IF SELF.List.Left <> SELF.List.Right
      RETURN FALSE
    END
  END
  RETURN TRUE

FieldPairsClass.Equal PROCEDURE
  CODE
  RETURN SELF.EqualLeftRight()

FieldPairsClass.Kill PROCEDURE
I UNSIGNED,AUTO
  CODE
  IF ~SELF.List &= NULL
    LOOP I = 1 TO RECORDS(SELF.List)
      GET(SELF.List,I)
      SELF.List.Left &= NULL
      SELF.List.Right &= NULL
    END
    DISPOSE(SELF.List)
  END
  RETURN

FieldPairsClass.Init PROCEDURE
  CODE
  SELF.Kill
  SELF.List &= NEW FieldPairsQueue
  RETURN

! --- BufferedPairs class. Manages a queue with two pairs of fields

BufferedPairsClass.AddPair PROCEDURE(*? Left,*? Right)
Temp ANY
  CODE
? ASSERT(~(SELF.List &= NULL))
  CLEAR(SELF.List)
  SELF.List.Left &= Left
  SELF.List.Right &= Right
  Temp = Right                   ! Make sure Buffer has correct type
  CLEAR(Right)
  SELF.RealList.Buffer = Right
  Right = Temp
  ADD(SELF.List)
  RETURN

BufferedPairsClass.AssignLeftToBuffer PROCEDURE
I UNSIGNED,AUTO
  CODE
  LOOP I = 1 TO RECORDS(SELF.List)
    GET(SELF.List,I)
    SELF.RealList.Buffer = SELF.List.Left
    PUT(SELF.RealList)
  END
  RETURN

BufferedPairsClass.AssignRightToBuffer PROCEDURE
I UNSIGNED,AUTO
  CODE
  LOOP I = 1 TO RECORDS(SELF.List)
    GET(SELF.List,I)
    SELF.RealList.Buffer = SELF.List.Right
    PUT(SELF.RealList)
  END
  RETURN

BufferedPairsClass.AssignBufferToLeft PROCEDURE
I UNSIGNED,AUTO
  CODE
  LOOP I = 1 TO RECORDS(SELF.List)
    GET(SELF.List,I)
    SELF.List.Left = SELF.RealList.Buffer
    PUT(SELF.RealList)
  END
  RETURN

BufferedPairsClass.AssignBufferToRight PROCEDURE
I UNSIGNED,AUTO
  CODE
  LOOP I = 1 TO RECORDS(SELF.List)
    GET(SELF.List,I)
    SELF.List.Right = SELF.RealList.Buffer
    PUT(SELF.RealList)
  END
  RETURN

BufferedPairsClass.EqualLeftBuffer     PROCEDURE(BYTE CaseSensitive)
I UNSIGNED,AUTO
  CODE
  LOOP I = 1 TO RECORDS(SELF.List)
    GET(SELF.List,I)
    IF CaseSensitive
       IF SELF.RealList.Left <> SELF.RealList.Buffer
         RETURN FALSE
       END
    ELSE
       IF UPPER(SELF.RealList.Left) <> UPPER(SELF.RealList.Buffer)
         RETURN FALSE
       END
    END
  END
  RETURN TRUE

BufferedPairsClass.EqualLeftBuffer PROCEDURE
  CODE
  RETURN SELF.EqualLeftBuffer(False)

BufferedPairsClass.EqualRightBuffer    PROCEDURE(BYTE CaseSensitive)
I UNSIGNED,AUTO
  CODE
  LOOP I = 1 TO RECORDS(SELF.List)
    GET(SELF.List,I)
    IF CaseSensitive
       IF SELF.RealList.Right <> SELF.RealList.Buffer
         RETURN FALSE
       END
    ELSE
       IF UPPER(SELF.RealList.Right) <> UPPER(SELF.RealList.Buffer)
         RETURN FALSE
       END
    END
  END
  RETURN TRUE

BufferedPairsClass.EqualRightBuffer PROCEDURE
  CODE
  RETURN SELF.EqualRightBuffer(False)

BufferedPairsClass.Kill PROCEDURE
I UNSIGNED,AUTO
  CODE
  IF ~SELF.RealList &= NULL
    LOOP I = 1 TO RECORDS (SELF.RealList)
      GET(SELF.RealList,I)
      SELF.RealList.Left &= NULL
      SELF.RealList.Right &= NULL
      SELF.RealList.Buffer &= NULL
    END
    DISPOSE(SELF.RealList)
  END
  SELF.List &= SELF.RealList
  RETURN

BufferedPairsClass.Init PROCEDURE
  CODE
  SELF.Kill
  SELF.RealList &= NEW BufferedPairsQueue
  SELF.List &= SELF.RealList
  RETURN

! --- INIClass

INIClass.Construct PROCEDURE()
  CODE
  SELF.critSect &= NewCriticalSection()
  RETURN

INIClass.Destruct PROCEDURE()
  CODE
  SELF.critSect.kill()
  RETURN

INIClass.Init PROCEDURE(STRING S)
  CODE
  SELF.Init(S, NVD_INI)
  RETURN

INIClass.Init PROCEDURE(STRING s, UNSIGNED nvType, LONG extraData)
  CODE
  SELF.extraData = extraData
  SELF.nvType = nvType
  SELF.FileName = S
  SELF.Sectors &= NEW SectorQueue
  SELF.Fetch
  SELF.FetchQueue('__Dont_Touch_Me__','Sectors',SELF.Sectors,SELF.Sectors.Family,SELF.Sectors.Item,SELF.Sectors.Type) !***
  RETURN

INIClass.Kill PROCEDURE

critProc  CriticalProcedure

  CODE
  critProc.init(SELF.critSect)
  IF  NOT SELF.Sectors &= NULL
    SELF.UpdateQueue('__Dont_Touch_Me__','Sectors',SELF.Sectors,SELF.Sectors.Family,SELF.Sectors.Item,SELF.Sectors.Type) !***
    DISPOSE(SELF.Sectors)
    SELF.Sectors &= NULL
  END
  RETURN

INIClass.GetSector PROCEDURE(STRING Family, STRING Item, STRING Type)

critProc  CriticalProcedure

  CODE
  critProc.init(SELF.critSect)
  RETURN Family & '$' & AddDollar(Item) & '$$' & Type   ! *** Primary to make getfamily easy


INIClass.GetFamily PROCEDURE(STRING Sector, *CSTRING Family, *CSTRING Item, *CSTRING Type)

critProc  CriticalProcedure
FP        UNSIGNED,AUTO
SP        UNSIGNED,AUTO

  CODE
  critProc.init(SELF.critSect)
  FP = INSTRING('$',Sector,1,1)
  SP = INSTRING('$$',Sector,1,1)
  LOOP
   IF Sector[SP+2] = '$' ! Used $ in Item Text
      SP = INSTRING('$$',Sector,1,SP+3)
      CYCLE
   END
   BREAK
  END

? ASSERT(FP > 1 AND SP < LEN(Sector) AND SP > FP+1)
  Family = Sector[1:FP-1]
  Item = Sector[FP+1:SP-1]       !*** More - maybe, consider null items
  Type = Sector[SP+2:LEN(Sector)]
  RETURN

INIClass.AddSector PROCEDURE(STRING Sector)

critProc  CriticalProcedure

  CODE
  critProc.init(SELF.critSect)
  SELF.GetFamily (Sector, SELF.Sectors.Family, SELF.Sectors.Item, SELF.Sectors.Type)
  GET (SELF.Sectors, SELF.Sectors.Family, SELF.Sectors.Item, SELF.Sectors.Type)

  IF ERRORCODE()
    ADD (SELF.Sectors, SELF.Sectors.Family, SELF.Sectors.Item, SELF.Sectors.Type)
  END
  RETURN

INIClass.DeleteSector PROCEDURE(STRING Sector)

critProc  CriticalProcedure

  CODE
  critProc.init(SELF.critSect)
  SELF.GetFamily (Sector, SELF.Sectors.Family, SELF.Sectors.Item, SELF.Sectors.Type)
  GET (SELF.Sectors, SELF.Sectors.Family, SELF.Sectors.Item, SELF.Sectors.Type)

  IF ~ERRORCODE()
    DELETE(SELF.Sectors)
  END
  RETURN

INIClass.GetSectors PROCEDURE(<STRING Family>, <STRING Item>, <STRING Type>, SectorQueue SQ)

critProc  CriticalProcedure
I         UNSIGNED,AUTO

  CODE
  critProc.init(SELF.critSect)
  FREE(SQ)     !**** Could be dodgy, maybe we should append? !** Possibly, need to keep an eye on it.

  LOOP I = 1 TO RECORDS(SELF.Sectors)
    GET(SELF.Sectors,I)
    IF Family AND Family<>SELF.Sectors.Family THEN CYCLE .  !*** We could optimise this using the sorted queue get syntax (Get(,SELF.Sectors.Family etc)
    IF Item AND Item<>SELF.Sectors.Item THEN CYCLE .        !*** If we can be bothered
    IF Type AND Type<>SELF.Sectors.Type THEN CYCLE .        !*** Nope, me neither
    SQ = SELF.Sectors
    SQ.Item = RemoveDollar(SELF.Sectors.Item)
    ADD(SQ)
  END
  RETURN

INIClass.Fetch PROCEDURE(STRING Sector, STRING Name, STRING Default, STRING Filename)

critProc  CriticalProcedure
RVal      CSTRING(1024),AUTO

  CODE
  critProc.init(SELF.critSect)
  CASE SELF.nvType
  OF NVD_INI
    RVal = GETINI (Sector, Name, Default, Filename)
  OF NVD_Registry
    RVal = GETREG (CHOOSE(SELF.extraData = 0, REG_CLASSES_ROOT, SELF.extraData), CHOOSE(SELF.Filename = '', Sector, SELF.Filename & '\' & Sector), Name)
    IF RVal = '' THEN RVal = Default.
  OF NVD_Table
?   ASSERT(FALSE, 'NV_Table: Not Supported Yet')
    RVal = ''
  END
  RETURN RVal


INIClass.Update PROCEDURE(STRING Sector, STRING Name, STRING Value, STRING Filename)

critProc  CriticalProcedure

  CODE
  critProc.init(SELF.critSect)
  CASE(SELF.nvType)
  OF NVD_INI
    PUTINI(Sector, Name, Value, Filename)
  OF NVD_Registry
    IF Value = ''            ! Setting a 'null' value may as well be a delete
      SELF.Remove (Sector, Name)
    ELSE
      PUTREG(CHOOSE(SELF.extraData = 0, REG_CLASSES_ROOT, SELF.extraData), CHOOSE(SELF.Filename = '', Sector, SELF.Filename & '\' & Sector), Name, Value)
    END
  OF NVD_Table
?   ASSERT(FALSE, 'NV_Table: Not Supported Yet')
  END
  RETURN

INIClass.Remove PROCEDURE (STRING Sector, STRING Name)

critProc  CriticalProcedure

  CODE
  critProc.init(SELF.critSect)

  CASE(SELF.nvType)
  OF NVD_INI
    PUTINI(Sector, Name,, SELF.Filename)
  OF NVD_Registry
    DELETEREG(CHOOSE(SELF.extraData = 0, REG_CLASSES_ROOT, SELF.extraData), CHOOSE(SELF.Filename = '', Sector, SELF.Filename & '\' & Sector), Name)
  OF NVD_Table
?   ASSERT(FALSE, 'NV_Table: Not Supported Yet')
  END
  RETURN

INIClass.Update PROCEDURE
  CODE
  RETURN

INIClass.Fetch PROCEDURE
  CODE
  RETURN

INIClass.Update PROCEDURE(STRING Sec,STRING Name,STRING Value)

critProc  CriticalProcedure

  CODE
  critProc.init(SELF.critSect)
  SELF.Update (Sec, Name, Value, SELF.FileName)
  RETURN

INIClass.Fetch PROCEDURE(STRING Sec,STRING Name,*? Value)

critProc  CriticalProcedure

  CODE
  critProc.init(SELF.critSect)
  Value = SELF.Fetch (Sec, Name, Value, SELF.FileName)
  RETURN

INIClass.TryFetch PROCEDURE(STRING Sec,STRING Name)
  CODE
  RETURN SELF.Fetch(Sec, Name, '', SELF.FileName)


INIClass.Fetch PROCEDURE(STRING Sec,STRING Name)

critProc  CriticalProcedure
Res       CSTRING(1000),AUTO

  CODE
  critProc.init(SELF.critSect)
  Res = SELF.Fetch (Sec, Name, INIUnknown, SELF.Filename)
? ASSERT(Res <> INIUnknown, 'INI Assert: Could not find ' & Name & ' in sector ' & Sec & ' of ' & SELF.Filename)
  RETURN Res


INIClass.FetchField PROCEDURE(STRING Sec,STRING Name,BYTE Field)

critProc  CriticalProcedure
Res       CSTRING(1000),AUTO
I         UNSIGNED,AUTO
Finger    UNSIGNED(1)

  CODE
  critProc.init(SELF.critSect)
  Res = SELF.Fetch (Sec, Name)

  LOOP WHILE Field > 1
    Finger = INSTRING (',', Res, 1, Finger)
?   ASSERT(Finger)

    Finger += 1
    Field -= 1
  END

  I = INSTRING (',', Res, 1, Finger)
  RETURN Res[Finger : CHOOSE(I = 0,LEN(Res),I-1) ]


INIClass.TryFetchField PROCEDURE(STRING Sec,STRING Name,BYTE Field)

critProc  CriticalProcedure
Res       CSTRING(1000),AUTO
I         UNSIGNED,AUTO
Finger    UNSIGNED(1)

  CODE
  critProc.init(SELF.critSect)
  Res = SELF.TryFetch(Sec,Name)

  IF NOT Res
    RETURN ''
  END

  LOOP WHILE Field > 1
    Finger = INSTRING (',', Res, 1, Finger)
    IF Finger = 0 THEN
      RETURN ''
    END
    Finger += 1
    Field -= 1
  END

  I = INSTRING (',', Res, 1, Finger)
  RETURN Res [Finger : CHOOSE(I = 0,LEN(Res),I-1) ]


! Queue Layout :
! [Sector]
! Name = ItemsInQueue
! Name_1 = F1,F2,F3 etc
INIClass.FetchQueue PROCEDURE(STRING Sector,STRING Name,QUEUE Q,*? F1,<*? F2>,<*? F3>,<*? F4>)

critProc  CriticalProcedure
NItems    UNSIGNED,AUTO
I         UNSIGNED,AUTO

  CODE
  critProc.init(SELF.critSect)
  NItems = SELF.TryFetch (Sector, Name)

  LOOP I = 1 TO NItems
    F1 = SELF.FetchField (Sector, Name & '_' & I, 1)
    IF ~OMITTED(6)
      F2 = SELF.FetchField (Sector, Name & '_' & I, 2)
    END
    IF ~OMITTED(7)
      F3 = SELF.FetchField (Sector, Name & '_' & I, 3)
    END
    IF ~OMITTED(8)
      F4 = SELF.FetchField (Sector, Name & '_' & I, 4)
    END
    ADD(Q)
  END
  RETURN

INIClass.UpdateQ PROCEDURE (STRING pSection, *QUEUE pQ)

critProc  CriticalProcedure
NbrOfRec  LONG,AUTO
EntryStr  STRING('ENTRY_'),STATIC
ValueStr  CSTRING(1000),AUTO
QField    ANY
ndx       LONG,AUTO
FldNdx    LONG

  CODE
  critProc.init(SELF.critSect)

  NbrOfRec = RECORDS (pQ)
  SELF.Update (pSection, 'Entries', NbrOfRec)

  LOOP ndx = 1 TO RECORDS (pQ)
    GET (pQ, ndx)
    IF ERRORCODE()
      BREAK
    END

    ValueStr = ''
    FldNdx = 1

    LOOP
      QField &= WHAT (pQ, FldNdx)

      IF QField &= NULL
        BREAK
      END

      IF ValueStr
        IF INSTRING (',', QField, 1, 1) <> 0
          ValueStr = ValueStr & ',[' & CLIP (QField) & ']'
        ELSE
          ValueStr = ValueStr & ',' & CLIP (QField)
        END
      ELSE
        IF INSTRING (',', QField, 1, 1) <> 0
          ValueStr = '[' & CLIP (QField) & ']'
        ELSE
          ValueStr = CLIP (QField)
        END
      END

      FldNdx += 1
    END

    SELF.Update (pSection, EntryStr & ndx, ValueStr)
  END
  RETURN

INIClass.FetchQ PROCEDURE (STRING pSection, *QUEUE pQ)

critProc  CriticalProcedure
NbrOfRec  LONG,AUTO
EntryStr  STRING('ENTRY_'),STATIC
ValueStr  CSTRING(1000),AUTO
FldNdx    LONG,AUTO
ndx       LONG,AUTO

  CODE
  critProc.init(SELF.critSect)
  FREE (pQ)

  NbrOfRec = SELF.Fetch (pSection, 'Entries')

  LOOP ndx = 1 TO NbrOfRec
    ValueStr = SELF.Fetch (pSection, EntryStr & ndx)
    IF ValueStr
      FldNdx = 1
      SELF.ParseStr (ValueStr, pQ, FldNdx)
      ADD (pQ)
    END
  END
  RETURN

INIClass.ParseStr PROCEDURE (STRING szValue, *QUEUE pQ, LONG FldNbr)

Pos       UNSIGNED,AUTO
AField    ANY

  CODE
  AField &= WHAT (pq, FldNbr)

  IF NOT AField &= NULL
    IF szValue[1] = '['
      Pos = INSTRING (']', szValue, 1, 1)
      IF Pos > 2
        AField = szValue [2 : Pos - 1]
        Pos = INSTRING (',', szValue, 1, Pos + 1)  ! Make sure that pos will point to next comma after the ]
      END
    ELSE
      Pos = INSTRING (',', szValue, 1, 1)
      IF Pos = 0
        AField = szValue
      ELSIF Pos = 1
        AField = ''
      ELSE
        AField = szValue [1 : Pos - 1]
      END
    END

    IF Pos <> 0
      IF szValue [Pos + 1 : LEN(szValue)]
        SELF.ParseStr (szValue[Pos + 1 : LEN(szValue)], pQ, FldNbr + 1)
      END
    END
  END
  RETURN

INIClass.UpdateQueue PROCEDURE(STRING Sector,STRING Name,QUEUE Q,*? F1,<*? F2>,<*? F3>,<*? F4>)

critProc  CriticalProcedure
I         UNSIGNED,AUTO
st        CSTRING(5000),AUTO

  CODE
  critProc.init(SELF.critSect)
  SELF.Update (Sector, Name, RECORDS(Q))

  LOOP I = 1 TO RECORDS(Q)
    GET(Q,I)
    St = F1
    IF ~OMITTED(6)
      St = St & ',' & F2
    END
    IF ~OMITTED(7)
      St = St & ',' & F3
    END
    IF ~OMITTED(8)
      St = St & ',' & F4
    END
    SELF.Update(Sector, Name & '_' & I, St)
  END
  RETURN

INIClass.Update PROCEDURE (STRING ProcedureName, WINDOW W)

Info      LIKE(WindowInfo),AUTO

  CODE
  Info.Maximized = CHOOSE (W {PROP:Maximize} <> 0)
  Info.Minimized = CHOOSE (W {PROP:Iconize} <> 0)

  Info.X = _nopos
  Info.Y = _nopos
  Info.W = _nopos
  Info.H = _nopos

  IF NOT Info.Minimized AND NOT Info.Maximized
    Info.X = W {PROP:XPos}
    Info.Y = W {PROP:YPos}

    IF W {PROP:Resize}
      Info.W = W {PROP:Width}
      Info.H = W {PROP:Height}
    END
  END

  SELF.UpdateWindowInfo (ProcedureName, Info)
  RETURN

INIClass.UpdateWindowInfo PROCEDURE (STRING ProcedureName, *WindowInfo Info)

critProc  CriticalProcedure,AUTO

   CODE
   critProc.Init (SELF.critSect)

   SELF.Update (ProcedureName, 'Maximize', CHOOSE (NOT Info.Maximized, 'No', 'Yes'))
   SELF.Update (ProcedureName, 'Minimize', CHOOSE (NOT Info.Minimized, 'No', 'Yes'))

   IF Info.X = _nopos
     SELF.Remove (ProcedureName, 'XPos')
   ELSE
     SELF.Update (ProcedureName, 'XPos', Info.X)
   END
   IF Info.Y = _nopos
     SELF.Remove (ProcedureName, 'YPos')
   ELSE
     SELF.Update (ProcedureName, 'YPos', Info.Y)
   END
   IF Info.W = _nopos
     SELF.Remove (ProcedureName, 'Width')
   ELSE
     SELF.Update (ProcedureName, 'Width', Info.W)
   END
   IF Info.H = _nopos
     SELF.Remove (ProcedureName, 'Height')
   ELSE
     SELF.Update (ProcedureName, 'Height', Info.H)
   END
   RETURN

INIClass.Fetch PROCEDURE (STRING ProcedureName, WINDOW W)

Info      LIKE(WindowInfo),AUTO
IsMax     BYTE,AUTO

  CODE
  SELF.FetchWindowInfo (ProcedureName, Info)

  IF Info.Got
    IsMax = W {PROP:Maximize}

    IF NOT Info.Maximized AND IsMax
      W {PROP:Maximize} = FALSE
    END

    W {PROP:XPos} = Info.X
    W {PROP:YPos} = Info.Y

    IF W {PROP:Resize}
      IF Info.W <> _nopos
        W {PROP:Width} = Info.W
      END
      IF Info.H <> _nopos
        W {PROP:Height} = Info.H
      END
    END

    IF Info.Maximized AND NOT IsMax
      W {PROP:Maximize} = TRUE
    END

    W {PROP:Iconize} = Info.Minimized
  END

INIClass.FetchWindowInfo PROCEDURE (STRING ProcedureName, *WindowInfo Info)

critProc  CriticalProcedure,AUTO

  CODE
  critProc.Init (SELF.critSect)

  CASE SELF.TryFetch (ProcedureName, 'Maximize')
  OF 'No'
    Info.Maximized = FALSE
  OF 'Yes'
    Info.Maximized = TRUE
  ELSE
    Info.Got = FALSE
    RETURN
  END

  Info.Minimized = FALSE
  SELF.Fetch (ProcedureName, 'Minimize', Info.Minimized)

  Info.X = _nopos
  Info.Y = _nopos
  Info.W = _nopos
  Info.H = _nopos

  SELF.Fetch (ProcedureName, 'XPos', Info.X)
  SELF.Fetch (ProcedureName, 'YPos', Info.Y)
  SELF.Fetch (ProcedureName, 'Width', Info.W)
  SELF.Fetch (ProcedureName, 'Height', Info.H)

  Info.Got = TRUE
  RETURN


AddDollar PROCEDURE(STRING Src)

Hold  STRING(1024),AUTO
SP    UNSIGNED,AUTO
TP    UNSIGNED(0)

  CODE
  LOOP SP = 1 TO LEN(SRC)
    IF Src[SP] = '$'
      TP += 2
      Hold[TP-1] = '$'
      Hold[TP] = '$'
    END
    TP += 1
    Hold[TP] = Src[SP]
  END
  RETURN Hold [1 : TP]


RemoveDollar PROCEDURE(STRING Src)

Hold  STRING(1024),AUTO
SP    UNSIGNED,AUTO
TP    UNSIGNED,AUTO
L     UNSIGNED,AUTO

  CODE
  TP = 0
  SP = 0
  L  = LEN (Src)

  LOOP WHILE SP < L
     TP += 1
     SP += 1
     IF Src[SP] = '$'
       IF SP + 2 <= L
         IF Src[SP+1] = '$' AND Src[SP+2] = '$'
           SP += 2
         END
       END
     END
     Hold[TP] = Src[SP]
  END
  IF TP = 0
    RETURN ''
  END
  RETURN Hold [1 : TP]

! --- SelectFileClass

SelectFileClass.Init PROCEDURE
  CODE
  SELF.WindowTitle = Dfl:DefaultWindowTitle
  SELF.Flags = Dfl:DefaultFlags
  RETURN

SelectFileClass.AddMask PROCEDURE (STRING FileMasks)
  CODE
  IF FileMasks
    IF SELF.MaskString
      SELF.MaskString = SELF.MaskString & '|' & CLIP(FileMasks)
    ELSE
      SELF.MaskString = CLIP(FileMasks)
    END
  END
  RETURN

SelectFileClass.AddMask PROCEDURE (STRING Description, STRING Pattern)
  CODE
? ASSERT(~INSTRING ('|', Description, 1, 1) AND ~INSTRING ('|', Pattern, 1, 1))
  SELF.AddMask (CLIP(Description) & '|' & Pattern)
  RETURN

SelectFileClass.SetMask PROCEDURE (STRING Description, STRING Pattern)
  CODE
? ASSERT(~INSTRING ('|', Description, 1, 1) AND ~INSTRING ('|', Pattern, 1, 1))
  SELF.SetMask (CLIP(Description) & '|' & Pattern)
  RETURN

SelectFileClass.SetMask PROCEDURE(STRING FileMask)
  CODE
  CLEAR(SELF.MaskString)
  SELF.AddMask (FileMask)
  RETURN

SelectFileClass.GetCancelled PROCEDURE()
  CODE
  RETURN SELF.DialogCancelled

SelectFileClass.Ask PROCEDURE (BYTE KeepDir)

Result    CSTRING(File:MaxFilePath),AUTO

  CODE
  SELF.DialogCancelled = FALSE
  IF KeepDir
    SELF.SavePath = PATH()
  END
  IF SELF.DefaultDirectory
    SETPATH (SELF.DefaultDirectory)
  END

  Result = SELF.DefaultFile

  IF ~FILEDIALOG (SELF.WindowTitle, Result, CHOOSE(SELF.MaskString = '', Dfl:DefaultFileMask, SELF.MaskString), SELF.Flags)
    SELF.DialogCancelled = TRUE
    IF SELF.ClearOnCancel
       CLEAR(Result)
    END
  END

  IF KeepDir
    SETPATH (SELF.SavePath)
  END
  RETURN Result


SelectFileClass.Ask   PROCEDURE (SelectFileQueue DFQ, BYTE KeepDir)

ActualFlags BYTE,AUTO
Found       CSTRING(10000),AUTO
Path        CSTRING(File:MaxFilePath),AUTO
Separator   STRING(1),AUTO
Pos         UNSIGNED,AUTO
NameStart   UNSIGNED,AUTO

  CODE
  FREE(DFQ)

  IF KeepDir
    SELF.SavePath = PATH()
  END
  IF SELF.DefaultDirectory
    SETPATH (SELF.DefaultDirectory)
  END

  Found = SELF.DefaultFile
  ActualFlags = BOR (SELF.Flags, FILE:Multi)
  ActualFlags = BAND(ActualFlags, 255 - FILE:Save)  !Save dialog not allowed with mutli select

  IF FILEDIALOG (SELF.WindowTitle, Found, CHOOSE(SELF.MaskString = '', Dfl:DefaultFileMask, SELF.MaskString), ActualFlags)
    Separator[1] = CHOOSE (BAND(ActualFlags,FILE:LongName) = 0, ' ', '|')
    Pos = INSTRING (Separator, Found, 1, 1)

    IF Pos                             !Multi-Selected files
?     ASSERT(Pos > 1)
      Path = Found [1 : Pos - 1]
      IF Found [Pos - 1] <> '\'
        Path = Path & '\'
      END

      LOOP
        NameStart = Pos + 1
        Pos = INSTRING (Separator, Found, 1, NameStart)
        IF Pos = 0 THEN
          Pos = LEN (Found) + 1
        END
        DFQ.Name = Path & Found [NameStart : Pos-1]
        DO GetShortName
        ADD(DFQ)
?       ASSERT(~ERRORCODE())
      WHILE Pos <= LEN(Found)
    ELSE
      DFQ.Name = Found                 !Single file only selected
      DO GetShortName
      ADD(DFQ)
?     ASSERT(~ERRORCODE())
    END
  END

  IF KeepDir
    SETPATH (SELF.SavePath)
  END
  RETURN

GetShortName ROUTINE
  DFQ.ShortName = CHOOSE(BAND(ActualFlags,FILE:LongName)=0,'',SHORTPATH(DFQ.Name))
  EXIT

! --- TranslatorClass

TranslatorClass.Construct             PROCEDURE()
  CODE
  SELF.critSect &= NewCriticalSection()
  RETURN

TranslatorClass.Destruct              PROCEDURE()
  CODE
  IF NOT SELF.Killed
     SELF.Kill()
  END
  SELF.critSect.Kill()
  RETURN

TranslatorClass.Init PROCEDURE()
Cnst      ConstantClass
critProc  CriticalProcedure
  CODE
  critProc.init(SELF.critSect)
  SELF.Queue &= NEW TranslatorQueue
  SELF.TypeMapping &= NEW TypeMappingQueue
  SELF.Killed = False
  SELF.AddTranslation(Translation)

  Cnst.Init (Term:EndGroup)
  Cnst.AddItem (ConstType:UShort, SELF.TypeMapping.ControlType)
  Cnst.AddItem (ConstType:UShort, SELF.TypeMapping.Property)
  Cnst.Set (TranslatorTypeMappings)
  Cnst.Next (SELF.TypeMapping)
  Cnst.Kill

  SORT (SELF.TypeMapping, SELF.TypeMapping.Controltype)
  RETURN

TranslatorClass.AddTranslation PROCEDURE(TranslatorGroup TG)

Cnst     ConstantClass
Text     CSTRING(MaxTLen),AUTO
Repl     CSTRING(MaxTLen),AUTO

  CODE
  Cnst.Init
  Cnst.AddItem (ConstType:PString, Text)
  Cnst.AddItem (ConstType:PString, Repl)
  Cnst.Set (TG)

  LOOP WHILE Cnst.Next() = Level:Benign
    SELF.AddTranslation (Text, Repl)
  END
  Cnst.Kill
  RETURN

TranslatorClass.Kill PROCEDURE()

ExtractFilename CSTRING(FILE:MaxFilePath),AUTO,STATIC
ExtractFile     FILE,DRIVER('ASCII'),NAME(ExtractFilename),CREATE
                  RECORD
Line                STRING(MaxTLen)
                  END
                END
c               UNSIGNED,AUTO
d               UNSIGNED,AUTO
i               UNSIGNED,AUTO
L               UNSIGNED,AUTO
Items           LONG,AUTO

critProc  CriticalProcedure
  CODE
  critProc.init(SELF.critSect)

  IF SELF.ExtractText
    ExtractFilename = SELF.ExtractText
    CREATE (ExtractFile)
?   ASSERT(~ERRORCODE())
    OPEN (ExtractFile)
?   ASSERT(~ERRORCODE())

    Items = 0
    LOOP i = 1 TO RECORDS (SELF.Queue)
      GET (SELF.Queue, i)

      IF NOT SELF.Queue.Replacement
        Items += 1
      END
    END

    SELF.ExtractText = NAME(ExtractFile)           !Fully qualifies file name
    L = LEN (SELF.ExtractText)
    d = 0
    c = L

    LOOP WHILE c <> 0
      CASE VAL (SELF.ExtractText[c])
      OF VAL ('\')
      OROF VAL (':')
        BREAK
      OF VAL ('.')
        d = c
      END

      c -= 1
    END

    IF d = 0
      d = L + 1
    END

    c += 1
?   ASSERT (c < d)

    ExtractFile.Line = SUB (SELF.ExtractText [c : d-1], 1, 8) & ' GROUP'
    DO AddLine

    ExtractFile.Line = 'Items      USHORT(' & Items & ')'
    DO AddLine

    LOOP i = 1 TO RECORDS (SELF.Queue)
      GET (SELF.Queue, i)

      IF NOT SELF.Queue.Replacement
        ExtractFile.Line = '           PSTRING(''' & CLIP(SELF.Queue.TextProp) & ''')'
        DO AddLine
        ExtractFile.Line = '           PSTRING('''')'
        DO AddLine
      END
    END

    ExtractFile.Line = '   END'
    DO AddLine

    CLOSE(ExtractFile)
?   ASSERT(~ERRORCODE())
  END

  DISPOSE (SELF.Queue)
  DISPOSE (SELF.TypeMapping)
  SELF.Killed = TRUE
  RETURN

AddLine ROUTINE
  ADD(ExtractFile)
? ASSERT(~ERRORCODE())
  EXIT


TranslatorClass.AddTranslation PROCEDURE (STRING Source, STRING Replacement)

critProc  CriticalProcedure
Err       LONG,AUTO
  CODE
  critProc.init(SELF.critSect)

? ASSERT(~SELF.Queue &= NULL)

  IF LEN(Source)
    SELF.Queue.TextProp = Source
    GET (SELF.Queue, SELF.Queue.TextProp)
    Err = ERRORCODE()

    SELF.Queue.Replacement = Replacement
    IF Err
      ADD (SELF.Queue, SELF.Queue.TextProp)
    ELSE
      PUT(SELF.Queue,SELF.Queue.TextProp)
    END
?   ASSERT(~ERRORCODE())
  END
  RETURN

TranslatorClass.TranslateWindow PROCEDURE (<WINDOW Win>)

ThisField SIGNED(0)

  CODE
  Win{PROP:Text} = SELF.TranslateString (Win{PROP:Text})

  LOOP
    ThisField = Win{PROP:NextField, ThisField}
    IF ThisField = 0
      BREAK
    END

    SELF.TranslateControl (ThisField, Win)
  END
  RETURN


TranslatorClass.TranslateControls PROCEDURE (SHORT LowCtlID, SHORT HighCtlID, <WINDOW Win>)

ThisField SIGNED(0)

  CODE
  LOOP
    ThisField = Win{PROP:NextField, ThisField}
    IF ThisField = 0
      BREAK
    END
    IF INRANGE (ThisField, LowCtlID, HighCtlID)
      SELF.TranslateControl (ThisField, Win)
    END
  END
  RETURN

TranslatorClass.TranslateControl PROCEDURE (SHORT CtlID,<WINDOW Win>)

ColCnt     UNSIGNED,AUTO
CtrlType   UNSIGNED,AUTO
BeforeText ANY
i          LONG,AUTO

critProc  CriticalProcedure
  CODE
  critProc.init(SELF.critSect)

? ASSERT(~SELF.TypeMapping &= NULL)

  CtrlType = Win$CtlId{PROP:Type}
  SORT (SELF.TypeMapping, SELF.TypeMapping.ControlType)

  SELF.TypeMapping.ControlType = CtrlType
  i = POSITION (SELF.TypeMapping)

  IF ERRORCODE() = 0
    LOOP
      GET (SELF.TypeMapping, i)
      IF SELF.TypeMapping.ControlType <> CtrlType
        BREAK
      END

      SELF.TranslateProperty (SELF.TypeMapping.Property, CtlId, Win)
      i += 1
    WHILE i <= RECORDS (SELF.TypeMapping)
  END

  CASE CtrlType
  OF   CREATE:List
  OROF CREATE:Combo
  OROF CREATE:DropList
  OROF CREATE:DropCombo
    ColCnt = 1
    LOOP UNTIL NOT Win$CtlId{PROPList:Exists, ColCnt}
      BeforeText = Win$CtlID{PROPList:Header, ColCnt}
      IF BeforeText <> ''
        Win$CtlId{PROPList:Header, ColCnt} = SELF.TranslateString(BeforeText)
      END

      BeforeText = Win$CtlID{PROPList:Header + PROPList:Group, ColCnt}
      IF BeforeText <> ''
        Win$CtlId{PROPList:Header + PROPList:Group, ColCnt} = SELF.TranslateString(BeforeText)
      END
      ColCnt += 1
    END
  END
  RETURN

TranslatorClass.TranslateProperty PROCEDURE (USHORT Property, SHORT CtrlId, <WINDOW Win>)
  CODE
  Win$CtrlId{Property} = SELF.TranslateString (Win$CtrlId{Property})
  RETURN


TranslatorClass.TranslateString PROCEDURE (STRING LookFor)

RVal     STRING(MaxTLen),AUTO
L        UNSIGNED,AUTO

  CODE
  L = SELF.Translate (LookFor, RVal, 0)
  RETURN CHOOSE (L = 0, '', RVal [1 : L])


TranslatorClass.Translate PROCEDURE (*STRING Str, *STRING RVal, UNSIGNED Level)

S        &STRING,AUTO
i1       UNSIGNED,AUTO
i2       UNSIGNED,AUTO
finger   UNSIGNED,AUTO

critProc  CriticalProcedure
  CODE
  critProc.init(SELF.critSect)

  IF  NOT Str
    i2 = CHOOSE (SIZE(RVal) < SIZE(Str), SIZE(RVal), SIZE(Str))
    LOOP i1 = 1 TO i2
      RVal [i1] = ' '
    END
    RETURN i2
  END

  IF  Level > MaxRecursion
?   ASSERT (FALSE, 'Too deep recursion')
    RETURN 0
  END

  S &= Str
  SELF.Queue.TextProp = Str
  GET (SELF.Queue, SELF.Queue.TextProp)

  IF  ERRORCODE()
    IF  SELF.ExtractText
      SELF.Queue.TextProp = Str
      SELF.Queue.Replacement = ''
      ADD (SELF.Queue, SELF.Queue.TextProp)
?     ASSERT (ERRORCODE() = 0)
    END
  ELSIF SELF.Queue.Replacement
    S &= SELF.Queue.Replacement [1 : LEN (SELF.Queue.Replacement)]
  END

  !!! Resolve macros

  IF  SIZE(RVal) = 0
    RETURN 0
  END

  finger = 0
  i1 = 0

  LOOP WHILE i1 < SIZE(S)
    i1 += 1

    IF  S[i1] = '%'
      IF  i1 < SIZE(S)
        i2 = i1
        LOOP WHILE i2 < SIZE(S)
          i2 += 1
        UNTIL S[i2] = '%'

        IF  S[i2] = '%'
          IF  i2 = i1 + 1             ! don't treat %% as a macro
            i1 = i2                   !   => replace %% with %
          ELSE
            finger += SELF.Translate (S [i1 + 1 : i2 - 1],            |
                                      RVal [finger + 1 : SIZE(RVal)], |
                                      Level + 1)
            IF  finger = SIZE(RVal)
              BREAK
            END

            i1 = i2
            CYCLE
          END
        END
      END
    END

    finger += 1
    RVal [finger] = S[i1]
  UNTIL finger = SIZE(RVal)

  RETURN finger

!-------------------------------------

CalendarClass.Ask                   PROCEDURE(STRING pTitle,LONG pDate=0)
 INCLUDE('ABUTILUI.INC', 'CalendarClassWindow')
 CODE
   OPEN(Screen)
   SELF.CtrlSun       = ?Sun
   SELF.CtrlExit      = ?Exit
   SELF.CtrlToday     = ?Today
   SELF.CtrlDay1      = ?Day1
   SELF.CtrlDay42     = ?Day42
   SELF.CtrlPrevMonth = ?PrevMonth
   SELF.CtrlNextMonth = ?NextMonth
   SELF.CtrlPrevYear  = ?PrevYear
   SELF.CtrlNextYear  = ?NextYear
   SELF.CtrlMore7     = ?More7
   SELF.CtrlLess7     = ?Less7
   SELF.CtrlMore15    = 0
   SELF.CtrlLess15    = 0
   SELF.CtrlCurrTime  = 0
   SELF.CtrlMonthYear = ?MonthYear
   SELF.CtrlTheDate   = 0
   SELF.CtrlTitleBG   = ?BoxTitleBackground
   SELF.CtrlBodyBG    = ?BoxBodyBackground
   RETURN PARENT.Ask(pTitle,pDate)

CalendarClass.SetUp                 PROCEDURE()
 CODE
 RETURN

!-------------------------------------
CalendarSmallClass.Ask                   PROCEDURE(STRING pTitle,LONG pDate=0)
 INCLUDE('ABUTILUI.INC', 'CalendarSmallClassWindow')
 CODE
   OPEN(Screen)
   SELF.CtrlSun         = ?Sun
   SELF.CtrlExit        = ?Exit
   SELF.CtrlToday       = ?Today
   SELF.CtrlDay1        = ?Day1
   SELF.CtrlDay42       = ?Day42
   SELF.CtrlPrevMonth   = ?PrevMonth
   SELF.CtrlNextMonth   = ?NextMonth
   SELF.CtrlPrevYear    = ?PrevYear
   SELF.CtrlNextYear    = ?NextYear
   SELF.CtrlMore7       = ?More7
   SELF.CtrlLess7       = ?Less7
   SELF.CtrlMore15      = ?More15
   SELF.CtrlLess15      = ?Less15
   SELF.CtrlCurrTime    = ?CurrTime
   SELF.CtrlMonthYear   = ?MonthYear
   SELF.CtrlTheDate     = ?TheDate
   SELF.CtrlTitleBG     = 0
   SELF.CtrlBodyBG      = ?BoxBodyBackground
   RETURN PARENT.Ask(pTitle,pDate)

CalendarSmallClass.SetUp                 PROCEDURE()
 CODE
!-------------------------------------
CalendarSmallTouchClass.Ask              PROCEDURE(STRING pTitle,LONG pDate=0)
 INCLUDE('ABUTILUI.INC', 'CalendarSmallTouchClassWindow')
 CODE
   OPEN(Screen)
   SELF.CtrlToday       = ?Today
   SELF.CtrlExit        = ?Exit
   SELF.CtrlCancel      = ?Cancel
   SELF.CtrlPrevMonth   = ?PrevMonth
   SELF.CtrlNextMonth   = ?NextMonth
   SELF.CtrlPrevYear    = ?PrevYear
   SELF.CtrlNextYear    = ?NextYear
   SELF.CtrlMore1       = ?More1
   SELF.CtrlLess1       = ?Less1
   SELF.CtrlYearText    = ?YearText
   SELF.CtrlMonthText   = ?MonthText
   SELF.CtrlDayText     = ?DayText
   SELF.CtrlTheDate     = ?TheDate
   SELF.CtrlTitleBG     = 0
   SELF.CtrlBodyBG      = ?BoxBodyBackgroundMid
   SELF.CtrlDOWText     = ?TheDateDOW
   SELF.SelectOnClose   = true
   RETURN PARENT.Ask(pTitle,pDate)

CalendarSmallTouchClass.SetUp           PROCEDURE()
 CODE
!-------------------------------------
CalendarSmallTouchIconsClass.Ask              PROCEDURE(STRING pTitle,LONG pDate=0)
 INCLUDE('ABUTILUI.INC', 'CalendarSmallTouchIconsClassWindow')
 CODE
   OPEN(Screen)
   SELF.CtrlToday       = ?Today
   SELF.CtrlExit        = ?Exit
   SELF.CtrlCancel      = ?Cancel
   SELF.CtrlPrevMonth   = ?PrevMonth
   SELF.CtrlNextMonth   = ?NextMonth
   SELF.CtrlPrevYear    = ?PrevYear
   SELF.CtrlNextYear    = ?NextYear
   SELF.CtrlMore1       = ?More1
   SELF.CtrlLess1       = ?Less1
   SELF.CtrlYearText    = ?YearText
   SELF.CtrlMonthText   = ?MonthText
   SELF.CtrlDayText     = ?DayText
   SELF.CtrlTheDate     = ?TheDate
   SELF.CtrlTitleBG     = 0
   SELF.CtrlBodyBG      = ?BoxBodyBackgroundMid
   SELF.CtrlDOWText     = ?TheDateDOW
   SELF.SelectOnClose   = true

   SELF.CtrlPrevMonth{PROP:NoFrame} = true
   SELF.CtrlNextMonth{PROP:NoFrame} = true
   SELF.CtrlPrevYear{PROP:NoFrame} = true
   SELF.CtrlNextYear{PROP:NoFrame} = true
   SELF.CtrlMore1{PROP:NoFrame} = true
   SELF.CtrlLess1{PROP:NoFrame} = true

   SELF.CtrlToday{PROP:NoFrame} = true
   SELF.CtrlExit{PROP:NoFrame} = true
   SELF.CtrlCancel{PROP:NoFrame} = true

   RETURN PARENT.Ask(pTitle,pDate)
!-------------------------------------
CalendarSmallTouchIconsClass.SetUp           PROCEDURE()
 CODE
 SELECT(SELF.CtrlMore1)
!-------------------------------------
CalendarSmallTouchRegionsClass.Ask              PROCEDURE(STRING pTitle,LONG pDate=0)
 INCLUDE('ABUTILUI.INC', 'CalendarSmallTouchRegionsClassWindow')
 CODE
   OPEN(Screen)
   SELF.CtrlToday       = ?Today
   SELF.CtrlExit        = ?Exit
   SELF.CtrlCancel      = ?Cancel
   SELF.CtrlPrevMonth   = ?PrevMonthPanel
   SELF.CtrlNextMonth   = ?NextMonthPanel
   SELF.CtrlPrevYear    = ?PrevYearPanel
   SELF.CtrlNextYear    = ?NextYearPanel
   SELF.CtrlMore1       = ?More1Panel
   SELF.CtrlLess1       = ?Less1Panel
   SELF.CtrlYearText    = ?YearText:2
   SELF.CtrlMonthText   = ?MonthText:2
   SELF.CtrlDayText     = ?DayText:2
   SELF.CtrlTheDate     = ?TheDate
   SELF.CtrlTitleBG     = 0
   SELF.CtrlBodyBG      = ?BoxBodyBackgroundMid
   SELF.CtrlDOWText     = ?TheDateDOW
   SELF.SelectOnClose   = true

   SELF.CtrlToday{PROP:NoFrame} = true
   SELF.CtrlExit{PROP:NoFrame} = true
   SELF.CtrlCancel{PROP:NoFrame} = true

   ?BOXMonthTop1{PROP:GradientType}      = GradientTypes:Vertical
   ?BOXMonthTop1{PROP:GradientFromColor} = 00F5F5F5h
   ?BOXMonthTop1{PROP:GradientToColor}   = 00D3D3D3h!COLOR:Silver

   ?BOXMonthMid1{PROP:GradientType}      = GradientTypes:Vertical!Cylinder
   ?BOXMonthMid1{PROP:GradientFromColor} = 00D3D3D3h!00D3D3D3h!
   ?BOXMonthMid1{PROP:GradientToColor}   = 00F5F5F5h!00CFCFCFh!COLOR:Silver!00A9A9A9h


   ?BOXMonthTop2{PROP:GradientType}      = ?BOXMonthTop1{PROP:GradientType}
   ?BOXMonthTop2{PROP:GradientFromColor} = ?BOXMonthTop1{PROP:GradientFromColor}
   ?BOXMonthTop2{PROP:GradientToColor}   = ?BOXMonthTop1{PROP:GradientToColor}

   ?BOXMonthMid2{PROP:GradientType}      = ?BOXMonthMid1{PROP:GradientType}
   ?BOXMonthMid2{PROP:GradientFromColor} = ?BOXMonthMid1{PROP:GradientFromColor}
   ?BOXMonthMid2{PROP:GradientToColor}   = ?BOXMonthMid1{PROP:GradientToColor}


   ?BOXMonthTop3{PROP:GradientType}      = ?BOXMonthTop1{PROP:GradientType}
   ?BOXMonthTop3{PROP:GradientFromColor} = ?BOXMonthTop1{PROP:GradientFromColor}
   ?BOXMonthTop3{PROP:GradientToColor}   = ?BOXMonthTop1{PROP:GradientToColor}

   ?BOXMonthMid3{PROP:GradientType}      = ?BOXMonthMid1{PROP:GradientType}
   ?BOXMonthMid3{PROP:GradientFromColor} = ?BOXMonthMid1{PROP:GradientFromColor}
   ?BOXMonthMid3{PROP:GradientToColor}   = ?BOXMonthMid1{PROP:GradientToColor}

   RETURN PARENT.Ask(pTitle,pDate)
!-------------------------------------
CalendarSmallTouchRegionsClass.SetUp           PROCEDURE()
 CODE
!-------------------------------------
CalendarBaseClass.SetColor              PROCEDURE(LONG pColorSaturday,LONG pColorSunday,LONG pColorOther,LONG pColorHoliday)
 CODE
    SELF.ColorSaturday = pColorSaturday
    SELF.ColorSunday   = pColorSunday
    SELF.ColorOther    = pColorOther
    SELF.ColorHoliday  = pColorHoliday

CalendarBaseClass.IsHoliday             PROCEDURE(BYTE pDay,BYTE pMonth,SHORT pYear,BYTE pWeekDay)
 CODE
    RETURN False

!-------------------------------------
CalendarBaseClass.Ask                   PROCEDURE(STRING pTitle,LONG pDate=0)
WindowInitialized    BYTE
DayStr               STRING(3)
CurrTime             LONG
MonthYear            STRING(15)
The_date             LONG
the_last_date        LONG
the_month            SHORT
the_day              SHORT
the_year             SHORT
base_col             SHORT
last_day             SHORT
base_date            LONG
cur_mo               LONG
cur_yr               LONG
mo_and_yr            LONG
start_date           LONG
sunday_date          LONG
butnum               SHORT
FirstButNum          SHORT
daynum               SHORT
 INCLUDE('ABUTILUI.INC', 'CalendarDayGroup')
SatCol               BYTE
SunCol               BYTE
 INCLUDE('ABUTILUI.INC', 'CalendarMonthGroup')
loc:fecha            LONG
lHIni                LONG
lHEnd                LONG
idx                  LONG
  CODE
  PUSHBIND

  IF SELF.CtrlCurrTime THEN
     SELF.CtrlCurrTime{PROP:USE}=CurrTime
  END
  IF SELF.CtrlMonthYear THEN
     SELF.CtrlMonthYear{PROP:USE}=MonthYear
  END
  IF SELF.CtrlTheDate THEN
     SELF.CtrlTheDate{PROP:USE}=The_Date
  END
  SELF.FirstWeekDay = 7
  SELF.SetUp()
  IF NOT(SELF.FirstWeekDay>=1 AND SELF.FirstWeekDay<=7) THEN
     SELF.FirstWeekDay = 7
  END
  IF SELF.CtrlSun
     Do DoReorderDays
  END
  IF SELF.CtrlDay1>0
     FirstButNum = SELF.CtrlDay1 + (7-SELF.FirstWeekDay)
  END
  SELF.SelectedDate=0
  SELF.Response = RequestCancelled
  IF pDate=0 THEN
     start_date =  TODAY()
  ELSE
     start_date = pDate
  END
  IF ~INRANGE(start_date,DATE(1,1,1801),DATE(12,31,2099))
     start_date = Today()
  END
  the_date = start_date
  mo_and_yr = 0
  cur_mo    = 0
  cur_yr    = 0
  sunday_date = DATE(1,3,1993) ! A known Sunday
  loc:fecha = the_date
  the_last_date= the_date
  IF KEYCODE() = MouseRight
    SETKEYCODE(0)
  END
  IF SELF.XPos THEN
     IF SELF.RightAlignment THEN
        0{PROP:XPOS}=SELF.XPos-0{PROP:Width}
     ELSE
        0{PROP:XPOS}=SELF.XPos
     END
  END
  IF SELF.YPos THEN
     0{PROP:YPOS}=SELF.YPos
  END
  DO PrepareProcedure
  lHIni=0{PROP:HEIGHT}
  IF CLIP(pTitle) THEN
     0{prop:text}=pTitle
  ELSE
     0{prop:text}=''
  END
  DISPLAY()
  lHEnd=0{PROP:HEIGHT}
  IF lHIni<>lHEnd THEN
     0{PROP:HEIGHT}=0{PROP:HEIGHT}+(lHIni-lHEnd)
  END
  SELF.Response = RequestCancelled
  ACCEPT
    CASE EVENT()
    OF EVENT:AlertKey
       CASE KEYCODE()
         OF UpKey
               Do PrevWeek
               SELECT(SELF.CtrlExit)
         OF DownKey
               Do NextWeek
               SELECT(SELF.CtrlExit)
         OF LeftKey
               Do PrevDay
               SELECT(SELF.CtrlExit)
         OF RightKey
               Do NextDay
               SELECT(SELF.CtrlExit)
         OF EnterKey
                 IF FOCUS()<> SELF.CtrlExit
                    PRESSKEY(TabKey)
                    CYCLE
                 ELSE
                    ! Select the OK button
                    POST(Event:Accepted,SELF.CtrlExit)
                    CYCLE
                 END!IF
       END!CASE
    OF EVENT:OpenWindow
      IF NOT WindowInitialized
         DISPLAY()
         WindowInitialized = True
      END
      SELECT(SELF.CtrlExit)
    OF EVENT:GainFocus
      IF NOT WindowInitialized
         WindowInitialized = True
      END
      DISPLAY()
    OF EVENT:Timer
      Do ShowTime
    OF Event:Rejected
      BEEP
      DISPLAY(?)
      SELECT(?)
    END
    IF FIELD()>0 THEN
       CASE FIELD()
       OF SELF.CtrlDay1 TO SELF.CtrlDay42
         CASE EVENT()
         OF EVENT:Accepted
         ! Set numeric date:
         !   Month and year are known
         !   Day is current button minus button for first day of month
         !     minus the first day of month plus one
         the_date = DATE(the_month,?-SELF.CtrlDay1-base_col+1,the_year)
         DO DoSelect
         DO DoUpdateScreen
         the_last_date= the_date
         SELF.Response = RequestCompleted
         POST(Event:Accepted,SELF.CtrlExit)
         CYCLE
         OF EVENT:Selected
         the_date = DATE(the_month,?-SELF.CtrlDay1-base_col+1,the_year)
         DO DoSelect
         DO DoUpdateScreen
         the_last_date = the_date
         END
       OF SELF.CtrlToday
          CASE EVENT()
          OF EVENT:Accepted
             DO DoToday
             DO DoSelect
             DO DoUpdateScreen
          END
       OF SELF.CtrlPrevMonth
         CASE EVENT()
         OF EVENT:Accepted
           Do DoPrevMonth
         END
       OF SELF.CtrlNextMonth
         CASE EVENT()
         OF EVENT:Accepted
           Do DoNextMonth
         END
       OF SELF.CtrlPrevYear
         CASE EVENT()
         OF EVENT:Accepted
           Do DoPrevYear
         END
       OF SELF.CtrlNextYear
         CASE EVENT()
         OF EVENT:Accepted
           Do DoNextYear
         END
       OF SELF.CtrlLess1
         CASE EVENT()
         OF EVENT:Accepted
           Do PrevDay
         END
       OF SELF.CtrlMore1
         CASE EVENT()
         OF EVENT:Accepted
           Do NextDay
         END
       OF SELF.CtrlLess7
         CASE EVENT()
         OF EVENT:Accepted
           Do PrevWeek
         END
       OF SELF.CtrlMore7
         CASE EVENT()
         OF EVENT:Accepted
           Do NextWeek
         END
       OF SELF.CtrlLess15
         CASE EVENT()
         OF EVENT:Accepted
           Do Prev2Week
         END
       OF SELF.CtrlMore15
         CASE EVENT()
         OF EVENT:Accepted
           Do Next2Week
         END
       OF SELF.CtrlCancel
         CASE EVENT()
         OF EVENT:Accepted
            SELF.Response = RequestCancelled
            POST(Event:CloseWindow)
         END
       OF SELF.CtrlExit
         CASE EVENT()
         OF EVENT:Accepted
            IF SELF.SelectOnClose THEN
               SELF.Response = RequestCompleted
            END
            POST(Event:CloseWindow)
         END
       END
    END
  END
  IF SELF.Response = RequestCancelled THEN
     loc:fecha = start_date
  END
  SELF.SelectedDate = loc:fecha
  SELF.XPos=0
  SELF.YPos=0
  POPBIND
  RETURN(loc:fecha)
!---------------------------------------------------------------------------
PrepareProcedure ROUTINE
  ALERT(EnterKey)
  ALERT(DownKey)
  ALERT(RightKey)
  ALERT(UpKey)
  ALERT(LeftKey)
  DO ShowTime
  DO DoUpdateScreen

!---------------------------------------------------------------------------
ProcedureReturn ROUTINE
!---------------------------------------------------------------------------
DoReorderDays   ROUTINE
  IF SELF.CtrlSun>0
     base_col=SELF.FirstWeekDay
     LOOP butnum=0 TO 6
        (SELF.CtrlSun+butnum){PROP:TEXT}=ClarionDayOfTheWeekShort[base_col]
        DISPLAY(SELF.CtrlSun+butnum)
        IF base_col=6 THEN SatCol=butnum+1.
        IF base_col=7 THEN SunCol=butnum+1.
        base_col+=1
        IF base_col>7 THEN base_col=1.
     END
  END

DoUpdateSceenCtrlText ROUTINE
  IF SELF.CtrlMonthText
     SELF.CtrlMonthText{PROP:TEXT}=CLIP(FORMAT(MONTH(the_date),@P##P))
  END
  IF SELF.CtrlYearText
     SELF.CtrlYearText{PROP:TEXT}=YEAR(the_date)
  END
  IF SELF.CtrlDayText
     SELF.CtrlDayText{PROP:TEXT}=CLIP(FORMAT(DAY(the_date),@P##P))
  END
  IF SELF.CtrlDOWText
     SELF.CtrlDOWText{PROP:TEXT}=ClarionDayOfTheWeekLong[(the_date % 7)]
     DISPLAY(SELF.CtrlDOWText)
  END

DoUpdateScreen          ROUTINE
  the_month = MONTH(the_date)
  the_day = DAY(the_date)
  the_year = YEAR(the_date)
  DO DoUpdateSceenCtrlText
  IF SELF.CtrlDay1=0  THEN EXIT.
  ! Need to show new month?
  IF (cur_mo=the_month AND cur_yr=the_year) THEN
     IF the_last_date= the_date THEN EXIT.
        daynum=DAY(the_last_date)
        butnum=daynum-1+base_col+SELF.CtrlDay1
        butnum{PROP:TEXT}=CLIP(FORMAT(daynum,@P##P))
        butnum{PROP:FontStyle}=FONT:regular
        daynum=DAY(the_date)
        butnum=daynum-1+base_col+SELF.CtrlDay1
        butnum{PROP:FontStyle}=FONT:bold+FONT:underline !today's date
     EXIT
  END
  MonthYear = CLIP(ClarionMonthListLong[the_month]) & ' ' & the_year
  base_date = DATE(the_month, 1, the_year)
  base_col = ABS((base_date - (sunday_date-(7-SELF.FirstWeekDay) )) % 7)  ! Column for first day
  last_day = DAY(DATE(the_month + 1, 1, the_year) -1) ! Last day of month
  LOOP butnum = SELF.CtrlDay1 TO SELF.CtrlDay42 ! Label buttons
    daynum = butnum - SELF.CtrlDay1 - base_col + 1
    DO DayToStr
    IF LEN(CLIP(daystr)) > 0
      UNHIDE(butnum)
      butnum{PROP:TEXT}=clip(FORMAT(daynum,@P##P))
      IF butnum=SELF.CtrlDay1+(SunCol-1) or butnum=SELF.CtrlDay1+7+(SunCol-1) or butnum=SELF.CtrlDay1+14+(SunCol-1) or butnum=SELF.CtrlDay1+21+(SunCol-1) or butnum=SELF.CtrlDay1+28+(SunCol-1) or butnum=SELF.CtrlDay1+35+(SunCol-1) THEN
        butnum{PROP:FontColor}=SELF.ColorSunday
        butnum{PROP:FontStyle}=FONT:regular
      ELSE
        IF butnum=SELF.CtrlDay1+(SatCol-1) or butnum=SELF.CtrlDay1+7+(SatCol-1) or butnum=SELF.CtrlDay1+14+(SatCol-1) or butnum=SELF.CtrlDay1+21+(SatCol-1) or butnum=SELF.CtrlDay1+28+(SatCol-1) or butnum=SELF.CtrlDay1+35+(SatCol-1) THEN
          butnum{PROP:FontColor}=SELF.ColorSaturday
          butnum{PROP:FontStyle}=FONT:regular
        ELSE
          butnum{PROP:FontColor}=SELF.ColorOther
          butnum{PROP:FontStyle}=FONT:regular
        END
      END
      IF SELF.IsHoliday(daystr,the_month,the_year,daynum) THEN
         butnum{PROP:FontColor}=SELF.ColorHoliday
         butnum{PROP:FontStyle}=FONT:regular
      END
      IF DATE(the_month, daystr, the_year) = the_date
        butnum{PROP:FontStyle}=FONT:bold+FONT:underline !today's date
      END
     ELSE
      HIDE(butnum)
    END
  END
  cur_mo = the_month
  cur_yr = the_year
  mo_and_yr = the_month + the_year ! Currently showing this month/year
!---------------------------------------------------------------------------
! Show the current time
ShowTime        ROUTINE
  CurrTime = CLOCK()
!---------------------------------------------------------------------------
NextDay         ROUTINE
  the_date = the_date + 1
  DO DoSelect
  Do DoUpdateScreen
  the_last_date= the_date
!---------------------------------------------------------------------------
PrevDay         ROUTINE
  the_date = the_date - 1
  DO DoSelect
  Do DoUpdateScreen
  the_last_date= the_date
!---------------------------------------------------------------------------
PrevWeek        ROUTINE
  the_date = the_date - 7
  DO DoSelect
  Do DoUpdateScreen
  the_last_date= the_date
!---------------------------------------------------------------------------
NextWeek        ROUTINE
  the_date = the_date + 7
  DO DoSelect
  Do DoUpdateScreen
  the_last_date= the_date
!---------------------------------------------------------------------------
Prev2Week        ROUTINE
  the_date = the_date - 15
  DO DoSelect
  Do DoUpdateScreen
  the_last_date= the_date
!---------------------------------------------------------------------------
Next2Week        ROUTINE
  the_date = the_date + 15
  DO DoSelect
  Do DoUpdateScreen
  the_last_date= the_date
!---------------------------------------------------------------------------
! Show next month
DoNextMonth     ROUTINE
  the_month += 1
  DO FixupMonth
  DO DoSelect
  Do DoUpdateScreen
  the_last_date= the_date
!---------------------------------------------------------------------------
! Show last month
DoPrevMonth     ROUTINE
  the_month -= 1
  DO FixupMonth
  DO DoSelect
  Do DoUpdateScreen
  the_last_date= the_date
!---------------------------------------------------------------------------
! Show same month, next year
DoNextYear      ROUTINE
  the_year += 1
  DO FixupMonth
  DO DoSelect
  Do DoUpdateScreen
  the_last_date= the_date
!---------------------------------------------------------------------------
! Show same month, last year
DoPrevYear      ROUTINE
  the_year -= 1
  DO FixupMonth
  DO DoSelect
  Do DoUpdateScreen
  the_last_date= the_date
!---------------------------------------------------------------------------
! Pressed a button to select a date
DoSelect        ROUTINE
  IF the_date > 0
    loc:fecha = the_date
  END
!---------------------------------------------------------------------------
! Show current month/year
DoToday         ROUTINE
  the_date = Today()
  DO DoSelect
  Do DoUpdateScreen
  the_last_date = the_date
!---------------------------------------------------------------------------
! Fix month number up
FixupMonth      ROUTINE
  IF the_month < 1
     the_year -= 1
     the_month = 12
  ELSIF the_month > 12
     the_year += 1
     the_month = 1
  END
  the_date = DATE(the_month, the_day, the_year)
  IF MONTH(the_date)<>the_month
     the_date = DATE(the_month, 1, the_year)
  END
  the_last_date= the_date
!---------------------------------------------------------------------------
! Exit calendar
DayToStr        ROUTINE
  IF daynum > last_day THEN daynum = 0. ! Blank button
  IF daynum <= 0
    daystr = ''
  ELSE
    daystr = FORMAT(daynum,@n2)
  END

CalendarBaseClass.SetUp                 PROCEDURE()
 CODE

CalendarBaseClass.SetPosition           PROCEDURE(UNSIGNED pParentControl)
px LONG
py LONG
pcx LONG
pcy LONG
 CODE
 px = 0{PROP:Xpos}
 py = 0{PROP:Ypos}
 pcx = pParentControl{PROP:Xpos} + 1
 pcy = pParentControl{PROP:Ypos} + 1 + (pParentControl{PROP:Height}+0)*2
 SELF.SetPosition(pcx + px, pcy + py)

CalendarBaseClass.SetPosition           PROCEDURE(LONG pXPos,LONG pYPos)
 CODE
   SELF.XPos=pXPos
   SELF.YPos=pYPos
!---------------------------------------------------------------------------
!---------------------------------------------------------------------------
!===  DynaStringClass  ========
DynaStringClass.Construct PROCEDURE()
  CODE
  SELF.cs &= NEW CSTRING(1)

DynaStringClass.Destruct PROCEDURE()
  CODE
  DISPOSE(SELF.cs)

DynaStringClass.Str PROCEDURE(STRING s)
  CODE
  DISPOSE(self.cs)
  SELF.cs &= NEW CSTRING(LEN(s) + 1)
  SELF.cs = s
  RETURN SELF.cs

DynaStringClass.Str PROCEDURE()
  CODE
  RETURN SELF.cs

DynaStringClass.Append PROCEDURE(STRING s)
  CODE
  SELF.Str(SELF.Str() & s)
