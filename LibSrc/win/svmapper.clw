
  member

!**********************************************************************
!
!   Module:         svmapper.clw
!
!
!**********************************************************************

!*
!*  Includes
!*

  include('svapi.inc'),once
  include('svmapper.inc'),once

!*
!*  Equates
!*

!*
!*  Declarations
!*

!*
!*  Function Prototypes
!*

  map
    include('svapifnc.inc')
  end


TableMapper.Construct procedure

  code
    self.GrpInfo &= new GroupFields
    self.FieldsInfo &= new(TFieldsInfo)
    SELF.ADOFieldsQ &= new(TADOFieldsQ)
!    if (~self.GrpInfo &= null)
!      self.bInitialised = true.


TableMapper.Destruct procedure

  code
    if (~self.GrpInfo &= null)
      dispose(self.GrpInfo)
    end
    if (~self.FieldsInfo &= null)
      dispose(self.FieldsInfo)
    end

    if ~(SELF.ADOFieldsQ &= null)
      SELF.ClearADOObjects()
      dispose(SELF.ADOFieldsQ)
    end


TableMapper.MapRsToGroup procedure(*CRecordSet SourceRs, *Group TargetGrp, byte bCwStdDateTime)

bResult         bool,auto

  code
!    if self.bInitialised
      bResult = true
?     assert(~SourceRs &= null)
      if (~SourceRs &= null)
        self.bCwStdDateTime = bCwStdDateTime
        if self.SetupGroupFieldQueue(TargetGrp)
          bResult = self.AssignFields(SourceRs).
      end
!    end

!TC Added 11/14
TableMapper.MapRsToGroup procedure(*CRecordSet SourceRs, *QUEUE TargetQ, byte bCwStdDateTime)

bResult         bool,auto

  code
!    if self.bInitialised
      bResult = true
?     assert(~SourceRs &= null)
      if (~SourceRs &= null)
        self.bCwStdDateTime = bCwStdDateTime
        if self.SetupGroupFieldQueue(TargetQ)
          bResult = self.AssignFields(SourceRs).
      end
!    end
!TC Added 11/14

TableMapper.MapRsToGroup procedure(*CRecordSet SourceRs, byte bCwStdDateTime)

bResult         bool,auto

  code
!    if self.bInitialised
      bResult = true
?     assert(~SourceRs &= null)
      if (~SourceRs &= null)
        self.bCwStdDateTime = bCwStdDateTime
        if records(self.GrpInfo)
          bResult = self.AssignFields(SourceRs).
      end
!    end


TableMapper.MapGroupToRs procedure(*CRecordSet TargetRs, *Group SourceGrp, byte bCwStdDateTime)

bResult         bool,auto

  code
!    if self.bInitialised
      bResult = true
?     assert(~TargetRs &= null)
      if (~TargetRs &= null)
        self.bCwStdDateTime = bCwStdDateTime
        if self.SetupGroupFieldQueue(SourceGrp)

        end
      end
!    end


TableMapper.MapGroupToRs procedure(*CRecordSet TargetRs, byte bCwStdDateTime)

bResult         bool,auto

  code
!    if self.bInitialised
      bResult = true
?     assert(~TargetRs &= null)
      if (~TargetRs &= null)
        self.bCwStdDateTime = bCwStdDateTime
        if records(self.GrpInfo)

        end
      end
!    end


TableMapper.SetupGroupFieldQueue procedure(*Group Grp)

bResult         bool
FieldName       cstring(64)
szFieldName     &cstring
i               long

  code
!    if self.bInitialised
      if records(self.GrpInfo)
        free(self.GrpInfo).
      i = 1
      self.Grp &= Grp
      loop
        FieldName = ''
        FieldName = who(Grp, i)
        if len(FieldName)
          szFieldName &= self.StripPrefix(FieldName)
          self.GrpInfo.FieldName = szFieldName
          self.GrpInfo.FieldPos = i
          add(self.GrpInfo)

          szFieldName &= NULL

          if ~errorcode()
            bResult = true
          else
            free(self.GrpInfo)
            bResult = false
            break
          end
        else
          break
        end
        i += 1
      end
!    end
    return bResult

!TC Added 11/14
TableMapper.SetupGroupFieldQueue procedure(*QUEUE Q)

bResult         bool
FieldName       cstring(64)
szFieldName     &cstring
i               long

  code
!    if self.bInitialised
      if records(self.GrpInfo)
        free(self.GrpInfo).
      i = 1
      self.Q &= Q
      loop
        FieldName = ''
        FieldName = who(Q, i)
        if len(FieldName)
          szFieldName &= self.StripPrefix(FieldName)
          self.GrpInfo.FieldName = szFieldName
          self.GrpInfo.FieldPos = i
          add(self.GrpInfo)
          if ~errorcode()
            bResult = true
          else
            free(self.GrpInfo)
            bResult = false
            break
          end
        else
          break
        end
        i += 1
      end
!    end
    return bResult
!TC Added 11/14


TableMapper.AssignFields procedure(*CRecordSet Rs, byte bTarget)

bResult         bool
Flds            &CFields
Fld             &CField
hr              HRESULT
dwCount         long
iCol            long
lType           LONG

  code
    Flds &= Rs.GetFields(hr)
    if hr = S_OK
      hr = Flds.GetCount(dwCount)
      if hr = S_OK
        loop iCol = 0 to dwCount - 1
          Fld &= Flds.GetField(iCol, hr)
          hr = Fld.GetType( lType )
          if hr = S_OK
            if bTarget
              bResult = self.AssignField(Fld, true)
            else
              bResult = self.AssignField(Fld)
            end
            dispose(Fld)
          end
        end
      end
      dispose(Flds)
    end
    return bResult


TableMapper.AssignField procedure(*CField Fld, byte bTarget)

bResult         bool
strName         &CStr
szName          &cstring
szNameUpr       &cstring
hr              HRESULT


!gValue          group(gVariant),OVER(aVar).
gValue          LIKE(gVariant)
AVar            Variant,OVER(gValue)
vtValue         group(tVariant), over(gValue)
                end

vtValuer        &tVariantr
aFld            any
szRetVal        &cstring
DateGroup       group(DateGroupType).
DateTime        group
Date              date
Time              time
                end
Money           group(TCURRENCY).
dwBytes         long
Dec             like(_DECIMAL)
lType           LONG
!n               GROUP(_NUMERIC).
n_info          LONG

  code
    if bTarget
    else
      hr = Fld.GetType( lType )
      strName &= Fld.GetName(hr)

      if hr = S_OK
        szName &= strName.GetCStr()
        self.GrpInfo.FieldName = upper(szName)
        get(self.GrpInfo, self.GrpInfo.FieldName)
        if ~errorcode()
          hr = Fld.GetValue(gValue)
          if hr = S_OK
            aFld &= null
            IF ~SELF.Grp &= NULL
              aFld &= what(self.Grp, self.GrpInfo.FieldPos)
            ELSE
              aFld &= WHAT(SELF.Q, SELF.GrpInfo.FieldPos)
            END
            if ~aFld &= null
              if IsString(aFld)
                aFld = ''
                if vtValue.vt = VT_DATE or vtValue.vt = VT_CY
                  vtValuer &= vtValue
                  if vtValue.vt = VT_DATE
                    self.GetDateValue(vtValuer, DateGroup, hr)
                    if self.bCwStdDateTime
                      aFld = DateGroup
                    else
                      DateTime.Date = DateGroup.Date
                      DateTime.Time = DateGroup.Time
                      aFld = DateTime
                    end
                  else
                    Money.lo = vtValue.iVal
                    Money.hi = vtValue.iVal2
                    aFld = Money
                  end
                elsif vtValue.vt = VT_DECIMAL
                  MoveMemory(address(Dec), address(vtValue), size(_DECIMAL))
                  aFld = Dec
                else
                  aFld = aVar
                end
              else
                aFld = 0
                if vtValue.vt = VT_DATE
                  vtValuer &= vtValue
                  self.GetDateValue(vtValuer, DateGroup, hr)
                  aFld = DateGroup.Date
                elsif vtValue.vt = VT_DECIMAL
                  aFld = aVar
                elsif vtValue.vt = VT_BOOL
                  aFld = aVar
                else
                  hr = self.GetNumericSize(vtValue, dwBytes)
                  if hr = S_OK
                    if dwBytes = 8
                      !vtValuer &= address(vtValue)
                      !aFld = vtValuer.rVal
                      aFld = aVar
                    else
                      if vtValue.vt = VT_R4
                        aFld = vtValue.srVal
                      else
                        aFld = aVar !vtValue.iVal
                      end
                    end
                  end
                end
              end
              if hr = S_OK
                bResult = true.
            end
            aFld &= null
            hr = VariantClear(vtValue)
          end
        end
      end
      strName.Release()
      DISPOSE( strName )
    end
    !TC Added 11/14
    IF ~aFld &= NULL
      aFld &= NULL
    END
    !TC Added 11/14
    return bResult


TableMapper.GetDateValue procedure(*tVariantr vtValuer, *DateGroupType DateGroup, *HRESULT hr)

cwDate          long
cwTime          long
DateConverter   CDateConverter

  code
    if DateConverter.SQLDateTimeToCW(vtValuer.rVal, cwDate, cwTime)
      DateGroup.Date = cwDate
      DateGroup.Time = cwTime
      hr = S_OK
    end


TableMapper.GetNumericSize procedure(*tVariant vtValue, *long dwBytes)

hr          long

  code
    hr = S_FALSE
    dwBytes = 0
    case vtValue.vt
      of VT_I2
      orof VT_I4
      orof VT_I1
      orof VT_UI1
      orof VT_UI2
      orof VT_UI4
      orof VT_INT
      orof VT_UINT
      orof VT_HRESULT
      orof VT_R4
        hr = S_OK
        dwBytes = 4
      of VT_I8
      orof VT_UI8
      orof VT_R8
      orof VT_CY
        hr = S_OK
        dwBytes = 8
      of VT_DECIMAL
        hr = S_OK
        dwBytes = 32
    end
    return hr


TableMapper.GetStringValue procedure(*tVariant vtValue, *HRESULT hr)

szValue         &cstring
cstrVal         CStr
szTemp          &cstring

  code
    hr = S_FALSE
    case vtValue.vt
      of VT_BSTR
        assert(cstrVal.Init(vtValue.iVal))
        szTemp &= cstrVal.GetCStr()
        szValue &= new cstring(len(szTemp) + 1)
        szValue = szTemp
        szTemp &= NULL
        hr = S_OK
      of VT_NULL
        szValue &= new cstring(1)
        szValue = ''
        hr = S_OK
    end
    return szValue


TableMapper.StripPrefix procedure(*cstring szFieldName)

nPos            long,auto

  code
    nPos = instring(':', szFieldName, 1, 1)
    if nPos
      szFieldName = szFieldName[nPos + 1 : len(szFieldName)]
    end
    return szFieldName


TableMapper.AddFieldsInfo    procedure(string pTableName, string pColName, *? pTargetVar, short pDateTimeID)

  code
    clear(self.FieldsInfo)
    self.FieldsInfo.BaseTable = clip(upper(pTableName))
    self.FieldsInfo.ColName = clip(upper(pColName))
    self.FieldsInfo.ColNameSQL = clip(pColName)      ! Some backend are case sensitive with colname
    self.FieldsInfo.TargetField &= null
    self.FieldsInfo.TargetField &= pTargetVar
    self.FieldsInfo.DateTimeID = pDateTimeID
    add(self.FieldsInfo, self.FieldsInfo.BaseTable, self.FieldsInfo.ColName)


TableMapper.AddFieldsInfo    procedure(string pTableName, *group pFileRecord)

ndx             long
FieldName       string(64)
szFieldName     cstring(64)
ColVar          any
PosStr          long

  code
    ndx = 1
    loop
      szFieldName = who(pFileRecord, ndx)
      if szFieldName
        FieldName = self.StripPrefix(szFieldName)
        PosStr =  instring('|', FieldName, 1, 1)
        if PosStr
          FieldName = clip(FieldName[1 : PosStr - 1])
        end
        ColVar &= what(pFileRecord, ndx)
        self.AddFieldsInfo(pTableName, FieldName, ColVar, 0)
        ColVar &= null
        ndx += 1
      else
        break
      end  
    end

TableMapper.AddFieldsInfo    procedure(string pTableName, *queue pQueue)

ndx             long
FieldName       string(64)
szFieldName     cstring(64)
ColVar          any

  code
    ndx = 1
    loop
      szFieldName = who(pQueue, ndx)
      if szFieldName
        FieldName = self.StripPrefix(szFieldName)
        ColVar &= what(pQueue, ndx)
        self.AddFieldsInfo(pTableName, FieldName, ColVar, 0)
        ColVar &= null
        ndx += 1
      else
        break
      end  
    end
  
  
TableMapper.ClearFieldInfo    procedure()

  code
    free(self.FieldsInfo)


TableMapper.InitADOObjects      procedure(*CRecordset pRecordset)

hr              HRESULT
dwCount         long
ndx             long
bstrName        bstring

BaseTable       string('BASETABLENAME')
Properties      &CProperties
Property        &CProperty

gVar            like(gVariant)
aVar            variant, over(gVar)

  code
    SELF.ClearADOObjects()
    SELF.Flds &= pRecordset.GetFields(hr)
    if hr = S_Ok and ~(SELF.Flds &= null)
      hr = SELF.Flds.GetCount(dwCount)
      if hr = S_OK
        loop ndx = 0 to dwcount - 1
          clear(SELF.ADOFieldsQ)
          SELF.ADOFieldsQ.Field &= null
          SELF.ADOFieldsQ.TargetField &= null
          SELF.ADOFieldsQ.Field &= SELF.Flds.GetField(ndx, hr)
          if hr = S_OK and ~(SELF.ADOFieldsQ.Field &= null)
            SELF.ADOFieldsQ.Index = ndx + 1

            hr = SELF.ADOFieldsQ.Field.GetName(bstrName)
            SELF.ADOFieldsQ.FieldName = bstrName

            ! Get BASETABLENAME property
            Properties &= SELF.ADOFieldsQ.Field.GetProperties(hr)
            if hr = S_OK
              Property &= Properties.GetItem(BaseTable, hr)
              if hr = S_OK
                hr = Property.GetValue(gVar)
                if hr = S_OK
                  SELF.ADOFieldsQ.BaseTableStr = aVar
                  
                  add(SELF.ADOFieldsQ, SELF.ADOFieldsQ.Index)
                end
              end
            end

            if ~(Property &= null)
              dispose(Property)
            end

            if ~(Properties &= null)
              dispose(Properties)
            end

          end
        end
      end
    end


TableMapper.ClearADOObjects     procedure

ndx     long

  code
    if records(SELF.ADOFieldsQ)
      loop ndx = 1 to records(SELF.ADOFieldsQ)
        get(SELF.ADOFieldsQ, ndx)
        if ~(SELF.ADOFieldsQ.Field &= null)
          dispose(SELF.ADOFieldsQ.Field)
          SELF.ADOFieldsQ.Field &= null
          put(SELF.ADOFieldsQ)
        end
      end
      free(SELF.ADOFieldsQ)
    end

    if ~(SELF.Flds &= null)
      dispose(SELF.Flds)
    end



    
TableMapper.Kill        PROCEDURE()
 CODE


TableMapper.Map         PROCEDURE(*CRecordset RS)

  code

    SELF.InitADOObjects(RS)
    SELF.Map()



TableMapper.Map         procedure()

ndx             long
hr              HRESULT

gVar            like(gVariant)
aVar            variant, over(gVar)
vtValue         group(tVariant), over(gVar)
                end

vtValuer        &tVariantr
lType           LONG

DateGroup       group(DateGroupType).
DateTime        group
Date              date
Time              time
                end

bstrDate        bstring



    code


    if records(SELF.ADOFieldsQ)
      loop ndx = 1 to records(SELF.ADOFieldsQ)
        SELF.ADOFieldsQ.Index = ndx
        get(SELF.ADOFieldsQ, SELF.ADOFieldsQ.Index)

        if ~errorcode()
          if self.ADOFieldsQ.TargetField &= null
            self.FieldsInfo.BaseTable = clip(upper(SELF.ADOFieldsQ.BaseTableStr))
            self.FieldsInfo.ColName = clip(upper(SELF.ADOFieldsQ.FieldName))
            get(self.FieldsInfo, self.FieldsInfo.BaseTable, self.FieldsInfo.ColName)
            if ~errorcode()
              SELF.ADOFieldsQ.TargetField &= self.FieldsInfo.TargetField
              SELF.ADOFieldsQ.DateTimeID = self.FieldsInfo.DateTimeID
              put(SELF.ADOFieldsQ)
              !do Mapping
            else
              ! Try to lookup the colname in the internal queue with a blank tablename
              self.FieldsInfo.BaseTable = ''
              self.FieldsInfo.ColName = clip(upper(SELF.ADOFieldsQ.FieldName))
              get(self.FieldsInfo, self.FieldsInfo.BaseTable, self.FieldsInfo.ColName)
              if ~errorcode()
                SELF.ADOFieldsQ.TargetField &= self.FieldsInfo.TargetField
                SELF.ADOFieldsQ.DateTimeID = self.FieldsInfo.DateTimeID
                put(SELF.ADOFieldsQ)
                !do Mapping
              end
            end
          end

          do Mapping

        end
      end
    end


Mapping     routine

    data

    code
     hr = SELF.ADOFieldsQ.Field.GetValue(gVar)
     if hr = S_OK
       if gVar.vt = VT_DATE ! Column is a date or time type
         !hr = SELF.ADOFieldsQ.Field.GetType(lType)
         !if hr = S_OK
           case self.ADOFieldsQ.DateTimeID
           of adDBDate
           orof adDate
             hr =  VarBstrFromDate(gVar.dblVal, 0, 2, bstrDate)
             !self.FieldsInfo.TargetField = deformat(bstrDate, @d17)
             SELF.ADOFieldsQ.TargetField = deformat(bstrDate, @d17)
           of adDBTime
             hr =  VarBstrFromDate(gVar.dblVal, 0, 1, bstrDate)
             !self.FieldsInfo.TargetField = deformat(bstrDate, @t4)
             self.ADOFieldsQ.TargetField = deformat(bstrDate, @t4)
           of adDBTimeStamp
             vtValuer &= vtValue
             self.GetDateValue(vtValuer, DateGroup, hr)
             if self.bCwStdDateTime
               self.ADOFieldsQ.TargetField = DateGroup
             else
               DateTime.Date = DateGroup.Date
               DateTime.Time = DateGroup.Time
               self.ADOFieldsQ.TargetField = DateTime
             end
           end
         !end
       else
         if gVar.vt = VT_NULL
           case self.ADOFieldsQ.DateTimeID
           of adDBDate
           orof adDate
             hr =  VarBstrFromDate(gVar.dblVal, 0, 2, bstrDate)
             self.ADOFieldsQ.TargetField = deformat(bstrDate, @d17)
           of adDBTime
             hr =  VarBstrFromDate(gVar.dblVal, 0, 1, bstrDate)
             self.ADOFieldsQ.TargetField = deformat(bstrDate, @t4)
           of adDBTimeStamp
             vtValuer &= vtValue
             self.GetDateValue(vtValuer, DateGroup, hr)
             if self.bCwStdDateTime
               self.ADOFieldsQ.TargetField = DateGroup
             else
               DateTime.Date = DateGroup.Date
               DateTime.Time = DateGroup.Time
               self.ADOFieldsQ.TargetField = DateTime
             end
           else
             clear(self.ADOFieldsQ.TargetField)
           end
         else
           self.ADOFieldsQ.TargetField = aVar
         end
       end
     end


TableMapper.MapToRS     procedure(*CRecordset pRS)

oFields     &CFields
oField      &CField

hr          HRESULT

gVar        like(gVariant)
aVar        variant, over(gVar)

dwCount     long
bstrName    bstring
iCol        long

Properties  &CProperties
Property    &CProperty
AttribVal   long

BaseTableStr    string('BASETABLENAME')
BaseTableName   string(128)
RetValue        byte

    code
    if ~records(self.FieldsInfo)
      RetValue = false
    else
      oFields &= pRS.GetFields(hr)
      if hr = S_OK
        hr = oFields.GetCount(dwCount)
        if hr = S_OK
          loop iCol = 0 to dwCount - 1
            oField &= oFields.GetField(iCol, hr)
            if hr = S_OK
              hr = oField.GetName(bstrName)
              Properties &= oField.GetProperties(hr)
              if hr = S_OK
                Property &= Properties.GetItem(BaseTableStr, hr)
                if hr = S_OK
                  hr = oField.GetAttributes(AttribVal)
                  if hr = S_OK
                    if band(AttribVal, 4) or band(AttribVal, 8)
                      hr = Property.GetValue(gVar)
                      if hr = S_OK
                        BaseTableName = aVar
                        self.FieldsInfo.BaseTable = clip(upper(BaseTableName))
                        self.FieldsInfo.ColName = clip(upper(bstrName))
                        get(self.FieldsInfo, self.FieldsInfo.BaseTable, self.FieldsInfo.ColName)
                        if ~errorcode()
                          aVar = self.FieldsInfo.TargetField
                          hr = oField.PutValue(gVar)
                          if hr = S_OK
                            RetValue = true
                          else
                            dispose(Property)
                            dispose(Properties)
                            dispose(oField)
                            RetValue = false
                            break
                          end
                        else
                          ! Lookup with ColName with a blank tablename
                          self.FieldsInfo.BaseTable = ''
                          self.FieldsInfo.ColName = clip(upper(bstrName))
                          get(self.FieldsInfo, self.FieldsInfo.BaseTable, self.FieldsInfo.ColName)
                          if ~errorcode()
                            aVar = self.FieldsInfo.TargetField
                            hr = oField.PutValue(gVar)
                            if hr = S_OK
                              RetValue = true
                            else
                              dispose(Property)
                              dispose(Properties)
                              dispose(oField)
                              RetValue = false
                              break
                            end
                          end
                        end
                      end
                    end
                  end
                end
                dispose(Property)
              end
              dispose(Properties)
            end
            dispose(oField)
          end
        end
        dispose(oFields)
      end
    end
    return(RetValue)


TableMapper.GetWHERE        procedure(*CRecordset RS)

bResult         bool
Flds            &CFields
Fld             &CField
hr              HRESULT
dwCount         long
iCol            long
lType           LONG
bstrName        bstring
Properties      &CProperties
BaseTableStr    string('BASETABLENAME')
BaseTableName   string(128)
Property        &CProperty

gVar            like(gVariant)
aVar            variant, over(gVar)
vtValue         group(tVariant), over(gVar)
                end

vtValuer        &tVariantr

bstrDate        bstring
bstrValue       bstring

DateGroup       group(DateGroupType).
DateTime        group
Date              date
Time              time
                end

sBool           short

sReturnVal      string(1024)

szTime          cstring(10)
szDate          cstring(10)

  code
    bResult = true
    sReturnVal = ''
    Flds &= Rs.GetFields(hr)
    if hr = S_OK
      hr = Flds.GetCount(dwCount)
      if hr = S_OK
        loop iCol = 0 to dwCount - 1
          Fld &= Flds.GetField(iCol, hr)
          if hr = S_OK
            hr = Fld.GetName(bstrName)
            if hr = S_OK
              ! Get BASETABLENAME property
              Properties &= Fld.GetProperties(hr)
              if hr = S_OK
                Property &= Properties.GetItem(BaseTableStr, hr)
                if hr = S_OK
                  hr = Property.GetValue(gVar)
                  if hr = S_OK
                    BaseTableName = aVar
                    self.FieldsInfo.BaseTable = clip(upper(BaseTableName))
                    self.FieldsInfo.ColName = clip(upper(bstrName))
                    get(self.FieldsInfo, self.FieldsInfo.BaseTable, self.FieldsInfo.ColName)
                    if ~errorcode()
                      hr = Fld.GetValue(gVar)
                      if hr = S_OK
                        case gVar.vt
                        of VT_BOOL
                          sReturnVal = clip(sReturnVal) & ' ' & clip(self.FieldsInfo.ColNameSQL) & ' = ' & (aVar * -1) & ' AND '
                        of VT_DATE
                        !if gVar.vt = VT_DATE ! Column is a date or time type
                          hr = Fld.GetType(lType)
                          if hr = S_OK
                            case self.FieldsInfo.DateTimeID
                            of adDBDate
                              hr =  VarBstrFromDate(gVar.dblVal, 0, 2, bstrDate)
                              sReturnVal = clip(sReturnVal) & ' ' & clip(self.FieldsInfo.ColNameSQL) & ' = ''' & bstrDate & '''' & ' AND '
                            of adDBTime
                              hr =  VarBstrFromDate(gVar.dblVal, 0, 1, bstrDate)
                              sReturnVal = clip(sReturnVal) & ' ' & clip(self.FieldsInfo.ColNameSQL) & ' = ''' & bstrDate & '''' & ' AND '
                            of adDBTimeStamp
                              vtValuer &= vtValue
                              self.GetDateValue(vtValuer, DateGroup, hr)
                              DateTime.Date = DateGroup.Date
                              DateTime.Time = DateGroup.Time
                              if self.szDatePicture
                                szDate = self.szDatePicture
                              else
                                szDate = '@d10'
                              end
                              if self.szTimePicture
                                szTime = self.szTimePicture
                              else
                                szTime = '@t4'
                              end
                              sReturnVal = clip(sReturnVal) & ' ' & clip(self.FieldsInfo.ColNameSQL) & ' = ''' & format(DateTime.Date, szDate) & ' ' & format(DateTime.Time, szTime) & '''' & ' AND '
                            end
                          end
                        else
                          if gVar.vt = VT_NULL
                            sReturnVal = clip(sReturnVal) & ' ' & clip(self.FieldsInfo.ColNameSQL) & ' IS NULL AND'
                          else
                            if isstring(self.FieldsInfo.TargetField)
                              sReturnVal = clip(sReturnVal) & ' ' & clip(self.FieldsInfo.ColNameSQL) & ' = ''' & clip(quote(aVar, 1)) & '''' & ' AND '
                            else
                              sReturnVal = clip(sReturnVal) & ' ' & clip(self.FieldsInfo.ColNameSQL) & ' = ' & aVar & ' AND '
                            end
                          end
                        end
                      end
                    end
                  end
                end
                dispose(Property)
              end
              dispose(Properties)
            end
          end
        end
        dispose(Flds)
      end
    end
    if sReturnVal
      sReturnVal = sReturnVal[1 : len(clip(sReturnVal)) - 4]   ! Remove extra ' AND' in string
    end
    return(sReturnVal)

