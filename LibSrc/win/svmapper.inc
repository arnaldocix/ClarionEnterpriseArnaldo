
!ABCIncludeFile(SV)

OMIT('_EndOfInclude_',_ADOMapper_)
_ADOMapper_ EQUATE(1)



!*
!* Includes
!*

  include('svbase.inc'),once
  include('svdate.inc'),once

!*
!* Defines
!*

!*
!* Declarations
!*


GroupFields     queue,type
FieldName         cstring(64)
FieldPos          long
                end
                
TFieldsInfo     queue, type
BaseTable         cstring(64)
ColName           cstring(64)
ColNameSQL        cstring(64)
TargetField       any
DateTimeID        short
                end

TADOFieldsG     group, type
index             long
Field             &cField
FieldName         string(50)
BaseTableStr      string(128)
TargetField       any
DateTimeID        short
                end

TADOFieldsQ     queue(TADOFieldsG), type
                end


TableMapper     CLASS,TYPE,MODULE('svmapper.clw'),LINK('svmapper.clw', _svLinkMode_),DLL(_svDllMode_)

Flds                 &cFields, protected

Construct            PROCEDURE()
Destruct             PROCEDURE()
Kill                 PROCEDURE(),VIRTUAL

InitADOObjects       procedure(*CRecordset pRecordset), virtual

MapRsToGroup         PROCEDURE(*CRecordSet SourceRs, *Group TargetGrp, byte bCwStdDateTime = false)
MapRsToGroup         PROCEDURE(*CRecordSet SourceRs, *QUEUE TargetQ, byte bCwStdDateTime = false)
MapRsToGroup         PROCEDURE(*CRecordSet SourceRs, byte bCwStdDateTime = false)
MapGroupToRs         PROCEDURE(*CRecordSet TargetRs, *Group SourceGrp, byte bCwStdDateTime = false)
MapGroupToRs         PROCEDURE(*CRecordSet TargetRs, byte bCwStdDateTime = false)
Map                  PROCEDURE(*CRecordSet Rs)
Map                  PROCEDURE()
MapToRS              PROCEDURE(*CRecordset Rs), BYTE
GetWHERE             PROCEDURE(*CRecordSet Rs), STRING

! Protected

SetupGroupFieldQueue PROCEDURE(*Group Grp),byte,protected
SetupGroupFieldQueue PROCEDURE(*QUEUE Q),byte,protected
AssignFields         PROCEDURE(*CRecordSet Rs, byte bTarget = false),byte,protected
AssignField          PROCEDURE(*CField Fld, byte bTarget = false),byte,protected
GetDateValue         PROCEDURE(*tVariantr vtValuer, *DateGroupType DateGroup, *long hr),protected
GetNumericSize       PROCEDURE(*tVariant vtValue, *long dwBytes),long,protected
GetStringValue       PROCEDURE(*tVariant vtValue, *long hr),*cstring,protected
StripPrefix          PROCEDURE(*cstring szFieldName),*cstring,protected

AddFieldsInfo        PROCEDURE(string pTableName, string pColName, *? pTargetVar, short pDateTimeID)
AddFieldsInfo        PROCEDURE(string pTableName, *group pFileRecord)
AddFieldsInfo        PROCEDURE(string pTableName, *queue pQueue)
ClearFieldInfo       PROCEDURE()
ClearADOObjects      PROCEDURE()


szDatePicture       cstring(10)
szTimePicture       cstring(10)

! Private

GrpInfo           &GroupFields,private
Grp               &group,private
Q                 &queue,private
bInitialised      bool,private
bCwStdDateTime    bool,private
FieldsInfo        &TFieldsInfo
ADOFieldsQ        &TADOFieldsQ, protected

                end

 _EndOfInclude_

