  MEMBER

  PRAGMA ('define(init_priority=>19)')
TraceFiles        EQUATE(0)

FileIDType        EQUATE(LONG)                  !Data type for unique reference to file


    INCLUDE('ABFILE.INC'),ONCE
    INCLUDE('ABUTIL.INC'),ONCE
    INCLUDE('ERRORS.CLW'),ONCE
    INCLUDE('CWSYNCHC.INC'),ONCE

FileOpenServer                CLASS,TYPE
MyFile                          &FILE,PROTECTED
Usage                           UNSIGNED,PROTECTED

Construct                       PROCEDURE
Destruct                        PROCEDURE,VIRTUAL

Init                            PROCEDURE(FIlE aFile),VIRTUAL
Link                            PROCEDURE
Release                         PROCEDURE
                              END


ThreadedFileOpenServer        CLASS(FileOpenServer),IMPLEMENTS(IFileOpenServer),TYPE
                              END


UnthreadedFileOpenServer      CLASS(FileOpenServer),IMPLEMENTS(IFileOpenServer),TYPE
CSect                           &CriticalSection,PRIVATE
FileUsageCount                  UNSIGNED,PRIVATE

Construct                       PROCEDURE
Destruct                        PROCEDURE,VIRTUAL

Init                            PROCEDURE(File afile),DERIVED
                              END


UnthreadedFileOpenerMap       QUEUE
FileID                          UNSIGNED
Opener                          &UnthreadedFileOpenServer
                              END


CreateOpenerCrit              CriticalSection




    MAP
ConcatGetComponents             PROCEDURE(KeyFieldQueue,*CSTRING,BYTE HowMany),PRIVATE
DupString                       PROCEDURE(STRING),*STRING,PRIVATE
CasedValue                      PROCEDURE(STRING FieldName,*? Field,? FieldValue),STRING,PRIVATE
LocalAction                     PROCEDURE(BYTE Mode),BYTE,PRIVATE
CleanUp                         PROCEDURE(FileManager),PRIVATE
OpCodeCanBeDone                 PROCEDURE(SIGNED opCode),BYTE,PRIVATE

CreateThreadedFileOpenServer    PROCEDURE(FILE aFile), *IFileOpenServer,PRIVATE
CreateUnthreadedFileOpenServer  PROCEDURE(FILE aFile), *IFileOpenServer,PRIVATE

CreateFileOpenServer            PROCEDURE(FILE aFile, BOOL threaded),*IFileOpenServer,PRIVATE
CreateFileOpenServer            PROCEDURE(FILE aFile),*IFileOpenServer,PRIVATE
    END



RelationQueue                 QUEUE,TYPE,PRIVATE      ! List of all related files
File                            &RelationManager      ! Related file
Fields                          &BufferedPairsClass   ! List of linking fields
HisKey                          &KEY                  ! Only filled in for 1:MANY relationships
UpdateMode                      BYTE                  ! Action on Update RI flag
DeleteMode                      BYTE                  ! Action on Delete RI flag
Opened                          BYTE
                              END


KeyFieldQueue                 QUEUE,TYPE,PRIVATE ! QUEUE of all key components
Field                           ANY              ! Key component
FieldName                       &STRING          ! This should contain the UPPER for a case insensitive field
Ascend                          BYTE             ! Ascending/descending flag
                              END

FileKeyQueue                  QUEUE,TYPE,PRIVATE ! QUEUE of all keys in a file
Key                             &KEY             ! Reference to a KEY
Description                     STRING(80)       ! Key description text (from Data dictionary)
Fields                          &KeyFieldQueue   ! Reference to key components queue
AutoInc                         BYTE             ! Auto-increment flag (from Data dictionary)
Dups                            BYTE             ! Duplicates allowed flag (from Data dictionary)
NoCase                          BYTE             ! Case sensitive flag (from Data dictionary)
                              END

FieldsList                    QUEUE,TYPE,PRIVATE
Tag                             ASTRING
Fld                             ANY
fType                           ASTRING
fPicture                        ASTRING
                              END

SaveQueue                     QUEUE,TYPE,PRIVATE
Buffer                          USHORT
Id                              USHORT
AutoIncDone                     BYTE
State                           LONG
Pos                             STRING(1024)
                              END


Epoc                          LONG(1),THREAD
szDbTextLog                   CSTRING(FILE:MaxFilePath + 1),THREAD  ! DbAudit Data

TransactionManagerQueue QUEUE,TYPE
RM                       &RelationManager
RMInstance               LONG
Cascade                  BYTE
                        END

FilesOnTransactionQueue QUEUE,TYPE
RM                        &RelationManager
RMInstance                LONG
PrevLogout                BYTE
                        END


  COMPILE('***',TraceFiles)
Trace FILE,DRIVER('ASCII'),CREATE
R       RECORD
B         STRING(1000)
        END
      END

LinkASCIIDriver  EQUATE(lib_mode)
  ***
  OMIT('***',TraceFiles)
LinkASCIIDriver  EQUATE(0)
  ***

  COMPILE('=== DO LINK', LinkASCIIDriver)
    PRAGMA ('link (C%V%ASC%X%%L%.LIB)')
! === DO LINK


StatusQ_t                     QUEUE,TYPE
Thread                          LONG
Proc                            ASTRING
ID                              FileIDType
UStat                           BYTE
Hold                            USHORT
ScopeLevel                      USHORT
NestedOpen                      USHORT
                              END


FileMapping_t                 QUEUE,TYPE
FileLabel                       FileIDType
FileManager                     &FileManager
                              END
FileMapping_r                 QUEUE,TYPE
FileLabel                       FileIDType
RelationManager                 &RelationManager
                              END


FilesManager                 CLASS,THREAD
Errs                            &ErrorClass
FileMapping                     &FileMapping_t
RelationMapping                 &FileMapping_r
StatusQ                         &StatusQ_t

Construct                       PROCEDURE
Destruct                        PROCEDURE

AddFileMapping                  PROCEDURE(FileManager FM),PRIVATE
AddFileMapping                  PROCEDURE(RelationManager RM),PRIVATE

FindRecord                      PROCEDURE(FileManager FM),BYTE
GetFileID                       PROCEDURE(File ThisFile),FileIDType
GetFileMapping                  PROCEDURE(FileIDType FileLabel),*FileManager
GetRelationFileMapping          PROCEDURE(FileIDType FileLabel),*RelationManager
NoteClose                       PROCEDURE(FileManager FM)
NoteOpen                        PROCEDURE(FileManager FM)
NoteUsage                       PROCEDURE(FileManager FM, BYTE Level)
RemoveFileMapping               PROCEDURE(FileIdType FileLabel)
RemoveFileMapping               PROCEDURE(FileManager FM)
RemoveRelationFileMapping       PROCEDURE(FileIdType FileLabel)
RemoveRelationFileMapping       PROCEDURE(RelationManager RM)
Trace                           PROCEDURE(STRING S)
                              END



FilesManager.Construct PROCEDURE

  CODE
  SELF.FileMapping &= NEW FileMapping_t
  SELF.RelationMapping &= NEW FileMapping_r
  SELF.StatusQ &= NEW StatusQ_t
  COMPILE('***',TraceFiles)
  CREATE(Trace)
? ASSERT(~ERRORCODE(),'Unable to create trace file')
  OPEN(Trace)
? ASSERT(~ERRORCODE(),'Unable to open trace file')
  ***

FilesManager.Destruct PROCEDURE

  CODE
  DISPOSE(SELF.StatusQ)
  DISPOSE(SELF.FileMapping)
  FREE(SELF.RelationMapping)
  DISPOSE(SELF.RelationMapping)
  COMPILE('***',TraceFiles)
  CLOSE(Trace)
  ***

FilesManager.NoteUsage PROCEDURE(FileManager FM,BYTE Level)
  CODE
  IF SELF.FindRecord(FM) <> Level:Benign
    SELF.StatusQ.ScopeLevel = 0
    DO AddQ
  ELSIF SELF.StatusQ.Proc = SELF.Errs.GetProcedureName()
    IF Level > SELF.StatusQ.Ustat
      SELF.StatusQ.Ustat = Level
    COMPILE('***',TraceFiles)
      SELF.Trace('New Level :' & SELF.StatusQ.Proc & '(' & SELF.StatusQ.ScopeLevel & '):' & SELF.StatusQ.Ustat)
    ***
      PUT(SELF.StatusQ)
?     ASSERT(~ERRORCODE())
    END
  ELSE
    DO AddQ
    IF Level < UseType:Returns AND ~SELF.StatusQ.Hold
    COMPILE('***',TraceFiles)
      SELF.Trace('**Record getting clobbered')
    ***
      SELF.StatusQ.Hold = FM.SaveFile()
      PUT(SELF.StatusQ)
?     ASSERT(~ERRORCODE())
    END
  END

AddQ ROUTINE
    SELF.StatusQ.Thread = THREAD()
    SELF.StatusQ.Proc = SELF.Errs.GetProcedureName()
    SELF.StatusQ.Ustat = Level
    SELF.StatusQ.ScopeLevel += 1
    SELF.StatusQ.Hold = 0
    SELF.StatusQ.NestedOpen = 0
    COMPILE('***',TraceFiles)
      SELF.Trace('New Scope :'&CLIP(FM.GetName()) & '(' & SELF.StatusQ.ScopeLevel & '):' & SELF.StatusQ.Ustat)
    ***
    ADD(SELF.StatusQ)
?   ASSERT(~ERRORCODE())

FilesManager.NoteOpen  PROCEDURE(FileManager FM)
  CODE
  IF SELF.FindRecord(FM) = Level:Benign AND SELF.StatusQ.Proc = SELF.Errs.GetProcedureName()
    SELF.StatusQ.NestedOpen += 1
    COMPILE('***',TraceFiles)
      SELF.Trace('Nested Open:' & CLIP(FM.GetName()) & ':' & SELF.StatusQ.NestedOpen)
    ***
    PUT(SELF.StatusQ)
?   ASSERT(~ERRORCODE())
  END

FilesManager.NoteClose PROCEDURE(FileManager FM)
  CODE
  IF SELF.FindRecord(FM) = Level:Benign AND SELF.StatusQ.Proc = SELF.Errs.GetProcedureName()
    IF SELF.StatusQ.NestedOpen
      SELF.StatusQ.NestedOpen -= 1
      PUT(SELF.StatusQ)
?     ASSERT(~ERRORCODE())
    COMPILE('***',TraceFiles)
      SELF.Trace('Un-nesting Open:' & CLIP(FM.GetName()) & ':' & SELF.StatusQ.NestedOpen)
    ***
    ELSE
    COMPILE('***',TraceFiles)
      SELF.Trace('Cleanup:' & CLIP(FM.GetName()) & '(' & SELF.StatusQ.ScopeLevel & '):' & SELF.StatusQ.Ustat)
    ***
      IF SELF.StatusQ.Hold
      COMPILE('***',TraceFiles)
        IF SELF.StatusQ.Ustat < UseType:Returns
          SELF.Trace('**Restoring:' & CLIP(FM.GetName()))
        ELSE
          SELF.Trace('**Discarding:' & CLIP(FM.GetName()))
        END
      ***
        FM.RestoreFile(SELF.StatusQ.Hold, CHOOSE(SELF.StatusQ.Ustat < UseType:Returns))
      END
      DELETE(SELF.StatusQ)
?     ASSERT(~ERRORCODE())
    END
  END

FilesManager.FindRecord PROCEDURE(FileManager FM)

FID   LONG,AUTO
Th    LONG,AUTO
Pos   LONG,AUTO

  CODE
  SORT (SELF.StatusQ, SELF.StatusQ.Thread, SELF.StatusQ.Id, SELF.StatusQ.ScopeLevel)

  Th  = THREAD()
  Fid = SELF.GetFileID(FM.File)
  SELF.StatusQ.Id = Fid
  SELF.StatusQ.Thread = Th
  SELF.StatusQ.ScopeLevel = 1
  GET (SELF.StatusQ, SELF.StatusQ.thread, SELF.StatusQ.Id, SELF.StatusQ.ScopeLevel)
  IF ERRORCODE()
    RETURN Level:Notify
  END
  Pos = POINTER (SELF.StatusQ)

  LOOP
    Pos += 1
    GET (SELF.StatusQ, Pos)
  WHILE ERRORCODE() = 0 AND SELF.StatusQ.Thread = Th AND SELF.StatusQ.Id = Fid

  GET (SELF.StatusQ, Pos - 1)
  RETURN Level:Benign

FilesManager.Trace PROCEDURE(STRING S)
  CODE
  COMPILE('***',TraceFiles)
  Trace.R.B = SELF.Errs.GetProcedureName() &'(' & THREAD() & '):' & CLIP(S)
  ADD(Trace)
  ***


FilesManager.AddFileMapping PROCEDURE(FileManager FM)

  CODE
    SELF.Errs &= FM.Errors                  ! more -- only assign once?
    SELF.FileMapping.FileLabel = SELF.GetFileID(FM.File)
    GET(SELF.FileMapping, SELF.FileMapping.FileLabel)
?   ASSERT(ERRORCODE(),'Mapping for file already exists for the FM.|File Manager''s File Name:'&FM.GetName())
    SELF.FileMapping.FileLabel = SELF.GetFileID(FM.File)
    SELF.FileMapping.FileManager &= FM
    ADD(SELF.FileMapping, SELF.FileMapping.FileLabel)
?   ASSERT(~ERRORCODE(),'Error appending to ordered queue')

FilesManager.GetRelationFileMapping PROCEDURE(FileIDType FileLabel)
  CODE
    SELF.RelationMapping.FileLabel = FileLabel
    GET(SELF.RelationMapping, SELF.RelationMapping.FileLabel)
    IF ~ERRORCODE() THEN RETURN SELF.RelationMapping.RelationManager.
    RETURN NULL

FilesManager.AddFileMapping PROCEDURE(RelationManager RM)
  CODE
    SELF.RelationMapping.FileLabel = SELF.GetFileID(RM.Me.File)
    GET(SELF.RelationMapping, SELF.RelationMapping.FileLabel)
?   ASSERT(ERRORCODE(),'Mapping for file already exists for the RM.|Relation Manager''s File Name:'&RM.Me.GetName())
    SELF.RelationMapping.FileLabel = SELF.GetFileID(RM.Me.File)
    SELF.RelationMapping.RelationManager &= RM
    ADD(SELF.RelationMapping, SELF.RelationMapping.FileLabel)
?   ASSERT(~ERRORCODE(),'Error appending to ordered queue')

FilesManager.RemoveFileMapping PROCEDURE(FileManager FM)

  CODE
    SELF.RemoveFileMapping(SELF.GetFileID(FM.File))


FilesManager.RemoveFileMapping PROCEDURE(FileIDType FileLabel)

  CODE
    IF ~SELF.GetFileMapping(FileLabel) &= NULL
      DELETE(SELF.FileMapping)
?     ASSERT(~ERRORCODE(),'Delete failed on queue')
    END

FilesManager.RemoveRelationFileMapping PROCEDURE(RelationManager RM)

  CODE
    SELF.RemoveRelationFileMapping(SELF.GetFileID(RM.Me.File))


FilesManager.RemoveRelationFileMapping PROCEDURE(FileIdType FileLabel)

  CODE
    IF ~SELF.GetRelationFileMapping(FileLabel) &= NULL
      DELETE(SELF.RelationMapping)
?     ASSERT(~ERRORCODE(),'Delete failed on queue')
    END

FilesManager.GetFileMapping PROCEDURE(FileIDType FileLabel)

  CODE
    SELF.FileMapping.FileLabel = FileLabel
    GET(SELF.FileMapping, SELF.FileMapping.FileLabel)
    IF ~ERRORCODE() THEN RETURN SELF.FileMapping.FileManager.
    RETURN NULL


FilesManager.GetFileID PROCEDURE(FILE ThisFile)   ! This procedure returns a unique, application global, file ID

  CODE
  RETURN INSTANCE(ThisFile, THREAD())


OpCodeCanBeDone  PROCEDURE (opCode)
  CODE
    CASE opCode
    OF   DriverOp:DESTROY
    OROF DriverOp:Close
    OROF DriverOp:GetNulls
    OROF DriverOp:SetNulls
      RETURN TRUE
    ELSE
      RETURN FALSE
    END


FileManager.FunctionCalled  PROCEDURE(SIGNED opCode, *Params Parameters, *CSTRING ErrCode, *CSTRING ErrMsg)
RetVal  BYTE(TRUE)
  CODE
  IF SELF.InView
    RETURN TRUE
  END
  SELF.StopOperation = Parameters.StopOperation
  IF NOT Parameters.View &= NULL
    SELF.InView = TRUE
  END
  CASE opCode
  OF DriverOp:Delete
    RetVal = SELF.PreDelete(ErrCode, ErrMsg)
  OF DriverOp:Add
  OROF DriverOp:Append
  OROF DriverOP:AddLen
  OROF DriverOp:AppendLen
    RetVal = SELF.PreInsert(opCode, Parameters.Len, ErrCode, ErrMsg)
  OF DriverOp:Put
  OROF DriverOp:PutFilePtr
  OROF DriverOp:PutFilePtrLen
    RetVal = SELF.PreUpdate(Parameters.Pointer, Parameters.Len, ErrCode, ErrMsg)
  OF DriverOp:Create
    RetVal = SELF.PreCreate(ErrCode, ErrMsg)
  OF DriverOp:GETfilekey
  OROF DriverOp:GETfileptrlen
  OROF DriverOp:GETfileptr
  OROF DriverOp:GETkeyptr
  OROF DriverOp:REGETfile
  OROF DriverOp:REGETkey
  OROF DriverOp:Next
  OROF DriverOp:Previous
    RetVal = SELF.PreFetch(opCode, Parameters.Key1, Parameters.Position, Parameters.Pointer, Parameters.Len, ErrCode, ErrMsg)
  END
  Parameters.StopOperation = SELF.StopOperation
  RETURN RetVal

FileManager.FunctionDone                    PROCEDURE(SIGNED opCode, *Params Parameters, *CSTRING ErrCode, *CSTRING ErrMsg)
  CODE
  IF SELF.InView
    IF Parameters.View &= NULL
      RETURN TRUE
    END
    SELF.InView = FALSE
  END
  CASE opCode
  OF DriverOp:Add
  OROF DriverOp:Append
  OROF DriverOp:AddLen
  OROF DriverOp:AppendLen
    RETURN SELF.PostInsert(ErrCode, ErrMsg)
  OF DriverOp:Put
  OROF DriverOp:PutFilePtr
  OROF DriverOp:PutFilePtrLen
    RETURN SELF.PostUpdate(ErrCode, ErrMsg)
  OF DriverOp:Delete
    RETURN SELF.PostDelete(ErrCode, ErrMsg)
  OF DriverOp:Create
    RETURN SELF.PostCreate(ErrCode, ErrMsg)
  OF DriverOp:GETfilekey
  OROF DriverOp:GETfileptrlen
  OROF DriverOp:GETfileptr
  OROF DriverOp:GETkeyptr
  OROF DriverOp:REGETfile
  OROF DriverOp:REGETkey
  OROF DriverOp:Next
  OROF DriverOp:Previous
    RETURN SELF.PostFetch(opCode, Parameters.Key1, Parameters.Position, Parameters.Pointer, Parameters.Len, ErrCode, ErrMsg)
  END
  RETURN TRUE

FileManager.FileCallBackInterface.FunctionCalled PROCEDURE(SIGNED opCode, *Params Parameters, *CSTRING ErrCode, *CSTRING ErrMsg)
RetVal  BYTE
  CODE
  CASE opCode
  OF DriverOp:Add
  OROF DriverOp:Append
  OROF DriverOp:AddLen
  OROF DriverOp:AppendLen
  OF DriverOP:Close
    IF SELF.InRestore AND SELF.Opened AND Parameters.View &= NULL
      SELF.Opened = 0                 ! File is being closed by a call to RESTORESTATE() - eed to make filemanager aware of the close.
      SELF.Used = False
      SELF.Close()
?   ELSIF ~SELF.InClose AND ~SELF.InRestore AND SELF.Opened AND Parameters.View &= NULL
?     SELF.InCallBack += 1
?     IF SELF.File{Prop:Thread}       ! Only enforce the assertion for threaded files
?       ASSERT(SELF.DoNotAssertOnCloseMisuse, 'You are calling CLOSE(' & NAME(SELF.File) & ') instead of FileManager.Close()')
?     END
?     SELF.InCallBack -= 1
    END
  OF DriverOp:Destroy
    SELF.InCallBack += 1
    IF ~Parameters.Index
      SELF.Cleanup()                ! File destructor being called.  Make sure associated file manager killed first
    ELSIF SELF.Used
?     ASSERT(SELF.DoNotAssertOnCloseMisuse, 'You have not called FileManager.Close() for ' & NAME(SELF.File))
    END
    SELF.InCallBack -= 1
  END
  RetVal = SELF.FunctionCalled(opCode,Parameters,ErrCode,ErrMsg)
? SELF.InRestore = CHOOSE(opCode = DriverOp:RestoreState)
  RETURN RetVal

FileManager.FileCallBackInterface.FunctionDone PROCEDURE(SIGNED opCode, *Params Parameters, *CSTRING ErrCode, *CSTRING ErrMsg)
  CODE
? IF ERRORCODE() = NotOpenErr AND NOT OpCodeCanBeDone(opCode) AND SELF.LazyOpen AND ~SELF.Used AND ~SELF.InUseFile AND ~SELF.InCallBack AND Parameters.View &= NULL
?   SELF.InCallBack += 1
?   ASSERT(SELF.DoNotAssertOnCloseMisuse, 'You have not called FileManager.UseFile() for ' & NAME(SELF.File))
?   SELF.InCallBack -= 1
? END
  CASE opCode
  OF DriverOP:SetBlobProperty
    IF Parameters.Property = PROP:Handle
      SELF.BlobChanged = True
    END
  OF DriverOp:PutBlobData
    SELF.BlobChanged = True
? OF DriverOp:RestoreState
?   SELF.InRestore = False
  OF DriverOp:Delete
    SELF.BlobChanged = false
  OF DriverOp:NEXT
  OROF DriverOp:PREVIOUS
  OROF DriverOp:GetFileKey
  OROF DriverOp:GetFilePtr
  OROF DriverOp:GetFilePtrLen
  OROF DriverOp:GetKeyPtr
  OROF DriverOp:RegetFile
  OROF DriverOp:RegetKey
       IF SELF.SavePreviousBuffer
          IF NOT (SELF.PreviousBuffer &= NULL)
             DISPOSE(SELF.PreviousBuffer)
          END
          SELF.PreviousBuffer &= DupString(SELF.Buffer)
       END
  OF DriverOp:Add
  OROF DriverOp:Append
  OROF DriverOp:AddLen
  OROF DriverOp:AppendLen
    SELF.BlobChanged = false
  OF DriverOp:Put
  OROF DriverOp:PutFilePtr
  OROF DriverOp:PutFilePtrLen
    SELF.BlobChanged = false
  END
  RETURN SELF.FunctionDone(opCode,Parameters,ErrCode,ErrMsg)

FileManager.PreDelete PROCEDURE(*CSTRING ErrCode, *CSTRING ErrMsg)

  CODE
  RETURN True


FileManager.PostDelete PROCEDURE(*CSTRING ErrCode, *CSTRING ErrMsg)

  CODE
  RETURN True


FileManager.PreInsert PROCEDURE(SIGNED OpCode, UNSIGNED AddLen, *CSTRING ErrCode, *CSTRING ErrMsg)

  CODE
  RETURN True


FileManager.PostInsert PROCEDURE(*CSTRING ErrCode, *CSTRING ErrMsg)

  CODE
  RETURN True


FileManager.PreUpdate PROCEDURE(LONG Pntr, UNSIGNED PutLen, *CSTRING ErrCode, *CSTRING ErrMsg)

  CODE
  RETURN True


FileManager.PostUpdate PROCEDURE(*CSTRING ErrCode, *CSTRING ErrMsg)

  CODE
  RETURN True

FileManager.PreFetch PROCEDURE(SIGNED OpCode, KEY key, STRING positionBuffer, LONG pointer, UNSIGNED recLen, *CSTRING ErrCode, *CSTRING ErrMsg)

  CODE
  RETURN True

FileManager.PostFetch    PROCEDURE(SIGNED OpCode, KEY key, STRING positionBuffer, LONG pointer, UNSIGNED recLen, *CSTRING ErrCode, *CSTRING ErrMsg)

  CODE
  RETURN True

FileManager.PreCreate    PROCEDURE(*CSTRING ErrCode, *CSTRING ErrMsg)

  CODE
  RETURN True

FileManager.PostCreate   PROCEDURE(*CSTRING ErrCode, *CSTRING ErrMsg)

  CODE
  RETURN True

FileManager.Destruct PROCEDURE
  CODE
  IF NOT SELF.Killed
    SELF.Kill()
  END
? IF NOT SELF.File &= NULL
?   ASSERT(NOT (STATUS(SELF.File) AND SELF.File{PROP:Thread} AND NOT SELF.DoNotAssertOnCloseMisuse) ,'The file is still open. Open Counter is '&SELF.Opened&' on File: ' & SELF.File{Prop:Name})
? END
  SELF.FileName &= NULL

! Add a record to a file. Make sure record is valid and perform any
! required autoincrement operations.
! This function must handle errors itself to get the job done
FileManager.Insert PROCEDURE
  CODE
    RETURN SELF.InsertServer(1)

FileManager.AddKey PROCEDURE(KEY k,STRING Desc,BYTE AutoInc)
CF   UNSIGNED,AUTO
FB   CSTRING(1000),AUTO
I    BYTE,AUTO
Comp BYTE,AUTO
  CODE
    SELF.Keys.Key &= k
    SELF.Keys.Description = Desc
    SELF.Keys.Fields &= NEW KeyFieldQueue
    SELF.Keys.AutoInc = AutoInc
    IF AutoInc
      SELF.HasAutoInc = 1
    END
    SELF.Keys.Dups = k{PROP:Dup}
    SELF.Keys.NoCase = k{PROP:NoCase}
    ADD(SELF.Keys)
    IF k{PROP:Primary}
      SELF.PrimaryKey = RECORDS(SELF.Keys)
    END
    Comp = K{PROP:Components}
    LOOP I = 1 TO Comp
      CLEAR(SELF.Keys.Fields)
      SELF.Keys.Fields.Ascend = K{PROP:Ascending,I}
      CF = K{PROP:Field,I}
      SELF.Keys.Fields.Field &= WHAT(SELF.Buffer,CF)
      FB = SELF.File{PROP:Label,CF}
      SELF.Keys.Fields.FieldName &= DupString(CHOOSE(SELF.Keys.NoCase AND IsString(SELF.Keys.Fields.Field),'UPPER('&FB&')',FB))
      ADD(SELF.Keys.Fields)
    END

! Add a record to a file. Make sure record is valid and perform any
! required autoincrement operations.
! This function will return upon an error setting lasterror
FileManager.TryInsert PROCEDURE
  CODE
    RETURN SELF.InsertServer(0)

! Add a record to a file. Make sure record is valid and perform any
! required autoincrement operations.
! HandleError state hinges on a parameter
FileManager.InsertServer PROCEDURE(BYTE HandleError)
I SIGNED,AUTO
  CODE
    IF SELF.UseFile()<>Level:Benign THEN RETURN Level:Fatal.
    IF SELF.ValidateRecord()
      RETURN Level:Notify
    END
    SELF.File{PROP:IPRequestCount} = 0
    IF SELF.HasAutoInc
      IF SELF.AutoIncDone
        PUT(SELF.File)
      ELSE
        IF HandleError AND SELF.PrimeAutoInc() OR ~HandleError AND SELF.TryPrimeAutoInc()
          RETURN Level:Notify
        END
      END
    ELSE
      ADD(SELF.File)
    END
    CASE ERRORCODE()
    OF NoError
      SELF.AutoIncDone = 0
      RETURN Level:Benign
    OF DupKeyErr
      IF HandleError
        IF ~SELF.HasAutoInc
          GET(SELF.File,0)      ! Flag for DUPLICATE function
        END
        LOOP I = 1 TO RECORDS(SELF.Keys)
          GET(SELF.Keys,I)
          IF DUPLICATE(SELF.Keys.Key)
            SELF.Errors.SetKey(CHOOSE(CLIP(SELF.Keys.Description)<>'',CLIP(SELF.Keys.Description),SELF.Keys.Key{PROP:NAME}))
            SELF.ThrowMessage(Msg:DuplicateKey,SELF.Keys.Description)
            RETURN Level:Notify
          END
        END
      ELSE
        SELF.SetError(Msg:DuplicateKey)
      END
    ELSE
      SELF.SetError(Msg:AddFailed)
      IF HandleError
        RETURN SELF.Throw()
      END
    END
    RETURN Level:Notify


! Cancel an autoincrement operation if there was one
FileManager.CancelAutoInc PROCEDURE(<RelationManager R>)
RetVal BYTE(Level:Benign)
  CODE
  IF SELF.UseFile()=Level:Benign
    IF SELF.HasAutoInc AND SELF.AutoIncDone
      IF R &= NULL
        RetVal = SELF.DeleteRecord(False)
      ELSE
        RetVal = R.Delete(0)
      END
      IF RetVal=Level:Benign
        SELF.AutoIncDone = 0
      END
    END
  END
  RETURN RetVal

! This routine primes the auto increment fields of a record
! A key part of the specification is that fields values
! (other than autoincrement ones) are -not- corrupted.
FileManager.PrimeAutoIncServer PROCEDURE(BYTE HandleError)
I SHORT,AUTO
SaveKeys CSTRING(2000),AUTO
NewKeys  CSTRING(2000),AUTO
SaveRec  USHORT,AUTO
AutoVal  ANY
AutoIncField ANY
Attempts BYTE(0)
  CODE
    IF SELF.UseFile()<>Level:Benign THEN RETURN Level:Fatal.
    IF NOT SELF.HasAutoInc OR SELF.AutoIncDone
      RETURN Level:Benign
    END
    SELF.File{PROP:IPRequestCount} = 0
    LOOP
      LOOP I = 1 TO RECORDS(SELF.Keys)
        GET(SELF.Keys,I)
        IF SELF.Keys.AutoInc
          SELF.Errors.SetKey(CHOOSE(CLIP(SELF.Keys.Description)<>'',CLIP(SELF.Keys.Description),SELF.Keys.Key{PROP:NAME}))
! For each key with autoincrement try to find a valid new component value
          SaveRec = SELF.SaveBuffer()
          NOMEMO(SELF.File)
          IF RECORDS(SELF.Keys.Fields) = 1
            GET(SELF.Keys.Fields,1)
            AutoIncField &= SELF.Keys.Fields.Field
            SET(SELF.Keys.Key)
            IF SELF.Keys.Fields.Ascend
              PREVIOUS(SELF.File)
            ELSE
              NEXT(SELF.File)
            END
            CASE ERRORCODE()
            OF NoError
              AutoVal = AutoIncField + 1
            OF BadRecErr
              AutoVal = 1
            ELSE
              SELF.SetError(Msg:AbortReading)
              IF HandleError
                RETURN SELF.Throw()
              ELSE
                RETURN Level:Notify
              END
            END
          ELSE
            ConcatGetComponents(SELF.Keys.Fields,SaveKeys,SELF.Keys.AutoInc-1)
            GET(SELF.Keys.Fields,SELF.Keys.AutoInc)
            AutoIncField &= SELF.Keys.Fields.Field
            CLEAR(AutoIncField,1)
            SET(SELF.Keys.Key,SELF.Keys.Key)
            IF SELF.Keys.Fields.Ascend
              PREVIOUS(SELF.File)
            ELSE
              NEXT(SELF.File)
            END
            CASE ERRORCODE()
            OF NoError
              ConcatGetComponents(SELF.Keys.Fields,NewKeys,SELF.Keys.AutoInc-1)
              IF SaveKeys = NewKeys
                AutoVal = AutoIncField + 1
              ELSE
                AutoVal = 1
              END
            OF BadRecErr
              AutoVal = 1
            ELSE
              SELF.SetError(Msg:AbortReading)
              IF HandleError
                RETURN SELF.Throw()
              ELSE
                RETURN Level:Notify
              END
            END
          END
          SELF.RestoreBuffer(SaveRec)
          AutoIncField = AutoVal
        END
      END
      ADD(SELF.File)
      IF ERRORCODE()
        Attempts += 1
      ! This probably means someone else got there first
      ! In a 'heavy' usage network a number > 3 might be useful
      ! although the machine will go 'dead' for longer
        IF Attempts = 3
           IF ERRORCODE()=DupKeyErr
              SELF.SetError(Msg:RetryAutoInc)
              LOOP I = 1 TO RECORDS(SELF.Keys)
                GET(SELF.Keys,I)
                IF DUPLICATE(SELF.Keys.Key)
                  SELF.Errors.SetKey(CHOOSE(CLIP(SELF.Keys.Description)<>'',CLIP(SELF.Keys.Description),SELF.Keys.Key{PROP:NAME}))
                  BREAK
                END
              END
           ELSE
              SELF.SetError(Msg:RetryAutoInc)
           END
           SELF.SetError(Msg:RetryAutoInc)
           IF HandleError
             IF ~Self.Throw()
               Attempts = 0
               CYCLE
             END
           END
           RETURN Level:Notify
        END
      ELSE
        SELF.AutoIncDone = 1
        RETURN Level:Benign
      END
    END
?   ASSERT(0,'Coding error, this point should not be reachable')

FileManager.PrimeAutoInc PROCEDURE
  CODE
    RETURN SELF.PrimeAutoIncServer(1)

FileManager.TryPrimeAutoInc PROCEDURE
  CODE
    RETURN SELF.PrimeAutoIncServer(0)

FileManager.GetAutoIncDone PROCEDURE()
  CODE
    RETURN SELF.AutoIncDone
FileManager.BindFields PROCEDURE
  CODE
    BIND(SELF.Buffer)

FileManager.ClearKey     PROCEDURE(KEY K,BYTE LowComp,BYTE HighComp,BYTE High)
I BYTE,AUTO
  CODE
    SELF.SetKey(K)
    LOOP I = LowComp TO HighComp
      GET(SELF.Keys.Fields,I)
      IF ERRORCODE()
        RETURN
      END
      IF SELF.Keys.Fields.Ascend XOR High
        CLEAR(SELF.Keys.Fields.Field,-1)
      ELSE
        CLEAR(SELF.Keys.Fields.Field,1)
      END
    END

! Close errors are considered unimportant so this routine simply
! glosses over them
FileManager.Close PROCEDURE

  CODE
    SELF.InClose += 1
    FilesManager.NoteClose(SELF)
    IF SELF.UseMRP
       SELF.UseMRP = False
       SELF.File{PROP:IPRequestCount} = 0
    END
    IF SELF.Opened
      SELF.Opened -= 1
      IF ~SELF.Opened
        IF SELF.Initialized
           SELF.Opener.CloseFile()
        END
        SELF.Used = False
      END
    END
    SELF.InClose -= 1
  RETURN Level:Benign


FileManager.IsOpened PROCEDURE

  CODE
  RETURN CHOOSE(SELF.Initialized AND SELF.Opened > 0, True, False)


FileManager.GetOpened PROCEDURE()
  CODE
  RETURN SELF.Opened


FileManager.GetComponents PROCEDURE(KEY K)
  CODE
    SELF.SetKey(K)
    RETURN RECORDS(SELF.Keys.Fields)

FileManager.GetError PROCEDURE
  CODE
    RETURN SELF.LastError

! Scan through list of key information and select information for given key
FileManager.SetKey PROCEDURE(KEY K)
I UNSIGNED,AUTO
  CODE
    IF SELF.UseFile()<>Level:Benign THEN RETURN.
    LOOP I = 1 TO RECORDS(SELF.Keys)
      GET(SELF.Keys,I)
    UNTIL SELF.Keys.Key &= K
?   ASSERT(SELF.Keys.Key &= K,'Key does not exist in this file')

FileManager.GetField PROCEDURE(KEY K,BYTE B)
  CODE
    SELF.SetKey(K)
    GET(SELF.Keys.Fields,B)
?   ASSERT(~ERRORCODE(),'Key does not have component of this number :'&B)
    RETURN SELF.Keys.Fields.Field

FileManager.GetFieldName PROCEDURE(KEY K,BYTE B)
  CODE
    SELF.SetKey(K)
    GET(SELF.Keys.Fields,B)
?   ASSERT(~ERRORCODE(),'Key does not have component of this number :'&B)
    RETURN SELF.Keys.Fields.FieldName

FileManager.GetFieldAscending PROCEDURE(KEY K,BYTE B)
RetVal BYTE,AUTO
  CODE
    SELF.SetKey(K)
    GET(SELF.Keys.Fields,B)
?   ASSERT(~ERRORCODE(),'Key does not have component of this number :'&B)
    RetVal = SELF.Keys.Fields.Ascend
    RETURN RetVal

FileManager.GetFieldName PROCEDURE(*? Fld)
FNum LONG,AUTO
  CODE
    FNum = WHERE(SELF.Buffer,Fld)
    RETURN WHO(SELF.Buffer,FNum)

! The semantic for a failed get is simply to clear down the buffer
FileManager.Fetch PROCEDURE(KEY Key)
ReturnCode BYTE,AUTO
  CODE
    ReturnCode = SELF.TryFetch(Key)
    IF ReturnCode
      CLEAR(SELF.File)
    END
    RETURN ReturnCode

FileManager.TryFetch PROCEDURE(KEY Key)
  CODE
    IF SELF.UseFile()<>Level:Benign THEN RETURN Level:Fatal.
?   SELF.SetKey(Key)        ! Simply an assert if the key is not in the file
    GET(SELF.File,Key)
    IF ERRORCODE()
      RETURN Level:Notify
    ELSE
      RETURN Level:Benign
    END

FileManager.GetName PROCEDURE
  CODE
    IF SELF.FileName &= NULL
      IF SELF.FileNameValue
        RETURN CLIP(SELF.FileNameValue)
      END
      IF SELF.File &= NULL
        RETURN 'Unknown File'
      END
      RETURN NAME(SELF.File)
    END
    RETURN CLIP(SELF.FileName)

FileManager.GetEOF PROCEDURE
  CODE
    RETURN SELF.AtEOF

FileManager.SetErrors PROCEDURE(ErrorClass E)
  CODE
    SELF.Errors &= E


FileManager.Init PROCEDURE

  CODE
  SELF.Opened = 0
  SELF.LazyOpen = True                            !Default behaviour is Lazy Open enabled
  SELF.Killed = False
  SELF.UseMRP = False
  SELF.MRPSize = 10
  FilesManager.AddFileMapping(SELF)



FileManager.Init PROCEDURE(File File, ErrorClass E)

  CODE
    SELF.SetErrors(E)
    SELF.File &= File
    SELF.Opener &= CreateFileOpenServer(SELF.File)
    SELF.Keys &= NEW FileKeyQueue
    SELF.Buffers &= NEW BufferQueue
    SELF.Saved &= NEW SaveQueue
    SELF.Fields &= NEW FieldsList
    SELF.SkipHeldRecords = 0
    IF NOT SELF.OpenModeOverridden
       SELF.OpenMode = 42H
    END
    SELF.Create = 1
    SELF.LockRecover = 10
    SELF.PrimaryKey = 0
    SELF.FileNameValue = SELF.File{PROP:Name}
    SELF.CleanedUp = False
    SELF.Initialized = true
    SELF.Killed = False
    SELF.UseMRP = False
    SELF.MRPSize = 20
    CALLBACK(File, SELF.FileCallBackInterface)



FileManager.AddField PROCEDURE(ASTRING Tag, *? Fld, ASTRING fType, <ASTRING fPicture>)

  CODE
  IF SELF.FindField(Tag) THEN RETURN Level:Notify.  !Field with this tag already exists
  CLEAR(SELF.Fields)
  SELF.Fields.Tag = UPPER(Tag)
  SELF.Fields.Fld &= Fld
  SELF.Fields.fType = fType
  SELF.Fields.fPicture = CHOOSE(OMITTED(5) = True, '', fPicture)
  ADD(SELF.Fields, SELF.Fields.Tag)
  RETURN CHOOSE(ERRORCODE() = 0, Level:Benign, Level:Notify)


FileManager.FindField PROCEDURE(ASTRING Tag)

  CODE
  SELF.Fields.Tag = UPPER(Tag)
  GET(SELF.Fields, SELF.Fields.Tag)
  RETURN CHOOSE(ERRORCODE() = 0, True, False)


FileManager.GetField PROCEDURE(ASTRING Tag)

RVal    ANY,AUTO

  CODE
  IF SELF.FindField(Tag)
    RVal &= SELF.Fields.Fld
  ELSE
    RVal &= NULL
  END
  RETURN RVal


FileManager.GetFields PROCEDURE

  CODE
  RETURN RECORDS(SELF.Fields)


FileManager.GetField PROCEDURE(LONG Idx, *ASTRING Tag, *? Fld)

RVal  BYTE(Level:Notify)

  CODE
  IF INRANGE(Idx, 1, RECORDS(SELF.Fields))
    GET(SELF.Fields, Idx)
    IF ~ERRORCODE()
      Tag = SELF.Fields.Tag
      Fld &= SELF.Fields.Fld
      RVal = Level:Benign
    END
  END
  RETURN RVal


FileManager.GetFieldType PROCEDURE(ASTRING Tag)

  CODE
  RETURN CHOOSE(SELF.FindField(Tag) = True, SELF.Fields.fType, '')


FileManager.GetFieldPicture PROCEDURE(ASTRING Tag)

  CODE
  RETURN CHOOSE(SELF.FindField(Tag) = True, SELF.Fields.fPicture, '')


FileManager.KeyToOrder PROCEDURE(KEY K,BYTE B)
I BYTE,AUTO
RetVal CSTRING(512)
  CODE
    IF NOT K &= NULL
      IF ~B
        B = 1
      END
      SELF.SetKey(K)
      LOOP I = B TO RECORDS(SELF.Keys.Fields)
        GET(SELF.Keys.Fields,I)
        RetVal = RetVal & CHOOSE(RetVal = '','',',') & CHOOSE(SELF.Keys.Fields.Ascend <> 0,'','-') & SELF.Keys.Fields.FieldName
      END
    END
    RETURN RetVal

FileManager.CleanUp PROCEDURE
I SIGNED,AUTO
J SIGNED,AUTO
K SIGNED,AUTO
  CODE
    IF SELF.CleanedUp THEN RETURN.
    SELF.CleanedUp = TRUE
    LOOP WHILE SELF.Opened>0
         SELF.Close()
    END
    FilesManager.RemoveFileMapping(SELF)

    LOOP j = 1 TO RECORDS(SELF.Keys)
      GET(SELF.Keys, j)
      LOOP k = 1 TO RECORDS(SELF.Keys.Fields)
        GET(SELF.Keys.Fields, k)
        DISPOSE(SELF.Keys.Fields.FieldName)
        SELF.Keys.Fields.Field &= NULL
      END
      DISPOSE(SELF.Keys.Fields)
    END
    DISPOSE(SELF.Keys)
    IF NOT SELF.PreviousBuffer &= NULL
       DISPOSE(SELF.PreviousBuffer)
    END
    IF ~SELF.Buffers &= NULL
      LOOP I = 1 TO RECORDS(SELF.Buffers)
        GET(SELF.Buffers,I)
        DISPOSE(SELF.Buffers.Buffer)
      END
      DISPOSE(SELF.Buffers)
    END
    LOOP I = 1 TO RECORDS(SELF.Saved)
      GET(SELF.Saved, I)
      FREESTATE(SELF.File, SELF.Saved.State)
    END
    DISPOSE(SELF.Saved)
    IF ~SELF.Fields &= NULL
      LOOP I = 1 TO RECORDS(SELF.Fields)
        GET(SELF.Fields,I)
        SELF.Fields.Fld &= NULL
      END
      DISPOSE(SELF.Fields)
    END
    SELF.Opener.Release()
    SELF.Opener &= NULL
    SELF.FileName &= NULL
    SELF.OpenModeOverridden = False

FileManager.Kill PROCEDURE
  CODE
  IF SELF.Initialized
    IF NOT SELF.Killed
       SELF.CleanUp()
       IF NOT SELF.File &= NULL
         CALLBACK(SELF.File, SELF.FileCallBackInterface,TRUE)
       END
       SELF.Killed = True
       SELF.Initialized = False
    END
  END

FileManager.NextServer PROCEDURE(BYTE HandleError,BYTE Prev)
  CODE
    IF SELF.UseFile()<>Level:Benign THEN RETURN Level:Fatal.
    LOOP
      SELF.AtEOF = 0
      IF SELF.UseMRP
         IF SELF.File{PROP:IPRequestCount} = 0
            SELF.File{PROP:IPRequestCount} = SELF.MRPSize
         END
      END
      IF Prev
        PREVIOUS(SELF.File)
      ELSE
        NEXT(SELF.File)
      END
      CASE ERRORCODE()
      OF BadRecErr
        SELF.AtEOF = 1
        RETURN Level:Notify
      OF NoError
        RETURN Level:Benign
      OF IsHeldErr
        IF SELF.SkipHeldRecords
          CYCLE
        ELSE
          SELF.SetError(Msg:RecordHeld)
          IF HandleError
            RETURN SELF.Throw()
          ELSE
            RETURN Level:Notify
          END
        END
      ELSE
        SELF.SetError(Msg:AbortReading)
        IF HandleError
          RETURN SELF.Throw()
        ELSE
          RETURN Level:Fatal
        END
      END
    END

FileManager.Next PROCEDURE
  CODE
    RETURN SELF.NextServer(1)

FileManager.TryNext PROCEDURE
  CODE
    RETURN SELF.NextServer(0)

! This function 'clears' a record and performs auto-inc function where
! applicable.
FileManager.PrimeRecord PROCEDURE(BYTE SC)
BufferHandle  USHORT,AUTO
BH2           USHORT,AUTO
Result        BYTE,AUTO
  CODE
    IF SELF.UseFile()<>Level:Benign THEN RETURN Level:Fatal.
    IF SELF.AliasedFile&=NULL
      IF ~SC
        CLEAR(SELF.File)
      END
      SELF.PrimeFields
      IF Self.HasAutoInc
        SELF.AutoIncDone = 0
        Result = SELF.PrimeAutoInc()
      ELSE
        Result = Level:Benign
      END
    ELSE
      SELF.AliasedFile.Open()
      BufferHandle=SELF.AliasedFile.SaveFile()
      IF SC
        BH2 = SELF.SaveBuffer()
        SELF.AliasedFile.RestoreBuffer(BH2,SELF)          ! Prime 'junk' into new record
      END
      Result=SELF.AliasedFile.PrimeRecord(SC)
      IF ~Result                                          !~Result == Level:Benign
        BH2 = SELF.AliasedFile.SaveFile()
        SELF.RestoreFile(BH2,SELF.AliasedFile)      ! Aliased file has 'full' primary characteristics restored
      END
      SELF.AliasedFile.RestoreFile(BufferHandle)
      SELF.AliasedFile.Close()
    END
    RETURN Result


FileManager.PrimeFields PROCEDURE
  CODE


FileManager.OpenServer PROCEDURE(BYTE HandleError,BYTE ForceOpen)

RVal BYTE,AUTO

  CODE
    FilesManager.NoteOpen(SELF)
    COMPILE('***',TraceFiles)
    IF TraceFiles
      FilesManager.Trace('Open'&CHOOSE(HandleError=1,'(Errors):',':')&SELF.GetName())
    END
    ***
    SELF.BindFields
    IF ForceOpen OR ~SELF.LazyOpen AND ~SELF.Opened
      RVal = SELF.OpenFile(HandleError)
      IF RVal
        RETURN RVal
      END
    END
    SELF.Opened += 1
    RETURN Level:Benign

! Create and Open mode are public fields of the file manager class
! which should be 'defaulted' in derived classes
! In the handled-error case the function will create or build a file
! in an attempt to get things working
FileManager.OpenFile PROCEDURE(BYTE HandleError)

Err UNSIGNED,AUTO

  CODE
    IF SELF.UseFile(UseType:Initialize) <> Level:Benign             ! File manager must now be initialized
      do HardError
    END
    COMPILE('***',TraceFiles)
    FilesManager.Trace('Open File:'&SELF.GetName())
    ***
    LOOP 2 TIMES  ! Allow 1 create or build cycle
      Err = SELF.Opener.OpenFile(SELF.OpenMode)
      IF Err = NoAccessErr
        SEND(SELF.File, 'Recover=' & SELF.LockRecover)
        Err = SELF.Opener.OpenFile(SELF.OpenMode)
      END
      CASE Err
      OF NoError ! OROF IsOpenErr
        SELF.Used=True
        SELF.AtEOF = 0
        SELF.AutoIncDone = 0
        RETURN Level:Benign
      OF RecordLimitErr
        IF BAND(SELF.OpenMode,3)
          SELF.OpenMode = BAND(SELF.OpenMode,0F0H)
          SELF.Throw(Msg:RecordLimit)
          CYCLE
        END
      OF NoAccessErr
        IF BAND(SELF.OpenMode,3)
          SELF.OpenMode = BAND(SELF.OpenMode,0F0H)
          SELF.Throw(Msg:AccessDenied)
          CYCLE
        END
      OF NoFileErr
        IF SELF.Create
          CREATE(SELF.File)
          IF ERRORCODE()
            SELF.SetError(Msg:CreateFailed)
            DO AnError
          ELSE
            CYCLE
          END
        END
      OF InvalidFileErr
        SELF.SetError(Msg:InvalidFile)
        DO AnError
      OF BadKeyErr
        SELF.SetError(Msg:RebuildKey)
        IF HandleError
          SELF.Throw
          BUILD(SELF.File)
          IF ERRORCODE()
            RETURN SELF.Throw(Msg:RebuildFailed)
          ELSE
            CYCLE
          END
        ELSE
          RETURN Level:Notify
        END
      END
    UNTIL 1
    DO HardError

HardError ROUTINE
    SELF.SetError(Msg:OpenFailed)
    DO AnError

AnError ROUTINE
    IF HandleError
      RETURN SELF.Throw()
    END
    RETURN Level:Notify

FileManager.TryOpen PROCEDURE()
  CODE
    RETURN SELF.OpenServer(0)

FileManager.Open PROCEDURE()
  CODE
    RETURN SELF.OpenServer(1)

!This procedure forces the file open if it has already be 'opened' as a LazyFile
FileManager.UseFile PROCEDURE(BYTE Usage)
i       BYTE(Level:Benign)
SaveID  USHORT,AUTO
  CODE
    SELF.InUseFile = TRUE
    IF Usage ~= UseType:Initialize              ! Initialization should be done in derived class, nothing to do here
      IF Usage <> UseType:Benign
        FilesManager.NoteUsage(SELF,Usage)
      END
      IF SELF.LazyOpen
        IF ~SELF.Used
          IF SELF.Opened < 1                    ! File not previously OPENed with explicit .Open or .TryOpen call
            SELF.InUseFile = FALSE
            RETURN SELF.Throw(Msg:UseClosedFile)
          END

          SaveID=SELF.SaveBuffer()              ! File buffer saved so that SET(,key) etc work ok after real open
          i=SELF.OpenFile(1)                    ! if driver corrupts buffer when OPENed
          SELF.RestoreBuffer(SaveID,1)
        END
      END
    ELSE
      !Explicit call to Access:.UseFile in code
      IF SELF.LazyOpen
         IF ~SELF.Used
            IF SELF.Opened < 1                    ! File not previously OPENed with explicit .Open or .TryOpen call
               SELF.Opened += 1
               SELF.OpenServer(1,1)
               SELF.Opened -= 1
            END
         ELSE
            IF SELF.Opened < 1
               SELF.OpenServer(1,0)
            END
         END
      END
    END
    SELF.InUseFile = FALSE
    RETURN i


FileManager.Position PROCEDURE
  CODE
    IF SELF.UseFile()<>Level:Benign THEN RETURN ''.
    IF SELF.PrimaryKey
      GET(SELF.Keys,SELF.PrimaryKey)
      RETURN POSITION(SELF.Keys.Key)
    ELSE
      RETURN POSITION(SELF.File)
    END


FileManager.Previous PROCEDURE
  CODE
    RETURN SELF.NextServer(1,1)


FileManager.TryPrevious PROCEDURE
  CODE
    RETURN SELF.NextServer(0,1)

FileManager.Update PROCEDURE
  CODE
    RETURN SELF.UpdateServer(1)


FileManager.TryUpdate PROCEDURE
  CODE
    RETURN SELF.UpdateServer(0)


! In the handled-error case the buffer is re-freshed if PUT encounters
! a WATCH violation
FileManager.UpdateServer PROCEDURE(BYTE HandleError)
Hold  STRING(1024),AUTO
  CODE
    IF SELF.UseFile()<>Level:Benign THEN RETURN Level:Fatal.
    IF SELF.ValidateRecord()
      RETURN Level:User
    END
    Hold = SELF.Position()
    PUT(SELF.File)
    CASE ERRORCODE()
    OF NoError
      RETURN Level:Benign
    OF RecordChangedErr
      SELF.SetError(CHOOSE(HandleError = 2,Msg:ConcurrencyFailedFromForm,Msg:ConcurrencyFailed))
      IF HandleError
        SELF.Throw
      END
      WATCH(SELF.File)
      SELF.TryReget(Hold)
    ELSE
      SELF.SetError(Msg:PutFailed)
      IF HandleError
        RETURN SELF.Throw()
      END
    END
    RETURN Level:Notify


FileManager.TryReget PROCEDURE(STRING P)
  CODE
    IF SELF.UseFile()<>Level:Benign THEN RETURN Level:Fatal.
    IF SELF.PrimaryKey
      GET(SELF.Keys,SELF.PrimaryKey)
      REGET(SELF.Keys.Key,P)
    ELSE
      REGET(SELF.File,P)
    END
    RETURN CHOOSE(ERRORCODE() = 0,Level:Benign,Level:Notify)

! Regresses a buffer (and memos) to the value from handle Id.
! The handle is invalidated by this option
FileManager.RestoreBuffer PROCEDURE(*USHORT Id,BYTE DoRestore)
  CODE
    SELF.RestoreBuffer(Id,SELF,DoRestore)

FileManager.RestoreBuffer PROCEDURE(*USHORT Id,FileManager Frm, BYTE DoRestore)
i SHORT,AUTO
Memos UNSIGNED,AUTO
  CODE
    IF ~Id AND ~DoRestore THEN RETURN .       ! Effectively DISPOSE(Null)
    Frm.Buffers.Id = Id
    GET(Frm.Buffers,Frm.Buffers.Id)
?   ASSERT(~ERRORCODE(),'Restore handle is invalid')
    IF DoRestore
      SELF.Buffer = Frm.Buffers.Buffer
    END
    DO KillBuffer
    Memos = SELF.File{PROP:Memos}
    LOOP I = 1 TO Memos
      Frm.Buffers.Id = Id + I
      GET(Frm.Buffers,Frm.Buffers.Id)
?     ASSERT(~ERRORCODE(),'Restore handle is invalid (memo does not appear to have been stored)')
      IF DoRestore
        SELF.File{PROP:Value,-I} = Frm.Buffers.Buffer
      END
      DO KillBuffer
    END
    Frm.Buffers.Id = Id + Memos + 1
    GET(Frm.Buffers,Frm.Buffers.Id)
?   ASSERT(~ERRORCODE(),'Restore handle is invalid (nullity information not appear to have been stored)')
    SETNULLS(SELF.File,Frm.Buffers.Buffer)
    DO KillBuffer
    Id = 0

KillBuffer ROUTINE
    DISPOSE(Frm.Buffers.Buffer)
    DELETE(Frm.Buffers)

FileManager.EqualBuffer PROCEDURE(*USHORT Id)
I SHORT,AUTO
  CODE
    IF SELF.UseFile()<>Level:Benign THEN RETURN 0.        ! Is this needed? Should a buffer compare require an open file?
    IF SELF.BlobChanged THEN RETURN 0.      ! A blob has changed so the buffers are unequal
    SELF.Buffers.Id = Id
    GET(SELF.Buffers,SELF.Buffers.Id)
?   ASSERT(~ERRORCODE(),'Restore handle is invalid')
    IF SELF.Buffer <> SELF.Buffers.Buffer THEN RETURN 0.
    LOOP I = 1 TO SELF.File{PROP:Memos}
      SELF.Buffers.Id = Id + I
      GET(SELF.Buffers,SELF.Buffers.Id)
?     ASSERT(~ERRORCODE(),'Restore handle is invalid (memo does not appear to have been stored)')
      IF SELF.File{PROP:Value,-I} <> SELF.Buffers.Buffer THEN RETURN 0.
    END
    RETURN 1


FileManager.RestoreFile PROCEDURE(*USHORT Id,BYTE DoRestore)
  CODE
    SELF.RestoreFile(Id,SELF,DoRestore)

FileManager.RestoreFile PROCEDURE(*USHORT Id,FileManager Frm,BYTE DoRestore)
K &KEY
  CODE
    IF SELF.UseFile()=Level:Benign
      Frm.Saved.Id = Id
      GET(Frm.Saved,Frm.Saved.Id)
?     ASSERT(~ERRORCODE(),'Restore handle is invalid')
      IF DoRestore
        IF SELF.AliasedFile &= NULL
           RESTORESTATE(SELF.File,Frm.Saved.State)
        ELSE
          IF SELF.PrimaryKey
            GET(SELF.Keys,Frm.PrimaryKey)
            RESET(SELF.Keys.Key,Frm.Saved.Pos)
          ELSE
            RESET(SELF.File,Frm.Saved.Pos)
          END
          NEXT(SELF.File)
        END
      END
      SELF.RestoreBuffer(Frm.Saved.Buffer,Frm,DoRestore)
      IF DoRestore
        SELF.AutoIncDone = Frm.Saved.AutoIncDone
      END
      FREESTATE(SELF.File, Frm.Saved.State)
      DELETE(Frm.Saved)
      Id = 0
    END

FileManager.SetError PROCEDURE(USHORT Err)
  CODE
    SELF.Errors.SetErrors
    SELF.LastError = Err


FileManager.SetName PROCEDURE(STRING st)
  CODE
?   ASSERT(~ (SELF.FileName &= NULL),'Cannot perform SetName unless filename is a variable')
    SELF.FileName = st


FileManager.SaveBuffer PROCEDURE
Id LONG,AUTO
I  BYTE,AUTO
Memos UNSIGNED,AUTO
  CODE
    Id = RECORDS(SELF.Buffers)
    IF Id
      GET(SELF.Buffers,Id)
?     ASSERT(~ERRORCODE(),'Saved buffer storage corrupt')
      Id = SELF.Buffers.Id + 1
    ELSE
      Id = 1
    END
    SELF.Buffers.Id = Id
    SELF.Buffers.Buffer &= DupString(SELF.Buffer)
    ADD(SELF.Buffers,SELF.Buffers.Id)
?   ASSERT(~ERRORCODE(),'Unable to save new buffer contents')
    Memos = SELF.File{PROP:Memos}
    LOOP I = 1 TO Memos
      SELF.Buffers.Id = Id + I
      SELF.Buffers.Buffer &= DupString(SELF.File{PROP:Value,-I})
      ADD(SELF.Buffers,SELF.Buffers.Id)
?     ASSERT(~ERRORCODE(),'Unable to save new buffer contents of memo')
    END
    SELF.Buffers.Id = Id + Memos + 1
    SELF.Buffers.Buffer &= DupString(GETNULLS(SELF.File)) ! Unsupported case handled with null string
    ADD(SELF.Buffers,SELF.Buffers.Id)
    RETURN Id

FileManager.SaveFile PROCEDURE
Id LONG,AUTO
  CODE
    IF SELF.UseFile()<>Level:Benign THEN RETURN 0.
    Id = RECORDS(SELF.Saved)
    IF Id
      GET(SELF.Saved,Id)
?     ASSERT(~ERRORCODE(),'Saved file storage corrupt')
      Id = SELF.Saved.Id + 1
    ELSE
      Id = 1
    END
    SELF.Saved.Id = Id
    SELF.Saved.Buffer = SELF.SaveBuffer()
    SELF.Saved.State = GETSTATE(SELF.File)
    IF SELF.PrimaryKey
      GET(SELF.Keys,SELF.PrimaryKey)
      SELF.Saved.Pos = POSITION(SELF.Keys.Key)
    ELSE
      SELF.Saved.Pos = POSITION(SELF.File)
    END
    SELF.Saved.AutoIncDone = SELF.AutoIncDone
    ADD(SELF.Saved)
    RETURN Id

FileManager.Throw PROCEDURE
  CODE
    SELF.Errors.SetFile(SELF.GetName())
    RETURN SELF.Errors.TakeError(SELF.LastError)

FileManager.Throw PROCEDURE(USHORT ErrNum)
  CODE
    SELF.SetError(ErrNum)
    RETURN SELF.Throw()

FileManager.ThrowMessage PROCEDURE(USHORT ErrNum,STRING Me)
  CODE
    SELF.Errors.SetFile(SELF.GetName())
    RETURN SELF.Errors.ThrowMessage(ErrNum,Me)


FileManager.TryValidateField PROCEDURE(UNSIGNED Field)
  CODE
    RETURN SELF.ValidateFieldServer(Field, 0)


FileManager.ValidateField PROCEDURE(UNSIGNED Field)
  CODE
    RETURN SELF.ValidateFieldServer(Field, 1)


FileManager.ValidateFieldServer PROCEDURE(UNSIGNED Field,BYTE HandleErrors)
BufferHandle  USHORT,AUTO
RVal          BYTE(Level:Benign)
  CODE
    IF ~SELF.AliasedFile&=NULL
?     ASSERT(SELF.AliasedFile.AliasedFile&=NULL,'Cannot have alias based upon an alias')
      BufferHandle=SELF.AliasedFile.SaveBuffer()
      SELF.AliasedFile.Buffer=SELF.Buffer
      RVal=SELF.AliasedFile.ValidateField(Field)
      SELF.AliasedFile.RestoreBuffer(BufferHandle)
    END
    RETURN RVal


FileManager.ValidateFields PROCEDURE(UNSIGNED Low,UNSIGNED High,<*UNSIGNED U>)
BufferHandle  USHORT,AUTO
I             UNSIGNED,AUTO
RetVal        BYTE(Level:Benign)
  CODE
    IF SELF.AliasedFile&=NULL
      LOOP I = Low TO High
        RetVal = SELF.ValidateField(I)
        IF RetVal
          IF ~OMITTED(4)
            U = I
          END
          BREAK
        END
      END
    ELSE
?     ASSERT(SELF.AliasedFile.AliasedFile&=NULL,'Cannot have alias based upon an alias')
      RetVal=SELF.AliasedFile.UseFile()
      IF RetVal<>Level:Benign THEN RETURN RetVal.
      BufferHandle=SELF.AliasedFile.SaveBuffer()
      SELF.AliasedFile.Buffer=SELF.Buffer
      RetVal=SELF.AliasedFile.ValidateFields(Low,High,U)
      SELF.AliasedFile.RestoreBuffer(BufferHandle)
    END
    RETURN RetVal


FileManager.ValidateRecord PROCEDURE(<*UNSIGNED U>)
BufferHandle  USHORT,AUTO
Result        BYTE,AUTO
  CODE
    IF SELF.AliasedFile&=NULL
      Result=SELF.ValidateFields(1,SELF.File{PROP:Fields},U)
    ELSE
?     ASSERT(SELF.AliasedFile.AliasedFile&=NULL,'Cannot have alias based upon an alias')
      Result=SELF.AliasedFile.UseFile()
      IF Result<>Level:Benign THEN RETURN Result.
      BufferHandle=SELF.AliasedFile.SaveBuffer()
      SELF.AliasedFile.Buffer=SELF.Buffer
      Result=SELF.AliasedFile.ValidateRecord(U)
      SELF.AliasedFile.RestoreBuffer(BufferHandle)
    END
    RETURN Result

FileManager.DeleteRecord PROCEDURE(BYTE Query)
  CODE
  IF Query AND SELF.Throw(Msg:ConfirmDelete) <> Level:Benign
      RETURN Level:Cancel
  END
  DELETE(SELF.File)
  IF ERRORCODE() THEN
     SELF.SetError(Msg:DeleteFailed)
     RETURN(Level:Notify)
  ELSE
     RETURN(Level:Benign)
  END

FileManager.Deleted PROCEDURE
  CODE
  RETURN(Level:Benign)

FileManager.SetOpenMode PROCEDURE(BYTE OpenMode)
  CODE
  SELF.OpenMode = OpenMode
  SELF.OpenModeOverridden = True

FileManager.GetOpenMode PROCEDURE()
  CODE
  RETURN SELF.OpenMode

FileManager.SetUseMRP         PROCEDURE(BYTE UseMRP=True)
 CODE
    SELF.UseMRP = UseMRP

FileManager.GetUseMRP         PROCEDURE()
 CODE
    RETURN(SELF.UseMRP)

FileManager.SetMRPSize                      PROCEDURE(BYTE MRPSize)
 CODE
    SELF.MRPSize = MRPSize

FileManager.GetMRPSize                      PROCEDURE()
 CODE
    RETURN SELF.MRPSize

ConcatGetComponents PROCEDURE(KeyFieldQueue Fields,*CSTRING Into,BYTE HowMany)
I BYTE,AUTO
  CODE
    Into = ''
    LOOP I = 1 TO HowMany
      GET(Fields,I)
      IF ERRORCODE()
        BREAK
      END
      Into = Into & Fields.Field & '|'
    END

! Choose the buffer size for a given string
! Must be at least one byte but other than that may clip
! Then allocate string and copy in value
DupString PROCEDURE(STRING St)
SizeIs SIGNED,AUTO
NS &STRING,AUTO
  CODE
    SizeIs = LEN(CLIP(St))
    IF SizeIs = 0
      SizeIs = 1
    END
    NS &= NEW STRING (SizeIs)
    NS = St
    RETURN NS

! Dictionary definitions should not contain circular references as they
! cannot be guaranteed to work for all usages of a file scheme.
! Of course they _do_ contain circular references so we need to spot
! when we are cascading a call and get out when a cycle is hit ...
! We actually use this mechanism to side-step the 'proper' cycle of 'parent<->>child'




! Relation manager class


RelationManager.Construct PROCEDURE

  CODE
  SELF.DeferedRelationsDone = false


RelationManager.DeferedRelations PROCEDURE

  CODE
  IF ~SELF.DeferedRelationsDone               ! Ensure that the DeferedRelationships are processed only one
    SELF.DeferedAddRelations()
    SELF.DeferedRelationsDone = True
  END


RelationManager.DeferedAddRelations PROCEDURE

  CODE
  ! There is nothing to do here by default

  ! May be overridden in derived version to add relations just before the RM needs them
  ! to prevent a hit at init time.


RelationManager.AddRelation PROCEDURE(RelationManager F)
  CODE
?   ASSERT(NOT SELF.Relations &= NULL,'Relation manager incorrectly initialized')
    CLEAR(SELF.Relations)
    SELF.Relations.File &= F
    ADD(SELF.Relations)

RelationManager.AddRelation PROCEDURE(RelationManager F,BYTE Update,BYTE Delete,KEY His)
  CODE
    SELF.AddRelation(F)
    SELF.Relations.Fields &= NEW BufferedPairsClass
    SELF.Relations.Fields.Init
    SELF.Relations.UpdateMode = Update
    SELF.Relations.DeleteMode = Delete
    SELF.Relations.HisKey &= His
    PUT(SELF.Relations)

RelationManager.AddRelationLink PROCEDURE(*? left,*? right)
  CODE
?   ASSERT(NOT SELF.Relations &= NULL AND NOT SELF.Relations.Fields &= NULL,'Relation manager incorrectly initialized')
    SELF.Relations.Fields.AddPair(left,right)

RelationManager.AddRelationLink PROCEDURE(*string left,*string right)
  CODE
?   ASSERT(NOT SELF.Relations &= NULL AND NOT SELF.Relations.Fields &= NULL,'Relation manager incorrectly initialized')
    SELF.Relations.Fields.AddPair(left,right)

RelationManager.AddRelationLink PROCEDURE(*long left,*long right)
  CODE
?   ASSERT(NOT SELF.Relations &= NULL AND NOT SELF.Relations.Fields &= NULL,'Relation manager incorrectly initialized')
    SELF.Relations.Fields.AddPair(left,right)

RelationManager.CancelAutoInc PROCEDURE
  CODE
    RETURN SELF.Me.CancelAutoInc(SELF)

RelationManager.Insert PROCEDURE()
RetVal BYTE(Level:Benign)
  CODE
    IF SELF.UseLogoutOnInsert AND SELF.UseLogout
        RetVal = SELF.ForceOpen()
        DO CheckError
        IF SELF.UseLogout
          RetVal = SELF.LogoutInsert()
          DO CheckError
          LOGOUT(SELF.LogoutTimeout)
          IF ERRORCODE()
            SELF.Me.Throw(Msg:LogoutFailed)
            RetVal = Level:Notify
            DO CheckError
          END
        END
        RetVal = SELF.Me.Insert()
        IF RetVal
          IF SELF.UseLogout
            ROLLBACK
          END
          IF RetVal = Level:Notify
            SELF.Me.Throw
          END
          IF SELF.Me.LastError = Msg:ConcurrencyFailed OR SELF.Me.LastError = Msg:ConcurrencyFailedFromForm
            RetVal = Level:Notify
          ELSE
            RetVal = Level:User
          END
          DO CheckError
        END
        IF SELF.UseLogout
          COMMIT
?         ASSERT(~ERRORCODE(),'Unable to commit transaction')
        END
        SELF.Close
        RETURN RetVal
    ELSE
        RETURN SELF.Me.Insert()
    END

CheckError ROUTINE
  IF RetVal
    SELF.Close
    RETURN RetVal
  END

RelationManager.Update PROCEDURE(BYTE FromForm)
RetVal BYTE(Level:Benign)
I LONG,AUTO
  CODE
    RetVal = SELF.ForceOpen()
    DO CheckError
    IF SELF.UseLogout
      RetVal = SELF.LogoutUpdate()
      DO CheckError
      LOGOUT(SELF.LogoutTimeout)
      IF ERRORCODE()
        SELF.Me.Throw(Msg:LogoutFailed)
        RetVal = Level:Notify
        DO CheckError
      END
    END
    RetVal = SELF.Me.TryUpdate()
    IF RetVal
      IF SELF.UseLogout
        ROLLBACK
      END
      IF FromForm AND SELF.Me.LastError = Msg:ConcurrencyFailed
        SELF.Me.LastError = Msg:ConcurrencyFailedFromForm
      END
      IF RetVal = Level:Notify
        SELF.Me.Throw
      END
      IF SELF.Me.LastError = Msg:ConcurrencyFailed OR SELF.Me.LastError = Msg:ConcurrencyFailedFromForm
        RetVal = Level:Notify
      ELSE
        RetVal = Level:User
      END
      DO CheckError
    END
    SELF.DeferedRelations()
    LOOP I = 1 TO RECORDS(SELF.Relations)
      GET(SELF.Relations,I)
      IF LocalAction(SELF.Relations.UpdateMode) AND NOT SELF.Relations.Fields.EqualLeftBuffer()
        RetVal = SELF.Relations.File.UpdateSecondary(SELF.Relations.HisKey,SELF.Relations.Fields,SELF.Relations.UpdateMode)
        IF RetVal
          SELF.Relations.Fields.AssignRightToLeft
          DO CheckError
        END
      END
    END
    IF SELF.UseLogout
      COMMIT
?     ASSERT(~ERRORCODE(),'Unable to commit transaction')
    END
    SELF.Close
    RETURN RetVal

CheckError ROUTINE
  IF RetVal
    SELF.Close
    RETURN RetVal
  END

RelationManager.UpdateSecondary PROCEDURE(KEY MyKey,BufferedPairsClass Fields,BYTE Mode)
RetVal BYTE(Level:Benign)
Preserve USHORT,AUTO
  CODE
    SELF.Me.UseFile()
    Preserve = SELF.Me.SaveFile()
    CLEAR(SELF.Me.File)
    Fields.AssignBufferToRight
    SELF.Me.ClearKey(MyKey,RECORDS(Fields.List)+1)
    SET(MyKey,MyKey)
    LOOP
      IF SELF.Me.TryNext()=Level:Fatal
        IF SELF.UseLogout
          ROLLBACK  ! ROLLBACK in case LOGOUT issued in parent
        END
        SELF.Me.Throw()
        RetVal = Level:Notify
        DO Ret
      END
      IF SELF.Me.GetEOF() OR NOT Fields.EqualRightBuffer()
        DO Ret
      END
      SELF.Save
      CASE Mode
      OF RI:Restrict
        SELF.Me.SetError(Msg:RestrictUpdate)
        IF SELF.UseLogout
          ROLLBACK
        END
        SELF.Me.Throw
        Fields.AssignBufferToLeft
        RetVal = Level:Notify
        DO Ret
      OF RI:Clear
        Fields.ClearRight
      OF RI:Cascade
        Fields.AssignLeftToRight
      ELSE
?       ASSERT(0,'Invalid RI mode specified')
        DO Ret
      END
      IF SELF.CascadeUpdates()
        RetVal = Level:Notify
        DO Ret
      END
    END
?   ASSERT(0,'Coding error, this point should not have been reached')

Ret ROUTINE
  SELF.Me.RestoreFile(Preserve)
  RETURN RetVal

RelationManager.Close PROCEDURE(BYTE Cascading)
  CODE
  RETURN SELF.OpenCloseServer(Cascading,0)

RelationManager.Delete PROCEDURE(BYTE Query)
Current:Position STRING(1024)
RetVal BYTE,AUTO
I      LONG,AUTO
ChildQuery BYTE,AUTO
  CODE
    IF Query AND SELF.Me.Throw(Msg:ConfirmDelete) <> Level:Benign
      RETURN Level:Cancel
    END
    ChildQuery = False
    LOOP
      SETCURSOR(Cursor:Wait)
      RetVal = Level:Benign
      Current:Position = SELF.Me.Position()
      RetVal = SELF.ForceOpen()
      DO CheckError
      IF SELF.UseLogout
        RetVal = SELF.LogoutDelete()
        DO CheckError
        LOGOUT(SELF.LogoutTimeout)
        IF ERRORCODE()
          SELF.Me.Throw(Msg:LogoutFailed)
          RetVal = Level:Notify
          DO CheckError
        END
      END
      SELF.Me.TryReget(Current:Position)
      SELF.DeferedRelations()
      LOOP I = 1 TO RECORDS(SELF.Relations)
        GET(SELF.Relations,I)
        IF LocalAction(SELF.Relations.DeleteMode)
          RetVal = SELF.Relations.File.DeleteSecondary(SELF.Relations.HisKey,SELF.Relations.Fields,SELF.Relations.DeleteMode,ChildQuery)
          DO CheckError
        END
      END
      IF SELF.Me.DeleteRecord(False)<>Level:Benign THEN
        IF SELF.UseLogout
          ROLLBACK
        END
        SELF.Me.Throw
        RetVal = Level:Notify
      ELSE
        IF SELF.UseLogout
          COMMIT
        END
      END
      DO CloseDown
    WHILE RetVal AND SELF.Me.Throw(Msg:RetryDelete) = Level:Benign
    RETURN RetVal

CheckError ROUTINE
  IF RetVal
    DO CloseDown
    RETURN RetVal
  END

CloseDown ROUTINE
  SETCURSOR()
  SELF.Close

RelationManager.DeleteSecondary PROCEDURE(KEY MyKey,BufferedPairsClass Fields,BYTE Mode,BYTE Query=1)
RetVal BYTE(Level:Benign)
I LONG,AUTO
Preserve USHORT,AUTO
  CODE
    SELF.Me.UseFile()
    Preserve = SELF.Me.SaveFile()
    CLEAR(SELF.Me.File)
    Fields.AssignLeftToRight
    SELF.Me.ClearKey(MyKey,RECORDS(Fields.List)+1)
    SET(MyKey,MyKey)
    LOOP
      IF SELF.Me.TryNext()=Level:Fatal
        IF SELF.UseLogout
          ROLLBACK
        END
        SELF.Me.Throw()
        RetVal = Level:Notify
        DO Ret
      END
      IF SELF.Me.GetEOF() OR NOT Fields.EqualLeftRight()
        DO Ret
      END
      IF SELF.Me.Deleted() THEN CYCLE.
      CASE Mode
      OF RI:Restrict
        SELF.Me.SetError(Msg:RestrictDelete)
        IF SELF.UseLogout
          ROLLBACK
        END
        SELF.Me.Throw
        RetVal = Level:Notify
        DO Ret
      OF RI:Clear
        SELF.Save
        Fields.ClearRight
        IF SELF.CascadeUpdates()
          RetVal = Level:Notify
          DO Ret
        END
      OF RI:Cascade
        SELF.DeferedRelations()
        LOOP I = 1 TO RECORDS(SELF.Relations)
          GET(SELF.Relations,I)
          IF LocalAction(SELF.Relations.DeleteMode)
            RetVal = SELF.Relations.File.DeleteSecondary(SELF.Relations.HisKey,SELF.Relations.Fields,SELF.Relations.DeleteMode,Query)
            IF RetVal
              DO Ret
            END
          END
        END
        IF SELF.Me.DeleteRecord(Query)<>Level:Benign THEN
          IF SELF.UseLogout
            ROLLBACK
          END
          SELF.Me.Throw
          RetVal = Level:Notify
          DO Ret
        END
      ELSE
?       ASSERT(0,'Invalid RI mode')
        DO Ret
      END
    END
?   ASSERT(0,'Coding error, this point should not have been reached.')

Ret ROUTINE
  SELF.Me.RestoreFile(Preserve)
  RETURN RetVal

RelationManager.CascadeUpdates PROCEDURE
I BYTE,AUTO
  CODE
    SELF.DeferedRelations()
    LOOP I = 1 TO RECORDS(SELF.Relations)
      GET(SELF.Relations,I)
      IF LocalAction(SELF.Relations.UpdateMode) AND NOT SELF.Relations.Fields.EqualLeftBuffer()
        IF SELF.Relations.File.UpdateSecondary(SELF.Relations.HisKey,SELF.Relations.Fields,SELF.Relations.UpdateMode)
          SELF.Relations.Fields.AssignRightToLeft
          RETURN Level:Notify
        END
      END
    END
    IF SELF.Me.TryUpdate()
      IF SELF.UseLogout
        ROLLBACK
      END
      RETURN SELF.Me.Throw()
    ELSE
      RETURN Level:Benign
    END

RelationManager.Init PROCEDURE(FileManager F,BYTE Log)
  CODE
    SELF.Me           &= F
    SELF.Relations    &= NEW RelationQueue
    SELF.UseLogout     = Log
    SELF.LogoutTimeout = 2
    FilesManager.AddFileMapping(SELF)

RelationManager.Kill PROCEDURE
I UNSIGNED,AUTO
  CODE
    IF NOT SELF.Relations &= NULL
      FilesManager.RemoveRelationFileMapping(SELF)
      ! SELF.DeferedRelations()
      LOOP I = 1 TO RECORDS(SELF.Relations)
        GET(SELF.Relations,I)
        IF NOT SELF.Relations.Fields &= NULL
          SELF.Relations.Fields.Kill
          DISPOSE(SELF.Relations.Fields)
        END
      END
      DISPOSE(SELF.Relations)
    END

RelationManager.SetAlias PROCEDURE(RelationManager R)
  CODE
    SELF.AliasFile &= R

RelationManager.ListLinkingFields PROCEDURE(RelationManager Him,FieldPairsClass Trgt,BYTE RightFirst)
I UNSIGNED,AUTO
J BYTE,AUTO
  CODE
    SELF.DeferedRelations()
    LOOP I = 1 TO RECORDS(SELF.Relations)
      GET(SELF.Relations,I)
      IF SELF.Relations.File &= Him
        IF SELF.Relations.Fields &= NULL
?         ASSERT(~RightFirst,'Coding error, linking fields not found, recursing indefinately.')
          Him.ListLinkingFields(SELF,Trgt,1)
          RETURN
        END
?       ASSERT(~SELF.Relations.Fields.List &= NULL,'Relation must have at least on linking field')
        LOOP J = 1 TO RECORDS(SELF.Relations.Fields.List)
          GET(SELF.Relations.Fields.List,J)
          IF RightFirst
            Trgt.AddPair(SELF.Relations.Fields.List.Left,SELF.Relations.Fields.List.Right)
          ELSE
            Trgt.AddPair(SELF.Relations.Fields.List.Right,SELF.Relations.Fields.List.Left)
          END
        END
        RETURN
      END
    END

RelationManager.LogoutPrime PROCEDURE
FileToLogout &FILE
  CODE
    SELF.Me.UseFile()
    IF SELF.UseLogout
       FileToLogout &= SELF.Me.File
       IF NOT (SELF.Me.AliasedFile&=NULL)
          ! It is an ALIAS
          IF NOT SELF.Me.File{PROP:DriverLogsOutAlias}
             ! The driver does not support transaction over the ALIAS
             FileToLogout &= SELF.Me.AliasedFile.File
          END
       END
       IF FileToLogout{PROP:LOGOUT}<1
          !Never add the same file more than once to a transaction
          FileToLogout{PROP:Logout} = 1
          CASE ERRORCODE()
          OF 0
          OF 80
            SELF.UseLogout = 0
?           ASSERT(0,'Driver does not support transaction framing.')
          ELSE
            SELF.Me.Throw(Msg:LogoutFailed)
            RETURN Level:Notify
          END
       END
    END
    RETURN Level:Benign

RelationManager.LogoutDelete PROCEDURE
RetVal BYTE,AUTO
I LONG,AUTO
  CODE
  RetVal = SELF.LogoutPrime()
  IF ~RetVal
    SELF.DeferedRelations()
    LOOP I = 1 TO RECORDS(SELF.Relations)
      GET(SELF.Relations,I)
      IF SELF.Relations.DeleteMode = RI:Cascade
        RetVal = SELF.Relations.File.LogoutDelete()
      ELSIF SELF.Relations.DeleteMode = RI:Clear
        RetVal = SELF.Relations.File.LogoutUpdate()
      END
    UNTIL RetVal
  END
  RETURN RetVal

RelationManager.LogoutUpdate PROCEDURE
RetVal BYTE,AUTO
I LONG,AUTO
  CODE
  RetVal = SELF.LogoutPrime()
  IF ~RetVal
    SELF.DeferedRelations()
    LOOP I = 1 TO RECORDS(SELF.Relations)
      GET(SELF.Relations,I)
      IF SELF.Relations.UpdateMode = RI:Cascade OR SELF.Relations.UpdateMode = RI:Clear
        RetVal = SELF.Relations.File.LogoutUpdate()
      END
    UNTIL RetVal
  END
  RETURN RetVal

RelationManager.LogoutInsert PROCEDURE
RetVal BYTE,AUTO
I LONG,AUTO
  CODE
  IF SELF.Me.File{PROP:LOGOUT}<1  
    RetVal = SELF.LogoutPrime()
    IF ~RetVal
      SELF.DeferedRelations()
      LOOP I = 1 TO RECORDS(SELF.Relations)
        GET(SELF.Relations,I)
        RetVal = SELF.Relations.File.LogoutInsert()
      UNTIL RetVal
    END
    RETURN RetVal    
  END
  RETURN Level:Benign

RelationManager.OpenCloseServer PROCEDURE(BYTE Cascading,BYTE Opening)
I   BYTE(1)
Res BYTE,AUTO
ForcingOpen BYTE
OldOpened   SIGNED
  CODE
    IF Cascading
      IF SELF.LastTouched = Epoc
        RETURN  Level:Benign
      END
    ELSE
      Epoc += 1
    END
    ForcingOpen = SELF.ForcingOpen
    SELF.LastTouched = Epoc
?    ASSERT(NOT SELF.Relations &= NULL,'Relation manager incorrectly initialized.')
    IF Opening
      Res = SELF.Me.Open()
      IF NOT Cascading THEN Res=SELF.Me.UseFile().
    ELSE
      Res = SELF.Me.Close()
    END
    IF ~Cascading OR ForcingOpen
       SELF.DeferedRelations()
    END
    LOOP UNTIL Res
      GET(SELF.Relations,I)
      IF ERRORCODE()
        BREAK
      END
      IF Opening
         OldOpened = SELF.Relations.File.Me.Opened
         IF ForcingOpen
            Res = SELF.Relations.File.ForceOpen(1) ! Go through 'public' interface to pick up VIRTUAL ness
         ELSE
            Res = SELF.Relations.File.Open(1) ! Go through 'public' interface to pick up VIRTUAL ness
         END
         IF OldOpened < SELF.Relations.File.Me.GetOpened()
            SELF.Relations.Opened +=1
            PUT(SELF.Relations)
         END
      ELSE
        IF SELF.Relations.Opened
           OldOpened = SELF.Relations.File.Me.GetOpened()
           Res = SELF.Relations.File.Close(1)
           IF OldOpened > SELF.Relations.File.Me.GetOpened()
              SELF.Relations.Opened -=1
              PUT(SELF.Relations)
           END
        END
      END
      I += 1
    END
    RETURN Res

RelationManager.SetOpenRelated PROCEDURE()
  CODE
  SELF.ForcingOpen = True

RelationManager.ForceOpen PROCEDURE(BYTE Cascading)
RetVal  BYTE
  CODE
  SELF.ForcingOpen = True
  RetVal = SELF.Open(Cascading)
  RETURN RetVal

RelationManager.Open PROCEDURE(BYTE Cascading)
RetVal  BYTE
  CODE
  IF SELF.Me.LazyOpen = False
     SELF.ForcingOpen = True
  END
  RetVal = SELF.OpenCloseServer(Cascading,1)
  SELF.ForcingOpen = False
  RETURN RetVal

RelationManager.SetQuickScan PROCEDURE(BYTE On,BYTE Propagate)
I UNSIGNED,AUTO
  CODE
    IF SELF.Me.IsOpened()
      SEND(SELF.Me.File,'QUICKSCAN=' & CHOOSE(On = 1,'on','off'))
    END
    IF Propagate
      IF BAND(Propagate,080H)
        IF SELF.LastTouched = Epoc
          RETURN
        END
      ELSE
        Epoc += 1
        Propagate = Propagate + 80H
      END
      SELF.LastTouched = Epoc
      SELF.DeferedRelations()
      LOOP I = 1 TO RECORDS(SELF.Relations)
        GET(SELF.Relations,I)
        CASE BAND(Propagate,07FH)
        OF Propagate:OneMany
          IF ~SELF.Relations.HisKey&=NULL THEN CYCLE .
        OF Propagate:ManyOne
          IF SELF.Relations.HisKey&=NULL THEN CYCLE .
        END
        SELF.Relations.File.SetQuickScan(On,Propagate)
      END
    END

RelationManager.Save  PROCEDURE
I LONG,AUTO
  CODE
    SELF.DeferedRelations()
    LOOP I = 1 TO RECORDS(SELF.Relations)
      GET(SELF.Relations,I)
      IF NOT SELF.Relations.Fields &= NULL
        SELF.Relations.Fields.AssignLeftToBuffer
      END
    END

RelationManager.GetNbRelations PROCEDURE ! returns the number of relations defined for the primary file
  CODE
  SELF.DeferedRelations()
  RETURN Records(self.relations)

RelationManager.GetNbFiles PROCEDURE (RelationManager ParentMgr) ! returns the number of children files in this relation
i       LONG
nbFiles LONG
  CODE
  nbFiles = 1
  SELF.DeferedRelations()
  LOOP i = 1 to RECORDS(SELF.Relations)
    GET(SELF.Relations, i)
    IF not (SELF.Relations.File &= ParentMgr)
      nbFiles += SELF.Relations.File.GetNbFiles(SELF)
    END
  END
  RETURN nbFiles

RelationManager.getRelation PROCEDURE(LONG relPos) ! returns the relation number relPos in the list of relations
  CODE
  SELF.DeferedRelations()
  IF (relPos < 1) THEN RETURN NULL.
  IF (relPos > RECORDS(SELF.relations)) THEN RETURN NULL.
  Get(SELF.relations, relPos)
  RETURN self.relations.File

RelationManager.getRelation PROCEDURE(FILE f) ! returns the relation manager for the file f
i       LONG
  CODE
  SELF.DeferedRelations()
  IF (f &= null) then RETURN NULL.
  LOOP i = 1 to RECORDS(SELF.Relations)
    GET(SELF.Relations,I)
    IF (SELF.Relations.File.Me.File &= f)
      RETURN SELF.Relations.File
    END
  END
  RETURN NULL

RelationManager.getRelationType PROCEDURE(LONG relPos) !returns the type of the relation i (-1 = pb, 1 : 1:many relation, 0 : other relation)
  CODE
  SELF.DeferedRelations()
  IF (relPos < 1) THEN RETURN -1.
  IF (relPos > RECORDS(SELF.relations)) THEN RETURN -1.
  GET(SELF.relations, relPos)
  IF (SELF.relations.HisKey &= NULL) THEN RETURN 0.
  RETURN 1






! View manager class, handles filters, range limits and order clauses for a view


ViewManager.Destruct PROCEDURE

  CODE
  IF SELF.Inited AND NOT SELF.Killed
     SELF.Kill()
  END

! Specify a 'current value' range limit
ViewManager.AddRange PROCEDURE(*? Field)
  CODE
    SELF.Order.LimitType = Limit:Current
    SELF.LimitMajorComponents(Field)
    SELF.Order.RangeList.AddItem(Field)
    SELF.SetFreeElement

! Single value range limit
ViewManager.AddRange PROCEDURE(*? Field,*? Value)
  CODE
    SELF.Order.LimitType = Limit:Single
    SELF.LimitMajorComponents(Field)
    SELF.Order.RangeList.AddItem(Value)
    SELF.SetFreeElement

! Pair of values range limit
ViewManager.AddRange PROCEDURE(*? Field,*? Low,*? High)
  CODE
    SELF.Order.LimitType = Limit:Pair
    SELF.LimitMajorComponents(Field)
    SELF.Order.RangeList.AddItem(Low)
    SELF.Order.RangeList.AddItem(High)
    SELF.SetFreeElement

! File base range limit
ViewManager.AddRange PROCEDURE(*? Field,RelationManager MyFile,RelationManager HisFile)
  CODE
    SELF.Order.LimitType = Limit:File
    HisFile.ListLinkingFields(MyFile,SELF.Order.RangeList)
?   ASSERT(RECORDS(SELF.Order.RangeList.List),'File based range limit requires linking fields.')
    SELF.SetFreeElement

ViewManager.AddSortOrder PROCEDURE(<Key K>)
  CODE
    CLEAR(SELF.Order)
    SELF.Order.MainKey &= K
    SELF.Order.RangeList &= NEW BufferedPairsClass
    SELF.Order.RangeList.Init
    IF ~SELF.Order.MainKey &= NULL AND SELF.Primary.Me.GetComponents(K)
      SELF.Order.FreeElement &= SELF.Primary.Me.GetField(K,1)
    END
    ADD(SELF.Order)
?   ASSERT(~ERRORCODE(),'Unable to add new sort order.')
    SELF.SetOrder(SELF.Primary.Me.KeyToOrder(SELF.Order.MainKey,1))
    RETURN RECORDS(SELF.Order)

ViewManager.AppendOrder PROCEDURE(STRING F)
  CODE
    IF SELF.Order.Order &= NULL
      SELF.SetOrder(F)
    ELSIF F AND F[1] = '*'
      SELF.SetOrder(F[2:LEN(F)])
    ELSE
      SELF.SetOrder(SELF.Order.Order & ',' & F)
    END

ViewManager.ApplyFilter PROCEDURE
RangeFilter CSTRING(5000)
FieldName   CSTRING(500),AUTO
I           BYTE,AUTO
RRL         BYTE,AUTO
  CODE
    RRL = RECORDS(SELF.Order.RangeList.List)
    CASE SELF.Order.LimitType
    OF Limit:Current OROF Limit:Single OROF Limit:File
      LOOP I = 1 TO RRL
        GET(SELF.Order.RangeList.List,I)
        FieldName = SELF.Primary.Me.GetFieldName(SELF.Order.MainKey,I)
        RangeFilter = RangeFilter & CHOOSE(I = 1,'',' AND ') & FieldName & ' = ' & CasedValue(FieldName,SELF.Order.RangeList.List.Left,SELF.Order.RangeList.List.Right)
      END
    OF Limit:Pair
      LOOP I = 1 TO RRL-1
        GET(SELF.Order.RangeList.List,I)
        FieldName = SELF.Primary.Me.GetFieldName(SELF.Order.MainKey,I)
        CASE I
        OF RRL-1
          RangeFilter = RangeFilter & CHOOSE(RangeFilter = '','',' AND ') & FieldName & ' >= ' & CasedValue(FieldName,SELF.Order.RangeList.List.Left,SELF.Order.RangeList.List.Right)
        ELSE
          RangeFilter = RangeFilter & CHOOSE(I = 1,'',' AND ') & FieldName & ' = ' & CasedValue(FieldName,SELF.Order.RangeList.List.Left,SELF.Order.RangeList.List.Right)
        END
      END
      GET(SELF.Order.RangeList.List,RRL)
      RangeFilter = RangeFilter & ' AND ' & FieldName & ' <<= ' & CasedValue(FieldName,SELF.Order.RangeList.List.Left,SELF.Order.RangeList.List.Right)
    END
    IF ~(SELF.Order.Filter &= NULL)
      LOOP I = 1 TO RECORDS(SELF.Order.Filter)
        GET(SELF.Order.Filter,I)
        RangeFilter = RangeFilter & CHOOSE(RangeFilter = '','(',' AND (') & SELF.Order.Filter.Filter & ')'
      END
    END
    SELF.View{PROP:Filter} = RangeFilter
    IF ERRORCODE()
      SELF.Primary.Me.Throw(Msg:ViewOpenFailed)
    END

ViewManager.ApplyOrder PROCEDURE
  CODE
    SELF.View{PROP:Order} = SELF.Order.Order
    IF ERRORCODE()
      SELF.Primary.Me.Throw(Msg:ViewOpenFailed)
    END

ViewManager.ApplyRange PROCEDURE
  CODE
    CASE SELF.Order.LimitType
    OF Limit:Pair
      GET(SELF.Order.RangeList.List,RECORDS(SELF.Order.RangeList.List)-1)
      IF SELF.Order.RangeList.List.Left <> SELF.Order.RangeList.List.Right
        SELF.Order.RangeList.List.Right = SELF.Order.RangeList.List.Left
        PUT(SELF.Order.RangeList.List)
        GET(SELF.Order.RangeList.List,RECORDS(SELF.Order.RangeList.List))
        SELF.Order.RangeList.List.Right = SELF.Order.RangeList.List.Left
        PUT(SELF.Order.RangeList.List)
        SELF.ApplyFilter
        RETURN 1
      END
    OROF Limit:Single
      GET(SELF.Order.RangeList.List,RECORDS(SELF.Order.RangeList.List))
      IF SELF.Order.RangeList.List.Left <> SELF.Order.RangeList.List.Right
        SELF.Order.RangeList.List.Right = SELF.Order.RangeList.List.Left
        PUT(SELF.Order.RangeList.List)
        SELF.ApplyFilter
        RETURN 1
      END
    OF Limit:File
      IF ~SELF.Order.RangeList.EqualRightBuffer()
        SELF.Order.RangeList.AssignRightToBuffer()
        SELF.ApplyFilter
        RETURN 1
      END
    END
    RETURN 0

ViewManager.Close PROCEDURE
  CODE
    SELF.Close(FALSE)

ViewManager.Close PROCEDURE(BYTE Force)
  CODE
    IF SELF.Opened OR Force
      CLOSE(SELF.View)
      SELF.Opened = 0
    END

ViewManager.ClearFilesRecord PROCEDURE()
i           LONG,AUTO
nFiles      LONG,AUTO
Fr          &File
Record   &GROUP, AUTO
  CODE
      nFiles = SELF.View{PROP:Files}
      LOOP i=1 TO nFiles
        Fr &= SELF.View{PROP:File,i}
        Record &= Fr{PROP:Record}
        CLEAR(Record)
      END

ViewManager.Init PROCEDURE(VIEW V,RelationManager F,<SortOrder S>)
  CODE
    SELF.Inited = True
    SELF.View &= V
    SELF.Primary &= F
    IF OMITTED(4)
      SELF.Order &= NEW SortOrder
      SELF.DisposeOrder = 1
    ELSE
      SELF.Order &= S
      SELF.DisposeOrder = 0
    END
    SELF.SavedBuffers &= NEW(BuffersQueue)
    SELF.PageSize = 20
    SELF.PagesBehind = 2
    SELF.PagesAhead = 0
    SELF.TimeOut = 60
    SELF.Killed = False
    SELF.UseView()

ViewManager.Kill PROCEDURE
I LONG,AUTO
J SIGNED,AUTO
  CODE
    IF NOT SELF.Order &= NULL
      LOOP I = 1 TO RECORDS(SELF.Order)
        GET(SELF.Order,I)
        SELF.Order.RangeList.Kill
        DISPOSE(SELF.Order.RangeList)
        IF ~SELF.Order.Filter &= NULL
          LOOP J = 1 TO RECORDS(SELF.Order.Filter)
            GET(SELF.Order.Filter,J)
            DISPOSE(SELF.Order.Filter.Filter)
          END
        END
        DISPOSE(SELF.Order.Filter)
        DISPOSE(SELF.Order.Order)
        SELF.Order.FreeElement &= NULL
        PUT(SELF.Order)
      END
      IF SELF.DisposeOrder
        DISPOSE(SELF.Order)
      ELSE
        SELF.Order &= NULL
      END
    END
    DISPOSE(SELF.SavedBuffers)
    SELF.Close()
    SELF.UnUseView()
    SELF.Killed = True

ViewManager.GetFirstSortFieldName PROCEDURE()
IdxStar  BYTE,AUTO
IdxEnd   BYTE,AUTO
RetVal   CSTRING(512)
 CODE
    IF SELF.Order.Order
       IdxEnd = INSTRING(',',SELF.Order.Order,1,1)
       IF IdxEnd
          IdxEnd-=1
       ELSE
          IdxEnd=LEN(CLIP(SELF.Order.Order))
       END
       IF SELF.Order.Order[1]='+' OR SELF.Order.Order[1]='-'
          IdxStar = 2
       ELSE
          IdxStar = 1
       END
       IF IdxEnd>IdxStar+6
          IF UPPER(SELF.Order.Order[(IdxStar):(IdxStar+5)])='UPPER(' AND SELF.Order.Order[IdxEnd]=')'
             IdxEnd-=1
             IdxStar+=6
          END
       END
       RetVal = SELF.Order.Order[(IdxStar):(IdxEnd)]
    END
    RETURN RetVal

ViewManager.GetFirstSortField PROCEDURE()
F        &File
FieldIdx BYTE,AUTO
Field    ANY
IdxStar  BYTE,AUTO
IdxEnd   BYTE,AUTO
Record   &GROUP, AUTO
  CODE
    IF SELF.Order.Order
       IdxEnd = INSTRING(',',SELF.Order.Order,1,1)
       IF IdxEnd
          IdxEnd-=1
       ELSE
          IdxEnd=LEN(CLIP(SELF.Order.Order))
       END
       IF SELF.Order.Order[1]='+' OR SELF.Order.Order[1]='-'
          IdxStar = 2
       ELSE
          IdxStar = 1
       END
       IF IdxEnd>IdxStar+6
          IF UPPER(SELF.Order.Order[(IdxStar):(IdxStar+5)])='UPPER(' AND SELF.Order.Order[IdxEnd]=')'
             IdxEnd-=1
             IdxStar+=6
          END
       END
       LOOP FieldIdx = 1 to SELF.View{PROP:Fields}
            F &= SELF.View{PROP:FieldsFile, FieldIdx}
            Record &= F{PROP:Record}
            IF UPPER(F{PROP:Label, SELF.View{PROP:Field, FieldIdx}})=UPPER(SELF.Order.Order[(IdxStar):(IdxEnd)])
               Field &= What(Record, SELF.View{PROP:Field, FieldIdx})
               BREAK
            END
       END
    END
    RETURN Field

ViewManager.LimitMajorComponents PROCEDURE(*? Field)
F ANY
I BYTE,AUTO
  CODE
    SELF.Order.RangeList.Init
    LOOP I = 1 TO SELF.Primary.Me.GetComponents(SELF.Order.MainKey)
      F &= SELF.Primary.Me.GetField(SELF.Order.MainKey,I)
      IF F &= Field THEN BREAK .
      SELF.Order.RangeList.AddItem(F)
    END

ViewManager.UseView PROCEDURE

i           LONG,AUTO
nFiles      LONG,AUTO
Fr          &File
fm          &FileManager,auto
rm          &RelationManager,auto

  CODE
? ASSERT(~SELF.View&=NULL,'View manager has not been assigned a view')
  nFiles = SELF.View{PROP:Files}
  LOOP i=1 TO nFiles
    Fr &= SELF.View{PROP:File,i}
?   ASSERT(~Fr &= NULL,'View driver failed to return constituent file.')
    fm &= FilesManager.GetFileMapping(FilesManager.GetFileID(Fr))
?   ASSERT(~fm &= NULL, 'Unable to obtain file manager for constituent file:'&Fr{PROP:Name})
    rm &= FilesManager.GetRelationFileMapping(FilesManager.GetFileID(Fr))
?   ASSERT(~rm &= NULL, 'Unable to obtain relation manager for constituent file:'&Fr{PROP:Name})
    rm.Open()
    SELF.Used = True
    IF NOT fm &= NULL AND fm.UseFile()<>Level:Benign THEN RETURN.
  END

ViewManager.UnUseView                       PROCEDURE
i           LONG,AUTO
nFiles      LONG,AUTO
Fr          &File
rm          &RelationManager,auto

  CODE
  IF NOT SELF.Used = True THEN RETURN.
? ASSERT(~SELF.View&=NULL,'View manager has not been assigned a view')
  nFiles = SELF.View{PROP:Files}
  LOOP i=1 TO nFiles
    Fr &= SELF.View{PROP:File,i}
?   ASSERT(~Fr &= NULL,'View driver failed to return constituent file.')
    rm &= FilesManager.GetRelationFileMapping(FilesManager.GetFileID(Fr))
?   ASSERT(~rm &= NULL, 'Unable to obtain relation manager for constituent file:'&Fr{PROP:Name})
    rm.Close()
  END
  SELF.Used = False

ViewManager.Open PROCEDURE
  CODE
    IF ~SELF.Opened
?     ASSERT(RECORDS(SELF.Order),'At least 1 view order must be specified')
      SELF.Opened = 0
      OPEN(SELF.View)
      CASE ERRORCODE()
      OF NoError
         SELF.Opened = 1
      OF IsOpenErr
      ELSE
        SELF.Primary.Me.Throw(Msg:ViewOpenFailed)
      END
      BUFFER(SELF.View,SELF.PageSize,SELF.PagesBehind,SELF.PagesAhead,SELF.TimeOut)
    END
    SELF.ApplyOrder
    SELF.ApplyFilter

ViewManager.Reset PROCEDURE
  CODE
    SELF.Reset(0)

ViewManager.Reset PROCEDURE(BYTE LocatePos)
  CODE
    SELF.Open
    SET(SELF.View,LocatePos)
    IF ERRORCODE()
      SELF.Primary.Me.Throw(Msg:ViewOpenFailed)
    END

ViewManager.RestoreBuffers                  PROCEDURE()
i           LONG,AUTO
  CODE
? ASSERT(~SELF.SavedBuffers&=NULL,'View manager has not  properly initialized')
  IF NOT SELF.SavedBuffers &= NULL
     LOOP i=1 TO RECORDS(SELF.SavedBuffers)
          GET(SELF.SavedBuffers,i)
          IF NOT ERRORCODE()
             SELF.SavedBuffers.fm.RestoreBuffer(SELF.SavedBuffers.Buff)
          END
     END
     FREE(SELF.SavedBuffers)
  END

ViewManager.SaveBuffers                     PROCEDURE()
i           LONG,AUTO
nFiles      LONG,AUTO
Fr          &File
fm          &FileManager,auto
  CODE
? ASSERT(~SELF.View&=NULL,'View manager has not been assigned a view')
? ASSERT(~SELF.SavedBuffers&=NULL,'View manager has not  properly initialized')
  IF NOT SELF.SavedBuffers &= NULL
     FREE(SELF.SavedBuffers)
     nFiles = SELF.View{PROP:Files}
     LOOP i=1 TO nFiles
       Fr &= SELF.View{PROP:File,i}
?      ASSERT(~Fr &= NULL,'View driver failed to return constituent file.')
       fm &= FilesManager.GetFileMapping(FilesManager.GetFileID(Fr))
       IF ~fm &= NULL
           SELF.SavedBuffers.Fm   &= Fm
           SELF.SavedBuffers.Buff = fm.SaveBuffer()
           ADD(SELF.SavedBuffers)
       END
     END
  END

ViewManager.SetFilter PROCEDURE(STRING F)
  CODE
    SELF.SetFilter(F,'5 Standard')

ViewManager.SetFilter PROCEDURE(STRING F,STRING Id)
  CODE
    IF SELF.Order.Filter &= NULL
      SELF.Order.Filter &= NEW FilterQueue
      PUT(SELF.Order)
    END
    SELF.Order.Filter.Id = Id
    GET(SELF.Order.Filter,-SELF.Order.Filter.Id)
    IF ~ERRORCODE()
      DISPOSE(SELF.Order.Filter.Filter)
    ELSE
      ADD(SELF.Order.Filter,-SELF.Order.Filter.Id)
    END
    IF F
      SELF.Order.Filter.Filter &= DupString(F)
      PUT(SELF.Order.Filter)
    ELSE
      DELETE(SELF.Order.Filter)
    END

ViewManager.GetFreeElementAscending PROCEDURE
FEP BYTE,AUTO
  CODE
    FEP = SELF.GetFreeElementPosition()
    IF FEP
      RETURN SELF.Primary.Me.GetFieldAscending(SELF.Order.MainKey,FEP)
    END
    RETURN 1

ViewManager.GetFreeElementName PROCEDURE
FEP BYTE,AUTO
  CODE
    FEP = SELF.GetFreeElementPosition()
    IF FEP
      RETURN SELF.Primary.Me.GetFieldName(SELF.Order.MainKey,FEP)
    END
    RETURN ''

ViewManager.GetFreeElementPosition PROCEDURE
FEP BYTE,AUTO
  CODE
    FEP = RECORDS(SELF.Order.RangeList.List)
    IF SELF.Order.LimitType <> Limit:Pair THEN
      FEP += 1
    END
    RETURN CHOOSE(~SELF.Order.MainKey &= NULL AND FEP <= SELF.Primary.Me.GetComponents(SELF.Order.MainKey),FEP,0)

ViewManager.GetFieldAscending PROCEDURE(*? Fld)
F        &File
FieldIdx BYTE,AUTO
FN       CSTRING(100)
Record   &GROUP, AUTO
Field    ANY
ParField ANY
Pos      SHORT,AUTO
FieldAscending BYTE,AUTO
  CODE
    FieldAscending = 1
    ParField &= Fld
    LOOP FieldIdx = 1 to SELF.View{PROP:Fields}
         F &= SELF.View{PROP:FieldsFile, FieldIdx}
         Record &= F{PROP:Record}
         Field &= What(Record, SELF.View{PROP:Field, FieldIdx})
         IF Field &= ParField
            FN = F{PROP:Label, SELF.View{PROP:Field, FieldIdx}}
            BREAK
         END
    END
    IF FN
       Pos = STRPOS(SELF.View{PROP:ORDER},'{{{{{{[+-]}{{'&FN&'}[,]}|{{{{[+-]}{{'&FN&'}$}}',1)
       IF NOT Pos
          Pos = STRPOS(SELF.View{PROP:ORDER},'{{{{{{[+-]}{{UPPER('&FN&')}[,]}|{{{{[+-]}{{UPPER('&FN&')}$}}',1)
       END
       IF Pos
          FieldAscending = CHOOSE(SUB(SELF.View{PROP:ORDER},Pos,1)='+',True,False)
       END
    END
    RETURN FieldAscending

ViewManager.GetFieldName PROCEDURE(*? Fld)
F        &File
FieldIdx BYTE,AUTO
FN       CSTRING(100)
Record   &GROUP, AUTO
Field    ANY
ParField ANY

  CODE
    ParField &= Fld
    LOOP FieldIdx = 1 to SELF.View{PROP:Fields}
         F &= SELF.View{PROP:FieldsFile, FieldIdx}
         Record &= F{PROP:Record}
         Field &= What(Record, SELF.View{PROP:Field, FieldIdx})
         IF Field &= ParField
            FN = Who(Record, SELF.View{PROP:Field, FieldIdx})
            BREAK
         END
    END
    RETURN FN

ViewManager.SetFreeElement PROCEDURE
FEP BYTE,AUTO
  CODE
    FEP = SELF.GetFreeElementPosition()
    IF FEP
      SELF.Order.FreeElement &= SELF.Primary.Me.GetField(SELF.Order.MainKey,FEP)
    END
    SELF.SetOrder(SELF.Primary.Me.KeyToOrder(SELF.Order.MainKey,1))
    PUT(SELF.Order)


ViewManager.SetOrder PROCEDURE(STRING F)
  CODE
?   ASSERT(~SELF.Order &= NULL,'View does not have any sort orders defined')
    DISPOSE(SELF.Order.Order)
    IF F
      SELF.Order.Order &= DupString(F)
    END
    PUT(SELF.Order)

ViewManager.ValidateRecord PROCEDURE
  CODE
    RETURN Record:OK  ! Cannot validate primary record because of project implications

ViewManager.Next PROCEDURE
  CODE
    LOOP
      NEXT(SELF.View)
      IF ERRORCODE()
        IF ERRORCODE() = BadRecErr
          RETURN Level:Notify
        ELSE
          SELF.Primary.Me.Throw(Msg:AbortReading)
          RETURN Level:Fatal
        END
      ELSE
        CASE SELF.ValidateRecord()
        OF Record:OK
          RETURN Level:Benign
        OF Record:OutOfRange
          RETURN Level:Notify
        END
      END
    END


! This function calls the underlying file manager to prime a record
! for insertion. It then uses range-limit information to fill in other
! fields if it can.
ViewManager.PrimeRecord PROCEDURE(BYTE SC)
F      ANY
I      LONG,AUTO
nFiles LONG,AUTO
Delta  SHORT
FR     &FILE
  CODE
    IF ~SC
      nFiles = SELF.View{PROP:Files}
      LOOP i=1 TO nFiles
        Fr &= SELF.View{PROP:File,i}
        CLEAR(FR)
      END
    END
    CASE SELF.Order.LimitType
    OF Limit:Pair
      Delta = -2
    OROF Limit:Current OROF Limit:Single OROF Limit:File
      LOOP I = 1 TO RECORDS(SELF.Order.RangeList.List) + Delta
        GET(SELF.Order.RangeList.List,I)
        F &= SELF.Primary.Me.GetField(SELF.Order.MainKey,I)
        F = SELF.Order.RangeList.List.Right
      END
    END
    RETURN SELF.Primary.Me.PrimeRecord(1)

ViewManager.Previous  PROCEDURE
  CODE
    LOOP
      PREVIOUS(SELF.View)
      IF ERRORCODE()
        IF ERRORCODE() = BadRecErr
          RETURN Level:Notify
        ELSE
          SELF.Primary.Me.Throw(Msg:AbortReading)
          RETURN Level:Fatal
        END
      ELSE
        CASE SELF.ValidateRecord()
        OF Record:OK
          RETURN Level:Benign
        OF Record:OutOfRange
          RETURN Level:Notify
        END
      END
    END

ViewManager.SetSort PROCEDURE(BYTE B)
  CODE
    IF B = 0
      B = 1
    END
    IF B = POINTER(SELF.Order)
      RETURN 0
    ELSE
      GET(SELF.Order,B)
?     ASSERT(~ERRORCODE(),'Sort order does not exist:'&B)
      RETURN 1
    END

CasedValue PROCEDURE(STRING FieldName,*? Field,? FieldValue)
TValue CSTRING(2000),AUTO
Value CSTRING(2000),AUTO
I     UNSIGNED,AUTO
J     UNSIGNED(2)
  CODE
    IF IsString(Field)
      TValue = FieldValue
      Value[1] = ''''
      LOOP I = 1 TO LEN(TValue)
        Value[J] = TValue[I]
        J += 1
        IF TValue[I] = ''''        ! Double up for parsing purposes
          Value[J] = TValue[I]
          J += 1
        END
      END
      Value[J :J+1] = '''<0>'
      RETURN CHOOSE(INSTRING('UPPER(',FieldName,1,1) <> 0,'UPPER('&Value&')', CHOOSE(INSTRING('LOWER(',FieldName,1,1) <> 0,'LOWER('&Value&')',Value))
    ELSE
      RETURN FieldValue
    END

LocalAction PROCEDURE(BYTE Mode)
  CODE
  RETURN CHOOSE(Mode=RI:Restrict OR Mode=RI:Cascade OR Mode=RI:Clear)



GetFileManager PROCEDURE(ASTRING Tag)

RVal  &FileManager,AUTO

  CODE
  RVal &= (TIED(Tag, AS:AddrFileManager))
  RETURN RVal


SetFileManager PROCEDURE(FileManager FM, ASTRING Tag)

  CODE
  TIE(Tag, AS:AddrFileManager, ADDRESS(FM))


GetFileField PROCEDURE(ASTRING FMTag, ASTRING FldTag)

FM    &FileManager,AUTO
RVal  ANY,AUTO

  CODE
  FM &= GetFileManager(FMTag)
  IF FM &= NULL
    RVal &= NULL
  ELSE
    RVal &= FM.GetField(FldTag)
  END
  RETURN RVal


GetFieldValue PROCEDURE(ASTRING FMTag, ASTRING FldTag)

FM    &FileManager,AUTO
Fld   ANY,AUTO

  CODE
  FM &= GetFileManager(FMTag)
  Fld &= GetFileField(FMTag, FldTag)
  RETURN CHOOSE(FM &= NULL OR Fld &= NULL, '', FORMAT(Fld, FM.GetFieldPicture(FldTag)))


!* DbTrigger Classes.

DbLogFileManager.Init              PROCEDURE(ErrorClass EC, STRING LogFileName)

DbTextLogFile       FILE,DRIVER('ASCII'),NAME(szDbTextLog),CREATE,THREAD
Record                RECORD
strEntry                STRING(1000)
                      END
                    END
 CODE
 PARENT.Init(DbTextLogFile, EC)
 SELF.FileName &= szDbTextLog
 SELF.FileNameValue = CLIP(LogFileName)
 SELF.Buffer &= DbTextLogFile.Record
 SELF.LazyOpen = False
 SELF.LockRecover = 10






DbChangeManager.AddItem                      PROCEDURE(*? Left, STRING Name, STRING FileName)

Cp CriticalProcedure

 CODE
  Cp.Init(SELF.Crit)
  IF SELF.SetThread(FileName)
    SELF.TriggerQueue.BFP.AddItem(Left)
    SELF.NameQueue.FileName = FileName
    SELF.NameQueue.FieldName = Name
    GET(SELF.NameQueue,+SELf.NameQueue.FileName,+SELF.NameQueue.FieldName)
    IF ERRORCODE()
       SELF.NameQueue.Ptr = RECORDS(SELF.TriggerQueue.BFP.List)
       ADD(SELF.NameQueue)
    END
   END

DbChangeManager.Equal                        PROCEDURE(STRING FileName)

Cp CriticalProcedure

 CODE
   Cp.Init(SELF.Crit)
   IF SELF.SetThread(FileName)
      RETURN SELF.TriggerQueue.BFP.Equal()
   ELSE
      RETURN True
   END

DbChangeManager.CheckChanges                 PROCEDURE(STRING FileName, FILE File)

I   USHORT,AUTO
Cp  CriticalProcedure

 CODE
   Cp.Init(SELF.Crit)
   SELF.DbChangeAudit.BeforeChange(FileName, SELF.TriggerQueue.BFP)
   IF SELF.SetThread(FileName)
    LOOP I = 1 TO RECORDS(SELF.TriggerQueue.BFP.List)
     GET(SELF.TriggerQueue.BFP.List,I)
     SELF.CheckPair(SELF.TriggerQueue.BFP)
    END
   END
   SELF.DbChangeAudit.OnChange(FileName, File)

DbChangeManager.CheckPair                    PROCEDURE(FieldPairsClass FP)

Cp  CriticalProcedure

 CODE
   Cp.Init(SELF.Crit)
   IF FP.List.Left <> FP.List.Right
      SELF.NameQueue.Ptr = POINTER(SELF.TriggerQueue.BFP.List)
      GET(SELF.NameQueue,SELF.NameQueue.Ptr)
      SELF.DbChangeAudit.ChangeField(FP.List.Left, FP.List.Right, SELF.NameQueue.FieldName, SELF.NameQueue.FileName)
   END

DbChangeManager.Update                       PROCEDURE(STRING FileName)

Cp  CriticalProcedure

 CODE
   Cp.Init(SELF.Crit)
   IF SELF.SetThread(FileName)
      SELF.TriggerQueue.BFP.AssignLeftToRight()
   END

DbChangeManager.SetThread                    PROCEDURE(STRING FileName)

Cp CriticalProcedure

 CODE
   Cp.Init(SELF.Crit)
?  ASSERT(~SELF.TriggerQueue&=NULL,'DbTriggers not inited.')
   SELF.TriggerQueue.ID = THREAD()
   SELF.TriggerQueue.FileName = FileName
   GET(SELF.TriggerQueue, +SELF.TriggerQueue.ID, +SELF.TriggerQueue.FileName)
   IF ERRORCODE()
      RETURN False
   ELSE
      RETURN True
   END

DbChangeManager.AddThread                    PROCEDURE(STRING FileName)

Cp CriticalProcedure

   CODE
   Cp.Init(SELF.Crit)
   SELF.TriggerQueue.ID = THREAD()
   SELF.TriggerQueue.BFP &= NEW(BufferedPairsClass)
   SELF.TriggerQueue.BFP.Init()
   ADD(SELF.TriggerQueue)

DbChangeManager.Init                         PROCEDURE(*IDbChangeAudit IDbC)
 CODE
   SELF.Crit &= NewCriticalSection()
   SELF.DbChangeAudit &= IDbC
   SELF.TriggerQueue &= NEW(DbTriggerQueue)
   SELF.NameQueue &= NEW(DbNameQueue)
   SELF.Inited = True
   SELF.Killed = False

DbChangeManager.Kill                         PROCEDURE
 CODE
   IF SELF.Inited
      IF NOT SELF.Killed
         SELF.Inited = False
         LOOP
          GET(SELF.TriggerQueue,1)
          IF ERRORCODE() THEN BREAK.
          SELF.TriggerQueue.BFP.Kill()
          DISPOSE(SELF.TriggerQueue.BFP)
          DELETE(SELF.TriggerQueue)
         END
         DISPOSE(SELF.TriggerQueue)
         FREE(SELF.NameQueue)
         DISPOSE(SELF.NameQueue)
         SELF.Crit.kill()
         SELF.Killed = True
      END
   END

DbChangeManager.Construct                  PROCEDURE
 CODE

DbChangeManager.Destruct                   PROCEDURE
 CODE
    IF NOT SELF.Killed
       SELF.Kill
    END




DbAuditManager.AddLogFile      PROCEDURE(STRING FileName, STRING LogFileName)

Cp  CriticalProcedure

 CODE
   Cp.Init(SELF.Crit)
   SELF.SyncAction()
   SELF.LogFiles.FileName = FileName
   SELF.LogFiles.LogFileName = LogFileName
   GET(SELF.LogFiles,+SELF.LogFiles.FileName)
   IF ERRORCODE()
      CLEAR(SELF.Logfiles)
      SELF.LogFiles.FileName = FileName
      SELF.LogFiles.LogFileName = LogFileName
      ADD(SELF.LogFiles)
      SELF.AddItem(FileName, 'Action', SELF.Actions.Action, 'Action', '@s20') ! Add the Action Column for this file.
   END

DbAuditManager.AddItem         PROCEDURE(STRING FileName, STRING FieldName, *? Field, STRING FieldHeader, STRING FieldPicture)

Cp CriticalProcedure

 CODE
   Cp.Init(SELF.Crit)
   IF NOT SELF.SetFM(FileName) THEN RETURN.

   SELF.SyncAction()
   SELF.ColumnInfo.FileName = FileName
   SELF.ColumnInfo.FieldName = FieldName
   GET(SELF.ColumnInfo,+SELF.ColumnInfo.FileName,+SELF.ColumnInfo.FieldName)
   IF ERRORCODE()
      CLEAR(SELF.ColumnInfo)
      SELF.ColumnInfo.FileName = FileName
      SELF.ColumnInfo.FieldName = FieldName
      SELF.ColumnInfo.Field &= Field
      SELF.ColumnInfo.FieldHeader = FieldHeader
      SELF.ColumnInfo.FieldPicture = FieldPicture
      SELF.ColumnInfo.Length = LEN(FORMAT(Field,FieldPicture))
      IF LEN(CLIP(SELF.ColumnInfo.FieldHeader)) > SELF.ColumnInfo.Length
         SELF.ColumnInfo.Length = LEN(CLIP(SELF.ColumnInfo.FieldHeader))
      END
      ADD(SELF.ColumnInfo)
   END

DbAuditManager.AppendLog                      PROCEDURE(STRING FileName)

I         USHORT,AUTO
HeaderPic ASTRING
Cp        CriticalProcedure

 CODE
   Cp.Init(SELF.Crit)
   IF NOT SELF.SetFM(FileName) THEN RETURN.

   SELF.SyncAction()
   SELF.OpenLogFile(FileName)
   CLEAR(SELF.LFM.Buffer)
   SELF.LFM.Buffer = '|' & FORMAT(Today(),@D17) & '|' & FORMAT(Clock(),@T7) & '|'
   SORT(SELF.ColumnInfo,+SELF.ColumnInfo.FileName)
   LOOP I = 1 TO RECORDS(SELF.ColumnInfo)
    GET(SELF.ColumnInfo,I)
    IF SELF.ColumnInfo.FileName < FileName THEN CYCLE.
    IF SELF.ColumnInfo.FileName > FileName THEN BREAK.
    HeaderPic = '@s' & SELF.ColumnInfo.Length
    SELF.LFM.Buffer = CLIP(SELF.LFM.Buffer) & FORMAT(FORMAT(SELF.ColumnInfo.Field,SELF.ColumnInfo.FieldPicture),HeaderPic) & '|'
   END
   SELF.LFM.Insert()
   SELF.CloseLogFile(FileName)

DbAuditManager.CreateHeader                   PROCEDURE(STRING FileName, DbLogFileManager LFM)

I           USHORT,AUTO
lHeader     USHORT,AUTO
SaveBuffer  ASTRING
Cp          CriticalProcedure

 CODE
 Cp.Init(SELF.Crit)
 SELF.SyncAction()
 LFM.Create = True
 CLEAR(LFM.Buffer)
 LFM.Buffer = '| Date' & ALL(' ',LEN(FORMAT(Today(),@D17))-5) & '| Time' & ALL(' ',LEN(FORMAT(Clock(),@T7))-5) & '|'
 SORT(SELF.ColumnInfo,+SELF.ColumnInfo.FileName)
 LOOP I = 1 TO RECORDS(SELF.ColumnInfo)
   GET(SELF.ColumnInfo,I)
   IF SELF.ColumnInfo.FileName < FileName THEN CYCLE.
   IF SELF.ColumnInfo.FileName > FileName THEN BREAK.
   lHeader = LEN(CLIP(SELF.ColumnInfo.FieldHeader))
   IF lHeader < SELF.ColumnInfo.Length
      LFM.Buffer = CLIP(LFM.Buffer) & SELF.ColumnInfo.FieldHeader & ALL(' ',SELF.ColumnInfo.Length-lHeader) & '|'
   ELSE
      LFM.Buffer = CLIP(LFM.Buffer) & SELF.ColumnInfo.FieldHeader & '|'
   END
 END
 SaveBuffer = LFM.Buffer
 LFM.Buffer = ALL('-',LEN(CLIP(SaveBuffer)))
 LFM.Insert()
 LFM.Buffer = SaveBuffer
 LFM.Insert()
 LFM.Buffer = ALL('-',LEN(CLIP(SaveBuffer)))
 LFM.Insert()

DbAuditManager.Init                           PROCEDURE(ErrorClass ErrorHandler)
 CODE
   SELF.Crit &= NewCriticalSection()
   SELF.Errors &= ErrorHandler
   SELF.ColumnInfo &= NEW (DbColumnQueue)
   SELF.LogFiles &= NEW (LogFileQueue)
   SELF.LFM &= NEW (DbLogFileManager)
   SELF.LFM.CleanedUp = TRUE
   SELF.Actions &= NEW(ThreadedAction)
   SELF.Inited = True
   SELF.Killed = False

DbAuditManager.Kill                           PROCEDURE()
 CODE
   IF SELF.Inited
      IF NOT SELF.Killed
         SELF.Inited = False
         DISPOSE(SELF.Actions)
         LOOP UNTIL RECORDS(SELF.ColumnInfo) < 1
          GET(SELF.ColumnInfo,1)
          SELF.ColumnInfo.Field &= NULL
          DELETE(SELF.ColumnInfo)
         END
         FREE(SELF.ColumnInfo)
         DISPOSE(SELF.ColumnInfo)
         FREE(SELF.LogFiles)
         DISPOSE(SELF.LogFiles)
         IF SELF.LFM.Opened
            SELF.LFM.Close()
         END
         DISPOSE(SELF.LFM)
         SELF.Killed = True
      END
   END

DbAuditManager.Construct                  PROCEDURE
 CODE

DbAuditManager.Destruct                   PROCEDURE
 CODE
    IF NOT SELF.Killed
       SELF.Kill
    END

DbAuditManager.SyncAction PROCEDURE

  CODE
  SELF.SyncAction(THREAD())

dbAuditManager.SyncAction PROCEDURE(UNSIGNED Id)

  CODE
  SELF.Actions.Id = Id
  GET(SELF.Actions, SELF.Actions.Id)
  IF ERRORCODE()
    SELF.Actions.Action = ''
    ADD(SELF.Actions, SELF.Actions.Id)
?   ASSERT(~ERRORCODE())
  END

DbAuditManager.SetAction PROCEDURE(UNSIGNED Id, STRING Action)

  CODE
  SELF.SyncAction(id)
  SELF.Actions.Action = Action
  PUT(SELF.Actions, SELF.Actions.Id)
? ASSERT(~ERRORCODE())

DbAuditManager.SetAction PROCEDURE(STRING Action)

  CODE
  SELF.SetAction(THREAD(), Action)

DbAuditManager.GetAction PROCEDURE(UNSIGNED Id)

  CODE
  SELF.SyncAction(id)
  RETURN SELF.Actions.Action

DbAuditManager.GetAction PROCEDURE

  CODE
  RETURN SELF.GetAction(THREAD())

DbAuditManager.SetFM                          PROCEDURE(STRING FileName)
 CODE
   SELF.LogFiles.FileName = FileName
   GET(SELF.LogFiles,SELF.LogFiles.FileName)
   IF ERRORCODE()
      RETURN False
   ELSE
      RETURN True
   END

DbAuditManager.OnDelete                       PROCEDURE(STRING FileName, FILE File)

Cp  CriticalProcedure

 CODE
   Cp.Init(SELF.Crit)
   SELF.SetAction('On Delete')
   SELF.AppendLog(FileName)

DbAuditManager.OnFieldChange     PROCEDURE(*? Left, *? Right, STRING FieldName, STRING FileName)
 CODE

DbAuditManager.OnInsert                       PROCEDURE(STRING FileName, FILE File)

Cp  CriticalProcedure

 CODE
   Cp.Init(SELF.Crit)
   SELF.SetAction('On Insert')
   SELF.AppendLog(FileName)

DbAuditManager.OnChange                       PROCEDURE(STRING FileName, FILE File)

Cp  CriticalProcedure

 CODE
   Cp.Init(SELF.Crit)
   SELF.SetAction('After Change')
   SELF.AppendLog(FileName)

DbAuditManager.BeforeChange                   PROCEDURE(STRING FileName, BufferedPairsClass BFP)

Cp  CriticalProcedure

 CODE
   Cp.Init(SELF.Crit)
   SELF.SetAction('Before Change')
   BFP.AssignLeftToBuffer()
   BFP.AssignRightToLeft
   SELF.AppendLog(FileName)
   BFP.AssignBufferToLeft()


DbAuditManager.IDbChangeAudit.ChangeField     PROCEDURE(*? Left, *? Right, STRING FieldName, STRING FileName)
 CODE
   SELF.OnFieldChange(Left, Right, FieldName, FileName)

DbAuditManager.IDbChangeAudit.OnChange        PROCEDURE(STRING FileName, FILE File)
 CODE
   SELF.OnChange(FileName, File)

DbAuditManager.IDbChangeAudit.BeforeChange    PROCEDURE(STRING FileName, BufferedPairsClass BFP)
 CODE
   SELF.BeforeChange(FileName, BFP)

DbAuditManager.OpenLogFile                    PROCEDURE(STRING FileName)

CreateHeader BYTE

 CODE
   SELF.LogFiles.FileName = FileName
   GET(SELF.LogFiles,SELF.LogFiles.FileName)
   IF NOT ERRORCODE()
      IF SELF.LFM.Opened
         SELF.LFM.Close()
         SELF.LFM.Kill()
         SELF.LFM.Opened = False
      END
      szDbTextLog = CLIP(SELF.LogFiles.LogFileName)
      SELF.LFM.Init(SELF.Errors, SELF.LogFiles.LogFileName)
      CreateHeader = Exists(szDbTextLog)
      IF SELF.LFM.Open()
      ELSE
         IF NOT CreateHeader
            SELF.CreateHeader(FileName, SELF.LFM)
         END
      END
   END
   SELF.LFM.Opened = True

DbAuditManager.CloseLogFile                    PROCEDURE(STRING FileName)

 CODE
   SELF.LogFiles.FileName = FileName
   GET(SELF.LogFiles,SELF.LogFiles.FileName)
   IF NOT ERRORCODE()
      IF SELF.LFM.Opened
         SELF.LFM.Close()
         SELF.LFM.Kill()
         SELF.LFM.Opened = False
         PUT(SELF.LogFiles)
?        ASSERT(~ERRORCODE())
      END
   END





FileOpenServer.Construct PROCEDURE

  CODE
  SELF.Usage = 0


FileOpenServer.Destruct PROCEDURE

  CODE


FileOpenServer.Link PROCEDURE

  CODE
  SELF.Usage += 1


FileOpenServer.Release PROCEDURE

This  &FileOpenServer,AUTO

  CODE
  IF SELF.Usage = 0
    This &= SELF
    DISPOSE(This)
  ELSE
    SELF.Usage -= 1
  END



FileOpenServer.Init PROCEDURE(FILE MyFile)

  CODE
  SELF.MyFile &= MyFile





ThreadedFileOpenServer.IFileOpenServer.OpenFile PROCEDURE(UNSIGNED OpenMode)

  CODE
  OPEN(SELF.MyFile, OpenMode)
  RETURN CHOOSE(ERRORCODE() = NoError OR ERRORCODE() = IsOpenErr, NoError, ERRORCODE())


ThreadedFileOpenServer.IFileOpenServer.CloseFile PROCEDURE

  CODE
  CLOSE(SELF.MyFile)
  RETURN ERRORCODE()


ThreadedFileOpenServer.IFileOpenServer.Link PROCEDURE

  CODE
  SELF.Link()


ThreadedFileOpenServer.IFileOpenServer.Release PROCEDURE

  CODE
  SELF.Release()




UnthreadedFileOpenServer.Construct PROCEDURE

  CODE
  SELF.CSect &= new CriticalSection


UnthreadedFileOpenServer.Destruct PROCEDURE

  CODE
  DISPOSE(SELF.CSect)


UnthreadedFileOpenServer.Init PROCEDURE(FILE MyFile)

  CODE
  PARENT.Init(MyFile)
  SELF.FileUsageCount = 0


UnthreadedFileOpenServer.IFileOpenServer.OpenFile PROCEDURE(UNSIGNED OpenMode)

cp  CriticalProcedure
RetVal  BYTE
  CODE
  cp.init(SELF.CSect.GetIFace())
  IF SELF.FileUsageCount = 0
    OPEN(SELF.MyFile, OpenMode)
    IF ERRORCODE() = NoError OR ERRORCODE() = IsOpenErr
      SELF.FileUsageCount += 1
      RetVal = NoError
    ELSE
      RetVal = ERRORCODE()
    END
  ELSE
    SELF.FileUsageCount += 1
    RetVal = NoError
  END
  RETURN RetVal


UnthreadedFileOpenServer.IFileOpenServer.CloseFile PROCEDURE

cp  CriticalProcedure

  CODE
  cp.init(SELF.CSect.GetIFace())
  IF SELF.FileUsageCount > 0
    IF SELF.FileUsageCount = 1               ! pending close
      CLOSE(SELF.MyFile)
      IF ERRORCODE() THEN RETURN ERRORCODE().
    END
    SELF.FileUsageCount -= 1
  END
  RETURN NoError


UnthreadedFileOpenServer.IFileOpenServer.Link PROCEDURE

cp  CriticalProcedure

  CODE
  cp.init(SELF.CSect.GetIFace())
  SELF.Link()


UnthreadedFileOpenServer.IFileOpenServer.Release PROCEDURE

cp  CriticalProcedure

  CODE
  cp.init(SELF.CSect.GetIFace())
  IF SELF.Usage = 0     ! pending destroy
    UnthreadedFileOpenerMap.FileID = ADDRESS(SELF.MyFile)
    GET(UnthreadedFileOpenerMap, UnthreadedFileOpenerMap.FileID)
    IF ~ERRORCODE()
      DELETE(UnThreadedFileOpenerMap)
?     ASSERT(~ERRORCODE())
    END
    cp.Kill()
  END
  SELF.Release()









CreateThreadedFileOpenServer PROCEDURE(FILE aFile)

srv &ThreadedFileOpenServer,auto

  CODE
  srv &= NEW ThreadedFileOpenServer
  srv.init(aFile)
  return srv.IFileOpenServer


CreateUnthreadedFileOpenServer PROCEDURE(FILE aFile)

  CODE
  UnthreadedFileOpenerMap.FileID = ADDRESS(aFile)
  GET(UnthreadedFileOpenerMap, UnthreadedFileOpenerMap.FileID)
  IF ERRORCODE()
    UnthreadedFileOpenerMap.Opener &= NEW UnthreadedFileOpenServer
    UnthreadedFileOpenerMap.Opener.init(aFile)
    ADD(UnthreadedFileOpenerMap, UnthreadedFileOpenerMap.FileID)
?   ASSERT(~ERRORCODE())
  ELSE
    UnthreadedFileOpenerMap.Opener.Link()
  END
  return UnthreadedFileOpenerMap.Opener.IFileOpenServer


CreateFileOpenServer PROCEDURE(FILE aFile, BOOL threaded)

cp  CriticalProcedure

  CODE
  cp.Init(CreateOpenerCrit.GetIFace())
  IF threaded
    RETURN CreateThreadedFileOpenServer(aFile)
  End
  RETURN CreateUnthreadedFileOpenServer(aFile)


CreateFileOpenServer PROCEDURE(FILE aFile)

cp  CriticalProcedure

  CODE
  cp.Init(CreateOpenerCrit.GetIFace())
  RETURN CreateFileOpenServer(aFile, aFile{PROP:thread})


TransactionManager.AddItem                 PROCEDURE(RelationManager RM,BYTE pCascadeTransaction=1)
 CODE
    SELF.Files.RMInstance = INSTANCE(RM, THREAD())
    GET(SELF.RMList,SELF.RMList.RMInstance)
    IF NOT ERRORCODE() THEN RETURN.
    SELF.Files.RM &= RM
    SELF.Files.Cascade = pCascadeTransaction
    SELF.Files.RMInstance = INSTANCE(RM, THREAD())
    ADD(SELF.Files)

TransactionManager.Reset                   PROCEDURE()
 CODE
    IF SELF.TransactionRuning THEN RETURN.
    FREE(SELF.Files)

TransactionManager.AddFileToLogout         PROCEDURE(RelationManager RM,BYTE pCascadeTransaction)
FileToLogout &FILE
I            LONG,AUTO
lTotal       LONG,AUTO
lRM          &RelationManager
RetVal       BYTE,AUTO
 CODE
    RM.Me.UseFile()
    FileToLogout &= RM.Me.File
    SELF.RMList.RMInstance = INSTANCE(RM, THREAD())
    GET(SELF.RMList,SELF.RMList.RMInstance)
    IF NOT ERRORCODE()
       RETURN Level:Benign
    END
    IF NOT (RM.Me.AliasedFile&=NULL)
       ! It is an ALIAS
       IF NOT RM.Me.File{PROP:DriverLogsOutAlias}
          ! The driver does not support transaction over the ALIAS
          FileToLogout &= RM.Me.AliasedFile.File
       END
    END
    IF FileToLogout{PROP:LOGOUT}<1
       !Never add the same file more than once to a transaction
       FileToLogout{PROP:Logout} = 1
       CASE ERRORCODE()
       OF 0
       OF 80
?         ASSERT(0,'Driver does not support transaction framing.')
       ELSE
         RM.Me.Throw(Msg:LogoutFailed)
         RETURN Level:Notify
       END
    END
    SELF.RMList.RMInstance = INSTANCE(RM, THREAD())
    SELF.RMList.RM  &= RM
    ADD(SELF.RMList)
    RetVal = Level:Benign
    IF pCascadeTransaction
       lTotal = RM.GetNbRelations()
       LOOP I=1 TO lTotal
            lRM &= RM.GetRelation(I)
            RetVal = SELF.AddFileToLogout(lRM,pCascadeTransaction)
            IF RetVal<>Level:Benign
               BREAK
            END
       END
    END
    RETURN RetVal

TransactionManager.SetTimeout              PROCEDURE(BYTE pTimeout)
 CODE
    SELF.LogoutTimeout = pTimeout

TransactionManager.Run                     PROCEDURE(BYTE pTimeout=2)
RetVal  BYTE,AUTO
 CODE
    IF SELF.TransactionRuning THEN RETURN Level:Fatal.
    SELF.LogoutTimeout = pTimeout
    IF SELF.Start()=Level:Benign
         RetVal = SELF.Process()
         SELF.Finish(RetVal)
       RETURN RetVal
    END
    RETURN Level:Fatal

TransactionManager.Start        PROCEDURE()
I       LONG,AUTO
RetVal  BYTE,AUTO
 CODE
    IF SELF.TransactionRuning THEN RETURN Level:Fatal.
    IF SELF.AutoLogoutOff
       FREE(SELF.UselogoutList)
       SELF.LogoutOff = True
    END
    FREE(SELF.RMList)
    LOOP I=1 TO RECORDS(SELF.Files)
         GET(SELF.Files,I)
         IF NOT ERRORCODE()
            IF SELF.AutoLogoutOff
               SELF.SetLogoutOff(SELF.Files.RM)
            END
            RetVal = SELF.AddFileToLogout(SELF.Files.RM,SELF.Files.Cascade)
            IF RetVal<>Level:Benign
               BREAK
            END
         END
    END
    IF RetVal=Level:Benign
       LOGOUT(SELF.LogoutTimeout)
       IF NOT ERRORCODE()
          SELF.TransactionRuning = True
       ELSE
          RetVal=Level:Fatal
          GET(SELF.Files,1)
          IF NOT ERRORCODE()
             SELF.Files.RM.Me.Throw(Msg:LogoutFailed)
          END
       END
    END
    IF RetVal<>Level:Benign
       IF SELF.AutoLogoutOff
          SELF.RestoreLogout()
       END
    END
    RETURN RetVal

TransactionManager.Process      PROCEDURE()
 CODE
    RETURN Level:Benign

TransactionManager.Finish       PROCEDURE(BYTE pErrorLevel)
 CODE
    IF NOT SELF.TransactionRuning THEN RETURN pErrorLevel.
    IF pErrorLevel = Level:Benign
       RETURN SELF.TransactionCommit()
    ELSE
       RETURN SELF.TransactionRollBack()
    END    

TransactionManager.TransactionCommit       PROCEDURE()
RetVal  BYTE,AUTO
 CODE
    IF NOT SELF.TransactionRuning THEN RETURN Level:Benign.
    RetVal = Level:Benign
    COMMIT
    IF ERRORCODE()
         RetVal = Level:Fatal
    END
    IF SELF.AutoLogoutOff
       SELF.RestoreLogout()
    END
    SELF.TransactionRuning = False
    RETURN RetVal

TransactionManager.TransactionRollBack     PROCEDURE()
RetVal  BYTE,AUTO
 CODE
    IF NOT SELF.TransactionRuning THEN RETURN Level:Benign.
    RetVal = Level:Benign
    ROLLBACK
    IF ERRORCODE()
         RetVal = Level:Fatal
    END
    IF SELF.AutoLogoutOff
       SELF.RestoreLogout()
    END
    SELF.TransactionRuning = False
    RETURN RetVal

TransactionManager.Construct               PROCEDURE()
 CODE
    SELF.Files &= NEW(TransactionManagerQueue)
    SELF.RMList&= NEW(FilesOnTransactionQueue)
    SELF.UselogoutList &= NEW(FilesOnTransactionQueue)
    SELF.LogoutTimeout = 2
    SELF.LogoutOff = False
    SELF.AutoLogoutOff = True

TransactionManager.Destruct                PROCEDURE()
 CODE
    IF SELF.LogoutOff
       SELF.RestoreLogout()
    END
    FREE(SELF.Files)
    DISPOSE(SELF.Files)
    FREE(SELF.RMList)
    DISPOSE(SELF.RMList)
    FREE(SELF.UselogoutList)
    DISPOSE(SELF.UselogoutList)

TransactionManager.SetLogoutOff            PROCEDURE()
I            LONG,AUTO
 CODE
    SELF.LogoutOff = True
    SELF.AutoLogoutOff = False
    FREE(SELF.UselogoutList)
    LOOP I=1 TO RECORDS(SELF.Files)
         GET(SELF.Files,I)
         IF NOT ERRORCODE()
            SELF.SetLogoutOff(SELF.Files.RM)
         END
    END

TransactionManager.SetLogoutOff            PROCEDURE(RelationManager RM)
I            LONG,AUTO
lTotal       LONG,AUTO
lRM          &RelationManager
 CODE
    SELF.UselogoutList.RMInstance = INSTANCE(RM, THREAD())
    GET(SELF.UselogoutList,SELF.UselogoutList.RMInstance)
    IF NOT ERRORCODE()
       RETURN
    END
    SELF.UselogoutList.PrevLogout = RM.UseLogout
    SELF.UselogoutList.RMInstance = INSTANCE(RM, THREAD())
    SELF.UselogoutList.RM &= RM
    ADD(SELF.UselogoutList)
    RM.UseLogout = False
    lTotal = RM.GetNbRelations()
    LOOP I=1 TO lTotal
         lRM &= RM.GetRelation(I)
         SELF.SetLogoutOff(lRM)
    END

TransactionManager.RestoreLogout           PROCEDURE()
I            LONG,AUTO
lRM          &RelationManager
 CODE
    IF NOT SELF.LogoutOff THEN RETURN.
    LOOP I=1 TO RECORDS(SELF.UselogoutList)
         GET(SELF.UselogoutList,I)
         IF NOT ERRORCODE()
            lRM &= SELF.UselogoutList.RM
            lRM.UseLogout = SELF.UselogoutList.PrevLogout
         END
    END
    FREE(SELF.UselogoutList)
    SELF.LogoutOff = False
    SELF.AutoLogoutOff = True
