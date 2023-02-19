  MEMBER

  PRAGMA ('define(init_priority=>19)')

    INCLUDE('TRIGGER.INC'),ONCE

    MAP
DupString                       PROCEDURE(STRING),*STRING,PRIVATE
    END

TriggerManager.Construct PROCEDURE
  CODE
    SELF.Setup()
    IF NOT SELF.File &=NULL
       CALLBACK(SELF.File, SELF.FileCallBackInterface)
    END

TriggerManager.Destruct PROCEDURE
  CODE
    IF NOT SELF.PreviousBuffer &= NULL
       DISPOSE(SELF.PreviousBuffer)
    END
    IF NOT SELF.File &=NULL
       CALLBACK(SELF.File, SELF.FileCallBackInterface,True)
    END

TriggerManager.Setup                           PROCEDURE()
  CODE
    !The template must derive the method and
    !add the following two lines:
    !SELF.File &= MyFile
    !SELF.Buffer &= My:Record

TriggerManager.Setup                           PROCEDURE(*FILE TheFile,*GROUP TheBuffer)
  CODE
    IF SELF.File &=NULL
       SELF.File &= TheFile
       SELF.Buffer &= TheBuffer
       CALLBACK(SELF.File, SELF.FileCallBackInterface)
    END

TriggerManager.FunctionCalled                  PROCEDURE(SIGNED opCode, *Params Parameters, *CSTRING ErrCode, *CSTRING ErrMsg)
RetVal  BYTE,AUTO
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
  ELSE
    RetVal = TRUE
  END
  Parameters.StopOperation = SELF.StopOperation
  RETURN RetVal

TriggerManager.FunctionDone                    PROCEDURE(SIGNED opCode, *Params Parameters, *CSTRING ErrCode, *CSTRING ErrMsg)
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

TriggerManager.FileCallBackInterface.FunctionCalled PROCEDURE(SIGNED opCode, *Params Parameters, *CSTRING ErrCode, *CSTRING ErrMsg)
  CODE
  RETURN SELF.FunctionCalled(opCode,Parameters,ErrCode,ErrMsg)

TriggerManager.FileCallBackInterface.FunctionDone PROCEDURE(SIGNED opCode, *Params Parameters, *CSTRING ErrCode, *CSTRING ErrMsg)
  CODE
  CASE opCode
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
  END
  RETURN SELF.FunctionDone(opCode,Parameters,ErrCode,ErrMsg)

TriggerManager.PreDelete PROCEDURE(*CSTRING ErrCode, *CSTRING ErrMsg)

  CODE
  RETURN True


TriggerManager.PostDelete PROCEDURE(*CSTRING ErrCode, *CSTRING ErrMsg)

  CODE
  RETURN True


TriggerManager.PreInsert PROCEDURE(SIGNED OpCode, UNSIGNED AddLen, *CSTRING ErrCode, *CSTRING ErrMsg)

  CODE
  RETURN True


TriggerManager.PostInsert PROCEDURE(*CSTRING ErrCode, *CSTRING ErrMsg)

  CODE
  RETURN True


TriggerManager.PreUpdate PROCEDURE(LONG Pntr, UNSIGNED PutLen, *CSTRING ErrCode, *CSTRING ErrMsg)

  CODE
  RETURN True


TriggerManager.PostUpdate PROCEDURE(*CSTRING ErrCode, *CSTRING ErrMsg)

  CODE
  RETURN True

TriggerManager.PreFetch PROCEDURE(SIGNED OpCode, KEY key, STRING positionBuffer, LONG pointer, UNSIGNED recLen, *CSTRING ErrCode, *CSTRING ErrMsg)

  CODE
  RETURN True

TriggerManager.PostFetch    PROCEDURE(SIGNED OpCode, KEY key, STRING positionBuffer, LONG pointer, UNSIGNED recLen, *CSTRING ErrCode, *CSTRING ErrMsg)

  CODE
  RETURN True

TriggerManager.PreCreate    PROCEDURE(*CSTRING ErrCode, *CSTRING ErrMsg)

  CODE
  RETURN True

TriggerManager.PostCreate   PROCEDURE(*CSTRING ErrCode, *CSTRING ErrMsg)

  CODE
  RETURN True


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
