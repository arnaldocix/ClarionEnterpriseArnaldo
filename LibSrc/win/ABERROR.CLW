  MEMBER

  INCLUDE('ABERROR.INC'),ONCE
  INCLUDE('ABERROR.TRN'),ONCE
  INCLUDE('CWSYNCHC.INC'),ONCE

  MAP
Replace       PROCEDURE(STRING Find,STRING Replace,*CSTRING Into),PRIVATE
  END


PNQ         QUEUE,TYPE
Thread        LONG
Name          ASTRING
            END


Window WINDOW('Error'),AT(,,320,96),FONT('Microsoft Sans Serif',8,,FONT:regular,CHARSET:ANSI),CENTER,GRAY,RESIZE
       IMAGE,AT(2,4,26,28),USE(?Image, ImageFeq)
       LIST,AT(32,4,286,71),USE(?List, ListFeq),VSCROLL,FORMAT('217L(2)|M~Message~S(255)@s255@42L(2)|M~Category~L(1)@s255@')
       BUTTON('Button 1'),AT(4,80,36,14),USE(?Button1,BaseButtonFeq + 7),HIDE
       BUTTON('Button 2'),AT(44,80,36,14),USE(?Button2,BaseButtonFeq + 6),HIDE
       BUTTON('Button 3'),AT(84,80,36,14),USE(?Button3,BaseButtonFeq + 5),HIDE
       BUTTON('Button 4'),AT(124,80,36,14),USE(?Button4,BaseButtonFeq + 4),HIDE
       BUTTON('Button 5'),AT(164,80,36,14),USE(?Button5,BaseButtonFeq + 3),HIDE
       BUTTON('Button 6'),AT(204,80,36,14),USE(?Button6,BaseButtonFeq + 2),HIDE
       BUTTON('Button 7'),AT(244,80,36,14),USE(?Button7,BaseButtonFeq + 1),HIDE
       BUTTON('Button 8'),AT(284,80,36,14),USE(?Button8,BaseButtonFeq),HIDE
     END


StdErrorFile  FILE,DRIVER('ASCII'),CREATE,NAME('ABCError.Log')
Record          RECORD
Txt               STRING(1024)
                END
              END

  OMIT('=== DO LINK', lib_mode)
    PRAGMA ('link (C%V%ASC%X%%L%.LIB)')
! === DO LINK

ErrorStatusClass.Construct PROCEDURE

  CODE
  SELF.V.SaveErrorCode = NoError
  SELF.V.SaveError = ''
  SELF.V.SaveFileErrorCode = NoError
  SELF.V.SaveFileError = ''
  SELF.V.FieldName = ''
  SELF.V.FileName = ''
  SELF.V.KeyName = ''
  SELF.V.MessageText = ''
  SELF.V.PrevErrorID = 0

  SELF.V.ErrorBuffer.Id       = 0
  SELF.V.ErrorBuffer.Message  &=NEW(STRING(1))
  SELF.V.ErrorBuffer.Title    &=NEW(STRING(1))
  SELF.V.ErrorBuffer.Fatality = 0
  SELF.V.ErrorBuffer.Category = ''

ErrorStatusClass.Destruct PROCEDURE

  CODE
  DISPOSE(SELF.V.ErrorBuffer.Message)
  DISPOSE(SELF.V.ErrorBuffer.Title)


ErrorClass.Construct PROCEDURE

  CODE
  SELF.critSect &= NewCriticalSection()
  SELF.StdErrorLog &= NULL


ErrorClass.Destruct PROCEDURE

  CODE
  SELF.Kill()
  SELF.critSect.Kill()
! *************************************************************
! This method should be called from a syncronized caller
! that syncronize SELF.Errors using SELF.critSect
! *************************************************************
ErrorClass.SetId PROCEDURE(USHORT Id,UNSIGNED StartPos)
I UNSIGNED,AUTO
   CODE
?   ASSERT(~(Self.Errors &= NULL),'ErrorClass not correctly initialized')
    LOOP I = StartPos TO 1 BY -1
      GET(SELF.Errors,I)
      IF Self.Errors.Id = Id
        RETURN Level:Benign
      END
    END
    RETURN Level:Notify

! *************************************************************
! This method should be called from a syncronized caller
! that syncronizes SELF.Errors using SELF.critSect
! This method remains PROTECTED, but the prototype was changed to
! return the ErrorLevel to force any use of it to verify if
! it is called or not from a safe place.
! *************************************************************
! This routines finds the first occurrence of Id in the queue starting
! from the end of the list. It is -not- a sorted list to allow multiple
! entries with a clear definition of which overrides the other.
ErrorClass.SetId PROCEDURE(USHORT Id)
RetVal BYTE
  CODE
    RetVal = SELF.SetId(ID,RECORDS(Self.Errors))
    ASSERT(RetVal=Level:Benign,'Identifier not found.')
    RETURN RetVal

ErrorClass.GetErrorEntry                   PROCEDURE(USHORT Id,ErrorEntryGroup pErrorEntry)
RetVal  BYTE
  CODE
  RetVal = SELF.GetErrorEntry(Id,pErrorEntry,0)
  ASSERT(~(RetVal=Level:Notify),'Identifier not found.')
  RETURN RetVal

ErrorClass.GetErrorEntry                   PROCEDURE(USHORT Id,*ErrorEntryGroup pErrorEntry,UNSIGNED StartPos)
critProc  CriticalProcedure
RetVal  BYTE
  CODE
    critProc.init(SELF.critSect)
    IF StartPos=0 THEN StartPos=RECORDS(SELF.Errors).
    RetVal = SELF.SetId(ID,StartPos)
    IF RetVal=Level:Benign
       pErrorEntry = SELF.Errors
    END
    RETURN RetVal

ErrorClass.SetErrorEntry                   PROCEDURE(USHORT Id,*ErrorEntryGroup pErrorEntry,UNSIGNED StartPos)
critProc  CriticalProcedure
RetVal  BYTE
  CODE
    critProc.init(SELF.critSect)
    IF StartPos=0 THEN StartPos=RECORDS(SELF.Errors).
    RetVal = SELF.SetId(ID,StartPos)
    IF RetVal=Level:Benign
       SELF.Errors = pErrorEntry
       PUT(SELF.Errors)
       IF ERRORCODE()
          RetVal = Level:Notify
       END
    END
    RETURN RetVal

ErrorClass.StoreErrorEntry                 PROCEDURE(USHORT Id)
lStatus &ErrorStatusClass
EG LIKE(ErrorEntryGroup)
  CODE
?   ASSERT(~(SELF.Status &= NULL),'ErrorClass not correctly initialized with Status')
  lStatus &= INSTANCE(SELF.Status,THREAD())
  lStatus.V.PrevErrorID = lStatus.V.ErrorBuffer.Id
  SELF.GetErrorEntry(Id,EG)
  lStatus.V.ErrorBuffer.Id = EG.Id
  DISPOSE(lStatus.V.ErrorBuffer.Message)
  lStatus.V.ErrorBuffer.Message &= NEW(STRING(LEN(EG.Message)))
  lStatus.V.ErrorBuffer.Message = EG.Message
  DISPOSE(lStatus.V.ErrorBuffer.Title)
  lStatus.V.ErrorBuffer.Title &= NEW(STRING(LEN(EG.Title)))
  lStatus.V.ErrorBuffer.Title = EG.Title
  lStatus.V.ErrorBuffer.Fatality = EG.Fatality
  lStatus.V.ErrorBuffer.Category = EG.Category

! This function is to Do 'innocent' errors such as end of file
! By default it does nothing, you may wish to insert tracing in here for
! debug purposes
ErrorClass.TakeBenign PROCEDURE
  CODE
    RETURN Level:Benign

! This procedure takes a group of type ErrorBlock and translates the
! group into entries in the Errors queue.
ErrorClass.AddErrors PROCEDURE(ErrorBlock ErrsIn)
Follow USHORT(3)
Slen   BYTE,AUTO
Errs   &STRING
critProc  CriticalProcedure
  CODE
    critProc.init(SELF.critSect)
    Errs &= ErrsIn
?   ASSERT(NOT SELF.Errors &= NULL,'ErrorClass incorrectly initialized.')
    LOOP ErrsIn.Number TIMES
      SELF.Errors.Id = VAL(Errs[Follow])+256 * VAL(Errs[Follow+1])      ! Id is a little-endian binary 16 bit word
      Follow += 2
      SELF.Errors.Fatality = VAL(Errs[Follow])
      Follow += 1
      Slen = VAL(Errs[Follow])
      Follow += 1
      SELF.Errors.Title &= Errs[Follow :Follow+Slen-1]
      Follow += Slen
      Slen = VAL(Errs[Follow])
      Follow += 1
      SELF.Errors.Message &= Errs[Follow :Follow+Slen-1]
      Follow += Slen
      SELF.Errors.Category = ''
      ADD(SELF.Errors)
?     ASSERT(~ERRORCODE(),'Unable to add new error message to queue.')
    END

ErrorClass.AddError PROCEDURE(USHORT Id,*STRING Message,*STRING Title,BYTE Fatality,ASTRING Category)
critProc  CriticalProcedure
  CODE
      critProc.init(SELF.critSect)
?     ASSERT(NOT SELF.Errors &= NULL,'ErrorClass incorrectly initialized.')
      SELF.Errors.Id = Id
      SELF.Errors.Fatality = Fatality
      SELF.Errors.Title &= Title
      SELF.Errors.Message &= Message
      SELF.Errors.Category = Category
      ADD(SELF.Errors)
?     ASSERT(~ERRORCODE(),'Unable to add new error message to queue.')

! This is for errors which the program is not expected to Do.
! The default action is to issue a message and HALT
! You may, if you wish, return from this function but it is up to you
! to make sure this 'fatal' condition has been rendered benign
ErrorClass.TakeFatal  PROCEDURE
   CODE
     SELF.Msg(SELF.SubsString() & '  Press OK to end this application', |
             SELF.GetErrorBufferTitle(),ICON:Exclamation,Button:OK,BUTTON:OK,SELF.MsgStyle)
     HALT(0,SELF.GetErrorBufferTitle())
     RETURN Level:Fatal

ErrorClass.TakeNotify   PROCEDURE
   CODE
     SELF.Msg(SELF.SubsString(),SELF.GetErrorBufferTitle(),ICON:Exclamation,|
       Button:OK,BUTTON:OK,SELF.MsgStyle)

! This is purely here to enable people to add their own fatality levels
! By default an unknown fatality is a program error
ErrorClass.TakeOther PROCEDURE
  CODE
    RETURN SELF.TakeProgram()

! This is for errors which really suggest a mal-functioning program
! The default action is simply to make the error fatal
ErrorClass.TakeProgram   PROCEDURE
  CODE
    RETURN SELF.TakeFatal()

! This is really to allow the user to confirm an action
! The simple case is just a yes or no with a yes default
ErrorClass.TakeUser PROCEDURE
  CODE
    IF SELF.Msg(SELF.SubsString(),SELF.GetErrorBufferTitle(),ICON:Question, |
       Button:Yes+Button:No,BUTTON:Yes,SELF.MsgStyle) = Button:Yes
      RETURN Level:Benign
    ELSE
      RETURN Level:Cancel
    END


ErrorClass.Init PROCEDURE(ErrorStatusClass pErrorStatusClass)
critProc  CriticalProcedure
  CODE
     critProc.init(SELF.critSect)
     SELF.Status &= INSTANCE(pErrorStatusClass,0)
     SELF.Init()


ErrorClass.Init PROCEDURE()
critProc  CriticalProcedure
  CODE
  critProc.init(SELF.critSect)
?   ASSERT(~(SELF.Status &= NULL),'ErrorClass not correctly initialized with Status')
  IF SELF.StdErrorLog &= NULL
    SELF.StdErrorLog &= NEW StandardErrorLogClass
    SELF.SetCategory( ,'ABC')
    SELF.Errors &= NULL
    SELF.ProcNames &= NULL
    SELF.History &= NULL
    SELF.SetHistoryThreshold(0)                         !Switch off error history for backwards compatibility
    SELF.SetHistoryViewLevel(Level:Fatal)
    SELF.SetHistoryResetOnView(False)
    SELF.SetErrorLog(SELF.StdErrorLog.ErrorLogInterface)
  END


ErrorClass.SetErrorLog PROCEDURE(ErrorLogInterface ErrLog)
critProc  CriticalProcedure
  CODE
  critProc.init(SELF.critSect)
?   ASSERT(~(SELF.Status &= NULL),'ErrorClass not correctly initialized with Status')
  SELF.ErrorLog &= ErrLog
  IF SELF.Errors &= NULL
    SELF.Errors &= NEW ErrorEntry
  END
  SELF.AddErrors (DefaultErrors)
  IF SELF.ProcNames &= NULL
    SELF.ProcNames &= NEW PNQ
  END
  IF SELF.History &= NULL
    SELF.History &= NEW ErrorHistoryList
  END


ErrorClass.Kill PROCEDURE
critProc  CriticalProcedure
  CODE
  critProc.init(SELF.critSect)
  DISPOSE(SELF.Errors)
  SELF.Errors &= NULL
  DISPOSE(SELF.History)
  SELF.History &= NULL
  DISPOSE(SELF.ProcNames)
  SELF.ProcNames &= NULL
  DISPOSE(SELF.StdErrorLog)
  SELF.StdErrorLog &= NULL


! For people that want to use the ErrorClass as a simple way
! of putting messages on the screen
ErrorClass.Message PROCEDURE(SHORT Id,LONG Buttons,LONG Default)
  CODE
    IF SELF.GetSilent() THEN RETURN Default .

    !SELF.SetId(Id)
    SELF.StoreErrorEntry(Id)
    RETURN SELF.MessageBox(, SELF.SubsString(),SELF.GetErrorBufferTitle(),ICON:Question,Buttons,Default,SELF.MsgStyle)


ErrorClass.GetProcedureName PROCEDURE
critProc  CriticalProcedure
  CODE
  critProc.init(SELF.critSect)
? ASSERT(~(SELF.Status &= NULL),'ErrorClass not correctly initialized with Status')
? ASSERT(~SELF.ProcNames &= NULL,'ErrorClass incorrectly initialized.')
  IF ~RECORDS(SELF.ProcNames) THEN RETURN ''.
  GET(SELF.ProcNames,RECORDS(SELF.ProcNames))
  LOOP WHILE SELF.ProcNames.Thread <> THREAD()
    IF ERRORCODE() THEN RETURN '' .
    GET(SELF.Procnames,POINTER(SELF.ProcNames)-1)
  END
  RETURN SELF.ProcNames.Name

ErrorClass.SetProcedureName PROCEDURE(<STRING S>)
critProc  CriticalProcedure
  CODE
  critProc.init(SELF.critSect)
? ASSERT(~(SELF.Status &= NULL),'ErrorClass not correctly initialized with Status')
  IF OMITTED(2)
    IF SELF.GetProcedureName()
      DELETE(SELF.ProcNames)
    END
  ELSE
    SELF.ProcNames.Name = CLIP(S)
    SELF.Procnames.Thread = THREAD()
    ADD(SELF.ProcNames)
  END

ErrorClass.SetErrors PROCEDURE
lStatus &ErrorStatusClass
  CODE
    ASSERT(~(SELF.Status &= NULL),'ErrorClass not correctly initialized with Status')
    IF NOT SELF.Status &= NULL
       lStatus &= INSTANCE(SELF.Status,THREAD())
       lStatus.V.SaveErrorCode = ERRORCODE()
       lStatus.V.SaveError = CLIP(ERROR())
       lStatus.V.SaveFileErrorCode = CLIP(FILEERRORCODE())
       lStatus.V.SaveFileError = CLIP(FILEERROR())
    END

ErrorClass.SetField PROCEDURE(STRING Name)
  CODE
    SELF.SetFieldName(CLIP(Name))

ErrorClass.SetFile PROCEDURE(STRING Name)
  CODE
    SELF.SetFileName(CLIP(Name))

ErrorClass.SetKey PROCEDURE(STRING Name)
  CODE
    SELF.SetKeyName(CLIP(Name))

ErrorClass.SetFatality PROCEDURE(SHORT Id,BYTE level)
RetVal  BYTE
  CODE
    SELF.critSect.Wait()
    RetVal = SELF.SetId(ID)
    IF RetVal=Level:Benign
       SELF.Errors.Fatality = level
       PUT(SELF.Errors)
       IF ERRORCODE()
          RetVal = Level:Notify
       END
    END
    SELF.critSect.Release()
?   ASSERT(RetVal=Level:Benign,'Unable to change fatality level.')
    RETURN RetVal


! The function takes a description string and substitutes in for these
! %File          -> FileName
! %Key           -> KeyName
! %Error         -> ERROR() string
! %ErrorCode     -> ERRORCODE()
! %FileError     -> FILEERROR()
! %FileErrorCode -> FILEERRORCODE()
! %Message       -> User define message
! %Field         -> Fieldname
! %ErrorText     -> %FileError(%FileErrorCode)
! %Previous      -> Whatever the previous incarnation of this error said
! %Procedure     -> Current procedure name
! %Category      -> The category of the error
ErrorClass.SubsString PROCEDURE
BuildString CSTRING(2000)
EG LIKE(ErrorStatusGroup)
PreviousError LIKE(ErrorEntryGroup)
  CODE
?   ASSERT(~(SELF.Status &= NULL),'ErrorClass not correctly initialized with Status')
    SELF.GetAllStatusVariables(EG)
    BuildString = EG.ErrorBuffer.Message
    Replace('%FileErrorCode',EG.SaveFileErrorCode,BuildString)
    Replace('%FileError',EG.SaveFileError,BuildString)
    Replace('%File',EG.FileName,BuildString)
    Replace('%Key',EG.KeyName,BuildString)
    Replace('%ErrorCode',EG.SaveErrorCode,BuildString)
    IF EG.SaveErrorCode = 90 OR EG.SaveErrorCode = 100 OR EG.SaveErrorCode = 47
      Replace('%ErrorText',EG.SaveFileError&' ('&EG.SaveFileErrorCode&')',BuildString)
    ELSE
      Replace('%ErrorText',EG.SaveError&' ('&EG.SaveErrorCode&')',BuildString)
    END
    Replace('%Error',EG.SaveError,BuildString)
    Replace('%Message',EG.MessageText,BuildString)
    Replace('%Field',EG.FieldName,BuildString)
    Replace('%Procedure',SELF.GetProcedureName(),BuildString)
    Replace('%Category', EG.ErrorBuffer.Category, BuildString)

    IF INSTRING('%Previous',BuildString,1,1)
      IF SELF.GetErrorEntry(EG.PrevErrorID,PreviousError)=Level:Benign
        Replace('%Previous',PreviousError.Message,BuildString)
      ELSE
        Replace('%Previous','',BuildString)
      END
    END
    RETURN BuildString

! The main 'accept error' function. Saves the error state, locates the
! error number and then calls the relevant TakeError function
! The ELSE clause allows 'template' defined errors.
ErrorClass.Throw  PROCEDURE(SHORT Id)
  CODE
    SELF.SetErrors()
    RETURN SELF.TakeError(Id)

ErrorClass.TakeError PROCEDURE(SHORT Id)
  CODE
    !SELF.SetId(Id)
    SELF.StoreErrorEntry(Id)
    CASE SELF.GetErrorBufferFatality()
    OF Level:Benign
      RETURN SELF.TakeBenign()
    OF Level:User
    OROF Level:Cancel
      RETURN SELF.TakeUser()
    OF Level:Program
      RETURN SELF.TakeProgram()
    OF Level:Fatal
      RETURN SELF.TakeFatal()
    OF Level:Notify
      SELF.TakeNotify()
      RETURN Level:Notify
    ELSE
      RETURN SELF.TakeOther()
    END

ErrorClass.ThrowFile PROCEDURE(SHORT Id,STRING File)
  CODE
    SELF.SetFile(File)
    RETURN SELF.Throw(Id)

ErrorClass.ThrowMessage PROCEDURE(SHORT Id,STRING Message)
  CODE
    SELF.SetMessageText(CLIP(Message))
    RETURN SELF.Throw(Id)

ErrorClass.RemoveErrors PROCEDURE(ErrorBlock ErrsIn)

Errs  &STRING
P     USHORT(3)
L     USHORT,AUTO
  CODE
?   ASSERT(~(SELF.Status &= NULL),'ErrorClass not correctly initialized with Status')
  Errs &= ErrsIn
  LOOP ErrsIn.Number TIMES
    L=VAL(Errs[P])+256 * VAL(Errs[P+1])
    P+=3
    P+=VAL(Errs[P])+1
    P+=VAL(Errs[P])+1
    SELF.RemoveError(L)
  END

ErrorClass.RemoveError PROCEDURE(USHORT Id)
lErrorCode  SHORT
  CODE
    lErrorCode = 0
    SELF.critSect.Wait()
    IF SELF.SetId(Id)=Level:Benign
       DELETE(SELF.Errors)
       lErrorCode = ERRORCODE()
    END
    SELF.critSect.Release()
?   ASSERT(~lErrorCode,'Unable to delete error id :'&Id)

ErrorClass.Msg PROCEDURE(STRING Txt, <STRING Caption>, <STRING Icon>, LONG Buttons, LONG DefaultButton, LONG Style)

RVal  LONG,AUTO

  CODE
  RVal = DefaultButton
  IF SELF.GetLogErrors()
    SELF.critSect.Wait()
?   ASSERT(~SELF.ErrorLog &= NULL, 'Attempt to LogErrors with ErrorLogInterface NULL!')
    ASSERT(SELF.ErrorLog.Open() = Level:Benign, 'Unable to Open error log; ' & ERROR())          ! Do not make this conditionally compile in debug
    ASSERT(SELF.ErrorLog.Take(FORMAT(TODAY(), @D17) & '@' & FORMAT(CLOCK(), @T8) & ' --> ' & Txt) = Level:Benign, 'Unable to update error log; ' & ERROR())     ! ditto
    ASSERT(SELF.ErrorLog.Close() = Level:Benign, 'Unable to close error log; ' & ERROR())        ! ditto
    SELF.critSect.Release()
  END
  IF SELF.GetHistoryThreshold()
    SELF.critSect.Wait()
    IF SELF.GetHistoryThreshold() > 0
      LOOP WHILE RECORDS(SELF.History) >= SELF.GetHistoryThreshold()
        GET(SELF.History, 1)                  ! Delete 'scrolled' items from 'top' of list
?       ASSERT(~ERRORCODE())
        DELETE(SELF.History)
?       ASSERT(~ERRORCODE())
      END
    END
    SELF.History.Txt = Txt
    SELF.History.Id = SELF.GetErrorBufferId()
    SELF.History.Category = SELF.GetCategory(SELF.GetErrorBufferId())
    SELF.AddHistory
    SELF.critSect.Release()
  END
  IF ~SELF.GetSilent()
    IF SELF.GetHistoryThreshold()
      IF SELF.GetErrorBufferFatality() = SELF.GetHistoryViewLevel()
        RVal = SELF.HistoryMsg(Caption, Icon, Buttons, DefaultButton)
        IF SELF.GetHistoryResetOnView()
          SELF.ResetHistory
        END
      END
    ELSE
      RVal = SELF.MessageBox(SELF.GetErrorBufferFatality(), Txt, Caption, Icon, Buttons, DefaultButton, Style)
    END
  END
  RETURN RVal


ErrorClass.AddHistory PROCEDURE
RetVal  BYTE
  CODE
  SELF.critSect.Wait()
  ADD(SELF.History)                       ! Add new item to end of list
  RetVal = ERRORCODE()
  SELF.critSect.Release()
? ASSERT(~RetVal,'Error ('&RetVal&') Add new item to History')


ErrorClass.MessageBox PROCEDURE(BYTE Level, STRING Txt, <STRING Caption>, <STRING Icon>, LONG Buttons, LONG DefaultButton, LONG Style)
  CODE

  RETURN MESSAGE(Txt, Caption, Icon, Buttons, DefaultButton, Style)


ErrorClass.ResetHistory PROCEDURE

  CODE
?   ASSERT(~(SELF.Status &= NULL),'ErrorClass not correctly initialized with Status')
  SELF.critSect.Wait()
  FREE(SELF.History)
  SELF.critSect.Release()


ErrorClass.ViewHistory PROCEDURE

  CODE
?   ASSERT(~(SELF.Status &= NULL),'ErrorClass not correctly initialized with Status')

  SELF.HistoryMsg('Errors History', 0, BUTTON:OK + BUTTON:HELP, BUTTON:OK)


ErrorClass.SetCategory PROCEDURE(SHORT Id, ASTRING Category)

  CODE
  IF Id = -1
    SELF.SetDefaultCategory(Category)
  ELSE
    SELF.critSect.Wait()
    IF SELF.SetId(Id) = Level:Benign
       SELF.Errors.Category = Category
       PUT(SELF.Errors)
    END
    SELF.critSect.Release()
?   ASSERT(~ERRORCODE())
  END


ErrorClass.GetCategory PROCEDURE(SHORT Id)
RVal  ASTRING,AUTO
  CODE
  IF Id = -1
    RVal = SELF.GetDefaultCategory()
  ELSE
    SELF.critSect.Wait()
    RVal = CHOOSE(SELF.SetId(Id, RECORDS(SELF.Errors)) = Level:Benign AND SELF.Errors.Category <> '', SELF.Errors.Category, SELF.GetDefaultCategory())
    SELF.critSect.Release()
  END
  RETURN RVal


ErrorClass.HistoryMsg PROCEDURE(STRING Caption, LONG Icon, LONG Buttons, LONG DefaultButton)
MsgBox MsgBoxClass
Hist   HistHandlerClass
lHistory  QUEUE(ErrorHistoryList)
          END
lIndex    SHORT
  CODE
  MsgBox.Init(Window, SELF, Caption, Icon, Buttons, DefaultButton)
  SELF.critSect.Wait()
  LOOP lIndex=1 TO RECORDS(SELF.History)
       GET(SELF.History,lIndex)
       lHistory = SELF.History
       ADD(lHistory)
  END
  SELF.critSect.Release()
  Hist.Init(Window, SELF, lHistory)
  MsgBox.HistoryHandler &= Hist.WindowComponent
  MsgBox.Run
  FREE(lHistory)
  RETURN MsgBox.MsgRVal


ErrorClass.GetErrorCode PROCEDURE(UNSIGNED errType)

  CODE
  CASE(errType)
  OF ErrClarion
    RETURN SELF.GetSaveErrorCode()
  OF ErrFile
    RETURN SELF.GetSaveFileErrorCode()
  END
? ASSERT(False)
  RETURN NoError



ErrorClass.GetError PROCEDURE(UNSIGNED errType)

  CODE
  CASE(errType)
  OF ErrClarion
    RETURN SELF.GetSaveError()
  OF ErrFile
    RETURN SELF.GetSaveFileError()
  END
? ASSERT(False)
  RETURN ''

ErrorClass.GetMsgStyle                     PROCEDURE()
critProc  CriticalProcedure
  CODE
     critProc.init(SELF.critSect)
     RETURN SELF.MsgStyle

ErrorClass.SetMsgStyle                     PROCEDURE(SHORT MsgStyle)
critProc  CriticalProcedure
  CODE
     critProc.init(SELF.critSect)
     SELF.MsgStyle = MsgStyle

ErrorClass.GetDefaultCategory              PROCEDURE()
critProc  CriticalProcedure
  CODE
     critProc.init(SELF.critSect)
     RETURN SELF.DefaultCategory

ErrorClass.SetDefaultCategory              PROCEDURE(ASTRING pDefaultCategory)
critProc  CriticalProcedure
  CODE
     critProc.init(SELF.critSect)
     SELF.DefaultCategory = pDefaultCategory

ErrorClass.SetSilent                       PROCEDURE(BYTE pSilent=1)
critProc  CriticalProcedure
  CODE
     critProc.init(SELF.critSect)
     SELF.Silent = pSilent

ErrorClass.GetSilent                       PROCEDURE()
critProc  CriticalProcedure
  CODE
     critProc.init(SELF.critSect)
     RETURN SELF.Silent

ErrorClass.SetLogErrors                    PROCEDURE(BYTE pLogErrors=1)
critProc  CriticalProcedure
  CODE
     critProc.init(SELF.critSect)
     SELF.LogErrors = pLogErrors

ErrorClass.GetLogErrors                    PROCEDURE()
critProc  CriticalProcedure
  CODE
     critProc.init(SELF.critSect)
     RETURN SELF.LogErrors

ErrorClass.SetHistoryThreshold             PROCEDURE(LONG pHThreshold=-1)
critProc  CriticalProcedure
  CODE
     critProc.init(SELF.critSect)
     SELF.HistoryThreshold = pHThreshold

ErrorClass.GetHistoryThreshold             PROCEDURE()
critProc  CriticalProcedure
  CODE
     critProc.init(SELF.critSect)
     RETURN SELF.HistoryThreshold

ErrorClass.SetHistoryViewLevel             PROCEDURE(LONG pHViewLevel)
critProc  CriticalProcedure
  CODE
     critProc.init(SELF.critSect)
     SELF.HistoryViewLevel = pHViewLevel

ErrorClass.GetHistoryViewLevel             PROCEDURE()
critProc  CriticalProcedure
  CODE
     critProc.init(SELF.critSect)
     RETURN SELF.HistoryViewLevel

ErrorClass.SetHistoryResetOnView           PROCEDURE(BYTE pHResetOnView=1)
critProc  CriticalProcedure
  CODE
     critProc.init(SELF.critSect)
     SELF.HistoryResetOnView = pHResetOnView

ErrorClass.GetHistoryResetOnView           PROCEDURE()
critProc  CriticalProcedure
  CODE
     critProc.init(SELF.critSect)
     RETURN SELF.HistoryResetOnView

ErrorClass.GetFileName                     PROCEDURE()
lStatus &ErrorStatusClass
  CODE
?   ASSERT(~(SELF.Status &= NULL),'ErrorClass not correctly initialized with Status')
  lStatus &= INSTANCE(SELF.Status,THREAD())
  RETURN lStatus.V.FileName

ErrorClass.SetFileName                     PROCEDURE(STRING pFileName)
lStatus &ErrorStatusClass
  CODE
?   ASSERT(~(SELF.Status &= NULL),'ErrorClass not correctly initialized with Status')
  lStatus &= INSTANCE(SELF.Status,THREAD())
  lStatus.V.FileName = pFileName

ErrorClass.GetFieldName                    PROCEDURE()
lStatus &ErrorStatusClass
  CODE
?   ASSERT(~(SELF.Status &= NULL),'ErrorClass not correctly initialized with Status')
  lStatus &= INSTANCE(SELF.Status,THREAD())
  RETURN lStatus.V.FieldName

ErrorClass.SetFieldName                    PROCEDURE(STRING pFieldName)
lStatus &ErrorStatusClass
  CODE
?   ASSERT(~(SELF.Status &= NULL),'ErrorClass not correctly initialized with Status')
  lStatus &= INSTANCE(SELF.Status,THREAD())
  lStatus.V.FieldName = pFieldName

ErrorClass.GetKeyName                      PROCEDURE()
lStatus &ErrorStatusClass
  CODE
?   ASSERT(~(SELF.Status &= NULL),'ErrorClass not correctly initialized with Status')
  lStatus &= INSTANCE(SELF.Status,THREAD())
  RETURN lStatus.V.KeyName

ErrorClass.SetKeyName                      PROCEDURE(STRING pKeyName)
lStatus &ErrorStatusClass
  CODE
?   ASSERT(~(SELF.Status &= NULL),'ErrorClass not correctly initialized with Status')
  lStatus &= INSTANCE(SELF.Status,THREAD())
  lStatus.V.KeyName = pKeyName

ErrorClass.GetMessageText                  PROCEDURE()
lStatus &ErrorStatusClass
  CODE
?   ASSERT(~(SELF.Status &= NULL),'ErrorClass not correctly initialized with Status')
  lStatus &= INSTANCE(SELF.Status,THREAD())
  RETURN lStatus.V.MessageText

ErrorClass.SetMessageText                  PROCEDURE(STRING pMessageText)
lStatus &ErrorStatusClass
  CODE
?   ASSERT(~(SELF.Status &= NULL),'ErrorClass not correctly initialized with Status')
  lStatus &= INSTANCE(SELF.Status,THREAD())
  lStatus.V.MessageText = pMessageText

ErrorClass.GetSaveError                    PROCEDURE()
lStatus &ErrorStatusClass
  CODE
?   ASSERT(~(SELF.Status &= NULL),'ErrorClass not correctly initialized with Status')
  lStatus &= INSTANCE(SELF.Status,THREAD())
  RETURN lStatus.V.SaveError

ErrorClass.SetSaveError                    PROCEDURE(STRING pSaveError)
lStatus &ErrorStatusClass
  CODE
?   ASSERT(~(SELF.Status &= NULL),'ErrorClass not correctly initialized with Status')
  lStatus &= INSTANCE(SELF.Status,THREAD())
  lStatus.V.SaveError = pSaveError

ErrorClass.GetSaveErrorCode                PROCEDURE()
lStatus &ErrorStatusClass
  CODE
?   ASSERT(~(SELF.Status &= NULL),'ErrorClass not correctly initialized with Status')
  lStatus &= INSTANCE(SELF.Status,THREAD())
  RETURN lStatus.V.SaveErrorCode

ErrorClass.SetSaveErrorCode                PROCEDURE(LONG pSaveErrorCode)
lStatus &ErrorStatusClass
  CODE
?   ASSERT(~(SELF.Status &= NULL),'ErrorClass not correctly initialized with Status')
  lStatus &= INSTANCE(SELF.Status,THREAD())
  lStatus.V.SaveErrorCode = pSaveErrorCode

ErrorClass.GetSaveFileError                PROCEDURE()
lStatus &ErrorStatusClass
  CODE
?   ASSERT(~(SELF.Status &= NULL),'ErrorClass not correctly initialized with Status')
  lStatus &= INSTANCE(SELF.Status,THREAD())
  RETURN lStatus.V.SaveFileError

ErrorClass.SetSaveFileError                PROCEDURE(STRING pSaveFileError)
lStatus &ErrorStatusClass
  CODE
?   ASSERT(~(SELF.Status &= NULL),'ErrorClass not correctly initialized with Status')
  lStatus &= INSTANCE(SELF.Status,THREAD())
  lStatus.V.SaveFileError = pSaveFileError

ErrorClass.GetSaveFileErrorCode            PROCEDURE()
lStatus &ErrorStatusClass
  CODE
?   ASSERT(~(SELF.Status &= NULL),'ErrorClass not correctly initialized with Status')
  lStatus &= INSTANCE(SELF.Status,THREAD())
  RETURN lStatus.V.SaveFileErrorCode

ErrorClass.SetSaveFileErrorCode            PROCEDURE(STRING pSaveFileErrorCode)
lStatus &ErrorStatusClass
  CODE
?   ASSERT(~(SELF.Status &= NULL),'ErrorClass not correctly initialized with Status')
  lStatus &= INSTANCE(SELF.Status,THREAD())
  lStatus.V.SaveFileErrorCode = pSaveFileErrorCode

ErrorClass.SetPrevErrorID                  PROCEDURE(UNSIGNED pErrorID)
lStatus &ErrorStatusClass
  CODE
?   ASSERT(~(SELF.Status &= NULL),'ErrorClass not correctly initialized with Status')
  lStatus &= INSTANCE(SELF.Status,THREAD())
  lStatus.V.PrevErrorID = pErrorID

ErrorClass.GetPrevErrorID                  PROCEDURE()
lStatus &ErrorStatusClass
  CODE
?   ASSERT(~(SELF.Status &= NULL),'ErrorClass not correctly initialized with Status')
  lStatus &= INSTANCE(SELF.Status,THREAD())
  RETURN lStatus.V.PrevErrorID

ErrorClass.SetErrorBufferId                PROCEDURE(UNSIGNED pErrorID)
lStatus &ErrorStatusClass
  CODE
?   ASSERT(~(SELF.Status &= NULL),'ErrorClass not correctly initialized with Status')
  lStatus &= INSTANCE(SELF.Status,THREAD())
  lStatus.V.ErrorBuffer.Id = pErrorId

ErrorClass.GetErrorBufferId                PROCEDURE()
lStatus &ErrorStatusClass
  CODE
?   ASSERT(~(SELF.Status &= NULL),'ErrorClass not correctly initialized with Status')
  lStatus &= INSTANCE(SELF.Status,THREAD())
  RETURN lStatus.V.ErrorBuffer.Id

ErrorClass.SetErrorBufferMessage           PROCEDURE(STRING pMessage)
lStatus &ErrorStatusClass
  CODE
?   ASSERT(~(SELF.Status &= NULL),'ErrorClass not correctly initialized with Status')
  lStatus &= INSTANCE(SELF.Status,THREAD())
  DISPOSE(lStatus.V.ErrorBuffer.Message)
  lStatus.V.ErrorBuffer.Message &= NEW(STRING(LEN(pMessage)))
  lStatus.V.ErrorBuffer.Message = pMessage

ErrorClass.GetErrorBufferMessage           PROCEDURE()
lStatus &ErrorStatusClass
  CODE
?   ASSERT(~(SELF.Status &= NULL),'ErrorClass not correctly initialized with Status')
  lStatus &= INSTANCE(SELF.Status,THREAD())
  RETURN lStatus.V.ErrorBuffer.Message

ErrorClass.SetErrorBufferTitle             PROCEDURE(STRING pTitle)
lStatus &ErrorStatusClass
  CODE
?   ASSERT(~(SELF.Status &= NULL),'ErrorClass not correctly initialized with Status')
  lStatus &= INSTANCE(SELF.Status,THREAD())
  DISPOSE(lStatus.V.ErrorBuffer.Title)
  lStatus.V.ErrorBuffer.Title &= NEW(STRING(LEN(pTitle)))
  lStatus.V.ErrorBuffer.Title = pTitle

ErrorClass.GetErrorBufferTitle             PROCEDURE()
lStatus &ErrorStatusClass
  CODE
?   ASSERT(~(SELF.Status &= NULL),'ErrorClass not correctly initialized with Status')
  lStatus &= INSTANCE(SELF.Status,THREAD())
  RETURN lStatus.V.ErrorBuffer.Title

ErrorClass.SetErrorBufferFatality          PROCEDURE(BYTE pFatality)
lStatus &ErrorStatusClass
  CODE
?   ASSERT(~(SELF.Status &= NULL),'ErrorClass not correctly initialized with Status')
  lStatus &= INSTANCE(SELF.Status,THREAD())
  lStatus.V.ErrorBuffer.Fatality = pFatality

ErrorClass.GetErrorBufferFatality          PROCEDURE()
lStatus &ErrorStatusClass
  CODE
?   ASSERT(~(SELF.Status &= NULL),'ErrorClass not correctly initialized with Status')
  lStatus &= INSTANCE(SELF.Status,THREAD())
  RETURN lStatus.V.ErrorBuffer.Fatality

ErrorClass.SetErrorBufferCategory          PROCEDURE(ASTRING pCategory)
lStatus &ErrorStatusClass
  CODE
?   ASSERT(~(SELF.Status &= NULL),'ErrorClass not correctly initialized with Status')
  lStatus &= INSTANCE(SELF.Status,THREAD())
  lStatus.V.ErrorBuffer.Category = pCategory

ErrorClass.GetErrorBufferCategory          PROCEDURE()
lStatus &ErrorStatusClass
  CODE
?   ASSERT(~(SELF.Status &= NULL),'ErrorClass not correctly initialized with Status')
  lStatus &= INSTANCE(SELF.Status,THREAD())
  RETURN lStatus.V.ErrorBuffer.Category

ErrorClass.GetAllStatusVariables           PROCEDURE(*ErrorStatusGroup pErrorStatusGroup)
lStatus &ErrorStatusClass
  CODE
?   ASSERT(~(SELF.Status &= NULL),'ErrorClass not correctly initialized with Status')
  lStatus &= INSTANCE(SELF.Status,THREAD())
  pErrorStatusGroup = lStatus.V




MsgBoxClass.Init PROCEDURE(WINDOW Win, ErrorClass Err, <STRING Caption>, LONG Icon, LONG Buttons, LONG DefaultButton, LONG Style)
  CODE

  SELF.Win &= Win
  SELF.Err &= Err
  SELF.Caption &= NEW STRING(LEN(Caption))
  SELF.Caption = Caption
  SELF.Icon = Icon
  SELF.ButtonTypes = Buttons
  SELF.MsgRVal = DefaultButton
  SELF.Style = Style


MsgBoxClass.Init PROCEDURE

RVal  BYTE,AUTO
i     LONG,AUTO
Feq   UNSIGNED,AUTO

  CODE
  RVal = PARENT.Init()
  IF RVal = Level:Benign
    IF ~(SELF.HistoryHandler &= NULL)
      SELF.AddItem(SELF.HistoryHandler)
    END

    OPEN(SELF.Win)
    SELF.Opened = True
    IF SELF.Caption
      SELF.Win{PROP:Text} = SELF.Caption
    END
    SELF.MsgButtons = 0
    LOOP i = 7 TO 0 BY -1
      IF BAND(SELF.ButtonTypes, 2 ^ i)
        SELF.MsgButtons += 1
        SELF.MsgButtonID[SELF.MsgButtons] = 2 ^ i
        Feq=BaseButtonFeq + SELF.MsgButtons - 1
        Feq{PROP:Text} = CHOOSE(i + 1, 'O&k', '&Yes', '&No', '&Abort', '&Retry', '&Ignore', '&Cancel', '&Help')
        IF SELF.MsgRVal = 2 ^ i
          Feq{PROP:Default} = True
          SELECT(Feq)
        END
        UNHIDE(Feq)
      END
    END
    SELF.SetupAdditionalFeqs
  END
  RETURN RVal


MsgBoxClass.Kill PROCEDURE

RVal  BYTE,AUTO

  CODE
  RVal = PARENT.Kill()
  IF RVal = Level:Benign
    DISPOSE(SELF.Caption)

    CLOSE(SELF.Win)
    SELF.Opened = False
  END
  RETURN RVal


MsgBoxClass.FetchFeq PROCEDURE(BYTE Btn)
i LONG,AUTO
  CODE

  LOOP i = 1 TO SELF.MsgButtons
    IF SELF.MsgButtonID[i] = Btn
      RETURN BaseButtonFeq + i - 1
    END
  END
  RETURN 0


MsgBoxClass.FetchStdButton PROCEDURE(SHORT Feq)
i LONG,AUTO
  CODE

  LOOP i = BaseButtonFeq TO BaseButtonFeq + SELF.MsgButtons - 1
    IF Feq = i
      RETURN SELF.MsgButtonID[i - BaseButtonFeq + 1]
    END
  END
  RETURN 0


MsgBoxClass.TakeAccepted PROCEDURE

RVal  BYTE,AUTO
i     LONG,AUTO

  CODE
  RVal = PARENT.TakeAccepted()
  IF RVal = Level:Benign

    i = SELF.FetchStdButton(ACCEPTED())
    IF i
      SELF.MsgRVal = i
      RETURN Level:Fatal
    END
  END

  RETURN RVal


MsgBoxClass.SetupAdditionalFeqs PROCEDURE
  CODE

  IF SELF.Icon = 0
     ImageFeq{PROP:Text} = ICON:Exclamation
  ELSE
     ImageFeq{PROP:Text} = SELF.Icon
  END



HistHandlerClass.Init PROCEDURE(WINDOW Win, ErrorClass Err, ErrorHistoryList History)
  CODE

  SELF.Win &= Win
  SELF.Err &= Err
  SELF.History &= History


HistHandlerClass.TakeEvent PROCEDURE
  CODE

  CASE EVENT()
  OF EVENT:OpenWindow
    ListFeq{PROP:Selected} = RECORDS(SELF.History)

    SELF.LBColumns = SELF.Win $ ListFeq {PROPLIST:Exists, 0}
    SELF.Win $ ListFeq{PROP:VLBVal} = ADDRESS(SELF)
    SELF.Win $ ListFeq{PROP:VLBProc} = ADDRESS(SELF._VLBProc)
  END
  RETURN Level:Benign


HistHandlerClass._VLBProc PROCEDURE(LONG RowNum, SHORT ColNum)
  CODE

  RETURN SELF.VLBProc(RowNum, ColNum)


HistHandlerClass.VLBProc PROCEDURE(LONG RowNum, SHORT ColNum)
  CODE

  CASE RowNum
  OF -1                                                     ! How many rows
    RETURN RECORDS(SELF.History)
  OF -2                                                     ! How many cols
    RETURN SELF.LBColumns
  OF -3
    RETURN True
  ELSE
    GET(SELF.History, RowNum)
?   ASSERT(~ERRORCODE())
    CASE ColNum
    OF 1
      RETURN SELF.History.Txt
    OF 2
      RETURN SELF.History.Category
    END
  END
  RETURN ''


HistHandlerClass.WindowComponent.Kill PROCEDURE
  CODE


HistHandlerClass.WindowComponent.Reset PROCEDURE(BYTE Force)
  CODE


HistHandlerClass.WindowComponent.ResetRequired PROCEDURE
  CODE

  RETURN False


HistHandlerClass.WindowComponent.SetAlerts PROCEDURE
  CODE


HistHandlerClass.WindowComponent.TakeEvent PROCEDURE
  CODE

  RETURN SELF.TakeEvent()


HistHandlerClass.WindowComponent.Update PROCEDURE
  CODE


HistHandlerClass.WindowComponent.UpdateWindow PROCEDURE
  CODE

  DISPLAY(ListFeq)





! The routine replaces instances of 'find' with 'replace' in 'into'
Replace       PROCEDURE(string find,string replace,*cstring into)
Locate LONG,AUTO
  CODE
    IF UPPER(find)<>UPPER(replace)
       LOOP
         Locate = INSTRING(UPPER(find),UPPER(into),1,1)
         IF ~Locate THEN RETURN .
         into = SUB(into,1,Locate-1) & replace & SUB(into,Locate+LEN(find),LEN(into))
       END
    END


StandardErrorLogClass.Construct PROCEDURE

  CODE
  SELF.Usage = 0


StandardErrorLogClass.Destruct PROCEDURE

  CODE
  SELF.Close(True)


StandardErrorLogClass.Open PROCEDURE(BYTE Force)
critProc  CriticalProcedure
  CODE
  critProc.init(SELF.critSect)
  IF ~SELF.Usage OR Force                       !Allows nested 'opens' should the need arise
    IF ~EXISTS(NAME(StdErrorFile))
      CREATE(StdErrorFile)
      IF ERRORCODE() THEN RETURN Level:Notify.
    END
    OPEN(StdErrorFile, 42h)                     !Read/write deny none
    IF ERRORCODE() THEN RETURN Level:Notify.
  END
  SELF.Usage += 1
  RETURN Level:Benign


StandardErrorLogClass.Close PROCEDURE(BYTE Force)
critProc  CriticalProcedure
  CODE
  critProc.init(SELF.critSect)
  IF SELF.Usage = 1 OR Force
    CLOSE(StdErrorFile)
    IF ERRORCODE() THEN RETURN Level:Notify.
    SELF.Usage = 1
  END
  SELF.Usage -= 1
  RETURN Level:Benign


StandardErrorLogClass.ErrorLogInterface.Open PROCEDURE(BYTE Force)

  CODE
  RETURN SELF.Open(Force)


StandardErrorLogClass.ErrorLogInterface.Close PROCEDURE(BYTE Force)

  CODE
  RETURN SELF.Close(Force)


StandardErrorLogClass.ErrorLogInterface.Take PROCEDURE(STRING Txt)
critProc  CriticalProcedure
  CODE
  critProc.init(SELF.critSect)
  IF SELF.Usage
    StdErrorFile.Txt = Txt
    ADD(StdErrorFile)
    RETURN CHOOSE(ERRORCODE() = 0, Level:Benign, Level:Notify)
  END
  RETURN Level:Notify

