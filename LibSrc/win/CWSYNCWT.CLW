          MEMBER

          INCLUDE ('CWSYNCHM.INC'),ONCE
          INCLUDE ('CWSYNCHC.INC'),ONCE

          INCLUDE ('CWSYNCWT.INC'),ONCE

  PRAGMA('project(#compile CWSYNCLP.CLW)')
          
  MAP
_StartProcessThreaded_  PROCEDURE(STRING pSELF),PRIVATE
    MODULE('CWSYNCLP')
      __CallAddr(LONG),PROC,NAME('__CallAddr')
    END
  END

NOTIFICATION:WorkingThreadFinish EQUATE(1020)
NOTIFICATION:WorkingThreadUpdate EQUATE(1021)
    
_StartProcessThreaded_  PROCEDURE(STRING pSELF)
SSELF &WorkingThreadManager
  CODE
  SSELF &= pSELF+0
  SSELF.StartProcessThreaded()
 
WorkingThreadManager.Construct               PROCEDURE()
 CODE
    SELF._WorkingThreadNumber = 0
    SELF._StackSize           = 25000
    SELF._IsCancelled         = False
    SELF._IsPaused            = False
    SELF._Finish              = True

    SELF._Lock_WorkingThread  &= NewCriticalSection()

    SELF._AccessTo_ClassData     &= NewReaderWriterLock()
    SELF._AccessTo_IsCancelled   &= NewReaderWriterLock()
    SELF._AccessTo_IsPaused      &= NewReaderWriterLock()
    SELF._AccessTo_StackSize     &= NewReaderWriterLock()



WorkingThreadManager.Destruct                PROCEDURE()
 CODE
    SELF._Lock_WorkingThread.Kill()

    SELF._AccessTo_IsCancelled.Kill()
    SELF._AccessTo_IsPaused.Kill()
    SELF._AccessTo_StackSize.Kill()
    SELF._AccessTo_ClassData.Kill()

WorkingThreadManager.ResetProcess            PROCEDURE()    
 CODE
    SELF._Lock_WorkingThread.Wait()
    IF SELF._WorkingThreadNumber > 0 AND SELF._IsPaused = False AND SELF._Finish = True
       SELF._IsPaused           = False
       SELF._Finish             = True
       SELF._IsCancelled        = False
       SELF._WorkingThreadNumber = 0
       SELF._CallingThreadNumber= 0
    END
    SELF._Lock_WorkingThread.Release()

WorkingThreadManager.StartProcess            PROCEDURE(BYTE pUseNotification = 1)
 CODE
    IF SELF.IsStarted() = FALSE
       SELF.StartProcess(pUseNotification, 0, 0, 0)
    END

WorkingThreadManager.StartProcess            PROCEDURE(BYTE pUseNotification = 1, LONG pWorkingThreadProcAddress, LONG pWhenFinishProcAddress, LONG pCallerUpdateAddress)
 CODE
    IF SELF.IsStarted() = FALSE
       SELF._ThreadProcAddress        = pWorkingThreadProcAddress
       SELF._WhenFinishProcAddress    = pWhenFinishProcAddress
       SELF._CallerUpdateAddress      = pCallerUpdateAddress
       SELF._UseNotification          = pUseNotification      
       SELF._Lock_WorkingThread.Wait()
       SELF.InitializeProcess()
       SELF._Finish = False
       SELF._IsCancelled = False
       SELF._CallingThreadNumber = THREAD()
       SELF._WorkingThreadNumber = START (_StartProcessThreaded_,SELF._StackSize, ADDRESS(SELF))       
       RESUME(SELF._WorkingThreadNumber)
       SELF._IsPaused = FALSE
       SELF._Lock_WorkingThread.Release()
       
    END

WorkingThreadManager.InitializeProcess       PROCEDURE()
 CODE
    !This should be used to initialize the local procedure variables associated with the WorkingThreadManager
    !It will be executed in the same thread than the StartProcess

WorkingThreadManager.StartProcessThreaded    PROCEDURE()
 CODE
    SELF.DoWorkingThreadProcess()
    SELF._Finish = True
    IF SELF._UseNotification
       NOTIFY(NOTIFICATION:WorkingThreadFinish,SELF._CallingThreadNumber,SELF._WorkingThreadNumber)
    ELSE
       SELF.DoWhenProcessFinish()
    END
   
WorkingThreadManager.PauseProcess            PROCEDURE()
CP CriticalProcedure
 CODE
    CP.Init(SELF._Lock_WorkingThread)
    IF SELF._WorkingThreadNumber>0 AND SELF._IsPaused = False
       SUSPEND(SELF._WorkingThreadNumber)
       SELF._IsPaused = True
    END
    
WorkingThreadManager.ResumeProcess           PROCEDURE()
CP CriticalProcedure
 CODE
    CP.Init(SELF._Lock_WorkingThread)
    IF SELF._WorkingThreadNumber>0 AND SELF._IsPaused = True
       RESUME(SELF._WorkingThreadNumber)
       SELF._IsPaused = False
    END

WorkingThreadManager.CancelProcess           PROCEDURE()
CP CriticalProcedure
 CODE
    CP.Init(SELF._AccessTo_IsCancelled.Writer())  
    SELF._IsCancelled = True
  
WorkingThreadManager.TakeEvent               PROCEDURE() 
Ncode   UNSIGNED
NThread SIGNED
NParam  LONG
 CODE
    IF EVENT() = EVENT:Notify
       IF NOTIFICATION(Ncode, NThread, NParam)
          IF NParam = SELF._WorkingThreadNumber 
             CASE Ncode 
             OF NOTIFICATION:WorkingThreadFinish
                IF SELF._Finish = True
                   SELF.DoWhenProcessFinish()
                END
             OF NOTIFICATION:WorkingThreadUpdate
                SELF.DoCallerUpdate()
             END
          END
       END
    END
    
WorkingThreadManager.WorkingThreadProcess    PROCEDURE()
 CODE
    !Virtual
    !in the derived method write the code that will need to be run in the working thread

WorkingThreadManager.WhenProcessFinish       PROCEDURE()
 CODE
    !Virtual
    !This code will be executed in the calling thread IF the pUseNotification parameter was used (default to true) when the process was started

WorkingThreadManager.DoWorkingThreadProcess  PROCEDURE()
 CODE
    IF SELF._ThreadProcAddress = 0
       SELF.WorkingThreadProcess()
    ELSE
       __CallAddr(SELF._ThreadProcAddress)
    END 

WorkingThreadManager.UpdateCaller            PROCEDURE()
 CODE
    IF SELF._UseNotification
       NOTIFY(NOTIFICATION:WorkingThreadUpdate,SELF._CallingThreadNumber,SELF._WorkingThreadNumber)
    ELSE
       SELF.DoCallerUpdate()
    END

WorkingThreadManager.DoCallerUpdate          PROCEDURE()
 CODE
    IF SELF._CallerUpdateAddress = 0
       SELF.UpdatingCaller()
    ELSE
       __CallAddr(SELF._CallerUpdateAddress)
    END 
       
WorkingThreadManager.UpdatingCaller            PROCEDURE()
 CODE

WorkingThreadManager.DoWhenProcessFinish       PROCEDURE()
CP CriticalProcedure
 CODE
    CP.Init(SELF._Lock_WorkingThread)
    IF SELF._WhenFinishProcAddress = 0
       SELF.WhenProcessFinish()
    ELSE
       __CallAddr(SELF._WhenFinishProcAddress)
    END

WorkingThreadManager.IsPaused                PROCEDURE()
CP CriticalProcedure
 CODE
    CP.Init(SELF._AccessTo_IsPaused.Reader())
    RETURN SELF._IsPaused

WorkingThreadManager.IsCancelled             PROCEDURE()
CP CriticalProcedure
 CODE
    CP.Init(SELF._AccessTo_IsCancelled.Reader())
    RETURN SELF._IsCancelled

WorkingThreadManager.IsStarted               PROCEDURE()    
CP CriticalProcedure
 CODE
    CP.Init(SELF._Lock_WorkingThread)
    IF SELF._WorkingThreadNumber > 0
       RETURN TRUE
    ELSE
       RETURN False
    END

WorkingThreadManager.IsProcessFinish         PROCEDURE()
CP CriticalProcedure
 CODE
    CP.Init(SELF._Lock_WorkingThread)
    RETURN SELF._Finish
    
WorkingThreadManager.AssignToClass         PROCEDURE(BYTE pVal=0)
CP CriticalProcedure
 CODE
    CP.Init(SELF._AccessTo_ClassData.Writer())
    SELF.DoAssignToClassTmp(pVal)
    
WorkingThreadManager.AssignFromClass       PROCEDURE(BYTE pVal=0)
CP CriticalProcedure
 CODE
    CP.Init(SELF._AccessTo_ClassData.Reader())
    SELF.DoAssignFromClassTmp(pVal)
    
WorkingThreadManager.DoAssignToClassTmp       PROCEDURE(BYTE pVal=0)
 CODE
    !Virtual method
    !In the derived method local variables values can be assigned to the class fields
    !The call to this methods is synchronised
    
WorkingThreadManager.DoAssignFromClassTmp     PROCEDURE(BYTE pVal=0)
 CODE
    !Virtual method
    !In the derived method class fields values can be assigned to local variables
    !The call to this methods is synchronised

WorkingThreadManager.WriteToClassTmp            PROCEDURE(BYTE pVal=0)
CP CriticalProcedure
 CODE
    CP.Init(SELF._AccessTo_ClassData.Writer())
    SELF.DoWriteToClassTmp(pVal)
    
WorkingThreadManager.ReadFromClassTmp          PROCEDURE(BYTE pVal=0)
CP CriticalProcedure
 CODE
    CP.Init(SELF._AccessTo_ClassData.Reader())
    SELF.DoReadFromClassTmp(pVal)
    
WorkingThreadManager.DoWriteToClassTmp          PROCEDURE(BYTE pVal=0)
 CODE
    !Virtual method
    !In the derived method local variables values can be assigned to the class fields
    !The call to this methods is synchronised
    
WorkingThreadManager.DoReadFromClassTmp         PROCEDURE(BYTE pVal=0)
 CODE
    !Virtual method
    !In the derived method class fields values can be assigned to local variables
    !The call to this methods is synchronised

WorkingThreadManager.GetStackSize            PROCEDURE()
CP CriticalProcedure
 CODE
    CP.Init(SELF._AccessTo_StackSize.Reader())
    RETURN SELF._StackSize
WorkingThreadManager.SetStackSize            PROCEDURE(UNSIGNED pValue)
CP CriticalProcedure
 CODE
    CP.Init(SELF._AccessTo_StackSize.Writer())    
    SELF._StackSize = pValue

WorkingThreadManager.GetWorkingThreadNumber  PROCEDURE()
 CODE
    RETURN SELF._WorkingThreadNumber