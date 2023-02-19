 MEMBER

   INCLUDE('ABTOOLBA.INC'),ONCE
   INCLUDE('ABWINDOW.INC'),ONCE
   INCLUDE('ABERROR.INC'),ONCE
   INCLUDE('ABUserControl.INC'),ONCE

!UserControlClass.TakeFieldEvent PROCEDURE()
! CODE
! RETURN SELF.TakeFieldEvent(SELF.GetOriginalFEQ(FIELD())) 	  

!UserControlClass.TakeFieldEvent PROCEDURE(SIGNED control)
! CODE
! RETURN PARENT.TakeFieldEvent()    

UserControlClass.UpdateWindow PROCEDURE()
Ndx      SIGNED
 CODE
 LOOP Ndx = 1 TO RECORDS(SELF.FEQList)
   GET(SELF.FEQList,Ndx)
   DISPLAY(SELF.FEQList.HostedFEQ)
 END

UserControlClass.AddFEQ PROCEDURE(SIGNED pOriginalFEQ,SIGNED pHostedFEQ)
  CODE
  IF NOT pOriginalFEQ OR NOT pHostedFEQ
    RETURN Level:Cancel
  END
  IF SELF.GetHostedFEQ(pHostedFEQ) > 0
    DELETE(SELF.FEQList)
  END
  SELF.FEQList.OriginalFEQ = pOriginalFEQ
  SELF.FEQList.HostedFEQ   = pHostedFEQ
  ADD(SELF.FEQList)
  RETURN Level:Benign

UserControlClass.Init PROCEDURE(Window pTarget,SIGNED pParentFEQ=0,<LONG pXPos>,<LONG pYPos>)
  CODE
  RETURN Level:Benign
!
!This MUST be called from the derived Init method
!
UserControlClass.Open PROCEDURE(Window pControl, Window pTarget,SIGNED pParentFEQ=0,<LONG pXPos>,<LONG pYPos>)
  CODE
  OPEN(pControl)
  HIDE(0)
  SELF.Init()
  SELF.Clone(pTarget,pParentFEQ,pXPos,pYPOS)
  CLOSE(pControl)
  RETURN Level:Benign

UserControlClass.Kill PROCEDURE()
  CODE
  IF NOT SELF.Dead
    FREE(SELF.FEQList)
    RETURN PARENT.Kill()
  END
  RETURN Level:Benign

UserControlClass.Clone PROCEDURE(Window pTarget,SIGNED pParentFEQ=0,<LONG pXPos>,<LONG pYPos>)

Omit:pXPos EQUATE(4)
Omit:pYPos EQUATE(5)

OrigX    SIGNED
OrigY    SIGNED
FEQNewX  SIGNED
FEQNewY  SIGNED

Ndx      SIGNED
FEQ      SIGNED

tmpFEQList QUEUE
FEQ         SIGNED
ParentFEQ   SIGNED
X           SIGNED
Y           SIGNED
W           SIGNED
H           SIGNED
           END
X       LONG       
  CODE
  SELF.parentWindow &= System{PROP:Target}
  FREE(tmpFEQList)
  LOOP Ndx = FirstField() TO LastField()
    tmpFEQList.FEQ = Ndx
    tmpFEQList.ParentFEQ = Ndx{PROP:Parent}
    GETPOSITION(Ndx,tmpFEQList.X,tmpFEQList.Y,tmpFEQList.W,tmpFEQList.H)
    ADD(tmpFEQList)
  END
  SETTARGET(pTarget)
  GETPOSITION(pParentFEQ,OrigX,OrigY)
  LOOP Ndx = 1 TO RECORDS(tmpFEQList)
    GET(tmpFEQList,Ndx)
    IF tmpFEQList.ParentFEQ
      FEQ = CLONE(0,tmpFEQList.FEQ,SELF.GetHostedFEQ(tmpFEQList.ParentFEQ),,SELF.parentWindow)
    ELSE
      FEQ = CLONE(0,tmpFEQList.FEQ,pParentFEQ,,SELF.parentWindow)
    END
    !How to know when the controls does not have a Use variable to not assign this
    !X = tmpFEQList.FEQ{PROP:UseAddress}
    !MESSAGE(x)
    !IF tmpFEQList.FEQ{PROP:UseAddress} <> 0
      !FEQ{PROP:Use} = (tmpFEQList.FEQ{PROP:UseAddress})
    !END
    IF OMITTED(Omit:pXPos)
      FEQNewX = tmpFEQList.X 
    ELSE
      FEQNewX = tmpFEQList.X + pXPos + OrigX
    END
    IF OMITTED(Omit:pYPos)
      FEQNewY = tmpFEQList.Y 
    ELSE
      FEQNewY = tmpFEQList.Y + pYPos + OrigY
    END
    SETPOSITION(FEQ,FEQNewX,FEQNewY,tmpFEQList.W,tmpFEQList.H)
    SELF.AddFEQ(tmpFEQList.FEQ,FEQ)
  END
  SETTARGET()
  RETURN Level:Benign


UserControlClass.Construct          PROCEDURE()
  CODE
  SELF.FEQList &= NEW UserControlFEQList
  CLEAR(SELF.FEQList)

UserControlClass.Destruct           PROCEDURE()
  CODE
  IF NOT SELF.Dead
    SELF.Kill()
  END
  IF NOT SELF.FEQList &= NULL
    FREE(SELF.FEQList)
    DISPOSE(SELF.FEQList)
  END

UserControlClass.GetOriginalFEQ PROCEDURE(SIGNED pHostedFEQ)
  CODE
  SELF.FEQList.HostedFEQ = pHostedFEQ
  GET(SELF.FEQList,SELF.FEQList.HostedFEQ)
  IF ERRORCODE()
    RETURN -1000
  END
  RETURN SELF.FEQList.OriginalFEQ

UserControlClass.GetHostedFEQ PROCEDURE(SIGNED pOriginalFEQ)
  CODE
  SELF.FEQList.OriginalFEQ = pOriginalFEQ
  GET(SELF.FEQList,SELF.FEQList.OriginalFEQ)
  IF ERRORCODE()
    RETURN -1000
  END
  RETURN SELF.FEQList.HostedFEQ

UserControlClass.BindToControl      PROCEDURE(SIGNED pOriginalFEQ,*? SelfProperty)
  CODE
  (SELF.GetHostedFEQ(pOriginalFEQ)){PROP:Use} = SelfProperty

UserControlClass.Contents           PROCEDURE(SIGNED pOriginalFEQ)
  CODE
  RETURN CONTENTS(SELF.GetHostedFEQ(pOriginalFEQ))

UserControlClass.Change             PROCEDURE(SIGNED pOriginalFEQ,STRING value)
  CODE
  CHANGE(SELF.GetHostedFEQ(pOriginalFEQ), value)

UserControlContainerClass.Construct PROCEDURE()
  CODE
  SELF.Controls &= NEW UserControlList

UserControlContainerClass.Destruct PROCEDURE()
  CODE 
  IF NOT SELF.Controls &= NULL
    DISPOSE(SELF.Controls)
  END

UserControlContainerClass.AddItem   PROCEDURE(UserControlClass pUserControl,LONG pParentFEQ=0,<LONG pXPos>,<LONG pYPos>)
  CODE
  ASSERT(NOT SELF.parentWindow &= NULL,'SELF.parentWindow Not Initialized')

  SELF.Controls.Control &= pUserControl
  ADD(SELF.Controls)
  SELF.Controls.Control.Init(SELF.parentWindow,pParentFEQ,pXPOS,pYPOS)
  RETURN Level:Benign

UserControlContainerClass.Init PROCEDURE(Window pTarget)
  CODE
  SELF.parentWindow &= pTarget
  SELF.Init()

UserControlContainerClass.Init PROCEDURE()
  CODE

UserControlContainerClass.TakeEvent PROCEDURE()
Ndx LONG
  CODE
  LOOP Ndx = 1 TO RECORDS(SELF.Controls)
    GET(SELF.Controls,Ndx)
    SELF.Controls.Control.TakeEvent()
  END

UserControlContainerClass.UpdateWindow PROCEDURE
Ndx LONG
  CODE
  LOOP Ndx = 1 TO RECORDS(SELF.Controls)
    GET(SELF.Controls,Ndx)
    IF NOT ERRORCODE()
      SELF.Controls.Control.UpdateWindow()
    END
  END

UserControlContainerClass.Kill PROCEDURE()
Ndx LONG
  CODE
  LOOP Ndx = 1 TO RECORDS(SELF.Controls)
    GET(SELF.Controls,Ndx)
    IF NOT ERRORCODE()
      SELF.Controls.Control.Kill()
    END
  END

UserControlContainerClass.WindowComponent.Kill                          PROCEDURE()
  CODE
  SELf.Kill()

UserControlContainerClass.WindowComponent.Reset                         PROCEDURE(BYTE Force)
  CODE
UserControlContainerClass.WindowComponent.ResetRequired                 PROCEDURE()!,BYTE      ! 1 if reset of window required
  CODE
  RETURN False

UserControlContainerClass.WindowComponent.SetAlerts                     PROCEDURE()
  CODE

UserControlContainerClass.WindowComponent.TakeEvent                     PROCEDURE()!,BYTE
  CODE
  SELF.TakeEvent()
  RETURN Level:Benign

UserControlContainerClass.WindowComponent.Update                        PROCEDURE()           ! Everything but the window!
  CODE
UserControlContainerClass.WindowComponent.UpdateWindow                  PROCEDURE()
  CODE
  SELF.UpdateWindow()
