!Resize method implementations

  MEMBER()

  INCLUDE('ABRESIZE.INC'),ONCE
  INCLUDE('ABERROR.INC'),ONCE

  MAP
  END


WindowResizeClass.Init PROCEDURE(BYTE AppStrategy,BYTE SetWindowMinSize,BYTE SetWindowMaxSize)
  CODE
  SELF.ControlQueue &= NEW ControlQueue
  SELF.ResizeList   &= NEW ResizeQueue
  SELF.AppStrategy   = AppStrategy
  REGISTEREVENT (EVENT:DoResize,  ADDRESS(SELF.TakeResize), ADDRESS(SELF))
  REGISTEREVENT (EVENT:Restored,  ADDRESS(SELF.TakeResize), ADDRESS(SELF))
  REGISTEREVENT (EVENT:GainFocus, ADDRESS(SELF.TakeResize), ADDRESS(SELF))
  REGISTEREVENT (EVENT:Maximized, ADDRESS(SELF.TakeResize), ADDRESS(SELF))

  0{PROP:IMM} = TRUE
  SELF.Reset

  IF AppStrategy = AppStrategy:Surface OR AppStrategy = AppStrategy:SurfaceRight
    SELF.SetParentDefaults
  END
  IF SetWindowMinSize
    0{PROP:MinWidth}  = SELF.OrigWin.Width
    0{PROP:MinHeight} = SELF.OrigWin.Height
  END
  IF SetWindowMaxSize
    0{PROP:MaxWidth}  = SELF.OrigWin.Width
    0{PROP:MaxHeight} = SELF.OrigWin.Height
  END
  SELF.DeferMoves = TRUE
  RETURN

WindowResizeClass.GetPositionStrategy PROCEDURE(SHORT ControlType,<BYTE AppStrategy>)

AppStrat  BYTE,AUTO
RVal      USHORT(Resize:Reposition)

  CODE
  AppStrat = CHOOSE (NOT OMITTED(3), AppStrategy, SELF.AppStrategy)
  CASE AppStrat
  OF AppStrategy:Centered
    RVal = Resize:FixXCenter+Resize:FixYCenter
  OF AppStrategy:NoResize
    RVal = Resize:LockXPos + Resize:LockYPos
  OF AppStrategy:Spread
    CASE ControlType
    OF CREATE:Button
    OROF CREATE:Radio
    OROF CREATE:Check
      RVal = Resize:FixNearestX + Resize:FixNearestY
    END
  OF AppStrategy:Surface
    CASE ControlType
    OF CREATE:Button
    OROF CREATE:Radio
    OROF CREATE:Check
      RVal = Resize:FixNearestX + Resize:FixNearestY
    OF CREATE:Entry
    OROF CREATE:Combo
    OROF CREATE:Spin
    OROF CREATE:DropCombo
    OROF CREATE:String
    OROF CREATE:SString
    OROF CREATE:Prompt
      RVal = Resize:FixTop
    OF CREATE:List
    OROF CREATE:Sheet
    OROF CREATE:Panel
    OROF CREATE:Image
    OROF CREATE:Option
    OROF CREATE:Group
    OROF CREATE:Region
      RVal = Resize:FixLeft + Resize:FixTop
    END
  OF AppStrategy:SurfaceRight
    CASE ControlType
    OF CREATE:Button
    OROF CREATE:Radio
    OROF CREATE:Check
      RVal = Resize:FixNearestX + Resize:FixNearestY
    OF CREATE:String
    OROF CREATE:SString
    OROF CREATE:Prompt
      RVal = Resize:FixLeft+Resize:FixTop
    OF CREATE:Entry
    OROF CREATE:Combo
    OROF CREATE:Spin
    OROF CREATE:DropCombo
      RVal = Resize:FixLeft+Resize:FixTop
    OF CREATE:List
    OROF CREATE:Sheet
    OROF CREATE:Panel
    OROF CREATE:Image
    OROF CREATE:Option
    OROF CREATE:Group
    OROF CREATE:Region
    OROF CREATE:Text
    OROF CREATE:RTF
      RVal = Resize:FixLeft + Resize:FixTop
    END
  END
  RETURN RVal


WindowResizeClass.GetResizeStrategy PROCEDURE(SHORT ControlType,<BYTE AppStrategy>)

AppStrat  BYTE,AUTO
RVal      USHORT(Resize:Resize)

  CODE
  AppStrat = CHOOSE (NOT OMITTED(3), AppStrategy, SELF.AppStrategy)
  CASE AppStrat
  OF AppStrategy:Centered
    RVal = Resize:LockSize
  OF AppStrategy:NoResize
    RVal = Resize:LockWidth + Resize:LockHeight
  OF AppStrategy:Spread
    CASE SELF.ControlQueue.Type
    OF CREATE:Button
    OROF CREATE:Radio
    OROF CREATE:Check
      RVal = Resize:LockWidth + Resize:LockHeight
    OF CREATE:Entry
    OROF CREATE:Combo
    OROF CREATE:Spin
    OROF CREATE:DropCombo
      RVal = Resize:LockHeight
    END
  OF AppStrategy:Surface
    CASE SELF.ControlQueue.Type
    OF CREATE:Button
    OROF CREATE:Radio
    OROF CREATE:Check
      RVal = Resize:LockWidth + Resize:LockHeight
    OF CREATE:Entry
    OROF CREATE:Combo
    OROF CREATE:Spin
    OROF CREATE:DropCombo
    OROF CREATE:String
    OROF CREATE:SString
    OROF CREATE:Prompt
      RVal = Resize:LockHeight
    OF CREATE:List
    OROF CREATE:Sheet
    OROF CREATE:Panel
    OROF CREATE:Image
    OROF CREATE:Option
    OROF CREATE:Group
    OROF CREATE:Region
      RVal = Resize:ConstantRight + Resize:ConstantBottom
    END
  OF AppStrategy:SurfaceRight
    CASE SELF.ControlQueue.Type
    OF CREATE:Button
    OROF CREATE:Radio
    OROF CREATE:Check
      RVal = Resize:LockWidth + Resize:LockHeight
    OF CREATE:String
    OROF CREATE:SString
    OROF CREATE:Prompt
      RVal = Resize:LockSize
    OF CREATE:Entry
    OROF CREATE:Combo
    OROF CREATE:Spin
    OROF CREATE:DropCombo
      RVal = Resize:ConstantRight+Resize:LockHeight
    OF CREATE:List
    OROF CREATE:Sheet
    OROF CREATE:Panel
    OROF CREATE:Image
    OROF CREATE:Option
    OROF CREATE:Group
    OROF CREATE:Region
    OROF CREATE:Text
    OROF CREATE:RTF
      RVal = Resize:ConstantRight + Resize:ConstantBottom
    END
  END
  RETURN RVal


WindowResizeClass.Reset PROCEDURE()

FieldCounter  SIGNED(0)

  CODE
  SELF.GetSizeInfo (0,SELF.OrigWin)
  SELF.OrigWin.Maximized = 0{PROP:Maximize}
  SELF.OrigWin.Iconized  = 0{PROP:Iconize}
  SELF.PreviousWin = SELF.OrigWin
  LOOP
    FieldCounter = 0{PROP:NextField,FieldCounter}
    IF FieldCounter = 0
      BREAK
    END

    SELF.ControlQueue.ID = FieldCounter
    GET (SELF.ControlQueue, SELF.ControlQueue.ID)

    IF ERRORCODE()
      SELF.ControlQueue.Type = FieldCounter{PROP:Type}
      SELF.ControlQueue.HasChildren = FALSE
      IF NOT FieldCounter{PROP:InToolbar} AND INRANGE (SELF.ControlQueue.Type, 1, CREATE:Toolbar-1)
        CASE SELF.ControlQueue.Type
        OF CREATE:Menu
        OROF CREATE:Item
        OROF CREATE:Tab
        OROF CREATE:Menubar
        OROF CREATE:Application
        OROF CREATE:Window
        ELSE
          SELF.ControlQueue.ID = FieldCounter
          SELF.GetSizeInfo (FieldCounter, SELF.ControlQueue.Pos)

          !List boxes with drop attribute are treated like entry fields
          IF SELF.ControlQueue.Type = CREATE:List AND SELF.ControlQueue.ID{PROP:Drop}
            SELF.ControlQueue.Type = CREATE:Entry
          END

          SELF.ControlQueue.PositionalStrategy = SELF.GetPositionStrategy (SELF.ControlQueue.Type)
          SELF.ControlQueue.ResizeStrategy     = SELF.GetResizeStrategy   (SELF.ControlQueue.Type)
          SELF.ControlQueue.ParentID = 0

          ADD (SELF.ControlQueue, SELF.ControlQueue.ID)
?         ASSERT(~ERRORCODE(),ERROR())
        END
      END
    ELSE
      SELF.GetSizeInfo (FieldCounter, SELF.ControlQueue.Pos)
      PUT (SELF.ControlQueue, SELF.ControlQueue.ID)
?     ASSERT(~ERRORCODE(),ERROR())
    END
  END

  SELF.ResizeCalled = FALSE
  RETURN

WindowResizeClass.RestoreWindow PROCEDURE()           !reset whole window back to init size/position
  CODE
  0{PROP:Maximize} = SELF.OrigWin.Maximized
  0{PROP:Iconize}  = SELF.OrigWin.Iconized
  SETPOSITION (0,,, SELF.OrigWin.Width, SELF.OrigWin.Height)
  SELF.RestoreControls()
  RETURN

WindowResizeClass.WindowResizeBegin PROCEDURE()
  CODE

WindowResizeClass.WindowResizeEnd PROCEDURE()
  CODE

WindowResizeClass.TakeResize PROCEDURE()
  CODE
  RETURN SELF.Resize()


WindowResizeClass.Resize PROCEDURE()
  CODE
  RETURN SELF.Resize(0)

WindowResizeClass.Resize PROCEDURE(SIGNED Control)

CurrentSize    LIKE(PositionGroup),AUTO
WinPos         LIKE(PositionGroup),AUTO
WinCurrentPos  LIKE(PositionGroup),AUTO
i              UNSIGNED,AUTO
SaveDefer      SIGNED,AUTO

  CODE
  FREE (SELF.ResizeList)

  SELF.GetSizeInfo (Control, CurrentSize)

  ! Check that resize required
  
  IF LASTFIELD() <> 0 AND |
     NOT (Control = 0 AND CurrentSize.Width = SELF.PreviousWin.Width AND CurrentSize.Height = SELF.PreviousWin.Height)
    IF Control = 0
       SELF.WindowResizeBegin()
    END
    IF SELF.DeferMoves THEN
      SaveDefer = SYSTEM{PROP:DeferMove}
      SYSTEM{PROP:DeferMove} = RECORDS(SELF.ControlQueue)
    END
    IF CurrentSize.Width = SELF.OrigWin.Width AND CurrentSize.Height = SELF.OrigWin.Height     !window reset to design size
      SELF.RestoreControls()
    ELSE
      IF Control = 0
        WinPos = SELF.OrigWin
        WinPos.XPos = 0
        WinPos.YPos = 0
        WinCurrentPos = CurrentSize
        WinCurrentPos.XPos = 0
        WinCurrentPos.YPos = 0
      ELSE
        SELF.GetOrigPos (Control, WinPos)
        SELF.GetCurrentPos (Control, WinCurrentPos)
      END

      SELF.ResizeChildren (Control, WinPos, WinCurrentPos)

      IF NOT SELF.DeferMoves
        SELF.SetPriorities (CurrentSize)
        IF (SELF.PreviousWin.Width^2)+(SELF.PreviousWin.Height^2) > (CurrentSize.Width^2)+(CurrentSize.Height^2)
          SORT (SELF.ResizeList, +SELF.ResizeList.Priority)   !Contraction
        ELSE
          SORT (SELF.ResizeList, -SELF.ResizeList.Priority)   !Expansion
        END
      END

      LOOP i = 1 TO RECORDS(SELF.ResizeList)
        GET (SELF.ResizeList,i)
        IF SELF.ResizeList.Before <> SELF.ResizeList.After
          SELF.SetSize (SELF.ResizeList.ControlID, SELF.ResizeList.After)
        END
      END
    END
    IF SELF.DeferMoves
      SYSTEM{PROP:DeferMove} = CHOOSE (SaveDefer < 0, SaveDefer, 0)
    END

    SELF.ResizeCalled = TRUE
    SELF.PreviousWin  = CurrentSize
    IF Control = 0
       SELF.WindowResizeEnd()
    END
  END
  RETURN Level:Benign


WindowResizeClass.Kill PROCEDURE()
  CODE
  UNREGISTEREVENT (EVENT:DoResize,  ADDRESS(SELF.TakeResize), ADDRESS(SELF))
  UNREGISTEREVENT (EVENT:Restored,  ADDRESS(SELF.TakeResize), ADDRESS(SELF))
  UNREGISTEREVENT (EVENT:GainFocus, ADDRESS(SELF.TakeResize), ADDRESS(SELF))
  UNREGISTEREVENT (EVENT:Maximized, ADDRESS(SELF.TakeResize), ADDRESS(SELF))
  DISPOSE (SELF.ControlQueue)
  DISPOSE (SELF.ResizeList)
  RETURN

WindowResizeClass.SetAnchor   PROCEDURE (SIGNED Control, USHORT AnchorStrategy)
lPositionalStrategy  USHORT,AUTO
lResizeStrategy      USHORT,AUTO
lMove                BYTE
  CODE
  lPositionalStrategy = 0
  lResizeStrategy     = 0

  IF BAND (AnchorStrategy, Anchor:Right + Anchor:Left) = Anchor:Right + Anchor:Left
    lPositionalStrategy = Resize:FixLeft
    lResizeStrategy     = Resize:ConstantRight
  ELSIF BAND (AnchorStrategy, Anchor:GrowRight + Anchor:GrowLeft) = Anchor:GrowRight + Anchor:GrowLeft
    lPositionalStrategy = Resize:FixLeft
    lResizeStrategy     = Resize:ConstantRight
  ELSIF BAND (AnchorStrategy, Anchor:Right)
    IF BAND (AnchorStrategy, Anchor:GrowLeft)
      lPositionalStrategy = Resize:FixXToCenter
      lResizeStrategy     = Resize:ConstantRight
    ELSE
      lPositionalStrategy = Resize:FixRight
      lResizeStrategy     = Resize:LockWidth
    END
  ELSIF BAND (AnchorStrategy, Anchor:Left)
    lPositionalStrategy = Resize:FixLeft
    IF BAND (AnchorStrategy, Anchor:GrowRight)
      lResizeStrategy     = Resize:ConstantRightCenter
    ELSE
      lResizeStrategy     = Resize:LockWidth
    END
  ELSIF BAND (AnchorStrategy, Anchor:GrowRight)
    lPositionalStrategy = Resize:FixXToCenter
    lResizeStrategy     = Resize:Resize
  ELSIF BAND (AnchorStrategy, Anchor:GrowLeft)
    lPositionalStrategy = Resize:Reposition
    lResizeStrategy     = Resize:ConstantRightCenter
  ELSE
    lMove               = 1
    lPositionalStrategy = Resize:FixXToCenter
  END

  IF BAND (AnchorStrategy, Anchor:Top + Anchor:Bottom) = Anchor:Top + Anchor:Bottom
    lPositionalStrategy += Resize:FixTop
    lResizeStrategy     += Resize:ConstantBottom
  ELSIF BAND (AnchorStrategy, Anchor:GrowTop + Anchor:GrowBottom) = Anchor:GrowTop + Anchor:GrowBottom
    lPositionalStrategy += Resize:FixYCenter
    lResizeStrategy     += Resize:ConstantBottomCenter
  ELSIF BAND (AnchorStrategy,Anchor:Top)
    IF BAND (AnchorStrategy, Anchor:GrowBottom)
      lPositionalStrategy += Resize:FixTop
      lResizeStrategy     += Resize:ConstantBottomCenter
    ELSE
      lPositionalStrategy += Resize:FixTop
      lResizeStrategy     += Resize:LockHeight
    END
  ELSIF BAND (AnchorStrategy, Anchor:Bottom)
    IF BAND (AnchorStrategy, Anchor:GrowTop)
      lPositionalStrategy += Resize:FixYToCenter
      lResizeStrategy     += Resize:ConstantBottom
    ELSE
      lPositionalStrategy += Resize:FixBottom
      lResizeStrategy     += Resize:LockHeight
    END
  ELSIF BAND (AnchorStrategy, Anchor:GrowBottom)
    lPositionalStrategy += Resize:FixYToCenter
    lResizeStrategy     += Resize:Resize
  ELSIF BAND (AnchorStrategy, Anchor:GrowTop)
    lPositionalStrategy += Resize:Reposition
    lResizeStrategy     += Resize:ConstantBottomCenter
  ELSE
    lMove               += 2
    lPositionalStrategy += Resize:FixYToCenter
    lResizeStrategy     += Resize:LockHeight
  END

  SELF.SetStrategy (Control, lPositionalStrategy, lResizeStrategy)
  RETURN

WindowResizeClass.SetAnchor   PROCEDURE (SIGNED Control, USHORT AnchorStrategy, SIGNED ParentFeq)
  CODE
  SELF.SetAnchor (Control, AnchorStrategy)
  SELF.SetParentControl (Control, ParentFeq)
  RETURN

WindowResizeClass.SetStrategy PROCEDURE (SIGNED ControlID, USHORT PositionalStrategy, USHORT ResizeStrategy)
FieldCounter   UNSIGNED,AUTO
CtlType        SIGNED,AUTO
  CODE
  IF ControlID = 0
    LOOP FieldCounter = 1 TO RECORDS(SELF.ControlQueue)
      GET (SELF.ControlQueue, FieldCounter)
?     ASSERT(~ERRORCODE(),ERROR())

      DO AddStrategy
    END
  ELSE
    SELF.ControlQueue.ID = ControlID
    GET (SELF.ControlQueue, SELF.ControlQueue.ID)

    IF ERRORCODE()                                    !New control, so add it
      CtlType = ControlID{PROP:Type}

?     ASSERT(~ControlID{PROP:InToolbar},'SetStrategy should not be called with a control in the toolbar')              !Cannot resize controls in toolbars
?     ASSERT(INRANGE (CtlType, 1, CREATE:Toolbar-1),'Known control types')  !Known control types
?     ASSERT(CtlType <> CREATE:Menu,'SetStrategy should not be called with a control of type Menu')                  !No resize for menus
?     ASSERT(CtlType <> CREATE:Item,'SetStrategy should not be called with a control of type Menu Item')                  !No resize for menu items
?     ASSERT(CtlType <> CREATE:Tab,'SetStrategy should not be called with a control of type Tab')                   !No resize for tabs use SHEET
    
      SELF.ControlQueue.ID   = ControlID
      SELF.ControlQueue.Type = CtlType
      SELF.ControlQueue.HasChildren = FALSE
      SELF.GetSizeInfo (ControlID, SELF.ControlQueue.Pos)

      ADD (SELF.ControlQueue, SELF.ControlQueue.ID)
?     ASSERT(~ERRORCODE(),ERROR())
    END
    DO AddStrategy
  END

AddStrategy ROUTINE
  SELF.ControlQueue.PositionalStrategy = PositionalStrategy
  SELF.ControlQueue.ResizeStrategy     = ResizeStrategy
  PUT (SELF.ControlQueue)
? ASSERT(~ERRORCODE(),ERROR())

  IF SELF.ControlQueue.ParentID = 0
     SELF.SetParentDefault()
  END
  EXIT

WindowResizeClass.SetStrategy PROCEDURE (SIGNED SrcCtrl, SIGNED DestCtrl)
PosStrat  USHORT,AUTO
ResStrat  USHORT,AUTO
  CODE
  SELF.ControlQueue.ID = SrcCtrl
  GET (SELF.ControlQueue, SELF.ControlQueue.ID)
  IF ERRORCODE() = 0
    PosStrat = SELF.ControlQueue.PositionalStrategy
    ResStrat = SELF.ControlQueue.ResizeStrategy

    SELF.SetStrategy (DestCtrl, PosStrat, ResStrat)
  END
  RETURN

!This function removes a control from the resizer, disabling any resizing

WindowResizeClass.RemoveControl PROCEDURE (SIGNED Control)
DelPrnt  SIGNED,AUTO
i        UNSIGNED,AUTO
  CODE
  SELF.ControlQueue.ID = Control
  GET (SELF.ControlQueue, SELF.ControlQueue.ID)

  IF ERRORCODE() = 0
    DelPrnt = SELF.ControlQueue.ParentID
    DELETE (SELF.ControlQueue)

    IF ERRORCODE() = 0
      !Fix up controls that had the deleted control as a parent
      LOOP i = 1 TO RECORDS (SELF.ControlQueue)
        GET (SELF.ControlQueue, i)                 
?       ASSERT(~ERRORCODE(),ERROR())

        IF SELF.ControlQueue.ParentID = Control
          SELF.ControlQueue.ParentID = DelPrnt
          PUT(SELF.ControlQueue)
?         ASSERT(~ERRORCODE(),ERROR())
        END
      END
    END
  END
  RETURN

WindowResizeClass.SetParentControl PROCEDURE (SIGNED ControlID, SIGNED ParentID)
  CODE
  IF ControlID = ParentID
    RETURN
  END

  SELF.ControlQueue.ID = ControlID
  GET (SELF.ControlQueue, SELF.ControlQueue.ID)

  IF ERRORCODE() = 0
    SELF.ControlQueue.ParentID = ParentID
    PUT (SELF.ControLQueue)
?   ASSERT(~ERRORCODE(),ERROR())
  END

  IF ParentID <> 0
    SELF.ControlQueue.ID = ParentID
    GET (SELF.ControlQueue, SELF.ControlQueue.ID)
?   ASSERT(~ERRORCODE(),ERROR())

    IF NOT SELF.ControlQueue.HasChildren
      SELF.ControlQueue.HasChildren = TRUE
      PUT (SELF.ControlQueue)
?     ASSERT(~ERRORCODE(),ERROR())
    END
  END
  RETURN

WindowResizeClass.SetParentDefaults PROCEDURE()
iPnt        UNSIGNED,AUTO
  CODE
? ASSERT(NOT SELF.ResizeCalled,'SetParentDefaults should only be called before any resizes take place')                 !should only be called before any resizes take place
  LOOP iPnt = 1 TO RECORDS(SELF.ControlQueue)
    GET (SELF.ControlQueue, iPnt)
?   ASSERT(~ERRORCODE(),ERROR())

    SELF.SetParentDefault()
  END
  RETURN

WindowResizeClass.SetParentDefault PROCEDURE()
pPnt        UNSIGNED,AUTO
ParentID    SIGNED,AUTO
iSize       LIKE(PositionGroup),AUTO
ThisControl LIKE(ControlQueue.ID),AUTO
ThisControlParent LIKE(ControlQueue.ParentID),AUTO
  CODE
? ASSERT(SELF.ControlQueue.Type <> CREATE:Tab,'Tabs controls can not have its parent changed')

  ThisControl       = SELF.ControlQueue.ID
  ThisControlParent = SELF.ControlQueue.ParentID
  ParentID          = ThisControl{PROP:Parent}

  IF ParentID <> 0
    LOOP WHILE (ParentID{PROP:Type} + 0) = CREATE:Tab
      ThisControl = ParentID
      ParentID    = ThisControl{PROP:Parent}
    END

    SELF.SetParentControl (ThisControl, ParentID)
  ELSE
    CASE ThisControl{PROP:Type} + 0
    OF CREATE:Group
    OROF CREATE:Option
    OROF CREATE:Tab
    OROF CREATE:Sheet
      iSize = SELF.ControlQueue.Pos

      LOOP pPnt = 1 TO RECORDS(SELF.ControlQueue)
        GET (SELF.ControlQueue, pPnt)
?       ASSERT(~ERRORCODE())

        IF SELF.ControlQueue.ID = ThisControl
          CYCLE
        END
        ! Treat ThisControl as a parent for controls located within it
        IF INRANGE (SELF.ControlQueue.Pos.XPos, iSize.XPos, iSize.XPos + iSize.Width - 1) AND |
           INRANGE (SELF.ControlQueue.Pos.YPos, iSize.YPos, iSize.YPos + iSize.Height - 1)
          IF ThisControlParent = 0 OR ThisControlParent <> SELF.ControlQueue.ID
            SELF.SetParentControl (SELF.ControlQueue.ID, ThisControl)
          END
        END
      END
    END
  END
  RETURN

WindowResizeClass.GetParentControl PROCEDURE (SIGNED ControlID)
  CODE
  SELF.ControlQueue.ID = ControlID
  GET (SELF.ControlQueue, SELF.ControlQueue.ID)
? ASSERT(~ERRORCODE())
  RETURN SELF.ControlQueue.ParentID


WindowResizeClass.SetPosition PROCEDURE (SIGNED ControlID, |
                                         *PositionGroup ParentOrigPos, |
                                         *PositionGroup ParentCurrentPos, |
                                         *PositionGroup OrigPos, |
                                         *PositionGroup NewPos)
ConstBottom    BYTE,AUTO
ConstRight     BYTE,AUTO
ConstBottomCnt BYTE,AUTO
ConstRightCnt  BYTE,AUTO
HeightLocked   BYTE,AUTO
WidthLocked    BYTE,AUTO
OrigLogicalX   SIGNED,AUTO
OrigLogicalY   SIGNED,AUTO
Delta          SIGNED,AUTO
xPositional    USHORT,AUTO
yPositional    USHORT,AUTO
xScale         DECIMAL(6,4),AUTO
yScale         DECIMAL(6,4),AUTO

  CODE
? ASSERT (SELF.ControlQueue.ID = ControlID)

  xScale         = ParentCurrentPos.Width  / ParentOrigPos.Width
  yScale         = ParentCurrentPos.Height / ParentOrigPos.Height
  WidthLocked    = CHOOSE (BAND(SELF.ControlQueue.ResizeStrategy, Resize:LockWidth) <> 0)
  HeightLocked   = CHOOSE (BAND(SELF.ControlQueue.ResizeStrategy, Resize:LockHeight) <> 0)
  ConstRight     = CHOOSE (BAND(SELF.ControlQueue.ResizeStrategy, Resize:ConstantRight) <> 0)
  ConstBottom    = CHOOSE (BAND(SELF.ControlQueue.ResizeStrategy, Resize:ConstantBottom) <> 0)
  ConstRightCnt  = CHOOSE (BAND(SELF.ControlQueue.ResizeStrategy, Resize:ConstantRightCenter) <> 0)
  ConstBottomCnt = CHOOSE (BAND(SELF.ControlQueue.ResizeStrategy, Resize:ConstantBottomCenter) <> 0)
  xPositional    = BAND (SELF.ControlQueue.PositionalStrategy, 000FFh)
  yPositional    = BAND (SELF.ControlQueue.PositionalStrategy, 0FF00h)

  OrigLogicalX   = OrigPos.XPos - ParentOrigPos.XPos
  OrigLogicalY   = OrigPos.YPos - ParentOrigPos.YPos

  IF NOT ConstRight AND NOT ConstRightCnt
    NewPos.Width = OrigPos.Width * CHOOSE (NOT WidthLocked, xScale, 1)
  END
  IF NOT ConstBottom AND NOT ConstBottomCnt
    NewPos.Height = OrigPos.Height * CHOOSE (NOT HeightLocked, yScale, 1)
  END

  IF xPositional = Resize:FixNearestX
    xPositional = CHOOSE (OrigLogicalX < ParentOrigPos.Width - OrigLogicalX - OrigPos.Width, Resize:FixLeft, Resize:FixRight)
  END
  IF yPositional = Resize:FixNearestY
    yPositional = CHOOSE (OrigLogicalY < ParentOrigPos.Height - OrigLogicalY - OrigPos.Height, Resize:FixTop, Resize:FixBottom)
  END

  CASE xPositional
  OF Resize:LockXPos
    NewPos.XPos = OrigPos.XPos
  OF Resize:FixRight
    Delta = ParentOrigPos.Width - OrigLogicalX
    IF NOT WidthLocked
      Delta = Delta * xScale
    END
    NewPos.XPos = ParentCurrentPos.XPos + ParentCurrentPos.Width - Delta
  OF Resize:FixLeft
    NewPos.XPos = ParentCurrentPos.XPos + OrigLogicalX
  OF Resize:FixXCenter
    !Delta=OrigLogicalX-(ParentOrigPos.Width/2)
    NewPos.XPos = ParentCurrentPos.XPos + ParentCurrentPos.Width/2 + (OrigLogicalX - ParentOrigPos.Width/2) * CHOOSE (NOT WidthLocked, xScale, 1)
  OF Resize:FixXToCenter
    !Delta=ParentOrigPos.Width/2-OrigPos.XPos
    NewPos.XPos = (ParentCurrentPos.XPos - ParentOrigPos.XPos) + ParentCurrentPos.Width/2 - (ParentOrigPos.Width/2 - OrigPos.XPos)
  ELSE
    NewPos.XPos = ParentCurrentPos.XPos + OrigLogicalX * xScale
  END

  CASE yPositional
  OF Resize:LockYPos
    NewPos.YPos = OrigPos.YPos
  OF Resize:FixBottom
    Delta = ParentOrigPos.Height - OrigLogicalY
    IF NOT HeightLocked
      Delta = Delta * yScale
    END
    NewPos.YPos = ParentCurrentPos.YPos + ParentCurrentPos.Height - Delta
  OF Resize:FixTop
    NewPos.YPos = ParentCurrentPos.YPos + OrigLogicalY
  OF Resize:FixYCenter
    !Delta=OrigLogicalY-(ParentOrigPos.Height/2)
    NewPos.YPos=ParentCurrentPos.YPos + ParentCurrentPos.Height/2 + (OrigLogicalY - ParentOrigPos.Height/2) * CHOOSE (NOT yScale, yScale, 1)
  OF Resize:FixYToCenter
    !Delta=ParentOrigPos.Height/2-OrigPos.YPos
    NewPos.YPos = (ParentCurrentPos.YPos - ParentOrigPos.YPos) + ParentCurrentPos.Height/2 - (ParentOrigPos.Height/2 - OrigPos.YPos)
  ELSE
    NewPos.YPos = ParentCurrentPos.YPos + OrigLogicalY * yScale
  END

  Delta = ParentCurrentPos.XPos + ParentCurrentPos.Width - (ParentOrigPos.Width - OrigLogicalX) - NewPos.XPos
  IF ConstRight
    NewPos.Width = OrigPos.Width + Delta
  ELSIF ConstRightCnt
    NewPos.Width = OrigPos.Width + Delta/2
  END

  Delta = ParentCurrentPos.YPos + ParentCurrentPos.Height - (ParentOrigPos.Height - OrigLogicalY) - NewPos.YPos
  IF ConstBottom
    NewPos.Height = OrigPos.Height + Delta
  ELSIF ConstBottomCnt
    NewPos.Height = OrigPos.Height + Delta/2
  END
  RETURN

!This method gets the original position of the control. This is the position
!of the control when the last Reset was called.
!Returns False if the CtrlId is not recognised

WindowResizeClass.GetOrigPos PROCEDURE (SIGNED CtrlId, *PositionGroup PG)
  CODE
  SELF.ControlQueue.ID = CtrlId
  GET (SELF.ControlQueue, SELF.ControlQueue.Id)
  IF ERRORCODE() <> 0 
    RETURN FALSE
  END

  PG = SELF.ControlQueue.Pos
  RETURN TRUE


!This method gets the current position of a control.
!Returns False if the CtrlId is not recognised

WindowResizeClass.GetCurrentPos PROCEDURE (SIGNED CtrlId, *PositionGroup PG)
  CODE
  SELF.ControlQueue.ID = CtrlId
  GET (SELF.ControlQueue, SELF.ControlQueue.Id)
  IF ERRORCODE() <> 0
    RETURN FALSE
  END

  SELF.GetSizeInfo (CtrlId, PG)
  RETURN TRUE


WindowResizeClass.ResizeChildren PROCEDURE (SIGNED ParentID, |
                                            *PositionGroup ParentOrigPos, |
                                            *PositionGroup ParentCurrentPos)

FieldCounter   UNSIGNED,AUTO
ResizeList     &ResizeQueue,AUTO
POrigPos       LIKE(PositionGroup),AUTO
PNewPos        LIKE(PositionGroup),AUTO

  CODE
  ResizeList &= SELF.ResizeList

  LOOP FieldCounter=1 TO RECORDS (SELF.ControlQueue)
    GET (SELF.ControlQueue, FieldCounter)
?   ASSERT(~ERRORCODE())

    IF SELF.ControlQueue.ParentID = ParentID
      CLEAR (ResizeList)
      ResizeList.ControlID = SELF.ControlQueue.ID
      ResizeList.Type      = SELF.ControlQueue.Type

      SELF.GetSizeInfo (SELF.ControlQueue.ID, ResizeList.Before)
      SELF.SetPosition (ResizeList.ControlID, |
                        ParentOrigPos, |
                        ParentCurrentPos, |
                        SELF.ControlQueue.Pos, |
                        ResizeList.After)

      POrigPos = SELF.ControlQueue.Pos
      PNewPos  = ResizeList.After
      ADD (ResizeList)
?     ASSERT(~ERRORCODE())

      IF SELF.ControlQueue.HasChildren
        IF ResizeList.Before <> ResizeList.After OR SELF.ControlQueue.ID{PROP:Full}
          SELF.ResizeChildren (SELF.ControlQueue.ID, POrigPos, PNewPos)
        END
      END
    END
  END
  RETURN

WindowResizeClass.RestoreControls PROCEDURE()
i    UNSIGNED,AUTO
  CODE
  LOOP i = 1 TO RECORDS (SELF.ControlQueue)
    GET (SELF.ControlQueue, i)
?   ASSERT(~ERRORCODE())

    SELF.SetSize (SELF.ControlQueue.ID, SELF.ControlQueue.Pos)
  END
  RETURN

WindowResizeClass.GetSizeInfo PROCEDURE (SIGNED ControlID, *PositionGroup s)
  CODE
  GETPOSITION (ControlID, s.XPos, s.YPos, s.Width, s.Height)

  IF ControlId = 0
    s.Width  = 0{PROP:ClientWidth}
    s.Height = 0{PROP:ClientHeight}
  END
  RETURN

WindowResizeClass.SetSize PROCEDURE (SIGNED ControlID, *PositionGroup s)
Pos   LIKE(PositionGroup),AUTO
W     UNSIGNED,AUTO
H     UNSIGNED,AUTO
  CODE
  SELF.GetSizeInfo (ControlID, Pos)

  IF Pos <> s
    W = CHOOSE (NOT ControlID{PROP:NoWidth},  s.Width,  _NoPos)
    H = CHOOSE (NOT ControlID{PROP:NoHeight}, s.Height, _NoPos)

    SETPOSITION (ControlID, s.XPos, s.Ypos, W, H)
  END
  RETURN

WindowResizeClass.SetPriorities PROCEDURE (*PositionGroup CurrentSize)
  CODE
  IF CurrentSize.Width <> SELF.PreviousWin.Width AND CurrentSize.Height <> SELF.PreviousWin.Height
    DO FullResize
  ELSIF CurrentSize.Width = SELF.PreviousWin.Width
    DO VerticalResize
  ELSE
    DO HorizontalResize
  END
  RETURN

VerticalResize ROUTINE
  DATA
i   UNSIGNED,AUTO
  CODE
  LOOP i = 1 TO RECORDS (SELF.ResizeList)
    GET (SELF.ResizeList, i)
?   ASSERT(~ERRORCODE())
 
    SELF.ResizeList.Priority = SELF.ResizeList.After.YPos
    PUT (SELF.ResizeList)
?   ASSERT(~ERRORCODE())
  END
  EXIT

HorizontalResize ROUTINE
  DATA
i   UNSIGNED,AUTO
  CODE
  LOOP i = 1 TO RECORDS (SELF.ResizeList)
    GET (SELF.ResizeList, i)
?   ASSERT(~ERRORCODE())

    SELF.ResizeList.Priority = SELF.ResizeList.After.XPos
    PUT (SELF.ResizeList)
?   ASSERT(~ERRORCODE())
  END
  EXIT

FullResize ROUTINE
  DATA
i   UNSIGNED,AUTO
  CODE
  LOOP i = 1 TO RECORDS (SELF.ResizeList)
    GET (SELF.ResizeList, i)
?   ASSERT(~ERRORCODE())

    SELF.ResizeList.Priority = (SELF.ResizeList.After.XPos^2) + (SELF.ResizeList.After.YPos^2)
    PUT (SELF.ResizeList)
?   ASSERT(~ERRORCODE())
  END
  EXIT
