! Declarations of Resize Classes and Types

!ABCIncludeFile

    OMIT('_EndOfInclude_',_ResizePresent_)
_ResizePresent_               EQUATE(1)

 INCLUDE('ABRESIZE.EQU'),ONCE

!--------------------------------------------------

WindowResizeClass             CLASS,TYPE,MODULE('ABRESIZE.CLW'),LINK('ABRESIZE.CLW',_ABCLinkMode_),DLL(_ABCDllMode_)
AppStrategy                     BYTE,PRIVATE
AutoTransparent                 BYTE
OrigWin                         LIKE(WindowPositionGroup),PRIVATE
ControlQueue                    &ControlQueue,PRIVATE
DeferMoves                      BYTE
PreviousWin                     LIKE(PositionGroup),PRIVATE
ResizeCalled                    BYTE(False),PRIVATE
ResizeList                      &ResizeQueue,PRIVATE

GetParentControl                PROCEDURE(SIGNED Control),SIGNED,VIRTUAL
GetPositionStrategy             PROCEDURE(SHORT ControlType,<BYTE AppStrategy>),USHORT
GetResizeStrategy               PROCEDURE(SHORT ControlType,<BYTE AppStrategy>),USHORT
GetOrigPos                      PROCEDURE(SIGNED Control,*PositionGroup PG),BYTE,PROC
GetCurrentPos                   PROCEDURE(SIGNED Control,*PositionGroup PG),BYTE,PROC
Init                            PROCEDURE(BYTE AppStrategy=AppStrategy:Resize,BYTE SetWindowMinSize=False,BYTE SetWindowMaxSize=False)   !,EXTENDS
Kill                            PROCEDURE()
RemoveControl                   PROCEDURE(SIGNED Control)
Reset                           PROCEDURE(),VIRTUAL
Resize                          PROCEDURE(),BYTE,VIRTUAL,PROC
Resize                          PROCEDURE(SIGNED Control),BYTE,VIRTUAL,PROC
ResizeChildren                  PROCEDURE(SIGNED ParentId,*PositionGroup ParentOrigPos,*PositionGroup ParentCurrentPos),PROTECTED
WindowResizeBegin               PROCEDURE(),VIRTUAL
WindowResizeEnd                 PROCEDURE(),VIRTUAL
RestoreWindow                   PROCEDURE(),VIRTUAL
SetAnchor                       PROCEDURE(SIGNED Control,USHORT AnchorStrategy)
SetAnchor                       PROCEDURE(SIGNED Control,USHORT AnchorStrategy,SIGNED ParentFeq)
SetParentControl                PROCEDURE(SIGNED Control,SIGNED ParentFeq=0)
SetParentDefaults               PROCEDURE(),VIRTUAL
SetParentDefault                PROCEDURE(),VIRTUAL,PROTECTED
SetPosition                     PROCEDURE(SIGNED Control,*PositionGroup ParentOrigPos,*PositionGroup ParentNewPos,*PositionGroup OrigPos,*PositionGroup NewPos),VIRTUAL
SetStrategy                     PROCEDURE(SIGNED Source,SIGNED Destination)
SetStrategy                     PROCEDURE(SIGNED Control=0,USHORT PositionStrategy,USHORT ResizeStrategy)
TakeResize                      PROCEDURE(),BYTE,PRIVATE
RestoreControls                 PROCEDURE(),PRIVATE                   !resize all controls to design size/position
GetSizeInfo                     PROCEDURE(SIGNED ControlID,*PositionGroup),PRIVATE
SetSize                         PROCEDURE(SIGNED ControlID,*PositionGroup),PRIVATE
SetPriorities                   PROCEDURE(*PositionGroup CurrentSize),PRIVATE
                              END

!--------------------------------------------------

_EndOfInclude_
