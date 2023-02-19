!Declarations of EnhancedFocus Class and Type
OMIT('_EndOfInclude_',_EnhancedFocus_)
_EnhancedFocus_ EQUATE(1)

ControlTypeQueue   QUEUE,TYPE
CT                   USHORT
                   END

EnhancedFocusClassType CLASS,TYPE,MODULE('EFOCUS.CLW'),LINK('EFOCUS.CLW',1),DLL(0)
ChangeColor               BYTE
BackgroundColor           LONG
UseBoldText               BYTE
ExcludeDropList           BYTE
BackgroundColorRequired   LONG
!
DisplayBox                BYTE
BoxFillColor              LONG
BoxBorderColor            LONG
BoxSize                   SHORT
BoxFillColorRequired      LONG
BoxBorderColorRequired    LONG
!
DisplayIndicator          BYTE
IndicatorColor            LONG
IndicatorCharacter        CSTRING(3)
IndicatorOffset           SHORT
!
enableOSK                 BYTE(1)
_popupTouchKeyboardFolder CSTRING(FILE:MaxFilePath),PROTECTED
!
scField                   UNSIGNED,PROTECTED
scColor                   LONG,PROTECTED
scStyle                   LONG,PROTECTED
scPColor                  LONG,PROTECTED
scX                       SHORT,PROTECTED
scY                       SHORT,PROTECTED
scW                       SHORT,PROTECTED
scH                       SHORT,PROTECTED
!
ExcludedControlType       &ControlTypeQueue,PROTECTED
!
Init                      PROCEDURE()
Init                      PROCEDURE(BYTE pChangeColor,LONG pBackgroundColor,BYTE pUseBoldText,BYTE pExcludeDropList,LONG pBackgroundColorRequired,BYTE pDisplayBox,LONG pBoxFillColor,LONG pBoxBorderColor,SHORT pBoxSize,LONG pBoxFillColorRequired,LONG pBoxBorderColorRequired,BYTE pDisplayIndicator,LONG pIndicatorColor,STRING pIndicatorCharacter,SHORT pIndicatorOffset),VIRTUAL
DisableControlType        PROCEDURE(USHORT pControlType)
EnableControlType         PROCEDURE(USHORT pControlType)
PopupTouchKeyboard        PROCEDURE()
TakeEvent                 PROCEDURE(),VIRTUAL
EnhanceFocusUnselect      PROCEDURE(),VIRTUAL
EnhanceFocusSelect        PROCEDURE(UNSIGNED pControl),VIRTUAL
ControlAllowed            PROCEDURE(UNSIGNED pControl),BYTE,VIRTUAL
SetCurrentControl         PROCEDURE(UNSIGNED pControl),BYTE
Destruct                  PROCEDURE()
Construct                 PROCEDURE()
SetOnScreenKeyboard       PROCEDURE(<BYTE onOff>)
GetOnScreenKeyboard       PROCEDURE(),BYTE
ControlUseOnScreenKeyboard PROCEDURE(UNSIGNED pControl),BYTE,VIRTUAL
HasTouchInput             PROCEDURE(),BYTE
DrawColorAndFontsIndicator PROCEDURE(BYTE doDraw),VIRTUAL
DrawBoxAndCaretIndicator  PROCEDURE(BYTE doDraw),VIRTUAL
IsCurrentControlVisible   PROCEDURE(),BYTE
                        END

EnterByTabClass         CLASS,TYPE,MODULE('EFOCUS.CLW'),LINK('EFOCUS.CLW',1),DLL(0)
ExcludedControl           &ControlTypeQueue,PROTECTED
NextTabStopControlQ       &NextTabStopControlQueue,PROTECTED
Inited                    BYTE,PROTECTED
Init                      PROCEDURE(BYTE pOmitListbox=1),VIRTUAL
TakeEvent                 PROCEDURE(),BYTE,VIRTUAL
ExcludeControl            PROCEDURE(UNSIGNED pControl)
PressKey                  PROCEDURE(UNSIGNED K)
SetNextTabStop            PROCEDURE(UNSIGNED pControl,UNSIGNED pNextControl)
Destruct                  PROCEDURE()
Construct                 PROCEDURE()
                        END
!--------------------------------------------------

_EndOfInclude_


