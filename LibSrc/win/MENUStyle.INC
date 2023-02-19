!Declarations of MenuStyleManager Class and Type
OMIT('_EndOfInclude_',_MenuStyleManager_)
_MenuStyleManager_ EQUATE(1)

  INCLUDE('CWINT.INT'),ONCE

MenuThemeType           GROUP,TYPE
TextNormalColor                 LONG
TextSelectedColor               LONG
TextOverColor                   LONG
MenuBarNormalColorLeft          LONG
MenuBarNormalColorRight         LONG
MenuBarOverColorLeft            LONG
MenuBarOverColorRight           LONG
MenuBarSelectedColorLeft        LONG
MenuBarSelectedColorRight       LONG
ImageBkgndColorLeft             LONG
ImageBkgndColorRight            LONG
ItemNormalBkgndColor            LONG
SelectionBarColorLeft           LONG
SelectionBarColorRight          LONG
SelectionBorderColor            LONG
SelectionBarVerticalGradient    BYTE
Flat                            BYTE
ShowImageBar                    BYTE
Embossed                        BYTE
LineMode                        BYTE
                        END
    
MenuStyleManager CLASS,TYPE,MODULE('MENUStyle.CLW'),LINK('MENUStyle.CLW',1),DLL(0)
MenuFeq                LONG,PROTECTED
MenuInterface          &IMenuInterface,PROTECTED
MenuStyle              &IMenuStyle,PROTECTED
Theme                  GROUP(MenuThemeType).
delayRefresh           BYTE,PROTECTED
needRefresh            BYTE,PROTECTED
windowsThemeIgnored    BYTE,PROTECTED
ForceTheme             BYTE,PROTECTED
InitMenuInterface      PROCEDURE(),BYTE,PROC,PROTECTED
Init                   PROCEDURE(SIGNED MenuFEQ)
UseCustomTheme         PROCEDURE(BYTE useValue)
UseCustomTheme         PROCEDURE(),BYTE
MenuHasGradient        PROCEDURE(BYTE MenuPart),BYTE
SetColor               PROCEDURE(BYTE MenuPart, LONG StartColor, LONG EndColor,<BYTE vertical>)
GetStartColor          PROCEDURE(BYTE MenuPart),LONG
GetEndColor            PROCEDURE(BYTE MenuPart),LONG
SetVerticalGradient    PROCEDURE(BYTE MenuPart, BYTE vertical=1)
GetIsVerticalGradient  PROCEDURE(BYTE MenuPart),BYTE

SuspendRefresh         PROCEDURE()
ApplyTheme             PROCEDURE()
SetDefaultThemeValues  PROCEDURE()
ClearThemeValues       PROCEDURE()
Refresh                PROCEDURE(BYTE force=0)

IgnoreWindowsTheme     PROCEDURE(BYTE ignore=FALSE)
IsWindowsThemeIgnored  PROCEDURE(),BYTE

SetImage               PROCEDURE(BYTE MenuPart, STRING ImageName)
GetImage               PROCEDURE(BYTE MenuPart),STRING

SetTextColor           PROCEDURE(BYTE MenuTextType, LONG TextColor)
GetTextColor           PROCEDURE(BYTE MenuTextType),LONG

SetFont                PROCEDURE(STRING FontName,LONG FontSize,LONG FontStyle,BYTE FontCharSet,LONG FontAngle,LONG FontColor)
GetFont                PROCEDURE(*STRING FontName,*LONG FontSize,*LONG FontStyle,*BYTE FontCharSet,*LONG FontAngle,*LONG FontColor)
SetFlatMode            PROCEDURE(BYTE modeOn=1)
GetFlatMode            PROCEDURE(),BYTE

SetImageBar            PROCEDURE(LONG modeOn=1)
GetImageBar            PROCEDURE(),LONG

SetVerticalSelection   PROCEDURE(LONG modeVert=1)
GetVerticalSelection   PROCEDURE(),LONG

AlignShortcutsToLeft   PROCEDURE(BYTE value=1)
AreShortcutsLeftAligned PROCEDURE(),BYTE

SetMenuMetric          PROCEDURE(UNSIGNED pMenuMetric,UNSIGNED pValue)
GetMenuMetric          PROCEDURE(UNSIGNED pMenuMetric),UNSIGNED

SetLinesEmbossed       PROCEDURE(BYTE value)             !Value: TRUE to show lines embossed
GetLinesEmbossed       PROCEDURE(),BYTE       

SetLineMode            PROCEDURE(BYTE value)             !Value: line mode
GetLineMode            PROCEDURE(),BYTE

SetWhiteBackground     PROCEDURE()
SetBackgroundColor     PROCEDURE(LONG pColorValue)

SetThemeColors         PROCEDURE()
SetThemeColors         PROCEDURE(STRING pTheme),VIRTUAL
SetCustomThemeColors   PROCEDURE(STRING pTheme),VIRTUAL,PROTECTED
MixColors              PROCEDURE(LONG pColor1,LONG pColor2,BYTE pPercentageMix),LONG,PROTECTED
                  END

_EndOfInclude_



