  MEMBER

  MAP
      MODULE('API')
         MSMGetSysColor(signed nIndex),long,pascal,NAME('GetSysColor')
      END
  END
  INCLUDE('MENUStyle.INC'),ONCE

MenuStyleManager.Init   PROCEDURE (SIGNED MenuFEQ)
  CODE
  SELF.SetImageBar(-1)
  SELF.SetVerticalSelection(-1)
  SELF.MenuFeq = MenuFEQ
  IF SELF.InitMenuInterface()
    SELF.ForceTheme = CHOOSE (NOT SELF.MenuStyle.OSThemeAvailable())
  END
  RETURN

MenuStyleManager.InitMenuInterface PROCEDURE()
  CODE
  IF SELF.MenuFeq = 0
    RETURN FALSE
  END

  IF SELF.MenuInterface &= NULL
     SELF.MenuInterface &= ((SELF.MenuFeq){PROP:Interface})
     SELF.MenuStyle &= SELF.MenuInterface.GetStyle()
     SELF.MenuStyle.UseCustomTheme(TRUE)
  END
  RETURN TRUE

MenuStyleManager.UseCustomTheme  PROCEDURE(BYTE setValue)
  CODE
  IF SELF.InitMenuInterface()
     IF setValue <> SELF.MenuStyle.UseCustomTheme()
        IF setValue           
           SELF.MenuStyle.UseCustomTheme(TRUE)
           SELF.IgnoreWindowsTheme(SELF.windowsThemeIgnored)          
        ELSE        
           SELF.windowsThemeIgnored = SELF.IsWindowsThemeIgnored()
           !SELF.IgnoreWindowsTheme(FALSE)
           SELF.MenuStyle.UseCustomTheme(FALSE)
        END
        SELF.Refresh()
     END
  END
  RETURN

MenuStyleManager.UseCustomTheme PROCEDURE()
  CODE
  IF SELF.InitMenuInterface()
     RETURN SELF.MenuStyle.UseCustomTheme()
  END
  RETURN FALSE
    
MenuStyleManager.MenuHasGradient  PROCEDURE(BYTE MenuPart)
lBrush   &IBrush,AUTO
  CODE
  IF SELF.InitMenuInterface()
    lBrush &= SELF.MenuStyle.MenuBrush(MenuPart)

    IF NOT lBrush &= NULL
      IF lBrush.BrushType() = BrushTypes:BrushGradient
        RETURN TRUE
      END
    END
  END
  RETURN FALSE

MenuStyleManager.SuspendRefresh  PROCEDURE()
  CODE
  SELF.delayRefresh = TRUE
  RETURN
    
MenuStyleManager.Refresh  PROCEDURE(BYTE force=0)
  CODE
  IF force OR NOT SELF.delayRefresh
    IF SELF.delayRefresh
      IF SELF.needRefresh             
        IF SELF.InitMenuInterface()
          SELF.MenuStyle.SetDirty()
        END
      END
    ELSIF SELF.InitMenuInterface()
      SELF.MenuStyle.SetDirty()
    END
    SELF.needRefresh = FALSE
  ELSE
    IF SELF.delayRefresh
      SELF.needRefresh = TRUE
    ELSIF SELF.InitMenuInterface()
      SELF.MenuStyle.SetDirty()
    END       
  END
  RETURN

MenuStyleManager.SetColor  PROCEDURE(BYTE MenuPart, LONG StartColor, LONG EndColor,<BYTE vertical>)
lBrush    &IBrush,AUTO
gradientType BYTE,AUTO!GradientTypes
  CODE
  IF SELF.InitMenuInterface() AND SELF.MenuStyle.UseCustomTheme()
    lBrush &= SELF.MenuStyle.MenuBrush(MenuPart)
    IF NOT lBrush &= NULL
      IF OMITTED(vertical)
        vertical = SELF.GetIsVerticalGradient(MenuPart)
      END
      IF vertical
         gradientType = GradientTypes:Vertical
      ELSE
         gradientType = GradientTypes:Horizontal
      END    
      lBrush.Gradient (StartColor, EndColor, gradientType)
      SELF.Refresh()
    END
  END
  RETURN

MenuStyleManager.GetStartColor   PROCEDURE(BYTE MenuPart)
lBrush   &IBrush,AUTO
  CODE
  IF SELF.InitMenuInterface() AND SELF.MenuStyle.UseCustomTheme()
    lBrush &= SELF.MenuStyle.MenuBrush (MenuPart)
    IF NOT lBrush &= NULL
      IF lBrush.BrushType() = BrushTypes:BrushGradient
        RETURN lBrush.GradientFrom()
      END
    END
  END
  RETURN COLOR:None

MenuStyleManager.GetEndColor   PROCEDURE(BYTE MenuPart)
lBrush   &IBrush,AUTO
  CODE
  IF SELF.InitMenuInterface() AND SELF.MenuStyle.UseCustomTheme()
    lBrush &= SELF.MenuStyle.MenuBrush (MenuPart)
    IF NOT lBrush &= NULL
      IF lBrush.BrushType() = BrushTypes:BrushGradient
        RETURN lBrush.GradientTo()
      END
    END
  END
  RETURN COLOR:None

MenuStyleManager.SetVerticalGradient   PROCEDURE(BYTE MenuPart,BYTE value=1)
lBrush       &IBrush,AUTO
gradientType BYTE,AUTO!GradientTypes
  CODE
  IF SELF.InitMenuInterface() AND SELF.MenuStyle.UseCustomTheme()
    lBrush &= SELF.MenuStyle.MenuBrush (MenuPart)
    IF NOT lBrush &= NULL
      IF lBrush.BrushType() = BrushTypes:BrushGradient
        IF value
           gradientType = GradientTypes:Vertical
        ELSE
           gradientType = GradientTypes:Horizontal
        END
        lBrush.Gradient (SELF.GetStartColor(MenuPart), SELF.GetEndColor(MenuPart), gradientType)
      END
    END
  END
  RETURN

MenuStyleManager.GetIsVerticalGradient   PROCEDURE(BYTE MenuPart)
lBrush   &IBrush,AUTO
  CODE
  IF SELF.InitMenuInterface() AND SELF.MenuStyle.UseCustomTheme()
    lBrush &= SELF.MenuStyle.MenuBrush (MenuPart)
    IF NOT lBrush &= NULL
      IF lBrush.BrushType() = BrushTypes:BrushGradient
        IF NOT (lBrush.GradientType() = GradientTypes:Horizontal OR lBrush.GradientType() = GradientTypes:HorizontalCylinder)
           RETURN TRUE
        END
      END
    END
  END
  RETURN FALSE

MenuStyleManager.SetImage  PROCEDURE(BYTE MenuPart,STRING ImageName)
lBrush       &IBrush,AUTO
lImageName   CSTRING(FILE:MaxFileName)
  CODE
  IF SELF.InitMenuInterface() AND SELF.MenuStyle.UseCustomTheme()
    lBrush &= SELF.MenuStyle.MenuBrush (MenuPart)
    IF NOT lBrush &= NULL
      lImageName = CLIP(ImageName)
      lBrush.ImageBrush (lImageName)
      SELF.Refresh()
    END
  END
  RETURN

MenuStyleManager.GetImage  PROCEDURE(BYTE MenuPart)
lBrush   &IBrush,AUTO
  CODE
  IF SELF.InitMenuInterface() AND SELF.MenuStyle.UseCustomTheme()
    lBrush &= SELF.MenuStyle.MenuBrush (MenuPart)
    IF NOT lBrush &= NULL
      RETURN lBrush.ImageName()
    END
  END
  RETURN ''

MenuStyleManager.SetTextColor  PROCEDURE(BYTE MenuTextType,LONG TextColor)
  CODE
  IF SELF.InitMenuInterface() AND SELF.MenuStyle.UseCustomTheme()
    SELF.MenuStyle.MenuColor (MenuTextType, TextColor)
    SELF.Refresh()
  END
  RETURN

MenuStyleManager.GetTextColor  PROCEDURE(BYTE MenuTextType)
  CODE
  IF SELF.InitMenuInterface() AND SELF.MenuStyle.UseCustomTheme()
    RETURN SELF.MenuStyle.MenuColor (MenuTextType)
  END
  RETURN COLOR:None

MenuStyleManager.SetFont  PROCEDURE(STRING FontName,LONG FontSize,LONG FontStyle,BYTE FontCharSet,LONG FontAngle,LONG FontColor)
lMenuFont   &IFontProperties,AUTO
lFontName   CSTRING(32)
  CODE
  IF SELF.InitMenuInterface() AND SELF.MenuStyle.UseCustomTheme()
    lMenuFont &= SELF.MenuStyle.MenuFont()
    IF NOT lMenuFont &= NULL
      lFontName = CLIP(FontName)
      lMenuFont.FontName (lFontName)
      lMenuFont.FontSize (FontSize)
      lMenuFont.FontStyle (FontStyle)
      lMenuFont.FontCharSet (FontCharSet)
      lMenuFont.FontAngle (FontAngle)
      lMenuFont.FontColor (FontColor)
      SELF.Refresh()
    END
  END
  RETURN

MenuStyleManager.GetFont  PROCEDURE(*STRING FontName,*LONG FontSize,*LONG FontStyle,*BYTE FontCharSet,*LONG FontAngle,*LONG FontColor)
lMenuFont   &IFontProperties,AUTO
  CODE
  IF SELF.InitMenuInterface() AND SELF.MenuStyle.UseCustomTheme()
    lMenuFont &= SELF.MenuStyle.MenuFont()
    IF NOT lMenuFont &= NULL
      FontName    = lMenuFont.FontName()
      FontSize    = lMenuFont.FontSize()
      FontStyle   = lMenuFont.FontStyle()
      FontCharSet = lMenuFont.FontCharSet()
      FontAngle   = lMenuFont.FontAngle()
      FontColor   = lMenuFont.FontColor()
    END
  END
  RETURN

MenuStyleManager.AlignShortcutsToLeft  PROCEDURE(BYTE value=1)
  CODE
  IF SELF.InitMenuInterface() AND SELF.MenuStyle.UseCustomTheme()
    SELF.MenuStyle.AlignShortcuts (value)
    SELF.Refresh()
  END
  RETURN

MenuStyleManager.AreShortcutsLeftAligned  PROCEDURE()
  CODE
  IF SELF.InitMenuInterface()
    RETURN SELF.MenuStyle.AlignShortcuts()
  END
  RETURN FALSE
    
MenuStyleManager.SetFlatMode  PROCEDURE(BYTE modeOn=1)
  CODE
  IF SELF.InitMenuInterface() AND SELF.MenuStyle.UseCustomTheme()
       IF modeOn=True
          SELF.MenuStyle.FlatMode(FlatMenuMode:On)
       ELSE
          SELF.MenuStyle.FlatMode(FlatMenuMode:Off)
       END
    SELF.Refresh()
  END
  RETURN  

MenuStyleManager.GetFlatMode  PROCEDURE()
  CODE
  IF SELF.InitMenuInterface()
    IF SELF.MenuStyle.FlatMode() = FlatMenuMode:On
      RETURN TRUE
    END
  END
  RETURN FALSE

MenuStyleManager.SetLinesEmbossed  PROCEDURE (BYTE value)
mode   BYTE,AUTO 
  CODE
  IF SELF.InitMenuInterface() AND SELF.MenuStyle.UseCustomTheme()
    mode = SELF.MenuStyle.LineMode()
    IF value <> 0
      mode = BOR (mode, MenuLine:Embossed)
    ELSIF BAND (mode, MenuLine:Embossed) <> 0
      mode = BXOR (mode, MenuLine:Embossed)
    END
    SELF.Theme.Embossed = CHOOSE (value <> 0)
    SELF.MenuStyle.LineMode (mode)
    SELF.Refresh()
  END
  RETURN

MenuStyleManager.GetLinesEmbossed  PROCEDURE ()
mode   BYTE,AUTO 
  CODE
  IF SELF.InitMenuInterface()
    mode = SELF.MenuStyle.LineMode()
    IF BAND (mode, MenuLine:Embossed) <> 0
      RETURN TRUE
    END
  END
  RETURN FALSE

MenuStyleManager.SetLineMode  PROCEDURE (BYTE value)
mode   BYTE,AUTO
  CODE
  IF SELF.InitMenuInterface() AND SELF.MenuStyle.UseCustomTheme()
    SELF.Theme.LineMode = value

    mode = BAND (SELF.MenuStyle.LineMode(), MenuLine:Embossed)
    mode = BOR  (mode, value)
    SELF.MenuStyle.LineMode (mode)
    SELF.Refresh()
  END
  RETURN

MenuStyleManager.GetLineMode  PROCEDURE()
mode   BYTE,AUTO
  CODE
  IF SELF.InitMenuInterface()
    mode = SELF.MenuStyle.LineMode()
    IF BAND (mode, MenuLine:Embossed)
      mode = BXOR (mode, MenuLine:Embossed)
    END
  ELSE
    mode = MenuLine:Default
  END
  RETURN mode

MenuStyleManager.SetMenuMetric  PROCEDURE(UNSIGNED pMenuMetric,UNSIGNED pValue)
  CODE
  IF SELF.InitMenuInterface() AND SELF.MenuStyle.UseCustomTheme()
    SELF.MenuStyle.MetricValue (pMenuMetric, pValue)
    SELF.Refresh()
  END
  RETURN

MenuStyleManager.GetMenuMetric  PROCEDURE (UNSIGNED pMenuMetric)
  CODE
  IF SELF.InitMenuInterface()
    RETURN SELF.MenuStyle.MetricValue (pMenuMetric)    
  END
  RETURN 0

MenuStyleManager.SetImageBar  PROCEDURE(LONG modeOn=1)
  CODE
    IF modeOn > 0
       SELF.Theme.ShowImageBar = TRUE
    ELSE
       SELF.Theme.ShowImageBar = FALSE
    END
  RETURN

MenuStyleManager.GetImageBar  PROCEDURE()
  CODE
  RETURN SELF.Theme.ShowImageBar
        
MenuStyleManager.SetVerticalSelection  PROCEDURE(LONG modeVert=1)
  CODE
    IF modeVert > 0
       SELF.Theme.SelectionBarVerticalGradient = TRUE
    ELSE
       SELF.Theme.SelectionBarVerticalGradient = FALSE
    END
  RETURN

MenuStyleManager.GetVerticalSelection  PROCEDURE()
  CODE
  RETURN SELF.Theme.SelectionBarVerticalGradient

MenuStyleManager.SetThemeColors  PROCEDURE()
themeColor   LONG
  CODE
  IF SELF.InitMenuInterface()
    themeColor = COLOR:ACTIVECAPTION
    IF NOT SYSTEM{PROP:ThemeActive}
      SELF.SetThemeColors('WINDOWSCLASSIC')
    ELSE
      SELF.Theme.Embossed = TRUE
      SELF.Theme.Flat = FALSE
      IF SELF.GetImageBar()
        SELF.Theme.ImageBkgndColorLeft  = SELF.MixColors(SELF.MixColors(COLOR:WHITE,themeColor,15),COLOR:BLACK,5)
        SELF.Theme.ImageBkgndColorRight = SELF.MixColors(COLOR:WHITE,themeColor,70)
        SELF.Theme.ShowImageBar = TRUE
        SELF.Theme.LineMode = MenuLine:Default
      ELSE
        SELF.Theme.ImageBkgndColorLeft  = COLOR:MENU
        SELF.Theme.ImageBkgndColorRight = COLOR:MENU
        SELF.Theme.ShowImageBar = FALSE
        SELF.Theme.LineMode = MenuLine:ShowGutter
      END

      SELF.Theme.SelectionBarVerticalGradient = TRUE
          
      SELF.Theme.MenuBarNormalColorLeft    = SELF.MixColors(COLOR:WHITE,themeColor,10)
      SELF.Theme.MenuBarNormalColorRight   = SELF.MixColors(COLOR:WHITE,themeColor,50)

      SELF.Theme.MenuBarSelectedColorLeft  = SELF.MixColors(COLOR:WHITE,themeColor,10)
      SELF.Theme.MenuBarSelectedColorRight = SELF.MixColors(COLOR:BLACK,themeColor,80)
      SELF.Theme.MenuBarOverColorLeft      = SELF.MixColors(COLOR:WHITE,themeColor,20)
      SELF.Theme.MenuBarOverColorRight     = SELF.MixColors(COLOR:WHITE,themeColor,70)
      SELF.Theme.SelectionBorderColor      = SELF.MixColors(COLOR:WHITE,themeColor,50)
      SELF.Theme.SelectionBarColorLeft     = SELF.MixColors(COLOR:WHITE,themeColor,10)
      SELF.Theme.SelectionBarColorRight    = SELF.MixColors(COLOR:WHITE,themeColor,50)

      SELF.Theme.TextNormalColor           = COLOR:MENUTEXT
      SELF.Theme.TextSelectedColor         = COLOR:MENUTEXT
      SELF.Theme.TextOverColor             = COLOR:MENUTEXT
    END
  END
  RETURN

MenuStyleManager.SetThemeColors  PROCEDURE(STRING pTheme)
  CODE
  IF NOT SELF.InitMenuInterface()
    RETURN
  END
  IF UPPER(pTheme) = 'OFF'
     IF SELF.ForceTheme
        SELF.IgnoreWindowsTheme(FALSE)
        SELF.SetDefaultThemeValues()
        RETURN
     ELSE
        SELF.UseCustomTheme(FALSE)
        RETURN
     END
  END
  IF UPPER(pTheme) = 'DEFAULT'
     SELF.IgnoreWindowsTheme(FALSE)
     SELF.SetDefaultThemeValues()
     RETURN
  END
  SELF.UseCustomTheme(TRUE)
  SELF.SetDefaultThemeValues()
  SELF.IgnoreWindowsTheme(TRUE)
  CASE(UPPER(pTheme))
  OF 'AUTO'
    SELF.SetImageBar(TRUE)
    SELF.IgnoreWindowsTheme(FALSE)
    SELF.SetThemeColors()
  OF 'AUTONOBAR'
    SELF.SetImageBar(FALSE)
    SELF.IgnoreWindowsTheme(FALSE)
    SELF.SetThemeColors()
  OF 'AUTONOBARWHITE'
    SELF.SetImageBar(FALSE)
    SELF.IgnoreWindowsTheme(FALSE)
    SELF.SetThemeColors()
    SELF.Theme.ImageBkgndColorLeft         = COLOR:White
    SELF.Theme.ImageBkgndColorRight        = COLOR:White
    SELF.Theme.ItemNormalBkgndColor        = COLOR:White
  OF 'WINDOWSCLASSIC'
    SELF.IgnoreWindowsTheme(FALSE)
    SELF.Theme.MenuBarNormalColorLeft      = 0F0F0F0h
    SELF.Theme.MenuBarNormalColorRight     = 0F0F0F0h
    SELF.Theme.SelectionBarColorLeft       = COLOR:Navy
    SELF.Theme.SelectionBarColorRight      = COLOR:Navy
    SELF.Theme.SelectionBarVerticalGradient= TRUE
    SELF.Theme.SelectionBorderColor        = COLOR:None
    SELF.Theme.ImageBkgndColorLeft         = 0F0F0F0h
    SELF.Theme.ImageBkgndColorRight        = 0F0F0F0h
    SELF.Theme.ItemNormalBkgndColor        = 0F0F0F0h
    SELF.Theme.MenuBarOverColorLeft        = COLOR:Navy
    SELF.Theme.MenuBarOverColorRight       = COLOR:Navy
    SELF.Theme.MenuBarSelectedColorLeft    = COLOR:Navy
    SELF.Theme.MenuBarSelectedColorRight   = COLOR:Navy
    SELF.Theme.TextNormalColor             = COLOR:Black
    SELF.Theme.TextSelectedColor           = COLOR:White
    SELF.Theme.TextOverColor               = COLOR:White
    SELF.Theme.Flat                        = FALSE
    SELF.Theme.ShowImageBar                = FALSE
    SELF.Theme.Embossed                    = TRUE
    SELF.Theme.LineMode                    = MenuLine:FullSeparator + MenuLine:Embossed
  OF 'WINDOWSCLASSICLIGHT'
    SELF.IgnoreWindowsTheme(FALSE)
    SELF.Theme.MenuBarNormalColorLeft      = 0F0F0F0h
    SELF.Theme.MenuBarNormalColorRight     = 0F0F0F0h
    SELF.Theme.SelectionBarColorLeft       = 0FF9E3Eh
    SELF.Theme.SelectionBarColorRight      = SELF.Theme.SelectionBarColorLeft
    SELF.Theme.SelectionBarVerticalGradient= TRUE
    SELF.Theme.SelectionBorderColor        = COLOR:None
    SELF.Theme.ImageBkgndColorLeft         = 0F0F0F0h
    SELF.Theme.ImageBkgndColorRight        = 0F0F0F0h
    SELF.Theme.ItemNormalBkgndColor        = 0F0F0F0h
    SELF.Theme.MenuBarOverColorLeft        = SELF.Theme.SelectionBarColorLeft
    SELF.Theme.MenuBarOverColorRight       = SELF.Theme.SelectionBarColorLeft
    SELF.Theme.MenuBarSelectedColorLeft    = SELF.Theme.SelectionBarColorLeft
    SELF.Theme.MenuBarSelectedColorRight   = SELF.Theme.SelectionBarColorLeft
    SELF.Theme.TextNormalColor             = COLOR:Black
    SELF.Theme.TextSelectedColor           = COLOR:White
    SELF.Theme.TextOverColor               = COLOR:White
    SELF.Theme.Flat                        = FALSE
    SELF.Theme.ShowImageBar                = FALSE
    SELF.Theme.Embossed                    = TRUE
    SELF.Theme.LineMode                    = MenuLine:FullSeparator + MenuLine:Embossed
  OF 'WINDOWSCLASSICLIGHTWHITE'
    SELF.IgnoreWindowsTheme(FALSE)
    SELF.Theme.MenuBarNormalColorLeft      = 0F0F0F0h
    SELF.Theme.MenuBarNormalColorRight     = 0F0F0F0h
    SELF.Theme.SelectionBarColorLeft       = 0FF9E3Eh
    SELF.Theme.SelectionBarColorRight      = SELF.Theme.SelectionBarColorLeft
    SELF.Theme.SelectionBarVerticalGradient= TRUE
    SELF.Theme.SelectionBorderColor        = COLOR:None
    SELF.Theme.ImageBkgndColorLeft         = COLOR:White
    SELF.Theme.ImageBkgndColorRight        = COLOR:White
    SELF.Theme.ItemNormalBkgndColor        = COLOR:White
    SELF.Theme.MenuBarOverColorLeft        = SELF.Theme.SelectionBarColorLeft
    SELF.Theme.MenuBarOverColorRight       = SELF.Theme.SelectionBarColorLeft
    SELF.Theme.MenuBarSelectedColorLeft    = SELF.Theme.SelectionBarColorLeft
    SELF.Theme.MenuBarSelectedColorRight   = SELF.Theme.SelectionBarColorLeft
    SELF.Theme.TextNormalColor             = COLOR:Black
    SELF.Theme.TextSelectedColor           = COLOR:White
    SELF.Theme.TextOverColor               = COLOR:White
    SELF.Theme.Flat                        = FALSE
    SELF.Theme.ShowImageBar                = FALSE
    SELF.Theme.Embossed                    = TRUE
    SELF.Theme.LineMode                    = MenuLine:FullSeparator + MenuLine:Embossed
  OF 'WINDOWS2K'
    SELF.Theme.MenuBarNormalColorLeft      = 0F0F0F0h
    SELF.Theme.MenuBarNormalColorRight     = 0F0F0F0h
    SELF.Theme.SelectionBarColorLeft       = COLOR:Silver
    SELF.Theme.SelectionBarColorRight      = COLOR:Silver
    SELF.Theme.SelectionBarVerticalGradient= TRUE
    SELF.Theme.SelectionBorderColor        = COLOR:Gray
    SELF.Theme.ImageBkgndColorLeft         = COLOR:Silver
    SELF.Theme.ImageBkgndColorRight        = COLOR:Silver
    SELF.Theme.ItemNormalBkgndColor        = COLOR:Menu
    SELF.Theme.MenuBarOverColorLeft        = COLOR:Silver
    SELF.Theme.MenuBarOverColorRight       = COLOR:Silver
    SELF.Theme.MenuBarSelectedColorLeft    = COLOR:Silver
    SELF.Theme.MenuBarSelectedColorRight   = COLOR:Silver
    SELF.Theme.TextNormalColor             = COLOR:Black
    SELF.Theme.TextSelectedColor           = COLOR:Black
    SELF.Theme.TextOverColor               = COLOR:Black
    SELF.Theme.Flat                        = TRUE
    SELF.Theme.ShowImageBar                = TRUE
    SELF.Theme.Embossed                    = FALSE
    SELF.Theme.LineMode                    = MenuLine:Default
  OF 'VISTABLACK'
    SELF.Theme.MenuBarNormalColorLeft      = 0D0D0D0h
    SELF.Theme.MenuBarNormalColorRight     = COLOR:Gray
    SELF.Theme.MenuBarOverColorLeft        = 0D0D0D0h
    SELF.Theme.MenuBarOverColorRight       = 0D0D0D0h
    SELF.Theme.MenuBarSelectedColorLeft    = 0BFBFBFh
    SELF.Theme.MenuBarSelectedColorRight   = 0DADADAh
    SELF.Theme.SelectionBarColorLeft       = 0D0D0D0h
    SELF.Theme.SelectionBarColorRight      = COLOR:Gray
    SELF.Theme.SelectionBarVerticalGradient= TRUE
    SELF.Theme.SelectionBorderColor        = 0969696h
    SELF.Theme.ImageBkgndColorLeft         = 0D0D0D0h
    SELF.Theme.ImageBkgndColorRight        = COLOR:Gray
    SELF.Theme.ItemNormalBkgndColor        = 0F0F0F0h
    SELF.Theme.TextNormalColor             = COLOR:Black
    SELF.Theme.TextSelectedColor           = COLOR:Black
    SELF.Theme.TextOverColor               = COLOR:Black
    SELF.Theme.Flat                        = FALSE
    SELF.Theme.ShowImageBar                = FALSE
    SELF.Theme.Embossed                    = TRUE
    SELF.Theme.LineMode                    = MenuLine:ShowGutter + MenuLine:Embossed
  OF 'VISTAAERO'
    SELF.Theme.MenuBarNormalColorLeft      = COLOR:White
    SELF.Theme.MenuBarNormalColorRight     = 0F5E6E1h
    SELF.Theme.MenuBarOverColorLeft        = 0FF9933h
    SELF.Theme.MenuBarOverColorRight       = 0FF9933h
    SELF.Theme.MenuBarSelectedColorLeft    = 0FF9933h
    SELF.Theme.MenuBarSelectedColorRight   = 0FF9933h
    SELF.Theme.SelectionBarColorLeft       = 0FFEDDBh
    SELF.Theme.SelectionBarColorRight      = 0FFDCB9h
    SELF.Theme.SelectionBarVerticalGradient= TRUE
    SELF.Theme.SelectionBorderColor        = 0FFBD7Bh
    SELF.Theme.ImageBkgndColorLeft         = 0F0F0F0h
    SELF.Theme.ImageBkgndColorRight        = 0F0F0F0h
    SELF.Theme.ItemNormalBkgndColor        = 0F0F0F0h
    SELF.Theme.TextNormalColor             = COLOR:Black
    SELF.Theme.TextSelectedColor           = COLOR:Black
    SELF.Theme.TextOverColor               = COLOR:Black
    SELF.Theme.Flat                        = FALSE
    SELF.Theme.ShowImageBar                = FALSE
    SELF.Theme.Embossed                    = TRUE
    SELF.Theme.LineMode                    = MenuLine:ShowGutter + MenuLine:Embossed
  OF 'VISTAAEROWHITE'
    SELF.Theme.MenuBarNormalColorLeft      = COLOR:White
    SELF.Theme.MenuBarNormalColorRight     = 0F5E6E1h
    SELF.Theme.MenuBarOverColorLeft        = 0FF9933h
    SELF.Theme.MenuBarOverColorRight       = 0FF9933h
    SELF.Theme.MenuBarSelectedColorLeft    = 0FF9933h
    SELF.Theme.MenuBarSelectedColorRight   = 0FF9933h
    SELF.Theme.SelectionBarColorLeft       = 0FFEDDBh
    SELF.Theme.SelectionBarColorRight      = 0FFDCB9h
    SELF.Theme.SelectionBarVerticalGradient= TRUE
    SELF.Theme.SelectionBorderColor        = 0FFBD7Bh
    SELF.Theme.ImageBkgndColorLeft         = COLOR:White
    SELF.Theme.ImageBkgndColorRight        = COLOR:White
    SELF.Theme.ItemNormalBkgndColor        = COLOR:White
    SELF.Theme.TextNormalColor             = COLOR:Black
    SELF.Theme.TextSelectedColor           = COLOR:Black
    SELF.Theme.TextOverColor               = COLOR:Black
    SELF.Theme.Flat                        = FALSE
    SELF.Theme.ShowImageBar                = FALSE
    SELF.Theme.Embossed                    = TRUE
    SELF.Theme.LineMode                    = MenuLine:ShowGutter + MenuLine:Embossed
  OF 'BABYBLUE'  
    SELF.Theme.MenuBarNormalColorLeft      = 0F7F7F7h
    SELF.Theme.MenuBarNormalColorRight     = 0EAEAEAh
    SELF.Theme.MenuBarOverColorLeft        = 0FCDCBEh
    SELF.Theme.MenuBarOverColorRight       = 0FCDCBEh
    SELF.Theme.MenuBarSelectedColorLeft    = 0FCDCBEh
    SELF.Theme.MenuBarSelectedColorRight   = 0FCDCBEh
    SELF.Theme.SelectionBarColorLeft       = 0FBD2AEh
    SELF.Theme.SelectionBarColorRight      = 0FBD2AEh
    SELF.Theme.SelectionBarVerticalGradient= TRUE
    SELF.Theme.SelectionBorderColor        = 0FFB56Ah
    SELF.Theme.ImageBkgndColorLeft         = 0F0F0F0h
    SELF.Theme.ImageBkgndColorRight        = 0F0F0F0h
    SELF.Theme.ItemNormalBkgndColor        = 0F0F0F0h
    SELF.Theme.TextNormalColor             = COLOR:Black
    SELF.Theme.TextSelectedColor           = COLOR:Black
    SELF.Theme.TextOverColor               = COLOR:Black
    SELF.Theme.Flat                        = FALSE
    SELF.Theme.ShowImageBar                = FALSE
    SELF.Theme.Embossed                    = TRUE
    SELF.Theme.LineMode                    = MenuLine:ShowGutter + MenuLine:Embossed
  OF 'BABYBLUEWHITE'
    SELF.Theme.MenuBarNormalColorLeft      = 0F7F7F7h
    SELF.Theme.MenuBarNormalColorRight     = 0EAEAEAh
    SELF.Theme.MenuBarOverColorLeft        = 0FCDCBEh
    SELF.Theme.MenuBarOverColorRight       = 0FCDCBEh
    SELF.Theme.MenuBarSelectedColorLeft    = 0FCDCBEh
    SELF.Theme.MenuBarSelectedColorRight   = 0FCDCBEh
    SELF.Theme.SelectionBarColorLeft       = 0FBD2AEh
    SELF.Theme.SelectionBarColorRight      = 0FBD2AEh
    SELF.Theme.SelectionBarVerticalGradient= TRUE
    SELF.Theme.SelectionBorderColor        = 0FFB56Ah
    SELF.Theme.ImageBkgndColorLeft         = COLOR:White
    SELF.Theme.ImageBkgndColorRight        = COLOR:White
    SELF.Theme.ItemNormalBkgndColor        = COLOR:White
    SELF.Theme.TextNormalColor             = COLOR:Black
    SELF.Theme.TextSelectedColor           = COLOR:Black
    SELF.Theme.TextOverColor               = COLOR:Black
    SELF.Theme.Flat                        = FALSE
    SELF.Theme.ShowImageBar                = FALSE
    SELF.Theme.Embossed                    = TRUE
    SELF.Theme.LineMode                    = MenuLine:ShowGutter + MenuLine:Embossed
  OF 'XPLUNABLUE'
    SELF.Theme.MenuBarNormalColorLeft      = COLOR:White
    SELF.Theme.MenuBarNormalColorRight     = 0F5E6E1h
    SELF.Theme.MenuBarOverColorLeft        = 0FEECDDh
    SELF.Theme.MenuBarOverColorRight       = 0E2A981h
    SELF.Theme.MenuBarSelectedColorLeft    = 0FEECDDh
    SELF.Theme.MenuBarSelectedColorRight   = 0E2A981h
    SELF.Theme.SelectionBarColorLeft       = 0FEECDDh
    SELF.Theme.SelectionBarColorRight      = 0E2A981h
    SELF.Theme.SelectionBarVerticalGradient= TRUE
    SELF.Theme.SelectionBorderColor        = COLOR:Navy
    SELF.Theme.ImageBkgndColorLeft         = 0FEECDDh
    SELF.Theme.ImageBkgndColorRight        = 0E2A981h
    SELF.Theme.ItemNormalBkgndColor        = 0F0F0F0h
    SELF.Theme.TextNormalColor             = COLOR:Black
    SELF.Theme.TextSelectedColor           = COLOR:White
    SELF.Theme.TextOverColor               = COLOR:White
    SELF.Theme.Flat                        = TRUE
    SELF.Theme.ShowImageBar                = TRUE
    SELF.Theme.Embossed                    = TRUE
    SELF.Theme.LineMode                    = MenuLine:Default + MenuLine:Embossed
  OF 'XPLUNAGREEN'
    SELF.Theme.MenuBarNormalColorLeft      = COLOR:White!
    SELF.Theme.MenuBarNormalColorRight     = 0F5E6E1h!
    SELF.Theme.MenuBarOverColorLeft        = 0DEF7F4h
    SELF.Theme.MenuBarOverColorRight       = 091C6B7h
    SELF.Theme.MenuBarSelectedColorLeft    = 0DEF7F4h
    SELF.Theme.MenuBarSelectedColorRight   = 091C6B7h
    SELF.Theme.SelectionBarColorLeft       = 0DEF7F4h
    SELF.Theme.SelectionBarColorRight      = 091C6B7h
    SELF.Theme.SelectionBarVerticalGradient= TRUE
    SELF.Theme.SelectionBorderColor        = 05CAB96h
    SELF.Theme.ImageBkgndColorLeft         = 0F1FCFBh!0DEF7F4h
    SELF.Theme.ImageBkgndColorRight        = 0DDEEEAh!091C6B7h
    SELF.Theme.ItemNormalBkgndColor        = 0F0F0F0h
    SELF.Theme.TextNormalColor             = COLOR:Black
    SELF.Theme.TextSelectedColor           = COLOR:White
    SELF.Theme.TextOverColor               = COLOR:White
    SELF.Theme.Flat                        = TRUE
    SELF.Theme.ShowImageBar                = TRUE
    SELF.Theme.Embossed                    = TRUE
    SELF.Theme.LineMode                    = MenuLine:Default + MenuLine:Embossed
  OF 'XPLUNASILVER'
    SELF.Theme.MenuBarNormalColorLeft      = COLOR:White
    SELF.Theme.MenuBarNormalColorRight     = 0F5E6E1h
    SELF.Theme.MenuBarOverColorLeft        = 0E6DBDBh
    SELF.Theme.MenuBarOverColorRight       = 0BFA6A7h
    SELF.Theme.MenuBarSelectedColorLeft    = 0E6DBDBh
    SELF.Theme.MenuBarSelectedColorRight   = 0BFA6A7h
    SELF.Theme.SelectionBarColorLeft       = 0E6DBDBh
    SELF.Theme.SelectionBarColorRight      = 0BFA6A7h
    SELF.Theme.SelectionBarVerticalGradient= TRUE
    SELF.Theme.SelectionBorderColor        = COLOR:GrayText
    SELF.Theme.ImageBkgndColorLeft         = 0E6DBDBh
    SELF.Theme.ImageBkgndColorRight        = 0BFA6A7h
    SELF.Theme.ItemNormalBkgndColor        = 0F0F0F0h
    SELF.Theme.TextNormalColor             = COLOR:Black
    SELF.Theme.TextSelectedColor           = COLOR:White
    SELF.Theme.TextOverColor               = COLOR:White
    SELF.Theme.Flat                        = FALSE
    SELF.Theme.ShowImageBar                = TRUE
    SELF.Theme.Embossed                    = TRUE
    SELF.Theme.LineMode                    = MenuLine:Default + MenuLine:Embossed
  OF 'XPENERGYBLUE'
    SELF.Theme.MenuBarNormalColorLeft      = COLOR:White
    SELF.Theme.MenuBarNormalColorRight     = 0F5E6E1h
    SELF.Theme.MenuBarOverColorLeft        = 0FFCFBDh
    SELF.Theme.MenuBarOverColorRight       = 0FFCFBDh
    SELF.Theme.MenuBarSelectedColorLeft    = 0FFBD7Bh
    SELF.Theme.MenuBarSelectedColorRight   = 0FFBD7Bh
    SELF.Theme.SelectionBarColorLeft       = 0E2A981h
    SELF.Theme.SelectionBarColorRight      = 0C08000h
    SELF.Theme.SelectionBarVerticalGradient= TRUE
    SELF.Theme.SelectionBorderColor        = 0C08000h
    SELF.Theme.ImageBkgndColorLeft         = 0C08000h
    SELF.Theme.ImageBkgndColorRight        = 0E2A981h
    SELF.Theme.ItemNormalBkgndColor        = 0F0F0F0h
    SELF.Theme.TextNormalColor             = COLOR:Black
    SELF.Theme.TextSelectedColor           = COLOR:White
    SELF.Theme.TextOverColor               = COLOR:White
    SELF.Theme.Flat                        = FALSE
    SELF.Theme.ShowImageBar                = TRUE
    SELF.Theme.Embossed                    = TRUE
    SELF.Theme.LineMode                    = MenuLine:Default + MenuLine:Embossed
  OF 'VISTAENERGYBLUE'
    SELF.Theme.MenuBarNormalColorLeft      = COLOR:White
    SELF.Theme.MenuBarNormalColorRight     = 0F5E6E1h
    SELF.Theme.MenuBarOverColorLeft        = 0E2A981h
    SELF.Theme.MenuBarOverColorRight       = 0C08000h
    SELF.Theme.MenuBarSelectedColorLeft    = 0E2A981h
    SELF.Theme.MenuBarSelectedColorRight   = 0C08000h
    SELF.Theme.SelectionBarColorLeft       = 0E2A981h
    SELF.Theme.SelectionBarColorRight      = 0C08000h
    SELF.Theme.SelectionBarVerticalGradient= TRUE
    SELF.Theme.SelectionBorderColor        = 0C08000h
    SELF.Theme.ImageBkgndColorLeft         = 0E9BC9Eh
    SELF.Theme.ImageBkgndColorRight        = 0EC9F00h
    SELF.Theme.ItemNormalBkgndColor        = 0F0F0F0h
    SELF.Theme.TextNormalColor             = COLOR:Black
    SELF.Theme.TextSelectedColor           = COLOR:White
    SELF.Theme.TextOverColor               = COLOR:White
    SELF.Theme.Flat                        = FALSE
    SELF.Theme.ShowImageBar                = TRUE
    SELF.Theme.Embossed                    = TRUE
    SELF.Theme.LineMode                    = MenuLine:Default + MenuLine:Embossed
  OF 'ENERGYBLUELIGHT'  
    SELF.Theme.MenuBarNormalColorLeft      = COLOR:White
    SELF.Theme.MenuBarNormalColorRight     = 0F5E6E1h
    SELF.Theme.MenuBarOverColorLeft        = 0E2A981h
    SELF.Theme.MenuBarOverColorRight       = 0E2A981h
    SELF.Theme.MenuBarSelectedColorLeft    = 0E2A981h
    SELF.Theme.MenuBarSelectedColorRight   = 0E2A981h
    SELF.Theme.SelectionBarColorLeft       = 0E2A981h
    SELF.Theme.SelectionBarColorRight      = 0E2A981h
    SELF.Theme.SelectionBarVerticalGradient= TRUE
    SELF.Theme.SelectionBorderColor        = 0C08000h
    SELF.Theme.ImageBkgndColorLeft         = 0F7F7F7h
    SELF.Theme.ImageBkgndColorRight        = 0EAEAEAh
    SELF.Theme.ItemNormalBkgndColor        = 0F0F0F0h
    SELF.Theme.TextNormalColor             = COLOR:Black
    SELF.Theme.TextSelectedColor           = COLOR:White
    SELF.Theme.TextOverColor               = COLOR:White
    SELF.Theme.Flat                        = FALSE
    SELF.Theme.ShowImageBar                = TRUE
    SELF.Theme.Embossed                    = TRUE
    SELF.Theme.LineMode                    = MenuLine:Default + MenuLine:Embossed
  OF 'XPLUNABLUELIGHT'
    SELF.Theme.MenuBarNormalColorLeft      = COLOR:White
    SELF.Theme.MenuBarNormalColorRight     = 0F5E6E1h
    SELF.Theme.MenuBarOverColorLeft        = 0FFF5EEh
    SELF.Theme.MenuBarOverColorRight       = 0F0D5C1h
    SELF.Theme.MenuBarSelectedColorLeft    = 0FFF5EEh
    SELF.Theme.MenuBarSelectedColorRight   = 0F0D5C1h
    SELF.Theme.SelectionBarColorLeft       = 0FFF5EEh
    SELF.Theme.SelectionBarColorRight      = 0F0D5C1h
    SELF.Theme.SelectionBarVerticalGradient= TRUE
    SELF.Theme.SelectionBorderColor        = COLOR:Navy
    SELF.Theme.ImageBkgndColorLeft         = 0FFF9F4h
    SELF.Theme.ImageBkgndColorRight        = 0F8ECE2h
    SELF.Theme.ItemNormalBkgndColor        = 0F0F0F0h
    SELF.Theme.TextNormalColor             = COLOR:Black
    SELF.Theme.TextSelectedColor           = COLOR:Black
    SELF.Theme.TextOverColor               = COLOR:Black
    SELF.Theme.Flat                        = FALSE
    SELF.Theme.ShowImageBar                = TRUE
    SELF.Theme.Embossed                    = TRUE
    SELF.Theme.LineMode                    = MenuLine:Default + MenuLine:Embossed
  OF 'XPLUNAGREENLIGHT'
    SELF.Theme.MenuBarNormalColorLeft      = COLOR:White
    SELF.Theme.MenuBarNormalColorRight     = 0F5E6E1h
    SELF.Theme.MenuBarOverColorLeft        = 0E3F9F7h
    SELF.Theme.MenuBarOverColorRight       = 0B0D7CCh
    SELF.Theme.MenuBarSelectedColorLeft    = 0E3F9F7h
    SELF.Theme.MenuBarSelectedColorRight   = 0B0D7CCh
    SELF.Theme.SelectionBarColorLeft       = 0E3F9F7h
    SELF.Theme.SelectionBarColorRight      = 0B0D7CCh
    SELF.Theme.SelectionBarVerticalGradient= TRUE
    SELF.Theme.SelectionBorderColor        = 05CAB96h
    SELF.Theme.ImageBkgndColorLeft         = 0F1FCFBh
    SELF.Theme.ImageBkgndColorRight        = 0DDEEEAh
    SELF.Theme.ItemNormalBkgndColor        = 0F0F0F0h
    SELF.Theme.TextNormalColor             = COLOR:Black
    SELF.Theme.TextSelectedColor           = COLOR:Black
    SELF.Theme.TextOverColor               = COLOR:Black
    SELF.Theme.Flat                        = FALSE
    SELF.Theme.ShowImageBar                = TRUE
    SELF.Theme.Embossed                    = TRUE
    SELF.Theme.LineMode                    = MenuLine:Default + MenuLine:Embossed
  ELSE
    !If the string is not supported it call custom colors that need to be overrided by the user    
    SELF.SetCustomThemeColors (UPPER(pTheme))
  END
  RETURN

MenuStyleManager.SetCustomThemeColors  PROCEDURE(STRING pTheme)
  CODE
  RETURN

MenuStyleManager.SetDefaultThemeValues  PROCEDURE()    
  CODE
  SELF.IgnoreWindowsTheme(FALSE)
  SELF.Theme.MenuBarNormalColorLeft      = COLOR:MENUBAR
  SELF.Theme.MenuBarNormalColorRight     = COLOR:MENUBAR
  SELF.Theme.MenuBarOverColorLeft        = COLOR:HIGHLIGHT
  SELF.Theme.MenuBarOverColorRight       = COLOR:HIGHLIGHT
  SELF.Theme.MenuBarSelectedColorLeft    = COLOR:HIGHLIGHT
  SELF.Theme.MenuBarSelectedColorRight   = COLOR:HIGHLIGHT

  SELF.Theme.SelectionBarColorLeft       = COLOR:HIGHLIGHT
  SELF.Theme.SelectionBarColorRight      = COLOR:HIGHLIGHT
  SELF.Theme.SelectionBarVerticalGradient= TRUE
  SELF.Theme.SelectionBorderColor        = COLOR:HIGHLIGHT

  SELF.Theme.ImageBkgndColorLeft         = COLOR:Menu
  SELF.Theme.ImageBkgndColorRight        = COLOR:Menu

  SELF.Theme.ItemNormalBkgndColor        = COLOR:Menu

  SELF.Theme.TextNormalColor             = COLOR:MENUTEXT
  SELF.Theme.TextSelectedColor           = COLOR:HIGHLIGHTTEXT
  SELF.Theme.TextOverColor               = COLOR:HIGHLIGHTTEXT

  SELF.Theme.Flat                        = FALSE
  SELF.Theme.ShowImageBar                = FALSE
  SELF.Theme.Embossed                    = TRUE
  SELF.Theme.LineMode                    = MenuLine:FullSeparator + MenuLine:Embossed
  RETURN

MenuStyleManager.ClearThemeValues  PROCEDURE()    
  CODE
  SELF.Theme.MenuBarNormalColorLeft      = COLOR:None
  SELF.Theme.MenuBarNormalColorRight     = COLOR:None
  SELF.Theme.MenuBarOverColorLeft        = COLOR:None
  SELF.Theme.MenuBarOverColorRight       = COLOR:None
  SELF.Theme.MenuBarSelectedColorLeft    = COLOR:None
  SELF.Theme.MenuBarSelectedColorRight   = COLOR:None

  SELF.Theme.SelectionBarColorLeft       = COLOR:None
  SELF.Theme.SelectionBarColorRight      = COLOR:None
  SELF.Theme.SelectionBarVerticalGradient= TRUE
  SELF.Theme.SelectionBorderColor        = COLOR:None

  SELF.Theme.ImageBkgndColorLeft         = COLOR:None
  SELF.Theme.ImageBkgndColorRight        = COLOR:None

  SELF.Theme.ItemNormalBkgndColor        = COLOR:None

  SELF.Theme.TextNormalColor             = COLOR:None
  SELF.Theme.TextSelectedColor           = COLOR:None
  SELF.Theme.TextOverColor               = COLOR:None

  SELF.Theme.Flat                        = FALSE
  SELF.Theme.ShowImageBar                = FALSE
  SELF.Theme.Embossed                    = FALSE
  SELF.Theme.LineMode                    = MenuLine:Default
  RETURN

MenuStyleManager.SetWhiteBackground  PROCEDURE()
  CODE
  SELF.SetBackgroundColor (COLOR:White)
  RETURN
    
MenuStyleManager.SetBackgroundColor  PROCEDURE(LONG pColorValue)
  CODE
  SELF.Theme.ItemNormalBkgndColor = pColorValue
  RETURN

MenuStyleManager.ApplyTheme  PROCEDURE()
  CODE
  SELF.delayRefresh = TRUE
  IF SELF.UseCustomTheme()
      SELF.SetFlatMode (SELF.Theme.Flat)
      SELF.SetLinesEmbossed (SELF.Theme.Embossed)    
      SELF.SetLineMode (SELF.Theme.LineMode)
      IF SELF.Theme.ShowImageBar <> 0
        SELF.SetColor (MenuBrushes:ImageBkgnd, SELF.Theme.ImageBkgndColorLeft, SELF.Theme.ImageBkgndColorRight)
      ELSE
        SELF.SetColor (MenuBrushes:ImageBkgnd, SELF.Theme.ItemNormalBkgndColor, SELF.Theme.ItemNormalBkgndColor)
      END

      SELF.SetTextColor (MenuColors:NormalText,   SELF.Theme.TextNormalColor)
      SELF.SetTextColor (MenuColors:SelectedText, SELF.Theme.TextSelectedColor)
      SELF.SetTextColor (MenuColors:HotText,      SELF.Theme.TextOverColor)

      SELF.SetColor (MenuBrushes:NormalBarBkgnd,   SELF.Theme.MenuBarNormalColorLeft,   SELF.Theme.MenuBarNormalColorRight, TRUE)
      SELF.SetColor (MenuBrushes:SelectedBarBkgnd, SELF.Theme.MenuBarSelectedColorLeft, SELF.Theme.MenuBarSelectedColorRight, TRUE)
      SELF.SetColor (MenuBrushes:HotBkgnd,         SELF.Theme.MenuBarOverColorLeft,     SELF.Theme.MenuBarOverColorRight, TRUE)
        
      SELF.SetColor (MenuBrushes:SelectedBkgnd, SELF.Theme.SelectionBarColorLeft, SELF.Theme.SelectionBarColorRight, SELF.Theme.SelectionBarVerticalGradient)
      SELF.SetColor (MenuBrushes:FrameBrush,    SELF.Theme.SelectionBorderColor,  SELF.Theme.SelectionBorderColor, TRUE)

      SELF.SetColor (MenuBrushes:NormalBkgnd, SELF.Theme.ItemNormalBkgndColor, SELF.Theme.ItemNormalBkgndColor) 
  END
  SELF.Refresh ()
  SELF.delayRefresh = FALSE
  RETURN

MenuStyleManager.IgnoreWindowsTheme  PROCEDURE(BYTE ignore)
  CODE
  IF SELF.InitMenuInterface() AND SELF.MenuStyle.UseCustomTheme()
    SELF.MenuStyle.IgnoreOSTheme (ignore)
  END
  RETURN

MenuStyleManager.IsWindowsThemeIgnored  PROCEDURE()
  CODE
  IF SELF.InitMenuInterface()
    RETURN SELF.MenuStyle.IgnoreOSTheme()
  END
  RETURN TRUE
        
MenuStyleManager.MixColors   PROCEDURE(LONG pColor1,LONG pColor2,BYTE pPercentageMix)
CWColor1           LONG
RGBT1              GROUP,OVER(CWColor1)
R                    BYTE
G                    BYTE
B                    BYTE
NotUsed              BYTE
                   END
CWColor2           LONG
RGBT2              GROUP,OVER(CWColor2)
R                    BYTE
G                    BYTE
B                    BYTE
NotUsed              BYTE
                   END

RetCWColor         LONG
RetRGBT            GROUP,OVER(RetCWColor)
R                    BYTE
G                    BYTE
B                    BYTE
NotUsed              BYTE
                   END
  CODE
  IF pPercentageMix > 100
    pPercentageMix = 100
  END

  IF pColor1 = COLOR:None
    RETURN pColor2
  END

  IF pColor2 = COLOR:None
    RETURN pColor1
  END

  IF BAND (pColor1, 80000000H) <> 0
    pColor1 = MSMGetSysColor (BAND (pColor1, 0FFh))
  END
  IF BAND (pColor2, 80000000H) <> 0
    pColor2 = MSMGetSysColor (BAND (pColor2, 0FFh))
  END

  CWColor1 = pColor1
  CWColor2 = pColor2

  RetRGBT.R = (RGBT1.R + ((RGBT2.R - RGBT1.R) * pPercentageMix/100))
  RetRGBT.G = (RGBT1.G + ((RGBT2.G - RGBT1.G) * pPercentageMix/100))
  RetRGBT.B = (RGBT1.B + ((RGBT2.B - RGBT1.B) * pPercentageMix/100))

  RETURN RetCWColor
