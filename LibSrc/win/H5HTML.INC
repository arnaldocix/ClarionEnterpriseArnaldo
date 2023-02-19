OMIT('_EndOfInclude_',_H5HtmlPresent_)
_H5HtmlPresent_ EQUATE(1)

                     INCLUDE('H5FILES.INC'),ONCE
                     INCLUDE('H5TXTOUT.INC'),ONCE
                     INCLUDE('H5CLIENT.INC'),ONCE

HTML:TextChanged     EQUATE('onChange')
HTML:TextValue       EQUATE('this.value')
HTML:TextAreaChanged EQUATE('onChange')
HTML:TextAreaValue   EQUATE('this.value')
HTML:CheckChanged    EQUATE('onClick')
HTML:CheckValue      EQUATE('this.checked')
HTML:RadioChanged    EQUATE('onClick')
HTML:SelectChanged   EQUATE('onChange')
HTML:SelectValue     EQUATE('this.selectedIndex')

!
! This module contains classes for outputing either Html files, or
! the internal java intereface file.
!

HtmlFontClass        CLASS,TYPE,MODULE('H5HTML.CLW'),DLL(BaseClassDllMode)
Color                  LONG
Face                   STRING(50)               ! Not CSTRING to compare is ok
Size                   SIGNED
Style                  SIGNED
                     END

HtmlFontQueue        QUEUE,TYPE
Feq                    SIGNED
ThisFont               &HtmlFontClass
                     END

HtmlOptionGroup      GROUP,TYPE
ScaleX                 REAL
ScaleY                 REAL
                     END

HtmlClass            CLASS(TextOutputClass),TYPE,MODULE('H5HTML.CLW'),LINK('H5HTML',LinkBaseClasses),DLL(BaseClassDllMode)
CreateOpen             PROCEDURE(STRING Filename, HtmlOptionGroup Opts, STRING JavaLibraryPath, *WebClientManagerClass Client)
GetFontChanged         PROCEDURE(*HtmlFontClass NewFont),BYTE
GetFontStyle           PROCEDURE(*HtmlFontClass CurFont),STRING
GetFontStyle           PROCEDURE(*HtmlFontClass CurFont, *HtmlFontClass PrevFont),STRING
GetPixelsX             PROCEDURE(SIGNED dlg),SIGNED
GetPixelsY             PROCEDURE(SIGNED dlg),SIGNED
GetControlReference    PROCEDURE(SIGNED Feq),STRING
Init                   PROCEDURE(*WebFilesClass Files)
Kill                   PROCEDURE
WriteSpace             PROCEDURE(SIGNED num)
PopFont                PROCEDURE(SIGNED Feq)
PushFont               PROCEDURE(*HtmlFontClass NewFont, SIGNED Feq)
TakeNewControl         PROCEDURE(SIGNED Feq, BYTE CanSelect)
!WriteAppletDimParameter PROCEDURE(SIGNED x, SIGNED y, SIGNED dx, SIGNED dy)
!WriteAppletFilenameParameter PROCEDURE(STRING param, STRING filename)
!WriteAppletFontParameter PROCEDURE(*HtmlFontClass CurFont)
!WriteAppletFooter      PROCEDURE()
!WriteAppletHeader      PROCEDURE(SIGNED Feq, STRING _ClassName, SIGNED Width, SIGNED Height, BYTE Scale=1)
!WriteAppletHeader      PROCEDURE(SIGNED Feq, STRING Name, STRING _ClassName, SIGNED Width, SIGNED Height, BYTE Scale),PRIVATE
!WriteAppletHeaderPixel PROCEDURE(SIGNED Feq, STRING _ClassName, SIGNED Width, SIGNED Height)
!WriteAppletOptParameter PROCEDURE(STRING param, STRING value)
!WriteAppletParameter   PROCEDURE(STRING param, STRING value)
!WriteAppletUAID        PROCEDURE(SIGNED Feq)
!WriteBasicAppletHeader PROCEDURE(STRING AppletName, STRING ClassName, SIGNED Width, SIGNED Height, BYTE Scale=1),PRIVATE
!WriteChildAppletFooter PROCEDURE()
!WriteChildAppletHeader PROCEDURE(STRING name, SIGNED Feq)
!WriteContainerAppletHeader PROCEDURE(SIGNED Feq, STRING ClassName, SIGNED Width, SIGNED Height, BYTE Scale=1)
WriteControlHeader     PROCEDURE()
WriteControlFooter     PROCEDURE()
WriteEventHandler      PROCEDURE(SIGNED Update, STRING HandlerName, STRING NewValue)
WriteFontFooter        PROCEDURE(*HtmlFontClass CurFont)
WriteFontHeader        PROCEDURE(*HtmlFontClass CurFont)
WriteFormFooter        PROCEDURE
WriteFormHeader        PROCEDURE(<STRING attr>)
!WriteFormHeaderNoJava  PROCEDURE(<STRING attr>)
WriteFrameCheckScript  PROCEDURE()
WriteJQueryReferences  PROCEDURE(),VIRTUAL
WriteJavaScript        PROCEDURE(),VIRTUAL
WriteJquerySubmit      PROCEDURE(),VIRTUAL
WriteOnFocusHandler    PROCEDURE(<STRING ActionText>)
WriteRefreshTimer      PROCEDURE(SIGNED Delay)
WriteTableHeader       PROCEDURE(<STRING attr>)
WriteTableFooter       PROCEDURE()
WriteTableNewCol       PROCEDURE(<STRING attr>)
WriteTableNewRow       PROCEDURE(<STRING>)
WriteText              PROCEDURE(STRING Text)
!WriteSubmitApplet      PROCEDURE(SIGNED Timer=0,SIGNED TimerAction=0)

AppletCount            SIGNED,PRIVATE
Browser                &BrowserManagerClass,PROTECTED
Client                 &WebClientManagerClass,PROTECTED
Files                  &WebFilesClass,PROTECTED
FocusControl           SIGNED,PRIVATE
FocusSelectable        BYTE,PRIVATE
Fonts                  &HtmlFontQueue,PRIVATE
!JavaLibraryCab         CSTRING(FILE:MaxFilepath),PRIVATE
!JavaLibraryZip         CSTRING(FILE:MaxFilepath),PRIVATE
ListboxCount           SIGNED,PRIVATE
Option                 LIKE(HtmlOptionGroup)
ShortParams            BYTE,PRIVATE
UseFonts               BYTE
                     END


_EndOfInclude_
