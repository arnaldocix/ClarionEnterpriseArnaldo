!ABCIncludeFile

  OMIT('_EndOfInclude_',_ABCUserControlsPresent_)
_ABCUserControlsPresent_   EQUATE(1)

UserControlFEQList QUEUE,TYPE
OriginalFEQ         SIGNED
HostedFEQ           SIGNED
                   END

UserControlClass CLASS(WindowManager),TYPE,MODULE('ABUserControl.CLW'),LINK('ABUserControl.CLW',_ABCLinkMode_),DLL(_ABCDllMode_)
parentWindow       &Window,PROTECTED
FEQList            &UserControlFEQList,PROTECTED
AddFEQ             PROCEDURE(SIGNED pOriginalFEQ,SIGNED pHostedFEQ),BYTE,PROC,PROTECTED
Init               PROCEDURE(Window pTarget,SIGNED pParentFEQ=0,<LONG pXPos>,<LONG pYPos>),BYTE,PROC,VIRTUAL
Open               PROCEDURE(Window pControl,Window pTarget,SIGNED pParentFEQ=0,<LONG pXPos>,<LONG pYPos>),BYTE,PROC,PROTECTED
Clone              PROCEDURE(Window pTarget,SIGNED pParentFEQ=0,<LONG pXPos>,<LONG pYPos>),BYTE,PROC,PROTECTED
Contents           PROCEDURE(SIGNED pOriginalFEQ),STRING,PROTECTED
Change             PROCEDURE(SIGNED pOriginalFEQ,STRING value),PROTECTED
Construct          PROCEDURE()
Destruct           PROCEDURE()
UpdateWindow       PROCEDURE()
GetHostedFEQ       PROCEDURE(SIGNED pOriginalFEQ),SIGNED,PROTECTED
GetOriginalFEQ     PROCEDURE(SIGNED pHostedFEQ),SIGNED,PROTECTED
BindToControl      PROCEDURE(SIGNED pOriginalFEQ,*? SelfProperty),PROTECTED
!TakeFieldEvent     PROCEDURE(),DERIVED,BYTE,PROC
!TakeFieldEvent     PROCEDURE(SIGNED control),VIRTUAL,BYTE,PROC
Kill               PROCEDURE(),PROC,BYTE,VIRTUAL
                 END

UserControlList  QUEUE,TYPE
Control            &UserControlClass
                 END

UserControlContainerClass CLASS,IMPLEMENTS(WindowComponent),TYPE,MODULE('ABUserControl.CLW'),LINK('ABUserControl.CLW',_ABCLinkMode_),DLL(_ABCDllMode_)
parentWindow        &Window,PROTECTED
Controls            &UserControlList,PROTECTED
AddItem             PROCEDURE(UserControlClass pUserControl,SIGNED pParentFEQ=0,<LONG pXPos>,<LONG pYPos>),BYTE,PROC
Construct           PROCEDURE()
Destruct            PROCEDURE()
Init                PROCEDURE(Window pTarget)
Init                PROCEDURE(),VIRTUAL,PROTECTED
TakeEvent           PROCEDURE(),VIRTUAL
UpdateWindow        PROCEDURE()
Kill                PROCEDURE()
                  END

_EndOfInclude_