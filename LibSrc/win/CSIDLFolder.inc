!ABCIncludeFile


  OMIT('_EndOfInclude_',_CSIDLFolderClassPresent_)  ! Omit this if already compiled
_CSIDLFolderClassPresent_  EQUATE(1)

  include('ISystemFolder.inc'), ONCE

CSIDLFolder  CLASS,TYPE,IMPLEMENTS(ISystemFolder),MODULE('CSIDLFolder.clw'),LINK('CSIDLFolder.CLW',_ABCLinkMode_),DLL(_ABCDllMode_)
LastResult      LONG
CONSTRUCT       PROCEDURE()
             END


  _EndOfInclude_