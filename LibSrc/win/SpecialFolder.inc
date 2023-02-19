!ABCIncludeFile


  OMIT('_EndOfInclude_',_SpecialFolderClassPresent_)  ! Omit this if already compiled
_SpecialFolderClassPresent_  EQUATE(1)

	include('ISystemFolder.inc'), ONCE
	include('CSIDLFolder.inc'), once

SpecialFolder       CLASS, TYPE, MODULE('SpecialFolder.clw'), LINK('SpecialFolder.CLW',_ABCLinkMode_),DLL(_ABCDllMode_)

refISystemFolder        &ISystemFolder
DefaultISystemFolder    &CSIDLFolder   ! Used to supply a default ISystemFolder implementor

GetDir                  PROCEDURE(LONG nFolder), STRING
GetDir					PROCEDURE(LONG nFolder, STRING SubDirectory), STRING
GetError                PROCEDURE(), LONG
CreateDir               PROCEDURE(STRING sDirectory), LONG, PROC
CreateDirCS				PROCEDURE(*CSTRING szDirecory), LONG, PROC
CreateDirIn             PROCEDURE(LONG nFolder, STRING sSubDirectory), LONG, PROC
CreateDirInCS			PROCEDURE(LONG nFolder, *CSTRING szSubDirectory), LONG, PROC
SetISystemFolder        PROCEDURE(*ISystemFolder pISystemFolder)

SetPropDataPath			PROCEDURE(STRING sDataPath)


CONSTRUCT               PROCEDURE()
DESTRUCT				PROCEDURE()

				END

	_EndOfInclude_
