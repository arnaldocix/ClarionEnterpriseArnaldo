  MEMBER

  MAP
	END

	INCLUDE('SpecialFolder.inc'), ONCE


SpecialFolder.CONSTRUCT     PROCEDURE()

	CODE
		SELF.DefaultISystemFolder &= NEW CSIDLFolder
		SELF.refISystemFolder &= SELF.DefaultISystemFolder.ISystemFolder

SpecialFolder.DESTRUCT      PROCEDURE()

	CODE
		DISPOSE(SELF.DefaultISystemFolder)

		
SpecialFolder.GetDir        PROCEDURE(LONG nFolder)

	CODE
		RETURN SELF.refISystemFolder.GetDir(nFolder)
		
		
SpecialFolder.GetDir        PROCEDURE(LONG nFolder, STRING SubDir)
	CODE
		RETURN SELF.GetDir(nFolder) & CHOOSE(SubDir[1] = '\', SubDir, '\' & clip(SubDir))
		
		
SpecialFolder.GetError      PROCEDURE()

	CODE
		RETURN SELF.refISystemFolder.GetError()

SpecialFolder.CreateDir     PROCEDURE(STRING sDirectory)

szDir		CSTRING(260)

	CODE
		szDir = sDirectory
		RETURN SELF.refISystemFolder.CreateDir(szDir)
		
		
SpecialFolder.CreateDirCS   PROCEDURE(*CSTRING szDirectory)

	CODE
		RETURN SELF.refISystemFolder.CreateDir(szDirectory)
		
		
SpecialFolder.CreateDirIn   PROCEDURE(LONG nFolder, STRING sSubDirectory)

szDirectory					CSTRING(260)

	CODE
		szDirectory = sSubDirectory
		RETURN SELF.refISystemFolder.CreateDirIn(nFolder, szDirectory)
		
		
SpecialFolder.CreateDirInCS   PROCEDURE(LONG nFolder, *CSTRING szSubDirectory)

	CODE
		RETURN SELF.refISystemFolder.CreateDirIn(nFolder, szSubDirectory)
		
		
SpecialFolder.SetPropDataPath       PROCEDURE(STRING pDataPath)

	CODE
		system{PROP:DataPath} = pDataPath
		
		
SpecialFolder.SetISystemFolder      PROCEDURE(*ISystemFolder pISystemFolder)		

	CODE
		SELF.refISystemFolder &= pISystemFolder


