
          MEMBER

          MAP
            MODULE('')
SHGetFolderPath     PROCEDURE(LONG pHandleOwner, LONG nFolder, LONG nToken, LONG pFlag, CONST *CSTRING szPath),LONG,PASCAL,NAME('SHGetFolderPathA')
SHCreateDirectoryEx PROCEDURE(LONG pHandleOwner, CONST *CSTRING szPath, LONG pSecurityAttribute),LONG,PASCAL,NAME('SHCreateDirectoryExA')
GetFullPathNameA    PROCEDURE(CONST *CSTRING szPathIn, LONG lSize, CONST *CSTRING szPathReturned, <CONST *CSTRING>),LONG,PROC,PASCAL,NAME('GetFullPathNameA')
            END
          END

  INCLUDE('CSIDLFolder.inc'), ONCE
  INCLUDE('CSIDL.EQU'), ONCE

CSIDLFolder.Construct    PROCEDURE()
  CODE
  RETURN  

  
CSIDLFolder.ISystemFolder.GetError  PROCEDURE()
  CODE
  RETURN SELF.LastResult


CSIDLFolder.ISystemFolder.GetDir   PROCEDURE(LONG nCSIDL)
szDirectory   CSTRING(FILE:MaxFilePath)
  CODE
  SELF.LastResult = SHGetFolderPath (0, nCSIDL, 0, SV:SHGFP_TYPE_CURRENT, szDirectory)
  RETURN CLIP(szDirectory)
    
    
CSIDLFolder.ISystemFolder.CreateDir PROCEDURE (CONST *CSTRING szDirectory)
szFullPath    CSTRING(FILE:MaxFilePath)
lRetValue     LONG,AUTO
  CODE
  lRetValue = GetFullPathNameA (szDirectory, FILE:MaxFilePath, szFullPath)
  IF lRetValue > 0 AND lRetValue < 248
    SELF.LastResult = SHCreateDirectoryEx (0, szFullPath, 0)
  ELSE
    SELF.LastResult = lRetValue
  END
  RETURN SELF.LastResult
    
    
CSIDLFolder.ISystemFolder.CreateDirIn   PROCEDURE (LONG nFolder, CONST *CSTRING szSubDirectoryName)
szParentDirectory    CSTRING(FILE:MaxFilePath)
szPath               CSTRING(FILE:MaxFilePath),AUTO
  CODE
  szParentDirectory = SELF.ISystemFolder.GetDir (nFolder)

  IF SELF.LastResult = 0
    IF szSubDirectoryName[1] = '\'
      szPath = szParentDirectory & szSubDirectoryName
    ELSE
      szPath = szParentDirectory & '\' & szSubDirectoryName
    END
    SELF.LastResult = SHCreateDirectoryEx (0, szPath, 0)
  END
  RETURN SELF.LastResult
