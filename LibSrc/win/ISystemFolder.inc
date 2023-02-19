
  OMIT('EndDefine',_ISystemFolder_)
_ISystemFolder_  EQUATE(1)

ISystemFolder       INTERFACE
GetDir                  PROCEDURE(LONG nCSIDL),STRING,PROC
CreateDirIn             PROCEDURE(LONG nCSIDL, CONST *CSTRING szDirectory),LONG,PROC
CreateDir               PROCEDURE(CONST *CSTRING szDirectory),LONG,PROC
GetError                PROCEDURE(),LONG
                    END

  EndDefine