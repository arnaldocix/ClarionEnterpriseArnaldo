!ABCIncludeFile
  OMIT('__EndOfInclude__',__MDISyncro_INC)

__MDISyncro_INC  EQUATE(1)

 INCLUDE('CWSYNCHC.INC'),ONCE

MDISynchronization       CLASS,MODULE('MDISync.CLW'),LINK('MDISync.CLW',_ABCLinkMode_),DLL(_ABCDllMode_),TYPE
CloseFlag                   BYTE,PRIVATE
OpenFlag                    BYTE,PRIVATE
GainFlag                    BYTE,PRIVATE
Locked                      BYTE,PRIVATE
TakeEvent                   PROCEDURE(),BYTE,PROC
Construct                   PROCEDURE()
Destruct                    PROCEDURE()
Wait                        PROCEDURE(),PROTECTED
Release                     PROCEDURE(),PROTECTED
                        END

! __EndOfInclude__
