!ABCIncludeFile(ABC)

OMIT('_EndOfInclude_',_PDFTargetGenerator_)
_PDFTargetGenerator_ EQUATE(1)

INCLUDE('ABPRTARG.INC'),ONCE

!********************************************
!PDFTargetGenerator Class
PDFTargetGenerator    CLASS(TargetGenerator),MODULE('ABPRTARY.CLW'),THREAD,TYPE,LINK('ABPRTARY.CLW',_ABCLinkMode_),DLL(_ABCDllMode_)
Flate                BYTE, PROTECTED
Flated               BYTE, PROTECTED
TextBuffer           &STRING, PROTECTED
BufferLen            ULONG, PROTECTED
StreamBuffer         &STRING, PROTECTED
StreamLen            ULONG, PROTECTED
ObjNum               ULONG, PROTECTED
Encrypt              BYTE, PROTECTED
Encrypting           BYTE,PROTECTED
AccessPermissions    LONG,PROTECTED
EncryptInited        BYTE,PROTECTED
FlateStream          &STRING
Buffering            BYTE,PROTECTED
EscapedStream        &STRING, PROTECTED

Construct            PROCEDURE
Destruct             PROCEDURE
Init                 PROCEDURE(<STRING TargetFileName>),VIRTUAL
StoreStr             PROCEDURE(STRING str, LONG strLen, *BYTE retCode),BYTE,VIRTUAL
StartBuffering       PROCEDURE()
StopBuffering        PROCEDURE()
StartFlate           PROCEDURE()
StopFlate            PROCEDURE()
GetFlateStatus       PROCEDURE(),BYTE
CompressBuffer       PROCEDURE(),LONG !Return the new buffer size
GetBufferSize        PROCEDURE(),LONG
FlushBuffer          PROCEDURE(BYTE pEcapeCharacters=0)
EncryptID            CSTRING(256),PROTECTED
Password             CSTRING(256),PROTECTED
SetEncryption        PROCEDURE(BYTE Encrypt,<STRING pPassword>,<ULONG pAccessPermissions>)
GetEncryption        PROCEDURE(),BYTE
SetPassword          PROCEDURE(STRING pPassword,<ULONG pAccessPermissions>)
EncryptStart         PROCEDURE(ULONG nObjNum)
EncryptStop          PROCEDURE()
EncryptGetLastError  PROCEDURE(),LONG
EncryptGetOwnerString PROCEDURE(),STRING
EncryptGetUserString PROCEDURE(),STRING
EncryptInit          PROCEDURE(),LONG,PROTECTED
EncryptEnd           PROCEDURE()
EncryptGetID         PROCEDURE(),STRING
EncryptSetID         PROCEDURE(STRING pID)
EncryptGetHexID      PROCEDURE(),STRING
EncryptWriteStr      PROCEDURE(ULONG nObjNum,STRING str,BYTE pClipStr=0),BYTE,PROC
                  END
 _EndOfInclude_
