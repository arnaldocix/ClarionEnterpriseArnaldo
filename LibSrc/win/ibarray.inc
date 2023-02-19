
!ABCIncludeFile(IBC)

  OMIT('_EndOfInclude_',_ArrayPresent_)
_ArrayPresent_ EQUATE(1)

  INCLUDE('IBCBASE.INC'),ONCE

Element                 CLASS(CInterface2),MODULE('IBARRAY'),DLL(_IbcDllMode_),TYPE
                        END


BaseArray               CLASS(CInterface2),MODULE('IBARRAY.CLW'),DLL(_IbcDllMode_),LINK('IBARRAY.CLW',_IbcLinkMode_),TYPE ! Abstract
Head                      LONG,PROTECTED
Used                      UNSIGNED,PROTECTED
ReleaseComponents         BOOL,PROTECTED
Size                      BYTE,PROTECTED

Construct                 PROCEDURE
Destruct                  PROCEDURE,DERIVED

_init                     PROCEDURE,PROTECTED
_space                    PROCEDURE,PROTECTED

CastItem                  PROCEDURE(SIGNED pos),LONG
DoClear                   PROCEDURE
Is_Item                   PROCEDURE(UNSIGNED pos = 0),BYTE
Kill                      PROCEDURE,VIRTUAL
Ordinality                PROCEDURE,SIGNED
SetReleaseComponents      PROCEDURE(BOOL b)
Swap                      PROCEDURE(UNSIGNED pos1, UNSIGNED pos2)
                        END



! An array or 'objects'

Array                   CLASS(BaseArray),MODULE('IBARRAY.CLW'),DLL(_IbcDllMode_),LINK('IBARRAY.CLW',_IbcLinkMode_),type
_set                      PROCEDURE(CInterface2 next, SIGNED index),PRIVATE

GetIterator               PROCEDURE(),*CIterator2
Kill                      PROCEDURE,DERIVED
Item                      PROCEDURE(SIGNED pos),*CInterface2
Replace                   PROCEDURE(CInterface2 next, UNSIGNED pos, BYTE nodel = 0)
Remove                    PROCEDURE(UNSIGNED pos, BYTE nodel = 0)
DoAppend                  PROCEDURE(CInterface2 next)
DoAdd                     PROCEDURE(CInterface2 next, UNSIGNED pos)
DoSort                    PROCEDURE(ICInterfaceCompare Cmp)
Find                      PROCEDURE(CInterface2 search),UNSIGNED
ZapAll                    PROCEDURE(CInterface2 search, BYTE nodel = 0),BYTE,PROC
Zap                       PROCEDURE(CInterface2 search, BYTE nodel = 0),BYTE,PROC
                        END


! An array of 'interfaces'

ArrayI                  CLASS(BaseArray),MODULE('IBARRAY.CLW'),DLL(_IbcDllMode_),LINK('IBARRAY.CLW',_IbcLinkMode_),type
_Set                      PROCEDURE(IInterface2 next, SIGNED index),PRIVATE

DoAdd                     PROCEDURE(IInterface2 next, UNSIGNED pos)
DoAppend                  PROCEDURE(IInterface2 next)
DoSort                    PROCEDURE(IIInterfaceCompare Cmp)
Find                      PROCEDURE(IInterface2 search),UNSIGNED
GetIterator               PROCEDURE(),*IIterator2
Item                      PROCEDURE(SIGNED pos),*IInterface2
Kill                      PROCEDURE,DERIVED
Remove                    PROCEDURE(UNSIGNED pos, BYTE nodel = 0)
Replace                   PROCEDURE(IInterface2 next, UNSIGNED pos, BYTE nodel = 0)
Zap                       PROCEDURE(IInterface2 search, BYTE nodel = 0),BYTE,PROC
ZapAll                    PROCEDURE(IInterface2 search, BYTE nodel = 0),BYTE,PROC
                        END


  _EndOfInclude_

