OMIT('_EndOfInclude_',_H5TxtOutPresent_)
_H5TxtOutPresent_ EQUATE(1)

TextOutputClass      CLASS,TYPE,MODULE('H5TXTOUT.CLW'),LINK('H5TXTOUT',LinkBaseClasses),DLL(BaseClassDllMode)
Close                  PROCEDURE
Open                   PROCEDURE(STRING)
CreateOpen             PROCEDURE(STRING)
GetSize                PROCEDURE(STRING),ULONG
Write                  PROCEDURE(STRING)
Writeln                PROCEDURE(<STRING>)
AppendTextFile         PROCEDURE(STRING fileName)
                     END

_EndOfInclude_
