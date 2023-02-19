OMIT('_EndOfInclude_',_H5ClientPresent_)
_H5ClientPresent_ EQUATE(1)

                     INCLUDE('H5SINK.INC'),ONCE
                     INCLUDE('H5JSL.INC'),ONCE

ClarionComFeq        EQUATE(-29999)

                     ITEMIZE,PRE(BROWSER)
IE30                   EQUATE
IE40                   EQUATE
NetScape3              EQUATE
Mozilla4               EQUATE
Unknown                EQUATE
                     END

BrowserManagerClass  CLASS,MODULE('H5CLIENT.CLW'),LINK('H5CLIENT',LinkBaseClasses),TYPE,DLL(BaseClassDllMode)
Init                   PROCEDURE(BYTE, BYTE, BYTE),VIRTUAL

Kind                   BYTE
SetNoCache             BYTE
SupportsStyleSheets    BYTE
SubmitFromJava         BYTE
                     END


WebClientManagerInterface CLASS(WebDataSinkClass),MODULE('ABSTRACT'),TYPE,DLL(BaseClassDllMode)
Feq2Id                 PROCEDURE(SIGNED Feq),UNSIGNED,VIRTUAL
                     END


WebClientManagerClass CLASS(WebClientManagerInterface),MODULE('H5CLIENT.CLW'),TYPE,DLL(BaseClassDllMode)
Feq2Id                 PROCEDURE(SIGNED Feq),UNSIGNED,VIRTUAL
Init                   PROCEDURE(*WebDataSinkClass, *BrowserManagerClass)
Kill                   PROCEDURE
NextHtmlPage           PROCEDURE
SetIP                  PROCEDURE
TakeFile               PROCEDURE(STRING Filename, SIGNED, BYTE dontmove),VIRTUAL
TakeHtmlPage           PROCEDURE(STRING Filename, SIGNED, BYTE dontmove),VIRTUAL
TakeJslData            PROCEDURE(STRING, SIGNED),VIRTUAL
TakeUnauthorized       PROCEDURE(STRING Filename, SIGNED),VIRTUAL

Browser                &BrowserManagerClass
IP                     CSTRING(255)             ! IP address of the client
Jsl                    &JslManagerClass
Broker                 &WebDataSinkClass,PROTECTED
PageNumber             SIGNED,PRIVATE
                     END


_EndOfInclude_
