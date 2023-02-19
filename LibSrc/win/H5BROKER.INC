OMIT('_EndOfInclude_',_H5BrokerPresent_)
_H5BrokerPresent_ EQUATE(1)

                     INCLUDE('H5SINK.INC'),ONCE
                     INCLUDE('H5JSL.INC'),ONCE
                     INCLUDE('H5HTTP.INC'),ONCE
                     INCLUDE('H5CLIENT.INC'),ONCE
                     INCLUDE('H5FILES.INC'),ONCE

BrokerClass          CLASS(WebDataSinkClass),MODULE('H5Broker.clw'),LINK('H5BROKER',LinkBaseClasses),TYPE,DLL(BaseClassDllMode)
CloseChannel           PROCEDURE
GetAuthorizedInfo      PROCEDURE(STRING AreaName, *STRING User, *STRING Password)
GetClient              PROCEDURE,*WebClientManagerClass
GetEnabled             PROCEDURE,BYTE
GetRequestArguments    PROCEDURE,STRING
Init                   PROCEDURE(STRING ProgramName, WebFilesClass)
Kill                   PROCEDURE
OpenChannel            PROCEDURE,BYTE
ProcessHttpHeader      PROCEDURE(STRING)
SetClient              PROCEDURE
SetClientBrowser       PROCEDURE,PROTECTED
TakeBeforeBuildHeader  PROCEDURE,VIRTUAL
TakeFile               PROCEDURE(STRING Filename, SIGNED Security, BYTE dontmove),VIRTUAL
TakeHtmlPage           PROCEDURE(STRING Filename, SIGNED Security, BYTE dontmove),VIRTUAL
TakeJslData            PROCEDURE(STRING, SIGNED Security),VIRTUAL
TakeUnauthorized       PROCEDURE(STRING Filename, SIGNED Security),VIRTUAL

Channel                LONG,PRIVATE
CurClient              &WebClientManagerClass,PROTECTED
Files                  &WebFilesClass
Http                   &HttpClass
ServerName             CSTRING(255),PROTECTED
UseRPC                 BYTE,PRIVATE
                     END


_EndOfInclude_
