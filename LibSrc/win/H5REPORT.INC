OMIT('_EndOfInclude_',_H5ReportPresent_)
_H5ReportPresent_ EQUATE(1)

                     INCLUDE('H5CLIENT.INC'),ONCE

WebReportClass       CLASS,TYPE,MODULE('H5REPORT.CLW'),LINK('H5REPORT',LinkBaseClasses),TYPE,DLL(BaseClassDllMode)
Init                   PROCEDURE(WebServerClass, HtmlClass, QUEUE PreviewQueue)
Kill                   PROCEDURE,VIRTUAL
Preview                PROCEDURE,VIRTUAL
SetNumPages            PROCEDURE(SIGNED NumPages=0),VIRTUAL

Q                      &QUEUE,PROTECTED
Server                 &WebServerClass
Html                   &HtmlClass
NumPages               SIGNED,PROTECTED
                     END

_EndOfInclude_
