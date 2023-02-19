Debuger CLASS,TYPE,MODULE('Debuger.CLW'),LINK('Debuger.CLW')!,_ABCLinkMode_),DLL(_ABCDllMode_)

debugfilter    string(1)
debugactive        byte
pgmname           cstring(25)
osmode         byte
Duplicates     long
numbersame     long
Lastmsg       cstring(250)
Thismsg       cstring(250)

init                    Procedure(<string>,<byte>,<short>)
!                       parm 1 = program name
!                       parm 2 = true/false - if true then debuger on regardless of project debug setting; default is false
!                                '/Debuger' on the command line will also turn on the debuger functions for that launch
!                       parm 3 = number of duplicate debug messages in a row before warning message.
!                                if zero, then no warning message; if omitted then default of 50 messages will be used

kill                    Procedure()

Debugout                PROCEDURE(string,<string>,<byte>,<byte>)
!                       Parm 1 = body of message
!                       Parm 2 = message header
!                       Parm 3 = if not omitted and = True then a message() will be issued along with the debugoutstriung
!                       Parm 4 = if not omitted and = true then the debugout will be issued regardless of debug setting.

DebugBreak              procedure()  ! 32bit api call
         end
