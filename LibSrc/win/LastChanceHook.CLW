  MEMBER
  PRAGMA('define(profile=>off)')
  INCLUDE('CWEXCPT.INT'),ONCE

  MAP
    Hook(*ICWExceptionInfo),LONG
    HEX(LONG),STRING,PRIVATE
    EnterProc(UNSIGNED Line,*CSTRING Proc,*CSTRING File),NAME('Profile:EnterProc')
    LeaveProc(),NAME('Profile:LeaveProc')
    GetExceptionText(LONG ExceptionNumber),STRING,PRIVATE
  END

EXCEPTION_ACCESS_VIOLATION          EQUATE(0C0000005H)
EXCEPTION_DATATYPE_MISALIGNMENT     EQUATE(080000002H)
EXCEPTION_BREAKPOINT                EQUATE(080000003H)
EXCEPTION_SINGLE_STEP               EQUATE(080000004H)
EXCEPTION_ARRAY_BOUNDS_EXCEEDED     EQUATE(0C000008CH)
EXCEPTION_FLT_DENORMAL_OPERAND      EQUATE(0C000008DH)
EXCEPTION_FLT_DIVIDE_BY_ZERO        EQUATE(0C000008EH)
EXCEPTION_FLT_INEXACT_RESULT        EQUATE(0C000008FH)
EXCEPTION_FLT_INVALID_OPERATION     EQUATE(0C0000090H)
EXCEPTION_FLT_OVERFLOW              EQUATE(0C0000091H)
EXCEPTION_FLT_STACK_CHECK           EQUATE(0C0000092H)
EXCEPTION_FLT_UNDERFLOW             EQUATE(0C0000093H)
EXCEPTION_INT_DIVIDE_BY_ZERO        EQUATE(0C0000094H)
EXCEPTION_INT_OVERFLOW              EQUATE(0C0000095H)
EXCEPTION_PRIV_INSTRUCTION          EQUATE(0C0000096H)
EXCEPTION_IN_PAGE_ERROR             EQUATE(0C0000006H)
EXCEPTION_ILLEGAL_INSTRUCTION       EQUATE(0C000001DH)
EXCEPTION_NONCONTINUABLE_EXCEPTION  EQUATE(0C0000025H)
EXCEPTION_STACK_OVERFLOW            EQUATE(0C00000FDH)
EXCEPTION_INVALID_DISPOSITION       EQUATE(0C0000026H)
EXCEPTION_GUARD_PAGE                EQUATE(080000001H)

TextEXCEPTION_UNKNOWN                  EQUATE('Unknown')
TextEXCEPTION_ACCESS_VIOLATION         EQUATE('Access Violation')
TextEXCEPTION_DATATYPE_MISALIGNMENT    EQUATE('Data Type Misalignment')
TextEXCEPTION_ARRAY_BOUNDS_EXCEEDED    EQUATE('Array Bounds Exceeded')
TextEXCEPTION_FLT_DENORMAL_OPERAND     EQUATE('Float Denormal Operand')
TextEXCEPTION_FLT_DIVIDE_BY_ZERO       EQUATE('Float Divide By Zero')
TextEXCEPTION_FLT_INEXACT_RESULT       EQUATE('Float Inexact Result')
TextEXCEPTION_FLT_INVALID_OPERATION    EQUATE('Float Invalid Operation')
TextEXCEPTION_FLT_OVERFLOW             EQUATE('Float Overflow')
TextEXCEPTION_FLT_STACK_CHECK          EQUATE('Float Stack Check')
TextEXCEPTION_FLT_UNDERFLOW            EQUATE('Float Underflow')
TextEXCEPTION_INT_DIVIDE_BY_ZERO       EQUATE('Divide by Zero')
TextEXCEPTION_INT_OVERFLOW             EQUATE('Integer Overflow')
TextEXCEPTION_PRIV_INSTRUCTION         EQUATE('Privileged Instruction')
TextEXCEPTION_IN_PAGE_ERROR            EQUATE('In Page Error')
TextEXCEPTION_ILLEGAL_INSTRUCTION      EQUATE('Illegal Instruction')
TextEXCEPTION_NONCONTINUABLE_EXCEPTION EQUATE('Non-Continuable Error')
TextEXCEPTION_STACK_OVERFLOW           EQUATE('Stack Overflow')
TextEXCEPTION_INVALID_DISPOSITION      EQUATE('Invalid Disposition')
TextEXCEPTION_GUARD_PAGE               EQUATE('Guard Page')

LastChanceHookRegistering   CLASS,PRIVATE
CONSTRUCT                      PROCEDURE()
                            END

ProfilerStackQueue  QUEUE,THREAD,PRIVATE
Line                    LONG
Proc                    CSTRING(256)
File                    CSTRING(256)
                    END

EnterProc           PROCEDURE(UNSIGNED Line,*CSTRING Proc,*CSTRING File)
  CODE
  PUSHERRORS()
    CLEAR(ProfilerStackQueue)
    ProfilerStackQueue.Line = Line
    ProfilerStackQueue.Proc = Proc
    ProfilerStackQueue.File = File
    ADD(ProfilerStackQueue)
  POPERRORS()
  RETURN
        
LeaveProc           PROCEDURE()
  CODE
  PUSHERRORS()
    GET(ProfilerStackQueue,RECORDS(ProfilerStackQueue))
    DELETE(ProfilerStackQueue)
  POPERRORS()
  RETURN
        
! ------------------------------------------------------------------------------

Hook  PROCEDURE (*ICWExceptionInfo info)

S       CSTRING(1024)
st      CSTRING(1024)
Caption CSTRING(256)
  CODE
  IF info &= NULL
    RETURN 0
  END

  Caption = 'Exception (' & HEX (info.ExceptionCode()) & ') '&GetExceptionText(info.ExceptionCode())&' at ' & HEX (info.ExceptionAddress())
  S = 'Address='&HEX (info.ExceptionAddress())&|
      '<13,10>Registers' & |
      '<13,10>EAX=' & HEX (info.Register (i386_Register:Reg32_EAX)) & |
      '       EBX=' & HEX (info.Register (i386_Register:Reg32_EBX)) & |
      '       ECX=' & HEX (info.Register (i386_Register:Reg32_ECX)) & |
      '       EDX=' & HEX (info.Register (i386_Register:Reg32_EDX)) & |
      '<13,10>ESI=' & HEX (info.Register (i386_Register:Reg32_ESI)) & |
      '       EDI=' & HEX (info.Register (i386_Register:Reg32_EDI)) & |
      '       ESP=' & HEX (info.Register (i386_Register:Reg32_ESP)) & |
      '       EBP=' & HEX (info.Register (i386_Register:Reg32_EBP)) & |
        '<13,10,13,10>Current thread is being terminated'
        LOOP I#=RECORDS(ProfilerStackQueue) TO 1 By -1
            GET(ProfilerStackQueue,I#)
            St=St&'<13,10>'&ProfilerStackQueue.Proc&' - '&ProfilerStackQueue.File&' ('&ProfilerStackQueue.Line&')'
        END
        S=S&'<13,10>Stack:'&St
 
  MESSAGE( S,Caption,,,,2)
  RETURN 1   !value of 1 signals to kill thread

! ------------------------------------------------------------------------------

HEX  PROCEDURE (LONG A)

i       UNSIGNED,AUTO
S       STRING(8),AUTO
DIGITS  STRING('0123456789ABCDEF'),STATIC

  CODE
  i = SIZE(S)

  LOOP WHILE i <> 0
    S [i] = DIGITS [BAND (A, 0Fh) + 1]
    A     = BSHIFT (A, -4)
    i    -= 1
  END

  RETURN S

! ------------------------------------------------------------------------------

LastChanceHookRegistering.CONSTRUCT PROCEDURE()
  CODE
  SYSTEM {PROP:LastChanceHook} = ADDRESS (Hook)
  RETURN
        
! ------------------------------------------------------------------------------

GetExceptionText PROCEDURE (LONG exceptionNumber)
  CODE
  CASE exceptionNumber
  OF EXCEPTION_ACCESS_VIOLATION
     RETURN TextEXCEPTION_ACCESS_VIOLATION
  OF EXCEPTION_DATATYPE_MISALIGNMENT
     RETURN TextEXCEPTION_DATATYPE_MISALIGNMENT
  OF EXCEPTION_ARRAY_BOUNDS_EXCEEDED
     RETURN TextEXCEPTION_ARRAY_BOUNDS_EXCEEDED
  OF EXCEPTION_FLT_DENORMAL_OPERAND
     RETURN TextEXCEPTION_FLT_DENORMAL_OPERAND
  OF EXCEPTION_FLT_DIVIDE_BY_ZERO
     RETURN TextEXCEPTION_FLT_DIVIDE_BY_ZERO
  OF EXCEPTION_FLT_INEXACT_RESULT
     RETURN TextEXCEPTION_FLT_INEXACT_RESULT
  OF EXCEPTION_FLT_INVALID_OPERATION
     RETURN TextEXCEPTION_FLT_INVALID_OPERATION
  OF EXCEPTION_FLT_OVERFLOW
     RETURN TextEXCEPTION_FLT_OVERFLOW
  OF EXCEPTION_FLT_STACK_CHECK
     RETURN TextEXCEPTION_FLT_STACK_CHECK
  OF EXCEPTION_FLT_UNDERFLOW
     RETURN TextEXCEPTION_FLT_UNDERFLOW
  OF EXCEPTION_INT_DIVIDE_BY_ZERO
     RETURN TextEXCEPTION_INT_DIVIDE_BY_ZERO
  OF EXCEPTION_INT_OVERFLOW
     RETURN TextEXCEPTION_INT_OVERFLOW
  OF EXCEPTION_PRIV_INSTRUCTION
     RETURN TextEXCEPTION_PRIV_INSTRUCTION
  OF EXCEPTION_IN_PAGE_ERROR
     RETURN TextEXCEPTION_IN_PAGE_ERROR
  OF EXCEPTION_ILLEGAL_INSTRUCTION
     RETURN TextEXCEPTION_ILLEGAL_INSTRUCTION
  OF EXCEPTION_NONCONTINUABLE_EXCEPTION
     RETURN TextEXCEPTION_NONCONTINUABLE_EXCEPTION
  OF EXCEPTION_STACK_OVERFLOW
     RETURN TextEXCEPTION_STACK_OVERFLOW
  OF EXCEPTION_INVALID_DISPOSITION
     RETURN TextEXCEPTION_INVALID_DISPOSITION
  OF EXCEPTION_GUARD_PAGE
     RETURN TextEXCEPTION_GUARD_PAGE
  ELSE
     RETURN TextEXCEPTION_UNKNOWN
  END
