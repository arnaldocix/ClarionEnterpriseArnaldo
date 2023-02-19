! Declarations of INT64 and UINT64 types used in prototypes
!
! INT64    GROUP,TYPE
! lo         ULONG
! hi         LONG
!          END
!
! UINT64   GROUP,TYPE
! lo         ULONG
! hi         ULONG
!          END

             MODULE('i64* family of RTL functions')
!!! <summary>
!!! Returns sign of passed 64-bit signed integer
!!! </summary>
!!! <param name="v">Reference to 64-bit signed integer variable.</param>
!!! <returns>0 if passed value is 0, -1 if paased value is negative, +1 if passed value is positive.</returns>
i64Sign        PROCEDURE (*INT64 v),SIGNED,RAW,NAME('Cla$i64Sign')

!!! <summary>
!!! Negates value of passed signed 64-bit signed integer variable
!!! </summary>
!!! <param name="v">Reference to 64-bit signed integer variable.</param>
!!! <returns>0 if operation successful, other value in case of overflow.</returns> 
i64Negate      PROCEDURE (*INT64 v),UNSIGNED,PROC,RAW,NAME('Cla$i64Negate')

!!! <summary>
!!! Negates value of passed signed 64 bit signed integer variable if it is
!!! negative
!!! </summary>
!!! <param name="v">Reference to 64-bit signed integer variable.</param>
!!! <returns>0 if operation successful, other value in case of overflow.</returns> 
i64ABS         PROCEDURE (*INT64),UNSIGNED,PROC,RAW,NAME('Cla$i64ABS')

!!! <summary>
!!! Compares value of passed 64-bit integer with 0
!!! </summary>
!!! <param name="v">Reference to 64-bit integer variable.</param>
!!! <returns>TRUE if passed value is 0.</returns> 
i64Is0         PROCEDURE (*INT64 v),BOOL,RAW,NAME('Cla$i64is0')
i64Is0         PROCEDURE (*UINT64 v),BOOL,RAW,NAME('Cla$i64is0')

!!! <summary>
!!! Checks that value of passed 64-bit integer can be stored to 32-bit
!!! integer without truncating
!!! </summary>
!!! <param name="v">Reference to 64-bit integer variable.</param>
!!! <returns>TRUE if passed value has significant bits in lower ULONG only.</returns> 
is32Bit        PROCEDURE (*INT64 v),BOOL,RAW,NAME('Cla$is32Bit')
is32Bit        PROCEDURE (*UINT64 v),BOOL,RAW,NAME('Cla$is32BitU')

!!! <summary>
!!! Assign 32-bit value to passed 64-bit integer variable
!!! </summary>
!!! <param name="dest">Variable to change.</param>
!!! <param name="src">Value to assign to destination variable.</param>
i64Assign      PROCEDURE (*INT64 dest, LONG src),RAW,NAME('Cla$i64AssignS')
i64Assign      PROCEDURE (*UINT64 dest, ULONG src),RAW,NAME('Cla$i64AssignU')

!!! <summary>
!!! Assign 64-bit value to passed 64-bit integer variable
!!! </summary>
!!! <param name="dest">Variable to change.</param>
!!! <param name="src">Value to assign to destination variable.</param>
!!! <returns>0 if operation successful, other value in case of overflow or truncation.</returns> 
i64Assign      PROCEDURE (*INT64 dest,  *INT64  src),UNSIGNED,PROC,RAW,NAME('Cla$i64AssignSS')
i64Assign      PROCEDURE (*INT64 dest,  *UINT64 src),UNSIGNED,PROC,RAW,NAME('Cla$i64AssignSU')
i64Assign      PROCEDURE (*UINT64 dest, *INT64  src),UNSIGNED,PROC,RAW,NAME('Cla$i64AssignUS')
i64Assign      PROCEDURE (*UINT64 dest, *UINT64 src),UNSIGNED,PROC,RAW,NAME('Cla$i64AssignUU')

!!! <summary>
!!! Store 64-bit integer value to 32-bit integer variable
!!! </summary>
!!! <param name="src">64-bit integer variable.</param>
!!! <param name="dest">32-bit integer variable.</param>
!!! <returns>0 if operation successful, other value in case of truncation</returns>
i64To32        PROCEDURE (*INT64 src, *LONG dest),UNSIGNED,PROC,RAW,NAME('Cla$i64To32')
i64To32        PROCEDURE (*UINT64 src, *ULONG dest),UNSIGNED,PROC,RAW,NAME('Cla$i64To32U')

!!! <summary>
!!! Return lower 32 bits of the passed 64-bit integer parameter
!!! </summary>
!!! <param name="val">Source 64-bit integer.</param>
!!! <returns>Lower 32 bit of passed parameter as an unsigned integer value.</returns>
i64Lo32        PROCEDURE (*INT64 val),UNSIGNED,RAW,NAME('Cla$i64Lo32')
i64Lo32        PROCEDURE (*UINT64 val),UNSIGNED,RAW,NAME('Cla$i64Lo32U')

!!! <summary>
!!! Return upper 32 bits of the passed 64-bit integer parameter
!!! </summary>
!!! <param name="val">Source 64-bit signed integer.</param>
!!! <returns>Lower 32 bit of passed parameter as a signed integer value.</returns>
i64Hi32        PROCEDURE (*INT64 val),SIGNED,RAW,NAME('Cla$i64Hi32')

!!! <summary>
!!! Return upper 32 bits of the passed 64-bit integer parameter
!!! </summary>
!!! <param name="val">Source 64-bit unsigned integer.</param>
!!! <returns>Lower 32 bit of passed parameter as an unsigned integer value.</returns>
i64Hi32        PROCEDURE (*UINT64 val),UNSIGNED,RAW,NAME('Cla$i64Hi32U')

!!! <summary>
!!! Store DECIMAL value to 64-bit integer variable
!!! </summary>
!!! <param name="dest">64-bit integer variable.</param>
!!! <param name="src">DECIMAL value to assign to 64-bit integer variable.</param>
i64FromDecimal PROCEDURE (*INT64  dest, *DECIMAL src),RAW,NAME('Cla$i64FromDecS')
i64FromDecimal PROCEDURE (*UINT64 dest, *DECIMAL src),RAW,NAME('Cla$i64FromDecU')

!!! <summary>
!!! Store 64-bit integer value to DECIMAL variable.
!!! </summary>
!!! <param name="dest">DECIMAL variable.</param>
!!! <param name="src">64-bit integer value to store to DECIMAL variable.</param>
i64ToDecimal   PROCEDURE (*DECIMAL dest, *INT64  src),RAW,NAME('Cla$i64ToDecS')
i64ToDecimal   PROCEDURE (*DECIMAL dest, *UINT64 src),RAW,NAME('Cla$i64ToDecU')

!!! <summary>
!!! Shift value of 64-bit integer value to given number of bits to left or
!!! to right and store result to another 64-bit integer variable. Shift is
!!! to right if shift count value is negative, and is to left if shift count
!!! value is positive. The same variable can be passed both as a source value
!!! and as a destination variable. 
!!! </summary>
!!! <param name="src">64-bit integer value to shift.</param>
!!! <param name="cnt">Number of bits to shift.</param>
!!! <param name="dest">64-bit integer variable to store result to.</param>
i64Shift       PROCEDURE (*INT64  src, SIGNED cnt, *INT64  dest),RAW,NAME('Cla$i64Shift')
i64Shift       PROCEDURE (*UINT64 src, SIGNED cnt, *UINT64 dest),RAW,NAME('Cla$i64ShiftU')

!!! <summary>
!!! Shift value of 64-bit variable to left to given number of bits and store
!!! result to another 64-bit integer variable. The same variable can be passed
!!! both as a source value and as a destination variable.
!!! </summary>
!!! <param name="src">64-bit integer value to shift.</param>
!!! <param name="cnt">Number of bits to shift.</param>
!!! <param name="dest">64-bit integer variable to store result to.</param>
i64LShift      PROCEDURE (*INT64  src, UNSIGNED cnt, *INT64  dest),RAW,NAME('Cla$i64LShift')
i64LShift      PROCEDURE (*UINT64 src, UNSIGNED cnt, *UINT64 dest),RAW,NAME('Cla$i64LShiftU')

!!! <summary>
!!! Shift value of 64-bit variable to right to given number of bits and store
!!! result to another 64-bit integer variable. The same variable can be passed
!!! both as a source value and as a destination variable.
!!! </summary>
!!! <param name="src">64-bit integer value to shift.</param>
!!! <param name="cnt">Number of bits to shift.</param>
!!! <param name="dest">64-bit integer variable to store result to.</param>
i64RShift      PROCEDURE (*INT64  src, UNSIGNED cnt, *INT64  dest),RAW,NAME('Cla$i64RShift')
i64RShift      PROCEDURE (*UINT64 src, UNSIGNED cnt, *UINT64 dest),RAW,NAME('Cla$i64RShiftU')

!!! <summary>
!!! Add value of one 64-bit integer variable to another one. Possible overflow
!!! is ignored.
!!! </summary>
!!! <param name="dest">64-bit integer variable to increase by value of another variabke.</param>
!!! <param name="src">64-bit integer value to add to value of destination variable.</param>
i64Add         PROCEDURE (*INT64 dest,  *INT64 src),RAW,NAME('Cla$i64Add')
i64Add         PROCEDURE (*UINT64 dest, *UINT64 src),RAW,NAME('Cla$i64Add')

!!! <summary>
!!! Subtract value of one 64-bit integer variable from another one. Possible
!!! overflow is ignored.
!!! </summary>
!!! <param name="dest">64-bit integer variable to increase by value of another variabke.</param>
!!! <param name="src">64-bit integer value to add to value of destination variable.</param>
i64Sub         PROCEDURE (*INT64 dest,  *INT64 src),RAW,NAME('Cla$i64Sub')
i64Sub         PROCEDURE (*UINT64 dest, *UINT64 src),RAW,NAME('Cla$i64Sub')

!!! <summary>
!!! Assign sum of two 64-bit integer values to passed 64-bit integer variable.
!!! The same variable can be passed as one of operands and as a destination.
!!! Possible overflow is ignored.
!!! </summary>
!!! <param name="op1">First 64-bit integer operand.</param>
!!! <param name="op2">Second 64-bit integer operand.</param>
!!! <param name="dest">Variable to store the result.</param>
i64Add         PROCEDURE (*INT64  op1, *INT64  op2, *INT64  dest),RAW,NAME('Cla$i64Add3')
i64Add         PROCEDURE (*UINT64 op1, *UINT64 op2, *UINT64 dest),RAW,NAME('Cla$i64Add3U')

!!! <summary>
!!! Assign result of subtraction of two 64-bit integer values to passed 64-bit
!!! integer variable. The same variable can be passed as one of operands and
!!! as a destination. Possible overflow is ignored.
!!! </summary>
!!! <param name="op1">First 64-bit integer operand of subtraction.</param>
!!! <param name="op2">Second 64-bit integer operand of subtraction.</param>
!!! <param name="dest">Variable to store the result.</param>
i64Sub         PROCEDURE (*INT64  op1, *INT64  op2, *INT64  dest),RAW,NAME('Cla$i64Sub3')
i64Sub         PROCEDURE (*UINT64 op1, *UINT64 op2, *UINT64 dest),RAW,NAME('Cla$i64Sub3U')

!!! <summary>
!!! Multiply value of 64-bit integer variable by 2
!!! </summary>
!!! <param name="v">64-bit integer variable.</param>
!!! <returns>0 if operation successful, other value in case of overflow.</returns> 
i64Mult2       PROCEDURE (*INT64 v),UNSIGNED,PROC,RAW,NAME('Cla$i64Mult2')
i64Mult2       PROCEDURE (*UINT64 v),UNSIGNED,PROC,RAW,NAME('Cla$i64Mult2U')

!!! <summary>
!!! Multiply value of 64-bit integer variable by 10, add passed 32-bit value
!!! and store result back to variable
!!! <param name="v">64-bit integer variable.</param>
!!! <param name="inc">32-bit integer increment value.</param>
!!! <returns>0 if operation successful, other value in case of overflow.</returns> 
i64Mult10      PROCEDURE (*INT64  v, LONG  inc = 0),UNSIGNED,PROC,RAW,NAME('Cla$i64Mult10')
i64Mult10      PROCEDURE (*UINT64 v, ULONG inc = 0),UNSIGNED,PROC,RAW,NAME('Cla$i64Mult10U')

!!! <summary>
!!! Multiply two 64-bit integer values and store result to passed 64-bit
!!! integer variable. The same variable can be passed as one of parameters
!!! and as a destination. Possible overflow is ignored.
!!! <param name="op1">64-bit integer multiplier.</param>
!!! <param name="op2">64-bit integer multiplicand.</param>
!!! <param name="dest">Variable to store result.</param>
i64Mult        PROCEDURE (*INT64  op1, *INT64  op2, *INT64  dest),RAW,NAME('Cla$i64Mult')
i64Mult        PROCEDURE (*UINT64 op1, *UINT64 op2, *UINT64 dest),RAW,NAME('Cla$i64MultU')

!!! <summary>
!!! Divide value of 64-bit integer variable to 10 and store quotient back to
!!! variable. The remainder is stored to 32-bit integer variable. Value of
!!! remainder is positive always.
!!! </summary>
!!! <param name="v">64-bit integer variable.</param>
!!! <param name="rem">32-bit integer variable to store remainder.</param>
!!! <returns>0 if operation successful, other value in case of errors.</returns> 
i64Div10       PROCEDURE (*INT64  v, *LONG rem),UNSIGNED,PROC,RAW,NAME('Cla$i64Div10')
i64Div10       PROCEDURE (*UINT64 v, *LONG rem),UNSIGNED,PROC,RAW,NAME('Cla$i64Div10U')

!!! <summary>
!!! Store result of division of two 64-bit signed integer values to passed
!!! 64-bit variable. The same variable can be passed as one of operands and
!!! as a destination variable.
!!!
!!! Result corresponds to value of define(compatible_modulus) pragma set to "on"
!!! </summary>
!!! <param name="op1">64-bit integer dividend.</param>
!!! <param name="op2">64-bit integer divisor.</param>
!!! <param name="dest">Variable to store quotient.</param>
!!! <returns>0 if operation successful, other value in case of errors.</returns> 
i64Div         PROCEDURE (*INT64 op1, *INT64 op2, *INT64 dest),UNSIGNED,PROC,RAW,NAME('Cla$i64Div')

!!! <summary>
!!! Store result of division of two 64-bit unsigned integer values to passed
!!! 64-bit variable. The same variable can be passed as one of operands and
!!! as a destination variable.
!!! </summary>
!!! <param name="op1">64-bit integer dividend.</param>
!!! <param name="op2">64-bit integer divisor.</param>
!!! <param name="dest">Variable to store quotient.</param>
!!! <returns>0 if operation successful, other value in case of errors.</returns> 
i64Div         PROCEDURE (*UINT64 op1, *UINT64 op2, *UINT64 dest),UNSIGNED,PROC,RAW,NAME('Cla$i64DivU')

             END

