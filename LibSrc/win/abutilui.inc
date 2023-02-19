!!
!! ABUTILUI.INC - user interface structures and controls for popup calendar
!!
!! CAUTION - Do NOT rename or remove variables or screen controls
!!
!! Window structure for the Standard popup Calendar
!!
 SECTION('CalendarClassWindow')
Screen WINDOW,AT(,,109,116),CENTER,GRAY,SYSTEM,FONT('Arial',8,,FONT:bold)
        BOX,AT(0,0,109,14),USE(?BoxTitleBackground),COLOR(0FF8000H),FILL(0FF8000H), LINEWIDTH(1)
        BOX,AT(0,14,109,101),USE(?BoxBodyBackground),COLOR(COLOR:White), FILL(COLOR:White),LINEWIDTH(1)
        STRING(@s15),AT(28,2,52,9),USE(?MonthYear),TRN,CENTER,FONT('Microsoft Sans Serif',8,COLOR:White,FONT:bold)
        STRING('MMM'),AT(4,15,16,8),USE(?Sun),TRN,CENTER,FONT('Arial',7, COLOR:Navy,FONT:regular)
        STRING('MMM'),AT(21,15,10,8),USE(?Mon),TRN,CENTER,FONT('Arial',7, COLOR:Navy,FONT:regular)
        STRING('MMM'),AT(32,15,16,8),USE(?Tue),TRN,CENTER,FONT('Arial',7, COLOR:Navy,FONT:regular)
        STRING('MMM'),AT(48,15,11,8),USE(?Wed),TRN,CENTER,FONT('Arial',7, COLOR:Navy,FONT:regular)
        STRING('MMM'),AT(60,15,16,8),USE(?Thu),TRN,CENTER,FONT('Arial',7, COLOR:Navy,FONT:regular)
        STRING('MMM'),AT(74,15,16,8),USE(?Fri),TRN,CENTER,FONT('Arial',7, COLOR:Navy,FONT:regular)
        STRING('MMM'),AT(88,15,16,8),USE(?Sat),TRN,CENTER,FONT('Arial',7, COLOR:Navy,FONT:regular)
        LINE,AT(5,21,98,0),USE(?Line1),COLOR(COLOR:Black)
        BUTTON,AT(5,23,14,12),USE(?Day1),SKIP,FONT('Arial',9,COLOR:Black,,CHARSET:ANSI), FLAT
        BUTTON,AT(19,23,14,12),USE(?Day2),SKIP,FONT('Arial',9,COLOR:Black,, CHARSET:ANSI),FLAT
        BUTTON,AT(33,23,14,12),USE(?Day3),SKIP,FONT('Arial',9,COLOR:Black,, CHARSET:ANSI),FLAT
        BUTTON,AT(47,23,14,12),USE(?Day4),SKIP,FONT('Arial',9,COLOR:Black,, CHARSET:ANSI),FLAT
        BUTTON,AT(61,23,14,12),USE(?Day5),SKIP,FONT('Arial',9,COLOR:Black,, CHARSET:ANSI),FLAT
        BUTTON,AT(75,23,14,12),USE(?Day6),SKIP,FONT('Arial',9,COLOR:Black,, CHARSET:ANSI),FLAT
        BUTTON,AT(89,23,14,12),USE(?Day7),SKIP,FONT('Arial',9,COLOR:Black,, CHARSET:ANSI),FLAT
        BUTTON,AT(5,35,14,12),USE(?Day8),SKIP,FONT('Arial',9,COLOR:Black,,CHARSET:ANSI), FLAT
        BUTTON,AT(19,35,14,12),USE(?Day9),SKIP,FONT('Arial',9,COLOR:Black,, CHARSET:ANSI),FLAT
        BUTTON,AT(33,35,14,12),USE(?Day10),SKIP,FONT('Arial',9,COLOR:Black,, CHARSET:ANSI),FLAT
        BUTTON,AT(47,35,14,12),USE(?Day11),SKIP,FONT('Arial',9,COLOR:Black,, CHARSET:ANSI),FLAT
        BUTTON,AT(61,35,14,12),USE(?Day12),SKIP,FONT('Arial',9,COLOR:Black,, CHARSET:ANSI),FLAT
        BUTTON,AT(75,35,14,12),USE(?Day13),SKIP,FONT('Arial',9,COLOR:Black,, CHARSET:ANSI),FLAT
        BUTTON,AT(89,35,14,12),USE(?Day14),SKIP,FONT('Arial',9,COLOR:Black,, CHARSET:ANSI),FLAT
        BUTTON,AT(5,47,14,12),USE(?Day15),SKIP,FONT('Arial',9,COLOR:Black,, CHARSET:ANSI),FLAT
        BUTTON,AT(19,47,14,12),USE(?Day16),SKIP,FONT('Arial',9,COLOR:Black,, CHARSET:ANSI),FLAT
        BUTTON,AT(33,47,14,12),USE(?Day17),SKIP,FONT('Arial',9,COLOR:Black,, CHARSET:ANSI),FLAT
        BUTTON,AT(47,47,14,12),USE(?Day18),SKIP,FONT('Arial',9,COLOR:Black,, CHARSET:ANSI),FLAT
        BUTTON,AT(61,47,14,12),USE(?Day19),SKIP,FONT('Arial',9,COLOR:Black,, CHARSET:ANSI),FLAT
        BUTTON,AT(75,47,14,12),USE(?Day20),SKIP,FONT('Arial',9,COLOR:Black,, CHARSET:ANSI),FLAT
        BUTTON,AT(89,47,14,12),USE(?Day21),SKIP,FONT('Arial',9,COLOR:Black,, CHARSET:ANSI),FLAT
        BUTTON,AT(5,59,14,12),USE(?Day22),SKIP,FONT('Arial',9,COLOR:Black,, CHARSET:ANSI),FLAT
        BUTTON,AT(19,59,14,12),USE(?Day23),SKIP,FONT('Arial',9,COLOR:Black,, CHARSET:ANSI),FLAT
        BUTTON,AT(33,59,14,12),USE(?Day24),SKIP,FONT('Arial',9,COLOR:Black,, CHARSET:ANSI),FLAT
        BUTTON,AT(47,59,14,12),USE(?Day25),SKIP,FONT('Arial',9,COLOR:Black,, CHARSET:ANSI),FLAT
        BUTTON,AT(61,59,14,12),USE(?Day26),SKIP,FONT('Arial',9,COLOR:Black,, CHARSET:ANSI),FLAT
        BUTTON,AT(75,59,14,12),USE(?Day27),SKIP,FONT('Arial',9,COLOR:Black,, CHARSET:ANSI),FLAT
        BUTTON,AT(89,59,14,12),USE(?Day28),SKIP,FONT('Arial',9,COLOR:Black,, CHARSET:ANSI),FLAT
        BUTTON,AT(5,71,14,12),USE(?Day29),SKIP,FONT('Arial',9,COLOR:Black,, CHARSET:ANSI),FLAT
        BUTTON,AT(19,71,14,12),USE(?Day30),SKIP,FONT('Arial',9,COLOR:Black,, CHARSET:ANSI),FLAT
        BUTTON,AT(33,71,14,12),USE(?Day31),SKIP,FONT('Arial',9,COLOR:Black,, CHARSET:ANSI),FLAT
        BUTTON,AT(47,71,14,12),USE(?Day32),SKIP,FONT('Arial',9,COLOR:Black,, CHARSET:ANSI),FLAT
        BUTTON,AT(61,71,14,12),USE(?Day33),SKIP,FONT('Arial',9,COLOR:Black,, CHARSET:ANSI),FLAT
        BUTTON,AT(75,71,14,12),USE(?Day34),SKIP,FONT('Arial',9,COLOR:Black,, CHARSET:ANSI),FLAT
        BUTTON,AT(89,71,14,12),USE(?Day35),SKIP,FONT('Arial',9,COLOR:Black,, CHARSET:ANSI),FLAT
        BUTTON,AT(5,83,14,12),USE(?Day36),SKIP,FONT('Arial',9,COLOR:Black,, CHARSET:ANSI),FLAT
        BUTTON,AT(19,83,14,12),USE(?Day37),SKIP,FONT('Arial',9,COLOR:Black,, CHARSET:ANSI),FLAT
        BUTTON,AT(33,83,14,12),USE(?Day38),SKIP,FONT('Arial',9,COLOR:Black,, CHARSET:ANSI),FLAT
        BUTTON,AT(47,83,14,12),USE(?Day39),SKIP,FONT('Arial',9,COLOR:Black,, CHARSET:ANSI),FLAT
        BUTTON,AT(61,83,14,12),USE(?Day40),SKIP,FONT('Arial',9,COLOR:Black,, CHARSET:ANSI),FLAT
        BUTTON,AT(75,83,14,12),USE(?Day41),SKIP,FONT('Arial',9,COLOR:Black,, CHARSET:ANSI),FLAT
        BUTTON,AT(89,83,14,12),USE(?Day42),SKIP,FONT('Arial',9,COLOR:Black,, CHARSET:ANSI),FLAT
        LINE,AT(5,98,98,0),USE(?Line2),COLOR(COLOR:Black)
        BUTTON('&Today'),AT(5,101,30,13),USE(?Today),FONT('Microsoft Sans Serif', 8,COLOR:Black,FONT:regular,CHARSET:ANSI),FLAT
        BUTTON('&Close'),AT(73,101,30,13),USE(?Exit),FONT('Microsoft Sans Serif', 8,COLOR:Black,FONT:regular,CHARSET:ANSI),FLAT
        BUTTON,AT(1,2,9,9),USE(?PrevYear),ICON('VCRFIRST.ICO'),IMM,TIP('Previous Year'), FLAT,DELAY(50)
        BUTTON,AT(10,2,9,9),USE(?PrevMonth),ICON('VCRPRIOR.ICO'),IMM,TIP('Previous Month'),FLAT,DELAY(50)
        BUTTON,AT(19,2,9,9),USE(?Less7),ICON('VCRUP.ICO'),IMM,TIP('Previous Week'), FLAT,DELAY(50)
        BUTTON,AT(80,2,9,9),USE(?More7),ICON('VCRDOWN.ICO'),IMM,TIP('Next Week'),FLAT, DELAY(50)
        BUTTON,AT(89,2,9,9),USE(?NextMonth),ICON('VCRNEXT.ICO'),IMM,TIP('Next Month'), FLAT,DELAY(50)
        BUTTON,AT(98,2,9,9),USE(?NextYear),ICON('VCRLAST.ICO'),IMM,TIP('Next Year'), FLAT,DELAY(50)
    END
!!
!! Window structure for the Small popup Calendar
!!
 SECTION('CalendarSmallClassWindow')
Screen WINDOW('Calendar'),AT(,,208,136),CENTER,GRAY,SYSTEM,FONT('Arial',8,,FONT:bold), |
            TIMER(100),DOUBLE
        PANEL,AT(1,0,207,136),USE(?BoxBodyBackground),FILL(COLOR:Gray),BEVEL(-1,1)
        STRING(@t6),AT(41,5),USE(?CurrTime),TRN,FONT('Arial',8,COLOR:White,, CHARSET:ANSI)
        STRING(@s15),AT(9,16,105,15),USE(?MonthYear),TRN,CENTER,FONT('Microsoft Sans Serif',12,COLOR:Lime,FONT:bold)
        STRING(@D2),AT(130,16,59,15),USE(?TheDate),TRN,RIGHT,FONT('Microsoft Sans Serif',12,COLOR:Lime,FONT:bold,CHARSET:ANSI)
        STRING('MMM'),AT(5,34,16,10),USE(?Sun),TRN,CENTER,FONT('Arial',7, COLOR:White,FONT:regular)
        STRING('MMM'),AT(21,34,16,10),USE(?Mon),TRN,CENTER,FONT('Arial',7, COLOR:White,FONT:regular)
        STRING('MMM'),AT(37,34,16,10),USE(?Tue),TRN,CENTER,FONT('Arial',7, COLOR:White,FONT:regular)
        STRING('MMM'),AT(53,34,16,10),USE(?Wed),TRN,CENTER,FONT('Arial',7, COLOR:White,FONT:regular)
        STRING('MMM'),AT(69,34,16,10),USE(?Thu),TRN,CENTER,FONT('Arial',7, COLOR:White,FONT:regular)
        STRING('MMM'),AT(85,34,16,10),USE(?Fri),TRN,CENTER,FONT('Arial',7, COLOR:White,FONT:regular)
        STRING('MMM'),AT(101,34,16,10),USE(?Sat),TRN,CENTER,FONT('Arial',7, COLOR:White,FONT:regular)
        BUTTON,AT(5,46,16,14),USE(?Day1),SKIP,FONT('Arial',9,COLOR:Black,,CHARSET:ANSI)
        BUTTON,AT(21,46,16,14),USE(?Day2),SKIP,FONT('Arial',9,COLOR:Black,, CHARSET:ANSI)
        BUTTON,AT(37,46,16,14),USE(?Day3),SKIP,FONT('Arial',9,COLOR:Black,, CHARSET:ANSI)
        BUTTON,AT(53,46,16,14),USE(?Day4),SKIP,FONT('Arial',9,COLOR:Black,, CHARSET:ANSI)
        BUTTON,AT(69,46,16,14),USE(?Day5),SKIP,FONT('Arial',9,COLOR:Black,, CHARSET:ANSI)
        BUTTON,AT(85,46,16,14),USE(?Day6),SKIP,FONT('Arial',9,COLOR:Black,, CHARSET:ANSI)
        BUTTON,AT(101,46,16,14),USE(?Day7),SKIP,FONT('Arial',9,COLOR:Black,, CHARSET:ANSI)
        BUTTON,AT(5,60,16,14),USE(?Day8),SKIP,FONT('Arial',9,COLOR:Black,,CHARSET:ANSI)
        BUTTON,AT(21,60,16,14),USE(?Day9),SKIP,FONT('Arial',9,COLOR:Black,, CHARSET:ANSI)
        BUTTON,AT(37,60,16,14),USE(?Day10),SKIP,FONT('Arial',9,COLOR:Black,, CHARSET:ANSI)
        BUTTON,AT(53,60,16,14),USE(?Day11),SKIP,FONT('Arial',9,COLOR:Black,, CHARSET:ANSI)
        BUTTON,AT(69,60,16,14),USE(?Day12),SKIP,FONT('Arial',9,COLOR:Black,, CHARSET:ANSI)
        BUTTON,AT(85,60,16,14),USE(?Day13),SKIP,FONT('Arial',9,COLOR:Black,, CHARSET:ANSI)
        BUTTON,AT(101,60,16,14),USE(?Day14),SKIP,FONT('Arial',9,COLOR:Black,, CHARSET:ANSI)
        BUTTON,AT(5,74,16,14),USE(?Day15),SKIP,FONT('Arial',9,COLOR:Black,, CHARSET:ANSI)
        BUTTON,AT(21,74,16,14),USE(?Day16),SKIP,FONT('Arial',9,COLOR:Black,, CHARSET:ANSI)
        BUTTON,AT(37,74,16,14),USE(?Day17),SKIP,FONT('Arial',9,COLOR:Black,, CHARSET:ANSI)
        BUTTON,AT(53,74,16,14),USE(?Day18),SKIP,FONT('Arial',9,COLOR:Black,, CHARSET:ANSI)
        BUTTON,AT(69,74,16,14),USE(?Day19),SKIP,FONT('Arial',9,COLOR:Black,, CHARSET:ANSI)
        BUTTON,AT(85,74,16,14),USE(?Day20),SKIP,FONT('Arial',9,COLOR:Black,, CHARSET:ANSI)
        BUTTON,AT(101,74,16,14),USE(?Day21),SKIP,FONT('Arial',9,COLOR:Black,, CHARSET:ANSI)
        BUTTON,AT(5,88,16,14),USE(?Day22),SKIP,FONT('Arial',9,COLOR:Black,, CHARSET:ANSI)
        BUTTON,AT(21,88,16,14),USE(?Day23),SKIP,FONT('Arial',9,COLOR:Black,, CHARSET:ANSI)
        BUTTON,AT(37,88,16,14),USE(?Day24),SKIP,FONT('Arial',9,COLOR:Black,, CHARSET:ANSI)
        BUTTON,AT(53,88,16,14),USE(?Day25),SKIP,FONT('Arial',9,COLOR:Black,, CHARSET:ANSI)
        BUTTON,AT(69,88,16,14),USE(?Day26),SKIP,FONT('Arial',9,COLOR:Black,, CHARSET:ANSI)
        BUTTON,AT(85,88,16,14),USE(?Day27),SKIP,FONT('Arial',9,COLOR:Black,, CHARSET:ANSI)
        BUTTON,AT(101,88,16,14),USE(?Day28),SKIP,FONT('Arial',9,COLOR:Black,, CHARSET:ANSI)
        BUTTON,AT(5,102,16,14),USE(?Day29),SKIP,FONT('Arial',9,COLOR:Black,, CHARSET:ANSI)
        BUTTON,AT(21,102,16,14),USE(?Day30),SKIP,FONT('Arial',9,COLOR:Black,, CHARSET:ANSI)
        BUTTON,AT(37,102,16,14),USE(?Day31),SKIP,FONT('Arial',9,COLOR:Black,, CHARSET:ANSI)
        BUTTON,AT(53,102,16,14),USE(?Day32),SKIP,FONT('Arial',9,COLOR:Black,, CHARSET:ANSI)
        BUTTON,AT(69,102,16,14),USE(?Day33),SKIP,FONT('Arial',9,COLOR:Black,, CHARSET:ANSI)
        BUTTON,AT(85,102,16,14),USE(?Day34),SKIP,FONT('Arial',9,COLOR:Black,, CHARSET:ANSI)
        BUTTON,AT(101,102,16,14),USE(?Day35),SKIP,FONT('Arial',9,COLOR:Black,, CHARSET:ANSI)
        BUTTON,AT(5,116,16,14),USE(?Day36),SKIP,FONT('Arial',9,COLOR:Black,, CHARSET:ANSI)
        BUTTON,AT(21,116,16,14),USE(?Day37),SKIP,FONT('Arial',9,COLOR:Black,, CHARSET:ANSI)
        BUTTON,AT(37,116,16,14),USE(?Day38),SKIP,FONT('Arial',9,COLOR:Black,, CHARSET:ANSI)
        BUTTON,AT(53,116,16,14),USE(?Day39),SKIP,FONT('Arial',9,COLOR:Black,, CHARSET:ANSI)
        BUTTON,AT(69,116,16,14),USE(?Day40),SKIP,FONT('Arial',9,COLOR:Black,, CHARSET:ANSI)
        BUTTON,AT(85,116,16,14),USE(?Day41),SKIP,FONT('Arial',9,COLOR:Black,, CHARSET:ANSI)
        BUTTON,AT(101,116,16,14),USE(?Day42),SKIP,FONT('Arial',9,COLOR:Black,, CHARSET:ANSI)
        BUTTON('&Today'),AT(123,46,82,14),USE(?Today),FONT('Microsoft Sans Serif',8,,FONT:bold)
        BUTTON('- Month'),AT(123,60,41,14),USE(?PrevMonth),FONT('Microsoft Sans Serif',8,,FONT:bold)
        BUTTON('+ &Month'),AT(123,74,41,14),USE(?NextMonth),FONT('Microsoft Sans Serif',8,,FONT:bold)
        BUTTON('- Year'),AT(123,88,41,14),USE(?PrevYear),FONT('Microsoft Sans Serif',8,,FONT:bold)
        BUTTON('+ &Year'),AT(123,102,41,14),USE(?NextYear),FONT('Microsoft Sans Serif',8,,FONT:bold)
        BUTTON('- 7 Day'),AT(164,60,41,14),USE(?Less7),FONT('Microsoft Sans Serif',8,,FONT:bold)
        BUTTON('+ 7 &Day'),AT(164,74,41,14),USE(?More7),FONT('Microsoft Sans Serif',8,,FONT:bold)
        BUTTON('- 15 Days'),AT(164,88,41,14),USE(?Less15),FONT('Microsoft Sans Serif',8,,FONT:bold)
        BUTTON('+ 15 Days'),AT(164,102,41,14),USE(?More15),FONT('Microsoft Sans Serif',8,,FONT:bold)
        BUTTON('&Close'),AT(123,116,82,14),USE(?Exit),LEFT,FONT('Microsoft Sans Serif',8,,FONT:bold)
    END
!!
!! Window structure for the Small popup Calendar Touch Friendly
!!
 SECTION('CalendarSmallTouchClassWindow')
Screen WINDOW,AT(,,130,120),CENTER,GRAY,IMM,SYSTEM,FONT('Arial',8,,FONT:bold), |
            TIMER(100),NOFRAME
        PANEL,AT(0,0,128,20),USE(?BoxBodyBackgroundTop),FILL(COLOR:Gray)
        PANEL,AT(0,20,128,75),USE(?BoxBodyBackgroundMid),FILL(COLOR:Gray)
        PANEL,AT(0,95,128,24),USE(?BoxBodyBackgroundBottom),FILL(COLOR:Gray)
        REGION,AT(2,48,126,20),USE(?PANELReadOnlyButtons)
        BUTTON,AT(4,3,16,16),USE(?Today),SKIP,FONT('Microsoft Sans Serif',16,, |
                FONT:bold),ICON('MiniCal.ico'),FLAT,TRN
        BUTTON('Mon'),AT(4,48,38,20),USE(?MonthText),SKIP,FONT('Microsoft Sans S' & |
                'erif',16,,FONT:bold)
        BUTTON('Day'),AT(46,48,38,20),USE(?DayText),SKIP,FONT('Microsoft Sans Se' & |
                'rif',16,,FONT:bold)
        BUTTON('Year'),AT(86,48,38,20),USE(?YearText),SKIP,FONT('Microsoft Sans ' & |
                'Serif',16,,FONT:bold)
        PROMPT('Monday'),AT(19,2,105,12),USE(?TheDateDOW),TRN,FONT('Segoe UI',9, |
                COLOR:White,FONT:regular,CHARSET:ANSI),RIGHT
        STRING(@D18),AT(19,10,105,12),USE(?TheDate),TRN,RIGHT,FONT('Segoe UI',9, |
                COLOR:White,FONT:regular,CHARSET:ANSI)
        BUTTON('-'),AT(4,71,38,20),USE(?PrevMonth),FONT('Microsoft Sans Serif', |
                18,,FONT:bold),IMM,DELAY(2)
        BUTTON('+'),AT(4,25,38,20),USE(?NextMonth),FONT('Microsoft Sans Serif', |
                18,,FONT:bold),IMM,DELAY(2)
        BUTTON('-'),AT(46,71,38,20),USE(?Less1),FONT('Microsoft Sans Serif',18,, |
                FONT:bold),IMM,DELAY(2)
        BUTTON('+'),AT(46,25,38,20),USE(?More1),FONT('Microsoft Sans Serif',18,, |
                FONT:bold),IMM,DELAY(2)
        BUTTON('-'),AT(86,71,38,20),USE(?PrevYear),FONT('Microsoft Sans Serif', |
                18,,FONT:bold),IMM,DELAY(2)
        BUTTON('+'),AT(86,25,38,20),USE(?NextYear),FONT('Microsoft Sans Serif', |
                18,,FONT:bold),IMM
        BUTTON('&Set'),AT(6,100,58,14),USE(?Exit),FONT('Microsoft Sans Serif',8, |
                ,FONT:bold)
        BUTTON('&Cancel'),AT(65,100,58,14),USE(?Cancel)
    END
!!
!! Window structure for the Small popup Calendar Touch Friendly
!!
 SECTION('CalendarSmallTouchIconsClassWindow')
Screen WINDOW,AT(,,128,124),CENTER,SYSTEM,FONT('Arial',8,,FONT:bold),TIMER(100), |
            NOFRAME
        PANEL,AT(0,0,128,20),USE(?BoxBodyBackgroundTop),FILL(COLOR:Gray)
        PANEL,AT(0,20,128,75),USE(?BoxBodyBackgroundMid),FILL(COLOR:Gray)
        PANEL,AT(0,95,128,29),USE(?BoxBodyBackgroundBottom),FILL(COLOR:Gray)
        REGION,AT(2,48,126,20),USE(?PANELReadOnlyButtons)
        BUTTON,AT(4,3,16,16),USE(?Today),SKIP,FONT('Microsoft Sans Serif',16,, |
                FONT:bold),ICON('MiniCal.ico'),FLAT,TRN
        PROMPT('Monday'),AT(19,2,105,12),USE(?TheDateDOW),TRN,FONT('Segoe UI',9, |
                COLOR:White,FONT:regular,CHARSET:ANSI),RIGHT
        STRING(@D18),AT(19,10,105,12),USE(?TheDate),TRN,RIGHT,FONT('Segoe UI',9, |
                COLOR:White,FONT:regular,CHARSET:ANSI)
        BUTTON('Mon'),AT(4,48,38,20),USE(?MonthText),SKIP,FONT('Segoe UI Light', |
                16,,FONT:regular)
        BUTTON('Day'),AT(46,48,38,20),USE(?DayText),SKIP,FONT('Segoe UI Light', |
                16,,FONT:regular)
        BUTTON('Year'),AT(86,48,38,20),USE(?YearText),SKIP,FONT('Segoe UI Light', |
                16,,FONT:regular)
        BUTTON('+'),AT(4,25,38,20),USE(?NextMonth),FONT('Segoe UI Light',18,,FONT:bold), |
                IMM,DELAY(2)
        BUTTON('-'),AT(4,71,38,20),USE(?PrevMonth),FONT('Segoe UI Light',18,,FONT:bold), |
                IMM,DELAY(2)
        BUTTON('+'),AT(46,25,38,20),USE(?More1),FONT('Segoe UI Light',18,,FONT:bold), |
                IMM,DELAY(2)
        BUTTON('-'),AT(46,71,38,20),USE(?Less1),FONT('Segoe UI Light',18,,FONT:bold), |
                IMM,DELAY(2)
        BUTTON('+'),AT(86,25,38,20),USE(?NextYear),FONT('Segoe UI Light',18,,FONT:bold), |
                IMM,DELAY(2)
        BUTTON('-'),AT(86,71,38,20),USE(?PrevYear),FONT('Segoe UI Light',18,,FONT:bold), |
                IMM,DELAY(2)
        BUTTON,AT(25,98,30,24),USE(?Exit),FONT('Microsoft Sans Serif',8,,FONT:bold), |
                ICON('MiniOk.ico'),FLAT,TRN
        BUTTON,AT(74,98,30,24),USE(?Cancel),ICON('MiniCancel.ico'),FLAT,TRN
    END
!!
!! Window structure for the Small popup Calendar Touch Friendly
!!
 SECTION('CalendarSmallTouchRegionsClassWindow')
Screen WINDOW,AT(,,130,124),CENTER,SYSTEM,FONT('Arial',8,,FONT:bold),TIMER(100), |
            NOFRAME
        PANEL,AT(0,0,130,20),USE(?BoxBodyBackgroundTop),FILL(COLOR:Gray)
        PANEL,AT(0,20,130,75),USE(?BoxBodyBackgroundMid),FILL(COLOR:Gray)
        PANEL,AT(0,95,130,29),USE(?BoxBodyBackgroundBottom),FILL(COLOR:Gray)
        BUTTON,AT(4,3,16,16),USE(?Today),SKIP,FONT('Microsoft Sans Serif',16,, |
                FONT:bold),ICON('MiniCal.ico'),FLAT,TRN
        PROMPT('Monday'),AT(20,2,105,12),USE(?TheDateDOW),TRN,FONT('Segoe UI',9, |
                COLOR:White,FONT:regular,CHARSET:ANSI),RIGHT
        STRING(@D18),AT(20,10,105,12),USE(?TheDate),TRN,RIGHT,FONT('Segoe UI',9, |
                COLOR:White,FONT:regular,CHARSET:ANSI)
        BUTTON,AT(26,98,30,24),USE(?Exit),SKIP,FONT('Microsoft Sans Serif',8,, |
                FONT:bold),ICON('MiniOk.ico'),FLAT,TRN
        BUTTON,AT(74,98,30,24),USE(?Cancel),SKIP,ICON('MiniCancel.ico'),FLAT,TRN
        BOX,AT(7,26,38,36),USE(?BOXMonthTop1),FILL(0E0E0E0H),ROUND,LINEWIDTH(0)
        BOX,AT(7,56,38,37),USE(?BOXMonthMid1),FILL(0E0E0E0H),ROUND,LINEWIDTH(0)
        BOX,AT(47,26,38,36),USE(?BOXMonthTop2),FILL(0E0E0E0H),ROUND,LINEWIDTH(0)
        BOX,AT(47,56,38,37),USE(?BOXMonthMid2),FILL(0E0E0E0H),ROUND,LINEWIDTH(0)
        BOX,AT(87,26,38,36),USE(?BOXMonthTop3),FILL(0E0E0E0H),ROUND,LINEWIDTH(0)
        BOX,AT(87,56,38,37),USE(?BOXMonthMid3),FILL(0E0E0E0H),ROUND,LINEWIDTH(0)
        REGION,AT(7,26,38,27),USE(?NextMonthPanel)
        REGION,AT(47,26,38,27),USE(?More1Panel)
        REGION,AT(87,26,38,27),USE(?NextYearPanel)
        REGION,AT(87,64,38,27),USE(?PrevYearPanel)
        REGION,AT(47,64,38,27),USE(?Less1Panel)
        REGION,AT(7,64,38,27),USE(?PrevMonthPanel)
        PROMPT('+'),AT(20,24),USE(?PROMPT1),TRN,FONT('Segoe UI Light',20)
        PROMPT('+'),AT(60,24),USE(?PROMPT1:2),TRN,FONT('Segoe UI Light',20)
        PROMPT('+'),AT(100,24),USE(?PROMPT1:3),TRN,FONT('Segoe UI Light',20)
        PROMPT('-'),AT(102,68),USE(?PROMPT1:4),TRN,FONT('Segoe UI Light',20)
        PROMPT('-'),AT(62,68),USE(?PROMPT1:5),TRN,FONT('Segoe UI Light',20)
        PROMPT('-'),AT(22,68),USE(?PROMPT1:6),TRN,FONT('Segoe UI Light',20)
        PROMPT('2012'),AT(87,46,38),USE(?YearText:2),TRN,FONT('Segoe UI Light',20), |
                CENTER
        PROMPT('31'),AT(47,46,38),USE(?DayText:2),TRN,FONT('Segoe UI Light',20),CENTER
        PROMPT('Jan'),AT(7,46,38),USE(?MonthText:2),TRN,FONT('Segoe UI Light',20), |
                CENTER
        BOX,AT(7,26,38,66),USE(?BOXBorde1),COLOR(COLOR:White),ROUND,LINEWIDTH(2)
        BOX,AT(47,26,38,66),USE(?BOXBorde2),COLOR(COLOR:White),ROUND,LINEWIDTH(2)
        BOX,AT(87,26,38,66),USE(?BOXBorde3),COLOR(COLOR:White),ROUND,LINEWIDTH(2)
    END
!!
!! Day Group declaration with Day labels
!!
 SECTION('CalendarDayGroup')
_ClarionDayOfTheWeekListShort            GROUP,STATIC
d2                   STRING('Mon')
d3                   STRING('Tue')
d4                   STRING('Wed')
d5                   STRING('Thu')
d6                   STRING('Fri')
d7                   STRING('Sat')
d1                   STRING('Sun')
                     END
ClarionDayOfTheWeekShort         STRING(3),DIM(7),OVER(_ClarionDayOfTheWeekListShort)

_ClarionDayOfTheWeekListLong GROUP,STATIC
dd2                  STRING('Monday         ')
dd3                  STRING('Tuesday        ')
dd4                  STRING('Wednesday      ')
dd5                  STRING('Thursday       ')
dd6                  STRING('Friday         ')
dd7                  STRING('Saturday       ')
dd1                  STRING('Sunday         ')
 END

ClarionDayOfTheWeekLong          STRING(15),DIM(7),OVER(_ClarionDayOfTheWeekListLong)

!!
!! Month Group declaration with Day labels
!!
 SECTION('CalendarMonthGroup')
_ClarionMonthListLong          GROUP,STATIC
mm1                   STRING('January   ')
mm2                   STRING('February  ')
mm3                   STRING('March     ')
mm4                   STRING('April     ')
mm5                   STRING('May       ')
mm6                   STRING('June      ')
mm7                   STRING('July      ')
mm8                   STRING('August    ')
mm9                   STRING('September ')
mm10                  STRING('October   ')
mm11                  STRING('November  ')
mm12                  STRING('December  ')
                     END
ClarionMonthListLong          STRING(10),DIM(12),OVER(_ClarionMonthListLong)
_ClarionMonthListShort        GROUP,STATIC
m1                   STRING('Jan')
m2                   STRING('Feb')
m3                   STRING('Mar')
m4                   STRING('Apr')
m5                   STRING('May')
m6                   STRING('Jun')
m7                   STRING('Jul')
m8                   STRING('Aug')
m9                   STRING('Sep')
m10                  STRING('Oct')
m11                  STRING('Nov')
m12                  STRING('Dec')
                     END
ClarionMonthListShort        STRING(3),DIM(12),OVER(_ClarionMonthListShort)
