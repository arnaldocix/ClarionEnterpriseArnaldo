  MEMBER
  INCLUDE ('ERRORS.CLW')

DELETE                    EQUATE(00010000h)
READ_CONTROL              EQUATE(00020000h)
WRITE_DAC                 EQUATE(00040000h)
WRITE_OWNER               EQUATE(00080000h)
SYNCHRONIZE               EQUATE(00100000h)

STANDARD_RIGHTS_REQUIRED  EQUATE(000F0000h)

STANDARD_RIGHTS_READ      EQUATE(READ_CONTROL)
STANDARD_RIGHTS_WRITE     EQUATE(READ_CONTROL)
STANDARD_RIGHTS_EXECUTE   EQUATE(READ_CONTROL)

STANDARD_RIGHTS_ALL       EQUATE(001F0000h)

SPECIFIC_RIGHTS_ALL       EQUATE(0000FFFFh)

!! Equates for API

GENERIC_READ              EQUATE(80000000h)
GENERIC_WRITE             EQUATE(40000000h)
GENERIC_EXECUTE           EQUATE(20000000h)
GENERIC_ALL               EQUATE(10000000h)

FILE_ATTRIBUTE_READONLY   EQUATE(00000001h)
FILE_ATTRIBUTE_HIDDEN     EQUATE(00000002h)
FILE_ATTRIBUTE_SYSTEM     EQUATE(00000004h)
FILE_ATTRIBUTE_DIRECTORY  EQUATE(00000010h)
FILE_ATTRIBUTE_ARCHIVE    EQUATE(00000020h)
FILE_ATTRIBUTE_NORMAL     EQUATE(00000080h)
FILE_ATTRIBUTE_TEMPORARY  EQUATE(00000100h)

CREATE_NEW                EQUATE(1)
CREATE_ALWAYS             EQUATE(2)
OPEN_EXISTING             EQUATE(3)
OPEN_ALWAYS               EQUATE(4)
TRUNCATE_EXISTING         EQUATE(5)

SHARE_NONE                EQUATE(0)
INVALID_LONG_VALUE        EQUATE(4294967295)
INVALID_LONG              EQUATE( -1 )
SM_REMOTESESSION          EQUATE(01000h)
SM_MAXIMUMTOUCHES         EQUATE(95)


!! Type DEFS for API

  !! File Stuff
FILETIME                        GROUP,TYPE
dwLowDateTime                     ULONG
dwHighDateTime                    ULONG
                                END

WIN32_FIND_DATA                 GROUP,TYPE
dwFileAttributes                  ULONG
ftCreationTime                    GROUP(FILETIME).
ftLastAccessTime                  GROUP(FILETIME).
ftLastWriteTime                   GROUP(FILETIME).
nFileSizeHigh                     ULONG
nFileSizeLow                      ULONG
dwReserved0                       ULONG
dwReserved1                       ULONG
cFileName                         CSTRING( FILE:MaxFilePath )
cAlternateFileName                CSTRING( 14 )
                                END

SYSTEMTIME                      GROUP,TYPE
wYear                             USHORT
wMonth                            USHORT
wDayOfWeek                        USHORT
wDay                              USHORT
wHour                             USHORT
wMinute                           USHORT
wSecond                           USHORT
wMilliseconds                     USHORT
                                END

SPI_GETDRAGFULLWINDOWS          EQUATE(38)
SPI_SETDRAGFULLWINDOWS          EQUATE(37)
SPIF_UPDATEINIFILE              EQUATE(1)
SPIF_SENDWININICHANGE           EQUATE(2)

HexDigitsUp                     STRING('0123456789ABCDEF'),PRIVATE
HexDigitsLow                    STRING('0123456789abcdef'),PRIVATE


  MAP
    MODULE('C Library')
      mkdir (*CSTRING),SIGNED,PROC,RAW,NAME('_mkdir')
      rmdir (*CSTRING),SIGNED,PROC,RAW,NAME('_rmdir')
    END
    MODULE('win32')
      FindFirstFile( *CSTRING lpFileName, *WIN32_FIND_DATA ),LONG,RAW,PASCAL,NAME('FindFirstFileA')
      FindClose( LONG hFindFile ),BOOL,RAW,PASCAL,PROC
      GetLastError(),ULONG,PASCAL,RAW
      FindWindow( <*CSTRING>, *CSTRING ),UNSIGNED,PASCAL,RAW,NAME( 'FindWindowA' )
      FileTimeToSystemTime(*FILETIME lpFileTime, *SYSTEMTIME lpSystemTime),BOOL,RAW,PASCAL,PROC
      FileTimeToLocalFileTime(*FILETIME lpFileTime, *FILETIME lpLocalFileTime),BOOL,RAW,PASCAL,PROC
      CreateFile( *CSTRING lpFileName, ULONG dwDesiredAccess, ULONG dwShareMode, LONG lpSecurityAttributes, ULONG dwCreationDisposition, ULONG dwFlagsAndAttributes, LONG hTemplateFile), LONG,RAW,PASCAL,NAME('CreateFileA')
      WriteFile( LONG hFile, *STRING lpBuffer, ULONG nNumberOfBytesToWrite, *ULONG lpNumberOfBytesWritten, LONG lpOverlapped ),BOOL,RAW,PASCAL
      CloseHandle( LONG hObject ),BOOL,RAW,PASCAL,PROC
      WNetGetUser( <*CSTRING lpName>, *CSTRING lpUserName, *ULONG lpnLength ),ULONG,RAW,PASCAL,PROC,NAME('WNetGetUserA')
      CreateEvent( <*?>, BOOL bManualReset, BOOL bInitialState, *CSTRING lpName ),LONG,RAW,PASCAL,NAME('CreateEventA')
      GetSystemMetrics( SIGNED nIndex ),SIGNED,RAW,PASCAL
      GetTempPath( LONG nBufferLength, *? lpBuffer ),LONG,RAW,PASCAL,NAME('GetTempPathA')
      GetTempFileName( *CSTRING lpPathName, *CSTRING lpPrefixString, UNSIGNED uUnique, *CSTRING lpTempFileName ),UNSIGNED,RAW,PASCAL,NAME('GetTempFileNameA')

      SystemParametersInfo( LONG uAction, LONG uParam, *? lpvParam, LONG fuWinIni ), LONG, RAW, PASCAL, DLL(TRUE), NAME('SystemParametersInfoA'),PROC
      GetSysColor(signed nIndex),long,pascal,NAME('GetSysColor')
    END
    INCLUDE('CWUTIL.INC'),ONCE

    GetFileSystemTimeDate (STRING FileName, BYTE which, *SYSTEMTIME systime),BOOL,PRIVATE
  END


!-----------------------------------------------------------------------------!
!OSVersion
! Return the Current OS Version (Windows 95, Windows 98 or Windows NT)
!-----------------------------------------------------------------------------!

OSVersion  PROCEDURE()
  CODE
  RETURN SYSTEM {PROP:WindowsVersion}

!-----------------------------------------------------------------------------!
!FileExists
! Return wether a file exists or not (TRUE = Exists, FALSE = Does NOT)
!-----------------------------------------------------------------------------!

FileExists         PROCEDURE ( STRING sFile )

bRetVal            BYTE,AUTO
hFindHandle        SIGNED,AUTO
szFindFile         CSTRING(FILE:MaxFileName),AUTO
gFileFind          LIKE (WIN32_FIND_DATA),AUTO

  CODE
  hFindHandle = 0
  bRetVal     = FALSE
  szFindFile  = CLIP (sFile)
  CLEAR (gFileFind)

  hFindHandle = FindFirstFile (szFindFile, gFileFind)
  bRetVal = CHOOSE (NOT (hFindHandle = INVALID_LONG_VALUE OR hFindHandle <= 0))

  FindClose (hFindHandle)
  RETURN bRetVal

!-----------------------------------------------------------------------------!
!WindowExists
! Return wether a window exists or not (TRUE = Exists, FALSE = Does NOT)
!-----------------------------------------------------------------------------!

WindowExists       PROCEDURE (STRING sWindow)

hWnd               UNSIGNED,AUTO
szWindowName       CSTRING (128),AUTO

  CODE
  szWindowName = CLIP (sWindow)
  hWnd = FindWindow (,szWindowName)
  RETURN CHOOSE (hWnd <> 0)

!-----------------------------------------------------------------------------!
!ValidateOLE
! Validate that the OLE Control Created
!-----------------------------------------------------------------------------!
ValidateOLE        PROCEDURE( SIGNED OleControl, <STRING OleFileName>, <STRING OleCreateName> )

lCallRet           LONG

  CODE
  IF NOT OleControl{PROP:OLE} AND OleControl{PROP:Object} = ''
    IF ~OMITTED( 2 )
      OleControl{PROP:Create} = CLIP( OleCreateName )
      IF OleControl{PROP:OLE} = FALSE AND OleControl{PROP:Object} = ''
        IF ~OMITTED( 3 )
          lCallRet = CALL( CLIP( OleFileName ),'DllRegisterServer' )
          IF lCallRet
            MESSAGE('Can not Register OLE Control ' & CLIP( OleFileName ) & ' Error: ' & CLIP( lCallRet ) & '.  Please check the installation of the control.','Fatal OLE Error',ICON:Exclamation)
            RETURN FALSE
          END
          UNLOAD( CLIP( OleFileName ) )
          OleControl{PROP:Create} = CLIP( OleCreateName )
        ELSE
          RETURN FALSE
        END
        IF NOT OleControl{PROP:OLE} AND OleControl{PROP:Object} = ''
          MESSAGE('Can not Find OLE Control ' & CLIP( OleFileName ) & ' !  Please re-install.','Fatal OLE Error',ICON:Exclamation)
          RETURN FALSE
        ELSE
          RETURN TRUE
        END
      END
    ELSE
      RETURN FALSE
    END
  ELSE
    RETURN TRUE
  END

!-----------------------------------------------------------------------------!
!GetUserName
! Return the network username
!-----------------------------------------------------------------------------!
GetUserName        PROCEDURE()

sRetUser           CSTRING(128)
dwUserLen          ULONG,AUTO

  CODE
  dwUserLen = SIZE ( sRetUser )
  IF WNetGetUser (, sRetUser, dwUserLen ) = 0
    RETURN CLIP (sRetUser)
  ELSE
    RETURN ''
  END

!-----------------------------------------------------------------------------!
!BeginUnique
! Set APP to run in Single Process
!-----------------------------------------------------------------------------!
BeginUnique        PROCEDURE( STRING sAppName )

szEventName        CSTRING(FILE:MaxFilePath),AUTO
hEvent             LONG,AUTO

  CODE
  szEventName = CLIP( sAppName ) & '_UEvent'

  hEvent = CreateEvent (, 0, 0, szEventName )
  IF hEvent = 0 OR GetLastError() = 183             !Already Exists
    RETURN 0
  ELSE
    RETURN hEvent
  END

!-----------------------------------------------------------------------------!
!EndUnique
! Un Set APP to run in Single Process
!-----------------------------------------------------------------------------!
EndUnique          PROCEDURE( LONG hUnique )

  CODE
  CloseHandle ( hUnique )
  RETURN


!-----------------------------------------------------------------------------!
!IsTermServer
! Is this application running on terminal server
!-----------------------------------------------------------------------------!
IsTermServer       PROCEDURE()

  CODE
  RETURN CHOOSE (GetSystemMetrics( SM_REMOTESESSION ) <> 0)


!-----------------------------------------------------------------------------!
!HasTouchInput
! Returns TRUE if the the maximum number of contacts supported by any digitizer in the system is > 0
!-----------------------------------------------------------------------------!
HasTouchInput       PROCEDURE()
  CODE
  RETURN CHOOSE (GetSystemMetrics( SM_MAXIMUMTOUCHES ) <> 0)


!-----------------------------------------------------------------------------!
!GetFileSystemTimeDate
! Get the system File Time and Date for the specified file.
!-----------------------------------------------------------------------------!
GetFileSystemTimeDate PROCEDURE (STRING FileName, BYTE which, *SYSTEMTIME systime)

hFindHandle        LONG,AUTO
gFileFind          GROUP( WIN32_FIND_DATA ),AUTO.
szFindFile         CSTRING(FILE:MaxFileName),AUTO
gTempLocalTime     GROUP(FILETIME),AUTO.

  CODE
  hFindHandle = 0
  szFindFile  = CLIP (FileName)
  CLEAR (gFileFind)

  hFindHandle = FindFirstFile( szFindFile, gFileFind )
  IF hFindHandle = INVALID_LONG_VALUE OR hFindHandle <= 0
    RETURN FALSE
  END

  FindClose (hFindHandle)

  CASE which
  OF 0
    FileTimeToLocalFileTime( gFileFind.ftLastWriteTime, gTempLocalTime )
  OF 1
    FileTimeToLocalFileTime( gFileFind.ftCreationTime, gTempLocalTime )
  OF 2
    FileTimeToLocalFileTime( gFileFind.ftLastAccessTime, gTempLocalTime )
  END

  FileTimeToSystemTime ( gTempLocalTime, systime )
  RETURN TRUE

!-----------------------------------------------------------------------------!
!GetFileTime
! Get the File Time for the specified file.
!-----------------------------------------------------------------------------!
GetFileTime        PROCEDURE ( STRING iFile, BYTE bType=0 )

gTempSystemTime    GROUP(SYSTEMTIME),AUTO.

  CODE
  IF GetFileSystemTimeDate (iFile, bType, gTempSystemTime)
    RETURN DEFORMAT( gTempSystemTime.wHour & ':' & gTempSystemTime.wMinute & ':' & gTempSystemTime.wSecond, @t4 )
  END

  RETURN 0

!-----------------------------------------------------------------------------!
!GetFileDate
! Get the File Date for the specified file.
!-----------------------------------------------------------------------------!
GetFileDate        PROCEDURE ( STRING iFile, BYTE bType=0 )

gTempSystemTime    GROUP(SYSTEMTIME),AUTO.

  CODE
  IF GetFileSystemTimeDate (iFile, bType, gTempSystemTime)
    RETURN DEFORMAT( gTempSystemTime.wMonth & '/' & gTempSystemTime.wDay & '/' & gTempSystemTime.wYear, @d2 )
  END

  RETURN 0

!-----------------------------------------------------------------------------!
!CreateDirectory
! Create a directory
!-----------------------------------------------------------------------------!
CreateDirectory    PROCEDURE ( STRING sDirectory )

szDirName          CSTRING(FILE:MaxFilePath),AUTO

  CODE
  szDirName = CLIP( sDirectory )
  RETURN mkdir ( szDirName )

!-----------------------------------------------------------------------------!
!RemoveDirectory
! Remove a directory
!-----------------------------------------------------------------------------!
RemoveDirectory    PROCEDURE ( STRING sDirectory )

szDirName          CSTRING(FILE:MaxFilePath),AUTO

  CODE
  szDirName = CLIP( sDirectory )
  RETURN rmdir ( szDirName )

!-----------------------------------------------------------------------------!
!GetTempPath
! Retreive the path pointed to by TMP or TEMP
!-----------------------------------------------------------------------------!
GetTempPath        PROCEDURE()

sTmpPath           STRING(FILE:MaxFilePath),AUTO
lPathSize          LONG,AUTO

  CODE
  lPathSize = GetTempPath ( SIZE (sTmpPath), sTmpPath )
  IF lPathSize = 0 OR lPathSize > SIZE (sTmpPath)
    RETURN ''
  ELSE
    RETURN sTmpPath [ 1 : lPathSize ]
  END

!-----------------------------------------------------------------------------!
!GetTempPath
! Retreive the path pointed to by TMP or TEMP
!-----------------------------------------------------------------------------!
GetTempFileName    PROCEDURE ( STRING sPrefix, <STRING sDirectory> )

szPrefix           CSTRING( 4 ),AUTO
szPath             CSTRING(FILE:MaxFilePath),AUTO
szTempname         CSTRING(FILE:MaxFilePath),AUTO

  CODE
  IF OMITTED( 2 )
    szPath = GetTempPath()
    IF CLIP( szPath ) = ''
      szPath = '.'
    END
  ELSE
    szPath = CLIP( sDirectory )
  END
  szPrefix = CLIP( sPrefix )
  IF szPrefix = ''
    szPrefix = '$$$'
  END
  IF GetTempFileName( szPath, szPrefix, 0, szTempName ) = 0
    RETURN ''
  ELSE
    RETURN CLIP( szTempName )
  END

!-----------------------------------------------------------------------------!
!FullDrag
! Query or change full window dragging setting
!-----------------------------------------------------------------------------!
FullDrag           PROCEDURE( <LONG lDragSetting> )

lCurrentSetting    LONG

  CODE
  IF ~OMITTED( 1 )
    SystemParametersInfo(SPI_SETDRAGFULLWINDOWS, lDragSetting, lCurrentSetting, SPIF_SENDWININICHANGE+SPIF_UPDATEINIFILE)
    RETURN lDragSetting
  END
  SystemParametersInfo(SPI_GETDRAGFULLWINDOWS, 0, lCurrentSetting, SPIF_SENDWININICHANGE+SPIF_UPDATEINIFILE)
  RETURN lCurrentSetting

!-----------------------------------------------------------------------------!
!LongToHex
!ShortToHex
!ByteToHex
! Convert numeric values to hexadecimal string
!-----------------------------------------------------------------------------!

LongToHex PROCEDURE(ULONG in, BYTE LowerCase)
Out STRING(8),AUTO
Inb BYTE,DIM(4),OVER(in)
  CODE
  Out[1 : 2] = ByteToHex(Inb[4], LowerCase)
  Out[3 : 4] = ByteToHex(Inb[3], LowerCase)
  Out[5 : 6] = ByteToHex(Inb[2], LowerCase)
  Out[7 : 8] = ByteToHex(Inb[1], LowerCase)
  RETURN Out

ShortToHex PROCEDURE(USHORT in, BYTE LowerCase)
Out STRING(4),AUTO
Inb BYTE,DIM(2),OVER(in)
  CODE
  Out[1 : 2] = ByteToHex(Inb[2], LowerCase)
  Out[3 : 4] = ByteToHex(Inb[1], LowerCase)
  RETURN Out

ByteToHex PROCEDURE(BYTE in, BYTE LowerCase)
Out       STRING(2),AUTO
HEX       &STRING,AUTO
  CODE
  IF LowerCase
    HEX &= HexDigitsLow
  ELSE
    HEX &= HexDigitsUp
  END
  Out[1] = HEX [BSHIFT(in, -4) + 1]
  Out[2] = HEX [BAND(in, 0FH) + 1]
  RETURN Out

!-----------------------------------------------------------------------------!
!BLOBTOFILE
! Copy data from the BLOB field to file
!-----------------------------------------------------------------------------!

  OMIT ('=== DO LINK', lib_mode)
    PRAGMA('link(C%V%DOS%X%%L%.LIB)')
! === DO LINK

BLOBTOFILE  PROCEDURE (*BLOB b, STRING name)
OutFile FILE,DRIVER('DOS'),CREATE
          RECORD
buffer      STRING(32768)
          END
        END
sz      LONG,AUTO
start   LONG,AUTO
amount  LONG,AUTO
CurErr  SIGNED,AUTO
  CODE
  IF name = ''
    RETURN BadFileErr
  END
  sz = b{PROP:Size}
  IF sz = 0
    RETURN 0
  END
  OutFile{PROP:Name} = name
  CREATE (OutFile)
  IF ERRORCODE() THEN RETURN ERRORCODE().
  OPEN (OutFile)
  IF ERRORCODE() THEN RETURN ERRORCODE().
  SEND (OutFile, 'FILEBUFFERS=' & ROUND(sz/512, 1))

  CurErr = 0
  start  = 0
  LOOP WHILE sz <> 0
    amount = SIZE (OutFile.buffer)
    IF amount > sz
      amount = sz
    END
    OutFile.buffer [1 : amount] = b [start : start + amount - 1]
    ADD (OutFile, amount)
    CurErr = ERRORCODE()
    IF CurErr <> 0
      BREAK
    END
    start += amount
    sz    -= amount
  END

  CLOSE (OutFile)
  RETURN CurErr

!-----------------------------------------------------------------------------!
!FILETOBLOB
! Copy data from file to the BLOB field
!-----------------------------------------------------------------------------!

FILETOBLOB  PROCEDURE (STRING name, *BLOB b)
InFile  FILE,DRIVER('DOS')
          RECORD
buffer      STRING(32768)
          END
        END
sz      LONG,AUTO
start   LONG,AUTO
fetch   LONG,AUTO
CurErr  SIGNED,AUTO
  CODE
  IF name = ''
    RETURN BadFileErr
  END
  InFile{PROP:Name} = name
  OPEN (InFile,40h)
  IF ERRORCODE() THEN RETURN ERRORCODE().
  sz = BYTES(InFile)
  IF sz = 0
    CurErr = BadFileErr
  ELSE
    SEND (InFile, 'FILEBUFFERS=' & ROUND(sz/512, 1))
    b{PROP:Size} = sz
    
    CurErr = 0
    start  = 0
    LOOP WHILE sz <> 0
      fetch = SIZE (InFile.buffer)
      IF fetch > sz
        fetch = sz
      END
      GET (InFile, start + 1, fetch)
      CurErr = ERRORCODE()
      IF CurErr <> 0
        BREAK
      END
      b [start : start + fetch - 1] = InFile.buffer [1 : fetch]
      start += fetch
      sz    -= fetch
    END
  END

  CLOSE (InFile)
  RETURN CurErr

!-----------------------------------------------------------------------------!
!ResizeImage
! Resize the image to fit the original control size
! pImagePosition set the position where the image will go
!-----------------------------------------------------------------------------!

ResizeImage PROCEDURE (SIGNED ctl, SIGNED x, SIGNED y, SIGNED w, SIGNED h, <REPORT report>)
RH    SIGNED
RW    SIGNED
NW    SIGNED
NH    SIGNED
Ratio REAL
  CODE
  IF ctl = 0
    RETURN
  END

  IF NOT OMITTED (6)
    SETTARGET (report)
  END

  RW = ctl {PROP:MaxWidth}
  RH = ctl {PROP:MaxHeight}

  IF RW = 0 OR RH = 0
    NW = 0
    NH = 0
  ELSE
    Ratio = RW / RH
    NW = h * Ratio

    IF NW > w
      NW = w
      NH = w / Ratio
    ELSE
      NH = h
    END

    x += (w - NW) / 2
    y += (h - NH) / 2
  END

  SETPOSITION (ctl, x, y, NW, NH)

  IF NOT OMITTED (6)
    SETTARGET
  END
  RETURN


!-----------------------------------------------------------------------------!
! ColorToHSL
! Convert a Clarion color to HSL (hue-saturation-lightness)
!-----------------------------------------------------------------------------!
ColorToHSL           PROCEDURE  (LONG pColor,*REAL H,*REAL S,*REAL L)
var_R REAL
var_G REAL
var_B REAL

var_Min REAL
var_Max REAL
del_Max REAL

del_R REAL
del_G REAL
del_B REAL


R                  BYTE
G                  BYTE
B                  BYTE
  CODE
 ColorToRGB(pColor,R,G,B)

 var_R = ( R / 255 )                     !Where RGB values = 0 .. 255
 var_G = ( G / 255 )
 var_B = ( B / 255 )

 var_Min = MinByteValue( R, G, B )/255    !Min. value of RGB
 var_Max = MaxByteValue( R, G, B )/255    !Max. value of RGB
 del_Max = var_Max - var_Min     !Delta RGB value

 L = ( var_Max + var_Min ) / 2

 IF ( del_Max = 0 )                     !This is a gray, no chroma...
   H = 0                                !HSL results = 0 .. 1
   S = 0
 ELSE                                   !Chromatic data...
   IF ( L < 0.5 )
      S = del_Max / ( var_Max + var_Min )
   ELSE
      S = del_Max / ( 2 - var_Max - var_Min )
   END

   del_R = ( ( ( var_Max - var_R ) / 6 ) + ( del_Max / 2 ) ) / del_Max
   del_G = ( ( ( var_Max - var_G ) / 6 ) + ( del_Max / 2 ) ) / del_Max
   del_B = ( ( ( var_Max - var_B ) / 6 ) + ( del_Max / 2 ) ) / del_Max

   IF ( var_R = var_Max )
      H = del_B - del_G
   ELSIF ( var_G = var_Max )
      H = ( 1 / 3 ) + del_R - del_B
   ELSIF ( var_B = var_Max )
      H = ( 2 / 3 ) + del_G - del_R
   END

   IF ( H < 0 )
      H += 1
   END
   IF ( H > 1 )
      H -= 1
   END
 END

!-----------------------------------------------------------------------------!
! ColorToRGB
! Convert a Clarion color to RGB
!-----------------------------------------------------------------------------!
ColorToRGB           PROCEDURE  (LONG pColor,*BYTE R,*BYTE G,*BYTE B)
CWColor            LONG
RGBT               GROUP,OVER(CWColor)
R                    BYTE
G                    BYTE
B                    BYTE
NotUsed              BYTE
                   END
  CODE
  CWColor = pColor
  R=RGBT.R
  G=RGBT.G
  B=RGBT.B

!-----------------------------------------------------------------------------!
! MaxByteValue
! Return the maximum byte value from the 3 bytes passed as parameters
!-----------------------------------------------------------------------------!
MaxByteValue         PROCEDURE  (BYTE A,BYTE B,BYTE C)
  CODE
  IF A>B
     IF A>C
        RETURN A
     END
  ELSE
     IF B>C
        RETURN B
     END
  END
  RETURN C

!-----------------------------------------------------------------------------!
! MinByteValue
! Return the minimum byte value from the 3 bytes passed as parameters
!-----------------------------------------------------------------------------!
MinByteValue         PROCEDURE  (BYTE A,BYTE B,BYTE C)
  CODE
  IF A<B
     IF A<C
        RETURN A
     END
  ELSE
     IF B<C
        RETURN B
     END
  END
  RETURN C

!-----------------------------------------------------------------------------!
! HSLToColor
! Convert an HSL (hue-saturation-lightness) color to a Clarion color
!-----------------------------------------------------------------------------!
HSLToColor           PROCEDURE  (REAL H,REAL S,REAL L,*LONG pColor)
CWColor            LONG
R                  BYTE
G                  BYTE
B                  BYTE

vH                 REAL
v1                 REAL
v2                 REAL
vR                 REAL
  CODE
 !HSL values = 0 to 1
 !RGB results = 0 to 255

 IF S = 0 THEN
   R = L * 255
   G = L * 255
   B = L * 255
 ELSE

   IF ( L < 0.5 ) THEN
      v2 = L * ( 1 + S )
   ELSE
      v2 = ( L + S ) - ( S * L )
   END

   v1 = 2 * L - v2

   vH = H + ( 1 / 3 )
   Do Hue_2_RGB
   R = 255 * vR

   vH = H
   Do Hue_2_RGB
   G = 255 * vR

   vH = H - ( 1 / 3 )
   Do Hue_2_RGB
   B = 255 * vR
 END
 RGBToColor(R,G,B,pColor)

!--------------------------------------------------------------------------------

Hue_2_RGB ROUTINE

   IF ( vH < 0 )
      vH += 1
   END
   IF ( vH > 1 )
      vH -= 1
   END
   IF ( ( 6 * vH ) < 1 )
      vR=( v1 + ( v2 - v1 ) * 6 * vH )
      EXIT
   END
   IF ( ( 2 * vH ) < 1 )
      vR = ( v2 )
      EXIT
   END
   IF ( ( 3 * vH ) < 2 )
      vR= ( v1 + ( v2 - v1 ) * ( ( 2 / 3 ) - vH ) * 6 )
      EXIT
   END

   vR = v1

!-----------------------------------------------------------------------------!
! RGBToColor
! Convert an RGB color to a Clarion color
!-----------------------------------------------------------------------------!
RGBToColor      PROCEDURE (BYTE R, BYTE G, BYTE B, *LONG pColor)

RGBT               GROUP,OVER(pColor)
R                    BYTE
G                    BYTE
B                    BYTE
DitherMode           BYTE
                   END

  CODE
  RGBT.R = R
  RGBT.G = G
  RGBT.B = B
  RGBT.DitherMode = SYSTEM {PROP:ColorHighByte}

!-----------------------------------------------------------------------------!
! MixColors
! Mix color1 and color2 using the percentage of mix.
! If pColor1 is white the pColor2 will be the same pColor2 color but bright
! If pColor1 is black the pColor2 will be the same pColor2 color but darker
!-----------------------------------------------------------------------------!

MixColors   PROCEDURE (LONG pColor1, LONG pColor2, BYTE pPercentageMix)

CWColor1           LONG,AUTO
RGBT1              GROUP,OVER(CWColor1)
R                    BYTE
G                    BYTE
B                    BYTE
DitherMode           BYTE
                   END
CWColor2           LONG,AUTO
RGBT2              GROUP,OVER(CWColor2)
R                    BYTE
G                    BYTE
B                    BYTE
DitherMode           BYTE
                   END
RetCWColor         LONG
RetRGBT            GROUP,OVER(RetCWColor)
R                    BYTE
G                    BYTE
B                    BYTE
DitherMode           BYTE
                   END

DitherColors       LONG,AUTO

  CODE
  IF pPercentageMix > 100
     pPercentageMix = 100
  END

  DitherColors = SYSTEM {PROP:ColorHighByte} * 1000000h

  IF BAND (pColor1, 80000000h)
    pColor1 = GetSysColor (BAND (pColor1, 0FFFFh))
  ELSE
    pColor1 = BOR (DitherColors, BAND (pColor1, 0FFFFFFh))
  END
  IF BAND (pColor2, 80000000h)
    pColor2 = GetSysColor (BAND (pColor2, 0FFFFh))
  ELSE
    pColor2 = BOR (DitherColors, BAND (pColor2, 0FFFFFFh))
  END

  CWColor1 = pColor1
  CWColor2 = pColor2

  RetRGBT.R = (RGBT1.R + ((RGBT2.R - RGBT1.R) * pPercentageMix/100))
  RetRGBT.G = (RGBT1.G + ((RGBT2.G - RGBT1.G) * pPercentageMix/100))
  RetRGBT.B = (RGBT1.B + ((RGBT2.B - RGBT1.B) * pPercentageMix/100))
  RetRGBT.DitherMode = 0

  RETURN BOR (RetCWColor, DitherColors)
