  MODULE('cwutil.clw')
    ! changes in these prototypes need to be reflected in BLOBSRV.TPW
    OSVersion( ),STRING
    FileExists( STRING sFile ), BYTE
    WindowExists( STRING sWindow ), BYTE
    ValidateOLE( SIGNED OleControl, <STRING OleFileName>, <STRING OleCreateName> ), BYTE
    GetUserName( ),STRING
    BeginUnique( STRING sAppName ),LONG
    EndUnique( LONG hUnique )
    IsTermServer( ),BYTE
    GetFileTime( STRING iFile, BYTE bType=0 ),LONG
    GetFileDate( STRING iFile, BYTE bType=0 ),LONG
    CreateDirectory( STRING sDirectory ), BYTE,PROC
    RemoveDirectory( STRING sDirectory ), BYTE,PROC
    GetTempPath( ), STRING
    GetTempFileName( STRING sPrefix, <STRING sDirectory> ),STRING
    FullDrag( <LONG lDragSetting> ),LONG,PROC
    LongToHex(ULONG, BYTE LowerCase = FALSE),STRING
    ShortToHex(USHORT, BYTE LowerCase = FALSE),STRING
    ByteToHex(BYTE, BYTE LowerCase = FALSE),STRING
    FileToBLOB(STRING name, *BLOB b),SIGNED
    BLOBToFile(*BLOB b, STRING name),SIGNED
    ResizeImage(SIGNED ctl,SIGNED x,SIGNED y,SIGNED w,SIGNED h,<REPORT pReport>)
    ColorToHSL(LONG pColor,*REAL H,*REAL S,*REAL L)
    ColorToRGB(LONG pColor,*BYTE R,*BYTE G,*BYTE B)
    MaxByteValue(BYTE A,BYTE B,BYTE C),BYTE
    MinByteValue(BYTE A,BYTE B,BYTE C),BYTE
    HSLToColor(REAL H,REAL S,REAL L,*LONG pColor)
    RGBToColor(BYTE R,BYTE G,BYTE B,*LONG pColor)
    MixColors(LONG pColor1,LONG pColor2,BYTE pPercentageMix),LONG
    HasTouchInput(),BYTE
  END
  MODULE('Core')
    PathSplit(CONST *CSTRING path, <*CSTRING drive>, <*CSTRING dir>, <*CSTRING file>, <*CSTRING ext>), SIGNED, PROC, RAW, NAME('_fnsplit')
    PathMerge(*CSTRING path, <*CSTRING drive>, <*CSTRING dir>, <*CSTRING file>, <*CSTRING ext>), SIGNED, PROC, RAW, NAME('_fnmerge')
  END

COMPILE('Link', _ABCLinkMode_)
 PRAGMA('compile(CWUTIL.CLW)')
!Link
