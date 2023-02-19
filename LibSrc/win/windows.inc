
  OMIT('EndDefine',_WINDOWS_)
_WINDOWS_  EQUATE(1)

HANDLE                  EQUATE(SIGNED)
HWND                    EQUATE(HANDLE)
HINSTANCE               EQUATE(HANDLE)
HMODULE                 EQUATE(HANDLE)
HMENU                   EQUATE(HANDLE)
HDC                     EQUATE(HANDLE)
HICON                   EQUATE(HANDLE)
HCURSOR                 EQUATE(HANDLE)
HBRUSH                  EQUATE(HANDLE)
HBITMAP                 EQUATE(HANDLE)
HGDIOBJ                 EQUATE(HANDLE)
HFONT                   EQUATE(HANDLE)
HRGN                    EQUATE(HANDLE)
HGLOBAL                 EQUATE(HANDLE)
HPEN                    EQUATE(HANDLE)
HRESULT                 EQUATE(LONG)
DWORD                   EQUATE(ULONG)
LPCSTR                  EQUATE(CSTRING)
LPSTR                   EQUATE(CSTRING)
LPVOID                  EQUATE(LONG)
LPCVOID                 EQUATE(LONG)
PLONG                   EQUATE(LONG)

UUID GROUP,TYPE
Data1 ULONG
Data2 USHORT
Data3 USHORT
Data4 STRING(8)
     END


  MAP
    MODULE('WinApi')
      CloseHandle(HANDLE hObject),BOOL,PASCAL,PROC
      CreateSemaphore(<*SECURITY_ATTRIBUTES lpSemaphoreAttributes>, LONG lInitialCount, LONG lMaximumCount, *CSTRING lpName), LONG, RAW, PASCAL, NAME('CreateSemaphoreA')
      FindWindow(<*CSTRING lpClassName>, *CSTRING lpWindowName),UNSIGNED,PASCAL,RAW,NAME('FindWindowA')
      FreeLibrary(LONG hModule),BOOL,PASCAL,PROC
      GetDC(HWND),HDC,PASCAL,PROC,NAME('GetDC')
      GetDeviceCaps(HDC, SIGNED),SIGNED,PASCAL,NAME('GetDeviceCaps')
      GetLastError(),LONG,PASCAL
      GetOpenFileName(long),pascal,bool,proc,name('GetOpenFileNameA')
      GetPrivateProfileString(*LPCSTR,*LPCSTR,*LPCSTR,*LPCSTR,DWORD,*LPCSTR),DWORD,PASCAL,RAW,NAME('GetPrivateProfileStringA')
      GetProcAddress(LONG hModule, *CSTRING szProcName),LONG,PASCAL,RAW,NAME('GetProcAddress')
      GetSaveFileName(Long),pascal,bool,proc,name('GetSaveFileNameA')
      GetSystemMetrics(SIGNED),SIGNED,PASCAL,NAME('GetSystemMetrics')
      IsIconic(UNSIGNED),BOOL,PASCAL
      LoadLibrary(*CSTRING szLibFileName),HINSTANCE,PASCAL,RAW,NAME('LoadLibraryA')
      PatBlt(HDC, SIGNED, SIGNED, SIGNED, SIGNED, DWORD),BOOL,PASCAL
      ReleaseDC(HWND, HDC),SIGNED,PASCAL,PROC,NAME('ReleaseDC')
      RpcStringFree(*LONG gotString),LONG,PROC,PASCAL,NAME('RpcStringFreeA')
      GetForegroundWindow(),HWND,PASCAL
      SetForegroundWindow(UNSIGNED),BOOL,PASCAL
      SetWindowLong(HWND,SIGNED,LONG),LONG,PASCAL,NAME('SetWindowLongA')
      ShellExecute(HWND hwnd,<*LPCSTR lpOperation>,*LPCSTR lpFile,<*LPCSTR lpParameters>,<*LPCSTR lpDirectory>,SIGNED nShowCmd),HINSTANCE,PASCAL,RAW,PROC,NAME('ShellExecuteA')
      ShowWindow(UNSIGNED, SIGNED nCmdShow),BOOL,PASCAL
      UuidCreate(*UUID),LONG,PASCAL,PROC,RAW
      UuidFromString(LONG addressOfString, *UUID ret),LONG,PROC,PASCAL,RAW,NAME('UuidFromStringA')
      UuidToString(*UUID, *LONG resString),LONG,PROC,PASCAL,RAW,NAME('UuidToStringA')  !Returns the address of a CSTRING that must be freed via RpcStringFree
      WaitForSingleObject(HANDLE hHandle, LONG dwMilliseconds),LONG,PASCAL
    END
  END


!  EndDefine

