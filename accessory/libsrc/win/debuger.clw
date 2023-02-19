   member
   map
    module('Winapi')
    debugbreak(),pascal,raw,name('debugbreak')
    omit('***',_width32_)
    OutputDebugString(*cstring),pascal,raw,name('OutputDebugString')
    !  ***
    compile('***',_width32_)
    OutputDebugString(*cstring),pascal,raw,name('OutputDebugStringA')
    !  ***
    end
    
   end

   include('debuger.inc')

!-----------------------------------------------------------------------------------------!
Debuger.init  Procedure(Pgmname,posmode,pduplicates)
!-----------------------------------------------------------------------------------------!
   code
   self.pgmname = pgmname
   self.osmode = posmode   !this parameter if true, turns on debug regaredless of project setting

   self.debugfilter = '`'  ! used as first character to allow filtering in debugviewer.exe
   self.duplicates = 50    ! default number of same debug messages in a row before a warning message is issued

   if not omitted(4) then
      self.duplicates = pduplicates
      .

   if not omitted(3) and self.osmode = true then goto debuginit. ! force debug on regardless of debug in project
   if command('/Debuger') then goto debuginit.  ! force debug on a production app

?  goto debuginit
   self.debugactive = false
   return

debuginit

   self.debugactive = true
   self.debugout('Program Started -')
   if not omitted(3) and self.osmode = true then self.debugout('Warning - Debuger ALWAYS ON! (INIT)'). ! force debug on regardless of debug in project
   if command('/Debuger') then self.debugout('Warning - Debuger turned on from command line').  ! force debug on a production app

!-----------------------------------------------------------------------------------------!
Debuger.kill  Procedure()
!-----------------------------------------------------------------------------------------!
   code
   if self.debugactive = false then return.
   self.debugout('Program Ended -')
   self.debugactive = false

!-----------------------------------------------------------------------------------------!
Debuger.debugout  procedure(stuff,headr,flag,force)
!-----------------------------------------------------------------------------------------!
Altmsg cstring(250)

     code

   if self.debugactive = false and force <> true then return.
   altmsg = stuff
 !  if upper(flag) = 'B' or upper(flag) = 'M' then  ! message box and debugger server then break
   if flag = true then
      do debugmsg
      .
   
   if not omitted(3) then
      self.thismsg = clip(headr) & ' - ' & clip(altmsg)
      else
      self.thismsg = clip(altmsg)
      .
  
    self.thismsg = self.debugfilter & self.pgmname & ' - ' & self.thismsg

    OutputDebugString (self.thismsg)  ! send the debug message to the viewer
    
    if self.thismsg = self.lastmsg then
        self.numbersame += 1
        else
        self.numbersame = 0
        .

    if self.duplicates <> 0 and self.numbersame > 0 and |
       self.numbersame % self.duplicates = 0 and self.thismsg = self.lastmsg then
         altmsg = 'A series of ' & self.numbersame & ' duplicate debug messages have been issued'
         do debugmsg
         .
    self.lastmsg = self.thismsg

!-----------------------------------------------------------------------------------------!
debugmsg  routine
!-----------------------------------------------------------------------------------------!
    BEEP(BEEP:SystemExclamation)

    compile('***',_width32_)
?    CASE MESSAGE(altmsg,self.pgmname & ' - ' & headr, ICON:Exclamation, |
?               '&Continue|&Halt|&Debug', 1, 0)
?    OF 1  ! Name: &OK  (Default)
?       exit
?    OF 2  ! Name: &Abort
?       self.debugout('Program Halted -')
?       Halt()
?    OF 3  ! Name: Debug
?       self.debugbreak()

?    END !CASE
?    exit
    !  ***

    CASE MESSAGE(altmsg,self.pgmname & ' - ' & headr, ICON:Exclamation, |
               '&Continue|&Halt', 1, 0)
    OF 1  ! Name: &OK  (Default)
       exit
    OF 2  ! Name: &Abort
       self.debugout('Program Halted -')
       Halt()
    END !CASE

!-----------------------------------------------------------------------------------------!
Debuger.DebugBreak Procedure   ! only for 32 bit...doesnt work in 16 bit mode
!-----------------------------------------------------------------------------------------!
   code
   if self.Debugactive = false then return.
    compile('***',_width32_)
    debugbreak()    ! asm routine
    !  ***



   

   


