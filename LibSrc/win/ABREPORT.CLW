! Implementation module for process server
  MEMBER

  MAP
    Idx2Percentile (*PrintPreviewClass SELF,BYTE Index),SIGNED,PRIVATE
    Percentile2Idx (*PrintPreviewClass SELF,SIGNED Percentile),BYTE,PRIVATE

    MODULE('WinApi')
      MulDiv (SIGNED,SIGNED,SIGNED),SIGNED,PASCAL
      GetSystemMetrics (SIGNED),SIGNED,PASCAL
    END
  END

  INCLUDE('KEYCODES.CLW'),ONCE
  INCLUDE('ERRORS.CLW'),ONCE
  INCLUDE('ABREPORT.INC'),ONCE
  INCLUDE('ABREPORT.TRN'),ONCE
  INCLUDE('ABQUERY.INC'),ONCE

SM_CXVSCROLL      EQUATE(2)

ChildList         QUEUE,TYPE
VM                  &ViewManager
Level               BYTE
                  END

ZoomItemQueue     QUEUE,TYPE
Percentile          SIGNED                       !Zoom percentile or special zoom value
MenuText            CSTRING(64)
StatusText          CSTRING(64)
                  END

!PageManagerClass objects are entirely encapsulated within the PrintPreviewClass object's DISPLAY method.

PageManagerClass  CLASS,TYPE
PP                  &PrintPreviewClass,PRIVATE
ImageFeq            SIGNED,PRIVATE                          !Feq of ImageField
BorderFeq           SIGNED,PRIVATE                          !Feq of BorderField
ImageWidth          UNSIGNED,PRIVATE                        !Extent of image
ImageHeight         UNSIGNED,PRIVATE
XPos                SIGNED,PRIVATE                          !Thumbnail Pos and size
YPos                SIGNED,PRIVATE
Width               SIGNED,PRIVATE
Height              SIGNED,PRIVATE
FileName            &STRING,PRIVATE                         !wmf file
Neighbour           &PageManagerClass,PRIVATE               !Pointer to previous Object in linked list
PageNo              LONG,PRIVATE
ZoomState           SIGNED,PRIVATE
CentreOnX           BYTE,PRIVATE                            !Attempts to centre Zoomed image on these approximate Coords (Range 0-100)
CentreOnY           BYTE,PRIVATE                            !Reset to (0,0) at next redraw
Created             BYTE,PRIVATE

CoordContained      PROCEDURE(SIGNED X,SIGNED Y),LONG       !Returns page containing the mouse coords x,y
Delete              PROCEDURE()
Delete              PROCEDURE(LONG PageNo)
Draw                PROCEDURE(LONG PageNo,SIGNED ZoomFactor=NoZoom)
Exists              PROCEDURE(LONG PageNo),BYTE
GetProp             PROCEDURE(LONG PageNo,UNSIGNED Property),STRING
HighLight           PROCEDURE(LONG PageNo)
Init                PROCEDURE(*PrintPreviewClass P,LONG PageNo,SIGNED BaseFeq,*STRING FileName)
Init                PROCEDURE(*PrintPreviewClass P,LONG PageNo,SIGNED BaseFeq,*STRING FileName,SIGNED XPos,SIGNED YPos,SIGNED Width,SIGNED Height)
Kill                PROCEDURE()
SetCentre           PROCEDURE(LONG PageNo,BYTE X,BYTE Y)
SetPosition         PROCEDURE(LONG PageNo,SIGNED XPos,SIGNED YPos,SIGNED Width,SIGNED Height)
                  END

! ==============================================================================

Idx2Percentile  PROCEDURE (*PrintPreviewClass SELF, BYTE Index)
  CODE
? ASSERT(INRANGE(Index,0,RECORDS(SELF.ZoomQueue)))

  IF Index = 0
    RETURN SELF.UserPercentile
  END

  GET (SELF.ZoomQueue, Index)
? ASSERT (ERRORCODE() = 0)
  RETURN SELF.ZoomQueue.Percentile


Percentile2Idx  PROCEDURE (*PrintPreviewClass SELF, SIGNED Percentile)

num        BYTE,AUTO
idx        BYTE,AUTO

  CODE
  num = RECORDS (SELF.ZoomQueue)

  LOOP idx = 1 TO num
    GET (SELF.ZoomQueue, idx)
?   ASSERT (ERRORCODE() = 0)

    IF SELF.ZoomQueue.Percentile = Percentile
      RETURN idx
    END
  END
  RETURN num + 1                       !error condition

! ==============================================================================

ProcessClass.AddItem PROCEDURE (ViewManager VM, BYTE Level)
  CODE
  IF SELF.Children &= NULL
     SELF.Children &= NEW ChildList
  END

  SELF.Children.VM   &= VM
  SELF.Children.Level = Level
  ADD (SELF.Children)
  RETURN RECORDS(SELF.Children)


ProcessClass.Init PROCEDURE (VIEW V, RelationManager R, SIGNED ProgressText, <*BYTE PercentProgress>, LONG GuessRecords)
  CODE
  SELF.Percentile &= PercentProgress
  IF ~SELF.Percentile &= NULL
    SELF.Percentile = 0
  END

  SELF.RecordsToProcess = GuessRecords
  SELF.PText            = ProgressText
  PARENT.Init (V, R)
  RETURN


ProcessClass.Init PROCEDURE (VIEW V, RelationManager R, SIGNED ProgressText, <*BYTE PercentProgress>, StepClass StepManager, *? ValueField)
  CODE
  SELF.Percentile &= PercentProgress
  IF ~SELF.Percentile &= NULL
    SELF.Percentile = 0
  END
  SELF.PText              = ProgressText
  SELF.StepMgr           &= StepManager
  SELF.ValueField        &= ValueField
  SELF.CaseSensitiveValue = TRUE       ! For backward compatibility
  PARENT.Init (V, R)
  RETURN


ProcessClass.Kill PROCEDURE()
  CODE
  SELF.Percentile &= NULL
  SELF.ValueField &= NULL
  IF NOT SELF.Primary &= NULL
     SELF.Primary.SetQuickScan (0, Propagate:OneMany)
  END

  DISPOSE (SELF.Children)

  IF ~SELF.StepMgr &= NULL
    SELF.StepMgr.Kill()
  END
  PARENT.Kill()
  RETURN


ProcessClass.TakeRecord PROCEDURE()
  CODE
  RETURN Level:Benign


ProcessClass.Reset  PROCEDURE()
  CODE
  SELF.RecordsProcessed = 0

  IF ~SELF.StepMgr &= NULL
    SELF.SetProgressLimits()
  ELSIF SELF.RecordsToProcess = 0
    SELF.Primary.Me.UseFile()
    SELF.RecordsToProcess = RECORDS(SELF.Primary.Me.File)

    IF SELF.RecordsToProcess = 0       ! Records not supported
      SELF.FileSize = SELF.Primary.Me.File{PROP:FileSize}
    END
  END

  SELF.ChildRead = 0
  PARENT.Reset()
  RETURN


ProcessClass.ChildProcessingStart  PROCEDURE (USHORT ChildRead)
  CODE
  SELF.Children.VM.ApplyRange()
  SELF.Children.VM.Reset()
  SELF.ChildEOF = FALSE
  RETURN


ProcessClass.ChildProcessingEnd  PROCEDURE (USHORT ChildRead)
  CODE
  SELF.ChildEOF = TRUE
  RETURN


ProcessClass.Next  PROCEDURE (BYTE ProcessRecords)
RetVal          BYTE(Level:Benign)
Progress        BYTE,AUTO
PreviousLevel   BYTE,AUTO
  CODE
  LOOP
    IF SELF.RecordsProcessed AND NOT SELF.Children &= NULL AND SELF.ChildRead < RECORDS(SELF.Children)
      IF SELF.ChildRead <> 0
        PreviousLevel   = SELF.Children.Level
        SELF.ChildRead += 1
        GET (SELF.Children, SELF.ChildRead)

        IF NOT SELF.ChildEOF
          IF SELF.Children.Level <= PreviousLevel
            SELF.ChildRead -= 1
            GET (SELF.Children, SELF.ChildRead)
          ELSE
            SELF.Children.VM.ApplyRange()
            SELF.Children.VM.Reset()
            SELF.ChildEOF = FALSE
          END
        ELSE
          !IF the next one is in the same level get it
          !IF not then need to get the parent

          IF SELF.Children.Level = PreviousLevel
            SELF.ChildProcessingStart (SELF.ChildRead)
          ELSE
            SELF.ChildRead -= 1
            LOOP WHILE SELF.ChildRead > 1
              SELF.ChildRead -= 1
              GET (SELF.Children, SELF.ChildRead)

              IF SELF.Children.Level < PreviousLevel
                SELF.ChildEOF = FALSE
                BREAK
              END
            END
          END
        END
      ELSE
        !This will be executed for each parent record
        SELF.ChildRead += 1
        GET (SELF.Children, SELF.ChildRead)
        SELF.ChildProcessingStart (SELF.ChildRead)
      END
    END
  
    IF SELF.ChildEOF
      IF SELF.ChildRead = RECORDS(SELF.Children)
        SELF.ChildRead = 0
      END
    END

    IF SELF.ChildRead <> 0
      RetVal = SELF.Children.VM.Next()
    ELSE
      RetVal = PARENT.Next()
    END

    IF SELF.ChildRead = 0
      BREAK
    END
    IF RetVal = Level:Benign
      BREAK
    END

    SELF.ChildProcessingEnd (SELF.ChildRead)
  END !End of main loop

  IF RetVal = Level:Benign AND ProcessRecords
    IF SELF.FileSize <> 0
      SELF.BytesRead += BYTES (SELF.Primary.Me.File)
    END

    SELF.RecordsProcessed += 1

    IF ~SELF.Percentile &= NULL AND SELF.ChildRead = 0
      IF SELF.StepMgr &= NULL
        IF SELF.FileSize
          Progress = 100 * SELF.BytesRead / SELF.FileSize
        ELSE
          Progress = SELF.RecordsProcessed / SELF.RecordsToProcess*100
        END
      ELSIF SELF.CaseSensitiveValue
        Progress = SELF.StepMgr.GetPercentile (SELF.ValueField)
      ELSE
        Progress = SELF.StepMgr.GetPercentile (UPPER(SELF.ValueField))
      END

      IF Progress > 100
        Progress = 100
      END
      IF Progress <> SELF.Percentile
        SELF.Percentile = Progress
        SELF.UpdateDisplay()
      END
    END
  END
  RETURN RetVal


ProcessClass.UpdateDisplay PROCEDURE()
  CODE
  SELF.PText {PROP:Text} = CHOOSE (SELF.Percentile &= NULL, '', SELF.Percentile) & CompletedText
  DISPLAY
  RETURN


ProcessClass.SetProgressLimits PROCEDURE()

Lo  STRING(64),AUTO                             !64 characters should be enough to identify limits
Hi  STRING(64),AUTO

  CODE
? ASSERT(~SELF.ValueField &= NULL)
  PARENT.Reset()

  IF SELF.Previous()
    CLEAR (SELF.ValueField, 1)
  END

  Hi = SELF.ValueField
  PARENT.Reset()

  IF SELF.Next (FALSE)
    CLEAR (SELF.ValueField, -1)
  END

  Lo = SELF.ValueField
  SELF.SetProgressLimits (Lo, Hi)
  RETURN


ProcessClass.SetProgressLimits PROCEDURE (STRING Lo, STRING Hi)
  CODE
? ASSERT(~SELF.StepMgr &= NULL)
  IF SELF.CaseSensitiveValue
    SELF.StepMgr.SetLimit (Lo, Hi)
  ELSE
    SELF.StepMgr.SetLimit (UPPER(Lo), UPPER(Hi))
  END
  RETURN


ProcessClass.TakeAccepted PROCEDURE()
  CODE
  RETURN

ProcessClass.TakeLocate PROCEDURE()
  CODE
  IF ~SELF.Query &= NULL AND SELF.Query.Ask()
    SELF.SetFilter (SELF.Query.GetFilter())
  END
  RETURN

!
!
!------ Print Previewer Implementation Follows -------------------------------------------------------
!
!


PrintPreviewClass.Init PROCEDURE (*PreviewQueue ImageQueue)

Cnst       ConstantClass

  CODE
  PARENT.Init()

  SELF.PageManager &= NULL
  SELF.ImageQueue  &= ImageQueue       ! Assign the image queue
? ASSERT(~SELF.ImageQueue &= NULL)

  SELF.WinXPos          = _nopos
  SELF.WinYPos          = _nopos
  SELF.WinWidth         = _nopos
  SELF.WinHeight        = _nopos
  SELF.RTLLayout        = FALSE
  SELF.OutputFileQueue &= NEW OutputFileQueue
  SELF.ZoomQueue       &= NEW ZoomItemQueue
  SELF.Popup           &= NEW PopupClass
  SELF.Popup.Init()

  Cnst.Init (Term:Byte)
  Cnst.AddItem (ConstType:PString, SELF.ZoomQueue.MenuText)
  Cnst.AddItem (ConstType:PString, SELF.ZoomQueue.StatusText)
  Cnst.AddItem (ConstType:Short, SELF.ZoomQueue.Percentile)
  Cnst.Set (ZoomPresets)

  LOOP WHILE Cnst.Next() = Level:Benign
    ADD (SELF.ZoomQueue)
?   ASSERT(ERRORCODE() = 0)

    SELF.Popup.AddItem ('-' & SELF.ZoomQueue.MenuText, 'ZoomItem' & RECORDS(SELF.ZoomQueue))
  END
  Cnst.Kill()
  RETURN


PrintPreviewClass.Init PROCEDURE (*PreviewQueue ImageQueue, *ReportTargetSelectorClass pTargetSelector, *WMFDocumentParser WMFDocParser)
  CODE
  SELF.TargetSelector &= pTargetSelector
  SELF.WMFParser      &= WMFDocParser
  SELF.Init (ImageQueue)
  RETURN

    
PrintPreviewClass.SetINIManager PROCEDURE (*INIClass INI)
  CODE
? ASSERT(~INI &= NULL)
  SELF.INIMgr &= INI
  RETURN


PrintPreviewClass.Kill PROCEDURE()
  CODE
  IF NOT SELF.PageManager &= NULL
    SELF.PageManager.Kill()
    DISPOSE (SELF.PageManager)
  END

  IF NOT SELF.Popup &= NULL
    SELF.Popup.Kill()
    DISPOSE (SELF.Popup)
  END

  IF NOT SELF.OutputFileQueue &= NULL
    FREE (SELF.OutputFileQueue)
    DISPOSE (SELF.OutputFileQueue)
  END

  DISPOSE (SELF.ZoomQueue)
  RETURN PARENT.Kill()


PrintPreviewClass.SetRightToLeft  PROCEDURE()
  CODE
  SELF.RTLLayout = TRUE
  RETURN


PrintPreviewClass.SetPosition PROCEDURE (<SIGNED X>,<SIGNED Y>,<SIGNED W>,<SIGNED H>)
  CODE
  IF NOT OMITTED (X)
    SELF.WinXPos = X
  END
  IF NOT OMITTED (Y)
    SELF.WinYPos = Y
  END
  IF NOT OMITTED (W)
    SELF.WinWidth = W
  END
  IF NOT OMITTED (H)
    SELF.WinHeight = H
  END

  IF SELF.WinXPos <> _nopos AND SELF.WinYPos <> _nopos
    SELF.WindowPosSet = TRUE           ! Deprecated member
  END

  IF SELF.WinWidth <> _nopos AND SELF.WinHeight <> _nopos
    SELF.WindowSizeSet = TRUE          ! Deprecated member
  END
  RETURN


PrintPreviewClass.InitWindowPosition  PROCEDURE (WINDOW PreviewWindow)

x    SIGNED,AUTO
y    SIGNED,AUTO
w    SIGNED,AUTO
h    SIGNED,AUTO
doit BOOL,AUTO

  CODE
  x = SELF.WinXPos
  y = SELF.WinYPos
  w = SELF.WinWidth
  h = SELF.WinHeight

  IF x <> _nopos OR y <> _nopos OR w <> _nopos OR h <> _nopos
    doit = TRUE
  ELSIF SELF.SaveSize.Set
    x = SELF.SaveSize.XPos
    y = SELF.SaveSize.YPos
    w = SELF.SaveSize.Width
    h = SELF.SaveSize.Height
    doit = CHOOSE (x <> _nopos OR y <> _nopos OR w <> _nopos OR h <> _nopos)
  ELSIF NOT SELF.INIMgr &= NULL
    SELF.INIMgr.Fetch ('_PreviewWindow_', PreviewWindow)
    doit = FALSE
  END

  IF doit
    SETPOSITION (PreviewWindow, x, y, w, h)
  END

  IF SELF.Maximize
    PreviewWindow {PROP:Maximize} = TRUE    ! Window requested opened maximized
  END
  RETURN

!Return false from these functions to signal Supress Printing, True to signal printing OK


PrintPreviewClass.Display PROCEDURE (SHORT InitZoom,LONG InitCurrentPage,USHORT InitPagesAcross,USHORT InitPagesDown)

PreviewWindow WINDOW('Report Preview'),MAX,AT(,,310,120),CENTER,ICON(ICON:Print), |
       GRAY,IMM,RESIZE,STATUS(-1,90,70),FONT('MS Shell Dlg', 10,,, CHARSET:ANSI), |
       ALRT(MouseRightUp),ALRT(PgUpKey),ALRT(PgDnKey)
    MENUBAR, USE(?MENUBAR)
      MENU('&File'), USE(?FileMenu)
        ITEM('Pa&ges to Print...'), USE(?PagesToPrint), MSG('Select which pages to print')
        ITEM(''), SEPARATOR, USE(?SEPARATOR1)
        ITEM('&Print'), USE(?Print), MSG('Print the report')
        ITEM('Print C&urrent Page'), USE(?PrintCurrent), MSG('Print the current report page'),HIDE
        ITEM('&Save As...'), USE(?SaveAs), MSG('Save the report')
        ITEM('&Email report'), USE(?ProcFiles), MSG('Send the report by email')            
        ITEM(''), SEPARATOR, USE(?SEPARATOR2)
        ITEM('E&xit'), USE(?Exit), MSG('Exit without printing the report')
      END
      MENU('&View'), USE(?ViewMenu), DISABLE
        ITEM('&Next Page(s)'), USE(?Next), MSG('View the next page or pages of the report')
        ITEM('&Previous Page(s)'), USE(?Previous), MSG('View the previous page or pages of the report'), DISABLE
        ITEM(''), SEPARATOR, USE(?SEPARATOR3)
        ITEM('&Jump to a page'), USE(?Jump), MSG('Jump to a specific page of the report')
        ITEM(''), SEPARATOR, USE(?SEPARATOR4)
        ITEM('&Change Display'), USE(?ChangeDisplay), MSG('Change the number of pages displayed')
      END
      MENU('&Zoom'), USE(?ZoomMenu)
      END
    END
    TOOLBAR,AT(,,,18),USE(?TOOLBAR)
      BUTTON,AT(2,1,16,16),USE(?TBarPrint),ICON(ICON:Print),TIP('Print this report'),FLAT
      BUTTON,AT(20,1,16,16),USE(?TBarExit),ICON(ICON:NoPrint),TIP('Exit without printing the report'),FLAT
      BUTTON,AT(37,1,16,16),USE(?TBarZoom),ICON(ICON:Zoom),TIP('Zoom in on a page of the report'),FLAT
      PROMPT('&Page:'),AT(53,5,20,9),USE(?PagePrompt),RIGHT
      SPIN(@N4),AT(76,5,28,9),USE(SELF.CurrentPage,,?CurrentPage),RANGE(1,9999),STEP(1)
      PROMPT('&Across:'),AT(109,5,24,9),USE(?AcrossPrompt),RIGHT
      SPIN(@N2),AT(136,5,24,9),USE(SELF.PagesAcross,,?PagesAcross),MSG('Select the number of thumbnails in a row'),RANGE(1,10),STEP(1),VSCROLL
      PROMPT('&Down:'),AT(165,5,20,9),USE(?DownPrompt),RIGHT
      SPIN(@N2),AT(188,5,24,9),USE(SELF.PagesDown,,?PagesDown),MSG('Select the number of thumbnails in a column'),RANGE(1,10),STEP(1)
      PROMPT('Z&oom:'), AT(217,5,20,9),USE(?ZoomPrompt),HIDE,RIGHT
      COMBO(@S16),AT(240,5,69,9),USE(SELF.ZoomList,,?ZoomList),MSG('Set zoom to standard or user defined value'),DROP(10,70),HIDE,FROM('')
    END
 END

  CODE
  SELF.PrintOK = FALSE

  IF RECORDS (SELF.ImageQueue) <> 0
    SELF.InitZoom    = CHOOSE(InitZoom = 0, DefaultZoom, InitZoom)      !Uses zoom from AbReport.Trn if no passed
    SELF.FirstPage   = InitCurrentPage
    SELF.CurrentPage = InitCurrentPage
    SELF.PagesAcross = InitPagesAcross
    SELF.PagesDown   = InitPagesDown
    SELF.SetDefaultPages()

    IF SELF.RTLLayout
      SETLAYOUT (PreviewWindow, 1)
    END

    OPEN (PreviewWindow)

    SELF.PreviewWindow &= PreviewWindow
    SELF.InitWindowPosition (PreviewWindow)
    SETCURSOR (CURSOR:Wait)

    IF NOT SELF.Translator &= NULL
      SELF.Translator.TranslateWindow (PreviewWindow)
    END

    SELF.PrtPrev:TBarZoom    = ?TBarZoom
    SELF.PrtPrev:TBarPrint   = ?TBarPrint
    SELF.PrtPrev:TBarExit    = ?TBarExit
    SELF.PrtPrev:MenuPages   = ?PagesToPrint
    SELF.PrtPrev:MenuPrint   = ?Print
    SELF.PrtPrev:MenuPrintCurrent = ?PrintCurrent
    SELF.PrtPrev:MenuSaveAs  = ?SaveAs
    SELF.PrtPrev:MenuProcFiles = ?ProcFiles
    SELF.PrtPrev:MenuExit    = ?Exit
    SELF.PrtPrev:ZoomList    = ?ZoomList
    SELF.PrtPrev:ZoomPrompt  = ?ZoomPrompt
    SELF.PrtPrev:PagePrompt  = ?PagePrompt
    SELF.PrtPrev:CurrentPage = ?CurrentPage
    SELF.PrtPrev:PagesAcross = ?PagesAcross
    SELF.PrtPrev:PagesDown   = ?PagesDown
    SELF.PrtPrev:Next        = ?Next
    SELF.PrtPrev:Previous    = ?Previous
    SELF.PrtPrev:Jump        = ?Jump
    SELF.PrtPrev:ChangeDisplay = ?ChangeDisplay
    SELF.PrtPrev:ViewMenu    = ?ViewMenu
    SELF.PrtPrev:ZoomMenu    = ?ZoomMenu
    SELF.PrtPrev:LastCtrl    = ?ZoomList
    SELF.PrtPrev:LastDynCtrl = SELF.PrtPrev:LastCtrl
    
    IF NOT SELF.AllowSendReport
      HIDE (?ProcFiles)
    END

    IF NOT SELF.TargetSelector &= NULL
      CASE SELF.TargetSelector.Items (TRUE)
      OF 0
        DISABLE (?SaveAs)
      OF 1
        ?SaveAs{PROP:Text} = 'Save As ' & SELF.TargetSelector.GetOutputGeneratorName (1, TRUE)
      END
    ELSE
      DISABLE (?SaveAs)
    END

    IF SELF.AllowPrintCurrent
      UNHIDE (?PrintCurrent)    
    END

    PreviewWindow {PROP:Pixels} = TRUE
    SELF.Ask()
  
    CLOSE (PreviewWindow)
  END

  IF SELF.PrintOk
    SELF.SyncImageQueue()
  END
  RETURN SELF.PrintOK


!This method synchronizes the image queue with the contents of PageToPrint, ensuring that only the selected pages
!are sent to the printer

PrintPreviewClass.SyncImageQueue PROCEDURE()

i LONG,AUTO

  CODE
  LOOP i = RECORDS(SELF.ImageQueue) TO 1 BY -1
    IF NOT SELF.InPageList(i)
      SELF.DeleteImageQueue(i)
    END
  END
  RETURN


PrintPreviewClass.TakeCurrentPage   PROCEDURE()

_PrintOneQueue QUEUE(PreviewQueue)
               END
_OnePageReport REPORT('')
               END

  CODE
  OPEN (_OnePageReport)

  _OnePageReport{PROP:Text}    = SELF.OnePageReportJobName
  _OnePageReport{PROP:Preview} = _PrintOneQueue

  ENDPAGE (_OnePageReport)
  FREE (_PrintOneQueue)

  GET (SELF.ImageQueue, SELF.CurrentPage)

  IF ERRORCODE() = 0
    _PrintOneQueue.FileName = SELF.ImageQueue.FileName
    ADD(_PrintOneQueue)

    _OnePageReport{PROP:FlushPreview} = TRUE
    FREE(_PrintOneQueue)
  END

  CLOSE (_OnePageReport)
  RETURN


!This method removes an image from the image queue and also, remove ths associated image meta file

PrintPreviewClass.DeleteImageQueue PROCEDURE (LONG i)
  CODE
  IF POINTER (SELF.ImageQueue) <> i
    GET (SELF.ImageQueue, i)
    IF ERRORCODE() <> 0
      RETURN FALSE
    END
  END

  REMOVE (SELF.ImageQueue.Filename)
? ASSERT(ERRORCODE() = 0)
  DELETE (SELF.ImageQueue)
? ASSERT (ERRORCODE() = 0)
  RETURN TRUE


PrintPreviewClass.SetDefaultPages PROCEDURE()
  CODE
  SELF.PagesToPrint = '1-' & RECORDS(SELF.ImageQueue)
  RETURN


PrintPreviewClass.Open PROCEDURE()

i   UNSIGNED,AUTO

  CODE
  IF NOT SELF.Maximize
     SELF.InitWindowPosition (SELF.PreviewWindow)
  END

  GET (SELF.ImageQueue,1)
? ASSERT(ERRORCODE() = 0)

  SELF.PageManager &= NEW PageManagerClass                 !Root PageManager
  SELF.PageManager.Init (SELF, 1, BaseFeq, SELF.ImageQueue.Filename)

  IF RECORDS(SELF.ImageQueue) > 1
    ENABLE (SELF.PrtPrev:ViewMenu)
    SELF.PrtPrev:CurrentPage{PROP:RangeHigh} = RECORDS(SELF.ImageQueue)
    SELF.PrtPrev:CurrentPage{PROP:Msg}       = EnterStr & ' ' & RECORDS(SELF.ImageQueue)
  ELSE
    DISABLE (SELF.PrtPrev:CurrentPage, SELF.PrtPrev:PagesDown)
  END

  LOOP i = 1 TO RECORDS(SELF.ZoomQueue)
    GET (SELF.ZoomQueue, i)
?   ASSERT(ERRORCODE() = 0)

    CREATE (ZoomItem+i, CREATE:Item, SELF.PrtPrev:ZoomMenu, i)
    (ZoomItem+i){PROP:Use}   = SELF.mUse[i]
    (ZoomItem+i){PROP:Text}  = CLIP (SELF.ZoomQueue.MenuText)
    (ZoomItem+i){PROP:Check} = TRUE

    SELF.mUse[i] = CHOOSE (SELF.ZoomQueue.Percentile = DefaultZoom)
    SELF.Popup.AddItemEvent ('ZoomItem' & i, EVENT:Accepted, ZoomItem+i)
  END

  SELF.PrtPrev:ZoomList{PROP:From} = SELF.ZoomQueue.MenuText

  IF SELF.AllowUserZoom
    SELF.ResetUserZoom()
    UNHIDE (SELF.PrtPrev:ZoomList)
    UNHIDE (SELF.PrtPrev:ZoomPrompt)
  END

  SELF.ZoomIndex = Percentile2Idx (SELF, SELF.InitZoom)

  IF SELF.ZoomIndex > RECORDS(SELF.ZoomQueue)
?   ASSERT (SELF.AllowUserZoom)
    SELF.ZoomIndex      = 0
    SELF.UserPercentile = SELF.InitZoom
  END

  IF NOT SELF.Translator &= NULL
    SELF.Popup.SetTranslator (SELF.Translator)
  END

  SELF.NewZoom()
  SETCURSOR
  RETURN


PrintPreviewClass.ProcessResultFiles PROCEDURE (*OutputFileQueue OutputFile)
  CODE
  RETURN Level:Benign


PrintPreviewClass.TakeAccepted PROCEDURE()

c           UNSIGNED,AUTO
ZoomValue   SIGNED,AUTO

  CODE
  IF PARENT.TakeAccepted()
    RETURN Level:Notify
  END

  CASE FIELD()
  OF ZoomItem+1 TO ZoomItem+RECORDS(SELF.ZoomQueue)
    ZoomValue      = SELF.ZoomIndex
    SELF.ZoomIndex = ACCEPTED() - (ZoomItem+1) + 1

    IF SELF.ZoomIndex <> ZoomValue
      SELF.NewZoom()
    END
  OF SELF.PrtPrev:TBarZoom
    SELF.ConfigMenuChecks()
    SELF.Popup.Ask()
  OF SELF.PrtPrev:MenuPages
    SELF.AskPrintPages()
    SELF.DrawPage()
  OF SELF.PrtPrev:MenuPrintCurrent
    SELF.TakeCurrentPage()
  OF SELF.PrtPrev:MenuPrint
  OROF SELF.PrtPrev:TBarPrint
    SELF.PrintOk = CHOOSE (NOT SELF.ConfirmPages OR SELF.AskPrintPages())
    POST (EVENT:CloseWindow)
  OF SELF.PrtPrev:MenuExit
  OROF SELF.PrtPrev:TBarExit
    POST (EVENT:CloseWindow)
  OF SELF.PrtPrev:ZoomList
    LOOP c = 1 TO RECORDS(SELF.ZoomQueue)
      GET (SELF.ZoomQueue, c)
?     ASSERT(ERRORCODE() = 0)

      IF SELF.ZoomQueue.MenuText = SELF.ZoomList
        POST (EVENT:Accepted, ZoomItem + CHOICE (SELF.PrtPrev:ZoomList))
        RETURN Level:Notify
      END
    END

    ZoomValue = SELF.ZoomList
    IF ZoomValue < 0
      ZoomValue = -ZoomValue
    END

    SELF.ZoomList       = ZoomValue
    SELF.UserPercentile = ZoomValue
    SELF.ZoomIndex      = 0
    SELF.NewZoom()
  OF SELF.PrtPrev:CurrentPage
    IF Idx2Percentile (SELF, SELF.ZoomIndex) <> PageWidth
      c = SELF.PagesAcross * SELF.PagesDown

      IF SELF.CurrentPage < SELF.FirstPage OR SELF.CurrentPage >= SELF.FirstPage + c
        SELF.FirstPage = SELF.CurrentPage
        c = RECORDS(SELF.ImageQueue) - c + 1     !Max index of first displayed page

        IF SELF.FirstPage > c
          SELF.FirstPage = c
        END
        IF SELF.FirstPage < 1
          SELF.FirstPage = 1
        END
      ELSIF SELF.PageManager.Exists (SELF.CurrentPage)
        SELF.PageManager.HighLight (SELF.CurrentPage)
        RETURN Level:Benign
      END
    END
    SELF.DrawPage()
  OF SELF.PrtPrev:PagesAcross
    c = RECORDS(SELF.ImageQueue) - SELF.PagesAcross * SELF.PagesDown + 1

    LOOP WHILE SELF.FirstPage > c AND SELF.PagesAcross > 1
      SELF.PagesAcross -= 1
      c += SELF.PagesDown
    END
    SELF.DrawPage()
  OF SELF.PrtPrev:PagesDown
    c = RECORDS(SELF.ImageQueue) - SELF.PagesAcross * SELF.PagesDown + 1

    LOOP WHILE SELF.FirstPage > c AND SELF.PagesDown > 1
      SELF.PagesDown -= 1
      c += SELF.PagesAcross
    END
    SELF.DrawPage()
  OF SELF.PrtPrev:Next
  OROF SELF.PrtPrev:Previous
    SELF.CurrentPage += CHOOSE (ACCEPTED() = SELF.PrtPrev:Next, 1, -1)
    POST (EVENT:Accepted, SELF.PrtPrev:CurrentPage)
    RETURN Level:Notify
  OF SELF.PrtPrev:Jump
    IF SELF.AskPage()
      POST (EVENT:Accepted, SELF.PrtPrev:CurrentPage)
    END
  OF SELF.PrtPrev:ChangeDisplay
    SELF.AskThumbnails()
    SELF.CurrentPage = 1
    SELF.DrawPage()
  OF SELF.PrtPrev:MenuSaveAs
    RETURN SELF.OnSaveAs()
  OF SELF.PrtPrev:MenuProcFiles
    RETURN SELF.OnProcessFiles()     
  END
  RETURN Level:Benign


PrintPreviewClass.ParseImages  PROCEDURE (BOOL force)

ReturnValue    BYTE,AUTO

  CODE
  IF SELF.ReportTarget.SupportResultQueue()
    SELF.ReportTarget.SetResultQueue (SELF.OutputFileQueue)
  END

  ! The true parameter means that the AskProperties must ask for a name
  ! even if the name was set at the beginning

  IF SELF.ReportTarget.AskProperties (force) = Level:Benign
    SELF.WMFParser.Init (SELF.ImageQueue, SELF.ReportTarget, SELF.Errors)
    ReturnValue = SELF.WMFParser.GenerateReport (SELF.ShowOutputProgress)

    IF ReturnValue <> Level:Benign
      RETURN ReturnValue
    END

    IF SELF.ReportTarget.SupportResultQueue()
      RETURN SELF.ProcessResultFiles (SELF.OutputFileQueue)
    END
  END
  RETURN Level:Benign


PrintPreviewClass.OnSaveAs  PROCEDURE()
  CODE
  IF SELF.TargetSelector &= NULL OR NOT SELF.TargetSelector.Ask (FALSE, TRUE)
    RETURN Level:Benign
  END

  SELF.ReportTarget &= SELF.TargetSelector.GetReportSelected()
  RETURN SELF.ParseImages (TRUE)

  
PrintPreviewClass.OnProcessFiles  PROCEDURE()
  CODE
  !The selected SELF.ReportTarget must be set
  IF SELF.TargetSelector &= NULL OR SELF.TargetSelector.GetDefaultItemIndex() = 0
    RETURN Level:Benign
  END

  SELF.ReportTarget &= SELF.TargetSelector.GetReportDefault()
  RETURN SELF.ParseImages (FALSE)

    
PrintPreviewClass.TakeWindowEvent  PROCEDURE()
  CODE
  IF EVENT() <> EVENT:GainFocus
    RETURN PARENT.TakeWindowEvent()
  END
  RETURN Level:Benign


PrintPreviewClass.TakeEvent  PROCEDURE()

Pg         LONG,AUTO
Mx         SIGNED,AUTO
My         SIGNED,AUTO
val1       SIGNED,AUTO
val2       SIGNED,AUTO
val3       SIGNED,AUTO
Cx         BYTE,AUTO
Cy         BYTE,AUTO
ZoomIndex  BYTE,AUTO

  CODE
  IF PARENT.TakeEvent() <> Level:Benign
    RETURN Level:Notify
  END

  CASE EVENT()
  OF EVENT:CloseWindow
    IF NOT SELF.INIMgr &= NULL
      SELF.INIMgr.Update ('_PreviewWindow_', SELF.PreviewWindow)
    END
    IF NOT SELF.PageManager &= NULL
      SELF.PageManager.Kill()
      DISPOSE (SELF.PageManager)
      SELF.PageManager &= NULL
    END
  OF EVENT:Sized
    SELF.SaveSize.Set = TRUE
    GETPOSITION (SELF.PreviewWindow, SELF.SaveSize.XPos,  |
                                     SELF.SaveSize.YPos,  |
                                     SELF.SaveSize.Width, |
                                     SELF.SaveSize.Height)
    IF NOT SELF.PageManager &= NULL
       SELF.DrawPage()
    END
  OF EVENT:AlertKey
    CASE KEYCODE()
    OF PgUpKey
      IF SELF.CurrentPage > 1 AND NOT SELF.PrtPrev:Previous {PROP:Disable}
        POST (EVENT:Accepted, SELF.PrtPrev:Previous)
      END
    OF PgDnKey
      IF SELF.CurrentPage < RECORDS(SELF.ImageQueue) AND NOT SELF.PrtPrev:Next{PROP:Disable}
        POST (EVENT:Accepted, SELF.PrtPrev:Next)
      END
    OF MouseLeft
      DO GetCoord
      IF Pg <> 0
        SELF.CurrentPage = Pg

        IF Idx2Percentile (SELF, SELF.ZoomIndex) = NoZoom
          ZoomIndex = Percentile2Idx (SELF, PageWidth)

          IF ZoomIndex <= RECORDS(SELF.ZoomQueue)
            val1 = SELF.PageManager.GetProp (Pg, PROP:YPos)
            val2 = SELF.PageManager.GetProp (Pg, PROP:Height)
            Cy   = MulDiv (My - val1, 100, val2)

            SELF.ZoomIndex = ZoomIndex
            SELF.PageManager.SetCentre (Pg, 0, Cy)
            SELF.NewZoom()
          END
        ELSE
          ZoomIndex = Percentile2Idx (SELF, NoZoom)

          IF ZoomIndex <= RECORDS(SELF.ZoomQueue)
            SELF.ZoomIndex = ZoomIndex
            SELF.NewZoom()
          END
        END
      END
    OF MouseRightUp
      DO GetCoord
      IF Pg <> 0
        IF Idx2Percentile (SELF, SELF.ZoomIndex) = NoZoom
          val1 = SELF.PageManager.GetProp (Pg, PROP:XPos)
          val2 = SELF.PageManager.GetProp (Pg, PROP:Width)
          Cx   = MulDiv (Mx - val1, 100, val2)

          val1 = SELF.PageManager.GetProp (Pg, PROP:YPos)
          val2 = SELF.PageManager.GetProp (Pg, PROP:Height)
          Cy   = MulDiv (My - val1, 100, val2)
        ELSE
          val1 = SELF.PageManager.GetProp (Pg, PROP:Height)

          IF SELF.PageManager.GetProp (Pg, PROP:VScroll)
            val2 = SELF.PageManager.GetProp (Pg, PROP:MaxHeight)
            val3 = SELF.PageManager.GetProp (Pg, PROP:VScrollPos)
            val1 = MulDiv (val2 - val1, val3, 255)    !Scrolled amount

            Cy = MulDiv (My + val1, 100, val2)
          ELSE
            Cy = MulDiv (My, 100, val1)
          END

          val1 = SELF.PageManager.GetProp (Pg, PROP:Width)

          IF SELF.PageManager.GetProp (Pg, PROP:HScroll)
            val2 = SELF.PageManager.GetProp (Pg, PROP:MaxWidth)
            val3 = SELF.PageManager.GetProp (Pg, PROP:HScrollPos)
            val1 = MulDiv (val2 - val1, val3, 255)    !Scrolled amount

            Cx = MulDiv (Mx + val1, 100, val2)
          ELSE
            Cx = MulDiv (Mx, 100, val1)
          END
        END

        SELF.PageManager.SetCentre (Pg, Cx, Cy)

        IF SELF.Popup.Ask()
          SELF.CurrentPage = Pg
        ELSE
          SELF.PageManager.SetCentre (Pg, 0, 0)
        END
      END
    END
  END

  DO DisplayWindow
  RETURN Level:Benign

GetCoord ROUTINE
  Mx = MOUSEX()
  My = MOUSEY()
  Pg = SELF.PageManager.CoordContained (Mx, My)
  EXIT

DisplayWindow ROUTINE
  DATA
ZoomMode   SIGNED,AUTO
NumImages  LONG,AUTO
  CODE
  SELF.ConfigMenuChecks()
  ZoomMode  = Idx2Percentile (SELF, SELF.ZoomIndex)
  NumImages = RECORDS (SELF.ImageQueue)

  SELF.PrtPrev:Previous{PROP:Disable} = CHOOSE (SELF.CurrentPage = 1)
  SELF.PrtPrev:Next    {PROP:Disable} = CHOOSE (SELF.CurrentPage = NumImages)

  IF NumImages <= 1
    DISABLE (SELF.PrtPrev:Jump)
    DISABLE (SELF.PrtPrev:ChangeDisplay)
    DISABLE (SELF.PrtPrev:PagePrompt, SELF.PrtPrev:PagesDown)

    IF ZoomMode = NoZoom
      DISABLE (SELF.PrtPrev:ChangeDisplay)
    END
  ELSE
    ENABLE (SELF.PrtPrev:Jump)
    ENABLE (SELF.PrtPrev:ChangeDisplay)

    IF ZoomMode = NoZoom
      ENABLE (SELF.PrtPrev:ChangeDisplay)
      ENABLE (SELF.PrtPrev:PagesAcross, SELF.PrtPrev:PagesDown)
    ELSE
      DISABLE (SELF.PrtPrev:PagesAcross, SELF.PrtPrev:PagesDown)
    END
  END

  IF ZoomMode <> NoZoom
    DISABLE (SELF.PrtPrev:ChangeDisplay)
  END

  IF ZoomMode <> NoZoom OR SELF.ThumbnailsPresent <= 1
    SELF.PreviewWindow {PROP:StatusText, 2} = PageStr & ' ' & SELF.CurrentPage & ' ' & |
                                              OfStr & ' ' & NumImages
  ELSE
    SELF.PreviewWindow {PROP:StatusText, 2} = PageStr & ' ' & SELF.FirstPage & '-' & |
                                              SELF.FirstPage + SELF.ThumbnailsPresent - 1 & ' ' & |
                                              OfStr & ' ' & NumImages
  END

  IF SELF.ZoomIndex = 0
    SELF.PreviewWindow {PROP:StatusText, 3} = SBZoom & ' ' & SELF.UserPercentile & '%'
  ELSE
    SELF.PreviewWindow {PROP:StatusText, 3} = SELF.ZoomQueue.StatusText
  END

  DISPLAY (SELF.PrtPrev:CurrentPage, SELF.PrtPrev:PagesDown)
  DISPLAY (SELF.PrtPrev:Next, SELF.PrtPrev:ChangeDisplay)
  EXIT


PrintPreviewClass.TakeFieldEvent PROCEDURE()
  CODE
  CASE EVENT()
  OF EVENT:NewSelection
    CASE FIELD()
    OF SELF.PrtPrev:CurrentPage
    OROF SELF.PrtPrev:PagesAcross
    OROF SELF.PrtPrev:PagesDown
      POST (EVENT:Accepted, FIELD())
      RETURN Level:Notify
    END
  END
  RETURN Level:Benign


PrintPreviewClass.NewZoom PROCEDURE()
  CODE
  SELF.ResetUserZoom()
  SELF.DrawPage()
  SELF.ConfigMenuChecks()
  RETURN


PrintPreviewclass.ConfigMenuChecks PROCEDURE()

c          BYTE,AUTO
N          BYTE,AUTO
cur        BYTE,AUTO

  CODE
  N = RECORDS(SELF.ZoomQueue)

  LOOP c = 1 TO N
    cur = CHOOSE (c = SELF.ZoomIndex)

    SELF.Popup.SetItemCheck ('ZoomItem' & c, cur)
    SELF.mUse [c] = cur
  END

  DISPLAY (ZoomItem + 1, ZoomItem + N)
  RETURN


PrintPreviewClass.ResetUserZoom PROCEDURE()
  CODE
  IF SELF.AllowUserZoom
    IF SELF.ZoomIndex <> 0
      GET (SELF.ZoomQueue, SELF.ZoomIndex)
?     ASSERT(ERRORCODE() = 0)

      SELF.ZoomList = SELF.ZoomQueue.MenuText
    ELSE
      SELF.ZoomList = CLIP(SELF.ZoomList) & PCZoom
    END
    DISPLAY (SELF.PrtPrev:ZoomList)
  END
  RETURN


PrintPreviewClass.DrawPage PROCEDURE()

Ev         UNSIGNED,AUTO
ZoomMode   SIGNED,AUTO  
Page       LONG,AUTO
  
  CODE
  Ev       = EVENT()
  ZoomMode = Idx2Percentile (SELF, SELF.ZoomIndex)

  IF ZoomMode = NoZoom
    DO DrawThumbnails
  ELSE
    Page = SELF.CurrentPage

    IF Ev <> EVENT:Sized
      SELF.PageManager.Delete()

      IF NOT SELF.PageManager.Exists (Page)
        DO AddNewPageManager
      END
    END

    SELF.PageManager.Draw (Page, ZoomMode)

    IF Ev <> EVENT:Sized
      SELF.PageManager.HighLight (Page)
    END
  END
  RETURN

DrawThumbnails ROUTINE
  DATA
WWidth     SIGNED,AUTO
WHeight    SIGNED,AUTO

PWidth     SIGNED,AUTO
PHeight    SIGNED,AUTO
PX         SIGNED,AUTO
PY         SIGNED,AUTO

Pages      UNSIGNED,AUTO
NumImages  UNSIGNED,AUTO

TRow       UNSIGNED,AUTO
TCol       UNSIGNED,AUTO
  CODE
  NumImages = RECORDS(SELF.ImageQueue)

  IF Ev <> EVENT:Sized
    Pages = SELF.PagesAcross * SELF.PagesDown

    LOOP Page = 1 TO NumImages
      IF Page < SELF.FirstPage OR Page >= SELF.FirstPage + Pages
        IF SELF.PageManager.Exists (Page)
          SELF.PageManager.Delete (Page)
        END
      END
    END
  END

  ! Width and height of area to draw pages in

  WWidth  = SELF.PreviewWindow {PROP:ClientWidth}
  WHeight = SELF.PreviewWindow {PROP:ClientHeight}

  ! Width and height thumbnailed pages

  PWidth  = (WWidth - (SELF.PagesAcross + 1) * MinXSeperation) / SELF.PagesAcross
  PHeight = (WHeight - (SELF.PagesDown + 1) * MinYSeperation) / SELF.PagesDown

  ! Place pages

  SELF.ThumbnailsPresent = 0

  PX   = MinXSeperation
  PY   = MinYSeperation
  TRow = 1
  TCol = 1
  Page = SELF.FirstPage

  LOOP WHILE Page <= NumImages
    IF NOT SELF.PageManager.Exists (Page)
      DO AddNewPageManager
    END

    SELF.PageManager.SetPosition (Page, PX, PY, PWidth, PHeight)
    SELF.PageManager.Draw (Page, NoZoom)

    SELF.ThumbnailsPresent += 1

    IF TCol = SELF.PagesAcross
      IF TRow = SELF.PagesDown
        BREAK
      END

      TRow += 1
      TCol  = 1
      PX    = MinXSeperation
      PY   += MinYSeperation + SELF.PageManager.GetProp (Page, PROP:Height)
    ELSE
      TCol += 1
      PX   += MinXSeperation + SELF.PageManager.GetProp (Page, PROP:Width)
    END

    Page += 1
  END

  SELF.PageManager.HighLight (SELF.CurrentPage)
  EXIT

AddNewPageManager ROUTINE
  DATA
OldRoot    &PageManagerClass,AUTO
  CODE
  OldRoot &= SELF.PageManager

  GET (SELF.ImageQueue, Page)
? ASSERT(ERRORCODE() = 0)

  SELF.PageManager &= New PageManagerClass
  SELF.PageManager.Init (SELF, Page, BaseFeq + Page * 2 - 2, SELF.ImageQueue.Filename)
  SELF.PageManager.Neighbour &= OldRoot
  EXIT


PrintPreviewClass.SetZoomPercentile PROCEDURE (UNSIGNED Percentile)

Zoom       UNSIGNED,AUTO

  CODE
  SELF.ZoomIndex = 0

  LOOP Zoom = 1 TO RECORDS(SELF.ZoomQueue)
    GET (SELF.ZoomQueue, Zoom)
?   ASSERT(ERRORCODE() = 0)

    IF SELF.ZoomQueue.Percentile = Percentile
      SELF.ZoomIndex      = Zoom
      SELF.UserPercentile = 0
      RETURN
    END
  END

? ASSERT (SELF.AllowUserZoom)
  SELF.UserPercentile = Percentile
  RETURN


PrintPreviewClass.AskPage  PROCEDURE()

JumpPage   LONG,AUTO
RVal       BYTE(FALSE)

JumpWindow WINDOW('Jump to a Report Page'),AT(,,181,26),FONT('MS Shell Dlg',10,,,CHARSET:Default),CENTER,STATUS,GRAY, |
         DOUBLE
       PROMPT('&Page:'),AT(5,8),USE(?JumpPrompt)
       SPIN(@n5),AT(30,7,50,12),MSG('Select a page of the report'),USE(JumpPage),RANGE(1,10),STEP(1)
       BUTTON('OK'),AT(89,7,40,12),MSG('Jump to the selected page'),USE(?OKButton),DEFAULT
       BUTTON('Cancel'),AT(134,7,40,12),MSG('Cancel this selection'),USE(?CancelButton)
     END

  CODE
  JumpPage = SELF.CurrentPage

  IF SELF.RTLLayout
    SETLAYOUT (JumpWindow, 1)
  END

  OPEN (JumpWindow)

  IF NOT SELF.Translator &= NULL
    SELF.Translator.TranslateWindow (JumpWindow)
  END

  ACCEPT
    CASE EVENT()
    OF EVENT:OpenWindow
      ?JumpPage{PROP:RangeHigh} = RECORDS(SELF.ImageQueue)
    OF EVENT:Accepted
      CASE ACCEPTED()
      OF ?OKButton
        IF JumpPage <> SELF.CurrentPage
          RVal = TRUE                            !RVal=true if SELF.CurrentPage has changed
          SELF.CurrentPage = JumpPage
        END
      OROF ?CancelButton
        POST(EVENT:CloseWindow)
      END
    END
  END

  CLOSE (JumpWindow)
  RETURN RVal


PrintPreviewClass.AskThumbnails PROCEDURE()

SelectWindow WINDOW('Change the Report Display'),AT(,,141,64),FONT('MS Shell Dlg',10,,,CHARSET:Default),GRAY,DOUBLE
       GROUP('Pages Displayed'),AT(3,2,135,43),USE(?Group1),BOXED
         GROUP('Across'),AT(7,10,62,32),BOXED
           SPIN(@N2),AT(13,22,50,12),USE(SELF.PagesAcross,,?PagesAcross),HSCROLL,RANGE(1,10),STEP(1)
         END
         GROUP('Down'),AT(72,10,62,32),BOXED
           SPIN(@N2),AT(79,22,50,12),USE(SELF.PagesDown,,?PagesDown),HVSCROLL,RANGE(1,10),STEP(1)
         END
       END
       BUTTON('OK'),AT(98,47,40,14),KEY(EnterKey),USE(?OK)
     END

  CODE
  IF SELF.RTLLayout
    SETLAYOUT (SelectWindow, 1)
  END

  OPEN (SelectWindow)

  IF NOT SELF.Translator &= NULL
    SELF.Translator.TranslateWindow (SelectWindow)
  END

  ACCEPT
    CASE EVENT()
    OF EVENT:Accepted
      CASE FIELD()
      OF ?OK
        IF SELF.PagesAcross * SELF.PagesDown > RECORDS(SELF.ImageQueue)
          SELECT (?PagesAcross)
        ELSE
          POST (EVENT:CloseWindow)
        END
      END
    END
  END

  CLOSE (SelectWindow)
  RETURN


PrintPreviewClass.AskPrintPages PROCEDURE()

Preserve   LIKE(PrintPreviewClass.PagesToPrint),AUTO

PageToPrintWindow WINDOW('Pages to Print'),AT(,,260,37),FONT('MS Shell Dlg',8,,,CHARSET:Default),CENTER,SYSTEM,GRAY,RESIZE
       PROMPT('&Pages to Print:'),AT(4,8),USE(?Prompt)
       ENTRY(@s255),AT(56,4,200,11),USE(SELF.PagesToPrint, , ?PagesToPrint)
       BUTTON('&Reset'),AT(116,20,45,14),USE(?Reset)
       BUTTON('&Ok'),AT(164,20,45,14),USE(?Ok),DEFAULT
       BUTTON('&Cancel'),AT(212,20,45,14),USE(?Cancel),STD(STD:Close)
     END

RVal       BYTE,AUTO

  CODE
  Preserve = SELF.PagesToPrint

  IF SELF.RTLLayout
    SETLAYOUT (PageToPrintWindow, 1)
  END
  
  OPEN (PageToPrintWindow)

  IF NOT SELF.Translator &= NULL
    SELF.Translator.TranslateWindow (PageToPrintWindow)
  END
  IF NOT SELF.INIMgr &= NULL
    SELF.INIMgr.Fetch ('_PreviewAskPagesWindow_', PageToPrintWindow)
  END

  ACCEPT
    CASE EVENT()
    OF EVENT:Accepted
      CASE ACCEPTED()
      OF ?Cancel
        SELF.PagesToPrint = Preserve
        RVal = FALSE
        POST (EVENT:CloseWindow)
      OF ?Ok
        RVal = TRUE
        POST (EVENT:CloseWindow)
      OF ?Reset
        SELF.SetDefaultPages()
        SELECT (?PagesToPrint)
      END
    OF EVENT:CloseWindow
      IF NOT SELF.INIMgr &= NULL
        SELF.INIMgr.Update ('_PreviewAskPagesWindow_', PageToPrintWindow)
      END
    END
  END

  CLOSE (PageToPrintWindow)
  RETURN RVal


PrintPreviewClass.InPageList PROCEDURE (LONG PageN)

i          LONG(1)
j          LONG(0)
Lo         LONG,AUTO
Hi         LONG,AUTO
L          UNSIGNED,AUTO
Pos        UNSIGNED,AUTO
Itm        CSTRING(256),AUTO

  CODE
  L = LEN (CLIP (SELF.PagesToPrint))

  IF L <> 0
    LOOP
      IF i = L
        Itm = SELF.PagesToPrint [j + 1 : L]
      ELSIF SELF.PagesToPrint[i] = ','
        Itm = SELF.PagesToprint [j + 1 : i - 1]
        j = i
      END

      Pos = INSTRING ('-', Itm, 1, 1)

      IF Pos <> 0
        Lo = SUB (Itm, 1, Pos - 1)
        Hi = SUB (Itm, Pos + 1, LEN(Itm) - Pos)

        IF Lo > 0 AND Hi > 0 AND Lo <= Hi AND INRANGE (PageN, Lo, Hi)
          RETURN TRUE
        END
      ELSIF NUMERIC(Itm) AND PageN = Itm
        RETURN TRUE
      END
      i += 1
    WHILE i <= L
  END
  RETURN FALSE

! ==============================================================================

PageManagerClass.Init PROCEDURE (*PrintPreviewClass P, |
                                 LONG    PageNo,   |
                                 SIGNED  pBaseFeq, |
                                 *STRING FName)

TempImage     SIGNED,AUTO

  CODE
  SELF.PP &= P
  SELF.FileName &= NEW STRING(LEN(CLIP(FName)))
  SELF.FileName  = FName

  SELF.PageNo    = PageNo
  SELF.BorderFeq = pBaseFeq
  SELF.ImageFeq  = pBaseFeq + 1
  SELF.Created   = FALSE

  TempImage = CREATE (0, CREATE:Image)
  TempImage {PROP:Text} = SELF.Filename

  SELF.ImageWidth  = TempImage{PROP:MaxWidth}
  SELF.ImageHeight = TempImage{PROP:MaxHeight}
  DESTROY (TempImage)
  RETURN


PageManagerClass.Init PROCEDURE (*PrintPreviewClass P, |
                                 LONG    PageNo,   |
                                 SIGNED  pBaseFeq, |
                                 *STRING FName,    |
                                 SIGNED X, SIGNED Y, SIGNED W, SIGNED H)

  CODE
  SELF.Init (P, PageNo, pBaseFeq, FName)
  SELF.SetPosition (SELF.PageNo, X, Y, W, H)
  RETURN


PageManagerClass.SetPosition PROCEDURE (LONG PageNo, SIGNED X, SIGNED Y, SIGNED W, SIGNED H)

THeight    SIGNED,AUTO  
  
  CODE
  IF SELF.PageNo = PageNo
    THeight = MulDiv (W, SELF.ImageHeight, SELF.ImageWidth)

    IF THeight > H
      W = MulDiv (H, SELF.ImageWidth, SELF.ImageHeight)
    ELSE
      H = THeight
    END

    SELF.XPos   = X
    SELF.YPos   = Y
    SELF.Width  = W
    SELF.Height = H

    IF SELF.Created
      SETPOSITION (SELF.BorderFeq, X - 1, Y - 1, W + 2, H + 2)
      SETPOSITION (SELF.ImageFeq,  X, Y, W, H)
    END
  ELSIF NOT SELF.Neighbour &= NULL
    SELF.Neighbour.SetPosition (PageNo, X, Y, W, H)
  END
  RETURN


PageManagerClass.Draw PROCEDURE (LONG PageNo, SIGNED ZoomFactor)

TWidth        SIGNED,AUTO
THeight       SIGNED,AUTO
WWidth        SIGNED,AUTO
WHeight       SIGNED,AUTO
X             SIGNED,AUTO
Y             SIGNED,AUTO
Created       BOOL,AUTO
HScroll       BOOL,AUTO
VScroll       BOOL,AUTO

  CODE
  IF SELF.PageNo <> PageNo
    IF NOT SELF.Neighbour &= NULL
      SELF.Neighbour.Draw (PageNo, ZoomFactor)
    END
    RETURN
  END

  Created = FALSE

  IF NOT SELF.Created
    CREATE (SELF.BorderFeq, CREATE:Box)
    CREATE (SELF.ImageFeq,  CREATE:Image)
    SELF.Created = TRUE
    Created      = TRUE

    SELF.BorderFeq {PROP:Color}    = COLOR:Black
    SELF.BorderFeq {PROP:Fill}     = COLOR:White
    SELF.ImageFeq  {PROP:Cursor}   = CURSOR:Zoom
    SELF.ImageFeq  {PROP:Alrt,250} = MouseLeft
    SELF.ImageFeq  {PROP:Text}     = SELF.FileName
  END

  IF ZoomFactor = NoZoom
    SELF.ImageFeq {PROP:MaxWidth}  = SELF.Width
    SELF.ImageFeq {PROP:MaxHeight} = SELF.Height
    SELF.ImageFeq {PROP:VScroll}   = FALSE
    SELF.ImageFeq {PROP:HScroll}   = FALSE

    IF NOT SELF.PP.InPageList (SELF.PageNo)
      SELF.BorderFeq {PROP:Fill} = COLOR:Gray
      SELF.ImageFeq  {PROP:Text} = ''
    ELSIF NOT Created
      SELF.BorderFeq {PROP:Fill} = COLOR:White
      SELF.ImageFeq  {PROP:Text} = SELF.FileName
    END

    SETPOSITION (SELF.BorderFeq, SELF.XPos - 1, SELF.YPos - 1, SELF.Width + 2, SELF.Height + 2)
    SETPOSITION (SELF.ImageFeq,  SELF.XPos,     SELF.YPos,     SELF.Width,     SELF.Height)
  ELSE
    WWidth  = 0{PROP:ClientWidth} - MinXSeperation
    WHeight = 0{PROP:ClientHeight} - MinYSeperation

    IF ZoomFactor = PageWidth
      TWidth  = WWidth
      THeight = MulDiv (WWidth, SELF.ImageHeight, SELF.ImageWidth)
    ELSE
      TWidth  = MulDiv (SELF.ImageWidth,  ZoomFactor, 100)
      THeight = MulDiv (SELF.ImageHeight, ZoomFactor, 100)
    END

    SELF.ImageFeq {PROP:MaxWidth}  = TWidth
    SELF.ImageFeq {PROP:MaxHeight} = THeight

    IF THeight > WHeight
      THeight = WHeight
      VScroll = TRUE
    ELSE
      VScroll = FALSE
    END

    IF VScroll AND ZoomFactor <> PageWidth
      TWidth += GetSystemMetrics (SM_CXVSCROLL)
    END

    IF TWidth > WWidth
      TWidth  = WWidth
      HScroll = TRUE
    ELSE
      HScroll = FALSE
    END

    X = MinXSeperation / 2
    Y = MinYSeperation / 2

    SETPOSITION (SELF.BorderFeq, X - 1, Y - 1, TWidth + 2, THeight + 2)
    SETPOSITION (SELF.ImageFeq,  X, Y, TWidth, THeight)

    SELF.ImageFeq {PROP:HScroll} = HScroll
    SELF.ImageFeq {PROP:VScroll} = VScroll

    IF EVENT() <> EVENT:Sized
      IF VScroll
        SELF.ImageFeq {PROP:VScrollPos} = SELF.CentreOnY
      END
      IF HScroll
        SELF.ImageFeq {PROP:HScrollPos} = SELF.CentreOnX
      END
    END
  END

  SELF.CentreOnX = 0
  SELF.CentreOnY = 0
  SELF.ZoomState = ZoomFactor

  IF Created
    UNHIDE (SELF.BorderFeq)
    UNHIDE (SELF.ImageFeq)
  END
  RETURN


PageManagerClass.CoordContained PROCEDURE (SIGNED XPos, SIGNED YPos)

X          SIGNED,AUTO
Y          SIGNED,AUTO
H          SIGNED,AUTO
W          SIGNED,AUTO  
  
  CODE
  IF SELF.Created
    GETPOSITION (SELF.ImageFeq, X, Y, W, H)

    IF (XPos >= X AND XPos < X + W) AND |
       (YPos >= Y AND YPos < Y + H)
      RETURN SELF.PageNo
    END
  END

  IF NOT SELF.Neighbour &= NULL
    RETURN SELF.Neighbour.CoordContained (XPos, YPos)
  END
  RETURN 0


PageManagerClass.Delete PROCEDURE (LONG PageNo)
  CODE
  IF SELF.PageNo = PageNo
    IF SELF.Created
      SELF.Created = FALSE
      DESTROY (SELF.ImageFeq)
      DESTROY (SELF.BorderFeq)
    END

    SELF.ZoomState = NoZoom
  ELSIF NOT SELF.Neighbour &= NULL
    SELF.Neighbour.Delete (PageNo)
  END
  RETURN


PageManagerClass.Delete PROCEDURE()
  CODE
  IF NOT SELF.Neighbour &= NULL
    SELF.Neighbour.Delete()
  END
  SELF.Delete (SELF.PageNo)
  RETURN


PageManagerClass.Kill PROCEDURE()
  CODE
  IF NOT SELF.Neighbour &= NULL
    SELF.Neighbour.Kill()
    DISPOSE (SELF.Neighbour)
  END

  IF SELF.Created
    SELF.Created = FALSE
    DESTROY (SELF.ImageFeq)
    DESTROY (SELF.BorderFeq)
  END

  DISPOSE (SELF.Filename)
  RETURN


PageManagerClass.Exists PROCEDURE (LONG PageNo)
  CODE
  IF SELF.PageNo = PageNo
    RETURN TRUE
  END
  IF NOT SELF.Neighbour &= NULL
    RETURN SELF.Neighbour.Exists (PageNo)
  END
  RETURN FALSE


PageManagerClass.HighLight PROCEDURE (LONG PageNo)

Clr        LONG,AUTO  
  
  CODE
  IF SELF.Created
    Clr = COLOR:Black

    IF SELF.PageNo = PageNo AND SELF.ZoomState = NoZoom
      Clr = COLOR:Red
    END

    SELF.BorderFeq {PROP:Color} = Clr
  END
  IF NOT SELF.Neighbour &= NULL
    SELF.Neighbour.HighLight (PageNo)
  END
  RETURN


PageManagerClass.SetCentre PROCEDURE (LONG PageNo, BYTE X, BYTE Y)
  CODE
  IF SELF.PageNo = PageNo
    SELF.CentreOnX = MulDiv (X, 255, 100)
    SELF.CentreOnY = MulDiv (Y, 255, 100)
  ELSE
    SELF.CentreOnX = 0
    SELF.CentreOnY = 0
  END

  IF NOT SELF.Neighbour &= NULL
    SELF.Neighbour.SetCentre (PageNo, X, Y)
  END
  RETURN


PageManagerClass.GetProp PROCEDURE (LONG PageNo, UNSIGNED Prop)
  CODE
  IF SELF.PageNo = PageNo
    RETURN SELF.ImageFeq {Prop}
  END

  IF NOT SELF.Neighbour &= NULL
    RETURN SELF.Neighbour.GetProp (PageNo, Prop)
  END
  RETURN ''

! =============================================================================

ReportManager.Init PROCEDURE (*ProcessClass PC,<REPORT R>,<*PrintPreviewClass PV>)
  CODE
  SELF.UseMRP           = TRUE
  SELF.Process         &= PC
  SELF.Report          &= R
  SELF.Preview         &= PV
  SELF.RecordsPerCycle  = 1
  SELF.TimeSlice        = 100
  SELF.Processors      &= NEW ProcessorQueue
  SELF.Attribute       &= NEW ReportAttributeManager
  SELF.PreviewQueue    &= NEW PreviewQueue
  SELF.WMFParser       &= NEW WMFDocumentParser
  SELF.OutputFileQueue &= NEW OutputFileQueue

  IF SELF.TargetSelector &= NULL
    SELF.TargetSelectorCreated = TRUE
    SELF.TargetSelector &= NEW ReportTargetSelectorClass()
  ELSE
    SELF.TargetSelectorCreated = FALSE
  END

  IF NOT SELF.Preview &= NULL          
    SELF.Preview.Init (SELF.PreviewQueue, SELF.TargetSelector, SELF.WMFParser)
    SELF.Preview.Errors &= SELF.Errors
  END

  IF NOT SELF.BreakMan &= NULL
    SELF.BreakMan.Reset()
  END
  RETURN


ReportManager.Kill  PROCEDURE()
  CODE
  IF SELF.Dead OR SELF.Process &= NULL
    PARENT.Kill()
    RETURN Level:Notify
  END

  DISPOSE (SELF.PreviewQueue)

  IF NOT SELF.Preview &= NULL
    SELF.Preview.Kill()
  END

  IF NOT SELF.OutputFileQueue &= NULL
    FREE (SELF.OutputFileQueue)
    DISPOSE (SELF.OutputFileQueue)
  END

  SELF.Process.Kill()
  DISPOSE (SELF.Processors)
  DISPOSE (SELF.WMFParser)
  DISPOSE (SELF.Attribute)

  IF SELF.TargetSelectorCreated
    DISPOSE (SELF.TargetSelector)
  END
  RETURN PARENT.Kill()


ReportManager.SetReportTarget  PROCEDURE (*IReportGenerator pReportTarget)
   CODE
   SELF.ReportTarget &= pReportTarget
   RETURN


ReportManager.AddItem  PROCEDURE (*ReportTargetSelectorClass pTargetSelector)
   CODE
   IF NOT SELF.TargetSelectorCreated
     SELF.TargetSelector &= pTargetSelector
   END
   RETURN


ReportManager.AddItem PROCEDURE (*RecordProcessor RP)
  CODE
? ASSERT (NOT SELF.Processors &= NULL, 'Object not initialized')

  SELF.Processors.P &= RP
  ADD (SELF.Processors)
  RETURN


ReportManager.AddItem  PROCEDURE (*BreakManagerClass pBreakMan)
  CODE
  SELF.BreakMan &= pBreakMan
  RETURN


ReportManager.Next  PROCEDURE()
  CODE
  CASE SELF.Process.Next (TRUE)
  OF Level:Notify
    IF SELF.Process.RecordsProcessed <> 0
      SELF.Response = RequestCompleted
      POST (Event:CloseWindow)
      RETURN Level:Notify
    END
  OROF Level:Fatal
    SELF.Response = RequestCancelled
    POST (Event:CloseWindow)
    RETURN Level:Fatal
  END
  RETURN Level:Benign


ReportManager.Open  PROCEDURE()
  CODE
  PARENT.Open()

  IF NOT SELF.DeferOpenReport
    SELF.OpenReport()

    IF NOT SELF.OpenFailed
      IF SELF.Report{PROPPRINT:Extend}
        SELF.SetStaticControlsAttributes()
      END
    END
  END
  RETURN


ReportManager.Ask  PROCEDURE()
  CODE
  IF SELF.DeferWindow <> 0
    IF SELF.WaitCursor
      SETCURSOR (CURSOR:Wait)
    END

    SELF.StartTime = CLOCK()
    HIDE (0)
  END

  PARENT.Ask()
  RETURN


ReportManager.OpenReport  PROCEDURE()

RVal       BYTE,AUTO

  CODE
  SELF.Process.Reset()                 ! Needed for 're-read' case
  RVal = SELF.Next()
  SELF.DeferOpenReport = 0

  IF RVal <> Level:Benign
    SELF.OpenFailed = TRUE
    SELF.TakeNoRecords()
  ELSE
    SELF.OpenFailed = FALSE

    IF NOT SELF.Report &= NULL
      OPEN (SELF.Report)

      IF NOT SELF.Attribute &= NULL
        SELF.Attribute.Init (SELF.Report)
      END

      IF NOT SELF.Preview &= NULL
        SELF.Preview.OnePageReportJobName = SELF.Report{PROP:Text}
        SELF.Report{PROP:Preview} = SELF.PreviewQueue.FileName
      END
    END
  END
  RETURN RVal


ReportManager.TakeNoRecords PROCEDURE()
  CODE
  SELF.Errors.Throw (Msg:NoRecords)
  RETURN


ReportManager.TakeCloseEvent PROCEDURE()

W     &WINDOW,AUTO
I     LONG,AUTO
RVal  BYTE,AUTO
RVal2 BYTE,AUTO

  CODE
  IF EVENT() <> EVENT:CloseWindow
    RETURN Level:Benign
  END

  IF SELF.WaitCursor
    SETCURSOR
  END

  W &= NULL

  IF NOT SELF.KeepVisible
    IF NOT SELF.Preview &= NULL AND NOT SELF.Report &= NULL AND NOT SELF.SkipPreview
      W &= SYSTEM{PROP:TARGET} + 0

      IF NOT W &= NULL
        TARGET{PROP:Hide} = TRUE
      END
    END
  END

  RVal = Level:Benign

  LOOP I = 1 TO RECORDS(SELF.Processors)
    GET (SELF.Processors, I)
    RVal2 = SELF.Processors.P.TakeClose()

    IF RVal2 <> Level:Benign
      RVal = RVal2
    END
  END

  IF RVal = Level:Benign AND NOT SELF.OpenFailed
    SELF.AskPreview()
  END

  IF NOT SELF.Report &= NULL
    CLOSE (SELF.Report)
  END

  IF NOT W &= NULL
    SETTARGET (W)

    TARGET{PROP:Width}  = 0
    TARGET{PROP:Height} = 0
    TARGET{PROP:Hide}   = FALSE
  END
  RETURN RVal


ReportManager.TakeAccepted PROCEDURE()
  CODE
  CASE Accepted()
  OF SELF.QueryControl
    SELF.Process.TakeLocate()
  END
  RETURN PARENT.TakeAccepted()


ReportManager.TakeRecord PROCEDURE()

I            LONG,AUTO
RVal         BYTE(Level:Benign)

  CODE
  IF NOT SELF.BreakMan &= NULL
    SELF.BreakMan.AskBreak()
  END

  IF NOT SELF.Report &= NULL
    IF SELF.Report{PROPPRINT:Extend}
      SELF.SetDynamicControlsAttributes()
    END
  END

  RVal = SELF.Process.TakeRecord()
  DO CheckState

  LOOP I = 1 TO RECORDS(SELF.Processors)
    GET (SELF.Processors, I)

    RVal = SELF.Processors.P.TakeRecord()
    DO CheckState
  END

  IF SELF.Next()
    TARGET{PROP:Timer} = 0
    RVal = Level:Notify
  END
  RETURN RVal


CheckState ROUTINE
  CASE RVal
  OF Level:Fatal
    TARGET {PROP:Timer} = 0
    POST (EVENT:CloseWindow)
  OROF Level:Notify
    RETURN Level:Notify
  END
  EXIT


ReportManager.SetStaticControlsAttributes  PROCEDURE()
  CODE
  RETURN


ReportManager.SetDynamicControlsAttributes  PROCEDURE()
  CODE
  RETURN


ReportManager.TakeWindowEvent  PROCEDURE()

StartOfCycle LONG,AUTO
StartTime    LONG,AUTO
TimeTaken    LONG,AUTO
RVal         BYTE(Level:Benign)

  CODE
  IF EVENT() = EVENT:Timer
    IF SELF.DeferOpenReport
      SELF.OpenReport()

      IF NOT SELF.OpenFailed
        IF NOT SELF.Report &= NULL
          IF SELF.Report {PROPPRINT:Extend}
            SELF.SetStaticControlsAttributes()
          END
        END
      END
    END

    IF SELF.OpenFailed
      RETURN CHOOSE (NOT SELF.KeepVisible, Level:Fatal, Level:Benign)
    END

    ! Take a timer event. Adjust RecordsPerCycle to get a granularity of 
    ! around SELF.TimeSlice

    StartOfCycle = SELF.Process.RecordsProcessed
    StartTime    = CLOCK()

    IF SELF.DeferWindow <> 0 
      IF (StartTime - SELF.StartTime) > SELF.DeferWindow * 100
        TARGET{PROP:Hide} = FALSE
        SELF.DeferWindow  = 0
      END
    END

    LOOP
      !Set the MRP to SELF.RecordsPerCycle
      IF SELF.UseMRP
        IF SELF.Process.View {PROP:IPRequestCount} = 0
          SELF.Process.View {PROP:IPRequestCount} = SELF.RecordsPerCycle
        END
      END

      LOOP WHILE SELF.Process.RecordsProcessed - StartOfCycle < SELF.RecordsPerCycle
        RVal = SELF.TakeRecord()
        IF RVal <> Level:Benign
          RETURN RVal
        END
      END

      TimeTaken = CLOCK() - StartTime

      IF 5 * TimeTaken < SELF.TimeSlice * 4       ! Use integer arithmetic for speed
        SELF.RecordsPerCycle += SELF.RecordsPerCycle
      END
    WHILE 2 * TimeTaken < SELF.TimeSlice
    !
    IF 2 * TimeTaken > SELF.TimeSlice * 3 AND SELF.RecordsPerCycle > 1
      SELF.RecordsPerCycle /= 2
    END
  END

  RETURN PARENT.TakeWindowEvent()


ReportManager.ProcessResultFiles PROCEDURE(OutputFileQueue OutputFile)
  CODE
  RETURN Level:Benign


ReportManager.EndReport  PROCEDURE()
  CODE
  IF NOT SELF.BreakMan &= NULL
    SELF.BreakMan.AskBreak (TRUE)
  END
  IF NOT SELF.Report &= NULL
    ENDPAGE (SELF.Report)
  END
  RETURN Level:Benign


ReportManager.PrintReport  PROCEDURE()
Idx         UNSIGNED,AUTO
RVal        BYTE,AUTO

  CODE
  IF SELF.Report &= NULL
    RETURN
  END

  ! Used select the target at the beginning if this is not a standard report
  IF SELF.ReportTarget &= NULL
    FREE (SELF.OutputFileQueue)

    LOOP Idx = 1 TO RECORDS (SELF.PreviewQueue)
      GET (SELF.PreviewQueue, Idx)

      IF ERRORCODE() = 0
        SELF.OutputFileQueue.FileName = SELF.PreviewQueue.FileName
        ADD (SELF.OutputFileQueue)
      END
    END

    RVal = SELF.ProcessResultFiles (SELF.OutputFileQueue)

    SELF.Report{PROP:FlushPreview} = CHOOSE (RVal = Level:Benign)
    RETURN
  END

  IF RECORDS (SELF.PreviewQueue) = 0
    RETURN
  END

  IF SELF.ReportTarget.SupportResultQueue()
    SELF.ReportTarget.SetResultQueue (SELF.OutputFileQueue)
  END

  ! The false parameter means that the AskProperties must ask for a name
  ! only if the target name is blank

  IF SELF.ReportTarget.AskProperties (FALSE) <> Level:Benign
    RETURN
  END

  SELF.WMFParser.Init (SELF.PreviewQueue, SELF.ReportTarget, SELF.Errors)

  IF SELF.WMFParser.GenerateReport (SELF.ShowOutputProgress) <> Level:Benign
    RETURN
  END

  IF NOT SELF.ReportTarget.SupportResultQueue()
    RETURN
  END

  SELF.ProcessResultFiles (SELF.OutputFileQueue)

  IF NOT SELF.GetOpenOutputWhenFinish() OR RECORDS(SELF.OutputFileQueue) = 0
    RETURN
  END

  GET (SELF.OutputFileQueue, 1)

  IF ERRORCODE() = 0
    ! RUN() invokes default programs to open files of registered types
    RUN (SELF.OutputFileQueue.FileName)

    IF ERRORCODE() <> 0
      MESSAGE ('Error trying to open the report output.||' & ERROR(),'Error on preview')
    END
  END
  RETURN


ReportManager.CancelPrintReport  PROCEDURE()
  CODE
  IF NOT SELF.Report &= NULL
    SELF.Report{PROP:FlushPreview} = FALSE
  END
  RETURN


ReportManager.AskPreview PROCEDURE()

RetValue   BYTE,AUTO
  
  CODE
  RetValue = SELF.EndReport()
  
  IF SELF.Report &= NULL OR SELF.Response <> RequestCompleted OR SELF.OpenFailed
    RETURN
  END

  IF RetValue = Level:Benign
    IF SELF.SkipPreview OR SELF.Preview &= NULL OR SELF.Preview.Display (SELF.Zoom)
      SELF.PrintReport()
    ELSE
      SELF.CancelPrintReport()
    END
  END

  IF NOT SELF.Preview &= NULL
    FREE (SELF.Preview.ImageQueue)
  ELSE
    FREE (SELF.PreviewQueue)
  END
  RETURN


ReportManager.SetOpenOutputWhenFinish PROCEDURE (BYTE value)
  CODE
  SELF.OpenOutputWhenFinish = value
  RETURN


ReportManager.GetOpenOutputWhenFinish PROCEDURE()
  CODE
  RETURN SELF.OpenOutputWhenFinish


ReportManager.SetUseMRP  PROCEDURE (BYTE UseMRP)
  CODE
  SELF.UseMRP = UseMRP
  RETURN


ReportManager.GetUseMRP  PROCEDURE()
  CODE
  RETURN SELF.UseMRP
