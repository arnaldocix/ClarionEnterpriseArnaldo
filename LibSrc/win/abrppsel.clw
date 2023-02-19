    MEMBER
 INCLUDE('ABRPPSEL.INC'),ONCE
    MAP
    END
 PRAGMA ('link (EXP_PRI.ICO)')
ReportTargetSelectorClass.CONSTRUCT          PROCEDURE()
 CODE
    SELF.Horizontal =6
    SELF.Stretch    =1
    SELF.SelectedItemIndex = 0
    SELF.QOutputGen &= NEW(TargetOutputGeneratorQueue)

ReportTargetSelectorClass.DESTRUCT           PROCEDURE()
 CODE
    FREE(SELF.QOutputGen)
    DISPOSE(SELF.QOutputGen)
 
ReportTargetSelectorClass.SetSize            PROCEDURE(BYTE pHorizontal=6,BYTE pStretch=1)
 CODE
    SELF.Stretch    = pStretch
    SELF.Horizontal = pHorizontal

ReportTargetSelectorClass.AddItem            PROCEDURE(IOutputGeneratorTarget pPlugIn,BYTE pEnableOnPreview=1)
 CODE
    SELF.QOutputGen.OutputGenerator &= pPlugIn
    SELF.QOutputGen.EnableOnPreview  = pEnableOnPreview
    ADD(SELF.QOutputGen)

ReportTargetSelectorClass.AddItem                 PROCEDURE(IReportGenerator pPlugIn,BYTE pEnableOnPreview=1) ! Add a plugin to the object
theParent &IOutputGeneratorTarget
 CODE
    SELF.QOutputGen.ReportGenerator &= pPlugIn
    SELF.QOutputGen.DataOutputGenerator &= NULL
    theParent &= pPlugIn
    SELF.AddItem(theParent,pEnableOnPreview)
    
ReportTargetSelectorClass.AddItem                 PROCEDURE(IDataOutputGenerator pPlugIn) ! Add a plugin to the object
theParent &IOutputGeneratorTarget
 CODE
    SELF.QOutputGen.ReportGenerator &= NULL
    SELF.QOutputGen.DataOutputGenerator &= pPlugIn
    theParent &= pPlugIn
    SELF.AddItem(theParent,0)

ReportTargetSelectorClass.Items              PROCEDURE(BYTE pFromPreview=0)
lCount SHORT,AUTO
lIndex SHORT,AUTO
 CODE
    IF pFromPreview
       lCount = 0
       LOOP lIndex=1 TO RECORDS(SELF.QOutputGen)
            GET(SELF.QOutputGen,lIndex)
            IF ERRORCODE() THEN BREAK.
            IF SELF.QOutputGen.EnableOnPreview                  
               lCount+=1
            END
       END
       RETURN lCount
    ELSE
       RETURN RECORDS(SELF.QOutputGen)
    END

ReportTargetSelectorClass.Item PROCEDURE(SHORT GeneratorPos,BYTE pFromPreview=0)
lIndex SHORT
lCount SHORT
 CODE
    IF GeneratorPos > 0
       IF pFromPreview = 0
          GET(SELF.QOutputGen,GeneratorPos)
          IF NOT ERRORCODE() THEN
             RETURN SELF.QOutputGen.OutputGenerator
          END
       ELSE
          lCount = 0
          LOOP lIndex=1 TO RECORDS(SELF.QOutputGen)
               GET(SELF.QOutputGen,lIndex)
               IF ERRORCODE() THEN BREAK.
               IF SELF.QOutputGen.EnableOnPreview                  
                  lCount+=1
                  IF lCount = GeneratorPos
                     RETURN SELF.QOutputGen.OutputGenerator
                  END
               END
          END
       END
    END
    ASSERT(False,'The number "'&GeneratorPos&'" is not a valid Generator: GetOutputGeneratorName')
    RETURN ''

ReportTargetSelectorClass.GetOutputSelected PROCEDURE()
 CODE
     IF SELF.GetPrintSelected() THEN
        ASSERT(False,'Printer was selected as the output,it is not expected to call this method: GetSelected')
        RETURN ''
     ELSE
        ASSERT(~SELF.OutputSelected&=NULL,'PlugIn not selected method: GetSelected')
        RETURN SELF.OutputSelected
     END

ReportTargetSelectorClass.GetPrintSelected      PROCEDURE()
 CODE
    IF SELF.WithPrinter THEN
       RETURN SELF.PrintSelected
    ELSE
       RETURN False
    END

ReportTargetSelectorClass.GetSelectedItemIndex PROCEDURE()
 CODE
    RETURN SELF.SelectedItemIndex

ReportTargetSelectorClass.GetDefaultItemIndex PROCEDURE()
 CODE
    RETURN SELF.DefaultItemIndex

ReportTargetSelectorClass.SetDefaultOutputGenerator PROCEDURE(STRING GeneratorNameOrIndex)
lIndex BYTE
 CODE
    SELF.DefaultItemIndex = SELF.GetOutputGeneratorIndex(GeneratorNameOrIndex)
  
ReportTargetSelectorClass.ASK                PROCEDURE(BYTE pWithPrinter=0,BYTE pFromPreview=0)
lButton_W         SHORT(32)
lButton_H         SHORT(36)
lLeft_Margin      SHORT(2)
lTop_Margin       SHORT(2)
lSeparation       SHORT(2)
lPlugInChoice     SHORT
lFirstFeq         SHORT
lFEQ              LONG
lIndexH           SHORT
lIndex            SHORT
lHOR              SHORT
lVER              SHORT
lHorizontal       SHORT
lPrinterFEQ       LONG
lOK               BYTE
lQRecords         SHORT
lOutputGenerator  &IOutputGeneratorTarget

PlugInChoiceWindow WINDOW('Select an Output'),AT(,,103,30),FONT('Arial',8,COLOR:Black,FONT:regular,CHARSET:ANSI),CENTER,IMM,SYSTEM,GRAY,DOUBLE,MASK
       OPTION,AT(0,0,16,12),USE(lPlugInChoice,,?lPlugInChoice),FONT('Arial',8,COLOR:Black,FONT:regular,CHARSET:ANSI)
       END
       BUTTON('&OK'),AT(1,14,47,14),USE(?BOk),LEFT,ICON('WAOk.ico'),DEFAULT,FONT('Microsoft Sans Serif',8,,FONT:regular)
       BUTTON('&Cancel'),AT(55,14,47,14),USE(?BCancel),LEFT,ICON('WACancel.ico'),FONT('Microsoft Sans Serif',8,,FONT:regular)
     END

 CODE
  SELF.WithPrinter = pWithPrinter
  lQRecords = SELF.Items(pFromPreview)
      
  IF pWithPrinter THEN
     IF lQRecords=0 THEN
        SELF.PrintSelected   = True
        RETURN True
     END
     pWithPrinter = 1
  ELSE
     SELF.PrintSelected = False
     IF lQRecords=0 THEN
        RETURN FALSE
     ELSE
        IF lQRecords=1 THEN
           GET(SELF.QOutputGen,1)
           SELF.SelectedItemIndex = 1
           SELF.OutputSelected &= SELF.QOutputGen.OutputGenerator
           RETURN True
        END
     END
  END
  lOK=False
  Do PrepareWindow
  OPEN(PlugInChoiceWindow)
  Do CreateControls
  ACCEPT
    CASE EVENT()
    OF EVENT:OpenWindow
      SELECT(?BOk)
    END
    CASE FIELD()
    OF ?BOk
      CASE EVENT()
      OF EVENT:Accepted
        lOK=True
        POST(Event:CloseWindow)
      END
    OF ?BCancel
      CASE EVENT()
      OF EVENT:Accepted
        lOK=False
        POST(Event:CloseWindow)
      END
    END
  END
  IF lOK THEN
     IF NOT pWithPrinter THEN
        lOutputGenerator &= SELF.Item(lPlugInChoice, pFromPreview)        
        SELF.SelectedItemIndex = lPlugInChoice
        SELF.OutputSelected &= SELF.QOutputGen.OutputGenerator
        SELF.PrintSelected = False
     ELSE
        IF lPlugInChoice=1 THEN
           SELF.PrintSelected = True
        ELSE
           lOutputGenerator &= SELF.Item(lPlugInChoice-1, pFromPreview)
           SELF.SelectedItemIndex = lPlugInChoice-1
           SELF.OutputSelected &= SELF.QOutputGen.OutputGenerator
           SELF.PrintSelected = False
        END
     END
  END
  RETURN lOK
PrepareWindow  ROUTINE
   lButton_W = 32
   lButton_H = 36
   lHorizontal=SELF.Horizontal
   IF SELF.Stretch THEN
      IF lHorizontal>(lQRecords+pWithPrinter) THEN
         lHorizontal=lQRecords+pWithPrinter
      ELSE
         IF (lQRecords+pWithPrinter)=lHorizontal+1 THEN
            lHorizontal+=1
         END
      END
   END

   IF lHorizontal<3 THEN
      lHorizontal = 3
   END
CreateControls  ROUTINE
   lIndexH=0
   lVER=lTop_Margin
   lHOR=lLeft_Margin
   LOOP lIndex=1 TO lQRecords+pWithPrinter
        lIndexH+=1
        IF lIndexH>lHorizontal THEN
           lIndexH=1
           lHOR=lLeft_Margin
           lVER=lVER+lButton_H+lSeparation
        END

        lFEQ=CREATE( 0 , CREATE:radio,?lPlugInChoice)
        lFEQ{PROP:Value}=lIndex
        lFEQ{PROP:FLAT}=True
        IF NOT(lIndex=1 AND pWithPrinter) THEN
           lOutputGenerator &= SELF.Item(lIndex-pWithPrinter, pFromPreview)
           lFEQ{PROP:ICON}=lOutputGenerator.DisplayIcon()
           lFEQ{PROP:TEXT}=lOutputGenerator.DisplayName()
        ELSE
           lFEQ{PROP:Value}=1 !The printer is always the first icon
           lPrinterFEQ=lFEQ
           lFEQ{PROP:ICON}='~EXP_PRI.ICO'
           lFEQ{PROP:Text}='Print'
        END
        SETPOSITION(lFEQ,lHOR,lVER,lButton_W,lButton_H)
        UNHIDE(lFEQ)
        IF lIndex=1 THEN
           lFirstFeq    =lFEQ
        END
        lHOR=lHOR+lButton_W+lSeparation
   END
   lPlugInChoice=1
   ?lPlugInChoice{PROP:SELECTED}=1
   lHOR=(lButton_W+lSeparation)*lHorizontal + 2
   ?BCancel{PROP:Width}=lButton_W+(lSeparation/2)+(lButton_W/2)
   ?BOk{PROP:Width}=?BCancel{PROP:Width}
   lVER=lVER+2*lSeparation+?BCancel{PROP:Height}+lButton_H+lSeparation
   ?BCancel{PROP:XPos}=lHOR-?BCancel{PROP:Width}-lSeparation
   ?BOk{PROP:XPos}= ?BCancel{PROP:XPos}-lSeparation-?BOk{PROP:Width}
   ?BCancel{PROP:YPos}=lVER-lSeparation-?BCancel{PROP:Height}
   ?BOk{PROP:YPos}=?BCancel{PROP:YPos}
   PlugInChoiceWindow{PROP:AT,3}=lHOR
   PlugInChoiceWindow{PROP:AT,4}=lVER

ReportTargetSelectorClass.FillQueue               PROCEDURE(QUEUE QueueToFill,BYTE ColumnToFill=1)
lColumn ANY
lIndex  SHORT
 CODE
    FREE(QueueToFill)
    IF RECORDS(SELF.QOutputGen)
       lColumn &= WHAT(QueueToFill,ColumnToFill)
       LOOP lIndex=1 TO RECORDS(SELF.QOutputGen)
            GET(SELF.QOutputGen,lIndex)
            IF ERRORCODE() THEN BREAK.
            lColumn = SELF.QOutputGen.OutputGenerator.DisplayName()
            ADD(QueueToFill)
       END
    END

ReportTargetSelectorClass.GetOutputGeneratorIndex PROCEDURE(STRING GeneratorNameOrIndex)
lIndex  BYTE
 CODE  
       IF NUMERIC(GeneratorNameOrIndex) THEN
          lIndex = GeneratorNameOrIndex           
          IF lIndex <= RECORDS(SELF.QOutputGen)
             RETURN lIndex
          END
?         ASSERT(False,'The number "'&GeneratorNameOrIndex&'" is not a valid Generator: GetReportGenerator')
       ELSE
          LOOP lIndex=1 TO RECORDS(SELF.QOutputGen)
               GET(SELF.QOutputGen,lIndex)
               IF ERRORCODE() THEN BREAK.
               IF UPPER(CLIP(GeneratorNameOrIndex))=UPPER(SELF.QOutputGen.OutputGenerator.DisplayName()) THEN
                  RETURN lIndex
               END
          END
?         ASSERT(False,'The name "'&GeneratorNameOrIndex&'" is not a valid Generator Name: GetReportGenerator')
       END
       RETURN 0

ReportTargetSelectorClass.SetSelectedOutputGenerator PROCEDURE(STRING GeneratorNameOrIndex)
lIndex  BYTE
 CODE  
       SELF.OutputSelected &= NULL
       SELF.SelectedItemIndex = SELF.GetOutputGeneratorIndex(GeneratorNameOrIndex)
       IF SELF.SelectedItemIndex > 0
          GET(SELF.QOutputGen, SELF.SelectedItemIndex)
          SELF.OutputSelected &= SELF.QOutputGen.OutputGenerator
          RETURN SELF.SelectedItemIndex
       END
       RETURN 0
 
ReportTargetSelectorClass.GetOutputGenerator      PROCEDURE(STRING GeneratorName)
lIndex  SHORT
 CODE
    IF SELF.SetSelectedOutputGenerator(GeneratorName)>0
       RETURN SELF.QOutputGen.OutputGenerator
    END
    RETURN NULL

ReportTargetSelectorClass.GetOutputGeneratorName  PROCEDURE(SHORT GeneratorPos,BYTE pFromPreview=0)
lFound &IOutputGeneratorTarget
 CODE
    lFound &= SELF.Item(GeneratorPos,pFromPreview)
    IF NOT lFound &= NULL
       RETURN lFound.DisplayName()
    END
    ASSERT(False,'The number "'&GeneratorPos&'" is not a valid Generator: GetOutputGeneratorName')
    RETURN ''

ReportTargetSelectorClass.GetReportGenerator      PROCEDURE(STRING GeneratorName)
lIndex  SHORT,AUTO
 CODE
    lIndex = SELF.SetSelectedOutputGenerator(GeneratorName)
    IF lIndex > 0
       GET(SELF.QOutputGen,lIndex)
       IF NOT ERRORCODE() THEN
          RETURN SELF.QOutputGen.ReportGenerator
       END
       ASSERT(False,'The number "'&GeneratorName&'" is not a valid Generator: GetReportGenerator')
    END
    RETURN NULL

ReportTargetSelectorClass.GetDataOutputGenerator  PROCEDURE(STRING GeneratorName)
lIndex  SHORT,AUTO
 CODE
    lIndex = SELF.SetSelectedOutputGenerator(GeneratorName)
    IF lIndex > 0
       GET(SELF.QOutputGen,lIndex)
       IF NOT ERRORCODE() THEN
          RETURN SELF.QOutputGen.DataOutputGenerator
       END
       ASSERT(False,'The number "'&GeneratorName&'" is not a valid Generator: GetDataOutputGenerator')
    END
    RETURN NULL

ReportTargetSelectorClass.GetReportSelected       PROCEDURE()!It is only valid to call this method if GetPrintSelected=False
lIndex  SHORT,AUTO
 CODE
     IF SELF.GetPrintSelected() THEN
        ASSERT(False,'Printer was selected as the output,it is not expected to call this method: GetSelected')
        RETURN ''
     ELSE
        lIndex = SELF.GetSelectedItemIndex()
        ASSERT(~lIndex=0,'PlugIn not selected method: GetSelected')
        GET(SELF.QOutputGen,lIndex)
        IF ERRORCODE() THEN RETURN NULL.
        RETURN SELF.QOutputGen.ReportGenerator
     END

ReportTargetSelectorClass.GetReportDefault        PROCEDURE()
 CODE
    ASSERT(~SELF.DefaultItemIndex=0,'PlugIn not set as Default')
    IF SELF.DefaultItemIndex > 0
       GET(SELF.QOutputGen,SELF.DefaultItemIndex)
       IF ERRORCODE() THEN RETURN NULL.
       RETURN SELF.QOutputGen.ReportGenerator
    END
    RETURN NULL

ReportTargetSelectorClass.GetDataOutputSelected   PROCEDURE()
lIndex  SHORT,AUTO
 CODE
     IF SELF.GetPrintSelected() THEN
        ASSERT(False,'Printer was selected as the output,it is not expected to call this method: GetSelected')
        RETURN ''
     ELSE
        lIndex = SELF.GetSelectedItemIndex()
        ASSERT(~lIndex=0,'PlugIn not selected method: GetSelected')
        GET(SELF.QOutputGen,lIndex)
        IF ERRORCODE() THEN RETURN NULL.
        RETURN SELF.QOutputGen.DataOutputGenerator
     END 

ReportTargetSelectorClass.GetDataOutputDefault        PROCEDURE()
 CODE
    ASSERT(~SELF.DefaultItemIndex=0,'PlugIn not set as Default')
    IF SELF.DefaultItemIndex > 0
       GET(SELF.QOutputGen,SELF.DefaultItemIndex)
       IF ERRORCODE() THEN RETURN NULL.
       RETURN SELF.QOutputGen.DataOutputGenerator
    END
    RETURN NULL
     
ReportTargetSelectorClass.IsSelectedAReportOuput  PROCEDURE()
 CODE
    IF NOT SELF.QOutputGen.ReportGenerator &= NULL
       RETURN TRUE
    ELSE
       RETURN FALSE
    END

ReportTargetSelectorClass.IsSelectedADataOuput    PROCEDURE()
 CODE
    IF NOT SELF.QOutputGen.DataOutputGenerator &= NULL
       RETURN TRUE
    ELSE
       RETURN FALSE
    END
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
