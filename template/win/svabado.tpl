#TEMPLATE(ABC_ADO, 'SoftVelocity ADO Template'), FAMILY('ABC')
#HELP('ClarionHelp.chm')
#COMMENT(60)
#!
#INCLUDE('ADO.TPW')
#!------------------------------------------------------------------------
#PROCEDURE(Form,'Edit a Record from a File'),WINDOW,REPORT,HLP('~TPLProcADOForm.htm'),PARENT(Window(aBC))
#RESTRICT
  #FOR( %ApplicationTemplate )
    #IF(UPPER(SUB(%ApplicationTemplate,1,10)) = 'ADOSUPPORT')
      #ACCEPT
    #ENDIF
  #ENDFOR
  #REJECT
#ENDRESTRICT
#DEFAULT
NAME DefaultADOForm
CATEGORY 'ADOForm'
[COMMON]
DESCRIPTION 'FORM (Add/Edit/Delete)'
FROM ABC Window
[PROMPTS]
%WindowOperationMode STRING  ('Use WINDOW setting')
%INISaveWindow LONG  (1)
[ADDITION]
NAME ABC_ADO SaveButton
[INSTANCE]
INSTANCE 1
PROCPROP
[PROMPTS]
%InsertAllowed LONG  (1)
%InsertMessage @S30  ('Record will be Added')
%ChangeAllowed LONG  (1)
%ChangeMessage @S30  ('Record will be Changed')
%DeleteAllowed LONG  (1)
%DeleteMessage @S30  ('Record will be Deleted')
%MessageHeader LONG  (0)
%HistoryKey DEFAULT  ('CtrlH')
[ADDITION]
NAME ABC CancelButton
[INSTANCE]
INSTANCE 2
[WINDOW]
FormWindow WINDOW('Update Records...'),AT(18,5,289,159),CENTER,SYSTEM,GRAY,MDI
       BUTTON('OK'),AT(5,140,40,12),USE(?OK),DEFAULT,REQ,#SEQ(1),#ORIG(?OK),#LINK(?Cancel)
       BUTTON('Cancel'),AT(50,140,40,12),USE(?Cancel),#SEQ(2),#ORIG(?Cancel)
       STRING(@S40),AT(95,140,,),USE(ActionMessage),#ORIG(?ActionMessage)
     END

#ENDDEFAULT
#!----------------------------------------------------------------
#PROCEDURE(Browse,'Browse Fields in a List Box'),WINDOW,REPORT,HLP('~TPLProcADOBrowse.htm'),PARENT(Window(ABC))
#RESTRICT
  #FOR( %ApplicationTemplate )
    #IF(UPPER(SUB(%ApplicationTemplate,1,10)) = 'ADOSUPPORT')
      #ACCEPT
    #ENDIF
  #ENDFOR
  #REJECT
#ENDRESTRICT
#DEFAULT
NAME DefaultADOBrowse
CATEGORY 'ADOBrowse'
[COMMON]
DESCRIPTION 'Browse with Update'
FROM ABC Window
[PROMPTS]
%WindowOperationMode STRING  ('Use WINDOW setting')
%INISaveWindow LONG  (1)
[ADDITION]
NAME ABC_ADO BrowseBox
[INSTANCE]
INSTANCE 1
PROCPROP

[ADDITION]
NAME ABC_ADO BrowseUpdateButtons
[INSTANCE]
INSTANCE 2
PARENT 1
PROCPROP
[PROMPTS]
%UpdateProcedure PROCEDURE  ()
[ADDITION]
NAME ABC CloseButton
[INSTANCE]
INSTANCE 3
[WINDOW]
BrowseWindow WINDOW('Browse Records'),AT(0,0,247,140),SYSTEM,GRAY,MDI
       LIST,AT(5,5,235,100),USE(?List),IMM,HVSCROLL,MSG('Browsing Records'),FROM(Queue:Browse),#SEQ(1), |
           #ORIG(BrowseBox)
       BUTTON('&Insert'),AT(5,110,40,12),USE(?Insert),#SEQ(2),#ORIG(?Insert),#LINK(?Change)
       BUTTON('&Change'),AT(50,110,40,12),USE(?Change),DEFAULT,#SEQ(2),#ORIG(?Change), |
           #LINK(?Delete)
       BUTTON('&Delete'),AT(95,110,40,12),USE(?Delete),#SEQ(2),#ORIG(?Delete),#LINK(?Insert)
       BUTTON('Close'),AT(200,110,40,12),USE(?Close),#SEQ(3),#ORIG(?Close)
     END

#ENDDEFAULT
#!-------------------------------------------------------------------------
#PROCEDURE(Process,'Sequential Record Processor'),PRIMARY('File(s) to Process',OPTKEY),HLP('~TPLProcADOProcess.htm'),WINDOW,PARENT(Window(ABC))
#RESTRICT
  #FOR( %ApplicationTemplate )
    #IF(UPPER(SUB(%ApplicationTemplate,1,10)) = 'ADOSUPPORT')
      #ACCEPT
    #ENDIF
  #ENDFOR
  #REJECT
#ENDRESTRICT
#DEFAULT
NAME DefaultADOProcess
CATEGORY 'ADOProcess'
[COMMON]
DESCRIPTION 'Process procedure'
FROM ABC Window
[PROMPTS]
%WindowOperationMode STRING  ('Use WINDOW setting')
%INISaveWindow LONG  (1)
[ADDITION]
NAME ABC_ADO ProcessControl
[INSTANCE]
INSTANCE 1
PROCPROP
[PROMPTS]
%RecordsetIsParameter LONG  (0)

[WINDOW]
ProgressWindow WINDOW('Progress...'),AT(,,142,59),CENTER,TIMER(1),GRAY,DOUBLE
       PROGRESS,USE(Progress:Thermometer,,?Progress:Thermometer),AT(15,15,111,12),RANGE(0,100),#SEQ(1),#ORIG(?Progress:Thermometer)
       STRING(''),AT(0,3,141,10),USE(?Progress:UserString),CENTER,#SEQ(1),#ORIG(?Progress:UserString)
       STRING(''),AT(0,30,141,10),USE(?Progress:PctText),CENTER,#SEQ(1),#ORIG(?Progress:PctText)
       BUTTON('Cancel'),AT(45,42,50,15),USE(?Progress:Cancel),#SEQ(1),#ORIG(?Progress:Cancel)
     END
#ENDDEFAULT
#!---------------------------------------------------------------------
#PROCEDURE(Report,'Generic Reporting Procedure'),WINDOW,REPORT,PRIMARY('Report Procedure',OPTKEY),HLP('~TPLProcADOReport.htm'),PARENT(Window(ABC))
#RESTRICT
  #FOR( %ApplicationTemplate )
    #IF(UPPER(SUB(%ApplicationTemplate,1,10)) = 'ADOSUPPORT')
      #ACCEPT
    #ENDIF
  #ENDFOR
  #REJECT
#ENDRESTRICT
#DEFAULT
NAME DefaultADOReportA4Portrait
CATEGORY 'ADOReport'
[COMMON]
DESCRIPTION 'Report (Paper size A4 - Portrait)'
FROM ABC Window
[ADDITION]
NAME ABC_ADO ProcessControl
[INSTANCE]
INSTANCE 1
PROCPROP
[PROMPTS]
%RecordsetIsParameter LONG  (0)

[REPORT]
Report REPORT,AT(1000,2000,6250,7688),PAPER(PAPER:A4),PRE(RPT),FONT('Arial',10,,FONT:regular,CHARSET:ANSI),THOUS
       HEADER,AT(1000,1000,6250,1000),USE(?Header)
       END
Detail DETAIL,USE(?Detail)
       END
       FOOTER,AT(1000,9688,6250,1000),USE(?Footer)
       END
       FORM,AT(1000,1000,6250,9688),USE(?Form)
       END
     END
[WINDOW]
ProgressWindow WINDOW('Progress...'),AT(,,142,59),CENTER,TIMER(1),GRAY,DOUBLE
       PROGRESS,USE(Progress:Thermometer,,?Progress:Thermometer),AT(15,15,111,12),RANGE(0,100),#SEQ(1),#ORIG(?Progress:Thermometer)
       STRING(''),AT(0,3,141,10),USE(?Progress:UserString),CENTER,#SEQ(1),#ORIG(?Progress:UserString)
       STRING(''),AT(0,30,141,10),USE(?Progress:PctText),CENTER,#SEQ(1),#ORIG(?Progress:PctText)
       BUTTON('Cancel'),AT(45,42,50,15),USE(?Progress:Cancel),#SEQ(1),#ORIG(?Progress:Cancel)
     END
#ENDDEFAULT
#DEFAULT
NAME DefaultADOReportA4Landscape
CATEGORY 'ADOReport'
[COMMON]
DESCRIPTION 'Report (Paper size A4 - Landscape)'
FROM ABC Window
[ADDITION]
NAME ABC_ADO ProcessControl
[INSTANCE]
INSTANCE 1
PROCPROP
[PROMPTS]
%RecordsetIsParameter LONG  (0)

[REPORT]
Report REPORT,AT(1000,2000,7688,4250),PAPER(PAPER:A4),PRE(RPT),FONT('Arial',10,,FONT:regular,CHARSET:ANSI),LANDSCAPE,THOUS
       HEADER,AT(1000,1000,7688,1000),USE(?Header)
       END
Detail DETAIL,USE(?Detail)
       END
       FOOTER,AT(1000,6250,7688,1000),USE(?Footer)
       END
       FORM,AT(1000,1000,7688,6250),USE(?Form)
       END
     END
[WINDOW]
ProgressWindow WINDOW('Progress...'),AT(,,142,59),CENTER,TIMER(1),GRAY,DOUBLE
       PROGRESS,USE(Progress:Thermometer,,?Progress:Thermometer),AT(15,15,111,12),RANGE(0,100),#SEQ(1),#ORIG(?Progress:Thermometer)
       STRING(''),AT(0,3,141,10),USE(?Progress:UserString),CENTER,#SEQ(1),#ORIG(?Progress:UserString)
       STRING(''),AT(0,30,141,10),USE(?Progress:PctText),CENTER,#SEQ(1),#ORIG(?Progress:PctText)
       BUTTON('Cancel'),AT(45,42,50,15),USE(?Progress:Cancel),#SEQ(1),#ORIG(?Progress:Cancel)
     END
#ENDDEFAULT
#DEFAULT
NAME DefaultADOReportLegalPortrait
CATEGORY 'ADOReport'
[COMMON]
DESCRIPTION 'Report (Paper size Legal - Portrait)'
FROM ABC Window
[ADDITION]
NAME ABC_ADO ProcessControl
[INSTANCE]
INSTANCE 1
PROCPROP
[PROMPTS]
%RecordsetIsParameter LONG  (0)

[REPORT]
Report REPORT,AT(1000,2000,6500,10000),PAPER(PAPER:LEGAL),PRE(RPT),FONT('Arial',10,,FONT:regular,CHARSET:ANSI),THOUS
       HEADER,AT(1000,1000,6500,1000),USE(?Header)
       END
Detail DETAIL,USE(?Detail)
       END
       FOOTER,AT(1000,12000,6500,1000),USE(?Footer)
       END
       FORM,AT(1000,1000,6500,12000),USE(?Form)
       END
     END
[WINDOW]
ProgressWindow WINDOW('Progress...'),AT(,,142,59),CENTER,TIMER(1),GRAY,DOUBLE
       PROGRESS,USE(Progress:Thermometer,,?Progress:Thermometer),AT(15,15,111,12),RANGE(0,100),#SEQ(1),#ORIG(?Progress:Thermometer)
       STRING(''),AT(0,3,141,10),USE(?Progress:UserString),CENTER,#SEQ(1),#ORIG(?Progress:UserString)
       STRING(''),AT(0,30,141,10),USE(?Progress:PctText),CENTER,#SEQ(1),#ORIG(?Progress:PctText)
       BUTTON('Cancel'),AT(45,42,50,15),USE(?Progress:Cancel),#SEQ(1),#ORIG(?Progress:Cancel)
     END
#ENDDEFAULT
#DEFAULT
NAME DefaultADOReportLegalLandscape
CATEGORY 'ADOReport'
[COMMON]
DESCRIPTION 'Report (Paper size Legal - Landscape)'
FROM ABC Window
[ADDITION]
NAME ABC_ADO ProcessControl
[INSTANCE]
INSTANCE 1
PROCPROP
[PROMPTS]
%RecordsetIsParameter LONG  (0)

[REPORT]
Report REPORT,AT(1000,2000,10000,4500),PAPER(PAPER:LEGAL),PRE(RPT),FONT('Arial',10,,FONT:regular,CHARSET:ANSI),LANDSCAPE,THOUS
       HEADER,AT(1000,1000,10000,1000),USE(?Header)
       END
Detail DETAIL,USE(?Detail)
       END
       FOOTER,AT(1000,6500,10000,1000),USE(?Footer)
       END
       FORM,AT(1000,1000,10000,6500),USE(?Form)
       END
     END
[WINDOW]
ProgressWindow WINDOW('Progress...'),AT(,,142,59),CENTER,TIMER(1),GRAY,DOUBLE
       PROGRESS,USE(Progress:Thermometer,,?Progress:Thermometer),AT(15,15,111,12),RANGE(0,100),#SEQ(1),#ORIG(?Progress:Thermometer)
       STRING(''),AT(0,3,141,10),USE(?Progress:UserString),CENTER,#SEQ(1),#ORIG(?Progress:UserString)
       STRING(''),AT(0,30,141,10),USE(?Progress:PctText),CENTER,#SEQ(1),#ORIG(?Progress:PctText)
       BUTTON('Cancel'),AT(45,42,50,15),USE(?Progress:Cancel),#SEQ(1),#ORIG(?Progress:Cancel)
     END
#ENDDEFAULT
#DEFAULT
NAME DefaultADOReportLetterLandscape
CATEGORY 'ADOReport'
[COMMON]
DESCRIPTION 'Report (Paper size Letter - Landscape)'
FROM ABC Window
[ADDITION]
NAME ABC_ADO ProcessControl
[INSTANCE]
INSTANCE 1
PROCPROP
[PROMPTS]
%RecordsetIsParameter LONG  (0)

[REPORT]
Report REPORT,AT(1000,2000,9000,4500),PAPER(PAPER:LETTER),PRE(RPT),FONT('Arial',10,,FONT:regular,CHARSET:ANSI), |
         LANDSCAPE,THOUS
       HEADER,AT(1000,1000,9000,1000),USE(?Header)
       END
Detail DETAIL,USE(?Detail)
       END
       FOOTER,AT(1000,6500,9000,1000),USE(?Footer)
       END
       FORM,AT(1000,1000,9000,6500),USE(?Form)
       END
     END
[WINDOW]
ProgressWindow WINDOW('Progress...'),AT(,,142,59),CENTER,TIMER(1),GRAY,DOUBLE
       PROGRESS,USE(Progress:Thermometer,,?Progress:Thermometer),AT(15,15,111,12),RANGE(0,100),#SEQ(1),#ORIG(?Progress:Thermometer)
       STRING(''),AT(0,3,141,10),USE(?Progress:UserString),CENTER,#SEQ(1),#ORIG(?Progress:UserString)
       STRING(''),AT(0,30,141,10),USE(?Progress:PctText),CENTER,#SEQ(1),#ORIG(?Progress:PctText)
       BUTTON('Cancel'),AT(45,42,50,15),USE(?Progress:Cancel),#SEQ(1),#ORIG(?Progress:Cancel)
     END
#ENDDEFAULT
#DEFAULT
NAME DefaultADOReportLetterPortrait
CATEGORY 'ADOReport'
[COMMON]
DESCRIPTION 'Report (Paper size Letter - Portrait)'
FROM ABC Window
[ADDITION]
NAME ABC_ADO ProcessControl
[INSTANCE]
INSTANCE 1
PROCPROP
[PROMPTS]
%RecordsetIsParameter LONG  (0)

[REPORT]
Report REPORT,AT(1000,2000,6500,7000),PAPER(PAPER:LETTER),PRE(RPT),FONT('Arial',10,,FONT:regular,CHARSET:ANSI), |
         THOUS
       HEADER,AT(1000,1000,6500,1000),USE(?Header)
       END
Detail DETAIL,USE(?Detail)
       END
       FOOTER,AT(1000,9000,6500,1000),USE(?Footer)
       END
       FORM,AT(1000,1000,6500,9000),USE(?Form)
       END
     END
[WINDOW]
ProgressWindow WINDOW('Progress...'),AT(,,142,59),CENTER,TIMER(1),GRAY,DOUBLE
       PROGRESS,USE(Progress:Thermometer,,?Progress:Thermometer),AT(15,15,111,12),RANGE(0,100),#SEQ(1),#ORIG(?Progress:Thermometer)
       STRING(''),AT(0,3,141,10),USE(?Progress:UserString),CENTER,#SEQ(1),#ORIG(?Progress:UserString)
       STRING(''),AT(0,30,141,10),USE(?Progress:PctText),CENTER,#SEQ(1),#ORIG(?Progress:PctText)
       BUTTON('Cancel'),AT(45,42,50,15),USE(?Progress:Cancel),#SEQ(1),#ORIG(?Progress:Cancel)
     END
#ENDDEFAULT
#!---------------------------------------------------------------------
#!
#!
#PROCEDURE(ADOLoginProc, 'ADO Login Procedure'),WINDOW, PARENT(Window(ABC)), HLP('~TPLProcADOLogin.htm')
#RESTRICT
  #FOR( %ApplicationTemplate )
    #IF(UPPER(SUB(%ApplicationTemplate,1,10)) = 'ADOSUPPORT')
      #ACCEPT
    #ENDIF
  #ENDFOR
  #REJECT
#ENDRESTRICT
#DEFAULT
NAME DefaultADOLogin
GLOBAL
CATEGORY 'ADOLogin'
PROTOTYPE '(*CConnection pConn), long'
PARAMETERS '(*CConnection pConn)'
[COMMON]
DESCRIPTION 'ADO Login'
FROM ABC Window
[DATA]
RetValue    long
[PROMPTS]
%WindowOperationMode STRING  ('Use WINDOW setting')
%INISaveWindow LONG  (1)
%ReturnValue DEFAULT (RetValue)
[ADDITION]
NAME ABC_ADO ADOloginExtension
[INSTANCE]
INSTANCE 1
[ADDITION]
NAME ABC_ADO ADOLoginControl
[INSTANCE]
INSTANCE 2
[WINDOW]
Window WINDOW('Login '),AT(,,247,80),FONT('Microsoft Sans Serif',8,,FONT:regular,CHARSET:ANSI),GRAY,DOUBLE
       PROMPT('&User ID'),AT(21,11),USE(?PromptUserID)
       ENTRY(@s20),AT(65,11,161,10),USE(szUserID)
       PROMPT('&Password:'),AT(21,23),USE(?PromptPassword)
       ENTRY(@s20),AT(65,23,161,10),USE(szPassword),PASSWORD
       PROMPT('&Server:'),AT(21,35),USE(?PromptServer)
       ENTRY(@s20),AT(65,35,161,10),USE(szServer)
       CHECK('Blank Password'),AT(23,54),USE(ulBlankPassword)
       BUTTON('&Login'),AT(133,53,45,14),USE(?btnLogin), DEFAULT
       BUTTON('Cancel'),AT(181,53,45,14),USE(?btnCancel)
     END
#ENDDEFAULT
#PROCEDURE(ADOErrorsProc , 'ADO Errors Handling Procedure'), HLP('~TPLProcADOError.htm'),PARENT(Window(ABC))
#RESTRICT
  #FOR( %ApplicationTemplate )
    #IF(UPPER(SUB(%ApplicationTemplate,1,10)) = 'ADOSUPPORT')
      #ACCEPT
    #ENDIF
  #ENDFOR
  #REJECT
#ENDRESTRICT
#DEFAULT
NAME DefaultADOError
GLOBAL
CATEGORY 'ADOError'
PROTOTYPE '(*CConnection pConn)'
PARAMETERS '(*CConnection pConn)'
[COMMON]
DESCRIPTION 'ADO Errors Handling'
FROM ABC Window
[DATA]
ErrorQ                   QUEUE,PRE()
ErrorID                    LONG
ErrorDesc                  STRING(70)
                         END
oErrors                  &CErrors        ! Reference to the Errors Collection object
oError                   &CError         ! Reference to a single error contained in the Collection
ErrorCount               LONG ! Nbr of error in the collection
hr                       HRESULT         ! HRESULT var returned by methods call
lErrorID                 LONG ! Error ID returned by Number method
cstrDesc                 &Cstr           ! cstring class reference returned by CError.Description
ndx                      LONG
[PROMPTS]
%WindowOperationMode STRING  ('Use WINDOW setting')
%INISaveWindow LONG  (1)
[ADDITION]
NAME ABC_ADO ADOErrList
[INSTANCE]
INSTANCE 1
[PROMPTS]
%svConnObjectWithError DEFAULT  ('pConn')
[ADDITION]
NAME ABC CloseButton
[INSTANCE]
INSTANCE 2
[WINDOW]
Window WINDOW('Errors '),AT(,,397,161),FONT('Microsoft Sans Serif',8,,FONT:regular,CHARSET:ANSI),GRAY,DOUBLE
       LIST,AT(7,7,381,118),USE(?List1),VSCROLL,TIP('Tips'),FORMAT('69R(2)|M~Error Number~@N-_15@20L(2)|MP~Error Description~@s70@'), |
           #SEQ(1),#ORIG(?List1)
       BUTTON('Close'),AT(343,134,45,14),USE(?Close),#SEQ(2),#ORIG(?Close)
     END
#ENDDEFAULT
