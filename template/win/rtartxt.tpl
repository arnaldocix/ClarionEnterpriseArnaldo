#TEMPLATE(SVReportToTXTSupport, 'Report To TXT Support'),FAMILY('ABC','CW20')
#HELP('ClarionHelp.chm')
#!
#EXTENSION(SVReportToTXTGlobal, 'Report to TXT - Global'),APPLICATION(SVReportToTXT(SVReportToTXTSupport)),HLP('~TPLExtensionGlobalReportToTXT.htm')
#DISPLAY
#DISPLAY(' Report to TXT'),AT(3,,187), PROP(PROP:FontColor, 0FFFFFFH), PROP(PROP:Color,0808080H)
#DISPLAY
#DISPLAY('This template has added an extension to each report.')
#DISPLAY('Settings are modified at the procedure level.')
#PREPARE
    #CALL(%SetTargetGlobalPrompt,'TXT','TXTReporter','TextReportGenerator')
#ENDPREPARE
#INSERT(%TargetGlobalPrompts)
#ENABLE(NOT %GlobalDisable AND %GlobalUseABCClasess)
  #BUTTON('TXT Global String settings'),AT(10,,175),HLP('~TPLExtensionGlobalReportToTXTStringSettings.htm')
    #SHEET
      #TAB('CheckBox')
        #BOXED('CheckBox text')
          #PROMPT('Checked:',@S3),%TXTGlobalCheckBoxString,DEFAULT('[X]')
          #PROMPT('UnChecked:',@S3),%TXTGlobalUnCheckBoxString,DEFAULT('[_]')
        #ENDBOXED
        #ENDTAB
      #TAB('RadioButton')
        #BOXED('RadioButton Text')
          #PROMPT('Selected:',@S3),%TXTGlobalRadioButtonString,DEFAULT('(*)')
          #PROMPT('UnSelected:',@S3),%TXTGlobalUnRadioButtonString,DEFAULT('(_)')
        #ENDBOXED
      #ENDTAB
      #TAB('Lines')
        #BOXED('Lines && Boxes border')
          #PROMPT('Left Vertical Line:',@S1),%TXTGlobalLVLine,DEFAULT('|')
          #PROMPT('Right Vertical Line:',@S1),%TXTGlobalRVLine,DEFAULT('|')
          #PROMPT('Top Horizontal Line:',@S1),%TXTGlobalTHLine,DEFAULT('-')
          #PROMPT('Bottom Horizontal Line:',@S1),%TXTGlobalBHLine,DEFAULT('-')
          #PROMPT('Left Top Corner:',@S1),%TXTGlobalLTCorner,DEFAULT('/')
          #PROMPT('Right Top Corner:',@S1),%TXTGlobalRTCorner,DEFAULT('\')
          #PROMPT('Left Bottom Corner:',@S1),%TXTGlobalLBCorner,DEFAULT('\')
          #PROMPT('Right Bottom Corner:',@S1),%TXTGlobalRBCorner,DEFAULT('/')
        #ENDBOXED
      #ENDTAB
      #TAB('Fill Color')
        #BOXED('Boxes fill color')
          #PROMPT('Fill Color 1:',@S10),%TXTGlobalTextFill1,DEFAULT(''' ''')
          #PROMPT('Fill Color 2:',@S10),%TXTGlobalTextFill2,DEFAULT('CHR(176)')
          #PROMPT('Fill Color 3:',@S10),%TXTGlobalTextFill3,DEFAULT('CHR(177)')
          #PROMPT('Fill Color 4:',@S10),%TXTGlobalTextFill4,DEFAULT('CHR(178)')
          #PROMPT('Fill Color 5:',@S10),%TXTGlobalTextFill5,DEFAULT('CHR(219)')
        #ENDBOXED
      #ENDTAB
    #ENDSHEET
  #ENDBUTTON
#ENDENABLE
#!
#EXTENSION(SVReportToTXT,'Report to TXT'),PROCEDURE,REQ(SVReportToTXTGlobal),HLP('~TPLExtensionLocalReportToTXT.htm')
#RESTRICT
  #IF(UPPER(%ProcedureTemplate)='REPORT')
    #ACCEPT
  #ELSE
    #REJECT
  #ENDIF
#ENDRESTRICT
#PREPARE
  #CALL(%ReadABCFiles)
  #CALL(%SetClassDefaults, %lTargetObjectName,%lTargetObjectName,%lTargetObjectClass)
#ENDPREPARE
  #BOXED,HIDE
    #INSERT(%OOPHiddenPrompts)
  #ENDBOXED
  #DISPLAY(' Report to TXT'),AT(3,,190), PROP(PROP:FontColor, 0FFFFFFH), PROP(PROP:Color,0808080H)
  #BOXED,SECTION
     #BOXED,WHERE(%GlobalDisable OR NOT %GlobalUseABCClasess),AT(0,0)
       #DISPLAY('This option has been disabled at the global')
       #DISPLAY('level and is not available.')
     #ENDBOXED
     #BOXED,WHERE(NOT %GlobalDisable AND NOT %EnablePrintPreview AND %GlobalUseABCClasess),AT(0,0)
       #DISPLAY('This option has been disabled because the')
       #DISPLAY('report require the preview on.')
     #ENDBOXED                      
     #BOXED,WHERE(NOT %GlobalDisable AND %EnablePrintPreview AND %GlobalUseABCClasess),AT(0,0)
       #PROMPT('Populate on Report',DROP('Always|Conditional|Never')),%Populate,DEFAULT('Always')
       #BOXED,SECTION
         #BOXED,WHERE(%Populate='Never' AND NOT %GlobalDisable AND %GlobalUseABCClasess),AT(0,0)
           #DISPLAY('This option will not be available for this report.')
         #ENDBOXED
         #BOXED,WHERE(%Populate<>'Never' AND NOT %GlobalDisable AND %GlobalUseABCClasess),AT(0,0)
           #ENABLE(%Populate='Conditional')
             #PROMPT('Condition:',EDIT),%PopulateCondition,WHENACCEPTED(%SVExpresionEditor(%PopulateCondition))
           #ENDENABLE
         #ENDBOXED
       #ENDBOXED
       #BOXED,WHERE(%Populate<>'Never')
         #SHEET
           #TAB('General'),HLP('~TPLExtensionLocalReportToTXTGeneral.htm')
             #PREPARE
               #IF(NOT %TXTOverrideGlobalString)
                 #CALL(%ResetStringFromGlobal)
               #ENDIF
             #ENDPREPARE
             #PROMPT('Output Name Type:',DROP('Fixed[Fixed]|Variable[Variable]|Runtime[Runtime]')),%OutputNameType,DEFAULT('Runtime')
             #BOXED,SECTION
               #BOXED,WHERE(%OutputNameType='Fixed'),AT(0,0)
                 #PROMPT('File Name:',OPENDIALOG('Pick File','All|*.*')),%OutputNameFixed
               #ENDBOXED
               #BOXED,WHERE(%OutputNameType='Variable'),AT(0,0)
                 #PROMPT('File Name:',EXPR),%OutputNameVariable,REQ
               #ENDBOXED
               #BOXED,WHERE(%OutputNameType='Runtime'),AT(0,0)
                 #DISPLAY('The User will be prompted at runtime.')
               #ENDBOXED
             #ENDBOXED
             #PROMPT('Save output as separate files?',CHECK),%PagesAsDifferentFile,DEFAULT(%False),AT(10)
             #DISPLAY('Each file will be named as selected with the')
             #DISPLAY('page number added; e.g.')
             #DISPLAY('"SAVEnnn.TXT", where "nnn" is the page number.')
             #BOXED('TXT Options')
               #BUTTON('Margin and Page Length'),AT(10,,175),HLP('~TPLExtensionLocalReportToTXTMargin.htm')
                 #PROMPT('Initial Margin:',SPIN(@n3,0,200,1)),%TXTInitialMargin,DEFAULT(0)
                 #PROMPT('Top Margin:',SPIN(@n3,0,200,1)),%TXTTopMargin,DEFAULT(0)
                 #PROMPT('Bottom Margin:',SPIN(@n3,0,200,1)),%TXTBottomMargin,DEFAULT(0)
                 #PROMPT('Left Margin:',SPIN(@n3,0,200,1)),%TXTLeftMargin,DEFAULT(0)
                 #PROMPT('Page Length:',SPIN(@n3,0,600,1)),%TXTPageLen,DEFAULT(0)
                 #DISPLAY('If page length = 0 the report page size is used.')
                 #DISPLAY('The numbers unit are columns and rows.')
               #ENDBUTTON
               #PROMPT('Override global string settings?',CHECK),%TXTOverrideGlobalString,DEFAULT(%False),AT(10)
               #ENABLE(%TXTOverrideGlobalString)
                 #BUTTON('TXT Local String settings'),AT(10,,175),HLP('~TPLExtensionLocalReportToTXTStringSettings.htm')
                   #SHEET
                     #TAB('CheckBox')
                       #BOXED('CheckBox text')
                         #PROMPT('Checked:',@S3),%TXTLocalCheckBoxString,DEFAULT('[X]')
                         #PROMPT('UnChecked:',@S3),%TXTLocalUnCheckBoxString,DEFAULT('[_]')
                       #ENDBOXED
                     #ENDTAB
                     #TAB('RadioButton')
                       #BOXED('RadioButton Text')
                         #PROMPT('Selected:',@S3),%TXTLocalRadioButtonString,DEFAULT('(*)')
                         #PROMPT('UnSelected:',@S3),%TXTLocalUnRadioButtonString,DEFAULT('(_)')
                       #ENDBOXED
                     #ENDTAB
                     #TAB('Lines')
                       #BOXED('Lines && Boxes border')
                         #PROMPT('Left Vertical Line:',@S1),%TXTLocalLVLine,DEFAULT('|')
                         #PROMPT('Right Vertical Line:',@S1),%TXTLocalRVLine,DEFAULT('|')
                         #PROMPT('Top Horizontal Line:',@S1),%TXTLocalTHLine,DEFAULT('-')
                         #PROMPT('Bottom Horizontal Line:',@S1),%TXTLocalBHLine,DEFAULT('-')
                         #PROMPT('Left Top Corner:',@S1),%TXTLocalLTCorner,DEFAULT('/')
                         #PROMPT('Right Top Corner:',@S1),%TXTLocalRTCorner,DEFAULT('\')
                         #PROMPT('Left Bottom Corner:',@S1),%TXTLocalLBCorner,DEFAULT('\')
                         #PROMPT('Right Bottom Corner:',@S1),%TXTLocalRBCorner,DEFAULT('/')
                       #ENDBOXED
                     #ENDTAB
                     #TAB('Fill Color')
                       #BOXED('Boxes fill color')
                         #PROMPT('Fill Color 1:',@S10),%TXTLocalTextFill1,DEFAULT(''' ''')
                         #PROMPT('Fill Color 2:',@S10),%TXTLocalTextFill2,DEFAULT('CHR(176)')
                         #PROMPT('Fill Color 3:',@S10),%TXTLocalTextFill3,DEFAULT('CHR(177)')
                         #PROMPT('Fill Color 4:',@S10),%TXTLocalTextFill4,DEFAULT('CHR(178)')
                         #PROMPT('Fill Color 5:',@S10),%TXTLocalTextFill5,DEFAULT('CHR(219)')
                       #ENDBOXED
                     #ENDTAB
                   #ENDSHEET
                   #BUTTON('Reset from Global'),AT(10,,175),WHENACCEPTED(%ResetStringFromGlobal())
                   #ENDBUTTON
                 #ENDBUTTON
               #ENDENABLE
               #PROMPT('Omit Lines && Boxes?',CHECK),%TXTOmitLinesBoxes,DEFAULT(%False),AT(10)
               #DISPLAY('(also the Groups && Options lines)')
             #ENDBOXED
             #DISPLAY('To obtain a better txt output it''s recomended')
             #DISPLAY('that you use a font Courrier New, size 11')
             #DISPLAY('and a grid size of H:65 V:163')
           #ENDTAB
           #TAB('Controls'),HLP('~TPLExtensionLocalReportToTXTControls.htm')
             #BUTTON('Control''s TXT Properties'),FROM(%ReportControl,%GetTagDisplay()),INLINE,WHERE(%TXTIsValidControl()),HLP('~TPLExtensionLocalReportToTXTControlsProperties.htm')
               #DISPLAY(' Control: '&%ReportControl),AT(3,,190), PROP(PROP:FontColor, 0FFFFFFH), PROP(PROP:Color,0808080H)
               #BOXED('')
                 #BOXED,SECTION
                   #BOXED,WHERE(%True),AT(0,0)
                     #PROMPT('HIDE (Omit printing)',CHECK),%TXTHideControl,DEFAULT(%False),AT(10)
                     #PROMPT('SKIP (Omit processing)',CHECK),%TXTSkipControl,DEFAULT(%False),AT(10)
                   #ENDBOXED
                 #ENDBOXED
               #ENDBOXED  
             #ENDBUTTON
           #ENDTAB
           #INSERT(%TargetClassTab)
         #ENDSHEET
       #ENDBOXED
     #ENDBOXED
  #ENDBOXED
#!
#!
#!
#ATSTART
 #IF(NOT %GlobalDisable AND %Populate<>'Never' AND %EnablePrintPreview AND %GlobalUseABCClasess)
    #FOR(%ReportControl),WHERE(%TXTIsValidControl() AND %TXTHideControl)
        #CALL(%AddHidenControlToReport,'TXT',%ReportControl)
    #ENDFOR
    #CALL(%SetClassDefaults,%lTargetObjectName,%lTargetObjectName,%lTargetObjectClass)
    #IF(%Populate='Conditional')
       #CALL(%AddTargetToReport,%lTargetName,%lTargetObjectName,%lTargetObjectClass,%PopulateCondition)
    #ELSE
       #CALL(%AddTargetToReport,%lTargetName,%lTargetObjectName,%lTargetObjectClass,'')
    #ENDIF
 #ENDIF
 #DECLARE(%AttributeObjectName)
 #IF(%AppTemplateFamily='CLARION')
    #SET(%AttributeObjectName,'LocalAttribute')
 #ELSE
    #SET(%AttributeObjectName,'SELF.Attribute')
 #ENDIF
#ENDAT
#AT(%GatherObjects)
#IF(NOT %GlobalDisable AND %Populate<>'Never' AND %EnablePrintPreview AND %GlobalUseABCClasess)
    #CALL(%AddObjectList, %lTargetObjectName)
#ENDIF
#ENDAT
#AT(%LocalProcedures)
   #IF(NOT %GlobalDisable AND %Populate<>'Never' AND %EnablePrintPreview AND %GlobalUseABCClasess)
      #CALL(%GenerateVirtuals, %lTargetObjectName, 'Local Objects|Abc Objects|Report Target|'&%lTargetName, '%ReportTargetVirtuals(SVReportToTXTSupport)')
   #ENDIF
#ENDAT
#AT(%LocalDataClasses)
   #IF(NOT %GlobalDisable AND %Populate<>'Never' AND %EnablePrintPreview AND %GlobalUseABCClasess)
#INSERT(%GenerateClass, %lTargetObjectName, %lTargetName)
   #ENDIF
#ENDAT
#AT(%ReportTargetMethodCodeSection,%lTargetName),PRIORITY(5000),DESCRIPTION('Parent Call'),WHERE(%ParentCallValid())
#IF(NOT %GlobalDisable AND %Populate<>'Never' AND %EnablePrintPreview AND %GlobalUseABCClasess)
  #CALL(%GenerateParentCall)
#ENDIF
#ENDAT
#AT(%ReportTargetMethodCodeSection,'TXT','Setup','()'),PRIORITY(5500),WHERE(NOT %GlobalDisable AND %Populate<>'Never' AND %EnablePrintPreview AND %GlobalUseABCClasess)
#CASE(%OutputNameType)
#OF('Fixed')
SELF.SetFileName(%(%SVStripPling(%OutputNameFixed)))
#OF('Variable')
SELF.SetFileName(%OutputNameVariable)
#OF('Runtime')
SELF.SetFileName('')
#ENDCASE
SELF.SetPagesAsDifferentFile(%(CHOOSE(%PagesAsDifferentFile,'True','False')))
SELF.SetMargin(%TXTInitialMargin,%TXTTopMargin,%TXTBottomMargin,%TXTLeftMargin)
#IF(%TXTPageLen)
SELF.SetPageLen(%TXTPageLen)
#ENDIF
SELF.SetCheckBoxString('%'TXTLocalCheckBoxString','%'TXTLocalUnCheckBoxString')
SELF.SetRadioButtonString('%'TXTLocalRadioButtonString','%'TXTLocalUnRadioButtonString')
SELF.SetLineString('%'TXTLocalLVLine','%'TXTLocalRVLine','%'TXTLocalTHLine','%'TXTLocalBHLine','%'TXTLocalLTCorner','%'TXTLocalRTCorner','%'TXTLocalLBCorner','%'TXTLocalRBCorner')
SELF.SetTextFillString(%TXTLocalTextFill1,%TXTLocalTextFill2,%TXTLocalTextFill3,%TXTLocalTextFill4,%TXTLocalTextFill5)
SELF.SetOmitGraph(%(CHOOSE(%TXTOmitLinesBoxes,'True','False')))
#ENDAT
#!
#AT(%SetStaticControlsAttributes),PRIORITY(4500),WHERE(NOT %GlobalDisable AND %Populate<>'Never' AND %EnablePrintPreview AND %AppTemplateFamily='CLARION' AND %GlobalUseABCClasess)
#FOR(%ReportControl),WHERE(%TXTIsValidControl())
  #IF(%TXTSkipControl)
%AttributeObjectName.Set(%ReportControl,RepGen:TXT,TargetAttr:Skip,%ReportControl)
  #ENDIF
#ENDFOR
#ENDAT
#!
#AT(%SetDynamicControlsAttributes),PRIORITY(4500),WHERE(NOT %GlobalDisable AND %Populate<>'Never' AND %EnablePrintPreview AND %AppTemplateFamily='CLARION' AND %GlobalUseABCClasess)
#ENDAT
#!
#AT(%WindowManagerMethodCodeSection,'SetStaticControlsAttributes','()'),PRIORITY(5500),WHERE(NOT %GlobalDisable AND %Populate<>'Never' AND %EnablePrintPreview AND %AppTemplateFamily='ABC' AND %GlobalUseABCClasess)
#FOR(%ReportControl),WHERE(%TXTIsValidControl())
  #IF(%TXTSkipControl)
%AttributeObjectName.Set(%ReportControl,RepGen:TXT,TargetAttr:Skip,%ReportControl)
  #ENDIF
#ENDFOR
#ENDAT
#AT(%WindowManagerMethodCodeSection,'SetDynamicControlsAttributes','()'),PRIORITY(5500),WHERE(NOT %GlobalDisable AND %Populate<>'Never' AND %EnablePrintPreview AND %AppTemplateFamily='ABC' AND %GlobalUseABCClasess)
#ENDAT
#!
#GROUP(%GetTagDisplay),AUTO
#DECLARE(%lReturnValue)
#IF(%TXTSkipControl)
   #SET(%lReturnValue,%ReportControl&' - SKIP')
#ELSE
   #SET(%lReturnValue,%ReportControl)
#ENDIF
#RETURN(%lReturnValue)
#!
#!
#!
#!
#GROUP(%ResetStringFromGlobal)
#SET(%TXTLocalCheckBoxString,%TXTGlobalCheckBoxString)
#SET(%TXTLocalUnCheckBoxString,%TXTGlobalUnCheckBoxString)
#SET(%TXTLocalRadioButtonString,%TXTGlobalRadioButtonString)
#SET(%TXTLocalUnRadioButtonString,%TXTGlobalUnRadioButtonString)
#SET(%TXTLocalLVLine,%TXTGlobalLVLine)
#SET(%TXTLocalRVLine,%TXTGlobalRVLine)
#SET(%TXTLocalTHLine,%TXTGlobalTHLine)
#SET(%TXTLocalBHLine,%TXTGlobalBHLine)
#SET(%TXTLocalLTCorner,%TXTGlobalLTCorner)
#SET(%TXTLocalRTCorner,%TXTGlobalRTCorner)
#SET(%TXTLocalLBCorner,%TXTGlobalLBCorner)
#SET(%TXTLocalRBCorner,%TXTGlobalRBCorner)
#SET(%TXTLocalTextFill1,%TXTGlobalTextFill1)
#SET(%TXTLocalTextFill2,%TXTGlobalTextFill2)
#SET(%TXTLocalTextFill3,%TXTGlobalTextFill3)
#SET(%TXTLocalTextFill4,%TXTGlobalTextFill4)
#SET(%TXTLocalTextFill5,%TXTGlobalTextFill5)
#!
#!
#GROUP(%TXTIsValidControl),AUTO
#DECLARE(%lReturnValue)
#SET(%lReturnValue,%False)
#IF(NOT %ReportControl)
   #RETURN(%False)
#ENDIF
#CASE(%ReportControlType)
#OF('LIST')
#OROF('HEADER')
#OROF('DETAIL')
#OROF('FORM')
#OROF('FOOTER')
#OROF('BREAK')
#OROF('ELLIPSE')
#OROF('IMAGE')
   #SET(%lReturnValue,%False)
#OF('GROUP')
#OROF('OPTION')
   #IF(INSTRING(',BOXED',%ReportControlStatement,1,1)>0)
       #SET(%lReturnValue,%True)
   #ELSE
       #SET(%lReturnValue,%False)
   #ENDIF
#OF('LINE')
#OROF('BOX')
#OROF('RADIO')
#OROF('STRING')
#OROF('TEXT')
#OROF('CHECK')
   #SET(%lReturnValue,%True)
#ELSE
   #ERROR('ReportToPDF: %%PDFIsValidControl (Control not supported)')
#ENDCASE
#RETURN(%lReturnValue)
#!
#!
#INCLUDE('rtar_srv.TPW')
#!
