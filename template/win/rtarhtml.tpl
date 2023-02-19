#TEMPLATE(SVReportToHTMLSupport, 'Report To HTML Support'),FAMILY('ABC','CW20')
#HELP('ClarionHelp.chm')
#!
#EXTENSION(SVReportToHTMLGlobal, 'Report to HTML - Global'),APPLICATION(SVReportToHTML(SVReportToHTMLSupport)),HLP('~TPLExtensionGlobalReportToHTML.htm')
#DISPLAY
#DISPLAY(' Report to HTML'),AT(3,,187), PROP(PROP:FontColor, 0FFFFFFH), PROP(PROP:Color,0808080H)
#DISPLAY
#DISPLAY('This template has added an extension to each report.')
#DISPLAY('Settings are modified at the procedure level.')
#PREPARE
  #CALL(%SetTargetGlobalPrompt,'HTML','HTMLReporter','HTMLReportGenerator')
#ENDPREPARE
#INSERT(%TargetGlobalPrompts)
#ENABLE(NOT %GlobalDisable AND %GlobalUseABCClasess)
  #BUTTON('Html Global String settings'),AT(10,,175),HLP('~TPLExtensionGlobalReportToHTMLStringSettings.htm')
    #BOXED('Navigation Options')
      #PROMPT('First Page Text:',EXPR),%GlobalFirstPageText,REQ,DEFAULT('First')
      #PROMPT('Last Page Text:',EXPR),%GlobalLastPageText,REQ,DEFAULT('Last')
      #PROMPT('Next Page Text:',EXPR),%GlobalNextPageText,REQ,DEFAULT('Next')
      #PROMPT('Prior Page Text:',EXPR),%GlobalPriorPageText,REQ,DEFAULT('Prior')
      #PROMPT('No Page Selected:',EXPR),%GlobalNoPageText,REQ,DEFAULT('Select Page')
      #PROMPT('Page Prefix:',EXPR),%GlobalPagePrefix,REQ,DEFAULT('Page_')
      #PROMPT('Go link text:',EXPR),%GlobalPageGotext,REQ,DEFAULT('Load Page')
      #PROMPT('Create Single File',CHECK),%GlobalCreateSingleFile,DEFAULT(%False)
      #PROMPT('Create Sub Directory',CHECK),%GlobalCreateSubDirectory,DEFAULT(%True)
      #ENABLE(%GlobalCreateSubDirectory)
        #ENABLE(NOT %GlobalCreateSingleFile)
           #PROMPT('Files Directory:',EXPR),%GlobalFilesDirectory,REQ,DEFAULT('_Files')
        #ENDENABLE
        #PROMPT('Images Directory:',EXPR),%GlobalImagesDirectory,REQ,DEFAULT('_Images')
      #ENDENABLE
    #ENDBOXED
  #ENDBUTTON
#ENDENABLE
#!
#EXTENSION(SVReportToHTML,'Report to HTML'),PROCEDURE,HLP('~TPLExtensionLocalReportToHTML.htm'),REQ(SVReportToHTMLGlobal)
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
  #DISPLAY(' Report to HTML'),AT(3,,190), PROP(PROP:FontColor, 0FFFFFFH), PROP(PROP:Color,0808080H)
  #BOXED,SECTION
    #BOXED,WHERE(%GlobalDisable OR NOT %GlobalUseABCClasess),AT(0,0)
      #DISPLAY('This option has been disabled at the global')
      #DISPLAY('level and is not available.')
    #ENDBOXED
    #BOXED,WHERE(NOT %GlobalDisable AND %GlobalUseABCClasess AND NOT %EnablePrintPreview),AT(0,0)
      #DISPLAY('This option has been disabled because the')
      #DISPLAY('report require the preview on.')
    #ENDBOXED                      
    #BOXED,WHERE(NOT %GlobalDisable  AND %GlobalUseABCClasess AND %EnablePrintPreview),AT(0,0)
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
          #TAB('General'),HLP('~TPLExtensionLocalReportToHTML.htm#General')
            #PREPARE
              #IF(NOT %HtmlOverrideGlobalString)
                #CALL(%ResetStringFromGlobal)
              #ENDIF
            #ENDPREPARE
            #PROMPT('Output Name Type:',DROP('Fixed|Variable|Runtime')),%OutputNameType,DEFAULT('Runtime')
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
            #DISPLAY('Each file will be named as selected with the')
            #DISPLAY('page number added; e.g.')
            #DISPLAY('"SAVE_nnn.htm", where "nnn" is the page number.')
            #DISPLAY('The CSS file will be named with the same page name')
            #DISPLAY('but using the extension CSS.')
            #DISPLAY('The Navigation file will be named with ')
            #DISPLAY('the same page name but appending _pgNav.htm')
            #DISPLAY('and the Index will be _Index.htm.')
            #BOXED('HTML Options')
              #PROMPT('Document Name:',EXPR),%DocumentName,REQ,DEFAULT('Clarion Report')
              #PROMPT('Override global string settings?',CHECK),%HtmlOverrideGlobalString,DEFAULT(%False),AT(10)
              #ENABLE(%HtmlOverrideGlobalString)
                #BUTTON('Html String settings'),AT(10,,175),HLP('~TPLExtensionLocalReportToHTMLStringSettings.htm')
                  #BOXED('Navigation Options')
                    #PROMPT('First Page Text:',EXPR),%FirstPageText,REQ,DEFAULT('First')
                    #PROMPT('Last Page Text:',EXPR),%LastPageText,REQ,DEFAULT('Last')
                    #PROMPT('Next Page Text:',EXPR),%NextPageText,REQ,DEFAULT('Next')
                    #PROMPT('Prior Page Text:',EXPR),%PriorPageText,REQ,DEFAULT('Prior')
                    #PROMPT('No Page Selected:',EXPR),%NoPageText,REQ,DEFAULT('Select Page')
                    #PROMPT('Page Prefix:',EXPR),%PagePrefix,REQ,DEFAULT('Page_')
                    #PROMPT('Go link text:',EXPR),%PageGotext,REQ,DEFAULT('Load Page')
                    #PROMPT('Create Single File',CHECK),%CreateSingleFile,DEFAULT(%False)
                    #PROMPT('Create Sub Directory',CHECK),%CreateSubDirectory,DEFAULT(%True)
                    #ENABLE(%CreateSubDirectory)
                      #ENABLE(NOT %CreateSingleFile)
                         #PROMPT('Files Directory:',EXPR),%FilesDirectory,REQ,DEFAULT('_Files')
                      #ENDENABLE
                      #PROMPT('Images Directory:',EXPR),%ImagesDirectory,REQ,DEFAULT('_Images')
                    #ENDENABLE
                  #ENDBOXED
                #ENDBUTTON
              #ENDENABLE
            #ENDBOXED
          #ENDTAB
          #TAB('Controls'),HLP('~TPLExtensionLocalReportToHTMLControls.htm')
            #BUTTON('Control''s HTML Properties'),FROM(%ReportControl,%GetTagDisplay()),INLINE,WHERE(%HTMLIsValidControl()),HLP('~TPLExtensionLocalReportToHTML.htm#ControlsProperties')
              #DISPLAY(' Control: '&%ReportControl),AT(3,,190), PROP(PROP:FontColor, 0FFFFFFH), PROP(PROP:Color,0808080H)
              #BOXED('')
                #BOXED,SECTION
                  #BOXED,WHERE(%True),AT(0,0)
                    #PROMPT('HIDE (Omit printing)',CHECK),%HTMLHideControl,DEFAULT(%False),AT(10)
                    #PROMPT('SKIP (Omit processing)',CHECK),%HTMLSkipControl,DEFAULT(%False),AT(10)
                  #ENDBOXED
                  #BOXED,WHERE(NOT %HTMLSkipControl AND %ReportControlType='STRING' OR %ReportControlType='IMAGE')
                    #BOXED('Hyperlink')
                      #PROMPT('Target Origin:',DROP('Control Text[Text]|HardCoded[HardCoded]|Dynamic[Dynamic]|None[None]')),%HTMLTargetOrigin,DEFAULT('None')
                      #BOXED,SECTION
                        #BOXED,WHERE(%HTMLTargetOrigin='None'),AT(0,0)
                          #DISPLAY('There are no Hyperlink')
                        #ENDBOXED
                        #BOXED,WHERE(%HTMLTargetOrigin='Text'),AT(0,0)
                          #DISPLAY('The Target will be the text displayed on the report')
                        #ENDBOXED
                        #BOXED,WHERE(%HTMLTargetOrigin='HardCoded'),AT(0,0)
                          #PROMPT('Target:',EXPR),%HTMLTargetHardCoded
                        #ENDBOXED
                        #BOXED,WHERE(%HTMLTargetOrigin='Dynamic'),AT(0,0)
                          #PROMPT('Target:',EXPR),%HTMLTargetDynamic
                        #ENDBOXED
                      #ENDBOXED
                      #ENABLE(%HTMLTargetOrigin<>'None')
                        #PROMPT('Hyperlink Type:',DROP('HTTP|MAIL|FTP|NEWS|Other|Other Dynamic[Dynamic]|None')),%HTMLHyperlinkType,DEFAULT('HTTP')
                        #BOXED,SECTION
                          #BOXED,WHERE(%HTMLHyperlinkType='HTTP'),AT(0,0)
                            #DISPLAY('The http:// will be added to the Hyperlink')
                          #ENDBOXED
                          #BOXED,WHERE(%HTMLHyperlinkType='MAIL'),AT(0,0)
                            #DISPLAY('The mailto: will be added to the Hyperlink')
                          #ENDBOXED
                          #BOXED,WHERE(%HTMLHyperlinkType='FTP'),AT(0,0)
                            #DISPLAY('The ftp:// will be added to the Hyperlink')
                          #ENDBOXED
                          #BOXED,WHERE(%HTMLHyperlinkType='NEWS'),AT(0,0)
                            #DISPLAY('The news: will be added to the Hyperlink')
                          #ENDBOXED
                          #BOXED,WHERE(%HTMLHyperlinkType='None'),AT(0,0)
                            #DISPLAY('The Hyperlink will be used as is')
                          #ENDBOXED
                          #BOXED,WHERE(%HTMLHyperlinkType='Other'),AT(0,0)
                            #PROMPT('Other Type:',EXPR),%HTMLHyperlinkTypeOther
                          #ENDBOXED
                          #BOXED,WHERE(%HTMLHyperlinkType='Dynamic'),AT(0,0)
                            #PROMPT('Other Type:',EXPR),%HTMLHyperlinkTypeOtherDynamic
                          #ENDBOXED
                        #ENDBOXED
                        #PROMPT('Hyperlink Target:',DROP('None[None]|Same Browser[_top]|Other Browser[_blank]')),%HTMLHyperlinkTarget,DEFAULT('_blank')
                      #ENDENABLE
                    #ENDBOXED
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
    #IF(NOT %HtmlOverrideGlobalString)
      #CALL(%ResetStringFromGlobal)
    #ENDIF
    #FOR(%ReportControl),WHERE(%HTMLIsValidControl() AND %HTMLHideControl)
      #CALL(%AddHidenControlToReport,'HTML',%ReportControl)
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
    #CALL(%GenerateVirtuals, %lTargetObjectName, 'Local Objects|Abc Objects|Report Target|'&%lTargetName, '%ReportTargetVirtuals(SVReportToHTMLSupport)')
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
#AT(%ReportTargetMethodCodeSection,'HTML','Setup','()'),PRIORITY(5500),WHERE(NOT %GlobalDisable AND %Populate<>'Never' AND %EnablePrintPreview AND %GlobalUseABCClasess)
#CASE(%OutputNameType)
#OF('Fixed')
SELF.SetFileName(%(%SVStripPling(%OutputNameFixed)))
#OF('Variable')
SELF.SetFileName(%OutputNameVariable)
#OF('Runtime')
SELF.SetFileName('')
#ENDCASE
SELF.SetDocumentName(%(%SVStripPling(%DocumentName)))
SELF.SetNavigationText(%(%SVStripPling(%FirstPageText)),%(%SVStripPling(%LastPageText)),%(%SVStripPling(%NextPageText)),%(%SVStripPling(%PriorPageText)),%(%SVStripPling(%NoPageText)),%(%SVStripPling(%PagePrefix)),%(%SVStripPling(%PageGotext)))
SELF.SetSubDirectory(%CreateSubDirectory,%(%SVStripPling(%FilesDirectory)),%(%SVStripPling(%ImagesDirectory)))
SELF.SetSingleFile(%CreateSingleFile)
#ENDAT
#!
#AT(%SetStaticControlsAttributes),PRIORITY(4500),WHERE(NOT %GlobalDisable AND %Populate<>'Never' AND %EnablePrintPreview AND %AppTemplateFamily='CLARION' AND %GlobalUseABCClasess)
#INSERT(%CodeSetStaticControlsAttributes)
#ENDAT
#!
#AT(%SetDynamicControlsAttributes),PRIORITY(4500),WHERE(NOT %GlobalDisable AND %Populate<>'Never' AND %EnablePrintPreview AND %AppTemplateFamily='CLARION' AND %GlobalUseABCClasess)
#INSERT(%CodeSetDynamicControlsAttributes)
#ENDAT
#!
#AT(%WindowManagerMethodCodeSection,'SetStaticControlsAttributes','()'),PRIORITY(5500),WHERE(NOT %GlobalDisable AND %Populate<>'Never' AND %EnablePrintPreview AND %AppTemplateFamily='ABC' AND %GlobalUseABCClasess)
#INSERT(%CodeSetStaticControlsAttributes)
#ENDAT
#!
#AT(%WindowManagerMethodCodeSection,'SetDynamicControlsAttributes','()'),PRIORITY(5500),WHERE(NOT %GlobalDisable AND %Populate<>'Never' AND %EnablePrintPreview AND %AppTemplateFamily='ABC' AND %GlobalUseABCClasess)
#INSERT(%CodeSetDynamicControlsAttributes)
#ENDAT
#!
#GROUP(%GetTagDisplay),AUTO
#DECLARE(%lReturnValue)
#SET(%lReturnValue,%ReportControl)
#IF(%HTMLSkipControl)
  #SET(%lReturnValue,CLIP(%lReturnValue)&' - SKIP')
#ENDIF
#IF(%HTMLTargetOrigin<>'None')
  #SET(%lReturnValue,CLIP(%lReturnValue)&' - Hyper')
  #IF(%HTMLHyperlinkType<>'None')
    #SET(%lReturnValue,CLIP(%lReturnValue)&' - Type:'&%HTMLHyperlinkType)
  #ENDIF
#ENDIF
#RETURN(%lReturnValue)
#!
#!
#GROUP(%ResetStringFromGlobal)
#SET(%FirstPageText,%GlobalFirstPageText)
#SET(%LastPageText,%GlobalLastPageText)
#SET(%NextPageText,%GlobalNextPageText)
#SET(%PriorPageText,%GlobalPriorPageText)
#SET(%NoPageText,%GlobalNoPageText)
#SET(%PagePrefix,%GlobalPagePrefix)
#SET(%PageGotext,%GlobalPageGotext)
#SET(%CreateSingleFile,%GlobalCreateSingleFile)
#SET(%CreateSubDirectory,%GlobalCreateSubDirectory)
#SET(%FilesDirectory,%GlobalFilesDirectory)
#SET(%ImagesDirectory,%GlobalImagesDirectory)
#!
#!
#GROUP(%HTMLIsValidControl),AUTO
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
#OROF('IMAGE')
  #SET(%lReturnValue,%True)
#ELSE
  #ERROR('ReportToHtml: %%HtmlIsValidControl (Control not supported)')
#ENDCASE
#RETURN(%lReturnValue)
#!
#!
#!
#GROUP(%CodeSetStaticControlsAttributes)
#FOR(%ReportControl),WHERE(%HTMLIsValidControl())
  #IF(%HTMLSkipControl)
%AttributeObjectName.Set(%ReportControl,RepGen:HTML,TargetAttr:Skip,%ReportControl)
  #ELSE
    #IF((%ReportControlType='STRING' OR %ReportControlType='IMAGE') AND %HTMLTargetOrigin<>'None')
      #IF(%HTMLTargetOrigin<>'None')
        #IF(%HTMLHyperlinkTarget<>'None')
%AttributeObjectName.Set(%ReportControl,RepGen:HTML,TargetAttr:HyperlinkTarget,%(%SVStripPling(%HTMLHyperlinkTarget)))
        #ENDIF
        #CASE(%HTMLHyperlinkType)
        #OF('HTTP')
%AttributeObjectName.Set(%ReportControl,RepGen:HTML,TargetAttr:HyperlinkType,'http://')
        #OF('MAIL')
%AttributeObjectName.Set(%ReportControl,RepGen:HTML,TargetAttr:HyperlinkType,'mailto:')
        #OF('FTP')
%AttributeObjectName.Set(%ReportControl,RepGen:HTML,TargetAttr:HyperlinkType,'ftp://')
        #OF('NEWS')
%AttributeObjectName.Set(%ReportControl,RepGen:HTML,TargetAttr:HyperlinkType,'news:')
        #OF('Other')
%AttributeObjectName.Set(%ReportControl,RepGen:HTML,TargetAttr:HyperlinkType,%(%SVStripPling(%HTMLHyperlinkTypeOther)))
        #ENDCASE
      #ENDIF
      #CASE(%HTMLTargetOrigin)
      #OF('Text')
%AttributeObjectName.Set(%ReportControl,RepGen:HTML,TargetAttr:Hyperlink,TargetAttr:Hyperlink)
      #OF('HardCoded')
%AttributeObjectName.Set(%ReportControl,RepGen:HTML,TargetAttr:Hyperlink,%(%SVStripPling(%HtmlTargetHardCoded)))
      #ENDCASE
    #ENDIF
  #ENDIF
#ENDFOR
#!
#!
#GROUP(%CodeSetDynamicControlsAttributes)
#FOR(%ReportControl),WHERE(%HTMLIsValidControl())
  #IF((%ReportControlType='STRING' OR %ReportControlType='IMAGE'))
    #IF(%HTMLTargetOrigin<>'None')
      #IF(%HTMLHyperlinkType='Dynamic')
%AttributeObjectName.Set(%ReportControl,RepGen:HTML,TargetAttr:HyperlinkType,%HTMLHyperlinkTypeOtherDynamic)
      #ENDIF
    #ENDIF
    #IF(%HTMLTargetOrigin='Dynamic')
%AttributeObjectName.Set(%ReportControl,RepGen:HTML,TargetAttr:Hyperlink,%HtmlTargetDynamic)
    #ENDIF
  #ENDIF
#ENDFOR
#!
#!
#INCLUDE('rtar_srv.TPW')
#!
