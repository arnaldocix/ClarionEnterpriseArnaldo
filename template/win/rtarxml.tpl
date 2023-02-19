#TEMPLATE(SVReportToXMLSupport, 'Report To XML Support'),FAMILY('ABC','CW20')
#HELP('ClarionHelp.chm')
#!
#EXTENSION(SVReportToXMLGlobal, 'Report to XML - Global'),APPLICATION(SVReportToXML(SVReportToXMLSupport)),HLP('~TPLExtensionGlobalReportToXML.htm')
#DISPLAY
#DISPLAY(' Report to XML'),AT(3,,187), PROP(PROP:FontColor, 0FFFFFFH), PROP(PROP:Color,0808080H)
#DISPLAY
#DISPLAY('This template has added an extension to each report.')
#DISPLAY('Settings are modified at the procedure level.')
#PREPARE
    #CALL(%SetTargetGlobalPrompt,'XML','XMLReporter','XMLReportGenerator')
#ENDPREPARE
#INSERT(%TargetGlobalPrompts)
#!
#EXTENSION(SVReportToXML,'Report to XML'),PROCEDURE,REQ(SVReportToXMLGlobal),HLP('~TPLExtensionLocalReportToXML.htm')
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
  #DISPLAY(' Report to XML'),AT(3,,190), PROP(PROP:FontColor, 0FFFFFFH), PROP(PROP:Color,0808080H)
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
           #TAB('General'),HLP('~TPLExtensionLocalReportToXMLGeneral.htm')
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
             #PROMPT('Save output as separate files?',CHECK),%PagesAsDifferentFile,DEFAULT(%False),AT(10)
             #DISPLAY('Each file will be named as selected with the')
             #DISPLAY('page number added; e.g.')
             #DISPLAY('"SAVEnnn.XML", where "nnn" is the page number.')
             #BOXED('XML Options')
               #PROMPT('Encoding:',DROP('None|windows-1252|ISO-8859-1|UTF-8|UTF-16')),%LocalEncoding,DEFAULT('None')
               #PROMPT('Root Tag Name:',EXPR),%RootTagName,DEFAULT('Clarion_60_XML_Document'),REQ
               #PROMPT('Use Pages as Parent Tag?',CHECK),%PagesAsParentTag,AT(10)
               #PROMPT('Support Name Spaces?',CHECK),%SupportNameSpaces,AT(10)
               #PROMPT('Add CRLF at Tag end?',CHECK),%UseCRLF,AT(10),DEFAULT(%True)
               #PROMPT('Specify XSL Style Sheet?',CHECK),%SupportStyleSheet,AT(10)
               #BOXED,WHERE(%SupportStyleSheet)
                 #PROMPT('XSL File Name:',EXPR),%XSLFileName
                 #DISPLAY('If left blank XMLFileName.XSL will be used.')
               #ENDBOXED
               #PROMPT('Omit XML Header?',CHECK),%OmittXMLHeader,AT(10),DEFAULT(%False)
               #PROMPT('Which XML Format?',DROP('Tags[Tags]|Attributes[Attributes]|Metadata Tags[Metadata Tags]|Metadata Attributes[Metadata Attributes]')),%GeneratedXMLType,DEFAULT('Tags'),WHENACCEPTED(%TagSetDefaultValuesFromType())
               #BUTTON('Refresh values by type'),WHENACCEPTED(%TagSetDefaultValuesFromType()),AT(10,,175)
               #ENDBUTTON
             #ENDBOXED
             #BOXED,HIDE    
               #BUTTON('Possible Parent Tags'),MULTI(%XMLPossibleParentTags,%XMLPossibleParentTag),INLINE
                 #PROMPT('Parent Tag:',@S100),%XMLPossibleParentTag
                 #PROMPT('Parent Tag Instance:',@n5),%XMLPossibleParentTagInstance
               #ENDBUTTON
             #ENDBOXED
           #ENDTAB
           #TAB('Controls'),HLP('~TPLExtensionLocalReportToXMLControls.htm')
             #PREPARE
               #CALL(%LoadPossibleParentTags)
             #ENDPREPARE
             #BUTTON('Control''s XML Properties'),FROM(%ReportControl,%GetTagDisplay()),INLINE,WHERE(%XMLIsValidControl()),HLP('~TPLExtensionLocalReportToXMLControlsProperties.htm')
               #DISPLAY(' Control: '&%ReportControl),AT(3,,190), PROP(PROP:FontColor, 0FFFFFFH), PROP(PROP:Color,0808080H)
               #BOXED('')
                 #BOXED,SECTION
                   #BOXED,WHERE(%True),AT(0,0)
                     #PROMPT('HIDE (Omit printing)',CHECK),%TagHideControl,DEFAULT(%False),AT(10)
                     #PROMPT('SKIP (Omit processing)',CHECK),%TagSkipControl,DEFAULT(%False),AT(10)
                   #ENDBOXED
                   #BOXED,WHERE(%False),AT(0,0,0,0)
                     #PROMPT('Tag Instance:',@n5),%TagInstance
                   #ENDBOXED
                   #BOXED,WHERE(%False),AT(0,0,0,0)
                     #PROMPT('Initialized:',@n5),%TagInitialized,DEFAULT(0)
                     #PROMPT('Tag Name Type:',@s100),%TagPossibleTagNameType,DEFAULT('Control Text[Text]|Control Name[ControlName]|HardCoded[HardCoded]|Dynamic[Dynamic]')
                     #PROMPT('Tag Value Type:',@s100),%TagPossibleTagValueType,DEFAULT('Control Text[Text]|Control Name[ControlName]|HardCoded[HardCoded]|Dynamic[Dynamic]')
                   #ENDBOXED
                 #ENDBOXED
                 #ENABLE(NOT %TagSkipControl)
                   #PREPARE
                     #SET(%TagInstance,INSTANCE(%ReportControl))
                     #SET(%TagParent,%GetTagDefaultParentTag(%TagInstance,%ReportControlParent,%ReportControlParentType,%TagParent))
                     #IF(NOT %TagInitialized)
                       #CALL(%TagSetDefaultValues)
                     #ENDIF
                     #IF(CLIP(%ReportControlUse)='')
                       #SET(%TagPossibleTagNameType,'Control Text[Text]|Control Name[ControlName]|HardCoded[HardCoded]|Dynamic[Dynamic]')
                     #ELSE
                       #IF(%ReportControlType='TEXT' OR (LEFT(%ReportControlStatement,6)='GROUP,' OR LEFT(%ReportControlStatement,7)='OPTION,'))
                         #SET(%TagPossibleTagNameType,'Control Name[ControlName]|HardCoded[HardCoded]|Dynamic[Dynamic]|Control Variable[ControlUse]')
                         #IF(%TagNameType='Text')
                           #SET(%TagNameType,'ControlName')
                         #ENDIF
                       #ELSE
                         #SET(%TagPossibleTagNameType,'Control Text[Text]|Control Name[ControlName]|HardCoded[HardCoded]|Dynamic[Dynamic]|Control Variable[ControlUse]')
                       #ENDIF
                     #ENDIF
                   #ENDPREPARE
                   #PROMPT('Tag Type:',DROP('Tag[Tag]|Root Tag Child[RootTagChild]|Attribute[Attribute]|Value[Value]|Html Comment[HtmlComment]|Header Attribute[HeaderAttribute]')),%TagType,DEFAULT('RootTagChild'),PROP(PROP:Drop,15)
                   #ENABLE(%TagType='Tag' OR %TagType='Attribute' OR %TagType='Value')
                     #PROMPT('Parent Tag:',FROM(%XMLPossibleParentTags,(%TagInstance>%XMLPossibleParentTagInstance),%XMLPossibleParentTag)),%TagParent,REQ
                   #ENDENABLE
                   #ENABLE(NOT(%TagType='Value' OR %TagType='HtmlComment'))
                     #PROMPT('Name Type:',DROP(%TagPossibleTagNameType)),%TagNameType,DEFAULT('Text')
                     #BOXED,SECTION
                       #BOXED,WHERE(%TagNameType='HardCoded'),AT(0,0)
                         #PROMPT('Name:',EXPR),%TagNameHardCoded
                       #ENDBOXED
                       #BOXED,WHERE(%TagNameType='Dynamic'),AT(0,0)
                         #PROMPT('Name:',EXPR),%TagNameDynamic
                       #ENDBOXED
                       #BOXED,WHERE(%TagNameType='Text'),AT(0,0)
                         #DISPLAY('The Name will be the text displayed on the report')
                       #ENDBOXED
                       #BOXED,WHERE(%TagNameType='ControlName'),AT(0,0)
                         #DISPLAY('The Name will be the Control Name with out the "?"')
                       #ENDBOXED
                       #BOXED,WHERE(%TagNameType='ControlUse'),AT(0,0)
                         #DISPLAY('The Name will be the Variable Name')
                       #ENDBOXED
                     #ENDBOXED
                   #ENDENABLE
                   #PREPARE
                     #IF(CLIP(%ReportControlUse)='')
                       #IF(LEFT(%ReportControlStatement,6)='GROUP,' OR LEFT(%ReportControlStatement,7)='OPTION,')
                         #SET(%TagPossibleTagValueType,'Control Name[ControlName]|HardCoded[HardCoded]|Dynamic[Dynamic]')
                         #IF(%TagValueType='ControlUse' OR %TagValueType='Text')
                           #SET(%TagValueType,'ControlName')
                         #ENDIF
                       #ELSE
                         #SET(%TagPossibleTagValueType,'Control Text[Text]|Control Name[ControlName]|HardCoded[HardCoded]|Dynamic[Dynamic]')
                         #IF(%TagValueType='ControlUse')
                           #SET(%TagValueType,'Text')
                         #ENDIF
                       #ENDIF
                     #ELSE
                       #IF(LEFT(%ReportControlStatement,6)='GROUP,' OR LEFT(%ReportControlStatement,7)='OPTION,')
                         #SET(%TagPossibleTagValueType,'Control Name[ControlName]|HardCoded[HardCoded]|Dynamic[Dynamic]')
                         #IF(%TagValueType='ControlUse' OR %TagValueType='Text')
                           #SET(%TagValueType,'ControlName')
                         #ENDIF
                       #ELSE
                         #SET(%TagPossibleTagValueType,'Control Text[Text]|Control Name[ControlName]|HardCoded[HardCoded]|Dynamic[Dynamic]|Control Variable[ControlUse]')
                       #ENDIF
                     #ENDIF
                     #IF(%TagType='Tag' OR %TagType='RootTagChild')
                       #SET(%TagPossibleTagValueType,CLIP(%TagPossibleTagValueType)&'|No Value[None]')
                     #ELSE
                       #IF(%TagValueType='None')
                         #SET(%TagValueType,'Text')
                       #ENDIF
                     #ENDIF
                   #ENDPREPARE
                   #BOXED,SECTION
                     #BOXED,WHERE(%ReportControlType='TEXT'),AT(0,0)
                       #DISPLAY('Value Type: Control Text')
                       #DISPLAY('The value will be the text displayed on the report')
                     #ENDBOXED
                     #BOXED,WHERE(%ReportControlType<>'TEXT'),AT(0,0)
                       #PROMPT('Value Type:',DROP(%TagPossibleTagValueType)),%TagValueType,DEFAULT('Text')
                       #VALIDATE(NOT(%TagValueType=%TagNameType AND (%TagType='Tag' OR %TagType='RootTagChild')),'The value type cannot the same as the name type')
                       #BOXED,SECTION
                         #BOXED,WHERE(%TagValueType='HardCoded'),AT(0,0)
                           #PROMPT('Tag Name:',EXPR),%TagValueHardCoded
                         #ENDBOXED
                         #BOXED,WHERE(%TagValueType='Dynamic'),AT(0,0)
                           #PROMPT('Tag Name:',EXPR),%TagValueDynamic
                         #ENDBOXED
                         #BOXED,WHERE(%TagValueType='Text'),AT(0,0)
                           #DISPLAY('The value will be the text displayed on the report')
                         #ENDBOXED
                         #BOXED,WHERE(%TagValueType='ControlName'),AT(0,0)
                           #DISPLAY('The value will be the Control Name with out the "?"')
                         #ENDBOXED
                         #BOXED,WHERE(%TagValueType='ControlUse'),AT(0,0)
                           #DISPLAY('The value will be the Variable Name')
                         #ENDBOXED
                         #BOXED,WHERE(%TagValueType='None'),AT(0,0)
                           #DISPLAY('The tag will not have a value')
                         #ENDBOXED
                       #ENDBOXED
                     #ENDBOXED
                   #ENDBOXED
                   #ENABLE(%TagType='Value' OR ((%TagType='Tag' OR %TagType='RootTagChild') AND %TagValueType<>'None')),CLEAR
                     #PROMPT('The Value is CDATA?',CHECK),%TagValueIsCDATA,DEFAULT(%False),AT(10)
                   #ENDENABLE
                   #ENABLE(%TagType='RootTagChild' OR %TagType='Tag')
                     #PROMPT('Process only once',CHECK),%TagProcessControlOnce,DEFAULT(%False),AT(10)
                   #ENDENABLE
                 #ENDENABLE
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
    #FOR(%ReportControl),WHERE(%XMLIsValidControl() AND %TagHideControl)
        #CALL(%AddHidenControlToReport,%lTargetName,%ReportControl)
    #ENDFOR
    #CALL(%SetClassDefaults,%lTargetObjectName,%lTargetObjectName,%lTargetObjectClass)
    #IF(%Populate='Conditional')
       #CALL(%AddTargetToReport,%lTargetName,%lTargetObjectName,%lTargetObjectClass,%PopulateCondition)
    #ELSE
       #CALL(%AddTargetToReport,%lTargetName,%lTargetObjectName,%lTargetObjectClass,'')
    #ENDIF
 #ENDIF
 #DECLARE(%AttributeObjectName)
 #DECLARE(%ReportObjectName)
 #IF(%AppTemplateFamily='CLARION')
    #SET(%AttributeObjectName,'LocalAttribute')
    #SET(%ReportObjectName,%Report)
 #ELSE
    #SET(%AttributeObjectName,'SELF.Attribute')
    #SET(%ReportObjectName,'SELF.Report')
 #ENDIF
#ENDAT
#AT(%GatherObjects)
#IF(NOT %GlobalDisable AND %Populate<>'Never' AND %EnablePrintPreview AND %GlobalUseABCClasess)
    #CALL(%AddObjectList, %lTargetObjectName)
#ENDIF
#ENDAT
#AT(%LocalProcedures)
   #IF(NOT %GlobalDisable AND %Populate<>'Never' AND %EnablePrintPreview AND %GlobalUseABCClasess)
      #CALL(%GenerateVirtuals, %lTargetObjectName, 'Local Objects|Abc Objects|Report Target|'&%lTargetName, '%ReportTargetVirtuals(SVReportToXMLSupport)')
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
#AT(%ReportTargetMethodCodeSection,%lTargetName,'Setup','()'),PRIORITY(5500),WHERE(NOT %GlobalDisable AND %Populate<>'Never' AND %EnablePrintPreview AND %GlobalUseABCClasess)
#CASE(%OutputNameType)
#OF('Fixed')
SELF.SetFileName(%(%SVStripPling(%OutputNameFixed)))
#OF('Variable')
SELF.SetFileName(%OutputNameVariable)
#OF('Runtime')
SELF.SetFileName('')
#ENDCASE
SELF.SetRootTag(%(%SVStripPling(%RootTagName)))
SELF.SetForceXMLHeader(%(CHOOSE(%OmittXMLHeader,'False','True')))
SELF.SetSupportNameSpaces(%(CHOOSE(%SupportNameSpaces,'True','False')))
#IF(%UseCRLF)
SELF.SetUseCRLF(True)
#ELSE
SELF.SetUseCRLF(False)
#ENDIF
SELF.SetPagesAsDifferentFile(%(CHOOSE(%PagesAsDifferentFile,'True','False')))
SELF.SetPagesAsParentTag(%(CHOOSE(%PagesAsParentTag,'True','False')))
#IF(%LocalEncoding<>'None')
SELF.SetEncoding('%'LocalEncoding')
#ENDIF
#IF(%SupportStyleSheet)
   #IF(%XSLFileName)
SELF.SetXSL(%(%SVStripPling(%XSLFileName)))
   #ELSE
SELF.SetUseXMLasXSLName(True)
   #ENDIF
#ENDIF
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
#GROUP(%GetTagParentTagName,%pEmb,%pTagName,%pTagParent),PRESERVE
#FIX(%ReportControl,%pTagParent)
#IF(%ReportControl=%pTagParent)
         #CASE(%TagNameType)
         #OF('Dynamic')
            #IF(UPPER(%pEmb)='D')
%AttributeObjectName.Set(%pTagName,RepGen:XML,TargetAttr:TagParentName,%(%GetTagTagName()))
            #ENDIF
         #OF('Text')
            #IF(UPPER(%pEmb)='D')
               #IF(%ReportControlUse)
%AttributeObjectName.Set(%pTagName,RepGen:XML,TargetAttr:TagParentName,%ReportControlUse)
               #ELSE
%AttributeObjectName.Set(%pTagName,RepGen:XML,TargetAttr:TagParentName,%ReportObjectName $ %ReportControl{PROP:TEXT})
               #ENDIF
            #ENDIF
         #ELSE
            #IF(UPPER(%pEmb)='S')
%AttributeObjectName.Set(%pTagName,RepGen:XML,TargetAttr:TagParentName,%(%GetTagTagName()))
            #ENDIF
         #ENDCASE
#ENDIF
#!
#!
#!
#!
#GROUP(%GetTagDisplay),AUTO
#DECLARE(%lReturnValue)
#IF(%TagSkipControl)
  #SET(%lReturnValue,%ReportControl&' - SKIP')
#ELSE
  #SET(%lReturnValue,%ReportControl&' - '&%TagType)
  #CASE(%TagType)
  #OF('Tag')
    #SET(%lReturnValue,CLIP(%lReturnValue)&' - PARENT:'&%TagParent)
  #OF('Value')
    #SET(%lReturnValue,CLIP(%lReturnValue)&' - PARENT:'&%TagParent)
  #OF('Attribute')
    #SET(%lReturnValue,CLIP(%lReturnValue)&' - PARENT:'&%TagParent)
  #!OF('HtmlComment')
  #!OF('XMLComment')
  #ENDCASE
#ENDIF
#RETURN(%lReturnValue)
#!
#!
#!
#GROUP(%GetTagTagValue)
#CASE(%TagValueType)
#OF('ControlName')
  #RETURN(''''&RIGHT(%ReportControl,LEN(%ReportControl)-1)&'''')
#OF('HardCoded')
  #RETURN(''''&%TagNameHardCoded&'''')
#OF('Dynamic')
  #RETURN(%TagValueDynamic)
#OF('ControlUse')
  #RETURN(''''&%ReportControlUse&'''')
#ENDCASE
#!
#!
#!
#GROUP(%GetTagTagName)
#CASE(%TagNameType)
#OF('ControlName')
  #RETURN(''''&RIGHT(%ReportControl,LEN(%ReportControl)-1)&'''')
#OF('HardCoded')
  #RETURN(''''&%TagNameHardCoded&'''')
#OF('Dynamic')
  #RETURN(%TagNameDynamic)
#OF('ControlUse')
  #RETURN(''''&%ReportControlUse&'''')
#OF('None')
#ENDCASE
#!
#!
#!
#GROUP(%LoadPossibleParentTags),AUTO
#DECLARE(%lIndex)
#SET(%lIndex,0)
#FREE(%XMLPossibleParentTags)
#FOR(%ReportControl),WHERE((%TagType='Tag' OR %TagType='RootTagChild') AND %XMLIsValidControl())
  #SET(%lIndex,%lIndex+1)
  #ADD(%XMLPossibleParentTags,%lIndex)
  #SET(%XMLPossibleParentTag,%ReportControl)
  #SET(%XMLPossibleParentTagInstance,INSTANCE(%ReportControl))
#ENDFOR
#!
#!
#!
#GROUP(%GetTagDefaultParentTag,%pTagInstance,%pReportControlParent,%pReportControlParentType,%pTagParent),AUTO
#DECLARE(%lLastControl)
#DECLARE(%lParentControl)
#DECLARE(%lFound)
#SET(%lFound,%False)
#SET(%lLastControl,'')
#SET(%lParentControl,'')
#FOR(%XMLPossibleParentTags),WHERE(%pTagInstance>%XMLPossibleParentTagInstance)
  #SET(%lLastControl,%XMLPossibleParentTag)
  #IF(%pReportControlParent AND %pReportControlParentType='GROUP')
    #IF(%pReportControlParent=%XMLPossibleParentTag)
      #SET(%lParentControl,%XMLPossibleParentTag)
    #ENDIF
  #ENDIF
  #IF(%pTagParent=%XMLPossibleParentTag)
    #SET(%lFound,%True)
  #ENDIF
#ENDFOR
#IF(NOT %pTagParent OR NOT %lFound)
   #IF(%lParentControl)
      #RETURN(%lParentControl)
   #ELSE
      #RETURN(%lLastControl)
   #ENDIF
#ELSE
   #RETURN(%pTagParent)
#ENDIF
#!
#!
#!
#!
#GROUP(%TagSetDefaultValuesFromType)
#FOR(%ReportControl),WHERE(%XMLIsValidControl())
   #SET(%TagInstance,INSTANCE(%ReportControl))
   #CALL(%TagSetDefaultValues)
#ENDFOR
#GROUP(%TagSetDefaultValues)
#CASE(%GeneratedXMLType)
#OF('Tags')
    #CASE(%ReportControlType)
    #OF('GROUP')
       #IF(%ReportControlParent AND %ReportControlParentType='GROUP')
          #SET(%TagType,'Tag')
          #SET(%TagParent,%GetTagDefaultParentTag(%TagInstance,%ReportControlParent,%ReportControlParentType,%TagParent))
          #IF(%TagParent<>%ReportControlParent)
             #SET(%TagType,'RootTagChild')
          #ENDIF
       #ELSE
          #SET(%TagType,'RootTagChild')
       #ENDIF
       #IF(LEFT(%ReportControlStatement,6)='GROUP,' OR LEFT(%ReportControlStatement,7)='OPTION,')
          #SET(%TagNameType,'ControlName')
       #ELSE
          #SET(%TagNameType,'Text')
       #ENDIF
       #SET(%TagValueType,'None')
    #OF('STRING')
       #IF(%ReportControlParent AND %ReportControlParentType='GROUP')
          #SET(%TagType,'Tag')
          #SET(%TagParent,%GetTagDefaultParentTag(%TagInstance,%ReportControlParent,%ReportControlParentType,%TagParent))
          #IF(%TagParent<>%ReportControlParent)
             #SET(%TagType,'RootTagChild')
          #ENDIF
       #ELSE
          #SET(%TagType,'RootTagChild')
       #ENDIF
       #SET(%TagNameType,'ControlName')
       #SET(%TagValueType,'Text')
    #OF('CHECK')
       #IF(%ReportControlParent AND %ReportControlParentType='GROUP')
          #SET(%TagType,'Tag')
          #SET(%TagParent,%GetTagDefaultParentTag(%TagInstance,%ReportControlParent,%ReportControlParentType,%TagParent))
          #IF(%TagParent<>%ReportControlParent)
             #SET(%TagType,'RootTagChild')
          #ENDIF
       #ELSE
          #SET(%TagType,'RootTagChild')
       #ENDIF
       #SET(%TagNameType,'Text')
       #SET(%TagValueType,'Dynamic')
       #SET(%TagValueDynamic,%ReportControlUse)
    #OF('OPTION')
       #IF(%ReportControlParent AND %ReportControlParentType='GROUP')
          #SET(%TagType,'Tag')
          #SET(%TagParent,%GetTagDefaultParentTag(%TagInstance,%ReportControlParent,%ReportControlParentType,%TagParent))
          #IF(%TagParent<>%ReportControlParent)
             #SET(%TagType,'RootTagChild')
          #ENDIF
       #ELSE
          #SET(%TagType,'RootTagChild')
       #ENDIF
       #IF(LEFT(%ReportControlStatement,6)='GROUP,' OR LEFT(%ReportControlStatement,7)='OPTION,')
          #SET(%TagNameType,'ControlName')
       #ELSE
          #SET(%TagNameType,'Text')
       #ENDIF
       #SET(%TagValueType,'Dynamic')
       #SET(%TagValueDynamic,%ReportControlUse)
    #OF('TEXT')
       #IF(%ReportControlParent AND %ReportControlParentType='GROUP')
          #SET(%TagType,'Tag')
          #SET(%TagParent,%GetTagDefaultParentTag(%TagInstance,%ReportControlParent,%ReportControlParentType,%TagParent))
          #IF(%TagParent<>%ReportControlParent)
             #SET(%TagType,'RootTagChild')
          #ENDIF
       #ELSE
          #SET(%TagType,'RootTagChild')
       #ENDIF
       #SET(%TagNameType,'ControlName')
       #SET(%TagValueType,'Text')
    #ENDCASE
    #SET(%TagProcessControlOnce,%False)
    #SET(%TagSkipControl,%False)
#OF('Attributes')
    #CASE(%ReportControlType)
    #OF('GROUP')
       #IF(%ReportControlParent AND %ReportControlParentType='GROUP')
          #SET(%TagType,'Tag')
          #SET(%TagParent,%GetTagDefaultParentTag(%TagInstance,%ReportControlParent,%ReportControlParentType,%TagParent))
          #IF(%TagParent<>%ReportControlParent)
             #SET(%TagType,'RootTagChild')
          #ENDIF
       #ELSE
          #SET(%TagType,'RootTagChild')
       #ENDIF
       #IF(LEFT(%ReportControlStatement,6)='GROUP,' OR LEFT(%ReportControlStatement,7)='OPTION,')
          #SET(%TagNameType,'ControlName')
       #ELSE
          #SET(%TagNameType,'Text')
       #ENDIF
       #SET(%TagValueType,'None')
    #OF('STRING')
       #IF(%ReportControlParent AND %ReportControlParentType='GROUP')
          #SET(%TagType,'Attribute')
          #SET(%TagParent,%GetTagDefaultParentTag(%TagInstance,%ReportControlParent,%ReportControlParentType,%TagParent))
          #IF(%TagParent<>%ReportControlParent)
             #SET(%TagType,'RootTagChild')
          #ENDIF
       #ELSE
          #SET(%TagType,'RootTagChild')
       #ENDIF
       #SET(%TagNameType,'ControlName')
       #SET(%TagValueType,'Text')
    #OF('CHECK')
       #IF(%ReportControlParent AND %ReportControlParentType='GROUP')
          #SET(%TagType,'Attribute')
          #SET(%TagParent,%GetTagDefaultParentTag(%TagInstance,%ReportControlParent,%ReportControlParentType,%TagParent))
          #IF(%TagParent<>%ReportControlParent)
             #SET(%TagType,'RootTagChild')
          #ENDIF
       #ELSE
          #SET(%TagType,'RootTagChild')
       #ENDIF
       #SET(%TagNameType,'Text')
       #SET(%TagValueType,'Dynamic')
       #SET(%TagValueDynamic,%ReportControlUse)
    #OF('OPTION')
       #IF(%ReportControlParent AND %ReportControlParentType='GROUP')
          #SET(%TagType,'Attribute')
          #SET(%TagParent,%GetTagDefaultParentTag(%TagInstance,%ReportControlParent,%ReportControlParentType,%TagParent))
          #IF(%TagParent<>%ReportControlParent)
             #SET(%TagType,'RootTagChild')
          #ENDIF
       #ELSE
          #SET(%TagType,'RootTagChild')
       #ENDIF
       #IF(LEFT(%ReportControlStatement,6)='GROUP,' OR LEFT(%ReportControlStatement,7)='OPTION,')
          #SET(%TagNameType,'ControlName')
       #ELSE
          #SET(%TagNameType,'Text')
       #ENDIF
       #SET(%TagValueType,'Dynamic')
       #SET(%TagValueDynamic,%ReportControlUse)
    #OF('TEXT')
       #IF(%ReportControlParent AND %ReportControlParentType='GROUP')
          #SET(%TagType,'Atribute')
          #SET(%TagParent,%GetTagDefaultParentTag(%TagInstance,%ReportControlParent,%ReportControlParentType,%TagParent))
          #IF(%TagParent<>%ReportControlParent)
             #SET(%TagType,'RootTagChild')
          #ENDIF
       #ELSE
          #SET(%TagType,'RootTagChild')
       #ENDIF
       #SET(%TagNameType,'ControlName')
       #SET(%TagValueType,'Text')
    #ENDCASE
    #SET(%TagProcessControlOnce,%False)
    #SET(%TagSkipControl,%False)
#OF('Metadata Tags')
    #CASE(%ReportControlType)
    #OF('GROUP')
       #IF(%ReportControlParent AND %ReportControlParentType='GROUP')
          #SET(%TagType,'Tag')
          #SET(%TagParent,%GetTagDefaultParentTag(%TagInstance,%ReportControlParent,%ReportControlParentType,%TagParent))
          #IF(%TagParent<>%ReportControlParent)
             #SET(%TagType,'RootTagChild')
          #ENDIF
       #ELSE
          #SET(%TagType,'RootTagChild')
       #ENDIF
       #IF(LEFT(%ReportControlStatement,6)='GROUP,' OR LEFT(%ReportControlStatement,7)='OPTION,')
          #SET(%TagNameType,'ControlName')
       #ELSE
          #SET(%TagNameType,'Text')
       #ENDIF
       #SET(%TagValueType,'None')
    #OF('STRING')
    #OROF('CHECK')
    #OROF('TEXT')
    #OROF('OPTION')
       #IF(%ReportControlUse)
         #SET(%TagNameType,'ControlUse')
         #SET(%TagValueType,'Text')
         #IF(%ReportControlParent AND %ReportControlParentType='GROUP')
            #SET(%TagType,'Tag')
            #SET(%TagParent,%GetTagDefaultParentTag(%TagInstance,%ReportControlParent,%ReportControlParentType,%TagParent))
            #IF(%TagParent<>%ReportControlParent)
               #SET(%TagType,'RootTagChild')
            #ENDIF
         #ELSE
            #SET(%TagType,'RootTagChild')
         #ENDIF
       #ELSE
         #SET(%TagSkipControl,%True)
       #ENDIF
    #ENDCASE
    #SET(%TagProcessControlOnce,%False)
#OF('Metadata Attributes')
    #CASE(%ReportControlType)
    #OF('GROUP')
       #IF(%ReportControlParent AND %ReportControlParentType='GROUP')
          #SET(%TagType,'Tag')
          #SET(%TagParent,%GetTagDefaultParentTag(%TagInstance,%ReportControlParent,%ReportControlParentType,%TagParent))
          #IF(%TagParent<>%ReportControlParent)
             #SET(%TagType,'RootTagChild')
          #ENDIF
       #ELSE
          #SET(%TagType,'RootTagChild')
       #ENDIF
       #IF(LEFT(%ReportControlStatement,6)='GROUP,' OR LEFT(%ReportControlStatement,7)='OPTION,')
          #SET(%TagNameType,'ControlName')
       #ELSE
          #SET(%TagNameType,'Text')
       #ENDIF
       #SET(%TagValueType,'None')
    #OF('STRING')
    #OROF('CHECK')
    #OROF('TEXT')
    #OROF('OPTION')
       #IF(%ReportControlUse)
         #SET(%TagNameType,'ControlUse')
         #SET(%TagValueType,'Text')
         #IF(%ReportControlParent AND %ReportControlParentType='GROUP')
            #SET(%TagType,'Attribute')
            #SET(%TagParent,%GetTagDefaultParentTag(%TagInstance,%ReportControlParent,%ReportControlParentType,%TagParent))
            #IF(%TagParent<>%ReportControlParent)
               #SET(%TagType,'RootTagChild')
            #ENDIF
         #ELSE
            #SET(%TagType,'RootTagChild')
         #ENDIF
       #ELSE
         #SET(%TagSkipControl,%True)
       #ENDIF
    #ENDCASE
    #SET(%TagProcessControlOnce,%False)
#ENDCASE
#SET(%TagInitialized,1)
#!
#!
#GROUP(%XMLIsValidControl),AUTO
#DECLARE(%lReturnValue)
#SET(%lReturnValue,%False)
#IF(NOT %ReportControl)
   #RETURN(%False)
#ENDIF
#IF(INSTRING(',HIDE',%ReportControlStatement,1,1)>0)
   #RETURN(%False)
#ENDIF
#!
#CASE(%ReportControlType)
#OF('LINE')
#OROF('BOX')
#OROF('ELLIPSE')
#OROF('RADIO')
#OROF('IMAGE')
#OROF('LIST')
#OROF('HEADER')
#OROF('DETAIL')
#OROF('FORM')
#OROF('FOOTER')
#OROF('BREAK')
   #SET(%lReturnValue,%False)
#OF('GROUP')
#OROF('OPTION')
   #IF(INSTRING(',BOXED',%ReportControlStatement,1,1)>0)
       #SET(%lReturnValue,%True)
   #ELSE
       #SET(%lReturnValue,%False)
   #ENDIF
#OF('STRING')
#OROF('TEXT')
#OROF('CHECK')
   #SET(%lReturnValue,%True)
#ELSE
   #ERROR('ReportToXML: %%XMLIsValidControl (Control not supported)')
#ENDCASE
#RETURN(%lReturnValue)
#!
#!
#!
#GROUP(%CodeSetStaticControlsAttributes)
!XML STATIC
#FOR(%ReportControl),WHERE(%XMLIsValidControl())
  #IF(%TagSkipControl)
%AttributeObjectName.Set(%ReportControl,RepGen:XML,TargetAttr:Skip,%ReportControl)
  #ELSE
     #IF(%TagProcessControlOnce OR %TagType='HeaderAttribute')
%AttributeObjectName.Set(%ReportControl,RepGen:XML,TargetAttr:TagProcessOnce,%ReportControl)
     #ENDIF
     #CASE(%TagType)
     #OF('Tag')
     #OROF('RootTagChild')
%AttributeObjectName.Set(%ReportControl,RepGen:XML,TargetAttr:TagType,TagType:Tag)
     #OF('Attribute')
%AttributeObjectName.Set(%ReportControl,RepGen:XML,TargetAttr:TagType,TagType:TagAttribute)
     #OF('HeaderAttribute')
%AttributeObjectName.Set(%ReportControl,RepGen:XML,TargetAttr:TagType,TagType:HeaderAttribute)
     #OF('HtmlComment')
%AttributeObjectName.Set(%ReportControl,RepGen:XML,TargetAttr:TagType,TagType:Comment)
     #OF('Value')
%AttributeObjectName.Set(%ReportControl,RepGen:XML,TargetAttr:TagType,TagType:TagValue)
     #ELSE
%AttributeObjectName.Set(%ReportControl,RepGen:XML,TargetAttr:TagType,TagType:%TagType)
     #ENDCASE
     #IF(%TagType='Tag' OR %TagType='Attribute' OR %TagType='Value')
#INSERT(%GetTagParentTagName,'S',%ReportControl,%TagParent)
     #ENDIF
     #CASE(%TagType)
     #OF('Tag')
     #OROF('RootTagChild')
        #IF(%TagValueType<>'None' AND %TagValueIsCDATA)
%AttributeObjectName.Set(%ReportControl,RepGen:XML,TargetAttr:TagIsCDATA,True)
        #ENDIF
     #OF('Value')
        #IF(%TagValueIsCDATA)
%AttributeObjectName.Set(%ReportControl,RepGen:XML,TargetAttr:TagIsCDATA,True)
        #ENDIF
     #ENDCASE
     #CASE(%TagType)
     #OF('Tag')
     #OROF('RootTagChild')
        #IF(%TagNameType<>'Dynamic')
         #IF(%TagNameType<>'Text')
%AttributeObjectName.Set(%ReportControl,RepGen:XML,TargetAttr:TagName,%(%GetTagTagName()))
         #ELSE
%AttributeObjectName.Set(%ReportControl,RepGen:XML,TargetAttr:TagNameFromText,True)
         #ENDIF
        #ENDIF
         #IF(NOT (%TagValueType='None' OR %TagValueType='Dynamic'))
            #IF(%TagValueType<>'Text')
%AttributeObjectName.Set(%ReportControl,RepGen:XML,TargetAttr:TagValue,%(%GetTagTagValue()))
            #ELSE
%AttributeObjectName.Set(%ReportControl,RepGen:XML,TargetAttr:TagValueFromText,True)
            #END
         #ENDIF
     #OF('Attribute')
     #OROF('HeaderAttribute')
        #IF(%TagNameType<>'Dynamic')
         #IF(%TagNameType<>'Text')
%AttributeObjectName.Set(%ReportControl,RepGen:XML,TargetAttr:TagName,%(%GetTagTagName()))
         #ELSE
%AttributeObjectName.Set(%ReportControl,RepGen:XML,TargetAttr:TagNameFromText,True)
         #ENDIF
        #ENDIF
        #IF(%TagValueType<>'Dynamic')
         #IF(%TagValueType<>'Text')
%AttributeObjectName.Set(%ReportControl,RepGen:XML,TargetAttr:TagValue,%(%GetTagTagValue()))
         #ELSE
%AttributeObjectName.Set(%ReportControl,RepGen:XML,TargetAttr:TagValueFromText,True)
         #ENDIF
        #ENDIF
     #OF('Value')
     #OROF('HtmlComment')
        #IF(%TagValueType<>'Dynamic')
         #IF(%TagValueType<>'Text')
%AttributeObjectName.Set(%ReportControl,RepGen:XML,TargetAttr:TagValue,%(%GetTagTagValue()))
         #ELSE
%AttributeObjectName.Set(%ReportControl,RepGen:XML,TargetAttr:TagValueFromText,True)
         #ENDIF
        #ENDIF
     #ENDCASE
  #ENDIF
#ENDFOR
#!
#!
#GROUP(%CodeSetDynamicControlsAttributes)
#FOR(%ReportControl),WHERE(%XMLIsValidControl())
  #IF(NOT %TagSkipControl)
     #IF(%TagType='Tag' OR %TagType='Attribute' OR %TagType='Value')
#INSERT(%GetTagParentTagName,'D',%ReportControl,%TagParent)
     #ENDIF
     #IF(NOT (%TagType='Value' OR %TagType='HtmlComment'))
         #IF(%TagNameType='Dynamic')
%AttributeObjectName.Set(%ReportControl,RepGen:XML,TargetAttr:TagName,%(%GetTagTagName()))
         #ENDIF
     #ENDIF
     #IF(%TagValueType='Dynamic')
%AttributeObjectName.Set(%ReportControl,RepGen:XML,TargetAttr:TagValue,%(%GetTagTagValue()))
     #ENDIF
  #ENDIF
#ENDFOR
#!
#!
#!
#INCLUDE('rtar_srv.TPW')
#!
