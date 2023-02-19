#TEMPLATE (SVBusinessRules, 'SVBusinessRules'),FAMILY('ABC')
#HELP('ClarionHelp.chm')
#!
#EXTENSION(GlobalBusinessRulesManager,'Global Business Rules Manager'),APPLICATION(LocalBusinessRulesManager(SVBusinessRules)),HLP('~TPLExtensionGlobalBusinessRules')
#BOXED,HIDE
  #PROMPT('GlobalRulesImported',CHECK),%GlobalRulesImported,DEFAULT(%False),AT(,0,,0)
#ENDBOXED
#PROMPT('Mode:',OPTION),%GlobalRulesTemplateMode,AT(4,,192)
#PROMPT('Original',RADIO)
#PROMPT('Clone',RADIO)
#PROMPT('Rules File Name:',OPENDIALOG('Choose file','Business Rules|*.brf')),%GlobalRulesFileName,REQ,WHENACCEPTED(CALL(%ResolveBusRulesFileName,%GlobalRulesFileName))
#DISPLAY('Program will add .brf extension to file name if missing.')
#BOXED(''),WHERE(%GlobalRulesTemplateMode = 'Original')
  #INSERT(%GlobalBusinessRulesManagerPrompts)
#ENDBOXED
#!
#DECLARE(%GlobalRulesLoaded)
#ATSTART
#IF(%GlobalRulesTemplateMode = 'Clone')
  #CALL(%GlobalRulesImport,%GlobalRulesFileName&'.brf')
#ENDIF
#ENDAT
#!
#AT(%AfterGlobalIncludes)
   INCLUDE('ABRULE.INC'),ONCE
#ENDAT
#!
#AT(%GlobalData)
  #CASE(%ProgramExtension)
  #OF('DLL')
    #IF(%GlobalExternal)                              #!A DLL with global data external
      #IF(%ExternalSource='Dynamic Link Library (DLL)')
GlobalRule           LONG,EXTERNAL,DLL(dll_mode),THREAD
      #ELSE
GlobalRule           LONG,EXTERNAL,THREAD
      #ENDIF
    #ELSE                                             #!A DLL with export global data
GlobalRule           LONG,THREAD
    #ENDIF
  #OF('LIB')
    #IF(%GlobalExternal)                              #!A LIB with global data external
      #IF(%ExternalSource='Dynamic Link Library (DLL)')
GlobalRule           LONG,EXTERNAL,DLL(dll_mode),THREAD
      #ELSE
GlobalRule           LONG,EXTERNAL,THREAD
      #ENDIF
    #ELSE                                             #!A LIB with global data local
GlobalRule           LONG,THREAD
    #ENDIF
  #OF('EXE')
    #IF(%GlobalExternal)                              #!An EXE with global data external
      #IF(%ExternalSource='Dynamic Link Library (DLL)')
GlobalRule           LONG,EXTERNAL,DLL(dll_mode),THREAD
      #ELSE
GlobalRule           LONG,EXTERNAL,THREAD
      #ENDIF
    #ELSE                                             #!An EXE with global data local
GlobalRule           LONG,THREAD
    #ENDIF
  #ENDCASE
#ENDAT
#AT(%BeginningExports)
  #IF(NOT %GlobalExternal)
$GLOBALRULE                                             @?
  #ENDIF
#ENDAT
#!
#ATEND
  #IF(NOT %EditFilename)
    #IF(%GlobalRulesTemplateMode = 'Original')
      #CALL(%GlobalRulesExport,%GlobalRulesFileName&'.brf')
    #ENDIF
    #SET(%GlobalRulesImported,%False)
  #ENDIF
#ENDAT
#!
#GROUP(%ResolveBusRulesFileName,* %BRfile)
#DECLARE(%BRIndex,LONG)
#SET(%BRIndex,INSTRING('.',%BRFile))
#CASE(%BRIndex)
#OF(0)
#OF(1)
  #SET(%BRfile,'')
#ELSE
  #SET(%BRfile,SLICE(%BRfile,1,%BRIndex-1))
#ENDCASE
#SET(%BRFile,CLIP(%BRFIle))
#!
#!------------------------------------------------------------
#!
#EXTENSION(LocalBusinessRulesManager,'Local Business Rules Manager'),PROCEDURE,REQ(GlobalBusinessRulesManager),HLP('~TPLExtensionLocalBusinessRules')
#COMMENT(100)
#INSERT(%LocalBusinessRulesManagerPrompts)
#ATSTART
#INSERT(%BusinessRulesLocalAtStart)
#ENDAT
#AT(%CustomModuleDeclarations)
#INSERT(%BusinessRulesLocalAtStartOfModule)
#ENDAT
#AT(%CustomGlobalDeclarations)
#INSERT(%BusinessRulesLocalAtCustomDec)
#ENDAT
#AT(%LocalDataClassData)
#INSERT(%BusinessRulesLocalClassData)
#ENDAT
#AT(%LocalProcedures)
#!INSERT(%BusinessRulesLocalClassMethods)
#ENDAT
#AT(%ProcedureRoutines)
#ENDAT
#AT(%WindowManagerMethodCodeSection,'Init','(),BYTE'),PRIORITY(6500)
#INSERT(%BusinessRulesLocalInit)
#ENDAT
#AT(%WindowManagerMethodCodeSection,'Init','(),BYTE'),PRIORITY(9500)
#INSERT(%BusinessRulesLocalCheckAfterOpen)
#ENDAT
#AT(%WindowManagerMethodCodeSection,'TakeNewSelection','(),BYTE'),PRIORITY(6900)
#INSERT(%BusinessRulesLocalAfterTakeNewSelection)
#ENDAT
#AT(%WindowManagerMethodCodeSection,'TakeAccepted','(),BYTE'),PRIORITY(6400)
#INSERT(%BusinessRulesLocalAfterFieldsChange)
#ENDAT
#AT(%WindowManagerMethodCodeSection,'TakeAccepted','(),BYTE'),PRIORITY(2250)
#INSERT(%BusinessRulesLocalTakeAccepted)
#ENDAT
#AT(%WindowManagerMethodCodeSection,'TakeCompleted'),PRIORITY(2800)
  #IF(ITEMS(%AUXLRules)<>0 AND %LocalRuleCheckBeforeUpdate)
IF BusinessRulesManager.CheckAllRules(0)
  RETURN Level:Notify
END
  #ENDIF
#ENDAT
#!
#!------------------------------------------------------------
#CODE(BusinessRulesHandler,'Error handler for Business Rules'),REQ(LocalBusinessRulesManager),HLP('~TPLCodeBusinessRulesHandler')
#PREPARE
  #SET(%RuleBaseNameToCheck,%GetFirstRuleBaseName())
  #CALL(%FillAuxRuleName,%RuleBaseNameToCheck,%RuleNameToCheck)
  #CALL(%FillAuxGloRuleManName)
  #CALL(%FillAuxGloRuleManName)
  #IF(%RulesSelection=1)
    #IF(%ThisFunction=6)
       #SET(%ThisFunction,1)
    #ENDIF
  #ENDIF
  #IF(NOT %LocalRuleBaseNameToCheck OR NOT INLIST(%LocalRuleBaseNameToCheck,%AuxGloRuleManName))
    #SELECT(%AuxGloRuleManName,1)
    #SET(%LocalRuleBaseNameToCheck,%AuxGloRuleManName)
  #ENDIF
  #IF(%RulesSelection=2)
    #CALL(%FillAuxGloRuleName,%LocalRuleBaseNameToCheck)
  #ENDIF
#ENDPREPARE
#BOXED,HIDE
  #PROMPT('AuxRuleName:',@S255),%AuxRuleName,MULTI(''),AT(0,0,0,0)
  #PROMPT('AuxGloRuleManName:',@S255),%AuxGloRuleManName,MULTI(''),AT(0,0,0,0)
  #PROMPT('AuxGloRuleName:',@S255),%AuxGloRuleName,MULTI(''),AT(0,0,0,0)
#ENDBOXED
#PROMPT('Rules to check',OPTION),%RulesSelection,CHOICE,AT(10,0,,25),DEFAULT(1)
#PROMPT('All',RADIO),AT(20,10)
#PROMPT('Global',RADIO),AT(60,10)
#PROMPT('Local',RADIO),AT(100,10)
#BOXED,SECTION
   #BOXED,WHERE(%RulesSelection=1),AT(0,0,180)
      #DISPLAY('All Business Rules will be applied on this procedure.')
   #ENDBOXED
   #BOXED,WHERE(%RulesSelection=2),AT(0,0,180)
      #PROMPT('Global Rulebase Name:',FROM(%AuxGloRuleManName,,%AuxGloRuleManName)),%LocalRuleBaseNameToCheck,REQ,WHENACCEPTED(%FillAuxGloRuleName(%LocalRuleBaseNameToCheck))
   #ENDBOXED
   #BOXED,WHERE(%RulesSelection=3),AT(0,0,180)
      #PROMPT('Rulebase Name:',FROM(%RuleBases,,%RuleBaseName)),%RuleBaseNameToCheck,REQ,DEFAULT(%GetFirstRuleBaseName()),WHENACCEPTED(%FillAuxRuleName(%RuleBaseNameToCheck,%RuleNameToCheck))
   #ENDBOXED
#ENDBOXED
#BOXED,SECTION
  #BOXED,WHERE(%RulesSelection<>1),AT(0,0,180)
    #PROMPT('Select Function',OPTION),%ThisFunction,CHOICE,DEFAULT(1)
    #PROMPT('Count Broken Rules',RADIO)
    #PROMPT('Check All Rules ',RADIO)
    #PROMPT('List Broken Rules ',RADIO)
    #PROMPT('Check and List Broken Rules ',RADIO)
    #PROMPT('Check and List All Rules ',RADIO)
    #PROMPT('Check a Rule ',RADIO)
  #ENDBOXED
  #BOXED,WHERE(%RulesSelection=1),AT(0,0,180)
    #PROMPT('Select Function',OPTION),%ThisFunction,CHOICE,DEFAULT(1)
    #PROMPT('Count Broken Rules',RADIO)
    #PROMPT('Check All Rules ',RADIO)
    #PROMPT('List Broken Rules ',RADIO)
    #PROMPT('Check and List Broken Rules ',RADIO)
    #PROMPT('Check and List All Rules ',RADIO)
  #ENDBOXED
#ENDBOXED
#ENABLE(%ThisFunction<>3 AND %ThisFunction<>4 AND %ThisFunction<>5)
  #BUTTON('Business Rules Handler Prompts'),AT(10,,180)
    #BOXED
      #SHEET,HSCROLL
        #TAB('When Function is True')
          #BOXED,SECTION
            #BUTTON('Assign Values'),MULTI(%CheckedAssigns,%CheckedAssignVariable & ' = ' & %CheckedAssignValue),AT(7,0,105),HLP('~TPLFieldCheckBox_CheckedAssignValues')
              #DISPLAY('Values to assign when the check box is checked')
              #DISPLAY
              #PROMPT('Variable to Assign',FIELD),%CheckedAssignVariable,REQ
              #PROMPT('Value to Assign',@S64),%CheckedAssignValue,REQ
            #ENDBUTTON
            #BUTTON('Copy to "False"'),WHENACCEPTED(%CopyCheckboxControlPrompts(2,1)),AT(115,0,65)
            #ENDBUTTON
          #ENDBOXED
          #BOXED,SECTION
            #BUTTON('Hide/Unhide Controls'),MULTI(%CheckedHides,%CheckedControl & ' - ' & %CheckedControlAction),AT(7,0,105),HLP('~TPLFieldCheckBox_CheckedHideUnhide')
              #DISPLAY('Controls to Hide/Unhide when the check box is checked')
              #DISPLAY
              #PROMPT('Control to Hide/Unhide:',CONTROL),%CheckedControl,REQ
              #PROMPT('Hide or Unhide control',DROP('Hide|Unhide')),%CheckedControlAction,REQ
            #ENDBUTTON
            #BUTTON('Copy to "False"'),WHENACCEPTED(%CopyCheckboxControlPrompts(2,2)),AT(115,0,65)
            #ENDBUTTON
          #ENDBOXED
          #BOXED,SECTION
            #BUTTON('Enable/Disable Controls'),MULTI(%CheckedEnables,%CheckedControl1 & ' - ' & %CheckedControl1Action),AT(7,0,105),HLP('~TPLFieldCheckBox_CheckedEnableDisable')
              #DISPLAY('Controls to Enable/Disable when the check box is checked')
              #DISPLAY
              #PROMPT('Control to Enable/Disable:',CONTROL),%CheckedControl1,REQ
              #PROMPT('Enable or Disable control',DROP('Enable|Disable')),%CheckedControl1Action,REQ
            #ENDBUTTON
            #BUTTON('Copy to "False"'),WHENACCEPTED(%CopyCheckboxControlPrompts(2,3)),AT(115,0,65)
            #ENDBUTTON
          #ENDBOXED
          #BOXED,SECTION
            #BUTTON('Display a Message'),AT(7,0,105),HLP('~TPLFieldCheckBox_CheckedMessage')
              #DISPLAY('Message to display when the check box is checked')
              #DISPLAY
              #PROMPT('Enable Message',CHECK),%CheckedMessageEnable,DEFAULT(%False),AT(10)
              #ENABLE(%CheckedMessageEnable)
                #PROMPT('Title:',@S255),%CheckedMessageTitle,REQ
                #DISPLAY('Message:')
                #PROMPT('Message:',TEXT),%CheckedMessageText,REQ
                #PROMPT('Icon:',DROP('None|Application|Hand|Question|Exclamation|Asterisk|Help|Tick|Cross')),%CheckedMessageIcon,DEFAULT('None')
                #PROMPT('Button:',DROP('OK|Yes|No|Abort|Retry|Ignore|Cancel|Help')),%CheckedMessageButton,DEFAULT('OK')
              #ENDENABLE
            #ENDBUTTON
            #BUTTON('Copy to "False"'),WHENACCEPTED(%CopyCheckboxControlPrompts(2,4)),AT(115,0,65)
            #ENDBUTTON
          #ENDBOXED
          #BOXED,SECTION
            #BUTTON('Routines/Procedure/Code'),MULTI(%CheckedRoutines,CHOOSE(%CheckedRoutineType='Routine','DO '&%CheckedRoutine,CHOOSE(%CheckedRoutineType='Code',%CheckedCode,CHOOSE(%CheckedProcedureReturnValue<>'',%CheckedProcedureReturnValue&' = ','')&%CheckedProcedure&'('&%CheckedProcedureParameters&')'))),AT(7,0,105),HLP('~TPLFieldCheckBox_CheckedRoutines')
              #DISPLAY('Code to execute when the check box is checked')
              #DISPLAY
              #PROMPT('Type:',DROP('Routine|Procedure|Code')),%CheckedRoutineType,DEFAULT('Routine')
              #BOXED,SECTION
                #BOXED,WHERE(%CheckedRoutineType='Routine'),AT(0,0)
                  #PROMPT('Routine:',@S255),%CheckedRoutine,REQ
                #ENDBOXED
                #BOXED,WHERE(%CheckedRoutineType='Code'),AT(0,0)
                  #DISPLAY('Code:')
                  #PROMPT('Code:',TEXT),%CheckedCode,REQ
                #ENDBOXED
                #BOXED,WHERE(%CheckedRoutineType='Procedure'),AT(0,0)
                  #PROMPT('Procedure:',PROCEDURE),%CheckedProcedure,REQ
                  #PROMPT('Parameters:',EDIT(1024)),%CheckedProcedureParameters,WHENACCEPTED(%SVExpresionEditor(%CheckedProcedureParameters))
                  #PROMPT('Return Variable:',FIELD),%CheckedProcedureReturnValue
                #ENDBOXED
              #ENDBOXED
            #ENDBUTTON
            #BUTTON('Copy to "False"'),WHENACCEPTED(%CopyCheckboxControlPrompts(2,5)),AT(115,0,65)
            #ENDBUTTON
          #ENDBOXED
        #ENDTAB
        #TAB('When Function is False')
          #BOXED,SECTION
            #BUTTON('Assign Values'),MULTI(%UncheckedAssigns,%UncheckedAssignVariable & ' = ' & %UncheckedAssignValue),AT(7,0,105),HLP('~TPLFieldCheckBox_UncheckedAssignValues')
              #DISPLAY('Values to assign when the check box is unchecked')
              #DISPLAY
              #PROMPT('Variable to Assign',FIELD),%UncheckedAssignVariable,REQ
              #PROMPT('Value to Assign',@S64),%UncheckedAssignValue,REQ
            #ENDBUTTON
            #BUTTON('Copy to "True"'),WHENACCEPTED(%CopyCheckboxControlPrompts(1,1)),AT(115,0,65)
            #ENDBUTTON
          #ENDBOXED
          #BOXED,SECTION
            #BUTTON('Hide/Unhide Controls'),MULTI(%UnCheckedHides,%UnCheckedControl & ' - ' & %UnCheckedControlAction),AT(7,0,105),HLP('~TPLFieldCheckBox_UncheckedHideUnhide')
              #DISPLAY('Controls to Hide/Unhide when the check box is unchecked')
              #DISPLAY
              #PROMPT('Control to Hide/Unhide:',CONTROL),%UnCheckedControl,REQ
              #PROMPT('Hide or Unhide control',DROP('Hide|Unhide')),%UnCheckedControlAction,REQ
            #ENDBUTTON
            #BUTTON('Copy to "True"'),WHENACCEPTED(%CopyCheckboxControlPrompts(1,2)),AT(115,0,65)
            #ENDBUTTON
          #ENDBOXED
          #BOXED,SECTION
            #BUTTON('Enable/Disable Controls'),MULTI(%UnCheckedEnables,%UnCheckedControl1 & ' - ' & %UnCheckedControl1Action),AT(7,0,105),HLP('~TPLFieldCheckBox_UnCheckedEnableDisable')
              #DISPLAY('Controls to Enable/Disable when the check box is unchecked')
              #DISPLAY
              #PROMPT('Control to Enable/Disable:',CONTROL),%UnCheckedControl1,REQ
              #PROMPT('Enable or Disable control',DROP('Enable|Disable')),%UnCheckedControl1Action,REQ
            #ENDBUTTON
            #BUTTON('Copy to "True"'),WHENACCEPTED(%CopyCheckboxControlPrompts(1,3)),AT(115,0,65)
            #ENDBUTTON
          #ENDBOXED
          #BOXED,SECTION
            #BUTTON('Display a Message'),AT(7,0,105),HLP('~TPLFieldCheckBox_UnCheckedMessage')
              #DISPLAY('Message to display when the check box is unchecked')
              #DISPLAY
              #PROMPT('Enable Message',CHECK),%UnCheckedMessageEnable,DEFAULT(%False),AT(10)
              #ENABLE(%UnCheckedMessageEnable)
                #PROMPT('Title:',@S255),%UnCheckedMessageTitle,REQ
                #DISPLAY('Message:')
                #PROMPT('Message:',TEXT),%UnCheckedMessageText,REQ
                #PROMPT('Icon:',DROP('None|Application|Hand|Question|Exclamation|Asterisk|Help|Tick|Cross')),%UnCheckedMessageIcon,DEFAULT('None')
                #PROMPT('Button:',DROP('OK|Yes|No|Abort|Retry|Ignore|Cancel|Help')),%UnCheckedMessageButton,DEFAULT('OK')
              #ENDENABLE
            #ENDBUTTON
            #BUTTON('Copy to "True"'),WHENACCEPTED(%CopyCheckboxControlPrompts(1,4)),AT(115,0,65)
            #ENDBUTTON
          #ENDBOXED
          #BOXED,SECTION
            #BUTTON('Routines/Procedure/Code'),MULTI(%UnCheckedRoutines,CHOOSE(%UnCheckedRoutineType='Routine','DO '&%UnCheckedRoutine,CHOOSE(%UnCheckedRoutineType='Code',%UnCheckedCode,CHOOSE(%UnCheckedProcedureReturnValue<>'',%UnCheckedProcedureReturnValue&' = ','')&%UnCheckedProcedure&'('&%UnCheckedProcedureParameters&')'))),AT(7,0,105),HLP('~TPLFieldCheckBox_CheckedRoutines')
              #DISPLAY('Code to execute when the check box is unchecked')
              #DISPLAY
              #PROMPT('Type:',DROP('Routine|Procedure|Code')),%UnCheckedRoutineType,DEFAULT('Routine')
              #BOXED,SECTION
                #BOXED,WHERE(%UnCheckedRoutineType='Routine'),AT(0,0)
                  #PROMPT('Routine:',@S255),%UnCheckedRoutine,REQ
                #ENDBOXED
                #BOXED,WHERE(%UnCheckedRoutineType='Code'),AT(0,0)
                  #DISPLAY('Code:')
                  #PROMPT('Code:',TEXT),%UnCheckedCode,REQ
                #ENDBOXED
                #BOXED,WHERE(%UnCheckedRoutineType='Procedure'),AT(0,0)
                  #PROMPT('Procedure:',PROCEDURE),%UnCheckedProcedure,REQ
                  #PROMPT('Parameters:',EDIT(1024)),%UnCheckedProcedureParameters,WHENACCEPTED(%SVExpresionEditor(%UnCheckedProcedureParameters))
                  #PROMPT('Return Variable:',FIELD),%UnCheckedProcedureReturnValue
                #ENDBOXED
              #ENDBOXED
            #ENDBUTTON
            #BUTTON('Copy to "True"'),WHENACCEPTED(%CopyCheckboxControlPrompts(1,5)),AT(115,0,65)
            #ENDBUTTON
          #ENDBOXED
        #ENDTAB
      #ENDSHEET
    #ENDBOXED
  #ENDBUTTON
#ENDENABLE
#BOXED,SECTION
  #BOXED('Count Broken Rules'),WHERE(%ThisFunction = 1),AT(10,0,180)
    #PROMPT('Destination Variable:',EDIT(256)),%DestField1,WHENACCEPTED(%SVExpresionEditor(%DestField1))
    #PROMPT('UPDATE() screen before checking Rule',CHECK),%UpdateBeforeCount,AT(10)
  #ENDBOXED
  #BOXED('Check all Rules'),WHERE(%ThisFunction = 2),AT(10,0,180)
    #PROMPT('Destination Variable:',EDIT(256)),%DestField3,WHENACCEPTED(%SVExpresionEditor(%DestField3))
    #PROMPT('Modify error indicator',CHECK),%ModifyIndicatorAll,AT(10),DEFAULT(1)
    #PROMPT('UPDATE() screen before checking Rule',CHECK),%UpdateBeforeAll,AT(10)
    #PROMPT('DISPLAY() screen after checking Rule',CHECK),%DisplayAfterAll,AT(10)
  #ENDBOXED
  #BOXED('List Broken Rules'),WHERE(%ThisFunction = 3 OR %ThisFunction = 4 OR %ThisFunction = 5),AT(10,0,180)
    #DISPLAY('No prompts are required for this selection')
  #ENDBOXED
  #BOXED('Check Broken Rule'),WHERE(%ThisFunction = 6 AND %RulesSelection<>1),AT(10,0,180)
    #PROMPT('Destination Variable:',EDIT(256)),%DestField2,WHENACCEPTED(%SVExpresionEditor(%DestField2))
    #BOXED,SECTION
      #BOXED,AT(0,0),WHERE(%RulesSelection=3)
        #PROMPT('Rule Name:',FROM(%AuxRuleName)),%RuleNameToCheck,REQ
      #ENDBOXED
      #BOXED,AT(0,0),WHERE(%RulesSelection=2)
        #PROMPT('Rule Name:',FROM(%AuxGloRuleName)),%RuleNameToCheck,REQ
      #ENDBOXED
    #ENDBOXED
    #PROMPT('Modify error indicator',CHECK),%ModifyIndicator,AT(10),DEFAULT(1)
    #PROMPT('UPDATE() screen before checking Rule',CHECK),%UpdateBefore,AT(10)
    #PROMPT('DISPLAY() screen after checking Rule',CHECK),%DisplayAfter,AT(10)
  #ENDBOXED
#ENDBOXED
#!
#DECLARE(%lIfContent)
#CASE(%ThisFunction)
#OF(1)
#IF(%UpdateBeforeCount)
UPDATE
#ENDIF
#IF(%DestField1)
    #IF(%RulesSelection<>1)
%DestField1 = %RuleBaseNameToCheck.BrokenRuleCount()                                                    #<!Counting Broken Rules in RulesManager
    #ELSE
%DestField1 = BusinessRulesManager.BrokenRulesCount()                                                    #<!Counting Broken Rules in RulesManager
    #ENDIF
    #IF(%RulesManagerAreThereAnyAction())
IF %DestField1 
   #INSERT(%RulesManagerCheckedActions)
ELSE
   #INSERT(%RulesManagerUnCheckedActions)
END
    #ENDIF
#ELSE
    #IF(%RulesManagerAreThereAnyAction())
        #IF(%RulesSelection<>1)
IF %RuleBaseNameToCheck.BrokenRuleCount()                                                    #<!Counting Broken Rules in RulesManager
        #ELSE
IF BusinessRulesManager.BrokenRulesCount()                                                    #<!Counting Broken Rules in RulesManager
        #ENDIF
   #INSERT(%RulesManagerCheckedActions)
ELSE
   #INSERT(%RulesManagerUnCheckedActions)
END
    #ENDIF
#ENDIF
#OF(6)
#IF(%RulesSelection<>1)
#IF(%UpdateBefore)
UPDATE
#ENDIF
#IF(%DestField2)
%DestField2 = %RuleBaseNameToCheck.CheckRule('%RuleNameToCheck',%ModifyIndicator)                              #<!Checking for broken rule in RulesManager
    #IF(%RulesManagerAreThereAnyAction())
IF %DestField2 
   #INSERT(%RulesManagerCheckedActions)
ELSE
   #INSERT(%RulesManagerUnCheckedActions)
END
    #ENDIF
#ELSE
    #IF(%RulesManagerAreThereAnyAction())
IF %RuleBaseNameToCheck.CheckRule('%RuleNameToCheck',%ModifyIndicator)                                            #<!Checking for broken rule in RulesManager
   #INSERT(%RulesManagerCheckedActions)
ELSE
   #INSERT(%RulesManagerUnCheckedActions)
END
    #ELSE
%RuleBaseNameToCheck.CheckRule('%RuleNameToCheck',%ModifyIndicator)                                            #<!Checking for broken rule in RulesManager
    #ENDIF
#ENDIF
#IF(%DisplayAfter)
DISPLAY
#ENDIF
#ENDIF
#OF(2)
#IF(%UpdateBeforeAll)
UPDATE
#ENDIF
#IF(%DestField3)
    #IF(%RulesSelection<>1)
%DestField3 = %RuleBaseNameToCheck.CheckAllRules(%ModifyIndicatorAll)                                   #<!Checking all Rules in RulesManager
    #ELSE
%DestField3 = BusinessRulesManager.CheckAllRules(%ModifyIndicatorAll)                                   #<!Checking all Rules in RulesManager
    #ENDIF
    #IF(%RulesManagerAreThereAnyAction())
IF %DestField3 
   #INSERT(%RulesManagerCheckedActions)
ELSE
   #INSERT(%RulesManagerUnCheckedActions)
END
    #ENDIF
#ELSE
    #IF(%RulesManagerAreThereAnyAction())
        #IF(%RulesSelection<>1)
IF %RuleBaseNameToCheck.CheckAllRules(%ModifyIndicatorAll)                                                 #<!Checking all Rules in RulesManager
        #ELSE
IF BusinessRulesManager.CheckAllRules(%ModifyIndicatorAll)                                                 #<!Checking all Rules in RulesManager
        #ENDIF
   #INSERT(%RulesManagerCheckedActions)
ELSE
   #INSERT(%RulesManagerUnCheckedActions)
END
    #ELSE
        #IF(%RulesSelection<>1)
%RuleBaseNameToCheck.CheckAllRules(%ModifyIndicatorAll)                                                 #<!Checking all Rules in RulesManager
        #ELSE
BusinessRulesManager.CheckAllRules(%ModifyIndicatorAll)                                                 #<!Checking all Rules in RulesManager
        #ENDIF
    #ENDIF
#ENDIF
#IF(%DisplayAfterAll)
DISPLAY
#ENDIF
#OF(3)
#IF(%RulesSelection<>1)
%RuleBaseNameToCheck.EnumerateBrokenRules(%RuleBaseNameToCheck.GetDescription(),%RuleBaseNameToCheck.GetErrorImage())
IF %RuleBaseNameToCheck.BrokenRuleControl 
   SELECT(%RuleBaseNameToCheck.BrokenRuleControl)
END
#ELSE
BusinessRulesManager.EnumerateBrokenRules('%'LocalAllRuleBaseDescription','~%GlobalErrorIndicatorImage')
IF BusinessRulesManager.BrokenRuleControl 
   SELECT(BusinessRulesManager.BrokenRuleControl)
END
#ENDIF
#OF(4)
        #IF(%RulesSelection<>1)
%RuleBaseNameToCheck.CheckAllRules(%ModifyIndicatorAll)                                                 #<!Checking all Rules in RulesManager
        #ELSE
BusinessRulesManager.CheckAllRules(%ModifyIndicatorAll)                                                 #<!Checking all Rules in RulesManager
        #ENDIF
#IF(%RulesSelection<>1)
%RuleBaseNameToCheck.EnumerateBrokenRules(%RuleBaseNameToCheck.GetDescription()) #<!List Broken Rules in RulesManager
IF %RuleBaseNameToCheck.BrokenRuleControl 
   SELECT(%RuleBaseNameToCheck.BrokenRuleControl)
END
#ELSE
BusinessRulesManager.EnumerateBrokenRules('%'LocalAllRuleBaseDescription')              #<!List Broken Rules in RulesManager
IF BusinessRulesManager.BrokenRuleControl 
   SELECT(BusinessRulesManager.BrokenRuleControl)
END
#ENDIF
#OF(5)
        #IF(%RulesSelection<>1)
%RuleBaseNameToCheck.CheckAllRules(%ModifyIndicatorAll)                                                 #<!Checking all Rules in RulesManager
        #ELSE
BusinessRulesManager.CheckAllRules(%ModifyIndicatorAll)                                                 #<!Checking all Rules in RulesManager
        #ENDIF
#IF(%RulesSelection<>1)
%RuleBaseNameToCheck.EnumerateBrokenRules(%RuleBaseNameToCheck.GetDescription(),False) #<!List All Rules in RulesManager
IF %RuleBaseNameToCheck.BrokenRuleControl 
   SELECT(%RuleBaseNameToCheck.BrokenRuleControl)
END
#ELSE
BusinessRulesManager.EnumerateBrokenRules('%'LocalAllRuleBaseDescription',False)              #<!List All Rules in RulesManager
IF BusinessRulesManager.BrokenRuleControl 
   SELECT(BusinessRulesManager.BrokenRuleControl)
END
#ENDIF
#ENDCASE
#!------------------------------------------------------------
#!
#!
#!
#!
#!
#GROUP(%GlobalRulesAddOkControls,*%pGlobalRuleControls,%pUsingControls=0),AUTO
#IF(%pUsingControls)
  #FOR(%Control)
    #IF(UPPER(%Control)='?OK' OR UPPER(%Control)='?CANCEL' OR UPPER(%Control)='?OKBUTTON' OR UPPER(%Control)='?CANCELBUTTON' OR UPPER(%Control)='?CLOSE' OR UPPER(%Control)='?ACCEPT' OR UPPER(%Control)='?SAVE')
      #IF(NOT %SVInSymbol(%Control,%pGlobalRuleControls))
        #ADD(%pGlobalRuleControls,%Control)
      #ENDIF
    #ENDIF
  #ENDFOR
#ELSE
  #IF(NOT %SVInSymbol('?Ok',%pGlobalRuleControls))
    #ADD(%pGlobalRuleControls,'?Ok')
  #ENDIF
  #IF(NOT %SVInSymbol('?Cancel',%pGlobalRuleControls))
    #ADD(%pGlobalRuleControls,'?Cancel')
  #ENDIF
  #IF(NOT %SVInSymbol('?OkButton',%pGlobalRuleControls))
    #ADD(%pGlobalRuleControls,'?OkButton')
  #ENDIF
  #IF(NOT %SVInSymbol('?CancelButton',%pGlobalRuleControls))
    #ADD(%pGlobalRuleControls,'?CancelButton')
  #ENDIF
  #IF(NOT %SVInSymbol('?Close',%pGlobalRuleControls))
    #ADD(%pGlobalRuleControls,'?Close')
  #ENDIF
  #IF(NOT %SVInSymbol('?Accept',%pGlobalRuleControls))
    #ADD(%pGlobalRuleControls,'?Accept')
  #ENDIF
  #IF(NOT %SVInSymbol('?Save',%pGlobalRuleControls))
    #ADD(%pGlobalRuleControls,'?Save')
  #ENDIF
#ENDIF
#!
#!
#!
#!
#!
#GROUP(%LocalRulesGetControl,%pUseField),AUTO
#DECLARE(%lControl)
#CALL(%LocalRulesFindControl,%pUseField,%lControl)
#RETURN(%lControl)
#!
#!
#GROUP(%LocalRulesFindControl,%pUseField,*%pControl),PRESERVE
#SET(%pControl,'')
#FOR(%Control),WHERE(UPPER(%ControlUse)=UPPER(%pUseField))
    #SET(%pControl,%Control)
    #BREAK
#ENDFOR
#!
#!
#GROUP(%LocalRulesLoad),AUTO
#IF(%GlobalRulesTemplateMode = 'Clone')
  #CALL(%GlobalRulesImport,%GlobalRulesFileName&'.brf')
#ENDIF
#DECLARE(%lRuleExist)
#DECLARE(%AUXLocalRules),MULTI
#DECLARE(%AUXLocalRuleBaseName,%AUXLocalRules)
#DECLARE(%AUXLocalRuleName,%AUXLocalRules)
#DECLARE(%AUXLocalRuleDisable,%AUXLocalRules)
#DECLARE(%AUXLocalRuleShowIndicatorOverride,%AUXLocalRules)
#DECLARE(%AUXLocalRuleShowIndicatorOverrided,%AUXLocalRules)
#DECLARE(%AUXLocalRuleShowIndicatorDefault,%AUXLocalRules)
#DECLARE(%AUXLocalRuleControl,%AUXLocalRules)
#DECLARE(%AUXLocalRuleOffSetRight,%AUXLocalRules)
#DECLARE(%AUXLocalRuleHideUnHideControl,%AUXLocalRules),MULTI
#DECLARE(%AUXLocalRuleEnableDisableControl,%AUXLocalRules),MULTI
#FOR(%LocalRules)
    #ADD(%AUXLocalRules,%LocalRules)
    #SET(%AUXLocalRuleBaseName,%LocalRuleBaseName)
    #SET(%AUXLocalRuleName,%LocalRuleName)
    #SET(%AUXLocalRuleDisable,%LocalRuleDisable)
    #SET(%AUXLocalRuleShowIndicatorOverride,%LocalRuleShowIndicatorOverride)
    #SET(%AUXLocalRuleShowIndicatorOverrided,%LocalRuleShowIndicatorOverrided)
    #SET(%AUXLocalRuleShowIndicatorDefault,%LocalRuleShowIndicatorDefault)
    #SET(%AUXLocalRuleControl,%LocalRuleControl)
    #SET(%AUXLocalRuleOffSetRight,%LocalRuleOffSetRight)
    #FOR(%LocalRuleHideUnHideControl)
        #ADD(%AUXLocalRuleHideUnHideControl,%LocalRuleHideUnHideControl)
    #ENDFOR
    #FOR(%LocalRuleEnableDisableControl)
        #ADD(%AUXLocalRuleEnableDisableControl,%LocalRuleEnableDisableControl)
    #ENDFOR
#ENDFOR
#FREE(%LocalRules)
#FOR(%Control),WHERE(%ControlUse)
    #FOR(%GlobalRuleBases),WHERE(%GlobalRuleBaseName)
        #FOR(%GlobalRules),WHERE(%GlobalRuleName)
            #IF(UPPER(%GlobalRuleLinkField)=UPPER(%ControlUse))
                #SET(%lRuleExist,%False)
                #FOR(%LocalRules),WHERE(UPPER(%LocalRuleBaseName)=UPPER(%GlobalRuleBaseName) AND UPPER(%LocalRuleName)=UPPER(%GlobalRuleName))
                    #SET(%lRuleExist,%True)
                #ENDFOR
                #IF(NOT %lRuleExist)
                   #ADD(%LocalRules,ITEMS(%LocalRules)+1)
                   #SET(%LocalRuleBaseName,%GlobalRuleBaseName)
                   #SET(%LocalRuleBaseDescription,%GlobalRuleBaseDescription)
                   #SET(%LocalRuleName,%GlobalRuleName)
                   #SET(%LocalRuleDescription,%GlobalRuleDescription)
                   #SET(%LocalRuleExpression,%GlobalRuleExpression)
                   #SET(%LocalRuleLinkField,%GlobalRuleLinkField)
                   #SET(%LocalRuleShowIndicator,%GlobalRuleShowIndicator)
                   #SET(%LocalRuleOffSetRight,%GlobalRuleOffSetRight)
                   #SET(%LocalRuleShowIndicatorDefault,1)
                   #FOR(%GlobalRuleEnableDisableControl)
                        #ADD(%LocalRuleGlobalEnableDisableControl,%GlobalRuleEnableDisableControl)
                   #ENDFOR
                   #FOR(%GlobalRuleHideUnHideControl)
                        #ADD(%LocalRuleGlobalHideUnHideControl,%GlobalRuleHideUnHideControl)
                   #ENDFOR
                   #FOR(%AUXLocalRules),WHERE(UPPER(%AUXLocalRuleBaseName)=UPPER(%LocalRuleBaseName) AND UPPER(%AUXLocalRuleName)=UPPER(%LocalRuleName))
                       #SET(%LocalRuleDisable,%AUXLocalRuleDisable)
                       #SET(%LocalRuleShowIndicatorOverride,%AUXLocalRuleShowIndicatorOverride)
                       #SET(%LocalRuleShowIndicatorOverrided,%AUXLocalRuleShowIndicatorOverrided)
                       #SET(%LocalRuleShowIndicatorDefault,%AUXLocalRuleShowIndicatorDefault)
                       #SET(%LocalRuleControl,%AUXLocalRuleControl)
                       #SET(%LocalRuleOffSetRight,%AUXLocalRuleOffSetRight)
                       #FOR(%AUXLocalRuleHideUnHideControl)
                           #ADD(%LocalRuleHideUnHideControl,%AUXLocalRuleHideUnHideControl)
                       #ENDFOR
                       #FOR(%AUXLocalRuleEnableDisableControl)
                           #ADD(%LocalRuleEnableDisableControl,%AUXLocalRuleEnableDisableControl)
                       #ENDFOR
                       #BREAK
                   #ENDFOR
                #ENDIF
            #ENDIF
        #ENDFOR
    #ENDFOR
#ENDFOR
#FOR(%HotFields)
    #FOR(%GlobalRuleBases),WHERE(%GlobalRuleBaseName)
        #FOR(%GlobalRules),WHERE(%GlobalRuleName)
            #IF(UPPER(%GlobalRuleLinkField)=UPPER(%HotField))
                #SET(%lRuleExist,%False)
                #FOR(%LocalRules),WHERE(UPPER(%LocalRuleBaseName)=UPPER(%GlobalRuleBaseName) AND UPPER(%LocalRuleName)=UPPER(%GlobalRuleName))
                    #SET(%lRuleExist,%True)
                #ENDFOR
                #IF(NOT %lRuleExist)
                   #ADD(%LocalRules,ITEMS(%LocalRules)+1)
                   #SET(%LocalRuleBaseName,%GlobalRuleBaseName)
                   #SET(%LocalRuleBaseDescription,%GlobalRuleBaseDescription)
                   #SET(%LocalRuleName,%GlobalRuleName)
                   #SET(%LocalRuleDescription,%GlobalRuleDescription)
                   #SET(%LocalRuleExpression,%GlobalRuleExpression)
                   #SET(%LocalRuleLinkField,%GlobalRuleLinkField)
                   #SET(%LocalRuleShowIndicator,%GlobalRuleShowIndicator)
                   #SET(%LocalRuleOffSetRight,%GlobalRuleOffSetRight)
                   #SET(%LocalRuleShowIndicatorDefault,1)
                   #FOR(%GlobalRuleEnableDisableControl)
                        #ADD(%LocalRuleGlobalEnableDisableControl,%GlobalRuleEnableDisableControl)
                   #ENDFOR
                   #FOR(%GlobalRuleHideUnHideControl)
                        #ADD(%LocalRuleGlobalHideUnHideControl,%GlobalRuleHideUnHideControl)
                   #ENDFOR
                   #FOR(%AUXLocalRules),WHERE(UPPER(%AUXLocalRuleBaseName)=UPPER(%LocalRuleBaseName) AND UPPER(%AUXLocalRuleName)=UPPER(%LocalRuleName))
                       #SET(%LocalRuleDisable,%AUXLocalRuleDisable)
                       #SET(%LocalRuleShowIndicatorOverride,%AUXLocalRuleShowIndicatorOverride)
                       #SET(%LocalRuleShowIndicatorOverrided,%AUXLocalRuleShowIndicatorOverrided)
                       #SET(%LocalRuleShowIndicatorDefault,%AUXLocalRuleShowIndicatorDefault)
                       #SET(%LocalRuleControl,%AUXLocalRuleControl)
                       #SET(%LocalRuleOffSetRight,%AUXLocalRuleOffSetRight)
                       #FOR(%AUXLocalRuleHideUnHideControl)
                           #ADD(%LocalRuleHideUnHideControl,%AUXLocalRuleHideUnHideControl)
                       #ENDFOR
                       #FOR(%AUXLocalRuleEnableDisableControl)
                           #ADD(%LocalRuleEnableDisableControl,%AUXLocalRuleEnableDisableControl)
                       #ENDFOR
                       #BREAK
                   #ENDFOR
                #ENDIF
            #ENDIF
        #ENDFOR
    #ENDFOR
#ENDFOR
#!
#!
#GROUP(%FillAuxGloRuleManName)
#FREE(%AuxGloRuleManName)
#FOR(%LocalRules)
    #IF(NOT INLIST(%LocalRuleBaseName,%AuxGloRuleManName))
        #ADD(%AuxGloRuleManName,%LocalRuleBaseName)
    #ENDIF
#ENDFOR
#GROUP(%FillAuxGloRuleName,%pAuxGloRuleManName)
#FREE(%AuxGloRuleName)
#FOR(%LocalRules),WHERE(UPPER(%pAuxGloRuleManName)=UPPER(%LocalRuleBaseName))
    #IF(NOT INLIST(%LocalRuleName,%AuxGloRuleName))
        #ADD(%AuxGloRuleName,%LocalRuleName)
    #ENDIF
#ENDFOR
#!
#!
#GROUP(%GetFirstRuleBaseName)
 #FOR(%RuleBases)
    #RETURN(%RuleBaseName)
    #BREAK
 #ENDFOR
 #RETURN(0)
#!
#!
#GROUP(%FillAuxRuleName,%pRuleBaseName,*%pRuleNameToCheck)
 #DECLARE(%WasNull)
 #SET(%WasNull,%False)
 #FREE(%AuxRuleName)
 #FOR(%RuleBases),WHERE(UPPER(%pRuleBaseName)=UPPER(%RuleBaseName))
       #FOR(%Rules)
            #ADD(%AuxRuleName,%RuleName)
            #IF(NOT %pRuleNameToCheck)
               #SET(%pRuleNameToCheck,%RuleName)
               #SET(%WasNull,%True)
            #ENDIF
       #ENDFOR
 #ENDFOR
 #IF(NOT %WasNull)
    #IF(NOT INLIST(%pRuleNameToCheck,%AuxRuleName))
       #FOR(%AuxRuleName)
           #SET(%pRuleNameToCheck,%AuxRuleName)
           #BREAK
       #ENDFOR
    #ENDIF
 #ENDIF
#!
#!
#!
#GROUP(%RulesManagerAreThereAnyAction),AUTO
#DECLARE(%AreThereAny)
#SET(%AreThereAny,%False)
#FOR(%CheckedAssigns)
     #RETURN(%True)
#ENDFOR
#FOR(%UncheckedAssigns)
     #RETURN(%True)
#ENDFOR
#FOR(%CheckedHides)
     #RETURN(%True)
#ENDFOR
#FOR(%UnCheckedHides)
     #RETURN(%True)
#ENDFOR
#FOR(%CheckedEnables)
     #RETURN(%True)
#ENDFOR
#FOR(%UnCheckedEnables)
     #RETURN(%True)
#ENDFOR
#IF(%UnCheckedMessageEnable OR %CheckedMessageEnable)
     #RETURN(%True)
#ENDIF
#FOR(%CheckedRoutines)
     #RETURN(%True)
#ENDFOR
#FOR(%UnCheckedRoutines)
     #RETURN(%True)
#ENDFOR
#RETURN(%False)
#!
#!
#!
#GROUP(%CopyCheckboxControlPrompts,%pFrom,%pPrompt)
#CASE(%pPrompt)
#OF(1)
   #IF(%pFrom=2)
       #FREE(%UncheckedAssigns)
       #FOR(%CheckedAssigns)
           #ADD(%UncheckedAssigns,%CheckedAssigns)
           #SET(%UncheckedAssignVariable,%CheckedAssignVariable)
           #SET(%UncheckedAssignValue,%CheckedAssignValue)
       #ENDFOR
   #ELSE
       #FREE(%CheckedAssigns)
       #FOR(%UncheckedAssigns)
           #ADD(%CheckedAssigns,%UncheckedAssigns)
           #SET(%CheckedAssignVariable,%UncheckedAssignVariable)
           #SET(%CheckedAssignValue,%UncheckedAssignValue)
       #ENDFOR
   #ENDIF
#OF(2)
   #IF(%pFrom=2)
       #FREE(%UnCheckedHides)
       #FOR(%CheckedHides)
           #ADD(%UnCheckedHides,%CheckedHides)
           #SET(%UnCheckedControl,%CheckedControl)
           #SET(%UnCheckedControlAction,CHOOSE(UPPER(%CheckedControlAction)='HIDE','Unhide','Hide'))
       #ENDFOR
   #ELSE
       #FREE(%CheckedHides)
       #FOR(%UnCheckedHides)
           #ADD(%CheckedHides,%UnCheckedHides)
           #SET(%CheckedControl,%UnCheckedControl)
           #SET(%CheckedControlAction,CHOOSE(UPPER(%UnCheckedControlAction)='HIDE','Unhide','Hide'))
       #ENDFOR
   #ENDIF
#OF(3)
   #IF(%pFrom=2)
       #FREE(%UnCheckedEnables)
       #FOR(%CheckedEnables)
            #ADD(%UnCheckedEnables,%CheckedEnables)
            #SET(%UnCheckedControl1,%CheckedControl1)
            #SET(%UnCheckedControl1Action,CHOOSE(UPPER(%CheckedControl1Action)='DISABLE','Enable','Disable'))
       #ENDFOR
   #ELSE
       #FREE(%CheckedEnables)
       #FOR(%UnCheckedEnables)
            #ADD(%UnCheckedEnables,%CheckedEnables)
            #SET(%CheckedControl1,%UnCheckedControl1)
            #SET(%CheckedControl1Action,CHOOSE(UPPER(%UnCheckedControl1Action)='DISABLE','Enable','Disable'))
       #ENDFOR
   #ENDIF
#OF(4)
   #IF(%pFrom=2)
       #SET(%UnCheckedMessageEnable,%CheckedMessageEnable)
       #SET(%UnCheckedMessageTitle,%CheckedMessageTitle)
       #SET(%UnCheckedMessageText,%CheckedMessageText)
       #SET(%UnCheckedMessageIcon,%CheckedMessageIcon)
       #SET(%UnCheckedMessageText,%CheckedMessageButton)
   #ELSE
       #SET(%CheckedMessageEnable,%UnCheckedMessageEnable)
       #SET(%CheckedMessageTitle,%UnCheckedMessageTitle)
       #SET(%CheckedMessageText,%UnCheckedMessageText)
       #SET(%CheckedMessageIcon,%UnCheckedMessageIcon)
       #SET(%CheckedMessageText,%UnCheckedMessageButton)
   #ENDIF
#OF(5)
   #IF(%pFrom=2)
       #FREE(%UnCheckedRoutines)
       #FOR(%CheckedRoutines)
            #ADD(%UnCheckedRoutines,%CheckedRoutines)
            #SET(%UnCheckedRoutine,%CheckedRoutine)
            #SET(%UnCheckedRoutineType,%CheckedRoutineType)
            #SET(%UnCheckedCode,%CheckedCode)
            #SET(%UnCheckedProcedure,%CheckedProcedure)
            #SET(%UnCheckedProcedureParameters,%CheckedProcedureParameters)
            #SET(%UnCheckedProcedureReturnValue,%CheckedProcedureReturnValue)
       #ENDFOR
   #ELSE
       #FREE(%CheckedRoutines)
       #FOR(%UnCheckedRoutines)
            #ADD(%CheckedRoutines,%UnCheckedRoutines)
            #SET(%CheckedRoutine,%UnCheckedRoutine)
            #SET(%CheckedRoutineType,%UnCheckedRoutineType)
            #SET(%CheckedCode,%UnCheckedCode)
            #SET(%CheckedProcedure,%UnCheckedProcedure)
            #SET(%CheckedProcedureParameters,%UnCheckedProcedureParameters)
            #SET(%CheckedProcedureReturnValue,%UnCheckedProcedureReturnValue)
       #ENDFOR
   #ENDIF
#ENDCASE
#GROUP(%RulesManagerCheckedActions),AUTO
#DECLARE(%lSource)
      #FOR(%CheckedAssigns),WHERE(%CheckedAssignVariable)
  %CheckedAssignVariable = %CheckedAssignValue
      #ENDFOR
      #FOR(%CheckedHides)
        #IF(%CheckedControlAction='Hide')
  HIDE(%CheckedControl)
        #ELSE
  UNHIDE(%CheckedControl)
        #ENDIF
      #ENDFOR
      #FOR(%CheckedEnables)
        #IF(%CheckedControl1Action='Enable')
  ENABLE(%CheckedControl1)
        #ELSE
  DISABLE(%CheckedControl1)
        #ENDIF
      #ENDFOR
      #IF(%CheckedMessageEnable)
            #SET(%lSource,%CheckedMessageText)
  #CALL(%ReplaceSubString,%lSource,'<13,10>','|')
  MESSAGE('%'lSource','%'CheckedMessageTitle',ICON:%CheckedMessageIcon,BUTTON:%CheckedMessageButton)
      #ENDIF
      #FOR(%CheckedRoutines)
        #CASE(%CheckedRoutineType)
        #OF('Routine')
  DO %CheckedRoutine
        #OF('Code')
  #INSERT(%SVInsertCode,%CheckedCode)
        #OF('Procedure')
            #IF(%CheckedProcedureReturnValue)
  %CheckedProcedureReturnValue = %CheckedProcedure(%CheckedProcedureParameters)
            #ELSE
  %CheckedProcedure(%CheckedProcedureParameters)
            #ENDIF
        #ENDCASE
      #ENDFOR
#!
#!
#GROUP(%SVInsertCode,%pCheckedCode),AUTO
#DECLARE(%lLine)
#DECLARE(%lXPos)
#SET(%lXPos,1)
#LOOP
    #SET(%lXPos,INSTRING(CHR(13)&CHR(10),%pCheckedCode,1,1))
    #IF(%lXPos=0)
%pCheckedCode
        #BREAK
    #ENDIF
    #SET(%lLine,SUB(%pCheckedCode,1,%lXPos-1))
    #SET(%pCheckedCode,SUB(%pCheckedCode,%lXPos+2,LEN(%pCheckedCode)))
%lLine
#ENDLOOP
#!
#!
#GROUP(%RulesManagerUnCheckedActions)
#DECLARE(%lSource)
      #FOR(%UnCheckedAssigns),WHERE(%UnCheckedAssignVariable)
  %UnCheckedAssignVariable = %UnCheckedAssignValue
      #ENDFOR
      #FOR(%UnCheckedHides)
        #IF(%UnCheckedControlAction='Hide')
  HIDE(%UnCheckedControl)
        #ELSE
  UNHIDE(%UnCheckedControl)
        #ENDIF
      #ENDFOR
      #FOR(%UnCheckedEnables)
        #IF(%UnCheckedControl1Action='Enable')
  ENABLE(%UnCheckedControl1)
        #ELSE
  DISABLE(%UnCheckedControl1)
        #ENDIF
      #ENDFOR
      #IF(%UnCheckedMessageEnable)
            #SET(%lSource,%UnCheckedMessageText)
  #CALL(%ReplaceSubString,%lSource,'<13,10>','|')
  MESSAGE('%'UnCheckedMessageText','%'lSource',ICON:%UnCheckedMessageIcon,BUTTON:%UnCheckedMessageButton)
      #ENDIF
      #FOR(%UnCheckedRoutines)
        #CASE(%UnCheckedRoutineType)
        #OF('Routine')
  DO %UnCheckedRoutine
        #OF('Code')
  #INSERT(%SVInsertCode,%UnCheckedCode)
        #OF('Procedure')
            #IF(%UnCheckedProcedureReturnValue)
  %UnCheckedProcedureReturnValue = %UnCheckedProcedure(%UnCheckedProcedureParameters)
            #ELSE
  %UnCheckedProcedure(%UnCheckedProcedureParameters)
            #ENDIF
        #ENDCASE
      #ENDFOR
#!
#!
#GROUP(%ProcedureBindParseTextLoc,%pText)
#CALL(%ProcedureBindParseText(ABC),%pText)
#INCLUDE('SVFnGrp.TPW')
#!
#GROUP(%BusinessRulesLocalAtStart)
#FOR(%LocalRules),WHERE(NOT %LocalRuleDisable)
    #CALL(%ProcedureBindParseTextLoc,%LocalRuleExpression)
#ENDFOR

#FOR(%RuleBases)
    #FOR(%Rules)
        #CALL(%ProcedureBindParseTextLoc,%RuleDefinition)
    #ENDFOR
#ENDFOR
#!
#DECLARE(%AUXLRules),MULTI,UNIQUE
#DECLARE(%AUXLRulesDescription,%AUXLRules)
#DECLARE(%AUXLRulesHasLocalControls,%AUXLRules)
#DECLARE(%AUXLRulesHasGlobalControls,%AUXLRules)
#DECLARE(%AUXATRules),MULTI,UNIQUE
#!
#FREE(%AUXLRules)
  #FOR(%RuleBases)
    #ADD(%AUXLRules,%RuleBaseName)
    #SET(%AUXLRulesDescription,%RuleBaseDescription)
    #FOR(%Rules)
       #SET(%AUXLRulesHasLocalControls,0)
       #IF((ITEMS(%RuleHideUnHideControl) OR ITEMS(%RuleEnableDisableControl)))
           #SET(%AUXLRulesHasLocalControls,1)
       #ELSE
           #SET(%AUXLRulesHasLocalControls,0)
       #ENDIF
    #ENDFOR
  #ENDFOR
  #FOR(%LocalRules),WHERE(NOT %LocalRuleDisable)
       #ADD(%AUXLRules,%LocalRuleBaseName)
       #SET(%AUXLRulesDescription,%LocalRuleBaseDescription)
       #IF((%LocalRuleGlobalControlsAfterChanges AND (ITEMS(%LocalRuleGlobalHideUnHideControl) OR ITEMS(%LocalRuleGlobalEnableDisableControl))) OR (ITEMS(%LocalRuleHideUnHideControl) OR ITEMS(%LocalRuleEnableDisableControl)))
           #SET(%AUXLRulesHasGlobalControls,1)
       #ELSE
           #SET(%AUXLRulesHasGlobalControls,0)
       #ENDIF
  #ENDFOR
#!
#GROUP(%BusinessRulesLocalAtStartOfModule)
#ADD(%ModuleIncludeList,'ABRULE.INC')
#!
#GROUP(%BusinessRulesLocalAtCustomDec)
  #IF(%GlobalErrorIndicatorImage)
      #PROJECT(%GlobalErrorIndicatorImage)
  #ENDIF
  #IF(%GlobalViewRulesHeaderIcon)
      #PROJECT(%GlobalViewRulesHeaderIcon)
  #ENDIF
  #IF(%GlobalViewRulesValidRuleIcon)
      #PROJECT(%GlobalViewRulesValidRuleIcon)
  #ENDIF
  #IF(%GlobalViewRulesBrokenRuleIcon)
      #PROJECT(%GlobalViewRulesBrokenRuleIcon)
  #ENDIF
#!
#GROUP(%BusinessRulesLocalClassData)
#IF(ITEMS(%AUXLRules))
BusinessRulesManager  RulesManager
  #FOR(%AUXLRules)
%AUXLRules RulesCollection   !Business Rule Manager for %RuleBaseDescription
  #ENDFOR
#ENDIF
#!
#!
#GROUP(%BusinessRulesLocalInit)
#IF(ITEMS(%AUXLRules))
BIND('Action',ThisWindow.Request)
  #FOR(%AUXLRules)
    #IF(%GlobalErrorIndicatorImage)
%AUXLRules.SetErrorImage('~%GlobalErrorIndicatorImage')
    #ENDIF
%AUXLRules.SetDescription('%AUXLRulesDescription')
  #ENDFOR
  #DECLARE(%QuotedRuleDefinition)
  #DECLARE(%AUXShowIndicatorControl)
  #FREE(%AUXATRules)
  #!
  #! Local Rule Manager
  #!
  #FOR(%RuleBases)
    #!
    #! Local Rules
    #!
    #FOR(%Rules)
      #SET(%QuotedRuleDefinition,QUOTE(%RuleDefinition))
      #IF(%ShowIndicator)
%RuleBaseName.AddRule('%RuleName','%RuleDescription','%QuotedRuleDefinition',%RuleControl,%OffsetRight)  #<!Defining a rule in RulesManager
      #ELSE
%RuleBaseName.AddRule('%RuleName','%RuleDescription','%QuotedRuleDefinition')                            #<!Defining a rule in RulesManager
      #ENDIF
      #! If the control was added to all the rules will not be added here
      #FOR(%RuleEnableDisableControl),WHERE(NOT %SVInSymbol(%RuleEnableDisableControl,%LocalAllRuleEnableDisableControl))
         #! add it if is not on Global All Controls
         #IF(%LocalRuleAllGlobalControlsCheck)
            #IF(%SVInSymbol(%RuleEnableDisableControl,%GlobalAllRuleEnableDisableControl))
                #CYCLE
            #ENDIF
         #ENDIF
%RuleBaseName.AddControlToRule('%RuleName',%RuleEnableDisableControl,RuleAction:Disable)
      #ENDFOR
      #! If the control was added to all the rules will not be added here
      #FOR(%RuleHideUnHideControl),WHERE(NOT %SVInSymbol(%RuleHideUnHideControl,%LocalAllRuleHideUnHideControl))
         #! add it if is not on Global All Controls
         #IF(%LocalRuleAllGlobalControlsCheck)
            #IF(%SVInSymbol(%RuleHideUnHideControl,%GlobalAllRuleHideUnHideControl))
                #CYCLE
            #ENDIF
         #ENDIF
%RuleBaseName.AddControlToRule('%RuleName',%RuleHideUnHideControl,RuleAction:Hide)
      #ENDFOR
    #ENDFOR
  #ENDFOR
  #!
  #! Global rules populated on the procedure
  #!
  #FOR(%LocalRules),WHERE(NOT %LocalRuleDisable)
    #SET(%QuotedRuleDefinition,QUOTE(%LocalRuleExpression))
    #SET(%AUXShowIndicatorControl,'')
    #IF((%LocalRuleShowIndicatorOverride AND %LocalRuleShowIndicatorOverrided) OR (NOT %LocalRuleShowIndicatorOverride AND %LocalRuleShowIndicator))
        #IF(NOT %LocalRuleShowIndicatorDefault)
           #SET(%AUXShowIndicatorControl,%LocalRuleControl)
        #ENDIF
        #IF(%LocalRuleShowIndicatorDefault)
           #SET(%AUXShowIndicatorControl,%LocalRulesGetControl(%LocalRuleLinkField))
        #ENDIF
    #ELSE
        #SET(%AUXShowIndicatorControl,'')
    #ENDIF
    #IF(%AUXShowIndicatorControl)
%LocalRuleBaseName.AddRule('%LocalRuleName','%LocalRuleDescription','%QuotedRuleDefinition',%AUXShowIndicatorControl,%LocalRuleOffSetRight)  #<!Defining a rule in RulesManager
    #ELSE
%LocalRuleBaseName.AddRule('%LocalRuleName','%LocalRuleDescription','%QuotedRuleDefinition')                            #<!Defining a rule in RulesManager
    #ENDIF
    #!
    #! Local controls for this rule (only if are not in the global control for this rule
    #!
    #FOR(%LocalRuleEnableDisableControl),WHERE(NOT %SVInSymbol(%LocalRuleEnableDisableControl,%LocalRuleGlobalEnableDisableControl))
         #! add it if is not on Global All Controls
         #IF(%LocalRuleAllGlobalControlsCheck)
            #IF(%SVInSymbol(%LocalRuleEnableDisableControl,%GlobalAllRuleEnableDisableControl))
                #CYCLE
            #ENDIF
         #ENDIF
         #! add it if is not on All Controls for the procedure
         #IF(%SVInSymbol(%LocalRuleEnableDisableControl,%LocalAllRuleEnableDisableControl))
            #CYCLE
         #ENDIF
%LocalRuleBaseName.AddControlToRule('%LocalRuleName',%LocalRuleEnableDisableControl,RuleAction:Disable)
    #ENDFOR
    #FOR(%LocalRuleHideUnHideControl),WHERE(NOT %SVInSymbol(%LocalRuleHideUnHideControl,%LocalRuleGlobalHideUnHideControl))
         #! add it if is not on Global All Controls
         #IF(%LocalRuleAllGlobalControlsCheck)
            #IF(%SVInSymbol(%LocalRuleHideUnHideControl,%GlobalAllRuleHideUnHideControl))
                #CYCLE
            #ENDIF
         #ENDIF
         #! add it if is not on All Controls for the procedure
         #IF(%SVInSymbol(%LocalRuleHideUnHideControl,%LocalAllRuleHideUnHideControl))
            #CYCLE
         #ENDIF
%LocalRuleBaseName.AddControlToRule('%LocalRuleName',%LocalRuleHideUnHideControl,RuleAction:Hide)
    #ENDFOR
    #!
    #! Global controls for this rule (only if are present on the window)
    #!
    #FOR(%LocalRuleGlobalEnableDisableControl),WHERE(%SVInSymbol(%LocalRuleGlobalEnableDisableControl,%Control))
         #! add it if is not on Global All Controls
         #IF(%LocalRuleAllGlobalControlsCheck)
            #IF(%SVInSymbol(%LocalRuleGlobalEnableDisableControl,%GlobalAllRuleEnableDisableControl))
                #CYCLE
            #ENDIF
         #ENDIF
         #! add it if is not on All Controls for the procedure
         #IF(%SVInSymbol(%LocalRuleGlobalEnableDisableControl,%LocalAllRuleEnableDisableControl))
            #CYCLE
         #ENDIF
%LocalRuleBaseName.AddControlToRule('%LocalRuleName',%LocalRuleGlobalEnableDisableControl,RuleAction:Disable)
    #ENDFOR
    #FOR(%LocalRuleGlobalHideUnHideControl),WHERE(%SVInSymbol(%LocalRuleGlobalHideUnHideControl,%Control))
         #! add it if is not on Global All Controls
         #IF(%LocalRuleAllGlobalControlsCheck)
            #IF(%SVInSymbol(%LocalRuleGlobalHideUnHideControl,%GlobalAllRuleHideUnHideControl))
                #CYCLE
            #ENDIF
         #ENDIF
         #! add it if is not on All Controls for the procedure
         #IF(%SVInSymbol(%LocalRuleGlobalHideUnHideControl,%LocalAllRuleHideUnHideControl))
            #CYCLE
         #ENDIF
%LocalRuleBaseName.AddControlToRule('%LocalRuleName',%LocalRuleGlobalHideUnHideControl,RuleAction:Hide)
    #ENDFOR
  #ENDFOR
  #!
  #! Add each local rule manager to the RulesManager on the procedure
  #!
  #FOR(%RuleBases)
    #FOR(%Rules)
    #IF(NOT INLIST(%RuleBaseName,%AUXATRules))
        #ADD(%AUXATRules,%RuleBaseName)
%RuleBaseName.SetEnumerateIcons('~%GlobalViewRulesHeaderIcon','~%GlobalViewRulesValidRuleIcon','~%GlobalViewRulesBrokenRuleIcon')
BusinessRulesManager.AddRulesCollection(%RuleBaseName)
    #ENDIF
    #ENDFOR
  #ENDFOR
  #!
  #! Add each Global rule manager whose rules were populated on this procedure
  #! to the RulesManager verifying that that rule manager was not defined locally yet
  #!
  #FOR(%LocalRules),WHERE(NOT %LocalRuleDisable)
    #IF(NOT INLIST(%LocalRuleBaseName,%AUXATRules))
        #ADD(%AUXATRules,%LocalRuleBaseName)
%LocalRuleBaseName.SetEnumerateIcons('~%GlobalViewRulesHeaderIcon','~%GlobalViewRulesValidRuleIcon','~%GlobalViewRulesBrokenRuleIcon')
BusinessRulesManager.AddRulesCollection(%LocalRuleBaseName)
    #ENDIF
  #ENDFOR
BusinessRulesManager.SetEnumerateIcons('~%GlobalViewRulesHeaderIcon','~%GlobalViewRulesValidRuleIcon','~%GlobalViewRulesBrokenRuleIcon')
  #!
  #! Add all global controls only if are present on the window
  #! and will not in the Local List
  #!
  #IF(%LocalRuleAllGlobalControlsCheck)
     #FOR(%GlobalAllRuleEnableDisableControl),WHERE(%SVInSymbol(%GlobalAllRuleEnableDisableControl,%Control))
         #IF(NOT %SVInSymbol(%GlobalAllRuleEnableDisableControl,%LocalAllRuleEnableDisableControl))
BusinessRulesManager.AddControl(%GlobalAllRuleEnableDisableControl,RuleAction:Disable)
         #ENDIF
     #ENDFOR
     #FOR(%GlobalAllRuleHideUnHideControl),WHERE(%SVInSymbol(%GlobalAllRuleHideUnHideControl,%Control))
         #IF(NOT %SVInSymbol(%GlobalAllRuleHideUnHideControl,%LocalAllRuleHideUnHideControl))
BusinessRulesManager.AddControl(%GlobalAllRuleHideUnHideControl,RuleAction:Hide)
         #ENDIF
     #ENDFOR
  #ENDIF
  #!
  #! Add all local controls only if were not added by the global list
  #!
  #FOR(%LocalAllRuleEnableDisableControl)
BusinessRulesManager.AddControl(%LocalAllRuleEnableDisableControl,RuleAction:Disable)
  #ENDFOR
  #FOR(%LocalAllRuleHideUnHideControl)
BusinessRulesManager.AddControl(%LocalAllRuleHideUnHideControl,RuleAction:Hide)
  #ENDFOR
#ENDIF
#IF(ITEMS(%AUXLRules))
BusinessRulesManager.SetGlobalRuleReferences(GlobalRule)
#ENDIF
#!
#!
#GROUP(%BusinessRulesLocalCheckAfterOpen)
#IF(%LocalRuleCheckAfterOpenWindow)
  #IF(ITEMS(%AUXLRules))
BusinessRulesManager.CheckAllRules(1)                    #<!Checking all Rules in RulesManager
  #ENDIF
#ENDIF
#!
#GROUP(%BusinessRulesLocalAfterFieldsChange)
#IF(%LocalRuleCheckAfterFieldsChange)
  #IF(ITEMS(%AUXLRules))
  IF NOT 0{PROP:AcceptAll}
     IF ACCEPTED()
        UPDATE
        BusinessRulesManager.CheckAllRules(True)                 #<!Checking all Rules in RulesManager
        DISPLAY
     END
  END
  #ENDIF
#ENDIF
#GROUP(%BusinessRulesLocalAfterTakeNewSelection)
#IF(%LocalRuleCheckAfterFieldsChange)
  #IF(ITEMS(%AUXLRules))
  IF FIELD()
     UPDATE
     BusinessRulesManager.CheckAllRules(True)                 #<!Checking all Rules in RulesManager
     DISPLAY
  END
  #ENDIF
#ENDIF
#!
#GROUP(%BusinessRulesLocalTakeAccepted)
#IF(ITEMS(%AUXLRules))
BusinessRulesManager.TakeAccepted(Accepted())                                                      #<!RulesManager traps to determine if error-indicator was clicked
#ENDIF
#!
#GROUP(%GlobalBusinessRulesManagerPrompts)
  #BOXED('All Rules')
    #PROMPT('Default Description:',@s255),%GlobalAllRuleBaseDescription,REQ,DEFAULT('Procedure Rules')
    #PROMPT('Error-indicator Image',ICON('Select error-indicator image file','Icons (*.ico)|*.ICO|Windows Bitmaps (*.bmp)|*.BMP|GIF files|*.gif|JPEG files (*.jpg)|*.JPG|Windows Metafiles (*.wmf)|*:WMF|PCX files|*.PCX|All files|*.*')),%GlobalErrorIndicatorImage,DEFAULT('BRuleNo.ico')
    #PROMPT('Default Distance from Right:',SPIN(@n3,0,999)),%GlobalRuleOffSetRight,REQ,DEFAULT(3)
  #ENDBOXED
  #BUTTON('Global Rules'),MULTI(%GlobalRuleBases,%GlobalRuleBaseName & ': '& %GlobalRuleBaseDescription),AT(,,180),HLP('~TPLExtensionGlobalBusinessRulesBases')
    #PROMPT('Name:',@s255),%GlobalRuleBaseName,REQ
    #PROMPT('Description:',@s255),%GlobalRuleBaseDescription,REQ
    #BUTTON('Rules - '&  %GlobalRuleBaseName ),MULTI(%GlobalRules,%GlobalRuleName & ': '& %GlobalRuleDescription), AT(10),INLINE,HLP('~TPLExtensionGlobalBusinessRule')
      #PROMPT('Rule Name:',@s255),%GlobalRuleName,REQ
      #PROMPT('Rule Description:',@s255),%GlobalRuleDescription
      #BOXED('Rule Definition')
        #PROMPT('Evaluation Expression:',EDIT),%GlobalRuleExpression,REQ,WHENACCEPTED(%SVGlobalExpresionEditor(%GlobalRuleExpression))
        #DISPLAY('If this expression evaluates to zero, the Rule is broken')
      #ENDBOXED
      #PROMPT('Field to link to:',EDIT(256)),%GlobalRuleLinkField,REQ,WHENACCEPTED(%SVGlobalFieldSelection(%GlobalRuleLinkField))
      #DISPLAY('This Rule will be added to each Procedure')
      #DISPLAY('where the field is populated as a Control or')
      #DISPLAY('has been added into the Rules Hot Fields list.')
      #PROMPT('Display an error indicator when Rule is broken',Check),%GlobalRuleShowIndicator,AT(10),default(1)
      #DISPLAY
      #BUTTON('Controls'),AT(,,180),HLP('~TPLExtensionGlobalBusinessRuleControls')
        #BOXED
          #SHEET
            #TAB('Enable/Disable')
              #BOXED('Enable/Disable Controls')
                #DISPLAY('When the Rule is broken these Controls will be Disabled')
                #DISPLAY('and then Enabled when the Rule is validated.')
                #DISPLAY
                #!BUTTON('Enable/Disable Controls'),AT(,,180)
                  #PROMPT('Enable/Disable Control:',@S255),%GlobalRuleEnableDisableControl,MULTI('Enable/Disable Control'),INLINE
                  #BUTTON('Add Ok and Cancel Controls'),AT(,,165),WHENACCEPTED(%GlobalRulesAddOkControls(%GlobalRuleEnableDisableControl))
                  #ENDBUTTON
                #!ENDBUTTON
              #ENDBOXED
              #!DISPLAY
            #ENDTAB
            #TAB('Hide/UnHide')
              #BOXED('Hide/UnHide Controls')
                #DISPLAY('When the Rule is broken these controls will be Hidden')
                #DISPLAY('and then Unhidden when the Rule is validated.')
                #DISPLAY
                #!BUTTON('Hide/UnHide Controls'),AT(,,180)
                  #PROMPT('Hide/UnHide Control:',@S255),%GlobalRuleHideUnHideControl,MULTI('Hide/UnHide Control'),INLINE
                  #BUTTON('Add Ok and Cancel Controls'),AT(,,165),WHENACCEPTED(%GlobalRulesAddOkControls(%GlobalRuleHideUnHideControl))
                  #ENDBUTTON
                #!ENDBUTTON
              #ENDBOXED
            #ENDTAB
          #ENDSHEET
        #ENDBOXED
        #DISPLAY('These Controls will be used only if they')
        #DISPLAY('are populated on the Window.')
      #ENDBUTTON
    #ENDBUTTON
    #BUTTON('Add Standard Procedure Calls'),AT(,,174),HLP('~TPLExtensionGlobalBusinessRuleAddStandard')
      #DISPLAY('This will add a rule for every field in the')
      #DISPLAY('file specified below. The expression which defines')
      #DISPLAY('the rule will be')
      #DISPLAY(' ')
      #DISPLAY('             MyProc(''FieldName'',Action,1)')
      #DISPLAY(' ')
      #DISPLAY('where MyProc is the procedure name specified below.')
      #DISPLAY(' ')
      #PROMPT('File Name:',FILE),%BRMStdFileName
      #PROMPT('Procedure Name:',@S64),%BRMStdProcName
      #BUTTON('Do it!'),AT(,,60),WHENACCEPTED(CALL(%AddValidateProcedureCalls))
      #ENDBUTTON
    #ENDBUTTON
  #ENDBUTTON
  #BOXED('View Rules Icons')
    #PROMPT('Header Icon:',ICON('Select Header Icon File','Icons (*.ICO)|*.ICO|Windows Bitmaps (*.bmp)|*.BMP|GIF files|*.gif|JPEG files (*.jpg)|*.JPG|Windows Metafiles (*.wmf)|*:WMF|PCX files|*.PCX|All files|*.*')),%GlobalViewRulesHeaderIcon,REQ,DEFAULT('BRules.ico')
    #PROMPT('Valid Rule Icon:',ICON('Select an Icon for a Valid Rule','Icons (*.ICO)|*.ICO|Windows Bitmaps (*.bmp)|*.BMP|GIF files|*.gif|JPEG files (*.jpg)|*.JPG|Windows Metafiles (*.wmf)|*:WMF|PCX files|*.PCX|All files|*.*')),%GlobalViewRulesValidRuleIcon,REQ,DEFAULT('BRuleOk.ico')
    #PROMPT('Broken Rule Icon:',ICON('Select an Icon for a Broken Rule','Icons (*.ICO)|*.ICO|Windows Bitmaps (*.bmp)|*.BMP|GIF files|*.gif|JPEG files (*.jpg)|*.JPG|Windows Metafiles (*.wmf)|*:WMF|PCX files|*.PCX|All files|*.*')),%GlobalViewRulesBrokenRuleIcon,REQ,DEFAULT('BRuleNo.ico')
  #ENDBOXED
  #BOXED('When any Rule is broken')
    #DISPLAY('This is applicable only if the controls are')
    #DISPLAY('populated on the Window.')
    #BUTTON('All Rules - Other Controls'),AT(,,168),HLP('~TPLExtensionGlobalBusinessRuleControls')
      #BOXED
        #SHEET
          #TAB('Enable/Disable')
            #BOXED('Enable/Disable Controls')
              #DISPLAY('When any Rule is broken these controls will be Disabled')
              #DISPLAY('and will be Enabled when all the rules are validated.')
              #DISPLAY
              #PROMPT('Enable/Disable Control:',@S255),%GlobalAllRuleEnableDisableControl,MULTI('Enable/Disable Control'),INLINE
              #BUTTON('Add Ok and Cancel Controls'),AT(,,164),WHENACCEPTED(%GlobalRulesAddOkControls(%GlobalAllRuleEnableDisableControl))
              #ENDBUTTON
            #ENDBOXED
            #!DISPLAY
          #ENDTAB
          #TAB('Hide/UnHide')
            #BOXED('Hide/UnHide Controls')
              #DISPLAY('When any Rule is broken these controls will be Hidden')
              #DISPLAY('and will be Unhidden when all the Rules are validated.')
              #DISPLAY
              #PROMPT('Hide/UnHide Control:',@S255),%GlobalAllRuleHideUnHideControl,MULTI('Hide/UnHide Control'),INLINE
              #BUTTON('Add Ok and Cancel Controls'),AT(,,164),WHENACCEPTED(%GlobalRulesAddOkControls(%GlobalAllRuleHideUnHideControl))
              #ENDBUTTON
            #ENDBOXED
          #ENDTAB
        #ENDSHEET
      #ENDBOXED
      #DISPLAY('This is applicable only if the control is')
      #DISPLAY('populated on the Window.')
    #ENDBUTTON
  #ENDBOXED
#!
#GROUP(%AddValidateProcedureCalls)
#DECLARE(%Found,LONG)
#DECLARE(%NewRule,LONG)
#FIX(%File,%BRMStdFileName)
#FOR(%Field)
  #IF(NOT %FieldID)
    #CYCLE
  #ENDIF
  #SET(%Found,%False)
  #FOR(%GlobalRules)
    #IF(UPPER(%GlobalRuleLinkField) = UPPER(%Field))
      #SET(%Found,%True)
      #BREAK
    #ENDIF
  #ENDFOR
  #IF(NOT %Found)
    #SET(%NewRule,ITEMS(%GlobalRules)+1)
    #ADD(%GlobalRules,%NewRule)
    #SET(%GlobalRuleName,%Field&'Check')
    #SET(%GlobalRuleDescription,%Field&' OK')
    #SET(%GlobalRuleExpression,%BRMStdProcName&'('''&%Field&''',Action,1)')
    #SET(%GlobalRuleLinkField,%Field)
    #SET(%GlobalRuleShowIndicator,1)
  #ENDIF
#ENDFOR
#!
#GROUP(%LocalBusinessRulesManagerPrompts)
  #BOXED,AT(0,0,0,0),HIDE
    #BUTTON('Rules - '&  %LocalRuleBaseName ),MULTI(%LocalRules,%LocalRuleBaseName&'-'&%LocalRuleName & ': '& %LocalRuleDescription), AT(10)
      #PROMPT('Name:',@S255),%LocalRuleBaseName,REQ
      #PROMPT('Description:',@s255),%LocalRuleBaseDescription,REQ
      #PROMPT('Rule Name:',@s255),%LocalRuleName,REQ
      #PROMPT('Rule Description:',@s255),%LocalRuleDescription
      #PROMPT('Evaluation Expression:',@s255),%LocalRuleExpression
      #PROMPT('Field to link to:',@s255),%LocalRuleLinkField,REQ
      #PROMPT('Display an error indicator when a Rule is broken',Check),%LocalRuleShowIndicator,AT(10),default(1)
      #PROMPT('Enable/Disable Control:',@s255),%LocalRuleGlobalEnableDisableControl,MULTI('Enable/Disable Control'),INLINE
      #PROMPT('Hide/UnHide Control:',@s255),%LocalRuleGlobalHideUnHideControl,MULTI('Hide/UnHide Control'),INLINE
    #ENDBUTTON
  #ENDBOXED
  #BOXED,AT(0,0)
    #PROMPT('All Rules Description:',@s255),%LocalAllRuleBaseDescription,REQ,DEFAULT(%GlobalAllRuleBaseDescription)
    #PROMPT('Check Rules After Open Window',CHECK),%LocalRuleCheckAfterOpenWindow,AT(10),DEFAULT(1)
    #PROMPT('Check Rules After Fields Change',CHECK),%LocalRuleCheckAfterFieldsChange,AT(10),DEFAULT(1)
    #PROMPT('Check Rules Before Updating Record',CHECK),%LocalRuleCheckBeforeUpdate,DEFAULT(1),AT(10)
    #PROMPT('Check for Global Rules Controls',CHECK),%LocalRuleGlobalControlsAfterChanges,AT(10),DEFAULT(1)
    #DISPLAY
    #BUTTON('Hot Fields'),MULTI(%HotFields,%HotField),AT(,,180),HLP('~TPLExtensionLocalBusinessRulesHotFields')
      #PROMPT('Hot Field:',FIELD),%HotField,REQ
    #ENDBUTTON
    #PREPARE
      #CALL(%LocalRulesLoad)
    #ENDPREPARE
    #BUTTON('Used Global Rules'),FROM(%LocalRules,CHOOSE(%LocalRuleDisable=1,'    - ','X - ')&%LocalRuleBaseName&'-'&%LocalRuleName & ': '& %LocalRuleDescription),AT(,,180),HLP('~TPLExtensionLocalBusinessRulesUsedGlobal')
      #PROMPT('Disable this Rule',CHECK),%LocalRuleDisable,DEFAULT(0),AT(10)
      #DISPLAY('Evaluation Expression')
      #DISPLAY(%LocalRuleExpression),AT(,,180,40),PROP(PROP:FontColor, 0000000H),PROP(PROP:Color,0FFFFFFH),PROP(PROP:FontStyle,700)
      #ENABLE(NOT %LocalRuleDisable)
        #PROMPT('Override Display indicator',CHECK),%LocalRuleShowIndicatorOverride,AT(10),DEFAULT(0)
        #ENABLE(%LocalRuleShowIndicatorOverride)
          #PROMPT('Display an error indicator when a Rule is broken',Check),%LocalRuleShowIndicatorOverrided,AT(10),DEFAULT(1)
        #ENDENABLE
        #ENABLE((%LocalRuleShowIndicatorOverride AND %LocalRuleShowIndicatorOverrided) OR (NOT %LocalRuleShowIndicatorOverride AND %LocalRuleShowIndicator))
          #BOXED('Error indicator settings')
            #PROMPT('Use default control if found',CHECK),%LocalRuleShowIndicatorDefault,AT(10),DEFAULT(1)
            #BOXED,WHERE(%LocalRuleShowIndicatorDefault)
              #DISPLAY('Field to Link:'&%LocalRuleLinkField)
            #ENDBOXED
            #BOXED,SECTION
              #BOXED,AT(0,0),WHERE(NOT %LocalRuleShowIndicatorDefault)
                #PROMPT('Display Next to Control:',CONTROL),%LocalRuleControl,REQ
              #ENDBOXED
              #BOXED,AT(0,1),WHERE(%LocalRuleShowIndicatorDefault AND %LocalRulesGetControl(%LocalRuleLinkField))
                #DISPLAY('Display Next to Control: '&%LocalRulesGetControl(%LocalRuleLinkField))
              #ENDBOXED
              #BOXED,WHERE(NOT %LocalRuleShowIndicatorDefault OR %LocalRulesGetControl(%LocalRuleLinkField)<>'')
                #PROMPT('Distance from Right:',SPIN(@n3,0,999)),%LocalRuleOffSetRight,REQ,DEFAULT(%GlobalRuleOffSetRight)
              #ENDBOXED
              #BOXED,AT(0,1),WHERE(%LocalRuleShowIndicatorDefault AND %LocalRulesGetControl(%LocalRuleLinkField)='')
                #DISPLAY('There are no associated control for')
                #DISPLAY(%LocalRuleLinkField)
              #ENDBOXED
            #ENDBOXED
          #ENDBOXED
        #ENDENABLE
        #DISPLAY
        #BUTTON('Controls'),AT(,,180),HLP('~TPLExtensionLocalBusinessRulesUsedControls')
          #BOXED
            #SHEET
              #TAB('Enable/Disable')
                #BOXED('Enable/Disable Controls')
                  #DISPLAY('When the Rule is broken these controls will be Disabled')
                  #DISPLAY('and then Enabled when the Rule is validated.')
                  #DISPLAY
                  #PROMPT('Enable/Disable Control:',FROM(%Control,NOT INLIST(%Control,%LocalRuleEnableDisableControl))),%LocalRuleEnableDisableControl,MULTI('Enable/Disable Control'),INLINE
                  #BUTTON('Add Ok and Cancel Controls'),AT(,,180),WHENACCEPTED(%GlobalRulesAddOkControls(%LocalRuleEnableDisableControl,1))
                  #ENDBUTTON
                #ENDBOXED
              #ENDTAB
              #TAB('Hide/UnHide')
                #BOXED('Hide/UnHide Controls')
                  #DISPLAY('When the Rule is broken these controls will be Hidden')
                  #DISPLAY('and then Unhidden when the Rule is validated.')
                  #DISPLAY
                  #PROMPT('Hide/UnHide Control:',FROM(%Control,NOT INLIST(%Control,%LocalRuleHideUnHideControl))),%LocalRuleHideUnHideControl,MULTI('Hide/UnHide Control'),INLINE
                  #BUTTON('Add Ok and Cancel Controls'),AT(,,180),WHENACCEPTED(%GlobalRulesAddOkControls(%LocalRuleHideUnHideControl,1))
                  #ENDBUTTON
                #ENDBOXED
              #ENDTAB
            #ENDSHEET
          #ENDBOXED
        #ENDBUTTON
      #ENDENABLE
    #ENDBUTTON
    #BUTTON('Local Rules'),MULTI(%RuleBases,%RuleBaseName & ': '& %RuleBaseDescription),AT(,,180),HLP('~TPLExtensionLocalBusinessRuleBases')
      #PROMPT('Name:',@s255),%RuleBaseName,REQ
      #PROMPT('Description:',@s255),%RuleBaseDescription,REQ
      #BUTTON('Rules - '&  %RuleBaseName ),MULTI(%Rules,%RuleName & ': '& %RuleDescription), AT(10),INLINE,HLP('~TPLExtensionLocalBusinessRule')
        #PROMPT('Rule Name:',@s255),%RuleName,REQ
        #PROMPT('Rule Description:',@s255),%RuleDescription
        #BOXED('Rule Definition')
          #PROMPT('Evaluation Expression:',EDIT),%RuleDefinition,REQ,WHENACCEPTED(%SVExpresionEditor(%RuleDefinition))
          #DISPLAY('If this expression evaluates to zero, the Rule is broken')
        #ENDBOXED
        #PROMPT('Display an error indicator when a Rule is broken',Check),%ShowIndicator,AT(10),default(1)
        #ENABLE(%ShowIndicator)
        #BOXED('Error indicator settings')
          #PROMPT('Display to RHS of:',CONTROL),%RuleControl,REQ
          #PROMPT('Distance from RHS:',SPIN(@n3,0,999)),%OffSetRight,REQ,default(3)
        #ENDBOXED
        #ENDENABLE
        #BUTTON('Controls'),AT(,,180),HLP('~TPLExtensionLocalBusinessRuleControls')
          #BOXED
            #SHEET
              #TAB('Enable/Disable')
                #BOXED('Enable/Disable Controls')
                  #DISPLAY('When the Rule is broken these controls will be Disabled')
                  #DISPLAY('and then Enabled when the Rule is validated.')
                  #DISPLAY
                  #PROMPT('Enable/Disable Control:',FROM(%Control,NOT INLIST(%Control,%RuleEnableDisableControl))),%RuleEnableDisableControl,MULTI('Enable/Disable Control'),INLINE
                  #BUTTON('Add Ok and Cancel Controls'),AT(,,180),WHENACCEPTED(%GlobalRulesAddOkControls(%RuleEnableDisableControl,1))
                  #ENDBUTTON
                #ENDBOXED
              #ENDTAB
              #TAB('Hide/UnHide')
                #BOXED('Hide/UnHide Controls')
                  #DISPLAY('When the Rule is broken these controls will be Hidden')
                  #DISPLAY('and then Unhidden when the Rule is validated.')
                  #DISPLAY
                  #PROMPT('Hide/UnHide Control:',FROM(%Control,NOT INLIST(%Control,%RuleHideUnHideControl))),%RuleHideUnHideControl,MULTI('Hide/UnHide Control'),INLINE
                  #BUTTON('Add Ok and Cancel Controls'),AT(,,180),WHENACCEPTED(%GlobalRulesAddOkControls(%RuleHideUnHideControl,1))
                  #ENDBUTTON
                #ENDBOXED
              #ENDTAB
            #ENDSHEET
          #ENDBOXED
        #ENDBUTTON
      #ENDBUTTON
    #ENDBUTTON
    #DISPLAY
    #PROMPT('Check for Global "All Rules - Other Controls"',CHECK),%LocalRuleAllGlobalControlsCheck,AT(10),DEFAULT(1)
    #DISPLAY('"All Rules - Other Controls" is applicable only')
    #DISPLAY('if the control is populated on the Window.')
    #DISPLAY
    #BUTTON('All Rules - Other Controls'),AT(,,160),HLP('~TPLExtensionGlobalBusinessRuleControls')
      #BOXED
        #SHEET
          #TAB('Enable/Disable')
            #BOXED('Enable/Disable Controls')
              #DISPLAY('When any Rule is broken these controls will be Disabled')
              #DISPLAY('and will be Enabled when all the Rules are validated.')
              #DISPLAY
              #PROMPT('Enable/Disable Control:',FROM(%Control,NOT INLIST(%Control,%LocalAllRuleEnableDisableControl))),%LocalAllRuleEnableDisableControl,MULTI('Enable/Disable Control'),INLINE
              #BUTTON('Add Ok and Cancel Controls'),AT(,,180),WHENACCEPTED(%GlobalRulesAddOkControls(%LocalAllRuleEnableDisableControl))
              #ENDBUTTON
            #ENDBOXED
            #!DISPLAY
          #ENDTAB
          #TAB('Hide/UnHide')
            #BOXED('Hide/UnHide Controls')
              #DISPLAY('When any Rule is broken these controls will be Hidden')
              #DISPLAY('and will be Unhidden when all the Rules are validated.')
              #DISPLAY
              #PROMPT('Hide/UnHide Control:',FROM(%Control,NOT INLIST(%Control,%LocalAllRuleHideUnHideControl))),%LocalAllRuleHideUnHideControl,MULTI('Hide/UnHide Control'),INLINE
              #BUTTON('Add Ok and Cancel Controls'),AT(,,180),WHENACCEPTED(%GlobalRulesAddOkControls(%LocalAllRuleHideUnHideControl))
              #ENDBUTTON
            #ENDBOXED
          #ENDTAB
        #ENDSHEET
      #ENDBOXED
      #DISPLAY('This is applicable only if the control is')
      #DISPLAY('populated on the Window.')
    #ENDBUTTON
  #ENDBOXED
#!
#GROUP(%GlobalRulesExport,%GlobalRulesFileName)
  #CREATE(%GlobalRulesFileName)
RULESFILENAME                  %GlobalRulesFileName

ALLRULEBASEDESCRIPTION         %GlobalAllRuleBaseDescription
ERRORINDICATORIMAGE            %GlobalErrorIndicatorImage
RULEOFFSETRIGHT                %GlobalRuleOffSetRight

VIEWRULESHEADERICON            %GlobalViewRulesHeaderIcon
VIEWRULESVALIDRULEICON         %GlobalViewRulesValidRuleIcon
VIEWRULESBROKENRULEICON        %GlobalViewRulesBrokenRuleIcon

  #FOR(%GlobalAllRuleEnableDisableControl)
  ALLRULEENABLEDISABLECONTROL  %GlobalAllRuleEnableDisableControl
  #ENDFOR

  #FOR(%GlobalAllRuleHideUnHideControl)
  ALLRULEHIDEUNHIDECONTROL     %GlobalAllRuleHideUnHideControl
  #ENDFOR
  #FOR(%GlobalRuleBases)

  RULEBASES                    %GlobalRuleBases
  RULEBASENAME                 %GlobalRuleBaseName
  RULEBASEDESCRIPTION          %GlobalRuleBaseDescription
    #FOR(%GlobalRules)

    RULES                      %GlobalRules
    RULENAME                   %GlobalRuleName
    RULEDESCRIPTION            %GlobalRuleDescription
    RULEEXPRESSION             %GlobalRuleExpression
    RULELINKFIELD              %GlobalRuleLinkField
    RULESHOWINDICATOR          %GlobalRuleShowIndicator
      #FOR(%GlobalRuleEnableDisableControl)
      RULEENABLEDISABLECONTROL %GlobalRuleEnableDisableControl
      #ENDFOR
      #FOR(%GlobalRuleHideUnHideControl)
      RULEHIDEUNHIDECONTROL    %GlobalRuleHideUnHideControl
      #ENDFOR
    #ENDFOR
  #ENDFOR
  #CLOSE(%GlobalRulesFileName)
#!
#GROUP(%GlobalRulesImport,%GlobalRulesFileName)
  #IF(NOT %GlobalRulesImported)
    #!ASSERT(%False,'Importing global rules')
    #SET(%GlobalRulesImported,%True)
    #DECLARE(%GlobalRulesImportLine)
    #DECLARE(%GlobalRulesImportType)
    #DECLARE(%GlobalRulesImportValue)
    #DECLARE(%GlobalRulesIndex)
    #CLEAR(%GlobalAllRuleBaseDescription)
    #CLEAR(%GlobalErrorIndicatorImage)
    #CLEAR(%GlobalRuleOffSetRight)
    #CLEAR(%GlobalViewRulesHeaderIcon)
    #CLEAR(%GlobalViewRulesValidRuleIcon)
    #CLEAR(%GlobalViewRulesBrokenRuleIcon)
    #FREE(%GlobalAllRuleEnableDisableControl)
    #FREE(%GlobalAllRuleHideUnHideControl)
    #FREE(%GlobalRuleBases)
    #OPEN(%GlobalRulesFileName),READ
    #LOOP
      #READ(%GlobalRulesImportLine)
      #IF(%GlobalRulesImportLine = %EOF)
        #BREAK
      #ENDIF
      #CALL(%GlobalRulesParseImportLine)
      #CASE(%GlobalRulesImportType)
      #OF('ALLRULEBASEDESCRIPTION')
        #SET(%GlobalAllRuleBaseDescription,%GlobalRulesImportValue)
      #OF('ERRORINDICATORIMAGE')
        #SET(%GlobalErrorIndicatorImage,%GlobalRulesImportValue)
      #OF('RULEOFFSETRIGHT')
        #SET(%GlobalRuleOffSetRight,%GlobalRulesImportValue)
      #OF('VIEWRULESHEADERICON')
        #SET(%GlobalViewRulesHeaderIcon,%GlobalRulesImportValue)
      #OF('VIEWRULESVALIDRULEICON')
        #SET(%GlobalViewRulesValidRuleIcon,%GlobalRulesImportValue)
      #OF('VIEWRULESBROKENRULEICON')
        #SET(%GlobalViewRulesBrokenRuleIcon,%GlobalRulesImportValue)
      #OF('ALLRULEENABLEDISABLECONTROL')
        #ADD(%GlobalAllRuleEnableDisableControl,%GlobalRulesImportValue)
      #OF('ALLRULEHIDEUNHIDECONTROL')
        #ADD(%GlobalAllRuleHideUnHideControl,%GlobalRulesImportValue)
      #OF('RULEBASES')
        #ADD(%GlobalRuleBases,%GlobalRulesImportValue)
      #OF('RULEBASENAME')
        #SET(%GlobalRuleBaseName,%GlobalRulesImportValue)
      #OF('RULEBASEDESCRIPTION')
        #SET(%GlobalRuleBaseDescription,%GlobalRulesImportValue)
      #OF('RULES')
        #ADD(%GlobalRules,%GlobalRulesImportValue)
      #OF('RULENAME')
        #SET(%GlobalRuleName,%GlobalRulesImportValue)
      #OF('RULEDESCRIPTION')
        #SET(%GlobalRuleDescription,%GlobalRulesImportValue)
      #OF('RULEEXPRESSION')
        #SET(%GlobalRuleExpression,%GlobalRulesImportValue)
      #OF('RULELINKFIELD')
        #SET(%GlobalRuleLinkField,%GlobalRulesImportValue)
      #OF('RULESHOWINDICATOR')
        #SET(%GlobalRuleShowIndicator,%GlobalRulesImportValue)
      #OF('RULEENABLEDISABLECONTROL')
        #ADD(%GlobalRuleEnableDisableControl,%GlobalRulesImportValue)
      #OF('RULEHIDEUNHIDECONTROL')
        #ADD(%GlobalRuleHideUnHideControl,%GlobalRulesImportValue)
      #ENDCASE
    #ENDLOOP
    #CLOSE(%GlobalRulesFileName)
  #ENDIF
#!
#GROUP(%GlobalRulesParseImportLine)
  #SET(%GlobalRulesImportLine,LEFT(%GlobalRulesImportLine))
  #SET(%GlobalRulesIndex,INSTRING(' ',%GlobalRulesImportLine))
  #IF(%GlobalRulesIndex > 1)
    #SET(%GlobalRulesImportType,SLICE(%GlobalRulesImportLine,1,%GlobalRulesIndex-1))
  #ELSE
    #SET(%GlobalRulesImportType,' ')
  #ENDIF
  #SET(%GlobalRulesImportValue,SLICE(%GlobalRulesImportLine,%GlobalRulesIndex,LEN(%GlobalRulesImportLine)))
  #SET(%GlobalRulesImportValue,CLIP(LEFT(%GlobalRulesImportValue)))
#!
#GROUP(%GlobalRuleSetAttributes,%GlobalDataDefinition)
#DECLARE(%CodeLine)
#SET(%CodeLine,CLIP(%GlobalDataDefinition))
#CASE(%ProgramExtension)
#OF('DLL')
  #IF(%GlobalExternal)                              #!A DLL with global data external
    #SET(%CodeLine,%Codeline&',EXTERNAL,DLL(dll_mode)')
  #ENDIF
#OF('LIB')
  #IF(%GlobalExternal)                              #!A LIB with global data external
    #SET(%CodeLine,%Codeline)
  #ENDIF
#OF('EXE')
  #IF(%GlobalExternal)                              #!An EXE with global data external
    #SET(%CodeLine,%Codeline)
  #ENDIF
#ENDCASE
#RETURN(%CodeLine)
#!
