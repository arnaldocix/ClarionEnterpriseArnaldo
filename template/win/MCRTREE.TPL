#TEMPLATE(SVMultiChildRelationTree,'Multi Child Relation Tree Control - Softvelocity Addons'),FAMILY('CW20','ABC')
#!
#HELP('ClarionHelp.chm')
#!=============================================================================
#! CONTROL TEMPLATE
#!=============================================================================
#CONTROL(MultiChildRelationTree,'Multi Child Relation Tree Control'),PRIMARY(%TableSchematicsDescription&' - Multi Child Relation Tree ListBox'),DESCRIPTION('MultiReltree - Records related to ' & %Primary & ' (' & %TableSchematicsDescription &')'),WINDOW,MULTI,HLP('~IDH_MultiChildRelationTree'),WRAP(LIST)
  CONTROLS
    LIST,FROM(MCRT:Queue),AT(,,100,160),USE(?MCRTree),FORMAT('504L(1)|TS(200)@s128@'),VSCROLL,#REQ
  END
#PREPARE
  #DECLARE(%MCRTBuiltinIcons),UNIQUE
  #DECLARE(%MCRTFontStyles),UNIQUE
  #DECLARE(%MCRTFontCharsets),UNIQUE
  #CALL(%MakeFontStyleList)
  #CALL(%MakeFontCharsetList)
  #CALL(%MakeIconList)
#ENDPREPARE
#BUTTON('Multi Children Tree Behavior'),AT(10,,180)
  #SHEET
    #TAB('General'),HLP('~IDH_Setup0')
      #PREPARE
        #FIND(%ControlInstance,%ActiveTemplateInstance,%Control)
      #ENDPREPARE
      #BOXED('Table Schematic Description'),SECTION
        #PROMPT('',@S255),%TableSchematicsDescription,DEFAULT(''),AT(10,3,173)
      #ENDBOXED
      #BOXED('Tree')
        #PROMPT('Accept control from ToolBar',CHECK),%AcceptToolBarControl,DEFAULT(1),AT(10)
        #PROMPT('Active Invisible',CHECK),%ActiveInvisible,DEFAULT(%False),AT(10)
        #PROMPT('Enable Tree Root',CHECK),%EnableTreeRoot,DEFAULT(1),AT(10)
        #ENABLE(NOT %EnableTreeRoot),CLEAR
          #PROMPT('Disable Primary Table Level',CHECK),%DisablePrimaryLevel,DEFAULT(0),AT(10)
        #ENDENABLE
        #PROMPT('Tree Line &Height:',SPIN(@n2,1,20,1)),%MCRTRootLineHeight,DEFAULT(2)
        #PROMPT('&Expand Key:',KEYCODE),%MCRTRootExpandKey,DEFAULT('PlusKey')
        #PROMPT('&Contract Key:',KEYCODE),%MCRTRootContractKey,DEFAULT('MinusKey')
        #PROMPT('&Initial State:',DROP('Expanded|Contracted')),%MCRTRootInitialState,DEFAULT('Contracted')
        #BOXED('Popup')
          #ENABLE(%ControlHasColor OR %ControlHasStyle)
            #!BOXED,HIDE
              #PROMPT('Use &Setup Capability',CHECK),%MCRTRootUseSetupCapability,DEFAULT(%False),AT(10)
              #PROMPT('Setup Popup priority:',@n3),%MCRTSetupPopupPriority,DEFAULT(40)
            #!ENDBOXED
          #ENDENABLE
          #PROMPT('Expanding/Contracting via Popup',CHECK),%MCRTRootExpandContractViaPopup,DEFAULT(%True),AT(10)
          #ENABLE(%MCRTRootExpandContractViaPopup)
            #PROMPT('Use One Item for Both Actions',CHECK),%MCRTUseOneItem,DEFAULT(%False),AT(10)
            #PROMPT('Expand/Contract Popup priority:',@n3),%MCRTRootExpandContractPriority,DEFAULT(10)
          #ENDENABLE
          #PROMPT('Refresh via Popup',CHECK),%MCRTRefreshViaPopup,DEFAULT(%True),AT(10)
          #ENABLE(%MCRTRefreshViaPopup)
            #PROMPT('Refresh Popup priority:',@n3),%MCRTRefreshPriority,DEFAULT(30)
          #ENDENABLE
        #ENDBOXED
      #ENDBOXED
    #ENDTAB
    #TAB('Root'),WHERE(NOT %EnableTreeRoot)
      #DISPLAY
      #DISPLAY('Your MultiChildRelationTree control is not set')
      #DISPLAY('to use a common Root.')
    #ENDTAB
    #TAB('Root'),WHERE(%EnableTreeRoot),HLP('~IDH_Setup1')
      #PREPARE
        #FIND(%ControlInstance,%ActiveTemplateInstance,%Control)
      #ENDPREPARE
      #BOXED
        #SHEET
          #TAB('General'),HLP('~IDH_Setup1')
            #BOXED('Level')
              #PROMPT('Tree Root &Header:',OPTFIELD),%MCRTRootHeader
              #ENABLE(%ControlHasIcon)
                #PROMPT('Tree Root &Icon:',ICON('Select Root Icon','Icon files (*.ico)|*.ico')),%MCRTRootIcon
              #ENDENABLE
            #ENDBOXED
          #ENDTAB
          #TAB('Colors'),HLP('~IDH_Setup2')
            #BOXED('Colors'),SECTION
              #BOXED,WHERE(%ControlHasColor),AT(,0)
                #BOXED('Default Colors')
                  #PROMPT('Foreground Normal:',COLOR),%MCRTRootNormalFGColor,DEFAULT(-1)
                  #PROMPT('Background Normal:',COLOR),%MCRTRootNormalBGColor,DEFAULT(-1)
                  #PROMPT('Foreground Selected:',COLOR),%MCRTRootSelectedFGColor,DEFAULT(-1)
                  #PROMPT('Background Selected:',COLOR),%MCRTRootSelectedBGColor,DEFAULT(-1)
                #ENDBOXED
                #BOXED('Conditional Colors')
                  #BUTTON('Conditional Colors...'),MULTI(%MCRTRootConditionalColors,%MCRTRootColorCondition),INLINE,AT(,,,70)
                    #PROMPT('Condition:',EXPR),%MCRTRootColorCondition,REQ
                    #PROMPT('Foreground Normal:',COLOR),%MCRTRootNormalFGConditionalColor,DEFAULT(-1)
                    #PROMPT('Background Normal:',COLOR),%MCRTRootNormalBGConditionalColor,DEFAULT(-1)
                    #PROMPT('Foreground Selected:',COLOR),%MCRTRootSelectedFGConditionalColor,DEFAULT(-1)
                    #PROMPT('Background Selected:',COLOR),%MCRTRootSelectedBGConditionalColor,DEFAULT(-1)
                  #ENDBUTTON
                #ENDBOXED
              #ENDBOXED
              #BOXED,WHERE(~%ControlHasColor),AT(,0)
                #DISPLAY('Your MultiChildRelationTree control is not set to use custom colors. To activate custom colors for your MultiChildRelationTree control, follow these steps:'),AT(,,165,25)
                #DISPLAY('1.  If you aren''t in the window formatter, go there.'),AT(,,165)
                #DISPLAY('2.  Right-click on the MultiChildRelationTree control, and select "List Box Format..."'),AT(,,165,15)
                #DISPLAY('3.  Select the field you want to colorize, and select the "General" tab.'),AT(,,165,15)
                #DISPLAY('4.  Check the "Color" checkbox.'),AT(,,165)
                #DISPLAY('5.  Repeat steps 3 and 4 as necessary.'),AT(,,165)
                #DISPLAY('6.  When you call up the TreeControl actions window, this tab will let you set the colors.'),AT(,,165,20)
              #ENDBOXED
            #ENDBOXED
          #ENDTAB
          #TAB('Styles'),HLP('~IDH_Setup3')
            #BOXED('Styles'),SECTION
              #BOXED,WHERE(%ControlHasStyle),AT(,0)
                #BOXED('Default Style')
                  #PROMPT('Style Type:',DROP('Local List|Style Number')),%MCRTRootStyleType,DEFAULT('Local List')
                  #BOXED,SECTION
                    #BOXED,WHERE(%MCRTRootStyleType='Local List'),AT(0,0)
                      #PROMPT('Style:',FROM(%WindowStyles,,INSTANCE(%WindowStyles)&'-'&CHOOSE(%WindowStyleFontNameIsVariable,%WindowStyleFontNameVariable,%WindowStyleFontName)&','&CHOOSE(%WindowStyleFontSizeIsVariable,%WindowStyleFontSizeVariable,%WindowStyleFontSize))),%MCRTRootStyleLocalList
                    #ENDBOXED
                    #BOXED,WHERE(%MCRTRootStyleType='Style Number'),AT(0,0)
                      #PROMPT('Style Number:',SPIN(@n6,0,65535)),%MCRTRootStyle
                    #ENDBOXED
                  #ENDBOXED
                #ENDBOXED
                #BOXED('Conditional Styles')
                  #BUTTON('&Conditional Styles'),MULTI(%MCRTRootConditionalStyles,%MCRTRootStyleCondition),INLINE,HLP('~TPLControlBrowseBox_Styles')
                    #PROMPT('&Condition:',EXPR),%MCRTRootStyleCondition,REQ
                    #PROMPT('Style Type:',DROP('Local List|Style Number')),%MCRTRootConditionalStyleType,DEFAULT('Local List')
                    #BOXED,SECTION
                      #BOXED,WHERE(%MCRTRootConditionalStyleType='Local List'),AT(0,0)
                        #PROMPT('Style:',FROM(%WindowStyles,,INSTANCE(%WindowStyles)&'-'&CHOOSE(%WindowStyleFontNameIsVariable,%WindowStyleFontNameVariable,%WindowStyleFontName)&','&CHOOSE(%WindowStyleFontSizeIsVariable,%WindowStyleFontSizeVariable,%WindowStyleFontSize))),%MCRTRootConditionalStyleLocalList
                      #ENDBOXED
                      #BOXED,WHERE(%MCRTRootConditionalStyleType='Style Number'),AT(0,0)
                        #PROMPT('Style Number:',SPIN(@n6,0,65535)),%MCRTRootConditionalStyle
                      #ENDBOXED
                    #ENDBOXED
                  #ENDBUTTON
                #ENDBOXED
              #ENDBOXED
              #BOXED,WHERE(~%ControlHasStyle),AT(,0)
                #DISPLAY('Your MultiChildRelationTree control is not set to use custom styles. To activate custom styles for your MultiChildRelationTree control, follow these steps:'),AT(,,168,25)
                #DISPLAY('1.  If you aren''t in the window formatter, go there.'),AT(,,168)
                #DISPLAY('2.  Right-click on the MultiChildRelationTree control, and select "List Box Format..."'),AT(,,168,15)
                #DISPLAY('3.  Select the field you want to stylize, and select the "General" tab.'),AT(,,168,15)
                #DISPLAY('4.  Check the "Style" checkbox.'),AT(,,168)
                #DISPLAY('5.  Repeat steps 3 and 4 as necessary.'),AT(,,168)
                #DISPLAY('6.  When you call up the TreeControl actions window, this tab will let you set the styles.'),AT(,,168,20)
              #ENDBOXED
            #ENDBOXED
          #ENDTAB
        #ENDSHEET
      #ENDBOXED
    #ENDTAB
    #TAB('Primary Level'),WHERE(%DisablePrimaryLevel)
      #DISPLAY
      #DISPLAY('Your MultiChildRelationTree control is not set')
      #DISPLAY('to use a common Root using the Primary table.')
    #ENDTAB
    #TAB('Primary Level'),WHERE(NOT %DisablePrimaryLevel),HLP('~IDH_Primary')
      #PREPARE
        #FIND(%ControlInstance,%ActiveTemplateInstance,%Control)
      #ENDPREPARE
      #BOXED
        #SHEET
          #TAB('General'),HLP('~IDH_Primary')
            #DISPLAY('File: '&%Primary)
            #BOXED('Level')
              #PROMPT('Level &Title:',EXPR),%MCRTPrimaryLevelTitle
              #PROMPT('Field to &Display:',EXPR),%MCRTPrimaryDisplay
              #PROMPT('Advanced Display (Embed)',EMBEDBUTTON(%MCRTAfterSetQueueRecord,%ActiveTemplateInstance,%Primary)),AT(,,170)
              #PROMPT('Record &Filter:',EXPR),%MCRTPrimaryRecordFilter
              #ENABLE(%PrimaryKey)
                #PROMPT('Range Limit &Field:',COMPONENT(%PrimaryKey)),%RangeField,WHENACCEPTED(%LoadHigherKeysComponets(%Primary,%PrimaryKey,%RangeField,%RangeLimitType))
                #ENABLE(%RangeField)
                  #PREPARE
                    #CALL(%LoadHigherKeysComponets,%Primary,%PrimaryKey,%RangeField,%RangeLimitType)
                  #ENDPREPARE
                  #PROMPT('Range Limit &Type:',DROP('Current Value|Single Value|Range of Values|File Relationship')),%RangeLimitType,DEFAULT('Current Value'),WHENACCEPTED(%LoadHigherKeysComponets(%Primary,%PrimaryKey,%RangeField,%RangeLimitType))
                  #BOXED,HIDE
                    #BUTTON('HigherKeys'),MULTI(%HigherKeys,%HigherKey)
                      #PROMPT('Higher Field',COMPONENT(%PrimaryKey)),%HigherKey
                    #ENDBUTTON
                  #ENDBOXED
                  #BOXED,WHERE((%RangeLimitType='Single Value' OR %RangeLimitType='Range of Values'))
                    #ENABLE(ITEMS(%HigherKeys))
                      #BUTTON('Higher Key Component'),FROM(%HigherKeys,%HigherKeyField&' = '&CHOOSE(%HigherKeyValueType,'Current Value',%HigherKeyValueVariable,%HigherKeyValueFixed)),AT(,,175),HLP('~TPLHigherKeyComponent')
                        #ENABLE(%False)
                          #PROMPT ('Key Field:', FIELD),%HigherKeyField,DEFAULT(%HigherKey)
                        #ENDENABLE
                        #PROMPT('Value Type:',DROP('Current Value|Variable Value|Fixed Value')),%HigherKeyValueType,CHOICE,DEFAULT(1)
                        #BOXED,WHERE(%HigherKeyValueType=2),AT(,30)
                          #PROMPT ('Value:', FIELD),%HigherKeyValueVariable
                        #ENDBOXED
                        #BOXED,WHERE(%HigherKeyValueType=3),AT(,30)
                          #PROMPT ('Value:', EXPR),%HigherKeyValueFixed
                        #ENDBOXED
                      #ENDBUTTON
                    #ENDENABLE
                  #ENDBOXED
                  #BOXED,SECTION
                    #BOXED('Range Limit Boundary'),WHERE(%RangeLimitType='Single Value'),AT(,0)
                      #PROMPT('&Range Limit Value:',FIELD),%RangeLimit
                    #ENDBOXED
                    #BOXED('Range Limit Boundaries'),WHERE(%RangeLimitType='Range of Values'),AT(,0)
                      #PROMPT('&Low Limit Value:',FIELD),%RangeLow
                      #PROMPT('&High Limit Value:',FIELD),%RangeHigh
                    #ENDBOXED
                    #BOXED('Range Limiting File'),WHERE(%RangeLimitType='File Relationship'),AT(,0)
                      #PROMPT('&Related File:',FILE),%RangeFile
                    #ENDBOXED
                  #ENDBOXED
                #ENDENABLE
              #ENDENABLE
            #ENDBOXED
          #ENDTAB
          #TAB('Icons'),HLP('~IDH_PrimaryIcons')
            #BOXED('Icons'),SECTION
              #BOXED,WHERE(%ControlHasIcon),AT(,0)
                #ENABLE(%FileHasChildren(%Primary)),CLEAR
                  #PROMPT('Use different icons for Expand and Contract',CHECK),%MCRTPrimaryUseExpandAndContractIcon,DEFAULT(%False),AT(10)
                #ENDENABLE
                #BOXED('Default Icon')
                  #BOXED,SECTION
                    #BOXED,WHERE(%MCRTPrimaryUseExpandAndContractIcon),AT(,0)
                      #BOXED('Contracted')
                        #PROMPT('Icon Type:',DROP('Built-in[Builtin]|File|Variable|External')),%MCRTPrimaryDefaultContractedIconType,DEFAULT('File')
                        #BOXED,SECTION
                          #BOXED,WHERE(%MCRTPrimaryDefaultContractedIconType = 'Builtin'),AT(,0)
                            #PROMPT('Icon (ICON:*):',FROM(%MCRTBuiltinIcons)),%MCRTPrimaryDefaultContractedIconB,DEFAULT('None')
                          #ENDBOXED
                          #BOXED,WHERE(%MCRTPrimaryDefaultContractedIconType = 'File'),AT(,0)
                            #PROMPT('Icon (*.ico):',ICON('Select Primary Level Icon','Icon files (*.ico)|*.ico')),%MCRTPrimaryDefaultContractedIconF
                          #ENDBOXED
                          #BOXED,WHERE(%MCRTPrimaryDefaultContractedIconType = 'Variable'),AT(,0)
                            #PROMPT('Icon (!Variable):',OPTFIELD),%MCRTPrimaryDefaultContractedIconV
                            #VALIDATE(SUB(%MCRTPrimaryDefaultContractedIconV,1,1) = '!','An exclamation sign must be preceded!')
                          #ENDBOXED
                          #BOXED,WHERE(%MCRTPrimaryDefaultContractedIconType = 'External'),AT(,0)
                            #PROMPT('Icon (*.ico[i]):',@s65),%MCRTPrimaryDefaultContractedIconE
                            #VALIDATE(SUB(%MCRTPrimaryDefaultContractedIconE,LEN(%MCRTPrimaryDefaultContractedIconE),1) = ']','An order number of icon must be specified in the squared brackets! Example: icon.ico[1]')
                            #VALIDATE(INSTRING('.ico[',%MCRTPrimaryDefaultContractedIconE,1,1),'An order number of icon must be specified in the squared brackets! Example: icon.ico[1]')
                          #ENDBOXED
                        #ENDBOXED
                      #ENDBOXED
                      #BOXED('Expanded')
                        #PROMPT('Icon Type:',DROP('Built-in[Builtin]|File|Variable|External')),%MCRTPrimaryDefaultExpandedIconType,DEFAULT('File')
                        #BOXED,SECTION
                          #BOXED,WHERE(%MCRTPrimaryDefaultExpandedIconType = 'Builtin'),AT(,0)
                            #PROMPT('Icon (ICON:*):',FROM(%MCRTBuiltinIcons)),%MCRTPrimaryDefaultExpandedIconB,DEFAULT('None')
                          #ENDBOXED
                          #BOXED,WHERE(%MCRTPrimaryDefaultExpandedIconType = 'File'),AT(,0)
                            #PROMPT('Icon (*.ico):',ICON('Select Primary Level Icon','Icon files (*.ico)|*.ico')),%MCRTPrimaryDefaultExpandedIconF
                          #ENDBOXED
                          #BOXED,WHERE(%MCRTPrimaryDefaultExpandedIconType = 'Variable'),AT(,0)
                            #PROMPT('Icon (!Variable):',OPTFIELD),%MCRTPrimaryDefaultExpandedIconV
                            #VALIDATE(SUB(%MCRTPrimaryDefaultExpandedIconV,1,1) = '!','An exclamation sign must be preceded!')
                          #ENDBOXED
                          #BOXED,WHERE(%MCRTPrimaryDefaultExpandedIconType = 'External'),AT(,0)
                            #PROMPT('Icon (*.ico[i]):',@s65),%MCRTPrimaryDefaultExpandedIconE
                            #VALIDATE(SUB(%MCRTPrimaryDefaultExpandedIconE,LEN(%MCRTPrimaryDefaultExpandedIconE),1) = ']','An order number of icon must be specified in the squared brackets! Example: icon.ico[1]')
                            #VALIDATE(INSTRING('.ico[',%MCRTPrimaryDefaultExpandedIconE,1,1),'An order number of icon must be specified in the squared brackets! Example: icon.ico[1]')
                          #ENDBOXED
                        #ENDBOXED
                      #ENDBOXED
                    #ENDBOXED
                    #BOXED,WHERE(NOT %MCRTPrimaryUseExpandAndContractIcon),AT(,0)
                      #PROMPT('Icon Type:',DROP('Built-in[Builtin]|File|Variable|External')),%MCRTPrimaryDefaultIconType,DEFAULT('File')
                      #BOXED,SECTION
                        #BOXED,WHERE(%MCRTPrimaryDefaultIconType = 'Builtin'),AT(,0)
                          #PROMPT('Icon (ICON:*):',FROM(%MCRTBuiltinIcons)),%MCRTPrimaryDefaultIconB,DEFAULT('None')
                        #ENDBOXED
                        #BOXED,WHERE(%MCRTPrimaryDefaultIconType = 'File'),AT(,0)
                          #PROMPT('Icon (*.ico):',ICON('Select Primary Level Icon','Icon files (*.ico)|*.ico')),%MCRTPrimaryDefaultIconF
                        #ENDBOXED
                        #BOXED,WHERE(%MCRTPrimaryDefaultIconType = 'Variable'),AT(,0)
                          #PROMPT('Icon (!Variable):',OPTFIELD),%MCRTPrimaryDefaultIconV
                          #VALIDATE(SUB(%MCRTPrimaryDefaultIconV,1,1) = '!','An exclamation sign must be preceded!')
                        #ENDBOXED
                        #BOXED,WHERE(%MCRTPrimaryDefaultIconType = 'External'),AT(,0)
                          #PROMPT('Icon (*.ico[i]):',@s65),%MCRTPrimaryDefaultIconE
                          #VALIDATE(SUB(%MCRTPrimaryDefaultIconE,LEN(%MCRTPrimaryDefaultIconE),1) = ']','An order number of icon must be specified in the squared brackets! Example: icon.ico[1]')
                          #VALIDATE(INSTRING('.ico[',%MCRTPrimaryDefaultIconE,1,1),'An order number of icon must be specified in the squared brackets! Example: icon.ico[1]')
                        #ENDBOXED
                      #ENDBOXED
                    #ENDBOXED
                  #ENDBOXED
                #ENDBOXED
                #BOXED('Conditional Icons')
                  #BUTTON('Conditional Icons...'),MULTI(%MCRTPrimaryConditionalIcons,'IF ' & %MCRTPrimaryIconCondition & ' use ' & %SetConditionalIcon('primary')),INLINE,AT(,,,50)
                    #PROMPT('Condition:',EXPR),%MCRTPrimaryIconCondition,REQ
                    #BOXED,SECTION
                      #BOXED,WHERE(%MCRTPrimaryUseExpandAndContractIcon),AT(,0)
                        #BOXED('Contracted')
                          #PROMPT('Icon Type:',DROP('Built-in[Builtin]|File|Variable|External')),%MCRTPrimaryConditionalContractedIconType,DEFAULT('File')
                          #BOXED,SECTION
                            #BOXED,WHERE(%MCRTPrimaryConditionalContractedIconType = 'Builtin'),AT(,0)
                              #PROMPT('Icon (ICON:*):',FROM(%MCRTBuiltinIcons)),%MCRTPrimaryConditionalContractedIconB,DEFAULT('None')
                            #ENDBOXED
                            #BOXED,WHERE(%MCRTPrimaryConditionalContractedIconType = 'File'),AT(,0)
                              #PROMPT('Icon (*.ico):',ICON('Select Primary Level Icon','Icon files (*.ico)|*.ico')),%MCRTPrimaryConditionalContractedIconF
                            #ENDBOXED
                            #BOXED,WHERE(%MCRTPrimaryConditionalContractedIconType = 'Variable'),AT(,0)
                              #PROMPT('Icon (!Variable):',OPTFIELD),%MCRTPrimaryConditionalContractedIconV
                              #VALIDATE(LEN(%MCRTPrimaryConditionalContractedIconV) > 0 AND SUB(%MCRTPrimaryConditionalContractedIconV,1,1) = '!','An exclamation sign must be preceded!')
                            #ENDBOXED
                            #BOXED,WHERE(%MCRTPrimaryConditionalContractedIconType = 'External'),AT(,0)
                              #PROMPT('Icon (*.ico[i]):',@s65),%MCRTPrimaryConditionalContractedIconE
                              #VALIDATE(SUB(%MCRTPrimaryConditionalContractedIconE,LEN(%MCRTPrimaryConditionalContractedIconE),1) = ']','An order number of icon must be specified in the squared brackets! Example: icon.ico[1]')
                              #VALIDATE(INSTRING('.ico[',%MCRTPrimaryConditionalContractedIconE,1,1),'An order number of icon must be specified in the squared brackets! Example: icon.ico[1]')
                            #ENDBOXED
                          #ENDBOXED
                        #ENDBOXED
                        #BOXED('Expanded')
                          #PROMPT('Icon Type:',DROP('Built-in[Builtin]|File|Variable|External')),%MCRTPrimaryConditionalExpandedIconType,DEFAULT('File')
                          #BOXED,SECTION
                            #BOXED,WHERE(%MCRTPrimaryConditionalExpandedIconType = 'Builtin'),AT(,0)
                              #PROMPT('Icon (ICON:*):',FROM(%MCRTBuiltinIcons)),%MCRTPrimaryConditionalExpandedIconB,DEFAULT('None')
                            #ENDBOXED
                            #BOXED,WHERE(%MCRTPrimaryConditionalExpandedIconType = 'File'),AT(,0)
                              #PROMPT('Icon (*.ico):',ICON('Select Primary Level Icon','Icon files (*.ico)|*.ico')),%MCRTPrimaryConditionalExpandedIconF
                            #ENDBOXED
                            #BOXED,WHERE(%MCRTPrimaryConditionalExpandedIconType = 'Variable'),AT(,0)
                              #PROMPT('Icon (!Variable):',OPTFIELD),%MCRTPrimaryConditionalExpandedIconV
                              #VALIDATE(LEN(%MCRTPrimaryConditionalExpandedIconV) > 0 AND SUB(%MCRTPrimaryConditionalExpandedIconV,1,1) = '!','An exclamation sign must be preceded!')
                            #ENDBOXED
                            #BOXED,WHERE(%MCRTPrimaryConditionalExpandedIconType = 'External'),AT(,0)
                              #PROMPT('Icon (*.ico[i]):',@s65),%MCRTPrimaryConditionalExpandedIconE
                              #VALIDATE(SUB(%MCRTPrimaryConditionalExpandedIconE,LEN(%MCRTPrimaryConditionalExpandedIconE),1) = ']','An order number of icon must be specified in the squared brackets! Example: icon.ico[1]')
                              #VALIDATE(INSTRING('.ico[',%MCRTPrimaryConditionalExpandedIconE,1,1),'An order number of icon must be specified in the squared brackets! Example: icon.ico[1]')
                            #ENDBOXED
                          #ENDBOXED
                        #ENDBOXED
                      #ENDBOXED
                      #BOXED,WHERE(NOT %MCRTPrimaryUseExpandAndContractIcon),AT(,0)
                        #PROMPT('Icon Type:',DROP('Built-in[Builtin]|File|Variable|External')),%MCRTPrimaryConditionalIconType,DEFAULT('File')
                        #BOXED,SECTION
                          #BOXED,WHERE(%MCRTPrimaryConditionalIconType = 'Builtin'),AT(,0)
                            #PROMPT('Icon (ICON:*):',FROM(%MCRTBuiltinIcons)),%MCRTPrimaryConditionalIconB,DEFAULT('None')
                          #ENDBOXED
                          #BOXED,WHERE(%MCRTPrimaryConditionalIconType = 'File'),AT(,0)
                            #PROMPT('Icon (*.ico):',ICON('Select Primary Level Icon','Icon files (*.ico)|*.ico')),%MCRTPrimaryConditionalIconF
                          #ENDBOXED
                          #BOXED,WHERE(%MCRTPrimaryConditionalIconType = 'Variable'),AT(,0)
                            #PROMPT('Icon (!Variable):',OPTFIELD),%MCRTPrimaryConditionalIconV
                            #VALIDATE(LEN(%MCRTPrimaryConditionalIconV) > 0 AND SUB(%MCRTPrimaryConditionalIconV,1,1) = '!','An exclamation sign must be preceded!')
                          #ENDBOXED
                          #BOXED,WHERE(%MCRTPrimaryConditionalIconType = 'External'),AT(,0)
                            #PROMPT('Icon (*.ico[i]):',@s65),%MCRTPrimaryConditionalIconE
                            #VALIDATE(SUB(%MCRTPrimaryConditionalIconE,LEN(%MCRTPrimaryConditionalIconE),1) = ']','An order number of icon must be specified in the squared brackets! Example: icon.ico[1]')
                            #VALIDATE(INSTRING('.ico[',%MCRTPrimaryConditionalIconE,1,1),'An order number of icon must be specified in the squared brackets! Example: icon.ico[1]')
                          #ENDBOXED
                        #ENDBOXED
                      #ENDBOXED
                    #ENDBOXED
                  #ENDBUTTON
                #ENDBOXED
              #ENDBOXED
              #BOXED,WHERE(~%ControlHasIcon),AT(,0)
                #DISPLAY('Your MultiChildRelationTree control is not set to use custom icons. To activate custom icons for your MultiChildRelationTree control, follow these steps:'),AT(,,168,25)
                #DISPLAY('1.  If you aren''t in the window formatter, go there.'),AT(,,168)
                #DISPLAY('2.  Right-click on the MultiChildRelationTree control, and select "List Box Format..."'),AT(,,168,15)
                #DISPLAY('3.  Select the column you want to use an icon with from the left hand list.'),AT(10,,175,16)
                #DISPLAY('4.  Set the "Icon" property to either "Normal" or "Transparent".'),AT(10,,175,16)
                #DISPLAY('5.  Repeat steps 3-4 as necessary.'),AT(10,,175)
                #DISPLAY('6.  When you call up the MultiChildRelationTree control actions window, this tab will let you set the icons.'),AT(,,172,20)
              #ENDBOXED
            #ENDBOXED
          #ENDTAB
          #TAB('Colors'),HLP('~IDH_PrimaryColors')
            #BOXED('Colors'),SECTION
              #BOXED,WHERE(%ControlHasColor),AT(,0)
                #BOXED('Default Colors')
                  #PROMPT('Foreground Normal:',COLOR),%MCRTPrimaryNormalFGColor,DEFAULT(-1)
                  #PROMPT('Background Normal:',COLOR),%MCRTPrimaryNormalBGColor,DEFAULT(-1)
                  #PROMPT('Foreground Selected:',COLOR),%MCRTPrimarySelectedFGColor,DEFAULT(-1)
                  #PROMPT('Background Selected:',COLOR),%MCRTPrimarySelectedBGColor,DEFAULT(-1)
                #ENDBOXED
                #BOXED('Conditional Colors')
                  #BUTTON('Conditional Colors...'),MULTI(%MCRTPrimaryConditionalColors,%MCRTPrimaryColorCondition),INLINE,AT(,,,70)
                    #PROMPT('Condition:',EXPR),%MCRTPrimaryColorCondition,REQ
                    #PROMPT('Foreground Normal:',COLOR),%MCRTPrimaryNormalFGConditionalColor,DEFAULT(-1)
                    #PROMPT('Background Normal:',COLOR),%MCRTPrimaryNormalBGConditionalColor,DEFAULT(-1)
                    #PROMPT('Foreground Selected:',COLOR),%MCRTPrimarySelectedFGConditionalColor,DEFAULT(-1)
                    #PROMPT('Background Selected:',COLOR),%MCRTPrimarySelectedBGConditionalColor,DEFAULT(-1)
                  #ENDBUTTON
                #ENDBOXED
              #ENDBOXED
              #BOXED,WHERE(~%ControlHasColor),AT(,0)
                #DISPLAY('Your MultiChildRelationTree control is not set to use custom colors. To activate custom colors for your MultiChildRelationTree control, follow these steps:'),AT(,,168,25)
                #DISPLAY('1.  If you aren''t in the window formatter, go there.'),AT(,,168)
                #DISPLAY('2.  Right-click on the MultiChildRelationTree control, and select "List Box Format..."'),AT(,,168,15)
                #DISPLAY('3.  Select the field you want to colorize, and select the "General" tab.'),AT(,,168,15)
                #DISPLAY('4.  Check the "Color" checkbox.'),AT(,,168)
                #DISPLAY('5.  Repeat steps 3 and 4 as necessary.'),AT(,,168)
                #DISPLAY('6.  When you call up the TreeControl actions window, this tab will let you set the colors.'),AT(,,168,20)
              #ENDBOXED
            #ENDBOXED
          #ENDTAB
          #TAB('Styles'),HLP('~IDH_PrimaryStyles')
            #BOXED('Styles'),SECTION
              #BOXED,WHERE(%ControlHasStyle),AT(,0)
                #BOXED('Default Style')
                  #PROMPT('Style Type:',DROP('Local List|Style Number')),%MCRTPrimaryStyleType,DEFAULT('Local List')
                  #BOXED,SECTION
                    #BOXED,WHERE(%MCRTPrimaryStyleType='Local List'),AT(0,0)
                      #PROMPT('Style:',FROM(%WindowStyles,,INSTANCE(%WindowStyles)&'-'&CHOOSE(%WindowStyleFontNameIsVariable,%WindowStyleFontNameVariable,%WindowStyleFontName)&','&CHOOSE(%WindowStyleFontSizeIsVariable,%WindowStyleFontSizeVariable,%WindowStyleFontSize))),%MCRTPrimaryStyleLocalList
                    #ENDBOXED
                    #BOXED,WHERE(%MCRTPrimaryStyleType='Style Number'),AT(0,0)
                      #PROMPT('Style Number:',SPIN(@n6,0,65535)),%MCRTPrimaryStyle
                    #ENDBOXED
                  #ENDBOXED
                #ENDBOXED
                #BOXED('Conditional Styles')
                  #BUTTON('&Conditional Styles'),MULTI(%MCRTPrimaryConditionalStyles,%MCRTPrimaryStyleCondition),INLINE,HLP('~TPLControlBrowseBox_Styles')
                    #PROMPT('&Condition:',EXPR),%MCRTPrimaryStyleCondition,REQ
                    #PROMPT('Style Type:',DROP('Local List|Style Number')),%MCRTPrimaryConditionalStyleType,DEFAULT('Local List')
                    #BOXED,SECTION
                      #BOXED,WHERE(%MCRTPrimaryConditionalStyleType='Local List'),AT(0,0)
                        #PROMPT('Style:',FROM(%WindowStyles,,INSTANCE(%WindowStyles)&'-'&CHOOSE(%WindowStyleFontNameIsVariable,%WindowStyleFontNameVariable,%WindowStyleFontName)&','&CHOOSE(%WindowStyleFontSizeIsVariable,%WindowStyleFontSizeVariable,%WindowStyleFontSize))),%MCRTPrimaryConditionalStyleLocalList
                      #ENDBOXED
                      #BOXED,WHERE(%MCRTPrimaryConditionalStyleType='Style Number'),AT(0,0)
                        #PROMPT('Style Number:',SPIN(@n6,0,65535)),%MCRTPrimaryConditionalStyle
                      #ENDBOXED
                    #ENDBOXED
                  #ENDBUTTON
                #ENDBOXED
              #ENDBOXED
              #BOXED,WHERE(~%ControlHasStyle),AT(,0)
                #DISPLAY('Your MultiChildRelationTree control is not set to use custom styles. To activate custom styles for your MultiChildRelationTree control, follow these steps:'),AT(,,168,25)
                #DISPLAY('1.  If you aren''t in the window formatter, go there.'),AT(,,168)
                #DISPLAY('2.  Right-click on the MultiChildRelationTree control, and select "List Box Format..."'),AT(,,168,15)
                #DISPLAY('3.  Select the field you want to stylize, and select the "General" tab.'),AT(,,168,15)
                #DISPLAY('4.  Check the "Style" checkbox.'),AT(,,168)
                #DISPLAY('5.  Repeat steps 3 and 4 as necessary.'),AT(,,168)
                #DISPLAY('6.  When you call up the TreeControl actions window, this tab will let you set the styles.'),AT(,,168,20)
              #ENDBOXED
            #ENDBOXED
          #ENDTAB
          #TAB('&Hot Fields'),HLP('~TPLControlBrowseBox_Hot_Fields')
            #BUTTON('"Hot" Fields'),MULTI(%MCRTPrimaryHotFields,%MCRTPrimaryHotField),INLINE,HLP('~TPLControlBrowseBox_Hot_Fields')
              #PROMPT('Hot Field:',FIELD(%Primary)),%MCRTPrimaryHotField,REQ
            #ENDBUTTON
          #ENDTAB
        #ENDSHEET
      #ENDBOXED
    #ENDTAB
    #TAB('Child Levels'),HLP('~IDH_Child')
      #PREPARE
        #FIND(%ControlInstance,%ActiveTemplateInstance,%Control)
      #ENDPREPARE
      #BOXED('Child Files')
        #BUTTON('Child Files'),FROM(%Secondary,%Secondary & ' - link to ' & %SecondaryTo),INLINE,WHERE(%SecondaryType = 'MANY:1' OR %SecondaryType = '')
          #SHEET
            #TAB('General')
              #DISPLAY('File: '&%Secondary)
              #PREPARE
                #SET(%MCRTAuxLocalFile,%Secondary)
              #ENDPREPARE
              #BOXED,HIDE
                #PROMPT('Local FILE:',FILE),%MCRTAuxLocalFile
              #ENDBOXED
              #BOXED('Level')
                #PROMPT('Level &Title:',EXPR),%MCRTChildLevelTitle
                #PROMPT('Field to &Display:',EXPR),%MCRTChildDisplay
                #PROMPT('Advanced Display (Embed)',EMBEDBUTTON(%MCRTAfterSetQueueRecord,%ActiveTemplateInstance,%MCRTAuxLocalFile)),AT(,,170)
                #PREPARE
                  #FIND(%ControlInstance,%ActiveTemplateInstance,%Control)
                  #FIX(%Secondary,%MCRTAuxLocalFile)
                #ENDPREPARE
                #ENABLE(%SecondaryType = '')
                  #BUTTON('Custom Join'),AT(,,170)
                    #PROMPT('Key to use:',KEY(%Secondary)),%MCRTChildCustomJoinKey,REQ,WHENACCEPTED(%MCRTLoadKeyComponents(%Secondary,%MCRTChildCustomJoinKey,1))
                    #BOXED,HIDE
                      #BUTTON('KeyComponents'),MULTI(%MCRTChildCustomJoinKeyComponents,%MCRTChildCustomJoinKeyComponent)
                        #PROMPT('KeyComponent:',COMPONENT(%MCRTChildCustomJoinKey)),%MCRTChildCustomJoinKeyComponent
                      #ENDBUTTON
                    #ENDBOXED
                    #BUTTON('Custom Join Field'),AT(,,175),FROM(%MCRTChildCustomJoinKeyComponents,%MCRTChildCustomJoinKeyComponent&' = '&CHOOSE(%MCRTChildCustomJoinKeyComponentValueType,'Clear Value',%MCRTChildCustomJoinKeyComponentValueVariable,%MCRTChildCustomJoinKeyComponentValueFixed)),INLINE
                      #ENABLE(%False)
                        #PROMPT ('Key Field:', FIELD),%MCRTChildCustomJoinKeyComponentField,DEFAULT(%MCRTChildCustomJoinKeyComponent)
                      #ENDENABLE
                      #PROMPT('Value Type:',DROP('Clear Value|Variable Value|Fixed Value')),%MCRTChildCustomJoinKeyComponentValueType,CHOICE,DEFAULT(1)
                      #BOXED,WHERE(%MCRTChildCustomJoinKeyComponentValueType=2),AT(,30)
                        #PROMPT ('Value:', FIELD),%MCRTChildCustomJoinKeyComponentValueVariable
                      #ENDBOXED
                      #BOXED,WHERE(%MCRTChildCustomJoinKeyComponentValueType=3),AT(,30)
                        #PROMPT ('Value:', EXPR),%MCRTChildCustomJoinKeyComponentValueFixed
                      #ENDBOXED
                    #ENDBUTTON
                  #ENDBUTTON
                #ENDENABLE
                #PROMPT('Record &Filter:',EXPR),%MCRTChildRecordFilter
                #BOXED('Root')
                  #PROMPT('Level Root:',EXPR),%MCRTChildRootLevelName
                  #ENABLE(%MCRTChildRootLevelName)
                    #BUTTON('Level Root Format'),AT(,,162)
                      #SHEET,ADJUST
                        #TAB('Icons'),HLP('~IDH_PrimaryIcons')
                          #BOXED,SECTION
                            #BOXED,WHERE(%ControlHasIcon),AT(,0)
                              #PROMPT('Use different icons for Expand and Contract',CHECK),%MCRTChildRootUseExpandAndContractIcon,DEFAULT(%False),AT(10)
                              #BOXED('Default Icon'),SECTION
                                #BOXED,WHERE(NOT %MCRTChildRootUseExpandAndContractIcon),AT(,0)
                                  #PROMPT('Icon Type:',DROP('Built-in[Builtin]|File|Variable|External')),%MCRTChildRootDefaultIconType,DEFAULT('File')
                                  #BOXED,SECTION
                                    #BOXED,WHERE(%MCRTChildRootDefaultIconType = 'Builtin'),AT(,0)
                                      #PROMPT('Icon (ICON:*):',FROM(%MCRTBuiltinIcons)),%MCRTChildRootDefaultIconB,DEFAULT('None')
                                    #ENDBOXED
                                    #BOXED,WHERE(%MCRTChildRootDefaultIconType = 'File'),AT(,0)
                                      #PROMPT('Icon (*.ico):',ICON('Select ChildRoot Level Icon','Icon files (*.ico)|*.ico')),%MCRTChildRootDefaultIconF
                                    #ENDBOXED
                                    #BOXED,WHERE(%MCRTChildRootDefaultIconType = 'Variable'),AT(,0)
                                      #PROMPT('Icon (!Variable):',OPTFIELD),%MCRTChildRootDefaultIconV
                                      #VALIDATE(SUB(%MCRTChildRootDefaultIconV,1,1) = '!','An exclamation sign must be preceded!')
                                    #ENDBOXED
                                    #BOXED,WHERE(%MCRTChildRootDefaultIconType = 'External'),AT(,0)
                                      #PROMPT('Icon (*.ico[i]):',@s65),%MCRTChildRootDefaultIconE
                                      #VALIDATE(SUB(%MCRTChildRootDefaultIconE,LEN(%MCRTChildRootDefaultIconE),1) = ']','An order number of icon must be specified in the squared brackets! Example: icon.ico[1]')
                                      #VALIDATE(INSTRING('.ico[',%MCRTChildRootDefaultIconE,1,1),'An order number of icon must be specified in the squared brackets! Example: icon.ico[1]')
                                    #ENDBOXED
                                  #ENDBOXED
                                #ENDBOXED
                                #BOXED,WHERE(%MCRTChildRootUseExpandAndContractIcon),AT(,0)
                                  #BOXED('Contracted')
                                    #PROMPT('Icon Type:',DROP('Built-in[Builtin]|File|Variable|External')),%MCRTChildRootDefaultContractedIconType,DEFAULT('File')
                                    #BOXED,SECTION
                                      #BOXED,WHERE(%MCRTChildRootDefaultContractedIconType = 'Builtin'),AT(,0)
                                        #PROMPT('Icon (ICON:*):',FROM(%MCRTBuiltinIcons)),%MCRTChildRootDefaultContractedIconB,DEFAULT('None')
                                      #ENDBOXED
                                      #BOXED,WHERE(%MCRTChildRootDefaultContractedIconType = 'File'),AT(,0)
                                        #PROMPT('Icon (*.ico):',ICON('Select Primary Level Icon','Icon files (*.ico)|*.ico')),%MCRTChildRootDefaultContractedIconF
                                      #ENDBOXED
                                      #BOXED,WHERE(%MCRTChildRootDefaultContractedIconType = 'Variable'),AT(,0)
                                        #PROMPT('Icon (!Variable):',OPTFIELD),%MCRTChildRootDefaultContractedIconV
                                        #VALIDATE(LEN(%MCRTChildRootDefaultContractedIconV) > 0 AND SUB(%MCRTChildRootDefaultContractedIconV,1,1) = '!','An exclamation sign must be preceded!')
                                      #ENDBOXED
                                      #BOXED,WHERE(%MCRTChildRootDefaultContractedIconType = 'External'),AT(,0)
                                        #PROMPT('Icon (*.ico[i]):',@s65),%MCRTChildRootDefaultContractedIconE
                                        #VALIDATE(SUB(%MCRTChildRootDefaultContractedIconE,LEN(%MCRTChildRootDefaultContractedIconE),1) = ']','An order number of icon must be specified in the squared brackets! Example: icon.ico[1]')
                                        #VALIDATE(INSTRING('.ico[',%MCRTChildRootDefaultContractedIconE,1,1),'An order number of icon must be specified in the squared brackets! Example: icon.ico[1]')
                                      #ENDBOXED
                                    #ENDBOXED
                                  #ENDBOXED
                                  #BOXED('Expanded')
                                    #PROMPT('Icon Type:',DROP('Built-in[Builtin]|File|Variable|External')),%MCRTChildRootDefaultExpandedIconType,DEFAULT('File')
                                    #BOXED,SECTION
                                      #BOXED,WHERE(%MCRTChildRootDefaultExpandedIconType = 'Builtin'),AT(,0)
                                        #PROMPT('Icon (ICON:*):',FROM(%MCRTBuiltinIcons)),%MCRTChildRootDefaultExpandedIconB,DEFAULT('None')
                                      #ENDBOXED
                                      #BOXED,WHERE(%MCRTChildRootDefaultExpandedIconType = 'File'),AT(,0)
                                        #PROMPT('Icon (*.ico):',ICON('Select Primary Level Icon','Icon files (*.ico)|*.ico')),%MCRTChildRootDefaultExpandedIconF
                                      #ENDBOXED
                                      #BOXED,WHERE(%MCRTChildRootDefaultExpandedIconType = 'Variable'),AT(,0)
                                        #PROMPT('Icon (!Variable):',OPTFIELD),%MCRTChildRootDefaultExpandedIconV
                                        #VALIDATE(LEN(%MCRTChildRootDefaultExpandedIconV) > 0 AND SUB(%MCRTChildRootDefaultExpandedIconV,1,1) = '!','An exclamation sign must be preceded!')
                                      #ENDBOXED
                                      #BOXED,WHERE(%MCRTChildRootDefaultExpandedIconType = 'External'),AT(,0)
                                        #PROMPT('Icon (*.ico[i]):',@s65),%MCRTChildRootDefaultExpandedIconE
                                        #VALIDATE(SUB(%MCRTChildRootDefaultExpandedIconE,LEN(%MCRTChildRootDefaultExpandedIconE),1) = ']','An order number of icon must be specified in the squared brackets! Example: icon.ico[1]')
                                        #VALIDATE(INSTRING('.ico[',%MCRTChildRootDefaultExpandedIconE,1,1),'An order number of icon must be specified in the squared brackets! Example: icon.ico[1]')
                                      #ENDBOXED
                                    #ENDBOXED
                                  #ENDBOXED
                                #ENDBOXED
                              #ENDBOXED
                              #BOXED('Conditional Icons')
                                #BUTTON('Conditional Icons...'),MULTI(%MCRTChildRootConditionalIcons,'IF ' & %MCRTChildRootIconCondition & ' use ' & %SetConditionalIcon('ChildRoot')),INLINE
                                  #PROMPT('Condition:',EXPR),%MCRTChildRootIconCondition,REQ
                                  #BOXED,SECTION
                                    #BOXED,WHERE(NOT %MCRTChildRootUseExpandAndContractIcon),AT(,0)
                                      #PROMPT('Icon Type:',DROP('Built-in[Builtin]|File|Variable|External')),%MCRTChildRootConditionalIconType,DEFAULT('File')
                                      #BOXED,SECTION
                                        #BOXED,WHERE(%MCRTChildRootConditionalIconType = 'Builtin'),AT(,0)
                                          #PROMPT('Icon (ICON:*):',FROM(%MCRTBuiltinIcons)),%MCRTChildRootConditionalIconB,DEFAULT('None')
                                        #ENDBOXED
                                        #BOXED,WHERE(%MCRTChildRootConditionalIconType = 'File'),AT(,0)
                                          #PROMPT('Icon (*.ico):',ICON('Select ChildRoot Level Icon','Icon files (*.ico)|*.ico')),%MCRTChildRootConditionalIconF
                                        #ENDBOXED
                                        #BOXED,WHERE(%MCRTChildRootConditionalIconType = 'Variable'),AT(,0)
                                          #PROMPT('Icon (!Variable):',OPTFIELD),%MCRTChildRootConditionalIconV
                                          #VALIDATE(LEN(%MCRTChildRootConditionalIconV) > 0 AND SUB(%MCRTChildRootConditionalIconV,1,1) = '!','An exclamation sign must be preceded!')
                                        #ENDBOXED
                                        #BOXED,WHERE(%MCRTChildRootConditionalIconType = 'External'),AT(,0)
                                          #PROMPT('Icon (*.ico[i]):',@s65),%MCRTChildRootConditionalIconE
                                          #VALIDATE(SUB(%MCRTChildRootConditionalIconE,LEN(%MCRTChildRootConditionalIconE),1) = ']','An order number of icon must be specified in the squared brackets! Example: icon.ico[1]')
                                          #VALIDATE(INSTRING('.ico[',%MCRTChildRootConditionalIconE,1,1),'An order number of icon must be specified in the squared brackets! Example: icon.ico[1]')
                                        #ENDBOXED
                                      #ENDBOXED
                                    #ENDBOXED
                                    #BOXED,WHERE(%MCRTChildRootUseExpandAndContractIcon),AT(,0)
                                      #BOXED('Contracted')
                                        #PROMPT('Icon Type:',DROP('Built-in[Builtin]|File|Variable|External')),%MCRTChildRootConditionalContractedIconType,DEFAULT('File')
                                        #BOXED,SECTION
                                          #BOXED,WHERE(%MCRTChildRootConditionalContractedIconType = 'Builtin'),AT(,0)
                                            #PROMPT('Icon (ICON:*):',FROM(%MCRTBuiltinIcons)),%MCRTChildRootConditionalContractedIconB,DEFAULT('None')
                                          #ENDBOXED
                                          #BOXED,WHERE(%MCRTChildRootConditionalContractedIconType = 'File'),AT(,0)
                                            #PROMPT('Icon (*.ico):',ICON('Select ChildRoot Level Icon','Icon files (*.ico)|*.ico')),%MCRTChildRootConditionalContractedIconF
                                          #ENDBOXED
                                          #BOXED,WHERE(%MCRTChildRootConditionalContractedIconType = 'Variable'),AT(,0)
                                            #PROMPT('Icon (!Variable):',OPTFIELD),%MCRTChildRootConditionalContractedIconV
                                            #VALIDATE(LEN(%MCRTChildRootConditionalContractedIconV) > 0 AND SUB(%MCRTChildRootConditionalContractedIconV,1,1) = '!','An exclamation sign must be preceded!')
                                          #ENDBOXED
                                          #BOXED,WHERE(%MCRTChildRootConditionalContractedIconType = 'External'),AT(,0)
                                            #PROMPT('Icon (*.ico[i]):',@s65),%MCRTChildRootConditionalContractedIconE
                                            #VALIDATE(SUB(%MCRTChildRootConditionalContractedIconE,LEN(%MCRTChildRootConditionalContractedIconE),1) = ']','An order number of icon must be specified in the squared brackets! Example: icon.ico[1]')
                                            #VALIDATE(INSTRING('.ico[',%MCRTChildRootConditionalContractedIconE,1,1),'An order number of icon must be specified in the squared brackets! Example: icon.ico[1]')
                                          #ENDBOXED
                                        #ENDBOXED
                                      #ENDBOXED
                                      #BOXED('Expanded')
                                        #PROMPT('Icon Type:',DROP('Built-in[Builtin]|File|Variable|External')),%MCRTChildRootConditionalExpandedIconType,DEFAULT('File')
                                        #BOXED,SECTION
                                          #BOXED,WHERE(%MCRTChildRootConditionalExpandedIconType = 'Builtin'),AT(,0)
                                            #PROMPT('Icon (ICON:*):',FROM(%MCRTBuiltinIcons)),%MCRTChildRootConditionalExpandedIconB,DEFAULT('None')
                                          #ENDBOXED
                                          #BOXED,WHERE(%MCRTChildRootConditionalExpandedIconType = 'File'),AT(,0)
                                            #PROMPT('Icon (*.ico):',ICON('Select ChildRoot Level Icon','Icon files (*.ico)|*.ico')),%MCRTChildRootConditionalExpandedIconF
                                          #ENDBOXED
                                          #BOXED,WHERE(%MCRTChildRootConditionalExpandedIconType = 'Variable'),AT(,0)
                                            #PROMPT('Icon (!Variable):',OPTFIELD),%MCRTChildRootConditionalExpandedIconV
                                            #VALIDATE(LEN(%MCRTChildRootConditionalExpandedIconV) > 0 AND SUB(%MCRTChildRootConditionalExpandedIconV,1,1) = '!','An exclamation sign must be preceded!')
                                          #ENDBOXED
                                          #BOXED,WHERE(%MCRTChildRootConditionalExpandedIconType = 'External'),AT(,0)
                                            #PROMPT('Icon (*.ico[i]):',@s65),%MCRTChildRootConditionalExpandedIconE
                                            #VALIDATE(SUB(%MCRTChildRootConditionalExpandedIconE,LEN(%MCRTChildRootConditionalExpandedIconE),1) = ']','An order number of icon must be specified in the squared brackets! Example: icon.ico[1]')
                                            #VALIDATE(INSTRING('.ico[',%MCRTChildRootConditionalExpandedIconE,1,1),'An order number of icon must be specified in the squared brackets! Example: icon.ico[1]')
                                          #ENDBOXED
                                        #ENDBOXED
                                      #ENDBOXED
                                    #ENDBOXED
                                  #ENDBOXED
                                #ENDBUTTON
                              #ENDBOXED
                            #ENDBOXED
                            #BOXED,WHERE(~%ControlHasIcon),AT(,0)
                              #DISPLAY('Your MultiChildRelationTree control is not set to use custom icons. To activate custom icons for your MultiChildRelationTree control, follow these steps:'),AT(,,168,25)
                              #DISPLAY('1.  If you aren''t in the window formatter, go there.'),AT(,,168)
                              #DISPLAY('2.  Right-click on the MultiChildRelationTree control, and select "List Box Format..."'),AT(,,168,15)
                              #DISPLAY('3.  Select the column you want to use an icon with from the left hand list.'),AT(10,,175,16)
                              #DISPLAY('4.  Set the "Icon" property to either "Normal" or "Transparent".'),AT(10,,175,16)
                              #DISPLAY('5.  Repeat steps 3-4 as necessary.'),AT(10,,175)
                              #DISPLAY('6.  When you call up the MultiChildRelationTree control actions window, this tab will let you set the icons.'),AT(,,172,20)
                            #ENDBOXED
                          #ENDBOXED
                        #ENDTAB
                        #TAB('Colors'),HLP('~IDH_PrimaryColors')
                          #BOXED('Colors')
                            #BOXED,SECTION
                              #BOXED,WHERE(%ControlHasColor),AT(,0)
                                #BOXED('Default Colors')
                                  #PROMPT('Foreground Normal:',COLOR),%MCRTChildRootNormalFGColor,DEFAULT(-1)
                                  #PROMPT('Background Normal:',COLOR),%MCRTChildRootNormalBGColor,DEFAULT(-1)
                                  #PROMPT('Foreground Selected:',COLOR),%MCRTChildRootSelectedFGColor,DEFAULT(-1)
                                  #PROMPT('Background Selected:',COLOR),%MCRTChildRootSelectedBGColor,DEFAULT(-1)
                                #ENDBOXED
                                #BOXED('Conditional Colors')
                                  #BUTTON('Conditional Colors...'),MULTI(%MCRTChildRootConditionalColors,%MCRTChildRootColorCondition),INLINE,AT(,,,70)
                                    #PROMPT('Condition:',EXPR),%MCRTChildRootColorCondition,REQ
                                    #PROMPT('Foreground Normal:',COLOR),%MCRTChildRootNormalFGConditionalColor,DEFAULT(-1)
                                    #PROMPT('Background Normal:',COLOR),%MCRTChildRootNormalBGConditionalColor,DEFAULT(-1)
                                    #PROMPT('Foreground Selected:',COLOR),%MCRTChildRootSelectedFGConditionalColor,DEFAULT(-1)
                                    #PROMPT('Background Selected:',COLOR),%MCRTChildRootSelectedBGConditionalColor,DEFAULT(-1)
                                  #ENDBUTTON
                                #ENDBOXED
                              #ENDBOXED
                              #BOXED,WHERE(~%ControlHasColor),AT(,0)
                                #DISPLAY('Your MultiChildRelationTree control is not set to use custom colors. To activate custom colors for your MultiChildRelationTree control, follow these steps:'),AT(,,168,25)
                                #DISPLAY('1.  If you aren''t in the window formatter, go there.'),AT(,,168)
                                #DISPLAY('2.  Right-click on the MultiChildRelationTree control, and select "List Box Format..."'),AT(,,168,15)
                                #DISPLAY('3.  Select the field you want to colorize, and select the "General" tab.'),AT(,,168,15)
                                #DISPLAY('4.  Check the "Color" checkbox.'),AT(,,168)
                                #DISPLAY('5.  Repeat steps 3 and 4 as necessary.'),AT(,,168)
                                #DISPLAY('6.  When you call up the TreeControl actions window, this tab will let you set the colors.'),AT(,,168,20)
                              #ENDBOXED
                            #ENDBOXED
                          #ENDBOXED
                        #ENDTAB
                        #TAB('Styles'),HLP('~IDH_PrimaryStyles')
                          #BOXED('Styles'),SECTION
                            #BOXED,WHERE(%ControlHasStyle),AT(,0)
                              #BOXED('Default Style')
                                #PROMPT('Style Type:',DROP('Local List|Style Number')),%MCRTChildRootStyleType,DEFAULT('Local List')
                                #BOXED,SECTION
                                  #BOXED,WHERE(%MCRTChildRootStyleType='Local List'),AT(0,0)
                                    #PROMPT('Style:',FROM(%WindowStyles,,INSTANCE(%WindowStyles)&'-'&CHOOSE(%WindowStyleFontNameIsVariable,%WindowStyleFontNameVariable,%WindowStyleFontName)&','&CHOOSE(%WindowStyleFontSizeIsVariable,%WindowStyleFontSizeVariable,%WindowStyleFontSize))),%MCRTChildRootStyleLocalList
                                  #ENDBOXED
                                  #BOXED,WHERE(%MCRTChildRootStyleType='Style Number'),AT(0,0)
                                    #PROMPT('Style Number:',SPIN(@n6,0,65535)),%MCRTChildRootStyle
                                  #ENDBOXED
                                #ENDBOXED
                              #ENDBOXED
                              #BOXED('Conditional Styles')
                                #BUTTON('&Conditional Styles'),MULTI(%MCRTChildRootConditionalStyles,%MCRTChildRootStyleCondition),INLINE,HLP('~TPLControlBrowseBox_Styles')
                                  #PROMPT('&Condition:',EXPR),%MCRTChildRootStyleCondition,REQ
                                  #PROMPT('Style Type:',DROP('Local List|Style Number')),%MCRTChildRootConditionalStyleType,DEFAULT('Local List')
                                  #BOXED,SECTION
                                    #BOXED,WHERE(%MCRTChildRootConditionalStyleType='Local List'),AT(0,0)
                                      #PROMPT('Style:',FROM(%WindowStyles,,INSTANCE(%WindowStyles)&'-'&CHOOSE(%WindowStyleFontNameIsVariable,%WindowStyleFontNameVariable,%WindowStyleFontName)&','&CHOOSE(%WindowStyleFontSizeIsVariable,%WindowStyleFontSizeVariable,%WindowStyleFontSize))),%MCRTChildRootConditionalStyleLocalList
                                    #ENDBOXED
                                    #BOXED,WHERE(%MCRTChildRootConditionalStyleType='Style Number'),AT(0,0)
                                      #PROMPT('Style Number:',SPIN(@n6,0,65535)),%MCRTChildRootConditionalStyle
                                    #ENDBOXED
                                  #ENDBOXED
                                #ENDBUTTON
                              #ENDBOXED
                            #ENDBOXED
                            #BOXED,WHERE(~%ControlHasStyle),AT(,0)
                              #DISPLAY('Your MultiChildRelationTree control is not set to use custom styles. To activate custom styles for your MultiChildRelationTree control, follow these steps:'),AT(,,168,25)
                              #DISPLAY('1.  If you aren''t in the window formatter, go there.'),AT(,,168)
                              #DISPLAY('2.  Right-click on the MultiChildRelationTree control, and select "List Box Format..."'),AT(,,168,15)
                              #DISPLAY('3.  Select the field you want to stylize, and select the "General" tab.'),AT(,,168,15)
                              #DISPLAY('4.  Check the "Style" checkbox.'),AT(,,168)
                              #DISPLAY('5.  Repeat steps 3 and 4 as necessary.'),AT(,,168)
                              #DISPLAY('6.  When you call up the TreeControl actions window, this tab will let you set the styles.'),AT(,,168,20)
                            #ENDBOXED
                          #ENDBOXED
                        #ENDTAB
                      #ENDSHEET
                    #ENDBUTTON
                  #ENDENABLE
                #ENDBOXED
              #ENDBOXED
            #ENDTAB
            #TAB('Icons'),HLP('~IDH_PrimaryIcons')
              #BOXED('Icons'),SECTION
                #BOXED,WHERE(%ControlHasIcon),AT(,0)
                  #ENABLE(%FileHasChildren(%Secondary)),CLEAR
                    #PROMPT('Use different icons for Expand and Contract',CHECK),%MCRTChildUseExpandAndContractIcon,DEFAULT(%False),AT(10)
                  #ENDENABLE
                  #BOXED('Default Icon'),SECTION
                    #BOXED,WHERE(NOT %MCRTChildUseExpandAndContractIcon),AT(,0)
                      #PROMPT('Icon Type:',DROP('Built-in[Builtin]|File|Variable|External')),%MCRTChildDefaultIconType,DEFAULT('File')
                      #BOXED,SECTION
                        #BOXED,WHERE(%MCRTChildDefaultIconType = 'Builtin'),AT(,0)
                          #PROMPT('Icon (ICON:*):',FROM(%MCRTBuiltinIcons)),%MCRTChildDefaultIconB,DEFAULT('None')
                        #ENDBOXED
                        #BOXED,WHERE(%MCRTChildDefaultIconType = 'File'),AT(,0)
                          #PROMPT('Icon (*.ico):',ICON('Select ChildRoot Level Icon','Icon files (*.ico)|*.ico')),%MCRTChildDefaultIconF
                        #ENDBOXED
                        #BOXED,WHERE(%MCRTChildDefaultIconType = 'Variable'),AT(,0)
                          #PROMPT('Icon (!Variable):',OPTFIELD),%MCRTChildDefaultIconV
                          #VALIDATE(SUB(%MCRTChildDefaultIconV,1,1) = '!','An exclamation sign must be preceded!')
                        #ENDBOXED
                        #BOXED,WHERE(%MCRTChildDefaultIconType = 'External'),AT(,0)
                          #PROMPT('Icon (*.ico[i]):',@s65),%MCRTChildDefaultIconE
                          #VALIDATE(SUB(%MCRTChildDefaultIconE,LEN(%MCRTChildDefaultIconE),1) = ']','An order number of icon must be specified in the squared brackets! Example: icon.ico[1]')
                          #VALIDATE(INSTRING('.ico[',%MCRTChildDefaultIconE,1,1),'An order number of icon must be specified in the squared brackets! Example: icon.ico[1]')
                        #ENDBOXED
                      #ENDBOXED
                    #ENDBOXED
                    #BOXED,WHERE(%MCRTChildUseExpandAndContractIcon),AT(,0)
                      #BOXED('Contracted')
                        #PROMPT('Icon Type:',DROP('Built-in[Builtin]|File|Variable|External')),%MCRTChildDefaultContractedIconType,DEFAULT('File')
                        #BOXED,SECTION
                          #BOXED,WHERE(%MCRTChildDefaultContractedIconType = 'Builtin'),AT(,0)
                            #PROMPT('Icon (ICON:*):',FROM(%MCRTBuiltinIcons)),%MCRTChildDefaultContractedIconB,DEFAULT('None')
                          #ENDBOXED
                          #BOXED,WHERE(%MCRTChildDefaultContractedIconType = 'File'),AT(,0)
                            #PROMPT('Icon (*.ico):',ICON('Select Primary Level Icon','Icon files (*.ico)|*.ico')),%MCRTChildDefaultContractedIconF
                          #ENDBOXED
                          #BOXED,WHERE(%MCRTChildDefaultContractedIconType = 'Variable'),AT(,0)
                            #PROMPT('Icon (!Variable):',OPTFIELD),%MCRTChildDefaultContractedIconV
                            #VALIDATE(LEN(%MCRTChildDefaultContractedIconV) > 0 AND SUB(%MCRTChildDefaultContractedIconV,1,1) = '!','An exclamation sign must be preceded!')
                          #ENDBOXED
                          #BOXED,WHERE(%MCRTChildDefaultContractedIconType = 'External'),AT(,0)
                            #PROMPT('Icon (*.ico[i]):',@s65),%MCRTChildDefaultContractedIconE
                            #VALIDATE(SUB(%MCRTChildDefaultContractedIconE,LEN(%MCRTChildDefaultContractedIconE),1) = ']','An order number of icon must be specified in the squared brackets! Example: icon.ico[1]')
                            #VALIDATE(INSTRING('.ico[',%MCRTChildDefaultContractedIconE,1,1),'An order number of icon must be specified in the squared brackets! Example: icon.ico[1]')
                          #ENDBOXED
                        #ENDBOXED
                      #ENDBOXED
                      #BOXED('Expanded')
                        #PROMPT('Icon Type:',DROP('Built-in[Builtin]|File|Variable|External')),%MCRTChildDefaultExpandedIconType,DEFAULT('File')
                        #BOXED,SECTION
                          #BOXED,WHERE(%MCRTChildDefaultExpandedIconType = 'Builtin'),AT(,0)
                            #PROMPT('Icon (ICON:*):',FROM(%MCRTBuiltinIcons)),%MCRTChildDefaultExpandedIconB,DEFAULT('None')
                          #ENDBOXED
                          #BOXED,WHERE(%MCRTChildDefaultExpandedIconType = 'File'),AT(,0)
                            #PROMPT('Icon (*.ico):',ICON('Select Primary Level Icon','Icon files (*.ico)|*.ico')),%MCRTChildDefaultExpandedIconF
                          #ENDBOXED
                          #BOXED,WHERE(%MCRTChildDefaultExpandedIconType = 'Variable'),AT(,0)
                            #PROMPT('Icon (!Variable):',OPTFIELD),%MCRTChildDefaultExpandedIconV
                            #VALIDATE(LEN(%MCRTChildDefaultExpandedIconV) > 0 AND SUB(%MCRTChildDefaultExpandedIconV,1,1) = '!','An exclamation sign must be preceded!')
                          #ENDBOXED
                          #BOXED,WHERE(%MCRTChildDefaultExpandedIconType = 'External'),AT(,0)
                            #PROMPT('Icon (*.ico[i]):',@s65),%MCRTChildDefaultExpandedIconE
                            #VALIDATE(SUB(%MCRTChildDefaultExpandedIconE,LEN(%MCRTChildDefaultExpandedIconE),1) = ']','An order number of icon must be specified in the squared brackets! Example: icon.ico[1]')
                            #VALIDATE(INSTRING('.ico[',%MCRTChildDefaultExpandedIconE,1,1),'An order number of icon must be specified in the squared brackets! Example: icon.ico[1]')
                          #ENDBOXED
                        #ENDBOXED
                      #ENDBOXED
                    #ENDBOXED
                  #ENDBOXED
                  #BOXED('Conditional Icons')
                    #BUTTON('Conditional Icons...'),MULTI(%MCRTChildConditionalIcons,'IF ' & %MCRTChildIconCondition & ' - ' & %SetConditionalIcon('child')),INLINE
                      #PROMPT('Condition:',EXPR),%MCRTChildIconCondition,REQ
                      #BOXED,SECTION
                        #BOXED,WHERE(NOT %MCRTChildUseExpandAndContractIcon),AT(,0)
                          #PROMPT('Icon Type:',DROP('Built-in[Builtin]|File|Variable|External')),%MCRTChildConditionalIconType,DEFAULT('File')
                          #BOXED,SECTION
                            #BOXED,WHERE(%MCRTChildConditionalIconType = 'Builtin'),AT(,0)
                              #PROMPT('Icon (ICON:*):',FROM(%MCRTBuiltinIcons)),%MCRTChildConditionalIconB,DEFAULT('None')
                            #ENDBOXED
                            #BOXED,WHERE(%MCRTChildConditionalIconType = 'File'),AT(,0)
                              #PROMPT('Icon (*.ico):',ICON('Select ChildRoot Level Icon','Icon files (*.ico)|*.ico')),%MCRTChildConditionalIconF
                            #ENDBOXED
                            #BOXED,WHERE(%MCRTChildConditionalIconType = 'Variable'),AT(,0)
                              #PROMPT('Icon (!Variable):',OPTFIELD),%MCRTChildConditionalIconV
                              #VALIDATE(LEN(%MCRTChildConditionalIconV) > 0 AND SUB(%MCRTChildConditionalIconV,1,1) = '!','An exclamation sign must be preceded!')
                            #ENDBOXED
                            #BOXED,WHERE(%MCRTChildConditionalIconType = 'External'),AT(,0)
                              #PROMPT('Icon (*.ico[i]):',@s65),%MCRTChildConditionalIconE
                              #VALIDATE(SUB(%MCRTChildConditionalIconE,LEN(%MCRTChildConditionalIconE),1) = ']','An order number of icon must be specified in the squared brackets! Example: icon.ico[1]')
                              #VALIDATE(INSTRING('.ico[',%MCRTChildConditionalIconE,1,1),'An order number of icon must be specified in the squared brackets! Example: icon.ico[1]')
                            #ENDBOXED
                          #ENDBOXED
                        #ENDBOXED
                        #BOXED,WHERE(%MCRTChildUseExpandAndContractIcon),AT(,0)
                          #BOXED('Contracted')
                            #PROMPT('Icon Type:',DROP('Built-in[Builtin]|File|Variable|External')),%MCRTChildConditionalContractedIconType,DEFAULT('File')
                            #BOXED,SECTION
                              #BOXED,WHERE(%MCRTChildConditionalContractedIconType = 'Builtin'),AT(,0)
                                #PROMPT('Icon (ICON:*):',FROM(%MCRTBuiltinIcons)),%MCRTChildConditionalContractedIconB,DEFAULT('None')
                              #ENDBOXED
                              #BOXED,WHERE(%MCRTChildConditionalContractedIconType = 'File'),AT(,0)
                                #PROMPT('Icon (*.ico):',ICON('Select ChildRoot Level Icon','Icon files (*.ico)|*.ico')),%MCRTChildConditionalContractedIconF
                              #ENDBOXED
                              #BOXED,WHERE(%MCRTChildConditionalContractedIconType = 'Variable'),AT(,0)
                                #PROMPT('Icon (!Variable):',OPTFIELD),%MCRTChildConditionalContractedIconV
                                #VALIDATE(LEN(%MCRTChildConditionalContractedIconV) > 0 AND SUB(%MCRTChildConditionalContractedIconV,1,1) = '!','An exclamation sign must be preceded!')
                              #ENDBOXED
                              #BOXED,WHERE(%MCRTChildConditionalContractedIconType = 'External'),AT(,0)
                                #PROMPT('Icon (*.ico[i]):',@s65),%MCRTChildConditionalContractedIconE
                                #VALIDATE(SUB(%MCRTChildConditionalContractedIconE,LEN(%MCRTChildConditionalContractedIconE),1) = ']','An order number of icon must be specified in the squared brackets! Example: icon.ico[1]')
                                #VALIDATE(INSTRING('.ico[',%MCRTChildConditionalContractedIconE,1,1),'An order number of icon must be specified in the squared brackets! Example: icon.ico[1]')
                              #ENDBOXED
                            #ENDBOXED
                          #ENDBOXED
                          #BOXED('Expanded')
                            #PROMPT('Icon Type:',DROP('Built-in[Builtin]|File|Variable|External')),%MCRTChildConditionalExpandedIconType,DEFAULT('File')
                            #BOXED,SECTION
                              #BOXED,WHERE(%MCRTChildConditionalExpandedIconType = 'Builtin'),AT(,0)
                                #PROMPT('Icon (ICON:*):',FROM(%MCRTBuiltinIcons)),%MCRTChildConditionalExpandedIconB,DEFAULT('None')
                              #ENDBOXED
                              #BOXED,WHERE(%MCRTChildConditionalExpandedIconType = 'File'),AT(,0)
                                #PROMPT('Icon (*.ico):',ICON('Select ChildRoot Level Icon','Icon files (*.ico)|*.ico')),%MCRTChildConditionalExpandedIconF
                              #ENDBOXED
                              #BOXED,WHERE(%MCRTChildConditionalExpandedIconType = 'Variable'),AT(,0)
                                #PROMPT('Icon (!Variable):',OPTFIELD),%MCRTChildConditionalExpandedIconV
                                #VALIDATE(LEN(%MCRTChildConditionalExpandedIconV) > 0 AND SUB(%MCRTChildConditionalExpandedIconV,1,1) = '!','An exclamation sign must be preceded!')
                              #ENDBOXED
                              #BOXED,WHERE(%MCRTChildConditionalExpandedIconType = 'External'),AT(,0)
                                #PROMPT('Icon (*.ico[i]):',@s65),%MCRTChildConditionalExpandedIconE
                                #VALIDATE(SUB(%MCRTChildConditionalExpandedIconE,LEN(%MCRTChildConditionalExpandedIconE),1) = ']','An order number of icon must be specified in the squared brackets! Example: icon.ico[1]')
                                #VALIDATE(INSTRING('.ico[',%MCRTChildConditionalExpandedIconE,1,1),'An order number of icon must be specified in the squared brackets! Example: icon.ico[1]')
                              #ENDBOXED
                            #ENDBOXED
                          #ENDBOXED
                        #ENDBOXED
                      #ENDBOXED
                    #ENDBUTTON
                  #ENDBOXED
                #ENDBOXED
                #BOXED,WHERE(~%ControlHasIcon),AT(,0)
                  #DISPLAY('Your MultiChildRelationTree control is not set to use custom icons. To activate custom icons for your MultiChildRelationTree control, follow these steps:'),AT(,,168,25)
                  #DISPLAY('1.  If you aren''t in the window formatter, go there.'),AT(,,168)
                  #DISPLAY('2.  Right-click on the MultiChildRelationTree control, and select "List Box Format..."'),AT(,,168,15)
                  #DISPLAY('3.  Select the column you want to use an icon with from the left hand list.'),AT(10,,175,16)
                  #DISPLAY('4.  Set the "Icon" property to either "Normal" or "Transparent".'),AT(10,,175,16)
                  #DISPLAY('5.  Repeat steps 3-4 as necessary.'),AT(10,,175)
                  #DISPLAY('6.  When you call up the MultiChildRelationTree control actions window, this tab will let you set the icons.'),AT(,,172,20)
                #ENDBOXED
              #ENDBOXED
            #ENDTAB
            #TAB('Colors'),HLP('~IDH_PrimaryColors')
              #BOXED('Colors'),SECTION
                #BOXED,WHERE(%ControlHasColor),AT(,0)
                  #BOXED('Default Colors')
                    #PROMPT('Foreground Normal:',COLOR),%MCRTChildNormalFGColor,DEFAULT(-1)
                    #PROMPT('Background Normal:',COLOR),%MCRTChildNormalBGColor,DEFAULT(-1)
                    #PROMPT('Foreground Selected:',COLOR),%MCRTChildSelectedFGColor,DEFAULT(-1)
                    #PROMPT('Background Selected:',COLOR),%MCRTChildSelectedBGColor,DEFAULT(-1)
                  #ENDBOXED
                  #BOXED('Conditional Colors')
                    #BUTTON('Conditional Colors...'),MULTI(%MCRTChildConditionalColors,%MCRTChildColorCondition),INLINE
                      #PROMPT('Condition:',EXPR),%MCRTChildColorCondition,REQ
                      #PROMPT('Foreground Normal:',COLOR),%MCRTChildNormalFGConditionalColor,DEFAULT(-1)
                      #PROMPT('Background Normal:',COLOR),%MCRTChildNormalBGConditionalColor,DEFAULT(-1)
                      #PROMPT('Foreground Selected:',COLOR),%MCRTChildSelectedFGConditionalColor,DEFAULT(-1)
                      #PROMPT('Background Selected:',COLOR),%MCRTChildSelectedBGConditionalColor,DEFAULT(-1)
                    #ENDBUTTON
                  #ENDBOXED
                #ENDBOXED
                #BOXED,WHERE(~%ControlHasColor),AT(,0)
                  #DISPLAY('Your MultiChildRelationTree control is not set to use custom colors. To activate custom colors for your MultiChildRelationTree control, follow these steps:'),AT(,,168,25)
                  #DISPLAY('1.  If you aren''t in the window formatter, go there.'),AT(,,168)
                  #DISPLAY('2.  Right-click on the MultiChildRelationTree control, and select "List Box Format..."'),AT(,,168,15)
                  #DISPLAY('3.  Select the field you want to colorize, and select the "General" tab.'),AT(,,168,15)
                  #DISPLAY('4.  Check the "Color" checkbox.'),AT(,,168)
                  #DISPLAY('5.  Repeat steps 3 and 4 as necessary.'),AT(,,168)
                  #DISPLAY('6.  When you call up the TreeControl actions window, this tab will let you set the colors.'),AT(,,168,20)
                #ENDBOXED
              #ENDBOXED
            #ENDTAB
            #TAB('Styles'),HLP('~IDH_PrimaryStyles')
              #BOXED('Styles'),SECTION
                #BOXED,WHERE(%ControlHasStyle),AT(,0)
                  #BOXED('Default Style')
                    #PROMPT('Style Type:',DROP('Local List|Style Number')),%MCRTChildStyleType,DEFAULT('Local List')
                    #BOXED,SECTION
                      #BOXED,WHERE(%MCRTChildStyleType='Local List'),AT(0,0)
                        #PROMPT('Style:',FROM(%WindowStyles,,INSTANCE(%WindowStyles)&'-'&CHOOSE(%WindowStyleFontNameIsVariable,%WindowStyleFontNameVariable,%WindowStyleFontName)&','&CHOOSE(%WindowStyleFontSizeIsVariable,%WindowStyleFontSizeVariable,%WindowStyleFontSize))),%MCRTChildStyleLocalList
                      #ENDBOXED
                      #BOXED,WHERE(%MCRTChildStyleType='Style Number'),AT(0,0)
                        #PROMPT('Style Number:',SPIN(@n6,0,65535)),%MCRTChildStyle
                      #ENDBOXED
                    #ENDBOXED
                  #ENDBOXED
                  #BOXED('Conditional Styles')
                    #BUTTON('&Conditional Styles'),MULTI(%MCRTChildConditionalStyles,%MCRTChildStyleCondition),INLINE,HLP('~TPLControlBrowseBox_Styles')
                      #PROMPT('&Condition:',EXPR),%MCRTChildStyleCondition,REQ
                      #PROMPT('Style Type:',DROP('Local List|Style Number')),%MCRTChildConditionalStyleType,DEFAULT('Local List')
                      #BOXED,SECTION
                        #BOXED,WHERE(%MCRTChildConditionalStyleType='Local List'),AT(0,0)
                          #PROMPT('Style:',FROM(%WindowStyles,,INSTANCE(%WindowStyles)&'-'&CHOOSE(%WindowStyleFontNameIsVariable,%WindowStyleFontNameVariable,%WindowStyleFontName)&','&CHOOSE(%WindowStyleFontSizeIsVariable,%WindowStyleFontSizeVariable,%WindowStyleFontSize))),%MCRTChildConditionalStyleLocalList
                        #ENDBOXED
                        #BOXED,WHERE(%MCRTChildConditionalStyleType='Style Number'),AT(0,0)
                          #PROMPT('Style Number:',SPIN(@n6,0,65535)),%MCRTChildConditionalStyle
                        #ENDBOXED
                      #ENDBOXED
                    #ENDBUTTON
                  #ENDBOXED
                #ENDBOXED
                #BOXED,WHERE(~%ControlHasStyle),AT(,0)
                  #DISPLAY('Your MultiChildRelationTree control is not set to use custom styles. To activate custom styles for your MultiChildRelationTree control, follow these steps:'),AT(,,168,25)
                  #DISPLAY('1.  If you aren''t in the window formatter, go there.'),AT(,,168)
                  #DISPLAY('2.  Right-click on the MultiChildRelationTree control, and select "List Box Format..."'),AT(,,168,15)
                  #DISPLAY('3.  Select the field you want to stylize, and select the "General" tab.'),AT(,,168,15)
                  #DISPLAY('4.  Check the "Style" checkbox.'),AT(,,168)
                  #DISPLAY('5.  Repeat steps 3 and 4 as necessary.'),AT(,,168)
                  #DISPLAY('6.  When you call up the TreeControl actions window, this tab will let you set the styles.'),AT(,,168,20)
                #ENDBOXED
              #ENDBOXED
            #ENDTAB
            #TAB('&Hot Fields'),HLP('~TPLControlBrowseBox_Hot_Fields')
              #BUTTON('"Hot" Fields'),MULTI(%MCRTSecondaryHotFields,%MCRTSecondaryHotField),INLINE,HLP('~TPLControlBrowseBox_Hot_Fields')
                #PROMPT('Hot Field:',FIELD(%Secondary)),%MCRTSecondaryHotField,REQ
              #ENDBUTTON
            #ENDTAB
          #ENDSHEET
        #ENDBUTTON
      #ENDBOXED
    #ENDTAB
  #ENDSHEET
#ENDBUTTON
#!=============================================================================
#!
#!=============================================================================
#ATSTART
#!SET(%MCRTRootUseSetupCapability,%False)
#IF(UPPER(%AppTemplateFamily)='ABC')
#EQUATE(%WindowManagerObject,'ThisWindow')
#CALL(%GetObjectName(ABC),'Default',0),%WindowManagerObject
#ENDIF
#DECLARE(%IsFirstInstance)
#SET(%IsFirstInstance,(INLIST(%ActiveTemplate,%ActiveTemplate)=INSTANCE(%ActiveTemplate)) AND (INSTANCE(%ActiveTemplateInstance)=1))
  #IF(UPPER(%AppTemplateFamily)='CLARION')
  #INSERT(%FileControlInitialize(Clarion))          #! Opening/closing files
  #FOR(%Secondary),WHERE(%SecondaryType ='')    #! Check custom joins for joins to 'non-related' files
    #FIX(%File, %Primary)
    #IF(~INLIST(%Secondary, %Relation))         #! Secondary is not formally related to Primary so will need explicit Open/Close calls
      #ADD(%ProcFilesUsed, %Secondary)
    #ENDIF
  #ENDFOR
  #ELSE
  #INSERT(%FileControlInitialize(ABC))          #! Opening/closing files
  #FOR(%Secondary),WHERE(%SecondaryType ='')    #! Check custom joins for joins to 'non-related' files
    #FIX(%File, %Primary)
    #IF(~INLIST(%Secondary, %Relation))         #! Secondary is not formally related to Primary so will need explicit Open/Close calls
      #ADD(%ProcFilesUsed, %Secondary)
    #ENDIF
  #ENDFOR
  #ENDIF
  #EQUATE(%CheckChildRootCondition,%False)
  #DECLARE(%CheckChildRootConditionItems)
#!-----------------------------------------------------------------------------
#! Prepare translator
#!-----------------------------------------------------------------------------
  #!DECLARE(%MCRTTranslatingItems),MULTI
  #!DECLARE(%MCRTTranslation,%MCRTTranslatingItems)
  #!CALL(%PrepareTranslator)
#!-----------------------------------------------------------------------------
#! Declare neccessary variables
#!-----------------------------------------------------------------------------
  #DECLARE(%mInstancePrefix)                        #! Template instance prefix
  #DECLARE(%mTempVariable)                          #! Just a temporary variable
  #DECLARE(%mTempLevel,LONG)                        #! The temporary level variable
  #DECLARE(%mNextLevel,LONG)                        #! The next level variable
  #DECLARE(%mTempInstance,LONG)                     #! The temporary instance
  #DECLARE(%mTempChildRoot)                         #! The temporary ChildRoot sign
  #DECLARE(%mChildRootExist)                        #! Sign of ChildRoot existance
  #DECLARE(%mTreeControl)                           #! Tree control
  #DECLARE(%mTreeQueue)                             #! Tree queue equate
  #DECLARE(%mTreeStateQueue)                        #! Tree state queue equate
  #DECLARE(%mTempFontName)                          #! Font name for a style
#!-----------------------------------------------------------------------------
#! Declare icon list queue
#!-----------------------------------------------------------------------------
  #DECLARE(%IconList),UNIQUE                        #! Tree icon list
  #DECLARE(%IconListType,%IconList)                 #! Tree icon storage type
#!-----------------------------------------------------------------------------
#! Declare main work queue
#!-----------------------------------------------------------------------------
  #DECLARE(%mFileTreeList),MULTI                    #! The main work queue
  #DECLARE(%mFileTreeFile,%mFileTreeList)           #! File
  #DECLARE(%mFileTreeLevel,%mFileTreeList,LONG)     #! File level
  #DECLARE(%mFileTreeDisplay,%mFileTreeList)        #! Variable to display
  #DECLARE(%mFileTreeParent,%mFileTreeList)         #! Parent file for a level
  #DECLARE(%mFileTreeChildren,%mFileTreeList,LONG)  #! Quantity of children
  #DECLARE(%mFileTreeKey,%mFileTreeList)            #! KEY reference
  #DECLARE(%mFileTreeCustomJoin,%mFileTreeList)     #! If the relation is a custom join this will be True
  #DECLARE(%mFileTreeCustomJoinField,%mFileTreeList),MULTI #! If the relation is a custom it will store the component field asignemnt type
  #DECLARE(%mFileTreeCustomJoinValueType,%mFileTreeCustomJoinField)#! If the relation is a custom it will store the component field asignemnt type
  #DECLARE(%mFileTreeCustomJoinValue,%mFileTreeCustomJoinField)     #! If the relation is a custom it will store the component field asignemnt
  #DECLARE(%mFileTreeTitle,%mFileTreeList)          #! File level title
  #DECLARE(%mFileTreeChildRoot,%mFileTreeList)      #! File level ChildRoot
  #DECLARE(%mFileTreeIsPrimary,%mFileTreeList)      #! Is current file primary?
  #DECLARE(%mFileTreeOneToManyChild,%mFileTreeList),MULTI  #! Is current file primary?
#!-----------------------------------------------------------------------------
#! Declare the queue containing all available tree level numbers
#!-----------------------------------------------------------------------------
  #DECLARE(%mThereIsSorting)                        #! Will be true if there is any Additional sorting field
  #DECLARE(%mLevelList,LONG),UNIQUE                 #! Queue of all levels
  #DECLARE(%ExpandPopupText)
  #DECLARE(%ContractPopupText)
  #DECLARE(%InsertPopupText)
  #DECLARE(%ChangePopupText)
  #DECLARE(%DeletePopupText)
  #DECLARE(%PopupMenu),UNIQUE
  #DECLARE(%PopupMenuName,%PopupMenu)
  #DECLARE(%PopupMenuText,%PopupMenu)
  #DECLARE(%PopupMenuConditionToEnable,%PopupMenu)
  #DECLARE(%PopupMenuConditionToShow,%PopupMenu)
  #DECLARE(%PopupMenuAction,%PopupMenu)
  #DECLARE(%PopupMenuSeparation,%PopupMenu)
#!-----------------------------------------------------------------------------
#! Set instance prefix
#!-----------------------------------------------------------------------------
  #SET(%mInstancePrefix,'MCRT' & %ActiveTemplateInstance & ':')
#!-----------------------------------------------------------------------------
#! Set the tree queues
#!-----------------------------------------------------------------------------
  #FOR(%Control),WHERE(%ControlInstance = %ActiveTemplateInstance)
    #SET(%mTreeControl,%Control)
    #SET(%mTreeQueue,EXTRACT(%ControlStatement,'FROM',1))
    #SET(%mTreeStateQueue,SUB(%mTreeQueue,1,INSTRING(':',%mTreeQueue,1,1)) & 'LoadedQueue')
  #ENDFOR
#!-----------------------------------------------------------------------------
#! Add the first level
#!-----------------------------------------------------------------------------
  #FIX(%Control,%mTreeControl)
#IF(NOT %DisablePrimaryLevel)
  #FIX(%File,%Primary)
  #ADD(%mFileTreeList,ITEMS(%mFileTreeList) + 1)
  #SET(%mFileTreeFile,%Primary)
  #SET(%mFileTreeLevel,1)
  #IF(%MCRTPrimaryLevelTitle)
    #SET(%mFileTreeDisplay,%ConvertData(%MCRTPrimaryLevelTitle) & ' & ' & %ConvertData(%MCRTPrimaryDisplay))
  #ELSE
    #SET(%mFileTreeDisplay,%ConvertData(%MCRTPrimaryDisplay))
  #ENDIF
  #SET(%mFileTreeParent,%False)
  #IF(%PrimaryKey)
    #SET(%mFileTreeKey,%PrimaryKey)
  #ELSE
    #SET(%mFileTreeKey,%False)
  #ENDIF
  #SET(%mFileTreeTitle,%MCRTPrimaryLevelTitle)
  #SET(%mFileTreeChildRoot,%False)
  #SET(%mFileTreeIsPrimary,%True)
#!-----------------------------------------------------------------------------
#! Count "children" number for primary level
#!-----------------------------------------------------------------------------
  #SET(%mFileTreeChildren,0)
  #FOR(%Secondary),WHERE(%SecondaryTo = %mFileTreeFile)
    #SET(%mFileTreeChildren,%mFileTreeChildren + 1)
  #ENDFOR
#ENDIF
#!-----------------------------------------------------------------------------
#! Add the other level(s)
#!-----------------------------------------------------------------------------
  #FOR(%Secondary)
  #!,WHERE()
    #IF(%SecondaryType = '1:MANY')
#!-----------------------------------------------------------------------------
#! Emulate #FIX(%mFileTreeList,%SecondaryTo)
#!-----------------------------------------------------------------------------
       #IF(%mFileTreeList <> %SecondaryTo)
          #FOR(%mFileTreeList)
            #IF(%mFileTreeFile = %SecondaryTo)
              #BREAK
            #ENDIF
          #ENDFOR
       #ENDIF
       #IF(%mFileTreeFile = %SecondaryTo)
          #ADD(%mFileTreeOneToManyChild,%Secondary)
       #ENDIF
    #ELSE
       #IF(%MCRTChildRootLevelName)
         #SET(%mChildRootExist,%True)
       #ELSE
         #SET(%mChildRootExist,%False)
       #ENDIF
#!-----------------------------------------------------------------------------
#! Emulate #FIX(%mFileTreeList,%SecondaryTo)
#!-----------------------------------------------------------------------------
       #FOR(%mFileTreeList),WHERE(~%mFileTreeChildRoot)
         #IF(%mFileTreeFile = %SecondaryTo)
           #SET(%mTempInstance,INSTANCE(%mFileTreeList))
           #BREAK
         #ENDIF
       #ENDFOR
       #SELECT(%mFileTreeList,%mTempInstance)
       #FIX(%File,%Secondary)
       #FIX(%Relation,%SecondaryTo)
       #IF(%DisablePrimaryLevel)
           #IF(%SecondaryTo=%Primary)
               #SET(%mNextLevel,1)
           #ELSE
               #SET(%mNextLevel,%mFileTreeLevel + 1)
           #ENDIF
       #ELSE
           #SET(%mNextLevel,%mFileTreeLevel + 1)
       #ENDIF
       #IF(%mChildRootExist)
         #SELECT(%mFileTreeList,ITEMS(%mFileTreeList))
         #ADD(%mFileTreeList,ITEMS(%mFileTreeList) + 1)
         #SET(%mFileTreeFile,%Secondary)
         #SET(%mFileTreeLevel,%mNextLevel)
         #SET(%mFileTreeDisplay,%ConvertData(%MCRTChildRootLevelName))
         #SET(%mFileTreeParent,%SecondaryTo)
         #IF(%SecondaryType = 'MANY:1')
            #SET(%mFileTreeKey,%FileKey)
            #SET(%mFileTreeCustomJoin,%False)
         #ELSE
            #SET(%mFileTreeKey,%MCRTChildCustomJoinKey)
            #SET(%mFileTreeCustomJoin,%True)
            #FOR(%MCRTChildCustomJoinKeyComponents)
               #ADD(%mFileTreeCustomJoinField,%MCRTChildCustomJoinKeyComponent)
               #SET(%mFileTreeCustomJoinValueType,%MCRTChildCustomJoinKeyComponentValueType)
               #CASE(%MCRTChildCustomJoinKeyComponentValueType)
               #OF(1)
                  #SET(%mFileTreeCustomJoinValue,'')
               #OF(2)
                  #SET(%mFileTreeCustomJoinValue,%MCRTChildCustomJoinKeyComponentValueVariable)
               #OF(3)
                  #SET(%mFileTreeCustomJoinValue,%MCRTChildCustomJoinKeyComponentValueFixed)
               #ENDCASE
            #ENDFOR
         #ENDIF
         #SET(%mFileTreeTitle,%MCRTChildLevelTitle)
         #SET(%mFileTreeChildRoot,%True)
         #SET(%mFileTreeIsPrimary,%False)
         #SET(%mFileTreeChildren,1)
         #SET(%mNextLevel,%mNextLevel + 1)
       #ENDIF
       #SELECT(%mFileTreeList,ITEMS(%mFileTreeList))
       #ADD(%mFileTreeList,ITEMS(%mFileTreeList) + 1)
       #SET(%mFileTreeFile,%Secondary)
       #SET(%mFileTreeLevel,%mNextLevel)
       #IF(%MCRTChildLevelTitle)
         #SET(%mFileTreeDisplay,%ConvertData(%MCRTChildLevelTitle) & ' & ' & %ConvertData(%MCRTChildDisplay))
       #ELSE
         #SET(%mFileTreeDisplay,%ConvertData(%MCRTChildDisplay))
       #ENDIF
       #SET(%mFileTreeParent,%SecondaryTo)
       #IF(%SecondaryType = 'MANY:1')
          #SET(%mFileTreeKey,%FileKey)
          #SET(%mFileTreeCustomJoin,%False)
       #ELSE
          #SET(%mFileTreeKey,%MCRTChildCustomJoinKey)
          #SET(%mFileTreeCustomJoin,%True)
          #FOR(%MCRTChildCustomJoinKeyComponents)
             #ADD(%mFileTreeCustomJoinField,%MCRTChildCustomJoinKeyComponent)
             #SET(%mFileTreeCustomJoinValueType,%MCRTChildCustomJoinKeyComponentValueType)
             #CASE(%MCRTChildCustomJoinKeyComponentValueType)
             #OF(1)
                #SET(%mFileTreeCustomJoinValue,'')
             #OF(2)
                #SET(%mFileTreeCustomJoinValue,%MCRTChildCustomJoinKeyComponentValueVariable)
             #OF(3)
                #SET(%mFileTreeCustomJoinValue,%MCRTChildCustomJoinKeyComponentValueFixed)
             #ENDCASE
          #ENDFOR
       #ENDIF
       #SET(%mFileTreeTitle,%MCRTChildLevelTitle)
       #SET(%mFileTreeChildRoot,%False)
       #SET(%mFileTreeIsPrimary,%False)
#!-----------------------------------------------------------------------------
#! Count "children" number for primary level
#!-----------------------------------------------------------------------------
       #SET(%mFileTreeChildren,0)
       #FOR(%Secondary),WHERE(%SecondaryTo = %mFileTreeFile)
         #SET(%mFileTreeChildren,%mFileTreeChildren + 1)
       #ENDFOR
    #ENDIF
  #ENDFOR
#!-----------------------------------------------------------------------------
#! Fill queue with all available levels
#!-----------------------------------------------------------------------------
  #FOR(%mFileTreeList)
    #FIX(%mLevelList,%mFileTreeLevel)
    #IF(~%mLevelList)
      #ADD(%mLevelList,%mFileTreeLevel)
    #ENDIF
  #ENDFOR
#!-----------------------------------------------------------------------------
#! Fill icon list
#!-----------------------------------------------------------------------------
  #IF(%ControlHasIcon)
    #INSERT(%FillIconList)
  #ENDIF
#ENDAT
#!-----------------------------------------------------------------------------
#AT(%CustomGlobalDeclarations)
  #INSERT(%FileControlSetFlags(Clarion))
  #FIND(%ControlInstance,%ActiveTemplateInstance,%Control)
#!-----------------------------------------------------------------------------
#! Add icons to project
#!-----------------------------------------------------------------------------
  #IF(%ControlHasIcon)
    #INSERT(%AddIconsToProject)
  #ENDIF
  #!PROJECT('mcrtree.ico')
#ENDAT
#!-----------------------------------------------------------------------------
#!#AT(%CustomModuleDeclarations)
#!INCLUDE('MCRTREE%ActiveTemplateInstance.EQU'),ONCE
#!#ENDAT
#!-----------------------------------------------------------------------------
#AT(%CustomModuleDeclarations),WHERE(%MCRTRootUseSetupCapability)
  #DECLARE(%mInstancePrefixAlias)
  #SET(%mInstancePrefixAlias,'MCRT' & %ActiveTemplateInstance & ':')
  #ADD(%CustomModuleMapModule,'CURRENT MODULE')
  #ADD(%CustomModuleMapProcedure,%mInstancePrefixAlias & ':SetupTreeParams')
  #INSERT(%SetCustomModuleMapProcedurePrototype,'')
  #!ADD(%CustomModuleMapModule,'CURRENT MODULE')
  #!ADD(%CustomModuleMapProcedure,%mInstancePrefixAlias & ':RemoveIcon')
  #!INSERT(%SetCustomModuleMapProcedurePrototype,'(WINDOW _Wnd)')
#ENDAT
#!----------------------------------------------------------------------------
#AT(%CustomModuleDeclarations),WHERE(%AcceptToolbarControl AND UPPER(%AppTemplateFamily)='ABC')
  #CALL(%AddModuleIncludeFile(ABC),%ToolbarRelTreeType)
#ENDAT
#!----------------------------------------------------------------------------
#AT(%GenerateInstanceUpdate),WHERE(%AcceptToolbarControl AND %GenerateReltreeInstance=0)
  #SET(%GenerateReltreeInstance,%ActiveTemplateInstance)
#ENDAT
#!----------------------------------------------------------------------------
#AT(%DataSectionBeforeWindow)
  #COMMENT(51)
  #MESSAGE('Multi Child Relation tree initialization',3)
  #FIND(%ControlInstance,%ActiveTemplateInstance,%Control)
  #IF(%AcceptToolBarControl)
    #IF(UPPER(%AppTemplateFamily)='CLARION')
       #IF( %GenerateReltreeInstance = %ActiveTemplateInstance )
         #IF ( ~%GenerateBRWInstance )
ToolbarMode          UNSIGNED,AUTO
         #ENDIF
ActiveReltree        SIGNED
       #ENDIF
    #ELSE
%mInstancePrefix:Toolbar        CLASS(%ToolbarRelTreeType)
TakeEvent            PROCEDURE(<*LONG VCR>,WindowManager WM),VIRTUAL
  END
    #ENDIF
       #SET(%mTempVariable,%mInstancePrefix & ':SaveRecords')
%[20]mTempVariable LONG,AUTO
       #SET(%mTempVariable,%mInstancePrefix & ':SavePointer')
%[20]mTempVariable LONG,AUTO
    #SET(%mTempVariable,%mInstancePrefix & ':SaveLevel')
%[20]mTempVariable BYTE,AUTO
    #SET(%mTempVariable,%mInstancePrefix & ':LastInsertInstance')
%[22]mTempVariable LONG             #<! Last Inserted Level unique instance
  #ENDIF
  #SET(%mTempVariable,%mInstancePrefix & ':UpdatePopup')
%[20]mTempVariable STRING(255)      #<! String buffer for popup menu
  #SET(%mTempVariable,%mInstancePrefix & ':SubLevelExist')
%[20]mTempVariable BYTE,AUTO        #<! Flag of sublevel existence
  #SET(%mTempVariable,%mInstancePrefix & ':CurrentChoice')
%[20]mTempVariable LONG,AUTO        #<! Current choice
  #SET(%mTempVariable,%mInstancePrefix & ':CurrentLevel')
%[20]mTempVariable LONG,AUTO        #<! Current level
  #SET(%mTempVariable,%mInstancePrefix & ':ExpandAll')
  #IF(%MCRTRootInitialState = 'Contracted')
%[20]mTempVariable BYTE(0)          #<! Initial state flag (1 - if all levels are expanded)
  #ELSE
%[20]mTempVariable BYTE(1)          #<! Initial state flag (1 - if all levels are expanded)
  #ENDIF
  #SET(%mTempVariable,%mInstancePrefix & ':Popup')
%[20]mTempVariable STRING(2048)      #<! String buffer for popup menu
  #SET(%mTempVariable,%mInstancePrefix & ':PopupSelection')
%[20]mTempVariable SHORT             #<! Selected popup item
  #SET(%mTempVariable,%mInstancePrefix & ':NewItemLevel')
%[20]mTempVariable LONG             #<! Queue position of scroll thumb
  #SET(%mTempVariable,%mInstancePrefix & ':NewItemPosition')
%[20]mTempVariable STRING(512)      #<! Queue position of scroll thumb
  #SET(%mTempVariable,%mInstancePrefix & ':ForceReload')
%[20]mTempVariable BYTE(1)
  #SET(%mTempVariable,%mInstancePrefix & ':Action')
%[20]mTempVariable LONG,AUTO
#!-----------------------------------------------------------------------------
#! Tree queue
#!-----------------------------------------------------------------------------
  #SET(%mTempVariable,%mInstancePrefix & ':QueueType')
%[20]mTempVariable QUEUE,TYPE       #<! Tree queue type
  #SET(%mTempVariable,'m_Display')
%[22]mTempVariable STRING(255)      #<! What to display
  #IF(%ControlHasColor)
    #SET(%mTempVariable,'m_NormalFG')
%[22]mTempVariable LONG             #<! Normal foreground color
    #SET(%mTempVariable,'m_NormalBG')
%[22]mTempVariable LONG             #<! Normal background color
    #SET(%mTempVariable,'m_SelectedFG')
%[22]mTempVariable LONG             #<! Selected foreground color
    #SET(%mTempVariable,'m_SelectedBG')
%[22]mTempVariable LONG             #<! Selected background color
  #ENDIF
  #IF(%ControlHasIcon)
    #SET(%mTempVariable,'m_Icon')
%[22]mTempVariable LONG             #<! Level icon
  #ENDIF
  #SET(%mTempVariable,'m_Level')
%[22]mTempVariable LONG             #<! Level
  #IF(%ControlHasStyle)
    #SET(%mTempVariable,'m_Style')
%[22]mTempVariable LONG             #<! Style
  #ENDIF
  #SET(%mTempVariable,'m_Loaded')
%[22]mTempVariable LONG             #<! Loaded flag
  #SET(%mTempVariable,'m_Position')
%[22]mTempVariable STRING(512)      #<! Level position
  #SET(%mTempVariable,'m_Key')
%[22]mTempVariable &KEY             #<! Level key
  #SET(%mTempVariable,'m_Instance')
%[22]mTempVariable LONG             #<! Level unique instance
  #FOR(%MCRTPrimaryHotFields)
    #SET(%mTempVariable,%MCRTPrimaryHotField)
%[22]mTempVariable LIKE(%MCRTPrimaryHotField)             #<! Hot Field for %Primary
  #ENDFOR
  #FOR(%Secondary)
    #FOR(%MCRTSecondaryHotFields)
        #SET(%mTempVariable,%MCRTSecondaryHotField)
%[22]mTempVariable LIKE(%MCRTSecondaryHotField)             #<! Hot Field for %Secondary
    #ENDFOR
  #ENDFOR
  #SET(%mTempVariable,%False)
#EMBED(%MCRTEndOfQueue,'Before End of Queue Definition'),%ActiveTemplateInstance,MAP(%ActiveTemplateInstance,%ActiveTemplateInstanceDescription),LABEL
%[20]mTempVariable END
#!-----------------------------------------------------------------------------
#! Tree state queue
#!-----------------------------------------------------------------------------
  #SET(%mTempVariable,%mInstancePrefix & ':LoadedQueueType')
%[20]mTempVariable QUEUE,TYPE       #<! Tree state queue type
  #SET(%mTempVariable,'m_LoadedInstance')
%[22]mTempVariable LONG             #<! Loaded level instance
  #SET(%mTempVariable,'m_LoadedPosition')
%[22]mTempVariable STRING(512)      #<! Loaded level position
  #SET(%mTempVariable,%False)
%[20]mTempVariable END
%[20]mTreeQueue QUEUE(%mInstancePrefix:QueueType).            #<! Tree queue
%[20]mTreeStateQueue QUEUE(%mInstancePrefix:LoadedQueueType). #<! Tree state queue
  #IF(%RangeField AND %RangeLimitType <> 'File Relationship')
        #FIX(%File,%Primary)
        #FIX(%Key,%PrimaryKey)
        #FOR(%KeyField)
          #IF(UPPER(%KeyField)=UPPER(%RangeField))
            #IF(%RangeLimitType <> 'Current Value')
              #BREAK
            #ENDIF
          #ENDIF
          #FIX(%Field,%KeyField)                        #! Fix to component of key
          #SET(%ValueConstruct,%mInstancePrefix & ':Save:' & %KeyField)
          #IF(%FieldType = 'GROUP')                     #! IF component is a group
%[20]ValueConstruct LIKE(%KeyField),PRE(SAV)                  #<! Save Range Limit Group
          #ELSE                                         #! Else component NOT a group
%[20]ValueConstruct LIKE(%KeyField)                           #<! Save Range Limit Field
          #ENDIF                                        #! EndIf component is a group
          #IF(UPPER(%KeyField)=UPPER(%RangeField))
            #BREAK
          #ENDIF
        #ENDFOR
  #ENDIF
#ENDAT
#!-----------------------------------------------------------------------------
#AT(%PrepareAlerts)
  #COMMENT(51)
#INSERT(%GenerateRangeLimitSaveValueCode,'SAVE')
DO %mInstancePrefix:ReloadTree    #<! Refresh all tree
DO %mInstancePrefix:CustomizeTree #<! Customize tree control
#ENDAT
#!-----------------------------------------------------------------------------
#AT(%RefreshWindowBeforeDisplay)
  #COMMENT(51)
DO %mInstancePrefix:ReloadTree    #<! Refresh all tree
#ENDAT
#!-----------------------------------------------------------------------------
#AT(%ControlPostEventCaseHandling,%mTreeControl)
  #COMMENT(51)
CASE EVENT()
OF EVENT:AlertKey
  DO %mInstancePrefix:OnAlert #<! Process tree alert key
END
#ENDAT
#!-----------------------------------------------------------------------------
#AT(%ControlPreEventHandling,%mTreeControl,'NewSelection')
  #COMMENT(51)
DO %mInstancePrefix:OnNewSelection #<! Process new selection of tree
#ENDAT
#!-----------------------------------------------------------------------------
#AT(%ControlPreEventHandling,%mTreeControl,'Expanded')
  #COMMENT(51)
DO %mInstancePrefix:OnExpanded #<! Process level expanding
#ENDAT
#!-----------------------------------------------------------------------------
#AT(%ControlPreEventHandling,%mTreeControl,'Contracted')
  #COMMENT(51)
DO %mInstancePrefix:OnContracted #<! Process level contracting
#ENDAT
#!-----------------------------------------------------------------------------
#AT(%WindowEventHandling,'GainFocus')
#ENDAT
#!-----------------------------------------------------------------------------
#AT(%BeforeWindowClosing)
  #COMMENT(51)
FREE(%mTreeQueue)       #<! Free allocated memory
FREE(%mTreeStateQueue)  #<! Free allocated memory
#ENDAT
#!-----------------------------------------------------------------------------
#AT(%DataSectionAfterWindow)
#INSERT(%GenerateLevelEquates)
#ENDAT
#!-----------------------------------------------------------------------------
#AT(%WindowOtherEventHandling),WHERE(%AcceptToolBarControl AND UPPER(%AppTemplateFamily)='CLARION')
DO %mInstancePrefix:ReltreeDispatch
#ENDAT
#!-----------------------------------------------------------------------------
#AT(%WindowEventHandling,'OpenWindow'),WHERE(%AcceptToolBarControl AND %IsFirstInstance AND UPPER(%AppTemplateFamily)='CLARION')
DO %mInstancePrefix:AssignButtons
#ENDAT
#!-----------------------------------------------------------------------------
#AT(%WindowManagerMethodCodeSection,'Init','(),BYTE'),PRIORITY(8500),WHERE(%AcceptToolBarControl AND %IsFirstInstance AND UPPER(%AppTemplateFamily)='ABC')
Toolbar.AddTarget(%mInstancePrefix:Toolbar, %mTreeControl)
DO %mInstancePrefix:AssignButtons
#ENDAT
#!-----------------------------------------------------------------------------
#AT(%LocalProcedures),WHERE(%AcceptToolbarControl AND UPPER(%AppTemplateFamily)='ABC')
%mInstancePrefix:Toolbar.TakeEvent PROCEDURE(<*LONG VCR>,WindowManager WM)
  CODE
  CASE ACCEPTED()
  OF Toolbar:Bottom TO Toolbar:Up
    SELF.Control{PROPLIST:MouseDownRow} = CHOICE(SELF.Control) !! Server routines assume this
    EXECUTE(ACCEPTED()-Toolbar:Bottom+1)
      DO %mInstancePrefix:NextParent
      DO %mInstancePrefix:PreviousParent
      DO %mInstancePrefix:NextLevel
      DO %mInstancePrefix:PreviousLevel
      DO %mInstancePrefix:NextRecord
      DO %mInstancePrefix:PreviousRecord
    END
  #EMBED(%MCRTToolbarDispatch,'Reltree Toolbar Dispatch'),%ActiveTemplateInstance,MAP(%ActiveTemplateInstance,%ActiveTemplateinstanceDescription)
  ELSE
    PARENT.TakeEvent(VCR,%WindowManagerObject)
  END
  #EMBED (%MCRTAfterToolbarDispatch, 'After toolbar processed'),%ActiveTemplateInstance,TREE('After toolbar processed|MultiChildRelTree on '&%Primary)
#ENDAT
#!-----------------------------------------------------------------------------
#AT(%ProcedureRoutines)
  #COMMENT(60)
  #FIND(%ControlInstance,%ActiveTemplateInstance,%Control)
!------------------------------------------------------------------------------
!-------------  MULTI CHILD RELATION TREE CONTROL IMPLEMENTATION --------------
!------------------------------------------------------------------------------
  #FOR(%mFileTreeList)
    #SET(%mTempInstance,INSTANCE(%mFileTreeList))
    #IF(%mTempInstance = 1)
!| The following information will be usefull for understanding of link
!| relation files with their unique identifiers
!|
    #ENDIF
    #SET(%mTempVariable,'!| The file ' & %mFileTreeFile)
    #IF(%mFileTreeChildRoot)
%[24]mTempVariable has instance - %mTempInstance, level - %mFileTreeLevel (ChildRoot)
    #ELSE
%[24]mTempVariable has instance - %mTempInstance, level - %mFileTreeLevel
    #ENDIF
  #ENDFOR
!------------------------------------------------------------------------------
  #SET(%mTempVariable,%mInstancePrefix & ':CustomizeTree')
%[35]mTempVariable ROUTINE
!|
!| The given routine
!|
#!-----------------------------------------------------------------------------
#! Customize window
#!-----------------------------------------------------------------------------
  #IF(%MCRTRootLineHeight)
  %mTreeControl{PROP:LineHeight} = %mTreeControl{PROP:LineHeight} + %MCRTRootLineHeight
  #ENDIF
  #IF(%ControlHasIcon)
    #FOR(%IconList),WHERE(%IconListType <> 'Variable')
      #SET(%mTempVariable,INSTANCE(%IconList))
      #IF(%IconListType = 'Built-In')
  %mTreeControl{PROP:IconList,%mTempVariable} = %IconList
      #ELSIF(%IconListType = 'File')
  %mTreeControl{PROP:IconList,%mTempVariable} = '~%IconList'
      #ELSIF(%IconListType = 'External')
  %mTreeControl{PROP:IconList,%mTempVariable} = '%IconList'
      #ENDIF
    #ENDFOR
  #ENDIF
#!  #IF(%ControlHasStyle)
#!    #DECLARE(%mTempFontStyle)
#!    #FOR(%MCRTRootStyles)
#!      #SET(%mTempVariable,INSTANCE(%MCRTRootStyles))
#!      #SET(%mTempFontStyle,%False)
#!      #FOR(%MCRTRootStyleFontStyle)
#!        #IF(%mTempFontStyle)
#!          #SET(%mTempFontStyle,CLIP(%mTempFontStyle) & ' + ' & %MCRTRootStyleFontStyle)
#!        #ELSE
#!          #SET(%mTempFontStyle,%MCRTRootStyleFontStyle)
#!        #ENDIF
#!      #ENDFOR
#!      #IF(%MCRTRootStyleOtherFontName)
#!        #SET(%mTempFontName,%MCRTRootStyleOtherFontName)
#!      #ELSE
#!        #SET(%mTempFontName,%MCRTRootStyleFontName)
#!      #ENDIF
#!  %mTreeControl{PROPSTYLE:FontName,%mTempVariable} = %(%ConvertData(%mTempFontName))
#!  %mTreeControl{PROPSTYLE:FontSize,%mTempVariable} = %MCRTRootStyleFontSize
#!      #IF(%mTempFontStyle)
#!  %mTreeControl{PROPSTYLE:FontStyle,%mTempVariable} = %mTempFontStyle
#!      #ENDIF
#!  %mTreeControl{PROPSTYLE:CharSet,%mTempVariable} = %MCRTRootStyleFontCharset
#!  %mTreeControl{PROPSTYLE:TextColor,%mTempVariable} = %MCRTRootStyleNormalForegroundColor
#!  %mTreeControl{PROPSTYLE:BackColor,%mTempVariable} = %MCRTRootStyleNormalBackgroundColor
#!  %mTreeControl{PROPSTYLE:TextSelected,%mTempVariable} = %MCRTRootStyleSelectedForegroundColor
#!  %mTreeControl{PROPSTYLE:BackSelected,%mTempVariable} = %MCRTRootStyleSelectedBackgroundColor
#!      #IF(%MCRTRootStyleFontPicture)
#!  %mTreeControl{PROPSTYLE:Picture,%mTempVariable} = %(%ConvertData(%MCRTRootStyleFontPicture))
#!      #ENDIF
#!    #ENDFOR
#!  #ENDIF
  #IF(%MCRTRootExpandKey)
  %mTreeControl{PROP:Alrt,255} = %MCRTRootExpandKey
  #ELSE
  %mTreeControl{PROP:Alrt,255} = PlusKey
  #ENDIF
  #IF(%MCRTRootContractKey)
  %mTreeControl{PROP:Alrt,255} = %MCRTRootContractKey
  #ELSE
  %mTreeControl{PROP:Alrt,255} = MinusKey
  #ENDIF
  #EMBED(%MCRTDeclareUpdateAlerts,'Declare update buttons alerts'),%ActiveTemplateInstance,HIDE
  %mTreeControl{PROP:Selected} = 1
  POST(EVENT:NewSelection,%mTreeControl)
!------------------------------------------------------------------------------
  #SET(%mTempVariable,%mInstancePrefix & ':OnAlert')
%[35]mTempVariable ROUTINE
!|
!| The given routine
!|
  CASE KEYCODE()
  #IF(%MCRTRootExpandKey)
  OF %MCRTRootExpandKey
  #ELSE
  OF PlusKey
  #ENDIF
    %mTreeControl{PROPLIST:MouseDownRow} = CHOICE(%mTreeControl)
    POST(EVENT:Expanded,%mTreeControl)
  #IF(%MCRTRootContractKey)
  OF %MCRTRootContractKey
  #ELSE
  OF MinusKey
  #ENDIF
    %mTreeControl{PROPLIST:MouseDownRow} = CHOICE(%mTreeControl)
    POST(EVENT:Contracted,%mTreeControl)
  #EMBED(%MCRTProcessUpdateAlerts,'Processing of update key alerts'),%ActiveTemplateInstance,MAP(%ActiveTemplateInstance,%ActiveTemplateinstanceDescription)
  END
!------------------------------------------------------------------------------
  #SET(%mTempVariable,%mInstancePrefix & ':OnNewSelection')
%[35]mTempVariable ROUTINE
!|
!| The given routine
!|

  %mInstancePrefix:CurrentChoice = CHOICE(%mTreeControl)
  GET(%mTreeQueue,%mInstancePrefix:CurrentChoice)
  DO %mInstancePrefix:SetHotFieldsFromQueue
  #EMBED(%MCRTBeforeProcessTreeNewSelection,'Before process new selection on the tree'),%ActiveTemplateInstance,MAP(%ActiveTemplateInstance,%ActiveTemplateinstanceDescription)
#!-----------------------------------------------------------------------------
#! Prepare condition to disable update buttons on ChildRoots
#!-----------------------------------------------------------------------------
  #SET(%CheckChildRootConditionItems,0)
  #FOR(%mFileTreeList),WHERE(%mFileTreeChildRoot)
    #SET(%CheckChildRootConditionItems,%CheckChildRootConditionItems+1)
    #IF(%CheckChildRootCondition)
      #SET(%CheckChildRootCondition,%CheckChildRootCondition & ',' & %GetInstanceEquate(INSTANCE(%mFileTreeList)))
    #ELSE
      #SET(%CheckChildRootCondition,%GetInstanceEquate(INSTANCE(%mFileTreeList)))
    #ENDIF
  #ENDFOR
  #IF(%CheckChildRootCondition)
    #IF(%CheckChildRootConditionItems>1)
        #SET(%CheckChildRootCondition,'OR INLIST(' & %mTreeQueue & ':m_Instance,' & %CheckChildRootCondition & ')')
    #ELSE
        #SET(%CheckChildRootCondition,'OR ' & %mTreeQueue & ':m_Instance = ' & %CheckChildRootCondition )
    #ENDIF
  #ENDIF
  #SUSPEND
  #?IF %mTreeQueue:m_Level = 0 %CheckChildRootCondition
     #EMBED(%MCRTDisableControls,'Disable Update Buttons'),%ActiveTemplateInstance,MAP(%ActiveTemplateInstance,%ActiveTemplateinstanceDescription)
  #?ELSE
     #EMBED(%MCRTEnableControls,'Enable Update Buttons'),%ActiveTemplateInstance,MAP(%ActiveTemplateInstance,%ActiveTemplateinstanceDescription)
  #?END
  #RESUME
  #EMBED(%MCRTAfterDisableControls,'After Disable Update Buttons'),%ActiveTemplateInstance,MAP(%ActiveTemplateInstance,%ActiveTemplateinstanceDescription)
#!
  IF KEYCODE() = MouseRight
    SETKEYCODE(0)
  #FREE(%PopupMenu)
  #IF(%MCRTRootExpandContractViaPopup)
    #IF(%MCRTUseOneItem)
      #CALL(%AddItemToPopupMenu,'POPUP_EXPAND',%MCRTRootExpandContractPriority,'!CHOOSE('&%mTreeQueue&'.m_Loaded,'&%TranslateMacro('POPUP_CONTRACT')&','&%TranslateMacro('POPUP_EXPAND')&')','IF '&%mTreeQueue&'.m_Loaded THEN DO '&%mInstancePrefix&':OnContracted;ELSE;DO '&%mInstancePrefix&':OnExpanded;END','','')
    #ELSE
      #CALL(%AddItemToPopupMenu,'POPUP_EXPAND',%MCRTRootExpandContractPriority,'!'&%TranslateMacro('POPUP_EXPAND'),'DO '&%mInstancePrefix&':OnExpanded','','')
      #CALL(%AddItemToPopupMenu,'POPUP_CONTRACT',%MCRTRootExpandContractPriority+10,'!'&%TranslateMacro('POPUP_CONTRACT'),'DO '&%mInstancePrefix&':OnContracted','','')
    #ENDIF
  #ENDIF
  #IF(%MCRTRefreshViaPopup)
     #IF(UPPER(%AppTemplateFamily)='CLARION')
        #CALL(%AddItemToPopupMenu,'POPUP_REFRESH',%MCRTRefreshPriority,'!'&%TranslateMacro('POPUP_REFRESH'),'DO RefreshWindow','','',1)
     #ELSE
        #CALL(%AddItemToPopupMenu,'POPUP_REFRESH',%MCRTRefreshPriority,'!'&%TranslateMacro('POPUP_REFRESH'),%WindowManagerObject&'.Reset()','','',1)
     #ENDIF
  #ENDIF
#!----------------------------------------------------------------------------
#! If "Use Setup Capability" is checked on then "Setup" item is added...
#!----------------------------------------------------------------------------
  #IF(%MCRTRootUseSetupCapability)
    #CALL(%AddItemToPopupMenu,'POPUP_SETUP',%MCRTSetupPopupPriority,'!'&%TranslateMacro('POPUP_SETUP'),%mInstancePrefix&':SetupTreeParams;DO '&%mInstancePrefix&':ReloadTree','','',1)
  #ENDIF
    #INSERT(%GeneratePopupMenu)
  END
  #EMBED(%MCRTAfterProcessTreeNewSelection,'After process new selection on the tree'),%ActiveTemplateInstance,MAP(%ActiveTemplateInstance,%ActiveTemplateinstanceDescription)
!------------------------------------------------------------------------------
  #SET(%mTempVariable,%mInstancePrefix & ':SetHotFieldsToQueue')
%[35]mTempVariable ROUTINE
!|
!| The given routine
!|
  #EMBED(%MCRTSetHotFieldsToQueue,'Set Hot Fields To Queue'),%ActiveTemplateInstance,MAP(%ActiveTemplateInstance,%ActiveTemplateInstanceDescription)
  #FOR(%MCRTPrimaryHotFields)
  %mTreeQueue.%MCRTPrimaryHotField = %MCRTPrimaryHotField
  #ENDFOR
  #FOR(%Secondary)
    #FOR(%MCRTSecondaryHotFields)
  %mTreeQueue.%MCRTSecondaryHotField = %MCRTSecondaryHotField
    #ENDFOR
  #ENDFOR
!------------------------------------------------------------------------------
  #SET(%mTempVariable,%mInstancePrefix & ':SetHotFieldsFromQueue')
%[35]mTempVariable ROUTINE
!|
!| The given routine
!|
  #EMBED(%MCRTSetHotFieldsFromQueue,'Set Hot Fields From Queue'),%ActiveTemplateInstance,MAP(%ActiveTemplateInstance,%ActiveTemplateInstanceDescription)
  #FOR(%MCRTPrimaryHotFields)
  %MCRTPrimaryHotField = %mTreeQueue.%MCRTPrimaryHotField
  #ENDFOR
  #FOR(%Secondary)
    #FOR(%MCRTSecondaryHotFields)
  %MCRTSecondaryHotField = %mTreeQueue.%MCRTSecondaryHotField
    #ENDFOR
  #ENDFOR
!------------------------------------------------------------------------------
  #SET(%mTempVariable,%mInstancePrefix & ':ReloadTree')
%[35]mTempVariable ROUTINE
!|
!| The given routine
!|
  #EMBED(%MCRTBeforeReloadTree,'Before Reload Tree'),%ActiveTemplateInstance,MAP(%ActiveTemplateInstance,%ActiveTemplateInstanceDescription)
  #IF(NOT %ActiveInvisible)
  IF %mTreeControl{PROP:Visible}
     #INDENT(+3)
  #ENDIF
  DO %mInstancePrefix:LoadRoot
  #IF(NOT %DisablePrimaryLevel)
    #FOR(%mFileTreeList),WHERE(%mFileTreeIsPrimary)
  DO %mInstancePrefix:LoadLevel_%mFileTreeFile
    #ENDFOR
  #ELSE
    #FOR(%Secondary),WHERE(%SecondaryTo=%Primary)
        #IF(%MCRTChildRootLevelName)
  DO %mInstancePrefix:LoadChildRoot_%Secondary
        #ELSE
  DO %mInstancePrefix:LoadLevel_%Secondary
        #ENDIF
    #ENDFOR
  #ENDIF
  IF %mInstancePrefix:NewItemLevel
    %mInstancePrefix:CurrentChoice = 0
    LOOP
      %mInstancePrefix:CurrentChoice += 1
      GET(%mTreeQueue,%mInstancePrefix:CurrentChoice)
      IF ERRORCODE() THEN BREAK.
      IF %mTreeQueue.m_Instance <> %mInstancePrefix:NewItemLevel THEN CYCLE.
      IF %mTreeQueue.m_Position <> %mInstancePrefix:NewItemPosition THEN CYCLE.
      %mTreeControl{PROP:Selected} = %mInstancePrefix:CurrentChoice
      BREAK
    END
  END
  #IF(NOT %ActiveInvisible)
     #INDENT(-3)
  END
  #ENDIF
  #EMBED(%MCRTAfterReloadTree,'After Reload Tree'),%ActiveTemplateInstance,MAP(%ActiveTemplateInstance,%ActiveTemplateInstanceDescription)
!------------------------------------------------------------------------------
  #SET(%mTempVariable,%mInstancePrefix & ':LoadRoot')
%[35]mTempVariable ROUTINE
!|
!| The given routine perform loading of root level of the relation tree.
!|
  #INSERT(%FormatLevelRoutineData),NOINDENT
  FREE(%mTreeQueue)
  IF %mInstancePrefix:ForceReload
     FREE(%mTreeStateQueue)
     %mInstancePrefix:ForceReload = False
  END
  CLEAR(%mTreeQueue)
  #IF(%EnableTreeRoot)
  %mTreeQueue.m_Display = %(%ConvertData(%MCRTRootHeader))
  #IF(%ControlHasColor)
    #IF(ITEMS(%MCRTRootConditionalColors))
      #FOR(%MCRTRootConditionalColors)
        #IF(INSTANCE(%MCRTRootConditionalColors) = 1)
  IF %MCRTRootColorCondition
        #ELSE
  ELSIF %MCRTRootColorCondition
        #ENDIF
    %mTreeQueue.m_NormalFG = %MCRTRootNormalFGConditionalColor
    %mTreeQueue.m_NormalBG = %MCRTRootNormalBGConditionalColor
    %mTreeQueue.m_SelectedFG = %MCRTRootSelectedFGConditionalColor
    %mTreeQueue.m_SelectedBG = %MCRTRootSelectedBGConditionalColor
      #ENDFOR
  ELSE
    %mTreeQueue.m_NormalFG = %MCRTRootNormalFGColor
    %mTreeQueue.m_NormalBG = %MCRTRootNormalBGColor
    %mTreeQueue.m_SelectedFG = %MCRTRootSelectedFGColor
    %mTreeQueue.m_SelectedBG = %MCRTRootSelectedBGColor
  END
    #ELSE
  %mTreeQueue.m_NormalFG = %MCRTRootNormalFGColor
  %mTreeQueue.m_NormalBG = %MCRTRootNormalBGColor
  %mTreeQueue.m_SelectedFG = %MCRTRootSelectedFGColor
  %mTreeQueue.m_SelectedBG = %MCRTRootSelectedBGColor
    #ENDIF
  #ENDIF
  #IF(%ControlHasIcon)
    #FIX(%IconList,%MCRTRootIcon)
  #INSERT(%SetLevelIcon)
  #ENDIF
  #IF(%ControlHasStyle)
    #IF(ITEMS(%MCRTRootConditionalStyles))
      #FOR(%MCRTRootConditionalStyles)
        #IF(INSTANCE(%MCRTRootConditionalStyles) = 1)
  IF %MCRTRootStyleCondition
        #ELSE
  ELSIF %MCRTRootStyleCondition
        #ENDIF
    %mTreeQueue.m_Style = %(%GetStyleNumber(%MCRTRootConditionalStyleType,%MCRTRootConditionalStyle,%MCRTRootConditionalStyleLocalList)) ! %MCRTRootConditionalStyle
      #ENDFOR
  ELSE
    %mTreeQueue.m_Style = %(%GetStyleNumber(%MCRTRootStyleType,%MCRTRootStyle,%MCRTRootStyleLocalList)) ! %MCRTRootStyle
  END
    #ELSE
  %mTreeQueue.m_Style = %(%GetStyleNumber(%MCRTRootStyleType,%MCRTRootStyle,%MCRTRootStyleLocalList)) ! %MCRTRootStyle
    #ENDIF
  #ENDIF
  %mTreeQueue.m_Level = 0
  %mTreeQueue.m_Loaded = FALSE
  %mTreeQueue.m_Key &= NULL
  %mTreeQueue.m_Instance = 0
  #IF(%MCRTRootUseSetupCapability)
    #IF(%ControlHasStyle)
  #INSERT(%LoadStyleFromIniFile)
    #ELSIF(%ControlHasColor)
  #INSERT(%LoadColorsFromIniFile)
    #ENDIF
  #ENDIF
  #EMBED(%MCRTAfterFormatRoot,'After Format Root'),%ActiveTemplateInstance,MAP(%ActiveTemplateInstance,%ActiveTemplateInstanceDescription)
  ADD(%mTreeQueue)
  #ENDIF
  #MESSAGE('Multi Children Tree levels generation',3)
#!-----------------------------------------------------------------------------
#! Load child file levels
#!-----------------------------------------------------------------------------
  #FOR(%mFileTreeList),WHERE(~%mFileTreeChildRoot)
!------------------------------------------------------------------------------
    #SET(%mTempVariable,%mInstancePrefix & ':LoadLevel_' & %mFileTreeFile)
%[35]mTempVariable ROUTINE
!|
!| The given routine is used for load of the single tree level (%mFileTreeFile).
!|
    #FIX(%File,%mFileTreeFile)
  CLEAR(%FilePrefix:Record)
  #IF(%mFileTreeCustomJoin)
    #FOR(%mFileTreeCustomJoinField)
       #IF(%mFileTreeCustomJoinValueType=1)
  CLEAR(%mFileTreeCustomJoinField)
       #ELSE
  %mFileTreeCustomJoinField = %mFileTreeCustomJoinValue
       #ENDIF
    #ENDFOR
  #ELSE
    #IF(%mFileTreeIsPrimary)
  #INSERT(%GenerateRangeAsign)
    #ELSE
       #FIX(%Secondary,%mFileTreeFile)
       #FIX(%Relation,%mFileTreeParent)
       #FOR(%RelationKeyField)
         #IF(%RelationKeyField)
  %RelationKeyFieldLink = %RelationKeyField
         #ENDIF
       #ENDFOR
    #ENDIF
  #ENDIF
  #IF(UPPER(%AppTemplateFamily)='ABC')
  Access:%mFileTreeFile.UseFile()
  #ENDIF
  DO %mInstancePrefix:SetOrderLevel_%mFileTreeFile
  LOOP
    #EMBED(%BeforeFileNext,'Relation Tree, Before NEXT on File'),%ActiveTemplateInstance,%mFileTreeFile,MAP(%ActiveTemplateInstance,%ActiveTemplateinstanceDescription)
    #IF(UPPER(%AppTemplateFamily)='CLARION')
    NEXT(%mFileTreeFile)
    IF ERRORCODE()
      IF ERRORCODE() = BadRecErr
        BREAK
      ELSE
        MESSAGE(%(%TranslateMacro('MSG_ERR_GETFILE')) & ERRORFILE(),%(%TranslateMacro('MSG_TITLE')),ICON:Exclamation,BUTTON:Cancel)
        POST(EVENT:CloseWindow)
      END
    END
    #ELSE
    IF Access:%mFileTreeFile.Next() NOT= Level:Benign
      IF Access:%mFileTreeFile.GetEOF()
        BREAK
      ELSE
        POST(EVENT:CloseWindow)
        EXIT
      END
    END
    #ENDIF
    #EMBED(%AfterFileNext,'Relation Tree, After NEXT on File'),%ActiveTemplateInstance,%mFileTreeFile,MAP(%ActiveTemplateInstance,%ActiveTemplateinstanceDescription)
    #IF(%mFileTreeCustomJoin)
       #FOR(%mFileTreeCustomJoinField)
         #SET(%mTempVariable,%GetField(%mFileTreeCustomJoinField))
         #IF(%mFileTreeCustomJoinValueType<>1)
             #SET(%mTempVariable,%mFileTreeCustomJoinField & ' <> ' & %mFileTreeCustomJoinValue)
    IF %mTempVariable THEN BREAK.
         #ENDIF
       #ENDFOR
    #ELSE
       #FOR(%RelationKeyField),WHERE(%RelationKeyField)
         #SET(%mTempVariable,%GetField(%RelationKeyFieldLink))
         #SET(%mTempVariable,%mTempVariable & ' <> ')
         #SET(%mTempVariable,%mTempVariable & %GetField(%RelationKeyField))
    IF %mTempVariable THEN BREAK.
       #ENDFOR
    #ENDIF
    #IF(%mFileTreeIsPrimary)
    #INSERT(%GenerateRangeFilter)
    #ENDIF
#!-----------------------------------------------------------------------
#! Checking for 1:MANY tables
#!-----------------------------------------------------------------------
    #IF(ITEMS(%mFileTreeOneToManyChild))
!-----------------------------------------------------------------------
! Fetching the 1:MANY Tables
!-----------------------------------------------------------------------
       #FOR(%mFileTreeOneToManyChild)
           #FIX(%File,%mFileTreeOneToManyChild)
           #FIX(%Relation,%mFileTreeFile)
    CLEAR(%FilePrefix:Record)
           #FOR(%RelationKeyField)
               #IF(%RelationKeyField)
    %RelationKeyFieldLink = %RelationKeyField
               #ENDIF
           #ENDFOR
           #IF(UPPER(%AppTemplateFamily)='ABC')
    Access:%mFileTreeOneToManyChild.UseFile()
           #ENDIF
    SET(%FileKey,%FileKey)
           #IF(UPPER(%AppTemplateFamily)='CLARION')
    NEXT(%mFileTreeOneToManyChild)
           #ELSE
    Access:%mFileTreeOneToManyChild.TryNext()
           #ENDIF
       #ENDFOR
!-----------------------------------------------------------------------
       #FIX(%File,%mFileTreeFile)
    #ENDIF
#!-----------------------------------------------------------------------
    #IF(%mFileTreeIsPrimary)
      #IF(%MCRTPrimaryRecordFilter)
    IF ~(%MCRTPrimaryRecordFilter) THEN CYCLE.
      #ENDIF
    #ELSE
      #FIX(%Secondary,%mFileTreeFile)
      #IF(%MCRTChildRecordFilter)
    IF ~(%MCRTChildRecordFilter) THEN CYCLE.
      #ENDIF
    #ENDIF
    %mInstancePrefix:SubLevelExist = 0
    #FOR(%Secondary),WHERE((%SecondaryType = 'MANY:1' OR %SecondaryType = '') AND %SecondaryTo = %mFileTreeFile)
    DO %mInstancePrefix:CheckSubLevel_%Secondary
    #ENDFOR

    CLEAR(%mTreeQueue)
    %mTreeQueue.m_Level = CHOOSE(%mInstancePrefix:SubLevelExist = 0,%mFileTreeLevel,-%mFileTreeLevel)
    %mTreeQueue.m_Display = %mFileTreeDisplay
    #IF(%mFileTreeIsPrimary)
       #IF(%mFileTreeKey)
    %mTreeQueue.m_Key &= %mFileTreeKey
    %mTreeQueue.m_Position = POSITION(%mTreeQueue.m_Key)
    #!%mTreeQueue.m_Position = POSITION(%mFileTreeKey)
       #ELSE
    %mTreeQueue.m_Key &= %mFileTreeFile
    %mTreeQueue.m_Position = POSITION(%mTreeQueue.m_Key)
    #!%mTreeQueue.m_Position = POSITION(%mFileTreeFile)
       #ENDIF
    #ELSE
    %mTreeQueue.m_Key &= %mFileTreeKey
    %mTreeQueue.m_Position = POSITION(%mTreeQueue.m_Key)
    #!%mTreeQueue.m_Position = POSITION(%mFileTreeKey)
    #ENDIF
    #SET(%mTempInstance,INSTANCE(%mFileTreeList))
    %mTreeQueue.m_Instance = %(%GetInstanceEquate(%mTempInstance))
    Do %mInstancePrefix:SetHotFieldsToQueue
    #EMBED(%MCRTAfterSetQueueRecord,'After Set Queue Record'),%ActiveTemplateInstance,%mFileTreeFile,MAP(%ActiveTemplateInstance,%ActiveTemplateInstanceDescription)
    #IF(%mFileTreeChildren = 0)
    %mTreeQueue.m_Loaded = FALSE
    DO %mInstancePrefix:FormatLevel_%mFileTreeFile
    ADD(%mTreeQueue,POINTER(%mTreeQueue) + 1)
    #ELSE

    CLEAR(%mTreeStateQueue)
    %mTreeStateQueue.m_LoadedInstance = %mTreeQueue.m_Instance
    %mTreeStateQueue.m_LoadedPosition = %mTreeQueue.m_Position
    GET(%mTreeStateQueue,%mTreeStateQueue.m_LoadedInstance,%mTreeStateQueue.m_LoadedPosition)
    IF ERRORCODE() AND ~%mInstancePrefix:ExpandAll
      %mTreeQueue.m_Loaded = FALSE
      DO %mInstancePrefix:FormatLevel_%mFileTreeFile
      ADD(%mTreeQueue,POINTER(%mTreeQueue) + 1)
    ELSE
      IF %mInstancePrefix:ExpandAll
        ADD(%mTreeStateQueue,%mTreeStateQueue.m_LoadedInstance,%mTreeStateQueue.m_LoadedPosition)
      END
      %mTreeQueue.m_Level = ABS(%mTreeQueue.m_Level)
      %mTreeQueue.m_Loaded = CHOOSE(%mInstancePrefix:SubLevelExist <> 0)
      DO %mInstancePrefix:FormatLevel_%mFileTreeFile
      ADD(%mTreeQueue,POINTER(%mTreeQueue) + 1)
      IF ~%mInstancePrefix:SubLevelExist
        DELETE(%mTreeStateQueue)
        CYCLE
      END
#!-----------------------------------------------------------------------------
#! Loading of file-levels
#!-----------------------------------------------------------------------------
      #!FOR(%Secondary),WHERE(%SecondaryType = 'MANY:1' AND %SecondaryTo = %mFileTreeFile)
      #!  #IF(~%MCRTChildRootLevelName)
      #!DO %mInstancePrefix:LoadLevel_%Secondary
      #!  #ENDIF
      #!ENDFOR
#!-----------------------------------------------------------------------------
#! Loading of ChildRoot-levels
#!-----------------------------------------------------------------------------
      #!FOR(%Secondary),WHERE(%SecondaryType = 'MANY:1' AND %SecondaryTo = %mFileTreeFile)
      #!  #IF(%MCRTChildRootLevelName)
      #!DO %mInstancePrefix:LoadChildRoot_%Secondary
      #!  #ENDIF
      #!ENDFOR
#!-----------------------------------------------------------------------------
#! Loading of sublevels
#!-----------------------------------------------------------------------------
      #FOR(%Secondary),WHERE((%SecondaryType = 'MANY:1' OR %SecondaryType = '') AND %SecondaryTo = %mFileTreeFile)
        #IF(%MCRTChildRootLevelName)
      DO %mInstancePrefix:LoadChildRoot_%Secondary
        #ELSE
      DO %mInstancePrefix:LoadLevel_%Secondary
        #ENDIF
      #ENDFOR
    END
    #ENDIF
  END
!-----------------------------------------------------------------------------
    #SET(%mTempVariable,%mInstancePrefix & ':CheckSubLevel_' & %mFileTreeFile)
%[35]mTempVariable ROUTINE
!|
!| The given routine
!|
    #FIX(%File,%mFileTreeFile)
    #FIX(%Secondary,%mFileTreeFile)
    #FIX(%Relation,%mFileTreeParent)
  CLEAR(%FilePrefix:Record)
    #IF(%mFileTreeIsPrimary)
  #INSERT(%GenerateRangeAsign)
    #ELSE
       #FIX(%Secondary,%mFileTreeFile)
       #IF(%mFileTreeCustomJoin)
          #FOR(%mFileTreeCustomJoinField)
             #IF(%mFileTreeCustomJoinValueType=1)
  CLEAR(%mFileTreeCustomJoinField)
             #ELSE
  %mFileTreeCustomJoinField = %mFileTreeCustomJoinValue
             #ENDIF
          #ENDFOR
       #ELSE
          #FOR(%RelationKeyField)
            #IF(%RelationKeyField)
  %RelationKeyFieldLink = %RelationKeyField
            #ENDIF
          #ENDFOR
       #ENDIF
    #ENDIF
  #IF(UPPER(%AppTemplateFamily)='ABC')
  Access:%mFileTreeFile.UseFile()
  #ENDIF
  DO %mInstancePrefix:SetOrderLevel_%mFileTreeFile
  LOOP
    #IF(UPPER(%AppTemplateFamily)='CLARION')
    NEXT(%mFileTreeFile)
    IF ERRORCODE()
      IF ERRORCODE() = BadRecErr
        BREAK
      ELSE
        MESSAGE(%(%TranslateMacro('MSG_ERR_GETFILE')) & ERRORFILE(),%(%TranslateMacro('MSG_TITLE')),ICON:Exclamation,BUTTON:Cancel)
        POST(EVENT:CloseWindow)
        EXIT
      END
    END
    #ELSE
    IF Access:%mFileTreeFile.Next() <> Level:Benign
      IF Access:%mFileTreeFile.GetEOF()
        BREAK
      ELSE
        POST(EVENT:CloseWindow)
        EXIT
      END
    END
    #ENDIF
    #IF(%mFileTreeIsPrimary)
    #INSERT(%GenerateRangeFilter)
    #ELSE
       #FIX(%Secondary,%mFileTreeFile)
       #IF(%mFileTreeCustomJoin)
          #FOR(%mFileTreeCustomJoinField)
             #IF(%mFileTreeCustomJoinValueType<>1)
                #SET(%mTempVariable,%mFileTreeCustomJoinField & ' <> ' & %mFileTreeCustomJoinValue)
    IF %mTempVariable THEN BREAK.
             #ENDIF
          #ENDFOR
       #ELSE
          #FOR(%RelationKeyField),WHERE(%RelationKeyField)
            #SET(%mTempVariable,%GetField(%RelationKeyFieldLink))
            #SET(%mTempVariable,%mTempVariable & ' <> ')
            #SET(%mTempVariable,%mTempVariable & %GetField(%RelationKeyField))
    IF %mTempVariable THEN BREAK.
          #ENDFOR
       #ENDIF
    #ENDIF
    #IF(%mFileTreeIsPrimary)
      #IF(%MCRTPrimaryRecordFilter)
    IF ~(%MCRTPrimaryRecordFilter) THEN CYCLE.
      #ENDIF
    #ELSE
      #FIX(%Secondary,%mFileTreeFile)
      #IF(%MCRTChildRecordFilter)
    IF ~(%MCRTChildRecordFilter) THEN CYCLE.
      #ENDIF
    #ENDIF
    %mInstancePrefix:SubLevelExist += 1
    BREAK
  END
!-----------------------------------------------------------------------------
    #SET(%mTempVariable,%mInstancePrefix & ':SetOrderLevel_' & %mFileTreeFile)
%[35]mTempVariable ROUTINE
  #EMBED(%BeforeSetKey,'Relation Tree, Before SET Key on File'),%ActiveTemplateInstance,%mFileTreeFile,MAP(%ActiveTemplateInstance,%ActiveTemplateinstanceDescription)
    #IF(%mFileTreeIsPrimary)
       #IF(%mFileTreeKey)
        #IF(%RangeField)
  SET(%mFileTreeKey,%mFileTreeKey)
        #ELSE
  SET(%mFileTreeKey)
        #ENDIF
       #ELSE
  SET(%mFileTreeFile)
       #ENDIF
    #ELSE
  SET(%mFileTreeKey,%mFileTreeKey)
    #ENDIF
  #EMBED(%AfterSetKey,'Relation Tree, After SET Key on File'),%ActiveTemplateInstance,%mFileTreeFile,MAP(%ActiveTemplateInstance,%ActiveTemplateinstanceDescription)
!-----------------------------------------------------------------------------
    #SET(%mTempVariable,%mInstancePrefix & ':FormatLevel_' & %mFileTreeFile)
%[35]mTempVariable ROUTINE
!|
!| This routine formats a line of the display queue %mTreeQueue to display the
!| contents of a record of %mFileTreeFile.
!|
    #INSERT(%FormatLevelRoutineData),NOINDENT
    #IF(%ControlHasColor)
      #IF(%mFileTreeIsPrimary)
        #IF(ITEMS(%MCRTPrimaryConditionalColors))
          #FOR(%MCRTPrimaryConditionalColors)
            #IF(INSTANCE(%MCRTPrimaryConditionalColors) = 1)
  IF %MCRTPrimaryColorCondition
            #ELSE
  ELSIF %MCRTPrimaryColorCondition
            #ENDIF
    %mTreeQueue.m_NormalFG = %MCRTPrimaryNormalFGConditionalColor
    %mTreeQueue.m_NormalBG = %MCRTPrimaryNormalBGConditionalColor
    %mTreeQueue.m_SelectedFG = %MCRTPrimarySelectedFGConditionalColor
    %mTreeQueue.m_SelectedBG = %MCRTPrimarySelectedBGConditionalColor
          #ENDFOR
  ELSE
    %mTreeQueue.m_NormalFG = %MCRTPrimaryNormalFGColor
    %mTreeQueue.m_NormalBG = %MCRTPrimaryNormalBGColor
    %mTreeQueue.m_SelectedFG = %MCRTPrimarySelectedFGColor
    %mTreeQueue.m_SelectedBG = %MCRTPrimarySelectedBGColor
  END
        #ELSE
  %mTreeQueue.m_NormalFG = %MCRTPrimaryNormalFGColor
  %mTreeQueue.m_NormalBG = %MCRTPrimaryNormalBGColor
  %mTreeQueue.m_SelectedFG = %MCRTPrimarySelectedFGColor
  %mTreeQueue.m_SelectedBG = %MCRTPrimarySelectedBGColor
        #ENDIF
      #ELSE
        #FIX(%Secondary,%mFileTreeFile)
        #IF(ITEMS(%MCRTChildConditionalColors))
          #FOR(%MCRTChildConditionalColors)
            #IF(INSTANCE(%MCRTChildConditionalColors) = 1)
  IF %MCRTChildColorCondition
            #ELSE
  ELSIF %MCRTChildColorCondition
            #ENDIF
    %mTreeQueue.m_NormalFG = %MCRTChildNormalFGConditionalColor
    %mTreeQueue.m_NormalBG = %MCRTChildNormalBGConditionalColor
    %mTreeQueue.m_SelectedFG = %MCRTChildSelectedFGConditionalColor
    %mTreeQueue.m_SelectedBG = %MCRTChildSelectedBGConditionalColor
          #ENDFOR
  ELSE
    %mTreeQueue.m_NormalFG = %MCRTChildNormalFGColor
    %mTreeQueue.m_NormalBG = %MCRTChildNormalBGColor
    %mTreeQueue.m_SelectedFG = %MCRTChildSelectedFGColor
    %mTreeQueue.m_SelectedBG = %MCRTChildSelectedBGColor
  END
        #ELSE
  %mTreeQueue.m_NormalFG = %MCRTChildNormalFGColor
  %mTreeQueue.m_NormalBG = %MCRTChildNormalBGColor
  %mTreeQueue.m_SelectedFG = %MCRTChildSelectedFGColor
  %mTreeQueue.m_SelectedBG = %MCRTChildSelectedBGColor
        #ENDIF
      #ENDIF
    #ENDIF
    #IF(%ControlHasIcon)
      #IF(%mFileTreeIsPrimary)
        #IF(%MCRTPrimaryUseExpandAndContractIcon)
  IF %mTreeQueue.m_Loaded
     !Expanded
           #IF(ITEMS(%MCRTPrimaryConditionalIcons))
             #FOR(%MCRTPrimaryConditionalIcons)
               #IF(INSTANCE(%MCRTPrimaryConditionalIcons) = 1)
     IF %MCRTPrimaryIconCondition
               #ELSE
     ELSIF %MCRTPrimaryIconCondition
               #ENDIF
               #CALL(%FixIconList,'PRIMARY',%True,'EXPANDED')
       #INSERT(%SetLevelIcon)
             #ENDFOR
     ELSE
             #CALL(%FixIconList,'PRIMARY',%False,'EXPANDED')
       #INSERT(%SetLevelIcon)
     END
           #ELSE
             #CALL(%FixIconList,'PRIMARY',%False,'EXPANDED')
     #INSERT(%SetLevelIcon)
           #ENDIF
  ELSE
     !Contracted
           #IF(ITEMS(%MCRTPrimaryConditionalIcons))
             #FOR(%MCRTPrimaryConditionalIcons)
               #IF(INSTANCE(%MCRTPrimaryConditionalIcons) = 1)
     IF %MCRTPrimaryIconCondition
               #ELSE
     ELSIF %MCRTPrimaryIconCondition
               #ENDIF
               #CALL(%FixIconList,'PRIMARY',%True,'CONTRACTED')
       #INSERT(%SetLevelIcon)
             #ENDFOR
     ELSE
             #CALL(%FixIconList,'PRIMARY',%False,'CONTRACTED')
       #INSERT(%SetLevelIcon)
     END
           #ELSE
             #CALL(%FixIconList,'PRIMARY',%False,'CONTRACTED')
     #INSERT(%SetLevelIcon)
           #ENDIF
  END
        #ELSE
           #IF(ITEMS(%MCRTPrimaryConditionalIcons))
             #FOR(%MCRTPrimaryConditionalIcons)
               #IF(INSTANCE(%MCRTPrimaryConditionalIcons) = 1)
  IF %MCRTPrimaryIconCondition
               #ELSE
  ELSIF %MCRTPrimaryIconCondition
               #ENDIF
               #CALL(%FixIconList,'PRIMARY',%True,'DEFAULT')
    #INSERT(%SetLevelIcon)
             #ENDFOR
  ELSE
             #CALL(%FixIconList,'PRIMARY',%False,'DEFAULT')
    #INSERT(%SetLevelIcon)
  END
           #ELSE
             #CALL(%FixIconList,'PRIMARY',%False,'DEFAULT')
  #INSERT(%SetLevelIcon)
           #ENDIF
        #ENDIF
      #ELSE
        #FIX(%Secondary,%mFileTreeFile)
        #IF(%MCRTChildUseExpandAndContractIcon)
  IF %mTreeQueue.m_Loaded
     !Expanded
           #IF(ITEMS(%MCRTChildConditionalIcons))
             #FOR(%MCRTChildConditionalIcons)
               #IF(INSTANCE(%MCRTChildConditionalIcons) = 1)
     IF %MCRTChildIconCondition
               #ELSE
     ELSIF %MCRTChildIconCondition
               #ENDIF
               #CALL(%FixIconList,'CHILD',%True,'EXPANDED')
       #INSERT(%SetLevelIcon)
             #ENDFOR
     ELSE
             #CALL(%FixIconList,'CHILD',%False,'EXPANDED')
       #INSERT(%SetLevelIcon)
     END
           #ELSE
             #CALL(%FixIconList,'CHILD',%False,'EXPANDED')
     #INSERT(%SetLevelIcon)
           #ENDIF
  ELSE
     !Contracted
           #IF(ITEMS(%MCRTChildConditionalIcons))
             #FOR(%MCRTChildConditionalIcons)
               #IF(INSTANCE(%MCRTChildConditionalIcons) = 1)
     IF %MCRTChildIconCondition
               #ELSE
     ELSIF %MCRTChildIconCondition
               #ENDIF
               #CALL(%FixIconList,'CHILD',%True,'CONTRACTED')
       #INSERT(%SetLevelIcon)
             #ENDFOR
     ELSE
             #CALL(%FixIconList,'CHILD',%False,'CONTRACTED')
       #INSERT(%SetLevelIcon)
     END
           #ELSE
             #CALL(%FixIconList,'CHILD',%False,'CONTRACTED')
     #INSERT(%SetLevelIcon)
           #ENDIF
  END
        #ELSE
           #IF(ITEMS(%MCRTChildConditionalIcons))
             #FOR(%MCRTChildConditionalIcons)
               #IF(INSTANCE(%MCRTChildConditionalIcons) = 1)
  IF %MCRTChildIconCondition
               #ELSE
  ELSIF %MCRTChildIconCondition
               #ENDIF
               #CALL(%FixIconList,'CHILD',%True,'DEFAULT')
    #INSERT(%SetLevelIcon)
             #ENDFOR
  ELSE
             #CALL(%FixIconList,'CHILD',%False,'DEFAULT')
    #INSERT(%SetLevelIcon)
  END
           #ELSE
             #CALL(%FixIconList,'CHILD',%False,'DEFAULT')
  #INSERT(%SetLevelIcon)
           #ENDIF
        #ENDIF
      #ENDIF
    #ENDIF
#!
    #IF(%ControlHasStyle)
      #IF(%mFileTreeIsPrimary)
        #IF(ITEMS(%MCRTPrimaryConditionalStyles))
          #FOR(%MCRTPrimaryConditionalStyles)
            #IF(INSTANCE(%MCRTPrimaryConditionalStyles) = 1)
  IF %MCRTPrimaryStyleCondition
            #ELSE
  ELSIF %MCRTPrimaryStyleCondition
            #ENDIF
    %mTreeQueue.m_Style = %(%GetStyleNumber(%MCRTPrimaryConditionalStyleType,%MCRTPrimaryConditionalStyle,%MCRTPrimaryConditionalStyleLocalList)) ! %MCRTPrimaryConditionalStyle
          #ENDFOR
  #!ELSE
    %mTreeQueue.m_Style = %(%GetStyleNumber(%MCRTPrimaryStyleType,%MCRTPrimaryStyle,%MCRTPrimaryStyleLocalList)) ! %MCRTPrimaryStyle
  END
        #ELSE
  %mTreeQueue.m_Style = %(%GetStyleNumber(%MCRTPrimaryStyleType,%MCRTPrimaryStyle,%MCRTPrimaryStyleLocalList)) ! %MCRTPrimaryStyle
        #ENDIF
      #ELSE
        #IF(ITEMS(%MCRTChildConditionalStyles))
          #FOR(%MCRTChildConditionalStyles)
            #IF(INSTANCE(%MCRTChildConditionalStyles) = 1)
  IF %MCRTChildStyleCondition
            #ELSE
  ELSIF %MCRTChildStyleCondition
            #ENDIF
    %mTreeQueue.m_Style = %(%GetStyleNumber(%MCRTChildConditionalStyleType,%MCRTChildConditionalStyle,%MCRTChildConditionalStyleLocalList)) ! %MCRTChildConditionalStyle
          #ENDFOR
  #!ELSE
    %mTreeQueue.m_Style = %(%GetStyleNumber(%MCRTChildStyleType,%MCRTChildStyle,%MCRTChildStyleLocalList)) ! %MCRTChildStyle
  END
        #ELSE
  %mTreeQueue.m_Style = %(%GetStyleNumber(%MCRTChildStyleType,%MCRTChildStyle,%MCRTChildStyleLocalList)) ! %MCRTChildStyle
        #ENDIF
      #ENDIF
    #ENDIF
#!-----------------------------------------------------------------------------
#! Load level colors/styles from INI-file during runtime
#!-----------------------------------------------------------------------------
    #IF(%MCRTRootUseSetupCapability)
      #IF(%ControlHasStyle)
  #INSERT(%LoadStyleFromIniFile)
      #ELSIF(%ControlHasColor)
  #INSERT(%LoadColorsFromIniFile)
      #ENDIF
  rou_Prefix   = GETINI(%(%ConvertData(%mInstancePrefix & ':Parameters')),'Prefix_'  & %mTreeQueue.m_Instance,,%(%StripPling(%INIFileName)))
  IF rou_Prefix
    %mTreeQueue.m_Display = CLIP(rou_Prefix) & ' ' & %mTreeQueue.m_Display
  END
    #ENDIF
  #EMBED(%MCRTAfterFormatLevel,'After Format Level'),%ActiveTemplateInstance,%mFileTreeFile,MAP(%ActiveTemplateInstance,%ActiveTemplateInstanceDescription)
  #ENDFOR #! End of cycle for file-levels loading
#!-----------------------------------------------------------------------------
#! Load child ChildRoot levels
#!-----------------------------------------------------------------------------
  #FOR(%mFileTreeList),WHERE(%mFileTreeChildRoot)
!------------------------------------------------------------------------------
    #SET(%mTempVariable,%mInstancePrefix & ':LoadChildRoot_' & %mFileTreeFile)
%[35]mTempVariable ROUTINE
!|
!| The given routine loads the ChildRoot level with name %mFileTreeDisplay
!| before target file %mFileTreeFile level loading.
!|
  %mInstancePrefix:SubLevelExist = 0
  DO %mInstancePrefix:CheckSubLevel_%mFileTreeFile

  CLEAR(%mTreeQueue)
  %mTreeQueue.m_Level = CHOOSE(%mInstancePrefix:SubLevelExist = 0,%mFileTreeLevel,-%mFileTreeLevel)
  %mTreeQueue.m_Display = %mFileTreeDisplay
    #IF(%mFileTreeParent = %Primary)
      #IF(%PrimaryKey)
        #SET(%mTempVariable,%PrimaryKey)
      #ELSE
        #SET(%mTempVariable,%Primary)
      #ENDIF
    #ELSE
      #FIX(%File,%mFileTreeParent)
      #FIX(%Relation,%mFileTreeFile)
      #IF(NOT %mFileTreeCustomJoin)
         #SET(%mTempVariable,%FileKey)
      #ELSE
         #SET(%mTempVariable,%MCRTGetCustomJoinParentKey(%mFileTreeParent))
      #ENDIF
    #ENDIF
  #!mFileTreeParent = %mFileTreeParent
  #!mFileTreeFile = %mFileTreeFile
  #!mTempVariable = %mTempVariable This shuld be the mFileTreeParent Key
  #!Im guessing that I will need a function to return this because I will need to loop thru the symbol and get the parent key
  #!
  %mTreeQueue.m_Key &= %mTempVariable
  %mTreeQueue.m_Position = POSITION(%mTreeQueue.m_Key)
  #!%mTreeQueue.m_Position = POSITION(%mTempVariable)
    #SET(%mTempInstance,INSTANCE(%mFileTreeList))
  %mTreeQueue.m_Instance = %(%GetInstanceEquate(%mTempInstance))

  CLEAR(%mTreeStateQueue)
  %mTreeStateQueue.m_LoadedInstance = %mTreeQueue.m_Instance
  %mTreeStateQueue.m_LoadedPosition = %mTreeQueue.m_Position
  GET(%mTreeStateQueue,%mTreeStateQueue.m_LoadedInstance,%mTreeStateQueue.m_LoadedPosition)
  IF ERRORCODE() AND ~%mInstancePrefix:ExpandAll
    %mTreeQueue.m_Loaded = FALSE
    DO %mInstancePrefix:FormatChildRoot_%mFileTreeFile
    ADD(%mTreeQueue,POINTER(%mTreeQueue) + 1)
  ELSE
    IF %mInstancePrefix:ExpandAll
      ADD(%mTreeStateQueue,%mTreeStateQueue.m_LoadedInstance,%mTreeStateQueue.m_LoadedPosition)
    END
    %mTreeQueue.m_Level = ABS(%mTreeQueue.m_Level)
    %mTreeQueue.m_Loaded = CHOOSE(%mInstancePrefix:SubLevelExist <> 0)
    DO %mInstancePrefix:FormatChildRoot_%mFileTreeFile
    ADD(%mTreeQueue,POINTER(%mTreeQueue) + 1)
    DO %mInstancePrefix:LoadLevel_%mFileTreeFile
  END
!------------------------------------------------------------------------------
    #SET(%mTempVariable,%mInstancePrefix & ':FormatChildRoot_' & %mFileTreeFile)
%[35]mTempVariable ROUTINE
!|
!| This routine formats a line of the display queue %mTreeQueue (ChildRoot)
!| to display the contents of a record of %mFileTreeFile.
!|
    #FIX(%File,%mFileTreeFile)
    #FIX(%Secondary,%mFileTreeFile)
    #IF(%ControlHasColor)
      #IF(ITEMS(%MCRTChildRootConditionalColors))
        #FOR(%MCRTChildRootConditionalColors)
          #IF(INSTANCE(%MCRTChildRootConditionalColors) = 1)
  IF %MCRTChildRootColorCondition
          #ELSE
  ELSIF %MCRTChildRootColorCondition
          #ENDIF
    %mTreeQueue.m_NormalFG = %MCRTChildRootNormalFGConditionalColor
    %mTreeQueue.m_NormalBG = %MCRTChildRootNormalBGConditionalColor
    %mTreeQueue.m_SelectedFG = %MCRTChildRootSelectedFGConditionalColor
    %mTreeQueue.m_SelectedBG = %MCRTChildRootSelectedBGConditionalColor
        #ENDFOR
  ELSE
    %mTreeQueue.m_NormalFG = %MCRTChildRootNormalFGColor
    %mTreeQueue.m_NormalBG = %MCRTChildRootNormalBGColor
    %mTreeQueue.m_SelectedFG = %MCRTChildRootSelectedFGColor
    %mTreeQueue.m_SelectedBG = %MCRTChildRootSelectedBGColor
  END
      #ELSE
  %mTreeQueue.m_NormalFG = %MCRTChildRootNormalFGColor
  %mTreeQueue.m_NormalBG = %MCRTChildRootNormalBGColor
  %mTreeQueue.m_SelectedFG = %MCRTChildRootSelectedFGColor
  %mTreeQueue.m_SelectedBG = %MCRTChildRootSelectedBGColor
      #ENDIF
    #ENDIF
    #IF(%ControlHasIcon)
      #IF(%MCRTChildRootUseExpandAndContractIcon)
  IF %mTreeQueue.m_Loaded
     !Expanded
         #IF(ITEMS(%MCRTChildRootConditionalIcons))
           #FOR(%MCRTChildRootConditionalIcons)
             #IF(INSTANCE(%MCRTChildRootConditionalIcons) = 1)
     IF %MCRTChildRootIconCondition
             #ELSE
     ELSIF %MCRTChildRootIconCondition
             #ENDIF
             #CALL(%FixIconList,'CHILDROOT',%True,'EXPANDED')
       #INSERT(%SetLevelIcon)
           #ENDFOR
     ELSE
           #CALL(%FixIconList,'CHILDROOT',%False,'EXPANDED')
       #INSERT(%SetLevelIcon)
     END
         #ELSE
           #CALL(%FixIconList,'CHILDROOT',%False,'EXPANDED')
     #INSERT(%SetLevelIcon)
         #ENDIF
  ELSE
     !Contracted
         #IF(ITEMS(%MCRTChildRootConditionalIcons))
           #FOR(%MCRTChildRootConditionalIcons)
             #IF(INSTANCE(%MCRTChildRootConditionalIcons) = 1)
     IF %MCRTChildRootIconCondition
             #ELSE
     ELSIF %MCRTChildRootIconCondition
             #ENDIF
             #CALL(%FixIconList,'CHILDROOT',%True,'CONTRACTED')
       #INSERT(%SetLevelIcon)
           #ENDFOR
     ELSE
           #CALL(%FixIconList,'CHILDROOT',%False,'CONTRACTED')
       #INSERT(%SetLevelIcon)
     END
         #ELSE
           #CALL(%FixIconList,'CHILDROOT',%False,'CONTRACTED')
     #INSERT(%SetLevelIcon)
         #ENDIF
  END
      #ELSE
         #IF(ITEMS(%MCRTChildRootConditionalIcons))
           #FOR(%MCRTChildRootConditionalIcons)
             #IF(INSTANCE(%MCRTChildRootConditionalIcons) = 1)
  IF %MCRTChildRootIconCondition
             #ELSE
  ELSIF %MCRTChildRootIconCondition
             #ENDIF
             #CALL(%FixIconList,'CHILDROOT',%True,'DEFAULT')
    #INSERT(%SetLevelIcon)
           #ENDFOR
  ELSE
           #CALL(%FixIconList,'CHILDROOT',%False,'DEFAULT')
    #INSERT(%SetLevelIcon)
  END
         #ELSE
           #CALL(%FixIconList,'CHILDROOT',%False,'DEFAULT')
  #INSERT(%SetLevelIcon)
         #ENDIF
      #ENDIF
    #ENDIF
  #IF(%ControlHasStyle)
    #IF(ITEMS(%MCRTChildRootConditionalStyles))
      #FOR(%MCRTChildRootConditionalStyles)
        #IF(INSTANCE(%MCRTChildRootConditionalStyles) = 1)
  IF %MCRTChildRootStyleCondition
        #ELSE
  ELSIF %MCRTChildRootStyleCondition
        #ENDIF
    %mTreeQueue.m_Style = %(%GetStyleNumber(%MCRTChildRootConditionalStyleType,%MCRTChildRootConditionalStyle,%MCRTChildRootConditionalStyleLocalList)) ! %MCRTChildRootConditionalStyle
      #ENDFOR
  ELSE
    %mTreeQueue.m_Style = %(%GetStyleNumber(%MCRTChildRootStyleType,%MCRTChildRootStyle,%MCRTChildRootStyleLocalList)) ! %MCRTChildRootStyle
  END
    #ELSE
  %mTreeQueue.m_Style = %(%GetStyleNumber(%MCRTChildRootStyleType,%MCRTChildRootStyle,%MCRTChildRootStyleLocalList)) ! %MCRTChildRootStyle
    #ENDIF
  #ENDIF
  #EMBED(%MCRTAfterFormatChildRootLevel,'After Format Child Root Level'),%ActiveTemplateInstance,%mFileTreeFile,MAP(%ActiveTemplateInstance,%ActiveTemplateInstanceDescription)
  #ENDFOR #! End of cycle for ChildRoot-levels loading
!------------------------------------------------------------------------------
  #SET(%mTempVariable,%mInstancePrefix & ':ExpandAll')
%[35]mTempVariable ROUTINE
  %mTreeControl{PROP:Selected} = 1
  %mInstancePrefix:CurrentChoice = 1
  %mInstancePrefix:ForceReload = True
  DO %mInstancePrefix:ExpandLevel
!------------------------------------------------------------------------------
  #SET(%mTempVariable,%mInstancePrefix & ':OnExpanded')
%[35]mTempVariable ROUTINE
!|
!| The given routine is used for expanding of the clicked single level of
!| MultiChildRelationTree.
!|
!| First, we see where the expanding comes from. Since the alert-key handling
!| sets %mTreeControl{PROPLIST:MouseDownRow} to CHOICE, we can rely on this
!| property containing the correct selection.
!|
!| Next, we retrieve the %mTreeQueue record that corresponds to the requested
!| load row. If the requested load row is already loaded, we don't have to do
!| anything and exit from routine.
!|
!| If the requested row is not expanded:
!|
!| First,  we set %mTreeQueue.m_Level to a positive value for the selected row.
!|
!| Next,  we  add a record the the queue  %mTreeStateQueue. This queue is used
!| to rebuild the display of tree when necessary.
!|
!| Next, we save the current context of loaded level in temporary variables.
!| They are used for level-ChildRoots.
!|
!| Next, we make search for a sublevels of the loaded level to format it and
!| save the information into tree queue %mTreeQueue.
!|
!| And finally, we load all records corresponding of the specified loaded level.
!|
  #!INSERT(%GetCurrentChoice)
  %mInstancePrefix:CurrentChoice = %mTreeControl{PROPLIST:MouseDownRow}
  DO %mInstancePrefix:ExpandLevel
!------------------------------------------------------------------------------
  #SET(%mTempVariable,%mInstancePrefix & ':ExpandLevel')
%[35]mTempVariable ROUTINE
  !
  ! %mInstancePrefix:CurrentChoice shuld be fixed to a value before call this routine
  !
  GET(%mTreeQueue,%mInstancePrefix:CurrentChoice)

  IF %mTreeQueue.m_Loaded THEN EXIT.

  IF %mTreeQueue.m_Level = 0         #<! If root - expand all levels
    %mInstancePrefix:ExpandAll = TRUE
    %mInstancePrefix:ForceReload = True
    DO %mInstancePrefix:ReloadTree    #<! Refresh all tree
    EXIT
  END

  %mTreeQueue.m_Level = ABS(%mTreeQueue.m_Level)

  CLEAR(%mTreeStateQueue)
  %mTreeStateQueue.m_LoadedInstance = %mTreeQueue.m_Instance
  %mTreeStateQueue.m_LoadedPosition = %mTreeQueue.m_Position
  GET(%mTreeStateQueue,%mTreeStateQueue.m_LoadedInstance,%mTreeStateQueue.m_LoadedPosition)
  IF ERRORCODE()
    ADD(%mTreeStateQueue,%mTreeStateQueue.m_LoadedInstance,%mTreeStateQueue.m_LoadedPosition)
  END

  REGET(%mTreeQueue.m_Key,%mTreeQueue.m_Position) #<! Retrieve the level key position
  %mInstancePrefix:SubLevelExist = 0

  EXECUTE %mTreeQueue.m_Level
  #FOR(%mLevelList)
    BEGIN #<! The %mLevelList level
#!-----------------------------------------------------------------------------
#! Process the 1 level
#!-----------------------------------------------------------------------------
    #IF(%mLevelList = 1 AND NOT %DisablePrimaryLevel)
      #FOR(%mFileTreeList),WHERE(%mFileTreeLevel = 2)
      DO %mInstancePrefix:CheckSubLevel_%mFileTreeFile
      #ENDFOR
      %mTreeQueue.m_Loaded = CHOOSE(%mInstancePrefix:SubLevelExist <> 0)
      #IF(NOT %DisablePrimaryLevel)
      DO %mInstancePrefix:FormatLevel_%Primary
      #ELSE
      DO %mInstancePrefix:FormatLevel_%mFileTreeFile
      #ENDIF
      IF ~%mTreeQueue.m_Loaded
        DELETE(%mTreeStateQueue)
      END
    #ELSE
#!-----------------------------------------------------------------------------
#! Process sublevels for specified uplevel to formating (all except last)
#! If the level is last skip expanded code
#!-----------------------------------------------------------------------------
      #IF(%mLevelList = ITEMS(%mLevelList))
      EXIT
      #ELSE
        #FOR(%mFileTreeList),WHERE(%mFileTreeLevel = %mLevelList)
          #SET(%mTempInstance,INSTANCE(%mFileTreeList))
          #SET(%mTempLevel,%mFileTreeLevel)
          #SET(%mTempVariable,%mFileTreeFile)
          #SET(%mTempChildRoot,%mFileTreeChildRoot)
#!-----------------------------------------------------------------------------
#! This BREAK prevents code generation for unexistent sublevels
#!-----------------------------------------------------------------------------
          #IF(%mTempInstance = ITEMS(%mFileTreeList))
            #BREAK
          #ENDIF
      IF %mTreeQueue.m_Instance = %(%GetInstanceEquate(%mTempInstance))
          #!SET(%mTempInstance,INSTANCE(%mFileTreeList) + 1)
          #!SELECT(%mFileTreeList,%mTempInstance)
          #FOR(%mFileTreeList),WHERE(%mFileTreeParent = %mTempVariable AND %mFileTreeLevel = %mTempLevel + 1)
        DO %mInstancePrefix:CheckSubLevel_%mFileTreeFile
          #ENDFOR
          #FOR(%mFileTreeList),WHERE(%mFileTreeFile = %mTempVariable AND %mFileTreeLevel = %mTempLevel + 1)
        DO %mInstancePrefix:CheckSubLevel_%mFileTreeFile
          #ENDFOR
        %mTreeQueue.m_Loaded = CHOOSE(%mInstancePrefix:SubLevelExist <> 0)
          #IF(%mTempChildRoot)
        DO %mInstancePrefix:FormatChildRoot_%mTempVariable
          #ELSE
        DO %mInstancePrefix:FormatLevel_%mTempVariable
          #ENDIF
        IF ~%mTreeQueue.m_Loaded
          DELETE(%mTreeStateQueue)
        END
      END
        #ENDFOR
      #ENDIF
    #ENDIF
    END
  #ENDFOR
  END

  IF %mTreeQueue.m_Loaded
    PUT(%mTreeQueue)
  ELSE
    EXIT
  END

#!-----------------------------------------------------------------------------
#! Load sublevels
#!-----------------------------------------------------------------------------
  EXECUTE %mTreeQueue.m_Level
  #FOR(%mLevelList),WHERE(INSTANCE(%mLevelList) < ITEMS(%mLevelList))
    BEGIN #<! The %mLevelList level
#!-----------------------------------------------------------------------------
#! Process the 1 level
#!-----------------------------------------------------------------------------
    #IF(%mLevelList = 1 AND NOT %DisablePrimaryLevel)
      #FOR(%mFileTreeList),WHERE(%mFileTreeLevel = 2)
        #IF(%mFileTreeChildRoot)
      DO %mInstancePrefix:LoadChildRoot_%mFileTreeFile
        #ELSE
      DO %mInstancePrefix:LoadLevel_%mFileTreeFile
        #ENDIF
      #ENDFOR
    #ELSE
      #FOR(%mFileTreeList),WHERE(%mFileTreeLevel = %mLevelList)
        #SET(%mTempInstance,INSTANCE(%mFileTreeList))
        #SET(%mTempLevel,%mFileTreeLevel)
        #SET(%mTempVariable,%mFileTreeFile)
#!-----------------------------------------------------------------------------
#! This BREAK prevents code generation for unexistent sublevels
#!-----------------------------------------------------------------------------
        #IF(%mTempInstance = ITEMS(%mFileTreeList))
          #BREAK
        #ENDIF
        #SUSPEND
      #?IF %mTreeQueue.m_Instance = %(%GetInstanceEquate(%mTempInstance))
          #SUSPEND
            #FOR(%mFileTreeList),WHERE(%mFileTreeParent = %mTempVariable AND %mFileTreeLevel = %mTempLevel + 1)
              #IF(%mFileTreeChildRoot)
        DO %mInstancePrefix:LoadChildRoot_%mFileTreeFile
              #ELSE
        DO %mInstancePrefix:LoadLevel_%mFileTreeFile
              #ENDIF
            #ENDFOR
          #RESUME
          #SUSPEND
            #FOR(%mFileTreeList),WHERE(%mFileTreeFile = %mTempVariable AND %mFileTreeLevel = %mTempLevel + 1)
              #IF(%mFileTreeChildRoot)
        DO %mInstancePrefix:LoadChildRoot_%mFileTreeFile
              #ELSE
        DO %mInstancePrefix:LoadLevel_%mFileTreeFile
              #ENDIF
            #ENDFOR
          #RESUME
      #?END
        #RESUME
      #ENDFOR
    #ENDIF
    END
  #ENDFOR
  END
  Do %mInstancePrefix:OnNewSelection

!-----------------------------------------------------------------------------
  #SET(%mTempVariable,%mInstancePrefix & ':ContractAll')
%[35]mTempVariable ROUTINE
!|
!| The given routine
!|
  %mTreeControl{PROP:Selected} = 1
  %mInstancePrefix:CurrentChoice = 1
  %mInstancePrefix:ForceReload = True
  DO %mInstancePrefix:ContractLevel
!------------------------------------------------------------------------------
  #SET(%mTempVariable,%mInstancePrefix & ':OnContracted')
%[35]mTempVariable ROUTINE
!|
!| The given routine
!|
  #!INSERT(%GetCurrentChoice)
  %mInstancePrefix:CurrentChoice = %mTreeControl{PROPLIST:MouseDownRow}
  DO %mInstancePrefix:ContractLevel
!------------------------------------------------------------------------------
  #SET(%mTempVariable,%mInstancePrefix & ':ContractLevel')
%[35]mTempVariable ROUTINE
  !
  ! %mInstancePrefix:CurrentChoice shuld be fixed to a value before call this routine
  !
  GET(%mTreeQueue,%mInstancePrefix:CurrentChoice)

  IF ~%mTreeQueue.m_Loaded AND %mTreeQueue.m_Level THEN EXIT.

  IF %mTreeQueue.m_Level = 0          #<! If root - contract all levels
    %mInstancePrefix:ExpandAll = FALSE
    %mInstancePrefix:ForceReload = True
    DO %mInstancePrefix:ReloadTree    #<! Refresh all tree
    EXIT
  END

  CLEAR(%mTreeStateQueue)
  %mTreeStateQueue.m_LoadedInstance = %mTreeQueue.m_Instance
  %mTreeStateQueue.m_LoadedPosition = %mTreeQueue.m_Position
  GET(%mTreeStateQueue,%mTreeStateQueue.m_LoadedInstance,%mTreeStateQueue.m_LoadedPosition)
  IF ~ERRORCODE()
    DELETE(%mTreeStateQueue)
  END

!// Reget the level key position
  REGET(%mTreeQueue.m_Key,%mTreeQueue.m_Position)
  %mInstancePrefix:SubLevelExist = 0

  EXECUTE ABS(%mTreeQueue.m_Level)
  #FOR(%mLevelList)
    BEGIN !// The %mLevelList level
#!-----------------------------------------------------------------------------
#! Process the 1 level for checking of sublevels
#!-----------------------------------------------------------------------------
    #IF(%mLevelList = 1)
      #FOR(%mFileTreeList),WHERE(%mFileTreeLevel = 2)
      DO %mInstancePrefix:CheckSubLevel_%mFileTreeFile
      #ENDFOR
    #ELSE
#!-----------------------------------------------------------------------------
#! Process sublevels for specified uplevel to formating (all except last).
#! If the level is last skip expanded code
#!-----------------------------------------------------------------------------
      #IF(%mLevelList = ITEMS(%mLevelList))
      EXIT
      #ELSE
        #FOR(%mFileTreeList),WHERE(%mFileTreeLevel = %mLevelList)
          #SET(%mTempInstance,INSTANCE(%mFileTreeList))
          #SET(%mTempLevel,%mFileTreeLevel)
          #SET(%mTempVariable,%mFileTreeFile)
#!-----------------------------------------------------------------------------
#! This BREAK prevents code generation for unexistent sublevels
#!-----------------------------------------------------------------------------
          #IF(%mTempInstance = ITEMS(%mFileTreeList))
            #BREAK
          #ENDIF
          #SUSPEND
      #?IF %mTreeQueue.m_Instance = %(%GetInstanceEquate(%mTempInstance))
          #FOR(%mFileTreeList),WHERE(%mFileTreeParent = %mTempVariable AND %mFileTreeLevel = %mTempLevel + 1)
        DO %mInstancePrefix:CheckSubLevel_%mFileTreeFile
          #ENDFOR
          #FOR(%mFileTreeList),WHERE(%mFileTreeFile = %mTempVariable AND %mFileTreeLevel = %mTempLevel + 1)
        DO %mInstancePrefix:CheckSubLevel_%mFileTreeFile
          #ENDFOR
      #?END
          #RESUME
        #ENDFOR
      #ENDIF
    #ENDIF
    END
  #ENDFOR
  END

  %mTreeQueue.m_Level = CHOOSE(%mInstancePrefix:SubLevelExist = 0,%mTreeQueue.m_Level,-%mTreeQueue.m_Level)
  %mTreeQueue.m_Loaded = FALSE

  EXECUTE ABS(%mTreeQueue.m_Level)
  #FOR(%mLevelList)
    BEGIN #<! The %mLevelList level
#!-----------------------------------------------------------------------------
#! Format 1 level
#!-----------------------------------------------------------------------------
    #IF(%mLevelList = 1 AND NOT %DisablePrimaryLevel)
      DO %mInstancePrefix:FormatLevel_%Primary
    #ELSE
#!-----------------------------------------------------------------------------
#! Format all other sublevels (except first)
#!-----------------------------------------------------------------------------
      #IF(%mLevelList = ITEMS(%mLevelList))
      EXIT
      #ELSE
        #FOR(%mFileTreeList),WHERE(%mFileTreeLevel = %mLevelList)
          #SET(%mTempInstance,INSTANCE(%mFileTreeList))
#!-----------------------------------------------------------------------------
#! This BREAK prevents code generation for unexistent sublevels
#!-----------------------------------------------------------------------------
          #IF(%mTempInstance = ITEMS(%mFileTreeList))
            #BREAK
          #ENDIF
      IF %mTreeQueue.m_Instance = %(%GetInstanceEquate(%mTempInstance))
        REGET(%mTreeQueue.m_Key,%mTreeQueue.m_Position)
          #IF(%mFileTreeChildRoot)
        DO %mInstancePrefix:FormatChildRoot_%mFileTreeFile
          #ELSE
        DO %mInstancePrefix:FormatLevel_%mFileTreeFile
          #ENDIF
      END
        #ENDFOR
      #ENDIF
    #ENDIF
    END
  #ENDFOR
  END

  PUT(%mTreeQueue)

  %mInstancePrefix:CurrentLevel = ABS(%mTreeQueue.m_Level)
  %mInstancePrefix:CurrentChoice += 1
  LOOP
    GET(%mTreeQueue,%mInstancePrefix:CurrentChoice)
    IF ERRORCODE() THEN BREAK.
    IF ABS(%mTreeQueue.m_Level) <= %mInstancePrefix:CurrentLevel THEN BREAK.
    DELETE(%mTreeQueue)
  END
  Do %mInstancePrefix:OnNewSelection
#!-----------------------------------------------------------------------------
#!-----------------------------------------------------------------------------
#! AcceptToolbarControl ROUTINES
#!-----------------------------------------------------------------------------
#!-----------------------------------------------------------------------------
#IF (%AcceptToolbarControl)
#IF(UPPER(%AppTemplateFamily)='CLARION')
!------------------------------------------------------------------------------
%mInstancePrefix:ReltreeDispatch ROUTINE
  IF ToolbarMode = TreeMode AND ActiveReltree = %mTreeControl THEN
    CASE ACCEPTED()
    OF TBarBrwHelp
      PRESSKEY(F1Key)
    OF TBarBrwBottom TO TBarBrwUp
      %mTreeControl{PROPLIST:MouseDownRow} = CHOICE(%mTreeControl) !! Server routines assume this
      EXECUTE(ACCEPTED()-TBarBrwBottom+1)
        DO %mInstancePrefix:NextParent
        DO %mInstancePrefix:PreviousParent
        DO %mInstancePrefix:NextLevel
        DO %mInstancePrefix:PreviousLevel
        DO %mInstancePrefix:NextRecord
        DO %mInstancePrefix:PreviousRecord
      END
    #EMBED(%MCRTToolbarDispatch,'Reltree Toolbar Dispatch'),%ActiveTemplateInstance,MAP(%ActiveTemplateInstance,%ActiveTemplateinstanceDescription)
    END
    #EMBED (%MCRTAfterToolbarDispatch, 'After toolbar processed'),%ActiveTemplateInstance,TREE('After toolbar processed|MultiChildRelTree on '&%Primary)
  END
#ENDIF
!------------------------------------------------------------------------------
%mInstancePrefix:NextParent ROUTINE
  GET(%mTreeQueue,CHOICE(%mTreeControl))
  IF ABS(%mTreeQueue.m_Level) > 1 THEN
    %mInstancePrefix:SaveLevel = ABS(%mTreeQueue.m_Level)-1
    DO %mInstancePrefix:NextSavedLevel
  END
!------------------------------------------------------------------------------
%mInstancePrefix:PreviousParent ROUTINE
  GET(%mTreeQueue,CHOICE(%mTreeControl))
  IF ABS(%mTreeQueue.m_Level) > 1 THEN
    %mInstancePrefix:SaveLevel = ABS(%mTreeQueue.m_Level)-1
    DO %mInstancePrefix:PreviousSavedLevel
  ELSE
    GET(%mTreeQueue,1)
    IF ERRORCODE() THEN
       EXIT
    END
    %mInstancePrefix:SavePointer = POINTER(%mTreeQueue)
    %mInstancePrefix:SaveLevel   = 1
    %mTreeControl{PROPLIST:MouseDownRow} = %mInstancePrefix:SavePointer
    SELECT(%mTreeControl,1)
    Do %mInstancePrefix:OnNewSelection
  END
!------------------------------------------------------------------------------
%mInstancePrefix:NextLevel ROUTINE
  GET(%mTreeQueue,CHOICE(%mTreeControl))
  %mInstancePrefix:SaveLevel = ABS(%mTreeQueue.m_Level)
  DO %mInstancePrefix:NextSavedLevel
!------------------------------------------------------------------------------
%mInstancePrefix:NextSavedLevel ROUTINE
  LOOP
    LOOP
      GET(%mTreeQueue,POINTER(%mTreeQueue)+1)
      IF ERRORCODE() THEN
        EXIT                ! Unable to find another record on similar level
      END
    WHILE ABS(%mTreeQueue.m_Level) > %mInstancePrefix:SaveLevel
    IF ABS(%mTreeQueue.m_Level) = %mInstancePrefix:SaveLevel THEN
      SELECT(%mTreeControl,POINTER(%mTreeQueue))
      Do %mInstancePrefix:OnNewSelection
      EXIT
    END
    %mInstancePrefix:SavePointer = POINTER(%mTreeQueue)
    %mTreeControl{PROPLIST:MouseDownRow} = %mInstancePrefix:SavePointer
    DO %mInstancePrefix:OnExpanded
    GET(%mTreeQueue,%mInstancePrefix:SavePointer)
  END
  Do %mInstancePrefix:OnNewSelection
!------------------------------------------------------------------------------
%mInstancePrefix:PreviousSavedLevel ROUTINE
  LOOP
    LOOP
      GET(%mTreeQueue,POINTER(%mTreeQueue)-1)
      IF ERRORCODE() THEN
        EXIT                ! Unable to find another record on similar level
      END
    WHILE ABS(%mTreeQueue.m_Level) > %mInstancePrefix:SaveLevel
    IF ABS(%mTreeQueue.m_Level) = %mInstancePrefix:SaveLevel THEN
      SELECT(%mTreeControl,POINTER(%mTreeQueue))
      Do %mInstancePrefix:OnNewSelection
      EXIT
    END
    %mInstancePrefix:SavePointer = POINTER(%mTreeQueue)
    %mInstancePrefix:SaveRecords = RECORDS(%mTreeQueue)
    %mTreeControl{PROPLIST:MouseDownRow} = %mInstancePrefix:SavePointer
    DO %mInstancePrefix:OnExpanded
    IF RECORDS(%mTreeQueue) <> %mInstancePrefix:SaveRecords THEN
      %mInstancePrefix:SavePointer += 1 + RECORDS(%mTreeQueue) - %mInstancePrefix:SaveRecords
    END
    GET(%mTreeQueue,%mInstancePrefix:SavePointer)
  END
  Do %mInstancePrefix:OnNewSelection
!------------------------------------------------------------------------------
%mInstancePrefix:PreviousLevel ROUTINE
  GET(%mTreeQueue,CHOICE(%mTreeControl))
  %mInstancePrefix:SaveLevel = ABS(%mTreeQueue.m_Level)
  DO %mInstancePrefix:PreviousSavedLevel
!------------------------------------------------------------------------------
%mInstancePrefix:NextRecord ROUTINE
  #SET(%CheckChildRootCondition,'')
  #SET(%CheckChildRootConditionItems,0)
  #FOR(%mFileTreeList),WHERE(%mFileTreeChildRoot)
    #SET(%CheckChildRootConditionItems,%CheckChildRootConditionItems+1)
    #IF(%CheckChildRootCondition)
      #SET(%CheckChildRootCondition,%CheckChildRootCondition & ',' & %GetInstanceEquate(INSTANCE(%mFileTreeList)))
    #ELSE
      #SET(%CheckChildRootCondition,%GetInstanceEquate(INSTANCE(%mFileTreeList)))
    #ENDIF
  #ENDFOR
  #IF(%CheckChildRootCondition)
    #IF(%CheckChildRootConditionItems>1)
        #SET(%CheckChildRootCondition,'INLIST(' & %mTreeQueue & ':m_Instance,' & %CheckChildRootCondition & ')')
    #ELSE
        #SET(%CheckChildRootCondition,%mTreeQueue & ':m_Instance = ' & %CheckChildRootCondition)
    #ENDIF
  LOOP
  #ENDIF
     DO %mInstancePrefix:OnExpanded
     IF CHOICE(%mTreeControl) < RECORDS(%mTreeQueue)
       SELECT(%mTreeControl,CHOICE(%mTreeControl)+1)
       %mInstancePrefix:CurrentChoice=CHOICE(%mTreeControl)
       GET(%mTreeQueue,%mInstancePrefix:CurrentChoice)
  #IF(%CheckChildRootCondition)
     ELSE
       BREAK
     END
     IF NOT %CheckChildRootCondition THEN BREAK.
  END
  #ELSE
     END
  #ENDIF
  Do %mInstancePrefix:OnNewSelection

!------------------------------------------------------------------------------
%mInstancePrefix:PreviousRecord ROUTINE
  #IF(%CheckChildRootCondition)
  LOOP
  #ENDIF
     %mInstancePrefix:SavePointer = CHOICE(%mTreeControl)-1
     LOOP
       %mInstancePrefix:SaveRecords = RECORDS(%mTreeQueue)
       %mTreeControl{PROPLIST:MouseDownRow} = %mInstancePrefix:SavePointer
       DO %mInstancePrefix:OnExpanded
       IF RECORDS(%mTreeQueue) = %mInstancePrefix:SaveRecords THEN
         BREAK
       END
       %mInstancePrefix:SavePointer += RECORDS(%mTreeQueue) - %mInstancePrefix:SaveRecords
     END
     %mInstancePrefix:CurrentChoice=%mInstancePrefix:SavePointer
     GET(%mTreeQueue,%mInstancePrefix:CurrentChoice)
     SELECT(%mTreeControl,%mInstancePrefix:SavePointer)
  #IF(%CheckChildRootCondition)
  UNTIL(CHOICE(%mTreeControl)=1 OR NOT %CheckChildRootCondition)
  #ENDIF
  Do %mInstancePrefix:OnNewSelection

!------------------------------------------------------------------------------
%mInstancePrefix:AssignButtons ROUTINE
  #IF(UPPER(%AppTemplateFamily)='CLARION')
  Toolbarmode = TreeMode
  ActiveReltree = %mTreeControl
  ENABLE(TBarBrwBottom,TBarBrwUp)
  TBarBrwBottom{PROP:ToolTip}='Go to next parent record'
  TBarBrwTop{PROP:ToolTip}='Go to parent record'
  TBarBrwPageDown{PROP:ToolTip}='Go to next similar record'
  TBarBrwPageUp{PROP:ToolTip}='Go to previous similar record'
  TBarBrwDown{PROP:ToolTip}='Go to next record'
  TBarBrwUP{PROP:ToolTip}='Go to previous record'
  #ENDIF
  #EMBED(%MCRTAssignToolbarButtons,'Assign Toolbar Buttons'),%ActiveTemplateInstance,MAP(%ActiveTemplateInstance,%ActiveTemplateinstanceDescription)
  #IF(UPPER(%AppTemplateFamily)='ABC')
  Toolbar.SetTarget(%mTreeControl)
  #ENDIF
#ENDIF
#!-----------------------------------------------------------------------------
#!-----------------------------------------------------------------------------
#ENDAT
#!-----------------------------------------------------------------------------
#AT(%LocalProcedures)
  #IF(%MCRTRootUseSetupCapability)
!-----------------------------------------------------------------------------
    #SET(%mTempVariable,%mInstancePrefix & ':SetupTreeParams')
%[35]mTempVariable PROCEDURE
!|
!| The given procedure allows end user to customize relation tree at run-time.
!| Access via popup menu.
!|
#INSERT(%GenerateLevelEquates)
loc_LFontName                       STRING(32)    ! Font name of level
loc_LFontSize                       LONG          ! Font size of level
loc_LFontStyle                      LONG          ! Font style of level
loc_LColor                          SIGNED,DIM(4) ! Color array of level
loc_LPrefix                         STRING(64)    ! Prefix of level
loc_LUnderline                      BYTE          ! Underline check
loc_LItalic                         BYTE          ! Italic check
loc_LBold                           BYTE          ! Bold check
loc_LStrikeout                      BYTE          ! Strikeout check
loc_LCompleted                      BYTE          ! Completition sign
loc_LoopIndex                       SIGNED,AUTO   ! Just loop index
    #SET(%mTempVariable,'50L(1)|')
    #IF(%ControlHasColor)
      #SET(%mTempVariable,%mTempVariable & '*')
    #ELSIF(%ControlHasStyle)
      #SET(%mTempVariable,%mTempVariable & 'Y')
    #ENDIF
    #SET(%mTempVariable,%mTempVariable & 'TS(200)@s128@')
loc_Format         STRING('%mTempVariable')       ! Format string for LIST
loc_LevelQueue     QUEUE,PRE()                    ! Queue for LIST
m_Name               STRING(64)                   ! Name to display
    #IF(%ControlHasColor)
m_ColorNFG           LONG                         ! Normal FG of level
m_ColorNBG           LONG                         ! Normal BG of level
m_ColorSFG           LONG                         ! Selected FG of level
m_ColorSBG           LONG                         ! Selected BG of level
    #ENDIF
m_Level              LONG                         ! Level
    #IF(%ControlHasStyle)
m_Style              LONG                         ! Style of level
    #ENDIF
m_Instance           LONG                         ! Instance of level
m_FontName           STRING(64)                   ! Font name of level
m_FontSize           LONG                         ! Font size of level
m_FontStyle          LONG                         ! Font style of level
m_Prefix             STRING(64)                   ! Prefix of level
m_NameOrig           STRING(64)                   ! Original name of level
m_ChildRoot          BYTE                         ! ChildRoot sign
                   END

window WINDOW('Tree Custom Setup'),AT(,,148,198),FONT('Microsoft Sans Serif',8,,),CENTER,GRAY,DOUBLE
       LIST,AT(4,4,140,140),USE(?lst_TreeLevel),VSCROLL,FORMAT('50L(1)|TS(200)@s128@'),FROM(loc_LevelQueue)
       PANEL,AT(4,148,140,28),USE(?TipPanel),BEVEL(-1)
       PROMPT('To access configuration menu please click right mouse button and select appropri' &|
           'ate popup menu item'),AT(6,150,136,24),USE(?TipPrompt)
       BUTTON('&OK'),AT(44,180,48,14),USE(?btn_Save),DEFAULT
       BUTTON('Cancel'),AT(96,180,48,14),USE(?btn_Cancel)
     END

    #IF(%ControlHasStyle)
level_window WINDOW('Level Setup'),AT(,,216,184),FONT('Microsoft Sans Serif',8,,),CENTER,GRAY,DOUBLE
    #ELSIF(%ControlHasColor)
level_window WINDOW('Level Setup'),AT(,,216,96),FONT('Microsoft Sans Serif',8,,),CENTER,GRAY,DOUBLE
    #ELSE
level_window WINDOW('Level Setup'),AT(,,216,40),FONT('Microsoft Sans Serif',8,,),CENTER,GRAY,DOUBLE
    #ENDIF
       PROMPT('Prefix:'),AT(4,9),USE(?loc_LPrefix:Prompt)
       ENTRY(@s20),AT(42,8,114,12),USE(loc_LPrefix)
    #IF(%ControlHasColor OR %ControlHasStyle)
       GROUP('Colors'),AT(4,24,152,66),USE(?ColorGroup),BOXED
         PROMPT('Normal Foreground:'),AT(12,36),USE(?Prompt:NFG)
         PANEL,AT(84,36,40,10),USE(?Panel:NFG),BEVEL(-1)
         BUTTON('...'),AT(126,36,24,10),USE(?btn_NFG)
         PROMPT('Normal Background:'),AT(12,48),USE(?Prompt:NBG)
         PANEL,AT(84,48,40,10),USE(?Panel:NBG),BEVEL(-1)
         BUTTON('...'),AT(126,48,24,10),USE(?btn_NBG)
         PROMPT('Selected Foreground:'),AT(12,60),USE(?Prompt:SFG)
         PANEL,AT(84,60,40,10),USE(?Panel:SFG),BEVEL(-1)
         BUTTON('...'),AT(126,60,24,10),USE(?btn_SFG)
         PROMPT('Selected Background:'),AT(12,72),USE(?Prompt:SBG)
         PANEL,AT(84,72,40,10),USE(?Panel:SBG),BEVEL(-1)
         BUTTON('...'),AT(126,72,24,10),USE(?btn_SBG)
       END
    #ENDIF
    #IF(%ControlHasStyle)
       GROUP('Font'),AT(4,92,152,88),USE(?FontGroup),BOXED
         PROMPT('Face:'),AT(11,104),USE(?loc_LFontName:Prompt)
         ENTRY(@s32),AT(11,116,108,12),USE(loc_LFontName),DISABLE
         BUTTON('...'),AT(96,104,24,10),USE(?btn_Font)
         PROMPT('Size:'),AT(124,104),USE(?loc_LFontSize:Prompt)
         SPIN(@n_3B),AT(124,116,24,12),USE(loc_LFontSize),RIGHT,RANGE(0,100),STEP(1)
         GROUP('Font Styles'),AT(12,132,136,40),USE(?StylesGroup),BOXED
           CHECK('Underline'),AT(20,144),USE(loc_LUnderline),VALUE('1','0')
           CHECK('Italic'),AT(20,156),USE(loc_LItalic),VALUE('1','0')
           CHECK('Bold'),AT(100,144),USE(loc_LBold),VALUE('1','0')
           CHECK('Strikeout'),AT(100,156),USE(loc_LStrikeout)
         END
       END
    #ENDIF
       BUTTON('&OK'),AT(164,4,48,14),USE(?btn_LSave),DEFAULT
       BUTTON('Cancel'),AT(164,20,48,14),USE(?btn_LCancel)
     END

  CODE
  OPEN(window)
  DO %mInstancePrefix:PrepareProcedure
  DISPLAY
  ACCEPT
    CASE EVENT()
    OF EVENT:Completed
      LOOP loc_LoopIndex = 1 TO RECORDS(loc_LevelQueue)
        GET(loc_LevelQueue,loc_LoopIndex)
    #IF(%ControlHasStyle)
        PUTINI(%(%ConvertData(%mInstancePrefix & ':Parameters')),'ColorNFG_'  & loc_LevelQueue.m_Instance,?lst_TreeLevel{PROPSTYLE:TextColor,loc_LevelQueue.m_Instance},%(%StripPling(%INIFileName)))
        PUTINI(%(%ConvertData(%mInstancePrefix & ':Parameters')),'ColorNBG_'  & loc_LevelQueue.m_Instance,?lst_TreeLevel{PROPSTYLE:BackColor,loc_LevelQueue.m_Instance},%(%StripPling(%INIFileName)))
        PUTINI(%(%ConvertData(%mInstancePrefix & ':Parameters')),'ColorSFG_'  & loc_LevelQueue.m_Instance,?lst_TreeLevel{PROPSTYLE:TextSelected,loc_LevelQueue.m_Instance},%(%StripPling(%INIFileName)))
        PUTINI(%(%ConvertData(%mInstancePrefix & ':Parameters')),'ColorSBG_'  & loc_LevelQueue.m_Instance,?lst_TreeLevel{PROPSTYLE:BackSelected,loc_LevelQueue.m_Instance},%(%StripPling(%INIFileName)))
        PUTINI(%(%ConvertData(%mInstancePrefix & ':Parameters')),'FontName_'  & loc_LevelQueue.m_Instance,loc_LevelQueue.m_FontName,%(%StripPling(%INIFileName)))
        PUTINI(%(%ConvertData(%mInstancePrefix & ':Parameters')),'FontSize_'  & loc_LevelQueue.m_Instance,loc_LevelQueue.m_FontSize,%(%StripPling(%INIFileName)))
        PUTINI(%(%ConvertData(%mInstancePrefix & ':Parameters')),'FontStyle_' & loc_LevelQueue.m_Instance,loc_LevelQueue.m_FontStyle,%(%StripPling(%INIFileName)))
        PUTINI(%(%ConvertData(%mInstancePrefix & ':Parameters')),'Prefix_'    & loc_LevelQueue.m_Instance,loc_LevelQueue.m_Prefix,%(%StripPling(%INIFileName)))
    #ELSIF(%ControlHasColor)
        PUTINI(%(%ConvertData(%mInstancePrefix & ':Parameters')),'ColorNFG_' & loc_LevelQueue.m_Instance,loc_LevelQueue.m_ColorNFG,%(%StripPling(%INIFileName)))
        PUTINI(%(%ConvertData(%mInstancePrefix & ':Parameters')),'ColorNBG_' & loc_LevelQueue.m_Instance,loc_LevelQueue.m_ColorNBG,%(%StripPling(%INIFileName)))
        PUTINI(%(%ConvertData(%mInstancePrefix & ':Parameters')),'ColorSFG_' & loc_LevelQueue.m_Instance,loc_LevelQueue.m_ColorSFG,%(%StripPling(%INIFileName)))
        PUTINI(%(%ConvertData(%mInstancePrefix & ':Parameters')),'ColorSBG_' & loc_LevelQueue.m_Instance,loc_LevelQueue.m_ColorSBG,%(%StripPling(%INIFileName)))
    #ENDIF
      END
      POST(EVENT:CloseWindow)
    END
    CASE FIELD()
    OF ?lst_TreeLevel
      CASE EVENT()
      OF EVENT:Expanding
        CYCLE
      OF EVENT:Contracting
        CYCLE
      OF EVENT:NewSelection
        GET(loc_LevelQueue,CHOICE(?lst_TreeLevel))
        IF KEYCODE() = MouseRight
          EXECUTE POPUP(%(%TranslateMacro('SETUP_ITEM')))
            BEGIN
              DO %mInstancePrefix:SetupLevel
              IF loc_LCompleted
    #IF(%ControlHasStyle)
                IF loc_LColor[1]
                  ?lst_TreeLevel{PROPSTYLE:TextColor,loc_LevelQueue.m_Instance} = loc_LColor[1]
                END
                IF loc_LColor[2]
                  ?lst_TreeLevel{PROPSTYLE:BackColor,loc_LevelQueue.m_Instance} = loc_LColor[2]
                END
                IF loc_LColor[3]
                  ?lst_TreeLevel{PROPSTYLE:TextSelected,loc_LevelQueue.m_Instance} = loc_LColor[3]
                END
                IF loc_LColor[4]
                  ?lst_TreeLevel{PROPSTYLE:BackSelected,loc_LevelQueue.m_Instance} = loc_LColor[4]
                END
                IF loc_LFontName
                  ?lst_TreeLevel{PROPSTYLE:FontName,loc_LevelQueue.m_Instance} = loc_LFontName
                  loc_LevelQueue.m_FontName = loc_LFontName
                END
                IF loc_LFontSize
                  ?lst_TreeLevel{PROPSTYLE:FontSize,loc_LevelQueue.m_Instance} = loc_LFontSize
                  loc_LevelQueue.m_FontSize = loc_LFontSize
                END
                IF loc_LFontStyle
                  ?lst_TreeLevel{PROPSTYLE:FontStyle,loc_LevelQueue.m_Instance} = loc_LFontStyle
                  loc_LevelQueue.m_FontStyle = loc_LFontStyle
                END
                IF loc_LPrefix
                  loc_LevelQueue.m_Name = CLIP(loc_LPrefix) & ' ' & loc_LevelQueue.m_NameOrig
                  loc_LevelQueue.m_Prefix = loc_LPrefix
                END
    #ELSIF(%ControlHasColor)
                IF loc_LColor[1]
                  loc_LevelQueue.m_ColorNFG = loc_LColor[1]
                END
                IF loc_LColor[2]
                  loc_LevelQueue.m_ColorNBG = loc_LColor[2]
                END
                IF loc_LColor[3]
                  loc_LevelQueue.m_ColorSFG = loc_LColor[3]
                END
                IF loc_LColor[4]
                  loc_LevelQueue.m_ColorSBG = loc_LColor[4]
                END
    #ENDIF
                PUT(loc_LevelQueue)
                DISPLAY
              END
            END
          END
        END
      END
    OF ?btn_Save
      CASE EVENT()
      OF EVENT:Accepted
        POST(EVENT:Completed)
      END
    OF ?btn_Cancel
      CASE EVENT()
      OF EVENT:Accepted
        POST(EVENT:CloseWindow)
      END
    END
  END
  CLOSE(window)
!------------------------------------------------------------------------------
    #SET(%mTempVariable,%mInstancePrefix & ':PrepareProcedure')
%[35]mTempVariable ROUTINE
!|
!| The given routine prepares level's queue to display on the setup window.
!| It also makes some customizing.
!|
  #!%mInstancePrefix:RemoveIcon(window)
    #IF(%ControlHasColor OR %ControlHasStyle)
  ?lst_TreeLevel{PROP:Format} = loc_Format
    #ENDIF
  ?lst_TreeLevel{PROP:LineHeight} = ?lst_TreeLevel{PROP:LineHeight} + 2
#!    #IF(%MCRTLanguage <> 'English')
  window{PROP:Text} = %(%TranslateMacro('SETUP_TITLE'))
  ?btn_Save{PROP:Text} = %(%TranslateMacro('SETUP_SAVE'))
  ?btn_Cancel{PROP:Text} = %(%TranslateMacro('SETUP_CANCEL'))
  ?TipPrompt{PROP:Text} = %(%TranslateMacro('SETUP_TIP'))
#!      #IF(INSTRING(%MCRTLanguage,'Russian|Ukrainian',1,1))
#!  ?btn_Save{PROP:Key} = AltC
#!      #ENDIF
#!    #ENDIF
  FREE(loc_LevelQueue)
  CLEAR(loc_LevelQueue)
  loc_LevelQueue.m_Name      = %(%TranslateMacro('SETUP_ROOT'))
  loc_LevelQueue.m_NameOrig  = loc_LevelQueue.m_Name
    #IF(%ControlHasColor)
  loc_LevelQueue.m_ColorNFG  = COLOR:None
  loc_LevelQueue.m_ColorNBG  = COLOR:None
  loc_LevelQueue.m_ColorSFG  = COLOR:None
  loc_LevelQueue.m_ColorSBG  = COLOR:None
    #ENDIF
  loc_LevelQueue.m_Level     = 0
    #IF(%ControlHasStyle)
  loc_LevelQueue.m_Style     = 0
    #ENDIF
  loc_LevelQueue.m_Instance  = 0
  ADD(loc_LevelQueue)
    #FOR(%mFileTreeList)
  CLEAR(loc_LevelQueue)
      #IF(%mFileTreeChildRoot)
  loc_LevelQueue.m_Name      = %(%ConvertData(%mFileTreeFile)) & ' ' & %(%TranslateMacro('SETUP_CHILDROOT'))
  loc_LevelQueue.m_ChildRoot = TRUE
      #ELSE
  loc_LevelQueue.m_Name      = %(%ConvertData(%mFileTreeFile))
  loc_LevelQueue.m_NameOrig  = loc_LevelQueue.m_Name
      #ENDIF
      #IF(%ControlHasColor)
  loc_LevelQueue.m_ColorNFG  = COLOR:None
  loc_LevelQueue.m_ColorNBG  = COLOR:None
  loc_LevelQueue.m_ColorSFG  = COLOR:None
  loc_LevelQueue.m_ColorSBG  = COLOR:None
      #ENDIF
  loc_LevelQueue.m_Level     = %mFileTreeLevel
      #IF(%ControlHasStyle)
  loc_LevelQueue.m_Style     = 0
      #ENDIF
  loc_LevelQueue.m_Instance  = %(%GetInstanceEquate(INSTANCE(%mFileTreeList)))
  ADD(loc_LevelQueue)
    #ENDFOR
  LOOP loc_LoopIndex = 1 TO RECORDS(loc_LevelQueue)
    GET(loc_LevelQueue,loc_LoopIndex)
    #IF(%ControlHasColor OR %ControlHasStyle)
    loc_LColor[1] = GETINI(%(%ConvertData(%mInstancePrefix & ':Parameters')),'ColorNFG_'  & loc_LevelQueue.m_Instance,-1,%(%StripPling(%INIFileName)))
    loc_LColor[2] = GETINI(%(%ConvertData(%mInstancePrefix & ':Parameters')),'ColorNBG_'  & loc_LevelQueue.m_Instance,-1,%(%StripPling(%INIFileName)))
    loc_LColor[3] = GETINI(%(%ConvertData(%mInstancePrefix & ':Parameters')),'ColorSFG_'  & loc_LevelQueue.m_Instance,-1,%(%StripPling(%INIFileName)))
    loc_LColor[4] = GETINI(%(%ConvertData(%mInstancePrefix & ':Parameters')),'ColorSBG_'  & loc_LevelQueue.m_Instance,-1,%(%StripPling(%INIFileName)))
    #ENDIF
    #IF(%ControlHasColor)
    IF loc_LColor[1]
      loc_LevelQueue.m_ColorNFG = loc_LColor[1]
    END
    IF loc_LColor[2]
      loc_LevelQueue.m_ColorNBG = loc_LColor[2]
    END
    IF loc_LColor[3]
      loc_LevelQueue.m_ColorSFG = loc_LColor[3]
    END
    IF loc_LColor[4]
      loc_LevelQueue.m_ColorSBG = loc_LColor[4]
    END
    #ENDIF
    #IF(%ControlHasStyle)
    loc_LFontName  = GETINI(%(%ConvertData(%mInstancePrefix & ':Parameters')),'FontName_'  & loc_LevelQueue.m_Instance,,%(%StripPling(%INIFileName)))
    loc_LFontSize  = GETINI(%(%ConvertData(%mInstancePrefix & ':Parameters')),'FontSize_'  & loc_LevelQueue.m_Instance,,%(%StripPling(%INIFileName)))
    loc_LFontStyle = GETINI(%(%ConvertData(%mInstancePrefix & ':Parameters')),'FontStyle_' & loc_LevelQueue.m_Instance,,%(%StripPling(%INIFileName)))
    loc_LPrefix    = GETINI(%(%ConvertData(%mInstancePrefix & ':Parameters')),'Prefix_'    & loc_LevelQueue.m_Instance,,%(%StripPling(%INIFileName)))
    IF loc_LColor[1]
      ?lst_TreeLevel{PROPSTYLE:TextColor,loc_LevelQueue.m_Instance} = loc_LColor[1]
    END
    IF loc_LColor[2]
      ?lst_TreeLevel{PROPSTYLE:BackColor,loc_LevelQueue.m_Instance} = loc_LColor[2]
    END
    IF loc_LColor[3]
      ?lst_TreeLevel{PROPSTYLE:TextSelected,loc_LevelQueue.m_Instance} = loc_LColor[3]
    END
    IF loc_LColor[4]
      ?lst_TreeLevel{PROPSTYLE:BackSelected,loc_LevelQueue.m_Instance} = loc_LColor[4]
    END
    IF loc_LFontName
      ?lst_TreeLevel{PROPSTYLE:FontName,loc_LevelQueue.m_Instance} = loc_LFontName
      loc_LevelQueue.m_FontName = loc_LFontName
    END
    IF loc_LFontSize
      ?lst_TreeLevel{PROPSTYLE:FontSize,loc_LevelQueue.m_Instance} = loc_LFontSize
      loc_LevelQueue.m_FontSize = loc_LFontSize
    END
    IF loc_LFontStyle
      ?lst_TreeLevel{PROPSTYLE:FontStyle,loc_LevelQueue.m_Instance} = loc_LFontStyle
      loc_LevelQueue.m_FontStyle = loc_LFontStyle
    END
    IF loc_LPrefix
      loc_LevelQueue.m_Name = CLIP(loc_LPrefix) & ' ' & loc_LevelQueue.m_NameOrig
      loc_LevelQueue.m_Prefix = loc_LPrefix
    END
    loc_LevelQueue.m_Style = loc_LevelQueue.m_Instance
    #ENDIF
    #IF(%ControlHasColor OR %ControlHasStyle)
    PUT(loc_LevelQueue)
    #ENDIF
  END
  SELECT(?lst_TreeLevel,1)
  POST(EVENT:NewSelection,?lst_TreeLevel)
!-----------------------------------------------------------------------------
    #SET(%mTempVariable,%mInstancePrefix & ':SetupLevel')
%[35]mTempVariable ROUTINE
!|
!| The given routine processes the edit style window.
!|
!| First, all variables are cleared.
!|
!| Then user makes a choice and variables are filled with a new values that
!| assigned to variables of displayed level queue if user press OK. If user
!| clicks OK the variable loc_LCompleted accept TRUE value.
!|
  CLEAR(loc_LCompleted)
  CLEAR(loc_LColor[])
  CLEAR(loc_LFontName)
  CLEAR(loc_LFontSize)
  CLEAR(loc_LPrefix)

  OPEN(level_window)
  DO %mInstancePrefix:PrepareWindow
  ACCEPT
    CASE EVENT()
    OF EVENT:Completed
      loc_LFontStyle = 0
      IF loc_LBold
        loc_LFontStyle = BOR(loc_LFontStyle,FONT:Bold)
      END
      IF loc_LItalic
        loc_LFontStyle = BOR(loc_LFontStyle,FONT:Italic)
      END
      IF loc_LUnderline
        loc_LFontStyle = BOR(loc_LFontStyle,FONT:Underline)
      END
      IF loc_LStrikeout
        loc_LFontStyle = BOR(loc_LFontStyle,FONT:Strikeout)
      END
      loc_LCompleted = TRUE
      POST(EVENT:CloseWindow)
    END
    CASE FIELD()
    #IF(%ControlHasStyle)
    OF ?btn_Font
      CASE EVENT()
      OF EVENT:Accepted
        IF FONTDIALOG(%(%TranslateMacro('SETUP_CHOOSE')) & ' ' & %(LOWER(%TranslateMacro('SETUP_FONT'))),loc_LFontName,loc_LFontSize)
          DISPLAY
        END
      END
    #ENDIF
    #IF(%ControlHasColor OR %ControlHasStyle)
    OF ?btn_NFG
      CASE EVENT()
      OF EVENT:Accepted
        IF COLORDIALOG(%(%TranslateMacro('SETUP_CHOOSE')) & ' ' & %(LOWER(%TranslateMacro('SETUP_NFG'))),loc_LColor[1])
          ?Panel:NFG{PROP:Fill} = loc_LColor[1]
          DISPLAY
        END
      END
    OF ?btn_NBG
      CASE EVENT()
      OF EVENT:Accepted
        IF COLORDIALOG(%(%TranslateMacro('SETUP_CHOOSE')) & ' ' & %(LOWER(%TranslateMacro('SETUP_NBG'))),loc_LColor[2])
          ?Panel:NBG{PROP:Fill} = loc_LColor[2]
          DISPLAY
        END
      END
    OF ?btn_SFG
      CASE EVENT()
      OF EVENT:Accepted
        IF COLORDIALOG(%(%TranslateMacro('SETUP_CHOOSE')) & ' ' & %(LOWER(%TranslateMacro('SETUP_SFG'))),loc_LColor[3])
          ?Panel:SFG{PROP:Fill} = loc_LColor[3]
          DISPLAY
        END
      END
    OF ?btn_SBG
      CASE EVENT()
      OF EVENT:Accepted
        IF COLORDIALOG(%(%TranslateMacro('SETUP_CHOOSE')) & ' ' & %(LOWER(%TranslateMacro('SETUP_SBG'))),loc_LColor[4])
          ?Panel:SBG{PROP:Fill} = loc_LColor[4]
          DISPLAY
        END
      END
    #ENDIF
    OF ?btn_LSave
      CASE EVENT()
      OF EVENT:Accepted
        POST(EVENT:Completed)
      END
    OF ?btn_LCancel
      CASE EVENT()
      OF EVENT:Accepted
        POST(EVENT:CloseWindow)
      END
    END
  END
  CLOSE(level_window)
!------------------------------------------------------------------------------
    #SET(%mTempVariable,%mInstancePrefix & ':PrepareWindow')
%[35]mTempVariable ROUTINE
!|
!| The given routine prepare style edit window. It translates all window text
!| if set language are different from English and also makes some customization.
!|
  #!%mInstancePrefix:RemoveIcon(level_window)
#!    #IF(%MCRTLanguage <> 'English')
  level_window{PROP:Text} = %(%TranslateMacro('SETUP_LEVEL_TITLE'))
  ?loc_LPrefix:Prompt{PROP:Text} = %(%TranslateMacro('SETUP_LEVEL_PREFIX'))
      #IF(%ControlHasColor OR %ControlHasStyle)
  ?ColorGroup{PROP:Text} = %(%TranslateMacro('SETUP_LEVEL_COLOR_GROUP'))
  ?Prompt:NFG{PROP:Text} = %(%TranslateMacro('SETUP_LEVEL_NFG'))
  ?Prompt:NBG{PROP:Text} = %(%TranslateMacro('SETUP_LEVEL_NBG'))
  ?Prompt:SFG{PROP:Text} = %(%TranslateMacro('SETUP_LEVEL_SFG'))
  ?Prompt:SBG{PROP:Text} = %(%TranslateMacro('SETUP_LEVEL_SBG'))
      #ENDIF
      #IF(%ControlHasStyle)
  ?FontGroup{PROP:Text} = %(%TranslateMacro('SETUP_LEVEL_FONT_GROUP'))
  ?loc_LFontName:Prompt{PROP:Text} = %(%TranslateMacro('SETUP_LEVEL_FONT'))
  ?loc_LFontSize:Prompt{PROP:Text} = %(%TranslateMacro('SETUP_LEVEL_SIZE'))
  ?StylesGroup{PROP:Text} = %(%TranslateMacro('SETUP_LEVEL_STYLE_GROUP'))
  ?loc_LUnderline{PROP:Text} = %(%TranslateMacro('SETUP_LEVEL_UNDERLINE'))
  ?loc_LItalic{PROP:Text} = %(%TranslateMacro('SETUP_LEVEL_ITALIC'))
  ?loc_LBold{PROP:Text} = %(%TranslateMacro('SETUP_LEVEL_BOLD'))
  ?loc_LStrikeout{PROP:Text} = %(%TranslateMacro('SETUP_LEVEL_STRIKEOUT'))
      #ENDIF
  ?btn_LSave{PROP:Text} = %(%TranslateMacro('SETUP_LEVEL_OK'))
  ?btn_LCancel{PROP:Text} = %(%TranslateMacro('SETUP_LEVEL_CANCEL'))
#!    #ENDIF
  IF loc_LevelQueue.m_Instance = 0 OR loc_LevelQueue.m_ChildRoot
    DISABLE(?loc_LPrefix:Prompt,?loc_LPrefix)
  END
      #IF(%ControlHasColor)
  loc_LColor[1] = GETINI(%(%ConvertData(%mInstancePrefix & ':Parameters')),'ColorNFG_'  & loc_LevelQueue.m_Instance,,%(%StripPling(%INIFileName)))
  loc_LColor[2] = GETINI(%(%ConvertData(%mInstancePrefix & ':Parameters')),'ColorNBG_'  & loc_LevelQueue.m_Instance,,%(%StripPling(%INIFileName)))
  loc_LColor[3] = GETINI(%(%ConvertData(%mInstancePrefix & ':Parameters')),'ColorSFG_'  & loc_LevelQueue.m_Instance,,%(%StripPling(%INIFileName)))
  loc_LColor[4] = GETINI(%(%ConvertData(%mInstancePrefix & ':Parameters')),'ColorSBG_'  & loc_LevelQueue.m_Instance,,%(%StripPling(%INIFileName)))
  loc_LPrefix   = GETINI(%(%ConvertData(%mInstancePrefix & ':Parameters')),'Prefix_'    & loc_LevelQueue.m_Instance,,%(%StripPling(%INIFileName)))
  IF loc_LColor[1]
    ?Panel:NFG{PROP:Fill} = loc_LColor[1]
  END
  IF loc_LColor[2]
    ?Panel:NBG{PROP:Fill} = loc_LColor[2]
  END
  IF loc_LColor[3]
    ?Panel:SFG{PROP:Fill} = loc_LColor[3]
  END
  IF loc_LColor[4]
    ?Panel:SBG{PROP:Fill} = loc_LColor[4]
  END
      #ENDIF
  DISPLAY
!-----------------------------------------------------------------------------
#!    #SET(%mTempVariable,%mInstancePrefix & ':RemoveIcon')
#!%[35]mTempVariable PROCEDURE(WINDOW _Wnd)
#!!|
#!!|
#!!|
#!  CODE
#!  %mInstancePrefix:SetWindowLong(_Wnd{PROP:Handle},-16,BAND(%mInstancePrefix:GetWindowLong(_Wnd{PROP:Handle},-16),BXOR(20000h,-1)))
  #ENDIF #!------------------------ End of IF(%MCRTRootUseSetupCapability) ---
#ENDAT
#!=============================================================================
#! CONTROL TEMPLATE
#!=============================================================================
#CONTROL(MultiChildRelationTreeUpdateButtons,'Multi Children Relation Tree Update Buttons'),DESCRIPTION('Update a Record from Tree'),WINDOW,HLP('~IDH_MultiChildRelationTreeButtons'),REQ(MultiChildRelationTree)
  CONTROLS
    BUTTON('Insert'),AT(,,48,14),USE(?Insert)
    BUTTON('Change'),AT(52,0,48,14),USE(?Change)
    BUTTON('Delete'),AT(52,0,48,14),USE(?Delete)
    BUTTON('View'),AT(52,0,48,14),USE(?View)
  END
#!=============================================================================
#BUTTON('Update a Record from Tree'),AT(10,,180)
#SHEET,ADJUST
  #TAB('General')
    #PROMPT('Use Update via &Popup',CHECK),%MCRTUpdateViaPopup,DEFAULT(%True),AT(10)
    #ENABLE(%MCRTUpdateViaPopup)
      #PROMPT('Add Separation',CHECK),%MCRTUseViaPopupSeparation,DEFAULT(%True),AT(10)
      #PROMPT('Use Button &Text for Popup',CHECK),%MCRTUseButtonText,DEFAULT(%True),AT(10)
    #ENDENABLE
    #PROMPT('Use &Alternative Update',CHECK),%MCRTUseAlternativeUpdate,DEFAULT(%False),AT(10)
    #ENABLE(%MCRTUseAlternativeUpdate)
      #PROMPT('&Activation Key',KEYCODE),%MCRTActivationKey,DEFAULT('AltInsert'),REQ
      #PROMPT('&Use Help Tooltip',CHECK),%MCRTUseHelpTooltip,DEFAULT(%True)
    #ENDENABLE
    #PROMPT('Popup priority:',@n3),%MCRTUpdateButtonPriority,DEFAULT(100)
  #ENDTAB
  #TAB('Primary Level'),WHERE(%DisablePrimaryLevel)
    #DISPLAY
    #DISPLAY('Your MultiChildRelationTree control is not set')
    #DISPLAY('to use a common Root using the Primary table.')
  #ENDTAB
  #TAB('Primary Level'),WHERE(NOT %DisablePrimaryLevel),CLEAR
    #BOXED('Update Procedure'),SECTION
      #DISPLAY('File: '&%Primary)
      #PROMPT('&Update Procedure:',PROCEDURE),%MCRTPrimaryUpdateProcedure
      #PROMPT('Call with &parameters:',@s255),%MCRTPrimaryUpdateProcedureParameters
      #PROMPT('Allow Insert',CHECK),%MCRTPrimaryAllowInsert,DEFAULT(%True),AT(10)
      #PROMPT('Allow Change',CHECK),%MCRTPrimaryAllowChange,DEFAULT(%True),AT(10)
      #PROMPT('Allow Delete',CHECK),%MCRTPrimaryAllowDelete,DEFAULT(%True),AT(10)
      #PROMPT('Allow View',CHECK),%MCRTPrimaryAllowView,DEFAULT(%True),AT(10)
    #ENDBOXED
  #ENDTAB
  #TAB('Child Levels')
    #BOXED('Update Procedures')
      #BUTTON('Secondary Level'),FROM(%Secondary,CHOOSE(LEN(CLIP(%MCRTChildUpdateProcedure)) = 0,%Secondary & ' is not edited','Edit ' & %Secondary & ' with ' & %MCRTChildUpdateProcedure)),INLINE,WHERE(%SecondaryType = 'MANY:1' OR %SecondaryType = '')
        #BOXED,SECTION
          #DISPLAY('File: '&%Secondary)
          #PROMPT('&Update Procedure:',PROCEDURE),%MCRTChildUpdateProcedure
          #PROMPT('Call with &parameters:',@s255),%MCRTChildUpdateProcedureParameters
          #PROMPT('&Description:',@s64),%MCRTChildUpdateDescription
          #PROMPT('Allow Insert',CHECK),%MCRTSecondaryAllowInsert,DEFAULT(%True),AT(10)
          #PROMPT('Allow Change',CHECK),%MCRTSecondaryAllowChange,DEFAULT(%True),AT(10)
          #PROMPT('Allow Delete',CHECK),%MCRTSecondaryAllowDelete,DEFAULT(%True),AT(10)
          #PROMPT('Allow View',CHECK),%MCRTSecondaryAllowView,DEFAULT(%True),AT(10)
          #PROMPT('Allow MANY:1 child Autoinc',CHECK),%MCRTSecondaryAllowMOChildAutoinc,DEFAULT(%False),AT(10)
        #ENDBOXED
      #ENDBUTTON
    #ENDBOXED
  #ENDTAB
#ENDSHEET
#ENDBUTTON
#!-----------------------------------------------------------------------------
#ATSTART
  #DECLARE(%mInsertControl)
  #DECLARE(%mChangeControl)
  #DECLARE(%mDeleteControl)
  #DECLARE(%mViewControl)
  #DECLARE(%mInsertPopupText)
  #DECLARE(%mChangePopupText)
  #DECLARE(%mDeletePopupText)
  #DECLARE(%mViewPopupText)
  #DECLARE(%mPopupString)
  #DECLARE(%mTempChildren,LONG)
  #DECLARE(%mMCRTSecondaryAllowMOChildAutoinc)
  #FOR(%Control),WHERE(%ControlInstance = %ActiveTemplateInstance)
    #CASE(%ControlOriginal)
    #OF('?Insert')
      #SET(%mInsertControl,%Control)
      #IF(%MCRTUseButtonText)
        #SET(%mInsertPopupText,EXTRACT(%ControlStatement,'BUTTON',1))
      #ELSE
        #SET(%mInsertPopupText,%TranslateMacro('POPUP_INSERT'))
      #ENDIF
    #OF('?Change')
      #SET(%mChangeControl,%Control)
      #IF(%MCRTUseButtonText)
        #SET(%mChangePopupText,EXTRACT(%ControlStatement,'BUTTON',1))
      #ELSE
        #SET(%mChangePopupText,%TranslateMacro('POPUP_CHANGE'))
      #ENDIF
    #OF('?Delete')
      #SET(%mDeleteControl,%Control)
      #IF(%MCRTUseButtonText)
        #SET(%mDeletePopupText,EXTRACT(%ControlStatement,'BUTTON',1))
      #ELSE
        #SET(%mDeletePopupText,%TranslateMacro('POPUP_DELETE'))
      #ENDIF
    #OF('?View')
      #SET(%mViewControl,%Control)
      #IF(%MCRTUseButtonText)
        #SET(%mViewPopupText,EXTRACT(%ControlStatement,'BUTTON',1))
      #ELSE
        #SET(%mViewPopupText,%TranslateMacro('POPUP_VIEW'))
      #ENDIF
    #ENDCASE
  #ENDFOR
#ENDAT
#!-----------------------------------------------------------------------------
#AT(%DataSectionBeforeWindow)
  #COMMENT(51)
  #IF(%MCRTUseAlternativeUpdate AND %MCRTActivationKey)
    #SET(%mTempVariable,%mInstancePrefix & ':AltInsert')
%[20]mTempVariable BYTE      #<! Sign of alternative adding
  #ENDIF
#ENDAT
#!-----------------------------------------------------------------------------
#AT(%MCRTDeclareUpdateAlerts,%ActiveTemplateParentInstance)
  #IF(%mInsertControl)
%mTreeControl{PROP:Alrt,255} = InsertKey
    #IF(%MCRTUseAlternativeUpdate AND %MCRTActivationKey)
%mTreeControl{PROP:Alrt,255} = %MCRTActivationKey
    #ENDIF
  #ENDIF
  #IF(%mChangeControl OR %mViewControl)
%mTreeControl{PROP:Alrt,255} = CtrlEnter
%mTreeControl{PROP:Alrt,255} = MouseLeft2
  #ENDIF
  #IF(%mDeleteControl)
%mTreeControl{PROP:Alrt,255} = DeleteKey
  #ENDIF
  #IF(%MCRTUseHelpTooltip AND %MCRTUseAlternativeUpdate)
%mTreeControl{PROP:Tip} = %(%TranslateMacro('INFO_TIP')) & ' "%MCRTActivationKey"'
  #ENDIF
#ENDAT
#!-----------------------------------------------------------------------------
#AT(%MCRTProcessUpdateAlerts,%ActiveTemplateParentInstance)
  #IF(%mInsertControl)
OF InsertKey
  IF ~%mInsertControl{PROP:Disable}
    POST(EVENT:Accepted,%mInsertControl)
  END
    #IF(%MCRTUseAlternativeUpdate AND %MCRTActivationKey)
OF %MCRTActivationKey
  IF ~%mInsertControl{PROP:Disable}
    %mInstancePrefix:AltInsert = TRUE
    POST(EVENT:Accepted,%mInsertControl)
  END
    #ENDIF
  #ENDIF
  #IF(%mChangeControl)
OF CtrlEnter
OROF MouseLeft2
  IF ~%mChangeControl{PROP:Disable}
    POST(EVENT:Accepted,%mChangeControl)
  END
  #ELSE
     #IF(%mViewControl)
OF CtrlEnter
OROF MouseLeft2
  IF ~%mViewControl{PROP:Disable}
    POST(EVENT:Accepted,%mViewControl)
  END
     #ENDIF
  #ENDIF
  #IF(%mDeleteControl)
OF DeleteKey
  IF ~%mDeleteControl{PROP:Disable}
    POST(EVENT:Accepted,%mDeleteControl)
  END
  #ENDIF
#ENDAT
#!-----------------------------------------------------------------------------
#AT(%MCRTGeneratePopup,%ActiveTemplateParentInstance),WHERE(%MCRTUpdateViaPopup)
#IF((%mInsertControl OR %mChangeControl OR %mDeleteControl) AND %MCRTUseViaPopupSeparation)
#CALL(%AddSeparationToPopupMenu,%MCRTUpdateButtonPriority)
#ENDIF
#IF(%mInsertControl)
#CALL(%AddItemToPopupMenu,'INSERT_BUTTON',%MCRTUpdateButtonPriority,'!'&%mInsertPopupText,'POST(EVENT:Accepted,'&%mInsertControl&')','NOT '&%mInsertControl&'{PROP:Disable}','')
#ENDIF
#IF(%mChangeControl)
#CALL(%AddItemToPopupMenu,'CHANGE_BUTTON',%MCRTUpdateButtonPriority+10,'!'&%mChangePopupText,'POST(EVENT:Accepted,'&%mChangeControl&')','NOT '&%mChangeControl&'{PROP:Disable}','')
#ENDIF
#IF(%mDeleteControl)
#CALL(%AddItemToPopupMenu,'DELETE_BUTTON',%MCRTUpdateButtonPriority+20,'!'&%mDeletePopupText,'POST(EVENT:Accepted,'&%mDeleteControl&')','NOT '&%mDeleteControl&'{PROP:Disable}','')
#ENDIF
#ENDAT
#!-----------------------------------------------------------------------------
#AT(%MCRTDisableControls,%ActiveTemplateParentInstance)
#DECLARE(%CheckChildInsertActionConditionD)
#DECLARE(%CheckChildInsertActionConditionDItems)
#SET(%CheckChildInsertActionConditionDItems,0)
#IF(NOT %MCRTPrimaryAllowInsert)
    #SET(%CheckChildInsertActionConditionDItems,%CheckChildInsertActionConditionDItems+1)
    #SET(%CheckChildInsertActionConditionD,'0,' & %GetInstanceEquate(1))
#ENDIF
#FOR(%Secondary),WHERE(%SecondaryType = 'MANY:1' OR %SecondaryType = '')
   #FIND(%mFileTreeFile,%Secondary)
   #IF(%mFileTreeFile AND %mFileTreeChildRoot)
      #IF(NOT %MCRTSecondaryAllowInsert)
         #SET(%CheckChildInsertActionConditionDItems,%CheckChildInsertActionConditionDItems+1)
         #IF(%CheckChildInsertActionConditionD)
           #SET(%CheckChildInsertActionConditionD,%CheckChildInsertActionConditionD & ',' & %GetInstanceEquate(INSTANCE(%mFileTreeList)))
         #ELSE
           #SET(%CheckChildInsertActionConditionD,%GetInstanceEquate(INSTANCE(%mFileTreeList)))
         #ENDIF
      #ENDIF
   #ENDIF
#ENDFOR
#IF(%mInsertControl)
   #IF(%CheckChildInsertActionConditionD)
       #IF(%CheckChildInsertActionConditionDItems>1)
          #SET(%CheckChildInsertActionConditionD,'INLIST(' & %mTreeQueue & ':m_Instance,' & %CheckChildInsertActionConditionD & ')')
       #ELSE
          #SET(%CheckChildInsertActionConditionD,%mTreeQueue & ':m_Instance = ' & %CheckChildInsertActionConditionD )
       #ENDIF
IF %CheckChildInsertActionConditionD
   %mInsertControl{PROP:Disable} = TRUE
END
   #ENDIF
#ENDIF
#IF(%mChangeControl)
IF ~%mChangeControl{PROP:Disable}
   %mChangeControl{PROP:Disable} = TRUE
END
#ENDIF
#IF(%mDeleteControl)
IF ~%mDeleteControl{PROP:Disable}
   %mDeleteControl{PROP:Disable} = TRUE
END
#ENDIF
#IF(%mViewControl)
IF ~%mViewControl{PROP:Disable}
   %mViewControl{PROP:Disable} = TRUE
END
#ENDIF
#ENDAT
#!
#!
#!
#!
#AT(%MCRTEnableControls,%ActiveTemplateParentInstance)
#DECLARE(%CheckChildInsertActionCondition)
#DECLARE(%CheckChildChangeActionCondition)
#DECLARE(%CheckChildDeleteActionCondition)
#DECLARE(%CheckChildViewActionCondition)
#DECLARE(%CheckChildInsertActionConditionItems)
#DECLARE(%CheckChildChangeActionConditionItems)
#DECLARE(%CheckChildDeleteActionConditionItems)
#DECLARE(%CheckChildViewActionConditionItems)
#SET(%CheckChildInsertActionConditionItems,0)
#SET(%CheckChildChangeActionConditionItems,0)
#SET(%CheckChildDeleteActionConditionItems,0)
#SET(%CheckChildViewActionConditionItems,0)
#!
#IF(NOT %MCRTPrimaryAllowInsert)
    #IF(%MCRTHasChildWithAction(%mFileTreeFile,1,'INSERT')=0)
       #SET(%CheckChildInsertActionConditionItems,%CheckChildInsertActionConditionItems+1)
       #SET(%CheckChildInsertActionCondition,'0,' & %GetInstanceEquate(1))
    #ENDIF
#ENDIF
#IF(NOT %MCRTPrimaryAllowChange)
    #SET(%CheckChildChangeActionConditionItems,%CheckChildChangeActionConditionItems+1)
    #SET(%CheckChildChangeActionCondition,%GetInstanceEquate(1))
#ENDIF
#IF(NOT %MCRTPrimaryAllowDelete)
    #SET(%CheckChildDeleteActionConditionItems,%CheckChildDeleteActionConditionItems+1)
    #SET(%CheckChildDeleteActionCondition,%GetInstanceEquate(1))
#ENDIF
#IF(NOT %MCRTPrimaryAllowView)
    #SET(%CheckChildViewActionConditionItems,%CheckChildViewActionConditionItems+1)
    #SET(%CheckChildViewActionCondition,%GetInstanceEquate(1))
#ENDIF
#FOR(%Secondary),WHERE(%SecondaryType = 'MANY:1' OR %SecondaryType = '')
   #FIND(%mFileTreeFile,%Secondary)
   #IF(%mFileTreeFile)
      #IF(NOT %MCRTSecondaryAllowInsert)
         #IF(%mFileTreeChildRoot)
            #IF(%MCRTHasChildWithAction(%mFileTreeFile,%mFileTreeLevel+1,'INSERT')=0)
               #SET(%CheckChildInsertActionConditionItems,%CheckChildInsertActionConditionItems+1)
               #IF(%CheckChildInsertActionCondition)
                  #SET(%CheckChildInsertActionCondition,%CheckChildInsertActionCondition & ',' & %GetInstanceEquate(INSTANCE(%mFileTreeList)))
               #ELSE
                  #SET(%CheckChildInsertActionCondition,%GetInstanceEquate(INSTANCE(%mFileTreeList)))
               #ENDIF
            #ENDIF
         #ELSE
            #IF(%MCRTHasChildWithAction(%mFileTreeFile,%mFileTreeLevel,'INSERT')=0)
               #SET(%CheckChildInsertActionConditionItems,%CheckChildInsertActionConditionItems+1)
               #IF(%CheckChildInsertActionCondition)
                 #SET(%CheckChildInsertActionCondition,%CheckChildInsertActionCondition & ',' & %GetInstanceEquate(INSTANCE(%mFileTreeList)+CHOOSE(%mFileTreeChildRoot,1,0)))
               #ELSE
                 #SET(%CheckChildInsertActionCondition,%GetInstanceEquate(INSTANCE(%mFileTreeList)+CHOOSE(%mFileTreeChildRoot,1,0)))
               #ENDIF
            #ENDIF
         #ENDIF
      #ENDIF
      #IF(NOT %MCRTSecondaryAllowChange)
         #SET(%CheckChildChangeActionConditionItems,%CheckChildChangeActionConditionItems+1)
         #IF(%CheckChildChangeActionCondition)
           #SET(%CheckChildChangeActionCondition,%CheckChildChangeActionCondition & ',' & %GetInstanceEquate(INSTANCE(%mFileTreeList)+CHOOSE(%mFileTreeChildRoot,1,0)))
         #ELSE
           #SET(%CheckChildChangeActionCondition,%GetInstanceEquate(INSTANCE(%mFileTreeList)+CHOOSE(%mFileTreeChildRoot,1,0)))
         #ENDIF
      #ENDIF
      #IF(NOT %MCRTSecondaryAllowDelete)
         #SET(%CheckChildDeleteActionConditionItems,%CheckChildDeleteActionConditionItems+1)
         #IF(%CheckChildDeleteActionCondition)
           #SET(%CheckChildDeleteActionCondition,%CheckChildDeleteActionCondition & ',' & %GetInstanceEquate(INSTANCE(%mFileTreeList)+CHOOSE(%mFileTreeChildRoot,1,0)))
         #ELSE
           #SET(%CheckChildDeleteActionCondition,%GetInstanceEquate(INSTANCE(%mFileTreeList)+CHOOSE(%mFileTreeChildRoot,1,0)))
         #ENDIF
      #ENDIF
      #IF(NOT %MCRTSecondaryAllowView)
         #SET(%CheckChildViewActionConditionItems,%CheckChildViewActionConditionItems+1)
         #IF(%CheckChildViewActionCondition)
           #SET(%CheckChildViewActionCondition,%CheckChildViewActionCondition & ',' & %GetInstanceEquate(INSTANCE(%mFileTreeList)+CHOOSE(%mFileTreeChildRoot,1,0)))
         #ELSE
           #SET(%CheckChildViewActionCondition,%GetInstanceEquate(INSTANCE(%mFileTreeList)+CHOOSE(%mFileTreeChildRoot,1,0)))
         #ENDIF
      #ENDIF
   #ENDIF
#ENDFOR
#!
#IF(%mInsertControl)
   #IF(%CheckChildInsertActionCondition)
       #IF(%CheckChildInsertActionConditionItems>1)
           #SET(%CheckChildInsertActionCondition,'INLIST(' & %mTreeQueue & ':m_Instance,' & %CheckChildInsertActionCondition & ')')
       #ELSE
           #SET(%CheckChildInsertActionCondition,%mTreeQueue & ':m_Instance = ' & %CheckChildInsertActionCondition )
       #ENDIF
IF %CheckChildInsertActionCondition
   %mInsertControl{PROP:Disable} = TRUE
ELSE
   #ENDIF
IF %mInsertControl{PROP:Disable}
   %mInsertControl{PROP:Disable} = FALSE
END
   #IF(%CheckChildInsertActionCondition)
END
   #ENDIF
#ENDIF
#IF(%mChangeControl)
   #IF(%CheckChildChangeActionCondition)
       #IF(%CheckChildChangeActionConditionItems>1)
           #SET(%CheckChildChangeActionCondition,'INLIST(' & %mTreeQueue & ':m_Instance,' & %CheckChildChangeActionCondition & ')')
       #ELSE
           #SET(%CheckChildChangeActionCondition,%mTreeQueue & ':m_Instance = ' & %CheckChildChangeActionCondition )
       #ENDIF
IF %CheckChildChangeActionCondition
   %mChangeControl{PROP:Disable} = TRUE
ELSE
   #ENDIF
IF %mChangeControl{PROP:Disable}
   %mChangeControl{PROP:Disable} = FALSE
END
   #IF(%CheckChildChangeActionCondition)
END
   #ENDIF
#ENDIF
#IF(%mDeleteControl)
   #IF(%CheckChildDeleteActionCondition)
       #IF(%CheckChildDeleteActionConditionItems>1)
          #SET(%CheckChildDeleteActionCondition,'INLIST(' & %mTreeQueue & ':m_Instance,' & %CheckChildDeleteActionCondition & ')')
       #ELSE
          #SET(%CheckChildDeleteActionCondition,%mTreeQueue & ':m_Instance = ' & %CheckChildDeleteActionCondition )
       #ENDIF
IF %CheckChildDeleteActionCondition
   %mDeleteControl{PROP:Disable} = TRUE
ELSE
   #ENDIF
IF %mDeleteControl{PROP:Disable}
   %mDeleteControl{PROP:Disable} = FALSE
END
   #IF(%CheckChildDeleteActionCondition)
END
   #ENDIF
#ENDIF
#IF(%mViewControl)
   #IF(%CheckChildViewActionCondition)
       #IF(%CheckChildViewActionConditionItems>1)
          #SET(%CheckChildViewActionCondition,'INLIST(' & %mTreeQueue & ':m_Instance,' & %CheckChildViewActionCondition & ')')
       #ELSE
          #SET(%CheckChildViewActionCondition,%mTreeQueue & ':m_Instance = ' & %CheckChildViewActionCondition )
       #ENDIF
IF %CheckChildViewActionCondition
   %mViewControl{PROP:Disable} = TRUE
ELSE
   #ENDIF
IF %mViewControl{PROP:Disable}
   %mViewControl{PROP:Disable} = FALSE
END
   #IF(%CheckChildViewActionCondition)
END
   #ENDIF
#ENDIF
#ENDAT
#!-----------------------------------------------------------------------------
#AT(%ControlEventHandling,%mInsertControl,'Accepted')
%mInstancePrefix:Action = InsertRecord
DO %mInstancePrefix:UpdateTree
#ENDAT
#!-----------------------------------------------------------------------------
#AT(%ControlEventHandling,%mChangeControl,'Accepted')
%mInstancePrefix:Action = ChangeRecord
DO %mInstancePrefix:UpdateTree
#ENDAT
#!-----------------------------------------------------------------------------
#AT(%ControlEventHandling,%mDeleteControl,'Accepted')
%mInstancePrefix:Action = DeleteRecord
DO %mInstancePrefix:UpdateTree
#ENDAT
#!-----------------------------------------------------------------------------
#AT(%ControlEventHandling,%mViewControl,'Accepted')
%mInstancePrefix:Action = ViewRecord
DO %mInstancePrefix:UpdateTree
#ENDAT
#!-----------------------------------------------------------------------------
#AT(%MCRTToolbarDispatch,%ActiveTemplateParentInstance),WHERE(%AcceptToolbarControl)
#IF(UPPER(%AppTemplateFamily)='CLARION')
OF TBarBrwInsert TO TBarBrwDelete
  %mTreeControl{PROPLIST:MouseDownRow} = CHOICE(%mTreeControl) !! Server routines assume this
  EXECUTE(ACCEPTED()-TBarBrwInsert+1)
#ELSE
OF Toolbar:Insert TO Toolbar:Delete
  SELF.Control{PROPLIST:MouseDownRow} = CHOICE(SELF.Control) !! Server routines assume this
  EXECUTE(ACCEPTED()-Toolbar:Insert+1)
#ENDIF
    BEGIN
     #IF(%mInsertControl)
        IF ~%mInsertControl{PROP:Disable}
          POST(EVENT:Accepted,%mInsertControl)
        END
     #ENDIF
    END
    BEGIN
     #IF(%mChangeControl)
        IF ~%mChangeControl{PROP:Disable}
          POST(EVENT:Accepted,%mChangeControl)
        END
     #ENDIF
    END
    BEGIN
     #IF(%mDeleteControl)
        IF ~%mDeleteControl{PROP:Disable}
          POST(EVENT:Accepted,%mDeleteControl)
        END
     #ENDIF
    END
  END
#ENDAT
#!-----------------------------------------------------------------------------
#AT(%MCRTAssignToolbarButtons,%ActiveTemplateParentInstance),WHERE(%AcceptToolBarControl)
#IF(UPPER(%AppTemplateFamily)='CLARION')
  #IF (%mDeleteControl)
IF %mDeleteControl{PROP:Disable} THEN
  #ENDIF
  DISABLE(TBarBrwDelete)
  #IF (%mDeleteControl)
ELSE
  ENABLE(TBarBrwDelete)
END
  #ENDIF
  #IF (%mInsertControl)
IF %mInsertControl{PROP:Disable} THEN
  #ENDIF
  DISABLE(TBarBrwInsert)
  #IF (%mInsertControl)
ELSE
  ENABLE(TBarBrwInsert)
END
  #ENDIF
  #IF (%mChangeControl)
IF %mChangeControl{PROP:Disable} THEN
  #ENDIF
  DISABLE(TBarBrwChange)
  #IF (%mChangeControl)
ELSE
  ENABLE(TBarBrwChange)
END
  #ENDIF
#ELSE
  #IF (%mDeleteControl)
%mInstancePrefix:Toolbar.DeleteButton = %mDeleteControl
  #ENDIF
  #IF (%mInsertControl)
%mInstancePrefix:Toolbar.InsertButton = %mInsertControl
  #ENDIF
  #IF (%mChangeControl)
%mInstancePrefix:Toolbar.ChangeButton = %mChangeControl
  #ENDIF
#ENDIF
#ENDAT
#!-----------------------------------------------------------------------------
#AT(%ProcedureRoutines)
  #COMMENT(60)
!------------------------------------------------------------------------------
  #SET(%mTempVariable,%mInstancePrefix & ':UpdateTree')
%[35]mTempVariable ROUTINE
!|
!| The given routine is an user action redirector.
!| Here the global variable GlobalRequest accept value of %mInstancePrefix:Action
!| and calls action routine according of GlobalRequest value.
!|
  #IF ( %AcceptToolbarControl )
     #DECLARE(%lVCRPrefix)
     #IF(UPPER(%AppTemplateFamily)='CLARION')
         #SET(%lVCRPrefix,'VCR')
     #ELSE
         #SET(%lVCRPrefix,'VCR:')
     #ENDIF
  LOOP
    VCRRequest = %(%lVCRPrefix&'None')
    %mTreeControl{PROPLIST:MouseDownRow} = CHOICE(%mTreeControl)
  #ELSE
    %mTreeControl{PROPLIST:MouseDownRow} = %mInstancePrefix:CurrentChoice
  #ENDIF
    GlobalRequest = %mInstancePrefix:Action
    CASE GlobalRequest
  #IF(%mInsertControl)
    OF InsertRecord
      DO %mInstancePrefix:InsertDispatcher
  #ENDIF
  #IF(%mChangeControl)
    OF ChangeRecord
      DO %mInstancePrefix:ChangeDispatcher
  #ENDIF
  #IF(%mDeleteControl)
    OF DeleteRecord
      DO %mInstancePrefix:DeleteDispatcher
  #ENDIF
  #IF(%mViewControl)
    OF ViewRecord
      DO %mInstancePrefix:ViewDispatcher
  #ENDIF
    END
    SELECT(%mTreeControl)
  #IF ( %AcceptToolbarControl )
    CASE VCRRequest
      OF %(%lVCRPrefix&'Forward')
        IF %mInstancePrefix:Action = InsertRecord
           DO %mInstancePrefix:PreviousParent
        ELSE
           DO %mInstancePrefix:NextRecord
        END
      OF %(%lVCRPrefix&'Backward')
        DO %mInstancePrefix:PreviousRecord
      OF %(%lVCRPrefix&'PageForward')
        DO %mInstancePrefix:NextLevel
      OF %(%lVCRPrefix&'PageBackward')
        DO %mInstancePrefix:PreviousLevel
      OF %(%lVCRPrefix&'First')
        DO %mInstancePrefix:PreviousParent
      OF %(%lVCRPrefix&'Last')
        DO %mInstancePrefix:NextParent
      OF %(%lVCRPrefix&'Insert')
        DO %mInstancePrefix:PreviousParent
        %mInstancePrefix:Action = InsertRecord
      OF %(%lVCRPrefix&'None')
        %mInstancePrefix:LastInsertInstance = 0
        BREAK
    END
  END
  #ENDIF
#IF(%mInsertControl)
!------------------------------------------------------------------------------
  #SET(%mTempVariable,%mInstancePrefix & ':InsertDispatcher')
%[35]mTempVariable ROUTINE
!|
!| The given routine is an "insert" action dispatcher.
!| When user clicks "Insert" button or key "Insert" the variable %mInstancePrefix:Action
!| accept value InsertRecord and %mInstancePrefix:UpdateTree is called
!| where according of %mInstancePrefix:Action value neccessary action dispatcher is called.
!|
  #INSERT(%GetCurrentChoice)
  IF %mInsertControl{PROP:Disable} THEN EXIT.
  #SUSPEND
  #?IF ABS(%mTreeQueue.m_Level)
    #?REGET(%mTreeQueue.m_Key,%mTreeQueue.m_Position)
  #?END
  #?CASE(ABS(%mTreeQueue.m_Level))
#!-----------------------------------------------------------------------------
#! Here root level processes
#!-----------------------------------------------------------------------------
    #IF(%MCRTPrimaryUpdateProcedure AND %MCRTPrimaryAllowInsert AND NOT %DisablePrimaryLevel)
      #SUSPEND
  #?OF 0
      #FIX(%File,%Primary)
      #IF(UPPER(%AppTemplateFamily)='CLARION')
    #?GET(%File,0)
      #ENDIF
      #FOR(%Field),WHERE(%FieldType = 'MEMO')
    #?CLEAR(%Field)
      #ENDFOR
    #?CLEAR(%FilePrefix:Record)
      #IF(UPPER(%AppTemplateFamily)='ABC')
    #?Access:%File.PrimeRecord()
      #ENDIF
      #IF(%MCRTPrimaryUpdateProcedure)
    #EMBED(%MCRTBeforeInsertLevel,'Before Insert Level'),%ActiveTemplateInstance,%File,MAP(%ActiveTemplateInstance,%ActiveTemplateInstanceDescription)
    %MCRTPrimaryUpdateProcedure%MCRTPrimaryUpdateProcedureParameters
    #EMBED(%MCRTAfterInsertLevel,'After Insert Level'),%ActiveTemplateInstance,%File,MAP(%ActiveTemplateInstance,%ActiveTemplateInstanceDescription)
      #ENDIF
    #?IF GlobalResponse = RequestCompleted
      #?%mInstancePrefix:NewItemLevel    = 1
      #IF(%PrimaryKey)
      #?%mInstancePrefix:NewItemPosition = POSITION(%PrimaryKey)
      #ELSE
      #?%mInstancePrefix:NewItemPosition = POSITION(%Primary)
      #ENDIF
      #?DO %mInstancePrefix:ReloadTree
    #?END
      #RESUME
    #ENDIF
#!-----------------------------------------------------------------------------
#! All sublevels process here
#!-----------------------------------------------------------------------------
    #FOR(%mLevelList)
      #SUSPEND
  #?OF %mLevelList
        #FOR(%mFileTreeList),WHERE(%mFileTreeLevel = %mLevelList)
          #SET(%mTempInstance,INSTANCE(%mFileTreeList))
          #SUSPEND
    #?IF %mTreeQueue.m_Instance = %(%GetInstanceEquate(%mTempInstance))
#!-----------------------------------------------------------------------------
#! Make POPUP string if it is possible and count child levels for specified
#! level. Save parent file to %mTempVariable.
#!-----------------------------------------------------------------------------
            #SET(%mTempVariable,%mFileTreeFile)
            #SET(%mTempChildren,0)
            #SET(%mPopupString,%False)
            #IF(%DisablePrimaryLevel AND %mFileTreeLevel=1 AND NOT %mFileTreeChildRoot)
                #FIX(%Secondary,%mFileTreeFile)
                #IF(%MCRTSecondaryAllowInsert)
                    #IF(%MCRTChildUpdateProcedure)
                       #SET(%mPopupString,%mFileTreeFile)
                       #SET(%mTempChildren,%mTempChildren + 1)
                    #ENDIF
                #ENDIF
            #ENDIF
            #FOR(%mFileTreeList),WHERE(%mFileTreeParent = %mTempVariable AND (%mFileTreeLevel = %mLevelList + 1))
              #FIX(%Secondary,%mFileTreeFile)
              #IF(%MCRTSecondaryAllowInsert)
                 #IF(%mPopupString)
                   #IF(%MCRTChildUpdateProcedure)
                     #SET(%mPopupString,%mPopupString & '|-|' & %mFileTreeFile)
                   #ELSE
                     #SET(%mPopupString,%mPopupString & '|-|~' & %mFileTreeFile)
                   #ENDIF
                 #ELSE
                   #IF(%MCRTChildUpdateProcedure)
                     #SET(%mPopupString,%mFileTreeFile)
                   #ELSE
                     #SET(%mPopupString,'~' & %mFileTreeFile)
                   #ENDIF
                 #ENDIF
                 #SET(%mTempChildren,%mTempChildren + 1)
              #ENDIF
            #ENDFOR
#!-----------------------------------------------------------------------------
#! If quantity of children levels more than 1 EXECUTE POPUP
#!-----------------------------------------------------------------------------
            #IF(%mTempChildren > 1)
              #IF(%MCRTUseAlternativeUpdate AND %MCRTActivationKey)
      #?IF %mInstancePrefix:AltInsert
         #INSERT(%SingleFileInsertAction)
         %mInstancePrefix:AltInsert = FALSE
      #?ELSE
         #INSERT(%MultipleFileInsertAction)
      #?END
              #ELSE
      #INSERT(%MultipleFileInsertAction)
              #ENDIF
            #ELSIF(%mTempChildren = 1)
#!-----------------------------------------------------------------------------
#! If there is one child
#!-----------------------------------------------------------------------------
              #IF(%DisablePrimaryLevel AND %mFileTreeLevel=1 AND NOT %mFileTreeChildRoot)
                #FIX(%Secondary,%mFileTreeFile)
                #IF(%MCRTSecondaryAllowInsert)
                    #IF(%MCRTChildUpdateProcedure)
                        #INSERT(%SingleFileInsertAction)
                    #ENDIF
                #ENDIF
              #ENDIF
              #FOR(%mFileTreeList),WHERE(%mFileTreeParent = %mTempVariable AND (%mFileTreeLevel = %mLevelList + 1))
                #FIX(%File,%mFileTreeFile)
                #FIX(%Secondary,%mFileTreeFile)
                #IF(%MCRTSecondaryAllowInsert)
                #SUSPEND
                  #IF(UPPER(%AppTemplateFamily)='CLARION')
      #?GET(%File,0)
                  #ENDIF
                  #FOR(%Field),WHERE(%FieldType = 'MEMO')
      #?CLEAR(%Field)
                  #ENDFOR
      #?CLEAR(%FilePrefix:Record)
                  #FIX(%Relation,%mFileTreeParent)
                  #FOR(%FileKeyField),WHERE(%FileKeyField AND %FileKeyFieldLink)
      #?%FileKeyField = %FileKeyFieldLink
                  #ENDFOR
                  #IF(UPPER(%AppTemplateFamily)='ABC')
      #?Access:%File.PrimeRecord(1)
                  #ENDIF
                  #SET(%mMCRTSecondaryAllowMOChildAutoinc,%MCRTSecondaryAllowMOChildAutoinc)
                  #FOR(%mFileTreeOneToManyChild)
                     #FIX(%File,%mFileTreeOneToManyChild)
                     #IF(UPPER(%AppTemplateFamily)='CLARION')
      #?GET(%File,0)
                     #ENDIF
                     #FOR(%Field),WHERE(%FieldType = 'MEMO')
      #?CLEAR(%Field)
                     #ENDFOR
      #?CLEAR(%FilePrefix:Record)
                     #IF(%mMCRTSecondaryAllowMOChildAutoinc)
                       #IF(UPPER(%AppTemplateFamily)='ABC')
      #?Access:%File.PrimeRecord(1)
                       #ENDIF
                     #ENDIF
                  #ENDFOR
                  #FIX(%Secondary,%mFileTreeFile)
                  #IF(%MCRTChildUpdateProcedure)
     #EMBED(%MCRTBeforeInsertLevel,'Before Insert Level'),%ActiveTemplateInstance,%mFileTreeFile,MAP(%ActiveTemplateInstance,%ActiveTemplateInstanceDescription)
     %MCRTChildUpdateProcedure%MCRTChildUpdateProcedureParameters
     #EMBED(%MCRTAfterInsertLevel,'After Insert Level'),%ActiveTemplateInstance,%mFileTreeFile,MAP(%ActiveTemplateInstance,%ActiveTemplateInstanceDescription)
                  #ENDIF
      #?IF GlobalResponse = RequestCompleted
        #?%mInstancePrefix:NewItemLevel    = %(%mInstancePrefix & ':F_' & UPPER(%mFileTreeFile))
        #!#?%mInstancePrefix:NewItemPosition = POSITION(%mFileTreeKey)
        #?%mInstancePrefix:NewItemPosition = POSITION(%mTreeQueue.m_Key)
        #?DO %mInstancePrefix:ReloadTree
      #?END
      #?EXIT
                #RESUME
                #ENDIF
              #ENDFOR
            #ELSE
#!-----------------------------------------------------------------------------
#! If there are no children (for ChildRoots) or there is one child
#!-----------------------------------------------------------------------------
      #INSERT(%SingleFileInsertAction)
            #ENDIF
      #?EXIT
    #?END
          #RESUME
        #ENDFOR
      #RESUME
    #ENDFOR
  #?END
    #IF(%MCRTUseAlternativeUpdate AND %MCRTActivationKey)
  IF %mInstancePrefix:AltInsert
    %mInstancePrefix:AltInsert = FALSE
  END
    #ENDIF
  #RESUME
#ENDIF
#IF(%mChangeControl)
!------------------------------------------------------------------------------
  #SET(%mTempVariable,%mInstancePrefix & ':ChangeDispatcher')
%[35]mTempVariable ROUTINE
!|
!| The given routine is an "change" action dispatcher.
!| When user clicks "Change" button or key "Enter" the variable %mInstancePrefix:Action
!| accept value ChangeRecord and %mInstancePrefix:UpdateTree is called
!| where according of %mInstancePrefix:Action value neccessary action dispatcher is called.
!|
  #INSERT(%GetCurrentChoice)
  IF %mChangeControl{PROP:Disable} THEN EXIT.
  #SUSPEND
  #?IF %mTreeQueue:m_Level = 0
    #?EXIT
  #?END
  #?REGET(%mTreeQueue.m_Key,%mTreeQueue.m_Position)
  #?CASE(ABS(%mTreeQueue.m_Level))
    #FOR(%mLevelList)
      #SUSPEND
  #?OF %mLevelList
#!-----------------------------------------------------------------------------
#! Allow change code generating only for a file levels
#!-----------------------------------------------------------------------------
        #FOR(%mFileTreeList),WHERE(%mFileTreeLevel = %mLevelList AND ~%mFileTreeChildRoot)
          #SET(%mTempInstance,INSTANCE(%mFileTreeList))
          #!IF(%mTempInstance = ITEMS(%mFileTreeList))
          #IF(%mTempInstance > ITEMS(%mFileTreeList))
            #BREAK
          #ENDIF
          #SUSPEND
    #?IF %mTreeQueue.m_Instance = %mTempInstance ! %(%GetInstanceEquate(%mTempInstance))
            #SELECT(%mFileTreeList,%mTempInstance)
            #IF(%mTempInstance = 1 AND NOT %DisablePrimaryLevel)
              #IF(%MCRTPrimaryUpdateProcedure)
      #EMBED(%MCRTBeforeChangeLevel,'Before Change Level'),%ActiveTemplateInstance,%mFileTreeFile,MAP(%ActiveTemplateInstance,%ActiveTemplateInstanceDescription)
      %MCRTPrimaryUpdateProcedure%MCRTPrimaryUpdateProcedureParameters
      #EMBED(%MCRTAfterChangeLevel,'After Change Level'),%ActiveTemplateInstance,%mFileTreeFile,MAP(%ActiveTemplateInstance,%ActiveTemplateInstanceDescription)
              #ENDIF
            #ELSE
              #FIX(%Secondary,%mFileTreeFile)
              #IF(%MCRTChildUpdateProcedure)
      #EMBED(%MCRTBeforeChangeLevel,'Before Change Level'),%ActiveTemplateInstance,%mFileTreeFile,MAP(%ActiveTemplateInstance,%ActiveTemplateInstanceDescription)
      %MCRTChildUpdateProcedure%MCRTChildUpdateProcedureParameters
      #EMBED(%MCRTAfterChangeLevel,'After Change Level'),%ActiveTemplateInstance,%mFileTreeFile,MAP(%ActiveTemplateInstance,%ActiveTemplateInstanceDescription)
              #ENDIF
            #ENDIF
      #?IF GlobalResponse = RequestCompleted
        #?%mInstancePrefix:NewItemLevel    = %(%mInstancePrefix & ':F_' & UPPER(%mFileTreeFile))
        #!#?%mInstancePrefix:NewItemPosition = POSITION(%mFileTreeKey)
        #?%mInstancePrefix:NewItemPosition = POSITION(%mTreeQueue.m_Key)
        #?DO %mInstancePrefix:ReloadTree
      #?END
      #?EXIT
    #?END
          #RESUME
        #ENDFOR
      #RESUME
    #ENDFOR
  #?END
  #RESUME
#ENDIF
!------------------------------------------------------------------------------
#IF(%mDeleteControl)
  #SET(%mTempVariable,%mInstancePrefix & ':DeleteDispatcher')
%[35]mTempVariable ROUTINE
!|
!| The given routine is an "delete" action dispatcher.
!| When user clicks "Delete" button or key "Delete" the variable %mInstancePrefix:Action
!| accept value DeleteRecord and %mInstancePrefix:UpdateTree is called
!| where according of %mInstancePrefix:Action value neccessary action dispatcher is called.
!|
  #INSERT(%GetCurrentChoice)
  IF %mDeleteControl{PROP:Disable} THEN EXIT.
  #SUSPEND
  #?IF %mTreeQueue:m_Level = 0
    #?EXIT
  #?END
  #?REGET(%mTreeQueue.m_Key,%mTreeQueue.m_Position)
  #?CASE(ABS(%mTreeQueue.m_Level))
    #FOR(%mLevelList)
      #SUSPEND
  #?OF %mLevelList
#!-----------------------------------------------------------------------------
#! Allow delete code generating only for a file levels
#!-----------------------------------------------------------------------------
        #FOR(%mFileTreeList),WHERE(%mFileTreeLevel = %mLevelList AND ~%mFileTreeChildRoot)
          #SET(%mTempInstance,INSTANCE(%mFileTreeList))
          #!IF(%mTempInstance = ITEMS(%mFileTreeList))
          #IF(%mTempInstance > ITEMS(%mFileTreeList))
            #BREAK
          #ENDIF
          #SUSPEND
    #?IF %mTreeQueue.m_Instance = %(%GetInstanceEquate(%mTempInstance))
            #SELECT(%mFileTreeList,%mTempInstance)
            #IF(%mTempInstance = 1 AND NOT %DisablePrimaryLevel)
              #IF(%MCRTPrimaryUpdateProcedure)
      #EMBED(%MCRTBeforeDeleteLevel,'Before Delete Level'),%ActiveTemplateInstance,%mFileTreeFile,MAP(%ActiveTemplateInstance,%ActiveTemplateInstanceDescription)
      %MCRTPrimaryUpdateProcedure%MCRTPrimaryUpdateProcedureParameters
      #EMBED(%MCRTAfterDeleteLevel,'After Delete Level'),%ActiveTemplateInstance,%mFileTreeFile,MAP(%ActiveTemplateInstance,%ActiveTemplateInstanceDescription)
              #ENDIF
            #ELSE
              #FIX(%Secondary,%mFileTreeFile)
              #IF(%MCRTChildUpdateProcedure)
      #EMBED(%MCRTBeforeDeleteLevel,'Before Delete Level'),%ActiveTemplateInstance,%mFileTreeFile,MAP(%ActiveTemplateInstance,%ActiveTemplateInstanceDescription)
      %MCRTChildUpdateProcedure%MCRTChildUpdateProcedureParameters
      #EMBED(%MCRTAfterDeleteLevel,'After Delete Level'),%ActiveTemplateInstance,%mFileTreeFile,MAP(%ActiveTemplateInstance,%ActiveTemplateInstanceDescription)
              #ENDIF
            #ENDIF
      #?IF GlobalResponse = RequestCompleted
        #?%mInstancePrefix:NewItemLevel    = %(%mInstancePrefix & ':F_' & UPPER(%mFileTreeFile))
        #!#?%mInstancePrefix:NewItemPosition = POSITION(%mFileTreeKey)
        #?%mInstancePrefix:NewItemPosition = POSITION(%mTreeQueue.m_Key)
        #?DO %mInstancePrefix:ReloadTree
      #?END
      #?EXIT
    #?END
          #RESUME
        #ENDFOR
      #RESUME
    #ENDFOR
  #?END
  #RESUME
#ENDIF
!------------------------------------------------------------------------------
#IF(%mViewControl)
  #SET(%mTempVariable,%mInstancePrefix & ':ViewDispatcher')
%[35]mTempVariable ROUTINE
!|
!| The given routine is a "View" action dispatcher.
!| When user clicks "View" button the variable %mInstancePrefix:Action
!| accept value ViewRecord and %mInstancePrefix:UpdateTree is called
!| where according of %mInstancePrefix:Action value neccessary action dispatcher is called.
!|
  #INSERT(%GetCurrentChoice)
  IF %mViewControl{PROP:Disable} THEN EXIT.
  #SUSPEND
  #?IF %mTreeQueue:m_Level = 0
    #?EXIT
  #?END
  #?REGET(%mTreeQueue.m_Key,%mTreeQueue.m_Position)
  #?CASE(ABS(%mTreeQueue.m_Level))
    #FOR(%mLevelList)
      #SUSPEND
  #?OF %mLevelList
#!-----------------------------------------------------------------------------
#! Allow View code generating only for a file levels
#!-----------------------------------------------------------------------------
        #FOR(%mFileTreeList),WHERE(%mFileTreeLevel = %mLevelList AND ~%mFileTreeChildRoot)
          #SET(%mTempInstance,INSTANCE(%mFileTreeList))
          #!IF(%mTempInstance = ITEMS(%mFileTreeList))
          #IF(%mTempInstance > ITEMS(%mFileTreeList))
            #BREAK
          #ENDIF
          #SUSPEND
    #?IF %mTreeQueue.m_Instance = %mTempInstance ! %(%GetInstanceEquate(%mTempInstance))
            #SELECT(%mFileTreeList,%mTempInstance)
            #IF(%mTempInstance = 1 AND NOT %DisablePrimaryLevel)
              #IF(%MCRTPrimaryUpdateProcedure)
      #EMBED(%MCRTBeforeViewLevel,'Before View Level'),%ActiveTemplateInstance,%mFileTreeFile,MAP(%ActiveTemplateInstance,%ActiveTemplateInstanceDescription)
      %MCRTPrimaryUpdateProcedure%MCRTPrimaryUpdateProcedureParameters
      #EMBED(%MCRTAfterViewLevel,'After View Level'),%ActiveTemplateInstance,%mFileTreeFile,MAP(%ActiveTemplateInstance,%ActiveTemplateInstanceDescription)
              #ENDIF
            #ELSE
              #FIX(%Secondary,%mFileTreeFile)
              #IF(%MCRTChildUpdateProcedure)
      #EMBED(%MCRTBeforeViewLevel,'Before View Level'),%ActiveTemplateInstance,%mFileTreeFile,MAP(%ActiveTemplateInstance,%ActiveTemplateInstanceDescription)
      %MCRTChildUpdateProcedure%MCRTChildUpdateProcedureParameters
      #EMBED(%MCRTAfterViewLevel,'After View Level'),%ActiveTemplateInstance,%mFileTreeFile,MAP(%ActiveTemplateInstance,%ActiveTemplateInstanceDescription)
              #ENDIF
            #ENDIF
#!      #?IF GlobalResponse = RequestCompleted
#!        #?%mInstancePrefix:NewItemLevel    = %(%mInstancePrefix & ':F_' & UPPER(%mFileTreeFile))
#!        #?%mInstancePrefix:NewItemPosition = POSITION(%mFileTreeKey)
#!        #?DO %mInstancePrefix:ReloadTree
#!      #?END
     #?EXIT
    #?END
          #RESUME
        #ENDFOR
      #RESUME
    #ENDFOR
  #?END
  #RESUME
#ENDIF
#ENDAT
#!=============================================================================
#! CONTROL TEMPLATE
#!=============================================================================
#CONTROL(MultiChildRelationTreeExpandContractButtons,'Expand/Contract buttons for a Multi Children Relation Tree'),DESCRIPTION('Expand/Contract buttons for a Relation Tree for ' & %Primary),HLP('~IDH_MultiChildRelationTreeExpandContractButtons'),REQ(MultiChildRelationTree)
  CONTROLS
       BUTTON('&Expand All'),AT(,,45,14),USE(?ExpandAll)
       BUTTON('Co&ntract All'),AT(50,0,45,14),USE(?ContractAll)
  END
#ATSTART
  #DECLARE(%ExpandControl)
  #DECLARE(%ContractControl)
  #FOR(%Control),WHERE(%ControlInstance = %ActiveTemplateInstance)
    #CASE(%ControlOriginal)
    #OF('?ExpandAll')
      #SET(%ExpandControl,%Control)
      #SET(%ValueConstruct,EXTRACT(%ControlStatement,'BUTTON',1))
      #SET(%ExpandPopupText,SUB(%ValueConstruct,2,LEN(%ValueConstruct)-2))
    #OF('?ContractAll')
      #SET(%ContractControl,%Control)
      #SET(%ValueConstruct,EXTRACT(%ControlStatement,'BUTTON',1))
      #SET(%ContractPopupText,SUB(%ValueConstruct,2,LEN(%ValueConstruct)-2))
    #ENDCASE
  #ENDFOR
#ENDAT
#AT(%ControlEventHandling,%ExpandControl,'Accepted')
#IF(NOT %DisablePrimaryLevel)
%mInstancePrefix:ExpandAll = True
%mTreeControl{PROPLIST:MouseDownRow} = CHOICE(%mTreeControl)
DO %mInstancePrefix:ExpandAll
%mInstancePrefix:ExpandAll = False
#ELSE
%mInstancePrefix:ExpandAll = True
%mInstancePrefix:ForceReload = True
DO %mInstancePrefix:ReloadTree
%mInstancePrefix:ExpandAll = False
%mInstancePrefix:ForceReload = False
#ENDIF
#ENDAT
#AT(%ControlEventHandling,%ContractControl,'Accepted')
#IF(NOT %DisablePrimaryLevel)
%mTreeControl{PROPLIST:MouseDownRow} = CHOICE(%mTreeControl)
DO %mInstancePrefix:ContractAll
#ELSE
%mInstancePrefix:ExpandAll = False
%mInstancePrefix:ForceReload = True
DO %mInstancePrefix:ReloadTree
%mInstancePrefix:ForceReload = False
#ENDIF
#ENDAT
#!=============================================================================
#! CONTROL TEMPLATE
#!=============================================================================
#CONTROL(MultiChildRelationTreeSelectButton,'Multi Children Relation Tree Select Button'),DESCRIPTION('Select a Record from Tree'),WINDOW,HLP('~IDH_MultiChildRelationTreeSelectButton'),REQ(MultiChildRelationTree)
  CONTROLS
    BUTTON('&Select'),AT(,,48,14),USE(?Select),#REQ
  END
#!-----------------------------------------------------------------------------
#ATSTART
  #DECLARE(%mSelectControl)
  #FOR(%Control),WHERE(%ControlInstance = %ActiveTemplateInstance)
    #SET(%mSelectControl,%Control)
  #ENDFOR
#ENDAT
#!-----------------------------------------------------------------------------
#AT(%ControlEventHandling,%mSelectControl,'Accepted')
%mInstancePrefix:CurrentChoice = %mTreeControl{PROPLIST:MouseDownRow}
GET(%mTreeQueue,%mInstancePrefix:CurrentChoice)
REGET(%mTreeQueue.m_Key,%mTreeQueue.m_Position)
IF ABS(%mTreeQueue.m_Level)
#IF(UPPER(%AppTemplateFamily)='CLARION')
   LocalResponse = RequestCompleted
#ELSE
   %WindowManagerObject.Response = RequestCompleted
#ENDIF
   POST(EVENT:CloseWindow)
END
#ENDAT
#AT(%MCRTDisableControls,%ActiveTemplateParentInstance)
#IF(%mSelectControl)
IF ~%mSelectControl{PROP:Disable}
   %mSelectControl{PROP:Disable} = TRUE
END
#ENDIF
#ENDAT
#AT(%MCRTEnableControls,%ActiveTemplateParentInstance)
#IF(%mSelectControl)
IF %mSelectControl{PROP:Disable}
   %mSelectControl{PROP:Disable} = FALSE
END
#ENDIF
#ENDAT
#!=============================================================================
#! CONTROL TEMPLATE
#!=============================================================================
#CONTROL(MultiChildRelationTreeButton,'Multi Children Relation Tree Button'),DESCRIPTION('Button ('&%GetControlName(%ActiveTemplateInstance)&')'),WINDOW,HLP('~IDH_MultiChildRelationTreeButton'),REQ(MultiChildRelationTree),MULTI
  CONTROLS
    BUTTON('Button'),AT(,,48,14),USE(?Button)
  END
#!=============================================================================
#BUTTON('Tree Button'),AT(10,,180)
#SHEET,ADJUST
  #TAB('General')
    #PROMPT('Disable Condition:',EDIT),%MCRTDisableButtonConditionc,WHENACCEPTED(%SVExpresionEditor(%MCRTDisableButtonCondition))
    #PROMPT('Use Button via &Popup',CHECK),%MCRTUseButtonViaPopup,DEFAULT(%True),AT(10)
    #ENABLE(%MCRTUseButtonViaPopup)
      #PROMPT('Add Separation',CHECK),%MCRTUseButtonViaPopupSeparation,DEFAULT(%True),AT(10)
      #PROMPT('Use &Alternative Text',CHECK),%MCRTUseAlternativeText,DEFAULT(%False),AT(10)
      #ENABLE(%MCRTUseAlternativeText)
        #PROMPT('Popup &Text',EXPR),%MCRTAlternativeText,DEFAULT('Button'),REQ
      #ENDENABLE
      #PROMPT('Popup priority:',@n3),%MCRTDisableButtonPriority,DEFAULT(200)
    #ENDENABLE
    #PROMPT('Refresh Tree After Action',CHECK),%MCRTRefreshTreeAfterAction,DEFAULT(%False),AT(10)
  #ENDTAB
  #TAB('Primary Level'),WHERE(%DisablePrimaryLevel)
    #DISPLAY
    #DISPLAY('Your MultiChildRelationTree control is not set')
    #DISPLAY('to use a common Root using the Primary table.')
  #ENDTAB
  #TAB('Primary Level'),WHERE(NOT %DisablePrimaryLevel),CLEAR
    #BOXED('Button Action'),SECTION
      #DISPLAY('File: '&%Primary)
      #BOXED('Item Options')
        #PROMPT('Enable Button',CHECK),%MCRTPrimaryAllowButton,DEFAULT(%False),AT(10)
        #ENABLE(%MCRTPrimaryAllowButton)
          #PROMPT('Disable Condition:',EDIT),%MCRTPrimaryDisableButtonCondition,WHENACCEPTED(%SVExpresionEditor(%MCRTPrimaryDisableButtonCondition))
        #ENDENABLE
        #ENABLE(%MCRTUseButtonViaPopup)
          #PROMPT('Allow Popup',CHECK),%MCRTPrimaryAllowButtonPopup,DEFAULT(%False),AT(10)
          #!ENABLE(%MCRTPrimaryAllowButtonPopup)
            #!PROMPT('Remove if button is disable',CHECK),%MCRTPrimaryAllowButtonPopupRemove,DEFAULT(%True),AT(10)
          #!ENDENABLE
        #ENDENABLE
      #ENDBOXED
      #BOXED('Root Options')
        #ENABLE(%EnableTreeRoot),CLEAR
          #PROMPT('Enable Button on Root',CHECK),%MCRTPrimaryAllowButtonRoot,DEFAULT(%False),AT(10)
          #ENABLE(%MCRTPrimaryAllowButtonRoot)
            #PROMPT('Disable Condition:',EDIT),%MCRTPrimaryDisableButtonConditionRoot,WHENACCEPTED(%SVExpresionEditor(%MCRTPrimaryDisableButtonConditionRoot))
          #ENDENABLE
          #ENABLE(%MCRTUseButtonViaPopup)
            #PROMPT('Allow Popup on Root',CHECK),%MCRTPrimaryAllowButtonPopupRoot,DEFAULT(%False),AT(10)
            #!ENABLE(%MCRTPrimaryAllowButtonPopupRoot)
              #!PROMPT('Remove if button is disable',CHECK),%MCRTPrimaryAllowButtonPopupRemoveRoot,DEFAULT(%True),AT(10)
            #!ENDENABLE
          #ENDENABLE
        #ENDENABLE
      #ENDBOXED
    #ENDBOXED
  #ENDTAB
  #TAB('Child Levels')
    #BOXED('Button Action')
      #BUTTON('Secondary Level'),FROM(%Secondary,'Item:'&CHOOSE(%MCRTSecondaryAllowButton,'E','D')&'-'&CHOOSE(%MCRTSecondaryAllowButtonPopup,'P','_')&' Root:'&CHOOSE(%MCRTSecondaryAllowButtonRoot,'E','D')&'-'&CHOOSE(%MCRTSecondaryAllowButtonPopupRoot,'P','_')&' for '&%Secondary),INLINE,WHERE(%SecondaryType = 'MANY:1' OR %SecondaryType = '')
        #BOXED,SECTION
          #DISPLAY('File: '&%Secondary)
          #BOXED('Item Options')
            #PROMPT('Enable Button',CHECK),%MCRTSecondaryAllowButton,DEFAULT(%False),AT(10)
            #ENABLE(%MCRTSecondaryAllowButton)
              #PROMPT('Disable Condition:',EDIT),%MCRTSecondaryDisableButtonCondition,WHENACCEPTED(%SVExpresionEditor(%MCRTSecondaryDisableButtonCondition))
            #ENDENABLE
            #ENABLE(%MCRTUseButtonViaPopup)
              #PROMPT('Allow Popup',CHECK),%MCRTSecondaryAllowButtonPopup,DEFAULT(%False),AT(10)
              #!ENABLE(%MCRTSecondaryAllowButtonPopup)
                #!PROMPT('Remove if button is disable',CHECK),%MCRTSecondaryAllowButtonPopupRemove,DEFAULT(%True),AT(10)
              #!ENDENABLE
            #ENDENABLE
          #ENDBOXED
          #BOXED('Root Options')
            #DISPLAY('This only will be used if the level has a Root')
            #!ENABLE(%GetHasChildRoot(%Secondary))
              #PROMPT('Enable Button on Root',CHECK),%MCRTSecondaryAllowButtonRoot,DEFAULT(%False),AT(10)
              #ENABLE(%MCRTSecondaryAllowButtonRoot)
                #PROMPT('Disable Condition:',EDIT),%MCRTSecondaryDisableButtonConditionRoot,WHENACCEPTED(%SVExpresionEditor(%MCRTSecondaryDisableButtonConditionRoot))
              #ENDENABLE
              #ENABLE(%MCRTUseButtonViaPopup)
                #PROMPT('Allow Popup on Root',CHECK),%MCRTSecondaryAllowButtonPopupRoot,DEFAULT(%False),AT(10)
                #!ENABLE(%MCRTSecondaryAllowButtonPopupRoot)
                  #!PROMPT('Remove if button is disable',CHECK),%MCRTSecondaryAllowButtonPopupRemoveRoot,DEFAULT(%True),AT(10)
                #!ENDENABLE
              #ENDENABLE
            #!ENDENABLE
          #ENDBOXED
        #ENDBOXED
      #ENDBUTTON
    #ENDBOXED
  #ENDTAB
#ENDSHEET
#ENDBUTTON
#!-----------------------------------------------------------------------------
#ATSTART
  #DECLARE(%mButtonControl)
  #DECLARE(%mButtonPopupText)
  #DECLARE(%mPopupString)
  #DECLARE(%mTempChildren,LONG)
  #DECLARE(%mButtonControlDisableCondition)
  #DECLARE(%mButtonControlDisableConditionPlus)
  #DECLARE(%mButtonControlShowPopupCondition)
  #DECLARE(%mButtonControlDisableConditionItems)
  #DECLARE(%mButtonControlShowPopupConditionItems)
  #DECLARE(%lAllowEmbedFile)
  #DECLARE(%lAllowEmbedRoot)
  #FOR(%Control),WHERE(%ControlInstance = %ActiveTemplateInstance)
    #!IF(%ControlOriginal='?Button')
      #SET(%mButtonControl,%Control)
      #IF(%MCRTUseAlternativeText)
        #SET(%mButtonPopupText,%MCRTAlternativeText)
      #ELSE
        #SET(%mButtonPopupText,EXTRACT(%ControlStatement,'BUTTON',1))
        #SET(%mButtonPopupText,SUB(%mButtonPopupText,2,LEN(%mButtonPopupText)-2))
      #ENDIF
    #!ENDIF
  #ENDFOR
#ENDAT
#!
#AT(%ControlEventHandling,%mButtonControl,'Accepted'),PRIORITY(4500)
  #INSERT(%GetCurrentChoice)
  #SUSPEND
  #?IF NOT %mButtonControl{PROP:Disable} THEN
    #?CASE(ABS(%mTreeQueue.m_Level))
      #IF(%EnableTreeRoot AND %MCRTPrimaryAllowButtonRoot)
    #?OF 0
      #EMBED(%MCRTButtonAcceptedLevel,'Accepted for Level'),%ActiveTemplateInstance,'TREEROOT',TREE('Control Events{{PRIORITY(4000)}|'&%mButtonControl&'|Accepted|For Level|Tree Root')
      #ENDIF
      #FOR(%mLevelList)
        #SUSPEND
    #?OF %mLevelList
        #FOR(%mFileTreeList),WHERE(%mFileTreeLevel = %mLevelList)
          #SET(%mTempInstance,INSTANCE(%mFileTreeList))
          #IF(%mFileTreeFile=%Primary)
            #SET(%lAllowEmbedFile,%MCRTPrimaryAllowButton)
            #SET(%lAllowEmbedRoot,%MCRTPrimaryAllowButtonRoot)
          #ELSE
            #FIX(%Secondary,%mFileTreeFile)
            #SET(%lAllowEmbedFile,%MCRTSecondaryAllowButton)
            #SET(%lAllowEmbedRoot,%MCRTSecondaryAllowButtonRoot)
          #ENDIF
          #IF(NOT(%lAllowEmbedFile OR %lAllowEmbedRoot))
            #CYCLE
          #ENDIF
          #SUSPEND
      #?IF %mTreeQueue.m_Instance = %mTempInstance ! %(%GetInstanceEquate(%mTempInstance))
          #IF(%mFileTreeChildRoot)
            #IF(%lAllowEmbedRoot)
        #EMBED(%MCRTButtonAcceptedLevel,'Accepted for Level'),%ActiveTemplateInstance,'ROOT',%mFileTreeFile,TREE('Control Events{{PRIORITY(4000)}|'&%mButtonControl&'|Accepted|For Level|Root - '&%mFileTreeFile)
            #ENDIF
          #ELSE
            #IF(%lAllowEmbedFile)
        #?REGET(%mTreeQueue.m_Key,%mTreeQueue.m_Position)
              #SELECT(%mFileTreeList,%mTempInstance)
              #IF(%mTempInstance <> 1)
                #FIX(%Secondary,%mFileTreeFile)
              #ENDIF
        #EMBED(%MCRTButtonAcceptedLevel,'Accepted for Level'),%ActiveTemplateInstance,%mFileTreeFile,TREE('Control Events{{PRIORITY(4000)}|'&%mButtonControl&'|Accepted|For Level|'&%mFileTreeFile)
              #IF(%MCRTRefreshTreeAfterAction)
        #?%mInstancePrefix:NewItemLevel    = %(%mInstancePrefix & ':F_' & UPPER(%mFileTreeFile))
        #?%mInstancePrefix:NewItemPosition = POSITION(%mTreeQueue.m_Key)
        #?DO %mInstancePrefix:ReloadTree
        #?DO %mInstancePrefix:OnNewSelection
              #ENDIF
            #ENDIF
          #ENDIF
      #?END
          #RESUME
        #ENDFOR
        #RESUME
      #ENDFOR
    #?END
  #?END
  #RESUME
#ENDAT
#!
#AT(%MCRTGeneratePopup,%ActiveTemplateParentInstance)
  #SET(%mButtonControlShowPopupCondition,'')
  #SET(%mButtonControlShowPopupConditionItems,0)
  #IF(%MCRTUseButtonViaPopup)
      #IF(%MCRTPrimaryAllowButtonPopupRoot)
         #SET(%mButtonControlShowPopupConditionItems,%mButtonControlShowPopupConditionItems+1)
         #SET(%mButtonControlShowPopupCondition,'0,')
      #ENDIF
      #IF(%MCRTPrimaryAllowButtonPopup)
         #SET(%mButtonControlShowPopupConditionItems,%mButtonControlShowPopupConditionItems+1)
         #SET(%mButtonControlShowPopupCondition,%mButtonControlShowPopupCondition & %GetInstanceEquate(1)& ',')
      #ENDIF
      #FOR(%Secondary),WHERE(%SecondaryType = 'MANY:1' OR %SecondaryType = '')
         #FIND(%mFileTreeFile,%Secondary)
         #IF(%mFileTreeFile)
            #IF(%mFileTreeChildRoot)
               #IF(%MCRTSecondaryAllowButtonPopupRoot)
                  #SET(%mButtonControlShowPopupConditionItems,%mButtonControlShowPopupConditionItems+1)
                  #SET(%mButtonControlShowPopupCondition,%mButtonControlShowPopupCondition & %GetInstanceEquate(INSTANCE(%mFileTreeList))& ',')
               #ENDIF
               #IF(%MCRTSecondaryAllowButtonPopup)
                  #SET(%mButtonControlShowPopupConditionItems,%mButtonControlShowPopupConditionItems+1)
                  #SET(%mButtonControlShowPopupCondition,%mButtonControlShowPopupCondition & %GetInstanceEquate(INSTANCE(%mFileTreeList)+1)& ',')
               #ENDIF
            #ELSE
               #IF(%MCRTSecondaryAllowButtonPopup)
                  #SET(%mButtonControlShowPopupConditionItems,%mButtonControlShowPopupConditionItems+1)
                  #SET(%mButtonControlShowPopupCondition,%mButtonControlShowPopupCondition & %GetInstanceEquate(INSTANCE(%mFileTreeList))& ',')
               #ENDIF
            #ENDIF
         #ENDIF
      #ENDFOR
    #IF(%mButtonControlShowPopupCondition)
      #IF(%mButtonControlShowPopupConditionItems>1)
          #SET(%mButtonControlShowPopupCondition,'INLIST(' & %mTreeQueue & ':m_Instance,' & SUB(%mButtonControlShowPopupCondition,1,LEN(%mButtonControlShowPopupCondition)-1) & ')')
      #ELSE
          #SET(%mButtonControlShowPopupCondition,%mTreeQueue & ':m_Instance = ' & SUB(%mButtonControlShowPopupCondition,1,LEN(%mButtonControlShowPopupCondition)-1) )
      #ENDIF
    #ENDIF
  #!
  #!
  #!
    #CALL(%AddItemToPopupMenu,'BUTTON'&%ActiveTemplateInstance,%MCRTDisableButtonPriority,%mButtonPopupText,'POST(EVENT:Accepted,'&%mButtonControl&')','NOT '&%mButtonControl&'{PROP:Disable}',%mButtonControlShowPopupCondition,%MCRTUseButtonViaPopupSeparation)
  #ENDIF
#ENDAT
#!
#!
#AT(%MCRTAfterDisableControls,%ActiveTemplateParentInstance)
  #SET(%mButtonControlDisableCondition,'')
  #SET(%mButtonControlDisableConditionItems,0)
  #SET(%mButtonControlDisableConditionPlus,'')
  #IF(NOT %MCRTPrimaryAllowButtonRoot)
     #SET(%mButtonControlDisableConditionItems,%mButtonControlDisableConditionItems+1)
     #SET(%mButtonControlDisableCondition,'0,')
  #ELSE
     #IF(%MCRTPrimaryDisableButtonConditionRoot)
        #SET(%mButtonControlDisableConditionPlus,'('&%mTreeQueue & ':m_Instance=0 AND '&%MCRTPrimaryDisableButtonConditionRoot&')')
     #ENDIF
  #ENDIF
  #IF(NOT %MCRTPrimaryAllowButton)
      #SET(%mButtonControlDisableConditionItems,%mButtonControlDisableConditionItems+1)
      #SET(%mButtonControlDisableCondition,%mButtonControlDisableCondition & %GetInstanceEquate(1)& ',')
  #ELSE
      #IF(%MCRTPrimaryDisableButtonCondition)
        #IF(%mButtonControlDisableConditionPlus)
            #SET(%mButtonControlDisableConditionPlus,%mButtonControlDisableConditionPlus&' OR ')
        #ENDIF
        #SET(%mButtonControlDisableConditionPlus,%mButtonControlDisableConditionPlus & '('&%mTreeQueue & ':m_Instance='&%GetInstanceEquate(1)&' AND '&%MCRTPrimaryDisableButtonCondition&')')
      #ENDIF
  #ENDIF
  #FOR(%Secondary),WHERE(%SecondaryType = 'MANY:1' OR %SecondaryType = '')
     #FIND(%mFileTreeFile,%Secondary)
     #IF(%mFileTreeFile)
        #IF(%mFileTreeChildRoot)
           #IF(NOT %MCRTSecondaryAllowButtonRoot)
               #SET(%mButtonControlDisableConditionItems,%mButtonControlDisableConditionItems+1)
               #SET(%mButtonControlDisableCondition,%mButtonControlDisableCondition & %GetInstanceEquate(INSTANCE(%mFileTreeList))&',')
           #ELSE
               #IF(%MCRTSecondaryDisableButtonConditionRoot)
                   #IF(%mButtonControlDisableConditionPlus)
                       #SET(%mButtonControlDisableConditionPlus,%mButtonControlDisableConditionPlus&' OR ')
                   #ENDIF
                   #SET(%mButtonControlDisableConditionPlus,%mButtonControlDisableConditionPlus & '('&%mTreeQueue & ':m_Instance='&%GetInstanceEquate(INSTANCE(%mFileTreeList))&' AND '&%MCRTSecondaryDisableButtonConditionRoot&')')
               #ENDIF
           #ENDIF
           #IF(NOT %MCRTSecondaryAllowButton)
               #SET(%mButtonControlDisableConditionItems,%mButtonControlDisableConditionItems+1)
               #SET(%mButtonControlDisableCondition,%mButtonControlDisableCondition & %GetInstanceEquate(INSTANCE(%mFileTreeList)+1)& ',')
           #ELSE
               #IF(%MCRTSecondaryDisableButtonCondition)
                 #IF(%mButtonControlDisableConditionPlus)
                     #SET(%mButtonControlDisableConditionPlus,%mButtonControlDisableConditionPlus&' OR ')
                 #ENDIF
                 #SET(%mButtonControlDisableConditionPlus,%mButtonControlDisableConditionPlus & '('&%mTreeQueue & ':m_Instance='&%GetInstanceEquate(INSTANCE(%mFileTreeList)+1)&' AND '&%MCRTSecondaryDisableButtonCondition&')')
               #ENDIF
           #ENDIF
        #ELSE
           #IF(NOT %MCRTSecondaryAllowButton)
               #SET(%mButtonControlDisableConditionItems,%mButtonControlDisableConditionItems+1)
               #SET(%mButtonControlDisableCondition,%mButtonControlDisableCondition & %GetInstanceEquate(INSTANCE(%mFileTreeList))& ',')
           #ELSE
               #IF(%MCRTSecondaryDisableButtonCondition)
                 #IF(%mButtonControlDisableConditionPlus)
                     #SET(%mButtonControlDisableConditionPlus,%mButtonControlDisableConditionPlus&' OR ')
                 #ENDIF
                 #SET(%mButtonControlDisableConditionPlus,%mButtonControlDisableConditionPlus & '('&%mTreeQueue & ':m_Instance='&%GetInstanceEquate(INSTANCE(%mFileTreeList))&' AND '&%MCRTSecondaryDisableButtonCondition&')')
               #ENDIF
           #ENDIF
        #ENDIF
     #ENDIF
  #ENDFOR
  #IF(%mButtonControlDisableCondition)
      #IF(%mButtonControlDisableConditionItems>1)
          #SET(%mButtonControlDisableCondition,'INLIST(' & %mTreeQueue & ':m_Instance,' & SUB(%mButtonControlDisableCondition,1,LEN(%mButtonControlDisableCondition)-1) & ')')
      #ELSE
          #SET(%mButtonControlDisableCondition,%mTreeQueue & ':m_Instance = ' & SUB(%mButtonControlDisableCondition,1,LEN(%mButtonControlDisableCondition)-1) )
      #ENDIF
      #IF(%mButtonControlDisableConditionPlus)
          #SET(%mButtonControlDisableCondition,%mButtonControlDisableCondition&' OR '&%mButtonControlDisableConditionPlus)
      #ENDIF
  #ELSE
      #SET(%mButtonControlDisableCondition,%mButtonControlDisableConditionPlus)
  #ENDIF
  #IF(%MCRTDisableButtonCondition)
      #IF(%mButtonControlDisableCondition)
         #SET(%mButtonControlDisableCondition,%MCRTDisableButtonCondition&' OR '&%mButtonControlDisableCondition)
      #ELSE
         #SET(%mButtonControlDisableCondition,%MCRTDisableButtonCondition)
      #ENDIF
  #ENDIF
#IF(%mButtonControlDisableCondition)
IF %mButtonControlDisableCondition
   %mButtonControl{PROP:Disable} = TRUE
END
#ENDIF
#IF(%mButtonControlDisableCondition)
IF NOT (%mButtonControlDisableCondition)
   %mButtonControl{PROP:Disable} = FALSE
END
#ENDIF
#ENDAT
#!=============================================================================
#! *** END OF TEMPLATE ***
#!=============================================================================
#GROUP(%GetControlName,%pInstance)
  #FOR(%Control),WHERE(%ControlInstance = %pInstance)
    #IF(%ControlOriginal='?Button')
      #RETURN(%Control)
    #ENDIF
  #ENDFOR
  #RETURN('UNKNOWN')
#GROUP(%GetHasChildRoot,%pSecondary),PRESERVE
#FIND(%mFileTreeFile,%Secondary)
#IF(%mFileTreeFile)
   #IF(%mFileTreeChildRoot)
     #RETURN(%True)
   #ENDIF
#ENDIF
#RETURN(%False)
#!---------------------------------------------------------------------------
#GROUP(%GetStyleNumber,%pStyleType,%pStyleNumber,%pStyleString),AUTO
#IF(%pStyleType='Local List')
   #DECLARE(%LocStyleSep)
   #SET(%LocStyleSep,INSTRING('-',%pStyleString,1,1)-1)
   #IF(%LocStyleSep>0)
      #RETURN(LEFT(%pStyleString,%LocStyleSep))
   #ELSE
      #RETURN(0)
   #ENDIF
#ELSE
   #RETURN(%pStyleNumber)
#ENDIF
#!---------------------------------------------------------------------------
#GROUP(%MCRTLoadKeyComponents,%pFile,%pKey,%pSecondary=0),PRESERVE
#FIX(%File,%pFile)
#IF(%File)
   #FIX(%Key,%pKey)
   #IF(%Key)
      #IF(%pSecondary)
         #FREE(%MCRTChildCustomJoinKeyComponents)
         #FOR(%KeyField)
            #ADD(%MCRTChildCustomJoinKeyComponents,ITEMS(%MCRTChildCustomJoinKeyComponents)+1)
            #SET(%MCRTChildCustomJoinKeyComponent,%KeyField)
         #ENDFOR
      #ENDIF
   #ENDIF
#ENDIF
#!---------------------------------------------------------------------------
#GROUP(%MCRTGetCustomJoinParentKey,%pCustomJoinParentFile),PRESERVE
#FIX(%Secondary,%pCustomJoinParentFile)
#IF(%Secondary)
   #FIX(%File,%SecondaryTo)
   #FIX(%Relation,%Secondary)
   #RETURN(%RelationKey)
#ELSE
   #RETURN(%PrimaryKey)
#ENDIF
#!---------------------------------------------------------------------------
#GROUP(%MCRTGenerateHigherKeyCode),AUTO
#CALL(%GenerateHigherKeyCodeComponent(CLARION),'',%mInstancePrefix&':Save:')
#!---------------------------------------------------------------------------
#GROUP(%MCRTHasChildWithAction,%pParentFile,%pParentLevel,%pAction),AUTO,PRESERVE
#DECLARE(%lTempChildren)
#SET(%lTempChildren,0)
#FOR(%mFileTreeList),WHERE(%mFileTreeParent = %pParentFile AND (%mFileTreeLevel = %pParentLevel + 1))
   #FIX(%Secondary,%mFileTreeFile)
   #IF(%MCRTChildUpdateProcedure)
      #CASE(UPPER(%pAction))
      #OF('INSERT')
        #IF(%MCRTPrimaryAllowInsert)
           #SET(%lTempChildren,%lTempChildren+1)
        #ENDIF
      #OF('CHANGE')
        #IF(%MCRTPrimaryAllowChange)
           #SET(%lTempChildren,%lTempChildren+1)
        #ENDIF
      #OF('DELETE')
        #IF(%MCRTPrimaryAllowDelete)
           #SET(%lTempChildren,%lTempChildren+1)
        #ENDIF
      #ENDCASE
   #ENDIF
#ENDFOR
#RETURN(%lTempChildren)
#!---------------------------------------------------------------------------
#GROUP(%LoadHigherKeysComponets,%pFile,%pKey,%pField,%pRangeLimitType,%pUseInConditional=0),AUTO
#CALL(%LoadHigherKeysComponets(CLARION),%pFile,%pKey,%pField,%pRangeLimitType,%pUseInConditional)
#!---------------------------------------------------------------------------
#GROUP(%GenerateFileRelationshipCode,%pMyFile,%pParentFile,%pAction),PRESERVE
#FIX(%File,%pMyFile)
#FIX(%Relation,%pParentFile)
#IF(%Relation)
   #FOR(%FileKeyField)
   #CASE(UPPER(%pAction))
   #OF('ASIGN')
%FileKeyField = %FileKeyFieldLink
   #OF('BREAK')
IF %FileKeyField <> %FileKeyFieldLink THEN BREAK.
   #ELSE
      #ERROR(%Procedure&'MultiChildTree - GenerateFileRelationshipCode - wrong action - File:'&%pMyFile&' - ParentFile:'&%pParentFile&' - Action:'&%pAction)
   #ENDCASE
   #ENDFOR
#ENDIF
#!---------------------------------------------------------------------------
#GROUP(%GenerateRangeFilter)
    #IF(%mFileTreeIsPrimary)
       #IF(%RangeField)
#INSERT(%GenerateHigherKeyCodeFilter,%mInstancePrefix&':Save:')
          #CASE(%RangeLimitType)
          #OF('Current Value')
#INSERT(%GenerateRangeLimitSaveValueCode,'BREAK')
          #OF('Single Value')
IF %RangeField <> %RangeLimit THEN BREAK.
          #OF('Range of Values')
IF %RangeField < %RangeLow THEN BREAK.
IF %RangeField > %RangeHigh THEN BREAK.
          #OF('File Relationship')
#INSERT(%GenerateFileRelationshipCode,%Primary,%RangeFile,'BREAK')
          #ENDCASE
       #ENDIF
    #ENDIF
#!---------------------------------------------------------------------------
#GROUP(%GenerateRangeAsign)
    #IF(%mFileTreeIsPrimary)
       #IF(%RangeField)
          #CALL(%LoadHigherKeysComponets,%Primary,%PrimaryKey,%RangeField,%RangeLimitType)
#INSERT(%MCRTGenerateHigherKeyCode)
#INSERT(%GenerateRangeLimitSaveValueCode,'ASIGN')
          #CASE(%RangeLimitType)
          #OF('Single Value')
%RangeField = %RangeLimit
          #OF('Range of Values')
%RangeField = %RangeLow
          #OF('File Relationship')
#INSERT(%GenerateFileRelationshipCode,%Primary,%RangeFile,'ASIGN')
          #ENDCASE
       #ENDIF
    #ENDIF
#!---------------------------------------------------------------------------
#GROUP(%GenerateHigherKeyCodeFilter,%pPrefix)
    #IF((%RangeLimitType='Single Value' OR %RangeLimitType='Range of Values') AND ITEMS(%HigherKeys))
      #FOR(%HigherKeys),WHERE(%HigherKeyValueType<>1)
        #IF(%HigherKeyValueType=2)
IF %pPrefix%HigherKeyField <> %HigherKeyValueVariable THEN BREAK.
        #ELSE
IF %pPrefix%HigherKeyField <> %HigherKeyValueFixed THEN BREAK.
        #ENDIF
      #ENDFOR
    #ENDIF
#!---------------------------------------------------------------------------
#GROUP(%GenerateRangeLimitSaveValueCode,%pAction),PRESERVE
  #IF(%RangeField AND %RangeLimitType <> 'File Relationship')
        #FIX(%File,%Primary)
        #FIX(%Key,%PrimaryKey)
        #FOR(%KeyField)
          #IF(UPPER(%KeyField)=UPPER(%RangeField))
            #IF(%RangeLimitType <> 'Current Value')
              #BREAK
            #ENDIF
          #ENDIF
          #FIX(%Field,%KeyField)                 #! Fix to component of key
          #SET(%ValueConstruct,%mInstancePrefix & ':Save:' & %KeyField)
          #CASE(UPPER(%pAction))
          #OF('SAVE')
%ValueConstruct = %KeyField                  #<! Save Range Limit Field
          #OF('ASIGN')
%KeyField = %ValueConstruct                  #<! Restore Saved Range Limit Field
          #OF('BREAK')
IF %KeyField <> %ValueConstruct THEN BREAK.      #<! Validate Saved Range Limit Field
          #ELSE
             #ERROR(%Procedure&'MultiChildTree - GenerateRangeLimitSaveValueCode - wrong action - Action:'&%pAction)
          #ENDCASE
          #IF(UPPER(%KeyField)=UPPER(%RangeField))
            #BREAK
          #ENDIF
        #ENDFOR
  #ENDIF
#!
#!---------------------------------------------------------------------------
#!
#GROUP(%GetWindowStyleProperty,%pStyleType,%pStyleNumber,%pStyleString,%pProperty),AUTO
#DECLARE(%lStyleProperty)
#DECLARE(%lStyleNumber)
#SET(%lStyleNumber,%GetStyleNumber(%pStyleType,%pStyleNumber,%pStyleString))
#!IF(UPPER(%pStyleType)='LOCAL LIST')
   #SELECT(%WindowStyles,%lStyleNumber)
   #IF(%WindowStyles)
     #CASE(UPPER(%pProperty))
     #OF('FONTNAME')
        #IF(%WindowStyleFontNameIsVariable)
            #SET(%lStyleProperty,%WindowStyleFontNameVariable)
        #ELSE
            #SET(%lStyleProperty,''''&QUOTE(%WindowStyleFontName)&'''')
        #ENDIF
     #OF('FONTSIZE')
        #SET(%lStyleProperty,CHOOSE(%WindowStyleFontSizeIsVariable,%WindowStyleFontSizeVariable,%WindowStyleFontSize))
     #OF('FONTSTYLE')
        #SET(%lStyleProperty,CHOOSE(%WindowStyleFontStyleIsVariable,%WindowStyleFontStyleVariable,%WindowStyleFontStyle))
     #OF('TEXTCOLOR')
        #SET(%lStyleProperty,CHOOSE(%WindowStyleForegroundNormalIsVariable,%WindowStyleForegroundNormalVariable,%WindowStyleForegroundNormal))
     #OF('BACKCOLOR')
        #SET(%lStyleProperty,CHOOSE(%WindowStyleBackgroundNormalIsVariable,%WindowStyleBackgroundNormalVariable,%WindowStyleBackgroundNormal))
     #OF('TEXTSELECTED')
        #SET(%lStyleProperty,CHOOSE(%WindowStyleForegroundSelectedIsVariable,%WindowStyleForegroundSelectedVariable,%WindowStyleForegroundSelected))
     #OF('BACKSELECTED')
        #SET(%lStyleProperty,CHOOSE(%WindowStyleBackgroundSelectedIsVariable,%WindowStyleBackgroundSelectedVariable,%WindowStyleBackgroundSelected))
     #OF('PICTURE')
        #IF(%WindowStylePictureIsVariable)
           #SET(%lStyleProperty,'CLIP('&%WindowStylePictureVariable&')')
        #ELSE
           #IF(%WindowStylePicture)
              #SET(%lStyleProperty,''''&QUOTE(%WindowStylePicture)&'''')
           #ENDIF
        #ENDIF
     #ELSE
        #SET(%lStyleProperty,'')
     #ENDCASE
   #ENDIF
#!ENDIF
#RETURN(%lStyleProperty)
#!
#!
#GROUP(%GeneratePopupMenu),AUTO
#DECLARE(%lPopupMenuSep)
#DECLARE(%lPopupMenuCount)
#EMBED(%MCRTGeneratePopup,'Before Generate POPUP menu'),%ActiveTemplateInstance,HIDE
%mInstancePrefix:Popup = ''
#FOR(%PopupMenu)
    #IF(ITEMS(%PopupMenu)=INSTANCE(%PopupMenu))
        #SET(%lPopupMenuSep,'')
    #ELSE
        #SET(%lPopupMenuSep,'&''|''')
    #ENDIF
    #IF(%PopupMenuText='---')
%mInstancePrefix:Popup = CLIP(%mInstancePrefix:Popup) &'-' %lPopupMenuSep
      #CYCLE
    #ENDIF
    #IF(%PopupMenuConditionToShow)
IF %PopupMenuConditionToShow
    #ENDIF
    #IF(%PopupMenuSeparation)
%mInstancePrefix:Popup = CLIP(%mInstancePrefix:Popup) & '-|'
    #ENDIF
    #IF(%PopupMenuConditionToEnable)
IF %PopupMenuConditionToEnable
   %mInstancePrefix:Popup = CLIP(%mInstancePrefix:Popup) & %(%SVStripPling(%PopupMenuText)) %lPopupMenuSep
ELSE
   %mInstancePrefix:Popup = CLIP(%mInstancePrefix:Popup) & '~' & %(%SVStripPling(%PopupMenuText)) %lPopupMenuSep
END
    #ELSE
%mInstancePrefix:Popup = CLIP(%mInstancePrefix:Popup) & %(%SVStripPling(%PopupMenuText)) %lPopupMenuSep
    #ENDIF
    #IF(%PopupMenuConditionToShow)
END
    #ENDIF
#ENDFOR
#EMBED(%MCRTBeforeExecutePopup,'Before Execute POPUP menu'),%ActiveTemplateInstance,MAP(%ActiveTemplateInstance,%ActiveTemplateinstanceDescription)
IF %mInstancePrefix:Popup[(LEN(CLIP(%mInstancePrefix:Popup))):(LEN(CLIP(%mInstancePrefix:Popup)))]='|'
   %mInstancePrefix:Popup = %mInstancePrefix:Popup[1:(LEN(CLIP(%mInstancePrefix:Popup))-1)]
END
#?%mInstancePrefix:PopupSelection = POPUP(%mInstancePrefix:Popup)
#SET(%lPopupMenuCount,0)
#FOR(%PopupMenu)
    #IF(%PopupMenuText='---')
        #CYCLE
    #ENDIF
    #SET(%lPopupMenuCount,%lPopupMenuCount+1)
    #IF(%PopupMenuConditionToShow)
IF %PopupMenuConditionToShow
   IF %mInstancePrefix:PopupSelection=%lPopupMenuCount
      %PopupMenuAction
   END
ELSE
   %mInstancePrefix:PopupSelection+=1
END
    #ELSE
IF %mInstancePrefix:PopupSelection=%lPopupMenuCount
   %PopupMenuAction
END
    #ENDIF
#ENDFOR
#EMBED(%MCRTInsideExecutePopup,'Inside Execute POPUP menu'),%ActiveTemplateInstance,MAP(%ActiveTemplateInstance,%ActiveTemplateinstanceDescription)
#!
#!
#!
#GROUP(%AddSeparationToPopupMenu,%pPriority),AUTO
#ADD(%PopupMenu,FORMAT(%pPriority,@n03)&'---')
#SET(%PopupMenuText,'---')
#SET(%PopupMenuName,%pPriority&'---')
#!
#!
#!
#GROUP(%AddItemToPopupMenu,%pName,%pPriority,%pText,%pAction,%pConditionToEnable,%pConditionToShow,%pAddSeparation=0)
#FIND(%PopupMenuName,%pName)
#IF(%PopupMenuName<>%pName)
#ADD(%PopupMenu,FORMAT(%pPriority,@n03)&%pName)
#ENDIF
#SET(%PopupMenuName,%pName)
#SET(%PopupMenuText,%pText)
#SET(%PopupMenuConditionToEnable,%pConditionToEnable)
#SET(%PopupMenuConditionToShow,%pConditionToShow)
#SET(%PopupMenuAction,%pAction)
#SET(%PopupMenuSeparation,%pAddSeparation)
#!
#!
#!#############################################################################
#!
#!  Multi Child Relation Tree Control Template Groups
#!
#!#############################################################################
#! *** START OF GROUPS ***
#!
#!-----------------------------------------------------------------------------
#GROUP(%FileHasChildren,%pFile),PRESERVE
#IF(%pFile=%Primary AND ITEMS(%Secondary)>0)
    #RETURN(%True)
#ENDIF
#FOR(%Secondary)
  #IF(%SecondaryTo=%pFile)
      #RETURN(%True)
  #ENDIF
#ENDFOR
#RETURN(%False)
#!-----------------------------------------------------------------------------
#GROUP(%GenerateLevelEquates)
  #FOR(%mFileTreeList)
    #SET(%mTempInstance,INSTANCE(%mFileTreeList))
    #IF(%mFileTreeChildRoot)
      #SET(%mTempVariable,%mInstancePrefix & ':S_' & UPPER(%mFileTreeFile))
%[28]mTempVariable EQUATE(%mTempInstance) ! Level %mFileTreeLevel (ChildRoot)
    #ELSE
      #SET(%mTempVariable,%mInstancePrefix & ':F_' & UPPER(%mFileTreeFile))
%[28]mTempVariable EQUATE(%mTempInstance) ! Level %mFileTreeLevel
    #ENDIF
  #ENDFOR
#!-----------------------------------------------------------------------------
#GROUP(%GetDefaultStyle),AUTO
  #DECLARE(%Format)
  #DECLARE(%LoopIndex,LONG)
  #DECLARE(%StartPos,LONG)
  #EQUATE(%StyleNumber,%False)
  #FIND(%ControlInstance,%ActiveTemplateInstance,%Control)
  #SET(%Format,EXTRACT(%ControlStatement,'FORMAT',1))
  #SET(%StartPos,INSTRING('Z(',%Format,1,1))
  #LOOP,FOR(%LoopIndex,%StartPos + 2,LEN(%Format))
    #IF(SUB(%Format,%LoopIndex,1) = ')')
      #BREAK
    #ENDIF
    #SET(%StyleNumber,%StyleNumber & SUB(%Format,%LoopIndex,1))
  #ENDLOOP
  #RETURN(%StyleNumber)
#!-----------------------------------------------------------------------------
#GROUP(%MakeFontStyleList),AUTO
  #DECLARE(%Line)
  #FREE(%MCRTFontStyles)
  #OPEN('EQUATES.CLW'),READ
  #LOOP
    #READ(%Line)
    #IF(%Line = %EOF)
      #BREAK
    #ENDIF
    #IF(INSTRING('FONT:',%Line,1,1))
      #ADD(%MCRTFontStyles,SUB(%Line,1,INSTRING('EQUATE',%Line,1,1) - 1))
    #ENDIF
  #ENDLOOP
  #CLOSE('EQUATES.CLW'),READ
#!=============================================================================
#GROUP(%MakeFontCharsetList),AUTO
  #DECLARE(%Line)
  #FREE(%MCRTFontCharsets)
  #OPEN('EQUATES.CLW'),READ
  #LOOP
    #READ(%Line)
    #IF(%Line = %EOF)
      #BREAK
    #ENDIF
    #IF(INSTRING('CHARSET:',%Line,1,1))
      #ADD(%MCRTFontCharsets,SUB(%Line,1,INSTRING('EQUATE',%Line,1,1) - 1))
    #ENDIF
  #ENDLOOP
  #CLOSE('EQUATES.CLW'),READ
#!=============================================================================
#GROUP(%MakeIconList)
  #DECLARE(%Line)
  #FREE(%MCRTBuiltinIcons)
  #ADD(%MCRTBuiltinIcons,'None')
  #OPEN('EQUATES.CLW'),READ
  #LOOP
    #READ(%Line)
    #IF(%Line = %EOF)
      #BREAK
    #ENDIF
    #IF(INSTRING('ICON:',%Line,1,1))
      #ADD(%MCRTBuiltinIcons,SUB(%Line,1,INSTRING('EQUATE',%Line,1,1) - 1))
    #ENDIF
  #ENDLOOP
  #CLOSE('EQUATES.CLW'),READ
#!=============================================================================
#!#GROUP(%MakeLanguageList),AUTO
#!  #DECLARE(%Line)
#!  #FREE(%MCRTLanguages)
#!  #OPEN('MCRTREE.TRN'),READ
#!  #LOOP
#!    #READ(%Line)
#!    #IF(%Line = %EOF)
#!      #BREAK
#!    #ENDIF
#!    #IF(SUB(%Line,1,1) = '[' AND SUB(%Line,LEN(%Line),1) = ']')
#!      #ADD(%MCRTLanguages,SUB(%Line,2,LEN(%Line) - 2))
#!    #ENDIF
#!  #ENDLOOP
#!  #FIX(%MCRTLanguages,'English')
#!  #IF(~%MCRTLanguages)
#!    #ADD(%MCRTLanguages,'English')
#!  #ENDIF
#!  #CLOSE('MCRTREE.TRN'),READ
#!=============================================================================
#!#GROUP(%PrepareTranslator),AUTO
#!  #DECLARE(%Line)
#!  #DECLARE(%EqualPos,LONG)
#!  #EQUATE(%SectionFound,%False)
#!  #FREE(%MCRTTranslatingItems)
#!  #OPEN('MCRTREE.TRN'),READ
#!  #LOOP
#!    #READ(%Line)
#!    #IF(%Line = %EOF)
#!      #BREAK
#!    #ENDIF
#!    #IF(SUB(%Line,1,1) = '[' AND SUB(%Line,LEN(%Line),1) = ']')
#!      #IF(SUB(%Line,2,LEN(%Line) - 2) = %MCRTLanguage)
#!        #SET(%SectionFound,1)
#!      #ELSE
#!        #IF(%SectionFound)
#!          #SET(%SectionFound,%SectionFound + 1)
#!        #ENDIF
#!      #ENDIF
#!      #IF(%SectionFound = 2)
#!        #BREAK
#!      #ENDIF
#!    #ELSE
#!      #IF(~%Line OR ~%SectionFound OR SUB(%Line,1,1) = '!')
#!        #CYCLE
#!      #ENDIF
#!      #SET(%EqualPos,INSTRING('=',%Line,1,1))
#!      #IF(%EqualPos)
#!        #ADD(%MCRTTranslatingItems,SUB(%Line,1,%EqualPos - 1))
#!        #SET(%MCRTTranslation,SUB(%Line,%EqualPos + 1,LEN(%Line)))
#!      #ENDIF
#!    #ENDIF
#!  #ENDLOOP
#!  #CLOSE('MCRTREE.TRN'),READ
#!=============================================================================
#GROUP(%TranslateMacro,%MyMacro),AUTO
#DECLARE(%lMyMacro)
#!  #FIX(%MCRTTranslatingItems,%MyMacro)
#!  #IF(%MCRTTranslatingItems)
#!    #RETURN('''' & %MCRTTranslation & '''')
#!  #ELSE
    #CASE(UPPER(%MyMacro))
    #OF('MSG_TITLE')
      #SET(%lMyMacro,'Multi Child Relation Tree')
    #OF('MSG_ERR_GETFILE')
      #SET(%lMyMacro,'An error occured loading level.|File:')
    #OF('MSG_ERR_GETCHOICE')
      #SET(%lMyMacro,'An error occured getting tree queue data.|Description:')
    #OF('POPUP_EXPAND')
      #SET(%lMyMacro,'E&xpand Level')
    #OF('POPUP_CONTRACT')
      #SET(%lMyMacro,'Con&tract Level')
    #OF('POPUP_SETUP')
      #SET(%lMyMacro,'&Setup Tree...')
    #OF('POPUP_REFRESH')
      #SET(%lMyMacro,'&Refresh Tree')
    #OF('POPUP_INSERT')
      #SET(%lMyMacro,'&Insert...')
    #OF('POPUP_CHANGE')
      #SET(%lMyMacro,'&Change...')
    #OF('POPUP_DELETE')
      #SET(%lMyMacro,'&Delete')
    #OF('POPUP_VIEW')
      #SET(%lMyMacro,'&View')
    #OF('SETUP_ROOT')
      #SET(%lMyMacro,'Root')
    #OF('SETUP_CHILDROOT')
      #SET(%lMyMacro,'(Child Root)')
    #OF('SETUP_NFG')
      #SET(%lMyMacro,'Normal Foreground')
    #OF('SETUP_NBG')
      #SET(%lMyMacro,'Normal Background')
    #OF('SETUP_SFG')
      #SET(%lMyMacro,'Selected Foreground')
    #OF('SETUP_SBG')
      #SET(%lMyMacro,'Selected Background')
    #OF('SETUP_FONT')
      #SET(%lMyMacro,'Font')
    #OF('SETUP_ITEM')
      #SET(%lMyMacro,'Level Setup...')
    #OF('SETUP_CHOOSE')
      #SET(%lMyMacro,'Choose')
    #OF('SETUP_TIP')
      #SET(%lMyMacro,'To access configuration menu please click right mouse button and select appropriate popup menu item')
    #OF('SETUP_TITLE')
      #SET(%lMyMacro,'Tree Custom Setup')
    #OF('SETUP_SAVE')
      #SET(%lMyMacro,'&Save')
    #OF('SETUP_CANCEL')
      #SET(%lMyMacro,'Cancel')
    #OF('SETUP_LEVEL_TITLE')
      #SET(%lMyMacro,'Level Setup')
    #OF('SETUP_LEVEL_PREFIX')
      #SET(%lMyMacro,'Prefix:')
    #OF('SETUP_LEVEL_COLOR_GROUP')
      #SET(%lMyMacro,'Colors')
    #OF('SETUP_LEVEL_NFG')
      #SET(%lMyMacro,'Normal Foreground:')
    #OF('SETUP_LEVEL_NBG')
      #SET(%lMyMacro,'Normal Background:')
    #OF('SETUP_LEVEL_SFG')
      #SET(%lMyMacro,'Selected Foreground:')
    #OF('SETUP_LEVEL_SBG')
      #SET(%lMyMacro,'Selected Background:')
    #OF('SETUP_LEVEL_FONT_GROUP')
      #SET(%lMyMacro,'Font')
    #OF('SETUP_LEVEL_FONT')
      #SET(%lMyMacro,'Face:')
    #OF('SETUP_LEVEL_SIZE')
      #SET(%lMyMacro,'Size:')
    #OF('SETUP_LEVEL_STYLE_GROUP')
      #SET(%lMyMacro,'Font Styles')
    #OF('SETUP_LEVEL_UNDERLINE')
      #SET(%lMyMacro,'Underline')
    #OF('SETUP_LEVEL_ITALIC')
      #SET(%lMyMacro,'Italic')
    #OF('SETUP_LEVEL_BOLD')
      #SET(%lMyMacro,'Bold')
    #OF('SETUP_LEVEL_STRIKEOUT')
      #SET(%lMyMacro,'Strikeout')
    #OF('SETUP_LEVEL_OK')
      #SET(%lMyMacro,'&OK')
    #OF('SETUP_LEVEL_CANCEL')
      #SET(%lMyMacro,'Cancel')
    #OF('INFO_TIP')
      #SET(%lMyMacro,'To add a record of current level type use key(s):<32>')
    #ELSE
      #ERROR('The equate ' & %MyMacro & ' is not found!')
    #ENDCASE
    #RETURN('''' & %lMyMacro & '''')
#!  #ENDIF
#!=============================================================================
#GROUP(%ConvertData,%MyData),AUTO
  #IF(SUB(%MyData,1,1) = '!')
    #SET(%MyData,SUB(%MyData,2,LEN(%MyData)))
  #ELSE
    #SET(%MyData,'''' & %MyData & '''')
  #ENDIF
  #RETURN(%MyData)
#!=============================================================================
#GROUP(%GetField,%MyField),AUTO,PRESERVE
  #DECLARE(%ExpressionField)
  #FIND(%Field,%MyField)
  #IF(INSTRING(%FieldType,'STRING-CSTRING-PSTRING-GROUP',1,1))
    #SET(%ExpressionField,'UPPER(CLIP(' & %MyField & '))')
  #ELSE
    #SET(%ExpressionField,%MyField)
  #ENDIF
  #RETURN(%ExpressionField)
#!=============================================================================
#GROUP(%SetCustomModuleMapProcedurePrototype,%MyProcedurePrototype)
  #IF(%CWTemplateVersion > 'v3' AND VAREXISTS(%ABCVersion))
    #SET(%CustomModuleMapProcedurePrototype,'PROCEDURE' & %MyProcedurePrototype)
  #ELSE
    #SET(%CustomModuleMapProcedurePrototype,%MyProcedurePrototype)
  #ENDIF
#!=============================================================================
#GROUP(%FillIconList)
  #IF(%MCRTRootIcon)
    #CALL(%AddListIcon,%MCRTRootIcon)
  #ENDIF
  #IF(%MCRTPrimaryUseExpandAndContractIcon)
     #CASE(%MCRTPrimaryDefaultContractedIconType)
     #OF('Builtin')
        #IF(%MCRTPrimaryDefaultContractedIconB <> 'None')
          #CALL(%AddListIcon,%MCRTPrimaryDefaultContractedIconB)
        #ENDIF
     #OF('File')
        #IF(%MCRTPrimaryDefaultContractedIconF)
          #CALL(%AddListIcon,%MCRTPrimaryDefaultContractedIconF)
        #ENDIF
     #OF('Variable')
        #IF(%MCRTPrimaryDefaultContractedIconV)
          #CALL(%AddListIcon,%MCRTPrimaryDefaultContractedIconV)
        #ENDIF
     #OF('External')
        #IF(%MCRTPrimaryDefaultContractedIconE)
          #CALL(%AddListIcon,%MCRTPrimaryDefaultContractedIconE)
        #ENDIF
     #ENDCASE
     #CASE(%MCRTPrimaryDefaultExpandedIconType)
     #OF('Builtin')
        #IF(%MCRTPrimaryDefaultExpandedIconB <> 'None')
          #CALL(%AddListIcon,%MCRTPrimaryDefaultExpandedIconB)
        #ENDIF
     #OF('File')
        #IF(%MCRTPrimaryDefaultExpandedIconF)
          #CALL(%AddListIcon,%MCRTPrimaryDefaultExpandedIconF)
        #ENDIF
     #OF('Variable')
        #IF(%MCRTPrimaryDefaultExpandedIconV)
          #CALL(%AddListIcon,%MCRTPrimaryDefaultExpandedIconV)
        #ENDIF
     #OF('External')
        #IF(%MCRTPrimaryDefaultExpandedIconE)
          #CALL(%AddListIcon,%MCRTPrimaryDefaultExpandedIconE)
        #ENDIF
     #ENDCASE
  #ELSE
     #CASE(%MCRTPrimaryDefaultIconType)
     #OF('Builtin')
        #IF(%MCRTPrimaryDefaultIconB <> 'None')
          #CALL(%AddListIcon,%MCRTPrimaryDefaultIconB)
        #ENDIF
     #OF('File')
        #IF(%MCRTPrimaryDefaultIconF)
          #CALL(%AddListIcon,%MCRTPrimaryDefaultIconF)
        #ENDIF
     #OF('Variable')
        #IF(%MCRTPrimaryDefaultIconV)
          #CALL(%AddListIcon,%MCRTPrimaryDefaultIconV)
        #ENDIF
     #OF('External')
        #IF(%MCRTPrimaryDefaultIconE)
          #CALL(%AddListIcon,%MCRTPrimaryDefaultIconE)
        #ENDIF
     #ENDCASE
  #ENDIF
  #FOR(%MCRTPrimaryConditionalIcons)
     #IF(%MCRTPrimaryUseExpandAndContractIcon)
        #CASE(%MCRTPrimaryConditionalExpandedIconType)
        #OF('Builtin')
           #IF(%MCRTPrimaryConditionalExpandedIconB <> 'None')
             #CALL(%AddListIcon,%MCRTPrimaryConditionalExpandedIconB)
           #ENDIF
        #OF('File')
           #IF(%MCRTPrimaryConditionalExpandedIconF)
             #CALL(%AddListIcon,%MCRTPrimaryConditionalExpandedIconF)
           #ENDIF
        #OF('Variable')
           #IF(%MCRTPrimaryConditionalExpandedIconV)
             #CALL(%AddListIcon,%MCRTPrimaryConditionalExpandedIconV)
           #ENDIF
        #OF('External')
           #IF(%MCRTPrimaryConditionalExpandedIconE)
             #CALL(%AddListIcon,%MCRTPrimaryConditionalExpandedIconE)
           #ENDIF
        #ENDCASE
        #CASE(%MCRTPrimaryConditionalContractedIconType)
        #OF('Builtin')
           #IF(%MCRTPrimaryConditionalContractedIconB <> 'None')
             #CALL(%AddListIcon,%MCRTPrimaryConditionalContractedIconB)
           #ENDIF
        #OF('File')
           #IF(%MCRTPrimaryConditionalContractedIconF)
             #CALL(%AddListIcon,%MCRTPrimaryConditionalContractedIconF)
           #ENDIF
        #OF('Variable')
           #IF(%MCRTPrimaryConditionalContractedIconV)
             #CALL(%AddListIcon,%MCRTPrimaryConditionalContractedIconV)
           #ENDIF
        #OF('External')
           #IF(%MCRTPrimaryConditionalContractedIconE)
             #CALL(%AddListIcon,%MCRTPrimaryConditionalContractedIconE)
           #ENDIF
        #ENDCASE
     #ELSE
        #CASE(%MCRTPrimaryConditionalIconType)
        #OF('Builtin')
           #IF(%MCRTPrimaryConditionalIconB <> 'None')
             #CALL(%AddListIcon,%MCRTPrimaryConditionalIconB)
           #ENDIF
        #OF('File')
           #IF(%MCRTPrimaryConditionalIconF)
             #CALL(%AddListIcon,%MCRTPrimaryConditionalIconF)
           #ENDIF
        #OF('Variable')
           #IF(%MCRTPrimaryConditionalIconV)
             #CALL(%AddListIcon,%MCRTPrimaryConditionalIconV)
           #ENDIF
        #OF('External')
           #IF(%MCRTPrimaryConditionalIconE)
             #CALL(%AddListIcon,%MCRTPrimaryConditionalIconE)
           #ENDIF
        #ENDCASE
     #ENDIF
  #ENDFOR
  #!!!!!!!!!!!!!!!!
  #! CHILD
  #!!!!!!!!!!!!!!!!
  #!FOR(%Secondary),WHERE(%SecondaryType = 'MANY:1')
  #FOR(%Secondary)
    #IF(%MCRTChildUseExpandAndContractIcon)
        #CASE(%MCRTChildDefaultExpandedIconType)
        #OF('Builtin')
           #IF(%MCRTChildDefaultExpandedIconB <> 'None')
             #CALL(%AddListIcon,%MCRTChildDefaultExpandedIconB)
           #ENDIF
        #OF('File')
           #IF(%MCRTChildDefaultExpandedIconF)
             #CALL(%AddListIcon,%MCRTChildDefaultExpandedIconF)
           #ENDIF
        #OF('Variable')
           #IF(%MCRTChildDefaultExpandedIconV)
             #CALL(%AddListIcon,%MCRTChildDefaultExpandedIconV)
           #ENDIF
        #OF('External')
           #IF(%MCRTChildDefaultExpandedIconE)
             #CALL(%AddListIcon,%MCRTChildDefaultExpandedIconE)
           #ENDIF
        #ENDCASE
        #CASE(%MCRTChildDefaultContractedIconType)
        #OF('Builtin')
           #IF(%MCRTChildDefaultContractedIconB <> 'None')
             #CALL(%AddListIcon,%MCRTChildDefaultContractedIconB)
           #ENDIF
        #OF('File')
           #IF(%MCRTChildDefaultContractedIconF)
             #CALL(%AddListIcon,%MCRTChildDefaultContractedIconF)
           #ENDIF
        #OF('Variable')
           #IF(%MCRTChildDefaultContractedIconV)
             #CALL(%AddListIcon,%MCRTChildDefaultContractedIconV)
           #ENDIF
        #OF('External')
           #IF(%MCRTChildDefaultContractedIconE)
             #CALL(%AddListIcon,%MCRTChildDefaultContractedIconE)
           #ENDIF
        #ENDCASE
        #FOR(%MCRTChildConditionalIcons)
           #CASE(%MCRTChildConditionalExpandedIconType)
           #OF('Builtin')
              #IF(%MCRTChildConditionalExpandedIconB <> 'None')
                #CALL(%AddListIcon,%MCRTChildConditionalExpandedIconB)
              #ENDIF
           #OF('File')
              #IF(%MCRTChildConditionalExpandedIconF)
                #CALL(%AddListIcon,%MCRTChildConditionalExpandedIconF)
              #ENDIF
           #OF('Variable')
              #IF(%MCRTChildConditionalExpandedIconV)
                #CALL(%AddListIcon,%MCRTChildConditionalExpandedIconV)
              #ENDIF
           #OF('External')
              #IF(%MCRTChildConditionalExpandedIconE)
                #CALL(%AddListIcon,%MCRTChildConditionalExpandedIconE)
              #ENDIF
           #ENDCASE
           #CASE(%MCRTChildConditionalContractedIconType)
           #OF('Builtin')
              #IF(%MCRTChildConditionalContractedIconB <> 'None')
                #CALL(%AddListIcon,%MCRTChildConditionalContractedIconB)
              #ENDIF
           #OF('File')
              #IF(%MCRTChildConditionalContractedIconF)
                #CALL(%AddListIcon,%MCRTChildConditionalContractedIconF)
              #ENDIF
           #OF('Variable')
              #IF(%MCRTChildConditionalContractedIconV)
                #CALL(%AddListIcon,%MCRTChildConditionalContractedIconV)
              #ENDIF
           #OF('External')
              #IF(%MCRTChildConditionalContractedIconE)
                #CALL(%AddListIcon,%MCRTChildConditionalContractedIconE)
              #ENDIF
           #ENDCASE
        #ENDFOR
    #ELSE
        #CASE(%MCRTChildDefaultIconType)
        #OF('Builtin')
           #IF(%MCRTChildDefaultIconB <> 'None')
             #CALL(%AddListIcon,%MCRTChildDefaultIconB)
           #ENDIF
        #OF('File')
           #IF(%MCRTChildDefaultIconF)
             #CALL(%AddListIcon,%MCRTChildDefaultIconF)
           #ENDIF
        #OF('Variable')
           #IF(%MCRTChildDefaultIconV)
             #CALL(%AddListIcon,%MCRTChildDefaultIconV)
           #ENDIF
        #OF('External')
           #IF(%MCRTChildDefaultIconE)
             #CALL(%AddListIcon,%MCRTChildDefaultIconE)
           #ENDIF
        #ENDCASE
        #FOR(%MCRTChildConditionalIcons)
           #CASE(%MCRTChildConditionalIconType)
           #OF('Builtin')
              #IF(%MCRTChildConditionalIconB <> 'None')
                #CALL(%AddListIcon,%MCRTChildConditionalIconB)
              #ENDIF
           #OF('File')
              #IF(%MCRTChildConditionalIconF)
                #CALL(%AddListIcon,%MCRTChildConditionalIconF)
              #ENDIF
           #OF('Variable')
              #IF(%MCRTChildConditionalIconV)
                #CALL(%AddListIcon,%MCRTChildConditionalIconV)
              #ENDIF
           #OF('External')
              #IF(%MCRTChildConditionalIconE)
                #CALL(%AddListIcon,%MCRTChildConditionalIconE)
              #ENDIF
           #ENDCASE
        #ENDFOR
    #ENDIF
    #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    #! CHILD ROOT
    #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     #IF(%MCRTChildRootLevelName)
        #IF(%MCRTChildRootUseExpandAndContractIcon)
            #CASE(%MCRTChildRootDefaultExpandedIconType)
            #OF('Builtin')
               #IF(%MCRTChildRootDefaultExpandedIconB <> 'None')
                 #CALL(%AddListIcon,%MCRTChildRootDefaultExpandedIconB)
               #ENDIF
            #OF('File')
               #IF(%MCRTChildRootDefaultExpandedIconF)
                 #CALL(%AddListIcon,%MCRTChildRootDefaultExpandedIconF)
               #ENDIF
            #OF('Variable')
               #IF(%MCRTChildRootDefaultExpandedIconV)
                 #CALL(%AddListIcon,%MCRTChildRootDefaultExpandedIconV)
               #ENDIF
            #OF('External')
               #IF(%MCRTChildRootDefaultExpandedIconE)
                 #CALL(%AddListIcon,%MCRTChildRootDefaultExpandedIconE)
               #ENDIF
            #ENDCASE
            #CASE(%MCRTChildRootDefaultContractedIconType)
            #OF('Builtin')
               #IF(%MCRTChildRootDefaultContractedIconB <> 'None')
                 #CALL(%AddListIcon,%MCRTChildRootDefaultContractedIconB)
               #ENDIF
            #OF('File')
               #IF(%MCRTChildRootDefaultContractedIconF)
                 #CALL(%AddListIcon,%MCRTChildRootDefaultContractedIconF)
               #ENDIF
            #OF('Variable')
               #IF(%MCRTChildRootDefaultContractedIconV)
                 #CALL(%AddListIcon,%MCRTChildRootDefaultContractedIconV)
               #ENDIF
            #OF('External')
               #IF(%MCRTChildRootDefaultContractedIconE)
                 #CALL(%AddListIcon,%MCRTChildRootDefaultContractedIconE)
               #ENDIF
            #ENDCASE
            #FOR(%MCRTChildRootConditionalIcons)
               #CASE(%MCRTChildRootConditionalExpandedIconType)
               #OF('Builtin')
                  #IF(%MCRTChildRootConditionalExpandedIconB <> 'None')
                    #CALL(%AddListIcon,%MCRTChildRootConditionalExpandedIconB)
                  #ENDIF
               #OF('File')
                  #IF(%MCRTChildRootConditionalExpandedIconF)
                    #CALL(%AddListIcon,%MCRTChildRootConditionalExpandedIconF)
                  #ENDIF
               #OF('Variable')
                  #IF(%MCRTChildRootConditionalExpandedIconV)
                    #CALL(%AddListIcon,%MCRTChildRootConditionalExpandedIconV)
                  #ENDIF
               #OF('External')
                  #IF(%MCRTChildRootConditionalExpandedIconE)
                    #CALL(%AddListIcon,%MCRTChildRootConditionalExpandedIconE)
                  #ENDIF
               #ENDCASE
               #CASE(%MCRTChildRootConditionalContractedIconType)
               #OF('Builtin')
                  #IF(%MCRTChildRootConditionalContractedIconB <> 'None')
                    #CALL(%AddListIcon,%MCRTChildRootConditionalContractedIconB)
                  #ENDIF
               #OF('File')
                  #IF(%MCRTChildRootConditionalContractedIconF)
                    #CALL(%AddListIcon,%MCRTChildRootConditionalContractedIconF)
                  #ENDIF
               #OF('Variable')
                  #IF(%MCRTChildRootConditionalContractedIconV)
                    #CALL(%AddListIcon,%MCRTChildRootConditionalContractedIconV)
                  #ENDIF
               #OF('External')
                  #IF(%MCRTChildRootConditionalContractedIconE)
                    #CALL(%AddListIcon,%MCRTChildRootConditionalContractedIconE)
                  #ENDIF
               #ENDCASE
            #ENDFOR
        #ELSE
            #CASE(%MCRTChildRootDefaultIconType)
            #OF('Builtin')
               #IF(%MCRTChildRootDefaultIconB <> 'None')
                 #CALL(%AddListIcon,%MCRTChildRootDefaultIconB)
               #ENDIF
            #OF('File')
               #IF(%MCRTChildRootDefaultIconF)
                 #CALL(%AddListIcon,%MCRTChildRootDefaultIconF)
               #ENDIF
            #OF('Variable')
               #IF(%MCRTChildRootDefaultIconV)
                 #CALL(%AddListIcon,%MCRTChildRootDefaultIconV)
               #ENDIF
            #OF('External')
               #IF(%MCRTChildRootDefaultIconE)
                 #CALL(%AddListIcon,%MCRTChildRootDefaultIconE)
               #ENDIF
            #ENDCASE
            #FOR(%MCRTChildRootConditionalIcons)
               #CASE(%MCRTChildRootConditionalIconType)
               #OF('Builtin')
                  #IF(%MCRTChildRootConditionalIconB <> 'None')
                    #CALL(%AddListIcon,%MCRTChildRootConditionalIconB)
                  #ENDIF
               #OF('File')
                  #IF(%MCRTChildRootConditionalIconF)
                    #CALL(%AddListIcon,%MCRTChildRootConditionalIconF)
                  #ENDIF
               #OF('Variable')
                  #IF(%MCRTChildRootConditionalIconV)
                    #CALL(%AddListIcon,%MCRTChildRootConditionalIconV)
                  #ENDIF
               #OF('External')
                  #IF(%MCRTChildRootConditionalIconE)
                    #CALL(%AddListIcon,%MCRTChildRootConditionalIconE)
                  #ENDIF
               #ENDCASE
            #ENDFOR
        #ENDIF
     #ENDIF
  #ENDFOR
#!=============================================================================
#GROUP(%AddIconsToProject)
  #IF(%MCRTRootIcon)
    #CALL(%AddIconToProject,%MCRTRootIcon)
  #ENDIF
  #IF(%MCRTPrimaryUseExpandAndContractIcon)
     #CASE(%MCRTPrimaryDefaultContractedIconType)
     #OF('Builtin')
        #IF(%MCRTPrimaryDefaultContractedIconB <> 'None')
          #CALL(%AddIconToProject,%MCRTPrimaryDefaultContractedIconB)
        #ENDIF
     #OF('File')
        #IF(%MCRTPrimaryDefaultContractedIconF)
          #CALL(%AddIconToProject,%MCRTPrimaryDefaultContractedIconF)
        #ENDIF
     #ENDCASE
     #CASE(%MCRTPrimaryDefaultExpandedIconType)
     #OF('Builtin')
        #IF(%MCRTPrimaryDefaultExpandedIconB <> 'None')
          #CALL(%AddIconToProject,%MCRTPrimaryDefaultExpandedIconB)
        #ENDIF
     #OF('File')
        #IF(%MCRTPrimaryDefaultExpandedIconF)
          #CALL(%AddIconToProject,%MCRTPrimaryDefaultExpandedIconF)
        #ENDIF
     #ENDCASE
  #ELSE
     #CASE(%MCRTPrimaryDefaultIconType)
     #OF('Builtin')
        #IF(%MCRTPrimaryDefaultIconB <> 'None')
          #CALL(%AddIconToProject,%MCRTPrimaryDefaultIconB)
        #ENDIF
     #OF('File')
        #IF(%MCRTPrimaryDefaultIconF)
          #CALL(%AddIconToProject,%MCRTPrimaryDefaultIconF)
        #ENDIF
     #ENDCASE
  #ENDIF
  #FOR(%MCRTPrimaryConditionalIcons)
     #IF(%MCRTPrimaryUseExpandAndContractIcon)
        #CASE(%MCRTPrimaryConditionalExpandedIconType)
        #OF('Builtin')
           #IF(%MCRTPrimaryConditionalExpandedIconB <> 'None')
             #CALL(%AddIconToProject,%MCRTPrimaryConditionalExpandedIconB)
           #ENDIF
        #OF('File')
           #IF(%MCRTPrimaryConditionalExpandedIconF)
             #CALL(%AddIconToProject,%MCRTPrimaryConditionalExpandedIconF)
           #ENDIF
        #ENDCASE
        #CASE(%MCRTPrimaryConditionalContractedIconType)
        #OF('Builtin')
           #IF(%MCRTPrimaryConditionalContractedIconB <> 'None')
             #CALL(%AddIconToProject,%MCRTPrimaryConditionalContractedIconB)
           #ENDIF
        #OF('File')
           #IF(%MCRTPrimaryConditionalContractedIconF)
             #CALL(%AddIconToProject,%MCRTPrimaryConditionalContractedIconF)
           #ENDIF
        #ENDCASE
     #ELSE
        #CASE(%MCRTPrimaryConditionalIconType)
        #OF('Builtin')
           #IF(%MCRTPrimaryConditionalIconB <> 'None')
             #CALL(%AddIconToProject,%MCRTPrimaryConditionalIconB)
           #ENDIF
        #OF('File')
           #IF(%MCRTPrimaryConditionalIconF)
             #CALL(%AddIconToProject,%MCRTPrimaryConditionalIconF)
           #ENDIF
        #ENDCASE
     #ENDIF
  #ENDFOR
  #!!!!!!!!!!!!!!!!
  #! CHILD
  #!!!!!!!!!!!!!!!!
  #!FOR(%Secondary),WHERE(%SecondaryType = 'MANY:1')
  #FOR(%Secondary)
    #IF(%MCRTChildUseExpandAndContractIcon)
        #CASE(%MCRTChildDefaultExpandedIconType)
        #OF('Builtin')
           #IF(%MCRTChildDefaultExpandedIconB <> 'None')
             #CALL(%AddIconToProject,%MCRTChildDefaultExpandedIconB)
           #ENDIF
        #OF('File')
           #IF(%MCRTChildDefaultExpandedIconF)
             #CALL(%AddIconToProject,%MCRTChildDefaultExpandedIconF)
           #ENDIF
        #ENDCASE
        #CASE(%MCRTChildDefaultContractedIconType)
        #OF('Builtin')
           #IF(%MCRTChildDefaultContractedIconB <> 'None')
             #CALL(%AddIconToProject,%MCRTChildDefaultContractedIconB)
           #ENDIF
        #OF('File')
           #IF(%MCRTChildDefaultContractedIconF)
             #CALL(%AddIconToProject,%MCRTChildDefaultContractedIconF)
           #ENDIF
        #ENDCASE
        #FOR(%MCRTChildConditionalIcons)
           #CASE(%MCRTChildConditionalExpandedIconType)
           #OF('Builtin')
              #IF(%MCRTChildConditionalExpandedIconB <> 'None')
                #CALL(%AddIconToProject,%MCRTChildConditionalExpandedIconB)
              #ENDIF
           #OF('File')
              #IF(%MCRTChildConditionalExpandedIconF)
                #CALL(%AddIconToProject,%MCRTChildConditionalExpandedIconF)
              #ENDIF
           #ENDCASE
           #CASE(%MCRTChildConditionalContractedIconType)
           #OF('Builtin')
              #IF(%MCRTChildConditionalContractedIconB <> 'None')
                #CALL(%AddIconToProject,%MCRTChildConditionalContractedIconB)
              #ENDIF
           #OF('File')
              #IF(%MCRTChildConditionalContractedIconF)
                #CALL(%AddIconToProject,%MCRTChildConditionalContractedIconF)
              #ENDIF
           #ENDCASE
        #ENDFOR
    #ELSE
        #CASE(%MCRTChildDefaultIconType)
        #OF('Builtin')
           #IF(%MCRTChildDefaultIconB <> 'None')
             #CALL(%AddIconToProject,%MCRTChildDefaultIconB)
           #ENDIF
        #OF('File')
           #IF(%MCRTChildDefaultIconF)
             #CALL(%AddIconToProject,%MCRTChildDefaultIconF)
           #ENDIF
        #ENDCASE
        #FOR(%MCRTChildConditionalIcons)
           #CASE(%MCRTChildConditionalIconType)
           #OF('Builtin')
              #IF(%MCRTChildConditionalIconB <> 'None')
                #CALL(%AddIconToProject,%MCRTChildConditionalIconB)
              #ENDIF
           #OF('File')
              #IF(%MCRTChildConditionalIconF)
                #CALL(%AddIconToProject,%MCRTChildConditionalIconF)
              #ENDIF
           #ENDCASE
        #ENDFOR
    #ENDIF
    #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    #! CHILD ROOT
    #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     #IF(%MCRTChildRootLevelName)
        #IF(%MCRTChildRootUseExpandAndContractIcon)
            #CASE(%MCRTChildRootDefaultExpandedIconType)
            #OF('Builtin')
               #IF(%MCRTChildRootDefaultExpandedIconB <> 'None')
                 #CALL(%AddIconToProject,%MCRTChildRootDefaultExpandedIconB)
               #ENDIF
            #OF('File')
               #IF(%MCRTChildRootDefaultExpandedIconF)
                 #CALL(%AddIconToProject,%MCRTChildRootDefaultExpandedIconF)
               #ENDIF
            #ENDCASE
            #CASE(%MCRTChildRootDefaultContractedIconType)
            #OF('Builtin')
               #IF(%MCRTChildRootDefaultContractedIconB <> 'None')
                 #CALL(%AddIconToProject,%MCRTChildRootDefaultContractedIconB)
               #ENDIF
            #OF('File')
               #IF(%MCRTChildRootDefaultContractedIconF)
                 #CALL(%AddIconToProject,%MCRTChildRootDefaultContractedIconF)
               #ENDIF
            #ENDCASE
            #FOR(%MCRTChildRootConditionalIcons)
               #CASE(%MCRTChildRootConditionalExpandedIconType)
               #OF('Builtin')
                  #IF(%MCRTChildRootConditionalExpandedIconB <> 'None')
                    #CALL(%AddIconToProject,%MCRTChildRootConditionalExpandedIconB)
                  #ENDIF
               #OF('File')
                  #IF(%MCRTChildRootConditionalExpandedIconF)
                    #CALL(%AddIconToProject,%MCRTChildRootConditionalExpandedIconF)
                  #ENDIF
               #ENDCASE
               #CASE(%MCRTChildRootConditionalContractedIconType)
               #OF('Builtin')
                  #IF(%MCRTChildRootConditionalContractedIconB <> 'None')
                    #CALL(%AddIconToProject,%MCRTChildRootConditionalContractedIconB)
                  #ENDIF
               #OF('File')
                  #IF(%MCRTChildRootConditionalContractedIconF)
                    #CALL(%AddIconToProject,%MCRTChildRootConditionalContractedIconF)
                  #ENDIF
               #ENDCASE
            #ENDFOR
        #ELSE
            #CASE(%MCRTChildRootDefaultIconType)
            #OF('Builtin')
               #IF(%MCRTChildRootDefaultIconB <> 'None')
                 #CALL(%AddIconToProject,%MCRTChildRootDefaultIconB)
               #ENDIF
            #OF('File')
               #IF(%MCRTChildRootDefaultIconF)
                 #CALL(%AddIconToProject,%MCRTChildRootDefaultIconF)
               #ENDIF
            #ENDCASE
            #FOR(%MCRTChildRootConditionalIcons)
               #CASE(%MCRTChildRootConditionalIconType)
               #OF('Builtin')
                  #IF(%MCRTChildRootConditionalIconB <> 'None')
                    #CALL(%AddIconToProject,%MCRTChildRootConditionalIconB)
                  #ENDIF
               #OF('File')
                  #IF(%MCRTChildRootConditionalIconF)
                    #CALL(%AddIconToProject,%MCRTChildRootConditionalIconF)
                  #ENDIF
               #ENDCASE
            #ENDFOR
        #ENDIF
     #ENDIF
  #ENDFOR
#!=============================================================================
#GROUP(%AddIconToProject,%MyIcon)
#IF(UPPER(SUB(%MyIcon,1,5)) = 'ICON:')
#ELSIF(SUB(%MyIcon,1,1) = '!')
#ELSIF(SUB(%MyIcon,1,1) = '~')
#ELSIF(SUB(%MyIcon,LEN(CLIP(%MyIcon)),1) = ']')
#ELSE
  #PROJECT(%MyIcon)
#ENDIF
#!=============================================================================
#GROUP(%AddListIcon,%MyIcon)
#IF(UPPER(SUB(%MyIcon,1,5)) = 'ICON:')
  #ADD(%IconList,%MyIcon)
  #SET(%IconListType,'Built-In')
#ELSIF(SUB(%MyIcon,1,1) = '!')
  #SET(%mTempVariable,SUB(%MyIcon,2,LEN(%MyIcon)))
  #ADD(%IconList,%mTempVariable)
  #SET(%IconListType,'Variable')
#ELSIF(SUB(%MyIcon,LEN(CLIP(%MyIcon)),1) = ']')
  #ADD(%IconList,%MyIcon)
  #SET(%IconListType,'External')
#ELSE
  #IF(SUB(%MyIcon,1,1) = '~')
    #SET(%mTempVariable,SUB(%MyIcon,2,LEN(%MyIcon)))
    #ADD(%IconList,%mTempVariable)
  #ELSE
    #ADD(%IconList,%MyIcon)
  #ENDIF
  #SET(%IconListType,'File')
#ENDIF
#!=============================================================================
#GROUP(%SetConditionalIcon,%MyLevelType),AUTO
  #DECLARE(%ConditionalIcon)
  #CASE(UPPER(%MyLevelType))
  #OF('PRIMARY')
    #IF(%MCRTPrimaryUseExpandAndContractIcon)
       #CASE(%MCRTPrimaryConditionalContractedIconType)
       #OF('Builtin')
         #SET(%ConditionalIcon,%MCRTPrimaryConditionalContractedIconB)
       #OF('File')
         #SET(%ConditionalIcon,%MCRTPrimaryConditionalContractedIconF)
       #OF('Variable')
         #SET(%ConditionalIcon,%MCRTPrimaryConditionalContractedIconV)
       #OF('External')
         #SET(%ConditionalIcon,%MCRTPrimaryConditionalContractedIconE)
       #ENDCASE
       #CASE(%MCRTPrimaryConditionalExpandedIconType)
       #OF('Builtin')
         #SET(%ConditionalIcon,%ConditionalIcon&' - '&%MCRTPrimaryConditionalExpandedIconB)
       #OF('File')
         #SET(%ConditionalIcon,%ConditionalIcon&' - '&%MCRTPrimaryConditionalExpandedIconF)
       #OF('Variable')
         #SET(%ConditionalIcon,%ConditionalIcon&' - '&%MCRTPrimaryConditionalExpandedIconV)
       #OF('External')
         #SET(%ConditionalIcon,%ConditionalIcon&' - '&%MCRTPrimaryConditionalExpandedIconE)
       #ENDCASE
    #ELSE
       #CASE(%MCRTPrimaryConditionalIconType)
       #OF('Builtin')
         #SET(%ConditionalIcon,%MCRTPrimaryConditionalIconB)
       #OF('File')
         #SET(%ConditionalIcon,%MCRTPrimaryConditionalIconF)
       #OF('Variable')
         #SET(%ConditionalIcon,%MCRTPrimaryConditionalIconV)
       #OF('External')
         #SET(%ConditionalIcon,%MCRTPrimaryConditionalIconE)
       #ENDCASE
    #ENDIF
  #OF('CHILD')
    #IF(%MCRTChildUseExpandAndContractIcon)
       #CASE(%MCRTChildConditionalContractedIconType)
       #OF('Builtin')
         #SET(%ConditionalIcon,%MCRTChildConditionalContractedIconB)
       #OF('File')
         #SET(%ConditionalIcon,%MCRTChildConditionalContractedIconF)
       #OF('Variable')
         #SET(%ConditionalIcon,%MCRTChildConditionalContractedIconV)
       #OF('External')
         #SET(%ConditionalIcon,%MCRTChildConditionalContractedIconE)
       #ENDCASE
       #CASE(%MCRTChildConditionalExpandedIconType)
       #OF('Builtin')
         #SET(%ConditionalIcon,%ConditionalIcon&' - '&%MCRTChildConditionalExpandedIconB)
       #OF('File')
         #SET(%ConditionalIcon,%ConditionalIcon&' - '&%MCRTChildConditionalExpandedIconF)
       #OF('Variable')
         #SET(%ConditionalIcon,%ConditionalIcon&' - '&%MCRTChildConditionalExpandedIconV)
       #OF('External')
         #SET(%ConditionalIcon,%ConditionalIcon&' - '&%MCRTChildConditionalExpandedIconE)
       #ENDCASE
    #ELSE
       #CASE(%MCRTChildConditionalIconType)
       #OF('Builtin')
         #SET(%ConditionalIcon,%MCRTChildConditionalIconB)
       #OF('File')
         #SET(%ConditionalIcon,%MCRTChildConditionalIconF)
       #OF('Variable')
         #SET(%ConditionalIcon,%MCRTChildConditionalIconV)
       #OF('External')
         #SET(%ConditionalIcon,%MCRTChildConditionalIconE)
       #ENDCASE
    #ENDIF
  #OF('CHILDROOT')
    #IF(%MCRTChildRootUseExpandAndContractIcon)
       #CASE(%MCRTChildRootConditionalContractedIconType)
       #OF('Builtin')
         #SET(%ConditionalIcon,%MCRTChildRootConditionalContractedIconB)
       #OF('File')
         #SET(%ConditionalIcon,%MCRTChildRootConditionalContractedIconF)
       #OF('Variable')
         #SET(%ConditionalIcon,%MCRTChildRootConditionalContractedIconV)
       #OF('External')
         #SET(%ConditionalIcon,%MCRTChildRootConditionalContractedIconE)
       #ENDCASE
       #CASE(%MCRTChildRootConditionalExpandedIconType)
       #OF('Builtin')
         #SET(%ConditionalIcon,%ConditionalIcon&' - '&%MCRTChildRootConditionalExpandedIconB)
       #OF('File')
         #SET(%ConditionalIcon,%ConditionalIcon&' - '&%MCRTChildRootConditionalExpandedIconF)
       #OF('Variable')
         #SET(%ConditionalIcon,%ConditionalIcon&' - '&%MCRTChildRootConditionalExpandedIconV)
       #OF('External')
         #SET(%ConditionalIcon,%ConditionalIcon&' - '&%MCRTChildRootConditionalExpandedIconE)
       #ENDCASE
    #ELSE
       #CASE(%MCRTChildRootConditionalIconType)
       #OF('Builtin')
         #SET(%ConditionalIcon,%MCRTChildRootConditionalIconB)
       #OF('File')
         #SET(%ConditionalIcon,%MCRTChildRootConditionalIconF)
       #OF('Variable')
         #SET(%ConditionalIcon,%MCRTChildRootConditionalIconV)
       #OF('External')
         #SET(%ConditionalIcon,%MCRTChildRootConditionalIconE)
       #ENDCASE
    #ENDIF
  #ENDCASE
  #RETURN(%ConditionalIcon)
#!=============================================================================
#! Fix the icon from the icon list
#!
#GROUP(%FixIconList,%MyLevelType,%MyIsConditional,%pExpandedOrContracted)
  #FIX(%IconList,%False)
  #CASE(UPPER(%MyLevelType))
  #OF('PRIMARY')
    #IF(%MyIsConditional)
       #CASE(UPPER(%pExpandedOrContracted))
       #OF('CONTRACTED')
          #CASE(%MCRTPrimaryConditionalContractedIconType)
          #OF('Builtin')
            #FIX(%IconList,%MCRTPrimaryConditionalContractedIconB)
          #OF('File')
            #FIX(%IconList,%MCRTPrimaryConditionalContractedIconF)
          #OF('Variable')
            #FIX(%IconList,%MCRTPrimaryConditionalContractedIconV)
          #OF('External')
            #FIX(%IconList,%MCRTPrimaryConditionalContractedIconE)
          #ENDCASE
       #OF('EXPANDED')
          #CASE(%MCRTPrimaryConditionalExpandedIconType)
          #OF('Builtin')
            #FIX(%IconList,%MCRTPrimaryConditionalExpandedIconB)
          #OF('File')
            #FIX(%IconList,%MCRTPrimaryConditionalExpandedIconF)
          #OF('Variable')
            #FIX(%IconList,%MCRTPrimaryConditionalExpandedIconV)
          #OF('External')
            #FIX(%IconList,%MCRTPrimaryConditionalExpandedIconE)
          #ENDCASE
       #ELSE
          #CASE(%MCRTPrimaryConditionalIconType)
          #OF('Builtin')
            #FIX(%IconList,%MCRTPrimaryConditionalIconB)
          #OF('File')
            #FIX(%IconList,%MCRTPrimaryConditionalIconF)
          #OF('Variable')
            #FIX(%IconList,%MCRTPrimaryConditionalIconV)
          #OF('External')
            #FIX(%IconList,%MCRTPrimaryConditionalIconE)
          #ENDCASE
       #ENDCASE
    #ELSE
       #CASE(UPPER(%pExpandedOrContracted))
       #OF('CONTRACTED')
          #CASE(%MCRTPrimaryDefaultContractedIconType)
          #OF('Builtin')
            #FIX(%IconList,%MCRTPrimaryDefaultContractedIconB)
          #OF('File')
            #FIX(%IconList,%MCRTPrimaryDefaultContractedIconF)
          #OF('Variable')
            #FIX(%IconList,%MCRTPrimaryDefaultContractedIconV)
          #OF('External')
            #FIX(%IconList,%MCRTPrimaryDefaultContractedIconE)
          #ENDCASE
       #OF('EXPANDED')
          #CASE(%MCRTPrimaryDefaultExpandedIconType)
          #OF('Builtin')
            #FIX(%IconList,%MCRTPrimaryDefaultExpandedIconB)
          #OF('File')
            #FIX(%IconList,%MCRTPrimaryDefaultExpandedIconF)
          #OF('Variable')
            #FIX(%IconList,%MCRTPrimaryDefaultExpandedIconV)
          #OF('External')
            #FIX(%IconList,%MCRTPrimaryDefaultExpandedIconE)
          #ENDCASE
       #ELSE
          #CASE(%MCRTPrimaryDefaultIconType)
          #OF('Builtin')
            #FIX(%IconList,%MCRTPrimaryDefaultIconB)
          #OF('File')
            #FIX(%IconList,%MCRTPrimaryDefaultIconF)
          #OF('Variable')
            #FIX(%IconList,%MCRTPrimaryDefaultIconV)
          #OF('External')
            #FIX(%IconList,%MCRTPrimaryDefaultIconE)
          #ENDCASE
       #ENDCASE
    #ENDIF
  #OF('CHILD')
    #IF(%MyIsConditional)
       #CASE(UPPER(%pExpandedOrContracted))
       #OF('CONTRACTED')
          #CASE(%MCRTChildConditionalContractedIconType)
          #OF('Builtin')
            #FIX(%IconList,%MCRTChildConditionalContractedIconB)
          #OF('File')
            #FIX(%IconList,%MCRTChildConditionalContractedIconF)
          #OF('Variable')
            #FIX(%IconList,%MCRTChildConditionalContractedIconV)
          #OF('External')
            #FIX(%IconList,%MCRTChildConditionalContractedIconE)
          #ENDCASE
       #OF('EXPANDED')
          #CASE(%MCRTChildConditionalExpandedIconType)
          #OF('Builtin')
            #FIX(%IconList,%MCRTChildConditionalExpandedIconB)
          #OF('File')
            #FIX(%IconList,%MCRTChildConditionalExpandedIconF)
          #OF('Variable')
            #FIX(%IconList,%MCRTChildConditionalExpandedIconV)
          #OF('External')
            #FIX(%IconList,%MCRTChildConditionalExpandedIconE)
          #ENDCASE
       #ELSE
          #CASE(%MCRTChildConditionalIconType)
          #OF('Builtin')
            #FIX(%IconList,%MCRTChildConditionalIconB)
          #OF('File')
            #FIX(%IconList,%MCRTChildConditionalIconF)
          #OF('Variable')
            #FIX(%IconList,%MCRTChildConditionalIconV)
          #OF('External')
            #FIX(%IconList,%MCRTChildConditionalIconE)
          #ENDCASE
       #ENDCASE
    #ELSE
       #CASE(UPPER(%pExpandedOrContracted))
       #OF('CONTRACTED')
          #CASE(%MCRTChildDefaultContractedIconType)
          #OF('Builtin')
            #FIX(%IconList,%MCRTChildDefaultContractedIconB)
          #OF('File')
            #FIX(%IconList,%MCRTChildDefaultContractedIconF)
          #OF('Variable')
            #FIX(%IconList,%MCRTChildDefaultContractedIconV)
          #OF('External')
            #FIX(%IconList,%MCRTChildDefaultContractedIconE)
          #ENDCASE
       #OF('EXPANDED')
          #CASE(%MCRTChildDefaultExpandedIconType)
          #OF('Builtin')
            #FIX(%IconList,%MCRTChildDefaultExpandedIconB)
          #OF('File')
            #FIX(%IconList,%MCRTChildDefaultExpandedIconF)
          #OF('Variable')
            #FIX(%IconList,%MCRTChildDefaultExpandedIconV)
          #OF('External')
            #FIX(%IconList,%MCRTChildDefaultExpandedIconE)
          #ENDCASE
       #ELSE
          #CASE(%MCRTChildDefaultIconType)
          #OF('Builtin')
            #FIX(%IconList,%MCRTChildDefaultIconB)
          #OF('File')
            #FIX(%IconList,%MCRTChildDefaultIconF)
          #OF('Variable')
            #FIX(%IconList,%MCRTChildDefaultIconV)
          #OF('External')
            #FIX(%IconList,%MCRTChildDefaultIconE)
          #ENDCASE
       #ENDCASE
    #ENDIF
  #OF('CHILDROOT')
    #IF(%MyIsConditional)
       #CASE(UPPER(%pExpandedOrContracted))
       #OF('CONTRACTED')
          #CASE(%MCRTChildRootConditionalContractedIconType)
          #OF('Builtin')
            #FIX(%IconList,%MCRTChildRootConditionalContractedIconB)
          #OF('File')
            #FIX(%IconList,%MCRTChildRootConditionalContractedIconF)
          #OF('Variable')
            #FIX(%IconList,%MCRTChildRootConditionalContractedIconV)
          #OF('External')
            #FIX(%IconList,%MCRTChildRootConditionalContractedIconE)
          #ENDCASE
       #OF('EXPANDED')
          #CASE(%MCRTChildRootConditionalExpandedIconType)
          #OF('Builtin')
            #FIX(%IconList,%MCRTChildRootConditionalExpandedIconB)
          #OF('File')
            #FIX(%IconList,%MCRTChildRootConditionalExpandedIconF)
          #OF('Variable')
            #FIX(%IconList,%MCRTChildRootConditionalExpandedIconV)
          #OF('External')
            #FIX(%IconList,%MCRTChildRootConditionalExpandedIconE)
          #ENDCASE
       #ELSE
          #CASE(%MCRTChildRootConditionalIconType)
          #OF('Builtin')
            #FIX(%IconList,%MCRTChildRootConditionalIconB)
          #OF('File')
            #FIX(%IconList,%MCRTChildRootConditionalIconF)
          #OF('Variable')
            #FIX(%IconList,%MCRTChildRootConditionalIconV)
          #OF('External')
            #FIX(%IconList,%MCRTChildRootConditionalIconE)
          #ENDCASE
       #ENDCASE
    #ELSE
       #CASE(UPPER(%pExpandedOrContracted))
       #OF('CONTRACTED')
          #CASE(%MCRTChildRootDefaultContractedIconType)
          #OF('Builtin')
            #FIX(%IconList,%MCRTChildRootDefaultContractedIconB)
          #OF('File')
            #FIX(%IconList,%MCRTChildRootDefaultContractedIconF)
          #OF('Variable')
            #FIX(%IconList,%MCRTChildRootDefaultContractedIconV)
          #OF('External')
            #FIX(%IconList,%MCRTChildRootDefaultContractedIconE)
          #ENDCASE
       #OF('EXPANDED')
          #CASE(%MCRTChildRootDefaultExpandedIconType)
          #OF('Builtin')
            #FIX(%IconList,%MCRTChildRootDefaultExpandedIconB)
          #OF('File')
            #FIX(%IconList,%MCRTChildRootDefaultExpandedIconF)
          #OF('Variable')
            #FIX(%IconList,%MCRTChildRootDefaultExpandedIconV)
          #OF('External')
            #FIX(%IconList,%MCRTChildRootDefaultExpandedIconE)
          #ENDCASE
       #ELSE
          #CASE(%MCRTChildRootDefaultIconType)
          #OF('Builtin')
            #FIX(%IconList,%MCRTChildRootDefaultIconB)
          #OF('File')
            #FIX(%IconList,%MCRTChildRootDefaultIconF)
          #OF('Variable')
            #FIX(%IconList,%MCRTChildRootDefaultIconV)
          #OF('External')
            #FIX(%IconList,%MCRTChildRootDefaultIconE)
          #ENDCASE
       #ENDCASE
    #ENDIF
  #ENDCASE
#!-----------------------------------------------------------------------------
#! Set or reset level icon
#!
#GROUP(%SetLevelIcon)
#!-----------------------------------------------------------------------------
#IF(%IconList)
  #CASE(%IconListType)
  #OF('Variable')
%mTreeQueue.m_Icon = %IconList
  #ELSE
    #SET(%mTempVariable,INSTANCE(%IconList))
%mTreeQueue.m_Icon = %mTempVariable
  #ENDCASE
#ELSE
%mTreeQueue.m_Icon = 0
#ENDIF
#!-----------------------------------------------------------------------------
#! Make instance level EQUATE
#!
#!-----------------------------------------------------------------------------
#GROUP(%GetInstanceEquate,%MyInstance),AUTO
  #DECLARE(%MyInstanceEquate)
  #SET(%MyInstanceEquate,%mInstancePrefix & ':')
  #SELECT(%mFileTreeList,%MyInstance)
  #IF(%mFileTreeChildRoot)
    #SET(%MyInstanceEquate,%MyInstanceEquate & 'S_')
  #ELSE
    #SET(%MyInstanceEquate,%MyInstanceEquate & 'F_')
  #ENDIF
  #SET(%MyInstanceEquate,%MyInstanceEquate & UPPER(%mFileTreeFile))
  #RETURN(%MyInstanceEquate)
#!-----------------------------------------------------------------------------
#!
#!
#!-----------------------------------------------------------------------------
#GROUP(%GetCurrentChoice)
%mInstancePrefix:CurrentChoice = %mTreeControl{PROPLIST:MouseDownRow}
GET(%mTreeQueue,%mInstancePrefix:CurrentChoice)
#!-----------------------------------------------------------------------------
#!
#!
#!-----------------------------------------------------------------------------
#GROUP(%MultipleFileInsertAction),AUTO,PRESERVE
#DECLARE(%lCounter)
#SET(%lCounter,0)
#IF(%AcceptToolBarControl)
#?IF %mInstancePrefix:LastInsertInstance = 0
   #?%mInstancePrefix:LastInsertInstance = POPUP(%(%ConvertData(%mPopupString)))
#?END
#?EXECUTE %mInstancePrefix:LastInsertInstance
#ELSE
#?EXECUTE POPUP(%(%ConvertData(%mPopupString)))
#ENDIF
  #IF(%DisablePrimaryLevel AND %mFileTreeLevel=1 AND NOT %mFileTreeChildRoot)
     #FIX(%Secondary,%mFileTreeFile)
     #IF(%MCRTSecondaryAllowInsert)
         #IF(%MCRTChildUpdateProcedure)
  #?BEGIN
    #SUSPEND
    #INSERT(%SingleFileInsertAction)
    #RESUME
  #?END
         #ENDIF
     #ENDIF
  #ENDIF
  #FOR(%mFileTreeList),WHERE((%mFileTreeLevel = %mLevelList + 1) AND %mFileTreeParent = %mTempVariable)
    #FIX(%Secondary,%mFileTreeFile)
    #IF(%MCRTSecondaryAllowInsert)
  #?BEGIN
    #SUSPEND
      #FIX(%File,%mFileTreeFile)
      #IF(UPPER(%AppTemplateFamily)='CLARION')
    #?GET(%File,0)
      #ENDIF
      #FOR(%Field),WHERE(%FieldType = 'MEMO')
    #?CLEAR(%Field)
      #ENDFOR
    #?CLEAR(%FilePrefix:Record)
      #FIX(%Relation,%mFileTreeParent)
      #FOR(%FileKeyField),WHERE(%FileKeyField AND %FileKeyFieldLink)
    #?%FileKeyField = %FileKeyFieldLink
      #ENDFOR
      #IF(UPPER(%AppTemplateFamily)='ABC')
    #?Access:%File.PrimeRecord(1)
      #ENDIF
      #SET(%mMCRTSecondaryAllowMOChildAutoinc,%MCRTSecondaryAllowMOChildAutoinc)
      #FOR(%mFileTreeOneToManyChild)
                     #FIX(%File,%mFileTreeOneToManyChild)
                     #IF(UPPER(%AppTemplateFamily)='CLARION')
      #?GET(%File,0)
                     #ENDIF
                     #FOR(%Field),WHERE(%FieldType = 'MEMO')
      #?CLEAR(%Field)
                     #ENDFOR
      #?CLEAR(%FilePrefix:Record)
                     #IF(%mMCRTSecondaryAllowMOChildAutoinc)
                     #IF(UPPER(%AppTemplateFamily)='ABC')
      #?Access:%File.PrimeRecord(1)
                     #ENDIF
                     #ENDIF
      #ENDFOR
      #SET(%lCounter,%lCounter+1)
      #IF(%AcceptToolBarControl)
    #?%mInstancePrefix:LastInsertInstance = %lCounter
      #ENDIF
      #FIX(%Secondary,%mFileTreeFile)
      #IF(%MCRTChildUpdateProcedure)
    #EMBED(%MCRTBeforeInsertLevel,'Before Insert Level'),%ActiveTemplateInstance,%mFileTreeFile,MAP(%ActiveTemplateInstance,%ActiveTemplateInstanceDescription)
    %MCRTChildUpdateProcedure%MCRTChildUpdateProcedureParameters
    #EMBED(%MCRTAfterInsertLevel,'After Insert Level'),%ActiveTemplateInstance,%mFileTreeFile,MAP(%ActiveTemplateInstance,%ActiveTemplateInstanceDescription)
      #ENDIF
    #?IF GlobalResponse = RequestCompleted
      #?%mInstancePrefix:NewItemLevel    = %(%mInstancePrefix & ':F_' & UPPER(%mFileTreeFile))
      #!#?%mInstancePrefix:NewItemPosition = POSITION(%mFileTreeKey)
      #?%mInstancePrefix:NewItemPosition = POSITION(%mTreeQueue.m_Key)
      #?DO %mInstancePrefix:ReloadTree
    #?END
    #RESUME
  #?END
    #ENDIF
  #ENDFOR
#?END
#!-----------------------------------------------------------------------------
#!
#!
#!-----------------------------------------------------------------------------
#GROUP(%SingleFileInsertAction)
  #SELECT(%mFileTreeList,%mTempInstance)
  #IF(%mFileTreeIsPrimary)
      #FIX(%File,%Primary)
      #IF(NOT %MCRTPrimaryAllowInsert)
!Insert on %File is not Allowed
         #RETURN
      #ENDIF
  #ELSE
      #FIX(%File,%mTempVariable)
      #FIX(%Secondary,%File)
      #IF(NOT %MCRTSecondaryAllowInsert)
!Insert on %File is not Allowed
         #RETURN
      #ENDIF
  #ENDIF
  #SUSPEND
    #IF(UPPER(%AppTemplateFamily)='CLARION')
#?GET(%File,0)
    #ENDIF
    #FOR(%Field),WHERE(%FieldType = 'MEMO')
#?CLEAR(%Field)
    #ENDFOR
#?CLEAR(%FilePrefix:Record)
    #IF(%mFileTreeIsPrimary)
      #IF(UPPER(%AppTemplateFamily)='ABC')
#?Access:%File.PrimeRecord()
      #ENDIF
      #SET(%mMCRTSecondaryAllowMOChildAutoinc,%MCRTSecondaryAllowMOChildAutoinc)
      #FOR(%mFileTreeOneToManyChild)
                     #FIX(%File,%mFileTreeOneToManyChild)
                     #IF(UPPER(%AppTemplateFamily)='CLARION')
#?GET(%File,0)
                     #ENDIF
                     #FOR(%Field),WHERE(%FieldType = 'MEMO')
#?CLEAR(%Field)
                     #ENDFOR
#?CLEAR(%FilePrefix:Record)
                     #IF(%mMCRTSecondaryAllowMOChildAutoinc)
                     #IF(UPPER(%AppTemplateFamily)='ABC')
#!#?Access:%File.PrimeRecord(1)
                     #ENDIF
                     #ENDIF
      #ENDFOR
      #IF(%MCRTChildUpdateProcedure)
#EMBED(%MCRTBeforeInsertLevel,'Before Insert Level'),%ActiveTemplateInstance,%File,MAP(%ActiveTemplateInstance,%ActiveTemplateInstanceDescription)
%MCRTChildUpdateProcedure%MCRTChildUpdateProcedureParameters
#EMBED(%MCRTAfterInsertLevel,'After Insert Level'),%ActiveTemplateInstance,%File,MAP(%ActiveTemplateInstance,%ActiveTemplateInstanceDescription)
      #ENDIF
    #ELSE
      #FIX(%Secondary,%mFileTreeFile)
      #FIX(%Relation,%mFileTreeParent)
      #FOR(%FileKeyField),WHERE(%FileKeyField AND %FileKeyFieldLink)
#?%FileKeyField = %FileKeyFieldLink
      #ENDFOR
      #IF(UPPER(%AppTemplateFamily)='ABC')
#?Access:%File.PrimeRecord(1)
      #ENDIF
      #SET(%mMCRTSecondaryAllowMOChildAutoinc,%MCRTSecondaryAllowMOChildAutoinc)
      #FOR(%mFileTreeOneToManyChild)
                     #FIX(%File,%mFileTreeOneToManyChild)
                     #IF(UPPER(%AppTemplateFamily)='CLARION')
#?GET(%File,0)
                     #ENDIF
                     #FOR(%Field),WHERE(%FieldType = 'MEMO')
#?CLEAR(%Field)
                     #ENDFOR
#?CLEAR(%FilePrefix:Record)
                     #IF(%mMCRTSecondaryAllowMOChildAutoinc)
                     #IF(UPPER(%AppTemplateFamily)='ABC')
#?Access:%File.PrimeRecord(1)
                     #ENDIF
                     #ENDIF
      #ENDFOR
      #FIX(%Secondary,%mTempVariable)
      #IF(%MCRTChildUpdateProcedure)
#EMBED(%MCRTBeforeInsertLevel,'Before Insert Level'),%ActiveTemplateInstance,%Secondary,MAP(%ActiveTemplateInstance,%ActiveTemplateInstanceDescription)
%MCRTChildUpdateProcedure%MCRTChildUpdateProcedureParameters
#EMBED(%MCRTAfterInsertLevel,'After Insert Level'),%ActiveTemplateInstance,%Secondary,MAP(%ActiveTemplateInstance,%ActiveTemplateInstanceDescription)
      #ENDIF
    #ENDIF
#?IF GlobalResponse = RequestCompleted
  #?%mInstancePrefix:NewItemLevel    = %(%mInstancePrefix & ':F_' & UPPER(%mFileTreeFile))
  #!#?%mInstancePrefix:NewItemPosition = POSITION(%mFileTreeKey)
  #?%mInstancePrefix:NewItemPosition = POSITION(%mTreeQueue.m_Key)
  #?DO %mInstancePrefix:ReloadTree
#?END
  #RESUME
#!-----------------------------------------------------------------------------
#!
#!
#!-----------------------------------------------------------------------------
#GROUP(%LoadColorsFromIniFile)
! Change colors from ini-file
%mTreeQueue.m_NormalFG   = GETINI(%(%ConvertData(%mInstancePrefix & ':Parameters')),'ColorNFG_'  & %mTreeQueue.m_Instance,,%(%StripPling(%INIFileName)))
%mTreeQueue.m_NormalBG   = GETINI(%(%ConvertData(%mInstancePrefix & ':Parameters')),'ColorNBG_'  & %mTreeQueue.m_Instance,,%(%StripPling(%INIFileName)))
%mTreeQueue.m_SelectedFG = GETINI(%(%ConvertData(%mInstancePrefix & ':Parameters')),'ColorSFG_'  & %mTreeQueue.m_Instance,,%(%StripPling(%INIFileName)))
%mTreeQueue.m_SelectedBG = GETINI(%(%ConvertData(%mInstancePrefix & ':Parameters')),'ColorSBG_'  & %mTreeQueue.m_Instance,,%(%StripPling(%INIFileName)))
#!-----------------------------------------------------------------------------
#!
#!
#!-----------------------------------------------------------------------------
#GROUP(%LoadStyleFromIniFile)
! Change styles from ini-file
rou_FontName  = GETINI(%(%ConvertData(%mInstancePrefix & ':Parameters')),'FontName_'  & %mTreeQueue.m_Instance,,%(%StripPling(%INIFileName)))
rou_FontSize  = GETINI(%(%ConvertData(%mInstancePrefix & ':Parameters')),'FontSize_'  & %mTreeQueue.m_Instance,,%(%StripPling(%INIFileName)))
rou_FontStyle = GETINI(%(%ConvertData(%mInstancePrefix & ':Parameters')),'FontStyle_' & %mTreeQueue.m_Instance,,%(%StripPling(%INIFileName)))
rou_TextColor = GETINI(%(%ConvertData(%mInstancePrefix & ':Parameters')),'ColorNFG_' & %mTreeQueue.m_Instance,,%(%StripPling(%INIFileName)))
rou_BackColor = GETINI(%(%ConvertData(%mInstancePrefix & ':Parameters')),'ColorNBG_' & %mTreeQueue.m_Instance,,%(%StripPling(%INIFileName)))
rou_TextSelected = GETINI(%(%ConvertData(%mInstancePrefix & ':Parameters')),'ColorSFG_' & %mTreeQueue.m_Instance,,%(%StripPling(%INIFileName)))
rou_BackSelected = GETINI(%(%ConvertData(%mInstancePrefix & ':Parameters')),'ColorSBG_' & %mTreeQueue.m_Instance,,%(%StripPling(%INIFileName)))
IF rou_FontName
  %mTreeControl{PROPSTYLE:FontName,%mTreeQueue.m_Instance}  = rou_FontName
END
IF rou_FontSize
  %mTreeControl{PROPSTYLE:FontSize,%mTreeQueue.m_Instance}  = rou_FontSize
END
IF rou_FontStyle
  %mTreeControl{PROPSTYLE:FontStyle,%mTreeQueue.m_Instance} = rou_FontStyle
END
IF rou_TextColor
  %mTreeControl{PROPSTYLE:TextColor,%mTreeQueue.m_Instance} = rou_TextColor
END
IF rou_BackColor
  %mTreeControl{PROPSTYLE:BackColor,%mTreeQueue.m_Instance} = rou_BackColor
END
IF rou_TextSelected
  %mTreeControl{PROPSTYLE:TextSelected,%mTreeQueue.m_Instance} = rou_TextSelected
END
IF rou_BackSelected
  %mTreeControl{PROPSTYLE:BackSelected,%mTreeQueue.m_Instance} = rou_BackSelected
END
%mTreeQueue.m_Style = %mTreeQueue.m_Instance
#!-----------------------------------------------------------------------------
#!
#!
#!-----------------------------------------------------------------------------
#GROUP(%FormatLevelRoutineData)
  #IF(%MCRTRootUseSetupCapability)
  DATA

    #SET(%mTempVariable,'rou_Prefix')
%[35]mTempVariable STRING(40)
    #IF(%ControlHasStyle)
      #SET(%mTempVariable,'rou_FontName')
%[35]mTempVariable STRING(40)
      #SET(%mTempVariable,'rou_FontSize')
%[35]mTempVariable LONG,AUTO
      #SET(%mTempVariable,'rou_FontStyle')
%[35]mTempVariable LONG,AUTO
      #SET(%mTempVariable,'rou_TextColor')
%[35]mTempVariable LONG,AUTO
      #SET(%mTempVariable,'rou_BackColor')
%[35]mTempVariable LONG,AUTO
      #SET(%mTempVariable,'rou_TextSelected')
%[35]mTempVariable LONG,AUTO
      #SET(%mTempVariable,'rou_BackSelected')
%[35]mTempVariable LONG,AUTO
    #ENDIF

  CODE
  #ENDIF
#!-----------------------------------------------------------------------------
#!=============================================================================
#! *** END OF GROUPS ***
#!=============================================================================
#INCLUDE('SVFnGrp.TPW')
