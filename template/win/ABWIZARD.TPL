#TEMPLATE(ABCWIZARDS,'Clarion ABC Wizard Templates'),FAMILY('ABC')
#!
#HELP('ClarionHelp.chm')
#GROUP(%TemplateSetPrompts)
#PROMPT('UtilityTemplateSet',@S100),%UtilityTemplateSet,DEFAULT('ABC_Wizards')
#GROUP(%DeclareTemplateSet)
#SET(%UtilityTemplateSet,'ABC_Wizards')
#!
#GROUP(%DeclareSymbols)
#!
#!
#DECLARE(%ProgramType)
#SET(%ProgramType,'Simple')                 #!Mutually exclusive options
#!SET(%ProgramType,'Full')
#!
#DECLARE(%EditInPlaceString)                #!Dictionary user option constants - Should be case INSENSITIVE
#SET(%EditInPlaceString,'EDITINPLACE')
#!
#DECLARE(%BrowseRoutine)                    #!Used to construct the browse routine name to call for a given tab
#!
#EQUATE(%GenerationReporting,%False)
#EQUATE(%GenerateAllReports,%True)

#DECLARE(%ReplaceExistingProcedures)
#DECLARE(%TXAFile)
#DECLARE(%Comments)
#DECLARE(%CurrentProcedure)
#DECLARE(%CurrentTab)
#DECLARE(%InitializeTab)
#!
#DECLARE(%WindowWidth,LONG)
#DECLARE(%WindowHeight,LONG)
#DECLARE(%WindowButtonRows,LONG)
#DECLARE(%WindowButtonColumns,LONG)
#!
#DECLARE(%WorksheetWidth,LONG)
#DECLARE(%WorksheetHeight,LONG)
#DECLARE(%WorksheetButtonRows,LONG)
#DECLARE(%WorksheetButtonColumns,LONG)
#!
#DECLARE(%MaximumTopOffset,LONG)
#DECLARE(%CurrentX,LONG)
#DECLARE(%CurrentY,LONG)
#DECLARE(%CurrentLineLength,LONG)
#DECLARE(%ValueConstruct)
#DECLARE(%HoldConstruct)
#DECLARE(%TestHeight,LONG)
#DECLARE(%TestWidth,LONG)
#DECLARE(%TestControlType)
#DECLARE(%PictureType)
#DECLARE(%RelationRows,LONG)
#DECLARE(%RelationCols,LONG)
#DECLARE(%TabLineCount,LONG)
#DECLARE(%CheckIndentWidth,LONG)
#DECLARE(%CurrentTabLength,LONG)
#DECLARE(%CurrentTabLineLength,LONG)
#DECLARE(%ControlPlacementTries)
#DECLARE(%VerticalSpaceNextControl)
#DECLARE(%GenerateProgramSection)
#DECLARE(%GenerateInternet)
#DECLARE(%ProcCount)
#DECLARE(%ContCounter)
#DECLARE(%DecimalPortion)
#!
#DECLARE(%RelationList),MULTI
#DECLARE(%BrowseFiles),UNIQUE
#DECLARE(%ChildBrowseFiles),UNIQUE
#DECLARE(%SelectFiles),UNIQUE
#DECLARE(%UpdateFiles),UNIQUE
#!
#DECLARE(%SQLUsed)
#DECLARE(%SQLDrivers),UNIQUE
#DECLARE(%SQLFile,%SQLDrivers)
#!
#DECLARE(%ProcedureList),MULTI
#DECLARE(%QuickProcedureType,%ProcedureList)
#DECLARE(%QuickProcedurePrimary,%ProcedureList)
#DECLARE(%QuickProcedurePrimaryKey,%ProcedureList)
#DECLARE(%QuickProcedureDescription,%ProcedureList)
#DECLARE(%QuickProcedureDefault,%ProcedureList)
#DECLARE(%QuickProcedureWindow,%ProcedureList)
#DECLARE(%QuickProcedureReport,%ProcedureList)
#DECLARE(%QuickProcedureReportColumns,%ProcedureList)
#DECLARE(%QuickProcedureReportColumnFields),MULTI
#DECLARE(%QuickProcedureReportColumnField,%QuickProcedureReportColumnFields)
#DECLARE(%QuickProcedureReportColumnFieldHeader,%QuickProcedureReportColumnFields)
#DECLARE(%QuickProcedureReportColumnFieldPicture,%QuickProcedureReportColumnFields)
#DECLARE(%QuickProcedureReportColumnFieldJust,%QuickProcedureReportColumnFields)
#!
#DECLARE(%FilledEmbed,%ProcedureList),UNIQUE
#DECLARE(%FilledEmbedContents,%FilledEmbed),MULTI
#!
#DECLARE(%QuickOtherFiles,%ProcedureList),UNIQUE
#!
#DECLARE(%WindowLayerID,LONG),UNIQUE
#DECLARE(%WindowLayerTabText,%WindowLayerID)
#DECLARE(%WindowLayerTabREQ,%WindowLayerID)
#DECLARE(%WindowLayerTopOffset,%WindowLayerID)
#DECLARE(%WindowLayerBottomOffset,%WindowLayerID)
#DECLARE(%WindowLayerSpecialCase,%WindowLayerID)
#DECLARE(%WindowLayerCheckIndentLevel,%WindowLayerID)
#DECLARE(%WindowLayerClosed,%WindowLayerID)
#!
#DECLARE(%WindowControlID,%WindowLayerID,LONG),MULTI
#DECLARE(%WindowControl,%WindowControlID)
#DECLARE(%WindowControlXOffset,%WindowControlID)
#DECLARE(%WindowControlYOffset,%WindowControlID)
#DECLARE(%WindowControlWidth,%WindowControlID)
#DECLARE(%WindowControlHeight,%WindowControlID)
#DECLARE(%WindowControlStatement,%WindowControlID)
#!
#DECLARE(%ProcedureDataItemID,LONG),MULTI
#DECLARE(%ProcedureDataItem,%ProcedureDataItemID)
#DECLARE(%ProceduredataItemType,%ProcedureDataItemID)
#!
#DECLARE(%QuickTemplateInstance,LONG),MULTI
#DECLARE(%QuickTemplateType,%QuickTemplateInstance)
#DECLARE(%QuickTemplateParent,%QuickTemplateInstance)
#DECLARE(%QuickTemplatePrimary,%QuickTemplateInstance)
#DECLARE(%QuickTemplatePrimaryKey,%QuickTemplateInstance)
#DECLARE(%QuickTemplateProcProp,%QuickTemplateInstance)
#DECLARE(%QuickTemplateControl,%QuickTemplateInstance),MULTI
#DECLARE(%QuickTemplateSecondaryFiles,%QuickTemplateInstance),UNIQUE
#DECLARE(%LocalQuickTemplateInstance)
#DECLARE(%QuickReportDateInstance,LONG)
#DECLARE(%QuickReportTimeInstance,LONG)
#!
#DECLARE(%FileList),MULTI
#DECLARE(%FileListPromptWidth,%FileList)
#DECLARE(%FileListControlsWidth,%FileList)
#DECLARE(%FileListDescription,%FileList)
#DECLARE(%FileListSQL,%FileList)
#DECLARE(%FileListTab,%FileList),MULTI
#DECLARE(%FileListTabHasPrompts,%FileListTab)
#!
#DECLARE(%FieldListID,%FileList,LONG),UNIQUE
#DECLARE(%FieldList,%FieldListID)
#DECLARE(%FieldListType,%FieldListID)
#DECLARE(%FieldListJustification,%FieldListID)
#DECLARE(%FieldListLookup,%FieldListID)
#DECLARE(%FieldListLookupKey,%FieldListID)
#DECLARE(%FieldListLookupField,%FieldListID)
#DECLARE(%FieldListVerticalSpace,%FieldListID)
#DECLARE(%FieldListControl,%FieldListID),MULTI
#DECLARE(%FieldListControlType,%FieldListControl)
#DECLARE(%FieldListControlXOffset,%FieldListControl)
#DECLARE(%FieldListControlYOffset,%FieldListControl)
#DECLARE(%FieldListControlWidth,%FieldListControl)
#DECLARE(%FieldListControlHeight,%FieldListControl)
#DECLARE(%FieldListControlOrig,%FieldListControl)
#DECLARE(%FieldListControlLink,%FieldListControl)
#DECLARE(%FieldListControlEntryMode,%FieldListControl)
#DECLARE(%FieldListCheckColumn,%FieldListID,LONG)
#DECLARE(%FieldListLinkList,%FieldListID),MULTI
#DECLARE(%FieldListTab,%FieldListID)
#DECLARE(%FieldListHeader,%FieldListID)
#DECLARE(%FieldListPicture,%FieldListID)
#DECLARE(%FieldListPromptWidth,%FieldListID)
#DECLARE(%FieldListBrowseWidth,%FieldListID)
#DECLARE(%FieldListBrowseHeaderWidth,%FieldListID)
#DECLARE(%FieldListBrowseHeaderJust,%FieldListID)
#DECLARE(%FieldListBrowseHeaderIndent,%FieldListID)
#DECLARE(%FieldListBrowseColumnJust,%FieldListID)
#DECLARE(%FieldListBrowseColumnIndent,%FieldListID)
#DECLARE(%FieldListTotalWidth,%FieldListID)
#DECLARE(%FieldListTotalHeight,%FieldListID)
#DECLARE(%FieldListRequiresEnd,%FieldListID)
#DECLARE(%FieldListReportControl,%FieldListID),MULTI
#DECLARE(%FieldListReportControlType,%FieldListReportControl)
#DECLARE(%FieldListReportControlXOffset,%FieldListReportControl)
#DECLARE(%FieldListReportControlYOffset,%FieldListReportControl)
#DECLARE(%FieldListReportControlWidth,%FieldListReportControl)
#DECLARE(%FieldListReportControlHeight,%FieldListReportControl)
#!
#DECLARE(%KeyListID,%FileList,LONG),UNIQUE
#DECLARE(%KeyList,%KeyListID)
#DECLARE(%KeyListTab,%KeyListID)
#DECLARE(%KeyListNoPopulate,%KeyListID)
#DECLARE(%KeyListDescription,%KeyListID)
#DECLARE(%KeyListField,%KeyListID),MULTI
#!
#DECLARE(%TempProcedureName)
#!
#!INSERT(%QuickReportDeclarations)
#INSERT(%QuickBrowseDeclarations)
#INSERT(%QuickFormDeclarations)
#INSERT(%QuickAppDeclarations)
#INSERT(%QuickDeclareAdditions)
#INSERT(%QuickDeclareFields)
#INSERT(%QuickDeclareProcedure)
#INSERT(%QuickDeclareDefaults)
#!--------------------------------------------------------------------------
#!GROUP(%QuickReportDeclarations)
#!DECLARE(%QuickReportColumnFields),MULTI
#!DECLARE(%QuickReportColumnField,%QuickReportColumnFields)
#!DECLARE(%QuickReportColumnFieldHeader,%QuickReportColumnFields)
#!DECLARE(%QuickReportColumnFieldPicture,%QuickReportColumnFields)
#!DECLARE(%QuickReportColumnFieldJust,%QuickReportColumnFields)
#!
#GROUP(%QuickDeclareDefaults)
#DECLARE(%HighestUserLevel)
#DECLARE(%Menubar)
#DECLARE(%LayerWindow)
#DECLARE(%LayerPreSheet)
#DECLARE(%LayerPostSheet)
#DECLARE(%LayerFirstChildBrowse)
#DECLARE(%MaximumSheetHeight,LONG)
#DECLARE(%MaximumSheetWidth,LONG)
#DECLARE(%MinimumSheetWidth,LONG)
#DECLARE(%MinimumTextHeight,LONG)
#DECLARE(%MinimumBrowseHeight,LONG)
#DECLARE(%MaximumBrowseItemWidth,LONG)
#DECLARE(%InitialSheetYOffset,LONG)
#DECLARE(%DefaultXOffset,LONG)
#DECLARE(%DefaultYOffset,LONG)
#DECLARE(%DefaultReportXOffset,LONG)
#DECLARE(%DefaultReportColumns,LONG)
#DECLARE(%DefaultReportInitialYOffset,LONG)
#DECLARE(%DefaultReportYOffset,LONG)
#DECLARE(%DefaultReportTextYOffset,LONG)
#DECLARE(%DefaultButtonWidth,LONG)
#DECLARE(%DefaultButtonHeight,LONG)
#DECLARE(%DefaultRadioHeight,LONG)
#DECLARE(%DefaultRadioWidth,LONG)
#DECLARE(%DefaultCheckHeight,LONG)
#DECLARE(%DefaultCheckBoxXOffset,LONG)
#DECLARE(%DefaultPromptWidth,LONG)
#DECLARE(%DefaultPromptHeight,LONG)
#DECLARE(%DefaultControlHeight,LONG)
#DECLARE(%DefaultControlWidth,LONG)
#DECLARE(%MinimumControlWidth,LONG)
#DECLARE(%DefaultPromptYOffset,LONG)
#DECLARE(%DefaultListItemHeight,LONG)
#DECLARE(%MaximumRelationCols,LONG)
#DECLARE(%MinimumTabLength,LONG)
#DECLARE(%TabTextOffset,LONG)
#DECLARE(%TabLineVerticalOffset,LONG)
#DECLARE(%TabDefaultVerticalOffset,LONG)
#DECLARE(%DefaultSpinWidthOffset,LONG)
#DECLARE(%MaximumCheckColumns,LONG)
#DECLARE(%CheckColumnWidth,LONG)
#DECLARE(%MaximumBrowseFields,LONG)
#DECLARE(%RadioInitialYOffset,LONG)
#DECLARE(%DefaultRadioXOffset,LONG)
#DECLARE(%DefaultRadioYOffset,LONG)
#DECLARE(%TopControlOffset,LONG)
#DECLARE(%MiddleControlOffset,LONG)
#DECLARE(%BottomControlOffset,LONG)
#DECLARE(%TextControlOffset,LONG)
#DECLARE(%DefaultSelectControlText)
#DECLARE(%DefaultInsertControlText)
#DECLARE(%DefaultChangeControlText)
#DECLARE(%DefaultDeleteControlText)
#DECLARE(%DefaultCancelControlText)
#DECLARE(%DefaultCloseControlText)
#DECLARE(%DefaultHelpControlText)
#DECLARE(%DefaultOKControlText)
#DECLARE(%DefaultControlSetings,'')
#EQUATE(%DataOptimize,'ISAM')
#!--------------------------------------------------------------------------
#GROUP(%BeginGeneration)
#SET(%TXAFile,%Application & '.txa')
#CREATE(%TXAFile)
#!--------------------------------------------------------------------------
#GROUP(%GenerateProcedures)
#SET(%ProcCount,0)
#MESSAGE('Wizard Code Generation Progress',0)
#MESSAGE('Beginning Generation',1)
#INSERT(%BeginGeneration)
#IF(%GenerateProgramSection)
[APPLICATION]
#IF(%FirstProcedure)
PROCEDURE %FirstProcedure
#ELSE
PROCEDURE Main
#ENDIF
#IF(%DictionaryFile)
DICTIONARY '%DictionaryFile'
#ENDIF
[COMMON]
FROM ABC
[PROMPTS]
  #FOR(%File)
    #FIX(%Driver,%FileDriver)
    #IF(NOT(UPPER(%Driver) = UPPER(%FileDriver)))
      #ERROR('Required driver not registered, please register '&%FileDriver&' File:'&%File)
      #ABORT
    #ENDIF
    #IF(%DriverSQL)
      #SET(%DataOptimize,'SQL')
      #BREAK
    #END
  #ENDFOR
  #MESSAGE('Optimizing code for '&%DataOptimize&'.',2)
  #CASE(%DataOptimize)
  #OF('ISAM')
%%ActiveInvisible  DEFAULT (0)
%%AllowUnFilled    DEFAULT (0)
%%RetainRow        DEFAULT (1)
%%ResetOnGainFocus DEFAULT (0)
%%AutoToolBar      DEFAULT (1)
%%AutoRefresh      DEFAULT (1)
  #OF('SQL')
%%ActiveInvisible  DEFAULT (0)
%%AllowUnFilled    DEFAULT (0)
%%RetainRow        DEFAULT (0)
%%ResetOnGainFocus DEFAULT (0)
%%AutoToolBar      DEFAULT (1)
%%AutoRefresh      DEFAULT (1)
  #ENDCASE
#ENDIF
  #IF (%GenerateInternet)
[ADDITION]
NAME Web Web
[INSTANCE]
INSTANCE 1
[PROJECT]
-- Generator
%#noedit
%#system win32
%#model clarion dll
  #ENDIF
#FOR(%ProcedureList)
  #SET(%ProcCount,%ProcCount + 1)
  #MESSAGE('Generating Procedure ' & %ProcCount & ':  ' & %ProcedureList,1)
  #MESSAGE('Generating Symbols',2)
  #INSERT(%ResetGlobalSymbols)
  #IF(%QuickProcedurePrimary)
    #FIX(%FileList,%QuickProcedurePrimary)
    #IF(NOT %FileList)
      #ADD(%FileList,%QuickProcedurePrimary)
      #INSERT(%AccumulateFieldList,%FileList)
    #ENDIF
  #ENDIF
  #SET(%CurrentProcedure,%ProcedureList)
  #MESSAGE('Constructing Procedure',2)
  #CASE(%QuickProcedureType)
  #OF('ABC Browse')
    #INSERT(%ConstructQuickBrowse)
  #OF('ABC Form')
    #INSERT(%ConstructQuickForm)
  #OF('ABC Frame')
    #INSERT(%ConstructQuickApp)
  #OF('ABC Window')
    #INSERT(%ConstructQuickOtherWindow)
  #OF('ABC Process')
    #!FIX(%FileList,%QuickProcedurePrimary)
    #!INSERT(%GenerateProcedure)
  #OF('ABC Report')
    #IF(~%GenerateAllReports)
      #CYCLE
    #ENDIF
    #IF(%CusReportDateTimePosition='FOOTER' AND NOT %CusReportAddFooter)
       #SET(%CusReportDateTime,%False)
    #ENDIF
    #IF(%CusReportDateTimePosition='HEADER' AND NOT %CusReportAddHeader)
       #SET(%CusReportDateTime,%False)
    #ENDIF
    #IF(%CusReportDateTimePosition='FORM' AND NOT %CusReportAddForm)
       #SET(%CusReportDateTime,%False)
    #ENDIF
    #IF(%CusReportDateTimePosition='HEADERFOOTER' AND NOT (%CusReportAddFooter OR %CusReportAddHeader))
       #SET(%CusReportDateTime,%False)
    #ENDIF
    #IF(%CusReportDateTime)
       #SET(%QuickReportDateInstance,ITEMS(%QuickTemplateInstance)+1)
       #ADD(%QuickTemplateInstance,%QuickReportDateInstance)
       #SET(%QuickTemplateType,'ReportDate')
       #SET(%QuickTemplateProcProp,%False)
       #SET(%QuickReportTimeInstance,ITEMS(%QuickTemplateInstance)+1)
       #ADD(%QuickTemplateInstance,%QuickReportTimeInstance)
       #SET(%QuickTemplateType,'ReportTime')
       #SET(%QuickTemplateProcProp,%False)
    #ENDIF
  #ELSE
    #CYCLE
  #ENDCASE
  #FIX(%FileList,%QuickProcedurePrimary)
#INSERT(%GenerateProcedure)
#ENDFOR
#PURGE(%RelationList)
#PURGE(%BrowseFiles)
#PURGE(%ChildBrowseFiles)
#PURGE(%SelectFiles)
#PURGE(%UpdateFiles)
#PURGE(%ProcedureList)
#MESSAGE('Importing...',1)
#INSERT(%CloseAndImport)
#!--------------------------------------------------------------------------
#GROUP(%ResetGlobalSymbols)
#INSERT(%SetGlobalDefaults)
#SET(%HighestUserLevel,9999)
#SET(%Menubar,10000)
#SET(%LayerWindow,10001)
#SET(%LayerPostSheet,10002)
#SET(%LayerFirstChildBrowse,9950)
#SET(%TopControlOffset,0)
#SET(%MiddleControlOffset,10000)
#SET(%BottomControlOffset,20000)
#SET(%TextControlOffset,30000)
#FREE(%QuickTemplateInstance)
#FREE(%ProcedureDataItemID)
#FREE(%WindowLayerID)
#FREE(%RelationList)
#!--------------------------------------------------------------------------
#GROUP(%GenerateProcedure)
#IF(%QuickProcedureType='ABC Form' AND (%QuickProcedure <> %ProcedureList))
  #FIX(%File,%QuickProcedurePrimary)
  #IF(INSTRING(%EditInPlaceString,UPPER(%FileUserOptions),1,1))
    #RETURN                                                             #!Don't generate
  #ENDIF
#ENDIF
#MESSAGE('',3)
#MESSAGE('Generating Procedure Header',2)
#INSERT(%GenerateProcedureHeader)
#MESSAGE('Generating File List',2)
#INSERT(%GenerateFileList)
#MESSAGE('Generating Procedure Prompts',2)
#INSERT(%GenerateProcedurePrompts)
#IF(%QuickProcedureType='ABC Report')
#INSERT(%GenerateReportPrompts)
    #IF(%QuickReportPromptSortType='Runtime Key Selection')
#INSERT(%GenerateRuntimeSortList)
    #ENDIF
#ENDIF
#MESSAGE('Generating Field Prompts',2)
#INSERT(%GenerateFieldPrompts)
#MESSAGE('Generating Extensions',2)
#INSERT(%GenerateAdditionPrompts)
#MESSAGE('Generating Embeds',2)
#INSERT(%GenerateEmbeds)
#IF(%QuickProcedureType='ABC Frame' AND %SQLUsed)
[EMBED]
EMBED %%WindowManagerMethodCodeSection
[INSTANCES]
WHEN 'Init'
[INSTANCES]
WHEN '(),BYTE'
[DEFINITION]
[SOURCE]
PROPERTY:BEGIN
PRIORITY 6400
PROPERTY:END
SETCURSOR(Cursor:Wait)
OPEN(SQLOpenWindow)
ACCEPT
  IF EVENT() = Event:OpenWindow
[SOURCE]
PROPERTY:BEGIN
PRIORITY 7800
PROPERTY:END
    POST(EVENT:CloseWindow)
  END
END
CLOSE(SQLOpenWindow)
SETCURSOR()
[END]
[END]
[END]
[END]
#ENDIF
#IF(%QuickProcedureWindow)
#MESSAGE('Generating Window',2)
#INSERT(%GenerateWindow)
#ENDIF
#IF(%QuickProcedureReport)
   #IF(%QuickProcedureType='ABC Process')
#MESSAGE('Generating Process',2)
#INSERT(%GenerateProcess)
   #ELSE
#MESSAGE('Generating Report',2)
#INSERT(%GenerateReport)
   #ENDIF
#ENDIF
#MESSAGE('',2)
#!--------------------------------------------------------------------------
#GROUP(%CloseAndImport)
#CLOSE(%TXAFile)
#!CLOSE(%Comments)
#IMPORT(%TXAFile),REPLACE
#!--------------------------------------------------------------------------
#GROUP(%GenerateRuntimeSortList),PRESERVE,AUTO
#DECLARE(%Counter)
#DECLARE(%SCounter)
#SET(%Counter,0)
#SET(%SCounter,'')
#IF(%QuickProcedurePrimary)
  #FIX(%File,%QuickProcedurePrimary)
  #IF(%File)
    #IF(ITEMS(%Key))
%%AdditionalSortType DEFAULT  ('DynamicNamed')
          #SET(%Counter,0)
          #SET(%SCounter,'')
          #FOR(%Key)
             #SET(%Counter,%Counter+1)
             #IF(CLIP(%SCounter))
                #SET(%SCounter,CLIP(%SCounter)&','&%Counter)
             #ELSE
                #SET(%SCounter,%Counter)
             #ENDIF
          #ENDFOR
%%AddSortDynamicNamed MULTI LONG  (%SCounter)
%%AddSortFieldDinNameDescription DEPEND %%AddSortDynamicNamed DEFAULT TIMES %(ITEMS(%Key))
       #SET(%Counter,0)
       #FOR(%Key)
          #SET(%Counter,%Counter+1)
          #IF(%KeyDescription)
WHEN  (%Counter) ('%KeyDescription')
          #ELSE
WHEN  (%Counter) ('%KeyID')
          #ENDIF
       #ENDFOR

%%AddSortFieldsDinName DEPEND %%AddSortDynamicNamed MULTI LONG TIMES %(ITEMS(%Key))
       #SET(%Counter,0)
       #FOR(%Key)
          #SET(%SCounter,'')
          #FOR(%KeyField)
             #FOR(%Field)
               #IF(%Field=%KeyField)
                  #IF(CLIP(%SCounter))
                     #SET(%SCounter,CLIP(%SCounter)&','&INSTANCE(%Field))
                  #ELSE
                     #SET(%SCounter,INSTANCE(%Field))
                  #ENDIF
               #ENDIF
             #ENDFOR
          #ENDFOR
          #SET(%Counter,%Counter+1)
WHEN  (%Counter) (%SCounter)
       #ENDFOR

%%AddSortFieldDinName DEPEND %%AddSortFieldsDinName FIELD TIMES %(ITEMS(%Key))
       #SET(%Counter,0)
       #FOR(%Key)
          #SET(%Counter,%Counter+1)
WHEN  (%Counter)TIMES %(ITEMS(%KeyField))
          #FOR(%KeyField)
             #FOR(%Field)
               #IF(%Field=%KeyField)
WHEN  (%(INSTANCE(%Field))) (%KeyField)
               #ENDIF
             #ENDFOR
          #ENDFOR
       #ENDFOR

%%AddSortFieldDinNameOrder DEPEND %%AddSortFieldsDinName DEFAULT TIMES %(ITEMS(%Key))
       #SET(%Counter,0)
       #FOR(%Key)
          #SET(%Counter,%Counter+1)
WHEN  (%Counter)TIMES %(ITEMS(%KeyField))
          #FOR(%KeyField)
             #FOR(%Field)
               #IF(%Field=%KeyField)
                #IF(%KeyFieldSequence='ASCENDING')
WHEN  (%(INSTANCE(%Field))) ('+')
                #ELSE
WHEN  (%(INSTANCE(%Field))) ('-')
                #ENDIF
               #ENDIF
             #ENDFOR
          #ENDFOR
       #ENDFOR
    #ENDIF
  #ENDIF
#ENDIF
#!--------------------------------------------------------------------------
#GROUP(%GenerateFileList)
[FILES]
#IF(%QuickProcedureWindow)
  #FOR(%QuickTemplateInstance),WHERE(%QuickTemplatePrimary)
[PRIMARY]
%QuickTemplatePrimary
[INSTANCE]
%QuickTemplateInstance
    #IF(%QuickTemplatePrimaryKey)
[KEY]
%QuickTemplatePrimaryKey
    #ENDIF
    #IF(ITEMS(%QuickTemplateSecondaryFiles))
[SECONDARY]
      #FOR(%QuickTemplateSecondaryFiles)
%QuickTemplateSecondaryFiles
      #ENDFOR
    #ENDIF
  #ENDFOR
#ELSIF(%QuickProcedureReport)
[PRIMARY]
%QuickProcedurePrimary
[INSTANCE]
0
  #IF(%QuickProcedurePrimaryKey)
[KEY]
%QuickProcedurePrimaryKey
  #ENDIF
#ENDIF
#IF(ITEMS(%QuickOtherFiles))
[OTHERS]
  #FOR(%QuickOtherFiles)
%QuickOtherFiles
  #ENDFOR
#ENDIF
#!--------------------------------------------------------------------------
#GROUP(%GenerateEmbeds)
#IF(ITEMS(%FilledEmbed))
[EMBED]
  #FOR(%FilledEmbed)
EMBED %%%FilledEmbed
[DEFINITION]
[SOURCE]
    #FOR(%FilledEmbedContents)
%FilledEmbedContents
    #ENDFOR
[END]
  #ENDFOR
[END]
#ENDIF
#!--------------------------------------------------------------------------
#GROUP(%GenerateWindow)
#CASE(%QuickProcedureType)
#OF('ABC Frame')
  #CALL(%CustomizationWindow,%DefaultControlSetings,%CusFrameBackGround,%CusFrameBackGroundMode,%CusFrameIcon,''''&%CusFrameWindowFontName&''','&%CusFrameWindowFontSize&','&%CusFrameWindowFontColor&','&%CusFrameWindowFontStyle&','&%CusFrameWindowFontCharSet,%CusFrameSystemMenu,%CusFrameEntryPaterns,%CusFrameWindowPosition)
  #IF(%ControlModel='Button')
[WINDOW]
AppFrame APPLICATION('Application'),AT(,,505,318),STATUS(-1,80,120,45),MAX,RESIZE%DefaultControlSetings
  #ELSE
[ADDITION]
NAME ABC FrameBrowseControl
[INSTANCE]
INSTANCE 1
PROCPROP
[WINDOW]
AppFrame APPLICATION('Application'),AT(,,505,318),STATUS(-1,80,120,45),MAX,RESIZE%DefaultControlSetings
    #SET(%LocalQuickTemplateInstance,ITEMS(%QuickTemplateInstance)+1)
  #ENDIF
#OF('ABC Form')
  #SET(%LocalQuickTemplateInstance,ITEMS(%QuickTemplateInstance)+1)
  #IF(%CusBrowseViewButtonEnable AND %QuickBrowseUpdateProcedure)
[ADDITION]
NAME ABC ViewFormActions
[INSTANCE]
#SET(%ValueConstruct,%LocalQuickTemplateInstance)
#SET(%LocalQuickTemplateInstance,%LocalQuickTemplateInstance+1)
INSTANCE %ValueConstruct
  #ENDIF
  #IF(VAREXISTS(%CusFormWindowEntryPaterns))
  #CALL(%CustomizationWindow,%DefaultControlSetings,%CusFormWindowBackGround,%CusFormWindowBackGroundMode,%CusFormWindowIcon,''''&%CusFormWindowFontName&''','&%CusFormWindowFontSize&','&%CusFormWindowFontColor&','&%CusFormWindowFontStyle&','&%CusFormWindowFontCharSet,%CusFormWindowSystemMenu,%CusFormWindowEntryPaterns,%CusFormWindowPosition)
  #ELSE
  #CALL(%CustomizationWindow,%DefaultControlSetings,%CusFormWindowBackGround,%CusFormWindowBackGroundMode,%CusFormWindowIcon,''''&%CusFormWindowFontName&''','&%CusFormWindowFontSize&','&%CusFormWindowFontColor&','&%CusFormWindowFontStyle&','&%CusFormWindowFontCharSet,%CusFormWindowSystemMenu,%False,%CusFormWindowPosition)
  #ENDIF
  #IF(%ControlModel NOT='Button')
[ADDITION]
NAME ABC FormVCRControls
[INSTANCE]
#SET(%ValueConstruct,%LocalQuickTemplateInstance)
#SET(%LocalQuickTemplateInstance,%LocalQuickTemplateInstance+1)
INSTANCE %ValueConstruct
PARENT 2
  #ENDIF
[ADDITION]
NAME ABC ViewFormActions
[INSTANCE]
#SET(%ValueConstruct,%LocalQuickTemplateInstance)
#SET(%LocalQuickTemplateInstance,%LocalQuickTemplateInstance+1)
INSTANCE %ValueConstruct
[ADDITION]
NAME ABC WindowResize
[INSTANCE]
#SET(%ValueConstruct,%LocalQuickTemplateInstance)
#SET(%LocalQuickTemplateInstance,%LocalQuickTemplateInstance+1)
INSTANCE %ValueConstruct
[PROMPTS]
%%UseAppStrategy    DEFAULT (1)
%%AppStrategy       DEFAULT ('Surface')
%%RestrictMinSize   DEFAULT (1)
%%WindowMinWidth    DEFAULT (0)
%%WindowMaxWidth    DEFAULT (0)
[WINDOW]
#SET(%ValueConstruct,%QuickProcedureDescription)
#CALL(%ReplaceSubString,%ValueConstruct,'%FileName',%QuickProcedurePrimary)
QuickWindow WINDOW('%'ValueConstruct'),AT(,,%WindowWidth,%WindowHeight),RESIZE,IMM,GRAY,MDI,HLP('%ProcedureList')%DefaultControlSetings
#OF('ABC Window')
#SET(%LocalQuickTemplateInstance,ITEMS(%QuickTemplateInstance)+1)
[ADDITION]
NAME ABC WindowResize
[INSTANCE]
#SET(%ValueConstruct,%LocalQuickTemplateInstance)
#SET(%LocalQuickTemplateInstance,%LocalQuickTemplateInstance+1)
INSTANCE %ValueConstruct
[PROMPTS]
%%UseAppStrategy    DEFAULT (1)
%%AppStrategy       DEFAULT ('Surface')
%%RestrictMinSize   DEFAULT (1)
%%WindowMinWidth    DEFAULT (0)
%%WindowMaxWidth    DEFAULT (0)
[WINDOW]
#SET(%ValueConstruct,%CusOtherWindowWindowName)
#CALL(%CustomizationWindow,%DefaultControlSetings,%CusOtherWindowWindowBackGround,%CusOtherWindowWindowBackGroundMode,%CusOtherWindowWindowIcon,''''&%CusOtherWindowWindowFontName&''','&%CusOtherWindowWindowFontSize&','&%CusOtherWindowWindowFontColor&','&%CusOtherWindowWindowFontStyle&','&%CusOtherWindowWindowFontCharSet,%CusOtherWindowWindowSystemMenu,%CusOtherWindowWindowEntryPaterns,%CusOtherWindowWindowPosition)
QuickWindow WINDOW('%'ValueConstruct'),AT(,,%WindowWidth,%WindowHeight),RESIZE,IMM,GRAY,HLP('%ProcedureList')%DefaultControlSetings
#OF('ABC Browse')
  #CALL(%CustomizationWindow,%DefaultControlSetings,%CusBrowseWindowBackGround,%CusBrowseWindowBackGroundMode,%CusBrowseWindowIcon,''''&%CusBrowseWindowFontName&''','&%CusBrowseWindowFontSize&','&%CusBrowseWindowFontColor&','&%CusBrowseWindowFontStyle&','&%CusBrowseWindowFontCharSet,%CusBrowseWindowSystemMenu,%CusBrowseWindowEntryPaterns,%CusBrowseWindowPosition)
#SET(%LocalQuickTemplateInstance,ITEMS(%QuickTemplateInstance)+1)
[ADDITION]
NAME ABC WindowResize
[INSTANCE]
#SET(%ValueConstruct,%LocalQuickTemplateInstance)
#SET(%LocalQuickTemplateInstance,%LocalQuickTemplateInstance+1)
INSTANCE %ValueConstruct
[PROMPTS]
%%UseAppStrategy    DEFAULT (1)
%%AppStrategy       DEFAULT ('Surface')
%%RestrictMinSize   DEFAULT (1)
%%WindowMinWidth    DEFAULT (0)
%%WindowMaxWidth    DEFAULT (0)
[WINDOW]
  #IF(%ControlModel='Toolbar' AND %ProgramType NOT='Full')
    #SET(%WindowHeight,%WindowHeight-18)
  #ENDIF
QuickWindow WINDOW('%'QuickProcedureDescription'),AT(,,%WindowWidth,%WindowHeight),IMM,RESIZE,GRAY,MDI,HLP('%ProcedureList')%DefaultControlSetings
#ELSE
[WINDOW]
QuickWindow WINDOW('%'QuickProcedureDescription'),AT(,,%WindowWidth,%WindowHeight),IMM,GRAY,MDI,HLP('%ProcedureList')%DefaultControlSetings
#ENDCASE
#FIX(%WindowLayerID,%MenuBar)
#SUSPEND
  #?MENUBAR,USE(?Menubar)
    #FOR(%WindowControlID)
       %WindowControlStatement
    #ENDFOR
  #?END
#RESUME
#IF (%QuickProcedureType = 'ABC Frame' AND %ControlModel <> 'Button')
  #CALL(%CustomizationToolbar,%DefaultControlSetings,%CusFrameToolbarBackGround,%CusFrameToolbarBackGroundMode)
  TOOLBAR,AT(0,0,400,%CusFrameToolbarHeight),USE(?Toolbar)%DefaultControlSetings
  #SET(%HoldConstruct,%CusFrameToolbarHeight-%CusFrameToolbarIconHeight)
  #IF(%HoldConstruct>4)
     #SET(%HoldConstruct,%HoldConstruct/2)
  #ELSE
     #SET(%HoldConstruct,2)
  #END
  #IF(%CusFrameToolbarIconFlat)
     #SET(%DefaultControlSetings,',FLAT')
  #ELSE
     #SET(%DefaultControlSetings,'')
  #END
  #IF (%GenerateInternet)
    #SET(%ValueConstruct,4)
    BUTTON,AT(%ValueConstruct,%HoldConstruct,%CusFrameToolbarIconWidth,%CusFrameToolbarIconHeight),TIP('Go to the First Page'),USE(?Toolbar:Top,Toolbar:Top),DISABLE,ICON('VCRFIRST.GIF')%DefaultControlSetings,#SEQ(1),#ORIG(?Toolbar:Top,Toolbar:Top)
    #SET(%ValueConstruct,%ValueConstruct+%CusFrameToolbarIconWidth)
    BUTTON,AT(%ValueConstruct,%HoldConstruct,%CusFrameToolbarIconWidth,%CusFrameToolbarIconHeight),TIP('Go to the Prior Page'),USE(?Toolbar:PageUp,Toolbar:PageUp),DISABLE,ICON('VCRPRIOR.GIF')%DefaultControlSetings,#SEQ(1),#ORIG(?Toolbar:PageUp,Toolbar:PageUp)
    #SET(%ValueConstruct,%ValueConstruct+%CusFrameToolbarIconWidth)
    BUTTON,AT(%ValueConstruct,%HoldConstruct,%CusFrameToolbarIconWidth,%CusFrameToolbarIconHeight),TIP('Go to the Prior Record'),USE(?Toolbar:Up,Toolbar:Up),DISABLE,ICON('VCRUP.GIF')%DefaultControlSetings,#SEQ(1),#ORIG(?Toolbar:Up,Toolbar:Up)
    #SET(%ValueConstruct,%ValueConstruct+%CusFrameToolbarIconWidth)
    BUTTON,AT(%ValueConstruct,%HoldConstruct,%CusFrameToolbarIconWidth,%CusFrameToolbarIconHeight),TIP('Locate record'),USE(?Toolbar:Locate,Toolbar:Locate),DISABLE,ICON('FIND.GIF')%DefaultControlSetings,#SEQ(1),#ORIG(?Toolbar:Locate,Toolbar:Locate)
    #SET(%ValueConstruct,%ValueConstruct+%CusFrameToolbarIconWidth)
    BUTTON,AT(%ValueConstruct,%HoldConstruct,%CusFrameToolbarIconWidth,%CusFrameToolbarIconHeight),TIP('Go to the Next Record'),USE(?Toolbar:Down,Toolbar:Down),DISABLE,ICON('VCRDOWN.GIF')%DefaultControlSetings,#SEQ(1),#ORIG(?Toolbar:Down,Toolbar:Down)
    #SET(%ValueConstruct,%ValueConstruct+%CusFrameToolbarIconWidth)
    BUTTON,AT(%ValueConstruct,%HoldConstruct,%CusFrameToolbarIconWidth,%CusFrameToolbarIconHeight),TIP('Go to the Next Page'),USE(?Toolbar:PageDown,Toolbar:PageDown),DISABLE,ICON('VCRNEXT.GIF')%DefaultControlSetings,#SEQ(1),#ORIG(?Toolbar:PageDown,Toolbar:PageDown)
    #SET(%ValueConstruct,%ValueConstruct+%CusFrameToolbarIconWidth)
    BUTTON,AT(%ValueConstruct,%HoldConstruct,%CusFrameToolbarIconWidth,%CusFrameToolbarIconHeight),TIP('Go to the Last Page'),USE(?Toolbar:Bottom,Toolbar:Bottom),DISABLE,ICON('VCRLAST.GIF')%DefaultControlSetings,#SEQ(1),#ORIG(?Toolbar:Bottom,Toolbar:Bottom)
    #SET(%ValueConstruct,%ValueConstruct+%CusFrameToolbarIconWidth)
    BUTTON,AT(%ValueConstruct,%HoldConstruct,%CusFrameToolbarIconWidth,%CusFrameToolbarIconHeight),TIP('Select This Record'),USE(?Toolbar:Select,Toolbar:Select),DISABLE,ICON('MARK.GIF')%DefaultControlSetings,#SEQ(1),#ORIG(?Toolbar:Select,Toolbar:Select)
    #SET(%ValueConstruct,%ValueConstruct+%CusFrameToolbarIconWidth)
    BUTTON,AT(%ValueConstruct,%HoldConstruct,%CusFrameToolbarIconWidth,%CusFrameToolbarIconHeight),TIP('Insert a New Record'),USE(?Toolbar:Insert,Toolbar:Insert),DISABLE,ICON('INSERT.GIF')%DefaultControlSetings,#SEQ(1),#ORIG(?Toolbar:Insert,Toolbar:Insert)
    #SET(%ValueConstruct,%ValueConstruct+%CusFrameToolbarIconWidth)
    BUTTON,AT(%ValueConstruct,%HoldConstruct,%CusFrameToolbarIconWidth,%CusFrameToolbarIconHeight),TIP('Edit This Record'),USE(?Toolbar:Change,Toolbar:Change),DISABLE,ICON('EDIT.GIF')%DefaultControlSetings,#SEQ(1),#ORIG(?Toolbar:Change,Toolbar:Change)
    #SET(%ValueConstruct,%ValueConstruct+%CusFrameToolbarIconWidth)
    BUTTON,AT(%ValueConstruct,%HoldConstruct,%CusFrameToolbarIconWidth,%CusFrameToolbarIconHeight),TIP('Delete This Record'),USE(?Toolbar:Delete,Toolbar:Delete),DISABLE,ICON('DELETE.GIF')%DefaultControlSetings,#SEQ(1),#ORIG(?Toolbar:Delete,Toolbar:Delete)
    #SET(%ValueConstruct,%ValueConstruct+%CusFrameToolbarIconWidth)
    BUTTON,AT(%ValueConstruct,%HoldConstruct,%CusFrameToolbarIconWidth,%CusFrameToolbarIconHeight),TIP('Previous value'),USE(?Toolbar:History,Toolbar:History),DISABLE,ICON('DITTO.GIF')%DefaultControlSetings,#SEQ(1),#ORIG(?Toolbar:Help,Toolbar:History)
    #SET(%ValueConstruct,%ValueConstruct+%CusFrameToolbarIconWidth)
    BUTTON,AT(%ValueConstruct,%HoldConstruct,%CusFrameToolbarIconWidth,%CusFrameToolbarIconHeight),TIP('Get Help'),USE(?Toolbar:Help,Toolbar:Help),DISABLE,ICON('HELP.GIF')%DefaultControlSetings,#SEQ(1),#ORIG(?Toolbar:Help,Toolbar:Help)
  #ELSE
    #SET(%ValueConstruct,4)
    BUTTON,AT(%ValueConstruct,%HoldConstruct,%CusFrameToolbarIconWidth,%CusFrameToolbarIconHeight),TIP('%'CusFrameToolbarTipTop'),USE(?Toolbar:Top,Toolbar:Top),DISABLE,ICON('%'CusFrameToolbarIconTop')%DefaultControlSetings,#SEQ(1),#ORIG(?Toolbar:Top,Toolbar:Top)
    #SET(%ValueConstruct,%ValueConstruct+%CusFrameToolbarIconWidth)
    BUTTON,AT(%ValueConstruct,%HoldConstruct,%CusFrameToolbarIconWidth,%CusFrameToolbarIconHeight),TIP('%'CusFrameToolbarTipPageUp'),USE(?Toolbar:PageUp,Toolbar:PageUp),DISABLE,ICON('%'CusFrameToolbarIconPageUp')%DefaultControlSetings,#SEQ(1),#ORIG(?Toolbar:PageUp,Toolbar:PageUp)
    #SET(%ValueConstruct,%ValueConstruct+%CusFrameToolbarIconWidth)
    BUTTON,AT(%ValueConstruct,%HoldConstruct,%CusFrameToolbarIconWidth,%CusFrameToolbarIconHeight),TIP('%'CusFrameToolbarTipUp'),USE(?Toolbar:Up,Toolbar:Up),DISABLE,ICON('%'CusFrameToolbarIconUp')%DefaultControlSetings,#SEQ(1),#ORIG(?Toolbar:Up,Toolbar:Up)
    #SET(%ValueConstruct,%ValueConstruct+%CusFrameToolbarIconWidth)
    BUTTON,AT(%ValueConstruct,%HoldConstruct,%CusFrameToolbarIconWidth,%CusFrameToolbarIconHeight),TIP('%'CusFrameToolbarTipLocate'),USE(?Toolbar:Locate,Toolbar:Locate),DISABLE,ICON('%'CusFrameToolbarIconLocate')%DefaultControlSetings,#SEQ(1),#ORIG(?Toolbar:Locate,Toolbar:Locate)
    #SET(%ValueConstruct,%ValueConstruct+%CusFrameToolbarIconWidth)
    BUTTON,AT(%ValueConstruct,%HoldConstruct,%CusFrameToolbarIconWidth,%CusFrameToolbarIconHeight),TIP('%'CusFrameToolbarTipDown'),USE(?Toolbar:Down,Toolbar:Down),DISABLE,ICON('%'CusFrameToolbarIconDown')%DefaultControlSetings,#SEQ(1),#ORIG(?Toolbar:Down,Toolbar:Down)
    #SET(%ValueConstruct,%ValueConstruct+%CusFrameToolbarIconWidth)
    BUTTON,AT(%ValueConstruct,%HoldConstruct,%CusFrameToolbarIconWidth,%CusFrameToolbarIconHeight),TIP('%'CusFrameToolbarTipPageDown'),USE(?Toolbar:PageDown,Toolbar:PageDown),DISABLE,ICON('%'CusFrameToolbarIconPageDown')%DefaultControlSetings,#SEQ(1),#ORIG(?Toolbar:PageDown,Toolbar:PageDown)
    #SET(%ValueConstruct,%ValueConstruct+%CusFrameToolbarIconWidth)
    BUTTON,AT(%ValueConstruct,%HoldConstruct,%CusFrameToolbarIconWidth,%CusFrameToolbarIconHeight),TIP('%'CusFrameToolbarTipBottom'),USE(?Toolbar:Bottom,Toolbar:Bottom),DISABLE,ICON('%'CusFrameToolbarIconBottom')%DefaultControlSetings,#SEQ(1),#ORIG(?Toolbar:Bottom,Toolbar:Bottom)
    #SET(%ValueConstruct,%ValueConstruct+%CusFrameToolbarIconWidth)
    BUTTON,AT(%ValueConstruct,%HoldConstruct,%CusFrameToolbarIconWidth,%CusFrameToolbarIconHeight),TIP('%'CusFrameToolbarTipSelect'),USE(?Toolbar:Select,Toolbar:Select),DISABLE,ICON('%'CusFrameToolbarIconSelect')%DefaultControlSetings,#SEQ(1),#ORIG(?Toolbar:Select,Toolbar:Select)
    #SET(%ValueConstruct,%ValueConstruct+%CusFrameToolbarIconWidth)
    BUTTON,AT(%ValueConstruct,%HoldConstruct,%CusFrameToolbarIconWidth,%CusFrameToolbarIconHeight),TIP('%'CusFrameToolbarTipInsert'),USE(?Toolbar:Insert,Toolbar:Insert),DISABLE,ICON('%'CusFrameToolbarIconInsert')%DefaultControlSetings,#SEQ(1),#ORIG(?Toolbar:Insert,Toolbar:Insert)
    #SET(%ValueConstruct,%ValueConstruct+%CusFrameToolbarIconWidth)
    BUTTON,AT(%ValueConstruct,%HoldConstruct,%CusFrameToolbarIconWidth,%CusFrameToolbarIconHeight),TIP('%'CusFrameToolbarTipChange'),USE(?Toolbar:Change,Toolbar:Change),DISABLE,ICON('%'CusFrameToolbarIconChange')%DefaultControlSetings,#SEQ(1),#ORIG(?Toolbar:Change,Toolbar:Change)
    #SET(%ValueConstruct,%ValueConstruct+%CusFrameToolbarIconWidth)
    BUTTON,AT(%ValueConstruct,%HoldConstruct,%CusFrameToolbarIconWidth,%CusFrameToolbarIconHeight),TIP('%'CusFrameToolbarTipDelete'),USE(?Toolbar:Delete,Toolbar:Delete),DISABLE,ICON('%'CusFrameToolbarIconDelete')%DefaultControlSetings,#SEQ(1),#ORIG(?Toolbar:Delete,Toolbar:Delete)
    #SET(%ValueConstruct,%ValueConstruct+%CusFrameToolbarIconWidth)
    BUTTON,AT(%ValueConstruct,%HoldConstruct,%CusFrameToolbarIconWidth,%CusFrameToolbarIconHeight),TIP('%'CusFrameToolbarTipHistory'),USE(?Toolbar:History,Toolbar:History),DISABLE,ICON('%'CusFrameToolbarIconHistory')%DefaultControlSetings,#SEQ(1),#ORIG(?Toolbar:History,Toolbar:History)
    #SET(%ValueConstruct,%ValueConstruct+%CusFrameToolbarIconWidth)
    BUTTON,AT(%ValueConstruct,%HoldConstruct,%CusFrameToolbarIconWidth,%CusFrameToolbarIconHeight),TIP('%'CusFrameToolbarTipHelp'),USE(?Toolbar:Help,Toolbar:Help),DISABLE,ICON('%'CusFrameToolbarIconHelp')%DefaultControlSetings,#SEQ(1),#ORIG(?Toolbar:Help,Toolbar:Help)
  #ENDIF
  END
#ENDIF
#FIX(%WindowLayerID,%LayerPreSheet)
  #INSERT(%ConstructWindowLayer)
#SUSPEND
  #?SHEET,AT(%DefaultXOffset,%DefaultYOffset,%WorksheetWidth,%WorksheetHeight),USE(?CurrentTab),#ORIG(CurrentTab)
#SET(%HoldConstruct,0)
#FOR(%WindowLayerID),WHERE(%WindowLayerID =< %HighestUserLevel AND %WindowLayerID > %LayerPreSheet)
  #SET(%HoldConstruct,%HoldConstruct+1)
  #IF(%QuickProcedureType='ABC Browse')
    #SET(%ValueConstruct,%CusBrowseTabs)
    #CALL(%ReplaceSubString,%ValueConstruct,'%#',%HoldConstruct)
    #CALL(%ReplaceSubString,%ValueConstruct,'%Key',SUB(%WindowLayerTabText,INSTRING(':',%WindowLayerTabText,1,1)+1,LEN(%WindowLayerTabText)-INSTRING(':',%WindowLayerTabText,1,1)))
  #ELSE
    #SET(%ValueConstruct,%CusFormTabs)
    #CALL(%ReplaceSubString,%ValueConstruct,'%#',%HoldConstruct)
    #CALL(%ReplaceSubString,%ValueConstruct,'%Text',%WindowLayerTabText)
  #ENDIF
    TAB('%ValueConstruct'),USE(?Tab:%WindowLayerID)
      #INSERT(%ConstructWindowLayer)
    END
#ENDFOR
  #?END
#RESUME
#FIX(%WindowLayerID,%LayerPostSheet)
#INSERT(%ConstructWindowLayer)
#FIX(%WindowLayerID,%LayerWindow)
#INSERT(%ConstructWindowLayer)
 END
#!--------------------------------------------------------------------------
#GROUP(%ConstructWindowLayer)
#FOR(%WindowControlID)
  #FIND(%QuickTemplateControl,%WindowControl)
  #IF(%QuickTemplateControl)
 #INSERT(%PopulateControlTemplate)
  #ELSE
    #FIND(%FieldList,%WindowControl)
    #IF(%FieldList)
      #IF(%WindowLayerID = %LayerWindow)
 #INSERT(%PopulateDictionaryControl,%WindowControlXOffset,%WindowControlYOffset,%WindowWidth,%WindowHeight)
      #ELSE
 #INSERT(%PopulateDictionaryControl,%WindowControlXOffset,%WindowControlYOffset,%WorksheetWidth,%WorksheetHeight)
      #ENDIF
    #ELSE
 %WindowControlStatement,USE(%WindowControl),AT(%WindowControlXOffset,%WindowControlYOffset,%WindowControlWidth,%WindowControlHeight)
    #ENDIF
  #ENDIF
#ENDFOR
#!--------------------------------------------------------------------------
#GROUP(%GetColorSuffix),AUTO
#DECLARE(%ColorSuffix)
#SERVICE('ClaTPLS.DLL','GenGetColorSuffix')
#RETURN %ColorSuffix
#!--------------------------------------------------------------------------
#GROUP(%GenerateProcess),AUTO
#SET(%ValueConstruct,%CusProcessWindowName)
#CALL(%ReplaceSubString,%ValueConstruct,'%FileName',%File)
#CALL(%CustomizationWindow,%DefaultControlSetings,%CusProcessWindowBackGround,%CusProcessWindowBackGroundMode,%CusProcessWindowIcon,''''&%CusProcessWindowFontName&''','&%CusProcessWindowFontSize&','&%CusProcessWindowFontColor&','&%CusProcessWindowFontStyle&','&%CusProcessWindowFontCharSet,%False,%False,%CusProcessWindowPosition)
[WINDOW]
ProgressWindow WINDOW('%'ValueConstruct'),|
       AT(,,%CusProcessWindowWidth,%CusProcessWindowHeight),TIMER(1),GRAY,DOUBLE%DefaultControlSetings
       #SET(%DefaultControlSetings,'')
       #IF(%CusProcessProgressVertical)
          #SET(%DefaultControlSetings,CLIP(%DefaultControlSetings)&',VERTICAL')
       #ENDIF
       #IF(%CusProcessProgressSmooth)
          #SET(%DefaultControlSetings,CLIP(%DefaultControlSetings)&',SMOOTH')
       #ENDIF
       PROGRESS,USE(Progress:Thermometer),AT(%CusProcessProgressX,%CusProcessProgressY,%CusProcessProgressWidth,%CusProcessProgressHeight),RANGE(0,100)%DefaultControlSetings,#ORIG(Progress:Thermometer)
       STRING(''),AT(%CusProcessUserStringX,%CusProcessUserStringY,%CusProcessUserStringWidth,%CusProcessUserStringHeight),USE(?Progress:UserString),%CusProcessUserStringJustification,#ORIG(?Progress:UserString)
       STRING(''),AT(%CusProcessPctTextX,%CusProcessPctTextY,%CusProcessPctTextWidth,%CusProcessPctTextHeight),USE(?Progress:PctText),%CusProcessPctTextJustification,#ORIG(?Progress:PctText)
#CALL(%CustomizationButtons,%DefaultCancelControlText,%DefaultControlSetings,%CusProcessCancelButtonText,%CusProcessCancelButtonIcon,%CusProcessCancelButtonKeyCode,%CusProcessCancelButtonFlat,%CusProcessCancelButtonJustification,%CusProcessCancelButtonCursor,%CusProcessCancelButtonMsg,%CusProcessCancelButtonTip,%CusProcessCancelButtonSkip)
#CASE(UPPER(%CusProcessButtonsPos))
#OF('RIGHT')
   #SET(%CusProcessButtonsX,%CusProcessWindowWidth-%CusProcessButtonsWidth-2)
   #SET(%CusProcessButtonsY,%CusProcessWindowHeight-%CusProcessButtonsHeight-2)
#OF('LEFT')
   #SET(%CusProcessButtonsX,2)
   #SET(%CusProcessButtonsY,%CusProcessWindowHeight-%CusProcessButtonsHeight-2)
#OF('CENTER')
   #SET(%CusProcessButtonsX,INT((%CusProcessWindowWidth-%CusProcessButtonsWidth)/2))
   #SET(%CusProcessButtonsY,%CusProcessWindowHeight-%CusProcessButtonsHeight-2)
#ELSE
#ENDCASE
       BUTTON('%DefaultCancelControlText'),AT(%CusProcessButtonsX,%CusProcessButtonsY,%CusProcessButtonsWidth,%CusProcessButtonsHeight),USE(?Progress:Cancel)%DefaultControlSetings,#ORIG(?Progress:Cancel)
     END
#!--------------------------------------------------------------------------
#GROUP(%ConstructQuickOtherWindow)
#MESSAGE('Determining Window Size',3)
#!
#SET(%DefaultButtonWidth,%CusOtherWindowButtonsWidth)
#SET(%DefaultButtonHeight,%CusOtherWindowButtonsHeight)
#!
#SET(%WindowWidth,%CusOtherWindowWindowWidth)
#SET(%WindowHeight,%CusOtherWindowWindowHeight)
#!
#SET(%ValueConstruct,0)
#IF(%CusOtherWindowOkButtonEnable)
   #SET(%ValueConstruct,%ValueConstruct+1)
#ENDIF
#IF(%CusOtherWindowCancelButtonEnable)
   #SET(%ValueConstruct,%ValueConstruct+1)
#ENDIF
#IF(%CusOtherWindowHelpButtonEnable)
   #SET(%ValueConstruct,%ValueConstruct+1)
#ENDIF
#ADD(%WindowLayerID,%LayerWindow)
#SET(%CurrentY,%WindowHeight - (%DefaultYOffset + %DefaultButtonHeight))
#SET(%CurrentX,%DefaultXOffset)
#!
#SET(%CurrentX,%WindowWidth - (%ValueConstruct * (%DefaultXOffset + %DefaultButtonWidth)))
#SET(%CurrentY,%WindowHeight - (%DefaultButtonHeight + %DefaultYOffset))
#!
#IF(%CusOtherWindowOkButtonEnable)
   #INSERT(%QuickSetOtherWindowOkButton)
   #SELECT(%QuickTemplateControl,1)
   #MESSAGE('Populating: ?Ok',3)
   #ADD(%WindowControlID,ITEMS(%WindowControlID) + 1)
   #SET(%WindowControl,%QuickTemplateControl)
   #SET(%WindowControlXOffset,%CurrentX)
   #SET(%WindowControlYOffset,%CurrentY)
   #SET(%WindowControlHeight,%DefaultButtonHeight)
   #SET(%WindowControlWidth,%DefaultButtonWidth)
   #SET(%CurrentX,%CurrentX + %DefaultXOffset + %DefaultButtonWidth)
#ENDIF
#IF(%CusOtherWindowCancelButtonEnable)
   #INSERT(%QuickSetOtherWindowCancelButton)
   #SELECT(%QuickTemplateControl,1)
   #MESSAGE('Populating: ?Cancel',3)
   #ADD(%WindowControlID,ITEMS(%WindowControlID) + 1)
   #SET(%WindowControl,%QuickTemplateControl)
   #SET(%WindowControlXOffset,%CurrentX)
   #SET(%WindowControlYOffset,%CurrentY)
   #SET(%WindowControlHeight,%DefaultButtonHeight)
   #SET(%WindowControlWidth,%DefaultButtonWidth)
   #SET(%CurrentX,%CurrentX + %DefaultXOffset + %DefaultButtonWidth)
#ENDIF
#IF(%CusOtherWindowHelpButtonEnable)
   #INSERT(%QuickSetOtherWindowHelpButton)
   #SELECT(%QuickTemplateControl,1)
   #MESSAGE('Populating: ?Help',3)
   #ADD(%WindowControlID,ITEMS(%WindowControlID) + 1)
   #SET(%WindowControl,%QuickTemplateControl)
   #SET(%WindowControlXOffset,%CurrentX)
   #SET(%WindowControlYOffset,%CurrentY)
   #SET(%WindowControlHeight,%DefaultButtonHeight)
   #SET(%WindowControlWidth,%DefaultButtonWidth)
   #SET(%CurrentX,%CurrentX + %DefaultXOffset + %DefaultButtonWidth)
#ENDIF
#!--------------------------------------------------------------------------
#GROUP(%GenerateReport),AUTO
#DECLARE(%RowCount,LONG)
#DECLARE(%RowCountDetail)
#DECLARE(%ColumnCount,LONG)
#DECLARE(%DetailHeight,LONG)
#DECLARE(%ReportStartX,LONG)
#DECLARE(%ReportHeight,LONG)
#DECLARE(%HeaderHeight,LONG)
#DECLARE(%ColumnWidth,LONG)
#DECLARE(%ControlPositionX,LONG)
#DECLARE(%ControlPositionY,LONG)
#DECLARE(%HeaderStartX,LONG)
#DECLARE(%lPaperType)
#DECLARE(%lPosX,LONG)
#DECLARE(%lPosY,LONG)
#DECLARE(%lPosH,LONG)
#DECLARE(%lPosW,LONG)
#DECLARE(%lATPos)
#DECLARE(%lFont)
#DECLARE(%lColor)
#DECLARE(%HeaderBoxStringH,LONG)
#SET(%HeaderBoxStringH,170)
#FIX(%FileList,%QuickProcedurePrimary)
#FIX(%File,%QuickProcedurePrimary)
#FIX(%Key,%QuickProcedurePrimaryKey)
#SET(%RowCount,0)
#SET(%ColumnCount,0)
#IF(ITEMS(%QuickProcedureReportColumnFields)>0)
   #IF(ITEMS(%QuickProcedureReportColumnFields)<%QuickProcedureReportColumns)
     #SET(%QuickProcedureReportColumns,ITEMS(%QuickProcedureReportColumnFields))
   #ENDIF
   #FOR(%QuickProcedureReportColumnFields)
     #IF(%ColumnCount = 0)
       #SET(%RowCount,%RowCount+1)
     #ENDIF
     #SET(%ColumnCount,%ColumnCount + 1)
     #IF(%ColumnCount = %QuickProcedureReportColumns)
       #SET(%ColumnCount,0)
     #ENDIF
   #ENDFOR
#ELSE
   #FOR(%FieldListID),WHERE(%FieldListType <> 'MEMO')
     #IF(%ColumnCount = 0)
       #SET(%RowCount,%RowCount+1)
     #ENDIF
     #SET(%ColumnCount,%ColumnCount + 1)
     #IF(%ColumnCount = %QuickProcedureReportColumns)
       #SET(%ColumnCount,0)
     #ENDIF
   #ENDFOR
   #IF(%RowCount=0 AND ITEMS(%FieldListID)>0)
      #SET(%QuickProcedureReportColumns,1)
   #ENDIF
#ENDIF
#IF(%RowCount = 1 AND %ColumnCount <> 0)
  #SET(%QuickProcedureReportColumns,%ColumnCount)
#ENDIF
#!SET(%DetailHeight,100 + (180 * %RowCount))
#!SET(%HeaderHeight,%DetailHeight + 260)
#!SET(%ColumnWidth,(6000 - (100 * %QuickProcedureReportColumns)) / %QuickProcedureReportColumns)
#!SET(%HeaderStartX,1000)
#!SET(%ReportStartX,%HeaderHeight + %HeaderStartX)
#!SET(%ReportHeight,9000 - %ReportStartX)
#SET(%ValueConstruct,%CusReportWindowName)
#CALL(%ReplaceSubString,%ValueConstruct,'%FileName',%File)
#CALL(%CustomizationWindow,%DefaultControlSetings,%CusReportWindowBackGround,%CusReportWindowBackGroundMode,%CusReportWindowIcon,''''&%CusReportWindowFontName&''','&%CusReportWindowFontSize&','&%CusReportWindowFontColor&','&%CusReportWindowFontStyle&','&%CusReportWindowFontCharSet,%False,%False,%CusReportWindowPosition)
[WINDOW]
ProgressWindow WINDOW('%'ValueConstruct'),|
       AT(,,%CusReportWindowWidth,%CusReportWindowHeight),TIMER(1),GRAY,DOUBLE%DefaultControlSetings
       #SET(%DefaultControlSetings,'')
       #IF(%CusReportProgressVertical)
          #SET(%DefaultControlSetings,CLIP(%DefaultControlSetings)&',VERTICAL')
       #ENDIF
       #IF(%CusReportProgressSmooth)
          #SET(%DefaultControlSetings,CLIP(%DefaultControlSetings)&',SMOOTH')
       #ENDIF
       PROGRESS,USE(Progress:Thermometer),AT(%CusReportProgressX,%CusReportProgressY,%CusReportProgressWidth,%CusReportProgressHeight),RANGE(0,100)%DefaultControlSetings,#ORIG(Progress:Thermometer)
       STRING(''),AT(%CusReportUserStringX,%CusReportUserStringY,%CusReportUserStringWidth,%CusReportUserStringHeight),USE(?Progress:UserString),%CusReportUserStringJustification,#ORIG(?Progress:UserString)
       STRING(''),AT(%CusReportPctTextX,%CusReportPctTextY,%CusReportPctTextWidth,%CusReportPctTextHeight),USE(?Progress:PctText),%CusReportPctTextJustification,#ORIG(?Progress:PctText)
#CALL(%CustomizationButtons,%DefaultCancelControlText,%DefaultControlSetings,%CusReportCancelButtonText,%CusReportCancelButtonIcon,%CusReportCancelButtonKeyCode,%CusReportCancelButtonFlat,%CusReportCancelButtonJustification,%CusReportCancelButtonCursor,%CusReportCancelButtonMsg,%CusReportCancelButtonTip,%CusReportCancelButtonSkip)
#CASE(UPPER(%CusReportButtonsPos))
#OF('RIGHT')
   #SET(%CusReportButtonsX,%CusReportWindowWidth-%CusReportButtonsWidth-2)
   #SET(%CusReportButtonsY,%CusReportWindowHeight-%CusReportButtonsHeight-2)
#OF('LEFT')
   #SET(%CusReportButtonsX,2)
   #SET(%CusReportButtonsY,%CusReportWindowHeight-%CusReportButtonsHeight-2)
#OF('CENTER')
   #SET(%CusReportButtonsX,INT((%CusReportWindowWidth-%CusReportButtonsWidth)/2))
   #SET(%CusReportButtonsY,%CusReportWindowHeight-%CusReportButtonsHeight-2)
#ELSE
#ENDCASE
       BUTTON('%DefaultCancelControlText'),AT(%CusReportButtonsX,%CusReportButtonsY,%CusReportButtonsWidth,%CusReportButtonsHeight),USE(?Progress:Cancel)%DefaultControlSetings,#ORIG(?Progress:Cancel)
     END
[REPORT]
#SET(%RowCountDetail,1)
#SET(%ColumnCount,0)
#SET(%RowCount,1)
#FIX(%File,%QuickProcedurePrimary)
#IF(ITEMS(%QuickProcedureReportColumnFields)>0)
   #FOR(%QuickProcedureReportColumnFields)
       #SET(%ColumnCount,%ColumnCount+1)
       #IF(%ColumnCount > %QuickProcedureReportColumns)
         #SET(%ColumnCount,1)
         #SET(%RowCount,%RowCount+1)
       #ENDIF
   #ENDFOR
   #SET(%RowCountDetail,%RowCount)
#ELSE
   #FOR(%FieldListID),WHERE(%FieldListType <> 'MEMO')
     #FOR(%FieldListReportControl)
       #SET(%ColumnCount,%ColumnCount+1)
       #IF(%ColumnCount > %QuickProcedureReportColumns)
         #SET(%ColumnCount,1)
         #SET(%RowCount,%RowCount+1)
       #ENDIF
     #ENDFOR
   #ENDFOR
   #SET(%RowCountDetail,%RowCount)
#ENDIF
#IF(%CusReportPaperType='PAPER:USER')
   #SET(%lPaperType,',PAPER(PAPER:USER,'&%CusReportWidth&','&%CusReportHeight&')')
#ELSE
   #SET(%lPaperType,',PAPER('&%CusReportPaperType&')')
#ENDIF
#IF(UPPER(%CusReportOrientation)='LANDSCAPE')
   #SET(%lPaperType,CLIP(%lPaperType)&',LANDSCAPE')
#ENDIF
#SET(%lPosX,%CusReportMarginLeft)
#SET(%lPosY,%CusReportMarginTop)
#SET(%lPosH,%CusReportHeight-%CusReportMarginTop-%CusReportMarginBottom)
#IF(%CusReportAddHeader)
  #IF(%CusReportAddHeaderBox)
   #SET(%lPosY,%lPosY+(%HeaderBoxStringH*(%RowCountDetail-1)))
   #SET(%lPosH,%lPosH-(%HeaderBoxStringH*(%RowCountDetail-1)))
  #ENDIF
#ENDIF
#SET(%lPosW,%CusReportWidth-%CusReportMarginLeft-%CusReportMarginRight)
#IF(%QuickProcedureReportColumns=1)
    #IF(%CusReportDetailPosWidth=0)
        #SET(%ColumnWidth,%lPosW-100)
    #ELSE
        #SET(%ColumnWidth,%CusReportDetailPosWidth-100)
    #ENDIF
    #IF(VAREXISTS(%CusLabelReportWidth) AND VAREXISTS(%CusLabelReportHPitch))
        #SET(%ColumnWidth,%ColumnWidth-(%CusLabelReportHPitch-%CusLabelReportWidth))
    #ENDIF
#ELSE
    #SET(%ColumnWidth,(%lPosW / %QuickProcedureReportColumns)-100)
#ENDIF
#SET(%lFont,',FONT('''&%CusReportFontName&''','&%CusReportFontSize&','&%CusReportFontColor&','&%CusReportFontStyle&','&%CusReportFontCharSet&')')
#SET(%ValueConstruct,%CusReportJobName)
#CALL(%ReplaceSubString,%ValueConstruct,'%FileName',%File)
Report REPORT('%'ValueConstruct'),AT(%lPosX,%lPosY,%lPosW,%lPosH),PRE(RPT)%lPaperType%lFont,THOUS
#!START THE HEADER
#IF(%CusReportAddHeader)
   #SET(%lFont,',FONT('''&%CusReportHeaderFontName&''','&%CusReportHeaderFontSize&','&%CusReportHeaderFontColor&','&%CusReportHeaderFontStyle&','&%CusReportHeaderFontCharSet&')')
   #SET(%lPosX,%CusReportHeaderMarginLeft)
   #SET(%lPosY,%CusReportHeaderMarginTop)
   #SET(%lPosH,%CusReportHeaderPosHeight)
   #IF(%CusReportAddHeaderBox)
       #SET(%lPosH,%lPosH+(%HeaderBoxStringH*(%RowCountDetail-1)))
   #ENDIF
   #SET(%lPosW,%CusReportWidth-%CusReportHeaderMarginLeft-%CusReportHeaderMarginRight)
   #IF(%CusReportHeaderColor<>'COLOR:NONE' AND %CusReportHeaderColor<>-1)
      #SET(%lColor,',COLOR('&%CusReportHeaderColor&')')
   #ELSE
      #SET(%lColor,'')
   #ENDIF
       HEADER,AT(%lPosX,%lPosY,%lPosW,%lPosH),USE(?Header)%lFont%lColor
#!IF(%CusReportAddHeader)
   #IF(%CusReportDateTime AND (%CusReportDateTimePosition='HEADER' OR %CusReportDateTimePosition='HEADERFOOTER'))
         STRING('%'CusReportDateText'),AT(%CusReportDatePromptPosX,%CusReportDatePromptPosY,344,135),USE(?ReportDatePrompt:2),TRN,FONT('Arial',8,,FONT:regular),#SEQ(%QuickReportDateInstance),#ORIG(?ReportDatePrompt)
         STRING('<<-- Date Stamp -->'),AT(%CusReportDatePosX,%CusReportDatePosY,927,135),USE(?ReportDateStamp:2),TRN,FONT('Arial',8,,FONT:regular),#SEQ(%QuickReportDateInstance),#ORIG(?ReportDateStamp)
         STRING('%'CusReportTimeText'),AT(%CusReportTimePromptPosX,%CusReportTimePromptPosY,271,135),USE(?ReportTimePrompt:2),TRN,FONT('Arial',8,,FONT:regular),#SEQ(%QuickReportTimeInstance),#ORIG(?ReportTimePrompt)
         STRING('<<-- Time Stamp -->'),AT(%CusReportTimePosX,%CusReportTimePosY,927,135),USE(?ReportTimeStamp:2),TRN,FONT('Arial',8,,FONT:regular),#SEQ(%QuickReportTimeInstance),#ORIG(?ReportTimeStamp)
   #ENDIF
#!ENDIF
#!IF(%CusReportAddHeader)
   #IF(%CusReportPageNum AND (%CusReportPageNumPosition='HEADER' OR %CusReportPageNumPosition='HEADERFOOTER'))
         STRING(%CusReportPageNumPic),AT(%CusReportPageNumPosX,%CusReportPageNumPosY,700,135),FONT('Arial',8,,FONT:regular),PAGENO,USE(?PageCount:2)
   #ENDIF
#!ENDIF
#SET(%ValueConstruct,%CusReportTitleName)
#!
#CALL(%ReplaceSubString,%ValueConstruct,'%FileName',%File)
   #SET(%lFont,',FONT('''&%CusReportTitleFontName&''','&%CusReportTitleFontSize&','&%CusReportTitleFontColor&','&%CusReportTitleFontStyle&','&%CusReportTitleFontCharSet&')')
         STRING('%'ValueConstruct'),AT(0,%CusReportTitlePosY,%lPosW,220),USE(?ReportTitle),%CusReportTitleJust%lFont
 #IF(%CusReportAddHeaderBox)
   #IF(%CusReportHeaderBoxColor<>'COLOR:NONE' AND %CusReportHeaderBoxColor<>-1)
      #SET(%lColor,',FILL('&%CusReportHeaderBoxColor&')')
   #ELSE
      #SET(%lColor,'')
   #ENDIF
   #SET(%lPosW,%CusReportWidth-%CusReportHeaderMarginLeft-%CusReportHeaderMarginRight)
   #SET(%lPosH,%CusReportHeaderBoxPosHeight+((%HeaderBoxStringH+10)*(%RowCountDetail-1)))
         BOX,AT(0,%CusReportHeaderBoxPosY,%lPosW,%lPosH),COLOR(00H),USE(?HeaderBox)%lColor
#SET(%ValueConstruct,1)
#LOOP
  #IF(%ValueConstruct = %QuickProcedureReportColumns)
    #BREAK
  #ENDIF
  #SET(%ControlPositionX,%ValueConstruct * (100 + %ColumnWidth))
  #SET(%lPosH,%CusReportHeaderBoxPosHeight+((%HeaderBoxStringH+10)*(%RowCountDetail-1)))
         LINE,AT(%ControlPositionX,%CusReportHeaderBoxPosY,0,%lPosH),COLOR(00H),USE(?HeaderLine:%ValueConstruct)
  #SET(%ValueConstruct,%ValueConstruct+1)
#ENDLOOP
 #ELSE
    #SET(%CusReportHeaderBoxPosY,%lPosH-85)
    #SET(%CusReportHeaderBoxPosHeight,0)
 #ENDIF
 #! END IF(%CusReportAddHeaderBox)
#SET(%ValueConstruct,1)
#SET(%ColumnCount,0)
#SET(%RowCount,0)
#FIX(%File,%QuickProcedurePrimary)
#IF(ITEMS(%QuickProcedureReportColumnFields)>0)
   #FOR(%QuickProcedureReportColumnFields)
       #SET(%ColumnCount,%ColumnCount+1)
       #IF(%ColumnCount > %QuickProcedureReportColumns)
         #SET(%ColumnCount,1)
         #SET(%RowCount,%RowCount+1)
       #ENDIF
       #SET(%ControlPositionX,50 + ((%ColumnCount - 1) * (100 + %ColumnWidth)))
       #SET(%ControlPositionY,(%RowCount * (%HeaderBoxStringH+10)) + %CusReportHeaderBoxPosY + (%CusReportHeaderBoxPosHeight/2)-85)
         STRING('%'QuickProcedureReportColumnFieldHeader'),AT(%ControlPositionX,%ControlPositionY,%ColumnWidth,%HeaderBoxStringH),TRN,USE(?HeaderTitle:%ValueConstruct)
       #SET(%ValueConstruct,%ValueConstruct+1)
   #ENDFOR
#ELSE
   #FOR(%FieldListID),WHERE(%FieldListType <> 'MEMO')
     #FOR(%FieldListReportControl)
       #SET(%ColumnCount,%ColumnCount+1)
       #IF(%ColumnCount > %QuickProcedureReportColumns)
         #SET(%ColumnCount,1)
         #SET(%RowCount,%RowCount+1)
       #ENDIF
       #SET(%ControlPositionX,50 + ((%ColumnCount - 1) * (100 + %ColumnWidth)))
       #SET(%ControlPositionY,(%RowCount * (%HeaderBoxStringH+10)) + %CusReportHeaderBoxPosY + (%CusReportHeaderBoxPosHeight/2)-85)
         STRING('%'FieldListHeader'),AT(%ControlPositionX,%ControlPositionY,%ColumnWidth,%HeaderBoxStringH),TRN,USE(?HeaderTitle:%ValueConstruct)
       #SET(%ValueConstruct,%ValueConstruct+1)
     #ENDFOR
   #ENDFOR
#ENDIF
       END
#ENDIF
#!END THE HEADER
#SET(%ColumnCount,0)
#SET(%RowCount,0)
#SET(%lATPos,'AT('&%CusReportDetailPosX&','&%CusReportDetailPosY&',')
#IF(%CusReportDetailPosWidth=0)
#!   #IF(UPPER(%CusReportOrientation)='LANDSCAPE')
#!      #SET(%CusReportDetailPosWidth,%CusReportHeight-%CusReportMarginTop-%CusReportMarginBottom)
#!   #ELSE
      #SET(%CusReportDetailPosWidth,%CusReportWidth-%CusReportMarginLeft-%CusReportMarginRight)
#!   #ENDIF
#ENDIF
#SET(%lATPos,CLIP(%lATPos)&%CusReportDetailPosWidth)
#IF(%CusReportDetailAutoAdjustHeightWithRowCount)
    #SET(%lATPos,CLIP(%lATPos)&','&(%CusReportDetailPosHeight*%RowCountDetail)&')')
#ELSE
    #SET(%lATPos,CLIP(%lATPos)&','&(%CusReportDetailPosHeight)&')')
#ENDIF
   #IF(%CusReportDetailColor<>'COLOR:NONE' AND %CusReportDetailColor<>-1)
      #SET(%lColor,',COLOR('&%CusReportDetailColor&')')
   #ELSE
      #SET(%lColor,'')
   #ENDIF
Detail DETAIL,%lATPos,USE(?Detail)%lColor
#IF(%CusReportAddDetailLines)
#SET(%ValueConstruct,1)
#IF(%CusReportDetailAutoAdjustHeightWithRowCount)
    #SET(%lPosH,(%CusReportDetailPosHeight*%RowCountDetail))
#ELSE
    #SET(%lPosH,%CusReportDetailPosHeight)
#ENDIF
         LINE,AT(0,0,0,%lPosH),COLOR(00H),USE(?DetailLine:0)
#LOOP
  #IF(%ValueConstruct = %QuickProcedureReportColumns)
         LINE,AT(%CusReportDetailPosWidth,0,0,%lPosH),COLOR(00H),USE(?DetailLine:%ValueConstruct)
    #BREAK
  #ENDIF
  #SET(%ControlPositionX,%ValueConstruct * (100 + %ColumnWidth))
         LINE,AT(%ControlPositionX,0,0,%lPosH),COLOR(00H),USE(?DetailLine:%ValueConstruct)
  #SET(%ValueConstruct,%ValueConstruct+1)
#ENDLOOP
#ENDIF
#!
#FIX(%File,%QuickProcedurePrimary)
#IF(ITEMS(%QuickProcedureReportColumnFields)>0)
   #FOR(%QuickProcedureReportColumnFields)
     #FIX(%Field,%QuickProcedureReportColumnField)
     #FOR(%FieldReportControl)
       #CASE(SUB(%FieldReportControl,1,4))
       #OF('TEXT')
         #SET(%ValueConstruct,%FieldReportControl)
       #OF('CHEC')
         #SET(%ValueConstruct,%FieldReportControl)
       #OF('STRI')
         #SET(%ValueConstruct,%FieldReportControl)
       #ELSE
         #SET(%ValueConstruct,'STRING(' & %FieldPicture & '),USE(' & %Field & ')')
       #ENDCASE
       #BREAK
     #ENDFOR
     #SET(%ValueConstruct,REPLACE(%ValueConstruct,'MSG',''))
     #IF(EXTRACT(%ValueConstruct,'STRING')<>'')
        #SET(%ValueConstruct,REPLACE(%ValueConstruct,'STRING',%QuickProcedureReportColumnFieldPicture,1))
        #SET(%ValueConstruct,REPLACE(%ValueConstruct,'LEFT',''))
        #SET(%ValueConstruct,REPLACE(%ValueConstruct,'RIGHT',''))
        #SET(%ValueConstruct,REPLACE(%ValueConstruct,'CENTER',''))
        #SET(%ValueConstruct,REPLACE(%ValueConstruct,'DECIMAL',''))
        #CASE(%QuickProcedureReportColumnFieldJust)
        #OF('L')
           #SET(%ValueConstruct,CLIP(%ValueConstruct)&',LEFT')
        #OF('R')
           #SET(%ValueConstruct,CLIP(%ValueConstruct)&',RIGHT')
        #OF('C')
           #SET(%ValueConstruct,CLIP(%ValueConstruct)&',CENTER')
        #OF('D')
           #SET(%ValueConstruct,CLIP(%ValueConstruct)&',DECIMAL')
        #ENDCASE
     #ENDIF
     #SET(%ColumnCount,%ColumnCount+1)
     #IF(%ColumnCount > %QuickProcedureReportColumns)
       #SET(%ColumnCount,1)
       #SET(%RowCount,%RowCount+1)
     #ENDIF
     #SET(%ControlPositionX,50 + ((%ColumnCount - 1) * (100 + %ColumnWidth)))
     #SET(%ControlPositionY,50 + (%RowCount * 180))
         %ValueConstruct,AT(%ControlPositionX,%ControlPositionY,%ColumnWidth,170)
   #ENDFOR
#ELSE
   #FOR(%FieldListID),WHERE(%FieldListType <> 'MEMO')
     #FOR(%FieldListReportControl)
       #SET(%ValueConstruct,%FieldListReportControl)
       #SET(%ValueConstruct,REPLACE(%ValueConstruct,'MSG',''))
       #SET(%ColumnCount,%ColumnCount+1)
       #IF(%ColumnCount > %QuickProcedureReportColumns)
         #SET(%ColumnCount,1)
         #SET(%RowCount,%RowCount+1)
       #ENDIF
       #SET(%ControlPositionX,50 + ((%ColumnCount - 1) * (100 + %ColumnWidth)))
       #SET(%ControlPositionY,50 + (%RowCount * 180))
         %ValueConstruct,AT(%ControlPositionX,%ControlPositionY,%ColumnWidth,170)
     #ENDFOR
   #ENDFOR
#ENDIF
#SET(%lPosY,(%CusReportDetailPosHeight*%RowCountDetail))
#IF(%CusReportAddDetailLines)
         LINE,AT(0,%lPosY,%CusReportDetailPosWidth,0),COLOR(00H),USE(?DetailEndLine)
#ENDIF
       END
#IF(%CusReportAddFooter)
    #SET(%lPosX,%CusReportFooterMarginLeft)
    #SET(%lPosY,%CusReportHeight-%CusReportFooterPosHeight-%CusReportFooterMarginBottom)
    #SET(%lPosH,%CusReportFooterPosHeight)
    #SET(%lPosW,%CusReportWidth-%CusReportFooterMarginLeft-%CusReportFooterMarginRight)
   #IF(%CusReportFooterColor<>'COLOR:NONE' AND %CusReportFooterColor<>-1)
      #SET(%lColor,',COLOR('&%CusReportFooterColor&')')
   #ELSE
      #SET(%lColor,'')
   #ENDIF
       FOOTER,AT(%lPosX,%lPosY,%lPosW,%lPosH),USE(?Footer)%lColor
#!IF(%CusReportAddFooter)
   #IF(%CusReportDateTime AND (%CusReportDateTimePosition='FOOTER' OR %CusReportDateTimePosition='HEADERFOOTER'))
         STRING('%'CusReportDateText'),AT(%CusReportDatePromptPosX,%CusReportDatePromptPosY,344,135),USE(?ReportDatePrompt:2),TRN,FONT('Arial',8,,FONT:regular),#SEQ(%QuickReportDateInstance),#ORIG(?ReportDatePrompt)
         STRING('<<-- Date Stamp -->'),AT(%CusReportDatePosX,%CusReportDatePosY,927,135),USE(?ReportDateStamp:2),TRN,FONT('Arial',8,,FONT:regular),#SEQ(%QuickReportDateInstance),#ORIG(?ReportDateStamp)
         STRING('%'CusReportTimeText'),AT(%CusReportTimePromptPosX,%CusReportTimePromptPosY,271,135),USE(?ReportTimePrompt:2),TRN,FONT('Arial',8,,FONT:regular),#SEQ(%QuickReportTimeInstance),#ORIG(?ReportTimePrompt)
         STRING('<<-- Time Stamp -->'),AT(%CusReportTimePosX,%CusReportTimePosY,927,135),USE(?ReportTimeStamp:2),TRN,FONT('Arial',8,,FONT:regular),#SEQ(%QuickReportTimeInstance),#ORIG(?ReportTimeStamp)
   #ENDIF
#!ENDIF
#!IF(%CusReportAddFooter)
   #IF(%CusReportPageNum AND (%CusReportPageNumPosition='FOOTER' OR %CusReportPageNumPosition='HEADERFOOTER'))
         STRING(%CusReportPageNumPic),AT(%CusReportPageNumPosX,%CusReportPageNumPosY,700,135),FONT('Arial',8,,FONT:regular),PAGENO,USE(?PageCount:2)
   #ENDIF
#!ENDIF
       END
#ENDIF #!%CusReportAddFooter
#IF(%CusReportAddForm)
  #SET(%lPosX,%CusReportFormMarginLeft)
  #SET(%lPosY,%CusReportFormMarginTop)
  #SET(%lPosH,%CusReportHeight-%CusReportFormMarginTop-%CusReportFormMarginBottom)
  #SET(%lPosW,%CusReportWidth-%CusReportFormMarginLeft-%CusReportFormMarginRight)
  #SET(%lFont,',FONT('''&%CusReportFormFontName&''','&%CusReportFormFontSize&','&%CusReportFormFontColor&','&%CusReportFormFontStyle&','&%CusReportFormFontCharSet&')')
   #IF(%CusReportFormColor<>'COLOR:NONE' AND %CusReportFormColor<>-1)
      #SET(%lColor,',COLOR('&%CusReportFormColor&')')
   #ELSE
      #SET(%lColor,'')
   #ENDIF
       FORM,AT(%lPosX,%lPosY,%lPosW,%lPosH),USE(?Form)%lFont%lColor
  #IF(%CusReportFormAddImage)
     #IF(%CusReportFormImagePosWidth=0)
        #SET(%lPosW,%CusReportWidth-%CusReportFormMarginLeft-%CusReportFormMarginRight)
     #ELSE
        #SET(%lPosW,%CusReportFormImagePosWidth)
     #END
     #IF(%CusReportFormImagePosHeight=0)
        #SET(%lPosH,%CusReportHeight-%CusReportFormMarginTop-%CusReportFormMarginBottom)
     #ELSE
        #SET(%lPosH,%CusReportFormImagePosHeight)
     #END
         IMAGE('%'CusReportFormImage'),AT(%CusReportFormImagePosX,%CusReportFormImagePosY,%lPosW,%lPosH),USE(?FormImage),TILED
  #ENDIF
#!IF(%CusReportAddForm)
   #IF(%CusReportDateTime AND %CusReportDateTimePosition='FORM')
         STRING('%'CusReportDateText'),AT(%CusReportDatePromptPosX,%CusReportDatePromptPosY,344,135),USE(?ReportDatePrompt:2),TRN,FONT('Arial',8,,FONT:regular),#SEQ(%QuickReportDateInstance),#ORIG(?ReportDatePrompt)
         STRING('<<-- Date Stamp -->'),AT(%CusReportDatePosX,%CusReportDatePosY,927,135),USE(?ReportDateStamp:2),TRN,FONT('Arial',8,,FONT:regular),#SEQ(%QuickReportDateInstance),#ORIG(?ReportDateStamp)
         STRING('%'CusReportTimeText'),AT(%CusReportTimePromptPosX,%CusReportTimePromptPosY,271,135),USE(?ReportTimePrompt:2),TRN,FONT('Arial',8,,FONT:regular),#SEQ(%QuickReportTimeInstance),#ORIG(?ReportTimePrompt)
         STRING('<<-- Time Stamp -->'),AT(%CusReportTimePosX,%CusReportTimePosY,927,135),USE(?ReportTimeStamp:2),TRN,FONT('Arial',8,,FONT:regular),#SEQ(%QuickReportTimeInstance),#ORIG(?ReportTimeStamp)
   #ENDIF
#!ENDIF
#!IF(%CusReportAddForm)
   #IF(%CusReportPageNum AND %CusReportPageNumPosition='FORM')
         STRING(%CusReportPageNumPic),AT(%CusReportPageNumPosX,%CusReportPageNumPosY,700,135),FONT('Arial',8,,FONT:regular),PAGENO,USE(?PageCount:2)
   #ENDIF
#!ENDIF
       END
#ENDIF
     END
#!-------------------------------------------------------
#GROUP(%AddPrimaryBrowse,%NewProcedureFile)
#DECLARE(%NewProcedureName)
#FIND(%File,%NewProcedureFile)
#SET(%NewProcedureName,%CusBrowseProcName)
#CALL(%ReplaceSubString,%NewProcedureName,'%FileName',%File)
#IF(NOT %ReplaceExistingProcedures)
  #FIX(%Procedure,%NewProcedureName)
#ENDIF
#IF(NOT %Procedure)
  #FIX(%ProcedureList,%NewProcedureName)
  #IF(NOT %ProcedureList)
    #ADD(%ProcedureList,%NewProcedureName)
    #SET(%QuickProcedureType,'ABC Browse')
    #SET(%QuickProcedurePrimary,%File)
    #SET(%QuickProcedureDescription,%CusBrowseWindowName)
    #CALL(%ReplaceSubString,%QuickProcedureDescription,'%FileName',%File)
    #SET(%QuickProcedureDefault,'Primary Browse')
    #SET(%QuickProcedureWindow,%True)
  #ENDIF
#ENDIF
#!-------------------------------------------------------
#GROUP(%AddSecondaryBrowse,%NewProcedureKey)
#DECLARE(%NewProcedureName)
#FIND(%Key,%NewProcedureKey)
#SET(%NewProcedureName,%CusBrowseProcNameSec)
#CALL(%ReplaceSubString,%NewProcedureName,'%Key',%Key)
#CALL(%ReplaceSubString,%NewProcedureName,'%FileName',%File)
#IF(NOT %ReplaceExistingProcedures)
  #FIX(%Procedure,%NewProcedureName)
#ENDIF
#IF(NOT %Procedure)
  #FIX(%ProcedureList,%NewProcedureName)
  #IF(NOT %ProcedureList)
    #ADD(%ProcedureList,%NewProcedureName)
    #SET(%QuickProcedureType,'ABC Browse')
    #SET(%QuickProcedurePrimary,%File)
    #SET(%QuickProcedurePrimaryKey,%Key)
    #SET(%QuickProcedureDescription,%CusBrowseWindowNameSec)
    #CALL(%ReplaceSubString,%QuickProcedureDescription,'%FileName',%File)
    #CALL(%ReplaceSubString,%QuickProcedureDescription,'%Key',%Key)
    #SET(%QuickProcedureDefault,'Secondary Browse')
    #SET(%QuickProcedureWindow,%True)
  #ENDIF
#ENDIF
#!-------------------------------------------------------
#GROUP(%AddSelectBrowse,%NewProcedureFile)
#DECLARE(%NewProcedureName)
#FIX(%File,%NewProcedureFile)
#SET(%NewProcedureName,%CusBrowseParentProcName)
#CALL(%ReplaceSubString,%NewProcedureName,'%FileName',%NewProcedureFile)
#IF(NOT %ReplaceExistingProcedures)
  #FIX(%Procedure,%NewProcedureName)
#ENDIF
#IF(NOT %Procedure)
  #FIX(%ProcedureList,%NewProcedureName)
  #IF(NOT %ProcedureList)
    #ADD(%ProcedureList,%NewProcedureName)
    #SET(%QuickProcedureType,'ABC Browse')
    #SET(%QuickProcedurePrimary,%File)
    #SET(%QuickProcedureDescription,%CusBrowseParentProcWindowName)
    #CALL(%ReplaceSubString,%QuickProcedureDescription,'%FileName',%File)
    #SET(%QuickProcedureDefault,'Select Browse')
    #SET(%QuickProcedureWindow,%True)
  #ENDIF
#ENDIF
#!-------------------------------------------------------
#GROUP(%AddUpdateProcedure,%NewProcedureFile,%OverrideProcedureName)
#DECLARE(%NewProcedureName)
#IF(NOT %OverrideProcedureName)
  #SET(%NewProcedureName,%CusFormProcName)
  #CALL(%ReplaceSubString,%NewProcedureName,'%FileName',%NewProcedureFile)
#ELSE
  #SET(%NewProcedureName,%OverrideProcedureName)
#ENDIF
#IF(NOT %ReplaceExistingProcedures)
  #FIX(%Procedure,%NewProcedureName)
#ENDIF
#IF(NOT %Procedure)
  #FIX(%ProcedureList,%NewProcedureName)
  #IF(NOT %ProcedureList)
    #ADD(%ProcedureList,%NewProcedureName)
    #SET(%QuickProcedureType,'ABC Form')
    #SET(%QuickProcedurePrimary,%NewProcedureFile)
    #SET(%QuickProcedureDescription,%CusFormWindowName)
    #!CALL(%ReplaceSubString,%QuickProcedureDescription,'%FileName',%File)
    #SET(%QuickProcedureDefault,'Update Form')
    #SET(%QuickProcedureWindow,%True)
  #ENDIF
#ENDIF
#!-------------------------------------------------------
#GROUP(%AddReportProcedure,%NewProcedurePrimary)
#DECLARE(%NewProcedureName)
#FIX(%File,%NewProcedurePrimary)
#IF(%File)
  #SET(%NewProcedureName,%CusReportProcName)
  #CALL(%ReplaceSubString,%NewProcedureName,'%FileName',%File)
#ELSE
  #FIND(%Key,%NewProcedurePrimary)
  #SET(%NewProcedureName,%CusReportProcNameKey)
  #CALL(%ReplaceSubString,%NewProcedureName,'%FileName',%File)
  #CALL(%ReplaceSubString,%NewProcedureName,'%Key',%Key)
#ENDIF
#IF(NOT %ReplaceExistingProcedures)
  #FIX(%Procedure,%NewProcedureName)
#ENDIF
#IF(NOT %Procedure)
  #FIX(%ProcedureList,%NewProcedureName)
  #IF(NOT %ProcedureList)
    #ADD(%ProcedureList,%NewProcedureName)
    #SET(%QuickProcedureType,'ABC Report')
    #SET(%QuickProcedurePrimary,%File)
    #SET(%QuickProcedurePrimaryKey,%Key)
    #IF(%Key)
      #SET(%QuickProcedureDescription,'Print the ' & %File & ' File by ' & %Key)
    #ELSE
      #SET(%QuickProcedureDescription,'Print the ' & %File & ' File')
    #ENDIF
    #SET(%QuickProcedureDefault,'Report')
    #SET(%QuickProcedureReport,%True)
    #SET(%QuickProcedureReportColumns,%DefaultReportColumns)
  #ENDIF
#ENDIF
#!--------------------------------------------------------------------------
#GROUP(%GenerateReportPrompts)
%%EnablePrintPreview LONG  (%CusReportShowPreview)
#IF(%CusReportShowPreview)
%%MaximizePreviewWindow LONG  (%CusReportPreviewMaximized)
%%InitialZoom DEFAULT  ('%'CusReportPreviewDefaultZoom')
#ENDIF
#!--------------------------------------------------------------------------
#INCLUDE('ABWProc.TPW')
#INCLUDE('ABWCntrl.TPW')
#INCLUDE('ABWField.TPW')
#INCLUDE('ABWBrw.TPW')
#INCLUDE('ABWForm.TPW')
#INCLUDE('ABWRep.TPW')
#INCLUDE('ABWStart.TPW')
#INCLUDE('ABWApp.TPW')
#INCLUDE('ABWDeflt.TPW')
#INCLUDE('ABWDctP.TPW')
#INCLUDE('ABWTheme.TPW')
#INCLUDE('ABWCus.TPW')
#INCLUDE('SVFnGrp.TPW')

