#TEMPLATE(Clarion,'Clarion Release Templates')
#HELP('ClarionHelp.chm')
#SYSTEM
  #TAB('OOP &Embed Tree Options'),HLP('~Application_Options_Dialog.htm#GEN3')
    #BOXED('&Options'),AT(12,,216)
      #PROMPT('Show &PROCEDURE Keyword',CHECK),%ShowPROCEDUREonEmbeds,DEFAULT(%True),AT(10,14)
      #PROMPT('Show &VIRTUAL Keyword',CHECK),%ShowVIRTUALonEmbeds,DEFAULT(%True),AT(10)
      #PROMPT('Show P&ROTECTED Keyword',CHECK),%ShowPROTECTEDonEmbeds,DEFAULT(%True),AT(10)
      #PROMPT('Show &Base Class',CHECK),%ShowBaseClassOnEmbeds,DEFAULT(%True),AT(10)
      #PROMPT('Show Object &Description',CHECK),%ShowDescriptionOnEmbeds,DEFAULT(%True),AT(10)
      #PROMPT('Show D&etails',CHECK),%ShowDetailsOnEmbeds,DEFAULT(%True),AT(10)
      #PROMPT('&Color Entries',CHECK),%ColorEntriesOnEmbeds,DEFAULT(%True),AT(10)
    #ENDBOXED
    #BOXED('&Colors'),AT(12,,216),WHERE(%ColorEntriesOnEmbeds)
      #PROMPT('&DATA Sections',COLOR),%ColorDataSection,DEFAULT(00000FFH)
      #PROMPT('&CODE Sections',COLOR),%ColorCodeSection,DEFAULT(0FF0000H)
      #PROMPT('&VIRTUAL Methods',COLOR),%ColorVirtualMethod,DEFAULT(0008000H)
      #PROMPT('&PROTECTED Methods',COLOR),%ColorProtectedMethod,DEFAULT(00000FFH)
      #PROMPT('&New Methods',COLOR),%ColorNewMethod,DEFAULT(0FF00FFH)
    #ENDBOXED
  #ENDTAB
#!
#CALL(%DeclareClassGlobals)
#!
#DECLARE (%SysActiveInvisible)               #!These variables store the 'system default values' of user configurable options
#DECLARE (%SysAllowUnfilled)
#DECLARE (%SysRetainRow)
#DECLARE (%SysResetOnGainFocus)
#DECLARE (%SysAutoToolbar)
#DECLARE (%SysAutoRefresh)
#!
#APPLICATION('CW Default Application'),HLP('~TPLApplication')
#!
#!------------------------------------------------------------------------------
#!                          Template Release  9.0
#!------------------------------------------------------------------------------
#! CW Application Template
#!------------------------------------------------------------------------------
#!
#! These are the Clarion for Windows Templates.  The #PROCEDURE templates
#! included in this template set are:
#!
#! ToDo     - A default "Under Construction" procedure
#! Window   - A general purpose window handling procedure
#!  Browse  - Window copied for Browse grouping
#!  Form    - Window copied for Form grouping
#!  Frame   - Window copied for Frame grouping
#!  Menu    - Window copied for Menu grouping
#! Process  - A general purpose sequential record handler
#! Source   - A generic source procedure - Embed points only
#!
#!------------------------------------------------------------------------------
#!
#! Global Prompts - (Appear on 'Global Settings' screen-moved from #PROGRAM).
#!
#!------------------------------------------------------------------------------
#!
#PREPARE
  #IF(%GlobalUseABCClasess)
    #CALL(%ReadABCFiles)
  #ENDIF
  #!CALL(%SetupGlobalObjects)
#ENDPREPARE
#!
#SHEET,ADJUST
  #TAB('OOPPrompts'),WHERE(%False)
    #PROMPT('Enable the use of ABC clasess', CHECK),%GlobalUseABCClasess,DEFAULT(0),AT(10)
    #BOXED,WHERE(%GlobalUseABCClasess)
      #INSERT(%OOPPrompts)
    #ENDBOXED
    #PROMPT('External Globals and ABC''s Source Module',OPTION),%ExternalSource,AT(,,180),DEFAULT('Statically Linked Library (LIB)')
    #PROMPT('&Dynamic Link Library (DLL)',RADIO)
    #PROMPT('&Statically Linked Library (LIB)',RADIO)
  #ENDTAB
  #TAB('&General'),HLP('~TPLApplication_General')
    #PROMPT('Program &Author:',@s40),%ProgramAuthor
    #PROMPT('Default &Icon:',ICON),%ProgramIcon,DEFAULT('')
    #PROMPT('&Use column description as MSG() when MSG() is blank',CHECK),%MessageDescription,DEFAULT(%True),AT(10,,180)
    #PROMPT('&Generate template global data as EXTERNAL',CHECK),%GlobalExternal,AT(10,,180)
    #PROMPT('G&enerate EMBED Comments',CHECK),%GenerateEmbedComments,AT(10,,180)
    #BOXED('.INI File Settings'),SECTION,AT(10,,180)
      #PROMPT('&Use .INI file to save and restore program settings',CHECK),%INIActive,AT(10,,170)
      #ENABLE(%INIActive)
        #BUTTON('INI File Options'),AT(,,155),HLP('~TPLINIFileOptions')
        #PROMPT('.&INI File to use:',DROP('Program Name.INI|Other')),%INIFile
        #BOXED, WHERE(%INIFile='Other')
        #! #ENABLE(%INIFile='Other')
          #PROMPT('&Other File Name:',@S255),%ININame
        #! #ENDENABLE
        #ENDBOXED
        #BOXED,SECTION,WHERE(%INIFile='Program Name.INI')
          #ENABLE(%INIFile='Program Name.INI')
            #PROMPT('Location of "Program Name.INI" File', OPTION), %INIProgramIniLocation, DEFAULT('APPDIR')
            #PROMPT('App Directory', RADIO),AT(15,15),VALUE('APPDIR')
            #PROMPT('CSIDL Directory', RADIO),AT(85,15),VALUE('CSIDLDIR')
            #ENABLE(%INIProgramIniLocation = 'CSIDLDIR')
              #BOXED,SECTION
                #PROMPT('CSIDL Directory:', DROP('CSIDL_PERSONAL[SV:CSIDL_PERSONAL]|CSIDL_COMMON_DOCUMENTS[SV:CSIDL_COMMON_DOCUMENTS]|CSIDL_APPDATA[SV:CSIDL_APPDATA]|CSIDL_LOCAL_APPDATA   (non-roaming)[SV:CSIDL_LOCAL_APPDATA]|CSIDL_COMMON_APPDATA[SV:CSIDL_COMMON_APPDATA]')), %INICSIDLDirectory,PROMPTAT(10,2,155),AT(10,15,155)
                #PROMPT('Company Name SubDirectory: ', @S50), %CSIDLCompanyDir, PROMPTAT(10,28,155),AT(10,41,155)
                #PROMPT('Product Name SubDirectory: ', @s50), %CSIDLProductDir, PROMPTAT(10,54,155),AT(10,67,155) 
                #PROMPT('Create folders if they don''t exist', CHECK), %CSIDLCreate, DEFAULT(%False), AT(10)
                #DISPLAY('Path:')
                #DISPLAY(SUB(%INICSIDLDirectory,4,LEN(%INICSIDLDirectory)-3) & '\' & %CSIDLCompanyDir & '\' & %CSIDLProductDir),AT(10)
              #ENDBOXED
            #ENDENABLE   
            #BOXED, HIDE
              #PROMPT('Use app directory',CHECK),%INIInAppDirectory,DEFAULT(%False) 
            #ENDBOXED
          #ENDENABLE
        #ENDBOXED
        #ENDBUTTON
        #DISPLAY('INI File: "'&CHOOSE(%INIFile='Other',%ININame,%INIProgramIniLocation&'\Program Name.INI')&'"')
      #ENDENABLE
    #ENDBOXED
  #ENDTAB
  #TAB('App &Settings'),HLP('~TPLApplication_AppSettings')
    #DISPLAY('')
    #ENABLE(%ProgramExtension = 'EXE')
      #PROMPT('Use Legacy Button Margins Compatibility',CHECK),%ButtonMarginsCompatibility,DEFAULT(0),AT(10)
    #ENDENABLE
    #INSERT(%XPVistaManifestGlobalPrompts)
    #BUTTON('Extended UI'),AT(,,175),HLP('~TPLExtendedUIGlobal')
       #BOXED('Extended UI')
           #PROMPT('Enable Themed Menu',CHECK),%ExtUIXPMenuEnableGlobal,DEFAULT(%False)
           #ENABLE(%ExtUIXPMenuEnableGlobal)
               #PROMPT('Color type:',DROP('OFF|OS Theme without bar[AUTONOBAR]|OS Theme with bar[AUTO]|OS Theme without bar and white background[AUTONOBARWHITE]|WindowsClassic|WindowsClassicLight|WindowsClassicLightWhite|VistaBlack|VistaAero|VistaAeroWhite|BabyBlue|BabyBlueWhite|XPLunaBlue|XPLunaBlueLight|XPLunaGreen|XPLunaGreenLight|XPLunaSilver|XPenergyBlue|VistaEnergyBlue|EnergyBlueLight|Windows2K|User Defined')),%ExtUIXPMenuColorTypeGlobal,DEFAULT('OFF'),WHENACCEPTED(%ExtendedUISetXPMenuColor())
               #PROMPT('Disable Vertical Bar',CHECK),%ExtUIXPMenuDisableImageBar,DEFAULT(%True)
               #BOXED, SECTION 
               #BOXED('Runtime Change Support'),WHERE(%ExtUIXPMenuColorTypeGlobal<>'User Defined'),AT(,0)
                   #PROMPT('Enable Runtime Support',CHECK),%ExtUIXPMenuEnableRuntime,DEFAULT(%False)
                   #ENABLE(%ExtUIXPMenuEnableRuntime)
                      #PROMPT('Storage Variable:',FROM(%GlobalData)),%ExtUIXPMenuRuntimeVar
                   #ENDENABLE
               #ENDBOXED
               #BOXED('User Colors'),WHERE(%ExtUIXPMenuColorTypeGlobal='User Defined'),AT(,0)
               #BUTTON('User Colors'),HLP('~TPLExtendedUIGlobalUserColors')
                   #BOXED('Vertical Bar')
                      #PROMPT('Left Color',COLOR),%ExtUIXPMenuColorLeftGlobal,DEFAULT(15920364)
                      #PROMPT('Right Color',COLOR),%ExtUIXPMenuColorRightGlobal,DEFAULT(12560039)
                   #ENDBOXED
                   #BOXED('Selection Bar')
                      #PROMPT('Left Color',COLOR),%ExtUIXPMenuColorSelectionBarLeftGlobal,DEFAULT(15920364)
                      #PROMPT('Right Color',COLOR),%ExtUIXPMenuColorSelectionBarRightGlobal,DEFAULT(15920364)
                      #PROMPT('Use Vertical Gradient',CHECK),%ExtUIXPMenuSelVertical,DEFAULT(%True)
                   #ENDBOXED
                   #BOXED('Selection Border')
                      #PROMPT('Color',COLOR),%ExtUIXPMenuColorSelectionBorderGlobal,DEFAULT(15920364)
                   #ENDBOXED
                   #BOXED('Over Menu Bar Item')
                      #PROMPT('Left Color',COLOR),%ExtUIXPMenuColorHotLeftGlobal,DEFAULT(15920364)
                      #PROMPT('Right Color',COLOR),%ExtUIXPMenuColorHotRightGlobal,DEFAULT(15920364)
                   #ENDBOXED
                   #BOXED('Selected Menu Bar Item')
                      #PROMPT('Left Color',COLOR),%ExtUIXPMenuColorSelectedLeftGlobal,DEFAULT(15920364)
                      #PROMPT('Right Color',COLOR),%ExtUIXPMenuColorSelectedRightGlobal,DEFAULT(15920364)
                   #ENDBOXED
                   #BOXED('Normal Bar')
                      #PROMPT('Left Color',COLOR),%ExtUIXPMenuColorNormalBarLeftGlobal,DEFAULT(15920364)
                      #PROMPT('Right Color',COLOR),%ExtUIXPMenuColorNormalBarRightGlobal,DEFAULT(15920364)
                   #ENDBOXED
                   #PROMPT('Item Background Color',COLOR),%ExtUIXPMenuColorItemBackgroundGlobal,DEFAULT(-2147483644)
                   #BOXED('Font Color')
                      #PROMPT('Normal Text',COLOR),%ExtUIXPMenuColorNormalText,DEFAULT(0)
                      #PROMPT('Selected Text',COLOR),%ExtUIXPMenuColorSelectedText,DEFAULT(0)
                      #PROMPT('Hot Text',COLOR),%ExtUIXPMenuColorHotText,DEFAULT(0)
                   #ENDBOXED
                   #PROMPT('Flat',CHECK),%ExtUIXPMenuFlat,DEFAULT(%True)
                   #PROMPT('Show Image Bar',CHECK),%ExtUIXPMenuShowImageBar,DEFAULT(%False)
                   #PROMPT('Separator3D',CHECK),%ExtUIXPMenuSeparator3D,DEFAULT(%True)
                   #PROMPT('SeparatorFull',CHECK),%ExtUIXPMenuSeparatorFull,DEFAULT(%False)
                   #PROMPT('VerticalLine',CHECK),%ExtUIXPMenuVerticalLine,DEFAULT(%True)
               #ENDBUTTON
               #ENDBOXED
               #ENDBOXED
           #ENDENABLE
           #BOXED('MDI Tab')
              #PROMPT('MDI Tab:',DROP('Disable[DISABLE]|Enable Top[TOP]|Enable Bottom[BOTTOM]')),%ExtUIMDITabGlobal,DEFAULT('DISABLE')
              #ENABLE(%ExtUIMDITabGlobal<>'DISABLE')
                  #PROMPT('MDI Tab Style',DROP('Default[Default]|B&W[BlackAndWhite]|Colored[Colored]|Squared[Squared]|Boxed[Boxed]')),%ExtUIMDITabStyleGlobal,DEFAULT('Default')
              #ENDENABLE
           #ENDBOXED
           #PROMPT('Sheet''s Tab Control Style:',DROP('Default[Default]|B&W[BlackAndWhite]|Colored[Colored]|Squared[Squared]|Boxed[Boxed]')),%ExtUITabStyleGlobal,DEFAULT('Default')
       #ENDBOXED
    #ENDBUTTON
    #INSERT(%EnhanceFocusGlobalPrompts)
    #INSERT(%UseInsteadGlobalPrompts)
    #ENABLE(%GlobalUseABCClasess)
    #PROMPT('Report Preview Mode',OPTION),%GlobalReportPreviewMode,DEFAULT('Procedure')
    #PROMPT('ABC Class',RADIO)
    #PROMPT('Procedure',RADIO)
    #ENDENABLE
    #ENABLE(NOT %GlobalUseABCClasess OR (%GlobalUseABCClasess AND %GlobalReportPreviewMode='Procedure'))
    #PROMPT('Default Report Preview Procedure',@S100),%GlobalReportPreview,DEFAULT('ReportPreview')
    #ENDENABLE
    #INSERT(%BrowseBoxExtendedGlobalOptionsPrompts)
    #PROMPT('Browse Active Invisible',CHECK),%GlobalBrowseActiveInvisible,DEFAULT(%True),AT(10)
    #INSERT(%RebasePromps)
  #ENDTAB
  #TAB('&File Control Flags'),HLP('~TPLApplication_FileControlFlags')
    #PROMPT('&Generate all file declarations',CHECK),%DefaultGenerate,AT(10,,180)
    #PROMPT('&When done with a File:',DROP('Close the File|Keep the File Open')),%DefaultCloseFile,DEFAULT('Close the File')
    #PROMPT('&Enclose RI code in transaction frame',CHECK),%DefaultRILogout,DEFAULT(1),AT(10,,180)
    #ENABLE(%DefaultRILogout)
      #PROMPT('&Issue template warning if LOGOUT() not allowed',CHECK),%WarnOnLogoutError,DEFAULT(%True),AT(10,,180)
    #ENDENABLE
    #PROMPT('&Seconds for RECOVER:',SPIN(@N3,1,120,1)),%LockRecoverTime,DEFAULT(10)
    #BOXED('File Attributes')
      #PROMPT('&Threaded:',DROP('Use File Setting|All Threaded|None Threaded')),%DefaultThreaded,DEFAULT('Use File Setting')
      #PROMPT('&Create:',DROP('Use File Setting|Create All|Create None')),%DefaultCreate,DEFAULT('Use File Setting')
      #PROMPT('E&xternal:',DROP('All External|None External')),%DefaultExternal,DEFAULT('None External')
      #ENABLE(%DefaultExternal = 'None External'),CLEAR
         #PROMPT('Generate file declarations in &Modules',CHECK),%DefaultLocalExternal,AT(10)
      #ENDENABLE
      #BOXED,SECTION
          #BOXED('External Files'),WHERE(%DefaultExternal = 'All External'),AT(,0)
            #PROMPT('Dec&laring Module:',@S255),%DefaultExternalSource
            #PROMPT('All &files are declared in another .APP',CHECK),%DefaultExternalAPP,AT(15,,156)
          #ENDBOXED
          #BOXED('Export Files'),WHERE(%DefaultExternal = 'None External' AND %ProgramExtension='DLL'),AT(,0)
            #PROMPT('Ex&port all file declarations',CHECK),%DefaultExport,AT(25,,156)
          #ENDBOXED
      #ENDBOXED
    #ENDBOXED
    #INSERT(%OOPPrompts(ABC))
       #PROMPT('Enable Triggers Support',CHECK),%TriggersSupportEnableGlobal,AT(10)
    #ENABLE(%TriggersSupportEnableGlobal)
       #BOXED,HIDE
        #BOXED('Class')
           #PROMPT('Default TriggersManager Class:', FROM(%pClassName)),%TriggersManagerType,DEFAULT('TriggerManager'),REQ
        #ENDBOXED
        #BOXED('Individual Triggers Manager Overrides')
           #BUTTON('Individual TriggersManager Overrides for '&%File&' file'),FROM(%File,%File & ' - ' & %FileDescription),AT(10,,180),INLINE,SORT
             #WITH(%ClassItem, 'ClientSideTriggers:' & %File)
               #INSERT(%GlobalClassPrompts(ABC))
             #ENDWITH
          #ENDBUTTON
        #ENDBOXED
       #ENDBOXED
    #ENDENABLE
    #BOXED('File Access')
      #PROMPT('File Open &Mode:',DROP('Share|Open|Other')),%DefaultOpenMode,DEFAULT('Share')
      #ENABLE(%DefaultOpenMode='Other')
        #BOXED('Other Open Mode')
          #PROMPT('&User Access:',DROP('Read/Write|Read Only|Write Only')),%DefaultUserAccess,DEFAULT('Read/Write')
          #PROMPT('Ot&her Access:',DROP('Deny None|Deny Read|Deny Write|Deny All|Any Access')),%DefaultOtherAccess,DEFAULT('Deny None')
        #ENDBOXED
      #ENDENABLE
    #ENDBOXED
    #BUTTON('File Access Data Path'),AT(,,175),HLP('~TPLApplication_FileAccessDataPath')
        #BOXED('Data Files Path')
        #PROMPT('Set SYSTEM{{PROP:DataPath} to this location', CHECK), %GeneratePropDataPath, DEFAULT(%False), AT(10)
        #ENABLE(%GeneratePropDataPath)
          #BOXED, SECTION
            #PROMPT('Data Path Folder', OPTION), %PropDataPathLocation,DEFAULT('CSIDLLIKEINI')
            #PROMPT('Use same CSIDL value as set for INI file', RADIO), VALUE('CSIDLLIKEINI')
            #PROMPT('Specify a different CSIDL value', RADIO), VALUE('OTHERCSIDL')
            #PROMPT('Specify a Folder', RADIO), VALUE('SPECIFYDIR')
            #BOXED('CSIDL Value'), SECTION, WHERE(%PropDataPathLocation = 'OTHERCSIDL'), AT(10,50)
              #PROMPT('CSIDL Folder:', DROP('CSIDL_PERSONAL[SV:CSIDL_PERSONAL]|CSIDL_COMMON_DOCUMENTS[SV:CSIDL_COMMON_DOCUMENTS]|CSIDL_APPDATA[SV:CSIDL_APPDATA]|CSIDL_LOCAL_APPDATA   (non-roaming)[SV:CSIDL_LOCAL_APPDATA]|CSIDL_COMMON_APPDATA[SV:CSIDL_COMMON_APPDATA]')), %DataPathCSIDLDirectory,PROMPTAT(10,2,155),AT(10,15,155), DEFAULT(%INICSIDLDirectory)
              #PROMPT('Company Name SubFolder: ', @S50), %DataPathCSIDLCompanyDir, PROMPTAT(10,28,155),AT(10,41,155), DEFAULT(%CSIDLCompanyDir)
              #PROMPT('Product Name SubFolder: ', @s50), %DataPathCSIDLProductDir, PROMPTAT(10,54,155),AT(10,67,155) ,DEFAULT(%CSIDLProductDir)
              #PROMPT('Create folders if they don''t exist', CHECK), %DataPathCSIDLCreate, DEFAULT(%False), AT(10)
              #DISPLAY('Path:')
              #DISPLAY(SUB(%DataPathCSIDLDirectory,4,LEN(%DataPathCSIDLDirectory)-3) & '\' & %DataPathCSIDLCompanyDir & '\' & %DataPathCSIDLProductDir),AT(10)
            #ENDBOXED
            #BOXED('Specify a Folder'), SECTION, WHERE(%PropDataPathLocation = 'SPECIFYDIR'), AT(10,50)
              #PROMPT('Enter Folder: ',@s100), %DataPathOtherDirectory,PROMPTAT(10,2,155),AT(10,15,155)
              #PROMPT('Create folders if they don''t exist', CHECK), %DataPathOtherDirectoryCreate, DEFAULT(%False), AT(10)
            #ENDBOXED
          #ENDBOXED
        #ENDENABLE
      #ENDBOXED
    #ENDBUTTON
  #ENDTAB
  #TAB('&Individual File Overrides'),HLP('~TPLApplication_IndividualFileOverrides')
    #BUTTON('Individual File Overrides'),FROM(%File,%File & ' - ' & %FileDescription),AT(10,,,180),HLP('~TPLApplication'),INLINE,SORT,HLP('~TPLApplication_IndividualFileOverrides')
      #PROMPT('&Generate file declaration',CHECK),%OverrideGenerate,AT(10,,180)
      #PROMPT('&When Done with the File:',DROP('Use Default|Close the File|Keep the File Open')),%OverrideCloseFile,DEFAULT('Use Default')
      #PROMPT('Us&e RI transaction frame',DROP('Use Default|Yes|No')),%OverrideRILogout,DEFAULT('Use Default')
      #BOXED('File Attributes')
        #PROMPT('&Threaded:',DROP('Use Default|Use File Setting|Threaded|Not Threaded')),%OverrideThreaded,DEFAULT('Use Default')
        #PROMPT('&Create:',DROP('Use Default|Use File Setting|Create File|Do Not Create File')),%OverrideCreate,DEFAULT('Use Default')
        #PROMPT('E&xternal:',DROP('Use Default|External|Not External')),%OverrideExternal,DEFAULT('Use Default')
        #ENABLE(%OverrideExternal = 'Not External')
           #PROMPT('Generate file declarations in a &Module',CHECK),%OverrideLocalExternal,AT(10)
        #ENDENABLE
        #BOXED,SECTION
          #BOXED('External File'),WHERE(%OverrideExternal = 'External'),AT(,0)
            #PROMPT('Dec&laring Module:',@S255),%OverrideExternalSource
            #PROMPT('The &file is declared in another .APP',CHECK),%OverrideExternalAPP,AT(15,,156)
          #ENDBOXED
          #BOXED('Export File'),WHERE((%OverrideExternal='Not External' OR (%OverrideExternal='Use Default' AND %DefaultExternal='None External')) AND %ProgramExtension='DLL'),AT(,0)
            #PROMPT('Ex&port file declaration',CHECK),%OverrideExport,AT(25,,156)
          #ENDBOXED
        #ENDBOXED
      #ENDBOXED
      #BOXED('File Access')
        #PROMPT('File Open &Mode:',DROP('Use Default|Share|Open|Other')),%OverrideOpenMode,DEFAULT('Use Default')
        #ENABLE(%OverRideOpenMode = 'Other')
          #BOXED('Other Open Mode')
            #PROMPT('&User Access:',DROP('Use Default|Read/Write|Read Only|Write Only')),%OverrideUserAccess,DEFAULT('Use Default')
            #PROMPT('Ot&her Access:',DROP('Use Default|Deny None|Deny Read|Deny Write|Deny All|Any Access')),%OverrideOtherAccess,DEFAULT('Use Default')
          #ENDBOXED
        #ENDENABLE
      #ENDBOXED
    #ENDBUTTON
  #ENDTAB
  #TAB('&Classes'),HLP('~TPLApplication_Classes')
    #PROMPT('Enable the use of ABC classes', CHECK),%GlobalUseABCClasess,DEFAULT(0),AT(10)
    #ENABLE(%GlobalUseABCClasess)
       #BUTTON('&Refresh Application Builder Class Information'),WHENACCEPTED(%ForceReadABCFiles()),AT(,,170)
       #ENDBUTTON
       #BUTTON('Application Builder Class &Viewer'),WHENACCEPTED(%ViewABCs()),AT(,,170)
       #ENDBUTTON
       #BUTTON('ABC Library Files'),AT(,,170),HLP('~TPLApplication_ABC_Library')
         #BOXED('ABC Library Files')
           #INSERT(%AbcLibraryPrompts)
         #ENDBOXED
       #ENDBUTTON
       #BOXED('Default Classes')
           #BUTTON('Edit in Place'),AT(,,170)
             #PROMPT('Browse EIP &Manager:', FROM(%pClassName)),%CWBrowseEIPManagerType,DEFAULT('CWEIPManager'),REQ
             #PROMPT('&Entry Class:', FROM(%pClassName)),%EditInPlaceEntryType,DEFAULT('EditEntryClass'),REQ
             #PROMPT('&Text Class:', FROM(%pClassName)),%EditInPlaceTextType,DEFAULT('EditTextClass'),REQ
             #PROMPT('&Check Class:', FROM(%pClassName)),%EditInPlaceCheckType,DEFAULT('EditCheckClass'),REQ
             #PROMPT('&Spin Class:', FROM(%pClassName)),%EditInPlaceSpinType,DEFAULT('EditSpinClass'),REQ
             #PROMPT('&DropList Class:', FROM(%pClassName)),%EditInPlaceDropListType,DEFAULT('EditDropListClass'),REQ
             #PROMPT('Dro&pCombo Class:', FROM(%pClassName)),%EditInPlaceDropComboType,DEFAULT('EditDropComboClass'),REQ
             #PROMPT('C&olor Class:', FROM(%pClassName)),%EditInPlaceColorType,DEFAULT('EditColorClass'),REQ
             #PROMPT('&File Class:', FROM(%pClassName)),%EditInPlaceFileType,DEFAULT('EditFileClass'),REQ
             #PROMPT('Fon&t Class:', FROM(%pClassName)),%EditInPlaceFontType,DEFAULT('EditFontClass'),REQ
             #PROMPT('&MultiSelect Class:', FROM(%pClassName)),%EditInPlaceMultiSelectType,DEFAULT('EditMultiSelectClass'),REQ
             #PROMPT('Ca&lendar Class:', FROM(%pClassName)),%EditInPlaceCalendarType,DEFAULT('EditCalendarClass'),REQ
             #PROMPT('Loo&kup Class:', FROM(%pClassName)),%EditInPlaceLookupType,DEFAULT('EditLookupClass'),REQ
             #PROMPT('Other Class:', FROM(%pClassName)),%EditInPlaceOtherType,DEFAULT('EditEntryClass'),REQ
           #ENDBUTTON
           #BUTTON('Reports'),AT(,,170)
             #PROMPT('Print Previewer:', FROM(%pClassName)),%PrintPreviewType,DEFAULT('PrintPreviewClass'),REQ
             #PROMPT('Report Target Selector:', FROM(%pClassName)),%ReportTargetSelectorManagerType,DEFAULT('ReportTargetSelectorClass'),REQ
           #ENDBUTTON
           #BUTTON('Calendar Class'),AT(,,170)
             #PROMPT('Calendar Class:', FROM(%pClassName)),%CalendarManagerType,DEFAULT('CalendarClass'),REQ
             #BUTTON('&Configure'),AT(100,,90),HLP('~TPLApplication_Classes_General_Configure4')
               #PROMPT('Change Default Color',CHECK),%GlobalChangeColor,AT(10),DEFAULT(%False)
               #ENABLE(%GlobalChangeColor)
                 #BOXED('Color Settings')
                   #PROMPT('Sunday Color:',COLOR),%GlobalColorSunday,DEFAULT(00000FFH)
                   #PROMPT('Saturday Color:',COLOR),%GlobalColorSaturday,DEFAULT(00000FFH)
                   #PROMPT('Holiday Color:',COLOR),%GlobalColorHoliday,DEFAULT(0008000H)
                   #PROMPT('Other Color:',COLOR),%GlobalColorOther,DEFAULT(0)
                 #ENDBOXED
               #ENDENABLE
               #PROMPT('Action for Close Button:',DROP('Select and Close|Cancel')),%GlobalSelectOnClose,DEFAULT('Select and Close')
             #ENDBUTTON
           #ENDBUTTON
       #ENDBOXED
    #ENDENABLE
  #ENDTAB
  #TAB('Clarion Version'),HLP('~TPLApplication_ClarionVersion')
      #DISPLAY('')
      #DISPLAY(' Clarion Version: '&%CWVersion), AT(10,,170)
      #DISPLAY(' Template Family: '&%AppTemplateFamily), AT(10,,170)
      #DISPLAY(' Template Version: '&%CWTemplateVersion), AT(10,,170)
      #BOXED,HIDE
        #PROMPT('Template Family',@S10),%AppTemplateFamily,DEFAULT('CLARION'),PROP(PROP:READONLY,1)
        #PROMPT('Template Version',@S10),%CWTemplateVersion,DEFAULT('v10.0'),PROP(PROP:READONLY,1)
      #ENDBOXED
  #ENDTAB
#ENDSHEET
#!------------------------------------------------------------------------------
#!
#! Global Template Declarations.
#!
#!------------------------------------------------------------------------------
#!
#IF(INSTRING(' ',%Application)>0)
   #ERROR('The application name can not contain any spaces.')
#ENDIF
#MESSAGE('Generating ' & %Application,0)          #! Open the Message Box
#SET(%CWTemplateVersion,'v10.0')
#SET(%AppTemplateFamily,'CLARION')
#DECLARE(%GlobalIncludeList),UNIQUE               #! List of global include statements
#DECLARE(%ModuleIncludeList),UNIQUE               #! List of module include statements
#DECLARE(%CalloutModules),UNIQUE
#! OOP
#SET(%ExternalSource,'Statically Linked Library (LIB)')
#DECLARE(%ClassDeclarations),MULTI                #! List of module class declaration code statements
#DECLARE(%OOPConstruct)                           #! Used to construct OOP & general purpose declaration statements
#DECLARE(%ByteCount,LONG)                         #! Used to test for filled EMBED points
#DECLARE(%IncludePrototype,LONG)                  #! Used to test for prototype requirements in class declarations
#!
#DECLARE(%UsedFile),UNIQUE                        #! Label of every file used
#DECLARE(%ProcFilesUsed),UNIQUE                   #! Label of every file used
#DECLARE(%UsedDriverDLLs),UNIQUE
#DECLARE(%PrintPreviewUsed)
#DECLARE(%FileExternalFlag)
#DECLARE(%FileThreadedFlag)
#DECLARE(%IniFileName)                            #! Used to construct INI file
#DECLARE(%GenerationCompleted,%Module),SAVE
#DECLARE(%GenerateModule)
#DECLARE(%VBXList),UNIQUE
#DECLARE(%OLENeeded)
#DECLARE(%OCXList),UNIQUE
#DECLARE(%LastTarget32),SAVE
#DECLARE(%LastProgramExtension),SAVE
#DECLARE(%LastApplicationDebug),SAVE
#DECLARE(%LastApplicationLocalLibrary),SAVE
#!
#DECLARE(%CustomGlobalMapModule),UNIQUE
#DECLARE(%CustomGlobalMapProcedure,%CustomGlobalMapModule),MULTI
#DECLARE(%CustomGlobalMapProcedurePrototype,%CustomGlobalMapProcedure)
#DECLARE(%CustomGlobalData),UNIQUE
#DECLARE(%CustomGlobalDataDeclaration,%CustomGlobalData)
#DECLARE(%CustomGlobalDataBeforeFiles,%CustomGlobalData)
#DECLARE(%CustomGlobalDataComponent,%CustomGlobalData),MULTI
#DECLARE(%CustomGlobalDataComponentIndent,%CustomGlobalDataComponent)
#DECLARE(%CustomGlobalDataComponentDeclaration,%CustomGlobalDataComponent)
#!
#DECLARE(%CustomModuleMapModule),UNIQUE
#DECLARE(%CustomModuleMapProcedure,%CustomModuleMapModule),MULTI
#DECLARE(%CustomModuleMapProcedurePrototype,%CustomModuleMapProcedure)
#DECLARE(%CustomModuleData),UNIQUE
#DECLARE(%CustomModuleDataDeclaration,%CustomModuleData)
#DECLARE(%CustomModuleDataComponent,%CustomModuleData),MULTI
#DECLARE(%CustomModuleDataComponentIndent,%CustomModuleDataComponent)
#DECLARE(%CustomModuleDataComponentDeclaration,%CustomModuleDataComponent)
#DECLARE(%CustomGlobalMapIncludes),UNIQUE
#DECLARE(%CustomGlobalDeclarationIncludes),UNIQUE
#DECLARE(%CustomGlobalProcedureIncludes),UNIQUE
#!
#DECLARE(%CustomFlags),UNIQUE                     #! Allows third party developers to create
#DECLARE(%CustomFlagSetting,%CustomFlags)         #! Their own "symbols"
#!
#DECLARE(%AccessMode)                             #! File open mode equate
#DECLARE(%BuildFile)                              #! Construction filenames
#DECLARE(%BuildHeader)
#DECLARE(%BuildInclude)
#DECLARE(%ExportFile)                             #! File for Export list
#DECLARE(%ValueConstruct)                         #! Construct various strings
#DECLARE(%HoldConstruct)                          #! Construct various strings
#DECLARE(%TempUserFilter)                         #! Store the user filter if it is a constant and there are some range
#DECLARE(%RegenerateGlobalModule)                 #! Will we regen the global module?
#DECLARE(%ClipName)
#DECLARE(%RIDelFileName)
#DECLARE(%RIUpdFileName)
#DECLARE(%SFFileName)                            #! Standard Functions
#SET(%ClipName,SUB(%Application,1,5))
#SET(%RIDelFileName,%ClipName & '_RD.CLW')
#PROJECT(%RIDelFileName)                            #! Referential Integrity
#SET(%RIUpdFileName,%ClipName & '_RU.CLW')
#PROJECT(%RIUpdFileName)                            #! Referential Integrity
#SET(%SFFileName,%ClipName & '_SF.CLW')
#PROJECT(%SFFileName)                            #! Standard Functions
#DECLARE(%UpdateRelationPrimary),UNIQUE
#DECLARE(%UpdateRelationSecondary,%UpdateRelationPrimary),UNIQUE
#DECLARE(%UpdateAttachedFile,%UsedFile),UNIQUE
#DECLARE(%DeleteRelationPrimary),UNIQUE
#DECLARE(%DeleteRelationSecondary,%DeleteRelationPrimary),UNIQUE
#DECLARE(%DeleteAttachedFile,%UsedFile),UNIQUE
#DECLARE(%AllFile),UNIQUE
#EQUATE(%FilesPerFCModule,100)                     #! No of file definitions per FC module
#DECLARE(%ResizeGlobalDefsPlaced, LONG)
#DECLARE(%BRWList),UNIQUE
#!
#DECLARE (%Category),UNIQUE
#DECLARE (%CategoryDllInit,%Category)
#DECLARE (%CategoryDllKill,%Category)
#DECLARE (%CategoryLibName, %Category)
#DECLARE (%CategoryDllMode, %Category)
#DECLARE (%CategoryLinkMode, %Category)
#DECLARE (%CategoryDllModePrefix, %Category)
#DECLARE (%CategoryGlobal,%Category),MULTI
#DECLARE (%CategoryGlobalType,%CategoryGlobal)
#DECLARE (%CategoryDllInitParam,%CategoryGlobal)
#!
#!------------------------------------------------------------------------------
#!
#! Initialization Code for Global User-defined Symbols.
#!
#!------------------------------------------------------------------------------
#!
#SET (%ResizeGlobalDefsPlaced, %False)
#IF(%INIFile = 'Program Name.INI')                #! IF using program.ini
  #CASE(%INIProgramIniLocation)
  #OF('APPDIR')
    #SET(%INIFileName, '.\' & %Application & '.INI')
  #OF('CSIDLDIR')
    #SET(%INIFileName, '!svgloIniFileName')
  #ENDCASE
  #! #IF(%INIProgramIniLocation = 'CSIDLDIR'
  #! #SET(%INIFileName, %Application & '.INI')       #! SET the file name
#ELSE                                             #! ELSE (IF NOT using Program.ini)
  #SET(%INIFileName,%ININame)                     #! SET the file name
#ENDIF                                            #! END (IF using program.ini)

#!
#!------------------------------------------------------------------------------
#!
#! Main Source Code Generation Loop.
#!
#!------------------------------------------------------------------------------
#!
#DECLARE(%GlobalRegenerate)                       #! Flag that controls generation
#SET(%BuildFile,(%Application & '.B1$'))          #! Make temp program filename
#SET(%BuildHeader,(%Application & '.H1$'))          #! Make temp program filename
#SET(%BuildInclude,(%Application & '.I1$'))          #! Make temp program filename
#!
#!
#AT(%BeforeGlobalIncludes)
#CALL(%EnhanceFocusGlobalIncludeList)
#CALL(%UseInsteadGlobalIncludeList)
#ENDAT
#!
#!
#!
#IF(%GlobalUseABCClasess)
#CALL(%ReadABCFiles)
#!
#!
#FIX(%Driver,'ASCII')                           #!ASCII driver required by ABError.CLW
#IF(%Driver <> 'ASCII')
  #ERROR('Ascii file driver MUST be registered, used by Global Error Manager')
  #ABORT
#END
#!PROJECT(%DriverLIB)
#!
#IF(~%GlobalExternal)
  #IF(%ProgramExtension='DLL')                    #! Include all TopSpeed's base classes in non-external DLL
    #FIX(%Driver,'ASCII')                         #! Required for ASCIIViewer etc...
    #PROJECT(%DriverLIB)
  #ENDIF
#ENDIF
#!
#CALL(%FormatManagerAddDriver)
#!
#CALL(%AddCategory, 'ABC','Init','Kill')
#!CALL(%SetCategoryLocationFromPrompts, 'ABC', 'ABC', 'ABC')
#CALL(%SetCategoryLocation, 'ABC', 'ABC', 1, 0)
#!
#IF(%ProgramExtension<>'EXE')
  #FOR(%pClassName)
    #FIX (%Category, %pClassCategory)
    #IF (%Category AND %CategoryLinkMode)
      #ADD(%GlobalIncludeList,%RemovePath(%pClassIncFile))
    #ENDIF
  #ENDFOR
#ENDIF
#!
#ENDIF
#!
#!
#IF((%INIFile='Program Name.INI' AND %INIProgramIniLocation = 'CSIDLDIR') OR (%GeneratePropDataPath = %True AND %PropDataPathLocation = 'OTHERCSIDL') or %DataPathOtherDirectoryCreate = %True)
  #ADD(%GlobalIncludeList, 'CSIDL.EQU')
  #ADD(%GlobalIncludeList, 'SPECIALFOLDER.INC')
#ENDIF
#!
#!
#IF(%TriggersSupportEnableGlobal)
    #CALL(%ReadABCFiles(ABC))
    #CALL(%TriggersSetupGlobalObjects)
       #PROJECT(%Application&'T0.CLW')
#ENDIF
#!
#!
#IF (%EditProcedure)                              #! Special case for editing embedded source in context
  #CREATE(%EditFilename)
  #FIND(%ModuleProcedure,%EditProcedure)
  #FIX(%Procedure,%ModuleProcedure)               #! Fix current procedure
  #MESSAGE('Generating Module:    ' & %Module,1)  #! Post generation message
  #MESSAGE('Generating Procedure: ' & %Procedure,2) #! Post generation message
  #GENERATE(%Procedure)                           #! Generate procedure code
  #COMMENT(60)                                    #! Set comment alignment to column 60
  #CLOSE
  #ABORT
#ENDIF
#IF(~%ConditionalGenerate OR %DictionaryChanged OR %RegistryChanged)
  #SET(%GlobalRegenerate,%True)                   #! Generate Everything
  #FOR(%Module)
    #SET(%GenerationCompleted,%False)
  #ENDFOR
#ELSE                                             #! ELSE (If no global change)
  #SET(%GlobalRegenerate,%False)                  #! Generate changed modules only
#ENDIF                                            #! END (IF Global Change)
#EMBED(%BeforeGenerateApplication,'Before Generating Application'),HIDE
#FOR(%Module), WHERE (%Module <> %Program)        #! For all member modules
  #MESSAGE('Generating Module:    ' & %Module, 1) #! Post generation message
  #SET(%GenerateModule,%True)
  #IF(%ModuleExternal)
    #SET(%GenerateModule,%False)
  #ELSIF(%ModuleChanged)
  #ELSIF(%GlobalRegenerate)
  #ELSIF(NOT %GenerationCompleted)
  #ELSIF(NOT FILEEXISTS(%Module))
  #ELSE
    #SET(%GenerateModule,%False)
  #ENDIF
  #IF(%GenerateModule)                            #! IF module to be generated
    #GENERATE(%Module)
    #COMMENT(60)                                  #! Set comment alignment to column 60
  #ENDIF                                          #! END (If module to be...)
#ENDFOR                                           #! END (For all member modules)
#FIX(%Module,%Program)                            #! FIX to program module
#MESSAGE('Generating Module:    ' & %Module, 1)   #! Post generation message
#SET(%GenerateModule,%True)
#IF(%ModuleExternal)
  #SET(%GenerateModule,%False)
#ELSIF(%RegenerateGlobalModule)
#ELSIF(%ModuleChanged)
#ELSIF(%GlobalRegenerate)
#ELSIF(NOT %GenerationCompleted)
#ELSIF(NOT FILEEXISTS(%Module))
#ELSIF(%LastTarget32<>%Target32)
#ELSIF(%LastProgramExtension<>%ProgramExtension)
#ELSIF(%LastApplicationDebug<>%ApplicationDebug)
#ELSIF(%LastApplicationLocalLibrary<>%ApplicationLocalLibrary)
#ELSE
  #SET(%GenerateModule,%False)
#ENDIF
#SET(%LastTarget32,%Target32)
#SET(%LastProgramExtension,%ProgramExtension)
#SET(%LastApplicationDebug,%ApplicationDebug)
#SET(%LastApplicationLocalLibrary,%ApplicationLocalLibrary)
#FREE(%UsedDriverDLLs)
#EMBED(%CustomGlobalDeclarations,'Compile Global Declarations'),HIDE
#FOR(%File)
  #IF(%DefaultGenerate OR %OverrideGenerate)
    #ADD(%UsedFile,%File)
  #ENDIF
#ENDFOR
#!
#!Ensure that Aliased files are included in %UsedFile to force generation of aliased file
#!
#FOR(%File),WHERE(%AliasFile AND INLIST(%File,%UsedFile))
  #ADD(%UsedFile,%AliasFile)
#ENDFOR
#IF(%GenerateModule)
  #SET(%GenerationCompleted,%False)
  #EMBED (%BeforeGenerateProgram, 'Before Generating Program'), HIDE
  #CREATE(%BuildFile)                             #! Create temp module file
  #MESSAGE('Generating Program Code',2)           #! Post generation message
  #GENERATE(%Program)
  #COMMENT(60)                                    #! Set comment alignment to column 60
  #FOR(%ModuleProcedure)                          #! For all procs in module
    #FIX(%Procedure,%ModuleProcedure)             #! Fix current procedure
    #MESSAGE('Generating Procedure: ' & %Procedure, 2) #! Post generation message
    #GENERATE(%Procedure)                         #! Generate procedure code
    #COMMENT(60)                                  #! Set comment alignment to column 60
  #ENDFOR                                         #! EndFor all procs in module
  #CLOSE(%BuildFile)                              #! Close last temp file
  #REPLACE(%Program,%BuildFile)                   #! Replace if changed
  #REMOVE(%BuildFile)
  #INSERT(%GenerateStandardFunctions)
  #INSERT(%GenerateRICode)
  #!
  #IF(~%GlobalExternal)
    #IF(%DefaultLocalExternal)
        #CALL(%GenerateGlobalData,'FIRST','LOCAL','LINK')
        #CALL(%WriteFDModules,'LINK')
    #ENDIF
  #ENDIF 
  #!
  #INSERT(%ConstructXPManifest)
  #!
  #INSERT(%ConstructExportFile)
  #FOR(%UsedFile)                                   #! FOR all files used
    #FIX(%File,%UsedFile)                           #! FIX to that file
    #FIX(%Driver,%FileDriver)                       #! FIX to file's driver
    #ADD(%UsedDriverDLLs,%DriverDLL)
    #PROJECT(%DriverLIB)                            #! ADD driver LIB to project
  #ENDFOR                                           #! END (FOR all files used)
  #INSERT(%ConstructShipList)
#ELSE
  #FOR(%UsedFile)                                   #! FOR all files used
    #FIX(%File,%UsedFile)                           #! FIX to that file
    #FIX(%Driver,%FileDriver)                       #! FIX to file's driver
    #ADD(%UsedDriverDLLs,%DriverDLL)
    #PROJECT(%DriverLIB)                            #! ADD driver LIB to project
  #ENDFOR                                           #! END (FOR all files used)
#ENDIF
#REMOVE(%BuildFile)                               #! Remove the temp file
#CALL(%GenerateGlobalDataAndFilesModules)
#!
#EMBED(%AfterGeneratedApplication),HIDE
#!
#SET(%GenerationCompleted,%True)
#!
#!
#AT(%CustomGlobalDeclarations)
 #!#IF(%GlobalUseABCClasess AND %ProgramExtension='DLL' AND %DefaultExternal = 'None External' AND %DefaultExport)
 #!   #ADD( %CustomGlobalMapIncludes,'CWUtil.INC' )
 #!   #PROJECT('CWUtil.CLW')
 #!#ENDIF
#ENDAT
#!
#!
#AT(%DLLExportList)
 #!#IF(%GlobalUseABCClasess AND %ProgramExtension='DLL' AND %DefaultExternal = 'None External' AND %DefaultExport)
#!#INSERT(%ExportCWUtils)
 #!#ENDIF
#ENDAT
#!
#!
#AT(%BeforeFileDeclarations)
#INSERT(%FormatManagerGlobalDataDCT)
#ENDAT
#!
#!
#AT(%AfterFileDeclarations)
#INSERT(%FormatManagerGlobalData)
#ENDAT
#!
#AT(%ProgramSetup), WHERE(%ProgramIcon <> '')
  #IF(SLICE(%ProgramIcon,1,1) = '!')
      #DECLARE(%tmpIconValue)
      #SET(%tmpIconValue,SLICE(%ProgramIcon,2,LEN(%ProgramIcon)))
SYSTEM{PROP:Icon} = %tmpIconValue
  #ELSE
    #IF((INSTRING(':',%ProgramIcon,1,1) > 0 AND INSTRING(':\',%ProgramIcon,1,1) = 0) OR SLICE(LEFT(%ProgramIcon),1,1)='''')
SYSTEM{PROP:Icon} = %ProgramIcon
    #ELSE
SYSTEM{PROP:Icon} = '%ProgramIcon'
    #ENDIF
  #ENDIF
#ENDAT
#!
#AT(%CustomGlobalDeclarations)
    #IF (SLICE(%ProgramIcon,1,1) = '~')
      #DECLARE(%tmpIconValue)
      #SET(%tmpIconValue,SLICE(%ProgramIcon,2,LEN(%ProgramIcon)))
      #PROJECT(%tmpIconValue)
    #ENDIF
#ENDAT
#!
#! Set the TRN attribute to every control that is in a Tab
#! Only when %ForceMakeTransparentXPManifest = %True
#!
#AT(%WindowStructureForEachControls)
#CALL(%AddTRNXPManifest)
#ENDAT
#!
#!-----------------------------------
#! Triggers support start
#!-----------------------------------
#!
#AT(%BeforeGlobalIncludes),WHERE(%TriggersSupportEnableGlobal)
#CALL(%AddModuleIncludeFile(ABC),%TriggersManagerType,1)
#ENDAT
#!
#!
#AT(%TriggerManagerCodeSection),PRIORITY(5000),DESCRIPTION('Parent Call'),WHERE(NOT %GlobalExternal AND %TriggersSupportEnableGlobal)
  #CALL(%GenerateParentCall(ABC))
#ENDAT
#!
#AT(%ProgramProcedures),WHERE(%TriggersSupportEnableGlobal)
#CALL(%GenerateTriggersCLW)
#ENDAT
#!
#AT(%TriggerManagerCodeSection,,'Setup','()'),WHERE(%TriggersSupportEnableGlobal)
SELF.File &= %File
SELF.Buffer &= %FilePrefix:Record
#ENDAT
#!
#!
#!
#AT(%TriggerManagerDataSection, , 'PreDelete', '(*CSTRING ErrCode,*CSTRING ErrMsg),BYTE'),WHERE(~%AliasFile AND %TriggersSupportEnableGlobal),PRIORITY(5000),DESCRIPTION('DCT Data')
#INSERT(%TriggerGenerateData,'BEFORE_DELETE')
#ENDAT
#!
#!
#AT(%TriggerManagerCodeSection, , 'PreDelete', '(*CSTRING ErrCode,*CSTRING ErrMsg),BYTE'),WHERE(~%AliasFile AND %TriggersSupportEnableGlobal)
#PRIORITY(100),DESCRIPTION('Start Scope')
#INSERT(%TriggerGenerateCodeStart,'BEFORE_DELETE')
#PRIORITY(6000),DESCRIPTION('DCT Code')
#INSERT(%TriggerGenerateCodeDCTCode,'BEFORE_DELETE')
#PRIORITY(7000),DESCRIPTION('End Scope')
#INSERT(%TriggerGenerateCodeEnd,'BEFORE_DELETE')
#ENDAT
#!
#!
#AT(%ClassVirtualMethodRoutineCodeSection, , 'PreDelete', '(*CSTRING ErrCode,*CSTRING ErrMsg),BYTE'),WHERE(~%AliasFile AND %TriggersSupportEnableGlobal)
#INSERT(%TriggerGenerateCodeRoutine,'BEFORE_DELETE')
#ENDAT
#!
#!
#AT(%TriggerManagerDataSection, , 'PostDelete', '(*CSTRING ErrCode,*CSTRING ErrMsg),BYTE'),WHERE(~%AliasFile  AND %TriggersSupportEnableGlobal),PRIORITY(5000),DESCRIPTION('DCT Data')
#INSERT(%TriggerGenerateData,'AFTER_DELETE')
#ENDAT
#!
#!
#AT(%TriggerManagerCodeSection, , 'PostDelete', '(*CSTRING ErrCode,*CSTRING ErrMsg),BYTE'),WHERE(~%AliasFile AND %TriggersSupportEnableGlobal)
#PRIORITY(100),DESCRIPTION('Start Scope')
#INSERT(%TriggerGenerateCodeStart,'AFTER_DELETE')
#PRIORITY(6000),DESCRIPTION('DCT Code')
#INSERT(%TriggerGenerateCodeDCTCode,'AFTER_DELETE')
#PRIORITY(7000),DESCRIPTION('End Scope')
#INSERT(%TriggerGenerateCodeEnd,'AFTER_DELETE')
#ENDAT
#!
#!
#AT(%ClassVirtualMethodRoutineCodeSection, , 'PostDelete', '(*CSTRING ErrCode,*CSTRING ErrMsg),BYTE'),WHERE(~%AliasFile AND %TriggersSupportEnableGlobal)
#INSERT(%TriggerGenerateCodeRoutine,'AFTER_DELETE')
#ENDAT
#!
#!
#AT(%TriggerManagerDataSection, , 'PreInsert', '(SIGNED OpCode,UNSIGNED AddLen,*CSTRING ErrCode,*CSTRING ErrMsg),BYTE'),WHERE(~%AliasFile AND %TriggersSupportEnableGlobal),PRIORITY(5000),DESCRIPTION('DCT Data')
#INSERT(%TriggerGenerateData,'BEFORE_INSERT')
#ENDAT
#!
#!
#AT(%TriggerManagerCodeSection, , 'PreInsert', '(SIGNED OpCode,UNSIGNED AddLen,*CSTRING ErrCode,*CSTRING ErrMsg),BYTE'),WHERE(~%AliasFile AND %TriggersSupportEnableGlobal)
#PRIORITY(100),DESCRIPTION('Start Scope')
#INSERT(%TriggerGenerateCodeStart,'BEFORE_INSERT')
#PRIORITY(6000),DESCRIPTION('DCT Code')
#INSERT(%TriggerGenerateCodeDCTCode,'BEFORE_INSERT')
#PRIORITY(7000),DESCRIPTION('End Scope')
#INSERT(%TriggerGenerateCodeEnd,'BEFORE_INSERT')
#ENDAT
#!
#!
#AT(%ClassVirtualMethodRoutineCodeSection, , 'PreInsert', '(SIGNED OpCode,UNSIGNED AddLen,*CSTRING ErrCode,*CSTRING ErrMsg),BYTE'),WHERE(~%AliasFile AND %TriggersSupportEnableGlobal)
#INSERT(%TriggerGenerateCodeRoutine,'BEFORE_INSERT')
#ENDAT
#!
#!
#AT(%TriggerManagerDataSection, , 'PostInsert', '(*CSTRING ErrCode,*CSTRING ErrMsg),BYTE'),WHERE(~%AliasFile AND %TriggersSupportEnableGlobal),PRIORITY(5000),DESCRIPTION('DCT Data')
#INSERT(%TriggerGenerateData,'AFTER_INSERT')
#ENDAT
#!
#!
#AT(%TriggerManagerCodeSection, , 'PostInsert', '(*CSTRING ErrCode,*CSTRING ErrMsg),BYTE'),WHERE(~%AliasFile AND %TriggersSupportEnableGlobal)
#PRIORITY(100),DESCRIPTION('Start Scope')
#INSERT(%TriggerGenerateCodeStart,'AFTER_INSERT')
#PRIORITY(6000),DESCRIPTION('DCT Code')
#INSERT(%TriggerGenerateCodeDCTCode,'AFTER_INSERT')
#PRIORITY(7000),DESCRIPTION('End Scope')
#INSERT(%TriggerGenerateCodeEnd,'AFTER_INSERT')
#ENDAT
#!
#!
#AT(%ClassVirtualMethodRoutineCodeSection, , 'PostInsert', '(*CSTRING ErrCode,*CSTRING ErrMsg),BYTE'),WHERE(~%AliasFile AND %TriggersSupportEnableGlobal)
#INSERT(%TriggerGenerateCodeRoutine,'AFTER_INSERT')
#ENDAT
#!
#!
#AT(%TriggerManagerDataSection, , 'PreUpdate', '(LONG Pntr,UNSIGNED PutLen,*CSTRING ErrCode,*CSTRING ErrMsg),BYTE'),WHERE(~%AliasFile AND %TriggersSupportEnableGlobal),PRIORITY(5000),DESCRIPTION('DCT Data')
#INSERT(%TriggerGenerateData,'BEFORE_UPDATE')
#ENDAT
#!
#!
#AT(%TriggerManagerCodeSection, , 'PreUpdate', '(LONG Pntr,UNSIGNED PutLen,*CSTRING ErrCode,*CSTRING ErrMsg),BYTE'),WHERE(~%AliasFile AND %TriggersSupportEnableGlobal)
#PRIORITY(100),DESCRIPTION('Start Scope')
#INSERT(%TriggerGenerateCodeStart,'BEFORE_UPDATE')
#PRIORITY(6000),DESCRIPTION('DCT Code')
#INSERT(%TriggerGenerateCodeDCTCode,'BEFORE_UPDATE')
#PRIORITY(7000),DESCRIPTION('End Scope')
#INSERT(%TriggerGenerateCodeEnd,'BEFORE_UPDATE')
#ENDAT
#!
#!
#AT(%ClassVirtualMethodRoutineCodeSection, , 'PreUpdate', '(LONG Pntr,UNSIGNED PutLen,*CSTRING ErrCode,*CSTRING ErrMsg),BYTE'),WHERE(~%AliasFile AND %TriggersSupportEnableGlobal)
#INSERT(%TriggerGenerateCodeRoutine,'BEFORE_UPDATE')
#ENDAT
#!
#!
#AT(%TriggerManagerDataSection, , 'PostUpdate', '(*CSTRING ErrCode,*CSTRING ErrMsg),BYTE'),WHERE(~%AliasFile AND %TriggersSupportEnableGlobal),PRIORITY(5000),DESCRIPTION('DCT Data')
#INSERT(%TriggerGenerateData,'AFTER_UPDATE')
#ENDAT
#!
#!
#AT(%TriggerManagerCodeSection, , 'PostUpdate', '(*CSTRING ErrCode,*CSTRING ErrMsg),BYTE'),WHERE(~%AliasFile AND %TriggersSupportEnableGlobal)
#PRIORITY(100),DESCRIPTION('Start Scope')
#INSERT(%TriggerGenerateCodeStart,'AFTER_UPDATE')
#PRIORITY(6000),DESCRIPTION('DCT Code')
#INSERT(%TriggerGenerateCodeDCTCode,'AFTER_UPDATE')
#PRIORITY(7000),DESCRIPTION('End Scope')
#INSERT(%TriggerGenerateCodeEnd,'AFTER_UPDATE')
#ENDAT
#!
#!
#AT(%ClassVirtualMethodRoutineCodeSection, , 'PostUpdate', '(*CSTRING ErrCode,*CSTRING ErrMsg),BYTE'),WHERE(~%AliasFile AND %TriggersSupportEnableGlobal)
#INSERT(%TriggerGenerateCodeRoutine,'AFTER_UPDATE')
#ENDAT
#!
#!
#AT(%TriggerManagerDataSection, , 'PreFetch', '(SIGNED OpCode,KEY key,STRING positionBuffer,LONG pointer,UNSIGNED recLen,*CSTRING ErrCode,*CSTRING ErrMsg),BYTE'),WHERE(~%AliasFile),PRIORITY(5000),DESCRIPTION('DCT Data')
#INSERT(%TriggerGenerateData,'BEFORE_FETCH')
#ENDAT
#!
#!
#AT(%TriggerManagerCodeSection, , 'PreFetch', '(SIGNED OpCode,KEY key,STRING positionBuffer,LONG pointer,UNSIGNED recLen,*CSTRING ErrCode,*CSTRING ErrMsg),BYTE'),WHERE(~%AliasFile)
#PRIORITY(100),DESCRIPTION('Start Scope')
#INSERT(%TriggerGenerateCodeStart,'BEFORE_FETCH')
#PRIORITY(6000),DESCRIPTION('DCT Code')
#INSERT(%TriggerGenerateCodeDCTCode,'BEFORE_FETCH')
#PRIORITY(7000),DESCRIPTION('End Scope')
#INSERT(%TriggerGenerateCodeEnd,'BEFORE_FETCH')
#ENDAT
#!
#!
#AT(%ClassVirtualMethodRoutineCodeSection, , 'PreFetch', '(SIGNED OpCode,KEY key,STRING positionBuffer,LONG pointer,UNSIGNED recLen,*CSTRING ErrCode,*CSTRING ErrMsg),BYTE'),WHERE(~%AliasFile)
#INSERT(%TriggerGenerateCodeRoutine,'BEFORE_FETCH')
#ENDAT
#!
#!
#AT(%TriggerManagerDataSection, , 'PostFetch', '(SIGNED OpCode,KEY key,STRING positionBuffer,LONG pointer,UNSIGNED recLen,*CSTRING ErrCode,*CSTRING ErrMsg),BYTE'),WHERE(~%AliasFile),PRIORITY(5000),DESCRIPTION('DCT Data')
#INSERT(%TriggerGenerateData,'AFTER_FETCH')
#ENDAT
#!
#!
#AT(%TriggerManagerCodeSection, , 'PostFetch', '(SIGNED OpCode,KEY key,STRING positionBuffer,LONG pointer,UNSIGNED recLen,*CSTRING ErrCode,*CSTRING ErrMsg),BYTE'),WHERE(~%AliasFile)
#PRIORITY(100),DESCRIPTION('Start Scope')
#INSERT(%TriggerGenerateCodeStart,'AFTER_FETCH')
#PRIORITY(6000),DESCRIPTION('DCT Code')
#INSERT(%TriggerGenerateCodeDCTCode,'AFTER_FETCH')
#PRIORITY(7000),DESCRIPTION('End Scope')
#INSERT(%TriggerGenerateCodeEnd,'AFTER_FETCH')
#ENDAT
#!
#!
#AT(%ClassVirtualMethodRoutineCodeSection, , 'PostFetch', '(SIGNED OpCode,KEY key,STRING positionBuffer,LONG pointer,UNSIGNED recLen,*CSTRING ErrCode,*CSTRING ErrMsg),BYTE'),WHERE(~%AliasFile)
#INSERT(%TriggerGenerateCodeRoutine,'AFTER_FETCH')
#ENDAT
#!
#!
#AT(%TriggerManagerDataSection, , 'PreCreate', '(*CSTRING ErrCode,*CSTRING ErrMsg),BYTE'),WHERE(~%AliasFile),PRIORITY(5000),DESCRIPTION('DCT Data')
#INSERT(%TriggerGenerateData,'BEFORE_CREATE')
#ENDAT
#!
#!
#AT(%TriggerManagerCodeSection, , 'PreCreate', '(*CSTRING ErrCode,*CSTRING ErrMsg),BYTE'),WHERE(~%AliasFile)
#PRIORITY(100),DESCRIPTION('Start Scope')
#INSERT(%TriggerGenerateCodeStart,'BEFORE_CREATE')
#PRIORITY(6000),DESCRIPTION('DCT Code')
#INSERT(%TriggerGenerateCodeDCTCode,'BEFORE_CREATE')
#PRIORITY(7000),DESCRIPTION('End Scope')
#INSERT(%TriggerGenerateCodeEnd,'BEFORE_CREATE')
#ENDAT
#!
#!
#AT(%ClassVirtualMethodRoutineCodeSection, , 'PreCreate', '(*CSTRING ErrCode,*CSTRING ErrMsg),BYTE'),WHERE(~%AliasFile)
#INSERT(%TriggerGenerateCodeRoutine,'BEFORE_CREATE')
#ENDAT
#!
#!
#AT(%TriggerManagerDataSection, , 'PostCreate', '(*CSTRING ErrCode,*CSTRING ErrMsg),BYTE'),WHERE(~%AliasFile),PRIORITY(5000),DESCRIPTION('DCT Data')
#INSERT(%TriggerGenerateData,'AFTER_CREATE')
#ENDAT
#!
#!
#AT(%TriggerManagerCodeSection, , 'PostCreate', '(*CSTRING ErrCode,*CSTRING ErrMsg),BYTE'),WHERE(~%AliasFile)
#PRIORITY(100),DESCRIPTION('Start Scope')
#INSERT(%TriggerGenerateCodeStart,'AFTER_CREATE')
#PRIORITY(6000),DESCRIPTION('DCT Code')
#INSERT(%TriggerGenerateCodeDCTCode,'AFTER_CREATE')
#PRIORITY(7000),DESCRIPTION('End Scope')
#INSERT(%TriggerGenerateCodeEnd,'AFTER_CREATE')
#ENDAT
#!
#!
#AT(%ClassVirtualMethodRoutineCodeSection, , 'PostCreate', '(*CSTRING ErrCode,*CSTRING ErrMsg),BYTE'),WHERE(~%AliasFile)
#INSERT(%TriggerGenerateCodeRoutine,'AFTER_CREATE')
#ENDAT
#!
#!
#!-----------------------------------
#! Triggers support end
#!-----------------------------------
#!
#AT(%TriggerManagerCodeSection,,'PreInsert','(SIGNED OpCode,UNSIGNED AddLen,*CSTRING ErrCode,*CSTRING ErrMsg),BYTE'),PRIORITY(101),WHERE(~%AliasFile AND %TriggersSupportEnableGlobal)
#INSERT(%GenerateIdentityColumnSetting)
#ENDAT
#!
#!------------------------------------------------------------------------------
#!
#! End of CW #APPLICATION Template
#!
#!------------------------------------------------------------------------------
#!
#!
#!
#GROUP(%GenerateIdentityColumnSetting),AUTO
#DECLARE(%lFileCount)
#DECLARE(%FileIdentityField)
#DECLARE(%FileIdentityFieldInstance)
#DECLARE(%FileIdentityFieldColumn,LONG)
#SET(%FileIdentityField,'')
#SET(%FileIdentityFieldInstance,0)
#SET(%FileIdentityFieldColumn,1)
#SET(%lFileCount,0)
#FOR(%Field),WHERE(%FieldType<>'MEMO' AND %FieldType<>'BLOB' AND %FieldType<>'END')
      #SET(%lFileCount,%lFileCount+1)
      #IF((EXTRACT(UPPER(%FieldUserOptions), UPPER('IsIdentity'),1) = '1' OR UPPER(EXTRACT(UPPER(%FieldUserOptions), UPPER('IsIdentity'),1)) = 'TRUE') AND %FieldType<>'GROUP')
         #IF(%FileIdentityFieldInstance=0)
            #SET(%FileIdentityField,%Field)
            #SET(%FileIdentityFieldInstance,%lFileCount)
            #SET(%FileIdentityFieldColumn,1)
         #ELSE
            #ERROR('Only one Identity column per File is allowed - Second instance (File:'&%File&' - Column:'&%Field&')')
         #ENDIF
      #ENDIF
#ENDFOR
#IF(%FileIdentityFieldInstance<>0)
%File{PROP:ServerAutoInc}
%File{PROP:ServerAutoInc,%FileIdentityFieldInstance}=%FileIdentityFieldColumn !%FileIdentityField
   #FOR(%Key),WHERE(%KeyAuto=%FileIdentityField)
      #ERROR('An Identity Column can not belong to an Auto Number Key (Key:'&%Key&' - Column:'&%FileIdentityField&')')
      #ERROR('Set the Auto Number option to false.')
An Identity Column can not belong to an Auto Number Key (Key:%Key - Column:%FileIdentityField)
Set the auto Number option to false.
   #ENDFOR
#ELSE
   #FOR(%Field),WHERE((EXTRACT(UPPER(%FieldUserOptions), UPPER('ServerAutoIncColumn'),1) <> '') AND %FieldType<>'MEMO' AND %FieldType<>'BLOB' AND %FieldType<>'GROUP')
%File{PROP:ServerAutoInc}
      #BREAK
   #ENDFOR
   #SET(%lFileCount,0)
   #FOR(%Field),WHERE(%FieldType<>'MEMO' AND %FieldType<>'BLOB' AND %FieldType<>'END')
      #SET(%lFileCount,%lFileCount+1)
      #IF(EXTRACT(UPPER(%FieldUserOptions), UPPER('ServerAutoIncColumn'),1) <> '')
         #IF(%FieldType<>'GROUP')
            #SET(%FileIdentityField,%Field)
            #SET(%FileIdentityFieldInstance,%lFileCount)
            #SET(%FileIdentityFieldColumn,EXTRACT(UPPER(%FieldUserOptions), UPPER('ServerAutoIncColumn'),1))
%File{PROP:ServerAutoInc,%FileIdentityFieldInstance}=%FileIdentityFieldColumn !%FileIdentityField
            #FOR(%Key),WHERE(%KeyAuto=%FileIdentityField)
               #ERROR('A Server Auto Inc Column can not belong to an Auto Number Key (Key:'&%Key&' - Column:'&%FileIdentityField&')')
               #ERROR('Set the Auto Number option to false.')
A Server Auto Inc Column can not belong to an Auto Number Key (Key:%Key - Column:%FileIdentityField)
Set the auto Number option to false.
            #ENDFOR
         #ENDIF
      #ENDIF
   #ENDFOR
#ENDIF
#!
#!
#!
#!
#!
#PROCEDURE(External,'External Procedure','External'),HLP('~TPLProcExternal')
#AT(%CustomGlobalDeclarations)
  #INSERT(%FileControlSetFlags)
#ENDAT
#!PROMPT('Only One Instance Allowed of the Procedure',CHECK),%ProcedureOneInstance,AT(10)
#!AT(%GlobalData),WHERE(%ProcedureOneInstance)
#!CALL(%AddCustomGlobalData, 'GLO:oneInstance_'&%Procedure&'_thread', 'LONG,EXTERNAL,DLL')
#!ENDAT
#!
#DEFAULT
NAME DefaultExternalProcedure
[COMMON]
DESCRIPTION 'External Procedure'
FROM Clarion External
#ENDDEFAULT
#!
#PROCEDURE(Source,'Source Procedure'),HLP('~TPLProcSource')#! Source procedure
#COMMENT(60)
#BOXED('Source properties'),AT(10,,180)
#PROMPT('Generate Open/Close Files Routines',CHECK),%GenerateOpenClose,DEFAULT(%False),AT(10)
#ENDBOXED
#IF(%Parameters)                                  #! IF Parameters used
%[20]Procedure %ProcedureType %Parameters         #<! Declare Procedure
#ELSE                                             #! ELSE (IF parameters not used)
%[20]Procedure %ProcedureType                     #<! Declare Procedure
#ENDIF                                            #! END (IF parameters used)
#EMBED(%GatherSymbols,'Gather Template Symbols'),HIDE
#INSERT(%FileControlInitialize)
#AT(%CustomGlobalDeclarations)
  #INSERT(%FileControlSetFlags)
#ENDAT
#FOR(%LocalData)
#IF(LEFT(%LocalDataStatement,6)='&CLASS')
#SET(%ValueConstruct,EXTRACT(%LocalDataStatement,'&CLASS',1))
#IF(NOT %ValueConstruct)
    #SET(%ValueConstruct,'CLASS')
#ENDIF
%[20]LocalData &%ValueConstruct #<!%LocalDataDescription
#ELSE
%[20]LocalData %LocalDataStatement #<!%LocalDataDescription
#ENDIF
#ENDFOR
#EMBED(%DataSection,'Data Section'),DATA,TREE('Data Section{{PRIORITY(1000)}')          #! Embedded Source Code
  CODE                                            #<! Begin processed code
#EMBED(%ProcessedCode,'Processed Code'),DATA,LABEL,TREE('Processed Code{{PRIORITY(2000)}') #! Embedded Source Code
#EMBED(%ProcedureRoutines,'Procedure Routines'),DATA,LABEL,TREE('Procedure Routines{{PRIORITY(3000)}')
#EMBED(%LocalProcedures,'Local Procedures'),DATA,LABEL,TREE('Local Procedures{{PRIORITY(4000)}')
#AT(%DataSection),WHERE(%GenerateOpenClose)
FilesOpened     BYTE(0)
#ENDAT
#AT(%ProcedureRoutines),WHERE(%GenerateOpenClose)
!--------------------------------------
OpenFiles  ROUTINE
  PUSHBIND
  #INSERT(%FileControlOpen)
!--------------------------------------
CloseFiles ROUTINE
  #INSERT(%FileControlClose)
  POPBIND
BindFields ROUTINE
#ENDAT
#!
#!
#DEFAULT
NAME DefaultSourceProcedure
[COMMON]
DESCRIPTION 'Source Procedure'
FROM Clarion Source
#ENDDEFAULT
#INCLUDE('Program.TPW')                           #! The CW program template
#INCLUDE('Window.TPW')                            #! G/P window handler
#INCLUDE('Browse.TPW')                            #! Browse records from a file
#INCLUDE('Form.TPW')                              #! View / Edit a file record
#INCLUDE('Frame.TPW')                             #! MDI Frame
#INCLUDE('Menu.TPW')                              #! Non-MDI menu procedure
#INCLUDE('Viewer.TPW')                            #! ASCII file viewer
#INCLUDE('Process.TPW')                           #! Record Processing Proc
#INCLUDE('Report.TPW')                            #! Record reporting proc
#INCLUDE('Splash.TPW')                            #! Splash Window proc
#INCLUDE('ToDo.TPW')                              #! Under Construction proc
#INCLUDE('Relation.TPW')                          #! Relational Integrity Code
#INCLUDE('Standard.TPW')                          #! Standard Group code
#INCLUDE('FileCtrl.TPW')                          #! Standard File Control code
#INCLUDE('File.TPW')                              #! Global Data and File declaration code
#INCLUDE('Code.TPW')                              #! Code Templates
#INCLUDE('CtlBrow.TPW')                           #! Browse control template(s)
#INCLUDE('CtlView.TPW')                           #! ASCII viewer control template
#INCLUDE('CtlOLE.TPW')                            #! OLE Controls
#INCLUDE('CtlHTML.TPW')                           #! HTML Control
#INCLUDE('RelTree.TPW')
#INCLUDE('Control.TPW')                           #! Control Templates
#INCLUDE('Extens.TPW')                            #! Extension Templates
#INCLUDE('Field.TPW')                             #! Generic Field Handling
#INCLUDE('Module.TPW')
#INCLUDE('StdFunc.TPW')
#INCLUDE('BuildExp.TPW')
#INCLUDE('BuildShp.TPW')
#INCLUDE('TplError.TPW')
#INCLUDE('cwRTF.TPW')                             #! RTF Template
#INCLUDE('cwHH.TPW')                              #! HTML Help Template
#INCLUDE('SVFnGrp.TPW')
#INCLUDE('SVUSortOrder.tpw')
#INCLUDE('ENHANCED.TPW')
#INCLUDE('PROCBIND.TPW')
#INCLUDE('ABOOP.TPW')
#INCLUDE('QCENTER.TPW')
#INCLUDE('BrwExt.TPW')
#INCLUDE('ABOOPU.TPW')
#INCLUDE('REBASEDLL.TPW')
#INCLUDE('UTIL.TPW')
#INCLUDE('xmlsprt.tpw')
#INCLUDE('rtfctl.tpw')
#INCLUDE('HelpUtil.tpw')
#INCLUDE('CWBLOB.TPW')                            #! Blob support
#INCLUDE('BLOBSRV.TPW')                           #! Blob service support
#INCLUDE('CWMDISync.tpw')
#INCLUDE('MDISync.tpw')
#INCLUDE('CWtriggers.tpw')
#INCLUDE('VistaManifest.tpw')
#INCLUDE('tutil.tpw')
#INCLUDE('abthread.tpw')