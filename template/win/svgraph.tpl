#TEMPLATE(SVGraph, 'SVGraph'), family('ABC','CW20')
#HELP('ClarionHelp.chm')
#SYSTEM
#EQUATE(%equSVGraphVersion, '1.1.003')
#EQUATE(%equSVDefault, 'Default')
#EQUATE(%equSVTrue, 'True')
#EQUATE(%equSVFalse, 'False')
#EQUATE(%equSVYesNo, 'Yes[' & %equSVTrue & ']|No[' & %equSVFalse & ']')
#EQUATE(%equSVDefaultYesNo, %equSVDefault &'|'& %equSVYesNo)
#EQUATE(%equSVTitleBgrColor, 0808080H)
#EQUATE(%equSVTitleFontColor, 0FFFFFFH)
#EQUATE(%equSVTextSetControl,'Set the control')
#EQUATE(%equSVTextSetZone,'Set the zone')
#!=======================================================================================
#EXTENSION(SVGraphGlobal, 'Graph Global Extension'), application, HLP('~TPLExtensionSVGraphGlobal')
#PREPARE
  #DECLARE(%Yep, long)
  #SET(%Yep,0)
  #CALL(%ReadABCFiles(ABC))                         #! Loads ABC Classes if not already loaded
  #FOR(%pClassName)
    #IF(upper(sub(%pClassName,1,10))=upper('GraphClass'))
      #SET(%Yep,1)
      #BREAK
    #ENDIF
  #ENDFOR
  #IF(%Yep=0)
    #CALL(%ForceReadABCFiles(ABC))                  #! The net Classes might have been suppressed
  #ENDIF
#ENDPREPARE
  #BUTTON('ABC Library file'), AT(,,170), HLP('~TPLGraph_ABCLibrary')
    #PROMPT('Graph', FROM(%pClassName)), %GraphType, DEFAULT('GraphClass'), REQ
    #BOXED('ABC Library Files')
      #INSERT(%ABCLibraryPrompts(ABC))
    #ENDBOXED
  #ENDBUTTON
#!
#AT(%BeforeGenerateApplication), WHERE(%IsAppTemplate('ABC'))
  #CALL(%AddCategory(ABC),'GRAPH')
  #CALL(%SetCategoryLocationFromPrompts(ABC),'GRAPH','GRAPH','')
#ENDAT
#!
#AT(%BeforeGenerateApplication), WHERE(%IsAppTemplate('CLARION'))
  #pdefine('_ABCDllMode_',0)
  #pdefine('_ABCLinkMode_',1)
#ENDAT
#! **************************************************************************************
#CONTROL(GraphReturnButton,'Return to the Graph Control'), window, REQ(Graph), HLP('~TPLControlGraphReturnButton')
  #PROMPT('Control To Return From:',CONTROL),%ReturnFromControl, REQ
  #BOXED('')
    #DISPLAY('This will hide the selected control')
    #DISPLAY('and will generate the call to the')
    #DISPLAY('method '&%ThisObjectName&'.UnHide()')
  #ENDBOXED
  CONTROLS
    BUTTON('Return to Graph'),AT(,,66,14),USE(?ReturnToGraph)
  END
#ATSTART
  #DECLARE(%RefreshGraphControl)
  #CALL(%GetControlName),%RefreshGraphControl
#ENDAT
#!
#AT(%ControlEventHandling,%RefreshGraphControl,'Accepted'),PRIORITY(5000)
hide(%ReturnFromControl)
hide(%RefreshGraphControl)
  #FOR(%GraphDrillDownControlsHide)
unhide(%GraphDrillDownControlsHide)
  #ENDFOR
  #FOR(%GraphDrillDownControlsUNHide)
hide(%GraphDrillDownControlsUNHide)
  #ENDFOR
%ThisObjectName.UnHide()
#ENDAT
#!
#AT(%GraphMethodCodeSection,%ActiveTemplateParentInstance,'DrillDown','(long parGraphNpp,string parGraphName,long parNodeNpp,string parNodeName,real parNodeX,real parNodeY)'),PRIORITY(4800)
!unhide(%RefreshGraphControl)
#ENDAT
#! ********************************************************************************
#CONTROL(GraphRefreshButton,'Refresh the Graph Control'), window, REQ(Graph), HLP('~TPLControlGraphRefreshButton')
  #BOXED('')
    #DISPLAY('This will generate the call to the')
    #DISPLAY('method '&%ThisObjectName&'.Refresh(true)')
  #ENDBOXED
  CONTROLS
    BUTTON('Refresh Graph'),AT(,,57,14),USE(?RefreshGraph)
  END
#ATSTART
  #DECLARE(%RefreshGraphControl)
  #CALL(%GetControlName),%RefreshGraphControl
#ENDAT
#!
#AT(%ControlEventHandling,%RefreshGraphControl,'Accepted'),PRIORITY(5000)
%ThisObjectName.Refresh(true)
#ENDAT
#! ********************************************************************************
#CONTROL(GraphPrintButton,'Print the Graph Control'), window, REQ(Graph), HLP('~TPLGraph_GraphPrintButton')
  #BOXED('')
    #DISPLAY('This will generate the call to the')
    #DISPLAY('method '&%ThisObjectName&'.PrintGraph()')
  #ENDBOXED
  CONTROLS
    BUTTON('Print Graph'),AT(,,57,14),USE(?PrintGraph)
  END
#ATSTART
  #DECLARE(%PrintGraphControl)
  #CALL(%GetControlName),%PrintGraphControl
#ENDAT
#!
#AT(%ControlEventHandling,%PrintGraphControl,'Accepted'),PRIORITY(5000)
%ThisObjectName.PrintGraph()
#ENDAT
#! ********************************************************************************
#CODE(GraphRefreshCode, 'Refresh the Graph Control by Code'), REQ(Graph), HLP('~TPLGraph_GraphRefreshCode')
  #BOXED('')
    #DISPLAY('This will generate the call to the')
    #DISPLAY('method '&%ThisObjectName&'.Refresh(true)')
  #ENDBOXED
%ThisObjectName.Refresh(true)
#!
#CODE(GraphRefreshCodeReport, 'Refresh the Graph Control by Code'), REQ(GraphReport), HLP('~TPLGraph_GraphRefreshCodeReport')
  #BOXED('')
    #DISPLAY('This will generate the call to the')
    #DISPLAY('method '&%ThisObjectName&'.Refresh(true)')
    #PROMPT('Init the Object before refresh',check), %GraphInitBeforeRefresh, DEFAULT(%True),AT(10)
  #ENDBOXED
#IF(%GraphInitBeforeRefresh)
  #CALL(%InsertProc,%ThisObjectName & '.Init','!'&%Report,'!'&%LocalControlName,%GraphIndentLeft,%GraphIndentTop,%GraphIndentRight,%GraphIndentBottom)
#ENDIF
%ThisObjectName.Refresh(true)
#!
#! **************************************************************************************
#! the #CONTROL for structures REPORT
#! **************************************************************************************
#!
#CONTROL(GraphReport,'Graph Control'), DESCRIPTION('Graph Control ' & %ActiveTemplateInstance &' - '& slice(%LocalControlName,2,len(%LocalControlName)) ), report, MULTI, REQ(SVGraphGlobal), PRIMARY(%ThisObjectName,OPTIONAL), HLP('~TPLControlGraphReport')
#PREPARE
  #CALL(%ReadABCFiles(ABC))
  #CALL(%SetClassDefaults(ABC), 'GRP' & %ActiveTemplateInstance,'GRP' & %ActiveTemplateInstance, %GraphType)
  #CALL(%GetControlName),%LocalControlname
#ENDPREPARE
  CONTROLS
    GROUP,USE(?GraphReport),BOXED
    END
  END
#INSERT(%GraphControlPrompts)
#!
#ATSTART
  #CALL(%GraphControlAtStart)
#ENDAT
#!
#! For CLARION templates (Data) ---------------------------------------------------------
#AT(%LocalDataClasses), WHERE(%IsAppTemplate('CLARION'))
  #FOR(%GraphDataSources), WHERE(%GraphDataSource='File')
    #CALL(%StandardViewFilter(Clarion),%SortRecordFilter,%PrimaryKey,%SortRangeField,%SortRangeLimitType,'Graph:Save:'&%ActiveTemplateInstance,%Temp1,%Temp2,'Declare')
  #ENDFOR
#ENDAT
#!
#AT(%DataSectionBeforeWindow), WHERE(%IsAppTemplate('CLARION') AND %ListView)
  #CALL(%ConstructView(ABC))
  #CALL(%FileControlInitialize(Clarion))
#ENDAT
#!
#! For CLARION templates (Code) ---------------------------------------------------------
#AT(%BeforeWindowClosing),PRIORITY(4000),WHERE(%IsAppTemplate('CLARION'))
%ThisObjectName.Kill()
#ENDAT
#!
#AT(%BeforeBind)
#CALL(%GraphControlInsertBind)
#ENDAT
#AT(%WindowEventHandling,'OpenWindow'),WHERE(%IsAppTemplate('CLARION') AND %ListView)
#INSERT(%StandardViewFilter(Clarion),%SortRecordFilter,%PrimaryKey,%SortRangeField,%SortRangeLimitType,'Graph:Save:'&%ActiveTemplateInstance,%Temp1,%Temp2,'Bind')
#ENDAT
#!AT(%AfterFileOpen),PRIORITY(5000),WHERE(%IsAppTemplate('CLARION') AND %ListView)
#!ENDAT
#!
#AT(%ReportBeforeLookups),PRIORITY(7500),WHERE(%IsAppTemplate('CLARION'))
  #IF(%GraphAutoRefresh)
%ThisObjectName.Init(%Report,%LocalControlName,%GraphIndentLeft,%GraphIndentTop,%GraphIndentRight,%GraphIndentBottom)
  #ENDIF
#ENDAT
#!
#AT(%BeforePrint),PRIORITY(7500),WHERE(%IsAppTemplate('CLARION'))
  #IF(%GraphAutoRefresh)
%ThisObjectName.Refresh(true)
  #ENDIF
#ENDAT
#!
#AT(%PrepareAlerts),PRIORITY(8600),WHERE(%IsAppTemplate('CLARION'))
#INSERT(%AddGraphPointBreakBreaksClassInitialisation)
#ENDAT
#!
#! For ABC templates (Code) -------------------------------------------------------------
#AT(%GatherObjects), WHERE(%IsAppTemplate('ABC'))
  #ADD(%ObjectList,'GRP' & %ActiveTemplateInstance)
#ENDAT
#!
#AT(%LocalProcedures)
  #CALL(%GenerateVirtuals(ABC), 'GRP' & %ActiveTemplateInstance, 'Local Objects|Graph Objects|'&%ThisObjectName, '%GraphManagerVirtuals(SVGraph)')
#ENDAT
#!
#AT(%DataSection),PRIORITY(1300), WHERE(%IsAppTemplate('ABC') AND %ListView)
  #CALL(%ConstructView(ABC))
#ENDAT
#!
#AT(%LocalDataClasses),WHERE(%IsAppTemplate('ABC'))
  #FOR(%GraphDataSources),WHERE(%GraphDataSource='File')
%[20](%GraphVMname()) %ViewManagerType
  #ENDFOR
#ENDAT
#!
#AT(%WindowManagerMethodCodeSection,'Kill','(),BYTE'),PRIORITY(2500),WHERE(%IsAppTemplate('ABC'))
%ThisObjectName.Kill()
#ENDAT
#!
#AT(%WindowManagerMethodCodeSection,'Init','(),BYTE'),PRIORITY(8030),WHERE(%IsAppTemplate('ABC'))
  #CALL(%GrapInsertCodeVMinit)
#ENDAT
#!
#AT(%WindowManagerMethodCodeSection,'Init','(),BYTE'),PRIORITY(8040),WHERE(%IsAppTemplate('ABC'))
#INSERT(%AddGraphPointBreakBreaksClassInitialisation)
#ENDAT
#!
#AT(%ProcessManagerMethodCodeSection,'TakeRecord','(),BYTE'),PRIORITY(2500),WHERE(%IsAppTemplate('ABC'))
  #IF(%GraphAutoRefresh)
  #CALL(%InsertProc,%ThisObjectName & '.Init','!'&%Report,'!'&%LocalControlName,%GraphIndentLeft,%GraphIndentTop,%GraphIndentRight,%GraphIndentBottom)
  #ENDIF
#ENDAT
#!
#AT(%ProcessManagerMethodCodeSection,'TakeRecord','(),BYTE'),PRIORITY(5500),WHERE(%IsAppTemplate('ABC'))
  #IF(%GraphAutoRefresh)
%ThisObjectName.Refresh(true)
  #ENDIF
#ENDAT
#!---------------------------------------------------------------------------------------
#! Common
#!---------------------------------------------------------------------------------------
#AT(%CustomModuleDeclarations)
  #IF(%GraphHasBreaks())
    #ADD(%ModuleIncludeList,'ABBreak.inc')
  #ENDIF
#ENDAT
#!
#AT(%GraphMethodCodeSection,%ActiveTemplateInstance),PRIORITY(5000), DESCRIPTION('Parent Call')
  #CALL(%ParentCallValid(ABC)),%LocalParentCallValid
  #IF(%LocalParentCallValid)
    #CALL(%GenerateParentCall(ABC))
  #ENDIF
#ENDAT
#!
#AT(%LocalDataClasses)
  #CALL(%GenerateClass(ABC), 'GRP' & %ActiveTemplateInstance)
  #CALL(%AddGraphTotalsFieldsDeclaration)
  #CALL(%AddGraphPointBreakBreaksClassDeclaration)
#ENDAT
#!
#AT(%LocalProcedures)
  #CALL(%AddGraphPointBreakBreaksClassCode)
#ENDAT
#!
#AT(%GraphMethodCodeSection,%ActiveTemplateInstance,'SetDefault','()'),PRIORITY(6000)
#INSERT(%GraphControlAtSetDefault)
#ENDAT
#!
#AT(%GraphMethodCodeSection,%ActiveTemplateInstance,'BeginRefresh','(),bool'),PRIORITY(4500)
#INSERT(%GraphControlAtRefresh)
#ENDAT
#!
#AT(%NewMethodCodeSection,%ActiveTemplateInstance,%ClassItem),PRIORITY(500)
#INSERT(%GraphControlAtSerieEmbeds500)
#ENDAT
#!
#AT(%NewMethodCodeSection,%ActiveTemplateInstance,%ClassItem),PRIORITY(9000)
#INSERT(%GraphControlAtSerieEmbeds9000)
#ENDAT
#!
#AT(%NewMethodCodeSection,%ActiveTemplateInstance,%ClassItem),PRIORITY(4500)
#INSERT(%GraphControlAtSerieEmbeds45001)
#ENDAT
#!
#AT(%NewMethodCodeSection,%ActiveTemplateInstance,%ClassItem),PRIORITY(4500)
#INSERT(%GraphControlAtSerieEmbeds45002)
#ENDAT
#!
#AT(%NewMethodDataSection,%ActiveTemplateInstance,%ClassItem),PRIORITY(5000)
#INSERT(%GraphControlAtSerieEmbedsData5000)
#ENDAT
#!
#AT(%NewMethodCodeSection,%ActiveTemplateInstance,%ClassItem),PRIORITY(5000)
#INSERT(%GraphControlAtSerieEmbeds5000)
#ENDAT
#! **************************************************************************************
#! the #CONTROL for structures WINDOW
#! **************************************************************************************
#!
#CONTROL(Graph,'Graph Control'),DESCRIPTION('Graph Control ' & %ActiveTemplateInstance &' - '& slice(%LocalControlName,2,len(%localControlName)) ), window, MULTI, REQ(SVGraphGlobal), PRIMARY(%ThisObjectName,OPTIONAL), HLP('~TPLControlSVGraph')
#PREPARE
  #CALL(%ReadABCFiles(ABC))
  #CALL(%SetClassDefaults(ABC), 'GRP'&%ActiveTemplateInstance, 'GRP'&%ActiveTemplateInstance, %GraphType)
  #CALL(%GetControlName),%LocalControlname
#ENDPREPARE
   CONTROLS
     GROUP,AT(,,179,126),USE(?Graph),BOXED,BEVEL(-1,1)
     END
   END
#INSERT(%GraphControlPrompts)
#!
#ATSTART
  #INSERT(%GraphControlAtStart)
#ENDAT
#!
#! For CLARION templates (Data) ---------------------------------------------------------
#!
#AT(%LocalDataClasses), WHERE(%IsAppTemplate('CLARION'))
  #FOR(%GraphDataSources), WHERE(%GraphDataSource='File')
#INSERT(%StandardViewFilter(Clarion),%SortRecordFilter,%PrimaryKey,%SortRangeField,%SortRangeLimitType,'Graph:Save:'&%ActiveTemplateInstance,%Temp1,%Temp2,'Declare')
  #ENDFOR
#ENDAT
#!
#AT(%DataSectionBeforeWindow), WHERE(%IsAppTemplate('CLARION') AND %ListView)
  #CALL(%ConstructView(ABC))
  #CALL(%FileControlInitialize(Clarion))
#ENDAT
#! For CLARION templates (Code) ---------------------------------------------------------
#AT(%BeforeWindowClosing),PRIORITY(4000),WHERE(%IsAppTemplate('CLARION'))
%ThisObjectName.Kill()
#ENDAT
#!
#AT(%BeforeBind)
#CALL(%GraphControlInsertBind)
#ENDAT
#AT(%WindowEventHandling,'OpenWindow'),WHERE(%IsAppTemplate('CLARION'))
  #FOR(%GraphDataSources), WHERE(%GraphDataSource='File')
    #CALL(%StandardViewFilter(Clarion),%SortRecordFilter,%PrimaryKey,%SortRangeField,%SortRangeLimitType,'Graph:Save:'&%ActiveTemplateInstance,%Temp1,%Temp2,'Bind')
  #ENDFOR
#ENDAT
#!
#AT(%PrepareAlerts),PRIORITY(8500),WHERE(%IsAppTemplate('CLARION'))
%ThisObjectName.Init(%Window,%LocalControlName,%GraphIndentLeft,%GraphIndentTop,%GraphIndentRight,%GraphIndentBottom)          ! Initializing cGraph object
#ENDAT
#!
#AT(%PrepareAlerts),PRIORITY(8600),WHERE(%IsAppTemplate('CLARION'))
  #CALL(%AddGraphPointBreakBreaksClassInitialisation)
#ENDAT
#!
#AT(%RefreshWindowBeforeDisplay),PRIORITY(8500),WHERE(%IsAppTemplate('CLARION'))
  #IF(%GraphAutoRefresh)
%ThisObjectName.Refresh(true)
  #ENDIF
#ENDAT
#!
#AT(%AcceptLoopBeforeEventHandling),PRIORITY(4500),WHERE(%IsAppTemplate('CLARION'))
%ThisObjectName.TakeEvent()
#ENDAT
#! For ABC templates (Data) -------------------------------------------------------------
#AT(%DataSection),PRIORITY(1300),WHERE(%IsAppTemplate('ABC') AND %ListView)
  #CALL(%ConstructView(ABC))
#ENDAT
#!
#AT(%GatherObjects),WHERE(%IsAppTemplate('ABC'))
  #ADD(%ObjectList,'GRP' & %ActiveTemplateInstance)
#ENDAT
#!
#AT(%LocalDataClasses), WHERE(%IsAppTemplate('ABC'))
  #FOR(%GraphDataSources), WHERE(%GraphDataSource='File')
%[20](%GraphVMname()) %ViewManagerType
  #ENDFOR
#ENDAT
#! For ABC templates (Code) -------------------------------------------------------------
#AT(%LocalProcedures)
  #CALL(%GenerateVirtuals(ABC), 'GRP' & %ActiveTemplateInstance, 'Local Objects|Graph Objects|'&%ThisObjectName, '%GraphManagerVirtuals(SVGraph)')
#ENDAT
#!
#AT(%WindowManagerMethodCodeSection,'Kill','(),BYTE'),PRIORITY(2500),WHERE(%IsAppTemplate('ABC'))
%ThisObjectName.Kill()
#ENDAT
#!
#AT(%WindowManagerMethodCodeSection,'Reset','(BYTE Force=0)'),PRIORITY(8000),WHERE(%IsAppTemplate('ABC'))
  #IF(%GraphAutoRefresh)
%ThisObjectName.Refresh(true)
  #ENDIF
#ENDAT
#!
#AT(%WindowManagerMethodCodeSection,'Init','(),BYTE'),PRIORITY(8030),WHERE(%IsAppTemplate('ABC'))
  #CALL(%GrapInsertCodeVMinit)
#ENDAT
#!
#AT(%WindowManagerMethodCodeSection,'Init','(),BYTE'),PRIORITY(8040),WHERE(%IsAppTemplate('ABC'))
  #CALL(%AddGraphPointBreakBreaksClassInitialisation)
#ENDAT
#!
#AT(%WindowManagerMethodCodeSection,'Init','(),BYTE'),PRIORITY(8900),WHERE(%IsAppTemplate('ABC'))
  #CALL(%InsertProc,%ThisObjectName & '.Init','!'&%Window,'!'&%LocalControlName,%GraphIndentLeft,%GraphIndentTop,%GraphIndentRight,%GraphIndentBottom)
#ENDAT
#!
#AT(%WindowManagerMethodCodeSection,'TakeEvent','(),BYTE'),PRIORITY(3800)
%ThisObjectName.TakeEvent()
#ENDAT
#!---------------------------------------------------------------------------------------
#! Common
#!---------------------------------------------------------------------------------------
#AT(%CustomModuleDeclarations)
  #IF(%GraphHasBreaks())
    #ADD(%ModuleIncludeList,'ABBreak.inc')
  #ENDIF
#ENDAT
#!
#AT(%LocalDataClasses)
  #CALL(%GenerateClass(ABC), 'GRP' & %ActiveTemplateInstance)
  #CALL(%AddGraphTotalsFieldsDeclaration)
  #CALL(%AddGraphPointBreakBreaksClassDeclaration)
#ENDAT
#!
#AT(%LocalProcedures)
  #CALL(%AddGraphPointBreakBreaksClassCode)
#ENDAT
#!
#AT(%GraphMethodCodeSection,%ActiveTemplateInstance), PRIORITY(5000),DESCRIPTION('Parent Call')
  #CALL(%ParentCallValid(ABC)),%LocalParentCallValid
  #IF(%LocalParentCallValid)
    #CALL(%GenerateParentCall(ABC))
  #ENDIF
#ENDAT
#!
#AT(%GraphMethodCodeSection,%ActiveTemplateInstance,'SetDefault','()'),PRIORITY(6000)
#INSERT(%GraphControlAtSetDefault)
#ENDAT
#!
#AT(%GraphMethodCodeSection,%ActiveTemplateInstance,'BeginRefresh','(),bool'),PRIORITY(4500)
#INSERT(%GraphControlAtRefresh)
#ENDAT
#!
#AT(%NewMethodCodeSection,%ActiveTemplateInstance,%ClassItem),PRIORITY(500)
#INSERT(%GraphControlAtSerieEmbeds500)
#ENDAT
#!
#AT(%NewMethodCodeSection,%ActiveTemplateInstance,%ClassItem),PRIORITY(9000)
#INSERT(%GraphControlAtSerieEmbeds9000)
#ENDAT
#!
#AT(%NewMethodDataSection,%ActiveTemplateInstance,%ClassItem),PRIORITY(5000)
#INSERT(%GraphControlAtSerieEmbedsData5000)
#ENDAT
#!
#AT(%NewMethodCodeSection,%ActiveTemplateInstance,%ClassItem),PRIORITY(4500)
#INSERT(%GraphControlAtSerieEmbeds45001)
#ENDAT
#!
#AT(%NewMethodCodeSection,%ActiveTemplateInstance,%ClassItem),PRIORITY(4500)
#INSERT(%GraphControlAtSerieEmbeds45002)
#ENDAT
#!
#AT(%NewMethodCodeSection,%ActiveTemplateInstance,%ClassItem),PRIORITY(5000)
#INSERT(%GraphControlAtSerieEmbeds5000)
#ENDAT
#!
#AT(%GraphMethodCodeSection, %ActiveTemplateInstance, 'DrillDown','()'),PRIORITY(4500), WHERE(%GraphDrillDown)
  #FOR(%GraphDrillDownControlsHide)
HIDE(%GraphDrillDownControlsHide)
  #ENDFOR
  #FOR(%GraphDrillDownControlsUNHide)
UNHIDE(%GraphDrillDownControlsUNHide)
  #ENDFOR
#ENDAT
#!
#AT(%GraphMethodCodeSection, %ActiveTemplateInstance, 'ReturnFromDrillDown','()'),PRIORITY(4500), WHERE(%GraphReturnFromDrillDown)
  #FOR(%GraphReturnFromDrillDownControlsHide)
HIDE(%GraphReturnFromDrillDownControlsHide)
  #ENDFOR
  #FOR(%GraphReturnFromDrillDownControlsUNHide)
UNHIDE(%GraphReturnFromDrillDownControlsUNHide)
  #ENDFOR
#ENDAT
#!
#!------------------------------------------------------------------------------------------
#GROUP(%GraphControlAtRefresh)
! Graph Data
  #FOR(%GraphDataSources)
SELF.%(%GraphSeriesPrefix())_Process()
  #ENDFOR
  #CASE(%GraphAxisListGroupingSort)
  #OF('Name')
SORT(SELF.qCluster, %GraphAxisListGroupingSequence%('SELF.qCluster.eName'))
  #ELSE
SORT(SELF.qCluster, %GraphAxisListGroupingSequence%('SELF.qCluster.eID'))
  #ENDCASE
#!------------------------------------------------------------------------------------------
#GROUP(%GraphControlAtSetDefault)
  #! Diagram type
SELF.GraphType(GraphType:%GraphTypeInit, false)
SELF.eGraphSubType = GraphSubType:%GraphSubTypeInit
SELF.eGraphFigure = FigureType:%GraphFigureInit
  #CALL(%InsertYesNo,'SELF.oTitle.eHide','choose(~' & %GraphInitTitle & ')')
  #CALL(%InsertProc,'SELF.oTitle.SetText',%GraphTitle)
  #CALL(%InsertProc,'SELF.oTitle.SetFont',%GraphTitleFontName,%GraphTitleFontSize,%GraphTitleFontColor,%GraphTitleFontStyle,%GraphTitleFontChar)
  #IF(~%IsReport())
SELF.eWallpaper = Wallpaper:%GraphInitWallpaperStyle
    #IF(%GraphWallpaper)
SELF.eWallpaperFile = '%'GraphWallpaper'
    #ENDIF
  #ELSE
SELF.eWallpaper = Wallpaper:None
CLEAR(self.eWallpaperFile)
  #ENDIF
  #CALL(%InsertYesNo,'SELF.e3D',%GraphInit3D)                 #! 3D-effect
  #IF(%GraphInitZoom<>'Auto')
SELF.eZoom = %GraphInitZoom
  #ENDIF
  #CALL(%InsertYesNo,'SELF.eAxisListAutoSwap',%GraphInitAxisListAutoSwap)
SELF.oLegend.ePosition = LegendPosition:%GraphLegendInitPos
  #CALL(%InsertYesNo,'SELF.oLegend.eBox',%GraphInitLegendBox)
SELF.eAxisListStyle = AxisListStyle:%GraphInitAxisListStyle
  #CALL(%InsertYesNo,'SELF.eAxisListGrid',%GraphInitGrid)
  #CALL(%InsertYesNo,'SELF.eAxisListScaleMinMax',%GraphAxisListScaleMinMax)
SELF.eAxisListScale = Scale:%GraphAxisListScale
  #IF(%GraphAxisXThickness<>0)
SELF.eAxisXThickness = %GraphAxisXThickness
  #ENDIF
  #IF(%GraphAxisYThickness<>0)
SELF.eAxisYThickness = %GraphAxisYThickness
  #ENDIF
  #IF(%GraphAxisXColor<>-1)
SELF.eAxisXColor = %GraphAxisXColor
  #ENDIF
  #IF(%GraphAxisYColor<>-1)
SELF.eAxisYColor = %GraphAxisYColor
  #ENDIF
  #IF(%GraphAxisXScaleInterval<>0)
SELF.eAxisXScaleInterval = abs(SELF.ToW(%GraphAxisXScaleInterval)) ! Distance between strokes of an Axis X in units of the device
  #ENDIF
  #IF(%GraphAxisYScaleInterval<>0)
SELF.eAxisYScaleInterval = abs(SELF.ToH(%GraphAxisYScaleInterval)) ! Distance between strokes of an Axis Y in units of the device
  #ENDIF
  #IF(%GraphXAxisFormat)
SELF.eAxisXFormat = %(%StripPling(%GraphXAxisFormat))
  #ENDIF
  #CALL(%InsertYesNo,'SELF.eAxisXname',%GraphInitAxisListName)
  #CALL(%InsertProc,'SELF.oAxisXName.SetText',%GraphXAxisName)
  #CALL(%InsertProc,'SELF.oAxisXName.SetFont',%GraphXAxisNameFontName,%GraphXAxisNameFontSize,%GraphXAxisNameFontColor,%GraphXAxisNameFontStyle,%GraphXAxisNameFontChar)
  #CALL(%InsertYesNo,'SELF.eAxisXGrid',%GraphInitAxisXGrid)
  #IF(%GraphYAxisFormat)
SELF.eAxisYFormat = %(%StripPling(%GraphYAxisFormat))
  #ENDIF
  #CALL(%InsertYesNo,'SELF.eAxisYname',%GraphInitAxisListName)
  #CALL(%InsertProc,'SELF.oAxisYName.SetText',%GraphYAxisName)
  #CALL(%InsertProc,'SELF.oAxisYName.SetFont',%GraphYAxisNameFontName,%GraphYAxisNameFontSize,%GraphYAxisNameFontColor,%GraphYAxisNameFontStyle,%GraphYAxisNameFontChar)
  #CALL(%InsertYesNo,'SELF.eAxisYGrid',%GraphInitAxisYGrid)
SELF.eNodeType = NodeType:%GraphInitNodeType
  #IF(%GraphNodeRadius<>0)                                    #! If %GraphNodeRadius=0 then eNodeRadius is default
SELF.eNodeRadius = %GraphNodeRadius
  #ENDIF
  #CALL(%InsertYesNo,'SELF.eNodeMinMax',%GraphInitNodeMinMax)
  #CALL(%InsertYesNo,'SELF.eNodeLabel',%GraphNodeLabel)
  #CALL(%InsertYesNo,'SELF.eNodeValue',%GraphNodeValue)
  #CALL(%InsertYesNo,'SELF.eNodeBgr',%GraphNodeBgr)
  #CALL(%InsertYesNo,'SELF.eBestPositionNodeText',%GraphInitBestPositionNodeText)
  #IF(~%IsReport())
    #CALL(%InsertTrueFalse, 'SELF.eSaveFileNameAlwaysExt', %GraphFileNameForSaveAlwaysExt)
    #IF(%GraphFileNameForSave)
SELF.eSaveFileName = %(%StripPling(%GraphFileNameForSave))
    #ENDIF
  #ENDIF
  #!
  #CALL(%InsertPrintOptions)
  #CALL(%InsertDisplayOptions)
#!------------------------------------------------------------------------------------------
#GROUP(%InsertPrintOptions), AUTO
  #IF(%IsReport())
    #RETURN
  #ENDIF
  #CALL(%InsertYesNo,'SELF.gPrint.ePreview',%GraphInitPrintPreview)
  #CASE(%GraphInitPrintOrientation)
  #OF('1')                                          #! Portrait
SELF.gPrint.eOrientation = equ:Portrait
  #OF('2')                                          #! Landscape
SELF.gPrint.eOrientation = equ:Landscape
  #ELSE                                             #! Orientation same as on the printer
SELF.gPrint.eOrientation = equ:Auto
  #ENDCASE
  #CALL(%InsertYesNo,'SELF.gPrint.eBox',%GraphInitPrintBox)
  #IF(%GraphInitPrintLeft)
SELF.gPrint.eL = %GraphInitPrintLeft
  #ENDIF
  #IF(%GraphInitPrintTop)
SELF.gPrint.eT = %GraphInitPrintTop
  #ENDIF
  #IF(%GraphInitPrintRight)
SELF.gPrint.eR = %GraphInitPrintRight
  #ENDIF
  #IF(%GraphInitPrintBottom)
SELF.gPrint.eB = %GraphInitPrintBottom
  #ENDIF
  #! "Print best fit"
  #IF(%GraphInitPrintAlignH OR %GraphInitPrintAlignV)
SELF.gPrint.eAlignment = Alignment:%GraphInitPrintAlignH + Alignment:%GraphInitPrintAlignV
  #ENDIF
  #CALL(%InsertYesNo,'SELF.gPrint.eProportional',%GraphInitPrintProportional)
SELF.gPrint.eStretch =  %(%GraphPrintStretch())
#!------------------------------------------------------------------------------------------
#GROUP(%InsertDisplayOptions), AUTO
  #IF(%IsReport())
    #RETURN
  #ENDIF
  #CALL(%InsertPopupOptions)
  #IF(%GraphAccessibleDrillDown)
    #IF(%GraphDrillDown AND %GraphDrillDownControl)
SELF.eFDrillDown = %GraphDrillDownControl
    #ENDIF
    #IF(%GraphReturnFromDrillDown AND %GraphReturnFromDrillDownControl)
SELF.eFReturnFromDrillDown = %GraphReturnFromDrillDownControl
    #ENDIF
  #ENDIF
  #CALL(%InsertYesNo,'SELF.eToolTip',choose(~%GraphAccessibleTooltip, %equSVFalse, %GraphInitToolTip))
  #IF(%GraphAccessibleTooltip)
    #CALL(%InsertYesNo,'SELF.eBestPositionToolTip', choose(~%GraphAccessibleTooltip, %equSVFalse, %GraphInitBestPositionToolTip))
    #CALL(%InsertYesNo,'SELF.gToolTipProp.eTrn', choose(~%GraphAccessibleTooltip, %equSVFalse, %GraphInitToolTipTrn))
    #IF(%GraphInitToolTipBgr <> -1 AND %GraphInitToolTipTrn=%equSVFalse)
SELF.gToolTipProp.eBgrColor = %GraphInitToolTipBgr
    #ENDIF
    #IF(%GraphInitToolTipCorrectionOfWidth)
SELF.gToolTipProp.CorrectionOfWidth = %GraphInitToolTipCorrectionOfWidth
    #ENDIF
    #IF(%GraphInitToolTipCorrectionOfHeight)
SELF.gToolTipProp.CorrectionOfHeight = %GraphInitToolTipCorrectionOfHeight
    #ENDIF
  #ENDIF
  #! To show
CLEAR(SELF.gShowMouse)
CLEAR(SELF.gShowMouseX)
CLEAR(SELF.gShowMouseY)
CLEAR(SELF.gShowDiagramName)
CLEAR(SELF.gShowDiagramNameV)
CLEAR(SELF.gShowNodeName)
CLEAR(SELF.gShowNodeNameV)
CLEAR(SELF.gShowNodeValue)
CLEAR(SELF.gShowNodeValueX)
CLEAR(SELF.gShowNodeValueY)
  #! Mouse ------------------------------------------------------------------------------
  #CALL(%InsertTrue, 'SELF.gShowMouse.eOnT', %GraphShowMouseOnTooltip)
  #CALL(%InsertTrue, 'SELF.gShowMouse.eOnW', %GraphShowMouseOnWindow)
  #IF(%GraphShowMouseOnControl AND %GraphShowMouseControl)
SELF.gShowMouse.eOnF = %GraphShowMouseControl
  #ENDIF
  #IF(%GraphShowMouseOnStatusBar AND %GraphShowMouseZone)
SELF.gShowMouse.eOnS = %GraphShowMouseZone
  #ENDIF
  #IF(%GraphShowMouseXOnControl AND %GraphShowMouseXControl)
SELF.gShowMouseX.eOnF = %GraphShowMouseXControl
  #ENDIF
  #IF(%GraphShowMouseYOnControl AND %GraphShowMouseYControl)
SELF.gShowMouseY.eOnF = %GraphShowMouseYControl
  #ENDIF
  #IF(%GraphShowMouseXOnStatusBar AND %GraphShowMouseXZone)
SELF.gShowMouseX.eOnS = %GraphShowMouseXZone
  #ENDIF
  #IF(%GraphShowMouseYOnStatusBar AND %GraphShowMouseYZone)
SELF.gShowMouseY.eOnS = %GraphShowMouseYZone
  #ENDIF
  #! Diagram ----------------------------------------------------------------------------
  #CALL(%InsertTrue, 'SELF.gShowDiagramName.eOnT', %GraphShowDiagramOnTooltip)
  #CALL(%InsertTrue, 'SELF.gShowDiagramName.eOnW', %GraphShowDiagramOnWindow)
  #IF(%GraphShowDiagramOnControl AND %GraphShowDiagramControl)
SELF.gShowDiagramName.eOnF = %GraphShowDiagramControl
  #ENDIF
  #IF(%GraphShowDiagramOnStatusBar AND %GraphShowDiagramZone)
SELF.gShowDiagramName.eOnS = %GraphShowDiagramZone
  #ENDIF
  #IF(%GraphShowDiagramVOnControl AND %GraphShowDiagramVControl)
SELF.gShowDiagramNameV.eOnF = %GraphShowDiagramVControl
  #ENDIF
  #IF(%GraphShowDiagramVOnStatusBar AND %GraphShowDiagramVZone)
SELF.gShowDiagramNameV.eOnS = %GraphShowDiagramVZone
  #ENDIF
  #! Node Name --------------------------------------------------------------------------
  #CALL(%InsertTrue, 'SELF.gShowNodeName.eOnT', %GraphShowNodeNameOnTooltip)
  #CALL(%InsertTrue, 'SELF.gShowNodeName.eOnW', %GraphShowNodeNameOnWindow)
  #IF(%GraphShowNodeNameOnControl AND %GraphShowNodeNameControl)
SELF.gShowNodeName.eOnF = %GraphShowNodeNameControl
  #ENDIF
  #IF(%GraphShowNodeNameOnStatusBar AND %GraphShowNodeNameZone)
SELF.gShowNodeName.eOnS = %GraphShowNodeNameZone
  #ENDIF
  #IF(%GraphShowNodeNameVOnControl AND %GraphShowNodeNameVControl)
SELF.gShowNodeNameV.eOnF = %GraphShowNodeNameVControl
  #ENDIF
  #IF(%GraphShowNodeNameVOnStatusBar AND %GraphShowNodeNameVZone)
SELF.gShowNodeNameV.eOnS = %GraphShowNodeNameVZone
  #ENDIF
  #! Node Value -------------------------------------------------------------------------
  #CALL(%InsertTrue, 'SELF.gShowNodeValue.eOnT', %GraphShowNodeValueOnTooltip)
  #CALL(%InsertTrue, 'SELF.gShowNodeValue.eOnW', %GraphShowNodeValueOnWindow)
  #IF(%GraphShowNodeValueOnControl AND %GraphShowNodeValueControl)
SELF.gShowNodeValue.eOnF = %GraphShowNodeValueControl
  #ENDIF
  #IF(%GraphShowNodeValueOnControl AND %GraphShowNodeValueControl)
SELF.gShowNodeValue.eOnF = %GraphShowNodeValueControl
  #ENDIF
  #IF(%GraphShowNodeValueOnStatusBar AND %GraphShowNodeValueZone)
SELF.gShowNodeValue.eOnS = %GraphShowNodeValueZone
  #ENDIF
  #IF(%GraphShowNodeValueXOnControl AND %GraphShowNodeValueXControl)
SELF.gShowNodeValueX.eOnF = %GraphShowNodeValueXControl
  #ENDIF
  #IF(%GraphShowNodeValueYOnControl AND %GraphShowNodeValueYControl)
SELF.gShowNodeValueY.eOnF = %GraphShowNodeValueYControl
  #ENDIF
  #IF(%GraphShowNodeValueXOnStatusBar AND %GraphShowNodeValueXZone)
SELF.gShowNodeValueX.eOnS = %GraphShowNodeValueXZone
  #ENDIF
  #IF(%GraphShowNodeValueYOnStatusBar AND %GraphShowNodeValueYZone)
SELF.gShowNodeValueY.eOnS = %GraphShowNodeValueYZone
  #ENDIF
#!------------------------------------------------------------------------------------------
#GROUP(%InsertPopupOptions), AUTO
  #EQUATE(%locPopUpItems, 'SELF.ePopUpItems')
  #EQUATE(%locPopUpSubGraphType, 'SELF.ePopUpSubGraphType')
  #EQUATE(%locPopUpSubLegend, 'SELF.ePopUpSubLegend')
  #EQUATE(%locPopUpSubAxisList, 'SELF.ePopUpSubAxisList')
  #EQUATE(%locPopUpSubNode, 'SELF.ePopUpSubNode')
  #EQUATE(%locPopUpSubWallpaper, 'SELF.ePopUpSubWallpaper')
  #!.....................................................................................
  #CALL(%InsertTrueFalse, 'SELF.ePopUp', %GraphAccessiblePopUp)
  #IF(~%GraphAccessiblePopUp)
    #RETURN
  #ENDIF
CLEAR(SELF.ePopUpItems)
  #CALL(%InsertAdd, %GraphPopUpTitle, %locPopUpItems, 'GraphPop:Title')
  #CALL(%InsertAdd, %GraphPopUpWallpaper, %locPopUpItems, 'GraphPop:Wallpaper')
  #IF(%GraphPopUpWallpaper)
CLEAR(SELF.ePopUpSubWallpaper)
    #CALL(%InsertAdd, %GraphPopUpWallpaperNone, %locPopUpSubWallpaper, 'GraphPop:Wallpaper:None')
    #CALL(%InsertAdd, %GraphPopUpWallpaperStretched, %locPopUpSubWallpaper, 'GraphPop:Wallpaper:Stretched')
    #CALL(%InsertAdd, %GraphPopUpWallpaperTiled, %locPopUpSubWallpaper, 'GraphPop:Wallpaper:Tiled')
    #CALL(%InsertAdd, %GraphPopUpWallpaperCentered, %locPopUpSubWallpaper, 'GraphPop:Wallpaper:Centered')
  #ENDIF
  #CALL(%InsertAdd, %GraphPopUp3D, %locPopUpItems, 'GraphPop:3D')
  #!call(%InsertAdd, %GraphPopUpGradient, %locPopUpItems, 'GraphPop:Gradient')
  #CALL(%InsertAdd, %GraphPopUpZoom, %locPopUpItems, 'GraphPop:Zoom')
  #CALL(%InsertAdd, %GraphPopUpType, %locPopUpItems, 'GraphPop:GraphType')
  #IF(%GraphPopUpType)
CLEAR(SELF.ePopUpSubGraphType)
    #CALL(%InsertAdd, %GraphPopUpScatterGraph, %locPopUpSubGraphType, 'GraphPop:ScatterGraph')
    #CALL(%InsertAdd, %GraphPopUpLine, %locPopUpSubGraphType, 'GraphPop:Line')
    #CALL(%InsertAdd, %GraphPopUpAreaGraph, %locPopUpSubGraphType, 'GraphPop:AreaGraph')
    #CALL(%InsertAdd, %GraphPopUpFloatingArea, %locPopUpSubGraphType, 'GraphPop:FloatingArea')
    #CALL(%InsertAdd, %GraphPopUpColumnChart, %locPopUpSubGraphType, 'GraphPop:ColumnChart')
    #CALL(%InsertAdd, %GraphPopUpColumnWithAccumulation, %locPopUpSubGraphType, 'GraphPop:ColumnWithAccumulation')
    #CALL(%InsertAdd, %GraphPopUpFloatingColumn, %locPopUpSubGraphType, 'GraphPop:FloatingColumn')
    #CALL(%InsertAdd, %GraphPopUpBarChart, %locPopUpSubGraphType, 'GraphPop:BarChart')
    #CALL(%InsertAdd, %GraphPopUpBarWithAccumulation, %locPopUpSubGraphType, 'GraphPop:BarWithAccumulation')
    #CALL(%InsertAdd, %GraphPopUpFloatingBar, %locPopUpSubGraphType, 'GraphPop:FloatingBar')
    #CALL(%InsertAdd, %GraphPopUpPieChart, %locPopUpSubGraphType, 'GraphPop:PieChart')
  #ENDIF
  #CALL(%InsertAdd, %GraphPopUpFigure, %locPopUpItems, 'GraphPop:Figure')
  #CALL(%InsertAdd, %GraphPopUpLegend, %locPopUpItems, 'GraphPop:Legend')
  #IF(%GraphPopUpLegend)
CLEAR(SELF.ePopUpSubLegend)
    #CALL(%InsertAdd, %GraphPopUpLegendNone, %locPopUpSubLegend, 'GraphPop:Legend:None')
    #CALL(%InsertAdd, %GraphPopUpLegendLeft, %locPopUpSubLegend, 'GraphPop:Legend:Left')
    #CALL(%InsertAdd, %GraphPopUpLegendRight, %locPopUpSubLegend, 'GraphPop:Legend:Right')
    #CALL(%InsertAdd, %GraphPopUpLegendTop, %locPopUpSubLegend, 'GraphPop:Legend:Top')
    #CALL(%InsertAdd, %GraphPopUpLegendBottom, %locPopUpSubLegend, 'GraphPop:Legend:Bottom')
    #CALL(%InsertAdd, %GraphPopUpLegendBox, %locPopUpItems, 'GraphPop:LegendBox')
  #ENDIF
  #CALL(%InsertAdd, %GraphPopUpAxisList, %locPopUpItems, 'GraphPop:AxisList')
  #IF(%GraphPopUpAxisList)
CLEAR(SELF.ePopUpSubAxisList)
    #CALL(%InsertAdd, %GraphPopUpAxisListNone, %locPopUpSubAxisList, 'GraphPop:AxisList:None')
    #CALL(%InsertAdd, %GraphPopUpAxisListStandard, %locPopUpSubAxisList, 'GraphPop:AxisList:Standard')
    #CALL(%InsertAdd, %GraphPopUpAxisListLong, %locPopUpSubAxisList, 'GraphPop:AxisList:Long')
    #CALL(%InsertAdd, %GraphPopUpGrid, %locPopUpItems, 'GraphPop:Grid')
    #CALL(%InsertAdd, choose(%GraphPopUpGrid AND %GraphPopUpGridX), %locPopUpItems, 'GraphPop:GridX')
    #CALL(%InsertAdd, choose(%GraphPopUpGrid AND %GraphPopUpGridY), %locPopUpItems, 'GraphPop:GridY')
    #CALL(%InsertAdd, %GraphPopUpAxisListName, %locPopUpItems, 'GraphPop:AxisListName')
    #CALL(%InsertAdd, %GraphPopUpAxisListScale, %locPopUpItems, 'GraphPop:AxisListScale')
    #CALL(%InsertAdd, %GraphPopUpAxisListScaleMinMax, %locPopUpItems, 'GraphPop:AxisListScaleMinMax')
  #ENDIF
  #CALL(%InsertAdd, %GraphPopUpNode, %locPopUpItems, 'GraphPop:Node')
  #IF(%GraphPopUpNode)
CLEAR(SELF.ePopUpSubNode)
    #CALL(%InsertAdd, %GraphPopUpNodeSquare, %locPopUpSubNode, 'GraphPop:Node:Square')
    #CALL(%InsertAdd, %GraphPopUpNodeTriangle, %locPopUpSubNode, 'GraphPop:Node:Triangle')
    #CALL(%InsertAdd, %GraphPopUpNodeCircle, %locPopUpSubNode, 'GraphPop:Node:Circle')
    #CALL(%InsertAdd, %GraphPopUpNodeNone, %locPopUpSubNode, 'GraphPop:Node:None')
    #CALL(%InsertAdd, %GraphPopUpNodeMinMax, %locPopUpItems, 'GraphPop:NodeMinMax')
    #CALL(%InsertAdd, %GraphPopUpNodeLabel, %locPopUpItems, 'GraphPop:NodeLabel')
    #CALL(%InsertAdd, %GraphPopUpNodeValue, %locPopUpItems, 'GraphPop:NodeValue')
    #CALL(%InsertAdd, %GraphPopUpNodeBgr, %locPopUpItems, 'GraphPop:NodeBgr')
  #ENDIF
  #CALL(%InsertAdd, %GraphPopUpPrint, %locPopUpItems, 'GraphPop:Print')
  #CALL(%InsertAdd, %GraphPopUpPrintBestFit, %locPopUpItems, 'GraphPop:PrintBestFit')
  #CALL(%InsertAdd, %GraphPopUpSave, %locPopUpItems, 'GraphPop:Save')
  #CALL(%InsertAdd, %GraphPopUpSaveAs, %locPopUpItems, 'GraphPop:SaveAs')
  #IF(%GraphAccessibleDrillDown)
    #CALL(%InsertAdd, %GraphPopUpDrillDown, %locPopUpItems, 'GraphPop:DrillDown')
    #CALL(%InsertAdd, %GraphPopUpReturnFromDrillDown, %locPopUpItems, 'GraphPop:ReturnFromDrillDown')
  #ENDIF
  #IF(%GraphAccessibleTooltip)
    #CALL(%InsertAdd, %GraphPopUpToolTip, %locPopUpItems, 'GraphPop:ToolTip')
  #ENDIF
#!------------------------------------------------------------------------------------------
#GROUP(%GetControlName)
  #FOR(%ReportControl), WHERE(%ActiveTemplateInstance=%ReportControlInstance)
    #RETURN(%ReportControl)
  #ENDFOR
  #FOR(%Control), WHERE(%ActiveTemplateInstance=%ControlInstance)
    #RETURN(%Control)
  #ENDFOR
  #RETURN('')
#!------------------------------------------------------------------------------------------
#GROUP(%IsAppTemplate, %parTemplate)
  #IF(upper(%AppTemplateFamily)=upper(%parTemplate))
    #RETURN(%True)
  #ENDIF
  #RETURN('')
#!------------------------------------------------------------------------------------------
#GROUP(%IsReport)
  #FOR(%ReportControl), WHERE(%ReportControlInstance = %ActiveTemplateInstance)
    #RETURN(%True)
  #ENDFOR
  #RETURN('')
#!------------------------------------------------------------------------------------------
#GROUP(%IsDefault, * %parVar)
  #IF(upper(%parVar)=upper(%equSVDefault))
    #RETURN(%True)
  #ENDIF
  #RETURN('')
#!------------------------------------------------------------------------------------------
#GROUP(%StripPling, %parValue)
  #IF(sub(%parValue,1,1)='!')
    #RETURN(sub(%parValue,2))
  #ELSIF(sub(%parValue,1,1)='=')
    #RETURN('evaluate(' & %StripPling(sub(%parValue, 2)) & ')')
  #ENDIF
  #RETURN( '''' & quote(%parValue) & '''' )
#!------------------------------------------------------------------------------------------
#GROUP(%AddParam, *%parList, %par1), AUTO
  #IF(len(%par1) OR %parList)
    #IF(numeric(%par1))
      #SET(%parList, clip(%par1) & clip(%parList))
    #ELSIF(len(%par1))
      #SET(%parList, %StripPling(%par1) & clip(%parList))
    #ENDIF
    #SET(%parList, ',' & clip(%parList))
  #ENDIF
  #RETURN(%parList)
#!------------------------------------------------------------------------------------------
#GROUP(%InsertTrueFalse, %parLeft, long %parValue)
%parLeft = %(choose(~%parValue,'false','true'))
#!------------------------------------------------------------------------------------------
#GROUP(%InsertTrue, %parLeft, long %parValue)
  #IF(%parValue)
%parLeft = true
  #ENDIF
#!------------------------------------------------------------------------------------------
#GROUP(%InsertYesNo, %parLeft, %parValue)
  #CASE(upper(%parValue))
  #OF(upper(%equSVTrue))
  #OROF(upper(%equSVFalse))
%parLeft = %parValue
  #ENDCASE
#!------------------------------------------------------------------------------------------
#GROUP(%InsertAdd, %parFlag, %parLeft, %parValue)
  #IF(%parFlag)
%parLeft += %parValue
  #ENDIF
#!------------------------------------------------------------------------------------------
#GROUP(%CallProc, %parProc, %par1, %par2='', %par3='', %par4='', %par5='', %par6='', %par7=''), AUTO
  #DECLARE(%locConstruct)
  #!................................................
  #CALL(%AddParam,%locConstruct,%par7)
  #CALL(%AddParam,%locConstruct,%par6)
  #CALL(%AddParam,%locConstruct,%par5)
  #CALL(%AddParam,%locConstruct,%par4)
  #CALL(%AddParam,%locConstruct,%par3)
  #CALL(%AddParam,%locConstruct,%par2)
  #IF(len(%par1) OR %locConstruct)
    #IF(numeric(%par1))
      #SET(%locConstruct, clip(%par1) & %locConstruct)
    #ELSIF(len(%par1))
      #SET(%locConstruct, %StripPling(%par1) & %locConstruct)
    #ENDIF
  #ENDIF
  #RETURN( %parProc & '(' & %locConstruct & ')' )
#!------------------------------------------------------------------------------------------
#GROUP(%InsertProc, %parProc, %par1, %par2='', %par3='', %par4='', %par5='', %par6='', %par7=''), AUTO
  #IF(%par1 OR %par2 OR %par3 OR %par4 OR %par5 OR %par6 OR %par7)
%(%CallProc(%parProc, %par1, %par2, %par3, %par4, %par5, %par6, %par7))
  #ENDIF
#!------------------------------------------------------------------------------------------
#GROUP(%GraphAxisListScaleIntervalTip)
  #IF(%GraphToShowTips)
    #RETURN(choose(~%Isreport(),'Distance between strokes of an Axis in pixels','Distance between strokes of an Axis in millimeters'))
  #ENDIF
#!------------------------------------------------------------------------------------------
#GROUP(%GraphAxisListThicknessTip)
  #IF(%GraphToShowTips)
    #RETURN(choose(~%Isreport(),'Thickness of a line of an axis in pixels','Thickness of a line of an axis in millimeters'))
  #ENDIF
#!------------------------------------------------------------------------------------------
#GROUP(%GraphAxisListColorTip)
  #IF(%GraphToShowTips)
    #RETURN( 'If is set -1 (color:none) then the value settings in a method SetDefault is used')
  #ENDIF
#!------------------------------------------------------------------------------------------
#GROUP(%GraphFontTip)
  #IF(%GraphToShowTips)
    #RETURN('Font...')
  #ENDIF
#!------------------------------------------------------------------------------------------
#GROUP(%GraphListOfAccessibleTabs)
  #IF(%IsReport())
    #RETURN('General[General]|Initial values[InitialValues]|Data sources[DataSources]|Class[Class]')
  #ENDIF
  #RETURN('General[General]|Initial values[InitialValues]|Display options[DisplayOptions]|Data sources[DataSources]|Class[Class]')
#!------------------------------------------------------------------------------------------
#GROUP(%GraphControlPrompts)
  #BOXED('Hidden Prompts'), AT(0,0,0,0), WHERE(%False), hide
    #PROMPT('To show tips in a template',check), %GraphToShowTips, DEFAULT(%True)
    #PROMPT('ControlName',FROM(%Control)),%LocalControlName
    #PROMPT('LocalBaseClassToUse',@S255),%LocalBaseClassToUse
    #PROMPT('LocalParentCallValid',@s1),%LocalParentCallValid
    #PROMPT('LocalMethodEmbedPointValid',@s1),%LocalMethodEmbedPointValid
    #PROMPT('LocalEmbedTreeDesc',@s1),%LocalEmbedTreeDesc
    #PROMPT('Series Methods Present',@n6),%SeriesMethodsPresent
    #INSERT(%OOPHiddenPrompts(ABC))
  #ENDBOXED
  #DISPLAY('Graph control properties: ' & Slice(%LocalControlName,2,LEN(%LocalControlName))),AT(6)
  #PROMPT('View ',drop(%GraphListOfAccessibleTabs())),%GraphCurrentTablevel1, DEFAULT('General'),PROMPTAT(6,20),AT(35,20,160)
  #BOXED, WHERE(%GraphCurrentTabLevel1 = 'General'), AT(6,32)
    #DISPLAY
    #BOXED('Note')
      #DISPLAY('If set to "Default" or "0" then the setting of')
      #DISPLAY('property will be as set in a method SetDefault.')
    #ENDBOXED
    #DISPLAY
    #PROMPT('Refresh Automatically',check), %GraphAutoRefresh, DEFAULT(%True), AT(10)
    #ENABLE(~%GraphAutoRefresh)
      #DISPLAY('You will need to call the method ' & %ThisObjectName & '.Refresh(true)')
      #DISPLAY('manually to display the graph')
    #ENDENABLE
    #DISPLAY
    #BOXED('Accessibility'), SECTION
      #ENABLE(~%IsReport()), CLEAR
        #PROMPT('PopUp Menu',check), %GraphAccessiblePopUp, DEFAULT(%True), AT(10,2)
        #PROMPT('Tooltip',check), %GraphAccessibleTooltip, DEFAULT(%True), AT(10)
        #PROMPT('Drill Down',check), %GraphAccessibleDrillDown, DEFAULT(%True), AT(10)
      #ENDENABLE
    #ENDBOXED
    #DISPLAY
  #ENDBOXED
  #BOXED, WHERE(%GraphCurrentTabLevel1 = 'InitialValues'), AT(6,32)
    #! Initializing object
    #SHEET, HSCROLL
      #TAB('Graph'), HLP('~TPLGraphControlPromptsInitGraph')
        #PROMPT('Graph type',drop('Scatter Graph[ScatterGraph]|Line Graph[Line]|Area Graph[AreaGraph]|Floating Area[FloatingArea]|Column Chart[ColumnChart]|Column with accumulation[ColumnWithAccumulation]|Floating Column[FloatingColumn]|Bar Chart[BarChart]|Bar with accumulation[BarWithAccumulation]|Floating Bar[FloatingBar]|Pie Chart[PieChart]')), %GraphTypeInit, DEFAULT('ColumnChart')
        #ENABLE(%GraphTypeInit='ColumnWithAccumulation' OR %GraphTypeInit='BarWithAccumulation')
          #PROMPT('Graph subtype',drop('Simple[Simple]|Normalized[Normalized]')), %GraphSubTypeInit, DEFAULT('Simple')
        #ENDENABLE
        #PROMPT('Graph figure',drop('Bar[Bar]|Cylinder[Cylinder]')), %GraphFigureInit, DEFAULT('Bar'), prop(prop:tip,choose(%GraphToShowTips,'for "Column Chart", "Floating Column", "Bar Chart", "Floating Bar"',''))
        #PROMPT('3D-effect',drop(%equSVYesNo)), %GraphInit3D, DEFAULT(%equSVTrue)
        #PROMPT('Auto swap axes',drop(%equSVYesNo)), %GraphInitAxisListAutoSwap, DEFAULT(%equSVTrue)
        #!PROMPT('Gradient-effect',drop(%equSVYesNo)), %GraphInitGradient, DEFAULT(%equSVTrue)
      #ENDTAB
      #TAB('Title'), HLP('~TPLGraph_Init_Title')
        #PROMPT('Title',drop(%equSVYesNo)), %GraphInitTitle, DEFAULT(%equSVTrue)
        #BOXED('Properties'),SECTION
          #PROMPT('Title text',EXPR),%GraphTitle,AT(60,3,105),PROMPTAT(,3)
          #BOXED(''), WHERE(%False), hide, AT(0,0,0,0)
            #PROMPT('',@s255),%GraphTitleFontName
            #PROMPT('',@s30),%GraphTitleFontSize, DEFAULT(16)
            #PROMPT('',@s30),%GraphTitleFontColor
            #PROMPT('',@s30),%GraphTitleFontStyle
            #PROMPT('',@s30),%GraphTitleFontChar
          #ENDBOXED
          #BUTTON('F'),AT(166,3,15,10),whenaccepted(%SVFSelectFont(%GraphTitleFontName,%GraphTitleFontSize,%GraphTitleFontColor,%GraphTitleFontStyle,%GraphTitleFontChar)),prop(prop:tip,%GraphFontTip()),prop(prop:fontstyle,700)
          #ENDBUTTON
          #DISPLAY('FONT(' & %GraphTitleFontName &','& %GraphTitleFontSize &','& %GraphTitleFontColor &','& %GraphTitleFontStyle &','& %GraphTitleFontChar & ')')
        #ENDBOXED
      #ENDTAB
      #TAB('Legend'), HLP('~TPLGraph_Init_Legend')
        #PROMPT('Location',drop('Bottom[Bottom]|Top[Top]|Left[Left]|Right[Right]|None[None]')),%GraphLegendInitPos, DEFAULT('Bottom')
        #PROMPT('Box',drop(%equSVYesNo)), %GraphInitLegendBox, DEFAULT(%equSVFalse)
      #ENDTAB
      #TAB('Axis...'), HLP('~TPLGraph_Init_AxisList')
        #BOXED
          #SHEET
            #TAB('General'), HLP('~TPLGraph_Init_AxisList_General')
              #PROMPT('Axis styles',drop('Standard|Long|None')), %GraphInitAxisListStyle, DEFAULT('Standard')
              #PROMPT('Name',drop(%equSVYesNo)), %GraphInitAxisListName, DEFAULT(%equSVTrue)
              #PROMPT('Grid',drop(%equSVYesNo)), %GraphInitGrid, DEFAULT(%equSVTrue)
              #PROMPT('Scale style',drop('Linear[Linear]|As MS Word[AsMSWord]')),%GraphAxisListScale, DEFAULT('Linear')
              #PROMPT('Scale min/max',drop(%equSVYesNo)),%GraphAxisListScaleMinMax, DEFAULT(%equSVFalse), prop(prop:tip,choose(%GraphToShowTips,'Scale from minimal up to maximal',''))
              #DISPLAY
            #ENDTAB
            #TAB('X-Axis'), HLP('~TPLGraphControlPromptsAxisXAxis')
              #DISPLAY('Horizontal Properties'), AT(3,,182), prop(prop:fontcolor, %equSVTitleFontColor), prop(prop:color,%equSVTitleBgrColor)
              #PROMPT('X-Axis Label',OPTFIELD), %GraphXAxisLabel
              #PROMPT('Format of values', PICTURE), %GraphXAxisFormat
              #ENABLE(1), SECTION
                #PROMPT('Name',OPTFIELD), %GraphXAxisName, AT(40,3,125), PROMPTAT(,3)
                #BOXED(''), WHERE(%False), hide, AT(0,0,0,0)
                  #PROMPT('',@s255), %GraphXAxisNameFontName
                  #PROMPT('',@s30), %GraphXAxisNameFontSize
                  #PROMPT('',@s30), %GraphXAxisNameFontColor
                  #PROMPT('',@s30), %GraphXAxisNameFontStyle
                  #PROMPT('',@s30), %GraphXAxisNameFontChar
                #ENDBOXED
                #BUTTON('F'), AT(166,3,14,10), whenaccepted(%SVFSelectFont(%GraphXAxisNameFontName,%GraphXAxisNameFontSize,%GraphXAxisNameFontColor,%GraphXAxisNameFontStyle,%GraphXAxisNameFontChar)), prop(prop:tip,%GraphFontTip()),prop(prop:fontstyle,700)
                #ENDBUTTON
                #DISPLAY('FONT(' & %GraphXAxisNameFontName &','& %GraphXAxisNameFontSize &','&  %GraphXAxisNameFontColor &','& %GraphXAxisNameFontStyle &','& %GraphXAxisNameFontChar & ')')
              #ENDENABLE
              #DISPLAY
              #PROMPT('Grid',drop(%equSVYesNo)), %GraphInitAxisXGrid, DEFAULT(%equSVTrue)
              #PROMPT('Color',color), %GraphAxisXColor, DEFAULT(-1), prop(prop:tip, %GraphAxisListColorTip())
              #PROMPT('Thickness',spin(@n6.2,0,100.00,choose(~%IsReport(),1,0.05))), %GraphAxisXThickness, prop(prop:tip, %GraphAxisListThicknessTip())
              #PROMPT('Scale interval',spin(@n6.2,0,100.00,choose(~%IsReport(),1,0.05))), %GraphAxisXScaleInterval, prop(prop:tip, %GraphAxisListScaleIntervalTip())
            #ENDTAB
            #TAB('Y-Axis'), HLP('~TPLGraphControlPromptsAxisYAxis')
              #DISPLAY('Vertical Properties'), AT(3,,182), prop(prop:fontcolor, %equSVTitleFontColor), prop(prop:color,%equSVTitleBgrColor)
              #PROMPT('Y-Axis Label',OPTFIELD), %GraphYAxisLabel
              #PROMPT('Format of values', PICTURE), %GraphYAxisFormat
              #ENABLE(1),SECTION
                #PROMPT('Name',OPTFIELD), %GraphYAxisName, AT(40,3,125), PROMPTAT(,3)
                #BOXED(''), WHERE(%False), hide, AT(0,0,0,0)
                  #PROMPT('',@s255), %GraphYAxisNameFontName
                  #PROMPT('',@s30), %GraphYAxisNameFontSize
                  #PROMPT('',@s30), %GraphYAxisNameFontColor
                  #PROMPT('',@s30), %GraphYAxisNameFontStyle
                  #PROMPT('',@s30), %GraphYAxisNameFontChar
                #ENDBOXED
                #BUTTON('F'), AT(166,3,14,10), whenaccepted(%SVFSelectFont(%GraphYAxisNameFontName,%GraphYAxisNameFontSize,%GraphYAxisNameFontColor,%GraphYAxisNameFontStyle,%GraphYAxisNameFontChar)), prop(prop:tip,%GraphFontTip()),prop(prop:fontstyle,700)
                #ENDBUTTON
                #DISPLAY('FONT(' & %GraphYAxisNameFontName &','& %GraphYAxisNameFontSize &','& %GraphYAxisNameFontColor &','& %GraphYAxisNameFontStyle &','& %GraphYAxisNameFontChar & ')')
              #ENDENABLE
              #DISPLAY
              #PROMPT('Grid',drop(%equSVYesNo)), %GraphInitAxisYGrid, DEFAULT(%equSVTrue)
              #PROMPT('Color',color), %GraphAxisYColor, DEFAULT(-1), prop(prop:tip, %GraphAxisListColorTip())
              #PROMPT('Thickness',spin(@n6.2,0,100.00,choose(~%IsReport(),1,0.05))), %GraphAxisYThickness, prop(prop:tip, %GraphAxisListThicknessTip())
              #PROMPT('Scale interval',spin(@n6.2,0,100.00,choose(~%IsReport(),1,0.05))), %GraphAxisYScaleInterval, prop(prop:tip,%GraphAxisListScaleIntervalTip())
            #ENDTAB
            #TAB('Grouping'), HLP('~TPLGraphControlPromptsAxisGrouping')
              #PROMPT('Sort',drop('Name[Name]|ID[ID]')), %GraphAxisListGroupingSort, DEFAULT('Name')
              #PROMPT('Sequence of sorting',drop('Ascending[+]|Descending[-]')), %GraphAxisListGroupingSequence, DEFAULT('+')
            #ENDTAB
          #ENDSHEET
        #ENDBOXED
        #DISPLAY('On the Axis Name you can use these variables:')
        #DISPLAY('Graph:TotalRecords : Records read for this source')
        #DISPLAY('Graph:PointNumber  : Point number for this source')
        #DISPLAY('Graph:SPointNumber : Point number for this point set')
      #ENDTAB
      #TAB('Node'), HLP('~TPLGraphControlPromptsInitNode')
        #! Default type of node for all diagrams
        #PROMPT('Node type',drop('Square[Square]|Triangle[Triangle]|Circle[Circle]|None[None]')), %GraphInitNodeType, DEFAULT('Circle')
        #PROMPT('Node radius',spin(@n6.2,0,999,1)), %GraphNodeRadius
        #PROMPT('Minimum/Maximum',drop(%equSVYesNo)), %GraphInitNodeMinMax, DEFAULT(%equSVFalse)
        #BOXED('Text of node')
          #PROMPT('Show Label on Nodes',drop(%equSVYesNo)), %GraphNodeLabel, DEFAULT(%equSVFalse)
          #PROMPT('Show Value on Nodes',drop(%equSVYesNo)), %GraphNodeValue, DEFAULT(%equSVFalse)
          #PROMPT('Background',drop(%equSVYesNo)), %GraphNodeBgr, DEFAULT(%equSVFalse)
          #PROMPT('Best position',drop(%equSVYesNo)), %GraphInitBestPositionNodeText, DEFAULT(%equSVTrue)
        #ENDBOXED
      #ENDTAB
      #TAB('Other...'), HLP('~TPLGraphControlPromptsInitOther')
        #BOXED
          #SHEET, hscroll
            #TAB('Zoom'), HLP('~TPLGraphControlPromptsInitOtherZoom')
              #PROMPT('Zoom',drop('Auto[Auto]|500%[500]|200%[200]|100%[100]|50%[50]|25%[25]')), %GraphInitZoom, DEFAULT('Auto')
            #ENDTAB
            #TAB('ToolTip'), WHERE(%GraphAccessibleTooltip AND ~%IsReport()), HLP('~TPLGraph_Init_Other_ToolTip')
              #PROMPT('ToolTip',drop(%equSVYesNo)), %GraphInitToolTip, DEFAULT(%equSVTrue)
              #PROMPT('Transparent',drop(%equSVYesNo)), %GraphInitToolTipTrn, DEFAULT(%equSVFalse)
              #ENABLE(%GraphInitToolTipTrn=%equSVFalse)
                #PROMPT('Background',color), %GraphInitToolTipBgr, DEFAULT(-1), prop(prop:tip, %GraphAxisListColorTip())
              #ENDENABLE
              #PROMPT('Best position',drop(%equSVYesNo)), %GraphInitBestPositionToolTip, DEFAULT(%equSVTrue)
              #DISPLAY('Other'), AT(3,,182), prop(prop:fontcolor, %equSVTitleFontColor), prop(prop:color,%equSVTitleBgrColor)
              #PROMPT('Corection of Width',spin(@n-6.2,0,999,1)), %GraphInitToolTipCorrectionOfWidth
              #PROMPT('Corection of Height',spin(@n-6.2,0,999,1)), %GraphInitToolTipCorrectionOfHeight
            #ENDTAB
            #TAB('Indent'), HLP('~TPLGraphControlPromptsInitOtherIndent')
              #BOXED('Indents for drawing'), SECTION
                #PROMPT('Left',spin(@n4,0,9999,1)), %GraphIndentLeft, AT(35,5,40), PROMPTAT(8,5,30), DEFAULT(2)
                #PROMPT('Top',spin(@n4,0,9999,1)), %GraphIndentTop, AT(115,5,40), PROMPTAT(85,5,30), DEFAULT(2)
                #PROMPT('Right',spin(@n4,0,9999,1)), %GraphIndentRight, AT(35,20,40), PROMPTAT(8,20,30), DEFAULT(2)
                #PROMPT('Bottom',spin(@n4,0,9999,1)), %GraphIndentBottom, AT(115,20,40), PROMPTAT(85,20,30), DEFAULT(2)
              #ENDBOXED
            #ENDTAB
            #TAB('Print'), WHERE(~%IsReport()), HLP('~TPLGraphControlPromptsInitOtherPrint')
              #DISPLAY('Print'), AT(3,,182), prop(prop:fontcolor, %equSVTitleFontColor), prop(prop:color,%equSVTitleBgrColor)
              #PROMPT('Preview',drop(%equSVYesNo)), %GraphInitPrintPreview, DEFAULT(%equSVTrue)
              #PROMPT('Orientation',drop('As on Printer[0]|Portrait[1]|Landscape[2]')), %GraphInitPrintOrientation, DEFAULT('0')
              #BOXED('Indents from edges of a paper (mm)'), SECTION
                #PROMPT('Left',spin(@n6.2,0,600,1)), %GraphInitPrintLeft, AT(35,5,40), PROMPTAT(8,5,30), DEFAULT(20)
                #PROMPT('Top',spin(@n6.2,0,600,1)), %GraphInitPrintTop, AT(115,5,40), PROMPTAT(85,5,30), DEFAULT(10)
                #PROMPT('Right',spin(@n6.2,0,600,1)), %GraphInitPrintRight, AT(35,20,40), PROMPTAT(8,20,30), DEFAULT(10)
                #PROMPT('Bottom',spin(@n6.2,0,600,1)), %GraphInitPrintBottom, AT(115,20,40), PROMPTAT(85,20,30), DEFAULT(20)
              #ENDBOXED
              #PROMPT('Box',drop(%equSVYesNo)), %GraphInitPrintBox, DEFAULT(%equSVFalse)
              #DISPLAY('Print best fit'), AT(3,,182), prop(prop:fontcolor, %equSVTitleFontColor), prop(prop:color,%equSVTitleBgrColor)
              #BOXED, SECTION
                #PROMPT('Horizontal alignment',drop('Left[Left]|Right[Right]|Center[CenterH]')), %GraphInitPrintAlignH, DEFAULT('Left')
                #PROMPT('Vertical alignment',drop('Top[Top]|Bottom[Bottom]|Center[CenterV]')), %GraphInitPrintAlignV, DEFAULT('Top')
                #PROMPT('Proportional',drop(%equSVYesNo)), %GraphInitPrintProportional, DEFAULT(%equSVTrue)
                #ENABLE(%True), SECTION
                  #BOXED('Stretch'), AT(,0), WHERE(%GraphInitPrintProportional<>%equSVTrue)
                    #PROMPT('Horizontally',check), %GraphInitPrintStretchH, DEFAULT(%False), AT(15), whenaccepted(%GraphSyncPrintStretch(%True))
                    #PROMPT('Vertically',check), %GraphInitPrintStretchV, DEFAULT(%False), AT(15), whenaccepted(%GraphSyncPrintStretch(%True))
                  #ENDBOXED
                  #BOXED, AT(,0), WHERE(%GraphInitPrintProportional=%equSVTrue)
                    #PROMPT('Stretch',option), %GraphInitPrintStretch, DEFAULT('0'), AT(,0), whenaccepted(%GraphSyncPrintStretch())
                    #PROMPT('None',radio), value('0'), AT(15)
                    #PROMPT('Horizontally',radio), value('01b'), AT(15)
                    #PROMPT('Vertically',radio), value('10b'), AT(15)
                  #ENDBOXED
                #ENDENABLE
              #ENDBOXED
              #DISPLAY
            #ENDTAB
            #TAB('Other'), HLP('~TPLGraphControlPromptsInitOtherOther')
              #DISPLAY('Wallpaper'), AT(3,,182), prop(prop:fontcolor, %equSVTitleFontColor),prop(prop:color,%equSVTitleBgrColor)
              #ENABLE(~%IsReport())
              #PROMPT('Wallpaper',drop('None[None]|Stretched[Stretched]|Tiled[Tiled]|Centered[Centered]')), %GraphInitWallpaperStyle, DEFAULT('Tiled')
              #BOXED('Properties'),SECTION
                #PROMPT('Wallpaper',OPENDIALOG('Select Wallpaper','All Images|*.bmp;*.pcx;*.gif;*.jpg;*.wmf|*.*|*.*')), %GraphWallpaper, prop(prop:tip,choose(%GraphToShowTips,'Wallpaper filename',''))
              #ENDBOXED
              #ENDENABLE
              #DISPLAY('Save as'), AT(3,,182), prop(prop:fontcolor, %equSVTitleFontColor),prop(prop:color,%equSVTitleBgrColor)
              #ENABLE(~%IsReport())
                #PROMPT('File name',OPTFIELD), %GraphFileNameForSave, AT(50,,130), prop(prop:tip,choose(%GraphToShowTips,'Filename for Saving',''))
                #PROMPT('Always add file extension',check), %GraphFileNameForSaveAlwaysExt, AT(50), DEFAULT(%True)
              #ENDENABLE
              #DISPLAY
            #ENDTAB
          #ENDSHEET
        #ENDBOXED
      #ENDTAB
    #ENDSHEET
  #ENDBOXED
  #BOXED, WHERE(%GraphCurrentTabLevel1 = 'DisplayOptions' AND ~%IsReport()), AT(6,32)
    #SHEET,ADJUST
      #TAB('PopUp Menu'), HLP('~TPLGraph_Display_Popup')
        #ENABLE(%GraphAccessiblePopup), SECTION
          #BOXED
            #SHEET,ADJUST
            #TAB('General'), HLP('~TPLGraphControlPromptsDisplayPop')
              #BOXED(''), prop(prop:bevelOuter,-1), AT(3,25,90,140), SECTION
                #PROMPT('Title',check), %GraphPopUpTitle, DEFAULT(%True), AT(10)
                #PROMPT('Wallpaper...',check), %GraphPopUpWallpaper, DEFAULT(%True), AT(10), prop(prop:fontstyle, 400+700)
                #PROMPT('Zoom',check), %GraphPopUpZoom, DEFAULT(%True), AT(10)
                #PROMPT('Graph type...',check), %GraphPopUpType, DEFAULT(%True), AT(10), prop(prop:fontstyle, 400+700)
                #PROMPT('Figure',check), %GraphPopUpFigure, DEFAULT(%True), AT(10)
                #PROMPT('3D-effect',check), %GraphPopUp3D, DEFAULT(%True), AT(10)
                #!prompt('Gradient-effect',check), %GraphPopUpGradient, DEFAULT(%False), AT(10)
                #PROMPT('Legend...',check), %GraphPopUpLegend, DEFAULT(%True), AT(10), prop(prop:fontstyle, 400+700)
                #PROMPT('Axis...',check), %GraphPopUpAxisList, DEFAULT(%True), AT(10), prop(prop:fontstyle, 400+700)
                #PROMPT('Node...',check), %GraphPopUpNode, DEFAULT(%True), AT(10), prop(prop:fontstyle, 400+700)
                #PROMPT('Print',check), %GraphPopUpPrint, DEFAULT(%True), AT(10)
                #PROMPT('Print best fit',check), %GraphPopUpPrintBestFit, DEFAULT(%True), AT(10)
                #PROMPT('Save',check), %GraphPopUpSave, DEFAULT(%True), AT(10)
                #PROMPT('Save As',check), %GraphPopUpSaveAs, DEFAULT(%True), AT(10)
              #ENDBOXED
              #BOXED(''),prop(prop:bevelOuter,-1), AT(95,25,90,140), SECTION
                #ENABLE(%GraphAccessibleDrillDown)
                  #PROMPT('Drill Down',check), %GraphPopUpDrillDown, DEFAULT(%True), AT(10)
                  #PROMPT('Return',check), %GraphPopUpReturnFromDrillDown, DEFAULT(%True), AT(10), prop(prop:tip,choose(%GraphToShowTips,'Return from Drill Down',''))
                #ENDENABLE
                #ENABLE(%GraphAccessibleTooltip)
                  #PROMPT('ToolTip',check), %GraphPopUpToolTip, DEFAULT(%True), AT(10)
                #ENDENABLE
              #ENDBOXED
            #ENDTAB
            #TAB('Graph type'), WHERE(%GraphPopUpType), HLP('~TPLGraph_Display_Popup_GraphType')
              #DISPLAY('Graph types'), AT(3,,182), prop(prop:fontcolor, %equSVTitleFontColor),prop(prop:color,%equSVTitleBgrColor)
              #PROMPT('Scatter Graph',check), %GraphPopUpScatterGraph, DEFAULT(%True), AT(10)
              #PROMPT('Line Graph',check), %GraphPopUpLine, DEFAULT(%True), AT(10)
              #PROMPT('Area Graph',check), %GraphPopUpAreaGraph, DEFAULT(%True), AT(10)
              #PROMPT('Floating Area',check), %GraphPopUpFloatingArea, DEFAULT(%True), AT(10)
              #PROMPT('Column Chart',check), %GraphPopUpColumnChart, DEFAULT(%True), AT(10)
              #PROMPT('Column with accumulation',check), %GraphPopUpColumnWithAccumulation, DEFAULT(%True), AT(10)
              #PROMPT('Floating Column',check), %GraphPopUpFloatingColumn, DEFAULT(%True), AT(10)
              #PROMPT('Bar Chart',check), %GraphPopUpBarChart, DEFAULT(%True), AT(10)
              #PROMPT('Bar with accumulation',check), %GraphPopUpBarWithAccumulation, DEFAULT(%True), AT(10)
              #PROMPT('Floating Bar',check), %GraphPopUpFloatingBar, DEFAULT(%True), AT(10)
              #PROMPT('Pie Chart',check), %GraphPopUpPieChart, DEFAULT(%True), AT(10)
            #ENDTAB
            #TAB('Legend'), WHERE(%GraphPopUpLegend), HLP('~TPLGraph_Display_Popup_Legend')
              #DISPLAY('Legend position'), AT(3,,182), prop(prop:fontcolor, %equSVTitleFontColor),prop(prop:color,%equSVTitleBgrColor)
              #PROMPT('Bottom',check), %GraphPopUpLegendBottom, DEFAULT(%True), AT(10)
              #PROMPT('Top',check), %GraphPopUpLegendTop, DEFAULT(%True), AT(10)
              #PROMPT('Left',check), %GraphPopUpLegendLeft, DEFAULT(%True), AT(10)
              #PROMPT('Right',check), %GraphPopUpLegendRight, DEFAULT(%True), AT(10)
              #PROMPT('None',check), %GraphPopUpLegendNone, DEFAULT(%True), AT(10)
              #DISPLAY('Other'), AT(3,,182), prop(prop:fontcolor, %equSVTitleFontColor),prop(prop:color,%equSVTitleBgrColor)
              #PROMPT('Box',check), %GraphPopUpLegendBox, DEFAULT(%True), AT(10)
            #ENDTAB
            #TAB('Axis'), WHERE(%GraphPopUpAxisList), HLP('~TPLGraph_Display_Popup_AxisList')
              #DISPLAY('Axis styles'), AT(3,,182), prop(prop:fontcolor, %equSVTitleFontColor),prop(prop:color,%equSVTitleBgrColor)
              #PROMPT('Standard',check), %GraphPopUpAxisListStandard, DEFAULT(%True), AT(10)
              #PROMPT('Long',check), %GraphPopUpAxisListLong, DEFAULT(%True), AT(10)
              #PROMPT('None',check), %GraphPopUpAxisListNone, DEFAULT(%True), AT(10)
              #DISPLAY('Other'), AT(3,,182), prop(prop:fontcolor, %equSVTitleFontColor),prop(prop:color,%equSVTitleBgrColor)
              #PROMPT('Name',check), %GraphPopUpAxisListName, DEFAULT(%True), AT(10)
              #PROMPT('Grid',check), %GraphPopUpGrid, DEFAULT(%True), AT(10)
              #ENABLE(%GraphPopUpGrid)
                #PROMPT('Grid X',check), %GraphPopUpGridX, DEFAULT(%True), AT(20)
                #PROMPT('Grid Y',check), %GraphPopUpGridY, DEFAULT(%True), AT(20)
              #ENDENABLE
              #PROMPT('Scale style',check), %GraphPopUpAxisListScale, DEFAULT(%True), AT(10)
              #PROMPT('Scale min/max',check), %GraphPopUpAxisListScaleMinMax, DEFAULT(%True), AT(10)
            #ENDTAB
            #TAB('Node'), WHERE(%GraphPopUpNode), HLP('~TPLGraph_Display_Popup_Node')
              #DISPLAY('Node type'), AT(3,,182), prop(prop:fontcolor, %equSVTitleFontColor),prop(prop:color,%equSVTitleBgrColor)
              #PROMPT('Square',check), %GraphPopUpNodeSquare, DEFAULT(%True), AT(10)
              #PROMPT('Triangle',check), %GraphPopUpNodeTriangle, DEFAULT(%True), AT(10)
              #PROMPT('Circle',check), %GraphPopUpNodeCircle, DEFAULT(%True), AT(10)
              #PROMPT('None',check), %GraphPopUpNodeNone, DEFAULT(%True), AT(10)
              #DISPLAY('Other'), AT(3,,182), prop(prop:fontcolor, %equSVTitleFontColor),prop(prop:color,%equSVTitleBgrColor)
              #PROMPT('Minimum/Maximum',check), %GraphPopUpNodeMinMax, DEFAULT(%True), AT(10)
              #PROMPT('Show Label',check), %GraphPopUpNodeLabel, DEFAULT(%True), AT(10)
              #PROMPT('Show Value',check), %GraphPopUpNodeValue, DEFAULT(%True), AT(10)
              #PROMPT('Background',check), %GraphPopUpNodeBgr, DEFAULT(%True), AT(10)
            #ENDTAB
            #TAB('Wallpaper'), WHERE(%GraphPopUpWallpaper), HLP('~TPLGraph_Display_Popup_Wallpaper')
              #DISPLAY('Wallpaper styles'), AT(3,,182), prop(prop:fontcolor, %equSVTitleFontColor),prop(prop:color,%equSVTitleBgrColor)
              #PROMPT('None',check), %GraphPopUpWallpaperNone, DEFAULT(%True), AT(10)
              #PROMPT('Stretched',check), %GraphPopUpWallpaperStretched, DEFAULT(%True), AT(10)
              #PROMPT('Tiled',check), %GraphPopUpWallpaperTiled, DEFAULT(%True), AT(10)
              #PROMPT('Centered',check), %GraphPopUpWallpaperCentered, DEFAULT(%True), AT(10)
            #ENDTAB
            #ENDSHEET
          #ENDBOXED
          #DISPLAY('Check the actions that you want to support via the')
          #DISPLAY('PopUp Menu. (Mouse Right Click)')
        #ENDENABLE
      #ENDTAB
      #TAB('Drill Down'), HLP('~TPLGraphControlPromptsDisplayDrill')
        #INSERT(%GraphDrillDownPrompts)
      #ENDTAB
      #TAB('Show...'), HLP('~TPLGraphControlPromptsDisplayShow')
        #BOXED
          #SHEET, hscroll
            #TAB('Mouse'), HLP('~TPLGraphControlPromptsDisplayShowMouse')
              #ENABLE(1), SECTION
                #DISPLAY('Display mouse coordinates as "Mouse:..."'), AT(3,,182), prop(prop:fontcolor, %equSVTitleFontColor),prop(prop:color,%equSVTitleBgrColor)
                #PROMPT('on Tooltip',check), %GraphShowMouseOnTooltip, AT(10,15), DEFAULT(%True)
                #PROMPT('on Window title',check), %GraphShowMouseOnWindow, AT(10,27)
                #PROMPT('on the control',check), %GraphShowMouseOnControl, AT(10,39)
                #ENABLE(%GraphShowMouseOnControl)
                  #PROMPT('',CONTROL), %GraphShowMouseControl, AT(90,39)
                  #VALIDATE(%GraphShowMouseControl<>'',%equSVTextSetControl)
                #ENDENABLE
                #PROMPT('on a zone of the Status Bar',check), %GraphShowMouseOnStatusBar, AT(10,51)
                #ENABLE(%GraphShowMouseOnStatusBar)
                  #PROMPT('',spin(@n2,0,99,1)), %GraphShowMouseZone, AT(110,51,30)
                  #VALIDATE(%GraphShowMouseZone>0,%equSVTextSetZone)
                #ENDENABLE
                #DISPLAY('This text is set in the MouseText method.')
              #ENDENABLE
              #ENABLE(1), SECTION
                #DISPLAY('Display mouse coordinates'), AT(3,0,182), prop(prop:fontcolor, %equSVTitleFontColor),prop(prop:color,%equSVTitleBgrColor)
                #BOXED('on the control'), SECTION
                  #PROMPT('for coordinate X',check), %GraphShowMouseXOnControl, AT(10,2)
                  #ENABLE(%GraphShowMouseXOnControl)
                    #PROMPT('',CONTROL), %GraphShowMouseXControl, AT(90,2)
                    #VALIDATE(%GraphShowMouseXControl<>'',%equSVTextSetControl)
                  #ENDENABLE
                  #PROMPT('for coordinate Y',check), %GraphShowMouseYOnControl, AT(10,15)
                  #ENABLE(%GraphShowMouseYOnControl)
                    #PROMPT('',CONTROL),%GraphShowMouseYControl, AT(90,15)
                    #VALIDATE(%GraphShowMouseYControl<>'',%equSVTextSetControl)
                  #ENDENABLE
                #ENDBOXED
                #BOXED('on a zone of the Status Bar'), SECTION
                  #PROMPT('for coordinate X',check), %GraphShowMouseXOnStatusBar, AT(10,2)
                  #ENABLE(%GraphShowMouseXOnStatusBar)
                    #PROMPT('',spin(@n2,0,99,1)), %GraphShowMouseXZone, AT(90,2,30)
                    #VALIDATE(%GraphShowMouseXZone>0,%equSVTextSetZone)
                  #ENDENABLE
                  #PROMPT('for coordinate Y',check), %GraphShowMouseYOnStatusBar, AT(10,15)
                  #ENABLE(%GraphShowMouseYOnStatusBar)
                    #PROMPT('',spin(@n2,0,99,1)), %GraphShowMouseYZone, AT(90,15,30)
                    #VALIDATE(%GraphShowMouseYZone>0,%equSVTextSetZone)
                  #ENDENABLE
                #ENDBOXED
                #DISPLAY('This text set in the MouseXText')
                #DISPLAY('and MouseYText methods.')
              #ENDENABLE
            #ENDTAB
            #TAB('Diagram'), HLP('~TPLGraph_Display_Show_Diagram')
              #ENABLE(1), SECTION
                #DISPLAY('Display diagram name as "Diagram:..."'), AT(3,,182), prop(prop:fontcolor, %equSVTitleFontColor),prop(prop:color,%equSVTitleBgrColor)
                #PROMPT('on Tooltip',check), %GraphShowDiagramOnTooltip, AT(10,15), DEFAULT(%True)
                #PROMPT('on Window title',check), %GraphShowDiagramOnWindow, AT(10,27)
                #PROMPT('on the control',check), %GraphShowDiagramOnControl, AT(10,39)
                #ENABLE(%GraphShowDiagramOnControl)
                  #PROMPT('',CONTROL),%GraphShowDiagramControl, AT(90,39)
                  #VALIDATE(%GraphShowDiagramControl<>'',%equSVTextSetControl)
                #ENDENABLE
                #PROMPT('on a zone of the Status Bar',check), %GraphShowDiagramOnStatusBar, AT(10,51)
                #ENABLE(%GraphShowDiagramOnStatusBar)
                  #PROMPT('',spin(@n2,0,99,1)), %GraphShowDiagramZone, AT(110,51,30)
                  #VALIDATE(%GraphShowDiagramZone>0,%equSVTextSetZone)
                #ENDENABLE
                #DISPLAY('This text is set in the DiagramText method.')
              #ENDENABLE
              #ENABLE(1), SECTION
                #DISPLAY('Display the diagram name'), AT(3,,182), prop(prop:fontcolor, %equSVTitleFontColor),prop(prop:color,%equSVTitleBgrColor)
                #PROMPT('on the control',check), %GraphShowDiagramVOnControl, AT(10,15)
                #ENABLE(%GraphShowDiagramVOnControl)
                  #PROMPT('',CONTROL), %GraphShowDiagramVControl, AT(90,15)
                  #VALIDATE(%GraphShowDiagramVControl<>'',%equSVTextSetControl)
                #ENDENABLE
                #PROMPT('on a zone of the Status Bar',check), %GraphShowDiagramVOnStatusBar, AT(10,27)
                #ENABLE(%GraphShowDiagramVOnStatusBar)
                  #PROMPT('',spin(@n2,0,99,1)), %GraphShowDiagramVZone, AT(110,27,30)
                  #VALIDATE(%GraphShowDiagramVZone>0,%equSVTextSetZone)
                #ENDENABLE
                #DISPLAY('This text is set in the DiagramNameText method.')
              #ENDENABLE
            #ENDTAB
            #TAB('Node Name'), HLP('~TPLGraph_Display_Show_NodeName')
              #ENABLE(1), SECTION
                #DISPLAY('Display node name as "Node:..."'), AT(3,,182), prop(prop:fontcolor, %equSVTitleFontColor),prop(prop:color,%equSVTitleBgrColor)
                #PROMPT('on Tooltip',check), %GraphShowNodeNameOnTooltip, AT(10,15), DEFAULT(%True)
                #PROMPT('on Window title',check), %GraphShowNodeNameOnWindow, AT(10,27)
                #PROMPT('on the control',check), %GraphShowNodeNameOnControl, AT(10,39)
                #ENABLE(%GraphShowNodeNameOnControl)
                  #PROMPT('',CONTROL), %GraphShowNodeNameControl, AT(90,39)
                  #VALIDATE(%GraphShowNodeNameControl<>'',%equSVTextSetControl)
                #ENDENABLE
                #PROMPT('on a zone of the Status Bar',check), %GraphShowNodeNameOnStatusBar, AT(10,51)
                #ENABLE(%GraphShowNodeNameOnStatusBar)
                  #PROMPT('',spin(@n2,0,99,1)), %GraphShowNodeNameZone, AT(110,51,30)
                  #VALIDATE(%GraphShowNodeNameZone>0,%equSVTextSetZone)
                #ENDENABLE
                #DISPLAY('This text is set in the NodeText method.')
              #ENDENABLE
              #ENABLE(1), SECTION
                #DISPLAY('Display node name'), AT(3,,182), prop(prop:fontcolor, %equSVTitleFontColor),prop(prop:color,%equSVTitleBgrColor)
                #PROMPT('on the control',check), %GraphShowNodeNameVOnControl, AT(10,15)
                #ENABLE(%GraphShowNodeNameVOnControl)
                  #PROMPT('',CONTROL), %GraphShowNodeNameVControl, AT(90,15)
                  #VALIDATE(%GraphShowNodeNameVControl<>'',%equSVTextSetControl)
                #ENDENABLE
                #PROMPT('on a zone of the Status Bar',check), %GraphShowNodeNameVOnStatusBar, AT(10,27)
                #ENABLE(%GraphShowNodeNameVOnStatusBar)
                  #PROMPT('',spin(@n2,0,99,1)), %GraphShowNodeNameVZone, AT(110,27,30)
                  #VALIDATE(%GraphShowNodeNameVZone>0,%equSVTextSetZone)
                #ENDENABLE
                #DISPLAY('This text is set in the NodeNameText method.')
              #ENDENABLE
            #ENDTAB
            #TAB('Node Value'), HLP('~TPLGraph_Display_Show_NodeValue')
              #ENABLE(1), SECTION
                #DISPLAY('Display node value as "Node:..."'), AT(3,,182), prop(prop:fontcolor, %equSVTitleFontColor),prop(prop:color,%equSVTitleBgrColor)
                #PROMPT('on Tooltip',check), %GraphShowNodeValueOnTooltip, AT(10,15), DEFAULT(%True)
                #PROMPT('on Window title',check), %GraphShowNodeValueOnWindow, AT(10,27)
                #PROMPT('on the control',check), %GraphShowNodeValueOnControl, AT(10,39)
                #ENABLE(%GraphShowNodeValueOnControl)
                  #PROMPT('',CONTROL), %GraphShowNodeValueControl, AT(90,39)
                  #VALIDATE(%GraphShowNodeValueControl<>'',%equSVTextSetControl)
                #ENDENABLE
                #PROMPT('on a zone of the Status Bar',check), %GraphShowNodeValueOnStatusBar, AT(10,51)
                #ENABLE(%GraphShowNodeValueOnStatusBar)
                  #PROMPT('',spin(@n2,0,99,1)), %GraphShowNodeValueZone, AT(110,51,30)
                  #VALIDATE(%GraphShowNodeValueZone>0,%equSVTextSetZone)
                #ENDENABLE
                #DISPLAY('This text is set in the NodeValueText method.')
              #ENDENABLE
              #ENABLE(1), SECTION
                #DISPLAY('Display node value'), AT(3,,182), prop(prop:fontcolor, %equSVTitleFontColor),prop(prop:color,%equSVTitleBgrColor)
                #BOXED('on the control'),SECTION
                  #PROMPT('for coordinate X',check), %GraphShowNodeValueXOnControl, AT(10,2)
                  #ENABLE(%GraphShowNodeValueXOnControl)
                    #PROMPT('',CONTROL), %GraphShowNodeValueXControl, AT(90,2)
                    #VALIDATE(%GraphShowNodeValueXControl<>'',%equSVTextSetControl)
                  #ENDENABLE
                  #PROMPT('for coordinate Y',check), %GraphShowNodeValueYOnControl, AT(10,15)
                  #ENABLE(%GraphShowNodeValueYOnControl)
                    #PROMPT('',CONTROL), %GraphShowNodeValueYControl, AT(90,15)
                    #VALIDATE(%GraphShowNodeValueYControl<>'',%equSVTextSetControl)
                  #ENDENABLE
                #ENDBOXED
                #BOXED('on a zone of the Status Bar'),SECTION
                  #PROMPT('for coordinate X',check), %GraphShowNodeValueXOnStatusBar, AT(10,2)
                  #ENABLE(%GraphShowNodeValueXOnStatusBar)
                    #PROMPT('',spin(@n2,0,99,1)), %GraphShowNodeValueXZone, AT(90,2,30)
                    #VALIDATE(%GraphShowNodeValueXZone>0,%equSVTextSetZone)
                  #ENDENABLE
                  #PROMPT('for coordinate Y',check), %GraphShowNodeValueYOnStatusBar, AT(10,15)
                  #ENABLE(%GraphShowNodeValueYOnStatusBar)
                    #PROMPT('',spin(@n2,0,99,1)), %GraphShowNodeValueYZone, AT(90,15,30)
                    #VALIDATE(%GraphShowNodeValueYZone>0,%equSVTextSetZone)
                  #ENDENABLE
                #ENDBOXED
                #DISPLAY('This text is set in the NodeXText')
                #DISPLAY('and NodeYText methods.')
              #ENDENABLE
            #ENDTAB
          #ENDSHEET
        #ENDBOXED
      #ENDTAB
    #ENDSHEET
  #ENDBOXED
  #BOXED,WHERE(%GraphCurrentTabLevel1 = 'DataSources'),AT(6,34), SECTION
    #BUTTON('Data Sources'),MULTI(%GraphDataSources, %GraphDataSources &' '& %GraphDataSourceName&' ['&%GraphDataSource&']'),INLINE,AT(6,0,190)
      #SHEET
        #TAB('Data Source'), HLP('~TPLGraph_Data_Source')
          #PROMPT('Source Name:',@s255), %GraphDataSourceName
          #PROMPT('Source:',drop('Queue|File')), %GraphDataSource, DEFAULT('Queue')
          #BOXED('Data From Queue'), WHERE(%GraphDataSource='Queue'), AT(,50), SECTION
            #PROMPT('Queue Name:',OPTFIELD), %GraphDataSourceQueueName
            #BOXED('Sort Fields'), SECTION
              #BUTTON('Sort'), MULTI(%GraphDataSourceQueueSort,(%GraphDataSourceQueueSortFieldSec & %GraphDataSourceQueueSortField)), INLINE, HLP('~TPLGraph_Data_Source_QueueSort')
                #PROMPT('Field:',OPTFIELD), %GraphDataSourceQueueSortField, REQ
                #PROMPT('Sequence:',drop('Ascending[+]|Descending[-]')),%GraphDataSourceQueueSortFieldSec, DEFAULT('+')
              #ENDBUTTON
            #ENDBOXED
            #PROMPT('Record Filter:',EXPR), %GraphDataSourceQueueFilter
          #ENDBOXED
          #BOXED('Data From View'), WHERE(%GraphDataSource='Other View'), AT(,50), SECTION
             #PROMPT('View Name:',EXPR), %GraphDataSourceViewName
             #PROMPT('&Record Filter:',EXPR), %SortRecordFilterView
             #BOXED('Sort Fields'), SECTION
                #BUTTON('Sort'), MULTI(%GraphDataSourceViewSort,(%GraphDataSourceViewSortFieldSec & %GraphDataSourceViewSortField)), INLINE, HLP('~TPLGraph_Data_Source_ViewSort')
                  #PROMPT('Field:',FIELD), %GraphDataSourceViewSortField, REQ
                  #PROMPT('Sequence:', drop('Ascending[+]|Descending[-]')), %GraphDataSourceViewSortFieldSec, DEFAULT('+')
                #ENDBUTTON
             #ENDBOXED
             #PROMPT('SQL Filter:',EXPR), %SortRecordSQLFilterView
          #ENDBOXED
          #BOXED('Data From File'), WHERE(%GraphDataSource='File'), AT(,50), SECTION
             #PROMPT('Key to Use:', key(%Primary)), %SortKey
             #PROMPT('&Record Filter:',EXPR), %SortRecordFilter
             #ENABLE(%SortKey)
               #PROMPT('Range Limit &Field:',COMPONENT(%SortKey)), %SortRangeField
               #ENABLE(%SortRangeField)
                 #PROMPT('Range Limit &Type:',drop('Current Value|Single Value|Range of Values|File Relationship')), %SortRangeLimitType, DEFAULT('Current Value')
                 #BOXED, SECTION
                   #BOXED('Range Limit Boundary'), WHERE(%SortRangeLimitType='Single Value'), AT(,0)
                     #PROMPT('&Range Limit Value:',FIELD), %SortRangeLimit
                   #ENDBOXED
                   #BOXED('Range Limit Boundaries'), WHERE(%SortRangeLimitType='Range of Values'), AT(,0)
                     #PROMPT('&Low Limit Value:',FIELD), %SortRangeLow
                     #PROMPT('&High Limit Value:',FIELD), %SortRangeHigh
                   #ENDBOXED
                   #BOXED('Range Limiting File'), WHERE(%SortRangeLimitType='File Relationship'), AT(,0)
                     #PROMPT('&Related File:',file), %SortRangeFile
                   #ENDBOXED
                 #ENDBOXED
               #ENDENABLE
             #ENDENABLE
             #PROMPT('SQL Filter:',EXPR), %SortRecordSQLFilter
             #BOXED('Additional Sort Fields'), SECTION
                #BUTTON('Additional Sort Fields'), MULTI(%GraphDataSourceFileSort,(%GraphDataSourceFileSortFieldSec& %GraphDataSourceFileSortField)), INLINE, HLP('~TPLGraph_Data_Source_FileSort')
                  #PROMPT('Field:',FIELD), %GraphDataSourceFileSortField, REQ
                  #PROMPT('Sequence:',drop('Ascending[+]|Descending[-]')), %GraphDataSourceFileSortFieldSec, DEFAULT('+')
                #ENDBUTTON
             #ENDBOXED
          #ENDBOXED
        #ENDTAB
        #TAB('Points'), HLP('~TPLGraph_Data_Points')
          #BUTTON('Points'), MULTI(%GraphDataPoints, choose(~%GraphPointSerieName,'',%GraphPointSerieName &' [')& %GraphPointAction &' ('& %GraphPointField &')'& choose(~%GraphPointSerieName,'',']')), INLINE, HLP('~TPLGraph_Data_Points_Point')
            #PROMPT('Series Name',@S255), %GraphPointSerieName
            #VALIDATE(~%GraphCheckSeriesName(),'The name must be unique')
            #INSERT(%GraphFillPrompts)
            #DISPLAY('')
            #PROMPT('Grouping of point',drop(%equSVYesNo)), %GraphPointGrouping, DEFAULT(%equSVFalse)
            #ENABLE(%GraphPointGrouping<>%equSVFalse)
              #PROMPT('Name',optfield), %GraphPointGroupingName
              #PROMPT('Text',EXPR), %GraphPointGroupingText
            #ENDENABLE
            #ENABLE(%GraphPointGrouping=%equSVFalse)
              #PROMPT('Point',optfield), %GraphPointXvalue
            #ENDENABLE
            #DISPLAY('')
            #PROMPT('Record Action',drop('Graph[Graph]|Count[Count]|Sum[Sum]|Average[Average]')), %GraphPointAction, DEFAULT('Graph')
            #ENABLE(%GraphPointAction<>'Count')
              #PROMPT('Field',FIELD), %GraphPointField
            #ENDENABLE
            #ENABLE(%GraphPointAction<>'Graph')
              #PROMPT('Value &Filter',@S255), %GraphPointFilter
            #ENDENABLE
            #PROMPT('Field name',expr), %GraphPointFieldName
            #ENABLE(1), SECTION
              #DISPLAY('')
              #BOXED(''), WHERE(%GraphPointAction='Graph'), AT(,0), SECTION
              #DISPLAY('Each record will be a point into the graphic.')
              #DISPLAY('')
              #DISPLAY('')
              #ENDBOXED
              #BOXED(''), WHERE(%GraphPointAction='Sum'), AT(,0), SECTION
              #DISPLAY('You need to specify a break and then the total')
              #DISPLAY('sum for each break will became the point')
              #DISPLAY('into the graphic.')
              #ENDBOXED
              #BOXED(''), WHERE(%GraphPointAction='Count'), AT(,0), SECTION
              #DISPLAY('You need to specify a break and then the total')
              #DISPLAY('count for each break will became the point')
              #DISPLAY('into the graphic.')
              #ENDBOXED
              #BOXED(''), WHERE(%GraphPointAction='Average'), AT(,0), SECTION
              #DISPLAY('You need to specify a break and then the average')
              #DISPLAY('for each break will became the point into')
              #DISPLAY('the graphic.')
              #ENDBOXED
            #ENDENABLE
            #BOXED('Break'), WHERE(%GraphPointAction<>'Graph'), SECTION
              #BUTTON('Break Fields'), MULTI(%GraphPointBreak,%GraphPointBreakField), INLINE, AT(,,,60), HLP('~TPLGraph_Data_Points_Break')
                #PROMPT('Field:',FIELD), %GraphPointBreakField, REQ
              #ENDBUTTON
            #ENDBOXED
            #DISPLAY('On the field you can use these variables:')
            #DISPLAY('Graph:TotalRecords : Records readed on these source')
            #DISPLAY('Graph:PointNumber  : Point number on these source')
            #DISPLAY('Graph:SPointNumber : Point number on these point set')
          #ENDBUTTON
        #ENDTAB
        #TAB('Hot Fields'), WHERE(%GraphDataSource='File' OR %GraphPointBreakHasBreaks()), HLP('~TPLGraph_Data_HotFields')
          #BUTTON('Hot Fields'), MULTI(%GraphHotFields,%GraphHotField), INLINE, AT(,,,60), HLP('~TPLGraph_Data_HotFields_Field')
            #PROMPT('Field:',FIELD), %GraphHotField, REQ
            #PROMPT('BIND Only',check), %GraphHotFieldBindOnly, DEFAULT(%False)
          #ENDBUTTON
          #DISPLAY('You need to specify a Hot Field if you want')
          #DISPLAY('to use that field in a filter or in any expresion.')
          #DISPLAY('')
          #BOXED(''), WHERE((%GraphPointAction<>'Graph' AND ITEMS(%GraphPointBreak)))
          #DISPLAY('If you are using the break to calculate, totals, sum')
          #DISPLAY('or average you will need to add any field involved ')
          #DISPLAY('into the calculation to the `Hot Fields` regardless')
          #DISPLAY('of using a Queue or a File.')
          #ENDBOXED
        #ENDTAB
      #ENDSHEET
    #ENDBUTTON
  #ENDBOXED
  #BOXED, WHERE(%GraphCurrentTabLevel1 = 'Class'), AT(6,32)
    #BUTTON('Graph Class'), AT(,,180), HLP('~TPLGraph_GraphClass')
      #WITH(%ClassItem, 'GRP' & %ActiveTemplateInstance)
        #INSERT(%ClassPrompts(ABC))
      #ENDWITH
    #ENDBUTTON
    #BOXED,WHERE(%GraphHasBreaks()), SECTION
      #BUTTON('Break Class'), AT(,,180), HLP('~TPLGraph_BreakClass')
        #PROMPT('Break:', FROM(%pClassName)), %DefaultBreakBaseClassType, DEFAULT('BreakManagerClass'), REQ
      #ENDBUTTON
    #ENDBOXED
  #ENDBOXED
#!------------------------------------------------------------------------------------------
#GROUP(%GraphFillPrompts)
  #BUTTON('Fill'), AT(5,,185), HLP('~TPLGraph_Data_Filling')
    #PROMPT('Style',drop('Solid[0]')), %GraphFillStyle, DEFAULT('0')
    #BOXED('Color'), SECTION
      #PROMPT('Type',drop(%equSVDefault&'|Fixed[Fixed]|Variable[Variable]')), %GraphColorType, DEFAULT(%equSVDefault)
      #BOXED(''), WHERE(%GraphColorType='Fixed'), AT(,15), SECTION
        #PROMPT('Color',COLOR), %GraphColorFixed, DEFAULT('COLOR:Blue')
      #ENDBOXED
      #BOXED(''), WHERE(%GraphColorType='Variable'), AT(,15), SECTION
        #PROMPT('Color',OPTFIELD), %GraphColorVariable
      #ENDBOXED
      #BOXED(''), WHERE(%GraphColorType=%equSVDefault), AT(,15), SECTION
        #DISPLAY('The color will be generated in runtime.')
      #ENDBOXED
    #ENDBOXED
  #ENDBUTTON
#!------------------------------------------------------------------------------------------
#GROUP(%GraphDrillDownPrompts), HLP('~TPLProcBrowse')
  #ENABLE(%GraphAccessibleDrillDown)
    #PROMPT('Drill Down',check), %GraphDrillDown, AT(10)
    #ENABLE(%GraphDrillDown)
      #BOXED('')
        #PROMPT('Control',FROM(%Control,%Control<>%LocalControlName)), %GraphDrillDownControl, AT(90)
        #VALIDATE(%GraphDrillDownControl<>'',%equSVTextSetControl)
        #BUTTON('Controls to Hide'), HLP('~TPLGraph_Display_Drilldown_Hide')
          #PROMPT('Controls to Hide',FROM(%Control, %GraphDrillDownFilterControls(%GraphDrillDownControlsHide))), %GraphDrillDownControlsHide, MULTI('Controls to Hide'), INLINE
        #ENDBUTTON
        #BUTTON('Controls to UnHide'), HLP('~TPLGraph_Display_Drilldown_UnHide')
          #PROMPT('Controls to UnHide',FROM(%Control, %GraphDrillDownFilterControls(%GraphDrillDownControlsUnHide))), %GraphDrillDownControlsUnHide, MULTI('Controls to UnHide'), INLINE
        #ENDBUTTON
        #DISPLAY('Is used in a method DrillDown.')
        #DISPLAY('Hides the diagram, unhides the Control')
        #DISPLAY('and establishes focus on the control.')
      #ENDBOXED
    #ENDENABLE
    #DISPLAY
    #PROMPT('Return from Drill Down',check), %GraphReturnFromDrillDown, AT(10)
    #ENABLE(%GraphReturnFromDrillDown)
      #BOXED('')
        #PROMPT('Control',FROM(%Control,%Control<>%LocalControlName)), %GraphReturnFromDrillDownControl, AT(90)
        #VALIDATE(%GraphReturnFromDrillDownControl<>'',%equSVTextSetControl)
        #BUTTON('Controls to Hide'), HLP('~TPLGraph_Display_ReturnFromDrilldown_Hide')
          #PROMPT('Controls to Hide',FROM(%Control, %GraphDrillDownFilterControls(%GraphReturnFromDrillDownControlsHide))), %GraphReturnFromDrillDownControlsHide, MULTI('Controls to Hide'), INLINE
        #ENDBUTTON
        #BUTTON('Controls to UnHide'), HLP('~TPLGraph_Display_ReturnFromDrilldown_UnHide')
          #PROMPT('Controls to UnHide',FROM(%Control, %GraphDrillDownFilterControls(%GraphReturnFromDrillDownControlsUnHide))), %GraphReturnFromDrillDownControlsUnHide, MULTI('Controls to UnHide'), INLINE
        #ENDBUTTON
        #DISPLAY('Used in the ReturnFromDrillDown method.')
        #DISPLAY('Unhides the diagram and hides the Control.')
      #ENDBOXED
    #ENDENABLE
  #ENDENABLE
#!------------------------------------------------------------------------------------------
#GROUP(%GraphDrillDownFilterControls, *%parMulti), auto, preserve
  #IF(%Control<>%LocalControlName AND (%Control=%parMulti OR ~inlist(%Control,%parMulti)))
    #RETURN(%True)
  #ENDIF
  #RETURN('')
#!------------------------------------------------------------------------------------------
#GROUP(%GraphSyncPrintStretch, long %parFlag=%False)
  #IF(%parFlag)
    #IF(%GraphInitPrintStretchH)
      #SET(%GraphInitPrintStretch, '01b')
    #ELSIF(%GraphInitPrintStretchV)
      #SET(%GraphInitPrintStretch, '10b')
    #ELSE
      #SET(%GraphInitPrintStretch, '0')
    #ENDIF
  #ELSE
    #SET(%GraphInitPrintStretchH, %False)
    #SET(%GraphInitPrintStretchV, %False)
    #CASE(%GraphInitPrintStretch)
    #OF('01b')
      #SET(%GraphInitPrintStretchH, %True)
    #OF('10b')
      #SET(%GraphInitPrintStretchV, %True)
    #ENDCASE
  #ENDIF
#!------------------------------------------------------------------------------------------
#GROUP(%GraphPrintStretch), AUTO
  #DECLARE(%locRet)
  #!.....................................................................................
  #IF(%GraphInitPrintProportional<>%equSVTrue)
    #SET(%locRet, choose(~%GraphInitPrintStretchV, '', '1') & %GraphInitPrintStretchH)
    #IF(int(%locRet)>0)
      #SET(%locRet, %locRet & 'b')
    #ENDIF
  #ELSE
    #SET(%locRet, %GraphInitPrintStretch)
  #ENDIF
  #RETURN(%locRet)
#!------------------------------------------------------------------------------------------
#INCLUDE('svFnGrp.tpw')
#INCLUDE('svGraphD.tpw')
#INCLUDE('PROCBIND.TPW')
