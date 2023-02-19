!ABCIncludeFile(GRAPH)

!=================
! Version: 1.1.039
!=================

OMIT('_EndOfInclude_',_GraphPresent_)
_GraphPresent_ equate(1)

 include('SVGraphB.inc'),once
!----------------------------------------------------------------------------------------
! "GraphClass"
!----------------------------------------------------------------------------------------
GraphClass          class(GraphAxisClass),implements(iDiagram),implements(iNode),type,module('SVGraph.clw'),link('SVGraph.clw',_ABCLinkMode_),dll(_ABCDllMode_)
!
gShowMouse            like(gShowToType)
gShowMouseX           like(gShowToType)
gShowMouseY           like(gShowToType)
gShowDiagramName      like(gShowToType)
gShowDiagramNameV     like(gShowToType)
gShowNodeName         like(gShowToType)
gShowNodeNameV        like(gShowToType)
gShowNodeValue        like(gShowToType)
gShowNodeValueX       like(gShowToType)
gShowNodeValueY       like(gShowToType)
eShowSBonFirstThread  bool                          ! If SatusBar in target is not added then to show on SatusBar of first thread
!
eSumYmax              real
! Initializing object
Init                  procedure(window parWin, long parFParent, real parL=0, real parT=0, real parR=0, real parB=0) !,EXTENDS
Kill                  procedure !,EXTENDS
! Set initial values
SetDefault            procedure(), virtual
! Send message to the object (to parent object eFParent)
PostEvent             procedure(long parEvent), virtual
! Handle events (which has been sent to the parent object eFParent, and controls)
TakeEvent             procedure(), virtual
TakeEventOfParent     procedure, virtual
CalcPopup             procedure(PopupClass PopupMgr), virtual
CalcPopupAdd2         procedure(PopupClass PopupMgr, string parText, string parName, long parItems, long parItemMask, long parCheck, long parCheckValue), long, virtual
PopupAsk              procedure(PopupClass PopupMgr), string, virtual
Popup                 procedure, virtual
BeginRefresh          procedure(), bool, virtual
Refresh               procedure(byte parRefresh=false), virtual ! Refresh object
Resize                procedure(long parResize=false), virtual  ! Set object's size
Draw                  procedure(), virtual          ! To draw object
CalcGraph             procedure(), virtual          ! To calculate object
CalcCurrentNode       procedure(), virtual
CalcCurrentGraph      procedure(), virtual
CalcBestPositionNodeText procedure(), virtual
DrawGraph             procedure(), virtual          ! To draw diagrams
DrawBarWithAccumulation procedure(), signed, virtual
DrawWallPaper         procedure(<unsigned parStyle>), virtual
DrawReport            procedure(*report parReport, long parBand, queue parQRpt, bool parBestFit=false), virtual
PrintGraph            procedure(bool parBestFit=false), virtual
SaveGraph             procedure(bool parAsk=false), virtual
SaveAsGraph           procedure, virtual
MouseText             procedure(bool parShow=true), string, virtual
MouseXText            procedure(bool parShow=true), string, virtual
MouseYText            procedure(bool parShow=true), string, virtual
DiagramNameText       procedure(bool parShow=true), string, virtual
DiagramText           procedure(bool parShow=true), string, virtual
NodeXText             procedure(bool parShow=true), string, virtual
NodeYText             procedure(bool parShow=true), string, virtual
NodeValueText         procedure(bool parShow=true), string, virtual
NodeNameText          procedure(bool parShow=true), string, virtual
NodeTipText           procedure(bool parShow=true), string, virtual
NodeText              procedure(bool parShowName=true, bool parShowValue=true), string, virtual
AllText               procedure(), string, virtual
Interactivity         procedure, virtual
GetValueFromStatusBar procedure(long parN), string, virtual
ShowOnStatusBar       procedure(string parText, long parN), virtual
GetValueFromField     procedure(long parField), string, virtual
ShowOnField           procedure(string parText, long parField), virtual
ToShowValues          procedure, virtual
ToolTip               procedure, virtual
DrillDown             procedure, virtual
DrillDown             procedure(long parGraphNpp, string parGraphName, long parNodeNpp, string parNodeName, real parNodeX, real parNodeY), virtual
ReturnFromDrillDown   procedure, virtual
ReturnFromDrillDown   procedure(long parGraphNpp, string parGraphName, long parNodeNpp, string parNodeName, real parNodeX, real parNodeY), virtual
GetMouse              procedure, virtual
IsOverNode            procedure(), bool, virtual
FindNearbyNodes       procedure(real parX, real parY), long, proc, virtual
                    end
!----------------------------------------------------------------------------------------

 _EndOfInclude_
