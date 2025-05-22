//////////////////////////////////////////////////////////////////////
//
//  TEST.PRG
//
//  Copyright:
//      Alaska Software, (c) 1997-2025. All rights reserved.         
//  
//  Contents:
//      This file contains the code for a sample application 
//      that illustrates usage of the XbpSplitbar class.
//      Furthermore, it shows how to create "Windows Explorer-style"
//      forms
//   
//////////////////////////////////////////////////////////////////////

#include "Gra.ch"
#include "Xbp.ch"
#include "Appevent.ch"
#include "Font.ch"
#include "Common.ch"
#include "Dll.ch"


****************
/*
 * Procedure Main()
 */
PROCEDURE Main
   LOCAL nEvent, mp1, mp2
   LOCAL oDlg, oXbp, drawingArea, aEditControls := {}
   LOCAL oXbp1, oXbp2, oXbp3, oXbp4, oXbp5, oXbp6

   //
   // Create MDI child window displayed in the main
   // window's drawingarea
   //
   oDlg := XbpDialog():new( SetAppWindow():DrawingArea, , {100,100}, {607,429}, , .F.)
   oDlg:taskList := .T.
   oDlg:title := "New form"
   oDlg:ClipChildren := .T.
   oDlg:DrawingArea:ClipChildren := .T.
   oDlg:create()

   drawingArea := oDlg:drawingArea
   drawingArea:setFontCompoundName( "8.Arial" )

   //
   // Create vertical splitbar object
   //
   oXbp2 := XbpSplitbar():new( drawingArea, , {204,0}, {8,372} )
   oXbp2:clipSiblings := .T.
   oXbp2:orientation  := XBPSPLITBAR_VERTICAL       // this is a vertical XbpSplitbar
   oXbp2:create()
   oXbp2:LiveRedraw   := .F.                        // don't render while dragging
   oXbp2:setColorBG( GRA_CLR_BLUE )                 // assign some fancy color

   // Create XBPs to be separated by the splitbar
   // and set them
   oXbp1 := XbpMle():new( drawingArea, , {0,0}, {204,372}, { { XBP_PP_BGCLR, XBPSYSCLR_ENTRYFIELD } } )
   oXbp1:clipSiblings := .T.
   oXbp1:create()

   oXbp3 := XbpStatic():new( drawingArea, , {212,0}, {340,372} )
   oXbp3:clipSiblings := .T.
   oXbp3:type := XBPSTATIC_TYPE_RECESSEDBOX
   oXbp3:ClipChildren := .T.
   oXbp3:create()

   oXbp2:Predecessor  := oXbp1
   oXbp2:Successor    := oXbp3

   //
   // Create horizontal splitbar object
   //
   // Note that we do not explicitly set the XBPs that
   // should be controlled. This is done implicitly by
   // the splitbar object via its parent object's 
   // childlist!
   oXbp4 := XbpMle():new( oXbp3, , {2,2}, {244,188}, { { XBP_PP_BGCLR, XBPSYSCLR_ENTRYFIELD } } )
   oXbp4:clipSiblings := .T.
   oXbp4:create()

   oXbp5 := XbpSplitbar():new( oXbp3, , {2,190}, {244,8} )
   oXbp5:clipSiblings := .T.
   oXbp5:orientation  := XBPSPLITBAR_HORIZONTAL
   oXbp5:create()
   oXbp5:setColorBG( GRA_CLR_GREEN )

   oXbp6 := XbpMle():new( oXbp3, , {2,198}, {244,172}, { { XBP_PP_BGCLR, XBPSYSCLR_ENTRYFIELD } } )
   oXbp6:clipSiblings := .T.
   oXbp6:create()

   oDlg:show()

   nEvent := xbe_None
   DO WHILE nEvent <> xbeP_Close
      nEvent := AppEvent( @mp1, @mp2, @oXbp )
      oXbp:handleEvent( nEvent, mp1, mp2 )
   ENDDO

RETURN


****************
/*
 * Overloaded AppSys() for creating the application's
 * main window
 */
PROCEDURE AppSys()

   LOCAL oDlg := XbpExplorerDialog():New():Create()

   // An overloaded AppSys() procedure should at 
   // least set a default window for later retrieval
   SetAppWindow( oDlg )

RETURN


****************
/*
 * Implementation of a dialog class that implements an
 * "Explorer-style" dialog window
 */

// Width of the treeview object within the dialog
#define TV_WIDTH   200
// Width of the splitbar object within the dialog
#define SP_WIDTH   6

CLASS XbpExplorerDialog FROM XbpDialog

EXPORTED:

  // Dialog components
  VAR oTreeview
  VAR oSplitbar

  // Lifecyle
  METHOD INIT, Create

  // layout Management
  METHOD Resize
  METHOD Rearrange

ENDCLASS


/* **********************************************************************
 * Initialization of the dialog object
 */
METHOD XbpExplorerDialog:init( oParent, oOwner, aPos, aSize, aPP, lVisible )

  DEFAULT aPos  TO { 75, 75 }
  DEFAULT aSize TO { AppDesktop():CurrentSize()[1] - 150,     ;
                     AppDesktop():CurrentSize()[2] - 150 }
  DEFAULT oParent TO AppDesktop()
  DEFAULT oOwner  TO SetAppWindow()

  ::XbpDialog:init( oParent, oOwner, aPos, aSize, aPP, lVisible )
  ::TaskList  := TRUE
  ::Title     := "Explorer Test Dialog"
  ::drawingArea:ClipChildren := TRUE
  ::drawingArea:ClipSiblings := .T.
  ::ClipChildren := TRUE
  ::Close     := { || AppQuit() }

  // Note: XbpTreeview uses the internal XbpDialog object 
  // _directly_ as its parent object! This is VERY uncommon;
  // normally the :drawingArea of an XbpDialog should be 
  // used as parent for its embedded XBPs. However, using
  // a dialog as the direct parent is required in certain
  // circumstances, eg. when the dialog's :drawingArea 
  // should be a sibling of the child being created
  ::oTreeView := XbpTreeView():New( self,,,{ TV_WIDTH, 300 } )
  ::oTreeview:HasLines   := .T.
  ::oTreeview:HasButtons := .T.
  ::oTreeview:ClipSiblings := .T.

  // the XbpSlitbar uses the XbpDialog as well as parent
  // for any further explanation see above
  ::oSplitBar := XbpSplitbar():New( self )

RETURN self


/* **********************************************************************
 * Create the dialog object
 */
METHOD XbpExplorerDialog:Create( oParent, oOwner, aPos, aSize, aPP, lVisible )
  LOCAL oItem

  ::XbpDialog:Create( oParent, oOwner, aPos, aSize, aPP, FALSE )
  ::DrawingArea:SetColorBG( XBPSYSCLR_APPWORKSPACE )
  ::MinSize := { TV_WIDTH + 50, 100 }

  aSize := ::CurrentSize()

  //
  // Create the dialog's components; a treeview and a splitbar
  // object that separates the treeview from the dialog's
  // :drawingArea
  //
  ::oTreeView:Create()

  ::oSplitBar:Create(,, { TV_WIDTH, 0 }, { SP_WIDTH, aSize[2] } )

  ::oSplitBar:Predecessor := ::oTreeView
  ::oSplitBar:Successor   := ::DrawingArea

  ::Show()

  ::Menubar():AddItem( { "~Quit", {|| AppQuit() } } )
  ::Rearrange()

  oItem := ::oTreeView:RootItem:AddItem( "Test Item #1" )
  oItem := ::oTreeView:RootItem:AddItem( "Test Item #2" )

RETURN self


/* **********************************************************************
 * Resize the dialog object; this also adjusts the size of the embedded
 * components
 */
METHOD XbpExplorerDialog:ReSize( aSizeOld, aSizeNew )

  ::XbpDialog:ReSize( aSizeOld, aSizeNew )
  ::Rearrange()

RETURN SELF


/* **********************************************************************
 * Rearrange the dialog's components; this recomputes the basic layout
 * after the dialog's size was changed
 */
METHOD XbpExplorerDialog:Rearrange()

   STATIC nDiff     := 0
   LOCAL aPosSP     := ::oSplitbar:CurrentPos()
   LOCAL aSize
   LOCAL aSizeDA    := ::DrawingArea:CurrentSize()
   LOCAL nNewValue
   LOCAL nTitlebar     
   LOCAL nBorder
   LOCAL aTmp

   //
   // Resize :drawingArea, XbpSplitbar and XbpTreeview
   //
   aSize     := ::CurrentSize()

   aTmp      := ::CalcClientRect( {0,0,aSize[1],aSize[2]} )
   nBorder   := (aSize[1] - aTmp[3]) /2
   nTitleBar := (aSize[2] - aTmp[4]) - (nBorder *2)

   aTmp      := ::CalcClientRect( {0,0,aSize[1],aSize[2]} )
   nBorder   := (aSize[1] - aTmp[3]) /2
   nTitleBar := (aSize[2] - aTmp[4]) - (nBorder *2)

   // Compute space occupied by dialog's titlebar, border
   // and menubar
   aSizeDA[2] := aSize[ 2 ] - (nTitleBar + nBorder *2)
   nNewValue  := aSize[1] - ( aPosSP[1] + SP_WIDTH + nBorder * 2 )

   ::DrawingArea:SetSize( { nNewValue, aSizeDA[2] } )
   ::DrawingArea:SetPos( { ::oSplitBar:CurrentPos()[1] + SP_WIDTH, 0 } )
   ::oSplitbar:SetSize( { SP_WIDTH, aSizeDA[2] } )
   ::oTreeview:SetSize( { aPosSP[1], aSizeDA[2] } )

RETURN SELF


/* **********************************************************************
 * Procedure implicitly called when either the dialog's close button or
 * the menu item "Quit" is selected
 */
PROCEDURE AppQuit()
   QUIT
RETURN

// EOF
