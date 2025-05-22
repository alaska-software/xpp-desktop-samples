//////////////////////////////////////////////////////////////////////
//
//  CHILDWIN.PRG
//
//  Copyright:
//      Alaska Software, (c) 1997-2025. All rights reserved.         
//  
//  Contents:
//      The program demonstrates how to define or change parent-child
//      relationships between windows (XbaseParts).
//   
//////////////////////////////////////////////////////////////////////


#include "Appevent.ch"
#include "Common.ch"
#include "Font.ch"
#include "Gra.ch"
#include "Xbp.ch"


/*
 * Define an XbpDialog window as application window
 */
PROCEDURE AppSys
   LOCAL oDlg, aPos[2], aSize

   aSize             := SetAppWindow():currentSize()
   aPos[1]           := 0.1 * aSize[1]
   aPos[2]           := 0.1 * aSize[2]
   aSize[1]          := 640
   aSize[2]          := 400

   oDlg              := XbpDialog():new( ,, aPos, aSize )
   oDlg:title        := "MDI window"
   oDlg:taskList     := .T.
   oDlg:close        := {|| PostAppEvent( xbeP_Quit ) }
   oDlg:create()
   oDlg:drawingArea:setFontCompoundName( FONT_HELV_SMALL )

   AppMenu( oDlg:menuBar() )

   SetAppWindow( oDlg )
   SetAppFocus( oDlg )
RETURN



/*
 * Menu must exist in the MDI application window
 */
PROCEDURE AppMenu( oMenuBar )
   LOCAL oMenu := XbpMenu():new( oMenuBar )
   oMenu:title := "Application"
   oMenu:create()

   oMenu:addItem( { "Quit", {|| PostAppEvent( xbeP_Quit ) } } )

   oMenuBar:addItem( { oMenu, } )
RETURN



/*
 * Create pushbuttons in the application window
 */
PROCEDURE Main

   LOCAL nEvent := NIL, mp1 := NIL, mp2 := NIL, oXbp := NIL, drawingArea

  /*
   * Since the window will contain child windows, :clipSiblings
   * must be set .T. (Child windows are siblings of the pushbuttons)
   */
   drawingArea       := SetAppWindow():drawingArea
   oXbp              := XbpPushbutton():new( drawingArea ,,{12,12}, {100,30} )
   oXbp:caption      := ":create()"
   oXbp:clipSiblings := .T.
   oXbp:activate     := {|| CreateChildWindow() }
   oXbp:create()

   oXbp              := XbpPushbutton():new( drawingArea ,,{124,12}, {100,30} )
   oXbp:caption      := "Quit"
   oXbp:clipSiblings := .T.
   oXbp:activate     := {|| PostAppEvent( xbeP_Quit ) }
   oXbp:create()

   DO WHILE nEvent <> xbeP_Quit
      nEvent := AppEvent( @mp1, @mp2, @oXbp )
      oXbp:handleEvent( nEvent, mp1, mp2 )
   ENDDO

RETURN



/*
 * Create a child window
 */
PROCEDURE CreateChildWindow
   STATIC snCount := 1
   LOCAL oParent, oDlg
#ifdef __OS2__
   LOCAL oXbp
#endif

  /*
   * This program uses an XbpDialog window as application window.
   * Therefore, the parent must be :drawingArea
   */
   oParent := SetAppWindow():drawingArea

   oDlg          := XbpDialog():new( oParent, , {48,48}, {240,180} )
   oDlg:title    := "Child window #" + Ltrim(Str( snCount++ ) )
   oDlg:taskList := .F.
   oDlg:close    := {|mp1,mp2,obj| obj:destroy() }
   oDLg:clipSiblings := .T.
   oDlg:create()
   oDlg:drawingArea:setFontCompoundName( FONT_HELV_SMALL )

  /*
   * Create pushbutton in the drawing area of the child window
   */
#ifdef __OS2__
   oParent       := oDlg:drawingArea
   oXbp          := XbpPushbutton():new(oParent,,{12,12}, {100,30} )
   oXbp:caption  := "Toggle Parent"
   oXbp:activate := {|mp1,mp2,obj| ToDesktop( obj ) }
   oXbp:create()
#endif
RETURN



/*
 * Move MDI child window to the desktop
 */
PROCEDURE ToDesktop( oPushButton )
   LOCAL oDlg, oParent

   oPushButton:activate := {|mp1,mp2,obj| ToMDIwindow( obj ) }

  /*
   * Determine the XbpDialog window which contains this pushbutton
   * from the parent-child relationship
   */
   oDlg    := oPushButton:setParent()    // drawing area
   oDlg    := oDlg:setParent()           // XbpDialog

  /*
   * The desktop window becomes the parent
   */
   oParent := AppDesktop()
   oDlg:setParent( oParent )
   oDlg:setOwner( oParent )

RETURN



/*
 * Move previous MDI child window back to application window
 */
PROCEDURE ToMDIwindow( oPushButton )
   LOCAL oDlg, oParent

   oPushButton:activate := {|mp1,mp2,obj| ToDesktop( obj ) }

  /*
   * Determine the XbpDialog window which contains this pushbutton
   * from the parent-child relationship
   */
   oDlg    := oPushButton:setParent()    // drawing area
   oDlg    := oDlg:setParent()           // XbpDialog

  /*
   * The parent window is :drawingArea again
   */
   oParent := SetAppwindow():drawingArea
   oDlg:setParent( oParent )
   oDlg:setOwner( oParent )

RETURN
