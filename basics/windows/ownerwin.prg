//////////////////////////////////////////////////////////////////////
//
//  OWNERWIN.PRG
//
//  Copyright:
//      Alaska Software, (c) 1997-2025. All rights reserved.         
//  
//  Contents:
//      The program demonstrates owner relations between windows. If the
//      parent is not identical with the owner, a window becomes dependent
//      from the owner (it is owned).
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
   LOCAL oDlg, aPos[2], aSize[2]

   aSize[1]      := 400
   aSize[2]      := 300
   aPos[1]       := 0.1 * aSize[1]
   aPos[2]       := 0.1 * aSize[2]

   oDlg          := XbpDialog():new( ,, aPos, aSize )
   oDlg:title    := "Application Window"
   oDlg:taskList := .T.
   oDlg:close    := {|| PostAppEvent( xbeP_Quit ) }
   oDlg:create()
   oDlg:drawingArea:setFontCompoundName( FONT_HELV_SMALL )
   SetAppWindow( oDlg )
   SetAppFocus( oDlg )
RETURN



/*
 * Create pushbuttons in the application window
 */
PROCEDURE Main
   LOCAL nEvent := NIL, mp1 := NIL, mp2 := NIL, oXbp
   LOCAL oParent := SetAppWindow():drawingArea

   oXbp          := XbpPushbutton():new( oParent, , {12,12}, {100,30} )
   oXbp:caption  := "Attached"
   oXbp:activate := {|| CreateOwnedWindow( .T. ) }
   oXbp:create()

   oXbp          := XbpPushbutton():new( oParent, , {124,12}, {100,30} )
   oXbp:caption  := "Free"
   oXbp:activate := {|| CreateOwnedWindow( .F. ) }
   oXbp:create()

   oXbp          := XbpPushbutton():new( oParent, , {236,12}, {100,30} )
   oXbp:caption  := "Quit"
   oXbp:activate := {|| PostAppEvent( xbeP_Quit ) }
   oXbp:create()

   DO WHILE nEvent <> xbeP_Quit
      nEvent := AppEvent( @mp1, @mp2, @oXbp )
      oXbp:handleEvent( nEvent, mp1, mp2 )
   ENDDO

RETURN


/*
 * Create an XbpDialog window that is owned by the application window
 */
PROCEDURE CreateOwnedWindow( lMoveWithOwner )
   STATIC snCount := 1
   LOCAL oParent, oDlg, oXbp, aPos, aSize

  /*
   * Create owned window at the right edge of the owner window
   */
   aSize := SetAppWindow():currentSize()
   aPos  := SetAppWindow():currentPos()

   aPos  := { aPos[1] + aSize[1] + 2, aPos[2] + aSize[2] - 150 }
   aPos[1] := IF ( aPos[1] > AppDesktop():CurrentSize()[1], AppDesktop():CurrentSize()[1] - 250, aPos[1] )
   aPos[2] := IF ( aPos[2] > AppDesktop():CurrentSize()[2], AppDesktop():CurrentSize()[2] - 250, aPos[2] )
   aSize := { 250, 150 }

  /*
   * Create dialog window with the desktop as parent and
   * the application window as owner.
   * :moveWithOwner defines whether or not this window moves
   * together with the application window.
   */
   oDlg               := XbpDialog():new( AppDesktop(), SetAppWindow(), aPos, aSize )
   IF lMoveWithOwner == .T.
      oDlg:title         := "Owned window #" + Ltrim(Str( snCount++ ) ) + " (attached)"
   ELSE
      oDlg:title         := "Owned window #" + Ltrim(Str( snCount++ ) )
   ENDIF
   oDlg:taskList      := .F.
   oDlg:moveWithOwner := lMoveWithOwner
   oDlg:close         := {|mp1,mp2,obj| obj:destroy(), SetAppFocus( SetAppWindow() ) }
   oDlg:create()
   oDlg:drawingArea:setFontCompoundName( FONT_HELV_SMALL )

  /*
   * Create static in the drawing area of the owned window for
   * displaying the current "mode"
   */
   oParent       := oDlg:drawingArea
   oXbp          := XbpStatic():new( oParent,, {12,80} )
   oXbp:type     := XBPSTATIC_TYPE_TEXT
   oXbp:autoSize := .T.
   IF lMoveWithOwner == .T.
      oXbp:caption  := "Window moves with Owner"
   ELSE
      oXbp:caption  := "Window does not move with Owner"
   ENDIF
   oXbp:create()

  /*
   * Create pushbutton in the drawing area of the owned window
   */
   oXbp          := XbpPushbutton():new( oParent,, {12,12}, {100,30} )
   oXbp:caption  := "Close"
   oXbp:activate := {|| PostAppEvent( xbeP_Close,,, oDlg ) }
   oXbp:create()
RETURN
