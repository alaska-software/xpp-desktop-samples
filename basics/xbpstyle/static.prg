//////////////////////////////////////////////////////////////////////
//
//  STATIC.PRG
//
//  Copyright:
//      Alaska Software, (c) 1997-2025. All rights reserved.         
//  
//  Contents:
//      The program demonstrates various possibilities of using
//      XbpStatic objects.
//   
//////////////////////////////////////////////////////////////////////


#include "Appevent.ch"
#include "Font.ch"
#include "Gra.ch"
#include "Xbp.ch"


/*
 * Display XbpStatic objects with different types and effects
 */
PROCEDURE Main
   LOCAL nEvent := NIL, mp1 := NIL, mp2 := NIL, oXbp
   LOCAL aStatics := {}

   SetAppWindow():useShortCuts := .T.
   SetColor( "N/W" )
   CLS

   @ 0, 0 SAY "Examples of how XbpStatic objects can appear"

   oXbp          := XbpStatic():new(,, {12,54}, {616,2} )
   oXbp:type     := XBPSTATIC_TYPE_RAISEDLINE
   oXbp:create()

   oXbp          := XbpPushbutton():new(,, {12,12}, {100,30} )
   oXbp:caption  := "Text"
   oXbp:activate := {|| StaticText( aStatics ) }
   oXbp:create()
   oXbp:setFontCompoundName( FONT_HELV_SMALL )

   oXbp          := XbpPushbutton():new(,, {124,12}, {100,30} )
   oXbp:caption  := "Boxes"
   oXbp:activate := {|| StaticBox( aStatics ) }
   oXbp:create()
   oXbp:setFontCompoundName( FONT_HELV_SMALL )

   oXbp          := XbpPushbutton():new(,, {236,12}, {100,30} )
   oXbp:caption  := "Quit"
   oXbp:activate := {|| PostAppEvent( xbeP_Close ) }
   oXbp:create()
   oXbp:setFontCompoundName( FONT_HELV_SMALL )

   DO WHILE nEvent <> xbeP_Close
      nEvent := AppEvent( @mp1, @mp2, @oXbp )
      oXbp:handleEvent( nEvent, mp1, mp2 )
   ENDDO

RETURN



/*
 * Display XbpStatic objects with different colors and fonts
 */

PROCEDURE StaticText( aStatics )
   LOCAL aPos := {120,300}
   LOCAL oXbp
   LOCAL aPresParam

  /*
   * Destroy previously created boxes
   */
   AEval( aStatics, {|o| o:destroy() } )
   ASize( aStatics, 0 )

  /*
   * Create XbpStatic objects.
   * Note that we have the objects calculate their
   * size dynamically, since the text height may
   * vary depending on the current screen resolution
   * and other system settings.
   */
   oXbp         := XbpStatic():new(,, aPos,,, .F. )
   oXbp:caption := "Default color and font"
   oXbp:autoSize:= .T.
   oXbp:create()
   AAdd( aStatics, oXbp )

   aPresParam   := { {XBP_PP_COMPOUNDNAME, FONT_COURIER_MEDIUM} }
   oXbp         := XbpStatic():new(,,,, aPresParam, .F. )
   oXbp:caption := "Fixed font, contrast color for foreground"
   oXbp:autoSize:= .T.
   oXbp:create()
   oXbp:setColorBG( GRA_CLR_BACKGROUND )
   oXbp:setColorFG( GRA_CLR_NEUTRAL )
   AAdd( aStatics, oXbp )
   aPos[2]      -= ( oXbp:currentSize()[2] + 20 )
   oXbp:setPos( aPos )

   aPresParam   := { {XBP_PP_COMPOUNDNAME, FONT_TIMES_SMALL} }
   oXbp         := XbpStatic():new(,,,, aPresParam, .F. )
   oXbp:caption := "Small proportional font, gray background, red foreground"
   oXbp:autoSize:= .T.
   oXbp:create()
   oXbp:setColorFG( GRA_CLR_RED )
   AAdd( aStatics, oXbp )
   aPos[2]      -= ( oXbp:currentSize()[2] + 20 )
   oXbp:setPos( aPos )

   aPresParam   := { {XBP_PP_COMPOUNDNAME, FONT_HELV_LARGE} }
   oXbp         := XbpStatic():new(,,,, aPresParam, .F. )
   oXbp:caption := "Large font, different colors"
   oXbp:autoSize:= .T.
   oXbp:create()
   oXbp:setColorBG( GRA_CLR_BLUE )
   oXbp:setColorFG( GRA_CLR_YELLOW )
   AAdd( aStatics, oXbp )
   aPos[2]      -= ( oXbp:currentSize()[2] + 20 )
   oXbp:setPos( aPos )

  /*
   * XbpStatics are created invisible. Display them all...
   */
   AEval( aStatics, {|o| o:show() } )
RETURN



/*
 * Display XbpStatic objects as frames showing different effects
 * Note: Presentation parameters are defined via array, not via method calls
 */

PROCEDURE StaticBox( aStatics )
   LOCAL aPos       := {120,300}
   LOCAL aSize      := {400, 30}
   LOCAL aSize1     := {398, 28}
   LOCAL aPresParam := { { XBP_PP_COMPOUNDNAME, FONT_HELV_SMALL  }  }
   LOCAL oXbp, oXbp1

  /*
   * Destroy previously created text
   */
   AEval( aStatics, {|o| o:destroy() } )
   ASize( aStatics, 0 )

   oXbp         := XbpStatic():new(,, aPos, aSize, aPresParam, .F. )
   oXbp:type    := XBPSTATIC_TYPE_RAISEDBOX
   oXbp:create()
   AAdd( aStatics, oXbp )
   Description( oXbp, "Raised Box", aPresParam )

   aPos[2] -= 50

   oXbp         := XbpStatic():new(,, aPos, aSize, aPresParam, .F. )
   oXbp:type    := XBPSTATIC_TYPE_RECESSEDBOX
   oXbp:create()
   AAdd( aStatics, oXbp )
   Description( oXbp, "Recessed Box", aPresParam )

   aPos[2] -= 50

   oXbp         := XbpStatic():new(,, aPos, aSize, aPresParam, .F. )
   oXbp:type    := XBPSTATIC_TYPE_RAISEDBOX
   oXbp:create()
   AAdd( aStatics, oXbp )

   oXbp1        := XbpStatic():new( oXbp,, {1,1}, aSize1 )
   oXbp1:type   := XBPSTATIC_TYPE_RECESSEDBOX
   oXbp1:create()

  /*
   * Note: oXbp1 is a child window of oXbp. Therefore we don't
   *       need to add it to the array aStatics.
   */

   Description( oXbp1, "Outside raised, inside recessed", aPresParam )

   aPos[2] -= 50

   oXbp         := XbpStatic():new(,, aPos, aSize, aPresParam, .F. )
   oXbp:type    := XBPSTATIC_TYPE_RECESSEDBOX
   oXbp:create()
   AAdd( aStatics, oXbp )

   oXbp1        := XbpStatic():new( oXbp,, {1,1}, aSize1 )
   oXbp1:type   := XBPSTATIC_TYPE_RAISEDBOX
   oXbp1:create()

   Description( oXbp1, "Outside recessed, inside raised", aPresParam )

   aPos[2] -= 50

  /*
   * XbpStatics are created unvisible. Display all...
   * Note: oXbp1 is created visible in unvisible parent window
   *       oXbp1 becomes visible automatically
   */
   AEval( aStatics, {|o| o:show() } )

RETURN



/*
 * Display description within boxes using XbpStatic type Text
 */

PROCEDURE Description( oBox, cText, aPresParam )
   LOCAL oXbp, aSize := oBox:currentSize()

   aSize[1] -= 2
   aSize[2] -= 2

  /*
   * Text is a child of the box
   */
   oXbp         := XbpStatic():new( oBox,, {1,1}, aSize, aPresParam )
   oXbp:caption := cText
   oXbp:options := XBPSTATIC_TEXT_CENTER + XBPSTATIC_TEXT_VCENTER
   oXbp:create()

RETURN
