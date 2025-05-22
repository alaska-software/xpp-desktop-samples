//////////////////////////////////////////////////////////////////////
//
//  MICROPS.PRG
//
//  Copyright:
//      Alaska Software, (c) 1997-2025. All rights reserved.         
//  
//  Contents:
//      This program demonstrates how to draw inside an Xbase Part.
//      A push button is displayed and a circle is drawn in it that
//      changes color when the button is clicked.
//      For drawing a "Micro" presentation space of the push button
//      is retrieved with :lockPS(). The circle is drawn in this PS.   
//   
//////////////////////////////////////////////////////////////////////


#include "Gra.ch"
#include "Appevent.ch"


PROCEDURE Main
   LOCAL nEvent, mp1, mp2, oXbp
   SetColor( "N/W" )
   CLS
   SetAppWindow():useShortCuts := .T.

  /*
   * Create a pushbutton
   */
   oXbp := XbpPushButton():new( , , {240,160}, {100,80} )
   oXbp:create()
   oXbp:cargo    := .F.
   oXbp:activate := {|mp1, mp2, obj| obj:cargo := ! obj:cargo, ;
                                     DrawCircle( obj ) }
   oXbp:paint    := {|mp1, mp2, obj| DrawCircle( obj ) }
   oXbp:lbDown   := {|mp1, mp2, obj| DrawCircle( obj ) }
   oXbp:lbUp     := {|mp1, mp2, obj| DrawCircle( obj ) }
   oXbp:motion   := {|mp1, mp2, obj| DrawCircle( obj ) }

  /*
   * Display circle initially
   */
   DrawCircle( oXbp )

   nEvent := 0
   DO WHILE nEvent <> xbeP_Close
      nEvent := AppEvent( @mp1, @mp2, @oXbp )
      oXbp:handleEvent( nEvent, mp1, mp2 )
   ENDDO
RETURN



/*
 * Display circle in push button with changing colors (red/green)
 */
STATIC PROCEDURE DrawCircle( oXbp )
   LOCAL oPS, aSize, nX, nY, nRadius, aAttr[ GRA_AA_COUNT ], aOld
   
  /*
   * Calculate center point and radius of the circle from the 
   * size of the pushbutton
   */
   aSize   := oXbp:currentSize()
   nX      := aSize[1] / 2
   nY      := aSize[2] / 2
   nRadius := Min( nX, nY ) * 0.8

  /*
   * Fill color for circle
   */
   aAttr [ GRA_AA_COLOR ] := IIf( oXbp:cargo, GRA_CLR_GREEN, GRA_CLR_RED )

  /*
   * Get MicroPS, set color attributes, draw circle and clean up
   */
   oPS  := oXbp:lockPS()
   aOld := oPS:setAttrArea( aAttr )

   GraArc( oPS, {nX , nY}, nRadius,,,, GRA_OUTLINEFILL )

   oPS:setAttrArea( aOld )
   oXbp:unlockPS( oPS )

RETURN
