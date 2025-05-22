//////////////////////////////////////////////////////////////////////
//
//  HELLO.PRG
//
//  Copyright:
//      Alaska Software, (c) 1997-2025. All rights reserved.         
//  
//  Contents:
//      This program uses a graphical path and a graphical segment
//      to display a character string in user-defined colors.
//   
//  Remarks:
//      This program works best on screens with a color resolution
//      of 16 bits per pixel or greater. Using it on systems with
//      less color depth might cause the different colors to be
//      displayed incorrectly. Furthermore, painting may be slow
//      under certain circumstances.On systems running the std.
//      VGA resolution (ie. with only 16 colors), the sample just
//      display a message box informing about that fact and exits.
//////////////////////////////////////////////////////////////////////

#include "Gra.ch"
#include "AppEvent.ch"


PROCEDURE Main
   LOCAL oPS, oFont
   LOCAL nEvent, mp1, mp2, oXbp

   SetAppWindow():useShortCuts := .T.
   SetColor( "N/W" )
   CLS

   oPS   := SetAppWindow():presSpace()
   oFont := XbpFont():new():create("48.Helvetica")
   GraSetFont( oPS ,oFont )

   /* 
    * Initialize the PS' color table and render
    * the colored string
    */
   InitializeColors( oPS )

   DrawColoredString( oPS, { 100, 200 }, "Hello World" )

   nEvent := xbe_None   
   DO WHILE nEvent != xbeP_Close .AND. nEvent != xbeK_ESC
      nEvent := AppEvent( @mp1, @mp2, @oXbp )
      oXbp:handleEvent( nEvent, mp1, mp2 )
   ENDDO
                                                         
                                                         
RETURN

******************************************************************************
* This function draws a string using self-defined colors
* Note: a vector font must be set in the presentation space
******************************************************************************
PROCEDURE DrawColoredString( oPS, aPos, cString )
   LOCAL i, nSegment, aOld, aSize, aAttr[ GRA_AL_COUNT ]
   LOCAL aRGB      := { 0, 0, 0 }
   LOCAL aBox      := GraQueryTextBox( oPS, cString )
   LOCAL aLeftBot  := aBox[2]
   LOCAL aRightTop := aBox[3]
   LOCAL nMaxColor
   LOCAL aAttrOld
   LOCAL nColorStep


   /*
    * Calculate absolute position and size of
    * the string
    */
   aLeftBot[1]  += aPos[1]
   aLeftBot[2]  += aPos[2]
   aRightTop[1] += aPos[1]
   aRightTop[2] += aPos[2]
   nMaxColor    := oPS:maxColorIndex()
   aSize        := { aRightTop[1] - aLeftBot[1], ;
                     aRightTop[2] - aLeftBot[2]  }

   nSegment := GraSegOpen( oPS, GRA_SEG_MODIFIABLE )

   /*
    * Create a graphical path within the
    * segment and render the string into it
    */
   GraPathBegin( oPS )
   GraStringAt ( oPS, aPos, cString )
   GraPathEnd  ( oPS )
   GraSegClose ( oPS )

   /* Set color for lines                   */
   aAttr[ GRA_AL_COLOR  ] := nMaxColor
   aOld  := GraSetAttrLine( oPS, aAttr )

   /* Re-create the path by drawing the     */
   /* segment                               */
   GraSegDraw ( oPS, nSegment )
   
   /* Enable clipping                       */
   GraPathClip( oPS, .T. )

   aRightTop[1] := aLeftBot[1]

   /*
    * Fill path with lines;
    * the color indeces used here are assumed to have 
    * been initialized using "InitializeColors()"
    */
    nColorStep := (aSize[1] + 4) / (nMaxColor -16)

    FOR i:= 0 TO aSize[1] + 5
      /* Set RGB color and draw a single    */
      /* line                               */
      aAttr[ GRA_AL_COLOR  ] := 16 + (i / nColorStep)
      aOld  := GraSetAttrLine( oPS, aAttr )

      aLeftBot[1]  ++
      aRightTop[1] ++
      GraLine( oPS, aLeftBot, aRightTop )
    NEXT

   /* Disable clipping                      */
   GraPathClip( oPS, .F. )

   /* Outline string with color black       */
   aAttr[ GRA_AL_COLOR  ] := GRA_CLR_BLACK
   GraSetAttrLine( oPS, aAttr )

   /*
    * Re-create the path by drawing the
    * segment and outline the string's
    * characters
    */
   GraSegDraw( oPS, nSegment )
   GraPathOutLine( oPS )

   /*
    * Release segment and reset attributes
    */
   GraSegDestroy( oPS, nSegment )
   GraSetAttrLine( oPS, aOld )
RETURN

******************************************************************************
* Initialize the color table of the Presentation Space with the colors
* required to render the colored string
******************************************************************************
PROCEDURE InitializeColors( oPS )

   LOCAL aRGB      := { 0, 0, 0 }
   LOCAL nMaxColor
   LOCAL nCurrColor
   LOCAL nColorStep
   LOCAL aColorTab := Array( 0 )

   
   /*
    * Get the maximum color index we can use to
    * set a color required by the application;
    * note that we'll exit here if the system is
    * bound to run low on colors
    */
   nMaxColor  := oPS:maxColorIndex()

   IF nMaxColor <= 16
      MsgBox( "Your System cannot Display Enough Colors for this " + ; 
              "Sample! Please Choose a Resolution with a higher "  + ;
              "Color Depth." )

      QUIT
   ENDIF
   nColorStep := 255 / (nMaxColor - 16)

   FOR nCurrColor := 0 TO nMaxColor - 16
      aRGB[3] := 255- Int( nCurrColor * nColorStep )
      aRGB[1] := Int( nCurrColor * nColorStep )
   
      AAdd( aColorTab, AClone(aRGB) )
   NEXT

   oPS:setColorIndex( 16, aColorTab )

Return

