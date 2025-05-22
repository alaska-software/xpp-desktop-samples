//////////////////////////////////////////////////////////////////////
//
//  FANCYFNT.PRG
//
//  Copyright:
//      Alaska Software, (c) 1997-2025. All rights reserved.         
//  
//  Contents:
//      This sample program renders some text strings demonstrating the GRA
//      engine's GraStringAt() primitive.
//   
//////////////////////////////////////////////////////////////////////

#include "appevent.ch"
#include "xbp.ch"
#include "gra.ch"


******************************************************************************
* Main() procedure and event loop
******************************************************************************
PROCEDURE Main

   LOCAL nEvent, mp1, mp2, oObj
   LOCAL oPS
   LOCAL oFont
   LOCAL aLineAttrs
   LOCAL aStringAttrs
   LOCAL aMatrix
   LOCAL i


   /*
    * Setup XbpCrt object created by the AppSys()
    * procedure
    */
   SetAppWindow():useShortCuts := .T.
   SetColor( "N/W+" )
   CLS

   /* Get presentation space for rendering   */
   oPS := SetAppWindow():presSpace()

   /* Set defaults                           */
   aLineAttrs   := Array( GRA_AL_COUNT )
   aStringAttrs := Array( GRA_AS_COUNT )
   aMatrix := GraInitMatrix()

   /*
    * Demonstrate the GRA engine's clipping feature
    */
   oFont := XbpFont():new( oPS ):create( "46.Helvetica" )
   GraSetFont( oPS, oFont )

   /* Change the line color and type         */
   aLineAttrs[ GRA_AL_COLOR ] := GRA_CLR_DARKPINK
   aLineAttrs[ GRA_AL_TYPE  ] := GRA_LINETYPE_SOLID
   oPS:setAttrLine( aLineAttrs )

   /* Set clip path to the outline of a      */
   /* sample string                          */
   GraPathBegin( oPS )
   GraStringAt( oPS, {10,300}, "Look, I'm clipped!" )
   GraPathEnd( oPS )
   GraPathClip( oPS, .T. )

   /* Draw some lines to demonstrate clipping*/
   FOR i := -10 TO SetAppWindow():currentSize()[1] STEP 3
      GraLine( oPS, {i, 10}, {i, SetAppWindow():currentSize()[2]} )
   NEXT

   /* Remove clip path                       */
   GraPathClip( oPS, .F. )

   /*
    * Demonstrate outlining fonts using paths
    */
   oFont:configure( "42.Courier New" )
   GraSetfont( oPS, oFont )

   /* Change transform matrix to have the    */
   /* text slant a bit to the left           */
   GraRotate( oPS, aMatrix, 20, {0, 0}, GRA_TRANSFORM_REPLACE )
   oPS:setGraTransform( aMatrix, GRA_TRANSFORM_REPLACE )

   /* Change the line color and type         */
   aLineAttrs[ GRA_AL_COLOR ] := GRA_CLR_BLACK
   aLineAttrs[ GRA_AL_TYPE  ] := GRA_LINETYPE_LONGDASH
   oPS:setAttrLine( aLineAttrs )

   FOR i:=1 TO SetAppWindow():currentSize()[2] STEP 100
      GraPathBegin( oPS )
      GraStringAt( oPS, {10,i}, "Text as Outline" )
      GraPathEnd( oPS )
      GraPathOutline( oPS )
   NEXT


   /*
    * Rotate text strings to show transforms via
    * SetGraTransform()
    */
   oFont:configure( "20.Times New Roman" )
   GraSetFont( oPS, oFont )

   /* Change the page's viewport to move the */
   /* viewport origin to the center of the   */
   /* page                                   */
   oPS:setViewport( {SetAppWindow():currentSize()[1] / 2, ;
                     SetAppWindow():currentSize()[2] / 2, ;
                     SetAppWindow():currentSize()[1], ;
                     SetAppWindow():currentSize()[2]} )

   /* Change the color to render text with   */
   aStringAttrs[ GRA_AS_COLOR ] := GRA_CLR_DARKBLUE
   oPS:setAttrString( aStringAttrs )

   aMatrix := GraInitMatrix()
   FOR i:=1 TO 18
      GraRotate( oPS, aMatrix, 20, {0, 0}, GRA_TRANSFORM_ADD )
      oPS:setGraTransform( aMatrix, GRA_TRANSFORM_REPLACE )
      GraStringAt( oPS, {60,60}, "Transformed Text..." )
   NEXT

   
   /*
    * Event loop; terminates on selection of
    * the window close button
    */
   nEvent := xbe_None
   WHILE nEvent <> xbeP_Close                           
      nEvent := AppEvent ( @mp1, @mp2, @oObj )          

      oObj:HandleEvent ( nEvent, mp1, mp2 )
   ENDDO

RETURN

// EOF
