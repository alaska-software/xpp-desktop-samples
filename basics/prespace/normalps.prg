//////////////////////////////////////////////////////////////////////
//
//  NORMALPS.PRG
//
//  Copyright:
//      Alaska Software, (c) 1997-2025. All rights reserved.         
//  
//  Contents:
//      In the example, a presentation space is linked to the device
//      context of XbpDialog:drawingArea. This makes it possible to
//      use the same presentation space object throughout the lifetime
//      of the program. Hence, graphical attributes need not be reset
//      each time graphical output is performed. Also, a drawing can
//      be constructed and displayed in the XbpDialog window using
//      graphical segments. This is not possible using a so-called
//      Micro PS.
//      
//////////////////////////////////////////////////////////////////////



#include "Xbp.ch"
#include "Gra.ch"
#include "Appevent.ch"

PROCEDURE AppSys
/*
 * Desktop remains application window
 */
RETURN



PROCEDURE Main
   LOCAL nEvent, mp1, mp2, oXbp
   LOCAL oDlg, oPS, nSegId := 0

  /*
   * Create dialog window
   */
   oDlg := XbpDialog():new( , , {10,30}, {640,400} )
   oDlg:taskList   := .T.
   oDlg:sizeRedraw := .T.
   oDlg:title := "Presentation Space Demo"
   oDlg:create()

  /*
   * Create presentation space and link it with
   * device context of :drawingArea 
   */
   oPS  := XbpPresSpace():new()
   oPS:create( oDlg:drawingArea:winDevice() )

  /*
   * The callback code block must be assigned to
   * :drawingArea since it displays the drawing.
   * The variable nSegId stores the id of the
   * graphical segment used for rendering. The
   * segment is created during the first call of
   * procedure DrawGraphic().
   */
   oDlg:drawingArea:paint := {|| DrawGraphic(oPS, @nSegId) } 

  /*
   * The initial display of the window causes a 
   * xbeP_Paint message to be sent to :drawingArea.
   * During processing of the xbeP_Paint message, the
   * codeblock in :paint is automatically evaluated.
   */
   oDlg:show()
   SetAppFocus( oDlg )

   DO WHILE nEvent <> xbeP_Close
      nEvent := Appevent( @mp1, @mp2, @oXbp )
      oXbp:handleEvent( nEvent, mp1, mp2 )
   ENDDO
RETURN



/*
 * This function draws a 'paper airplane'
 */
PROCEDURE DrawGraphic( oPS, nSegId )
   LOCAL aPoints, aAttr[ GRA_AA_COUNT ], aOldAttr

  /*
   * Check if a valid segment id was passed in
   * nSegId. If not, a new segment is created and
   * the paper airplane is rendered into it.
   *
   * NOTE: nSedId must be passed by reference!
   */
   IF nSegId != 0
      GraSegDraw( oPS, nSegId )
      RETURN
   ENDIF

   aPoints := { {635, 345}, ;
                {  5, 195}, ;
                { 80, 145}, ;
                {140, 100}, ;
                {225,  40}, ;
                {110,  25}, ;
                {180,  70}  }   

   nSegId := GraSegOpen( oPS )

   GraPathBegin( oPS )
      GraPos ( oPS, aPoints[1] )
      DrawPolyLine( oPS, aPoints, {2,3,1} )
      DrawPolyLine( oPS, aPoints, {4,5,1} )
   GraPathEnd( oPS )
   
   aAttr [ GRA_AA_SYMBOL ]   := GRA_SYM_DENSE8
   aAttr [ GRA_AA_COLOR  ]   := GRA_CLR_BLACK 
   aAttr [ GRA_AA_BACKCOLOR] := XBPSYSCLR_DIALOGBACKGROUND
   aOldAttr := GraSetAttrArea( oPS, aAttr )
   GraPathFill( oPS )
   DrawPolyLine( oPS, aPoints, {2,3,1} )
   DrawPolyLine( oPS, aPoints, {4,5,1} )
   
   GraPos ( oPS, aPoints[1] )
   DrawPolyLine( oPS, aPoints, {2,3,1} )
   DrawPolyLine( oPS, aPoints, {4,5,1} )
   
   GraPathBegin( oPS )
      DrawPolyLine( oPS, aPoints, {3,6,4,1} )
   GraPathEnd( oPS )
   
   aAttr [ GRA_AA_SYMBOL ] := GRA_SYM_DENSE6
   GraSetAttrArea( oPS, aAttr )
   GraPathFill( oPS )

   DrawPolyLine( oPS, aPoints, {3,6,4,1} )
   
   GraPos ( oPS, aPoints[4] )
   GraPathBegin( oPS )
   DrawPolyLine( oPS, aPoints, {6,7,4} )
   GraPathEnd( oPS )
   
   aAttr [ GRA_AA_SYMBOL ] := GRA_SYM_DENSE3
   GraSetAttrArea( oPS, aAttr )
   GraPathFill( oPS )

   aAttr [ GRA_AA_SYMBOL ] := GRA_SYM_HALFTONE
   GraSetAttrArea( oPS, aAttr )
   GraSetAttrArea( oPS, aOldAttr )

   GraSegClose( oPS, nSegId )

RETURN



/*
 * Draw a polygon
 */
STATIC PROCEDURE DrawPolyLine( oPS, aPoints, aSubScript )
   IF Valtype( aSubScript ) == "A"
      AEVAL( aSubScript, {|n| GraLine( oPS, , aPoints[n] ) } )
   ELSE
      AEVAL( aPoints, {|a| GraLine( oPS, , a ) } )
   ENDIF
RETURN
