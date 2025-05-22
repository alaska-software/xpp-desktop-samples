//////////////////////////////////////////////////////////////////////
//
//  SPINNER.PRG
//
//  Copyright:
//       Alaska Software, (c) 1997-2025. All rights reserved.         
//  
//  Contents:
//       The program demonstrates a Spinning Indicator. It is used for
//       unspecific screen output that indicates an "I'm busy" state.
//       The spinning indicator is displayed in a separate thread.
//       It is created using the GraArc() function and stored in
//       graphical segments.
//   
//////////////////////////////////////////////////////////////////////

#include "Gra.ch"
#include "AppEvent.ch"

PROCEDURE Main
   LOCAL aSpin, oThread
   LOCAL nEvent, mp1, mp2, oXbp

   SetAppWindow():useShortCuts := .T.
   SetColor( "N/W" )
   CLS
   ? "Press ESC or close the window to cancel..."
   ?

   aSpin    := InitSpinner( , {300,200}, 25 )
   oThread  := Thread():new( , "Spinner" )
   oThread:start( "StartSpinner", aSpin )

   nEvent := xbe_None
   DO WHILE nEvent != xbeP_Close .AND. nEvent != xbeK_ESC
      nEvent := AppEvent( @mp1, @mp2, @oXbp, 10 )
      
      IF nEvent != xbeP_None .AND. oXbp != NIL
         oXbp:handleEvent( nEvent, mp1, mp2 )
      ELSE
         ?? "."
      ENDIF
   ENDDO

   StopSpinner( aSpin )
   oThread:synchronize(0)

   DestroySpinner( aSpin )
RETURN



*******************************************************************************
* Initialize spinning indicator
*******************************************************************************
FUNCTION InitSpinner( oPS, aCenter, nRadius )
   LOCAL aAttrWhite [ GRA_AA_COUNT ], nSegment1
   LOCAL aAttrBlack [ GRA_AA_COUNT ], nSegment2
   LOCAL aMatrix := GraInitMatrix()

   aAttrWhite [ GRA_AA_COLOR ] := GRA_CLR_WHITE
   aAttrBlack [ GRA_AA_COLOR ] := GRA_CLR_BLACK

   /* Set the fill color to white           */
   GraSetAttrArea( oPS, aAttrWhite )

   /* Open first segment                    */
   nSegment1 := GraSegOpen( oPS )

   /* Open graphical path                   */
   GraPathBegin( oPS )
     /*
      * Draw arc segments; 90 and 270 degrees
      */
      GraPos ( oPS, aCenter )
      GraArc ( oPS, aCenter, nRadius-1, ,  90, 90 )
      GraLine( oPS, GraPos( oPS ), aCenter )
      GraArc ( oPS, aCenter, nRadius-1, , 270, 90 )
      GraLine( oPS, GraPos( oPS ), aCenter )
   /* Close path and fill it                */
   GraPathEnd( oPS )
   GraPathFill( oPS )

   /* Close first segment                   */
   GraSegClose( oPS )

   /* Set the fill color to black           */
   GraSetAttrArea( oPS, aAttrBlack )

   /* Open second segment                   */
   nSegment2 := GraSegOpen( oPS )

   /* Open graphical path                   */
   GraPathBegin( oPS )
     /*
      * Draw arc segments; 0 and 180 degrees
      */
      GraPos ( oPS, aCenter )
      GraArc ( oPS, aCenter, nRadius, ,   0, 90 )
      GraLine( oPS, GraPos( oPS ), aCenter )
      GraArc ( oPS, aCenter, nRadius, , 180, 90 )
      GraLine( oPS, GraPos( oPS ), aCenter )
   /* Close path and fill it                */
   GraPathEnd( oPS )
   GraPathFill( oPS )

   /* Close second segment                  */
   GraSegClose( oPS )

   /*
    * Return an array with the following elements:
    * 1: white segment
    * 2: black segment
    * 3: center point
    * 4: transformation matrix
    * 5: Presentation Space
    * 6: Flag; Continue: TRUE/FALSE
    */
RETURN { { nSegment1, aAttrWhite }, ;
         { nSegment2, aAttrBlack }, ;
           aCenter                , ;
           GraInitMatrix()        , ;
           oPS                    , ;
           .T.                      }



*******************************************************************************
* Display the spinning indicator
*******************************************************************************
PROCEDURE StartSpinner( aSpin )
   LOCAL aCenter := aSpin[3]
   LOCAL aMatrix := aSpin[4]
   LOCAL oPS     := aSpin[5]

   aSpin[6] := .T.

   DO WHILE aSpin[6]
      /* Display white segment              */
      GraSetAttrArea( oPS, aSpin[1,2] )
      GraSegDraw    ( oPS, aSpin[1,1], aMatrix )

      /* Display black segment              */
      GraSetAttrArea( oPS, aSpin[2,2] )
      GraSegDraw    ( oPS, aSpin[2,1], aMatrix )

      /* Rotate spinning indicator by 30    */
      /* degrees                            */
      GraRotate     ( oPS, aMatrix, -30 , aCenter, GRA_TRANSFORM_ADD )
      Sleep(10)
   ENDDO
RETURN



*******************************************************************************
* Stop display of spinning indicator
*******************************************************************************
PROCEDURE StopSpinner( aSpin )
   aSpin[6] := .F.
RETURN



*******************************************************************************
* Release graphic segments
*******************************************************************************
PROCEDURE DestroySpinner( aSpin )
   LOCAL oPS := aSpin[5]

   GraSegDestroy( oPS, aSpin[1,1] )
   GraSegDestroy( oPS, aSpin[2,1] )

   ASize( aSpin, 0 )

RETURN
