//////////////////////////////////////////////////////////////////////
//
//  WHEEL.PRG
//
//  Copyright:
//       Alaska Software, (c) 1997-2025. All rights reserved.         
//  
//  Contents:
//       Example for graphical segments and graphic transformations.
//       A spinning/moving wheel is displayed.
//   
//////////////////////////////////////////////////////////////////////


/*
 * Include files:
 */
#include "Gra.ch"
#include "AppEvent.ch"

****************
PROCEDURE Main()
   LOCAL nEvent := 0
   LOCAL aAttr, aMatrix := GraInitMatrix()
   LOCAL nSegment, aPoint, nRadius
   LOCAL nXmax, nYmax, nXdiff := 14, nYdiff := 0


   SetColor( "N/W" )
   CLS

   /*
    * Enable handling of system shortcuts, eg. for ALT-F4
    */
   SetAppWindow():useShortcuts := .T.

   SetAppWindow():presSpace():drawMode( GRA_DM_RETAIN )
   nXmax := SetAppWindow():xSize
   nYmax := SetAppWindow():ySize

   GraSetColor( NIL, GRA_CLR_CYAN, GRA_CLR_BLUE )

   /*
    * Define the wheel within a graphic segment
    * (a circle with a hub and four spokes)
    */
   nRadius  := 40
   aPoint   := {nRadius + 14,nRadius + 10}
   nSegment := graSegOpen ( NIL, GRA_SEG_MODIFIABLE )

   GraArc ( NIL, aPoint, nRadius )
   GraArc ( NIL, aPoint, nRadius-5 )
   GraArc ( NIL, aPoint, 10 )
   GraLine( NIL, { aPoint[1], aPoint[2]-nRadius }, ;
                 { aPoint[1], aPoint[2]+nRadius }  )
   GraLine( NIL, { aPoint[1]-nRadius, aPoint[2] }, ;
                 { aPoint[1]+nRadius, aPoint[2] }  )
   GraSegClose()

   /*
    * Draw the segment
    */
   aAttr := Array( GRA_AL_COUNT )
   aAttr [ GRA_AL_MIXMODE ] := GRA_FGMIX_XOR
   GraSetAttrLine( NIL, aAttr )
   GraSegDraw( NIL, nSegment, aMatrix)

   DO WHILE nEvent != xbeP_Close .AND. nEvent != xbeK_ESC

      /*
       * Erase drawing from screen (XOR colormix attribute!)
       */
      GraSegDraw( NIL, nSegment, aMatrix )

      /*
       * Rotate and move segment using a transformation
       * matrix that incorporates a translation and a
       * rotation
       */
      GraRotate( NIL, aMatrix, -10, aPoint, GRA_TRANSFORM_ADD )
      GraTranslate(NIL, aMatrix, nXdiff, nYdiff, GRA_TRANSFORM_ADD )

      /*
       * Display segment again
       */
      GraSegDraw( NIL, nSegment, aMatrix )

      /*
       * Calculate next position
       */
      aPoint[1] += nXdiff
      aPoint[2] += nYdiff

      /*
       * Change moving direction when the wheel hits one of
       * the window borders
       */
      IF aPoint[1] + nRadius  >= nXmax-nXdiff .AND. nXdiff <> 0
         nXdiff := 0
         nYdiff := 10

      ELSEIF aPoint[2] + nRadius >= nYmax-nYdiff .AND. nYdiff <> 0
         nXdiff := -10
         nYdiff := 0

      ELSEIF aPoint[1] - nRadius <= Abs(nXdiff) .AND. nXdiff <> 0
         nXdiff := 0
         nYdiff := -10

      ELSEIF aPoint[2] - nRadius <= Abs(nYdiff) .AND. nYdiff <> 0
         nXdiff := 10
         nYdiff := 0
      ENDIF

      nEvent := AppEvent(NIL, NIL, NIL, 0.01)
   ENDDO

RETURN
