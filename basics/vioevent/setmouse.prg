//////////////////////////////////////////////////////////////////////
//
//  SETMOUSE.PRG
//
//  Copyright:
//      Alaska Software, (c) 1997-2025. All rights reserved.         
//  
//  Contents:
//      Retrieve mouse position using text coordinates.
//   
//  Syntax:
//      SETMOUSE
//   
//  Return:
//      returns 0 always 
//   
//////////////////////////////////////////////////////////////////////


#include "appevent.ch"

****************
PROCEDURE Main()
   LOCAL nEvent, mp1, mp2, nMouseRow, nMouseCol
   LOCAL lContinue := .T. , nColor := 0

   CLS

   ? "Click with the left mouse button"
   ? 
   ? "A click with the right button ends the program"

   // Enable mouse events
   SetMouse( .T. )

   SET COLOR TO "W+/N,R+/N,B+/N,G+/N,BG+/N"

   DO WHILE lContinue
      nEvent    := AppEvent ( @mp1, @mp2 )

      IF nEvent == xbeM_LbDown

         nMouseRow := mp1[1]
         nMouseCol := mp1[2]

         IF ( ++nColor > 4 )
            nColor := 0
         ENDIF

         ColorSelect( nColor )
         SetPos( nMouseRow, nMouseCol )
         DevOut( "<- You clicked here" )

      ELSEIF nEvent == xbeM_RbDown
         lContinue := .F.

      ENDIF
   ENDDO

RETURN
