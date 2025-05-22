//////////////////////////////////////////////////////////////////////
//
//  EVENT.PRG
//
//  Copyright:
//      Alaska Software, (c) 1997-2025. All rights reserved.         
//  
//  Contents:
//      Example for user defined events.
//   
//  Syntax:
//      EVENT
//   
//  Return:
//      returns 0 always 
//   
//////////////////////////////////////////////////////////////////////


#include "Appevent.ch"

* User defined event codes:
#define  xbeU_DrawBox  xbeP_User + 1
#define  xbeU_Quit     xbeP_User + 2

****************
PROCEDURE Main()
   LOCAL nEvent := 0, mp1, mp2

   CLS
   @ 0,0 SAY " Display Box   |  QUIT"
   SetMouse(.T.)

   DO WHILE .T.
      nEvent := AppEvent( @mp1, @mp2 , ,0 )

      DO CASE
      CASE nEvent == xbeU_DrawBox
         DrawBox( mp1, mp2 )

      CASE nEvent == xbeU_Quit
         QUIT

      CASE nEvent == xbeP_Close
         QUIT

      CASE nEvent < xbeB_Event
         @ MaxRow(), 0
         ?? "Key code is:", nEvent

      CASE nEvent == xbeM_LbClick
         IF mp1[1] == 0 .AND. mp1[2] <= 21
            // Mouse clicked into the text in row 0, post events
            IF mp1[2] <= 15
               PostAppEvent( xbeU_DrawBox, Date(), Time(), SetAppWindow() )
            ELSE
               PostAppEvent( xbeU_Quit, mp1, mp2, SetAppWindow() )
            ENDIF
         ELSE
            @ mp1[1], mp1[2] SAY "Nothing was chosen"
         ENDIF
      ENDCASE

   ENDDO
RETURN

*********************************
PROCEDURE DrawBox( dDate, cTime )
   LOCAL nEvent := 0, nTop, nLeft, nBottom, nRight
   LOCAL mp1, mp2

   SAVE SCREEN
   @ 0, 0 SAY "Click for upper left corner"

   DO WHILE nEvent <> xbeM_LbClick
      nEvent := AppEvent( @mp1, @mp2,, 0 )
   ENDDO
   nTop  := mp1[1]
   nLeft := mp1[2]

   nEvent := 0
   @ nTop, nLeft SAY "Click for lower right corner"

   DO WHILE nEvent <> xbeM_LbClick
      nEvent  := AppEvent( @mp1, @mp2,, 0 )

      IF nEvent == xbeM_LbClick
         nBottom := mp1[1]
         nRight  := mp1[2]
      ENDIF

      IF nEvent == xbeM_LbClick .AND. ;
         ( nBottom <= nTop .OR. nRight <= nLeft )
         Tone(1000,1)
         nEvent := 0
      ENDIF
   ENDDO

   RESTORE SCREEN
   @ nTop, nLeft TO nBottom, nRight
   SetPos( nTop+1, nLeft+1 ) ; QQout( "Date:", dDate )
   SetPos( nTop+2, nLeft+1 ) ; QQout( "Time:", cTime )

RETURN
