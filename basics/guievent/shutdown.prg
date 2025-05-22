//////////////////////////////////////////////////////////////////////
//
//  SHUTDOWN.PRG
//
//  Copyright:
//      Alaska Software, (c) 1999-2025. All rights reserved.         
//  
//  Contents:
//      The example demonstrates how a user confirmation is obtained
//      before a program ends. The :close code block is called when
//      the window is closed by the user, and the :quit code block
//      is evaluated upon system shutdown.
//   
//////////////////////////////////////////////////////////////////////



#include "xbp.ch"
#include "appevent.ch"


PROCEDURE Main
   LOCAL nEvent, mp1, mp2, oXbp

   SetAppWindow():quit  := {|| AskForShutdown() }
   SetAppWindow():close := {|| AppQuit()        }
   SetAppWindow():useShortCuts := .T.

   ?
   ? "Close the window or press Ctrl+Alt+Del and attempt to"
   ? "shut down your system."
   
   // Infinite loop. The programm is terminated
   // in AppQuit() or AskForShutdown()
   DO WHILE .T.
      nEvent := AppEvent( @mp1, @mp2, @oXbp )
      oXbp:handleEvent( nEvent, mp1, mp2 )
   ENDDO
RETURN



/*
 * Function to confirm programm termination upon system shut down
 */
FUNCTION AskForShutdown()
   LOCAL nConfirm 

   nConfirm := ;
     ConfirmBox( , ; 
        "Do you want to shut down your computer ?" , ;
        "Xbase++ still running", ; 
         XBPMB_YESNO , ; 
         XBPMB_QUESTION+XBPMB_APPMODAL+XBPMB_MOVEABLE, ;
         XBPMB_DEFBUTTON2 )

   IF nConfirm == XBPMB_RET_NO
      nConfirm := XBP_REJECT
   ELSE
      COMMIT
      CLOSE ALL
      nConfirm := XBP_ALLOW
   ENDIF
RETURN nConfirm



/*
 * Routine to terminate the programm normally
 */
PROCEDURE AppQuit()
   LOCAL nConfirm

   nConfirm := ConfirmBox( , ;
                 "Do you really want to quit ?", ;
                 "Quit", ;
                  XBPMB_YESNO , ;
                  XBPMB_QUESTION+XBPMB_APPMODAL+XBPMB_MOVEABLE )

   IF nConfirm == XBPMB_RET_YES
      COMMIT
      CLOSE ALL
      QUIT
   ENDIF
RETURN
