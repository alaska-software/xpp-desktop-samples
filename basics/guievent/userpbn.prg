//////////////////////////////////////////////////////////////////////
//
//  USERPBN.PRG
//
//  Copyright:
//      Alaska Software, (c) 1999-2025. All rights reserved.         
//  
//  Contents:
//      The example demonstrates how to implement a user-defined
//      mouse-event handling for a pushbutton.
//   
//////////////////////////////////////////////////////////////////////



#include "Appevent.ch"
#include "Xbp.ch"


PROCEDURE Main
   LOCAL nEvent, mp1, mp2, oXbp
 
   SetColor( "N/W" )
   CLS

   oXbp          := UserPushButton():new()
   oXbp:caption  := "I react only to Right mouse clicks"
   oXbp:activate := {|| MsgBox( "I am clicked" ) }
   oXbp:create( , , {140,200}, {360,30} )


   SetAppFocus( oXbp )

   nEvent := 0
   DO WHILE nEvent <> xbeP_Close
      nEvent := AppEvent( @mp1, @mp2, @oXbp )
      oXbp:HandleEvent( nEvent, mp1, mp2 )
   ENDDO
RETURN


/*
 * Derive a user-defined class from the XbpPushbutton class
 * and re-define mouse event-handling methods
 */
CLASS UserPushButton FROM XbpPushButton
   EXPORTED
   METHOD rbUp, rbDown
   METHOD lbUp, lbDown, lbClick, lbDblClick
   INLINE METHOD Keyboard
     /*
      * We react only on right mouse clicks.
      */
   RETURN self
      
ENDCLASS


/*
 * Tell the super class to behave as if
 * there is a "Left button down" event
 */
METHOD UserPushButton:rbDown( mp1, mp2 )
   ::xbpPushButton:lbDown( mp1, mp2 )
RETURN self


/*
 * Translate "Right button up" to "Left button up"
 */
METHOD UserPushButton:rbUp( mp1, mp2 )
   ::xbpPushButton:lbUp( mp1, mp2 )
RETURN self


/*
 * Ignore the "Left button pressed" event
 */
METHOD UserPushButton:lbDown
RETURN self


/*
 * Ignore the "Left button released" event
 * but set focus to the pushbutton
 */
METHOD UserPushButton:lbUp
   SetAppFocus( self )
RETURN self


/*
 * Ignore the "Left button click" event
 */
METHOD UserPushButton:lbClick
RETURN self


/*
 * Ignore the "Left button double-click" event
 */
METHOD UserPushButton:lbDblClick
RETURN self
