//////////////////////////////////////////////////////////////////////
//
//  USERSLE.PRG
//
//  Copyright:
//      Alaska Software, (c) 1999-2025. All rights reserved.         
//  
//  Contents:
//      The example demonstrates how to implement a user-defined keyboard 
//      handling for a single line entry field.
//   
//////////////////////////////////////////////////////////////////////



#include "Appevent.ch"
#include "Xbp.ch"


PROCEDURE Main
   LOCAL nEvent, mp1, mp2, oXbp
 
   SetColor( "N/W" )
   CLS

   @ 1,1 SAY "The entry field transforms all lower case input to UPPER CASE"

   oXbp:= UpperCaseSLE():new()
   oXbp:create( , , {220,200}, {200,30} )

   SetAppFocus( oXbp )

   nEvent := 0
   DO WHILE nEvent <> xbeP_Close
      nEvent := AppEvent( @mp1, @mp2, @oXbp )
      oXbp:HandleEvent( nEvent, mp1, mp2 )
   ENDDO
RETURN


/*
 * Derive a user-defined class from the XbpSLE class
 * and re-define the :keyboard() method
 */
CLASS UpperCaseSLE FROM XbpSLE
   EXPORTED
   METHOD keyboard
ENDCLASS


/*
 * Overloaded :keyboard() method
 */
METHOD UpperCaseSLE:keyboard( nKey )

    SetPos( 3, 3 )
    QOut(  ":keyboard() method received ", nKey, "=>", Chr(nKey) )

   /*
    * Intercept a key code and change it, if necessary
    */
    IF nKey > 90 .AND. nKey < 255
       nKey := Asc( Upper( Chr( nKey ) ) )
    ENDIF

   /*
    * Pass the (changed) key code on to the super class
    */
    ::xbpSLE:keyboard( nKey )

    SetPos( 5, 3 )
    QOut(  ":keyboard() method processed", nKey, "=>", Chr(nKey) )

RETURN self
