//////////////////////////////////////////////////////////////////////
//
//  MAIN.PRG
//
//  Copyright:
//      Alaska Software, (c) 1997-2025. All rights reserved.         
//  
//  Contents:
//      Sample for the C-API of Xbase++
//   
//  Remarks:
//      How to call C-API functions
//   
//////////////////////////////////////////////////////////////////////


/* 
 * request function symbols and unmap the underscore
 */
#pragma map(F2BIN,"_F2BIN") 
#pragma map(BIN2F,"_BIN2F") 
#pragma map(HelloWorld,"_HELLOWORLD")
#pragma map(ListWindowHandles,"_LISTWINDOWHANDLES")
#pragma map(GetWindowText,"_GETWINDOWTEXT")

REQUEST _HelloWorld
REQUEST _Soundex

#include "Appevent.ch" 
#define CALL_EVENT_1 xbeP_User+1
#define CALL_EVENT_2 xbeP_User+2
#define CALL_EVENT_3 xbeP_User+3
#define CALL_EVENT_4 xbeP_User+4
#define CALL_EVENT_END xbeP_User+6

PROCEDURE Main 
   LOCAL nEvent, mp1, mp2, oBtn, oXbp , nNextEvent := CALL_EVENT_1

   SetColor("N/W") 
   CLS 

   oBtn := XbpPushButton():new( , , {450,300}, {130,40} ) 
   oBtn:caption := "Start" 
   oBtn:preSelect := .T.
   oBtn:create() 
   oBtn:activate:= {|| PostAppEvent(nNextEvent) } 

   nEvent := 0 
   DO WHILE nEvent <> xbeP_Close 
       nEvent := AppEvent( @mp1, @mp2, @oXbp ) 

       IF nEvent == CALL_EVENT_1
           BasicCallTest()
           nNextEvent := CALL_EVENT_2
           oBtn:caption := "Next" 
           oBtn:configure() 

       ELSEIF nEvent == CALL_EVENT_2
           CLS
           F2BinTest()
           nNextEvent := CALL_EVENT_3

       ELSEIF nEvent == CALL_EVENT_3
           CLS
           BrowseCust()
           nNextEvent := CALL_EVENT_4

       ELSEIF nEvent == CALL_EVENT_4
           CLS
           SoundexCust()
           nNextEvent := CALL_EVENT_END

       ELSEIF nEvent == CALL_EVENT_END
           CLS
           oBtn:caption := "Close sample" 
           oBtn:configure() 
           APIWrapperTest( oBtn ) 
           nNextEvent := xbeP_Close
       ENDIF

       oXbp:HandleEvent( nEvent, mp1, mp2 ) 
   ENDDO 

RETURN 


PROCEDURE BasicCallTest()
   LOCAL xValue

   ? "Executing a macro which calls a C-function:"
   xValue := &("_HelloWorld()")

   ? "Result of the macro:", xValue
RETURN

PROCEDURE APIWrapperTest(oXbp)
   LOCAL aHandles

   ? ""
   ? "Show title of the button"
   ? "by using the :getHWND()-method"
   ? GetWindowText( oXbp:getHWND() )

   ? ""
   ? "Calling a C-function which wraps and uses Win-API's:"
   aHandles:={}
   aHandles := ListWindowHandles()
   ? str( len( aHandles ), 4) + " visible top level windows found."
   AEval( aHandles, {|x| Qout( str( x, 12 ) + " - " + GetWindowText( x )) })


RETURN

PROCEDURE F2BinTest
   Local nNumber, cTemp 

   ? ""
   ? "Test F2BIN(100.234e16):"
   nNumber := 100.234e16 
   ?? nNumber 
   cTemp := F2Bin(nNumber) 

   ? "Test BIN2F(cTemp):"
   nNumber := Bin2F(cTemp) 
   ?? nNumber 
 
   ? "Test F2BIN(45345):"
   nNumber := 45345 
   ?? nNumber 
   cTemp := F2Bin(nNumber) 

   ? "Test BIN2F(cTemp):"
   nNumber := Bin2F(cTemp) 
   ?? nNumber 

RETURN

PROCEDURE BrowseCust()
    SET DEFAULT TO ..\..\data\misc
    USE Customer NEW
    ? Alias() + " unordered: "
    LIST LastName, FirstName NEXT 22
    CLOSE
RETURN

PROCEDURE SoundexCust()

    SET DEFAULT TO ..\..\data\misc
    USE Customer NEW
    INDEX ON _Soundex(FIELD->LASTNAME) TO soundex
    ? Alias() + " ordered by :" + IndexKey()
    LIST LastName, FirstName NEXT 22
    CLOSE
RETURN

 
 




