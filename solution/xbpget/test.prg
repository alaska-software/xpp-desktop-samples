//////////////////////////////////////////////////////////////////////
//
//  TEST.PRG
//
//  Copyright:
//   Alaska Software, (c) 1997-2025. All rights reserved.         
//  

//  Contents:
//      A simple sample program which demonstrates the use of the XbpGet
//      class in conjunction with the XbpGetController class.
//   
//////////////////////////////////////////////////////////////////////


#include "Gra.ch"
#include "Xbp.ch"
#include "Appevent.ch"
#include "Font.ch"

PROCEDURE Main
   LOCAL nEvent, mp1, mp2, aSize, aPos := Array(2)
   LOCAL oDlg, oXbp, drawingArea, oCtrl, oXbp1, oXbp2
   LOCAL aPP := { { XBP_PP_COMPOUNDNAME, FONT_DEFFIXED_SMALL } } 
   LOCAL aString    := { "A nice little String", NIL }
   LOCAL aNumeric   := { -1234.567, "@K 999,999,999.999" }
   LOCAL aDate      := { Date(), NIL }
   LOCAL aLogic     := { .T., NIL }
   LOCAL aPhone     := { 496196123456780, "(+99)9999 999 999 999" }
   LOCAL aZIP       := { 63110, "XX-99999" }
   LOCAL aTime      := { "12 12 00", "99:99:99" }
   LOCAL aBLZ       := { 4132486, "999-9-999" }
   LOCAL aUPPERCASE := { "a lowercase string", "@!" }

   SET CENTURY ON
   
   ? "Input field STRING must not be empty"
   ? "Input field TEL. can only be edited when LOGIC == TRUE"

   aSize    := AppDesktop():currentSize()
   aPos[1]  := aSize[1] / 2 - 207
   aPos[2]  := aSize[2] / 2 - 150

   oDlg := XbpDialog():new( SetAppWindow():setParent(), , aPos, {425,310}, , .T.)
   oDlg:taskList := .T.
   oDlg:title    := "Test Form for XbpGet"
   oDlg:visible  := .T.
   oDlg:close    := {|mp1,mp2,obj| obj:destroy() }
   oDlg:create()

   drawingArea := oDlg:drawingArea
   drawingArea:setFontCompoundName( "8.Arial" )

   oXbp := XbpPushButton():new( drawingArea, , {316,236}, {84,24} )
   oXbp:caption := "Ok"
   oXbp:clipSiblings := .T.
   oXbp:tabStop      := .T.
   oXbp:create()
   oXbp:activate := {|| oCtrl:getData(), PostAppEvent( xbeP_Close ) }

   oXbp := XbpPushButton():new( drawingArea, , {316,200}, {84,24} )
   oXbp:caption := "GetData"
   oXbp:clipSiblings := .T.
   oXbp:tabStop      := .T.
   oXbp:create()
   oXbp:activate := {|| printValues( oCtrl:getList() ) }

   oXbp1 := XbpStatic():new( drawingArea, , {8,152}, {288,116} )
   oXbp1:caption := "No Picture"
   oXbp1:clipSiblings := .T.
   oXbp1:type := XBPSTATIC_TYPE_GROUPBOX
   oXbp1:create()

   oXbp := XbpStatic():new( oXbp1, , {4,80}, {64,20} )
   oXbp:caption := "String"
   oXbp:clipSiblings := .T.
   oXbp:options := XBPSTATIC_TEXT_VCENTER+XBPSTATIC_TEXT_RIGHT
   oXbp:create()

   /*
    * Postvalidation: String must not be empty
    */
   oXbp := XbpGet():new( oXbp1, , {76,80}, {204,20}, aPP )
   oXbp:clipSiblings := .T.
   oXbp:Picture      := aString[2]
   oXbp:Datalink     := varBlock( @aString[1] )
   oXbp:postBlock    := {|| .NOT. Empty( aString[1] ) }
   oXbp:create()

   oXbp := XbpStatic():new( oXbp1, , {4,56}, {64,20} )
   oXbp:caption := "Numeric"
   oXbp:clipSiblings := .T.
   oXbp:options := XBPSTATIC_TEXT_VCENTER+XBPSTATIC_TEXT_RIGHT
   oXbp:create()

   oXbp := XbpGet():new( oXbp1, , {76,56}, {204,20}, aPP )
   oXbp:clipSiblings := .T.
   oXbp:Picture  := aNumeric[2]
   oXbp:Datalink := varBlock( @aNumeric[1] )
   oXbp:create()

   oXbp := XbpStatic():new( oXbp1, , {4,32}, {64,20} )
   oXbp:caption := "Date"
   oXbp:clipSiblings := .T.
   oXbp:options := XBPSTATIC_TEXT_VCENTER+XBPSTATIC_TEXT_RIGHT
   oXbp:create()

   oXbp := XbpGet():new( oXbp1, , {76,32}, {204,20}, aPP )
   oXbp:clipSiblings := .T.
   oXbp:Datalink := varBlock( @aDate[1] )
   oXbp:Picture  := aDate[2]
   oXbp:create()

   oXbp := XbpStatic():new( oXbp1, , {4,8}, {64,20} )
   oXbp:caption := "Logic"
   oXbp:clipSiblings := .T.
   oXbp:options := XBPSTATIC_TEXT_VCENTER+XBPSTATIC_TEXT_RIGHT
   oXbp:create()

   oXbp := XbpGet():new( oXbp1, , {76,8}, {204,20}, aPP )
   oXbp:clipSiblings := .T.
   oXbp:Datalink := varBlock( @aLogic[1] )
   oXbp:Picture  := aLogic[2]
   oXbp:create()

   oXbp2 := XbpStatic():new( drawingArea, , {8,4}, {288,140} )
   oXbp2:caption := "With Picture"
   oXbp2:clipSiblings := .T.
   oXbp2:type := XBPSTATIC_TYPE_GROUPBOX
   oXbp2:create()

   oXbp := XbpStatic():new( oXbp2, , {4,104}, {64,20} )
   oXbp:caption := "Phone"
   oXbp:clipSiblings := .T.
   oXbp:options := XBPSTATIC_TEXT_VCENTER+XBPSTATIC_TEXT_RIGHT
   oXbp:create()

   /*
    * Prevalidation: Editing is only allowed when Logic == .T.
    */
   oXbp := XbpGet():new( oXbp2, , {76,104}, {204,20}, aPP )
   oXbp:clipSiblings := .T.
   oXbp:Datalink := varBlock( @aPhone[1] )
   oXbp:Picture  := aPhone[2]
   oXbp:preBlock := {|| aLogic[1] }
   oXbp:create()

   oXbp := XbpStatic():new( oXbp2, , {4,80}, {64,20} )
   oXbp:caption := "ZIP"
   oXbp:clipSiblings := .T.
   oXbp:options := XBPSTATIC_TEXT_VCENTER+XBPSTATIC_TEXT_RIGHT
   oXbp:create()

   oXbp := XbpGet():new( oXbp2, , {76,80}, {204,20}, aPP )
   oXbp:clipSiblings := .T.
   oXbp:Datalink := varBlock( @aZIP[1] )
   oXbp:Picture  := aZIP[2]
   oXbp:create()

   oXbp := XbpStatic():new( oXbp2, , {4,56}, {64,20} )
   oXbp:caption := "Time"
   oXbp:clipSiblings := .T.
   oXbp:options := XBPSTATIC_TEXT_VCENTER+XBPSTATIC_TEXT_RIGHT
   oXbp:create()

   oXbp := XbpGet():new( oXbp2, , {76,56}, {204,20}, aPP )
   oXbp:clipSiblings := .T.
   oXbp:Datalink := varBlock( @aTime[1] )
   oXbp:Picture  := aTime[2]
   oXbp:create()

   oXbp := XbpStatic():new( oXbp2, , {4,32}, {64,20} )
   oXbp:caption := "Bank"
   oXbp:clipSiblings := .T.
   oXbp:options := XBPSTATIC_TEXT_VCENTER+XBPSTATIC_TEXT_RIGHT
   oXbp:create()

   oXbp := XbpGet():new( oXbp2, , {76,32}, {204,20}, aPP )
   oXbp:clipSiblings := .T.
   oXbp:Datalink := varBlock( @aBLZ[1] )
   oXbp:Picture  := aBLZ[2]
   oXbp:create()

   oXbp := XbpStatic():new( oXbp2, , {4,8}, {64,20} )
   oXbp:caption := "UPPERCASE"
   oXbp:clipSiblings := .T.
   oXbp:options := XBPSTATIC_TEXT_VCENTER+XBPSTATIC_TEXT_RIGHT
   oXbp:create()

   oXbp := XbpGet():new( oXbp2, , {76,8}, {204,20}, aPP )
   oXbp:clipSiblings := .T.
   oXbp:Datalink := varBlock( @aUPPERCASE[1] )
   oXbp:Picture  := aUPPERCASE[2]
   oXbp:create()

   /*
    * Create the XbpGetController object
    */
   oCtrl := XbpGetController():new( oDlg )
   oCtrl:create()

   /*
    * Terminate READ when application window is closed
    *
    * NOTE: 
    *
    * when the XbpGetController object is associated with a window, the
    * window does not need to call :killRead() in its :close code block.
    * We must do this here, because oCtrl does not know SetAppWindow(), 
    * and clicking the X-button in the application window must
    * terminate the application. :killRead() terminates the event
    * loop running in oCtrl:read()
    */
   SetAppWindow():close := {|| oCtrl:killRead() } 

   /*
    * Start editing in modal window at 2nd XbpGet
    */
   oCtrl:read( 2 )

RETURN


/*
 * generic code block for variable passed by reference
 */
FUNCTION varBlock( val )
RETURN { |x| IF( PCOUNT()==1, val := x, val ) }


/*
 * Display the current XbpGet values
 */
PROCEDURE printValues( aGetList )
  AEval( aGetList, { |oXbp| QOUT( ValType( oXbp:GetData() ), oXbp:GetData() ) } )
  QOUT( "-------------------------" )
RETURN


// EOF
