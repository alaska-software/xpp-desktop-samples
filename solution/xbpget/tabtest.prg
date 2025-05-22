//////////////////////////////////////////////////////////////////////
//
//  TABTEST.PRG
//
//  Copyright:
//      Alaska Software, (c) 1999-2025. All rights reserved.         
//  
//  Contents:
//      A sample program demonstrating pre- and postvalidation
//      of XbpGet objects embedded in tabpages.
//   
//////////////////////////////////////////////////////////////////////


#include "Gra.ch"
#include "Xbp.ch"
#include "Appevent.ch"
#include "Font.ch"


STATIC soText

PROCEDURE Main
   LOCAL nEvent, mp1, mp2, aPos[2], aSize
   LOCAL oDlg, oCtrl, oXbp, drawingArea
   LOCAL oTab1, aValues1, aPicture1, bBlock

   aSize    := AppDesktop():currentSize()
   aPos[1]  := aSize[1] / 2 - 344
   aPos[2]  := aSize[2] / 2 - 282

   oDlg := XbpDialog():new( AppDesktop(), , aPos, {344,282}, , .F.)
   oDlg:taskList := .T.
   oDlg:title := "XbpGet and Tabpages"
   oDlg:close := {|mp1,mp2,obj| obj:destroy() }
   oDlg:create()

   drawingArea := oDlg:drawingArea
   drawingArea:setFontCompoundName( "8.Arial" )

   aValues1    := { .F., 1234.56, "Must not be empty      ", Date() }
   aPicture1   := { "Y", "@K 999,999.99", "@K " + Replicate("X",30), "@K 99-99-99" }

   oTab1       := TabPage1(  drawingArea, {8,32}, {320,216}, aValues1, aPicture1 )
   oTab1:cargo := ""

   aValues2    := { 967, 56732.77, 1.95883, 0 }
   aPicture2   := { "@K 999,999.99", "@K$ 999,999.99", "@KL_ 999.99999", "@KZ 999,999.99" }

   oTab2       := TabPage2(  drawingArea, {8,32}, {320,216}, aValues2, aPicture2 )
   oTab2:cargo := ""

  /*
   * Display validation rule when mouse is on XbpGet
   */
   bBlock := {|x,y,o| IIf( soText:cargo <> o, soText:setCaption( o:cargo ), ), ;
                           soText:cargo := o }
   oTab1:motion := bBlock
   oTab2:motion := bBlock
   AEval( oTab1:childList(), {|o| IIf( o:isDerivedFrom( "XbpGet"), o:motion := bBlock, NIL ) } )
   AEval( oTab2:childList(), {|o| IIf( o:isDerivedFrom( "XbpGet"), o:motion := bBlock, NIL ) } )


   bBlock := {|x,y,o| IIf( o==oTab1, oTab2:minimize(), oTab1:minimize() ), o:maximize() }
   oTab1:tabActivate := bBlock
   oTab2:tabActivate := bBlock


   oXbp := XbpStatic():new( drawingArea, , {0,0}, {336,24},  )
   oXbp:clipSiblings := .T.
   oXbp:type         := XBPSTATIC_TYPE_RECESSEDBOX
   oXbp:create()

   soText := XbpStatic():new( oXbp, , {1,1}, {328,22} )
   soText:clipSiblings := .T.
   soText:options      := XBPSTATIC_TEXT_VCENTER+XBPSTATIC_TEXT_LEFT
   soText:create()

  /*
   * Create the XbpGetController Object
   */
   oCtrl := XbpGetController():new( oDlg )
   oCtrl:create()

  /*
   * Terminate READ when application window is closed
   *
   * NOTE: 
   *
   * when a window is associated with the XbpGetController object, the
   * window does not need to call :killRead() in its :close code block.
   * We must do it here, because oCtrl does not know SetAppWindow(), 
   * and clicking the X-button in the application window must
   * terminate the application. :killRead() terminates the event
   * loop running in oCtrl:read()
   */
   SetAppWindow():close := {|| oCtrl:killRead() } 

  /*
   * By default, the window is set to modal within :read() and the
   * event loop runs in :read()
   */
   oCtrl:read()
RETURN


/*
 * generic code block for variable passed by reference
 */
FUNCTION varBlock( val )
RETURN { |x| IF( PCOUNT()==1, val := x, val ) }



/*
 * Build the first tabpage
 */
FUNCTION TabPage1( oParent, aPos, aSize, aValues, aPicture )
   LOCAL oTab, oXbp
   LOCAL aPP := { { XBP_PP_COMPOUNDNAME, FONT_DEFFIXED_SMALL } } 

   oTab := XbpTabPage():new(oParent, , aPos, aSize )
   oTab:caption      := "Validate"
   oTab:clipSiblings := .T.
   oTab:minimized    := .F.
   oTab:preOffset    := 0
   oTab:postOffset   := 75
   oTab:create()

   oXbp := XbpStatic():new( oTab, , {16,144}, {88,24} )
   oXbp:caption      := "Logical"
   oXbp:clipSiblings := .T.
   oXbp:options      := XBPSTATIC_TEXT_VCENTER+XBPSTATIC_TEXT_RIGHT
   oXbp:create()

   oXbp := XbpGet():new( oTab, , {112,144}, {144,24}, aPP )
   oXbp:clipSiblings := .T.
   oXbp:dataLink     := VarBlock( @aValues[1] )
   oXbp:picture      := aPicture[1]
   oXbp:create()
   oXbp:cargo        := 'PICTURE "Y"'

   oXbp := XbpStatic():new( oTab, , {16,104}, {88,24} )
   oXbp:caption      := "Number"
   oXbp:clipSiblings := .T.
   oXbp:options      := XBPSTATIC_TEXT_VCENTER+XBPSTATIC_TEXT_RIGHT
   oXbp:create()

   oXbp := XbpGet():new( oTab, , {112,104}, {144,24}, aPP )
   oXbp:clipSiblings := .T.
   oXbp:dataLink     := VarBlock( @aValues[2] )
   oXbp:picture      := aPicture[2]
   oXbp:preBlock     := {|| aValues[1] } 
   oXbp:postBlock    := {|| aValues[2] > 1000 } 
   oXbp:create()
   oXbp:cargo        := 'Pre: Logical == .T.  Post: Number > 1000'

   oXbp := XbpStatic():new( oTab, , {16,64}, {88,24} )
   oXbp:caption      := "Name"
   oXbp:clipSiblings := .T.
   oXbp:options      := XBPSTATIC_TEXT_VCENTER+XBPSTATIC_TEXT_RIGHT
   oXbp:create()

   oXbp := XbpGet():new( oTab, , {112,64}, {144,24}, aPP )
   oXbp:clipSiblings := .T.
   oXbp:dataLink     := VarBlock( @aValues[3] )
   oXbp:picture      := aPicture[3]
   oXbp:postBlock    := {|| .NOT. Empty( aValues[3] ) } 
   oXbp:create()
   oXbp:cargo        := 'Post: .NOT. Empty( Name )'

   oXbp := XbpStatic():new( oTab, , {16,24}, {88,24} )
   oXbp:caption      := "Date"
   oXbp:clipSiblings := .T.
   oXbp:options      := XBPSTATIC_TEXT_VCENTER+XBPSTATIC_TEXT_RIGHT
   oXbp:create()

   oXbp := XbpGet():new( oTab, , {112,24}, {144,24}, aPP )
   oXbp:clipSiblings := .T.
   oXbp:dataLink     := VarBlock( @aValues[4] )
   oXbp:picture      := aPicture[4]
   oXbp:create()
   oXbp:cargo        := 'Post: no illegal date'

RETURN oTab


/*
 * Build the second tabpage
 */
FUNCTION TabPage2( oParent, aPos, aSize, aValues, aPicture )
   LOCAL oTab, oXbp
   LOCAL aPP := { { XBP_PP_COMPOUNDNAME, FONT_DEFFIXED_SMALL } } 

   oTab := XbpTabPage():new(oParent, , aPos, aSize )
   oTab:caption      := "Numerics"
   oTab:clipSiblings := .T.
   oTab:minimized    := .T.
   oTab:preOffset    := 25
   oTab:postOffset   := 50
   oTab:create()

   oXbp := XbpStatic():new( oTab, , {16,144}, {88,24} )
   oXbp:caption      := "Number 1"
   oXbp:clipSiblings := .T.
   oXbp:options      := XBPSTATIC_TEXT_VCENTER+XBPSTATIC_TEXT_RIGHT
   oXbp:create()

   oXbp := XbpGet():new( oTab, , {112,144}, {144,24}, aPP )
   oXbp:clipSiblings := .T.
   oXbp:dataLink     := VarBlock( @aValues[1] )
   oXbp:picture      := aPicture[1]
   oXbp:create()
   oXbp:cargo        := 'PICTURE ' + aPicture[1]

   oXbp := XbpStatic():new( oTab, , {16,104}, {88,24} )
   oXbp:caption      := "Number 2"
   oXbp:clipSiblings := .T.
   oXbp:options      := XBPSTATIC_TEXT_VCENTER+XBPSTATIC_TEXT_RIGHT
   oXbp:create()

   oXbp := XbpGet():new( oTab, , {112,104}, {144,24}, aPP )
   oXbp:clipSiblings := .T.
   oXbp:dataLink     := VarBlock( @aValues[2] )
   oXbp:picture      := aPicture[2]
   oXbp:postBlock    := {|| aValues[2] <> 0 } 
   oXbp:create()
   oXbp:cargo        := 'Post: Number 2 <> 0'

   oXbp := XbpStatic():new( oTab, , {16,64}, {88,24} )
   oXbp:caption      := "Number 3"
   oXbp:clipSiblings := .T.
   oXbp:options      := XBPSTATIC_TEXT_VCENTER+XBPSTATIC_TEXT_RIGHT
   oXbp:create()

   oXbp := XbpGet():new( oTab, , {112,64}, {144,24}, aPP )
   oXbp:clipSiblings := .T.
   oXbp:dataLink     := VarBlock( @aValues[3] )
   oXbp:picture      := aPicture[3]
   oXbp:preBlock     := {|o| aValues[3] := aValues[1] / aValues[2], o:setData(), .T. } 
   oXbp:create()
   oXbp:cargo        := 'Pre: Nummer 3 := Nummer 2 / Nummer 1 , TRUE'

   oXbp := XbpStatic():new( oTab, , {16,24}, {88,24} )
   oXbp:caption      := "Number 4"
   oXbp:clipSiblings := .T.
   oXbp:options      := XBPSTATIC_TEXT_VCENTER+XBPSTATIC_TEXT_RIGHT
   oXbp:create()

   oXbp := XbpGet():new( oTab, , {112,24}, {144,24}, aPP )
   oXbp:clipSiblings := .T.
   oXbp:dataLink     := VarBlock( @aValues[4] )
   oXbp:picture      := aPicture[4]
   oXbp:postBlock    := {|| aValues[4] >= 0 .AND. aValues[4] <= 100 } 
   oXbp:create()
   oXbp:cargo        := 'Post: Range from 0 - 100'

RETURN oTab

// EOF
