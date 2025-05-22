//////////////////////////////////////////////////////////////////////
//
//  SAMPLE1.PRG
//
//  Copyright:
//     Alaska Software, (c) 1997-2025. All rights reserved.         
//  
//  Contents:
//     Class code created by the Xbase++ FormDesigner
//     Creation date: 05.12.97 Time: 13:43:03
//   
//  Remarks:
//     This file contains the basic structure for the utilization-level of
//     a form. It may (and sould) be modified.
//   
//////////////////////////////////////////////////////////////////////


#include "Gra.ch"
#include "Xbp.ch"
#include "Appevent.ch"
#include "Font.ch"

PROCEDURE Main
   LOCAL nEvent, mp1 := NIL, mp2 := NIL
   LOCAL oDlg, oXbp, drawingArea, aEditControls := {}, oXbp1

   // SET DEFAULT is not written by the FormDesigner
   SET DEFAULT TO ..\..\data\misc

   USE CUSTOMER.DBF NEW EXCLUSIVE

   IF IsMemberVar(XbpDialog(),"ClientSize") == .T.
      aSize := {0,0}
   ELSE
      aSize := {354,197}
   ENDIF
   
   oDlg := XbpDialog():new( AppDesktop(), , {148,98}, aSize, , .F.)
   IF aSize[1] == 0 .AND. aSize[2] == 0
      oDlg:ClientSize := {334,155}
   ENDIF
   oDlg:border := XBPDLG_SIZEBORDER
   oDlg:taskList := .T.
   oDlg:title := "New Form    "
   oDlg:visible := .F.
   oDlg:create()

   drawingArea := oDlg:drawingArea
   drawingArea:setFontCompoundName( "8.Helv.normal" )

   oXbp := XbpPushButton():new( drawingArea, , {228,12}, {84,24} )
   oXbp:caption := "OK"
   oXbp:clipSiblings := .T.
   oXbp:create()
   oXbp:activate := {|| Gather( aEditControls ), PostAppEvent( xbeP_Close ) }

   oXbp := XbpPushButton():new( drawingArea, , {108,12}, {72,24} )
   oXbp:caption := "Next"
   oXbp:clipSiblings := .T.
   oXbp:create()
   oXbp:activate := {|| Gather( aEditControls ), CUSTOMER->(DbSkip(1)), Scatter ( aEditControls ) }

   oXbp := XbpPushButton():new( drawingArea, , {12,12}, {72,24} )
   oXbp:caption := "Previous"
   oXbp:clipSiblings := .T.
   oXbp:create()
   oXbp:activate := {|| Gather( aEditControls ), CUSTOMER->(DbSkip(-1)), Scatter ( aEditControls ) }

   oXbp1 := XbpStatic():new( drawingArea, , {12,48}, {300,96} )
   oXbp1:caption := "Customer"
   oXbp1:clipSiblings := .T.
   oXbp1:type := XBPSTATIC_TYPE_GROUPBOX
   oXbp1:create()

   oXbp := XbpStatic():new( oXbp1, , {12,48}, {72,24} )
   oXbp:caption := "Lastname:"
   oXbp:clipSiblings := .T.
   oXbp:options := XBPSTATIC_TEXT_VCENTER
   oXbp:create()

   oXbp := XbpSLE():new( oXbp1, , {96,48}, {192,24} )
   oXbp:bufferLength := 20
   oXbp:clipSiblings := .T.
   oXbp:tabStop := .T.
   oXbp:dataLink := {|x| IIf( PCOUNT()==0, Trim( CUSTOMER->LASTNAME ), CUSTOMER->LASTNAME := x ) }
   oXbp:create():setData()
   AAdd ( aEditControls, oXbp )

   oXbp := XbpStatic():new( oXbp1, , {12,12}, {72,24} )
   oXbp:caption := "Firstname:"
   oXbp:clipSiblings := .T.
   oXbp:options := XBPSTATIC_TEXT_VCENTER
   oXbp:create()

   oXbp := XbpSLE():new( oXbp1, , {96,12}, {192,24} )
   oXbp:bufferLength := 20
   oXbp:clipSiblings := .T.
   oXbp:tabStop := .T.
   oXbp:dataLink := {|x| IIf( PCOUNT()==0, Trim( CUSTOMER->FIRSTNAME ), CUSTOMER->FIRSTNAME := x ) }
   oXbp:create():setData()
   AAdd ( aEditControls, oXbp )

   oDlg:show()

   nEvent := xbe_None
   DO WHILE nEvent <> xbeP_Close
      nEvent := AppEvent( @mp1, @mp2, @oXbp )
      oXbp:handleEvent( nEvent, mp1, mp2 )
   ENDDO
RETURN
