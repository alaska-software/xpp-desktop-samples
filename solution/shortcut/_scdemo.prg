//////////////////////////////////////////////////////////////////////
//
//  _SCDEMO.PRG
//
//  Copyright:
//      Alaska Software, (c) 1997-2025. All rights reserved.         
//  
//  Contents:
//      Sample code for auto accelerator
//   
//  Remarks:
//   
//////////////////////////////////////////////////////////////////////


#ifndef _NEWFORM_
#define _NEWFORM_

#include "Gra.ch"
#include "Xbp.ch"
#include "Common.ch"
#include "Appevent.ch"
#include "Font.ch"

CLASS _NewForm FROM XbpDialog
   EXPORTED:
      VAR editControls

      * Enthaltene Kontrollelemente
      VAR TabPage1    
      VAR Static6     
      VAR Radiobutton1
      VAR Radiobutton2
      VAR Radiobutton3
      VAR Radiobutton4
      VAR Static7     
      VAR o3state1    
      VAR Checkbox2   
      VAR Checkbox1   
      VAR TabPage2    
      VAR Static1     
      VAR Static2     
      VAR SLE1        
      VAR Static3     
      VAR SLE2        
      VAR Static4     
      VAR Combobox1   
      VAR Static5     
      VAR Spinbutton1 
      VAR ButtonOK    
      VAR ButtonCancel

      METHOD init
      METHOD create
ENDCLASS

******************************************************************************
* Formular initialisieren
******************************************************************************
METHOD _NewForm:init( oParent, oOwner, aPos, aSize, aPP, lVisible )

   DEFAULT oParent  TO AppDesktop(), ;
           aPos     TO {477,298}, ;
           aSize    TO {330,306}, ;
           lVisible TO .F. , ;
           aPP      TO {}

   AAdd ( aPP, { XBP_PP_COMPOUNDNAME, "8.Arial" } )
   ::XbpDialog:init( oParent, oOwner, aPos, aSize, aPP, lVisible )
   ::XbpDialog:taskList := .T.
   ::XbpDialog:title := "Neues Formular"

   ::editControls := {}

   ::TabPage1     := XbpTabPage():new( ::drawingArea, , {12,48}, {300,216} )
   ::TabPage1:caption := "F~irst Tab"
   ::TabPage1:clipSiblings := .T.
   ::TabPage1:minimized := .F.
   ::TabPage1:postOffset := 70
   ::TabPage1:tabStop := .T.

   ::Static6      := XbpStatic():new( ::TabPage1, , {12,12}, {132,156} )
   ::Static6:caption := ""
   ::Static6:clipSiblings := .T.
   ::Static6:type := XBPSTATIC_TYPE_RAISEDBOX

   ::Radiobutton1 := XbpRadiobutton():new( ::Static6, , {12,120}, {108,24} )
   ::Radiobutton1:caption := "COM Port ~1"
   ::Radiobutton1:clipSiblings := .T.
   ::Radiobutton1:group := XBP_BEGIN_GROUP
   ::Radiobutton1:selection := .T.
   ::Radiobutton1:tabStop := .T.
   ::Radiobutton1:selected := {|| NIL }

   ::Radiobutton2 := XbpRadiobutton():new( ::Static6, , {12,84}, {108,24} )
   ::Radiobutton2:caption := "COM Port ~2"
   ::Radiobutton2:clipSiblings := .T.
   ::Radiobutton2:group := XBP_WITHIN_GROUP
   ::Radiobutton2:selected := {|| NIL }

   ::Radiobutton3 := XbpRadiobutton():new( ::Static6, , {12,48}, {108,24} )
   ::Radiobutton3:caption := "COM Port ~3"
   ::Radiobutton3:clipSiblings := .T.
   ::Radiobutton3:group := XBP_WITHIN_GROUP
   ::Radiobutton3:selected := {|| NIL }

   ::Radiobutton4 := XbpRadiobutton():new( ::Static6, , {12,12}, {108,24} )
   ::Radiobutton4:caption := "COM Port ~4"
   ::Radiobutton4:clipSiblings := .T.
   ::Radiobutton4:group := XBP_END_GROUP
   ::Radiobutton4:selected := {|| NIL }

   ::Static7      := XbpStatic():new( ::TabPage1, , {156,12}, {132,156} )
   ::Static7:caption := ""
   ::Static7:clipSiblings := .T.
   ::Static7:type := XBPSTATIC_TYPE_RAISEDBOX

   ::o3state1     := Xbp3state():new( ::Static7, , {12,120}, {108,24} )
   ::o3state1:clipSiblings := .T.
   ::o3state1:caption := "Pa~rity"
   ::o3state1:tabStop := .T.
   ::o3state1:selected := {|| NIL }

   ::Checkbox2    := XbpCheckbox():new( ::Static7, , {12,48}, {108,24} )
   ::Checkbox2:caption := "Hand~shake"
   ::Checkbox2:clipSiblings := .T.
   ::Checkbox2:tabStop := .T.
   ::Checkbox2:selected := {|| NIL }

   ::Checkbox1    := XbpCheckbox():new( ::Static7, , {12,12}, {108,24} )
   ::Checkbox1:caption := "Com~pression"
   ::Checkbox1:clipSiblings := .T.
   ::Checkbox1:tabStop := .T.
   ::Checkbox1:selected := {|| NIL }

   ::TabPage2     := XbpTabPage():new( ::drawingArea, , {12,48}, {300,216} )
   ::TabPage2:caption := "S~econd Tab"
   ::TabPage2:clipSiblings := .T.
   ::TabPage2:minimized := .F.
   ::TabPage2:preOffset := 30
   ::TabPage2:postOffset := 40
   ::TabPage2:tabStop := .T.

   ::Static1      := XbpStatic():new( ::TabPage2, , {12,12}, {276,168} )
   ::Static1:caption := "Customer"
   ::Static1:clipSiblings := .T.
   ::Static1:type := XBPSTATIC_TYPE_GROUPBOX

   ::Static2      := XbpStatic():new( ::Static1, , {12,120}, {72,24} )
   ::Static2:caption := "~Name"
   ::Static2:clipSiblings := .T.
   ::Static2:options := XBPSTATIC_TEXT_VCENTER+XBPSTATIC_TEXT_RIGHT

   ::SLE1         := XbpSLE():new( ::Static1, , {96,120}, {156,24} )
   ::SLE1:clipSiblings := .T.
   ::SLE1:group := XBP_BEGIN_GROUP
   ::SLE1:tabStop := .T.

   ::Static3      := XbpStatic():new( ::Static1, , {12,84}, {72,24} )
   ::Static3:caption := "S~urname"
   ::Static3:clipSiblings := .T.
   ::Static3:options := XBPSTATIC_TEXT_VCENTER+XBPSTATIC_TEXT_RIGHT

   ::SLE2         := XbpSLE():new( ::Static1, , {96,84}, {156,24} )
   ::SLE2:clipSiblings := .T.
   ::SLE2:group := XBP_END_GROUP

   ::Static4      := XbpStatic():new( ::Static1, , {12,48}, {72,24} )
   ::Static4:caption := "Ti~tle"
   ::Static4:clipSiblings := .T.
   ::Static4:options := XBPSTATIC_TEXT_VCENTER+XBPSTATIC_TEXT_RIGHT

   ::Combobox1    := XbpCombobox():new( ::Static1, , {96,12}, {72,60} )
   ::Combobox1:clipSiblings := .T.
   ::Combobox1:tabstop := .T.

   ::Static5      := XbpStatic():new( ::Static1, , {12,12}, {72,24} )
   ::Static5:caption := "~ZIP Code"
   ::Static5:clipSiblings := .T.
   ::Static5:options := XBPSTATIC_TEXT_VCENTER+XBPSTATIC_TEXT_RIGHT

   ::Spinbutton1  := XbpSpinbutton():new( ::Static1, , {96,12}, {72,24} )
   ::Spinbutton1:clipSiblings := .T.
   ::Spinbutton1:tabStop := .T.

   ::ButtonOK     := XbpPushButton():new( ::drawingArea, , {132,12}, {72,24} )
   ::ButtonOK:caption := "~Ok"
   ::ButtonOK:clipSiblings := .T.
   ::ButtonOK:tabStop := .T.
   ::ButtonOK:activate := {|| Gather( ::editControls ), PostAppEvent( xbeP_Close ) }

   ::ButtonCancel := XbpPushButton():new( ::drawingArea, , {216,12}, {96,24} )
   ::ButtonCancel:caption := "C~ancel"
   ::ButtonCancel:clipSiblings := .T.
   ::ButtonCancel:tabStop := .T.
   ::ButtonCancel:activate := {|| PostAppEvent( xbeP_Close ) }

RETURN self


******************************************************************************
* Systemresourcen anfordern
******************************************************************************
METHOD _NewForm:create( oParent, oOwner, aPos, aSize, aPP, lVisible )

   ::XbpDialog:create( oParent, oOwner, aPos, aSize, aPP, lVisible )

   ::TabPage1:create()

   ::Static6:create()

   ::Radiobutton1:create()

   ::Radiobutton2:create()

   ::Radiobutton3:create()

   ::Radiobutton4:create()

   ::Static7:create()

   ::o3state1:create()

   ::Checkbox2:create()

   ::Checkbox1:create()

   ::TabPage2:create()

   ::Static1:create()

   ::Static2:create()

   ::SLE1:create()

   ::Static3:create()

   ::SLE2:create()

   ::Static4:create()

   ::Combobox1:create()

   ::Static5:create()

   ::Spinbutton1:create()

   ::ButtonOK:create()

   ::ButtonCancel:create()


RETURN self

#endif

//EOF
/////
