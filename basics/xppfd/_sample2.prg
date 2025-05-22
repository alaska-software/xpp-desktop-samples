//////////////////////////////////////////////////////////////////////
//
//  _SAMPLE2.PRG
//
//  Copyright:
//     Alaska Software, (c) 1997-2025. All rights reserved.         
//  
//  Contents:
//     Class code created by the Xbase++ FormDesigner
//     Creation date: 05.12.97 Creation time: 13:48:13
//   
//  Remarks:
//     This file contains the implementation level of a form and is
//     overwritten automatically by the Xbase++ Form Designer.
//     Be careful when modifying this file since changes may get lost.
//   
//////////////////////////////////////////////////////////////////////


#ifndef _CUSTOMERFORM_
#define _CUSTOMERFORM_

#include "Gra.ch"
#include "Xbp.ch"
#include "Common.ch"
#include "Appevent.ch"
#include "Font.ch"

CLASS _CustomerForm FROM XbpDialog
   EXPORTED:
      VAR editControls

      * Contained control elements
      VAR ButtonOK      
      VAR ButtonNext    
      VAR ButtonPrevious
      VAR Static1       
      VAR Static2       
      VAR Lastname      
      VAR Static3       
      VAR Firstname     

      * Work area / alias
      VAR CUSTOMER

      METHOD init
      METHOD create
ENDCLASS

******************************************************************************
* Initialize form
******************************************************************************
METHOD _CustomerForm:init( oParent, oOwner, aPos, aSize, aPP, lVisible )

   DEFAULT oParent  TO AppDesktop(), ;
           aPos     TO {148,98}, ;
           lVisible TO .F. , ;
           aPP      TO {}

   AAdd ( aPP, { XBP_PP_COMPOUNDNAME, "8.Helv.normal" } )

   IF Empty(aSize) == .T.
      IF IsMemberVar(self,"ClientSize") == .T.
         aSize := {0,0}
      ELSE
         aSize := {354,197}
      ENDIF
   ENDIF

   ::XbpDialog:init( oParent, oOwner, aPos, aSize, aPP, lVisible )
   IF aSize[1] == 0 .AND. aSize[2] == 0
      ::XbpDialog:ClientSize := {334,155}
   ENDIF
   ::XbpDialog:border := XBPDLG_SIZEBORDER
   ::XbpDialog:taskList := .T.
   ::XbpDialog:title := "New Form    "
   ::XbpDialog:visible := .F.

   ::editControls := {}

   ::ButtonOK       := XbpPushButton():new( ::drawingArea, , {228,12}, {84,24} )
   ::ButtonOK:caption := "OK"
   ::ButtonOK:clipSiblings := .T.
   ::ButtonOK:activate := {|| Gather( ::editControls ), PostAppEvent( xbeP_Close ) }

   ::ButtonNext     := XbpPushButton():new( ::drawingArea, , {108,12}, {72,24} )
   ::ButtonNext:caption := "Next"
   ::ButtonNext:clipSiblings := .T.
   ::ButtonNext:activate := {|| Gather( ::editControls ), (::CUSTOMER)->(DbSkip(1)), Scatter ( ::editControls ) }

   ::ButtonPrevious := XbpPushButton():new( ::drawingArea, , {12,12}, {72,24} )
   ::ButtonPrevious:caption := "Previous"
   ::ButtonPrevious:clipSiblings := .T.
   ::ButtonPrevious:activate := {|| Gather( ::editControls ), (::CUSTOMER)->(DbSkip(-1)), Scatter ( ::editControls ) }

   ::Static1        := XbpStatic():new( ::drawingArea, , {12,48}, {300,96} )
   ::Static1:caption := "Customer"
   ::Static1:clipSiblings := .T.
   ::Static1:type := XBPSTATIC_TYPE_GROUPBOX

   ::Static2        := XbpStatic():new( ::Static1, , {12,48}, {72,24} )
   ::Static2:caption := "Lastname:"
   ::Static2:clipSiblings := .T.
   ::Static2:options := XBPSTATIC_TEXT_VCENTER

   ::Lastname       := XbpSLE():new( ::Static1, , {96,48}, {192,24} )
   ::Lastname:bufferLength := 20
   ::Lastname:clipSiblings := .T.
   ::Lastname:tabStop := .T.
   ::Lastname:dataLink := {|x| IIf( PCOUNT()==0, Trim( (::CUSTOMER)->LASTNAME ), (::CUSTOMER)->LASTNAME := x ) }
   AAdd( ::editControls, ::Lastname )

   ::Static3        := XbpStatic():new( ::Static1, , {12,12}, {72,24} )
   ::Static3:caption := "Firstname:"
   ::Static3:clipSiblings := .T.
   ::Static3:options := XBPSTATIC_TEXT_VCENTER

   ::Firstname      := XbpSLE():new( ::Static1, , {96,12}, {192,24} )
   ::Firstname:bufferLength := 20
   ::Firstname:clipSiblings := .T.
   ::Firstname:tabStop := .T.
   ::Firstname:dataLink := {|x| IIf( PCOUNT()==0, Trim( (::CUSTOMER)->FIRSTNAME ), (::CUSTOMER)->FIRSTNAME := x ) }
   AAdd( ::editControls, ::Firstname )

RETURN self


******************************************************************************
* Request system resources
******************************************************************************
METHOD _CustomerForm:create( oParent, oOwner, aPos, aSize, aPP, lVisible )

   ::XbpDialog:create( oParent, oOwner, aPos, aSize, aPP, lVisible )

   ::ButtonOK:create()

   ::ButtonNext:create()

   ::ButtonPrevious:create()

   ::Static1:create()

   ::Static2:create()

   ::Lastname:create()

   ::Static3:create()

   ::Firstname:create()


RETURN self

#endif

//EOF
/////
