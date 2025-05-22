//////////////////////////////////////////////////////////////////////
//
//  _EBROWSE.PRG
//
//  Copyright:
//     Alaska Software, (c) 1997-2025. All rights reserved.         
//  
//  Contents:
//     Class code created by the Xbase++ FormDesigner
//       Creation date: 20.10.97 Creation time: 12:08:36
//   
//  Remarks:
//     This file contains the implementation level of a form and is
//     overwritten automatically by the Xbase++ Form Designer.
//     Be careful when modifying this file since changes may get lost.
//   
//////////////////////////////////////////////////////////////////////


#ifndef _EDITFORM_
#define _EDITFORM_

#include "Gra.ch"
#include "Xbp.ch"
#include "Common.ch"
#include "Appevent.ch"

CLASS _EditForm FROM XbpDialog
   EXPORTED:
      VAR editControls

      * Contained control elements
      VAR oGroup       
      VAR oIncome      
      VAR oValueGroup  
      VAR oTotalCaption
      VAR oTotal       
      VAR oButtonCancel
      VAR oButtonOK    

      METHOD init
      METHOD create
ENDCLASS

******************************************************************************
* Initialize form
******************************************************************************
METHOD _EditForm:init( oParent, oOwner, aPos, aSize, aPP, lVisible )

   DEFAULT oParent  TO AppDesktop(), ;
           aPos     TO {443,163}, ;
           aSize    TO {267,321}, ;
           lVisible TO .F. , ;
           aPP      TO {}

   ::XbpDialog:init( oParent, oOwner, aPos, aSize, aPP, lVisible )
   ::XbpDialog:border := XBPDLG_SIZEBORDER
   ::XbpDialog:taskList := .T.
   ::XbpDialog:title := "New form"
   ::XbpDialog:visible := .F.

   ::editControls := {}

   ::oGroup        := XbpStatic():new( ::drawingArea, , {8,55}, {240,232} )
   ::oGroup:caption := "Group"
   ::oGroup:clipSiblings := .T.
   ::oGroup:type := XBPSTATIC_TYPE_GROUPBOX

   ::oIncome       := XbpSLE():new( ::oGroup, , {12,192}, {216,24} )
   ::oIncome:clipSiblings := .T.

   ::oValueGroup   := XbpStatic():new( ::oGroup, , {12,48}, {216,131} )
   ::oValueGroup:caption := "Gruppe"
   ::oValueGroup:clipSiblings := .T.
   ::oValueGroup:type := XBPSTATIC_TYPE_GROUPBOX

   ::oTotalCaption := XbpStatic():new( ::oGroup, , {12,12}, {120,23} )
   ::oTotalCaption:caption := "Total"
   ::oTotalCaption:clipSiblings := .T.
   ::oTotalCaption:options := XBPSTATIC_TEXT_VCENTER+XBPSTATIC_TEXT_RIGHT

   ::oTotal        := XbpSLE():new( ::oGroup, , {144,12}, {84,24} )
   ::oTotal:clipSiblings := .T.

   ::oButtonCancel := XbpPushButton():new( ::drawingArea, , {156,12}, {96,24} )
   ::oButtonCancel:caption := "Cancel"
   ::oButtonCancel:clipSiblings := .T.
   ::oButtonCancel:activate := {|| PostAppEvent( xbeP_Close ) }

   ::oButtonOK     := XbpPushButton():new( ::drawingArea, , {48,12}, {96,24} )
   ::oButtonOK:caption := "Ok"
   ::oButtonOK:clipSiblings := .T.
   ::oButtonOK:activate := {|| Gather( ::editControls ), PostAppEvent( xbeP_Close ) }

RETURN self


******************************************************************************
* Request system resources
******************************************************************************
METHOD _EditForm:create( oParent, oOwner, aPos, aSize, aPP, lVisible )

   ::XbpDialog:create( oParent, oOwner, aPos, aSize, aPP, lVisible )

   ::oGroup:create()

   ::oIncome:create()

   ::oValueGroup:create()

   ::oTotalCaption:create()

   ::oTotal:create()

   ::oButtonCancel:create()

   ::oButtonOK:create()


RETURN self

#endif

//EOF
/////
