//////////////////////////////////////////////////////////////////////
//
//  _WIZARD.PRG
//
//  Copyright:
//      Alaska Software, (c) 2001-2025. All rights reserved.         
//  
//  Contents:
//      Wizard dialog
//////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
//
//  Class code created by the Xbase++ FormDesigner
//    Creation date: 03.09.01 Creation time: 16.51.27
//
//  Contents  :
//    This file contains the implementation level of a form and is
//    overwritten automatically by the Xbase++ Form Designer.
//    Be careful when modifying this file since changes may get lost.
//
///////////////////////////////////////////////////////////////////////////////

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

      * Contained control elements
      VAR oImage    
      VAR oBtnBack  
      VAR oBtnNext  
      VAR oBtnFind  
      VAR oLabel    
      VAR oBtnCancel
      VAR oInput    

      METHOD init
      METHOD create
ENDCLASS

******************************************************************************
* Initialize form
******************************************************************************
METHOD _NewForm:init( oParent, oOwner, aPos, aSize, aPP, lVisible )

   DEFAULT oParent  TO AppDesktop(), ;
           aPos     TO {485,275}, ;
           aSize    TO {452,323}, ;
           lVisible TO .F.

   DEFAULT aPP TO {}
   AAdd ( aPP, { XBP_PP_COMPOUNDNAME, "8.Arial" } )
   ::XbpDialog:init( oParent, oOwner, aPos, aSize, aPP, lVisible )
   ::XbpDialog:taskList := .T.
   ::XbpDialog:title := "New form"

   ::editControls := {}

   ::oImage     := XbpStatic():new( ::drawingArea, , {12,24}, {108,264}, { { XBP_PP_BGCLR, -255 }, { XBP_PP_COMPOUNDNAME, "12.MS Sans Serif" } } )
   ::oImage:caption := 10
   ::oImage:clipSiblings := .T.
   ::oImage:type := XBPSTATIC_TYPE_BITMAP

   ::oBtnBack   := XbpPushButton():new( ::drawingArea, , {240,12}, {84,24}, { { XBP_PP_BGCLR, XBPSYSCLR_BUTTONMIDDLE }, { XBP_PP_FGCLR, -58 } } )
   ::oBtnBack:caption := "Back"
   ::oBtnBack:tabStop := .T.
   ::oBtnBack:activate := {|| NIL }

   ::oBtnNext   := XbpPushButton():new( ::drawingArea, , {336,12}, {84,24}, { { XBP_PP_BGCLR, XBPSYSCLR_BUTTONMIDDLE }, { XBP_PP_FGCLR, -58 } } )
   ::oBtnNext:caption := "Next"
   ::oBtnNext:tabStop := .T.
   ::oBtnNext:activate := {|| NIL }

   ::oBtnFind   := XbpPushButton():new( ::drawingArea, , {324,108}, {96,24}, { { XBP_PP_BGCLR, XBPSYSCLR_BUTTONMIDDLE }, { XBP_PP_FGCLR, -58 } } )
   ::oBtnFind:caption := "Find"
   ::oBtnFind:tabStop := .T.
   ::oBtnFind:activate := {|| NIL }

   ::oLabel     := XbpStatic():new( ::drawingArea, , {144,192}, {276,24} )
   ::oLabel:caption := "Text"
   ::oLabel:clipSiblings := .T.
   ::oLabel:options := XBPSTATIC_TEXT_VCENTER

   ::oBtnCancel := XbpPushButton():new( ::drawingArea, , {144,12}, {84,24}, { { XBP_PP_BGCLR, XBPSYSCLR_BUTTONMIDDLE }, { XBP_PP_FGCLR, -58 } } )
   ::oBtnCancel:caption := "Cancel"
   ::oBtnCancel:tabStop := .T.
   ::oBtnCancel:activate := {|| NIL }

   ::oInput     := XbpMle():new( ::drawingArea, , {144,144}, {276,48}, { { XBP_PP_BGCLR, XBPSYSCLR_ENTRYFIELD } } )
   ::oInput:horizScroll := .F.
   ::oInput:ignoreTab := .T.
   ::oInput:tabStop := .T.
   ::oInput:vertScroll := .F.

RETURN self


******************************************************************************
* Request system resources
******************************************************************************
METHOD _NewForm:create( oParent, oOwner, aPos, aSize, aPP, lVisible )

   ::XbpDialog:create( oParent, oOwner, aPos, aSize, aPP, lVisible )

   ::oImage:create()

   ::oBtnBack:create()

   ::oBtnNext:create()

   ::oBtnFind:create()

   ::oLabel:create()

   ::oBtnCancel:create()

   ::oInput:create()


RETURN self

#endif

//EOF
/////
