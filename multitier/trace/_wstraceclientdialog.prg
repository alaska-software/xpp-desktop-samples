//////////////////////////////////////////////////////////////////////
//
//  _WSTRACECLIENTDIALOG.PRG
//
//  Copyright:
//   Alaska Software, (c) 2012-2025. All rights reserved.         
//  
//  Contents:
//   
//     Class code created by the Xbase++ FormDesigner
//       Creation date: 30.11.2012 Creation time: 12:55:20
//   
//     This file contains the implementation level of a form and is
//     overwritten automatically by the Xbase++ Form Designer.
//     Be careful when modifying this file since changes may get lost.
//   
//  Remarks:
//   
//////////////////////////////////////////////////////////////////////


#ifndef _WSTRACECLIENTDIALOG_
#define _WSTRACECLIENTDIALOG_

#include "Gra.ch"
#include "Xbp.ch"
#include "Common.ch"
#include "Appevent.ch"
#include "Font.ch"

#PRAGMA LIBRARY( "ascom10.lib" )

CLASS _WSTraceClientDialog FROM XbpDialog
   EXPORTED:
      VAR editControls

      * Contained control elements
      VAR _Text
      VAR _TraceClientLabel
      VAR _Port
      VAR Static2
      VAR _Button
      VAR Static3
      VAR Static4
      VAR Static5
      VAR Static6

      METHOD init
      METHOD create
ENDCLASS

******************************************************************************
* Initialize form
******************************************************************************
METHOD _WSTraceClientDialog:init( oParent, oOwner, aPos, aSize, aPP, lVisible )


   DEFAULT oParent  TO AppDesktop(), ;
           aPos     TO {60,97}, ;
           lVisible TO .F.

   DEFAULT aPP TO {}
   AAdd ( aPP, { XBP_PP_COMPOUNDNAME, "8.Arial" } )

   IF Empty(aSize) == .T.
      IF IsMemberVar(self,"ClientSize") == .T.
         aSize := {0,0}
      ELSE
         aSize := {596,217}
      ENDIF
   ENDIF

   ::XbpDialog:init( oParent, oOwner, aPos, aSize, aPP, lVisible )
   IF aSize[1] == 0 .AND. aSize[2] == 0
      ::XbpDialog:ClientSize := {580,181}
   ENDIF
   ::XbpDialog:taskList := .T.
   ::XbpDialog:title := "WSTraceClientDialog"

   ::editControls := {}

   ::_Text             := XbpStatic():new( ::drawingArea, , {108,48}, {432,24}, { { XBP_PP_BGCLR, GRA_CLR_WHITE } } )
   ::_Text:caption := ""
   ::_Text:clipSiblings := .T.
   ::_Text:options := XBPSTATIC_TEXT_VCENTER

   ::_TraceClientLabel := XbpStatic():new( ::drawingArea, , {12,120}, {192,48}, { { XBP_PP_BGCLR, -255 }, { XBP_PP_COMPOUNDNAME, "24.MS Sans Serif" } } )
   ::_TraceClientLabel:caption := "Trace Client"
   ::_TraceClientLabel:clipSiblings := .T.
   ::_TraceClientLabel:options := XBPSTATIC_TEXT_VCENTER+XBPSTATIC_TEXT_CENTER

   ::_Port             := XbpSpinButton():new( ::drawingArea, , {264,132}, {60,24}, { { XBP_PP_BGCLR, XBPSYSCLR_ENTRYFIELD } } )
   ::_Port:tabStop := .T.

   ::Static2           := XbpStatic():new( ::drawingArea, , {204,132}, {48,24}, { { XBP_PP_BGCLR, -255 }, { XBP_PP_COMPOUNDNAME, "16.MS Sans Serif" } } )
   ::Static2:caption := "Port:"
   ::Static2:clipSiblings := .T.
   ::Static2:options := XBPSTATIC_TEXT_VCENTER+XBPSTATIC_TEXT_RIGHT

   ::_Button           := XbpPushButton():new( ::drawingArea, , {360,132}, {96,24}, { { XBP_PP_BGCLR, XBPSYSCLR_BUTTONMIDDLE }, { XBP_PP_FGCLR, -58 } } )
   ::_Button:caption := "Connect"
   ::_Button:tabStop := .T.
   ::_Button:activate := {|| NIL }

   ::Static3           := XbpStatic():new( ::drawingArea, , {12,84}, {552,24}, { { XBP_PP_BGCLR, GRA_CLR_DARKPINK } } )
   ::Static3:caption := ""
   ::Static3:clipSiblings := .T.
   ::Static3:options := XBPSTATIC_TEXT_VCENTER+XBPSTATIC_TEXT_RIGHT

   ::Static4           := XbpStatic():new( ::drawingArea, , {12,12}, {552,24}, { { XBP_PP_BGCLR, GRA_CLR_DARKPINK } } )
   ::Static4:caption := ""
   ::Static4:clipSiblings := .T.
   ::Static4:options := XBPSTATIC_TEXT_VCENTER+XBPSTATIC_TEXT_RIGHT

   ::Static5           := XbpStatic():new( ::drawingArea, , {552,48}, {12,24}, { { XBP_PP_BGCLR, GRA_CLR_DARKRED } } )
   ::Static5:caption := ""
   ::Static5:clipSiblings := .T.
   ::Static5:options := XBPSTATIC_TEXT_VCENTER+XBPSTATIC_TEXT_RIGHT

   ::Static6           := XbpStatic():new( ::drawingArea, , {12,48}, {84,24}, { { XBP_PP_BGCLR, GRA_CLR_DARKRED }, { XBP_PP_FGCLR, GRA_CLR_WHITE } } )
   ::Static6:caption := "Handled Event:"
   ::Static6:clipSiblings := .T.
   ::Static6:options := XBPSTATIC_TEXT_VCENTER+XBPSTATIC_TEXT_RIGHT

RETURN self


******************************************************************************
* Request system resources
******************************************************************************
METHOD _WSTraceClientDialog:create( oParent, oOwner, aPos, aSize, aPP, lVisible )

   ::XbpDialog:create( oParent, oOwner, aPos, aSize, aPP, lVisible )

   ::_Text:create()

   ::_TraceClientLabel:create()

   ::_Port:create()

   ::Static2:create()

   ::_Button:create()

   ::Static3:create()

   ::Static4:create()

   ::Static5:create()

   ::Static6:create()


RETURN self

#endif

//EOF
/////
