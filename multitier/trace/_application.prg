//////////////////////////////////////////////////////////////////////
//
//  _APPLICATION.PRG
//
//  Copyright:
//   Alaska Software, (c) 2012-2025. All rights reserved.         
//  
//  Contents:
//   The design level of the class Application
//   
//  Remarks:
//   
//////////////////////////////////////////////////////////////////////


#ifndef _APPLICATION_
#define _APPLICATION_

#include "Gra.ch"
#include "Xbp.ch"
#include "Common.ch"
#include "Appevent.ch"
#include "Font.ch"

#PRAGMA LIBRARY( "ascom10.lib" )

CLASS _Application FROM XbpDialog
   EXPORTED:
      VAR editControls

      * Contained control elements
      VAR Static1      
      VAR _HandledEvent
      VAR _HintMessage 
      VAR Static2      
      VAR Static3      
      VAR Static4            
      VAR Static5            
      VAR _NumClients        
      VAR _StartBrowserClient
      VAR _StartXbaseClient  

      METHOD init
      METHOD create
ENDCLASS

******************************************************************************
* Initialize form
******************************************************************************
METHOD _Application:init( oParent, oOwner, aPos, aSize, aPP, lVisible )


   DEFAULT oParent  TO AppDesktop(), ;
           aPos     TO {53,346}, ;
           lVisible TO .F.

   DEFAULT aPP TO {}
   AAdd ( aPP, { XBP_PP_COMPOUNDNAME, "8.Arial" } )
   
   IF Empty(aSize) == .T.
      IF IsMemberVar(self,"ClientSize") == .T.
         aSize := {0,0}
      ELSE
         aSize := {594,402}
      ENDIF
   ENDIF
   
   ::XbpDialog:init( oParent, oOwner, aPos, aSize, aPP, lVisible )
   IF aSize[1] == 0 .AND. aSize[2] == 0
      ::XbpDialog:ClientSize := {578,366}
   ENDIF
   ::XbpDialog:taskList := .T.
   ::XbpDialog:title := "Application"

   ::editControls := {}

   ::Static1             := XbpStatic():new( ::drawingArea, , {12,216}, {84,24}, { { XBP_PP_BGCLR, GRA_CLR_DARKPINK }, { XBP_PP_FGCLR, GRA_CLR_WHITE } } )
   ::Static1:caption := "Handled event:"
   ::Static1:clipSiblings := .T.
   ::Static1:options := XBPSTATIC_TEXT_VCENTER+XBPSTATIC_TEXT_CENTER

   ::_HandledEvent       := XbpStatic():new( ::drawingArea, , {108,216}, {432,24}, { { XBP_PP_BGCLR, GRA_CLR_WHITE } } )
   ::_HandledEvent:caption := "HANDLEDEVENT"
   ::_HandledEvent:clipSiblings := .T.
   ::_HandledEvent:options := XBPSTATIC_TEXT_VCENTER

   ::_HintMessage        := XbpStatic():new( ::drawingArea, , {12,252}, {552,24}, { { XBP_PP_BGCLR, GRA_CLR_DARKRED }, { XBP_PP_FGCLR, GRA_CLR_WHITE } } )
   ::_HintMessage:caption := "HINTMESSAGE"
   ::_HintMessage:clipSiblings := .T.
   ::_HintMessage:options := XBPSTATIC_TEXT_VCENTER+XBPSTATIC_TEXT_CENTER

   ::Static2             := XbpStatic():new( ::drawingArea, , {12,180}, {552,24}, { { XBP_PP_BGCLR, GRA_CLR_DARKRED } } )
   ::Static2:caption := ""
   ::Static2:clipSiblings := .T.
   ::Static2:options := XBPSTATIC_TEXT_VCENTER+XBPSTATIC_TEXT_RIGHT

   ::Static3             := XbpStatic():new( ::drawingArea, , {552,216}, {12,24}, { { XBP_PP_BGCLR, GRA_CLR_DARKPINK } } )
   ::Static3:caption := "Text"
   ::Static3:clipSiblings := .T.
   ::Static3:options := XBPSTATIC_TEXT_VCENTER+XBPSTATIC_TEXT_RIGHT

   ::Static4             := XbpStatic():new( ::drawingArea, , {12,312}, {204,36}, { { XBP_PP_BGCLR, -255 }, { XBP_PP_COMPOUNDNAME, "24.MS Sans Serif" } } )
   ::Static4:caption := "Trace Server"
   ::Static4:clipSiblings := .T.
   ::Static4:options := XBPSTATIC_TEXT_VCENTER

   ::Static5             := XbpStatic():new( ::drawingArea, , {12,288}, {168,24}, { { XBP_PP_BGCLR, -255 }, { XBP_PP_COMPOUNDNAME, "16.MS Sans Serif" } } )
   ::Static5:caption := "Connected Clients:"
   ::Static5:clipSiblings := .T.
   ::Static5:options := XBPSTATIC_TEXT_VCENTER+XBPSTATIC_TEXT_RIGHT

   ::_NumClients         := XbpStatic():new( ::drawingArea, , {192,288}, {48,24}, { { XBP_PP_BGCLR, -255 }, { XBP_PP_FGCLR, GRA_CLR_RED }, { XBP_PP_COMPOUNDNAME, "16.MS Sans Serif" } } )
   ::_NumClients:caption := "0"
   ::_NumClients:clipSiblings := .T.
   ::_NumClients:options := XBPSTATIC_TEXT_VCENTER

   ::_StartBrowserClient := XbpPushButton():new( ::drawingArea, , {408,324}, {144,24}, { { XBP_PP_BGCLR, XBPSYSCLR_BUTTONMIDDLE }, { XBP_PP_FGCLR, -58 } } )
   ::_StartBrowserClient:caption := "Start Browser Client"
   ::_StartBrowserClient:tabStop := .T.
   ::_StartBrowserClient:activate := {|| NIL }

   ::_StartXbaseClient   := XbpPushButton():new( ::drawingArea, , {408,288}, {144,24}, { { XBP_PP_BGCLR, XBPSYSCLR_BUTTONMIDDLE }, { XBP_PP_FGCLR, -58 } } )
   ::_StartXbaseClient:caption := "Start Xbase++ Client"
   ::_StartXbaseClient:tabStop := .T.
   ::_StartXbaseClient:activate := {|| NIL }

RETURN self


******************************************************************************
* Request system resources
******************************************************************************
METHOD _Application:create( oParent, oOwner, aPos, aSize, aPP, lVisible )

   ::XbpDialog:create( oParent, oOwner, aPos, aSize, aPP, lVisible )

   ::Static1:create()

   ::_HandledEvent:create()

   ::_HintMessage:create()

   ::Static2:create()

   ::Static3:create()

   ::Static4:create()

   ::Static5:create()

   ::_NumClients:create()

   ::_StartBrowserClient:create()

   ::_StartXbaseClient:create()


RETURN self

#endif

//EOF
/////
