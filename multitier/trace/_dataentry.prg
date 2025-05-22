//////////////////////////////////////////////////////////////////////
//
//  _DATAENTRY.PRG
//
//  Copyright:
//   Alaska Software, (c) 2012-2025. All rights reserved.         
//  
//  Contents:
//   The design level of the class DataEntry
//   
//  Remarks:
//   
//////////////////////////////////////////////////////////////////////


#ifndef _DATAENTRY_
#define _DATAENTRY_

#include "Gra.ch"
#include "Xbp.ch"
#include "Common.ch"
#include "Appevent.ch"
#include "Font.ch"

#PRAGMA LIBRARY( "ascom10.lib" )

CLASS _DataEntry FROM XbpDialog
   EXPORTED:
      VAR editControls

      * Contained control elements
      VAR Static1       
      VAR _Port         
      VAR Static2       
      VAR _ComputerName 
      VAR Static3       
      VAR Static4       
      VAR _UrlDisplay   
      VAR _CopyUrl      
      VAR ButtonContinue
      VAR Static5       
      VAR _StartBrowser 

      METHOD init
      METHOD create
ENDCLASS

******************************************************************************
* Initialize form
******************************************************************************
METHOD _DataEntry:init( oParent, oOwner, aPos, aSize, aPP, lVisible )


   DEFAULT oParent  TO AppDesktop(), ;
           aPos     TO {85,407}, ;
           lVisible TO .F.

   DEFAULT aPP TO {}
   AAdd ( aPP, { XBP_PP_COMPOUNDNAME, "8.Arial" } )
   
   IF Empty(aSize) == .T.
      IF IsMemberVar(self,"ClientSize") == .T.
         aSize := {0,0}
      ELSE
         aSize := {404,335}
      ENDIF
   ENDIF
   
   ::XbpDialog:init( oParent, oOwner, aPos, aSize, aPP, lVisible )
   IF aSize[1] == 0 .AND. aSize[2] == 0
      ::XbpDialog:ClientSize := {388,299}
   ENDIF
   ::XbpDialog:taskList := .T.
   ::XbpDialog:title := "DataEntry"

   ::editControls := {}

   ::Static1        := XbpStatic():new( ::drawingArea, , {60,204}, {60,24}, { { XBP_PP_BGCLR, -255 } } )
   ::Static1:caption := "Port"
   ::Static1:clipSiblings := .T.
   ::Static1:options := XBPSTATIC_TEXT_VCENTER+XBPSTATIC_TEXT_RIGHT

   ::_Port          := XbpSpinButton():new( ::drawingArea, , {132,204}, {60,24}, { { XBP_PP_BGCLR, XBPSYSCLR_ENTRYFIELD } } )
   ::_Port:tabStop := .T.

   ::Static2        := XbpStatic():new( ::drawingArea, , {36,228}, {84,24}, { { XBP_PP_BGCLR, -255 } } )
   ::Static2:caption := "Computer Name:"
   ::Static2:clipSiblings := .T.
   ::Static2:options := XBPSTATIC_TEXT_VCENTER+XBPSTATIC_TEXT_RIGHT

   ::_ComputerName  := XbpStatic():new( ::drawingArea, , {132,228}, {228,24}, { { XBP_PP_BGCLR, -255 } } )
   ::_ComputerName:caption := "COMPUTERNAME"
   ::_ComputerName:clipSiblings := .T.
   ::_ComputerName:options := XBPSTATIC_TEXT_VCENTER

   ::Static3        := XbpStatic():new( ::drawingArea, , {12,264}, {360,24}, { { XBP_PP_BGCLR, GRA_CLR_DARKBLUE }, { XBP_PP_FGCLR, GRA_CLR_WHITE } } )
   ::Static3:caption := "Setting"
   ::Static3:clipSiblings := .T.
   ::Static3:options := XBPSTATIC_TEXT_VCENTER+XBPSTATIC_TEXT_CENTER

   ::Static4        := XbpStatic():new( ::drawingArea, , {12,168}, {360,24}, { { XBP_PP_BGCLR, GRA_CLR_DARKRED }, { XBP_PP_FGCLR, GRA_CLR_WHITE } } )
   ::Static4:caption := "Connect your Browser with this URL:"
   ::Static4:clipSiblings := .T.
   ::Static4:options := XBPSTATIC_TEXT_VCENTER+XBPSTATIC_TEXT_CENTER

   ::_UrlDisplay    := XbpStatic():new( ::drawingArea, , {12,132}, {360,24}, { { XBP_PP_BGCLR, GRA_CLR_WHITE } } )
   ::_UrlDisplay:caption := "URL"
   ::_UrlDisplay:clipSiblings := .T.
   ::_UrlDisplay:options := XBPSTATIC_TEXT_VCENTER+XBPSTATIC_TEXT_CENTER

   ::_CopyUrl       := XbpPushButton():new( ::drawingArea, , {12,96}, {144,24}, { { XBP_PP_BGCLR, XBPSYSCLR_BUTTONMIDDLE }, { XBP_PP_FGCLR, -58 } } )
   ::_CopyUrl:caption := "Copy Url to Clipboard"
   ::_CopyUrl:tabStop := .T.
   ::_CopyUrl:activate := {|| NIL }

   ::ButtonContinue := XbpPushButton():new( ::drawingArea, , {132,12}, {96,24}, { { XBP_PP_BGCLR, XBPSYSCLR_BUTTONMIDDLE }, { XBP_PP_FGCLR, -58 } } )
   ::ButtonContinue:caption := "Continue"
   ::ButtonContinue:tabStop := .T.
   ::ButtonContinue:activate := {|| Gather( ::editControls ), PostAppEvent( xbeP_Close ) }

   ::Static5        := XbpStatic():new( ::drawingArea, , {12,48}, {360,24}, { { XBP_PP_BGCLR, GRA_CLR_BROWN }, { XBP_PP_FGCLR, GRA_CLR_WHITE } } )
   ::Static5:caption := "Continue to executa application"
   ::Static5:clipSiblings := .T.
   ::Static5:options := XBPSTATIC_TEXT_VCENTER+XBPSTATIC_TEXT_CENTER

   ::_StartBrowser  := XbpCheckBox():new( ::drawingArea, , {228,96}, {108,24}, { { XBP_PP_BGCLR, -255 } } )
   ::_StartBrowser:caption := "Start Browser"
   ::_StartBrowser:tabStop := .T.
   ::_StartBrowser:selected := {|| NIL }

RETURN self


******************************************************************************
* Request system resources
******************************************************************************
METHOD _DataEntry:create( oParent, oOwner, aPos, aSize, aPP, lVisible )

   ::XbpDialog:create( oParent, oOwner, aPos, aSize, aPP, lVisible )

   ::Static1:create()

   ::_Port:create()

   ::Static2:create()

   ::_ComputerName:create()

   ::Static3:create()

   ::Static4:create()

   ::_UrlDisplay:create()

   ::_CopyUrl:create()

   ::ButtonContinue:create()

   ::Static5:create()

   ::_StartBrowser:create()


RETURN self

#endif

//EOF
/////
