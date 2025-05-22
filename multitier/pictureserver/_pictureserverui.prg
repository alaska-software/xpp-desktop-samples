//////////////////////////////////////////////////////////////////////
//
//  _PICTURESERVERUI.PRG
//
//  Copyright:
//   Alaska Software, (c) 2012-2025. All rights reserved.         
//  
//  Contents:
//   The design level of PictureServerUI.
//  Remarks:
//   
//////////////////////////////////////////////////////////////////////


#ifndef _PICTURESERVERUI_
#define _PICTURESERVERUI_

#include "Gra.ch"
#include "Xbp.ch"
#include "Common.ch"
#include "Appevent.ch"
#include "Font.ch"

#PRAGMA LIBRARY( "ascom10.lib" )

CLASS _PictureServerUI FROM XbpDialog
   EXPORTED:
      VAR editControls

      * Contained control elements
      VAR Static1          
      VAR _ComputerName    
      VAR Static2          
      VAR _Startup         
      VAR _Shutdown        
      VAR Static3          
      VAR _ConnectedClients
      VAR Static5          
      VAR _Processing      
      VAR Static6          
      VAR _Status          
      VAR _Port            
      VAR Static4          
      VAR Static7          
      VAR Static8          
      VAR Static9          
      VAR _BrowseUrl       
      VAR _Go              
      VAR _CopyUrl         

      METHOD init
      METHOD create
ENDCLASS

******************************************************************************
* Initialize form
******************************************************************************
METHOD _PictureServerUI:init( oParent, oOwner, aPos, aSize, aPP, lVisible )


   DEFAULT oParent  TO AppDesktop(), ;
           aPos     TO {641,342}, ;
           lVisible TO .F.

   DEFAULT aPP TO {}
   AAdd ( aPP, { XBP_PP_COMPOUNDNAME, "8.Arial" } )
   
   IF Empty(aSize) == .T.
      IF IsMemberVar(self,"ClientSize") == .T.
         aSize := {0,0}
      ELSE
         aSize := {558,374}
      ENDIF
   ENDIF
   
   ::XbpDialog:init( oParent, oOwner, aPos, aSize, aPP, lVisible )
   IF aSize[1] == 0 .AND. aSize[2] == 0
      ::XbpDialog:ClientSize := {542,338}
   ENDIF
   ::XbpDialog:taskList := .T.
   ::XbpDialog:title := "PictureServerUI"

   ::editControls := {}

   ::Static1           := XbpStatic():new( ::drawingArea, , {24,264}, {96,24}, { { XBP_PP_BGCLR, -255 } } )
   ::Static1:caption := "Computername:"
   ::Static1:clipSiblings := .T.
   ::Static1:options := XBPSTATIC_TEXT_VCENTER+XBPSTATIC_TEXT_RIGHT

   ::_ComputerName     := XbpStatic():new( ::drawingArea, , {132,264}, {228,24}, { { XBP_PP_BGCLR, -255 }, { XBP_PP_COMPOUNDNAME, "12.Arial" } } )
   ::_ComputerName:caption := "COMPUTERNAME"
   ::_ComputerName:clipSiblings := .T.
   ::_ComputerName:options := XBPSTATIC_TEXT_VCENTER

   ::Static2           := XbpStatic():new( ::drawingArea, , {36,240}, {84,24}, { { XBP_PP_BGCLR, -255 } } )
   ::Static2:caption := "Port:"
   ::Static2:clipSiblings := .T.
   ::Static2:options := XBPSTATIC_TEXT_VCENTER+XBPSTATIC_TEXT_RIGHT

   ::_Startup          := XbpPushButton():new( ::drawingArea, , {432,240}, {96,24}, { { XBP_PP_BGCLR, XBPSYSCLR_BUTTONMIDDLE }, { XBP_PP_FGCLR, -58 } } )
   ::_Startup:caption := "Start"
   ::_Startup:tabStop := .T.
   ::_Startup:activate := {|| NIL }

   ::_Shutdown         := XbpPushButton():new( ::drawingArea, , {432,216}, {96,24}, { { XBP_PP_BGCLR, XBPSYSCLR_BUTTONMIDDLE }, { XBP_PP_FGCLR, -58 } } )
   ::_Shutdown:caption := "Stop"
   ::_Shutdown:tabStop := .T.
   ::_Shutdown:activate := {|| NIL }

   ::Static3           := XbpStatic():new( ::drawingArea, , {24,192}, {96,24}, { { XBP_PP_BGCLR, -255 } } )
   ::Static3:caption := "Connected Clients:"
   ::Static3:clipSiblings := .T.
   ::Static3:options := XBPSTATIC_TEXT_VCENTER+XBPSTATIC_TEXT_RIGHT

   ::_ConnectedClients := XbpStatic():new( ::drawingArea, , {132,192}, {84,24}, { { XBP_PP_BGCLR, -255 }, { XBP_PP_FGCLR, GRA_CLR_RED } } )
   ::_ConnectedClients:caption := "NumCon"
   ::_ConnectedClients:clipSiblings := .T.
   ::_ConnectedClients:options := XBPSTATIC_TEXT_VCENTER

   ::Static5           := XbpStatic():new( ::drawingArea, , {36,24}, {84,24}, { { XBP_PP_BGCLR, -255 } } )
   ::Static5:caption := "Processing:"
   ::Static5:clipSiblings := .T.
   ::Static5:options := XBPSTATIC_TEXT_VCENTER+XBPSTATIC_TEXT_RIGHT

   ::_Processing       := XbpStatic():new( ::drawingArea, , {132,24}, {396,24}, { { XBP_PP_BGCLR, -255 } } )
   ::_Processing:caption := "CURRENT"
   ::_Processing:clipSiblings := .T.
   ::_Processing:options := XBPSTATIC_TEXT_VCENTER

   ::Static6           := XbpStatic():new( ::drawingArea, , {36,216}, {84,24}, { { XBP_PP_BGCLR, -255 } } )
   ::Static6:caption := "Status"
   ::Static6:clipSiblings := .T.
   ::Static6:options := XBPSTATIC_TEXT_VCENTER+XBPSTATIC_TEXT_RIGHT

   ::_Status           := XbpStatic():new( ::drawingArea, , {132,216}, {84,24}, { { XBP_PP_BGCLR, -255 } } )
   ::_Status:caption := "RUNNIN|Stopped"
   ::_Status:clipSiblings := .T.
   ::_Status:options := XBPSTATIC_TEXT_VCENTER

   ::_Port             := XbpSpinButton():new( ::drawingArea, , {132,240}, {60,24}, { { XBP_PP_BGCLR, XBPSYSCLR_ENTRYFIELD } } )
   ::_Port:tabStop := .T.

   ::Static4           := XbpStatic():new( ::drawingArea, , {36,108}, {84,24}, { { XBP_PP_BGCLR, -255 } } )
   ::Static4:caption := "Browse URL:"
   ::Static4:clipSiblings := .T.
   ::Static4:options := XBPSTATIC_TEXT_VCENTER+XBPSTATIC_TEXT_RIGHT

   ::Static7           := XbpStatic():new( ::drawingArea, , {12,300}, {516,24}, { { XBP_PP_BGCLR, GRA_CLR_BLUE }, { XBP_PP_FGCLR, GRA_CLR_WHITE } } )
   ::Static7:caption := "Server control"
   ::Static7:clipSiblings := .T.
   ::Static7:options := XBPSTATIC_TEXT_VCENTER+XBPSTATIC_TEXT_CENTER

   ::Static8           := XbpStatic():new( ::drawingArea, , {12,156}, {516,24}, { { XBP_PP_BGCLR, GRA_CLR_DARKGRAY }, { XBP_PP_FGCLR, GRA_CLR_WHITE } } )
   ::Static8:caption := "Information"
   ::Static8:clipSiblings := .T.
   ::Static8:options := XBPSTATIC_TEXT_VCENTER+XBPSTATIC_TEXT_CENTER

   ::Static9           := XbpStatic():new( ::drawingArea, , {12,60}, {516,24}, { { XBP_PP_BGCLR, GRA_CLR_BROWN }, { XBP_PP_FGCLR, GRA_CLR_WHITE } } )
   ::Static9:caption := "Activity"
   ::Static9:clipSiblings := .T.
   ::Static9:options := XBPSTATIC_TEXT_VCENTER+XBPSTATIC_TEXT_CENTER

   ::_BrowseUrl        := XbpStatic():new( ::drawingArea, , {132,108}, {216,24}, { { XBP_PP_BGCLR, -255 } } )
   ::_BrowseUrl:caption := "BROWSEURL"
   ::_BrowseUrl:clipSiblings := .T.
   ::_BrowseUrl:options := XBPSTATIC_TEXT_VCENTER

   ::_Go               := XbpPushButton():new( ::drawingArea, , {432,96}, {96,24}, { { XBP_PP_BGCLR, XBPSYSCLR_BUTTONMIDDLE }, { XBP_PP_FGCLR, -58 } } )
   ::_Go:caption := "Start Browser"
   ::_Go:tabStop := .T.
   ::_Go:activate := {|| NIL }

   ::_CopyUrl          := XbpPushButton():new( ::drawingArea, , {432,120}, {96,24}, { { XBP_PP_BGCLR, XBPSYSCLR_BUTTONMIDDLE }, { XBP_PP_FGCLR, -58 } } )
   ::_CopyUrl:caption := "URL to Clipboard"
   ::_CopyUrl:tabStop := .T.
   ::_CopyUrl:activate := {|| NIL }

RETURN self


******************************************************************************
* Request system resources
******************************************************************************
METHOD _PictureServerUI:create( oParent, oOwner, aPos, aSize, aPP, lVisible )

   ::XbpDialog:create( oParent, oOwner, aPos, aSize, aPP, lVisible )

   ::Static1:create()

   ::_ComputerName:create()

   ::Static2:create()

   ::_Startup:create()

   ::_Shutdown:create()

   ::Static3:create()

   ::_ConnectedClients:create()

   ::Static5:create()

   ::_Processing:create()

   ::Static6:create()

   ::_Status:create()

   ::_Port:create()

   ::Static4:create()

   ::Static7:create()

   ::Static8:create()

   ::Static9:create()

   ::_BrowseUrl:create()

   ::_Go:create()

   ::_CopyUrl:create()


RETURN self

#endif

//EOF
/////
