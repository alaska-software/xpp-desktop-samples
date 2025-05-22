//////////////////////////////////////////////////////////////////////
//
//  _WEBBROWSER.PRG
//
//  Copyright:
//      Alaska Software, (c) 2002-2025. All rights reserved.         
//  
//  Contents:
//   
//////////////////////////////////////////////////////////////////////


#ifndef _XBPWEBBROWSER_
#define _XBPWEBBROWSER_

#include "Gra.ch"
#include "Xbp.ch"
#include "Common.ch"
#include "Appevent.ch"
#include "Font.ch"

CLASS _XbpWebBrowser FROM XbpDialog
   EXPORTED:
      VAR editControls

      * Contained control elements
      VAR BackButton    
      VAR ForwardButton 
      VAR HomeButton    
      VAR StopButton    
      VAR URLCombo      
      VAR URLCaption    
      VAR Separator     
      VAR Statusbar     
      VAR BrowserControl

      METHOD init
      METHOD create
ENDCLASS

******************************************************************************
* Initialize form
******************************************************************************
METHOD _XbpWebBrowser:init( oParent, oOwner, aPos, aSize, aPP, lVisible )

   DEFAULT oParent  TO AppDesktop(), ;
           aPos     TO {619,131}, ;
           lVisible TO .F.

   DEFAULT aPP TO {}
   AAdd ( aPP, { XBP_PP_COMPOUNDNAME, "8.Arial" } )
   
   IF Empty(aSize) == .T.
      IF IsMemberVar(self,"ClientSize") == .T.
         aSize := {0,0}
      ELSE
         aSize := {611,476}
      ENDIF
   ENDIF
   
   ::XbpDialog:init( oParent, oOwner, aPos, aSize, aPP, lVisible )
   IF aSize[1] == 0 .AND. aSize[2] == 0
      ::XbpDialog:ClientSize := {593,433}
   ENDIF
   ::XbpDialog:drawingArea:clipChildren := .F.
   ::XbpDialog:taskList := .T.
   ::XbpDialog:title := "XbpWebBrowser"

   ::editControls := {}

   ::BackButton     := XbpPushButton():new( ::drawingArea, , {16,384}, {48,40}, { { XBP_PP_BGCLR, XBPSYSCLR_BUTTONMIDDLE }, { XBP_PP_FGCLR, -58 } } )
   ::BackButton:caption := "Back"
   ::BackButton:tabStop := .T.
   ::BackButton:activate := {|| NIL }

   ::ForwardButton  := XbpPushButton():new( ::drawingArea, , {72,384}, {64,40}, { { XBP_PP_BGCLR, XBPSYSCLR_BUTTONMIDDLE }, { XBP_PP_FGCLR, -58 } } )
   ::ForwardButton:caption := "Forward"
   ::ForwardButton:tabStop := .T.
   ::ForwardButton:activate := {|| NIL }

   ::HomeButton     := XbpPushButton():new( ::drawingArea, , {144,384}, {48,40}, { { XBP_PP_BGCLR, XBPSYSCLR_BUTTONMIDDLE }, { XBP_PP_FGCLR, -58 } } )
   ::HomeButton:caption := "Home"
   ::HomeButton:tabStop := .T.
   ::HomeButton:activate := {|| NIL }

   ::StopButton     := XbpPushButton():new( ::drawingArea, , {208,384}, {48,40}, { { XBP_PP_BGCLR, XBPSYSCLR_BUTTONMIDDLE }, { XBP_PP_FGCLR, -58 } } )
   ::StopButton:caption := "Stop"
   ::StopButton:tabStop := .T.
   ::StopButton:activate := {|| NIL }

   ::URLCombo       := XbpCombobox():new( ::drawingArea, , {48,272}, {528,104}, { { XBP_PP_BGCLR, XBPSYSCLR_ENTRYFIELD } } )
   ::URLCombo:tabstop := .T.

   ::URLCaption     := XbpStatic():new( ::drawingArea, , {8,352}, {32,24}, { { XBP_PP_BGCLR, -255 } } )
   ::URLCaption:caption := "URL"
   ::URLCaption:clipSiblings := .T.
   ::URLCaption:options := XBPSTATIC_TEXT_VCENTER+XBPSTATIC_TEXT_RIGHT

   ::Separator      := XbpStatic():new( ::drawingArea, , {0,344}, {592,4}, { { XBP_PP_BGCLR, GRA_CLR_DARKGRAY }, { XBP_PP_FGCLR, GRA_CLR_DARKGRAY } } )
   ::Separator:caption := "Separator"
   ::Separator:clipSiblings := .T.
   ::Separator:options := XBPSTATIC_TEXT_VCENTER+XBPSTATIC_TEXT_RIGHT

   ::Statusbar      := XbpStatusBar():new( ::drawingArea, , {0,0}, {592,24} )

   ::BrowserControl := XbpHTMLViewer():new( ::drawingArea, , {0,24}, {592,312} )

RETURN self


******************************************************************************
* Request system resources
******************************************************************************
METHOD _XbpWebBrowser:create( oParent, oOwner, aPos, aSize, aPP, lVisible )

   ::XbpDialog:create( oParent, oOwner, aPos, aSize, aPP, lVisible )

   ::BackButton:create()

   ::ForwardButton:create()

   ::HomeButton:create()

   ::StopButton:create()

   ::URLCombo:create()

   ::URLCaption:create()

   ::Separator:create()

   ::Statusbar:create()
   ::Statusbar:Caption := "Panel"

   ::BrowserControl:create()
   ::BrowserControl:Visible := .T.

RETURN self

#endif

//EOF
/////
