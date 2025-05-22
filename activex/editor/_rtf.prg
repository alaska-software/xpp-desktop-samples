//////////////////////////////////////////////////////////////////////
//
//  _RTF.PRG
//
//  Copyright:
//      Alaska Software, (c) 2002-2025. All rights reserved.         
//  
//  Contents:
//   
//////////////////////////////////////////////////////////////////////


#ifndef _RTFFORM_
#define _RTFFORM_

#include "Gra.ch"
#include "Xbp.ch"
#include "Common.ch"
#include "Appevent.ch"
#include "Font.ch"

CLASS _RtfForm FROM XbpDialog
   EXPORTED:
      VAR editControls

      * Contained control elements
      VAR oToolbar  
      VAR oFontSize 
      VAR oFontName 
      VAR oStatusbar
      VAR oRTF      

      METHOD init
      METHOD create
ENDCLASS

******************************************************************************
* Initialize form
******************************************************************************
METHOD _RTFForm:init( oParent, oOwner, aPos, aSize, aPP, lVisible )

   DEFAULT oParent  TO AppDesktop(), ;
           aPos     TO {599,253}, ;
           aSize    TO {600,395}, ;
           lVisible TO .F.

   DEFAULT aPP TO {}
   AAdd ( aPP, { XBP_PP_COMPOUNDNAME, "8.Arial" } )
   ::XbpDialog:init( oParent, oOwner, aPos, aSize, aPP, lVisible )
   ::XbpDialog:drawingArea:clipChildren := .T.
   ::XbpDialog:taskList := .T.
   ::XbpDialog:title := "RTFForm"

   ::editControls := {}

   ::oToolbar   := XbpToolBar():new( ::drawingArea, , {0,336}, {592,40} ,, .T. )

   ::oFontSize  := XbpComboBox():new( ::oToolbar, , {244,-150}, {56,172}, { { XBP_PP_BGCLR, XBPSYSCLR_ENTRYFIELD } } )
   ::oFontSize:tabstop := .T.

   ::oFontName  := XbpComboBox():new( ::oToolbar, , {8,-150}, {232,172}, { { XBP_PP_BGCLR, XBPSYSCLR_ENTRYFIELD } } )
   ::oFontName:tabstop := .T.

   ::oStatusbar := XbpStatusBar():new( ::drawingArea, , {0,0}, {592,28} ,, .T. )

   ::oRTF       := XbpRTF():new( ::drawingArea, , {0,28}, {592,308} ,, .T. )

RETURN self


******************************************************************************
* Request system resources
******************************************************************************
METHOD _RTFForm:create( oParent, oOwner, aPos, aSize, aPP, lVisible )

   ::XbpDialog:create( oParent, oOwner, aPos, aSize, aPP, lVisible )

   ::oToolbar:create()
   ::oToolbar:Style := XBPTOOLBAR_STYLE_FLAT
   ::oToolbar:BorderStyle := XBPFRAME_RECT

   ::oFontSize:create()

   ::oFontName:create()

   ::oStatusbar:create()
   ::oStatusbar:Caption := "Panel"

   ::oRTF:create()
   ::oRTF:Scrollbars := XBP_SCROLLBAR_HORIZ+XBP_SCROLLBAR_VERT


RETURN self

#endif

//EOF
/////
