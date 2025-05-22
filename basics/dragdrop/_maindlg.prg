///////////////////////////////////////////////////////////////////////////////
//
//  Class code created by the Xbase++ FormDesigner
//    Creation date: 25.07.2005 Creation time: 13:06:56
//
//  Contents  :
//    This file contains the implementation level of a form and is
//    overwritten automatically by the Xbase++ Form Designer.
//    Be careful when modifying this file since changes may get lost.
//
///////////////////////////////////////////////////////////////////////////////

#ifndef _MAINDIALOG_
#define _MAINDIALOG_

#include "Gra.ch"
#include "Xbp.ch"
#include "Common.ch"
#include "Appevent.ch"
#include "Font.ch"

CLASS _MainDialog FROM XbpDialog
   EXPORTED:
      VAR editControls

      * Contained control elements
      VAR Statusbar

      METHOD init
      METHOD create
ENDCLASS

******************************************************************************
* Initialize form
******************************************************************************
METHOD _MainDialog:init( oParent, oOwner, aPos, aSize, aPP, lVisible )

   DEFAULT oParent  TO AppDesktop(), ;
           aPos     TO {457,346}, ;
           aSize    TO {601,400}, ;
           lVisible TO .F.

   DEFAULT aPP TO {}
   AAdd ( aPP, { XBP_PP_COMPOUNDNAME, "8.Arial" } )
   ::XbpDialog:init( oParent, oOwner, aPos, aSize, aPP, lVisible )
   ::XbpDialog:drawingArea:clipChildren := .T.
   ::XbpDialog:taskList := .T.
   ::XbpDialog:title := "MainDialog"

   ::editControls := {}

   ::Statusbar := XbpStatusBar():new( ::drawingArea, , {0,0}, {592,24} )

RETURN self


******************************************************************************
* Request system resources
******************************************************************************
METHOD _MainDialog:create( oParent, oOwner, aPos, aSize, aPP, lVisible )

   ::XbpDialog:create( oParent, oOwner, aPos, aSize, aPP, lVisible )

   ::Statusbar:create()


RETURN self

#endif

//EOF
/////