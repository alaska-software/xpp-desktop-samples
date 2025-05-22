//////////////////////////////////////////////////////////////////////
//
//  _MAILDLG.PRG
//
//  Copyright:
//      Alaska Software, (c) 2002-2025. All rights reserved.         
//  
//  Contents:
//      Htmlmail Dialog base class.
//   
//////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////////////
//
//  Class code created by the Xbase++ FormDesigner
//    Creation date: 16.02.2005 Creation time: 15:21:48
//
//  Contents  :
//    This file contains the implementation level of a form and is
//    overwritten automatically by the Xbase++ Form Designer.
//    Be careful when modifying this file since changes may get lost.
//
///////////////////////////////////////////////////////////////////////////////

#ifndef _MAILSAMPLE_
#define _MAILSAMPLE_

#include "Gra.ch"
#include "Xbp.ch"
#include "Common.ch"
#include "Appevent.ch"
#include "Font.ch"

CLASS _MailSample FROM XbpDialog
   EXPORTED:
      VAR editControls

      * Contained control elements
      VAR Static1        
      VAR Static2        
      VAR Static3        
      VAR Static4        
      VAR oButtonSendmail
      VAR oListBoxOutput 
      VAR oEditEmail     
      VAR oEditServer    
      VAR oEditUsername  
      VAR oEditPassword  
      VAR oHtmlViewer    

      METHOD init
      METHOD create
ENDCLASS

******************************************************************************
* Initialize form
******************************************************************************
METHOD _MailSample:init( oParent, oOwner, aPos, aSize, aPP, lVisible )

   DEFAULT oParent  TO AppDesktop(), ;
           aPos     TO {1,28}, ;
           aSize    TO {906,615}, ;
           lVisible TO .F.

   DEFAULT aPP TO {}
   AAdd ( aPP, { XBP_PP_COMPOUNDNAME, "8.Arial" } )
   ::XbpDialog:init( oParent, oOwner, aPos, aSize, aPP, lVisible )
   ::XbpDialog:taskList := .T.
   ::XbpDialog:title := "MailSample"

   ::editControls := {}

   ::Static1         := XbpStatic():new( ::drawingArea, , {36,552}, {80,20} )
   ::Static1:caption := "Email "
   ::Static1:clipSiblings := .T.
   ::Static1:options := XBPSTATIC_TEXT_VCENTER+XBPSTATIC_TEXT_RIGHT

   ::Static2         := XbpStatic():new( ::drawingArea, , {36,524}, {80,20} )
   ::Static2:caption := "Server"
   ::Static2:clipSiblings := .T.
   ::Static2:options := XBPSTATIC_TEXT_VCENTER+XBPSTATIC_TEXT_RIGHT

   ::Static3         := XbpStatic():new( ::drawingArea, , {36,500}, {80,20} )
   ::Static3:caption := "Username"
   ::Static3:clipSiblings := .T.
   ::Static3:options := XBPSTATIC_TEXT_VCENTER+XBPSTATIC_TEXT_RIGHT

   ::Static4         := XbpStatic():new( ::drawingArea, , {36,472}, {80,20} )
   ::Static4:caption := "Password"
   ::Static4:clipSiblings := .T.
   ::Static4:options := XBPSTATIC_TEXT_VCENTER+XBPSTATIC_TEXT_RIGHT

   ::oButtonSendmail := XbpPushbutton():new( ::drawingArea, , {140,424}, {100,24}, { { XBP_PP_BGCLR, XBPSYSCLR_BUTTONMIDDLE }, { XBP_PP_FGCLR, -58 } } )
   ::oButtonSendmail:caption := "Send mail"
   ::oButtonSendmail:tabStop := .T.
   ::oButtonSendmail:activate := {|| NIL }

   ::oListBoxOutput  := XbpListBox():new( ::drawingArea, , {12,8}, {360,388}, { { XBP_PP_BGCLR, XBPSYSCLR_ENTRYFIELD } } )
   ::oListBoxOutput:tabStop := .T.

   ::oEditEmail      := XbpSLE():new( ::drawingArea, , {124,552}, {224,20}, { { XBP_PP_BGCLR, XBPSYSCLR_ENTRYFIELD } } )
   ::oEditEmail:bufferLength := 50
   ::oEditEmail:tabStop := .T.

   ::oEditServer     := XbpSLE():new( ::drawingArea, , {124,524}, {224,20}, { { XBP_PP_BGCLR, XBPSYSCLR_ENTRYFIELD } } )
   ::oEditServer:bufferLength := 50
   ::oEditServer:tabStop := .T.

   ::oEditUsername   := XbpSLE():new( ::drawingArea, , {124,500}, {224,20}, { { XBP_PP_BGCLR, XBPSYSCLR_ENTRYFIELD } } )
   ::oEditUsername:bufferLength := 50
   ::oEditUsername:tabStop := .T.

   ::oEditPassword   := XbpSLE():new( ::drawingArea, , {124,472}, {224,20}, { { XBP_PP_BGCLR, XBPSYSCLR_ENTRYFIELD } } )
   ::oEditPassword:bufferLength := 50
   ::oEditPassword:tabStop := .T.

   ::oHtmlViewer     := XbpHTMLViewer():new( ::drawingArea, , {388,8}, {500,572} ,, .T. )

RETURN self


******************************************************************************
* Request system resources
******************************************************************************
METHOD _MailSample:create( oParent, oOwner, aPos, aSize, aPP, lVisible )

   ::XbpDialog:create( oParent, oOwner, aPos, aSize, aPP, lVisible )

   ::Static1:create()

   ::Static2:create()

   ::Static3:create()

   ::Static4:create()

   ::oButtonSendmail:create()

   ::oListBoxOutput:create()

   ::oEditEmail:create()

   ::oEditServer:create()

   ::oEditUsername:create()

   ::oEditPassword:create()

   ::oHtmlViewer:create()


RETURN self

#endif

//EOF
/////
