//////////////////////////////////////////////////////////////////////
//
//  OWNERDRW.PRG
//
//  Copyright:
//      Alaska Software, (c) 1997-2025. All rights reserved.         
//  
//  Contents:
//      This sample shows how owner-drawing can be used to give your
//      application a custom look-and-feel. This source file contains
//      the Main procedure and code for maintaining the application's
//      window.
//   
//////////////////////////////////////////////////////////////////////

#include "font.ch"
#include "xbp.ch"
#include "gra.ch"
#include "appevent.ch"

#define DIALOG_TITLE "Owner-Draw Example"

//
// Procedure Main. This procedure creates the main window
// and implements the application's event loop.
//
PROCEDURE Main()

  LOCAL oClrBox
  LOCAL oDlg
  LOCAL nEvent, mp1, mp2, oXbp


  // Create application's main  window
  oDlg := XbpDialog():new( AppDesktop() )
  oDlg:taskList := .T.
  oDlg:title    := DIALOG_TITLE
  oDlg:drawingArea:bitmap := 100
  oDlg:visible  := .F.
  oDlg:close    := {|| PostAppEvent(xbeP_Quit) }
  oDlg:Create( ,, {100,100},{600,400} )

  // Create the menu system and the
  // listbox for displaying the color
  // values
  CreateMenuSystem( oDlg )

  oClrBox       := XbpColorListbox():new( oDlg:drawingArea )
  oClrBox:create( ,, {50,20},{200,100} )

  oDlg:Show()
  SetAppFocus( oDlg )


  // Event Loop
  nEvent := xbe_None
  DO WHILE nEvent != xbeP_Quit
     nEvent := AppEvent( @mp1, @mp2, @oXbp )
     oXbp:handleEvent( nEvent, mp1, mp2 )
  ENDDO

RETURN

//
// Change the title of the dialog after some menue
// elements have been selected
//
PROCEDURE AppendTitle( oDlg, cMessage )
  oDlg:setTitle( DIALOG_TITLE + "   [" + cMessage + "]" )
RETURN


//
// Procedure CreateMenuSystem(). This procedure creates
// the menu system of the sample application.
//
PROCEDURE CreateMenuSystem( oDlg )

 LOCAL oFMenu, oHMenu, oSMenu, oMbar

   //
   // Create application's menu system using
   // instances of the XbpImageMenu class.
   // This class implements the logic and
   // message processing required for
   // owner-drawn menus.
   //
   // "File" popup-menu
   oFMenu := XbpImageMenu():new()
   oFMenu:BarText:= "File Menu"
   oFMenu:title  := "~File"
   oFMenu:create( oDlg )

   oFMenu:addItem( {"~Open..."+Chr(9)+"Ctrl+O",;
                   {|| AppendTitle(oDlg, "Open selected")},,   ;
                       XBPMENUBAR_MIA_OWNERDRAW } )

   oFMenu:addItem( {"~Save"+Chr(9)+"Ctrl+S",;
                   {|| AppendTitle(oDlg, "Save selected")},,   ;
                       XBPMENUBAR_MIA_OWNERDRAW } )

   oFMenu:addItem( {NIL,;
                   {|| NIL}, XBPMENUBAR_MIS_SEPARATOR, XBPMENUBAR_MIA_OWNERDRAW } )

   oFMenu:addItem( {"~Properties",;
                   {|| MsgBox("Settings")},,           XBPMENUBAR_MIA_OWNERDRAW },;
                   121 )

   oFMenu:addItem( {NIL,;
                   {|| NIL}, XBPMENUBAR_MIS_SEPARATOR, XBPMENUBAR_MIA_OWNERDRAW } )

   oFMenu:addItem( {"~Quit"+Chr(9)+"Alt+F4",;
                   {|| PostAppEvent(xbeP_Quit)},,      XBPMENUBAR_MIA_OWNERDRAW },;
                   120 )

   // "Help -> Help Articles" sub-menu
   oSMenu := XbpImageMenu():new():create( oDlg )
   oSMenu:title  := "~Help Articles"

   oSMenu:addItem( {"Article ~1..."+Chr(9)+"Ctrl+1",;
                  {|| AppendTitle(oDlg, "Article 1") },,  ;
                      XBPMENUBAR_MIA_OWNERDRAW }, 123 )

   oSMenu:addItem( {"Article ~2..."+Chr(9)+"Ctrl+2",;
                  {|| AppendTitle(oDlg, "Article 2") },,  ;
                      XBPMENUBAR_MIA_OWNERDRAW }, 123 )

   oSMenu:addItem( {"Article ~3..."+Chr(9)+"Ctrl+3",;
                  {|| AppendTitle(oDlg, "Article 3")},,   ;
                      XBPMENUBAR_MIA_OWNERDRAW }, 123 )

   // "Help" popup-menu
   oHMenu := XbpImageMenu():new()
   oHMenu:BarText:= "Help"
   oHMenu:title  := "~Help"
   oHMenu:create( oDlg )

   oHMenu:addItem( {oSMenu,,, XBPMENUBAR_MIA_OWNERDRAW}, 122 )

   oHMenu:addItem( {NIL,;
                   {|| NIL}, XBPMENUBAR_MIS_SEPARATOR, XBPMENUBAR_MIA_OWNERDRAW } )

   oHMenu:addItem( {"~About",;
                  {|| MsgBox("About")},, ;
                      XBPMENUBAR_MIA_OWNERDRAW }, 121 )

   // Add popup-menus to menubar
   oMbar := oDlg:menuBar()
   oMbar:measureItem := {|nItem,aDims,self| MeasureMenubarItem(oDlg,self,nItem,aDims) }
   oMbar:drawItem    := {|oPS,aInfo,self  | DrawMenubarItem(oDlg,self,oPS,aInfo) }
   oMbar:addItem( {oFMenu,,, XBPMENUBAR_MIA_OWNERDRAW} )
   oMbar:addItem( {oHMenu,,, XBPMENUBAR_MIA_OWNERDRAW} )

RETURN


//
// Overloaded AppSys() procedure. This prevents creation of the
// default XbpCrt window.
//
PROCEDURE AppSys()
RETURN
