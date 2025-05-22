//////////////////////////////////////////////////////////////////////
//
//  SDIMENU.PRG
//
//  Copyright:
//      Alaska Software, (c) 1997-2025. All rights reserved.         
//  
//  Contents:
//      Create menu system for SDIDEMO.EXE
//   
//////////////////////////////////////////////////////////////////////


#include "Xbp.ch"
#include "Sdidemo.ch"



/*
 * Create menu system in menubar of the dialog
 */
PROCEDURE MenuCreate( oMenuBar )
   LOCAL oMenu

   // First sub-menu
   //
   oMenu := SubMenuNew( oMenuBar, "~File" )
   oMenu:setName( ID_FILE_MENU )

   oMenu:addItem( { "~Customer", } )
   oMenu:addItem( { "~Parts"   , } )
   oMenu:addItem( MENUITEM_SEPARATOR )
   oMenu:addItem( { "Cl~ose" , NIL , 0, ;
                    XBPMENUBAR_MIA_DISABLED } )
   oMenu:addItem( { "P~rint" , NIL , 0, ;
                    XBPMENUBAR_MIA_DISABLED } )
   oMenu:addItem( { "~Options", } )
   oMenu:addItem( MENUITEM_SEPARATOR )
   oMenu:addItem( { "~Exit" , NIL } )

   oMenu:itemSelected := ;
      {|nItem,mp2,obj| MenuSelect(obj, 100+nItem) } 

   oMenuBar:addItem( {oMenu, NIL} )

   // Second sub-menu  -> editing data
   //
   oMenu := SubMenuNew( oMenuBar, "~Data" )
   oMenu:setName( ID_DATA_MENU )

   oMenu:addItem( { "~New"   , NIL , 0, ;
                    XBPMENUBAR_MIA_DISABLED} )
   oMenu:addItem( { "~Save"  , NIL , 0, ;
                    XBPMENUBAR_MIA_DISABLED} )
   oMenu:addItem( { "~Undo"  , NIL , 0, ;
                    XBPMENUBAR_MIA_DISABLED} )
   oMenu:addItem( { "~Delete", NIL , 0, ;
                    XBPMENUBAR_MIA_DISABLED } )

   oMenu:itemSelected := ;
      {|nItem,mp2,obj| MenuSelect(obj, 200+nItem) } 
   
   oMenuBar:addItem( {oMenu, NIL} )

   // Third sub-menu -> Help menu
   //
   HelpMenu( oMenuBar )

RETURN



/*
 * Create sub-menu in a menu
 */
FUNCTION SubMenuNew( oMenu, cTitle )
   LOCAL oSubMenu := XbpMenu():new( oMenu )
   oSubMenu:title := cTitle
RETURN oSubMenu:create()



/*
 * Create standard help menu
 */
PROCEDURE HelpMenu( oMenuBar )
   LOCAL oMenu := SubMenuNew( oMenuBar, "~Help" )

   oMenu:addItem( { "Help ~index", ;
                    {|| HelpObject():showHelpIndex() } } )

   oMenu:addItem( { "~General help", ;
                    {|| HelpObject():showGeneralHelp() } } )

#ifdef __OS2__
   oMenu:addItem( { "~Using help", ;
                    {|| HelpObject():showHelp(ID_HELP_HELP) } } )

   oMenu:addItem( { "~Keys help", ;
                    {|| HelpObject():showKeysHelp() } } )
#endif

   oMenu:addItem( MENUITEM_SEPARATOR )

   oMenu:addItem( { "~Product information", ;
                    {|| MsgBox("Xbase++ SDI Demo") } } )
   
   oMenuBar:addItem( {oMenu, NIL} )
RETURN



/*
 * Routine which branches after menu selection
 */
PROCEDURE MenuSelect( oMenu, nID )
   LOCAL oDlg := SetAppWindow()

   DO CASE
   CASE nID < 200                   // First sub-menu
      DO CASE
      CASE nID == 101               // Customer
         EnableXbp( oDlg, ID_DATA_MENU, {1,2,3,4} )
         EnableXbp( oDlg, ID_FILE_MENU, {4,5} )

         IF Alias() <> "CUSTOMER"
            DelChildList( oDlg )
         ENDIF

         SELECT Customer
         Customer( oDlg )

      CASE nID == 102               // Parts
         EnableXbp( oDlg, ID_DATA_MENU, {1,2,3,4} )
         EnableXbp( oDlg, ID_FILE_MENU, {4,5} )

         IF Alias() <> "PARTS"
            DelChildList( oDlg )
         ENDIF

         SELECT Parts
         Parts( oDlg )

      CASE nID == 104               // Close
         DisableXbp( oDlg, ID_DATA_MENU, {1,2,3,4} )
         DisableXbp( oDlg, ID_FILE_MENU, {4,5} )
         DelChildList( oDlg )

      CASE nID == 105               // Print
         MsgBox("Print data for: " + Alias() ) 

      CASE nID == 106               // Options
         MsgBox( "Setting options" ) 

      CASE nID == 108               // Exit
         AppQuit()                  // Terminate program

      ENDCASE

   CASE nID < 300                   // Second sub-menu

      DO CASE
      CASE nID == 201               // New
         DlgNew( oDlg )
      CASE nID == 202               // Save
         DlgSave( oDlg )
      CASE nID == 203               // Undo
         DlgUndo( oDlg )
      CASE nID == 204               // Delete
         DlgDelete( oDlg )
      ENDCASE

   ENDCASE
RETURN
