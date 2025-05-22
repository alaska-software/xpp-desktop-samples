//////////////////////////////////////////////////////////////////////
//
//  MENUDEMO.PRG
//
//  Copyright:
//      Alaska Software, (c) 1997-2025. All rights reserved.         
//  
//  Contents:
//      The example demonstrates how a menu including sub-menus is programmed.
//      A menu system in the window and a context menu is created.
//   
//      Two possibilities for branching program flow via a menu are shown.
//      The first one uses a dispatcher procedure while the other uses
//      code blocks executed by a menu object.
//      
//////////////////////////////////////////////////////////////////////


#include "Appevent.ch"
#include "Xbp.ch"


PROCEDURE Main
   LOCAL nEvent, mp1, mp2, oXbp

   SetAppWindow():useShortCuts := .T.
   SetColor( "N/W" )
   SetCancel( .F. )
   CLS

  /*
   * Since the application window is of the XbpCrt class, mouse
   * messages must be switched on for AppEvent()
   *
   * Also, we need to receive graphical xy-coordinates instead
   * of row/col coordinates
   */
   SetMouse(.T.)
   SetAppWindow():mouseMode := XBPCRT_MOUSEMODE_PM

   ? "Click with the right mouse button to activate the context menu"

   CreateMenuSystem( SetAppWindow():menuBar() )

   CreateContextMenu( SetAppWindow() )

   DO WHILE nEvent <> xbeP_Close
      nEvent := AppEvent( @mp1, @mp2, @oXbp )
      oXbp:handleEvent( nEvent, mp1, mp2 )
   ENDDO

RETURN



/*
 * This procedure creates a menu system for a window
 */
PROCEDURE CreateMenuSystem( oMenubar )
   LOCAL oMenu, oSubMenu

  /*
   * This menu uses a dispatching procedure for program control
   * One code block is used for the entire menu. The code block
   * receives the numeric index of the selected menu item
   */

   oMenu       := XbpMenu():new( oMenuBar )
   oMenu:title := "~File Menu" 
   oMenu:create()
   oMenu:itemSelected := {|nItem| MenuDispatcher( 100+nItem ) }

   oMenu:addItem( {"~New" , NIL} )
   oMenu:addItem( {"~Open", NIL} )
   oMenu:addItem( {"~Save", NIL} )
   oMenu:addItem( {NIL, NIL, XBPMENUBAR_MIS_SEPARATOR, 0} )
   oMenu:addItem( {"~Exit", NIL} )

   oMenubar:addItem( {oMenu, NIL} )

  /*
   * Now we use a code block for each menu item. 
   * A dispatching procedure is not required in this case.
   */
   //
   oMenu := XbpMenu():new( oMenuBar )
   oMenu:title := "~Cascaded Menu" 
   oMenu:create()

   oMenu:addItem( { "~Disable Submenu", ;
                    {|nItem,lCheck,obj| ;
                       lCheck := obj:isItemChecked(1), ;
                       obj:checkItem( 1, !lCheck )   , ;
                       IIf( lCheck, obj:enableItem(2), obj:disableItem(2) ) ;
                  } } )

  /*
   * Let's create a submenu. The title of the menu is displayed as menu item.
   */
   oSubMenu       := XbpMenu():new( oMenu )
   oSubMenu:title := "~Here is a SubMenu" 
   oSubMenu:create()

   oSubMenu:addItem( { "First"     , {|| QOut( "First item"  ) } } )
   oSubMenu:addItem( { "Second"    , {|| QOut( "Second item" ) } } )
   oSubMenu:addItem( { "Third item", {|| QOut( "Third item"  ) } } )
   oSubMenu:addItem( { "#4"        , {|| QOut( "Fourth item" ) } } )

  /*
   * The submenu is added as a menu item
   */
   oMenu:addItem( { oSubMenu, NIL } )

   oMenu:addItem( { "~Normal item", {|| QOut( "Normal item" ) } } )

   oMenubar:addItem( { oMenu, NIL } )

RETURN


PROCEDURE MenuDispatcher( nSelection )

   DO CASE
   CASE nSelection == 101
      QOut( "Selected: File New"  )

   CASE nSelection == 102
      QOut( "Selected: File Open" )

   CASE nSelection == 103
      QOut( "Selected: File Save" )

  /*
   * nSelection == 104 never occurs
   * The 4th item in the File menu is the separator line!
   */

   CASE nSelection == 105
      QUIT

   ENDCASE
RETURN



/*
 * This procedure creates a context menu for oParent which could be
 * any Xbase Part.
 * 
 * Note that oParent opens the context menu via the :rbDown code block.
 */
PROCEDURE CreateContextMenu( oParent )
   LOCAL oMenu

   oMenu := XbpMenu():new( oParent )
   oMenu:create()

   oMenu:title := "Popup"
   oMenu:addItem( { "~First"  , {|| QOut( "First item from context menu"  ) } } )
   oMenu:addItem( { "~Second" , {|| QOut( "Second item from context menu" ) } } )
   oMenu:addItem( { "~Third"  , {|| QOut( "Third item from context menu"  ) } } )
   oMenu:addItem( { "F~ourth" , {|| QOut( "Fourth item from context menu" ) } } )

   oParent:rbDown := {|aPos,x,obj| QOut( aPos ), oMenu:popup( obj, aPos ) }

RETURN
