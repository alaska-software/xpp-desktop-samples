//////////////////////////////////////////////////////////////////////
//
//  MDIMENU.PRG
//
//  Copyright:
//      Alaska Software, (c) 1997-2025. All rights reserved.         
//  
//  Contents:
//      Create the application menu for MDIDEMO
//   
//////////////////////////////////////////////////////////////////////


#include "Xbp.ch"
#include "Mdidemo.ch"



/*
 * Create menu system in the menubar of the dialog window
 */
PROCEDURE MenuCreate( oMenuBar )
   LOCAL oMenu

   // first submenu -> Document
   //
   oMenu := SubMenuNew( oMenuBar, "~Document" )

   oMenu:addItem( { "~Customer" , {|| Customer()  } } )
   oMenu:addItem( { "~Parts"    , {|| Parts()     } } )

   oMenu:addItem( MENUITEM_SEPARATOR )

   oMenu:addItem( { "Options", ;
                    {|| MsgBox("Set options") } } )

   oMenu:addItem( MENUITEM_SEPARATOR )

   oMenu:addItem( { "~Exit" , {|| AppQuit() } } )

   oMenuBar:addItem( {oMenu, NIL} )

   // second submenu -> Help menu
   //
   HelpMenu( oMenuBar )

RETURN



/*
 * Create submenu within a menu
 */
FUNCTION SubMenuNew( oMenu, cTitle )
   LOCAL oSubMenu := XbpMenu():new( oMenu )
   oSubMenu:title := cTitle
RETURN oSubMenu:create()



/*
 * Create default help menu
 */
PROCEDURE HelpMenu( oMenuBar )
   LOCAL oMenu := SubMenuNew( oMenuBar, "~Help" )

   oMenu:addItem( { "Help ~index", ;
                    {|| HelpObject():showHelpIndex() } } )

   oMenu:addItem( { "~General help", ;
                    {|| HelpObject():showGeneralHelp() } } )

   oMenu:addItem( MENUITEM_SEPARATOR )

   oMenu:addItem( { "~Product information", ;
                    {|| MsgBox("Xbase++ MDI Demo"+Chr(13)+"Parts table images, courtesy of www.althans.de") } } )

   oMenuBar:addItem( {oMenu, NIL} )
RETURN



/*
 * Create menu to manage open windows
 */
FUNCTION WinMenu()
   STATIC soMenu

   IF soMenu == NIL
      soMenu := WindowMenu():new( GetApplication():MainForm:MenuBar() ):create()
   ENDIF
RETURN soMenu



/*
 * Menu class to manage open windows
 */
CLASS WindowMenu FROM XbpMenu
   PROTECTED:
     CLASS VAR windowStack, winCount, offset
   EXPORTED:
     CLASS METHOD initClass, setWindowFocus, closeWindows, cascadeWindows
     CLASS VAR currentWin READONLY
     VAR   editMenu
     METHOD init, create, addItem, delItem, setItem, checkItem
ENDCLASS



/*
 * Initialize member variables of the class object (class variables)
 */
CLASS METHOD WindowMenu:initClass
   ::windowStack := {}
   ::winCount    := 0
   ::offset      := 0
RETURN self



/*
 * Set focus to a window by ordinal position in menu
 */
CLASS METHOD WindowMenu:setWindowFocus( nItem )
   LOCAL oDlg := ::windowStack[ nItem - ::offset ]

   IF oDlg:getFrameState() == XBPDLG_FRAMESTAT_MINIMIZED
      oDlg:setFrameState( XBPDLG_FRAMESTAT_NORMALIZED )
   ENDIF

   SetAppFocus( oDlg )
RETURN self



/*
 * Cascade windows
 */
CLASS METHOD WindowMenu:cascadeWindows
   LOCAL i, aDlgSize, aDaSize, nX, nY, nDY

   ::windowStack[1]:setFrameState( XBPDLG_FRAMESTAT_NORMALIZED )
   aDlgSize := ::windowStack[1]:currentSize()
   aDaSize  := ::windowStack[1]:drawingArea:currentSize()
   nDY      := aDlgSize[2] - aDAsize[2]
   nX       := -1
   nY       := -1

   FOR i:=1 TO ::winCount
      aDlgSize := ::windowStack[i]:currentSize()
      IF i == 1 .OR. nY < 0
         nX := 0
         nY := SetAppWindow():drawingArea:currentSize()[2] - aDlgSize[2]
      ELSE
         ::windowStack[i]:setFrameState( XBPDLG_FRAMESTAT_NORMALIZED )
         aDlgSize := ::windowStack[i]:currentSize()
         nX       += nDY
         nY       := ::windowStack[i-1]:currentPos() [2] + ;
                     ::windowStack[i-1]:currentSize()[2] - ;
                     aDlgSize[2] - nDY
      ENDIF
      ::windowStack[i]:setPos( {nX, nY} )
      ::windowStack[i]:toFront()
   NEXT
   SetAppFocus( ATail( ::windowStack ) )
RETURN self



/*
 * Close all child windows
 */
CLASS METHOD WindowMenu:closeWindows
   DO WHILE ! Empty( ::windowStack )
      ATail(::windowStack):destroy()
   ENDDO
RETURN self



/*
 * Initialize object and add a submenu for basic database operations
 */
METHOD WindowMenu:init( oParent, aPresParam, lVisible )
   ::xbpMenu:init( oParent, aPresParam, lVisible )
   ::title    := "~Window"
   ::editMenu := DataDialogMenu():new( oParent, aPresParam, lVisible )
RETURN self



/*
 * Request system resources and add default items
 */
METHOD WindowMenu:create( oParent, aPresParam, lVisible )
   ::xbpMenu:create( oParent, aPresParam, lVisible )

   ::itemSelected := {|nItem| ::setWindowFocus( nItem ) }
   ::editMenu:create( oParent, aPresParam, lVisible )

   ::xbpMenu:addItem( { "~Cascade"  , {|| ::cascadeWindows() } } )
   ::xbpMenu:addItem( { "Close ~All", {|| ::closeWindows()   } } )
   ::xbpMenu:addItem( MENUITEM_SEPARATOR )
   ::offset := 3

RETURN self



/*
 * Use title of a dialog window as text for menu item
 */
METHOD WindowMenu:addItem( oDlg )
   LOCAL cItem := oDlg:getTitle()

   AAdd( ::windowStack, oDlg )
   ::winCount ++
   cItem := "~" + Ltrim( Str( ::winCount ) ) + Chr(9) + cItem
   ::xbpMenu:addItem( { cItem, NIL} )
   IF ::numItems() == ::offset + 1
      ::setParent():insItem( ::setParent():numItems() , {self, NIL} )
      ::setParent():insItem( 2, { ::editMenu, NIL} )
   ENDIF
RETURN self



/*
 * Transfer changed window title to menu item
 */
METHOD WindowMenu:setItem( oDlg )
   LOCAL aItem, i := AScan( ::windowStack, oDlg )

   IF i == 0
      ::addItem( oDlg )
   ELSE
      aItem    := ::xbpMenu:getItem( i + ::offset )
      aItem[1] := "~" + Ltrim( Str(i) ) + Chr(9) + oDlg:getTitle()
      ::xbpMenu:setItem( i + ::offset, aItem )
      IF oDlg == ::currentWin
         ::checkItem( oDlg )
      ENDIF
   ENDIF

RETURN self



/*
 * Delete dialog window from window stack and from menu
 */
METHOD WindowMenu:delItem( oDlg )
   LOCAL i    := AScan( ::windowStack, oDlg )
   LOCAL nPos := ::setParent():numItems()-1

   IF i > 0
      ::xbpMenu:delItem( i + ::offset )

      IF ::currentWin ==  ::windowStack[i]
         ::currentWin := NIL
      ENDIF

      ADel( ::windowStack, i )
      ::winCount --
      Asize( ::windowStack, ::winCount )

      IF i <= ::winCount
         AEval( ::windowStack, {|o| ::setItem( o ) }, i )
      ENDIF

      IF ::numItems() == ::offset
         ::setParent():delItem( nPos )
         ::setParent():delItem( 2 )
      ELSEIF ::currentWin == NIL
         SetAppFocus( Atail( ::windowStack ) )
      ENDIF
   ENDIF
RETURN self



/*
 * Check menu item for current window
 */
METHOD WindowMenu:checkItem( oDlg )
   LOCAL i := AScan( ::windowStack, oDlg ) + ::offset

   IF ::currentWin <> NIL
      ::xbpMenu:checkItem( AScan(::windowStack, ::currentWin ) + ::offset , .F. )
   ENDIF

   ::currentWin := oDlg
   ::xbpMenu:checkItem( i, .T. )
RETURN self
