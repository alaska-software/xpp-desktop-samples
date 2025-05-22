//////////////////////////////////////////////////////////////////////
//
//  MDIBASE.PRG
//
//  Copyright:
//      Alaska Software, (c) 1997-2025. All rights reserved.         
//  
//  Contents:
//      Class WindowMenu() for managing child windows in MDI applications
//      from the menubar of the application window.
//   
//  Remarks:
//      The program code used to test the WindowMenu() class shows various
//      relationships between windows.
//   
//////////////////////////////////////////////////////////////////////



#include "Appevent.ch"
#include "Common.ch"
#include "Gra.ch"
#include "Xbp.ch"

#define  MENUITEM_SEPARATOR   {NIL, NIL, XBPMENUBAR_MIS_SEPARATOR, 0}


#ifdef   DEBUG


/*
 * XbpDialog window must be used as application window
 */
PROCEDURE AppSys
   LOCAL oDlg, aPos[2], aSize

   aSize    := SetAppWindow():currentSize()
   aPos[1]  := 0.1 * aSize[1]
   aPos[2]  := 0.1 * aSize[2]
   aSize[1] *= 0.8
   aSize[2] *= 0.8

   oDlg       := DialogWindow( AppDesktop(), , aPos, aSize, "Testing the WindowMenu class" )
   oDlg:DrawingArea:SetColorBG( XBPSYSCLR_APPWORKSPACE )
   oDlg:close := {|| AppQuit() }
RETURN



/*
 * Terminate application
 */
PROCEDURE AppQuit
   QUIT
RETURN



/*
 * The Main procedure provides merely an event loop
 */
PROCEDURE Main
   LOCAL nEvent, mp1, mp2, oXbp

   DO WHILE .T.
      nEvent := AppEvent( @mp1, @mp2, @oXbp )
      oXbp:handleEvent( nEvent, mp1, mp2 )
   ENDDO
RETURN



/*
 * Helper function to get different window titles
 */
FUNCTION Id
   STATIC snAsc := 64

   snAsc++
   IF snAsc  > 90
      snAsc := 65
   ENDIF

RETURN Chr( snAsc )



/*
 * Child windows are created by using :drawingArea as parent window
 */
PROCEDURE CreateChildWindow( oDlg )
   LOCAL oWin, oWinMenu, oParent, oOwner, aPos[2], aSize, cChild

   oParent    := ;
   oOwner     := oDlg:drawingArea

  /*
   * Get the WindowMenu object 
   */
   oWinMenu   := oDlg:childFromName( WindowMenu():ID )

   aSize      := oDlg:currentSize()
   aSize[1]   *= 0.6
   aSize[2]   *= 0.4

  /*
   * Let WindowMenu calculate the initial position for the child window
   */
   aPos       := oWinMenu:nextWinPos( aSize )
   
  /*
   * Create the child window and tell it how to close itself
   */
   oWin       := DialogWindow( oParent, oOwner, aPos, aSize, "Child window ["+ Id() +"]" )
   oWin:close := {|mp1,mp2,obj| CloseChildWindow( obj ) }

RETURN



/*
 * Owned windows have the same parent as the owner window
 */
PROCEDURE CreateOwnedWindow( oDlg )
   LOCAL oWin, oParent, oOwner, aPos[2], aSize

   oParent    := oDlg:setParent()
   oOwner     := oDlg
  /*
   * Just for demonstration: reduce width of owner window to 50%
   * and display owned window at the right edge of the owner
   */
   aSize      := oOwner:currentSize()
   aSize[1]   *= 0.5
   aPos[1]    := oOwner:currentPos()[1] + aSize[1] + 2
   aPos[2]    := oOwner:currentPos()[2]

   oOwner:setSize( aSize )

  /*
   * Create the owned window and tell it how to close itself
   */
   oWin       := DialogWindow( oParent, oOwner, aPos, aSize, "Owner is ["+ oOwner:getTitle() +"]" )
   oWin:close := {|mp1,mp2,obj| CloseOwnedWindow( obj ) }
   oWin:DrawingArea:SetColorBG( XBPSYSCLR_APPWORKSPACE )

RETURN



/*
 * Free windows have the desktop window as parent
 */
PROCEDURE CreateFreeWindow( oDlg )
   LOCAL oWin, oParent, oOwner, aPos[2], aSize

   oParent    := ;
   oOwner     := AppDesktop()

   aSize      := SetAppWindow():currentSize()
   aPos       := SetAppWindow():currentPos ()
   aPos[1]    += 24
   aPos[2]    -= 24
   
   oWin       := DialogWindow( oParent, oOwner, aPos, aSize, "Free window ["+ Id() +"]" )
   oWin:close := {|mp1,mp2,obj| CloseWindow( obj ) }
   oWin:DrawingArea:SetColorBG( XBPSYSCLR_APPWORKSPACE )
RETURN


/*
 * Modal windows are created by disabling the owner window. Therefore, parent
 * and owner must be different. If parent == owner, the window becomes a child
 * and is disabled together with its parent -> dead lock situation!!!
 */
PROCEDURE CreateModalWindow( oDlg )
   LOCAL oWin, oParent, oOwner, aPos, aSize
   LOCAL aDlgSize, aDaSize, nX, nY, nDY, nDX

   oParent    := oDlg:setParent()
   oOwner     := oDlg

   aSize      := oOwner:currentSize()
   aPos       := oOwner:currentPos ()
   aPos[1]    += 24
   aPos[2]    -= 24
   
  /*
   * Disable owner window and corresponding menu item in WindowMenu
   */
   WinMenuDisable( oOwner )

   oWin       := DialogWindow( oParent, oOwner, aPos, aSize, "Modal for ["+ oOwner:getTitle() +"]", .T. )
   oWin:close := {|mp1,mp2,obj| CloseModalWindow( obj ) }
RETURN



/*
 * Close a child window
 */
PROCEDURE CloseChildWindow( oDlg )
   WinMenuDel ( oDlg )
   CloseWindow( oDlg )
RETURN



/*
 * Close an owned window
 */
PROCEDURE CloseOwnedWindow( oDlg )
   LOCAL oOwner := oDlg:setOwner()
   LOCAL aSize  := oOwner:currentSize()

  /*
   * Just for demonstration: resize owner window by the width of owned window
   */
   aSize[1] += oDlg:currentSize()[1]

   WinMenuDel ( oDlg )
   CloseWindow( oDlg )

   oOwner:setSize( aSize )
   SetAppFocus( oOwner )
RETURN



/*
 * When a modal window is closed, the owner must be enabled again
 */
PROCEDURE CloseModalWindow( oDlg )
   LOCAL oOwner := oDlg:setOwner()

   WinMenuDel ( oDlg )
   CloseWindow( oDlg )

   WinMenuEnable( oOwner )

   SetAppFocus( oOwner )
RETURN


/*
 * Close a window
 */
PROCEDURE CloseWindow( oDlg )
   LOCAL oOwner := oDlg:setOwner()
   LOCAL i

  /*
   * :cargo is used in this program to manage a list of owned windows
   */
   IF Valtype( oOwner:cargo ) == "A" .AND. ;
      (i := AScan( oOwner:cargo, oDlg ) ) > 0
      ADel ( oOwner:cargo, i )
      ASize( oOwner:cargo, Len( oOwner:cargo ) - 1 )
   ENDIF

   oDlg:hide()
   oDlg:destroy()

   IF Valtype( oDlg:cargo ) == "A"
      AEval( oDlg:cargo, {|oSlave| IIf( oSlave:moveWithOwner, ;
                                        WinMenuDel( oSlave ), NIL ) } )
   ENDIF
RETURN



/*
 * The function creates all kinds of dialog windows used in this program
 */
FUNCTION DialogWindow( oParent, oOwner, aPos, aSize, cTitle, lModal )
   LOCAL oDlg, oXbp

   DEFAULT oOwner  TO oParent, ;
           cTitle  TO " "    , ;
           lModal  TO .F.
 
   oDlg               := XbpDialog():new( oParent, oOwner, aPos, aSize,, .F. )
   oDlg:title         := cTitle
   oDlg:taskList      := ( oParent == AppDeskTop() )
   oDlg:moveWithOwner := ( oOwner <> oParent .AND. .NOT. lModal )
   oDlg:clipSiblings  := .T.
   oDlg:cargo         := {}
   oDlg:create()
   oDlg:drawingArea:setFontCompoundName( "8.Helv" )

   IF oParent == AppDesktop()
      DialogMenu( oDlg )
   ELSE
      WinMenuAdd( oDlg )
      oXbp := XbpPushButton():new( oDlg:drawingArea, , {10,50}, {80,30} )
      oXbp:caption := "Free"
      oXbp:create()
      oXbp:activate := {|| CreateFreeWindow( oDlg ) }

      oXbp := XbpPushButton():new( oDlg:drawingArea, , {10,10}, {80,30} )
      oXbp:caption := "Modal"
      oXbp:create()
      oXbp:activate := {|| CreateModalWindow( oDlg ) }

   ENDIF

   IF Valtype( oOwner:cargo ) == "A"
      AAdd( oOwner:cargo, oDlg )
   ENDIF

   oDlg:show()
   SetAppFocus( oDlg )

RETURN oDlg



/*
 * The function creates the menu system for an MDI window and adds
 * an instance of the WindowMenu() class to the menubar. There is only one
 * WindowMenu object in an MDI window.
 */
PROCEDURE DialogMenu( oDlg )
   LOCAL oMenuBar := oDlg:menuBar()
   LOCAL oMenu    := XbpMenu():new( oMenuBar )

  /*
   * When the MDI window gets focus, it passes itself to SetAppWindow()
   * This is a prerequisite for the WinMenu() function below.
   */
   oDlg:setDisplayFocus := {|mp1,mp2,obj| SetAppWindow( obj ) }

   oMenu:title := "~Open"
   oMenu:create()

   oMenu:addItem( { "~Child window", {|| CreateChildWindow( oDlg ) } } )
   oMenu:addItem( { "~Owned window", {|| CreateOwnedWindow( oDlg ) } } )
   oMenu:addItem( { "~Free window" , {|| CreateFreeWindow ( oDlg ) } } )
   oMenu:addItem( { "~Modal window", {|| CreateModalWindow( oDlg ) } } )

   oMenu:addItem( MENUITEM_SEPARATOR )

   oMenu:addItem( { "~Exit", {|| PostAppEvent( xbeP_Close,,, oDlg ) } } )

   oMenuBar:addItem( { oMenu, NIL } )

  /*
   * Create an instance of the WindowMenu class for managing child windows
   */
   oMenu := WindowMenu():new( oMenuBar )

  /*
   * Tell the menu where to display in the menubar
   */
   oMenu:menuPos := 2

   oMenu:create()
RETURN



/******************************************************************************
 * The code above is the test scenario for the WindowMenu class.
 *
 * The code below is ready to run elsewhere.
 * However, the application window must be an XbpDialog window!
 *
 ******************************************************************************/

#endif  DEBUG





/*
 * Wrapper function to get the WindowMenu object without a reference stored
 * in a variable.
 */
FUNCTION WinMenu
RETURN SetAppWindow():childFromName( WindowMenu():ID )



/*
 * When a XbpDialog window is created as child, this procedure adds the
 * window title to the window menu
 */
PROCEDURE WinMenuAdd( oDlg )
   LOCAL oWinMenu := WinMenu()

   IF oWinMenu <> NIL
      oDlg:setDisplayFocus := {|mp1,mp2,obj| oWinMenu:checkItem( obj ) }
      oWinMenu:addItem( oDlg )
   ENDIF
RETURN



/*
 * When a child window is closed, this procedure removes its title from
 * the window menu
 */
PROCEDURE WinMenuDel( oDlg )
   LOCAL oWinMenu := WinMenu()

   IF oWinMenu <> NIL
      oDlg:setDisplayFocus := NIL
      oWinMenu:delItem( oDlg )
   ENDIF
RETURN



/*
 * Disable a window and its corresponding menu item.
 */
PROCEDURE WinMenuDisable( oDlg )
   LOCAL oWinMenu := WinMenu()

   IF oWinMenu <> NIL
      oWinMenu:disableItem( oDlg )
   ENDIF
RETURN



/*
 * Enables a dialog window together with the corresponding menu item
 */
PROCEDURE WinMenuEnable( oDlg )
   LOCAL oWinMenu := WinMenu()

   IF oWinMenu <> NIL
      oWinMenu:enableItem( oDlg )
   ENDIF
RETURN



/*
 * User-defined Menu class to manage child windows in an MDI application
 */

CLASS WindowMenu FROM XbpMenu
   PROTECTED:
     VAR windowStack, winCount

   EXPORTED:
     CLASS METHOD initClass

     CLASS VAR  Id  READONLY

     VAR     menuPos
     VAR     currentWin  , offset  READONLY
     METHOD  init, create, addItem    , insItem , setItem, delItem
     METHOD  setChildWin , closeWin   , closeAll, cascade, nextWinPos
     METHOD  enableItem  , disableItem, checkItem
ENDCLASS


/*
 * Initialize class object
 *
 * The :Id is used to retrieve a WindowMenu object from an MDI window
 * using the :childFromName() method -> see function WinMenu() above
 */
CLASS METHOD WindowMenu:initClass

   ::id := 123456789

RETURN self



/*
 * Initialize object
 */
METHOD WindowMenu:init( oParent, aPresParam, lVisible )

   ::xbpMenu:init( oParent, aPresParam, lVisible )

   ::windowStack := {}
   ::winCount    := 0
   ::menuPos     := 0
   ::offset      := 0
   ::title       := "~Windows"

RETURN self


/*
 * Request system resources and add default menu items
 */
METHOD WindowMenu:create( oParent, aPresParam, lVisible )
   ::xbpMenu:create( oParent, aPresParam, lVisible )

   ::xbpMenu:addItem( { "~Cascade"  , {|| ::cascade()  } } )
   ::xbpMenu:addItem( { "C~lose"    , {|| ::closeWin() } } )
   ::xbpMenu:addItem( { "Close ~All", {|| ::closeAll() } } )
   ::xbpMenu:addItem( MENUITEM_SEPARATOR )

   ::xbpMenu:setName( ::Id )

   ::offset       := ::xbpMenu:numItems()
   ::itemSelected := {|nItem| ::setChildWin( nItem - ::offset ) }

RETURN self



/*
 * Calculate position for next window to open from position of 
 * ::currentWin and size of new window
 */
METHOD WindowMenu:nextWinPos( aSize )
   LOCAL oDialog := ::setParent():setParent()
   LOCAL aDaSize := oDialog:drawingArea:currentSize()
   LOCAL aPos, aPosTmp
   LOCAL i

   IF ::numItems() == ::offset 
     /*
      * No window is open 
      */
      aPos := { 0,  aDaSize[2] - aSize[2] }
   ELSEIF ::currentWin:getFrameState() == XBPDLG_FRAMESTAT_MINIMIZED
     /*
      * The current window is currently minimized; try to find
      * another window for computing the window position
      */
      aPos := { 0,  aDaSize[2] }
   
      FOR i:= 1 TO Len(::windowStack)
         IF ::windowStack[i]:getFrameState() != XBPDLG_FRAMESTAT_MINIMIZED
            aPosTmp   := ::windowStack[i]:currentPos()
            aPosTmp[2]+= ::windowStack[i]:currentSize()[2]

            IF aPosTmp[1] > aPos[1] .OR. aPosTmp[2] < aPos[2]
               aPos := aPosTmp
            ENDIF
         ENDIF
      NEXT

      aPos := { aPos[1] + 24, aPos[2] - aSize[2] - 24 }
   ELSE
      aPos := ::currentWin:currentPos()
      aPos := { aPos[1] + 24, ;
                aPos[2] + ::currentWin:currentSize()[2] - aSize[2] - 24 }
   ENDIF

RETURN aPos



/*
 * Use title of child window as text for menu item
 */
METHOD WindowMenu:addItem( oDlg )
   LOCAL oOwner := oDlg:setOwner()
   LOCAL oParent:= oDlg:setParent()
   LOCAL i, cItem 

   IF oOwner <> oParent  .AND. ;
      ( i := AScan( ::windowStack, oOwner ) ) < Len( ::windowStack )

     /*
      * Owned window must be adjacent to owner in the menu
      */
      ::insItem( i+1, oDlg )
   ELSE
      AAdd( ::windowStack, oDlg )
      ::winCount ++

      cItem := "~" + Ltrim( Str( ::winCount ) ) + " " + oDlg:getTitle()
      ::xbpMenu:addItem( { cItem, NIL} )

      IF ::numItems() == ::offset + 1

        /*
         * The first window managed by the WindowMenu opens
         * Display WindowMenu in menubar
         */
         IF ::setParent():numItems() < ::menuPos
            ::setParent():addItem( {self, NIL} )
            ::menuPos := ::setParent():numItems()
         ELSE
            ::setParent():insItem( ::menuPos, {self, NIL} )
         ENDIF

      ENDIF

      ::setChildWin( ::winCount )
   ENDIF
RETURN self



/*
 * Insert a window to window stack and menu
 */
METHOD WindowMenu:insItem( nStackPos, oDlg )
   LOCAL cItem  := oDlg:getTitle()

   AAdd( ::windowStack, NIL )
   AIns( ::windowStack, nStackPos )

   ::windowStack[ nStackPos ] := oDlg
   ::winCount ++

   cItem := "~" + Ltrim( Str( nStackPos ) ) + " " + cItem
   ::xbpMenu:insItem( nStackPos + ::offset, {cItem, NIL} )
   AEval( ::windowStack, {|oWin| ::setItem( oWin ) }, nStackPos + 1 )

   ::setChildWin( nStackPos )

RETURN self



/*
 * Disable child window and menu item
 */
METHOD WindowMenu:disableItem( oDlg )
   LOCAL i := AScan( ::windowStack, oDlg )

   IF i > 0
      ::xbpMenu:disableItem( i + ::offset )
   ENDIF
   oDlg:disable()

RETURN self



/*
 * Enable child window and menu item
 */
METHOD WindowMenu:enableItem( oDlg )
   LOCAL i := AScan( ::windowStack, oDlg )

   IF i > 0
      ::xbpMenu:enableItem( i + ::offset )
   ENDIF
   oDlg:enable()

RETURN self



/*
 * Transfer changed window title to menu item
 */
METHOD WindowMenu:setItem( oDlg )
   LOCAL aItem, i := AScan( ::windowStack, oDlg )

   IF i == 0
      ::addItem( oDlg )
   ELSE
      aItem      := ::xbpMenu:getItem( i + ::offset )
      aItem[1]   := "~" + Ltrim( Str(i) ) + " " + oDlg:getTitle()
      ::xbpMenu:setItem( i + ::offset, aItem )

     /*
      * Disabled and Checked status is lost for menu item. Reset!
      */
      IF .NOT. oDlg:isEnabled()
         ::xbpMenu:disableItem( i + ::offset )
      ENDIF

      IF oDlg == ::currentWin
         ::checkItem( oDlg )
      ENDIF
   ENDIF

RETURN self



/*
 * Delete child window from window stack and from menu
 */
METHOD WindowMenu:delItem( oDlg )
   LOCAL i    := AScan( ::windowStack, oDlg )

   IF i > 0 .AND. ::status() == XBP_STAT_CREATE
      ::xbpMenu:delItem( i + ::offset )

      IF ::currentWin == ::windowStack[i]
         ::currentWin := NIL
      ENDIF

      ADel( ::windowStack, i )
      ::winCount --
      Asize( ::windowStack, ::winCount )

      IF i <= ::winCount
        /*
         * re-number menu items below the deleted item
         */
         AEval( ::windowStack, {|o| ::setItem( o ) }, i )
      ENDIF

      IF ::winCount == 0
        /*
         * The last child window is closed.
         * Remove Window Menu from menubar
         */
         ::currentWin := NIL
         ::setParent():delItem( ::menuPos )

      ELSEIF ::currentWin == NIL

         ::setChildWin( Min( i, ::winCount ) )
      ENDIF
   ENDIF

RETURN self



/*
 * Check the corresponding menu item of a child window when it gets focus.
 * This method must be called from the :setDisplayFocus callback of
 * a child window -> see procedure WinMenuAdd() above
 */
METHOD WindowMenu:checkItem( oDlg )
   LOCAL i

   IF ::currentWin <> oDlg .AND. ::status() == XBP_STAT_CREATE
      IF ::currentWin <> NIL
         i := AScan( ::windowStack, ::currentWin ) + ::offset
         ::xbpMenu:checkItem( i , .F. )
      ENDIF

      i := AScan( ::windowStack, oDlg ) + ::offset
      ::xbpMenu:checkItem( i, .T. )
      ::currentWin := oDlg
   ENDIF

RETURN self



/*
 * Set focus to a child window by ordinal position of the window.
 * This method is called when a window is selected within the window menu
 */
METHOD WindowMenu:setChildWin( nStackPos )

   ::checkItem( ::windowStack[ nStackPos ]  )

   IF ::currentWin:getFrameState() == XBPDLG_FRAMESTAT_MINIMIZED
      ::currentWin:setFrameState( XBPDLG_FRAMESTAT_NORMALIZED )
   ENDIF

   SetAppFocus( ::currentWin )
RETURN self



/*
 * Close current child window
 *
 * Note: The closing routine must be called from the :close callback of a
 *       child window -> see procedure CreateChildWindow() above
 */
METHOD WindowMenu:closeWin

   IF ::currentWin <> NIL
      ::currentWin:handleEvent( xbeP_Close, NIL, NIL )
   ENDIF

RETURN self



/*
 * Close all child windows
 *
 * Note: The closing routine must be called from the :close callback of a
 *       child window -> see procedure CreateChildWindow() above.
 * 
 *       !!! Otherwise we have an endless loop here !!!
 */
METHOD WindowMenu:closeAll

   DO WHILE ! Empty( ::windowStack )
      ATail( ::windowStack ):handleEvent( xbeP_Close, NIL, NIL )
   ENDDO

RETURN self



/*
 * Cascade child windows in parent
 */
METHOD WindowMenu:cascade
   LOCAL i, aPos, aDlgSize, aDaSize, nX, nY, nDY, nDX
   LOCAL oDialog := ::setParent():setParent()

   ::windowStack[1]:setFrameState( XBPDLG_FRAMESTAT_NORMALIZED )
   aDlgSize := ::windowStack[1]:currentSize()
   aDaSize  := ::windowStack[1]:drawingArea:currentSize()
   nDX      := ( aDlgSize[1] - aDAsize[1] ) / 2
   nDY      := ( aDlgSize[2] - aDAsize[2] ) - nDX
   nY       := -1

   FOR i:=1 TO ::winCount
      ::windowStack[i]:setFrameState( XBPDLG_FRAMESTAT_NORMALIZED )
      aDlgSize := ::windowStack[i]:currentSize()

      DO CASE
      CASE i == 1 .OR. nY < 0 
         nX := 0
         nY := oDialog:drawingArea:currentSize()[2] - aDlgSize[2]

      CASE ::windowStack[i]:moveWithOwner

        /*
         * Just one way to do it: position attached owned windows
         * at the right edge of the owner window
         */
         aPos    := ::windowStack[i]:setOwner():currentPos()
         aPos[1] += ::windowStack[i]:setOwner():currentSize()[1] + 2
         aPos[2] += ::windowStack[i]:setOwner():currentSize()[2] - aDlgSize[2]
         ::windowStack[i]:setPos( aPos )
         ::windowStack[i]:toFront()

         LOOP

      OTHERWISE
         nX += nDY
         nY := ::windowStack[i-1]:currentPos() [2] + ;
               ::windowStack[i-1]:currentSize()[2] - ;
               aDlgSize[2] - nDY
      ENDCASE

      ::windowStack[i]:setPos( {nX, nY} )
      ::windowStack[i]:toFront()
   NEXT

  /*
   * Set focus to window in front
   */
   ::setChildWin( ::winCount )

RETURN self
