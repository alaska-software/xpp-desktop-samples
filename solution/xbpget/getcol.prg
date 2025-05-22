//////////////////////////////////////////////////////////////////////
//
//  GETCOL.PRG
//
//  Copyright:
//      Alaska Software, (c) 1999-2025. All rights reserved.         
//  
//  Contents:
//      This example implements the XbpGetColumn class which uses
//      XbpGet objects for editing of single cells in a XbpBrowse object.
//   
//  Remarks:
//     
//      The following programming techniques are demonstrated:
//     
//      1. How to embed an editable Xbase Part into a column object so that
//         a browser based on the XbpBrowse() class becomes editable.
//     
//      2. How to move the edit control upon browser request.
//     
//      3. How to use XbpGet objects as edit controls in a browser column.
//   
//      4. How to replace the XbpGetController class when only one XbpGet
//         object is active at any given point in time.
//   
//////////////////////////////////////////////////////////////////////


#pragma Library( "XppUI2.LIB" )

#include "Appevent.ch"
#include "Common.ch"


CLASS XbpGetColumn FROM XbpColumn
   PROTECTED:
   CLASS VAR activeColumn SHARED // The column that is currently being edited
   VAR    xbpGet                 // The associated XbpGet object
   METHOD createXbp              // Create it upon first edit request
   METHOD setPosAndSize          // Assign pos and size for editing

   EXPORTED:
   INLINE METHOD preValidate     // These methods are called by an
   RETURN .T.                    // XbpGet object during editing.
                                 // We don't do validation in a browser
   INLINE METHOD postValidate    // so these are dummy methods that
   RETURN .T.                    // must return a logical value.

   VAR setHome                   // Used by an XbpGet object

                                 ** Overloaded methods 
   METHOD init                   // Initialize object
   METHOD create                 // Request system resources
   METHOD destroy                // Release System resources
   METHOD resize                 // Resize must be reflected by XbpGet during edit
   METHOD hiliteRow              // Display the XbpGet during browsing

                                 ** This is required for editing
   VAR    active  READONLY       // Tells if we are in edit mode
   VAR    browser                // XbpBrowse object the column is contained in
   VAR    bufferLength           // Max. number of characters for editing

   METHOD _LbDown                // LbDown method for :dataArea object
   METHOD read                   // Begin editing
   METHOD keyBoard               // Called by an XbpGet object for UP / DOWN keys
   METHOD killRead               // End editing
ENDCLASS


/*
 * Initialize the object
 */
METHOD XbpGetColumn:init( oParent, oOwner, aPos, aSize, aPP, lVisible )
   ::xbpColumn:init( oParent, oOwner, aPos, aSize, aPP, lVisible )
   ::active  := .F.
   ::setHome := .T.

   IF oParent <> NIL .AND. oParent:isDerivedFrom( "XbpBrowse" )
      ::browser := oParent
   ENDIF
RETURN self


/*
 * Request system resources
 */
METHOD XbpGetColumn:create( oParent, oOwner, aPos, aSize, aPP, lVisible )
   LOCAL oError

   ::xbpColumn:create( oParent, oOwner, aPos, aSize, aPP, lVisible )

   /*
    * Set focus back to the browser if the column object gets input focus.
    * This assures that keyboard events are always processed by the browser.
    */
   ::dataArea:lbDown := {|aPos| ::_lbDown( aPos ) }
   ::setInputFocus   := {|x,y,obj| SetAppFocus( obj:browser ) }

   IF ::browser == NIL
      /*
       * Find the XbpBrowse object in the Parent-Child hierarchy
       *
       * NOTE: The DO WHILE loop demonstrates how to search the parent-child
       *       hierarchy upwards. The top of the Parent-Child tree is reached
       *       when the Desktop window is the parent (oParent == AppDesktop())
       */
      oParent := ::setParent()

      DO WHILE .NOT. oParent:isDerivedFrom( "XbpBrowse" )
         oParent := oParent:setParent()
         IF oParent == AppDesktop()
            oError             := Error():new()
            oError:description := "Illegal parent defined for " + ::className()
            oError:operation   := ProcName()
            oError:args        := { ::setParent() }
            Eval( ErrorBlock(), oError )
         ENDIF
      ENDDO

      /*
       * An XbpBrowse object is found in the Parent-Child hierarchy
       */
      ::browser := oParent
   ENDIF
RETURN self



/*
 * Clean up memory and set object references to NIL
 */
METHOD XbpGetColumn:destroy()
   IF ::xbpGet <> NIL
      ::xbpGet:controller := NIL
      ::xbpGet:destroy()
      ::xbpGet := NIL
   ENDIF
   ::active  := .F.
   ::browser := NIL
   ::XbpColumn:destroy()
RETURN self


/*
 * Create the XbpGet object and share the :dataLink code block 
 */
METHOD XbpGetColumn:createXbp
   ::xbpGet            := XbpGet():new( ::dataArea, ::dataArea, NIL, NIL, NIL, .F. )
   ::xbpGet:controller := self
   ::xbpGet:dataLink   := ::dataLink

   IF ::bufferLength <> NIL
      ::xbpGet:bufferLength := ::bufferLength
   ENDIF

   ::xbpGet:create()

   IF Empty(::Picture) == .F.
      ::xbpGet:Picture := ::Picture
   ENDIF

RETURN


/*
 * Determine the X/Y coordinates for the XbpGet display.
 */
METHOD XbpGetColumn:setPosAndSize( nRowPos )
   LOCAL aRect, aPos, aSize

   aRect := ::dataArea:cellRect( nRowPos )
   aPos  := { aRect[1], aRect[2] }
   aSize := { aRect[3]-aRect[1], aRect[4]-aRect[2] }
   ::xbpGet:setPos ( aPos  )
   ::xbpGet:setSize( aSize )
RETURN self


/*
 * When the column is resized while Edit mode is active,
 * the edit control must be adjusted to the column width.
 */
METHOD XbpGetColumn:resize( aOldSize, aNewSize )

   IF .NOT. ::active
      RETURN ::xbpColumn:resize( aOldSize, aNewSize )
   ENDIF

   ::xbpGet:hide()
   ::xbpColumn:resize( aOldSize, aNewSize )
   ::setPosAndSize( ::browser:rowPos )
   ::xbpGet:show()

   SetAppFocus( ::xbpGet )
RETURN self


/*
 * This method is called from the XbpBrowse object when the browse cursor
 * is moved. It must be overloaded for editable column objects and is
 * used to position the edit control at the browse cursor.
 */
METHOD XbpGetColumn:hiliteRow( nRowPos, lHilite, lFrame, lRepaint )
   IF ::active
      IF lHilite
         /*
          * We are in Edit mode and the browser wants to display its
          * cursor in highlight color. Resume with editing.
          */
         ::read( nRowPos )
      ELSE
         /*
          * We are in Edit mode and the browser wants to display its cursor
          * in normal color. Eval the :dataLink code block upon changes
          * and hide the XbpGet.
          */
         IF ::xbpGet <> NIL
            IF ::xbpGet:changed
               ::xbpGet:getData()
               ::xbpColumn:refreshRows( nRowPos, nRowPos )
            ENDIF
            ::xbpGet:hide()
         ENDIF
         ::xbpColumn:hiliteRow( nRowPos, lHilite, lFrame, lRepaint )
      ENDIF
   ELSE   
      /*
       * We are in non-edit mode. Pass everything on to the super class.
       */
      ::xbpColumn:hiliteRow( nRowPos, lHilite, lFrame, lRepaint )
   ENDIF
RETURN self


/*
 * Create and/or display the XbpGet object and edit the current row
 */
METHOD XbpGetColumn:read( nRowPos, nFirstKey )

   DEFAULT nRowPos TO ::browser:rowPos

   IF ::activeColumn <> NIL  .AND. ;
      ::activeColumn <> self
      /*
       * The browser is clicked with the mouse while another column
       * is currently being edited. Terminate edit in that column.
       */
      ::activeColumn:killRead()
   ENDIF

   ::activeColumn := self

   IF ::xbpGet == NIL
      /*
       * The column has not been edited yet. Create the XbpGet object,
       * upon first editing.
       */
      ::createXbp()
   ENDIF

   ::active  := .T.
   ::setHome := .T.
   ::setPosAndSize( nRowPos )

   ::xbpGet:setData()
   ::xbpGet:changed( .F. )
   ::xbpGet:show()

   SetAppFocus( ::xbpGet )
   IF ::xbpGet:isDerivedFrom( "XbpSLE" )
      ::xbpGet:setMarked( {1,1} )
   ENDIF

   IF nFirstKey <> NIL
      ::xbpGet:clear()
      PostAppEvent( xbeP_Keyboard, nFirstKey, NIL, ::xbpGet )
   ENDIF
RETURN self


/*
 * Left button is pressed in the :dataArea of the column
 */
METHOD XbpGetColumn:_lbDown( aPos )
   LOCAL nRowPos := ::dataArea:cellFromPos( aPos )

   ::active := .T.

   IF nRowPos == ::browser:rowPos
      ::read( nRowPos )
   ENDIF
RETURN self



/*
 * Keyboard handling method
 *
 * When edit mode is active, the XbpGet object has input focus and
 * calls this method since the XbpColumn object is the controller
 * of the XbpGet object
 * (see method :createXbp() -> ::xbpGet:controller := self)
 *
 * - The edit mode is activated by
 *   1) the ENTER key (cell content remains)
 *   2) an editing key (the cell is cleared)
 *
 * - Esc terminates editing
 *
 */
METHOD XbpGetColumn:keyBoard( nKey )
   LOCAL aRowPos, bBlock
   
   DO CASE
   CASE nKey == xbeK_ALT_F4
      PostAppEvent( xbeP_Close,,, SetAppWindow() )
      RETURN .T.

   CASE nKey == xbeK_ENTER
      /*
       * Edit next cell or activate edit mode.
       */
      IF ::active
         bBlock := {|o| o:down() }
      ELSE
         ::read( ::browser:rowPos )
      ENDIF      

   CASE .NOT. ::active
      /*
       * Keys processed by XbpGet activate edit mode and clear the
       * edit buffer. Other keys are ignored.
       */
      IF nKey > 31 .AND. nKey < 256
         ::read( ::browser:rowPos, nKey )
      ENDIF

   CASE nKey == xbeK_ESC
      /*
       * Undo changes upon ESC
       */
      ::xbpGet:undo()
      ::killRead()
      bBlock := {|o| o:refreshCurrent() }
      
   /*
    * Keys causing browse cursor movement during editing
    *
    * NOTE: Tab keys cause xbeK_ENTER to be posted to the browser so that
    *       the Edit mode is switched on in the next or previous column.
    */
   CASE nkey == xbeK_TAB
      IF ::browser:colPos == ::browser:colCount
         bBlock := {|o| o:firstCol(), o:down(), ;
                        PostAppEvent( xbeP_Keyboard, xbeK_ENTER,, o ) }
      ELSE
         bBlock := {|o| o:right(), ;
                        PostAppEvent( xbeP_Keyboard, xbeK_ENTER,, o ) }
      ENDIF

   CASE nkey == xbeK_SH_TAB
      IF ::browser:colPos == 1
         bBlock := {|o| o:up(), o:lastCol(), ;
                        PostAppEvent( xbeP_Keyboard, xbeK_ENTER,, o ) }
      ELSE
         bBlock := {|o| o:left(), ;
                        PostAppEvent( xbeP_Keyboard, xbeK_ENTER,, o ) }
      ENDIF

   CASE nkey == xbeK_DOWN
      bBlock := {|o| o:down() }

   CASE nKey == xbeK_UP
      bBlock := {|o| o:up() }

   CASE nkey == xbeK_PGDN
      bBlock := {|o| o:pageDown() }

   CASE nKey == xbeK_PGUP
      bBlock := {|o| o:pageUp() }

   CASE nkey == xbeK_CTRL_PGDN
      bBlock := {|o| o:goBottom() }

   CASE nKey == xbeK_CTRL_PGUP
      bBlock := {|o| o:goTop() }

   ENDCASE

   IF bBlock <> NIL
      /*
       * Invoke browser action
       */
      Eval( bBlock, ::browser )
      ::browser:forceStable()
   ENDIF

   /*
    * XbpGet needs a logical return value
    */
RETURN ( bBlock <> NIL )


/*
 * Terminate the Edit mode
 */
METHOD XbpGetColumn:killRead
   IF ::active
      ::xbpGet:hide()
      ::active       := .F.
      ::activeColumn := NIL
      SetAppFocus( ::browser )
   ENDIF
RETURN self


