//////////////////////////////////////////////////////////////////////
//
//  XBPGET.PRG
//
//  Copyright:
//      Alaska Software, (c) 1997-2025. All rights reserved.         
//  
//  Contents:
//      Sample implementation of a XbpGet class derived from XbpSle which uses
//      an overloaded textmode Get object as server for the picture and buffer
//      management.
//   
//  Remarks:
//      The mechanisms for pre- and postvalidation are implemented in the
//      XbpGetController class (XBPGETC.PRG). This assures correct validation
//      of single XbpGet objects in both modal and modeless windows.
//   
//////////////////////////////////////////////////////////////////////


#include "Appevent.ch"
#include "Common.ch"
#include "Gra.ch"
#include "Xbp.ch"
#include "Font.ch"


/*
 * InvisibleGet service class
 *
 */
CLASS InvisibleGet FROM Get
   EXPORTED:
   METHOD init
   METHOD display
ENDCLASS

METHOD InvisibleGet:Init( nRow, nCol, bVarBlock, cVarName, cPicture, ;
                          cColor, bValid, bWhen )
RETURN ::Sle:Init( bVarBlock, cVarName, cPicture, bValid, bWhen )

METHOD InvisibleGet:display()
RETURN self


/*
 * XbpGet class
 */
CLASS XbpGet FROM XbpSLE


   PROTECTED:
   CLASS VAR oContextMenu SHARED  // Popup menu for XbpGet objects
   CLASS METHOD enableMenuItems

   VAR    Get                     // The embedded invisible service GET
   VAR    original                // The original value before editing

   EXPORTED:
   CLASS METHOD initClass

   VAR    value                   // The current value during editing

                                  ** creation flags
   VAR    Picture                 // The picture mask
   VAR    preBlock                // Code blocks for pre- and
   VAR    postBlock               // postvalidation
   VAR    controller              // The XbpGetController object

                                  ** overloaded from XbpSle
   METHOD Init, Create            // Lifecycle
   METHOD Clear                   // Clear edit buffer
   METHOD Keyboard                // Overloaded keyboard handling
   METHOD LbUp                    // inquire mark
   METHOD RbDown                  // focus change or context menu display
   METHOD RbUp                    // Display context menu

   METHOD CutMarked               // Methods for Copy, Delete, Insert
   METHOD PasteMarked             // operations
   METHOD DeleteMarked            //
   METHOD itemSelected            // context menu selection

                                  ** Overloaded from DataRef
   METHOD SetData, GetData        // Overloaded SetData, GetData
   METHOD Undo                    // Overloaded Undo to Get

   METHOD SetEditBuffer           // Change edit buffer
   METHOD home                    // Set position in Get and XbpSle

                                  ** Focus change methods
   METHOD setInputFocus           // Input focus is obtained
   METHOD killInputFocus          // Input focus is lost

   METHOD preValidate             // Validation before input focus is obtained
   METHOD postValidate            // Validation before input focus is lost
   METHOD badDate                 // Overloaded Date validation

   ACCESS ASSIGN METHOD picture

ENDCLASS


/*
 * Create the context menu for XbpGet objects
 */
CLASS METHOD XbpGet:InitClass
   ::oContextMenu := XbpMenu():new( AppDesktop() ):create()

   ::oContextMenu:title := "XbpGet Popup"
   ::oContextMenu:addItem( { "~Validate"   ,  } )
   ::oContextMenu:addItem( { "~Undo"       ,  } )
   ::oContextMenu:addItem( {NIL, NIL , XBPMENUBAR_MIS_SEPARATOR, 0} )
   ::oContextMenu:addItem( { "Cu~t"        ,  } )
   ::oContextMenu:addItem( { "~Copy"       ,  } )
   ::oContextMenu:addItem( { "~Paste"      ,  } )
   ::oContextMenu:addItem( { "~Delete"     ,  } )
   ::oContextMenu:addItem( {NIL, NIL , XBPMENUBAR_MIS_SEPARATOR, 0} )
   ::oContextMenu:addItem( { "Select ~All" ,  } )

   #define MENU_ITEM_VALIDATE   1
   #define MENU_ITEM_UNDO       2
   #define MENU_ITEM_CUT        4
   #define MENU_ITEM_COPY       5
   #define MENU_ITEM_PASTE      6
   #define MENU_ITEM_DELETE     7
   #define MENU_ITEM_SELECTALL  9

RETURN self


/*
 * Enable all applicable menu items
 */
CLASS METHOD XbpGet:enableMenuItems( oXbpGet )
   LOCAL i, imax := ::oContextMenu:numItems()
   LOCAL xTemp

   FOR i:=1 TO imax
      ::oContextMenu:enableItem(i)
   NEXT

   IF oXbpGet:postBlock == NIL
      IF oXbpGet:Get:type <> "D"
         ::oContextMenu:disableItem( MENU_ITEM_VALIDATE )
      ENDIF
   ENDIF

   IF .NOT. oXbpGet:changed
      ::oContextMenu:disableItem( MENU_ITEM_UNDO )
   ENDIF

   xTemp := oXbpGet:queryMarked()
   IF xTemp[1] == xTemp[2]
      ::oContextMenu:disableItem( MENU_ITEM_CUT    )
      ::oContextMenu:disableItem( MENU_ITEM_COPY   )
      ::oContextMenu:disableItem( MENU_ITEM_DELETE )
   ENDIF

   IF Abs( xTemp[1] - xTemp[2] ) == Len( oXbpGet:editBuffer() )
      ::oContextMenu:disableItem( MENU_ITEM_SELECTALL )
   ENDIF

   xTemp := XbpClipboard():New():Create()
   xTemp:open()
   IF xTemp:getBuffer( XBPCLPBRD_TEXT ) == NIL
      ::oContextMenu:disableItem( MENU_ITEM_PASTE )
   ENDIF
   xTemp:close()
   xTemp:destroy()
RETURN self


/*
 * Initialize the object
 */
METHOD XbpGet:Init( oParent, oOwner, aPos, aSize, aPP, lVisible )

   ::XbpSle:init( oParent, oOwner, aPos, aSize, aPP, lVisible )
   ::tabStop := .T.

RETURN self


/*
 * Create method
 */
METHOD XbpGet:Create( oParent, oOwner, aPos, aSize, aPP, lVisible )
   LOCAL bBlock := {|x| IIf(x==NIL, ::value, ::value := x ) }

   /*
    * ::dataLink code block must be defined already
    */
   ::value    := ;
   ::original := Eval( ::dataLink )

   /*
    * now initialize the text mode get object as picture server
    */
   ::Get := InvisibleGet():New(,, bBlock, "", ::picture )

   ::Get:setFocus()
   ::BufferLength := Len( ::Get:buffer )
   ::Get:killFocus()
   ::Get:reset()

   /*
    * create the GUI SLE
    */
   ::XbpSle:Create( oParent, oOwner, aPos, aSize, aPP, lVisible )
RETURN self


/*
 * This method accepts data of type C,D,L,N and transfers a Picture
 * formatted string to the edit buffer of the XbpSLE.
 */
METHOD XbpGet:SetData( xData )
   LOCAL cBuffer

   IF PCount() == 1
      ::value := xData
   ELSE
      ::value := Eval( ::dataLink )
   ENDIF

   ::original := ::value
   ::changed  := .F.
   cBuffer    := Transform ( ::value, IIf(::Get:Picture==NIL, "", ::Get:picture) )

   /*
    * update the buffer of the XbpSLE
    */
   ::SetEditBuffer( cBuffer )

   /*
    * update the buffer of the picture server Get
    */
   IF ::Get:hasFocus
      ::Get:UpdateBuffer()

      /*
       * XbpGet has focus, set the initial mark
       */
      IF Set( _SET_INSERT )
         ::SetMarked ( { ::Get:Pos, ::Get:Pos } )
      ELSE
         ::SetMarked ( { ::Get:Pos, ::Get:Pos + 1 } )
      ENDIF
   ENDIF

RETURN self


/*
 * The method returns the edited value and passes
 * it to the :dataLink code block.
 */
METHOD XbpGet:GetData( lSetOriginal )
   DEFAULT lSetOriginal TO .T.

   IF ::Get:hasFocus
      ::Get:assign()
      ::Get:pos := ::queryMarked()[1]
   ENDIF

   ::value := Eval( ::DataLink, ::value )

   IF lSetOriginal
      ::original := ::value
      ::changed  := .F.
   ENDIF
RETURN ::value


/*
 * Undo changes made in the edit buffer.
 */
METHOD XbpGet:Undo()
   LOCAL cBuffer

   ::value   := ::original
   ::changed := .F.

   IF ::Get:hasFocus
      ::Get:Reset()
      cBuffer := ::Get:buffer
   ELSE
      cBuffer := Transform ( ::value, IIf(::Get:Picture==NIL, "", ::Get:picture) )
   ENDIF
RETURN ::SetEditBuffer( cBuffer )


/*
 * Change the edit buffer of the XbpSle.
 */
METHOD XbpGet:SetEditBuffer( cBuffer )
   ::XbpSle:SetData( cBuffer )
RETURN self


/*
 * Clears the edit buffer.
 */
METHOD XbpGet:clear
   ::get:clear := .T.
   ::xbpSle:clear()
RETURN .T.



/*
 * Move the caret to the first editable character in the edit buffer.
 */
METHOD XbpGet:home
   LOCAL aMarked, lClear := ::get:clear

   IF ::Get:hasFocus
      ::get:home()
   ELSE
      ::Get:pos := 1
   ENDIF

   ::get:clear := lClear
   ::XbpSle:setFirstChar( ::get:pos )

   IF Set( _SET_INSERT )
      aMarked := { ::Get:Pos, ::Get:Pos }
   ELSE
      aMarked := { ::Get:Pos, ::Get:Pos + 1 }
   ENDIF

   ::SetMarked( aMarked )
RETURN self



/*
 * XbpGet obtains input focus. A prevalidation is performed by the
 * Controller object. If the prevalidation fails, the Controller object
 * sets focus to the next XbpGet.
 */
METHOD XbpGet:setInputFocus

   IF ::controller:preValidate( self )
      /*
       * The Controller object checks wether or not self
       * may obtain input focus (it calls self:preValidate()).
       */
      ::get:setFocus()
      ::xbpSLE:setInputFocus()
      ::get:buffer := ::editBuffer()

      IF ::controller:setHome
         ::home()
         ::controller:setHome := .F.
      ELSE
         ::get:pos := ::QueryMarked()[1]
      ENDIF
   ELSEIF ! ::get:hasFocus
      ::get:setFocus()
   ENDIF

   ::changed := .NOT. ( ::original == ::value )
RETURN self


/*
 * XbpGet looses input focus. A postvalidation is performed by the
 * Controller object. If the postvalidation fails, the focus remains
 * with the XbpGet.
 */
METHOD XbpGet:killInputFocus

   IF ::controller:postValidate( self )
      /*
       * The Controller object checks wether or not self
       * may loose input focus (it calls self:postValidate()).
       */
      IF ::Get:hasFocus
         IF ::Get:Type == "N"
            ::Get:ToDecPos()
            ::SetEditBuffer( Trim(::Get:Buffer) )
         ENDIF
         ::Get:killFocus()
      ENDIF
      ::xbpSLE:killInputFocus()
   ELSE
      /*
       * Postvalidation failed. Mark all characters in the edit buffer.
       */
      ::home()
      ::setMarked( {1, Len(::editBuffer())+1 } )
   ENDIF

RETURN self


/*
 * Overloaded Keyboard handling. Marking is performed by the XbpSLE.
 * Cursor movement is taken from the invisible Get because it knows
 * how to navigate in a Picture formatted string.
 */
METHOD XbpGet:Keyboard( nKey )
   LOCAL aMarked, cChar
   LOCAL lSetPos := .T., lHandled := .T.

   aMarked := ::QueryMarked()

   DO CASE
   CASE .NOT. ::get:hasFocus
        /*
         * Delegate keyboard events to the Controller object if XbpGet
         * does not have focus (this can happen after a failed validation).
         */
        ::controller:keyboard( nKey )
        RETURN self

   CASE nKey == xbeK_SH_RIGHT
        aMarked[2] := Min( aMarked[2] + 1, ::Bufferlength + 1 )

        IF .NOT. Set( _SET_INSERT ) .AND. aMarked[1]==aMarked[2]
           aMarked[1] --
           aMarked[2] := Min( aMarked[2] + 1, ::Bufferlength + 1 )
        ENDIF

        ::XbpSle:setMarked( aMarked )

        IF aMarked[2] < aMarked[1]
           /*
            * The caret is on the left side of the mark
            */
           ::Get:pos := aMarked[2]
        ELSE
           ::Get:pos := aMarked[2] - 1
        ENDIF
        lSetPos := .F.

   CASE nKey == xbeK_SH_LEFT
        aMarked[2] := Max( aMarked[2] - 1, 1 )

        IF .NOT. Set( _SET_INSERT ) .AND. aMarked[1]==aMarked[2]
           aMarked[1] ++
           aMarked[2] := Max( aMarked[2] - 1, 1 )
        ENDIF

        ::XbpSle:setMarked( aMarked )

        IF aMarked[2] < aMarked[1]
           /*
            * The caret is on the left side of the mark
            */
           ::Get:pos := aMarked[2]
        ELSE
           ::Get:pos := aMarked[2] - 1
        ENDIF
        lSetPos   := .F.

   CASE nKey == xbeK_SH_END
        IF ::Get:Type == "N"
           ::Get:ToDecPos()
           ::Get:_End()
        ENDIF

        aMarked[2] := ::Bufferlength + 1
        ::Get:Pos  := ::Bufferlength
        lSetPos    := .F.
        ::setMarked( aMarked )

   CASE nKey == xbeK_SH_HOME
        IF aMarked[2]-aMarked[1] == 1
           aMarked[1] := aMarked[2]
        ENDIF
        ::home()
        aMarked[2] := ::Get:Pos
        lSetPos    := .F.
        ::SetMarked( aMarked )

   CASE nKey == xbeK_CTRL_U
        ::Undo()

   CASE nKey == xbeK_CTRL_LEFT
        /*
         * In overstrike mode, the caret is on the right side of the marked
         * character. Set the caret to the left side before the XbpSLE
         * handles the key.
         */
        ::setMarked( { ::Get:pos, ::Get:pos } )
        ::XbpSle:Keyboard( nKey )
        ::Get:pos := ::querymarked()[2]

   CASE nKey == xbeK_CTRL_RIGHT
        ::XbpSle:Keyboard( nKey )
        ::Get:pos := ::querymarked()[2]

   CASE nKey == xbeK_LEFT
        IF aMarked[2] > aMarked[1]
           ::Get:Pos := Min( ::get:pos, aMarked[1] )
        ENDIF
        ::Get:Left()

   CASE nKey == xbeK_RIGHT
        IF aMarked[1] > aMarked[2]
           ::Get:Pos := Max( ::get:pos, aMarked[1] - 1 )
        ENDIF
        ::Get:Right()

        IF ::get:typeOut .AND. ::get:type <> "L"
           ::get:Pos ++
        ENDIF

   CASE nKey == xbeK_HOME
        ::Home()

   CASE nKey == xbeK_END
        IF ::Get:Type == "N"
           ::Get:ToDecPos()
        ENDIF
        ::Get:_End()

        ::XbpSle:SetFirstChar( Len( ::Get:Buffer ) )
        IF Set( _SET_INSERT)
           /*
            * We're behind the last character
            */
           ::Get:Pos ++
        ENDIF

   CASE nKey == xbeK_INS
        Set( _SET_INSERT, ! Set( _SET_INSERT) )

   CASE nKey == xbeK_CTRL_INS
        ::CopyMarked()
        lSetPos := .F.

   CASE .NOT. ::editable
        /*
         * XbpGet is Read only. No other editing keys are processed.
         * Check if there is a navigation key pressed
         */
        lHandled := ::controller:keyboard( nKey )

   CASE nKey == xbeK_SH_INS
        ::PasteMarked()

   CASE nKey == xbeK_BS
        ::Get:BackSpace()

   CASE nKey == xbeK_DEL
        IF Set(_SET_INSERT) .AND. aMarked[1] == aMarked[2]
           aMarked[2] := aMarked[1] + 1
           ::SetMarked(aMarked)
        ENDIF
        ::DeleteMarked()

   CASE nKey == xbeK_SH_DEL
        ::CutMarked()

   CASE nKey == xbeK_CTRL_T
        ::Get:DelWordRight()

   CASE nKey == xbeK_CTRL_Y
        ::Get:DelEnd()

   CASE nKey == xbeK_CTRL_BS
        ::Get:DelWordLeft()

   CASE nKey == xbeK_CTRL_C
     ::copyMarked()

   CASE nKey == xbeK_CTRL_V
     ::pasteMarked()

   CASE nKey == xbeK_CTRL_X
     ::cutMarked()

   CASE nKey == xbeK_SH_CTRL_LEFT
     /*
      * XbpSle will handle key
      */
     lSetPos := .F.
     lHandled := .F.

   CASE nKey == xbeK_SH_CTRL_RIGHT
     /*
      * XbpSle will handle key
      */
     lSetPos := .F.
     lHandled := .F.

   CASE nKey >= 32 .AND. nKey <= 255
      cChar := Chr(nKey)

      IF ::Get:Type == "N" .AND. cChar $ ".,"
         ::Get:CondClear()
         ::Get:ToDecPos()
      ELSE

         IF Set(_SET_INSERT)
            IF Abs( aMarked[2]-aMarked[1]) > 0
               ::deleteMarked()
            ENDIF
            ::Get:Insert( cChar )
         ELSE
            IF Abs(aMarked[2]-aMarked[1]) > 1
               ::deleteMarked()
               ::Get:Insert( cChar )
            ELSE
               ::Get:Overstrike( cChar )
            ENDIF
         ENDIF

         IF ::Get:typeOut .AND. ::get:type <> "L"
            ::Get:pos ++
         ENDIF

      ENDIF

   OTHERWISE

     /*
      * Check if a key is pressed that navigates between XbpGets
      */
     lHandled := ::controller:keyboard( nKey )

   ENDCASE

   IF ! ::EditBuffer() == ::Get:Buffer
      IF ::Get:Type == "N"
         ::SetEditBuffer( Trim(::Get:Buffer) )
      ELSE
         ::SetEditBuffer( ::Get:Buffer )
      ENDIF

      /*
       * assign internal ::value
       */
      ::Get:assign()
      ::changed := .NOT. ( ::original == ::value )
   ENDIF

   IF lSetPos
      IF Set( _SET_INSERT )
         aMarked := { ::Get:Pos, ::Get:Pos }
      ELSE
         aMarked := { ::Get:Pos, ::Get:Pos+1 }
      ENDIF
      ::SetMarked( aMarked )
   ENDIF

   IF ! lHandled
      RETURN ::XbpSle:Keyboard( nKey )
   ENDIF
RETURN self


/*
 * Overloaded LbUp() method. When the left mouse button is released,
 * the position of the caret must be determined.
 */
METHOD XbpGet:LbUp(mp1, mp2)
   LOCAL aMarked

   IF ::get:type == "L"
      ::XbpSle:LbUp( mp1, mp2 )
      RETURN ::home()
   ENDIF

   /*
    * Tell the server Get where the cursor is
    */
   aMarked   := ::queryMarked()
   ::Get:Pos := aMarked[1]

   IF aMarked[2] - aMarked[1] == 0 .AND. ! Set( _SET_INSERT )
      /*
       * Mark the current character when overstrike mode is active
       */
      ::SetMarked( { aMarked[1], aMarked[1]+1 } )
   ENDIF
RETURN ::XbpSle:LbUp( mp1, mp2 )


/*
 * Overloaded :rbDown() method. When the right button is pressed, an XbpGet
 * must gain input focus. This triggers the post validation for the XbpGet
 * that currently has focus.
 */
METHOD XbpGet:rbDown( mp1, mp2 )
   IF .NOT. ::hasInputFocus()
      SetAppFocus( self )
   ELSE
      ::XbpSle:rbDown( mp1, mp2 )
   ENDIF
RETURN self


/*
 * Overloaded :rbDown() method. When the right button is pressed, an XbpGet
 * must gain input focus. This triggers the post validation for the XbpGet
 * that currently has focus.
 */
METHOD XbpGet:rbUp( mp1 )
   IF ::hasInputFocus()
      ::enableMenuItems( self )
      ::oContextMenu:itemSelected := {|nItem| ::itemSelected( nItem ) }
      ::oContextMenu:popUp( self, mp1 )
   ENDIF
RETURN self


/*
 * Overloaded :cutMarked() method. Copy the marked characters to the
 * clipboard and delete them from the edit buffer.
 */
METHOD XbpGet:CutMarked()
   ::CopyMarked()
RETURN ::DeleteMarked()


/*
 * Overloaded :PasteMarked() method. Insert the clipboard contents
 * into the edit buffer.
 */
METHOD XbpGet:PasteMarked()
   LOCAL aMarked := ::QueryMarked()
   LOCAL cString, oClipboard, i, nPaste, cChar

   /*
    * Get clipboard contents
    */
   oClipboard := XbpClipboard():New():Create()
   oClipboard:open()
   cString := oClipboard:getBuffer( XBPCLPBRD_TEXT )
   oClipboard:close()
   oClipboard:destroy()

   /*
    * Do not paste when the clipboard does not contain a valid string.
    */
   IF cString == NIL
      Tone(1000)
      RETURN .F.
   ENDIF

   /*
    * delete the marked string and
    * insert the current contents of the clipboard
    */
   ::deleteMarked()
   ::Get:Pos := Min( aMarked[1], aMarked[2] )

   IF Abs(aMarked[2] - aMarked[1]) <= 1
      nPaste := Len( cString )
   ELSE
      nPaste := Min( Len( cString ), Abs(aMarked[2] - aMarked[1]) )
   ENDIF

   FOR i := 1 TO nPaste
      IF i <= Len( cString )
         cChar := SubStr( cString, i, 1 )
      ELSE
         cChar := " "
      ENDIF
      ::Get:Insert( cChar )
   NEXT

   IF Set(_SET_INSERT) .AND. Max( aMarked[1], aMarked[2] ) + nPaste > ::bufferLength
      /*
       * Edit buffer is full. Set the Get cursor behind the last character.
       */
      ::get:pos := ::bufferLength + 1
   ENDIF
RETURN .T.


/*
 * Overloaded :deleteMarked() method
 */
METHOD XbpGet:DeleteMarked()
   LOCAL aMarked := ::QueryMarked()
   LOCAL nMarked := Abs( aMarked[2] - aMarked[1] )
   LOCAL lReturn := .T.

   IF nMarked == 0
      /*
       * We're in insert mode and nothing is marked -> delete nothing.
       */
       lReturn := .F.
   ELSEIF nMarked == 1
      ::Get:Delete()
   ELSE
      ::Get:Pos := Max( aMarked[1], aMarked[2] )
      FOR i := 1 TO nMarked
         ::Get:BackSpace()
      NEXT
   ENDIF

RETURN lReturn


/*
 * Selection in context menu
 */
METHOD XbpGet:itemSelected( nItem )
   DO CASE
   CASE nItem == MENU_ITEM_VALIDATE
      IF .NOT. ::postValidate()
         MsgBox( "Illegal data" )
         SetAppFocus( self )
      ENDIF

   CASE nItem == MENU_ITEM_UNDO      ; ::undo()
   CASE nItem == MENU_ITEM_CUT       ; ::cutMarked()
   CASE nItem == MENU_ITEM_COPY      ; ::copyMarked()
   CASE nItem == MENU_ITEM_PASTE     ; ::pasteMarked()
   CASE nItem == MENU_ITEM_DELETE    ; ::deleteMarked()
   CASE nItem == MENU_ITEM_SELECTALL ; ::setMarked( {1, Len(::editBuffer())+1 } )
   ENDCASE

   IF ! ::EditBuffer() == ::Get:Buffer
      IF ::Get:Type == "N"
         ::SetEditBuffer( Trim(::Get:Buffer) )
      ELSE
         ::SetEditBuffer( ::Get:Buffer )
      ENDIF

      /*
       * assign internal ::value
       */
      ::Get:assign()
      ::changed := .NOT. ( ::original == ::value )
   ENDIF

RETURN self


/*
 * Picture mask is changed. Tell it to the service Get.
 */
METHOD XbpGet:picture( cPict )
   IF Valtype( cPict ) == "C"
      ::picture := cPict
      IF ::Get <> NIL
         ::Get:picture := cPict
         ::Get:reset()
      ENDIF
   ENDIF
RETURN ::picture


/*
 * Perform a postvalidation.
 */
METHOD XbpGet:postValidate
   LOCAL lValid := .T.

   IF ::badDate()
      ::undo()
      RETURN .F.
   ENDIF

   /*
    * Assign value to edited variable to assure a proper
    * postvalidation context, but keep the original value.
    * Passing .F. assures that an :undo() is possible during
    * editing for all XbpGets contained in a dialog window, even if
    * focus changes (dialog wide :undo())
    */
   ::getData( .F. )

   IF Valtype( ::postBlock ) == "B"
      lValid := Eval( ::postBlock, self )
   ENDIF
RETURN lValid


/*
 * Perform a prevalidation.
 */
METHOD XbpGet:preValidate
   LOCAL lValid := .T.

   IF Valtype( ::preBlock ) == "B"
      lValid := Eval( ::preBlock, self )
   ENDIF
RETURN lValid


/*
 * Validate a date value.
 */
METHOD XbpGet:badDate

   IF ::Get:type <> "D"
      RETURN .F.
   ELSEIF ::Get:hasFocus
      /*
       * When an illegal date is entered
       * - Get:badDate() is .T. during editing
       * - Get:buffer is set to "  /  /  " due to :undo() in :postValidate()
       * - After postvalidation we have ::Get:buffer <> ::editBuffer() == .T.
       */
      RETURN ::Get:badDate() .OR. ::Get:buffer <> ::editBuffer()
   ENDIF

   /*
    * An empty date is valid
    */
RETURN IIf( .NOT. (::original == ::value), Empty( ::value ), .F. )
