//////////////////////////////////////////////////////////////////////
//
//  DATADLG.PRG
//
//  Copyright:
//      Alaska Software, (c) 1997-2025. All rights reserved.         
//  
//  Contents:
//      Class DataDialog and DataDialogMenu
//   
//////////////////////////////////////////////////////////////////////


#include "Gra.ch"
#include "Xbp.ch"
#include "Dmlb.ch"
#include "Common.ch"
#include "Appevent.ch"
#include "Mdidemo.ch"

/*
 * Class declaration
 */
CLASS DataDialog FROM XbpDialog
   PROTECTED:
     VAR appendMode              // Is it a new record?
     VAR editControls            // List of XBPs for editing data
     VAR appendControls          // List of XBPs enabled only
                                 // during APPEND
     VAR keyControls             // List of XBPs for keyboard navigation
     VAR keyboardBlock           // Code block for xbeP_Keyboard messages

   EXPORTED:
     VAR area      READONLY      // current work area
     VAR newTitle                // code block to change window title
     VAR contextMenu             // context menu for data dialog
     VAR windowMenu              // dynamic window menu in
                                 // application window
     VAR currentControl          // references the current Edit control
     VAR seekOrder               // Data for searching a record
     VAR seekExpr                // using the SeekDialog
     VAR seekFields
     VAR seekTitle

     METHOD init                 // overloaded methods
     METHOD create
     METHOD configure
     METHOD destroy
     METHOD addEditControl       // register XBP for edit
     METHOD addAppendControl     // register XBP for append
     METHOD addKeyControl        // register XBP for keyboard handling
     METHOD notify               // process DBO message
     METHOD readData             // read data from DBF
     METHOD validateAll          // validate all data stored in XBPs
     METHOD writeData            // write data from XBPs to DBF
     METHOD isIndexUnique        // check index value for uniqueness
     METHOD setDisplayFocus      // called after window has received focus
     METHOD keyHandler           // key handling for edit controls
ENDCLASS



/*
 * Initialize data dialog
 */
METHOD DataDialog:init( oParent, oOwner , ;
                        aPos   , aSize  , ;
                        aPParam, lVisible )
   LOCAL aPSize

   DEFAULT lVisible TO .F., ;
           oParent  TO SetAppWindow()

  /*
   * Default position: centered on parent
   */
   IF aPos == NIL
      aPSize := oParent:currentSize()
      aPos   := { (aPSize[1] - aSize[1]) / 2, ;
                  (aPSize[2] - aSize[2]) / 2  }
   ENDIF

   ::xbpDialog:init( oParent, oOwner, ;
                     aPos   , aSize , ;
                     aPParam, lVisible )

   ::maxButton       := .F.
   ::area            := 0
   ::border          := XBPDLG_THINBORDER
   ::editControls    := {}
   ::appendControls  := {}
   ::keyControls     := {}
   ::keyboardBlock   := {|nKey,x,obj| ::keyHandler( nKey, obj ) }
   ::appendMode      := .F.
   ::newTitle        := {|obj| obj:getTitle() }

RETURN self



/*
 * Load system resources and
 * register DataDialog in current work area
 */
METHOD DataDialog:create( oParent, oOwner , ;
                          aPos   , aSize  , ;
                          aPParam, lVisible )

   ::xbpDialog:create( oParent, oOwner , ;
                       aPos   , aSize  , ;
                       aPParam, lVisible )

   ::appendMode      := Eof()
   ::area            := Select()

   ::close           := {|mp1,mp2,obj| obj:destroy() }

   DbRegisterClient( self )

RETURN self



/*
 * Configure system resources
 * Register data dialog in new work area if necessary
 */
METHOD DataDialog:configure( oParent, oOwner , ;
                             aPos   , aSize  , ;
                             aPParam, lVisible )
   LOCAL lRegister := (::area <> Select())

   ::xbpDialog:configure( oParent, oOwner , ;
                          aPos   , aSize  , ;
                          aPParam, lVisible )
   IF lRegister
      (::area)->( DbDeRegisterClient( self ) )
   ENDIF

   ::area       := Select()
   ::appendMode := Eof()

   IF lRegister
      DbRegisterClient( self )
   ENDIF

RETURN self



/*
 * Release system resources, unregister data dialog from work area
 */
METHOD DataDialog:destroy()

  /*
   * Ignore call to Destroy() when window is not created.
   */
   IF ::Status() == XBP_STAT_INIT
      RETURN self
   ENDIF

   ::writeData()
   ::hide()
   (::Area)->( DbCloseArea() )

  /*
   * Delete menu item in window menu
   */
   IF ! Empty( ::windowMenu )
      ::windowMenu:delItem( self ) //
      ::windowMenu := NIL
   ENDIF

  /*
   * Delete reference of data dialog and context menu
   */
   IF ! Empty( ::contextMenu )
      ::contextMenu:cargo := NIL
      ::contextMenu       := NIL
   ENDIF

  /*
   * Release system resources and set instance variables
   * to values corresponding to the :init() state
   */
   ::xbpDialog:destroy()
   ::Area           :=  0
   ::appendMode     := .F.
   ::editControls   := {}
   ::appendControls := {}
   ::newTitle       := {|obj| obj:getTitle() }

RETURN self



/*
 * Notify method:
 *   - Write data to fields prior to moving the record pointer
 *   - Read data from fields after moving the record pointer
 */
METHOD DataDialog:notify( nEvent, mp1, mp2 )

   IF nEvent <> xbeDBO_Notify
      RETURN self                         // ** RETURN **
   ENDIF

   DbSuspendNotifications()
   DO CASE
   CASE mp1 == DBO_MOVE_PROLOG            // record pointer is about
      ::writeData()                       // to be moved

   CASE mp1 == DBO_MOVE_DONE .OR. ;       // skip is done
        mp1 == DBO_GOBOTTOM  .OR. ;
        mp1 == DBO_GOTOP
      ::readData()

      DO CASE
      CASE Empty( ::contextMenu )
      CASE Eof()                          // Enable/disable items
         ::contextMenu:disableBottom()    // in context menu
         ::contextMenu:disableEof()       // depending on the
      CASE mp1 == DBO_GOBOTTOM            // position of the
         ::contextMenu:enableAll()        // record pointer
         ::contextMenu:disableBottom()
      CASE mp1 == DBO_GOTOP
         ::contextMenu:enableAll()
         ::contextMenu:disableTop()
      OTHERWISE
         ::contextMenu:enableAll()
      ENDCASE

      DO CASE
      CASE Eof()                               // Enable/disable items
         WinMenu():editMenu:disableBottom()    // in edit menu of the window
         WinMenu():editMenu:disableEof()       // depending on the
      CASE mp1 == DBO_GOBOTTOM                 // position of the
         WinMenu():editMenu:enableAll()        // record pointer
         WinMenu():editMenu:disableBottom()
      CASE mp1 == DBO_GOTOP
         WinMenu():editMenu:enableAll()
         WinMenu():editMenu:disableTop()
      OTHERWISE
         WinMenu():editMenu:enableAll()
      ENDCASE

   ENDCASE
   DbResetNotifications()

RETURN self



/*
 * Add an edit control to internal list
 */
METHOD DataDialog:addEditControl( oXbp )
   IF AScan( ::editControls, oXbp ) == 0
      AAdd(  ::editControls, oXbp )
      ::addKeyControl( oXbp )
   ENDIF
RETURN self



/*
 * Add an append control to internal list
 */
METHOD DataDialog:addAppendControl( oXbp )
   IF AScan( ::appendControls, oXbp ) == 0
      AAdd(  ::appendControls, oXbp )
      ::addKeyControl( oXbp )
   ENDIF
RETURN self



/*
 * Add a control to internal list for keyboard handling
 * Each XBP gets the same keyboard code block. It is evaluated
 * after the xbeP_Keyboard message and calls a customized key handler
 */
METHOD DataDialog:addKeyControl( oXbp )
   IF AScan( ::keyControls, oXbp ) == 0
      AAdd(  ::keyControls, oXbp )

      oXbp:keyBoard := ::keyboardBlock
   ENDIF
RETURN self



/*
 * Window has received focus. Select work area, check menu item
 * and set focus to current Edit control
 */
METHOD DataDialog:setDisplayFocus

 LOCAL oFocus

   DbSelectArea( ::area )

   IF ::windowMenu <> NIL
      ::windowMenu:checkItem( self )
   ENDIF

   IF ::currentControl <> NIL
      oFocus := SetAppFocus()
      IF oFocus != self .AND. GetParentForm(oFocus) != self
         RETURN self
      ENDIF

      SetAppFocus( ::currentControl )
   ENDIF
RETURN self



/*
 * Customized key handling for edit controls
 */
METHOD DataDialog:keyHandler( nKey, oXbp )
   LOCAL i:=AScan( ::keyControls, oXbp )

   DO CASE
   CASE i == 0
   CASE oXbp:isDerivedFrom( "XbpMLE" ) .OR. oXbp:isDerivedFrom( "XbpListbox" )
     // Return, PageUp, PageDown are processed by MLE and list box.
     // Therefore, the keys must be ignored.

   CASE nKey == xbeK_RETURN
      IF ++i > Len( ::keyControls )
         i := 1
      ENDIF
      DO WHILE ! ::keyControls[i]:isenabled()
         i ++
      ENDDO
      SetAppFocus( ::keyControls[i] )

   CASE nKey == xbeK_PGDN
      IF ! Eof()
         SKIP
         IF Eof()
            DbSuspendNotifications()
            SKIP -1                    // Avoid append situation
            DbResetNotifications()
            DbGobottom()               // Trigger notification
         ENDIF
      ENDIF

   CASE nKey == xbeK_PGUP
      IF ! Bof()
         SKIP -1
      ENDIF

   ENDCASE
RETURN self



/*
 * Read current record and transfer data to edit controls
 */
METHOD DataDialog:readData()
   LOCAL i, imax  := Len( ::editControls )

   FOR i:=1 TO imax                       // Transfer data from file
      ::editControls[i]:setData()         // to XBPs
   NEXT

   Eval( ::newTitle, self )               // Set new window title

   IF Eof()                               // enable/disable XBPs
      IF ! ::appendMode                   // active only during
         imax  := Len( ::appendControls ) // APPEND
         FOR i:=1 TO imax                 //
            ::appendControls[i]:enable()  // Hit Eof(), so
         NEXT                             // enable XBPs
      ENDIF
      ::appendMode := .T.
   ELSEIF ::appendMode                    // Record pointer was
      imax  := Len( ::appendControls )    // moved from Eof() to
      FOR i:=1 TO imax                    // an existing record.
         ::appendControls[i]:disable()    // Disable append-only
      NEXT                                // XBPs
      ::appendMode := .F.
   ENDIF

RETURN



/*
 * Write data from edit controls to file
 */
METHOD DataDialog:writeData()
   LOCAL i, imax
   LOCAL lLocked   := .F. , ;       // Is record locked?
         lAppend   := .F. , ;       // Is record new?
         aChanged  := {}  , ;       // XBPs containing changed data
         nOldArea  := Select()      // Current work area

   dbSelectArea( ::area )

   IF Eof()                         // Append a new record
      IF ::validateAll()            // Validate data first
         APPEND BLANK
         lAppend  := .T.
         aChanged := ::editControls // Test all possible changes
         lLocked  := ! NetErr()     // Implicit lock
      ELSE
         MsgBox("Invalid data")     // Do not write invalid data
         DbSelectArea( nOldArea )   // to new record
         RETURN .F.                 // *** RETURN ***
      ENDIF
   ELSE
      imax := Len( ::editControls ) // Find all XBPs containing
      FOR i:=1 TO imax              // changed data
         IF ::editControls[i]:changed
            AAdd( aChanged, ::editControls[i] )
         ENDIF
      NEXT

      IF Empty( aChanged )          // Nothing has changed, so
         DbSelectArea( nOldArea )   // no record lock necessary
         RETURN .T.                 // *** RETURN ***
      ENDIF

      lLocked := DbRLock( Recno() ) // Lock current record
   ENDIF

   IF ! lLocked
      MsgBox( "Record is currently locked" )
      DbSelectArea( nOldArea )      // Record lock failed
      RETURN .F.                    // *** RETURN ***
   ENDIF

   imax := Len( aChanged )          // Write access is necessary
   FOR i:=1 TO imax                 // only for changed data
      IF ! lAppend
         IF ! aChanged[i]:validate()
            aChanged[i]:undo()      // invalid data !
            LOOP                    // undo changes and validate
         ENDIF                      // next XBP
      ENDIF
      aChanged[i]:getData()         // Get data from XBP and
   NEXT                             // write to file

   DbCommit()                       // commit buffers
   DbRUnlock( Recno() )             // Release record lock

   IF ::appendMode                  // Disable append-only XBPs
      imax  := Len( ::appendControls ) // after APPEND
      FOR i:=1 TO imax
         ::appendControls[i]:disable()
      NEXT
      ::appendMode := .F.

      IF ! Empty( ::contextMenu )
         ::contextMenu:disableBottom()
         ::contextMenu:enableEof()
      ENDIF
      WinMenu():editMenu:disableBottom()
      WinMenu():editMenu:enableEof()
   ENDIF

   DbSelectArea( nOldArea )

RETURN .T.



/*
 * Validate data of all edit controls
 * This is necessary prior to appending a new record to the database
 */
METHOD DataDialog:validateAll()
   LOCAL i := 0, imax := Len( ::editControls )
   LOCAL lValid := .T.

   DO WHILE ++i <= imax .AND. lValid
      lValid := ::editControls[i]:validate()
   ENDDO

RETURN lValid



/*
 * Check whether an index value does *not* exist in an index
 */
METHOD DataDialog:isIndexUnique( nOrder, xIndexValue )
   LOCAL nOldOrder := OrdNumber()
   LOCAL nRecno    := Recno()
   LOCAL lUnique   := .F.

   DbSuspendNotifications()         // Suppress notification from DBO
                                    // to self during DbSeek() !!!
   OrdSetFocus( nOrder )

   lUnique := .NOT. DbSeek( xIndexValue )

   OrdSetFocus( nOldOrder )
   DbGoTo( nRecno )

   DbResetNotifications()

RETURN lUnique



/*
 * Class declaration for context menu
 */
CLASS DataDialogMenu FROM XbpMenu
   PROTECTED:
      VAR disabledItems
      METHOD disable
      METHOD enable

   EXPORTED:
      VAR disableTop
      VAR disableBottom
      VAR disableEof

      METHOD init, create
      METHOD goTop, goBottom, skip, seek, append, delete
      METHOD enableAll
      METHOD enableTop
      METHOD enableBottom
      METHOD enableEof
      METHOD disableTop
      METHOD disableBottom
      METHOD disableEof
ENDCLASS



/*
 * Initialize instance variables
 */
METHOD DataDialogMenu:init( oParent, aPresParam, lVisible )
   ::xbpMenu:init( oParent, aPresParam, lVisible )
   ::disabledItems := {}

   ::xbpMenu:title := "~Edit"

   // menu items are disabled after Bof() or GoTop()
   ::disableTop    := { 6, 9 }

   // menu items are disabled after GoBottom()
   ::disableBottom := { 7, 10 }

   // menu items are disabled at Eof()
   ::disableEof    := { 1, 2, 3 }

RETURN self



/*
 * Initialize menu items
 */
METHOD DataDialogMenu:create( oParent, aPresParam, lVisible )
   ::xbpMenu:create( oParent, aPresParam, lVisible )

   ::addItem( { "~New"      , {|| ::append()             } } )
   ::addItem( { "~Seek"     , {|| ::seek()               } } )
   ::addItem( { "~Delete"   , {|| ::delete()             } } )
   ::addItem( { "S~ave"     , {|| WinMenu():currentWin:writeData() } } )

   ::addItem( MENUITEM_SEPARATOR )

   ::addItem( { "~First"    , {|| ::goTop()    } } )
   ::addItem( { "~Last"     , {|| ::gobottom() } } )

   ::addItem( MENUITEM_SEPARATOR )

   ::addItem( { "~Previous" , {|| ::skip(-1) } } )

   ::addItem( { "Ne~xt"     , {|| ::skip( 1) } } )

RETURN self



/*
 * Some standard database operations
 */
METHOD DataDialogMenu:goTop
   (WinMenu():currentWin:area)->(DbGotop())
RETURN self

METHOD DataDialogMenu:skip( nSkip )
   LOCAL nOldArea := Select()
   LOCAL nArea    := WinMenu():currentWin:area

   DEFAULT nSkip TO 1

   DbSelectArea( nArea )
   DbSkip(nSkip)
   IF Eof()
      DbSuspendNotifications()
      SKIP -1                          // Avoid append situation
      DbResetNotifications()
      DbGobottom()                     // Trigger notification
   ENDIF

   DbSelectArea( nOldArea )
RETURN self

METHOD DataDialogMenu:goBottom
   (WinMenu():currentWin:area)->(DbGobottom())
RETURN self

METHOD DataDialogMenu:seek
   SeekRecord()
RETURN self

METHOD DataDialogMenu:append
   (WinMenu():currentWin:area)->(DbGoto(Lastrec()+1))
RETURN self

METHOD DataDialogMenu:delete
   DeleteRecord()
RETURN self



/*
 * Enable all menu items
 */
METHOD DataDialogMenu:enableAll
   LOCAL i, imax

   IF ! Empty( ::disabledItems )
      imax := Len( ::disabledItems )

      FOR i:=1 TO imax
         ::enableItem( ::disabledItems[i] )
      NEXT
     ::disabledItems := {}
   ENDIF

RETURN self



/*
 * Enable some menu items
 */
METHOD DataDialogMenu:enable( aItems )
   LOCAL i, imax, j, jmax

   IF ! Empty( aItems )
      imax := Len( aItems )
      jmax := Len( ::disabledItems )

      FOR i:=1 TO imax
         j := AScan( ::disabledItems, aItems[i] )

         IF j > 0
            ::enableItem( aItems[i] )
            ADel( ::disabledItems, j )
            jmax--
         ENDIF
      NEXT

      ASize( ::disabledItems, jmax )
   ENDIF

RETURN self



/*
 * Disable some menu items
 */
METHOD DataDialogMenu:disable( aItems )
   LOCAL i, imax

   IF ! Empty( aItems )
      imax := Len( aItems )

      FOR i:=1 TO imax
         IF AScan( ::disabledItems, aItems[i] ) == 0
            ::disableItem( aItems[i] )
            AAdd( ::disabledItems, aItems[i] )
         ENDIF
      NEXT
   ENDIF

RETURN self



/*
 * Enable menu items which may be selected after GO TOP
 */
METHOD DataDialogMenu:enableTop
RETURN ::enable( ::disableTop )



/*
 * Enable menu items which may be selected after GO BOTTOM
 */
METHOD DataDialogMenu:enableBottom
RETURN ::enable( ::disableBottom )



/*
 * Enable menu items which may be selected at Eof()
 */
METHOD DataDialogMenu:enableEof
RETURN ::enable( ::disableEof )



/*
 * Disable menu items which may not be selected after GO TOP
 * or at Bof()
 */
METHOD DataDialogMenu:disableTop
RETURN ::disable( ::disableTop )



/*
 * Disable menu items which may not be selected after GO BOTTOM
 */
METHOD DataDialogMenu:disableBottom
RETURN ::disable( ::disableBottom )



/*
 * Disable menu items which may not be selected at Eof()
 */
METHOD DataDialogMenu:disableEof
RETURN ::disable( ::disableEof )
