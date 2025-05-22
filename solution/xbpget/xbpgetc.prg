//////////////////////////////////////////////////////////////////////
//
//  XBPGETC.PRG
//
//  Copyright:
//       Alaska Software, (c) 1999-2025. All rights reserved.         
//  
//  Contents:
//       The XbpGetController class performs pre- and postvalidation of XbpGet
//       objects and navigates between input controls in a window. The usage is
//       as follows:
//   
//         oWindow is a window containing XbpGet objects
//   
//         // Create a controller object
//         oController := XbpGetController():new( oWindow ):create()
//   
//         // 1st variant: a modal Read does not require an event Loop
//         oController:read()
//   
//         // 2nd variant: a modeless Read requires an event Loop
//         oController:read( , .F. )
//         DO WHILE Condition
//            nEvent := AppEvent( mp1, mp2, oXbp )
//            oXbp:handleEvent( nEvent, mp1, mp2 )
//         ENDDO
//   
//  Remarks:
//       The :create() method replaces all callback code blocks required for
//       focus change and keyboard navigation.
//   
//       To program an own XbpGetController class, the following methods
//       must be implemented because they are called by XbpGet objects:
//   
//          :preValidate ( oXbpGet ) --> lIsValid
//          :postValidate( oXbpGet ) --> lIsValid
//          :keyboard    ( nKey    ) --> lKeyIsHandled
//   
//       This instance variable must exist. It is accessed from XbpGet
//   
//          :setHome := .T. | .F.
//   
//////////////////////////////////////////////////////////////////////


#include "Appevent.ch"
#include "Common.ch"
#include "Get.ch"
#include "Xbp.ch"


#define  CB_CLOSE                 1
#define  CB_SETINPUTFOCUS         2
#define  CB_SETDISPLAYFOCUS       3
#define  CB_KILLDISPLAYFOCUS      4
#define  CB_KEYBOARD              5
#define  CB_TABACTIVATE           6

#define  CB_GETSET_COUNT          6

#define  SV_GET_CB                1
#define  SV_XBP                   2
#define  SV_OLD_CB                3

#define  SV_COUNT                 3


CLASS XbpGetController
   PROTECTED:
   VAR aControls
   VAR aGetList
   VAR aCallback
   VAR aGetSet
   VAR aNewCB
   VAR nCurrent
   VAR oWindow

   VAR hasDisplayFocus

   VAR thisXbp, lastXbp
   VAR readExit

   METHOD collect
   METHOD saveCallBack
   METHOD getCallback
   METHOD restCallBack
   METHOD activate

   EXPORTED:
                             ** These must be exported
   VAR doPostValidate        // enable validation
   VAR exitState READONLY    // last exit state
   VAR setHome               // XbpGet cursor must be set to first position

   METHOD init, create, destroy

                             ** Methods reserved for XbpDialog/XbpCrt windows
   METHOD setDisplayFocus    // window obtains display focus
   METHOD killDisplayFocus   // window looses display focus
   METHOD setInputFocus      // window obtains input focus
   METHOD close              // window is about to be closed

                             ** Methods reserved for other XBPs
   METHOD addEditControl     // Adds an editable XBP
   METHOD addFocusControl    // Adds an XBP that can get input focus
   METHOD tabActivate        // intercepts Tab activation events
   METHOD keyboard           // keyboard navigation for XBPs contained in window
                             ** Methods for navigation
   METHOD first              // go to first control
   METHOD last               // go to last control
   METHOD up                 // go to previous control
   METHOD down               // go to next control
                             ** Methods for editing
   METHOD preValidate        // data validation on single entry field level
   METHOD postValidate       //
   METHOD clearEventLoop     // clear event loop after validation
   METHOD read               // start editing
   METHOD killRead           // enforce edit termination
   METHOD getActive          // returns current edit control
   METHOD getList            // array containing edit controls
   METHOD setData            // transfer data into edit controls
   METHOD getData            // get data from edit controls
ENDCLASS


METHOD XbpGetController:init( oWindow )
   ::oWindow         := oWindow
   ::doPostValidate  := .T.
   ::setHome         := .F.
   ::hasDisplayFocus := .F.
RETURN self


/*
 * Create new callback code blocks and get/set codeblocks to replace
 * existing callback code blocks.
 */
METHOD XbpGetController:create( oWindow )
   ::aControls := {}
   ::aGetList  := {}
   ::aCallback := {}
   ::nCurrent  := 1

   IF Valtype( oWindow ) == "O"
      ::oWindow := oWindow
   ENDIF
 
   ::aGetSet                      := Array( CB_GETSET_COUNT )
   ::aGetSet[CB_CLOSE           ] := {|o,b| IIf( PCount()<>2, o:close           , o:close            := b ) }
   ::aGetSet[CB_SETINPUTFOCUS   ] := {|o,b| IIf( PCount()<>2, o:setInputFocus   , o:setInputFocus    := b ) }
   ::aGetSet[CB_SETDISPLAYFOCUS ] := {|o,b| IIf( PCount()<>2, o:setDisplayFocus , o:setDisplayFocus  := b ) }
   ::aGetSet[CB_KILLDISPLAYFOCUS] := {|o,b| IIf( PCount()<>2, o:killDisplayFocus, o:killDisplayFocus := b ) }
   ::aGetSet[CB_KEYBOARD        ] := {|o,b| IIf( PCount()<>2, o:keyboard        , o:keyboard         := b ) }
   ::aGetSet[CB_TABACTIVATE     ] := {|o,b| IIf( PCount()<>2, o:tabActivate     , o:tabActivate      := b ) }

   ::aNewCB                       := Array( CB_GETSET_COUNT )
   ::aNewCB [CB_CLOSE           ] := {|mp1,mp2,o| ::close           ( mp1, mp2, o ) }
   ::aNewCB [CB_SETINPUTFOCUS   ] := {|mp1,mp2,o| ::setInputFocus   ( mp1, mp2, o ) }
   ::aNewCB [CB_SETDISPLAYFOCUS ] := {|mp1,mp2,o| ::setDisplayFocus ( mp1, mp2, o ) }
   ::aNewCB [CB_KILLDISPLAYFOCUS] := {|mp1,mp2,o| ::killDisplayFocus( mp1, mp2, o ) }  
   ::aNewCB [CB_KEYBOARD        ] := {|mp1,mp2,o| ::keyboard        ( mp1, mp2, o ) }  
   ::aNewCB [CB_TABACTIVATE     ] := {|mp1,mp2,o| ::tabActivate     ( mp1, mp2, o ) }  

   ::collect( ::oWindow )   
RETURN self


/*
 * Restore the original state in the window and release all arrays.
 */
METHOD XbpGetController:destroy

   /*
    * Remove self's reference
    */
   AEval( ::aGetList, {|o| IIf( IsMemberVar( o, "Controller" ), o:Controller := NIL, NIL ) } )

   /*
    * Restore changed callback codeblocks
    */
   ::restCallback()

   ::aCallback := ;
   ::aControls := ;
   ::aGetList  := ;
   ::aGetSet   := ;
   ::aNewCB    := ;
   ::thisXbp   := ;
   ::lastXbp   := ;
   ::oWindow   := NIL
RETURN self


/*
 * Save and replace all callback code blocks for keyboard and
 * focus change events. This enables the controller to take corrective
 * action when a pre- or postvalidation fails.
 */
METHOD XbpGetController:collect( oXbp )
   LOCAL aXbp, i, imax

   IF oXbp:isDerivedFrom( "XbpDialog" ) .OR. ;
      oXbp:isDerivedFrom( "XbpCrt"    )

      ::saveCallback( oXbp, ::aGetSet[CB_CLOSE           ], ::aNewCB[CB_CLOSE           ] )
      ::saveCallback( oXbp, ::aGetSet[CB_SETINPUTFOCUS   ], ::aNewCB[CB_SETINPUTFOCUS   ] )
      ::saveCallback( oXbp, ::aGetSet[CB_KILLDISPLAYFOCUS], ::aNewCB[CB_KILLDISPLAYFOCUS] )
      ::saveCallback( oXbp, ::aGetSet[CB_SETDISPLAYFOCUS ], ::aNewCB[CB_SETDISPLAYFOCUS ] )
   ENDIF

   IF oXbp:isDerivedFrom( "XbpDialog" )
      RETURN ::collect( oXbp:drawingArea )
   ENDIF

   /*
    * ::aGetList collects editable Xbase Parts.
    * ::aControls contains Xbase Parts which can get focus in the window.
    */

   IF IsMemberVar( oXbp, "dataLink" )
      ::addEditControl( oXbp )

      IF IsMemberVar( oXbp, "tabStop" ) .AND. oXbp:TabStop
         ::addFocusControl( oXbp )
      ENDIF
   ELSE
      IF oXbp:isDerivedFrom( "XbpTabpage" ) 
         ::saveCallback( oXbp, ::aGetSet[CB_TABACTIVATE], ::aNewCB[CB_TABACTIVATE] )

      ELSEIF IsMemberVar( oXbp, "tabStop" ) .AND. oXbp:TabStop
         ::addFocusControl( oXbp )
      ENDIF

      aXbp := oXbp:childList()   
      imax := Len( aXbp )
      FOR i:=1 TO imax
         ::collect( aXbp[i] )
      NEXT
   ENDIF

   IF IsMemberVar( oXbp, "Controller" )
      oXbp:controller := self
      IF Atail( ::aGetList ) <> oXbp
         AAdd( ::aGetList, oXbp )
      ENDIF
   ENDIF

RETURN self


/*
 * ::aGetList collects editable Xbase Parts.
 */
METHOD XbpGetController:addEditControl( oXbp )
   IF IsMemberVar( oXbp, "tabStop" ) .AND. !oXbp:TabStop
      RETURN(SELF)
   ENDIF
   IF Ascan( ::aGetList, oXbp ) == 0
      AAdd( ::aGetList, oXbp )
   ENDIF
RETURN self



/*
 * ::aControls contains Xbase Parts which can get focus in the window.
 * The :tabStop flag defines whether or not an XBP is added to that list
 * (keyboard navigation within a window).
 */
METHOD XbpGetController:addFocusControl( oXbp )

   IF oXbp:tabStop
      AAdd( ::aControls, oXbp )

      IF oXbp:isDerivedFrom( "XbpGet" )
        /*
         * XbpGet does not require a :keyboard code block, since its
         * :keyboard() method handles the communication with XbpGetController.
         */
         ::saveCallback( oXbp, ::aGetSet[CB_KEYBOARD], NIL )
      ELSE
        /*
         * Replace and save the :keyboard code block to ensure that a
         * focus change is handled by XbpGetController.
         */
         ::saveCallback( oXbp, ::aGetSet[CB_KEYBOARD], ::aNewCB[CB_KEYBOARD] )
      ENDIF
   ENDIF

RETURN self



/*
 * Save and replace one callback code block.
 */
METHOD XbpGetController:saveCallBack( oXbp, bGetSet, bNew )
   LOCAL bOld := Eval( bGetSet, oXbp )
   LOCAL aSave[ SV_COUNT ]

   aSave[SV_GET_CB] := bGetSet
   aSave[SV_XBP   ] := oXbp
   aSave[SV_OLD_CB] := bOld

   EVal( bGetSet, oXbp, bNew )
   AAdd( ::aCallback, aSave )
RETURN self


/*
 * Return a saved callback code block.
 */
METHOD XbpGetController:getCallback( oXbp, bGetSet )
   LOCAL i := Ascan( ::aCallback, {|a| a[SV_XBP] == oXbp .AND. a[SV_GET_CB] == bGetSet } )
RETURN IIf( i==0, NIL, ::aCallback[i, SV_OLD_CB] )


/*
 * Restore all saved callback code blocks.
 */
METHOD XbpGetController:restCallBack
   AEval( ::aCallback, {|a| Eval( a[SV_GET_CB], a[SV_XBP], a[SV_OLD_CB] ) } )
RETURN self


/*
 * Window gains input focus.
 * Set focus to current editable XBP inside window and enable validation
 */
METHOD XbpGetController:setInputFocus   
   LOCAL bBlock

   IF ::oWindow <> NIL .AND. ::oWindow:getFrameState() == XBPDLG_FRAMESTAT_MINIMIZED
      /*
       * The window is minimized. Don't set focus.
       */
      RETURN self
   ENDIF

   /*
    * If the asscociated window is a dialog, 
    * check whether this event was sent during 
    * a restore operation. In this case, 
    * internal state may have to be reset to 
    * properly reflect the validation state. 
    * Otherwise, the user can't switch between
    * tab pages after a restore, for instance.
    */
   IF ::hasDisplayFocus == .F. .AND. ;
      ::oWindow         != NIL .AND. ;
      (::oWindow:IsDerivedFrom(XbpDialog()) == .T. .OR.;
       ::oWindow:IsDerivedFrom(XbpCrt())    == .T.)
      ::setDisplayFocus()

      IF ::exitState == GE_LASTGET
         ::exitState = GE_NOEXIT
      ENDIF
   ENDIF

   ::doPostValidate := .T.

   IF ::lastXbp <> NIL
      ::thisXbp := ::lastXbp
      RETURN SetAppFocus( ::lastXbp )
   ENDIF

   IF ::thisXbp == NIL
      ::thisXbp := ::aControls[ ::nCurrent ]
   ENDIF

   bBlock := ::getCallBack( ::oWindow, ::aGetSet[ CB_SETINPUTFOCUS ] )
   IF bBlock <> NIL
      /*
       * Evaluate the original :setInputFocus codeblock.
       */
      Eval( bBlock, NIL, NIL, ::oWindow  )
   ENDIF

   SetAppFocus( ::thisXbp )
RETURN self


/*
 * Window gains focus.
 * Set focus to current editable XBP inside window and enable validation
 */
METHOD XbpGetController:setDisplayFocus   
   LOCAL bBlock := ::getCallBack( ::oWindow, ::aGetSet[ CB_SETDISPLAYFOCUS ] )

   ::hasDisplayFocus = .T.

   IF bBlock <> NIL
      /*
       * Evaluate the original :setDisplayFocus codeblock.
       */
      Eval( bBlock, NIL, NIL, ::oWindow  )
   ENDIF

RETURN ::setInputFocus()


/*
 * Window looses focus.
 * Keep track of XBP that has focus and disable validation
 */
METHOD XbpGetController:killDisplayFocus   
   LOCAL bBlock
   LOCAL oXbp := SetAppFocus()
   LOCAL i    := Ascan( ::aControls, {|o| o == oXbp } )

   ::hasDisplayFocus = .F.

   IF i > 0 .AND. i <> ::nCurrent
      ::nCurrent := i
      ::thisXbp := ::aControls[i]
   ENDIF

   ::doPostValidate := .F.
   ::lastXbp        := ::thisXbp
   ::thisXbp        := NIL
   ::exitState      := GE_LASTGET

   bBlock := ::getCallBack( ::oWindow, ::aGetSet[ CB_KILLDISPLAYFOCUS ] )
   IF bBlock <> NIL
      /*
       * Evaluate the original :killDisplayFocus codeblock.
       */
      Eval( bBlock, NIL, NIL, ::oWindow  )
   ENDIF
RETURN self


/*
 * Window is about to be closed.
 */
METHOD XbpGetController:close
   LOCAL lSave := ::doPostValidate
   LOCAL bBlock

   IF ::thisXbp == NIL
      ::thisXbp := ::lastXbp
   ENDIF
   
   /*
    * Enforce post validation on current XbpGet when window gets closed
    */
   ::doPostValidate := .T.
   ::readExit       := ::postValidate()
   ::doPostValidate := lSave

   IF ::readExit
      /*
       * Window can be closed. Evaluate the original :close code block
       */
      bBlock := ::getCallBack( ::oWindow, ::aGetSet[ CB_CLOSE ] )

      IF bBlock <> NIL
         Eval( bBlock, NIL, NIL, ::oWindow  )
      ENDIF
      ::destroy()
   ELSE
      /*
       * Post validation failed. Keep window open.
       */
      MsgBox( "Illegal data. Please correct before closing the window." )

      IF ::oWindow:getFrameState() == XBPDLG_FRAMESTAT_MINIMIZED
         ::oWindow:setFrameState( XBPDLG_FRAMESTAT_NORMALIZED )
      ENDIF

      /*
       * Void the xbeP_Close event and set focus to XBP whose validation failed.
       */
      SetAppFocus( ::oWindow )
      SetAppFocus( ::lastXbp )
   ENDIF
RETURN self


/*
 * A TabPage is clicked.
 */
METHOD XbpGetController:tabActivate( mp1, mp2, oTab )
   LOCAL bBlock := ::getCallback( oTab, ::aGetSet[ CB_TABACTIVATE ] )

   IF ::exitState <> GE_LASTGET
      /*
       * Postvalidation is successful.
       * Evaluate the original :tabActivate code block.
       */
      oTab := Eval( bBlock, mp1, mp2, oTab )
   ELSE
      /*
       * Postvalidation failed.
       * TabPage remains in the foreground.
       */
      oTab := ::thisXbp:setParent()
      DO WHILE ! oTab:isDerivedfrom( "XbpTabpage" )
         oTab := oTab:setParent()
         IF oTab == ::oWindow .OR. oTab:isDerivedFrom( "XbpIWindow" )
            EXIT
         ENDIF
      ENDDO
      oTab:toFront()
   ENDIF
RETURN oTab


/*
 * Keyboard handling
 */
METHOD XbpGetController:keyboard( nKey, mp2, oXbp )
   LOCAL bBlock := SetAppEvent( nKey )
   LOCAL i

   ::exitState := GE_NOEXIT
   ::setHome   := .T.

   DO CASE
   CASE bBlock <> NIL
     /*
      * A code block is associated with an event.
      */

   CASE nKey == xbeK_ALT_F4
      PostAppEvent( xbeP_Close,,, ::oWindow )
      RETURN .T.

   CASE nKey == xbeK_TAB
      ::exitState := GE_DOWN
      bBlock      := {|| ::down() }

   CASE nKey == xbeK_SH_TAB
      ::exitState := GE_UP
      bBlock      := {|| ::up() }

   CASE ::thisXbp <> NIL .AND. .NOT. Empty( ::thisXbp:childList() )
     /*
      * NOTE: XBP contains other controls and must handle the
      *       following keys on its own
      */

   CASE nKey == xbeK_CTRL_HOME
      ::exitState := GE_TOP
      bBlock      := {|| ::first() }

   CASE nKey == xbeK_CTRL_END
      ::exitState := GE_BOTTOM
      bBlock      := {|| ::last() }

   CASE nKey == xbeK_ENTER
      IF ::thisXbp:isDerivedFrom( "XbpPushbutton" )
         PostAppEvent( xbeP_Activate, NIL, NIL, ::thisXbp )
         ::setHome   := .F.
      ELSEIF ! ::thisXbp:isDerivedFrom( "XbpMLE" )
         ::exitState := GE_DOWN
         bBlock      := {|| ::down() }
      ENDIF

   CASE nKey == xbeK_DOWN
      ::exitState := GE_DOWN
      bBlock      := {|| ::down() }

   CASE nKey == xbeK_UP
      ::exitState := GE_UP
      bBlock      := {|| ::up() }

   OTHERWISE
      ::setHome   := .F.
      bBlock      := ::getCallback( ::thisXbp, ::aGetSet[CB_KEYBOARD] )

   ENDCASE

   IF bBlock <> NIL 
      i := Ascan( ::aControls, {|o| o == oXbp } )
      IF i > 0 .AND. i <> ::nCurrent
         /*
          * A non-XbpGet object has received input focus via mouse click
          */
         ::nCurrent := i
         ::lastXbp  := ::thisXbp
         ::thisXbp  := ::aControls[i]
      ENDIF
      /*
       * Evaluate the code block associated with a key
       */
      Eval( bBlock, nKey, NIL, ::thisXbp )
   ENDIF
RETURN ( bBlock <> NIL )


/*
 * Validation before an Xbase Part gains input focus.
 */
METHOD XbpGetController:preValidate( oXbp )
   LOCAL lValid := .T.

   DEFAULT oXbp TO ::thisXbp

   IF ::lastXbp == oXbp .OR. ::exitState == GE_LASTGET
      /*
       * No prevalidation is required when:
       *  - the window that contains ::lastXbp has lost focus and gets it
       *    it back now. The prevalidation is done already in that case.
       *  - a postvalidation has failed and focus must be set back to
       *    the XbpGet holding illegal data.
       */
   ELSEIF IsMethod( oXbp, "preValidate" )
      /*
       * Focus change between controls in the same window
       * requires a prevalidation.
       */
      lValid := oXbp:preValidate()

   ENDIF

   IF lValid
      IF ::exitState == GE_LASTGET .AND. ::lastXbp <> NIL
         /*
          * Focus goes back to XbpGet where postvalidation failed
          */
         SetAppFocus( ::lastXbp )
         lValid      := .F.
         ::clearEventLoop()
         ::exitState := GE_NOEXIT
      ELSE
         ::thisXbp   := ;
         ::lastXbp   := oXbp
         ::nCurrent  := AScan( ::aControls, oXbp )
      ENDIF
   ELSE
      /*
       * Prevalidation failed.
       */
      Tone(500)
      ::setHome   := .T.

      IF ::exitState == GE_UP
         ::up()
      ELSEIF ::exitState == GE_DOWN .OR. ::lastXbp == NIL
         ::down()
      ELSE
         SetAppFocus( ::lastXbp )
      ENDIF
      ::exitState := GE_WHEN
   ENDIF
RETURN lValid


/*
 * Validation before an Xbase Part looses input focus.
 */
METHOD XbpGetController:postValidate( oXbp )
   LOCAL lValid := .T., lSave

   DEFAULT oXbp TO ::thisXbp

   IF .NOT. ::doPostValidate     .OR. ;
       oXbp        == NIL        .OR. ;
       ::exitState == GE_WHEN    .OR. ;
       ::exitState == GE_LASTGET .OR. ;
       ( ::oWindow:getFrameState() == XBPDLG_FRAMESTAT_MINIMIZED .AND. LastAppEvent() <> xbeP_Close )
      /*
       * Postvalidation must return .T. when:
       *  - no XBP has got focus in the window yet (initial display)
       *  - the window containing the XBP looses focus (another window is clicked)
       *  - focus is taken away due to a failed prevalidation
       *  - the window is minimized
       */
   ELSEIF IsMethod( oXbp, "postValidate" )
      /*
       * Prevent from recursive postvalidation when the input focus is
       * lost due to a MsgBox() displayed in the postvalidation routine.
       */
      lSave            := ::doPostvalidate
      ::doPostvalidate := .F.

      IF .NOT. oXbp:postValidate()
         Tone(1000)
         lValid      := .F.
         ::lastXbp   := oXbp
         ::exitState := GE_LASTGET

         IF oXbp == ::thisXbp
            SetAppFocus( oXbp )
         ELSEIF oXbp == ::lastXbp
            ::thisXbp  := oXbp
            ::nCurrent := AScan( ::aControls, oXbp )
            SetAppFocus( oXbp )
         ENDIF
      ENDIF
      ::doPostvalidate := lSave
   ENDIF

   IF ::exitState == GE_NOEXIT
      ::exitState := GE_WRITE
   ENDIF
RETURN lValid


/*
 * Process pending events after a failed Pre validation.
 */
METHOD XbpGetController:clearEventLoop
   LOCAL nEvent, mp1, mp2, oXbp

   DO WHILE NextAppEvent() <> xbe_None
      nEvent := AppEvent( @mp1, @mp2, @oXbp )
      oXbp:handleEvent( nEvent, mp1, mp2 )
   ENDDO
RETURN self


/*
 * Sets or retrives the current Xbase Part.
 */
METHOD XbpGetController:getActive( oXbp )
   LOCAL oOld := IIF( ::thisXbp == NIL, ::lastXbp, ::thisXbp )
   LOCAL i

   IF oOld == NIL
      oOld := SetAppFocus()
   ENDIF

   IF oXbp <> NIL .AND. ( i := AScan( ::aGetList, oXbp ) ) > 0
      ::nCurrent := AScan( ::aControls, ::aGetList[i] )
   ENDIF
RETURN oOld


/*
 * Methods for navigation.
 */
METHOD XbpGetController:first
   ::nCurrent := 1

   IF .NOT. ::aControls[ ::nCurrent ]:isVisible()
      ::down()
   ENDIF
RETURN ::Activate()


METHOD XbpGetController:last
   ::nCurrent := Len( ::aControls )

   IF .NOT. ::aControls[ ::nCurrent ]:isVisible()
      ::up()
   ENDIF
RETURN ::Activate()


METHOD XbpGetController:up
   LOCAL lExit := .F.

   DO WHILE ! lExit
      IF -- ::nCurrent < 1
         ::nCurrent := Len( ::aControls )
      ENDIF
      lExit := ::aControls[ ::nCurrent ]:isVisible() .AND.;
               ::aControls[ ::nCurrent ]:isEnabled()
   ENDDO
RETURN ::Activate()


METHOD XbpGetController:down
   LOCAL lExit := .F.

   DO WHILE ! lExit
      IF ++ ::nCurrent > Len( ::aControls )
         ::nCurrent := 1
      ENDIF
      lExit := ::aControls[ ::nCurrent ]:isVisible() .AND.;
               ::aControls[ ::nCurrent ]:isEnabled()
   ENDDO
RETURN ::Activate()


/*
 * Set input focus to the current Xbase Part.
 */
METHOD XbpGetController:activate
   IF ! Empty( ::aControls )
      ::thisXbp := ::aControls[ ::nCurrent ]
      SetAppFocus( ::thisXbp )
   ENDIF
RETURN ::thisXbp


/*
 * Modal (default) or modeless read method.
 */
METHOD XbpGetController:read( nPos, lModal )
   LOCAL nEvent, mp1, mp2, oXbp, oFocus
   LOCAL oDlg := ::owindow

   DEFAULT lModal TO .T., ;
           nPos   TO  1

   oDlg:show()

   IF Len(::aGetList) == 0
      RETURN self
   ENDIF

   ::setData()
   oFocus := ::getActive( ::aGetList[ nPos ] )

   IF lModal
      oDlg:setModalState( XBP_DISP_APPMODAL )

      ::readExit := .F.

      ::activate()
      DO WHILE .NOT. ::readExit
         nEvent := AppEvent( @mp1, @mp2, @oXbp )
         oXbp:handleEvent( nEvent, mp1, mp2 )
      ENDDO

      oDlg:setModalState( XBP_DISP_MODELESS )
      SetAppFocus( oFocus )
   ELSE
      ::activate()
   ENDIF
RETURN self


/*
 * The :close() method performs a postvalidation. If this is successful,
 * :readExit is set to .T. and the event loop in :read() is exited.
 */
METHOD XbpGetController:killRead
   IF ::oWindow <> NIL
      ::close( ::oWindow )
   ELSE
      ::readExit := .T.
   ENDIF
RETURN self


METHOD XbpGetController:getList
RETURN AClone( ::aGetList )


METHOD XbpGetController:setData
   AEval( ::aGetlist, {|o| IIf(o:dataLink==NIL, NIL, o:setData() ) } )
RETURN self


METHOD XbpGetController:getData
   AEval( ::aGetlist, {|o| o:getData() } )
RETURN self
