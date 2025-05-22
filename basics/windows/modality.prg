//////////////////////////////////////////////////////////////////////
//
//  MODALITY.PRG
//
//  Copyright:
//      Alaska Software, (c) 1997-2025. All rights reserved.         
//  
//  Contents:
//      The program shows the difference between modal and modeless windows.
//   
//      It also shows  programming techniques of how to return data
//      edited in a dialog window to the program. If the dialog is modal,
//      a user defined function can be used. For modeless dialogs, however,
//      the function PostAppEvent() must be used together with user defined
//      events.
//   
//////////////////////////////////////////////////////////////////////


#include "Appevent.ch"
#include "Font.ch"
#include "Gra.ch"
#include "Xbp.ch"

STATIC saStack := {}

  /*
   * User defined event for modeless dialog window
   */

#define xbeP_ModelessClosed           xbeP_User + 100

/*
 * Pushbuttons open another dialog window in modal or modeless state
 */
PROCEDURE Main
   LOCAL nEvent := NIL, mp1 := NIL, mp2 := NIL, oXbp := NIL

   SetColor( "N/W" )
   CLS

   oXbp          := XbpPushbutton():new(,,{12,12}, {100,30} )
   oXbp:caption  := "Open modal"
   oXbp:activate := {|| Modal() }
   oXbp:TabStop  := .T.
   oXbp:create()
   oXbp:setFontCompoundName( FONT_HELV_SMALL )

   oXbp          := XbpPushbutton():new(,,{124,12}, {100,30} )
   oXbp:caption  := "Open modeless"
   oXbp:activate := {|| Modeless() }
   oXbp:TabStop  := .T.
   oXbp:create()
   oXbp:setFontCompoundName( FONT_HELV_SMALL )

   oXbp          := XbpPushbutton():new(,,{236,12}, {100,30} )
   oXbp:caption  := "Quit"
   oXbp:activate := {|| PostAppEvent( xbeP_Quit ) }
   oXbp:TabStop  := .T.
   oXbp:create()
   oXbp:setFontCompoundName( FONT_HELV_SMALL )

   /*
    * NOTE: The variable oXbp is used as temporary storage until the
    *       Xbase Part is created. After :create() the XBP is referenced
    *       in the child list of its parent. Therefore, oXbp can be
    *       recycled to create the next Xbase Part.
    */

   DO WHILE nEvent <> xbeP_Quit
      nEvent := AppEvent( @mp1, @mp2, @oXbp )
      oXbp:handleEvent( nEvent, mp1, mp2 )

      IF nEvent == xbeP_ModelessClosed
         DispoutAt( 0, 0, PadR( "Modeless dialog returned: " + mp1, 80 ) )
         IF mp2 <> NIL
            SetAppFocus( mp2 )
         ELSE
            SetAppFocus( SetAppWindow() )
         ENDIF
      ENDIF
   ENDDO

RETURN



/*
 * Procedure gets data from a modal dialog
 */
PROCEDURE Modal()
   STATIC snCount := 1
   LOCAL cVar := "Change this data #" + Ltrim( Str( snCount++ ) )

   DispoutAt( 0, 0, PadR( "Modal dialog opens", 80 ) )

   cVar := ModalDialog( cVar )

   DispoutAt( 0, 0, PadR( "Modal dialog returned: " + cVar, 80 ) )
RETURN



/*
 * Wrapper function for modeless dialog
 * Note: The second DispoutAt() call from above is removed because the
 *       dialog window stays open
 */
PROCEDURE Modeless()
   STATIC snCount := 1
   LOCAL cVar := "Change this data #" + Ltrim( Str( snCount++ ) )

   DispoutAt( 0, 0, PadR( "Modeless dialog opens", 80 ) )

   ModelessDialog( cVar )

RETURN



/*
 * The function demonstrates how to use a modal dialog.
 * Since the dialog is modal, data can be returned from the function.
 */
FUNCTION Modaldialog( cString )
   LOCAL oDlg, oSle, oXbp, oFocus, aPos, aSize
   LOCAL nXsize := 240
   LOCAL nYsize := 180

  /*
   * Get size of desktop window
   */
   aSize := AppDeskTop():currentSize()

  /*
   * Calculate center position for new dialog
   */
   aPos  := { ( aSize[1]-nXsize )/2, ( aSize[2]-nYsize ) / 2 }

  /*
   * Create dialog window with different parent and owner windows
   */
   oDlg           := XbpDialog():new( AppDesktop(), SetAppWindow(), aPos, {nXsize,nYsize} )
   oDlg:title     := "Modal dialog"
   oDlg:taskList  := .T.
   oDlg:minButton := .F.
   oDlg:maxButton := .F.
   oDlg:create()
   oDlg:drawingArea:setFontCompoundName( FONT_HELV_SMALL )

  /*
   * Get size of drawingArea
   */
   aSize := oDlg:drawingArea:currentSize()

  /*
   * Calculate center position for Single Line Edit field and create it
   *   20 = height of Sle
   */
   aPos := { 12, ( aSize[2] - 20 ) / 2 }
   oSle := XbpSle():new( oDlg:drawingArea,, aPos, { aSize[1] - 24, 20 } )
   oSle:TabStop  := .T.
   oSle:create()
   oSle:setData( cString )

  /*
   * Calculate center position for pushbutton and create it.
   * Pressing the pushbutton resets the display state of the 
   * modal dialog, which automatically closes the dialog.
   */
   aPos          := { ( aSize[1]-72 ) / 2, 12 }
   oXbp          := XbpPushbutton():new( oDlg:drawingArea, , aPos, {72,24} )
   oXbp:caption  := "OK"
   oXbp:activate := {|| oDlg:modalResult := XBP_MRESULT_OK }
   oXbp:TabStop  := .T.
   oXbp:create()

  /*
   * Save XBP which has focus and set focus to Sle
   */
   oFocus := SetAppFocus( oSle )

  /*
   * Display dialog as modal window 
   * IMPORTANT: This disables the owner of the dialog
   *            which is SetAppWindow()
   */
   oDlg:showmodal()

  /*
   * get data from Sle, release dialog window and
   * set focus back to previuos XBP
   */
   cString := oSle:getData()
   oDlg:destroy()
   SetAppFocus( oFocus )

RETURN cString



/*
 * The procedure demonstrates how to use a modeless dialog
 * Since the dialog is modeless, no data can be returned
 */
PROCEDURE ModelessDialog( cString )
   LOCAL oDlg, oSle, oXbp, aPos, aSize
   LOCAL nXsize := 240
   LOCAL nYsize := 180

  /*
   * Get size of desktop window
   */
   aSize    := AppDesktop():currentSize()

  /*
   * Calculate center position for new dialog
   */
   aPos  := { ( aSize[1]-nXsize )/2, ( aSize[2]-nYsize ) / 2 }

  /*
   * Create dialog window with same parent and owner windows
   */
   oDlg          := XbpDialog():new( AppDesktop(),, aPos, {nXsize,nYsize} )
   oDlg:title    := "Modeless dialog"
   oDlg:taskList  := .T.
   oDlg:minButton := .F.
   oDlg:maxButton := .F.
   oDlg:create()
   oDlg:drawingArea:setFontCompoundName( FONT_HELV_SMALL )

  /*
   * The dialog window must react now to the xbeP_Close event
   */
   oDlg:close := {|mp1,mp2,obj| CloseModelessDialog( obj ) }
   AAdd( saStack, oDlg )

  /*
   * Get size of drawingArea
   */
   aSize := oDlg:drawingArea:currentSize()

  /*
   * Calculate center position for Single Line Edit field and create it
   *   20 = height of Sle
   */
   aPos := { 12, ( aSize[2] - 20 ) / 2 }
   oSle := XbpSle():new( oDlg:drawingArea,, aPos, { aSize[1] - 24, 20 } )
   oSle:TabStop  := .T.
   oSle:create()
   oSle:setData( cString )

  /*
   * Calculate center position for pushbutton and create it
   */
   aPos          := { ( aSize[1]-72 ) / 2, 12 }
   oXbp          := XbpPushbutton():new( oDlg:drawingArea, , aPos, {72,24} )
   oXbp:caption  := "OK"
   oXbp:TabStop  := .T.
   oXbp:create()

  /*
   * Pushbutton posts the xbeP_Close event to the dialog window
   */
   oXbp:activate := {|| PostAppEvent( xbeP_Close,,, oDlg ) }

  /*
   * set focus to Sle
   */
   SetAppFocus( oSle )

RETURN



/*
 * The procedure is used to close a modeless dialog window.
 * For data processing within the Main event loop, a user defined event
 * is posted with data attached as first message parameter.
 */
PROCEDURE CloseModelessDialog( oDlg )
   LOCAL oSle, cData

  /*
   * XbpSle is created first. Therefore it is referenced in the first
   * element of the drawing area's child list
   */
   oSle := oDlg:drawingArea:childList()[1]

  /*
   * Retrieve data from edit buffer
   */
   cData := oSle:getData()

  /*
   * Release dialog window and contained controls
   */

   ADel( saStack, AScan( saStack, oDlg ) )
   ASize( saStack, Len( saStack ) - 1 )
   oDlg:destroy()

  /*
   * Post user defined event and attach edited data to it. cData appears
   * in the Main event loop when AppEvent() is called. It gets assigned
   * to the first message parameter mp1. The second message parameter
   * receives the next window that is to receive the focus.
   */
   PostAppEvent( xbeP_ModelessClosed, cData, ATail( saStack ) )

RETURN
