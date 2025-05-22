//////////////////////////////////////////////////////////////////////
//
//  ARRAYBOX.PRG
//
//  Copyright:
//      Alaska Software, (c) 1997-2025. All rights reserved.         
//  
//  Contents:
//   
//  Syntax:
//      ChoiceBox( [<oOwner>], [<aPos>], [<aSize>], ;
//                 <aStrings>, [<cTitle>], [<nMarkMode>] ) --> aSelected
//   
//  Parameters:
//      <oOwner>    - The window which becomes inaccessible while the selection
//                    window is open. Default is SetAppWindow()
//   
//      <aPos>      - Position relative to <oOwner>.
//                    It defaults to a centered position
//   
//      <aSize>     - Size of the selection window. The default is calculated
//                    from the length of <aStrings>
//   
//      <sStrings>  - Array with character strings to be displayed in the listbox
//   
//      <cTitle>    - Title for the selection window
//   
//      <nMarkMode> - XBPLISTBOX_MM_EXTENDED -> multiple selection is possible
//                    Default is XBPLISTBOX_MM_SINGLE
//   
//  Return:
//      An array with numeric values is returned. Each element indicates
//      a selected element of <aStrings>.
//   
//  Remarks:
//      ChoiceBox() requires that XbpDialog is used for the application window.
//   
//////////////////////////////////////////////////////////////////////


#include "Appevent.ch"
#include "Common.ch"
#include "Font.ch"
#include "Gra.ch"
#include "Xbp.ch"

PROCEDURE AppSys
   LOCAL oDlg, aPos[2], aSize

   aSize    := SetAppWindow():currentSize()
   aPos[1]  := 0.1 * aSize[1]
   aPos[2]  := 0.1 * aSize[2]
   aSize[1] := 640
   aSize[2] := 400

   oDlg       := XbpDialog():new( ,, aPos, aSize,, .F. )
   oDlg:title := "Application window"
   oDlg:close := {|| oDlg:destroy() }
   oDlg:create()
   SetAppWindow( oDlg )

   oDlg:show()
   SetAppFocus( oDlg )
RETURN

PROCEDURE Main
   LOCAL aArray := Directory()
   LOCAL aSelect

   AEval( aArray, {|a| a := a[1] },,, .T. )

   ChoiceBox( ,,, aArray, "Single Choice" )
   aSelect := ChoiceBox( ,{50,10},{300,150}, aArray, "Multiple Choice", XBPLISTBOX_MM_EXTENDED )

   AEval( aSelect, {|x| x := aArray[x] },,, .T. )
   ChoiceBox( ,,, aSelect, "Selected" )
RETURN




/*
 * Modal window which displays a listbox for single or multiple selection
 */
FUNCTION ChoiceBox( oOwner, aPos, aSize, aStrings, cTitle, nMarkMode )
   LOCAL nEvent := NIL, mp1 := NIL, mp2 := NIL, oXbp := NIL
   LOCAL oDeskTop , oDlg      , oFocus
   LOCAL oBtnCancel, oListBox , oBtnOK := NIL
   LOCAL bKeyboard, aSelect
   LOCAL nXsize   , nYsize

   DEFAULT oOwner    TO SetAppWindow(), ;
           cTitle    TO " "           , ;
           nMarkMode TO XBPLISTBOX_MM_SINGLE

  /*
   * Create a dialog window to host the listbox
   */
   oDesktop       := AppDesktop()

   oDlg           := XbpDialog():new( oDeskTop, oOwner, , , , .F. )
   oDlg:taskList  := .F.
   oDlg:sysMenu   := .F.
   oDlg:minButton := .F.
   oDlg:maxButton := .F.
   oDlg:title     := cTitle
   oDlg:border    := XBPDLG_SIZEBORDER
   oDlg:resize    := {|mp1,mp2,obj| Resize( obj ) }
   oDlg:create()
   oDlg:drawingArea:SetFontCompoundName( FONT_HELV_SMALL )


  /*
   * Create the listbox and use keyhandler code block
   */
   bKeyboard               := {|nKey,x,obj| KeyHandler( nKey, obj, oDlg ) }
   oListBox                := XbpListBox():new( oDlg:drawingArea, , { 8,48} )
   oListBox:tabStop        := .T.
   oListBox:adjustHeight   := .T.
   oListBox:horizScroll    := .T.
   oListBox:markMode       := nMarkMode
   oListBox:keyBoard       := bKeyboard
   oListBox:itemSelected   := {|| PostAppEvent( xbeP_Activate,,, oBtnOk) }
   oListBox:create()
   oListBox:setFontCompoundName( FONT_DEFFIXED_MEDIUM )

   AEval( aStrings, {|c| oListBox:addItem( c ) } )
   oListBox:setData( {1}, .T. )


  /*
   * Create two pushbuttons
   */
   oBtnOk              := XbpPushButton():new( oDlg:drawingArea,,, { 72, 24 } )
   oBtnOk:caption      := "Ok"
   oBtnOk:tabStop      := .T.
   oBtnOk:keyBoard     := bKeyboard
   oBtnOk:activate     := {|| aSelect := oListBox:getData()   , ;
                              PostAppEvent( xbeP_Close,,, oDlg) }
   oBtnOk:create()

   oBtnCancel          := XbpPushButton():new( oDlg:drawingArea,, {72,0}, { 72, 24 } )
   oBtnCancel:caption  := "Cancel"
   oBtnCancel:tabStop  := .T.
   oBtnCancel:keyBoard := bKeyboard
   oBtnCancel:activate := {|| PostAppEvent( xbeP_Close,,, oDlg) }
   oBtnCancel:create()


  /*
   * Set the size of the window and define minimum size
   */
   nXsize  := 144 + 16
   nYsize  := 120 + 48
   mp1     := oDlg:calcFrameRect( { 0, 0, nXsize, nYsize } )
   oDlg:minSize := { mp1[3], mp1[4] }

   IF aSize == NIL
      nYsize := Max( 120 , Min( 240, 12 * Len( aStrings ) ) ) + 48
      nXsize := 144 + 24
      aSize  := oDlg:calcFrameRect( { 0, 0, nXsize, nYsize } )
      aSize  :=  { aSize[3], aSize[4] }
   ELSE
      aSize[1] := Max( aSize[1], oDlg:minSize[1] )
      aSize[2] := Max( aSize[2], oDlg:minSize[2] )
   ENDIF

   oDlg:setSize( aSize )
   MoveToOwner( oDlg, aPos )
   Resize( oDlg )


  /*
   * Switch dialog to modal. The event loop is required since the
   * function is to return a value.
   */
   oDlg:setModalState( XBP_DISP_APPMODAL )
   oDlg:show()
   oFocus := SetAppFocus( oListBox )

   aSelect := {}

   DO WHILE nEvent <> xbeP_Close
      nEvent := AppEvent( @mp1, @mp2, @oXbp )
      oXbp:handleEvent( nEvent, mp1, mp2 )
   ENDDO


  /*
   * Turn off modal state, release the dialog resources and set focus back
   */
   oDlg:setModalState( XBP_DISP_MODELESS )
   oDlg:destroy()
   SetAppFocus( oFocus )

RETURN aSelect



/*
 * Resize and rearrange the contents of the dialog window. This
 * procedure is called via the :resize code block which is evaluated
 * each time the user resizes the window.
 */
STATIC PROCEDURE Resize( oDlg )
   LOCAL aDaSize    := oDlg:drawingArea:currentSize()
   LOCAL aChild     := oDlg:drawingArea:childList()
   LOCAL oListBox   := aChild[1]
   LOCAL oBtnOk     := aChild[2]
   LOCAL oBtnCancel := aChild[3]
   LOCAL aPos, aPbSize, aLbSize, nDist

   aPbSize := oBtnOK:currentSize()

  /*
   * Allow for variable distance between pushbuttons
   */
   nDist   := Min( 24, Max( 0, aDaSize[1] - 16 - 2 * aPbSize[1] ) )


  /*
   * Center both pushbuttons
   */
   aPos    := CenterPos( { nDist + 2 * aPbSize[1] ,     aPbSize[2] }, ;
                         { aDaSize[1]             , 2 * aPbSize[2] }  )

   oBtnOk:setPos( aPos, .F. )
   aPos[1] += ( nDist + aPbSize[1] )
   oBtnCancel:setPos( aPos, .F. )


  /*
   * The listbox is centered with 8 pixels distance to the dialog frame
   */
   aLbSize := { aDaSize[1] - 16, aDaSize[2] - 8 - 2 * aPbSize[2] }
   oListBox:setSize( aLbSize, .F. )

   oDlg:drawingArea:invalidateRect()
RETURN



/*
 * React to Return and Esc keys
 */
STATIC PROCEDURE KeyHandler( nKey, oXbp, oDlg )

   DO CASE
   CASE nKey == xbeK_RETURN
      IF oXbp:isDerivedFrom( "XbpPushbutton" )
         PostAppEvent( xbeP_Activate,,, oXbp )
      ENDIF

   CASE nKey == xbeK_ESC
      PostAppEvent( xbeP_Close,,, oDlg )

   ENDCASE

RETURN



/*
 * Move a window within its parent according to the origin of
 * its owner window. Default position is centered on the owner.
 */
STATIC PROCEDURE MoveToOwner( oDlg, aPos )
   LOCAL oOwner  := oDlg:setOwner()
   LOCAL oParent := oDlg:setParent()
   LOCAL aPos1, nWidth

   DEFAULT aPos TO CenterPos( oDlg:currentSize(), oOwner:currentSize() )

   DO WHILE oOwner <> oParent
      aPos1   := oOwner:currentPos()

      aPos[1] += aPos1[1]
      aPos[2] += aPos1[2]

      IF oOwner:isDerivedFrom( "XbpDialog" )
         // adjust for thickness of frame
         nWidth  := ( oOwner:currentSize()[1] - oOwner:drawingArea:currentSize()[1] ) / 4
         aPos[1] -= nWidth
         aPos[2] -= nWidth
      ENDIF
      oOwner := oOwner:setParent()
   ENDDO

   oDlg:setPos( aPos )
RETURN



/*
 * Calculate the center position from size and reference size
 */
STATIC FUNCTION CenterPos( aSize, aRefSize )
RETURN { Int( (aRefSize[1] - aSize[1]) / 2 ) ;
       , Int( (aRefSize[2] - aSize[2]) / 2 ) }
