//////////////////////////////////////////////////////////////////////
//
//  PICKLIST.PRG
//
//  Copyright:
//      Alaska Software, (c) 1997-2025. All rights reserved.         
//  
//  Contents:
//      Class XbpPicklist which displays two listboxes for selection of
//      data. Available data is displayed in the left listbox, while
//      selected data is displayed in the right listbox. Four pushbuttons
//      are used to transfer data between both listboxes.
//   
//      The function Picklist() creates a modal dialog for easy use of the
//      XbpPicklist class.
//   
//   
//  Remarks:
//      The dialog is sizable and calculates position and size of its
//      contained controls when the user changes the size of the window.
//   
//////////////////////////////////////////////////////////////////////



#include "Appevent.ch"
#include "Common.ch"
#include "Gra.ch"
#include "Xbp.ch"


#ifdef DEBUG

PROCEDURE AppSys
// Not required here
RETURN


/*
 * Testing the Picklist() function
 */
PROCEDURE Main
   LOCAL nEvent, mp1, mp2
   LOCAL oDlg, oXbp, drawingArea, oList

   oDlg := XbpDialog():new( AppDesktop(), , {10,30}, {600,400}, , .F.)
   oDlg:taskList := .T.
   oDlg:title := "Picklist Test"
   oDlg:visible := .F.
   oDlg:maxButton := .F.
   oDlg:border := XBPDLG_DLGBORDER
   oDlg:create()

   drawingArea := oDlg:drawingArea
   drawingArea:setFontCompoundName( "8.Helv" )

   oXbp := XbpStatic():new( drawingArea, , {144,264}, {288,84} )
   oXbp:caption := "Picklist Test"
   oXbp:clipSiblings := .T.
   oXbp:setFontCompoundName( "18.Helv" )
   oXbp:setColorFG( GRA_CLR_RED )
   oXbp:options := XBPSTATIC_TEXT_VCENTER+XBPSTATIC_TEXT_CENTER
   oXbp:create()

   oList := XbpListBox():new( drawingArea, , {96,120}, {420,132} )
   oList:clipSiblings := .T.
   oList:create()

   oXbp := XbpPushButton():new( drawingArea, , {180,48}, {96,24} )
   oXbp:caption := "Picklist"
   oXbp:clipSiblings := .T.
   oXbp:create()
   oXbp:activate := {|| OpenPicklist( oList ) }

   oXbp := XbpPushButton():new( drawingArea, , {312,48}, {96,24} )
   oXbp:caption := "OK"
   oXbp:clipSiblings := .T.
   oXbp:create()
   oXbp:activate := {|| PostAppEvent( xbeP_Close ) }

   oDlg:show()
   SetAppWindow( oDlg )            
   SetAppFocus( oXbp )            

   nEvent := xbe_None
   DO WHILE nEvent <> xbeP_Close
      nEvent := AppEvent( @mp1, @mp2, @oXbp )
      oXbp:handleEvent( nEvent, mp1, mp2 )
   ENDDO
RETURN



/*
 * Open picklist and let user choose files
 */
PROCEDURE OpenPicklist( oListbox )
   LOCAL aArray := Directory( ".\*.*" )
   LOCAL cPath := CurDrive()+ ":\" + CurDir() + "\"

   Aeval( aArray, {|a| a := a[1] },,, .T. )

   aArray := PickList ( , aArray, "Picklist Dialog"  )

   oListBox:clear()

   AEval( aArray, {|c| oListbox:addItem( cPath + c ) } )
RETURN

#endif  // DEBUG



/*
 * Wrapper function which displays a modal Picklist dialog
 * It returns the selected items from the picklist
 */
FUNCTION Picklist( oOwner, aArray, cTitle, cAvailText, cSelectText, oHelpLink )
   LOCAL nEvent, mp1, mp2, oXbp, oDlg, oFocus

   DEFAULT oOwner TO SetAppWindow(), ;
           cTitle TO ""

   oDlg := XbpPicklistDialog():new( AppDeskTop(), oOwner,,,, .F. )
   oDlg:title    := cTitle
   oDlg:taskList := .F.

   IF Valtype( cAvailText ) == "C"
      oDlg:picklist:leftText:caption := cAvailText
   ENDIF

   IF Valtype( cSelectText ) == "C"
      oDlg:picklist:rightText:caption := cSelectText
   ENDIF

   IF Valtype( oHelpLink ) == "O"
      oDlg:helpLink := oHelpLink
   ENDIF

   oDlg:create()

   oDlg:pickList:setData( aArray )

  /*
   * Display dialog centered on its owner, switch to modal state and
   * let user select data within the event loop.
   */
   MoveToOwner( oDlg )
   oDlg:show()
   oDlg:setModalState( XBP_DISP_APPMODAL )
   oFocus := SetAppFocus( oDlg:pickList:leftListBox )

   DO WHILE nEvent <> xbeP_Close
      nEvent := AppEvent( @mp1, @mp2, @oXbp )
      oXbp:handleEvent( nEvent, mp1, mp2 )
   ENDDO

   aArray := oDlg:selectedItems

   oDlg:setModalState( XBP_DISP_MODELESS )
   oDlg:hide()
   SetAppFocus( oFocus )
   oDlg:destroy()


RETURN aArray



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
        /*
         * Adjust for thickness of the owner window's frame
         */
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



/*
 * Dialog class which displays a Picklist object and pushbuttons
 * for selection of data.
 */
CLASS XbpPicklistDialog FROM XbpDialog
   PROTECTED:
   VAR groupMgr

   EXPORTED:
   METHOD init, create, resize
   VAR pickList READONLY
   VAR selectedItems
ENDCLASS


/*
 * Initialize object
 */
METHOD XbpPickListDialog:init( oParent, oOwner, aPos, aSize, aPP, lVisible )

   DEFAULT oParent TO AppDesktop()  , ;
           oOwner  TO SetAppWindow(), ;
           aSize   TO { 400, 300 }  , ;
           aPos    TO { 30 , 30 }

   ::xbpDialog:init( oParent, oOwner, aPos, aSize, aPP, lVisible )

   ::pickList  := XbpPickList():new( ::drawingArea )
   ::groupMgr  := GroupManager():new( ::drawingArea, , {72,24})
   ::selectedItems := {}

RETURN self



/*
 * Create the object and calculate minimum size of dialog
 */
METHOD XbpPickListDialog:create( oParent, oOwner, aPos, aSize, aPP, lVisible )
   LOCAL aMinSize

   ::xbpDialog:create( oParent, oOwner, aPos, aSize, aPP, lVisible )
   ::drawingArea:setFontCompoundName( "8.Helv" )

   ::groupMgr:create()

   ::picklist:create()

   // for a modeless dialog, user defined events must be used here
   // instead of xbeP_Close
   ::groupMgr:createButton( "Ok", {|| ::selectedItems := ::pickList:getData() , ;
                                   PostAppEvent( xbeP_Close,,, self ) }, .T., .T. )
   ::groupMgr:createButton( "Cancel", {|| ::selectedItems := {} , ;
                                   PostAppEvent( xbeP_Close,,, self ) }, .T., .T. )

   IF ::helpLink <> NIL
      ::groupMgr:createButton( "Help", {|| PostAppEvent( xbeP_HelpRequest,,, self) }, .T., .T. )
   ENDIF
   ::groupMgr:setPos( {12,12} )
   ::picklist:setPos( {12,48} )

  /*
   * Calculate minimum size for dialog window
   */
   aSize     := ::pickList:minSize()

   aSize[1]  += ( 12 + 12 )             // 2 * 12 pixels distance
   aSize[2]  += ( 12 + 24 + 12 + 12 )   // 48 = y coord of line

   aMinSize  := ::groupMgr:minSize()
   aSize[1]  := Max( aSize[1], aMinSize[1] )
   aSize[2]  := Max( aSize[2], aMinSize[2] )

   aMinSize  := ::calcFrameRect( { 0, 0, aSize[1], aSize[2] } )
   ::minSize := { aMinSize[3], aMinSize[4] }
   aSize     := ::currentSize()
   aSize[1]  := Max( aSize[1], ::minSize[1] )
   aSize[2]  := Max( aSize[2], ::minSize[2] )

   // Call :resize() method to recompute the layout
   ::resize()
   
RETURN self



/*
 * Reposition contained controls after resize message
 */
METHOD XbpPickListDialog:resize()
   LOCAL aSize := ::drawingArea:currentSize()

   ::groupMgr:center( aSize[1]-24, 12 )

   // Change the size hidden
   ::picklist:setSize( { aSize[1] - ( 12 + 12 ), ;
                         aSize[2] - ( 12 + 24 + 12 + 12 ) } , .F. )

   // :invalidateRect() triggers an xbeP_Paint event
   ::drawingArea:invalidateRect( { 1, 1, aSize[1], aSize[2] } )
RETURN self



/*
 * Picklist class with two listboxes for selection of data.
 * Four pushbuttons are used to transfer items between left and right
 * listbox. The pushbuttons are automatically positioned by a
 * GroupManager object
 */
CLASS XbpPickList FROM XbpStatic, DataRef

   EXPORTED:
   VAR    groupMgr
   VAR    leftText
   VAR    rightText
   VAR    leftListBox
   VAR    rightListBox

   METHOD init, create
   METHOD setSize, minSize
   METHOD checkButtonState
   METHOD transferItems
   METHOD setData, getData

ENDCLASS


/*
 * Initialize listboxes und and group manager
 */
METHOD XbpPickList:init( oParent, oOwner, aPos, aSize, aPP, lVisible )

   ::xbpStatic:init( oParent, oOwner, aPos, aSize, aPP, lVisible )
   ::xbpStatic:type := XBPSTATIC_TYPE_RAISEDBOX
   ::xbpStatic:clipSiblings :=.T.

   ::groupMgr            :=  GroupManager():new( self, self, {28,28} )
   ::groupMgr:horizontal := .F.

   ::leftText            := XbpStatic():new( self, , {0,0}, {0 ,16} )
   ::leftText:caption    := "Available items"
   ::leftText:type       := XBPSTATIC_TYPE_TEXT
   ::leftText:options    := XBPSTATIC_TEXT_VCENTER+XBPSTATIC_TEXT_LEFT

   ::rightText           := XbpStatic():new( self, , {0,0}, {0 ,16} )
   ::rightText:caption   := "Selected items"
   ::rightText:type      := XBPSTATIC_TYPE_TEXT
   ::rightText:options   := XBPSTATIC_TEXT_VCENTER+XBPSTATIC_TEXT_LEFT

   ::leftListBox         := XbpListBox():new( self )
   ::rightListBox        := XbpListBox():new( self )

   ::leftListBox :markMode := XBPLISTBOX_MM_EXTENDED
   ::rightListBox:markMode := XBPLISTBOX_MM_EXTENDED
   ::leftListBox :tabstop  := .T.
   ::rightListBox:tabstop  := .T.

RETURN self



/*
 * Add four buttons and create the object
 */
METHOD XbpPickList:create( oParent, oOwner, aPos, aSize, aPP, lVisible )

   ::xbpStatic:create( oParent, oOwner, aPos, aSize, aPP, lVisible )

   ::groupMgr:create()

   ::leftText:create()
   ::rightText:create()

   ::leftListBox:create()
   ::rightListBox:create()

   ::groupMgr:createButton( ">" , {|| ::transferItems( ::leftListBox, ::rightListBox, .F. )  }, .F., .F. )
   ::groupMgr:createButton( ">>", {|| ::transferItems( ::leftListBox, ::rightListBox, .T. )  }, .F., .F. )
   ::groupMgr:createButton( "<" , {|| ::transferItems( ::rightListBox, ::leftListBox, .F. )  }, .F., .F. )
   ::groupMgr:createButton( "<<", {|| ::transferItems( ::rightListBox, ::leftListBox, .T. )  }, .F., .F. )

   ::setSize( ::xbpStatic:currentSize() )

RETURN self



/*
 * Change size of XbpStatic and reposition contained controls
 */
METHOD XbpPickList:setSize( aSize, lPaint )
   LOCAL aLbSize[2], aPos

   DEFAULT lPaint TO .T.
   ::xbpStatic:setSize( aSize, lPaint )

  /*
   * Calculate width and height of the listboxes
   */
   aLbSize[1] := aSize[1] - 2 - 12 - 28 - 12 - 2
   aLbSize[1] /= 2
   aLbSize[2] := aSize[2] - 2 - 16 - 2 - 2

   aPos := {2,2}
   ::leftListBox:setPos ( aPos, lPaint )
   ::leftListBox:setSize( aLbSize, lPaint )

   ::leftText:setPos ( { aPos[1], 2+aLbSize[2]+2}, lPaint )
   ::leftText:setSize( {aLbSize[1],16}, lPaint )


  /*
   * Calculate position of the four pushbuttons and let the
   * GroupManager center them
   */
   aPos[1] += ( aLbSize[1] + 12 )

   ::groupMgr:setPos( aPos, lPaint )
   ::groupMgr:center( aSize[2]-2-16, 2 )

  /*
   * Position of the right listbox
   */
   aPos[1] += ( 28 + 12 )

   ::rightListBox:setPos( aPos, lPaint )
   ::rightListBox:setSize( aLbSize, lPaint )

   ::rightText:setPos ( { aPos[1], 2+aLbSize[2]+2}, lPaint )
   ::rightText:setSize( {aLbSize[1],16}, lPaint )

RETURN self



/*
 * Minimum size of an XbpPicklist object
 */
METHOD XbpPickList:minSize( )
   LOCAL aSize := ::groupMgr:minSize()

   aSize[1] += (  2 * (40 + 12 + 2) )
   aSize[2] += ( 16 + 2 + 2 )

RETURN aSize



/*
 * Toggle disabled/enabled state of pushbuttons
 */
METHOD XbpPickList:checkButtonState
   LOCAL nCount1 := ::leftListBox:numItems()
   LOCAL nCount2 := ::rightListbox:numItems()

   IF nCount1 > 0
      ::groupMgr:part(1):enable()
   ELSE
      ::groupMgr:part(1):disable()
   ENDIF

   IF nCount1 > 1
      ::groupMgr:part(2):enable()
   ELSE
      ::groupMgr:part(2):disable()
   ENDIF

   IF nCount2 > 0
      ::groupMgr:part(3):enable()
   ELSE
      ::groupMgr:part(3):disable()
   ENDIF

   IF nCount2 > 1
      ::groupMgr:part(4):enable()
   ELSE
      ::groupMgr:part(4):disable()
   ENDIF

RETURN self



/*
 * Transfer marked items or all items between both listboxes
 */
METHOD XbpPickList:transferItems( oSender, oReceiver, lAll )
   LOCAL aBuffer, aItem, nItem, i, nCount

   DEFAULT oSender   TO ::leftListBox  , ;
           oReceiver TO ::rightListbox , ;
           lAll      TO .F.

   IF ! lAll
      aItem := oSender:getData()
   ELSE
      aItem := AEval( Array(oSender:numItems()), {|x,i| x:=i },,, .T. )
   ENDIF

   nCount := 0
   IF lAll
      nCount  := oSender:numItems()
      aBuffer := AClone( oSender:cargo )
      ASize( oSender:cargo, 0 )
      oSender:clear()

   ELSEIF ! Empty( aItem )
      nCount  := Len( aItem )
      aBuffer := Array( nCount )

     /*
      * Remove items from bottom to top. Otherwise their ordinal
      * position changes.
      */
      FOR i := nCount TO 1 STEP -1
         nItem      := aItem[i]
         aBuffer[i] := oSender:cargo[ nItem ]
         ADel( oSender:cargo, nItem )
         oSender:delItem( nItem )
      NEXT
      ASize( oSender:cargo, oSender:numItems() )
      aItem[1] := Min( aItem[1], oSender:numItems() )
      oSender:setData( {aItem[1]}, .T. )
   ENDIF

  /*
   * Find insertion point
   */
   aItem := oReceiver:getData()
   IF ! Empty( aItem )
      oReceiver:setData( aItem, .F. )
      nItem := ATail( aItem )
   ELSE
      nItem := 0
   ENDIF

  /*
   * insert after last marked element
   */
   FOR i:=1 TO nCount
      nItem ++

      AAdd( oReceiver:cargo, NIL )              // add element to data array

      IF nItem > oReceiver:numItems()
         oReceiver:addItem( aBuffer[i] )        // add item to listbox
      ELSE
         AIns( oReceiver:cargo, nItem )         // insert string to data array
         oReceiver:insItem( nItem, aBuffer[i] ) // insert item to listbox
      ENDIF

      oReceiver:cargo[ nItem ] := aBuffer[i]
   NEXT

   oReceiver:setData( { nItem }, .T. )

   ::checkButtonState()

RETURN self



/*
 * Transfer array to left listbox and clear right listbox
 */
METHOD XbpPickList:setdata( aArray )

   IF aArray == NIL
      aArray := Eval( ::dataLink )
   ENDIF

   ::leftListBox:clear()
   ::leftListBox:cargo := AClone( aArray )

   AEval( aArray, {|c| ::leftListBox:addItem( c ) } )

   ::rightListBox:clear()
   ::rightListBox:cargo := {}

   ::checkButtonState()

RETURN 



/*
 * Clone items listed in right listbox and eval datalink
 */
METHOD XbpPickList:getData()
   LOCAL aItems := AClone( ::rightListBox:cargo )

RETURN IIf( ::dataLink <> NIL, Eval( ::dataLink, aItems ), aItems )
