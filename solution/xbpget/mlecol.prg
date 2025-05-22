//////////////////////////////////////////////////////////////////////
//
//  MLECOL.PRG
//
//  Copyright:
//      Alaska Software, (c) 1999-2025. All rights reserved.         
//  
//  Contents:
//      This example implements the XbpMleColumn class that can be used
//      for display and editing of memo fields in an XbpBrowse object.
//   
//      The Mle is provided by the MemoEditDialog class, which is also
//      implemented in this file.
//   
//  Remarks:
//     
//      The following programming techniques are demonstrated:
//     
//      1. How to subclass from the XbpGetColumn class in order to use
//         another Xbase Part than XbpGet for editing in the browser.
//   
//      2. How to exchange the :dataLink code block "secretly" so that
//         one code block is used for data display with XbpColumn and the
//         other is used for editing with XbpMle.
//   
//////////////////////////////////////////////////////////////////////


#include "Appevent.ch"
#include "Common.ch"
#include "Xbp.ch"


CLASS XbpMleColumn FROM XbpGetColumn
   PROTECTED:
   VAR    MemoDlg IS xbpGet IN XbpGetColumn
   VAR    _dataLink
   METHOD setPosAndSize

   EXPORTED:   
   METHOD create
   METHOD createXbp
   METHOD firstMemoLine
   METHOD hiliteRow
ENDCLASS


/*
 * Replace the :dataLink code block before the object is :create()ed.
 * The original :dataLink code block is preserved and used later for
 * XbpMLE. The new code block retrieves the first line of text from the
 * memo field.
 */
METHOD XbpMleColumn:create( p1, p2, p3, p4, p5, p6 )
   ::bufferLength := 40
   ::_dataLink    := ::dataLink
   ::dataLink     := {|| ::firstMemoLine() }
   ::xbpGetColumn:create( p1, p2, p3, p4, p5, p6 )
RETURN self


METHOD XbpMleColumn:firstMemoLine
RETURN MemoLine( Eval( ::_dataLink ), ::bufferLength, 1 )


/*
 * Create a MemoEditDialog instead of the XbpGet object and assign the
 * original :dataLink code block which accesses the memo field.
 *
 * Note 1: the instance variable :MemoDlg is actually declared as
 *         :xbpGet in the super class, but renamed in this class.
 *         This makes the code better readable.
 *
 * Note 2: The owner of the MemoEditDialog is the window that contains the
 *         browser. The parent of the MemoEditDialog is the same as for the
 *         browser window. If the browser window is moved, the MemoEditDialog
 *         window is moved as well.
 */
METHOD XbpMleColumn:createXbp
   LOCAL oOwner := ::setParent()
   LOCAL oParent, aSize := { 220, 180 }

   DO WHILE .NOT. ( oOwner:IsDerivedFrom( "XbpDialog" ) .OR. oOwner:IsDerivedFrom( "XbpCrt" ) )
      oOwner := oOwner:setParent()
   ENDDO

   IF oOwner:isDerivedFrom( "XbpCrt" )
      oParent := AppDesktop()
   ELSE
      oParent := oOwner:setParent()
   ENDIF

   aSize[1]                := ::currentSize()[1]
   ::MemoDlg               := MemoEditDialog():new( oParent, oOwner,, aSize,, .F. )
   ::MemoDlg:controller    := self
   ::MemoDlg:dataLink      := ::_dataLink
   ::MemoDlg:moveWithOwner := .T.
   ::MemoDlg:title         := ::heading:getCell(1)
   ::MemoDlg:create()
RETURN self


/*
 * Overloaded from XbpGetColumn: only the position of the MemoEditDialog window
 * changes with the browse cursor, not the size
 *
 * Note: The position of the browse cursor cannot be directly used for
 *       positioning of the MemoEditDialog window. The coordinates must be
 *       transformed to the origin of the coordinate system that is
 *       valid for the browser window.
 */
METHOD XbpMleColumn:setPosAndSize( nRowPos )
   LOCAL aRect  := ::dataArea:cellRect( nRowPos )
   LOCAL aPos   := { aRect[1], aRect[2] + ( aRect[4]-aRect[2] ) }
   LOCAL aSize  := ::MemoDlg:currentSize()
   LOCAL oOwner := ::MemoDlg:setOwner()

   aPos[1] += ::currentPos()[1] + ::setParent():currentPos()[1]
   aPos[2] += ::footing:currentSize()[2]

   aPos[1] += ::browser:currentPos()[1]
   aPos[2] += ::browser:currentPos()[2]

   aPos[1] += oOwner:currentPos()[1]
   aPos[2] += oOwner:currentPos()[2]

   aPos[1] := Max( 0, Min( aPos[1], oOwner:setParent():currentSize()[1]-aSize[1] ) )
   aPos[2] := Max( 0, aPos[2]-aSize[2] )

   ::MemoDlg:setPos( aPos )
RETURN self


/*
 * Overloaded from XbpGetColumn: The MemoEditDialog window remains visible
 * during (de)hilighting.
 */
METHOD XbpMleColumn:hiliteRow( nRowPos, lHilite, lFrame, lRepaint )
   DEFAULT nRowPos TO ::browser:rowPos

   IF ::active
      IF lHilite
         ::read( nRowPos )
      ELSE
         IF ::MemoDlg:changed
            ::MemoDlg:getData()
            ::xbpColumn:refreshRows( nRowPos, nRowPos )
         ENDIF
      ENDIF
   ENDIF

   ::xbpColumn:hiliteRow( nRowPos, lHilite, lFrame, lRepaint )
RETURN self


******************************************************************************
/*
 * MemoEditDialog with embedded XbpMle and :dataLink code block
 */

CLASS MemoEditDialog FROM XbpDialog
   PROTECTED:
   VAR btnOk    
   VAR bntUndo

   EXPORTED:
   VAR    controller
   VAR    MLE        
   VAR    dataLink
   VAR    changed
   VAR    bufferLength
   ACCESS ASSIGN METHOD dataLink   
   ACCESS ASSIGN METHOD changed

   METHOD init
   METHOD create
   METHOD setData
   METHOD getData
   METHOD editBuffer
   METHOD clear
   METHOD undo
   METHOD setMarked
   METHOD keyboard
ENDCLASS


METHOD MemoEditDialog:init( oParent, oOwner, aPos, aSize, aPP, lVisible )
   LOCAL aMinSize := {218,160}

   DEFAULT oParent  TO AppDesktop(), ;
           aPos     TO {0,0}, ;
           aSize    TO {218,160}, ;
           lVisible TO .F.

   aSize[1] := Max( aSize[1], aMinSize[1] )
   aSize[2] := Max( aSize[2], aMinSize[2] )

   ::XbpDialog:init( oParent, oOwner, aPos, aSize, aPP, lVisible )
   ::XbpDialog:taskList := .F.
   ::XbpDialog:title    := "Memo"
   ::XbpDialog:minSize  := aMinSize
   ::XbpDialog:close    := {|| ::controller:killRead() }
   ::XbpDialog:setInputFocus := {|x,y,obj| SetAppFocus( IIf(obj:isVisible(),obj:MLE, obj:controller) ) }

   ::MLE                := XbpMle():new( ::drawingArea )
   ::MLE:ignoreTab      := .T.
   ::MLE:wordWrap       := .T.
   ::MLE:horizScroll    := .F.

   ::btnOk              := XbpPushButton():new( ::drawingArea, , {8,8}, {88,24} )
   ::btnOk:caption      := "~Ok"
   ::btnOk:activate     := {|| ::getData(), ::controller:killRead() }

   ::bntUndo            := XbpPushButton():new( ::drawingArea, , {112,8}, {88,24} )
   ::bntUndo:caption    := "~Undo"
   ::bntUndo:activate   := {|| ::undo()   , ::controller:killRead() }

   ::bufferLength       := 66
RETURN self


METHOD MemoEditDialog:create( oParent, oOwner, aPos, aSize, aPP, lVisible )
   ::XbpDialog:create( oParent, oOwner, aPos, aSize, aPP, lVisible )

   aPos     := {8,48}
   aSize    := ::drawingArea:currentSize()
   aSize[1] -= 16
   aSize[2] -= 56

   ::MLE:create(,, aPos, aSize )
   ::btnOk:create()
   ::bntUndo:create()

   ::drawingArea:resize := {|aOld, aNew| aNew[1] -= 16, ;
                                         aNew[2] -= 56, ;
                                         ::Mle:setSize( aNew ) }
RETURN self


/*
 * Methods for editing
 */
METHOD MemoEditDialog:setData( xValue )
   IF .NOT. ::isVisible()
      ::show()
   ENDIF

   IF PCount() == 0
      xValue := ::MLE:setData()
   ELSE
      xValue := ::MLE:setData( xValue )
   ENDIF
RETURN xValue


METHOD MemoEditDialog:getData
   LOCAL xRet := ::MLE:getData()

   ::changed := .F.
   ::controller:browser:refreshCurrent()
RETURN xRet


METHOD MemoEditDialog:editBuffer
RETURN ::MLE:editBuffer()


METHOD MemoEditDialog:clear
RETURN ::MLE:clear()


METHOD MemoEditDialog:undo
RETURN ::MLE:undo()


METHOD MemoEditDialog:setMarked( aMarked )
RETURN ::MLE:setMarked( aMarked )


METHOD MemoEditDialog:dataLink( bBlock )
   IF PCount() == 1
      ::MLE:dataLink := bBlock
   ENDIF
RETURN ::MLE:dataLink


METHOD MemoEditDialog:changed( lChanged )
   IF Valtype( lChanged ) == "L"
      ::MLE:changed( lChanged )
   ENDIF
RETURN ::MLE:changed


/*
 * xbeK_ALT_UP and xbeK_ALT_DOWN are used for browser navigation
 */
METHOD MemoEditDialog:keyboard( nKey )
   DO CASE
   CASE nKey == xbeK_ALT_F4
      ::controller:killRead()

   CASE nKey == xbeK_ALT_UP
      ::controller:keyboard( xbeK_UP )
      SetAppFocus( self )

   CASE nKey == xbeK_ALT_DOWN
      ::controller:keyboard( xbeK_DOWN )
      SetAppFocus( self )

   CASE nKey == xbeK_ALT_O
      ::getData()
      ::controller:killRead()

   CASE nKey == xbeK_ALT_U
      ::setData()
      ::controller:killRead()

   CASE .NOT. ::controller:keyboard( nKey )
      ::xbpDialog:keyboard( nKey )

   ENDCASE
RETURN self
