//////////////////////////////////////////////////////////////////////
//
//  COMBOCOL.PRG
//
//  Copyright:
//      Alaska Software, (c) 1999-2025. All rights reserved.         
//  
//  Contents:
//      This example implements the XbpComboboxColumn class which uses
//      a combobox for editing of single cells in an XbpBrowse object.
//   
//      The combobox is provided by the XbpGetCombobox class, which is also
//      implemented in this file.
//   
//  Remarks:
//     
//      The following programming techniques are demonstrated:
//     
//      1. How to subclass from the XbpGetColumn class in order to use
//         another Xbase Part than XbpGet for editing in the browser.
//   
//      2. How to program a combobox class with an unambiguous :datalink
//   
//////////////////////////////////////////////////////////////////////


#include "Appevent.ch"
#include "Common.ch"
#include "Xbp.ch"


CLASS XbpComboboxColumn FROM XbpGetColumn
                                     ** overloaded XbpGetColumn methods
   PROTECTED:                        // re-defined for better readability
   VAR    comboBox IS xbpGet IN XbpGetColumn
   METHOD setPosAndSize              // sets pos and size to the current row

                                     ** new in this class
   VAR    aItems                     // Array for combobox items

   EXPORTED:                         // overloaded XbpGetColumn methods
   METHOD init                       // initialize the object
   METHOD destroy                    // release memory
   METHOD createXbp                  // create the XbpGetCombobox object

                                     ** new in this class
   VAR    comboType                  // transferred to ::combobox:type
   VAR    comboHeight                // height of the combobox
   METHOD setItems                   // Transfer array values to combobox
ENDCLASS


/*
 * Initialize the object
 */
METHOD XbpComboboxColumn:init( p1, p2, p3, p4, p5, p6 )
   ::xbpGetColumn:init( p1, p2, p3, p4, p5, p6 )
   ::aItems           := {}
   ::comboType        := XBPCOMBO_DROPDOWN
   ::comboHeight      := 150
RETURN self


/*
 * Release resources and destroy the array reference for the valid values.
 */
METHOD XbpComboboxColumn:destroy
   ::aItems := {}
   ::xbpGetColumn:destroy()
RETURN self


 /*
 * Create an XbpCombobox instead of an the XbpGet object and assign
 * the :dataLink code block
 *
 * Note: the instance variable :comboBox is actually declared as
 *       :xbpGet in the super class, but renamed in this class.
 *       This makes the code better readable.
 */
METHOD XbpComboboxColumn:createXbp
   ::comboBox              := XbpGetComboBox():new( ::dataArea, ::dataArea, NIL, NIL, NIL, .F. )
   ::combobox:controller   := self
   ::comboBox:type         := ::comboType
   ::comboBox:dataLink     := ::dataLink

   IF ::bufferLength <> NIL
      ::comboBox:bufferLength := ::bufferLength
   ENDIF

   ::comboBox:create()
   ::comboBox:setItems( ::aItems )
RETURN self

 
/*
 * Determine the X/Y coordinates for the combobox display.
 */
METHOD XbpComboboxColumn:setPosAndSize( nRowPos )
   LOCAL aRect, aPos, aSize

   aRect    := ::dataArea:cellRect( nRowPos )
   aPos     := { aRect[1], aRect[2] }
   aSize    := { aRect[3]-aRect[1], aRect[4]-aRect[2] }
   aPos[2]  -= ::comboHeight
   aSize[2] += ::comboHeight

   ::comboBox:setPos ( aPos  )
   ::comboBox:setSize( aSize )
RETURN self


/*
 * Clone the array that contains the valid string values
 */
METHOD XbpComboboxColumn:setItems( aStrings )
   ::aItems := AClone( aStrings )
   IF ::comboBox <> NIL
      ::comboBox:setItems( ::aItems )
   ENDIF
RETURN self


******************************************************************************
/*
 * Combobox class with unambiguous :dataLink and overloaded
 * :keyboard() method.
 *
 *    - :dataLink, :setData(), :getData() is always resolved to the
 *      XbpSLE part of the combobox
 *
 *    - the xbeK_UP, xbeK_DOWN keys are used for browser navigation
 *      as long as the listbox part is not dropped down.
 */

CLASS XbpGetCombobox FROM XbpCombobox
   EXPORTED:
   VAR    dataLink
   VAR    controller
   VAR    bufferLength

   ACCESS ASSIGN METHOD dataLink
   ACCESS ASSIGN METHOD bufferLength
   METHOD clear
   METHOD undo
   METHOD setData
   METHOD editBuffer
   METHOD getData
   METHOD setItems
   METHOD getItems
   METHOD keyboard
ENDCLASS


METHOD XbpGetCombobox:dataLink( bData )
   IF PCount() == 1
      ::XbpSLE:datalink := bData
   ENDIF
RETURN ::XbpSLE:datalink


METHOD XbpGetCombobox:bufferLength( nLen )
   IF Valtype( nLen ) == "N"
      ::XbpSLE:bufferLength := nLen
   ENDIF
RETURN ::XbpSLE:bufferLength


METHOD XbpGetCombobox:clear
RETURN ::xbpSLE:clear()


METHOD XbpGetCombobox:undo
RETURN ::xbpSLE:undo()


METHOD XbpGetCombobox:editBuffer
RETURN ::xbpSLE:editBuffer()


METHOD XbpGetCombobox:setData( xValue )
   IF xValue == NIL
      xValue := ::xbpSLE:setData()
   ELSE
      xValue := ::xbpSLE:setData( xValue )
   ENDIF
   ::changed := .F.
RETURN xValue


METHOD XbpGetCombobox:getData
   LOCAL cValue := Trim( ::xbpSLE:getData() )
   LOCAL aItems := ::getItems()
   LOCAL nPos

   /*
    * If an edited value does not exist in the listbox, insert
    * it at the correct sort position. The combobox "learns".
    */
   IF Ascan( aItems, {|c| Upper(c) == Upper(cValue) } ) == 0
      nPos := AScan( aItems, {|c| Upper(c) > Upper(cValue) } )
      IF nPos == 0
         ::addItem( cValue )
      ELSE
         ::insItem( nPos, cValue )
      ENDIF
   ENDIF
RETURN cValue


METHOD XbpGetCombobox:setItems( aArray )
   ::xbpComboBox:clear()
RETURN AEval( aArray, {|c| ::addItem( c ) } )


METHOD XbpGetCombobox:getItems
RETURN AEval( Array( ::numItems() ), ;
              {|x,i| x := ::getItem(i) },,, .T. )


METHOD XbpGetCombobox:keyboard( nKey )
   DO CASE
   CASE nKey == xbeK_ALT_DOWN .OR. nKey == xbeK_CTRL_RETURN
      /*
       * Drop down the listbox.
       */
      ::listboxFocus( .T. )

   CASE .NOT. ::listboxFocus()
      /*
       * If the combobox is not embedded in a browser column or
       * if the :controller object (XbpComboboxColumn) does not handle
       * the key, the default key-handling of XbpCombobox steps in.
       */
      IF ::controller == NIL .OR. .NOT. ::controller:keyboard( nKey )
         DO CASE
         CASE ::controller <> NIL .AND. ::type == XBPCOMBO_DROPDOWNLIST
            IF nKey == xbeK_LEFT
               ::controller:keyboard( xbeK_SH_TAB )
            ELSEIF nKey == xbeK_RIGHT
               ::controller:keyboard( xbeK_TAB )
            ELSE
               ::xbpCombobox:keyboard( nKey )
            ENDIF

         CASE nKey == xbeK_RETURN
            PostAppEvent( xbeLB_ItemSelected,,, self )
         OTHERWISE
            ::xbpCombobox:keyboard( nKey )
         ENDCASE
      ENDIF

   OTHERWISE
      /*
       * All other situations are treated by XbpCombobox.
       */
      ::xbpCombobox:keyboard( nKey )

   ENDCASE
RETURN self
