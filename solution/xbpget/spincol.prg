//////////////////////////////////////////////////////////////////////
//
//  SPINCOL.PRG
//
//  Copyright:
//      Alaska Software, (c) 1999-2025. All rights reserved.         
//  
//  Contents:
//      This example implements the XbpSpinbuttonColumn class which uses
//      a spinbutton for editing of single cells in an XbpBrowse object.
//      (Editing of integers)
//   
//      The spinbutton is provided by the XbpGetSpinbutton class, which is also
//      implemented in this file.
//   
//  Remarks:
//     
//      The following programming techniques are demonstrated:
//     
//      1. How to subclass from the XbpGetColumn class in order to use
//         another Xbase Part than XbpGet for editing in the browser.
//   
//      2. How to change the key-handling of XbpSpinbutton
//   
//////////////////////////////////////////////////////////////////////


#include "Appevent.ch"
#include "Common.ch"
#include "Xbp.ch"


CLASS XbpSpinbuttonColumn FROM XbpGetColumn
   PROTECTED:
   VAR    spinbutton IS xbpGet IN XbpGetColumn
   VAR    aMinMax

   EXPORTED:   
   METHOD init
   METHOD createXbp
   METHOD setNumLimits
ENDCLASS


/*
 * Initialize the object
 */
METHOD XbpSpinbuttonColumn:init( p1, p2, p3, p4, p5, p6 )
   ::xbpGetColumn:init( p1, p2, p3, p4, p5, p6 )
   ::aMinMax := { 0, 99999 }
RETURN self


/*
 * Create an XbpSpinbutton instead of an the XbpGet object and assign
 * the :dataLink code block
 *
 * Note: the instance variable :spinbutton is actually declared as
 *       :xbpGet in the super class, but renamed in this class.
 *       This makes the code better readable.
 */
METHOD XbpSpinbuttonColumn:createXbp
   ::spinbutton            := XbpGetSpinbutton():new( ::dataArea, ::dataArea, NIL, NIL, NIL, .F. )
   ::spinbutton:controller := self
   ::spinbutton:dataLink   := ::dataLink
   ::spinbutton:create()
   ::spinbutton:setNumLimits( ::aMinMax[1], ::aMinMax[2] )
RETURN self


/*
 * Store the Min/Max range and transfer it to the spinbutton
 * if it exists already.
 */
METHOD XbpSpinbuttonColumn:setNumLimits( nMin, nMax )
   ::aMinMax := { nMin, nMax }
   IF ::spinbutton <> NIL
      ::spinbutton:setNumLimits( ::aMinMax[1], ::aMinMax[2] )
   ENDIF
RETURN self


******************************************************************************
/*
 * Spinbutton class with overloaded :keyboard() method. This allows for
 * browser navigation with the xbeK_UP, xbeK_DOWN keys.
 * The spinbutton reacts to xbeK_ALT_UP, xbeK_ALT_DOWN instead
 */

CLASS XbpGetSpinbutton FROM XbpSpinbutton
   EXPORTED:
   VAR    controller
   METHOD keyboard
ENDCLASS


METHOD XbpGetSpinbutton:keyboard( nKey )
   DO CASE
   /*
    * The spinbutton reacts to xbeK_ALT_UP and xbeK_ALT_DOWN as if
    * xbeK_UP or xbeK_DOWN was pressed.
    */
   CASE nKey == xbeK_ALT_UP
      ::xbpSpinbutton:keyboard( xbeK_UP )

   CASE nKey == xbeK_ALT_DOWN
      ::xbpSpinbutton:keyboard( xbeK_DOWN )

   CASE ::controller == NIL .OR. .NOT. ::controller:keyboard( nKey )
      /*
       * xbeK_UP and xbeK_DOWN are directed to the browser by the
       * :controller object (XbpSpinbuttonColumn) and the browser uses
       * UP/DOWN for navigation. All keys that are not understood by
       * the :controller object are handled by XbpSpinbutton.
       */
      ::xbpSpinbutton:keyboard( nKey )

   ENDCASE
RETURN self
