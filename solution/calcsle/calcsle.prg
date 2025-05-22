//////////////////////////////////////////////////////////////////////
//
//  CALCSLE.PRG
//
//  Copyright:
//       Alaska Software, (c) 1999-2025. All rights reserved.         
//  
//  Contents:
//       The CalcSLE class displays calculator-type entry fields for numeric
//       values. The display of entered data is right aligned, the number of
//       decimal places is unknown and the largest number to be entered is
//       limited by the width of the entry field.
//   
//  Remarks:
//   
//       NOTE:
//       Right aligned display DOES NOT WORK under Windows 95 but only under
//       Windows 98 and NT
//   
//       The :setData() method accepts a numeric value and the :dataLink
//       code block must return a value of type "N", if it is used.
//   
//       Only the characters "-.,0123456789" are accepted for editing.
//   
//       The cursor-left and backspace keys delete the last entered digit.
//   
//////////////////////////////////////////////////////////////////////



#include "Appevent.ch"
#include "Nls.ch"
#include "Xbp.ch"


PROCEDURE Main
   LOCAL nEvent, mp1, mp2, oXbp, aMarked

   SETCOLOR( "N/W" )
   CLS

   ? "   Sample for the CalcSLE class for inputting numeric values."
   ? "   Close window to quit."

   oSle:= CalcSLE():new()
   oSle:tabStop := .T.
   oSle:create( , , {230,192}, {140,24} )
   oSle:setData( 234567.231 )

   oSle:= CalcSLE():new()
   oSle:tabStop := .T.
   oSle:create( , , {230,160}, {140,24} )
   oSle:setData( -9876.543 )

   SetAppfocus( oSle )

   // Event loop
   nEvent := 0
   DO WHILE nEvent <> xbeP_Close
      nEvent := AppEvent( @mp1, @mp2, @oXbp )
      oXbp:HandleEvent( nEvent, mp1, mp2 )
   ENDDO
RETURN



/*
 * Class for calculator-type numeric entry fields
 */
CLASS CalcSLE FROM XbpSLE

   HIDDEN:
   CLASS VAR cDecimal  SHARED
   CLASS VAR cThousand SHARED
   VAR cPicture
   VAR lIsFloat
   VAR lIsMinus
   VAR lTypeOut
   VAR nNumber

   METHOD makePicture

   EXPORTED:
   CLASS METHOD initClass

   METHOD init, create, destroy
   METHOD setEditbuffer
   METHOD setData
   METHOD getData
   METHOD setInputFocus
   METHOD keyBoard
   METHOD lbDown
   METHOD lbUp
   METHOD rbUp
   METHOD typeOut
ENDCLASS


/*
 * Store country-specific delimiters in class variables
 */
CLASS METHOD CalcSLE:initClass
   ::cDecimal  := SetLocale( NLS_SDECIMAL  )   
   ::cThousand := SetLocale( NLS_STHOUSAND )   
RETURN self


/*
 * Initilize super class
 */
METHOD CalcSLE:init( oParent, oOwner, aPos, aSize, aPP, lVisible )
   ::xbpSLE:init( oParent, oOwner, aPos, aSize, aPP, lVisible )
   ::align := XBPSLE_RIGHT
RETURN self


/*
 * Request system resources and set initial value to zero.
 */
METHOD CalcSLE:create( oParent, oOwner, aPos, aSize, aPP, lVisible )
   ::xbpSLE:create( oParent, oOwner, aPos, aSize, aPP, lVisible )
   ::setData(0)
RETURN self


/*
 * Release system resources and internal values.
 */
METHOD CalcSLE:destroy
   ::cPicture := ;
   ::nNumber  := ;
   ::lIsMinus := ; 
   ::lIsFloat := ; 
   ::lTypeOut := NIL
   ::xbpSLE:destroy()
RETURN self


/*
 * Assign numeric value.
 */
METHOD CalcSLE:setData( nNumber )
   IF PCount() == 0
      nNumber := Eval( ::dataLink )
   ENDIF
   ::nNumber  := nNumber
   ::makePicture()
   ::setEditBuffer()
RETURN ::nNumber


/*
 * Retrieve edited value and pass it to the :dataLink code block.
 */
METHOD CalcSLE:getData
   IF Valtype( ::dataLink ) == "B"
      Eval( ::dataLink, ::nNumber )
   ENDIF
RETURN ::nNumber


/*
 * Create a Picture format.
 */
METHOD CalcSLE:makePicture
   LOCAL cBuffer, nLen, nDec

   IF ::nNumber == 0
      ::cPicture := ""
      ::lIsMinus := .F.
      ::lIsFloat := .F.
      ::lTypeOut := .F.
   ELSE
      ::lIsMinus := ( ::nNumber < 0 )

      cBuffer := LTrim( Str( ::nNumber, ::bufferLength+32, ::bufferLength ) )
      nLen    := Len( cBuffer )
      DO WHILE SubStr( cBuffer, nLen, 1 ) == "0"
         nLen --
      ENDDO

      cBuffer := SubStr( cBuffer, 1, nLen )
      IF Right( cBuffer, 1 ) == "."
         nLen --
         cBuffer := SubStr( cBuffer, 1, nLen )
      ENDIF

      nDec := At( ".", cBuffer )
      IF nDec > 0
         ::lIsFloat := .T.
         ::cPicture := "." + Replicate( "9", nLen-nDec )
         nLen       := nDec - 1
         nDec       := 0
      ELSE
         ::lIsFloat := .F.
      ENDIF

      DO WHILE nLen > 0
         ::cPicture := "9" + ::cPicture
         nDec ++
         nLen --
         IF nDec == 3
            nDec := 0
            IF nLen > 0
               ::cPicture := "," + ::cPicture
            ENDIF
         ENDIF
      ENDDO
   ENDIF
RETURN self


/*
 * Set edit buffer when input focus is obtained.
 */
METHOD CalcSLE:setInputFocus( mp1, mp2 )
   ::xbpSLE:setInputFocus( mp1, mp2 )
   ::setEditbuffer()
RETURN self


/*
 * Delegate the xbeM_LbDown event to the parent. This disables the
 * marking of digits in the entry field.
 */
METHOD CalcSLE:lbDown( mp1, mp2 )
   ::setParent():lbDown( mp1, mp2 )
RETURN self


/*
 * Input focus is obtained when the left mouse button is released.
 */
METHOD CalcSLE:lbUp( mp1, mp2 )
   ::xbpSLE:lbUp( mp1, mp2 )
   SetAppFocus( self )
RETURN self


/*
 * Suppress the default popup menu of the XbpSle class
 * by dirceting the xbeM_RbUp event to the parent.
 */
METHOD CalcSLE:rbUp( mp1, mp2 )
   ::setParent():rbUp( mp1, mp2 )
RETURN self


/*
 * Determine from the font size if the visible part of the entry
 * field is entirely filled with digits.
 */
METHOD CalcSLE:typeOut
   LOCAL oPS        := ::lockPS()
   LOCAL aBox1      := GraQueryTextBox( oPS, "9" )
   LOCAL nFontWidth := aBox1[3,1] - aBox1[1,1]
   LOCAL aBox2      := GraQueryTextBox( oPS, ::editBuffer() )
   LOCAL nBuffWidth := aBox2[3,1] - aBox2[1,1]
   LOCAL nDispWidth := ::currentSize()[1] - 2 * nFontWidth

   ::unlockPS(oPS)
   ::lTypeOut := ( nBuffWidth > nDispWidth )

   IF ::lTypeOut
      ::xbpSle:typeOut()
   ENDIF
RETURN self


/*
 * Transfer the numeric value into the edit buffer
 */
METHOD CalcSLE:setEditbuffer
   LOCAL nLen, cBuffer

   IF Right( ::cPicture, 1 ) == "."
     /*
      * Number with decimal point but no decimal digits.
      */
      nLen    := Len( ::cPicture )
      cBuffer := Left( Transform( ::nNumber, ::cPicture +"9" ), nLen )
   ELSE
      cBuffer := Transform( ::nNumber, ::cPicture ) 
   ENDIF

   cBuffer := LTrim( cBuffer )

   IF ::nNumber == 0 .AND. ::lIsMinus
     /*
      * A negative number is being entered but the value is still zero.
      */
      cBuffer := "-" + cBuffer
   ENDIF

   IF ::lIsFloat
     /*
      * Set the caret behind the last digit in the edit buffer.
      */
      ::xbpSLE:setData( cBuffer )
      nLen := Len( ::editBuffer() ) + 1
   ELSE
     /*
      * Set the caret to the left of the decimal point.
      */
      ::xbpSLE:setData( cBuffer + ::cDecimal )
      nLen := Len( ::editBuffer() ) 
   ENDIF

   ::setMarked( {nLen, nLen} )
   ::typeOut()
RETURN self


/*
 * Keyboard event handler.
 */
METHOD CalcSLE:keyBoard( nKey )
   LOCAL cChr := IIf( nKey > 0 .AND. nKey < 255, Chr(nKey), " " )
   LOCAL nLen

   DO CASE
   CASE nKey == xbeK_TAB
      ::xbpSLE:keyBoard( nKey )

   CASE nKey == xbeK_ENTER
      PostAppevent( xbeP_Keyboard, xbeK_TAB, NIL, self )

   CASE nKey == xbeK_LEFT
      PostAppevent( xbeP_Keyboard, xbeK_BS, NIL, self )

   CASE nKey == xbeK_BS
      ::xbpSle:keyBoard( nKey )
      cChr      := StrTran( ::editBuffer(), ::cThousand, "" )
      ::nNumber := Val( StrTran( cChr, ::cDecimal , "." ) )

      IF ::nNumber <> 0
         ::lIsMinus := ( ::nNumber < 0 )
      ENDIF

      IF ::lIsFloat
        /*
         * Remove last character of the picture string.
         */
         ::cPicture := SubStr( ::cPicture, 1, Len( ::cPicture )-1 )
         IF Right( ::cPicture, 1 ) == "."
            ::cPicture := SubStr( ::cPicture, 1, Len( ::cPicture )-1 )
         ENDIF
         ::lIsFloat := ( "." $ ::cPicture )

         IF ::lIsMinus .AND. ::nNumber == 0
           /*
            * The edit buffer may contain something like "-0.00"
            */
            ::lIsMinus := ::lIsFloat
         ENDIF
      ELSE
        /*
         * Remove first character of the picture string.
         */
         ::cPicture := SubStr( ::cPicture, 2 )
         IF Left( ::cPicture, 1 ) == ","
            ::cPicture := SubStr( ::cPicture, 2 )
         ENDIF
         IF ::lIsMinus .AND. ::nNumber == 0
           /*
            * Remove the minus sign when the edit buffer contains "-0."
            */
            ::lIsMinus := .F.
         ENDIF
      ENDIF

      ::setEditbuffer()

   CASE ::lTypeOut
     /*
      * The edit buffer is full.
      */
      Tone(1000)

   CASE cChr $ "0123456789"
      IF ::nNumber == 0
        /*
         * The buffer may contain 0.00 or -0.00 when a number
         * between < 1 and and > -1 is entered.
         */
         IF cChr == "0"
            IF .NOT. ::lIsFloat
               IF ::lIsMinus
                  ::cPicture := "9"
               ELSE
                  ::cPicture := ""
               ENDIF
            ENDIF
         ELSE
            IF ::lIsFloat
               ::nNumber := Val( StrTran( ::editBuffer(), ::cDecimal, "." ) + cChr )
            ELSEIF ::lIsMinus
               ::nNumber := - Val( cChr )
            ELSE
               ::nNumber := Val( cChr )
            ENDIF
         ENDIF
      ELSE
         ::xbpSle:keyBoard( nKey )
         cChr      := StrTran( ::editBuffer(), ::cThousand, "" )
         ::nNumber := Val( StrTran( cChr, ::cDecimal , "." ) )
      ENDIF

      IF ::lIsFloat
         IF At( ".", ::cPicture ) == 0
            ::cPicture += "."
         ELSE
            ::cPicture += "9"
         ENDIF
      ELSE
         IF Left( ::cPicture, 3 ) == "999"
            ::cPicture := "9," + ::cPicture
         ELSE
            ::cPicture := "9" + ::cPicture
         ENDIF
      ENDIF
      ::setEditbuffer()

   CASE cChr $ ",."
      IF .NOT. ::lIsFloat
         ::lIsFloat := .T.
         IF ::nNumber == 0
            ::cPicture += "9"
         ENDIF
         ::cPicture += "."
         ::setEditbuffer()
      ENDIF

   CASE cChr == "-"
      IF .NOT. ::lIsMinus
         ::lIsMinus := .T.
         ::nNumber  *= -1
         IF Left( ::cPicture, 3 ) == "999"
            ::cPicture := "9," + ::cPicture
         ELSE
            ::cPicture := "9"  + ::cPicture
         ENDIF
         ::setEditbuffer()
      ENDIF

   OTHERWISE
      ::setParent():keyboard( nKey )

   ENDCASE
RETURN self
