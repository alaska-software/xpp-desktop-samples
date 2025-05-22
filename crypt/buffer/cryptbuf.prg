//////////////////////////////////////////////////////////////////////
//
//  CRYPTBUF.PRG
//
//  Copyright:
//       Alaska Software, (c) 2002-2025. All rights reserved.         
//  
//  Contents:
//       De- and Encrypting a buffer
//   
//////////////////////////////////////////////////////////////////////



#include "Gra.ch"
#include "Xbp.ch"
#include "Appevent.ch"
#include "Font.ch"
#include "crypt.ch"

PROC APPSYS
RETURN

PROCEDURE Main
   LOCAL nEvent, mp1, mp2
   LOCAL oDlg, oXbp, drawingArea, aEditControls := {}
   LOCAL aPP
   LOCAL oKey, oInp, oOut

   oDlg := XbpDialog():new( AppDesktop(), , {293,396}, {600,185}, , .F.)
   oDlg:taskList := .T.
   oDlg:title := "Crypt a buffer"
   oDlg:icon:= 1
   oDlg:create()

   drawingArea := oDlg:drawingArea
   drawingArea:setFontCompoundName( "8.Arial" )

   oXbp := XbpStatic():new( drawingArea, , {34,120}, {72,14} )
   oXbp:caption := "Input"
   oXbp:clipSiblings := .T.
   oXbp:options := XBPSTATIC_TEXT_VCENTER
   oXbp:create()

   oXbp := XbpStatic():new( drawingArea, , {34,84}, {72,14} )
   oXbp:caption := "Key"
   oXbp:clipSiblings := .T.
   oXbp:options := XBPSTATIC_TEXT_VCENTER
   oXbp:create()

   oXbp := XbpStatic():new( drawingArea, , {34,48}, {72,14} )
   oXbp:caption := "Output"
   oXbp:clipSiblings := .T.
   oXbp:options := XBPSTATIC_TEXT_VCENTER
   oXbp:create()

   aPP := { { XBP_PP_BGCLR, XBPSYSCLR_ENTRYFIELD }, ;
            { XBP_PP_COMPOUNDNAME, "12.Courier" } }
   oInp := XbpSLE():new( drawingArea, , {84,120}, {504,24}, aPP )
   oInp:bufferLength := 64
   oInp:tabStop := .T.
   oInp:create()
   oInp:setData("Enter data in hexadecimal form like FF00FF00.")

   /*
    * The key entry field. You can set :unreadable to .T.
    * if desired. It is .F. by default for demonstration purposes.
    */
   oKey := XbpSLE():new( drawingArea, , {84,84}, {504,24}, aPP)
   oKey:tabStop := .T.
   oKey:bufferLength := 64
//   oKey:unReadable := .T.
   oKey:create()
   oKey:setData("Please generate a key or enter data in hexadecimal.")

   oOut := XbpSLE():new( drawingArea, , {84,48}, {504,24}, aPP) 
   oOut:bufferLength := 64
   oOut:tabStop := .T.
   oOut:create()
   oOut:setData("To decrypt, copy the result to the input field.")

   oXbp := XbpPushButton():new( drawingArea, , {6,12}, {112,24}, aPP)
   oXbp:caption := "Encrypt"
   oXbp:tabStop := .T.
   oXbp:create()
   oXbp:activate := {|| BuffCrypt(oInp, oKey, oOut, .T.) }

   oXbp := XbpPushButton():new( drawingArea, , {120,12}, {96,24}, aPP)
   oXbp:caption := "Decrypt"
   oXbp:tabStop := .T.
   oXbp:create()
   oXbp:activate := {|| BuffCrypt(oInp, oKey, oOut, .F.) }

   oXbp := XbpPushButton():new( drawingArea, , {228,12}, {130,24}, aPP)
   oXbp:caption := "Generate Key"
   oXbp:tabStop := .T.
   oXbp:create()
   oXbp:activate := {|| GenKey(oKey) }

   oXbp := XbpPushButton():new( drawingArea, , {370,12}, {116,24}, aPP)
   oXbp:caption := "Sample data"
   oXbp:tabStop := .T.
   oXbp:create()
   oXbp:activate := {|| oInp:setData("59:45:41:48:21") }

   /*
    * The COPY-pushbutton uses a bitmap as caption, with a
    * transparent color.
    */
   oXbp := XbpPushButton():new( drawingArea, , {6,48}, {22,95}, aPP)
   oXbp:caption := XbpBitmap():new()
   oXbp:caption:load(,2)
   oXbp:caption:transparentClr := GRA_CLR_WHITE
   oXbp:caption:create()
   oXbp:tabStop := .T.
   oXbp:create()
   oXbp:activate := {|| oInp:setData(oOut:getData()) }

   oXbp := XbpPushButton():new( drawingArea, , {492,12}, {96,24}, aPP)
   oXbp:caption := "Quit"
   oXbp:tabStop := .T.
   oXbp:create()
   oXbp:activate := {|| PostAppEvent( xbeP_Close ) }

   oDlg:show()
   SetAppFocus(oInp)

   nEvent := xbe_None
   DO WHILE nEvent <> xbeP_Close
      nEvent := AppEvent( @mp1, @mp2, @oXbp )
      oXbp:handleEvent( nEvent, mp1, mp2 )
   ENDDO
RETURN

/*
 * Encrypt the input sle to output-sle using the
 * key-sle-data as cipher key. All input/output is
 * done by using hexadeciaml string notations.
 */
PROCEDURE BuffCrypt(oInputXbp, oKeyXbp, oOutXbp, lEncrypt)
LOCAL oAes, oKey
LOCAL cBuffer, cResult

    oKey := SecureKey():new()
    cBuffer := oKeyXbp:getData()
    IF empty(cBuffer)
        RETURN
    ENDIF
    oKey:setStrKey(cBuffer)

    oAes := AesCrypt():new(oKey)
    cBuffer := oInputXbp:getData()
    IF empty(cBuffer)
        RETURN
    ENDIF
   /*
    * use the toBin() method to convert a hexstring to binary data,
    * then prepare the input buffer
    */
    cBuffer := oKey:toBin(cBuffer)
    oAes:prepareBuffer(@cBuffer)

    IF lEncrypt
       cResult := oAes:encrypt(cBuffer)
    ELSE
       cResult := oAes:decrypt(cBuffer)
    ENDIF
   /*
    * use the tostring() method to convert result to a hexstring
    */
    cResult := oKey:toString(cResult,":")
    oOutXbp:setData(cResult)    
RETURN

/*
 * Generate a new key and display to an SLE as hex-string
 */
PROCEDURE GenKey(oKeyXbp)
LOCAL oKey
     oKey := SecureKey():generate()
     oKeyXbp:setData(oKey:toString(,":"))
RETURN
