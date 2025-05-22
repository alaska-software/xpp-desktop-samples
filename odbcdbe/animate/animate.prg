//////////////////////////////////////////////////////////////////////
//
//  ANIMATE.PRG
//
//  Copyright:
//      Alaska Software, (c) 1997-2025. All rights reserved.         
//  
//  Contents:
//      This sample shows what you can do with Xbase++ regarding a large  
//      amount of data.
//      Bitmap data will be read from a  and displayed via the
//      XbBitmap object. Additionally, a 2nd thread is running which
//      draws a marquee.
//   
//  Remarks:
//      1. For best display results, use the "TrueColor" option from
//         your display properties.
//      2. Copy DBF bmpdb to database server before start program.
//         This can the done by using load sample.
//   
//////////////////////////////////////////////////////////////////////


#include "font.ch"
#include "xbp.ch"
#include "gra.ch"
#include "appevent.ch"
#include "common.ch"

PROCEDURE MAIN(cDsn)
  LOCAL oT1,oT2

  SetAppWindow():useShortCuts := .T.
  SetColor("W/B+")
  CLS

  DEFAULT cDsn TO "SQLDB1;UID=hs;PWD=sunny"

  /*
   * Create the threads for the marquee and the animation
   */
  oT1 := Thread():New()
  oT1:start("marquee",;
            "***This sample stresses the bandwidth of your workstation "+;
            "to some extend. One thread handles the marquee and another one "+ ;
            "loads a total of 11 MB bitmap data and draws the animation "+;
            "again and again!",;
            {50,320},;
            {320,30},;
            34,;
            GRA_CLR_YELLOW,;
            GRA_CLR_BLACK,;
            FONT_TIMES_XLARGE)

  oT2 := Thread():New()
  oT2:start("drawBitmapFromDB","BMPDB", cDsn)

  @ MaxRow() , 6 SAY "***PRESS ESC OR CLOSE THE WINDOW TO EXIT!"

  /*
   * Wait for input of ESC or window close and terminate 
   * only if all threads have been terminated before
   */
  nEvent := xbeP_None
  DO WHILE nEvent != xbeP_Close .AND. nEvent != xbeK_ESC
     nEvent := AppEvent( @mp1, @mp2, @oXbp )
     oXbp:handleEvent( nEvent, mp1, mp2 )
  ENDDO

  oT1:synchronize(0)
  oT2:synchronize(0)

RETURN

/*
 * drawBitmapFromDB(cFile) => NIL
 * displays bitmaps from a database table until escape was pressed
 */
FUNCTION drawBitmapFromDB(cFile, cDsn)
  LOCAL oPS, oBMP, nEvent, oSession
  
  oSession := dacSession():New("DBE=ODBCDBE;DSN="+cDsn) 
  IF (!oSession:isConnected()) 
    SetColor("W/R")
    @ MaxRow() - 3, 6 SAY "Cannot connect to:" + cDsn
    @ MaxRow() - 2, 6 SAY "Try to pass a DSN name on the command line."
    @ MaxRow() - 1, 6 SAY "Sample: ANIMATE SQLServer;UID=xx;PWD=xx"
    RETURN NIL
  ENDIF

  USE (cFile) EXCLUSIVE
  oPS := SetAppWindow():presSpace() 
  oBMP:= XbpBitmap():new():create( oPS ) 

  nEvent := xbeP_None
  DO WHILE nEvent != xbeP_Close .AND. nEvent != xbeK_ESC
    nEvent := LastAppEvent(,,,1)
    oBMP:setBuffer(FIELD->BITMAP,XBPBMP_FORMAT_WIN3X)
    oBMP:draw(oPS,{50,50})
    IF(EOF())
      DbGoTop()
    ELSE
      DbSkip(1)
    ENDIF
    Sleep(0.1)
  ENDDO

  USE

  oSession:disconnect()
RETURN NIL


/*
 * The Marquee()-function displays cText at the given postion,
 * in the given size, color and with the selected font as
 * running letters.
 */
FUNCTION Marquee(cText,aPos,aSize,nChars,nFG,nBG,cFont)
  LOCAL aAA    := Array( GRA_AA_COUNT )
  LOCAL aAS    := Array( GRA_AS_COUNT )
  LOCAL oF     := XbpFont():New()
  LOCAL aTBox
  LOCAL nEvent := xbeP_None

  LOCAL nMax,nOffset,nStart
  LOCAL cP

  oF:familyName := "Times New Roman" 
  oF:height     := 16
  oF:width      :=  8 
  oF:create()                        
 
  GraSetFont ( , oF ) 
  aAA[GRA_AA_COLOR]     := nBG
  aAA[GRA_AA_BACKCOLOR] := nFG
  GraSetAttrArea( , aAA )

  aAS[GRA_AS_COLOR]     := nFG
  aAS[GRA_AS_BACKCOLOR] := nBG
  GraSetAttrString( , aAS )

  nMax   := nOffset := nChars
  nStart := 1
  
  DO WHILE nEvent != xbeP_Close .AND. nEvent != xbeK_ESC
    nEvent := LastAppEvent(,,,1)
  
    IF( nStart==1 )
       cP := Space(nOffset) + Substr(cText,nStart,nMax-nOffset)
    ELSE
       cP := Substr(cText,nStart,nMax)
    ENDIF

    GraBox( , aPos , {aSize[1]+aPos[1],aSize[2]+aPos[2]} ,GRA_FILL )
    GraStringAt(,{aPos[1]+2,aPos[2]+int((aSize[2]-oF:nominalPointSize)/2)},cP)
    Sleep(8)
    IF (nOffset==0)
       nStart++
       IF (nStart==Len(cText))
          nOffset := nMax
          nStart := 1
       ENDIF
    ELSE
       nOffset--
    ENDIF 
  ENDDO
RETURN NIL

/*
 * Load ODBCDBE as default dbe
 */
PROCEDURE DbeSys
   DbeLoad( "ODBCDBE" )
   DbeSetDefault( "ODBCDBE" )
RETURN








