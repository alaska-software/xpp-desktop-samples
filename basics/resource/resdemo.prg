//////////////////////////////////////////////////////////////////////
//
//  RESDEMO.PRG
//
//  Copyright:
//           Alaska Software, (c) 1997-2025. All rights reserved.         
//  
//  Contents:
//           Demonstrates the usage of Resources with Xbase++
//   
//  Remarks:
//           The resources are defined in RESDEMO.ARC
//   
//////////////////////////////////////////////////////////////////////


#include "Gra.ch"
#include "Xbp.ch"
#include "Appevent.ch"

#include "resdemo.ch"

PROCEDURE Main
   LOCAL nEvent, mp1, mp2
   LOCAL oDlg, oXbp
   LOCAL i, aPtrs, aBmps, aIcons
   LOCAL cJpeg

   oDlg := SetAppWindow()

   aPtrs  := { RD_CROSS, RD_BEAM, RD_NODROP, RD_ARROW, RD_MOVE}
   aBmps  := { RD_BMP1,  RD_BMP2, RD_BMP3,   RD_BMP4,  RD_BMP5}
   aIcons := { RD_XPP,   RD_FLD,  RD_XPPFD,  RD_XFF,   RD_PBUILD}

   for i:= 1 TO len( aPtrs)
           oXbp := XbpStatic():new(,, {i*70,192}, {30,40} )
           oXbp:type := XBPSTATIC_TYPE_BITMAP
           oXbp:autoSize := .T.
           oXbp:caption := aBmps[i]
           oXbp:create()
           oXbp:setPointer(,aPtrs[i],XBPWINDOW_POINTERTYPE_POINTER)

           oXbp := XbpStatic():new(,, {i*70+5,120}, {32,32} )
           oXbp:type := XBPSTATIC_TYPE_ICON 
           oXbp:caption := aIcons[i]
           oXbp:create()
           oXbp:setPointer(,aPtrs[i],XBPWINDOW_POINTERTYPE_POINTER)
   NEXT
   /* 
    * A JPEG file is loaded as resource
    */
   cJpeg := LoadResource("TILE1",,"JPEG")
   IF cJpeg != NIL
      oXbp := XbpBitmap():new():create()
      oXbp:setBuffer(cJpeg)
      oXbp:draw(, {10,10})
   ELSE
      @ MaxRow()-1,1 SAY DosErrorMessage(DosError())
   ENDIF
   ? "Move the mouse over the bitmaps and icons."
   ? "Watch the changing mouse pointer."
   ? "Press Esc to quit."

   oDlg:show()

   nEvent := xbe_None
   DO WHILE nEvent <> xbeP_Close .AND. nEvent <> xbeK_ESC
      nEvent := AppEvent( @mp1, @mp2, @oXbp )
      oXbp:handleEvent( nEvent, mp1, mp2 )
   ENDDO
RETURN

