//////////////////////////////////////////////////////////////////////
//
//  BMPREF.PRG
//
//  Copyright:
//   Alaska Software, (c) 1999-2025. All rights reserved.         
//  
//  Contents:
//       Alaska Management Console for the Advantage Database Server
//   
//       Class for handling datafiels containing bitmaps
//////////////////////////////////////////////////////////////////////


#include "Gra.ch"
#include "Xbp.ch"
#include "Common.ch"

/*
 * CLASS FOR handling datafiels containing bitmaps
 */
CLASS BitmapRef FROM XbpStatic, DataRef
   PROTECTED:
   VAR oBmp
   VAR oPS
   VAR lIsEmpty

   EXPORTED:
   VAR autoScale
   METHOD init, create, destroy
   METHOD setdata, getdata, editbuffer, display, load
ENDCLASS



/*
 * Initialize object
 */
METHOD BitmapRef:init( oParent, oOwner, aPos, aSize, aPresParem, lVisible )
   ::dataRef:init()
   ::xbpStatic:init( oParent, oOwner, aPos, aSize, aPresParem, lVisible )
   ::autoScale := .T.
   ::lIsEmpty  := .T.
   ::oPS       := XbpPresSpace():new()
   ::oBmp      := XbpBitmap():new()
RETURN self



/*
 * REQUEST SYSTEM RESOURCES AND DEFINE COLOR FOR 
 * EMPTY DATAFIELD
 */
METHOD BitmapRef:create( oParent, oOwner, aPos, aSize, aPresParem, lVisible )
   LOCAL aAttr[ GRA_AA_COUNT ]
   ::xbpStatic:create( oParent, oOwner, aPos, aSize, aPresParem, lVisible )

   ::oPS:create( ::xbpStatic:winDevice() )
   ::oBmp:create( ::oPS )

   aAttr[ GRA_AA_COLOR ] := GRA_CLR_PALEGRAY
   ::oPS:setAttrArea( aAttr )
   ::xbpStatic:paint := {|para1| ::display(para1) }
RETURN self



/*
 * Release System resources
 */
METHOD BitmapRef:destroy
   ::oBmp:destroy()
   ::oPS:destroy()
   ::xbpStatic:paint := NIL
   ::xbpStatic:destroy()
RETURN self



/*
 * Transfer bitmap data from FIELD TO buffer
 */
METHOD BitmapRef:setdata( cBuffer )
   IF cBuffer == NIL
      cBuffer := Eval( ::dataLink )
   ENDIF

   IF ! Empty( cBuffer )
      ::oBmp:setBuffer( cBuffer )
      ::lIsEmpty := .F.
   ELSE
      ::lIsEmpty := .T.
   ENDIF
   ::changed :=  .F.
   ::display()
RETURN cBuffer



/*
 * Transer buffered FIELD TO bitmap
 */
METHOD BitmapRef:getdata
   LOCAL cBuffer := ::oBmp:setBuffer()

   IF Valtype( ::dataLink ) == "B" .AND. .NOT. ::lIsEmpty
      Eval( ::dataLink, cBuffer )
   ENDIF
RETURN cBuffer


METHOD BitmapRef:editbuffer
RETURN ::oBmp:setBuffer()



/*
 * Draw bitmap
 */
METHOD BitmapRef:display(aClip)
   LOCAL aSize    := ::currentSize()
   LOCAL aTarget, aSource, nAspect
   LOCAL Ret

   DEFAULT aCLIP TO {0, 0, aSize[1], aSize[2]}

   // Prevent flickering with this...
   GraPathBegin( ::oPS )
   GraBox( ::oPS, { aClip[1]-1, aClip[2]-1 }, { aClip[3]+1, aClip[4]+1 }, GRA_OUTLINE )
   GraPathEnd( ::oPS )
   GraPathClip( ::oPS, .T. )

   GraBox( ::oPS, {1,1}, {aSize[1]-2,aSize[2]-2}, GRA_FILL )

   IF ::lIsEmpty
      GraPathClip( ::oPS, .F. )
      RETURN .F.
   ENDIF

   aSource := {0,0,::oBmp:xSize,::oBmp:ySize}
   aTarget := {1,1,aSize[1]-2,aSize[2]-2}

   IF ::autoScale
      nAspect    := aSource[3] / aSource[4]
      IF nAspect > 1
         aTarget[4] := aTarget[3] / nAspect
      ELSE
         aTarget[3] := aTarget[4] * nAspect
      ENDIF
   ELSE
      aTarget[3] := aSource[3]
      aTarget[4] := aSource[4]
   ENDIF

   IF aTarget[3] < aSize[1]-2
      nAspect := ( aSize[1]-2-aTarget[3] ) / 2
      aTarget[1] += nAspect
      aTarget[3] += nAspect
   ENDIF

   IF aTarget[4] < aSize[2]-2
      nAspect := ( aSize[2]-2-aTarget[4] ) / 2
      aTarget[2] += nAspect
      aTarget[4] += nAspect
   ENDIF

   Ret := ::oBmp:draw( ::oPS, aTarget, aSource, , GRA_BLT_BBO_IGNORE )
   GraPathClip( ::oPS, .F. )
  
RETURN Ret



/*
 * Load bitmap
 */
METHOD BitmapRef:load( nBitmap )
   IF ( ::changed := ::oBmp:load( ,nBitmap ) )
      ::lIsEmpty := .F.
   ENDIF
RETURN ::changed

