//////////////////////////////////////////////////////////////////////
//
//  SDIPART.PRG
//
//  Copyright:
//      Alaska Software, (c) 1997-2025. All rights reserved.         
//  
//  Contents:
//      Definition of a parts data entry screen for SDIDEMO.EXE
//   
//////////////////////////////////////////////////////////////////////


#include "Gra.ch"
#include "Xbp.ch"
#include "Common.ch"
#include "Sdidemo.ch"

PROCEDURE Parts( oDlg )
   LOCAL oXbp, oStatic, drawingArea, oBitmapRef
   LOCAL aControls, bKeyHandler
   FIELD PARTNO, PARTNAME, PARTTYPE, PURCHASE, SELLPRICE, MARGIN, BMPIMAGE

   // Get IWindow from XbpDialog
   drawingArea := oDlg:drawingArea

   IF Empty( drawingArea:childList() )
      oDlg:cargo := { Recno(), Recno() }
      aControls  := {}

      // Create group box for fields
      oStatic := XbpStatic():new( drawingArea,, {13,87}, {288,247} )
      oStatic:caption := "Part"
      oStatic:type := XBPSTATIC_TYPE_GROUPBOX
      oStatic:create()

      // Create static texts for SLEs as well as SLEs for fields
      oXbp := XbpStatic():new( oStatic,, {14,197}, {94,22} )
      oXbp:caption := "Part No:"
      oXbp:options := XBPSTATIC_TEXT_RIGHT
      oXbp:create()

      oXbp := XbpSLE():new( oStatic,, {114,197}, {85,22} )
      oXbp:bufferLength := 8
      oXbp:tabStop  := .T.
      oXbp:group    := XBP_BEGIN_GROUP
      oXbp:dataLink := {|x| IIf( x==NIL, Trim( PARTNO ), PARTNO := x ) }
      oXbp:validate := {|o| IsIndexUnique( 1, PadR(RTrim(o:editBuffer()),8) ) }
      oXbp:create():setData()
      oXbp:setName( ID_SLE_PRIMKEY )
      AAdd( aControls, oXbp )

      IF ! Eof()
         oXbp:disable()
      ELSE
         SetAppFocus( oXbp )
      ENDIF

      oXbp := XbpStatic():new( oStatic,, {14,167}, {94,22} )
      oXbp:caption := "Part Name:"
      oXbp:options := XBPSTATIC_TEXT_RIGHT
      oXbp:create()

      oXbp := XbpSLE():new( oStatic,, {114,167}, {152,22} )
      oXbp:bufferLength := 20
      oXbp:tabStop  := .T.
      oXbp:group    := XBP_WITHIN_GROUP
      oXbp:dataLink := {|x| IIf( x==NIL, Trim( PARTNAME ), PARTNAME := x ) }
      oXbp:create():setData()
      AAdd( aControls, oXbp )

      IF ! Eof()
         SetAppFocus( oXbp )
      ENDIF

      oXbp := XbpStatic():new( oStatic,, {14,137}, {94,22} )
      oXbp:caption := "Type:"
      oXbp:options := XBPSTATIC_TEXT_RIGHT
      oXbp:create()

      oXbp := XbpSLE():new( oStatic,, {114,137}, {152,22} )
      oXbp:bufferLength := 20
      oXbp:group    := XBP_WITHIN_GROUP
      oXbp:dataLink := {|x| IIf( x==NIL, Trim( PARTTYPE ), PARTTYPE := x ) }
      oXbp:create():setData()
      AAdd( aControls, oXbp )

      oXbp := XbpStatic():new( oStatic,, {14,107}, {94,22} )
      oXbp:caption := "Purchase:"
      oXbp:options := XBPSTATIC_TEXT_RIGHT
      oXbp:create()

      oXbp := XbpSLE():new( oStatic,, {114,107}, {83,22} )
      oXbp:bufferLength := 8
      oXbp:group    := XBP_WITHIN_GROUP
      oXbp:dataLink := {|x| IIf( x==NIL, LTrim(Str( PURCHASE, 8, 2 )) , PURCHASE := Val( x ) ) }
      oXbp:create():setData()
      AAdd( aControls, oXbp )

      oXbp := XbpStatic():new( oStatic,, {14,77}, {94,22} )
      oXbp:caption := "Sell Price:"
      oXbp:options := XBPSTATIC_TEXT_RIGHT
      oXbp:create()

      oXbp := XbpSLE():new( oStatic,, {114,77}, {84,22} )
      oXbp:bufferLength := 8
      oXbp:group    := XBP_WITHIN_GROUP
      oXbp:dataLink := {|x| IIf( x==NIL, LTrim(Str( SELLPRICE, 8, 2 )) , SELLPRICE := Val( x ) ) }
      oXbp:create():setData()
      AAdd( aControls, oXbp )

      oXbp := XbpStatic():new( oStatic,, {14,47}, {94,22} )
      oXbp:caption := "Calc.:"
      oXbp:options := XBPSTATIC_TEXT_RIGHT
      oXbp:create()

      oXbp := XbpSLE():new( oStatic,, {114,47}, {48,22} )
      oXbp:bufferLength := 4
      oXbp:group    := XBP_WITHIN_GROUP
      oXbp:dataLink := {|x| IIf( x==NIL, LTrim(Str( MARGIN, 4, 1 )) , MARGIN := Val( x ) ) }
      oXbp:create():setData()
      AAdd( aControls, oXbp )

      oXbp := XbpPushButton():new( oStatic,, {114,17}, {152,22} )
      oXbp:caption  := "Bitmap"
      oXbp:activate := {|| GetBitmapFile( oBitmapRef ) }
      oXbp:group    := XBP_END_GROUP
      oXbp:create()
      AAdd( aControls, oXbp )

      oBitmapRef := BitmapRef():new( drawingArea,, {310,87}, {295,238} )
      oBitmapRef:dataLink := {|x| IIf( x== NIL, BMPIMAGE, BMPIMAGE := x ) }
      oBitmapRef:create():setData()
      AAdd( aControls, oBitmapRef )

      oXbp := XbpPushButton():new( drawingArea,, {15,15}, {90,30} )
      oXbp:caption := "Previous"
      oXbp:group   := XBP_BEGIN_GROUP
      oXbp:create()
      oXbp:setName( ID_BTN_PREVIOUS )
      oXbp:activate := {|| DlgSkip( oDlg, -1) }
      AAdd( aControls, oXbp )

      oXbp := XbpPushButton():new( drawingArea,, {135,15}, {90,30} )
      oXbp:caption := "Next"
      oXbp:group    := XBP_WITHIN_GROUP
      oXbp:create()
      oXbp:setName( ID_BTN_NEXT )
      oXbp:activate :=  {|| DlgSkip( oDlg, 1) }
      AAdd( aControls, oXbp )

      oXbp := XbpPushButton():new( drawingArea,, {255,15}, {90,30} )
      oXbp:caption  := "Save"
      oXbp:group    := XBP_WITHIN_GROUP
      oXbp:tabStop  := .T.
      oXbp:activate := {|| DlgSave( oDlg ) }
      oXbp:create():disable()
      oXbp:setName( ID_BTN_SAVE )
      AAdd( aControls, oXbp )

      oXbp := XbpPushButton():new( drawingArea,, {375,15}, {90,30} )
      oXbp:caption  := "Undo"
      oXbp:group    := XBP_WITHIN_GROUP
      oXbp:activate := {|| DlgUndo( oDlg ) }
      oXbp:create():disable()
      oXbp:setName( ID_BTN_UNDO )
      AAdd( aControls, oXbp )

      oXbp := XbpPushButton():new( drawingArea,, {505,15}, {90,30} )
      oXbp:caption := "Help"
      oXbp:group   := XBP_END_GROUP
      oXbp:create()
      oXbp:activate := {|| HelpObject():showHelpContents() }
      AAdd( aControls, oXbp )

      bKeyHandler := {|nKey,x,obj| DlgKeyhandler( nKey, obj, aControls, oDlg ) }
      AEval( aControls, {|o| o:keyBoard := bKeyHandler } )

   ENDIF
RETURN



/*
 * Transfer an external bitmap image to the database
 */
FUNCTION GetBitmapFile( oBmpRef )
   STATIC scPath
   LOCAL  oDlg, cCurDir := CurDrive() + ":\" + CurDir() + "\"

   IF scPath == NIL
      scPath := cCurDir
   ENDIF

   oDlg := XbpFileDialog():new():create()
   cFile := oDlg:open( scPath + "*.BMP" )

   oDlg:destroy()

   IF cFile <> NIL
      scPath := SubStr( cFile, 1, Rat( "\", cFile ) )
      oBmpRef:loadFile( cFile )
      oBmpRef:draw()
   ENDIF

   CurDir( cCurDir )
RETURN cFile



/*
 * Class for managing database fields which store bitmap data
 */
CLASS BitmapRef FROM XbpStatic, DataRef
   PROTECTED:
   VAR oBmp
   VAR oPS
   VAR lIsEmpty

   EXPORTED:
   VAR autoScale
   METHOD init, create, destroy
   METHOD setdata, getdata, editbuffer, draw, loadFile
ENDCLASS



/*
 * Initialize the object
 */
METHOD BitmapRef:init( oParent, oOwner, aPos, aSize, aPresParem, lVisible )
   ::dataRef:init()
   ::xbpStatic:init( oParent, oOwner, aPos, aSize, aPresParem, lVisible )
   ::xbpStatic:type := XBPSTATIC_TYPE_RAISEDBOX
   ::autoScale := .T.
   ::lIsEmpty  := .T.
   ::oPS       := XbpPresSpace():new()
   ::oBmp      := XbpBitmap():new()
RETURN self



/*
 * Request system resources and define color for displaying
 * an empty database field
 */
METHOD BitmapRef:create( oParent, oOwner, aPos, aSize, aPresParem, lVisible )
   LOCAL aAttr[ GRA_AA_COUNT ]
   ::xbpStatic:create( oParent, oOwner, aPos, aSize, aPresParem, lVisible )

   ::oPS:create( ::xbpStatic:winDevice() )
   ::oBmp:create( ::oPS )

   aAttr[ GRA_AA_COLOR ] := GRA_CLR_PALEGRAY
   ::oPS:setAttrArea( aAttr )
   ::xbpStatic:paint := {|aClip| ::draw(aClip) }
RETURN self



/*
 * Release system resources
 */
METHOD BitmapRef:destroy
   ::oBmp:destroy()
   ::oPS:destroy()
   ::xbpStatic:destroy()
RETURN self



/*
 * Transfer bitmap data from field into buffer
 */
METHOD BitmapRef:setdata( cBuffer )
   IF cBuffer == NIL
      cBuffer := Eval( ::dataLink )
   ENDIF

   IF ! Empty( cBuffer )
      ::oBmp:setBuffer( cBuffer, XBPBMP_FORMAT_WIN3X )
      ::lIsEmpty := .F.
   ELSE
      ::lIsEmpty := .T.
   ENDIF
   ::changed :=  .F.
   ::draw()
RETURN cBuffer



/*
 * Transfer the bitmap buffer into field
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
 * Draw the bitmap and maintain the aspect ratio
 */
METHOD BitmapRef:draw( aClip )
   LOCAL aSize    := ::currentSize()
   LOCAL aTarget, aSource, nAspect, lRet

   /*
    * Prepare clipping path
    */
   DEFAULT aClip TO { 1, 1, aSize[1]-1, aSize[2]-1 }
   GraPathBegin( ::oPS )
   GraBox( ::oPS, { aClip[1]-1, aClip[2]-1 }, { aClip[3]+1, aClip[4]+1 }, GRA_OUTLINE )
   GraPathEnd( ::oPS )
   GraPathClip( ::oPS, .T. )

   GraBox( ::oPS, {1,1}, {aSize[1]-2,aSize[2]-2}, GRA_FILL )

   IF ::lIsEmpty
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

   lRet := ::oBmp:draw( ::oPS, aTarget, aSource, , GRA_BLT_BBO_IGNORE )

   GraPathClip( ::oPS, .F. )

RETURN lRet



/*
 * Load an external bitmap file
 */
METHOD BitmapRef:loadFile( cBitmap )
   IF ( ::changed := ::oBmp:loadFile( cBitmap ) )
      ::lIsEmpty := .F.
   ENDIF
RETURN ::changed
