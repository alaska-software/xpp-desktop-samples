//////////////////////////////////////////////////////////////////////
//
//  MDIPART.PRG
//
//  Copyright:
//      Alaska Software, (c) 1997-2025. All rights reserved.         
//  
//  Contents:
//      Definition of a parts data entry screen for MDIDEMO.EXE
//   
//////////////////////////////////////////////////////////////////////


#include "Gra.ch"
#include "Xbp.ch"
#include "Common.ch"
#include "Mdidemo.ch"
#include "Appevent.ch"

#pragma library("XppUi2")

PROCEDURE Parts( nRecno )
   LOCAL oDlg, oXbp, oStatic, drawingArea, oBitmapRef, oFocus, bContextMenu, aStatic := {}
   FIELD PARTNO, PARTNAME, PARTTYPE, PURCHASE, SELLPRICE, MARGIN, BMPIMAGE

   IF ! OpenParts( nRecno )          // Open parts data base
      RETURN
   ENDIF

   oDlg := DataDialog():new( GetApplication():MainForm:DrawingArea,, ;
                             NIL, {635,315},, .F.  )
   oDlg:title := "Part No: "+ LTrim( PARTNO )
   oDlg:icon  := ICON_PARTS
   oDlg:create()
   oDlg:seekOrder := 1               // Data for searching a record
   oDlg:seekExpr  := {|x| Upper(x) } // using the SeekDialog
   oDlg:seekFields:= {"PARTNO","PARTNAME"}
   oDlg:seekTitle :=  "Seek part number"

   drawingArea := oDlg:drawingArea   // Get drawing area from dialog
   drawingArea:setFontCompoundName( "8.Helv" )


   // Activate context menu with right mouse click
   bContextMenu       := {|mp1,mp2,obj| SetAppFocus( oDlg )           , ;
                                        ContextMenu():cargo := oDlg   , ;
                                        ContextMenu():popup( obj, mp1 ) }
   drawingArea:RbDown := bContextMenu

                                     // Create group box for fields
   oStatic := XbpStatic():new( drawingArea,, {13,27}, {288,247} )
   oStatic:caption := "Part"
   oStatic:type := XBPSTATIC_TYPE_GROUPBOX
   oStatic:create()
   AAdd( aStatic, oStatic )

   oXbp := XbpStatic():new( oStatic,, {14,197}, {94,22} )
   oXbp:caption := "Part No:"
   oXbp:options := XBPSTATIC_TEXT_RIGHT
   oXbp:create()
   AAdd( aStatic, oXbp )

   oXbp := XbpSLE():new( oStatic,, {114,197}, {85,22} )
   oXbp:bufferLength := 8
   oXbp:group    := XBP_BEGIN_GROUP
   oXbp:tabStop  := .T.
   oXbp:dataLink := {|x| IIf( x==NIL, Trim( PARTNO ), PARTNO := x ) }
   oXbp:validate := {|o| oDlg:IsIndexUnique( 1, PadR(RTrim(o:editBuffer()),8) ) }
   oXbp:create():setData()
   oDlg:addEditControl( oXbp )       // register Xbp as EditControl
   oDlg:addAppendControl( oXbp )     // and AppendControl

   IF ! Eof()                        // PartNo may be edited only
      oXbp:disable()                 // with a new part
   ELSE
      oFocus := oXbp
   ENDIF

   oXbp := XbpStatic():new( oStatic,, {14,167}, {94,22} )
   oXbp:caption := "Part Name:"
   oXbp:options := XBPSTATIC_TEXT_RIGHT
   oXbp:create()
   AAdd( aStatic, oXbp )

   oXbp := XbpSLE():new( oStatic,, {114,167}, {152,22} )
   oXbp:bufferLength := 20
   oXbp:tabStop  := .T.
   oXbp:group    := XBP_WITHIN_GROUP
   oXbp:dataLink := {|x| IIf( x==NIL, Trim( PARTNAME ), PARTNAME := x ) }
   oXbp:create():setData()
   oDlg:addEditControl( oXbp )

   IF ! Eof()
      oFocus := oXbp
   ENDIF

   oXbp := XbpStatic():new( oStatic,, {14,137}, {94,22} )
   oXbp:caption := "Type:"
   oXbp:options := XBPSTATIC_TEXT_RIGHT
   oXbp:create()
   AAdd( aStatic, oXbp )

   oXbp := XbpSLE():new( oStatic,, {114,137}, {152,22} )
   oXbp:bufferLength := 20
   oXbp:group    := XBP_WITHIN_GROUP
   oXbp:dataLink := {|x| IIf( x==NIL, Trim( PARTTYPE ), PARTTYPE := x ) }
   oXbp:create():setData()
   oDlg:addEditControl( oXbp )

   oXbp := XbpStatic():new( oStatic,, {14,107}, {94,22} )
   oXbp:caption := "Purchase:"
   oXbp:options := XBPSTATIC_TEXT_RIGHT
   oXbp:create()
   AAdd( aStatic, oXbp )

   oXbp := XbpSLE():new( oStatic,, {114,107}, {83,22} )
   oXbp:bufferLength := 8
   oXbp:group    := XBP_WITHIN_GROUP
   oXbp:dataLink := {|x| IIf( x==NIL, LTrim(Str( PURCHASE, 8, 2 )) , PURCHASE := Val( x ) ) }
   oXbp:create():setData()
   oDlg:addEditControl( oXbp )

   oXbp := XbpStatic():new( oStatic,, {14,77}, {94,22} )
   oXbp:caption := "Sell Price:"
   oXbp:options := XBPSTATIC_TEXT_RIGHT
   oXbp:create()
   AAdd( aStatic, oXbp )

   oXbp := XbpSLE():new( oStatic,, {114,77}, {84,22} )
   oXbp:bufferLength := 8
   oXbp:group    := XBP_WITHIN_GROUP
   oXbp:dataLink := {|x| IIf( x==NIL, LTrim(Str( SELLPRICE, 8, 2 )) , SELLPRICE := Val( x ) ) }
   oXbp:create():setData()
   oDlg:addEditControl( oXbp )

   oXbp := XbpStatic():new( oStatic,, {14,47}, {94,22} )
   oXbp:caption := "Margin:"
   oXbp:options := XBPSTATIC_TEXT_RIGHT
   oXbp:create()
   AAdd( aStatic, oXbp )

   oXbp := XbpSLE():new( oStatic,, {114,47}, {48,22} )
   oXbp:bufferLength := 4
   oXbp:group    := XBP_WITHIN_GROUP
   oXbp:dataLink := {|x| IIf( x==NIL, LTrim(Str( MARGIN, 4, 1 )) , MARGIN := Val( x ) ) }
   oXbp:create():setData()
   oDlg:addEditControl( oXbp )

   oXbp := XbpPushButton():new( oStatic,, {114,12}, {152,22} )
   oXbp:caption  := "Image"
   oXbp:activate := {|| GetBitmapFile( oBitmapRef ) }
   oXbp:group    := XBP_END_GROUP
   oXbp:create()
   oDlg:addKeyControl( oXbp )

   oBitmapRef := BitmapRef():new( drawingArea,, {310,27}, {295,238} )
   oBitmapRef:dataLink := {|x| IIf( x== NIL, BMPIMAGE, BMPIMAGE := x ) }
   oBitmapRef:create():setData()
   oDlg:addEditControl( oBitmapRef )
   AAdd( aStatic, oBitmapRef )

   AEval( aStatic, {|o| o:rbDown := bContextMenu } )

   oDlg:show()
   oDlg:contextMenu := ContextMenu()
   oDlg:windowMenu  := WinMenu()
   WinMenu():addItem( oDlg )


   // Change window title and item in window menu
   oDlg:newTitle := {|obj| ;
        obj:setTitle( "Part No: "+LTrim(PARTNO) ), ;
        WinMenu():setItem( obj ) }

   WinMenu():checkItem( oDlg )
   oDlg:currentControl := oFocus
   SetAppFocus( oDlg )

RETURN



/*
 * Open parts database
 */
FUNCTION OpenParts( nRecno )
   LOCAL nOldArea := Select(), lDone := .F.

 * We need to remove the VIA clause as it forces the USE
 * to work based on the FOXDBE. Without the VIA clause
 * the current default DatabaseEngine (pgdbe) is used.
 */
#ifdef USE_POSTGRES
   USE Parts NEW
#else
   USE Parts NEW VIA FOXCDX
#endif
   IF ! NetErr()
      SET INDEX TO Parts
      IF nRecno <> NIL
         DbGoto( nRecno )
      ENDIF
      lDone := .T.
   ELSE
      DbSelectArea( nOldArea )
      MsgBox( "Table can not be opened" )
   ENDIF

RETURN lDone



/*
 * Create context menu for parts dialog
 */
STATIC FUNCTION ContextMenu()
   STATIC soMenu

   IF soMenu == NIL
      soMenu       := DataDialogMenu():new()
      soMenu:title := "Parts context menu"
      soMenu:create()
   ENDIF

RETURN soMenu




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
   oDlg:fileFilters := { {"Image formats","*.bmp;*.jpg;*.png;*.gif"} }
   cFile := oDlg:open( scPath )

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
   ::xbpStatic:type := XBPSTATIC_TYPE_RECESSEDBOX
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

   aAttr[ GRA_AA_COLOR ] := GRA_CLR_BACKGROUND
   ::oPS:setAttrArea( aAttr )
   ::xbpStatic:paint := {|aClip| ::draw(aClip) }
RETURN self



/*
 * Release system resources
 */
METHOD BitmapRef:destroy
   ::oBmp:destroy()
   ::oPS:destroy()
   ::xbpStatic:paint := NIL
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
      ::oBmp:setBuffer( cBuffer, XBPBMP_FORMAT_PNG )
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
   LOCAL cBuffer := ::oBmp:setBuffer( ,XBPBMP_FORMAT_PNG)

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
