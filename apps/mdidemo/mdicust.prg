//////////////////////////////////////////////////////////////////////
//
//  MDICUST.PRG
//
//  Copyright:
//      Alaska Software, (c) 1997-2025. All rights reserved.         
//  
//  Contents:
//      Definition of a customer data entry screen for MDIDEMO.EXE
//   
//////////////////////////////////////////////////////////////////////



#include "Xbp.ch"
#include "Mdidemo.ch"
#include "Appevent.ch"


/*
 * Customer dialog
 */
PROCEDURE Customer( nRecno )
   LOCAL oXbp, oStatic, drawingArea, oDlg, oFocus, bContextMenu, aStatic := {}
   FIELD CUSTNO, MR_MRS, LASTNAME, FIRSTNAME, STREET, CITY, ZIP , ;
         PHONE , FAX   , NOTES   , BLOCKED  , TOTALSALES

   IF ! OpenCustomer( nRecno )       // open customer database
      RETURN
   ENDIF

   oDlg := DataDialog():new( GetApplication():MainForm:DrawingArea, , ;
                             NIL , {605,315},, .F.  )
   oDlg:title := "Customer No: "+ LTrim( CUSTNO )
   oDlg:icon  := ICON_CUSTOMER
   oDlg:create()

   oDlg:seekOrder := 2               // Data for searching a record
   oDlg:seekExpr  := {|x| Upper(x) } // using the SeekDialog
   oDlg:seekFields:= {"LASTNAME","FIRSTNAME"}
   oDlg:seekTitle := "Seek lastname"

   drawingArea := oDlg:drawingArea   // Get drawing area from dialog
   drawingArea:setFontCompoundName( "8.Helv" )

   // Activate context menu with right mouse click
   bContextMenu       := {|mp1,mp2,obj| SetAppFocus( oDlg )           , ;
                                        ContextMenu():cargo := oDlg   , ;
                                        ContextMenu():popup( obj, mp1 ) }
   drawingArea:RbDown := bContextMenu

   oStatic := XbpStatic():new( drawingArea ,, {12,227}, {579,58} )
   oStatic:type := XBPSTATIC_TYPE_GROUPBOX
   oStatic:create()
   AAdd( aStatic, oStatic )

   oXbp := XbpStatic():new( oStatic ,, {9,14}, {80,22} )
   oXbp:caption := "Cust.No.:"
   oXbp:options := XBPSTATIC_TEXT_RIGHT
   oXbp:create()
   AAdd( aStatic, oXbp )

   oXbp := XbpSLE():new( oStatic,, {95,14}, {68,22} )
   oXbp:bufferLength := 6
   oXbp:tabStop  := .T.
   oXbp:group    := XBP_BEGIN_GROUP
   oXbp:dataLink := {|x| IIf( x==NIL, LTrim(CUSTNO), CUSTNO := PadL(Trim(x),6) ) }

   // Customer number must be unique
   oXbp:validate := {|o| oDlg:isIndexUnique( 1, Padl(Trim(o:editBuffer()),6) ) }
   oXbp:create():setData()

   oDlg:addEditControl( oXbp )       // register Xbp as EditControl
   oDlg:addAppendControl( oXbp )     // and AppendControl

   IF ! Eof()                        // Customer number may be edited
      oXbp:disable()                 // only with new customer
   ELSE
      oFocus := oXbp
   ENDIF

   oXbp := XbpCheckbox():new( oStatic,, {209,14}, {94,20} )
   oXbp:group    := XBP_WITHIN_GROUP
   oXbp:dataLink := {|x| IIf( x==NIL, BLOCKED, BLOCKED := x ) }
   oXbp:caption := "Blocked"
   oXbp:create():setData()
   oDlg:addEditControl( oXbp )       // register Xbp as EditControl

   oXbp := XbpStatic():new( oStatic,, {339,14}, {80,22} )
   oXbp:caption := "Sales:"
   oXbp:options  := XBPSTATIC_TEXT_RIGHT
   oXbp:create()
   AAdd( aStatic, oXbp )

   oXbp := XbpSLE():new( oStatic,, {426,14}, {144,22} )
   oXbp:bufferLength := 8
   oXbp:group    := XBP_WITHIN_GROUP
   oXbp:dataLink := {|x| IIf( x==NIL, Transform(TOTALSALES, "@N"), TOTALSALES := Val(x) ) }
   oXbp:create():setData()
   oXbp:disable()
   oDlg:addEditControl( oXbp )

   oStatic := XbpStatic():new( drawingArea,, {11,14}, {288,208} )
   oStatic:type := XBPSTATIC_TYPE_GROUPBOX
   oStatic:create()
   AAdd( aStatic, oStatic )

   oXbp := XbpStatic():new( oStatic,, {5,165}, {80,22} )
   oXbp:caption := "Mr/Mrs:"
   oXbp:options := XBPSTATIC_TEXT_RIGHT
   oXbp:create()
   AAdd( aStatic, oXbp )

   oXbp := XbpSLE():new( oStatic,, {95,165}, {180,22} )
   oXbp:bufferLength := 20
   oXbp:tabStop  := .T.
   oXbp:group    := XBP_WITHIN_GROUP
   oXbp:dataLink := {|x| IIf( x==NIL, Trim(MR_MRS), MR_MRS := x ) }
   oXbp:create():setData()
   oDlg:addEditControl( oXbp )

   oXbp := XbpStatic():new( oStatic,, {5,135}, {80,22} )
   oXbp:caption := "Lastname:"
   oXbp:options := XBPSTATIC_TEXT_RIGHT
   oXbp:create()
   AAdd( aStatic, oXbp )

   oXbp := XbpSLE():new( oStatic,, {95,135}, {180,22} )
   oXbp:bufferLength := 20
   oXbp:group    := XBP_WITHIN_GROUP
   oXbp:dataLink := {|x| IIf( x==NIL, Trim(LASTNAME), LASTNAME := x ) }
   oXbp:create():setData()
   oDlg:addEditControl( oXbp )

   IF ! Eof()
      oFocus := oXbp
   ENDIF

   oXbp := XbpStatic():new( oStatic,, {5,105}, {80,22} )
   oXbp:caption := "Firstname:"
   oXbp:options := XBPSTATIC_TEXT_RIGHT
   oXbp:create()
   AAdd( aStatic, oXbp )

   oXbp := XbpSLE():new( oStatic,, {95,105}, {180,22} )
   oXbp:bufferLength := 20
   oXbp:group    := XBP_WITHIN_GROUP
   oXbp:dataLink := {|x| IIf( x==NIL, Trim(FIRSTNAME), FIRSTNAME := x ) }
   oXbp:create():setData()
   oDlg:addEditControl( oXbp )

   oXbp := XbpStatic():new( oStatic,, {5,75}, {80,22} )
   oXbp:caption := "Street:"
   oXbp:options := XBPSTATIC_TEXT_RIGHT
   oXbp:create()
   AAdd( aStatic, oXbp )

   oXbp := XbpSLE():new( oStatic,, {95,75}, {178,22} )
   oXbp:bufferLength := 30
   oXbp:group    := XBP_WITHIN_GROUP
   oXbp:dataLink := {|x| IIf( x==NIL, Trim(STREET), STREET := x ) }
   oXbp:create():setData()
   oDlg:addEditControl( oXbp )

   oXbp := XbpStatic():new( oStatic,, {5,45}, {80,22} )
   oXbp:caption := "Zip:"
   oXbp:options := XBPSTATIC_TEXT_RIGHT
   oXbp:create()
   AAdd( aStatic, oXbp )

   oXbp := XbpSLE():new( oStatic,, {95,45}, {60,22} )
   oXbp:bufferLength := 5
   oXbp:group    := XBP_WITHIN_GROUP
   oXbp:dataLink := {|x| IIf( x==NIL, Trim(ZIP), ZIP := x ) }
   oXbp:create():setData()
   oDlg:addEditControl( oXbp )

   oXbp := XbpStatic():new( oStatic,, {5,15}, {80,22} )
   oXbp:caption := "City:"
   oXbp:options := XBPSTATIC_TEXT_RIGHT
   oXbp:create()
   AAdd( aStatic, oXbp )

   oXbp := XbpSLE():new( oStatic,, {95,15}, {180,22} )
   oXbp:bufferLength := 30
   oXbp:group    := XBP_WITHIN_GROUP
   oXbp:dataLink := {|x| IIf( x==NIL, CITY, CITY := x ) }
   oXbp:create():setData()
   oDlg:addEditControl( oXbp )

   oStatic := XbpStatic():new( drawingArea,, {315,14}, {276,209} )
   oStatic:caption := "Notes:"
   oStatic:type := XBPSTATIC_TYPE_GROUPBOX
   oStatic:create()
   AAdd( aStatic, oStatic )

   oXbp := XbpMle():new( oStatic,, {7,70}, {253,115} )
   oXbp:dataLink := {|x| IIf( x==NIL, NOTES, NOTES := x ) }
   oXbp:ignoreTab:= .T.
   oXbp:tabStop  := .T.
   oXbp:group    := XBP_WITHIN_GROUP
   oXbp:create():setData()
   oDlg:addEditControl( oXbp )

   oXbp := XbpStatic():new( oStatic,, {7,40}, {80,22} )
   oXbp:caption := "Phone:"
   oXbp:options := XBPSTATIC_TEXT_RIGHT
   oXbp:create()
   AAdd( aStatic, oXbp )

   oXbp := XbpSLE():new( oStatic,, {95,40}, {140,22} )
   oXbp:bufferLength := 15
   oXbp:tabStop  := .T.
   oXbp:group    := XBP_WITHIN_GROUP
   oXbp:dataLink := {|x| IIf( x==NIL, Trim(PHONE), PHONE := x ) }
   oXbp:create():setData()
   oDlg:addEditControl( oXbp )

   oXbp := XbpStatic():new( oStatic,, {7,10}, {80,22} )
   oXbp:caption := "Fax:"
   oXbp:options := XBPSTATIC_TEXT_RIGHT
   oXbp:create()
   AAdd( aStatic, oXbp )

   oXbp := XbpSLE():new( oStatic,, {95,10}, {140,22} )
   oXbp:bufferLength := 15
   oXbp:group    := XBP_END_GROUP
   oXbp:dataLink := {|x| IIf( x==NIL, Trim(FAX), FAX := x ) }
   oXbp:create():setData()
   oDlg:addEditControl( oXbp )

   AEval( aStatic, {|o| o:rbDown := bContextMenu } )

   oDlg:show()
   oDlg:contextMenu := ContextMenu()
   oDlg:windowMenu  := WinMenu()
   WinMenu():addItem( oDlg )

   // Create new window title if record changes
   oDlg:newTitle := {|obj| ;
        obj:setTitle( "Customer No: "+LTrim(CUSTNO) ), ;
        WinMenu():setItem( obj ) }
   WinMenu():checkItem( oDlg )

   oDlg:currentControl := oFocus
   SetAppFocus( oDlg )

RETURN



/*
 * Open customer database
 */
FUNCTION OpenCustomer( nRecno )
   LOCAL nOldArea := Select(), lDone := .F.

   USE Customer NEW
   IF ! NetErr()
      SET INDEX TO CustA, CustB
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
 * Create context menu for customer dialog
 */
STATIC FUNCTION ContextMenu()
   STATIC soMenu

   IF soMenu == NIL
      soMenu       := DataDialogMenu():new()
      soMenu:title := "Customer context menu"
      soMenu:create()

   ENDIF

RETURN soMenu
