//////////////////////////////////////////////////////////////////////
//
//  SDICUST.PRG
//
//  Copyright:
//      Alaska Software, (c) 1997-2025. All rights reserved.         
//  
//  Contents:
//      Definition of a customer data entry screen for SDIDEMO.EXE
//   
//////////////////////////////////////////////////////////////////////



#include "Xbp.ch"
#include "Sdidemo.ch"

/*
 * Customer dialog
 */
PROCEDURE Customer( oDlg )
   LOCAL oXbp, oStatic, drawingArea
   LOCAL aControls, bKeyhandler

   FIELD CUSTNO, MR_MRS, LASTNAME, FIRSTNAME, STREET, CITY, ZIP , ;
         PHONE , FAX   , NOTES   , BLOCKED  , TOTALSALES

   drawingArea := oDlg:drawingArea

   IF Empty( drawingArea:childList() )
      oDlg:cargo   := { Recno(), Recno() }
      aControls    := {}

      oStatic      := XbpStatic():new( drawingArea,, {12,277}, {579,58} )
      oStatic:type := XBPSTATIC_TYPE_GROUPBOX
      oStatic:create()
   
      oXbp         := XbpStatic():new( oStatic,, {9,14}, {80,22} )
      oXbp:caption := "Cust.No.:"
      oXbp:options := XBPSTATIC_TEXT_RIGHT
      oXbp:create()
   
      oXbp := XbpSLE():new( oStatic,, {95,14}, {68,22} )
      oXbp:bufferLength := 6
      oXbp:tabStop  := .T.
      oXbp:group    := XBP_BEGIN_GROUP
      oXbp:dataLink := {|x| IIf( x==NIL, Trim(CUSTNO), CUSTNO := PadL(Trim(x),6) ) }
      oXbp:validate := {|o| IsIndexUnique( 1, Padl(Trim(o:editBuffer()),6) ) }
      oXbp:create():setData()
      oXbp:setName( ID_SLE_PRIMKEY )
      AAdd( aControls, oXbp )

      IF ! Eof()
         oXbp:disable()
      ELSE
         SetAppFocus( oXbp )
      ENDIF

      oXbp := XbpCheckbox():new( oStatic,, {209,14}, {94,20} )
      oXbp:dataLink := {|x| IIf( x==NIL, BLOCKED, BLOCKED := x ) }
      oXbp:caption  := "Blocked"
      oXbp:tabStop  := .T.
      oXbp:group    := XBP_WITHIN_GROUP
      oXbp:selected := {|| EnableXbp( oDlg, ID_BTN_UNDO ) }
      oXbp:create():setData()
      AAdd( aControls, oXbp )
   
      oXbp := XbpStatic():new( oStatic,, {339,14}, {80,22} )
      oXbp:caption := "Sales:"
      oXbp:options := XBPSTATIC_TEXT_RIGHT
      oXbp:create()
   
      oXbp := XbpSLE():new( oStatic,, {426,14}, {144,22} )
      oXbp:bufferLength := 8
      oXbp:group    := XBP_WITHIN_GROUP
      oXbp:dataLink := {|x| IIf( x==NIL, Transform(TOTALSALES, "@N"), TOTALSALES := Val(x) ) }
      oXbp:create():setData()
      AAdd( aControls, oXbp )
      oXbp:disable()

      oStatic := XbpStatic():new( drawingArea,, {11,64}, {288,208} )
      oStatic:caption := "Address:"
      oStatic:type := XBPSTATIC_TYPE_GROUPBOX
      oStatic:create()
   
      oXbp := XbpStatic():new( oStatic,, {5,165}, {80,22} )
      oXbp:caption := "Mr/Mrs:"
      oXbp:options := XBPSTATIC_TEXT_RIGHT
      oXbp:create()
   
      oXbp := XbpSLE():new( oStatic,, {95,165}, {180,22} )
      oXbp:bufferLength := 20
      oXbp:tabStop  := .T.
      oXbp:group    := XBP_WITHIN_GROUP
      oXbp:dataLink := {|x| IIf( x==NIL, Trim(MR_MRS), MR_MRS := x ) }
      oXbp:create():setData()
      AAdd( aControls, oXbp )
   
      oXbp := XbpStatic():new( oStatic,, {5,135}, {80,22} )
      oXbp:caption := "Lastname:"
      oXbp:options := XBPSTATIC_TEXT_RIGHT
      oXbp:create()
   
      oXbp := XbpSLE():new( oStatic,, {95,135}, {180,22} )
      oXbp:bufferLength := 20
      oXbp:group    := XBP_WITHIN_GROUP
      oXbp:dataLink := {|x| IIf( x==NIL, Trim(LASTNAME), LASTNAME := x ) }
      oXbp:create():setData()
      AAdd( aControls, oXbp )
      IF ! Eof()
         SetAppFocus( oXbp )
      ENDIF
   
      oXbp := XbpStatic():new( oStatic,, {5,105}, {80,22} )
      oXbp:caption := "Firstname:"
      oXbp:options := XBPSTATIC_TEXT_RIGHT
      oXbp:create()
   
      oXbp := XbpSLE():new( oStatic,, {95,105}, {180,22} )
      oXbp:bufferLength := 20
      oXbp:group    := XBP_WITHIN_GROUP
      oXbp:dataLink := {|x| IIf( x==NIL, Trim(FIRSTNAME), FIRSTNAME := x ) }
      oXbp:create():setData()
      AAdd( aControls, oXbp )
   
      oXbp := XbpStatic():new( oStatic,, {5,75}, {80,22} )
      oXbp:caption := "Street:"
      oXbp:options := XBPSTATIC_TEXT_RIGHT
      oXbp:create()
   
      oXbp := XbpSLE():new( oStatic,, {95,75}, {178,22} )
      oXbp:bufferLength := 30
      oXbp:group    := XBP_WITHIN_GROUP
      oXbp:dataLink := {|x| IIf( x==NIL, Trim(STREET), STREET := x ) }
      oXbp:create():setData()
      AAdd( aControls, oXbp )
   
      oXbp := XbpStatic():new( oStatic,, {5,45}, {80,22} )
      oXbp:caption := "Zip:"
      oXbp:options := XBPSTATIC_TEXT_RIGHT
      oXbp:create()
   
      oXbp := XbpSLE():new( oStatic,, {95,45}, {60,22} )
      oXbp:bufferLength := 5
      oXbp:group    := XBP_WITHIN_GROUP
      oXbp:dataLink := {|x| IIf( x==NIL, Trim(ZIP), ZIP := x ) }
      oXbp:create():setData()
      AAdd( aControls, oXbp )
   
      oXbp := XbpStatic():new( oStatic,, {5,15}, {80,22} )
      oXbp:caption := "City:"
      oXbp:options := XBPSTATIC_TEXT_RIGHT
      oXbp:create()
   
      oXbp := XbpSLE():new( oStatic,, {95,15}, {180,22} )
      oXbp:bufferLength := 30
      oXbp:group    := XBP_WITHIN_GROUP
      oXbp:dataLink := {|x| IIf( x==NIL, Trim(CITY), CITY := x ) }
      oXbp:create():setData()
      AAdd( aControls, oXbp )
   
      oStatic := XbpStatic():new( drawingArea,, {315,64}, {276,209} )
      oStatic:caption := "Notes:"
      oStatic:type := XBPSTATIC_TYPE_GROUPBOX
      oStatic:create()
   
      oXbp := XbpMle():new( oStatic,, {7,70}, {253,115} )
      oXbp:dataLink  := {|x| IIf( x==NIL, NOTES, NOTES := x ) }
      oXbp:ignoreTab := .T.
      oXbp:tabStop   := .T.
      oXbp:group     := XBP_WITHIN_GROUP
      oXbp:create():setData()
      AAdd( aControls, oXbp )
   
      oXbp := XbpStatic():new( oStatic,, {7,40}, {80,22} )
      oXbp:caption := "Phone:"
      oXbp:options := XBPSTATIC_TEXT_RIGHT
      oXbp:create()
   
      oXbp := XbpSLE():new( oStatic,, {95,40}, {140,22} )
      oXbp:bufferLength := 15
      oXbp:group    := XBP_WITHIN_GROUP
      oXbp:dataLink := {|x| IIf( x==NIL, Trim(PHONE), PHONE := x ) }
      oXbp:create():setData()
      AAdd( aControls, oXbp )
   
      oXbp := XbpStatic():new( oStatic,, {7,10}, {80,22} )
      oXbp:caption := "Fax:"
      oXbp:options := XBPSTATIC_TEXT_RIGHT
      oXbp:create()
   
      oXbp := XbpSLE():new( oStatic,, {95,10}, {140,22} )
      oXbp:bufferLength := 15
      oXbp:group    := XBP_END_GROUP
      oXbp:dataLink := {|x| IIf( x==NIL, Trim(FAX), FAX := x ) }
      oXbp:create():setData()
      AAdd( aControls, oXbp )
 
      oXbp := XbpPushButton():new( drawingArea,, {15,10}, {90,30} )
      oXbp:caption := "Previous"
      oXbp:group   := XBP_BEGIN_GROUP
      oXbp:create()
      oXbp:setName( ID_BTN_PREVIOUS )
      oXbp:activate := {|| DlgSkip( oDlg, -1) }
      AAdd( aControls, oXbp )

      oXbp := XbpPushButton():new( drawingArea,, {135,10}, {90,30} )
      oXbp:caption := "Next"
      oXbp:group   := XBP_WITHIN_GROUP
      oXbp:create()
      oXbp:setName( ID_BTN_NEXT )
      oXbp:activate :=  {|| DlgSkip( oDlg, 1) }
      AAdd( aControls, oXbp )

      oXbp := XbpPushButton():new( drawingArea,, {255,10}, {90,30} )
      oXbp:caption  := "Save"
      oXbp:tabStop  := .T.
      oXbp:group    := XBP_WITHIN_GROUP
      oXbp:activate := {|| DlgSave( oDlg ) }
      oXbp:create():disable()
      oXbp:setName( ID_BTN_SAVE )
      AAdd( aControls, oXbp )

      oXbp := XbpPushButton():new( drawingArea,, {375,10}, {90,30} )
      oXbp:caption  := "Undo"
      oXbp:group    := XBP_WITHIN_GROUP
      oXbp:activate := {|| DlgUndo( oDlg ) }
      oXbp:create():disable()
      oXbp:setName( ID_BTN_UNDO )
      AAdd( aControls, oXbp )

      oXbp := XbpPushButton():new( drawingArea,, {505,10}, {90,30} )
      oXbp:caption := "Help"
      oXbp:group   := XBP_END_GROUP
      oXbp:create()
      oXbp:activate := {|| HelpObject():showHelpContents() }
      AAdd( aControls, oXbp )

      bKeyHandler := {|nKey,x,obj| DlgKeyhandler( nKey, obj, aControls, oDlg ) }
      AEval( aControls, {|o| o:keyBoard := bKeyHandler } )
   ENDIF   
RETURN

