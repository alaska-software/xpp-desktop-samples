//////////////////////////////////////////////////////////////////////
//
//  RADIOGRP.PRG
//
//  Copyright:
//      Alaska Software, (c) 1997-2025. All rights reserved.         
//  
//  Contents:
//      In this file the XbpRadioGroup class is programmed as a user defined
//      class. It displays a group of radio buttons inside a static frame.
//      The class is an example of multiple inheritance and shows how to
//      inherit from XbpStatic and DataRef.
//      
//      The edited 'value' of XbpRadioGroup is numeric indicating the
//      ordinal position of the currently selected radio button.
//   
//////////////////////////////////////////////////////////////////////



#include "Appevent.ch"
#include "Common.ch"
#include "Xbp.ch"
#include "Gra.ch"

PROCEDURE AppSys
   LOCAL oCrt := XbpCrt():New()
   oCrt:ClipChildren := .F.
   oCrt:Create()
   oCrt:SetTitle( "Radiobutton Groups" )
   oCrt:useShortCuts := .T.
   SetAppWindow( oCrt )
RETURN

PROCEDURE Main
   LOCAL mp1, mp2, nEvent, oXbp, aPresParam
   LOCAL nCom := 2, nDay := 3, aDays

   SetColor( "N/W" )
   CLS

   aDays         := {"Monday", "Tuesday" , "Wednesday", "Thursday", ;
                     "Friday", "Saturday", "Sunday" }

   oXbp          := XbpRadioGroup():new( SetAppWindow(),, {200,100},, aPresParam )
   oXbp:caption  := "Ports"
   oXbp:captions := { "COM1","COM2","COM3","COM4" }
   oXbp:Visible  := .F.
   oXbp:dataLink := {|x| IIf( x==NIL, nCom, nCom := x ) }
   oXbp:selected := {|mp1,mp2,obj| obj:getData(), DispOutAt(1, 0, nCom ) }
   oXbp:create():setData()
   oXbp:Show()

   oXbp          := XbpRadioGroup():new( SetAppWindow(),, {300,100},, aPresParam )
   oXbp:type     := XBPSTATIC_TYPE_RAISEDBOX
   oXbp:captions := aDays
   oXbp:Visible  := .F.
   oXbp:dataLink := {|x| IIf( x==NIL, nDay, nDay := x ) }
   oXbp:selected := {|mp1,mp2,obj| obj:getData(), DispOutAt(2, 0, Padr(aDays[ nDay ],10) ) }
   oXbp:create():setData()
   oXbp:Show()

   SetAppFocus( oXbp )

   DO WHILE nEvent <> xbeP_Close 
      nEvent := AppEvent ( @mp1, @mp2, @oXbp )
      oXbp:handleEvent ( nEvent, mp1, mp2 )
   ENDDO

RETURN



/*
 * Example for a class inheriting DataRef 
 */
CLASS XbpRadioGroup FROM XbpStatic, DataRef

   PROTECTED:
                                       ** Internals
   VAR    radioButtons                 // Array for XbpRadioButton
   VAR    size                         // Initial size
   VAR    fontHeight                   // Height of font
   VAR    current                      // Currently selected radio button
   METHOD calcSize                     // Calculate size of XbpStatic 

   EXPORTED:
                                       ** Life cycle
   METHOD init                         // Initialize object
   METHOD create                       // Request system resources
   METHOD destroy                      // Release system resources
                                       ** Configuration
   VAR    captions                     // Captions for radio buttons
                                       ** Runtime
   VAR    selected                     // Callback: Radio button is selected
   METHOD selected                     // Select radio button
                                       ** Overloaded from DataRef
   METHOD setData                      // Transfer data to buffer
   METHOD getData                      // Transfer data from buffer (-> :dataLink)
   METHOD editBuffer                   // Current value in buffer
                                       ** Overloaded from XbpStatic
   METHOD setInputFocus                // Set focus to radio button
ENDCLASS



/*
 * Initialize object
 */
METHOD XbpRadioGroup:init( oParent, oOwner, aPos, aSize, aPP, lVisible )

   DEFAULT lVisible TO .T.

   ::xbpStatic:init( oParent, oOwner, aPos, aSize, aPP, lVisible )
   ::dataRef:init()

   ::visible      := lVisible
   ::size         := aSize
   ::type         := XBPSTATIC_TYPE_GROUPBOX
   ::caption      := ""
   ::autoSize     := .T.
   ::tabStop      := .T.
   ::captions     := {}
   ::radioButtons := {}
   ::current      := 0

RETURN self



/*
 * Request system resources
 */
METHOD XbpRadioGroup:create( oParent, oOwner, aPos, aSize, aPP, lVisible )
   LOCAL oRadio, i, imax, bSelected

   DEFAULT lVisible TO ::visible, ;
           aSize    TO ::size

   ::visible := lVisible
   ::xbpStatic:create( oParent, oOwner, aPos, aSize, aPP, lVisible )

   ::size := ::calcSize()              // Calculate font metrics
   IF ::autoSize
      ::xbpStatic:setSize( ::size )
   ELSE
      ::size := ::xbpStatic:currentSize()
   ENDIF

   imax           := Len( ::captions ) // Number of radio buttons to create
   ::radioButtons := Array( imax )     // Array for radio buttons
                                       // Callback for radio buttons
   bSelected := {|mp1, mp2, obj| ::selected( mp1, mp2, obj ) }
                                       // Position and size of one radio button
   aPos      := { 4            , ::size[2] - ::fontHeight - 2 }
   aSize     := { ::size[1] - 8, ::fontHeight }

   IF ::type == XBPSTATIC_TYPE_GROUPBOX
      aPos[2] -= ::fontHeight          // Deduct space for caption of group box
   ENDIF

   FOR i:=1 TO imax                    // Radio buttons are contained in self 
      oRadio := XbpRadioButton():new( self, self, aPos, aSize,, .T. )

      IF i == 1                        // Set group flag for Radio buttons
         oRadio:tabstop := .T.
         oRadio:group := XBP_BEGIN_GROUP
      ELSEIF i == imax
         oRadio:group := XBP_END_GROUP
      ELSE
         oRadio:group := XBP_WITHIN_GROUP
      ENDIF

      oRadio:caption  := ::captions[i]
      oRadio:selected := bSelected
      oRadio:create()

      ::radioButtons[i] := oRadio
      aPos[2] -= ::fontHeight          // Y position for next radio button
   NEXT   

RETURN self



/*
 * Release system resources
 */
METHOD XbpRadioGroup:destroy

   ::xbpStatic:destroy()               // Implicitly calls :destroy() for
   ::radioButtons := {}                // contained radio buttons
   ::captions     := {}
   ::size         := NIL

RETURN self



/*
 * Calculate the size for XbpStatic from font metrics so that
 * all radio buttons are visible
 */
METHOD XbpRadioGroup:calcSize
   LOCAL i, imax, nXsize, nYsize, nWidth, aSize

   // calculate size from font metrics
   oPS          := ::xbpStatic:lockPS()
   aBox         := GraQueryTextBox( oPS, "^g" )
   ::fontHeight := aBox[3,2] - aBox[2,2] + 4


   imax   := Len( ::captions )
   nXsize := 0
   nYSize := 0

   FOR i:=1 TO imax
      aBox   := GraQueryTextBox( oPS, ::captions[i] )
      nWidth := aBox[3,1] - aBox[2,1]
      nXsize := Max( nXsize, nWidth )
      nYsize += ::fontHeight
   NEXT

   ::xbpStatic:unLockPS( oPS )

   aSize := { nXsize + 36 , nYsize + 4 }

   IF ::type == XBPSTATIC_TYPE_GROUPBOX
      aSize[2] += ::fontHeight         // Caption of Group box has a size
   ENDIF

RETURN aSize



/*
 * This method is called via callback code block (oRadio:selected) of a
 * single radio button, when it is selected.
 */
METHOD XbpRadioGroup:selected( mp1, mp2, oRadioButton )

   DEFAULT mp1 TO .F.

   IF mp1
      mp2 := AScan( ::radioButtons, oRadioButton )
   ELSE
      mp2 := 0
   ENDIF

   ::setData( mp2 )

   IF Valtype( ::selected ) == "B"
      Eval( ::selected, mp1, mp2, self )
   ENDIF

RETURN self



/*
 * Retrieve ordinal position of the current radio button
 */
METHOD XbpRadioGroup:editBuffer
RETURN ::current



/*
 * Set current radio button
 */
METHOD XbpRadioGroup:setData( nButton )

   DEFAULT nButton TO ::dataRef:setData()

   ::current := ::dataRef:setData( nButton )
   IF ::current > 0 .AND. ::current <= Len( ::radioButtons )
      ::radioButtons[ ::current ]:setData( .T. )
   ENDIF

RETURN  ::current



/*
 * Let DataRef evaluate the :datalink code block
 */
METHOD XbpRadioGroup:getData

   ::current := AScan( ::radioButtons, {|o| o:editBuffer() } ) 

   IF ::current > 0
      ::dataRef:getData()
   ENDIF

RETURN ::current



/*
 * Set input focus to current radio button
 */
METHOD XbpRadioGroup:setInputFocus

   ::current := Max( 1, AScan( ::radioButtons, {|o| o:editBuffer() } ) )

   IF ::current > 0 .AND. ::current <= Len( ::radioButtons )
      SetAppFocus( ::radioButtons[ ::current ] )
   ENDIF

RETURN self
