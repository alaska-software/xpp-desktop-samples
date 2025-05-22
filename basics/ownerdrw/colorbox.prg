//////////////////////////////////////////////////////////////////////
//
//  COLORBOX.PRG
//
//  Copyright:
//   Alaska Software, (c) 1997-2025. All rights reserved.         
//  
//  Contents:
//   This source contains the maintenance code of the owner-drawn 
//   listbox of the OWNERDRW sample application.
//    
//////////////////////////////////////////////////////////////////////

#include "font.ch"
#include "xbp.ch"
#include "gra.ch"
#include "appevent.ch"


// Defines for the spacing between the individual
// elements that make up a listbox entry
#define COLOR_BOX_SPACING    2
#define COLOR_BOX_STRSPACING 8


//
// Declaration of a class derived from XbpListbox.
// The class implements a listbox for displaying
// predefined colors.
//
CLASS XbpColorListbox FROM XbpListbox
  PROTECTED:
   VAR    aColors

  EXPORTED:
   METHOD MeasureItem()
   METHOD DrawItem()

   METHOD               init()
   METHOD               create()

ENDCLASS


//
// Overloaded :Init() method. It initializes
// the internal color array and set owner-drawing
// mode
//
METHOD XbpColorListbox:Init( oParent, oOwner, aPos, aSize, aPP, lVisible )

   ::XbpListbox:Init( oParent, oOwner, aPos, aSize, aPP, lVisible )

   ::drawMode := XBP_DRAW_OWNER

   ::aColors := {;
                    { GraMakeRGBColor({200,200,255}), "Blue #1" }, ;
                    { GraMakeRGBColor({160,160,255}), "Blue #2" }, ;
                    { GraMakeRGBColor({120,120,255}), "Blue #3" }, ;
                    { GraMakeRGBColor({ 80, 80,255}), "Blue #4" }, ;
                    { GraMakeRGBColor({ 40, 40,255}), "Blue #5" }, ;
                    { GraMakeRGBColor({  0,  0,255}), "Blue #6" } ;
                  }

RETURN self


//
// Overloaded :Create() method. The method adds
// the items for the predefined colors to the
// listbox.
//
METHOD XbpColorListbox:Create( oParent, oOwner, aPos, aSize, aPP, lVisible )

  LOCAL i

   ::XbpListbox:Create( oParent, oOwner, aPos, aSize, aPP, lVisible )

   FOR i:=1 TO Len(::aColors) 
      ::addItem( ::aColors[i][2] )
   NEXT

   ::SetData( 1 )

RETURN self


//
// Overloaded :MeasureItem() method for computing
// the dimensions of the list entries. The method
// uses GraQueryTextBox() to compute the textbox
// required for rendering a color's name and
// adds the space required for drawing the color
// box.
//
METHOD XbpColorListbox:MeasureItem( mp1, mp2 )

   LOCAL oPS
   LOCAL oFont, oFontPrev
   LOCAL aBox


   oPS    := AppDesktop():lockPS()
   oFont  := ::SetFont()
   IF oFont != NIL
      GraSetFont( oPS, oFont )
   ENDIF
      
   aBox   := GraQueryTextBox( oPS, ::aColors[1][2] )
   AppDesktop():unlockPS()
   
   mp2[2]  := aBox[3][2] - aBox[2][2] + COLOR_BOX_SPACING *4
   mp2[1]  := mp2[2] *2

RETURN ::XbpListbox:MeasureItem(mp1, mp2)


//
// Overloaded :DrawItem() method. This method
// does the actual rendering of the individual
// list items with respect to their respective
// selection state. It uses Grabox() for 
// rendering the item background and color
// boxes. For outputting the color names, 
// GraStringAt() is used.
//
METHOD XbpColorListbox:DrawItem( mp1, mp2 )

   LOCAL aPosStart    
   LOCAL aPosEnd      
   LOCAL aAreaAttrs   := Array( GRA_AA_COUNT )
   LOCAL aAreaAttrs2  := Array( GRA_AA_COUNT )
   LOCAL aStringAttrs := Array( GRA_AS_COUNT )
   LOCAL oFont        
   LOCAL nItem
   LOCAL nHeight      := mp2[4][4] - mp2[4][2] - COLOR_BOX_SPACING *2


   // Select background color depending on
   // the item's selection state and render
   // the background. Selected items are 
   // rendered using a special background 
   // color.
   IF BAND(mp2[3],XBP_DRAWSTATE_SELECTED) != 0
      aAreaAttrs2[GRA_AA_COLOR] := XBPSYSCLR_HILITEBACKGROUND
   ELSE
      aAreaAttrs2[GRA_AA_COLOR] := XBPSYSCLR_WINDOW
   ENDIF

   GraSetAttrArea( mp1, aAreaAttrs2 )
   GraBox( mp1, {mp2[4][1],mp2[4][2]}, {mp2[4][3],mp2[4][4]}, GRA_FILL )

   // Draw a focus rectangle if the listbox
   // currently has the input focus.
   IF BAND(mp2[3],XBP_DRAWSTATE_FOCUS) != 0
      mp1:drawFocusRect( {mp2[4][1],mp2[4][2]},{mp2[4][3],mp2[4][4]}   )
   ENDIF

   // Draw a colored rectangle and output the 
   // name of the color next to it. 
   IF mp2[1] == 0
      RETURN self
   ENDIF

   aPosStart := {mp2[4][1]    + COLOR_BOX_SPACING, ;
                 mp2[4][2]    + COLOR_BOX_SPACING}
   aPosEnd   := {aPosStart[1] + nHeight *2,   ;
                 aPosStart[2] + nHeight }

   aAreaAttrs[GRA_AA_COLOR] := ::aColors[mp2[1]][1]
   GraSetAttrArea( mp1, aAreaAttrs )   
   GraBox( mp1, aPosStart, aPosEnd, GRA_OUTLINEFILL )

   aPosStart[1] := aPosEnd[1] + COLOR_BOX_STRSPACING
   aPosStart[2] += (aPosEnd[2] - aPosStart[2]) /2
   
   IF BAND(mp2[3],XBP_DRAWSTATE_DISABLED) != 0
      aStringAttrs[GRA_AS_COLOR] := XBPSYSCLR_INACTIVETITLETEXT
   ELSEIF BAND(mp2[3],XBP_DRAWSTATE_SELECTED) != 0
      aStringAttrs[GRA_AS_COLOR] := XBPSYSCLR_WINDOW
   ENDIF
   aStringAttrs[GRA_AS_VERTALIGN] := GRA_VALIGN_HALF
   GraSetAttrString( mp1, aStringAttrs )

   oFont := ::SetFont()
   IF oFont != NIL
      GraSetFont( mp1, oFont )
   ENDIF

   GraStringAt( mp1, aPosStart, ::aColors[mp2[1]][2] )

RETURN ::XbpListbox:DrawItem(mp1, mp2)

