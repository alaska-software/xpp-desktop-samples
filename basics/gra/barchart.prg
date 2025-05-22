//////////////////////////////////////////////////////////////////////
//
//  BARCHART.PRG
//
//  Copyright:
//      Alaska Software, (c) 1997-2025. All rights reserved.         
//  
//  Contents:
//      This program draws a simple barchart.
//   
//////////////////////////////////////////////////////////////////////

/*
 * Include files:
 */
#include "Gra.ch"
#include "AppEvent.ch"


****************
PROCEDURE Main()

   /* Array containing values for chart     */
   LOCAL aValues
   /* Fill patterns                         */
   LOCAL aAttr

   LOCAL i, imax, nX, dDate
   LOCAL nEvent, mp1, mp2, oXbp

   /* Origin for x and y axis               */
   LOCAL nX0   := 40, nY0   := 30
   /* Relative max. values for x and y      */
   LOCAL nXmax := 580, nYmax := 350
   /* Relative x value for first bar        */
   LOCAL nStart:= 0
   /* X distance between two bars           */
   LOCAL nStep := 45
   /* Width of a bar                        */
   LOCAL nWidth:= 30
   /* Array with fill patterns              */
   LOCAL aSymbols := { GRA_SYM_DENSE1,    ;
                       GRA_SYM_DENSE2,    ;
                       GRA_SYM_DENSE3,    ;
                       GRA_SYM_DENSE4,    ;
                       GRA_SYM_DENSE5,    ;
                       GRA_SYM_DENSE6,    ;
                       GRA_SYM_DENSE7,    ;
                       GRA_SYM_DENSE8,    ;
                       GRA_SYM_VERT,      ;
                       GRA_SYM_HORIZ,     ;
                       GRA_SYM_DIAG1,     ;
                       GRA_SYM_DIAG2,     ;
                       GRA_SYM_DIAG3,     ;
                       GRA_SYM_DIAG4,     ;
                       GRA_SYM_NOSHADE,   ;
                       GRA_SYM_SOLID,     ;
                       GRA_SYM_HALFTONE,  ;
                       GRA_SYM_HATCH,     ;
                       GRA_SYM_DIAGHATCH, ;
                       GRA_SYM_BLANK      }


   /*
    * Enable handling of system shortcuts, eg. for ALT-F4
    */
   SetAppWindow():useShortcuts := .T.

   /* Fill window with pale gray            */
   SetColor( "N/W" )
   CLS

   /*
    * Value set for bars
    */
   aValues := { 184,  84, 144, 254, 170, 235, ;
                289, 190, 152,  36, 107, 128  }

   /*
    * Draw x and y axis
    */
   GraLine( NIL, {nX0, nY0}, {nX0+nXmax, nY0} )
   GraLine( NIL, {nX0, nY0}, {nX0      , nY0+nYMax} )

   aAttr := ARRAY( GRA_AM_COUNT )
   aAttr [ GRA_AM_SYMBOL ] := GRA_MARKSYM_PLUS
   GraSetAttrMarker( NIL, aAttr )

   aAttr := ARRAY( GRA_AS_COUNT )
   aAttr [ GRA_AS_HORIZALIGN ] := GRA_HALIGN_LEFT
   aAttr [ GRA_AS_VERTALIGN ]  := GRA_VALIGN_HALF
   GraSetAttrString( NIL, aAttr )

   /*
    * Draw vertical scale
    */
   i := 50
   DO WHILE i < nY0 + nYmax
      GraMarker  ( NIL, { nX0   , i + nY0 } )
      GraStringAt( NIL, { nX0-35, i + nY0 }, Str(i,3) )
      i += 50
   ENDDO

   aAttr [ GRA_AS_HORIZALIGN ] := GRA_HALIGN_CENTER
   aAttr [ GRA_AS_VERTALIGN ]  := GRA_VALIGN_BOTTOM
   GraSetAttrString( NIL, aAttr )

   aAttr := Array( GRA_AA_COUNT )
   imax  := Len( aValues )
   dDate := StoD( "19960128" )

   FOR i:=1 TO imax
      /*
       * Draw horizontal scale
       */
      nX := nX0 + ( i*nStep )
      GraMarker  ( NIL, { nX-(nWidth/2), nY0 } )
      GraStringAt( NIL, { nX-(nWidth/2), nY0-25 }, ;
                   Substr(CMonth( dDate ),1,3)+"." )
      /*
       * Draw bars using different fill patterns
       */
      aAttr [ GRA_AA_SYMBOL ] := aSymbols[i]
      GraSetAttrArea( NIL, aAttr )

      GraBox( NIL, ;
              { nX-nWidth, nY0 }, ;
              { nX       , nY0 + aValues[i] }, ;
              GRA_OUTLINEFILL )

      /* Set next month                     */
      dDate += 28

   NEXT

   nEvent := xbeP_None
   DO WHILE nEvent != xbeP_Close .AND. nEvent != xbeK_ESC
       nEvent := AppEvent( @mp1, @mp2, @oXbp )
       oXbp:handleEvent( nEvent, mp1, mp2 )
   ENDDO

RETURN
