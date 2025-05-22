//////////////////////////////////////////////////////////////////////
//
//  SAVEGRA.PRG
//
//  Copyright:
//      Alaska Software, (c) 1997-2025. All rights reserved.         
//  
//  Contents:
//      This program creates an array which contains code blocks
//      with graphics primitives and saves the array to a file.
//      The array can be loaded and displayed using LOADGRA.EXE.
//   
//  Remarks:
//      The example must be linked with /PMTYPE:PM.
//   
//////////////////////////////////////////////////////////////////////

/*
 * Include files
 */
#include "gra.ch"
#include "appevent.ch"
#include "box.ch"
#include "xbp.ch"
#include "font.ch"


MEMVAR aDrawCodeblocks

*******************************************************************************
/*
 * Main procedure
 */
*******************************************************************************
PROCEDURE Main( cOutputFile )

   LOCAL nEvent, mp1 := NIL, mp2 := NIL, oXbp := NIL, bBlock
   LOCAL aAttr := Array( GRA_AM_COUNT )
   /* Exit flag for the event loop      */
   LOCAL lContinue := .T.
   /* Popup menu                        */
   LOCAL oMenu
   /* Codeblock with the current drawing*/
   /* function                          */
   LOCAL bDrawFunc
   /* Array with the parameters for the */
   /* current drawing function          */
   LOCAL aDrawPoints:= {}

   /* Array with the codeblocks to      */
   /* execute the various drawing       */
   /* functions                         */
   PRIVATE aDrawCodeblocks := {}


   /*
    * Check parameters passed to the program. If no
    * file name was passed, set the default
    */
   IF Valtype( cOutputFile ) != 'C'
      cOutputFile := "test.xpf"
   ENDIF

   /*
    * Display usage information
    */
   SetColor( "W/B" )
   @ MaxRow()-2,  0 CLEAR
   @ MaxRow()-2, 27 SAY "SAVEGRA Sample Application"
   @ MaxRow()-1,  9 SAY "Left button  - Select coordinates to"+;
                        " be used for rendering"
   @ MaxRow(),    9 SAY "Right button - Select current drawing operation"
   SetColor( "W/N" )

   /* Activate the mouse and request graphic window */
   /* coordinates                                   */
   SetMouse( .T. )
   SetAppWindow():mouseMode := XBPCRT_MOUSEMODE_PM

   /* Switch off automatic text marking */
   SetAppWindow():autoMark  := .F.

   /*
    * Create popup menu for selecting the current
    * drawing function
    */
   oMenu := XbpMenu():New():Create()

   oMenu:addItem ( { "Line      (F2)", ;
                   { || bDrawFunc := {|aPoints| DrawLine(aPoints)   } } } )
   oMenu:addItem ( { "Spline    (F3)", ;
                   { || bDrawFunc := {|aPoints| DrawSpline(aPoints) } } } )
   oMenu:addItem ( { "Circle    (F4)", ;
                   { || bDrawFunc := {|aPoints| DrawCircle(aPoints) } } } )
   oMenu:addItem ( { "Rectangle (F5)", ;
                   { || bDrawFunc := {|aPoints| DrawBox(aPoints)    } } } )
   oMenu:addItem ( { "Text      (F6)", ;
                   { || bDrawFunc := {|aPoints| DrawText(aPoints)   } } } )
   oMenu:addItem ( { , , XBPMENUBAR_MIS_SEPARATOR } )
   oMenu:addItem ( { "Exit     (ESC)", ;
                   { || lContinue := .F. } } )

   bDrawFunc := {|aPoints| drawLine(aPoints) }

   /*
    * Event loop
    */
   DO WHILE lContinue
      nEvent := AppEvent( @mp1, @mp2, @oXbp )

      DO CASE
      CASE nEvent == xbeM_LbDown
         /*
          * Left button was pressed; mark and save
          * current cursor position
          */
         aAttr[ GRA_AM_COLOR  ] := GRA_CLR_RED
         aAttr[ GRA_AM_SYMBOL ] := GRA_MARKSYM_SIXPOINTSTAR

         GraSetAttrMarker ( , aAttr )
         GraMarker( , mp1 )
         AAdd( aDrawPoints, mp1 )

         bBlock := Eval( bDrawFunc, aDrawPoints )
         IF Valtype( bBlock ) == 'B'
            /* We have collected enough coordinates to */
            /* execute the current drawing function    */

            ASize( aDrawPoints, 0 )
            Eval( bBlock )

            /* Add code block to array with the drawing */
            /* functions                                */
            AAdd( aDrawCodeblocks, bBlock)
         ENDIF

      CASE nEvent == xbeM_RbDown
         /*
          * Right button was pressed; delete all coordinates
          * already saved and display the popup
          * menu
          */
         ClearPoints( @aDrawPoints )

         oMenu:popUp ( , mp1, 1, ;
                       XBPMENU_PU_DEFAULT + XBPMENU_PU_MOUSE_RBDOWN )

      CASE nEvent == xbeK_F1
         /*
          * Key pressed: F1 - reserved for help
          */
          ClearPoints( @aDrawPoints )

      CASE nEvent == xbeK_F2
         /*
          * Key pressed: F2 - draw line
          */
         ClearPoints( @aDrawPoints )
         bDrawFunc := {|aPoints| drawLine( aPoints ) }

      CASE nEvent == xbeK_F3
         /*
          * Key pressed: F3 - draw spline
          */
        ClearPoints( @aDrawPoints )
        bDrawFunc := {|aPoints| drawSpline( aPoints ) }

      CASE nEvent == xbeK_F4
         /*
          * Key pressed: F4 - draw circle
          */
         ClearPoints( @aDrawPoints )
         bDrawFunc := {|aPoints| drawCircle( aPoints ) }

      CASE nEvent == xbeK_F5
         /*
          * Key pressed: F5 - draw rectangle
          */
         ClearPoints( @aDrawPoints )
         bDrawFunc := {|aPoints| drawBox( aPoints ) }

      CASE nEvent == xbeK_F6
         /*
          * Key pressed: F6 - draw text
          */
         ClearPoints( @aDrawPoints )
         bDrawFunc := {|aPoints| drawText( aPoints ) }

      CASE nEvent == xbeK_ESC
         /*
          * Key pressed: ESC - exit event loop
          */
         ClearPoints( @aDrawPoints )
         lContinue := .F.

      CASE nEvent == xbeP_Close
         /*
          * Close button of window was clicked; exit
          * event loop
          */
         ClearPoints( @aDrawPoints )
         lContinue := .F.

      OTHERWISE
          /*
           * Default handling of this event
           */
           IF ( oXbp != NIL )
            oXbp:HandleEvent ( nEvent, mp1, mp2 )
         ENDIF
      ENDCASE

   ENDDO

   /*
    * Finished collecting drawing operations;
    * save array that contains the graphic code blocks
    */
   IF Len(aDrawCodeblocks) > 0
      SAVE ALL LIKE aDrawCodeblocks TO (cOutputFile)
   ENDIF
   /*
    * Display status information
    */
    SetColor( "W/B" )
    @ MaxRow()-1,  0 CLEAR

    IF Len(aDrawCodeblocks) > 0
       @ MaxRow()-1,  6 SAY "The GRA primitives necessary to reconstruct your" +;
                            " picture have been"
       @ MaxRow()  ,  6 SAY "saved to disk. Use LOADGRA.EXE to redisplay it."  +;
                            " Please hit a key..."
    ELSE
       @ MaxRow()-1,  6 SAY "Quit. Nothing has been saved. Please hit a key..."
    ENDIF

    INKEY( 0 )

RETURN


*******************************************************************************
/*
 * Functions to create codeblocks with GRA primitives
 */
*******************************************************************************

****************************
FUNCTION DrawLine( aPoints )
  LOCAL aLine, bBlock

  IF Len( aPoints ) == 2
     aLine  := AClone( aPoints )
     bBlock := {|| GraSetColor( , GRA_CLR_PINK ), ;
                   GraLine ( , aLine[1], aLine[2] ) }
  ENDIF
RETURN bBlock



******************************
FUNCTION DrawSpline( aPoints )
  LOCAL aSpline, bBlock

  IF Len( aPoints ) == 4
     aSpline := AClone( aPoints )
     bBlock  := {|| GraSetColor( , GRA_CLR_GREEN ), ;
                    GraSpline  ( , aSpline, .F. ) }
  ENDIF
RETURN bBlock



******************************
FUNCTION DrawCircle( aPoints )
  LOCAL aCenter, nRadius, bBlock

  IF Len( aPoints ) == 2
     aCenter := AClone( aPoints[1] )
     nRadius := Sqrt( (aPoints[1,1]-aPoints[2,1])^2 + (aPoints[1,2]-aPoints[2,2])^2 )
     bBlock  := {|| GraSetColor( , GRA_CLR_CYAN ), ;
                    GraArc( , aCenter, nRadius ) }
  ENDIF
RETURN bBlock



***************************
FUNCTION DrawBox( aPoints )
  LOCAL aBox, bBlock

  IF Len( aPoints ) == 2
     aBox    := AClone( aPoints )
     bBlock := {|| GraSetColor( , GRA_CLR_PALEGRAY ), ;
                   GraBox(, aBox[1], aBox[2], GRA_FILL ) }
  ENDIF
RETURN bBlock



****************************
FUNCTION DrawText( aPoints )
  LOCAL aTextPos, cText := "", cChar, nKey, bBlock

  IF Len( aPoints ) == 1
     aTextPos := AClone( aPoints[1] )

     /*
      * Display text prompt
      */
     GraSetColor( , GRA_CLR_YELLOW )
     GraSetFont ( , XbpFont():New():Create(FONT_DEFPROP_MEDIUM) )
     GraStringAt( , aTextPos, ">" )

     /*
      * Collect all key presses until a special
      * non-alphanumeric character is pressed
      */
     DO WHILE .T.
        nKey := Inkey( 0 )
        IF nKey >= 0 .AND. nKey <= 255
           cChar := Chr( nKey )

           IF IsAlpha( cChar ) .OR. ;
              IsDigit( cChar ) .OR. ;
              cChar $ "/ "

              cText += cChar
              GraStringAt( , aTextPos, Space( Len(cText) ) )
              GraStringAt( , aTextPos, cText )
           ELSE
              EXIT
           ENDIF
        ENDIF
     ENDDO
     GraStringAt( , aTextPos, Space( Len(cText) ) )

     bBlock :=  {|| GraSetColor( , GRA_CLR_YELLOW ), ;
                    GraSetFont ( , XbpFont():New():Create(FONT_DEFPROP_MEDIUM) ), ;
                    GraStringAt( , aTextPos, cText ) }
  ENDIF

RETURN bBlock


*******************************************************************************
/*
 * Function to remove all coordinates currently saved both from the
 * coordinate array passed in as well as from the screen
 */
*******************************************************************************
PROCEDURE ClearPoints( aPoints )

   LOCAL aAttr := Array( GRA_AM_COUNT )
   LOCAL i

   IF Len(aPoints) <> 0
      aAttr[ GRA_AM_COLOR  ] := GRA_CLR_BLACK
      aAttr[ GRA_AM_SYMBOL ] := GRA_MARKSYM_SIXPOINTSTAR
      GraSetAttrMarker ( , aAttr )

      FOR i:=1 TO Len( aPoints )
         GraMarker( , aPoints[i] )
      NEXT

      aPoints := {}
   ENDIF

RETURN
