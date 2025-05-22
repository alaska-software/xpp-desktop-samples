//////////////////////////////////////////////////////////////////////
//
//  MENUTO.PRG
//
//  Copyright:
//     Alaska Software, (c) 1997-2025. All rights reserved.         
//  
//  Contents:
//     Example of the migration of a procedural menu.
//   
//////////////////////////////////////////////////////////////////////




#include "Common.ch"

PROCEDURE Main
   LOCAL nSelect, promptList := {}

   SET MESSAGE TO MaxRow()
   SET WRAP ON
   SetMouse( .T. )

   DO WHILE .T.

      Setcolor( "N/W" )
      CLS

      @ 1, 1 SAY "Example for migrating @ PROMPT, MENU TO using pushbuttons"
      @ 3, 1 SAY "This is the usual appearance of @ PROMPT, MENU TO :"

      SetColor( "N/BG,W+/B" )

      @  8, 32 PROMPT " First prompt  " MESSAGE PadC( "First" , MaxCol()+1 )
      @ 10, 32 PROMPT " Second prompt " MESSAGE PadC( "Second", MaxCol()+1 )
      @ 12, 32 PROMPT " Third prompt  " MESSAGE PadC( "Third" , MaxCol()+1 )
      @ 14, 32 PROMPT " Fourth prompt " MESSAGE PadC( "Fourth", MaxCol()+1 )
      @ 16, 32 PROMPT " Exit          " MESSAGE PadC( "Exit"  , MaxCol()+1 )

      nSelect := 1
      MENU TO nSelect

      DO CASE
      CASE nSelect == 5
         QUIT

      OTHERWISE
         Alert( "Prompt #" + Str(nSelect,1) + " is selected;; " + ;
                "Next comes a GUI look of the MENU TO command" )
      ENDCASE

     /*
      * The following code is the same as above
      * but preprocessed with other directives
      */

      #include "Menuto.ch"

      Setcolor( "N/W" )
      CLS

      @ 1, 1 SAY "Example for migrating @ PROMPT, MENU TO using pushbuttons"
      @ 3, 1 SAY "This is a GUI appearance of @ PROMPT, MENU TO :"

      SetColor( "N/BG,W+/B" )

      @  8, 32 PROMPT " First prompt   " MESSAGE PadC( "First" , MaxCol()+1 )
      @ 10, 32 PROMPT " Second prompt  " MESSAGE PadC( "Second", MaxCol()+1 )
      @ 12, 32 PROMPT " Third prompt   " MESSAGE PadC( "Third" , MaxCol()+1 )
      @ 14, 32 PROMPT " F~ourth prompt " MESSAGE PadC( "Fourth", MaxCol()+1 )
      @ 16, 32 PROMPT " Exit           " MESSAGE PadC( "Exit"  , MaxCol()+1 )

      nSelect := 1
      MENU TO nSelect

      DO CASE
      CASE nSelect == 5
         QUIT

      OTHERWISE
         Alert( "Prompt #" + Str(nSelect,1) + " is selected;; " + ;
                "Next comes the normal look of the MENU TO command" )
      ENDCASE
   ENDDO

RETURN


/*
 * The function creates one pushbutton for each @ PROMPT command
 * and assumes the following to position the buttons:
 *
 *   - text coordinates are ignored. Instead, pushbuttons are
 *     displayed centered in the window from top to bottom
 *
 *   - a pushbutton uses two text rows as height
 *
 *   - the width of a pushbutton is calculated from font width
 *
 *   - a vertical distance between buttons is calculated from font height
 *     and number of buttons
 */
FUNCTION ButtonMenuTo( aPromptList, nVar, cVarName )
   LOCAL nEvent, mp1 := NIL, mp2 := NIL, oXbp := NIL, aPos, aSize, cPrompt
   LOCAL oFrame, oMsg := NIL
   LOCAL nCount      := Len( aPromptList )
   LOCAL nFontHeight := SetAppWindow():fontHeight
   LOCAL nFontWidth  := SetAppWindow():fontWidth
   LOCAL aButtons[ nCount ]
   LOCAL nDistance, nHeight, nWidth
   LOCAL i, lExit := NIL, bActivate, bKeyHandler

   DEFAULT nVar TO 1
   ReadVar( cVarName )

  /*
   * Height of one pushbutton
   */
   nHeight := 2 * nFontheight

  /*
   * Width of one pushbutton
   */
   nWidth := ( Len( aPromptList[1,3] ) + 4 ) * nFontwidth

  /*
   * Vertical distance between pushbuttons
   */
   nDistance := ( Maxrow() - 2 * nCount ) / nCount
   nDistance := Min( nFontHeight/2, Max( 0, nDistance * nFontHeight ) )

  /*
   * Size of pushbuttons
   */
   aSize := { nWidth, nHeight }

  /*
   * Calculate centered position for first pushbutton
   */
   aPos    := Array( 2 )
   aPos[1] := ( MaxCol() + 1 ) * nFontWidth
   aPos[1] -= nWidth
   aPos[1] /= 2

   aPos[2] := ( MaxRow() + 1 ) * nFontHeight
   aPos[2] -= ( (nCount * nHeight) + (nDistance * (nCount-1)) ) / 2
   aPos[2] -= nHeight

  /*
   * Callback code blocks for pushbuttons
   */
   bKeyHandler := {|mp1,mp2,obj| KeyHandler( mp1, obj, aButtons, aPromptList, oMsg, @lExit ) }
   bActivate   := {|mp1,mp2,obj| lExit := .T., nVar := AScan( aButtons, obj ) }

  /*
   * Create the pushbuttons. :cargo stores the short-cut letter
   */
   FOR i:=1 TO nCount
      cPrompt              := LTrim( aPromptList[i,3] )
      IF .NOT. "~" $ cPrompt
         cPrompt := "~" + cPrompt
      ENDIF
      aButtons[i]          := XbpPushbutton():new( ,, aPos, aSize )
      aButtons[i]:caption  := cPrompt
      aButtons[i]:cargo    := Upper( SubStr( cPrompt, At( "~", cPrompt)+1, 1 ) )
      aButtons[i]:create()

     /*
      * Assign callback code blocks for event handling
      */
      aButtons[i]:activate := bActivate
      aButtons[i]:keyboard := bKeyHandler

     /*
      * y position for next pushbutton
      */
      aPos[2] -= ( nDistance + nHeight )
   NEXT

  /*
   * Display MESSAGE text in status bar at the bottom of the window
   */
   IF Set( _SET_MESSAGE ) > 0
     /*
      * Frame for statusbar
      */
      oFrame      := XbpStatic():new(,, {0,0}, { (MaxCol()+1)*nFontWidth, nFontHeight*1.5} )
      oFrame:type := XBPSTATIC_TYPE_RAISEDBOX
      oFrame:create()

     /*
      * Status bar is child of frame
      */
      aSize    := oFrame:currentSize()
      aSize[1] -= 2
      aSize[2] -= 2

      oMsg         := XbpStatic():new( oFrame,, {1,1}, aSize )
      oMsg:type    := XBPSTATIC_TYPE_TEXT
      oMsg:options := XBPSTATIC_TEXT_VCENTER + XBPSTATIC_TEXT_CENTER
      oMsg:caption := aPromptList[ nVar , 4 ]
      oMsg:create()
      oMsg:setColorFG( GRA_CLR_RED )
   ENDIF

  /*
   * Set focus to initial "prompt" pushbutton
   */
   SetAppFocus( aButtons[nVar] )
   lExit := .F.
   nVar  := 0

   DO WHILE ! lExit
      nEvent := AppEvent( @mp1, @mp2, @oXbp )
      oXbp:handleEvent( nEvent, mp1, mp2 )
   ENDDO

  /*
   * release system resources
   */
   AEval( aButtons, {|o| o:destroy() } )

   IF oFrame <> NIL
      oFrame:destroy()
     /*
      * oMsg is a child of oFrame and is destroyed implicitly
      */
   ENDIF

   ReadVar( "" )
   ASize( aPromptList, 0 )

RETURN nVar



/*
 * Process keys not understood by XbpPushbutton
 */
STATIC PROCEDURE KeyHandler( nKey, obj, aButtons, aPromptList, oMsg, lExit )
   LOCAL nCurrent := AScan( aButtons, obj ), cChar := Upper( Chr(nKey) )

   DO CASE
   CASE nKey == xbeK_UP .OR. nKey == xbeK_LEFT
      nCurrent --

   CASE nKey == xbeK_DOWN .OR. nKey == xbeK_RIGHT
      nCurrent ++

   CASE nKey == xbeK_RETURN .OR. nKey == xbeK_PGDN
      PostAppEvent( xbeP_Activate,,, obj )
      nCurrent := -1

   CASE nKey == xbeK_ESC
     /*
      * lExit is passed by reference from the bKeyHandler codeblock
      * This terminates the event loop
      */
      lExit    := .T.
      nCurrent := -1

   CASE ( cChar >= "A" .AND. cChar <= "Z" ) .OR. cChar $ "0123456789Ž™š"
     /*
      * Selection via short-cut key
      */
      nCurrent := AScan( aButtons, {|o| o:cargo == cChar } )

      IF nCurrent > 0
         PostAppEvent( xbeP_Activate,,, aButtons[ nCurrent ] )
      ELSE
         nCurrent := -1
      ENDIF

   ENDCASE

   IF nCurrent <> -1
     /*
      * Find next next pushbutton
      */
      IF nCurrent < 1
         IF Set( _SET_WRAP )
            nCurrent := Len( aButtons )
         ELSE
            nCurrent := 1
         ENDIF

      ELSEIF nCurrent > Len( aButtons )
         IF Set( _SET_WRAP )
            nCurrent := 1
         ELSE
            nCurrent := Len( aButtons )
         ENDIF
      ENDIF

     /*
      * Set focus to next pushbutton
      */
      SetAppFocus( aButtons[ nCurrent ] )

     /*
      * Display MESSAGE text
      */
      IF oMsg <> NIL
         oMsg:setCaption( aPromptList[ nCurrent, 4 ] )
      ENDIF
   ENDIF

RETURN
