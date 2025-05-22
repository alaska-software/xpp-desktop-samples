//////////////////////////////////////////////////////////////////////
//
//  OBOX.PRG
//
//  Copyright:
//      Alaska Software, (c) 1997-2025. All rights reserved.         
//  
//  Contents:
//      The example shows basic object oriented programming techniques.
//      Three classes are implemented in this file.
//   
//////////////////////////////////////////////////////////////////////



#include "setcurs.ch"
#include "inkey.ch"
#include "box.ch"

#define  IS_NEW      0
#define  IS_HIDDEN   1
#define  IS_VISIBLE  2


/*
 * A box object can be moved on the screen using the cursor keys
 */
PROCEDURE Main

   LOCAL oBox, nKey := 0, nRow := 0, nCol := 0

  /*
   * Create box object
   */
   oBox := Box():new( 10, 10, 20, 40, ;
                     B_FAT + " ", "W+/BG", "Title" )
   oBox:show()
   oBox:say( 0, 0, "Press a key,..." )
   oBox:say( 1, 0, "Arrow keys move the box" )
   oBox:say( 2, 0, "Esc terminates the program" )
   nRow := 3

   DO WHILE nKey <> K_ESC
      nKey := Inkey(0)

      DO CASE
      CASE nKey == K_UP       ;   oBox:up()
      CASE nKey == K_LEFT     ;   oBox:left()
      CASE nKey == K_DOWN     ;   oBox:down()
      CASE nKey == K_RIGHT    ;   oBox:right()
      ENDCASE
      oBox:say( nRow, nCol, ;
                "The Inkey() code was:" + LTrim(Str(nKey)) )
      nRow ++
      nCol ++
      IF oBox:nCursorCol >= oBox:nRight-1
         nCol := 0
      ENDIF
   ENDDO

   oBox:hide()

RETURN



/*
 * Label class for displaying a string
 */
CLASS Label
  EXPORTED:
     VAR    nRow, nCol, cLabel, cColor 
     METHOD init, Show
ENDCLASS



/*
 * Initialize object with default values if no arguments are passed
 * to Label():new()
 */
METHOD Label:init( nRow, nCol, cLabel, cColor )
   ::nRow   := IIF( nRow == NIL, Row(), nRow )
   ::nCol   := IIF( nCol == NIL, Col(), nCol )
   ::cLabel := IIF( cLabel==NIL, "" , cLabel )
   ::cColor := IIF( cColor==NIL, SetColor(), cColor )
RETURN self


/*
 * Display label 
 */
METHOD Label:Show
   @ ::nRow, ::nCol SAY ::cLabel COLOR ::cColor
RETURN self



/*
 * Class to manage the screen cursor. The class object has
 * own member variables and methods
 */
CLASS Cursor
   EXPORTED:
      CLASS VAR nMaxRow, nMaxCol READONLY SHARED
      CLASS METHOD initClass, Hide, SetMode

      VAR    nRow, nCol, nShape
      METHOD init, Show, UpdatePos
ENDCLASS



/*
 * Initialize class object
 */
CLASS METHOD Cursor:initClass()
   ::nMaxRow := MaxRow()
   ::nMaxCol := MaxCol()
RETURN self



/*
 * Initialize object with default values if no arguments are passed
 * to Cursor():new()
 */
METHOD Cursor:init( nRow, nCol, nShape )
   ::nRow   := IIF( nRow  ==NIL, Row(), nRow )
   ::nCol   := IIF( nCol  ==NIL, Col(), nCol )
   ::nShape := IIF( nShape==NIL, SetCursor(), nShape )
RETURN self



/*
 * Example for a class method. Self is the class object
 */
CLASS METHOD Cursor:SetMode( nMaxRow, nMaxCol )
   IF SetMode( nMaxRow, nMaxCol )
      ::nMaxRow := MaxRow()
      ::nMaxCol := MaxCol()
   ENDIF
RETURN self



/*
 * Switch cursor off. This method is declared a CLASS METHOD 
 * since no member variables are needed.
 */
CLASS METHOD Cursor:Hide
   SetCursor( SC_NONE )
RETURN self



/*
 * Switch cursor on and reposition it if necessary
 */
METHOD Cursor:Show( nRow, nCol )
   IF ValType( nRow ) == "N"
      ::nRow := nRow
   ENDIF
   IF ValType( nCol ) == "N"
      ::nCol := nCol
   ENDIF
   SetPos( ::nRow, ::nCol )
   SetCursor( ::nShape )
RETURN self



/*
 * Update current cursor position
 */
METHOD Cursor:UpdatePos
   ::nRow  := Row()
   ::nCol  := Col()
RETURN self



/*
 *        Box class inherits from two classes
 *                  Cursor  Label              
 *                     \     /                 
 *                       Box                   
 *
 * The class declaration uses the FROM clause
 */
CLASS Box FROM Label,Cursor 
                                       // Cursor class variables
                                       // are SHARED.
   EXPORTED:                           // Globally visible
   VAR nTop, nLeft, nBottom, nRight, cBox
   VAR cScreen, nStatus READONLY       // READONLY -> Assignment is only
                                       // possible in class Box and it's
                                       // sub-classes
   VAR nCursorRow IS nRow IN Cursor    // Redefine! nRow and nCol 
   VAR nCursorCol IS nCol IN Cursor    // exist in both superclasses
   VAR nLabelRow  IS nRow IN Label     // "Cursor" and "Label"
   VAR nLabelCol  IS nCol IN Label     // 

   METHOD init, Show, Hide, Say        // Initialization and display
   METHOD Up, Down, Left, Right        // Movement

   HIDDEN:                             // Only in Box class visible
   METHOD Savescreen, Restscreen       // Save and restore screen
ENDCLASS



/*
 * Initialize Box object. Arguments are passed to class method :new()
 * and :init() receives them subsequently.
 */
METHOD Box:init( nTop, nLeft , nBottom, nRight, ;
                 cBox, cColor, cTitle )

   ::nTop       := nTop
   ::nLeft      := nLeft
   ::nBottom    := nBottom
   ::nRight     := nRight
   ::cBox       := cBox
   ::nStatus    := IS_NEW

  /*
   * Initialize instance of Cursor class
   */
   ::Cursor:init( nTop+1, nLeft+1, SC_NORMAL )


  /*
   * Initialize instance of Label class
   */
   IF ValType( cTitle ) <> "C"
      cTitle := ""            
   ENDIF                      
   nLeft := nLeft+ Round((nRight-nLeft-Len(cTitle)+1)/2 ,0)

   ::Label:init( nTop, nLeft, cTitle, cColor )

RETURN self



/*
 * Display Box object
 */
METHOD Box:Show
   LOCAL cBackGround

   IF ::nStatus == IS_NEW              // Box was never displayed
      ::SaveScreen()                   // Save background
      DispBox( ::nTop   , ::nLeft , ;
               ::nBottom, ::nRight, ;
               ::cBox   , ;
               ::cColor   )            // cColor is declared in 
                                       // Label class
   ELSEIF ::nStatus == IS_HIDDEN       // Box is hidden
      cBackGround := ;                 // Save background
         SaveScreen( ::nTop, ::nLeft, ::nBottom, ::nRight )
      ::RestScreen()                   // Restore foreground
      ::cScreen := cBackGround         // Store background
   ENDIF

   ::nStatus := IS_VISIBLE             // Current status
   ::Label :Show()                     // Display title and cursor
   ::Cursor:Show()                     // by calling the :show() method
                                       // of the respective class
RETURN self



/* 
 * Hide Box object
 */
METHOD Box:Hide
   LOCAL cForeGround

   IF ::nStatus == IS_VISIBLE
      ::Cursor:Hide()

      cForeGround := ;
         SaveScreen( ::nTop, ::nLeft, ::nBottom, ::nRight )
      ::RestScreen()
      ::cScreen := cForeGround
      ::nStatus := IS_HIDDEN
   ENDIF
RETURN self
     


/*
 * Display a value at relative coordinates in the box
 * (0,0 equals to nTop+1 and nLeft+1). If the row is
 * below the bottom line of the box scroll its contents.
 */
METHOD Box:Say( nRow, nCol, xValue )
   LOCAL cOldColor := SetColor( ::cColor )

   IF ::nStatus == IS_VISIBLE
      nRow += ::nTop  + 1
      nCol += ::nLeft + 1

      IF nRow >= ::nBottom
         ::nCursorRow := nRow := ::nBottom-1
         SCROLL( ::nTop+1, ::nLeft+1, ::nBottom-1, ::nRight-1, 1 )
      ENDIF

      IF nCol >= ::nRight 
         ::nCursorCol := nCol := ::nRight-1
      ENDIF

      @ nRow, nCol SAY PADR( xValue, ::nRight-nCol )

      ::Cursor:UpdatePos()
   ENDIF 

   SetColor( cOldColor )
RETURN self



/*
 * Move box one row up
 */
METHOD Box:Up
   IF ::nStatus == IS_VISIBLE .AND. ::nTop > 0
      DispBegin()
      ::Hide()                         // Instance variables:
      ::nTop--                         // - in Box class
      ::nBottom--                      // - in Box class
      ::nCursorRow--                   // - nRow in Cursor class
      ::nLabelRow--                    // - nRow in Label class
      ::Show()
      DispEnd()
   ENDIF
RETURN self
     


/*
 * Move box one row down
 */
METHOD Box:Down
   IF ::nStatus == IS_VISIBLE .AND.;   // SHARED class variable
      ::nBottom <  ::nMaxRow           // nMaxRow from Cursor class
      DispBegin()
      ::Hide()                         // Instance variables:
      ::nTop++                         // - in Box class
      ::nBottom++                      // - in Box class
      ::nCursorRow++                   // - nRow in Cursor class
      ::nLabelRow++                    // - nRow in Label class
      ::Show()
      DispEnd()
   ENDIF
RETURN self
     


/*
 * Move box one column to the left
 */
METHOD Box:Left
   IF ::nStatus == IS_VISIBLE .AND. ::nLeft > 0
      DispBegin()
      ::Hide()                         // Instance variables:
      ::nLeft--                        // - in Box class
      ::nRight--                       // - in Box class
      ::nCursorCol--                   // - nCol in Cursor class
      ::nLabelCol--                    // - nCol in Label class
      ::Show()
      DispEnd()
   ENDIF
RETURN self
     


/*
 * Move box one column to the right
 */
METHOD Box:Right
   IF ( ::nStatus == IS_VISIBLE ) .AND.; // SHARED class variable
      ( ::nRight  <  ::nMaxCol )       // nMaxCol from Cursor class
      DispBegin()
      ::Hide()                         // Instance variables:
      ::nLeft++                        // - in Box class
      ::nRight++                       // - in Box class
      ::nCursorCol++                   // - nCol in Cursor class
      ::nLabelCol++                    // - nCol in Label class
      ::Show()
      DispEnd()
   ENDIF
RETURN self
     


/*
 * Save screen
 */
METHOD Box:Savescreen
   ::cScreen := SaveScreen( ::nTop, ::nLeft, ::nBottom, ::nRight )
RETURN self
     


/*
 * Restore screen
 */
METHOD Box:Restscreen
   RestScreen( ::nTop, ::nLeft, ::nBottom, ::nRight, ::cScreen )
RETURN self

/* EOF
 */
