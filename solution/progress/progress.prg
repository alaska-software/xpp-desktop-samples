//////////////////////////////////////////////////////////////////////
//
//  PROGRESS.PRG
//
//  Copyright:
//      Alaska Software, (c) 1997-2025. All rights reserved.         
//  
//  Contents:
//      The program contains the class ProgressBar for visualization
//      of a progressing process. This is accomplished by drawing
//      colored squares from left to right within the micro presentation
//      space of XbpStatic using GraBox() (-> horizontal display).
//   
//      If the height of the progress bar exceeds its width, the squares
//      are drawn from bottom to top (-> vertical display).
//   
//  Remarks:
//      - Automatic refresh
//   
//        Drawing of the squares occurs in a separate thread and is initiated
//        by a signal in the method :increment(). If this method is not
//        invoked after :maxwait/100 seconds, the display is automatically
//        refreshed.
//   
//      - Progress during DbCreateIndex() / OrdCreate()
//   
//        The progress of creating an index file is displayed in the example.
//        If a ProgressBar object is used in this situation, the index file
//        must be closed after being created. This way, the code block where
//        the ProgressBar object is embedded is destroyed.
//   
//        Note: A ProgressBar object refreshes display during indexing only 
//        when an additional square must be drawn.
//   
//////////////////////////////////////////////////////////////////////



#include "Appevent.ch"
#include "Gra.ch"
#include "Xbp.ch"


/*
 * The Main procedure demonstrates the usage of a ProgressBar object
 * in conjunction with creating a database and an index.
 */
#ifdef DEBUG

PROCEDURE Main
   LOCAL nEvent, mp1, mp2, oXbp, i, oProgress, oDlg

   SetColor( "N/W,W+/N" )
   CLS

  /*
   * Create object and define the range for the progress bar
   */
   oProgress := ProgressBar():new( ,, {120,200}, {400,24},, .F. )
   oProgress:create()
   oProgress:minimum := 1
   oProgress:maximum := 2000

   ? "Creating a database file"

   DbCreate( "TEST", { {"NUMBER", "C", 16, 0} } )
   USE Test EXCLUSIVE

   DO WHILE LastRec() < 2000
      DbAppend()
      REPLACE FIELD->NUMBER WITH PadL( Recno()^2, 16, "0" )
      oProgress:increment()
   ENDDO      

   WAIT "Database is created. Press a key to create an index file"


  /*
   * Reset the current value, change color and size
   */
   oProgress:current := 1
   oProgress:color   := GRA_CLR_RED
   oProgress:setSize( {300,20} )
   DbCreateIndex( "Test", "NUMBER", ;
                      {|| oProgress:increment(), NUMBER } )
   SET INDEX TO Test


   WAIT "Indexing using a vertical progress bar. Press a key..."
   oProgress:current := 1
   oProgress:color   := GRA_CLR_GREEN
   oProgress:setPos ( {10,30} )
   oProgress:setSize( {15,220} )

   DbCreateIndex( "Test", "NUMBER", ;
                      {|| oProgress:increment(), NUMBER } )
   SET INDEX TO Test


   WAIT "Indexing using a tiny progress bar. Press a key..."
   oProgress:current := 1
   oProgress:color   := GRA_CLR_PINK
   oProgress:setSize( {120,12} )

   DbCreateIndex( "Test", "NUMBER", ;
                      {|| oProgress:increment(), NUMBER } )

   SET INDEX TO Test
   WAIT "Press a key to QUIT"
   oProgress:destroy()
   
RETURN

#endif // DEBUG



/*
 * Class for visualizing a progressing process
 */
CLASS ProgressBar FROM XbpStatic, Signal, Thread
   PROTECTED:
   VAR    squares, every, _current
   METHOD displayHoriz, displayVert

   EXPORTED:
   VAR           maxWait, color 
   VAR           minimum, current, maximum
   ASSIGN METHOD minimum, current, maximum 

   METHOD init   , create , destroy , setSize
   METHOD display, execute, increment
ENDCLASS



/*
 * Initialize the object and set Thread:interval() to zero. This way,
 * method :execute() is automatically repeated.
 */
METHOD ProgressBar:init( oParent, oOwner, aPos, aSize, aPP, lVisible )
   ::Thread:init()
   ::Thread:setInterval( 0 )
   ::Thread:atStart := {|| ::xbpStatic:show() }

   ::Signal:init()

   ::xbpStatic:init( oParent, oOwner, aPos, aSize, aPP, lVisible )
   ::xbpStatic:type  := XBPSTATIC_TYPE_RAISEDBOX
   ::xbpStatic:paint := {|| ::display() }

   ::color   := GRA_CLR_BLUE
   ::squares := 1
   ::current := 0
   ::every   := 1
   ::maxWait := 100
   ::minimum := 0
   ::maximum := 100
RETURN



/*
 * Request system resources; calculate the number or squares which
 * fit into the progress bar and start the thread.
 */
METHOD ProgressBar:create( oParent, oOwner, aPos, aSize, aPP, lVisible )
   
   ::xbpStatic:create( oParent, oOwner, aPos, aSize, aPP, lVisible )

   aSize     := ::currentSize()
   ::squares := Int( aSize[1] / (aSize[2]+1) )
   ::start()

RETURN



/*
 * Stop the thread of ProgressBar and release system resources
 */
METHOD ProgressBar:destroy

  /*
   * Turn off automatic repetition of :execute().
   */
   ::thread:setInterVal( NIL )


   IF ::thread:active
     /*
      * Thread is still active.
      * Signal thread to leave its :wait() state
      */
      ::signal()
   ENDIF


   IF ThreadObject() <> ::thread
     /*
      * The current thread is not the thread of ProgressBar (self).
      * Therefore, the current thread must wait for the end of self:thread
      */
      ::thread:synchronize(0)
   ENDIF


  /*
   * System resources are released when self:thread has terminated
   */
   ::xbpStatic:destroy()

RETURN self



/*
 * Change the size of ProgressBar. Before the size is changed,
 * everything is overpainted with the background color.
 */
METHOD ProgressBar:setSize( aSize )
   LOCAL oPS, aAttr[ GRA_AA_COUNT ], _aSize

   oPS       := ::lockPS()
   _aSize    := ::currentSize()
   _aSize[1] -= 2
   _aSize[2] -= 2
   aAttr [ GRA_AA_COLOR ] := GRA_CLR_BACKGROUND
   GraSetAttrArea( oPS, aAttr )
   GraBox( oPS, {1,1}, _aSize, GRA_FILL )
   ::unlockPS( oPS )
   ::xbpStatic:setSize( aSize )

RETURN self



/*
 * ASSIGN method for :minimum
 */
METHOD ProgressBar:minimum( nMinimum )

   IF ::maximum <> NIL .AND. nMinimum > ::maximum
      ::minimum := ::maximum
      ::maximum := nMinimum
   ELSE
      ::minimum := nMinimum  
   ENDIF

   ::current := ::minimum 
RETURN self



/*
 * ASSIGN method for :current
 */
METHOD ProgressBar:current( nCurrent )

   IF Valtype( nCurrent ) == "N"
      ::current := nCurrent

      IF Valtype( ::maximum ) + Valtype( ::minimum ) == "NN"
         ::every    := Int( ( ::maximum - ::minimum ) / ::squares )
         ::_current := ::current
      ENDIF
   ENDIF

RETURN ::current



/*
 * ASSIGN method for :maximum
 */
METHOD ProgressBar:maximum( nMaximum )

   IF ::minimum <> NIL .AND. nMaximum < ::minimum
      ::maximum := ::minimum
      ::minimum := nMaximum
   ELSE
      ::maximum := nMaximum  
   ENDIF

   ::current := ::minimum 

RETURN self



/*
 * Increment the current value and refresh display if necessary
 */
METHOD ProgressBar:increment( nIncrement )

   IF Valtype( nIncrement ) <> "N"
      nIncrement := 1
   ENDIF

  /*
   * While a progress is displayed, PROTECTED VAR :_current is incremented
   * to avoid the overhead of the ASSIGN method :current()
   */
   ::_current += nIncrement
   

   IF Int( ::_current % ::every ) == 0
     /*
      * This interrupts the ::wait( ::maxWait ) method in :execute().
      * The progress bar is then refreshed immediately in its own thread.
      * Since the display occurs in a separate thread, it does not
      * slow down the actual process whose progress is visualized.
      * Index creation, for example, does not update the display,
      * but only signals self:thread
      */
      ::signal()
   ENDIF
RETURN



/*
 * Refresh progress bar automatically every ::maxWait / 100 seconds
 * This method runs in self:thread and is automatically restarted
 * due to :setInterval(0)
 */
METHOD ProgressBar:execute

   ::wait( ::maxWait )
   ::display()

RETURN self



/*
 * Visualize the current state of a process
 */
METHOD ProgressBar:display
   LOCAL oPS   := ::lockPS()
   LOCAL aSize := ::currentSize()
   LOCAL aAttr [ GRA_AA_COUNT ]

   aSize[1] -= 2    
   aSize[2] -= 2

   IF aSize[1] > aSize[2]
      ::displayHoriz( oPS, aSize, aAttr )
   ELSE
      ::displayVert ( oPS, aSize, aAttr )
   ENDIF

   ::unlockPS( oPS )
RETURN self



/*
 * Display squares from left to right (horizontal display)
 */
METHOD ProgressBar:displayHoriz( oPS, aSize, aAttr )
   LOCAL nX, aPos1, aPos2, nCenter

  /*
   * Max. x coordinate for squares
   */
   nX := aSize[1] * ::_current / ( ::maximum - ::minimum )
   nX := Min( nX, aSize[1] )

  /*
   * Fill the area to the right of the squares with background color
   */
   aAttr [ GRA_AA_COLOR ] := GRA_CLR_BACKGROUND
   GraSetAttrArea( oPS, aAttr )
   GraBox( oPS, {1+nX,1}, {aSize[1],aSize[2]}, GRA_FILL )

  /*
   * Define fill color for squares
   */
   aAttr [ GRA_AA_COLOR ] := ::color
   GraSetAttrArea( oPS, aAttr )

  /*
   * Calculate position for leftmost square (starting position)
   */
   aPos1     := { 2, 2 }   
   ::squares := Int( aSize[1] / (aSize[2]+1) )
   nCenter   := 2 + ( aSize[1] - (::squares * (aSize[2]+1)) ) / 2
   aPos1[1]  := Max( 2, nCenter )
   aPos2     := { aPos1[1]+aSize[2]-2 , aSize[2]-1 }   

  /*
   * Draw the squares
   */
   DO WHILE aPos2[1] < nX
      GraBox( oPS, aPos1, aPos2, GRA_FILL )
      aPos1[1] += aSize[2]+1
      aPos2[1] += aSize[2]+1
   ENDDO

   IF aPos2[1] < aSize[1]
      GraBox( oPS, aPos1, aPos2, GRA_FILL )
   ENDIF

RETURN self



/*
 * Display squares from bottom to top (vertical display)
 */
METHOD ProgressBar:displayVert( oPS, aSize, aAttr )
   LOCAL nY, aPos1, aPos2, nCenter

  /*
   * Max. y coordinate for squares
   */
   nY := aSize[2] * ::_current / ( ::maximum - ::minimum )
   nY := Min( nY, aSize[2] )

  /*
   * Fill the area above the squares with background color
   */
   aAttr [ GRA_AA_COLOR ] := GRA_CLR_BACKGROUND
   GraSetAttrArea( oPS, aAttr )
   GraBox( oPS, {1,nY}, {aSize[1],aSize[2]}, GRA_FILL )

  /*
   * Define fill color for squares
   */
   aAttr [ GRA_AA_COLOR ] := ::color
   GraSetAttrArea( oPS, aAttr )

  /*
   * Calculate position for lowest square (starting position)
   */
   aPos1     := { 2, 2 }   
   ::squares := Int( aSize[2] / (aSize[1]+1) )
   nCenter   := 2 + (aSize[2] - (::squares * (aSize[1]+1)) ) / 2
   aPos1[2]  := Max( 2, nCenter )
   aPos2     := { aSize[1]-1, aPos1[2]+aSize[1]-2 }   

  /*
   * Draw the squares
   */
   DO WHILE aPos2[2] < nY
      GraBox( oPS, aPos1, aPos2, GRA_FILL )
      aPos1[2] += aSize[1]+1
      aPos2[2] += aSize[1]+1
   ENDDO

   IF aPos2[2] < aSize[2]
      GraBox( oPS, aPos1, aPos2, GRA_FILL )
   ENDIF

RETURN self


