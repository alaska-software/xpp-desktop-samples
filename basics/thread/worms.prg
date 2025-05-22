//////////////////////////////////////////////////////////////////////
//
//  WORMS.PRG
//
//  Copyright:
//      Alaska Software, (c) 1997-2025. All rights reserved.         
//  
//  Contents:
//      This samples application demonstrates the effect thread
//      priority has on thread execution. It uses separate threads
//      to perform database operations and to render some graphics.
//      Because of different priorities, rendering speed varies
//      visibly between the threads.
//   
//  Remarks:
//      To increase the difference in execution speed try to
//      increase the workload of your system. This can be achieved
//      by moving the sample's window or by temporarily opening
//      other applications.
//   
//////////////////////////////////////////////////////////////////////

#include "appevent.ch"
#include "xbp.ch"
#include "gra.ch"
#include "thread.ch"
#include "font.ch"

#include "WORMS.CH"


*******************************************************************************
/*
 * Main-Procedure
 */
*******************************************************************************
PROCEDURE main()

   LOCAL oDlg, oPS
   LOCAL aPos
   LOCAL nEvent := NIL, mp1 := NIL, mp2 := NIL, oObj := NIL
   LOCAL i            := 1
   LOCAL aThreads     := {}
   LOCAL aSizeDesktop := AppDesktop():currentSize()
   LOCAL nSeconds
   LOCAL aRect, nHeight, aSlice
   LOCAL aPrios       := { { PRIORITY_LOWEST,  GRA_CLR_RED, .T.  },;
                           { PRIORITY_HIGHEST, GRA_CLR_PINK, .T. },;
                           { PRIORITY_NORMAL,  GRA_CLR_BLUE, .T. } }
   LOCAL aTitles      := { "Lowest Priority", "Highest Priority", ;
                           "Normal Priority" }

   /*
    * Create dialog for output
    */
    aPos                    := { (aSizeDesktop[1] - 640) /2, ;
                                 (aSizeDesktop[2] - 480) /2 }
    oDlg                    := XbpDialog():new( AppDesktop(),, aPos, ;
                                                {640, 480},, .T. )
    oDlg:taskList           := .T.
    oDlg:title              := "Worms: A thread priority test programm!"
    oDlg:border             := XBPDLG_RAISEDBORDERTHICK_FIXED
    oDlg:maxButton          := .F.
    oDlg:minButton          := .T.
    oDlg:create()
    oDlg:drawingArea:setColorBG( GRA_CLR_PALEGRAY )
    SetAppWindow( oDlg )
    SetAppFocus( oDlg )

   /*
    * Create the shared Presentation Space used for
    * rendering, see "SharedPS" class definition
    */
    oPS := SharedPS():new():create( oDlg:drawingArea:winDevice() )

   /*
    * Compute the output area for the individual threads
    */
    aRect   := oDlg:drawingArea:currentSize()
    nHeight := Abs(aRect[2] / Len(aPrios) )

    aSlice  := {0, aRect[2]-nHeight, aRect[1]-1, aRect[2]-1 }

   /*
    * Create the threads
    */
    nSeconds := Seconds()
    FOR i:= 1 TO Len( aPrios )
       AAdd( aThreads, WormsThread():new( oPS,              ;
                                          aSlice,           ;
                                          ID_BMP_PHASE1,    ;
                                          MAX_PHASES        ))
       aThreads[i]:setStartTime( nSeconds + 2 )
       aThreads[i]:setPriority( aPrios[i][1] )
       aThreads[i]:start( , aTitles[i], aPrios[i][2], aPrios[i][3])

       aSlice[2] -= nHeight
       aSlice[4] -= nHeight
    NEXT

    nEvent := xbe_None
    DO WHILE nEvent != xbeP_Close
       /*
        * Check wether there are events pending for
        * the dialog
        */
        nEvent := AppEvent ( @mp1, @mp2, @oObj, 1 )

        IF nEvent <> xbeP_None .AND. oObj <> NIL
            oObj:handleEvent( nEvent, mp1, mp2 )
        ENDIF

        IF nEvent != xbeP_Close
          nEvent := xbeP_None
        ENDIF
    ENDDO

   /*
    * Stop the Threads and wait to complete termination
    */
    AEval( aThreads, {|x,i| x:terminate()} )
    ThreadWaitAll( aThreads )

   /*
    * find out who won the race
    */
    i := -1000
    AEval( aThreads, {|x,j| i := x:checkPos(i, @oObj) } )
    oObj:markWinner()

RETURN



*******************************************************************************
/*
 * Declaration of a class derived from "XbpPresSpace". This class
 * manages the drawing surface the threads are rendering into.
 * Because its methods were declared as "SYNC METHODS", it implicitly
 * does access arbitration for its properties (such as the current
 * color setting).
 */
*******************************************************************************
CLASS SharedPS FROM XbpPresSpace

EXPORTED:

   SYNC METHOD DrawLine
   SYNC METHOD DrawBox
   SYNC METHOD DrawString
   SYNC METHOD DrawCircle
   SYNC METHOD GetStringBox

ENDCLASS



*******************************************************************************
/*
 * Draw a line on the shared PS
 */
*******************************************************************************
METHOD SharedPS:DrawLine( aStartPos, aEndPos, aAttrs )

   LOCAL aOldAttrs

   IF aAttrs != NIL
      aOldAttrs := GraSetAttrLine( self, aAttrs )
   ENDIF

   GraLine( self, aStartPos, aEndPos )

   IF aAttrs != NIL
      GraSetAttrLine( self, aOldAttrs )
   ENDIF

RETURN self



*******************************************************************************
/*
 * Draw a box on the shared PS
 */
*******************************************************************************
METHOD SharedPS:DrawBox( aStartPos, aEndPos, aAttrs )

   LOCAL aOldAttrs

   IF aAttrs != NIL
      aOldAttrs := GraSetAttrArea( self, aAttrs )
   ENDIF

   GraBox( self, aStartPos, aEndPos )

   IF aAttrs != NIL
      GraSetAttrArea( self, aOldAttrs )
   ENDIF

RETURN self



*******************************************************************************
/*
 * Draw a string on the shared PS
 */
*******************************************************************************
METHOD SharedPS:DrawString( aPos, cString, aAttrs )

   LOCAL aOldAttrs

   IF aAttrs != NIL
      aOldAttrs := GraSetAttrString( self, aAttrs )
   ENDIF

   GraStringAt( self, aPos, cString )

   IF aAttrs != NIL
      GraSetAttrString( self, aOldAttrs )
   ENDIF

RETURN self


*******************************************************************************
/*
 * Draw a circle on the shared PS
 */
*******************************************************************************
METHOD SharedPS:DrawCircle( aPos, nRadius, aAttrs )

   LOCAL aOldAttrs

   IF aAttrs != NIL
      aOldAttrs := GraSetAttrLine( self, aAttrs )
   ENDIF

   GraArc( self, aPos, nRadius )

   IF aAttrs != NIL
      GraSetAttrLine( self, aOldAttrs )
   ENDIF

RETURN self



*******************************************************************************
/*
 * Get output dimensions of a string rendered on the PS
 */



*******************************************************************************
METHOD SharedPS:GetStringBox( cString )

   LOCAL aTextBox

   aTextBox := GraQueryTextBox( self, cString )

RETURN aTextBox



*******************************************************************************
/*
 * Declaration of a class derived from "Thread". All thread objects
 * managed by this sample are instances of this class.
 */
*******************************************************************************
CLASS WormsThread FROM Thread
   VAR lTerminateRequest

EXPORTED:
   METHOD init
   METHOD atStart
   METHOD execute
   METHOD markWinner
   METHOD checkPos
   INLINE METHOD terminate
         ::lTerminateRequest := .T.
   RETURN self

PROTECTED:
   VAR oPS

   /* Start position for rendering      */
   VAR aStartPos
   /* Current position                  */
   VAR aPos
   /* String to render                  */
   VAR cString
   /* Color to render the string in     */
   VAR nColor
   /* Display area on the parent        */
   VAR aOutputArea
   /* Array with the XbpBitmap objects  */
   /* that display the individual anim  */
   /* phases                            */
   VAR aAnimPhases
   /* Current animation phase           */
   VAR nCurrAnimPhase
   /* Current lap no.                   */
   VAR nCurrLap
   /* Enable lap no. output?            */
   VAR bOutputLap

ENDCLASS



*******************************************************************************
/*
 * Initialize WormsThread object
 */
*******************************************************************************
METHOD WormsThread:init( oPS, aArea, nBaseResID, nMaxPhases )

    LOCAL i

    ::Thread:init()
    ::oPS               := oPS

    ::aOutputArea       := AClone( aArea )

    ::nCurrAnimPhase    := 1
    ::aAnimPhases       := Array( nMaxPhases )

   /*
    * Create the XbpBitmap objects for rendering
    * the individual animation phases
    */
    FOR i := 1 TO nMaxPhases
      ::aAnimPhases[i] := XbpBitmap():new():create( ::oPS )
      ::aAnimPhases[i]:load( , nBaseResID + (i-1) )
    NEXT

    ::aStartPos         := { aArea[1] - ::aAnimPhases[::nCurrAnimPhase]:xSize, ;
                             aArea[2] + (aArea[4]-aArea[2])/2 }
    ::bOutputLap        := .F.
    ::nCurrLap          := 0
    ::lTerminateRequest := .F.

RETURN self



*******************************************************************************
/*
 * Initialize attributes and position of a WormsThread object
 */
*******************************************************************************
METHOD WormsThread:atStart( cString, nColor, bLaps )

    LOCAL oFont

    ::aPos               := aClone( ::aStartPos )
    ::cString            := cString
    ::nColor             := nColor
    ::bOutputLap         := bLaps

    oFont                := XbpFont():new( ::oPS ):create( "12.Helv" )
    GraSetFont( ::oPS, oFont )

RETURN



*******************************************************************************
/*
 * This method gets called when the thread is running. It contains the code
 * to be executed independently from the application's main thread. For
 * demonstration purposes, it performs some database operations and renders
 * the individual phases of the animation used to visualize thread execution.
 * Furthermore, it renders imagery to frame the current thread's output area
 * and outputs the thread priority.
 *
 */
*******************************************************************************
METHOD WormsThread:execute()

    LOCAL aStringAttrs := Array( GRA_AS_COUNT )
    LOCAL aLineAttrs   := Array( GRA_AL_COUNT )
    LOCAL aTextBox
    LOCAL aStringPos   := Array( 2 )
    LOCAL oCurrPhase
    LOCAL cTmpString, cTmpDBName
    LOCAL i

    FIELD TmpID

    DO WHILE !::lTerminateRequest

       /*
        * Creation and management of a temporary
        * database in order to increase the system
        * load. This increases the effect of the
        * different thread priorities for better
        * demonstration
        */
        cTmpDBName := "TmpDB" + AllTrim( Str(ThreadID()) ) + ".DBF"
        DbCreate( cTmpDBName, { { "TmpID", "N", 9, 2 } } )

        USE (cTmpDBName) EXCLUSIVE

        FOR i:=1 TO 90
           DBAppend()
           TmpID := i
        NEXT

        SLEEP( 1 )

        DbGoTop()
        FOR i:=1 TO 90
           SKIP
           DBDelete()
        NEXT
        DbPack()

        USE
        ERASE (cTmpDBName)

        ::oPS:DrawLine( {::aPos[1], ::aPos[2]-1 }, {::aPos[1], ::aPos[2]-1 }, NIL )

       /*
        * Render the current animation phase
        */
        oCurrPhase := ::aAnimPhases[::nCurrAnimPhase]
        oCurrPhase:draw( ::oPS,                             ;
                         {::aPos[1], ::aPos[2],             ;
                         ::aPos[1] + oCurrPhase:xSize,      ;
                         ::aPos[2] + oCurrPhase:ySize} )

       /*
        * Compute the position for outputting the thread
        * priority
        */
        cTmpString      := ::cString
        IF ::bOutputLap == .T.
           cTmpString := ::cString + " (Lap: " + ;
                         AllTrim( Str(::nCurrLap) ) + ")"
        ENDIF

        aTextBox        := ::oPS:GetStringBox( cTmpString )
        aStringPos[1]   := ::aOutputArea[3] - ::aOutputArea[1]
        aStringPos[1]   -= aTextBox[3][1] - aTextBox[1][1]
        aStringPos[1]   := ::aOutputArea[1] + aStringPos[1] /2

        aStringPos[2]   := ::aOutputArea[4]- (aTextBox[3][2] - aTextBox[2][2])

       /*
        * Set string attributes and output priority
        */
        aStringAttrs[ GRA_AS_COLOR ]     := ::nColor
        aStringAttrs[ GRA_AS_BACKCOLOR ] := GRA_CLR_PALEGRAY
        aStringAttrs[ GRA_AS_BGMIXMODE ] := GRA_BGMIX_OVERPAINT
        ::oPS:DrawString( aStringPos, cTmpString, aStringAttrs )

       /*
        * Draw a box TO frame the output area
        */
        ::oPS:DrawBox( {::aOutputArea[1], ::aOutputArea[2]}, ;
                       {::aOutputArea[3], ::aOutputArea[4]}, NIL )

       /*
        * Set next animation phase
        */
        IF ::nCurrAnimPhase >= MAX_PHASES
            ::aPos[1] += X_VELOCITY
            ::nCurrAnimPhase := 1
        ELSE
            ::nCurrAnimPhase++
        ENDIF

       /*
        * Reset worm position if it has moved
        * past the righmost window border and
        * erase the Worm's trail
        */
        IF ::aPos[1] - ::aAnimPhases[::nCurrAnimPhase]:xSize > ::aOutputArea[3]
            aLineAttrs[GRA_AL_COLOR] := GRA_CLR_PALEGRAY
            ::oPS:DrawLine( {::aOutputArea[1], ::aStartPos[2]-1}, ;
                            {::aOutputArea[3], ::aStartPos[2]-1}, ;
                            aLineAttrs )
            ::aPos     := AClone( ::aStartPos )
            ::nCurrAnimPhase := 1
            ::nCurrLap++
        ENDIF
    ENDDO

RETURN


*******************************************************************************
/*
 * Only one of the worms may win
 */
*******************************************************************************
METHOD WormsThread:markWinner( )

       LOCAL i
       LOCAL aLineAttrs := Array( GRA_AL_COUNT )


       FOR i:=1 TO 5
           aLineAttrs[GRA_AL_COLOR] := GRA_CLR_BLUE + i
           ::oPS:DrawCircle( {::aPos[1]+20, ::aPos[2]}, 10*i, aLineAttrs )
       NEXT
       Sleep(250)
RETURN

*******************************************************************************
/*
 * check if the worms position is a bit further
 */
*******************************************************************************
METHOD WormsThread:checkPos( nOthersPos, oSelf )
LOCAL nRetPos := nOthersPos
LOCAL nPos

        nPos := ::nCurrLap * 2000 + ::aPos[1]
        IF nPos > nRetPos
            nRetPos := nPos
            oSelf := self
        ENDIF

RETURN nRetPos


*******************************************************************************
/*
 * Prevent creation of the default Crt
 */
*******************************************************************************
PROCEDURE AppSys()

RETURN
