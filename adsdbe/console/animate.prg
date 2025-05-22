//////////////////////////////////////////////////////////////////////
//
//  ANIMATE.PRG
//
//  Copyright:
//   Alaska Software, (c) 1999-2025. All rights reserved.         
//  
//  Contents:
//       Alaska Management Console for the Advantage Database Server
//   
//       This class is used to show an animation
//////////////////////////////////////////////////////////////////////


   CLASS Animation FROM Thread
   PROTECTED:

      // data required for displaying animated bitmaps
      VAR cDllName, aSource  
      VAR aBitmaps, nCurrent, nTotal
      VAR aRect
      VAR autoscale

      // overloaded methods
      METHOD atStart, execute, atEnd 
      METHOD display

   EXPORTED:
      // overloaded method
      METHOD init

      // new method in Animation class
      METHOD stop
   ENDCLASS


   // Initialize Animation class. <aSource> must receive either
   // an array of file names or resource ids. <cDllName> is
   // an optional parameter that may contain the name of a DLL
   // that contains the requested resource.
   METHOD Animation:init( aSource, cDllName )
      ::Thread:init()
      ::aSource   := Aclone( aSource )
      ::cDllName  := cDllName
      ::autoscale := .F.
   RETURN self


   // <oXbp> represents the Xbase-part that is used as a canvas for
   // the animation. <oProceed> is used to signal the availability
   // of the thread. <nInterval> is an optional parameter that
   // determines the thread interval.
   METHOD Animation:atStart( oXbp, nInterval, oProceed )
      LOCAL i, oPS := oXbp:lockps()

      IF nInterval == NIL
         nInterval := 10
      ENDIF

      ::aRect := oXbp:currentSize()
      ::aRect := { 0, 0, ::aRect[1], ::aRect[2] }

      ::nCurrent := 0
      ::nTotal   := Len( ::aSource )
      ::aBitmaps := Array( ::nTotal )

      FOR i:=1 TO ::nTotal
         ::aBitmaps[i] := XbpBitmap():new():create(oPS)
         IF Valtype( ::aSource[i] ) == "N"
            ::aBitmaps[i]:load( ::cDllName, ::aSource[i] )
         ELSE
            ::aBitmaps[i]:loadFile( ::aSource[i] )
         ENDIF
      NEXT
      oXbp:unlockPS(oPS)
      ::setInterval( nInterval )

      // Signal that animation thread is available and running.
      oProceed:signal()
   RETURN self


   // Show animation.
   METHOD Animation:execute( oXbp )
      LOCAL oPS := oXbp:lockPS()

      // Step to next bitmap.
      ::nCurrent ++
      IF ::nCurrent > ::nTotal
         ::nCurrent := 1
      ENDIF

      // Show current bitmap.
      ::display(::aBitmaps[ ::nCurrent], oXbp, oPS)
      oXbp:unlockPS( oPS )
   RETURN self      


   // Only performed at end of animation.
   METHOD Animation:atEnd( oXbp )
      // Clear array of bitmap objects.
      AEval( ::aBitmaps, {|o| o:destroy() } )

      // Force re-paint of Xbase part.
      oXbp:invalidateRect( ::aRect )

      // Support the gabage collector.
      ::aBitmaps := NIL
      ::aRect    := NIL
   RETURN self


   // Stop the animation.
   METHOD Animation:stop
      ::setInterval( NIL )
      ::synchronize( 0 )
   RETURN self


   // Display a bitmap of the animation.
   METHOD Animation:display(oBmp, oXbp, oPS)
      LOCAL aSize    := oXbp:currentsize()
      LOCAL aTarget, aSource, nAspect

      aSource := {0,0,oBmp:xSize,oBmp:ySize}
      aTarget := {1,1,aSize[1]-2,aSize[2]-2}

      IF ::autoScale
         // Rescale the bitmap to fit in the presentation space.
         nAspect    := aSource[3] / aSource[4]
         IF nAspect > 1
            aTarget[4] := aTarget[3] / nAspect
         ELSE
            aTarget[3] := aTarget[4] * nAspect
         ENDIF
      ELSE
         // Keep original size.
         aTarget[3] := aSource[3]
         aTarget[4] := aSource[4]
      ENDIF

      IF aTarget[3] < aSize[1]-2
         nAspect := ( aSize[1]-2-aTarget[3] ) / 2
         aTarget[1] += nAspect
         aTarget[3] += nAspect
      ENDIF

     IF aTarget[4] < aSize[2]-2
         nAspect := ( aSize[2]-2-aTarget[4] ) / 2
         aTarget[2] += nAspect
         aTarget[4] += nAspect
      ENDIF
   
   RETURN oBmp:draw( oPS, aTarget, aSource)
