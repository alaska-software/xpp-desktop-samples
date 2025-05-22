//////////////////////////////////////////////////////////////////////
//
//  BUSYBOX.PRG
//
//  Copyright:
//      Alaska Software, (c) 1997-2025. All rights reserved.         
//  
//  Contents:
//      This program contains the class XbpBusyBox(). It displays a
//      spinning indicator and a message in an XbpDialog window
//   
//  Remarks:
//       The XbpBusyBox() dialog disables its owner in the :show() method
//       and enables its owner in the :hide() method. This means: the
//       dialog window, which is the owner of XbpBusyBox(), can not be
//       selected with the mouse as long as XbpBusyBox is visible!
//   
//     Example :
//       oDialog  := XbpDialog():new():create( .... ):show()
//       oBusyBox := XbpBusyBox():new( oDialog:setParent(), oDialog ):create()
//       oBusyBox:show( "Message for the user ..." )
//   
//       <do some time consuming tasks>
//   
//       oBusybox:hide():destroy()
//   
//     Bitmaps required:
//       BUSYBOX1.BMP, BUSYBOX2.BMP, BUSYBOX3.BMP
//   
//////////////////////////////////////////////////////////////////////


#include "Gra.ch"
#include "Xbp.ch"
#include "Common.ch"
#include "Appevent.ch"
#include "Busybox.ch"


CLASS XbpBusyBox FROM XbpDialog, Thread
   PROTECTED:
   VAR aBitmaps
   VAR Static1
   VAR Static2
   VAR busy
   METHOD execute

   EXPORTED:
   METHOD init
   METHOD create
   METHOD destroy
   METHOD show
   METHOD hide
ENDCLASS

******************************************************************************
* Initialize dialog
******************************************************************************
METHOD XbpBusyBox:init( oParent, oOwner )

   DEFAULT oParent  TO SetAppWindow():setParent(), ;
           oOwner   TO SetAppWindow()

   ::XbpDialog:init( oParent, oOwner, {0,0}, {340,140}, , .F. )
   ::Thread:init()

   ::busy    := .F.
   ::Static1 := XbpStatic():new( ::drawingArea, , {23,45}, {48,47} )
   ::Static2 := XbpStatic():new( ::drawingArea, , {91,28}, {206,78} )
   ::aBitMaps:= { XbpBitmap():new(), ;
                  XbpBitmap():new(), ;
                  XbpBitmap():new()  }

RETURN self


******************************************************************************
* Request system resources
******************************************************************************
METHOD XbpBusyBox:create( oParent, oOwner )
   LOCAL oPS

   ::XbpDialog:border := XBPDLG_DLGBORDER
   ::XbpDialog:origin := XBPDLG_ORIGIN_SCREEN
   ::XbpDialog:titleBar := .F.
   ::XbpDialog:Sysmenu  := .F.
   ::XbpDialog:minButton:= .F.
   ::XbpDialog:maxButton:= .F.
   ::XbpDialog:create( oParent, oOwner )
   ::XbpDialog:drawingArea:setColorBG( GRA_CLR_PALEGRAY )
   ::XbpDialog:drawingArea:setFontCompoundName( "8.Helv.normal" )

   ::Static1:type := XBPSTATIC_TYPE_RAISEDRECT
   ::Static1:create()

   ::Static2:caption := "I am busy working"
   ::Static2:options := XBPSTATIC_TEXT_WORDBREAK+XBPSTATIC_TEXT_VCENTER+XBPSTATIC_TEXT_CENTER
   ::Static2:create()

   oPS := XbpPresSpace():new():create()

   ::aBitMaps[1]:create( oPS )         // Use three XbpBitmap objects
   ::aBitMaps[2]:create( oPS )         // to display the spinning indicator
   ::aBitMaps[3]:create( oPS )
   ::aBitMaps[1]:loadfile( "busybox1.bmp" )
   ::aBitMaps[2]:loadfile( "busybox2.bmp" )
   ::aBitMaps[3]:loadfile( "busybox3.bmp" )

RETURN self


******************************************************************************
* Release system resources
******************************************************************************
METHOD XbpBusyBox:destroy()

   IF ::busy
      ::hide()
   ENDIF

   ::aBitMaps[1]:destroy()
   ::aBitMaps[2]:destroy()
   ::aBitMaps[3]:destroy()
   ::xbpDialog:destroy()
RETURN self


******************************************************************************
* Disable Owner, display dialog and start thread
******************************************************************************
METHOD XbpBusyBox:show( cMessage )
   LOCAL aPos, aSize, bError

   IF Valtype( cMessage ) == "C"       // Display message even if
      ::Static2:caption := cMessage    // dialog has "busy" state
      ::Static2:configure()
   ENDIF

   IF ! ::busy
      IF ::setOwner() <> ::setParent()
         bError := ErrorBlock( {|e| break(e) } )
         BEGIN SEQUENCE                // XbpCrt doesn't know :disable()
            ::setOwner():disable()
         ENDSEQUENCE
         ErrorBlock( bError )
      ENDIF

      aPos     := ::setOwner():currentPos()
      aSize    := ::setOwner():currentSize()
      aSize[1] -= 340
      aSize[2] -= 140
      aSize[1] /= 2
      aSize[2] /= 2
      aPos[1]  += aSize[1]
      aPos[2]  += aSize[2]

      ::xbpDialog:setPos( aPos )
      ::xbpDialog:show()
      SetAppFocus( self )
      ::busy := .T.
      ::start()
   ENDIF

RETURN self


******************************************************************************
* Round robin display of bitmaps  (separate thread)
******************************************************************************
METHOD XbpBusyBox:execute()
   LOCAL i := 1, oPS := ::Static1:lockPS()

   DO WHILE ::busy
      ::aBitmaps[i]:draw( oPS, {2,2} )
      Sleep(10)
      i++
      IF i > Len(::aBitmaps)
         i := 1
      ENDIF
   ENDDO

   ::Static1:unlockPS( oPS )

RETURN self


******************************************************************************
* Close dialog and stop thread
******************************************************************************
METHOD XbpBusyBox:hide()
   LOCAL bError

   IF ::busy
      ::busy := .F.
      ::Thread:synchronize(0)

      IF ::setOwner() <> ::setParent()
         bError := ErrorBlock( {|e| break(e) } )
         BEGIN SEQUENCE                // XbpCrt doesn't know :enable()
            ::setOwner():enable()
         ENDSEQUENCE
         ErrorBlock( bError )
      ENDIF

      ::XbpDialog:hide()
   ENDIF

RETURN self
