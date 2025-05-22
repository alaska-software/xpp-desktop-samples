//////////////////////////////////////////////////////////////////////
//
//  CRTSTYLE.PRG
//
//  Copyright:
//      Alaska Software, (c) 1997-2025. All rights reserved.         
//  
//  Contents:
//      This program lets you test various settings for an XbpCrt window.
//      Typical configurations are displayed via the menu system.
//   
//////////////////////////////////////////////////////////////////////


#include "Appevent.ch"
#include "Font.ch"
#include "Gra.ch"
#include "Xbp.ch"

STATIC slTaskList   := .T.
STATIC slTitleBar   := .T.
STATIC slSysMenu    := .T.
STATIC slMinMax     := .T.
STATIC snBorder     := 2
STATIC saBorders    := { ;
   { "XBPDLG_NO_BORDER          ", XBPDLG_NO_BORDER           }, ;
   { "XBPDLG_DLGBORDER          ", XBPDLG_DLGBORDER           }, ;
   { "XBPDLG_RAISEDBORDERTHICK  ", XBPDLG_RAISEDBORDERTHICK   }, ;
   { "XBPDLG_RECESSEDBORDERTHICK", XBPDLG_RECESSEDBORDERTHICK }  }

STATIC soComboBox



/*
 * AppSys creates the application window. Here, it is user defined
 */
PROCEDURE AppSys()
   CreateAppWindow()
RETURN



/*
 * Procedure for terminating the program
 */
PROCEDURE AppQuit
  QUIT
RETURN



/*
 * The Main procedure merely consists of the event loop
 */
PROCEDURE Main
   LOCAL nEvent := NIL, mp1 := NIL, mp2 := NIL, oXbp := NIL

   DO WHILE .T.
      nEvent := AppEvent( @mp1, @mp2, @oXbp )
      oXbp:handleEvent( nEvent, mp1, mp2 )
   ENDDO
RETURN



/*
 * This dialog window has been designed with the FormDesigner
 */
PROCEDURE CreateAppWindow
   LOCAL oDlg, oXbp, drawingArea, oGroup

   oDlg := XbpDialog():new( AppDesktop(), , {3,30}, {0,0}, , .F.)
   oDlg:clientSize := {530,450}
   oDlg:taskList := .T.
   oDlg:title    := "XbpCrt styles"
   oDlg:close    := {|| AppQuit() }
   oDlg:create()

   CreateMenuSystem( oDlg:menuBar() )

   drawingArea := oDlg:drawingArea
   drawingArea:setFontCompoundName( FONT_HELV_SMALL )

   oGroup := XbpStatic():new( drawingArea, , {12,336}, {276,84} )
   oGroup:caption := "Configuration"
   oGroup:clipSiblings := .T.
   oGroup:clipChildren := .T.
   oGroup:type := XBPSTATIC_TYPE_GROUPBOX
   oGroup:create()

   oXbp := XbpCheckbox():new( oGroup, , {12,36}, {84,24} )
   oXbp:caption := ":taskList"
   oXbp:create()
   oXbp:selected := {|lChecked| slTaskList := lChecked }
   oXbp:setData( slTaskList )

   oXbp := XbpCheckbox():new( oGroup, , {12,12}, {84,24} )
   oXbp:caption := ":titleBar"
   oXbp:create()
   oXbp:selected := {|lChecked| slTitleBar := lChecked }
   oXbp:setData( slTitleBar )

   oXbp := XbpCheckbox():new( oGroup, , {96,36}, {84,24} )
   oXbp:caption := ":sysMenu"
   oXbp:create()
   oXbp:selected := {|lChecked| slSysMenu := lChecked }
   oXbp:setData( slSysMenu )

   oXbp := XbpCheckbox():new( oGroup, , {96,12}, {84,24} )
   oXbp:caption := ":minMax"
   oXbp:create()
   oXbp:selected := {|lChecked| slMinMax := lChecked }
   oXbp:setData( slMinMax )

   oXbp := XbpStatic():new( drawingArea, , {24,300}, {84,24} )
   oXbp:caption := ":border"
   oXbp:clipSiblings := .T.
   oXbp:options := XBPSTATIC_TEXT_VCENTER
   oXbp:create()

   oXbp := XbpCombobox():new( drawingArea, , {84,180}, {324,144} )
   oXbp:clipSiblings := .T.
   oXbp:type := XBPCOMBO_DROPDOWNLIST
   oXbp:create()

   AEval( saBorders, {|a| oXbp:xbpListBox:addItem( a[1] ) } )
   oXbp:xbpListbox:setData( {2}, .T. )
   oXbp:xbpSle:setData( saBorders[2,1] )
   soComboBox := oXbp

   oXbp := XbpPushButton():new( drawingArea, , {432,336}, {84,24} )
   oXbp:caption := ":create()"
   oXbp:clipSiblings := .T.
   oXbp:create()
   oXbp:activate := {|| CreateTestCrt() }

   oXbp := XbpPushButton():new( drawingArea, , {432,300}, {84,24} )
   oXbp:caption := "Cancel"
   oXbp:clipSiblings := .T.
   oXbp:create()
   oXbp:activate := {|| PostAppEvent( xbeP_Close,,, SetAppWindow() ) }

   oDlg:show()

   SetAppWindow( oDlg )
   SetAppFocus( oDlg )
RETURN



/*
 * The menu calls functions which create an XbpCrt window using
 * typical configurations
 */
PROCEDURE CreateMenuSystem( oMenuBar )
   LOCAL oMenu := XbpMenu():new( oMenuBar )

   oMenu:title := "~Typical Configurations"
   oMenu:create()

   oMenu:addItem( { "~Normal"  , {|| NormalWindow()  } } )
   oMenu:addItem( { "~Startup" , {|| StartupWindow() } } )
   oMenu:addItem( { "~About"   , {|| AboutWindow()   } } )
   oMenu:addItem( { "~Tool"    , {|| ToolWindow()    } } )
   oMenu:addItem( { "~Exit"    , {|| PostAppEvent( xbeP_Close,,,SetAppWindow() ) } } )

   oMenubar:addItem( {oMenu, NIL} )
RETURN



/*
 * Normal XbpCrt window (most common configuration)
 */
PROCEDURE NormalWindow()
   LOCAL oCrt, oXbp, aPos, aSize := {640,400}
   LOCAL oParent := AppDesktop()

   aPos          := CenterPos( aSize, oParent:currentSize() )
   oCrt          := XbpCrt():new( oParent, , aPos, 25, 80 )
   oCrt:taskList := .T.
   oCrt:title    := "Normal XbpCrt Window on Desktop"
   oCrt:close    := {|mp1,mp2,obj| obj:destroy() }
   oCrt:create()

   oXbp := XbpPushButton():new( oCrt, , {12,12}, {84,24} )
   oXbp:caption := "Close"
   oXbp:create()
   oXbp:activate := {|| PostAppEvent(xbeP_Close,,, oCrt) }
RETURN



/*
 * XbpCrt window without title bar
 */
PROCEDURE StartupWindow()
   LOCAL oCrt, oXbp, aPos, aSize := {320,320}
   LOCAL oParent := AppDesktop()

   aPos          := CenterPos( aSize, oParent:currentSize() )
   oCrt          := XbpCrt():new( oParent, , aPos, 20, 40 )
   oCrt:taskList := .F.
   oCrt:titleBar := .F.
   oCrt:border   := XBPDLG_DLGBORDER
   oCrt:close    := {|mp1,mp2,obj| obj:destroy() }
   oCrt:create()

   oXbp := XbpPushButton():new( oCrt, , {12,12}, {84,24} )
   oXbp:caption := "Close"
   oXbp:create()
   oXbp:activate := {|| PostAppEvent(xbeP_Close,,, oCrt) }
RETURN



/*
 * Modal XbpCrt Window
 */
PROCEDURE AboutWindow()
   LOCAL oCrt, oXbp, aPos, aSize := {400,320}
   LOCAL oParent := AppDesktop(), oOwner := SetAppwindow()

   aPos          := CenterPos( aSize, oParent:currentSize() )
   oCrt          := XbpCrt():new( oParent, oOwner , aPos, 20, 50 )
   oCrt:taskList := .T.
   oCrt:sysMenu  := .F.
   oCrt:title    := "Modal XbpCrt Window"
   oCrt:border   := XBPDLG_DLGBORDER

  /*
   * Important: when the window is closed, it's state must be reset
   *            to modeless. This is done in the :close code block
   */
   oCrt:close    := {|mp1,mp2,obj| obj:setModalState( XBP_DISP_MODELESS), obj:destroy() }
   oCrt:create()

   oXbp := XbpPushButton():new( oCrt, , {12,12}, {84,24} )
   oXbp:caption  := "Close"
   oXbp:cancel   := .T.
   oXbp:create()
   oXbp:activate := {|| PostAppEvent(xbeP_Close,,, oCrt) }

   oCrt:showModal( oXbp )

RETURN



/*
 * Modeless Tool Window
 */
PROCEDURE ToolWindow()
   LOCAL oCrt, oXbp, aPos, aSize := {400,160}
   LOCAL oParent := AppDesktop()

   aPos          := CenterPos( aSize, oParent:currentSize() )
   oCrt          := XbpCrt():new( oParent, , aPos, 10, 50 )
   oCrt:taskList := .F.
   oCrt:title    := "Modeless Tool Window"
   oCrt:border   := XBPDLG_RECESSEDBORDERTHICK
   oCrt:close    := {|mp1,mp2,obj| obj:destroy() }
   oCrt:create()

   oXbp := XbpPushButton():new( oCrt, , {12,12}, {84,24} )
   oXbp:caption := "Close"
   oXbp:create()
   oXbp:activate := {|| PostAppEvent(xbeP_Close,,, oCrt) }
RETURN



/*
 * This procedure creates an XbpCrt window and configures it with
 * the selected settings.
 */
PROCEDURE CreateTestCrt
   STATIC snCount := 0
   LOCAL oCrt, oXbp, oGroup, aPos, oParent
   LOCAL oAppWin, aPP := {{ XBP_PP_COMPOUNDNAME, FONT_HELV_SMALL }}

   oParent := AppDesktop()
   aPos    := SetAppWindow():currentPos()
   aPos[1] += SetAppWindow():currentSize()[1] + 2

   snBorder := AScan( saBorders, {|a| a[1] == soComboBox:xbpSle:getData() } )

   oCrt := XbpCrt():new( oParent,, aPos, 20, 50, ;
                        "Example [" + Chr(65+snCount++) + "]" )
   oCrt:taskList     := slTaskList
   oCrt:titleBar     := slTitleBar
   oCrt:sysMenu      := slSysMenu
   oCrt:minMax       := slMinMax
   oCrt:border       := saBorders[ snBorder, 2 ]
   oCrt:clipChildren := .F.
   oCrt:close        := {|mp1,mp2,obj| obj:destroy() }
   oCrt:create()

   oAppWin := SetAppWindow( oCrt )
   SetColor( "N/W" )
   CLS

   IF snCount > 25
      snCount := 0
   ENDIF

   oGroup := XbpStatic():new( , , {12,168}, {200,84}, aPP, .F. )
   oGroup:caption := "Configuration"
   oGroup:clipSiblings := .T.
   oGroup:clipChildren := .T.
   oGroup:type := XBPSTATIC_TYPE_GROUPBOX
   oGroup:create():Show()

   oXbp := XbpCheckbox():new( oGroup, , {12,36}, {84,24} )
   oXbp:caption := ":taskList"
   oXbp:create()
   oXbp:setData( slTaskList )

   oXbp := XbpCheckbox():new( oGroup, , {12,12}, {84,24} )
   oXbp:caption := ":titleBar"
   oXbp:create()
   oXbp:setData( slTitleBar )

   oXbp := XbpCheckbox():new( oGroup, , {96,36}, {84,24} )
   oXbp:caption := ":sysMenu"
   oXbp:create()
   oXbp:setData( slSysMenu )

   oXbp := XbpCheckbox():new( oGroup, , {96,12}, {84,24} )
   oXbp:caption := ":minMax"
   oXbp:create()
   oXbp:setData( slMinMax )

   oXbp := XbpStatic():new( , , {24,132}, {84,24}, aPP )
   oXbp:caption := ":border"
   oXbp:clipSiblings := .T.
   oXbp:options := XBPSTATIC_TEXT_VCENTER
   oXbp:create()

   oXbp := XbpStatic():new( , , {84,132}, {324,24}, aPP )
   oXbp:clipSiblings := .T.
   oXbp:options := XBPSTATIC_TEXT_VCENTER
   oXbp:caption := saBorders[ snBorder, 1 ]
   oXbp:create()

   oXbp := XbpPushButton():new( , , {12,12}, {84,24}, aPP )
   oXbp:caption := "Close"
   oXbp:create()
   oXbp:activate := {|| PostAppEvent(xbeP_Close,,, oCrt) }

   oGroup:disable()

   SetAppWindow( oAppWin )

RETURN


/*
 * Calculate the center position from size and reference size
 */
STATIC FUNCTION CenterPos( aSize, aRefSize )
RETURN { Int( (aRefSize[1] - aSize[1]) / 2 ) ;
       , Int( (aRefSize[2] - aSize[2]) / 2 ) }
