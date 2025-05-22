//////////////////////////////////////////////////////////////////////
//
//  DLGSTYLE.PRG
//
//  Copyright:
//      Alaska Software, (c) 1997-2025. All rights reserved.         
//  
//  Contents:
//      This program lets you test various settings for an XbpDialog window.
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
STATIC slMinButton  := .T.
STATIC slMaxButton  := .T.
STATIC slHideButton := .T.
STATIC snBorder     :=  2
STATIC saBorders    := { ;
   { "XBPDLG_NO_BORDER                ", XBPDLG_NO_BORDER                 }, ;
   { "XBPDLG_SIZEBORDER               ", XBPDLG_SIZEBORDER                }, ;
   { "XBPDLG_DLGBORDER                ", XBPDLG_DLGBORDER                 }, ;
   { "XBPDLG_RAISEDBORDERTHICK        ", XBPDLG_RAISEDBORDERTHICK         }, ;
   { "XBPDLG_RECESSEDBORDERTHICK      ", XBPDLG_RECESSEDBORDERTHICK       }, ;
   { "XBPDLG_RAISEDBORDERTHICK_FIXED  ", XBPDLG_RAISEDBORDERTHICK_FIXED   }, ;
   { "XBPDLG_RECESSEDBORDERTHICK_FIXED", XBPDLG_RECESSEDBORDERTHICK_FIXED }  }

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
   oDlg:clientSize := {530,460}
   oDlg:taskList := .T.
   oDlg:gridMove := .T.
   oDlg:title    := "Dialog styles"
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
   oXbp:caption := ":hideButton"
   oXbp:create()
   oXbp:selected := {|lChecked| slHideButton := lChecked }
   oXbp:setData( slHideButton )

   #ifdef __WIN32__
     /*
      * :hideButton is ignored under Windows
      */
      oXbp:disable()
   #endif

   oXbp := XbpCheckbox():new( oGroup, , {180,36}, {84,24} )
   oXbp:caption := ":minButton"
   oXbp:create()
   oXbp:selected := {|lChecked| slMinButton := lChecked }
   oXbp:setData( slMinButton )

   oXbp := XbpCheckbox():new( oGroup, , {180,12}, {84,24} )
   oXbp:caption := ":maxButton"
   oXbp:create()
   oXbp:selected := {|lChecked| slMaxButton := lChecked }
   oXbp:setData( slMaxButton )

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
   oXbp:activate := {|| CreateTestDialog() }

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
 * The menu calls functions which create an XbpDialog window using
 * typical configurations
 */
PROCEDURE CreateMenuSystem( oMenuBar )
   LOCAL oMenu := XbpMenu():new( oMenuBar )

   oMenu:title := "~Typical Dialogs"
   oMenu:create()

   oMenu:addItem( { "~Normal"  , {|| NormalWindow()  } } )
   oMenu:addItem( { "~Startup" , {|| StartupWindow() } } )
   oMenu:addItem( { "~About"   , {|| AboutWindow()   } } )
   oMenu:addItem( { "~Tool"    , {|| ToolWindow()    } } )
   oMenu:addItem( { "~Exit"    , {|| PostAppEvent( xbeP_Close,,,SetAppWindow() ) } } )

   oMenubar:addItem( {oMenu, NIL} )

RETURN



/*
 * Normal XbpDialog window (most common configuration)
 */
PROCEDURE NormalWindow()
   LOCAL oDlg, drawingArea, oXbp
   LOCAL oParent := AppDesktop()

   oDlg            := XbpDialog():new( oParent )
   oDlg:visible    := .F.
   oDlg:clientSize := {600,380}
   oDlg:taskList   := .T.
   oDlg:title      := "Normal Window on desktop"
   oDlg:close      := {|mp1,mp2,obj| obj:destroy() }
   oDlg:create()
   CenterControl( oDlg )

   drawingArea := oDlg:drawingArea
   drawingArea:setFontCompoundName( "8.Helv" )

   oXbp := XbpPushButton():new( drawingArea, , {12,12}, {84,24} )
   oXbp:caption    := "Close"
   oXbp:create()
   oXbp:activate   := {|| PostAppEvent(xbeP_Close,,, oDlg) }

   oDlg:show()

RETURN



/*
 * XbpDialog window without title bar
 */
PROCEDURE StartupWindow()
   LOCAL oDlg, drawingArea, oXbp
   LOCAL oParent := AppDesktop()

   oDlg            := XbpDialog():new( oParent )
   oDlg:visible    := .F.
   oDlg:clientSize := {300,280}
   oDlg:taskList   := .F.
   oDlg:titleBar   := .F.
   oDlg:border     := XBPDLG_DLGBORDER
   oDlg:close      := {|mp1,mp2,obj| obj:destroy() }
   oDlg:create()
   CenterControl( oDlg )

   drawingArea     := oDlg:drawingArea
   drawingArea:setFontCompoundName( "8.Helv" )

   oXbp := XbpPushButton():new( drawingArea, , {12,12}, {84,24} )
   oXbp:caption    := "Close"
   oXbp:create()
   oXbp:activate   := {|| PostAppEvent(xbeP_Close,,, oDlg) }

   oDlg:show()

RETURN



/*
 * Modal XbpDialog Window
 */
PROCEDURE AboutWindow()
   LOCAL oDlg, drawingArea, oXbp
   LOCAL oParent := AppDesktop(), oOwner := SetAppwindow()

   oDlg            := XbpDialog():new( oParent, oOwner )
   oDlg:visible    := .F.
   oDlg:clientSize := {400,280}
   oDlg:taskList   := .T.
   oDlg:sysMenu    := .F.
   oDlg:title      := "Modal XbpDialog Window"
   oDlg:border     := XBPDLG_DLGBORDER
   oDlg:close      := {|mp1,mp2,obj| obj:destroy() }
   oDlg:create()
   CenterControl( oDlg )

   drawingArea := oDlg:drawingArea
   drawingArea:setFontCompoundName( "8.Helv" )

   oXbp := XbpPushButton():new( drawingArea, , {12,12}, {84,24} )
   oXbp:caption    := "Close"
   oXbp:cancel     := .T.
   oXbp:create()
   oXbp:activate   := {|| PostAppEvent(xbeP_Close,,, oDlg) }

   oDlg:showModal()

RETURN



/*
 * Modeless Tool Window
 */
PROCEDURE ToolWindow()
   LOCAL oDlg, drawingArea, oXbp
   LOCAL oParent := AppDesktop()

   oDlg            := XbpDialog():new( oParent )
   oDlg:visible    := .F.
   oDlg:clientSize := {400,120}
   oDlg:taskList   := .F.
   oDlg:title      := "Modeless Tool Window"
   oDlg:border     := XBPDLG_RECESSEDBORDERTHICK
   oDlg:close      := {|mp1,mp2,obj| obj:destroy() }
   oDlg:create()
   CenterControl( oDlg )

   drawingArea     := oDlg:drawingArea
   drawingArea:setFontCompoundName( "8.Helv" )

   oXbp            := XbpPushButton():new( drawingArea, , {12,12}, {84,24} )
   oXbp:caption    := "Close"
   oXbp:create()
   oXbp:activate   := {|| PostAppEvent(xbeP_Close,,, oDlg) }

   oDlg:show()

RETURN



/*
 * This procedure creates an XbpDialog window and configures it with
 * the selected settings.
 */
PROCEDURE CreateTestDialog
   STATIC snCount := 0
   LOCAL oDlg, oXbp, drawingArea, oGroup, aPos, oParent

   oParent  := AppDesktop()
   aPos     := SetAppWindow():currentPos()
   aPos[1]  += SetAppWindow():currentSize()[1] + 2

   snBorder := AScan( saBorders, {|a| a[1] == soComboBox:xbpSle:getData() } )

   oDlg            := XbpDialog():new( oParent,, aPos,,, .F.)
   oDlg:clientSize := {427,267}
   oDlg:taskList   := slTaskList
   oDlg:titleBar   := slTitleBar
   oDlg:sysMenu    := slSysMenu
   oDlg:minButton  := slMinButton
   oDlg:maxButton  := slMaxButton
   oDlg:hideButton := slHideButton
   oDlg:border     := saBorders[ snBorder, 2 ]
   oDlg:title      := "Example [" + Chr(65+snCount++) + "]"
   oDlg:close      := {|mp1,mp2,obj| obj:destroy() }
   oDlg:create()

   IF snCount > 25
      snCount := 0
   ENDIF

   drawingArea := oDlg:drawingArea
   drawingArea:setFontCompoundName( FONT_HELV_SMALL )

   oGroup := XbpStatic():new( drawingArea, , {12,168}, {276,84} )
   oGroup:caption := "Configuration"
   oGroup:clipSiblings := .T.
   oGroup:clipChildren := .T.
   oGroup:type := XBPSTATIC_TYPE_GROUPBOX
   oGroup:create()

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
   oXbp:caption := ":hideButton"
   oXbp:create()
   oXbp:setData( slHideButton )

   oXbp := XbpCheckbox():new( oGroup, , {180,36}, {84,24} )
   oXbp:caption := ":minButton"
   oXbp:create()
   oXbp:setData( slMinButton )

   oXbp := XbpCheckbox():new( oGroup, , {180,12}, {84,24} )
   oXbp:caption := ":maxButton"
   oXbp:create()
   oXbp:setData( slMaxButton )

   oXbp := XbpStatic():new( drawingArea, , {24,132}, {84,24} )
   oXbp:caption := ":border"
   oXbp:clipSiblings := .T.
   oXbp:options := XBPSTATIC_TEXT_VCENTER
   oXbp:create()

   oXbp := XbpStatic():new( drawingArea, , {84,132}, {324,24} )
   oXbp:clipSiblings := .T.
   oXbp:options := XBPSTATIC_TEXT_VCENTER
   oXbp:caption := saBorders[ snBorder, 1 ]
   oXbp:create()

   oXbp := XbpPushButton():new( drawingArea, , {12,12}, {84,24} )
   oXbp:caption := "Close"
   oXbp:create()
   oXbp:activate := {|| PostAppEvent(xbeP_Close,,, oDlg) }

   oGroup:disable()

   oDlg:show()

RETURN

