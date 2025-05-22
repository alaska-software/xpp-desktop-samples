//////////////////////////////////////////////////////////////////////
//
//  CONFDIAL.PRG
//
//  Copyright:
//       Alaska Software, (c) 1997-2025. All rights reserved.         
//  
//  Contents:
//       Implementation of configuration dialog for XPPSPOOL.EXE
//   
//////////////////////////////////////////////////////////////////////



#include "gra.ch"
#include "xbp.ch"
#include "common.ch"
#include "appevent.ch"
#include "xppspool.ch"


/*
 * Configuration Dialog Class
 */
CLASS ConfigureDialog FROM XbpDialog
    EXPORTED:
        VAR ConfigureBox    // Static Box that holds the spin buttons
        VAR LPTPortText     // Caption of port spin button
        VAR LPTPortSpin     // Port spin button
        VAR IntervalText    // Caption of interval spin button
        VAR IntervalSpin    // Interval spin button
        VAR ButtonOk        // OK button
        VAR ButtonCancel    // Cancel button
        VAR ButtonHelp      // Help button

        VAR nLPTPort        // LPT port
        VAR nInterval       // Redraw interval in 1/100 seconds

        METHOD init
        METHOD create
        METHOD popup
ENDCLASS

/*
 * Initialize dialog
 */
METHOD ConfigureDialog:init(oParent, oOwner, aPos, aSize, aPP, lVisible)

    DEFAULT oParent  TO SetAppWindow():setParent(), ;
            aPos     TO {0,0}, ;
            aSize    TO {224,192}, ;
            lVisible TO .F.

    ::XbpDialog:init( oParent, oOwner, aPos, aSize, aPP, lVisible )

    ::ConfigureBox := XbpStatic():new( ::drawingArea, , {12,64}, {196,80} )
    ::LPTPortText  := XbpStatic():new( ::drawingArea, , {40,108}, {68,24} )
    ::LPTPortSpin  := XbpSpinbutton():new( ::drawingArea, , {112,108}, {60,23} )
    ::IntervalText := XbpStatic():new( ::drawingArea, , {52,76}, {56,24} )
    ::IntervalSpin := XbpSpinbutton():new( ::drawingArea, , {112,76}, {60,23} )
    ::ButtonOk     := XbpPushButton():new( ::drawingArea, , {8,20}, {60,32} )
    ::ButtonCancel := XbpPushButton():new( ::drawingArea, , {80,20}, {60,32} )
    ::ButtonHelp   := XbpPushButton():new( ::drawingArea, , {152,20}, {60,32} )

    /*
     * Default port LPT1, default interval: 1 second
     */
    ::nLPTPort := 1
    ::nInterval := 100

RETURN self

/*
 * Request system resources
 */
METHOD ConfigureDialog:create( oParent, oOwner, aPos, aSize, aPP, lVisible )

    ::XbpDialog:border := XBPDLG_RAISEDBORDERTHICK_FIXED
    ::XbpDialog:maxButton := .F.
    ::XbpDialog:minButton := .F.
    ::XbpDialog:sysMenu := .F.
    ::XbpDialog:icon := ID_APPNAME
    ::XbpDialog:taskList := .F.
    ::XbpDialog:title := "SpoolControl V1.0"
    ::XbpDialog:visible := .F.
    ::XbpDialog:create( oParent, oOwner, aPos, aSize, aPP, lVisible )
    ::XbpDialog:drawingArea:setColorBG( GRA_CLR_PALEGRAY )
    ::XbpDialog:drawingArea:setFontCompoundName( "8.Helv.normal" )

    ::ConfigureBox:caption := ""
    ::ConfigureBox:type := XBPSTATIC_TYPE_RECESSEDRECT
    ::ConfigureBox:create()

    ::LPTPortText:caption := "LPT Port: "
    ::LPTPortText:options := XBPSTATIC_TEXT_VCENTER+XBPSTATIC_TEXT_RIGHT
    ::LPTPortText:create()

    ::LPTPortSpin:create()
    ::LPTPortSpin:setColorBG( GRA_CLR_WHITE )
    ::LPTPortSpin:setData(2)

    ::IntervalText:caption := "Interval: "
    ::IntervalText:options := XBPSTATIC_TEXT_VCENTER+XBPSTATIC_TEXT_RIGHT
    ::IntervalText:create()

    ::IntervalSpin:create()
    ::IntervalSpin:setColorBG( GRA_CLR_WHITE )
    ::IntervalSpin:setData(100)

    ::ButtonOk:caption := "OK"
    ::ButtonOk:preSelect := .T.
    ::ButtonOk:create()
    /*
     * Post RECONFIG-event, if button has been pressed.
     */
    ::ButtonOK:activate := {|| ::nLPTPort := ::LPTPortSpin:getData(),;
                                ::nInterval := ::IntervalSpin:getData(),;
                                PostAppEvent(xbeP_Reconfig, ::nLPTPort,;
                                                ::nInterval, ::setOwner()),;
                                ::hide(),;
                                SetAppFocus(::setOwner())}

    ::ButtonCancel:caption := "Cancel"
    ::ButtonCancel:create()
    ::ButtonCancel:activate := {|| ::hide()}

    ::ButtonHelp:caption := "Help"
    ::ButtonHelp:create()
    ::ButtonHelp:activate := {|| MsgBox("Sorry, no help available"), ::hide()}
RETURN self

/*
 * Popup dialog
 */
METHOD ConfigureDialog:popup(aPos)

    ::IntervalSpin:setData(::nInterval)
    ::LPTPortSpin:setData(::nLPTPort)
    ::setPos({aPos[1]+32, aPos[2]+32})
    ::show()
    SetAppFocus(::ButtonOK)

RETURN
