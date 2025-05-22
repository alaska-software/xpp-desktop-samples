//////////////////////////////////////////////////////////////////////
//
//  CONFDIAL.PRG
//
//  Copyright:
//       Alaska Software, (c) 1997-2025. All rights reserved.         
//  
//  Contents:
//       Implementation of configuration dialog for MODEM.EXE
//   
//////////////////////////////////////////////////////////////////////


#include "gra.ch"
#include "xbp.ch"
#include "common.ch"
#include "appevent.ch"

MEMVAR oDlg

/*
 * Configuration Dialog Class
 */
CLASS ConfigureDialog FROM XbpDialog
    EXPORTED:
        VAR ConfigureBox    // Static Box for spin buttons
        VAR COMPortText     // Caption for COM Port select spin button
        VAR COMPortSpin     // COM Port spin button
        VAR IntervalText    // Caption for interval spin button
        VAR IntervalSpin    // Interval spin button
        VAR ButtonOk        // OK button
        VAR ButtonCancel    // Cancel button
        VAR ButtonHelp      // Help button

        METHOD init
        METHOD create
        METHOD popup
ENDCLASS

/*
 * Initialize dialog
 */
METHOD ConfigureDialog:init( oParent, oOwner, aPos, aSize, aPP, lVisible )

    DEFAULT oParent  TO SetAppWindow(), ;
            aPos     TO {0,0}, ;
            aSize    TO {224,192}, ;
            lVisible TO .F.

    ::XbpDialog:init( oParent, oOwner, aPos, aSize, aPP, lVisible )

    ::ConfigureBox := XbpStatic():new( ::drawingArea, , {12,64}, {196,80} )
    ::COMPortText  := XbpStatic():new( ::drawingArea, , {40,108}, {68,24} )
    ::COMPortSpin  := XbpSpinbutton():new( ::drawingArea, , {112,108}, {60,23} )
    ::IntervalText := XbpStatic():new( ::drawingArea, , {52,76}, {56,24} )
    ::IntervalSpin := XbpSpinbutton():new( ::drawingArea, , {112,76}, {60,23} )
    ::ButtonOk     := XbpPushButton():new( ::drawingArea, , {8,20}, {60,32} )
    ::ButtonCancel := XbpPushButton():new( ::drawingArea, , {80,20}, {60,32} )
    ::ButtonHelp   := XbpPushButton():new( ::drawingArea, , {152,20}, {60,32} )
RETURN self


/*
 * Request system resources
 */
METHOD ConfigureDialog:create( oParent, oOwner, aPos, aSize, aPP, lVisible )

    ::XbpDialog:border := XBPDLG_RAISEDBORDERTHICK_FIXED
    ::XbpDialog:minButton := .F.
    ::XbpDialog:maxButton := .F.
    ::XbpDialog:sysMenu := .F.
    ::XbpDialog:taskList := .F.
    ::XbpDialog:title := "Configuration"
    ::XbpDialog:visible := .F.
    ::XbpDialog:create( oParent, oOwner, aPos, aSize, aPP, lVisible )
    ::XbpDialog:drawingArea:setColorBG( GRA_CLR_PALEGRAY )
    ::XbpDialog:drawingArea:setFontCompoundName( "8.Helv.normal" )

    ::ConfigureBox:caption := ""
    ::ConfigureBox:type := XBPSTATIC_TYPE_RECESSEDRECT
    ::ConfigureBox:create()

    ::COMPortText:caption := "COM Port: "
    ::COMPortText:options := XBPSTATIC_TEXT_VCENTER+XBPSTATIC_TEXT_RIGHT
    ::COMPortText:create()

    ::COMPortSpin:create()
    ::COMPortSpin:setColorBG( GRA_CLR_WHITE )
    ::COMPortSpin:setData(2)

    ::IntervalText:caption := "Interval: "
    ::IntervalText:options := XBPSTATIC_TEXT_VCENTER+XBPSTATIC_TEXT_RIGHT
    ::IntervalText:create()

    ::IntervalSpin:create()
    ::IntervalSpin:setColorBG( GRA_CLR_WHITE )
    ::IntervalSpin:setData(20)

    ::ButtonOk:caption := "OK"
    ::ButtonOk:preSelect := .T.
    ::ButtonOk:create()

    /*
     * Hide dialog and reconfigure COM Port thread.
     */
    ::ButtonOK:activate := {|| ::hide(), ;
                                SetAppWindow(oDlg), SetAppFocus(oDlg),;
                                oDlg:reconfig(::COMPortSpin:getData(),;
                                                ::IntervalSpin:getdata()) }

    ::ButtonCancel:caption := "Cancel"
    ::ButtonCancel:create()
    ::ButtonCancel:activate := {|| ::hide(), SetAppWindow(oDlg),;
                                    SetAppFocus(oDlg) }

    ::ButtonHelp:caption := "Help"
    ::ButtonHelp:create()
    ::ButtonHelp:activate := {|| MsgBox("Sorry, no help available"), ::hide(), ;
                                    SetAppWindow(oDlg), SetAppFocus(oDlg)  }

RETURN self

/*
 * Popup dialog
 */
METHOD ConfigureDialog:popup(aPos)

    ::setPos({aPos[1]+20, aPos[2]+20})
    ::show()
    SetAppFocus(::ButtonOK)

RETURN
