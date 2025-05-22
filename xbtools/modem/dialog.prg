//////////////////////////////////////////////////////////////////////
//
//  DIALOG.PRG
//
//  Copyright:
//       Alaska Software, (c) 1997-2025. All rights reserved.         
//  
//  Contents:
//       Implementation of main dialog for MODEM.EXE
//   
//////////////////////////////////////////////////////////////////////


#include "gra.ch"
#include "xbp.ch"
#include "common.ch"
#include "appevent.ch"

#include "modres.ch"
#include "xbtcom.ch"
#include "natmsg.ch"

MEMVAR lStopThread

/*
 * Loop-Back MODEM Class
 */
CLASS Modem FROM XbpDialog
    EXPORTED:
        VAR OuterBox        // Boxes, Bitmaps, Text and Button of
        VAR InnerBox        // Modem outline
        VAR ExitButton
        VAR PowerText
        VAR RTSText
        VAR CTSText
        VAR DTRText
        VAR DSRText
        VAR DCDText
        VAR ONLText
        VAR RTSBmp
        VAR CTSBmp
        VAR DTRBmp
        VAR DSRBmp
        VAR DCDBmp
        VAR ONLBmp

        VAR nComPort        // COM Port
        VAR nInterval       // Time to sleep before next loop
        VAR COMThread       // Thread that reads COM Port

        VAR ConfigDialog    // configuration dialog

        METHOD init
        METHOD create
        METHOD reconfig
ENDCLASS

/*
 * Initialize dialog
 */
METHOD Modem:init( oParent, oOwner, aPos, aSize, aPP, lVisible )

    DEFAULT oParent  TO SetAppWindow(), ;
            aPos     TO {32,32}, ;
            aSize    TO {235,67}, ;
            lVisible TO .F.

    /*
     * Create sync variable
     */
    PUBLIC lStopThread := .F.

    ::XbpDialog:init( oParent, oOwner, aPos, aSize, aPP, lVisible )

    /*
     * Init modem outline and all LEDs
     */
    ::OuterBox := XbpStatic():new( ::drawingArea, , {0,0}, {232,44} )
    ::InnerBox := XbpStatic():new( ::drawingArea, , {6,6}, {220,32} )

    ::ExitButton := XbpPushButton():new( ::drawingArea, , {187,21}, {30,13} )
    ::PowerText := XbpStatic():new( ::drawingArea, , {182,8}, {40,12} )

    ::RTSText := XbpStatic():new( ::drawingArea, , {8,8}, {24,12} )
    ::CTSText := XbpStatic():new( ::drawingArea, , {32,8}, {24,12} )
    ::DTRText := XbpStatic():new( ::drawingArea, , {56,8}, {24,12} )
    ::DSRText := XbpStatic():new( ::drawingArea, , {80,8}, {24,12} )
    ::DCDText := XbpStatic():new( ::drawingArea, , {104,8}, {24,12} )
    ::ONLText := XbpStatic():new( ::drawingArea, , {140,8}, {24,12} )

    ::RTSBmp := XbpStatic():new(::drawingArea, ,{11, 20}, {16, 16})
    ::RTSBmp:caption := ID_BMP_LEDOFF
    ::RTSBmp:type := XBPSTATIC_TYPE_BITMAP

    ::CTSBmp := XbpStatic():new(::drawingArea, ,{35, 20}, {16, 16})
    ::CTSBmp:caption := ID_BMP_LEDOFF
    ::CTSBmp:type := XBPSTATIC_TYPE_BITMAP

    ::DTRBmp := XbpStatic():new(::drawingArea, ,{59, 20}, {16, 16})
    ::DTRBmp:caption := ID_BMP_LEDOFF
    ::DTRBmp:type := XBPSTATIC_TYPE_BITMAP

    ::DSRBmp := XbpStatic():new(::drawingArea, ,{83, 20}, {16, 16})
    ::DSRBmp:caption := ID_BMP_LEDOFF
    ::DSRBmp:type := XBPSTATIC_TYPE_BITMAP

    ::DCDBmp := XbpStatic():new(::drawingArea, ,{107, 20}, {16, 16})
    ::DCDBmp:caption := ID_BMP_LEDOFF
    ::DCDBmp:type := XBPSTATIC_TYPE_BITMAP

    ::ONLBmp := XbpStatic():new(::drawingArea, ,{143, 20}, {16, 16})
    ::ONLBmp:caption := ID_BMP_LEDOFF
    ::ONLBmp:type := XBPSTATIC_TYPE_BITMAP

    ::ConfigDialog := ConfigureDialog():new()

    ::nComPort := 0
    ::nInterval := 0

RETURN self

/*
 * Request system resources
 */
METHOD Modem:create( oParent, oOwner, aPos, aSize, aPP, lVisible )

    ::XbpDialog:border := XBPDLG_NO_BORDER
    ::XbpDialog:titleBar := .T.
    ::XbpDialog:icon := ID_APPNAME
    ::XbpDialog:sysMenu := .F.
    ::XbpDialog:maxButton := .F.
    ::XbpDialog:minButton := .F.
    ::XbpDialog:taskList := .T.
    ::XbpDialog:title := "Xbase++ LoopBack Modem"
    ::XbpDialog:visible := .F.
    ::XbpDialog:create( oParent, oOwner, aPos, aSize, aPP, lVisible )
    ::XbpDialog:drawingArea:setColorBG( GRA_CLR_BLACK )
    ::XbpDialog:drawingArea:setColorFG( GRA_CLR_DARKGRAY )

    ::OuterBox:caption := ""
    ::OuterBox:type := XBPSTATIC_TYPE_RAISEDRECT
    ::OuterBox:create()

    ::InnerBox:caption := ""
    ::InnerBox:type := XBPSTATIC_TYPE_RECESSEDRECT
    ::InnerBox:create()

    ::ExitButton:caption := ""
    ::ExitButton:pointerFocus := .F.
    ::ExitButton:create()
    ::ExitButton:setColorFG( GRA_CLR_DARKGRAY )
    ::ExitButton:setColorBG( GRA_CLR_WHITE )
    ::ExitButton:activate := {|| PostAppEvent(xbeP_Close) }

    ::PowerText:caption := "POWER"
    ::PowerText:options := XBPSTATIC_TEXT_VCENTER+XBPSTATIC_TEXT_CENTER
    ::PowerText:create()
    ::PowerText:setColorFG( GRA_CLR_WHITE )
    ::PowerText:setFontCompoundName( nationMsg( NAT_MSG_FNT_COURIER_SMALL ) )

    ::RTSText:caption := "RTS"
    ::RTSText:options := XBPSTATIC_TEXT_VCENTER+XBPSTATIC_TEXT_CENTER
    ::RTSText:create()
    ::RTSText:setColorFG( GRA_CLR_WHITE )
    ::RTSText:setFontCompoundName( nationMsg( NAT_MSG_FNT_COURIER_SMALL ) )

    ::CTSText:caption := "CTS"
    ::CTSText:options := XBPSTATIC_TEXT_VCENTER+XBPSTATIC_TEXT_CENTER
    ::CTSText:create()
    ::CTSText:setColorFG( GRA_CLR_WHITE )
    ::CTSText:setFontCompoundName( nationMsg( NAT_MSG_FNT_COURIER_SMALL ) )

    ::DTRText:caption := "DTR"
    ::DTRText:options := XBPSTATIC_TEXT_VCENTER+XBPSTATIC_TEXT_CENTER
    ::DTRText:create()
    ::DTRText:setColorFG( GRA_CLR_WHITE )
    ::DTRText:setFontCompoundName( nationMsg( NAT_MSG_FNT_COURIER_SMALL ) )

    ::DSRText:caption := "DSR"
    ::DSRText:options := XBPSTATIC_TEXT_VCENTER+XBPSTATIC_TEXT_CENTER
    ::DSRText:create()
    ::DSRText:setColorFG( GRA_CLR_WHITE )
    ::DSRText:setFontCompoundName( nationMsg( NAT_MSG_FNT_COURIER_SMALL ) )

    ::DCDText:caption := "DCD"
    ::DCDText:options := XBPSTATIC_TEXT_VCENTER+XBPSTATIC_TEXT_CENTER
    ::DCDText:create()
    ::DCDText:setColorFG( GRA_CLR_WHITE )
    ::DCDText:setFontCompoundName( nationMsg( NAT_MSG_FNT_COURIER_SMALL ) )

    ::ONLText:caption := "ONL"
    ::ONLText:options := XBPSTATIC_TEXT_VCENTER+XBPSTATIC_TEXT_CENTER
    ::ONLText:create()
    ::ONLText:setColorFG( GRA_CLR_WHITE )
    ::ONLText:setFontCompoundName( nationMsg( NAT_MSG_FNT_COURIER_SMALL ) )
    ::ONLText:RbClick := {||::ConfigDialog:popup(::currentPos())}

    ::RTSBmp:create()
    ::CTSBmp:create()
    ::DTRBmp:create()
    ::DSRBmp:create()
    ::DCDBmp:create()
    ::ONLBmp:create()
    ::ONLBmp:RbClick := {||::ConfigDialog:popup(::currentPos())}

    ::ConfigDialog:create()

    ::InnerBox:RbClick := {||::ConfigDialog:popup(::currentPos())}

    ::show()

    /*
     * create COM Port thread
     */
    ::COMThread := Thread():new()

RETURN self

/*
 * Reconfigure COM Port thread
 */
METHOD Modem:reconfig(nComPort, nInterval)

    IF nComPort == ::nComPort .AND. nInterval == ::nInterval
        RETURN
    ENDIF

    /*
     * If COM Port thread is active, set sync variable
     * and wait for its termination.
     */
    IF ::COMThread:active
        lStopThread := .T.
        ::COMThread:synchronize(0)
    ENDIF

    lStopThread := .F.
    /*
     * Set new COM Port and new interval
     */
    ::nComPort := nComPort
    ::nInterval := nInterval

    /*
     * Restart thread
     */
    ::COMThread:start("ComReadFun", self)

RETURN

/*
 * Read data from COM Port, send all data back and set LEDs
 */
PROC ComReadFun(oDlg)
    LOCAL n, nComPort := oDlg:nComPort
    LOCAL cData

    /*
     * Try to open COM Port.
     */
    IF !COM_OPEN(nComPort)
       RETURN
    ENDIF

    COM_INIT(nComPort, 19200, "N", 8, 1)
    /*
     * Write request completes when all of the data
     * has been written.
     */
    COM_SENDMODE(nComPort, INFINITE_WRITE_TIMEOUT)
    COM_HARD(nComPort, .T.)
    COM_SOFT(nComPort, .F.)

    /*
     * As long as sync variable contains FALSE ...
     */
    WHILE !lStopThread
        /*
         * Turn on ONL - LED
         */
        setLED(oDlg:ONLBmp, .T.)
        IF Len(cData := COM_READ(nComPort)) > 0
           /*
            * Back to sender.
            */
           COM_SEND(nComPort, cData)
        ENDIF
        /*
         * Turn off ONL - LED
         */
        setLED(oDlg:ONLBmp, .F.)

        /*
         * Set LEDs.
         */
        setLED(oDlg:RTSBmp, COM_RTS(nComPort))
        setLED(oDlg:CTSBmp, COM_CTS(nComPort))
        setLED(oDlg:DTRBmp, COM_DTR(nComPort))
        setLED(oDlg:DSRBmp, COM_DSR(nComPort))
        setLED(oDlg:DCDBmp, COM_DCD(nComPort))
        Sleep(oDlg:nInterval)
    END
    COM_CLOSE(nComPort)

RETURN

/*
 * Set LED.
 */
PROC setLED(oBmp, lSignal)
    IF lSignal
        IF oBmp:caption == ID_BMP_LEDOFF
            oBmp:caption := ID_BMP_LEDON
            oBmp:configure()
        ENDIF
    ELSE
        IF oBmp:caption == ID_BMP_LEDON
            oBmp:caption := ID_BMP_LEDOFF
            oBmp:configure()
        ENDIF
    ENDIF
RETURN

/*
 * Let LED blink.
 */
PROC blinkLED(oBmp)

    oBmp:caption := ID_BMP_LEDON
    oBmp:configure()
    Sleep(20)
    oBmp:caption := ID_BMP_LEDOFF
    oBmp:configure()

RETURN


