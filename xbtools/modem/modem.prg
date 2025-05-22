//////////////////////////////////////////////////////////////////////
//
//  MODEM.PRG
//
//  Copyright:
//       Alaska Software, (c) 1997-2025. All rights reserved.         
//  
//  Contents:
//       Draw a modem and set its LEDs accordingly to the signals
//       of a given COM - Port. All data sent to this modem will be
//       sent back.
//   
//////////////////////////////////////////////////////////////////////


#include "gra.ch"
#include "xbp.ch"
#include "appevent.ch"

/*
 * Main dialog is available as PUBLIC variable.
 */
MEMVAR oDlg

PROCEDURE AppSys

    /*
     * Place modem on the desktop.
     */
    PUBLIC oDlg := Modem():new(, , , , , .F.)
    oDlg:create()

RETURN

/*
 * We don't need any databases for this application.
 */
PROCEDURE DbeSys
RETURN

/*
 * Main event loop.
 */
PROCEDURE Main()
    LOCAL nEvent, mp1, mp2, oXbp

    SetAppWindow(oDlg)
    SetAppFocus(oDlg)

    oDlg:reconfig(2, 20)

    DO WHILE nEvent <> xbeP_Close
        nEvent := AppEvent( @mp1, @mp2, @oXbp )
        oXbp:handleEvent( nEvent, mp1, mp2 )
    ENDDO

RETURN

