//////////////////////////////////////////////////////////////////////
//
//  XPPSPOOL.PRG
//
//  Copyright:
//       Alaska Software, (c) 1997-2025. All rights reserved.         
//  
//  Contents:
//       Spool Control Utility
//       uses XbaseTools functions to add files to a spooler, delete
//       files from the spooler, etc.
//   
//////////////////////////////////////////////////////////////////////


#include "common.ch"
#include "appevent.ch"
#include "xbp.ch"
#include "gra.ch"
#include "xppspool.ch"

static oDlg, oDesktop

/*
 * We create main window in function MAIN.
 */
PROCEDURE AppSys
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

    /*
    * Create main application dialog.
    */
    oDesktop := SetAppWindow()

    oDlg := MainDialog():new(oDesktop, oDesktop, {32, 32}, {320, 160}, , .F.)
    oDlg:create()

    SetAppWindow(oDlg)
    SetAppFocus(oDlg)

    /*
     * Start thread that reads job list for spooler.
     * Port: LPT1, Interval: 1 second
     */
    oDlg:oSpoolList:reconfig(1, 100)

    DO WHILE nEvent <> xbeP_Close
        nEvent := AppEvent( @mp1, @mp2, @oXbp )
        oXbp:handleEvent( nEvent, mp1, mp2 )
    ENDDO

RETURN

/*
 * MainDialog class.
 */
CLASS MainDialog FROM XbpDialog
    EXPORTED:
        VAR oSpoolList          
        VAR oConfigDialog       

        METHOD init
        METHOD create
        METHOD resize
        METHOD handleEvent
ENDCLASS

/*
 * Initialize MainDialog.
 */
METHOD MainDialog:init(oParent, oOwner, aPos, aSize, aPresParam, lVisible)

    ::XbpDialog:init(oParent, oOwner, aPos, aSize, aPresParam, lVisible)

    /*
     * Owner of spoollist and configuration dialog is this dialog!
     */
    ::oSpoolList := SpoolList():new(::drawingArea, self, lVisible)
    ::oConfigDialog := ConfigureDialog():new(oParent, self)

RETURN

/*
 * Request system resources for MainDialog.
 */
METHOD MainDialog:create(oParent, oOwner, aPos, aSize, aPresParam, lVisible)
    LOCAL aItems, oMenuBar

    ::XbpDialog:title := "Xbase++ Spool Control Utility"
    ::XbpDialog:icon := ID_APPNAME
    ::XbpDialog:create(oParent, oOwner, aPos, aSize, aPresParam, lVisible)

    ::drawingArea:SetColorBG(GRA_CLR_PALEGRAY)
    ::drawingArea:SetFontCompoundName( "8.Helv.normal" )

    ::oSpoolList:create()
    ::oConfigDialog:create()

    ::show()
RETURN

/*
 * Handle events
 */
METHOD MainDialog:handleEvent(nEvent, mp1, mp2)

    ::XbpDialog:handleEvent(nEvent, mp1, mp2)

    DO CASE
        CASE nEvent == xbeP_Resize
            ::resize(mp1, mp2)

        CASE nEvent == xbeP_Reconfig
            ::oSpoolList:reconfig(mp1, mp2)
    ENDCASE
RETURN

/*
 * Resize spoollist.
 */
METHOD MainDialog:resize(aSizeOld, aSizeNew)
    LOCAL aRect

    /*
     * Get new rectangle.
     */
    aRect := ::CalcClientRect({ 0, 0, aSizeNew[1], aSizeNew[2] })

    /*
     * Resize spool list.
     */
    ::oSpoolList:setSize({aRect[3] - aRect[1], aRect[4] - aRect[2]})

RETURN

