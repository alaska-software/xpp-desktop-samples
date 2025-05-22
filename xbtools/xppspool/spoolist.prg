//////////////////////////////////////////////////////////////////////
//
//  SPOOLIST.PRG
//
//  Copyright:
//       Alaska Software, (c) 1997-2025. All rights reserved.         
//  
//  Contents:
//       Implementation of spooler job listbox for XPPSPOOL.EXE
//   
//////////////////////////////////////////////////////////////////////


#include "gra.ch"
#include "xbp.ch"
#include "common.ch"
#include "appevent.ch"
#include "xbtprint.ch"
#include "xppspool.ch"

#pragma Library( "xppui2.lib" )

/*
 * PUBLIC thread sync variable
 */
MEMVAR lStopThread

/*
 * SpoolList Class
 */
CLASS SpoolList FROM XbpListBox
    EXPORTED:
        VAR nLPTPort        // LPT port to read from
        VAR nInterval       // Sleep before read port again
        VAR oHeadLine       // Headline : "<x> jobs for LPT<y>"
        VAR oPopUp          // Context Menu
        VAR oFileSelect     // File Select Dialog
        VAR SpoolThread     // Spool Read Thread
        VAR aJobList        // List of jobs for port

        METHOD init
        METHOD create
        METHOD popupContextMenu
        METHOD addFile
        METHOD deleteMarked
        METHOD reconfig
        METHOD setSize
ENDCLASS

/*
 * Initialize dialog
 */
METHOD SpoolList:init(oParent, oOwner, lVisible)

    /*
     * Create sync variable
     */
    PUBLIC lStopThread := .F.

    ::XbpListBox:init(oParent, oOwner, {0,0}, {120,120}, , lVisible)
    ::oHeadLine := XbpStatic():new(oParent, oOwner, {0, 100}, {20, 20})
    ::oFileSelect := XbpFiledialog():new(oParent, oOwner)

    ::nLPTPort := 0
    ::nInterval := 0

    ::aJobList := {}

    /*
     * create thread for reading the port
     */
    ::SpoolThread := Thread():new()

RETURN self

/*
 * Request system resources
 */
METHOD SpoolList:create(oParent, oOwner, aPos, aSize, aPP, lVisible)
    LOCAL aItems

    ::XbpListBox:adjustHeight := .F.
    ::XbpListBox:horizScroll := .T.
    ::XbpListBox:multiSelection := .T.
    ::XbpListBox:create(oParent, oOwner, aPos, aSize, aPP, lVisible)
    ::XbpListBox:setColorBG(GRA_CLR_WHITE)
    ::XbpListBox:SetFontCompoundName( "8.Courier.normal" )

    ::oFileSelect:create()

    ::oHeadLine:caption := ""
    ::oHeadLine:create()
    ::oHeadLine:setColorBG(GRA_CLR_PALEGRAY)
    ::oHeadLine:setColorFG(GRA_CLR_BLACK)
    ::oHeadLine:SetFontCompoundName( "8.Helv.normal" )

    /*
     * Items of context-menu.
     */
    aItems := { ;
        { "~Configure ...", ;
                {||::setOwner():oConfigDialog:popup({20,20}) }, 0, 0 }, ;
        { , , XBPMENUBAR_MIS_SEPARATOR, 0 }, ;
        { "~Add Job", {|| ::addFile() }, 0, 0 }, ;
        { "~Delete Jobs", {;
                { "~Current", ;
                    {|| SpoolControl(SPL_DELETE_CURJOB, ::nLPTPort) }, 0, 0 }, ;
                { "~Selected", ;
                    {|| ::deleteMarked() }, 0, 0 }, ;
                { "~All", ;
                    {|| SpoolFlush(::nLPTPort) }, 0, 0 } ;
            }, 0, 0 }, ;
        { , , XBPMENUBAR_MIS_SEPARATOR, 0 }, ;
        { "~Pause Printing", ;
                {|| SpoolControl(SPL_PAUSE, ::nLPTPort)}, 0, 0 }, ;
        { "~Countinue Printing", ;
                {|| SpoolControl(SPL_CONTINUE, ::nLPTPort)}, 0, 0 }, ;
        { , , XBPMENUBAR_MIS_SEPARATOR, 0 }, ;
        { "~Hold Spooler", ;
                {|| SpoolControl(SPL_HOLD, ::nLPTPort)}, 0, 0 }, ;
        { "~Release Spooler", ;
                {|| SpoolControl(SPL_RELEASE, ::nLPTPort)}, 0, 0 }, ;
        { , , XBPMENUBAR_MIS_SEPARATOR, 0 }, ;
        { "~Exit", ;
                {|| PostAppEvent(xbeP_Close) }, 0, 0 } }

    ::oPopUp := MenuNew(self, "Actions", aItems)

    ::setSize(::setParent():currentSize())

    ::RbDown := {|aPos| ::popupContextMenu(aPos)}
    ::show()

RETURN self

/*
 * Resize spoollist.
 */
METHOD SpoolList:setSize(aSize)
    LOCAL lStr

    /*
     * Resize Headline.
     */
    ::oHeadLine:setPos({0, aSize[2]-::oHeadLine:currentSize()[2]})
    ::oHeadLine:setSize({aSize[1], ::oHeadLine:currentSize()[2]})
    IF ::nLPTPort == 0
        ::nLPTPort := 1
    ENDIF
    lStr := AllTrim(Str(SpoolCount(::nLPTPort), 3))
    lStr += " job(s) for printer "
    lStr += SpoolName(::nLPTPort)+":"
    ::oHeadLine:setCaption(lStr)
    /*
     * Resize listbox.
     */
    ::XbpListBox:setSize({aSize[1], aSize[2]-::oHeadLine:currentSize()[2]})
RETURN

/*
 * Popup context menu.
 */
METHOD SpoolList:popupContextMenu(aPos)

    ::oPopUp:PopUp(self, aPos, , ;
                    XBPMENU_PU_DEFAULT + XBPMENU_PU_MOUSE_RBDOWN)
RETURN

/*
 * Reconfigure Spool Port thread
 */
METHOD SpoolList:reconfig(nLPTPort, nInterval)

    IF nLPTPort == ::nLPTPort .AND. nInterval == ::nInterval
        RETURN
    ENDIF

    /*
     * If thread is active, set sync variable
     * and wait for its termination.
     */
    IF ::SpoolThread:active
        lStopThread := .T.
        ::SpoolThread:synchronize(0)
    ENDIF

    lStopThread := .F.
    /*
     * Set new LPT Port and new interval
     */
    ::nLPTPort := nLPTPort
    ::nInterval := nInterval
    ::oHeadLine:setCaption(AllTrim(Str(SpoolCount(nLPTPort), 3))+;
                            " job(s) for printer "+SpoolName(nLPTPort)+":")
    /*
     * Restart thread
     */
    ::SpoolThread:start("SpoolReadFun", self)

RETURN

/*
 * Open file select dialog and add selected file to spooler.
 */
METHOD SpoolList:addFile
    LOCAL cFile

    /*
     * Popup file select dialog.
     */
    ::oFileSelect:title := "Select file to send"
    cFile := ::oFileSelect:open("*")

    IF cFile == NIL
        RETURN
    ENDIF

    /*
     * Add file.
     */
    SpoolAdd(cFile, ::nLPTPort)
RETURN

/*
 * Delete all files marked in listbox from spooler.
 */
METHOD SpoolList:deleteMarked
    LOCAL aMarked := ::getdata()
    LOCAL n

    /*
     * Get job titles.
     */
    FOR n := 1 TO Len(aMarked)
        aMarked[n] := ::aJobList[aMarked[n]]
    NEXT

    /*
     * Delete jobs
     */
    FOR n := 1 TO Len(aMarked)
        SpoolDel(aMarked[n], ::nLPTPort)
    NEXT

RETURN

/*
 * Read Spool Port
 */
PROC SpoolReadFun(oList)
    LOCAL n, nLPTPort := oList:nLPTPort
    LOCAL aNewJobList

    /*
     * As long as sync variable contains FALSE ...
     */
    WHILE !lStopThread
        aNewJobList := {}
        IF SpoolActiv(nLPTPort)
            /*
             * Get new job list.
             */
            FOR n := 1 TO SpoolCount(nLPTPort)
                Aadd(aNewJobList, SpoolEntry(n, nLPTPort))
            NEXT
        ELSE
            oList:oHeadLine:setCaption("No spooler active for printer "+SpoolName(nLPTPort))
        ENDIF
        IF !AComp(oList:aJobList, aNewJobList)
            /*
             * Display new job list.
             */
            oList:clear()
            FOR n := 1 TO SpoolCount(nLPTPort)
                oList:addItem(Str(n, 3)+" "+CHR(179)+SpoolEntry(n, nLPTPort))
            NEXT
            oList:oHeadLine:setCaption(AllTrim(Str(SpoolCount(nLPTPort), 3))+;
                            " job(s) for printer "+SpoolName(nLPTPort)+":")
            oList:aJobList := aNewJobList
        ENDIF
        /*
         * Sleep before next turn.
         */
        Sleep(oList:nInterval)
    END

RETURN

/*
 * Create a menu and add items in <aItems>.
 */
STATIC FUNCTION MenuNew(oParent, cTitle, aItems, bActivateBlock)
    LOCAL oMenu, i

    /*
     * create new menu
     */
    oMenu := XbpMenu():new(oParent)
    oMenu:create()
    oMenu:SetFontCompoundName( "8.Helv.normal" )

    oMenu:title := cTitle

    /*
     * Add items
     */
    IF aItems != NIL
        FOR i := 1 TO Len(aItems)
            IF ValType(aItems[i,2]) == "A"
                oMenu:addItem({MenuNew(oMenu, aItems[i,1], aItems[i,2]), , 0, 0})
            ELSE
                oMenu:addItem(aItems[i])
            ENDIF
        NEXT
    ENDIF

    /*
     * set activate codeblock
     */
    oMenu:activateItem := bActivateBlock

RETURN oMenu


/*
 * Compare arrays.
 * Return .T. if both arrays contain the same elements.
 */
STATIC FUNCTION AComp(aArr1, aArr2)
    LOCAL n, nLen

    IF ValType(aArr1) != "A" .OR. ValType(aArr2) != "A"
        RETURN .F.
    ENDIF

    nLen := Len(aArr1)
    IF nLen != Len(aArr2)
        RETURN .F.
    ENDIF

    FOR n := 1 TO nLen
        IF aArr1[n] != aArr2[n]
            RETURN .F.
        ENDIF
    NEXT
RETURN .T.
