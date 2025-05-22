//////////////////////////////////////////////////////////////////////
//
//  KEYGEN.PRG
//
//  Copyright:
//       Alaska Software, (c) 2002-2025. All rights reserved.         
//  
//  Contents:
//       Generating cipher keys
//////////////////////////////////////////////////////////////////////



#include "Gra.ch"
#include "Xbp.ch"
#include "Appevent.ch"
#include "Font.ch"
#include "dmlb.ch"
#include "crypt.ch"
#pragma library ("xppui2")
#pragma library ("adac20b")

PROC APPSYS
RETURN

PROCEDURE Main
   LOCAL nEvent, mp1, mp2
   LOCAL oDlg, oXbp, drawingArea, aEditControls := {}
   LOCAL aPP
   LOCAL oBrowse, oQBrw, oSpds
   LOCAL cTbl := "keys"
   LOCAL oMenu

   oDlg := XbpDialog():new( AppDesktop(), , {160,160}, {600,385})
   oDlg:taskList := .T.
   oDlg:title := "Generating keys"
   oDlg:create()

   /*
    * setup a menu
    */
   oMenu := XbpMenu():new(oDlg:menubar()):create()
   oMenu:title := "~Generate"
   oMenu:addItem({"~Tell me more",{|| ShowHelp(drawingArea)}})
   oMenu:addItem({"~6 keys",   {|| GenKeys(oDlg, cTbl, 2)}})
   oMenu:addItem({"~30 keys",  {|| GenKeys(oDlg, cTbl, 10)}})
   oMenu:addItem({"300 ~keys", {|| GenKeys(oDlg, cTbl, 100)}})
   oMenu:addItem({"3000 ke~ys",{|| GenKeys(oDlg, cTbl, 1000)}})
   oMenu:addItem({"~Exit",     {|| PostAppEvent(xbeP_Close)}})
   oDlg:menubar():addItem({oMenu,})

   SetAppFocus(oDlg)

   drawingArea := oDlg:drawingArea
   drawingArea:setFontCompoundName( "8.Arial" )

   /*
    * open the keys table
    */
   OpenKeys(oDlg, cTbl)

   aPP := {{ XBP_PP_COMPOUNDNAME, FONT_DEFFIXED_SMALL}}

   /*
    * Use XbpSortedQuickBrowse() if installed, XbpQuickBrowse() otherwise
    */
   IF DllLoad("XPPSQBRW")#0
      oQBrw := &("XbpSortedQuickBrowse")()
      oBrowse := oQBrw:new( oDlg:drawingArea,,,oDlg:drawingArea:currentSize(), aPP)
      oBrowse:style    := XBP_STYLE_SYSTEMDEFAULT
      oSPds :=  &("DacSortedPagedDataStore")()
      oBrowse:dataLink := oSPds:new()
      oBrowse:create()
      oBrowse:dataLink:setOrder({{"KEY", "KEY", "KEYD"},;
                                 {"LENGTH", "LENGTH", "LENGTHD"},;
                                 {"CREATED", "CREATED", "CREATEDD"},;
                                 {"SECONDS", "SECONDS", "SECONDSD"}})
   ELSE
      oBrowse := XbpQuickBrowse():new( oDlg:drawingArea,,,;
                                       oDlg:drawingArea:currentSize(), aPP)
      oBrowse:style    := XBP_STYLE_SYSTEMDEFAULT
      oBrowse:dataLink := DacPagedDataStore():new()
      oBrowse:create()
   ENDIF
   oBrowse:setColWidth(480, 1)

   /*
    * resize browse on resize of dialog
    */
   oDlg:drawingArea:resize := ;
       {|mp1,mp2,obj| oBrowse:setSize(mp2), ;
                      oBrowse:dataLink:setAbsolutePageSize( oBrowse:rowCount ) }

   SetAppFocus(oBrowse)

   nEvent := xbe_None
   DO WHILE nEvent <> xbeP_Close
      nEvent := AppEvent( @mp1, @mp2, @oXbp )
      oXbp:handleEvent( nEvent, mp1, mp2 )
   ENDDO
RETURN

/*
 * Open key table or create new table on 1st start
 */
PROCEDURE OpenKeys(oParent, cTbl)
LOCAL aStr := {{"KEY",     "C",64, 0},;
               {"LENGTH",  "N", 3, 0},;
               {"CREATED", "C",19, 0},;
               {"SECONDS", "N",12, 2}}
FIELD key, length, created, seconds
LOCAL oWait

     SET CENTURY ON
     IF !FILE(cTbl+"."+DbeInfo(COMPONENT_DATA, DBE_EXTENSION))
        DbCreate(cTbl, aStr)
     ENDIF
     USE (cTbl) NEW
     oWait:=WaitMsg(oParent,;
            Var2char(Reccount())+" keys in table. Now creating index.")
     /*
      * Delete leftover index file completely, this 
      * guaranteess proper recreation of index
      */
     Ferase(cTbl+"."+DbeInfo(COMPONENT_ORDER, DBE_EXTENSION))
     INDEX ON KEY     TAG KEY      TO (cTbl) UNIQUE
     INDEX ON KEY     TAG KEYD     TO (cTbl) UNIQUE DESCENDING
     INDEX ON LENGTH  TAG LENGTH   TO (cTbl) 
     INDEX ON LENGTH  TAG LENGTHD  TO (cTbl) DESCENDING
     INDEX ON CREATED TAG CREATED  TO (cTbl) 
     INDEX ON CREATED TAG CREATEDD TO (cTbl) DESCENDING
     INDEX ON SECONDS TAG SECONDS  TO (cTbl) 
     INDEX ON SECONDS TAG SECONDSD TO (cTbl) DESCENDING
     oWait:destroy()
RETURN

/*
 * Generate keys and store into table
 */
PROCEDURE GenKeys(oParent, cTbl, nMax)
LOCAL oKey, cKey
LOCAL nKeyLen
LOCAL nStart, i, nTotal
LOCAL nOldFocus
LOCAL oWait
FIELD key, length, created, seconds

     /*
      * suspend notifications to avoid a flickering browse
      */
     DbSuspendNotifications()
     /*
      * display a message on top of the browse
      */
     oWait:=WaitMsg(oParent,"Generating "+var2char(nMax*3)+" cipher keys")
     /*
      * set order focus to first (unique) index tag
      */
     nOldFocus := OrdSetFocus(1)
     nStart := Seconds()
     nTotal := 0
     FOR nKeyLen := 128 TO 256 STEP 64
         FOR i:= 1 TO nMax
             oKey := SecureKey():generate(nKeyLen)
             cKey := padr(oKey:toString(), FieldInfo(FieldPos("KEY"), FLD_LEN))
             /*
              * check if key already exists 
              */
             IF !DbSeek(cKey)
                 APPEND BLANK
                 key := cKey
                 length := nKeyLen
                 created := dtoc(Date())+" "+time()
                 seconds := Seconds()
                 DbCommit()
                 nTotal++
             ELSE
                 ConfirmBox(oParent,"Key already exists! "+ str(recno()),;
                            "KeyGen", XBPMB_OK, XBPMB_CRITICAL)
             ENDIF
         NEXT
     NEXT
     nStart := Seconds()-nStart      
     /*
      * allow notifications 
      */
     DbResumeNotifications()
     /*
      * reset previous order focus (used by sorted quick browse)
      */
     OrdSetFocus(nOldFocus)
     oWait:destroy()
     ConfirmBox(oParent,str(nTotal,6)+" new keys, "+ str(reccount())+" total keys, " +;
                   str(nTotal/nStart,6)+" keys/second",;
                   "KeyGen", XBPMB_OK, XBPMB_INFORMATION)
RETURN 

/*
 * Show help text
 */
PROCEDURE ShowHelp(oParent)
LOCAL cMsg
    TEXT INTO cMsg WRAP CHR(13)+CHR(10)
        This sample shows how to generate cipher keys for later
        use.
        It also let's you visually inspect the quality of the
        generated keys.
        Click on the headings of the columns to sort by this column.
        You should not find any relations between keys, the length of the key
        or the time when the key is created.
    ENDTEXT
    ConfirmBox(oParent, cMsg, "KeyGen", XBPMB_OK, XBPMB_INFORMATION)
RETURN

/*
 * Display a message on the screen
 * Call :destroy() to hide message
 */
FUNCTION WaitMsg(oParent, cMsg)
LOCAL oMsg
LOCAL oText
LOCAL aPos
LOCAL aSize   

    /*
     * create a recessed box and a text field inside of it
     */
    oMsg := XbpStatic():new(oParent,,,,,.F.)
    oMsg:type := XBPSTATIC_TYPE_RAISEDBOX
    oMsg:create()

    oText := XbpStatic():new(oMsg,,{8,6})
    oText:caption := cMsg
    oText:type := XBPSTATIC_TYPE_TEXT 
    oText:autoSize := .T.
    oText:create()

    aSize := oText:currentSize()
    aSize[1]+=16
    aSize[2]+=16
    oMsg:setSize(aSize)
    /*
     * center message to parent
     */
    aPos := CenterPos(aSize, oParent:currentSize())
    oMsg:setPos(aPos)
    oMsg:show()
RETURN oMsg

FUNCTION  CenterPos( aSize, aRefSize ) 
RETURN  { Int( (aRefSize[1] - aSize[1]) / 2 ) ;
         , Int( (aRefSize[2] - aSize[2]) / 2 ) }

PROC dbesys
    DbeLoad("DBFDBE", .T.)
    DbeLoad("CDXDBE", .T.)
    DbeBuild("DBFCDX", "DBFDBE", "CDXDBE")
RETURN

