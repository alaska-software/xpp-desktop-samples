//////////////////////////////////////////////////////////////////////
//
//  DB2.PRG
//
//  Copyright:
//      Alaska Software, (c) 2000-2025. All rights reserved.         
//  
//  Contents:
//      This sample shows how to import data from a DB2 database
//      and export them into an Excel spreadsheet.
//   
//////////////////////////////////////////////////////////////////////


#pragma library( "XppUi2.LIB" )
#pragma library( "XppUi3.LIB" )
#pragma library( "ADAC20B.LIB" )
#include "xbp.ch"
#include "font.ch"
#include "Appevent.ch"
/*
 * Including odbcdbe.ch includes requests to ODBCUTIL.LIB.
 * Please see odbcdbe.ch for details.
 */
#include "odbcdbe.ch"
#include "dmlb.ch"

STATIC oState, oProgr

PROCEDURE MAIN()
   LOCAL nEvent:=0, mp1:=0, mp2:=0, oDlg, oParent, oXbp := NIL
   LOCAL oWiz1, oWiz2, oWiz3, oWiz4, oWiz5, oWiz6
   LOCAL oSource, oTarget

   /*
    * This uses the SAMPLE database of a DB2/NT Personal Edition
    * and the MS-Excel driver
    */
   oSource := OdbcData():new("DBE=ODBCDBE;DSN=SAMPLE;UID=dba;DBALIAS=SAMPLE",;
                              "gt.sales", .F.)
   oTarget := OdbcData():new("DBE=ODBCDBE;DRIVER=Microsoft Excel Driver (*.xls);"+;
                              "ReadOnly=0;UID=;PWD=;DBQ=test.xls", "Mydata", .T.)

   oDlg := GuiStdDialog( "DB2/ODBCDBE Sample" , {400, 440})
   oParent := oDlg:drawingArea

   oProgr := XbpStatic():new(oDlg:drawingArea,,{10, 3},{38,20})
   oProgr:options := XBPSTATIC_TYPE_TEXT
   oProgr:create()
   oState := XbpStatic():new(oDlg:drawingArea,,{40, 3},{350,20})
   oState:options := XBPSTATIC_TYPE_TEXT
   oState:create()

   /*
    * create the wizard pages
    */
   oWiz1 := WizardDlg():New(,oDlg)
   oWiz2 := WizardDlg():New(,oDlg)
   oWiz3 := WizardDlg():New(,oDlg)
   oWiz4 := WizardDlg():New(,oDlg)
   oWiz5 := WizardDlg():New(,oDlg)
   oWiz6 := WizardDlg():New(,oDlg)

   oWiz1:setParams(10,, oWiz2, "Connect to source", oSource:cConnStr,,;
                   {|c| GetConnection(oParent, oSource, c)})

   oWiz1:title := "ODBCDBE Data Export Wizard"
   oWiz1:icon := 1
   oWiz1:Create()

   oWiz2:setParams(10,oWiz1,oWiz3, "Select source table", oSource:cName,;
                   {|c| GetTable(oWiz2, oSource, @c)},;
                   {|c| FinishGetTable(oSource, c,, oWiz2)})
   oWiz2:title := oWiz1:title
   oWiz2:icon := 1
   oWiz2:Create()

   oWiz3:setParams(11,oWiz2,oWiz4, "Connect to target",oTarget:cConnStr,,;
                   {|c| GetConnection(oParent, oTarget, c)})
   oWiz3:title := oWiz1:title
   oWiz3:icon := 1
   oWiz3:Create()

   oWiz4:setParams(11,oWiz3,oWiz5, "Select target table",oTarget:cName,;
                   {|c| GetTable(oWiz4, oTarget, @c)},;
                   {|c| FinishGetTable(oTarget, c, oSource, oWiz4)})
   oWiz4:title := oWiz1:title
   oWiz4:icon := 1
   oWiz4:Create()

   oWiz5:setParams(12,oWiz4,oWiz6, "Start transfer",,,;
                   {|| Transfer(oSource:cAlias, oTarget:cAlias, oState, oProgr)})
   oWiz5:title := oWiz1:title
   oWiz5:icon := 1
   oWiz5:Create()

   oWiz6:setParams(12,oWiz5,, "View Result",,,;
                   {||ViewResult(oParent, oTarget)})
   oWiz6:title := oWiz1:title
   oWiz6:icon := 1
   oWiz6:Create()

   oWiz1:show()

   DO WHILE nEvent <> xbeP_Close
      nEvent := AppEvent( @mp1, @mp2, @oXbp )
      oXbp:handleEvent( nEvent, mp1, mp2 )
   ENDDO

   DbCloseAll()
   DisconnectAll()
RETURN

/*
 * class that holds the required data of one 
 * data set
 */
CLASS OdbcData
EXPORTED:
    VAR oSession
    VAR cConnStr
    VAR cName
    VAR cAlias
    VAR lDontDrop
    INLINE METHOD init(cConnStr, cTable, lDontDrop)
        ::cConnStr := cConnStr
        ::cName := cTable
        ::lDontDrop := lDontDrop
    RETURN self
ENDCLASS

/*
 * Try to connect to given data source, if connect failed,
 * pop up the ODBC dialog box to select other data sources.
 */
FUNCTION GetConnection(oParent, oData, cDsn)

  IF oData:oSession != NIL .AND. oData:oSession:isConnected()
      oData:oSession:disconnect()
  ENDIF
  oData:cConnStr := cDsn

  /*
   * If the driver needs more information, it can popup a dialog.
   */
  DbeInfo(COMPONENT_DICTIONARY, ODBCDBE_WIN_HANDLE, oParent:getHwnd())
  DbeInfo(COMPONENT_DICTIONARY, ODBCDBE_PROMPT_MODE,ODBC_PROMPT_COMPLETE)

  oData:oSession := dacSession():new(oData:cConnStr)
  IF !oData:oSession:isConnected()
      /*
       * Connection failed, let the user choose a DSN.
       */
      DO WHILE .T.
          oData:oSession := dacSession():new("DBE=ODBCDBE")
          IF oData:oSession:isConnected()
             EXIT
          ENDIF
          IF ConfirmBox(oParent, "Connection failed, retry?", "ODBC Sample",;
                        XBPMB_YESNO, XBPMB_CRITICAL) == XBPMB_RET_NO
             RETURN .F.
          ENDIF
      ENDDO
  ENDIF

  oState:setCaption("Connected to " + oData:oSession:setProperty(ODBCSSN_DBMS_NAME))
  /*
   * Excel needs this
   */
  IF oData:lDontDrop
     oData:oSession:setProperty(ODBCSSN_DROP_BEFORE_CREATE, .F.)
  ENDIF
  
RETURN .T.

/*
 * Read available table names and provide a selection
 */
FUNCTION GetTable(oParent, oData, cOut)
   LOCAL cCatalog, aTables
   LOCAL bErr := ErrorBlock({|e| Break(e)})
   LOCAL nEvent:=0, mp1:=0, mp2:=0, oDlg, oXbp
   LOCAL oBtnOk
   LOCAL nRetry

   oData:oSession:setDefault()
   aTables := {}

   /*
    * Query catalog for all tables of the database
    */
   cCatalog := oData:oSession:setProperty(ODBCSSN_CURRENT_DATABASE)
   cCatalog := "@" + cCatalog + ".TABLES"   
   nRetry := 2
   DO WHILE nRetry > 0
      BEGIN SEQUENCE
        USE (cCatalog) NEW
        DbEval({|| AAdd( aTables, Field->Table_name)})
        dbCloseArea()
        nRetry := 0
      END SEQUENCE
      ErrorBlock(bErr)
      nRetry--
      IF Empty(aTables)
         cCatalog := "@TABLES"
      ENDIF
   ENDDO
   IF Empty(aTables)
        MsgBox("No tables available or catalog query not supported", "Error")
        RETURN .F.
   ENDIF
   /* Modal dialog with list box 
    */
   oDlg:= GuiStdDialog( "Select table" , {300,300},,oParent)
   oDlg:setModalState(XBP_DISP_APPMODAL)

   /*
    * Fill listbox with table names
    */
   oXbp := XbpListbox():new(oDlg:drawingArea)
   oXbp:create( ,, {10,50}, {270, 220} )
   AEval( aTables, {|e,i| oXbp:addItem(e)})
   oXbp:itemMarked := {|mp1, mp2, obj| oData:cName := aTables[oXbp:getData()[1]]}
   oXbp:ItemSelected := {|mp1, mp2, obj| oData:cName := aTables[oXbp:getData()[1]],;
                         PostAppEvent( xbeP_Close,,, oDlg )}

   oBtnOk := XbpPushButton():new( oDlg:drawingArea, , {81,12}, {99,24} )
   oBtnOk:caption := "Ok"
   oBtnOk:tabStop := .T.
   oBtnOk:preSelect := .T.
   oBtnOk:create()
   oBtnOk:activate := {|| PostAppEvent( xbeP_Close,,, oDlg ) }

   oDlg:keyBoard := {| nKeyCode, uNIL, self | IIF(xbeK_ENTER==nKeyCode, ;
                       PostAppEvent(xbeP_Activate,,,::oBtnOk),) }

   oDlg:show()
   SetAppFocus(oXbp)

   nEvent := xbe_None
   DO WHILE nEvent <> xbeP_Close
      nEvent := AppEvent( @mp1, @mp2, @oXbp )
      oXbp:handleEvent( nEvent, mp1, mp2 )
   ENDDO
   oDlg:hide()
   oDlg:destroy()
   SetAppFocus(oParent)
   cOut := oData:cName
RETURN !empty(oData:cName)

/*
 * Table was selected by user; adjust structure of
 * target table, then open table
 */
FUNCTION FinishGetTable(oData, cInput, oSource, oOwner)
LOCAL aNewStru := {}
LOCAL nError := 0
LOCAL oError
LOCAL cMsg := ""
LOCAL bErr
  
  bErr := ErrorBlock({|e| Break(e)})
  oData:oSession:setDefault()
  oData:cName := cInput
  BEGIN SEQUENCE
    IF oSource != NIL
       /* 
        * embed table name into quotes if 
        * name contains special chars 
        */
       IF ":" $ cInput .OR. "$" $ cInput
          cInput := '"' + cInput + '"'
       ENDIF
       AdjustStructure( oSource, oData, @aNewStru)
       IF ModiStruDlg(oOwner, aNewStru) != 1
          nError := 1
          BREAK
       ENDIF
       BEGIN SEQUENCE
           DbCreate(cInput, aNewStru)
       RECOVER USING oError
          /* 
           * ignore create errors but show warning
           */
          cMsg := oData:oSession:getLastMessage()
          IF empty(cMsg) .AND. oError != NIL
             cMsg := oError:description   
          ENDIF
          cMsg += CHR(13)+CHR(10)+"If this was not expected, modify the table name or"
          cMsg += CHR(13)+CHR(10)+"the structure of the new table"
          MsgBox(cInput+":"+ cMsg, "Warning")
       END SEQUENCE
    ENDIF
    USE (cInput) NEW
    oData:cAlias := ALIAS()
  RECOVER USING oError
    ErrorBlock(bErr)
    IF nError = 0
        nError := 2
        cMsg := oData:oSession:getLastMessage()
        IF empty(cMsg) .AND. oError != NIL
           cMsg := oError:description   
        ENDIF
    ENDIF
  END SEQUENCE
  ErrorBlock(bErr)
  IF nError = 2
     MsgBox("Error:"+ cInput+":"+cMsg, "Error")
  ENDIF
RETURN nError == 0

/*
 * Export the source table
 */
// this is how it works in Excel:
// CREATE TABLE "Tabelle1$A1:C1" (NAME VARCHAR, BORN DATE)
//  writes the names of the fields into the first cells: A1=NAME, B1=BORN
//  Note: you must always supply one extra cell, not A1:B1 but A1:C1 (??)

FUNCTION Transfer(cSrcAlias, cTrgAlias, oState, oProgress)
LOCAL nRecs := 0
LOCAL i, nFields, lRet := .T.  

    nFields := (cSrcALias)->(FCount()) 
    oState:setCaption("Records: " + var2char((cSrcALias)->(Reccount())) + ;
                    "  Fields: " + var2char(nFields))
    oProgress:setCaption(Var2Char(nRecs))
    DO WHILE !(cSrcALias)->(eof())
        BEGIN SEQUENCE
            (cTrgAlias)->(dbAppend())
            FOR i:=1 TO nFields
                (cTrgAlias)->(FieldPut(i, (cSrcAlias)->(FieldGet(i))))
            NEXT
            (cTrgAlias)->(dbSkip()) 
        RECOVER 
            lRet := .F.
        END SEQUENCE
        (cSrcALias)->(dbSkip())
        nRecs++
        oProgress:setCaption(StrZero(nRecs,6))
    ENDDO
    (cSrcAlias)->(dbCloseArea())
    BEGIN SEQUENCE
        (cTrgAlias)->(dbCloseArea())
    RECOVER 
        lRet := .F.
    END SEQUENCE

RETURN lRet

/*
 * To avoid type clashes on DbCreate, adjust types for the target.
 * Return: 
 *      "Y" - no changes
 *      "M" - successfully changed
 *      "?" - no match found
 */
FUNCTION AdjustStructure(oSource, oTarget, aNewStruct)
STATIC aCompatibleTypes := {"CMQW", "NIF", "VX"}

LOCAL cDts, i, j
LOCAL nPos
LOCAL cNewType, cType
LOCAL cRet

    cRet := "Y" 
    aNewStruct := AClone( (oSource:cAlias)->(DbStruct()) )
    cDts := oTarget:oSession:setProperty(ODBCSSN_DATATYPES)
    FOR i := 1 TO len(aNewStruct)
        cNewType := NIL
        cType := aNewStruct[i][2] 
        IF cType $ cDts
           LOOP
        ENDIF
        cRet := "M"
        /*
         * type not directly supported, try to find a similiar type
         */
         nPos := AScan(aCompatibleTypes, {|e,i| cType $ e})
         IF nPos > 0
            /*
             * now check if a type of the same group matches
             */
            FOR j:= 1 TO len(aCompatibleTypes[nPos])
                IF aCompatibleTypes[nPos][j] $ cDts
                    cNewType := aCompatibleTypes[nPos][j]
                    EXIT
                ENDIF
            NEXT 
         ENDIF
         IF cNewType != NIL
            aNewStruct[i][2] := cNewType
         ELSE
            cRet := "Y"
         ENDIF
    NEXT
RETURN cRet


/*
 * opens a browse
 */
FUNCTION ViewResult(oParent, oData)
LOCAL oQb
LOCAL aPos

  oData:oSession:setDefault()
  USE (oData:cName) NEW READONLY SHARED

  aPos := {5, 25}

  oQb := XbpQuickBrowse():new(oParent,,aPos,{380,370})
  oQb:dataLink := DacPagedDataStore():new()
  oQb:create()
  SetAppFocus( oQb )
RETURN .T.

/*
 * Create standard dialog
 */
FUNCTION GuiStdDialog( cTitle , aSize, oParent, oOwner)
   LOCAL oDlg, aPos

   aPos := CenterPos( aSize, AppDesktop():currentSize() )

   IF oParent = NIL
        oParent := AppDesktop()
   ENDIF
   oDlg          := XbpDialog():new(oParent ,oOwner, aPos, aSize,, .F. )
   oDlg:icon     := 1
   oDlg:taskList := .T.
   oDlg:maxButton := .F.
   oDlg:title    := cTitle
   oDlg:create()
   oDlg:drawingArea:setFontCompoundName( FONT_DEFPROP_SMALL )
   oDlg:show()
RETURN oDlg


FUNCTION CenterPos( aSize, aRefSize )
RETURN { Int( (aRefSize[1] - aSize[1]) / 2 ) ;
       , Int( (aRefSize[2] - aSize[2]) / 2 ) }

/*
 * ModiStruDlg: manually modify a table structure
 * RETURN: 1 - ok
 *         0 -  cancelled
 */
FUNCTION ModiStruDlg(oParent, aModiStruct)
   LOCAL nEvent, mp1:=0, mp2:=0
   LOCAL oDlg, oXbp:=NIL, oBtnOk, drawingArea
   LOCAL nRet := 0
   LOCAL nWidth, nYSize
   LOCAL aQSize, aQPos
   LOCAL aStruct := AClone(aModiStruct)

   nYSize := min(len(aStruct)*20+120, 500)
   oDlg := GuiStdDialog("Modify Structure",  {300,nYSize},,oParent)
   oDlg:setModalState(XBP_DISP_APPMODAL)
   drawingArea := oDlg:drawingArea

   oXbp := XbpQuickEditBrowse():New()
   oXbp:hScroll := .F.
   oXbp:tabStop := .T.
   oXbp:DataLink := DacPagedDataStore():New(aStruct)
   nWidth := drawingArea:CurrentSize()[1]-20
   aQSize := drawingArea:CurrentSize()
   aQSize[1] -= 20
   aQSize[2] -= 60
   aQPos  := drawingArea:CurrentPos()
   aQPOs[1] += 10
   aQPos[2] += drawingArea:CurrentSize()[2]-aQSize[2]-10
   oXbp:Create(drawingArea,,aQPos, aQSize)
  
   oXbp:setHeader({"Column", "Type", "Prec.","Scale"})
   nWidth -= 20
   oXbp:setColWidth( nWidth/2,1)
   oXbp:setColWidth( nWidth/6,2)
   oXbp:setColWidth( nWidth/6,3)
   oXbp:setColWidth( nWidth/6,4)
   oXbp:show()
   SetAppFocus(oXbp)

   oBtnOk := XbpPushButton():new( drawingArea, , {31,12}, {99,24} )
   oBtnOk:caption := "Ok"
   oBtnOk:tabStop := .T.
   oBtnOk:preSelect := .T.
   oBtnOk:create()
   oBtnOk:activate := {|| ACopy(aStruct, aModiStruct), nRet:=1, ;
                        PostAppEvent( xbeP_Close,,, oDlg ) }

   oXbp := XbpPushButton():new( drawingArea, , {156,12}, {96,24} )
   oXbp:caption := "Cancel"
   oXbp:tabStop := .T.
   oXbp:create()
   oXbp:activate := {|| PostAppEvent( xbeP_Close,,, oDlg ) }
   
   oDlg:keyBoard := {| nKeyCode, uNIL, self | IIF(xbeK_ENTER==nKeyCode, ;
                       PostAppEvent(xbeP_Activate,,,oBtnOk),) }
   oDlg:show()

   nEvent := xbe_None
   DO WHILE nEvent <> xbeP_Close
      nEvent := AppEvent( @mp1, @mp2, @oXbp )
      oXbp:handleEvent( nEvent, mp1, mp2 )
   ENDDO
   oDlg:hide()
   oDlg:destroy()
   SetAppFocus(oParent)
RETURN nRet

PROCEDURE AppSys
RETURN


/*
 * Load ODBCDBE as default dbe
 */
PROCEDURE DbeSys
   DbeLoad( "ODBCDBE" )
   DbeSetDefault( "ODBCDBE" )
RETURN

PROCEDURE DisconnectAll()
LOCAL i, aSSn
  /*
   * don't forget to disconnect!
   */
  aSSn := DacSession():sessionList()
  FOR i:= 1 TO len(aSSn)
     aSSn[i]:disconnect()
  NEXT
RETURN
