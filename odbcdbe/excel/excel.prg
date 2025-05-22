//////////////////////////////////////////////////////////////////////
//
//  EXCEL.PRG
//
//  Copyright:
//      Alaska Software, (c) 2000-2025. All rights reserved.         
//  
//  Contents:
//      This sample shows how to browse data of a MS-Excel spreadsheet
//      using the ODBCDBE and XbpQuickBrowse.
//   
//////////////////////////////////////////////////////////////////////


#pragma library( "XppUi2.LIB" )
#pragma library( "ADAC20B.LIB" )
#include "Appevent.ch"
/*
 * Including odbcdbe.ch includes requests to ODBCUTIL.LIB.
 * Please see odbcdbe.ch for details.
 */
#include "odbcdbe.ch"

PROCEDURE MAIN(cDSN)
  LOCAL oSession
  LOCAL cConnString
  LOCAL cSheet
  LOCAL aThreads

  /*
   * You may optionally use a different DSN, since
   * most of the DSNs have localized names.
   */
  IF cDSN = NIL
      cDSN := "Excel Files"
  ENDIF
  /*
   * A sample Excel-spreadsheet is included. Please open this file
   * using MS-Excel to read the explanations.
   */
  cSheet := "sheet1.xls"
 
  /*
   * setting up the connection string
   */
  cConnString := "DBE=ODBCDBE;"+"DSN=" +cDSN + ";" + "DBQ=" + cSheet
  
  /*
   * create 3 threads, each shows a portion of the same 
   * spreadsheet
   */
  XbpCrt()
  aThreads := Array(3)
  aThreads[1] := Thread():new()
  aThreads[1]:start("BrowseOdbc", cConnString, "DATA", {50,30})
  aThreads[2] := Thread():new()
  aThreads[2]:start("BrowseOdbc", cConnString, "A29:C34", {450,100})
  aThreads[3] := Thread():new()
  aThreads[3]:start("BrowseOdbc", cConnString, "A19:B22", {50,400})

  /*
   * wait for termination of all browse-threads
   */
  ThreadWaitAll(aThreads)

RETURN

/*
 * The BrowseOdbc-function establishes a connection, opens
 * the table (the part of the spreadsheet, respectively) and
 * opens a browse.
 */
PROCEDURE BrowseOdbc(cConnString, cTable, aPos)
LOCAL oSession

  oSession := dacSession():new(cConnString)

  IF !oSession:isConnected()
     MsgBox( oSession:getLastMessage(), "Cannot connect to " + cConnString)
     RETURN
  ENDIF

  USE (cTable) NEW 
  QBrowse(cTable, aPos)
  dbCloseArea()

  /*
   * don't forget to disconnect!
   */
  oSession:disconnect()

RETURN


/*
 * The QBrowse-function sets up a new Crt-object where a
 * XbpQuickBrowse() will be shown.
 * You should avoid using other browsers than those based
 * on DacPagedDataStore() for workload reasons.
 */
PROCEDURE QBrowse(cTitle, aPos)
LOCAL oCrt, oXbp, oQb, nEvent, mp1, mp2
LOCAL nRows, nCols

  nRows := Min(RecCount()*2.5, 25)
  nCols := Min(Fcount() * 20, 80)

  oCrt := XbpCrt():new(AppDesktop(),, aPos, nRows, nCols, cTitle)
  oCrt:create()
  oCrt:setDisplayFocus := {|| SetAppFocus(oQb) }

  oQb := XbpQuickBrowse():new(oCrt,,, {oCrt:xSize,oCrt:ySize})
  oQb:dataLink := DacPagedDataStore():new()
  oQb:create()

  DO WHILE nEvent <> xbeP_Close 
      nEvent := AppEvent( @mp1, @mp2, @oXbp ) 
      oXbp:handleEvent( nEvent, mp1, mp2 ) 
  ENDDO   
  
  oQb:destroy()
  oCrt:destroy()
RETURN

/*
 * No implicit GUI reuqired
 */
PROCEDURE AppSys
RETURN

/*
 * Load ODBCDBE as default dbe
 */
PROCEDURE DbeSys
   DbeLoad( "ODBCDBE" )
   DbeSetDefault( "ODBCDBE" )
RETURN


