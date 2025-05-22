//////////////////////////////////////////////////////////////////////
//
//  ACCESS.PRG
//
//  Copyright:
//      Alaska Software, (c) 2000-2025. All rights reserved.         
//  
//  Contents:
//      This sample shows how to make use data of a MS-Access database
//      using the ODBCDBE and XbpQuickBrowse.
//   
//////////////////////////////////////////////////////////////////////


#include "Appevent.ch"
#include "odbcdbe.ch"
#pragma library("XppUi2")

PROCEDURE MAIN(cDriver, cDatabase, cTable)
  LOCAL cConnect
 
  /*
   * You may optionally use a different DSN, since
   * most of the DSNs have localized names.
   */
  IF cDriver = NIL
      cDriver := "AccessTest"
  ENDIF

  /*
   * A sample MS-Access database is included. 
   */
  IF cDatabase = NIL
      cDatabase := ".\test.mdb"
  ENDIF

  IF cTable = NIL
      cTable := "Contacts"
  ENDIF
  
  /*
   * setting up the connection string
   */
  cConnect := "DBE=ODBCDBE;DSN=" + cDriver + ";DBQ=" + cDatabase

  BrowseOdbc(cConnect, cTable, {50,30})  

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

  nRows := Min(RecCount()*4, 25)
  nCols := Min(Fcount() * 25, 80)

  oCrt := XbpCrt():new(AppDesktop(),, aPos, nRows, nCols, cTitle)
  oCrt:create()
  oCrt:setInputFocus := {| uNIL1, uNIL2, self | SetAppFocus(oQb) }

  oQb := XbpQuickBrowse():new(oCrt,,, {oCrt:xSize,oCrt:ySize})
  oQb:dataLink := DacPagedDataStore():new()
  oQb:create()
  SetAppFocus( oQb )

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



