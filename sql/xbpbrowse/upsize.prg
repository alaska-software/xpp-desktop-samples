//////////////////////////////////////////////////////////////////////
//
//  UPSIZE.PRG
//
//  Copyright:
//      Alaska Software, (c) 2013-2025. All rights reserved.         
//  
//  Contents:
//      Implements the helper function CheckDataExists() and the
//      Logger() class which implements the IUpsizeLogger interface.
//   
//////////////////////////////////////////////////////////////////////

CLASS Logger FROM IUpsizeLogger
  PROTECTED:
  VAR LbLog

  EXPORTED:
  METHOD Init()
  METHOD Finish()

  METHOD Output()
  METHOD Progress( nJobId, nPercent )

  METHOD Alert()
  METHOD Stage()
ENDCLASS

METHOD Logger:Init(oParent)
  oParent := oParent:DrawingArea
  ::lbLog := XbpListbox():New()
  ::lbLog:vertScroll := .T.
  ::lbLog:Create(oParent,oParent,{0,0},oParent:currentSize())
  ::lbLog:AddItem("### started")
RETURN(SELF)

METHOD Logger:Finish(lState)
  UNUSED(lState)
  ::lbLog:AddItem("### finished")
  MsgBox("Upsizing done successfully.")
  ::lbLog:Hide()
  ::lbLog:Destroy()
RETURN(SELF)

METHOD Logger:Output(cTxt)
  ::lbLog:AddItem(cTxt)
RETURN(SELF)

METHOD Logger:Progress( nJobId, nPercent )
  UNUSED(nJobId)
  UNUSED(nPercent)
RETURN(SELF)

METHOD Logger:Alert(cTxt)
  MsgBox(cTxt,"Upsizing Error")
RETURN

METHOD Logger:Stage(cTxt)
  ::lbLog:AddItem("### "+cTxt)
RETURN

/// <summary>
/// Retrieves the value from a given sequence of
/// tuples with the syntax "id=value;", based
/// on a given identifier.
/// </summary>
///
/// <returns>Value of cId</returns>
FUNCTION _GetValueFromConnectionTuple(cCS, cId)
  LOCAL nS,nE,cVal
  LOCAL cCpy := Lower(cCS)
  nS   := AT(Lower(cId)+"=",cCpy)
  nE   := AT(";",cCpy,nS)
  nS   += Len(cId)+1
  cVal := SubStr(cCS,nS,nE-nS)
RETURN(cVal)


/// <summary>
/// Checks if the database specified in the connection
/// string exits. If not, creates the database. Checks
/// also if cTbl exists. If not, cTbl is created using
/// the dbfupsizer.
/// </summary>
FUNCTION CheckDataExists( cConnStr, cTbl, cErrMsg )
  LOCAL cConnection, cDB
  LOCAL oS
  LOCAL nS,nE
  LOCAL oInfo, oStmt, oLogger

  cErrMsg := ""
  nS      := AT("DB=",cConnStr)
  nE      := AT(";",cConnStr,nS)

  // Remove database from connection specifier. This way, we can
  // connect to whatever is the default db and can use the
  // information schema to check if the database exists.
  //
  cConnection := SubStr(cConnStr,1,nS-1) + SubStr(cConnStr,nE+1)
  cDB         := _GetValueFromConnectionTuple(cConnStr,"DB")
  oS := DacSession():New(cConnection)
  IF(!oS:IsConnected())
    cErrMsg := "Error ("+oS:GetLastMessage()+") when connecting. Connection-String: "+cConnection
    RETURN .F.
  ENDIF

  // Check if database exists. If not, create it
  oInfo := DacSchema():New(oS)
  IF(!oInfo:isDatabase(cDB))
    oStmt := DacSQLStatement():FromChar([CREATE DATABASE "::dbname" WITH OWNER = "::owner" ENCODING = 'UTF8'])
    oStmt:DbName := cDB
    oStmt:Owner  := _GetValueFromConnectionTuple(cConnStr,"UID")
    oStmt:Build():Execute()
  ENDIF

  oS:Disconnect()

  // Re-connect to the server and the proper database
  oS := DacSession():New(cConnStr)
  IF(!oS:IsConnected())
    cErrMsg := "Error ("+oS:GetLastMessage()+") when connecting. Connection-String: "+cConnStr
    RETURN .F.
  ENDIF

  // Check if the table exists. If not, upsize it!
  IF(!Table(cTbl, oS))
    oLogger := Logger():New( XbpApplication():mainform )
    DbfUpsize( ".\northwind-customers.upsize", oLogger )
  ENDIF

  oS:Disconnect()

RETURN(.T.)
