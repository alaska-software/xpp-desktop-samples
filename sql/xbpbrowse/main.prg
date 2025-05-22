//////////////////////////////////////////////////////////////////////
//
//  MAIN.PRG
//
//  Copyright:
//      Alaska Software, (c) 2013-2025. All rights reserved.         
//  
//  Contents:
//      Show usage of XbpBrowse in the context of dynamic SQL pass-through 
//      statements.
//   
//////////////////////////////////////////////////////////////////////

#include "dac.ch"
#pragma library("dbfupsize.lib")

PROCEDURE Main
  LOCAL oSession, oForm
  LOCAL aQueries
  LOCAL cConnStr
  LOCAL cErrMsg := ""

  SET CHARSET TO ANSI

  // aQueries holds query descriptions and the associated SELECT statement
  // - the description goes into the combo box for selection by the user
  // - the SELECT is executed and the result set is displayed in an XbpBrowse.
  //
  aQueries := {;
               { "All customers",;
                 "SELECT * FROM customers ORDER BY customerid", .T., "customers" },;
               { "Countries by customer count",;
                 "SELECT Country,Count(*) AS Total FROM customers GROUP BY country ORDER BY Total DESC", .F., NIL },;
               { "Customers by categories",;
                 "SELECT ContactTitle,Count(*) AS Count FROM customers GROUP BY ContactTitle ORDER BY Count DESC", .F., NIL},;
               { "Marketing managers only",;
                 "SELECT * FROM customers WHERE ContactTitle='Marketing Manager' ORDER BY customerid", .T., "customers" } }

  // Create the form
  oForm := SqlForm():New( AppDesktop(), aQueries )

  // Assemble connection string.
  cConnStr := "DBE=pgdbe;"
  cConnStr += "UID=postgres;"
  cConnStr += "PWD=postgres;"
  cConnStr += "SERVER=localhost;"
  cConnStr += "DB=xppsamples;"

  // Verify that the database specified in the connection string and the table
  // "customers" exist on server. If not, create database and/or table.
  //
  IF(!CheckDataExists(cConnStr, "customers", @cErrMsg))
    MsgBox("Unable to create database or upsize table", cErrMsg)
  ELSE

    // Establish connection and show form modal.
    oSession := DacSession():new(cConnStr)
    oForm:showModal()
    oSession:disconnect()
  ENDIF

RETURN


PROCEDURE AppSys
RETURN


PROCEDURE DbeSys
  DbeLoad("PGDBE")
RETURN
