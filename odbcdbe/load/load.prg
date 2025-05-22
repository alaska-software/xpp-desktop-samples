//////////////////////////////////////////////////////////////////////
//
//  LOAD.PRG
//
//  Copyright:
//      Alaska Software, (c) 1997-2025. All rights reserved.         
//  
//  Contents:
//      This sample shows how to load local data into an ODBC data source.
//   
//  Remarks:
//      Please install the ODBC driver and the ODBC data source before running
//      the sample. You then pass then name of the DSN on the command line.
//   
//      You may try to export the customer.dbf to sheet1.xls located
//      in the excel-sample directory.
//      Syntax: load customer DBFDBE <your Excel Files DSN-name>
//   
//////////////////////////////////////////////////////////////////////


/*
 * Including odbcdbe.ch includes requests to ODBCUTIL.LIB.
 * Please see odbcdbe.ch for details.
 */
#include "odbcdbe.ch"
#include "common.ch"
#include "dll.ch"
#pragma library("odbcut10.lib")

STATIC aKeyWords

PROCEDURE MAIN(cTable, cVia, cDsn, cTargetName)

  DEFAULT cVia TO ""

  IF cDsn = NIL .OR. "ODBC" $ Upper(cVia)
     Help()
     QUIT
  ENDIF

  TblExport(cTable, cVia, cDSN, cTargetName)

RETURN

PROC Help
  ? "LOAD <SourceTable> <SourceDbe> <ODBC-target as DSN> [<TargetTableName>]"
  ? "  Export local data to an ODBC data source"
  ? "  Sample: "
  ? "  Load customer DBFNTX SQLDB1;UID=xx;PWD=xxx customer2"
RETURN


FUNCTION TblExport(cTable, cVia, cDSN, cTargetName)
LOCAL aStruct
LOCAL i

  oSession := dacSession():New("DBE=ODBCDBE;DSN="+cDSN)
  IF (!oSession:isConnected())
    ? "cannot connect to server !"
    QUIT
  ENDIF

  USE (cTable) ALIAS srctbl EXCLUSIVE VIA (cVia)
  aStruct := DbStruct()
  AEVAL(aStruct, {|e,i| Qout(e)})
  ? RecCount() , "records in source table."

  IF cTargetName = NIL
     cTargetName := cTable
  ENDIF
  cTargetName := MakeSqlTableName(cTargetName)
  aStruct := MakeSqlStruct(aStruct, oSession)
  DbCreate(cTargetName, aStruct, "ODBCDBE")
  USE (cTargetName) EXCLUSIVE VIA ODBCDBE ALIAS EXPORT NEW
  AEVAL(DbStruct(), {|e,i| Qout(e)})
  DO WHILE !srctbl->(eof())
     export->(DbAppend())
     FOR i:= 1 TO export->(Fcount())
         export->(FieldPut(i, srctbl->(FieldGet(i))))
     NEXT
     srctbl->(dbSkip())
  ENDDO
  export->(DbCommit())
  ? RecCount() , "records successfully exported."
  CLOSE srctbl
  CLOSE export

  oSession:disconnect()
RETURN .T.


FUNCTION MakeSqlTableName(cTable)
LOCAL c, i
LOCAL cNotAllowed := "\/()"

   i := Rat("\", cTable)
   IF i > 0
        cTable := Substr(cTable, i+1)
   ENDIF
   i := Rat("/", cTable)
   IF i > 0
        cTable := Substr(cTable, i+1)
   ENDIF
   i := Rat(".", cTable)
   IF i > 0
        cTable := Substr(cTable,1, i-1)
   ENDIF

   c := ""
   FOR i:=1 TO len(cTable)
        IF !cTable[i] $ cNotAllowed
            c += cTable[i]
        ENDIF
   NEXT
RETURN c


FUNCTION MakeSqlStruct(aStruct, oSession)
LOCAL a := AClone(aStruct)
   AEVAL(a, {|e,i| a[i][1] := MakeSqlFieldName(a[i][1]) })
   AEVAL(a, {|e,i| a[i] := MakeSqlFieldType(a[i], oSession) })
RETURN a


/*
 * Check if field name violates SQL syntax by using a keyword
 * This will be corrected by adding and underscore automatically.
 */
FUNCTION MakeSqlFieldName(cFieldName)
LOCAL c

   c := cFieldName
   IF aKeyWords = NIL
      aKeyWords := OdbcListSqlKeywords(oSession)
   ENDIF

   IF ASCAN(aKeyWords, cFieldName) > 0
      c += "_"
      ? "Field " + cFieldname + " changed to: " + c
   ENDIF
RETURN c

/*
 * check if source type is supported on target
 */
FUNCTION MakeSqlFieldType(aFldRec, oSession)
STATIC aAltTypes := { { "C", "MWX" },;
                      { "N", "FI"  },;
                      { "L", "NIF" },;
                      { "D", "C"   } }
LOCAL cTgTypes := oSession:setProperty(ODBCSSN_DATATYPES)
LOCAL aRec := AClone(aFldRec)
LOCAL nPos, i

   IF ! aFldRec[2] $ cTgTypes
      /*
       * search for alternative types
       */
      nPos := AScan(aAltTypes, {|e| e[1] == aFldRec[2]})
      IF nPos > 0
         /*
          * find a supported type
          */
         FOR i:= 1 TO len(aAltTypes[nPos][2])
             IF aAltTypes[nPos][2][i] $ cTgTypes
                aRec[2] := aAltTypes[nPos][2][i]
             ENDIF
         NEXT
      ENDIF
   ENDIF

* set memo field to 0 otherwise ODBCDBE assumes this is a var char
   IF (aFldRec[2] == "M") .OR. (aFldRec[2] == "V")
     aRec[3] = 0
   ENDIF
RETURN aRec


*******************************************************************************
* local DbeSys() is always executed at program startup
*******************************************************************************

#define MSG_DBE_NOT_LOADED   " database engine not loaded"
#define MSG_DBE_NOT_CREATED  " database engine could not be created"

PROCEDURE DbeSys()
/*
 *   The lHidden parameter is set to .T. for all database engines
 *   which will be combined to a new abstract database engine.
 */
LOCAL aDbes := { { "DBFDBE", .T.},;
                 { "NTXDBE", .F.},;
                 { "FOXDBE", .T.},;
                 { "CDXDBE", .F.},;
                 { "DELDBE", .F.},;
                 { "SDFDBE", .F.},;
                 { "ODBCDBE", .F.} }
LOCAL aBuild :={ { "DBFNTX", 1, 2 },;
                 { "FOXCDX", 3, 4 } }
LOCAL i

  /*
   *   Set the sorting order and the date format
   */
  SET COLLATION TO AMERICAN
  SET DATE TO AMERICAN

  /*
   *   load all database engines
   */
  FOR i:= 1 TO len(aDbes)
      IF ! DbeLoad( aDbes[i][1], aDbes[i][2])
         Alert( aDbes[i][1] + MSG_DBE_NOT_LOADED , {"OK"} )
      ENDIF
  NEXT i

  /*
   *   create database engines
   */
  FOR i:= 1 TO len(aBuild)
      IF ! DbeBuild( aBuild[i][1], aDbes[aBuild[i][2]][1], aDbes[aBuild[i][3]][1])
         Alert( aBuild[i][1] + MSG_DBE_NOT_CREATED , {"OK"} )
      ENDIF
  NEXT i

  DbeSetDefault("ODBCDBE")

RETURN

//
// EOF
