//////////////////////////////////////////////////////////////////////
//
//  CRYPT.PRG
//
//  Copyright:
//       Alaska Software, (c) 2002-2025. All rights reserved.         
//  
//  Contents:
//       Crypting of table data
//  Remarks:
//       The sample demonstrates how to encrypt data of a table, and
//       how to operate on crypted data. A generated key is used. The
//       key is not stored anywhere, that means, there is no key
//       management built into the sample.
//////////////////////////////////////////////////////////////////////

#include "dbstruct.ch"
#include "dmlb.ch"
#include "inkey.ch"
#include "foxdbe.ch"
#include "crypt.ch"

MEMVAR getList, cField

PROCEDURE MAIN( )
LOCAL oKey
LOCAL oCryptedDbf
LOCAL aTarStruct
LOCAL i
LOCAL cTable := "customer"
  
  ? "Create a crypted version of "+cTable
  CLS
  /* 
   * generate a key for encryption
   */
  oKey := SecureKey():generate()
  /* 
   * create a CryptTable()-instance
   */
  oCryptedDbf := CryptTable():new(,oKey)
  
  /* 
   * Open the source table
   */
  SET DEFAULT TO "..\..\data\misc"
  USE (cTable) NEW EXCLUSIVE ALIAS open
  open->(EditDbf(1))

  /* 
   * Adapt structure for target table for encryption
   */
  aTarStruct := open->(oCryptedDbf:setupStruct(,"FOXCDX"))

  /* 
   * Create crypted table in current directory using FOXDBE
   */
  SET DEFAULT TO
  DbeSetDefault("FOXCDX")
  DbCreate("crypted", aTarStruct)

  USE crypted NEW EXCLUSIVE ALIAS secure
  /* 
   * enable automatic encrypting/decrypting 
   */
  secure->(oCryptedDbf:enable())

  /* 
   * export data 
   */
  DO WHILE !open->(eof())
     secure->(DbAppend())
     FOR i:= 1 TO open->(Fcount())
        secure->(FieldPut(oCryptedDbf:fieldOffset+i, open->(FieldGet(i))))
     NEXT
     open->(dbskip()) 
  ENDDO
  secure->(oCryptedDbf:disable())
  CLOSE ALL

  @ 0, 0 SAY "Browse crypted version of "+cTable+", press ESC to exit"
  /* 
   * Warning: Editing undecrypted data will result in data loss.
   *          Any attempt to read memo-fields will fail with a read error.
   */
  USE crypted NEW EXCLUSIVE ALIAS secure
  BROWSE()

  /* 
   * enable automatic encrypting/decrypting 
   */
  secure->(oCryptedDbf:enable())
  /* 
   * Creating an index on an encrypted table must be done with crypting enabled.
   * Currently, the index file itself is not encrypted.
   */
  cField := aTarStruct[oCryptedDbf:fieldOffset+3][DBS_NAME]
  INDEX ON &cField TO (cTable+"A")
  secure->(EditDbf(oCryptedDbf:fieldOffset+1))
  secure->(oCryptedDbf:disable())
  CLOSE
RETURN

PROCEDURE EditDbf(nStartField)
LOCAL i
LOCAL cTable := DbInfo( DBO_FILENAME )
  CLS
  IF IsCrypted()
     @ 1, 2 SAY "Edit crypted "+cTable
  ELSE
     @ 1, 2 SAY "Edit uncrypted "+cTable
  ENDIF
  @ 2, 2 SAY "F4: Append  F5:Toggle delete      PgDn: Next Record  PgUp: Prev. Record"
  @ 3, 2 SAY "ESC: leave screen"
  CLEAR GETS
  dbgotop()
  SetKey( K_F4, {|| DBAPPEND(), DispRefresh(GetList) } )
  SetKey( K_F5, {|| IF(!Deleted(), DBDELETE(),;
                      dbRecall()), DispRefresh(GetList) } )
  FOR i:= nStartField TO Fcount()
     cField := FieldName(i)
     @ i+5, 2  SAY cField+": "
     @ i+5, 15 GET &cField
  NEXT

  DO WHILE .T.
     DispRefresh(GetList)
     READ SAVE
     IF K_ESC == LastKey() 
        EXIT
     ENDIF
     IF K_PGDN==LastKey()
        dbSkip()
     ELSEIF K_PGUP==LastKey()
        dbSkip(-1)
     ENDIF
  ENDDO
  SetKey( K_F4 )
  SetKey( K_F5 )
RETURN

/*
 * Refresh all current Gets in GetList-array.
 */
PROCEDURE DispRefresh( GetList )
   LOCAL i, imax 
   LOCAL oGet, nRow := Row(), nCol := Col()

   SET CURSOR OFF

   IF getList != NIL
      imax := Len(GetList)
      FOR i:=1 TO imax
         oGet := GetList[i]
         oGet:updateBuffer()
      NEXT
   ENDIF
   IF deleted()
      @ 5, 2 SAY "Deleted"
   ELSE
      @ 5, 2 SAY "       "
   ENDIF
   SET CURSOR ON
   SetPos( nRow, nCol )

RETURN

#define MSG_DBE_NOT_LOADED   " database engine not loaded"
#define MSG_DBE_NOT_CREATED  " database engine could not be created"

*******************************************************************************
* DbeSys() is always executed at program startup
*******************************************************************************
PROCEDURE dbeSys()
LOCAL aDbes := { { "DBFDBE", .T.},;
                 { "NTXDBE", .T.},;
                 { "FOXDBE", .T.},;
                 { "CDXDBE", .T.},;
                 { "DELDBE", .F.},;
                 { "SDFDBE", .F.} }
LOCAL aBuild :={ { "DBFNTX", 1, 2 } , { "FOXCDX", 3, 4} } 
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
  DbeSetDefault("DBFNTX")                 
RETURN
