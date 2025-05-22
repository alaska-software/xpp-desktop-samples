//////////////////////////////////////////////////////////////////////
//
//  SQLCMD.PRG
//
//  Copyright:
//      Alaska Software, (c) 2001-2025. All rights reserved.         
//  
//  Contents:
//      SQL command style usage
//      
//////////////////////////////////////////////////////////////////////


#include "sqlcmd.ch"

PROCEDURE main(cDsn)
LOCAL x
LOCAL cSqlCmd
MEMVAR oSession

    ? "Please choose the DSN of the Nortwhind sample database"

    CREATE CONNECTION INTO oSession DATASOURCE (cDsn) DIALOG 
    IF !oSession:isconnected()  
        Alert("Connection failed")
        QUIT
    ENDIF

    ? "Connected to:"
    ? oSession:setProperty(ODBCSSN_DBMS_NAME)
    ? "Running on:"
    ? oSession:setProperty(ODBCSSN_SERVER_NAME)

    SQL "SELECT * FROM customers" VIA oSession 
    LIST ALL
    CLOSE

    SQL "SELECT SUM(UnitPrice) FROM [Order Details] WHERE orderId = 10250" INTO x  
    ? "Total of order 10250:", x 

    SQL CMD "SET LANGUAGE 'Italiano'"
    SQL "SELECT @@LANGID AS 'Language ID'" INTO x
    ? "Language-Id:", x
   
    TEXT INTO cSqlCmd
        SELECT @@CONNECTIONS AS 'Login Attempts',
               GETDATE() AS 'Today''s Date and Time'
    ENDTEXT

    SQL (cSqlCmd) 
    LIST ALL
    CLOSE

    SQL CMD "UPDATE categories SET Description='unknown' WHERE Description IS NULL"

    EXECSP "sp_databases"
    LIST ALL
    CLOSE

    DELETE CONNECTION oSession
RETURN


/*
 * Load ODBCDBE as default dbe
 */
PROCEDURE DbeSys
   DbeLoad( "ODBCDBE" )
   DbeSetDefault( "ODBCDBE" )
RETURN

    
