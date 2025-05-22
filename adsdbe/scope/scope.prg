//////////////////////////////////////////////////////////////////////
//
//  SCOPE.PRG
//
//  Copyright:
//   Alaska Software, (c) 1999-2025. All rights reserved.         
//  
//  Contents:
//       This sample shows how to use the SCOPE functionality of ADS.
//   
//  Remarks:
//       Please copy the files CUSTOMER.DBF and CUSTOMER.DBT from the Xbase++
//       installation (..\source\samples\data) to your network drive.
//   
//       See READ.ME for a complete SCOPE description
//   
//////////////////////////////////////////////////////////////////////


#include "ads.ch"
#include "adsdbe.ch"
#include "adsext.ch"
#include "common.ch"


PROCEDURE MAIN( )
   LOCAL oSession, aFields

   // make sure this mapping and directory does exist
   LOCAL cServerDrive := "F:"
   LOCAL cDirOnServer4Tables := "\TestDir"

   CLS

   SET DEFAULT TO ( cServerDrive + cDirOnServer4Tables )

   // connect to the ADS server
   oSession := dacSession():New( "ADSDBE", cServerDrive )

   // check if we are connected to the ADS server
   IF .NOT. ( oSession:isConnected( ) )
      ? "Status : unable to connect to ADS" 
      ? "Error  :",oSession:getLastError()
      ? "Message:",oSession:getLastMessage()
      QUIT
   ENDIF

   ? "Status : connected to ADS ..."

   IF .NOT. File( "CUSTOMER.DBF" )
      ? "Error  : database file missing"
      ? "disconnnect ..."
      ? oSession:disconnect()
      QUIT
   ENDIF

   USE Customer NEW SHARED

   aFields := {}
   AEval( DbStruct(), {|a| IIf( a[2] $ "CDLN", ;
                                AAdd( aFields, a[1] ), ) } )

   INDEX ON LastName TAG Last  TO Last
   INDEX ON State    TAG State TO State
   SET INDEX TO Last, State

   OrdSetFocus( "Last")

   // set a TOP and a BOTTOM SCOPE
   ? AX_SetScope( ADS_SCOPE_TOP   , "F" )
   ? AX_SetScope( ADS_SCOPE_BOTTOM, "S" )
   @ 0,  0 CLEAR TO 0, 80
   @ 0,  0 SAY AX_SetScope( ADS_SCOPE_TOP    )
   @ 0, 40 SAY AX_SetScope( ADS_SCOPE_BOTTOM )
   dbGoTop()
   DbEdit(1,,,, aFields )

   // release the TOP SCOPE
   AX_ClrScope( ADS_SCOPE_TOP )
   dbGoTop()
   @ 0,  0 CLEAR TO 0, 80
   @ 0,  0 SAY AX_SetScope( ADS_SCOPE_TOP    )
   @ 0, 40 SAY AX_SetScope( ADS_SCOPE_BOTTOM )
   dbGoTop()
   DbEdit(1,,,, aFields )

   // change the TAG and the SCOPE
   ? OrdSetFocus( "State")
   ? AX_SetScope( ADS_SCOPE_TOP   , "CA" )
   ? AX_SetScope( ADS_SCOPE_BOTTOM, "CA" )
   @ 0,  0 CLEAR TO 0, 80
   @ 0,  0 SAY AX_SetScope( ADS_SCOPE_TOP    )
   @ 0, 40 SAY AX_SetScope( ADS_SCOPE_BOTTOM )
   dbGoTop()
   DbEdit(1,,,, aFields )

   USE
   CLS
   // disconnect also removes internal resources
   ? "disconnnect ..."
   ? oSession:disconnect()
RETURN


/*
 * overload the default DBESYS for your required DBE
 */
PROCEDURE DbeSys()

   /*
    *   Set the sorting order and the date format
    */
   SET COLLATION TO AMERICAN
   SET DATE TO AMERICAN

   /*
    * Load ADS database engine and register it as default DBE;
    * there is no need to load it "hidden", because we don't build
    * a compound DBE
    */
   IF ! DbeLoad( "ADSDBE", .F. )
      Alert( "Database Engine ADSDBE not loaded" , {"OK"} )
   ENDIF
   DbeSetDefault( "ADSDBE" )
RETURN

/*******
 * EOF */
