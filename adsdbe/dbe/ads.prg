//////////////////////////////////////////////////////////////////////
//
//  ADS.PRG
//
//  Copyright:
//         Alaska Software, (c) 1999-2025. All rights reserved.         
//  
//  Contents:
//         The sample demonstrates the usage of the ADS DBE.
//   
//  Remarks:
//         Please copy the files CUSTOMER.DBF and CUSTOMER.DBT from the Xbase++
//         installation (..\source\samples\data) to your network drive.
//   
//////////////////////////////////////////////////////////////////////

#include "adsdbe.ch"
#include "ads.ch"


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
  IF ( oSession:isConnected( ) )
   ? "Status:", oSession:isConnected()
   ?
   IF ! FILE( "CUSTOMER.DBF")
      ? "Status: file missing"
      ? "disconnnect from ADS ... "
      oSession:disConnect()
      QUIT
   ENDIF

   DbeInfo( COMPONENT_DATA, ADSDBE_TBL_MODE, ADSDBE_NTX)
   ? DbeInfo( COMPONENT_DATA, ADSDBE_TBL_MODE)
   DbeInfo( COMPONENT_ORDER, ADSDBE_INDEX_EXT, "NTX")
   ? DbeInfo( COMPONENT_ORDER, ADSDBE_INDEX_EXT)

   USE Customer NEW SHARED

   INDEX ON LastName TO CustA
   SET INDEX TO CustA

   dbGoTop()
   aFields := {}
   AEval( DbStruct(), {|a| IIf( a[2] $ "CDLN", ;
                                AAdd( aFields, a[1] ), ) } )
   DbEdit(,,,, aFields )

   USE

   // disconnect from the ADS
   ? "disconnnect from ADS ... "
   ? oSession:disconnect()
  ELSE
   ? "Error:", oSession:getLastError()
  ENDIF

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
  IF ! DbeLoad( "ADSDBE", .F.)
     Alert( "Database engine ADSDBE not loaded" , {"OK"} )
  ENDIF
  DbeSetDefault("ADSDBE")
RETURN

/*******
 * EOF */
