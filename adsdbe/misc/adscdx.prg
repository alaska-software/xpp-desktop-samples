//////////////////////////////////////////////////////////////////////
//
//  ADSCDX.PRG
//
//  Copyright:
//         Alaska Software, (c) 1999-2025. All rights reserved.         
//  
//  Contents:
//         The sample demonstrates the usage of the ADS DBE for FTP/CDX files.
//   
//  Remarks:
//         Please copy the files PARTS.DBF and PARTS.FPT from the Xbase++
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
    IF ! FILE( "PARTS.DBF") .OR. ! FILE( "PARTS.FPT")
       ? "Status: files missing"
       ? "disconnnect from ADS ... "
       oSession:disConnect()
       QUIT
    ENDIF

    // use FPT/CDX compatible files
    DbeInfo( COMPONENT_DATA, ADSDBE_TBL_MODE, ADSDBE_CDX)
    ? DbeInfo( COMPONENT_DATA, ADSDBE_TBL_MODE)
    DbeInfo( COMPONENT_ORDER, ADSDBE_INDEX_EXT, "CDX")
    ? DbeInfo( COMPONENT_ORDER, ADSDBE_INDEX_EXT)

    USE Parts NEW SHARED

    IF ! FILE( "PARTS.CDX")
      INDEX ON PartName TAG PartName TO Parts
    ENDIF
    SET INDEX TO Parts

    OrdSetFocus( "PartName")

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
