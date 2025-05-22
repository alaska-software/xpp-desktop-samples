//////////////////////////////////////////////////////////////////////
//
//  SDF.PRG
//
//  Copyright:
//       Alaska Software, (c) 1997-2025. All rights reserved.         
//  
//  Contents:
//       For a better understanding of the concepts and the possibilities
//       of the Xbase++ database engines (DBE) this example uses the
//       SDF-DBE to display data from an ASCII file in normal and
//       indexed order.
//   
//       In addition, the standard behaviour of the SDF-DBE is changed
//       by setting specific attributes using the function DbeInfo().
//   
//   Notes:
//       This example assumes the file TEST.TXT will be retrieved from a
//       host system. The file is formatted according to the system
//       data format specification, meaning that it has a fixed
//       record length and one line in the ASCII file is one record.
//   
//////////////////////////////////////////////////////////////////////


#include "dmlb.ch"
#include "sdfdbe.ch"

#define dSDF_FILE    "TEST.SDF"
#define dSDF_DATA    "TEST.TXT"

PROCEDURE MAIN()
   LOCAL cStructData

   /*
    * register the SDF database engine as the default DBE
    */
   DbeSetDefault( "SDFDBE" )

   CLS

   /*
    * Note:
    *   To describe the data file the SDF DBE supports a
    *   'structure extended file' which is similar
    *   to the Windows INI file format.
    *
    * Display structure extended file for SDF format
    */
   IF File( dSDF_FILE )
      cStructData := MemoRead( dSDF_FILE )
      ? "Structure of the TXT file:"
      ? cStructData
      WAIT "Press a key to Browse() the TXT file ..."
   ELSE
      ALERT( "Structure extended file not found", {"OK"} )
   ENDIF

   /*
    * The imported file retrieved from the host has the following
    * specification:
    *
    * - Logical fields are filled with Y or N.
    * - There is a decimal comma instead of a decimal point.
    *
    * The default settings of the SDF DBE are according to the
    * Xbase-Specs as follows:
    *
    * - Logical value is T or F
    * - Decimal point is used "."
    *
    * To view the host data we simply change two attributes of the SDF DBE.
    * The defines are found in DMLB.CH and SDFDBE.CH respectively.
    */
   DbeInfo( COMPONENT_DATA , SDFDBE_LOGICAL_TOKEN ,  "YN" )
   DbeInfo( COMPONENT_DATA , SDFDBE_DECIMAL_TOKEN ,  "," )

   CLS

   @ 0,0 SAY "Browse() the imported TXT file, Esc to next browse"
   /*
    * Open and browse the imported file:
    */
   USE TEST
   BROWSE()
   USE

   /*
    * Build a compound DBE using DbeBuild() where DATA component is taken from DBFDBE
    * and ORDER component from NTXDBE
    *
    * NOTE: A compound DBE created by DbeBuild() automatically
    *       becomes the default DBE. This is not the case when a DBE
    *       is loaded by DbeLoad().
    *
    */
   DbeBuild( "SDFNTX" , "SDFDBE" , "NTXDBE" )
   CLS

   /*
    * Create an index for the SDF file and display the data
    *
    * NOTE: A compound DBE has all attributes of its component DBEs
    *
    */
   USE TEST
   INDEX ON NAME1 TO SDFNAME
   SET INDEX TO SDFNAME
   GO TOP

   @ 0,0 CLEAR TO 1,79
   @ 0,0 SAY "Browse() the indexed TXT file, Esc to quit"
   WAIT

   BROWSE()
   USE

   CLS
RETURN


/*******
 * EOF */
