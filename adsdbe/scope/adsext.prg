//////////////////////////////////////////////////////////////////////
//
//  ADSEXT.PRG
//
//  Copyright:
//   Alaska Software, (c) 1999-2025. All rights reserved.         
//  
//  Contents:
//       This sample shows how to use the ADS client API. See the READ.ME file.
//   
//  Remarks:
//       The functions AX_SetScope() and AX_ClrScope() are wrapped. However,
//       a few more functions are implemented which are required for most of
//       the API functions.
//   
//////////////////////////////////////////////////////////////////////


/*------------------- INCLUDES ----------------------------*/
#include "ads.ch"
#include "dll.ch"
#include "dmlb.ch"
#include "error.ch"
#include "adsdbe.ch"

/*------------------- DEFINES  ----------------------------*/

/* NOTE:
 * These defines must match the define constants from ACE.H !
 *
 * ACE.H cannot be included here because it's a C-file
 */
#define ACE_SUCCESS      0
#define ADS_RAWKEY       1
#define ADS_STRINGKEY    2
#define ADS_DOUBLEKEY    4

#define AE_NO_SCOPE   5038

/*
 * Name of the ADS client DLL
 */
#define ADSDLL      "ACE32.DLL"

/*------------------- MODULE GLOBAL DATA -------------------*/

/*
 * store handles and DLL calls in STATICs for high performance
 */
STATIC snAdsDllHandle    := 0
STATIC scDllGetLastError := ""

/*------------------- INIT/EXIT PROC'S  -------------------*/

INIT PROCEDURE ADSEXTI()
    snAdsDllHandle    := DllLoad( ADSDLL )
    scDllGetLastError := DllPrepareCall( snAdsDllHandle, DLL_STDCALL, "AdsGetLastError"   )
RETURN


EXIT PROCEDURE ADSEXTE()
    DllUnload( snAdsDllHandle )
RETURN


/*------------------- LOCAL FUNCTIONS   -------------------*/

/*
 * check rc , raise an error if necessary
 */
STATIC FUNCTION AdsExtErrorCheck( rc, nIgnoreError )
   LOCAL oErr
   LOCAL nLen    := 0
   LOCAL cBuffer := Space(128)
   LOCAL cWord   := W2Bin(Len(cBuffer))   /* 16 bit buffer */

   IF nIgnoreError == NIL
      nIgnoreError := 0
   ENDIF

   IF rc != ACE_SUCCESS .AND. rc != nIgnoreError
      DllExecuteCall( scDllGetLastError, @rc, @cBuffer, @cWord )
      nLen               := Bin2W( cWord )
      cBuffer            := SubStr( cBuffer, 1, nLen )
      oErr               := Error():new()
      oErr:genCode       := XPP_ERR_NOTABLE
      oErr:description   := cBuffer
      oErr:subCode       := rc
      oErr:canDefault    := .F.
      oErr:canRetry      := .F.
      oErr:canSubstitute := .T.
      oErr:operation     := ProcName(2) + "(" + LTrim(Str(ProcLine(2))) + ")"
      oErr:subSystem     := ADSDLL
      RETURN Eval( ErrorBlock(), oErr )
   ENDIF
RETURN rc

/*
 * get table handle of current workarea
 */
STATIC FUNCTION AdsGetTableHandle()
RETURN(DbInfo(ADSDBO_TABLE_HANDLE))

/*
 * get index handle of leading index of the current workarea
 */
STATIC FUNCTION AdsGetIndexHandle()
RETURN(OrdInfo(ADSORD_INDEX_HANDLE))


/*
 * AX_ClrScope()
 * clear top or bottom scope
 */
FUNCTION AX_ClrScope( nScopeType )
   LOCAL rc, hIndex

   IF nScopeType == NIL
      RETURN .F.
   ENDIF

   /*
    * retrieve current index handle
    */
   hIndex := AdsGetIndexHandle()
   /* call ADS API
    */
   rc := DllCall( snAdsDllHandle, DLL_STDCALL, "AdsClearScope", hIndex, nScopeType )
   AdsExtErrorCheck( rc )

RETURN rc == ACE_SUCCESS


/*
 * AX_SetScope( [<nScopeType>] [, <xValue>] ) -> xValue
 * set scope
 */
FUNCTION AX_SetScope( nScopeType, xScope )
   LOCAL rc, hIndex
   LOCAL cLen, nLen, nType, cType
   LOCAL cBuffer /* decouple passed value (ANSI/OEM conv!) */

   /*
    * retrieve current index handle
    */
   hIndex := AdsGetIndexHandle()

   /*
    * no scope value: query current scope value
    */
   IF PCount() == 1
      cBuffer := space(128)
      cLen := W2Bin(len(cBuffer))
      rc := DllCall( snAdsDllHandle, DLL_STDCALL, "AdsGetScope", hIndex, ;
                     nScopeType, @cBuffer, @cLen )
      AdsExtErrorCheck( rc, AE_NO_SCOPE )

      cBuffer := SubStr( cBuffer, 1, Bin2W(cLen))

      RETURN cBuffer
   ENDIF

   cType := Valtype( xScope )

   /*
    * check type of scope value and
    * convert to character
    */
   DO CASE
   CASE cType == "C"
      nLen   := Len( xScope )
      nType  := ADS_STRINGKEY

      /* convert to ANSI if we are running OEM
       */
      IF Set( _SET_CHARSET ) == CHARSET_OEM
         cBuffer := ConvToAnsiCp( xScope )
      ELSE
         cBuffer := xScope
      ENDIF

   CASE cType == "D"
      nLen    := 8
      nType   := ADS_STRINGKEY
      cBuffer := DTOS( xScope )

   CASE cType == "L"
      nLen    := 1
      nType   := ADS_STRINGKEY
      cBuffer := IIF( xScope, "T", "F" )

   CASE cType == "N"
      cBuffer    := f2bin( xScope )
      nLen       := LEN( cBuffer )
      nType      := ADS_DOUBLEKEY

   OTHERWISE
       RETURN NIL
   ENDCASE

   rc := DllCall( snAdsDllHandle, DLL_STDCALL, "AdsSetScope", hIndex, ;
                  nScopeType, @cBuffer, nLen, nType )
   AdsExtErrorCheck( rc )

RETURN xScope
