/******************************************************************************
 *
 *  F2BIN.C
 *
 *  Copyright:
 *    Alaska Software, (c) 1995-2002. All rights reserved.
 *  
 *  Contents :
 *    Example to create a C function which is callable from Xbase++
 *    which implements a conversion from float to binary 
 *
 *  Remarks :
 *    The functions F2BIN()/BIN2F() are now part of the runtime 
 *    library and are shown here only for illustrative purpose.
 *
 ******************************************************************************/

#ifdef __WINDOWS__
#include <windows.h>
#else
#include <os2def.h>
#endif


#include "xppdef.h" 
#include "xpppar.h" 
#include "xppcon.h" 

#ifdef _MSC_VER 
int _fltused; 
#endif 

#define MAXLEN 8 
/* this is the "little secret": 
 * treat the memory of a double  
 * like a string with 8 bytes (and the terminating 0) 
 */ 
typedef union { 
      double dbl; 
      char string[MAXLEN+1]; 
} F2BinBuffer; 
 
/* 
 * Create a binary string from a numeric 
 * 
 * F2BIN(nNumber) => c8Binary   
 */ 
XPPRET XPPENTRY F2BIN(XppParamList paramList) { 
   ContainerHandle chResult = _conNew(NULLCONTAINER); 
   
   F2BinBuffer valBuffer={0}; 
   
   if ( _partype( paramList, 1) & (XPP_NUMERIC | XPP_DOUBLE)) { 
      valBuffer.dbl = _parnd( paramList, 1); 
   } 
   else { 
      /* return a 0-filled buffer if any errors  */
      valBuffer.dbl = 0.0; 
   } 
   _conPutCL( chResult, valBuffer.string, MAXLEN );      
   _conReturn(paramList, chResult); 
   return; 
} 
 
/* 
 * reverse the binary string to a numeric 
 * 
 * BIN2F(c8Binary) => nNumber (0 if any parameter errors) 
 * 
 */ 
XPPRET XPPENTRY BIN2F(XppParamList paramList) { 
   F2BinBuffer valBuffer={0}; 
   
   if ( _partype( paramList, 1) & (XPP_CHARACTER | XPP_MEMO)) { 
      _parc( valBuffer.string, MAXLEN+1, paramList, 1); 
   } 
   _retnd( paramList, valBuffer.dbl); 
   return; 
   
} 
