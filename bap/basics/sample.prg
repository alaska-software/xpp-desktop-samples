///////////////////////////////////////////////////////////////////////////////
//
//                             A  C  S  N
//
// +---------------  Alaska Certified Solutions Network  -------------------+
// |                                                                        |
// |        This file is proved and certified by Alaska Software            |
// |                                                                        |
// |                   No: <Certification number>                           |
// |                                                                        |
// |   For more information about ACSN read the appropriate announcement    |
// |      or scan for ACSN in the Alaska Support-LIBs on CompuServe or      |
// |                   at WWW.ALASKA-SOFTWARE.COM                           |
// |                                                                        |
// +------------------------------------------------------------------------+
//
// FILE NAME
//
//    SAMPLE.PRG
//
// AUTHOR
//
//    (c) Copyright 1998-2000, Gernot Trautmann
//
//    ALL RIGHTS RESERVED
//
//    This file is the property of AUTHOR. It participates in the
//    Alaska Certified Solutions Network program. Permission to use, 
//    copy, modify, and distribute this software for any purpose and 
//    without fee is hereby granted, provided that the above copyright 
//    notice appear in all copies and that the name of the author or
//    Alaska Software not be used in advertising or publicity pertaining 
//    to distribution of the software without specific, written prior 
//    permission. 
//
// WARRANTY
//
//    THE MATERIAL EMBODIED ON THIS SOFTWARE IS PROVIDED TO YOU "AS-IS"
//    AND WITHOUT WARRANTY OF ANY KIND, EXPRESS, IMPLIED OR OTHERWISE,
//    INCLUDING WITHOUT LIMITATION, ANY WARRANTY OF MERCHANTABILITY OR
//    FITNESS FOR A PARTICULAR PURPOSE.  IN NO EVENT SHALL THE AUTHOR
//    OR ALASKA SOFTWARE BE LIABLE TO YOU OR ANYONE ELSE FOR ANY DIRECT,
//    SPECIAL, INCIDENTAL, INDIRECT OR CONSEQUENTIAL DAMAGES OF ANY KIND, 
//    INCLUDING WITHOUT LIMITATION, LOSS OF PROFIT AND LOSS OF USE.
//
// DESCRIPTION
//
//    How to use the binary access package DLL
//
// REMARKS
//
//    This sample shows the principles of usage only.
//    To integrate the BAP.DLL into your project, add the BAP.LIB 
//    to your project file. Copy the BAP.DLL and the BAP.LIB
//    to the XPPW32\LIB directory.
//    See the read.me for a description of all functions.
//
// HISTORY
//
//    28.09.00 GT callback sample added
//    10.02.97 GT creation of file
//
///////////////////////////////////////////////////////////////////////////////
#include "bap.ch"
#include "dll.ch"


PROCEDURE GeneralSample
Local aBin, cSubject:=space(30), nCount :=0, cBin
/*
  Assume the following C-structure to create:

  typedef strcut {  
     CHAR name[20];
     CHAR *pSubject;
     LONG value;
     LONG *pCount;
  } TESTSTRUCT;

*/
    /* Initialize 4 members */
    aBin := BaInit(4)

    /* add a character by value, 
     * a character by reference,
     * a numeric by value ,
     * and a numeric by reference 
         */
    ? BaStruct( aBin, substr("Frankfurter Ebblewoiexpress",1,20))
    ? BaStruct( aBin, @cSubject)
    ? BaStruct( aBin, 6)
    ? BaStruct( aBin, @nCount)
    
    /* get the binary representation of the structure */
    cBin := BaAccess( aBin)
    /* 
     * call a function which can manipulate this structure 
     */
    // FUNC( [@]cBin)

    /* 
     * extract all members, reassign values
     * that were passed by reference
     */
    ?             BaExtract(aBin, cBin)
    ? cSubject := BaExtract(aBin)
    ?             BaExtract(aBin)
    ? nCount   := BaExtract(aBin)
RETURN

/*
 * Sample to demonstrate BaCallback() usage
 */ 
PROCEDURE main
LOCAL lRet, hModule
LOCAL nCallback, nFlags
LOCAL nCount
    /* This API is only supported under Windows NT */
    IF "NT" $ OS()
        nCallBack := BaCallback("EnumWinStations", BA_CB_GENERIC2)
        lRet := LogicOf( EnumWindowStationsA(nCallback, 0))
        ? lRet
        WAIT
    ENDIF        

    /* 
     * Sample to use a generic callback function with 1 
     * parameter
     */
    nCallBack := BaCallBack("EnumCodePages", BA_CB_GENERIC1)
    nFlags := 0
    lRet := LogicOf( EnumSystemCodePagesA( nCallback, nFlags))
    ? lRet
    WAIT

    /* 
     * Sample to use a generic callback function with 4
     * parameters, different type interpretations and
     * user passed parameter
     */
    nCallBack := BaCallBack("EnumResName", BA_CB_GENERIC4)
    hModule := GetModuleHandleA("XppNat.dll")
    PRIVATE aResources := { {}, {} }
    /* list all Bitmap resource IDs */
    ? LogicOf( EnumResourceNamesA(hModule, 2, nCallback, "aResources[1]"))
    /* list all Icon resource IDs */
    ? LogicOf( EnumResourceNamesA(hModule, 3, nCallback, "aResources[2]"))
    ? aResources
RETURN

FUNCTION EnumCodePages(nPar1)
    ? StringOf(nPar1)
RETURN 1

FUNCTION EnumWinStations(nPar1, nPar2)
    ? StringOf(nPar1), nPar2
RETURN 1

FUNCTION EnumResName(hModule, lpszType, lpszName, lParam)
    /* Note: despite of the type/name PARAMETERS are declared
     * as strings, they must be interpreted as numerics 
     * if it's a default resource types (see winuser.h)
     */
    /* Note: lParam can not carry references to arrays, objects
     * or strings. To access variables of the callee it is
     * simpler to pass the name of the variable
     */
    AAdd(&(StringOf(lParam)), { Int(lpszType) , Int(lpszName) } )
RETURN 1

DLLFUNCTION EnumSystemCodePagesA(nCallBack, nFlags) USING STDCALL FROM KERNEL32.DLL
DLLFUNCTION EnumWindowStationsA(nCallback, nUser) USING STDCALL FROM USER32.DLL
DLLFUNCTION EnumResourceNamesA(hModule, lpszType, nCallback, lParam) USING STDCALL FROM KERNEL32.DLL
DLLFUNCTION GetModuleHandleA(cMod) USING STDCALL FROM KERNEL32.DLL

