//////////////////////////////////////////////////////////////////////
//
//  XDLL1.PRG
//
//  Copyright:
//       Alaska Software, (c) 1997-2025. All rights reserved.         
//  
//  Contents:
//       Example for a Xbase++ DLL
//   
//  Remarks:
//       This file must be compiled with the /dll switch in order to have it
//       linked to a DLL
//   
//////////////////////////////////////////////////////////////////////


/*
 * Each program wich uses this DLL receives its own instances of STATIC variables.
 */ 

STATIC xdll1Static := 42

/*
 * Init and exit procedures are executed for any program that uses the DLL.
 */
INIT PROCEDURE xdll1Init
   ? "xdll1       :", PROCNAME()
   ? "xdll1Static :", xdll1Static
RETURN

EXIT PROCEDURE xdll1Exit
   ? "xdll1       :", PROCNAME()
   ? "xdll1Static :", xdll1Static
RETURN

/*
 * Exported function for DLL
 */
PROCEDURE fun1( cCallingFunc, xValue )

  ? "xdll1 :", PROCNAME(), PCOUNT(), cCallingFunc
  ? "xdll1Static :", xdll1Static := xValue

RETURN

CLASS Action1
EXPORTED:
      METHOD init
      INLINE METHOD print()
             ? "Action1:print"
      RETURN self
ENDCLASS

METHOD Action1:init()
        ? "Action1:init"
RETURN self







