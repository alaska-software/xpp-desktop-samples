//////////////////////////////////////////////////////////////////////
//
//  XDLL2.PRG
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

STATIC xdll2Static := 24

/*
 * Init and exit procedures are executed for any program that uses the DLL.
 */
INIT PROCEDURE xdll2Init
   ? "xdll2       :", PROCNAME()
   ? "xdll2Static :", xdll2Static
RETURN

EXIT PROCEDURE xdll2Exit
   ? "xdll2       :", PROCNAME()
   ? "xdll2Static :", xdll2Static
RETURN

/*
 * Exported function for DLL
 */
PROCEDURE fun2( cCallingFunc, xValue )

  ? "xdll2 :", PROCNAME(), PCOUNT(), cCallingFunc
  ? "xdll2Static :", xdll2Static := xValue

RETURN

CLASS Action2
EXPORTED:
      METHOD init
      INLINE METHOD print()
             ? "Action2:print"
      RETURN self
ENDCLASS

METHOD Action2:init()
        ? "Action2:init"
RETURN self

   








