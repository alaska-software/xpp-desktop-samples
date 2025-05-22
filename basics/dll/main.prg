//////////////////////////////////////////////////////////////////////
//
//  MAIN.PRG
//
//  Copyright:
//       Alaska Software, (c) 1997-2025. All rights reserved.         
//  
//  Contents:
//       Example of a program that uses Xbase++ DLLs 
//       static binding and dynamic binding
//   
//////////////////////////////////////////////////////////////////////


INIT PROCEDURE modInit()
   ? PROCNAME()
RETURN

EXIT PROCEDURE modExit()
   ? PROCNAME()
RETURN

PROCEDURE main( cCallParameter )
LOCAL oAction, oTest

   ? "Test program to call Xbase++ function within DLL's."
   ? "You may pass additional parameters on the command line."
   ? ""

   /* 
    * statically linked DLL which has one function exported: fun1 
    */
   ? ""
   ? "1. Call a function of the static linked XDLL1"
   fun1( "FROM "+PROCNAME(), cCallParameter ) // in XDLL1.DLL
   ? ""
   ? "2. Call a function of the static linked XDLL1 via macro"
   &("fun1")( "FROM "+PROCNAME(), cCallParameter ) 

   /* 
    * dynamically linked DLL which has one function exported: fun2
    */

   ? ""
   ? "3. Call a function of the dynamic loaded XDLL2"
   DllLoad( "XDLL2" )
   &("fun2")( "FROM "+PROCNAME(), cCallParameter ) 
   DllUnload( "XDLL2" )

   ? ""
   ? "4. Call a function of the dynamic loaded XDLL2 with prefix"
   DllLoad( "XDLL2", "myPrefix" )
   /* 
    * To reach all previous declared functions after loading a 
    * DLL dynamically you may prefix all functions of that DLL.
    * To use prefixed functions, you must call them via macro.
    */
   &("myPrefixFun2")( "FROM "+PROCNAME(), cCallParameter ) 

   DllUnload( "XDLL2" )

   ? ""
   ? "5. Use a class of the static linked XDLL1"
   /* 
    * The creation of the object must be done via a macro call to the
    * new() method. All subsequent method calls can be done as usually.
    */
   oTest:=Action1():new()
   oTest:print()

   ? ""
   ? "6. Use a class of the dynamic loaded XDLL2"
   DllLoad( "XDLL2" )
   /* 
    * The creation of the object must be done via a macro call to the
    * class function. All subsequent method calls can be done as usually.
    */
   oAction := &("Action2")()
   oTest := oAction:new()
   oTest:print()
   DllUnload( "XDLL2" )


RETURN


