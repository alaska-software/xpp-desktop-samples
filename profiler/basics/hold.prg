//////////////////////////////////////////////////////////////////////
//
//  HOLD.PRG
//
//  Copyright:
//      Alaska Software, (c) 2001-2025. All rights reserved.         
//  
//  Contents:
//      How to use prfHold()/prfResume()
//   
//      These functions allow to trace specific parts of a
//      program only which reduces the amount of data to be analyzed.
//   
//      The output of tha analyzer will be different from the CALLS-sample.
//      There is no call to function A() traced.
//      There is only one call to function C() in the statistic, and
//      quite consequently only one call to B().
//      The calls to Array(), AFill(), ASort()  etc. will be traced
//      if you have configured "Built-In functions": include.
//   
//  Remarks:
//   
//      You may keep the prfHold()/prfResume() calls in the source
//      code even if not compiled /profile. The profile.ch takes care about
//      that.
//   
//      Please use XppProf.exe to run this program
//      Use the following settings:      
//        Operation-Mode: Trace on
//        Built-In functions: include
//        Size for Symbols: 20
//        Timeline: checked
//   
//////////////////////////////////////////////////////////////////////


// You must include "profiler.ch"
#include "profiler.ch"

PROCEDURE Main()
    // stop profiling as early as possible
    prfHold("MyHold")
    ? AppName() + " Profiler sample - please use XppProf to run this sample."
    A()
    ? "Done."
RETURN

FUNCTION A()
    B()
    // this call to C() is of interest
    prfResume("MyHold")
    C("specific")
    // stop profiling again
    prfHold("MyHold")
    C()
RETURN NIL

FUNCTION B()
    Sleep(10)
RETURN NIL

FUNCTION C(cMode)
LOCAL a
    IF cMode = NIL
       Sleep(20)
       B()
    ELSE
       a := Array(400)
       AFill(a, 1)
       ASort(a)
       B()
    ENDIF
RETURN NIL


