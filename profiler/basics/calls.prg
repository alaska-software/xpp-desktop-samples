//////////////////////////////////////////////////////////////////////
//
//  CALLS.PRG
//
//  Copyright:
//      Alaska Software, (c) 2001-2025. All rights reserved.         
//  
//  Contents:
//      How to interprete the Call/time statistic output
//   
//      The output of the XppPat->Call/Time statistic should look like:
//      (Right-Click on the Call/Time statistic, then select "Copy").
//   
//      CALL  NUMBERS TIMECLEAR   TIMEGROSS   TIMEMAX     TIMEMIN     TIMECALL    PERCCLEAR
//      A     1       0           703         0           0           0           0.00     
//      B     3       303         303         101         101         101         42.86    
//      C     2       400         602         200         200         200         56.58    
//      MAIN  1       0           703         0           0           0           0.00     
//      Sum   0       0           0           0           0           0           99.43    
//   
//      As you can see, function A() itself didn't consumed any time (TIMECLEAR). The time
//      A() has spent came due to other functions (TIMEGROSS). 
//   
//      Function B() has consumed 303 millisecs in total, and the average time 
//      (TIMECALL) is the same as the shortest amount of time it has spent (TIMEMIN) and
//      the same as the longest (TIMEMAX). B() has not spend any measurable time by
//      calling other functions (TIEMCLEAR == TIMEGROSS). It has been called 3 times.
//      (TIMECLEAR / NUMBERS => TIMECALL).
//      It has occupied almost 43 % of the runtime of the application.
//   
//      Function C() has been called 2 times, and consumed most of the runtime of the
//      application (PERCCLEAR): over 56%. Despite it has called other functions, 
//      most of the time was spent inside of C() (TIMECLEAR > TIMEGROSS-TIMECLEAR).
//      The TIMEGROSS value contains the 2 calls to Sleep(20) == 400 milliseconds
//      plus the two calls to function B() 100 milliseconds each => ca. 600 msecs.
//   
//  Remarks:
//   
//      Please use XppProf.exe to run this program
//      Use the following settings:      
//        Operation-Mode: Trace on
//        Built-In functions: suppress
//        Size for Symbols: 10
//        Timeline: checked
//   
//////////////////////////////////////////////////////////////////////


PROCEDURE Main()
    ? AppName() + " Profiler sample - please use XppProf to run this sample."
    A()
    ? "Done."
RETURN

FUNCTION A()
    B()
    C()
    C()
RETURN NIL

FUNCTION B()
    Sleep(10)
RETURN NIL

FUNCTION C()
    Sleep(20)
    B()
RETURN NIL

