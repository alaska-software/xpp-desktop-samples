
1. How To Build C - API Samples
--------------------------------

Please refer to our C-API documentation to see how to use
the Xbase++ C-API.

The makefiles we provide are ready-to-use with 

Win32: MicrosoftVisual-C++ Version 4.0. 
OS/2:  IBM VisualAge for C++ 3.0

To build the project enter 

       make -f capi.mak

on the command line. Make sure that the directories of 
your C-compiler are correctly listed in the PATH, LIB 
and INCLUDE environment variables. 


2. Important Notes for Xbase++ 1.2 for OS/2 
--------------------------------------------

The following changes of Xbase++ from 1.02.139 to 1.20.180
require you to adapt your C-API source code :

1. After the name transition of Xbase++ you should replace 
   all defines and type names starting with XB2_ or Xb2 with 
   XPP_ or Xpp, respectively. This could also be done by adding 
   #defines to your program like
     
       #define Xb2ParamList XppParamList
     
   Please refer to ..\INCLUDE\xb2.ch for a sample.

2. We also changed the calling convention XPPENTRY from 

      _Optlink  

        to

      __cdecl,

   so most C-compiler won't have a problem to use the C-API.
   Therefore, please make sure you always use the XPPENTRY 
   define in your source code. Also, you must recompile every 
   C-API module you want to use in conjunction with 
   Xbase++ 1.20.180.



