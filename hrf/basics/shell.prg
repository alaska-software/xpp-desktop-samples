//////////////////////////////////////////////////////////////////////
//
//  SHELL.PRG
//
//  Copyright:
//      Alaska Software, (c) 2000-2023. All rights reserved.         
//  
//  Contents:
//      Sample for HRF utility
//   
//  Remarks:
//      Util stuff for the HRF sample code
//   
//  Syntax:
//      SHELL
//   
//  Return:
//      n/a
//   
//////////////////////////////////////////////////////////////////////


 #include "Dll.ch"

 /* **********************************************************************
  * Use the shell function to display a document
  * */
 PROCEDURE ShellOpenFile( cFile )

#define SW_HIDE             0
#define SW_NORMAL           1
#define SW_SHOWMINIMIZED    2
#define SW_SHOWMAXIMIZED    3
#define SW_MAXIMIZE         3
#define SW_SHOWNOACTIVATE   4
#define SW_SHOW             5
#define SW_MINIMIZE         6
#define SW_SHOWMINNOACTIVE  7
#define SW_SHOWNA           8
#define SW_RESTORE          9
#define SW_SHOWDEFAULT      10

    DllCall( "SHELL32.DLL"  , DLL_STDCALL, ; 
             "ShellExecuteA", AppDesktop():GetHWND(), "open", cFile, ;
             NIL, CurDir(), SW_NORMAL ) 
 RETURN

// EOF
