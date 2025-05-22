//////////////////////////////////////////////////////////////////////
//
//  ADSMGMT.PRG
//
//  Copyright:
//   Alaska Software, (c) 1999-2025. All rights reserved.         
//  
//  Contents:
//       Alaska Management Console for the Advantage Database Server
//   
//       Experimental user interface for the ADS Management API functions.
//   
//  Remarks:
//       REQUIREMENTS:
//        - ACE32.DLL
//        - AXCWS32.DLL
//        - ADSMG.LIB
//   
//////////////////////////////////////////////////////////////////////



#include "Gra.ch"
#include "Xbp.ch"
#include "Common.ch"
#include "Appevent.ch"
#include "Font.ch"
#include "AdsMg.ch"



// Overload default procedure DbeSys.
// (No database engines required.)
PROCEDURE DbeSys
RETURN 



// Overload default procedure AppSys. 
// (No crt window required.)
PROCEDURE AppSys
RETURN 



// MAIN PROCEDURE
PROCEDURE Main
   LOCAL nEvent, oXbp, mp1, mp2
   LOCAL MgForm := ManageForm():New():Create()   // Create main dialog window

   SetAppWindow(MgForm)
   SetAppFocus(MgForm)

   // Search network for available database servers.
   PostAppEvent( xbeP_Activate,,,MgForm:ScanButton )

   // Event loop.
   nEvent := xbe_None
   WHILE nEvent != xbeP_Close
      nEvent := AppEvent ( @mp1, @mp2, @oXbp )

      // Check if an user defined event was retrieved.
      // (The refreshment threads post user events that let the
      //  relating Xbase parts refresh.)
      IF nEvent == xbeP_User + 1
         // This event contains a codeblock which must be evaluated.
         Eval(mp1, oXbp)
      ELSE 
         // Handle the event.
         oXbp:HandleEvent ( nEvent, mp1, mp2 ) 
      ENDIF 
      IF nEvent == xbeP_Quit
         QUIT   // AppQuit()
      ENDIF
   ENDDO
RETURN 



// All ADS-Errors will be handled/shown by this procedure.
PROCEDURE AdsError(nRC, nOperation)
   DO CASE 
      CASE nOperation == ADSMG_CONNECT
           cOp := "Connect"
      CASE nOperation == ADSMG_DISCONNECT
           cOp := "Disconnect"
      CASE nOperation == ADSMG_GETWORKERTHREADACTIVITY
           cOp := "GetWorkerThreadActivity"
      CASE nOperation == ADSMG_GETLOCKS
           cOp := "GetLocks"
      CASE nOperation == ADSMG_GETLOCKOWNER
           cOp := "GetLockOwner"
      CASE nOperation == ADSMG_GETOPENTABLES
           cOp := "GetOpenTables"
      CASE nOperation == ADSMG_GETCOMMSTATS
           cOp := "GetCommStats"
      CASE nOperation == ADSMG_GETUSERNAMES  
           cOp := "GetUserNames"
      CASE nOperation == ADSMG_GETOPENINDEXES
           cOp := "GetOpenIndexes"
      CASE nOperation == ADSMG_RESETCOMMSTATS
           cOp := "ResetCommStats"
      CASE nOperation == ADSMG_GETACTIVITYINFO
           cOp := "GetActivityInfo"
      CASE nOperation == ADSMG_KILLUSER
           cOp := "KillUser"
      CASE nOperation == ADSMG_GETINSTALLINFO
           cOp := "GetInstallInfo"
      CASE nOperation == ADSMG_GETSERVERTYPE
           cOp := "GetServerType"
      CASE nOperation == ADSMG_GETCONFIGINFO
           cOp := "GetConfigInfo"
      OTHERWISE
           cOp := "unknown"
   ENDCASE 

   DO CASE 
      CASE nRC == AE_WRONG_PARAMETER
           AlertBox(,"Wrong types of parameters have been passed to AdsMg"+cOp+".", ;
                  ,XBPSTATIC_SYSICON_ICONWARNING, "ERROR")
      OTHERWISE 
           AlertBox(,"ADS ERROR "+AllTrim(Str(nRC))+" occured during operation <";
                    +cOp+">.",,XBPSTATIC_SYSICON_ICONWARNING, "ADS ERROR")
   ENDCASE 
RETURN 

//EOF
/////
