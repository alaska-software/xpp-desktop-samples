//////////////////////////////////////////////////////////////////////
//
//  SERVERS.PRG
//
//  Copyright:
//   Alaska Software, (c) 1999-2025. All rights reserved.         
//  
//  Contents:
//       Alaska Management Console for the Advantage Database Server
//   
//       Display server installation/configuration information
//////////////////////////////////////////////////////////////////////


#include "Gra.ch"
#include "Xbp.ch"
#include "Common.ch"
#include "Appevent.ch"
#include "Font.ch"
#include "AdsMg.ch"

#define CONFIG_PARAMS 26
#define CONFIG_MEM    13
#define INSTALL_INFO   8



// Show installation info of selected ADS.
FUNCTION ShowServerInstallationInfo(cServer, oStage)
   LOCAL aCaptions := { "User option purchased:", ;  // Identify displayed inf.
                        "Registered owner.....:", ;
                        "Advantage version....:", ;
                        "Installation date....:", ;
                        "OEM char language....:", ;
                        "ANSI char language...:", ;
                        "Eval. expiration date:", ;
                        "Serial number........:"  }
   LOCAL aStatics := Array(INSTALL_INFO)             // Used to display the inf.
   LOCAL aInstallInfo  := Array(INSTALL_INFO)        // Used to retrieve the inf.
   LOCAL nHandle, nRC                                // Connection handle, error code
   LOCAL nI

   IF ValType(oStage) != "O"
      // Abort function when invalid "stage" has been passed.
      RETURN nil
   ENDIF 

   oStage:setcaption("Installation Info")

   // Create GUI elements
   SetAppWindow():Bitmap:=BitmapRef():New(oStage, oStage, {8,8}, {540,236}):Create()
   SetAppWindow():Bitmap:load(ID_ADSGREY)
   SetAppWindow():Bitmap:display()

   // Show information ids and create statics that will contain the data.
   FOR nI := 1 TO INSTALL_INFO
      CreateStatic( oStage, {8,484-nI*24}, {180,24}, aCaptions[nI], "10.Courier" )
      aStatics[nI] := CreateStatic(oStage, {196,484-nI*24}, {200,24}, , "10.Courier")
   NEXT 


   // Call ADS and display info
   nRC := AdsMgConnect(cServer,,,@nHandle)
   IF nRC != AE_SUCCESS
      AdsError(nRC, ADSMG_CONNECT)
   ELSE 
      // Request information.
      nRC := AdsMgGetInstallInfo(nHandle, aInstallInfo)
      IF nRC != AE_SUCCESS
         AdsError(nRC, ADSMG_GETINSTALLINFO)
      ELSE 
         // Fill statics with retrieved information.
         aStatics[1]:setcaption(AllTrim(str(aInstallInfo[1])))
         FOR nI := 2 TO INSTALL_INFO
             aStatics[nI]:setcaption(aInstallInfo[nI])
         NEXT 
      ENDIF 
      // Disconnect from ADS.
      nRC := AdsMgDisConnect(nHandle)
      IF nRC != AE_SUCCESS
         AdsError(nRC, ADSMG_DISCONNECT)
      ENDIF
   ENDIF 
RETURN nil  // This function does not create a refreshment thread.



// Show memory configuration
FUNCTION ShowServerMemConfigurationInfo(cServer, oStage)
   LOCAL aCaptions := { "Total memory taken by configuration parameters:", ; // IDs
                        "Memory taken by connections...................:", ; 
                        "Memory taken by workareas.....................:", ;
                        "Memory taken by tables........................:", ;
                        "Memory taken by indexes.......................:", ;
                        "Memory taken by locks.........................:", ;
                        "Memory taken by user buffer...................:", ;
                        "Memory taken by TPS header elements...........:", ;
                        "Memory taken by TPS visibility elements.......:", ;
                        "Memory taken by TPS memo elements.............:", ;
                        "Memory taken by Receive ECB...................:", ;
                        "Memory taken by Send ECB......................:", ;
                        "Memory taken by worker threads................:"  }
   LOCAL oBitmap                                    // Used to display logo.
   LOCAL aStatics := Array(CONFIG_MEM)              // Used to display inf.
   LOCAL aMemConfigInfo  := Array(CONFIG_MEM)       // Used to retrieve inf.
   LOCAL aConfigParamsInfo := Array(CONFIG_PARAMS)
   LOCAL nHandle, nRC                               // Conn. handle, error code
   LOCAL nI

   IF ValType(oStage) != "O"
      // Invalid "stage" passed. Abort function.
      RETURN nil
   ENDIF 

   oStage:setcaption("Memory Configuration Info")

   // Create GUI elements
   oBitmap:=BitmapRef():New(oStage, oStage, {8,8}, {540,212}):Create()
   oBitmap:load(ID_ADSGREY)
   oBitmap:display()

   // Create ID and data statics.
   FOR nI := 1 TO CONFIG_MEM
      CreateStatic(oStage, {8,532-nI*24}, {380,24}, aCaptions[nI], "10.Courier")
      aStatics[nI] := CreateStatic(oStage, {396,532-nI*24}, {100,24}, , "10.Courier")
   NEXT 



   // Call ADS and display info
   nRC := AdsMgConnect(cServer,,,@nHandle)
   IF nRC != AE_SUCCESS
      AdsError(nRC, ADSMG_CONNECT)
   ELSE 
      // Retrieve information.
      nRC := AdsMgGetConfigInfo(nHandle, aConfigParamsInfo, aMemConfigInfo)
      IF nRC != AE_SUCCESS
         AdsError(nRC, ADSMG_GETCONFIGINFO)
      ELSE
         // Fill statics with information.
         FOR nI := 1 TO CONFIG_MEM
             aStatics[nI]:setcaption(AllTrim(str(aMemConfigInfo[nI])))
         NEXT 
      ENDIF 

      // Disconnect from ADS.
      nRC := AdsMgDisConnect(nHandle)
      IF nRC != AE_SUCCESS
         AdsError(nRC, ADSMG_DISCONNECT)
      ENDIF 
  ENDIF 
RETURN nil // This function does not create a refreshment thread, so return nil.



// Show configuration parameters
FUNCTION ShowServerConfigurationParamsInfo(cServer, oStage)
   LOCAL aCaptions := { "Number of connections............:", ; //Information IDs.
                        "Number of work areas.............:", ;
                        "Number of tables.................:", ;
                        "Number of indexes................:", ;
                        "Number of locks..................:", ;
                        "User buffer......................:", ;
                        "Statistics dump interval.........:", ;
                        "Max. size of error log...........:", ;
                        "Number of TPS header elements....:", ;
                        "Number of TPS visibility elements:", ;
                        "Number of TPS memo elements......:", ;
                        "Number of Receive ECBs...........:", ;
                        "Number of Send ECBs..............:", ;
                        "Number of packets per burst......:", ;
                        "Number of worker threads.........:", ;
                        "Index sort buffer size...........:", ;
                        "Reserved.........................:", ;
                        "Reserved.........................:", ;
                        "Error log path...................:", ;
                        "Semaphore file path..............:", ;
                        "TPS log file path................:", ;
                        "Reserved.........................:", ;
                        "Reserved.........................:", ;
                        "NT Service IP send port..........:", ;
                        "NT Service IP receive port.......:", ;
                        "Reserved.........................:"  }
   LOCAL aStatics := Array(CONFIG_PARAMS)           // Used to show information.
   LOCAL aMemConfigInfo  := Array(CONFIG_MEM)       // Used to retrieve inf.
   LOCAL aConfigParamsInfo := Array(CONFIG_PARAMS)
   LOCAL nHandle, nRC                               // Conn. handle, error code.
   LOCAL nI

   IF ValType(oStage) != "O"
      // Invalid "stage". Abort function.
      RETURN nil
   ENDIF 

   oStage:setcaption("Configuration Parameters Info")


   // Create GUI elements
   FOR nI := 1 TO CONFIG_PARAMS
      CreateStatic(oStage, {8,532-nI*20}, {280,20}, aCaptions[nI] , "8.Courier")
      aStatics[nI] := CreateStatic(oStage, {296,532-nI*20}, {200,20},,"8.Courier")
   NEXT 


   // Call ADS and display info
   nRC := AdsMgConnect(cServer,,,@nHandle)
   IF nRC != AE_SUCCESS
      AdsError(nRC, ADSMG_CONNECT)
   ELSE 
      // Request information.
      nRC := AdsMgGetConfigInfo(nHandle, aConfigParamsInfo, aMemConfigInfo)
      IF nRC != AE_SUCCESS
         AdsError(nRC, ADSMG_GETCONFIGINFO)
      ELSE 
         // Fill statics with retrieved information.
         FOR nI := 1 TO CONFIG_PARAMS
             IF ValType(aConfigParamsInfo[nI]) == "N"
                aStatics[nI]:setcaption(AllTrim(str(aConfigParamsInfo[nI])))
             ELSEIF ValType(aConfigParamsInfo[nI]) == "C"
                aStatics[nI]:setcaption(aConfigParamsInfo[nI])
             ELSE 
                aStatics[nI]:setcaption("***")
             ENDIF 
         NEXT 
      ENDIF 

      // Disconnect from ADS.
      nRC := AdsMgDisConnect(nHandle)
      IF nRC != AE_SUCCESS
         AdsError(nRC, ADSMG_DISCONNECT)
      ENDIF 
  ENDIF 
RETURN nil // No refreshment thread is created, return nil.













