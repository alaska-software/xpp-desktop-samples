//////////////////////////////////////////////////////////////////////
//
//  COMMSTAT.PRG
//
//  Copyright:
//   Alaska Software, (c) 1999-2025. All rights reserved.         
//  
//  Contents:
//       Alaska Management Console for the Advantage Database Server
//   
//       Display communication statistics
//////////////////////////////////////////////////////////////////////


#include "Gra.ch"
#include "Xbp.ch"
#include "Common.ch"
#include "Appevent.ch"
#include "Font.ch"
#include "AdsMg.ch"

#define COMM_ELEMENTS 11  // Number of elements that will be displayed.


// This function creates the Xbase parts required for displaying the
// information and returns a thread object that automatically refreshes
// the communication statistics. The drive letter of the mapped ADS server
// and the "stage" where the data has to be shown must be passed as parameter.
FUNCTION ShowCommStats(cAdsDrive, oStage)
   LOCAL nI
   LOCAL aStatics := Array(COMM_ELEMENTS) // Array of statics for data display.
   LOCAL oThread                          // Thread object
   LOCAL oPushButton                      // "Reset" pushbutton
   LOCAL oTitle                           // IDs of static
   LOCAL oProceed := Signal():New()       // Signal to ensure start of thread

   IF ValType(oStage) != "O"  // No valid "stage" 
      RETURN nil
   ENDIF 

   // Set title of groupbox
   oStage:setcaption("Communication Statistics")

   // Show ADSDBE logo
   SetAppWindow():Bitmap:=BitmapRef():New(oStage, oStage, {8,8}, {540,236}):Create()
   SetAppWindow():Bitmap:load(ID_ADSGREY)
   SetAppWindow():Bitmap:display()

   // Show the IDs of the data statics that display the data
   CreateStatic(oStage, {8,484}, {300,20}, ;
                "% of packets with checksum failures :", "10.Courier")

   CreateStatic(oStage, {8,460}, {300,20}, ;
                "Total packets received..............:", "10.Courier")

   CreateStatic(oStage, {8,436}, {300,20}, ;
                "Received packets out of sequence....:", "10.Courier")

   CreateStatic(oStage, {8,412}, {300,20}, ;
                "Packets whose owner is not logged in:", "10.Courier")

   CreateStatic(oStage, {8,388}, {300,20}, ;
                "Received requests out of sequence...:", "10.Courier")

   CreateStatic(oStage, {8,364}, {300,20}, ;
                "Checksum failures...................:", "10.Courier")

   CreateStatic(oStage, {8,340}, {300,20}, ;
                "Server initiated disconnects........:", "10.Courier")

   CreateStatic(oStage, {8,316}, {300,20}, ;
                "Removed partial connections.........:", "10.Courier")

   CreateStatic(oStage, {8,292}, {300,20}, ;
                "Received invalid packets............:", "10.Courier")

   CreateStatic(oStage, {8,268}, {300,20}, ;
                "Receive From failed.................:", "10.Courier")

   CreateStatic(oStage, {8,244}, {300,20}, ;
                "Send To failed......................:", "10.Courier")

   // Pushbutton that resets the communication statistics on activation.
   oPushButton := XbpPushbutton():New(oStage,oStage,{416,244},{140,24})
   oPushButton:activate := {|| ResetCommStats(cAdsDrive) }
   oPushButton:setfontcompoundname("10.Courier")
   oPushButton:caption := "Reset Comm Stats"
   oPushButton:create()

   // Create the statics that show the information itself.
   FOR nI := 1 TO COMM_ELEMENTS
      aStatics[nI] := CreateStatic(oStage, {308,508-nI*24}, {100,20},,"10.Courier")
   NEXT 

   // Create and start the thread that refreshes the data.
   oThread:=CommStats():New()
   oThread:start(, oProceed, cAdsDrive, aStatics)
   oProceed:wait()
RETURN oThread


// Reset the communication stastics. The drive letter of the mapped
// ADS server must be passed to the procedure.
PROCEDURE ResetCommStats(cAdsDrive)
   LOCAL nRC, nHandle

   // Connect to the ADS. Show error if unsuccessful.
   nRC := AdsMgConnect(cAdsDrive,,,@nHandle)  
   IF nRC != AE_SUCCESS
      AdsError(nRC, ADSMG_CONNECT)
   ELSE 
      // Reset the communication statistics.
      nRC := AdsMgResetCommStats(nHandle)
      IF nRC != AE_SUCCESS
         AdsError(nRC, ADSMG_RESETCOMMSTATS)
      ENDIF 

      // Disconnect from ADS.
      nRC := AdsMgDisConnect(nHandle)
      IF nRC != AE_SUCCESS
         AdsError(nRC, ADSMG_DISCONNECT)
      ENDIF 
   ENDIF 
RETURN 



// This class implements the thread that automatically refreshes the
// communication statistics information.
CLASS CommStats from Thread
   EXPORTED:
   VAR nHandle  // Handle of ADS connection.

   METHOD atstart, execute, atend
ENDCLASS 


// This method is only executed at the starting time of the thread.
// A signal object, the drive letter of the mapped ADS and an array of
// Xbase part to show the information must be passed to this method.
METHOD CommStats:atstart(oProceed, cAdsDrive, aStage)
   LOCAL nRC
  
   // Set the thread interval to the default setting.
   ::setinterval(DEF_INTERVAL)
   ::nHandle := 0

   // Connect to the ADS.
   nRC := AdsMgConnect(cAdsDrive,,,@::nHandle)
   IF nRC != AE_SUCCESS
      // Could not connect to ADS. Terminate the thread and set an invalid
      // connection handle.
      AdsError(nRC, ADSMG_CONNECT)
      ::setinterval( nil )
      ::nHandle := -1
   ENDIF 
   // Signal that the thread has been started.
   oProceed:signal()
RETURN self



// This method is only executed when the thread ends.
METHOD CommStats:atend()
   LOCAL nRC

   IF ::nHandle != -1
      // Valid connection handle. Disconnect from ADS.
      nRC := AdsMgDisconnect(::nHandle)
      IF nRC != AE_SUCCESS
         AdsError(nRC, ADSMG_DISCONNECT)
      ENDIF 
   ENDIF 
RETURN self



// Refresh the communication statistics information.
METHOD CommStats:execute(oProceed, cAdsDrive, aStage)
   LOCAL aInfo := Array(COMM_ELEMENTS)
   LOCAL cWork := ""

   // Get comm. stats.
   nRC := AdsMgGetCommStats(::nHandle, aInfo)

   IF nRC != AE_SUCCESS
      // Error. Show according message.
      AdsError(nRC, ADSMG_GETCOMMSTATS)
   ELSE 
      // Refresh the information.
      FOR nI := 1 TO COMM_ELEMENTS
        IF nI == 1
           aStage[nI]:setcaption(Str(aInfo[nI],10,2))
        ELSE 
           aStage[nI]:setcaption(Str(aInfo[nI]))
        ENDIF 
      NEXT 
   ENDIF 
RETURN self






