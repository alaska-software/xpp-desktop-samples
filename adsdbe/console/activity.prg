//////////////////////////////////////////////////////////////////////
//
//  ACTIVITY.PRG
//
//  Copyright:
//   Alaska Software, (c) 1999-2025. All rights reserved.         
//  
//  Contents:
//       Alaska Management Console for the Advantage Database Server
//   
//////////////////////////////////////////////////////////////////////


#include "Gra.ch"
#include "Xbp.ch"
#include "Common.ch"
#include "Appevent.ch"
#include "Font.ch"
#include "AdsMg.ch"

#define ACTIVITY_ELEMENTS 13

FUNCTION ShowActivityInfo(cAdsDrive, oStage)
   LOCAL aCaptions :=  { "Number of operations since started:", ;
                         "Number of logged errors...........:", ;
                         "Length of time ADS has been up....:", ;
                         "Users.............................:", ;
                         "Connections.......................:", ;
                         "Work areas........................:", ;
                         "Tables............................:", ;
                         "Indexes...........................:", ;
                         "Locks.............................:", ;
                         "TPS header elements...............:", ;
                         "TPS visibility elements...........:", ;
                         "TPS memo elements.................:", ;
                         "Worker threads....................:"  }
   LOCAL nI, nN
   LOCAL aStatics := Array(ACTIVITY_ELEMENTS)
   LOCAL oThread
   LOCAL oPushButton
   LOCAL oTitle
   LOCAL oProceed := Signal():New()

   IF ValType(oStage) != "O"
      RETURN nil
   ENDIF 


   oStage:setcaption("Activity Info")

   // Create GUI elements
   SetAppWindow():Bitmap:=BitmapRef():New(oStage, oStage, {8,8}, {540,164}):Create()
   SetAppWindow():Bitmap:load(ID_ADSGREY)
   SetAppWindow():Bitmap:display()

   FOR nI := 1 TO ACTIVITY_ELEMENTS
      IF nI <= 3
         CreateStatic(oStage, {8,532-nI*24}, {300,24}, aCaptions[nI], "10.Courier")
      ELSE
         CreateStatic(oStage, {8,484-nI*24}, {300,24}, aCaptions[nI], "10.Courier")
      ENDIF 
   NEXT 

   CreateStatic(oStage, {308,412}, {70,20}, "  In use", "10.Courier")
   CreateStatic(oStage, {378,412}, {70,20}, " Maximum", "10.Courier")
   CreateStatic(oStage, {448,412}, {70,20}, "Rejected", "10.Courier")

   // GUI-elements to display data
   FOR nI := 1 TO ACTIVITY_ELEMENTS
      IF nI <= 3
         aStatics[nI] := CreateStatic(oStage, {308,532-nI*24}, {200,20},,"10.Courier")
      ELSE
         aStatics[nI]:=Array(3)
         FOR nN := 1 TO 3
            aStatics[nI,nN]:=CreateStatic(oStage, {308+(nN-1)*70,484-nI*24}, ;
                                          {70,20},,"10.Courier")
         NEXT 
      ENDIF 
   NEXT 

   // Start the thread that retrieves and displays the data
   oThread:=ActivityThread():New()
   oThread:start(, oProceed, cAdsDrive, aStatics)
   oProceed:wait()
RETURN oThread




CLASS ActivityThread from Thread
   EXPORTED:
   VAR nHandle

   METHOD atstart, execute, atend
ENDCLASS 



// Connect to ADS...
METHOD ActivityThread:atstart(oProceed, cAdsDrive, aStage)
   LOCAL nRC
  
   ::setinterval(100)
   ::nHandle := 0

   nRC := AdsMgConnect(cAdsDrive,,,@::nHandle)
   IF nRC != AE_SUCCESS
      AdsError(nRC, ADSMG_CONNECT)
      ::setinterval( nil )
      ::nHandle := -1
   ENDIF 
   oProceed:signal()
RETURN self



// Disconnect from ADS
METHOD ActivityThread:atend()
   LOCAL nRC

   IF ::nHandle != -1
      nRC := AdsMgDisconnect(::nHandle)
      IF nRC != AE_SUCCESS
         AdsError(nRC, ADSMG_DISCONNECT)
      ENDIF 
   ENDIF 
RETURN self



// Retrieve data, display data.
METHOD ActivityThread:execute(oProceed, cAdsDrive, aStage)
   LOCAL aInfo := Array(ACTIVITY_ELEMENTS)
   LOCAL cWork := ""
   LOCAL nI, nN

   aInfo[3] := Array(4)
   FOR nI := 4 TO ACTIVITY_ELEMENTS
      aInfo[nI] := Array(3)
   NEXT 
      
   nRC := AdsMgGetActivityInfo(::nHandle, aInfo)

   IF nRC != AE_SUCCESS
      AdsError(nRC, ADSMG_GETACTIVITYINFO)
   ELSE 
      FOR nI := 1 TO ACTIVITY_ELEMENTS
        IF nI <= 2
           aStage[nI]:setcaption(Str(aInfo[nI]))
        ELSEIF nI == 3
           // Time server is running
           aStage[nI]:setcaption(Str(aInfo[nI,1],5,0)+ " Days, "+;
                                 StrZero(aInfo[nI,2],2,0)+":"+ ;
                                 StrZero(aInfo[nI,3],2,0)+":"+ ;
                                 StrZero(aInfo[nI,4],2,0))
        ELSE 
           // In use/max/rejected data
           FOR nN := 1 TO 3
              aStage[nI,nN]:setcaption(Str(aInfo[nI,nN],8,0))
           NEXT 
        ENDIF 
      NEXT 
   ENDIF 
RETURN self






