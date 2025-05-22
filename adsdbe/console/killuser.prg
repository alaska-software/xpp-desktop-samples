//////////////////////////////////////////////////////////////////////
//
//  KILLUSER.PRG
//
//  Copyright:
//   Alaska Software, (c) 1999-2025. All rights reserved.         
//  
//  Contents:
//       Alaska Management Console for the Advantage Database Server
//   
//       Kill User (Force disconnection).
//////////////////////////////////////////////////////////////////////


#include "Gra.ch"
#include "Xbp.ch"
#include "Common.ch"
#include "Appevent.ch"
#include "Font.ch"
#include "AdsMg.ch"

// Major function for KILL USER
FUNCTION KillUser(cAdsDrive, oStage)
   LOCAL nI
   LOCAL oListBox
   LOCAL oPushButton
   LOCAL oTitle

   IF ValType(oStage) != "O"
      RETURN nil
   ENDIF 

   // Set title of "stage".
   oStage:setcaption("Kill User (Force disconnect)")


   // Create GUI-elements
   CreateStatic(oStage, {8, 509}, {416,24}, ;
                "Snapshot list of connected users. "+ ;
                "Actual data may already differ.")

   // Listbox that contains a "snapshot" of the connected users.
   oListBox := CreateListbox(oStage, oStage, {8,259}, {416,250} )

   // Fill listbox with ids of connected users.
   KUFillListBox(cAdsDrive, oListBox)

   // This pushbutton activates the "kill" procedure.
   oPushButton := XbpPushButton():New(oStage, oStage, {454,259}, {100,24})
   oPushButton:setcaption("Kill user")
   oPushButton:activate := {|| KUKillUser(cAdsDrive, oListBox) }
   oPushButton:create()

   // Show ADSDBE logo.
   SetAppWindow():Bitmap:=BitmapRef():New(oStage, oStage, {8,8}, {540,236}):Create()
   SetAppWindow():Bitmap:load(ID_ADSGREY)
   SetAppWindow():Bitmap:display()
RETURN nil



// Fill the listbox with a list of active users
PROCEDURE KUFillListBox( cAdsDrive, oListBox )
   LOCAL nHandle := 0
   LOCAL aUserNames := Array(ADS_MAX_INFO)
   LOCAL nI, nAmount := ADS_MAX_INFO
   LOCAL nRC := AdsMgConnect(cAdsDrive,,,@nHandle) // Connect to ADS.

   // Clear the listbox.
   oListBox:clear()

   IF nRC != AE_SUCCESS
      AdsError(nRC, ADSMG_CONNECT)
   ELSE 
      // Prepare array to meet the requirements of the Ads function.
      FOR nI := 1 TO ADS_MAX_INFO
         aUserNames[nI] := Array(2)
      NEXT 

      // Get the information required for filling the listbox.
      nRC := AdsMgGetUserNames(nHandle,,aUserNames , @nAmount)
      IF nRC != AE_SUCCESS
         AdsError(nRC, ADSMG_GETUSERNAMES)
      ELSE 
         // Now fill the listbox.
         FOR nI := 1 TO nAmount
            oListBox:additem(aUserNames[nI,1])
         NEXT 
      ENDIF 

      // Disconnect from ADS.
      nRC := AdsMgDisConnect(nHandle)
      IF nRC != AE_SUCCESS
         AdsError(nRC, ADSMG_DISCONNECT)
      ENDIF 
   ENDIF 
RETURN 



// This function finally "kills" an user
PROCEDURE KUKillUser(cAdsDrive, oListBox)
   LOCAL aMarked := oListBox:getdata()
   LOCAL nAmount := len(aMarked)
   LOCAL cUser
   LOCAL nHandle, nRC

   IF nAmount == 0
      // No user selected in the listbox.
      AlertBox(,"A user must be selected to perform this operation.", ;
               ,XBPSTATIC_SYSICON_ICONINFORMATION, "Info")
   ELSE
      // Get first marked user. (Others are ignored.)
      cUser := oListBox:getitem(aMarked[1])
      
      // "Kill user" on confirmation.
      IF AlertBox(, "Forcing disconnection of user "+cUser+". ;";
                     +"This may result in a loss of data and inconsistent databases. ;";
                     +"Do you wish to proceed?", ;
                     {"YES", "NO"}, ;
                     XBPSTATIC_SYSICON_ICONQUESTION, ;
                     "Please confirm:" ) == 1
                    

         // Connect to Ads.
         nRC := AdsMgConnect(cAdsDrive,,,@nHandle)

         IF nRC != AE_SUCCESS
            AdsError(nRC, ADSMG_CONNECT)
         ELSE 

            // Force disconnect of first marked user.
            nRC := AdsMgKillUser(nHandle,cUser)
            IF nRC != AE_SUCCESS
               IF nRC == 7050
                  // ADS-Error 7050:Looks as if that user has already gone.
                  AlertBox(,"User has already disconnected in the meantime.", ;
                           ,XBPSTATIC_SYSICON_ICONINFORMATION, "Info")
               ELSE 
                  AdsError(nRC, ADSMG_KILLUSER)
               ENDIF 
            ELSE
               // Successful disconnection.
               AlertBox(,"User has been successfully disconnected.", ;
                        ,XBPSTATIC_SYSICON_ICONINFORMATION," Info")
            ENDIF 
 
            // Disconnect from ADS.
            nRC := AdsMgDisConnect(nHandle)
            IF nRC != AE_SUCCESS
               AdsError(nRC, ADSMG_DISCONNECT)
            ENDIF 

            // Refresh the listbox.
            KUFillListBox(cAdsDrive, oListBox)
         ENDIF 
      ENDIF 
   ENDIF 
RETURN 




