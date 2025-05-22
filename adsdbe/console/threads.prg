//////////////////////////////////////////////////////////////////////
//
//  THREADS.PRG
//
//  Copyright:
//   Alaska Software, (c) 1999-2025. All rights reserved.         
//  
//  Contents:
//       Alaska Management Console for the Advantage Database Server
//   
//       Thread classes that are used to automatically refresh the
//       contents of the browser created by the function ShowBrowser().
//////////////////////////////////////////////////////////////////////


#include "Gra.ch"
#include "Xbp.ch"
#include "Common.ch"
#include "Appevent.ch"
#include "Font.ch"
#include "AdsMg.ch"

#pragma Library( "XppUI2.LIB" ) 
#pragma Library( "Adac20B.LIB" ) 



/********************************************************************************
     Generic function that shows the selected browser 
     and starts the refresher thread
 ********************************************************************************/
FUNCTION ShowBrowser(nBrowser, cAdsDrive, oStage)
   LOCAL oThread                     // Thread object that refreshes the browser.
   LOCAL oBrowse                     // Browser object that contains the inf.
   LOCAL oProceed := Signal():New()  // Used to synchronize with the start of 
                                     // the thread.
   LOCAL aCaptions := { "Locks and lock owners", ;
                        "Open Indexes", ;
                        "Worker Thread Activity", ;
                        "Open Tables", ;
                        "User Names" }
   LOCAL aCols
   LOCAL aArray
   LOCAL aBrowSize := {546,520}

   DO CASE
      CASE nBrowser == ADSMG_LOCKBROWSER
         aCols := { "User"+Space(5), ;
                    "Conn. num.", ;
                    "Table name"+Space(10), ;
                    "Locked rec.", ;
                    "Locking type", ;
                    "Auth. User", ;
                    "Auth. User IP", ;
                    "OS Login Name", ;
                    "TS client IP", ;
                  }
         aArray := Array( ADS_MAX_INFO, 9 )

         // Do this to set correct datatypes
         aArray[1] := { "NONE", 0, "NONE", 0, "NONE", "NONE", "NONE", "NONE", "NONE" }
      CASE nBrowser == ADSMG_INDEXBROWSER
         aCols := {"Index Name"+Space(55)}
         aArray := Array( ADS_MAX_INFO, 1 )

         // Do this to set correct datatypes
         aArray[1,1] := "NONE"
      CASE nBrowser == ADSMG_THREADBROWSER
         aCols := {"Thread ID"+Space(2), ;
                   "Operation", ;
                   "User"+Space(6), ;
                   "Conn. number" }
         aArray := Array( ADS_MAX_INFO, 4 )

         // Do this to create correct datatypes
         aArray[1] := { 0,0,"NONE",0 }

         // Show logo.
         SetAppWindow():Bitmap:=BitmapRef():New(oStage, oStage, {402,8}, ;
                                                {160,520}):Create()
         SetAppWindow():Bitmap:load(ID_ADSVERT)
         SetAppWindow():Bitmap:display()

         aBrowSize := {386,520}
      CASE nBrowser == ADSMG_TABLEBROWSER
         aCols := { "Table name"+Space(42), ;
                    "Locking mode" }
         aArray := Array( ADS_MAX_INFO, 2 )

         // Do this to set correct datatypes
         aArray[1] := { "NONE", "NONE" }
      CASE nBrowser == ADSMG_USERBROWSER
         aCols := { "User name "+Space(42), ;
                    "Conn. number" }
         aArray := Array( ADS_MAX_INFO, 2 )

         // Do this to set correct datatypes
         aArray[1] := { "NONE", 0 }
   ENDCASE 

   // Set the caption
   oStage:setcaption( aCaptions[nBrowser] )

   // Create the browser.
   oBrowse := XbpQuickBrowse():new(oStage,oStage, {8,8}, aBrowSize )
   oBrowse:datalink := DacPagedDataStore():New( aArray )
   oBrowse:dataArea:referenceArray := aCols
   oBrowse:hScroll := .F.
   oBrowse:create()
   oBrowse:setHeader( aCols )

   // Create and start the thread.
   DO CASE
      CASE nBrowser == ADSMG_LOCKBROWSER
         oThread:=LockOwners():New()
      CASE nBrowser == ADSMG_INDEXBROWSER
         oThread:=OpenIndexes():New()
      CASE nBrowser == ADSMG_THREADBROWSER
         oThread:=ThreadActivity():New()
      CASE nBrowser == ADSMG_TABLEBROWSER
         oThread:=OpenTables():New()
      CASE nBrowser == ADSMG_USERBROWSER
         oThread:=UserNames():New()
   ENDCASE 

   oThread:start(, oProceed, cAdsDrive, oBrowse)

   // Wait until the thread signals availability.
   oProceed:Wait()
RETURN oThread



/****************************************************************
 *                         THREAD ACTIVITY                      *
 ****************************************************************/

CLASS ThreadActivity FROM AdsInfoThread
   EXPORTED:
   METHOD execute
ENDCLASS 



METHOD ThreadActivity:execute(oProceed,cAdsDrive, oBrowse)
   LOCAL nRC, nAmount := ADS_MAX_INFO
   LOCAL bBlock := {|o| o:refreshall() } //Codeblock for user event.
   LOCAL aArray := Array( ADS_MAX_INFO, 4 )

   // Request information.
   nRC := AdsMgGetWorkerThreadActivity(::nHandle, aArray , @nAmount)
   IF nRC != AE_SUCCESS
      AdsError(nRC, ADSMG_GETWORKERTHREADACTIVITY)
   ELSE 
      // In case there was no information retrieved
      IF nAmount == 0 
         aArray[1] := { 0,0,"NONE",0 }
      ENDIF 

      oBrowse:datalink := DacPagedDataStore():New( aArray )

      // Post a user event that lets the browser refresh its information.
      PostAppEvent( xbeP_User + 1,bBlock ,,oBrowse)
   ENDIF 
RETURN self



/****************************************************************
 *                          LOCKS AND OWNERS                    *
 ****************************************************************/

CLASS LockOwners FROM AdsInfoThread
   EXPORTED:
   VAR aOpenTables
   VAR aLocks
   VAR aLockOwner
   METHOD init, execute
ENDCLASS 


METHOD LockOwners:init()
   LOCAL nI
  
   // Create the arrays required by the Ads functions.
   ::aOpenTables := Array(ADS_MAX_INFO)

   // Ensure the dimensions awaited by the Ads functions.
   FOR nI := 1 TO ADS_MAX_INFO
      ::aOpenTables[nI] := Array(2)
   NEXT 

   ::aLocks := Array(ADS_MAX_INFO)
   ::aLockOwner := Array(6)

   ::AdsInfoThread:init()
RETURN self


// To receive a complete list of all lock owners, a list of open tables
// must be requested. Based upon this list, all record locks for each table
// must be requested. Then, finally, the owner of the record lock can be
// requested. (Hm, many "be requested"...)
METHOD LockOwners:execute(oProceed,cAdsDrive, oBrowse)
   LOCAL nRC, nI, nN
   LOCAL nOpenTables := ADS_MAX_INFO
   LOCAL nLocks
   LOCAL nAmount     := 0            // Counter for displayed rows
   LOCAL bBlock := {|o| o:refreshall() }
   LOCAL aArray := Array( ADS_MAX_INFO, 9 )

   // Get an array of open tables.
   nRC := AdsMgGetOpenTables(::nHandle,,,::aOpenTables , @nOpenTables)
   IF nRC != AE_SUCCESS
      AdsError(nRC, ADSMG_GETOPENTABLES)
   ELSE 
      // In case there was no information retrieved
      aArray[1] := { "NONE", 0, "NONE", 0, "NONE", "NONE", "NONE", "NONE", "NONE" }

      FOR nI := 1 TO nOpenTables
         // Now request the locks of each open table.
         nLocks := ADS_MAX_INFO
         nRC := AdsMgGetLocks(::nHandle,::aOpenTables[nI,1],,,::aLocks,@nLocks)
         IF nRC != AE_SUCCESS
            AdsError(nRC, ADSMG_GETLOCKS)  // Operation unsuccessful
         ELSE 
            // Do this for each lock
            FOR nN := 1 TO nLocks    
               // Now get the owner of each lock.
               nRC := AdsMgGetLockOwner(::nHandle,::aOpenTables[nI,1],::aLocks[nN], ;
                                        ::aLockOwner, @nLockType)
               IF nRC != AE_SUCCESS
                  AdsError(nRC, ADSMG_GETLOCKOWNER)
               ELSE 
                  // Well, we're there... The browser-array can be filled with
                  // the found information. (The retrieved data is copied to
                  // a thread-local array, first.)
                  IF nAmount+1 <= ADS_MAX_INFO   // prevent overflow
                     nAmount++                   // Increase counter

                     aArray[nAmount,1] := ::aLockOwner[1]
                     aArray[nAmount,2] := ::aLockOwner[2]

                     // Add table name only once to the browser.
                     IF nN == 1
                        aArray[nAmount,3] := ::aOpenTables[nI,1]
                     ELSE 
                        aArray[nAmount,3] := Space(50)
                     ENDIF 

                     aArray[nAmount,4] := ::aLocks[nN]

                     // Determine lock type.
                     DO CASE 
                        CASE nLockType == ADS_MGMT_RECORD_LOCK
                           aArray[nAmount,5] := "Record lock"
                        CASE nLockType == ADS_MGMT_FILE_LOCK
                           aArray[nAmount,5] := "File lock"
                        CASE nLockType == ADS_MGMT_NO_LOCK
                           aArray[nAmount,5] := "No lock"
                        OTHERWISE 
                           aArray[nAmount,5] :="Unknown"
                     ENDCASE 

                     IF .NOT. Empty( ::aLockOwner[3] )
                       aArray[nAmount, 6] := ::aLockOwner[3]
                     ENDIF
                     IF .NOT. Empty( ::aLockOwner[4] )
                       aArray[nAmount, 7] := ::aLockOwner[4]
                     ENDIF
                     IF .NOT. Empty( ::aLockOwner[5] )
                       aArray[nAmount, 8] := ::aLockOwner[5]
                     ENDIF
                     IF .NOT. Empty( ::aLockOwner[6] )
                       aArray[nAmount, 9] := ::aLockOwner[6]
                     ENDIF
                  ENDIF //Overflow-prevention
               ENDIF //ADS-Error
            NEXT // For nN
         ENDIF // ADS-Error
      NEXT // For nI

      oBrowse:dataLink := DacPagedDataStore():New( aArray )
   ENDIF // ADS-Error

   // Tell the browser to refresh. (Codeblock is posted to the event queue.)
   PostAppEvent( xbeP_User + 1,bBlock ,,oBrowse)
RETURN self



/****************************************************************
 *                              INDEXES                         *
 ****************************************************************/

CLASS OpenIndexes FROM AdsInfoThread
   EXPORTED:
   METHOD execute
ENDCLASS 



METHOD OpenIndexes:execute(oProceed, cAdsDrive, oBrowse)
   LOCAL nRC, nAmount := ADS_MAX_INFO
   LOCAL bBlock := {|o| o:refreshall() }  // Codeblock for user-event.
   LOCAL aArrayGetInfo := Array( ADS_MAX_INFO )
   LOCAL aArray := Array( ADS_MAX_INFO, 1 )
   LOCAL nI

   // Retrieve information from ADS.
   nRC := AdsMgGetOpenIndexes(::nHandle,,,,aArrayGetInfo , @nAmount)
   IF nRC != AE_SUCCESS
      AdsError(nRC, ADSMG_GETOPENINDEXES)
   ELSE 
      FOR nI := 1 TO ADS_MAX_INFO
         aArray[nI,1] := aArrayGetInfo[nI]
      NEXT 

      // In case there was no information retrieved
      IF nAmount == 0 
         aArray[1] := { "NONE" }
      ENDIF 

      oBrowse:dataLink := DacPagedDataStore():New ( aArray )

      // Post user event to refresh the browser.
      PostAppEvent( xbeP_User + 1,bBlock ,,oBrowse)
   ENDIF 
RETURN self



/****************************************************************
 *                             TABLES                           *
 ****************************************************************/

CLASS OpenTables FROM AdsInfoThread
   EXPORTED:
   METHOD execute
ENDCLASS 



METHOD OpenTables:execute(oProceed,cAdsDrive, oBrowse)
   LOCAL nRC, nI, nAmount := ADS_MAX_INFO
   LOCAL bBlock := {|o| o:refreshall() } //Codeblock for user event.
   LOCAL aArray := Array( ADS_MAX_INFO, 2 )

   // Retrieve information from ADS.
   nRC := AdsMgGetOpenTables(::nHandle,,,aArray , @nAmount)
   IF nRC != AE_SUCCESS
      AdsError(nRC, ADSMG_GETOPENTABLES)
   ELSE 
      FOR nI := 1 TO nAmount
         // Store information to browser.
         DO CASE 
            CASE aArray[nI,2] == ADS_MGMT_PROPRIETARY_LOCKING
              aArray[nI,2] := "PROPR."
            CASE aArray[nI,2] == ADS_MGMT_CDX_LOCKING
               aArray[nI,2] := "CDX"
            CASE aArray[nI,2] == ADS_MGMT_NTX_LOCKING
               aArray[nI,2] := "NTX"
            CASE aArray[nI,2] == ADS_MGMT_ADT_LOCKING
               aArray[nI,2] := "ADT"
            OTHERWISE 
               aArray[nI,2] := "UNKNOWN"
         ENDCASE 
      NEXT 

      FOR nI := nAmount + 1 TO ADS_MAX_INFO
          aArray[nI] := { NIL, NIL }
      NEXT 

      // In case there was no information retrieved.
      IF nAmount == 0
         aArray[1] := { "NONE", "NONE" }
      ENDIF 

      oBrowse:dataLink := DacPagedDataStore():New( aArray )

      // Post a user event that lets the browser refresh its data.
      PostAppEvent( xbeP_User + 1,bBlock ,,oBrowse)
   ENDIF 
RETURN self



/****************************************************************
 *                              USERS                           *
 ****************************************************************/

CLASS UserNames FROM AdsInfoThread
   EXPORTED:
   METHOD execute
ENDCLASS 



METHOD UserNames:execute(oProceed, cAdsDrive, oBrowse)
   LOCAL nRC, nAmount := ADS_MAX_INFO
   LOCAL bBlock := {|o| o:refreshall(),o:forcestable() } //Codeblock used for user event.
   LOCAL aArray := Array( ADS_MAX_INFO, 2 )
   LOCAL nI

   // Request information.
   nRC := AdsMgGetUserNames(::nHandle,, aArray , @nAmount)
   IF nRC != AE_SUCCESS
      AdsError(nRC, ADSMG_GETUSERNAMES)
   ELSE 
      // The function fills the second col. of the array
      // with zeros. These are now removed.
      FOR nI := nAmount + 1 TO ADS_MAX_INFO
          aArray[nI] := { NIL, NIL }
      NEXT 

      // In case there was no information retrieved
      IF nAmount == 0 
         aArray[1] := { "NONE", 0 }
      ENDIF 

      oBrowse:dataLink := DacPagedDataStore():New( aArray )

      // Post user event that let's the browser refresh.
      PostAppEvent( xbeP_User + 1,bBlock ,,oBrowse)
   ENDIF 
RETURN self



/******************************************************************
 *                             SUPERCLASS                         *
 ******************************************************************/

// This class implements the superclass on which the threads
// that automatically refresh the browsers are based.
CLASS AdsInfoThread FROM Thread
   EXPORTED:
   VAR nHandle  // ADS connection handle.
   METHOD atstart, atend
ENDCLASS 



// Only executed at start-up time of thread.
METHOD AdsInfoThread:atstart(oProceed,cAdsDrive, oStage)
   LOCAL nRC
  
   // Set default execution interval.
   ::setinterval(DEF_INTERVAL)
   ::nHandle := 0

   // Connect to ADS and retrieve connection handle.
   nRC := AdsMgConnect(cAdsDrive,,,@::nHandle)

   IF nRC != AE_SUCCESS
      // Could not connect to ADS. Set execution interval to nil and
      // set an invalid connection handle.
      ::setinterval( NIL )
      ::nHandle := -1
      AdsError(nRC, ADSMG_CONNECT)
   ENDIF 
   // Signal successful start of thread.
   oProceed:signal()
RETURN self

// Executed at end of thread.
METHOD AdsInfoThread:atend()
   LOCAL nRC

   IF ::nHandle != -1
      // Valid connection handle. Disconnect from ADS.
      nRC := AdsMgDisconnect(::nHandle)
      IF nRC != AE_SUCCESS
         AdsError(nRC, ADSMG_DISCONNECT)
      ENDIF 
   ENDIF 
RETURN self












