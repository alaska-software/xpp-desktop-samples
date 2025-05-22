////////////////////////////////////////////////////////////////////////////
//
//  README.TXT
//
//  Alaska Management Console for Advantage Database Server
//  
//  (C) Copyright 1999 Alaska Software.
//
//
// WARRANTY
//
//    THE MATERIAL EMBODIED ON THIS SOFTWARE IS PROVIDED TO YOU "AS-IS"
//    AND WITHOUT WARRANTY OF ANY KIND, EXPRESS, IMPLIED OR OTHERWISE,
//    INCLUDING WITHOUT LIMITATION, ANY WARRANTY OF MERCHANTABILITY OR
//    FITNESS FOR A PARTICULAR PURPOSE.  IN NO EVENT SHALL THE AUTHOR
//    OR ALASKA SOFTWARE BE LIABLE TO YOU OR ANYONE ELSE FOR ANY DIRECT,
//    SPECIAL, INCIDENTAL, INDIRECT OR CONSEQUENTIAL DAMAGES OF ANY KIND,
//    INCLUDING WITHOUT LIMITATION, LOSS OF PROFIT AND LOSS OF USE.
///////////////////////////////////////////////////////////////////////////



1.0 OVERVIEW
============

The Alaska ADS Management Console is a front-end for the ADS Management API 
functions. The implemented support for the ADS Management API is 
experimental; any usage of the implemented functions will be entirely at
your own risk. See the above "Warranty" section for further details.

To successfully run the ADS Management Console, your system must be
set to a screen resolution of at least 800x600 pixels. Further ahead,
the files ACE32.DLL and AXCWS32.DLL must be in your path.

The dynamic link library ADSMG.DLL provides support for the ADS Management 
API functions. To use these functions, link the library ADSMG.LIB to your 
application and ensure that the file ADSMG.DLL is in your path.

Due to the fact that the ADS Management API support is only experimental,
there is no extensive documentation provided for it. Anyway, in the
following you find some short descriptions of the supported functions,
which include the function prototypes and a brief description of what
the functions do and what parameters they expect.


Best regards,
your Alaska Xbase++ Team



2.0 MANAGEMENT API FUNCTIONS:
=============================

Each of the management API functions return an ADS error code if 
the operation was unsuccessful or AE_SUCCESS for a successful operation.

AE_WRONG_PARAMETER is returned if at least one of the passed parameters
did not fulfill the function's requirements.
If a function expects an array as a parameter, this array MUST be of
a defined type and size. The usage of empty arrays will result in
the AE_WRONG_PARAMETER return code.



2.1 FUNCTION REFERENCE:
-----------------------

FUNCTION:
   AdsMgConnect()

SYNTAX:
   AdsMgConnect( <cNetworkDrive>,
                 [<cUserName>},
                 [<cPassword>],
                 @<nHandle>) -> nRC

DESCRIPTION:
   This function establishes a management connection to the ADS
   and passes a connection handle to <nHandle>.



FUNCTION:
   AdsMgDisconnect()

SYNTAX:
   AdsMgDisconnect( <nHandle> ) -> nRC

DESCRIPTION:
   Disconnects from the ADS.



FUNCTION:
   AdsMgGetActivityInfo()

SYNTAX:
   AdsMgGetActivityInfo( <nHandle>,
                         <aActivityInfo> ) -> nRC

PARAMETERS:
   <aActivityInfo> must be an array with 13 elements.

DESCRIPTION:
   This function retrieves activity information from the ADS.



FUNCTION:
   AdsMgGetCommStats()

SYNTAX:
   AdsMgGetCommStats( <nHandle>,
                      <aCommStatsArray>) -> nRC

PARAMETERS:
   <aCommStatsArray> must have the following structure:
   aCommStatsArray := Array(11)

DESCRIPTION:
   Used to retrieve communication statistics from the database server.



FUNCTION:
   AdsMgGetConfigInfo()

SYNTAX:
   AdsMgGetConfigInfo( <nHandle>,
                       <aConfigValues>,
                       <aConfigMemory> ) -> nRC

PARAMETERS:
   <aConfigValues> must be of type Array(26) and
   <aConfigMemory> must be of type Array(13).

DESCRIPTION:
   Retrieves configuration information from the database server.



FUNCTION:
   AdsMgGetInstallInfo()

SYNTAX:
   AdsMgGetInstallInfo( <nHandle>,
                        <aInstallInfo> ) -> nRC

PARAMETERS:
   <aInstallInfo> must be an array with eight elements.

DESCRIPTION:
   This function retrieves installation information from the
   database server.



FUNCTION:
   AdsMgGetLockOwner()

SYNTAX:
   AdsMgGetLockOwner( <nHandle>,
                      <cTableName>,
                      <nRecordNumber>,
                      <aLockOwner>,
                      @<nLockType> ) -> nRC

PARAMETERS:
   <cTableName> specifies the name of the table on which the
   lock's owner shall be determined.
   <nRecordNumer> determines the number of the record that shall
   be examined.
   <aLockOwner> is an array with two elements.
   <nLockType> determines the type of the set lock.

DESCRIPTION:
   Retrieves information about the user that has set a specific
   record lock on the given table.



FUNCTION:
   AdsMgGetLocks()

SYNTAX:
   AdsMgGetLocks( <nHandle>,
                  <cTableName>,
                  [<cUserName>],
                  [<nConnNumber>],
                  <aLocks>,
                  @<nLockAmount> ) -> nRC

PARAMETERS:
   <nLockAmount> determines the max. amount of locks that shall
   be retrieved. <aLocks> is an array with <nLockAmount> elements.

DESCRIPTION:
   Retrieves a list of locks set on a given table.



FUNCTION:
   AdsMgGetOpenIndexes()

SYNTAX:
   AdsMgGetOpenIndexes( <nHandle>,
                        [<cTableName>],
                        [<cUserName>],
                        [<nConnNumber>],
                        <aOpenIndexes>,
                        @<nLengthaOpenIndexes> ) -> nRC

PARAMETERS:
   <nLengthaOpenIndexes> determines the max. amount of open
   indexes that shall be returned.
   <aOpenIndexes> is an array consisting of <nLengthaOpenIndexes>
   elements.

DESCRIPTION:
   Retrieves a list of opened indexes.



FUNCTION:
   AdsMgGetOpenTables()

SYNTAX:
   AdsMgGetOpenTables( <nHandle>, 
                       [<cUserName>], 
                       [nConnNumber], 
                       aArray, 
                       @nArrayLen) -> nRC

PARAMETERS:
   <nArrayLen> determines the max. amount of open tables
   that shall be retrieved.
   <aArray> must be an array with the size of <nArrayLen>,
   where each element consists of an array with two elements.

DESCRIPTION:
      This function retrieves a list of opened tables.



FUNCTION:
   AdsMgGetServerType()

SYNTAX:
   AdsMgGetServerType( <nHandle>,
                       @<nServerType> ) -> nRC

DESCRIPTION:
   Determines the type of the database server to which the
   passed connection handle refers.



FUNCTION:
   AdsMgGetUserNames()

SYNTAX:
   AdsMgGetUserNames( <nHandle>,
                      [<cFileName>],
                      <aUserNames>,
                      @<nAmount> ) -> nRC

PARAMETERS:
   <nAmount> specifies the max. amount of users you want to
   retrieve.
   <aUserNames> must be an array of size <nAmount>, where
   each array element itself consists of an array with two
   elements.

DESCRIPTION:
   Used to retrieve a list of connected users.




FUNCTION:
   AdsMgGetWorkerThreadActivity()

SYNTAX:
   AdsMgGetWorkerThreadActivity( <nHandle>,
                                 <aWorkerThreads>,
                                 @<nThreadAmount> ) -> nRC  

PARAMETERS:
   <nThreadAmount> determines the max. amount of worker thread
   information that shall be returned.
   <aWorkerThread> must be an array with <nThreadAmount> elements,
   where each element consists of an array with four elements.

DESCRIPTION:
   Retrieves a list of worker threads that handle user requests.



FUNCTION:
   AdsMgKillUser()

SYNTAX:
   AdsMgKillUser( <nHandle>,
                  <cUserName>,
                  [<nConnNumber>] ) -> nRC

DESCRIPTION:
   Forces the disconnection of the user specified in <cUserName>.
   NOTE: Forcing the disconnecion of a user may result in a loss
         of data and/or inconsistent databases! 




FUNCTION:
   AdsMgResetCommStats()

SYNTAX:
   AdsMgResetCommStats( <nHandle> ) -> nRC

DESCRIPTION:
   Used to reset the server's communcation statistics.



// EOF
//////  


