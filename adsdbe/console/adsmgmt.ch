//////////////////////////////////////////////////////////////////////
//
//  ADSMGMT.CH
//
//  Copyright:
//   Alaska Software, (c) 1999-2025. All rights reserved.         
//  
//  Contents:
//       Alaska Management Console for the Advantage Database Server
//   
//       Defines constants for the Alaska ADS Management Console
//////////////////////////////////////////////////////////////////////


#ifndef __ADSMGMTCH__
#define __ADSMGMTCH__

//ADS errors.
#define AE_SUCCESS                     0
#define AE_WRONG_PARAMETER            -1

//Max. size of arrays used to retrieve ADS information.
#define ADS_MAX_INFO 1000

//Server IDs.
#define ADS_MGMT_NETWARE_SERVER 1
#define ADS_MGMT_NT_SERVER      2
#define ADS_MGMT_LOCAL_SERVER   3

//Locking modes.
#define ADS_MGMT_PROPRIETARY_LOCKING   1
#define ADS_MGMT_CDX_LOCKING           2
#define ADS_MGMT_NTX_LOCKING           3
#define ADS_MGMT_ADT_LOCKING           4

//Lock types.
#define ADS_MGMT_NO_LOCK         1
#define ADS_MGMT_RECORD_LOCK     2
#define ADS_MGMT_FILE_LOCK       3


//ID of ADS Management functions.
#define ADSMG_CONNECT                  1
#define ADSMG_DISCONNECT               2
#define ADSMG_GETWORKERTHREADACTIVITY  3
#define ADSMG_GETOPENTABLES            4
#define ADSMG_GETCOMMSTATS             5
#define ADSMG_GETUSERNAMES             6
#define ADSMG_GETLOCKOWNER             7
#define ADSMG_GETOPENINDEXES           8
#define ADSMG_RESETCOMMSTATS           9
#define ADSMG_GETACTIVITYINFO         10
#define ADSMG_GETLOCKS                11
#define ADSMG_KILLUSER                12
#define ADSMG_GETINSTALLINFO          13
#define ADSMG_GETSERVERTYPE           14
#define ADSMG_GETCONFIGINFO           15

#ENDIF 

