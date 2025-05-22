//////////////////////////////////////////////////////////////////////
//
//  NETNAME.PRG
//
//  Copyright:
//       Alaska Software, (c) 1997-2025. All rights reserved.         
//  
//  Contents:
//       Retrieves the name of the workstation
//   
//////////////////////////////////////////////////////////////////////



#include "dll.ch"

/*
 * Note: You should know the interface provided by APIs in detail. 
 *       Pay attention to:
 *       - calling convention
 *       - type of parameters
 *       - number and order of parameters
 *       - pass by value or by reference
 */

/*
 * Defines for Windows Win32 API functions
 */

#define ERROR_SUCCESS 0

#define HKEY_CLASSES_ROOT           2147483648
#define HKEY_CURRENT_USER           2147483649
#define HKEY_LOCAL_MACHINE          2147483650
#define HKEY_USERS                  2147483651

#define KEY_QUERY_VALUE         1
#define KEY_SET_VALUE           2
#define KEY_CREATE_SUB_KEY      4
#define KEY_ENUMERATE_SUB_KEYS  8
#define KEY_NOTIFY              16
#define KEY_CREATE_LINK         32

/*
 * Declarations for Windows Win32 API functions
 */

// LONG WINAPI RegOpenKeyEx(
//    HKEY    hKey,
//    LPCTSTR lpSubKey,
//    DWORD   ulOptions,
//    REGSAM  samDesired,
//    PHKEY   phkResult
// );
// 
// See: https://msdn.microsoft.com/en-us/library/windows/desktop/ms724897(v=vs.85).aspx
EXTERN LONG RegOpenKeyEx( hKey       AS UINTEGER, ;
                          lpSubKey   AS STRING,   ;
                          ulOptions  AS UINTEGER, ;
                          samDesired AS UINTEGER, ;
                          @phkResult AS UINTEGER ) IN WIN32API

// LONG WINAPI RegQueryValueEx(
//    HKEY    hKey,
//    LPCTSTR lpValueName,
//    LPDWORD lpReserved,
//    LPDWORD lpType,
//    LPBYTE  lpData,
//    LPDWORD lpcbData
// );
// 
// See: https://msdn.microsoft.com/en-us/library/windows/desktop/ms724911(v=vs.85).aspx
EXTERN LONG RegQueryValueEx( hKey        AS UINTEGER, ;
                             lpValueName AS STRING,   ;
                             lpReserved  AS INTEGER,  ;
                             @lpType     AS UINTEGER, ;
                             @lpData     AS STRING,   ;
                             @lpcbData   AS UINTEGER ) IN WIN32API

// LONG WINAPI RegCloseKey(
//    HKEY hKey
// );
// 
// See: https://msdn.microsoft.com/en-us/library/windows/desktop/ms724837(v=vs.85).aspx
EXTERN LONG RegCloseKey( hKey AS UINTEGER ) IN WIN32API

// DWORD WINAPI GetLastError(void);
// 
// See: https://msdn.microsoft.com/en-us/library/windows/desktop/ms679360(v=vs.85).aspx
EXTERN UINTEGER GetLastError() IN WIN32API

PROCEDURE MAIN
   LOCAL cNetName

   ? "NETNAME - returns the name of your workstation"
   cNetName := NetName()
   IF Empty( cNetName )
      ? "The name of your workstation has not yet been configured."
   ELSE
      ? cNetName
   ENDIF
RETURN


/// <summary>
/// Retrieves the value of a registry key
/// </summary>
/// <returns>The value as a character string, or "" on error</returns>
FUNCTION QueryRegistry( nHKEYHandle, cKeyName, cEntryName )
   LOCAL cData := ""
   LOCAL nDataSize
   LOCAL nKeyHandle
   LOCAL nValueType

   /*
    * You must assgin a value to all parameters are about to pass to a
    * dll which is not Xbase++.
    */
   nKeyHandle := 0

   // open the registry key
   IF ERROR_SUCCESS == RegOpenKeyEx( nHKEYHandle, cKeyName, 0, KEY_QUERY_VALUE, @nKeyHandle )
      ? "key is open."

      // Retrieve the value type and the length of the value
      nValueType := 0
      nDataSize  := 0
      RegQueryValueEx( nKeyHandle, cEntryName, 0, @nValueType, 0, @nDataSize )

      IF nDataSize > 0

         ? "Key value length:", nDataSize

         // Additionally, you must preserve space for strings are passed by
         // reference. The terminating zero shall be omitted.
         cData := space( nDataSize - 1 )

         // Get the value
         nValueType := 0
         IF ERROR_SUCCESS == RegQueryValueEx( nKeyHandle, cEntryName, 0, @nValueType, @cData, @nDataSize )

            ? "Key value type  :", nValueType

         ELSE

            ? "Error", GetLastError()

         ENDIF

      ENDIF

      ? "close key."

      RegCloseKey( nKeyHandle)

   ENDIF

RETURN cData

/// <summary>
/// Returns the name of the workstation the program is running on
/// </summary>
/// <returns>The name of the workstation</returns>
FUNCTION NetName()
   LOCAL nHKey      := HKEY_LOCAL_MACHINE
   LOCAL cKeyName   := "System\CurrentControlSet\Control\ComputerName\ComputerName"
   LOCAL cEntryName := "ComputerName"

RETURN QueryRegistry(nHKey, cKeyName, cEntryName)
