//////////////////////////////////////////////////////////////////////
//
//  EXTERN.PRG
//
//  Copyright:
//      Alaska Software, (c) 1998-2025. All rights reserved.         
//  
//  Contents:
//      how to use external dlls
//   
//////////////////////////////////////////////////////////////////////



#include "dll.ch"

/*

   external DLL calling methods

   Please follow these guidelines if you plan to use functions from
   external DLL's:

   1. Check whether the calling convention of the function you want to
      class is supported by Xbase++'s EXTERN command. The calling 
      convention for Win32 API functions is STDCALL. Note: 64 Bit DLLs 
      are not supported!
   2. All external functions have a strong typed interface. By contrast,
      variables in Xbase++ are dynamic typed and the the only moment a 
      type is fixed is at the time a value is assignment to the variable. 
      Therefore, please make sure that all variables you intend to pass 
      on to an external function has the proper value (type) assigned,
      and does not change its value during the call.
   3. If strings are returned by an external function as out parameters, 
      you have to allocate enough space for the strings before the call
      (using SPACE(xx)).
   4. Declare a reference parameter (@) if the external function expects 
      a pointer.
*/

/*
 * Defines for Windows Win32 API functions
 */

// Flags from GetVolumeInformation
//
// See: https://msdn.microsoft.com/en-us/library/windows/desktop/aa364993(v=vs.85).aspx
#define FILE_CASE_PRESERVED_NAMES         0x00000002
#define FILE_CASE_SENSITIVE_SEARCH        0x00000001
#define FILE_DAX_VOLUME                   0x20000000
#define FILE_FILE_COMPRESSION             0x00000010
#define FILE_NAMED_STREAMS                0x00040000
#define FILE_PERSISTENT_ACLS              0x00000008
#define FILE_READ_ONLY_VOLUME             0x00080000
#define FILE_SEQUENTIAL_WRITE_ONCE        0x00100000
#define FILE_SUPPORTS_ENCRYPTION          0x00020000
#define FILE_SUPPORTS_EXTENDED_ATTRIBUTES 0x00800000
#define FILE_SUPPORTS_HARD_LINKS          0x00400000
#define FILE_SUPPORTS_OBJECT_IDS          0x00010000
#define FILE_SUPPORTS_OPEN_BY_FILE_ID     0x01000000
#define FILE_SUPPORTS_REPARSE_POINTS      0x00000080
#define FILE_SUPPORTS_SPARSE_FILES        0x00000040
#define FILE_SUPPORTS_TRANSACTIONS        0x00200000
#define FILE_SUPPORTS_USN_JOURNAL         0x02000000
#define FILE_UNICODE_ON_DISK              0x00000004
#define FILE_VOLUME_IS_COMPRESSED         0x00008000
#define FILE_VOLUME_QUOTAS                0x00000020

/*
 * Declarations for Windows Win32 API functions
 */


// BOOL WINAPI GetVolumeInformation(
//    LPCTSTR lpRootPathName,
//    LPTSTR  lpVolumeNameBuffer,
//    DWORD   nVolumeNameSize,
//    LPDWORD lpVolumeSerialNumber,
//    LPDWORD lpMaximumComponentLength,
//    LPDWORD lpFileSystemFlags,
//    LPTSTR  lpFileSystemNameBuffer,
//    DWORD   nFileSystemNameSize
// );
//
// See: https://msdn.microsoft.com/en-us/library/windows/desktop/aa364993(v=vs.85).aspx
EXTERN BOOL GetVolumeInformation( lpRootPathName            AS STRING,   ;
                                  @lpVolumeNameBuffer       AS STRING,   ;
                                  nVolumeNameSize           AS UINTEGER, ;
                                  @lpVolumeSerialNumber     AS UINTEGER, ;
                                  @lpMaximumComponentLength AS UINTEGER, ;
                                  @lpFileSystemFlags        AS UINTEGER, ;
                                  @lpFileSystemNameBuffer   AS STRING,   ;
                                  nFileSystemNameSize       AS UINTEGER ) IN WIN32API


// DWORD WINAPI GetLastError(void);
// 
// See: https://msdn.microsoft.com/en-us/library/windows/desktop/ms679360(v=vs.85).aspx
EXTERN UINTEGER GetLastError() IN WIN32API


PROCEDURE MAIN( cVolname )
   LOCAL oVolumeInfo
   LOCAL nFlags

   SET CHARSET TO ANSI

    ? "EXTERN [drive:] retrieve volume information"

   IF cVolName == NIL
       cVolName := CurDrive()
   ENDIF

   oVolumeInfo := SysGetVolumeInformation( cVolName )

   IF .NOT. Empty( oVolumeInfo )

      nFlags := oVolumeInfo:Flags

      ? "Drive     :", oVolumeInfo:Root
      ? "Label     :", oVolumeInfo:Name
      ? "Serial no.:", oVolumeInfo:Serial
      ? "FS type   :", oVolumeInfo:FSName
      ? "Flag      :", "FILE_CASE_PRESERVED_NAMES        :", TestFlag( nFlags, FILE_CASE_PRESERVED_NAMES )
      ? "            FILE_CASE_SENSITIVE_SEARCH       :",     TestFlag( nFlags, FILE_CASE_SENSITIVE_SEARCH )
      ? "            FILE_DAX_VOLUME                  :",     TestFlag( nFlags, FILE_DAX_VOLUME )
      ? "            FILE_FILE_COMPRESSION            :",     TestFlag( nFlags, FILE_FILE_COMPRESSION )
      ? "            FILE_NAMED_STREAMS               :",     TestFlag( nFlags, FILE_NAMED_STREAMS )
      ? "            FILE_PERSISTENT_ACLS             :",     TestFlag( nFlags, FILE_PERSISTENT_ACLS )
      ? "            FILE_READ_ONLY_VOLUME            :",     TestFlag( nFlags, FILE_READ_ONLY_VOLUME )
      ? "            FILE_SEQUENTIAL_WRITE_ONCE       :",     TestFlag( nFlags, FILE_SEQUENTIAL_WRITE_ONCE )
      ? "            FILE_SUPPORTS_ENCRYPTION         :",     TestFlag( nFlags, FILE_SUPPORTS_ENCRYPTION )
      ? "            FILE_SUPPORTS_EXTENDED_ATTRIBUTES:",     TestFlag( nFlags, FILE_SUPPORTS_EXTENDED_ATTRIBUTES )
      ? "            FILE_SUPPORTS_HARD_LINKS         :",     TestFlag( nFlags, FILE_SUPPORTS_HARD_LINKS )
      ? "            FILE_SUPPORTS_OBJECT_IDS         :",     TestFlag( nFlags, FILE_SUPPORTS_OBJECT_IDS )
      ? "            FILE_SUPPORTS_OPEN_BY_FILE_ID    :",     TestFlag( nFlags, FILE_SUPPORTS_OPEN_BY_FILE_ID )
      ? "            FILE_SUPPORTS_REPARSE_POINTS     :",     TestFlag( nFlags, FILE_SUPPORTS_REPARSE_POINTS )
      ? "            FILE_SUPPORTS_SPARSE_FILES       :",     TestFlag( nFlags, FILE_SUPPORTS_SPARSE_FILES )
      ? "            FILE_SUPPORTS_TRANSACTIONS       :",     TestFlag( nFlags, FILE_SUPPORTS_TRANSACTIONS )
      ? "            FILE_SUPPORTS_USN_JOURNAL        :",     TestFlag( nFlags, FILE_SUPPORTS_USN_JOURNAL )
      ? "            FILE_UNICODE_ON_DISKc            :",     TestFlag( nFlags, FILE_UNICODE_ON_DISK )
      ? "            FILE_VOLUME_IS_COMPRESSED        :",     TestFlag( nFlags, FILE_VOLUME_IS_COMPRESSED )
      ? "            FILE_VOLUME_QUOTAS               :",     TestFlag( nFlags, FILE_VOLUME_QUOTAS )

   ELSE

       ? "Error with drive " + cVolName, SysGetLastError()

   ENDIF
   
RETURN

/// <summary>Test for a flag in a value</summary>
/// <returns>.T. if the flag is set, .F. otherwise</returns>
FUNCTION TestFlag( nValue, nFlag )
RETURN( nFlag == BAnd(nValue, nFlag) )

/// <summary>
/// Retrieve some information about a volume
/// </summary>
/// <param name="cVolRoot">Drive letter</param>
/// <returns>A DataObject with the member variables :root, :name, :serial, :flags and :FSName, NIL on error</returns>
FUNCTION SysGetVolumeInformation( cVolRoot )
    LOCAL cVolName
    LOCAL nVolSerial
    LOCAL nMaxNameLen
    LOCAL nFSFlags
    LOCAL cFSName
    LOCAL lVolInfo
    LOCAL oVolumeInfo

    IF ! ":" $ cVolRoot
        cVolRoot += ":"
    ENDIF
    IF ! "\" $ cVolRoot
        cVolRoot += "\"
    ENDIF

    /*
     * Parameter initialization and call of the Win32 API function
     */

    cVolName    := Space(128)
    cFSName     := Space(128)
    nVolSerial  := 0
    nMaxNameLen := 0
    nFSFlags    := 0

    lVolInfo := GetVolumeInformation( cVolRoot,      ;
                                      @cVolName,     ;
                                      Len(cVolName), ;
                                      @nVolSerial,   ;
                                      @nMaxNameLen,  ;
                                      @nFSFlags,     ;
                                      @cFSName,      ;
                                      Len(cFSName) )

    /*
     * Initialize return value 
     */

    IF .NOT. lVolInfo
       RETURN NIL
    ENDIF

    oVolumeInfo := DataObject():new()
    oVolumeInfo:Root   := cVolRoot
    oVolumeInfo:Name   := ZTrim( cVolName )
    oVolumeInfo:Serial := nVolSerial
    oVolumeInfo:Flags  := nFSFlags
    oVolumeInfo:FSName := ZTrim( cFSName )

RETURN oVolumeInfo


/// <summary>
/// Retrieve the error code of the last operating system error
/// </summary>
/// <returns>The operating system error code</returns>
/// <remarks>Provides access to the GetLastError() API using an
/// application/framework-compatible prefixed name</remarks>
FUNCTION SysGetLastError()
RETURN GetLastError()


/// <summary>
/// Cut all characters in string as soon as a Chr(0) is found.
/// </summary>
/// <param name="cString">A character string</param>
/// <returns>cNewString</returns>
/// <remarks>Most API's are creating zero-terminated strings. 
/// Use this function to cut the string to it's correct length 
/// after the API-call<remarks>
FUNCTION ZTrim(cString)
   LOCAL nZPos

   nZPos := At( Chr(0), cString )

   IF nZPos > 0
      cString := Substr( cString, 1, nZPos - 1 )
   ENDIF

RETURN cString

