//////////////////////////////////////////////////////////////////////
//
//  PLAYSND.PRG
//
//  Copyright:
//       Alaska Software, (c) 1997-2025. All rights reserved.         
//  
//  Contents:
//       Play system sounds via the dll interface
//   
//////////////////////////////////////////////////////////////////////



#include "inkey.ch"
#include "directry.ch"
#include "dll.ch"

/*
 * Defines for Windows Win32 API functions
 */

// flags for PlaySound
#define SND_SYNC            0
#define SND_ASYNC           1
#define SND_FILENAME        131072
#define SND_PURGE           64

// flags for MessageBeep
#define MB_ICONHAND         16
#define MB_OK               0
#define MB_DEFAULT          -1

/*
 * Declarations for Windows Win32 API functions
 */

// BOOL WINAPI MessageBeep(
//    UINT uType
// );
// 
// See: https://msdn.microsoft.com/en-us/library/windows/desktop/ms680356(v=vs.85).aspx
EXTERN INTEGER MessageBeep( uType AS UINTEGER ) IN WIN32API

// UINT waveOutGetNumDevs(void);
// 
// See: https://msdn.microsoft.com/en-us/library/dd743860(v=vs.85).aspx
EXTERN UINTEGER waveOutGetNumDevs() IN winmm.dll

// BOOL PlaySound(
//    LPCTSTR pszSound,
//    HMODULE hmod,
//    DWORD   fdwSound
// );
// 
// See: https://msdn.microsoft.com/en-us/library/dd743680(v=vs.85).aspx
EXTERN BOOL PlaySound( pszSound AS STRING,   ;
                       hmod     AS UINTEGER, ;
                       fdwSound AS UINTEGER ) IN winmm.dll


PROCEDURE MAIN()
   LOCAL cLookupDir
   LOCAL aPlayField
   LOCAL lRun
   LOCAL nSelect
   LOCAL aSize

   SET CHARSET TO ANSI

   CLS
   cLookupDir := GetEnv( "WINDIR") + "\MEDIA"

   @ 1, 2 SAY "PLAYSND - play all the system sounds. Press Esc to quit."
   @ 3, 2 SAY "Available sounds from " + cLookupDir + ":"

   // Get all files from the default sound directory
   aPlayField := Directory( cLookupDir + "\*.wav")

   IF len(aPlayField) = 0
      MessageBeep( MB_ICONHAND)
      Alert( "Error: No wave files found in "+ cLookupDir )
      QUIT
   ENDIF

   AEval( aPlayField, {|x,i| x := x[F_NAME]},,,.T.)
   aPlayField := ASort( aPlayField )

   lRun := .T.
   nSelect := 1

   IF waveOutGetNumDevs() = 0
      MessageBeep( MB_ICONHAND)
      Alert( "Error: No waveform output devices found!" )
   ENDIF

   SetMouse(.T.)
   SetColor("N/G,N/W")
   aSize := SetAppWindow():setMode()

   DO WHILE lRun
      nSelect := AChoice( 5,2,aSize[1]-4,40, aPlayField,,,nSelect, 8)
      IF nSelect > 0
         PlayWaveFile( cLookupDir + "\" + aPlayField[nSelect])
      ELSE
         IF LASTKEY()==K_ESC
            lRun := .F.
         ENDIF
      ENDIF
   ENDDO

   SetColor("W/N/,W/N")
   CLS

RETURN

/// <summary>
/// Stops sound output.
/// </summary>
PROCEDURE StopSound()
   // Stops waveform sound
   PlaySound( 0, 0, 0 )
   // Stops non-waveform sound
   PlaySound( 0, 0, SND_PURGE )
RETURN

/// <summary>
/// Play a given sound file asynchronously
/// </summary>
/// <param name="cFileName">The sound file to play</param>
PROCEDURE PlayWaveFile(cFilename)

   StopSound()
   PlaySound( cFileName, 0, SND_FILENAME + SND_ASYNC )

RETURN
