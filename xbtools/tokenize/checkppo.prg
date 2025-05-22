//////////////////////////////////////////////////////////////////////
//
//  CHECKPPO.PRG
//
//  Copyright:
//      Alaska Software, (c) 1997-2025. All rights reserved.         
//  
//  Contents:
//      This program demonstrates the tokenizer system with nested
//      calls to TokenInit().
//   
//  Remarks:
//      It is useful for analysis of PPO files that contain very long
//      lines with continuation characters. Each line that contains ";"
//      is split into separate lines. The result is displayed with AChoice().
//   
//////////////////////////////////////////////////////////////////////


#define CRLF   Chr(13)+Chr(10)

PROCEDURE Main( cPpoFile )
   LOCAL nLines := 1000
   LOCAL aLines[ nLines ]
   LOCAL cPpo, cLine, cSave, nCount

   IF Empty( cPpoFile )
      cPpoFile := "osenv.ppo"
   ENDIF

   IF Empty( cPpo := MemoRead( cPpoFile ) )
      ? "PPO file parser parses line continuation characters"
      ? "in a preprocessed output file and displays each line"
      ? "as a single line for syntax error checks."
      ?
      ? "Please specify a PPO file on the command line, like:"
      ? 
      ? "CHECKPPO osenv.ppo"
      QUIT
   ENDIF

   nCount := 1
   TokenInit( @cPpo, CRLF )            // Parse CRLF
   DO WHILE ! TokenEnd()
      cLine := TokenNext( @cPpo )
      cSave := SaveToken()             // Save tokenizer env.
      TokenInit( @cLine, ";" )         // Parse ";"

      DO WHILE ! TokenEnd()
         aLines[ nCount ] := TokenNext( @cLine )
         nCount ++
         IF nCount > nLines
            nLines += 1000
            ASize( aLines, nLines )
         ENDIF
      ENDDO

      RestToken( cSave )               // Restore tokenizer env.
   ENDDO

   ASize( aLines, nCount )

   CLS
   ?? " Preprocessed Output file:", cPpoFile

   @ 1, 0 TO MaxRow(), MaxCol() DOUBLE

   AChoice( 2, 1, MaxRow()-1, MaxCol()-1, aLines )
   CLS
RETURN
