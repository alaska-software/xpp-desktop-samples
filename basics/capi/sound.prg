//////////////////////////////////////////////////////////////////////
//
//  SOUND.PRG
//
//  Copyright:
//      Alaska Software, (c) 1997-2025. All rights reserved.         
//  
//  Contents:
//      calling the C-functions SOUNDEX() and DIFFERENCE()
//   
//  Syntax:
//      SOUND word1 [word..]
//   
//////////////////////////////////////////////////////////////////////

#include "soundex.ch"


/*
 * Note: If you use C-API functions in macros, you must insert a REQUEST
 * to that symbol that is generated by the C-compiler. In this case,
 * you have to use the symbol mangled with the underscore.
 */
REQUEST _SOUNDEX, _DIFFERENCE


/*
 * prints out the value, its soundex and difference of
 * every command line parameter
 */

MEMVAR nMode

PROCEDURE MAIN()
Local i:=0
Local cSoundex,cDiff

   ? "Sample program to demonstrate the calling of SOUNDEX() and DIFFERENCE()."
   ? ""
   IF PCOUNT()==0
      ? "SOUND: [word] [word] .."
      ? "      Get the soundex code and the difference, if the words are given in pairs."
      ? "      A soundex is used to search for phonetical matches."
      ? "      The difference between two soundexes expresses the ranking of"
      ? "      correspondence, 0 means totally different."
      ? ""
      RETURN
   ENDIF
   nMode:=SOUNDEX_CLIPPER

   ? "compatibility mode: (nMode) = " + str(nMode)
   DO WHILE i++ < PCOUNT()                      // for every parameter
      cSoundex := '_SOUNDEX("'+PVALUE(i)+'",nMode)'
      ? cSoundex, &cSoundex                     // show call and evaluate
   ENDDO
   ? ""
   IF PCOUNT() % 2 == 0                         // if params are in pairs
      FOR i:=1 TO PCOUNT() STEP 2
         cDiff := '_DIFFERENCE("'+PVALUE(i)+'","'+PVALUE(i+1)+'",nMode)'
         ? cDiff,&cDiff                         // calculate the difference
      NEXT
   ENDIF
   ? ""
RETURN




