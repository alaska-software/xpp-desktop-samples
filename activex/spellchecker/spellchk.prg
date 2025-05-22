//////////////////////////////////////////////////////////////////////
//
//  SPELLCHK.PRG
//
//  Copyright:
//   Alaska Software, (c) 2002-2025. All rights reserved.         
//  
//  Contents:
//   
//  Remarks:
//   
//  Syntax:
//   
//  Return:
//   
//////////////////////////////////////////////////////////////////////

#include "common.ch"

#pragma library( "ascom10.lib" )

#define WRONGWORD   1
#define WORDPOS     2
#define SUGGESTIONS 3
#define CR           Chr( 13 )
#define LF           Chr( 10 )
#define CRLF         Chr( 13 ) + Chr( 10 )
#define TAB          Chr( 9 )
#define SPACE        Chr( 32 )
#define COMMA        ","
#define SEMICOLON    ";"
#define FULLSTOP     "."
#define DOUBLEPOINT  ":"

/// <summary>
/// <para>
///   Spellchecker class
/// </para>
/// </summary>
CLASS XbSpellChecker
PROTECTED
  /// <summary>
  /// <para>
  ///   The string to check for misspelled words
  /// </para>
  /// </summary>
  VAR cString

  /// <summary>
  /// <para>
  ///   Instance of a MS-Word Application
  /// </para>
  /// </summary>
  VAR oWord

  /// <summary>
  /// <para>
  ///   Array of array with three elements. First element
  ///   holds the word, second is the word number and third
  ///   elements is an array of suggestions
  /// </para>
  /// </summary>
  VAR aSuggestions

  /// <summary>
  /// <para>
  ///   Contains the character sequences that are recognized
  ///   as word delemeters
  /// </para>
  /// </summary>
  VAR aWordDelem

  /// <summary>
  /// <para>
  ///   contains numeric value of what suggestion to return next
  /// </para>
  /// </summary>
  VAR currSuggestion

  METHOD isDelemeter
  METHOD nextWord

EXPORTED
  METHOD init
  METHOD destroy
  METHOD checkSpelling
  METHOD getSuggestion

  METHOD reset
ENDCLASS


/// <summary>
/// <para>
///   Initialize the MS-Word application and
///   reset all IVars
/// </para>
/// </summary>
/// <returns>
///   self
/// </returns>
METHOD XbSpellChecker:reset( )

  IF NIL == ::oWord
    ::oWord := CreateObject( "Word.Application" )

    IF NIL == ::oWord
    MsgBox( "Cannot access Word application. Please make sure MS Word is" + Chr(13) + Chr(10) +;
            "correctly installed on your computer.", "Error" )
      BREAK()
    ENDIF

    ::oWord:Documents:Add()
  ENDIF

  ::aSuggestions := {}

  ::currSuggestion := NIL

  ::cString := ""

RETURN self

/// <summary>
/// <para>
///   Destroy all system resources, including the reference
///   to the MS Word instance
/// </para>
/// </summary>
METHOD XbSpellChecker:destroy()
  ::reset( .F. )

  ::oWord:quit()
  ::oWord:destroy()

RETURN self

/// <summary>
/// <para>
///   Checks whether the position passed delemits a word.
///   This is a helper method for <bold>:nextWord()</bold>.
/// </para>
/// </summary>
/// <returns>
///   lIsDelemiter
///   Second parameter contains the length of the delemeter found.
/// </returns>
METHOD XbSpellChecker:isDelemeter( nPos, nDelemLen )

  LOCAL nLen, cSub, n, cDelem, nStringLen
  LOCAL lDelem := .F.

  nDelemLen := 0

  // nPos beyond the string end means it is a delimeter
  nStringLen := Len( ::cString )
  IF nPos > nStringLen
    RETURN .T.
  ENDIF

  // Check the current position whether there is a
  // delimeter
  nLen := Len( ::aWordDelem )
  FOR n := 1 TO nLen

    cDelem := ::aWordDelem[n]
    nDelemLen := Len( cDelem )
    cSub := SubStr( ::cString, nPos, Len( cDelem ) )
    IF cDelem == cSub

      lDelem := .T.
      exit

    ENDIF

  NEXT

  // End of string also is a delemeter
  IF nPos == nStringLen
    lDelem := .T.
  ENDIF

RETURN lDelem

/// <summary>
/// <para>
///   Return the next word beginning from parameter nPos
/// </para>
/// </summary>
/// <returns>
///   cWord
/// </returns>
METHOD XbSpellChecker:nextWord( nFirst, nLast )

  LOCAL cWord
  LOCAL nDelemLen := NIL

  nFirst := nLast

  // increment positon until there is no delimeter
  DO WHILE ::isDelemeter( nFirst, @nDelemLen ) .AND. ;
           nFirst <= Len( ::cString )
    nFirst++
  ENDDO


  nLast := nFirst

  // Increment the string positon as long as it is
  // within the current word.
  DO WHILE ! ::isDelemeter( nLast, @nDelemLen )
    nLast++
  ENDDO

  // In case nPos is position of last character of the string
  // increment it, so that the SubStr() function call extracts
  // also the last word with all characters
  IF nLast == Len( ::cString )
    nLast++
  ENDIF

  cWord := SubStr( ::cString, nFirst, nLast - nFirst )

  // increment positon until there is no delimeter
  DO WHILE ::isDelemeter( nLast, @nDelemLen ) .AND. ;
           nLast <= Len( ::cString )
    nLast++
  ENDDO

RETURN cWord

/// <summary>
/// <para>
///   Initialization method
/// </para>
/// </summary>
/// <returns>
///   XbSpellChecker():new() -> oSpellChecker
/// </returns>
METHOD XbSpellChecker:init()

  ::aWordDelem := {CR, LF, CRLF, TAB, SPACE, ;
                   COMMA, SEMICOLON, FULLSTOP, DOUBLEPOINT }

RETURN self

/// <summary>
/// <para>
///   Checks the string passed whether it is spelled
///   correctly.
/// </para>
/// </summary>
/// <returns>
///   lCorrect
/// </returns>
METHOD XbSpellChecker:checkSpelling( cString )

  LOCAL nFirst, nLast, lCorrect, aSugg
  LOCAL cWord := NIL

  // prepare locals and IVars for next spellchecking
  ::reset()
  ::cString := cString

  lCorrect := .T.

  nFirst := 1
  nLast  := 1

  // find all word and lookup whether they are spelled
  // correct
  DO WHILE !Empty( cWord := ::nextWord( @nFirst, @nLast ) )

    IF ::oWord:checkSpelling( cWord )
      loop
    ENDIF

    // if a word is misspelled, remember the word and the
    // position of the word
    aSugg := Array( 3 )
    aSugg[WRONGWORD]   := cWord
    aSugg[WORDPOS]     := nFirst
    AAdd( ::aSuggestions, aSugg )

  ENDDO

  IF NIL != ::aSuggestions
    lCorrect := .F.
  ENDIF

RETURN lCorrect

/// <summary>
/// <para>
///   After <bold>:checkSpelling()</bold> was called,
///   <bold>:getSuggestion()</bold> returns the
///   first spelling suggestion.
/// </para>
/// </summary>
/// <returns>
///   aSuggestion {cMisspelled, nWordpos, aSuggestions }
/// </returns>
METHOD XbSpellChecker:getSuggestion()

  LOCAL aSugg, nCount, oSuggestions, n

  IF NIL == ::aSuggestions
    RETURN NIL
  ENDIF

  IF NIL == ::currSuggestion
    ::currSuggestion := 1
  ELSE
    ::currSuggestion++
  ENDIF

  IF ::currSuggestion > Len( ::aSuggestions )
    RETURN NIL
  ENDIF

  aSugg := ::aSuggestions[ ::currSuggestion ]
  aSugg[SUGGESTIONS] := {}

  // Get the suggestions for a misspelled word.
  oSuggestions := ::oWord:getSpellingSuggestions( aSugg[WRONGWORD] )

  nCount := oSuggestions:count

  FOR n := 1 TO nCount

    // insert all suggestions to a list
    AAdd( aSugg[SUGGESTIONS], oSuggestions:item( n ):name )

  NEXT

RETURN aSugg
