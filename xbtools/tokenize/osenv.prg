//////////////////////////////////////////////////////////////////////
//
//  OSENV.PRG
//
//  Copyright:
//       Alaska Software, (c) 1997-2025. All rights reserved.         
//  
//  Contents:
//       This program demonstrates the tokenizer system.
//       It displays the entire OS environment using AChoice()
//   
//////////////////////////////////////////////////////////////////////


#define CRLF   Chr(13)+Chr(10)

PROCEDURE Main
   LOCAL cEnv := EnvParam()
   LOCAL aEnvParam := {}, aEnvironment
   LOCAL cParam, cPath, aPath, cString, cDriveList
   LOCAL nPos, nRel, nMaxLen := 0
   LOCAL i, imax, j, jmax

   /* break env-string into drive list and 
    * variable list
    */
   nPos:= RAT(":=", cEnv )
   IF nPos != 0
      /* convert drive list to array */
      nRel := At( CRLF, Substr(cEnv, nPos))
      cDriveList := SubStr( cEnv, 1, nPos + nRel)
      cEnv := Substr( cEnv, nPos + nRel + 1)
      aEnvParam := SplitToArray( cDriveList, CRLF, ":=" )
   ENDIF
   /* convert variable list to array */
   aEnvironment := SplitToArray( cEnv, CRLF, "=" )

   /* copy both to one array */
   j := len( aEnvParam)
   ASize( aEnvParam, len(aEnvironment) + j )
   ACopy( aEnvironment, aEnvParam, 1, len(aEnvironment), j + 1 )
   aEnvironment := {}

   /* calculate max width of left column */
   AEval( aEnvparam, {|e,i| iif( len(e[1]) > nMaxLen, nMaxLen :=  len(e[1]), ) })

   /* sort array */
   imax := Len( aEnvParam ) - 1
   ASize( aEnvParam, imax )
   ASort( aEnvParam, , , {|a1,a2| Upper( a1[1] ) < Upper( a2[1] ) } )

   /* Parse the pattern SETTING1;SETTING2;SETTING3 and remove CRLF */
   FOR i:=1 TO imax
      cParam := Padr( aEnvParam[i,1], nMaxLen )
      cPath  := StrTran( aEnvParam[i,2], CRLF, "" )
      jmax   := NumToken( cPath, ";" )
      aPath  := Array( jmax )

      TokenInit( @cPath, ";" )
      FOR j:=1 TO jmax
         aPath[j] := TokenNext( @cPath )
      NEXT

      ASort( aPath, , ,  {|c1,c2| Upper( c1 ) < Upper( c2 ) } )
      aEnvParam[i,1] := cParam
      aEnvParam[i,2] := aPath
   NEXT

   /* Format all strings */
   FOR i:=1 TO imax
      AAdd( aEnvironment, aEnvParam[i,1] +" ³ "+aEnvParam[i,2,1] )
      jmax := Len( aEnvparam[i,2] )
      FOR j:=2 TO jmax
         AAdd( aEnvironment, Space( nMaxLen ) +" ³ " + aEnvParam[i,2,j] )
      NEXT
   NEXT

   Achoice( 0, 0, MaxRow(), MaxCol(), aEnvironment )

RETURN

/* 
 * SplitToArray( cString, cLineSep, cSideSep ) => aArray
 *
 * split <cString> for every <cLineSep> and break into 
 * left and right side on <cSideSep>
 * returns 2-dimensional array 
 */
FUNCTION SplitToArray( cString, cLineSep, cSideSep)
LOCAL aResult := {}
LOCAL nPos
LOCAL cTemp, cLeft, cRight

   
   TokenInit( @cString, cLineSep)
   DO WHILE !TokenEnd()
      cTemp  := TokenNext( @cString )
      nPos   := Rat( cSideSep, cTemp )
      IF nPos != 0
         cLeft  := Left( cTemp, nPos-1 )
         cRight := Substr( cTemp, nPos + len(cSideSep))
      ENDIF
      AAdd( aResult, {cLeft, cRight})
   ENDDO
   TokenInit()

RETURN aResult
