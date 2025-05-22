//////////////////////////////////////////////////////////////////////
//
//  LOADGRA.PRG
//
//  Copyright:
//      Alaska Software, (c) 1997-2025. All rights reserved.         
//  
//  Contents:
//      This program loads the array which was created and saved
//      by the "SAVEGRA.EXE" sample and executes the code blocks 
//      contained in the array elements
//   
//  Remarks:
//       This sample must be linked with /PMTYPE:PM.
//   
//////////////////////////////////////////////////////////////////////

/*
 * Include files
 */
#include "appevent.ch"


/* Explicitly request functions needed to execute */
/* the loaded codeblocks                          */
REQUEST GraSetColor, GraSetFont         
                                        
MEMVAR aDrawCodeblocks


*******************************************************************************
/*
 * Main procedure
 */
*******************************************************************************
PROCEDURE Main( cInputFile )

   LOCAL   i, nEvent
   /* Name of saved PRIVATE variable */
   PRIVATE aDrawCodeblocks                


   /*
    * Check parameters passed to the program. If no
    * file name was passed, try to find the default
    * file "test.xpf"
    */
   IF Valtype(cInputFile) != 'C' 
      IF File("test.xpf")
         cInputFile := "test.xpf"
      ELSE
         Alert( "USAGE: loadgra <sourceFilename>" )
         RETURN
      ENDIF
   ENDIF

   /*
    * Load saved code blocks and execute them
    */
   RESTORE FROM (cInputFile) ADDITIVE     

   IF Type( "aDrawCodeblocks" ) == "A"
      FOR i := 1 TO Len( aDrawCodeblocks )
         /* Execute codeblock */
         Eval( aDrawCodeblocks[i] )       
      NEXT
   ENDIF

   /*
    * Display status information 
    */
   SetColor( "W/B" )   
   @ MaxRow()-2,  0 CLEAR
   @ MaxRow()-2, 27 SAY "LOADGRA Sample Application"
   @ MaxRow()-1,  5 SAY "The picture has been restored using the GRA" + ;
                        " primitives stored in the"
   @ MaxRow(),    5 SAY "file specified."

   /* 
    * Event loop; terminates on key press or
    * selection of the window's close button
    */
   DO WHILE .T.                           
      nEvent := AppEvent ()               
      IF ( nEvent <= 255 .OR. nEvent == xbeP_Close )
         EXIT
      ENDIF
   ENDDO

RETURN
