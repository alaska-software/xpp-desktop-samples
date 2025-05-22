//////////////////////////////////////////////////////////////////////
//
//  LOGIN_T.PRG
//
//  Copyright:
//     Alaska Software, (c) 1997-2025. All rights reserved.         
//  
//  Contents:
//     Simple login routine for text mode. This file is the starting point
//     for a small example that shows the migration of Clipper code. See
//     LOGIN_H.PRG (Hybrid) and LOGIN_G.PRG (GUI) to get an idea about
//     necessary code changes.
//   
//////////////////////////////////////////////////////////////////////


#include "Inkey.ch"

PROCEDURE Main
   LOCAL cPassWord, cUserID

   SET SCOREBOARD OFF
   SET CURSOR OFF
   SetBlink( .F. )
   DisplayLogo()

   cUserID   := Space(15)
   cPassWord := Space(15)

   PassWord( @cUserID, @cPassWord )

   DisplayLogo()

   IF cPassWord == "ALASKA"
      Alert( "Password is correct;Access granted", {"OK"}, "N/BG,W+/G" )
   ELSE
      Alert( "Password is wrong, ;" + ;
             "Access denied!"    , {"OK"} )
   ENDIF

RETURN


/*
 * Display a company logo
 */
FUNCTION DisplayLogo
   LOCAL cColor := SetColor("B/W*")
   CLS

   @  1, 0 SAY "       ÛÛÛ°      ÛÛÛ°           ÛÛÛ°         ÛÛÛ°    ÛÛÛ°   Û°       ÛÛÛ°       "
   @  2, 0 SAY "      ÛÛ° ÛÛ°      Û°          ÛÛ° ÛÛ°      Û°   Û°   Û°  ÛÛ°       ÛÛ° ÛÛ°     "
   @  3, 0 SAY "     ÛÛ°   ÛÛ°     Û°         ÛÛ°   ÛÛ°    Û°         Û° ÛÛ°       ÛÛ°   ÛÛ°    "
   @  4, 0 SAY "    ÛÛ°     ÛÛ°    Û°        ÛÛ°     ÛÛ°    Û°        Û°ÛÛ°       ÛÛ°     ÛÛ°   "
   @  5, 0 SAY "    Û° ÛÛÛÛÛ Û°    Û°        Û° ÛÛÛÛÛ Û°     ÛÛÛ°     ÛÛ°         Û° ÛÛÛÛÛ Û°   "
   @  6, 0 SAY "    Û°       Û°    Û°        Û°       Û°        Û°    Û°ÛÛ°       Û°       Û°   "
   @  7, 0 SAY "    Û°       Û°    Û°        Û°       Û°         Û°   Û° ÛÛ°      Û°       Û°   "
   @  8, 0 SAY "    Û°       Û°    Û°   Û°   Û°       Û°   Û°   Û°    Û°  ÛÛ°     Û°       Û°   "
   @  9, 0 SAY "   ÛÛÛ°     ÛÛÛ°  ÛÛÛÛÛÛÛ°  ÛÛÛ°     ÛÛÛ°   ÛÛÛÛ°    ÛÛÛ°  ÛÛ°   ÛÛÛ°     ÛÛÛ°  "
   @ 10, 0 SAY "                                                                                "
   @ 11, 0 SAY "   ÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛ   "

   SetColor( cColor )
RETURN NIL



/*
 * Login routine for entering pass word and user ID
 */
PROCEDURE PassWord( cUserID, cPassWord )
   LOCAL GetList := {}

   @ 13, 25 CLEAR TO 21, 55
   @ 13, 25 TO 21, 55 DOUBLE

   SET CURSOR ON
   @ 15, 27 SAY "Enter your ID and password!"
   @ 17, 28 SAY " User ID:" GET cUserID
   @ 19, 28 SAY "Password:" GET cPassWord SEND reader := {|o| HiddenReader(o) }
   READ
   SET CURSOR OFF

   cUserID   := AllTrim( Upper( cUserID ) )
   cPassWord := AllTrim( Upper( cPassWord ) )
RETURN



/*
 * Get reader for hidden input of a string
 */
PROCEDURE HiddenReader( oGet )
   LOCAL cBuffer, cChar, nKey

   IF GetPreValidate( oGet )
      oGet:setFocus()

      cBuffer := oGet:buffer
      cChar   := Replicate( "*", Len(Trim(cBuffer)) )
      oGet:varPut( PadR(cChar, Len(cBuffer)) )
      oGet:updateBuffer()

      DO WHILE oGet:exitState == GE_NOEXIT

         IF oGet:typeOut
            oGet:exitState := GE_ENTER
         ENDIF

         DO WHILE oGet:exitState == GE_NOEXIT
            nKey := Inkey(0)

            DO CASE
            CASE nKey == K_DEL .OR. nKey == K_BS
               GetApplykey( oGet, nKey )
               cBuffer := Stuff( cBuffer, oGet:pos, 1, "" )

            CASE nKey >= 32 .AND. nKey <= 255
               GetApplykey( oGet, Asc( "*" ) )
               cChar:= Chr(nKey)

               IF oGet:typeOut
                  cBuffer := Stuff( cBuffer, oGet:pos, 1, cChar )
               ELSEIF Set( _SET_INSERT )
                  cBuffer := Stuff( cBuffer, oGet:pos-1, 0, cChar )
               ELSE
                  cBuffer := Stuff( cBuffer, oGet:pos-1, 1, cChar )
               ENDIF

            OTHERWISE
               GetApplykey( oGet, nKey )
            ENDCASE
         ENDDO

         IF ! GetPostValidate( oGet )
            oGet:exitState:= GE_NOEXIT
         ENDIF
      ENDDO

      oGet:killFocus()

      oGet:varPut( cBuffer )
   ENDIF

RETURN
