//////////////////////////////////////////////////////////////////////
//
//  DBGET1.PRG
//
//  Copyright:
//      Alaska Software, (c) 1997-2025. All rights reserved.         
//  
//  Contents:
//      First migration step of an application: source code remains unchanged.
//   
//      Standard GET mask on a database. Navigation is performed with
//      SETKEY commands.
//   
//////////////////////////////////////////////////////////////////////


#include "inkey.ch"

PROCEDURE Main
   LOCAL GetList:={}, lExit

   SET DEFAULT TO "..\..\data\misc"

   USE dgetcust EXCLUSIVE
   SET DELETED ON
   GO TOP
   
   SET(_SET_SCOREBOARD,.F.)

   SETCOLOR ( "N/W, W+/B, B/W, R/W, W+/N" )
   CLS

   /*
    * Creation of the GET mask
    */
   @  0, 0 TO 24,79 DOUBLE
   @  0, 1 SAY PADC(" Change Customer ", 76, CHR(205))

   @  1, 1 TO 8,78
   @  2, 7 SAY "c/o:"         GET  MR_MRS
   @  2,14 SAY "(1=Mr. 2=Mrs. 3=Miss 4=Sirs)"

   @  3, 5 SAY "Name1:"       GET  NAME1
   @  4, 5 SAY "Name2:"       GET  NAME2
   @  5, 4 SAY "Street:"      GET  STREET
   @  6, 7 SAY "Zip:"         GET  ZIP
   @  7, 6 SAY "City:"        GET  CITY
   @  2,53 SAY "CustNo:"      GET  CUSTNO
   @  3,54 SAY "Phone:"       GET  PHONE
   @  4,56 SAY "Fax:"         GET  FAX
   @  6,49 SAY "BusinessID:"  GET  BUSINESSID
   @  9, 1 TO 13,78
   @ 10, 3 SAY "Remarks:"     GET  REMARK1
   @ 11,12                    GET  REMARK2
   @ 12,12                    GET  REMARK3

   @ 14, 1 TO 16,78
   @ 15, 5 SAY "First:"       GET  FIRSTDATE
   @ 15,23 SAY "Last:"        GET  LASTDATE
   @ 15,39 SAY "Responsible:" GET  KEYPERSON

   @ 18   ,3     SAY "F1"        COLOR  "B+/W"
   @ ROW(),COL() SAY "-Help"     COLOR  "N/W"
   SETKEY ( K_F1, {|| TONE ( 1000 ), GetHelp() } )

   @ 18,15       SAY "F2"        COLOR  "B+/W"
   @ ROW(),COL() SAY "-Next"     COLOR  "N/W"
   SETKEY ( K_F2, {|| DBSKIP(1), GetRefresh(GetList) } )

   @ 18,27       SAY "F3"        COLOR  "B+/W"
   @ ROW(),COL() SAY "-Previous" COLOR  "N/W"
   SETKEY ( K_F3, {|| DBSKIP(-1), GetRefresh(GetList) } )

   @ 18,43       SAY "F4"        COLOR  "B+/W"
   @ ROW(),COL() SAY "-New"      COLOR  "N/W"
   SETKEY ( K_F4, {|| DBAPPEND(), GetRefresh(GetList) } )

   @ 18,54       SAY "F5"        COLOR  "B+/W"
   @ ROW(),COL() SAY "-Delete"   COLOR  "N/W"
   SETKEY ( K_F5, {|| DBDELETE(), GetRefresh(GetList) } )

   @ 18,68       SAY "F6"        COLOR  "B+/W"
   @ ROW(),COL() SAY "-Exit"     COLOR  "N/W"
   SETKEY ( K_F6, {|| GetActive():ExitState := GE_WRITE, lExit:=.T. } )

   @ 20, 0 SAY "Ì" + REPLICATE("Í",78) + "¹"

   COLORSELECT(3)
   @ 21, 9 SAY "Simple Get entry screen programmed in procedural style using"
   @ 23, 9 SAY "@ SAY...GET and SETKEY()"
   COLORSELECT(0)

   lExit := .F.

   DO WHILE ! lExit
      READ SAVE
      IF LASTKEY()     == K_PGDN
         DBSKIP(1)
      ELSEIF LASTKEY() == K_PGUP
         DBSKIP(-1)
      ENDIF
      GetRefresh(GetList)
   ENDDO
   CLS
   USE
RETURN

/*
 * GetHelp() called by K_F1
 */
PROCEDURE GetHelp()
   LOCAL cScreen, nRow := Row(), nCol := Col(), x
   LOCAL aKeyBlocks := { SETKEY ( K_F2, NIL ),  ;
                         SETKEY ( K_F3, NIL ),  ;
                         SETKEY ( K_F4, NIL ),  ;
                         SETKEY ( K_F5, NIL ),  ;
                         SETKEY ( K_F6, NIL )}

   cScreen := SaveScreen(0,0,MaxRow(),MaxCol())
   CLS
   ?
   ? "This is assumed to be a Help-Screen"
   ?
   ? "The current field is:", READVAR()
   ?
   WAIT

   FOR x := 1 TO LEN( aKeyBlocks )
      SETKEY( K_F2 - x +1, aKeyBlocks[ x ])
   NEXT x

   RestScreen(0,0,MaxRow(),MaxCol(),cScreen)

   SetPos( nRow, nCol )
RETURN

/*
 * Refresh all current Gets in GetList-array.
 */
PROCEDURE GetRefresh( GetList )
   LOCAL i, imax := Len(GetList)
   LOCAL oGet, nRow := Row(), nCol := Col()

   SET CURSOR OFF

   FOR i:=1 TO imax
      oGet := GetList[i]
      oGet:updateBuffer()
   NEXT
   SET CURSOR ON
   SetPos( nRow, nCol )

RETURN
