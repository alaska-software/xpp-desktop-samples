//////////////////////////////////////////////////////////////////////
//
//  DBGET2.PRG
//
//  Copyright:
//      Alaska Software, (c) 1997-2025. All rights reserved.         
//  
//  Contents:
//      Standard Get screen for a database combined with pushbuttons.
//   
//      This file must be linked in windows mode (/SUBSYSTEM:WINDOWS
//      or /PM:PM) since it uses graphical pushbutton objects.
//   
//////////////////////////////////////////////////////////////////////


#include "Getexit.ch"
#include "AppEvent.ch"
#include "SetCurs.ch"

/*
 * Position and height for pushbuttons.
 */
#define B_TOP     90
#define B_HEIGHT  30

****************
PROCEDURE Main()

   LOCAL GetList:={}, lExit

   SetMouse(.T.)
   SetCursor( SC_NORMAL )
   SetAppWindow():AutoFocus := .F.
   SetAppWindow():AutoMark  := .F.

   SET DEFAULT TO "..\..\data\misc"
   USE dgetcust EXCLUSIVE
   SET DELETED ON
   GO TOP
   
   SET SCOREBOARD OFF

   SetColor( "N/W, W+/B, B/W, R/W, W+/N" )
   CLS

   @  0, 0 TO 24,79 DOUBLE
   /*
    * Creation of the GET mask
    */
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

   /*
    * Create pushbuttons (they are automatically added to the
    * child list of the current SetAppWindow() object -> XbpCrt window)
    */
   PushButton ( { 25, B_TOP }, { 50, B_HEIGHT }, "Help",      ;
                       {|| GetHelp() } )

   PushButton ( { 120, B_TOP }, { 50, B_HEIGHT }, "Next",     ;
                {|| GetUpdate(), DbSkip(1), GetRefresh(GetList) } )

   PushButton ( { 220, B_TOP }, { 80, B_HEIGHT }, "Previous", ;
                {|| GetUpdate(), DbSkip(-1), GetRefresh(GetList) } )

   PushButton ( { 340, B_TOP }, { 50, B_HEIGHT }, "New",      ;
                {|| GetUpdate(), DbAppend(), GetRefresh(GetList) } )

   PushButton ( { 430, B_TOP }, { 70, B_HEIGHT }, "Delete",   ;
                {|| GetUpdate(), DbDelete(), GetRefresh(GetList) } )

   PushButton ( { 540, B_TOP }, { 50, B_HEIGHT }, "Exit",     ;
                {|| PostAppEvent ( xbeP_Close ) } )
   @ 20, 0 SAY "Ì" + Replicate("Í", 78) + "¹"

   ColorSelect(3)
   @ 21, 9 SAY "Simple Get entry screen programmed in procedural style using"
   @ 23, 9 SAY "@ SAY...GET and graphical Pushbuttons (Xbase++ hybrid mode)"
   ColorSelect(0)

   lExit := .F.

   DO WHILE ! lExit
      READ SAVE
      IF LastAppEvent() == xbeK_PGDN
         DBSKIP(1)
      ELSEIF LastAppEvent() == xbeK_PGUP
         DBSKIP(-1)
      ELSEIF LastAppEvent() == xbeP_Close
         /*
          * The user has closed the window via system menu and
          * READ SAVE was terminated. This terminates the program, too.
          */
         AppQuit()
      ENDIF
      GetRefresh(GetList)
   ENDDO

RETURN

*******************
PROCEDURE AppQuit()
  CLOSE ALL
  QUIT
RETURN

*******************
PROCEDURE GetHelp()

   LOCAL cScreen, nRow := Row(), nCol := Col()

   AEval( SetAppWindow():ChildList(), {|o| o:hide() } )
   SetAppFocus ( SetAppWindow() )

   SET CURSOR OFF

   cScreen := SaveScreen(0,0,MaxRow(),MaxCol())
   CLS
   ?
   ? "This is assumed to be a Help-Screen"
   ?
   ? "The current field is:", READVAR()
   ?
   WAIT

   RestScreen(0,0,MaxRow(),MaxCol(),cScreen)

   AEVAL( SetAppWindow():ChildList(), {|o| o:show() } )

   SetPos( nRow, nCol )

   SET CURSOR ON

RETURN

*******************************
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

*******************************
FUNCTION GetUpdate()

  /*
   * If the text buffer of the current GET object has changed, the
   * buffer must be saved and the current GET object must loose its
   * input focus.
   */
   IF GetActive():changed
      RETURN GetKillActive()
   ENDIF

RETURN NIL

/*
 * Create new XbpPushButton object in current XbpCRT window and
 * set default values.
 */
FUNCTION PushButton ( aPos, aSize, xCaption, bBlock )

   LOCAL button

   button := XbpPushButton():New ( NIL, NIL, aPos, aSize )
   /*
    * Assign caption (String or resource ID of bitmap)
    */
   button:Caption  := xCaption
   /*
    * The button does not receive keyboard focus.
    */
   button:PointerFocus := .F.
   /*
    * The button is not visible immediately after :create()
    */
   button:Visible      := .F.
   /*
    * Assign activate code block.
    */
   button:Activate := bBlock
   /*
    * Create button (request system resources)
    */
   button:Create ()

   /*
    * Change Font.
    */
   button:SetFontCompoundName("10.Helv")
   /*
    * Display button with new font.
    */
   button:Show ()

RETURN button
