//////////////////////////////////////////////////////////////////////
//
//  DBCLIENT.PRG
//
//  Copyright:
//       Alaska Software, (c) 1997-2025. All rights reserved.         
//  
//  Contents:
//       Standard Get object with notify link to database
//   
//   Note:
//       This example demonstrates how to link a dialog element to a work
//       area so that changes in the work area are registered automatically
//       by a dialog element
//   
//////////////////////////////////////////////////////////////////////


#include "AppEvent.ch"
#include "dmlb.ch"

/*
 * Simple GET command which uses class DbGet instead of Get.
 * Each DbGet object is registered as a client in the work area.
 * The work area calls the method :notify() of its clients whenever
 * a change occurs in the work area.
 */
#command  @ <nRow>, <nCol> DBGET <xVar> ;
                      [PICTURE <cPict>] ;
                        [VALID <lValid>] ;
                         [WHEN <lWhen>] ;
                         [SEND <message>] ;
      =>  Aadd( getList, ;
              ( DbGet():new( <nRow>, <nCol>, anchorCB(@<xVar>),<(xVar)>, ;
                <cPict>, NIL, <{lValid}>, <{lWhen}>) ):display() ) ;;
          DbRegisterClient( ATail( getList )) ;
       [; Atail( getList ):<message>]


/*
 * The standard GET object of Xbase++ does not have a :notify() method
 * So we create a new class.
 */
CLASS dbGet FROM Get
  EXPORTED:
  METHOD notify
ENDCLASS

METHOD DBGET:notify(nEvent,nDboMsg,xValue)

  /*
   *  Is it a database event ?
   */
  IF (nEvent==xbeDBO_Notify)

    /*
     *  For simplicity only DBO_MOVE_DONE is of interest here
     */
    IF (nDboMsg==DBO_MOVE_DONE)
       ::varGet()
       ::updateBuffer()
    ENDIF

  ENDIF
RETURN


PROCEDURE MAIN( cMode )
  LOCAL getList := {}

  CLS

  /*
   * Open database and establish SetKey navigation
   */
  SET DEFAULT TO "..\..\data\misc"
  USE DGETCUST EXCLUSIVE

  SetMouse(.T.)
  SetAppEvent( xbeK_PGDN , {||DbSkip(1)} )
  SetAppEvent( xbeK_PGUP , {||DbSkip(-1)} )
  DispOutAt( MAXROW(), 0 , "<PgDn = Next> <PgUp = Previous> <Esc = Quit>", "B/W+" )


  /*
   * Create Get fields and register them in the work area
   */
  @ 10,10 DBGET FIELD->CUSTNO
  @ 11,10 DBGET FIELD->NAME1
  @ 12,10 DBGET FIELD->STREET
  @ 13,10 DBGET FIELD->CITY
  @ 14,10 DBGET FIELD->ZIP
  @ 15,10 DBGET FIELD->PHONE
  @ 17,10 DBGET FIELD->REMARK1
  @ 18,10 DBGET FIELD->REMARK2

  READ

  USE

RETURN
