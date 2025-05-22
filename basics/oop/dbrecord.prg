//////////////////////////////////////////////////////////////////////
//
//  DBRECORD.PRG
//
//  Copyright:
//      Alaska Software, (c) 1997-2025. All rights reserved.         
//  
//  Contents:
//      The Program demonstrates the creation of a dynamic class. The example
//      class has instance variables whose names are taken from the database
//      fields in the current workarea.
//   
//////////////////////////////////////////////////////////////////////



#include "Inkey.ch"
#include "Class.ch"


PROCEDURE Main
   LOCAL oRecord, getList := {}
   CLS
   @ MaxRow(), 0 SAY "<PgDn> Next   <PgUp> Previous   <Esc>  Quit"

   SET DEFAULT TO ..\..\data\misc

   USE CUSTOMER

   oRecord := DbRecord():new()

   DO WHILE Lastkey() <> K_ESC
      @  8,10 SAY "Record   :" + PadL( oRecord:recno, 5)
      @ 10,10 SAY "Firstname:" GET oRecord:firstname
      @ 12,10 SAY "Lastname :" GET oRecord:lastname
      READ

      IF Updated()
         IF oRecord:put()
            SKIP
         ELSE
            Alert( "Record is locked" )
            LOOP
         ENDIF

      ELSEIF LastKey() == K_PGUP
         SKIP -1
      ELSE
         SKIP
      ENDIF

      oRecord:get()
   ENDDO

RETURN



/*
 * This Function returns the class object of an arbitrary Record class
 * which is created for a particular work area. Instances of such a class
 * have instance variables with the same names as the fields in the
 * work area.
 */
FUNCTION DBRecord()
   LOCAL i, imax, aIVar, aMethod, oClass

   oClass := ClassObject( Alias() )

   IF oClass <> NIL
      RETURN oClass
   ENDIF

  /*
   * Class object does not exist, yet. Create the class according to
   * the fields in the current workarea. Alias() is used as the class name.
   */
   imax    := FCount()
   aIVar   := Array( imax )
   aMethod := {{ "INIT", CLASS_EXPORTED, {|self| self:get() } }}

   FOR i:=1 TO imax
      aIVar[i] := { FieldName(i), CLASS_EXPORTED }
   NEXT

RETURN ClassCreate( Alias(), {DbBase()}, aIVar, aMethod )



/*
 * Abstract record class which cannot be instantiated since :init() is
 * declared as DEFERRED. The class provides for the :put() and :get()
 * mechanism of it's subclasses.
 */
CLASS DbBase
   EXPORTED:

   CLASS VAR alias READONLY
   INLINE CLASS METHOD initClass
      ::alias := Alias()
   RETURN self

   VAR recno READONLY
   METHOD get, put
   DEFERRED METHOD init
ENDCLASS



/*
 * Transfer values from fields to instance variables
 */
METHOD DbBase:get
   LOCAL nArea := select(), i, imax , cName
   LOCAL subSelf

  /*
   * Get Self of the derived class
   */
   subSelf := ::&( ::alias )

   SELECT (::alias)
   ::recno := Recno()
   imax    := FCount()

  /*
   * Accessing instance variables is accomplished using the & operator
   */
   FOR i:=1 TO imax
      cName := FieldName(i)
      subSelf:&(cName) := FieldGet(i)
   NEXT

   SELECT (nArea)
RETURN self



/*
 * Transfer values from instance variables to fields and provide for
 * record locking
 */
METHOD DbBase:put
   LOCAL i, imax, cName, nOldRec
   LOCAL nArea := Select(), lLocked := .F.
   LOCAL subSelf := ::&(::alias)

   SELECT (::alias)
   IF ::recno <> Recno()
      nOldRec := Recno()
      GOTO ::recno
   ENDIF

   IF DbRLock()
      lLocked := .T.
      imax    := FCount()

      FOR i:=1 TO imax
         cName := FieldName(i)
         FieldPut( i, subSelf:&(cName) )
      NEXT
      DbUnlock()
   ENDIF

   IF ::recno <> Recno()
      GOTO nOldRec
   ENDIF

   SELECT (nArea)
RETURN lLocked
