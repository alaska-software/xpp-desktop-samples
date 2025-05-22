//////////////////////////////////////////////////////////////////////
//
//  SAMPLE3.PRG
//
//  Copyright:
//     Alaska Software, (c) 1997-2025. All rights reserved.         
//  
//  Contents:
//     This file is a modified version of the SAMPLE2.PRG file
//   
//  Remarks:
//     The form CustomerForm() can be used in a multi-user environment.
//     It locks records to write changed data back to the database.
//   
//     The user defined methods :skip(), :setData() und :getData() are
//     added to the class declaration.
//   
//////////////////////////////////////////////////////////////////////


#include "Gra.ch"
#include "Xbp.ch"
#include "Common.ch"
#include "Appevent.ch"

******************************************************************************
* Main procedure to test the form
******************************************************************************
PROCEDURE Main

   LOCAL nEvent := NIL, oXbp := NIL, mp1 := NIL, mp2 := NIL

   // SET DEFAULT is not written by the FormDesigner
   SET DEFAULT TO ..\..\data\misc

   USE CUSTOMER NEW

   CustomerForm():New():Create()

   DO WHILE nEvent != xbeP_Close
      nEvent := AppEvent ( @mp1, @mp2, @oXbp )
      oXbp:HandleEvent ( nEvent, mp1, mp2 )
      IF nEvent == xbeP_Quit
         QUIT   // AppQuit()
      ENDIF
   ENDDO

RETURN


******************************************************************************
* This class is derived from the implementation-level class of the form.
* Instance variables are declared in the _CustomerForm class.
******************************************************************************
CLASS CustomerForm FROM _CustomerForm
   EXPORTED:
   METHOD init
   METHOD create

   * Application specific methods (they are user-defined)
   METHOD skip
   METHOD setData
   METHOD getData
ENDCLASS


******************************************************************************
* Initialize form
******************************************************************************
METHOD CustomerForm:init( oParent, oOwner, aPos, aSize, aPP, lVisible )

   * Execute method of the super class
   ::_CustomerForm:init( oParent, oOwner, aPos, aSize, aPP, lVisible )

   * Change code blocks for push buttons
   ::ButtonOK:activate       := {|| ::getData(), PostAppEvent( xbeP_Close ) }
   ::ButtonPrevious:activate := {|| ::skip(-1) }
   ::ButtonNext:activate     := {|| ::skip( 1) }
RETURN self


******************************************************************************
* Request system resources
******************************************************************************
METHOD CustomerForm:create( oParent, oOwner, aPos, aSize, aPP, lVisible )

   * Execute method of the super class
   ::_CustomerForm:create( oParent, oOwner, aPos, aSize, aPP, lVisible )

   * Assign work area
   ::CUSTOMER := Select()

   * Transfer values to Editcontrols
   ::setData()

   * Display the form
   ::show()

RETURN self


******************************************************************************
* New method: Change record pointer
* Prior to changing the record pointer all changed data is written to
* the database (:getData()). If the current record is locked and data
* is changed in the form, SKIP is not executed. If the form is unchanged
* SKIP is always executed.
******************************************************************************
METHOD CustomerForm:skip( nSkip )

   IF ::getData()                      // Write record
      (::CUSTOMER)->(DbSkip( nSkip ))  // Change record pointer
      ::setData()                      // Read record
   ENDIF

RETURN self

******************************************************************************
* New method: Transfer data from the database to edit controls
******************************************************************************
METHOD CustomerForm:setData

   AEval( ::editControls, {|o| o:setData() } )

RETURN self


******************************************************************************
* New method: Transfer data from edit controls to the database,
*             only if the record is successfully locked!
******************************************************************************
METHOD CustomerForm:getData
   LOCAL lOk := ( AScan( ::editControls, {|o| o:changed } ) == 0 )

   IF ! lOk                            // Data is changed
      lOk := (::CUSTOMER)->(DbRLock()) // Lock record
      IF lOk                           // Record is locked
         AEval( ::editControls, {|o| o:getData() } )
         (::CUSTOMER)->(DbUnlock())    // Unlock record
      ELSE
         MsgBox( "Record is currently locked" )
      ENDIF
   ENDIF

RETURN lOk

* Include program code for the implementation-level class of the form
#include "_SAMPLE2.PRG"

//EOF
/////
