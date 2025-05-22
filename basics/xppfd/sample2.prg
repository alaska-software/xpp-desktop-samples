//////////////////////////////////////////////////////////////////////
//
//  SAMPLE2.PRG
//
//  Copyright:
//     Alaska Software, (c) 1997-2025. All rights reserved.         
//  
//  Contents:
//     Class code created by the Xbase++ FormDesigner
//     Creation date: 05.12.97 Creation time: 13:48:13
//   
//  Remarks:
//     This file contains the basic structure for the utilization-level of
//     a form. It may (and sould) be modified.
//   
//////////////////////////////////////////////////////////////////////


#include "Gra.ch"
#include "Xbp.ch"
#include "Common.ch"
#include "Appevent.ch"
#include "Font.ch"


******************************************************************************
* This class is derived from the implementation-level class of the form.
* Instance variables are declared in the _CustomerForm class.
******************************************************************************
CLASS CustomerForm FROM _CustomerForm
   EXPORTED:
      METHOD init
      METHOD create
ENDCLASS

******************************************************************************
* Initialize form
******************************************************************************
METHOD CustomerForm:init( oParent, oOwner, aPos, aSize, aPP, lVisible )

   * Execute method of the super class
   ::_CustomerForm:init( oParent, oOwner, aPos, aSize, aPP, lVisible )

RETURN self


******************************************************************************
* Request system resources
******************************************************************************
METHOD CustomerForm:create( oParent, oOwner, aPos, aSize, aPP, lVisible )

   * Execute method of the super class
   ::_CustomerForm:create( oParent, oOwner, aPos, aSize, aPP, lVisible )

   * Open databases and assign work area
   USE CUSTOMER.DBF NEW EXCLUSIVE
   ::CUSTOMER := Select()

   * Transfer values to EditControls
   AEval ( ::EditControls, { | oXbp | oXbp:SetData() } )

   * Display the form
   ::show()

RETURN self


******************************************************************************
* Main procedure to test a form
******************************************************************************
PROCEDURE Main

   LOCAL nEvent, oXbp := NIL, mp1 := NIL, mp2 := NIL

   // SET DEFAULT is not written by the FormDesigner
   SET DEFAULT TO ..\..\data\misc

   CustomerForm():New():Create()

   nEvent := xbe_None
   WHILE nEvent != xbeP_Close
      nEvent := AppEvent ( @mp1, @mp2, @oXbp )
      oXbp:HandleEvent ( nEvent, mp1, mp2 )
      IF nEvent == xbeP_Quit
         QUIT   // AppQuit()
      ENDIF
   ENDDO

RETURN

* Include program code for the implementation-level class of the form
#include "_SAMPLE2.PRG"

//EOF
/////
