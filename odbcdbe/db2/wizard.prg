//////////////////////////////////////////////////////////////////////
//
//  WIZARD.PRG
//
//  Copyright:
//      Alaska Software, (c) 2001-2025. All rights reserved.         
//  
//  Contents:
//      Wizard dialog
//////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
//
//  Class code created by the Xbase++ FormDesigner
//    Creation date: 17.08.01 Creation time: 14.23.35
//
//  Contents  :
//    This file contains the basic structure for the utilization-level of
//    a form. It may (and sould) be modified.
//
///////////////////////////////////////////////////////////////////////////////

#include "Gra.ch"
#include "Xbp.ch"
#include "Common.ch"
#include "Appevent.ch"
#include "Font.ch"


******************************************************************************
* This class is derived from the implementation-level class of the form.
* Instance variables are declared in the _NewForm class.
******************************************************************************
CLASS WizardDlg FROM _NewForm
   EXPORTED:
    VAR nImage
    VAR oPrev, oNext
    VAR cLabel, cInput
    VAR bFindAction, bAction
    METHOD goPrev
    METHOD goNext
    METHOD find
    METHOD init
    METHOD create
    METHOD setParams
    METHOD show
ENDCLASS

******************************************************************************
* Initialize form
******************************************************************************
METHOD WizardDlg:init( oParent, oOwner, aPos, aSize, aPP, lVisible )

   * Execute method of the super class
   ::_NewForm:init( oParent, oOwner, aPos, aSize, aPP, lVisible )

RETURN self


******************************************************************************
* Request system resources
******************************************************************************
METHOD WizardDlg:create( oParent, oOwner, aPos, aSize, aPP, lVisible )

   ::oImage:caption := ::nImage
   ::oBtnBack:activate := {|| ::goPrev() }
   ::oBtnNext:activate := {|| ::goNext() }
   ::oBtnNext:preSelect := .T.
   ::oBtnFind:activate := {|| ::find() }
   ::oLabel:caption := ::cLabel

   ::keyBoard := {| nKeyCode, uNIL, self | IIF(xbeK_ENTER==nKeyCode, ;
                    PostAppEvent(xbeP_Activate,,,::oBtnNext),) }

   IF aPos = NIL
      /* try to get pos of oPrev */
      IF ::oPrev != NIL
         aPos := ::oPrev:currentPos()
      ELSE
         aPos := CenterPos(::currentSize(), AppDesktop():currentSize())
      ENDIF
   ENDIF

   * Execute method of the super class
   ::_NewForm:create( oParent, oOwner, aPos, aSize, aPP, lVisible )

   IF ::cInput != NIL
       ::oInput:dataLink := {|x| IIf( x==NIL, ::cInput, ::cInput:=x ) } 
       ::oInput:setData()
   ELSE
       ::oInput:hide()
   ENDIF
   IF ::oPrev = NIL
      ::oBtnBack:hide()
   ENDIF
   IF ::oNext = NIL
      ::oBtnNext:setCaption("Finish")
   ENDIF
   IF ::bFindAction = NIL
      ::oBtnFind:hide()
   ENDIF
   ::oBtnCancel:activate := {||PostAppEvent(xbeP_Close,,,self)}
   ::setModalState(XBP_DISP_APPMODAL)
RETURN self

******************************************************************************
* overloaded: show
******************************************************************************
METHOD WizardDlg:show()
LOCAL lRet
   lRet := ::XbpDialog:show()
   SetAppFocus(::oBtnNext)
RETURN lRet


******************************************************************************
* setParams
******************************************************************************
METHOD WizardDlg:setParams( nImage, oPrev, oNext, cLabel, cInputDef, ;
                            bFindAction, bAction)
    ::nImage := nImage
    ::oPrev  := oPrev
    ::oNext  := oNext 
    ::cLabel := cLabel
    ::cInput := cInputDef
    ::bFindAction :=bFindAction
    ::bAction := bAction
RETURN self


******************************************************************************
* goPrev: activate previuis dialog
******************************************************************************
METHOD WizardDlg:goPrev()
    IF ::oPrev != NIL
        ::setModalState(XBP_DISP_MODELESS)
        ::hide()
        ::oPrev:show()
        ::setModalState(XBP_DISP_APPMODAL)
    ENDIF
RETURN self

******************************************************************************
* goNext: activate next dialog
******************************************************************************
METHOD WizardDlg:goNext()
LOCAL lNext := .T.

    IF ::bAction = NIL
       lNext := .T.
    ELSE
       ::oInput:getData()
       ::setModalState(XBP_DISP_MODELESS)
       IF ::oNext != NIL
           lNext := Eval(::bAction, ::cInput, self)
       ELSE
           /* last wizard */
           ::destroy()
           Eval(::bAction, ::cInput, self)
           lNext := .F.
       ENDIF
    ENDIF 
    IF lNext
        ::setModalState(XBP_DISP_MODELESS)
        ::hide()
        ::oNext:show()
        ::setModalState(XBP_DISP_APPMODAL)
    ENDIF
RETURN self

******************************************************************************
* find: activate find dialog
******************************************************************************
METHOD WizardDlg:find()
    IF ::bFindAction != NIL
        IF Eval(::bFindAction, @::cInput, self)
            ::oInput:setData(::cInput)
            SetAppFocus(::oBtnNext)
        ENDIF
    ENDIF
RETURN self

* Include program code for the implementation-level class of the form
#include "_WIZARD.PRG"

//EOF
/////
