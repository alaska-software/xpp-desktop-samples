//////////////////////////////////////////////////////////////////////
//
//  SMARTFORM.PRG
//
//  Copyright:
//   Alaska Software, (c) 2017-2025. All rights reserved.         
//  
//  Contents:
//   Experimental implementation of a simple form mechanism using the 
//   XbpHTMLWindow() class
//   
//////////////////////////////////////////////////////////////////////

#include "Gra.ch"
#include "Xbp.ch"
#include "class.ch"

#pragma library("xppwui.lib")

/// <summary>
/// Experimental implementation of a simple form mechanism using the
/// XbpHtmlWindow() class as the rendering engine. The basic idea
/// is to simply define entry fields, bind them to a model and have
/// a nice looking dialog without pictures masks and validation.
/// </summary>
CLASS XbpSmartForm FROM XbpDialog
  PROTECTED:
  VAR btnSave
  VAR btnCancel

  VAR htmlElements
  VAR htmlContent
  VAR htmlForm

  VAR firstField
  VAR fieldCount

  VAR model
  METHOD putModelValues()
  METHOD getFormValues()
  EXPORTED:
  METHOD init()
  METHOD create()
  METHOD showModal()

  METHOD setModel()

  METHOD startDefinition()
  METHOD endDefinition()
  METHOD addHeading()
  METHOD addField()
  METHOD addFields()
  METHOD addHtml()
ENDCLASS


METHOD XbpSmartForm:startDefinition(cTxt)
  ::htmlContent   := LoadResource( 42, BinaryName() , "TEMPLATE" )
  ::htmlContent   := StrTran( ::htmlContent, "::hero", cTxt )
RETURN(SELF)


METHOD XbpSmartForm:endDefinition()
  ::htmlContent := StrTran( ::htmlContent, "::form", ::htmlElements )
  ::htmlForm:html := ::htmlContent
  ::htmlForm:create()
RETURN(SELF)


METHOD XbpSmartForm:addHtml(cHtml)
  ::htmlElements += cHtml + Chr(13)+Chr(10)
RETURN(SELF)


METHOD XbpSmartForm:addFields( cLabel, aId, aHint, aSize )
  LOCAL cTxt
  LOCAL n
  ::addHtml("<tr>")
  cTxt := '<TD width=25% align="right">::label</TD>'
  cTxt := StrTran( cTxt, "::label", AllTrim(cLabel)+":" )
  ::addHtml( cTxt )
  ::addHtml("<td>")

  FOR n:=1 TO Len(aId)
    cTxt := '<INPUT id="::id" type="text" novalue="::hint" size="::size" maxlength="::size">'
    cTxt := StrTran( cTxt, "::id", Lower(AllTrim(aId[n])) )
    cTxt := StrTran( cTxt, "::hint", AllTrim(aHint[n]) )
    cTxt := StrTran( cTxt, "::size", AllTrim(Str(aSize[n],,0)))
    ::addHtml(cTxt)
  NEXT n

  ::addHtml("</td>")
  ::addHtml("</tr>")
RETURN( SELF )


METHOD XbpSmartForm:addField( cLabel, cId, cHint, nSize )
  LOCAL cTxt

  ::fieldCount++
  ::addHtml("<tr>")
  cTxt := '<TD width=25% align="right">::label</TD>'
  cTxt := StrTran( cTxt, "::label", AllTrim(cLabel)+":" )
  ::addHtml( cTxt )

  IF(::firstField==::fieldCount)
    cTxt := '<TD><INPUT autofocus id="::id" type="text" novalue="::hint" size="::size" maxlength="::size"></TD>'
  ELSE
    cTxt := '<TD><INPUT id="::id" type="text" novalue="::hint" size="::size" maxlength="::size"></TD>'
  ENDIF
  cTxt := StrTran( cTxt, "::id", Lower(AllTrim(cId)) )
  cTxt := StrTran( cTxt, "::hint", AllTrim(cHint) )
  cTxt := StrTran( cTxt, "::size", AllTrim(Str(nSize,,0)))
  ::addHtml(cTxt)

  ::addHtml("</tr>")
RETURN( SELF )


METHOD XbpSmartForm:addHeading( cTxt )
  ::addHtml("<tr>")
  ::addHtml('<td colspan="2">')
  ::addHtml("<hr>")
  ::addHtml('<h3 style="color:gray;padding-left:5px;">'+Var2Char(cTxt)+'</h3>')
  ::addHtml("</td>")
  ::addHtml("</tr>")
RETURN(SELF)


METHOD XbpSmartForm:putModelValues()
  LOCAL aM,n,cId,oE
  aM := ::model:classDescribe(CLASS_DESCR_MEMBERS)
  FOR n:=1 TO Len(aM)
    cId := Lower(aM[n][1])
    oE  := ::htmlForm:childFromId( cId )
    IF(ValType(oE)=="O")
     oE:text := Char2Utf8( AllTrim(Var2Char(::model[n])) )
    ENDIF
  NEXT n
RETURN(SELF)


METHOD XbpSmartForm:getFormValues()
  LOCAL aM,n,cId,oE
  aM := ::model:classDescribe(CLASS_DESCR_MEMBERS)
  FOR n:=1 TO Len(aM)
    cId := Lower(aM[n][1])
    oE  := ::htmlForm:childFromId( cId )
    IF(ValType(oE)=="O")
     ::model:SetNoIvar(cId, Utf82Char( oE:text ) )
    ENDIF
  NEXT n
RETURN(SELF)


METHOD XbpSmartForm:setModel( oModel )
  IF(ValType(oModel)=="O")
    ::model := oModel
    ::putModelValues()
  ELSE
    ::getFormValues()
  ENDIF
RETURN(::model)


METHOD XbpSmartForm:init( oParent, aSize )
  LOCAL oCanvas

  ::htmlContent  := ""
  ::htmlElements := ""
  ::firstField   := 1
  ::fieldCount   := 0

  SUPER:init( oParent, , , aSize , {{ XBP_PP_COMPOUNDNAME, "10.Segoe UI" }} , .F. )
  ::taskList := .F.
  ::sysMenu  := .F.
  ::titleBar := .F.
  ::border   := XBPDLG_DLGBORDER
  ::title    := "<undefined>"
  ::drawingArea:setColorBG( GRA_CLR_WHITE )

  oCanvas := ::drawingArea
  oCanvas:setFontCompoundName( "10.Segoe UI" )

  ::btnSave := XbpPushButton():new( oCanvas, , {490,5}, {96,24}, { { XBP_PP_BGCLR, XBPSYSCLR_BUTTONMIDDLE }, { XBP_PP_FGCLR, -58 } } )
  ::btnSave:caption := "Save (Enter)"
  ::btnSave:tabStop := .T.
  ::btnSave:default := .T.

  ::btnCancel := XbpPushButton():new( oCanvas, , {390,5}, {96,24}, { { XBP_PP_BGCLR, XBPSYSCLR_BUTTONMIDDLE }, { XBP_PP_FGCLR, -58 } } )
  ::btnCancel:caption := "Cancel (Esc)"
  ::btnCancel:tabStop := .T.
  ::btnCancel:cancel  := .T.

RETURN self


METHOD XbpSmartForm:showModal()
RETURN( SUPER:showModal( ::htmlForm ) )


METHOD XbpSmartForm:create()
  LOCAL aSize

  SUPER

#define PB_AREA_HEIGHT 30
  aSize    := ::drawingArea:currentSize()
  aSize[2] := aSize[2] - PB_AREA_HEIGHT
  ::htmlForm := XbpHtmlWindow():new( ::drawingArea, , {0,PB_AREA_HEIGHT}, aSize, { { XBP_PP_BGCLR, -255 } } )
  ::btnSave:create()
  ::btnCancel:create()

  CenterControl( SELF )
RETURN Self
