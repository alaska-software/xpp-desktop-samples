//////////////////////////////////////////////////////////////////////
//
//  CONTACTNAVIGATOR.PRG
//
//  Copyright:
//   Alaska Software, (c) 2002-2025. All rights reserved.         
//  
//  Contents:
//   
//  Remarks:
//   
//  Syntax:
//   
//  Return:
//   
//////////////////////////////////////////////////////////////////////


#pragma library( "ascom10.lib" )

#include "common.ch"
#include "outlook.ch"

#define ID_CONTACTITEM "_ContactItem"

CLASS ContactNavigator

PROTECTED

  VAR oOutlook
  VAR oContacts
  VAR nCurrent

EXPORTED

  METHOD init
  METHOD destroy

  METHOD goNextContact
  METHOD goPrevContact
  METHOD goFirstContact
  METHOD getContact

  METHOD isFirst
  METHOD isEnd

  METHOD importDbf

ENDCLASS

/// <summary>
/// <para>
/// Initialize the Outlook object and get access to
/// the contact instance
/// </para>
/// </summary>
/// <returns>
/// self
/// </returns>
METHOD ContactNavigator:init()
  LOCAL oNS

  ::oOutlook := CreateObject( "Outlook.Application" )
  IF NIL == ::oOutlook
    MsgBox( "Cannot access Outlook application. Please make sure MS Outlook is" + Chr(13) + Chr(10) +;
            "correctly installed on your computer.", "Error" )
    BREAK()
  ENDIF

  oNS  := ::oOutlook:GetNamespace( "MAPI" )

  ::oContacts := oNS:getDefaultFolder( olFolderContacts )

  ::nCurrent := 1

RETURN self

/// <summary>
/// <para>
/// Get rid of all allocated resources
/// </para>
/// </summary>
/// <returns>
/// self
/// </returns>
METHOD ContactNavigator:destroy()

  ::oOutlook:quit()
  ::oOutlook:destroy()
  ::oOutlook := NIL
  ::oContacts := NIL

RETURN self

/// <summary>
/// <para>
/// prepare returning next contact
/// </para>
/// </summary>
/// <returns>
/// self
/// </returns>
METHOD ContactNavigator:goNextContact()
  LOCAL oContact
  LOCAL nMax := ::oContacts:items:count
  LOCAL cName

  // do not increment if last element is reached
  IF nMax < ::nCurrent
    RETURN self
  ENDIF

  ::nCurrent++

  oContact := ::getContact()

  IF oContact == NIL
    RETURN self
  ENDIF

  cName := oContact:InterfaceName()

  // Test whether the name of the interface is correct.
  IF ! ( cName == ID_CONTACTITEM )
    RETURN ::goNextContact()
  ENDIF

RETURN self

/// <summary>
/// <para>
/// prepare returning prev contact
/// </para>
/// </summary>
/// <returns>
/// self
/// </returns>
METHOD ContactNavigator:goPrevContact()
  LOCAL oContact, cName

  IF ::nCurrent <= 1
    ::nCurrent := 1
    RETURN self
  ENDIF

  ::nCurrent--

  oContact := ::getContact()

  IF oContact == NIL
    RETURN self
  ENDIF

  cName := oContact:InterfaceName()

  IF ! ( cName == ID_CONTACTITEM )
    RETURN ::goPrevContact()
  ENDIF
RETURN self

/// <summary>
/// <para>
/// prepare returning prev contact
/// </para>
/// </summary>
/// <returns>
/// self
/// </returns>
METHOD ContactNavigator:goFirstContact()
  ::nCurrent := 1
RETURN self

/// <summary>
/// <para>
/// return current contact
/// </para>
/// </summary>
/// <returns>
/// self
/// </returns>
METHOD ContactNavigator:getContact()

  LOCAL nMax := ::oContacts:items:count
  LOCAL oContact

  // return contact only, if it within the allowed range
  IF 0 < ::nCurrent .AND. ::nCurrent <= nMax
    oContact := ::oContacts:items( ::nCurrent )
  ENDIF
RETURN oContact

/// <summary>
/// <para>
/// Determines whether the first contact is reached
/// </para>
/// </summary>
/// <returns>
/// lFirst
/// </returns>
METHOD ContactNavigator:isFirst()

  LOCAL lFirst := .F.

  IF ::nCurrent == 1
    lFirst := .T.
  ENDIF

RETURN lFirst

/// <summary>
/// <para>
/// Determine whether the last contact is reached
/// </para>
/// </summary>
/// <returns>
/// self
/// </returns>
METHOD ContactNavigator:isEnd()

  LOCAL lLast := .F.

  IF ::nCurrent > ::oContacts:items:count
    lLast := .T.
  ENDIF

RETURN lLast

/// <summary>
/// <para>
/// Import all Contacts to a DBF Database.
/// </para>
/// </summary>
/// <returns>
/// self
/// </returns>
METHOD ContactNavigator:importDbf( cDBFFile )

  LOCAL aStruct := {}
  LOCAL nA
  LOCAL oContact, nOldCurr

  DEFAULT cDBFFile TO "OUTLOOK"

  // for restoring the current contact when we have finished
  nOldCurr := ::nCurrent

  AAdd( aStruct, {"BADDRCITY",  "C", 50, 0} )
  AAdd( aStruct, {"BADDRCOUNT", "C", 50, 0} )
  AAdd( aStruct, {"BADDRPOST",  "C", 50, 0} )
  AAdd( aStruct, {"BADDRPOSTB", "C", 50, 0} )
  AAdd( aStruct, {"BADDRSTATE", "C", 50, 0} )
  AAdd( aStruct, {"BADDRSTREE", "C", 50, 0} )
  AAdd( aStruct, {"BADDRFAXNU", "C", 50, 0} )
  AAdd( aStruct, {"BADDRHOMEP", "C", 50, 0} )
  AAdd( aStruct, {"BADDRTELNU", "C", 50, 0} )
  AAdd( aStruct, {"CALLBACKTE", "C", 50, 0} )
  AAdd( aStruct, {"COMPANYNAM", "C", 50, 0} )
  AAdd( aStruct, {"EMAIL1ADDR", "C", 50, 0} )
  AAdd( aStruct, {"FIRSTNAME",  "C", 50, 0} )
  AAdd( aStruct, {"MIDDLENAME", "C", 50, 0} )
  AAdd( aStruct, {"LASTNAME",   "C", 50, 0} )
  AAdd( aStruct, {"JOBTITLE",   "C", 50, 0} )
  AAdd( aStruct, {"CREATETIME", "D",  0, 0} )
  AAdd( aStruct, {"BIRTHDAY",   "D",  0, 0} )
  AAdd( aStruct, {"LASTMODTIM", "D",  0, 0} )
  AAdd( aStruct, {"ANNIVERSAR", "D",  0, 0} )
  AAdd( aStruct, {"ASSISTANTN", "C", 50, 0} )
  AAdd( aStruct, {"EMAIL1DISP", "C", 50, 0} )
  AAdd( aStruct, {"INITIALS",   "C", 50, 0} )
  AAdd( aStruct, {"NICKNAME",   "C", 50, 0} )
  AAdd( aStruct, {"PROFESSION", "C", 50, 0} )
  AAdd( aStruct, {"SUFFIX",     "C", 50, 0} )
  AAdd( aStruct, {"TITLE",      "C", 50, 0} )


  // Create a new database file. Delete the old one if any.
  IF File( cDBFFile + ".DBF" )
    FErase( cDBFFile  + ".DBF" )
  ENDIF
  DBCreate( cDBFFile, aStruct, "FOXCDX" )
  DbCloseAll()
  DbUseArea( .T., , cDBFFile, , .F. )

  nA := Select()

  // Iterate all contacts and write details to database
  ::goFirstContact()
  DO WHILE ! ::isEnd()

    oContact := ::getContact()

    ( nA )->( DbAppend() )

    ( nA )->BADDRCITY   :=  oContact:BusinessAddressCity
    ( nA )->BADDRCOUNT  :=  oContact:BusinessAddressCountry
    ( nA )->BADDRPOST   :=  oContact:BusinessAddressPostalCode
    ( nA )->BADDRPOSTB  :=  oContact:BusinessAddressPostOfficeBox
    ( nA )->BADDRSTATE  :=  oContact:BusinessAddressState
    ( nA )->BADDRSTREE  :=  oContact:BusinessAddressStreet
    ( nA )->BADDRFAXNU  :=  oContact:BusinessFaxNumber
    ( nA )->BADDRHOMEP  :=  oContact:BusinessHomePage
    ( nA )->BADDRTELNU  :=  oContact:BusinessTelephoneNumber
    ( nA )->CALLBACKTE  :=  oContact:CallbackTelephoneNumber
    ( nA )->COMPANYNAM  :=  oContact:CompanyName
    ( nA )->EMAIL1ADDR  :=  oContact:Email1Address
    ( nA )->FIRSTNAME   :=  oContact:FirstName
    ( nA )->MIDDLENAME  :=  oContact:MiddleName
    ( nA )->LASTNAME    :=  oContact:LastName
    ( nA )->JOBTITLE    :=  oContact:JobTitle
    ( nA )->CREATETIME  :=  oContact:CreationTime
    ( nA )->BIRTHDAY    :=  oContact:Birthday
    ( nA )->LASTMODTIM  :=  oContact:LastModificationTime
    ( nA )->ANNIVERSAR  :=  oContact:Anniversary
    ( nA )->ASSISTANTN  :=  oContact:AssistantName
    ( nA )->EMAIL1DISP  :=  oContact:Email1DisplayName
    ( nA )->INITIALS    :=  oContact:Initials
    ( nA )->NICKNAME    :=  oContact:NickName
    ( nA )->PROFESSION  :=  oContact:Profession
    ( nA )->SUFFIX      :=  oContact:Suffix
    ( nA )->TITLE       :=  oContact:Title


    ::goNextContact()
  ENDDO

  ( nA )->( DbCloseArea() )

  ::nCurrent := nOldCurr

RETURN self
