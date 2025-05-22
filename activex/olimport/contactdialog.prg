//////////////////////////////////////////////////////////////////////
//
//  CONTACTDIALOG.PRG
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


#include "common.ch"
#include "appevent.ch"
#include "xbp.ch"

#define XBP_HEIGHT      20
#define XBP_WIDTH       350
#define XBP_VERT_SPACE  5
#define XBP_HORIZ_SPACE 20

#define MODULO_HORIZ_RATIO 5
#define NAME_HORIZ_RATIO   0.4   // 2/5
#define VALUE_HORIZ_RATIO  0.6   // 3/5

#define CRLF Chr( 13 ) + Chr( 10 )
#define SPACE Chr( 32 )

CLASS ContactItem FROM XbpStatic
PROTECTED
  VAR    oName
  VAR    oValue

  METHOD calcLeftPos
  METHOD calcRightPos
  METHOD calcLeftSize
  METHOD calcRightSize
EXPORTED
  METHOD init
  METHOD create

  METHOD setPosAndSize

  METHOD getName
  METHOD getValue
ENDCLASS

CLASS ContactDialog FROM XbpDialog

PROTECTED
  VAR    oContacts

  // The values that can be shown and updated
  VAR    oBAddrCity
  VAR    oBAddrCountry
  VAR    oBAddrPostalCode
  VAR    oBAddrPostOfficeBox
  VAR    oBAddrState
  VAR    oBAddrStreet
  VAR    oBAddrFaxNumber
  VAR    oBAddrHomePage
  VAR    oBAddrTelephoneNumber
  VAR    oBAddrCallBackTelephoneNumber
  VAR    oCompanyName
  VAR    oEmail1Address
  VAR    oFirstName
  VAR    oMiddleName
  VAR    oLastname
  VAR    oJobTitle

  VAR    oCreationTime
  VAR    oLastModificationTime
  VAR    oBirthday
  VAR    oAnniversary
  VAR    oAssistantName
  VAR    oEmail1DisplayName
  VAR    oInitials
  VAR    oNickName
  VAR    oProfession
  VAR    oSuffix
  VAR    oTitle

  VAR    oNextButton
  VAR    oPrevButton
  VAR    oExportButton

  VAR    oHint

  METHOD updateDialog

EXPORTED
  METHOD init
  METHOD create
  METHOD destroy
  METHOD showModal

  METHOD resize

  METHOD navigateNextContact
  METHOD navigatePrevContact
  METHOD exportContact

ENDCLASS

/// <summary>
/// <para>
/// Initialize the base class and all IVars
/// </para>
/// </summary>
/// <returns>
/// self
/// </returns>
METHOD ContactItem:init( oParent, oOwner, aPos, aSize, aPresParam, lVisible )

  LOCAL aNameSize, aNamePos, aValueSize, aValuePos

  ::XbpStatic:init( oParent, oOwner, aPos, aSize, aPresParam, .F. )

  aNameSize  := ::calcLeftSize()
  aValueSize := ::calcRightSize()

  aNamePos   := ::calcLeftPos()
  aValuePos  := ::calcRightPos()

  ::oName := XbpStatic():new( oParent, oOwner, aNamePos, aNameSize, ;
                              aPresParam, lVisible )
  ::oValue := XbpSle():new( oParent, oOwner, aValuePos, aValueSize, ;
                              aPresParam, lVisible )

  ::oValue:editable := .F.

RETURN self

/// <summary>
/// <para>
/// Create baseclass and all IVars
/// </para>
/// </summary>
/// <returns>
/// self
/// </returns>
METHOD ContactItem:create( oParent, oOwner, aPos, aSize, aPresParam, lVisible )

  LOCAL aNameSize, aNamePos, aValueSize, aValuePos

  ::XbpStatic:create( oParent, oOwner, aPos, aSize, aPresParam, .F. )

  aNameSize  := ::calcLeftSize()
  aValueSize := ::calcRightSize()

  aNamePos   := ::calcLeftPos()
  aValuePos  := ::calcRightPos()

  ::oName:create( oParent, oOwner, aNamePos, aNameSize, ;
                  aPresParam, lVisible )
  ::oValue:create( oParent, oOwner, aValuePos, aValueSize, ;
                   aPresParam, lVisible )
RETURN self

/// <summary>
/// <para>
/// Calculate position of left XbaseParts
/// </para>
/// </summary>
/// <returns>
/// self
/// </returns>
METHOD ContactItem:calcLeftPos()
  LOCAL aRetPos
  aRetPos := ::currentPos()
RETURN aRetPos

/// <summary>
/// <para>
/// Calculate position of right XbaseParts
/// </para>
/// </summary>
/// <returns>
/// self
/// </returns>
METHOD ContactItem:calcRightPos()
  LOCAL aPos, aRetPos, aLeftSize
  aLeftSize := ::calcLeftSize()
  aPos := ::currentPos()
  aRetPos := { aPos[1] + aLeftSize[1], aPos[2] }
RETURN aRetPos

/// <summary>
/// <para>
/// calculate size of left XbaseParts
/// </para>
/// </summary>
/// <returns>
/// self
/// </returns>
METHOD ContactItem:calcLeftSize()
  LOCAL aSize, aRetSize
  aSize := ::currentSize()

  // normalize the horizontal size
  aSize[1] := MODULO_HORIZ_RATIO * ( int( aSize[1] / MODULO_HORIZ_RATIO ) )
  aRetSize := { aSize[1] * NAME_HORIZ_RATIO, aSize[2] }
RETURN aRetSize

/// <summary>
/// <para>
/// Calculate size of right XbaseParts
/// </para>
/// </summary>
/// <returns>
/// self
/// </returns>
METHOD ContactItem:calcRightSize()
  LOCAL aSize, aRetSize
  aSize := ::currentSize()

  // normalize the horizontal size
  aSize[1] := MODULO_HORIZ_RATIO * ( int( aSize[1] / MODULO_HORIZ_RATIO ) )
  aRetSize := { aSize[1] * VALUE_HORIZ_RATIO, aSize[2] }
RETURN aRetSize

/// <summary>
/// <para>
/// Set Position and size of base class and all IVars
/// </para>
/// </summary>
/// <returns>
/// lSuccess
/// </returns>
METHOD ContactItem:setPosAndSize( aPos, aSize, lPaint )

  DEFAULT lPaint TO .T.

  ::XbpStatic:setPosAndSize( aPos, aSize, lPaint )
  ::oName:setPosAndSize( ::calcLeftPos(), ::calcLeftSize(), lPaint )
  ::oValue:setPosAndSize( ::calcRightPos(), ::calcRightSize(), lPaint )
RETURN .T.

/// <summary>
/// <para>
/// return XbasePart representing the name
/// </para>
/// </summary>
/// <returns>
/// Xbase Part
/// </returns>
METHOD ContactItem:getName()
RETURN ::oName

/// <summary>
/// <para>
/// return XbasePart representing the value
/// </para>
/// </summary>
/// <returns>
/// Xbase Part
/// </returns>
METHOD ContactItem:getValue()
RETURN ::oValue

/// <summary>
/// <para>
/// Update XbaseParts
/// </para>
/// </summary>
/// <returns>
/// self
/// </returns>
METHOD ContactDialog:updateDialog()

  LOCAL oContact := ::oContacts:getContact()

  IF ::oContacts:isFirst()
    ::oPrevButton:disable()
  ELSE
    ::oPrevButton:enable()
  ENDIF

  IF ::oContacts:isEnd()
    ::oNextButton:disable()
  ELSE
    ::oNextButton:enable()
  ENDIF

  IF NIL == oContact
    RETURN self
  ENDIF

  // visualize the contact
  ::oBAddrCity:getValue():setData( ;
                            oContact:BusinessAddressCity )

  ::oBAddrCountry:getValue():setData( ;
                            oContact:BusinessAddressCountry )

  ::oBAddrPostalCode:getValue():setData( ;
                            oContact:BusinessAddressPostalCode )

  ::oBAddrPostOfficeBox:getValue():setData( ;
                            oContact:BusinessAddressPostOfficeBox )

  ::oBAddrState:getValue():setData( ;
                            oContact:BusinessAddressState )

  ::oBAddrStreet:getValue():setData( ;
                            oContact:BusinessAddressStreet )

  ::oBAddrFaxNumber:getValue():setData( ;
                            oContact:BusinessFaxNumber )

  ::oBAddrHomePage:getValue():setData( ;
                            oContact:BusinessHomePage )

  ::oBAddrTelephoneNumber:getValue():setData( ;
                            oContact:BusinessTelephoneNumber )

  ::oBAddrCallBackTelephoneNumber:getValue():setData( ;
                            oContact:CallbackTelephoneNumber )

  ::oCompanyName:getValue():setData( ;
                            oContact:CompanyName )

  ::oEmail1Address:getValue():setData( ;
                            oContact:Email1Address )

  ::oFirstName:getValue():setData( ;
                            oContact:FirstName )

  ::oMiddleName:getValue():setData(  ;
                            oContact:MiddleName )

  ::oLastname:getValue():setData( ;
                            oContact:LastName )

  ::oJobTitle:getValue():setData( ;
                            oContact:JobTitle )

  ::oCreationTime:getValue():setData( ;
                            Transform( oContact:CreationTime, "" ) )

  ::oLastModificationTime:getValue():setData( ;
                            Transform( oContact:LastModificationTime, ""  ) )

  ::oBirthday:getValue():setData(  ;
                            Transform( oContact:Birthday, ""  ) )

  ::oAnniversary:getValue():setData(  ;
                            Transform( oContact:Anniversary, ""  ) )

  ::oAssistantName:getValue():setData( ;
                            oContact:AssistantName )

  ::oEmail1DisplayName:getValue():setData( ;
                            oContact:Email1DisplayName )

  ::oInitials:getValue():setData( ;
                            oContact:Initials )

  ::oNickName:getValue():setData( ;
                            oContact:NickName )

  ::oProfession:getValue():setData( ;
                            oContact:Profession )

  ::oSuffix:getValue():setData( ;
                            oContact:Suffix )

  ::oTitle:getValue():setData( ;
                            oContact:Title )

RETURN self

/// <summary>
/// <para>
/// Initialize Dialog and Dialog IVars
/// </para>
/// </summary>
/// <returns>
/// self
/// </returns>
METHOD ContactDialog:init( oParent, oOwner, aPos, aSize, aPresParam, lVisible )

  LOCAL nHeight, nWidth

  IF aSize == NIL
    // Calculation of Hight and Size
    nHeight := 29 * ( XBP_VERT_SPACE + XBP_HEIGHT )
    nWidth  := 3 * XBP_HORIZ_SPACE + 2 * XBP_WIDTH
    aSize := { nWidth, nHeight }
  ENDIF

  // initialize base class
  ::XbpDialog:init( oParent, oOwner, aPos, aSize, aPresParam, lVisible )

  ::minSize := { nWidth, nHeight }
  ::tasklist := .T.
  ::title := "Xbase++ Outlook address Viewer"

  ::oHint := XbpStatic():new( ::drawingArea )
  ::oHint:setColorBg( GraMakeRGBColor( { 255, 255, 255 } ) )
  ::oHint:options := XBPSTATIC_TEXT_WORDBREAK
  ::oHint:caption := GetHintText()

  ::oBAddrCity := ContactItem():new( ::drawingArea )
  ::oBAddrCity:getName():caption := "Address City"

  ::oBAddrCountry                   := ContactItem():new( ::drawingArea )
  ::oBAddrCountry:getName():caption := "Address Country"

  ::oBAddrPostalCode                := ContactItem():new( ::drawingArea )
  ::oBAddrPostalCode:getName():caption := "Address Postal Code"

  ::oBAddrPostOfficeBox             := ContactItem():new( ::drawingArea )
  ::oBAddrPostOfficeBox:getName():caption := "Address Post Office Box"

  ::oBAddrState                     := ContactItem():new( ::drawingArea )
  ::oBAddrState:getName():caption := "Address State"

  ::oBAddrStreet                    := ContactItem():new( ::drawingArea )
  ::oBAddrStreet:getName():caption := "Address Street"

  ::oBAddrFaxNumber                 := ContactItem():new( ::drawingArea )
  ::oBAddrFaxNumber:getName():caption := "Address Fax Number"

  ::oBAddrHomePage                  := ContactItem():new( ::drawingArea )
  ::oBAddrHomePage:getName():caption := "Address Homepage"

  ::oBAddrTelephoneNumber           := ContactItem():new( ::drawingArea )
  ::oBAddrTelephoneNumber:getName():caption := "Address Telephone Number"

  ::oBAddrCallBackTelephoneNumber   := ContactItem():new( ::drawingArea )
  ::oBAddrCallBackTelephoneNumber:getName():caption := "Address Callback Number"

  ::oCompanyName                    := ContactItem():new( ::drawingArea )
  ::oCompanyName:getName():caption := "Company Name"

  ::oEmail1Address                  := ContactItem():new( ::drawingArea )
  ::oEmail1Address:getName():caption := "Email 1 Address"

  ::oFirstName                      := ContactItem():new( ::drawingArea )
  ::oFirstName:getName():caption := "First Name"

  ::oMiddleName                     := ContactItem():new( ::drawingArea )
  ::oMiddleName:getName():caption := "Middle Name"

  ::oLastname                       := ContactItem():new( ::drawingArea )
  ::oLastname:getName():caption := "Last Name"

  ::oJobTitle                       := ContactItem():new( ::drawingArea )
  ::oJobTitle:getName():caption := "Job Title"

  ::oCreationTime                   := ContactItem():new( ::drawingArea )
  ::oCreationTime:getName():caption := "Creation Time"

  ::oLastModificationTime           := ContactItem():new( ::drawingArea )
  ::oLastModificationTime:getName():caption := "Last Modification Time"

  ::oBirthday                       := ContactItem():new( ::drawingArea )
  ::oBirthday:getName():caption := "Birthday"

  ::oAnniversary                    := ContactItem():new( ::drawingArea )
  ::oAnniversary:getName():caption := "Anniversary"

  ::oAssistantName                  := ContactItem():new( ::drawingArea )
  ::oAssistantName:getName():caption := "Assistant Name"

  ::oEmail1DisplayName              := ContactItem():new( ::drawingArea )
  ::oEmail1DisplayName:getName():caption := "Email Display Name"

  ::oInitials                       := ContactItem():new( ::drawingArea )
  ::oInitials:getName():caption := "Initials"

  ::oNickName                       := ContactItem():new( ::drawingArea )
  ::oNickName:getName():caption := "Nickname"

  ::oProfession                     := ContactItem():new( ::drawingArea )
  ::oProfession:getName():caption := "Profession"

  ::oSuffix                         := ContactItem():new( ::drawingArea )
  ::oSuffix:getName():caption := "Suffix"

  ::oTitle                          := ContactItem():new( ::drawingArea )
  ::oTitle:getName():caption := "Title"

  ::oNextButton := XbpPushButton():new( ::drawingArea )
  ::oNextButton:caption := "Next"
  ::oNextButton:activate := {|| ::navigateNextContact() }

  ::oPrevButton := XbpPushButton():new( ::drawingArea )
  ::oPrevButton:caption := "Prev"
  ::oPrevButton:activate := {|| ::navigatePrevContact() }

  ::oExportButton := XbpPushButton():new( ::drawingArea )
  ::oExportButton:caption := "Export"
  ::oExportButton:activate := {|| ::exportContact() }


  ::oContacts := ContactNavigator():new()
RETURN self

/// <summary>
/// <para>
/// Create Dialog and Dialog IVars
/// </para>
/// </summary>
/// <returns>
/// self
/// </returns>
METHOD ContactDialog:create( oParent, oOwner, aPos, aSize, aPresParam, lVisible )

  ::XbpDialog:create( oParent, oOwner, aPos, aSize, aPresParam, lVisible )

  ::drawingArea:hide()

  ::oHint:create()
  ::oBAddrCity:create()
  ::oBAddrCountry:create()
  ::oBAddrPostalCode:create()
  ::oBAddrPostOfficeBox:create()
  ::oBAddrState:create()
  ::oBAddrStreet:create()
  ::oBAddrFaxNumber:create()
  ::oBAddrHomePage:create()
  ::oBAddrTelephoneNumber:create()
  ::oBAddrCallBackTelephoneNumber:create()
  ::oCompanyName:create()
  ::oEmail1Address:create()
  ::oFirstName:create()
  ::oMiddleName:create()
  ::oLastname:create()
  ::oJobTitle:create()
  ::oCreationTime:create()
  ::oLastModificationTime:create()
  ::oBirthday:create()
  ::oAnniversary:create()
  ::oAssistantName:create()
  ::oEmail1DisplayName:create()
  ::oInitials:create()
  ::oNickName:create()
  ::oProfession:create()
  ::oSuffix:create()
  ::oTitle:create()

  ::oNextButton:create()
  ::oPrevButton:create()
  ::oExportButton:create()

  ::updateDialog()

RETURN self

/// <summary>
/// <para>
/// Show the dialog modal
/// </para>
/// </summary>
/// <returns>
/// self
/// </returns>
METHOD ContactDialog:showModal()

  LOCAL nEvent := NIL, mp1 := NIL, mp2 := NIL, oXbp := NIL

  // Set focus and handle all events.

  SetAppFocus( self )

  nEvent := 0
  DO WHILE nEvent <> xbeP_Close
    nEvent := AppEvent( @mp1, @mp2, @oXbp )
    oXbp:handleEvent( nEvent, mp1, mp2 )
  ENDDO

RETURN self


/// <summary>
/// <para>
/// Destroy Dialog
/// </para>
/// </summary>
/// <returns>
/// self
/// </returns>
METHOD ContactDialog:destroy()
  ::oContacts:destroy()

  ::XbpDialog:destroy()
RETURN self

/// <summary>
/// <para>
/// Resize Dialog
/// </para>
/// </summary>
/// <returns>
/// self
/// </returns>
METHOD ContactDialog:resize()

  LOCAL oXbp, aXbps, n, nLen
  LOCAL nY, nX

  ::drawingArea:hide()

  // The XbaseParts at the right handside of the drawingArea will
  // be changed in size and position
  nY := ::drawingArea:currentSize()[2] - XBP_VERT_SPACE
  nX := 2 * XBP_HORIZ_SPACE + XBP_WIDTH

  nY -= 8 * XBP_HEIGHT
  ::oHint:setPosAndSize( { nX, nY } , ;
                         { XBP_WIDTH, 8 * XBP_HEIGHT}, .F. )

  nY := nY - XBP_VERT_SPACE - XBP_HEIGHT
  ::oPrevButton:setPosAndSize( { nX, nY }, ;
                               { XBP_WIDTH/3, XBP_HEIGHT }, .F. )

  ::oNextButton:setPosAndSize( { nX + XBP_WIDTH/3, nY }, ;
                               { XBP_WIDTH/3, XBP_HEIGHT }, .F.)

  ::oExportButton:setPosAndSize( { nX + 2 * XBP_WIDTH/3, nY }, ;
                                 { XBP_WIDTH/3, XBP_HEIGHT }, .F.)

  // Prepare nY for positioning the xbase parts to the lefthand side
  // of the drawing area.
  nY := ::drawingArea:currentSize()[2] - XBP_VERT_SPACE

  // Collect all XbaseParts of the left collumn
  aXbps := {                                    ;
             ::oCreationTime                   ,;
             ::oLastModificationTime           ,;
             ::oInitials                       ,;
             ::oTitle                          ,;
             ::oNickName                       ,;
             ::oFirstName                      ,;
             ::oMiddleName                     ,;
             ::oLastname                       ,;
             ::oSuffix                         ,;
             ::oEmail1DisplayName              ,;
             ::oEmail1Address                  ,;
             ::oBirthday                       ,;
             ::oAnniversary                    ,;
             ::oProfession                     ,;
             ::oJobTitle                       ,;
             ::oCompanyName                    ,;
             ::oBAddrStreet                    ,;
             ::oBAddrPostalCode                ,;
             ::oBAddrCity                      ,;
             ::oBAddrCountry                   ,;
             ::oBAddrState                     ,;
             ::oBAddrPostOfficeBox             ,;
             ::oBAddrFaxNumber                 ,;
             ::oBAddrHomePage                  ,;
             ::oBAddrTelephoneNumber           ,;
             ::oBAddrCallBackTelephoneNumber   ,;
             ::oAssistantName                  ;
           }

  // Set new positon and new size of all XbaseParts
  nLen := Len( aXbps )
  FOR n := 1 TO nLen
    nY := nY - XBP_VERT_SPACE - XBP_HEIGHT

    oXbp := aXbps[n]
    oXbp:setPosAndSize( { XBP_HORIZ_SPACE, nY }, { XBP_WIDTH, XBP_HEIGHT }, .F. )

  NEXT

  ::drawingArea:show()

RETURN self

/// <summary>
/// <para>
/// Navigate to and display next contact
/// </para>
/// </summary>
/// <returns>
/// self
/// </returns>
METHOD ContactDialog:navigateNextContact()
  ::oContacts:goNextContact()
  ::updateDialog()
RETURN self

/// <summary>
/// <para>
/// Navigate to and display previous contact
/// </para>
/// </summary>
/// <returns>
/// self
/// </returns>
METHOD ContactDialog:navigatePrevContact()
  ::oContacts:goPrevContact()
  ::updateDialog()
RETURN self

/// <summary>
/// <para>
/// Create a DBF Table
/// </para>
/// </summary>
/// <returns>
/// self
/// </returns>
METHOD ContactDialog:exportContact()

  // exporting to DBF is a time consuming operation,
  // thus all buttons are disabled while exporting
  // is in progress
  ::oPrevButton:disable()
  ::oNextButton:disable()
  ::oExportButton:disable()

  ::oContacts:importDbf()

  MsgBox( "OUTLOOK.DBF successfully created", "Finished" )

  ::oPrevButton:enable()
  ::oNextButton:enable()
  ::oExportButton:enable()
RETURN self

/// <summary>
/// <para>
/// Create a new instance of the dialog
/// </para>
/// </summary>
/// <returns>
/// self
/// </returns>
FUNCTION CreateContactDialog()

  LOCAL oDlg

  oDlg := ContactDialog():new( AppDesktop(), , {100, 100} )

  oDlg:create()

RETURN oDlg

/// <summary>
/// <para>
/// Return the hint string
/// </para>
/// </summary>
/// <returns>
/// cHint
/// </returns>
FUNCTION GetHintText()

  LOCAL cHint := ""

  cHint += CRLF + SPACE
  cHint += "Use the buttons 'Next' and 'Prev' for navigating the Outlook "
  cHint += "Contacts."
  cHint += CRLF + CRLF + SPACE
  cHint += "Use the button 'Export' for creating a DBF file named "
  cHint += "'outlook.dbf'."
  cHint += CRLF + SPACE
  cHint += "Each outlook contact will be stored in a record of the DBF file."


RETURN cHint
