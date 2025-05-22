//////////////////////////////////////////////////////////////////////
//
//  CUSTTABL.PRG
//
//  Copyright:
//      Alaska Software, (c) 2000-2023. All rights reserved.         
//  
//  Contents:
//      Sample for HRF
//   
//  Remarks:
//      We will demonstrate a basic usage of the HRF
//      - load a document from a template string
//      - fill a table in the document with the contents of a database
//      - write back the document to a file
//   
//  Syntax:
//      CUSTTABL
//   
//  Return:
//      returns 0 always
//   
//////////////////////////////////////////////////////////////////////


 /* **********************************************************************
  * Force the library to be added to the program
  * */
 #pragma Library( "HRFCLASS.LIB" )

 /* **********************************************************************
  * Our main program
  * */
 PROCEDURE Main

    LOCAL oDocument, oCustomer, oRow, oNewRow, lChgClr := .T.
    FIELD CUSTNO, MR_MRS, LASTNAME, FIRSTNAME
    FIELD ZIP, STREET, CITY, PHONE, FAX

    // Load our document
    ? "We first load the document"
    WAIT
    oDocument := HTMLDocument():LoadFile( "custtab_tmpl.htm" )
    IF oDocument == NIL
       Alert("Template file CUSTTAB_TMPL.HTM not found")
       QUIT
    ENDIF
    ? "-> done"

    // Now we fill the Customer table with our customer information
    ? "Now we fill the Customer table with our customer information"
    WAIT

    // get the customer table, we assigned it with the IDE "Customers"
    oCustomer := oDocument:GetElementByID( "Customers" )

    // now get its second row, this row is assumed to contain the data,
    // we clone this row and remove it from the table
    oRow := oCustomer:Rows[2]:Clone( NIL )
    oCustomer:Rows[2]:Destroy()

    // after the row was cloned and removed, we will copy the row to
    // the table after filling it with data
    USE ..\..\data\misc\Customer
    WHILE !EOF()
       oNewRow := oRow:Clone()
       oNewRow:GetElement( "CID" ):SetContent( CUSTNO )
       oNewRow:GetElement( "GENDER" ):SetContent( MR_MRS )
       oNewRow:GetElement( "NAME" ):SetContent( LASTNAME )
       oNewRow:GetElement( "SURNAME" ):SetContent( FIRSTNAME )
       oNewRow:GetElement( "ZIP" ):SetContent( ZIP )
       oNewRow:GetElement( "ADRESS" ):SetContent( STREET )
       oNewRow:GetElement( "CITY" ):SetContent( CITY )
       oNewRow:GetElement( "PHONE" ):SetContent( PHONE )
       oNewRow:GetElement( "FAX" ):SetContent( FAX )
       IF ( lChgClr := !lChgClr )
          oNewRow:BGColor := "#CCCCCC"
       ENDIF
       oNewRow:SetParent( oCustomer )
       SKIP 1
    ENDDO
    CLOSE ALL
    ? "-> done"

    // Now write the document back
    ? "Now we write the document back and display it"
    WAIT
    oDocument:SaveFile( "custtabl.htm" )

    // Now launch the default web browser with the file
    ShellOpenFile( "custtabl.htm" )
    ? "-> done"

 RETURN

 // EOF
