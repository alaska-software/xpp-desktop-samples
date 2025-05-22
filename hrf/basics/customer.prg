//////////////////////////////////////////////////////////////////////
//
//  CUSTOMER.PRG
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
//      - fill the input elements and select comboboxes with values
//      - write back the document to a file
//   
//  Syntax:
//      CUSTOMER
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

    LOCAL oDocument, oCountry

    // Load our document
    ? "We first load the document"
    WAIT
    oDocument := HTMLDocument():LoadFile( "custome_tmpl.htm" )
    IF oDocument == NIL
       Alert("Template file $CUSTOMER.HTM not found")
       QUIT
    ENDIF
    ? "-> done"

    // Now we fill the Country combobox with some countries which we know
    ? "Now we fill the Country combobox with some countries which we know"
    WAIT
    oCountry := oDocument:GetElementByName( "Country" )
    oCountry:Add( "France" )
    oCountry:Add( "Great Britain" )
    oCountry:Add( "Germany" )
    oCountry:Add( "Hungary" )
    oCountry:Add( "Italy" )
    oCountry:Add( "United States of America" )
    ? "-> done"

    // now we alter the contents of the customer data
    ? "Now we alter the contents of the customer data"
    WAIT
    oDocument:GetElementByName( "Name" ):Value    := "Please enter your last name here"
    oDocument:GetElementByName( "Surname" ):Value := "Please enter your first name here"
    ? "-> done"

    // now write the document back
    ? "Now we write the document back and display it"
    WAIT
    oDocument:SaveFile( "Customer.htm" )

    // now launch the default browser with the file
    ShellOpenFile( "Customer.htm" )
    ? "-> done"

 RETURN

 // EOF
