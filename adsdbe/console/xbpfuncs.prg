//////////////////////////////////////////////////////////////////////
//
//  XBPFUNCS.PRG
//
//  Copyright:
//   Alaska Software, (c) 1999-2025. All rights reserved.         
//  
//  Contents:
//       Alaska Management Console for the Advantage Database Server
//   
//       Functions to create Xbase-parts
//////////////////////////////////////////////////////////////////////


// Create a listbox.
FUNCTION CreateListBox(oParent, oOwner, aPos, aSize, aPP, lVisible)
RETURN XbpListBox():New(oParent, oOwner, aPos, aSize, aPP, lVisible):Create()

// Create a static.
FUNCTION CreateStatic(oParent, aPos, aSize, cCaption, cFont)
   LOCAL oxbp := XbpStatic():New(oParent, oParent, aPos, aSize)
   
   IF cFont != NIL
      oXbp:setfontcompoundname( cFont )
   ENDIF 

   IF cCaption != NIL
      oXbp:caption := cCaption
   ENDIF 

   oXbp:Create()
RETURN oXbp

