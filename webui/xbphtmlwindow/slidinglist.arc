//////////////////////////////////////////////////////////////////////
//
//  SLIDINGLIST.ARC
//
//  Copyright:
//      Alaska Software, (c) 2015-2025. All rights reserved.         
//  
//  Contents:
//      Definition of the resources used by the control. These are compiled
//      using the Resource Compiler and get linked into the .EXE file. In
//      the example, the control loads a background image and the HTML and
//      CSS definitions and templates from the application's resource.
//      
//////////////////////////////////////////////////////////////////////


USERDEF PNG
  BGIMAGE          = FILE "resources\alaska-logo.png"

USERDEF HTML
  ITEMLIST         = FILE "resources\slidinglist.htm"
  ITEMTEMPLATE     = FILE "resources\itemtemplate.htm"
  ITEMTEXTTEMPLATE = FILE "resources\itemtexttemplate.htm"
