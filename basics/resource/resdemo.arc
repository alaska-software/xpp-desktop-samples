//////////////////////////////////////////////////////////////////////
//
//  RESDEMO.ARC
//
//  Copyright:
//           Alaska Software, (c) 1997-2025. All rights reserved.         
//  
//  Contents:
//           Defines several resources
//////////////////////////////////////////////////////////////////////


#include "resdemo.ch"

POINTER
#ifdef __OS2__
        RD_CROSS  = "BCROSS.PTR"
        RD_BEAM   = "ABEAM.PTR"
        RD_NODROP = "NODROP.PTR"
        RD_ARROW  = "FIREW.PTR"
        RD_MOVE   = "XMAS.PTR"
#endif
#ifdef __WIN32__
        RD_CROSS  = "BCROSS.CUR"
        RD_BEAM   = "ABEAM.CUR"
        RD_NODROP = "NODROP.CUR"
        RD_ARROW  = "FIREW.CUR"
        RD_MOVE   = "XMAS.CUR"
#endif

ICON
        RD_XPP    = "Xpp.ico"
        RD_FLD    = "XppFld.ico"
        RD_XPPFD  = "XppBook.ico"
        RD_XFF    = "XppXff.ico"
        RD_PBUILD = "Pbuild.ico"
        
BITMAP
        RD_BMP1   = FILE "Tile3.bmp"
        RD_BMP2   = FILE "Tile5.bmp"
        RD_BMP3   = FILE "Tile7.bmp"
        RD_BMP4   = FILE "Tile9.bmp"
        RD_BMP5   = FILE "Tile10.bmp"

USERDEF JPEG 
        TILE1     = FILE "Tile1.jpg"


