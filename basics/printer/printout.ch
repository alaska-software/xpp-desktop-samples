//////////////////////////////////////////////////////////////////////
//
//  PRINTOUT.CH
//
//  Copyright:
//      Alaska Software, (c) 1997-2025. All rights reserved.         
//  
//  Contents:
//      This file header contains defines and definitions used by the 
//      "Printers" sample
//     
//////////////////////////////////////////////////////////////////////


/*
 * Dialog and control window captions
 */
#define WINDOW_CAPTION          "Printers"
#define PREVIEW_CAPTION         "Preview"
#define GROUPBOX_CAPTION        "Installed Printers:"
#define CLOSEPUSH_CAPTION       "Close"
#define OPTIONSPUSH_CAPTION     "Printer Settings"
#define PREVIEWPUSH_CAPTION     "Preview"
#define PRINTPUSH_CAPTION       "Print Testpage"

/*
 * Window placing and sizing
 */
/* Default dialog and child window sizes     */
#define DEFWINDOW_SIZE_X        488
#define DEFWINDOW_SIZE_Y        252
#define PREVIEWWINDOW_SIZE_X    300
#define PREVIEWWINDOW_SIZE_Y    200

#define PUSHBUTTON_SIZE_X       105
#define PUSHBUTTON_SIZE_Y       30

/* Default spacing between GUI objects       */
#define STD_SPACING_X           15
#define STD_SPACING_Y           10

/* 
 * Ordinals for retrieving child windows 
 * from the parent's child list (Printers.prg)
 */
#define GROUPBOX_ORDINAL        1
#define LISTBOX_ORDINAL         2
#define CLOSEPUSH_ORDINAL       3
#define PRINTPUSH_ORDINAL       4
#define PREVIEWPUSH_ORDINAL     5

#define NUM_CHILDS              5
