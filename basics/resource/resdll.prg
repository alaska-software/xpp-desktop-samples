//////////////////////////////////////////////////////////////////////
//
//  RESDLL.PRG
//
//  Copyright:
//           Alaska Software, (c) 1997-2025. All rights reserved.         
//  
//  Contents:
//           Demonstrates the usage of Resource DLLs with Xbase++
//   
//  Remarks:
//           The resources are defined in RESDLL.ARC
//   
//////////////////////////////////////////////////////////////////////


#include "Gra.ch"
#include "Xbp.ch"
#include "Appevent.ch"
#pragma library ("Xppui2")
 
PROCEDURE Main 
LOCAL nEvent, mp1, mp2, oXbp, oDlg

      oDlg := SetAppWindow()

      oXbp := XbpPushButton():new( , , {1,340}, {280,40} ) 
      oXbp:caption := "Create new tree view" 
      oXbp:preSelect := .T.
      oXbp:create() 
      oXbp:activate:= {|| CreateTreeView(oDlg) } 

      @  8, 38 SAY "This sample demonstrates the usage of"
      @  9, 38 SAY "Resource DLL's as a powerful mechanism"
      @ 10, 38 SAY "to dynamically change the appearance of"
      @ 11, 38 SAY "the application."
      @ 13, 38 SAY "You can try to load any DLL which"
      @ 14, 38 SAY "contains bitmap and/or icon resources"
      @ 15, 38 SAY "in the id range 10..73."
      @ 16, 38 SAY "Please also try XppNat.Dll"
 
      DO WHILE nEvent <> xbeP_Close 
         nEvent := AppEvent( @mp1, @mp2, @oXbp ) 
         oXbp:handleEvent( nEvent, mp1, mp2 ) 
      ENDDO 
RETURN 

/*
 * Select a DLL containing resources
 */
FUNCTION SelectResourceDll(oParent)
LOCAL cName, oXbp

      oXbp := XbpFileDialog():new()
      oXbp:defExtension := "DLL"
      oXbp:validatePath := .T.
      oXbp:title := "Select a resouce DLL"
      oXbp:create()
      cName := oXbp:open("IMAGES.DLL", .T.)
      IF cName != NIL
            cName:=DllLoad(cName)
      ENDIF    
RETURN cName

/*
 * Create a tree view, choose a resource DLL first
 */
FUNCTION CreateTreeView(oParent)
LOCAL oItem1, oItem2 
STATIC oTree
LOCAL cDll

      IF oTree != NIL
           oTree:destroy()
      ENDIF

      cDll := SelectResourceDll()

      oTree := XbpTreeView():new(oParent ,, {0,0}, {280,330} ) 
 
      oTree:hasLines   := .T. 
      oTree:hasButtons := .T. 
      oTree:create() 
 
      oItem1 := oTree:rootItem:addItem( CreateItem("Level A1",  10, cDll))

      oTree:rootItem:addItem( CreateItem("Level B1",  20, cDll) ) 

      oItem2 := oItem1:addItem( CreateItem("Level A2",  30, cDll) ) 
 
      oItem1:addItem( CreateItem("Level B2",  40, cDll) ) 
 
      oItem2:addItem( CreateItem("Level A3",  50, cDll) ) 
      oItem2:addItem( CreateItem("Level B3",  60, cDll) ) 
      oItem2:addItem( CreateItem("Level C3",  70, cDll) ) 

      oTree:rootItem:expand(.T.)
RETURN oTree

/*
 * Create a tree view item having different
 * resource ids
 */
FUNCTION CreateItem(cCaption, nId, cDll)
LOCAL oItem
      oItem := XbpTreeViewItem():new()
      oItem:dllName := cDll      
      oItem:caption := cCaption
      oItem:image := nId
      oItem:markedImage := nId+1
      oItem:expandedImage := nId+2
      oItem:create()
RETURN oItem
      
