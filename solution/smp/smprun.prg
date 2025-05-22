//////////////////////////////////////////////////////////////////////
//
//  SMPRUN.PRG
//
//  Copyright:
//      Alaska Software, (c) 2001-2025. All rights reserved.         
//  
//  Contents:
//      SmpRun - Attach/Detach process to/from CPU/Core.
//////////////////////////////////////////////////////////////////////


#include "dll.ch"
#include "inkey.ch"


/*
 * Test loop to retrieve the assigend CPU and to assign a CPU
 * Input: 1-32 -> Assign the CPU
 *           0 -> End sample
 */
PROCEDURE Main
    LOCAL nCpu, nCpuMask

    ? "Input: 1-32 -> Assign the CPU"
    ? "          0 -> End sample"
    ?

    /*
     * Stress the CPU to get an indication
     */
    Thread():new():start("stress")
    DO WHILE .T.
        nCpu := SmpGetCPU()
        ? "Application currently runs on CPU: " + Var2char(BitSet2Array(nCpu))

        INPUT "Enter new CPU: " TO nCpu

        IF nCpu = 0
           EXIT
        ENDIF
        nCpuMask := 0
        nCpuMask[nCpu] := .T.
        IF SmpSetCPU(nCpuMask) = 0
           ? "Cannot set CPU to:" + var2char(nCpu)
        END
    ENDDO
RETURN


/*
 * The required functions are part of the runtime library.
 */
FUNCTION SmpGetCPU()
    LOCAL nCpuMask
    nCpuMask := DllCall("Xpprt1.dll",DLL_CDECL, "_sysGetCPU")
RETURN nCpuMask


FUNCTION SmpSetCPU(nCpuMask)
    rc := DllCall("Xpprt1.dll",DLL_CDECL, "_sysSetCPU", nCpuMask)
RETURN rc

FUNCTION BitSet2Array(n)
    LOCAL i
    LOCAL aSet:= {}
    // transform mask to array
    FOR i:= 1 TO 32
        IF n[i]
            AAdd(aSet, i)
        ENDIF
    NEXT
RETURN aSet
/*
 * Just something to do..
 */
PROCEDURE Stress()
    LOCAL a:= {3,2,1,0}
    DO WHILE .T.
        ASort(a)
    ENDDO
RETURN
