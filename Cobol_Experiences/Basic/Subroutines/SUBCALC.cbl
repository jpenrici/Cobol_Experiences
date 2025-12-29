*>*****************************************************************
*> FILE: SUBCALC.cbl
*>*****************************************************************

IDENTIFICATION DIVISION.
    PROGRAM-ID. SUBCALC.
    *> PURPOSE: Calculate Average From A Passed Table

    DATA DIVISION.
    WORKING-STORAGE SECTION.

    01  WS-CALCS.
        05 WS-SUM            PIC 9(07) VALUE 0.
        05 L-IDX             PIC 9(02).

    LINKAGE SECTION.

    01  LK-TABLE-DATA.
        05 LK-SIZE           PIC 9(02).
        05 LK-NUMBERS        PIC 9(03) OCCURS 1 TO 50 TIMES
                             DEPENDING ON Lk-SIZE.

    01  LK-RESULT-AVG        PIC 9(03)V99.

    *> STATUS CODES: 00 = OK, 01 = EMPTY, 02 = NEGATIVE FOUND
    01  LK-STATUS            PIC X(02).

    PROCEDURE DIVISION USING LK-TABLE-DATA, LK-RESULT-AVG, LK-STATUS.
    BEGIN-CALC.

        MOVE "00" TO LK-STATUS
        INITIALIZE WS-SUM LK-RESULT-AVG.

        *> Check if empty table
        IF LK-SIZE = 0
            MOVE "01" TO LK-STATUS
            EXIT PROGRAM
        END-IF.

        *> Logical check
        PERFORM VARYING L-IDX FROM 1 BY 1 UNTIL L-IDX > LK-SIZE
            IF LK-NUMBERS(L-IDX) = 0
                MOVE "02" TO LK-STATUS
                EXIT PROGRAM
            END-IF
            ADD LK-NUMBERS(L-IDX) TO WS-SUM
        END-PERFORM.

        COMPUTE LK-RESULT-AVG = WS-SUM / LK-SIZE.

        EXIT PROGRAM.
