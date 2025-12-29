IDENTIFICATION DIVISION.
    PROGRAM-ID. SUBCALC.
    *> PURPOSE: Calculate Average From A Passed Table

    DATA DIVISION.
    WORKING-STORAGE SECTION.

    01  WS-CALCS.
        05 WS-SUM            PIC 9(05) VALUE 0.
        05 L-IDX             PIC 9(02).

    LINKAGE SECTION.

    *> THIS MUST MATCH THE STRUCTURE PASSED BY THE CALLER
    01  LK-TABLE-DATA.
        05 LK-NUMBERS        PIC 9(03) OCCURS 1 TO 50 TIMES
                                DEPENDING ON Lk-SIZE.
    01  LK-RESULT-AVG        PIC 9(03)V99.
    01  LK-SIZE              PIC 9(02).

    PROCEDURE DIVISION USING LK-TABLE-DATA, LK-RESULT-AVG, LK-SIZE.
    BEGIN-CALC.

        INITIALIZE WS-SUM.

        IF LK-SIZE > 0   *> Avoid division by zero
                PERFORM VARYING L-IDX FROM 1 BY 1 UNTIL L-IDX > LK-SIZE
                    ADD LK-NUMBERS(L-IDX) TO WS-SUM
                END-PERFORM
                COMPUTE LK-RESULT-AVG = WS-SUM / LK-SIZE
        ELSE
                MOVE 0 TO LK-RESULT-AVG
        END-IF

        EXIT PROGRAM.
