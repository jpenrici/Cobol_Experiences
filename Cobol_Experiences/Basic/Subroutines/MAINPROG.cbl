IDENTIFICATION DIVISION.
    PROGRAM-ID. MAINPROG.
    *> PURPOSE: Demonstrate Calls And Tables (Using 'Depending On')

    DATA DIVISION.
    WORKING-STORAGE SECTION.

    01  WS-CONTROL-VARS.
        *> Current size of the table
        05 WS-CURRENT-SIZE   PIC 9(02) VALUE 0.

    01  WS-TABLE-DATA.
        05 WS-NUMBERS    PIC 9(03)
                            OCCURS 1 TO 50 TIMES
                            DEPENDING ON WS-CURRENT-SIZE.

    01  WS-RESULTS.
        05 WS-AVERAGE        PIC 9(03)V99.
        05 WS-DISPLAY-AVG    PIC ZZZ.99.

    01  WS-COUNTERS.
        05 I                 PIC 9(02).

    PROCEDURE DIVISION.
    MAIN-LOGIC.

        DISPLAY "--- MAIN PROGRAM STARTED ---".

        MOVE 15 TO WS-CURRENT-SIZE.

        PERFORM FILL-DATA.

        *> Calling External Sub-program
        *> Passing The Entire Table And The Result Variable
        CALL "SUBCALC" USING WS-TABLE-DATA, WS-AVERAGE, WS-CURRENT-SIZE.

        MOVE WS-AVERAGE TO WS-DISPLAY-AVG.
        DISPLAY "Elemnts processed:" WS-CURRENT-SIZE.
        DISPLAY "The calculated average is: " WS-DISPLAY-AVG.

        DISPLAY "--- MAIN PROGRAM ENDED ---".
        STOP RUN.

    FILL-DATA.
        *> Filling Table With Sample Data
        PERFORM VARYING I FROM 1 BY 1 UNTIL I > WS-CURRENT-SIZE
                COMPUTE WS-NUMBERS(I) = I * 10
        END-PERFORM
        . *> END FILL-DATA
