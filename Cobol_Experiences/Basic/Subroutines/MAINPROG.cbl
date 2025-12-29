*>*****************************************************************
*> FILE: MAINPROG.cbl
*>*****************************************************************

IDENTIFICATION DIVISION.
    PROGRAM-ID. MAINPROG.
    *> PURPOSE: Demonstrate Calls And Tables (Using 'Depending On')

    DATA DIVISION.
    WORKING-STORAGE SECTION.

    01  WS-TABLE-DATA.
        05 WS-TABLE-SIZE      PIC 9(02) VALUE 0.
        05 WS-TABLE-NUMBERS   PIC 9(03)
                              OCCURS 1 TO 50 TIMES
                              DEPENDING ON WS-TABLE-SIZE.

    01  WS-RESULTS.
        05 WS-AVERAGE        PIC 9(03)V99.
        05 WS-STATUS         PIC X(02).

    01  WS-COUNTERS.
        05 I                 PIC 9(02).

    01  WS-DISPLAY.
        05 WS-DISPLAY-AVG    PIC ZZZ.99.

    01  WS-REPORT-LINE       PIC X(80).

    PROCEDURE DIVISION.
    MAIN-LOGIC.

        DISPLAY "--- MAIN PROGRAM STARTED ---".

        MOVE 15 TO WS-TABLE-SIZE.

        PERFORM FILL-DATA.

        *> Calling External Sub-program
        *> Passing The Entire Table And The Result Variable
        CALL "SUBCALC" USING WS-TABLE-DATA,
                             WS-AVERAGE,
                             WS-STATUS.

        EVALUATE WS-STATUS
            WHEN "00"
                MOVE WS-AVERAGE TO WS-DISPLAY-AVG

                INITIALIZE WS-REPORT-LINE
                STRING "RESULT: The average of "   DELIMITED BY SIZE
                    WS-TABLE-SIZE                  DELIMITED BY SIZE
                    " elements is "                DELIMITED BY SIZE
                    WS-DISPLAY-AVG                 DELIMITED BY SIZE
                    INTO WS-REPORT-LINE
                DISPLAY WS-REPORT-LINE
            WHEN "01"
                DISPLAY "ERROR: Table is empty."
            WHEN "02"
                DISPLAY "ERROR: Unkown status" WS-STATUS
        END-EVALUATE.

        DISPLAY "--- MAIN PROGRAM ENDED ---".
        STOP RUN.

    FILL-DATA.
        *> Filling Table With Sample Data
        PERFORM VARYING I FROM 1 BY 1 UNTIL I > WS-TABLE-SIZE
                COMPUTE WS-TABLE-NUMBERS(I) = I * 10
        END-PERFORM
        . *> END FILL-DATA
