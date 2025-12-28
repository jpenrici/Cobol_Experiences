*>*****************************************************************
*> Purpose: Studying the behavior of Tables.
*> Tect Stack:  GnuCOBOL
*>*****************************************************************

IDENTIFICATION DIVISION.
    PROGRAM-ID. MatrixStudy.

    DATA DIVISION.
    WORKING-STORAGE SECTION.

    *> Defining Tables (similar to "Arrays" in "modern" languages).
    *> Level 01 is the root record.
    01  SALES-DATA.
        05 VENDOR-ROW OCCURS 5 TIMES INDEXED BY V-IDX.
            10 MONTH-COL OCCURS 3 TIMES INDEXED BY M-IDX.
                15 SALES-VALUE       PIC 9(04)V99 VALUE ZERO.

    *> Variable for search control
    01 WS-FOUND-FLAG    PIC X VALUE 'N'.
        88 FOUND    VALUE 'Y'.
        88 NOT-FOUND VALUE 'N'.

    *> Variables for logic and display
    01  WS-TOTAL-SALES             PIC 9(06)V99 VALUE ZERO.
    01  WS-DISPLAY-MSG             PIC X(100).
    01  WS-BONUS-THRESHOLD         PIC 9(04)    VALUE 500.
    01  WS-FORMATTED-VAL           PIC ZZZ9.99. *> Z-Usage
    01  WS-V-IDX-DISPLAY           PIC 9.
    01  WS-M-IDX-DISPLAY           PIC 9.

    PROCEDURE DIVISION.
    000-MAIN.

        DISPLAY "--- STARTING COBOL MATRIX PROCESSING ---"

        PERFORM 100-INITIALIZE-DATA
        PERFORM 200-POPULATE-DATA
        PERFORM 300-PROCESS-AND-DISPLAY
        PERFORM 400-SEARCH-LOGIC    *> Extra

        DISPLAY "--- PROCESSING COMPLETE ---"
        STOP RUN.

    100-INITIALIZE-DATA.
        INITIALIZE SALES-DATA *> Clears the table memory
        . *> END 100-INITIALIZE-DATA

    200-POPULATE-DATA.
        *> Simulating data entry into the matrix via a loop.
        PERFORM VARYING V-IDX FROM 1 BY 1 UNTIL V-IDX > 5
            PERFORM VARYING M-IDX FROM 1 BY 1 UNTIL M-IDX > 3

                COMPUTE SALES-VALUE(V-IDX, M-IDX) =
                    (V-IDX * 100) + (M-IDX * 50)

            END-PERFORM
        END-PERFORM
        . *> END 200-POPULATE-DATA.

    300-PROCESS-AND-DISPLAY.
        PERFORM VARYING V-IDX FROM 1 BY 1 UNTIL V-IDX > 5
            PERFORM VARYING M-IDX FROM 1 BY 1 UNTIL M-IDX > 3

                *> If SALES-VALUE(row, col) > WS-BONUS-THRESHOLD
                *> If that's true, apply a bonus.
                IF SALES-VALUE(V-IDX, M-IDX) IS GREATER THAN
                    WS-BONUS-THRESHOLD

                    *> Move index to numeric variable
                    SET WS-V-IDX-DISPLAY TO V-IDX
                    SET WS-M-IDX-DISPLAY TO M-IDX

                    COMPUTE SALES-VALUE(V-IDX, M-IDX) =
                            SALES-VALUE(V-IDX, M-IDX) * 1.10

                    MOVE SALES-VALUE(V-IDX, M-IDX) TO WS-FORMATTED-VAL

                    *> Concatenate the result message.
                    INITIALIZE WS-DISPLAY-MSG
                    STRING "Bonus applied to "
                        "VENDOR-ROW:" WS-V-IDX-DISPLAY
                        " MONTH-COL:" WS-M-IDX-DISPLAY
                        " Value: " WS-FORMATTED-VAL
                        DELIMITED BY SIZE INTO WS-DISPLAY-MSG

                    DISPLAY WS-DISPLAY-MSG
                END-IF

            END-PERFORM
        END-PERFORM
        . *> END 300-PROCESS-AND-DISPLAY.

    400-SEARCH-LOGIC.
        DISPLAY "--- SEARCHING FOR TOP VENDOR (MONTH 1) ---"

        SET V-IDX TO 1
        SET NOT-FOUND TO TRUE

        SEARCH VENDOR-ROW
            AT END
                DISPLAY "No vendor reached the goal."
            WHEN SALES-VALUE(V-IDX, 1) > 600
                SET FOUND TO TRUE
                SET WS-V-IDX-DISPLAY TO V-IDX
                DISPLAY "Vendor found: " WS-V-IDX-DISPLAY
                DISPLAY "Value: " SALES-VALUE(V-IDX, 1)
        END-SEARCH
        . *> END 400-SEARCH-LOGIC.
