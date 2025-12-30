*>*****************************************************************
*> FILE: main_payroll.cbl
*>*****************************************************************

IDENTIFICATION DIVISION.
    PROGRAM-ID. MAIN-PAYROLL.
    *> Description: Main program to read CSV and process payroll totals.

    ENVIRONMENT DIVISION.

    INPUT-OUTPUT SECTION.
    FILE-CONTROL.
        SELECT CSV-FILE ASSIGN TO 'payroll.csv'
        ORGANIZATION IS LINE SEQUENTIAL.

        SELECT REPORT-FILE ASSIGN TO 'totals.txt'
        ORGANIZATION IS LINE SEQUENTIAL.

    DATA DIVISION.

    FILE SECTION.
    FD  CSV-FILE.
    01  CSV-RECORD           PIC X(100).

    FD  REPORT-FILE.
    01  REPORT-RECORD        PIC X(50).

    WORKING-STORAGE SECTION.
    01  WS-EOF               PIC X(01) VALUE 'N'.
    *> Accumulator: PIC 9(10)V99 supports up to 9,999,999,999.99
    01  WS-TOTAL-GROSS       PIC 9(10)V99 VALUE ZERO.

    *> Fields to hold parsed CSV data
    01  WS-EMP-DATA.
        05 WS-EMP-ID        PIC X(05).
        05 WS-EMP-SALARY    PIC X(12).

    *> Formatted output for the report
    01  WS-OUTPUT-LINE.
        05 FILLER            PIC X(13) VALUE "Total Gross: ".
        05 WS-OUT-TOTAL      PIC Z,ZZZ,ZZZ,ZZ9.99.

    PROCEDURE DIVISION.
    0100-START-LOGIC.
        OPEN INPUT CSV-FILE
                OUTPUT REPORT-FILE

        PERFORM UNTIL WS-EOF = 'Y'
            READ CSV-FILE
                AT END
                    MOVE 'Y' TO WS-EOF
                NOT AT END
                    PERFORM 0200-PROCESS-CSV-LINE
            END-READ
        END-PERFORM

        PERFORM 0300-WRITE-REPORT

        CLOSE CSV-FILE
                REPORT-FILE

        STOP RUN
        . *> END 0100-START-LOGIC

    0200-PROCESS-CSV-LINE.
        MOVE SPACES TO WS-EMP-ID WS-EMP-SALARY

        *> Parsing CSV using UNSTRING.
        *> It splits the line using ',' as delimiter.
        UNSTRING CSV-RECORD DELIMITED BY ","
            INTO WS-EMP-ID,
                 WS-EMP-SALARY
        END-UNSTRING

        *> Convert the string "1234.56" to a numeric value and sum it.
        ADD FUNCTION NUMVAL(WS-EMP-SALARY) TO WS-TOTAL-GROSS.

        DISPLAY "Processed: " WS-EMP-ID " - Salary: " WS-EMP-SALARY
        . *> END 0200-PROCESS-CSV-LINE

    0300-WRITE-REPORT.
        MOVE WS-TOTAL-GROSS TO WS-OUT-TOTAL
        MOVE WS-OUTPUT-LINE TO REPORT-RECORD
        WRITE REPORT-RECORD.
        DISPLAY "Final Total Written to File: " WS-OUT-TOTAL
        . *> 0300-WRITE-REPORT
