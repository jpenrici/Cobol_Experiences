*>*****************************************************************
*> FILE: main.cbl
*>*****************************************************************

IDENTIFICATION DIVISION.
       PROGRAM-ID. MAIN-TEST.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       *> This structure must be identical to the one in Subroutine
       01 WS-EMPLOYEE-DATA.
          05 WS-EMP-ID          PIC 9(04)    VALUE 0101     USAGE COMP-5.
          05 WS-EMP-NAME        PIC X(20)    VALUE "CblTest".
          05 WS-HOURS-WORKED    PIC 9(03)V99 VALUE 40.00    USAGE COMP-5.
          05 WS-HOURLY-RATE     PIC 9(03)V99 VALUE 050.50   USAGE COMP-5.
          05 WS-GROSS-PAY       PIC 9(07)V99 VALUE ZERO     USAGE COMP-5.

       *> Formatted variables for display purposes
       01 WS-DISPLAY-GROSS      PIC Z(6)9.99.

       PROCEDURE DIVISION.
       0001-MAIN-PROCEDURE.

           DISPLAY "--- COBOL MAIN: Unit Test Start ---".
           DISPLAY "Testing with Name: " WS-EMP-NAME.

           *> Calling the sub-program using the Linkage structure
           CALL "PAYROLL_ENGINE" USING WS-EMPLOYEE-DATA.

           *> Format the result for a clean output
           MOVE WS-GROSS-PAY TO WS-DISPLAY-GROSS.

           DISPLAY "--- COBOL MAIN: Results Received ---".
           DISPLAY "Calculated Gross Pay: $" WS-DISPLAY-GROSS.
           DISPLAY "--- COBOL MAIN: End of Test ---".

           STOP RUN.
