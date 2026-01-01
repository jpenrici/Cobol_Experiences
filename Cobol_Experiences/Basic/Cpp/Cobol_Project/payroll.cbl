*>*****************************************************************
*> FILE: payroll.cbl
*>*****************************************************************

IDENTIFICATION DIVISION.
    PROGRAM-ID. PAYROLL_ENGINE.

    DATA DIVISION.

    WORKING-STORAGE SECTION.
    01 WS-TEMP-CALC          PIC 9(7)V99 VALUE ZERO.

    LINKAGE SECTION.
    01 LK-EMPLOYEE-DATA.
    *> COMP-5 aligns with C++ integer types (native binary)
        05 LK-EMP-ID          PIC 9(04) USAGE COMP-5.
        05 LK-EMP-NAME        PIC X(20).
    *> Using V99 implies fixed point math handled manually in C++
        05 LK-HOURS-WORKED    PIC 9(03)V99 USAGE COMP-5.
        05 LK-HOURLY-RATE     PIC 9(03)V99 USAGE COMP-5.
        05 LK-GROSS-PAY       PIC 9(07)V99 USAGE COMP-5.

    PROCEDURE DIVISION USING LK-EMPLOYEE-DATA.

    0100-PROCESS-PAYROLL.
        DISPLAY "COBOL: Processing data for: " LK-EMP-NAME.

        *> Business Logic: Gross Pay = Hours * Rate
        COMPUTE LK-GROSS-PAY = LK-HOURS-WORKED * LK-HOURLY-RATE.

        DISPLAY "COBOL: Calculation complete.".
        GOBACK.
