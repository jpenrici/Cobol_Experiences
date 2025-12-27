*>*****************************************************************
*> Author:      Your Name
*> Date:        2025-12-26
*> Purpose:     Study of type and variable declaration.
*> Tect Stack:  GnuCOBOL / Debian 13
*>*****************************************************************

IDENTIFICATION DIVISION.
       PROGRAM-ID. PersonData.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       *> Group Item: 01 Level defines the record
       01 WS-PERSON-RECORD.
           *> Sub-levels (05) define the individual fields
           05 WS-NAME              PIC X(20).
           05 WS-AGE               PIC 9(03).
           *> V defines the logical decimal point for calculations
           05 WS-ACCOUNT-BALANCE   PIC 9(07)V99.
           05 WS-ACCOUNT-TYPE      PIC X(10).

       *> Variables for formatted output (Editing Masks)
       01 WS-FORMATTED-BALANCE     PIC $Z,ZZ9.99.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           *> Assigning values to fields
           MOVE "Bart Simpson"     TO WS-NAME.
           MOVE 30                 TO WS-AGE.
           MOVE 1500.75            TO WS-ACCOUNT-BALANCE.
           MOVE "SAVINGS"          TO WS-ACCOUNT-TYPE.

           *> Moving a numeric value to a mask variable formats it
           MOVE WS-ACCOUNT-BALANCE TO WS-FORMATTED-BALANCE.

           DISPLAY "--- Person Financial Record ---".
           DISPLAY "Name:    " WS-NAME.
           DISPLAY "Age:     " WS-AGE.
           DISPLAY "Type:    " WS-ACCOUNT-TYPE.
           DISPLAY "Balance: " WS-FORMATTED-BALANCE.
           DISPLAY "-------------------------------".

           STOP RUN.
