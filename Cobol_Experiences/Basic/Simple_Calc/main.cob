*>*****************************************************************
*> Tect Stack:  GnuCOBOL
*>*****************************************************************

IDENTIFICATION DIVISION.
    PROGRAM-ID. Inventory-Example.

    ENVIRONMENT DIVISION.
    CONFIGURATION SECTION.
    SPECIAL-NAMES.
        *> This swaps the function of comma and decimal point
        DECIMAL-POINT IS COMMA.

    DATA DIVISION.
    WORKING-STORAGE SECTION.

    *> 01 Level defines the main record structure
    01 WS-ITEM-STRUCTURE.
        *> 05 Level are subdivision of the record
        05 WS-ITEM-ID   PIC 9(05) VALUE 10001.

        *> Group Item: WS-ITEM-DETALAIS contains sub-fields
        05 WS-ITEM-DETAILS.
            10 WS-NAME          PIC X(15) VALUE "Shampoo Cães".
            10 WS-DESCRIPTION   PIC X(35) VALUE "Shampoo Neutro Cães e Gatos 500 ml".

        *> Numeric field with 2 decimal places (V is implicit decimal)
        05 WS-UNIT-PRICE    PIC 9(04)V99    VALUE 105,20. *> BRL format

        *> Quantity in stock
        05 WS-STOCK-QTY     PIC 9(03)   VALUE 058.

        *> Total
        05 WS-TOTAL-VALUE   PIC 9(07)V99.

        *> A constant defined with VALUE
        05 WS-STORE-NAME    PIC X(10)   VALUE "Pet Shop".

    01 WS-DISPLAY_FIELDS.
        *> Suppress leading zeros
        05 WS-MASK-QTY       PIC ZZ9.

        *> BRL format simulation
        05 WS-MASK-PRICE     PIC Z.ZZ9,99.
        05 WS-PRICE-DISPLAY  PIC X(10).

        05 WS-MASK-TOTAL     PIC ZZZ.ZZ9,99.
        05 WS-TOTAL-DISPLAY  PIC X(10).

    PROCEDURE DIVISION.
    MAIN-PROCEDURE.

        *> Math Operation
        COMPUTE WS-TOTAL-VALUE = WS-UNIT-PRICE * WS-STOCK-QTY.

        MOVE WS-STOCK-QTY TO WS-MASK-QTY.

        MOVE WS-UNIT-PRICE TO WS-MASK-PRICE.
        MOVE WS-TOTAL-VALUE TO WS-MASK-TOTAL.

        INITIALIZE WS-PRICE-DISPLAY WS-TOTAL-DISPLAY.

        STRING "R$ "  FUNCTION TRIM(WS-MASK-PRICE)
                DELIMITED BY SIZE
                INTO WS-PRICE-DISPLAY
        END-STRING

        STRING "R$ "  FUNCTION TRIM(WS-MASK-TOTAL)
                DELIMITED BY SIZE
                INTO WS-TOTAL-DISPLAY
        END-STRING

        DISPLAY "--- INVENTORY ITEM REPORT ---"
        DISPLAY "STORE: " WS-STORE-NAME
        DISPLAY "ID   : " WS-ITEM-ID
        DISPLAY "NAME : " WS-NAME
        DISPLAY "UNIT  PRICE : " WS-PRICE-DISPLAY
        DISPLAY "STOCK QTY   : " WS-MASK-QTY
        DISPLAY "TOTAL VALUE : " WS-TOTAL-DISPLAY
        DISPLAY "----------------------------"

        STOP RUN.
