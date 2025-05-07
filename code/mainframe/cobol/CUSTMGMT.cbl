       IDENTIFICATION DIVISION.
       PROGRAM-ID. CUSTMGMT.
       AUTHOR. MAINFRAME MODERNIZATION TEAM.
      *****************************************************************
      * CUSTOMER MANAGEMENT PROGRAM                                   *
      * THIS PROGRAM MAINTAINS CUSTOMER RECORDS INCLUDING ADD, UPDATE, *
      * DELETE, AND INQUIRY FUNCTIONS.                                *
      *****************************************************************
       
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUSTOMER-FILE ASSIGN TO CUSTFILE
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS CUSTOMER-ID
           FILE STATUS IS FILE-STATUS.
           
       DATA DIVISION.
       FILE SECTION.
       FD  CUSTOMER-FILE
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 200 CHARACTERS.
       01  CUSTOMER-RECORD.
           05  CUSTOMER-ID             PIC X(6).
           05  CUSTOMER-NAME           PIC X(30).
           05  CUSTOMER-ADDRESS.
               10  CUSTOMER-STREET     PIC X(30).
               10  CUSTOMER-CITY       PIC X(20).
               10  CUSTOMER-STATE      PIC X(2).
               10  CUSTOMER-ZIP        PIC X(10).
           05  CUSTOMER-CONTACT.
               10  CUSTOMER-PHONE      PIC X(15).
               10  CUSTOMER-EMAIL      PIC X(50).
           05  CUSTOMER-STATUS         PIC X(1).
               88  CUSTOMER-ACTIVE     VALUE 'A'.
               88  CUSTOMER-INACTIVE   VALUE 'I'.
           05  CUSTOMER-CREDIT-SCORE   PIC 9(3).
           05  CUSTOMER-CREDIT-LIMIT   PIC 9(7)V99.
           05  CUSTOMER-BALANCE        PIC S9(7)V99.
           05  FILLER                  PIC X(17).
       
       WORKING-STORAGE SECTION.
       01  WS-WORK-AREAS.
           05  FILE-STATUS             PIC X(2).
               88  FILE-SUCCESS        VALUE '00'.
               88  FILE-EOF            VALUE '10'.
               88  FILE-NOT-FOUND      VALUE '23'.
           05  WS-ACTION-CODE          PIC X(1).
               88  WS-ADD              VALUE 'A'.
               88  WS-UPDATE           VALUE 'U'.
               88  WS-DELETE           VALUE 'D'.
               88  WS-INQUIRY          VALUE 'I'.
           05  WS-ERROR-FLAG           PIC X(1).
               88  WS-ERROR            VALUE 'E'.
               88  WS-NO-ERROR         VALUE 'N'.
       
       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 1000-INITIALIZATION.
           PERFORM 2000-PROCESS UNTIL WS-ACTION-CODE = 'X'.
           PERFORM 9000-TERMINATION.
           STOP RUN.
       
       1000-INITIALIZATION.
           OPEN I-O CUSTOMER-FILE.
           IF NOT FILE-SUCCESS
               DISPLAY 'ERROR OPENING CUSTOMER FILE: ' FILE-STATUS
               MOVE 'X' TO WS-ACTION-CODE
           ELSE
               DISPLAY 'CUSTOMER MANAGEMENT SYSTEM INITIALIZED'
               MOVE 'N' TO WS-ERROR-FLAG
           END-IF.
       
       2000-PROCESS.
           PERFORM 2100-GET-ACTION.
           EVALUATE TRUE
               WHEN WS-ADD
                   PERFORM 3000-ADD-CUSTOMER
               WHEN WS-UPDATE
                   PERFORM 4000-UPDATE-CUSTOMER
               WHEN WS-DELETE
                   PERFORM 5000-DELETE-CUSTOMER
               WHEN WS-INQUIRY
                   PERFORM 6000-INQUIRY-CUSTOMER
           END-EVALUATE.
       
       2100-GET-ACTION.
           DISPLAY 'ENTER ACTION (A=ADD, U=UPDATE, D=DELETE, I=INQUIRY, X=EXIT): '.
           ACCEPT WS-ACTION-CODE.
           INSPECT WS-ACTION-CODE CONVERTING 'aiudx' TO 'AIUDX'.
       
       3000-ADD-CUSTOMER.
           MOVE SPACES TO CUSTOMER-RECORD.
           PERFORM 3100-ENTER-CUSTOMER-DATA.
           IF WS-NO-ERROR
               WRITE CUSTOMER-RECORD
                   INVALID KEY
                       DISPLAY 'ERROR: CUSTOMER ID ALREADY EXISTS'
                       MOVE 'E' TO WS-ERROR-FLAG
               END-WRITE
               IF WS-NO-ERROR
                   DISPLAY 'CUSTOMER ADDED SUCCESSFULLY'
               END-IF
           END-IF.
       
       3100-ENTER-CUSTOMER-DATA.
           MOVE 'N' TO WS-ERROR-FLAG.
           DISPLAY 'ENTER CUSTOMER ID (6 DIGITS): '.
           ACCEPT CUSTOMER-ID.
           DISPLAY 'ENTER CUSTOMER NAME: '.
           ACCEPT CUSTOMER-NAME.
           DISPLAY 'ENTER CUSTOMER STREET: '.
           ACCEPT CUSTOMER-STREET.
           DISPLAY 'ENTER CUSTOMER CITY: '.
           ACCEPT CUSTOMER-CITY.
           DISPLAY 'ENTER CUSTOMER STATE: '.
           ACCEPT CUSTOMER-STATE.
           DISPLAY 'ENTER CUSTOMER ZIP: '.
           ACCEPT CUSTOMER-ZIP.
           DISPLAY 'ENTER CUSTOMER PHONE: '.
           ACCEPT CUSTOMER-PHONE.
           DISPLAY 'ENTER CUSTOMER EMAIL: '.
           ACCEPT CUSTOMER-EMAIL.
           MOVE 'A' TO CUSTOMER-STATUS.
           PERFORM 3200-CALCULATE-CREDIT-SCORE.
           PERFORM 3300-DETERMINE-CREDIT-LIMIT.
           MOVE ZERO TO CUSTOMER-BALANCE.
       
       3200-CALCULATE-CREDIT-SCORE.
      *****************************************************************
      * BUSINESS RULE BR001: CALCULATE CREDIT SCORE                   *
      * CREDIT SCORE IS DETERMINED BY EXTERNAL CREDIT BUREAU API      *
      * FOR DEMO PURPOSES, WE'LL SET A DEFAULT VALUE                  *
      *****************************************************************
           MOVE 700 TO CUSTOMER-CREDIT-SCORE.
           
       3300-DETERMINE-CREDIT-LIMIT.
      *****************************************************************
      * BUSINESS RULE BR002: DETERMINE CREDIT LIMIT                   *
      * CREDIT LIMIT IS BASED ON CREDIT SCORE:                        *
      * - SCORE < 500: $1,000 LIMIT                                   *
      * - SCORE 500-699: $5,000 LIMIT                                 *
      * - SCORE >= 700: $10,000 LIMIT                                 *
      *****************************************************************
           EVALUATE TRUE
               WHEN CUSTOMER-CREDIT-SCORE < 500
                   MOVE 0100000 TO CUSTOMER-CREDIT-LIMIT
               WHEN CUSTOMER-CREDIT-SCORE < 700
                   MOVE 0500000 TO CUSTOMER-CREDIT-LIMIT
               WHEN OTHER
                   MOVE 1000000 TO CUSTOMER-CREDIT-LIMIT
           END-EVALUATE.
       
       4000-UPDATE-CUSTOMER.
           MOVE SPACES TO CUSTOMER-RECORD.
           DISPLAY 'ENTER CUSTOMER ID TO UPDATE: '.
           ACCEPT CUSTOMER-ID.
           READ CUSTOMER-FILE
               INVALID KEY
                   DISPLAY 'ERROR: CUSTOMER NOT FOUND'
                   MOVE 'E' TO WS-ERROR-FLAG
           END-READ.
           IF WS-NO-ERROR
               PERFORM 3100-ENTER-CUSTOMER-DATA
               IF WS-NO-ERROR
                   REWRITE CUSTOMER-RECORD
                       INVALID KEY
                           DISPLAY 'ERROR UPDATING CUSTOMER'
                           MOVE 'E' TO WS-ERROR-FLAG
                   END-REWRITE
                   IF WS-NO-ERROR
                       DISPLAY 'CUSTOMER UPDATED SUCCESSFULLY'
                   END-IF
               END-IF
           END-IF.
       
       5000-DELETE-CUSTOMER.
           MOVE SPACES TO CUSTOMER-RECORD.
           DISPLAY 'ENTER CUSTOMER ID TO DELETE: '.
           ACCEPT CUSTOMER-ID.
           READ CUSTOMER-FILE
               INVALID KEY
                   DISPLAY 'ERROR: CUSTOMER NOT FOUND'
                   MOVE 'E' TO WS-ERROR-FLAG
           END-READ.
           IF WS-NO-ERROR
               DELETE CUSTOMER-FILE
                   INVALID KEY
                       DISPLAY 'ERROR DELETING CUSTOMER'
                       MOVE 'E' TO WS-ERROR-FLAG
               END-DELETE
               IF WS-NO-ERROR
                   DISPLAY 'CUSTOMER DELETED SUCCESSFULLY'
               END-IF
           END-IF.
       
       6000-INQUIRY-CUSTOMER.
           MOVE SPACES TO CUSTOMER-RECORD.
           DISPLAY 'ENTER CUSTOMER ID FOR INQUIRY: '.
           ACCEPT CUSTOMER-ID.
           READ CUSTOMER-FILE
               INVALID KEY
                   DISPLAY 'ERROR: CUSTOMER NOT FOUND'
                   MOVE 'E' TO WS-ERROR-FLAG
           END-READ.
           IF WS-NO-ERROR
               DISPLAY '------------------------------------'
               DISPLAY 'CUSTOMER ID: ' CUSTOMER-ID
               DISPLAY 'CUSTOMER NAME: ' CUSTOMER-NAME
               DISPLAY 'ADDRESS: ' CUSTOMER-STREET
               DISPLAY '         ' CUSTOMER-CITY ', ' CUSTOMER-STATE ' ' CUSTOMER-ZIP
               DISPLAY 'PHONE: ' CUSTOMER-PHONE
               DISPLAY 'EMAIL: ' CUSTOMER-EMAIL
               IF CUSTOMER-ACTIVE
                   DISPLAY 'STATUS: ACTIVE'
               ELSE
                   DISPLAY 'STATUS: INACTIVE'
               END-IF
               DISPLAY 'CREDIT SCORE: ' CUSTOMER-CREDIT-SCORE
               DISPLAY 'CREDIT LIMIT: $' CUSTOMER-CREDIT-LIMIT
               DISPLAY 'CURRENT BALANCE: $' CUSTOMER-BALANCE
               DISPLAY '------------------------------------'
           END-IF.
       
       9000-TERMINATION.
           CLOSE CUSTOMER-FILE.
           DISPLAY 'CUSTOMER MANAGEMENT SYSTEM TERMINATED'.
       
       END PROGRAM CUSTMGMT. 