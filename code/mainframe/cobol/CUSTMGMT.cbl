       IDENTIFICATION DIVISION.
       PROGRAM-ID. CUSTMGMT.
       AUTHOR. AZURE-AI-FOUNDRY.
      *****************************************************************
      * CUSTMGMT - Customer Management Program                        *
      *                                                               *
      * This program provides customer management functions including *
      * add, update, delete, and inquiry of customer records.         *
      *                                                               *
      * TRANSACTION CODES:                                            *
      *   CADD - Add a new customer                                   *
      *   CUPD - Update an existing customer                          *
      *   CDEL - Delete a customer                                    *
      *   CINQ - Inquire about customer details                       *
      *                                                               *
      * DATE-WRITTEN: 2023-05-15                                      *
      * DATE-UPDATED: 2023-09-20                                      *
      *****************************************************************
       
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-MAINFRAME.
       OBJECT-COMPUTER. IBM-MAINFRAME.
       
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUSTOMER-FILE ASSIGN TO CUSTFILE
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CUST-ID
               FILE STATUS IS CUST-FILE-STATUS.
           
           SELECT TRANSACTION-FILE ASSIGN TO TRANFILE
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS TRAN-FILE-STATUS.
           
           SELECT REPORT-FILE ASSIGN TO RPTFILE
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS REPORT-FILE-STATUS.
       
       DATA DIVISION.
       FILE SECTION.
       FD  CUSTOMER-FILE
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F
           RECORD CONTAINS 200 CHARACTERS.
       01  CUSTOMER-RECORD.
           05  CUST-ID                 PIC X(10).
           05  CUST-NAME               PIC X(50).
           05  CUST-ADDRESS            PIC X(70).
           05  CUST-PHONE              PIC X(15).
           05  CUST-EMAIL              PIC X(30).
           05  CUST-TYPE               PIC X(1).
               88  CUST-TYPE-RETAIL    VALUE 'R'.
               88  CUST-TYPE-WHOLESALE VALUE 'W'.
               88  CUST-TYPE-PARTNER   VALUE 'P'.
           05  CUST-STATUS             PIC X(1).
               88  CUST-STATUS-ACTIVE  VALUE 'A'.
               88  CUST-STATUS-INACTIVE VALUE 'I'.
           05  CUST-CREDIT-LIMIT       PIC 9(7)V99.
           05  CUST-CURRENT-BALANCE    PIC S9(7)V99.
           05  CUST-LAST-ACTIVITY-DATE PIC X(10).
           05  FILLER                  PIC X(7).
       
       FD  TRANSACTION-FILE
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F
           RECORD CONTAINS 80 CHARACTERS.
       01  TRANSACTION-RECORD.
           05  TRAN-CODE               PIC X(4).
           05  TRAN-CUST-ID            PIC X(10).
           05  TRAN-DATA               PIC X(66).
       
       FD  REPORT-FILE
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F
           RECORD CONTAINS 132 CHARACTERS.
       01  REPORT-RECORD               PIC X(132).
       
       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05  CUST-FILE-STATUS        PIC X(2).
               88  CUST-FILE-SUCCESS   VALUE '00'.
               88  CUST-FILE-EOF       VALUE '10'.
               88  CUST-FILE-NOT-FOUND VALUE '23'.
           05  TRAN-FILE-STATUS        PIC X(2).
               88  TRAN-FILE-SUCCESS   VALUE '00'.
               88  TRAN-FILE-EOF       VALUE '10'.
           05  REPORT-FILE-STATUS      PIC X(2).
               88  REPORT-FILE-SUCCESS VALUE '00'.
       
       01  WS-WORK-AREAS.
           05  WS-TRANSACTION-COUNT    PIC 9(7) VALUE ZERO.
           05  WS-ADD-COUNT            PIC 9(5) VALUE ZERO.
           05  WS-UPDATE-COUNT         PIC 9(5) VALUE ZERO.
           05  WS-DELETE-COUNT         PIC 9(5) VALUE ZERO.
           05  WS-INQUIRY-COUNT        PIC 9(5) VALUE ZERO.
           05  WS-ERROR-COUNT          PIC 9(5) VALUE ZERO.
           05  WS-WORK-DATE            PIC X(10).
           05  WS-WORK-DATE-NUM REDEFINES WS-WORK-DATE.
               10  WS-YEAR             PIC 9(4).
               10  FILLER              PIC X.
               10  WS-MONTH            PIC 9(2).
               10  FILLER              PIC X.
               10  WS-DAY              PIC 9(2).
       
       01  WS-CURRENT-DATE.
           05  WS-CURR-DATE.
               10  WS-CURR-YEAR        PIC 9(4).
               10  WS-CURR-MONTH       PIC 9(2).
               10  WS-CURR-DAY         PIC 9(2).
           05  WS-CURR-TIME.
               10  WS-CURR-HOUR        PIC 9(2).
               10  WS-CURR-MINUTE      PIC 9(2).
               10  WS-CURR-SECOND      PIC 9(2).
               10  WS-CURR-MS          PIC 9(2).
           05  WS-DIFF-GMT             PIC S9(4).
       
       01  WS-REPORT-HEADER.
           05  FILLER                  PIC X(30) VALUE SPACES.
           05  FILLER                  PIC X(41) 
               VALUE 'CUSTOMER MANAGEMENT TRANSACTION REPORT'.
           05  FILLER                  PIC X(15) VALUE SPACES.
           05  FILLER                  PIC X(5) VALUE 'DATE:'.
           05  WS-HEADER-DATE          PIC X(10).
           05  FILLER                  PIC X(2) VALUE SPACES.
           05  FILLER                  PIC X(5) VALUE 'TIME:'.
           05  WS-HEADER-TIME          PIC X(8).
           05  FILLER                  PIC X(16) VALUE SPACES.
       
       01  WS-COLUMN-HEADER.
           05  FILLER                  PIC X(4) VALUE 'CODE'.
           05  FILLER                  PIC X(3) VALUE SPACES.
           05  FILLER                  PIC X(10) VALUE 'CUSTOMER ID'.
           05  FILLER                  PIC X(3) VALUE SPACES.
           05  FILLER                  PIC X(20) VALUE 'CUSTOMER NAME'.
           05  FILLER                  PIC X(3) VALUE SPACES.
           05  FILLER                  PIC X(20) VALUE 'STATUS'.
           05  FILLER                  PIC X(3) VALUE SPACES.
           05  FILLER                  PIC X(7) VALUE 'RESULT'.
           05  FILLER                  PIC X(59) VALUE SPACES.
       
       01  WS-DETAIL-LINE.
           05  WS-DET-TRAN-CODE        PIC X(4).
           05  FILLER                  PIC X(3) VALUE SPACES.
           05  WS-DET-CUST-ID          PIC X(10).
           05  FILLER                  PIC X(3) VALUE SPACES.
           05  WS-DET-CUST-NAME        PIC X(20).
           05  FILLER                  PIC X(3) VALUE SPACES.
           05  WS-DET-STATUS           PIC X(20).
           05  FILLER                  PIC X(3) VALUE SPACES.
           05  WS-DET-RESULT           PIC X(40).
           05  FILLER                  PIC X(26) VALUE SPACES.
       
       01  WS-SUMMARY-LINE.
           05  FILLER                  PIC X(25) VALUE SPACES.
           05  FILLER                  PIC X(20) 
               VALUE 'TRANSACTION SUMMARY:'.
           05  FILLER                  PIC X(5) VALUE SPACES.
           05  FILLER                  PIC X(22) 
               VALUE 'TOTAL TRANSACTIONS: '.
           05  WS-SUM-TOTAL            PIC ZZ,ZZ9.
           05  FILLER                  PIC X(55) VALUE SPACES.
       
       01  WS-DETAIL-COUNTS.
           05  FILLER                  PIC X(25) VALUE SPACES.
           05  FILLER                  PIC X(9) VALUE 'ADDS:    '.
           05  WS-SUM-ADDS             PIC Z,ZZ9.
           05  FILLER                  PIC X(5) VALUE SPACES.
           05  FILLER                  PIC X(9) VALUE 'UPDATES: '.
           05  WS-SUM-UPDATES          PIC Z,ZZ9.
           05  FILLER                  PIC X(5) VALUE SPACES.
           05  FILLER                  PIC X(9) VALUE 'DELETES: '.
           05  WS-SUM-DELETES          PIC Z,ZZ9.
           05  FILLER                  PIC X(5) VALUE SPACES.
           05  FILLER                  PIC X(9) VALUE 'INQUIRIES:'.
           05  WS-SUM-INQUIRIES        PIC Z,ZZ9.
           05  FILLER                  PIC X(5) VALUE SPACES.
           05  FILLER                  PIC X(9) VALUE 'ERRORS:  '.
           05  WS-SUM-ERRORS           PIC Z,ZZ9.
           05  FILLER                  PIC X(20) VALUE SPACES.
       
       01  WS-ERROR-LINE.
           05  FILLER                  PIC X(25) VALUE SPACES.
           05  FILLER                  PIC X(17) 
               VALUE '*** ERROR *** - '.
           05  WS-ERROR-MESSAGE        PIC X(60).
           05  FILLER                  PIC X(30) VALUE SPACES.
       
      *****************************************************************
      * COPY statements are included here                             *
      *****************************************************************
           COPY 'CUSTTYPE.CPY'.
           COPY 'ERRMSGS.CPY'.
       
       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 1000-INITIALIZE
           PERFORM 2000-PROCESS-TRANSACTIONS UNTIL TRAN-FILE-EOF
           PERFORM 3000-TERMINATE
           GOBACK.
       
       1000-INITIALIZE.
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE
           
           OPEN INPUT TRANSACTION-FILE
           IF NOT TRAN-FILE-SUCCESS
              DISPLAY 'ERROR OPENING TRANSACTION FILE: ' TRAN-FILE-STATUS
              MOVE 'ERROR OPENING TRANSACTION FILE' TO WS-ERROR-MESSAGE
              PERFORM 9000-WRITE-ERROR
              PERFORM 3000-TERMINATE
              GOBACK
           END-IF
           
           OPEN I-O CUSTOMER-FILE
           IF NOT CUST-FILE-SUCCESS
              DISPLAY 'ERROR OPENING CUSTOMER FILE: ' CUST-FILE-STATUS
              MOVE 'ERROR OPENING CUSTOMER FILE' TO WS-ERROR-MESSAGE
              PERFORM 9000-WRITE-ERROR
              PERFORM 3000-TERMINATE
              GOBACK
           END-IF
           
           OPEN OUTPUT REPORT-FILE
           IF NOT REPORT-FILE-SUCCESS
              DISPLAY 'ERROR OPENING REPORT FILE: ' REPORT-FILE-STATUS
              MOVE 'ERROR OPENING REPORT FILE' TO WS-ERROR-MESSAGE
              PERFORM 9000-WRITE-ERROR
              PERFORM 3000-TERMINATE
              GOBACK
           END-IF
           
           PERFORM 9100-WRITE-HEADERS
           
           READ TRANSACTION-FILE
               AT END SET TRAN-FILE-EOF TO TRUE
           END-READ.
       
       2000-PROCESS-TRANSACTIONS.
           ADD 1 TO WS-TRANSACTION-COUNT
           
           EVALUATE TRAN-CODE
               WHEN 'CADD'
                   PERFORM 2100-ADD-CUSTOMER
               WHEN 'CUPD'
                   PERFORM 2200-UPDATE-CUSTOMER
               WHEN 'CDEL'
                   PERFORM 2300-DELETE-CUSTOMER
               WHEN 'CINQ'
                   PERFORM 2400-INQUIRE-CUSTOMER
               WHEN OTHER
                   MOVE 'INVALID TRANSACTION CODE' TO WS-ERROR-MESSAGE
                   PERFORM 9000-WRITE-ERROR
                   ADD 1 TO WS-ERROR-COUNT
           END-EVALUATE
           
           READ TRANSACTION-FILE
               AT END SET TRAN-FILE-EOF TO TRUE
           END-READ.
       
       2100-ADD-CUSTOMER.
           MOVE TRAN-CUST-ID TO CUST-ID
           READ CUSTOMER-FILE
               INVALID KEY
                   PERFORM 2110-PERFORM-ADD
               NOT INVALID KEY
                   MOVE 'CUSTOMER ALREADY EXISTS' TO WS-ERROR-MESSAGE
                   PERFORM 9000-WRITE-ERROR
                   ADD 1 TO WS-ERROR-COUNT
           END-READ.
       
       2110-PERFORM-ADD.
           INITIALIZE CUSTOMER-RECORD
           MOVE TRAN-CUST-ID TO CUST-ID
           
      * Extract data from TRAN-DATA field
           UNSTRING TRAN-DATA DELIMITED BY '|' INTO
               CUST-NAME
               CUST-ADDRESS
               CUST-PHONE
               CUST-EMAIL
               CUST-TYPE
               CUST-CREDIT-LIMIT
           END-UNSTRING
           
           MOVE 'A' TO CUST-STATUS
           MOVE ZEROES TO CUST-CURRENT-BALANCE
           
      * Format current date as YYYY-MM-DD
           STRING WS-CURR-YEAR DELIMITED BY SIZE
                  '-'          DELIMITED BY SIZE
                  WS-CURR-MONTH DELIMITED BY SIZE
                  '-'          DELIMITED BY SIZE
                  WS-CURR-DAY  DELIMITED BY SIZE
               INTO CUST-LAST-ACTIVITY-DATE
           END-STRING
           
           WRITE CUSTOMER-RECORD
               INVALID KEY
                   MOVE 'ERROR WRITING CUSTOMER RECORD' TO WS-ERROR-MESSAGE
                   PERFORM 9000-WRITE-ERROR
                   ADD 1 TO WS-ERROR-COUNT
               NOT INVALID KEY
                   ADD 1 TO WS-ADD-COUNT
                   MOVE TRAN-CODE TO WS-DET-TRAN-CODE
                   MOVE CUST-ID TO WS-DET-CUST-ID
                   MOVE CUST-NAME TO WS-DET-CUST-NAME
                   MOVE 'ACTIVE' TO WS-DET-STATUS
                   MOVE 'ADDED SUCCESSFULLY' TO WS-DET-RESULT
                   PERFORM 9200-WRITE-DETAIL
           END-WRITE.
       
       2200-UPDATE-CUSTOMER.
           MOVE TRAN-CUST-ID TO CUST-ID
           READ CUSTOMER-FILE
               INVALID KEY
                   MOVE 'CUSTOMER DOES NOT EXIST' TO WS-ERROR-MESSAGE
                   PERFORM 9000-WRITE-ERROR
                   ADD 1 TO WS-ERROR-COUNT
               NOT INVALID KEY
                   PERFORM 2210-PERFORM-UPDATE
           END-READ.
       
       2210-PERFORM-UPDATE.
      * Extract data from TRAN-DATA field 
           UNSTRING TRAN-DATA DELIMITED BY '|' INTO
               CUST-NAME
               CUST-ADDRESS
               CUST-PHONE
               CUST-EMAIL
               CUST-TYPE
               CUST-STATUS
               CUST-CREDIT-LIMIT
           END-UNSTRING
           
      * Format current date as YYYY-MM-DD
           STRING WS-CURR-YEAR DELIMITED BY SIZE
                  '-'          DELIMITED BY SIZE
                  WS-CURR-MONTH DELIMITED BY SIZE
                  '-'          DELIMITED BY SIZE
                  WS-CURR-DAY  DELIMITED BY SIZE
               INTO CUST-LAST-ACTIVITY-DATE
           END-STRING
           
           REWRITE CUSTOMER-RECORD
               INVALID KEY
                   MOVE 'ERROR UPDATING CUSTOMER RECORD' TO WS-ERROR-MESSAGE
                   PERFORM 9000-WRITE-ERROR
                   ADD 1 TO WS-ERROR-COUNT
               NOT INVALID KEY
                   ADD 1 TO WS-UPDATE-COUNT
                   MOVE TRAN-CODE TO WS-DET-TRAN-CODE
                   MOVE CUST-ID TO WS-DET-CUST-ID
                   MOVE CUST-NAME TO WS-DET-CUST-NAME
                   IF CUST-STATUS-ACTIVE
                       MOVE 'ACTIVE' TO WS-DET-STATUS
                   ELSE
                       MOVE 'INACTIVE' TO WS-DET-STATUS
                   END-IF
                   MOVE 'UPDATED SUCCESSFULLY' TO WS-DET-RESULT
                   PERFORM 9200-WRITE-DETAIL
           END-REWRITE.
       
       2300-DELETE-CUSTOMER.
           MOVE TRAN-CUST-ID TO CUST-ID
           READ CUSTOMER-FILE
               INVALID KEY
                   MOVE 'CUSTOMER DOES NOT EXIST' TO WS-ERROR-MESSAGE
                   PERFORM 9000-WRITE-ERROR
                   ADD 1 TO WS-ERROR-COUNT
               NOT INVALID KEY
                   PERFORM 2310-PERFORM-DELETE
           END-READ.
       
       2310-PERFORM-DELETE.
      * Check if customer has outstanding balance
           IF CUST-CURRENT-BALANCE NOT = ZEROES
               MOVE 'CUSTOMER HAS OUTSTANDING BALANCE' TO WS-ERROR-MESSAGE
               PERFORM 9000-WRITE-ERROR
               ADD 1 TO WS-ERROR-COUNT
           ELSE
               DELETE CUSTOMER-FILE RECORD
                   INVALID KEY
                       MOVE 'ERROR DELETING CUSTOMER RECORD' 
                           TO WS-ERROR-MESSAGE
                       PERFORM 9000-WRITE-ERROR
                       ADD 1 TO WS-ERROR-COUNT
                   NOT INVALID KEY
                       ADD 1 TO WS-DELETE-COUNT
                       MOVE TRAN-CODE TO WS-DET-TRAN-CODE
                       MOVE CUST-ID TO WS-DET-CUST-ID
                       MOVE CUST-NAME TO WS-DET-CUST-NAME
                       MOVE 'DELETED' TO WS-DET-STATUS
                       MOVE 'DELETED SUCCESSFULLY' TO WS-DET-RESULT
                       PERFORM 9200-WRITE-DETAIL
               END-DELETE
           END-IF.
       
       2400-INQUIRE-CUSTOMER.
           MOVE TRAN-CUST-ID TO CUST-ID
           READ CUSTOMER-FILE
               INVALID KEY
                   MOVE 'CUSTOMER DOES NOT EXIST' TO WS-ERROR-MESSAGE
                   PERFORM 9000-WRITE-ERROR
                   ADD 1 TO WS-ERROR-COUNT
               NOT INVALID KEY
                   ADD 1 TO WS-INQUIRY-COUNT
                   MOVE TRAN-CODE TO WS-DET-TRAN-CODE
                   MOVE CUST-ID TO WS-DET-CUST-ID
                   MOVE CUST-NAME TO WS-DET-CUST-NAME
                   IF CUST-STATUS-ACTIVE
                       MOVE 'ACTIVE' TO WS-DET-STATUS
                   ELSE
                       MOVE 'INACTIVE' TO WS-DET-STATUS
                   END-IF
                   MOVE 'INQUIRY PROCESSED' TO WS-DET-RESULT
                   PERFORM 9200-WRITE-DETAIL
           END-READ.
       
       3000-TERMINATE.
           PERFORM 9300-WRITE-SUMMARY
           
           CLOSE TRANSACTION-FILE
           CLOSE CUSTOMER-FILE
           CLOSE REPORT-FILE.
       
       9000-WRITE-ERROR.
           MOVE TRAN-CODE TO WS-DET-TRAN-CODE
           MOVE TRAN-CUST-ID TO WS-DET-CUST-ID
           MOVE SPACES TO WS-DET-CUST-NAME
           MOVE 'ERROR' TO WS-DET-STATUS
           MOVE WS-ERROR-MESSAGE TO WS-DET-RESULT
           PERFORM 9200-WRITE-DETAIL.
       
       9100-WRITE-HEADERS.
      * Format current date and time for report header
           STRING WS-CURR-YEAR        DELIMITED BY SIZE
                  '-'                 DELIMITED BY SIZE
                  WS-CURR-MONTH       DELIMITED BY SIZE
                  '-'                 DELIMITED BY SIZE
                  WS-CURR-DAY         DELIMITED BY SIZE
               INTO WS-HEADER-DATE
           END-STRING
           
           STRING WS-CURR-HOUR        DELIMITED BY SIZE
                  ':'                 DELIMITED BY SIZE
                  WS-CURR-MINUTE      DELIMITED BY SIZE
                  ':'                 DELIMITED BY SIZE
                  WS-CURR-SECOND      DELIMITED BY SIZE
               INTO WS-HEADER-TIME
           END-STRING
           
           MOVE WS-REPORT-HEADER TO REPORT-RECORD
           WRITE REPORT-RECORD
           
           MOVE SPACES TO REPORT-RECORD
           WRITE REPORT-RECORD
           
           MOVE WS-COLUMN-HEADER TO REPORT-RECORD
           WRITE REPORT-RECORD
           
           MOVE SPACES TO REPORT-RECORD
           WRITE REPORT-RECORD.
       
       9200-WRITE-DETAIL.
           MOVE WS-DETAIL-LINE TO REPORT-RECORD
           WRITE REPORT-RECORD.
       
       9300-WRITE-SUMMARY.
           MOVE SPACES TO REPORT-RECORD
           WRITE REPORT-RECORD
           
           MOVE WS-TRANSACTION-COUNT TO WS-SUM-TOTAL
           MOVE WS-SUMMARY-LINE TO REPORT-RECORD
           WRITE REPORT-RECORD
           
           MOVE WS-ADD-COUNT TO WS-SUM-ADDS
           MOVE WS-UPDATE-COUNT TO WS-SUM-UPDATES
           MOVE WS-DELETE-COUNT TO WS-SUM-DELETES
           MOVE WS-INQUIRY-COUNT TO WS-SUM-INQUIRIES
           MOVE WS-ERROR-COUNT TO WS-SUM-ERRORS
           MOVE WS-DETAIL-COUNTS TO REPORT-RECORD
           WRITE REPORT-RECORD.
       
       END PROGRAM CUSTMGMT. 