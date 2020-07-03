       IDENTIFICATION DIVISION.
       PROGRAM-ID. SCREEN.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ADDRESS-BOOK ASSIGN TO "ADDRESS_BOOK.DAT"
                   ORGANIZATION IS INDEXED
                   ACCESS MODE IS DYNAMIC
                   RECORD KEY IS FD-NAME-CODE
                   ALTERNATE RECORD KEY IS FD-LAST-NAME
                   WITH DUPLICATES FILE STATUS IS FILESTATUS.


       DATA DIVISION.

       FILE SECTION.
       FD ADDRESS-BOOK.
       01 ADDRESS-RECORD.
       COPY "ADDRESS-RECORD.CPY" REPLACING ==(TAG)== BY ==FD-==.


       WORKING-STORAGE SECTION.
      *> FOLLOWING VARIABLE IS SHARED WITH WR-INDEX TO GET NEXT
      *> INDEX FOR INDEXED RECORD
       01 WS-INDEX-NUMBER EXTERNAL PIC S9(09).

       01 WS-RESPONSE PIC X.

       01 ADDRESS-STRUCTURE.
       COPY "ADDRESS-RECORD.CPY" REPLACING ==(TAG)== BY ==WS-==.

       COPY "FILESTATUS.CPY".

       01 FS-MSG                    PIC X(30).
       01 WS-MSG                    PIC X(30).
       01 WS-MESSAGE                PIC X(30).


       SCREEN SECTION.
       01 BLANK-SCREEN BLANK SCREEN.

       01 ACTION-ENTRY-SCREEN BLANK SCREEN.
          05  VALUE "ACTION: "                    LINE 1 COL 10.
          05  RESPONSE-INPUT                      LINE 1 COL 30
                PIC X TO WS-RESPONSE.
          05  MSG-VALUE                           LINE 10 COL 30
                PIC X(30) FROM WS-MESSAGE.

       01 DATA-ENTRY-SCREEN BLANK SCREEN.
          05  VALUE "ACTION: "                    LINE 1 COL 10.
          05  RESPONSE-INPUT                      LINE 1 COL 30
                PIC X FROM WS-RESPONSE.
          05  VALUE "LAST NAME:"                  LINE 3 COL 10.
          05  LAST-NAME                           LINE 3 COL 30
                 PIC X(20) TO WS-LAST-NAME.
          05  VALUE "FIRST NAME:"                 LINE 4 COL 10.
          05  FIRST-NAME                          LINE 4 COL 30
                 PIC X(20) TO WS-FIRST-NAME.
          05  VALUE "STREET NAME:"                LINE 5 COL 10.
          05  STREET-NAME                         LINE 5 COL 30
                 PIC X(40) TO WS-STREET-NAME.
          05  VALUE "CITY:"                       LINE 6 COL 10.
          05  CITY-NAME                           LINE 6 COL 30
                 PIC X(40) TO WS-CITY.
          05  VALUE "STATE:"                      LINE 7 COL 10.
          05  STATE-NAME                          LINE 7 COL 30
                PIC X(2) TO WS-STATE.
          05  VALUE "ZIP:"                        LINE 8 COL 10.
          05  ZIP-VALUE                           LINE 8 COL 30
                PIC X(10) TO WS-ZIP.
          05  VALUE "NOTES:"                      LINE 9 COL 10.
          05  ZIP-VALUE                           LINE 9 COL 30
                PIC X(50) TO WS-NOTES.
          05  MSG-VALUE                           LINE 10 COL 30
                PIC X(30) FROM WS-MESSAGE.

       01 FILE-STATUS-SCREEN.
          05 STATUS-MSG                             LINE 12 COL 30
             PIC X(30) FROM FS-MSG.


       PROCEDURE DIVISION.
            PERFORM CHECK-FILE-EXIST.
            PERFORM SHOW-SCREEN UNTIL WS-RESPONSE IS EQUAL TO 'Q'.
            STOP RUN.

       CHECK-FILE-EXIST SECTION.
            OPEN INPUT ADDRESS-BOOK
            IF NOT FS-SUCCESS
                IF FS-NO-FILE
                    PERFORM BUILD-EMPTY-FILE
                    CLOSE ADDRESS-BOOK
                ELSE
                    PERFORM GET-FILE-STATUS
                END-IF
            END-IF.
       CEF-EXIST.
           EXIT.

       SHOW-SCREEN SECTION.
            PERFORM INITIALIZE-WS-RECORD.
            DISPLAY ACTION-ENTRY-SCREEN.
            ACCEPT ACTION-ENTRY-SCREEN.

            IF WS-RESPONSE IS EQUAL TO 'H' THEN
               PERFORM SHOW-HELP
            END-IF.

            IF WS-RESPONSE IS EQUAL TO 'A' THEN
               PERFORM ADD-DATA
            END-IF.


       SS-EXIT.
          EXIT.

       ADD-DATA SECTION.
           DISPLAY DATA-ENTRY-SCREEN.
           ACCEPT DATA-ENTRY-SCREEN.
           IF WS-LAST-NAME IS EQUAL SPACES THEN
              MOVE " MUST HAVE LAST NAME " TO WS-MESSAGE
              GO TO AD-EXIT
           END-IF.
           PERFORM WFILE.
           PERFORM GET-FILE-STATUS.
           PERFORM INITIALIZE-WS-RECORD.
       AD-EXIT.
         EXIT.

       SHOW-HELP SECTION.
           DISPLAY "H - HELP " AT LINE 15 COL 10.
           DISPLAY "A - ADD NEW RECORD " AT LINE 16 COL 10.
           DISPLAY "F - FIND RECORD " AT LINE 17 COL 10.
           DISPLAY "E - FIND RECORD " AT LINE 18 COL 10.
           DISPLAY "Q - QUIT PROGRAM " AT LINE 18 COL 10.
       SH-EXIT.
         EXIT.

       WFILE SECTION.
           CALL "WR-INDEX".
           MOVE WS-INDEX-NUMBER TO WS-NAME-CODE.
           MOVE ADDRESS-STRUCTURE TO ADDRESS-RECORD.
           OPEN OUTPUT ADDRESS-BOOK.
           WRITE ADDRESS-RECORD.
           CLOSE ADDRESS-BOOK.
       WF-EXIT.
          EXIT.

       BUILD-EMPTY-FILE SECTION.
            MOVE -1
            TO WS-INDEX-NUMBER.
            CALL "WR-INDEX".
            OPEN OUTPUT ADDRESS-BOOK.
            PERFORM GET-FILE-STATUS.
            IF NOT FS-SUCCESS
                STOP RUN
            END-IF.
            MOVE 'FIRST_RECORD'
            TO WS-LAST-NAME.
            MOVE WS-INDEX-NUMBER
            TO WS-NAME-CODE.
            MOVE ADDRESS-STRUCTURE
            TO ADDRESS-RECORD.
            WRITE ADDRESS-RECORD.
            PERFORM GET-FILE-STATUS.
            IF NOT FS-SUCCESS
                STOP RUN
            END-IF.
       BEF-EXIT.
           EXIT.

       GET-FILE-STATUS SECTION.
       COPY "FILESTATUS-PROCEDURE.CPY" REPLACING ==MSG== BY ==WS-MSG==.
           MOVE FUNCTION CONCATENATE(" FILE ACTION: ",WS-MSG) TO FS-MSG.
           DISPLAY FILE-STATUS-SCREEN.
       GFS-EXIT.

       INITIALIZE-WS-RECORD SECTION.
           MOVE SPACES TO WS-RESPONSE.
           MOVE SPACES TO WS-LAST-NAME.
           MOVE SPACES TO WS-FIRST-NAME.
           MOVE SPACES TO WS-STREET-NAME.
           MOVE SPACES TO WS-CITY.
           MOVE SPACES TO WS-STATE.
           MOVE SPACES TO WS-ZIP.
           MOVE SPACES TO WS-NOTES.
           MOVE ZERO TO WS-NAME-CODE.
       IWR-EXIT.
           EXIT.
