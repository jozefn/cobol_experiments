       IDENTIFICATION DIVISION.
       PROGRAM-ID. ADDRESS.

       ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SPECIAL-NAMES.
            CLASS MYALPHA  IS "a" THRU "z", " ", "A" THRU "Z"
            CURSOR         IS      LLCC.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
            SELECT OPTIONAL     ADDRESS-FILE
                   ASSIGN       TO  OUTFILE
                   ORGANIZATION IS  INDEXED
                   RECORD KEY   IS FD-PHONE
                   ALTERNATE RECORD KEY IS FD-LAST-NAME  WITH DUPLICATES
                   FILE STATUS  IS  FILESTATUS
                   LOCK MODE    IS MANUAL WITH LOCK ON MULTIPLE RECORDS
                   ACCESS MODE  IS  DYNAMIC.

       DATA DIVISION.
        FILE SECTION.
        FD ADDRESS-FILE.
        01 ADDRESS-RECORD.
        COPY "ADDRESS-RECORD.CPY" REPLACING ==(TAG)== BY ==FD-==.

        WORKING-STORAGE SECTION.
        01 ADDRESS-STRUCTURE.
        COPY "ADDRESS-RECORD.CPY" REPLACING ==(TAG)== BY ==WS-==.



        COPY screenio.

        COPY "FILESTATUS.CPY".
        01  LLCC                       PIC 9(04).
        01  OUTFILE                    PIC X(50)
            VALUE "/mnt/c/cobfiles\address.idx".
        01  ID-NUM-ERROR               PIC X(01) VALUE SPACE.
        01  NAME-VALUE-ERROR           PIC X(01) VALUE SPACE.
        01  SCREEN-ERROR               PIC 9(01) VALUE ZERO.
            88 NO-SCREEN-ERROR-EXIST   VALUE 0.
            88 SCREEN-ERROR-EXISTS     VALUE 1.
        01  MSG-LINE                   PIC X(80).
        01  WS-ERROR                   PIC 9(01) VALUE 0.
            88 IN-ERROR                VALUE 1.
            88 NOT-IN-ERROR            VALUE 0.
        01  WS-PHONE1                  PIC X(03).
        01  WS-PHONE2                   PIC X(03).
        01  WS-PHONE3                   PIC X(04).
        01  WS-HYPHEN                   PIC S9(09) VALUE 0.
        01  P1                          PIC X.
        01  P2                          PIC X.
        01  P3                          PIC X.
        01  WS-COUNT                    PIC S9(09) VALUE 0.

        01  EXIT-KEY                   PIC X(85)
             VALUE "F1 = EXIT F4 = LOOKUP ENTER = SAVE "
             & "F2 = NEXT BY NAME F3 = LAST BY NAME".

        01  EXIT-KEY2                  PIC X(85)
             VALUE "F7 = BACK BY PHONE F8 = NEXT BY PHONE, "
             & "F9 = DELETE F10 = ADD NEW RECORD".

        01  SCREEN-STATUS PIC 9(04) .

        SCREEN SECTION.
        01 CLEAR-SCREEN BLANK SCREEN
           BACKGROUND-COLOR IS COB-COLOR-WHITE
           FOREGROUND-COLOR IS COB-COLOR-BLACK.

        01 SEARCH-SCREEN BACKGROUND-COLOR IS COB-COLOR-WHITE
                       FOREGROUND-COLOR IS COB-COLOR-BLACK.
           05  LINE 3 COL 10 VALUE "LAST NAME:".
           05  ERROR-LAST-NAME                     LINE 3 COL 9
                FOREGROUND-COLOR IS COB-COLOR-RED
                PIC X FROM ID-NUM-ERROR.
           05  LAST-NAME                           LINE 3 COL 30
                 PIC X(20) FROM FD-LAST-NAME TO FD-LAST-NAME.
           05  LINE 4 COL 10 VALUE "PHONE:".
           05  ERROR-PHONE                         LINE 4 COL 9
                FOREGROUND-COLOR IS COB-COLOR-RED
                PIC X FROM ID-NUM-ERROR.
           05  PHONE-NUMBER-AREA                   LINE 4 COL 30
                PIC X(12) FROM FD-PHONE TO FD-PHONE AUTO FULL UNDERLINE.
           05 KEY-DSC-AREA1    LINE 22 COLUMN 01
              BACKGROUND-COLOR IS COB-COLOR-BLUE
              FOREGROUND-COLOR IS COB-COLOR-WHITE
              PIC X(85)
              FROM EXIT-KEY.
           05 KEY-DSC-AREA2    LINE 23 COLUMN 01
              BACKGROUND-COLOR IS COB-COLOR-BLUE
              FOREGROUND-COLOR IS COB-COLOR-WHITE
              PIC X(85)
              FROM EXIT-KEY2.
           05 MSG-LINEX       LINE 24 COLUMN 01
              FOREGROUND-COLOR IS COB-COLOR-RED
              PIC X(77)
              FROM MSG-LINE.



        01 MAIN-SCREEN BACKGROUND-COLOR IS COB-COLOR-WHITE
                       FOREGROUND-COLOR IS COB-COLOR-BLACK.
           05  LINE 3 COL 10 VALUE "LAST NAME:".
           05  ERROR-LAST-NAME                     LINE 3 COL 9
                FOREGROUND-COLOR IS COB-COLOR-RED
                PIC X FROM ID-NUM-ERROR.
           05  LAST-NAME                           LINE 3 COL 30
                 PIC X(20) FROM FD-LAST-NAME TO FD-LAST-NAME.
           05  LINE 4 COL 10 VALUE "FIRST NAME:".
           05  FIRST-NAME                          LINE 4 COL 30
                 PIC X(20) FROM FD-FIRST-NAME TO FD-FIRST-NAME.
           05  LINE 5 COL 10 VALUE "STREET NAME:".
           05  STREET-NAME                         LINE 5 COL 30
                PIC X(40) FROM FD-STREET-NAME TO FD-STREET-NAME.
           05  LINE 6 COL 10 VALUE "CITY:".
           05  CITY-NAME                           LINE 6 COL 30
                PIC X(40) FROM FD-CITY TO FD-CITY.
           05  LINE 7 COL 10 VALUE "STATE:".
           05  STATE-NAME                          LINE 7 COL 30
                PIC X(2) FROM FD-STATE TO FD-STATE.
           05  LINE 8 COL 10 VALUE "ZIP:".
           05  ZIP-VALUE                           LINE 8 COL 30
                PIC X(10) FROM FD-ZIP TO FD-ZIP.
           05  LINE 9 COL 10 VALUE "PHONE:".
           05  ERROR-PHONE                         LINE 9 COL 9
                FOREGROUND-COLOR IS COB-COLOR-RED
                PIC X FROM ID-NUM-ERROR.
           05  PHONE-NUMBER-AREA                   LINE 9 COL 30
                PIC X(12) FROM FD-PHONE TO FD-PHONE AUTO FULL UNDERLINE.
           05  LINE 10 COL 10 VALUE "NOTES:".
           05  NOTES-VALUE                         LINE 10 COL 30
                PIC X(50) FROM FD-NOTES TO FD-NOTES.
           05 KEY-DSC-AREA1    LINE 22 COLUMN 01
              BACKGROUND-COLOR IS COB-COLOR-BLUE
              FOREGROUND-COLOR IS COB-COLOR-WHITE
              PIC X(85)
              FROM EXIT-KEY.
           05 KEY-DSC-AREA2    LINE 23 COLUMN 01
              BACKGROUND-COLOR IS COB-COLOR-BLUE
              FOREGROUND-COLOR IS COB-COLOR-WHITE
              PIC X(85)
              FROM EXIT-KEY2.
           05 MSG-LINEX       LINE 24 COLUMN 01
              FOREGROUND-COLOR IS COB-COLOR-RED
              PIC X(77)
              FROM MSG-LINE.
        01 RING-BELL          LINE 01 COLUMN 01 VALUE " " BELL.

       PROCEDURE DIVISION.
        MAIN SECTION.
        START-PRG.
        *> CONCUURENT USER TO SUPPORT FILE AND RECORD LOCKING.
            SET ENVIRONMENT "DB_HOME" TO "/mnt/c/cobfiles"
            PERFORM OPEN-FILE
            DISPLAY CLEAR-SCREEN
            PERFORM INITIALIZE-VARIABLES
        *>    PERFORM READ-NEXT-RECORD
            PERFORM SCREEN-LOOP THRU SCREEN-LOOP-EXIT.

        STOP-PRG.
            CLOSE ADDRESS-FILE.
            STOP RUN.

        OPEN-FILE.
            OPEN I-O ADDRESS-FILE
            IF FILESTATUS = '00' OR '05'
               CONTINUE
            ELSE
               DISPLAY 'CANNOT OPEN FILE ' FILESTATUS
               STOP RUN
            END-IF.

        INITIALIZE-VARIABLES.
            MOVE SPACES TO NAME-VALUE-ERROR.
            MOVE SPACES TO FD-LAST-NAME.
            MOVE SPACES TO FD-FIRST-NAME.
            MOVE SPACES TO FD-STREET-NAME.
            MOVE SPACES TO FD-CITY.
            MOVE SPACES TO FD-STATE.
            MOVE SPACES TO FD-ZIP.
            MOVE SPACES TO FD-NOTES.
            MOVE SPACES TO FD-PHONE.
            MOVE SPACES TO MSG-LINE.

        SCREEN-LOOP.
            MOVE FUNCTION UPPER-CASE(MSG-LINE) TO MSG-LINE
            SET NO-SCREEN-ERROR-EXIST TO TRUE
            DISPLAY MAIN-SCREEN
            ACCEPT  MAIN-SCREEN
            EVALUATE TRUE
               WHEN COB-CRT-STATUS = COB-SCR-F1
                  GO TO SCREEN-LOOP-EXIT
               WHEN COB-CRT-STATUS = COB-SCR-F2
                  PERFORM READ-NEXT-RECORD-BY-NAME
               WHEN COB-CRT-STATUS = COB-SCR-F3
                  PERFORM READ-LAST-RECORD-BY-NAME
               WHEN COB-CRT-STATUS = COB-SCR-OK
                  PERFORM EDIT-SCREEN-FIELDS
                  IF NO-SCREEN-ERROR-EXIST
                     MOVE SPACES TO MSG-LINE
                     PERFORM WRITE-RECORD
                  END-IF
               WHEN COB-CRT-STATUS = COB-SCR-F4
                  PERFORM INITIALIZE-VARIABLES
                  DISPLAY CLEAR-SCREEN
                  DISPLAY SEARCH-SCREEN
                  ACCEPT SEARCH-SCREEN
                  IF FD-LAST-NAME IS NOT EQUAL SPACES
                    PERFORM READ-NEXT-RECORD-BY-NAME
                  ELSE IF FD-PHONE NOT EQUAL SPACES
                    PERFORM READ-RECORD-BY-KEY
                  ELSE
                    MOVE "MUST CHOSE A NAME OR PHONE " TO MSG-LINE
                  END-IF
               WHEN COB-CRT-STATUS = COB-SCR-F7
                  PERFORM READ-LAST-RECORD
               WHEN COB-CRT-STATUS = COB-SCR-F8
                  PERFORM READ-NEXT-RECORD
               WHEN COB-CRT-STATUS = COB-SCR-F9
                  PERFORM DELETE-RECORD-BY-KEY
               WHEN COB-CRT-STATUS = COB-SCR-F10
                  PERFORM INITIALIZE-VARIABLES
                  MOVE "ADD NEW RECORD " TO MSG-LINE
                  GO TO SCREEN-LOOP
               WHEN OTHER
                  MOVE "INVALID RELEASE KEY PRESSED." TO MSG-LINE
                  DISPLAY RING-BELL
            END-EVALUATE
            GO TO SCREEN-LOOP.
        SCREEN-LOOP-EXIT.
            EXIT.

        RESET-SCREEN-IND.
            MOVE SPACES TO ID-NUM-ERROR NAME-VALUE-ERROR.

        WRITE-RECORD.
           MOVE ADDRESS-RECORD TO ADDRESS-STRUCTURE
           READ ADDRESS-FILE WITH LOCK KEY IS FD-PHONE  *> LOCK RECORD BEFORE UPDATI
           MOVE ADDRESS-STRUCTURE TO ADDRESS-RECORD

            WRITE ADDRESS-RECORD
            IF FILESTATUS = '00'
               MOVE "RECORD ADDED" TO MSG-LINE
            ELSE
               IF FILESTATUS = '22'
                  REWRITE ADDRESS-RECORD
                  IF FILESTATUS = '00' OR '02' *> 02 HANDLES DUP ALTERNATE KEY
                     MOVE "RECORD UPDATED" TO MSG-LINE
                  ELSE
                     STRING "CANNOT UPDATE RECORD."
                            " FILESTATUSUS = "
                            FILESTATUS DELIMITED BY SIZE
                            INTO MSG-LINE
                     END-STRING
                  END-IF
               ELSE
                  MOVE 'CANNOT WRITE RECORD ' TO MSG-LINE
                  STOP RUN
               END-IF
            END-IF.

        READ-RECORD-BY-KEY.
            PERFORM RESET-SCREEN-IND
            MOVE SPACES TO FD-LAST-NAME
            READ ADDRESS-FILE KEY IS FD-PHONE
              INVALID KEY
                MOVE "KEY NOT FOUND"          TO MSG-LINE
                DISPLAY RING-BELL
              NOT INVALID KEY
                MOVE "RECORD RETRIEVED"       TO MSG-LINE
            END-READ
            PERFORM RECORD-LOCK-CHECK.

        DELETE-RECORD-BY-KEY.
            PERFORM RESET-SCREEN-IND
            MOVE SPACES TO FD-LAST-NAME
            DELETE ADDRESS-FILE RECORD
              INVALID KEY
                MOVE "KEY NOT FOUND"          TO MSG-LINE
                DISPLAY RING-BELL
              NOT INVALID KEY
                MOVE "RECORD DELETED"         TO MSG-LINE
                MOVE SPACES TO FD-LAST-NAME
            END-DELETE.
            PERFORM RECORD-LOCK-CHECK.

        READ-NEXT-RECORD.
            PERFORM RESET-SCREEN-IND
            START ADDRESS-FILE KEY > FD-PHONE
              INVALID KEY
                MOVE "END OF FILE"            TO MSG-LINE
                DISPLAY RING-BELL
              NOT INVALID KEY
                READ ADDRESS-FILE NEXT
                  AT END
                     MOVE "END OF FILE"       TO MSG-LINE
                     DISPLAY RING-BELL
                  NOT AT END
                     MOVE "RECORD RETRIEVED"  TO MSG-LINE
                END-READ
                PERFORM RECORD-LOCK-CHECK
            END-START.

        READ-NEXT-RECORD-BY-NAME.
            PERFORM RESET-SCREEN-IND
            START ADDRESS-FILE KEY >= FD-LAST-NAME
              INVALID KEY
                MOVE "END OF FILE"            TO MSG-LINE
                DISPLAY RING-BELL
              NOT INVALID KEY
                READ ADDRESS-FILE NEXT
                  AT END
                     MOVE "END OF FILE"       TO MSG-LINE
                     DISPLAY RING-BELL
                  NOT AT END
                     MOVE "RECORD RETRIEVED"  TO MSG-LINE
                END-READ
                PERFORM RECORD-LOCK-CHECK
            END-START.

        READ-LAST-RECORD.
            PERFORM RESET-SCREEN-IND
            START ADDRESS-FILE KEY < FD-PHONE
              INVALID KEY
                MOVE "BEGINNING OF FILE"      TO MSG-LINE
                DISPLAY RING-BELL
              NOT INVALID KEY
                READ ADDRESS-FILE PREVIOUS
                  AT END
                     MOVE "BEGINNING OF FILE" TO MSG-LINE
                     DISPLAY RING-BELL
                  NOT AT END
                     MOVE "RECORD RETRIEVED"  TO MSG-LINE
                END-READ
                PERFORM RECORD-LOCK-CHECK
            END-START.

        READ-LAST-RECORD-BY-NAME.
            PERFORM RESET-SCREEN-IND
            START ADDRESS-FILE KEY < FD-LAST-NAME
              INVALID KEY
                MOVE "BEGINNING OF FILE"      TO MSG-LINE
                DISPLAY RING-BELL
              NOT INVALID KEY
                READ ADDRESS-FILE PREVIOUS
                  AT END
                     MOVE "BEGINNING OF FILE" TO MSG-LINE
                     DISPLAY RING-BELL
                  NOT AT END
                     MOVE "RECORD RETRIEVED"  TO MSG-LINE
                END-READ
                PERFORM RECORD-LOCK-CHECK
            END-START.

        RECORD-LOCK-CHECK.
           EVALUATE FILESTATUS
            WHEN 00 MOVE 'SUCCESS ' TO MSG-LINE
            WHEN 02 MOVE 'SUCCESS DUPLICATE ' TO MSG-LINE
            WHEN 04 MOVE 'SUCCESS INCOMPLETE ' TO MSG-LINE
            WHEN 05 MOVE 'SUCCESS OPTIONAL ' TO MSG-LINE
            WHEN 07 MOVE 'SUCCESS NO UNIT ' TO MSG-LINE
            WHEN 10 MOVE 'END OF FILE ' TO MSG-LINE
            WHEN 14 MOVE 'OUT OF KEY RANGE ' TO MSG-LINE
            WHEN 21 MOVE 'KEY INVALID ' TO MSG-LINE
            WHEN 22 MOVE 'KEY EXISTS ' TO MSG-LINE
            WHEN 23 MOVE 'KEY NOT EXISTS ' TO MSG-LINE
            WHEN 30 MOVE 'PERMANENT ERROR ' TO MSG-LINE
            WHEN 31 MOVE 'INCONSISTENT FILENAME ' TO MSG-LINE
            WHEN 34 MOVE 'BOUNDARY VIOLATION ' TO MSG-LINE
            WHEN 35 MOVE 'FILE NOT FOUND ' TO MSG-LINE
            WHEN 37 MOVE 'PERMISSION DENIED ' TO MSG-LINE
            WHEN 38 MOVE 'CLOSED WITH LOCK ' TO MSG-LINE
            WHEN 39 MOVE 'CONFLICT ATTRIBUTE ' TO MSG-LINE
            WHEN 41 MOVE 'ALREADY OPEN ' TO MSG-LINE
            WHEN 42 MOVE 'NOT OPEN ' TO MSG-LINE
            WHEN 43 MOVE 'READ NOT DONE ' TO MSG-LINE
            WHEN 44 MOVE 'RECORD OVERFLOW ' TO MSG-LINE
            WHEN 46 MOVE 'READ ERROR ' TO MSG-LINE
            WHEN 47 MOVE 'INPUT DENIED ' TO MSG-LINE
            WHEN 48 MOVE 'OUTPUT DENIED ' TO MSG-LINE
            WHEN 49 MOVE 'I/O DENIED ' TO MSG-LINE
            WHEN 51 MOVE 'RECORD LOCKED ' TO MSG-LINE
            WHEN 52 MOVE 'END-OF-PAGE ' TO MSG-LINE
            WHEN 57 MOVE 'I/O LINAGE ' TO MSG-LINE
            WHEN 61 MOVE 'FILE SHARING FAILURE ' TO MSG-LINE
            WHEN 91 MOVE 'FILE NOT AVAILABLE ' TO MSG-LINE
           END-EVALUATE.

        EDIT-SCREEN-FIELDS SECTION.
      ******************************************************
      * PERFORM EDITS IN REVERSE FIELD ORDER BEGINING WITH *
      * THE MOST COMMON ERROR FOR A FIELD FIRST.           *
      * ONCE A FIELD ERROR IS FOUND, SET THE ERROR FLAG    *
      * AND GO TO THE NEXT FIELD.                          *
      ******************************************************

        RESET-SCREEN-INFO.
            MOVE ZEROS  TO LLCC
            MOVE SPACES TO MSG-LINE
            PERFORM RESET-SCREEN-IND.

        EDIT-NAME-VALUE.
            IF FD-LAST-NAME = SPACES
               MOVE "NAME CANNOT BE BLANK" TO MSG-LINE
               SET SCREEN-ERROR-EXISTS TO TRUE
               MOVE 0030               TO LLCC
               MOVE "*" TO NAME-VALUE-ERROR
               GO TO EDIT-NAME-VALUE-EXIT
            END-IF
            IF FD-LAST-NAME NOT MYALPHA
               MOVE "NAME MUST CONTAIN ONLY ALPHA CHARACTERS"
                 TO MSG-LINE
               SET SCREEN-ERROR-EXISTS TO TRUE
               MOVE 0030               TO LLCC
               MOVE "*" TO NAME-VALUE-ERROR
            END-IF.
        EDIT-NAME-VALUE-EXIT.
            EXIT.

        EDIT-ID-FIELD.
            MOVE 0 TO WS-ERROR.
            MOVE 0 TO WS-COUNT.
            INSPECT FD-PHONE TALLYING WS-COUNT FOR ALL '-'.
            IF WS-COUNT NOT EQUAL 2
               MOVE 1 TO WS-ERROR
            END-IF.
            IF NOT-IN-ERROR
                UNSTRING FD-PHONE DELIMITED BY '-'
                        INTO WS-PHONE1 DELIMITER IN P1
                             WS-PHONE2 DELIMITER IN P2
                             WS-PHONE3 DELIMITER IN P3
                IF WS-PHONE1 NOT NUMERIC
                   MOVE 1 TO WS-ERROR
                END-IF
                IF WS-PHONE2 NOT NUMERIC
                   MOVE 1 TO WS-ERROR
                END-IF
                IF WS-PHONE3 NOT NUMERIC
                   MOVE 1 TO WS-ERROR
                END-IF
            END-IF.
            IF IN-ERROR
               MOVE "PHONE MUST BE IN ###-###-#### FORMAT" TO MSG-LINE
               SET SCREEN-ERROR-EXISTS TO TRUE
               DISPLAY LLCC LINE 25 COL 50
               MOVE 0630               TO LLCC
               MOVE "*" TO ID-NUM-ERROR
            END-IF.
        EDIT-ID-FIELD-EXIT.
            EXIT.

        END PROGRAM ADDRESS.
