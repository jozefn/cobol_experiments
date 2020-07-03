       IDENTIFICATION DIVISION.
       PROGRAM-ID. ADDRESS.
      
       ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SPECIAL-NAMES.
            CLASS MYALPHA  IS "A" THRU "Z", " ", "A" THRU "Z"
            CURSOR         IS      LLCC.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
            SELECT OPTIONAL     INDEXED-FILE
                   ASSIGN       TO  OUTFILE
                   ORGANIZATION IS  INDEXED
                   RECORD KEY   IS  PH-NUMBER
                   ALTERNATE RECORD KEY IS NAME-VALUE  WITH DUPLICATES
                   FILE STATUS  IS  FILE-STAT
                   LOCK MODE    IS MANUAL WITH LOCK ON MULTIPLE RECORDS
                   ACCESS MODE  IS  DYNAMIC.
      
       DATA DIVISION.
        FILE SECTION.
        FD   INDEXED-FILE.
        01   INDEXED-RECORD.
             02  PH-NUMBER             PIC 9(4).
             02  NAME-VALUE            PIC X(40).
      
        WORKING-STORAGE SECTION.
      
        01   WK-RECORD.
             02  WK-PH-NUMBER          PIC 9(4).
             02  WK-NAME-VALUE         PIC X(40).
      
      
        COPY SCREENIO.
        01  LLCC                       PIC 9(04).
        01  OUTFILE                    PIC X(50)
            VALUE "/MNT/C/COBFILES\SAMPLE4JOUT.IDX".
        01  FILE-STAT                  PIC X(02).
        01  ID-NUM-ERROR               PIC X(01) VALUE SPACE.
        01  NAME-VALUE-ERROR           PIC X(01) VALUE SPACE.
        01  SCREEN-ERROR               PIC 9(01) VALUE ZERO.
            88 NO-SCREEN-ERROR-EXIST   VALUE 0.
            88 SCREEN-ERROR-EXISTS     VALUE 1.
        01  MSG-LINE                   PIC X(80).
        01  EXIT-KEY                   PIC X(85)
             VALUE "F1 = EXIT F4 = LOOKUP ENTER = SAVE "
             & "F2 = NEXT BY NAME F3 = LAST BY NAME".
      
        01  EXIT-KEY2                  PIC X(85)
             VALUE "F7 = BACK BY PHONE F8 = NEXT BY PHONE, "
             & "F9 = DELETE".
      
        01  SCREEN-STATUS PIC 9(04) .
      
        SCREEN SECTION.
        01 CLEAR-SCREEN BLANK SCREEN
           BACKGROUND-COLOR IS COB-COLOR-WHITE
           FOREGROUND-COLOR IS COB-COLOR-BLACK.

        01 MAIN-SCREEN BACKGROUND-COLOR IS COB-COLOR-WHITE
                       FOREGROUND-COLOR IS COB-COLOR-BLACK.
           05                 LINE 01
                              COLUMN 01 VALUE "PH-NUMBER:" HIGHLIGHT.
           05 PH-NUMBER-ERROR LINE 01 COLUMN 12
              FOREGROUND-COLOR IS COB-COLOR-RED
              PIC X(1) FROM ID-NUM-ERROR.
           05 PH-NUMBER-AREA  LINE 01 COLUMN 14
              FOREGROUND-COLOR IS COB-COLOR-BLUE
              PIC 9(4)
              FROM PH-NUMBER TO PH-NUMBER AUTO FULL UNDERLINE.
           05                 LINE 03 COLUMN 01
              VALUE "NAME:     "  HIGHLIGHT.
           05 NAME-ERROR      LINE 03 COLUMN 12
              FOREGROUND-COLOR IS COB-COLOR-RED
              PIC X(1)
              FROM NAME-VALUE-ERROR.
           05 NAME-AREA       LINE 03 COLUMN 14
              FOREGROUND-COLOR IS COB-COLOR-BLUE
              PIC X(40)
              FROM NAME-VALUE TO NAME-VALUE AUTO UNDERLINE.
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
            SET ENVIRONMENT "DB_HOME" TO "/MNT/C/COBFILES"
            PERFORM OPEN-FILE
            DISPLAY CLEAR-SCREEN
            PERFORM INITIALIZE-VARIABLES
            PERFORM READ-NEXT-RECORD
            PERFORM SCREEN-LOOP THRU SCREEN-LOOP-EXIT.
      
        STOP-PRG.
            CLOSE INDEXED-FILE.
            STOP RUN.
      
        OPEN-FILE.
            OPEN I-O INDEXED-FILE
            IF FILE-STAT = '00' OR '05'
               CONTINUE
            ELSE
               DISPLAY 'CANNOT OPEN FILE ' FILE-STAT
               STOP RUN
            END-IF.
      
        INITIALIZE-VARIABLES.
            MOVE ZEROS  TO PH-NUMBER
            MOVE SPACES TO NAME-VALUE NAME-VALUE-ERROR
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
                  PERFORM READ-RECORD-BY-KEY
               WHEN COB-CRT-STATUS = COB-SCR-F7
                  PERFORM READ-LAST-RECORD
               WHEN COB-CRT-STATUS = COB-SCR-F8
                  PERFORM READ-NEXT-RECORD
               WHEN COB-CRT-STATUS = COB-SCR-F9
                  PERFORM DELETE-RECORD-BY-KEY
               WHEN OTHER
                  MOVE "INVALID RELEASE KEY PRESSED." TO MSG-LINE
                  DISPLAY RING-BELL
            END-EVALUATE
            GO TO SCREEN-LOOP.
        SCREEN-LOOP-EXIT.
            EXIT.
      
        RESET-SCREEN-IND.
            MOVE SPACES TO ID-NUM-ERROR NAME-VALUE-ERROR.
      
        UPPER-CASE-FIELDS.
            MOVE FUNCTION UPPER-CASE(NAME-VALUE) TO NAME-VALUE.
      
        WRITE-RECORD.
            MOVE INDEXED-RECORD TO WK-RECORD
            READ INDEXED-FILE WITH LOCK KEY IS PH-NUMBER  *> LOCK RECORD BEFORE UPDATI
            MOVE WK-RECORD TO INDEXED-RECORD
      
            WRITE INDEXED-RECORD
            IF FILE-STAT = '00'
               MOVE "RECORD ADDED" TO MSG-LINE
            ELSE
               IF FILE-STAT = '22'
                  REWRITE INDEXED-RECORD
                  IF FILE-STAT = '00' OR '02' *> 02 HANDLES DUP ALTERNATE KEY
                     MOVE "RECORD UPDATED" TO MSG-LINE
                  ELSE
                     STRING "CANNOT UPDATE RECORD."
                            " FILE-STATUS = "
                            FILE-STAT DELIMITED BY SIZE
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
            MOVE SPACES TO NAME-VALUE
            READ INDEXED-FILE KEY IS PH-NUMBER
              INVALID KEY
                MOVE "KEY NOT FOUND"          TO MSG-LINE
                DISPLAY RING-BELL
              NOT INVALID KEY
                MOVE "RECORD RETRIEVED"       TO MSG-LINE
            END-READ
            PERFORM RECORD-LOCK-CHECK.
      
        DELETE-RECORD-BY-KEY.
            PERFORM RESET-SCREEN-IND
            MOVE SPACES TO NAME-VALUE
            DELETE INDEXED-FILE RECORD
              INVALID KEY
                MOVE "KEY NOT FOUND"          TO MSG-LINE
                DISPLAY RING-BELL
              NOT INVALID KEY
                MOVE "RECORD DELETED"         TO MSG-LINE
                MOVE SPACES TO NAME-VALUE
            END-DELETE.
            PERFORM RECORD-LOCK-CHECK.
      
        READ-NEXT-RECORD.
            PERFORM RESET-SCREEN-IND
            START INDEXED-FILE KEY > PH-NUMBER
              INVALID KEY
                MOVE "END OF FILE"            TO MSG-LINE
                DISPLAY RING-BELL
              NOT INVALID KEY
                READ INDEXED-FILE NEXT
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
            PERFORM UPPER-CASE-FIELDS
            START INDEXED-FILE KEY > NAME-VALUE
              INVALID KEY
                MOVE "END OF FILE"            TO MSG-LINE
                DISPLAY RING-BELL
              NOT INVALID KEY
                READ INDEXED-FILE NEXT
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
            START INDEXED-FILE KEY < PH-NUMBER
              INVALID KEY
                MOVE "BEGINNING OF FILE"      TO MSG-LINE
                DISPLAY RING-BELL
              NOT INVALID KEY
                READ INDEXED-FILE PREVIOUS
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
            PERFORM UPPER-CASE-FIELDS
            START INDEXED-FILE KEY < NAME-VALUE
              INVALID KEY
                MOVE "BEGINNING OF FILE"      TO MSG-LINE
                DISPLAY RING-BELL
              NOT INVALID KEY
                READ INDEXED-FILE PREVIOUS
                  AT END
                     MOVE "BEGINNING OF FILE" TO MSG-LINE
                     DISPLAY RING-BELL
                  NOT AT END
                     MOVE "RECORD RETRIEVED"  TO MSG-LINE
                END-READ
                PERFORM RECORD-LOCK-CHECK
            END-START.
      
        RECORD-LOCK-CHECK.
           EVALUATE FILE-STAT
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
            PERFORM RESET-SCREEN-IND
            PERFORM UPPER-CASE-FIELDS.
      
        EDIT-NAME-VALUE.
            IF NAME-VALUE = SPACES
               MOVE "NAME CANNOT BE BLANK" TO MSG-LINE
               SET SCREEN-ERROR-EXISTS TO TRUE
               MOVE 0314               TO LLCC
               MOVE "*" TO NAME-VALUE-ERROR
               DISPLAY RING-BELL
               GO TO EDIT-NAME-VALUE-EXIT
            END-IF
            IF NAME-VALUE NOT MYALPHA
               MOVE "NAME MUST CONTAIN ONLY ALPHA CHARACTERS"
                 TO MSG-LINE
               SET SCREEN-ERROR-EXISTS TO TRUE
               MOVE 0314               TO LLCC
               MOVE "*" TO NAME-VALUE-ERROR
               DISPLAY RING-BELL
            END-IF.
        EDIT-NAME-VALUE-EXIT.
            EXIT.
      
        EDIT-ID-FIELD.
            IF PH-NUMBER NOT NUMERIC
               MOVE "PH-NUMBER MUST BE NUMERIC" TO MSG-LINE
               SET SCREEN-ERROR-EXISTS TO TRUE
               MOVE 0114               TO LLCC
               MOVE "*" TO ID-NUM-ERROR
               DISPLAY RING-BELL
               GO TO EDIT-ID-FIELD-EXIT
            END-IF
            IF PH-NUMBER < 1
               MOVE "PH-NUMBER MUST BE GREATER THAN ZERO" TO MSG-LINE
               SET SCREEN-ERROR-EXISTS TO TRUE
               MOVE 0114               TO LLCC
               MOVE "*" TO ID-NUM-ERROR
               DISPLAY RING-BELL
            END-IF.
        EDIT-ID-FIELD-EXIT.
            EXIT.
      
        END PROGRAM SAMPLE4J.

