       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG1.

       ENVIRONMENT DIVISION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       01 WS-INDEX-NUMBER EXTERNAL PIC S9(09).

       PROCEDURE DIVISION.
            MOVE -1 TO WS-INDEX-NUMBER.
           CALL "WR-INDEX".
           DISPLAY "NEW INDEX: "WS-INDEX-NUMBER.
           STOP RUN.  
