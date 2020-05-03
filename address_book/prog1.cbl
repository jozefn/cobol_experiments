       identification division.
       program-id. prog1.

       environment division.

       data division.

       working-storage section.
       01 ws-index-number external pic 9(09).

       procedure division.
           call "wr-index".
           display "new index: "ws-index-number.
           stop run.  
