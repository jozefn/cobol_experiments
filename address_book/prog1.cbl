       identification division.
       program-id. prog1.

       environment division.

       data division.

       working-storage section.
       01 ws-index-number external pic s9(09).

       procedure division.
            move -1 to ws-index-number.
           call "wr-index".
           display "new index: "ws-index-number.
           stop run.  
