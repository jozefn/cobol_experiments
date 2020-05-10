       identification division.
       program-id. wr-index.

       environment division.
       input-output section.
       file-control.
           select index-file assign to 'abindex'
                   status is ws-index-status
                   organization is sequential.
 
       data division.

       file section.
       fd index-file.
       01 index-record.
          05 index-field pic 9(09).


       working-storage section.
       01 ws-index-status pic x(02).
       01 ws-index-number external pic s9(09).
       01 ws-index-record.
          05 ws-index-field pic 9(09).

       procedure division.
           open input index-file.
           if ws-index-status not equal '00'
              open output index-file
              move 0 to ws-index-field
              write index-record from ws-index-record
              close index-file.
           if ws-index-number less than 0
              open output index-file
              move 0 to ws-index-field
              write index-record from ws-index-record
              close index-file.

           open input index-file.
           read index-file into ws-index-record.
           close index-file.
           add 1 to ws-index-field
           open output index-file.
           write index-record from ws-index-record.
           close index-file.
           move ws-index-field to ws-index-number.
       goback.




