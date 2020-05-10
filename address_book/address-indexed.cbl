       identification division.
       program-id. screen.

       environment division.
       input-output section.
       file-control.
           select address-book assign to "address_book.dat"
                   organization is indexed
                   access mode is dynamic
                   record key is fd-name-code
                   alternate record key is fd-last-name
                   with duplicates file status is filestatus.
 

       data division.

       file section.
       fd address-book.
       01 address-record.
       copy "address-record.cpy" replacing ==(tag)== by ==fd-==.
       

       working-storage section.
      *> following variable is shared with wr-index to get next
      *> index for indexed record
       01 ws-index-number external pic s9(09).

       01 ws-response pic x value 'c'.
       01 address-structure.
       copy "address-record.cpy" replacing ==(tag)== by ==ws-==.

       copy "filestatus.cpy".

       01 fs-msg                    pic x(30).


       screen section.
       01 data-entry-screen.
          05  value "Last name:" blank screen     line 1 col 10.
          05  last-name                           line 1 col 30 
                 pic x(20) to ws-last-name.
          05  value "First name:"                 line 2 col 10.
          05  first-name                          line 2 col 30 
                 pic x(20) to ws-first-name.
          05  value "Street name:"                line 3 col 10.
          05  street-name                         line 3 col 30 
                 pic x(40) to ws-street-name.
          05  value "City:"                       line 4 col 10. 
          05  city-name                           line 4 col 30 
                 pic x(40) to ws-city.
          05  value "State:"                      line 5 col 10.
          05  state-name                          line 5 col 30
                pic x(2) to ws-state.
          05  value "Zip:"                        line 6 col 10.
          05  zip-value                           line 6 col 30
                pic x(10) to ws-zip.
          05  value "notes:"                      line 7 col 10.
          05  zip-value                           line 7 col 30
                pic x(50) to ws-notes.

          05  value "c - to continue"                    line 8 col 10.
          05  value "q - to quit"                        line 9 col 10.
          05  value "enter response"                     line 10 col 10.
          05  response-input                             line 10 col 30 
                pic x to ws-response.

       01 file-status-screen.
          05 status-msg                             line 12 col 30
             pic x(30) from fs-msg.


       procedure division.
            open input address-book
            if not fs-success
                if fs-no-file 
                    perform build-empty-file
                    close address-book
                else 
                    display file-status-screen
                    stop run
                end-if
            end-if.

            open i-o address-book.  
            display data-entry-screen.
            accept data-entry-screen.

            if ws-response is not equal to 'q' then
                perform wfile
            end-if.

            close address-book.
            stop run.  


       wfile section.
            call "wr-index".
            move ws-index-number 
            to ws-name-code.
            move address-structure
            to address-record.
            write address-record.
            if fs-success
                move "record written" 
                to fs-msg
                display file-status-screen
            end-if.
       wf-exit.
          exit.

       build-empty-file section.
            move -1 
            to ws-index-number.
            call "wr-index".
            open output address-book.
            if not fs-success
                move function concatenate(" error status: ";filestatus)
                  to fs-msg
                display file-status-screen
                stop run
            end-if.
            move 'first_record'
            to ws-last-name.
            move ws-index-number 
            to ws-name-code.
            move address-structure
            to address-record.
            write address-record.
            if not fs-success
                move function concatenate(" error status: ";filestatus)
                  to fs-msg
                display file-status-screen
                stop run
            end-if.
       bef-exit.
           exit.

