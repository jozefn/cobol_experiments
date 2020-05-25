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

       01 ws-response pic x.
       
       01 address-structure.
       copy "address-record.cpy" replacing ==(tag)== by ==ws-==.

       copy "filestatus.cpy".

       01 fs-msg                    pic x(30).
       01 ws-msg                    pic x(30).
       01 ws-message                pic x(30).


       screen section.
       01 blank-screen blank screen.

       01 action-entry-screen blank screen.
          05  value "Action: "                    line 1 col 10.
          05  response-input                      line 1 col 30 
                pic x to ws-response.
          05  msg-value                           line 10 col 30
                pic x(30) from ws-message.

       01 data-entry-screen blank screen.
          05  value "Action: "                    line 1 col 10.
          05  response-input                      line 1 col 30 
                pic x from ws-response.
          05  value "Last name:"                  line 3 col 10.
          05  last-name                           line 3 col 30 
                 pic x(20) to ws-last-name.
          05  value "First name:"                 line 4 col 10.
          05  first-name                          line 4 col 30 
                 pic x(20) to ws-first-name.
          05  value "Street name:"                line 5 col 10.
          05  street-name                         line 5 col 30 
                 pic x(40) to ws-street-name.
          05  value "City:"                       line 6 col 10. 
          05  city-name                           line 6 col 30 
                 pic x(40) to ws-city.
          05  value "State:"                      line 7 col 10.
          05  state-name                          line 7 col 30
                pic x(2) to ws-state.
          05  value "Zip:"                        line 8 col 10.
          05  zip-value                           line 8 col 30
                pic x(10) to ws-zip.
          05  value "notes:"                      line 9 col 10.
          05  zip-value                           line 9 col 30
                pic x(50) to ws-notes.
          05  msg-value                           line 10 col 30
                pic x(30) from ws-message.

       01 file-status-screen.
          05 status-msg                             line 12 col 30
             pic x(30) from fs-msg.


       procedure division.
            perform check-file-exist.
            perform show-screen until ws-response is equal to 'q'.
            stop run.  

       check-file-exist section.
            open input address-book
            if not fs-success
                if fs-no-file 
                    perform build-empty-file
                    close address-book
                else 
                    perform get-file-status
                end-if
            end-if.
       cef-exist.
           exit.

       show-screen section.
            perform initialize-ws-record.
            display action-entry-screen.
            accept action-entry-screen.

            if ws-response is equal to 'h' then
               perform show-help
            end-if.

            if ws-response is equal to 'a' then
               perform add-data
            end-if.


       ss-exit.
          exit.

       add-data section.
           display data-entry-screen.
           accept data-entry-screen.
           if ws-last-name is equal spaces then
              move " Must have last name " to ws-message
              go to ad-exit
           end-if.
           perform wfile.
           perform get-file-status.
           perform initialize-ws-record.
       ad-exit.
         exit.

       show-help section.
           display "h - help " at line 15 col 10.
           display "a - add new record " at line 16 col 10.
           display "f - find record " at line 17 col 10.
           display "e - find record " at line 18 col 10.
           display "q - quit program " at line 18 col 10.
       sh-exit.
         exit.

       wfile section.
           call "wr-index".
           move ws-index-number to ws-name-code.
           move address-structure to address-record.
           open output address-book.
           write address-record.
           close address-book.
       wf-exit.
          exit.

       build-empty-file section.
            move -1 
            to ws-index-number.
            call "wr-index".
            open output address-book.
            perform get-file-status.
            if not fs-success
                stop run
            end-if.
            move 'first_record'
            to ws-last-name.
            move ws-index-number 
            to ws-name-code.
            move address-structure
            to address-record.
            write address-record.
            perform get-file-status.
            if not fs-success
                stop run
            end-if.
       bef-exit.
           exit.

       get-file-status section.
       copy "filestatus-procedure.cpy" replacing ==msg== by ==ws-msg==.
           move function concatenate(" file action: ",ws-msg) to fs-msg.
           display file-status-screen.
       gfs-exit.

       initialize-ws-record section.
           move spaces to ws-response.
           move spaces to ws-last-name.
           move spaces to ws-first-name.
           move spaces to ws-street-name.
           move spaces to ws-city.
           move spaces to ws-state.
           move spaces to ws-zip.
           move spaces to ws-notes.
           move zero to ws-name-code.
       iwr-exit.
           exit.

       
