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
       01 ws-response pic x value 'c'.
       01 address-structure.
       copy "address-record.cpy" replacing ==(tag)== by ==ws-==.

       01 filestatus            pic x(02).
          88 record-found       value '00'.


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

       procedure division.
           display data-entry-screen.
           accept data-entry-screen.

           if ws-response is not equal to 'q' then
              perform wfile.

           stop run.  

       wfile section.
           open output address-book.
           write address-record from address-structure.
           close address-book.
       wf-exit.
          exit.


