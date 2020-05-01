       identification division.
       program-id. screen.

       data division.

       working-storage section.
       01 response-in-ws pic x value 'c'.
       01 ws-id-in pic x(04).
       01 ws-name-in pic x(20).


       screen section.
       01 data-entry-screen.
          05  value "data entry screen" blank screen     line 1 col 35.
          05  value "id #"                               line 3 col 10.
          05  id-input                                   line 3 col 25
                 pic x(4) to ws-id-in.
          05  value "name"                               line 5 col 10.
          05  name-input                                 line 5 col 25
                 pic x(20) to ws-name-in.
          05  value "c - to continue"                    line 11 col 30.
          05  value "q - to quit"                        line 12 col 30.
          05  value "enter response"                     line 14 col 30.
          05  response-input                             line 14 col 45 
                pic x to response-in-ws.

       procedure division.
           display data-entry-screen.
           accept data-entry-screen.
           display ws-id-in at line 20.
           display ws-name-in at line 21.
           stop run.  
