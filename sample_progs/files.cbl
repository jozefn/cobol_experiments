       identification division.
       program-id. files.

       environment division.
       input-output section.
       file-control.
           select transactions assign to 'transactions.txt'
                   organization is sequential.

       data division.
       file section.
       fd transactions.
       01 transaction-structure.
               02 uid pic 9(5).
               02 desc pic x(25).
               02 details.
                       03 amount pic 9(6)v9(2).
                       03 start-balance pic 9(6)v9(2).
                       03 end-balance pic 9(6)v9(2).
               02 account-id pic 9(7).
               02 account-holder pic A(50).

       working-storage section.
       01 transaction-record.
               02 ws-uid pic 9(5).
               02 ws-desc pic x(25).
               02 ws-details.
                       03 ws-amount pic 9(6)v9(2).
                       03 ws-start-balance pic 9(6)v9(2).
                       03 ws-end-balance pic 9(6)v9(2).
               02 ws-account-id pic 9(7).
               02 ws-account-holder pic A(50).




       procedure division.
           display 'writing record'.
           move 12345 to ws-uid.
           move 'test transaction' to ws-desc.
           move 000124.34 to ws-amount.
           move 000177.54 to ws-start-balance.
           move 53.2 to ws-end-balance
           move '11111' to ws-account-id.
           move 'Joseph' to ws-account-holder.
           open output transactions.
           write transaction-structure from transaction-record.
           close transactions.
           stop run.  
