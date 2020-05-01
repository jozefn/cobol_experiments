       identification division.
       program-id. verbs.

       data division.
       working-storage section.
                         
      
       01 num1 pic 9(9) value 10.
       01 num2 pic 9(9) value 10.
       01 numa pic 9(9) value 100.
       01 numb pic 9(9) value 15.
       01 numc pic 9(9).  
       01 res-div pic 9(9).
       01 res-mult pic 9(9).
       01 res-sub pic 9(9).
       01 res-add pic 9(9).
       01 res-mov pic x(9).

       procedure division.
           compute numc = (num1 * num2).
           divide numa by numb giving res-div.
           multiply numa by numb giving res-mult.
           subtract numa from numb giving res-sub.
           add numa to numb giving res-add.
           initialize num1.
           initialize num2 replacing numeric data by 12345.
           move numa
             to res-mov.
           display "numc:"numc.
           display "res-div:"res-div.
           display "res-mult:"res-mult.
           display "res-sub:"res-sub.
           display "res-add:"res-add.
           display "res-mov:"res-mov.
           display "reinitialized num1: "num1.
           display "reinitialized num2: "num2.
           stop run.
