       identification division.
       program-id. conditionals.

       data division.
       working-storage section.
       01 num1 pic 9(9).
       01 num2 pic 9(9).
       01 num3 pic 9(5).
       01 num4 pic 9(6).
       01 neg-num pic s9(9) value -1234.
       01 class1 pic x(9) value 'abcd '.
       01 check-val pic 9(3).
        88 pass values are 041 thru 100.
        88 fail values are 000 thru 40.

       procedure division.
        move 25 to num1 num3.
        move 15 to num2 num4.
        if num1 > num2 then
          display 'in loop 1 - if block'
          if num3 = num4 then
             display 'in loop2 - if block'    
          else
             display 'in loop2 - else block'
          end-if
        else
          display 'in loop 1 - else block'
        end-if
        move 65 to check-val.
        if pass
           display 'passed with 'check-val' marks.'.
        if fail
           display 'failed with 'check-val' marks.'.

        evaluate true
           when num1 < 2
                   display 'num1 less than 2'
           when num1 < 19
                   display 'num1 less than 19'
           when num1 < 100
                   display 'num1 less than 1000'
        end-evaluate.
        stop run.
           









