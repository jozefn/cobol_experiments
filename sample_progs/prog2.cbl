       identification division.
       program-id. vars.

       data division.
       working-storage section.

       01 first-var     pic s9(3)v9(2).
       01 second-var    pic s9(3)v9(2) value -123.45.
       01 third-var     pic a(6) value 'abcdef'.
       01 forth-var     pic x(5) value 'a123$'. 
       01 group-var.
          05 subvar-1   pic 9(3) value 337.
          05 subvar-2   pic x(15) value 'lalala'.
          05 subvar-3   pic x(15) value 'lalala'.
          05 subvar-4   pic x(15) value 'lalala'.


       procedure division. 
           display "1st var :"first-var.
           display "2st var :"second-var.
           display "3rd var :"third-var.
           display "4th var :"forth-var.
           display "group var :"group-var.
           stop run.

