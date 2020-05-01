       identification division.
       program-id. sections-test.

       data division.
       working-storage section.

       procedure division.
       main-line section.
        perform sec-a.
        perform sec-b
        stop run.

       sec-a section.
           display 'in section-a'.

       a-exit.
         exit.

       sec-b section.
           display 'in section-b'.

       b-exit.
         exit.
          









