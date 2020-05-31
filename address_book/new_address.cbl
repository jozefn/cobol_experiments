       identification division.
       program-id. address.
      
       environment division.
        configuration section.
        special-names.
            class myalpha  is "a" thru "z", " ", "A" thru "Z"
            cursor         is      llcc.
        input-output section.
        file-control.
            select optional     address-file
                   assign       to  outfile
                   organization is  indexed
                   record key   is fd-phone
                   alternate record key is fd-last-name  with duplicates
                   file status  is  filestatus
                   lock mode    is manual with lock on multiple records
                   access mode  is  dynamic.
      
       data division.
        file section.
        fd address-file.
        01 address-record.
        copy "address-record.cpy" replacing ==(tag)== by ==fd-==.

        working-storage section.
        01 address-structure.
        copy "address-record.cpy" replacing ==(tag)== by ==ws-==.



        copy screenio.

        copy "filestatus.cpy".
        01  llcc                       pic 9(04).
        01  outfile                    pic x(50)
            value "/mnt/c/cobfiles\address.idx".
        01  id-num-error               pic x(01) value space.
        01  name-value-error           pic x(01) value space.
        01  screen-error               pic 9(01) value zero.
            88 no-screen-error-exist   value 0.
            88 screen-error-exists     value 1.
        01  msg-line                   pic x(80).
        01  exit-key                   pic x(85)
             value "f1 = exit f4 = lookup enter = save "
             & "f2 = next by name f3 = last by name".
      
        01  exit-key2                  pic x(85)
             value "f7 = back by phone f8 = next by phone, "
             & "f9 = delete".
      
        01  screen-status pic 9(04) .
      
        screen section.
        01 clear-screen blank screen
           background-color is cob-color-white
           foreground-color is cob-color-black.

        01 search-screen background-color is cob-color-white
                       foreground-color is cob-color-black.
           05  line 3 col 10 value "Last name:".  
           05  error-last-name                     line 3 col 9
                foreground-color is cob-color-red 
                pic x from id-num-error.
           05  last-name                           line 3 col 30 
                 pic x(20) from fd-last-name to fd-last-name.
           05  line 4 col 10 value "Phone:".
           05  error-phone                         line 4 col 9
                foreground-color is cob-color-red 
                pic x from id-num-error.
           05  phone-number-area                   line 4 col 30
                pic x(12) from fd-phone to fd-phone auto full underline.
           05 key-dsc-area1    line 22 column 01
              background-color is cob-color-blue
              foreground-color is cob-color-white
              pic x(85)
              from exit-key.
           05 key-dsc-area2    line 23 column 01
              background-color is cob-color-blue
              foreground-color is cob-color-white
              pic x(85)
              from exit-key2.
           05 msg-linex       line 24 column 01
              foreground-color is cob-color-red
              pic x(77)
              from msg-line.



        01 main-screen background-color is cob-color-white
                       foreground-color is cob-color-black.
           05  line 3 col 10 value "Last name:".  
           05  error-last-name                     line 3 col 9
                foreground-color is cob-color-red 
                pic x from id-num-error.
           05  last-name                           line 3 col 30 
                 pic x(20) from fd-last-name to fd-last-name.
           05  line 4 col 10 value "First name:".
           05  first-name                          line 4 col 30 
                 pic x(20) from fd-first-name to fd-first-name.
           05  line 5 col 10 value "Street name:". 
           05  street-name                         line 5 col 30 
                pic x(40) from fd-street-name to fd-street-name.
           05  line 6 col 10 value "City:".
           05  city-name                           line 6 col 30 
                pic x(40) from fd-city to fd-city.
           05  line 7 col 10 value "State:".
           05  state-name                          line 7 col 30
                pic x(2) from fd-state to fd-state.
           05  line 8 col 10 value "Zip:". 
           05  zip-value                           line 8 col 30
                pic x(10) from fd-zip to fd-zip.
           05  line 9 col 10 value "Phone:".
           05  error-phone                         line 9 col 9
                foreground-color is cob-color-red 
                pic x from id-num-error.
           05  phone-number-area                   line 9 col 30
                pic x(12) from fd-phone to fd-phone auto full underline.
           05  line 10 col 10 value "notes:". 
           05  notes-value                         line 10 col 30
                pic x(50) from fd-notes to fd-notes.
           05 key-dsc-area1    line 22 column 01
              background-color is cob-color-blue
              foreground-color is cob-color-white
              pic x(85)
              from exit-key.
           05 key-dsc-area2    line 23 column 01
              background-color is cob-color-blue
              foreground-color is cob-color-white
              pic x(85)
              from exit-key2.
           05 msg-linex       line 24 column 01
              foreground-color is cob-color-red
              pic x(77)
              from msg-line.
        01 ring-bell          line 01 column 01 value " " bell.
      
       procedure division.
        main section.
        start-prg.
        *> concuurent user to support file and record locking.
            set environment "db_home" to "/mnt/c/cobfiles"
            perform open-file
            display clear-screen
            perform initialize-variables
        *>    perform read-next-record
            perform screen-loop thru screen-loop-exit.
      
        stop-prg.
            close address-file.
            stop run.
      
        open-file.
            open i-o address-file
            if filestatus = '00' or '05'
               continue
            else
               display 'cannot open file ' filestatus
               stop run
            end-if.
      
        initialize-variables.
            move spaces to name-value-error.
            move spaces to fd-last-name.
            move spaces to fd-first-name.
            move spaces to fd-street-name.
            move spaces to fd-city.
            move spaces to fd-state.
            move spaces to fd-zip.
            move spaces to fd-notes.
            move spaces to fd-phone.
            move spaces to msg-line.
      
        screen-loop.
            move function upper-case(msg-line) to msg-line
            set no-screen-error-exist to true
            display main-screen
            accept  main-screen
            evaluate true
               when cob-crt-status = cob-scr-f1
                  go to screen-loop-exit
               when cob-crt-status = cob-scr-f2
                  perform read-next-record-by-name
               when cob-crt-status = cob-scr-f3
                  perform read-last-record-by-name
               when cob-crt-status = cob-scr-ok
                  perform edit-screen-fields
                  if no-screen-error-exist
                     move spaces to msg-line
                     perform write-record
                  end-if
               when cob-crt-status = cob-scr-f4
                  perform initialize-variables
                  display clear-screen
                  display search-screen
                  accept search-screen
                  if fd-last-name is not equal spaces
                    perform read-next-record-by-name
                  else if fd-phone not equal spaces
                    perform read-record-by-key
                  else
                    move "must chose a name or phone " to msg-line
                  end-if
               when cob-crt-status = cob-scr-f7
                  perform read-last-record
               when cob-crt-status = cob-scr-f8
                  perform read-next-record
               when cob-crt-status = cob-scr-f9
                  perform delete-record-by-key
               when other
                  move "invalid release key pressed." to msg-line
                  display ring-bell
            end-evaluate
            go to screen-loop.
        screen-loop-exit.
            exit.
      
        reset-screen-ind.
            move spaces to id-num-error name-value-error.
      
        write-record.
           move address-record to address-structure
           read address-file with lock key is fd-phone  *> lock record before updati
           move address-structure to address-record
      
            write address-record
            if filestatus = '00'
               move "record added" to msg-line
            else
               if filestatus = '22'
                  rewrite address-record
                  if filestatus = '00' or '02' *> 02 handles dup alternate key
                     move "record updated" to msg-line
                  else
                     string "cannot update record."
                            " filestatusus = "
                            filestatus delimited by size
                            into msg-line
                     end-string
                  end-if
               else
                  move 'cannot write record ' to msg-line
                  stop run
               end-if
            end-if.
      
        read-record-by-key.
            perform reset-screen-ind
            move spaces to fd-last-name
            read address-file key is fd-phone
              invalid key
                move "key not found"          to msg-line
                display ring-bell
              not invalid key
                move "record retrieved"       to msg-line
            end-read
            perform record-lock-check.
      
        delete-record-by-key.
            perform reset-screen-ind
            move spaces to fd-last-name
            delete address-file record
              invalid key
                move "key not found"          to msg-line
                display ring-bell
              not invalid key
                move "record deleted"         to msg-line
                move spaces to fd-last-name 
            end-delete.
            perform record-lock-check.
      
        read-next-record.
            perform reset-screen-ind
            start address-file key > fd-phone
              invalid key
                move "end of file"            to msg-line
                display ring-bell
              not invalid key
                read address-file next
                  at end
                     move "end of file"       to msg-line
                     display ring-bell
                  not at end
                     move "record retrieved"  to msg-line
                end-read
                perform record-lock-check
            end-start.
      
        read-next-record-by-name.
            perform reset-screen-ind
            start address-file key >= fd-last-name
              invalid key
                move "end of file"            to msg-line
                display ring-bell
              not invalid key
                read address-file next
                  at end
                     move "end of file"       to msg-line
                     display ring-bell
                  not at end
                     move "record retrieved"  to msg-line
                end-read
                perform record-lock-check
            end-start.
      
        read-last-record.
            perform reset-screen-ind
            start address-file key < fd-phone
              invalid key
                move "beginning of file"      to msg-line
                display ring-bell
              not invalid key
                read address-file previous
                  at end
                     move "beginning of file" to msg-line
                     display ring-bell
                  not at end
                     move "record retrieved"  to msg-line
                end-read
                perform record-lock-check
            end-start.
      
        read-last-record-by-name.
            perform reset-screen-ind
            start address-file key < fd-last-name
              invalid key
                move "beginning of file"      to msg-line
                display ring-bell
              not invalid key
                read address-file previous
                  at end
                     move "beginning of file" to msg-line
                     display ring-bell
                  not at end
                     move "record retrieved"  to msg-line
                end-read
                perform record-lock-check
            end-start.
      
        record-lock-check.
           evaluate filestatus
            when 00 move 'success ' to msg-line
            when 02 move 'success duplicate ' to msg-line
            when 04 move 'success incomplete ' to msg-line
            when 05 move 'success optional ' to msg-line
            when 07 move 'success no unit ' to msg-line
            when 10 move 'end of file ' to msg-line
            when 14 move 'out of key range ' to msg-line
            when 21 move 'key invalid ' to msg-line
            when 22 move 'key exists ' to msg-line
            when 23 move 'key not exists ' to msg-line
            when 30 move 'permanent error ' to msg-line
            when 31 move 'inconsistent filename ' to msg-line
            when 34 move 'boundary violation ' to msg-line
            when 35 move 'file not found ' to msg-line
            when 37 move 'permission denied ' to msg-line
            when 38 move 'closed with lock ' to msg-line
            when 39 move 'conflict attribute ' to msg-line
            when 41 move 'already open ' to msg-line
            when 42 move 'not open ' to msg-line
            when 43 move 'read not done ' to msg-line
            when 44 move 'record overflow ' to msg-line
            when 46 move 'read error ' to msg-line
            when 47 move 'input denied ' to msg-line
            when 48 move 'output denied ' to msg-line
            when 49 move 'i/o denied ' to msg-line
            when 51 move 'record locked ' to msg-line
            when 52 move 'end-of-page ' to msg-line
            when 57 move 'i/o linage ' to msg-line
            when 61 move 'file sharing failure ' to msg-line
            when 91 move 'file not available ' to msg-line
           end-evaluate.
      
        edit-screen-fields section.
      ******************************************************
      * perform edits in reverse field order begining with *
      * the most common error for a field first.           *
      * once a field error is found, set the error flag    *
      * and go to the next field.                          *
      ******************************************************
      
        reset-screen-info.
            move zeros  to llcc
            move spaces to msg-line
            perform reset-screen-ind.
      
        edit-name-value.
            if fd-last-name = spaces
               move "name cannot be blank" to msg-line
               set screen-error-exists to true
               move 0314               to llcc
               move "*" to name-value-error
               display ring-bell
               go to edit-name-value-exit
            end-if
            if fd-last-name not myalpha
               move "name must contain only alpha characters"
                 to msg-line
               set screen-error-exists to true
               move 0314               to llcc
               move "*" to name-value-error
               display ring-bell
            end-if.
        edit-name-value-exit.
            exit.
      
        edit-id-field.
            if fd-phone not numeric
               move "phone must be numeric" to msg-line
               set screen-error-exists to true
               move 0114               to llcc
               move "*" to id-num-error
               display ring-bell
               go to edit-id-field-exit
            end-if
            if fd-phone < 1
               move "phone must be greater than zero" to msg-line
               set screen-error-exists to true
               move 0114               to llcc
               move "*" to id-num-error
               display ring-bell
            end-if.
        edit-id-field-exit.
            exit.
      
        end program address.

