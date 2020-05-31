       identification division.
       program-id. address.
      
       environment division.
        configuration section.
        special-names.
            class myalpha  is "a" thru "z", " ", "A" thru "Z"
            cursor         is      llcc.
        input-output section.
        file-control.
            select optional     indexed-file
                   assign       to  outfile
                   organization is  indexed
                   record key   is  ph-number
                   alternate record key is name-value  with duplicates
                   file status  is  file-stat
                   lock mode    is manual with lock on multiple records
                   access mode  is  dynamic.
      
       data division.
        file section.
        fd   indexed-file.
        01   indexed-record.
             02  ph-number             pic 9(4).
             02  name-value            pic x(40).
      
        working-storage section.
      
        01   wk-record.
             02  wk-ph-number          pic 9(4).
             02  wk-name-value         pic x(40).
      
      
        copy screenio.
        01  llcc                       pic 9(04).
        01  outfile                    pic x(50)
            value "/mnt/c/cobfiles\sample4jout.idx".
        01  file-stat                  pic x(02).
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

        01 main-screen background-color is cob-color-white
                       foreground-color is cob-color-black.
           05                 line 01
                              column 01 value "ph-number:" highlight.
           05 ph-number-error line 01 column 12
              foreground-color is cob-color-red
              pic x(1) from id-num-error.
           05 ph-number-area  line 01 column 14
              foreground-color is cob-color-blue
              pic 9(4)
              from ph-number to ph-number auto full underline.
           05                 line 03 column 01
              value "name:     "  highlight.
           05 name-error      line 03 column 12
              foreground-color is cob-color-red
              pic x(1)
              from name-value-error.
           05 name-area       line 03 column 14
              foreground-color is cob-color-blue
              pic x(40)
              from name-value to name-value auto underline.
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
            perform read-next-record
            perform screen-loop thru screen-loop-exit.
      
        stop-prg.
            close indexed-file.
            stop run.
      
        open-file.
            open i-o indexed-file
            if file-stat = '00' or '05'
               continue
            else
               display 'cannot open file ' file-stat
               stop run
            end-if.
      
        initialize-variables.
            move zeros  to ph-number
            move spaces to name-value name-value-error
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
                  perform read-record-by-key
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
      
        upper-case-fields.
            move function upper-case(name-value) to name-value.
      
        write-record.
            move indexed-record to wk-record
            read indexed-file with lock key is ph-number  *> lock record before updati
            move wk-record to indexed-record
      
            write indexed-record
            if file-stat = '00'
               move "record added" to msg-line
            else
               if file-stat = '22'
                  rewrite indexed-record
                  if file-stat = '00' or '02' *> 02 handles dup alternate key
                     move "record updated" to msg-line
                  else
                     string "cannot update record."
                            " file-status = "
                            file-stat delimited by size
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
            move spaces to name-value
            read indexed-file key is ph-number
              invalid key
                move "key not found"          to msg-line
                display ring-bell
              not invalid key
                move "record retrieved"       to msg-line
            end-read
            perform record-lock-check.
      
        delete-record-by-key.
            perform reset-screen-ind
            move spaces to name-value
            delete indexed-file record
              invalid key
                move "key not found"          to msg-line
                display ring-bell
              not invalid key
                move "record deleted"         to msg-line
                move spaces to name-value
            end-delete.
            perform record-lock-check.
      
        read-next-record.
            perform reset-screen-ind
            start indexed-file key > ph-number
              invalid key
                move "end of file"            to msg-line
                display ring-bell
              not invalid key
                read indexed-file next
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
            perform upper-case-fields
            start indexed-file key > name-value
              invalid key
                move "end of file"            to msg-line
                display ring-bell
              not invalid key
                read indexed-file next
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
            start indexed-file key < ph-number
              invalid key
                move "beginning of file"      to msg-line
                display ring-bell
              not invalid key
                read indexed-file previous
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
            perform upper-case-fields
            start indexed-file key < name-value
              invalid key
                move "beginning of file"      to msg-line
                display ring-bell
              not invalid key
                read indexed-file previous
                  at end
                     move "beginning of file" to msg-line
                     display ring-bell
                  not at end
                     move "record retrieved"  to msg-line
                end-read
                perform record-lock-check
            end-start.
      
        record-lock-check.
           evaluate file-stat
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
            perform reset-screen-ind
            perform upper-case-fields.
      
        edit-name-value.
            if name-value = spaces
               move "name cannot be blank" to msg-line
               set screen-error-exists to true
               move 0314               to llcc
               move "*" to name-value-error
               display ring-bell
               go to edit-name-value-exit
            end-if
            if name-value not myalpha
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
            if ph-number not numeric
               move "ph-number must be numeric" to msg-line
               set screen-error-exists to true
               move 0114               to llcc
               move "*" to id-num-error
               display ring-bell
               go to edit-id-field-exit
            end-if
            if ph-number < 1
               move "ph-number must be greater than zero" to msg-line
               set screen-error-exists to true
               move 0114               to llcc
               move "*" to id-num-error
               display ring-bell
            end-if.
        edit-id-field-exit.
            exit.
      
        end program sample4j.

