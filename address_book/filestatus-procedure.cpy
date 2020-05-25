            evaluate filestatus
                when 00 move 'success ' to msg
                when 02 move 'success duplicate ' to msg
                when 04 move 'success incomplete ' to msg
                when 05 move 'success optional ' to msg
                when 07 move 'success no unit ' to msg
                when 10 move 'end of file ' to msg
                when 14 move 'out of key range ' to msg
                when 21 move 'key invalid ' to msg
                when 22 move 'key exists ' to msg
                when 23 move 'key not exists ' to msg
                when 30 move 'permanent error ' to msg
                when 31 move 'inconsistent filename ' to msg
                when 34 move 'boundary violation ' to msg
                when 35 move 'file not found ' to msg
                when 37 move 'permission denied ' to msg
                when 38 move 'closed with lock ' to msg
                when 39 move 'conflict attribute ' to msg
                when 41 move 'already open ' to msg
                when 42 move 'not open ' to msg
                when 43 move 'read not done ' to msg
                when 44 move 'record overflow ' to msg
                when 46 move 'read error ' to msg
                when 47 move 'input denied ' to msg
                when 48 move 'output denied ' to msg
                when 49 move 'i/o denied ' to msg
                when 51 move 'record locked ' to msg
                when 52 move 'end-of-page ' to msg
                when 57 move 'i/o linage ' to msg
                when 61 move 'file sharing failure ' to msg
                when 91 move 'file not available ' to msg
            end-evaluate.

