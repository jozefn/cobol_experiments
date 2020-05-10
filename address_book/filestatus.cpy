
       01 filestatus               pic x(02).
          88 fs-success            value '00'.
          88 fs-sucess-duplicate   value '02'.
          88 fs-sucess-incomplete  value '04'.
          88 fs-sucess-optional    value '05'.
          88 fs-sucess-no-unit     value '07'.
          88 fs-eof                value '10'.
          88 fs-out-of-range       value '14'.
          88 fs-key-invalid        value '21'.
          88 fs-key-exist          value '22'.
          88 fs-key-not-exist      value '23'.
          88 fs-permanent-error    value '30'.
          88 fs-inconsistent-fname value '31'.
          88 fs-boundry-violation  value '34'.
          88 fs-no-file            value '35'.
          88 fs-permission-denied  value '37'.
          88 fs-closed-with-lock   value '38'.
          88 fs-conflict-attribute value '39'.
          88 fs-already-opened     value '41'.
          88 fs-not-opened         value '42'.
          88 fs-read-not-done      value '43'.
          88 fs-record-overflow    value '44'.
          88 fs-read-error         value '46'.
          88 fs-input-denied       value '47'.
          88 fs-output-denied      value '48'.
          88 fs-io-denied          value '49'.
          88 fs-record-locked      value '51'.
          88 fs-end-of-page        value '52'.

      *EVALUATE STATUS
      * WHEN 00 MOVE 'SUCCESS ' TO MSG
      * WHEN 02 MOVE 'SUCCESS DUPLICATE ' TO MSG
      * WHEN 04 MOVE 'SUCCESS INCOMPLETE ' TO MSG
      * WHEN 05 MOVE 'SUCCESS OPTIONAL ' TO MSG
      * WHEN 07 MOVE 'SUCCESS NO UNIT ' TO MSG
      * WHEN 10 MOVE 'END OF FILE ' TO MSG
      * WHEN 14 MOVE 'OUT OF KEY RANGE ' TO MSG
      * WHEN 21 MOVE 'KEY INVALID ' TO MSG
      * WHEN 22 MOVE 'KEY EXISTS ' TO MSG
      * WHEN 23 MOVE 'KEY NOT EXISTS ' TO MSG
      * WHEN 30 MOVE 'PERMANENT ERROR ' TO MSG
      * WHEN 31 MOVE 'INCONSISTENT FILENAME ' TO MSG
      * WHEN 34 MOVE 'BOUNDARY VIOLATION ' TO MSG
      * WHEN 35 MOVE 'FILE NOT FOUND ' TO MSG
      * WHEN 37 MOVE 'PERMISSION DENIED ' TO MSG
      * WHEN 38 MOVE 'CLOSED WITH LOCK ' TO MSG
      * WHEN 39 MOVE 'CONFLICT ATTRIBUTE ' TO MSG
      * WHEN 41 MOVE 'ALREADY OPEN ' TO MSG
      * WHEN 42 MOVE 'NOT OPEN ' TO MSG
      * WHEN 43 MOVE 'READ NOT DONE ' TO MSG
      * WHEN 44 MOVE 'RECORD OVERFLOW ' TO MSG
      * WHEN 46 MOVE 'READ ERROR ' TO MSG
      * WHEN 47 MOVE 'INPUT DENIED ' TO MSG
      * WHEN 48 MOVE 'OUTPUT DENIED ' TO MSG
      * WHEN 49 MOVE 'I/O DENIED ' TO MSG
      * WHEN 51 MOVE 'RECORD LOCKED ' TO MSG
      * WHEN 52 MOVE 'END-OF-PAGE ' TO MSG
      * WHEN 57 MOVE 'I/O LINAGE ' TO MSG
      * WHEN 61 MOVE 'FILE SHARING FAILURE ' TO MSG
      * WHEN 91 MOVE 'FILE NOT AVAILABLE ' TO MSG
      *END-EVALUATE.
