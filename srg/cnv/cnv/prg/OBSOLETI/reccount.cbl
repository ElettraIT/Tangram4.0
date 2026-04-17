       Identification Division.
       Program-Id.                                 reccount           .
       Environment Division.
       Configuration Section.
       Data Division.
      *================================================================*
       Working-Storage Section.
      *================================================================*
       77  io_function                         pic 99 comp-x          .
           88  open_function                   value 1                .
           88  close_function                  value 2                .
           88  info_function                   value 4                .
       77  f_errno                             pic s9(4) comp-5
                                               external               .
       01  open_mode                           pic s9(4) comp-5       .
       01  info_mode                           pic s9(4) comp-5       .
           88  get_record_count                value -4               .
       01  logical_info                        pic x(17)              .
       01  file_name                           pic x(80)              .
       01  file_handle                         usage pointer, sync    .
       01  record_count                        pic 9(10)              .
       
      ******************************************************************
       Procedure Division.
      ******************************************************************

      *================================================================*
      *       Main                                                     *
      *================================================================*
       MAIN-LOGIC.
           display window erase.
           accept file_name line 02 position 01.
           set open_function to true.
           move 0 to open_mode.
           
           call "I$IO" using
                            io_function
                            file_name
                            open_mode
                            logical_info
                            file_handle.
           
           if f_errno not = 0
              display "Open Error " at 1010 f_errno convert
              stop run
           end-if.
           set info_function              to true.
           set get_record_count           to true.
           call "I$IO" using io_function file_handle info_mode
                                          record_count.
           display "Record count: " at 1010 record_count convert
           set close_function to true.
           call "I$IO" using io_function file_handle.
