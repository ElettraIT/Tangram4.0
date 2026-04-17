       Identification Division.
       Program-Id.                                 cobol01            .
       Environment Division.
       Configuration Section.
       Source-Computer.     N-d-K-Sia-PD .
       Object-Computer.     N-d-K-Sia-PD .
       Special-Names.       Decimal-Point is comma .
       Input-Output Section.
       File-Control.
      *    *===========================================================*
      *    * File Control [bin]                                        *
      *    *-----------------------------------------------------------*
           select bin        assign       to input-output   f-bin-pat
                             organization is binary sequential
                             access  mode is sequential
                             file  status is                f-bin-sts .

       Data Division.
       File Section.
       
      *    *===========================================================*
      *    * File Description [bin]                                    *
      *    *-----------------------------------------------------------*
       fd  bin  label record omitted.
      *    *-----------------------------------------------------------*
      *    * Record                                                    *
      *    *-----------------------------------------------------------*
       01  bin-rec.
      *        *-------------------------------------------------------*
      *        * Caratteri componenti il record, 8192 caratteri        *
      *        *-------------------------------------------------------*
           05  bin-chr       occurs 8192  pic  x(01)                  .


       Working-Storage Section.
      *    *===========================================================*
      *    * Area ausiliaria per controllo i-o su [bin]                *
      *    *-----------------------------------------------------------*
       01  f-bin.
      *        *-------------------------------------------------------*
      *        * File name                                             *
      *        *-------------------------------------------------------*
           05  f-bin-nam                  pic  x(04) value "stp "     .
      *        *-------------------------------------------------------*
      *        * File pathname                                         *
      *        *-------------------------------------------------------*
           05  f-bin-pat                  pic  x(40) value spaces     .
      *        *-------------------------------------------------------*
      *        * File status                                           *
      *        *-------------------------------------------------------*
           05  f-bin-sts                  pic  x(02) value "00"       .

      *    *===========================================================*
      *    * File area per [kk0]                                       *
      *    *-----------------------------------------------------------*
       01  comodo.
           05  stringa                    pic  x(120)                 .
           05  conta                      pic  9(07)                  .
           05  contax                     pic  9(07)                  .
           05  cod-cli                    pic  9(07)                  .
           05  parametro1                 pic  x(30)                  .
           05  parametro2                 pic  x(30)                  .
           
           
       01  parametro                      pic  x(60)                  .

       Procedure Division                                             .

       Main Section.
       main-000.
           move      "/abd/asc/prv_postscript/hello.pdf"
                                          to   f-bin-pat              .
           open      input   bin                                      .
       main-100.
           move      zero                 to   conta                  .
       main-200.
           add       1                    to   conta                  .
       main-202.
           read      bin    next
                            at end
                            go to main-900.
       main-210.
           go to     main-200.
       main-900.
           display " " line 01 position 01 erase.
           display conta line 02 position 01.
           close     bin                                              .
       main-999.
           exit      program.
           stop run.

