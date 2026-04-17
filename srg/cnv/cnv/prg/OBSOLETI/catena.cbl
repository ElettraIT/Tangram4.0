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
      *    * File Control [fil]                                        *
      *    *-----------------------------------------------------------*
           select  optional  fil   assign  to disk     w-fil-pat
                             organization  is relative
                             access   mode is dynamic
                             relative key  is          w-rcn-krn
                             file status   is          w-fil-sts      .

       Data Division.
       File Section.
       
      *    *===========================================================*
      *    * File Description [fil]                                    *
      *    *-----------------------------------------------------------*
       fd  fil           label record standard                        .
      *    *-----------------------------------------------------------*
      *    * Record                                                    *
      *    *-----------------------------------------------------------*
       01  fil-rec.
      *        *-------------------------------------------------------*
      *        * Area dati in grado di ospitare l'area w-rig del pro-  *
      *        * gramma chiamante                                      *
      *        *-------------------------------------------------------*
           05  fil-dat.
               10  filler occurs 768      pic  x(01)                  .


       Working-Storage Section.
      *================================================================*
       Working-Storage Section.
      *================================================================*

      *    *===========================================================*
      *    * File area per [fil]                                       *
      *    *-----------------------------------------------------------*
       01  w-fil.
      *        *-------------------------------------------------------*
      *        * File name                                             *
      *        *-------------------------------------------------------*
           05  w-fil-nam                  pic  x(04) value "tmp "     .
      *        *-------------------------------------------------------*
      *        * File pathname                                         *
      *        *-------------------------------------------------------*
           05  w-fil-pat                  pic  x(40) value spaces     .
      *        *-------------------------------------------------------*
      *        * File status                                           *
      *        *-------------------------------------------------------*
           05  w-fil-sts                  pic  x(02) value "00"       .

      *    *===========================================================*
      *    * Work-area per key-record number                           *
      *    *-----------------------------------------------------------*
       01  w-rcn.
      *        *-------------------------------------------------------*
      *        * File key record number                                *
      *        *-------------------------------------------------------*
           05  w-rcn-krn                  pic  9(05)                  .

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

       Procedure Division             chaining parametro              .

       Main Section.
       main-000.



           move      zero                 to   contax                 .
           move      spaces               to   parametro1             .
           move      spaces               to   parametro2             .
           unstring  parametro
                                delimited by "&"
                                          into parametro1
                                    count in   contax                 .
           add       2                    to   contax                 .
           unstring  parametro            into parametro2
                                  with pointer contax                 .





           move      "/abd/azi/prv/cli"
                                          to   f-fil-pat              .
           open      i-o    fil                                       .
           
           move      zero                 to   conta                  .
           if        parametro1  =    "ordinamento=descrizione       "
                     go to main-150
           else      go to main-100.
           
           
       main-100.
           move      zero                 to   fil-cod-cli            .
           start     fil    key not less
                            fil-k01
                            invalid key
                            go to main-900.
           go to     main-200.
       main-150.
           move      spaces               to   fil-rag-key            .
           move      zero                 to   fil-cod-cli-3          .
           start     fil    key not less
                            fil-k03
                            invalid key
                            go to main-900.
           go to     main-200.
       main-200.
           add       1                    to   conta                  .
       main-202.
           read      fil    next
                            with no lock
                            at end
                            go to main-900.
           if        parametro2 not  = "stato=estero                  "
                     go to main-205.
           if        fil-cod-naz = "IT "
                     go to main-202.
       main-205.
           if        conta = 1
                     go to main-210
           else      go to main-220.
       main-210.
           display "Content-type: text/html" with no advancing.
           display "".
           display "<head>".
           display "<title>TANGRAM</title>".
           display "</head>".
           display "<body>".
           display "<A NAME=inizio>".
           display "<h2><center>N de K : Lista clienti</center></h2>".
       
           display "<a HREF=http://192.42.172.2><img src=/icons/back.gif
      -              "></a>"                        .
           
           move spaces to stringa.
           string "<h3>" delimited by size
                  parametro1
                         delimited by size
                  "</h3>"
                         delimited by size
                                   into stringa                       .
           display   stringa                                          .
           
           move spaces to stringa.
           string "<h3>" delimited by size
                  parametro2
                         delimited by size
                  "</h3>"
                         delimited by size
                                   into stringa                       .
           display   stringa                                          .
            
       main-220.
           move fil-cod-cli to cod-cli.
           move spaces to stringa.
           string "<h4>" delimited by size
                  cod-cli
                         delimited by size
                  " - "
                         delimited by size
                  fil-rag-soc
                         delimited by size
                  " - "
                         delimited by size
                  fil-loc-cli
                         delimited by size
                  " - "
                         delimited by size
                  conta
                         delimited by size
                  "</h4>"
                         delimited by size
                                   into stringa                       .
           display   stringa                                          .
           go to main-200.
       main-900.
           display "<a HREF=#inizio><img src=/icons/up.gif></a>"      .
           display "</body>".
           close     fil                                              .
       main-999.
           exit      program.
           stop run.

