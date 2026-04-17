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
           select  optional  fil   assign to disk           f-fil-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is fil-k01
                   alternate record key   is fil-k02
                   alternate record key   is fil-k03
                   alternate record key   is fil-k04
                   alternate record key   is fil-k05
                   alternate record key   is fil-k06
                             file status  is                f-fil-sts .
       Data Division.
       File Section.
       
      *    *===========================================================*
      *    * File Description [fil]                                    *
      *    *-----------------------------------------------------------*
       fd  fil       label record standard                            .

      *    *===========================================================*
      *    * Record fisico                                             *
      *    *-----------------------------------------------------------*
       01  fil-rec.
      *        *-------------------------------------------------------*
      *        * Chiavi                                                *
      *        *-------------------------------------------------------*
           05  fil-key.
      *            *---------------------------------------------------*
      *            * Chiave numero 01 : CODCLI                         *
      *            *---------------------------------------------------*
               10  fil-k01.
                   15  fil-cod-cli        pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DATSYS                         *
      *            *---------------------------------------------------*
               10  fil-k02.
                   15  fil-ide-dat        pic  9(07)       comp-3     .
                   15  fil-cod-cli-2      pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : RAGKEY                         *
      *            *---------------------------------------------------*
               10  fil-k03.
                   15  fil-rag-key        pic  x(40)                  .
                   15  fil-cod-cli-3      pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 04 : CODMNE                         *
      *            *---------------------------------------------------*
               10  fil-k04.
                   15  fil-cod-mne        pic  x(10)                  .
                   15  fil-cod-cli-4      pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 05 : PRTIVA                         *
      *            *---------------------------------------------------*
               10  fil-k05.
                   15  fil-prt-iva        pic  9(11)       comp-3     .
                   15  fil-cod-cli-5      pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 06 : CODFIS                         *
      *            *---------------------------------------------------*
               10  fil-k06.
                   15  fil-cod-fis        pic  x(16)                  .
                   15  fil-cod-cli-6      pic  9(07)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  fil-dat.
               10  fil-ide-ute            pic  x(08)                  .
               10  fil-ide-fas            pic  x(06)                  .
               10  fil-rag-soc            pic  x(40)                  .
               10  fil-via-cli            pic  x(40)                  .
               10  fil-loc-cli            pic  x(40)                  .
               10  fil-cod-naz            pic  x(03)                  .
               10  fil-cod-cmn            pic  9(05)       comp-3     .
               10  fil-cod-fzn            pic  9(03)       comp-3     .
               10  fil-cod-lct            pic  9(03)       comp-3     .
               10  fil-num-tel            pic  x(20)                  .
               10  fil-num-fax            pic  x(20)                  .
               10  fil-num-tlx            pic  x(20)                  .
               10  fil-nom-int            pic  x(30)                  .
               10  fil-cod-iva            pic  9(05)       comp-3     .
               10  fil-snx-a13            pic  x(01)                  .
               10  fil-cod-cge            pic  9(07)       comp-3     .
               10  fil-alx-exp.
                   15  filler  occurs 80  pic  x(01)                  .


       Working-Storage Section.
      *    *===========================================================*
      *    * File area per [kk0]                                       *
      *    *-----------------------------------------------------------*
       01  f-fil.
           05  f-fil-nam                  pic  x(04)                  .
           05  f-fil-pat                  pic  x(40)                  .
           05  f-fil-sts                  pic  x(02)                  .
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

