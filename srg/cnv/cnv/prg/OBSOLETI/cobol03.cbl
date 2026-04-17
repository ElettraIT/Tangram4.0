       Identification Division.
       Program-Id.                                 cobol01            .
       Environment Division.
       Configuration Section.
       Source-Computer.     N-d-K-Sia-PD .
       Object-Computer.     N-d-K-Sia-PD .
       Special-Names.       Decimal-Point is comma .
       Input-Output Section.
       File-Control.
           select  optional  kk0   assign to disk           f-kk0-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is kk0-key
                             file status  is                f-kk0-sts .
       Data Division.
       File Section.
       
      *    *===========================================================*
      *    * File Description [kk0]                                    *
      *    *-----------------------------------------------------------*
       fd  kk0       label record standard                            .

      *    *===========================================================*
      *    * Record file di appoggio [kk0]                             *
      *    *-----------------------------------------------------------*
       01  kk0-rec.
      *        *-------------------------------------------------------*
      *        * Chiave di ordinamento                                 *
      *        *-------------------------------------------------------*
           05  kk0-key.
      *            *---------------------------------------------------*
      *            * Chiave numerica                                   *
      *            *---------------------------------------------------*
               10  kk0-num-key            pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  kk0-dat.
      *            *---------------------------------------------------*
      *            * Record trattato                                   *
      *            *---------------------------------------------------*
               10  kk0-fil-rec.
                   15  filler occurs 40   pic  x(01)                  .

       Working-Storage Section.
      *    *===========================================================*
      *    * File area per [kk0]                                       *
      *    *-----------------------------------------------------------*
       01  f-kk0.
           05  f-kk0-nam                  pic  x(04)                  .
           05  f-kk0-pat                  pic  x(40)                  .
           05  f-kk0-sts                  pic  x(02)                  .
      *    *===========================================================*
      *    * File area per [kk0]                                       *
      *    *-----------------------------------------------------------*
       01  conta.
           05  contatore                  pic  9(02)                  .

       Procedure Division                                             .

       Main Section.
       main-000.
       
           move      zero              to contatore                   .
           move      "/abd/asc/fileprvcgi"
                                          to   f-kk0-pat              .
           open      i-o    kk0                                       .

      *
           add       1                    to contatore                .
           move      contatore            to   kk0-num-key            .
           move      "Content-type: text/html"
                                          to   kk0-fil-rec            .
           write     kk0-rec                                          .
      *
           add       1                    to contatore                .
           move      contatore            to   kk0-num-key            .
           move      spaces               to   kk0-fil-rec            .
           write     kk0-rec                                          .
      *
           add       1                    to contatore                .
           move      contatore            to   kk0-num-key            .
           move      "<head>"             to   kk0-fil-rec            .
           write     kk0-rec                                          .
      *
           add       1                    to contatore                .
           move      contatore            to   kk0-num-key            .
           move      "<title>Ciao cobol</title>"
                                          to   kk0-fil-rec            .
           write     kk0-rec                                          .
      *
           add       1                    to contatore                .
           move      contatore            to   kk0-num-key            .
           move      "</head>"            to   kk0-fil-rec            .
           write     kk0-rec                                          .
      *
           add       1                    to contatore                .
           move      contatore            to   kk0-num-key            .
           move      "<body>"             to   kk0-fil-rec            .
           write     kk0-rec                                          .
      *
           add       1                    to contatore                .
           move      contatore            to   kk0-num-key            .
           move      "<h1>Ciao a tutti dal Cobol</h1>"
                                          to   kk0-fil-rec            .
           write     kk0-rec                                          .
      *
           add       1                    to contatore                .
           move      contatore            to   kk0-num-key            .
           move      "</body>"            to   kk0-fil-rec            .
           write     kk0-rec                                          .
      *
           move      zero                 to   kk0-num-key            .
           start     kk0    key not less
                            kk0-key
                            invalid key
                            go to main-900.
       main-200.
           read      kk0    next
                            with no lock
                            at end
                            go to main-900.
           if        kk0-num-key = 01
                     go to main-210
           else      go to main-220.
       main-210.
           display   kk0-fil-rec   with no advancing                  .
           go to main-200.
       main-220.
           display   kk0-fil-rec                                      .
           go to main-200.
       main-900.
           close     kk0                                              .
       main-999.
           exit      program.
           stop run.

