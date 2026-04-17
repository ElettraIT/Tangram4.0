       Identification Division.
       Program-Id.                                 provacli           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    cnv                 *
      *                        Area gestionale:    cnv                 *
      *                                Settore:                        *
      *                                   Fase:    cliins              *
      *                    ------------------------------------------- *
      *                       Versione attuale:    001 del 01/01/00    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Inserimento clienti da shell                *
      *                                                                *
      *================================================================*

      ******************************************************************
       Environment Division.
      ******************************************************************

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
           05  stringa                    pic  x(200)                 .
           05  conta                      pic  9(07)                  .
           05  contax                     pic  9(07)                  .
           05  cod-cli                    pic  9(07)                  .
           05  parametro1                 pic  x(200)                 .
           05  parametro2                 pic  x(200)                 .
           05  parametro3                 pic  x(200)                 .
           
           
       01  parametro                      pic  x(200)                 .

       Procedure Division             chaining parametro              .

       Main Section.
      *    *===========================================================*
      *    * Main                                                      *
      *    *-----------------------------------------------------------*
       main-000.
           move      zero                 to   contax                 .
           move      spaces               to   parametro1             .
           move      spaces               to   parametro2             .
           move      spaces               to   parametro3             .
           unstring  parametro
                                delimited by "&"
                                          into parametro1
                                    count in   contax                 .
           add       2                    to   contax                 .
           unstring  parametro            into parametro2
                                  with pointer contax                 .

           move      zero                 to   contax                 .
           unstring  parametro2
                                delimited by "&"
                                          into parametro2
                                    count in   contax                 .
           add       2                    to   contax                 .
           unstring  parametro2           into parametro3
                                  with pointer contax                 .



           move      "/abd/azi/prv/cli"
                                          to   f-fil-pat              .
           open      i-o    fil                                       .
           move      zero                 to   conta                  .
       main-100.
           move      9990001              to   fil-cod-cli            .
           move      "IT "                to   fil-cod-naz            .
           move      04311                to   fil-cod-cmn            .
           move      zero                 to   fil-cod-fzn            .
           move      zero                 to   fil-cod-lct            .
           move      11331133113          to   fil-prt-iva            .
      *
           move      zero                 to   contax                 .
           move      spaces               to   fil-rag-soc            .
           unstring  parametro1
                                delimited by "="
                                          into fil-rag-soc
                                    count in   contax                 .
           add       2                    to   contax                 .
           move      spaces               to   fil-rag-soc            .
           unstring  parametro1           into fil-rag-soc
                                  with pointer contax                 .
      *
           move      zero                 to   contax                 .
           move      spaces               to   fil-via-cli            .
           unstring  parametro2
                                delimited by "="
                                          into fil-via-cli
                                    count in   contax                 .
           add       2                    to   contax                 .
           move      spaces               to   fil-via-cli            .
           unstring  parametro2           into fil-via-cli
                                  with pointer contax                 .
      *
           move      zero                 to   contax                 .
           move      spaces               to   fil-loc-cli            .
           unstring  parametro3
                                delimited by "="
                                          into fil-loc-cli
                                    count in   contax                 .
           add       2                    to   contax                 .
           move      spaces               to   fil-loc-cli            .
           unstring  parametro3           into fil-loc-cli
                                  with pointer contax                 .
           

           rewrite   fil-rec invalid key
                            go to main-900.
           go to     main-200.
       main-200.
       main-900.
           close     fil                                              .
       main-999.
           exit      program.
           stop run.

