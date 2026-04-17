       Identification Division.
       Program-Id.                                 cliins01           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    cnv                 *
      *                        Area gestionale:    cnv                 *
      *                                Settore:                        *
      *                                   Fase:    cliins              *
      *                    ------------------------------------------- *
      *                       Versione attuale:    001 del 01/01/00    *
      *                       Ultima revisione:    NdK del 06/04/00    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Inserimento clienti da Internet             *
      *                                                                *
      *================================================================*

      ******************************************************************
       Environment Division.
      ******************************************************************

      *================================================================*
       Configuration Section.
      *================================================================*

       Source-Computer.     d-K-b-Snc-PD .
       Object-Computer.     d-K-b-Snc-PD .

       Special-Names.       Decimal-Point is comma .

      *================================================================*
       Input-Output Section.
      *================================================================*
      
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
                             
      ******************************************************************
       Data Division.
      ******************************************************************

      *================================================================*
       File Section.
      *================================================================*
       
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

      *================================================================*
       Working-Storage Section.
      *================================================================*

      *    *===========================================================*
      *    * File area generica                                        *
      *    *-----------------------------------------------------------*
       01  f-fil.
           05  f-fil-nam                  pic  x(04)                  .
           05  f-fil-pat                  pic  x(40)                  .
           05  f-fil-sts                  pic  x(02)                  .
           
      *    *===========================================================*
      *    * Area di work                                              *
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

      ******************************************************************
       Procedure Division.
      ******************************************************************

      *================================================================*
      *       Main                                                     *
      *================================================================*
       main-000.
      *              *-------------------------------------------------*
      *              * Normalizzazioni preliminari                     *
      *              *-------------------------------------------------*
           move      zero                 to   contax                 .
           move      spaces               to   parametro1             .
           move      spaces               to   parametro2             .
           move      spaces               to   parametro3             .
      *              *-------------------------------------------------*
      *              * Lettura parametri in input                      *
      *              *-------------------------------------------------*
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
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * [cli]                                       *
      *                  *---------------------------------------------*
           move      "/abd/azi/prv/cli"
                                          to   f-fil-pat              .
           open      i-o    fil                                       .
      *              *-------------------------------------------------*
      *              * Normalizzazione campi [cli]                     *
      *              *-------------------------------------------------*
           move      zero                 to   conta                  .
       main-100.
           move      9990001              to   fil-cod-cli            .
           move      "IT "                to   fil-cod-naz            .
           move      zero                 to   fil-cod-cmn            .
           move      zero                 to   fil-cod-fzn            .
           move      zero                 to   fil-cod-lct            .
           move      zero                 to   fil-prt-iva            .
      *              *-------------------------------------------------*
      *              * Preparazione ragione sociale                    *
      *              *-------------------------------------------------*
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
      *              *-------------------------------------------------*
      *              * Scrittura [cli]                                 *
      *              *-------------------------------------------------*
           write     fil-rec invalid key
                            go to main-900.
      *                  *---------------------------------------------*
      *                  * Oltre                                       *
      *                  *---------------------------------------------*
           go to     main-200.
       main-200.
      *              *-------------------------------------------------*
      *              * Preparazione pagina 'html'                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * 'head'                                      *
      *                  *---------------------------------------------*
           display "Content-type: text/html" with no advancing.
           display "".
           display "<head>".
           display "<title>Nicola de Kunovich</title>".
           display "</head>".
      *                  *---------------------------------------------*
      *                  * 'body' - inizio                             *
      *                  *---------------------------------------------*
           display "<body>".
           display "<A NAME=inizio>".
           display "<h2><center>N de K : Lista clienti</center></h2>".
       
           display "<a HREF=http://192.42.172.2/cli-ins.html><img src=/i
      -            "cons/back.gif></a>"                              .
      *                      *-----------------------------------------*
      *                      * Ragione sociale                         *
      *                      *-----------------------------------------*
           move spaces to stringa.
           string "<h3>" delimited by size
                  parametro1
                         delimited by size
                  "</h3>"
                         delimited by size
                                   into stringa                       .
           display   stringa                                          .
      *                      *-----------------------------------------*
      *                      * Indirizzo                               *
      *                      *-----------------------------------------*
           move spaces to stringa.
           string "<h3>" delimited by size
                  parametro2
                         delimited by size
                  "</h3>"
                         delimited by size
                                   into stringa                       .
           display   stringa                                          .
      *                      *-----------------------------------------*
      *                      * Localita'                               *
      *                      *-----------------------------------------*
           move spaces to stringa.
           string "<h3>" delimited by size
                  parametro3
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
       main-900.
      *              *-------------------------------------------------*
      *              * Operazioni di chiusura                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ritorno a inizio pagina                     *
      *                  *---------------------------------------------*
           display "<a HREF=#inizio><img src=/icons/up.gif></a>"      .
      *                  *---------------------------------------------*
      *                  * 'body' - fine                               *
      *                  *---------------------------------------------*
           display "</body>".
      *                  *---------------------------------------------*
      *                  * [cli]                                       *
      *                  *---------------------------------------------*
           close     fil                                              .
       main-999.
      *              *-------------------------------------------------*
      *              * Fine programma                                  *
      *              *-------------------------------------------------*
           exit      program.
           
      *================================================================*
           stop run.
      ******************************************************************

