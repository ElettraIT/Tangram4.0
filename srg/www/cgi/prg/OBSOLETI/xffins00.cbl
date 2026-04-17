       Identification Division.
       Program-Id.                                 xffins01           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    cnv                 *
      *                        Area gestionale:    cnv                 *
      *                                Settore:                        *
      *                                   Fase:    xffins              *
      *                    ------------------------------------------- *
      *                       Versione attuale:    001 del 18/02/19    *
      *                       Ultima revisione:    NdK del 18/02/19    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Inserimento dati fatture fornitori XML      *
      *                                                                *
      * ___ DA IMPLEMENTARE ___                                        *
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

      ******************************************************************
       Data Division.
      ******************************************************************

      *================================================================*
       Working-Storage Section.
      *================================================================*

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "msegrt"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/s"                                  .

      *    *===========================================================*
      *    * Record files                                              *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [xff]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/scf/fls/rec/rfxff"                          .

      *    *===========================================================*
      *    * File area generica                                        *
      *    *-----------------------------------------------------------*
       01  w-inp.
           05  w-inp-rec                  pic  x(99)                  .
           05  w-inp-tip                  pic  x(01)                  .
           05  w-inp-ide                  pic  x(40)                  .
           05  w-inp-val                  pic  x(40)                  .
           
      *    *===========================================================*
      *    * Work-area per allineamenti a destra o a sinistra oppure   *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cpw"                   .

      ******************************************************************
       Procedure Division            chaining  w-inp-rec              .
      ******************************************************************

      *================================================================*
      *       Main                                                     *
      *================================================================*
       main-000.
      *              *-------------------------------------------------*
      *              * Estrazione parametri in input                   *
      *              *-------------------------------------------------*
           move      w-inp-rec            to   w-all-str-alf          .
           move      "#"                  to   w-all-str-del          .
           perform   all-str-ext-000      thru all-str-ext-999        .
           move      w-all-str-cat (1)    to   w-inp-tip              .
           move      w-all-str-cat (2)    to   w-inp-ide              .
           move      w-all-str-cat (3)    to   w-inp-val              .
           
           
           
           
      *              *-------------------------------------------------*
      *              * [sff]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/scf/fls/ioc/obj/iofsff"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-sff                 .
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
      *              * Normalizzazione record                          *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/scf/fls/ioc/obj/iofsff"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-sff                 .
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           move      "IG"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-dat                to   rf-sff-ide-dat         .
           move      s-ute                to   rf-sff-ide-ute         .
           move      s-fas                to   rf-sff-ide-fas         .
           move      w-tes-dat-reg        to   rf-sff-dtr-ftf         .
           move      w-tes-num-ftf        to   rf-sff-num-ftf         .
           move      w-tes-tip-ftf (1)    to   rf-sff-tip-ftf         .
       main-900.
      *              *-------------------------------------------------*
      *              * [sff]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/scf/fls/ioc/obj/iofsff"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-sff                 .
       main-999.
      *              *-------------------------------------------------*
      *              * Fine programma                                  *
      *              *-------------------------------------------------*
           exit      program.
           
      *================================================================*
           stop run.
      ******************************************************************

      *    *===========================================================*
      *    * Subroutines per allineamenti a destra o a sinistra oppure *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cps"                   .


