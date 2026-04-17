       Identification Division.
       Program-Id.                                 pcge3010           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    cge                 *
      *                                Settore:    mov                 *
      *                                   Fase:    cge301              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 29/12/89    *
      *                       Ultima revisione:    NdK del 29/10/99    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Richieste per interrogazioni di contabilita'*
      *                                                                *
      *                    Il programma raccoglie le richieste per i   *
      *                    seguenti programmi di interrogazione:       *
      *                                                                *
      *                    Interrogazioni per conto:                   *
      *                                                                *
      *                    - pcge3011 : Lista movimenti di generale    *
      *                    - pcge3012 : Lista movimenti clienti        *
      *                    - pcge3013 : Lista movimenti fornitori      *
      *                    - pcge3014 : Partitario di generale         *
      *                    - pcge3015 : Partitario clienti             *
      *                    - pcge3016 : Partitario fornitori           *
      *                    - pcge301a : Saldo alla data sottoconto     *
      *                    - pcge301b : Saldo alla data cliente        *
      *                    - pcge301c : Saldo alla data fornitore      *
      *                    - pcge301e : Estratto conto clienti         *
      *                    - pcge301f : Estratto conto fornitori       *
      *                                                                *
      *                    Interrogazioni per data:                    *
      *                                                                *
      *                    - pcge3018 : Per data registrazione         *
      *                    - pcge3019 : Per data immiss./ultima mod.   *
      *                                                                *
      *================================================================*

      ******************************************************************
       Environment Division.
      ******************************************************************

      *================================================================*
       Configuration Section.
      *================================================================*

       Source-Computer.        N-d-K-Sia-PD .
       Object-Computer.        N-d-K-Sia-PD .

       Special-Names.          Decimal-Point     Is Comma .

      ******************************************************************
       Data Division.
      ******************************************************************

      *================================================================*
       Working-Storage Section.
      *================================================================*

      *    *===========================================================*
      *    * Area di identificazione                                   *
      *    *-----------------------------------------------------------*
       01  i-ide.
      *        *-------------------------------------------------------*
      *        * Sistema applicativo                                   *
      *        *-------------------------------------------------------*
           05  i-ide-sap                  pic  x(03) value
                     "pgm"                                            .
      *        *-------------------------------------------------------*
      *        * Area gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-arg                  pic  x(03) value
                     "cge"                                            .
      *        *-------------------------------------------------------*
      *        * Settore gestionale                                    *
      *        *-------------------------------------------------------*
           05  i-ide-set                  pic  x(03) value
                     "mov"                                            .
      *        *-------------------------------------------------------*
      *        * Fase gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-fas                  pic  x(06) value
                     "cge301"                                         .
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10) value
                     "pcge3010"                                       .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  i-ide-des                  pic  x(40) value
                     "  INTERROGAZIONI SU MOVIMENTI CONTABILI "       .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "mvideo" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/v"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per moduli di input-output          *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/f"                                  .

      *    *===========================================================*
      *    * Area di definizione della valuta base                     *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/c"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "msegrt"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/s"                                  .

      *    *===========================================================*
      *    * Area per definizione codici di errore di i-o              *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/e"                                  .

      *    *===========================================================*
      *    * Record logico del file piano dei conti                    *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfpdc"                          .

      *    *===========================================================*
      *    * Record logico del file clienti                            *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfcli"                          .

      *    *===========================================================*
      *    * Record logico del file fornitori                          *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rffnt"                          .

      *    *===========================================================*
      *    * Record logico tabella [zcc]                               *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfzcc"                          .

      *    *===========================================================*
      *    * Work-area per lettura personalizzazioni                   *
      *    *-----------------------------------------------------------*
       01  w-prs.
      *        *-------------------------------------------------------*
      *        * Numero livelli del piano dei conti                    *
      *        *-------------------------------------------------------*
           05  w-prs-liv-pdc              pic  9(01)                  .

      *    *===========================================================*
      *    * Link-area richieste per interrogazione                    *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/pcge3010.pgl"                   .

      *    *===========================================================*
      *    * Work area per valori precedenti                           *
      *    *-----------------------------------------------------------*
       01  w-pre.
      *        *-------------------------------------------------------*
      *        * Tipo interrogazione                                   *
      *        *-------------------------------------------------------*
           05  w-pre-tip-int              pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Modalita' interrogazione                              *
      *        *-------------------------------------------------------*
           05  w-pre-mod-int              pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Tipo visualizzazione                                  *
      *        *-------------------------------------------------------*
           05  w-pre-tip-vis              pic  x(01)                  .

      *    *===========================================================*
      *    * Work area per determinazione anno e mese di esercizio     *
      *    *-----------------------------------------------------------*
       01  w-ese-cge.
      *        *-------------------------------------------------------*
      *        * Mese di chiusura anno di esercizio                    *
      *        *-------------------------------------------------------*
           05  w-ese-cge-mce              pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Data registrazione movimento                          *
      *        *-------------------------------------------------------*
           05  w-ese-cge-dtr              pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Anno di esercizio movimento                           *
      *        *-------------------------------------------------------*
           05  w-ese-cge-esa              pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Mese di esercizio movimento                           *
      *        *-------------------------------------------------------*
           05  w-ese-cge-esm              pic  9(02)                  .

      *    *===========================================================*
      *    * Work area locale                                          *
      *    *-----------------------------------------------------------*
       01  w-wrk.
      *        *-------------------------------------------------------*
      *        * Status di uscita da routines specifiche               *
      *        *-------------------------------------------------------*
           05  OK                         pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Data attuale                                          *
      *        *-------------------------------------------------------*
           05  w-wrk-dat-att              pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Esercizio per data iniziale                           *
      *        *-------------------------------------------------------*
           05  w-wrk-ese-ini              pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Esercizio per data finale                             *
      *        *-------------------------------------------------------*
           05  w-wrk-ese-fin              pic  9(03)                  .

      *    *===========================================================*
      *    * Link-area per accettazione codice sottoconto              *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/acmnpdc0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice cliente                 *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/acmncli0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice fornitore               *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/acmnfnt0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice causale contabile       *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/acmnzcc0.acl"                   .

      *    *===========================================================*
      *    * Work per accettazioni campi espansi                       *
      *    *-----------------------------------------------------------*
       01  w-exp.
      *        *-------------------------------------------------------*
      *        * Work per : Tipo interrogazione                        *
      *        *-------------------------------------------------------*
           05  w-exp-tip-int.
               10  w-exp-tip-int-num      pic  9(02)       value 3    .
               10  w-exp-tip-int-lun      pic  9(02)       value 20   .
               10  w-exp-tip-int-tbl.
                   15  filler             pic  x(20) value
                            "Generale            "                    .
                   15  filler             pic  x(20) value
                            "Clienti             "                    .
                   15  filler             pic  x(20) value
                            "Fornitori           "                    .
      *        *-------------------------------------------------------*
      *        * Work per : Modalita' di interrogazione                *
      *        *-------------------------------------------------------*
           05  w-exp-mod-int.
               10  w-exp-mod-int-num      pic  9(02)       value 3    .
               10  w-exp-mod-int-lun      pic  9(02)       value 40   .
               10  w-exp-mod-int-tbl.
                   15  filler             pic  x(40) value
                            "Per Sottoconto contabile                ".
                   15  filler             pic  x(40) value
                            "Per Data di registrazione               ".
                   15  filler             pic  x(40) value
                            "Per data immissione/ultima Modifica     ".
      *        *-------------------------------------------------------*
      *        * Work per : Tipo visualizzazione per conti di generale *
      *        *            - sottorichiesta : per sottoconto          *
      *        *-------------------------------------------------------*
           05  w-exp-tvs-ge1.
               10  w-exp-tvs-ge1-num      pic  9(02)       value 3    .
               10  w-exp-tvs-ge1-lun      pic  9(02)       value 20   .
               10  w-exp-tvs-ge1-tbl.
                   15  filler             pic  x(20) value
                            "Saldo ad una data   "                    .
                   15  filler             pic  x(20) value
                            "Lista movimenti     "                    .
                   15  filler             pic  x(20) value
                            "Partitario          "                    .
      *        *-------------------------------------------------------*
      *        * Work per : Tipo visualizzazione per conti di generale *
      *        *            - sottorichiesta : per data registrazione  *
      *        *                               o data immissione/ulti- *
      *        *                               ma modifica             *
      *        *-------------------------------------------------------*
           05  w-exp-tvs-ge2.
               10  w-exp-tvs-ge2-num      pic  9(02)       value 2    .
               10  w-exp-tvs-ge2-lun      pic  9(02)       value 30   .
               10  w-exp-tvs-ge2-tbl.
                   15  filler             pic  x(30) value
                            "Solo il Totale movimenti      "          .
                   15  filler             pic  x(30) value
                            "Lista movimenti               "          .
      *        *-------------------------------------------------------*
      *        * Work per : Tipo visualizzazione per conti clienti o   *
      *        *            fornitori                                  *
      *        *-------------------------------------------------------*
           05  w-exp-tvs-cof.
               10  w-exp-tvs-cof-num      pic  9(02)       value 4    .
               10  w-exp-tvs-cof-lun      pic  9(02)       value 20   .
               10  w-exp-tvs-cof-tbl.
                   15  filler             pic  x(20) value
                            "Saldo ad una data   "                    .
                   15  filler             pic  x(20) value
                            "Lista movimenti     "                    .
                   15  filler             pic  x(20) value
                            "Estratto conto      "                    .
                   15  filler             pic  x(20) value
                            "Partitario          "                    .

      *    *===========================================================*
      *    * Work-area per valori di defaults generali                 *
      *    *-----------------------------------------------------------*
       01  w-def.
      *        *-------------------------------------------------------*
      *        * Tipo interrogazione                                   *
      *        *-------------------------------------------------------*
           05  w-def-tip-int              pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Modalita' di interrogazione                           *
      *        *-------------------------------------------------------*
           05  w-def-mod-int              pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Tipo visualizzazione                                  *
      *        *-------------------------------------------------------*
           05  w-def-tip-vis              pic  x(01)                  .

      *    *===========================================================*
      *    * Work per subroutines di editing                           *
      *    *-----------------------------------------------------------*
       01  w-edt.
      *        *-------------------------------------------------------*
      *        * Work per editing codice sottoconto                    *
      *        *-------------------------------------------------------*
           05  w-edt-cod-pdc.
               10  w-edt-cod-pdc-liv      pic  9(01)                  .
               10  w-edt-cod-pdc-cod      pic  9(07)                  .
               10  w-edt-cod-pdc-edm      pic  x(20)                  .
               10  w-edt-cod-pdc-c01      pic  9(02)                  .
               10  w-edt-cod-pdc-bwz      pic  x(01)                  .
               10  w-edt-cod-pdc-edt.
                   15  w-edt-cod-pdc-s00.
                       20  w-edt-cod-pdc-s30.
                           25  w-edt-cod-pdc-s3l
                                          pic  9(02).9(02).9(03)      .
                       20  w-edt-cod-pdc-s20 redefines
                           w-edt-cod-pdc-s30.
                           25  w-edt-cod-pdc-s2l
                                          pic  9(02).9(03)            .
                           25  filler     pic  x(03)                  .
                   15  w-edt-cod-pdc-d00 redefines
                       w-edt-cod-pdc-s00.
                       20  w-edt-cod-pdc-d30.
                           25  w-edt-cod-pdc-d3l
                                          pic  9(02).9(02).9(03)      .
                       20  w-edt-cod-pdc-d20 redefines
                           w-edt-cod-pdc-d30.
                           25  filler     pic  x(03)                  .
                           25  w-edt-cod-pdc-d2l
                                          pic  9(02).9(03)            .

      ******************************************************************
       Procedure Division.
      ******************************************************************

      *================================================================*
      *       Main program                                             *
      *----------------------------------------------------------------*
       main-000.
      *              *-------------------------------------------------*
      *              * Dichiarazione di inizio programma               *
      *              *-------------------------------------------------*
           perform   dic-ini-pgm-000      thru dic-ini-pgm-999        .
           if        OK                   not  = spaces
                     go to main-999.
       main-100.
      *              *-------------------------------------------------*
      *              * Esecuzione routine pre-esecuzione programma     *
      *              *-------------------------------------------------*
           move      spaces               to   OK                     .
           perform   pre-exe-pgm-000      thru pre-exe-pgm-999        .
           if        OK                   not  = spaces
                     go to  main-950.
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
           perform   rou-opn-fls-000      thru rou-opn-fls-999        .
       main-200.
      *              *-------------------------------------------------*
      *              * Normalizzazioni iniziali                        *
      *              *-------------------------------------------------*
           perform   nor-ric-sel-000      thru nor-ric-sel-999        .
      *              *-------------------------------------------------*
      *              * Titolo programma a video                        *
      *              *-------------------------------------------------*
           perform   vis-tit-pgm-000      thru vis-tit-pgm-999        .
      *              *-------------------------------------------------*
      *              * Visualizzazione prompt richieste                *
      *              *-------------------------------------------------*
           perform   pmt-ric-sel-000      thru pmt-ric-sel-999        .
      *              *-------------------------------------------------*
      *              * Accettazione record richieste                   *
      *              *-------------------------------------------------*
           move      spaces               to   OK                     .
           perform   acc-ric-sel-000      thru acc-ric-sel-999        .
           if        OK                   not  = spaces
                     go to  main-900.
      *              *-------------------------------------------------*
      *              * Regolarizzazione record richieste               *
      *              *-------------------------------------------------*
           perform   reg-ric-sel-000      thru reg-ric-sel-999        .
      *              *-------------------------------------------------*
      *              * Esecuzione programma di interrogazione          *
      *              *-------------------------------------------------*
           perform   ese-prg-int-000      thru ese-prg-int-999        .
      *              *-------------------------------------------------*
      *              * Test se programma deve riciclare                *
      *              *-------------------------------------------------*
           if        rr-snx-sel           =    spaces
                     go to  main-200.
       main-900.
      *              *-------------------------------------------------*
      *              * Close files                                     *
      *              *-------------------------------------------------*
           perform   rou-cls-fls-000      thru rou-cls-fls-999        .
       main-950.
      *              *-------------------------------------------------*
      *              * Dichiarazione di fine programma                 *
      *              *-------------------------------------------------*
           perform   dic-fin-pgm-000      thru dic-fin-pgm-999        .
       main-999.
           exit      program                                          .

      *================================================================*
      *       Routines                                                 *
      *================================================================*

      *    *===========================================================*
      *    * Esecuzione accettazione di un campo                       *
      *    *-----------------------------------------------------------*
       exe-acc-cmp-000.
      *              *-------------------------------------------------*
      *              * Tasto di funzione Exit : sempre abilitato       *
      *              *-------------------------------------------------*
           move      "EXIT"               to   v-pfk (20)             .
      *              *-------------------------------------------------*
      *              * Tasto di funzione DO   : sempre abilitato       *
      *              *-------------------------------------------------*
           move      "DO  "               to   v-pfk (05)             .
      *              *-------------------------------------------------*
      *              * Accettazione                                    *
      *              *-------------------------------------------------*
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       exe-acc-cmp-999.
           exit.

      *    *===========================================================*
      *    * Dichiarazione di inizio programma                         *
      *    *-----------------------------------------------------------*
       dic-ini-pgm-000.
      *              *-------------------------------------------------*
      *              * Sigla funzione                                  *
      *              *-------------------------------------------------*
           move      "P+"                 to   s-ope                  .
      *              *-------------------------------------------------*
      *              * Sistema applicativo                             *
      *              *-------------------------------------------------*
           move      i-ide-sap            to   s-sap                  .
      *              *-------------------------------------------------*
      *              * Area gestionale                                 *
      *              *-------------------------------------------------*
           move      i-ide-arg            to   s-arg                  .
      *              *-------------------------------------------------*
      *              * Settore gestionale                              *
      *              *-------------------------------------------------*
           move      i-ide-set            to   s-set                  .
      *              *-------------------------------------------------*
      *              * Fase gestionale                                 *
      *              *-------------------------------------------------*
           move      i-ide-fas            to   s-fas                  .
      *              *-------------------------------------------------*
      *              * Sigla interna del programma                     *
      *              *-------------------------------------------------*
           move      i-ide-pro            to   s-pro                  .
      *              *-------------------------------------------------*
      *              * Flag di save video                              *
      *              *-------------------------------------------------*
           move      "S"                  to   s-svv                  .
      *              *-------------------------------------------------*
      *              * Richiamo del modulo di segreteria               *
      *              *-------------------------------------------------*
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Controllo esito richiamo modulo                 *
      *              *-------------------------------------------------*
           if        s-liv                =    zero
                     move  "#"            to   OK
           else      move  spaces         to   OK                     .
       dic-ini-pgm-999.
           exit.

      *    *===========================================================*
      *    * Dichiarazione di fine programma                           *
      *    *-----------------------------------------------------------*
       dic-fin-pgm-000.
      *              *-------------------------------------------------*
      *              * Sigla funzione                                  *
      *              *-------------------------------------------------*
           move      "P-"                 to   s-ope                  .
      *              *-------------------------------------------------*
      *              * Sigla interna del programma                     *
      *              *-------------------------------------------------*
           move      i-ide-pro            to   s-pro                  .
      *              *-------------------------------------------------*
      *              * Richiamo del modulo di segreteria               *
      *              *-------------------------------------------------*
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
       dic-fin-pgm-999.
           exit.

      *    *===========================================================*
      *    * Open files                                                *
      *    *-----------------------------------------------------------*
       rou-opn-fls-000.
      *              *-------------------------------------------------*
      *              * Apertura file 'pdc'                             *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofpdc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdc                 .
      *              *-------------------------------------------------*
      *              * Apertura file 'cli'                             *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofcli"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cli                 .
      *              *-------------------------------------------------*
      *              * Apertura file 'fnt'                             *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/ioffnt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fnt                 .
      *              *-------------------------------------------------*
      *              * [zcc]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofzcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zcc                 .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice sottoconto      *
      *              *-------------------------------------------------*
           perform   cod-mne-pdc-opn-000  thru cod-mne-pdc-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice cliente         *
      *              *-------------------------------------------------*
           perform   cod-mne-cli-opn-000  thru cod-mne-cli-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice fornitore       *
      *              *-------------------------------------------------*
           perform   cod-mne-fnt-opn-000  thru cod-mne-fnt-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice causale         *
      *              *-------------------------------------------------*
           perform   cod-mne-zcc-opn-000  thru cod-mne-zcc-opn-999    .
       rou-opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Close files                                               *
      *    *-----------------------------------------------------------*
       rou-cls-fls-000.
      *              *-------------------------------------------------*
      *              * Chiusura file 'pdc'                             *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofpdc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdc                 .
      *              *-------------------------------------------------*
      *              * Chiusura file 'cli'                             *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofcli"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cli                 .
      *              *-------------------------------------------------*
      *              * Chiusura file 'fnt'                             *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/ioffnt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fnt                 .
      *              *-------------------------------------------------*
      *              * [zcc]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofzcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zcc                 .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice sottoconto     *
      *              *-------------------------------------------------*
           perform   cod-mne-pdc-cls-000  thru cod-mne-pdc-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice cliente        *
      *              *-------------------------------------------------*
           perform   cod-mne-cli-cls-000  thru cod-mne-cli-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice fornitore      *
      *              *-------------------------------------------------*
           perform   cod-mne-fnt-cls-000  thru cod-mne-fnt-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice causale        *
      *              *-------------------------------------------------*
           perform   cod-mne-zcc-cls-000  thru cod-mne-zcc-cls-999    .
       rou-cls-fls-999.
           exit.

      *    *===========================================================*
      *    * Routine pre-esecuzione programma                          *
      *    *                                                           *
      *    * Uscita  : OK = spaces : continua l'esecuzione             *
      *    *                "#"    : terminazione programma            *
      *    *-----------------------------------------------------------*
       pre-exe-pgm-000.
      *              *-------------------------------------------------*
      *              * Subroutine per la preparazione dei valori rela- *
      *              * tivi alla valuta base, determinati dalla segre- *
      *              * teria                                           *
      *              *-------------------------------------------------*
           move      "VB"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-dec                to   c-dec                  .
           move      s-asx                to   c-sgl                  .
           move      s-sgn                to   c-tdc                  .
           move      s-num                to   c-cdc                  .
           move      s-adx (01 : 20)      to   c-des                  .
           move      s-adx (21 : 20)      to   c-din                  .
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione numero livelli del    *
      *              * piano dei conti                                 *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/cge[liv-pdc]"   to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-num          to   w-prs-liv-pdc
           else      move  3              to   w-prs-liv-pdc          .
      *              *-------------------------------------------------*
      *              * Determinazione data attuale                     *
      *              *-------------------------------------------------*
           move      "DT"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-dat                to   w-wrk-dat-att          .
      *              *-------------------------------------------------*
      *              * Preparazione defaults generali                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-def-tip-int          .
           move      spaces               to   w-def-mod-int          .
           move      spaces               to   w-def-tip-vis          .
       pre-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione titolo programma                          *
      *    *-----------------------------------------------------------*
       vis-tit-pgm-000.
      *              *-------------------------------------------------*
      *              * Erase video                                     *
      *              *-------------------------------------------------*
           move      "ER"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Trattini a linea 01                             *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      01                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      all "="              to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Sigla del programma                             *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      02                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      i-ide-fas            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Descrizione del programma                       *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      02                   to   v-lin                  .
           move      21                   to   v-pos                  .
           move      i-ide-des            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Trattini a linea 03                             *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      03                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      all "="              to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Trattini a linea 22                             *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      22                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      all "="              to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tit-pgm-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazioni iniziali                                  *
      *    *-----------------------------------------------------------*
       nor-ric-sel-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione record richieste                *
      *              *-------------------------------------------------*
           move      spaces               to   rr                     .
           move      spaces               to   rr-tip-int             .
           move      spaces               to   rr-mod-int             .
           move      zero                 to   rr-cod-arc             .
           move      spaces               to   rr-des-arc             .
           move      spaces               to   rr-tip-vis             .
           move      zero                 to   rr-dat-ini             .
           move      zero                 to   rr-dat-fin             .
           move      zero                 to   rr-sel-cau             .
           move      zero                 to   rr-sel-imp             .
           move      spaces               to   rr-sel-cca             .
           move      spaces               to   rr-sel-ute             .
           move      spaces               to   rr-sel-fas             .
           move      spaces               to   rr-snx-sel             .
      *              *-------------------------------------------------*
      *              * Normalizzazione area valori precedenti          *
      *              *-------------------------------------------------*
           move      spaces               to   w-pre-tip-int          .
           move      spaces               to   w-pre-mod-int          .
           move      spaces               to   w-pre-tip-vis          .
       nor-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt richieste                          *
      *    *-----------------------------------------------------------*
       pmt-ric-sel-000.
      *              *-------------------------------------------------*
      *              * 'Tipo movimenti              :'                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tipo movimenti             :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * 'Tipo visualizzazione        :'                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tipo visualizzazione       :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * 'Data registrazione iniziale :'                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Data registrazione iniziale:"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * 'Data registrazione finale   :'                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Data registrazione finale  :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * 'Selezione su :'                                *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      14                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Selezione su :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * '- Causale contabile :'                         *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      21                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      08                   to   v-pos                  .
           move      "- Causale contabile :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Accettazione record richieste                             *
      *    *-----------------------------------------------------------*
       acc-ric-sel-000.
      *              *-------------------------------------------------*
      *              * Tipo movimenti                                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione eventuale default              *
      *                  *---------------------------------------------*
           if        rr-tip-int           not  = spaces
                     go to acc-ric-sel-010.
           if        w-def-tip-int        =    spaces
                     go to acc-ric-sel-010.
           move      w-def-tip-int        to   rr-tip-int             .
       acc-ric-sel-010.
      *                  *---------------------------------------------*
      *                  * Accettazione                                *
      *                  *---------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-int-lun    to   v-car                  .
           move      w-exp-tip-int-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      "GCF#"               to   v-msk                  .
           move      05                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-tip-int-tbl    to   v-txt                  .
           if        rr-tip-int           =    "G"
                     move  01             to   v-num
           else if   rr-tip-int           =    "C"
                     move  02             to   v-num
           else if   rr-tip-int           =    "F"
                     move  03             to   v-num
           else      move  zero           to   v-num                  .
           move      "DOWN"               to   v-pfk (02)             .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *                  *---------------------------------------------*
      *                  * Se premuta function-key 'EXIT' si esce      *
      *                  *---------------------------------------------*
           if        v-key                =    "EXIT"
                     move   "#"           to   OK
                     go to  acc-ric-sel-999.
      *                  *---------------------------------------------*
      *                  * Valore impostato in campo di destinazione   *
      *                  *---------------------------------------------*
           if        v-num                =    01
                     move  "G"            to   rr-tip-int
           else if   v-num                =    02
                     move  "C"            to   rr-tip-int
           else if   v-num                =    03
                     move  "F"            to   rr-tip-int
           else      move  spaces         to   rr-tip-int             .
      *                  *---------------------------------------------*
      *                  * Memorizzazione valore di default generale   *
      *                  *---------------------------------------------*
           move      rr-tip-int           to   w-def-tip-int          .
      *                  *---------------------------------------------*
      *                  * Confronto con valore precedente             *
      *                  *---------------------------------------------*
           if        rr-tip-int           =    w-pre-tip-int
                     go to  acc-ric-sel-040.
      *                  *---------------------------------------------*
      *                  * Normalizzazione codice archivio             *
      *                  *---------------------------------------------*
           move      zero                 to   rr-cod-arc             .
      *                  *---------------------------------------------*
      *                  * Normalizzazione descrizione archivio        *
      *                  *---------------------------------------------*
           move      spaces               to   rr-des-arc             .
      *                  *---------------------------------------------*
      *                  * Normalizzazione tipo visualizzazione        *
      *                  *---------------------------------------------*
           move      spaces               to   rr-tip-vis             .
      *                      *-----------------------------------------*
      *                      * Abblencamento area video tipo visualiz- *
      *                      * zazione                                 *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-tip-vis           to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Erase line 07-11                            *
      *                  *---------------------------------------------*
           move      "EL"                 to   v-ope                  .
           move      07                   to   v-lin                  .
           move      11                   to   v-lto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Se movimenti di generale                    *
      *                  *---------------------------------------------*
           if        rr-tip-int           not  = "G"
                     go to  acc-ric-sel-020.
      *                      *-----------------------------------------*
      *                      * Prompt per modalita' interrogazione     *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Modalita' interrogazione   :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Se tipo visualizzazione precedente 'E'  *
      *                      * o 'S' si rivisualizza :                 *
      *                      *-----------------------------------------*
           if        w-pre-tip-vis        not  = "E" and
                     w-pre-tip-vis        not  = "S"
                     go to  acc-ric-sel-040.
      *                          *-------------------------------------*
      *                          * Data registrazione iniziale e finale*
      *                          *-------------------------------------*
           move      zero                 to   rr-dat-ini             .
           move      zero                 to   rr-dat-fin             .
           perform   vis-dat-ief-000      thru vis-dat-ief-999        .
      *                          *-------------------------------------*
      *                          * Prompt 'Selezione su :'             *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      14                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Selezione su :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                          *-------------------------------------*
      *                          * Prompt '- Causale contabile :'      *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      21                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      08                   to   v-pos                  .
           move      "- Causale contabile :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                          *-------------------------------------*
      *                          * Prompt '- Importo           :'      *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      21                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      08                   to   v-pos                  .
           move      "- Importo           :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           go to     acc-ric-sel-040.
       acc-ric-sel-020.
      *                  *---------------------------------------------*
      *                  * Se movimenti clienti o fornitori            *
      *                  *---------------------------------------------*
           if        rr-tip-int           not  = "C" and
                     rr-tip-int           not  = "F"
                     go to  acc-ric-sel-040.
      *                      *-----------------------------------------*
      *                      * Prompt per codice cliente o fornitore   *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      01                   to   v-pos                  .
           if        rr-tip-int           =    "C"
                     move   "Codice cliente             :"
                                          to   v-alf
           else      move   "Codice fornitore           :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Se interrogazione precedente per data   *
      *                      * immissione/ultima modifica              *
      *                      *-----------------------------------------*
           if        rr-mod-int           not  = "I"
                     go to  acc-ric-sel-030.
      *                          *-------------------------------------*
      *                          * Visualizzazione data registrazione  *
      *                          * iniziale e finale                   *
      *                          *-------------------------------------*
           perform   vis-dat-ief-000      thru vis-dat-ief-999        .
      *                          *-------------------------------------*
      *                          * Abblencamento linea 21              *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      21                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-ric-sel-030.
      *                      *-----------------------------------------*
      *                      * Trattamento selezione su importo        *
      *                      *-----------------------------------------*
           if        w-pre-tip-vis        =    "P" or
                     w-pre-tip-vis        =    "E" or
                     w-pre-tip-vis        =    "S"
                     go to  acc-ric-sel-033.
      *                              *---------------------------------*
      *                              * Prompt                          *
      *                              *---------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      21                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      08                   to   v-pos                  .
           move      "- Importo           :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                              *---------------------------------*
      *                              * Valore                          *
      *                              *---------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      13                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "<BG"                to   v-edm                  .
           move      20                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-sel-imp           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-ric-sel-033.
      *                      *-----------------------------------------*
      *                      * Trattamento selezione su contenuto cau- *
      *                      * sale                                    *
      *                      *-----------------------------------------*
      *                              *---------------------------------*
      *                              * Prompt                          *
      *                              *---------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      21                   to   v-car                  .
           move      21                   to   v-lin                  .
           move      08                   to   v-pos                  .
           move      "- Contenuto causale :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                              *---------------------------------*
      *                              * Valore                          *
      *                              *---------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      21                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-sel-cca           to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-ric-sel-035.
      *                      *-----------------------------------------*
      *                      * Normalizzazione modalita' interrogazione*
      *                      *-----------------------------------------*
           move      spaces               to   rr-mod-int             .
           move      spaces               to   w-pre-mod-int          .
       acc-ric-sel-040.
      *                  *---------------------------------------------*
      *                  * Aggiornamento valore precedente             *
      *                  *---------------------------------------------*
           move      rr-tip-int           to   w-pre-tip-int          .
      *                  *---------------------------------------------*
      *                  * Test su valore impostato                    *
      *                  *---------------------------------------------*
           if        rr-tip-int           not  = "G" and
                     rr-tip-int           not  = "C" and
                     rr-tip-int           not  = "F"
                     go to  acc-ric-sel-000.
      *                  *---------------------------------------------*
      *                  * Se premuta function-key 'DO  ' si esce      *
      *                  *---------------------------------------------*
           if        v-key                not  = "DO  "
                     go to  acc-ric-sel-100.
           move      spaces               to   OK                     .
           perform   cnt-tas-dox-000      thru cnt-tas-dox-999        .
           if        OK                   not  = spaces
                     move   spaces        to   OK
                     go to  acc-ric-sel-000
           else      go to  acc-ric-sel-999.
       acc-ric-sel-100.
      *              *-------------------------------------------------*
      *              * Modalita' di interrogazione                     *
      *              *-------------------------------------------------*
           if        rr-tip-int           not  = "G"
                     go to  acc-ric-sel-200.
      *                  *---------------------------------------------*
      *                  * Preparazione eventuale default              *
      *                  *---------------------------------------------*
           if        rr-mod-int           not  = spaces
                     go to acc-ric-sel-105.
           if        w-def-mod-int        =    spaces
                     go to acc-ric-sel-105.
           move      w-def-mod-int        to   rr-mod-int             .
       acc-ric-sel-105.
      *                  *---------------------------------------------*
      *                  * Accettazione                                *
      *                  *---------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-mod-int-lun    to   v-car                  .
           move      w-exp-mod-int-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      "SDM#"               to   v-msk                  .
           move      07                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-mod-int-tbl    to   v-txt                  .
           if        rr-mod-int           =    "S"
                     move  01             to   v-num
           else if   rr-mod-int           =    "R"
                     move  02             to   v-num
           else if   rr-mod-int           =    "I"
                     move  03             to   v-num
           else      move  zero           to   v-num                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *                  *---------------------------------------------*
      *                  * Se premuta function-key 'EXIT' si esce      *
      *                  *---------------------------------------------*
           if        v-key                =    "EXIT"
                     move   "#"           to   OK
                     go to  acc-ric-sel-999.
      *                  *---------------------------------------------*
      *                  * Valore impostato in campo di destinazione   *
      *                  *---------------------------------------------*
           if        v-num                =    01
                     move  "S"            to   rr-mod-int
           else if   v-num                =    02
                     move  "R"            to   rr-mod-int
           else if   v-num                =    03
                     move  "I"            to   rr-mod-int
           else      move  spaces         to   rr-mod-int             .
      *                  *---------------------------------------------*
      *                  * Memorizzazione valore di default generale   *
      *                  *---------------------------------------------*
           move      rr-mod-int           to   w-def-mod-int          .
      *                  *---------------------------------------------*
      *                  * Confronto con valore precedente             *
      *                  *---------------------------------------------*
           if        rr-mod-int           =    w-pre-mod-int
                     go to  acc-ric-sel-140.
      *                  *---------------------------------------------*
      *                  * Azzeramento codice archivio                 *
      *                  *---------------------------------------------*
           move      zero                 to   rr-cod-arc             .
      *                  *---------------------------------------------*
      *                  * Normalizzazione descrizione archivio        *
      *                  *---------------------------------------------*
           move      spaces               to   rr-des-arc             .
      *                  *---------------------------------------------*
      *                  * Normalizzazione tipo visualizzazione        *
      *                  *---------------------------------------------*
           move      spaces               to   rr-tip-vis             .
      *                  *---------------------------------------------*
      *                  * Abblencamento area video tipo visualizza-   *
      *                  * zione                                       *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-tip-vis           to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Se valore precedente e' 'I' si ripristinano *
      *                  * data registrazione iniziale e finale        *
      *                  *---------------------------------------------*
           if        w-pre-mod-int        not  = "I"
                     go to  acc-ric-sel-110.
           perform   vis-dat-ief-000      thru vis-dat-ief-999        .
       acc-ric-sel-110.
      *                  *---------------------------------------------*
      *                  * Se interrogazione per data registrazione    *
      *                  *---------------------------------------------*
           if        rr-mod-int           =    "S"
                     go to  acc-ric-sel-120.
      *                      *-----------------------------------------*
      *                      * Abblencamento area video codice e de-   *
      *                      * scrizione archivio                      *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Normalizzazione selezione su importo    *
      *                      *-----------------------------------------*
           move      zero                 to   rr-sel-imp             .
      *                      *-----------------------------------------*
      *                      * Normalizzazione selezione su programma  *
      *                      *-----------------------------------------*
           move      spaces               to   rr-sel-fas             .
      *                      *-----------------------------------------*
      *                      * Normalizzazione selezione su utente     *
      *                      *-----------------------------------------*
           move      spaces               to   rr-sel-ute             .
      *                      *-----------------------------------------*
      *                      * Abblencamento linea 20                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Abblencamento linea 21                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      21                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Se tipo visualizzazione precedente 'E'  *
      *                      * o 'S' si rivisualizza :                 *
      *                      *-----------------------------------------*
           if        w-pre-tip-vis        not  = "E" and
                     w-pre-tip-vis        not  = "S"
                     go to  acc-ric-sel-115.
      *                          *-------------------------------------*
      *                          * Prompt 'Selezione su :'             *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      14                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Selezione su :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                          *-------------------------------------*
      *                          * Prompt '- Causale contabile :'      *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      21                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      08                   to   v-pos                  .
           move      "- Causale contabile :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-ric-sel-115.
      *                  *---------------------------------------------*
      *                  * Se interrogazione per data immissione/ult.  *
      *                  * modifica                                    *
      *                  *---------------------------------------------*
           if        rr-mod-int           not  = "I"
                     go to  acc-ric-sel-140.
      *                      *-----------------------------------------*
      *                      * Trattamento data immissione/ultima mo-  *
      *                      * difica iniziale                         *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Prompt                              *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      42                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Data immissione/ultima modifica iniziale :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                          *-------------------------------------*
      *                          * Valore                              *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      14                   to   v-lin                  .
           move      44                   to   v-pos                  .
           move      rr-dat-ini           to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Trattamento data immissione/ultima mo-  *
      *                      * difica finale                           *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Prompt                              *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      42                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Data immissione/ultima modifica finale   :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                          *-------------------------------------*
      *                          * Valore                              *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      15                   to   v-lin                  .
           move      44                   to   v-pos                  .
           move      rr-dat-fin           to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Prompt per selezione su utente          *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      21                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      08                   to   v-pos                  .
           move      "- Utente            :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Prompt per selezione su fase            *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      21                   to   v-car                  .
           move      21                   to   v-lin                  .
           move      08                   to   v-pos                  .
           move      "- Programma         :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           go to     acc-ric-sel-140.
       acc-ric-sel-120.
      *                  *---------------------------------------------*
      *                  * Se interrogazione per sottoconto            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Prompt per codice sottoconto            *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice sottoconto          :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Normalizzazione selezione su programma  *
      *                      *-----------------------------------------*
           move      spaces               to   rr-sel-fas             .
      *                      *-----------------------------------------*
      *                      * Normalizzazione selezione su utente     *
      *                      *-----------------------------------------*
           move      spaces               to   rr-sel-ute             .
      *                      *-----------------------------------------*
      *                      * Abblencamento linea 20                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Abblencamento linea 21                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      21                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Prompt per selezione su importo         *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      21                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      08                   to   v-pos                  .
           move      "- Importo           :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-ric-sel-140.
      *                  *---------------------------------------------*
      *                  * Aggiornamento valore precedente             *
      *                  *---------------------------------------------*
           move      rr-mod-int           to   w-pre-mod-int          .
      *                  *---------------------------------------------*
      *                  * Se premuta function-key 'UP  '              *
      *                  *---------------------------------------------*
           if        v-key                not  = "UP  "
                     go to  acc-ric-sel-160.
           if        rr-mod-int           =    "S" or
                     rr-mod-int           =    "R" or
                     rr-mod-int           =    "I" or
                     rr-mod-int           =    " "
                     go to  acc-ric-sel-000
           else      go to  acc-ric-sel-100.
       acc-ric-sel-160.
      *                  *---------------------------------------------*
      *                  * Controllo valore impostato                  *
      *                  *---------------------------------------------*
           if        rr-mod-int           not  = "S" and
                     rr-mod-int           not  = "R" and
                     rr-mod-int           not  = "I"
                     go to  acc-ric-sel-100.
      *                  *---------------------------------------------*
      *                  * Se premuta function-key 'DO  ' si esce      *
      *                  *---------------------------------------------*
           if        v-key                not  = "DO  "
                     go to  acc-ric-sel-200.
           move      spaces               to   OK                     .
           perform   cnt-tas-dox-000      thru cnt-tas-dox-999        .
           if        OK                   not  = spaces
                     move   spaces        to   OK
                     go to  acc-ric-sel-100
           else      go to  acc-ric-sel-999.
       acc-ric-sel-200.
           if        rr-tip-int           =    "G" and
                     rr-mod-int           not  = "S"
                     go to  acc-ric-sel-300.
      *              *-------------------------------------------------*
      *              * Codice archivio                                 *
      *              *-------------------------------------------------*
           if        rr-tip-int           =    "C"
                     go to acc-ric-sel-205
           else if   rr-tip-int           =    "F"
                     go to acc-ric-sel-210
           else      go to acc-ric-sel-215.
       acc-ric-sel-205.
      *                  *---------------------------------------------*
      *                  * Clienti                                     *
      *                  *---------------------------------------------*
           move      "AC"                 to   w-cod-mne-cli-ope      .
           move      rr-cod-arc           to   w-cod-mne-cli-cod      .
           move      08                   to   w-cod-mne-cli-lin      .
           move      30                   to   w-cod-mne-cli-pos      .
           move      08                   to   w-cod-mne-cli-rln      .
           move      41                   to   w-cod-mne-cli-rps      .
           move      09                   to   w-cod-mne-cli-vln      .
           move      41                   to   w-cod-mne-cli-vps      .
           move      10                   to   w-cod-mne-cli-lln      .
           move      41                   to   w-cod-mne-cli-lps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           perform   cod-mne-cli-cll-000  thru cod-mne-cli-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-mne-cli-foi-000  thru cod-mne-cli-foi-999    .
       acc-ric-sel-206.
           perform   cod-mne-cli-cll-000  thru cod-mne-cli-cll-999    .
           if        w-cod-mne-cli-ope    =    "F+"
                     go to acc-ric-sel-207.
           if        w-cod-mne-cli-ope    =    "AC"
                     go to acc-ric-sel-208.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-ric-sel-207.
           perform   cod-mne-cli-foi-000  thru cod-mne-cli-foi-999    .
           go to     acc-ric-sel-206.
       acc-ric-sel-208.
           move      w-cod-mne-cli-cod    to   v-num                  .
           move      v-num                to   rr-cod-arc             .
           go to     acc-ric-sel-220.
       acc-ric-sel-210.
      *                  *---------------------------------------------*
      *                  * Fornitori                                   *
      *                  *---------------------------------------------*
           move      "AC"                 to   w-cod-mne-fnt-ope      .
           move      rr-cod-arc           to   w-cod-mne-fnt-cod      .
           move      08                   to   w-cod-mne-fnt-lin      .
           move      30                   to   w-cod-mne-fnt-pos      .
           move      08                   to   w-cod-mne-fnt-rln      .
           move      41                   to   w-cod-mne-fnt-rps      .
           move      09                   to   w-cod-mne-fnt-vln      .
           move      41                   to   w-cod-mne-fnt-vps      .
           move      10                   to   w-cod-mne-fnt-lln      .
           move      41                   to   w-cod-mne-fnt-lps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           perform   cod-mne-fnt-cll-000  thru cod-mne-fnt-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-mne-fnt-foi-000  thru cod-mne-fnt-foi-999    .
       acc-ric-sel-211.
           perform   cod-mne-fnt-cll-000  thru cod-mne-fnt-cll-999    .
           if        w-cod-mne-fnt-ope    =    "F+"
                     go to acc-ric-sel-212.
           if        w-cod-mne-fnt-ope    =    "AC"
                     go to acc-ric-sel-213.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-ric-sel-212.
           perform   cod-mne-fnt-foi-000  thru cod-mne-fnt-foi-999    .
           go to     acc-ric-sel-211.
       acc-ric-sel-213.
           move      w-cod-mne-fnt-cod    to   v-num                  .
           move      v-num                to   rr-cod-arc             .
           go to     acc-ric-sel-220.
       acc-ric-sel-215.
      *                  *---------------------------------------------*
      *                  * Generale                                    *
      *                  *---------------------------------------------*
           move      "AC"                 to   w-cod-mne-pdc-ope      .
           move      w-prs-liv-pdc        to   w-cod-mne-pdc-liv      .
           move      rr-cod-arc           to   w-cod-mne-pdc-cod      .
           move      09                   to   w-cod-mne-pdc-lin      .
           move      30                   to   w-cod-mne-pdc-pos      .
           move      09                   to   w-cod-mne-pdc-dln      .
           move      41                   to   w-cod-mne-pdc-dps      .
           move      "B"                  to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           perform   cod-mne-pdc-cll-000  thru cod-mne-pdc-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-mne-pdc-foi-000  thru cod-mne-pdc-foi-999    .
       acc-ric-sel-216.
           perform   cod-mne-pdc-cll-000  thru cod-mne-pdc-cll-999    .
           if        w-cod-mne-pdc-ope    =    "F+"
                     go to acc-ric-sel-217.
           if        w-cod-mne-pdc-ope    =    "AC"
                     go to acc-ric-sel-218.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-ric-sel-217.
           perform   cod-mne-pdc-foi-000  thru cod-mne-pdc-foi-999    .
           go to     acc-ric-sel-216.
       acc-ric-sel-218.
           move      w-cod-mne-pdc-cod    to   v-num                  .
           move      v-num                to   rr-cod-arc             .
       acc-ric-sel-220.
      *                  *---------------------------------------------*
      *                  * Se premuta function-key 'EXIT' si esce      *
      *                  *---------------------------------------------*
           if        v-key                =    "EXIT"
                     move   "#"           to   OK
                     go to  acc-ric-sel-999.
      *                  *---------------------------------------------*
      *                  * Lettura descrizione conto                   *
      *                  *---------------------------------------------*
           if        rr-cod-arc           =    zero
                     move   spaces        to   rr-des-arc
                     go to  acc-ric-sel-250.
      *                      *-----------------------------------------*
      *                      * Se sottoconto contabile                 *
      *                      *-----------------------------------------*
           if        rr-tip-int           not  = "G"
                     go to  acc-ric-sel-230.
           move      "RK"                 to   f-ope                  .
           move      "CODPDC    "         to   f-key                  .
           move      rr-cod-arc           to   rf-pdc-cod-pdc         .
           move      "pgm/cge/fls/ioc/obj/iofpdc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdc                 .
           if        f-sts                not  = e-not-err
                     move   all "."       to   rf-pdc-des-pdc         .
           move      rf-pdc-des-pdc       to   rr-des-arc             .
           go to     acc-ric-sel-250.
       acc-ric-sel-230.
      *                      *-----------------------------------------*
      *                      * Se codice cliente                       *
      *                      *-----------------------------------------*
           if        rr-tip-int           not  = "C"
                     go to  acc-ric-sel-240.
           move      "RK"                 to   f-ope                  .
           move      "CODCLI    "         to   f-key                  .
           move      rr-cod-arc           to   rf-cli-cod-cli         .
           move      "pgm/cge/fls/ioc/obj/iofcli"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cli                 .
           if        f-sts                not  = e-not-err
                     move   all "."       to   rf-cli-rag-soc
                     move   all "."       to   rf-cli-via-cli
                     move   all "."       to   rf-cli-loc-cli         .
           move      rf-cli-rag-soc       to   rr-des-arc             .
           go to     acc-ric-sel-250.
       acc-ric-sel-240.
      *                      *-----------------------------------------*
      *                      * Se codice fornitore                     *
      *                      *-----------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODFNT    "         to   f-key                  .
           move      rr-cod-arc           to   rf-fnt-cod-fnt         .
           move      "pgm/cge/fls/ioc/obj/ioffnt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fnt                 .
           if        f-sts                not  = e-not-err
                     move   all "."       to   rf-fnt-rag-soc
                     move   all "."       to   rf-fnt-via-fnt
                     move   all "."       to   rf-fnt-loc-fnt         .
           move      rf-fnt-rag-soc       to   rr-des-arc             .
       acc-ric-sel-250.
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione conto           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se conto di generale                    *
      *                      *-----------------------------------------*
           if        rr-tip-int           not  = "G"
                     go to  acc-ric-sel-252.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      rr-des-arc           to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           go to     acc-ric-sel-260.
       acc-ric-sel-252.
      *                      *-----------------------------------------*
      *                      * Se conto clienti                        *
      *                      *-----------------------------------------*
           if        rr-tip-int           not  = "C"
                     go to  acc-ric-sel-254.
           if        rr-cod-arc           =    zero
                     move   spaces        to   rf-cli-via-cli
                                               rf-cli-loc-cli         .
      *                          *-------------------------------------*
      *                          * Ragione sociale                     *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      rr-des-arc           to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                          *-------------------------------------*
      *                          * Indirizzo                           *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      rf-cli-via-cli       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                          *-------------------------------------*
      *                          * Localita'                           *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      rf-cli-loc-cli       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           go to     acc-ric-sel-260.
       acc-ric-sel-254.
      *                      *-----------------------------------------*
      *                      * Se conto fornitori                      *
      *                      *-----------------------------------------*
           if        rr-cod-arc           =    zero
                     move   spaces        to   rf-fnt-via-fnt
                                               rf-fnt-loc-fnt         .
      *                          *-------------------------------------*
      *                          * Ragione sociale                     *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      rr-des-arc           to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                          *-------------------------------------*
      *                          * Indirizzo                           *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      rf-fnt-via-fnt       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                          *-------------------------------------*
      *                          * Localita'                           *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      rf-fnt-loc-fnt       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       acc-ric-sel-260.
      *                  *---------------------------------------------*
      *                  * Se premuta function-key 'UP'                *
      *                  *---------------------------------------------*
           if        v-key                not  = "UP"
                     go to  acc-ric-sel-270.
           if        rr-cod-arc           not  = zero and
                     f-sts                not  = e-not-err
                     go to  acc-ric-sel-200.
           if        rr-tip-int           =    "G"
                     go to  acc-ric-sel-100
           else      go to  acc-ric-sel-000.
       acc-ric-sel-270.
      *                  *---------------------------------------------*
      *                  * Controllo valore impostato                  *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err or
                     rr-cod-arc           =    zero
                     go to  acc-ric-sel-200.
      *                  *---------------------------------------------*
      *                  * Se premuta function-key 'DO' si esce        *
      *                  *---------------------------------------------*
           if        v-key                not  = "DO"
                     go to  acc-ric-sel-300.
           move      spaces               to   OK                     .
           perform   cnt-tas-dox-000      thru cnt-tas-dox-999        .
           if        OK                   not  = spaces
                     move   spaces        to   OK
                     go to  acc-ric-sel-200
           else      go to  acc-ric-sel-999.
       acc-ric-sel-300.
      *              *-------------------------------------------------*
      *              * Tipo visualizzazione                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo interroga-  *
      *                  * zione                                       *
      *                  *---------------------------------------------*
           if        rr-tip-int           =    "G"
                     go to acc-ric-sel-310
           else      go to acc-ric-sel-320.
       acc-ric-sel-310.
      *                      *-----------------------------------------*
      *                      * Se interrogazione 'di Generale'         *
      *                      *-----------------------------------------*
           perform   acc-tvs-gen-000      thru acc-tvs-gen-999        .
      *                          *-------------------------------------*
      *                          * Se premuta function-key 'EXIT' si   *
      *                          * esce                                *
      *                          *-------------------------------------*
           if        v-key                =    "EXIT"
                     move   "#"           to   OK
                     go to  acc-ric-sel-999.
      *                          *-------------------------------------*
      *                          * A confronto con valore precedente   *
      *                          *-------------------------------------*
           go to     acc-ric-sel-330.
       acc-ric-sel-320.
      *                      *-----------------------------------------*
      *                      * Se interrogazione 'Clienti' o 'Fornito- *
      *                      * ri'                                     *
      *                      *-----------------------------------------*
           perform   acc-tvs-cof-000      thru acc-tvs-cof-999        .
      *                          *-------------------------------------*
      *                          * Se premuta function-key 'EXIT' si   *
      *                          * esce                                *
      *                          *-------------------------------------*
           if        v-key                =    "EXIT"
                     move   "#"           to   OK
                     go to  acc-ric-sel-999.
      *                          *-------------------------------------*
      *                          * A confronto con valore precedente   *
      *                          *-------------------------------------*
           go to     acc-ric-sel-330.
       acc-ric-sel-330.
      *                  *---------------------------------------------*
      *                  * Confronto con valore precedente             *
      *                  *---------------------------------------------*
           if        rr-tip-vis           =    w-pre-tip-vis
                     go to  acc-ric-sel-375.
      *                      *-----------------------------------------*
      *                      * Se interrogazione su unico conto        *
      *                      *-----------------------------------------*
           if        rr-tip-int           =    "G" and
                     rr-mod-int           not  = "S"
                     go to  acc-ric-sel-375.
      *                          *-------------------------------------*
      *                          * Se tipo visualizzazione 'E'         *
      *                          *-------------------------------------*
           if        rr-tip-vis           =    "E"
                     go to  acc-ric-sel-340
           else      go to  acc-ric-sel-345.
       acc-ric-sel-340.
      *                              *---------------------------------*
      *                              * Se conto di generale valore non *
      *                              * accettabile                     *
      *                              *---------------------------------*
           if        rr-tip-int           =    "G"
                     go to  acc-ric-sel-300.
      *                              *---------------------------------*
      *                              * Preparazione data iniziale      *
      *                              *---------------------------------*
           move      w-wrk-dat-att        to   rr-dat-ini             .
      *                              *---------------------------------*
      *                              * Normalizzazione data finale     *
      *                              *---------------------------------*
           move      zero                 to   rr-dat-fin             .
      *                              *---------------------------------*
      *                              * Normalizzazione selezione cau-  *
      *                              * sale e importo                  *
      *                              *---------------------------------*
           move      zero                 to   rr-sel-cau             .
           move      zero                 to   rr-sel-imp             .
      *                              *---------------------------------*
      *                              * Abblencamento linee 14-21       *
      *                              *---------------------------------*
           move      "EL"                 to   v-ope                  .
           move      14                   to   v-lin                  .
           move      21                   to   v-lto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                              *---------------------------------*
      *                              * Prompt per data estratto conto  *
      *                              *---------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      55                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Estratto conto alla data   :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           go to     acc-ric-sel-375.
       acc-ric-sel-345.
      *                          *-------------------------------------*
      *                          * Se tipo visualizzazione 'S'         *
      *                          *-------------------------------------*
           if        rr-tip-vis           =    "S"
                     go to  acc-ric-sel-350
           else      go to  acc-ric-sel-355.
       acc-ric-sel-350.
      *                              *---------------------------------*
      *                              * Preparazione data iniziale      *
      *                              *---------------------------------*
           move      w-wrk-dat-att        to   rr-dat-ini             .
      *                              *---------------------------------*
      *                              * Normalizzazione data finale     *
      *                              *---------------------------------*
           move      zero                 to   rr-dat-fin             .
      *                              *---------------------------------*
      *                              * Normalizzazione selezione cau-  *
      *                              * sale e importo                  *
      *                              *---------------------------------*
           move      zero                 to   rr-sel-cau             .
           move      zero                 to   rr-sel-imp             .
      *                              *---------------------------------*
      *                              * Abblencamento linee 14-21       *
      *                              *---------------------------------*
           move      "EL"                 to   v-ope                  .
           move      14                   to   v-lin                  .
           move      21                   to   v-lto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                              *---------------------------------*
      *                              * Prompt per saldo alla data      *
      *                              *---------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      55                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Saldo alla data            :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           go to     acc-ric-sel-375.
       acc-ric-sel-355.
      *                          *-------------------------------------*
      *                          * Se tipo visualizzazione 'L'         *
      *                          *-------------------------------------*
           if        rr-tip-vis           =    "L"
                     go to  acc-ric-sel-360
           else      go to  acc-ric-sel-365.
       acc-ric-sel-360.
      *                              *---------------------------------*
      *                              * Se valore precedente a spazi :  *
      *                              * non si fa' nulla                *
      *                              *---------------------------------*
           if        w-pre-tip-vis        =    spaces
                     go to  acc-ric-sel-365.
      *                              *---------------------------------*
      *                              * Visualizzazione data registra-  *
      *                              * zione iniziale e finale         *
      *                              *---------------------------------*
           perform   vis-dat-ief-000      thru vis-dat-ief-999        .
      *                              *---------------------------------*
      *                              * Prompt 'Selezione su :'         *
      *                              *---------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      14                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Selezione su :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                              *---------------------------------*
      *                              * Prompt '- Causale contabile :'  *
      *                              *---------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      21                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      08                   to   v-pos                  .
           move      "- Causale contabile :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                              *---------------------------------*
      *                              * Prompt '- Importo           :'  *
      *                              *---------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      21                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      08                   to   v-pos                  .
           move      "- Importo           :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                              *---------------------------------*
      *                              * Prompt 'Contenuto causale'      *
      *                              *---------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      21                   to   v-car                  .
           move      21                   to   v-lin                  .
           move      08                   to   v-pos                  .
           move      "- Contenuto causale :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           go to     acc-ric-sel-375.
       acc-ric-sel-365.
      *                          *-------------------------------------*
      *                          * Se tipo visualizzazione 'P'         *
      *                          *-------------------------------------*
           if        rr-tip-vis           =    "P"
                     go to  acc-ric-sel-370
           else      go to  acc-ric-sel-375.
       acc-ric-sel-370.
      *                              *---------------------------------*
      *                              * Normalizzazione data iniziale e *
      *                              * finale                          *
      *                              *---------------------------------*
           if        w-pre-tip-vis        =    "S" or
                     w-pre-tip-vis        =    "E"
                     move   zero          to   rr-dat-ini
                     move   zero          to   rr-dat-fin             .
      *                              *---------------------------------*
      *                              * Normalizzazione selezione cau-  *
      *                              * sale e importo                  *
      *                              *---------------------------------*
           move      zero                 to   rr-sel-cau             .
           move      zero                 to   rr-sel-imp             .
      *                              *---------------------------------*
      *                              * Visualizzazione data registra-  *
      *                              * zione iniziale e finale         *
      *                              *---------------------------------*
           perform   vis-dat-ief-000      thru vis-dat-ief-999        .
      *                              *---------------------------------*
      *                              * Normalizzazione selezione cau-  *
      *                              * sale e importo                  *
      *                              *---------------------------------*
           move      zero                 to   rr-sel-cau             .
           move      zero                 to   rr-sel-imp             .
      *                              *---------------------------------*
      *                              * Abblencamento linee 17-21       *
      *                              *---------------------------------*
           move      "EL"                 to   v-ope                  .
           move      17                   to   v-lin                  .
           move      21                   to   v-lto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-ric-sel-375.
      *                  *---------------------------------------------*
      *                  * Aggiornamento valore precedente             *
      *                  *---------------------------------------------*
           move      rr-tip-vis           to   w-pre-tip-vis          .
      *                  *---------------------------------------------*
      *                  * Se premuta function-key 'UP  '              *
      *                  *---------------------------------------------*
           if        v-key                not  = "UP  "
                     go to  acc-ric-sel-385.
           if        rr-tip-int           =    "G" and
                     rr-mod-int           not  = "S"
                     go to  acc-ric-sel-380.
           if        rr-tip-vis           =    "S" or
                     rr-tip-vis           =    "L" or
                     rr-tip-vis           =    "P" or
                     rr-tip-vis           =    "E" or
                     rr-tip-vis           =    " "
                     go to  acc-ric-sel-200
           else      go to  acc-ric-sel-300.
       acc-ric-sel-380.
           if        rr-tip-vis           =    "T" or
                     rr-tip-vis           =    "L" or
                     rr-tip-vis           =    " "
                     go to  acc-ric-sel-100
           else      go to  acc-ric-sel-300.
       acc-ric-sel-385.
      *                  *---------------------------------------------*
      *                  * Controllo valore impostato                  *
      *                  *---------------------------------------------*
           if        rr-tip-int           =    "G" and
                     rr-mod-int           not  = "S"
                     go to  acc-ric-sel-390.
           if        rr-tip-vis           not  = "S" and
                     rr-tip-vis           not  = "L" and
                     rr-tip-vis           not  = "P" and
                     rr-tip-vis           not  = "E"
                     go to  acc-ric-sel-300
           else      go to  acc-ric-sel-395.
       acc-ric-sel-390.
           if        rr-tip-vis           not  = "T" and
                     rr-tip-vis           not  = "L"
                     go to  acc-ric-sel-300.
       acc-ric-sel-395.
      *                  *---------------------------------------------*
      *                  * Se premuta function-key 'DO  ' si esce      *
      *                  *---------------------------------------------*
           if        v-key                not  = "DO  "
                     go to  acc-ric-sel-400.
           move      spaces               to   OK                     .
           perform   cnt-tas-dox-000      thru cnt-tas-dox-999        .
           if        OK                   not  = spaces
                     move   spaces        to   OK
                     go to  acc-ric-sel-300
           else      go to  acc-ric-sel-999.
       acc-ric-sel-400.
      *              *-------------------------------------------------*
      *              * Data di registrazione iniziale oppure data saldo*
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      ">"                  to   v-edm                  .
           move      14                   to   v-lin                  .
           move      30                   to   v-pos                  .
           if        rr-mod-int           =    "I"
                     move   44            to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      rr-dat-ini           to   v-dat                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           move      v-dat                to   rr-dat-ini             .
      *                  *---------------------------------------------*
      *                  * Se premuta function-key 'EXIT' si esce      *
      *                  *---------------------------------------------*
           if        v-key                =    "EXIT"
                     move   "#"           to   OK
                     go to  acc-ric-sel-999.
      *                  *---------------------------------------------*
      *                  * Check su congruenza tra valore minimo e     *
      *                  * massimo                                     *
      *                  *---------------------------------------------*
           if        rr-dat-fin           not  = zero and
                     rr-dat-fin           <    rr-dat-ini
                     go to  acc-ric-sel-400.
      *                  *---------------------------------------------*
      *                  * Se premuta function-key 'UP' si ritorna     *
      *                  * all'impostazione precedente                 *
      *                  *---------------------------------------------*
           if        v-key                =    "UP  "
                     go to  acc-ric-sel-300.
      *                  *---------------------------------------------*
      *                  * Test se zero                                *
      *                  *---------------------------------------------*
           if        rr-dat-ini           =    zero and
                     rr-tip-vis           =    "P"
                     go to  acc-ric-sel-400.
      *                  *---------------------------------------------*
      *                  * Se premuta function-key 'DO' si esce        *
      *                  *---------------------------------------------*
           if        v-key                not  = "DO  "
                     go to  acc-ric-sel-480.
           move      spaces               to   OK                     .
           perform   cnt-tas-dox-000      thru cnt-tas-dox-999        .
           if        OK                   not  = spaces
                     move   spaces        to   OK
                     go to  acc-ric-sel-400
           else      go to  acc-ric-sel-999.
       acc-ric-sel-480.
      *                  *---------------------------------------------*
      *                  * Se tipo visualizzazione 'S' o 'E' a confer- *
      *                  * ma impostazioni                             *
      *                  *---------------------------------------------*
           if        rr-tip-vis           =    "S" or
                     rr-tip-vis           =    "E"
                     go to  acc-ric-sel-900.
       acc-ric-sel-500.
      *              *-------------------------------------------------*
      *              * Data di registrazione finale                    *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      ">"                  to   v-edm                  .
           move      15                   to   v-lin                  .
           move      30                   to   v-pos                  .
           if        rr-mod-int           =    "I"
                     move   44            to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      rr-dat-fin           to   v-dat                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           move      v-dat                to   rr-dat-fin             .
      *                  *---------------------------------------------*
      *                  * Se premuta function-key 'EXIT' si esce      *
      *                  *---------------------------------------------*
           if        v-key                =    "EXIT"
                     move   "#"           to   OK
                     go to  acc-ric-sel-999.
      *                  *---------------------------------------------*
      *                  * Check su congruenza tra valore minimo e     *
      *                  * massimo                                     *
      *                  *---------------------------------------------*
           if        rr-dat-fin           not  = zero and
                     rr-dat-fin           <    rr-dat-ini
                     go to  acc-ric-sel-500.
      *                  *---------------------------------------------*
      *                  * Controllo valore impostato                  *
      *                  *---------------------------------------------*
           move      spaces               to   OK                     .
           perform   cnt-dat-ief-000      thru cnt-dat-ief-999        .
           if        OK                   not  = spaces
                     move   spaces        to   OK
                     go to  acc-ric-sel-500.
      *                  *---------------------------------------------*
      *                  * Se premuta function-key 'UP  ' ritorna al-  *
      *                  * l'impostazione precedente                   *
      *                  *---------------------------------------------*
           if        v-key                =    "UP  "
                     go to  acc-ric-sel-400.
      *                  *---------------------------------------------*
      *                  * Se premuta function-key 'DO  ' si esce      *
      *                  *---------------------------------------------*
           if        v-key                =    "DO  "
                     go to  acc-ric-sel-999.
       acc-ric-sel-600.
      *              *-------------------------------------------------*
      *              * Se tipo visualizzazione 'P' : a conferma impo-  *
      *              * stazioni                                        *
      *              *-------------------------------------------------*
           if        rr-tip-vis           =    "P"
                     go to  acc-ric-sel-900.
      *              *-------------------------------------------------*
      *              * Selezione su causale contabile                  *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-zcc-ope      .
           move      rr-sel-cau           to   w-cod-mne-zcc-cod      .
           move      19                   to   w-cod-mne-zcc-lin      .
           move      30                   to   w-cod-mne-zcc-pos      .
           move      19                   to   w-cod-mne-zcc-dln      .
           move      36                   to   w-cod-mne-zcc-dps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           perform   cod-mne-zcc-cll-000  thru cod-mne-zcc-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-mne-zcc-foi-000  thru cod-mne-zcc-foi-999    .
       acc-ric-sel-602.
           perform   cod-mne-zcc-cll-000  thru cod-mne-zcc-cll-999    .
           if        w-cod-mne-zcc-ope    =    "F+"
                     go to acc-ric-sel-603.
           if        w-cod-mne-zcc-ope    =    "AC"
                     go to acc-ric-sel-604.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-ric-sel-603.
           perform   cod-mne-zcc-foi-000  thru cod-mne-zcc-foi-999    .
           go to     acc-ric-sel-602.
       acc-ric-sel-604.
           move      w-cod-mne-zcc-cod    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move   "#"           to   OK
                     go to  acc-ric-sel-999.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-sel-cau             .
       acc-ric-sel-630.
      *                  *---------------------------------------------*
      *                  * Lettura descrizione causale                 *
      *                  *---------------------------------------------*
           if        rr-sel-cau           =    zero
                     move   spaces        to   rf-zcc-des-cau
                     go to  acc-ric-sel-640.
      *                  *---------------------------------------------*
      *                  * Lettura                                     *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCAU    "         to   f-key                  .
           move      rr-sel-cau           to   rf-zcc-cod-cau         .
           move      "pgm/cge/fls/ioc/obj/iofzcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zcc                 .
           if        f-sts                not  = e-not-err
                     move   all "."       to   rf-zcc-des-cau         .
       acc-ric-sel-640.
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione causale         *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      36                   to   v-pos                  .
           move      rf-zcc-des-cau       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Se premuta function-key 'UP'                *
      *                  *---------------------------------------------*
           if        v-key                not  = "UP"
                     go to  acc-ric-sel-650.
           if        rr-sel-cau           =    zero or
                     f-sts                =    e-not-err
                     go to  acc-ric-sel-500
           else      go to  acc-ric-sel-600.
       acc-ric-sel-650.
      *                  *---------------------------------------------*
      *                  * Controllo valore impostato                  *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to  acc-ric-sel-600.
      *                  *---------------------------------------------*
      *                  * Se premuta function-key 'DO  ' si esce      *
      *                  *---------------------------------------------*
           if        v-key                =    "DO  "
                     go to  acc-ric-sel-999.
       acc-ric-sel-700.
      *              *-------------------------------------------------*
      *              * Se interrogazione per data si salta la selezio- *
      *              * ne su importo                                   *
      *              *-------------------------------------------------*
           if        rr-tip-int           =    "G" and
                     rr-mod-int           not  = "S"
                     go to  acc-ric-sel-750.
       acc-ric-sel-720.
      *              *-------------------------------------------------*
      *              * Selezione importo                               *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      13                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "<BG"                to   v-edm                  .
           move      20                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      rr-sel-imp           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           move      v-num                to   rr-sel-imp             .
      *                  *---------------------------------------------*
      *                  * Se premuta function-key 'EXIT' si esce      *
      *                  *---------------------------------------------*
           if        v-key                =    "EXIT"
                     move   "#"           to   OK
                     go to  acc-ric-sel-999.
      *                  *---------------------------------------------*
      *                  * Se premuta function-key 'UP'                *
      *                  *---------------------------------------------*
           if        v-key                =    "UP"
                     go to  acc-ric-sel-600.
      *                  *---------------------------------------------*
      *                  * Se premuta function-key 'DO  ' si esce      *
      *                  *---------------------------------------------*
           if        v-key                =    "DO  "
                     go to  acc-ric-sel-999.
       acc-ric-sel-740.
      *              *-------------------------------------------------*
      *              * Selezione su contenuto causale                  *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      21                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      rr-sel-cca           to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           move      v-alf                to   rr-sel-cca             .
      *                  *---------------------------------------------*
      *                  * Se premuta function-key 'EXIT' si esce      *
      *                  *---------------------------------------------*
           if        v-key                =    "EXIT"
                     move   "#"           to   OK
                     go to  acc-ric-sel-999.
      *                  *---------------------------------------------*
      *                  * Se premuta function-key 'UP'                *
      *                  *---------------------------------------------*
           if        v-key                =    "UP"
                     go to  acc-ric-sel-700.
      *                  *---------------------------------------------*
      *                  * Se premuta function-key 'DO  ' si esce      *
      *                  *---------------------------------------------*
           if        v-key                =    "DO  "
                     go to  acc-ric-sel-999.
       acc-ric-sel-750.
      *              *-------------------------------------------------*
      *              * Se interrogazione per data registrazione : a    *
      *              * conferma impostazioni                           *
      *              *-------------------------------------------------*
           if        rr-mod-int           not  = "I"
                     go to  acc-ric-sel-900.
      *              *-------------------------------------------------*
      *              * Selezione su utente                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "L"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      rr-sel-ute           to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           move      v-alf                to   rr-sel-ute             .
      *                  *---------------------------------------------*
      *                  * Se premuta function-key 'EXIT' si esce      *
      *                  *---------------------------------------------*
           if        v-key                =    "EXIT"
                     move   "#"           to   OK
                     go to  acc-ric-sel-999.
      *                  *---------------------------------------------*
      *                  * Se premuta function-key 'UP'                *
      *                  *---------------------------------------------*
           if        v-key                =    "UP"
                     go to  acc-ric-sel-600.
      *                  *---------------------------------------------*
      *                  * Se premuta function-key 'DO  ' si esce      *
      *                  *---------------------------------------------*
           if        v-key                =    "DO  "
                     go to  acc-ric-sel-999.
       acc-ric-sel-800.
      *              *-------------------------------------------------*
      *              * Selezione su fase                               *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "L"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      21                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      rr-sel-fas           to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           move      v-alf                to   rr-sel-fas             .
      *                  *---------------------------------------------*
      *                  * Se premuta function-key 'EXIT' si esce      *
      *                  *---------------------------------------------*
           if        v-key                =    "EXIT"
                     move   "#"           to   OK
                     go to  acc-ric-sel-999.
      *                  *---------------------------------------------*
      *                  * Se premuta function-key 'UP'                *
      *                  *---------------------------------------------*
           if        v-key                =    "UP"
                     go to  acc-ric-sel-750.
      *                  *---------------------------------------------*
      *                  * Se premuta function-key 'DO  ' si esce      *
      *                  *---------------------------------------------*
           if        v-key                =    "DO  "
                     go to  acc-ric-sel-999.
       acc-ric-sel-900.
      *              *-------------------------------------------------*
      *              * Richiesta conferma parametri impostati          *
      *              *-------------------------------------------------*
           move      "MX"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      "Tutto esatto (S/N/E) ?"
                                          to   v-not                  .
           move      "S"                  to   v-alf                  .
           move      "SNE"                to   v-msk                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "EXIT"               to   v-pfk (20)             .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Test su risposta dell'utente                *
      *                  *---------------------------------------------*
           if        v-key                not  = spaces
                     go to  acc-ric-sel-920.
           if        v-alf                =    "S"
                     move   "DO  "        to   v-key
                     go to  acc-ric-sel-999
           else if   v-alf                =    "E"
                     move   "EXIT"        to   v-key
                     move   "#"           to   OK
                     go to  acc-ric-sel-999
           else if   v-alf                =    "N"
                     go to  acc-ric-sel-000
           else      go to  acc-ric-sel-900.
       acc-ric-sel-920.
      *                      *-----------------------------------------*
      *                      * Se premuta function-key 'EXIT'          *
      *                      *-----------------------------------------*
           if        v-key                =    "EXIT"
                     move   "#"           to   OK
                     go to  acc-ric-sel-999.
      *                      *-----------------------------------------*
      *                      * Se premuta function-key 'UP  '          *
      *                      *-----------------------------------------*
           if        v-key                not  = "UP  "
                     go to  acc-ric-sel-999.
      *                          *-------------------------------------*
      *                          * Se interrogazione per conto         *
      *                          *-------------------------------------*
           if        rr-tip-int           =    "G" and
                     rr-mod-int           not  = "S"
                     go to  acc-ric-sel-940.
           if        rr-tip-vis           =    "S" or
                     rr-tip-vis           =    "E"
                     go to  acc-ric-sel-400
           else if   rr-tip-vis           =    "L"
                     go to  acc-ric-sel-700
           else if   rr-tip-vis           =    "P"
                     go to  acc-ric-sel-500.
       acc-ric-sel-940.
      *                          *-------------------------------------*
      *                          * Se interrogazione per data          *
      *                          *-------------------------------------*
           if        rr-mod-int           =    "R"
                     go to  acc-ric-sel-600
           else if   rr-mod-int           =    "I"
                     go to  acc-ric-sel-800.
       acc-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Accettazione Tipo visualizzazione per interrogazione su   *
      *    * conti 'di Generale'                                       *
      *    *-----------------------------------------------------------*
       acc-tvs-gen-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione della modalita' di in-   *
      *              * terrogazione                                    *
      *              *-------------------------------------------------*
           if        rr-mod-int           =    "S"
                     go to acc-tvs-gen-100
           else      go to acc-tvs-gen-500.
       acc-tvs-gen-100.
      *                  *---------------------------------------------*
      *                  * Preparazione eventuale default              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se il campo non e' a spaces : nessun    *
      *                      * default                                 *
      *                      *-----------------------------------------*
           if        rr-tip-vis           not  = spaces
                     go to acc-tvs-gen-120.
      *                      *-----------------------------------------*
      *                      * Se il default generale e' a spaces :    *
      *                      * default 'L'                             *
      *                      *-----------------------------------------*
           if        w-def-tip-vis        =    spaces
                     move  "L"            to   rr-tip-vis
                     go to acc-tvs-gen-120.
      *                      *-----------------------------------------*
      *                      * Test di compatibilta' per il default    *
      *                      *-----------------------------------------*
           if        w-def-tip-vis        not  = "S" and
                     w-def-tip-vis        not  = "L" and
                     w-def-tip-vis        not  = "P"
                     go to acc-tvs-gen-120.
           move      w-def-tip-vis        to   rr-tip-vis             .
       acc-tvs-gen-120.
      *                  *---------------------------------------------*
      *                  * Se modalita' per sottoconto                 *
      *                  *---------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tvs-ge1-lun    to   v-car                  .
           move      w-exp-tvs-ge1-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      "SLP"                to   v-msk                  .
           move      12                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-tvs-ge1-tbl    to   v-txt                  .
           if        rr-tip-vis           =    "S"
                     move  01             to   v-num
           else if   rr-tip-vis           =    "L"
                     move  02             to   v-num
           else if   rr-tip-vis           =    "P"
                     move  03             to   v-num
           else      move  zero           to   v-num                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *                  *---------------------------------------------*
      *                  * Se premuta function-key 'EXIT' si esce      *
      *                  *---------------------------------------------*
           if        v-key                =    "EXIT"
                     move   "#"           to   OK
                     go to  acc-tvs-gen-900.
      *                  *---------------------------------------------*
      *                  * Valore impostato in campo di destinazione   *
      *                  *---------------------------------------------*
           if        v-num                =    01
                     move  "S"            to   rr-tip-vis
           else if   v-num                =    02
                     move  "L"            to   rr-tip-vis
           else if   v-num                =    03
                     move  "P"            to   rr-tip-vis
           else      move  spaces         to   rr-tip-vis             .
      *                  *---------------------------------------------*
      *                  * Memorizzazione valore di default generale   *
      *                  *---------------------------------------------*
           move      rr-tip-vis           to   w-def-tip-vis          .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     acc-tvs-gen-900.
       acc-tvs-gen-500.
      *                  *---------------------------------------------*
      *                  * Preparazione eventuale default              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se il campo non e' a spaces : nessun    *
      *                      * default                                 *
      *                      *-----------------------------------------*
           if        rr-tip-vis           not  = spaces
                     go to acc-tvs-gen-520.
      *                      *-----------------------------------------*
      *                      * Se il default generale e' a spaces :    *
      *                      * default 'L'                             *
      *                      *-----------------------------------------*
           if        w-def-tip-vis        =    spaces
                     move  "L"            to   rr-tip-vis
                     go to acc-tvs-gen-520.
      *                      *-----------------------------------------*
      *                      * Test di compatibilta' per il default    *
      *                      *-----------------------------------------*
           if        w-def-tip-vis        not  = "T" and
                     w-def-tip-vis        not  = "L"
                     go to acc-tvs-gen-520.
           move      w-def-tip-vis        to   rr-tip-vis             .
       acc-tvs-gen-520.
      *                  *---------------------------------------------*
      *                  * Se modalita' per data registrazione o data  *
      *                  * immissione/ultima modifica                  *
      *                  *---------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tvs-ge2-lun    to   v-car                  .
           move      w-exp-tvs-ge2-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      "TL"                 to   v-msk                  .
           move      12                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-tvs-ge2-tbl    to   v-txt                  .
           if        rr-tip-vis           =    "T"
                     move  01             to   v-num
           else if   rr-tip-vis           =    "L"
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *                  *---------------------------------------------*
      *                  * Se premuta function-key 'EXIT' si esce      *
      *                  *---------------------------------------------*
           if        v-key                =    "EXIT"
                     move   "#"           to   OK
                     go to  acc-tvs-gen-900.
      *                  *---------------------------------------------*
      *                  * Valore impostato in campo di destinazione   *
      *                  *---------------------------------------------*
           if        v-num                =    01
                     move  "T"            to   rr-tip-vis
           else if   v-num                =    02
                     move  "L"            to   rr-tip-vis
           else      move  spaces         to   rr-tip-vis             .
      *                  *---------------------------------------------*
      *                  * Memorizzazione valore di default generale   *
      *                  *---------------------------------------------*
           move      rr-tip-vis           to   w-def-tip-vis          .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     acc-tvs-gen-900.
       acc-tvs-gen-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     acc-tvs-gen-999.
       acc-tvs-gen-999.
           exit.

      *    *===========================================================*
      *    * Accettazione Tipo visualizzazione per interrogazione su   *
      *    * conti 'Clienti' o 'Fornitori'                             *
      *    *-----------------------------------------------------------*
       acc-tvs-cof-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione eventuale default              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se il campo non e' a spaces : nessun    *
      *                      * default                                 *
      *                      *-----------------------------------------*
           if        rr-tip-vis           not  = spaces
                     go to acc-tvs-cof-100.
      *                      *-----------------------------------------*
      *                      * Se il default generale e' a spaces :    *
      *                      * default 'L'                             *
      *                      *-----------------------------------------*
           if        w-def-tip-vis        =    spaces
                     move  "L"            to   rr-tip-vis
                     go to acc-tvs-cof-100.
      *                      *-----------------------------------------*
      *                      * Test di compatibilta' per il default    *
      *                      *-----------------------------------------*
           if        w-def-tip-vis        not  = "S" and
                     w-def-tip-vis        not  = "L" and
                     w-def-tip-vis        not  = "E" and
                     w-def-tip-vis        not  = "P"
                     go to acc-tvs-cof-100.
           move      w-def-tip-vis        to   rr-tip-vis             .
       acc-tvs-cof-100.
      *              *-------------------------------------------------*
      *              * Accettazione                                    *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tvs-cof-lun    to   v-car                  .
           move      w-exp-tvs-cof-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      "SLEP"               to   v-msk                  .
           move      12                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-tvs-cof-tbl    to   v-txt                  .
           if        rr-tip-vis           =    "S"
                     move  01             to   v-num
           else if   rr-tip-vis           =    "L"
                     move  02             to   v-num
           else if   rr-tip-vis           =    "E"
                     move  03             to   v-num
           else if   rr-tip-vis           =    "P"
                     move  04             to   v-num
           else      move  zero           to   v-num                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-tvs-cof-200.
      *              *-------------------------------------------------*
      *              * Se premuta function-key 'EXIT' si esce          *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move   "#"           to   OK
                     go to  acc-tvs-cof-900.
       acc-tvs-cof-300.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        v-num                =    01
                     move  "S"            to   rr-tip-vis
           else if   v-num                =    02
                     move  "L"            to   rr-tip-vis
           else if   v-num                =    03
                     move  "E"            to   rr-tip-vis
           else if   v-num                =    04
                     move  "P"            to   rr-tip-vis
           else      move  spaces         to   rr-tip-vis             .
      *              *-------------------------------------------------*
      *              * Memorizzazione valore di default generale       *
      *              *-------------------------------------------------*
           move      rr-tip-vis           to   w-def-tip-vis          .
       acc-tvs-cof-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     acc-tvs-cof-999.
       acc-tvs-cof-999.
           exit.

      *    *===========================================================*
      *    * Editing del codice sottoconto con appoggio a sinistra     *
      *    *-----------------------------------------------------------*
       edt-pdc-asx-000.
      *              *-------------------------------------------------*
      *              * Determinazione se clausola blank when zero      *
      *              *-------------------------------------------------*
           move      zero                 to   w-edt-cod-pdc-c01      .
           inspect   w-edt-cod-pdc-edm
                                      tallying w-edt-cod-pdc-c01
                     for   all "B"                                    .
           if        w-edt-cod-pdc-c01    >    zero
                     move  "B"            to   w-edt-cod-pdc-bwz
           else      move  spaces         to   w-edt-cod-pdc-bwz      .
      *              *-------------------------------------------------*
      *              * Editing vero e proprio                          *
      *              *-------------------------------------------------*
           if        w-edt-cod-pdc-cod    =    zero  and
                     w-edt-cod-pdc-bwz    =    "B"
                     move  spaces         to   w-edt-cod-pdc-edt
           else      move  spaces         to   w-edt-cod-pdc-edt
                     if    w-edt-cod-pdc-liv
                                          =    2
                           move  w-edt-cod-pdc-cod
                                          to   w-edt-cod-pdc-s2l
                     else  move  w-edt-cod-pdc-cod
                                          to   w-edt-cod-pdc-s3l      .
       edt-pdc-asx-999.
           exit.

      *    *===========================================================*
      *    * Editing del codice sottoconto con appoggio a destra       *
      *    *-----------------------------------------------------------*
       edt-pdc-adx-000.
      *              *-------------------------------------------------*
      *              * Determinazione se clausola blank when zero      *
      *              *-------------------------------------------------*
           move      zero                 to   w-edt-cod-pdc-c01      .
           inspect   w-edt-cod-pdc-edm
                                      tallying w-edt-cod-pdc-c01
                     for   all "B"                                    .
           if        w-edt-cod-pdc-c01    >    zero
                     move  "B"            to   w-edt-cod-pdc-bwz
           else      move  spaces         to   w-edt-cod-pdc-bwz      .
      *              *-------------------------------------------------*
      *              * Editing vero e proprio                          *
      *              *-------------------------------------------------*
           if        w-edt-cod-pdc-cod    =    zero  and
                     w-edt-cod-pdc-bwz    =    "B"
                     move  spaces         to   w-edt-cod-pdc-edt
           else      move  spaces         to   w-edt-cod-pdc-edt
                     if    w-edt-cod-pdc-liv
                                          =    2
                           move  w-edt-cod-pdc-cod
                                          to   w-edt-cod-pdc-d2l
                     else  move  w-edt-cod-pdc-cod
                                          to   w-edt-cod-pdc-d3l      .
       edt-pdc-adx-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt e valore di data registrazione i-  *
      *    * niziale e finale                                          *
      *    *-----------------------------------------------------------*
       vis-dat-ief-000.
      *              *-------------------------------------------------*
      *              * Video in 'OFF'                                  *
      *              *-------------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Data registrazione iniziale                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Prompt                                      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Data registrazione iniziale:"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Valore                                      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      14                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-dat-ini           to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Data registrazione finale                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Prompt                                      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Data registrazione finale  :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Valore                                      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      15                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-dat-fin           to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Video in 'ON'                                   *
      *              *-------------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dat-ief-999.
           exit.

      *    *===========================================================*
      *    * Controlli per data iniziale e finale                      *
      *    *-----------------------------------------------------------*
       cnt-dat-ief-000.
      *              *-------------------------------------------------*
      *              * Se interrogazione per data : uscita             *
      *              *-------------------------------------------------*
           if        rr-tip-int           =    "G" and
                     rr-mod-int           not  = "S"
                     go to  cnt-dat-ief-999.
      *              *-------------------------------------------------*
      *              * Se interrogazione per conto                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se tipo visualizzazione 'L' : uscita        *
      *                  *---------------------------------------------*
           if        rr-tip-vis           =    "L"
                     go to  cnt-dat-ief-999.
      *                  *---------------------------------------------*
      *                  * Se tipo visualizzazione 'P'                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se zero                            *
      *                      *-----------------------------------------*
           if        rr-dat-fin           =    zero
                     go to  cnt-dat-ief-999.
      *                      *-----------------------------------------*
      *                      * Determinazione anno esercizio data ini- *
      *                      * ziale                                   *
      *                      *-----------------------------------------*
           move      rr-dat-ini           to   w-ese-cge-dtr          .
           perform   det-ese-cge-000      thru det-ese-cge-999        .
           move      w-ese-cge-esa        to   w-wrk-ese-ini          .
      *                      *-----------------------------------------*
      *                      * Determinazione anno esercizio data fi-  *
      *                      * nale                                    *
      *                      *-----------------------------------------*
           move      rr-dat-fin           to   w-ese-cge-dtr          .
           perform   det-ese-cge-000      thru det-ese-cge-999        .
           move      w-ese-cge-esa        to   w-wrk-ese-fin          .
      *                      *-----------------------------------------*
      *                      * Test di congruenza esercizio data ini-  *
      *                      * ziale e finale                          *
      *                      *-----------------------------------------*
           if        w-wrk-ese-ini        not  = w-wrk-ese-fin
                     go to  cnt-dat-ief-100
           else      go to  cnt-dat-ief-999.
       cnt-dat-ief-100.
      *                          *-------------------------------------*
      *                          * Messaggio di errore                 *
      *                          *-------------------------------------*
           move      "ME"                 to   v-ope                  .
           move      "Nel caso di visualizzazione 'Partitario', la data 
      -              "iniziale e quella finale devo-"
                                          to   v-nt1                  .
           move      "no appartenere allo stesso esercizio"
                                          to   v-nt2                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                          *-------------------------------------*
      *                          * Segnale di errore                   *
      *                          *-------------------------------------*
           move      "#"                  to   OK                     .
       cnt-dat-ief-999.
           exit.

      *    *===========================================================*
      *    * Controlli per tasto 'DO'                                  *
      *    *-----------------------------------------------------------*
       cnt-tas-dox-000.
      *              *-------------------------------------------------*
      *              * Se interrogazione di tipo 'G' e modalita' non   *
      *              * definita errore                                 *
      *              *-------------------------------------------------*
           if        rr-tip-int           =    "G" and
                     rr-mod-int           =    spaces
                     go to  cnt-tas-dox-900.
      *              *-------------------------------------------------*
      *              * Se interrogazione per conto                     *
      *              *-------------------------------------------------*
           if        rr-tip-int           =    "G" and
                     rr-mod-int           not  = "S"
                     go to  cnt-tas-dox-100.
      *                  *---------------------------------------------*
      *                  * Se codice archivio zero : errore            *
      *                  *---------------------------------------------*
           if        rr-cod-arc           =    zero
                     go to  cnt-tas-dox-900.
      *                  *---------------------------------------------*
      *                  * Se tipo visualizzazione 'P'                 *
      *                  *---------------------------------------------*
           if        rr-tip-vis           not  = "P"
                     go to  cnt-tas-dox-050.
      *                      *-----------------------------------------*
      *                      * Se data iniziale zero : errore          *
      *                      *-----------------------------------------*
           if        rr-dat-ini           =    zero
                     go to  cnt-tas-dox-900.
      *                      *-----------------------------------------*
      *                      * Controllo data finale                   *
      *                      *-----------------------------------------*
           move      spaces               to   OK                     .
           perform   cnt-dat-ief-000      thru cnt-dat-ief-999        .
           if        OK                   not  = spaces
                     go to  cnt-tas-dox-900.
           go to     cnt-tas-dox-999.
       cnt-tas-dox-050.
      *                  *---------------------------------------------*
      *                  * Se tipo visualizzazione 'S' e data iniziale *
      *                  * e' zero : errore                            *
      *                  *---------------------------------------------*
           if        rr-tip-vis           =    "S"     and
                     rr-dat-ini           =    zero
                     go to  cnt-tas-dox-900.
       cnt-tas-dox-100.
      *              *-------------------------------------------------*
      *              * Se interrogazione per data                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Uscita senza errore                         *
      *                  *---------------------------------------------*
           go to     cnt-tas-dox-999.
       cnt-tas-dox-900.
      *              *-------------------------------------------------*
      *              * Segnale di errore                               *
      *              *-------------------------------------------------*
           move      "#"                  to   OK                     .
       cnt-tas-dox-999.
           exit.
       
      *    *===========================================================*
      *    * Determinazione anno di esercizio per contabilita'         *
      *    *-----------------------------------------------------------*
       det-ese-cge-000.
      *              *-------------------------------------------------*
      *              * Determinazione anno di esercizio                *
      *              *-------------------------------------------------*
           move      w-ese-cge-dtr        to   s-dat                  .
           if        w-ese-cge-mce        =    12         or
                     s-mes                >    w-ese-cge-mce
                     move  s-saa          to   w-ese-cge-esa
                     go to det-ese-cge-500.
           subtract  1                    from s-saa                  .
           move      "NS"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-saa                to   w-ese-cge-esa          .
       det-ese-cge-500.
      *              *-------------------------------------------------*
      *              * Determinazione mese di esercizio                *
      *              *-------------------------------------------------*
           move      s-mes                to   w-ese-cge-esm          .
           if        w-ese-cge-mce        =    12
                     go to det-ese-cge-999.
           if        s-mes                >    w-ese-cge-mce
                     subtract w-ese-cge-mce
                                          from w-ese-cge-esm
                     go to det-ese-cge-999.
           if        s-mes                <    w-ese-cge-mce
                     add   12             to   w-ese-cge-esm
                     subtract w-ese-cge-mce
                                          from w-ese-cge-esm
                     go to det-ese-cge-999.
           move      12                   to   w-ese-cge-esm          .
       det-ese-cge-999.
           exit.

      *    *===========================================================*
      *    * Regolarizzazione richieste per interrogazione             *
      *    *-----------------------------------------------------------*
       reg-ric-sel-000.
      *              *-------------------------------------------------*
      *              * Tipo visualizzazione                            *
      *              *-------------------------------------------------*
           if        rr-tip-vis           not  = spaces
                     go to reg-ric-sel-100.
           if        w-def-tip-vis        =    spaces
                     move   "L"           to   rr-tip-vis
           else      move   w-def-tip-vis to   rr-tip-vis             .
       reg-ric-sel-100.
      *              *-------------------------------------------------*
      *              * Data registrazione iniziale                     *
      *              *-------------------------------------------------*
           if       (rr-tip-vis           =    "S" or
                     rr-tip-vis           =    "E"  ) and
                     rr-dat-ini           =    zero
                     move   w-wrk-dat-att to   rr-dat-ini             .
      *              *-------------------------------------------------*
      *              * Data registrazione finale                       *
      *              *-------------------------------------------------*
           if        rr-dat-fin           not  = zero
                     go to  reg-ric-sel-999.
      *                  *---------------------------------------------*
      *                  * Test se richiesto                           *
      *                  *---------------------------------------------*
           if        rr-tip-vis           =    "S" or
                     rr-tip-vis           =    "E"
                     go to  reg-ric-sel-999.
      *                  *---------------------------------------------*
      *                  * Regolarizzazione                            *
      *                  *---------------------------------------------*
           if        rr-dat-ini           =    zero
                     move   9991231       to   rr-dat-fin
           else      move   rr-dat-ini    to   rr-dat-fin             .
       reg-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Esecuzione programma di interrogazione                    *
      *    *-----------------------------------------------------------*
       ese-prg-int-000.
      *              *-------------------------------------------------*
      *              * Se interrogazione per conto                     *
      *              *-------------------------------------------------*
           if        rr-tip-int           =    "G" and
                     rr-mod-int           not  = "S"
                     go to  ese-prg-int-500.
      *                  *---------------------------------------------*
      *                  * Se tipo visualizzazione 'L'                 *
      *                  *---------------------------------------------*
           if        rr-tip-vis           not  = "L"
                     go to  ese-prg-int-100.
      *                      *-----------------------------------------*
      *                      * Se tipo interrogazione 'G'              *
      *                      *-----------------------------------------*
           if        rr-tip-int           not  = "G"
                     go to  ese-prg-int-020.
      *                          *-------------------------------------*
      *                          * Richiamo programma interessato      *
      *                          *-------------------------------------*
           call      "pgm/cge/prg/obj/pcge3011"
                                         using rr                     .
           cancel    "pgm/cge/prg/obj/pcge3011"                       .
           go to     ese-prg-int-999.
       ese-prg-int-020.
      *                      *-----------------------------------------*
      *                      * Se tipo interrogazione 'C'              *
      *                      *-----------------------------------------*
           if        rr-tip-int           not  = "C"
                     go to  ese-prg-int-040.
      *                          *-------------------------------------*
      *                          * Richiamo programma interessato      *
      *                          *-------------------------------------*
           call      "pgm/cge/prg/obj/pcge3012"
                                         using rr                     .
           cancel    "pgm/cge/prg/obj/pcge3012"                       .
           go to     ese-prg-int-999.
       ese-prg-int-040.
      *                      *-----------------------------------------*
      *                      * Se tipo interrogazione 'F'              *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Richiamo programma interessato      *
      *                          *-------------------------------------*
           call      "pgm/cge/prg/obj/pcge3013"
                                         using rr                     .
           cancel    "pgm/cge/prg/obj/pcge3013"                       .
           go to     ese-prg-int-999.
       ese-prg-int-100.
      *                  *---------------------------------------------*
      *                  * Se tipo visualizzazione 'P'                 *
      *                  *---------------------------------------------*
           if        rr-tip-vis           not  = "P"
                     go to  ese-prg-int-200.
      *                      *-----------------------------------------*
      *                      * Se tipo interrogazione 'G'              *
      *                      *-----------------------------------------*
           if        rr-tip-int           not  = "G"
                     go to  ese-prg-int-120.
      *                          *-------------------------------------*
      *                          * Richiamo programma interessato      *
      *                          *-------------------------------------*
           call      "pgm/cge/prg/obj/pcge3014"
                                         using rr                     .
           cancel    "pgm/cge/prg/obj/pcge3014"                       .
           go to     ese-prg-int-999.
       ese-prg-int-120.
      *                      *-----------------------------------------*
      *                      * Se tipo interrogazione 'C'              *
      *                      *-----------------------------------------*
           if        rr-tip-int           not  = "C"
                     go to  ese-prg-int-140.
      *                          *-------------------------------------*
      *                          * Richiamo programma interessato      *
      *                          *-------------------------------------*
           call      "pgm/cge/prg/obj/pcge3015"
                                         using rr                     .
           cancel    "pgm/cge/prg/obj/pcge3015"                       .
           go to     ese-prg-int-999.
       ese-prg-int-140.
      *                      *-----------------------------------------*
      *                      * Se tipo interrogazione 'F'              *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Richiamo programma interessato      *
      *                          *-------------------------------------*
           call      "pgm/cge/prg/obj/pcge3016"
                                         using rr                     .
           cancel    "pgm/cge/prg/obj/pcge3016"                       .
           go to     ese-prg-int-999.
       ese-prg-int-200.
      *                  *---------------------------------------------*
      *                  * Se tipo visualizzazione 'S'                 *
      *                  *---------------------------------------------*
           if        rr-tip-vis           not  = "S"
                     go to  ese-prg-int-300.
      *                      *-----------------------------------------*
      *                      * Se tipo interrogazione 'G'              *
      *                      *-----------------------------------------*
           if        rr-tip-int           not  = "G"
                     go to  ese-prg-int-220.
      *                          *-------------------------------------*
      *                          * Richiamo programma interessato      *
      *                          *-------------------------------------*
           call      "pgm/cge/prg/obj/pcge301a"
                                         using rr                     .
           cancel    "pgm/cge/prg/obj/pcge301a"                       .
           go to     ese-prg-int-999.
       ese-prg-int-220.
      *                      *-----------------------------------------*
      *                      * Se tipo interrogazione 'C'              *
      *                      *-----------------------------------------*
           if        rr-tip-int           not  = "C"
                     go to  ese-prg-int-240.
      *                          *-------------------------------------*
      *                          * Richiamo programma interessato      *
      *                          *-------------------------------------*
           call      "pgm/cge/prg/obj/pcge301b"
                                         using rr                     .
           cancel    "pgm/cge/prg/obj/pcge301b"                       .
           go to     ese-prg-int-999.
       ese-prg-int-240.
      *                      *-----------------------------------------*
      *                      * Se tipo interrogazione 'F'              *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Richiamo programma interessato      *
      *                          *-------------------------------------*
           call      "pgm/cge/prg/obj/pcge301c"
                                         using rr                     .
           cancel    "pgm/cge/prg/obj/pcge301c"                       .
           go to     ese-prg-int-999.
       ese-prg-int-300.
      *                  *---------------------------------------------*
      *                  * Se tipo visualizzazione 'E'                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se tipo interrogazione 'C'              *
      *                      *-----------------------------------------*
           if        rr-tip-int           not  = "C"
                     go to  ese-prg-int-320.
      *                          *-------------------------------------*
      *                          * Richiamo programma interessato      *
      *                          *-------------------------------------*
           call      "pgm/cge/prg/obj/pcge301e"
                                         using rr                     .
           cancel    "pgm/cge/prg/obj/pcge301e"                       .
           go to     ese-prg-int-999.
       ese-prg-int-320.
      *                      *-----------------------------------------*
      *                      * Se tipo interrogazione 'F'              *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Richiamo programma interessato      *
      *                          *-------------------------------------*
           call      "pgm/cge/prg/obj/pcge301f"
                                         using rr                     .
           cancel    "pgm/cge/prg/obj/pcge301f"                       .
           go to     ese-prg-int-999.
       ese-prg-int-500.
      *              *-------------------------------------------------*
      *              * Se interrogazione per data                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se interrogazione per data registrazione    *
      *                  *---------------------------------------------*
           if        rr-mod-int           =    "I"
                     go to  ese-prg-int-600.
      *                      *-----------------------------------------*
      *                      * Richiamo programma interessato          *
      *                      *-----------------------------------------*
           call      "pgm/cge/prg/obj/pcge3018"
                                         using rr                     .
           cancel    "pgm/cge/prg/obj/pcge3018"                       .
           go to     ese-prg-int-999.
       ese-prg-int-600.
      *                  *---------------------------------------------*
      *                  * Se interrogazione per data immissione/ulti- *
      *                  * ma modifica                                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Richiamo programma interessato          *
      *                      *-----------------------------------------*
           call      "pgm/cge/prg/obj/pcge3019"
                                         using rr                     .
           cancel    "pgm/cge/prg/obj/pcge3019"                       .
       ese-prg-int-999.
           exit.

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice sottoconto      *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/acmnpdc0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice cliente         *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/acmncli0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice fornitore       *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/acmnfnt0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione della causale contabile    *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/acmnzcc0.acs"                   .
