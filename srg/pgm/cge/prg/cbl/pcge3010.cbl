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
      *                       Ultima revisione:    NdK del 12/03/18    *
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
      *    cge3r1          - pcge3011 : Lista movimenti di generale    *
      *                    - pcge3012 : Lista movimenti clienti        *
      *                    - pcge3013 : Lista movimenti fornitori      *
      *    cge3r4          - pcge3014 : Partitario di generale         *
      *                    - pcge3015 : Partitario clienti             *
      *                    - pcge3016 : Partitario fornitori           *
      *    cge3ra          - pcge301a : Saldo alla data sottoconto     *
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

       Source-Computer.     w-i-p-NdK-PD .
       Object-Computer.     w-i-p-NdK-PD .

       Special-Names.       Decimal-Point is comma .

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
      *    * Area di comunicazione per modulo                "msegrt"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/s"                                  .

      *    *===========================================================*
      *    * Area per definizione codici di errore di i-o              *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/e"                                  .

      *    *===========================================================*
      *    * Area di definizione della valuta base                     *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/c"                                  .

      *    *===========================================================*
      *    * Work-area di controllo                                    *
      *    *-----------------------------------------------------------*
       01  w-cnt.
      *        *-------------------------------------------------------*
      *        * Flags di controllo uscita da routines fondamentali    *
      *        *-------------------------------------------------------*
           05  w-cnt-flg.
      *            *---------------------------------------------------*
      *            * Per routine dic-ini-pgm-000                       *
      *            *---------------------------------------------------*
               10  w-cnt-dic-ini-pgm      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Per routine pre-exe-pgm-000                       *
      *            *---------------------------------------------------*
               10  w-cnt-pre-exe-pgm      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Per routine rou-opn-fls-000                       *
      *            *---------------------------------------------------*
               10  w-cnt-rou-opn-fls      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Flags di tipo uscita da routines di accettazione      *
      *        *-------------------------------------------------------*
           05  w-cnt-acc.
      *            *---------------------------------------------------*
      *            * Da accettazione campi richieste                   *
      *            *---------------------------------------------------*
               10  w-cnt-acc-ric-sel      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Flags di uscita da controlli su tasto Do              *
      *        *-------------------------------------------------------*
           05  w-cnt-tdo.
      *            *---------------------------------------------------*
      *            * Per tasto Do su campi richieste                   *
      *            *---------------------------------------------------*
               10  w-cnt-tdo-ric-flg      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Flags di controllo su status impostazioni             *
      *        *-------------------------------------------------------*
           05  w-cnt-sts-imp.
      *            *---------------------------------------------------*
      *            * Impostazione richieste                            *
      *            *---------------------------------------------------*
               10  w-cnt-sts-imp-ric      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Flags di controllo su status visualizzazione prompts  *
      *        *-------------------------------------------------------*
           05  w-cnt-sts-pmt.
      *            *---------------------------------------------------*
      *            * Visualizzazione prompts richieste                 *
      *            *---------------------------------------------------*
               10  w-cnt-sts-pmt-ric      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Flags di controllo su status visualizzazione dati     *
      *        *-------------------------------------------------------*
           05  w-cnt-sts-vis.
      *            *---------------------------------------------------*
      *            * Visualizzazione dati richieste                    *
      *            *---------------------------------------------------*
               10  w-cnt-sts-vis-ric      pic  x(01)                  .

      *    *===========================================================*
      *    * Record files                                              *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [pdc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfpdc"                          .
      *        *-------------------------------------------------------*
      *        * [cli]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfcli"                          .
      *        *-------------------------------------------------------*
      *        * [fnt]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rffnt"                          .
      *        *-------------------------------------------------------*
      *        * [zcc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfzcc"                          .

      *    *===========================================================*
      *    * Link-area richieste per interrogazione                    *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/pcge3010.pgl"                   .

      *    *===========================================================*
      *    * Work-area per lettura personalizzazioni                   *
      *    *-----------------------------------------------------------*
       01  w-prs.
      *        *-------------------------------------------------------*
      *        * Numero livelli del piano dei conti                    *
      *        *-------------------------------------------------------*
           05  w-prs-liv-pdc              pic  9(01)                  .

      *    *===========================================================*
      *    * Work per subroutines di Let                               *
      *    *-----------------------------------------------------------*
       01  w-let.
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [pdc]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-pdc.
               10  w-let-arc-pdc-flg      pic  x(01)                  .
               10  w-let-arc-pdc-cod      pic  9(07)                  .
               10  w-let-arc-pdc-des      pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [cli]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-cli.
               10  w-let-arc-cli-flg      pic  x(01)                  .
               10  w-let-arc-cli-cod      pic  9(07)                  .
               10  w-let-arc-cli-rag      pic  x(40)                  .
               10  w-let-arc-cli-via      pic  x(40)                  .
               10  w-let-arc-cli-loc      pic  x(40)                  .
               10  w-let-arc-cli-cge      pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [fnt]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-fnt.
               10  w-let-arc-fnt-flg      pic  x(01)                  .
               10  w-let-arc-fnt-cod      pic  9(07)                  .
               10  w-let-arc-fnt-rag      pic  x(40)                  .
               10  w-let-arc-fnt-via      pic  x(40)                  .
               10  w-let-arc-fnt-loc      pic  x(40)                  .
               10  w-let-arc-fnt-cge      pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su tabella [zcc]                         *
      *        *-------------------------------------------------------*
           05  w-let-arc-zcc.
               10  w-let-arc-zcc-flg      pic  x(01)                  .
               10  w-let-arc-zcc-cod      pic  9(03)                  .
               10  w-let-arc-zcc-des      pic  x(30)                  .
               10  w-let-arc-zcc-tmi      pic  x(01)                  .
               10  w-let-arc-zcc-snb      pic  x(01)                  .

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
                            "Sottoconto contabile                    ".
                   15  filler             pic  x(40) value
                            "Data di registrazione                   ".
                   15  filler             pic  x(40) value
                            "data Inserimento o ultima modifica      ".
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
                            "solo il Totale movimenti      "          .
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
      *        * Data attuale                                          *
      *        *-------------------------------------------------------*
           05  w-def-dat-att              pic  9(07)                  .
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
      *    * Work per subroutines di editing codice sottoconto         *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/wedtpdc0.wkl"                   .

      *    *===========================================================*
      *    * Work per subroutines di Err                               *
      *    *-----------------------------------------------------------*
       01  w-err.
      *        *-------------------------------------------------------*
      *        * Work per Err con box centrale                         *
      *        *-------------------------------------------------------*
           05  w-err-box-err.
               10  w-err-box-err-msg      pic  x(65)                  .
               10  w-err-box-err-m02      pic  x(65)                  .
               10  w-err-box-err-m03      pic  x(65)                  .

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
           move      spaces               to   w-cnt-dic-ini-pgm      .
           perform   dic-ini-pgm-000      thru dic-ini-pgm-999        .
           if        w-cnt-dic-ini-pgm    not  = spaces
                     go to main-999.
       main-100.
      *              *-------------------------------------------------*
      *              * Esecuzione routine pre-esecuzione programma     *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-pre-exe-pgm      .
           perform   pre-exe-pgm-000      thru pre-exe-pgm-999        .
           if        w-cnt-pre-exe-pgm    not  = spaces
                     go to main-900.
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-rou-opn-fls      .
           perform   rou-opn-fls-000      thru rou-opn-fls-999        .
           if        w-cnt-rou-opn-fls    not  = spaces
                     go to main-900.
      *              *-------------------------------------------------*
      *              * Normalizzazione parametri di selezione          *
      *              *-------------------------------------------------*
           perform   nor-ric-sel-000      thru nor-ric-sel-999        .
       main-200.
      *              *-------------------------------------------------*
      *              * Accettazione record richieste                   *
      *              *-------------------------------------------------*
           perform   acc-ric-sel-000      thru acc-ric-sel-999        .
      *                  *---------------------------------------------*
      *                  * Se uscita per Exit                          *
      *                  *---------------------------------------------*
           if        w-cnt-acc-ric-sel    =    "E"
                     go to main-900.
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
                     move  "#"            to   w-cnt-dic-ini-pgm      .
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
      *              * [pdc]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofpdc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdc                 .
      *              *-------------------------------------------------*
      *              * [cli]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofcli"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cli                 .
      *              *-------------------------------------------------*
      *              * [fnt]                                           *
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
      *              * [pdc]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofpdc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdc                 .
      *              *-------------------------------------------------*
      *              * [cli]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofcli"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cli                 .
      *              *-------------------------------------------------*
      *              * [fnt]                                           *
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
           move      s-dat                to   w-def-dat-att          .
      *              *-------------------------------------------------*
      *              * Preparazione defaults generali                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-def-tip-int          .
           move      spaces               to   w-def-mod-int          .
           move      "L"                  to   w-def-tip-vis          .
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
      *    * Accettazione richieste di selezione                       *
      *    *-----------------------------------------------------------*
       acc-ric-sel-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-acc-ric-sel      .
      *              *-------------------------------------------------*
      *              * Normalizzazione status impostazione             *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-sts-imp-ric      .
      *              *-------------------------------------------------*
      *              * Normalizzazione status visualizzazione prompts  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-sts-pmt-ric      .
      *              *-------------------------------------------------*
      *              * Normalizzazione status visualizzazione dati     *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-sts-vis-ric      .
      *              *-------------------------------------------------*
      *              * Visualizzazione prompts                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Video in 'OFF'                              *
      *                  *---------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione titolo programma            *
      *                  *---------------------------------------------*
           perform   vis-tit-pgm-000      thru vis-tit-pgm-999        .
      *                  *---------------------------------------------*
      *                  * Prompts per richieste di selezione          *
      *                  *---------------------------------------------*
           perform   pmt-ric-sel-000      thru pmt-ric-sel-999        .
           move      "#"                  to   w-cnt-sts-pmt-ric      .
      *                  *---------------------------------------------*
      *                  * Video in 'ON'                               *
      *                  *---------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-ric-sel-100.
      *              *-------------------------------------------------*
      *              * Accettazioni                                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione func-key di impostazione    *
      *                  *---------------------------------------------*
           move      spaces               to   v-key                  .
      *                  *---------------------------------------------*
      *                  * Tipo interrogazione                         *
      *                  *---------------------------------------------*
           perform   acc-tip-int-000      thru acc-tip-int-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
       acc-ric-sel-150.
      *                  *---------------------------------------------*
      *                  * Modalita' di interrogazione                 *
      *                  *---------------------------------------------*
           perform   acc-mod-int-000      thru acc-mod-int-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-100.
       acc-ric-sel-200.
      *                  *---------------------------------------------*
      *                  * Codice archivio                             *
      *                  *---------------------------------------------*
           perform   acc-cod-arc-000      thru acc-cod-arc-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-150.
       acc-ric-sel-250.
      *                  *---------------------------------------------*
      *                  * Tipo visualizzazione                        *
      *                  *---------------------------------------------*
           perform   acc-tip-vis-000      thru acc-tip-vis-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-200.
       acc-ric-sel-300.
      *                  *---------------------------------------------*
      *                  * Data registrazione iniziale                 *
      *                  *---------------------------------------------*
           perform   acc-dat-ini-000      thru acc-dat-ini-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-250.
       acc-ric-sel-350.
      *                  *---------------------------------------------*
      *                  * Data registrazione finale                   *
      *                  *---------------------------------------------*
           perform   acc-dat-fin-000      thru acc-dat-fin-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-300.
       acc-ric-sel-400.
      *                  *---------------------------------------------*
      *                  * Causale contabile                           *
      *                  *---------------------------------------------*
           perform   acc-sel-cau-000      thru acc-sel-cau-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-350.
       acc-ric-sel-450.
      *                  *---------------------------------------------*
      *                  * Contenuto causale                           *
      *                  *---------------------------------------------*
           perform   acc-sel-cca-000      thru acc-sel-cca-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-400.
       acc-ric-sel-500.
      *                  *---------------------------------------------*
      *                  * Contenuto commento                          *
      *                  *---------------------------------------------*
           perform   acc-sel-cco-000      thru acc-sel-cco-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-450.
       acc-ric-sel-550.
      *                  *---------------------------------------------*
      *                  * Importo min                                 *
      *                  *---------------------------------------------*
           perform   acc-sel-imi-000      thru acc-sel-imi-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-500.
       acc-ric-sel-575.
      *                  *---------------------------------------------*
      *                  * Importo max                                 *
      *                  *---------------------------------------------*
           perform   acc-sel-ima-000      thru acc-sel-ima-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-550.
       acc-ric-sel-600.
      *                  *---------------------------------------------*
      *                  * Utente                                      *
      *                  *---------------------------------------------*
           perform   acc-sel-ute-000      thru acc-sel-ute-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-550.
       acc-ric-sel-650.
      *                  *---------------------------------------------*
      *                  * Fase                                        *
      *                  *---------------------------------------------*
           perform   acc-sel-fas-000      thru acc-sel-fas-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-600.
       acc-ric-sel-900.
      *              *-------------------------------------------------*
      *              * Flag di controllo status impostazioni           *
      *              *-------------------------------------------------*
           move      "#"                  to   w-cnt-sts-imp-ric      .
      *              *-------------------------------------------------*
      *              * Flag di controllo status visualizzazione        *
      *              *-------------------------------------------------*
           move      "#"                  to   w-cnt-sts-vis-ric      .
      *              *-------------------------------------------------*
      *              * Conferma impostazioni                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Cancellazione eventuali note operative      *
      *                  *---------------------------------------------*
           move      "NT"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-ric-sel-910.
      *                  *---------------------------------------------*
      *                  * Accettazione conferma                       *
      *                  *---------------------------------------------*
           move      "MX"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      "#CNF"               to   v-not                  .
           move      "S"                  to   v-alf                  .
           move      "SNE"                to   v-msk                  .
           move      "DO  "               to   v-pfk (05)             .
           move      "UP  "               to   v-pfk (01)             .
           move      "EXIT"               to   v-pfk (20)             .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           if        v-key                not  = spaces
                     go to acc-ric-sel-920.
           if        v-alf                =    "S"
                     move   "DO  "        to   v-key
           else if   v-alf                =    "E"
                     move   "EXIT"        to   v-key
           else if   v-alf                =    "N"
                     move   "UP  "        to   v-key                  .
       acc-ric-sel-920.
      *                  *---------------------------------------------*
      *                  * Test su risposta dell'utente                *
      *                  *---------------------------------------------*
           if        v-key                =    "DO  "
                     go to acc-ric-sel-930
           else if   v-key                =    "EXIT"
                     go to acc-ric-sel-940
           else if   v-key                =    "UP  "
                     go to acc-ric-sel-950
           else      go to acc-ric-sel-910.
       acc-ric-sel-930.
      *                  *---------------------------------------------*
      *                  * Se Do                                       *
      *                  *---------------------------------------------*
           perform   tdo-ric-sel-000      thru tdo-ric-sel-999        .
           if        w-cnt-tdo-ric-flg    =    spaces
                     move  "S"            to   w-cnt-acc-ric-sel
                     go to acc-ric-sel-999
           else      move  spaces         to   w-cnt-tdo-ric-flg
                     go to acc-ric-sel-900.
       acc-ric-sel-940.
      *                  *---------------------------------------------*
      *                  * Se Exit                                     *
      *                  *---------------------------------------------*
           move      "E"                  to   w-cnt-acc-ric-sel      .
           go to     acc-ric-sel-999.
       acc-ric-sel-950.
      *                  *---------------------------------------------*
      *                  * Se Up                                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Ad accettazioni                         *
      *                      *-----------------------------------------*
           go to     acc-ric-sel-100.
       acc-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt richieste                          *
      *    *-----------------------------------------------------------*
       pmt-ric-sel-000.
      *              *-------------------------------------------------*
      *              * Tipo interrogazione                             *
      *              *-------------------------------------------------*
           perform   pmt-tip-int-000      thru pmt-tip-int-999        .
      *              *-------------------------------------------------*
      *              * Modalita' di interrogazione                     *
      *              *-------------------------------------------------*
           perform   pmt-mod-int-000      thru pmt-mod-int-999        .
      *              *-------------------------------------------------*
      *              * Codice archivio                                 *
      *              *-------------------------------------------------*
           perform   pmt-cod-arc-000      thru pmt-cod-arc-999        .
      *              *-------------------------------------------------*
      *              * Tipo visualizzazione                            *
      *              *-------------------------------------------------*
           perform   pmt-tip-vis-000      thru pmt-tip-vis-999        .
      *              *-------------------------------------------------*
      *              * Data registrazione iniziale                     *
      *              *-------------------------------------------------*
           perform   pmt-dat-ini-000      thru pmt-dat-ini-999        .
      *              *-------------------------------------------------*
      *              * Data registrazione finale                       *
      *              *-------------------------------------------------*
           perform   pmt-dat-fin-000      thru pmt-dat-fin-999        .
      *              *-------------------------------------------------*
      *              * Selezioni                                       *
      *              *-------------------------------------------------*
           perform   pmt-sel-gen-000      thru pmt-sel-gen-999        .
      *              *-------------------------------------------------*
      *              * Causale contabile                               *
      *              *-------------------------------------------------*
           perform   pmt-sel-cau-000      thru pmt-sel-cau-999        .
      *              *-------------------------------------------------*
      *              * Contenuto causale                               *
      *              *-------------------------------------------------*
           perform   pmt-sel-cca-000      thru pmt-sel-cca-999        .
      *              *-------------------------------------------------*
      *              * Contenuto commento                              *
      *              *-------------------------------------------------*
           perform   pmt-sel-cco-000      thru pmt-sel-cco-999        .
      *              *-------------------------------------------------*
      *              * Importo min                                     *
      *              *-------------------------------------------------*
           perform   pmt-sel-imi-000      thru pmt-sel-imi-999        .
      *              *-------------------------------------------------*
      *              * Importo                                         *
      *              *-------------------------------------------------*
           perform   pmt-sel-ima-000      thru pmt-sel-ima-999        .
      *              *-------------------------------------------------*
      *              * Utente                                          *
      *              *-------------------------------------------------*
           perform   pmt-sel-ute-000      thru pmt-sel-ute-999        .
      *              *-------------------------------------------------*
      *              * Fase                                            *
      *              *-------------------------------------------------*
           perform   pmt-sel-fas-000      thru pmt-sel-fas-999        .
       pmt-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione richieste                                 *
      *    *-----------------------------------------------------------*
       vis-ric-sel-000.
      *              *-------------------------------------------------*
      *              * Tipo interrogazione                             *
      *              *-------------------------------------------------*
           perform   vis-tip-int-000      thru vis-tip-int-999        .
      *              *-------------------------------------------------*
      *              * Modalita' di interrogazione                     *
      *              *-------------------------------------------------*
           perform   vis-mod-int-000      thru vis-mod-int-999        .
      *              *-------------------------------------------------*
      *              * Codice archivio                                 *
      *              *-------------------------------------------------*
           perform   vis-cod-arc-000      thru vis-cod-arc-999        .
           perform   vis-cod-arc-des-000  thru vis-cod-arc-des-999    .
      *              *-------------------------------------------------*
      *              * Tipo visualizzazione                            *
      *              *-------------------------------------------------*
           perform   vis-tip-vis-000      thru vis-tip-vis-999        .
      *              *-------------------------------------------------*
      *              * Data registrazione iniziale                     *
      *              *-------------------------------------------------*
           perform   vis-dat-ini-000      thru vis-dat-ini-999        .
      *              *-------------------------------------------------*
      *              * Data registrazione finale                       *
      *              *-------------------------------------------------*
           perform   vis-dat-fin-000      thru vis-dat-fin-999        .
      *              *-------------------------------------------------*
      *              * Causale contabile                               *
      *              *-------------------------------------------------*
           perform   vis-sel-cau-000      thru vis-sel-cau-999        .
           perform   vis-sel-cau-des-000  thru vis-sel-cau-des-999    .
      *              *-------------------------------------------------*
      *              * Contenuto causale                               *
      *              *-------------------------------------------------*
           perform   vis-sel-cca-000      thru vis-sel-cca-999        .
      *              *-------------------------------------------------*
      *              * Contenuto commento                              *
      *              *-------------------------------------------------*
           perform   vis-sel-cco-000      thru vis-sel-cco-999        .
      *              *-------------------------------------------------*
      *              * Importo min                                     *
      *              *-------------------------------------------------*
           perform   vis-sel-imi-000      thru vis-sel-imi-999        .
      *              *-------------------------------------------------*
      *              * Importo max                                     *
      *              *-------------------------------------------------*
           perform   vis-sel-ima-000      thru vis-sel-ima-999        .
      *              *-------------------------------------------------*
      *              * Utente                                          *
      *              *-------------------------------------------------*
           perform   vis-sel-ute-000      thru vis-sel-ute-999        .
      *              *-------------------------------------------------*
      *              * Fase                                            *
      *              *-------------------------------------------------*
           perform   vis-sel-fas-000      thru vis-sel-fas-999        .
       vis-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per Tipo interrogazione           *
      *    *-----------------------------------------------------------*
       pmt-tip-int-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tipo movimenti             :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-tip-int-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per Modalita' di interrogazione   *
      *    *-----------------------------------------------------------*
       pmt-mod-int-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo interrogazione  *
      *              *-------------------------------------------------*
           if        rr-tip-int           =    spaces
                     go to pmt-mod-int-800
           else if   rr-tip-int           =    "G"
                     go to pmt-mod-int-100
           else      go to pmt-mod-int-800.
       pmt-mod-int-100.
      *              *-------------------------------------------------*
      *              * Se movimenti di Generale                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione prompt                      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Interrogazione         per :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     pmt-mod-int-999.
       pmt-mod-int-800.
      *              *-------------------------------------------------*
      *              * In tutti gli altri casi                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione prompt                      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     pmt-mod-int-999.
       pmt-mod-int-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per Codice archivio               *
      *    *-----------------------------------------------------------*
       pmt-cod-arc-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo interrogazione  *
      *              *-------------------------------------------------*
           if        rr-tip-int           =    spaces
                     go to pmt-cod-arc-800
           else if   rr-tip-int           =    "G"
                     go to pmt-cod-arc-100
           else if   rr-tip-int           =    "C"
                     go to pmt-cod-arc-200
           else if   rr-tip-int           =    "F"
                     go to pmt-cod-arc-300
           else      go to pmt-cod-arc-800.
       pmt-cod-arc-100.
      *              *-------------------------------------------------*
      *              * Se movimenti di Generale                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione della modalita' di   *
      *                  * interrogazione                              *
      *                  *---------------------------------------------*
           if        rr-mod-int           =    "S"
                     go to pmt-cod-arc-120
           else      go to pmt-cod-arc-800.
       pmt-cod-arc-120.
      *                  *---------------------------------------------*
      *                  * Se per Sottoconto                           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Visualizzazione prompt                  *
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
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     pmt-cod-arc-999.
       pmt-cod-arc-200.
      *              *-------------------------------------------------*
      *              * Se movimenti Clienti                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione prompt                      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice cliente             :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     pmt-cod-arc-999.
       pmt-cod-arc-300.
      *              *-------------------------------------------------*
      *              * Se movimenti Fornitori                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione prompt                      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice fornitore           :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     pmt-cod-arc-999.
       pmt-cod-arc-800.
      *              *-------------------------------------------------*
      *              * In tutti gli altri casi                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione prompt                      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     pmt-cod-arc-999.
       pmt-cod-arc-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per Tipo visualizzazione          *
      *    *-----------------------------------------------------------*
       pmt-tip-vis-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tipo visualizzazione       :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-tip-vis-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per Data iniziale                 *
      *    *-----------------------------------------------------------*
       pmt-dat-ini-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo visualizzazione *
      *              *-------------------------------------------------*
           if        rr-tip-vis           =    spaces
                     go to pmt-dat-ini-800
           else if   rr-tip-vis           =    "S"
                     go to pmt-dat-ini-100
           else if   rr-tip-vis           =    "L"
                     go to pmt-dat-ini-200
           else if   rr-tip-vis           =    "E"
                     go to pmt-dat-ini-300
           else if   rr-tip-vis           =    "P"
                     go to pmt-dat-ini-400
           else if   rr-tip-vis           =    "T"
                     go to pmt-dat-ini-500
           else      go to pmt-dat-ini-800.
       pmt-dat-ini-100.
      *              *-------------------------------------------------*
      *              * Se Saldo ad una data                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione prompt                      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Saldo alla data            :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     pmt-dat-ini-999.
       pmt-dat-ini-200.
      *              *-------------------------------------------------*
      *              * Se Lista movimenti                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione prompt                      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Data registrazione     dal :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     pmt-dat-ini-999.
       pmt-dat-ini-300.
      *              *-------------------------------------------------*
      *              * Se Estratto conto                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione prompt                      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Estratto conto alla data   :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     pmt-dat-ini-999.
       pmt-dat-ini-400.
      *              *-------------------------------------------------*
      *              * Se Partitario                                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione prompt                      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Data registrazione     dal :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     pmt-dat-ini-999.
       pmt-dat-ini-500.
      *              *-------------------------------------------------*
      *              * Se Totale movimenti                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione prompt                      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Data registrazione     dal :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     pmt-dat-ini-999.
       pmt-dat-ini-800.
      *              *-------------------------------------------------*
      *              * In tutti gli altri casi                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione prompt                      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     pmt-dat-ini-999.
       pmt-dat-ini-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per Data finale                   *
      *    *-----------------------------------------------------------*
       pmt-dat-fin-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo visualizzazione *
      *              *-------------------------------------------------*
           if        rr-tip-vis           =    spaces
                     go to pmt-dat-fin-800
           else if   rr-tip-vis           =    "S"
                     go to pmt-dat-fin-100
           else if   rr-tip-vis           =    "L"
                     go to pmt-dat-fin-200
           else if   rr-tip-vis           =    "E"
                     go to pmt-dat-fin-300
           else if   rr-tip-vis           =    "P"
                     go to pmt-dat-fin-400
           else if   rr-tip-vis           =    "T"
                     go to pmt-dat-fin-500
           else      go to pmt-dat-fin-800.
       pmt-dat-fin-100.
      *              *-------------------------------------------------*
      *              * Se Saldo ad una data                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione prompt                      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     pmt-dat-fin-999.
       pmt-dat-fin-200.
      *              *-------------------------------------------------*
      *              * Se Lista movimenti                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione prompt                      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "                        al :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     pmt-dat-fin-999.
       pmt-dat-fin-300.
      *              *-------------------------------------------------*
      *              * Se Estratto conto                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione prompt                      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     pmt-dat-fin-999.
       pmt-dat-fin-400.
      *              *-------------------------------------------------*
      *              * Se Partitario                                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione prompt                      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "                        al :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     pmt-dat-fin-999.
       pmt-dat-fin-500.
      *              *-------------------------------------------------*
      *              * Se Totale movimenti                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione prompt                      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "                        al :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     pmt-dat-fin-999.
       pmt-dat-fin-800.
      *              *-------------------------------------------------*
      *              * In tutti gli altri casi                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione prompt                      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     pmt-dat-fin-999.
       pmt-dat-fin-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per Selezioni                     *
      *    *-----------------------------------------------------------*
       pmt-sel-gen-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo visualizzazione *
      *              *-------------------------------------------------*
           if        rr-tip-vis           =    spaces
                     go to pmt-sel-gen-800
           else if   rr-tip-vis           =    "L" or
                     rr-tip-vis           =    "T"
                     go to pmt-sel-gen-100
           else      go to pmt-sel-gen-800.
       pmt-sel-gen-100.
      *              *-------------------------------------------------*
      *              * Se Lista o Totale movimenti                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione prompt                      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Selezione su                "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     pmt-sel-gen-999.
       pmt-sel-gen-800.
      *              *-------------------------------------------------*
      *              * In tutti gli altri casi                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione prompt                      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     pmt-sel-gen-999.
       pmt-sel-gen-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per Causale                       *
      *    *-----------------------------------------------------------*
       pmt-sel-cau-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo visualizzazione *
      *              *-------------------------------------------------*
           if        rr-tip-vis           =    spaces
                     go to pmt-sel-cau-800
           else if   rr-tip-vis           =    "L" or
                     rr-tip-vis           =    "T"
                     go to pmt-sel-cau-100
           else      go to pmt-sel-cau-800.
       pmt-sel-cau-100.
      *              *-------------------------------------------------*
      *              * Se Lista o Totale movimenti                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione prompt                      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "      - Causale contabile  :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     pmt-sel-cau-999.
       pmt-sel-cau-800.
      *              *-------------------------------------------------*
      *              * In tutti gli altri casi                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione prompt                      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     pmt-sel-cau-999.
       pmt-sel-cau-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per Contenuto causale             *
      *    *-----------------------------------------------------------*
       pmt-sel-cca-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo visualizzazione *
      *              *-------------------------------------------------*
           if        rr-tip-vis           =    spaces
                     go to pmt-sel-cca-800
           else if   rr-tip-vis           =    "L"
                     go to pmt-sel-cca-100
           else      go to pmt-sel-cca-800.
       pmt-sel-cca-100.
      *              *-------------------------------------------------*
      *              * Se Lista movimenti                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione prompt                      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "      - Contenuto causale  :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     pmt-sel-cca-999.
       pmt-sel-cca-800.
      *              *-------------------------------------------------*
      *              * In tutti gli altri casi                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione prompt                      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     pmt-sel-cca-999.
       pmt-sel-cca-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per Contenuto commento            *
      *    *-----------------------------------------------------------*
       pmt-sel-cco-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo visualizzazione *
      *              *-------------------------------------------------*
           if        rr-tip-vis           =    spaces
                     go to pmt-sel-cco-800
           else if   rr-tip-int           =    "G"   and
                     rr-mod-int           not  = "S"
                     go to pmt-sel-cco-800
           else if   rr-tip-vis           =    "L"
                     go to pmt-sel-cco-100
           else      go to pmt-sel-cco-800.
       pmt-sel-cco-100.
      *              *-------------------------------------------------*
      *              * Se Lista movimenti                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione prompt                      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "      - Contenuto commento :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     pmt-sel-cco-999.
       pmt-sel-cco-800.
      *              *-------------------------------------------------*
      *              * In tutti gli altri casi                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione prompt                      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     pmt-sel-cco-999.
       pmt-sel-cco-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per Importo min                   *
      *    *-----------------------------------------------------------*
       pmt-sel-imi-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo visualizzazione *
      *              *-------------------------------------------------*
           if        rr-tip-vis           =    spaces
                     go to pmt-sel-imi-800
           else if   rr-tip-int           =    "G"   and
                     rr-mod-int           not  = "S" and
                     rr-mod-int           not  = "R"
                     go to pmt-sel-imi-800
           else if   rr-tip-vis           =    "L"
                     go to pmt-sel-imi-100
           else      go to pmt-sel-imi-800.
       pmt-sel-imi-100.
      *              *-------------------------------------------------*
      *              * Se Lista movimenti                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione prompt                      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      21                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "      - Importo            :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     pmt-sel-imi-999.
       pmt-sel-imi-800.
      *              *-------------------------------------------------*
      *              * In tutti gli altri casi                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione prompt                      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      21                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     pmt-sel-imi-999.
       pmt-sel-imi-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per Importo max                   *
      *    *-----------------------------------------------------------*
       pmt-sel-ima-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo visualizzazione *
      *              *-------------------------------------------------*
           if        rr-tip-vis           =    spaces
                     go to pmt-sel-ima-800
           else if   rr-tip-int           =    "G"   and
                     rr-mod-int           not  = "S" and
                     rr-mod-int           not  = "R"
                     go to pmt-sel-ima-800
           else if   rr-tip-vis           =    "L"
                     go to pmt-sel-ima-100
           else      go to pmt-sel-ima-800.
       pmt-sel-ima-100.
      *              *-------------------------------------------------*
      *              * Se Lista movimenti                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione prompt                      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      21                   to   v-lin                  .
           move      50                   to   v-pos                  .
           move      "a :"                to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     pmt-sel-ima-999.
       pmt-sel-ima-800.
      *              *-------------------------------------------------*
      *              * In tutti gli altri casi                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione prompt                      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      21                   to   v-lin                  .
           move      50                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     pmt-sel-ima-999.
       pmt-sel-ima-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per Utente                        *
      *    *-----------------------------------------------------------*
       pmt-sel-ute-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo visualizzazione *
      *              *-------------------------------------------------*
           if        rr-mod-int           =    "I"
                     go to pmt-sel-ute-100
           else      go to pmt-sel-ute-999.
       pmt-sel-ute-100.
      *              *-------------------------------------------------*
      *              * Se Lista movimenti                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione prompt                      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "      - Utente             :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     pmt-sel-ute-999.
       pmt-sel-ute-800.
      *              *-------------------------------------------------*
      *              * In tutti gli altri casi                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione prompt                      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     pmt-sel-ute-999.
       pmt-sel-ute-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per Utente                        *
      *    *-----------------------------------------------------------*
       pmt-sel-fas-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo visualizzazione *
      *              *-------------------------------------------------*
           if        rr-mod-int           =    "I"
                     go to pmt-sel-fas-100
           else      go to pmt-sel-fas-999.
       pmt-sel-fas-100.
      *              *-------------------------------------------------*
      *              * Se Lista movimenti                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione prompt                      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      21                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "      - Fase               :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     pmt-sel-fas-999.
       pmt-sel-fas-800.
      *              *-------------------------------------------------*
      *              * In tutti gli altri casi                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione prompt                      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      21                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     pmt-sel-fas-999.
       pmt-sel-fas-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Tipo interrogazione                  *
      *    *-----------------------------------------------------------*
       acc-tip-int-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione eventuale default              *
      *                  *---------------------------------------------*
           if        rr-tip-int           not  = spaces
                     go to acc-tip-int-100.
           if        w-def-tip-int        =    spaces
                     go to acc-tip-int-100.
           move      w-def-tip-int        to   rr-tip-int             .
       acc-tip-int-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
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
           if        w-cnt-sts-imp-ric    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-tip-int-999.
       acc-tip-int-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        v-num                =    01
                     move  "G"            to   rr-tip-int
           else if   v-num                =    02
                     move  "C"            to   rr-tip-int
           else if   v-num                =    03
                     move  "F"            to   rr-tip-int
           else      move  spaces         to   rr-tip-int             .
       acc-tip-int-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Che sia accettabile                         *
      *                  *---------------------------------------------*
           if        rr-tip-int           not  = "G" and
                     rr-tip-int           not  = "C" and
                     rr-tip-int           not  = "F"
                     go to acc-tip-int-100.
       acc-tip-int-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Memorizzazione valore di default generale   *
      *                  *---------------------------------------------*
           move      rr-tip-int           to   w-def-tip-int          .
      *                  *---------------------------------------------*
      *                  * Confronto con valore precedente             *
      *                  *---------------------------------------------*
           if        rr-tip-int           =    w-pre-tip-int
                     go to  acc-tip-int-620.
      *                  *---------------------------------------------*
      *                  * Aggiornamento valore precedente             *
      *                  *---------------------------------------------*
           move      rr-tip-int           to   w-pre-tip-int          .
      *                  *---------------------------------------------*
      *                  * Normalizzazione modalita' interrogazione    *
      *                  *---------------------------------------------*
           move      spaces               to   rr-mod-int             .
           move      spaces               to   w-pre-mod-int          .
      *                  *---------------------------------------------*
      *                  * Normalizzazione codice archivio             *
      *                  *---------------------------------------------*
           move      zero                 to   rr-cod-arc             .
      *                  *---------------------------------------------*
      *                  * Normalizzazione descrizione archivio        *
      *                  *---------------------------------------------*
           move      spaces               to   rr-des-arc             .
           move      spaces               to   rr-cod-arc-rag         .
           move      spaces               to   rr-cod-arc-via         .
           move      spaces               to   rr-cod-arc-loc         .
       acc-tip-int-620.
      *                  *---------------------------------------------*
      *                  * Normalizzazione date                        *
      *                  *---------------------------------------------*
           move      zero                 to   rr-dat-ini             .
           move      zero                 to   rr-dat-fin             .
      *                  *---------------------------------------------*
      *                  * Normalizzazione altre selezioni             *
      *                  *---------------------------------------------*
           move      zero                 to   rr-sel-cau             .
           move      spaces               to   rr-sel-cau-des         .
           move      spaces               to   rr-sel-cca             .
           move      spaces               to   rr-sel-cco             .
           move      zero                 to   rr-sel-imi             .
           move      zero                 to   rr-sel-ima             .
      *                  *---------------------------------------------*
      *                  * Default per tipo visualizzazione            *
      *                  *---------------------------------------------*
           if        w-pre-tip-vis        =    spaces
                     move  w-def-tip-vis  to   rr-tip-vis
           else      move  w-pre-tip-vis  to   rr-tip-vis             .
      *                  *---------------------------------------------*
      *                  * Video in 'OFF'                              *
      *                  *---------------------------------------------*
           move      "OF"                 to   v-ope                  .
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
      *                  * Visualizzazione prompts                     *
      *                  *---------------------------------------------*
           perform   pmt-ric-sel-000      thru pmt-ric-sel-999        .
      *                  *---------------------------------------------*
      *                  * Visualizzazione richieste                   *
      *                  *---------------------------------------------*
           perform   vis-ric-sel-000      thru vis-ric-sel-999        .
      *                  *---------------------------------------------*
      *                  * Video in 'ON'                               *
      *                  *---------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-tip-int-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-tip-int-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-tip-int-100.
       acc-tip-int-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Tipo interrogazione                     *
      *    *-----------------------------------------------------------*
       vis-tip-int-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-int-lun    to   v-car                  .
           move      w-exp-tip-int-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
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
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tip-int-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Modalita' di interrogazione          *
      *    *-----------------------------------------------------------*
       acc-mod-int-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        rr-tip-int           not  = "G"
                     go to acc-mod-int-999.
      *                  *---------------------------------------------*
      *                  * Preparazione eventuale default              *
      *                  *---------------------------------------------*
           if        rr-mod-int           not  = spaces
                     go to acc-mod-int-100.
           if        w-def-mod-int        =    spaces
                     go to acc-mod-int-100.
           move      w-def-mod-int        to   rr-mod-int             .
       acc-mod-int-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-mod-int-lun    to   v-car                  .
           move      w-exp-mod-int-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      "SDI#"               to   v-msk                  .
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
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-mod-int-999.
       acc-mod-int-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        v-num                =    01
                     move  "S"            to   rr-mod-int
           else if   v-num                =    02
                     move  "R"            to   rr-mod-int
           else if   v-num                =    03
                     move  "I"            to   rr-mod-int
           else      move  spaces         to   rr-mod-int             .
       acc-mod-int-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Che sia accettabile                         *
      *                  *---------------------------------------------*
           if        rr-mod-int           not  = "S" and
                     rr-mod-int           not  = "R" and
                     rr-mod-int           not  = "I"
                     go to acc-mod-int-100.
       acc-mod-int-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Memorizzazione valore di default generale   *
      *                  *---------------------------------------------*
           move      rr-mod-int           to   w-def-mod-int          .
      *                  *---------------------------------------------*
      *                  * Confronto con valore precedente             *
      *                  *---------------------------------------------*
           if        rr-mod-int           =    w-pre-mod-int
                     go to  acc-mod-int-800.
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
      *                  *---------------------------------------------*
      *                  * Aggiornamento valore precedente             *
      *                  *---------------------------------------------*
           move      rr-mod-int           to   w-pre-mod-int          .
      *                  *---------------------------------------------*
      *                  * Default per tipo visualizzazione            *
      *                  *---------------------------------------------*
           if        w-pre-tip-vis        =    spaces
                     move  w-def-tip-vis  to   rr-tip-vis
           else      move  w-pre-tip-vis  to   rr-tip-vis             .
      *                  *---------------------------------------------*
      *                  * Video in 'OFF'                              *
      *                  *---------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Erase line 09-21                            *
      *                  *---------------------------------------------*
           move      "EL"                 to   v-ope                  .
           move      09                   to   v-lin                  .
           move      21                   to   v-lto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione prompts                     *
      *                  *---------------------------------------------*
           perform   pmt-ric-sel-000      thru pmt-ric-sel-999        .
      *                  *---------------------------------------------*
      *                  * Visualizzazione richieste                   *
      *                  *---------------------------------------------*
           perform   vis-ric-sel-000      thru vis-ric-sel-999        .
      *                  *---------------------------------------------*
      *                  * Video in 'ON'                               *
      *                  *---------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-mod-int-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-mod-int-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-mod-int-100.
       acc-mod-int-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Modalita' di interrogazione             *
      *    *-----------------------------------------------------------*
       vis-mod-int-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo visualizzazione *
      *              *-------------------------------------------------*
           if        rr-tip-int           =    spaces
                     go to vis-mod-int-800
           else if   rr-tip-int           =    "G"
                     go to vis-mod-int-100
           else      go to vis-mod-int-800.
       vis-mod-int-100.
      *              *-------------------------------------------------*
      *              * Se movimenti di Generale                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione                             *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-mod-int-lun    to   v-car                  .
           move      w-exp-mod-int-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
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
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     vis-mod-int-999.
       vis-mod-int-800.
      *              *-------------------------------------------------*
      *              * In tutti gli altri casi                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione                             *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     vis-mod-int-999.
       vis-mod-int-999.
           exit.

      *    *===========================================================*
      *    * Accettazione Codice archivio                              *
      *    *-----------------------------------------------------------*
       acc-cod-arc-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo visualizzazione *
      *              *-------------------------------------------------*
           if        rr-tip-int           =    spaces
                     go to acc-cod-arc-800
           else if   rr-tip-int           =    "G"
                     go to acc-cod-arc-100
           else if   rr-tip-int           =    "C"
                     go to acc-cod-arc-200
           else if   rr-tip-int           =    "F"
                     go to acc-cod-arc-300
           else      go to acc-cod-arc-800.
       acc-cod-arc-100.
      *              *-------------------------------------------------*
      *              * Se movimenti di Generale                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Accettazione codice Sottoconto              *
      *                  *---------------------------------------------*
           perform   acc-cod-pdc-000      thru acc-cod-pdc-999        .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     acc-cod-arc-999.
       acc-cod-arc-200.
      *              *-------------------------------------------------*
      *              * Se movimenti Clienti                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Accettazione codice Cliente                 *
      *                  *---------------------------------------------*
           perform   acc-cod-cli-000      thru acc-cod-cli-999        .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     acc-cod-arc-999.
       acc-cod-arc-300.
      *              *-------------------------------------------------*
      *              * Se movimenti Fornitori                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Accettazione codice Fornitore               *
      *                  *---------------------------------------------*
           perform   acc-cod-fnt-000      thru acc-cod-fnt-999        .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     acc-cod-arc-999.
       acc-cod-arc-800.
      *              *-------------------------------------------------*
      *              * In tutti gli altri casi                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     acc-cod-arc-999.
       acc-cod-arc-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione Codice archivio                           *
      *    *-----------------------------------------------------------*
       vis-cod-arc-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo visualizzazione *
      *              *-------------------------------------------------*
           if        rr-tip-int           =    spaces
                     go to vis-cod-arc-800
           else if   rr-tip-int           =    "G"
                     go to vis-cod-arc-100
           else if   rr-tip-int           =    "C"
                     go to vis-cod-arc-200
           else if   rr-tip-int           =    "F"
                     go to vis-cod-arc-300
           else      go to vis-cod-arc-800.
       vis-cod-arc-100.
      *              *-------------------------------------------------*
      *              * Se movimenti di Generale                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione codice Sottoconto           *
      *                  *---------------------------------------------*
           perform   vis-cod-pdc-000      thru vis-cod-pdc-999        .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     vis-cod-arc-999.
       vis-cod-arc-200.
      *              *-------------------------------------------------*
      *              * Se movimenti Clienti                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione codice Cliente              *
      *                  *---------------------------------------------*
           perform   vis-cod-cli-000      thru vis-cod-cli-999        .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     vis-cod-arc-999.
       vis-cod-arc-300.
      *              *-------------------------------------------------*
      *              * Se movimenti Fornitori                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione codice Fornitore            *
      *                  *---------------------------------------------*
           perform   vis-cod-fnt-000      thru vis-cod-fnt-999        .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     vis-cod-arc-999.
       vis-cod-arc-800.
      *              *-------------------------------------------------*
      *              * In tutti gli altri casi                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     vis-cod-arc-999.
       vis-cod-arc-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione Codice archivio, descrizione              *
      *    *-----------------------------------------------------------*
       vis-cod-arc-des-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo visualizzazione *
      *              *-------------------------------------------------*
           if        rr-tip-vis           =    spaces
                     go to vis-cod-arc-des-800
           else if   rr-tip-vis           =    "G"
                     go to vis-cod-arc-des-100
           else if   rr-tip-vis           =    "C"
                     go to vis-cod-arc-des-200
           else if   rr-tip-vis           =    "F"
                     go to vis-cod-arc-des-300
           else      go to vis-cod-arc-des-800.
       vis-cod-arc-des-100.
      *              *-------------------------------------------------*
      *              * Se movimenti di Generale                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione Sottoconto      *
      *                  *---------------------------------------------*
           perform   vis-cod-pdc-des-000  thru vis-cod-pdc-des-999    .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     vis-cod-arc-des-999.
       vis-cod-arc-des-200.
      *              *-------------------------------------------------*
      *              * Se movimenti Clienti                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione ragione sociale Cliente     *
      *                  *---------------------------------------------*
           perform   vis-cod-cli-rag-000  thru vis-cod-cli-rag-999    .
           perform   vis-cod-cli-via-000  thru vis-cod-cli-via-999    .
           perform   vis-cod-cli-loc-000  thru vis-cod-cli-loc-999    .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     vis-cod-arc-des-999.
       vis-cod-arc-des-300.
      *              *-------------------------------------------------*
      *              * Se movimenti Fornitori                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione ragione sociale Fornitore   *
      *                  *---------------------------------------------*
           perform   vis-cod-fnt-rag-000  thru vis-cod-fnt-rag-999    .
           perform   vis-cod-fnt-via-000  thru vis-cod-fnt-via-999    .
           perform   vis-cod-fnt-loc-000  thru vis-cod-fnt-loc-999    .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     vis-cod-arc-des-999.
       vis-cod-arc-des-800.
      *              *-------------------------------------------------*
      *              * In tutti gli altri casi                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     vis-cod-arc-des-999.
       vis-cod-arc-des-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Codice sottoconto                    *
      *    *-----------------------------------------------------------*
       acc-cod-pdc-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        rr-tip-int           not  = "G"
                     go to acc-cod-pdc-999.
           if        rr-mod-int           not  = "S"
                     go to acc-cod-pdc-999.
      *                  *---------------------------------------------*
      *                  * Se valore diverso da zero : lettura della   *
      *                  * descrizione e visualizzazione               *
      *                  *---------------------------------------------*
           if        rr-cod-arc           =    zero
                     go to acc-cod-pdc-100.
      *                  *---------------------------------------------*
      *                  * Lettura archivio piano dei conti            *
      *                  *---------------------------------------------*
           move      rr-cod-arc           to   w-let-arc-pdc-cod      .
           perform   let-arc-pdc-000      thru let-arc-pdc-999        .
           move      w-let-arc-pdc-des    to   rr-des-arc             .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione sottoconto      *
      *                  *---------------------------------------------*
           perform   vis-cod-pdc-des-000  thru vis-cod-pdc-des-999    .
       acc-cod-pdc-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
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
       acc-cod-pdc-110.
           perform   cod-mne-pdc-cll-000  thru cod-mne-pdc-cll-999    .
           if        w-cod-mne-pdc-ope    =    "F+"
                     go to acc-cod-pdc-115.
           if        w-cod-mne-pdc-ope    =    "AC"
                     go to acc-cod-pdc-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-pdc-115.
           perform   cod-mne-pdc-foi-000  thru cod-mne-pdc-foi-999    .
           go to     acc-cod-pdc-110.
       acc-cod-pdc-120.
           move      w-cod-mne-pdc-cod    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-cod-pdc-999.
       acc-cod-pdc-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-cod-arc             .
       acc-cod-pdc-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura archivio piano dei conti            *
      *                  *---------------------------------------------*
           move      rr-cod-arc           to   w-let-arc-pdc-cod      .
           perform   let-arc-pdc-000      thru let-arc-pdc-999        .
           move      w-let-arc-pdc-des    to   rr-des-arc             .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione sottoconto      *
      *                  *---------------------------------------------*
           perform   vis-cod-pdc-des-000  thru vis-cod-pdc-des-999    .
      *                  *---------------------------------------------*
      *                  * Se codice sottoconto non esistente : reim-  *
      *                  * postazione                                  *
      *                  *---------------------------------------------*
           if        w-let-arc-pdc-flg    not  = spaces
                     go to acc-cod-pdc-100.
      *                  *---------------------------------------------*
      *                  * Se sottoconto a zero : reimpostazione       *
      *                  *---------------------------------------------*
           if        rr-cod-arc           not  = zero
                     go to acc-cod-pdc-600.
           if        v-key                not  = "UP  "
                     go to acc-cod-pdc-100.
       acc-cod-pdc-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cod-pdc-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-cod-pdc-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-cod-pdc-100.
       acc-cod-pdc-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione Codice sottoconto                         *
      *    *-----------------------------------------------------------*
       vis-cod-pdc-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo interrogazione  *
      *              * e della modalita' di interrogazione             *
      *              *-------------------------------------------------*
           if        rr-tip-int           not  = "G"
                     go to vis-cod-pdc-800.
           if        rr-mod-int           not  = "S"
                     go to vis-cod-pdc-800.
       vis-cod-pdc-100.
      *              *-------------------------------------------------*
      *              * Codice                                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Editing con appoggio a sinistra             *
      *                  *---------------------------------------------*
           move      w-prs-liv-pdc        to   w-edt-cod-pdc-liv      .
           move      rr-cod-arc           to   w-edt-cod-pdc-cod      .
           move      "B"                  to   w-edt-cod-pdc-edm      .
           perform   edt-pdc-asx-000      thru edt-pdc-asx-999        .
      *                  *---------------------------------------------*
      *                  * Visualizzazione                             *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      09                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-edt-cod-pdc-edt    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Oltre                                       *
      *                  *---------------------------------------------*
           go to     vis-cod-pdc-999.
       vis-cod-pdc-800.
      *              *-------------------------------------------------*
      *              * Campo vuoto                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione                             *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      09                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Oltre                                       *
      *                  *---------------------------------------------*
           go to     vis-cod-pdc-999.
       vis-cod-pdc-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione Codice sottoconto, descrizione            *
      *    *-----------------------------------------------------------*
       vis-cod-pdc-des-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo interrogazione  *
      *              * e della modalita' di interrogazione             *
      *              *-------------------------------------------------*
           if        rr-tip-int           not  = "G"
                     go to vis-cod-pdc-des-800.
           if        rr-mod-int           not  = "S"
                     go to vis-cod-pdc-des-800.
       vis-cod-pdc-des-100.
      *              *-------------------------------------------------*
      *              * Descrizione                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione                             *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      rr-des-arc           to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Oltre                                       *
      *                  *---------------------------------------------*
           go to     vis-cod-pdc-des-999.
       vis-cod-pdc-des-800.
      *              *-------------------------------------------------*
      *              * Campo vuoto                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione                             *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Oltre                                       *
      *                  *---------------------------------------------*
           go to     vis-cod-pdc-des-999.
       vis-cod-pdc-des-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Codice cliente                       *
      *    *-----------------------------------------------------------*
       acc-cod-cli-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        rr-tip-int           not  = "C"
                     go to acc-cod-cli-999.
      *                  *---------------------------------------------*
      *                  * Se valore diverso da zero : lettura della   *
      *                  * ragione sociale e visualizzazione           *
      *                  *---------------------------------------------*
           if        rr-cod-arc           =    zero
                     go to acc-cod-cli-100.
      *                  *---------------------------------------------*
      *                  * Lettura file [cli]                          *
      *                  *---------------------------------------------*
           move      rr-cod-arc           to   w-let-arc-cli-cod      .
           perform   let-arc-cli-000      thru let-arc-cli-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione ragione sociale, indirizzo,  *
      *                  * localita', e sottoconto associato da ana-   *
      *                  * grafica contabile                           *
      *                  *---------------------------------------------*
           move      w-let-arc-cli-rag    to   rr-cod-arc-rag         .
           move      w-let-arc-cli-via    to   rr-cod-arc-via         .
           move      w-let-arc-cli-loc    to   rr-cod-arc-loc         .
      *                  *---------------------------------------------*
      *                  * Visualizzazione ragione sociale da anagra-  *
      *                  * fica contabile                              *
      *                  *---------------------------------------------*
           perform   vis-cod-cli-rag-000  thru vis-cod-cli-rag-999    .
           perform   vis-cod-cli-via-000  thru vis-cod-cli-via-999    .
           perform   vis-cod-cli-loc-000  thru vis-cod-cli-loc-999    .
       acc-cod-cli-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-cli-ope      .
           move      rr-cod-arc           to   w-cod-mne-cli-cod      .
           move      09                   to   w-cod-mne-cli-lin      .
           move      30                   to   w-cod-mne-cli-pos      .
           move      09                   to   w-cod-mne-cli-rln      .
           move      41                   to   w-cod-mne-cli-rps      .
           move      10                   to   w-cod-mne-cli-vln      .
           move      41                   to   w-cod-mne-cli-vps      .
           move      11                   to   w-cod-mne-cli-lln      .
           move      41                   to   w-cod-mne-cli-lps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-mne-cli-cll-000  thru cod-mne-cli-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-mne-cli-foi-000  thru cod-mne-cli-foi-999    .
       acc-cod-cli-110.
           perform   cod-mne-cli-cll-000  thru cod-mne-cli-cll-999    .
           if        w-cod-mne-cli-ope    =    "F+"
                     go to acc-cod-cli-115.
           if        w-cod-mne-cli-ope    =    "AC"
                     go to acc-cod-cli-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-cli-115.
           perform   cod-mne-cli-foi-000  thru cod-mne-cli-foi-999    .
           go to     acc-cod-cli-110.
       acc-cod-cli-120.
           move      w-cod-mne-cli-cod    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-cod-cli-999.
       acc-cod-cli-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-cod-arc             .
       acc-cod-cli-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-cod-cli-410.
      *                  *---------------------------------------------*
      *                  * Lettura file [cli]                          *
      *                  *---------------------------------------------*
           move      rr-cod-arc           to   w-let-arc-cli-cod      .
           perform   let-arc-cli-000      thru let-arc-cli-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione ragione sociale, indirizzo,  *
      *                  * localita', e sottoconto associato da ana-   *
      *                  * grafica contabile                           *
      *                  *---------------------------------------------*
           move      w-let-arc-cli-rag    to   rr-cod-arc-rag         .
           move      w-let-arc-cli-via    to   rr-cod-arc-via         .
           move      w-let-arc-cli-loc    to   rr-cod-arc-loc         .
      *                  *---------------------------------------------*
      *                  * Visualizzazione ragione sociale da anagra-  *
      *                  * fica contabile                              *
      *                  *---------------------------------------------*
           perform   vis-cod-cli-rag-000  thru vis-cod-cli-rag-999    .
           perform   vis-cod-cli-via-000  thru vis-cod-cli-via-999    .
           perform   vis-cod-cli-loc-000  thru vis-cod-cli-loc-999    .
       acc-cod-cli-430.
      *                  *---------------------------------------------*
      *                  * Se cliente non esistente                    *
      *                  *---------------------------------------------*
           if        w-let-arc-cli-flg    =    spaces
                     go to acc-cod-cli-440.
      *                      *-----------------------------------------*
      *                      * A reimpostazione                        *
      *                      *-----------------------------------------*
           go to     acc-cod-cli-100.
       acc-cod-cli-440.
      *                  *---------------------------------------------*
      *                  * Se codice a zero                            *
      *                  *---------------------------------------------*
           if        rr-cod-arc           not  = zero
                     go to acc-cod-cli-450.
      *                      *-----------------------------------------*
      *                      * Test se impostato 'Up'                  *
      *                      *-----------------------------------------*
           if        v-key                =    "UP  "
                     go to acc-cod-cli-600.
      *                      *-----------------------------------------*
      *                      * A reimpostazione                        *
      *                      *-----------------------------------------*
           go to     acc-cod-cli-100.
       acc-cod-cli-450.
      *                  *---------------------------------------------*
      *                  * Se codice diverso da zero                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Oltre                                   *
      *                      *-----------------------------------------*
           go to     acc-cod-cli-600.
       acc-cod-cli-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cod-cli-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-cod-cli-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-cod-cli-100.
       acc-cod-cli-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Codice cliente                          *
      *    *-----------------------------------------------------------*
       vis-cod-cli-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo interrogazione  *
      *              *-------------------------------------------------*
           if        rr-tip-int           not  = "C"
                     go to vis-cod-cli-800.
       vis-cod-cli-100.
      *              *-------------------------------------------------*
      *              * Codice                                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione                             *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      09                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-cod-arc           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Oltre                                       *
      *                  *---------------------------------------------*
           go to     vis-cod-cli-999.
       vis-cod-cli-800.
      *              *-------------------------------------------------*
      *              * Campo vuoto                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione                             *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      09                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      zero                 to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Oltre                                       *
      *                  *---------------------------------------------*
           go to     vis-cod-cli-999.
       vis-cod-cli-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Codice cliente , ragione sociale        *
      *    *-----------------------------------------------------------*
       vis-cod-cli-rag-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo interrogazione  *
      *              *-------------------------------------------------*
           if        rr-tip-int           not  = "C"
                     go to vis-cod-cli-rag-800.
       vis-cod-cli-rag-100.
      *              *-------------------------------------------------*
      *              * Ragione sociale                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione                             *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      rr-cod-arc-rag       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Oltre                                       *
      *                  *---------------------------------------------*
           go to     vis-cod-cli-rag-999.
       vis-cod-cli-rag-800.
      *              *-------------------------------------------------*
      *              * Campo vuoto                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione                             *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Oltre                                       *
      *                  *---------------------------------------------*
           go to     vis-cod-cli-rag-999.
       vis-cod-cli-rag-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Codice cliente, indirizzo               *
      *    *-----------------------------------------------------------*
       vis-cod-cli-via-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo interrogazione  *
      *              *-------------------------------------------------*
           if        rr-tip-int           not  = "C"
                     go to vis-cod-cli-via-800.
       vis-cod-cli-via-100.
      *              *-------------------------------------------------*
      *              * Indirizzo                                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione                             *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      rr-cod-arc-via       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Oltre                                       *
      *                  *---------------------------------------------*
           go to     vis-cod-cli-via-999.
       vis-cod-cli-via-800.
      *              *-------------------------------------------------*
      *              * Campo vuoto                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione                             *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Oltre                                       *
      *                  *---------------------------------------------*
           go to     vis-cod-cli-via-999.
       vis-cod-cli-via-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Codice cliente, localita'               *
      *    *-----------------------------------------------------------*
       vis-cod-cli-loc-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo interrogazione  *
      *              *-------------------------------------------------*
           if        rr-tip-int           not  = "C"
                     go to vis-cod-cli-loc-800.
       vis-cod-cli-loc-100.
      *              *-------------------------------------------------*
      *              * Localita'                                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione                             *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      rr-cod-arc-loc       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Oltre                                       *
      *                  *---------------------------------------------*
           go to     vis-cod-cli-loc-999.
       vis-cod-cli-loc-800.
      *              *-------------------------------------------------*
      *              * Campo vuoto                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione                             *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Oltre                                       *
      *                  *---------------------------------------------*
           go to     vis-cod-cli-loc-999.
       vis-cod-cli-loc-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Codice fornitore                     *
      *    *-----------------------------------------------------------*
       acc-cod-fnt-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        rr-tip-int           not  = "F"
                     go to acc-cod-fnt-999.
      *                  *---------------------------------------------*
      *                  * Se valore diverso da zero : lettura della   *
      *                  * ragione sociale e visualizzazione           *
      *                  *---------------------------------------------*
           if        rr-cod-arc           =    zero
                     go to acc-cod-fnt-100.
      *                  *---------------------------------------------*
      *                  * Memorizzazione ragione sociale, indirizzo,  *
      *                  * localita', e sottoconto associato da ana-   *
      *                  * grafica contabile                           *
      *                  *---------------------------------------------*
           move      w-let-arc-fnt-rag    to   rr-cod-arc-rag         .
           move      w-let-arc-fnt-via    to   rr-cod-arc-via         .
           move      w-let-arc-fnt-loc    to   rr-cod-arc-loc         .
      *                  *---------------------------------------------*
      *                  * Visualizzazione ragione sociale da anagra-  *
      *                  * fica contabile                              *
      *                  *---------------------------------------------*
           perform   vis-cod-fnt-rag-000  thru vis-cod-fnt-rag-999    .
           perform   vis-cod-fnt-via-000  thru vis-cod-fnt-via-999    .
           perform   vis-cod-fnt-loc-000  thru vis-cod-fnt-loc-999    .
       acc-cod-fnt-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-fnt-ope      .
           move      rr-cod-arc           to   w-cod-mne-fnt-cod      .
           move      09                   to   w-cod-mne-fnt-lin      .
           move      30                   to   w-cod-mne-fnt-pos      .
           move      09                   to   w-cod-mne-fnt-rln      .
           move      41                   to   w-cod-mne-fnt-rps      .
           move      10                   to   w-cod-mne-fnt-vln      .
           move      41                   to   w-cod-mne-fnt-vps      .
           move      11                   to   w-cod-mne-fnt-lln      .
           move      41                   to   w-cod-mne-fnt-lps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-mne-fnt-cll-000  thru cod-mne-fnt-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-mne-fnt-foi-000  thru cod-mne-fnt-foi-999    .
       acc-cod-fnt-110.
           perform   cod-mne-fnt-cll-000  thru cod-mne-fnt-cll-999    .
           if        w-cod-mne-fnt-ope    =    "F+"
                     go to acc-cod-fnt-115.
           if        w-cod-mne-fnt-ope    =    "AC"
                     go to acc-cod-fnt-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-fnt-115.
           perform   cod-mne-fnt-foi-000  thru cod-mne-fnt-foi-999    .
           go to     acc-cod-fnt-110.
       acc-cod-fnt-120.
           move      w-cod-mne-fnt-cod    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-cod-fnt-999.
       acc-cod-fnt-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-cod-arc             .
       acc-cod-fnt-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura file [fnt]                          *
      *                  *---------------------------------------------*
           move      rr-cod-arc           to   w-let-arc-fnt-cod      .
           perform   let-arc-fnt-000      thru let-arc-fnt-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione ragione sociale, indirizzo,  *
      *                  * localita', e sottoconto associato da ana-   *
      *                  * grafica contabile                           *
      *                  *---------------------------------------------*
           move      w-let-arc-fnt-rag    to   rr-cod-arc-rag         .
           move      w-let-arc-fnt-via    to   rr-cod-arc-via         .
           move      w-let-arc-fnt-loc    to   rr-cod-arc-loc         .
      *                  *---------------------------------------------*
      *                  * Visualizzazione ragione sociale da anagra-  *
      *                  * fica contabile                              *
      *                  *---------------------------------------------*
           perform   vis-cod-fnt-rag-000  thru vis-cod-fnt-rag-999    .
           perform   vis-cod-fnt-via-000  thru vis-cod-fnt-via-999    .
           perform   vis-cod-fnt-loc-000  thru vis-cod-fnt-loc-999    .
       acc-cod-fnt-430.
      *                  *---------------------------------------------*
      *                  * Se fornitore non esistente                  *
      *                  *---------------------------------------------*
           if        w-let-arc-fnt-flg    =    spaces
                     go to acc-cod-fnt-440.
      *                      *-----------------------------------------*
      *                      * A reimpostazione                        *
      *                      *-----------------------------------------*
           go to     acc-cod-fnt-100.
       acc-cod-fnt-440.
      *                  *---------------------------------------------*
      *                  * Se codice a zero                            *
      *                  *---------------------------------------------*
           if        rr-cod-arc           not  = zero
                     go to acc-cod-fnt-450.
      *                      *-----------------------------------------*
      *                      * Test se impostato 'Up'                  *
      *                      *-----------------------------------------*
           if        v-key                =    "UP  "
                     go to acc-cod-fnt-600.
      *                      *-----------------------------------------*
      *                      * A reimpostazione                        *
      *                      *-----------------------------------------*
           go to     acc-cod-fnt-100.
       acc-cod-fnt-450.
      *                  *---------------------------------------------*
      *                  * Se codice diverso da zero                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Oltre                                   *
      *                      *-----------------------------------------*
           go to     acc-cod-fnt-600.
       acc-cod-fnt-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cod-fnt-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-cod-fnt-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-cod-fnt-100.
       acc-cod-fnt-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Codice fornitore                        *
      *    *-----------------------------------------------------------*
       vis-cod-fnt-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo interrogazione  *
      *              *-------------------------------------------------*
           if        rr-tip-int           not  = "F"
                     go to vis-cod-fnt-800.
       vis-cod-fnt-100.
      *              *-------------------------------------------------*
      *              * Codice                                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione                             *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      09                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-cod-arc           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Oltre                                       *
      *                  *---------------------------------------------*
           go to     vis-cod-fnt-999.
       vis-cod-fnt-800.
      *              *-------------------------------------------------*
      *              * Campo vuoto                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione                             *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      09                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      zero                 to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Oltre                                       *
      *                  *---------------------------------------------*
           go to     vis-cod-fnt-999.
       vis-cod-fnt-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Codice fornitore , ragione sociale      *
      *    *-----------------------------------------------------------*
       vis-cod-fnt-rag-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo interrogazione  *
      *              *-------------------------------------------------*
           if        rr-tip-int           not  = "F"
                     go to vis-cod-fnt-rag-800.
       vis-cod-fnt-rag-100.
      *              *-------------------------------------------------*
      *              * Ragione sociale                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione                             *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      rr-cod-arc-rag       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Oltre                                       *
      *                  *---------------------------------------------*
           go to     vis-cod-fnt-rag-999.
       vis-cod-fnt-rag-800.
      *              *-------------------------------------------------*
      *              * Campo vuoto                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione                             *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Oltre                                       *
      *                  *---------------------------------------------*
           go to     vis-cod-fnt-rag-999.
       vis-cod-fnt-rag-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Codice fornitore, indirizzo             *
      *    *-----------------------------------------------------------*
       vis-cod-fnt-via-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo interrogazione  *
      *              *-------------------------------------------------*
           if        rr-tip-int           not  = "F"
                     go to vis-cod-fnt-via-800.
       vis-cod-fnt-via-100.
      *              *-------------------------------------------------*
      *              * Indirizzo                                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione                             *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      rr-cod-arc-via       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Oltre                                       *
      *                  *---------------------------------------------*
           go to     vis-cod-fnt-via-999.
       vis-cod-fnt-via-800.
      *              *-------------------------------------------------*
      *              * Campo vuoto                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione                             *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Oltre                                       *
      *                  *---------------------------------------------*
           go to     vis-cod-fnt-via-999.
       vis-cod-fnt-via-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Codice fornitore, localita'             *
      *    *-----------------------------------------------------------*
       vis-cod-fnt-loc-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo interrogazione  *
      *              *-------------------------------------------------*
           if        rr-tip-int           not  = "F"
                     go to vis-cod-fnt-loc-800.
       vis-cod-fnt-loc-100.
      *              *-------------------------------------------------*
      *              * Localita'                                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione                             *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      rr-cod-arc-loc       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Oltre                                       *
      *                  *---------------------------------------------*
           go to     vis-cod-fnt-loc-999.
       vis-cod-fnt-loc-800.
      *              *-------------------------------------------------*
      *              * Campo vuoto                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione                             *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Oltre                                       *
      *                  *---------------------------------------------*
           go to     vis-cod-fnt-loc-999.
       vis-cod-fnt-loc-999.
           exit.

      *    *===========================================================*
      *    * Accettazione Tipo visualizzazione                         *
      *    *-----------------------------------------------------------*
       acc-tip-vis-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo visualizzazione *
      *              *-------------------------------------------------*
           if        rr-tip-int           =    spaces
                     go to acc-tip-vis-800
           else if   rr-tip-int           =    "G"
                     go to acc-tip-vis-100
           else if   rr-tip-int           =    "C"
                     go to acc-tip-vis-200
           else if   rr-tip-int           =    "F"
                     go to acc-tip-vis-300
           else      go to acc-tip-vis-800.
       acc-tip-vis-100.
      *              *-------------------------------------------------*
      *              * Se movimenti di Generale                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Accettazione per movimenti di Generale      *
      *                  *---------------------------------------------*
           perform   acc-tip-vis-gen-000  thru acc-tip-vis-gen-999    .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     acc-tip-vis-999.
       acc-tip-vis-200.
      *              *-------------------------------------------------*
      *              * Se movimenti Clienti                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Accettazione per movimenti Clienti          *
      *                  *---------------------------------------------*
           perform   acc-tip-vis-cof-000  thru acc-tip-vis-cof-999    .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     acc-tip-vis-999.
       acc-tip-vis-300.
      *              *-------------------------------------------------*
      *              * Se movimenti Fornitori                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Accettazione per movimenti Fornitori        *
      *                  *---------------------------------------------*
           perform   acc-tip-vis-cof-000  thru acc-tip-vis-cof-999    .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     acc-tip-vis-999.
       acc-tip-vis-800.
      *              *-------------------------------------------------*
      *              * In tutti gli altri casi                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     acc-tip-vis-999.
       acc-tip-vis-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione Tipo visualizzazione                      *
      *    *-----------------------------------------------------------*
       vis-tip-vis-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo visualizzazione *
      *              *-------------------------------------------------*
           if        rr-tip-int           =    spaces
                     go to vis-tip-vis-800
           else if   rr-tip-int           =    "G"
                     go to vis-tip-vis-100
           else if   rr-tip-int           =    "C"
                     go to vis-tip-vis-200
           else if   rr-tip-int           =    "F"
                     go to vis-tip-vis-300
           else      go to vis-tip-vis-800.
       vis-tip-vis-100.
      *              *-------------------------------------------------*
      *              * Se movimenti di Generale                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione per movimenti di Generale   *
      *                  *---------------------------------------------*
           perform   vis-tip-vis-gen-000  thru vis-tip-vis-gen-999    .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     vis-tip-vis-999.
       vis-tip-vis-200.
      *              *-------------------------------------------------*
      *              * Se movimenti Clienti                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione per movimenti Clienti       *
      *                  *---------------------------------------------*
           perform   vis-tip-vis-cof-000  thru vis-tip-vis-cof-999    .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     vis-tip-vis-999.
       vis-tip-vis-300.
      *              *-------------------------------------------------*
      *              * Se movimenti Fornitori                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione per movimenti Fornitori     *
      *                  *---------------------------------------------*
           perform   vis-tip-vis-cof-000  thru vis-tip-vis-cof-999    .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     vis-tip-vis-999.
       vis-tip-vis-800.
      *              *-------------------------------------------------*
      *              * In tutti gli altri casi                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     vis-tip-vis-999.
       vis-tip-vis-999.
           exit.

      *    *===========================================================*
      *    * Accettazione Tipo visualizzazione                         *
      *    *                                                           *
      *    * Se movimenti di generale                                  *
      *    *-----------------------------------------------------------*
       acc-tip-vis-gen-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione della modalita' di       *
      *              * interrogazione                                  *
      *              *-------------------------------------------------*
           if        rr-mod-int           =    spaces
                     go to acc-tip-vis-gen-800
           else if   rr-mod-int           =    "S"
                     go to acc-tip-vis-gen-100
           else if   rr-mod-int           =    "R"
                     go to acc-tip-vis-gen-200
           else if   rr-mod-int           =    "I"
                     go to acc-tip-vis-gen-300
           else      go to acc-tip-vis-gen-800.
       acc-tip-vis-gen-100.
      *              *-------------------------------------------------*
      *              * Se per Sottoconto                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Accettazione per Sottoconto                 *
      *                  *---------------------------------------------*
           perform   acc-tip-vis-ge1-000  thru acc-tip-vis-ge1-999    .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     acc-tip-vis-gen-999.
       acc-tip-vis-gen-200.
      *              *-------------------------------------------------*
      *              * Se per data registrazione                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Accettazione per data registrazione         *
      *                  *---------------------------------------------*
           perform   acc-tip-vis-ge2-000  thru acc-tip-vis-ge2-999    .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     acc-tip-vis-gen-999.
       acc-tip-vis-gen-300.
      *              *-------------------------------------------------*
      *              * Se per data ultima modifica                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Accettazione per movimenti Fornitori        *
      *                  *---------------------------------------------*
           perform   acc-tip-vis-ge2-000  thru acc-tip-vis-ge2-999    .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     acc-tip-vis-gen-999.
       acc-tip-vis-gen-800.
      *              *-------------------------------------------------*
      *              * In tutti gli altri casi                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     acc-tip-vis-gen-999.
       acc-tip-vis-gen-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione Tipo visualizzazione                      *
      *    *                                                           *
      *    * Se movimenti di generale                                  *
      *    *-----------------------------------------------------------*
       vis-tip-vis-gen-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione della modalita' di       *
      *              * interrogazione                                  *
      *              *-------------------------------------------------*
           if        rr-mod-int           =    spaces
                     go to vis-tip-vis-gen-800
           else if   rr-mod-int           =    "S"
                     go to vis-tip-vis-gen-100
           else if   rr-mod-int           =    "R"
                     go to vis-tip-vis-gen-200
           else if   rr-mod-int           =    "I"
                     go to vis-tip-vis-gen-300
           else      go to vis-tip-vis-gen-800.
       vis-tip-vis-gen-100.
      *              *-------------------------------------------------*
      *              * Se per Sottoconto                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione per Sottoconto              *
      *                  *---------------------------------------------*
           perform   vis-tip-vis-ge1-000  thru vis-tip-vis-ge1-999    .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     vis-tip-vis-gen-999.
       vis-tip-vis-gen-200.
      *              *-------------------------------------------------*
      *              * Se per data registrazione                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione per data registrazione      *
      *                  *---------------------------------------------*
           perform   vis-tip-vis-ge2-000  thru vis-tip-vis-ge2-999    .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     vis-tip-vis-gen-999.
       vis-tip-vis-gen-300.
      *              *-------------------------------------------------*
      *              * Se per data ultima modifica                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Accettazione per movimenti Fornitori        *
      *                  *---------------------------------------------*
           perform   vis-tip-vis-ge2-000  thru vis-tip-vis-ge2-999    .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     vis-tip-vis-gen-999.
       vis-tip-vis-gen-800.
      *              *-------------------------------------------------*
      *              * In tutti gli altri casi                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     vis-tip-vis-gen-999.
       vis-tip-vis-gen-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Tipo visualizzazione                 *
      *    *                                                           *
      *    * Se movimenti di generale, per sottoconto                  *
      *    *-----------------------------------------------------------*
       acc-tip-vis-ge1-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        rr-tip-int           not  = "G"
                     go to acc-tip-vis-ge1-999.
           if        rr-mod-int           not  = "S"
                     go to acc-tip-vis-ge1-999.
      *                  *---------------------------------------------*
      *                  * Preparazione eventuale default              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se il campo non e' a spaces : nessun    *
      *                      * default                                 *
      *                      *-----------------------------------------*
           if        rr-tip-vis           not  = spaces
                     go to acc-tip-vis-ge1-100.
      *                      *-----------------------------------------*
      *                      * Se il default generale e' a spaces :    *
      *                      * default 'L'                             *
      *                      *-----------------------------------------*
           if        w-def-tip-vis        =    spaces
                     move  "L"            to   rr-tip-vis
                     go to acc-tip-vis-ge1-100.
      *                      *-----------------------------------------*
      *                      * Test di compatibilta' per il default    *
      *                      *-----------------------------------------*
           if        w-def-tip-vis        not  = "S" and
                     w-def-tip-vis        not  = "L" and
                     w-def-tip-vis        not  = "P"
                     go to acc-tip-vis-ge1-100.
           move      w-def-tip-vis        to   rr-tip-vis             .
       acc-tip-vis-ge1-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tvs-ge1-lun    to   v-car                  .
           move      w-exp-tvs-ge1-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      "SLP#"               to   v-msk                  .
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
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-tip-vis-ge1-999.
       acc-tip-vis-ge1-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        v-num                =    01
                     move  "S"            to   rr-tip-vis
           else if   v-num                =    02
                     move  "L"            to   rr-tip-vis
           else if   v-num                =    03
                     move  "P"            to   rr-tip-vis
           else      move  spaces         to   rr-tip-vis             .
       acc-tip-vis-ge1-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Che sia accettabile                         *
      *                  *---------------------------------------------*
           if        rr-tip-vis           not  = "S" and
                     rr-tip-vis           not  = "L" and
                     rr-tip-vis           not  = "P"
                     go to acc-tip-vis-ge1-100.
       acc-tip-vis-ge1-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Memorizzazione valore di default generale   *
      *                  *---------------------------------------------*
           move      rr-tip-vis           to   w-def-tip-vis          .
      *                  *---------------------------------------------*
      *                  * Confronto con valore precedente             *
      *                  *---------------------------------------------*
           if        rr-tip-vis           =    w-pre-tip-vis
                     go to  acc-tip-vis-ge1-800.
      *                  *---------------------------------------------*
      *                  * Aggiornamento valore precedente             *
      *                  *---------------------------------------------*
           move      rr-tip-vis           to   w-pre-tip-vis          .
      *                  *---------------------------------------------*
      *                  * Video in 'OFF'                              *
      *                  *---------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Erase line 14-21                            *
      *                  *---------------------------------------------*
           move      "EL"                 to   v-ope                  .
           move      14                   to   v-lin                  .
           move      21                   to   v-lto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione prompts                     *
      *                  *---------------------------------------------*
           perform   pmt-ric-sel-000      thru pmt-ric-sel-999        .
      *                  *---------------------------------------------*
      *                  * Visualizzazione richieste                   *
      *                  *---------------------------------------------*
           perform   vis-ric-sel-000      thru vis-ric-sel-999        .
      *                  *---------------------------------------------*
      *                  * Video in 'ON'                               *
      *                  *---------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-tip-vis-ge1-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-tip-vis-ge1-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-tip-vis-ge1-100.
       acc-tip-vis-ge1-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Tipo visualizzazione              *
      *    *                                                           *
      *    * Se movimenti di generale, per sottoconto                  *
      *    *-----------------------------------------------------------*
       vis-tip-vis-ge1-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo visualizzazione *
      *              *-------------------------------------------------*
           if        rr-mod-int           =    spaces
                     go to vis-tip-vis-ge1-800
           else if   rr-mod-int           =    "S"
                     go to vis-tip-vis-ge1-100
           else      go to vis-tip-vis-ge1-800.
       vis-tip-vis-ge1-100.
      *              *-------------------------------------------------*
      *              * Se movimenti di Generale                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione                             *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tvs-ge1-lun    to   v-car                  .
           move      w-exp-tvs-ge1-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
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
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     vis-tip-vis-ge1-999.
       vis-tip-vis-ge1-800.
      *              *-------------------------------------------------*
      *              * In tutti gli altri casi                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione                             *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     vis-tip-vis-ge1-999.
       vis-tip-vis-ge1-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Tipo visualizzazione                 *
      *    *                                                           *
      *    * Se movimenti di generale, per data                        *
      *    *-----------------------------------------------------------*
       acc-tip-vis-ge2-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        rr-tip-int           not  = "G"
                     go to acc-tip-vis-ge2-999.
           if        rr-mod-int           not  = "R"
                     go to acc-tip-vis-ge2-999.
      *                  *---------------------------------------------*
      *                  * Preparazione eventuale default              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se il campo non e' a spaces : nessun    *
      *                      * default                                 *
      *                      *-----------------------------------------*
           if        rr-tip-vis           not  = spaces
                     go to acc-tip-vis-ge2-100.
      *                      *-----------------------------------------*
      *                      * Se il default generale e' a spaces :    *
      *                      * default 'L'                             *
      *                      *-----------------------------------------*
           if        w-def-tip-vis        =    spaces
                     move  "L"            to   rr-tip-vis
                     go to acc-tip-vis-ge2-100.
      *                      *-----------------------------------------*
      *                      * Test di compatibilta' per il default    *
      *                      *-----------------------------------------*
           if        w-def-tip-vis        not  = "T" and
                     w-def-tip-vis        not  = "L"
                     go to acc-tip-vis-ge2-100.
           move      w-def-tip-vis        to   rr-tip-vis             .
       acc-tip-vis-ge2-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tvs-ge2-lun    to   v-car                  .
           move      w-exp-tvs-ge2-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      "TL#"                to   v-msk                  .
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
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-tip-vis-ge2-999.
       acc-tip-vis-ge2-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        v-num                =    01
                     move  "T"            to   rr-tip-vis
           else if   v-num                =    02
                     move  "L"            to   rr-tip-vis
           else      move  spaces         to   rr-tip-vis             .
       acc-tip-vis-ge2-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Che sia accettabile                         *
      *                  *---------------------------------------------*
           if        rr-tip-vis           not  = "T" and
                     rr-tip-vis           not  = "L"
                     go to acc-tip-vis-ge2-100.
       acc-tip-vis-ge2-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Memorizzazione valore di default generale   *
      *                  *---------------------------------------------*
           move      rr-tip-vis           to   w-def-tip-vis          .
      *                  *---------------------------------------------*
      *                  * Confronto con valore precedente             *
      *                  *---------------------------------------------*
           if        rr-tip-vis           =    w-pre-tip-vis
                     go to  acc-tip-vis-ge2-800.
      *                  *---------------------------------------------*
      *                  * Aggiornamento valore precedente             *
      *                  *---------------------------------------------*
           move      rr-tip-vis           to   w-pre-tip-vis          .
      *                  *---------------------------------------------*
      *                  * Video in 'OFF'                              *
      *                  *---------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Erase line 14-21                            *
      *                  *---------------------------------------------*
           move      "EL"                 to   v-ope                  .
           move      14                   to   v-lin                  .
           move      21                   to   v-lto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione prompts                     *
      *                  *---------------------------------------------*
           perform   pmt-ric-sel-000      thru pmt-ric-sel-999        .
      *                  *---------------------------------------------*
      *                  * Visualizzazione richieste                   *
      *                  *---------------------------------------------*
           perform   vis-ric-sel-000      thru vis-ric-sel-999        .
      *                  *---------------------------------------------*
      *                  * Video in 'ON'                               *
      *                  *---------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-tip-vis-ge2-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-tip-vis-ge2-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-tip-vis-ge2-100.
       acc-tip-vis-ge2-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Tipo visualizzazione              *
      *    *                                                           *
      *    * Se movimenti di generale, per data                        *
      *    *-----------------------------------------------------------*
       vis-tip-vis-ge2-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo visualizzazione *
      *              *-------------------------------------------------*
           if        rr-tip-int           =    spaces
                     go to vis-tip-vis-ge2-800
           else if   rr-tip-int           not  = "G"
                     go to vis-tip-vis-ge2-800
           else if   rr-mod-int           =    "R" or
                     rr-mod-int           =    "I"
                     go to vis-tip-vis-ge2-100
           else      go to vis-tip-vis-ge2-800.
       vis-tip-vis-ge2-100.
      *              *-------------------------------------------------*
      *              * Se movimenti di Generale                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione                             *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tvs-ge2-lun    to   v-car                  .
           move      w-exp-tvs-ge2-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      12                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-tvs-ge2-tbl    to   v-txt                  .
           if        rr-tip-vis           =    "T"
                     move  01             to   v-num
           else if   rr-tip-vis           =    "L"
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     vis-tip-vis-ge2-999.
       vis-tip-vis-ge2-800.
      *              *-------------------------------------------------*
      *              * In tutti gli altri casi                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione                             *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     vis-tip-vis-ge2-999.
       vis-tip-vis-ge2-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Tipo visualizzazione                 *
      *    *                                                           *
      *    * Se movimenti Clienti o Fornitori                          *
      *    *-----------------------------------------------------------*
       acc-tip-vis-cof-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        rr-tip-int           not  = "C" and
                     rr-tip-int           not  = "F"
                     go to acc-tip-vis-cof-999.
      *                  *---------------------------------------------*
      *                  * Preparazione eventuale default              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se il campo non e' a spaces : nessun    *
      *                      * default                                 *
      *                      *-----------------------------------------*
           if        rr-tip-vis           not  = spaces
                     go to acc-tip-vis-cof-100.
      *                      *-----------------------------------------*
      *                      * Se il default generale e' a spaces :    *
      *                      * default 'L'                             *
      *                      *-----------------------------------------*
           if        w-def-tip-vis        =    spaces
                     move  "L"            to   rr-tip-vis
                     go to acc-tip-vis-cof-100.
      *                      *-----------------------------------------*
      *                      * Test di compatibilta' per il default    *
      *                      *-----------------------------------------*
           if        w-def-tip-vis        not  = "S" and
                     w-def-tip-vis        not  = "L" and
                     w-def-tip-vis        not  = "E" and
                     w-def-tip-vis        not  = "P"
                     go to acc-tip-vis-cof-100.
           move      w-def-tip-vis        to   rr-tip-vis             .
       acc-tip-vis-cof-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tvs-cof-lun    to   v-car                  .
           move      w-exp-tvs-cof-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      "SLEP#"              to   v-msk                  .
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
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-tip-vis-cof-999.
       acc-tip-vis-cof-200.
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
       acc-tip-vis-cof-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Che sia accettabile                         *
      *                  *---------------------------------------------*
           if        rr-tip-vis           not  = "S" and
                     rr-tip-vis           not  = "L" and
                     rr-tip-vis           not  = "E" and
                     rr-tip-vis           not  = "P"
                     go to acc-tip-vis-cof-100.
       acc-tip-vis-cof-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Memorizzazione valore di default generale   *
      *                  *---------------------------------------------*
           move      rr-tip-vis           to   w-def-tip-vis          .
      *                  *---------------------------------------------*
      *                  * Confronto con valore precedente             *
      *                  *---------------------------------------------*
           if        rr-tip-vis           =    w-pre-tip-vis
                     go to  acc-tip-vis-cof-800.
      *                  *---------------------------------------------*
      *                  * Aggiornamento valore precedente             *
      *                  *---------------------------------------------*
           move      rr-tip-vis           to   w-pre-tip-vis          .
      *                  *---------------------------------------------*
      *                  * Video in 'OFF'                              *
      *                  *---------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Erase line 14-21                            *
      *                  *---------------------------------------------*
           move      "EL"                 to   v-ope                  .
           move      14                   to   v-lin                  .
           move      21                   to   v-lto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione prompts                     *
      *                  *---------------------------------------------*
           perform   pmt-ric-sel-000      thru pmt-ric-sel-999        .
      *                  *---------------------------------------------*
      *                  * Visualizzazione richieste                   *
      *                  *---------------------------------------------*
           perform   vis-ric-sel-000      thru vis-ric-sel-999        .
      *                  *---------------------------------------------*
      *                  * Video in 'ON'                               *
      *                  *---------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-tip-vis-cof-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-tip-vis-cof-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-tip-vis-cof-100.
       acc-tip-vis-cof-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Tipo visualizzazione              *
      *    *                                                           *
      *    * Se movimenti Clienti o Fornitori                          *
      *    *-----------------------------------------------------------*
       vis-tip-vis-cof-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo visualizzazione *
      *              *-------------------------------------------------*
           if        rr-tip-int           =    spaces
                     go to vis-tip-vis-cof-800
           else if   rr-tip-int           =    "C" or
                     rr-tip-int           =    "F"
                     go to vis-tip-vis-cof-100
           else      go to vis-tip-vis-cof-800.
       vis-tip-vis-cof-100.
      *              *-------------------------------------------------*
      *              * Se movimenti Clienti o Fornitori                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione                             *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tvs-cof-lun    to   v-car                  .
           move      w-exp-tvs-cof-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
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
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     vis-tip-vis-cof-999.
       vis-tip-vis-cof-800.
      *              *-------------------------------------------------*
      *              * In tutti gli altri casi                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione                             *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     vis-tip-vis-cof-999.
       vis-tip-vis-cof-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Data registrazione minima da ricercare     *
      *    *-----------------------------------------------------------*
       acc-dat-ini-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione eventuale default              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se il campo non e' a zero : nessun      *
      *                      * default                                 *
      *                      *-----------------------------------------*
           if        rr-dat-ini           not  = zero
                     go to acc-dat-ini-100.
      *                      *-----------------------------------------*
      *                      * Se il default generale e' a zero :      *
      *                      * nessun default                          *
      *                      *-----------------------------------------*
           if        w-def-dat-att        =    zero
                     go to acc-dat-ini-100.
      *                      *-----------------------------------------*
      *                      * Test se default da preparare            *
      *                      *-----------------------------------------*
           if        w-def-tip-vis        not  = "S" and
                     w-def-tip-vis        not  = "E"
                     go to acc-dat-ini-100.
           move      w-def-dat-att        to   rr-dat-ini             .
       acc-dat-ini-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      14                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-dat-ini           to   v-dat                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-dat-ini-150.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-dat-ini-999.
       acc-dat-ini-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-dat                to   rr-dat-ini             .
       acc-dat-ini-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su tipo visualizzazione                *
      *                  *---------------------------------------------*
           if        rr-dat-ini           not  = zero
                     go to acc-dat-ini-600.
           if        rr-tip-vis           not  = "S" and
                     rr-tip-vis           not  = "P"
                     go to acc-dat-ini-600.
           if        v-key                =    "UP  "
                     go to acc-dat-ini-600.
           go to     acc-dat-ini-100.
       acc-dat-ini-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-dat-ini-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-dat-ini-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-dat-ini-100.
       acc-dat-ini-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Data registrazione minima da ricercare  *
      *    *-----------------------------------------------------------*
       vis-dat-ini-000.
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      14                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-dat-ini           to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       vis-dat-ini-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Data registrazione massima da ricercare    *
      *    *-----------------------------------------------------------*
       acc-dat-fin-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        rr-tip-vis           not  = "L" and
                     rr-tip-vis           not  = "T" and
                     rr-tip-vis           not  = "P"
                     go to acc-dat-fin-999.
       acc-dat-fin-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      15                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-dat-fin           to   v-dat                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-dat-fin-150.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-dat-fin-999.
       acc-dat-fin-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-dat                to   rr-dat-fin             .
       acc-dat-fin-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-dat-fin-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-dat-fin-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-dat-fin-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-dat-fin-100.
       acc-dat-fin-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Data registrazione massima da ricercare *
      *    *-----------------------------------------------------------*
       vis-dat-fin-000.
      *              *-------------------------------------------------*
      *              * Test se campo da visualizzare                   *
      *              *-------------------------------------------------*
           if        rr-tip-vis           not  = "L" and
                     rr-tip-vis           not  = "T" and
                     rr-tip-vis           not  = "P"
                     go to vis-dat-fin-999.
       vis-dat-fin-100.
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      15                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-dat-fin           to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       vis-dat-fin-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Codice causale                             *
      *    *-----------------------------------------------------------*
       acc-sel-cau-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        rr-tip-vis           not  = "L" and
                     rr-tip-vis           not  = "T"
                     go to acc-sel-cau-999.
       acc-sel-cau-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-zcc-ope      .
           move      rr-sel-cau           to   w-cod-mne-zcc-cod      .
           move      18                   to   w-cod-mne-zcc-lin      .
           move      30                   to   w-cod-mne-zcc-pos      .
           move      18                   to   w-cod-mne-zcc-dln      .
           move      41                   to   w-cod-mne-zcc-dps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-mne-zcc-cll-000  thru cod-mne-zcc-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-mne-zcc-foi-000  thru cod-mne-zcc-foi-999    .
       acc-sel-cau-110.
           perform   cod-mne-zcc-cll-000  thru cod-mne-zcc-cll-999    .
           if        w-cod-mne-zcc-ope    =    "F+"
                     go to acc-sel-cau-115.
           if        w-cod-mne-zcc-ope    =    "AC"
                     go to acc-sel-cau-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-sel-cau-115.
           perform   cod-mne-zcc-foi-000  thru cod-mne-zcc-foi-999    .
           go to     acc-sel-cau-110.
       acc-sel-cau-120.
           move      w-cod-mne-zcc-cod    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-sel-cau-999.
       acc-sel-cau-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-sel-cau             .
       acc-sel-cau-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura archivio [zcc]                      *
      *                  *---------------------------------------------*
           move      rr-sel-cau           to   w-let-arc-zcc-cod      .
           perform   let-arc-zcc-000      thru let-arc-zcc-999        .
           move      w-let-arc-zcc-des    to   rr-sel-cau-des         .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione                 *
      *                  *---------------------------------------------*
           perform   vis-sel-cau-des-000  thru vis-sel-cau-des-999    .
      *                  *---------------------------------------------*
      *                  * Se codice errato : reimpostazione           *
      *                  *---------------------------------------------*
           if        w-let-arc-zcc-flg    not  = spaces
                     go to acc-sel-cau-100.
       acc-sel-cau-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-sel-cau-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-sel-cau-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-sel-cau-100.
       acc-sel-cau-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Codice causale da ricercare             *
      *    *-----------------------------------------------------------*
       vis-sel-cau-000.
      *              *-------------------------------------------------*
      *              * Test se campo da visualizzare                   *
      *              *-------------------------------------------------*
           if        rr-tip-vis           not  = "L" and
                     rr-tip-vis           not  = "T"
                     go to vis-sel-cau-999.
       vis-sel-cau-100.
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      18                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-sel-cau           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-sel-cau-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Descrizione codice causale da ricercare *
      *    *-----------------------------------------------------------*
       vis-sel-cau-des-000.
      *              *-------------------------------------------------*
      *              * Test se campo da visualizzare                   *
      *              *-------------------------------------------------*
           if        rr-tip-vis           not  = "L" and
                     rr-tip-vis           not  = "T"
                     go to vis-sel-cau-des-999.
       vis-sel-cau-des-100.
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      rr-sel-cau-des       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-sel-cau-des-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Contenuto causale                    *
      *    *-----------------------------------------------------------*
       acc-sel-cca-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        rr-tip-vis           not  = "L"
                     go to acc-sel-cca-999.
       acc-sel-cca-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-ric    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      rr-sel-cca           to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-sel-cca-999.
       acc-sel-cca-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   rr-sel-cca             .
       acc-sel-cca-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-sel-cca-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-sel-cca-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-sel-cca-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-sel-cca-100.
       acc-sel-cca-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione Contenuto causale                         *
      *    *-----------------------------------------------------------*
       vis-sel-cca-000.
      *              *-------------------------------------------------*
      *              * Test se campo da visualizzare                   *
      *              *-------------------------------------------------*
           if        rr-tip-vis           not  = "L"
                     go to vis-sel-cca-999.
       vis-sel-cca-100.
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-sel-cca           to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-sel-cca-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Contenuto commento                   *
      *    *-----------------------------------------------------------*
       acc-sel-cco-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        rr-tip-vis           not  = "L"
                     go to acc-sel-cco-999.
           if        rr-tip-int           =    "G"   and
                     rr-mod-int           not  = "S"
                     go to acc-sel-cco-999.
       acc-sel-cco-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-ric    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      rr-sel-cco           to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-sel-cco-999.
       acc-sel-cco-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   rr-sel-cco             .
       acc-sel-cco-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-sel-cco-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-sel-cco-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-sel-cco-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-sel-cco-100.
       acc-sel-cco-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione Contenuto commento                        *
      *    *-----------------------------------------------------------*
       vis-sel-cco-000.
      *              *-------------------------------------------------*
      *              * Test se campo da visualizzare                   *
      *              *-------------------------------------------------*
           if        rr-tip-vis           not  = "L"
                     go to vis-sel-cco-999.
       vis-sel-cco-100.
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-sel-cco           to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-sel-cco-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Importo da selezionare min           *
      *    *-----------------------------------------------------------*
       acc-sel-imi-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        rr-tip-vis           not  = "L"
                     go to acc-sel-imi-999.
           if        rr-tip-int           =    "G"   and
                     rr-mod-int           not  = "S" and
                     rr-mod-int           not  = "R"
                     go to acc-sel-imi-999.
       acc-sel-imi-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      13                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "<BG"                to   v-edm                  .
           move      21                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-sel-imi           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-sel-imi-999.
       acc-sel-imi-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-sel-imi             .
       acc-sel-imi-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-sel-imi-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-sel-imi-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-sel-imi-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-sel-imi-100.
       acc-sel-imi-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione Importo min                               *
      *    *-----------------------------------------------------------*
       vis-sel-imi-000.
      *              *-------------------------------------------------*
      *              * Test se campo da visualizzare                   *
      *              *-------------------------------------------------*
           if        rr-tip-vis           not  = "L"
                     go to vis-sel-imi-999.
       vis-sel-imi-100.
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      13                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "<BG"                to   v-edm                  .
           move      21                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-edt-cod-pdc-edt    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-sel-imi-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Importo da selezionare max           *
      *    *-----------------------------------------------------------*
       acc-sel-ima-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        rr-tip-vis           not  = "L"
                     go to acc-sel-ima-999.
           if        rr-tip-int           =    "G"   and
                     rr-mod-int           not  = "S" and
                     rr-mod-int           not  = "R"
                     go to acc-sel-ima-999.
      *                  *---------------------------------------------*
      *                  * Eventuale default                           *
      *                  *---------------------------------------------*
           if        rr-sel-ima           not  = zero
                     go to acc-sel-ima-100.
           move      rr-sel-imi           to   rr-sel-ima             .
       acc-sel-ima-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      13                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "<BG"                to   v-edm                  .
           move      21                   to   v-lin                  .
           move      54                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-sel-ima           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-sel-ima-999.
       acc-sel-ima-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-sel-ima             .
       acc-sel-ima-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-sel-ima-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-sel-ima-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-sel-ima-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-sel-ima-100.
       acc-sel-ima-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione Importo max                               *
      *    *-----------------------------------------------------------*
       vis-sel-ima-000.
      *              *-------------------------------------------------*
      *              * Test se campo da visualizzare                   *
      *              *-------------------------------------------------*
           if        rr-tip-vis           not  = "L"
                     go to vis-sel-ima-999.
       vis-sel-ima-100.
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      13                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "<BG"                to   v-edm                  .
           move      21                   to   v-lin                  .
           move      54                   to   v-pos                  .
           move      w-edt-cod-pdc-edt    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-sel-ima-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Utente                               *
      *    *-----------------------------------------------------------*
       acc-sel-ute-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        rr-tip-int           not  = "G"
                     go to acc-sel-ute-999.
           if        rr-mod-int           not  = "I"
                     go to acc-sel-ute-999.
       acc-sel-ute-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "L"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-ric    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      rr-sel-ute           to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-sel-ute-999.
       acc-sel-ute-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   rr-sel-ute             .
       acc-sel-ute-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-sel-ute-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-sel-ute-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-sel-ute-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-sel-ute-100.
       acc-sel-ute-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione Utente                                    *
      *    *-----------------------------------------------------------*
       vis-sel-ute-000.
      *              *-------------------------------------------------*
      *              * Test se campo da visualizzare                   *
      *              *-------------------------------------------------*
           if        rr-mod-int           not  = "I"
                     go to vis-sel-ute-999.
       vis-sel-ute-100.
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-sel-ute           to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-sel-ute-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Fase                                 *
      *    *-----------------------------------------------------------*
       acc-sel-fas-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        rr-tip-int           not  = "G"
                     go to acc-sel-fas-999.
           if        rr-mod-int           not  = "I"
                     go to acc-sel-fas-999.
       acc-sel-fas-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "L"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      21                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-ric    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      rr-sel-fas           to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-sel-fas-999.
       acc-sel-fas-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   rr-sel-fas             .
       acc-sel-fas-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-sel-fas-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-sel-fas-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-sel-fas-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-sel-fas-100.
       acc-sel-fas-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione Fase                                      *
      *    *-----------------------------------------------------------*
       vis-sel-fas-000.
      *              *-------------------------------------------------*
      *              * Test se campo da visualizzare                   *
      *              *-------------------------------------------------*
           if        rr-mod-int           not  = "I"
                     go to vis-sel-fas-999.
       vis-sel-fas-100.
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      21                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-sel-fas           to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-sel-fas-999.
           exit.

      *    *===========================================================*
      *    * Editing del codice sottoconto con appoggio a sx o dx      *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/wedtpdc0.wks"                   .

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
           move      spaces               to   rr-cod-arc-rag         .
           move      spaces               to   rr-cod-arc-via         .
           move      spaces               to   rr-cod-arc-loc         .
           move      spaces               to   rr-tip-vis             .
           move      zero                 to   rr-dat-ini             .
           move      zero                 to   rr-dat-fin             .
           move      zero                 to   rr-sel-cau             .
           move      spaces               to   rr-sel-cau-des         .
           move      zero                 to   rr-sel-imi             .
           move      zero                 to   rr-sel-ima             .
           move      spaces               to   rr-sel-cca             .
           move      spaces               to   rr-sel-cco             .
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
      *    * Controllo su tasto Do in parametri di selezione           *
      *    *-----------------------------------------------------------*
       tdo-ric-sel-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-tdo-ric-flg      .
       tdo-ric-sel-100.
      *              *-------------------------------------------------*
      *              * Controllo su Tipo interrogazione                *
      *              *-------------------------------------------------*
       tdo-ric-sel-150.
      *              *-------------------------------------------------*
      *              * Controllo su Modalita' di interrogazione        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se interrogazione di tipo 'G' e modalita'   *
      *                  * non definita : errore                       *
      *                  *---------------------------------------------*
           if        rr-tip-int           not  = "G"
                     go to  tdo-ric-sel-200.
           if        rr-mod-int           not  = spaces
                     go to  tdo-ric-sel-200.
      *                  *---------------------------------------------*
      *                  * Messaggio di errore                         *
      *                  *---------------------------------------------*
           move      "Manca la modalita' di interrogazione !            
      -              "               "    to   w-err-box-err-msg      .
      *                  *---------------------------------------------*
      *                  * Ad uscita con errore                        *
      *                  *---------------------------------------------*
           go to     tdo-ric-sel-900.
       tdo-ric-sel-200.
      *              *-------------------------------------------------*
      *              * Controllo su Codice archivio                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test a seconda delle richieste              *
      *                  * non definita : errore                       *
      *                  *---------------------------------------------*
           if        rr-tip-int           =    "G"
                     go to tdo-ric-sel-210
           else if   rr-tip-int           =    "C"
                     go to tdo-ric-sel-220
           else if   rr-tip-int           =    "F"
                     go to tdo-ric-sel-230
           else      go to tdo-ric-sel-240.
       tdo-ric-sel-210.
      *                  *---------------------------------------------*
      *                  * Se movimenti di Generale                    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se per Sottoconto                  *
      *                      *-----------------------------------------*
           if        rr-mod-int           not  = "S"
                     go to  tdo-ric-sel-250.
           if        rr-cod-arc           not  = zero
                     go to  tdo-ric-sel-250.
      *                      *-----------------------------------------*
      *                      * Messaggio di errore                     *
      *                      *-----------------------------------------*
           move      "Manca il codice Sottoconto !                      
      -              "               "    to   w-err-box-err-msg      .
      *                      *-----------------------------------------*
      *                      * Ad uscita con errore                    *
      *                      *-----------------------------------------*
           go to     tdo-ric-sel-900.
       tdo-ric-sel-220.
      *                  *---------------------------------------------*
      *                  * Se movimenti Clienti                        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test                                    *
      *                      *-----------------------------------------*
           if        rr-cod-arc           not  = zero
                     go to  tdo-ric-sel-250.
      *                      *-----------------------------------------*
      *                      * Messaggio di errore                     *
      *                      *-----------------------------------------*
           move      "Manca il codice Cliente !                         
      -              "               "    to   w-err-box-err-msg      .
      *                      *-----------------------------------------*
      *                      * Ad uscita con errore                    *
      *                      *-----------------------------------------*
           go to     tdo-ric-sel-900.
       tdo-ric-sel-230.
      *                  *---------------------------------------------*
      *                  * Se movimenti Fornitori                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test                                    *
      *                      *-----------------------------------------*
           if        rr-cod-arc           not  = zero
                     go to  tdo-ric-sel-250.
      *                      *-----------------------------------------*
      *                      * Messaggio di errore                     *
      *                      *-----------------------------------------*
           move      "Manca il codice Fornitore !                       
      -              "               "    to   w-err-box-err-msg      .
      *                      *-----------------------------------------*
      *                      * Ad uscita con errore                    *
      *                      *-----------------------------------------*
           go to     tdo-ric-sel-900.
       tdo-ric-sel-240.
      *                  *---------------------------------------------*
      *                  * Se tipo movimento non riconosciuto          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test                                    *
      *                      *-----------------------------------------*
           if        rr-cod-arc           not  = zero
                     go to  tdo-ric-sel-250.
      *                      *-----------------------------------------*
      *                      * Messaggio di errore                     *
      *                      *-----------------------------------------*
           move      "Tipo movimenti non riconosciuto !                 
      -              "               "    to   w-err-box-err-msg      .
      *                      *-----------------------------------------*
      *                      * Ad uscita con errore                    *
      *                      *-----------------------------------------*
           go to     tdo-ric-sel-900.
       tdo-ric-sel-250.
      *              *-------------------------------------------------*
      *              * Controllo su Tipo visualizzazione               *
      *              *-------------------------------------------------*
       tdo-ric-sel-300.
      *              *-------------------------------------------------*
      *              * Controllo su Data registrazione iniziale        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo visualizza- *
      *                  * zione                                       *
      *                  *---------------------------------------------*
           if        rr-tip-vis           =   "L"
                     go to tdo-ric-sel-305
           else if   rr-tip-vis           =   "S"
                     go to tdo-ric-sel-310
           else if   rr-tip-vis           =   "P"
                     go to tdo-ric-sel-320
           else if   rr-tip-vis           =   "E"
                     go to tdo-ric-sel-330
           else      go to tdo-ric-sel-350.
       tdo-ric-sel-305.
      *                  *---------------------------------------------*
      *                  * Se Lista per data                           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test                                    *
      *                      *-----------------------------------------*
           if        rr-dat-fin           =    zero
                     go to tdo-ric-sel-350.
           if        rr-dat-fin           not  < rr-dat-ini
                     go to tdo-ric-sel-350.
      *                      *-----------------------------------------*
      *                      * Messaggio di errore                     *
      *                      *-----------------------------------------*
           move      "La data massima non puo' essere inferiore a quella
      -              " minima !      "    to   w-err-box-err-msg      .
      *                      *-----------------------------------------*
      *                      * Ad uscita con errore                    *
      *                      *-----------------------------------------*
           go to     tdo-ric-sel-900.
       tdo-ric-sel-310.
      *                  *---------------------------------------------*
      *                  * Se Saldo ad una data                        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test                                    *
      *                      *-----------------------------------------*
           if        rr-dat-ini           not  = zero
                     go to  tdo-ric-sel-350.
      *                      *-----------------------------------------*
      *                      * Messaggio di errore                     *
      *                      *-----------------------------------------*
           move      "Manca la data per il saldo !                      
      -              "               "    to   w-err-box-err-msg      .
      *                      *-----------------------------------------*
      *                      * Ad uscita con errore                    *
      *                      *-----------------------------------------*
           go to     tdo-ric-sel-900.
       tdo-ric-sel-320.
      *                  *---------------------------------------------*
      *                  * Se Partitario                               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test                                    *
      *                      *-----------------------------------------*
           if        rr-dat-ini           not  = zero
                     go to  tdo-ric-sel-350.
      *                      *-----------------------------------------*
      *                      * Messaggio di errore                     *
      *                      *-----------------------------------------*
           move      "Manca la data iniziale per il partitario !        
      -              "               "    to   w-err-box-err-msg      .
      *                      *-----------------------------------------*
      *                      * Ad uscita con errore                    *
      *                      *-----------------------------------------*
           go to     tdo-ric-sel-900.
       tdo-ric-sel-330.
      *                  *---------------------------------------------*
      *                  * Se Estratto conto                           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test                                    *
      *                      *-----------------------------------------*
           if        rr-dat-ini           not  = zero
                     go to  tdo-ric-sel-350.
      *                      *-----------------------------------------*
      *                      * Messaggio di errore                     *
      *                      *-----------------------------------------*
           move      "Manca la data per l'estratto conto !              
      -              "               "    to   w-err-box-err-msg      .
      *                      *-----------------------------------------*
      *                      * Ad uscita con errore                    *
      *                      *-----------------------------------------*
           go to     tdo-ric-sel-900.
       tdo-ric-sel-350.
      *              *-------------------------------------------------*
      *              * Controllo su Data registrazione finale          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo visualizza- *
      *                  * zione                                       *
      *                  *---------------------------------------------*
           if        rr-tip-vis           =   "P"
                     go to tdo-ric-sel-360
           else      go to tdo-ric-sel-500.
       tdo-ric-sel-360.
      *                  *---------------------------------------------*
      *                  * Se Partitario ad una data                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se zero                            *
      *                      *-----------------------------------------*
           if        rr-dat-fin           not  = zero
                     go to  tdo-ric-sel-365.
      *                      *-----------------------------------------*
      *                      * Oltre                                   *
      *                      *-----------------------------------------*
           go to     tdo-ric-sel-500.
       tdo-ric-sel-365.
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
                     go to  tdo-ric-sel-370
           else      go to  tdo-ric-sel-800.
       tdo-ric-sel-370.
      *                      *-----------------------------------------*
      *                      * Messaggio di errore                     *
      *                      *-----------------------------------------*
           move      "Nel caso di visualizzazione 'Partitario', la data 
      -              "iniziale e  "       to   w-err-box-err-msg      .
           move      "quella finale devono appartenere allo stesso eserc
      -              "izio !      "       to   w-err-box-err-m02      .
      *                      *-----------------------------------------*
      *                      * Box di errore                           *
      *                      *-----------------------------------------*
           perform   box-msg-e02-000      thru box-msg-e02-999        .
      *                      *-----------------------------------------*
      *                      * Ad uscita con errore                    *
      *                      *-----------------------------------------*
           go to     tdo-ric-sel-920.
       tdo-ric-sel-500.
      *              *-------------------------------------------------*
      *              * Controllo su Selezione importo min - max        *
      *              *-------------------------------------------------*
           if        rr-sel-ima           =    zero
                     go to tdo-ric-sel-800.
           if        rr-sel-ima           not  < rr-sel-imi
                     go to tdo-ric-sel-800.
           move      "L'importo massimo non puo' essere inferiore al min
      -              "imo !  "            to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-800.
      *              *-------------------------------------------------*
      *              * Uscita per controlli superati                   *
      *              *-------------------------------------------------*
           go to     tdo-ric-sel-999.
       tdo-ric-sel-900.
      *              *-------------------------------------------------*
      *              * Uscita con errori                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Messaggio di errore                         *
      *                  *---------------------------------------------*
           perform   box-msg-err-000      thru box-msg-err-999        .
       tdo-ric-sel-920.
      *                  *---------------------------------------------*
      *                  * Flag di uscita                              *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cnt-tdo-ric-flg      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     tdo-ric-sel-999.
       tdo-ric-sel-999.
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
                     move   w-def-dat-att to   rr-dat-ini             .
      *              *-------------------------------------------------*
      *              * Data registrazione finale                       *
      *              *-------------------------------------------------*
           if        rr-dat-fin           not  = zero
                     go to  reg-ric-sel-200.
      *                  *---------------------------------------------*
      *                  * Test se richiesto                           *
      *                  *---------------------------------------------*
           if        rr-tip-vis           =    "S" or
                     rr-tip-vis           =    "E"
                     go to  reg-ric-sel-200.
      *                  *---------------------------------------------*
      *                  * Regolarizzazione                            *
      *                  *---------------------------------------------*
           if        rr-dat-ini           =    zero
                     move   9991231       to   rr-dat-fin
           else      move   rr-dat-ini    to   rr-dat-fin             .
       reg-ric-sel-200.
      *              *-------------------------------------------------*
      *              * Selezione su importo min - max                  *
      *              *-------------------------------------------------*
           if        rr-sel-imi           =    zero and
                     rr-sel-ima           =    zero
                     move  -9999999999999 to   rr-sel-imi
                     move   9999999999999 to   rr-sel-ima
           else if   rr-sel-imi           not  = zero and
                     rr-sel-ima           =    zero
                     move  rr-sel-imi     to   rr-sel-ima             .
       reg-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Esecuzione programma di interrogazione                    *
      *    *-----------------------------------------------------------*
       ese-prg-int-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo interrogazione  *
      *              *-------------------------------------------------*
           if        rr-tip-int           =    "G"
                     go to ese-prg-int-100
           else if   rr-tip-int           =    "C"
                     go to ese-prg-int-300
           else if   rr-tip-int           =    "F"
                     go to ese-prg-int-500
           else      go to ese-prg-int-900.
       ese-prg-int-100.
      *              *-------------------------------------------------*
      *              * Se movimenti di Generale                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione della modalita' di   *
      *                  * interrogazione                              *
      *                  *---------------------------------------------*
           if        rr-mod-int           =    "S"
                     go to ese-prg-int-120
           else if   rr-mod-int           =    "R"
                     go to ese-prg-int-140
           else if   rr-mod-int           =    "I"
                     go to ese-prg-int-160
           else      go to ese-prg-int-900.
       ese-prg-int-120.
      *                  *---------------------------------------------*
      *                  * Se per Sottoconto                           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Deviazione in funzione del tipo di      *
      *                      * visualizzazione                         *
      *                      *-----------------------------------------*
           if        rr-tip-vis           =    "S"
                     go to ese-prg-int-122
           else if   rr-tip-vis           =    "L"
                     go to ese-prg-int-124
           else if   rr-tip-vis           =    "P"
                     go to ese-prg-int-126
           else      go to ese-prg-int-900.
       ese-prg-int-122.
      *                      *-----------------------------------------*
      *                      * Se Saldo ad una data                    *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Richiamo programma di esecuzione    *
      *                          *-------------------------------------*
           call      "pgm/cge/prg/obj/pcge301a"
                                         using rr                     .
           cancel    "pgm/cge/prg/obj/pcge301a"                       .
      *                          *-------------------------------------*
      *                          * Ad uscita                           *
      *                          *-------------------------------------*
           go to     ese-prg-int-900.
       ese-prg-int-124.
      *                      *-----------------------------------------*
      *                      * Se Lista movimenti                      *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Richiamo programma di esecuzione    *
      *                          *-------------------------------------*
           call      "pgm/cge/prg/obj/pcge3011"
                                         using rr                     .
           cancel    "pgm/cge/prg/obj/pcge3011"                       .
      *                          *-------------------------------------*
      *                          * Ad uscita                           *
      *                          *-------------------------------------*
           go to     ese-prg-int-900.
       ese-prg-int-126.
      *                      *-----------------------------------------*
      *                      * Se Partitario                           *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Richiamo programma di esecuzione    *
      *                          *-------------------------------------*
           call      "pgm/cge/prg/obj/pcge3014"
                                         using rr                     .
           cancel    "pgm/cge/prg/obj/pcge3014"                       .
      *                          *-------------------------------------*
      *                          * Ad uscita                           *
      *                          *-------------------------------------*
           go to     ese-prg-int-900.
       ese-prg-int-140.
      *                  *---------------------------------------------*
      *                  * Se per Data registrazione                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Deviazione in funzione del tipo di      *
      *                      * visualizzazione                         *
      *                      *-----------------------------------------*
           if        rr-tip-vis           =    "T"
                     go to ese-prg-int-142
           else if   rr-tip-vis           =    "L"
                     go to ese-prg-int-144
           else      go to ese-prg-int-900.
       ese-prg-int-142.
      *                      *-----------------------------------------*
      *                      * Se Totale movimenti                     *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Richiamo programma di esecuzione    *
      *                          *-------------------------------------*
           call      "pgm/cge/prg/obj/pcge3018"
                                         using rr                     .
           cancel    "pgm/cge/prg/obj/pcge3018"                       .
      *                          *-------------------------------------*
      *                          * Ad uscita                           *
      *                          *-------------------------------------*
           go to     ese-prg-int-900.
       ese-prg-int-144.
      *                      *-----------------------------------------*
      *                      * Se Lista movimenti                      *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Richiamo programma di esecuzione    *
      *                          *-------------------------------------*
           call      "pgm/cge/prg/obj/pcge3018"
                                         using rr                     .
           cancel    "pgm/cge/prg/obj/pcge3018"                       .
      *                          *-------------------------------------*
      *                          * Ad uscita                           *
      *                          *-------------------------------------*
           go to     ese-prg-int-900.
       ese-prg-int-160.
      *                  *---------------------------------------------*
      *                  * Se per Data immissione/ultima modofica      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Deviazione in funzione del tipo di      *
      *                      * visualizzazione                         *
      *                      *-----------------------------------------*
           if        rr-tip-vis           =    "T"
                     go to ese-prg-int-162
           else if   rr-tip-vis           =    "L"
                     go to ese-prg-int-164
           else      go to ese-prg-int-900.
       ese-prg-int-162.
      *                      *-----------------------------------------*
      *                      * Se Totale movimenti                     *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Richiamo programma di esecuzione    *
      *                          *-------------------------------------*
           call      "pgm/cge/prg/obj/pcge3019"
                                         using rr                     .
           cancel    "pgm/cge/prg/obj/pcge3019"                       .
      *                          *-------------------------------------*
      *                          * Ad uscita                           *
      *                          *-------------------------------------*
           go to     ese-prg-int-900.
       ese-prg-int-164.
      *                      *-----------------------------------------*
      *                      * Se Lista movimenti                      *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Richiamo programma di esecuzione    *
      *                          *-------------------------------------*
           call      "pgm/cge/prg/obj/pcge3019"
                                         using rr                     .
           cancel    "pgm/cge/prg/obj/pcge3019"                       .
      *                          *-------------------------------------*
      *                          * Ad uscita                           *
      *                          *-------------------------------------*
           go to     ese-prg-int-900.
       ese-prg-int-300.
      *              *-------------------------------------------------*
      *              * Se movimenti Clienti                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo di visua-   *
      *                  * lizzazione                                  *
      *                  *---------------------------------------------*
           if        rr-tip-vis           =    "S"
                     go to ese-prg-int-310
           else if   rr-tip-vis           =    "L"
                     go to ese-prg-int-320
           else if   rr-tip-vis           =    "P"
                     go to ese-prg-int-330
           else if   rr-tip-vis           =    "E"
                     go to ese-prg-int-340
           else      go to ese-prg-int-900.
       ese-prg-int-310.
      *                  *---------------------------------------------*
      *                  * Se Saldo ad una data                        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Richiamo programma di esecuzione        *
      *                      *-----------------------------------------*
           call      "pgm/cge/prg/obj/pcge301b"
                                         using rr                     .
           cancel    "pgm/cge/prg/obj/pcge301b"                       .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     ese-prg-int-900.
       ese-prg-int-320.
      *                  *---------------------------------------------*
      *                  * Se Lista movimenti                          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Richiamo programma di esecuzione        *
      *                      *-----------------------------------------*
           call      "pgm/cge/prg/obj/pcge3012"
                                         using rr                     .
           cancel    "pgm/cge/prg/obj/pcge3012"                       .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     ese-prg-int-900.
       ese-prg-int-330.
      *                  *---------------------------------------------*
      *                  * Se Partitario                               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Richiamo programma di esecuzione        *
      *                      *-----------------------------------------*
           call      "pgm/cge/prg/obj/pcge3015"
                                         using rr                     .
           cancel    "pgm/cge/prg/obj/pcge3015"                       .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     ese-prg-int-900.
       ese-prg-int-340.
      *                  *---------------------------------------------*
      *                  * Se Estratto conto                           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Richiamo programma di esecuzione        *
      *                      *-----------------------------------------*
           call      "pgm/cge/prg/obj/pcge301e"
                                         using rr                     .
           cancel    "pgm/cge/prg/obj/pcge301e"                       .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     ese-prg-int-900.
       ese-prg-int-500.
      *              *-------------------------------------------------*
      *              * Se movimenti Fornitori                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo di visua-   *
      *                  * lizzazione                                  *
      *                  *---------------------------------------------*
           if        rr-tip-vis           =    "S"
                     go to ese-prg-int-510
           else if   rr-tip-vis           =    "L"
                     go to ese-prg-int-520
           else if   rr-tip-vis           =    "P"
                     go to ese-prg-int-530
           else if   rr-tip-vis           =    "E"
                     go to ese-prg-int-540
           else      go to ese-prg-int-900.
       ese-prg-int-510.
      *                  *---------------------------------------------*
      *                  * Se Saldo ad una data                        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Richiamo programma di esecuzione        *
      *                      *-----------------------------------------*
           call      "pgm/cge/prg/obj/pcge301c"
                                         using rr                     .
           cancel    "pgm/cge/prg/obj/pcge301c"                       .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     ese-prg-int-900.
       ese-prg-int-520.
      *                  *---------------------------------------------*
      *                  * Se Lista movimenti                          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Richiamo programma di esecuzione        *
      *                      *-----------------------------------------*
           call      "pgm/cge/prg/obj/pcge3013"
                                         using rr                     .
           cancel    "pgm/cge/prg/obj/pcge3013"                       .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     ese-prg-int-900.
       ese-prg-int-530.
      *                  *---------------------------------------------*
      *                  * Se Partitario                               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Richiamo programma di esecuzione        *
      *                      *-----------------------------------------*
           call      "pgm/cge/prg/obj/pcge3016"
                                         using rr                     .
           cancel    "pgm/cge/prg/obj/pcge3016"                       .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     ese-prg-int-900.
       ese-prg-int-540.
      *                  *---------------------------------------------*
      *                  * Se Estratto conto                           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Richiamo programma di esecuzione        *
      *                      *-----------------------------------------*
           call      "pgm/cge/prg/obj/pcge301f"
                                         using rr                     .
           cancel    "pgm/cge/prg/obj/pcge301f"                       .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     ese-prg-int-900.
       ese-prg-int-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     ese-prg-int-999.
       ese-prg-int-999.
           exit.

      *    *===========================================================*
      *    * Box per messaggio di errore                               *
      *    *-----------------------------------------------------------*
       box-msg-err-000.
      *              *-------------------------------------------------*
      *              * Salvataggio immagine video                      *
      *              *-------------------------------------------------*
           move      "SV"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Video in Off                                    *
      *              *-------------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Box                                             *
      *              *-------------------------------------------------*
           move      "BX"                 to   v-ope                  .
           move      12                   to   v-lin                  .
           move      04                   to   v-pos                  .
           move      14                   to   v-lto                  .
           move      77                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Messaggio nel box                               *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      65                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      06                   to   v-pos                  .
           move      w-err-box-err-msg    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Parentesi quadre di delimitazione               *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      73                   to   v-pos                  .
           move      "[ ]"                to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Video in On                                     *
      *              *-------------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Accettazione carattere di presa visione         *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "K"                  to   v-tip                  .
           move      13                   to   v-lin                  .
           move      74                   to   v-pos                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Ripristino immagine video                       *
      *              *-------------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       box-msg-err-999.
           exit.

      *    *===========================================================*
      *    * Box per messaggio di errore esteso, su due righe          *
      *    *-----------------------------------------------------------*
       box-msg-e02-000.
      *              *-------------------------------------------------*
      *              * Salvataggio immagine video                      *
      *              *-------------------------------------------------*
           move      "SV"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Video in Off                                    *
      *              *-------------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Box                                             *
      *              *-------------------------------------------------*
           move      "BX"                 to   v-ope                  .
           move      11                   to   v-lin                  .
           move      04                   to   v-pos                  .
           move      14                   to   v-lto                  .
           move      77                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Messaggio nel box                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Linea 01                                    *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      65                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      06                   to   v-pos                  .
           move      w-err-box-err-msg    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Linea 02                                    *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      65                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      06                   to   v-pos                  .
           move      w-err-box-err-m02    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Parentesi quadre di delimitazione               *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      73                   to   v-pos                  .
           move      "[ ]"                to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Video in On                                     *
      *              *-------------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Accettazione carattere di presa visione         *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "K"                  to   v-tip                  .
           move      13                   to   v-lin                  .
           move      74                   to   v-pos                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Ripristino immagine video                       *
      *              *-------------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       box-msg-e02-999.
           exit.

      *    *===========================================================*
      *    * Box per messaggio di errore esteso, su tre righe          *
      *    *-----------------------------------------------------------*
       box-msg-e03-000.
      *              *-------------------------------------------------*
      *              * Salvataggio immagine video                      *
      *              *-------------------------------------------------*
           move      "SV"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Video in Off                                    *
      *              *-------------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Box                                             *
      *              *-------------------------------------------------*
           move      "BX"                 to   v-ope                  .
           move      10                   to   v-lin                  .
           move      04                   to   v-pos                  .
           move      14                   to   v-lto                  .
           move      77                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Messaggio nel box                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Linea 01                                    *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      65                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      06                   to   v-pos                  .
           move      w-err-box-err-msg    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Linea 02                                    *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      65                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      06                   to   v-pos                  .
           move      w-err-box-err-m02    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Linea 03                                    *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      65                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      06                   to   v-pos                  .
           move      w-err-box-err-m03    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Parentesi quadre di delimitazione               *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      73                   to   v-pos                  .
           move      "[ ]"                to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Video in On                                     *
      *              *-------------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Accettazione carattere di presa visione         *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "K"                  to   v-tip                  .
           move      13                   to   v-lin                  .
           move      74                   to   v-pos                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Ripristino immagine video                       *
      *              *-------------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       box-msg-e03-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [pdc]                         *
      *    *-----------------------------------------------------------*
       let-arc-pdc-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-pdc-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice sottoconto a zero                *
      *              *-------------------------------------------------*
           if        w-let-arc-pdc-cod    =    zero
                     go to let-arc-pdc-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODPDC"             to   f-key                  .
           move      w-let-arc-pdc-cod    to   rf-pdc-cod-pdc         .
           move      "pgm/cge/fls/ioc/obj/iofpdc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdc                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-pdc-400.
       let-arc-pdc-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-pdc-des-pdc       to   w-let-arc-pdc-des      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-pdc-999.
       let-arc-pdc-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-pdc-flg      .
           move      all   "."            to   w-let-arc-pdc-des      .
           go to     let-arc-pdc-999.
       let-arc-pdc-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-pdc-des      .
       let-arc-pdc-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [cli]                         *
      *    *-----------------------------------------------------------*
       let-arc-cli-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-cli-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice cliente a zero                   *
      *              *-------------------------------------------------*
           if        w-let-arc-cli-cod    =    zero
                     go to let-arc-cli-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCLI    "         to   f-key                  .
           move      w-let-arc-cli-cod    to   rf-cli-cod-cli         .
           move      "pgm/cge/fls/ioc/obj/iofcli"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cli                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-cli-400.
       let-arc-cli-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-cli-rag-soc       to   w-let-arc-cli-rag      .
           move      rf-cli-via-cli       to   w-let-arc-cli-via      .
           move      rf-cli-loc-cli       to   w-let-arc-cli-loc      .
           move      rf-cli-cod-cge       to   w-let-arc-cli-cge      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-cli-999.
       let-arc-cli-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-cli-flg      .
           move      all   "."            to   w-let-arc-cli-rag      .
           go to     let-arc-cli-600.
       let-arc-cli-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-cli-rag      .
       let-arc-cli-600.
           move      spaces               to   w-let-arc-cli-via      .
           move      spaces               to   w-let-arc-cli-loc      .
           move      zero                 to   w-let-arc-cli-cge      .
       let-arc-cli-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [fnt]                         *
      *    *-----------------------------------------------------------*
       let-arc-fnt-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-fnt-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice fornitore a zero                 *
      *              *-------------------------------------------------*
           if        w-let-arc-fnt-cod    =    zero
                     go to let-arc-fnt-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODfnt    "         to   f-key                  .
           move      w-let-arc-fnt-cod    to   rf-fnt-cod-fnt         .
           move      "pgm/cge/fls/ioc/obj/ioffnt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fnt                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-fnt-400.
       let-arc-fnt-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-fnt-rag-soc       to   w-let-arc-fnt-rag      .
           move      rf-fnt-via-fnt       to   w-let-arc-fnt-via      .
           move      rf-fnt-loc-fnt       to   w-let-arc-fnt-loc      .
           move      rf-fnt-cod-cge       to   w-let-arc-fnt-cge      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-fnt-999.
       let-arc-fnt-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-fnt-flg      .
           move      all   "."            to   w-let-arc-fnt-rag      .
           go to     let-arc-fnt-600.
       let-arc-fnt-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-fnt-rag      .
       let-arc-fnt-600.
           move      spaces               to   w-let-arc-fnt-via      .
           move      spaces               to   w-let-arc-fnt-loc      .
           move      zero                 to   w-let-arc-fnt-cge      .
       let-arc-fnt-999.
           exit.

      *    *===========================================================*
      *    * Routine lettura tabella [zcc]                             *
      *    *-----------------------------------------------------------*
       let-arc-zcc-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zcc-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice causale a zero                   *
      *              *-------------------------------------------------*
           if        w-let-arc-zcc-cod    =    zero
                     go to let-arc-zcc-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCAU    "         to   f-key                  .
           move      w-let-arc-zcc-cod    to   rf-zcc-cod-cau         .
           move      "pgm/cge/fls/ioc/obj/iofzcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zcc                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-zcc-400.
       let-arc-zcc-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-zcc-des-cau       to   w-let-arc-zcc-des      .
           move      rf-zcc-tip-moi       to   w-let-arc-zcc-tmi      .
           move      rf-zcc-snx-bil       to   w-let-arc-zcc-snb      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-zcc-999.
       let-arc-zcc-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-zcc-flg      .
           move      all   "."            to   w-let-arc-zcc-des      .
           go to     let-arc-zcc-600.
       let-arc-zcc-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zcc-des      .
       let-arc-zcc-600.
           move      spaces               to   w-let-arc-zcc-tmi      .
           move      spaces               to   w-let-arc-zcc-snb      .
       let-arc-zcc-999.
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
