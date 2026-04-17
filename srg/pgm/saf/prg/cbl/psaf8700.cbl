       Identification Division.
       Program-Id.                                 psaf8700           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    saf                 *
      *                                Settore:    csc                 *
      *                                   Fase:    saf870              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 27/02/04    *
      *                       Ultima revisione:    NdK del 04/10/12    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Richieste per il programma psaf8701:        *
      *                                                                *
      *                    Stampa statistica di acquisto sul fatturato *
      *                    per forma di pagamento fornitore.           *
      *                                                                *
      *                    Emissione grafici web (chiamato da saf960)  *
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
                     "saf"                                            .
      *        *-------------------------------------------------------*
      *        * Settore gestionale                                    *
      *        *-------------------------------------------------------*
           05  i-ide-set                  pic  x(03) value
                     "csc"                                            .
      *        *-------------------------------------------------------*
      *        * Fase gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-fas                  pic  x(06) value
                     "saf870"                                         .
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10) value
                     "psaf8700"                                       .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  i-ide-des                  pic  x(40) value
                     "FATTURATO PER FORMA PAGAMENTO FORNITORI "       .

      *    *===========================================================*
      *    * Area per il programma di esecuzione                       *
      *    *-----------------------------------------------------------*
       01  i-exe.
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma di esecuzione             *
      *        *-------------------------------------------------------*
           05  i-exe-pro                  pic  x(10) value
                     "psaf8701  "                                     .
      *        *-------------------------------------------------------*
      *        * Pathname del programma di esecuzione                  *
      *        *-------------------------------------------------------*
           05  i-exe-pat                  pic  x(40) value
                     "pgm/saf/prg/obj/psaf8701                "       .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "mvideo" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/v"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "msegrt" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/s"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "mpslct" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/r"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per moduli                 "mbckgx" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/b"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "mmessg" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/m"                                  .

      *    *===========================================================*
      *    * Area per definizione codici di errore di i-o              *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/e"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per moduli di input-output          *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/f"                                  .

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
      *            *---------------------------------------------------*
      *            * Per routine sel-prm-stp-000                       *
      *            *---------------------------------------------------*
               10  w-cnt-sel-prm-stp      pic  x(01)                  .
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
      *        *-------------------------------------------------------*
      *        * Flags di controllo per tipo funzionamento             *
      *        *-------------------------------------------------------*
           05  w-cnt-fun.
      *            *---------------------------------------------------*
      *            * Si/No richieste per programma di esecuzione       *
      *            *---------------------------------------------------*
               10  w-cnt-fun-snx-ric      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Si/No richiesta di selezione stampa               *
      *            *---------------------------------------------------*
               10  w-cnt-fun-snx-stp      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Area per preparazione parametri selezione stampa      *
      *        *-------------------------------------------------------*
           05  w-cnt-stp.
               10  w-cnt-stp-tip-sel      pic  x(10)                  .
               10  w-cnt-stp-cod-stp      pic  x(08)                  .
               10  w-cnt-stp-tip-sta      pic  x(01)                  .
               10  w-cnt-stp-cod-mod      pic  x(08)                  .
               10  w-cnt-stp-tip-mod      pic  x(01)                  .
               10  w-cnt-stp-amp-lin      pic  9(03)                  .
               10  w-cnt-stp-top-lin      pic  9(04)                  .
               10  w-cnt-stp-lin-min      pic  9(02)                  .
               10  w-cnt-stp-bot-lin      pic  9(04)                  .
               10  w-cnt-stp-amp-car      pic  9(02)v9(02)            .
               10  w-cnt-stp-alt-int      pic  9(02)v9(02)            .
               10  w-cnt-stp-esp-fut      pic  x(99)                  .
               10  w-cnt-stp-fnz-spc      pic  x(99)                  .
      *        *-------------------------------------------------------*
      *        * Work per string-unstring record richieste             *
      *        *-------------------------------------------------------*
           05  w-stu-rrr.
               10  w-stu-rrr-pnt-stu      pic  9(05)                  .
               10  w-stu-rrr-255-byt.
                   15  filler occurs 255  pic  x(01)                  .
               10  w-stu-rrr-sav-pnt      pic  9(05)                  .

      *    *===========================================================*
      *    * Work per determinazione si/no aree gestionali attive      *
      *    *-----------------------------------------------------------*
       01  w-arg-snx.
      *        *-------------------------------------------------------*
      *        * Area [saf]                                            *
      *        *-------------------------------------------------------*
           05  w-arg-snx-saf.
      *            *---------------------------------------------------*
      *            * Si/No area [saf] attiva                           *
      *            * - S : Si'                                         *
      *            * - N : No                                          *
      *            *---------------------------------------------------*
               10  w-arg-snx-saf-snx      pic  x(01)                  .

      *    *===========================================================*
      *    * Work per personalizzazione 'pgm/saf/[snx-3pr]'            *
      *    *-----------------------------------------------------------*
       01  w-prs-saf-3pr.
      *        *-------------------------------------------------------*
      *        * Si/No trattamento terzo periodo di riferimento nelle  *
      *        * statistiche sul fatturato                             *
      *        *   - N : No, mai                                       *
      *        *   - S : Si, sempre                                    *
      *        *   - F : Si, ma solo in statistiche sul fatturato      *
      *        *   - P : Si, ma solo in statistiche sui prodotti       *
      *        *-------------------------------------------------------*
           05  w-prs-saf-3pr-snx          pic  x(01)                  .

      *    *===========================================================*
      *    * Work per personalizzazione 'pgm/saf[cod-dpz]'             *
      *    *-----------------------------------------------------------*
       01  w-prs-dpz-saf.
      *        *-------------------------------------------------------*
      *        * Definizione generale della personalizzazione          *
      *        *-------------------------------------------------------*
           05  w-prs-dpz-saf-prs.
      *            *---------------------------------------------------*
      *            * Provenienza dei codici dipendenza da trattare     *
      *            *  - R : Per normale richiesta all'operatore        *
      *            *  - + : Forzato dalla personalizzazione, includen- *
      *            *        do solamente i codici dipendenza di se-    *
      *            *        guito specificati                          *
      *            *  - - : Forzato dalla personalizzazione, escluden- *
      *            *        do solamente i codici dipendenza di se-    *
      *            *        guito specificati                          *
      *            *                                                   *
      *            * Note : Se questa voce e' posta al valore 'R', i   *
      *            *        codici delle dipendenze che seguono non    *
      *            *        sono significativi                         *
      *            *                                                   *
      *            *        Se questa voce e' posta al valore '+', op- *
      *            *        pure al valore '-', e tutti i codice del-  *
      *            *        le dipendenze che seguono sono a zero, si  *
      *            *        intendono specificare tutte le dipendenze  *
      *            *        esistenti per l'azienda in uso             *
      *            *---------------------------------------------------*
               10  w-prs-dpz-saf-pvz      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Tabella dei codici dipendenza, max 20 codici      *
      *            *---------------------------------------------------*
               10  w-prs-dpz-saf-tcc.
      *                *-----------------------------------------------*
      *                * Singolo elemento in tabella                   *
      *                *-----------------------------------------------*
                   15  w-prs-dpz-saf-ecc occurs 20.
      *                    *-------------------------------------------*
      *                    * Separatore                                *
      *                    *-------------------------------------------*
                       20  w-prs-dpz-saf-sep
                                          pic  x(01)                  .
      *                    *-------------------------------------------*
      *                    * Codice dipendenza                         *
      *                    *-------------------------------------------*
                       20  w-prs-dpz-saf-cod
                                          pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Contatori ed indici di comodo                         *
      *        *-------------------------------------------------------*
           05  w-prs-dpz-saf-c01          pic  9(02)                  .
           05  w-prs-dpz-saf-c02          pic  9(02)                  .

      *    *===========================================================*
      *    * Work-area per variabili di i.p.c. eventualmente passate   *
      *    * dal chiamante                                             *
      *    *-----------------------------------------------------------*
       01  w-ipc.
      *        *-------------------------------------------------------*
      *        * Per funzione di generazione grafici                   *
      *        *-------------------------------------------------------*
           05  w-ipc-gen-grf.
      *            *---------------------------------------------------*
      *            * Valore della variabile di i.p.c. relativa alla    *
      *            * richiesta di generazione set                      *
      *            *                                                   *
      *            * - S : Si, generazione grafico                     *
      *            * - N : No, nessuna azione                          *
      *            *---------------------------------------------------*
               10  w-ipc-gen-grf-snx      pic  x(01)                  .

      *    *===========================================================*
      *    * Records files                                             *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [yfp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcf/fls/rec/rfyfp"                          .

      *    *===========================================================*
      *    * Work-area richieste per stampa                            *
      *    *-----------------------------------------------------------*
       01  rr.
      *        *-------------------------------------------------------*
      *        * Codice forma di pagamento fornitore selezionata       *
      *        *  - Se codice 0000000 : Tutti i codici                 *
      *        *-------------------------------------------------------*
           05  rr-cod-fop                 pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Codice forma pagamento Fornitore selezionata, descri- *
      *        * zione                                                 *
      *        *  - Se codice 0000000 : Tutti i codici                 *
      *        *-------------------------------------------------------*
           05  rr-cod-fop-des             pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Tipi scadenza da includere                            *
      *        *                                                       *
      *        * - 01 : Tutti                                          *
      *        * - 02 : A scelta                                       *
      *        * - 03 : Rimesse dir. e Bonifici                        *
      *        * - 04 : Scadenze elettroniche                          *
      *        * - 05 : Ricevute banc. e Tratte                        *
      *        * - 06 : Solo Rimesse dirette                           *
      *        * - 07 : Solo Incassi elettronici                       *
      *        * - 08 : Solo Ri.Ba.                                    *
      *        * - 09 : Solo Conferme d'Ordine                         *
      *        * - 10 : Solo M.Av.                                     *
      *        * - 11 : Solo R.I.D.                                    *
      *        * - 12 : Solo Bonifici bancari                          *
      *        * - 13 : Solo C/C postali                               *
      *        * - 14 : Solo Ricevute bancarie                         *
      *        * - 15 : Solo Tratte                                    *
      *        * - 16 : Solo Paghero' cambiari                         *
      *        *-------------------------------------------------------*
           05  rr-tip-scd                 pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Si/No inclusione per tipo scadenza :                  *
      *        *                                                       *
      *        * - 01 : Rimesse dirette                                *
      *        * - 02 : Incassi elettronici                            *
      *        * - 03 : Ri.Ba.                                         *
      *        * - 04 : Conferme d'Ordine                              *
      *        * - 05 : M.Av.                                          *
      *        * - 06 : R.I.D.                                         *
      *        * - 07 : Bonifici bancari                               *
      *        * - 08 : C/C postali                                    *
      *        * - 09 : Ricevute bancarie                              *
      *        * - 10 : Tratte                                         *
      *        * - 11 : Paghero' cambiari                              *
      *        *-------------------------------------------------------*
           05  rr-sce-tsc.
               10  rr-snx-tsc occurs 11   pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Tipo di dettaglio di stampa                           *
      *        *                                                       *
      *        *  - 01 : Solo totale per codice forma pagamento        *
      *        *  - 02 : Con anche il totale per ogni fornitore        *
      *        *  - 03 : Con la lista documenti emessi per ogni forni- *
      *        *         tore                                          *
      *        *-------------------------------------------------------*
           05  rr-tip-dts                 pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Tipo di ordinamento per la forma pagamento fornitore  *
      *        *                                                       *
      *        *  - 01 : Ordinamento ABC decrescente                   *
      *        *  - 02 : Ordinamento ABC crescente                     *
      *        *  - 03 : Ordinamento per descrizione pagamento         *
      *        *  - 04 : Ordinamento per codice pagamento              *
      *        *  - 05 : Ordinamento per mnemonico pagamento           *
      *        *                                                       *
      *        * Non significativo se codice forma pagamento fornitore *
      *        * diversa da zero, ovvero un solo codice                *
      *        *-------------------------------------------------------*
           05  rr-tor-fop                 pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Tipo di ordinamento per fornitori                     *
      *        *                                                       *
      *        *  - 01 : Ordinamento ABC decrescente                   *
      *        *  - 02 : Ordinamento ABC crescente                     *
      *        *  - 03 : Ordinamento per ragione sociale fornitore     *
      *        *  - 04 : Ordinamento per codice fornitore              *
      *        *  - 05 : Ordinamento per mnemonico fornitore           *
      *        *                                                       *
      *        * Non significativo se tipo dettaglio stampa 01, ovvero *
      *        * solo totale per codice forma pagamento                *
      *        *-------------------------------------------------------*
           05  rr-tor-fnt                 pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Numero periodi di riferimento                         *
      *        *                                                       *
      *        *  - 01..03 : Numero effettivo di periodi di riferimen- *
      *        *             to                                        *
      *        *                                                       *
      *        *  - 11     : Due periodi di riferimento, con la stampa *
      *        *             del solo 2. periodo di cio' che non e'    *
      *        *             stato movimentato nel 1. periodo          *
      *        *                                                       *
      *        *  - 12     : Due periodi di riferimento, con la stampa *
      *        *             del solo 1. periodo di cio' che non e'    *
      *        *             stato movimentato nel 2. periodo          *
      *        *                                                       *
      *        * Forzato a 01 se tipo dettaglio stampa 03, ovvero con  *
      *        * la lista documenti emessi per ogni fornitore          *
      *        *-------------------------------------------------------*
           05  rr-num-pdr                 pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * 1. periodo di riferimento, data minima                *
      *        *-------------------------------------------------------*
           05  rr-p1d-min                 pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * 1. periodo di riferimento, data massima               *
      *        *-------------------------------------------------------*
           05  rr-p1d-max                 pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * 2. periodo di riferimento, data minima                *
      *        *                                                       *
      *        * Non significativo se il numero periodi di riferimento *
      *        * e' minore di 02, pero' significativo se pari a 11 op- *
      *        * pure 12                                               *
      *        *-------------------------------------------------------*
           05  rr-p2d-min                 pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * 2. periodo di riferimento, data massima               *
      *        *                                                       *
      *        * Non significativo se il numero periodi di riferimento *
      *        * e' minore di 02, pero' significativo se pari a 11 op- *
      *        * pure 12                                               *
      *        *-------------------------------------------------------*
           05  rr-p2d-max                 pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * 3. periodo di riferimento, data minima                *
      *        *                                                       *
      *        * Non significativo se il numero periodi di riferimento *
      *        * e' minore di 03, e nemmeno se pari a 11 oppure 12     *
      *        *-------------------------------------------------------*
           05  rr-p3d-min                 pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * 3. periodo di riferimento, data massima               *
      *        *                                                       *
      *        * Non significativo se il numero periodi di riferimento *
      *        * e' minore di 03, e nemmeno se pari a 11 oppure 12     *
      *        *-------------------------------------------------------*
           05  rr-p3d-max                 pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Si/no generazione grafico                             *
      *        *-------------------------------------------------------*
           05  rr-gen-grf                 pic  x(01)                  .

      *    *===========================================================*
      *    * Work per subroutines di Let                               *
      *    *-----------------------------------------------------------*
       01  w-let.
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [yfp]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-yfp.
               10  w-let-arc-yfp-flg      pic  x(01)                  .
               10  w-let-arc-yfp-cod      pic  9(07)                  .
               10  w-let-arc-yfp-des      pic  x(40)                  .

      *    *===========================================================*
      *    * Work-area per normalizzazioni richieste di selezione      *
      *    *-----------------------------------------------------------*
       01  w-nor-ric-sel.
      *        *-------------------------------------------------------*
      *        * Contatore                                             *
      *        *-------------------------------------------------------*
           05  w-nor-ric-sel-c01          pic  9(03)                  .

      *    *===========================================================*
      *    * Work per accettazioni campi espansi                       *
      *    *-----------------------------------------------------------*
       01  w-exp.
      *        *-------------------------------------------------------*
      *        * Work per : Tipi scadenza da includere                 *
      *        *-------------------------------------------------------*
           05  w-exp-tip-scd.
               10  w-exp-tip-scd-num      pic  9(02)       value 16   .
               10  w-exp-tip-scd-lun      pic  9(02)       value 25   .
               10  w-exp-tip-scd-tbl.
                   15  filler             pic  x(25) value
                            "Tutti                    "               .
                   15  filler             pic  x(25) value
                            "A scelta                 "               .
                   15  filler             pic  x(25) value
                            "RD+BB                    "               .
                   15  filler             pic  x(25) value
                            "IE+RIBA_CDO+MAV+RID      "               .
                   15  filler             pic  x(25) value
                            "RB+TR                    "               .
                   15  filler             pic  x(25) value
                            "Solo RD                  "               .
                   15  filler             pic  x(25) value
                            "Solo IE                  "               .
                   15  filler             pic  x(25) value
                            "Solo RIBA                "               .
                   15  filler             pic  x(25) value
                            "Solo CDO                 "               .
                   15  filler             pic  x(25) value
                            "Solo MAV                 "               .
                   15  filler             pic  x(25) value
                            "Solo RID                 "               .
                   15  filler             pic  x(25) value
                            "Solo BB                  "               .
                   15  filler             pic  x(25) value
                            "Solo CCP                 "               .
                   15  filler             pic  x(25) value
                            "Solo RB                  "               .
                   15  filler             pic  x(25) value
                            "Solo TR                  "               .
                   15  filler             pic  x(25) value
                            "Solo PC                  "               .
      *        *-------------------------------------------------------*
      *        * Work per : Si/No generico                             *
      *        *-------------------------------------------------------*
           05  w-exp-snx-gen.
               10  w-exp-snx-gen-num      pic  9(02)       value 02   .
               10  w-exp-snx-gen-lun      pic  9(02)       value 02   .
               10  w-exp-snx-gen-tbl.
                   15  filler             pic  x(02) value "Si"       .
                   15  filler             pic  x(02) value "No"       .
      *        *-------------------------------------------------------*
      *        * Work per : Tipo dettaglio stampa                      *
      *        *-------------------------------------------------------*
           05  w-exp-tip-dts.
               10  w-exp-tip-dts-num      pic  9(02)       value 3    .
               10  w-exp-tip-dts-lun      pic  9(02)       value 39   .
               10  w-exp-tip-dts-tbl.
                   15  filler             pic  x(39) value
                  "solo Totale per forma di pagamento     "           .
                   15  filler             pic  x(39) value
                  "con anche il totale per ogni Fornitore "           .
                   15  filler             pic  x(39) value
                  "con la Lista documenti per fornitore   "           .
      *        *-------------------------------------------------------*
      *        * Work per : Tipo ordinamento forma pagamento fornitori *
      *        *-------------------------------------------------------*
           05  w-exp-tor-csc.
               10  w-exp-tor-csc-num      pic  9(02)       value 5    .
               10  w-exp-tor-csc-lun      pic  9(02)       value 39   .
               10  w-exp-tor-csc-tbl.
                   15  filler             pic  x(39) value
                  "ABC decrescente                        "           .
                   15  filler             pic  x(39) value
                  "ABC crescente                          "           .
                   15  filler             pic  x(39) value
                  "per descrizione forma di pagamento     "           .
                   15  filler             pic  x(39) value
                  "per codice forma di pagamento          "           .
                   15  filler             pic  x(39) value
                  "per mnemonico forma di pagamento       "           .
      *        *-------------------------------------------------------*
      *        * Work per : Tipo ordinamento fornitori                 *
      *        *-------------------------------------------------------*
           05  w-exp-tor-fnt.
               10  w-exp-tor-fnt-num      pic  9(02)       value 5    .
               10  w-exp-tor-fnt-lun      pic  9(02)       value 39   .
               10  w-exp-tor-fnt-tbl.
                   15  filler             pic  x(39) value
                  "ABC decrescente                        "           .
                   15  filler             pic  x(39) value
                  "ABC crescente                          "           .
                   15  filler             pic  x(39) value
                  "per ragione sociale fornitore          "           .
                   15  filler             pic  x(39) value
                  "per codice fornitore                   "           .
                   15  filler             pic  x(39) value
                  "per mnemonico fornitore                "           .
      *        *-------------------------------------------------------*
      *        * Work per : Numero periodi di riferimento              *
      *        *-------------------------------------------------------*
           05  w-exp-num-pdr-t00.
               10  w-exp-num-pdr-num      pic  9(02)                  .
               10  w-exp-num-pdr-lun      pic  9(02)                  .
               10  w-exp-num-pdr-tbl.
                   15  filler             pic  x(39)                  .
                   15  filler             pic  x(39)                  .
                   15  filler             pic  x(39)                  .
                   15  filler             pic  x(39)                  .
                   15  filler             pic  x(39)                  .
               10  w-exp-num-pdr-tco.
                   15  w-exp-num-pdr-tcx
                               occurs 05  pic  9(02)                  .
           05  w-exp-num-pdr-t02.
               10  w-exp-num-pdr-nu2      pic  9(02)       value 4    .
               10  w-exp-num-pdr-lu2      pic  9(02)       value 39   .
               10  w-exp-num-pdr-tb2.
                   15  filler             pic  x(39) value
                  "Un solo periodo, dal .. al ..          "           .
                   15  filler             pic  x(39) value
                  "Due periodi confrontati tra loro       "           .
                   15  filler             pic  x(39) value
                  "Due periodi, solo se il 1. e' a zero   "           .
                   15  filler             pic  x(39) value
                  "Due periodi, solo se il 2. e' a zero   "           .
                   15  filler             pic  x(39) value
                  "                                                  ".
               10  w-exp-num-pdr-tc2.
                   15  filler             pic  9(02)       value 01   .
                   15  filler             pic  9(02)       value 02   .
                   15  filler             pic  9(02)       value 11   .
                   15  filler             pic  9(02)       value 12   .
                   15  filler             pic  9(02)       value 00   .
           05  w-exp-num-pdr-t03.
               10  w-exp-num-pdr-nu3      pic  9(02)       value 5    .
               10  w-exp-num-pdr-lu3      pic  9(02)       value 39   .
               10  w-exp-num-pdr-tb3.
                   15  filler             pic  x(39) value
                  "Un solo periodo, dal .. al ..          "           .
                   15  filler             pic  x(39) value
                  "Due periodi confrontati tra loro       "           .
                   15  filler             pic  x(39) value
                  "Tre periodi confrontati tra loro       "           .
                   15  filler             pic  x(39) value
                  "Due periodi, solo se il 1. e' a zero   "           .
                   15  filler             pic  x(39) value
                  "Due periodi, solo se il 2. e' a zero   "           .
               10  w-exp-num-pdr-tc3.
                   15  filler             pic  9(02)       value 01   .
                   15  filler             pic  9(02)       value 02   .
                   15  filler             pic  9(02)       value 03   .
                   15  filler             pic  9(02)       value 11   .
                   15  filler             pic  9(02)       value 12   .
           05  w-exp-num-pdr-war.
               10  w-exp-num-pdr-inx      pic  9(02)                  .
               10  w-exp-num-pdr-cod      pic  9(02)                  .

      *    *===========================================================*
      *    * Work per tabelle                                          *
      *    *-----------------------------------------------------------*
       01  w-tbl.
      *        *-------------------------------------------------------*
      *        * Tabella tipi pagamento                                *
      *        *-------------------------------------------------------*
           05  w-tbl-tpg.
               10  w-tbl-tpg-001.
                   15  filler             pic  x(07) value "RD....:"  .
                   15  filler             pic  x(07) value "IE....:"  .
                   15  filler             pic  x(07) value "RIBA..:"  .
                   15  filler             pic  x(07) value "CDO...:"  .
                   15  filler             pic  x(07) value "MAV...:"  .
                   15  filler             pic  x(07) value "RID...:"  .
                   15  filler             pic  x(07) value "BB....:"  .
                   15  filler             pic  x(07) value "CCP...:"  .
                   15  filler             pic  x(07) value "RB....:"  .
                   15  filler             pic  x(07) value "TR....:"  .
                   15  filler             pic  x(07) value "PC....:"  .
               10  w-tbl-tpg-002 redefines
                   w-tbl-tpg-001.
                   15  w-tbl-tpg-ele occurs 11
                                          pic  x(07)                  .

      *    *===========================================================*
      *    * Work per routines di Pmt                                  *
      *    *-----------------------------------------------------------*
       01  w-pmt.
      *        *-------------------------------------------------------*
      *        * Si/No inclusione tipi scadenza                        *
      *        *-------------------------------------------------------*
           05  w-pmt-snx-tsc.
               10  w-pmt-snx-tsc-ctr      pic  9(02)                  .

      *    *===========================================================*
      *    * Work per routines di Acc                                  *
      *    *-----------------------------------------------------------*
       01  w-acc.
      *        *-------------------------------------------------------*
      *        * Si/No inclusione tipi scadenza                        *
      *        *-------------------------------------------------------*
           05  w-acc-snx-tsc.
               10  w-acc-snx-tsc-ctr      pic  9(02)                  .

      *    *===========================================================*
      *    * Work-area per salvataggi                                  *
      *    *-----------------------------------------------------------*
       01  w-sav.
      *        *-------------------------------------------------------*
      *        * Codice forma di pagamento                             *
      *        *-------------------------------------------------------*
           05  w-sav-cod-fop              pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Tipo scadenza                                         *
      *        *-------------------------------------------------------*
           05  w-sav-tip-scd              pic  9(02)                  .

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

      *    *===========================================================*
      *    * Area di interfaccia per sottoprogramma         "pazi000d" *
      *    *-----------------------------------------------------------*
           copy      "pgm/azi/prg/cpy/pazi000d.pgl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione forma di pagamento             *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcf/prg/cpy/acmnyfp0.acl"                   .

      ******************************************************************
       Procedure Division.
      ******************************************************************

      *================================================================*
      *       Main                                                     *
      *================================================================*
       main-000.
      *              *-------------------------------------------------*
      *              * Dichiarazione di inizio programma               *
      *              *-------------------------------------------------*
           perform   dic-ini-pgm-000      thru dic-ini-pgm-999        .
           if        w-cnt-dic-ini-pgm    not  = spaces
                     go to main-999.
      *              *-------------------------------------------------*
      *              * Esecuzione routine pre-esecuzione programma     *
      *              *-------------------------------------------------*
           perform   pre-exe-pgm-000      thru pre-exe-pgm-999        .
           if        w-cnt-pre-exe-pgm    not  = spaces
                     go to main-900.
      *              *-------------------------------------------------*
      *              * Preparazione tipo funzionamento programma       *
      *              *-------------------------------------------------*
           perform   pre-tip-fun-000      thru pre-tip-fun-999        .
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
           perform   rou-opn-fls-000      thru rou-opn-fls-999        .
           if        w-cnt-rou-opn-fls    not  = spaces
                     go to main-750.
      *              *-------------------------------------------------*
      *              * Se no richieste : a selezione stampante         *
      *              *-------------------------------------------------*
           if        w-cnt-fun-snx-ric    not  = "S"
                     go to main-350.
       main-250.
      *              *-------------------------------------------------*
      *              * Accettazione richieste di selezione             *
      *              *-------------------------------------------------*
           perform   acc-ric-sel-000      thru acc-ric-sel-999        .
      *                  *---------------------------------------------*
      *                  * Se uscita per Exit                          *
      *                  *---------------------------------------------*
           if        w-cnt-acc-ric-sel    =    "E"
                     go to main-750.
      *              *-------------------------------------------------*
      *              * Regolarizzazione richieste di selezione         *
      *              *-------------------------------------------------*
           perform   reg-ric-sel-000      thru reg-ric-sel-999        .
       main-350.
      *              *-------------------------------------------------*
      *              * Se no stampa : ad esecuzione                    *
      *              *-------------------------------------------------*
           if        w-cnt-fun-snx-stp    not  = "S"
                     go to main-450.
      *              *-------------------------------------------------*
      *              * Preparazione defaults per parametri di selezio- *
      *              * ne stampa                                       *
      *              *-------------------------------------------------*
           perform   pre-prm-stp-000      thru pre-prm-stp-999        .
      *              *-------------------------------------------------*
      *              * Selezione parametri stampa                      *
      *              *-------------------------------------------------*
           perform   sel-prm-stp-000      thru sel-prm-stp-999        .
      *                  *---------------------------------------------*
      *                  * Test se uscita                              *
      *                  *---------------------------------------------*
           if        w-cnt-sel-prm-stp    not  = spaces
                     go to main-750.
       main-450.
      *                  *---------------------------------------------*
      *                  * Richiesta alla segreteria se funzionamento  *
      *                  * in background o foreground                  *
      *                  *---------------------------------------------*
           move      "BF"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Se esecuzione in foreground                 *
      *                  *---------------------------------------------*
           if        s-snb                =    "B"
                     go to main-500.
           perform   exe-pgm-frg-000      thru exe-pgm-frg-999        .
           go to     main-750.
       main-500.
      *                  *---------------------------------------------*
      *                  * Se esecuzione in background                 *
      *                  *---------------------------------------------*
           perform   exe-pgm-bkg-000      thru exe-pgm-bkg-999        .
       main-750.
      *              *-------------------------------------------------*
      *              * Close files                                     *
      *              *-------------------------------------------------*
           perform   rou-cls-fls-000      thru rou-cls-fls-999        .
       main-900.
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
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-dic-ini-pgm      .
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
           move      all   "="            to   v-alf                  .
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
           move      all   "="            to   v-alf                  .
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
           move      all   "="            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tit-pgm-999.
           exit.

      *    *===========================================================*
      *    * Programma di esecuzione in foreground                     *
      *    *-----------------------------------------------------------*
       exe-pgm-frg-000.
      *              *-------------------------------------------------*
      *              * Inizializzazione rullino messaggi di foreground *
      *              *-------------------------------------------------*
           move      "OF"                 to   m-ope                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
      *                  *---------------------------------------------*
      *                  * Se errore : uscita                          *
      *                  *---------------------------------------------*
            if       m-rsc                not  = spaces
                     go to exe-pgm-frg-999.
      *              *-------------------------------------------------*
      *              * Scrittura record richieste per foreground       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Inizio scrittura record richieste           *
      *                  *---------------------------------------------*
           move      "OO"                 to   b-ope                  .
           move      "F"                  to   b-tfe                  .
           call      "swd/mod/prg/obj/mbckgr"
                                         using b                      .
      *                      *-----------------------------------------*
      *                      * Se errore : uscita                      *
      *                      *-----------------------------------------*
            if       b-rsc                not  = spaces
                     go to exe-pgm-frg-999.
      *                  *---------------------------------------------*
      *                  * Estrazione segmenti da 255  bytes da record *
      *                  * richieste                                   *
      *                  *---------------------------------------------*
           move      1                    to   w-stu-rrr-pnt-stu      .
       exe-pgm-frg-200.
           move      spaces               to   w-stu-rrr-255-byt      .
           move      w-stu-rrr-pnt-stu    to   w-stu-rrr-sav-pnt      .
           unstring  rr                   into w-stu-rrr-255-byt
                                  with pointer w-stu-rrr-pnt-stu      .
           move      w-stu-rrr-255-byt    to   b-chr                  .
           if        w-stu-rrr-pnt-stu    =    w-stu-rrr-sav-pnt
                     go to exe-pgm-frg-400.
           move      "PT"                 to   b-ope                  .
           call      "swd/mod/prg/obj/mbckgr"
                                         using b                      .
           go to     exe-pgm-frg-200.
       exe-pgm-frg-400.
      *                  *---------------------------------------------*
      *                  * Fine scrittura record richieste             *
      *                  *---------------------------------------------*
           move      "CL"                 to   b-ope                  .
           call      "swd/mod/prg/obj/mbckgr"
                                         using b                      .
      *                      *-----------------------------------------*
      *                      * Se errore : uscita                      *
      *                      *-----------------------------------------*
            if       b-rsc                not  = spaces
                     go to exe-pgm-frg-999.
      *              *-------------------------------------------------*
      *              * Messaggio di programma in esecuzione            *
      *              *-------------------------------------------------*
           move      "PE"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Lancio del programma di esecuzione              *
      *              *-------------------------------------------------*
           call      i-exe-pat                                        .
      *              *-------------------------------------------------*
      *              * Cancel del programma di esecuzione              *
      *              *-------------------------------------------------*
           cancel    i-exe-pat                                        .
      *              *-------------------------------------------------*
      *              * Cancel del modulo "mbckgr"                      *
      *              *-------------------------------------------------*
           cancel    "swd/mod/prg/obj/mbckgr"                         .
      *              *-------------------------------------------------*
      *              * Visualizzazione eventuali errori di esecuzione  *
      *              *-------------------------------------------------*
           move      "VE"                 to   b-ope                  .
           move      "F"                  to   b-tfe                  .
           move      i-ide-des            to   b-chr                  .
           call      "swd/mod/prg/obj/mbckgv"
                                         using b                      .
           cancel    "swd/mod/prg/obj/mbckgv"                         .
       exe-pgm-frg-999.
           exit.

      *    *===========================================================*
      *    *  Selezione parametri stampa                               *
      *    *-----------------------------------------------------------*
       sel-prm-stp-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-sel-prm-stp      .
      *              *-------------------------------------------------*
      *              * Test se selezione stampa da eseguire            *
      *              *-------------------------------------------------*
           if        w-cnt-fun-snx-stp    not  = "S"
                     go to sel-prm-stp-999.
      *              *-------------------------------------------------*
      *              * Preparazione parametri per richiamo selezione   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Informazioni generali da segreteria         *
      *                  *---------------------------------------------*
           move      "IG"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                      *-----------------------------------------*
      *                      * Codice azienda                          *
      *                      *-----------------------------------------*
           move      s-azi                to   r-env-cod-azi          .
      *                      *-----------------------------------------*
      *                      * Codice terminale                        *
      *                      *-----------------------------------------*
           move      s-ter                to   r-env-cod-ter          .
      *                      *-----------------------------------------*
      *                      * Codice utente                           *
      *                      *-----------------------------------------*
           move      s-ute                to   r-env-cod-ute          .
      *                      *-----------------------------------------*
      *                      * Date and time da segreteria             *
      *                      *-----------------------------------------*
           move      "DT"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                      *-----------------------------------------*
      *                      * Date and time                           *
      *                      *-----------------------------------------*
           move      s-sdt                to   r-env-dat-tim          .
      *                  *---------------------------------------------*
      *                  * Informazioni da identificazione programma   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Sistema applicativo                     *
      *                      *-----------------------------------------*
           move      i-ide-sap            to   r-ide-sis-app          .
      *                      *-----------------------------------------*
      *                      * Area gestionale                         *
      *                      *-----------------------------------------*
           move      i-ide-arg            to   r-ide-are-ges          .
      *                      *-----------------------------------------*
      *                      * Settore gestionale                      *
      *                      *-----------------------------------------*
           move      i-ide-set            to   r-ide-set-ges          .
      *                      *-----------------------------------------*
      *                      * Fase gestionale                         *
      *                      *-----------------------------------------*
           move      i-ide-fas            to   r-ide-fas-ges          .
      *                  *---------------------------------------------*
      *                  * Informazioni da preparazione param. stampa  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Flags di tipo selezione                 *
      *                      *-----------------------------------------*
           move      w-cnt-stp-tip-sel    to   r-fix-tip-sel          .
      *                      *-----------------------------------------*
      *                      * Codice stampante                        *
      *                      *-----------------------------------------*
           move      w-cnt-stp-cod-stp    to   r-fix-cod-stp          .
      *                      *-----------------------------------------*
      *                      * Tipo di stampa                          *
      *                      *-----------------------------------------*
           move      w-cnt-stp-tip-sta    to   r-fix-tip-sta          .
      *                      *-----------------------------------------*
      *                      * Codice modulo                           *
      *                      *-----------------------------------------*
           move      w-cnt-stp-cod-mod    to   r-fix-cod-mod          .
      *                      *-----------------------------------------*
      *                      * Tipo modulo                             *
      *                      *-----------------------------------------*
           move      w-cnt-stp-tip-mod    to   r-fix-tip-mod          .
      *                      *-----------------------------------------*
      *                      * Ampiezza linea di stampa in caratteri   *
      *                      *-----------------------------------------*
           move      w-cnt-stp-amp-lin    to   r-fix-amp-lin          .
      *                      *-----------------------------------------*
      *                      * Top margin in linee                     *
      *                      *-----------------------------------------*
           move      w-cnt-stp-top-lin    to   r-fix-top-lin          .
      *                      *-----------------------------------------*
      *                      * Numero linee di stampa minimo           *
      *                      *-----------------------------------------*
           move      w-cnt-stp-lin-min    to   r-fix-lin-min          .
      *                      *-----------------------------------------*
      *                      * Bottom margin in linee                  *
      *                      *-----------------------------------------*
           move      w-cnt-stp-bot-lin    to   r-fix-bot-lin          .
      *                      *-----------------------------------------*
      *                      * Ampiezza caratteri                      *
      *                      *-----------------------------------------*
           move      w-cnt-stp-amp-car    to   r-fix-amp-car          .
      *                      *-----------------------------------------*
      *                      * Altezza interlinea                      *
      *                      *-----------------------------------------*
           move      w-cnt-stp-alt-int    to   r-fix-alt-int          .
      *                      *-----------------------------------------*
      *                      * Area riservata per espansioni future    *
      *                      *-----------------------------------------*
           move      w-cnt-stp-esp-fut    to   r-fix-esp-fut          .
      *                      *-----------------------------------------*
      *                      * Area riservata per funzioni speciali    *
      *                      *-----------------------------------------*
           move      w-cnt-stp-fnz-spc    to   r-fix-fnz-spc          .
      *              *-------------------------------------------------*
      *              * Richiamo modulo di selezione stampa             *
      *              *-------------------------------------------------*
           call      "swd/mod/prg/obj/mpslct"
                                         using r                      .
           cancel    "swd/mod/prg/obj/mpslct"                         .
      *              *-------------------------------------------------*
      *              * Status di uscita                                *
      *              *-------------------------------------------------*
           if        r-rsc                not  = spaces
                     move  "#"            to   w-cnt-sel-prm-stp      .
       sel-prm-stp-999.
           exit.

      *    *===========================================================*
      *    * Programma di esecuzione in background                     *
      *    *-----------------------------------------------------------*
       exe-pgm-bkg-000.
      *              *-------------------------------------------------*
      *              * Inizializzazione rullino messaggi di background *
      *              *-------------------------------------------------*
           move      "OB"                 to   m-ope                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
      *                  *---------------------------------------------*
      *                  * Se errore : uscita                          *
      *                  *---------------------------------------------*
            if       m-rsc                not  = spaces
                     go to exe-pgm-bkg-900.
      *              *-------------------------------------------------*
      *              * Scrittura record richieste per background       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Inizio scrittura record richieste           *
      *                  *---------------------------------------------*
           move      "OO"                 to   b-ope                  .
           move      "B"                  to   b-tfe                  .
           call      "swd/mod/prg/obj/mbckgr"
                                         using b                      .
      *                      *-----------------------------------------*
      *                      * Se errore : uscita                      *
      *                      *-----------------------------------------*
            if       b-rsc                not  = spaces
                     go to exe-pgm-bkg-900.
      *                  *---------------------------------------------*
      *                  * Estrazione segmenti da 255  bytes da record *
      *                  * richieste                                   *
      *                  *---------------------------------------------*
           move      1                    to   w-stu-rrr-pnt-stu      .
       exe-pgm-bkg-200.
           move      spaces               to   w-stu-rrr-255-byt      .
           move      w-stu-rrr-pnt-stu    to   w-stu-rrr-sav-pnt      .
           unstring  rr                   into w-stu-rrr-255-byt
                                  with pointer w-stu-rrr-pnt-stu      .
           move      w-stu-rrr-255-byt    to   b-chr                  .
           if        w-stu-rrr-pnt-stu    =    w-stu-rrr-sav-pnt
                     go to exe-pgm-bkg-400.
           move      "PT"                 to   b-ope                  .
           call      "swd/mod/prg/obj/mbckgr"
                                         using b                      .
           go to     exe-pgm-bkg-200.
       exe-pgm-bkg-400.
      *                  *---------------------------------------------*
      *                  * Fine scrittura record richieste             *
      *                  *---------------------------------------------*
           move      "CL"                 to   b-ope                  .
           call      "swd/mod/prg/obj/mbckgr"
                                         using b                      .
      *                      *-----------------------------------------*
      *                      * Se errore : uscita                      *
      *                      *-----------------------------------------*
            if       b-rsc                not  = spaces
                     go to exe-pgm-bkg-900.
      *              *-------------------------------------------------*
      *              * Lancio del programma di esecuzione  background  *
      *              * tramite chiamata al modulo di segreteria        *
      *              *-------------------------------------------------*
           move      "B+"                 to   s-ope                  .
           move      i-exe-pro            to   s-npb                  .
           move      i-exe-pat            to   s-pmo                  .
           move      i-ide-des            to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
       exe-pgm-bkg-900.
      *              *-------------------------------------------------*
      *              * Cancel del modulo "mbckgr"                      *
      *              *-------------------------------------------------*
           cancel    "swd/mod/prg/obj/mbckgr"                         .
      *              *-------------------------------------------------*
      *              * Cancel del modulo "mmessg"                      *
      *              *-------------------------------------------------*
           cancel    "swd/mod/prg/obj/mmessg"                         .
       exe-pgm-bkg-999.
           exit.

      *    *===========================================================*
      *    * Routine pre-esecuzione programma                          *
      *    *-----------------------------------------------------------*
       pre-exe-pgm-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-pre-exe-pgm      .
       pre-exe-pgm-050.
      *              *-------------------------------------------------*
      *              * Test se area [saf] attiva                       *
      *              *-------------------------------------------------*
           perform   arg-saf-snx-000      thru arg-saf-snx-999        .
           if        w-arg-snx-saf-snx    =    "S"
                     go to pre-exe-pgm-100.
           perform   arg-saf-err-000      thru arg-saf-err-999        .
           move      "#"                  to   w-cnt-pre-exe-pgm      .
           go to     pre-exe-pgm-999.
       pre-exe-pgm-100.
      *              *-------------------------------------------------*
      *              * Lettura della personalizzazione per il tratta-  *
      *              * mento del terzo periodo di riferimento nelle    *
      *              * statistiche sul fatturato                       *
      *              *-------------------------------------------------*
           perform   prs-snx-3pr-000      thru prs-snx-3pr-999        .
       pre-exe-pgm-200.
      *              *-------------------------------------------------*
      *              * Lettura della personalizzazione per il tratta-  *
      *              * mento del codice dipendenza nelle statistiche   *
      *              * sul fatturato                                   *
      *              *                                                 *
      *              * Nota : Questa routine deve essere richiamata    *
      *              *        prima della determinazione codici di-    *
      *              *        pendenza, e della scelta del codice      *
      *              *        dipendenza multiplo, in quanto intera-   *
      *              *        gisce con esse.                          *
      *              *-------------------------------------------------*
           perform   prs-dpz-saf-000      thru prs-dpz-saf-999        .
       pre-exe-pgm-250.
      *              *-------------------------------------------------*
      *              * Lettura della variabile eventuale di i.p.c. per *
      *              * generazione grafico                             *
      *              *-------------------------------------------------*
           perform   ipc-gen-grf-000      thru ipc-gen-grf-999        .
      *              *-------------------------------------------------*
      *              * Si/no generazione grafico                       *
      *              *-------------------------------------------------*
           move      w-ipc-gen-grf-snx    to   rr-gen-grf             .
       pre-exe-pgm-300.
      *              *-------------------------------------------------*
      *              * Determinazione codici dipendenze per l'azienda  *
      *              *-------------------------------------------------*
       pre-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Test se area [saf] attiva                                 *
      *    *-----------------------------------------------------------*
       arg-saf-snx-000.
      *              *-------------------------------------------------*
      *              * Lettura della personalizzazione                 *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/saf[snx]"       to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Se personalizzazione non esistente si pone il   *
      *              * valore di default : 'N'                         *
      *              *-------------------------------------------------*
           if        s-ves                not  = spaces
                     move  "N"            to   s-alf                  .
      *              *-------------------------------------------------*
      *              * Eventuale normalizzazione valore letto          *
      *              *-------------------------------------------------*
           if        s-alf                not  = "S"
                     move  "N"            to   s-alf                  .
      *              *-------------------------------------------------*
      *              * Memorizzazione del risultato                    *
      *              *-------------------------------------------------*
           move      s-alf                to   w-arg-snx-saf-snx      .
       arg-saf-snx-999.
           exit.

      *    *===========================================================*
      *    * Errore per area [saf] non attiva                          *
      *    *-----------------------------------------------------------*
       arg-saf-err-000.
      *              *-------------------------------------------------*
      *              * Video in Off                                    *
      *              *-------------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Visualizzazione titolo programma                *
      *              *-------------------------------------------------*
           perform   vis-tit-pgm-000      thru vis-tit-pgm-999        .
      *              *-------------------------------------------------*
      *              * Box                                             *
      *              *-------------------------------------------------*
           move      "BX"                 to   v-ope                  .
           move      07                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      17                   to   v-lto                  .
           move      80                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Messaggio nel box                               *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      76                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      "Attenzione : Le personalizzazioni non consentono l
      -              "'esecuzione del programma."
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      76                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      "             Non e' prevista la gestione delle sta
      -              "tistiche sul fatturato.   "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      76                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      "                                   Premere un tast
      -              "o per presa visione : [ ] "
                                          to   v-alf                  .
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
           move      15                   to   v-lin                  .
           move      76                   to   v-pos                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Cancellazione linea di accettazione             *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      76                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       arg-saf-err-999.
           exit.

      *    *===========================================================*
      *    * Lettura personalizzazione per il trattamento del terzo    *
      *    * periodo di riferimento nelle statistiche sul fatturato    *
      *    *-----------------------------------------------------------*
       prs-snx-3pr-000.
      *              *-------------------------------------------------*
      *              * Personalizzazione per la specifica fase gestio- *
      *              * nale                                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura della personalizzazione             *
      *                  *---------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      spaces               to   s-alf                  .
           string    "pgm/saf[snx-3pr]"
                                delimited by   size
                     i-ide-fas
                                delimited by   size
                                          into s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda dell'esito della let-  *
      *                  * tura                                        *
      *                  *---------------------------------------------*
           if        s-ves                =    spaces
                     go to prs-snx-3pr-150.
       prs-snx-3pr-100.
      *                  *---------------------------------------------*
      *                  * Se personalizzazione non esistente          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * A lettura personalizzazione valida per  *
      *                      * tutte le fasi gestionali                *
      *                      *-----------------------------------------*
           go to     prs-snx-3pr-500.
       prs-snx-3pr-150.
      *                  *---------------------------------------------*
      *                  * Se personalizzazione esistente              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Parametri letti in area di lavoro       *
      *                      *-----------------------------------------*
           move      s-alf                to   w-prs-saf-3pr          .
      *                      *-----------------------------------------*
      *                      * A regolarizzazione parametri            *
      *                      *-----------------------------------------*
           go to     prs-snx-3pr-700.
       prs-snx-3pr-500.
      *              *-------------------------------------------------*
      *              * Personalizzazione per tutte le fasi gestionali  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura della personalizzazione             *
      *                  *---------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/saf[snx-3pr]"   to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda dell'esito della let-  *
      *                  * tura                                        *
      *                  *---------------------------------------------*
           if        s-ves                =    spaces
                     go to prs-snx-3pr-650.
       prs-snx-3pr-600.
      *                  *---------------------------------------------*
      *                  * Se personalizzazione non esistente          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazione parametri in area di    *
      *                      * lavoro                                  *
      *                      *-----------------------------------------*
           move      spaces               to   w-prs-saf-3pr          .
      *                      *-----------------------------------------*
      *                      * A regolarizzazione parametri            *
      *                      *-----------------------------------------*
           go to     prs-snx-3pr-700.
       prs-snx-3pr-650.
      *                  *---------------------------------------------*
      *                  * Se personalizzazione esistente              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Parametri letti in area di lavoro       *
      *                      *-----------------------------------------*
           move      s-alf                to   w-prs-saf-3pr          .
      *                      *-----------------------------------------*
      *                      * A regolarizzazione parametri            *
      *                      *-----------------------------------------*
           go to     prs-snx-3pr-700.
       prs-snx-3pr-700.
      *              *-------------------------------------------------*
      *              * Regolarizzazione parametri                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Si/No trattamento 3. periodo            'N' *
      *                  *---------------------------------------------*
           if        w-prs-saf-3pr-snx    not  = "S" and
                     w-prs-saf-3pr-snx    not  = "F" and
                     w-prs-saf-3pr-snx    not  = "P"
                     move  "N"            to   w-prs-saf-3pr-snx      .
       prs-snx-3pr-800.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     prs-snx-3pr-999.
       prs-snx-3pr-999.
           exit.

      *    *===========================================================*
      *    * Lettura personalizzazione per il trattamento del codice   *
      *    * dipendenza nelle statistiche sul fatturato                *
      *    *-----------------------------------------------------------*
       prs-dpz-saf-000.
      *              *-------------------------------------------------*
      *              * Personalizzazione per la specifica fase gestio- *
      *              * nale                                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura della personalizzazione             *
      *                  *---------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      spaces               to   s-alf                  .
           string    "pgm/saf[cod-dpz]"
                                delimited by   size
                     i-ide-fas
                                delimited by   size
                                          into s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda dell'esito della let-  *
      *                  * tura                                        *
      *                  *---------------------------------------------*
           if        s-ves                =    spaces
                     go to prs-dpz-saf-150.
       prs-dpz-saf-100.
      *                  *---------------------------------------------*
      *                  * Se personalizzazione non esistente          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * A lettura personalizzazione valida per  *
      *                      * tutte le fasi gestionali                *
      *                      *-----------------------------------------*
           go to     prs-dpz-saf-500.
       prs-dpz-saf-150.
      *                  *---------------------------------------------*
      *                  * Se personalizzazione esistente              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Parametri letti in area di lavoro       *
      *                      *-----------------------------------------*
           move      s-alf                to   w-prs-dpz-saf-prs      .
      *                      *-----------------------------------------*
      *                      * A regolarizzazione parametri            *
      *                      *-----------------------------------------*
           go to     prs-dpz-saf-700.
       prs-dpz-saf-500.
      *              *-------------------------------------------------*
      *              * Personalizzazione per tutte le fasi gestionali  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura della personalizzazione             *
      *                  *---------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/saf[cod-dpz]"   to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda dell'esito della let-  *
      *                  * tura                                        *
      *                  *---------------------------------------------*
           if        s-ves                =    spaces
                     go to prs-dpz-saf-650.
       prs-dpz-saf-600.
      *                  *---------------------------------------------*
      *                  * Se personalizzazione non esistente          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazione parametri in area di    *
      *                      * lavoro                                  *
      *                      *-----------------------------------------*
           move      spaces               to   w-prs-dpz-saf-prs      .
      *                      *-----------------------------------------*
      *                      * A regolarizzazione parametri            *
      *                      *-----------------------------------------*
           go to     prs-dpz-saf-700.
       prs-dpz-saf-650.
      *                  *---------------------------------------------*
      *                  * Se personalizzazione esistente              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Parametri letti in area di lavoro       *
      *                      *-----------------------------------------*
           move      s-alf                to   w-prs-dpz-saf-prs      .
      *                      *-----------------------------------------*
      *                      * A regolarizzazione parametri            *
      *                      *-----------------------------------------*
           go to     prs-dpz-saf-700.
       prs-dpz-saf-700.
      *              *-------------------------------------------------*
      *              * Regolarizzazione parametri                      *
      *              *-------------------------------------------------*
       prs-dpz-saf-720.
      *                  *---------------------------------------------*
      *                  * Provenienza dei codici dipendenza da trat-  *
      *                  * tare                                        *
      *                  *---------------------------------------------*
           if        w-prs-dpz-saf-pvz    not  = "+" and
                     w-prs-dpz-saf-pvz    not  = "-"
                     move  "R"            to   w-prs-dpz-saf-pvz      .
       prs-dpz-saf-740.
      *                  *---------------------------------------------*
      *                  * Normalizzazione e compattamento codici di   *
      *                  * pendenza specificati                        *
      *                  *---------------------------------------------*
       prs-dpz-saf-742.
           move      zero                 to   w-prs-dpz-saf-c01      .
           move      zero                 to   w-prs-dpz-saf-c02      .
       prs-dpz-saf-744.
           add       1                    to   w-prs-dpz-saf-c01      .
           if        w-prs-dpz-saf-c01    >    20
                     go to prs-dpz-saf-746.
           move      spaces               to   w-prs-dpz-saf-sep
                                              (w-prs-dpz-saf-c01)     .
           if        w-prs-dpz-saf-cod
                    (w-prs-dpz-saf-c01)   not  numeric
                     move  zero           to   w-prs-dpz-saf-cod
                                              (w-prs-dpz-saf-c01)
                     go to prs-dpz-saf-744.
           if        w-prs-dpz-saf-pvz    =    "R"
                     move  zero           to   w-prs-dpz-saf-cod
                                              (w-prs-dpz-saf-c01)
                     go to prs-dpz-saf-744.
           add       1                    to   w-prs-dpz-saf-c02      .
           if        w-prs-dpz-saf-c02    =    w-prs-dpz-saf-c01
                     go to prs-dpz-saf-744.
           move      w-prs-dpz-saf-cod
                    (w-prs-dpz-saf-c01)   to   w-prs-dpz-saf-cod
                                              (w-prs-dpz-saf-c02)     .
           go to     prs-dpz-saf-744.
       prs-dpz-saf-746.
           add       1                    to   w-prs-dpz-saf-c02      .
           if        w-prs-dpz-saf-c02    >    20
                     go to prs-dpz-saf-760.
           move      zero                 to   w-prs-dpz-saf-cod
                                              (w-prs-dpz-saf-c02)     .
           go to     prs-dpz-saf-746.
       prs-dpz-saf-760.
      *                  *---------------------------------------------*
      *                  * Fine regolarizzazione parametri             *
      *                  * tare                                        *
      *                  *---------------------------------------------*
           go to     prs-dpz-saf-800.
       prs-dpz-saf-800.
      *              *-------------------------------------------------*
      *              * Preparazione richiamo determinazione codici di- *
      *              * pendenza per l'azienda e richiamo selezione     *
      *              * multipla codice dipendenza                      *
      *              *-------------------------------------------------*
       prs-dpz-saf-810.
      *                  *---------------------------------------------*
      *                  * Tipo provenienza codice dipendenza          *
      *                  *---------------------------------------------*
           move      "R0"                 to   w-dpz-tip-ope          .
           move      w-prs-dpz-saf-pvz    to   w-dpz-snx-zer          .
           call      "pgm/azi/prg/obj/pazi000d"
                                         using w-dpz                  .
       prs-dpz-saf-820.
      *                  *---------------------------------------------*
      *                  * Codici delle dipendenze                     *
      *                  *---------------------------------------------*
       prs-dpz-saf-822.
           move      zero                 to   w-prs-dpz-saf-c01      .
       prs-dpz-saf-824.
           add       1                    to   w-prs-dpz-saf-c01      .
           if        w-prs-dpz-saf-c01    >    20
                     go to prs-dpz-saf-830.
           if        w-prs-dpz-saf-cod
                    (w-prs-dpz-saf-c01)   =    zero
                     go to prs-dpz-saf-830.
           move      "R5"                 to   w-dpz-tip-ope          .
           move      w-prs-dpz-saf-cod
                    (w-prs-dpz-saf-c01)   to   w-dpz-cod-dpz          .
           call      "pgm/azi/prg/obj/pazi000d"
                                         using w-dpz                  .
           go to     prs-dpz-saf-824.
       prs-dpz-saf-830.
      *                  *---------------------------------------------*
      *                  * Fine preparazione                           *
      *                  *---------------------------------------------*
           go to     prs-dpz-saf-900.
       prs-dpz-saf-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     prs-dpz-saf-999.
       prs-dpz-saf-999.
           exit.

      *    *===========================================================*
      *    * Lettura della variabile eventuale di i.p.c. per genera-   *
      *    * zione grafico                                             *
      *    *-----------------------------------------------------------*
       ipc-gen-grf-000.
      *              *-------------------------------------------------*
      *              * Estrazione della variabile 'gen-grf' dal livel- *
      *              * lo precedente                                   *
      *              *-------------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "gen-grf"            to   s-var                  .
           move      "-"                  to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Deviazione a seconda dell'esito dell'operazione *
      *              *-------------------------------------------------*
           if        s-ves                =    spaces
                     go to ipc-gen-grf-200
           else      go to ipc-gen-grf-400.
       ipc-gen-grf-200.
      *              *-------------------------------------------------*
      *              * Se variabile esistente                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Valore della variabile                      *
      *                  *---------------------------------------------*
           move      s-alf                to   w-ipc-gen-grf-snx      .
      *                  *---------------------------------------------*
      *                  * Normalizzazione della variabile             *
      *                  *---------------------------------------------*
           if        w-ipc-gen-grf-snx    not  = "S"
                     move  "N"            to   w-ipc-gen-grf-snx      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     ipc-gen-grf-999.
       ipc-gen-grf-400.
      *              *-------------------------------------------------*
      *              * Se variabile non esistente                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Forzatura del valore                        *
      *                  *---------------------------------------------*
           move      "N"                  to   w-ipc-gen-grf-snx      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     ipc-gen-grf-999.
       ipc-gen-grf-999.
           exit.

      *    *===========================================================*
      *    * Preparazione tipo funzionamento programma                 *
      *    *-----------------------------------------------------------*
       pre-tip-fun-000.
      *              *-------------------------------------------------*
      *              * Si/No richieste ad utente                       *
      *              *-------------------------------------------------*
           move      "S"                  to   w-cnt-fun-snx-ric      .
      *              *-------------------------------------------------*
      *              * Si/No richiesta di selezione stampa             *
      *              *-------------------------------------------------*
           if        rr-gen-grf           =    "S"
                     move  "N"            to   w-cnt-fun-snx-stp
           else      move  "S"            to   w-cnt-fun-snx-stp      .
       pre-tip-fun-999.
           exit.

      *    *===========================================================*
      *    * Open files per richieste                                  *
      *    *-----------------------------------------------------------*
       rou-opn-fls-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-rou-opn-fls      .
      *              *-------------------------------------------------*
      *              * [yfp]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofyfp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-yfp                 .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione forma di pagamento     *
      *              *-------------------------------------------------*
           perform   cod-mne-yfp-opn-000  thru cod-mne-yfp-opn-999    .
       rou-opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Close files per richieste                                 *
      *    *-----------------------------------------------------------*
       rou-cls-fls-000.
      *              *-------------------------------------------------*
      *              * [yfp]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofyfp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-yfp                 .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione forma di pagamento    *
      *              *-------------------------------------------------*
           perform   cod-mne-yfp-cls-000  thru cod-mne-yfp-cls-999    .
       rou-cls-fls-999.
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
      *              * Normalizzazione parametri di selezione          *
      *              *-------------------------------------------------*
           perform   nor-ric-sel-000      thru nor-ric-sel-999        .
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
      *                  * Codice forma pagamento                      *
      *                  *---------------------------------------------*
           perform   acc-cod-fop-000      thru acc-cod-fop-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
       acc-ric-sel-200.
      *                  *---------------------------------------------*
      *                  * Tipi scadenza da includere                  *
      *                  *---------------------------------------------*
           perform   acc-tip-scd-000      thru acc-tip-scd-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-100.
       acc-ric-sel-210.
      *                  *---------------------------------------------*
      *                  * Si/No inclusione tipo scadenza              *
      *                  *---------------------------------------------*
           perform   acc-snx-tsc-000      thru acc-snx-tsc-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-200.
       acc-ric-sel-225.
      *                  *---------------------------------------------*
      *                  * Tipo di dettaglio di stampa                 *
      *                  *---------------------------------------------*
           perform   acc-tip-dts-000      thru acc-tip-dts-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-200.
       acc-ric-sel-250.
      *                  *---------------------------------------------*
      *                  * Tipo di ordinamento per la forma pagamento  *
      *                  * fornitore                                   *
      *                  *---------------------------------------------*
           perform   acc-tor-csc-000      thru acc-tor-csc-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-225.
       acc-ric-sel-300.
      *                  *---------------------------------------------*
      *                  * Tipo di ordinamento per fornitori           *
      *                  *---------------------------------------------*
           perform   acc-tor-fnt-000      thru acc-tor-fnt-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-250.
       acc-ric-sel-350.
      *                  *---------------------------------------------*
      *                  * Numero periodi di riferimento               *
      *                  *---------------------------------------------*
           perform   acc-num-pdr-000      thru acc-num-pdr-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-300.
       acc-ric-sel-400.
      *                  *---------------------------------------------*
      *                  * 1. periodo di riferimento, data minima      *
      *                  *---------------------------------------------*
           perform   acc-p1d-min-000      thru acc-p1d-min-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-350.
       acc-ric-sel-450.
      *                  *---------------------------------------------*
      *                  * 1. periodo di riferimento, data massima     *
      *                  *---------------------------------------------*
           perform   acc-p1d-max-000      thru acc-p1d-max-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-400.
       acc-ric-sel-500.
      *                  *---------------------------------------------*
      *                  * 2. periodo di riferimento, data minima      *
      *                  *---------------------------------------------*
           perform   acc-p2d-min-000      thru acc-p2d-min-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-450.
       acc-ric-sel-550.
      *                  *---------------------------------------------*
      *                  * 2. periodo di riferimento, data massima     *
      *                  *---------------------------------------------*
           perform   acc-p2d-max-000      thru acc-p2d-max-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-500.
       acc-ric-sel-600.
      *                  *---------------------------------------------*
      *                  * 3. periodo di riferimento, data minima      *
      *                  *---------------------------------------------*
           perform   acc-p3d-min-000      thru acc-p3d-min-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-550.
       acc-ric-sel-650.
      *                  *---------------------------------------------*
      *                  * 3. periodo di riferimento, data massima     *
      *                  *---------------------------------------------*
           perform   acc-p3d-max-000      thru acc-p3d-max-999        .
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
      *    * Visualizzazione prompts per richieste di selezione        *
      *    *-----------------------------------------------------------*
       pmt-ric-sel-000.
      *              *-------------------------------------------------*
      *              * Codice forma pagamento                          *
      *              *-------------------------------------------------*
           perform   pmt-cod-fop-000      thru pmt-cod-fop-999        .
      *              *-------------------------------------------------*
      *              * Tipi scadenza da includere                      *
      *              *-------------------------------------------------*
           perform   pmt-tip-scd-000      thru pmt-tip-scd-999        .
      *              *-------------------------------------------------*
      *              * Tipo di dettaglio di stampa                     *
      *              *-------------------------------------------------*
           perform   pmt-tip-dts-000      thru pmt-tip-dts-999        .
      *              *-------------------------------------------------*
      *              * Tipo di ordinamento per la forma pagamento      *
      *              * fornitore                                       *
      *              *-------------------------------------------------*
           perform   pmt-tor-csc-000      thru pmt-tor-csc-999        .
      *              *-------------------------------------------------*
      *              * Tipo di ordinamento per fornitori               *
      *              *-------------------------------------------------*
           perform   pmt-tor-fnt-000      thru pmt-tor-fnt-999        .
      *              *-------------------------------------------------*
      *              * Numero periodi di riferimento                   *
      *              *-------------------------------------------------*
           perform   pmt-num-pdr-000      thru pmt-num-pdr-999        .
      *              *-------------------------------------------------*
      *              * 1. periodo di riferimento, data minima          *
      *              *-------------------------------------------------*
           perform   pmt-p1d-min-000      thru pmt-p1d-min-999        .
      *              *-------------------------------------------------*
      *              * 1. periodo di riferimento, data massima         *
      *              *-------------------------------------------------*
           perform   pmt-p1d-max-000      thru pmt-p1d-max-999        .
      *              *-------------------------------------------------*
      *              * 2. periodo di riferimento, data minima          *
      *              *-------------------------------------------------*
           perform   pmt-p2d-min-000      thru pmt-p2d-min-999        .
      *              *-------------------------------------------------*
      *              * 2. periodo di riferimento, data massima         *
      *              *-------------------------------------------------*
           perform   pmt-p2d-max-000      thru pmt-p2d-max-999        .
      *              *-------------------------------------------------*
      *              * 3. periodo di riferimento, data minima          *
      *              *-------------------------------------------------*
           perform   pmt-p3d-min-000      thru pmt-p3d-min-999        .
      *              *-------------------------------------------------*
      *              * 3. periodo di riferimento, data massima         *
      *              *-------------------------------------------------*
           perform   pmt-p3d-max-000      thru pmt-p3d-max-999        .
       pmt-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Codice forma pagamento fornitore                 *
      *    *-----------------------------------------------------------*
       pmt-cod-fop-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice forma di pagamento  :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cod-fop-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per Tipi scadenza da includere    *
      *    *-----------------------------------------------------------*
       pmt-tip-scd-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tipi scadenza da includere :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-tip-scd-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per Si/No inclusione tipo scaden- *
      *    * za                                                        *
      *    *-----------------------------------------------------------*
       pmt-snx-tsc-000.
      *              *-------------------------------------------------*
      *              * Ciclo per i vari tipi scadenza                  *
      *              *-------------------------------------------------*
           move      zero                 to   w-pmt-snx-tsc-ctr      .
       pmt-snx-tsc-100.
           add       1                    to   w-pmt-snx-tsc-ctr      .
           if        w-pmt-snx-tsc-ctr    >    11
                     go to pmt-snx-tsc-999.
      *                  *---------------------------------------------*
      *                  * Visualizzazione prompt                      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      w-pmt-snx-tsc-ctr    to   v-lin                  .
           add       07                   to   v-lin                  .
           move      70                   to   v-pos                  .
           if        rr-tip-scd           =    02
                     move  w-tbl-tpg-ele
                          (w-pmt-snx-tsc-ctr)
                                          to   v-alf
           else      move  spaces         to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     pmt-snx-tsc-100.
       pmt-snx-tsc-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Tipo di dettaglio di stampa                      *
      *    *-----------------------------------------------------------*
       pmt-tip-dts-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Dettaglio di stampa        :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-tip-dts-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Tipo di ordinamento per la forma pagamento       *
      *    *-----------------------------------------------------------*
       pmt-tor-csc-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Ordinamento forme pagamento:"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-tor-csc-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Tipo di ordinamento per i fornitori              *
      *    *-----------------------------------------------------------*
       pmt-tor-fnt-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Ordinamento fornitori      :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-tor-fnt-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Numero periodi di riferimento                    *
      *    *-----------------------------------------------------------*
       pmt-num-pdr-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Periodi di riferimento     :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-num-pdr-999.
           exit.

      *    *===========================================================*
      *    * Prompt : 1. periodo di riferimento, data minima           *
      *    *-----------------------------------------------------------*
       pmt-p1d-min-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "1. Periodo ........... Dal :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-p1d-min-999.
           exit.

      *    *===========================================================*
      *    * Prompt : 1. periodo di riferimento, data massima          *
      *    *-----------------------------------------------------------*
       pmt-p1d-max-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      40                   to   v-pos                  .
           move      "al :"               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-p1d-max-999.
           exit.

      *    *===========================================================*
      *    * Prompt : 2. periodo di riferimento, data minima           *
      *    *-----------------------------------------------------------*
       pmt-p2d-min-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "2. Periodo ........... Dal :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-p2d-min-999.
           exit.

      *    *===========================================================*
      *    * Prompt : 2. periodo di riferimento, data massima          *
      *    *-----------------------------------------------------------*
       pmt-p2d-max-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      40                   to   v-pos                  .
           move      "al :"               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-p2d-max-999.
           exit.

      *    *===========================================================*
      *    * Prompt : 3. periodo di riferimento, data minima           *
      *    *-----------------------------------------------------------*
       pmt-p3d-min-000.
      *              *-------------------------------------------------*
      *              * Se le personalizzazioni non consentono il terzo *
      *              * periodo di riferimento : uscita                 *
      *              *-------------------------------------------------*
           if        w-prs-saf-3pr-snx    not  = "S" and
                     w-prs-saf-3pr-snx    not  = "F"
                     go to pmt-p3d-min-999.
      *              *-------------------------------------------------*
      *              * Visualizzazione prompt                          *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "3. Periodo ........... Dal :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-p3d-min-999.
           exit.

      *    *===========================================================*
      *    * Prompt : 3. periodo di riferimento, data massima          *
      *    *-----------------------------------------------------------*
       pmt-p3d-max-000.
      *              *-------------------------------------------------*
      *              * Se le personalizzazioni non consentono il terzo *
      *              * periodo di riferimento : uscita                 *
      *              *-------------------------------------------------*
           if        w-prs-saf-3pr-snx    not  = "S" and
                     w-prs-saf-3pr-snx    not  = "F"
                     go to pmt-p3d-max-999.
      *              *-------------------------------------------------*
      *              * Visualizzazione prompt                          *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      40                   to   v-pos                  .
           move      "al :"               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-p3d-max-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Forma di pagamento                         *
      *    *-----------------------------------------------------------*
       acc-cod-fop-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      rr-cod-fop           to   w-sav-cod-fop          .
      *                  *---------------------------------------------*
      *                  * Eventuale forzatura                         *
      *                  *---------------------------------------------*
           if        rr-gen-grf           not  = "S"
                     go to acc-cod-fop-100.
      *                  *---------------------------------------------*
      *                  * Codice a zero                               *
      *                  *---------------------------------------------*
           move      zero                 to   rr-cod-fop             .
           perform   vis-cod-fop-000      thru vis-cod-fop-999        .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     acc-cod-fop-999.
       acc-cod-fop-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-yfp-ope      .
           move      rr-cod-fop           to   w-cod-mne-yfp-cod      .
           move      05                   to   w-cod-mne-yfp-lin      .
           move      30                   to   w-cod-mne-yfp-pos      .
           move      05                   to   w-cod-mne-yfp-dln      .
           move      41                   to   w-cod-mne-yfp-dps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "PRSC"               to   v-pfk (07)             .
           move      "NXSC"               to   v-pfk (08)             .
           perform   cod-mne-yfp-cll-000  thru cod-mne-yfp-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-mne-yfp-foi-000  thru cod-mne-yfp-foi-999    .
       acc-cod-fop-110.
           perform   cod-mne-yfp-cll-000  thru cod-mne-yfp-cll-999    .
           if        w-cod-mne-yfp-ope    =    "F+"
                     go to acc-cod-fop-115.
           if        w-cod-mne-yfp-ope    =    "AC"
                     go to acc-cod-fop-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-fop-115.
           perform   cod-mne-yfp-foi-000  thru cod-mne-yfp-foi-999    .
           go to     acc-cod-fop-110.
       acc-cod-fop-120.
           move      w-cod-mne-yfp-cod    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-cod-fop-999.
       acc-cod-fop-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-cod-fop             .
       acc-cod-fop-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura tabella                             *
      *                  *---------------------------------------------*
           move      rr-cod-fop           to   w-let-arc-yfp-cod      .
           perform   let-arc-yfp-000      thru let-arc-yfp-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione descrizione                  *
      *                  *---------------------------------------------*
           if        rr-cod-fop           =    zero
                     move  "Tutte le forme di pagamento       "
                                          to   rr-cod-fop-des
           else      move  w-let-arc-yfp-des
                                          to   rr-cod-fop-des         .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione                 *
      *                  *---------------------------------------------*
           perform   vis-des-fop-000      thru vis-des-fop-999        .
      *                  *---------------------------------------------*
      *                  * Se codice errato : reimpostazione           *
      *                  *---------------------------------------------*
           if        w-let-arc-yfp-flg    not  = spaces
                     go to acc-cod-fop-100.
       acc-cod-fop-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore attuale come precedente : oltre   *
      *                  *---------------------------------------------*
           if        rr-cod-fop           =    w-sav-cod-fop
                     go to acc-cod-fop-800.
      *                  *---------------------------------------------*
      *                  * Test se necessario normalizzare tipi di     *
      *                  * scadenza                                    *
      *                  *---------------------------------------------*
           if        rr-cod-fop           =    zero
                     go to acc-cod-fop-800.
           if        rr-tip-scd           =    zero or
                     rr-tip-scd           =    01
                     go to acc-cod-fop-800.
      *                  *---------------------------------------------*
      *                  * Normalizzazione tipi di scadenza            *
      *                  *---------------------------------------------*
           move      01                   to   rr-tip-scd             .
      *                  *---------------------------------------------*
      *                  * Visualizzazione tipi di scadenza            *
      *                  *---------------------------------------------*
           perform   vis-tip-scd-000      thru vis-tip-scd-999        .
      *                  *---------------------------------------------*
      *                  * Normalizzazione selettori tipi di scadenza  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Video in Off                            *
      *                      *-----------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Visualizzazione prompt per scelta tipi  *
      *                      * scadenza da includere                   *
      *                      *-----------------------------------------*
           perform   pmt-snx-tsc-000      thru pmt-snx-tsc-999        .
      *                      *-----------------------------------------*
      *                      * Normalizzazione valori                  *
      *                      *-----------------------------------------*
           move      spaces               to   rr-sce-tsc             .
      *                      *-----------------------------------------*
      *                      * Visualizzazione valori                  *
      *                      *-----------------------------------------*
           perform   vis-snx-tsc-000      thru vis-snx-tsc-999        .
      *                      *-----------------------------------------*
      *                      * Video in On                             *
      *                      *-----------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       acc-cod-fop-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-cod-fop-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-cod-fop-100.
       acc-cod-fop-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Forma di pagamento        *
      *    *-----------------------------------------------------------*
       vis-cod-fop-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      05                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-cod-fop           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-fop-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Descrizione pagamento     *
      *    *-----------------------------------------------------------*
       vis-des-fop-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      rr-cod-fop-des       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-des-fop-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo selezione : Tipi scadenza da includere *
      *    *-----------------------------------------------------------*
       acc-tip-scd-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se valore da accettare                 *
      *                  *---------------------------------------------*
           if        rr-cod-fop           not  = zero
                     go to acc-tip-scd-999.
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      rr-tip-scd           to   w-sav-tip-scd          .
       acc-tip-scd-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-scd-lun    to   v-car                  .
           move      w-exp-tip-scd-num    to   v-ldt                  .
           move      spaces               to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      w-exp-tip-scd-tbl    to   v-txt                  .
           move      07                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-ric    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      rr-tip-scd           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-tip-scd-999.
       acc-tip-scd-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-tip-scd             .
       acc-tip-scd-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore a zero : reimpostazione, a meno   *
      *                  * che non si sia in Up                        *
      *                  *---------------------------------------------*
           if        rr-tip-scd           not  = zero
                     go to acc-tip-scd-600.
           if        v-key                =    "UP  "
                     go to acc-tip-scd-600
           else      go to acc-tip-scd-100.
       acc-tip-scd-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore attuale come precedente : oltre   *
      *                  *---------------------------------------------*
           if        rr-tip-scd           =    w-sav-tip-scd
                     go to acc-tip-scd-800.
      *                  *---------------------------------------------*
      *                  * Se valore richiesto : A scelta              *
      *                  *---------------------------------------------*
           if        rr-tip-scd           not  = 02
                     go to acc-tip-scd-620.
      *                      *-----------------------------------------*
      *                      * Visualizzazione prompt per scelta tipi  *
      *                      * scadenza da includere                   *
      *                      *-----------------------------------------*
           perform   pmt-snx-tsc-000      thru pmt-snx-tsc-999        .
      *                      *-----------------------------------------*
      *                      * Oltre                                   *
      *                      *-----------------------------------------*
           go to     acc-tip-scd-800.
       acc-tip-scd-620.
      *                  *---------------------------------------------*
      *                  * Se valore richiesto : altri che non siano a *
      *                  * scelta                                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se il valore precedente non era 'A scel-*
      *                      * ta' : oltre                             *
      *                      *-----------------------------------------*
           if        w-sav-tip-scd        not  = 02
                     go to acc-tip-scd-800.
      *                      *-----------------------------------------*
      *                      * Se il valore precedente era 'A scelta'  *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Video in Off                        *
      *                          *-------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                          *-------------------------------------*
      *                          * Visualizzazione prompt per scelta   *
      *                          * tipi scadenza da includere          *
      *                          *-------------------------------------*
           perform   pmt-snx-tsc-000      thru pmt-snx-tsc-999        .
      *                          *-------------------------------------*
      *                          * Normalizzazione valori per Si/No    *
      *                          * tipi scadenza da includere          *
      *                          *-------------------------------------*
           move      spaces               to   rr-sce-tsc             .
      *                          *-------------------------------------*
      *                          * Visualizzazione valori per Si/No    *
      *                          * tipi scadenza da includere          *
      *                          *-------------------------------------*
           perform   vis-snx-tsc-000      thru vis-snx-tsc-999        .
      *                          *-------------------------------------*
      *                          * Video in On                         *
      *                          *-------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       acc-tip-scd-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-tip-scd-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-tip-scd-100.
       acc-tip-scd-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Tipo di dettaglio in stampa             *
      *    *-----------------------------------------------------------*
       vis-tip-scd-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-scd-lun    to   v-car                  .
           move      w-exp-tip-scd-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      07                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-tip-scd-tbl    to   v-txt                  .
           move      rr-tip-scd           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tip-scd-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo selezione : Si/No tipo scadenza da in- *
      *    * cludere                                                   *
      *    *-----------------------------------------------------------*
       acc-snx-tsc-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se valore da accettare                 *
      *                  *---------------------------------------------*
           if        rr-tip-scd           not  = 02
                     go to acc-snx-tsc-999.
      *                  *---------------------------------------------*
      *                  * Inizializzazione contatore 01..11           *
      *                  *---------------------------------------------*
           move      01                   to   w-acc-snx-tsc-ctr      .
       acc-snx-tsc-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-snx-gen-lun    to   v-car                  .
           move      w-exp-snx-gen-num    to   v-ldt                  .
           move      "SN#"                to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      w-exp-snx-gen-tbl    to   v-txt                  .
           move      w-acc-snx-tsc-ctr    to   v-lin                  .
           add       07                   to   v-lin                  .
           move      78                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-ric    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           if        rr-snx-tsc
                    (w-acc-snx-tsc-ctr)   =    "S"
                     move  01             to   v-num
           else if   rr-snx-tsc
                    (w-acc-snx-tsc-ctr)   =    "N"
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-snx-tsc-999.
       acc-snx-tsc-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        v-num                =    01
                     move  "S"            to   rr-snx-tsc
                                              (w-acc-snx-tsc-ctr)
           else if   v-num                =    02
                     move  "N"            to   rr-snx-tsc
                                              (w-acc-snx-tsc-ctr)
           else      move  spaces         to   rr-snx-tsc
                                              (w-acc-snx-tsc-ctr)     .
       acc-snx-tsc-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-snx-tsc-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-snx-tsc-620.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione dell'impostazione        *
      *              *-------------------------------------------------*
           if        v-key                =    "UP  "
                     go to acc-snx-tsc-650
           else if   v-key                =    "DOWN"
                     go to acc-snx-tsc-700
           else if   v-key                =    "DO  "
                     go to acc-snx-tsc-800
           else      go to acc-snx-tsc-710.
       acc-snx-tsc-650.
      *              *-------------------------------------------------*
      *              * Se Up                                           *
      *              *-------------------------------------------------*
           if        w-acc-snx-tsc-ctr    =    1
                     go to acc-snx-tsc-999.
           subtract  1                    from w-acc-snx-tsc-ctr      .
           go to     acc-snx-tsc-100.
       acc-snx-tsc-700.
      *              *-------------------------------------------------*
      *              * Se Down                                         *
      *              *-------------------------------------------------*
           go to     acc-snx-tsc-999.
       acc-snx-tsc-710.
      *              *-------------------------------------------------*
      *              * Se Return                                       *
      *              *-------------------------------------------------*
           if        w-acc-snx-tsc-ctr        =    11
                     go to acc-snx-tsc-999.
      *                  *---------------------------------------------*
      *                  * Incremento contatore                        *
      *                  *---------------------------------------------*
           add       1                    to   w-acc-snx-tsc-ctr      .
           go to     acc-snx-tsc-100.
       acc-snx-tsc-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-snx-tsc-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-snx-tsc-100.
       acc-snx-tsc-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo selezione : Si/No tipo scadenza da  *
      *    * includere                                                 *
      *    *-----------------------------------------------------------*
       vis-snx-tsc-000.
      *              *-------------------------------------------------*
      *              * Ciclo                                           *
      *              *-------------------------------------------------*
           move      zero                 to   w-acc-snx-tsc-ctr      .
       vis-snx-tsc-200.
           add       1                    to   w-acc-snx-tsc-ctr      .
           if        w-acc-snx-tsc-ctr    >    11
                     go to vis-snx-tsc-999.
      *                  *---------------------------------------------*
      *                  * Visualizzazione                             *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      w-exp-snx-gen-lun    to   v-car                  .
           move      w-exp-snx-gen-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-exp-snx-gen-tbl    to   v-txt                  .
           move      w-acc-snx-tsc-ctr    to   v-lin                  .
           add       07                   to   v-lin                  .
           move      78                   to   v-pos                  .
           if        rr-snx-tsc
                    (w-acc-snx-tsc-ctr)   =    "S"
                     move  01             to   v-num
           else if   rr-snx-tsc
                    (w-acc-snx-tsc-ctr)   =    "N"
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     vis-snx-tsc-200.
       vis-snx-tsc-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Tipo di dettaglio in stampa                *
      *    *-----------------------------------------------------------*
       acc-tip-dts-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Eventuale forzatura                         *
      *                  *---------------------------------------------*
           if        rr-gen-grf           not  = "S"
                     go to acc-tip-dts-100.
      *                  *---------------------------------------------*
      *                  * Solo totale                                 *
      *                  *---------------------------------------------*
           move      01                   to   rr-tip-dts             .
           perform   vis-tip-dts-000      thru vis-tip-dts-999        .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     acc-tip-dts-999.
       acc-tip-dts-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-dts-lun    to   v-car                  .
           move      w-exp-tip-dts-num    to   v-ldt                  .
           move      "TCL#"               to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      09                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-tip-dts-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-tip-dts           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-tip-dts-999.
       acc-tip-dts-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-tip-dts             .
       acc-tip-dts-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-tip-dts-425.
      *                  *---------------------------------------------*
      *                  * Che sia un valore consentito                *
      *                  *---------------------------------------------*
           if        rr-tip-dts           =    zero           or
                     rr-tip-dts           >    w-exp-tip-dts-num
                     go to acc-tip-dts-100.
       acc-tip-dts-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-tip-dts-625.
      *                  *---------------------------------------------*
      *                  * Se la scelta implica il solo totale per     *
      *                  * forma pagamento  si normalizza e si vi-     *
      *                  * sualizza :                                  *
      *                  *  - Tipo di ordinamento per fornitori        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test                                    *
      *                      *-----------------------------------------*
           if        rr-tip-dts           not  = 01
                     go to acc-tip-dts-650.
      *                      *-----------------------------------------*
      *                      * Normalizzazione tipo ordinamento per    *
      *                      * fornitori                               *
      *                      *-----------------------------------------*
           move      zero                 to   rr-tor-fnt             .
           perform   vis-tor-fnt-000      thru vis-tor-fnt-999        .
       acc-tip-dts-650.
      *                  *---------------------------------------------*
      *                  * Se la scelta implica il dettaglio dei do-   *
      *                  * cumenti si normalizzano e si visualizzano : *
      *                  *  - Numero periodi di riferimento            *
      *                  *  - 2. periodo, data minima                  *
      *                  *  - 2. periodo, data massima                 *
      *                  *  - 3. periodo, data minima                  *
      *                  *  - 3. periodo, data massima                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test                                    *
      *                      *-----------------------------------------*
           if        rr-tip-dts           not  = 03
                     go to acc-tip-dts-675.
      *                      *-----------------------------------------*
      *                      * Normalizzazione numero periodi          *
      *                      *-----------------------------------------*
           move      01                   to   rr-num-pdr             .
           perform   vis-num-pdr-000      thru vis-num-pdr-999        .
      *                      *-----------------------------------------*
      *                      * Normalizzazione 2. periodo, data min    *
      *                      *-----------------------------------------*
           move      zero                 to   rr-p2d-min             .
           perform   vis-p2d-min-000      thru vis-p2d-min-999        .
      *                      *-----------------------------------------*
      *                      * Normalizzazione 2. periodo, data max    *
      *                      *-----------------------------------------*
           move      zero                 to   rr-p2d-max             .
           perform   vis-p2d-max-000      thru vis-p2d-max-999        .
      *                      *-----------------------------------------*
      *                      * Normalizzazione 3. periodo, data min    *
      *                      *-----------------------------------------*
           move      zero                 to   rr-p3d-min             .
           perform   vis-p3d-min-000      thru vis-p3d-min-999        .
      *                      *-----------------------------------------*
      *                      * Normalizzazione 3. periodo, data max    *
      *                      *-----------------------------------------*
           move      zero                 to   rr-p3d-max             .
           perform   vis-p3d-max-000      thru vis-p3d-max-999        .
       acc-tip-dts-675.
      *                  *---------------------------------------------*
      *                  * Fine dipendenze                             *
      *                  *---------------------------------------------*
           go to     acc-tip-dts-800.
       acc-tip-dts-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-tip-dts-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-tip-dts-100.
       acc-tip-dts-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Tipo di dettaglio in stampa             *
      *    *-----------------------------------------------------------*
       vis-tip-dts-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-dts-lun    to   v-car                  .
           move      w-exp-tip-dts-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      09                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-tip-dts-tbl    to   v-txt                  .
           move      rr-tip-dts           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tip-dts-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Tipo ordinamento per codice pagamento      *
      *    *-----------------------------------------------------------*
       acc-tor-csc-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Eventuale forzatura                         *
      *                  *---------------------------------------------*
           if        rr-gen-grf           not  = "S"
                     go to acc-tor-csc-025.
      *                  *---------------------------------------------*
      *                  * Ordinamento ABC decrescente                 *
      *                  *---------------------------------------------*
           move      01                   to   rr-tor-fop             .
           perform   vis-tor-csc-000      thru vis-tor-csc-999        .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     acc-tor-csc-999.
       acc-tor-csc-025.
      *                  *---------------------------------------------*
      *                  * Se impostato un codice pagamento fornitori  *
      *                  * diverso da zero : no accettazione           *
      *                  *---------------------------------------------*
           if        rr-cod-fop           =    zero
                     go to acc-tor-csc-100
           else      go to acc-tor-csc-999.
       acc-tor-csc-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tor-csc-lun    to   v-car                  .
           move      w-exp-tor-csc-num    to   v-ldt                  .
           move      spaces               to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      11                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-tor-csc-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-tor-fop           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-tor-csc-999.
       acc-tor-csc-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-tor-fop             .
       acc-tor-csc-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-tor-csc-425.
      *                  *---------------------------------------------*
      *                  * Che sia un valore consentito                *
      *                  *---------------------------------------------*
           if        rr-tor-fop           =    zero           or
                     rr-tor-fop           >    w-exp-tor-csc-num
                     go to acc-tor-csc-100.
       acc-tor-csc-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-tor-csc-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-tor-csc-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-tor-csc-100.
       acc-tor-csc-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Tipo ordinamento per codice pagamento   *
      *    *-----------------------------------------------------------*
       vis-tor-csc-000.
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tor-csc-lun    to   v-car                  .
           move      w-exp-tor-csc-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      11                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-tor-csc-tbl    to   v-txt                  .
           move      rr-tor-fop           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tor-csc-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Tipo ordinamento per fornitori             *
      *    *-----------------------------------------------------------*
       acc-tor-fnt-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-tor-fnt-025.
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se il tipo dettaglio stampa e' 01, ov-  *
      *                      * vero solo totale per forma pagamento ,  *
      *                      * non si accetta il campo                 *
      *                      *-----------------------------------------*
           if        rr-tip-dts           =    01
                     go to acc-tor-fnt-999.
       acc-tor-fnt-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tor-fnt-lun    to   v-car                  .
           move      w-exp-tor-fnt-num    to   v-ldt                  .
           move      spaces               to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      13                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-tor-fnt-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-tor-fnt           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-tor-fnt-999.
       acc-tor-fnt-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-tor-fnt             .
       acc-tor-fnt-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-tor-fnt-425.
      *                  *---------------------------------------------*
      *                  * Che sia un valore consentito                *
      *                  *---------------------------------------------*
           if        rr-tor-fnt           =    zero           or
                     rr-tor-fnt           >    w-exp-tor-fnt-num
                     go to acc-tor-fnt-100.
       acc-tor-fnt-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-tor-fnt-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-tor-fnt-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-tor-fnt-100.
       acc-tor-fnt-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Tipo ordinamento per fornitori          *
      *    *-----------------------------------------------------------*
       vis-tor-fnt-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tor-fnt-lun    to   v-car                  .
           move      w-exp-tor-fnt-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      13                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-tor-fnt-tbl    to   v-txt                  .
           move      rr-tor-fnt           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tor-fnt-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Numero periodi di riferimento              *
      *    *-----------------------------------------------------------*
       acc-num-pdr-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Eventuale forzatura                         *
      *                  *---------------------------------------------*
           if        rr-gen-grf           not  = "S"
                     go to acc-num-pdr-025.
      *                  *---------------------------------------------*
      *                  * 1 solo periodo                              *
      *                  *---------------------------------------------*
           move      01                   to   rr-num-pdr             .
           perform   vis-num-pdr-000      thru vis-num-pdr-999        .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     acc-num-pdr-999.
       acc-num-pdr-025.
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
       acc-num-pdr-030.
      *                      *-----------------------------------------*
      *                      * Se il tipo dettaglio stampa non implica *
      *                      * il dettaglio dei documenti, si accetta  *
      *                      * il campo                                *
      *                      *-----------------------------------------*
           if        rr-tip-dts           not  = 03
                     go to acc-num-pdr-100.
       acc-num-pdr-035.
      *                      *-----------------------------------------*
      *                      * Altrimenti si normalizzano i campi ne-  *
      *                      * cessari e poi si esce                   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Numero periodi di riferimento       *
      *                          *-------------------------------------*
           move      01                   to   rr-num-pdr             .
           perform   vis-num-pdr-000      thru vis-num-pdr-999        .
      *                          *-------------------------------------*
      *                          * 2. periodo, data min                *
      *                          *-------------------------------------*
           move      zero                 to   rr-p2d-min             .
           perform   vis-p2d-min-000      thru vis-p2d-min-999        .
      *                          *-------------------------------------*
      *                          * 2. periodo, data max                *
      *                          *-------------------------------------*
           move      zero                 to   rr-p2d-max             .
           perform   vis-p2d-max-000      thru vis-p2d-max-999        .
      *                          *-------------------------------------*
      *                          * 3. periodo, data min                *
      *                          *-------------------------------------*
           move      zero                 to   rr-p3d-min             .
           perform   vis-p3d-min-000      thru vis-p3d-min-999        .
      *                          *-------------------------------------*
      *                          * 3. periodo, data max                *
      *                          *-------------------------------------*
           move      zero                 to   rr-p3d-max             .
           perform   vis-p3d-max-000      thru vis-p3d-max-999        .
      *                          *-------------------------------------*
      *                          * Uscita                              *
      *                          *-------------------------------------*
           go to     acc-num-pdr-999.
       acc-num-pdr-100.
      *              *-------------------------------------------------*
      *              * Preparazione work per accettazione              *
      *              *-------------------------------------------------*
           if        w-prs-saf-3pr-snx    =    "S" or
                     w-prs-saf-3pr-snx    =    "F"
                     move  w-exp-num-pdr-t03
                                          to   w-exp-num-pdr-t00
           else      move  w-exp-num-pdr-t02
                                          to   w-exp-num-pdr-t00      .
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-num-pdr-lun    to   v-car                  .
           move      w-exp-num-pdr-num    to   v-ldt                  .
           move      spaces               to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      15                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-num-pdr-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   num-pdr-det-inx-000  thru num-pdr-det-inx-999    .
           move      w-exp-num-pdr-inx    to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-num-pdr-999.
       acc-num-pdr-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-exp-num-pdr-inx      .
           perform   num-pdr-det-cod-000  thru num-pdr-det-cod-999    .
           move      w-exp-num-pdr-cod    to   rr-num-pdr             .
       acc-num-pdr-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-num-pdr-425.
      *                  *---------------------------------------------*
      *                  * Che sia un valore consentito                *
      *                  *---------------------------------------------*
           if        rr-num-pdr           =    zero
                     go to acc-num-pdr-100.
       acc-num-pdr-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-num-pdr-625.
      *                  *---------------------------------------------*
      *                  * Normalizzazione date min-max 2. periodo, se *
      *                  * necessario                                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test                                    *
      *                      *-----------------------------------------*
           if        rr-num-pdr           >    1
                     go to acc-num-pdr-650.
      *                      *-----------------------------------------*
      *                      * Normalizzazione 2. periodo, data min    *
      *                      *-----------------------------------------*
           move      zero                 to   rr-p2d-min             .
           perform   vis-p2d-min-000      thru vis-p2d-min-999        .
      *                      *-----------------------------------------*
      *                      * Normalizzazione 2. periodo, data max    *
      *                      *-----------------------------------------*
           move      zero                 to   rr-p2d-max             .
           perform   vis-p2d-max-000      thru vis-p2d-max-999        .
       acc-num-pdr-650.
      *                  *---------------------------------------------*
      *                  * Normalizzazione date min-max 3. periodo, se *
      *                  * necessario                                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test                                    *
      *                      *-----------------------------------------*
           if        rr-num-pdr           =    03
                     go to acc-num-pdr-675.
      *                      *-----------------------------------------*
      *                      * Normalizzazione 3. periodo, data min    *
      *                      *-----------------------------------------*
           move      zero                 to   rr-p3d-min             .
           perform   vis-p3d-min-000      thru vis-p3d-min-999        .
      *                      *-----------------------------------------*
      *                      * Normalizzazione 3. periodo, data max    *
      *                      *-----------------------------------------*
           move      zero                 to   rr-p3d-max             .
           perform   vis-p3d-max-000      thru vis-p3d-max-999        .
       acc-num-pdr-675.
      *                  *---------------------------------------------*
      *                  * Fine dipendenze                             *
      *                  *---------------------------------------------*
           go to     acc-num-pdr-800.
       acc-num-pdr-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-num-pdr-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-num-pdr-100.
       acc-num-pdr-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Numero periodi di riferimento           *
      *    *-----------------------------------------------------------*
       vis-num-pdr-000.
      *              *-------------------------------------------------*
      *              * Preparazione work per accettazione              *
      *              *-------------------------------------------------*
           if        w-prs-saf-3pr-snx    =    "S" or
                     w-prs-saf-3pr-snx    =    "F"
                     move  w-exp-num-pdr-t03
                                          to   w-exp-num-pdr-t00
           else      move  w-exp-num-pdr-t02
                                          to   w-exp-num-pdr-t00      .
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-num-pdr-lun    to   v-car                  .
           move      w-exp-num-pdr-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      15                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-num-pdr-tbl    to   v-txt                  .
           perform   num-pdr-det-inx-000  thru num-pdr-det-inx-999    .
           move      w-exp-num-pdr-inx    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-num-pdr-999.
           exit.

      *    *===========================================================*
      *    * Subroutine per la determinazione dell'indice su tabella   *
      *    * per l'accettazione del campo espanso relativo al numero   *
      *    * di periodi di riferimento                                 *
      *    *-----------------------------------------------------------*
       num-pdr-det-inx-000.
           if        rr-num-pdr           =    zero
                     move  zero           to   w-exp-num-pdr-inx
                     go to num-pdr-det-inx-999.
       num-pdr-det-inx-100.
           move      zero                 to   w-exp-num-pdr-inx      .
       num-pdr-det-inx-200.
           add       1                    to   w-exp-num-pdr-inx      .
           if        w-exp-num-pdr-inx    >    w-exp-num-pdr-num
                     move  zero           to   w-exp-num-pdr-inx
                     go to num-pdr-det-inx-999.
           if        w-exp-num-pdr-tcx
                    (w-exp-num-pdr-inx)   =    rr-num-pdr
                     go to num-pdr-det-inx-999
           else      go to num-pdr-det-inx-200.
       num-pdr-det-inx-999.
           exit.

      *    *===========================================================*
      *    * Subroutine per la determinazione del numero periodi di    *
      *    * riferimento relativo all'indice per l'accettazione del    *
      *    * campo espanso relativo al numero di periodi riferimento   *
      *    *-----------------------------------------------------------*
       num-pdr-det-cod-000.
           if        w-exp-num-pdr-inx    =    zero           or
                     w-exp-num-pdr-inx    >    w-exp-num-pdr-num
                     move  zero           to   w-exp-num-pdr-cod
           else      move  w-exp-num-pdr-tcx
                          (w-exp-num-pdr-inx)
                                          to   w-exp-num-pdr-cod      .
       num-pdr-det-cod-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : 1. periodo di riferimento, data minima     *
      *    *-----------------------------------------------------------*
       acc-p1d-min-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-p1d-min-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      18                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-p1d-min           to   v-dat                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-p1d-min-999.
       acc-p1d-min-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-dat                to   rr-p1d-min             .
       acc-p1d-min-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-p1d-min-425.
      *                  *---------------------------------------------*
      *                  * Che non sia a zero, a meno che non si sia   *
      *                  * in Up                                       *
      *                  *---------------------------------------------*
           if        rr-p1d-min           not  = zero
                     go to acc-p1d-min-600.
           if        v-key                =    "UP  "
                     go to acc-p1d-min-600
           else      go to acc-p1d-min-100.
       acc-p1d-min-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-p1d-min-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-p1d-min-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-p1d-min-100.
       acc-p1d-min-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : 1. periodo di riferimento, data minima  *
      *    *-----------------------------------------------------------*
       vis-p1d-min-000.
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      18                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-p1d-min           to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-p1d-min-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : 1. periodo di riferimento, data massima    *
      *    *-----------------------------------------------------------*
       acc-p1d-max-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-p1d-max-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      18                   to   v-lin                  .
           move      45                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-p1d-max           to   v-dat                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-p1d-max-999.
       acc-p1d-max-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-dat                to   rr-p1d-max             .
       acc-p1d-max-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-p1d-max-425.
      *                  *---------------------------------------------*
      *                  * Che non sia a zero, a meno che non si sia   *
      *                  * in Up                                       *
      *                  *---------------------------------------------*
           if        rr-p1d-max           not  = zero
                     go to acc-p1d-max-600.
           if        v-key                =    "UP  "
                     go to acc-p1d-max-600
           else      go to acc-p1d-max-100.
       acc-p1d-max-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-p1d-max-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-p1d-max-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-p1d-max-100.
       acc-p1d-max-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : 1. periodo di riferimento, data massima *
      *    *-----------------------------------------------------------*
       vis-p1d-max-000.
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      18                   to   v-lin                  .
           move      45                   to   v-pos                  .
           move      rr-p1d-max           to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-p1d-max-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : 2. periodo di riferimento, data minima     *
      *    *-----------------------------------------------------------*
       acc-p2d-min-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-p2d-min-025.
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se il numero periodi di riferimento lo  *
      *                      * consente, si accetta il campo, altri-   *
      *                      * menti si esce                           *
      *                      *-----------------------------------------*
           if        rr-num-pdr           not  < 02
                     go to acc-p2d-min-100
           else      go to acc-p2d-min-999.
       acc-p2d-min-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      19                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-p2d-min           to   v-dat                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-p2d-min-999.
       acc-p2d-min-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-dat                to   rr-p2d-min             .
       acc-p2d-min-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-p2d-min-425.
      *                  *---------------------------------------------*
      *                  * Che non sia a zero, a meno che non si sia   *
      *                  * in Up                                       *
      *                  *---------------------------------------------*
           if        rr-p2d-min           not  = zero
                     go to acc-p2d-min-600.
           if        v-key                =    "UP  "
                     go to acc-p2d-min-600
           else      go to acc-p2d-min-100.
       acc-p2d-min-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-p2d-min-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-p2d-min-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-p2d-min-100.
       acc-p2d-min-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : 2. periodo di riferimento, data minima  *
      *    *-----------------------------------------------------------*
       vis-p2d-min-000.
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      19                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-p2d-min           to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-p2d-min-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : 2. periodo di riferimento, data massima    *
      *    *-----------------------------------------------------------*
       acc-p2d-max-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-p2d-max-025.
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se il numero periodi di riferimento lo  *
      *                      * consente, si accetta il campo, altri-   *
      *                      * menti si esce                           *
      *                      *-----------------------------------------*
           if        rr-num-pdr           not  < 02
                     go to acc-p2d-max-100
           else      go to acc-p2d-max-999.
       acc-p2d-max-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      19                   to   v-lin                  .
           move      45                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-p2d-max           to   v-dat                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-p2d-max-999.
       acc-p2d-max-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-dat                to   rr-p2d-max             .
       acc-p2d-max-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-p2d-max-425.
      *                  *---------------------------------------------*
      *                  * Che non sia a zero, a meno che non si sia   *
      *                  * in Up                                       *
      *                  *---------------------------------------------*
           if        rr-p2d-max           not  = zero
                     go to acc-p2d-max-600.
           if        v-key                =    "UP  "
                     go to acc-p2d-max-600
           else      go to acc-p2d-max-100.
       acc-p2d-max-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-p2d-max-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-p2d-max-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-p2d-max-100.
       acc-p2d-max-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : 2. periodo di riferimento, data massima *
      *    *-----------------------------------------------------------*
       vis-p2d-max-000.
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      19                   to   v-lin                  .
           move      45                   to   v-pos                  .
           move      rr-p2d-max           to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-p2d-max-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : 3. periodo di riferimento, data minima     *
      *    *-----------------------------------------------------------*
       acc-p3d-min-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-p3d-min-025.
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se le personalizzazioni non consentono  *
      *                      * il terzo periodo di riferimento : usci- *
      *                      * ta senza accettazione                   *
      *                      *-----------------------------------------*
           if        w-prs-saf-3pr-snx    not  = "S" and
                     w-prs-saf-3pr-snx    not  = "F"
                     go to acc-p3d-min-999.
      *                      *-----------------------------------------*
      *                      * Se il numero periodi di riferimento lo  *
      *                      * consente, si accetta il campo, altri-   *
      *                      * menti si esce                           *
      *                      *-----------------------------------------*
           if        rr-num-pdr           =    03
                     go to acc-p3d-min-100
           else      go to acc-p3d-min-999.
       acc-p3d-min-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      20                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-p3d-min           to   v-dat                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-p3d-min-999.
       acc-p3d-min-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-dat                to   rr-p3d-min             .
       acc-p3d-min-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-p3d-min-425.
      *                  *---------------------------------------------*
      *                  * Che non sia a zero, a meno che non si sia   *
      *                  * in Up                                       *
      *                  *---------------------------------------------*
           if        rr-p3d-min           not  = zero
                     go to acc-p3d-min-600.
           if        v-key                =    "UP  "
                     go to acc-p3d-min-600
           else      go to acc-p3d-min-100.
       acc-p3d-min-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-p3d-min-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-p3d-min-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-p3d-min-100.
       acc-p3d-min-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : 3. periodo di riferimento, data minima  *
      *    *-----------------------------------------------------------*
       vis-p3d-min-000.
      *              *-------------------------------------------------*
      *              * Se le personalizzazioni non consentono il terzo *
      *              * periodo di riferimento : uscita senza visualiz- *
      *              * zazione                                         *
      *              *-------------------------------------------------*
           if        w-prs-saf-3pr-snx    not  = "S" and
                     w-prs-saf-3pr-snx    not  = "F"
                     go to vis-p3d-min-999.
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      20                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-p3d-min           to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-p3d-min-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : 3. periodo di riferimento, data massima    *
      *    *-----------------------------------------------------------*
       acc-p3d-max-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-p3d-max-025.
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se le personalizzazioni non consentono  *
      *                      * il terzo periodo di riferimento : usci- *
      *                      * ta senza accettazione                   *
      *                      *-----------------------------------------*
           if        w-prs-saf-3pr-snx    not  = "S" and
                     w-prs-saf-3pr-snx    not  = "F"
                     go to acc-p3d-max-999.
      *                      *-----------------------------------------*
      *                      * Se il numero periodi di riferimento lo  *
      *                      * consente, si accetta il campo, altri-   *
      *                      * menti si esce                           *
      *                      *-----------------------------------------*
           if        rr-num-pdr           =    03
                     go to acc-p3d-max-100
           else      go to acc-p3d-max-999.
       acc-p3d-max-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      20                   to   v-lin                  .
           move      45                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-p3d-max           to   v-dat                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-p3d-max-999.
       acc-p3d-max-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-dat                to   rr-p3d-max             .
       acc-p3d-max-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-p3d-max-425.
      *                  *---------------------------------------------*
      *                  * Che non sia a zero, a meno che non si sia   *
      *                  * in Up                                       *
      *                  *---------------------------------------------*
           if        rr-p3d-max           not  = zero
                     go to acc-p3d-max-600.
           if        v-key                =    "UP  "
                     go to acc-p3d-max-600
           else      go to acc-p3d-max-100.
       acc-p3d-max-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-p3d-max-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-p3d-max-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-p3d-max-100.
       acc-p3d-max-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : 3. periodo di riferimento, data massima *
      *    *-----------------------------------------------------------*
       vis-p3d-max-000.
      *              *-------------------------------------------------*
      *              * Se le personalizzazioni non consentono il terzo *
      *              * periodo di riferimento : uscita senza visualiz- *
      *              * zazione                                         *
      *              *-------------------------------------------------*
           if        w-prs-saf-3pr-snx    not  = "S" and
                     w-prs-saf-3pr-snx    not  = "F"
                     go to vis-p3d-max-999.
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      20                   to   v-lin                  .
           move      45                   to   v-pos                  .
           move      rr-p3d-max           to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-p3d-max-999.
           exit.

      *    *===========================================================*
      *    * Controllo su tasto Do in parametri di selezione           *
      *    *-----------------------------------------------------------*
       tdo-ric-sel-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-tdo-ric-flg      .
       tdo-ric-sel-050.
      *              *-------------------------------------------------*
      *              * Controllo : Tipo di dettaglio di stampa         *
      *              *-------------------------------------------------*
       tdo-ric-sel-151.
           if        rr-tip-dts           not  = zero
                     go to tdo-ric-sel-152.
           move      "Manca il tipo di dettaglio di stampa !            
      -              "               "    to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-152.
           if        rr-tip-dts           =    01 or
                     rr-tip-dts           =    02 or
                     rr-tip-dts           =    03
                     go to tdo-ric-sel-200.
           move      "Tipo di dettaglio di stampa errato !              
      -              "               "    to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-200.
      *              *-------------------------------------------------*
      *              * Controllo : Tipo di ordinamento per forma di    *
      *              *             pagamento                           *
      *              *-------------------------------------------------*
       tdo-ric-sel-201.
           if        rr-cod-fop           not  = zero
                     go to tdo-ric-sel-250.
           if        rr-tor-fop           not  = zero
                     go to tdo-ric-sel-202.
           move      "Manca il tipo di ordinamento per la forma pagament
      -              "o !            "    to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-202.
           if        rr-tor-fop           =    01 or
                     rr-tor-fop           =    02 or
                     rr-tor-fop           =    03 or
                     rr-tor-fop           =    04 or
                     rr-tor-fop           =    05
                     go to tdo-ric-sel-250.
           move      "Tipo di ordinamento per la forma pagamento errato 
      -              "!              "    to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-250.
      *              *-------------------------------------------------*
      *              * Controllo : Tipo di ordinamento per fornitori   *
      *              *-------------------------------------------------*
       tdo-ric-sel-251.
           if        rr-tip-dts           =    01
                     go to tdo-ric-sel-300.
           if        rr-tor-fnt           not  = zero
                     go to tdo-ric-sel-252.
           move      "Manca il tipo di ordinamento per i fornitori !    
      -              "               "    to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-252.
           if        rr-tor-fnt           =    01 or
                     rr-tor-fnt           =    02 or
                     rr-tor-fnt           =    03 or
                     rr-tor-fnt           =    04 or
                     rr-tor-fnt           =    05
                     go to tdo-ric-sel-300.
           move      "Tipo di ordinamento per i fornitori errato !      
      -              "               "    to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-300.
      *              *-------------------------------------------------*
      *              * Controllo : Numero periodi di riferimento       *
      *              *-------------------------------------------------*
       tdo-ric-sel-301.
           if        w-prs-saf-3pr-snx    =    "S" or
                     w-prs-saf-3pr-snx    =    "F"
                     move  w-exp-num-pdr-t03
                                          to   w-exp-num-pdr-t00
           else      move  w-exp-num-pdr-t02
                                          to   w-exp-num-pdr-t00      .
           if        rr-num-pdr           not  = 00
                     go to tdo-ric-sel-302.
           move      "Manca il numero periodi di riferimento !          
      -              "               "    to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-302.
           perform   num-pdr-det-inx-000  thru num-pdr-det-inx-999    .
           if        w-exp-num-pdr-inx    not  = zero
                     go to tdo-ric-sel-306.
       tdo-ric-sel-304.
           move      "Numero periodi di riferimento errato !            
      -              "               "    to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-306.
           if        rr-tip-dts           =    03
                     if    rr-num-pdr     =    01
                           go to tdo-ric-sel-350
                     else  go to tdo-ric-sel-304.
       tdo-ric-sel-350.
      *              *-------------------------------------------------*
      *              * Controllo : 1. periodo di riferimento, data     *
      *              *                minima                           *
      *              *-------------------------------------------------*
       tdo-ric-sel-351.
           if        rr-p1d-min           not  = zero
                     go to tdo-ric-sel-400.
           move      "Manca la data iniziale per il 1. periodo di riferi
      -              "mento !        "    to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-400.
      *              *-------------------------------------------------*
      *              * Controllo : 1. periodo di riferimento, data     *
      *              *                massima                          *
      *              *-------------------------------------------------*
       tdo-ric-sel-401.
           if        rr-p1d-max           not  = zero
                     go to tdo-ric-sel-402.
           move      "Manca la data finale per il 1. periodo di riferime
      -              "nto !          "    to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-402.
           if        rr-p1d-max           not  < rr-p1d-min
                     go to tdo-ric-sel-450.
           move      "La data finale del 1. periodo e' inferiore alla da
      -              "ta iniziale !  "    to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-450.
      *              *-------------------------------------------------*
      *              * Controllo : 2. periodo di riferimento, data     *
      *              *                minima                           *
      *              *-------------------------------------------------*
       tdo-ric-sel-451.
           if        rr-num-pdr           =    01
                     go to tdo-ric-sel-500.
       tdo-ric-sel-452.
           if        rr-p2d-min           not  = zero
                     go to tdo-ric-sel-500.
           move      "Manca la data iniziale per il 2. periodo di riferi
      -              "mento !        "    to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-500.
      *              *-------------------------------------------------*
      *              * Controllo : 2. periodo di riferimento, data     *
      *              *                massima                          *
      *              *-------------------------------------------------*
       tdo-ric-sel-501.
           if        rr-num-pdr           =    01
                     go to tdo-ric-sel-550.
       tdo-ric-sel-502.
           if        rr-p2d-max           not  = zero
                     go to tdo-ric-sel-503.
           move      "Manca la data finale per il 2. periodo di riferime
      -              "nto !          "    to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-503.
           if        rr-p2d-max           not  < rr-p2d-min
                     go to tdo-ric-sel-550.
           move      "La data finale del 2. periodo e' inferiore alla da
      -              "ta iniziale !  "    to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-550.
      *              *-------------------------------------------------*
      *              * Controllo : 3. periodo di riferimento, data     *
      *              *                minima                           *
      *              *-------------------------------------------------*
       tdo-ric-sel-551.
           if        rr-num-pdr           not  = 03
                     go to tdo-ric-sel-600.
       tdo-ric-sel-552.
           if        rr-p3d-min           not  = zero
                     go to tdo-ric-sel-600.
           move      "Manca la data iniziale per il 3. periodo di riferi
      -              "mento !        "    to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-600.
      *              *-------------------------------------------------*
      *              * Controllo : 3. periodo di riferimento, data     *
      *              *                massima                          *
      *              *-------------------------------------------------*
       tdo-ric-sel-601.
           if        rr-num-pdr           not  = 03
                     go to tdo-ric-sel-650.
       tdo-ric-sel-602.
           if        rr-p3d-max           not  = zero
                     go to tdo-ric-sel-603.
           move      "Manca la data finale per il 3. periodo di riferime
      -              "nto !          "    to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-603.
           if        rr-p3d-max           not  < rr-p3d-min
                     go to tdo-ric-sel-650.
           move      "La data finale del 3. periodo e' inferiore alla da
      -              "ta iniziale !  "    to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-650.
      *              *-------------------------------------------------*
      *              * Controllo : 1. periodo di riferimento con       *
      *              *             2. periodo di riferimento           *
      *              *-------------------------------------------------*
       tdo-ric-sel-651.
           if        rr-num-pdr           =    01
                     go to tdo-ric-sel-700.
       tdo-ric-sel-652.
           if        rr-p1d-min           not > rr-p2d-min
                     go to tdo-ric-sel-700.
           move      "Il 2. periodo di riferimento non puo' precedere il
      -              " 1. periodo !  "    to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-700.
      *              *-------------------------------------------------*
      *              * Controllo : 2. periodo di riferimento con       *
      *              *             3. periodo di riferimento           *
      *              *-------------------------------------------------*
       tdo-ric-sel-701.
           if        rr-num-pdr           not  = 03
                     go to tdo-ric-sel-750.
       tdo-ric-sel-702.
           if        rr-p2d-min           not > rr-p3d-min
                     go to tdo-ric-sel-750.
           move      "Il 3. periodo di riferimento non puo' precedere il
      -              " 2. periodo !  "    to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-750.
      *              *-------------------------------------------------*
      *              * Uscita per controlli superati                   *
      *              *-------------------------------------------------*
           go to     tdo-ric-sel-999.
       tdo-ric-sel-900.
      *              *-------------------------------------------------*
      *              * Trattamento errore                              *
      *              *-------------------------------------------------*
           perform   box-msg-err-000      thru box-msg-err-999        .
      *              *-------------------------------------------------*
      *              * Flag di uscita ad errore                        *
      *              *-------------------------------------------------*
           move      "#"                  to   w-cnt-tdo-ric-flg      .
       tdo-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Regolarizzazione dei parametri di selezione               *
      *    *-----------------------------------------------------------*
       reg-ric-sel-000.
      *              *-------------------------------------------------*
      *              * Regolarizzazione scelta sui tipi scadenza       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo scedenza da *
      *                  * includere scelto                            *
      *                  *---------------------------------------------*
           go to     reg-ric-sel-010
                     reg-ric-sel-020
                     reg-ric-sel-030
                     reg-ric-sel-040
                     reg-ric-sel-050
                     reg-ric-sel-060
                     reg-ric-sel-070
                     reg-ric-sel-080
                     reg-ric-sel-090
                     reg-ric-sel-100
                     reg-ric-sel-110
                     reg-ric-sel-120
                     reg-ric-sel-130
                     reg-ric-sel-140
                     reg-ric-sel-150
                     reg-ric-sel-160
                     depending            on  rr-tip-scd              .
       reg-ric-sel-010.
      *                  *---------------------------------------------*
      *                  * Tipo scadenza da includere : Tutti          *
      *                  *---------------------------------------------*
           move      all "S"              to   rr-sce-tsc             .
           go to     reg-ric-sel-200.
       reg-ric-sel-020.
      *                  *---------------------------------------------*
      *                  * Tipo scadenza da includere : A scelta       *
      *                  *---------------------------------------------*
           go to     reg-ric-sel-200.
       reg-ric-sel-030.
      *                  *---------------------------------------------*
      *                  * Tipo scadenza da includere : Rimesse diret- *
      *                  * te e Bonifici                               *
      *                  *---------------------------------------------*
           move      all "N"              to   rr-sce-tsc             .
           move      "S"                  to   rr-snx-tsc (01)        .
           move      "S"                  to   rr-snx-tsc (08)        .
           go to     reg-ric-sel-200.
       reg-ric-sel-040.
      *                  *---------------------------------------------*
      *                  * Tipo scadenza da includere : Scadenze elet- *
      *                  * troniche                                    *
      *                  *---------------------------------------------*
           move      all "N"              to   rr-sce-tsc             .
           move      "S"                  to   rr-snx-tsc (03)        .
           move      "S"                  to   rr-snx-tsc (04)        .
           move      "S"                  to   rr-snx-tsc (05)        .
           move      "S"                  to   rr-snx-tsc (06)        .
           move      "S"                  to   rr-snx-tsc (07)        .
           go to     reg-ric-sel-200.
       reg-ric-sel-050.
      *                  *---------------------------------------------*
      *                  * Tipo scadenza da includere : Ricevute banca-*
      *                  * rie e Tratte                                *
      *                  *---------------------------------------------*
           move      all "N"              to   rr-sce-tsc             .
           move      "S"                  to   rr-snx-tsc (10)        .
           move      "S"                  to   rr-snx-tsc (11)        .
           go to     reg-ric-sel-200.
       reg-ric-sel-060.
      *                  *---------------------------------------------*
      *                  * Tipo scadenza da includere : Solo Rimesse   *
      *                  * dirette                                     *
      *                  *---------------------------------------------*
           move      all "N"              to   rr-sce-tsc             .
           move      "S"                  to   rr-snx-tsc (01)        .
           go to     reg-ric-sel-200.
       reg-ric-sel-070.
      *                  *---------------------------------------------*
      *                  * Tipo scadenza da includere : Solo Incassi   *
      *                  * elettronici                                 *
      *                  *---------------------------------------------*
           move      all "N"              to   rr-sce-tsc             .
           move      "S"                  to   rr-snx-tsc (03)        .
           go to     reg-ric-sel-200.
       reg-ric-sel-080.
      *                  *---------------------------------------------*
      *                  * Tipo scadenza da includere : Solo Ri.Ba.    *
      *                  *---------------------------------------------*
           move      all "N"              to   rr-sce-tsc             .
           move      "S"                  to   rr-snx-tsc (04)        .
           go to     reg-ric-sel-200.
       reg-ric-sel-090.
      *                  *---------------------------------------------*
      *                  * Tipo scadenza da includere : Solo Conferme  *
      *                  * d'Ordine                                    *
      *                  *---------------------------------------------*
           move      all "N"              to   rr-sce-tsc             .
           move      "S"                  to   rr-snx-tsc (05)        .
           go to     reg-ric-sel-200.
       reg-ric-sel-100.
      *                  *---------------------------------------------*
      *                  * Tipo scadenza da includere : Solo M.Av.     *
      *                  *---------------------------------------------*
           move      all "N"              to   rr-sce-tsc             .
           move      "S"                  to   rr-snx-tsc (06)        .
           go to     reg-ric-sel-200.
       reg-ric-sel-110.
      *                  *---------------------------------------------*
      *                  * Tipo scadenza da includere : Solo R.I.D.    *
      *                  *---------------------------------------------*
           move      all "N"              to   rr-sce-tsc             .
           move      "S"                  to   rr-snx-tsc (07)        .
           go to     reg-ric-sel-200.
       reg-ric-sel-120.
      *                  *---------------------------------------------*
      *                  * Tipo scadenza da includere : Solo Bonifici  *
      *                  * bancari                                     *
      *                  *---------------------------------------------*
           move      all "N"              to   rr-sce-tsc             .
           move      "S"                  to   rr-snx-tsc (08)        .
           go to     reg-ric-sel-200.
       reg-ric-sel-130.
      *                  *---------------------------------------------*
      *                  * Tipo scadenza da includere : Solo C/C posta-*
      *                  * li                                          *
      *                  *---------------------------------------------*
           move      all "N"              to   rr-sce-tsc             .
           move      "S"                  to   rr-snx-tsc (09)        .
           go to     reg-ric-sel-200.
       reg-ric-sel-140.
      *                  *---------------------------------------------*
      *                  * Tipo scadenza da includere : Solo Ricevute  *
      *                  * bancarie                                    *
      *                  *---------------------------------------------*
           move      all "N"              to   rr-sce-tsc             .
           move      "S"                  to   rr-snx-tsc (10)        .
           go to     reg-ric-sel-200.
       reg-ric-sel-150.
      *                  *---------------------------------------------*
      *                  * Tipo scadenza da includere : Solo Tratte    *
      *                  *---------------------------------------------*
           move      all "N"              to   rr-sce-tsc             .
           move      "S"                  to   rr-snx-tsc (11)        .
           go to     reg-ric-sel-200.
       reg-ric-sel-160.
      *                  *---------------------------------------------*
      *                  * Tipo scadenza da includere : Solo Paghero'  *
      *                  *---------------------------------------------*
           move      all "N"              to   rr-sce-tsc             .
           move      "S"                  to   rr-snx-tsc (11)        .
           go to     reg-ric-sel-200.
       reg-ric-sel-200.
       reg-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione richieste di selezione                    *
      *    *-----------------------------------------------------------*
       nor-ric-sel-000.
      *              *-------------------------------------------------*
      *              * Precaricamento tabella dipendenze selezionate   *
      *              *-------------------------------------------------*
       nor-ric-sel-100.
      *              *-------------------------------------------------*
      *              * Normalizzazioni altri campi                     *
      *              *-------------------------------------------------*
           move      zero                 to   rr-cod-fop             .
           move      spaces               to   rr-cod-fop-des         .
           move      zero                 to   rr-tip-scd             .
           move      spaces               to   rr-sce-tsc             .
           move      zero                 to   rr-tip-dts             .
           move      zero                 to   rr-tor-fop             .
           move      zero                 to   rr-tor-fnt             .
           move      zero                 to   rr-num-pdr             .
           move      zero                 to   rr-p1d-min             .
           move      zero                 to   rr-p1d-max             .
           move      zero                 to   rr-p2d-min             .
           move      zero                 to   rr-p2d-max             .
           move      zero                 to   rr-p3d-min             .
           move      zero                 to   rr-p3d-max             .
       nor-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Preparazione parametri per selezione stampa               *
      *    *-----------------------------------------------------------*
       pre-prm-stp-000.
      *              *-------------------------------------------------*
      *              * Flags di tipo selezione                         *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-stp-tip-sel      .
      *              *-------------------------------------------------*
      *              * Codice stampante                                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-stp-cod-stp      .
      *              *-------------------------------------------------*
      *              * Tipo di stampa                                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-stp-tip-sta      .
      *              *-------------------------------------------------*
      *              * Codice modulo                                   *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-stp-cod-mod      .
      *              *-------------------------------------------------*
      *              * Tipo modulo                                     *
      *              *   - L : Libero                                  *
      *              *   - T : Tipografico                             *
      *              *-------------------------------------------------*
           move      "L"                  to   w-cnt-stp-tip-mod      .
      *              *-------------------------------------------------*
      *              * Ampiezza linea di stampa in caratteri           *
      *              *-------------------------------------------------*
           move      132                  to   w-cnt-stp-amp-lin      .
      *              *-------------------------------------------------*
      *              * Top margin in linee                             *
      *              *-------------------------------------------------*
           move      1                    to   w-cnt-stp-top-lin      .
      *              *-------------------------------------------------*
      *              * Numero linee di stampa minimo                   *
      *              *-------------------------------------------------*
           move      30                   to   w-cnt-stp-lin-min      .
      *              *-------------------------------------------------*
      *              * Bottom margin in linee                          *
      *              *-------------------------------------------------*
           move      1                    to   w-cnt-stp-bot-lin      .
      *              *-------------------------------------------------*
      *              * Ampiezza caratteri                              *
      *              *-------------------------------------------------*
           move      zero                 to   w-cnt-stp-amp-car      .
      *              *-------------------------------------------------*
      *              * Altezza interlinea                              *
      *              *-------------------------------------------------*
           move      zero                 to   w-cnt-stp-alt-int      .
      *              *-------------------------------------------------*
      *              * Area riservata per espansioni future            *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-stp-esp-fut      .
      *              *-------------------------------------------------*
      *              * Area riservata per espansioni speciali          *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-stp-fnz-spc      .
       pre-prm-stp-999.
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
      *    * Routine lettura archivio [yfp]                            *
      *    *-----------------------------------------------------------*
       let-arc-yfp-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-yfp-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice a zero                           *
      *              *-------------------------------------------------*
           if        w-let-arc-yfp-cod    =    zero
                     go to let-arc-yfp-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODFOP    "         to   f-key                  .
           move      w-let-arc-yfp-cod    to   rf-yfp-cod-fop         .
           move      "pgm/dcf/fls/ioc/obj/iofyfp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-yfp                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-yfp-400.
       let-arc-yfp-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-yfp-des-fop       to   w-let-arc-yfp-des      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-yfp-999.
       let-arc-yfp-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-yfp-flg      .
           move      all   "."            to   w-let-arc-yfp-des      .
           go to     let-arc-yfp-999.
       let-arc-yfp-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-yfp-des      .
       let-arc-yfp-999.
           exit.

      *    *===========================================================*
      *    * Subroutines per l'accettazione della forma di pagamento   *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcf/prg/cpy/acmnyfp0.acs"                   .

