       Identification Division.
       Program-Id.                                 psvf6800           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    svf                 *
      *                                Settore:    csp                 *
      *                                   Fase:    svf680              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 18/10/05    *
      *                       Ultima revisione:    NdK del 15/10/08    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Richieste per il programma psvf6801:        *
      *                                                                *
      *                    Stampa statistica di vendita sul fatturato  *
      *                    per catalogo prodotti.                      *
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
                     "svf"                                            .
      *        *-------------------------------------------------------*
      *        * Settore gestionale                                    *
      *        *-------------------------------------------------------*
           05  i-ide-set                  pic  x(03) value
                     "csp"                                            .
      *        *-------------------------------------------------------*
      *        * Fase gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-fas                  pic  x(06) value
                     "svf680"                                         .
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10) value
                     "psvf6800"                                       .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  i-ide-des                  pic  x(40) value
                     "    FATTURATO PER CATALOGO PRODOTTI     "       .

      *    *===========================================================*
      *    * Area per il programma di esecuzione                       *
      *    *-----------------------------------------------------------*
       01  i-exe.
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma di esecuzione             *
      *        *-------------------------------------------------------*
           05  i-exe-pro                  pic  x(10) value
                     "psvf6801  "                                     .
      *        *-------------------------------------------------------*
      *        * Pathname del programma di esecuzione                  *
      *        *-------------------------------------------------------*
           05  i-exe-pat                  pic  x(40) value
                     "pgm/svf/prg/obj/psvf6801                "       .

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
      *        * Area [svf]                                            *
      *        *-------------------------------------------------------*
           05  w-arg-snx-svf.
      *            *---------------------------------------------------*
      *            * Si/No area [svf] attiva                           *
      *            * - S : Si'                                         *
      *            * - N : No                                          *
      *            *---------------------------------------------------*
               10  w-arg-snx-svf-snx      pic  x(01)                  .

      *    *===========================================================*
      *    * Work per personalizzazione 'pgm/svf/[snx-3pr]'            *
      *    *-----------------------------------------------------------*
       01  w-prs-svf-3pr.
      *        *-------------------------------------------------------*
      *        * Si/No trattamento terzo periodo di riferimento nelle  *
      *        * statistiche sul fatturato                             *
      *        *   - N : No, mai                                       *
      *        *   - S : Si, sempre                                    *
      *        *   - F : Si, ma solo in statistiche sul fatturato      *
      *        *   - P : Si, ma solo in statistiche sui prodotti       *
      *        *-------------------------------------------------------*
           05  w-prs-svf-3pr-snx          pic  x(01)                  .

      *    *===========================================================*
      *    * Work per personalizzazione 'pgm/svf[cod-dpz]'             *
      *    *-----------------------------------------------------------*
       01  w-prs-dpz-svf.
      *        *-------------------------------------------------------*
      *        * Definizione generale della personalizzazione          *
      *        *-------------------------------------------------------*
           05  w-prs-dpz-svf-prs.
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
               10  w-prs-dpz-svf-pvz      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Tabella dei codici dipendenza, max 20 codici      *
      *            *---------------------------------------------------*
               10  w-prs-dpz-svf-tcc.
      *                *-----------------------------------------------*
      *                * Singolo elemento in tabella                   *
      *                *-----------------------------------------------*
                   15  w-prs-dpz-svf-ecc occurs 20.
      *                    *-------------------------------------------*
      *                    * Separatore                                *
      *                    *-------------------------------------------*
                       20  w-prs-dpz-svf-sep
                                          pic  x(01)                  .
      *                    *-------------------------------------------*
      *                    * Codice dipendenza                         *
      *                    *-------------------------------------------*
                       20  w-prs-dpz-svf-cod
                                          pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Contatori ed indici di comodo                         *
      *        *-------------------------------------------------------*
           05  w-prs-dpz-svf-c01          pic  9(02)                  .
           05  w-prs-dpz-svf-c02          pic  9(02)                  .

      *    *===========================================================*
      *    * Records files                                             *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [zcp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfzcp"                          .

      *    *===========================================================*
      *    * Work-area richieste per stampa                            *
      *    *-----------------------------------------------------------*
       01  rr.
      *        *-------------------------------------------------------*
      *        * Codice dipendenza in uso                              *
      *        *-------------------------------------------------------*
           05  rr-dpz-inu                 pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Denominazione dipendenza in uso                       *
      *        *-------------------------------------------------------*
           05  rr-dpz-inu-den             pic  x(20)                  .
      *        *-------------------------------------------------------*
      *        * Numero globale dipendenze                             *
      *        *-------------------------------------------------------*
           05  rr-dpz-ctr-dpz             pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Numero dipendenze selezionate                         *
      *        *-------------------------------------------------------*
           05  rr-dpz-ctr-sel             pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Tabella dipendenze selezionate                        *
      *        *-------------------------------------------------------*
           05  rr-dpz-tbl-dpz.
      *            *---------------------------------------------------*
      *            * Elementi della tabella dipendenze selezionate     *
      *            *---------------------------------------------------*
               10  rr-dpz-ele-dpz occurs 99.
      *                *-----------------------------------------------*
      *                * Codice dipendenza selezionato                 *
      *                *-----------------------------------------------*
                   15  rr-dpz-ele-cod     pic  9(02)                  .
      *                *-----------------------------------------------*
      *                * Denominazione dipendenza                      *
      *                *-----------------------------------------------*
                   15  rr-dpz-ele-den     pic  x(20)                  .
      *        *-------------------------------------------------------*
      *        * Codice catalogo                                       *
      *        *-------------------------------------------------------*
           05  rr-cod-cpv                 pic  x(03)                  .
      *        *-------------------------------------------------------*
      *        * Codice catalogo, descrizione                          *
      *        *-------------------------------------------------------*
           05  rr-cod-cpv-des             pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Tipo di dettaglio di stampa                           *
      *        *  - 01 : Solo il totale per ogni catalogo prodotti     *
      *        *  - 02 : Con lista documenti emessi per ogni catalogo  *
      *        *         prodotti                                      *
      *        *-------------------------------------------------------*
           05  rr-tip-dts                 pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Tipo di ordinamento per catalogo prodotti             *
      *        *  - 01 : Ordinamento ABC decrescente sul fatturato     *
      *        *  - 02 : Ordinamento ABC decrescente sulla quantita'   *
      *        *  - 03 : Ordinamento ABC crescente sul fatturato       *
      *        *  - 04 : Ordinamento ABC crescente sulla quantita'     *
      *        *  - 05 : Ordinamento per descrizione catalogo          *
      *        *  - 06 : Ordinamento per codice catalogo               *
      *        *-------------------------------------------------------*
           05  rr-tor-csp                 pic  9(02)                  .
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
      *        * Forzato a 01 se tipo dettaglio stampa 02, ovvero con  *
      *        * la lista documenti emessi per ogni catalogo           *
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
      *        * Tipo calcolo % di variazione                          *
      *        *  - 01 : Sul fatturato                                 *
      *        *  - 02 : Sulla quantita'                               *
      *        *                                                       *
      *        * Non significativo se il numero periodi di riferimento *
      *        * e' minore di 02, pero' significativo se pari a 12     *
      *        *-------------------------------------------------------*
           05  rr-tpc-pdv                 pic  9(02)                  .

      *    *===========================================================*
      *    * Work per subroutines di Let                               *
      *    *-----------------------------------------------------------*
       01  w-let.
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [zcp]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-zcp.
               10  w-let-arc-zcp-flg      pic  x(01)                  .
               10  w-let-arc-zcp-cod      pic  x(03)                  .
               10  w-let-arc-zcp-des      pic  x(40)                  .

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
      *        * Work per : Tipo dettaglio stampa                      *
      *        *-------------------------------------------------------*
           05  w-exp-tip-dts.
               10  w-exp-tip-dts-num      pic  9(02)       value 2    .
               10  w-exp-tip-dts-lun      pic  9(02)       value 50   .
               10  w-exp-tip-dts-tbl.
                   15  filler             pic  x(50) value
                  "Solo il totale per ogni catalogo                  ".
                   15  filler             pic  x(50) value
                  "Con la lista documenti emessi per ogni catalogo   ".
      *        *-------------------------------------------------------*
      *        * Work per : Tipo ordinamento catalogo prodotti         *
      *        *-------------------------------------------------------*
           05  w-exp-tor-csp.
               10  w-exp-tor-csp-num      pic  9(02)       value 6    .
               10  w-exp-tor-csp-lun      pic  9(02)       value 50   .
               10  w-exp-tor-csp-tbl.
                   15  filler             pic  x(50) value
                  "Ordinamento ABC decrescente sul fatturato         ".
                   15  filler             pic  x(50) value
                  "Ordinamento ABC decrescente sulla quantita'       ".
                   15  filler             pic  x(50) value
                  "Ordinamento ABC crescente sul fatturato           ".
                   15  filler             pic  x(50) value
                  "Ordinamento ABC crescente sulla quantita'         ".
                   15  filler             pic  x(50) value
                  "Ordinamento per descrizione catalogo              ".
                   15  filler             pic  x(50) value
                  "Ordinamento per codice catalogo                   ".
      *        *-------------------------------------------------------*
      *        * Work per : Numero periodi di riferimento              *
      *        *-------------------------------------------------------*
           05  w-exp-num-pdr-t00.
               10  w-exp-num-pdr-num      pic  9(02)                  .
               10  w-exp-num-pdr-lun      pic  9(02)                  .
               10  w-exp-num-pdr-tbl.
                   15  filler             pic  x(50)                  .
                   15  filler             pic  x(50)                  .
                   15  filler             pic  x(50)                  .
                   15  filler             pic  x(50)                  .
                   15  filler             pic  x(50)                  .
               10  w-exp-num-pdr-tco.
                   15  w-exp-num-pdr-tcx
                               occurs 05  pic  9(02)                  .
           05  w-exp-num-pdr-t02.
               10  w-exp-num-pdr-nu2      pic  9(02)       value 4    .
               10  w-exp-num-pdr-lu2      pic  9(02)       value 50   .
               10  w-exp-num-pdr-tb2.
                   15  filler             pic  x(50) value
                  "Un solo periodo di riferimento, dal .. al ..      ".
                   15  filler             pic  x(50) value
                  "Due periodi di riferimento confrontati tra loro   ".
                   15  filler             pic  x(50) value
                  "Due periodi, ma solo se il 1. periodo e' a zero   ".
                   15  filler             pic  x(50) value
                  "Due periodi, ma solo se il 2. periodo e' a zero   ".
                   15  filler             pic  x(50) value
                  "                                                  ".
               10  w-exp-num-pdr-tc2.
                   15  filler             pic  9(02)       value 01   .
                   15  filler             pic  9(02)       value 02   .
                   15  filler             pic  9(02)       value 11   .
                   15  filler             pic  9(02)       value 12   .
                   15  filler             pic  9(02)       value 00   .
           05  w-exp-num-pdr-t03.
               10  w-exp-num-pdr-nu3      pic  9(02)       value 5    .
               10  w-exp-num-pdr-lu3      pic  9(02)       value 50   .
               10  w-exp-num-pdr-tb3.
                   15  filler             pic  x(50) value
                  "Un solo periodo di riferimento, dal .. al ..      ".
                   15  filler             pic  x(50) value
                  "Due periodi di riferimento confrontati tra loro   ".
                   15  filler             pic  x(50) value
                  "Tre periodi di riferimento confrontati tra loro   ".
                   15  filler             pic  x(50) value
                  "Due periodi, ma solo se il 1. periodo e' a zero   ".
                   15  filler             pic  x(50) value
                  "Due periodi, ma solo se il 2. periodo e' a zero   ".
               10  w-exp-num-pdr-tc3.
                   15  filler             pic  9(02)       value 01   .
                   15  filler             pic  9(02)       value 02   .
                   15  filler             pic  9(02)       value 03   .
                   15  filler             pic  9(02)       value 11   .
                   15  filler             pic  9(02)       value 12   .
           05  w-exp-num-pdr-war.
               10  w-exp-num-pdr-inx      pic  9(02)                  .
               10  w-exp-num-pdr-cod      pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Work per : Tipo calcolo percentuale di variazione     *
      *        *-------------------------------------------------------*
           05  w-exp-tpc-pdv.
               10  w-exp-tpc-pdv-num      pic  9(02)       value 2    .
               10  w-exp-tpc-pdv-lun      pic  9(02)       value 40   .
               10  w-exp-tpc-pdv-tbl.
                   15  filler             pic  x(40) value
                  "Per confronto tra i fatturati           "          .
                   15  filler             pic  x(40) value
                  "Per confronto tra le quantita'          "          .

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
      *    * Link-area per accettazione codice catalogo                *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/acodzcp0.acl"                   .

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
      *              * Test se area [svf] attiva                       *
      *              *-------------------------------------------------*
           perform   arg-svf-snx-000      thru arg-svf-snx-999        .
           if        w-arg-snx-svf-snx    =    "S"
                     go to pre-exe-pgm-100.
           perform   arg-svf-err-000      thru arg-svf-err-999        .
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
           perform   prs-dpz-svf-000      thru prs-dpz-svf-999        .
       pre-exe-pgm-300.
      *              *-------------------------------------------------*
      *              * Determinazione codici dipendenze per l'azienda  *
      *              *-------------------------------------------------*
           move      "IG"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      "DA"                 to   w-dpz-tip-ope          .
           move      s-ter                to   w-dpz-ide-ter          .
           move      s-ute                to   w-dpz-ide-ute          .
           move      s-azi                to   w-dpz-ide-azi          .
           move      s-sap                to   w-dpz-ide-sap          .
           move      s-arg                to   w-dpz-ide-arg          .
           move      s-set                to   w-dpz-ide-set          .
           move      s-fas                to   w-dpz-ide-fas          .
           move      i-ide-des            to   w-dpz-ide-des          .
           move      s-pro                to   w-dpz-ide-pro          .
           call      "pgm/azi/prg/obj/pazi000d"
                                         using w-dpz                  .
       pre-exe-pgm-305.
      *              *-------------------------------------------------*
      *              * Se zero dipendenze : errore ed uscita           *
      *              *-------------------------------------------------*
           if        w-dpz-ctr-dpz        >    zero
                     go to pre-exe-pgm-310.
           move      "EN"                 to   w-dpz-tip-ope          .
           call      "pgm/azi/prg/obj/pazi000d"
                                         using w-dpz                  .
           cancel    "pgm/azi/prg/obj/pazi000d"                       .
           move      "#"                  to   w-cnt-pre-exe-pgm      .
           go to     pre-exe-pgm-999.
       pre-exe-pgm-310.
      *              *-------------------------------------------------*
      *              * Selezione codice dipendenza per il programma,   *
      *              * con possibilita' di scegliere una o piu' di-    *
      *              * pendenze                                        *
      *              *-------------------------------------------------*
           move      "S+"                 to   w-dpz-tip-ope          .
           call      "pgm/azi/prg/obj/pazi000d"
                                         using w-dpz                  .
           cancel    "pgm/azi/prg/obj/pazi000d"                       .
       pre-exe-pgm-315.
      *              *-------------------------------------------------*
      *              * Se scelta non effettuata : uscita               *
      *              *-------------------------------------------------*
           if        w-dpz-ctr-dpz        =    zero
                     move  "#"            to   w-cnt-pre-exe-pgm
                     go to pre-exe-pgm-999.
       pre-exe-pgm-320.
      *              *-------------------------------------------------*
      *              * Altrimenti memorizzazione in campo selezione    *
      *              *                                                 *
      *              * - 00 : tutte le dipendenze                      *
      *              * - nn : codice dipendenza selezionata            *
      *              *-------------------------------------------------*
           move      w-dpz-cod-prg        to   rr-dpz-inu             .
       pre-exe-pgm-325.
      *              *-------------------------------------------------*
      *              * Memorizzazione denominazione dipendenza in uso  *
      *              *-------------------------------------------------*
           if        rr-dpz-inu           =    zero
                     move  spaces         to   rr-dpz-inu-den
           else      move  w-dpz-ele-den
                          (rr-dpz-inu)    to   rr-dpz-inu-den         .
       pre-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Test se area [svf] attiva                                 *
      *    *-----------------------------------------------------------*
       arg-svf-snx-000.
      *              *-------------------------------------------------*
      *              * Lettura della personalizzazione                 *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/svf[snx]"       to   s-alf                  .
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
           move      s-alf                to   w-arg-snx-svf-snx      .
       arg-svf-snx-999.
           exit.

      *    *===========================================================*
      *    * Errore per area [svf] non attiva                          *
      *    *-----------------------------------------------------------*
       arg-svf-err-000.
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
       arg-svf-err-999.
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
           string    "pgm/svf[snx-3pr]"
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
           move      s-alf                to   w-prs-svf-3pr          .
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
           move      "pgm/svf[snx-3pr]"   to   s-alf                  .
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
           move      spaces               to   w-prs-svf-3pr          .
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
           move      s-alf                to   w-prs-svf-3pr          .
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
           if        w-prs-svf-3pr-snx    not  = "S" and
                     w-prs-svf-3pr-snx    not  = "F" and
                     w-prs-svf-3pr-snx    not  = "P"
                     move  "N"            to   w-prs-svf-3pr-snx      .
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
       prs-dpz-svf-000.
      *              *-------------------------------------------------*
      *              * Personalizzazione per la specifica fase gestio- *
      *              * nale                                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura della personalizzazione             *
      *                  *---------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      spaces               to   s-alf                  .
           string    "pgm/svf[cod-dpz]"
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
                     go to prs-dpz-svf-150.
       prs-dpz-svf-100.
      *                  *---------------------------------------------*
      *                  * Se personalizzazione non esistente          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * A lettura personalizzazione valida per  *
      *                      * tutte le fasi gestionali                *
      *                      *-----------------------------------------*
           go to     prs-dpz-svf-500.
       prs-dpz-svf-150.
      *                  *---------------------------------------------*
      *                  * Se personalizzazione esistente              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Parametri letti in area di lavoro       *
      *                      *-----------------------------------------*
           move      s-alf                to   w-prs-dpz-svf-prs      .
      *                      *-----------------------------------------*
      *                      * A regolarizzazione parametri            *
      *                      *-----------------------------------------*
           go to     prs-dpz-svf-700.
       prs-dpz-svf-500.
      *              *-------------------------------------------------*
      *              * Personalizzazione per tutte le fasi gestionali  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura della personalizzazione             *
      *                  *---------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/svf[cod-dpz]"   to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda dell'esito della let-  *
      *                  * tura                                        *
      *                  *---------------------------------------------*
           if        s-ves                =    spaces
                     go to prs-dpz-svf-650.
       prs-dpz-svf-600.
      *                  *---------------------------------------------*
      *                  * Se personalizzazione non esistente          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazione parametri in area di    *
      *                      * lavoro                                  *
      *                      *-----------------------------------------*
           move      spaces               to   w-prs-dpz-svf-prs      .
      *                      *-----------------------------------------*
      *                      * A regolarizzazione parametri            *
      *                      *-----------------------------------------*
           go to     prs-dpz-svf-700.
       prs-dpz-svf-650.
      *                  *---------------------------------------------*
      *                  * Se personalizzazione esistente              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Parametri letti in area di lavoro       *
      *                      *-----------------------------------------*
           move      s-alf                to   w-prs-dpz-svf-prs      .
      *                      *-----------------------------------------*
      *                      * A regolarizzazione parametri            *
      *                      *-----------------------------------------*
           go to     prs-dpz-svf-700.
       prs-dpz-svf-700.
      *              *-------------------------------------------------*
      *              * Regolarizzazione parametri                      *
      *              *-------------------------------------------------*
       prs-dpz-svf-720.
      *                  *---------------------------------------------*
      *                  * Provenienza dei codici dipendenza da trat-  *
      *                  * tare                                        *
      *                  *---------------------------------------------*
           if        w-prs-dpz-svf-pvz    not  = "+" and
                     w-prs-dpz-svf-pvz    not  = "-"
                     move  "R"            to   w-prs-dpz-svf-pvz      .
       prs-dpz-svf-740.
      *                  *---------------------------------------------*
      *                  * Normalizzazione e compattamento codici di   *
      *                  * pendenza specificati                        *
      *                  *---------------------------------------------*
       prs-dpz-svf-742.
           move      zero                 to   w-prs-dpz-svf-c01      .
           move      zero                 to   w-prs-dpz-svf-c02      .
       prs-dpz-svf-744.
           add       1                    to   w-prs-dpz-svf-c01      .
           if        w-prs-dpz-svf-c01    >    20
                     go to prs-dpz-svf-746.
           move      spaces               to   w-prs-dpz-svf-sep
                                              (w-prs-dpz-svf-c01)     .
           if        w-prs-dpz-svf-cod
                    (w-prs-dpz-svf-c01)   not  numeric
                     move  zero           to   w-prs-dpz-svf-cod
                                              (w-prs-dpz-svf-c01)
                     go to prs-dpz-svf-744.
           if        w-prs-dpz-svf-pvz    =    "R"
                     move  zero           to   w-prs-dpz-svf-cod
                                              (w-prs-dpz-svf-c01)
                     go to prs-dpz-svf-744.
           add       1                    to   w-prs-dpz-svf-c02      .
           if        w-prs-dpz-svf-c02    =    w-prs-dpz-svf-c01
                     go to prs-dpz-svf-744.
           move      w-prs-dpz-svf-cod
                    (w-prs-dpz-svf-c01)   to   w-prs-dpz-svf-cod
                                              (w-prs-dpz-svf-c02)     .
           go to     prs-dpz-svf-744.
       prs-dpz-svf-746.
           add       1                    to   w-prs-dpz-svf-c02      .
           if        w-prs-dpz-svf-c02    >    20
                     go to prs-dpz-svf-760.
           move      zero                 to   w-prs-dpz-svf-cod
                                              (w-prs-dpz-svf-c02)     .
           go to     prs-dpz-svf-746.
       prs-dpz-svf-760.
      *                  *---------------------------------------------*
      *                  * Fine regolarizzazione parametri             *
      *                  * tare                                        *
      *                  *---------------------------------------------*
           go to     prs-dpz-svf-800.
       prs-dpz-svf-800.
      *              *-------------------------------------------------*
      *              * Preparazione richiamo determinazione codici di- *
      *              * pendenza per l'azienda e richiamo selezione     *
      *              * multipla codice dipendenza                      *
      *              *-------------------------------------------------*
       prs-dpz-svf-810.
      *                  *---------------------------------------------*
      *                  * Tipo provenienza codice dipendenza          *
      *                  *---------------------------------------------*
           move      "R0"                 to   w-dpz-tip-ope          .
           move      w-prs-dpz-svf-pvz    to   w-dpz-snx-zer          .
           call      "pgm/azi/prg/obj/pazi000d"
                                         using w-dpz                  .
       prs-dpz-svf-820.
      *                  *---------------------------------------------*
      *                  * Codici delle dipendenze                     *
      *                  *---------------------------------------------*
       prs-dpz-svf-822.
           move      zero                 to   w-prs-dpz-svf-c01      .
       prs-dpz-svf-824.
           add       1                    to   w-prs-dpz-svf-c01      .
           if        w-prs-dpz-svf-c01    >    20
                     go to prs-dpz-svf-830.
           if        w-prs-dpz-svf-cod
                    (w-prs-dpz-svf-c01)   =    zero
                     go to prs-dpz-svf-830.
           move      "R5"                 to   w-dpz-tip-ope          .
           move      w-prs-dpz-svf-cod
                    (w-prs-dpz-svf-c01)   to   w-dpz-cod-dpz          .
           call      "pgm/azi/prg/obj/pazi000d"
                                         using w-dpz                  .
           go to     prs-dpz-svf-824.
       prs-dpz-svf-830.
      *                  *---------------------------------------------*
      *                  * Fine preparazione                           *
      *                  *---------------------------------------------*
           go to     prs-dpz-svf-900.
       prs-dpz-svf-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     prs-dpz-svf-999.
       prs-dpz-svf-999.
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
           move      "S"                  to   w-cnt-fun-snx-stp      .
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
      *              * [zcp]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofzcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zcp                 .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice catalogo        *
      *              *-------------------------------------------------*
           perform   cod-cod-zcp-opn-000  thru cod-cod-zcp-opn-999    .
       rou-opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Close files per richieste                                 *
      *    *-----------------------------------------------------------*
       rou-cls-fls-000.
      *              *-------------------------------------------------*
      *              * [zcp]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofzcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zcp                 .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice catalogo       *
      *              *-------------------------------------------------*
           perform   cod-cod-zcp-cls-000  thru cod-cod-zcp-cls-999    .
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
      *                  * Codice catalogo                             *
      *                  *---------------------------------------------*
           perform   acc-cod-cpv-000      thru acc-cod-cpv-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
       acc-ric-sel-200.
      *                  *---------------------------------------------*
      *                  * Tipo di dettaglio di stampa                 *
      *                  *---------------------------------------------*
           perform   acc-tip-dts-000      thru acc-tip-dts-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-100.
       acc-ric-sel-250.
      *                  *---------------------------------------------*
      *                  * Tipo di ordinamento per catalogo prodotti   *
      *                  *---------------------------------------------*
           perform   acc-tor-csp-000      thru acc-tor-csp-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-200.
       acc-ric-sel-300.
      *                  *---------------------------------------------*
      *                  * Numero periodi di riferimento               *
      *                  *---------------------------------------------*
           perform   acc-num-pdr-000      thru acc-num-pdr-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-250.
       acc-ric-sel-350.
      *                  *---------------------------------------------*
      *                  * 1. periodo di riferimento, data minima      *
      *                  *---------------------------------------------*
           perform   acc-p1d-min-000      thru acc-p1d-min-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-300.
       acc-ric-sel-400.
      *                  *---------------------------------------------*
      *                  * 1. periodo di riferimento, data massima     *
      *                  *---------------------------------------------*
           perform   acc-p1d-max-000      thru acc-p1d-max-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-350.
       acc-ric-sel-450.
      *                  *---------------------------------------------*
      *                  * 2. periodo di riferimento, data minima      *
      *                  *---------------------------------------------*
           perform   acc-p2d-min-000      thru acc-p2d-min-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-400.
       acc-ric-sel-500.
      *                  *---------------------------------------------*
      *                  * 2. periodo di riferimento, data massima     *
      *                  *---------------------------------------------*
           perform   acc-p2d-max-000      thru acc-p2d-max-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-450.
       acc-ric-sel-550.
      *                  *---------------------------------------------*
      *                  * 3. periodo di riferimento, data minima      *
      *                  *---------------------------------------------*
           perform   acc-p3d-min-000      thru acc-p3d-min-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-500.
       acc-ric-sel-600.
      *                  *---------------------------------------------*
      *                  * 3. periodo di riferimento, data massima     *
      *                  *---------------------------------------------*
           perform   acc-p3d-max-000      thru acc-p3d-max-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-550.
       acc-ric-sel-650.
      *                  *---------------------------------------------*
      *                  * Tipo calcolo % di variazione                *
      *                  *---------------------------------------------*
           perform   acc-tpc-pdv-000      thru acc-tpc-pdv-999        .
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
      *              * Codice catalogo                                 *
      *              *-------------------------------------------------*
           perform   pmt-cod-cpv-000      thru pmt-cod-cpv-999        .
      *              *-------------------------------------------------*
      *              * Tipo di dettaglio di stampa                     *
      *              *-------------------------------------------------*
           perform   pmt-tip-dts-000      thru pmt-tip-dts-999        .
      *              *-------------------------------------------------*
      *              * Tipo di ordinamento per catalogo prodotti       *
      *              *-------------------------------------------------*
           perform   pmt-tor-csp-000      thru pmt-tor-csp-999        .
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
      *              *-------------------------------------------------*
      *              * Tipo calcolo % di variazione                    *
      *              *-------------------------------------------------*
           perform   pmt-tpc-pdv-000      thru pmt-tpc-pdv-999        .
       pmt-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Codice catalogo                                  *
      *    *-----------------------------------------------------------*
       pmt-cod-cpv-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice catalogo            :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cod-cpv-999.
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
      *    * Prompt : Tipo di ordinamento per catalogo prodotti        *
      *    *-----------------------------------------------------------*
       pmt-tor-csp-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tipo di ordinamento per il :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "         catalogo prodotti  "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-tor-csp-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Numero periodi di riferimento                    *
      *    *-----------------------------------------------------------*
       pmt-num-pdr-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Numero periodi di riferi-  :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "                   mento    "
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
           move      17                   to   v-lin                  .
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
           move      17                   to   v-lin                  .
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
           move      18                   to   v-lin                  .
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
           move      18                   to   v-lin                  .
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
           if        w-prs-svf-3pr-snx    not  = "S" and
                     w-prs-svf-3pr-snx    not  = "F"
                     go to pmt-p3d-min-999.
      *              *-------------------------------------------------*
      *              * Visualizzazione prompt                          *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      19                   to   v-lin                  .
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
           if        w-prs-svf-3pr-snx    not  = "S" and
                     w-prs-svf-3pr-snx    not  = "F"
                     go to pmt-p3d-max-999.
      *              *-------------------------------------------------*
      *              * Visualizzazione prompt                          *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      40                   to   v-pos                  .
           move      "al :"               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-p3d-max-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Tipo calcolo % di variazione                     *
      *    *-----------------------------------------------------------*
       pmt-tpc-pdv-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      21                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Calcolo % di variazione    :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-tpc-pdv-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Codice catalogo                      *
      *    *-----------------------------------------------------------*
       acc-cod-cpv-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cod-cpv-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-cod-zcp-ope      .
           move      rr-cod-cpv           to   w-cod-cod-zcp-cod      .
           move      06                   to   w-cod-cod-zcp-lin      .
           move      30                   to   w-cod-cod-zcp-pos      .
           move      spaces               to   v-edm                  .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-cod-zcp-cll-000  thru cod-cod-zcp-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-cod-zcp-foi-000  thru cod-cod-zcp-foi-999    .
       acc-cod-cpv-110.
           perform   cod-cod-zcp-cll-000  thru cod-cod-zcp-cll-999    .
           if        w-cod-cod-zcp-ope    =    "F+"
                     go to acc-cod-cpv-115.
           if        w-cod-cod-zcp-ope    =    "AC"
                     go to acc-cod-cpv-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-cpv-115.
           perform   cod-cod-zcp-foi-000  thru cod-cod-zcp-foi-999    .
           go to     acc-cod-cpv-110.
       acc-cod-cpv-120.
           move      w-cod-cod-zcp-cod    to   v-alf                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-cod-cpv-999.
       acc-cod-cpv-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   rr-cod-cpv             .
       acc-cod-cpv-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura file [zcp]                          *
      *                  *---------------------------------------------*
           move      rr-cod-cpv           to   w-let-arc-zcp-cod      .
           perform   let-arc-zcp-000      thru let-arc-zcp-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione descrizione                  *
      *                  *---------------------------------------------*
           if        rr-cod-cpv           =    spaces
                     move  "Tutti i codici catalogo"
                                          to   rr-cod-cpv-des
           else      move  w-let-arc-zcp-des
                                          to   rr-cod-cpv-des         .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione                 *
      *                  *---------------------------------------------*
           perform   vis-cod-cpv-des-000  thru vis-cod-cpv-des-999    .
      *                  *---------------------------------------------*
      *                  * Test su esito lettura                       *
      *                  *---------------------------------------------*
           if        w-let-arc-zcp-flg    not  = spaces
                     go to acc-cod-cpv-100.
       acc-cod-cpv-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se impostato un codice diverso da spaces si *
      *                  * normalizza il tipo ordinamento              *
      *                  *---------------------------------------------*
           if        rr-cod-cpv           =    spaces
                     go to acc-cod-cpv-800.
           move      zero                 to   rr-tor-csp             .
           perform   vis-tor-csp-000      thru vis-tor-csp-999        .
       acc-cod-cpv-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-cod-cpv-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-cod-cpv-100.
       acc-cod-cpv-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Codice catalogo                   *
      *    *-----------------------------------------------------------*
       vis-cod-cpv-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-cod-cpv           to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-cpv-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Codice catalogo, descrizione      *
      *    *-----------------------------------------------------------*
       vis-cod-cpv-des-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      rr-cod-cpv-des       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-cpv-des-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Tipo di dettaglio in stampa                *
      *    *-----------------------------------------------------------*
       acc-tip-dts-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-tip-dts-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-dts-lun    to   v-car                  .
           move      w-exp-tip-dts-num    to   v-ldt                  .
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
      *                  * Se la scelta implica il dettaglio dei do-   *
      *                  * cumenti si normalizzano e si visualizzano : *
      *                  *  - Numero periodi di riferimento            *
      *                  *  - 2. periodo, data minima                  *
      *                  *  - 2. periodo, data massima                 *
      *                  *  - 3. periodo, data minima                  *
      *                  *  - 3. periodo, data massima                 *
      *                  *  - Tipo calcolo % di variazione             *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test                                    *
      *                      *-----------------------------------------*
           if        rr-tip-dts           not  = 02
                     go to acc-tip-dts-650.
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
      *                      *-----------------------------------------*
      *                      * Normalizzazione % di variazione         *
      *                      *-----------------------------------------*
           move      zero                 to   rr-tpc-pdv             .
           perform   vis-tpc-pdv-000      thru vis-tpc-pdv-999        .
       acc-tip-dts-650.
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
      *    * Accettazione : Tipo ordinamento per catalogo prodotti     *
      *    *-----------------------------------------------------------*
       acc-tor-csp-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-tor-csp-025.
      *                  *---------------------------------------------*
      *                  * Se impostato un codice classificazione sta- *
      *                  * tistica prodotto diverso da spaces : nes-   *
      *                  * suna accettazione                           *
      *                  *---------------------------------------------*
           if        rr-cod-cpv           =    spaces
                     go to acc-tor-csp-100
           else      go to acc-tor-csp-999.
       acc-tor-csp-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tor-csp-lun    to   v-car                  .
           move      w-exp-tor-csp-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      11                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-tor-csp-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-tor-csp           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-tor-csp-999.
       acc-tor-csp-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-tor-csp             .
       acc-tor-csp-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-tor-csp-425.
      *                  *---------------------------------------------*
      *                  * Che sia un valore consentito                *
      *                  *---------------------------------------------*
           if        rr-tor-csp           =    zero           or
                     rr-tor-csp           >    w-exp-tor-csp-num
                     go to acc-tor-csp-100.
       acc-tor-csp-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-tor-csp-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-tor-csp-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-tor-csp-100.
       acc-tor-csp-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Tipo ordinamento per catalogo prodotti  *
      *    *-----------------------------------------------------------*
       vis-tor-csp-000.
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tor-csp-lun    to   v-car                  .
           move      w-exp-tor-csp-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      11                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-tor-csp-tbl    to   v-txt                  .
           move      rr-tor-csp           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tor-csp-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Numero periodi di riferimento              *
      *    *-----------------------------------------------------------*
       acc-num-pdr-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
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
           if        rr-tip-dts           not  = 02
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
      *                          * % di variazione                     *
      *                          *-------------------------------------*
           move      zero                 to   rr-tpc-pdv             .
           perform   vis-tpc-pdv-000      thru vis-tpc-pdv-999        .
      *                          *-------------------------------------*
      *                          * Uscita                              *
      *                          *-------------------------------------*
           go to     acc-num-pdr-999.
       acc-num-pdr-100.
      *              *-------------------------------------------------*
      *              * Preparazione work per accettazione              *
      *              *-------------------------------------------------*
           if        w-prs-svf-3pr-snx    =    "S" or
                     w-prs-svf-3pr-snx    =    "F"
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
           move      spaces               to   v-edm                  .
           move      14                   to   v-lin                  .
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
      *                  * Normalizzazione tipo calcolo % di variazio- *
      *                  * ne se necessario                            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test                                    *
      *                      *-----------------------------------------*
           if        rr-num-pdr           =    02 or
                     rr-num-pdr           =    03
                     go to acc-num-pdr-700.
      *                      *-----------------------------------------*
      *                      * Normalizzazione % di variazione         *
      *                      *-----------------------------------------*
           move      zero                 to   rr-tpc-pdv             .
           perform   vis-tpc-pdv-000      thru vis-tpc-pdv-999        .
       acc-num-pdr-700.
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
           if        w-prs-svf-3pr-snx    =    "S" or
                     w-prs-svf-3pr-snx    =    "F"
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
           move      14                   to   v-lin                  .
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
           move      17                   to   v-lin                  .
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
           move      17                   to   v-lin                  .
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
           move      17                   to   v-lin                  .
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
           move      17                   to   v-lin                  .
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
           move      18                   to   v-lin                  .
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
           move      18                   to   v-lin                  .
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
           move      18                   to   v-lin                  .
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
           move      18                   to   v-lin                  .
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
           if        w-prs-svf-3pr-snx    not  = "S" and
                     w-prs-svf-3pr-snx    not  = "F"
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
           move      19                   to   v-lin                  .
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
           if        w-prs-svf-3pr-snx    not  = "S" and
                     w-prs-svf-3pr-snx    not  = "F"
                     go to vis-p3d-min-999.
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      19                   to   v-lin                  .
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
           if        w-prs-svf-3pr-snx    not  = "S" and
                     w-prs-svf-3pr-snx    not  = "F"
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
           move      19                   to   v-lin                  .
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
           if        w-prs-svf-3pr-snx    not  = "S" and
                     w-prs-svf-3pr-snx    not  = "F"
                     go to vis-p3d-max-999.
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      19                   to   v-lin                  .
           move      45                   to   v-pos                  .
           move      rr-p3d-max           to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-p3d-max-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Tipo calcolo % di variazione               *
      *    *-----------------------------------------------------------*
       acc-tpc-pdv-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-tpc-pdv-025.
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se il numero periodi di riferimento lo  *
      *                      * consente, si accetta il campo, altri-   *
      *                      * menti si esce                           *
      *                      *-----------------------------------------*
           if        rr-num-pdr           =    02 or
                     rr-num-pdr           =    03
                     go to acc-tpc-pdv-100
           else      go to acc-tpc-pdv-999.
       acc-tpc-pdv-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tpc-pdv-lun    to   v-car                  .
           move      w-exp-tpc-pdv-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      21                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-tpc-pdv-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-tpc-pdv           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-tpc-pdv-999.
       acc-tpc-pdv-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-tpc-pdv             .
       acc-tpc-pdv-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-tpc-pdv-425.
      *                  *---------------------------------------------*
      *                  * Che sia un valore consentito                *
      *                  *---------------------------------------------*
           if        rr-tpc-pdv           =    zero           or
                     rr-tpc-pdv           >    w-exp-tpc-pdv-num
                     go to acc-tpc-pdv-100.
       acc-tpc-pdv-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-tpc-pdv-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-tpc-pdv-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-tpc-pdv-100.
       acc-tpc-pdv-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Tipo calcolo % di variazione            *
      *    *-----------------------------------------------------------*
       vis-tpc-pdv-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tpc-pdv-lun    to   v-car                  .
           move      w-exp-tpc-pdv-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      21                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-tpc-pdv-tbl    to   v-txt                  .
           move      rr-tpc-pdv           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tpc-pdv-999.
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
       tdo-ric-sel-100.
      *              *-------------------------------------------------*
      *              * Controllo : Tipo di dettaglio di stampa         *
      *              *-------------------------------------------------*
       tdo-ric-sel-101.
           if        rr-tip-dts           not  = zero
                     go to tdo-ric-sel-102.
           move      "Manca il tipo di dettaglio di stampa !            
      -              "               "    to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-102.
           if        rr-tip-dts           =    01 or
                     rr-tip-dts           =    02
                     go to tdo-ric-sel-150.
           move      "Tipo di dettaglio di stampa errato !              
      -              "               "    to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-150.
      *              *-------------------------------------------------*
      *              * Controllo : Tipo di ordinamento per il catalogo *
      *              *             prodotti                            *
      *              *-------------------------------------------------*
       tdo-ric-sel-151.
           if        rr-cod-cpv           not  = spaces
                     go to tdo-ric-sel-300.
           if        rr-tor-csp           not  = zero
                     go to tdo-ric-sel-152.
           move      "Manca il tipo di ordinamento per la stampa !      
      -              "               "    to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-152.
           if        rr-tor-csp           =    01 or
                     rr-tor-csp           =    02 or
                     rr-tor-csp           =    03 or
                     rr-tor-csp           =    04 or
                     rr-tor-csp           =    05 or
                     rr-tor-csp           =    06 or
                     rr-tor-csp           =    07
                     go to tdo-ric-sel-300.
           move      "Tipo di ordinamento errato !                      
      -              "               "    to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-300.
      *              *-------------------------------------------------*
      *              * Controllo : Numero periodi di riferimento       *
      *              *-------------------------------------------------*
       tdo-ric-sel-301.
           if        w-prs-svf-3pr-snx    =    "S" or
                     w-prs-svf-3pr-snx    =    "F"
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
           if        rr-tip-dts           =    02
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
      *              * Controllo : tipo calcolo % di variazione        *
      *              *-------------------------------------------------*
       tdo-ric-sel-751.
           if        rr-num-pdr           not  = 02 and
                     rr-num-pdr           not  = 03
                     go to tdo-ric-sel-800.
       tdo-ric-sel-752.
           if        rr-tpc-pdv           not  = zero
                     go to tdo-ric-sel-753.
           move      "Manca il tipo di calcolo per la % di variazione ! 
      -              "               "    to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-753.
           if        rr-tpc-pdv           =    01 or
                     rr-tpc-pdv           =    02
                     go to tdo-ric-sel-800.
           move      "Tipo di calcolo per la % di variazione errato !   
      -              "               "    to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-800.
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
       reg-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione richieste di selezione                    *
      *    *-----------------------------------------------------------*
       nor-ric-sel-000.
      *              *-------------------------------------------------*
      *              * Precaricamento tabella dipendenze selezionate   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Contatore globale dipendenze letto in pre-  *
      *                  * esecuzione del programma                    *
      *                  *---------------------------------------------*
           move      w-dpz-ctr-dpz        to   rr-dpz-ctr-dpz         .
      *                  *---------------------------------------------*
      *                  * Test se selezionata una sola dipendenza     *
      *                  *---------------------------------------------*
           if        rr-dpz-inu           not  = zero
                     go to nor-ric-sel-100.
      *                  *---------------------------------------------*
      *                  * Preparazione contatori                      *
      *                  *---------------------------------------------*
           move      zero                 to   rr-dpz-ctr-sel         .
           move      zero                 to   w-nor-ric-sel-c01      .
       nor-ric-sel-010.
           add       1                    to   w-nor-ric-sel-c01      .
           if        w-nor-ric-sel-c01    >    99
                     go to nor-ric-sel-100.
           if        w-dpz-ele-flg
                    (w-nor-ric-sel-c01)   =    spaces
                     go to nor-ric-sel-010.
           add       1                    to   rr-dpz-ctr-sel         .
           if        rr-dpz-ctr-sel       >    99
                     go to nor-ric-sel-100.
           move      w-nor-ric-sel-c01    to   rr-dpz-ele-cod
                                              (rr-dpz-ctr-sel)        .
           move      w-dpz-ele-den
                    (w-nor-ric-sel-c01)   to   rr-dpz-ele-den
                                              (rr-dpz-ctr-sel)        .
      *                  *---------------------------------------------*
      *                  * Riciclo su elemento successivo              *
      *                  *---------------------------------------------*
           go to     nor-ric-sel-010.
       nor-ric-sel-100.
      *              *-------------------------------------------------*
      *              * Normalizzazioni altri campi                     *
      *              *-------------------------------------------------*
           move      spaces               to   rr-cod-cpv             .
           move      spaces               to   rr-cod-cpv-des         .
           move      zero                 to   rr-tip-dts             .
           move      zero                 to   rr-tor-csp             .
           move      zero                 to   rr-num-pdr             .
           move      zero                 to   rr-p1d-min             .
           move      zero                 to   rr-p1d-max             .
           move      zero                 to   rr-p2d-min             .
           move      zero                 to   rr-p2d-max             .
           move      zero                 to   rr-p3d-min             .
           move      zero                 to   rr-p3d-max             .
           move      zero                 to   rr-tpc-pdv             .
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
           if        rr-num-pdr           =    03
                     move  198            to   w-cnt-stp-amp-lin
           else      move  132            to   w-cnt-stp-amp-lin      .
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
      *    * Routine di lettura archivio [zcp]                         *
      *    *-----------------------------------------------------------*
       let-arc-zcp-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zcp-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice a spaces                         *
      *              *-------------------------------------------------*
           if        w-let-arc-zcp-cod    =    spaces
                     go to let-arc-zcp-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCPV    "         to   f-key                  .
           move      w-let-arc-zcp-cod    to   rf-zcp-cod-cpv         .
           move      "pgm/dcp/fls/ioc/obj/iofzcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zcp                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-zcp-400.
       let-arc-zcp-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-zcp-des-cpv       to   w-let-arc-zcp-des      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-zcp-999.
       let-arc-zcp-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-zcp-flg      .
           move      all   "."            to   w-let-arc-zcp-des      .
           go to     let-arc-zcp-600.
       let-arc-zcp-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zcp-des      .
       let-arc-zcp-600.
       let-arc-zcp-999.
           exit.

      *    *===========================================================*
      *    * Subroutines per l'accettazione codice catalogo            *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/acodzcp0.acs"                   .

