       Identification Division.
       Program-Id.                                 pbol0210           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    bol                 *
      *                                Settore:    tab                 *
      *                                   Fase:    bol021              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 13/11/91    *
      *                       Ultima revisione:    NdK del 24/11/04    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Interrogazione tabella causali del traspor- *
      *                    to                                          *
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
                     "bol"                                            .
      *        *-------------------------------------------------------*
      *        * Settore gestionale                                    *
      *        *-------------------------------------------------------*
           05  i-ide-set                  pic  x(03) value
                     "tab"                                            .
      *        *-------------------------------------------------------*
      *        * Fase gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-fas                  pic  x(06) value
                     "bol021"                                         .
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10) value
                     "pbol0210"                                       .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  i-ide-des                  pic  x(40) value
                     "INTERROGAZIONE TABELLA CAUSALI TRASPORTO"       .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "mvideo" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/v"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "msegrt"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/s"                                  .

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
      *            * Per routine qry-rou-pri-000                       *
      *            *---------------------------------------------------*
               10  w-cnt-qry-rou-pri      pic  x(01)                  .
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
      *            * Si/No richieste pre esecuzione interrogazione     *
      *            *---------------------------------------------------*
               10  w-cnt-fun-snx-ric      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Si/No funzionamento ciclico interrogazione        *
      *            *---------------------------------------------------*
               10  w-cnt-fun-snx-cic      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Si/No funzionamento automatico interrogazione     *
      *            *---------------------------------------------------*
               10  w-cnt-fun-snx-aut      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Flag di Si/No primo giro di esecuzione            *
      *            *---------------------------------------------------*
               10  w-cnt-fun-prm-gir      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Area di controllo per funzionamento qry-routine       *
      *        *-------------------------------------------------------*
           05  w-cnt-qry.
      *            *---------------------------------------------------*
      *            * Flag di primo giro                                *
      *            *---------------------------------------------------*
               10  w-cnt-qry-mrk-uno      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Flag di interruzione forzata                      *
      *            *---------------------------------------------------*
               10  w-cnt-qry-flg-int      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Flag di uscita da subroutines principali          *
      *            *---------------------------------------------------*
               10  w-cnt-qry-flg-sub      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Area per salvataggio parametri rottura livello    *
      *            *---------------------------------------------------*
               10  w-cnt-qry-sav-liv.
                   15  w-cnt-qry-sav-l05  pic  x(64)                  .
                   15  w-cnt-qry-sav-l04  pic  x(64)                  .
                   15  w-cnt-qry-sav-l03  pic  x(64)                  .
                   15  w-cnt-qry-sav-l02  pic  x(64)                  .
                   15  w-cnt-qry-sav-l01  pic  x(64)                  .
      *            *---------------------------------------------------*
      *            * Area per salvataggio area di rottura              *
      *            *---------------------------------------------------*
               10  w-cnt-qry-sav-rot.
                   15  filler occurs 320  pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Literals                                              *
      *        *-------------------------------------------------------*
           05  w-cnt-lit.
               10  w-cnt-lit-t80          pic  x(80) value all "="    .

      *    *===========================================================*
      *    * Record files                                              *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [zct]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/bol/fls/rec/rfzct"                          .

      *    *===========================================================*
      *    * Work-area per valori di i.p.c.                            *
      *    *-----------------------------------------------------------*
       01  w-ipc.
      *        *-------------------------------------------------------*
      *        * Variabili di i.p.c. da livello precedente             *
      *        *-------------------------------------------------------*
           05  w-ipc-dlp.
      *            *---------------------------------------------------*
      *            * Per tipo interrogazione precablata                *
      *            *---------------------------------------------------*
               10  w-ipc-dlp-tdi.
      *                *-----------------------------------------------*
      *                * Tipo interrogazione                           *
      *                *-----------------------------------------------*
                   15  w-ipc-dlp-tdi-tip  pic  x(01)                  .
      *                *-----------------------------------------------*
      *                * Descrizione                                   *
      *                *-----------------------------------------------*
                   15  w-ipc-dlp-tdi-des  pic  x(20)                  .

      *    *===========================================================*
      *    * Work-area richieste per interrogazione                    *
      *    *-----------------------------------------------------------*
       01  rr.
           05  rr-tip-ord                 pic  x(01)                  .
           05  rr-cod-min                 pic  x(03)                  .
           05  rr-cod-max                 pic  x(03)                  .
           05  rr-des-min                 pic  x(20)                  .
           05  rr-des-max                 pic  x(20)                  .

      *    *===========================================================*
      *    * Link-area per accettazione codice causale                 *
      *    *-----------------------------------------------------------*
           copy      "pgm/bol/prg/cpy/acdezct0.acl"                   .

      *    *===========================================================*
      *    * Work per accettazioni campi espansi                       *
      *    *-----------------------------------------------------------*
       01  w-exp.
      *        *-------------------------------------------------------*
      *        * Work per : Tipo ordinamento                           *
      *        *-------------------------------------------------------*
           05  w-exp-tip-ord.
               10  w-exp-tip-ord-num      pic  9(02)       value 2    .
               10  w-exp-tip-ord-lun      pic  9(02)       value 20   .
               10  w-exp-tip-ord-tbl.
                   15  filler             pic  x(20) value
                            "Per descrizione     "                    .
                   15  filler             pic  x(20) value
                            "Per codice          "                    .

      *    *===========================================================*
      *    * Work-area per ridefinizione contenuto del mark-point      *
      *    *-----------------------------------------------------------*
       01  w-mpn.
           05  w-mpn-cod-ctr              pic  x(03)                  .

      *    *===========================================================*
      *    * Work area per controllo rotture di livello                *
      *    *-----------------------------------------------------------*
       01  w-rot.
      *        *-------------------------------------------------------*
      *        * 5. livello di rottura                                 *
      *        *-------------------------------------------------------*
           05  w-rot-l05.
               10  filler                 pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * 4. livello di rottura                                 *
      *        *-------------------------------------------------------*
           05  w-rot-l04.
               10  filler                 pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * 3. livello di rottura                                 *
      *        *-------------------------------------------------------*
           05  w-rot-l03.
               10  filler                 pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * 2. livello di rottura                                 *
      *        *-------------------------------------------------------*
           05  w-rot-l02.
               10  filler                 pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * 1. livello di rottura                                 *
      *        *-------------------------------------------------------*
           05  w-rot-l01.
               10  filler                 pic  x(01)                  .

      *    *===========================================================*
      *    * Work-area per allineamenti a destra o a sinistra oppure   *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cpw"                   .

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
           move      spaces               to   w-cnt-dic-ini-pgm      .
           perform   dic-ini-pgm-000      thru dic-ini-pgm-999        .
           if        w-cnt-dic-ini-pgm    not  = spaces
                     go to main-999.
      *              *-------------------------------------------------*
      *              * Esecuzione routine pre-esecuzione programma     *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-pre-exe-pgm      .
           perform   pre-exe-pgm-000      thru pre-exe-pgm-999        .
           if        w-cnt-pre-exe-pgm    not  = spaces
                     go to main-900.
      *              *-------------------------------------------------*
      *              * Preparazione tipo funzionamento                 *
      *              *-------------------------------------------------*
           perform   pre-tip-fun-000      thru pre-tip-fun-999        .
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-rou-opn-fls      .
           perform   rou-opn-fls-000      thru rou-opn-fls-999        .
           if        w-cnt-rou-opn-fls    not  = spaces
                     go to main-900.
      *              *-------------------------------------------------*
      *              * Segnale primo giro di esecuzione                *
      *              *-------------------------------------------------*
           move      "S"                  to   w-cnt-fun-prm-gir      .
      *              *-------------------------------------------------*
      *              * Se no richieste : a esecuzione interrogazione   *
      *              *-------------------------------------------------*
           if        w-cnt-fun-snx-ric    not  = "S"
                     go to main-500.
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
       main-500.
      *              *-------------------------------------------------*
      *              * Esecuzione programma di interrogazione          *
      *              *-------------------------------------------------*
           perform   qry-rou-pri-000      thru qry-rou-pri-999        .
      *              *-------------------------------------------------*
      *              * Se no richieste : a fine programma              *
      *              *-------------------------------------------------*
           if        w-cnt-fun-snx-ric    not  = "S"
                     go to main-750.
      *              *-------------------------------------------------*
      *              * Test se fine esecuzione da operatore            *
      *              *-------------------------------------------------*
           if        w-cnt-qry-rou-pri    not  = spaces
                     go to main-750.
      *              *-------------------------------------------------*
      *              * Segnale non piu' primo giro di esecuzione       *
      *              *-------------------------------------------------*
           move      "N"                  to   w-cnt-fun-prm-gir      .
      *              *-------------------------------------------------*
      *              * Test se tipo esecuzione ciclico                 *
      *              *-------------------------------------------------*
           if        w-cnt-fun-snx-ric    not  = "S" or
                     w-cnt-fun-snx-cic    not  = "S"
                     go to main-750
           else      go to main-250.
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
      *    * Interrogazione : Routine principale                       *
      *    *-----------------------------------------------------------*
       qry-rou-pri-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-qry-rou-pri      .
      *              *-------------------------------------------------*
      *              * Determinazione function-keys in Mark-points     *
      *              *-------------------------------------------------*
           perform   qry-det-fky-000      thru qry-det-fky-999        .
      *              *-------------------------------------------------*
      *              * Begin o Begin Automatico                        *
      *              *-------------------------------------------------*
           if        w-cnt-fun-snx-aut    =    "S"
                     move   "BA"          to   v-ope
           else      move   "BE"          to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
           perform   qry-opn-fls-000      thru qry-opn-fls-999        .
      *              *-------------------------------------------------*
      *              * Inizializzazione marker di primo giro           *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-qry-mrk-uno      .
      *              *-------------------------------------------------*
      *              * Inizializzazione flag di interruzione forzata   *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-qry-flg-int      .
      *              *-------------------------------------------------*
      *              * Inizializzazione area per rotture di livello    *
      *              *-------------------------------------------------*
           move      spaces               to   w-rot                  .
      *              *-------------------------------------------------*
      *              * Start iniziale                                  *
      *              *-------------------------------------------------*
           perform   qry-str-ini-000      thru qry-str-ini-999        .
           if        w-cnt-qry-flg-sub    not  = spaces
                     move   spaces        to   w-cnt-qry-flg-sub
                     go to  qry-rou-pri-600.
       qry-rou-pri-100.
      *              *-------------------------------------------------*
      *              * Salvataggio area rottura in area precedente     *
      *              *-------------------------------------------------*
           move      w-rot-l05            to   w-cnt-qry-sav-l05      .
           move      w-rot-l04            to   w-cnt-qry-sav-l04      .
           move      w-rot-l03            to   w-cnt-qry-sav-l03      .
           move      w-rot-l02            to   w-cnt-qry-sav-l02      .
           move      w-rot-l01            to   w-cnt-qry-sav-l01      .
       qry-rou-pri-200.
      *              *-------------------------------------------------*
      *              * Lettura sequenziale                             *
      *              *-------------------------------------------------*
           perform   qry-let-seq-000      thru qry-let-seq-999        .
           if        w-cnt-qry-flg-sub    not  = spaces
                     move   spaces        to   w-cnt-qry-flg-sub
                     go to  qry-rou-pri-500.
      *              *-------------------------------------------------*
      *              * Test se superamento limiti massimi              *
      *              *-------------------------------------------------*
           perform   qry-tst-max-000      thru qry-tst-max-999        .
           if        w-cnt-qry-flg-sub    not  = spaces
                     move   spaces        to   w-cnt-qry-flg-sub
                     go to  qry-rou-pri-500.
      *              *-------------------------------------------------*
      *              * Selezione su record letto                       *
      *              *-------------------------------------------------*
           perform   qry-sel-rec-000      thru qry-sel-rec-999        .
           if        w-cnt-qry-flg-sub    not  = spaces
                     move   spaces        to   w-cnt-qry-flg-sub
                     go to  qry-rou-pri-200.
      *              *-------------------------------------------------*
      *              * Composizione area per tests di rottura          *
      *              *-------------------------------------------------*
           perform   qry-cmp-rot-000      thru qry-cmp-rot-999        .
      *              *-------------------------------------------------*
      *              * Test se primo passaggio                         *
      *              *-------------------------------------------------*
           if        w-cnt-qry-mrk-uno    not  = spaces
                     go to qry-rou-pri-300.
      *                  *---------------------------------------------*
      *                  * Inizio di tutti i livelli                   *
      *                  *---------------------------------------------*
           perform   qry-rou-pri-790      thru qry-rou-pri-791        .
           perform   qry-rou-pri-750      thru qry-rou-pri-751        .
           perform   qry-rou-pri-740      thru qry-rou-pri-741        .
           perform   qry-rou-pri-730      thru qry-rou-pri-731        .
           perform   qry-rou-pri-720      thru qry-rou-pri-721        .
           perform   qry-rou-pri-710      thru qry-rou-pri-711        .
           go to     qry-rou-pri-400.
       qry-rou-pri-300.
      *              *-------------------------------------------------*
      *              * Se rottura del 5. livello                       *
      *              *-------------------------------------------------*
           if        w-rot-l05            =    w-cnt-qry-sav-l05
                     go to qry-rou-pri-310.
           move      w-rot                to   w-cnt-qry-sav-rot      .
           move      w-cnt-qry-sav-l05    to   w-rot-l05              .
           move      w-cnt-qry-sav-l04    to   w-rot-l04              .
           move      w-cnt-qry-sav-l03    to   w-rot-l03              .
           move      w-cnt-qry-sav-l02    to   w-rot-l02              .
           move      w-cnt-qry-sav-l01    to   w-rot-l01              .
           perform   qry-rou-pri-810      thru qry-rou-pri-811        .
           perform   qry-rou-pri-820      thru qry-rou-pri-821        .
           perform   qry-rou-pri-830      thru qry-rou-pri-831        .
           perform   qry-rou-pri-840      thru qry-rou-pri-841        .
           perform   qry-rou-pri-850      thru qry-rou-pri-851        .
           move      w-cnt-qry-sav-rot    to   w-rot                  .
           perform   qry-rou-pri-750      thru qry-rou-pri-751        .
           perform   qry-rou-pri-740      thru qry-rou-pri-741        .
           perform   qry-rou-pri-730      thru qry-rou-pri-731        .
           perform   qry-rou-pri-720      thru qry-rou-pri-721        .
           perform   qry-rou-pri-710      thru qry-rou-pri-711        .
           go to     qry-rou-pri-400.
       qry-rou-pri-310.
      *              *-------------------------------------------------*
      *              * Se rottura del 4. livello                       *
      *              *-------------------------------------------------*
           if        w-rot-l04            =    w-cnt-qry-sav-l04
                     go to qry-rou-pri-320.
           move      w-rot                to   w-cnt-qry-sav-rot      .
           move      w-cnt-qry-sav-l04    to   w-rot-l04              .
           move      w-cnt-qry-sav-l03    to   w-rot-l03              .
           move      w-cnt-qry-sav-l02    to   w-rot-l02              .
           move      w-cnt-qry-sav-l01    to   w-rot-l01              .
           perform   qry-rou-pri-810      thru qry-rou-pri-811        .
           perform   qry-rou-pri-820      thru qry-rou-pri-821        .
           perform   qry-rou-pri-830      thru qry-rou-pri-831        .
           perform   qry-rou-pri-840      thru qry-rou-pri-841        .
           move      w-cnt-qry-sav-rot    to   w-rot                  .
           perform   qry-rou-pri-740      thru qry-rou-pri-741        .
           perform   qry-rou-pri-730      thru qry-rou-pri-731        .
           perform   qry-rou-pri-720      thru qry-rou-pri-721        .
           perform   qry-rou-pri-710      thru qry-rou-pri-711        .
           go to     qry-rou-pri-400.
       qry-rou-pri-320.
      *              *-------------------------------------------------*
      *              * Se rottura del 3. livello                       *
      *              *-------------------------------------------------*
           if        w-rot-l03            =    w-cnt-qry-sav-l03
                     go to qry-rou-pri-330.
           move      w-rot                to   w-cnt-qry-sav-rot      .
           move      w-cnt-qry-sav-l03    to   w-rot-l03              .
           move      w-cnt-qry-sav-l02    to   w-rot-l02              .
           move      w-cnt-qry-sav-l01    to   w-rot-l01              .
           perform   qry-rou-pri-810      thru qry-rou-pri-811        .
           perform   qry-rou-pri-820      thru qry-rou-pri-821        .
           perform   qry-rou-pri-830      thru qry-rou-pri-831        .
           move      w-cnt-qry-sav-rot    to   w-rot                  .
           perform   qry-rou-pri-730      thru qry-rou-pri-731        .
           perform   qry-rou-pri-720      thru qry-rou-pri-721        .
           perform   qry-rou-pri-710      thru qry-rou-pri-711        .
           go to     qry-rou-pri-400.
       qry-rou-pri-330.
      *              *-------------------------------------------------*
      *              * Se rottura del 2. livello                       *
      *              *-------------------------------------------------*
           if        w-rot-l02            =    w-cnt-qry-sav-l02
                     go to qry-rou-pri-340.
           move      w-rot                to   w-cnt-qry-sav-rot      .
           move      w-cnt-qry-sav-l02    to   w-rot-l02              .
           move      w-cnt-qry-sav-l01    to   w-rot-l01              .
           perform   qry-rou-pri-810      thru qry-rou-pri-811        .
           perform   qry-rou-pri-820      thru qry-rou-pri-821        .
           move      w-cnt-qry-sav-rot    to   w-rot                  .
           perform   qry-rou-pri-720      thru qry-rou-pri-721        .
           perform   qry-rou-pri-710      thru qry-rou-pri-711        .
           go to     qry-rou-pri-400.
       qry-rou-pri-340.
      *              *-------------------------------------------------*
      *              * Se rottura del 1. livello                       *
      *              *-------------------------------------------------*
           if        w-rot-l01            =    w-cnt-qry-sav-l01
                     go to qry-rou-pri-400.
           move      w-rot                to   w-cnt-qry-sav-rot      .
           move      w-cnt-qry-sav-l01    to   w-rot-l01              .
           perform   qry-rou-pri-810      thru qry-rou-pri-811        .
           move      w-cnt-qry-sav-rot    to   w-rot                  .
           perform   qry-rou-pri-710      thru qry-rou-pri-711        .
       qry-rou-pri-400.
      *              *-------------------------------------------------*
      *              * Se segnale di interruzione attivo : fine ciclo  *
      *              *-------------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to qry-rou-pri-900.
      *              *-------------------------------------------------*
      *              * Livello di dettaglio                            *
      *              *-------------------------------------------------*
           perform   qry-liv-det-000      thru qry-liv-det-999        .
      *              *-------------------------------------------------*
      *              * Se segnale di interruzione : fine ciclo         *
      *              *-------------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to qry-rou-pri-900.
      *              *-------------------------------------------------*
      *              * Segnale di passaggio successivo al primo        *
      *              *-------------------------------------------------*
           move      "#"                  to   w-cnt-qry-mrk-uno      .
      *              *-------------------------------------------------*
      *              * Riciclo a lettura sequenziale file principale   *
      *              *-------------------------------------------------*
           go to     qry-rou-pri-100.
       qry-rou-pri-500.
      *              *-------------------------------------------------*
      *              * Test se almeno un passaggio                     *
      *              *-------------------------------------------------*
           if        w-cnt-qry-mrk-uno    =    spaces
                     go to qry-rou-pri-600.
      *              *-------------------------------------------------*
      *              * Fine di tutti i livelli                         *
      *              *-------------------------------------------------*
           perform   qry-rou-pri-810      thru qry-rou-pri-811        .
           perform   qry-rou-pri-820      thru qry-rou-pri-821        .
           perform   qry-rou-pri-830      thru qry-rou-pri-831        .
           perform   qry-rou-pri-840      thru qry-rou-pri-841        .
           perform   qry-rou-pri-850      thru qry-rou-pri-851        .
           perform   qry-rou-pri-890      thru qry-rou-pri-891        .
           if        w-cnt-qry-flg-int    not  = spaces
                     go to qry-rou-pri-900.
      *              *-------------------------------------------------*
      *              * Stop                                            *
      *              *-------------------------------------------------*
           move      "ST"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Subroutine di avanzamento pagina                *
      *              *-------------------------------------------------*
           perform   qry-pag-adv-000      thru qry-pag-adv-999        .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     qry-rou-pri-900.
       qry-rou-pri-600.
      *              *-------------------------------------------------*
      *              * Esecuzione per nessuna registrazione da elab.   *
      *              *-------------------------------------------------*
           perform   qry-nes-ela-000      thru qry-nes-ela-999        .
           go to     qry-rou-pri-900.
       qry-rou-pri-710.
      *              *-------------------------------------------------*
      *              * Esecuzione per inizio 1. livello di rottura     *
      *              *-------------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to qry-rou-pri-711.
           perform   qry-ini-lr1-000      thru qry-ini-lr1-999        .
       qry-rou-pri-711.
           exit.
       qry-rou-pri-720.
      *              *-------------------------------------------------*
      *              * Esecuzione per inizio 2. livello di rottura     *
      *              *-------------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to qry-rou-pri-721.
           perform   qry-ini-lr2-000      thru qry-ini-lr2-999        .
       qry-rou-pri-721.
           exit.
       qry-rou-pri-730.
      *              *-------------------------------------------------*
      *              * Esecuzione per inizio 3. livello di rottura     *
      *              *-------------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to qry-rou-pri-731.
           perform   qry-ini-lr3-000      thru qry-ini-lr3-999        .
       qry-rou-pri-731.
           exit.
       qry-rou-pri-740.
      *              *-------------------------------------------------*
      *              * Esecuzione per inizio 4. livello di rottura     *
      *              *-------------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to qry-rou-pri-741.
           perform   qry-ini-lr4-000      thru qry-ini-lr4-999        .
       qry-rou-pri-741.
           exit.
       qry-rou-pri-750.
      *              *-------------------------------------------------*
      *              * Esecuzione per inizio 5. livello di rottura     *
      *              *-------------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to qry-rou-pri-751.
           perform   qry-ini-lr5-000      thru qry-ini-lr5-999        .
       qry-rou-pri-751.
           exit.
       qry-rou-pri-790.
      *              *-------------------------------------------------*
      *              * Esecuzione per inizio ciclo                     *
      *              *-------------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to qry-rou-pri-791.
           perform   qry-ini-cic-000      thru qry-ini-cic-999        .
       qry-rou-pri-791.
           exit.
       qry-rou-pri-810.
      *              *-------------------------------------------------*
      *              * Esecuzione per fine 1. livello di rottura       *
      *              *-------------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to qry-rou-pri-811.
           perform   qry-fin-lr1-000      thru qry-fin-lr1-999        .
       qry-rou-pri-811.
           exit.
       qry-rou-pri-820.
      *              *-------------------------------------------------*
      *              * Esecuzione per fine 2. livello di rottura       *
      *              *-------------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to qry-rou-pri-821.
           perform   qry-fin-lr2-000      thru qry-fin-lr2-999        .
       qry-rou-pri-821.
           exit.
       qry-rou-pri-830.
      *              *-------------------------------------------------*
      *              * Esecuzione per fine 3. livello di rottura       *
      *              *-------------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to qry-rou-pri-831.
           perform   qry-fin-lr3-000      thru qry-fin-lr3-999        .
       qry-rou-pri-831.
           exit.
       qry-rou-pri-840.
      *              *-------------------------------------------------*
      *              * Esecuzione per fine 4. livello di rottura       *
      *              *-------------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to qry-rou-pri-841.
           perform   qry-fin-lr4-000      thru qry-fin-lr4-999        .
       qry-rou-pri-841.
           exit.
       qry-rou-pri-850.
      *              *-------------------------------------------------*
      *              * Esecuzione per fine 5. livello di rottura       *
      *              *-------------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to qry-rou-pri-851.
           perform   qry-fin-lr5-000      thru qry-fin-lr5-999        .
       qry-rou-pri-851.
           exit.
       qry-rou-pri-890.
      *              *-------------------------------------------------*
      *              * Esecuzione per fine ciclo                       *
      *              *-------------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to qry-rou-pri-891.
           perform   qry-fin-cic-000      thru qry-fin-cic-999        .
       qry-rou-pri-891.
           exit.
       qry-rou-pri-900.
      *              *-------------------------------------------------*
      *              * Close files                                     *
      *              *-------------------------------------------------*
           perform   qry-cls-fls-000      thru qry-cls-fls-999        .
       qry-rou-pri-950.
      *              *-------------------------------------------------*
      *              * End                                             *
      *              *-------------------------------------------------*
           move      "EN"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       qry-rou-pri-999.
           exit.

      *    *===========================================================*
      *    * Interrogazione : Subroutine di avanzamento pagina         *
      *    *-----------------------------------------------------------*
       qry-pag-adv-000.
      *              *-------------------------------------------------*
      *              * Avanzamento pagina                              *
      *              *-------------------------------------------------*
           move      "PA"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Test su esito interazione con operatore         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se continuazione normale                    *
      *                  *---------------------------------------------*
           if        v-key                =    spaces
                     go to qry-pag-adv-999.
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata da operatore        *
      *                  *---------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "#"            to   w-cnt-qry-flg-int
                     go to qry-pag-adv-999.
      *                  *---------------------------------------------*
      *                  * Se function-key prevista                    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Trattamento function-key                *
      *                      *-----------------------------------------*
           perform   qry-trt-fun-000      thru qry-trt-fun-999        .
      *                      *-----------------------------------------*
      *                      * Test su rientro da trattamento f-key    *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test se fine ciclo interrogazione   *
      *                          *-------------------------------------*
           if        w-cnt-qry-flg-int    =    spaces
                     go to qry-pag-adv-000.
       qry-pag-adv-999.
           exit.

      *    *===========================================================*
      *    * Routine pre-esecuzione programma                          *
      *    *-----------------------------------------------------------*
       pre-exe-pgm-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-pre-exe-pgm      .
      *              *-------------------------------------------------*
      *              * Estrazione variabili di i.p.c. per tipo inter-  *
      *              * rogazione                                       *
      *              *-------------------------------------------------*
           perform   ipc-dlp-tdi-000      thru ipc-dlp-tdi-999        .
       pre-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Preparazione tipo funzionamento programma                 *
      *    *-----------------------------------------------------------*
       pre-tip-fun-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo di interroga-   *
      *              * zione passato con le variabili di i.p.c.        *
      *              *-------------------------------------------------*
           if        w-ipc-dlp-tdi-tip    =    "D"
                     go to pre-tip-fun-200
           else      go to pre-tip-fun-600.
       pre-tip-fun-200.
      *              *-------------------------------------------------*
      *              * Se variabile di i.p.c. per descrizione          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * No richieste ad utente                      *
      *                  *---------------------------------------------*
           move      "N"                  to   w-cnt-fun-snx-ric      .
      *                  *---------------------------------------------*
      *                  * No funzionamento ciclico                    *
      *                  *---------------------------------------------*
           move      "N"                  to   w-cnt-fun-snx-cic      .
      *                  *---------------------------------------------*
      *                  * No funzionamento automatico                 *
      *                  *---------------------------------------------*
           move      "S"                  to   w-cnt-fun-snx-aut      .
      *                  *---------------------------------------------*
      *                  * Preparazione parametri                      *
      *                  *---------------------------------------------*
           move      "D"                  to   rr-tip-ord             .
           move      w-ipc-dlp-tdi-des    to   rr-des-min             .
           move      w-ipc-dlp-tdi-des    to   rr-des-max             .
           move      rr-des-max           to   w-all-str-alf          .
           move      20                   to   w-all-str-lun          .
           perform   all-str-pad-000      thru all-str-pad-999        .
           move      w-all-str-alf        to   rr-des-max             .
           move      spaces               to   rr-cod-min             .
           move      all "z"              to   rr-cod-max             .
           go to     pre-tip-fun-999.
       pre-tip-fun-600.
      *              *-------------------------------------------------*
      *              * Se nessuna variabile di i.p.c.                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * No richieste ad utente                      *
      *                  *---------------------------------------------*
           move      "N"                  to   w-cnt-fun-snx-ric      .
      *                  *---------------------------------------------*
      *                  * No funzionamento ciclico                    *
      *                  *---------------------------------------------*
           move      "N"                  to   w-cnt-fun-snx-cic      .
      *                  *---------------------------------------------*
      *                  * No funzionamento automatico                 *
      *                  *---------------------------------------------*
           move      "S"                  to   w-cnt-fun-snx-aut      .
      *                  *---------------------------------------------*
      *                  * Preparazione parametri                      *
      *                  *---------------------------------------------*
           move      "D"                  to   rr-tip-ord             .
           move      spaces               to   rr-des-min             .
           move      all "z"              to   rr-des-max             .
           move      rr-des-max           to   w-all-str-alf          .
           move      20                   to   w-all-str-lun          .
           perform   all-str-pad-000      thru all-str-pad-999        .
           move      w-all-str-alf        to   rr-des-max             .
           move      spaces               to   rr-cod-min             .
           move      all "z"              to   rr-cod-max             .
           go to     pre-tip-fun-999.
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
      *              * Open modulo accettazione codice causale         *
      *              *-------------------------------------------------*
           perform   cod-des-zct-opn-000  thru cod-des-zct-opn-999    .
       rou-opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Close files per richieste                                 *
      *    *-----------------------------------------------------------*
       rou-cls-fls-000.
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice causale        *
      *              *-------------------------------------------------*
           perform   cod-des-zct-cls-000  thru cod-des-zct-cls-999    .
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
      *                  * Abilitazione tasto Do                       *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cnt-sts-imp-ric      .
      *                  *---------------------------------------------*
      *                  * Tipo ordinamento                            *
      *                  *---------------------------------------------*
           perform   acc-tip-ord-000      thru acc-tip-ord-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
       acc-ric-sel-200.
      *                  *---------------------------------------------*
      *                  * Codice min                                  *
      *                  *---------------------------------------------*
           perform   acc-cod-min-000      thru acc-cod-min-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-100.
       acc-ric-sel-300.
      *                  *---------------------------------------------*
      *                  * Codice max                                  *
      *                  *---------------------------------------------*
           perform   acc-cod-max-000      thru acc-cod-max-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-200.
       acc-ric-sel-400.
      *                  *---------------------------------------------*
      *                  * Descrizione min                             *
      *                  *---------------------------------------------*
           perform   acc-des-min-000      thru acc-des-min-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-300.
       acc-ric-sel-500.
      *                  *---------------------------------------------*
      *                  * Descrizione max                             *
      *                  *---------------------------------------------*
           perform   acc-des-max-000      thru acc-des-max-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-400.
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
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tipo ordinamento           :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Selezione su tabella regioni"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "------ minimo ------"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      52                   to   v-pos                  .
           move      "----- massimo ------"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      09                   to   v-pos                  .
           move      "- Codice           :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      09                   to   v-pos                  .
           move      "- Descrizione      :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo selezione : Tipo ordinamento           *
      *    *-----------------------------------------------------------*
       acc-tip-ord-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-tip-ord-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-ord-lun    to   v-car                  .
           move      w-exp-tip-ord-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      05                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-tip-ord-tbl    to   v-txt                  .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-ric    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           if        rr-tip-ord           =    "D"
                     move  01             to   v-num
           else if   rr-tip-ord           =    "C"
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-tip-ord-999.
       acc-tip-ord-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        v-num                =    01
                     move  "D"            to   rr-tip-ord
           else if   v-num                =    02
                     move  "C"            to   rr-tip-ord
           else      move  spaces         to   rr-tip-ord             .
       acc-tip-ord-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
           if        rr-tip-ord           =    spaces
                     go to acc-tip-ord-100.
       acc-tip-ord-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-tip-ord-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-tip-ord-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-tip-ord-100.
       acc-tip-ord-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo selezione : Codice min                 *
      *    *-----------------------------------------------------------*
       acc-cod-min-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cod-min-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-des-zct-ope      .
           move      rr-cod-min           to   w-cod-des-zct-cod      .
           move      12                   to   w-cod-des-zct-lin      .
           move      30                   to   w-cod-des-zct-pos      .
           move      zero                 to   w-cod-des-zct-dln      .
           move      zero                 to   w-cod-des-zct-dps      .
           move      spaces               to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           if        w-cnt-sts-imp-ric    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           perform   cod-des-zct-cll-000  thru cod-des-zct-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-des-zct-foi-000  thru cod-des-zct-foi-999    .
       acc-cod-min-110.
           perform   cod-des-zct-cll-000  thru cod-des-zct-cll-999    .
           if        w-cod-des-zct-ope    =    "F+"
                     go to acc-cod-min-115.
           if        w-cod-des-zct-ope    =    "AC"
                     go to acc-cod-min-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-min-115.
           perform   cod-des-zct-foi-000  thru cod-des-zct-foi-999    .
           go to     acc-cod-min-110.
       acc-cod-min-120.
           move      w-cod-des-zct-cod    to   v-alf                  .
       acc-cod-min-150.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-cod-min-999.
       acc-cod-min-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   rr-cod-min             .
       acc-cod-min-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-cod-min-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cod-min-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-cod-min-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-cod-min-100.
       acc-cod-min-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo selezione : Codice max                 *
      *    *-----------------------------------------------------------*
       acc-cod-max-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cod-max-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-des-zct-ope      .
           move      rr-cod-max           to   w-cod-des-zct-cod      .
           move      12                   to   w-cod-des-zct-lin      .
           move      52                   to   w-cod-des-zct-pos      .
           move      zero                 to   w-cod-des-zct-dln      .
           move      zero                 to   w-cod-des-zct-dps      .
           move      spaces               to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           if        w-cnt-sts-imp-ric    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           perform   cod-des-zct-cll-000  thru cod-des-zct-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-des-zct-foi-000  thru cod-des-zct-foi-999    .
       acc-cod-max-110.
           perform   cod-des-zct-cll-000  thru cod-des-zct-cll-999    .
           if        w-cod-des-zct-ope    =    "F+"
                     go to acc-cod-max-115.
           if        w-cod-des-zct-ope    =    "AC"
                     go to acc-cod-max-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-max-115.
           perform   cod-des-zct-foi-000  thru cod-des-zct-foi-999    .
           go to     acc-cod-max-110.
       acc-cod-max-120.
           move      w-cod-des-zct-cod    to   v-alf                  .
       acc-cod-max-150.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-cod-max-999.
       acc-cod-max-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   rr-cod-max             .
       acc-cod-max-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-cod-max-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cod-max-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-cod-max-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-cod-max-100.
       acc-cod-max-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo selezione : Descrizione min            *
      *    *-----------------------------------------------------------*
       acc-des-min-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-des-min-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-ric    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      rr-des-min           to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-des-min-999.
       acc-des-min-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   rr-des-min             .
       acc-des-min-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-des-min-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-des-min-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-des-min-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-des-min-100.
       acc-des-min-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo selezione : Descrizione max            *
      *    *-----------------------------------------------------------*
       acc-des-max-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-des-max-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      52                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-ric    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      rr-des-max           to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-des-max-999.
       acc-des-max-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   rr-des-max             .
       acc-des-max-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-des-max-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-des-max-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-des-max-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-des-max-100.
       acc-des-max-999.
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
      *              * Controllo su codice min e max                   *
      *              *-------------------------------------------------*
           if        rr-cod-max           =    spaces
                     go to tdo-ric-sel-200.
           if        rr-cod-max           not  < rr-cod-min
                     go to tdo-ric-sel-200.
           move      "ME"                 to   v-ope                  .
           move      "Il codice massimo non puo' essere inferiore al cod
      -              "ice minimo !"       to   v-not                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "#"                  to   w-cnt-tdo-ric-flg      .
           go to     tdo-ric-sel-999.
       tdo-ric-sel-200.
      *              *-------------------------------------------------*
      *              * Controllo su descrizione min e max              *
      *              *-------------------------------------------------*
           if        rr-des-max           =    spaces
                     go to tdo-ric-sel-999.
           if        rr-des-max           not  < rr-des-min
                     go to tdo-ric-sel-999.
           move      "ME"                 to   v-ope                  .
           move      "La descrizione massima non puo' essere inferiore a
      -              "quella minima !"    to   v-not                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "#"                  to   w-cnt-tdo-ric-flg      .
           go to     tdo-ric-sel-999.
       tdo-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Regolarizzazione dei parametri di selezione               *
      *    *-----------------------------------------------------------*
       reg-ric-sel-000.
      *              *-------------------------------------------------*
      *              * Regolarizzazione tipo ordinamento               *
      *              *-------------------------------------------------*
           if        rr-tip-ord           =    spaces
                     move  "D"            to   rr-tip-ord             .
      *              *-------------------------------------------------*
      *              * Regolarizzazione codice min - max               *
      *              *-------------------------------------------------*
           if        rr-cod-max           =    spaces
                     move  rr-cod-min     to   rr-cod-max             .
           move      rr-cod-max           to   w-all-str-alf          .
           move      03                   to   w-all-str-lun          .
           perform   all-str-pad-000      thru all-str-pad-999        .
           move      w-all-str-alf        to   rr-cod-max             .
      *              *-------------------------------------------------*
      *              * Regolarizzazione descrizione min - max          *
      *              *-------------------------------------------------*
           if        rr-des-max           =    spaces
                     move  rr-des-min     to   rr-des-max             .
           move      rr-des-max           to   w-all-str-alf          .
           move      20                   to   w-all-str-lun          .
           perform   all-str-pad-000      thru all-str-pad-999        .
           move      w-all-str-alf        to   rr-des-max             .
       reg-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione richieste di selezione                    *
      *    *-----------------------------------------------------------*
       nor-ric-sel-000.
           move      spaces               to   rr-tip-ord             .
           move      spaces               to   rr-cod-min             .
           move      spaces               to   rr-cod-max             .
           move      spaces               to   rr-des-min             .
           move      spaces               to   rr-des-max             .
       nor-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Interrogazione : Open files                               *
      *    *-----------------------------------------------------------*
       qry-opn-fls-000.
      *              *-------------------------------------------------*
      *              * [zct]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/bol/fls/ioc/obj/iofzct"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zct                 .
       qry-opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Interrogazione : Close files                              *
      *    *-----------------------------------------------------------*
       qry-cls-fls-000.
      *              *-------------------------------------------------*
      *              * [zct]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/bol/fls/ioc/obj/iofzct"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zct                 .
       qry-cls-fls-999.
           exit.

      *    *===========================================================*
      *    * Interrogazione : Start iniziale                           *
      *    *-----------------------------------------------------------*
       qry-str-ini-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-qry-flg-sub      .
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo ordinamento     *
      *              *-------------------------------------------------*
           if        rr-tip-ord           =    "C"
                     go to qry-str-ini-200
           else      go to qry-str-ini-400.
       qry-str-ini-200.
      *              *-------------------------------------------------*
      *              * Se ordinamento per codice                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Start per codice                            *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "CODNAZ    "         to   f-key                  .
           move      rr-cod-min           to   rf-zct-cod-ctr         .
           move      "pgm/bol/fls/ioc/obj/iofzct"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zct                 .
      *                      *-----------------------------------------*
      *                      * Se Start errata                         *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     move  "#"            to   w-cnt-qry-flg-sub
                     go to qry-str-ini-999.
      *                      *-----------------------------------------*
      *                      * Se Ok                                   *
      *                      *-----------------------------------------*
           go to     qry-str-ini-999.
       qry-str-ini-400.
      *              *-------------------------------------------------*
      *              * Se ordinamento per descrizione                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Start per descrizione in uppercase          *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "DESKEY    "         to   f-key                  .
           move      rr-des-min           to   rf-zct-des-key         .
           move      spaces               to   rf-zct-cod-ctr         .
           move      "pgm/bol/fls/ioc/obj/iofzct"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zct                 .
      *                      *-----------------------------------------*
      *                      * Se Start errata                         *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     move  "#"            to   w-cnt-qry-flg-sub
                     go to qry-str-ini-999.
      *                      *-----------------------------------------*
      *                      * Se Ok                                   *
      *                      *-----------------------------------------*
           go to     qry-str-ini-999.
       qry-str-ini-999.
           exit.

      *    *===========================================================*
      *    * Interrogazione : Messaggio per nessuna registrazione      *
      *    *-----------------------------------------------------------*
       qry-nes-ela-000.
           move      "ME"                 to   v-ope                  .
           move      "Nessuna causale entro i limiti assegnati !"
                                          to   v-not                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       qry-nes-ela-999.
           exit.

      *    *===========================================================*
      *    * Interrogazione : Lettura sequenziale                      *
      *    *-----------------------------------------------------------*
       qry-let-seq-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-qry-flg-sub      .
      *              *-------------------------------------------------*
      *              * Lettura sequenziale archivio [zct]              *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/bol/fls/ioc/obj/iofzct"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zct                 .
      *                  *---------------------------------------------*
      *                  * Se fine file                                *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     move  "#"            to   w-cnt-qry-flg-sub      .
       qry-let-seq-999.
           exit.

      *    *===========================================================*
      *    * Interrogazione : Test se superamento limiti massimi       *
      *    *-----------------------------------------------------------*
       qry-tst-max-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-qry-flg-sub      .
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo ordinamento     *
      *              *-------------------------------------------------*
           if        rr-tip-ord           =    "C"
                     go to qry-tst-max-200
           else      go to qry-tst-max-400.
       qry-tst-max-200.
      *              *-------------------------------------------------*
      *              * Se ordinamento per codice                       *
      *              *-------------------------------------------------*
           if        rf-zct-cod-ctr       >    rr-cod-max
                     move  "#"            to   w-cnt-qry-flg-sub      .
           go to     qry-tst-max-999.
       qry-tst-max-400.
      *              *-------------------------------------------------*
      *              * Se ordinamento per descrizione                  *
      *              *-------------------------------------------------*
           if        rf-zct-des-key       >    rr-des-max
                     move  "#"            to   w-cnt-qry-flg-sub      .
           go to     qry-tst-max-999.
       qry-tst-max-999.
           exit.

      *    *===========================================================*
      *    * Interrogazione : Selezione su record letto                *
      *    *-----------------------------------------------------------*
       qry-sel-rec-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-qry-flg-sub      .
      *              *-------------------------------------------------*
      *              * Selezione su codice                             *
      *              *-------------------------------------------------*
           if        rf-zct-cod-ctr       <    rr-cod-min or
                     rf-zct-cod-ctr       >    rr-cod-max
                     move  "#"            to   w-cnt-qry-flg-sub
                     go to qry-sel-rec-999.
      *              *-------------------------------------------------*
      *              * Selezione su descrizione                        *
      *              *-------------------------------------------------*
           if        rf-zct-des-key       <    rr-des-min or
                     rf-zct-des-key       >    rr-des-max
                     move  "#"            to   w-cnt-qry-flg-sub
                     go to qry-sel-rec-999.
       qry-sel-rec-999.
           exit.

      *    *===========================================================*
      *    * Interrogazione : Composizione area per tests di rottura   *
      *    *-----------------------------------------------------------*
       qry-cmp-rot-000.
       qry-cmp-rot-999.
           exit.

      *    *===========================================================*
      *    * Interrogazione : Esecuzione per inizio ciclo              *
      *    *-----------------------------------------------------------*
       qry-ini-cic-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-qry-flg-int      .
      *              *-------------------------------------------------*
      *              * Intestazione pagina                             *
      *              *-------------------------------------------------*
           perform   int-pag-sta-000      thru int-pag-sta-999        .
      *                  *---------------------------------------------*
      *                  * Test se interruzione forzata                *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to qry-ini-cic-999.
       qry-ini-cic-999.
           exit.

      *    *===========================================================*
      *    * Interrogazione : Esecuzione per fine ciclo                *
      *    *-----------------------------------------------------------*
       qry-fin-cic-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-qry-flg-int      .
       qry-fin-cic-999.
           exit.

      *    *===========================================================*
      *    * Interrogazione : Inizio 5. livello di rottura             *
      *    *-----------------------------------------------------------*
       qry-ini-lr5-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-qry-flg-int      .
       qry-ini-lr5-999.
           exit.

      *    *===========================================================*
      *    * Interrogazione : Fine 5. livello di rottura               *
      *    *-----------------------------------------------------------*
       qry-fin-lr5-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-qry-flg-int      .
       qry-fin-lr5-999.
           exit.

      *    *===========================================================*
      *    * Interrogazione : Inizio 4. livello di rottura             *
      *    *-----------------------------------------------------------*
       qry-ini-lr4-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-qry-flg-int      .
       qry-ini-lr4-999.
           exit.

      *    *===========================================================*
      *    * Interrogazione : Fine 4. livello di rottura               *
      *    *-----------------------------------------------------------*
       qry-fin-lr4-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-qry-flg-int      .
       qry-fin-lr4-999.
           exit.

      *    *===========================================================*
      *    * Interrogazione : Inizio 3. livello di rottura             *
      *    *-----------------------------------------------------------*
       qry-ini-lr3-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-qry-flg-int      .
       qry-ini-lr3-999.
           exit.

      *    *===========================================================*
      *    * Interrogazione : Fine 3. livello di rottura               *
      *    *-----------------------------------------------------------*
       qry-fin-lr3-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-qry-flg-int      .
       qry-fin-lr3-999.
           exit.

      *    *===========================================================*
      *    * Interrogazione : Inizio 2. livello di rottura             *
      *    *-----------------------------------------------------------*
       qry-ini-lr2-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-qry-flg-int      .
       qry-ini-lr2-999.
           exit.

      *    *===========================================================*
      *    * Interrogazione : Fine 2. livello di rottura               *
      *    *-----------------------------------------------------------*
       qry-fin-lr2-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-qry-flg-int      .
       qry-fin-lr2-999.
           exit.

      *    *===========================================================*
      *    * Interrogazione : Inizio 1. livello di rottura             *
      *    *-----------------------------------------------------------*
       qry-ini-lr1-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-qry-flg-int      .
       qry-ini-lr1-999.
           exit.

      *    *===========================================================*
      *    * Interrogazione : Fine 1. livello di rottura               *
      *    *-----------------------------------------------------------*
       qry-fin-lr1-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-qry-flg-int      .
       qry-fin-lr1-999.
           exit.

      *    *===========================================================*
      *    * Interrogazione : Livello di dettaglio                     *
      *    *-----------------------------------------------------------*
       qry-liv-det-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-qry-flg-int      .
      *              *-------------------------------------------------*
      *              * Test se righe residue sufficienti               *
      *              *-------------------------------------------------*
           if        v-res                >    1
                     go to qry-liv-det-200.
      *              *-------------------------------------------------*
      *              * Intestazione pagina                             *
      *              *-------------------------------------------------*
           perform   int-pag-sta-000      thru int-pag-sta-999        .
      *                  *---------------------------------------------*
      *                  * Test se interruzione forzata                *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to qry-liv-det-999.
       qry-liv-det-200.
      *              *-------------------------------------------------*
      *              * Interlinea                                      *
      *              *-------------------------------------------------*
           move      "LF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Stampa                                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Codice causale                              *
      *                  *---------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      28                   to   v-pos                  .
           move      rf-zct-cod-ctr       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Descrizione causale                         *
      *                  *---------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      35                   to   v-pos                  .
           move      rf-zct-des-ctr       to   v-alf                  .
           move      "+"                  to   v-edm                  .
           move      rf-zct-cod-ctr       to   w-mpn-cod-ctr          .
           move      w-mpn                to   v-cnt                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       qry-liv-det-999.
           exit.

      *    *===========================================================*
      *    * Interrogazione : Function-keys previste in Mark-points    *
      *    *-----------------------------------------------------------*
       qry-det-fky-000.
      *              *-------------------------------------------------*
      *              * Determinazione eventuale function key Slct      *
      *              *-------------------------------------------------*
           move      "GV"                 to   s-ope                  .
           move      "fkselect"           to   s-var                  .
           move      "-"                  to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces and
                     s-alf                =    "SLCT"
                     move   "SLCT"        to   v-pfk(01)
           else      move   spaces        to   v-pfk(01)              .
      *              *-------------------------------------------------*
      *              * Function key Insr                               *
      *              *-------------------------------------------------*
           move      "P?"                 to   s-ope                  .
           move      "pbol0200"           to   s-pro                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-liv                =    zero
                     move  "INSR"         to   v-pfk(02)
           else      move  spaces         to   v-pfk(02)              .
      *              *-------------------------------------------------*
      *              * Altre function keys                             *
      *              *-------------------------------------------------*
           move      spaces               to   v-pfk(03)              .
           move      spaces               to   v-pfk(04)              .
           move      spaces               to   v-pfk(05)              .
           move      spaces               to   v-pfk(06)              .
           move      spaces               to   v-pfk(07)              .
           move      spaces               to   v-pfk(08)              .
           move      spaces               to   v-pfk(09)              .
           move      spaces               to   v-pfk(10)              .
       qry-det-fky-999.
           exit.

      *    *===========================================================*
      *    * Interrogazione : Trattamento Function-key selezionata     *
      *    *-----------------------------------------------------------*
       qry-trt-fun-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-qry-flg-int      .
      *              *-------------------------------------------------*
      *              * Trattamento function-key Slct                   *
      *              *-------------------------------------------------*
           if        v-key                not  = "SLCT"
                     go to qry-trt-fun-500.
           move      spaces               to   v-key                  .
      *                  *---------------------------------------------*
      *                  * Put della variabile di i.p.c.: 'cod-ctr'    *
      *                  *---------------------------------------------*
           move      "PV"                 to   s-ope                  .
           move      "cod-ctr"            to   s-var                  .
           move      "-"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      03                   to   s-car                  .
           move      v-cnt                to   w-mpn                  .
           move      w-mpn-cod-ctr        to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Segnale di interruzione interrogazione      *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cnt-qry-flg-int      .
      *                  *---------------------------------------------*
      *                  * Segnale di uscita dal programma             *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cnt-qry-rou-pri      .
           go to     qry-trt-fun-999.
       qry-trt-fun-500.
      *              *-------------------------------------------------*
      *              * Trattamento function-key Insr                   *
      *              *-------------------------------------------------*
           if        v-key                not  = "INSR"
                     go to qry-trt-fun-999.
           move      spaces               to   v-key                  .
      *                  *---------------------------------------------*
      *                  * Richiamo programma di gestione archivio     *
      *                  *---------------------------------------------*
           move      "pgm/bol/prg/obj/pbol0200"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat                                            .
           cancel    s-pat                                            .
       qry-trt-fun-999.
           exit.

      *    *===========================================================*
      *    * Intestazione per interrogazione                           *
      *    *-----------------------------------------------------------*
       int-pag-sta-000.
      *              *-------------------------------------------------*
      *              * Subroutine di avanzamento pagina                *
      *              *-------------------------------------------------*
           perform   qry-pag-adv-000      thru qry-pag-adv-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to int-pag-sta-999.
      *              *-------------------------------------------------*
      *              * Linea di trattini                               *
      *              *-------------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      01                   to   v-pos                  .
           move      w-cnt-lit-t80        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Titolo                                          *
      *              *-------------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      21                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      31                   to   v-pos                  .
           move      "CAUSALI DEL TRASPORTO"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Linea di trattini                               *
      *              *-------------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      01                   to   v-pos                  .
           move      w-cnt-lit-t80        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Interlinea                                      *
      *              *-------------------------------------------------*
           move      "LF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Fincatura                                       *
      *              *-------------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      29                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      26                   to   v-pos                  .
           move      "Codice       Descrizione     "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Interlinea                                      *
      *              *-------------------------------------------------*
           move      "LF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Sottolineatura                                  *
      *              *-------------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      29                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      26                   to   v-pos                  .
           move      "------   --------------------"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       int-pag-sta-999.
           exit.

      *    *===========================================================*
      *    * Lettura i.p.c. da livello precedente per tipo di interro- *
      *    * gazione e parametro relativo                              *
      *    *-----------------------------------------------------------*
       ipc-dlp-tdi-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione iniziale                        *
      *              *-------------------------------------------------*
           move      spaces               to   w-ipc-dlp-tdi-tip      .
           move      spaces               to   w-ipc-dlp-tdi-des      .
      *              *-------------------------------------------------*
      *              * Estrazione della eventuale variabile di i.p.c.  *
      *              * "tip-int" per tipo interrogazione               *
      *              *-------------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "tip-int"            to   s-var                  .
           move      "-"                  to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                not  = spaces
                     go to ipc-dlp-tdi-999.
           if        s-alf                =    "D"
                     go to ipc-dlp-tdi-300
           else      go to ipc-dlp-tdi-999.
       ipc-dlp-tdi-300.
      *              *-------------------------------------------------*
      *              * Estrazione della eventuale variabile di i.p.c.  *
      *              * "des-cmn" per la descrizione                    *
      *              *-------------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "des-key"            to   s-var                  .
           move      "-"                  to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                not  = spaces
                     go to ipc-dlp-tdi-999.
           if        s-alf                =    spaces
                     go to ipc-dlp-tdi-999.
           move      "D"                  to   w-ipc-dlp-tdi-tip      .
           move      s-alf                to   w-ipc-dlp-tdi-des      .
           go to     ipc-dlp-tdi-999.
       ipc-dlp-tdi-999.
           exit.

      *    *===========================================================*
      *    * Subroutines per allineamenti a destra o a sinistra oppure *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cps"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice causale         *
      *    *-----------------------------------------------------------*
           copy      "pgm/bol/prg/cpy/acdezct0.acs"                   .
