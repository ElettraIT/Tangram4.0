       Identification Division.
       Program-Id.                                 pcge2020           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    cge                 *
      *                                Settore:    arc                 *
      *                                   Fase:    cge202              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 25/10/89    *
      *                       Ultima revisione:    NdK del 16/10/01    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Richieste per il programma pcge2021:        *
      *                                                                *
      *                    Stampa anagrafica piano dei conti           *
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
                     "cge"                                            .
      *        *-------------------------------------------------------*
      *        * Settore gestionale                                    *
      *        *-------------------------------------------------------*
           05  i-ide-set                  pic  x(03) value
                     "arc"                                            .
      *        *-------------------------------------------------------*
      *        * Fase gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-fas                  pic  x(06) value
                     "cge202"                                         .
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10) value
                     "pcge2020  "                                     .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  i-ide-des                  pic  x(40) value
                     "    STAMPA ANAGRAFICA PIANO DEI CONTI   "       .

      *    *===========================================================*
      *    * Area per il programma di esecuzione                       *
      *    *-----------------------------------------------------------*
       01  z-exe.
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma di esecuzione             *
      *        *-------------------------------------------------------*
           05  z-exe-pro                  pic  x(10) value
                     "pcge2021  "                                     .
      *        *-------------------------------------------------------*
      *        * Pathname del programma di esecuzione                  *
      *        *-------------------------------------------------------*
           05  z-exe-pat                  pic  x(40) value
                     "pgm/cge/prg/obj/pcge2021                "       .

      *    *===========================================================*
      *    * Area per i parametri selezione stampa                     *
      *    *-----------------------------------------------------------*
       01  z-stp.
      *        *-------------------------------------------------------*
      *        * Si/No selezione stampa da eseguire                    *
      *        *-------------------------------------------------------*
           05  z-stp-snx-sel              pic  x(01) value "S"        .
      *        *-------------------------------------------------------*
      *        * Flags di tipo selezione                               *
      *        *-------------------------------------------------------*
           05  z-stp-tip-sel              pic  x(10) value spaces     .
      *        *-------------------------------------------------------*
      *        * Codice stampante                                      *
      *        *-------------------------------------------------------*
           05  z-stp-cod-stp              pic  x(08) value "        " .
      *        *-------------------------------------------------------*
      *        * Tipo di stampa                                        *
      *        *-------------------------------------------------------*
           05  z-stp-tip-sta              pic  x(01) value " "        .
      *        *-------------------------------------------------------*
      *        * Codice modulo                                         *
      *        *-------------------------------------------------------*
           05  z-stp-cod-mod              pic  x(08) value "        " .
      *        *-------------------------------------------------------*
      *        * Tipo modulo ( Libero o Tipografico )                  *
      *        *-------------------------------------------------------*
           05  z-stp-tip-mod              pic  x(01) value "L"        .
      *        *-------------------------------------------------------*
      *        * Ampiezza linea di stampa in caratteri                 *
      *        *-------------------------------------------------------*
           05  z-stp-amp-lin              pic  9(03) value 80         .
      *        *-------------------------------------------------------*
      *        * Top margin in linee                                   *
      *        *-------------------------------------------------------*
           05  z-stp-top-lin              pic  9(04)       value 1    .
      *        *-------------------------------------------------------*
      *        * Numero linee di stampa minimo                         *
      *        *-------------------------------------------------------*
           05  z-stp-lin-min              pic  9(02) value 30         .
      *        *-------------------------------------------------------*
      *        * Bottom margin in linee                                *
      *        *-------------------------------------------------------*
           05  z-stp-bot-lin              pic  9(04)       value 1    .
      *        *-------------------------------------------------------*
      *        * Ampiezza caratteri                                    *
      *        *-------------------------------------------------------*
           05  z-stp-amp-car              pic  9(02)v9(02) value zero .
      *        *-------------------------------------------------------*
      *        * Altezza interlinea                                    *
      *        *-------------------------------------------------------*
           05  z-stp-alt-int              pic  9(02)v9(02) value zero .
      *        *-------------------------------------------------------*
      *        * Area riservata per espansioni future                  *
      *        *-------------------------------------------------------*
           05  z-stp-esp-fut              pic  x(99) value spaces     .
      *        *-------------------------------------------------------*
      *        * Area riservata per funzioni speciali                  *
      *        *-------------------------------------------------------*
           05  z-stp-fnz-spc              pic  x(99) value spaces     .

      *    *===========================================================*
      *    * Record per le richieste                                   *
      *    *-----------------------------------------------------------*
       01  rr.
           05  rr-cod-min                 pic  9(07)                  .
           05  rr-mcs-min redefines
               rr-cod-min                                          .
               10  rr-mas-min             pic  9(02)                  .
               10  rr-con-min             pic  9(02)                  .
               10  rr-stc-min             pic  9(03)                  .
           05  rr-cod-max                 pic  9(07)                  .
           05  rr-mcs-max redefines
               rr-cod-max                                          .
               10  rr-mas-max             pic  9(02)                  .
               10  rr-con-max             pic  9(02)                  .
               10  rr-stc-max             pic  9(03)                  .

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
      *    * Area per utilizzo interno                                 *
      *    *-----------------------------------------------------------*
       01  y-are.
      *        *-------------------------------------------------------*
      *        * Status di uscita da routines specifiche               *
      *        *-------------------------------------------------------*
           05  OK                         pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * 80 trattini                                           *
      *        *-------------------------------------------------------*
           05  y-are-080-tra              pic  x(80) value all "="    .
      *        *-------------------------------------------------------*
      *        * Indice puntatore per string-unstring                  *
      *        *-------------------------------------------------------*
           05  y-pnt                      pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Comodo di 255  caratteri per string-unstring          *
      *        *-------------------------------------------------------*
           05  y-seu.
               10  filler occurs 255      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Salvataggio indice puntatore per string-unstring      *
      *        *-------------------------------------------------------*
           05  y-svp                      pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Work per regolarizzazione campi alfanumerici          *
      *        *-------------------------------------------------------*
           05  y-reg-are.
               10  y-reg-alf.
                   15  y-reg-chr occurs 20       
                                          pic  x(01)                  .
               10  y-reg-ctr              pic  9(02)                  .

      *    *===========================================================*
      *    * Work-area personalizzazioni                               *
      *    *-----------------------------------------------------------*
       01  w-prs.
           05  w-prs-liv-pdc              pic  9(01)                  .

      *    *===========================================================*
      *    * Work-area per mastro-conto-sottoconto                     *
      *    *-----------------------------------------------------------*
       01  w-mcs.
           05  w-mcs-cod-pdc              pic  9(07)                  .
           05  w-mcs-cod-pdr redefines
               w-mcs-cod-pdc.
               10  w-mcs-cod-mas          pic  9(02)                  .
               10  w-mcs-cod-con          pic  9(02)                  .
               10  w-mcs-cod-stc          pic  9(03)                  .

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
           perform   z-dic-ini-pgm-000    thru z-dic-ini-pgm-999      .
           if        OK                   not  = spaces
                     go to main-999.
      *              *-------------------------------------------------*
      *              * Visualizzazione titolo programma                *
      *              *-------------------------------------------------*
           perform   z-vis-tit-pgm-000    thru z-vis-tit-pgm-999      .
      *              *-------------------------------------------------*
      *              * Esecuzione routine pre-esecuzione programma     *
      *              *-------------------------------------------------*
           move      spaces               to   OK                     .
           perform   z-pre-exe-pgm-000    thru z-pre-exe-pgm-999      .
           if        OK                   not  = spaces
                     go to main-900.
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
           perform   z-ric-opn-fls-000    thru z-ric-opn-fls-999      .
      *              *-------------------------------------------------*
      *              * Normalizzazione record richieste                *
      *              *-------------------------------------------------*
           perform   z-ric-nor-rec-000    thru z-ric-nor-rec-999      .
      *              *-------------------------------------------------*
      *              * Visualizzazione prompts richieste               *
      *              *-------------------------------------------------*
           perform   z-ric-vis-pmt-000    thru z-ric-vis-pmt-999      .
      *              *-------------------------------------------------*
      *              * Inserimento valori richieste                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Accettazione record richieste               *
      *                  *---------------------------------------------*
           perform   z-ric-acc-rec-000    thru z-ric-acc-rec-999      .
      *                  *---------------------------------------------*
      *                  * Test su tasto di uscita                     *
      *                  *---------------------------------------------*
           if        v-key                =    "EXIT"
                     go to main-200
           else      go to main-300.
       main-200.
      *                      *-----------------------------------------*
      *                      * Uscita per tasto "EXIT"                 *
      *                      *-----------------------------------------*
           go to     main-800.
       main-300.
      *                      *-----------------------------------------*
      *                      * Uscita per tasto "DO  "                 *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Regolarizzazione record richieste   *
      *                          *-------------------------------------*
           perform   z-ric-reg-rec-000    thru z-ric-reg-rec-999      .
      *                          *-------------------------------------*
      *                          * Selezione parametri stampa          *
      *                          *-------------------------------------*
           perform   z-sel-prm-stp-000    thru z-sel-prm-stp-999      .
      *                          *-------------------------------------*
      *                          * Test su esito selezione             *
      *                          *-------------------------------------*
           if        OK                   not  = spaces
                     move  spaces         to   OK
                     go to main-800.
      *                          *-------------------------------------*
      *                          * Richiesta autorizzazione esecuzione *
      *                          * in background o foreground          *
      *                          *-------------------------------------*
           move      "BF"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                          *-------------------------------------*
      *                          * Deviazione in funzione del tipo di  *
      *                          * esecuzione da seguire               *
      *                          *-------------------------------------*
           if        s-snb                =    "B"
                     go to main-500.
      *                          *-------------------------------------*
      *                          * Se esecuzione in foreground         *
      *                          *-------------------------------------*
           perform   z-exe-pgm-frg-000    thru z-exe-pgm-frg-999      .
           go to     main-800.
       main-500.
      *                          *-------------------------------------*
      *                          * Se esecuzione in background         *
      *                          *-------------------------------------*
           perform   z-exe-pgm-bkg-000    thru z-exe-pgm-bkg-999      .
       main-800.
      *              *-------------------------------------------------*
      *              * Close files                                     *
      *              *-------------------------------------------------*
           perform   z-ric-cls-fls-000    thru z-ric-cls-fls-999      .
       main-900.
      *              *-------------------------------------------------*
      *              * Dichiarazione di fine programma                 *
      *              *-------------------------------------------------*
           perform   z-dic-fin-pgm-000    thru z-dic-fin-pgm-999      .
       main-999.
           exit      program                                          .

      *================================================================*
      *       Routines                                                 *
      *================================================================*

      *    *===========================================================*
      *    * Dichiarazione di inizio programma                         *
      *    *-----------------------------------------------------------*
       z-dic-ini-pgm-000.
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
       z-dic-ini-pgm-999.
           exit.

      *    *===========================================================*
      *    * Dichiarazione di fine programma                           *
      *    *-----------------------------------------------------------*
       z-dic-fin-pgm-000.
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
       z-dic-fin-pgm-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione titolo programma                          *
      *    *-----------------------------------------------------------*
       z-vis-tit-pgm-000.
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
           move      y-are-080-tra        to   v-alf                  .
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
           move      y-are-080-tra        to   v-alf                  .
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
           move      y-are-080-tra        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       z-vis-tit-pgm-999.
           exit.

      *    *===========================================================*
      *    * Programma di esecuzione in foreground                     *
      *    *-----------------------------------------------------------*
       z-exe-pgm-frg-000.
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
                     go to z-exe-pgm-frg-999.
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
                     go to z-exe-pgm-frg-999.
      *                  *---------------------------------------------*
      *                  * Estrazione segmenti da 255  bytes da record *
      *                  * richieste                                   *
      *                  *---------------------------------------------*
           move      1                    to   y-pnt                  .
       z-exe-pgm-frg-200.
           move      spaces               to   y-seu                  .
           move      y-pnt                to   y-svp                  .
           unstring  rr                   into y-seu
                                  with pointer y-pnt                  .
           move      y-seu                to   b-chr                  .
           if        y-pnt                =    y-svp
                     go to z-exe-pgm-frg-400.
           move      "PT"                 to   b-ope                  .
           call      "swd/mod/prg/obj/mbckgr"
                                         using b                      .
           go to     z-exe-pgm-frg-200.
       z-exe-pgm-frg-400.
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
                     go to z-exe-pgm-frg-999.
      *              *-------------------------------------------------*
      *              * Messaggio di programma in esecuzione            *
      *              *-------------------------------------------------*
           move      "PE"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Lancio del programma di esecuzione              *
      *              *-------------------------------------------------*
           call      z-exe-pat                                        .
      *              *-------------------------------------------------*
      *              * Cancel del programma di esecuzione              *
      *              *-------------------------------------------------*
           cancel    z-exe-pat                                        .
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
       z-exe-pgm-frg-999.
           exit.

      *    *===========================================================*
      *    * Programma di esecuzione in background                     *
      *    *-----------------------------------------------------------*
       z-exe-pgm-bkg-000.
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
                     go to z-exe-pgm-bkg-900.
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
                     go to z-exe-pgm-bkg-900.
      *                  *---------------------------------------------*
      *                  * Estrazione segmenti da 255  bytes da record *
      *                  * richieste                                   *
      *                  *---------------------------------------------*
           move      1                    to   y-pnt                  .
       z-exe-pgm-bkg-200.
           move      spaces               to   y-seu                  .
           move      y-pnt                to   y-svp                  .
           unstring  rr                   into y-seu
                                  with pointer y-pnt                  .
           move      y-seu                to   b-chr                  .
           if        y-pnt                =    y-svp
                     go to z-exe-pgm-bkg-400.
           move      "PT"                 to   b-ope                  .
           call      "swd/mod/prg/obj/mbckgr"
                                         using b                      .
           go to     z-exe-pgm-bkg-200.
       z-exe-pgm-bkg-400.
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
                     go to z-exe-pgm-bkg-900.
      *              *-------------------------------------------------*
      *              * Lancio del programma di esecuzione  background  *
      *              * tramite chiamata al modulo di segreteria        *
      *              *-------------------------------------------------*
           move      "B+"                 to   s-ope                  .
           move      z-exe-pro            to   s-npb                  .
           move      z-exe-pat            to   s-pmo                  .
           move      i-ide-des            to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
       z-exe-pgm-bkg-900.
      *              *-------------------------------------------------*
      *              * Cancel del modulo "mbckgr"                      *
      *              *-------------------------------------------------*
           cancel    "swd/mod/prg/obj/mbckgr"                         .
      *              *-------------------------------------------------*
      *              * Cancel del modulo "mmessg"                      *
      *              *-------------------------------------------------*
           cancel    "swd/mod/prg/obj/mmessg"                         .
       z-exe-pgm-bkg-999.
           exit.

      *================================================================*
      *       Selezione parametri stampa                               *
      *----------------------------------------------------------------*
       z-sel-prm-stp-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   OK                     .
      *              *-------------------------------------------------*
      *              * Test se selezione stampa da eseguire            *
      *              *-------------------------------------------------*
           if        z-stp-snx-sel        not  = "S"
                     go to z-sel-prm-stp-999.
      *              *-------------------------------------------------*
      *              * Preparazione parametri per richiamo selezione   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Informazioni generali da segreteria         *
      *                  *---------------------------------------------*
           move      "IG"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Codice azienda                              *
      *                  *---------------------------------------------*
           move      s-azi                to   r-env-cod-azi          .
      *                  *---------------------------------------------*
      *                  * Codice terminale                            *
      *                  *---------------------------------------------*
           move      s-ter                to   r-env-cod-ter          .
      *                  *---------------------------------------------*
      *                  * Codice utente                               *
      *                  *---------------------------------------------*
           move      s-ute                to   r-env-cod-ute          .
      *                  *---------------------------------------------*
      *                  * Date and time da segreteria                 *
      *                  *---------------------------------------------*
           move      "DT"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Date and time                               *
      *                  *---------------------------------------------*
           move      s-sdt                to   r-env-dat-tim          .
      *                  *---------------------------------------------*
      *                  * Sistema applicativo                         *
      *                  *---------------------------------------------*
           move      i-ide-sap            to   r-ide-sis-app          .
      *                  *---------------------------------------------*
      *                  * Area gestionale                             *
      *                  *---------------------------------------------*
           move      i-ide-arg            to   r-ide-are-ges          .
      *                  *---------------------------------------------*
      *                  * Settore gestionale                          *
      *                  *---------------------------------------------*
           move      i-ide-set            to   r-ide-set-ges          .
      *                  *---------------------------------------------*
      *                  * Fase gestionale                             *
      *                  *---------------------------------------------*
           move      i-ide-fas            to   r-ide-fas-ges          .
      *                  *---------------------------------------------*
      *                  * Flags di tipo selezione                     *
      *                  *---------------------------------------------*
           move      z-stp-tip-sel        to   r-fix-tip-sel          .
      *                  *---------------------------------------------*
      *                  * Codice stampante                            *
      *                  *---------------------------------------------*
           move      z-stp-cod-stp        to   r-fix-cod-stp          .
      *                  *---------------------------------------------*
      *                  * Tipo di stampa                              *
      *                  *---------------------------------------------*
           move      z-stp-tip-sta        to   r-fix-tip-sta          .
      *                  *---------------------------------------------*
      *                  * Codice modulo                               *
      *                  *---------------------------------------------*
           move      z-stp-cod-mod        to   r-fix-cod-mod          .
      *                  *---------------------------------------------*
      *                  * Tipo modulo                                 *
      *                  *---------------------------------------------*
           move      z-stp-tip-mod        to   r-fix-tip-mod          .
      *                  *---------------------------------------------*
      *                  * Ampiezza linea di stampa in caratteri       *
      *                  *---------------------------------------------*
           move      z-stp-amp-lin        to   r-fix-amp-lin          .
      *                  *---------------------------------------------*
      *                  * Top margin in linee                         *
      *                  *---------------------------------------------*
           move      z-stp-top-lin        to   r-fix-top-lin          .
      *                  *---------------------------------------------*
      *                  * Numero linee di stampa minimo               *
      *                  *---------------------------------------------*
           move      z-stp-lin-min        to   r-fix-lin-min          .
      *                  *---------------------------------------------*
      *                  * Bottom margin in linee                      *
      *                  *---------------------------------------------*
           move      z-stp-bot-lin        to   r-fix-bot-lin          .
      *                  *---------------------------------------------*
      *                  * Ampiezza caratteri                          *
      *                  *---------------------------------------------*
           move      z-stp-amp-car        to   r-fix-amp-car          .
      *                  *---------------------------------------------*
      *                  * Altezza interlinea                          *
      *                  *---------------------------------------------*
           move      z-stp-alt-int        to   r-fix-alt-int          .
      *                  *---------------------------------------------*
      *                  * Area riservata per espansioni future        *
      *                  *---------------------------------------------*
           move      z-stp-esp-fut        to   r-fix-esp-fut          .
      *                  *---------------------------------------------*
      *                  * Area riservata per funzioni speciali        *
      *                  *---------------------------------------------*
           move      z-stp-fnz-spc        to   r-fix-fnz-spc          .
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
                     move  "#"            to   OK                     .
       z-sel-prm-stp-999.
           exit.

      *================================================================*
      *       Regolarizzazione campo alfanumerico con padding di "z"   *
      *----------------------------------------------------------------*
       z-reg-cam-alf-000.
           move      20                   to   y-reg-ctr              .
       z-reg-cam-alf-100.
           if        y-reg-ctr            >    zero
                     if    y-reg-chr
                          (y-reg-ctr)     =    spaces
                           move    "z"    to   y-reg-chr
                                              (y-reg-ctr)
                           subtract 1     from y-reg-ctr
                           go to    z-reg-cam-alf-100.
       z-reg-cam-alf-999.
           exit.

      *    *===========================================================*
      *    * Open files ausiliari                                      *
      *    *-----------------------------------------------------------*
       z-ric-opn-fls-000.
       z-ric-opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Close files ausiliari                                     *
      *    *-----------------------------------------------------------*
       z-ric-cls-fls-000.
       z-ric-cls-fls-999.
           exit.

      *    *===========================================================*
      *    * Routine pre-esecuzione programma                          *
      *    *                                                           *
      *    * Uscita  : OK = spaces : continua l'esecuzione             *
      *    *                "#"    : terminazione programma            *
      *    *-----------------------------------------------------------*
       z-pre-exe-pgm-000.
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
       z-pre-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts richieste                         *
      *    *-----------------------------------------------------------*
       z-ric-vis-pmt-000.
      *              *-------------------------------------------------*
      *              * Visualizzazione prompts per impostazione        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Prompt 'Selezione su archivio piano dei     *
      *                  * conti:'                                     *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Selezione su archivio piano dei conti:"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Prompt per valore minimo e masssimo         *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "--- minimo ---         --- massimo ---"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Prompt '- Codice mastro'                    *
      *                  *---------------------------------------------*
           if        w-prs-liv-pdc        not  = 3
                     go to z-ric-vis-pmt-100.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      19                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      09                   to   v-pos                  .
           move      "- Codice mastro"    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       z-ric-vis-pmt-100.
      *                  *---------------------------------------------*
      *                  * Prompt '- Codice conto'                     *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      19                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      09                   to   v-pos                  .
           move      "- Codice conto"     to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Prompt '- Codice sottoconto'                *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      19                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      09                   to   v-pos                  .
           move      "- Codice sottoconto"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       z-ric-vis-pmt-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione record richieste                          *
      *    *-----------------------------------------------------------*
       z-ric-nor-rec-000.
           move      spaces               to   rr                     .
           move      zero                 to   rr-cod-min             .
           move      zero                 to   rr-cod-max             .
       z-ric-nor-rec-999.
           exit.

      *    *===========================================================*
      *    * Accettazione record richieste                             *
      *    *-----------------------------------------------------------*
       z-ric-acc-rec-000.
      *              *-------------------------------------------------*
      *              * Accettazione codice mastro iniziale e finale    *
      *              *-------------------------------------------------*
           if        w-prs-liv-pdc        not  = 3
                     go to z-ric-acc-rec-400.
      *                  *---------------------------------------------*
      *                  * Codice mastro iniziale                      *
      *                  *---------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9B"                 to   v-edm                  .
           move      11                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "EXIT"               to   v-pfk(1)               .
           move      "FIND"               to   v-pfk(2)               .
           move      "DOWN"               to   v-pfk(3)               .
           move      "DO  "               to   v-pfk(4)               .
           move      rr-mas-min           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-num                to   rr-mas-min             .
      *                      *-----------------------------------------*
      *                      * Se premuta function-key 'EXIT' si esce  *
      *                      * con flag di uscita dal programma        *
      *                      *-----------------------------------------*
           if        v-key                =    "EXIT"
                     move   "#"           to   OK
                     go to  z-ric-acc-rec-999.
      *                      *-----------------------------------------*
      *                      * Se premuta function-key 'FIND' si ac-   *
      *                      * cede all'interrogazione sui mastri con  *
      *                      * possibilita' di selezione codice mastro *
      *                      *-----------------------------------------*
           if        v-key                not  = "FIND"
                     go to  z-ric-acc-rec-275.
           move      spaces               to   OK                     .
           perform   fnd-cod-mas-000      thru fnd-cod-mas-999        .
           if        OK                   =    "R"
                     move   spaces        to   OK
                     go to z-ric-acc-rec-000.
           move      w-mcs-cod-mas        to   rr-mas-min             .
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9B"                 to   v-edm                  .
           move      11                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-mas-min           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       z-ric-acc-rec-275.
      *                      *-----------------------------------------*
      *                      * Check su congruenza tra valore minimo   *
      *                      * e massimo                               *
      *                      *-----------------------------------------*
           if        rr-mas-max           not  = zero and
                     rr-mas-max           <    rr-mas-min
                     go to z-ric-acc-rec-000.
      *                      *-----------------------------------------*
      *                      * Se premuta function-key 'DO' si esce    *
      *                      * dall'impostazione richieste             *
      *                      *-----------------------------------------*
           if        v-key                =    "DO  "
                     go to  z-ric-acc-rec-999.
       z-ric-acc-rec-300.
      *                  *---------------------------------------------*
      *                  * Codice mastro finale                        *
      *                  *---------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9B"                 to   v-edm                  .
           move      11                   to   v-lin                  .
           move      53                   to   v-pos                  .
           move      "EXIT"               to   v-pfk(1)               .
           move      "UP  "               to   v-pfk(2)               .
           move      "DOWN"               to   v-pfk(3)               .
           move      "DO  "               to   v-pfk(4)               .
           move      "FIND"               to   v-pfk(5)               .
           move      rr-mas-max           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-num                to   rr-mas-max             .
      *                      *-----------------------------------------*
      *                      * Se premuta function-key 'EXIT' si esce  *
      *                      * con flag di uscita dal programma        *
      *                      *-----------------------------------------*
           if        v-key                =    "EXIT"
                     move   "#"           to   OK
                     go to  z-ric-acc-rec-999.
      *                      *-----------------------------------------*
      *                      * Se premuta function-key 'FIND' si ac-   *
      *                      * cede all'interrogazione sui mastri con  *
      *                      * possibilita' di selezione codice mastro *
      *                      *-----------------------------------------*
           if        v-key                not  = "FIND"
                     go to  z-ric-acc-rec-350.
           move      spaces               to   OK                     .
           perform   fnd-cod-mas-000      thru fnd-cod-mas-999        .
           if        OK                   =    "R"
                     move   spaces        to   OK
                     go to  z-ric-acc-rec-300.
           move      w-mcs-cod-mas        to   rr-mas-max             .
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9B"                 to   v-edm                  .
           move      11                   to   v-lin                  .
           move      53                   to   v-pos                  .
           move      rr-mas-max           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       z-ric-acc-rec-350.
      *                      *-----------------------------------------*
      *                      * Check su congruenza tra valore minimo   *
      *                      * e massimo                               *
      *                      *-----------------------------------------*
           if        rr-mas-max           not  = zero and
                     rr-mas-max           <    rr-mas-min
                     go to z-ric-acc-rec-300.
      *                      *-----------------------------------------*
      *                      * Se function-key 'UP' ritorna all'impo-  *
      *                      * stazione precedente                     *
      *                      *-----------------------------------------*
           if        v-key                =    "UP  "
                     go to  z-ric-acc-rec-000.
      *                      *-----------------------------------------*
      *                      * Se il codice mastro minimo e il codice  *
      *                      * mastro massimo sono entrambi a zero     *
      *                      * si pongono a zero tutti gli altri para- *
      *                      * metri e si passa alla richiesta con-    *
      *                      * ferma finale                            *
      *                      *-----------------------------------------*
           if        rr-mas-min           =    zero and
                     rr-mas-max           =    zero
                     go to z-ric-acc-rec-360.
      *                      *-----------------------------------------*
      *                      * Se il codice mastro massimo e' zero si  *
      *                      * prosegue con le richieste, altrimenti   *
      *                      * si pongono a zero tutti gli altri para- *
      *                      * metri e si passa alla richiesta con-    *
      *                      * ferma finale                            *
      *                      *-----------------------------------------*
           if        rr-mas-max           =    zero or
                     rr-mas-max           =    rr-mas-min
                     go to  z-ric-acc-rec-375.
       z-ric-acc-rec-360.
      *                          *-------------------------------------*
      *                          * Codice conto iniziale               *
      *                          *-------------------------------------*
           if        rr-con-min           =    zero
                     go to z-ric-acc-rec-362.
           move      zero                 to   rr-con-min             .
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9B"                 to   v-edm                  .
           move      12                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-con-min           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       z-ric-acc-rec-362.
      *                          *-------------------------------------*
      *                          * Codice conto finale                 *
      *                          *-------------------------------------*
           if        rr-con-max           =    zero
                     go to z-ric-acc-rec-364.
           move      zero                 to   rr-con-max             .
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9B"                 to   v-edm                  .
           move      12                   to   v-lin                  .
           move      53                   to   v-pos                  .
           move      rr-con-max           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       z-ric-acc-rec-364.
      *                          *-------------------------------------*
      *                          * Codice sottoconto iniziale          *
      *                          *-------------------------------------*
           if        rr-stc-min           =    zero
                     go to z-ric-acc-rec-366.
           move      zero                 to   rr-stc-min             .
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9B"                 to   v-edm                  .
           move      13                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-stc-min           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       z-ric-acc-rec-366.
      *                          *-------------------------------------*
      *                          * Codice sottoconto finale            *
      *                          *-------------------------------------*
           if        rr-stc-max           =    zero
                     go to z-ric-acc-rec-800.
           move      zero                 to   rr-stc-max             .
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9B"                 to   v-edm                  .
           move      13                   to   v-lin                  .
           move      53                   to   v-pos                  .
           move      rr-stc-max           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           go to     z-ric-acc-rec-800.
       z-ric-acc-rec-375.
      *                      *-----------------------------------------*
      *                      * Se premuta function-key 'DO' si esce    *
      *                      * dall'impostazione richieste             *
      *                      *-----------------------------------------*
           if        v-key                =    "DO  "
                     go to  z-ric-acc-rec-999.
       z-ric-acc-rec-400.
      *              *-------------------------------------------------*
      *              * Accettazione codice conto iniziale e finale     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Codice conto iniziale                       *
      *                  *---------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9B"                 to   v-edm                  .
           move      12                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "EXIT"               to   v-pfk(1)               .
           move      "FIND"               to   v-pfk(2)               .
           move      "DOWN"               to   v-pfk(3)               .
           move      "DO  "               to   v-pfk(4)               .
           move      "UP  "               to   v-pfk(5)               .
           move      rr-con-min           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-num                to   rr-con-min             .
      *                      *-----------------------------------------*
      *                      * Se premuta function-key 'EXIT' si esce  *
      *                      * con flag di uscita dal programma        *
      *                      *-----------------------------------------*
           if        v-key                =    "EXIT"
                     move   "#"           to   OK
                     go to  z-ric-acc-rec-999.
      *                      *-----------------------------------------*
      *                      * Se premuta function-key 'FIND' si ac-   *
      *                      * cede all'interrogazione sui conti con   *
      *                      * possibilita' di selezione codice conto  *
      *                      *-----------------------------------------*
           if        v-key                not  = "FIND"
                     go to  z-ric-acc-rec-450.
           move      spaces               to   OK                     .
           perform   fnd-cod-con-000      thru fnd-cod-con-999        .
           if        OK                   =    "R"
                     move   spaces        to   OK
                     go to  z-ric-acc-rec-400.
           move      w-mcs-cod-con        to   rr-con-min             .
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9B"                 to   v-edm                  .
           move      12                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-con-min           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       z-ric-acc-rec-450.
      *                      *-----------------------------------------*
      *                      * Check su congruenza tra valore minimo   *
      *                      * e massimo                               *
      *                      *-----------------------------------------*
           if        rr-con-max           not  = zero and
                     rr-con-max           <    rr-con-min
                     go to z-ric-acc-rec-400.
      *                      *-----------------------------------------*
      *                      * Se function-key 'UP' ritorna all'impo-  *
      *                      * stazione precedente                     *
      *                      *-----------------------------------------*
           if        v-key                =    "UP  "
                     go to  z-ric-acc-rec-300.
      *                      *-----------------------------------------*
      *                      * Se premuta function-key 'DO' si esce    *
      *                      * dall'impostazione richieste             *
      *                      *-----------------------------------------*
           if        v-key                =    "DO  "
                     go to  z-ric-acc-rec-999.
       z-ric-acc-rec-500.
      *                  *---------------------------------------------*
      *                  * Codice conto finale                         *
      *                  *---------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9B"                 to   v-edm                  .
           move      12                   to   v-lin                  .
           move      53                   to   v-pos                  .
           move      "EXIT"               to   v-pfk(1)               .
           move      "UP  "               to   v-pfk(2)               .
           move      "DOWN"               to   v-pfk(3)               .
           move      "DO  "               to   v-pfk(4)               .
           move      "FIND"               to   v-pfk(5)               .
           move      rr-con-max           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-num                to   rr-con-max             .
      *                      *-----------------------------------------*
      *                      * Se premuta function-key 'EXIT' si esce  *
      *                      * con flag di uscita dal programma        *
      *                      *-----------------------------------------*
           if        v-key                =    "EXIT"
                     move   "#"           to   OK
                     go to  z-ric-acc-rec-999.
      *                      *-----------------------------------------*
      *                      * Se premuta function-key 'FIND' si ac-   *
      *                      * cede all'interrogazione sui conti con   *
      *                      * possibilita' di selezione codice conto  *
      *                      *-----------------------------------------*
           if        v-key                not  = "FIND"
                     go to  z-ric-acc-rec-550.
           move      spaces               to   OK                     .
           perform   fnd-cod-con-000      thru fnd-cod-con-999        .
           if        OK                   =    "R"
                     move   spaces        to   OK
                     go to  z-ric-acc-rec-500.
           move      w-mcs-cod-con        to   rr-con-max             .
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9B"                 to   v-edm                  .
           move      12                   to   v-lin                  .
           move      53                   to   v-pos                  .
           move      rr-con-max           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       z-ric-acc-rec-550.
      *                      *-----------------------------------------*
      *                      * Check su congruenza tra valore minimo   *
      *                      * e massimo                               *
      *                      *-----------------------------------------*
           if        rr-con-max           not  = zero and
                     rr-con-max           <    rr-con-min
                     go to z-ric-acc-rec-500.
      *                      *-----------------------------------------*
      *                      * Se function-key 'UP' ritorna all'impo-  *
      *                      * stazione precedente                     *
      *                      *-----------------------------------------*
           if        v-key                =    "UP  "
                     go to  z-ric-acc-rec-400.
      *                      *-----------------------------------------*
      *                      * Se il codice conto minimo e il codice   *
      *                      * conto massimo sono entrambi a zero      *
      *                      * si pongono a zero tutti gli altri para- *
      *                      * metri e si passa alla richiesta con-    *
      *                      * ferma finale                            *
      *                      *-----------------------------------------*
           if        rr-con-min           =    zero and
                     rr-con-max           =    zero
                     go to z-ric-acc-rec-560.
      *                      *-----------------------------------------*
      *                      * Se il codice conto massimo e' zero si   *
      *                      * prosegue con le richieste, altrimenti   *
      *                      * si pongono a zero tutti gli altri para- *
      *                      * metri e si passa alla richiesta con-    *
      *                      * ferma finale                            *
      *                      *-----------------------------------------*
           if        rr-con-max           =    zero or
                     rr-con-max           =    rr-con-min
                     go to  z-ric-acc-rec-575.
       z-ric-acc-rec-560.
      *                          *-------------------------------------*
      *                          * Codice sottoconto iniziale          *
      *                          *-------------------------------------*
           if        rr-stc-min           =    zero
                     go to z-ric-acc-rec-562.
           move      zero                 to   rr-stc-min             .
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9B"                 to   v-edm                  .
           move      13                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-stc-min           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       z-ric-acc-rec-562.
      *                          *-------------------------------------*
      *                          * Codice sottoconto finale            *
      *                          *-------------------------------------*
           if        rr-stc-max           =    zero
                     go to z-ric-acc-rec-800.
           move      zero                 to   rr-stc-max             .
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9B"                 to   v-edm                  .
           move      13                   to   v-lin                  .
           move      53                   to   v-pos                  .
           move      rr-stc-max           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           go to     z-ric-acc-rec-800.
       z-ric-acc-rec-575.
      *                      *-----------------------------------------*
      *                      * Se premuta function-key 'DO' si esce    *
      *                      * dall'impostazione richieste             *
      *                      *-----------------------------------------*
           if        v-key                =    "DO  "
                     go to  z-ric-acc-rec-999.
       z-ric-acc-rec-600.
      *              *-------------------------------------------------*
      *              * Accettazione codice sottoconto iniziale e finale*
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Codice sottoconto iniziale                  *
      *                  *---------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9B"                 to   v-edm                  .
           move      13                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "EXIT"               to   v-pfk(1)               .
           move      "DOWN"               to   v-pfk(2)               .
           move      "DO  "               to   v-pfk(3)               .
           move      "UP  "               to   v-pfk(4)               .
           move      "FIND"               to   v-pfk(5)               .
           move      rr-stc-min           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-num                to   rr-stc-min             .
      *                      *-----------------------------------------*
      *                      * Se premuta function-key 'EXIT' si esce  *
      *                      * con flag di uscita dal programma        *
      *                      *-----------------------------------------*
           if        v-key                =    "EXIT"
                     move   "#"           to   OK
                     go to  z-ric-acc-rec-999.
      *                      *-----------------------------------------*
      *                      * Se premuta function-key 'FIND' si ac-   *
      *                      * cede all'interrogazione sui sottoconti  *
      *                      * con possibilita' di selezione codice    *
      *                      * sottoconto                              *
      *                      *-----------------------------------------*
           if        v-key                not  = "FIND"
                     go to  z-ric-acc-rec-650.
           move      spaces               to   OK                     .
           perform   fnd-arc-pdc-000      thru fnd-arc-pdc-999        .
           if        OK                   =    "R"
                     move   spaces        to   OK
                     go to  z-ric-acc-rec-600.
           move      w-mcs-cod-stc        to   rr-stc-min             .
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9B"                 to   v-edm                  .
           move      13                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-stc-min           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       z-ric-acc-rec-650.
      *                      *-----------------------------------------*
      *                      * Check su congruenza tra valore minimo   *
      *                      * e massimo                               *
      *                      *-----------------------------------------*
           if        rr-stc-max           not  = zero and
                     rr-stc-max           <    rr-stc-min
                     go to z-ric-acc-rec-600.
      *                      *-----------------------------------------*
      *                      * Se function-key 'UP' ritorna all'impo-  *
      *                      * stazione precedente                     *
      *                      *-----------------------------------------*
           if        v-key                =    "UP  "
                     go to  z-ric-acc-rec-500.
      *                      *-----------------------------------------*
      *                      * Se premuta function-key 'DO' si esce    *
      *                      * dall'impostazione richieste             *
      *                      *-----------------------------------------*
           if        v-key                =    "DO  "
                     go to  z-ric-acc-rec-999.
       z-ric-acc-rec-700.
      *                  *---------------------------------------------*
      *                  * Codice sottoconto finale                    *
      *                  *---------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9B"                 to   v-edm                  .
           move      13                   to   v-lin                  .
           move      53                   to   v-pos                  .
           move      "EXIT"               to   v-pfk(1)               .
           move      "UP  "               to   v-pfk(2)               .
           move      "DOWN"               to   v-pfk(3)               .
           move      "DO  "               to   v-pfk(4)               .
           move      "FIND"               to   v-pfk(5)               .
           move      rr-stc-max           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-num                to   rr-stc-max             .
      *                      *-----------------------------------------*
      *                      * Se premuta function-key 'EXIT' si esce  *
      *                      * con flag di uscita dal programma        *
      *                      *-----------------------------------------*
           if        v-key                =    "EXIT"
                     move   "#"           to   OK
                     go to  z-ric-acc-rec-999.
      *                      *-----------------------------------------*
      *                      * Se premuta function-key 'FIND' si ac-   *
      *                      * cede all'interrogazione sui sottoconti  *
      *                      * con possibilita' di selezione codice    *
      *                      * sottoconto                              *
      *                      *-----------------------------------------*
           if        v-key                not  = "FIND"
                     go to  z-ric-acc-rec-750.
           move      spaces               to   OK                     .
           perform   fnd-arc-pdc-000      thru fnd-arc-pdc-999        .
           if        OK                   =    "R"
                     move   spaces        to   OK
                     go to  z-ric-acc-rec-700.
           move      w-mcs-cod-stc        to   rr-stc-max             .
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9B"                 to   v-edm                  .
           move      13                   to   v-lin                  .
           move      53                   to   v-pos                  .
           move      rr-stc-max           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       z-ric-acc-rec-750.
      *                      *-----------------------------------------*
      *                      * Check su congruenza tra valore minimo   *
      *                      * e massimo                               *
      *                      *-----------------------------------------*
           if        rr-stc-max           not  = zero and
                     rr-stc-max           <    rr-stc-min
                     go to z-ric-acc-rec-700.
      *                      *-----------------------------------------*
      *                      * Se function-key 'UP' ritorna all'impo-  *
      *                      * stazione precedente                     *
      *                      *-----------------------------------------*
           if        v-key                =    "UP  "
                     go to  z-ric-acc-rec-600.
      *                      *-----------------------------------------*
      *                      * Se premuta function-key 'DO' si esce    *
      *                      * dall'impostazione richieste             *
      *                      *-----------------------------------------*
           if        v-key                =    "DO  "
                     go to  z-ric-acc-rec-999.
       z-ric-acc-rec-800.
      *                  *---------------------------------------------*
      *                  * Conferma impostazioni                       *
      *                  *---------------------------------------------*
           move      "MX"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      "#SAV"               to   v-not                  .
           move      "S"                  to   v-alf                  .
           move      "SNE"                to   v-msk                  .
           move      "DO  "               to   v-pfk (1)              .
           move      "UP  "               to   v-pfk (2)              .
           move      "EXIT"               to   v-pfk (3)              .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Test su risposta dell'utente                    *
      *              *-------------------------------------------------*
           if        v-key              =    spaces
                     if      v-alf          =    "S"
                             move  "DO  " to   v-key
                             go to z-ric-acc-rec-999
                     else if v-alf        =    "N"
                             go to z-ric-acc-rec-000
                     else    move  "EXIT" to   v-key
                             go to z-ric-acc-rec-999.
           if        v-key                =    "UP  "
                     if     rr-mas-max    not  = zero and
                            rr-mas-max    not  = rr-mas-min
                            go to z-ric-acc-rec-300
                     else if
                            rr-con-max    not  = zero and
                            rr-con-max    not  = rr-con-min
                            go to z-ric-acc-rec-500
                     else   go to z-ric-acc-rec-700.
       z-ric-acc-rec-999.
           exit.

      *----------------------------------------------------------------*
      *    Tasto funzione "FIND" su codice mastro                      *
      *----------------------------------------------------------------*
       fnd-cod-mas-000.
      *       *--------------------------------------------------------*
      *       * Test se programma di interrogazione gia' attivo        *
      *       *--------------------------------------------------------*
           move      "P?"                 to   s-ope                  .
           move      "pcge0110"           to   s-pro                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-liv                not  = zero
                     go to  fnd-cod-mas-999.
      *       *--------------------------------------------------------*
      *       * Preparazione variabile di i.p.c. per possibilita' di   *
      *       * function-key "SLCT" durante l'interrogazione           *
      *       *--------------------------------------------------------*
           move      "PV"                 to   s-ope                  .
           move      "fkselect"           to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      04                   to   s-car                  .
           move      "SLCT"               to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *       *--------------------------------------------------------*
      *       * Richiamo programma di interrogazione                   *
      *       *--------------------------------------------------------*
           move      "pgm/cge/prg/obj/pcge0110"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat                                            .
           cancel    s-pat                                            .
      *       *--------------------------------------------------------*
      *       * Estrazione di eventuale variabile di i.p.c. determi-   *
      *       * nata da function-key "SLCT" durante l'interrogazione   *
      *       *--------------------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "select mas"         to   s-var                  .
           move      "+"                  to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                not  = spaces
                     move   "R"           to   OK
           else      move   s-num         to   w-mcs-cod-mas          .
       fnd-cod-mas-999.
           exit.

      *----------------------------------------------------------------*
      *    Tasto funzione "FIND" su codice conto di contabiluita'      *
      *----------------------------------------------------------------*
       fnd-cod-con-000.
      *       *--------------------------------------------------------*
      *       * Test se programma di interrogazione gia' attivo        *
      *       *--------------------------------------------------------*
           move      "P?"                 to   s-ope                  .
           move      "pcge0210"           to   s-pro                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-liv                not  = zero
                     go to  fnd-cod-con-999.
      *       *--------------------------------------------------------*
      *       * Preparazione variabile di i.p.c. per possibilita' di   *
      *       * function-key "SLCT" durante l'interrogazione           *
      *       *--------------------------------------------------------*
           move      "PV"                 to   s-ope                  .
           move      "fkselect"           to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      04                   to   s-car                  .
           move      "SLCT"               to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *       *--------------------------------------------------------*
      *       * Preparazione variabile di i.p.c. per codice mastro da  *
      *       * interrogare                                            *
      *       *--------------------------------------------------------*
           if        w-prs-liv-pdc        not  =    3
                     go to fnd-cod-con-100.
           move      "PV"                 to   s-ope                  .
           move      "cod-mas"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "N"                  to   s-tip                  .
           move      02                   to   s-car                  .
           move      rr-mas-min           to   s-num                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
       fnd-cod-con-100.
      *       *--------------------------------------------------------*
      *       * Richiamo programma di interrogazione                   *
      *       *--------------------------------------------------------*
           move      "pgm/cge/prg/obj/pcge0210"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat                                            .
           cancel    s-pat                                            .
      *       *--------------------------------------------------------*
      *       * Estrazione di eventuale variabile di i.p.c. determi-   *
      *       * nata da function-key "SLCT" durante l'interrogazione:  *
      *       * se esistente il valore in essa contenuto viene depo-   *
      *       * sitato nel record layout come se fosse stato diret-    *
      *       * tamente impostato, altrimenti si setta il flag di      *
      *       * reimpostazione                                         *
      *       *--------------------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "select con"         to   s-var                  .
           move      "+"                  to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                not  = spaces
                     move   "R"           to   OK
           else      move   s-num         to   w-mcs-cod-con          .
       fnd-cod-con-999.
           exit.

      *----------------------------------------------------------------*
*     *    Tasto funzione "FIND" su archivio piano dei conti           *
*     *----------------------------------------------------------------*
       fnd-arc-pdc-000.
      *              *-------------------------------------------------*
      *              * Test se programma di interrogazione gia' attivo *
      *              *-------------------------------------------------*
           move      "P?"                 to   s-ope                  .
           move      "pcge2010"           to   s-pro                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-liv                not  = zero
                     go to  fnd-arc-pdc-999.
      *              *-------------------------------------------------*
      *              * Preparazione variabile i.p.c. per possibilita'  *
      *              * di function-key "SLCT" durante l'interrogazione *
      *              *-------------------------------------------------*
           move      "PV"                 to   s-ope                  .
           move      "fkselect"           to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      04                   to   s-car                  .
           move      "SLCT"               to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Preparazione variabile di i.p.c. per guidare    *
      *              * l'interrogazione sul mastro impostato           *
      *              *-------------------------------------------------*
           if        w-prs-liv-pdc        not  = 3
                     move   zero          to   s-num
           else      move   rr-mas-min    to   s-num                  .
           move      "PV"                 to   s-ope                  .
           move      "cod-mas"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "N"                  to   s-tip                  .
           move      02                   to   s-car                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Preparazione variabile di i.p.c. per guidare    *
      *              * l'interrogazione sul conto  impostato           *
      *              *-------------------------------------------------*
           move      "PV"                 to   s-ope                  .
           move      "cod-con"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "N"                  to   s-tip                  .
           move      02                   to   s-car                  .
           move      rr-con-min           to   s-num                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Richiamo programma di interrogazione            *
      *              *-------------------------------------------------*
           move      "pgm/cge/prg/obj/pcge2010"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat                                            .
           cancel    s-pat                                            .
      *              *-------------------------------------------------*
      *              * Estrazione di eventuale variabile di i.p.c.     *
      *              *-------------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "select pdc"         to   s-var                  .
           move      "+"                  to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                not  = spaces
                     move   "R"           to   OK
           else      move   s-num         to   w-mcs-cod-pdc          .
       fnd-arc-pdc-999.
           exit.

      *    *===========================================================*
      *    * Regolarizzazione record richieste                         *
      *    *-----------------------------------------------------------*
       z-ric-reg-rec-000.
      *              *-------------------------------------------------*
      *              * Codice mastro minimo e massimo                  *
      *              *-------------------------------------------------*
           if        w-prs-liv-pdc        not  = 3
                     move   zero          to   rr-mas-max
                                               rr-mas-min
                     go to  z-ric-reg-rec-100.
           if        rr-mas-min           =    zero and
                     rr-mas-max           =    zero
                     move   99            to   rr-mas-max
           else if   rr-mas-min           not  = zero and
                     rr-mas-max           =    zero
                     move  rr-mas-min     to   rr-mas-max             .
       z-ric-reg-rec-100.
      *              *-------------------------------------------------*
      *              * Codice conto minimo e massimo                   *
      *              *-------------------------------------------------*
           if        rr-con-min           =    zero and
                     rr-con-max           =    zero
                     move   99            to   rr-con-max
           else if   rr-con-min           not  = zero and
                     rr-con-max           =    zero
                     move  rr-con-min     to   rr-con-max             .
      *              *-------------------------------------------------*
      *              * Codice sottoconto minimo e massimo              *
      *              *-------------------------------------------------*
           if        rr-stc-min           =    zero and
                     rr-stc-max           =    zero
                     move   999           to   rr-stc-max
           else if   rr-stc-min           not  = zero and
                     rr-stc-max           =    zero
                     move  rr-stc-min     to   rr-stc-max             .
       z-ric-reg-rec-999.
           exit.
