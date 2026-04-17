       Identification Division.
       Program-Id.                                 pbol3400           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    bol                 *
      *                                Settore:    com                 *
      *                                   Fase:    bol340              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 13/02/96    *
      *                       Ultima revisione:    NdK del 10/04/06    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Stampe etichette segnacolli a fronte bolle  *
      *                                                                *
      *                    Main                                        *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * La fase bol340, nella sua globalita', esegue i tipi di stampa  *
      * elencati nella tabella seguente :                              *
      *                                                                *
      *                                                                *
      *   Codice                                                       *
      *    tipo                                                        *
      *  interrog.        Descrizione per il tipo interrogazione       *
      * ----------  -------------------------------------------------- *
      *                                                                *
      * ETICLI      Stampa etichette segnacolli clienti       pbol340a *
      *                                                                *
      * ETIFNT      Stampa etichette segnacolli fornitori     pbol340b *
      *                                                                *
      * ETIDPZ      Stampa etichette segnacolli dipendenze    pbol340d *
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
                     "com"                                            .
      *        *-------------------------------------------------------*
      *        * Fase gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-fas                  pic  x(06) value
                     "bol340"                                         .
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10) value
                     "pbol3400"                                       .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  i-ide-des                  pic  x(40) value
                     "  STAMPE ETICHETTE DI SPEDIZIONE COLLI  "       .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "mvideo" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/v"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "msegrt"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/s"                                  .

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

      *    *===========================================================*
      *    * Area di interfaccia per sottoprogramma         "pazi000d" *
      *    *-----------------------------------------------------------*
           copy      "pgm/azi/prg/cpy/pazi000d.pgl"                   .

      *    *===========================================================*
      *    * Work-area per tipi interrogazione                         *
      *    *-----------------------------------------------------------*
       01  w-tin.
      *        *-------------------------------------------------------*
      *        * Tabella tipi interrogazione e dati ad essi associati  *
      *        *-------------------------------------------------------*
           05  w-tin-tbl-tin.
      *            *---------------------------------------------------*
      *            * Indice per puntamento su elemento in tabella ed   *
      *            * altri indici di comodo                            *
      *            *---------------------------------------------------*
               10  w-tin-ele-inx          pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Comodo per caricamento iniziale elementi          *
      *            *---------------------------------------------------*
               10  w-tin-ele-wci.
                   15  w-tin-ele-wci-des  pic  x(60)                  .
                   15  w-tin-ele-wci-ast  pic  x(01)                  .
                   15  w-tin-ele-wci-alf  pic  x(10)                  .
                   15  filler             pic  x(01)                  .
                   15  w-tin-ele-wci-ovy  pic  x(10)                  .
      *            *---------------------------------------------------*
      *            * Numero effettivo di elementi in tabella           *
      *            *---------------------------------------------------*
               10  w-tin-ele-num          pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Numero di elementi selezionabili per impostazio-  *
      *            * ne, cioe' escludendo quelli finali asteriscati    *
      *            *---------------------------------------------------*
               10  w-tin-ele-nes          pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Numero di elementi per pagina                     *
      *            *---------------------------------------------------*
               10  w-tin-ele-nep          pic  9(03)       value 14   .
      *            *---------------------------------------------------*
      *            * Numero di pagine totali con elementi selezionabi- *
      *            * li                                                *
      *            *---------------------------------------------------*
               10  w-tin-ele-npt          pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Numero pagina attualmente visualizzata            *
      *            *---------------------------------------------------*
               10  w-tin-ele-pag          pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Max numero di elementi in tabella                 *
      *            *---------------------------------------------------*
               10  w-tin-ele-max          pic  9(03)       value 50   .
      *            *---------------------------------------------------*
      *            * Elementi per tipi interrogazione                  *
      *            *---------------------------------------------------*
               10  w-tin-ele-tin occurs 50.
      *                *-----------------------------------------------*
      *                * Codice numerico tipo interrogazione           *
      *                *-----------------------------------------------*
                   15  w-tin-num-tin      pic  9(02)                  .
      *                *-----------------------------------------------*
      *                * Sigla alfanumerica tipo interrogazione        *
      *                *-----------------------------------------------*
                   15  w-tin-alf-tin      pic  x(10)                  .
      *                *-----------------------------------------------*
      *                * Descrizione tipo interrogazione               *
      *                *-----------------------------------------------*
                   15  w-tin-des-tin      pic  x(60)                  .
      *                *-----------------------------------------------*
      *                * Overlay da richiamare per il tipo di interro- *
      *                * gazione                                       *
      *                *-----------------------------------------------*
                   15  w-tin-ovy-tin      pic  x(10)                  .

      *    *===========================================================*
      *    * Work-area per visualizzazione pagina attuale              *
      *    *-----------------------------------------------------------*
       01  w-vis-pag.
      *        *-------------------------------------------------------*
      *        * Numero linea video per il numero pagina               *
      *        *-------------------------------------------------------*
           05  w-vis-pag-lin-000          pic  9(02)       value 06   .
      *        *-------------------------------------------------------*
      *        * Si/No pagina precedente alla pagina attuale           *
      *        *-------------------------------------------------------*
           05  w-vis-pag-snx-pre          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Si/No pagina seguente alla pagina attuale             *
      *        *-------------------------------------------------------*
           05  w-vis-pag-snx-seg          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Indice primo elemento visualizzato nella pagina at-   *
      *        * tuale                                                 *
      *        *-------------------------------------------------------*
           05  w-vis-pag-inx-pel          pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Indice ultimo elemento visualizzato nella pagina at-  *
      *        * tuale                                                 *
      *        *-------------------------------------------------------*
           05  w-vis-pag-inx-uel          pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Indice per scansione su elementi da visualizzare      *
      *        *-------------------------------------------------------*
           05  w-vis-pag-inx-tbl          pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per determinazione linea a video per elemento  *
      *        * in corso di trattamento                               *
      *        *-------------------------------------------------------*
           05  w-vis-pag-lin-acv          pic  9(03)                  .

      *    *===========================================================*
      *    * Work-area per richiamo overlay per l'esecuzione effettiva *
      *    * del tipo di interrogazione                                *
      *    *-----------------------------------------------------------*
       01  w-ovy-exe.
      *        *-------------------------------------------------------*
      *        * Pathname per il richiamo della overlay                *
      *        *-------------------------------------------------------*
           05  w-ovy-exe-pat.
      *            *---------------------------------------------------*
      *            * Prefisso comune                                   *
      *            *---------------------------------------------------*
               10  w-ovy-exe-pre          pic  x(16)       value
                     "pgm/bol/prg/obj/"                               .
      *            *---------------------------------------------------*
      *            * Postfisso variabile                               *
      *            *---------------------------------------------------*
               10  w-ovy-exe-pos          pic  x(10)                  .
      *        *-------------------------------------------------------*
      *        * Pathname filtrato                                     *
      *        *-------------------------------------------------------*
           05  w-ovy-exe-flt              pic  x(40)                  .

      *    *===========================================================*
      *    * Work-area per bufferizzazione campi accettazione campi    *
      *    * chiave trattati dal main                                  *
      *    *-----------------------------------------------------------*
       01  w-key.
      *        *-------------------------------------------------------*
      *        * Codice numerico tipo interrogazione                   *
      *        *-------------------------------------------------------*
           05  w-key-num-int              pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Codice alfanumerico tipo interrogazione               *
      *        *-------------------------------------------------------*
           05  w-key-alf-int              pic  x(10)                  .
      *        *-------------------------------------------------------*
      *        * Descrizione per codice tipo interrogazione            *
      *        *-------------------------------------------------------*
           05  w-key-des-int              pic  x(60)                  .
      *        *-------------------------------------------------------*
      *        * Overlay per il tipo interrogazione                    *
      *        *-------------------------------------------------------*
           05  w-key-ovy-int              pic  x(10)                  .

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
      *              * Esecuzione ciclo per la fase 'bol340'           *
      *              *-------------------------------------------------*
           perform   fas-bol-340-000      thru fas-bol-340-999        .
       main-900.
      *              *-------------------------------------------------*
      *              * Esecuzione routine post-esecuzione programma    *
      *              *-------------------------------------------------*
           perform   pos-exe-pgm-000      thru pos-exe-pgm-999        .
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
      *    * Routine pre-esecuzione programma                          *
      *    *-----------------------------------------------------------*
       pre-exe-pgm-000.
      *              *-------------------------------------------------*
      *              * Flag di uscita a spaces per continuare la nor-  *
      *              * male esecuzione del programma                   *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-pre-exe-pgm      .
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
           cancel    "pgm/azi/prg/obj/pazi000d"                       .
      *              *-------------------------------------------------*
      *              * Se zero dipendenze : errore ed uscita           *
      *              *-------------------------------------------------*
           if        w-dpz-ctr-dpz        >    zero
                     go to pre-exe-pgm-020.
           move      "EN"                 to   w-dpz-tip-ope          .
           call      "pgm/azi/prg/obj/pazi000d"
                                         using w-dpz                  .
           cancel    "pgm/azi/prg/obj/pazi000d"                       .
           move      "#"                  to   w-cnt-pre-exe-pgm      .
           go to     pre-exe-pgm-999.
       pre-exe-pgm-020.
      *              *-------------------------------------------------*
      *              * Selezione codice dipendenza per il programma    *
      *              *-------------------------------------------------*
           move      "SD"                 to   w-dpz-tip-ope          .
           call      "pgm/azi/prg/obj/pazi000d"
                                         using w-dpz                  .
           cancel    "pgm/azi/prg/obj/pazi000d"                       .
       pre-exe-pgm-040.
      *              *-------------------------------------------------*
      *              * Se scelta non effettuata : uscita               *
      *              *-------------------------------------------------*
           if        w-dpz-cod-prg        =    zero
                     move  "#"            to   w-cnt-pre-exe-pgm
                     go to pre-exe-pgm-999.
       pre-exe-pgm-080.
      *              *-------------------------------------------------*
      *              * Caricamento iniziale della tabella dei tipi in- *
      *              * terrogazione                                    *
      *              *-------------------------------------------------*
           perform   loa-tbl-tin-000      thru loa-tbl-tin-999        .
       pre-exe-pgm-100.
      *              *-------------------------------------------------*
      *              * Se esiste un solo elemento selezionabile per    *
      *              * impostazione si forza l'esecuzione del sotto-   *
      *              * programma relativo                              *
      *              *-------------------------------------------------*
           if        w-tin-ele-nes        =    1
                     move  1              to   w-tin-ele-inx
                     go to pre-exe-pgm-200.
      *              *-------------------------------------------------*
      *              * Altrimenti si esce                              *
      *              *-------------------------------------------------*
           go to     pre-exe-pgm-999.
       pre-exe-pgm-200.
      *              *-------------------------------------------------*
      *              * Forzatura per l'esecuzione dell'unico sotto-    *
      *              * programma previsto                              *
      *              *-------------------------------------------------*
       pre-exe-pgm-210.
      *                  *---------------------------------------------*
      *                  * Simulazione impostazione valori             *
      *                  *---------------------------------------------*
           move      w-tin-ele-inx        to   w-key-num-int          .
           move      w-tin-alf-tin
                    (w-tin-ele-inx)       to   w-key-alf-int          .
           move      w-tin-des-tin
                    (w-tin-ele-inx)       to   w-key-des-int          .
           move      w-tin-ovy-tin
                    (w-tin-ele-inx)       to   w-key-ovy-int          .
      *                  *---------------------------------------------*
      *                  * Memorizzazione del nome della overlay per   *
      *                  * formare il pathname completo per il ri-     *
      *                  * chiamo del programma di esecuzione          *
      *                  *---------------------------------------------*
           move      w-tin-ovy-tin
                    (w-tin-ele-inx)       to   w-ovy-exe-pos          .
       pre-exe-pgm-220.
      *                  *---------------------------------------------*
      *                  * Richiamo del programma di esecuzione        *
      *                  *---------------------------------------------*
           perform   ric-pgm-exe-000      thru ric-pgm-exe-999        .
       pre-exe-pgm-230.
      *                  *---------------------------------------------*
      *                  * Flag di uscita a non-spaces per interrompe- *
      *                  * re la normale esecuzione del programma e    *
      *                  * ritornare al chiamante                      *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cnt-pre-exe-pgm      .
       pre-exe-pgm-240.
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     pre-exe-pgm-999.
       pre-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Routine post-esecuzione programma                         *
      *    *-----------------------------------------------------------*
       pos-exe-pgm-000.
       pos-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Caricamento iniziale della tabella dei tipi interrogazio- *
      *    * ne                                                        *
      *    *-----------------------------------------------------------*
       loa-tbl-tin-000.
      *              *-------------------------------------------------*
      *              * Azzeramento preliminare numero effettivo di e-  *
      *              * lementi in tabella                              *
      *              *-------------------------------------------------*
           move      zero                 to   w-tin-ele-num          .
      *              *-------------------------------------------------*
      *              * Azzeramento preliminare numero di elementi se-  *
      *              * lezionabili per impostazione, esclusi quindi    *
      *              * quelli finali asteriscati                       *
      *              *-------------------------------------------------*
           move      zero                 to   w-tin-ele-nes          .
       loa-tbl-tin-100.
      *              *-------------------------------------------------*
      *              * Caricamento elementi in tabella con incremento  *
      *              * del numero effettivo di elementi in tabella ed  *
      *              * eventualmente del numero di elementi finali a-  *
      *              * steriscati e quindi non selezionabili per impo- *
      *              * stazione                                        *
      *              *-------------------------------------------------*
      *
           move      "Stampa etichette segnacolli clienti               
      -              "           ETICLI     pbol340a  "
                                          to   w-tin-ele-wci          .
           perform   loa-tbl-ele-000      thru loa-tbl-ele-999        .
      *       
           move      "Stampa etichette segnacolli fornitori             
      -              "           ETIFNT     pbol340b  "
                                          to   w-tin-ele-wci          .
           perform   loa-tbl-ele-000      thru loa-tbl-ele-999        .
      *
           if        w-dpz-ctr-dpz        not  > 1
                     go to loa-tbl-tin-200.
           move      "Stampa etichette segnacolli dipendenze            
      -              "           ETIDPZ     pbol340d  "
                                          to   w-tin-ele-wci          .
           perform   loa-tbl-ele-000      thru loa-tbl-ele-999        .

       loa-tbl-tin-200.
      *              *-------------------------------------------------*
      *              * Normalizzazione elementi residui                *
      *              *-------------------------------------------------*
           move      w-tin-ele-num        to   w-tin-ele-inx          .
       loa-tbl-tin-225.
           add       1                    to   w-tin-ele-inx          .
           if        w-tin-ele-inx        >    w-tin-ele-max
                     go to loa-tbl-tin-300.
           move      zero                 to   w-tin-num-tin
                                              (w-tin-ele-inx)         .
           move      spaces               to   w-tin-alf-tin
                                              (w-tin-ele-inx)         .
           move      spaces               to   w-tin-des-tin
                                              (w-tin-ele-inx)         .
           move      spaces               to   w-tin-ovy-tin
                                              (w-tin-ele-inx)         .
           go to     loa-tbl-tin-225.
       loa-tbl-tin-300.
      *              *-------------------------------------------------*
      *              * Determinazione numero pagine totali             *
      *              *-------------------------------------------------*
           move      w-tin-ele-nes        to   w-tin-ele-npt          .
           add       w-tin-ele-nep        to   w-tin-ele-npt          .
           subtract  1                    from w-tin-ele-npt          .
           divide    w-tin-ele-nep        into w-tin-ele-npt          .
       loa-tbl-tin-400.
      *              *-------------------------------------------------*
      *              * Numero pagina attualmente visualizzata a : 1    *
      *              *-------------------------------------------------*
           move      1                    to   w-tin-ele-pag          .
       loa-tbl-tin-999.
           exit.

      *    *===========================================================*
      *    * Caricamento elemento in tabella tipi interrogazione       *
      *    *-----------------------------------------------------------*
       loa-tbl-ele-000.
      *              *-------------------------------------------------*
      *              * Se tabella gia' satura : uscita                 *
      *              *-------------------------------------------------*
           if        w-tin-ele-num        =    w-tin-ele-max
                     go to loa-tbl-ele-999.
      *              *-------------------------------------------------*
      *              * Incremento numero effettivo di elementi in ta-  *
      *              * bella                                           *
      *              *-------------------------------------------------*
           add       1                    to   w-tin-ele-num          .
      *              *-------------------------------------------------*
      *              * Incremento se e' il caso il numero di elementi  *
      *              * selezionabili per impostazione, escluso quindi  *
      *              * quelli finali asteriscati                       *
      *              *-------------------------------------------------*
           if        w-tin-ele-wci-ast    =    spaces
                     add   1              to   w-tin-ele-nes          .
      *              *-------------------------------------------------*
      *              * Bufferizzazione codice numerico tipo interroga- *
      *              * zione                                           *
      *              *-------------------------------------------------*
           move      w-tin-ele-num        to   w-tin-num-tin
                                              (w-tin-ele-num)         .
      *              *-------------------------------------------------*
      *              * Bufferizzazione codice alfanumerico tipo inter- *
      *              * rogazione                                       *
      *              *-------------------------------------------------*
           move      w-tin-ele-wci-alf    to   w-tin-alf-tin
                                              (w-tin-ele-num)         .
      *              *-------------------------------------------------*
      *              * Bufferizzazione descrizione tipo interrogazione *
      *              *-------------------------------------------------*
           move      w-tin-ele-wci-des    to   w-tin-des-tin
                                              (w-tin-ele-num)         .
      *              *-------------------------------------------------*
      *              * Bufferizzazione overlay da richiamare per il    *
      *              * tipo interrogazione                             *
      *              *-------------------------------------------------*
           move      w-tin-ele-wci-ovy    to   w-tin-ovy-tin
                                              (w-tin-ele-num)         .
       loa-tbl-ele-999.
           exit.

      *    *===========================================================*
      *    * Esecuzione ciclo per la fase 'bol340'                     *
      *    *-----------------------------------------------------------*
       fas-bol-340-000.
      *              *-------------------------------------------------*
      *              * Video in Off                                    *
      *              *-------------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       fas-bol-340-025.
      *              *-------------------------------------------------*
      *              * Visualizzazione titolo programma                *
      *              *-------------------------------------------------*
           perform   vis-tit-pgm-000      thru vis-tit-pgm-999        .
       fas-bol-340-050.
      *              *-------------------------------------------------*
      *              * Normalizzazione iniziale valori di accettazione *
      *              *-------------------------------------------------*
      *                   *--------------------------------------------*
      *                   * Codice numerico per il tipo interrogazione *
      *                   *--------------------------------------------*
           move      zero                 to   w-key-num-int          .
      *                   *--------------------------------------------*
      *                   * Codice alfanumerico per il tipo interroga- *
      *                   * zione                                      *
      *                   *--------------------------------------------*
           move      spaces               to   w-key-alf-int          .
      *                   *--------------------------------------------*
      *                   * Descrizione per il tipo interrogazione     *
      *                   *--------------------------------------------*
           move      spaces               to   w-key-des-int          .
      *                   *--------------------------------------------*
      *                   * Nome overlay per il tipo interrogazione    *
      *                   *--------------------------------------------*
           move      spaces               to   w-key-ovy-int          .
       fas-bol-340-075.
      *              *-------------------------------------------------*
      *              * Prompts per impostazione tipo interrogazione    *
      *              *-------------------------------------------------*
       fas-bol-340-077.
      *                  *---------------------------------------------*
      *                  * Prompt effettivo per tipo interrogazione    *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      16                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tipo di stampa :"   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       fas-bol-340-079.
      *                  *---------------------------------------------*
      *                  * Visualizzazione codice numerico per tipo    *
      *                  * interrogazione                              *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      04                   to   v-lin                  .
           move      18                   to   v-pos                  .
           move      w-key-num-int        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       fas-bol-340-081.
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione per tipo inter- *
      *                  * rogazione                                   *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      60                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      21                   to   v-pos                  .
           move      w-key-des-int        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       fas-bol-340-083.
      *                  *---------------------------------------------*
      *                  * Visualizzazione trattini di separazione     *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      all   "-"            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       fas-bol-340-100.
      *              *-------------------------------------------------*
      *              * Visualizzazione pagina attuale                  *
      *              *-------------------------------------------------*
           perform   vis-pag-att-000      thru vis-pag-att-999        .
       fas-bol-340-125.
      *              *-------------------------------------------------*
      *              * Video in On                                     *
      *              *-------------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       fas-bol-340-200.
      *              *-------------------------------------------------*
      *              * Accettazione codice numerico tipo interroga-    *
      *              * zione                                           *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      04                   to   v-lin                  .
           move      18                   to   v-pos                  .
           if        w-tin-ele-num        >    zero
                     move  "DOWN"         to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           if        w-vis-pag-snx-pre    =    "S"
                     move  "PRSC"         to   v-pfk (07)             .
           if        w-vis-pag-snx-seg    =    "S"
                     move  "NXSC"         to   v-pfk (08)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      w-key-num-int        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Deviazione a seconda del tasto di funzione usa- *
      *              * to                                              *
      *              *-------------------------------------------------*
           if        v-key                =    spaces or
                     v-key                =    "DO  "
                     go to fas-bol-340-300
           else if   v-key                =    "PRSC"
                     go to fas-bol-340-400
           else if   v-key                =    "NXSC"
                     go to fas-bol-340-450
           else if   v-key                =    "DOWN"
                     go to fas-bol-340-500
           else if   v-key                =    "EXIT"
                     go to fas-bol-340-250
           else      go to fas-bol-340-200.
       fas-bol-340-250.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     fas-bol-340-999.
       fas-bol-340-300.
      *              *-------------------------------------------------*
      *              * Se Return o Do                                  *
      *              *-------------------------------------------------*
       fas-bol-340-310.
      *                  *---------------------------------------------*
      *                  * Valore impostato in campo di destinazione   *
      *                  *---------------------------------------------*
           move      v-num                to   w-key-num-int          .
       fas-bol-340-320.
      *                  *---------------------------------------------*
      *                  * Ricerca codice numerico impostato in ta-    *
      *                  * bella dei tipi interrogazione, e deviazione *
      *                  * a seconda dell'esito della ricerca          *
      *                  *---------------------------------------------*
           if        w-key-num-int        =    zero
                     go to fas-bol-340-330.
           move      zero                 to   w-tin-ele-inx          .
       fas-bol-340-322.
           add       1                    to   w-tin-ele-inx          .
           if        w-tin-ele-inx        >    w-tin-ele-nes
                     go to fas-bol-340-340.
           if        w-tin-num-tin
                    (w-tin-ele-inx)       =    w-key-num-int
                     go to fas-bol-340-350
           else      go to fas-bol-340-322.
       fas-bol-340-330.
      *                  *---------------------------------------------*
      *                  * Se codice numerico impostato : a zero       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Codice alfanumerico per il tipo inter-  *
      *                      * rogazione a spaces                      *
      *                      *-----------------------------------------*
           move      spaces               to   w-key-alf-int          .
      *                      *-----------------------------------------*
      *                      * Descrizione per il tipo interrogazione  *
      *                      * a spaces                                *
      *                      *-----------------------------------------*
           move      spaces               to   w-key-des-int          .
      *                      *-----------------------------------------*
      *                      * Nome overlay per il tipo interrogazione *
      *                      * a spaces                                *
      *                      *-----------------------------------------*
           move      spaces               to   w-key-ovy-int          .
      *                      *-----------------------------------------*
      *                      * Visualizzazione descrizione per il tipo *
      *                      * interrogazione                          *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      60                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      21                   to   v-pos                  .
           move      w-key-des-int        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Riciclo ad impostazione del codice nu-  *
      *                      * merico per il tipo di interrogazione    *
      *                      *-----------------------------------------*
           go to     fas-bol-340-200.
       fas-bol-340-340.
      *                  *---------------------------------------------*
      *                  * Se codice numerico impostato non esistente  *
      *                  * in tabella dei tipi interrogazione          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Codice alfanumerico per il tipo inter-  *
      *                      * rogazione a spaces                      *
      *                      *-----------------------------------------*
           move      spaces               to   w-key-alf-int          .
      *                      *-----------------------------------------*
      *                      * Descrizione per il tipo interrogazione  *
      *                      * a puntini                               *
      *                      *-----------------------------------------*
           move      all   "."            to   w-key-des-int          .
      *                      *-----------------------------------------*
      *                      * Nome overlay per il tipo interrogazione *
      *                      * a spaces                                *
      *                      *-----------------------------------------*
           move      spaces               to   w-key-ovy-int          .
      *                      *-----------------------------------------*
      *                      * Visualizzazione descrizione per il tipo *
      *                      * interrogazione                          *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      60                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      21                   to   v-pos                  .
           move      w-key-des-int        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Riciclo ad impostazione del codice nu-  *
      *                      * merico per il tipo di interrogazione    *
      *                      *-----------------------------------------*
           go to     fas-bol-340-200.
       fas-bol-340-350.
      *                  *---------------------------------------------*
      *                  * Se codice numerico impostato trovato nella  *
      *                  * tabella dei tipi interrogazione             *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Codice alfanumerico per il tipo di in-  *
      *                      * terrogazione                            *
      *                      *-----------------------------------------*
           move      w-tin-alf-tin
                    (w-tin-ele-inx)       to   w-key-alf-int          .
      *                      *-----------------------------------------*
      *                      * Descrizione per il tipo di interroga-   *
      *                      * zione                                   *
      *                      *-----------------------------------------*
           move      w-tin-des-tin
                    (w-tin-ele-inx)       to   w-key-des-int          .
      *                      *-----------------------------------------*
      *                      * Nome overlay per il tipo di interroga-  *
      *                      * zione                                   *
      *                      *-----------------------------------------*
           move      w-tin-ovy-tin
                    (w-tin-ele-inx)       to   w-key-ovy-int          .
      *                      *-----------------------------------------*
      *                      * Visualizzazione descrizione per il tipo *
      *                      * interrogazione                          *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      60                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      21                   to   v-pos                  .
           move      w-key-des-int        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Memorizzazione del nome della overlay   *
      *                      * per formare il pathname completo per    *
      *                      * il richiamo del programma di esecuzione *
      *                      *-----------------------------------------*
           move      w-key-ovy-int        to   w-ovy-exe-pos          .
      *                      *-----------------------------------------*
      *                      * Se il nome della overlay e' a spaces :  *
      *                      * si ricicla ad accettazione del codice   *
      *                      * numerico per il tipo di interrogazione  *
      *                      *-----------------------------------------*
           if        w-ovy-exe-pos        =    spaces
                     go to fas-bol-340-200.
      *                      *-----------------------------------------*
      *                      * Altrimenti si va' al richiamo effettivo *
      *                      * del programma di esecuzione             *
      *                      *-----------------------------------------*
           go to     fas-bol-340-700.
       fas-bol-340-400.
      *              *-------------------------------------------------*
      *              * Se Prsc                                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Video in Off                                *
      *                  *---------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Decremento numero pagina attualmente visua- *
      *                  * lizzata                                     *
      *                  *---------------------------------------------*
           subtract  1                    from w-tin-ele-pag          .
      *                  *---------------------------------------------*
      *                  * Visualizzazione pagina attuale              *
      *                  *---------------------------------------------*
           perform   vis-pag-att-000      thru vis-pag-att-999        .
      *                  *---------------------------------------------*
      *                  * Video in On                                 *
      *                  *---------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Riciclo ad accettazione codice numerico per *
      *                  * il tipo di interrogazione                   *
      *                  *---------------------------------------------*
           go to     fas-bol-340-200.
       fas-bol-340-450.
      *              *-------------------------------------------------*
      *              * Se Nxsc                                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Video in Off                                *
      *                  *---------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Incremento numero pagina attualmente visua- *
      *                  * lizzata                                     *
      *                  *---------------------------------------------*
           add       1                    to   w-tin-ele-pag          .
      *                  *---------------------------------------------*
      *                  * Visualizzazione pagina attuale              *
      *                  *---------------------------------------------*
           perform   vis-pag-att-000      thru vis-pag-att-999        .
      *                  *---------------------------------------------*
      *                  * Video in On                                 *
      *                  *---------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Riciclo ad accettazione codice numerico per *
      *                  * il tipo di interrogazione                   *
      *                  *---------------------------------------------*
           go to     fas-bol-340-200.
       fas-bol-340-500.
      *              *-------------------------------------------------*
      *              * Se Down                                         *
      *              *-------------------------------------------------*
       fas-bol-340-525.
      *                  *---------------------------------------------*
      *                  * Normalizzazione e visualizzazione valori di *
      *                  * accettazione                                *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Codice numerico per il tipo interroga-  *
      *                      * zione a zero                            *
      *                      *-----------------------------------------*
           move      zero                 to   w-key-num-int          .
      *                      *-----------------------------------------*
      *                      * Codice alfanumerico per il tipo inter-  *
      *                      * rogazione a spaces                      *
      *                      *-----------------------------------------*
           move      spaces               to   w-key-alf-int          .
      *                      *-----------------------------------------*
      *                      * Descrizione per il tipo interrogazione  *
      *                      * a spaces                                *
      *                      *-----------------------------------------*
           move      spaces               to   w-key-des-int          .
      *                      *-----------------------------------------*
      *                      * Nome overlay per il tipo interrogazione *
      *                      * a spaces                                *
      *                      *-----------------------------------------*
           move      spaces               to   w-key-ovy-int          .
      *                      *-----------------------------------------*
      *                      * Visualizzazione codice numerico per il  *
      *                      * tipo interrogazione                     *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      04                   to   v-lin                  .
           move      18                   to   v-pos                  .
           move      w-key-num-int        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Visualizzazione descrizione per il tipo *
      *                      * interrogazione                          *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      60                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      21                   to   v-pos                  .
           move      w-key-des-int        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       fas-bol-340-550.
      *                  *---------------------------------------------*
      *                  * Accettazione direttamente da corpo video    *
      *                  *---------------------------------------------*
       fas-bol-340-555.
      *                      *-----------------------------------------*
      *                      * Inizializzazione indice per scansione   *
      *                      * su elementi visualizzati                *
      *                      *-----------------------------------------*
           move      w-vis-pag-inx-pel    to   w-vis-pag-inx-tbl      .
       fas-bol-340-560.
      *                      *-----------------------------------------*
      *                      * Determinazione linea a video per ele-   *
      *                      * mento in esame                          *
      *                      *-----------------------------------------*
           move      w-vis-pag-inx-tbl    to   w-vis-pag-lin-acv      .
           subtract  w-vis-pag-inx-pel    from w-vis-pag-lin-acv      .
           add       w-vis-pag-lin-000    to   w-vis-pag-lin-acv      .
           add       1                    to   w-vis-pag-lin-acv      .
       fas-bol-340-570.
      *                      *-----------------------------------------*
      *                      * Accettazione tasto di funzione alla li- *
      *                      * nea corrispondente all'elemento in esa- *
      *                      * me                                      *
      *                      *-----------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "K"                  to   v-tip                  .
           move      w-vis-pag-lin-acv    to   v-lin                  .
           move      17                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           if        w-vis-pag-inx-tbl    <    w-tin-ele-nes
                     move  "DOWN"         to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           if        w-vis-pag-snx-pre    =    "S"
                     move  "PRSC"         to   v-pfk (07)             .
           if        w-vis-pag-snx-seg    =    "S"
                     move  "NXSC"         to   v-pfk (08)             .
           move      "SLCT"               to   v-pfk (10)             .
           move      "EXIT"               to   v-pfk (20)             .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       fas-bol-340-580.
      *                      *-----------------------------------------*
      *                      * Deviazione a seconda del tasto di fun-  *
      *                      * zione usato                             *
      *                      *-----------------------------------------*
           if        v-key                =    "DOWN"
                     go to fas-bol-340-630
           else if   v-key                =    "PRSC"
                     go to fas-bol-340-600
           else if   v-key                =    "NXSC"
                     go to fas-bol-340-610
           else if   v-key                =    "UP  "
                     go to fas-bol-340-620
           else if   v-key                =    spaces or
                     v-key                =    "DO  " or
                     v-key                =    "SLCT"
                     go to fas-bol-340-650
           else if   v-key                =    "EXIT"
                     go to fas-bol-340-660
           else      go to fas-bol-340-570.
       fas-bol-340-600.
      *                      *-----------------------------------------*
      *                      * Se Prsc                                 *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Video in Off                        *
      *                          *-------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                          *-------------------------------------*
      *                          * Decremento numero pagina attualmen- *
      *                          * te visualizzata                     *
      *                          *-------------------------------------*
           subtract  1                    from w-tin-ele-pag          .
      *                          *-------------------------------------*
      *                          * Visualizzazione pagina attuale      *
      *                          *-------------------------------------*
           perform   vis-pag-att-000      thru vis-pag-att-999        .
      *                          *-------------------------------------*
      *                          * Indice di scansione al primo ele-   *
      *                          * mento della pagina                  *
      *                          *-------------------------------------*
           move      w-vis-pag-inx-pel    to   w-vis-pag-inx-tbl      .
      *                          *-------------------------------------*
      *                          * Video in On                         *
      *                          *-------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                          *-------------------------------------*
      *                          * Riciclo ad accettazione tasto di    *
      *                          * funzione                            *
      *                          *-------------------------------------*
           go to     fas-bol-340-560.
       fas-bol-340-610.
      *                      *-----------------------------------------*
      *                      * Se Nxsc                                 *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Video in Off                        *
      *                          *-------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                          *-------------------------------------*
      *                          * Incremento numero pagina attualmen- *
      *                          * te visualizzata                     *
      *                          *-------------------------------------*
           add       1                    to   w-tin-ele-pag          .
      *                          *-------------------------------------*
      *                          * Visualizzazione pagina attuale      *
      *                          *-------------------------------------*
           perform   vis-pag-att-000      thru vis-pag-att-999        .
      *                          *-------------------------------------*
      *                          * Indice di scansione al primo ele-   *
      *                          * mento della pagina                  *
      *                          *-------------------------------------*
           move      w-vis-pag-inx-pel    to   w-vis-pag-inx-tbl      .
      *                          *-------------------------------------*
      *                          * Video in On                         *
      *                          *-------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                          *-------------------------------------*
      *                          * Riciclo ad accettazione tasto di    *
      *                          * funzione                            *
      *                          *-------------------------------------*
           go to     fas-bol-340-560.
       fas-bol-340-620.
      *                      *-----------------------------------------*
      *                      * Se Up                                   *
      *                      *-----------------------------------------*
       fas-bol-340-622.
      *                          *-------------------------------------*
      *                          * Se si e' al primo elemento in asso- *
      *                          * luto della tabella si ricicla ad    *
      *                          * accettazione codice numerico per il *
      *                          * tipo interrogazione                 *
      *                          *-------------------------------------*
           if        w-vis-pag-inx-tbl    =    1
                     go to fas-bol-340-200.
       fas-bol-340-624.
      *                          *-------------------------------------*
      *                          * Deviazione a seconda del numero d'- *
      *                          * ordine dell'elemento in esame nel-  *
      *                          * l'ambito della pagina               *
      *                          *-------------------------------------*
           if        w-vis-pag-inx-tbl    =    w-vis-pag-inx-pel
                     go to fas-bol-340-626
           else      go to fas-bol-340-628.
       fas-bol-340-626.
      *                          *-------------------------------------*
      *                          * Se l'elemento in esame e' il primo  *
      *                          * della pagina                        *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Video in Off                    *
      *                              *---------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                              *---------------------------------*
      *                              * Decremento numero pagina at-    *
      *                              * tualmente visualizzata          *
      *                              *---------------------------------*
           subtract  1                    from w-tin-ele-pag          .
      *                              *---------------------------------*
      *                              * Visualizzazione pagina attuale  *
      *                              *---------------------------------*
           perform   vis-pag-att-000      thru vis-pag-att-999        .
      *                              *---------------------------------*
      *                              * Indice di scansione all'ultimo  *
      *                              * elemento della pagina           *
      *                              *---------------------------------*
           move      w-vis-pag-inx-uel    to   w-vis-pag-inx-tbl      .
      *                              *---------------------------------*
      *                              * Video in On                     *
      *                              *---------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                              *---------------------------------*
      *                              * Riciclo ad accettazione tasto   *
      *                              * di funzione                     *
      *                              *---------------------------------*
           go to     fas-bol-340-560.
       fas-bol-340-628.
      *                          *-------------------------------------*
      *                          * Se l'elemento in esame non e' il    *
      *                          * primo della pagina                  *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Decremento indice di scansione  *
      *                              *---------------------------------*
           subtract  1                    from w-vis-pag-inx-tbl      .
      *                              *---------------------------------*
      *                              * Riciclo ad accettazione tasto   *
      *                              * di funzione                     *
      *                              *---------------------------------*
           go to     fas-bol-340-560.
       fas-bol-340-630.
      *                      *-----------------------------------------*
      *                      * Se Down                                 *
      *                      *-----------------------------------------*
       fas-bol-340-632.
      *                          *-------------------------------------*
      *                          * Se si e' all'ultimo elemento in as- *
      *                          * soluto della tabella si ricicla ad  *
      *                          * accettazione tasto di funzione      *
      *                          *-------------------------------------*
           if        w-vis-pag-inx-tbl    =    w-tin-ele-num
                     go to fas-bol-340-570.
       fas-bol-340-634.
      *                          *-------------------------------------*
      *                          * Deviazione a seconda del numero d'- *
      *                          * ordine dell'elemento in esame nel-  *
      *                          * l'ambito della pagina               *
      *                          *-------------------------------------*
           if        w-vis-pag-inx-tbl    =    w-vis-pag-inx-uel
                     go to fas-bol-340-636
           else      go to fas-bol-340-638.
       fas-bol-340-636.
      *                          *-------------------------------------*
      *                          * Se l'elemento in esame e' l'ultimo  *
      *                          * della pagina                        *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Video in Off                    *
      *                              *---------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                              *---------------------------------*
      *                              * Incremento numero pagina at-    *
      *                              * tualmente visualizzata          *
      *                              *---------------------------------*
           add       1                    to   w-tin-ele-pag          .
      *                              *---------------------------------*
      *                              * Visualizzazione pagina attuale  *
      *                              *---------------------------------*
           perform   vis-pag-att-000      thru vis-pag-att-999        .
      *                              *---------------------------------*
      *                              * Indice di scansione al primo    *
      *                              * elemento della pagina           *
      *                              *---------------------------------*
           move      w-vis-pag-inx-pel    to   w-vis-pag-inx-tbl      .
      *                              *---------------------------------*
      *                              * Video in On                     *
      *                              *---------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                              *---------------------------------*
      *                              * Riciclo ad accettazione tasto   *
      *                              * di funzione                     *
      *                              *---------------------------------*
           go to     fas-bol-340-560.
       fas-bol-340-638.
      *                          *-------------------------------------*
      *                          * Se l'elemento in esame non e' l'ul- *
      *                          * timo della pagina                   *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Incremento indice di scansione  *
      *                              *---------------------------------*
           add       1                    to   w-vis-pag-inx-tbl      .
      *                              *---------------------------------*
      *                              * Riciclo ad accettazione tasto   *
      *                              * di funzione                     *
      *                              *---------------------------------*
           go to     fas-bol-340-560.
       fas-bol-340-650.
      *                      *-----------------------------------------*
      *                      * Se Do o Slct o Return                   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Codice numerico per il tipo inter-  *
      *                          * rogazione                           *
      *                          *-------------------------------------*
           move      w-tin-num-tin
                    (w-vis-pag-inx-tbl)   to   w-key-num-int          .
      *                          *-------------------------------------*
      *                          * Codice alfanumerico per il tipo     *
      *                          * interrogazione                      *
      *                          *-------------------------------------*
           move      w-tin-alf-tin
                    (w-vis-pag-inx-tbl)   to   w-key-alf-int          .
      *                          *-------------------------------------*
      *                          * Descrizione per il tipo interroga-  *
      *                          * zione                               *
      *                          *-------------------------------------*
           move      w-tin-des-tin
                    (w-vis-pag-inx-tbl)   to   w-key-des-int          .
      *                          *-------------------------------------*
      *                          * Nome overlay per il tipo interroga- *
      *                          * zione                               *
      *                          *-------------------------------------*
           move      w-tin-ovy-tin
                    (w-vis-pag-inx-tbl)   to   w-key-ovy-int          .
      *                          *-------------------------------------*
      *                          * Memorizzazione del nome della over- *
      *                          * lay per formare il pathname comple- *
      *                          * to per il richiamo del programma di *
      *                          * esecuzione                          *
      *                          *-------------------------------------*
           move      w-key-ovy-int        to   w-ovy-exe-pos          .
      *                          *-------------------------------------*
      *                          * Se il nome della overlay e' a spa-  *
      *                          * ces : si ricicla ad accettazione    *
      *                          * tasto di funzione                   *
      *                          *-------------------------------------*
           if        w-ovy-exe-pos        =    spaces
                     go to fas-bol-340-570.
      *                          *-------------------------------------*
      *                          * Altrimenti si va' al richiamo ef-   *
      *                          * fettivo del programma di esecuzione *
      *                          *-------------------------------------*
           go to     fas-bol-340-700.
       fas-bol-340-660.
      *                      *-----------------------------------------*
      *                      * Se Exit                                 *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Ritorno all'accettazione del codice *
      *                          * numerico per il tipo interrogazione *
      *                          *-------------------------------------*
           go to     fas-bol-340-200.
       fas-bol-340-700.
      *              *-------------------------------------------------*
      *              * Richiamo del programma di esecuzione            *
      *              *-------------------------------------------------*
           perform   ric-pgm-exe-000      thru ric-pgm-exe-999        .
       fas-bol-340-750.
      *              *-------------------------------------------------*
      *              * Visualizzazione codice numerico per tipo inter- *
      *              * rogazione                                       *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      04                   to   v-lin                  .
           move      18                   to   v-pos                  .
           move      w-key-num-int        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Visualizzazione descrizione per tipo interroga- *
      *              * zione                                           *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      60                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      21                   to   v-pos                  .
           move      w-key-des-int        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Riciclo ad accettazione codice numerico per il  *
      *              * tipo di interrogazione                          *
      *              *-------------------------------------------------*
           go to     fas-bol-340-200.
       fas-bol-340-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione pagina attualmente visualizzata           *
      *    *-----------------------------------------------------------*
       vis-pag-att-000.
      *              *-------------------------------------------------*
      *              * Determinazioni iniziali                         *
      *              *-------------------------------------------------*
       vis-pag-att-010.
      *                  *---------------------------------------------*
      *                  * Si/No pagina precedente alla pagina attuale *
      *                  *---------------------------------------------*
           if        w-tin-ele-pag        >    1
                     move  "S"            to   w-vis-pag-snx-pre
           else      move  "N"            to   w-vis-pag-snx-pre      .
       vis-pag-att-020.
      *                  *---------------------------------------------*
      *                  * Si/No pagina seguente alla pagina attuale   *
      *                  *---------------------------------------------*
           if        w-tin-ele-pag        <    w-tin-ele-npt
                     move  "S"            to   w-vis-pag-snx-seg
           else      move  "N"            to   w-vis-pag-snx-seg      .
       vis-pag-att-030.
      *                  *---------------------------------------------*
      *                  * Indice del primo elemento visualizzato nel- *
      *                  * la pagina attuale                           *
      *                  *---------------------------------------------*
           move      w-tin-ele-pag        to   w-vis-pag-inx-pel      .
           subtract  1                    from w-vis-pag-inx-pel      .
           multiply  w-tin-ele-nep        by   w-vis-pag-inx-pel      .
           add       1                    to   w-vis-pag-inx-pel      .
       vis-pag-att-040.
      *                  *---------------------------------------------*
      *                  * Indice dell'ultimo elemento visualizzato    *
      *                  * nella pagina attuale                        *
      *                  *---------------------------------------------*
           move      w-vis-pag-inx-pel    to   w-vis-pag-inx-uel      .
           add       w-tin-ele-nep        to   w-vis-pag-inx-uel      .
           subtract  1                    from w-vis-pag-inx-uel      .
           if        w-vis-pag-inx-uel    >    w-tin-ele-nes
                     move  w-tin-ele-nes  to   w-vis-pag-inx-uel      .
       vis-pag-att-050.
      *              *-------------------------------------------------*
      *              * Abblencamento area totale occupata              *
      *              *-------------------------------------------------*
           move      "EL"                 to   v-ope                  .
           move      w-vis-pag-lin-000    to   v-lin                  .
           move      w-vis-pag-lin-000    to   v-lto                  .
           add       w-tin-ele-nep        to   v-lto                  .
           add       1                    to   v-lto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       vis-pag-att-100.
      *              *-------------------------------------------------*
      *              * Linea iniziale per il numero pagina             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se esiste una sola pagina non si visualizza *
      *                  * il numero pagina                            *
      *                  *---------------------------------------------*
           if        w-tin-ele-npt        not  > 1
                     go to vis-pag-att-200.
      *                  *---------------------------------------------*
      *                  * Visualizzazione literal                     *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      w-vis-pag-lin-000    to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Pagina"             to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione numero pagina               *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<"                  to   v-edm                  .
           move      w-vis-pag-lin-000    to   v-lin                  .
           move      08                   to   v-pos                  .
           move      w-tin-ele-pag        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       vis-pag-att-200.
      *              *-------------------------------------------------*
      *              * Linee effettive di tipi interrogazione          *
      *              *-------------------------------------------------*
       vis-pag-att-300.
      *                  *---------------------------------------------*
      *                  * Inizializzazione indice per scansione su e- *
      *                  * lementi da visualizzare                     *
      *                  *---------------------------------------------*
           move      w-vis-pag-inx-pel    to   w-vis-pag-inx-tbl      .
       vis-pag-att-400.
      *                  *---------------------------------------------*
      *                  * Trattamento elemento in esame               *
      *                  *---------------------------------------------*
       vis-pag-att-410.
      *                      *-----------------------------------------*
      *                      * Determinazione linea a video per ele-   *
      *                      * mento in esame                          *
      *                      *-----------------------------------------*
           move      w-vis-pag-inx-tbl    to   w-vis-pag-lin-acv      .
           subtract  w-vis-pag-inx-pel    from w-vis-pag-lin-acv      .
           add       w-vis-pag-lin-000    to   w-vis-pag-lin-acv      .
           add       1                    to   w-vis-pag-lin-acv      .
       vis-pag-att-420.
      *                      *-----------------------------------------*
      *                      * Se codice numerico per il tipo interro- *
      *                      * gazione a zero : no visualizzazione     *
      *                      *-----------------------------------------*
           if        w-tin-num-tin
                    (w-vis-pag-inx-tbl)   =    zero
                     go to vis-pag-att-500.
       vis-pag-att-430.
      *                      *-----------------------------------------*
      *                      * Visualizzazione codice numerico per il  *
      *                      * tipo interrogazione                     *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      spaces               to   v-edm                  .
           move      w-vis-pag-lin-acv    to   v-lin                  .
           move      12                   to   v-pos                  .
           move      w-tin-num-tin
                    (w-vis-pag-inx-tbl)   to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-pag-att-440.
      *                      *-----------------------------------------*
      *                      * Visualizzazione literal '-'             *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      w-vis-pag-lin-acv    to   v-lin                  .
           move      15                   to   v-pos                  .
           move      "-"                  to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-pag-att-450.
      *                      *-----------------------------------------*
      *                      * Visualizzazione descrizione per il tipo *
      *                      * di interrogazione                       *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      60                   to   v-car                  .
           move      w-vis-pag-lin-acv    to   v-lin                  .
           move      17                   to   v-pos                  .
           move      w-tin-des-tin
                    (w-vis-pag-inx-tbl)   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-pag-att-500.
      *                  *---------------------------------------------*
      *                  * Incremento indice per scansione su elementi *
      *                  * da visualizzare                             *
      *                  *---------------------------------------------*
           add       1                    to   w-vis-pag-inx-tbl      .
      *                  *---------------------------------------------*
      *                  * Se oltre ultimo elemento da visualizzare    *
      *                  * per la pagina si va' alla conclusione, al-  *
      *                  * trimenti si ricicla al trattamento dell'e-  *
      *                  * lemento in esame                            *
      *                  *---------------------------------------------*
           if        w-vis-pag-inx-tbl    >    w-vis-pag-inx-uel
                     go to vis-pag-att-900
           else      go to vis-pag-att-400.
       vis-pag-att-900.
      *              *-------------------------------------------------*
      *              * Linea finale per il literal 'segue'             *
      *              *-------------------------------------------------*
       vis-pag-att-905.
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda se esiste o no una pa- *
      *                  * gina seguente alla pagina attuale           *
      *                  *---------------------------------------------*
           if        w-vis-pag-snx-seg    =    "S"
                     go to vis-pag-att-910
           else      go to vis-pag-att-915.
       vis-pag-att-910.
      *                  *---------------------------------------------*
      *                  * Se esiste una pagina seguente alla pagina   *
      *                  * attuale                                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Visualizzazione literal 'segue'         *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      w-vis-pag-lin-000    to   v-lin                  .
           add       w-tin-ele-nep        to   v-lin                  .
           add       1                    to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Segue"              to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     vis-pag-att-999.
       vis-pag-att-915.
      *                  *---------------------------------------------*
      *                  * Se non esiste una pagina seguente alla pa-  *
      *                  * gina attuale                                *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Deviazione a seconda del numero di pa-  *
      *                      * gine totali                             *
      *                      *-----------------------------------------*
           if        w-tin-ele-npt        >    1
                     go to vis-pag-att-925
           else      go to vis-pag-att-920.
       vis-pag-att-920.
      *                      *-----------------------------------------*
      *                      * Se numero di pagine totali non superio- *
      *                      * re a 1                                  *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Uscita                              *
      *                          *-------------------------------------*
           go to     vis-pag-att-999.
       vis-pag-att-925.
      *                      *-----------------------------------------*
      *                      * Se numero di pagine totali superiore a  *
      *                      * 1                                       *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Visualizzazione literal 'fine '     *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      w-vis-pag-lin-acv    to   v-lin                  .
           add       1                    to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Fine "              to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                          *-------------------------------------*
      *                          * Uscita                              *
      *                          *-------------------------------------*
           go to     vis-pag-att-999.
       vis-pag-att-999.
           exit.

      *    *===========================================================*
      *    * Richiamo del programma di esecuzione                      *
      *    *-----------------------------------------------------------*
       ric-pgm-exe-000.
      *              *-------------------------------------------------*
      *              * Se nome overlay a spaces : uscita               *
      *              *-------------------------------------------------*
           if        w-ovy-exe-pos        =    spaces
                     go to ric-pgm-exe-999.
       ric-pgm-exe-100.
      *              *-------------------------------------------------*
      *              * Scrittura della variabile di i.p.c. relativa    *
      *              * al codice dipendenza per il sottoprogramma      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Scrittura effettiva                         *
      *                  *---------------------------------------------*
           move      "PV"                 to   s-ope                  .
           move      "cod-dpz"            to   s-var                  .
           move      "="                  to   s-dop                  .
           move      "N"                  to   s-tip                  .
           move      02                   to   s-car                  .
           move      zero                 to   s-dec                  .
           move      spaces               to   s-sgn                  .
           move      w-dpz-cod-prg        to   s-num                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
       ric-pgm-exe-200.
      *              *-------------------------------------------------*
      *              * Filtro pathname programma                       *
      *              *-------------------------------------------------*
           move      w-ovy-exe-pat        to   w-ovy-exe-flt          .
           move      w-ovy-exe-flt        to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           move      s-pat                to   w-ovy-exe-flt          .
       ric-pgm-exe-400.
      *              *-------------------------------------------------*
      *              * Salvataggio immagine video                      *
      *              *-------------------------------------------------*
           move      "SV"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Richiamo programma                              *
      *              *-------------------------------------------------*
           call      s-pat                                            .
       ric-pgm-exe-600.
      *              *-------------------------------------------------*
      *              * Cancellazione programma richiamato              *
      *              *-------------------------------------------------*
           cancel    s-pat                                            .
      *              *-------------------------------------------------*
      *              * Ripristino immagine video                       *
      *              *-------------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       ric-pgm-exe-999.
           exit.

