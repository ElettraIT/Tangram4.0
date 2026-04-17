       Identification division.
       Program-Id.                                 pcge7901           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    cge                 *
      *                                Settore:    iva                 *
      *                                   Fase:    cge790              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 04/02/91    *
      *                       Ultima revisione:    NdK del 08/02/10    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Esecuzione richieste del programma pcge7900 *
      *                                                                *
      *                    Numeratore pagine per vidimazione giornali  *
      *                    iva vendite e acquisti                      *
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
                     "iva"                                            .
      *        *-------------------------------------------------------*
      *        * Fase gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-fas                  pic  x(06) value
                     "cge790"                                         .
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10) value
                     "pcge7901"                                       .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  i-ide-des                  pic  x(40) value
                     "     NUMERATORE PAGINE GIORNALI IVA     "       .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "msegrt"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/s"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per moduli                "mbckgx"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/b"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "mprint"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/p"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "mmessg"  *
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
      *        * Contatore per segmenti di parametri selezione stampa  *
      *        *-------------------------------------------------------*
           05  y-ctr                      pic  9(05)                  .
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

      *    *===========================================================*
      *    * Area per ciclo di report-program                          *
      *    *-----------------------------------------------------------*
       01  y-crp.
      *        *-------------------------------------------------------*
      *        * Segnale per primo passaggio                           *
      *        *-------------------------------------------------------*
           05  y-crp-mrk-uno              pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Segnale di 'begin' eseguito                           *
      *        *-------------------------------------------------------*
           05  y-crp-mrk-beg              pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Area per salvataggio parametri rottura livello        *
      *        *-------------------------------------------------------*
           05  y-crp-sav-liv.
               10  y-crp-sav-l05          pic  x(64)                  .
               10  y-crp-sav-l04          pic  x(64)                  .
               10  y-crp-sav-l03          pic  x(64)                  .
               10  y-crp-sav-l02          pic  x(64)                  .
               10  y-crp-sav-l01          pic  x(64)                  .
      *        *-------------------------------------------------------*
      *        * Area per salvataggio area rottura                     *
      *        *-------------------------------------------------------*
           05  y-crp-sav-rot.
               10  filler occurs 320      pic  x(01)                  .

      *    *===========================================================*
      *    * Records file                                              *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [ada]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/azi/fls/rec/rfada"                          .

      *    *===========================================================*
      *    * Work-area richieste per stampa                            *
      *    *-----------------------------------------------------------*
       01  rr.
      *        *-------------------------------------------------------*
      *        * Tipo giornale                                         *
      *        *-------------------------------------------------------*
           05  rr-tip-gio                 pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Numero pagina iniziale                                *
      *        *-------------------------------------------------------*
           05  rr-pag-ini                 pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Numero pagine da stampare                             *
      *        *-------------------------------------------------------*
           05  rr-num-pag                 pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Dicitura per il titolo                                *
      *        *-------------------------------------------------------*
           05  rr-dic-tit                 pic  x(50)                  .
      *        *-------------------------------------------------------*
      *        * Anno da stampare                                      *
      *        *-------------------------------------------------------*
           05  rr-ann-stp                 pic  x(04)                  .
      *        *-------------------------------------------------------*
      *        * Tipo di stampa numero pagina                          *
      *        *-------------------------------------------------------*
           05  rr-tip-snp                 pic  x(01)                  .

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
      *    * Work-area per dati anagrafici azienda                     *
      *    *-----------------------------------------------------------*
       01  w-azi.
           05  w-azi-rag-soc              pic  x(40)                  .
           05  w-azi-ind-azi              pic  x(30)                  .
           05  w-azi-loc-azi              pic  x(30)                  .
           05  w-azi-prt-iva              pic  9(11)                  .

      *    *===========================================================*
      *    * Work-area locale                                          *
      *    *-----------------------------------------------------------*
       01  w-wrk.
           05  w-wrk-num-pag              pic  9(05)                  .
           05  w-wrk-pag-fin              pic  9(05)                  .
           05  w-wrk-ctr-ins              pic  9(05)                  .
           05  w-wrk-lun-tit              pic  9(05)                  .

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
           perform   z-dic-ini-pgm-000    thru z-dic-ini-pgm-999      .
           if        OK                   not  = spaces
                     go to main-999.
      *              *-------------------------------------------------*
      *              * Lettura parametri di selezione stampa           *
      *              *-------------------------------------------------*
           perform   z-let-sel-stp-000    thru z-let-sel-stp-999      .
           if        OK                   not  = spaces
                     go to main-999.
      *              *-------------------------------------------------*
      *              * Lettura record richieste                        *
      *              *-------------------------------------------------*
           perform   z-let-rec-ric-000    thru z-let-rec-ric-999      .
           if        OK                   not  = spaces
                     go to main-999.
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
           move      spaces               to   OK                     .
           perform   z-esr-opn-fls-000    thru z-esr-opn-fls-999      .
           if        OK                   not  = spaces
                     go to main-900.
      *              *-------------------------------------------------*
      *              * Ciclo di report-program                         *
      *              *-------------------------------------------------*
           move      spaces               to   OK                     .
           perform   z-cic-rep-pro-000    thru z-cic-rep-pro-999      .
           if        OK                   not  = spaces
                     go to main-800.
       main-800.
      *              *-------------------------------------------------*
      *              * Close files                                     *
      *              *-------------------------------------------------*
           perform   z-esr-cls-fls-000    thru z-esr-cls-fls-999      .
      *              *-------------------------------------------------*
      *              * Cancellazione modulo stampa                     *
      *              *-------------------------------------------------*
           cancel    "swd/mod/prg/obj/mprint"                         .
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
           move      "N"                  to   s-svv                  .
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
      *    * Lettura parametri di selezione stampa da segreteria       *
      *    *-----------------------------------------------------------*
       z-let-sel-stp-000.
      *              *-------------------------------------------------*
      *              * Inizializzazione area parametri stampa          *
      *              *-------------------------------------------------*
           move      spaces               to   p-sel                  .
      *              *-------------------------------------------------*
      *              * Inizializzazione numero progressivo segmento    *
      *              *-------------------------------------------------*
           move      zero                 to   y-ctr                  .
       z-let-sel-stp-100.
      *              *-------------------------------------------------*
      *              * Incremento numero progressivo segmento          *
      *              *-------------------------------------------------*
           add       1                    to   y-ctr                  .
      *              *-------------------------------------------------*
      *              * Richiamo del modulo di segreteria per l'estra-  *
      *              * zione del segmento di parametri stampa          *
      *              *-------------------------------------------------*
           move      "S<"                 to   s-ope                  .
           move      y-ctr                to   s-num                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Concatenazione del segmento in area parametri   *
      *              * di stampa selezionati                           *
      *              *-------------------------------------------------*
           move      y-ctr                to   y-pnt                  .
           multiply  80                   by   y-pnt                  .
           subtract  79                   from y-pnt                  .
           move      y-pnt                to   y-svp                  .
           string    s-alf
                     delimited by size    into p-sel
                                  with pointer y-pnt                  .
      *              *-------------------------------------------------*
      *              * Se non si e' alla fine del record si ricicla    *
      *              *-------------------------------------------------*
           if        y-pnt                not  = y-svp
                     go to z-let-sel-stp-100.
       z-let-sel-stp-999.
           exit.

      *    *===========================================================*
      *    * Lettura record richieste                                  *
      *    *-----------------------------------------------------------*
       z-let-rec-ric-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   OK                     .
      *              *-------------------------------------------------*
      *              * Richiesta tipo funzionamento a segreteria       *
      *              *-------------------------------------------------*
           move      "IG"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Inizio lettura record richieste                 *
      *              *-------------------------------------------------*
           move      "OI"                 to   b-ope                  .
           move      s-fun                to   b-tfe                  .
           call      "swd/mod/prg/obj/mbckgr"
                                         using b                      .
      *                  *---------------------------------------------*
      *                  * Se errori : uscita                          *
      *                  *---------------------------------------------*
           if        b-rsc                not  = spaces
                     move  "#"            to   OK
                     go to z-let-rec-ric-900.
      *              *-------------------------------------------------*
      *              * Estrazione segmenti da 255  bytes da record ri- *
      *              * chieste                                         *
      *              *-------------------------------------------------*
           move      1                    to   y-pnt                  .
       z-let-rec-ric-100.
           move      "GT"                 to   b-ope                  .
           call      "swd/mod/prg/obj/mbckgr"
                                         using b                      .
           if        b-rsc                not  = spaces
                     go to z-let-rec-ric-200.
           move      y-pnt                to   y-svp                  .
           string    b-chr
                     delimited by size    into rr
                                  with pointer y-pnt                  .
           if        y-pnt                not  = y-svp
                     go to z-let-rec-ric-100.
       z-let-rec-ric-200.
      *              *-------------------------------------------------*
      *              * Fine lettura record richieste                   *
      *              *-------------------------------------------------*
           move      "CL"                 to   b-ope                  .
           call      "swd/mod/prg/obj/mbckgr"
                                         using b                      .
      *                  *---------------------------------------------*
      *                  * Se errori : uscita                          *
      *                  *---------------------------------------------*
           if        b-rsc                not  = spaces
                     move  "#"            to   OK                     .
       z-let-rec-ric-900.
      *              *-------------------------------------------------*
      *              * Cancel modulo trattamento richieste             *
      *              *-------------------------------------------------*
           cancel    "swd/mod/prg/obj/mbckgr"                         .
       z-let-rec-ric-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di report-program                                   *
      *    *-----------------------------------------------------------*
       z-cic-rep-pro-000.
      *              *-------------------------------------------------*
      *              * Inizializzazione markers                        *
      *              *-------------------------------------------------*
           move      spaces               to   y-crp-mrk-uno
                                               y-crp-mrk-beg          .
      *              *-------------------------------------------------*
      *              * Inizializzazione area per rotture di livello    *
      *              *-------------------------------------------------*
           move      spaces               to   w-rot                  .
      *              *-------------------------------------------------*
      *              * Start iniziale                                  *
      *              *-------------------------------------------------*
           move      spaces               to   OK                     .
           perform   z-esr-str-ini-000    thru z-esr-str-ini-999      .
           if        OK                   not  = spaces
                     move  spaces         to   OK
                     go to z-cic-rep-pro-600.
       z-cic-rep-pro-100.
      *              *-------------------------------------------------*
      *              * Salvataggio area rottura in area precedente     *
      *              *-------------------------------------------------*
           move      w-rot-l05            to   y-crp-sav-l05          .
           move      w-rot-l04            to   y-crp-sav-l04          .
           move      w-rot-l03            to   y-crp-sav-l03          .
           move      w-rot-l02            to   y-crp-sav-l02          .
           move      w-rot-l01            to   y-crp-sav-l01          .
       z-cic-rep-pro-200.
      *              *-------------------------------------------------*
      *              * Lettura sequenziale del file principale         *
      *              *-------------------------------------------------*
           move      spaces               to   OK                     .
           perform   z-esr-let-seq-000    thru z-esr-let-seq-999      .
           if        OK                   not  = spaces
                     move  spaces         to   OK
                     go to z-cic-rep-pro-500.
      *              *-------------------------------------------------*
      *              * Test superamento limiti massimi                 *
      *              *-------------------------------------------------*
           move      spaces               to   OK                     .
           perform   z-esr-tst-max-000    thru z-esr-tst-max-999      .
           if        OK                   not  = spaces
                     move  spaces         to   OK
                     go to z-cic-rep-pro-500.
      *              *-------------------------------------------------*
      *              * Selezione su record letto                       *
      *              *-------------------------------------------------*
           move      spaces               to   OK                     .
           perform   z-esr-sel-rec-000    thru z-esr-sel-rec-999      .
           if        OK                   not  = spaces
                     move  spaces         to   OK
                     go to z-cic-rep-pro-200.
      *              *-------------------------------------------------*
      *              * Composizione area per tests di rottura          *
      *              *-------------------------------------------------*
           perform   z-esr-cmp-rot-000    thru z-esr-cmp-rot-999      .
      *              *-------------------------------------------------*
      *              * Se primo passaggio                              *
      *              *-------------------------------------------------*
           if        y-crp-mrk-uno        not  = spaces
                     go to z-cic-rep-pro-300.
      *                  *---------------------------------------------*
      *                  * Begin                                       *
      *                  *---------------------------------------------*
           move      "BE"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Se errori                               *
      *                      *-----------------------------------------*
           if        p-rsc                not  = spaces
                     move  "#"            to   OK
                     go to z-cic-rep-pro-900.
           move      "#"                  to   y-crp-mrk-beg          .
      *                  *---------------------------------------------*
      *                  * Inizio di tutti i livelli                   *
      *                  *---------------------------------------------*
           perform   z-cic-rep-pro-790    thru z-cic-rep-pro-791      .
           perform   z-cic-rep-pro-750    thru z-cic-rep-pro-751      .
           perform   z-cic-rep-pro-740    thru z-cic-rep-pro-741      .
           perform   z-cic-rep-pro-730    thru z-cic-rep-pro-731      .
           perform   z-cic-rep-pro-720    thru z-cic-rep-pro-721      .
           perform   z-cic-rep-pro-710    thru z-cic-rep-pro-711      .
           go to     z-cic-rep-pro-400.
       z-cic-rep-pro-300.
      *              *-------------------------------------------------*
      *              * Se rottura del 5. livello                       *
      *              *-------------------------------------------------*
           if        w-rot-l05            =    y-crp-sav-l05
                     go to z-cic-rep-pro-310.
           move      w-rot                to   y-crp-sav-rot          .
           move      y-crp-sav-l05        to   w-rot-l05              .
           move      y-crp-sav-l04        to   w-rot-l04              .
           move      y-crp-sav-l03        to   w-rot-l03              .
           move      y-crp-sav-l02        to   w-rot-l02              .
           move      y-crp-sav-l01        to   w-rot-l01              .
           perform   z-cic-rep-pro-810    thru z-cic-rep-pro-811      .
           perform   z-cic-rep-pro-820    thru z-cic-rep-pro-821      .
           perform   z-cic-rep-pro-830    thru z-cic-rep-pro-831      .
           perform   z-cic-rep-pro-840    thru z-cic-rep-pro-841      .
           perform   z-cic-rep-pro-850    thru z-cic-rep-pro-851      .
           move      y-crp-sav-rot        to   w-rot                  .
           perform   z-cic-rep-pro-750    thru z-cic-rep-pro-751      .
           perform   z-cic-rep-pro-740    thru z-cic-rep-pro-741      .
           perform   z-cic-rep-pro-730    thru z-cic-rep-pro-731      .
           perform   z-cic-rep-pro-720    thru z-cic-rep-pro-721      .
           perform   z-cic-rep-pro-710    thru z-cic-rep-pro-711      .
           go to     z-cic-rep-pro-400.
       z-cic-rep-pro-310.
      *              *-------------------------------------------------*
      *              * Se rottura del 4. livello                       *
      *              *-------------------------------------------------*
           if        w-rot-l04            =    y-crp-sav-l04
                     go to z-cic-rep-pro-320.
           move      w-rot                to   y-crp-sav-rot          .
           move      y-crp-sav-l04        to   w-rot-l04              .
           move      y-crp-sav-l03        to   w-rot-l03              .
           move      y-crp-sav-l02        to   w-rot-l02              .
           move      y-crp-sav-l01        to   w-rot-l01              .
           perform   z-cic-rep-pro-810    thru z-cic-rep-pro-811      .
           perform   z-cic-rep-pro-820    thru z-cic-rep-pro-821      .
           perform   z-cic-rep-pro-830    thru z-cic-rep-pro-831      .
           perform   z-cic-rep-pro-840    thru z-cic-rep-pro-841      .
           move      y-crp-sav-rot        to   w-rot                  .
           perform   z-cic-rep-pro-740    thru z-cic-rep-pro-741      .
           perform   z-cic-rep-pro-730    thru z-cic-rep-pro-731      .
           perform   z-cic-rep-pro-720    thru z-cic-rep-pro-721      .
           perform   z-cic-rep-pro-710    thru z-cic-rep-pro-711      .
           go to     z-cic-rep-pro-400.
       z-cic-rep-pro-320.
      *              *-------------------------------------------------*
      *              * Se rottura del 3. livello                       *
      *              *-------------------------------------------------*
           if        w-rot-l03            =    y-crp-sav-l03
                     go to z-cic-rep-pro-330.
           move      w-rot                to   y-crp-sav-rot          .
           move      y-crp-sav-l03        to   w-rot-l03              .
           move      y-crp-sav-l02        to   w-rot-l02              .
           move      y-crp-sav-l01        to   w-rot-l01              .
           perform   z-cic-rep-pro-810    thru z-cic-rep-pro-811      .
           perform   z-cic-rep-pro-820    thru z-cic-rep-pro-821      .
           perform   z-cic-rep-pro-830    thru z-cic-rep-pro-831      .
           move      y-crp-sav-rot        to   w-rot                  .
           perform   z-cic-rep-pro-730    thru z-cic-rep-pro-731      .
           perform   z-cic-rep-pro-720    thru z-cic-rep-pro-721      .
           perform   z-cic-rep-pro-710    thru z-cic-rep-pro-711      .
           go to     z-cic-rep-pro-400.
       z-cic-rep-pro-330.
      *              *-------------------------------------------------*
      *              * Se rottura del 2. livello                       *
      *              *-------------------------------------------------*
           if        w-rot-l02            =    y-crp-sav-l02
                     go to z-cic-rep-pro-340.
           move      w-rot                to   y-crp-sav-rot          .
           move      y-crp-sav-l02        to   w-rot-l02              .
           move      y-crp-sav-l01        to   w-rot-l01              .
           perform   z-cic-rep-pro-810    thru z-cic-rep-pro-811      .
           perform   z-cic-rep-pro-820    thru z-cic-rep-pro-821      .
           move      y-crp-sav-rot        to   w-rot                  .
           perform   z-cic-rep-pro-720    thru z-cic-rep-pro-721      .
           perform   z-cic-rep-pro-710    thru z-cic-rep-pro-711      .
           go to     z-cic-rep-pro-400.
       z-cic-rep-pro-340.
      *              *-------------------------------------------------*
      *              * Se rottura del 1. livello                       *
      *              *-------------------------------------------------*
           if        w-rot-l01            =    y-crp-sav-l01
                     go to z-cic-rep-pro-400.
           move      w-rot                to   y-crp-sav-rot          .
           move      y-crp-sav-l01        to   w-rot-l01              .
           perform   z-cic-rep-pro-810    thru z-cic-rep-pro-811      .
           move      y-crp-sav-rot        to   w-rot                  .
           perform   z-cic-rep-pro-710    thru z-cic-rep-pro-711      .
       z-cic-rep-pro-400.
      *              *-------------------------------------------------*
      *              * Se segnale di interruzione attivo : fine ciclo  *
      *              *-------------------------------------------------*
           if        OK                   not  = spaces
                     go to z-cic-rep-pro-900.
      *              *-------------------------------------------------*
      *              * Esecuzione per il livello di dettaglio          *
      *              *-------------------------------------------------*
           move      spaces               to   OK                     .
           perform   z-esr-liv-det-000    thru z-esr-liv-det-999      .
      *              *-------------------------------------------------*
      *              * Se segnale di interruzione : fine ciclo         *
      *              *-------------------------------------------------*
           if        OK                   not  = spaces
                     go to z-cic-rep-pro-900.
      *              *-------------------------------------------------*
      *              * Segnale di passaggio successivo al primo        *
      *              *-------------------------------------------------*
           move      "#"                  to   y-crp-mrk-uno          .
      *              *-------------------------------------------------*
      *              * Riciclo a lettura sequenziale file principale   *
      *              *-------------------------------------------------*
           go to     z-cic-rep-pro-100.
       z-cic-rep-pro-500.
      *              *-------------------------------------------------*
      *              * Test se almeno un passaggio                     *
      *              *-------------------------------------------------*
           if        y-crp-mrk-uno        =    spaces
                     go to z-cic-rep-pro-600.
           perform   z-cic-rep-pro-810    thru z-cic-rep-pro-811      .
           perform   z-cic-rep-pro-820    thru z-cic-rep-pro-821      .
           perform   z-cic-rep-pro-830    thru z-cic-rep-pro-831      .
           perform   z-cic-rep-pro-840    thru z-cic-rep-pro-841      .
           perform   z-cic-rep-pro-850    thru z-cic-rep-pro-851      .
           perform   z-cic-rep-pro-890    thru z-cic-rep-pro-891      .
           go to     z-cic-rep-pro-900.
       z-cic-rep-pro-600.
      *              *-------------------------------------------------*
      *              * Esecuzione per nessuna registrazione da elab.   *
      *              *-------------------------------------------------*
           perform   z-esr-nes-ela-000    thru z-esr-nes-ela-999      .
           go to     z-cic-rep-pro-900.
       z-cic-rep-pro-710.
      *              *-------------------------------------------------*
      *              * Esecuzione per inizio 1. livello di rottura     *
      *              *-------------------------------------------------*
           if        OK                   not  = spaces
                     go to z-cic-rep-pro-711.
           perform   z-esr-ini-lr1-000    thru z-esr-ini-lr1-999      .
       z-cic-rep-pro-711.
           exit.
       z-cic-rep-pro-720.
      *              *-------------------------------------------------*
      *              * Esecuzione per inizio 2. livello di rottura     *
      *              *-------------------------------------------------*
           if        OK                   not  = spaces
                     go to z-cic-rep-pro-721.
           perform   z-esr-ini-lr2-000    thru z-esr-ini-lr2-999      .
       z-cic-rep-pro-721.
           exit.
       z-cic-rep-pro-730.
      *              *-------------------------------------------------*
      *              * Esecuzione per inizio 3. livello di rottura     *
      *              *-------------------------------------------------*
           if        OK                   not  = spaces
                     go to z-cic-rep-pro-731.
           perform   z-esr-ini-lr3-000    thru z-esr-ini-lr3-999      .
       z-cic-rep-pro-731.
           exit.
       z-cic-rep-pro-740.
      *              *-------------------------------------------------*
      *              * Esecuzione per inizio 4. livello di rottura     *
      *              *-------------------------------------------------*
           if        OK                   not  = spaces
                     go to z-cic-rep-pro-741.
           perform   z-esr-ini-lr4-000    thru z-esr-ini-lr4-999      .
       z-cic-rep-pro-741.
           exit.
       z-cic-rep-pro-750.
      *              *-------------------------------------------------*
      *              * Esecuzione per inizio 5. livello di rottura     *
      *              *-------------------------------------------------*
           if        OK                   not  = spaces
                     go to z-cic-rep-pro-751.
           perform   z-esr-ini-lr5-000    thru z-esr-ini-lr5-999      .
       z-cic-rep-pro-751.
           exit.
       z-cic-rep-pro-790.
      *              *-------------------------------------------------*
      *              * Esecuzione per inizio ciclo                     *
      *              *-------------------------------------------------*
           if        OK                   not  = spaces
                     go to z-cic-rep-pro-791.
           perform   z-esr-ini-cic-000    thru z-esr-ini-cic-999      .
       z-cic-rep-pro-791.
           exit.
       z-cic-rep-pro-810.
      *              *-------------------------------------------------*
      *              * Esecuzione per fine 1. livello di rottura       *
      *              *-------------------------------------------------*
           if        OK                   not  = spaces
                     go to z-cic-rep-pro-811.
           perform   z-esr-fin-lr1-000    thru z-esr-fin-lr1-999      .
       z-cic-rep-pro-811.
           exit.
       z-cic-rep-pro-820.
      *              *-------------------------------------------------*
      *              * Esecuzione per fine 2. livello di rottura       *
      *              *-------------------------------------------------*
           if        OK                   not  = spaces
                     go to z-cic-rep-pro-821.
           perform   z-esr-fin-lr2-000    thru z-esr-fin-lr2-999      .
       z-cic-rep-pro-821.
           exit.
       z-cic-rep-pro-830.
      *              *-------------------------------------------------*
      *              * Esecuzione per fine 3. livello di rottura       *
      *              *-------------------------------------------------*
           if        OK                   not  = spaces
                     go to z-cic-rep-pro-831.
           perform   z-esr-fin-lr3-000    thru z-esr-fin-lr3-999      .
       z-cic-rep-pro-831.
           exit.
       z-cic-rep-pro-840.
      *              *-------------------------------------------------*
      *              * Esecuzione per fine 4. livello di rottura       *
      *              *-------------------------------------------------*
           if        OK                   not  = spaces
                     go to z-cic-rep-pro-841.
           perform   z-esr-fin-lr4-000    thru z-esr-fin-lr4-999      .
       z-cic-rep-pro-841.
           exit.
       z-cic-rep-pro-850.
      *              *-------------------------------------------------*
      *              * Esecuzione per fine 5. livello di rottura       *
      *              *-------------------------------------------------*
           if        OK                   not  = spaces
                     go to z-cic-rep-pro-851.
           perform   z-esr-fin-lr5-000    thru z-esr-fin-lr5-999      .
       z-cic-rep-pro-851.
           exit.
       z-cic-rep-pro-890.
      *              *-------------------------------------------------*
      *              * Esecuzione per fine ciclo                       *
      *              *-------------------------------------------------*
           if        OK                   not  = spaces
                     go to z-cic-rep-pro-891.
           perform   z-esr-fin-cic-000    thru z-esr-fin-cic-999      .
       z-cic-rep-pro-891.
           exit.
       z-cic-rep-pro-900.
      *              *-------------------------------------------------*
      *              * End                                             *
      *              *-------------------------------------------------*
           move      "EN"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           if        p-rsc                not  = spaces
                     go to z-cic-rep-pro-999.
       z-cic-rep-pro-999.
           exit.

      *    *===========================================================*
      *    * Routine pre-esecuzione programma                          *
      *    *-----------------------------------------------------------*
       z-pre-exe-pgm-000.
      *              *-------------------------------------------------*
      *              * Richiesta ragione sociale azienda a segretario  *
      *              *-------------------------------------------------*
           move      "IA"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-asx                to   w-azi-rag-soc          .
      *              *-------------------------------------------------*
      *              * Lettura altri dati anagrafici azienda           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Open [ada]                                  *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/azi/fls/ioc/obj/iofada"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ada                 .
      *                  *---------------------------------------------*
      *                  * Normalizzazione [ada]                       *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/azi/fls/ioc/obj/iofada"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ada                 .
      *                  *---------------------------------------------*
      *                  * Lettura [ada] dati anagrafici Azienda       *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODDPZ    "         to   f-key                  .
           move      zero                 to   rf-ada-cod-dpz         .
           move      "pgm/azi/fls/ioc/obj/iofada"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ada                 .
      *                  *---------------------------------------------*
      *                  * Close [ada]                                 *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/azi/fls/ioc/obj/iofada"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ada                 .
      *                  *---------------------------------------------*
      *                  * Salvataggio dati azienda letti              *
      *                  *---------------------------------------------*
           move      rf-ada-via-azi       to   w-azi-ind-azi          .
           move      rf-ada-loc-azi       to   w-azi-loc-azi          .
           move      rf-ada-prt-iva       to   w-azi-prt-iva          .
       z-pre-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Open files                                                *
      *    *-----------------------------------------------------------*
       z-esr-opn-fls-000.
       z-esr-opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Close files                                               *
      *    *-----------------------------------------------------------*
       z-esr-cls-fls-000.
       z-esr-cls-fls-999.
           exit.

      *    *===========================================================*
      *    * Start iniziale                                            *
      *    *-----------------------------------------------------------*
       z-esr-str-ini-000.
      *              *-------------------------------------------------*
      *              * Determinazione numero pagina iniziale           *
      *              *-------------------------------------------------*
           move      rr-pag-ini           to   w-wrk-num-pag          .
           subtract  1                    from w-wrk-num-pag          .
      *              *-------------------------------------------------*
      *              * Determinazione numero pagina finale             *
      *              *-------------------------------------------------*
           add       rr-num-pag
                     rr-pag-ini         giving w-wrk-pag-fin          .
           subtract  1                    from w-wrk-pag-fin          .
       z-esr-str-ini-999.
           exit.

      *    *===========================================================*
      *    * Messaggio per nessuna registrazione da elaborare          *
      *    *-----------------------------------------------------------*
       z-esr-nes-ela-000.
           move      "WR"                 to   m-ope                  .
           move      "Nessuna registrazione da elaborare !"
                                          to   m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
       z-esr-nes-ela-999.
           exit.

      *    *===========================================================*
      *    * Lettura sequenziale                                       *
      *    *-----------------------------------------------------------*
       z-esr-let-seq-000.
      *              *-------------------------------------------------*
      *              * Incremento contatore numero pagina              *
      *              *-------------------------------------------------*
           add       1                    to   w-wrk-num-pag          .
       z-esr-let-seq-999.
           exit.

      *    *===========================================================*
      *    * Test superamento limiti massimi                           *
      *    *-----------------------------------------------------------*
       z-esr-tst-max-000.
           if        w-wrk-num-pag        >    w-wrk-pag-fin
                     move   "#"           to   OK                     .
       z-esr-tst-max-999.
           exit.

      *    *===========================================================*
      *    * Selezione su record letto                                 *
      *    *-----------------------------------------------------------*
       z-esr-sel-rec-000.
       z-esr-sel-rec-999.
           exit.

      *    *===========================================================*
      *    * Composizione area di rottura y-rot                        *
      *    *-----------------------------------------------------------*
       z-esr-cmp-rot-000.
       z-esr-cmp-rot-999.
           exit.

      *    *===========================================================*
      *    * Esecuzione per inizio ciclo                               *
      *    *-----------------------------------------------------------*
       z-esr-ini-cic-000.
       z-esr-ini-cic-999.
           exit.

      *    *===========================================================*
      *    * Esecuzione per fine ciclo                                 *
      *    *-----------------------------------------------------------*
       z-esr-fin-cic-000.
       z-esr-fin-cic-999.
           exit.

      *    *===========================================================*
      *    * Esecuzione per inizio 5. livello di rottura               *
      *    *-----------------------------------------------------------*
       z-esr-ini-lr5-000.
       z-esr-ini-lr5-999.
           exit.

      *    *===========================================================*
      *    * Esecuzione per fine 5. livello di rottura                 *
      *    *-----------------------------------------------------------*
       z-esr-fin-lr5-000.
       z-esr-fin-lr5-999.
           exit.

      *    *===========================================================*
      *    * Esecuzione per inizio 4. livello di rottura               *
      *    *-----------------------------------------------------------*
       z-esr-ini-lr4-000.
       z-esr-ini-lr4-999.
           exit.

      *    *===========================================================*
      *    * Esecuzione per fine 4. livello di rottura                 *
      *    *-----------------------------------------------------------*
       z-esr-fin-lr4-000.
       z-esr-fin-lr4-999.
           exit.

      *    *===========================================================*
      *    * Esecuzione per inizio 3. livello di rottura               *
      *    *-----------------------------------------------------------*
       z-esr-ini-lr3-000.
       z-esr-ini-lr3-999.
           exit.

      *    *===========================================================*
      *    * Esecuzione per fine 3. livello di rottura                 *
      *    *-----------------------------------------------------------*
       z-esr-fin-lr3-000.
       z-esr-fin-lr3-999.
           exit.

      *    *===========================================================*
      *    * Esecuzione per inizio 2. livello di rottura               *
      *    *-----------------------------------------------------------*
       z-esr-ini-lr2-000.
       z-esr-ini-lr2-999.
           exit.

      *    *===========================================================*
      *    * Esecuzione per fine 2. livello di rottura                 *
      *    *-----------------------------------------------------------*
       z-esr-fin-lr2-000.
       z-esr-fin-lr2-999.
           exit.

      *    *===========================================================*
      *    * Esecuzione per inizio 1. livello di rottura               *
      *    *-----------------------------------------------------------*
       z-esr-ini-lr1-000.
       z-esr-ini-lr1-999.
           exit.

      *    *===========================================================*
      *    * Esecuzione per fine 1. livello di rottura                 *
      *    *-----------------------------------------------------------*
       z-esr-fin-lr1-000.
       z-esr-fin-lr1-999.
           exit.

      *    *===========================================================*
      *    * Esecuzione per il livello di dettaglio                    *
      *    *-----------------------------------------------------------*
       z-esr-liv-det-000.
      *              *-------------------------------------------------*
      *              * Avanzamento pagina                              *
      *              *-------------------------------------------------*
           move      "PA"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Se errore grave di i-o su stampa si esce    *
      *                  * con status di errore                        *
      *                  *---------------------------------------------*
           if        p-rsc                not  = spaces
                     move   "#"           to   OK
                     go to  z-esr-liv-det-999.
      *              *-------------------------------------------------*
      *              * Linea di '='                                    *
      *              *-------------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      p-sel-als-sel        to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           move      all   "="            to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       z-esr-liv-det-100.
      *              *-------------------------------------------------*
      *              * Prima linea intestazione                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ragione sociale azienda                     *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           move      w-azi-rag-soc        to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo di stampa   *
      *                  * numero pagina                               *
      *                  *---------------------------------------------*
           if        rr-tip-snp           =    "A"
                     perform  prn-liv-det-ann-000
                                          thru prn-liv-det-ann-999
           else      perform  prn-liv-det-pag-000
                                          thru prn-liv-det-pag-999    .
       z-esr-liv-det-380.
      *                      *-----------------------------------------*
      *                      * Interlinea                              *
      *                      *-----------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       z-esr-liv-det-400.
      *              *-------------------------------------------------*
      *              * Seconda linea intestazione                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Indirizzo azienda                           *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      30                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           move      w-azi-ind-azi        to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Titolo stampa                               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Determinazione posizione                *
      *                      *-----------------------------------------*
           move      zero                 to   w-wrk-ctr-ins          .
           inspect   rr-dic-tit       tallying w-wrk-ctr-ins
                                  for trailing spaces                 .
           subtract  w-wrk-ctr-ins        from 50
                                        giving w-wrk-lun-tit          .
      *                      *-----------------------------------------*
      *                      * Stampa                                  *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      w-wrk-lun-tit        to   p-car                  .
           move      p-lnr                to   p-lin                  .
           divide    2                    into w-wrk-ctr-ins
                                        giving p-pos                  .
           add       42                   to   p-pos                  .
           move      rr-dic-tit           to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Interlinea                              *
      *                      *-----------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       z-esr-liv-det-600.
      *              *-------------------------------------------------*
      *              * Terza linea intestazione                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Localita' azienda                           *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      30                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           move      w-azi-loc-azi        to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * 'Partita iva :'                             *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      13                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      108                  to   p-pos                  .
           move      "Partita iva :"      to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Partita iva azienda                         *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      11                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "9"                  to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      122                  to   p-pos                  .
           move      w-azi-prt-iva        to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Interlinea                              *
      *                      *-----------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *              *-------------------------------------------------*
      *              * Linea di '-'                                    *
      *              *-------------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      p-sel-als-sel        to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           move      all   "-"            to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       z-esr-liv-det-999.
           exit.

      *    *===========================================================*
      *    * Esecuzione per il livello di dettaglio                    *
      *    *                                                           *
      *    * Intestazione tipo 'Anno XXXX Pag. 99999'                  *
      *    *-----------------------------------------------------------*
       prn-liv-det-ann-000.
      *              *-------------------------------------------------*
      *              * Preparazione stringa numero pagina              *
      *              *-------------------------------------------------*
           move      20                   to   w-all-str-lun          .
           move      02                   to   w-all-str-num          .
           move      "Anno"               to   w-all-str-cat (1)      .
           move      rr-ann-stp           to   w-all-str-cat (2)      .
           perform   all-str-csb-000      thru all-str-csb-999        .
      *              *-------------------------------------------------*
      *              * Editing numero pagina                           *
      *              *-------------------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      05                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "<B"                 to   p-edm                  .
           move      w-wrk-num-pag        to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *              *-------------------------------------------------*
      *              * Preparazione stringa numero pagina              *
      *              *-------------------------------------------------*
           move      20                   to   w-all-str-lun          .
           move      03                   to   w-all-str-num          .
           move      w-all-str-alf        to   w-all-str-cat (1)      .
           move      "Pag."               to   w-all-str-cat (2)      .
           move      p-edt                to   w-all-str-cat (3)      .
           perform   all-str-csb-000      thru all-str-csb-999        .
      *              *-------------------------------------------------*
      *              * Allineamento a dx.                              *
      *              *-------------------------------------------------*
           perform   all-str-adx-000      thru all-str-adx-999        .
      *              *-------------------------------------------------*
      *              * Stampa                                          *
      *              *-------------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      20                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           subtract  19                   from p-sel-als-sel
                                        giving p-pos                  .
           move      w-all-str-alf        to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-liv-det-ann-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     prn-liv-det-ann-999.
       prn-liv-det-ann-999.
           exit.

      *    *===========================================================*
      *    * Esecuzione per il livello di dettaglio                    *
      *    *                                                           *
      *    * Intestazione tipo 'Pag. 99999/XXXX'                       *
      *    *-----------------------------------------------------------*
       prn-liv-det-pag-000.
      *              *-------------------------------------------------*
      *              * Editing numero pagina                           *
      *              *-------------------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      05                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "<B"                 to   p-edm                  .
           move      w-wrk-num-pag        to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *              *-------------------------------------------------*
      *              * Preparazione prompt numero pagina               *
      *              *-------------------------------------------------*
           move      20                   to   w-all-str-lun          .
           move      02                   to   w-all-str-num          .
           move      "Pag."               to   w-all-str-cat (1)      .
           move      p-edt                to   w-all-str-cat (2)      .
           perform   all-str-csb-000      thru all-str-csb-999        .
      *              *-------------------------------------------------*
      *              * Preparazione stringa numero pagina              *
      *              *-------------------------------------------------*
           move      20                   to   w-all-str-lun          .
           move      03                   to   w-all-str-num          .
           move      w-all-str-alf        to   w-all-str-cat (1)      .
           move      "/"                  to   w-all-str-cat (2)      .
           move      rr-ann-stp           to   w-all-str-cat (3)      .
           perform   all-str-cat-000      thru all-str-cat-999        .
      *              *-------------------------------------------------*
      *              * Allineamento a dx.                              *
      *              *-------------------------------------------------*
           perform   all-str-adx-000      thru all-str-adx-999        .
      *              *-------------------------------------------------*
      *              * Stampa                                          *
      *              *-------------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      20                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           subtract  19                   from p-sel-als-sel
                                        giving p-pos                  .
           move      w-all-str-alf        to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-liv-det-pag-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     prn-liv-det-pag-999.
       prn-liv-det-pag-999.
           exit.

      *    *===========================================================*
      *    * Subroutines per allineamenti a destra o a sinistra oppure *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cps"                   .

