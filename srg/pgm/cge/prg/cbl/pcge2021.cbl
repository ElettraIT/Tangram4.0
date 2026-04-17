       Identification division.
       Program-Id.                                 pcge2021           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    cge                 *
      *                                Settore:    arc                 *
      *                                   Fase:    cge202              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 25/10/89    *
      *                       Ultima revisione:    NdK del 02/10/08    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Esecuzione richieste del programma pcge2020 *
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
                     "pcge2021"                                       .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  i-ide-des                  pic  x(40) value
                     "    STAMPA ANAGRAFICA PIANO DEI CONTI   "       .

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
      *    * Record files                                              *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [pdc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfpdc"                          .
      *        *-------------------------------------------------------*
      *        * [zma]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfzma"                          .
      *        *-------------------------------------------------------*
      *        * [zcn]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfzcn"                          .

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

      *    *===========================================================*
      *    * Work per subroutines di editing codice sottoconto         *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/wedtpdc0.wkl"                   .

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
      *            *---------------------------------------------------*
      *            * Codice mastro                                     *
      *            *---------------------------------------------------*
               10  w-rot-l02-cod-mas      pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * 1. livello di rottura                                 *
      *        *-------------------------------------------------------*
           05  w-rot-l01.
      *            *---------------------------------------------------*
      *            * Codice conto                                      *
      *            *---------------------------------------------------*
               10  w-rot-l01-cod-con      pic  9(02)                  .

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
      *    * Open files                                                *
      *    *-----------------------------------------------------------*
       z-esr-opn-fls-000.
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
      *              * [zma]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofzma"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zma                 .
      *              *-------------------------------------------------*
      *              * [zcn]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofzcn"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zcn                 .
       z-esr-opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Close files                                               *
      *    *-----------------------------------------------------------*
       z-esr-cls-fls-000.
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
      *              * [zma]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofzma"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zma                 .
      *              *-------------------------------------------------*
      *              * [zcn]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofzcn"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zcn                 .
       z-esr-cls-fls-999.
           exit.

      *    *===========================================================*
      *    * Start iniziale                                            *
      *    *-----------------------------------------------------------*
       z-esr-str-ini-000.
      *              *-------------------------------------------------*
      *              * Tipo operazione: Start                          *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
      *              *-------------------------------------------------*
      *              * Tipo di confronto : 'not less'                  *
      *              *-------------------------------------------------*
           move      "NL"                 to   f-cfr                  .
      *              *-------------------------------------------------*
      *              * Composizione chiave 'CODSTC'                    *
      *              *-------------------------------------------------*
           move      rr-cod-min           to   rf-pdc-cod-pdc         .
           move      "CODSTC"             to   f-key                  .
      *              *-------------------------------------------------*
      *              * Richiamo modulo di input-output                 *
      *              *-------------------------------------------------*
           move      "pgm/cge/fls/ioc/obj/iofpdc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdc                 .
      *              *-------------------------------------------------*
      *              * Test su successo operazione                     *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     move   "#"           to   OK                     .
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
      *              * Tipo operazione: Read Next                      *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
      *              *-------------------------------------------------*
      *              * Richiamo modulo di input-output                 *
      *              *-------------------------------------------------*
           move      "pgm/cge/fls/ioc/obj/iofpdc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdc                 .
      *              *-------------------------------------------------*
      *              * Test se 'At End'                                *
      *              *-------------------------------------------------*
           if        f-sts                =    e-end-fil
                     move   "#"           to   OK                     .
       z-esr-let-seq-999.
           exit.

      *    *===========================================================*
      *    * Test superamento limiti massimi                           *
      *    *-----------------------------------------------------------*
       z-esr-tst-max-000.
           if        rf-pdc-cod-pdc       >    rr-cod-max
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
           move      rf-pdc-cod-pdc       to   w-mcs-cod-pdc          .
      *              *-------------------------------------------------*
      *              * Codice mastro                                   *
      *              *-------------------------------------------------*
           move      w-mcs-cod-mas        to   w-rot-l02-cod-mas      .
      *              *-------------------------------------------------*
      *              * Codice conto                                    *
      *              *-------------------------------------------------*
           move      w-mcs-cod-con        to   w-rot-l01-cod-con      .
       z-esr-cmp-rot-999.
           exit.

      *    *===========================================================*
      *    * Esecuzione per inizio ciclo                               *
      *    *-----------------------------------------------------------*
       z-esr-ini-cic-000.
      *              *-------------------------------------------------*
      *              * Intestazione foglio                             *
      *              *-------------------------------------------------*
           perform   int-pag-sta-000      thru int-pag-sta-999        .
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
      *              *-------------------------------------------------*
      *              * Se piano dei conti a due livelli si esce        *
      *              *-------------------------------------------------*
           if        w-prs-liv-pdc        =    2
                     go to  z-esr-ini-lr2-999.
      *              *-------------------------------------------------*
      *              * Test se linee residue sufficienti               *
      *              *-------------------------------------------------*
           if        p-res                >    6
                     go to  z-esr-ini-lr2-100.
      *                  *---------------------------------------------*
      *                  * Intestazione foglio                         *
      *                  *---------------------------------------------*
           perform   int-pag-sta-000      thru int-pag-sta-999        .
      *                      *-----------------------------------------*
      *                      * Se errori di i-o : uscita               *
      *                      *-----------------------------------------*
           if        OK                   not  = spaces
                     go to z-esr-ini-lr2-999.
       z-esr-ini-lr2-100.
      *              *-------------------------------------------------*
      *              * Due interlinee subordinate                      *
      *              *-------------------------------------------------*
           move      "VS"                 to   p-ope                  .
           add       2
                     p-lnr              giving p-lin                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       z-esr-ini-lr2-200.
      *                  *---------------------------------------------*
      *                  * Stampa codice e descrizione mastro          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Lettura tabella mastri                  *
      *                      *-----------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODMAS    "         to   f-key                  .
           move      w-rot-l02-cod-mas    to   rf-zma-cod-mas         .
           move      "pgm/cge/fls/ioc/obj/iofzma"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zma                 .
      *                      *-----------------------------------------*
      *                      * Se non trovato                          *
      *                      *-----------------------------------------*
           if        f-sts                =    e-not-fnd
                     move   all "."       to   rf-zma-des-mas         .
      *                      *-----------------------------------------*
      *                      * Visualizzazione codice mastro           *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      02                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "9"                  to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      01                   to   p-pos                  .
           move      rf-zma-cod-mas       to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Visualizzazione descrizione mastro      *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      06                   to   p-pos                  .
           move      rf-zma-des-mas       to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
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
      *              *-------------------------------------------------*
      *              * Test se linee residue sufficienti               *
      *              *-------------------------------------------------*
           if        p-res                >    3
                     go to  z-esr-ini-lr1-100.
      *                  *---------------------------------------------*
      *                  * Intestazione foglio                         *
      *                  *---------------------------------------------*
           perform   int-pag-sta-000      thru int-pag-sta-999        .
      *                      *-----------------------------------------*
      *                      * Se errori di i-o : uscita               *
      *                      *-----------------------------------------*
           if        OK                   not  = spaces
                     go to z-esr-ini-lr1-999.
       z-esr-ini-lr1-100.
      *              *-------------------------------------------------*
      *              * Una interlinea subordinata                      *
      *              *-------------------------------------------------*
           move      "VS"                 to   p-ope                  .
           add       1
                     p-lnr              giving p-lin                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       z-esr-ini-lr1-200.
      *              *-------------------------------------------------*
      *              * Stampa codice e descrizione conto               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura tabella conti                       *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCON    "         to   f-key                  .
           move      w-rot-l02-cod-mas    to   rf-zcn-cod-mas         .
           move      w-rot-l01-cod-con    to   rf-zcn-cod-con         .
           move      "pgm/cge/fls/ioc/obj/iofzcn"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zcn                 .
      *                      *-----------------------------------------*
      *                      * Se non trovato                          *
      *                      *-----------------------------------------*
           if        f-sts                =    e-not-fnd
                     move   all "."       to   rf-zcn-des-con         .
      *                  *---------------------------------------------*
      *                  * Visualizzazione codice conto                *
      *                  *---------------------------------------------*
           if        w-prs-liv-pdc        not  = 3
                     go to z-esr-ini-lr1-300.
      *                      *-----------------------------------------*
      *                      * Visualizzazione codice mastro           *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      02                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "9"                  to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      12                   to   p-pos                  .
           move      rf-zcn-cod-mas       to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Punto di separazione                    *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      01                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      14                   to   p-pos                  .
           move      "."                  to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       z-esr-ini-lr1-300.
      *                      *-----------------------------------------*
      *                      * Visualizzazione codice conto            *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      02                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "9"                  to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           if        w-prs-liv-pdc        =    2
                     move   12            to   p-pos
           else      move   15            to   p-pos                  .
           move      rf-zcn-cod-con       to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione conto           *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           if        w-prs-liv-pdc        =    2
                     move   17            to   p-pos
           else      move   20            to   p-pos                  .
           move      rf-zcn-des-con       to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
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
      *              * Se numero linee residue insufficiente per una   *
      *              * ulteriore linea : intestazione foglio           *
      *              *-------------------------------------------------*
           if        p-res                >    zero
                     go to z-esr-liv-det-200.
       z-esr-liv-det-100.
      *              *-------------------------------------------------*
      *              * Intestazione foglio                             *
      *              *-------------------------------------------------*
           perform   int-pag-sta-000      thru int-pag-sta-999        .
      *                  *---------------------------------------------*
      *                  * Se errori di i-o : uscita                   *
      *                  *---------------------------------------------*
           if        OK                   not  = spaces
                     go to z-esr-liv-det-999.
       z-esr-liv-det-200.
      *              *-------------------------------------------------*
      *              * Composizione e stampa linea                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Codice sottoconto                           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Editing con appoggio a sinistra         *
      *                      *-----------------------------------------*
           move      w-prs-liv-pdc        to   w-edt-cod-pdc-liv      .
           move      rf-pdc-cod-pdc       to   w-edt-cod-pdc-cod      .
           move      spaces               to   w-edt-cod-pdc-edm      .
           perform   edt-pdc-asx-000      thru edt-pdc-asx-999        .
      *                      *-----------------------------------------*
      *                      * Stampa                                  *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      09                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      26                   to   p-pos                  .
           move      w-edt-cod-pdc-edt    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Descrizione                                 *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           if        w-prs-liv-pdc        =    2
                     move 35              to   p-pos
           else      move 38              to   p-pos                  .
           move      rf-pdc-des-pdc       to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Tipo conto                                  *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      01                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      80                   to   p-pos                  .
           move      rf-pdc-tip-cnt       to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       z-esr-liv-det-999.
           exit.

      *    *===========================================================*
      *    * Intestazione foglio                                       *
      *    *-----------------------------------------------------------*
       int-pag-sta-000.
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
                     go to  int-pag-sta-999.
      *              *-------------------------------------------------*
      *              * Linea di '='                                    *
      *              *-------------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      80                   to   p-car                  .
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
      *              *-------------------------------------------------*
      *              * Linea della intestazione principale             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Descrizione azienda                         *
      *                  *---------------------------------------------*
           move      "IA"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           move      s-asx                to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Test se possibile stampare titolo stampa    *
      *                  * sulla stessa riga della descrizione azienda *
      *                  *---------------------------------------------*
           move      "UN"                 to   p-ope                  .
           move      p-lnr                to   p-lin                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           if        p-pos                >    32
                     move   "#"           to   p-mrk
           else      move   spaces        to   p-mrk                  .
           if        p-mrk                not  = spaces
                     go to  int-pag-sta-200.
       int-pag-sta-100.
      *                  *---------------------------------------------*
      *                  * Titolo stampa                               *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      22                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      30                   to   p-pos                  .
           move      "ELENCO PIANO DEI CONTI"
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           if        p-mrk                =    spaces
                     go to  int-pag-sta-200.
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           go to     int-pag-sta-300.
       int-pag-sta-200.
      *                  *---------------------------------------------*
      *                  * Determinazione posizione iniziale per data  *
      *                  * stampa e numero pagina                      *
      *                  *---------------------------------------------*
           subtract  19                   from p-sel-als-sel
                                        giving p-pos                  .
           if        p-pag                <    10
                     add   4              to   p-pos
           else if   p-pag                <    100
                     add   3              to   p-pos
           else if   p-pag                <    1000
                     add   2              to   p-pos
           else if   p-pag                <    10000
                     add   1              to   p-pos                  .
      *                  *---------------------------------------------*
      *                  * Data stampa                                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Richiesta data a 'msegrt'               *
      *                      *-----------------------------------------*
           move      "DT"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-dat                to   p-dat                  .
      *                      *-----------------------------------------*
      *                      * Stampa                                  *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "D"                  to   p-tip                  .
           move      p-lnr                to   p-lin                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Literal 'Pag.'                              *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      04                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           add       10                   to   p-pos                  .
           move      "Pag."               to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Numero pagina                               *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           if        p-pag                <    10
                     move  1              to   p-car
           else if   p-pag                <    100
                     move  2              to   p-car
           else if   p-pag                <    1000
                     move  3              to   p-car
           else if   p-pag                <    10000
                     move  4              to   p-car
           else      move  5              to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "<"                  to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           add       5                    to   p-pos                  .
           move      p-pag                to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Se non e' ancora stato stampato il titolo,  *
      *                  * lo si fa' ora                               *
      *                  *---------------------------------------------*
           if        p-mrk                not  = spaces
                     go to  int-pag-sta-100.
       int-pag-sta-300.
      *              *-------------------------------------------------*
      *              * Linea di '-'                                    *
      *              *-------------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      80                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           move      all   "-"            to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *              *-------------------------------------------------*
      *              * Linea della fincatura                           *
      *              *-------------------------------------------------*
           if        w-prs-liv-pdc        not  = 3
                     go to  int-pag-sta-400.
      *                  *---------------------------------------------*
      *                  * 'Mastro'                                    *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      06                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      01                   to   p-pos                  .
           move      "Mastro"             to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       int-pag-sta-400.
      *                  *---------------------------------------------*
      *                  * 'Conto'                                     *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      05                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      12                   to   p-pos                  .
           move      "Conto"              to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * 'Sottoconto'                                *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      10                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      26                   to   p-pos                  .
           move      "Sottoconto"         to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Posizionamento fisso per lasciare una in-   *
      *                  * terlinea di separazione                     *
      *                  *---------------------------------------------*
           move      "VP"                 to   p-ope                  .
           add       2
                     p-lnr              giving p-lin                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       int-pag-sta-999.
           exit.

      *    *===========================================================*
      *    * Editing del codice sottoconto con appoggio a sx o dx      *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/wedtpdc0.wks"                   .
