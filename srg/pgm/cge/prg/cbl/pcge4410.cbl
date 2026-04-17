       Identification Division.
       Program-Id.                                 pcge4410           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    cge                 *
      *                                Settore:    cli                 *
      *                                   Fase:    cge441              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 29/12/89    *
      *                       Ultima revisione:    NdK del 08/12/21    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Interrogazione su partite aperte cliente    *
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
                     "cli"                                            .
      *        *-------------------------------------------------------*
      *        * Fase gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-fas                  pic  x(06) value
                     "cge441"                                         .
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10) value
                     "pcge4410"                                       .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  i-ide-des                  pic  x(40) value
                     "    INTERROGAZIONE SU PARTITE CLIENTE   "       .

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
      *    * Record logico file [mgt]                                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfmgt"                          .

      *    *===========================================================*
      *    * Record logico file [mgr]                                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfmgr"                          .

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
      *        * Work per regolarizzazione campi alfanumerici          *
      *        *-------------------------------------------------------*
           05  y-reg-are.
               10  y-reg-alf.
                   15  y-reg-chr occurs 20       
                                          pic  x(01)                  .
               10  y-reg-ctr              pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Flag di primo giro esecuzione per richieste           *
      *        *-------------------------------------------------------*
           05  y-flg-prm-gir              pic  x(01)                  .

      *    *===========================================================*
      *    * Area per ciclo di report-program                          *
      *    *-----------------------------------------------------------*
       01  y-crp.
      *        *-------------------------------------------------------*
      *        * Segnale per primo passaggio per esecuzione            *
      *        *-------------------------------------------------------*
           05  y-crp-mrk-uno              pic  x(01)                  .
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
      *    * Work-area controllo modalita' di funzionamento            *
      *    *-----------------------------------------------------------*
       01  w-fun.
           05  w-fun-ric                  pic  x(01)                  .
           05  w-fun-cic                  pic  x(01)                  .
           05  w-fun-aut                  pic  x(01)                  .

      *    *===========================================================*
      *    * Record files                                              *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [cli]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfcli"                          .

      *    *===========================================================*
      *    * Work-area richieste per interrogazione                    *
      *    *-----------------------------------------------------------*
       01  rr.
           05  rr-cod-cli                 pic  9(07)                  .
           05  rr-cod-cli-rag             pic  x(40)                  .
           05  rr-tip-vis                 pic  x(01)                  .

      *    *===========================================================*
      *    * Work per accettazioni campi espansi                       *
      *    *-----------------------------------------------------------*
       01  w-exp.
      *        *-------------------------------------------------------*
      *        * Work per : Tipo visualizzazione                       *
      *        *-------------------------------------------------------*
           05  w-exp-tip-vis.
               10  w-exp-tip-vis-num      pic  9(02)       value 02   .
               10  w-exp-tip-vis-lun      pic  9(02)       value 20   .
               10  w-exp-tip-vis-tbl.
                   15  filler             pic  x(20) value
                            "solo partite Aperte "                    .
                   15  filler             pic  x(20) value
                            "Tutte, anche chiuse "                    .

      *    *===========================================================*
      *    * Work-area per ridefinizione contenuto del mark-point      *
      *    *-----------------------------------------------------------*
       01  w-mpn.
           05  w-mpn-dat-rif              pic  9(07)                  .
           05  w-mpn-num-rif              pic  x(10)                  .
           05  w-mpn-sdo-par              pic s9(13)                  .

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
      *            *---------------------------------------------------*
      *            * Data riferimento                                  *
      *            *---------------------------------------------------*
               10  w-rot-l01-dat-rif      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Numero riferimento                                *
      *            *---------------------------------------------------*
               10  w-rot-l01-num-rif      pic  x(10)                  .

      *    *===========================================================*
      *    * Work-area per bufferizzazione dati partita                *
      *    *-----------------------------------------------------------*
       01  w-par.
      *        *-------------------------------------------------------*
      *        * Data riferimento                                      *
      *        *-------------------------------------------------------*
           05  w-par-dat-rif              pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Numero riferimento                                    *
      *        *-------------------------------------------------------*
           05  w-par-num-rif              pic  x(10)                  .
      *        *-------------------------------------------------------*
      *        * Descrizione causale                                   *
      *        *-------------------------------------------------------*
           05  w-par-des-cau.
               10  w-par-rig-cau occurs 5 pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Saldo partita                                         *
      *        *-------------------------------------------------------*
           05  w-par-sdo-par              pic s9(13)                  .

      *    *===========================================================*
      *    * Work-area generica                                        *
      *    *-----------------------------------------------------------*
       01  w-wrk.
      *        *-------------------------------------------------------*
      *        * Segno partita                                         *
      *        *-------------------------------------------------------*
           05  w-wrk-sgn-par              pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Saldo cliente                                         *
      *        *-------------------------------------------------------*
           05  w-wrk-sdo-cli              pic s9(13)                  .
      *        *-------------------------------------------------------*
      *        * Flag di almeno una partita da visualizzare            *
      *        *-------------------------------------------------------*
           05  w-wrk-flg-uno              pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Contatore generico 'I'                                *
      *        *-------------------------------------------------------*
           05  I                          pic  9(03)                  .

      *    *===========================================================*
      *    * Link-area per accettazione codice cliente                 *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/acmncli0.acl"                   .

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
      *       Main program                                             *
      *----------------------------------------------------------------*
       main-000.
      *              *-------------------------------------------------*
      *              * Dichiarazione di inizio programma               *
      *              *-------------------------------------------------*
           perform   z-dic-ini-pgm-000    thru z-dic-ini-pgm-999      .
           if        OK                   not  = spaces
                     go to main-999.
       main-100.
      *              *-------------------------------------------------*
      *              * Esecuzione routine pre-esecuzione programma     *
      *              *-------------------------------------------------*
           move      spaces               to   OK                     .
           perform   z-pre-exe-pgm-000    thru z-pre-exe-pgm-999      .
           if        OK                   not  = spaces
                     go to  main-900.
      *              *-------------------------------------------------*
      *              * Preparazione tipo funzionamento                 *
      *              *-------------------------------------------------*
           perform   z-pre-tip-fun-000    thru z-pre-tip-fun-999      .
      *              *-------------------------------------------------*
      *              * Segnale primo giro di esecuzione                *
      *              *-------------------------------------------------*
           move      "S"                  to   y-flg-prm-gir          .
       main-200.
      *              *-------------------------------------------------*
      *              * Richiesta parametri di selezione                *
      *              *-------------------------------------------------*
           move      spaces               to   OK                     .
           perform   z-ric-par-sel-000    thru z-ric-par-sel-999      .
           if        OK                   not  = spaces
                     go to main-900.
      *              *-------------------------------------------------*
      *              * Esecuzione interrogazione                       *
      *              *-------------------------------------------------*
           move      spaces               to   OK                     .
           perform   z-qry-rou-pri-000    thru z-qry-rou-pri-999      .
      *              *-------------------------------------------------*
      *              * Segnale non piu' primo giro di esecuzione       *
      *              *-------------------------------------------------*
           move      "N"                  to   y-flg-prm-gir          .
      *              *-------------------------------------------------*
      *              * Test se tipo esecuzione ciclico                 *
      *              *-------------------------------------------------*
           if        w-fun-ric            not  = "S"
                     go to main-900.
           if        w-fun-cic            not  = "S"
                     go to main-900.
           go to     main-200.
       main-900.
      *              *-------------------------------------------------*
      *              * Dichiarazione di fine programma                 *
      *              *-------------------------------------------------*
           perform   z-dic-fin-pgm-000    thru z-dic-fin-pgm-999      .
       main-999.
           exit      program                                          .

      *================================================================*
      *       Routines fisse                                           *
      *================================================================*

      *================================================================*
      *       Dichiarazione di inizio programma                        *
      *----------------------------------------------------------------*
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

      *================================================================*
      *       Dichiarazione di fine programma                          *
      *----------------------------------------------------------------*
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

      *================================================================*
      *       Richiesta parametri di selezione                         *
      *----------------------------------------------------------------*
       z-ric-par-sel-000.
      *              *-------------------------------------------------*
      *              * Test se richieste ad utente                     *
      *              *-------------------------------------------------*
           if        w-fun-ric            not  = "S"
                     go to z-ric-par-sel-999.
      *              *-------------------------------------------------*
      *              * Azzeramento iniziale work-area richieste        *
      *              *-------------------------------------------------*
           perform   z-azz-ini-ric-000    thru z-azz-ini-ric-999      .
      *              *-------------------------------------------------*
      *              * Titolo programma a video                        *
      *              *-------------------------------------------------*
           perform   z-vis-tit-pgm-000    thru z-vis-tit-pgm-999      .
      *              *-------------------------------------------------*
      *              * Impostazione richieste per interrogazione       *
      *              *-------------------------------------------------*
           move      spaces               to   OK                     .
           perform   z-imp-ric-int-000    thru z-imp-ric-int-999      .
           if        OK                   not  = spaces
                     go to z-ric-par-sel-999.
      *              *-------------------------------------------------*
      *              * Regolarizzazione delle richieste                *
      *              *-------------------------------------------------*
           perform   z-reg-ric-int-000    thru z-reg-ric-int-999      .
       z-ric-par-sel-999.
           exit.

      *================================================================*
      *       Visualizzazione titolo programma                         *
      *----------------------------------------------------------------*
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

      *================================================================*
      *       Routine di interrogazione principale                     *
      *----------------------------------------------------------------*
       z-qry-rou-pri-000.
      *              *-------------------------------------------------*
      *              * Determinazione function-keys in Mark-points     *
      *              *-------------------------------------------------*
           perform   z-qry-det-fky-000    thru z-qry-det-fky-999      .
      *              *-------------------------------------------------*
      *              * Begin o Begin Automatico                        *
      *              *-------------------------------------------------*
           if        w-fun-aut            =    "S"
                     move   "BA"          to   v-ope
           else      move   "BE"          to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
           perform   z-qry-opn-fls-000    thru z-qry-opn-fls-999      .
           if        v-mrk                not  = spaces
                     move   spaces        to   v-mrk
                     go to  z-qry-rou-pri-950.
      *              *-------------------------------------------------*
      *              * Inizializzazione markers                        *
      *              *-------------------------------------------------*
           move      spaces               to   y-crp-mrk-uno          .
      *              *-------------------------------------------------*
      *              * Inizializzazione area per rotture di livello    *
      *              *-------------------------------------------------*
           move      spaces               to   w-rot                  .
      *              *-------------------------------------------------*
      *              * Start iniziale                                  *
      *              *-------------------------------------------------*
           perform   qry-str-ini-000      thru qry-str-ini-999        .
           if        v-mrk                not  = spaces
                     move   spaces        to   v-mrk
                     go to  z-qry-rou-pri-600.
       z-qry-rou-pri-100.
      *              *-------------------------------------------------*
      *              * Salvataggio area rottura in area precedente     *
      *              *-------------------------------------------------*
           move      w-rot-l05            to   y-crp-sav-l05          .
           move      w-rot-l04            to   y-crp-sav-l04          .
           move      w-rot-l03            to   y-crp-sav-l03          .
           move      w-rot-l02            to   y-crp-sav-l02          .
           move      w-rot-l01            to   y-crp-sav-l01          .
       z-qry-rou-pri-200.
      *              *-------------------------------------------------*
      *              * Lettura sequenziale                             *
      *              *-------------------------------------------------*
           perform   qry-let-seq-000      thru qry-let-seq-999        .
           if        v-mrk                not  = spaces
                     move   spaces        to   v-mrk
                     go to  z-qry-rou-pri-500.
      *              *-------------------------------------------------*
      *              * Test se superamento limiti massimi              *
      *              *-------------------------------------------------*
           perform   qry-tst-max-000      thru qry-tst-max-999        .
           if        v-mrk                not  = spaces
                     move   spaces        to   v-mrk
                     go to  z-qry-rou-pri-500.
      *              *-------------------------------------------------*
      *              * Selezione su record letto                       *
      *              *-------------------------------------------------*
           perform   z-qry-sel-rec-000    thru z-qry-sel-rec-999      .
           if        v-mrk                not  = spaces
                     move   spaces        to   v-mrk
                     go to  z-qry-rou-pri-200.
      *              *-------------------------------------------------*
      *              * Composizione area per tests di rottura          *
      *              *-------------------------------------------------*
           perform   qry-cmp-rot-000      thru qry-cmp-rot-999        .
      *              *-------------------------------------------------*
      *              * Test se primo passaggio                         *
      *              *-------------------------------------------------*
           if        y-crp-mrk-uno        not  = spaces
                     go to z-qry-rou-pri-300.
      *                  *---------------------------------------------*
      *                  * Inizio di tutti i livelli                   *
      *                  *---------------------------------------------*
           perform   z-qry-rou-pri-790    thru z-qry-rou-pri-791      .
           perform   z-qry-rou-pri-750    thru z-qry-rou-pri-751      .
           perform   z-qry-rou-pri-740    thru z-qry-rou-pri-741      .
           perform   z-qry-rou-pri-730    thru z-qry-rou-pri-731      .
           perform   z-qry-rou-pri-720    thru z-qry-rou-pri-721      .
           perform   z-qry-rou-pri-710    thru z-qry-rou-pri-711      .
           go to     z-qry-rou-pri-400.
       z-qry-rou-pri-300.
      *              *-------------------------------------------------*
      *              * Se rottura del 5. livello                       *
      *              *-------------------------------------------------*
           if        w-rot-l05            =    y-crp-sav-l05
                     go to z-qry-rou-pri-310.
           move      w-rot                to   y-crp-sav-rot          .
           move      y-crp-sav-l05        to   w-rot-l05              .
           move      y-crp-sav-l04        to   w-rot-l04              .
           move      y-crp-sav-l03        to   w-rot-l03              .
           move      y-crp-sav-l02        to   w-rot-l02              .
           move      y-crp-sav-l01        to   w-rot-l01              .
           perform   z-qry-rou-pri-810    thru z-qry-rou-pri-811      .
           perform   z-qry-rou-pri-820    thru z-qry-rou-pri-821      .
           perform   z-qry-rou-pri-830    thru z-qry-rou-pri-831      .
           perform   z-qry-rou-pri-840    thru z-qry-rou-pri-841      .
           perform   z-qry-rou-pri-850    thru z-qry-rou-pri-851      .
           move      y-crp-sav-rot        to   w-rot                  .
           perform   z-qry-rou-pri-750    thru z-qry-rou-pri-751      .
           perform   z-qry-rou-pri-740    thru z-qry-rou-pri-741      .
           perform   z-qry-rou-pri-730    thru z-qry-rou-pri-731      .
           perform   z-qry-rou-pri-720    thru z-qry-rou-pri-721      .
           perform   z-qry-rou-pri-710    thru z-qry-rou-pri-711      .
           go to     z-qry-rou-pri-400.
       z-qry-rou-pri-310.
      *              *-------------------------------------------------*
      *              * Se rottura del 4. livello                       *
      *              *-------------------------------------------------*
           if        w-rot-l04            =    y-crp-sav-l04
                     go to z-qry-rou-pri-320.
           move      w-rot                to   y-crp-sav-rot          .
           move      y-crp-sav-l04        to   w-rot-l04              .
           move      y-crp-sav-l03        to   w-rot-l03              .
           move      y-crp-sav-l02        to   w-rot-l02              .
           move      y-crp-sav-l01        to   w-rot-l01              .
           perform   z-qry-rou-pri-810    thru z-qry-rou-pri-811      .
           perform   z-qry-rou-pri-820    thru z-qry-rou-pri-821      .
           perform   z-qry-rou-pri-830    thru z-qry-rou-pri-831      .
           perform   z-qry-rou-pri-840    thru z-qry-rou-pri-841      .
           move      y-crp-sav-rot        to   w-rot                  .
           perform   z-qry-rou-pri-740    thru z-qry-rou-pri-741      .
           perform   z-qry-rou-pri-730    thru z-qry-rou-pri-731      .
           perform   z-qry-rou-pri-720    thru z-qry-rou-pri-721      .
           perform   z-qry-rou-pri-710    thru z-qry-rou-pri-711      .
           go to     z-qry-rou-pri-400.
       z-qry-rou-pri-320.
      *              *-------------------------------------------------*
      *              * Se rottura del 3. livello                       *
      *              *-------------------------------------------------*
           if        w-rot-l03            =    y-crp-sav-l03
                     go to z-qry-rou-pri-330.
           move      w-rot                to   y-crp-sav-rot          .
           move      y-crp-sav-l03        to   w-rot-l03              .
           move      y-crp-sav-l02        to   w-rot-l02              .
           move      y-crp-sav-l01        to   w-rot-l01              .
           perform   z-qry-rou-pri-810    thru z-qry-rou-pri-811      .
           perform   z-qry-rou-pri-820    thru z-qry-rou-pri-821      .
           perform   z-qry-rou-pri-830    thru z-qry-rou-pri-831      .
           move      y-crp-sav-rot        to   w-rot                  .
           perform   z-qry-rou-pri-730    thru z-qry-rou-pri-731      .
           perform   z-qry-rou-pri-720    thru z-qry-rou-pri-721      .
           perform   z-qry-rou-pri-710    thru z-qry-rou-pri-711      .
           go to     z-qry-rou-pri-400.
       z-qry-rou-pri-330.
      *              *-------------------------------------------------*
      *              * Se rottura del 2. livello                       *
      *              *-------------------------------------------------*
           if        w-rot-l02            =    y-crp-sav-l02
                     go to z-qry-rou-pri-340.
           move      w-rot                to   y-crp-sav-rot          .
           move      y-crp-sav-l02        to   w-rot-l02              .
           move      y-crp-sav-l01        to   w-rot-l01              .
           perform   z-qry-rou-pri-810    thru z-qry-rou-pri-811      .
           perform   z-qry-rou-pri-820    thru z-qry-rou-pri-821      .
           move      y-crp-sav-rot        to   w-rot                  .
           perform   z-qry-rou-pri-720    thru z-qry-rou-pri-721      .
           perform   z-qry-rou-pri-710    thru z-qry-rou-pri-711      .
           go to     z-qry-rou-pri-400.
       z-qry-rou-pri-340.
      *              *-------------------------------------------------*
      *              * Se rottura del 1. livello                       *
      *              *-------------------------------------------------*
           if        w-rot-l01            =    y-crp-sav-l01
                     go to z-qry-rou-pri-400.
           move      w-rot                to   y-crp-sav-rot          .
           move      y-crp-sav-l01        to   w-rot-l01              .
           perform   z-qry-rou-pri-810    thru z-qry-rou-pri-811      .
           move      y-crp-sav-rot        to   w-rot                  .
           perform   z-qry-rou-pri-710    thru z-qry-rou-pri-711      .
       z-qry-rou-pri-400.
      *              *-------------------------------------------------*
      *              * Se segnale di interruzione attivo : fine ciclo  *
      *              *-------------------------------------------------*
           if        OK                   not  = spaces
                     go to z-qry-rou-pri-900.
      *              *-------------------------------------------------*
      *              * Livello di dettaglio                            *
      *              *-------------------------------------------------*
           perform   z-qry-liv-det-000    thru z-qry-liv-det-999      .
      *              *-------------------------------------------------*
      *              * Se segnale di interruzione : fine ciclo         *
      *              *-------------------------------------------------*
           if        OK                   not  = spaces
                     go to z-qry-rou-pri-900.
      *              *-------------------------------------------------*
      *              * Segnale di passaggio successivo al primo        *
      *              *-------------------------------------------------*
           move      "#"                  to   y-crp-mrk-uno          .
      *              *-------------------------------------------------*
      *              * Riciclo a lettura sequenziale file principale   *
      *              *-------------------------------------------------*
           go to     z-qry-rou-pri-100.
       z-qry-rou-pri-500.
      *              *-------------------------------------------------*
      *              * Test se almeno un passaggio                     *
      *              *-------------------------------------------------*
           if        y-crp-mrk-uno        =    spaces
                     go to z-qry-rou-pri-600.
      *              *-------------------------------------------------*
      *              * Fine di tutti i livelli                         *
      *              *-------------------------------------------------*
           perform   z-qry-rou-pri-810    thru z-qry-rou-pri-811      .
           perform   z-qry-rou-pri-820    thru z-qry-rou-pri-821      .
           perform   z-qry-rou-pri-830    thru z-qry-rou-pri-831      .
           perform   z-qry-rou-pri-840    thru z-qry-rou-pri-841      .
           perform   z-qry-rou-pri-850    thru z-qry-rou-pri-851      .
           perform   z-qry-rou-pri-890    thru z-qry-rou-pri-891      .
           if        OK                   not  = spaces
                     go to z-qry-rou-pri-900.
      *              *-------------------------------------------------*
      *              * Stop                                            *
      *              *-------------------------------------------------*
           move      "ST"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Subroutine di avanzamento pagina                *
      *              *-------------------------------------------------*
           perform   z-qry-pag-adv-000    thru z-qry-pag-adv-999      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     z-qry-rou-pri-900.
       z-qry-rou-pri-600.
      *              *-------------------------------------------------*
      *              * Esecuzione per nessuna registrazione da elab.   *
      *              *-------------------------------------------------*
           perform   z-qry-nes-ela-000    thru z-qry-nes-ela-999      .
           go to     z-qry-rou-pri-900.
       z-qry-rou-pri-710.
      *              *-------------------------------------------------*
      *              * Esecuzione per inizio 1. livello di rottura     *
      *              *-------------------------------------------------*
           if        OK                   not  = spaces
                     go to z-qry-rou-pri-711.
           perform   z-qry-ini-lr1-000    thru z-qry-ini-lr1-999      .
       z-qry-rou-pri-711.
           exit.
       z-qry-rou-pri-720.
      *              *-------------------------------------------------*
      *              * Esecuzione per inizio 2. livello di rottura     *
      *              *-------------------------------------------------*
           if        OK                   not  = spaces
                     go to z-qry-rou-pri-721.
           perform   z-qry-ini-lr2-000    thru z-qry-ini-lr2-999      .
       z-qry-rou-pri-721.
           exit.
       z-qry-rou-pri-730.
      *              *-------------------------------------------------*
      *              * Esecuzione per inizio 3. livello di rottura     *
      *              *-------------------------------------------------*
           if        OK                   not  = spaces
                     go to z-qry-rou-pri-731.
           perform   z-qry-ini-lr3-000    thru z-qry-ini-lr3-999      .
       z-qry-rou-pri-731.
           exit.
       z-qry-rou-pri-740.
      *              *-------------------------------------------------*
      *              * Esecuzione per inizio 4. livello di rottura     *
      *              *-------------------------------------------------*
           if        OK                   not  = spaces
                     go to z-qry-rou-pri-741.
           perform   z-qry-ini-lr4-000    thru z-qry-ini-lr4-999      .
       z-qry-rou-pri-741.
           exit.
       z-qry-rou-pri-750.
      *              *-------------------------------------------------*
      *              * Esecuzione per inizio 5. livello di rottura     *
      *              *-------------------------------------------------*
           if        OK                   not  = spaces
                     go to z-qry-rou-pri-751.
           perform   z-qry-ini-lr5-000    thru z-qry-ini-lr5-999      .
       z-qry-rou-pri-751.
           exit.
       z-qry-rou-pri-790.
      *              *-------------------------------------------------*
      *              * Esecuzione per inizio ciclo                     *
      *              *-------------------------------------------------*
           if        OK                   not  = spaces
                     go to z-qry-rou-pri-791.
           perform   qry-ini-cic-000      thru qry-ini-cic-999        .
       z-qry-rou-pri-791.
           exit.
       z-qry-rou-pri-810.
      *              *-------------------------------------------------*
      *              * Esecuzione per fine 1. livello di rottura       *
      *              *-------------------------------------------------*
           if        OK                   not  = spaces
                     go to z-qry-rou-pri-811.
           perform   z-qry-fin-lr1-000    thru z-qry-fin-lr1-999      .
       z-qry-rou-pri-811.
           exit.
       z-qry-rou-pri-820.
      *              *-------------------------------------------------*
      *              * Esecuzione per fine 2. livello di rottura       *
      *              *-------------------------------------------------*
           if        OK                   not  = spaces
                     go to z-qry-rou-pri-821.
           perform   z-qry-fin-lr2-000    thru z-qry-fin-lr2-999      .
       z-qry-rou-pri-821.
           exit.
       z-qry-rou-pri-830.
      *              *-------------------------------------------------*
      *              * Esecuzione per fine 3. livello di rottura       *
      *              *-------------------------------------------------*
           if        OK                   not  = spaces
                     go to z-qry-rou-pri-831.
           perform   z-qry-fin-lr3-000    thru z-qry-fin-lr3-999      .
       z-qry-rou-pri-831.
           exit.
       z-qry-rou-pri-840.
      *              *-------------------------------------------------*
      *              * Esecuzione per fine 4. livello di rottura       *
      *              *-------------------------------------------------*
           if        OK                   not  = spaces
                     go to z-qry-rou-pri-841.
           perform   z-qry-fin-lr4-000    thru z-qry-fin-lr4-999      .
       z-qry-rou-pri-841.
           exit.
       z-qry-rou-pri-850.
      *              *-------------------------------------------------*
      *              * Esecuzione per fine 5. livello di rottura       *
      *              *-------------------------------------------------*
           if        OK                   not  = spaces
                     go to z-qry-rou-pri-851.
           perform   z-qry-fin-lr5-000    thru z-qry-fin-lr5-999      .
       z-qry-rou-pri-851.
           exit.
       z-qry-rou-pri-890.
      *              *-------------------------------------------------*
      *              * Esecuzione per fine ciclo                       *
      *              *-------------------------------------------------*
           if        OK                   not  = spaces
                     go to z-qry-rou-pri-891.
           perform   z-qry-fin-cic-000    thru z-qry-fin-cic-999      .
       z-qry-rou-pri-891.
           exit.
       z-qry-rou-pri-900.
      *              *-------------------------------------------------*
      *              * Close files                                     *
      *              *-------------------------------------------------*
           perform   z-qry-cls-fls-000    thru z-qry-cls-fls-999      .
       z-qry-rou-pri-950.
      *              *-------------------------------------------------*
      *              * End                                             *
      *              *-------------------------------------------------*
           move      "EN"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       z-qry-rou-pri-999.
           exit.

      *================================================================*
      *       Subroutine di avanzamento pagina                         *
      *----------------------------------------------------------------*
       z-qry-pag-adv-000.
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
                     go to z-qry-pag-adv-999.
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata da operatore        *
      *                  *---------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "#"            to   OK
                     go to z-qry-pag-adv-999.
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
           if        OK                   =    spaces
                     go to z-qry-pag-adv-000.
       z-qry-pag-adv-999.
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
      *    * Routine pre-esecuzione programma                          *
      *    *                                                           *
      *    * Uscita  : OK = spaces : continua l'esecuzione             *
      *    *                "#"    : terminazione programma            *
      *    *-----------------------------------------------------------*
       z-pre-exe-pgm-000.
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
      *              * Determinazione eventuale variabile di i.p.c.    *
      *              * relativa al codice cliente                      *
      *              *-------------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "cod-cli"            to   s-var                  .
           move      "-"                  to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move   s-num         to   rr-cod-cli
                     move   "A"           to   rr-tip-vis             .
      *                  *---------------------------------------------*
      *                  * Lettura ragione sociale cliente             *
      *                  *---------------------------------------------*
           if        rr-cod-cli           =    zero
                     go to  z-pre-exe-pgm-999.
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofcli"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cli                 .
           move      "NO"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofcli"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cli                 .
           move      rr-cod-cli           to   rf-cli-cod-cli         .
           move      "RK"                 to   f-ope                  .
           move      "CODCLI    "         to   f-key                  .
           move      "pgm/cge/fls/ioc/obj/iofcli"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cli                 .
           if        f-sts                not  = e-not-err
                     move   zero          to   rr-cod-cli
                     move   spaces        to   rr-tip-vis
           else      move   rf-cli-rag-soc
                                          to   rr-cod-cli-rag         .
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofcli"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cli                 .
       z-pre-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Preparazione tipo funzionamento programma                 *
      *    *-----------------------------------------------------------*
       z-pre-tip-fun-000.
      *              *-------------------------------------------------*
      *              * Si/No richieste ad utente                       *
      *              *                                                 *
      *              * S : Verra' eseguita la routine ric-prm-000/999  *
      *              * N : Esecuzione immediata senza richieste        *
      *              *-------------------------------------------------*
           if        rr-cod-cli           not  = zero and
                     rr-tip-vis           =    "A"
                     move   "N"           to   w-fun-ric
           else      move   "S"           to   w-fun-ric              .
      *              *-------------------------------------------------*
      *              * Si/No funzionamento ciclico                     *
      *              *                                                 *
      *              * S : Dopo una interrogazione viene data la pos-  *
      *              *     sibilita' di eseguirne un'altra.  Ammissi-  *
      *              *     bile solo se parametro precedente a "S"     *
      *              * N : Dopo una interrogazione il programma ter-   *
      *              *     mina                                        *
      *              *-------------------------------------------------*
           move      "S"                  to   w-fun-cic              .
      *              *-------------------------------------------------*
      *              * Si/No funzionamento automatico                  *
      *              *                                                 *
      *              * S : Vengono prima generate tutte le pagine di   *
      *              *     interrogazione, e poi si da' all'utente la  *
      *              *     possibilita' di scorrerle                   *
      *              * N : Le pagine vengono generate una alla volta e *
      *              *     l'utente puo' avanzare o scorrere le prece- *
      *              *     denti                                       *
      *              *-------------------------------------------------*
           move      "S"                  to   w-fun-aut              .
       z-pre-tip-fun-999.
           exit.

      *    *===========================================================*
      *    * Azzeramento iniziale work-area richieste                  *
      *    *                                                           *
      *    * A disposizione y-flg-prm-gir : S = Primo giro             *
      *    *                                N = Giro successivo al     *
      *    *                                    primo                  *
      *    *-----------------------------------------------------------*
       z-azz-ini-ric-000.
           move      zero                 to   rr-cod-cli             .
           move      spaces               to   rr-cod-cli-rag         .
           move      spaces               to   rr-tip-vis             .
       z-azz-ini-ric-999.
           exit.

      *    *===========================================================*
      *    * Impostazione richieste per interrogazione                 *
      *    *                                                           *
      *    * Uscita  : OK = spaces : continua l'esecuzione             *
      *    *                "#"    : terminazione programma            *
      *    *-----------------------------------------------------------*
       z-imp-ric-int-000.
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice cliente         *
      *              *-------------------------------------------------*
           perform   cod-mne-cli-opn-000  thru cod-mne-cli-opn-999    .
      *              *-------------------------------------------------*
      *              * Open file [cli]                                 *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofcli"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cli                 .
      *              *-------------------------------------------------*
      *              * Visualizzazione prompt richieste                *
      *              *-------------------------------------------------*
           perform   vis-pmt-ric-000      thru vis-pmt-ric-999        .
      *              *-------------------------------------------------*
      *              * Accettazione record richieste                   *
      *              *-------------------------------------------------*
           perform   acc-rec-ric-000      thru acc-rec-ric-999        .
      *                  *---------------------------------------------*
      *                  * Test su tipo uscita                         *
      *                  *---------------------------------------------*
           if        v-key                =    "EXIT"
                     move   "#"           to   OK                     .
      *              *-------------------------------------------------*
      *              * Close file [cli]                                *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofcli"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cli                 .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice cliente        *
      *              *-------------------------------------------------*
           perform   cod-mne-cli-cls-000  thru cod-mne-cli-cls-999    .
       z-imp-ric-int-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt richieste                          *
      *    *-----------------------------------------------------------*
       vis-pmt-ric-000.
      *              *-------------------------------------------------*
      *              * 'Codice cliente               :'                *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice cliente             :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * 'Tipo visualizzazione         :'                *
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
       vis-pmt-ric-999.
           exit.

      *    *===========================================================*
      *    * Accettazione record richieste                             *
      *    *-----------------------------------------------------------*
       acc-rec-ric-000.
      *              *-------------------------------------------------*
      *              * Codice cliente                                  *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-cli-ope      .
           move      rr-cod-cli           to   w-cod-mne-cli-cod      .
           move      10                   to   w-cod-mne-cli-lin      .
           move      30                   to   w-cod-mne-cli-pos      .
           move      10                   to   w-cod-mne-cli-rln      .
           move      41                   to   w-cod-mne-cli-rps      .
           move      zero                 to   w-cod-mne-cli-vln      .
           move      zero                 to   w-cod-mne-cli-vps      .
           move      zero                 to   w-cod-mne-cli-lln      .
           move      zero                 to   w-cod-mne-cli-lps      .
           move      "<B"                 to   v-edm                  .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "EXIT"               to   v-pfk (20)             .
           perform   cod-mne-cli-cll-000  thru cod-mne-cli-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-mne-cli-foi-000  thru cod-mne-cli-foi-999    .
       acc-rec-ric-010.
           perform   cod-mne-cli-cll-000  thru cod-mne-cli-cll-999    .
           if        w-cod-mne-cli-ope    =    "F+"
                     go to acc-rec-ric-015.
           if        w-cod-mne-cli-ope    =    "AC"
                     go to acc-rec-ric-020.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-rec-ric-015.
           perform   cod-mne-cli-foi-000  thru cod-mne-cli-foi-999    .
           go to     acc-rec-ric-010.
       acc-rec-ric-020.
           move      w-cod-mne-cli-cod    to   v-num                  .
           move      v-num                to   rr-cod-cli             .
      *                  *---------------------------------------------*
      *                  * Se premuta function-key 'EXIT' si esce      *
      *                  *---------------------------------------------*
           if        v-key                =    "EXIT"
                     go to  acc-rec-ric-999.
      *                  *---------------------------------------------*
      *                  * Lettura ragione sociale cliente             *
      *                  *---------------------------------------------*
           if        rr-cod-cli           =    zero
                     move   spaces        to   rr-cod-cli-rag
                     go to  acc-rec-ric-060.
           move      rr-cod-cli           to   rf-cli-cod-cli         .
           move      "RK"                 to   f-ope                  .
           move      "CODCLI    "         to   f-key                  .
           move      "pgm/cge/fls/ioc/obj/iofcli"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cli                 .
           if        f-sts                not  = e-not-err
                     move   all "."       to   rf-cli-rag-soc         .
           move      rf-cli-rag-soc       to   rr-cod-cli-rag         .
       acc-rec-ric-060.
      *                  *---------------------------------------------*
      *                  * Visualizzazione ragione sociale cliente     *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      rr-cod-cli-rag       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Controllo valore impostato                  *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err or
                     rr-cod-cli           =    zero
                     go to  acc-rec-ric-000.
      *                  *---------------------------------------------*
      *                  * Se premuta function-key 'DO  ' si esce      *
      *                  *---------------------------------------------*
           if        v-key                =    "DO  "
                     go to  acc-rec-ric-999.
       acc-rec-ric-100.
      *              *-------------------------------------------------*
      *              * Tipo visualizzazione                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione valore di default              *
      *                  *---------------------------------------------*
           if        rr-tip-vis           =    spaces
                     move   "A"           to   rr-tip-vis             .
      *                  *---------------------------------------------*
      *                  * Visualizzazione note operative              *
      *                  *---------------------------------------------*
           move      "NT"                 to   v-ope                  .
           move      "(A): Solo partite aperte  -  (T): Sia partite aper
      -              "te che chiuse"      to   v-not                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Accettazione                                *
      *                  *---------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "EXIT"               to   v-pfk (1)              .
           move      "DOWN"               to   v-pfk (2)              .
           move      "DO  "               to   v-pfk (3)              .
           move      "UP  "               to   v-pfk (4)              .
           move      rr-tip-vis           to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-alf                to   rr-tip-vis             .
      *                  *---------------------------------------------*
      *                  * Cancellazione note                          *
      *                  *---------------------------------------------*
           move      "NT"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Se premuta function-key 'EXIT' si esce      *
      *                  *---------------------------------------------*
           if        v-key                =    "EXIT"
                     go to  acc-rec-ric-999.
      *                  *---------------------------------------------*
      *                  * Visualizzazione literal                     *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      29                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      41                   to   v-pos                  .
           if        rr-tip-vis           =    "A"
                     move   "Solo partite aperte"
                                          to   v-alf
           else if   rr-tip-vis           =    "T"
                     move   "Sia partite aperte che chiuse"
                                          to   v-alf
           else if   rr-tip-vis           =    " "
                     move   spaces        to   v-alf
           else      move   all "."       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Se premuta function-key 'UP  '              *
      *                  *---------------------------------------------*
           if        v-key                not  = "UP  "
                     go to  acc-rec-ric-120.
           if        rr-tip-vis           =    "A" or
                     rr-tip-vis           =    "T" or
                     rr-tip-vis           =    " "
                     go to  acc-rec-ric-000
           else      go to  acc-rec-ric-100.
       acc-rec-ric-120.
      *                  *---------------------------------------------*
      *                  * Controllo valore impostato                  *
      *                  *---------------------------------------------*
           if        rr-tip-vis           not  = "A" and
                     rr-tip-vis           not  = "T"
                     go to  acc-rec-ric-100.
      *                  *---------------------------------------------*
      *                  * Se premuta function-key 'DO  ' si esce      *
      *                  *---------------------------------------------*
           if        v-key                =    "DO  "
                     go to  acc-rec-ric-999.
       acc-rec-ric-900.
      *              *-------------------------------------------------*
      *              * Richiesta conferma parametri impostati          *
      *              *-------------------------------------------------*
           move      "MX"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      "Tutto esatto (S/N/E) ?"
                                          to   v-not                  .
           move      "S"                  to   v-alf                  .
           move      "SNE"                to   v-msk                  .
           move      "DO  "               to   v-pfk (1)              .
           move      "EXIT"               to   v-pfk (2)              .
           move      "UP  "               to   v-pfk (3)              .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Test su risposta dell'utente                *
      *                  *---------------------------------------------*
           if        v-key                =    spaces
                     if      v-alf        =    "S"
                             move  "DO  " to   v-key
                             go to acc-rec-ric-999
                     else if v-alf        =    "N"
                             go to acc-rec-ric-000
                     else    move  "EXIT" to   v-key
                             go to acc-rec-ric-999.
           if        v-key                =    "UP  "
                     go to  acc-rec-ric-100.
       acc-rec-ric-999.
           exit.

      *    *===========================================================*
      *    * Regolarizzazione richieste per interrogazione             *
      *    *                                                           *
      *    *   A disposizione routine z-reg-cam-alf-000/999 per la re- *
      *    *   golarizzazione campo alfanumerico con padding di "z".   *
      *    *   Variabile di input/output per suddetta routine:         *
      *    *   - y-reg-alf (max 20 caratteri)                          *
      *    *-----------------------------------------------------------*
       z-reg-ric-int-000.
      *              *-------------------------------------------------*
      *              * Tipo visualizzazione                            *
      *              *-------------------------------------------------*
           if        rr-tip-vis           =    spaces
                     move   "A"           to   rr-tip-vis             .
       z-reg-ric-int-999.
           exit.

      *    *===========================================================*
      *    * Open files : se errori v-mrk:= "#"                        *
      *    *-----------------------------------------------------------*
       z-qry-opn-fls-000.
      *              *-------------------------------------------------*
      *              * Apertura file [mgt]                             *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmgt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgt                 .
      *              *-------------------------------------------------*
      *              * Apertura file 'mgr'                             *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmgr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgr                 .
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
       z-qry-opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Close files : se errori v-mrk:= "#"                       *
      *    *-----------------------------------------------------------*
       z-qry-cls-fls-000.
      *              *-------------------------------------------------*
      *              * Chiusura file [mgt]                             *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmgt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgt                 .
      *              *-------------------------------------------------*
      *              * Chiusura file 'mgr'                             *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmgr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgr                 .
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
       z-qry-cls-fls-999.
           exit.

      *    *===========================================================*
      *    * Start iniziale - se non valida v-mrk:= "#"                *
      *    *-----------------------------------------------------------*
       qry-str-ini-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di visualizzazione         *
      *              *-------------------------------------------------*
           move      spaces               to   w-wrk-flg-uno          .
      *              *-------------------------------------------------*
      *              * Start per chiave generica                       *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "ARCRIF"             to   f-key                  .
           move      "C"                  to   rf-mgr-tip-arc         .
           move      rr-cod-cli           to   rf-mgr-cod-arc         .
           move      zero                 to   rf-mgr-dat-rif         .
           move      spaces               to   rf-mgr-num-rif         .
           move      zero                 to   rf-mgr-dat-reg         .
           move      zero                 to   rf-mgr-num-prt         .
           move      zero                 to   rf-mgr-num-prg         .
      *              *-------------------------------------------------*
      *              * Richiamo modulo di input-output                 *
      *              *-------------------------------------------------*
           move      "pgm/cge/fls/ioc/obj/iofmgr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgr                 .
      *                  *---------------------------------------------*
      *                  * Test su successo operazione                 *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     move   "#"           to   v-mrk                  .
       qry-str-ini-999.
           exit.

      *    *===========================================================*
      *    * Messaggio per nessuna registrazione da elaborare          *
      *    *-----------------------------------------------------------*
       z-qry-nes-ela-000.
           move      "ME"                 to   v-ope                  .
           move      "Nessuna registrazione entro i limiti impostati"
                                          to   v-not                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       z-qry-nes-ela-999.
           exit.

      *    *===========================================================*
      *    * Lettura sequenziale - se fine v-mrk:= "#"                 *
      *    *-----------------------------------------------------------*
       qry-let-seq-000.
      *              *-------------------------------------------------*
      *              * Indicatore di programma in esecuzione           *
      *              *-------------------------------------------------*
           move      "IE"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
       qry-let-seq-200.
      *              *-------------------------------------------------*
      *              * Tipo operazione: Read Next                      *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmgr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgr                 .
      *                  *---------------------------------------------*
      *                  * Test se 'At End'                            *
      *                  *---------------------------------------------*
           if        f-sts                =    e-end-fil
                     move   "*"           to   v-mrk                  .
       qry-let-seq-999.
           exit.

      *    *===========================================================*
      *    * Test superamento limiti massimi - se si' v-mrk:= "#"      *
      *    *-----------------------------------------------------------*
       qry-tst-max-000.
      *              *-------------------------------------------------*
      *              * Test su tipo e codice archivio                  *
      *              *-------------------------------------------------*
           if        rf-mgr-tip-arc       not  = "C"        or
                     rf-mgr-cod-arc       not  = rr-cod-cli
                     move   "#"           to   v-mrk                  .
       qry-tst-max-999.
           exit.

      *    *===========================================================*
      *    * Selezione su record letto - se da ignorare v-mrk:= "#"    *
      *    *-----------------------------------------------------------*
       z-qry-sel-rec-000.
       z-qry-sel-rec-999.
           exit.

      *    *===========================================================*
      *    * Composizione area di rottura w-rot                        *
      *    *-----------------------------------------------------------*
       qry-cmp-rot-000.
           move      rf-mgr-dat-rif       to   w-rot-l01-dat-rif      .
           move      rf-mgr-num-rif       to   w-rot-l01-num-rif      .
       qry-cmp-rot-999.
           exit.

      *    *===========================================================*
      *    * Esecuzione per inizio ciclo                               *
      *    *                                                           *
      *    * Uscita  : OK = spaces : continua l'esecuzione             *
      *    *                "#"    : terminazione programma            *
      *    *-----------------------------------------------------------*
       qry-ini-cic-000.
      *              *-------------------------------------------------*
      *              * Azzeramento saldo cliente                       *
      *              *-------------------------------------------------*
           move      zero                 to   w-wrk-sdo-cli          .
       qry-ini-cic-999.
           exit.

      *    *===========================================================*
      *    * Esecuzione per fine ciclo                                 *
      *    *                                                           *
      *    * Uscita  : OK = spaces : continua l'esecuzione             *
      *    *                "#"    : terminazione programma            *
      *    *-----------------------------------------------------------*
       z-qry-fin-cic-000.
      *              *-------------------------------------------------*
      *              * Test se c'e' stata almeno una visualizzazione   *
      *              *-------------------------------------------------*
           if        w-wrk-flg-uno        not  = spaces
                     go to  z-qry-fin-cic-050.
      *                  *---------------------------------------------*
      *                  * Messaggio di nessuna elaborazione           *
      *                  *---------------------------------------------*
           perform   z-qry-nes-ela-000    thru z-qry-nes-ela-999      .
      *                  *---------------------------------------------*
      *                  * Terminazione programma                      *
      *                  *---------------------------------------------*
           move      "#"                  to   OK                     .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     z-qry-fin-cic-999.
       z-qry-fin-cic-050.
      *              *-------------------------------------------------*
      *              * Test su linee residue                           *
      *              *-------------------------------------------------*
           if        v-res                >    1
                     go to  z-qry-fin-cic-100.
      *                  *---------------------------------------------*
      *                  * Intestazione pagina video                   *
      *                  *---------------------------------------------*
           perform   int-pag-vid-000      thru int-pag-vid-999        .
      *                      *-----------------------------------------*
      *                      * Test su interazione con utente          *
      *                      *-----------------------------------------*
           if        v-key                not  = spaces
                     move   "#"           to   OK
                     go to  z-qry-fin-cic-999.
      *                      *-----------------------------------------*
      *                      * Salta interlinea vuota                  *
      *                      *-----------------------------------------*
           go to     z-qry-fin-cic-120.
       z-qry-fin-cic-100.
      *                  *---------------------------------------------*
      *                  * Interlinea vuota                            *
      *                  *---------------------------------------------*
           move      "LF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       z-qry-fin-cic-120.
      *                  *---------------------------------------------*
      *                  * Visualizzazione saldo cliente               *
      *                  *---------------------------------------------*
      *                          *-------------------------------------*
      *                          * 'Saldo cliente :'                   *
      *                          *-------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      15                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      47                   to   v-pos                  .
           move      "Saldo cliente :"    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                          *-------------------------------------*
      *                          * Saldo cliente                       *
      *                          *-------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      13                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "G"                  to   v-edm                  .
           move      v-lnr                to   v-lin                  .
           move      63                   to   v-pos                  .
           move      w-wrk-sdo-cli        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       z-qry-fin-cic-999.
           exit.

      *    *===========================================================*
      *    * Esecuzione per inizio 5. livello di rottura               *
      *    *                                                           *
      *    * Uscita  : OK = spaces : continua l'esecuzione             *
      *    *                "#"    : terminazione programma            *
      *    *-----------------------------------------------------------*
       z-qry-ini-lr5-000.
       z-qry-ini-lr5-999.
           exit.

      *    *===========================================================*
      *    * Esecuzione per fine 5. livello di rottura                 *
      *    *                                                           *
      *    * Uscita  : OK = spaces : continua l'esecuzione             *
      *    *                "#"    : terminazione programma            *
      *    *-----------------------------------------------------------*
       z-qry-fin-lr5-000.
       z-qry-fin-lr5-999.
           exit.

      *    *===========================================================*
      *    * Esecuzione per inizio 4. livello di rottura               *
      *    *                                                           *
      *    * Uscita  : OK = spaces : continua l'esecuzione             *
      *    *                "#"    : terminazione programma            *
      *    *-----------------------------------------------------------*
       z-qry-ini-lr4-000.
       z-qry-ini-lr4-999.
           exit.

      *    *===========================================================*
      *    * Esecuzione per fine 4. livello di rottura                 *
      *    *                                                           *
      *    * Uscita  : OK = spaces : continua l'esecuzione             *
      *    *                "#"    : terminazione programma            *
      *    *-----------------------------------------------------------*
       z-qry-fin-lr4-000.
       z-qry-fin-lr4-999.
           exit.

      *    *===========================================================*
      *    * Esecuzione per inizio 3. livello di rottura               *
      *    *                                                           *
      *    * Uscita  : OK = spaces : continua l'esecuzione             *
      *    *                "#"    : terminazione programma            *
      *    *-----------------------------------------------------------*
       z-qry-ini-lr3-000.
       z-qry-ini-lr3-999.
           exit.

      *    *===========================================================*
      *    * Esecuzione per fine 3. livello di rottura                 *
      *    *                                                           *
      *    * Uscita  : OK = spaces : continua l'esecuzione             *
      *    *                "#"    : terminazione programma            *
      *    *-----------------------------------------------------------*
       z-qry-fin-lr3-000.
       z-qry-fin-lr3-999.
           exit.

      *    *===========================================================*
      *    * Esecuzione per inizio 2. livello di rottura               *
      *    *                                                           *
      *    * Uscita  : OK = spaces : continua l'esecuzione             *
      *    *                "#"    : terminazione programma            *
      *    *-----------------------------------------------------------*
       z-qry-ini-lr2-000.
       z-qry-ini-lr2-999.
           exit.

      *    *===========================================================*
      *    * Esecuzione per fine 2. livello di rottura                 *
      *    *                                                           *
      *    * Uscita  : OK = spaces : continua l'esecuzione             *
      *    *                "#"    : terminazione programma            *
      *    *-----------------------------------------------------------*
       z-qry-fin-lr2-000.
       z-qry-fin-lr2-999.
           exit.

      *    *===========================================================*
      *    * Esecuzione per inizio 1. livello di rottura               *
      *    *                                                           *
      *    * Uscita  : OK = spaces : continua l'esecuzione             *
      *    *                "#"    : terminazione programma            *
      *    *-----------------------------------------------------------*
       z-qry-ini-lr1-000.
      *              *-------------------------------------------------*
      *              * Lettura [mgt] corrispondente a [mgr]            *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmgt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgt                 .
           move      "RK"                 to   f-ope                  .
           move      "DATREG"             to   f-key                  .
           move      rf-mgr-dat-reg       to   rf-mgt-dat-reg         .
           move      rf-mgr-num-prt       to   rf-mgt-num-prt         .
           move      "pgm/cge/fls/ioc/obj/iofmgt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgt                 .
      *              *-------------------------------------------------*
      *              * Azzeramento saldo partita                       *
      *              *-------------------------------------------------*
           move      zero                 to   w-par-sdo-par          .
      *              *-------------------------------------------------*
      *              * Memorizzazione dati partita                     *
      *              *-------------------------------------------------*
           move      rf-mgr-dat-rif       to   w-par-dat-rif          .
           move      rf-mgr-num-rif       to   w-par-num-rif          .
           move      rf-mgt-des-cau       to   w-par-des-cau          .
           move      rf-mgr-com-rig       to   w-par-rig-cau (5)      .
       z-qry-ini-lr1-999.
           exit.

      *    *===========================================================*
      *    * Esecuzione per fine 1. livello di rottura                 *
      *    *                                                           *
      *    * Uscita  : OK = spaces : continua l'esecuzione             *
      *    *                "#"    : terminazione programma            *
      *    *-----------------------------------------------------------*
       z-qry-fin-lr1-000.
      *              *-------------------------------------------------*
      *              * Test su flag di almeno una visualizzazione      *
      *              *-------------------------------------------------*
           if        w-wrk-flg-uno        not  = spaces
                     go to  z-qry-fin-lr1-100.
           if        rr-tip-vis           =    "A" and
                     w-par-sdo-par        =    zero
                     go to  z-qry-fin-lr1-999.
      *                  *---------------------------------------------*
      *                  * Flag di almeno una visualizzazione attivo   *
      *                  *---------------------------------------------*
           move      "#"                  to   w-wrk-flg-uno          .
      *                  *---------------------------------------------*
      *                  * Intestazione pagina video                   *
      *                  *---------------------------------------------*
           perform   int-pag-vid-000      thru int-pag-vid-999        .
      *                      *-----------------------------------------*
      *                      * Test su interazione con utente          *
      *                      *-----------------------------------------*
           if        v-key                not  = spaces
                     move   "#"           to   OK
                     go to  z-qry-fin-lr1-999.
       z-qry-fin-lr1-100.
      *              *-------------------------------------------------*
      *              * Test su tipo visualizzazione                    *
      *              *-------------------------------------------------*
           if        rr-tip-vis           =    "A" and
                     w-par-sdo-par        =    zero
                     go to  z-qry-fin-lr1-999.
      *              *-------------------------------------------------*
      *              * Aggiornamento saldo cliente                     *
      *              *-------------------------------------------------*
           add       w-par-sdo-par        to   w-wrk-sdo-cli          .
      *              *-------------------------------------------------*
      *              * Test su linee residue                           *
      *              *-------------------------------------------------*
           if        v-res                >    zero
                     go to  z-qry-fin-lr1-120.
      *                  *---------------------------------------------*
      *                  * Intestazione pagina video                   *
      *                  *---------------------------------------------*
           perform   int-pag-vid-000      thru int-pag-vid-999        .
      *                      *-----------------------------------------*
      *                      * Test su interazione con utente          *
      *                      *-----------------------------------------*
           if        v-key                not  = spaces
                     move   "#"           to   OK
                     go to  z-qry-fin-lr1-999.
       z-qry-fin-lr1-120.
      *              *-------------------------------------------------*
      *              * Dettaglio riga partita                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Data partita                                *
      *                  *---------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      v-lnr                to   v-lin                  .
           move      01                   to   v-pos                  .
           move      w-par-dat-rif        to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Numero partita                              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Allineamento a destra                   *
      *                      *-----------------------------------------*
           move      w-par-num-rif        to   w-all-str-alf          .
           move      10                   to   w-all-str-lun          .
           perform   all-str-adx-000      thru all-str-adx-999        .
      *                      *-----------------------------------------*
      *                      * Visualizzazione                         *
      *                      *-----------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      10                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      10                   to   v-pos                  .
           move      w-all-str-alf        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Prima riga descrizione causale          *
      *                      *-----------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      22                   to   v-pos                  .
           move      w-par-rig-cau (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                          *-------------------------------------*
      *                          * Definizione Mark-Point              *
      *                          *-------------------------------------*
           move      "+"                  to   v-edm                  .
           move      w-par-dat-rif        to   w-mpn-dat-rif          .
           move      w-par-num-rif        to   w-mpn-num-rif          .
           move      w-par-sdo-par        to   w-mpn-sdo-par          .
           move      w-mpn                to   v-cnt                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Saldo partita                           *
      *                      *-----------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      13                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "G"                  to   v-edm                  .
           move      v-lnr                to   v-lin                  .
           move      63                   to   v-pos                  .
           move      w-par-sdo-par        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                          *-------------------------------------*
      *                          * Interlinea                          *
      *                          *-------------------------------------*
           move      "LF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Completamento descrizione causale       *
      *                      *-----------------------------------------*
           move      1                    to   I                      .
       z-qry-fin-lr1-200.
           add       1                    to   I                      .
           if        I                    >    5
                     go to  z-qry-fin-lr1-999.
           if        w-par-rig-cau (I)    =    spaces
                     go to  z-qry-fin-lr1-200.
      *                          *-------------------------------------*
      *                          * Test su linee residue               *
      *                          *-------------------------------------*
           if        v-res                >    zero
                     go to  z-qry-fin-lr1-220.
      *                              *---------------------------------*
      *                              * Intestazione pagina video       *
      *                              *---------------------------------*
           perform   int-pag-vid-000      thru int-pag-vid-999        .
      *                              *---------------------------------*
      *                              * Test su interazione con utente  *
      *                              *---------------------------------*
           if        v-key                not  = spaces
                     move   "#"           to   OK
                     go to  z-qry-fin-lr1-999.
       z-qry-fin-lr1-220.
      *                          *-------------------------------------*
      *                          * Visualizzazione riga causale i-esima*
      *                          *-------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      22                   to   v-pos                  .
           move      w-par-rig-cau (I)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                              *---------------------------------*
      *                              * Interlinea                      *
      *                              *---------------------------------*
           move      "LF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                          *-------------------------------------*
      *                          * Riciclo a prossima riga             *
      *                          *-------------------------------------*
           go to     z-qry-fin-lr1-200.
       z-qry-fin-lr1-999.
           exit.

      *    *===========================================================*
      *    * Esecuzione per il livello di dettaglio                    *
      *    *                                                           *
      *    * Uscita  : OK = spaces : continua l'esecuzione             *
      *    *                "#"    : terminazione programma            *
      *    *-----------------------------------------------------------*
       z-qry-liv-det-000.
      *              *-------------------------------------------------*
      *              * Aggiornamento saldo partita                     *
      *              *-------------------------------------------------*
           if        rf-mgr-dar-ave       =    "A"
                     subtract rf-mgr-imp-mov
                                          from w-par-sdo-par
           else      add      rf-mgr-imp-mov
                                          to   w-par-sdo-par          .
       z-qry-liv-det-999.
           exit.

      *    *===========================================================*
      *    * Intestazione pagina video                                 *
      *    *-----------------------------------------------------------*
       int-pag-vid-000.
      *              *-------------------------------------------------*
      *              * Page advance                                    *
      *              *-------------------------------------------------*
           perform   z-qry-pag-adv-000    thru z-qry-pag-adv-999      .
      *                  *---------------------------------------------*
      *                  * Test su interazione con utente              *
      *                  *---------------------------------------------*
           if        v-key                not  = spaces
                     go to int-pag-vid-999.
      *              *-------------------------------------------------*
      *              * Visualizzazione titolo                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Trattini a linea 01                         *
      *                  *---------------------------------------------*
           move      "WR"                 to   v-ope                  .
           move      all    "="           to   v-cnt                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Titolo centrale 'PARTITE CLIENTE'           *
      *                  *---------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      15                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      33                   to   v-pos                  .
           move      "PARTITE CLIENTE"    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Interlinea                              *
      *                      *-----------------------------------------*
           move      "LF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Trattini a linea 03                         *
      *                  *---------------------------------------------*
           move      "WR"                 to   v-ope                  .
           move      all    "="           to   v-cnt                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Sottotitolo                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Editing codice cliente                      *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<"                  to   v-edm                  .
           move      rr-cod-cli           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Composizione stringa                    *
      *                      *-----------------------------------------*
           move      spaces               to   v-alf                  .
           string    "Cliente : "
                                delimited by   size
                     v-edt
                                delimited by   spaces
                     " "
                                delimited by   size
                     rr-cod-cli-rag
                                delimited by   size
                                          into v-alf                  .
      *                      *-----------------------------------------*
      *                      * Visualizzazione                         *
      *                      *-----------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      01                   to   v-pos                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                          *-------------------------------------*
      *                          * Interlinea                          *
      *                          *-------------------------------------*
           move      "LF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Lineette a linea 05                     *
      *                      *-----------------------------------------*
           move      "WR"                 to   v-ope                  .
           move      all    "-"           to   v-cnt                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Interlinea vuota                        *
      *                      *-----------------------------------------*
           move      "LF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Linea di fincatura                      *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * 'Data e Num. Partita'               *
      *                          *-------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      19                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Data e num. partita"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                          *-------------------------------------*
      *                          * 'Descrizione operazione'            *
      *                          *-------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      22                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      22                   to   v-pos                  .
           move      "Descrizione operazione"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                          *-------------------------------------*
      *                          * 'Saldo Partita'                     *
      *                          *-------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      13                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      68                   to   v-pos                  .
           move      "Saldo Partita"      to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                              *---------------------------------*
      *                              * Interlinea                      *
      *                              *---------------------------------*
           move      "LF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                          *-------------------------------------*
      *                          * Interlinea vuota                    *
      *                          *-------------------------------------*
           move      "LF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       int-pag-vid-999.
           exit.

      *    *===========================================================*
      *    * Determinazione function-keys previste in Mark-points      *
      *    *-----------------------------------------------------------*
       z-qry-det-fky-000.
      *              *-------------------------------------------------*
      *              * Determinazione eventuale function-key 'SLCT'    *
      *              *-------------------------------------------------*
           move      "GV"                 to   s-ope                  .
           move      "fkselect"           to   s-var                  .
           move      "-"                  to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                not  = spaces or
                     s-alf                not  = "SLCT"
                     move   spaces        to   v-pfk(01)
           else      move   s-alf         to   v-pfk(01)              .
      *              *-------------------------------------------------*
      *              * Function-key 'INSR'                             *
      *              *-------------------------------------------------*
           move      "INSR"               to   v-pfk(02)              .
      *              *-------------------------------------------------*
      *              * Function-key 'EXPD'                             *
      *              *-------------------------------------------------*
           move      "EXPD"               to   v-pfk(03)              .
      *              *-------------------------------------------------*
      *              * Altre function-key a spaces                     *
      *              *-------------------------------------------------*
           move      spaces               to   v-pfk(04)              .
           move      spaces               to   v-pfk(05)              .
           move      spaces               to   v-pfk(06)              .
           move      spaces               to   v-pfk(07)              .
           move      spaces               to   v-pfk(08)              .
           move      spaces               to   v-pfk(09)              .
           move      spaces               to   v-pfk(10)              .
       z-qry-det-fky-999.
           exit.

      *    *===========================================================*
      *    * Trattamento tasto di funzione selezionato da utente       *
      *    *                                                           *
      *    *  A disposizione :                                         *
      *    *                                                           *
      *    *    v-key : function-key selezionata                       *
      *    *    v-cnt : parametri associati al mark-point              *
      *    *    v-lin : mark-point line     01-21                      *
      *    *    v-pos : mark-point position 01-80                      *
      *    *                                                           *
      *    *  Uscita  : OK = spaces : continua l'esecuzione            *
      *    *                 "#"    : terminazione programma           *
      *    *-----------------------------------------------------------*
       qry-trt-fun-000.
      *              *-------------------------------------------------*
      *              * Function-key 'SLCT'                             *
      *              *-------------------------------------------------*
           if        v-key                not  = "SLCT"
                     go to  qry-trt-fun-100.
      *                  *---------------------------------------------*
      *                  * Movimento di v-cnt in area mark-point       *
      *                  *---------------------------------------------*
           move      v-cnt                to   w-mpn                  .
      *                  *---------------------------------------------*
      *                  * Estrazione di eventuale variabile di i.p.c. *
      *                  * relativa alla possibilita' di richiedere    *
      *                  * l'importo movimento                         *
      *                  *---------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "snx-imp"            to   s-var                  .
           move      "-"                  to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                not  = spaces or
                     s-alf                not  = "S"
                     move   zero          to   w-mpn-sdo-par
                     go to  qry-trt-fun-020.
      *                  *---------------------------------------------*
      *                  * Determinazione segno partita                *
      *                  *---------------------------------------------*
           if        w-mpn-sdo-par        <    zero
                     move   "-"           to   w-wrk-sgn-par
           else      move   "+"           to   w-wrk-sgn-par          .
      *                  *---------------------------------------------*
      *                  * Richiesta importo da attribuire alla partita*
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Prompt                                  *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Editing data riferimento            *
      *                          *-------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      w-mpn-dat-rif        to   v-dat
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                          *-------------------------------------*
      *                          * Composizione stringa                *
      *                          *-------------------------------------*
           move      spaces               to   v-alf                  .
           move      1                    to   I                      .
           string    "Partita nr. "
                                delimited by   size
                     w-mpn-num-rif
                                delimited by   spaces
                     " del "
                                delimited by   size
                     v-edt
                                delimited by   spaces
                     ";  importo da attribuire : "
                                delimited by   size
                                          into v-alf
                                  with pointer I                      .
      *                          *-------------------------------------*
      *                          * Visualizzazione                     *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      24                   to   v-lin                  .
           move      01                   to   v-pos                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Importo                                 *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Determinazione position             *
      *                          *-------------------------------------*
           move      I                    to   v-pos                  .
      *                          *-------------------------------------*
      *                          * Accettazione                        *
      *                          *-------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      13                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<G"                 to   v-edm                  .
           move      24                   to   v-lin                  .
           move      "EXIT"               to   v-pfk (1)              .
           move      "DO  "               to   v-pfk (2)              .
           move      "UP  "               to   v-pfk (3)              .
           move      w-mpn-sdo-par        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                          *-------------------------------------*
      *                          * Se function-key 'EXIT' si esce      *
      *                          *-------------------------------------*
           if        v-key                =    "EXIT"
                     go to  qry-trt-fun-040.
      *                          *-------------------------------------*
      *                          * Se function-key 'UP  '              *
      *                          *-------------------------------------*
           if        v-key                =    "UP  "
                     go to  qry-trt-fun-999.
      *                  *---------------------------------------------*
      *                  * Movimento valore impostato in work          *
      *                  *---------------------------------------------*
           move      v-num                to   w-mpn-sdo-par          .
      *                  *---------------------------------------------*
      *                  * Determinazione segno valore impostato       *
      *                  *---------------------------------------------*
           if        w-wrk-sgn-par        =    "-"
                     multiply -1          by   w-mpn-sdo-par          .
       qry-trt-fun-020.
      *                  *---------------------------------------------*
      *                  * Put variabile di ipc:'select datrif'        *
      *                  *---------------------------------------------*
           move      "PV"                 to   s-ope                  .
           move      "select datrif"      to   s-var                  .
           move      "-"                  to   s-dop                  .
           move      "D"                  to   s-tip                  .
           move      w-mpn-dat-rif        to   s-dat                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Put variabile di ipc:'select numrif'        *
      *                  *---------------------------------------------*
           move      "PV"                 to   s-ope                  .
           move      "select numrif"      to   s-var                  .
           move      "-"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      10                   to   s-car                  .
           move      w-mpn-num-rif        to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Put variabile di ipc:'select impmov'        *
      *                  *---------------------------------------------*
           move      "PV"                 to   s-ope                  .
           move      "select impmov"      to   s-var                  .
           move      "-"                  to   s-dop                  .
           move      "V"                  to   s-tip                  .
           move      13                   to   s-car                  .
           move      c-dec                to   s-dec                  .
           move      "S"                  to   s-sgn                  .
           move      w-mpn-sdo-par        to   s-num                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
       qry-trt-fun-040.
      *                  *---------------------------------------------*
      *                  * Segnale di interruzione interrogazione      *
      *                  *---------------------------------------------*
           move      "#"                  to   OK                     .
      *                  *---------------------------------------------*
      *                  * Flag di richieste a No                      *
      *                  *---------------------------------------------*
           move      "N"                  to   w-fun-ric              .
      *                  *---------------------------------------------*
      *                  * Flag di funzionamento ciclico a No          *
      *                  *---------------------------------------------*
           move      "N"                  to   w-fun-cic              .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     qry-trt-fun-999.
       qry-trt-fun-100.
      *              *-------------------------------------------------*
      *              * Function-key 'INSR'                             *
      *              *-------------------------------------------------*
           if        v-key                not  = "INSR"
                     go to  qry-trt-fun-200.
      *                  *---------------------------------------------*
      *                  * Test se programma di pareggiamento partite  *
      *                  * clienti gia' attivo                         *
      *                  *---------------------------------------------*
           move      "P?"                 to   s-ope                  .
           move      "pcge4400"           to   s-pro                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-liv                not  = zero
                     go to  qry-trt-fun-999.
      *                  *---------------------------------------------*
      *                  * Preparazione variabile di i.p.c. relativa al*
      *                  * codice cliente                              *
      *                  *---------------------------------------------*
           move      "PV"                 to   s-ope                  .
           move      "cod-cli"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "N"                  to   s-tip                  .
           move      07                   to   s-car                  .
           move      zero                 to   s-dec                  .
           move      spaces               to   s-sgn                  .
           move      rr-cod-cli           to   s-num                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Richiamo programma di pareggiamento partite *
      *                  * clienti                                     *
      *                  *---------------------------------------------*
           move      "pgm/cge/prg/obj/pcge4400"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat                                            .
           cancel    s-pat                                            .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     qry-trt-fun-999.
       qry-trt-fun-200.
      *              *-------------------------------------------------*
      *              * Function-key 'EXPD'                             *
      *              *-------------------------------------------------*
           if        v-key                not  = "EXPD"
                     go to  qry-trt-fun-999.
      *                  *---------------------------------------------*
      *                  * Test se programma di interrogazione su sin- *
      *                  * gola partita gia' attivo                    *
      *                  *---------------------------------------------*
           move      "P?"                 to   s-ope                  .
           move      "pcge4430"           to   s-pro                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-liv                not  = zero
                     go to  qry-trt-fun-999.
      *                  *---------------------------------------------*
      *                  * Movimento di v-cnt in area mark-point       *
      *                  *---------------------------------------------*
           move      v-cnt                to   w-mpn                  .
      *                  *---------------------------------------------*
      *                  * Put variabile di ipc:'select codcli'        *
      *                  *---------------------------------------------*
           move      "PV"                 to   s-ope                  .
           move      "select codcli"      to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "N"                  to   s-tip                  .
           move      07                   to   s-car                  .
           move      zero                 to   s-dec                  .
           move      spaces               to   s-sgn                  .
           move      rr-cod-cli           to   s-num                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Put variabile di ipc:'select datrif'        *
      *                  *---------------------------------------------*
           move      "PV"                 to   s-ope                  .
           move      "select datrif"      to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "D"                  to   s-tip                  .
           move      w-mpn-dat-rif        to   s-dat                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Put variabile di ipc:'select numrif'        *
      *                  *---------------------------------------------*
           move      "PV"                 to   s-ope                  .
           move      "select numrif"      to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      10                   to   s-car                  .
           move      w-mpn-num-rif        to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Richiamo programma di interrogazione su sin-*
      *                  * gola partita                                *
      *                  *---------------------------------------------*
           move      "pgm/cge/prg/obj/pcge4430"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat                                            .
           cancel    s-pat                                            .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     qry-trt-fun-999.
       qry-trt-fun-999.
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
      *    * Subroutines per l'accettazione del codice cliente         *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/acmncli0.acs"                   .
