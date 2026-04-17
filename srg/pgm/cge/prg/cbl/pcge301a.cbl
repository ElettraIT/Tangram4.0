       Identification Division.
       Program-Id.                                 pcge301a           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    cge                 *
      *                                Settore:    mov                 *
      *                                   Fase:    cge301              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 29/12/89    *
      *                       Ultima revisione:    NdK del 12/09/18    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Esecuzione richieste per interrogazione     *
      *                    programma cge3010:                          *
      *                                                                *
      *                    Saldo alla data sottoconto                  *
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
                     "mov"                                            .
      *        *-------------------------------------------------------*
      *        * Fase gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-fas                  pic  x(06) value
                     "cge3ra"                                         .
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10) value
                     "pcge301a"                                       .
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
      *    * Record file                                               *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [mgr]                                                 *
      *        *-------------------------------------------------------*
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
      *    * Record logico del file piano dei conti                    *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfpdc"                          .

      *    *===========================================================*
      *    * Record logico file [mgs]                                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfmgs"                          .

      *    *===========================================================*
      *    * Record numerazione chiusura esercizio [datbil]            *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/num/rec/rndatbil"                       .

      *    *===========================================================*
      *    * Work-area per ridefinizione contenuto del mark-point      *
      *    *-----------------------------------------------------------*
       01  w-mpn.
           05  filler                     pic  x(01)                  .

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
      *    * Work area per determinazione saldo ad una data            *
      *    *-----------------------------------------------------------*
       01  w-sad.
      *        *-------------------------------------------------------*
      *        * Codice sottoconto di cui si vuole ottenere il saldo   *
      *        *-------------------------------------------------------*
           05  w-sad-cod-pdc              pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Tipo sottoconto di cui si vuole ottenere il saldo     *
      *        *-------------------------------------------------------*
           05  w-sad-tip-cnt              pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Data di cui si vuole ottenere il saldo                *
      *        *-------------------------------------------------------*
           05  w-sad-dat-sad              pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Anno d'esercizio in cui e' stato richiesto il saldo   *
      *        *-------------------------------------------------------*
           05  w-sad-ann-ese              pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Mese d'esercizio in cui e' stato richiesto il saldo   *
      *        *-------------------------------------------------------*
           05  w-sad-mes-ese              pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Anno d'esercizio precedente quello in cui e' stato    *
      *        * richiesto il saldo                                    *
      *        *-------------------------------------------------------*
           05  w-sad-ann-pre              pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Data odierna                                          *
      *        *-------------------------------------------------------*
           05  w-sad-dat-att              pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Saldo alla data richiesta                             *
      *        *-------------------------------------------------------*
           05  w-sad-sdo-dat              pic s9(13)                  .
      *        *-------------------------------------------------------*
      *        * Saldo a inizio esercizio della data richiesta         *
      *        *-------------------------------------------------------*
           05  w-sad-sdo-ini              pic s9(13)                  .
      *        *-------------------------------------------------------*
      *        * Progressivo dare da inizio anno alla data richiesta   *
      *        *-------------------------------------------------------*
           05  w-sad-prg-dar              pic s9(13)                  .
      *        *-------------------------------------------------------*
      *        * Progressivo avere da inizio anno alla data richiesta  *
      *        *-------------------------------------------------------*
           05  w-sad-prg-ave              pic s9(13)                  .
      *        *-------------------------------------------------------*
      *        * Si/No saldo alla data di fine esercizio               *
      *        *-------------------------------------------------------*
           05  w-sad-snx-dfe              pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Progressivo dare rettifiche di bilancio               *
      *        *-------------------------------------------------------*
           05  w-sad-dar-bil              pic s9(13)                  .
      *        *-------------------------------------------------------*
      *        * Progressivo avere rettifiche di bilancio              *
      *        *-------------------------------------------------------*
           05  w-sad-ave-bil              pic s9(13)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per valore anno                                *
      *        *-------------------------------------------------------*
           05  w-sad-wrk-ann              pic  9(03)                  .

      *    *===========================================================*
      *    * Work per subroutines di editing codice sottoconto         *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/wedtpdc0.wkl"                   .

      *    *===========================================================*
      *    * Work area locale                                          *
      *    *-----------------------------------------------------------*
       01  w-wrk.
      *        *-------------------------------------------------------*
      *        * Flag di primo passaggio                               *
      *        *-------------------------------------------------------*
           05  w-wrk-flg-uno              pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Contatore di comodo 'I'                               *
      *        *-------------------------------------------------------*
           05  I                          pic  9(03)                  .

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Link-area richieste per interrogazione                    *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/pcge3010.pgl"                   .

      ******************************************************************
       Procedure Division                using rr                     .
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
                     go to  main-999.
       main-200.
      *              *-------------------------------------------------*
      *              * Esecuzione interrogazione                       *
      *              *-------------------------------------------------*
           move      spaces               to   OK                     .
           perform   z-qry-rou-pri-000    thru z-qry-rou-pri-999      .
       main-950.
      *              *-------------------------------------------------*
      *              * Dichiarazione di fine programma                 *
      *              *-------------------------------------------------*
           perform   dic-fin-pgm-000      thru dic-fin-pgm-999        .
       main-999.
           exit      program                                          .

      *================================================================*
      *       Routines fisse                                           *
      *================================================================*

      *================================================================*
      *       Routine di interrogazione principale                     *
      *----------------------------------------------------------------*
       z-qry-rou-pri-000.
      *              *-------------------------------------------------*
      *              * Determinazione function-keys in Mark-points     *
      *              *-------------------------------------------------*
           perform   z-qry-det-fky-000    thru z-qry-det-fky-999      .
      *              *-------------------------------------------------*
      *              * Begin                                           *
      *              *-------------------------------------------------*
           move      "BE"                 to   v-ope                  .
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
           perform   z-qry-str-ini-000    thru z-qry-str-ini-999      .
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
           perform   z-qry-let-seq-000    thru z-qry-let-seq-999      .
           if        v-mrk                not  = spaces
                     move   spaces        to   v-mrk
                     go to  z-qry-rou-pri-500.
      *              *-------------------------------------------------*
      *              * Test se superamento limiti massimi              *
      *              *-------------------------------------------------*
           perform   z-qry-tst-max-000    thru z-qry-tst-max-999      .
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
           perform   z-qry-cmp-rot-000    thru z-qry-cmp-rot-999      .
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
           perform   z-qry-ini-cic-000    thru z-qry-ini-cic-999      .
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
           perform   z-qry-trt-fun-000    thru z-qry-trt-fun-999      .
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
      *              * Lettura tipo sottoconto                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Open file [pdc]                             *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofpdc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdc                 .
      *                  *---------------------------------------------*
      *                  * Preparazione chiave per lettura             *
      *                  *---------------------------------------------*
           move      rr-cod-arc           to   rf-pdc-cod-pdc         .
      *                  *---------------------------------------------*
      *                  * Lettura record file [pdc]                   *
      *                  *---------------------------------------------*
           move      "RD"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofpdc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdc                 .
      *                      *-----------------------------------------*
      *                      * Test su esito operazione                *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     move   "#"           to   OK
           else      move   rf-pdc-tip-cnt
                                          to   w-sad-tip-cnt          .
      *                  *---------------------------------------------*
      *                  * Close file [pdc]                            *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofpdc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdc                 .
           if        OK                   not  = spaces
                     go to  pre-exe-pgm-999.
      *              *-------------------------------------------------*
      *              * Lettura [num] dati di bilancio                  *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/num/ioc/obj/indatbil"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-dat-bil             .
           move      "NO"                 to   f-ope                  .
           move      "pgm/cge/num/ioc/obj/indatbil"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-dat-bil             .
           move      "RD"                 to   f-ope                  .
           move      "pgm/cge/num/ioc/obj/indatbil"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-dat-bil             .
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/num/ioc/obj/indatbil"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-dat-bil             .
      *              *-------------------------------------------------*
      *              * Lettura scelta mese di chiusura bilancio        *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/cge[mes-chi]"
                                          to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-num          to   w-ese-cge-mce
           else      move  12             to   w-ese-cge-mce          .
           if        w-ese-cge-mce        <    01 or
                     w-ese-cge-mce        >    12
                     move  12             to   w-ese-cge-mce          .
      *              *-------------------------------------------------*
      *              * Determinazione data attuale                     *
      *              *-------------------------------------------------*
           move      "DT"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-dat                to   w-sad-dat-att          .
      *              *-------------------------------------------------*
      *              * Determinazione mese e anno di esercizio relati- *
      *              * vi alla data del giorno di cui e' stato richie- *
      *              * sto il saldo                                    *
      *              *-------------------------------------------------*
           move      rr-dat-ini           to   w-sad-dat-sad          .
           move      w-sad-dat-sad        to   w-ese-cge-dtr          .
           perform   det-ese-cge-000      thru det-ese-cge-999        .
           move      w-ese-cge-esa        to   w-sad-ann-ese          .
           move      w-ese-cge-esm        to   w-sad-mes-ese          .
      *              *-------------------------------------------------*
      *              * Determinazione anno precedente quello della da- *
      *              * ta richiesta                                    *
      *              *-------------------------------------------------*
           move      zero                 to   s-dat                  .
           move      w-sad-ann-ese        to   s-saa                  .
           subtract  1                    from s-saa                  .
           move      "NS"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-saa                to   w-sad-ann-pre          .
       pre-exe-pgm-999.
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
      *    * Open files : se errori v-mrk:= "#"                        *
      *    *-----------------------------------------------------------*
       z-qry-opn-fls-000.
      *              *-------------------------------------------------*
      *              * [mgr]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmgr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgr                 .
      *              *-------------------------------------------------*
      *              * Apertura file [mgs]                             *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmgs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgs                 .
       z-qry-opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Close files : se errori v-mrk:= "#"                       *
      *    *-----------------------------------------------------------*
       z-qry-cls-fls-000.
      *              *-------------------------------------------------*
      *              * Chiusura file [mgr]                             *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmgr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgr                 .
      *              *-------------------------------------------------*
      *              * Chiusura file [mgs]                             *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmgs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgs                 .
       z-qry-cls-fls-999.
           exit.

      *    *===========================================================*
      *    * Start iniziale - se non valida v-mrk:= "#"                *
      *    *-----------------------------------------------------------*
       z-qry-str-ini-000.
      *              *-------------------------------------------------*
      *              * Inizializzazione flag di primo passaggio        *
      *              *-------------------------------------------------*
           move      spaces               to   w-wrk-flg-uno          .
       z-qry-str-ini-999.
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
       z-qry-let-seq-000.
      *              *-------------------------------------------------*
      *              * Test su flag di primo passaggio                 *
      *              *-------------------------------------------------*
           if        w-wrk-flg-uno        =    spaces
                     move   "#"           to   w-wrk-flg-uno
                     go to  z-qry-let-seq-999.
           move      "#"                  to   v-mrk                  .
       z-qry-let-seq-999.
           exit.

      *    *===========================================================*
      *    * Test superamento limiti massimi - se si' v-mrk:= "#"      *
      *    *-----------------------------------------------------------*
       z-qry-tst-max-000.
       z-qry-tst-max-999.
           exit.

      *    *===========================================================*
      *    * Selezione su record letto - se da ignorare v-mrk:= "#"    *
      *    *-----------------------------------------------------------*
       z-qry-sel-rec-000.
      *              *-------------------------------------------------*
      *              * Determinazione saldo alla data richiesta        *
      *              *-------------------------------------------------*
           move      rr-cod-arc           to   w-sad-cod-pdc          .
           perform   det-sdo-dat-000      thru det-sdo-dat-999        .
       z-qry-sel-rec-999.
           exit.

      *    *===========================================================*
      *    * Composizione area di rottura w-rot                        *
      *    *-----------------------------------------------------------*
       z-qry-cmp-rot-000.
       z-qry-cmp-rot-999.
           exit.

      *    *===========================================================*
      *    * Esecuzione per inizio ciclo                               *
      *    *                                                           *
      *    * Uscita  : OK = spaces : continua l'esecuzione             *
      *    *                "#"    : terminazione programma            *
      *    *-----------------------------------------------------------*
       z-qry-ini-cic-000.
       z-qry-ini-cic-999.
           exit.

      *    *===========================================================*
      *    * Esecuzione per fine ciclo                                 *
      *    *                                                           *
      *    * Uscita  : OK = spaces : continua l'esecuzione             *
      *    *                "#"    : terminazione programma            *
      *    *-----------------------------------------------------------*
       z-qry-fin-cic-000.
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
       z-qry-ini-lr1-999.
           exit.

      *    *===========================================================*
      *    * Esecuzione per fine 1. livello di rottura                 *
      *    *                                                           *
      *    * Uscita  : OK = spaces : continua l'esecuzione             *
      *    *                "#"    : terminazione programma            *
      *    *-----------------------------------------------------------*
       z-qry-fin-lr1-000.
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
      *              * Intestazione pagina                             *
      *              *-------------------------------------------------*
           perform   int-pag-vid-000      thru int-pag-vid-999        .
      *                  *---------------------------------------------*
      *                  * Test su interazione con utente              *
      *                  *---------------------------------------------*
           if        v-key                not  = spaces
                     move   "#"           to   OK
                     go to  z-qry-liv-det-999.
      *              *-------------------------------------------------*
      *              * Literal 'Dare' e 'Avere'                        *
      *              *-------------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      24                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      57                   to   v-pos                  .
           move      "Dare               Avere"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Interlinee                              *
      *                      *-----------------------------------------*
           move      "LF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "LF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "LF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       z-qry-liv-det-100.
      *              *-------------------------------------------------*
      *              * Saldo a inizio anno                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Literal                                     *
      *                  *---------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Saldo a inizio anno                    :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Valore                                      *
      *                  *---------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      13                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "G"                  to   v-edm                  .
           move      v-lnr                to   v-lin                  .
           if        w-sad-sdo-ini        <    zero
                     move   64            to   v-pos
           else      move   44            to   v-pos                  .
           move      w-sad-sdo-ini        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Interlinee                              *
      *                      *-----------------------------------------*
           move      "LF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "LF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       z-qry-liv-det-200.
      *                  *---------------------------------------------*
      *                  * Literal 'Progressivi movimenti fino al'     *
      *                  *---------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      29                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Progressivi movimenti fino al"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Data di cui e' stato richiesto il saldo     *
      *                  *---------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      v-lnr                to   v-lin                  .
           move      31                   to   v-pos                  .
           move      rr-dat-ini           to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * ':'                                         *
      *                  *---------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      40                   to   v-pos                  .
           move      ":"                  to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Progressivo dare movimenti fino alla data   *
      *                  *---------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      13                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "G"                  to   v-edm                  .
           move      v-lnr                to   v-lin                  .
           move      43                   to   v-pos                  .
           move      w-sad-prg-dar        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Progressivo avere movimenti fino alla data  *
      *                  *---------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      13                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "G"                  to   v-edm                  .
           move      v-lnr                to   v-lin                  .
           move      63                   to   v-pos                  .
           move      w-sad-prg-ave        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Interlinea                              *
      *                      *-----------------------------------------*
           move      "LF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       z-qry-liv-det-300.
      *                  *---------------------------------------------*
      *                  * Progressivi rettifiche di bilancio          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se da stampare                     *
      *                      *-----------------------------------------*
           if        w-sad-snx-dfe        not  = "S"
                     go to z-qry-liv-det-400.
      *                      *-----------------------------------------*
      *                      * Interlinea vuota                        *
      *                      *-----------------------------------------*
           move      "LF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Prompt                                  *
      *                      *-----------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Progressivi rettifiche di bilancio     :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Progressivo dare rettifiche di bilancio *
      *                      *-----------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      13                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "G"                  to   v-edm                  .
           move      v-lnr                to   v-lin                  .
           move      43                   to   v-pos                  .
           move      w-sad-dar-bil        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Progressivo avere rettifiche di bilancio*
      *                      *-----------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      13                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "G"                  to   v-edm                  .
           move      v-lnr                to   v-lin                  .
           move      63                   to   v-pos                  .
           move      w-sad-ave-bil        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Interlinea                              *
      *                      *-----------------------------------------*
           move      "LF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       z-qry-liv-det-400.
      *                  *---------------------------------------------*
      *                  * Trattini per totale                         *
      *                  *---------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      37                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      44                   to   v-pos                  .
           move      "-------------------------------------"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Interlinea                              *
      *                      *-----------------------------------------*
           move      "LF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Literal 'Saldo al'                          *
      *                  *---------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Saldo al"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Data di cui e' stato richiesto il saldo     *
      *                  *---------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      v-lnr                to   v-lin                  .
           move      10                   to   v-pos                  .
           move      rr-dat-ini           to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * ':'                                         *
      *                  *---------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      40                   to   v-pos                  .
           move      ":"                  to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Saldo alla data richiesta                   *
      *                  *---------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      13                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "G"                  to   v-edm                  .
           move      v-lnr                to   v-lin                  .
           if        w-sad-sdo-dat        <    zero
                     move   64            to   v-pos
           else      move   44            to   v-pos                  .
           move      w-sad-sdo-dat        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
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
      *              * Trattini a linea 01                             *
      *              *-------------------------------------------------*
           move      "WR"                 to   v-ope                  .
           move      all    "="           to   v-cnt                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Titolo : 'SALDO ALLA DATA SOTTOCONTO'           *
      *              *-------------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      26                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      28                   to   v-pos                  .
           move      "SALDO ALLA DATA SOTTOCONTO"
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
      *              * Trattini a linea 03                             *
      *              *-------------------------------------------------*
           move      "WR"                 to   v-ope                  .
           move      all    "="           to   v-cnt                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Codice e descrizione sottoconto                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * 'Sottoconto:'                               *
      *                  *---------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      12                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Sottoconto :"        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Codice sottoconto                           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Editing con appoggio a sinistra         *
      *                      *-----------------------------------------*
           if        rr-cod-arc           >    99999
                     move   3             to   w-edt-cod-pdc-liv
           else      move   2             to   w-edt-cod-pdc-liv      .
           move      rr-cod-arc           to   w-edt-cod-pdc-cod      .
           move      spaces               to   w-edt-cod-pdc-edm      .
           perform   edt-pdc-asx-000      thru edt-pdc-asx-999        .
      *                      *-----------------------------------------*
      *                      * Visualizzazione                         *
      *                      *-----------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      09                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      14                   to   v-pos                  .
           move      w-edt-cod-pdc-edt    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Descrizione sottoconto                      *
      *                  *---------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           if        rr-cod-arc           >    99999
                     move   24            to   v-pos
           else      move   21            to   v-pos                  .
           move      rr-des-arc           to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Interlinea                              *
      *                      *-----------------------------------------*
           move      "LF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Lineette a linea 05                             *
      *              *-------------------------------------------------*
           move      "WR"                 to   v-ope                  .
           move      all    "-"           to   v-cnt                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Interlinea                              *
      *                      *-----------------------------------------*
           move      "LF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       int-pag-vid-999.
           exit.

      *    *===========================================================*
      *    * Determinazione function-keys previste in Mark-points      *
      *    *-----------------------------------------------------------*
       z-qry-det-fky-000.
           move      spaces               to   v-pfk(01)              .
           move      spaces               to   v-pfk(02)              .
           move      spaces               to   v-pfk(03)              .
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
       z-qry-trt-fun-000.
       z-qry-trt-fun-999.
           exit.

      *    *===========================================================*
      *    * Routine di determinazione saldo alla data                 *
      *    *                                                           *
      *    *   Input - Lettura record numerazioni rn-dat-bil           *
      *    *         - w-sad-cod-pdc : codice sottoconto di cui si     *
      *    *                           vuole ottenere il saldo         *
      *    *         - w-sad-tip-cnt : tipo sottoconto (P o E)         *
      *    *         - w-sad-dat-sad : data di cui si vuole ottenere   *
      *    *                           il saldo                        *
      *    *         - w-sad-ann-ese : anno d'esercizio in cui e' sta- *
      *    *                           to richiesto il saldo           *
      *    *         - w-sad-mes-ese : mese d'esercizio in cui e' sta- *
      *    *                           to richiesto il saldo           *
      *    *         - w-sad-ann-pre : anno d'esercizio precedente     *
      *    *                           quello in cui e' stato richie-  *
      *    *                           sto il saldo                    *
      *    *         - w-sad-dat-att : data odierna                    *
      *    *                                                           * 
      *    *  Output - w-sad-sdo-dat : saldo alla data richiesta       *
      *    *         - w-sad-sdo-ini : saldo a inizio esercizio della  *
      *    *                           data richiesta                  *
      *    *         - w-sad-prg-dar : progressivo dare da inizio e-   *
      *    *                           sercizio alla data richiesta    *
      *    *         - w-sad-prg-ave : progressivo avere da inizio e-  *
      *    *                           sercizio alla data richiesta    *
      *    *         - w-sad-snx-dfe : Si/No data saldo alla data di   *
      *    *                           fine esercizio                  *
      *    *         - w-sad-dar-bil : progressivo dare rettifiche di  *
      *    *                           bilancio                        *
      *    *         - w-sad-ave-bil : progressivo avere rettifiche di *
      *    *                           bilancio                        *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       det-sdo-dat-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione work per saldo alla data        *
      *              *-------------------------------------------------*
           move      zero                 to   w-sad-sdo-dat          .
           move      zero                 to   w-sad-sdo-ini          .
           move      zero                 to   w-sad-prg-dar          .
           move      zero                 to   w-sad-prg-ave          .
           move      "N"                  to   w-sad-snx-dfe          .
           move      zero                 to   w-sad-dar-bil          .
           move      zero                 to   w-sad-ave-bil          .
      *              *-------------------------------------------------*
      *              * Determinazione saldo ad inizio anno             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Determinazione se necessario lettura di una *
      *                  * o due schede saldi                          *
      *                  *                                             *
      *                  * - Se non e' mai stata eseguita una chiusura *
      *                  *   si leggono comunque due schede            *
      *                  *                                             *
      *                  *   altrimenti                                *
      *                  *                                             *
      *                  * - Se l'esercizio richiesto e' uguale o in-  *
      *                  *   feriore all'ultimo esercizio di chiusura  *
      *                  *   si legge una sola scheda                  *
      *                  *                                             *
      *                  *   altrimenti                                *
      *                  *                                             *
      *                  * - Se si e' in corso di chiusura si leggono  *
      *                  *   comunque due schede                       *
      *                  *                                             *
      *                  *   altrimenti                                *
      *                  *                                             *
      *                  * - Se l'esercizio precedente quello della    *
      *                  *   data richiesta e' pari all'esercizio del- *
      *                  *   l'ultima chiusura si legge una sola sche- *
      *                  *   da                                        *
      *                  *                                             *
      *                  *   altrimenti                                *
      *                  *                                             *
      *                  * - Si leggono due schede                     *
      *                  *---------------------------------------------*
           if        rn-dat-bil-ese-bil   =    zero
                     move  w-sad-ann-pre  to   w-sad-wrk-ann
                     go to det-sdo-dat-010
           else if   w-sad-ann-ese        not  > rn-dat-bil-ese-bil
                     go to det-sdo-dat-100
           else if   rn-dat-bil-flg-bil   not  = spaces
                     move  rn-dat-bil-ese-bil
                                          to   w-sad-wrk-ann
                     go to det-sdo-dat-010
           else if   w-sad-ann-pre        =    rn-dat-bil-ese-bil
                     go to det-sdo-dat-100
           else      move  w-sad-ann-pre  to   w-sad-wrk-ann
                     go to det-sdo-dat-010.
       det-sdo-dat-010.
      *                  *---------------------------------------------*
      *                  * Lettura scheda esercizio di chiusura        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se conto di tipo Economico : saldo ini- *
      *                      * ziale zero                              *
      *                      *-----------------------------------------*
           if        w-sad-tip-cnt        =    "E"
                     move   zero          to   w-sad-sdo-ini
                     go to  det-sdo-dat-060.
      *                      *-----------------------------------------*
      *                      * Se conto di tipo Patrimoniale : deter-  *
      *                      * minazione saldo finale esercizio di bi- *
      *                      * lancio                                  *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Normalizzazione record file [mgs]   *
      *                          *-------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmgs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgs                 .
      *                          *-------------------------------------*
      *                          * Preparazione campi chiave           *
      *                          *-------------------------------------*
           move      w-sad-wrk-ann        to   rf-mgs-ann-ese         .
           move      "G"                  to   rf-mgs-tip-rec         .
           move      w-sad-cod-pdc        to   rf-mgs-cod-con         .
      *                          *-------------------------------------*
      *                          * Lettura file [mgs]                  *
      *                          *-------------------------------------*
           move      "RD"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmgs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgs                 .
      *                          *-------------------------------------*
      *                          * Determinazione saldo finale eserci- *
      *                          * zio di bilancio                     *
      *                          *-------------------------------------*
           move      rf-mgs-sdo-ini       to   w-sad-sdo-ini          .
           move      zero                 to   I                      .
       det-sdo-dat-020.
           add       1                    to   I                      .
           if        I                    >    12
                     go to  det-sdo-dat-040.
           add       rf-mgs-dar-mes (I)   to   w-sad-sdo-ini          .
           subtract  rf-mgs-ave-mes (I)   from w-sad-sdo-ini          .
           go to     det-sdo-dat-020.
       det-sdo-dat-040.
           add       rf-mgs-dar-bil       to   w-sad-sdo-ini          .
           subtract  rf-mgs-ave-bil       from w-sad-sdo-ini          .
       det-sdo-dat-060.
      *                      *-----------------------------------------*
      *                      * Lettura [mgs] esercizio richiesto       *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Normalizzazione record file [mgs]   *
      *                          *-------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmgs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgs                 .
      *                          *-------------------------------------*
      *                          * Preparazione chiave per [mgs]       *
      *                          *-------------------------------------*
           move      w-sad-ann-ese        to   rf-mgs-ann-ese         .
           move      "G"                  to   rf-mgs-tip-rec         .
           move      w-sad-cod-pdc        to   rf-mgs-cod-con         .
      *                          *-------------------------------------*
      *                          * Lettura file [mgs]                  *
      *                          *-------------------------------------*
           move      "RD"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmgs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgs                 .
           go to     det-sdo-dat-200.
       det-sdo-dat-100.
      *                  *---------------------------------------------*
      *                  * Lettura scheda esercizio richiesto          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazione record file [mgs]       *
      *                      *-----------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmgs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgs                 .
      *                      *-----------------------------------------*
      *                      * Preparazione chiave per [mgs]           *
      *                      *-----------------------------------------*
           move      w-sad-ann-ese        to   rf-mgs-ann-ese         .
           move      "G"                  to   rf-mgs-tip-rec         .
           move      w-sad-cod-pdc        to   rf-mgs-cod-con         .
      *                      *-----------------------------------------*
      *                      * Lettura file [mgs]                      *
      *                      *-----------------------------------------*
           move      "RD"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmgs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgs                 .
      *                      *-----------------------------------------*
      *                      * Determinazione saldo inizio anno        *
      *                      *-----------------------------------------*
           move      rf-mgs-sdo-ini       to   w-sad-sdo-ini          .
       det-sdo-dat-200.
      *              *-------------------------------------------------*
      *              * Ciclo di scansione per accumulo progressivi men-*
      *              * sili anno in corso fino a mese precedente a     *
      *              * quello indicato da data situazione              *
      *              *-------------------------------------------------*
           move      w-sad-mes-ese        to   I                      .
       det-sdo-dat-220.
           subtract  1                    from I                      .
           if        I                    =    zero
                     go to  det-sdo-dat-240.
           add       rf-mgs-dar-mes (I)   to   w-sad-prg-dar          .
           add       rf-mgs-ave-mes (I)   to   w-sad-prg-ave          .
           go to     det-sdo-dat-220.
       det-sdo-dat-240.
      *              *-------------------------------------------------*
      *              * Se data di cui e 'stato richiesto il saldo e'   *
      *              * maggiore o uguale a data attuale si aggiungono  *
      *              * i progressivi del mese attuale                  *
      *              *-------------------------------------------------*
           if        w-sad-dat-sad        <    w-sad-dat-att
                     go to  det-sdo-dat-250.
           add       rf-mgs-dar-mes
                     (w-sad-mes-ese)      to   w-sad-prg-dar          .
           add       rf-mgs-ave-mes
                     (w-sad-mes-ese)      to   w-sad-prg-ave          .
           go to     det-sdo-dat-900.
       det-sdo-dat-250.
      *              *-------------------------------------------------*
      *              * Controllo se giorno situazione e' fine mese     *
      *              *-------------------------------------------------*
           move      w-sad-dat-sad        to   s-dat                  .
           add       1                    to   s-gio                  .
           move      "CD"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Se la data e' corretta vuol dire che il     *
      *                  * giorno situazione non e' a fine mese        *
      *                  *---------------------------------------------*
           if        s-sts                =    spaces
                     go to  det-sdo-dat-300.
      *                      *-----------------------------------------*
      *                      * Se fine mese si aggiungono i progressivi*
      *                      * del mese attuale                        *
      *                      *-----------------------------------------*
           add       rf-mgs-dar-mes
                     (w-sad-mes-ese)      to   w-sad-prg-dar          .
           add       rf-mgs-ave-mes
                     (w-sad-mes-ese)      to   w-sad-prg-ave          .
      *                      *-----------------------------------------*
      *                      * Se mese richiesto e' quello di chiusura *
      *                      * esercizio                               *
      *                      *-----------------------------------------*
           if        w-sad-mes-ese        not  = w-ese-cge-mce
                     go to  det-sdo-dat-900.
      *                          *-------------------------------------*
      *                          * Si/No data di fine esercizio : Si   *
      *                          *-------------------------------------*
           move      "S"                  to   w-sad-snx-dfe          .
      *                          *-------------------------------------*
      *                          * Bufferizzazione progressivi retti-  *
      *                          * fiche di bilancio                   *
      *                          *-------------------------------------*
           move      rf-mgs-dar-bil       to   w-sad-dar-bil          .
           move      rf-mgs-ave-bil       to   w-sad-ave-bil          .
      *                          *-------------------------------------*
      *                          * A uscita                            *
      *                          *-------------------------------------*
           go to     det-sdo-dat-900.
       det-sdo-dat-300.
      *              *-------------------------------------------------*
      *              * Normalizzazione record file [mgr]               *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmgr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgr                 .
      *              *-------------------------------------------------*
      *              * Preparazione chiave per lettura file [mgr]      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Codice sottoconto                           *
      *                  *---------------------------------------------*
           move      w-sad-cod-pdc        to   rf-mgr-cod-pdc         .
      *                  *---------------------------------------------*
      *                  * Data di registrazione                       *
      *                  *---------------------------------------------*
           move      zero                 to   s-dat                  .
           move      01                   to   s-gio                  .
           move      w-sad-mes-ese        to   s-mes                  .
           move      w-sad-ann-ese        to   s-saa                  .
           move      s-dat                to   rf-mgr-dat-reg         .
      *                  *---------------------------------------------*
      *                  * Numero protocollo                           *
      *                  *---------------------------------------------*
           move      zero                 to   rf-mgr-num-prt         .
      *                  *---------------------------------------------*
      *                  * Numero progressivo                          *
      *                  *---------------------------------------------*
           move      zero                 to   rf-mgr-num-prg         .
      *              *-------------------------------------------------*
      *              * Start file [mgr] per chiave "PDCDAT"            *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "PDCDAT"             to   f-key                  .
           move      "pgm/cge/fls/ioc/obj/iofmgr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgr                 .
      *                  *---------------------------------------------*
      *                  * Test su esito start                         *
      *                  *---------------------------------------------*
           if        f-sts                =    e-end-fil
                     go to  det-sdo-dat-900.
       det-sdo-dat-500.
      *              *-------------------------------------------------*
      *              * Lettura sequenziale file [mgr]                  *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmgr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgr                 .
      *                  *---------------------------------------------*
      *                  * Test su esito lettura                       *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to  det-sdo-dat-900.
      *              *-------------------------------------------------*
      *              * Test se superamento limiti massimi              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Codice sottoconto                           *
      *                  *---------------------------------------------*
           if        rf-mgr-cod-pdc        not  = w-sad-cod-pdc
                     go to  det-sdo-dat-900.
      *                  *---------------------------------------------*
      *                  * Data registrazione                          *
      *                  *---------------------------------------------*
           if        rf-mgr-dat-reg       >    w-sad-dat-sad
                     go to  det-sdo-dat-900.
      *                  *---------------------------------------------*
      *                  * Se movimento di bilancio si ricicla         *
      *                  *---------------------------------------------*
           if        rf-mgr-snx-mob        not  = "N"
                     go to  det-sdo-dat-500.
      *              *-------------------------------------------------*
      *              * Aggiornamento saldo alla data                   *
      *              *-------------------------------------------------*
           if        rf-mgr-dar-ave       =    "A"
                     add      rf-mgr-imp-mov
                                          to   w-sad-prg-ave
           else      add      rf-mgr-imp-mov
                                          to   w-sad-prg-dar          .
      *              *-------------------------------------------------*
      *              * Riciclo su lettura [mgr]                        *
      *              *-------------------------------------------------*
           go to     det-sdo-dat-500.
       det-sdo-dat-900.
      *              *-------------------------------------------------*
      *              * Determinazione saldo alla data richiesta        *
      *              *-------------------------------------------------*
           move      w-sad-sdo-ini        to   w-sad-sdo-dat          .
           add       w-sad-prg-dar        to   w-sad-sdo-dat          .
           subtract  w-sad-prg-ave        from w-sad-sdo-dat          .
           add       w-sad-dar-bil        to   w-sad-sdo-dat          .
           subtract  w-sad-ave-bil        from w-sad-sdo-dat          .
       det-sdo-dat-999.
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
      *    * Editing del codice sottoconto con appoggio a sx o dx      *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/wedtpdc0.wks"                   .

