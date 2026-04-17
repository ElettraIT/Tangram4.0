       Identification Division.
       Program-Id.                                 pcge3012           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    cge                 *
      *                                Settore:    mov                 *
      *                                   Fase:    cge301              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 29/12/89    *
      *                       Ultima revisione:    NdK del 12/02/19    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Esecuzione richieste per interrogazione     *
      *                    programma cge3010:                          *
      *                                                                *
      *                    Lista movimenti cliente                     *
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
      *    * Record file                                               *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [mgt]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfmgt"                          .
      *        *-------------------------------------------------------*
      *        * [mgr]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfmgr"                          .
      *        *-------------------------------------------------------*
      *        * [zcc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfzcc"                          .

      *    *===========================================================*
      *    * Work-area per lettura personalizzazioni                   *
      *    *-----------------------------------------------------------*
       01  w-prs.
      *        *-------------------------------------------------------*
      *        * Si/no visualizzazione con progressivo                 *
      *        *-------------------------------------------------------*
           05  w-prs-snx-prg              pic  x(01)                  .

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
      *    * Work-area per ridefinizione contenuto del mark-point      *
      *    *-----------------------------------------------------------*
       01  w-mpn.
           05  w-mpn-dat-reg              pic  9(07)                  .
           05  w-mpn-num-prt              pic  9(07)                  .
           05  w-mpn-num-prg              pic  9(05)                  .

      *    *===========================================================*
      *    * Work area per registrazione in corso di trattamento       *
      *    *-----------------------------------------------------------*
       01  w-reg.
      *        *-------------------------------------------------------*
      *        * Date registrazione primo ed ultimo movimento          *
      *        *-------------------------------------------------------*
           05  w-reg-dat-ini              pic  9(07) value zero       .
           05  w-reg-dat-fin              pic  9(07) value zero       .

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
      *    * Work per salvataggio 'Start'                              *
      *    *-----------------------------------------------------------*
       01  w-sav.
           05  w-sav-tip-arc              pic  x(01)                  .
           05  w-sav-cod-arc              pic  9(07)                  .
           05  w-sav-dat-reg              pic  9(07)                  .
           05  w-sav-num-prt              pic  9(07)                  .
           05  w-sav-num-prg              pic  9(05)                  .

      *    *===========================================================*
      *    * Work area locale                                          *
      *    *-----------------------------------------------------------*
       01  w-wrk.
      *        *-------------------------------------------------------*
      *        * Progressivo dare movimenti letti                      *
      *        *-------------------------------------------------------*
           05  w-wrk-prg-dar              pic s9(13)                  .
      *        *-------------------------------------------------------*
      *        * Progressivo avere movimenti letti                     *
      *        *-------------------------------------------------------*
           05  w-wrk-prg-ave              pic s9(13)                  .
      *        *-------------------------------------------------------*
      *        * Progressivo assoluto movimenti letti                  *
      *        *-------------------------------------------------------*
           05  w-wrk-prg-ass              pic s9(13)                  .
      *        *-------------------------------------------------------*
      *        * Contatore di comodo I                                 *
      *        *-------------------------------------------------------*
           05  I                          pic  9(03)                  .

      *    *===========================================================*
      *    * Work area descrizione causale e commento in riga          *
      *    *-----------------------------------------------------------*
       01  w-dca.
           05  w-dca-des-cau.
               10  w-dca-rig-cau occurs 5 pic  x(40)                  .
           05  w-dca-com-rig              pic  x(40)                  .
           05  w-dca-num-rig              pic  9(01)                  .

      *    *===========================================================*
      *    * Work area per controllo descrizione operazione precedente *
      *    *-----------------------------------------------------------*
       01  w-dop.
           05  w-dop-dat-reg              pic  9(07)                  .
           05  w-dop-num-prt              pic  9(07)                  .
           05  w-dop-des-cau.
               10  w-dop-rig-cau occurs 4 pic  x(40)                  .
           05  w-dop-com-rig              pic  x(40)                  .

      *    *===========================================================*
      *    * Work-area per allineamenti a destra o a sinistra oppure   *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cpw"                   .

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
      *              * Esecuzione routine pre-esecuzione programma     *
      *              *-------------------------------------------------*
           move      spaces               to   OK                     .
           perform   pre-exe-pgm-000      thru pre-exe-pgm-999        .
           if        OK                   not  = spaces
                     go to  main-999.
       main-200.
      *              *-------------------------------------------------*
      *              * Esecuzione interrogazione                       *
      *              *-------------------------------------------------*
           move      spaces               to   OK                     .
           perform   z-qry-rou-pri-000    thru z-qry-rou-pri-999      .
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
           perform   qry-det-fky-000      thru qry-det-fky-999        .
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
           perform   qry-sel-rec-000      thru qry-sel-rec-999        .
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
           perform   qry-liv-det-000      thru qry-liv-det-999        .
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
           perform   qry-pag-adv-000      thru qry-pag-adv-999        .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     z-qry-rou-pri-900.
       z-qry-rou-pri-600.
      *              *-------------------------------------------------*
      *              * Esecuzione per nessuna registrazione da elab.   *
      *              *-------------------------------------------------*
           perform   qry-nes-ela-000      thru qry-nes-ela-999        .
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
           perform   qry-fin-cic-000      thru qry-fin-cic-999        .
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
                     move  "#"            to   OK
                     go to qry-pag-adv-999.
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
                     go to qry-pag-adv-000.
       qry-pag-adv-999.
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
      *              * Lettura personalizzazione relativa alla         *
      *              * visualizzazione del progressivo                 *
      *              *-------------------------------------------------*
           perform   prs-snx-prg-000      thru prs-snx-prg-999        .
       pre-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Lettura della personalizzazione relativa alla visualizza- *
      *    * zione del progressivo                                     *
      *    *-----------------------------------------------------------*
       prs-snx-prg-000.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione                       *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/cge/mov/cge301[snx-prg]"
                                          to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-alf          to   w-prs-snx-prg
           else      move  spaces         to   w-prs-snx-prg          .
      *              *-------------------------------------------------*
      *              * Normalizzazione personalizzazione               *
      *              *-------------------------------------------------*
           if        w-prs-snx-prg        not  = "S"
                     move  "N"            to   w-prs-snx-prg          .
       prs-snx-prg-999.
           exit.

      *    *===========================================================*
      *    * Open files : se errori v-mrk:= "#"                        *
      *    *-----------------------------------------------------------*
       z-qry-opn-fls-000.
      *              *-------------------------------------------------*
      *              * [mgt]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmgt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgt                 .
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
      *              * [zcc]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofzcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zcc                 .
       z-qry-opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Close files : se errori v-mrk:= "#"                       *
      *    *-----------------------------------------------------------*
       z-qry-cls-fls-000.
      *              *-------------------------------------------------*
      *              * [mgt]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmgt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgt                 .
      *              *-------------------------------------------------*
      *              * [mgr]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmgr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgr                 .
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
       z-qry-cls-fls-999.
           exit.

      *    *===========================================================*
      *    * Start iniziale - se non valida v-mrk:= "#"                *
      *    *-----------------------------------------------------------*
       qry-str-ini-000.
      *              *-------------------------------------------------*
      *              * Tipo operazione: Start per chiave generica      *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
      *              *-------------------------------------------------*
      *              * Tipo di confronto : 'not less'                  *
      *              *-------------------------------------------------*
           move      "NL"                 to   f-cfr                  .
      *              *-------------------------------------------------*
      *              * Preparazione chiave                             *
      *              *-------------------------------------------------*
           move      "C"                  to   rf-mgr-tip-arc         .
           move      rr-cod-arc           to   rf-mgr-cod-arc         .
           move      rr-dat-ini           to   rf-mgr-dat-reg         .
           move      zero                 to   rf-mgr-num-prt         .
           move      zero                 to   rf-mgr-num-prg         .
      *              *-------------------------------------------------*
      *              * Nome chiave                                     *
      *              *-------------------------------------------------*
           move      "ARCDAT"             to   f-key                  .
      *              *-------------------------------------------------*
      *              * Richiamo modulo di input-output                 *
      *              *-------------------------------------------------*
           move      "pgm/cge/fls/ioc/obj/iofmgr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgr                 .
      *              *-------------------------------------------------*
      *              * Test su successo operazione                     *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     move   "#"           to   v-mrk
                     go to  qry-str-ini-999.
      *              *-------------------------------------------------*
      *              * Inizializzazione area controllo descrizione op. *
      *              *-------------------------------------------------*
           move      zero                 to   w-dop-dat-reg          .
           move      zero                 to   w-dop-num-prt          .
           move      spaces               to   w-dop-des-cau          .
           move      spaces               to   w-dop-com-rig          .
       qry-str-ini-999.
           exit.

      *    *===========================================================*
      *    * Messaggio per nessuna registrazione da elaborare          *
      *    *-----------------------------------------------------------*
       qry-nes-ela-000.
           move      "ME"                 to   v-ope                  .
           move      "Nessuna registrazione entro i limiti impostati"
                                          to   v-not                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       qry-nes-ela-999.
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
      *              *-------------------------------------------------*
      *              * Tipo operazione: Read Next                      *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
      *              *-------------------------------------------------*
      *              * Richiamo modulo di input-output                 *
      *              *-------------------------------------------------*
           move      "pgm/cge/fls/ioc/obj/iofmgr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgr                 .
      *              *-------------------------------------------------*
      *              * Test su successo operazione                     *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     move  "#"            to   v-mrk                  .
       qry-let-seq-999.
           exit.

      *    *===========================================================*
      *    * Test superamento limiti massimi - se si' v-mrk:= "#"      *
      *    *-----------------------------------------------------------*
       qry-tst-max-000.
           if        rf-mgr-tip-arc       not  = "C"        or
                     rf-mgr-cod-arc       not  = rr-cod-arc or
                     rf-mgr-dat-reg       >    rr-dat-fin
                     move   "#"           to   v-mrk                  .
       qry-tst-max-999.
           exit.

      *    *===========================================================*
      *    * Selezione su record letto - se da ignorare v-mrk:= "#"    *
      *    *-----------------------------------------------------------*
       qry-sel-rec-000.
      *              *-------------------------------------------------*
      *              * Test su causale contabile                       *
      *              *-------------------------------------------------*
           if        rr-sel-cau           =    zero
                     go to  qry-sel-rec-050.
           if        rf-mgr-cod-cau       not  = rr-sel-cau
                     move   "#"           to   v-mrk
                     go to  qry-sel-rec-999.
       qry-sel-rec-050.
      *              *-------------------------------------------------*
      *              * Test su importo                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su minimo                              *
      *                  *---------------------------------------------*
           if        rr-sel-imi           =    zero
                     go to qry-sel-rec-060.
       qry-sel-rec-055.
      *                  *---------------------------------------------*
      *                  * Minimo                                      *
      *                  *---------------------------------------------*
           if        rf-mgr-imp-mov       <    rr-sel-imi
                     move   "#"           to   v-mrk
                     go to  qry-sel-rec-999.
       qry-sel-rec-060.
      *                  *---------------------------------------------*
      *                  * Massimo                                     *
      *                  *---------------------------------------------*
           if        rr-sel-ima           =    zero
                     go to qry-sel-rec-100.
           if        rf-mgr-imp-mov       >    rr-sel-ima
                     move   "#"           to   v-mrk
                     go to  qry-sel-rec-999.
       qry-sel-rec-100.
      *              *-------------------------------------------------*
      *              * Test su contenuto commento                      *
      *              *-------------------------------------------------*
           if        rr-sel-cco           =    spaces
                     go to  qry-sel-rec-500.
      *                  *---------------------------------------------*
      *                  * Preparazione commento per il confronto      *
      *                  *                                             *
      *                  * Normalizzazione del valore impostato in     *
      *                  * formato privo di spaces e di caratteri      *
      *                  * diversi da A..Z - 0..9                      *
      *                  *---------------------------------------------*
           if        rf-mgr-com-rig       =    spaces
                     move  "#"            to   v-mrk
                     go to qry-sel-rec-999.
      *
           move      rf-mgr-com-rig       to   w-all-str-alf          .
           move      40                   to   w-all-str-lun          .
           perform   all-nor-atz-000      thru all-nor-atz-999        .
           move      w-all-str-alf        to   w-all-str-cat (2)      .
      *                  *---------------------------------------------*
      *                  * Preparazione stringa per il confronto       *
      *                  *---------------------------------------------*
           move      rr-sel-cco           to   w-all-str-cat (1)      .
      *                  *---------------------------------------------*
      *                  * Match tra i due valori                      *
      *                  *---------------------------------------------*
           perform   all-mch-atz-000      thru all-mch-atz-999        .
      *                      *-----------------------------------------*
      *                      * Se c'e' stato un match : selezione su-  *
      *                      * perata                                  *
      *                      *-----------------------------------------*
           if        w-all-str-flg        =    spaces
                     go to qry-sel-rec-500
           else      move   "#"           to   v-mrk
                     go to qry-sel-rec-999.
       qry-sel-rec-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione record [mgt]                    *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmgt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgt                 .
      *              *-------------------------------------------------*
      *              * Lettura [mgt] corrispondente a [mgr]            *
      *              *-------------------------------------------------*
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
       qry-sel-rec-550.
      *              *-------------------------------------------------*
      *              * Test su contenuto causale                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se da eseguire                         *
      *                  *---------------------------------------------*
           if        rr-sel-cca           =    spaces
                     go to  qry-sel-rec-900.
       qry-sel-rec-552.
      *                  *---------------------------------------------*
      *                  * Preparazione riga 1 per il confronto        *
      *                  *                                             *
      *                  * Normalizzazione del valore impostato in     *
      *                  * formato privo di spaces e di caratteri      *
      *                  * diversi da A..Z - 0..9                      *
      *                  *---------------------------------------------*
           move      rf-mgt-rig-cau (1)   to   w-all-str-alf          .
           move      40                   to   w-all-str-lun          .
           perform   all-nor-atz-000      thru all-nor-atz-999        .
           move      w-all-str-alf        to   w-all-str-cat (2)      .
      *                  *---------------------------------------------*
      *                  * Preparazione stringa per il confronto       *
      *                  *---------------------------------------------*
           move      rr-sel-cca           to   w-all-str-cat (1)      .
      *                  *---------------------------------------------*
      *                  * Match tra i due valori                      *
      *                  *---------------------------------------------*
           perform   all-mch-atz-000      thru all-mch-atz-999        .
      *                      *-----------------------------------------*
      *                      * Se c'e' stato un match : selezione su-  *
      *                      * perata                                  *
      *                      *-----------------------------------------*
           if        w-all-str-flg        =    spaces
                     go to qry-sel-rec-900.
       qry-sel-rec-554.
      *                  *---------------------------------------------*
      *                  * Preparazione riga 2 per il confronto        *
      *                  *                                             *
      *                  * Normalizzazione del valore impostato in     *
      *                  * formato privo di spaces e di caratteri      *
      *                  * diversi da A..Z - 0..9                      *
      *                  *---------------------------------------------*
           if        rf-mgt-rig-cau (2)   =    spaces
                     go to qry-sel-rec-560.
      *
           move      rf-mgt-rig-cau (2)   to   w-all-str-alf          .
           move      40                   to   w-all-str-lun          .
           perform   all-nor-atz-000      thru all-nor-atz-999        .
           move      w-all-str-alf        to   w-all-str-cat (2)      .
      *                  *---------------------------------------------*
      *                  * Preparazione stringa per il confronto       *
      *                  *---------------------------------------------*
           move      rr-sel-cca           to   w-all-str-cat (1)      .
      *                  *---------------------------------------------*
      *                  * Match tra i due valori                      *
      *                  *---------------------------------------------*
           perform   all-mch-atz-000      thru all-mch-atz-999        .
      *                      *-----------------------------------------*
      *                      * Se c'e' stato un match : selezione su-  *
      *                      * perata                                  *
      *                      *-----------------------------------------*
           if        w-all-str-flg        =    spaces
                     go to qry-sel-rec-900.
       qry-sel-rec-556.
      *                  *---------------------------------------------*
      *                  * Preparazione riga 3 per il confronto        *
      *                  *                                             *
      *                  * Normalizzazione del valore impostato in     *
      *                  * formato privo di spaces e di caratteri      *
      *                  * diversi da A..Z - 0..9                      *
      *                  *---------------------------------------------*
           if        rf-mgt-rig-cau (3)   =    spaces
                     go to qry-sel-rec-560.
      *
           move      rf-mgt-rig-cau (3)   to   w-all-str-alf          .
           move      40                   to   w-all-str-lun          .
           perform   all-nor-atz-000      thru all-nor-atz-999        .
           move      w-all-str-alf        to   w-all-str-cat (2)      .
      *                  *---------------------------------------------*
      *                  * Preparazione stringa per il confronto       *
      *                  *---------------------------------------------*
           move      rr-sel-cca           to   w-all-str-cat (1)      .
      *                  *---------------------------------------------*
      *                  * Match tra i due valori                      *
      *                  *---------------------------------------------*
           perform   all-mch-atz-000      thru all-mch-atz-999        .
      *                      *-----------------------------------------*
      *                      * Se c'e' stato un match : selezione su-  *
      *                      * perata                                  *
      *                      *-----------------------------------------*
           if        w-all-str-flg        =    spaces
                     go to qry-sel-rec-900.
       qry-sel-rec-558.
      *                  *---------------------------------------------*
      *                  * Preparazione riga 4 per il confronto        *
      *                  *                                             *
      *                  * Normalizzazione del valore impostato in     *
      *                  * formato privo di spaces e di caratteri      *
      *                  * diversi da A..Z - 0..9                      *
      *                  *---------------------------------------------*
           if        rf-mgt-rig-cau (4)   =    spaces
                     go to qry-sel-rec-560.
      *
           move      rf-mgt-rig-cau (4)   to   w-all-str-alf          .
           move      40                   to   w-all-str-lun          .
           perform   all-nor-atz-000      thru all-nor-atz-999        .
           move      w-all-str-alf        to   w-all-str-cat (2)      .
      *                  *---------------------------------------------*
      *                  * Preparazione stringa per il confronto       *
      *                  *---------------------------------------------*
           move      rr-sel-cca           to   w-all-str-cat (1)      .
      *                  *---------------------------------------------*
      *                  * Match tra i due valori                      *
      *                  *---------------------------------------------*
           perform   all-mch-atz-000      thru all-mch-atz-999        .
      *                      *-----------------------------------------*
      *                      * Se c'e' stato un match : selezione su-  *
      *                      * perata                                  *
      *                      *-----------------------------------------*
           if        w-all-str-flg        =    spaces
                     go to qry-sel-rec-900.
       qry-sel-rec-560.
      *                  *---------------------------------------------*
      *                  * Se nessun confronto superato : selezione    *
      *                  * non superata                                *
      *                  *---------------------------------------------*
           move      "#"                  to   v-mrk                  .                                               
       qry-sel-rec-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     qry-sel-rec-999.
       qry-sel-rec-999.
           exit.

      *    *===========================================================*
      *    * Composizione area di rottura w-rot                        *
      *    *-----------------------------------------------------------*
       qry-cmp-rot-000.
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
      *              * Azzeramento progressivi movimenti letti         *
      *              *-------------------------------------------------*
           move      zero                 to   w-wrk-prg-dar          .
           move      zero                 to   w-wrk-prg-ave          .
           move      zero                 to   w-wrk-prg-ass          .
      *              *-------------------------------------------------*
      *              * Intestazione pagina video                       *
      *              *-------------------------------------------------*
           perform   int-pag-vid-000      thru int-pag-vid-999        .
      *                  *---------------------------------------------*
      *                  * Test su interazione con utente              *
      *                  *---------------------------------------------*
           if        v-key                not  = spaces
                     move   "#"           to   OK                     .
       qry-ini-cic-999.
           exit.

      *    *===========================================================*
      *    * Esecuzione per fine ciclo                                 *
      *    *                                                           *
      *    * Uscita  : OK = spaces : continua l'esecuzione             *
      *    *                "#"    : terminazione programma            *
      *    *-----------------------------------------------------------*
       qry-fin-cic-000.
      *              *-------------------------------------------------*
      *              * Test su linee residue                           *
      *              *-------------------------------------------------*
           if        v-res                >    2
                     go to  qry-fin-cic-100.
      *                  *---------------------------------------------*
      *                  * Intestazione pagina                         *
      *                  *---------------------------------------------*
           perform int-pag-vid-000        thru int-pag-vid-999        .
      *                      *-----------------------------------------*
      *                      * Test su interazione con utente          *
      *                      *-----------------------------------------*
           if        v-key                not  = spaces
                     move   "#"           to   OK
                     go to  qry-fin-cic-999.
       qry-fin-cic-100.
      *              *-------------------------------------------------*
      *              * Trattini di separazione                         *
      *              *-------------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      59                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      22                   to   v-pos                  .
           move      all "-"              to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Literal 'Totale movimenti selezionati :'        *
      *              *-------------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      22                   to   v-pos                  .
           move      "Totale movimenti selezionati :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Progressivo dare movimenti selezionati          *
      *              *-------------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      13                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "G"                  to   v-edm                  .
           move      v-lnr                to   v-lin                  .
           move      53                   to   v-pos                  .
           move      w-wrk-prg-dar        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       qry-fin-cic-300.
      *              *-------------------------------------------------*
      *              * Literal contenente il range di date             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Editing data iniziale                       *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
      *
           if        w-reg-dat-ini        not  = zero and
                     rr-dat-ini           =    zero
                     move  w-reg-dat-ini  to   rr-dat-ini             .
      *
           move      rr-dat-ini           to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Assemblaggio                                *
      *                  *---------------------------------------------*
           move      30                   to   w-all-str-lun          .
           move      02                   to   w-all-str-num          .
           move      "("                  to   w-all-str-cat (1)      .
           move      v-edt                to   w-all-str-cat (2)      .
      *
           if        rr-dat-ini           =    0
                     move  "INIZIO"       to   w-all-str-cat (2)      .
      *
           perform   all-str-cat-000      thru all-str-cat-999        .
      *                  *---------------------------------------------*
      *                  * Editing data finale                         *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
      *
           if        w-reg-dat-fin        not  = zero and
                     rr-dat-fin           =    9991231
                     move  w-reg-dat-fin  to   rr-dat-fin             .
      *
           move      rr-dat-fin           to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Assemblaggio                                *
      *                  *---------------------------------------------*
           move      40                   to   w-all-str-lun          .
           move      03                   to   w-all-str-num          .
           move      w-all-str-alf        to   w-all-str-cat (1)      .
           move      "-"                  to   w-all-str-cat (2)      .
           move      v-edt                to   w-all-str-cat (3)      .
           perform   all-str-csb-000      thru all-str-csb-999        .
      *
           move      02                   to   w-all-str-num          .
           move      w-all-str-alf        to   w-all-str-cat (1)      .
           move      ")"                  to   w-all-str-cat (2)      .
           perform   all-str-cat-000      thru all-str-cat-999        .
      *                  *---------------------------------------------*
      *                  * Literal '(99/99/99 - 99/99/99)'             *
      *                  *---------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      22                   to   v-pos                  .
           move      w-all-str-alf        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       qry-fin-cic-400.
      *              *-------------------------------------------------*
      *              * Progressivo avere movimenti selezionati         *
      *              *-------------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      13                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "G"                  to   v-edm                  .
           move      v-lnr                to   v-lin                  .
           move      63                   to   v-pos                  .
           move      w-wrk-prg-ave        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       qry-fin-cic-999.
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
       qry-liv-det-000.
      *              *-------------------------------------------------*
      *              * Aggiornamento progressivi assoluto              *
      *              *-------------------------------------------------*
           if        rf-mgr-dar-ave       =    "A"
                     add      rf-mgr-imp-mov
                                          to   w-wrk-prg-ass
           else      subtract rf-mgr-imp-mov
                                          from w-wrk-prg-ass          .
      *              *-------------------------------------------------*
      *              * Bufferizzazione della prima e dell'ultima data  *
      *              * incontrate                                      *
      *              *-------------------------------------------------*
           if        w-reg-dat-ini        =    zero
                     move  rf-mgt-dat-reg to   w-reg-dat-ini          .
           move      rf-mgt-dat-reg       to   w-reg-dat-fin          .
       qry-liv-det-050.
      *              *-------------------------------------------------*
      *              * Se numero linee residue insufficiente per una   *
      *              * ulteriore linea : intestazione pagina           *
      *              *-------------------------------------------------*
           if        v-res                >    zero
                     go to qry-liv-det-100.
           perform   int-pag-vid-000      thru int-pag-vid-999        .
      *                  *---------------------------------------------*
      *                  * Test su interazione con utente              *
      *                  *---------------------------------------------*
           if        v-key                not  = spaces
                     move   "#"           to   OK
                     go to  qry-liv-det-999.
       qry-liv-det-100.
      *              *-------------------------------------------------*
      *              * Stampa testata                                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in presenza della personalizza-  *
      *                  * zione                                       *
      *                  *---------------------------------------------*
           if        w-prs-snx-prg        not  = "S"
                     go to qry-liv-det-105.
      *                  *---------------------------------------------*
      *                  * Linea di dettaglio con progressivo saldo    *
      *                  *---------------------------------------------*
           perform   qry-liv-det-prg-000  thru qry-liv-det-prg-999    .
      *                  *---------------------------------------------*
      *                  * Ad aggiornamento progressivi                *
      *                  *---------------------------------------------*
           go to     qry-liv-det-900.
       qry-liv-det-105.
      *                  *---------------------------------------------*
      *                  * Asterisco se movimento di bilancio          *
      *                  *---------------------------------------------*
           if        rf-mgt-dat-reg       =    w-dop-dat-reg and
                     rf-mgt-num-prt       =    w-dop-num-prt
                     go to qry-liv-det-110.
           if        rf-mgt-snx-mob       =    "N"
                     go to qry-liv-det-110.
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "*"                  to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       qry-liv-det-110.
      *                  *---------------------------------------------*
      *                  * Data registrazione                          *
      *                  *---------------------------------------------*
           if        rf-mgt-dat-reg       =    w-dop-dat-reg and
                     rf-mgt-num-prt       =    w-dop-num-prt
                     go to qry-liv-det-120.
           move      "PF"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      v-lnr                to   v-lin                  .
           move      03                   to   v-pos                  .
           move      rf-mgr-dat-reg       to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           go to     qry-liv-det-130.
       qry-liv-det-120.
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      09                   to   v-pos                  .
           move      "--"                 to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       qry-liv-det-130.
      *                  *---------------------------------------------*
      *                  * Concatenazione descrizione causale e riga   *
      *                  * di commento in area di comodo               *
      *                  *---------------------------------------------*
           move      rf-mgt-des-cau       to   w-dca-des-cau          .
           move      rf-mgr-com-rig       to   w-dca-com-rig          .
           perform   dca-con-cat-000      thru dca-con-cat-999        .
      *                  *---------------------------------------------*
      *                  * Prima linea descrizione causale             *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se data registrazione e numero     *
      *                      * protocollo gia' visualizzati            *
      *                      *-----------------------------------------*
           if        rf-mgt-dat-reg       =    w-dop-dat-reg and
                     rf-mgt-num-prt       =    w-dop-num-prt
                     go to qry-liv-det-140.
      *                      *-----------------------------------------*
      *                      * Eventuale concatenazione con altri      *
      *                      * elementi                                *
      *                      *-----------------------------------------*
           perform   dca-con-ael-000      thru dca-con-ael-999        .
      *                      *-----------------------------------------*
      *                      * Visualizzazione                         *
      *                      *-----------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      12                   to   v-pos                  .
           move      w-dca-rig-cau (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * A importo                               *
      *                      *-----------------------------------------*
           go to     qry-liv-det-190.
       qry-liv-det-140.
      *                  *---------------------------------------------*
      *                  * Linea di commento                           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se riga di commento gia' visualiz- *
      *                      * zata                                    *
      *                      *-----------------------------------------*
           if        rf-mgr-com-rig       =    w-dop-com-rig
                     go to qry-liv-det-150.
      *                      *-----------------------------------------*
      *                      * Visualizzazione                         *
      *                      *-----------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      12                   to   v-pos                  .
           move      rf-mgr-com-rig       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * A importo                               *
      *                      *-----------------------------------------*
           go to     qry-liv-det-190.
       qry-liv-det-150.
      *                  *---------------------------------------------*
      *                  * Indicatore di linea che segue               *
      *                  *---------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      12                   to   v-pos                  .
           move      """"""               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       qry-liv-det-190.
      *                  *---------------------------------------------*
      *                  * Mark-Point                                  *
      *                  *---------------------------------------------*
           move      "+"                  to   v-edm                  .
           move      rf-mgr-dat-reg       to   w-mpn-dat-reg          .
           move      rf-mgr-num-prt       to   w-mpn-num-prt          .
           move      rf-mgr-num-prg       to   w-mpn-num-prg          .
           move      w-mpn                to   v-cnt                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Importo movimento                           *
      *                  *---------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      13                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "G"                  to   v-edm                  .
           move      v-lnr                to   v-lin                  .
           if        rf-mgr-dar-ave       =    "A"
                     move   63            to   v-pos
           else      move   53            to   v-pos                  .
           move      rf-mgr-imp-mov       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Interlinea                              *
      *                      *-----------------------------------------*
           move      "LF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Righe descrizione causale residue           *
      *                  *---------------------------------------------*
           if        rf-mgt-dat-reg       =    w-dop-dat-reg and
                     rf-mgt-num-prt       =    w-dop-num-prt
                     go to qry-liv-det-900.
           move      1                    to   I                      .
       qry-liv-det-200.
           add       1                    to   I                      .
           if        I                    >    5
                     go to  qry-liv-det-900.
           if        w-dca-rig-cau (I)    =    spaces
                     go to  qry-liv-det-200.
      *                      *-----------------------------------------*
      *                      * Test su linee residue                   *
      *                      *-----------------------------------------*
           if        v-res                >    zero
                     go to  qry-liv-det-220.
           perform   int-pag-vid-000      thru int-pag-vid-999        .
           if        v-key                not  = spaces
                     move   "#"           to   OK
                     go to  qry-liv-det-999.
       qry-liv-det-220.
      *                      *-----------------------------------------*
      *                      * Riga descrizione causale i-esima        *
      *                      *-----------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      12                   to   v-pos                  .
           move      w-dca-rig-cau (I)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                          *-------------------------------------*
      *                          * Interlinea                          *
      *                          *-------------------------------------*
           move      "LF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Riciclo su riga successiva              *
      *                      *-----------------------------------------*
           go to     qry-liv-det-200.
       qry-liv-det-900.
      *              *-------------------------------------------------*
      *              * Aggiornamento progressivi movimenti selezionati *
      *              *-------------------------------------------------*
           if        rf-mgr-snx-mob       not  = "N"
                     go to qry-liv-det-950.
           if        rf-mgr-dar-ave       =    "A"
                     add    rf-mgr-imp-mov
                                          to   w-wrk-prg-ave
           else      add    rf-mgr-imp-mov
                                          to   w-wrk-prg-dar          .
       qry-liv-det-950.
      *              *-------------------------------------------------*
      *              * Memorizzazione per controllo descrizione op.    *
      *              *-------------------------------------------------*
           move      rf-mgt-dat-reg       to   w-dop-dat-reg          .
           move      rf-mgt-num-prt       to   w-dop-num-prt          .
           move      rf-mgt-des-cau       to   w-dop-des-cau          .
           move      rf-mgr-com-rig       to   w-dop-com-rig          .
       qry-liv-det-999.
           exit.

      *    *===========================================================*
      *    * Esecuzione per il livello di dettaglio                    *
      *    *                                                           *
      *    * Subroutine per dettaglio con progressivo saldo            *
      *    *-----------------------------------------------------------*
       qry-liv-det-prg-000.
      *              *-------------------------------------------------*
      *              * Bufferizzazione della prima e dell'ultima data  *
      *              * incontrate                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Fincatura per progressivo                   *
      *                  *---------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      01                   to   v-pos                  .
           move      " |        |                              |        
      -              "    |            |            "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Asterisco se movimento di bilancio          *
      *                  *---------------------------------------------*
           if        rf-mgt-dat-reg       =    w-dop-dat-reg and
                     rf-mgt-num-prt       =    w-dop-num-prt
                     go to qry-liv-det-prg-110.
           if        rf-mgt-snx-mob       =    "N"
                     go to qry-liv-det-prg-110.
      *                  *---------------------------------------------*
      *                  * Visualizzazione                             *
      *                  *---------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "*"                  to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       qry-liv-det-prg-110.
      *                  *---------------------------------------------*
      *                  * Data registrazione                          *
      *                  *---------------------------------------------*
           if        rf-mgt-dat-reg       =    w-dop-dat-reg and
                     rf-mgt-num-prt       =    w-dop-num-prt
                     go to qry-liv-det-prg-120.
           move      "PF"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      v-lnr                to   v-lin                  .
           move      03                   to   v-pos                  .
           move      rf-mgr-dat-reg       to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           go to     qry-liv-det-prg-130.
       qry-liv-det-prg-120.
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      09                   to   v-pos                  .
           move      "--"                 to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       qry-liv-det-prg-130.
      *                  *---------------------------------------------*
      *                  * Concatenazione descrizione causale e riga   *
      *                  * di commento in area di comodo               *
      *                  *---------------------------------------------*
           move      rf-mgt-des-cau       to   w-dca-des-cau          .
           move      rf-mgr-com-rig       to   w-dca-com-rig          .
           perform   dca-con-cat-000      thru dca-con-cat-999        .
      *                  *---------------------------------------------*
      *                  * Prima linea descrizione causale             *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se data registrazione e numero     *
      *                      * protocollo gia' visualizzati            *
      *                      *-----------------------------------------*
           if        rf-mgt-dat-reg       =    w-dop-dat-reg and
                     rf-mgt-num-prt       =    w-dop-num-prt
                     go to qry-liv-det-prg-140.
      *                      *-----------------------------------------*
      *                      * Eventuale concatenazione con altri      *
      *                      * elementi                                *
      *                      *-----------------------------------------*
           perform   dca-con-ael-000      thru dca-con-ael-999        .
      *                      *-----------------------------------------*
      *                      * Visualizzazione                         *
      *                      *-----------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      12                   to   v-pos                  .
           move      w-dca-rig-cau (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * A importo                               *
      *                      *-----------------------------------------*
           go to     qry-liv-det-prg-190.
       qry-liv-det-prg-140.
      *                  *---------------------------------------------*
      *                  * Linea di commento                           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se riga di commento gia' visualiz- *
      *                      * zata                                    *
      *                      *-----------------------------------------*
           if        rf-mgr-com-rig       =    w-dop-com-rig
                     go to qry-liv-det-prg-150.
      *                      *-----------------------------------------*
      *                      * Visualizzazione                         *
      *                      *-----------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      12                   to   v-pos                  .
           move      rf-mgr-com-rig       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * A importo                               *
      *                      *-----------------------------------------*
           go to     qry-liv-det-prg-190.
       qry-liv-det-prg-150.
      *                  *---------------------------------------------*
      *                  * Indicatore di linea che segue               *
      *                  *---------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      12                   to   v-pos                  .
           move      """"""               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       qry-liv-det-prg-190.
      *                      *-----------------------------------------*
      *                      * Mark-Point                              *
      *                      *-----------------------------------------*
           move      "+"                  to   v-edm                  .
           move      rf-mgr-dat-reg       to   w-mpn-dat-reg          .
           move      rf-mgr-num-prt       to   w-mpn-num-prt          .
           move      rf-mgr-num-prg       to   w-mpn-num-prg          .
           move      w-mpn                to   v-cnt                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Importo movimento                           *
      *                  *---------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "G"                  to   v-edm                  .
           move      v-lnr                to   v-lin                  .
           if        rf-mgr-dar-ave       =    "A"
                     move   56            to   v-pos
           else      move   43            to   v-pos                  .
           move      rf-mgr-imp-mov       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Progressivo                                 *
      *                  *---------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "G"                  to   v-edm                  .
           move      v-lnr                to   v-lin                  .
           move      69                   to   v-pos                  .
           move      w-wrk-prg-ass        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Righe descrizione causale residue           *
      *                  *---------------------------------------------*
           if        rf-mgt-dat-reg       =    w-dop-dat-reg and
                     rf-mgt-num-prt       =    w-dop-num-prt
                     go to  qry-liv-det-prg-999.
           move      1                    to   I                      .
       qry-liv-det-prg-200.
           add       1                    to   I                      .
           if        I                    >    5
                     go to  qry-liv-det-prg-999.
           if        w-dca-rig-cau (I)    =    spaces
                     go to  qry-liv-det-prg-200.
      *                      *-----------------------------------------*
      *                      * Test su linee residue                   *
      *                      *-----------------------------------------*
           if        v-res                >    zero
                     go to  qry-liv-det-prg-220.
           perform   int-pag-vid-000      thru int-pag-vid-999        .
           if        v-key                not  = spaces
                     move   "#"           to   OK
                     go to  qry-liv-det-prg-999.
       qry-liv-det-prg-220.
      *                      *-----------------------------------------*
      *                      * Fincatura per progressivo               *
      *                      *-----------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      01                   to   v-pos                  .
           move      " |        |                              |        
      -              "    |            |            "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Riga descrizione causale i-esima        *
      *                      *-----------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      12                   to   v-pos                  .
           move      w-dca-rig-cau (I)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                          *-------------------------------------*
      *                          * Interlinea                          *
      *                          *-------------------------------------*
           move      "LF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Riciclo su riga successiva              *
      *                      *-----------------------------------------*
           go to     qry-liv-det-prg-200.
       qry-liv-det-prg-999.
           exit.

      *    *===========================================================*
      *    * Intestazione pagina video                                 *
      *    *-----------------------------------------------------------*
       int-pag-vid-000.
      *              *-------------------------------------------------*
      *              * Page advance                                    *
      *              *-------------------------------------------------*
           perform   qry-pag-adv-000      thru qry-pag-adv-999        .
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
      *              * Titolo : 'LISTA MOVIMENTI CLIENTE'              *
      *              *-------------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      23                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "LISTA MOVIMENTI CLIENTE"
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
      *              * Codice e descrizione cliente                    *
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
           move      rr-cod-arc           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Composizione stringa                        *
      *                  *---------------------------------------------*
           move      spaces               to   v-alf                  .
           string    "Cliente : "
                                delimited by   size
                     v-edt
                                delimited by   spaces
                     " "
                                delimited by   size
                     rr-cod-arc-rag
                                delimited by   size
                                          into v-alf                  .
      *                  *---------------------------------------------*
      *                  * Visualizzazione                             *
      *                  *---------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      01                   to   v-pos                  .
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
       int-pag-vid-200.
      *              *-------------------------------------------------*
      *              * Linea di fincatura                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in presenza della personalizza-  *
      *                  * zione                                       *
      *                  *---------------------------------------------*
           if        w-prs-snx-prg        =    "S"
                     go to int-pag-vid-400.
       int-pag-vid-300.
      *                  *---------------------------------------------*
      *                  * Fincatura superiore                         *
      *                  *---------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "  Data reg          Descrizione operazione        
      -              "                Dare     Avere"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Sottolineatura                              *
      *                  *---------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "  -------- ---------------------------------------
      -              "- ----------------------------"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Interlinea vuota                            *
      *                  *---------------------------------------------*
           move      "LF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     int-pag-vid-999.
       int-pag-vid-400.
      *                  *---------------------------------------------*
      *                  * Fincatura per progressivo                   *
      *                  *---------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      01                   to   v-pos                  .
           move      " |Data reg|    Descrizione operazione    |    Dare
      -              "    |   Avere    |   Progr.   "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Sottolineatura per progressivo              *
      *                  *---------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "-+--------+------------------------------+--------
      -              "----+------------+------------"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     int-pag-vid-999.
       int-pag-vid-999.
           exit.

      *    *===========================================================*
      *    * Determinazione function-keys previste in Mark-points      *
      *    *-----------------------------------------------------------*
       qry-det-fky-000.
      *              *-------------------------------------------------*
      *              * Function-key 'SLCT'                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se si proviene dal programma di prima- *
      *                  * nota                                        *
      *                  *---------------------------------------------*
           move      "P?"                 to   s-ope                  .
           move      "pcge3000"           to   s-pro                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-liv                =    zero
                     go to  qry-det-fky-100.
      *                  *---------------------------------------------*
      *                  * Se si proviene da primanota                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Estrazione variabile ipc per function   *
      *                      * key 'SLCT'                              *
      *                      *-----------------------------------------*
           move      "GV"                 to   s-ope                  .
           move      "fkselect"           to   s-var                  .
           move      "-"                  to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                not  = spaces or
                     s-alf                not  = "SLCT"
                     move   spaces        to   v-pfk(01)
           else      move   s-alf         to   v-pfk(01)              .
           go to     qry-det-fky-200.
       qry-det-fky-100.
      *                  *---------------------------------------------*
      *                  * Se non si proviene da primanota 'SLCT' sem- *
      *                  * pre ammessa                                 *
      *                  *---------------------------------------------*
           move      "SLCT"               to   v-pfk(01)              .
       qry-det-fky-200.
      *              *-------------------------------------------------*
      *              * Function-key 'INSR'                             *
      *              *-------------------------------------------------*
           move      "INSR"               to   v-pfk(02)              .
      *              *-------------------------------------------------*
      *              * Function-key 'INSR'                             *
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
       qry-det-fky-999.
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
      *              *-------------------------------------------------*
      *              * Function-key 'SLCT'                             *
      *              *-------------------------------------------------*
           if        v-key                not  = "SLCT"
                     go to  z-qry-trt-fun-100.
      *                  *---------------------------------------------*
      *                  * Test se programma di primanota gia' attivo  *
      *                  *---------------------------------------------*
           move      "P?"                 to   s-ope                  .
           move      "pcge3000"           to   s-pro                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-liv                =    zero
                     go to  z-qry-trt-fun-050.
      *                  *---------------------------------------------*
      *                  * Se programma di primanota gia' attivo       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Movimento di v-cnt in area mark-point   *
      *                      *-----------------------------------------*
           move      v-cnt                to   w-mpn                  .
      *                      *-----------------------------------------*
      *                      * Put variabile di ipc:'select datreg'    *
      *                      *-----------------------------------------*
           move      "PV"                 to   s-ope                  .
           move      "select datreg"      to   s-var                  .
           move      "-"                  to   s-dop                  .
           move      "D"                  to   s-tip                  .
           move      w-mpn-dat-reg        to   s-dat                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                      *-----------------------------------------*
      *                      * Put variabile di ipc:'select numprt'    *
      *                      *-----------------------------------------*
           move      "PV"                 to   s-ope                  .
           move      "select numprt"      to   s-var                  .
           move      "-"                  to   s-dop                  .
           move      "N"                  to   s-tip                  .
           move      07                   to   s-car                  .
           move      zero                 to   s-dec                  .
           move      spaces               to   s-sgn                  .
           move      w-mpn-num-prt        to   s-num                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                      *-----------------------------------------*
      *                      * Segnale di avvenuta selezione per pro-  *
      *                      * gramma richieste                        *
      *                      *-----------------------------------------*
           move      "#"                  to   rr-snx-sel             .
      *                      *-----------------------------------------*
      *                      * Segnale di interruzione interrogazione  *
      *                      *-----------------------------------------*
           move      "#"                  to   OK                     .
      *                      *-----------------------------------------*
      *                      * Flag di richieste a No                  *
      *                      *-----------------------------------------*
           move      "N"                  to   w-fun-ric              .
      *                      *-----------------------------------------*
      *                      * Flag di funzionamento ciclico a No      *
      *                      *-----------------------------------------*
           move      "N"                  to   w-fun-cic              .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     z-qry-trt-fun-999.
       z-qry-trt-fun-050.
      *                  *---------------------------------------------*
      *                  * Se programma di primanota non attivo        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Movimento di v-cnt in area mark-point   *
      *                      *-----------------------------------------*
           move      v-cnt                to   w-mpn                  .
      *                      *-----------------------------------------*
      *                      * Put variabile di ipc:'select datreg'    *
      *                      *-----------------------------------------*
           move      "PV"                 to   s-ope                  .
           move      "select datreg"      to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "D"                  to   s-tip                  .
           move      w-mpn-dat-reg        to   s-dat                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                      *-----------------------------------------*
      *                      * Put variabile di ipc:'select numprt'    *
      *                      *-----------------------------------------*
           move      "PV"                 to   s-ope                  .
           move      "select numprt"      to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "N"                  to   s-tip                  .
           move      07                   to   s-car                  .
           move      zero                 to   s-dec                  .
           move      spaces               to   s-sgn                  .
           move      w-mpn-num-prt        to   s-num                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                      *-----------------------------------------*
      *                      * Salvataggio area per 'Start'            *
      *                      *-----------------------------------------*
           move      rf-mgr-tip-arc       to   w-sav-tip-arc          .
           move      rf-mgr-cod-arc       to   w-sav-cod-arc          .
           move      rf-mgr-dat-reg       to   w-sav-dat-reg          .
           move      rf-mgr-num-prt       to   w-sav-num-prt          .
           move      rf-mgr-num-prg       to   w-sav-num-prg          .
      *                      *-----------------------------------------*
      *                      * Richiamo programma di primanota         *
      *                      *-----------------------------------------*
           move      "pgm/cge/prg/obj/pcge3000"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat                                            .
           cancel    s-pat                                            .
           go to     z-qry-trt-fun-900.
       z-qry-trt-fun-100.
      *              *-------------------------------------------------*
      *              * Function-key 'INSR'                             *
      *              *-------------------------------------------------*
           if        v-key                not  = "INSR"
                     go to  z-qry-trt-fun-200.
      *                  *---------------------------------------------*
      *                  * Test se programma di primanota gia' attivo  *
      *                  *---------------------------------------------*
           move      "P?"                 to   s-ope                  .
           move      "pcge3000"           to   s-pro                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-liv                not  = zero
                     go to  z-qry-trt-fun-999.
      *                  *---------------------------------------------*
      *                  * Salvataggio area per 'Start'                *
      *                  *---------------------------------------------*
           move      rf-mgr-tip-arc       to   w-sav-tip-arc          .
           move      rf-mgr-cod-arc       to   w-sav-cod-arc          .
           move      rf-mgr-dat-reg       to   w-sav-dat-reg          .
           move      rf-mgr-num-prt       to   w-sav-num-prt          .
           move      rf-mgr-num-prg       to   w-sav-num-prg          .
      *                  *---------------------------------------------*
      *                  * Richiamo programma di primanota             *
      *                  *---------------------------------------------*
           move      "pgm/cge/prg/obj/pcge3000"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat                                            .
           cancel    s-pat                                            .
           go to     z-qry-trt-fun-900.
       z-qry-trt-fun-200.
      *              *-------------------------------------------------*
      *              * Function-key 'EXPD'                             *
      *              *-------------------------------------------------*
           if        v-key                not  = "EXPD"
                     go to  z-qry-trt-fun-999.
      *              *-------------------------------------------------*
      *              * Salvataggio area per 'Start'                    *
      *              *-------------------------------------------------*
           move      rf-mgr-tip-arc       to   w-sav-tip-arc          .
           move      rf-mgr-cod-arc       to   w-sav-cod-arc          .
           move      rf-mgr-dat-reg       to   w-sav-dat-reg          .
           move      rf-mgr-num-prt       to   w-sav-num-prt          .
           move      rf-mgr-num-prg       to   w-sav-num-prg          .
      *              *-------------------------------------------------*
      *              * Movimento di v-cnt in area mark-point           *
      *              *-------------------------------------------------*
           move      v-cnt                to   w-mpn                  .
      *              *-------------------------------------------------*
      *              * Richiamo routine trattamento function-key 'EXPD'*
      *              *-------------------------------------------------*
           perform   trt-fky-exp-000      thru trt-fky-exp-999        .
       z-qry-trt-fun-900.
      *              *-------------------------------------------------*
      *              * Ripristino 'Start' salvata                      *
      *              *-------------------------------------------------*
           move      w-sav-tip-arc        to   rf-mgr-tip-arc         .
           move      w-sav-cod-arc        to   rf-mgr-cod-arc         .
           move      w-sav-dat-reg        to   rf-mgr-dat-reg         .
           move      w-sav-num-prt        to   rf-mgr-num-prt         .
           move      w-sav-num-prg        to   rf-mgr-num-prg         .
           move      "SK"                 to   f-ope                  .
           move      "EQ"                 to   f-cfr                  .
           move      "ARCDAT"             to   f-key                  .
           move      "pgm/cge/fls/ioc/obj/iofmgr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgr                 .
      *                  *---------------------------------------------*
      *                  * Test su esito operazione                    *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     move   "#"           to   OK
                     go to  z-qry-trt-fun-999.
      *              *-------------------------------------------------*
      *              * Lettura record a cui si era arrivati nell'in-   *
      *              * terrogazione                                    *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmgr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgr                 .
      *                  *---------------------------------------------*
      *                  * Test su esito operazione                    *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     move   "#"           to   OK
                     go to  z-qry-trt-fun-999.
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
       z-qry-trt-fun-999.
           exit.

      *    *===========================================================*
      *    * Trattamento function-key 'EXPD'                           *
      *    *-----------------------------------------------------------*
       trt-fky-exp-000.
      *              *-------------------------------------------------*
      *              * Sottoprogramma espansione riga registrazione    *
      *              *-------------------------------------------------*
           call      "pgm/cge/prg/obj/pcge301x"
                                         using w-mpn                  .
           cancel    "pgm/cge/prg/obj/pcge301x"                       .
       trt-fky-exp-999.
           exit.

      *    *===========================================================*
      *    * Concatenazione descrizione causale e commento in riga e   *
      *    * determinazione numero righe effettive di descrizione      *
      *    *-----------------------------------------------------------*
       dca-con-cat-000.
           move      4                    to   w-dca-num-rig          .
       dca-con-cat-500.
           if        w-dca-rig-cau
                    (w-dca-num-rig)       =    spaces
                     subtract  1          from w-dca-num-rig
                     if        w-dca-num-rig
                                          >    zero
                               go to dca-con-cat-500.
           if        w-dca-com-rig        not  = spaces
                     add   1              to   w-dca-num-rig
                     move  w-dca-com-rig  to   w-dca-rig-cau
                                              (w-dca-num-rig)         .
       dca-con-cat-999.
           exit.

      *    *===========================================================*
      *    * Concatenazione descrizione causale con eventuali altri    *
      *    * elementi                                                  *
      *    *-----------------------------------------------------------*
       dca-con-ael-000.
      *              *-------------------------------------------------*
      *              * Test preliminare se a spazi                     *
      *              *-------------------------------------------------*
           if        rf-mgt-des-cau       =    spaces
                     go to dca-con-ael-900.
      *              *-------------------------------------------------*
      *              * Test sul tipo di movimento Iva                  *
      *              *                                                 *
      *              * Solo Dare / Avere Cliente italia e estero CEE   *
      *              *-------------------------------------------------*
           if        rf-mgt-tip-iva       not  = "1"   and
                     rf-mgt-tip-iva       not  = "2"   and
                     rf-mgt-tip-iva       not  = "A"   and
                     rf-mgt-tip-iva       not  = "B"
                     go to dca-con-ael-900.
      *              *-------------------------------------------------*
      *              * Test se presenti altri elementi                 *
      *              *-------------------------------------------------*
           if        rf-mgt-num-doc       =    spaces
                     go to dca-con-ael-900.
      *              *-------------------------------------------------*
      *              * Test se la descrizione della causale e' uguale  *
      *              * a quella contenuta in tabella                   *
      *              *-------------------------------------------------*
           if        rf-mgt-cod-cau       =    zero
                     go to dca-con-ael-900.
       dca-con-ael-200.
      *              *-------------------------------------------------*
      *              * Lettura [zcc]                                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione [zcc]                       *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofzcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zcc                 .
      *                  *---------------------------------------------*
      *                  * Lettura per codice                          *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCAU    "         to   f-key                  .
           move      rf-mgt-cod-cau       to   rf-zcc-cod-cau         .
           move      "pgm/cge/fls/ioc/obj/iofzcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zcc                 .
      *                  *---------------------------------------------*
      *                  * Test su esito operazione                    *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to dca-con-ael-900.
       dca-con-ael-300.
      *              *-------------------------------------------------*
      *              * Confronto                                       *
      *              *-------------------------------------------------*
           if        rf-zcc-des-cau       not  = rf-mgt-des-cau
                     go to dca-con-ael-900.
       dca-con-ael-400.
      *              *-------------------------------------------------*
      *              * Concatenazione con numero documento             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Editing data documento                      *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      rf-mgt-dat-doc       to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Assemblaggio                                *
      *                  *---------------------------------------------*
           move      40                   to   w-all-str-lun          .
           move      05                   to   w-all-str-num          .
           move      rf-mgt-des-cau       to   w-all-str-cat (1)      .
           move      "[Nr."               to   w-all-str-cat (2)      .
           move      rf-mgt-num-doc       to   w-all-str-cat (3)      .
           move      "del"                to   w-all-str-cat (4)      .
           move      v-edt                to   w-all-str-cat (5)      .
           perform   all-str-csb-000      thru all-str-csb-999        .
      *
           move      40                   to   w-all-str-lun          .
           move      02                   to   w-all-str-num          .
           move      w-all-str-alf        to   w-all-str-cat (1)      .
           move      "]"                  to   w-all-str-cat (2)      .
           perform   all-str-cat-000      thru all-str-cat-999        .
      *                  *---------------------------------------------*
      *                  * In campo di destinazione                    *
      *                  *---------------------------------------------*
           move      w-all-str-alf        to   w-dca-rig-cau (1)      .
       dca-con-ael-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     dca-con-ael-999.
       dca-con-ael-999.
           exit.

      *    *===========================================================*
      *    * Subroutines per allineamenti a destra o a sinistra oppure *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cps"                   .


