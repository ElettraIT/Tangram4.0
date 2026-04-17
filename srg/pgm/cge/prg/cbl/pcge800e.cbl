       Identification Division.
       Program-Id.                                 pcge8000           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    cge                 *
      *                                Settore:    bil                 *
      *                                   Fase:    cge800              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 24/11/04    *
      *                       Ultima revisione:    NdK del 22/11/21    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Chiusura fase di bilancio annuale           *
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
      *    * Descrizione tipo interrogazione per l'overlay             *
      *    *-----------------------------------------------------------*
       01  w-des-tit-pgm-ovy              pic  x(40)       value
                     "   CHIUSURA FASE DI BILANCIO ANNUALE    "       .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "mvideo" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/v"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "msegrt" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/s"                                  .

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

      *    *===========================================================*
      *    * Work-area di controllo                                    *
      *    *-----------------------------------------------------------*
       01  w-cnt.
      *        *-------------------------------------------------------*
      *        * Flags di controllo su modalita' di funzionamento      *
      *        *-------------------------------------------------------*
           05  w-cnt-mfu.
      *            *---------------------------------------------------*
      *            * Visualizzazione forzata da segreteria             *
      *            *---------------------------------------------------*
               10  w-cnt-mfu-vis-sgr      pic  x(01)                  .

      *    *===========================================================*
      *    * Record files                                              *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [mgs]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfmgs"                          .
      *        *-------------------------------------------------------*
      *        * [pdc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfpdc"                          .
      *        *-------------------------------------------------------*
      *        * [cli]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfcli"                          .
      *        *-------------------------------------------------------*
      *        * [fnt]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rffnt"                          .

      *    *===========================================================*
      *    * Records numerazioni                                       *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [datbil]                                              *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/num/rec/rndatbil"                       .

      *    *===========================================================*
      *    * Work-area referenze                                       *
      *    *-----------------------------------------------------------*
       01  w-ref.
      *        *-------------------------------------------------------*
      *        * Causali e sottoconti per bilancio                     *
      *        *-------------------------------------------------------*
           05  w-ref-cau-stc.
      *            *---------------------------------------------------*
      *            * Causale per 'Bilancio di chiusura'                *
      *            *---------------------------------------------------*
               10  w-ref-cau-chi          pic  9(03)                  .
               10  filler                 pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Causale per 'Bilancio di apertura'                *
      *            *---------------------------------------------------*
               10  w-ref-cau-ape          pic  9(03)                  .
               10  filler                 pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Sottoconto 'Utile d'esercizio'                    *
      *            *---------------------------------------------------*
               10  w-ref-stc-ute          pic  9(07)                  .
               10  filler                 pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Sottoconto 'Perdita d'esercizio'                  *
      *            *---------------------------------------------------*
               10  w-ref-stc-pee          pic  9(07)                  .

      *    *===========================================================*
      *    * Work-area generica                                        *
      *    *-----------------------------------------------------------*
       01  w-wrk.
      *        *-------------------------------------------------------*
      *        * Data attuale                                          *
      *        *-------------------------------------------------------*
           05  w-wrk-dat-att              pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Saldo di bilancio                                     *
      *        *-------------------------------------------------------*
           05  w-wrk-sdo-bil              pic s9(13)                  .
      *        *-------------------------------------------------------*
      *        * Utile o perdita dell'esercizio                        *
      *        *-------------------------------------------------------*
           05  w-wrk-uop-ese              pic s9(13)                  .
      *        *-------------------------------------------------------*
      *        * Flag di elaborazione vera e propria                   *
      *        *-------------------------------------------------------*
           05  w-wrk-flg-ela              pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Salvataggio ultima chiave file [mgs]                  *
      *        *-------------------------------------------------------*
           05  w-sav-mgs.
      *            *---------------------------------------------------*
      *            * Tipo record                                       *
      *            *---------------------------------------------------*
               10  w-sav-mgs-tip          pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Codice conto                                      *
      *            *---------------------------------------------------*
               10  w-sav-mgs-cod          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Numero records trattati di [mgs]                      *
      *        *-------------------------------------------------------*
           05  w-num-rec-tra              pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Numero errori riscontrati                             *
      *        *-------------------------------------------------------*
           05  w-num-err-ris              pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Area per messaggio di errore                          *
      *        *-------------------------------------------------------*
           05  w-err-msg-err              pic  x(80)                  .
      *        *-------------------------------------------------------*
      *        * Contatore di comodo 'I'                               *
      *        *-------------------------------------------------------*
           05  I                          pic  9(03)                  .

      *    *===========================================================*
      *    * Work per subroutines di editing codice sottoconto         *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/wedtpdc0.wkl"                   .

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Link-area comune per programmi della serie pcge4500       *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/pcge8000.pgl"                   .

      ******************************************************************
       Procedure Division                using i-ide
                                               w-ovy-exe
                                               w-tmn
                                               w-spg
                                               w-prs                  .
      ******************************************************************

      *================================================================*
      *       Main                                                     *
      *================================================================*
       main-000.
      *              *-------------------------------------------------*
      *              * Salvataggio immagine video                      *
      *              *-------------------------------------------------*
           move      "SV"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Visualizzazione titolo programma                *
      *              *-------------------------------------------------*
           perform   vis-tit-pgm-000      thru vis-tit-pgm-999        .
      *              *-------------------------------------------------*
      *              * Esecuzione routine pre-esecuzione programma     *
      *              *-------------------------------------------------*
           move      spaces               to   OK                     .
           perform   pre-exe-pgm-000      thru pre-exe-pgm-999        .
           if        OK                   not  = spaces
                     go to main-900.
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
           perform   ric-opn-fls-000      thru ric-opn-fls-999        .
      *              *-------------------------------------------------*
      *              * Normalizzazione record richieste                *
      *              *-------------------------------------------------*
           perform   nor-ric-sel-000      thru nor-ric-sel-999        .
      *              *-------------------------------------------------*
      *              * Visualizzazione prompts richieste               *
      *              *-------------------------------------------------*
           perform   pmt-ric-sel-000      thru pmt-ric-sel-999        .
      *              *-------------------------------------------------*
      *              * Inserimento valori richieste                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Accettazione record richieste               *
      *                  *---------------------------------------------*
           perform   acc-ric-sel-000      thru acc-ric-sel-999        .
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
           perform   reg-ric-sel-000      thru reg-ric-sel-999        .
      *                          *-------------------------------------*
      *                          * Esecuzione programma in foreground  *
      *                          *-------------------------------------*
           perform   exe-pgm-frg-000      thru exe-pgm-frg-999        .
       main-800.
      *              *-------------------------------------------------*
      *              * Close files                                     *
      *              *-------------------------------------------------*
           perform   ric-cls-fls-000      thru ric-cls-fls-999        .
       main-900.
      *              *-------------------------------------------------*
      *              * Ripristino immagine video                       *
      *              *-------------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
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
      *              *-------------------------------------------------*
      *              * Flag di eventuale visualizzazione forzata       *
      *              *-------------------------------------------------*
           move      s-sts                to   w-cnt-mfu-vis-sgr      .
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
           move      w-des-tit-pgm-ovy    to   v-alf                  .
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
      *    * Open files                                                *
      *    *-----------------------------------------------------------*
       ric-opn-fls-000.
       ric-opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Close files                                               *
      *    *-----------------------------------------------------------*
       ric-cls-fls-000.
       ric-cls-fls-999.
           exit.

      *    *===========================================================*
      *    * Routine pre-esecuzione programma                          *
      *    *-----------------------------------------------------------*
       pre-exe-pgm-000.
      *              *-------------------------------------------------*
      *              * Test se programma eseguibile                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su flag di eventuale visualizzazione   *
      *                  *---------------------------------------------*
           if        w-cnt-mfu-vis-sgr    not  = "V"
                     go to pre-exe-pgm-002.
      *                  *---------------------------------------------*
      *                  * Messaggio di errore                         *
      *                  *---------------------------------------------*
           move      "ME"                 to   v-ope                  .
           move      "Programma non eseguibile dall'utente !            
      -              "        "           to   v-not                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Flag di uscita ad errore                    *
      *                  *---------------------------------------------*
           move      "#"                  to   OK                     .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     pre-exe-pgm-999.
       pre-exe-pgm-002.
      *              *-------------------------------------------------*
      *              * Lettura dati relativi alla chiusura bilanci     *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/num/ioc/obj/indatbil"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-dat-bil             .
      *
           move      "NO"                 to   f-ope                  .
           move      "pgm/cge/num/ioc/obj/indatbil"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-dat-bil             .
      *
           move      "RD"                 to   f-ope                  .
           move      "pgm/cge/num/ioc/obj/indatbil"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-dat-bil             .
      *
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/num/ioc/obj/indatbil"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-dat-bil             .
      *              *-------------------------------------------------*
      *              * Determinazione data attuale                     *
      *              *-------------------------------------------------*
           move      "DT"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-dat                to   w-wrk-dat-att          .
      *              *-------------------------------------------------*
      *              * Test che la fase di bilancio sia in progresso   *
      *              *-------------------------------------------------*
           if        rn-dat-bil-flg-bil   not  = spaces
                     go to pre-exe-pgm-100.
      *                  *---------------------------------------------*
      *                  * Se no : messaggio di errore e uscita        *
      *                  *---------------------------------------------*
           move      "ME"                 to   v-ope                  .
           move      "Non e' stato dichiarato l'inizio fase di bilancio 
      -              "annuale."           to   v-nt1                  .
           move      "Pertanto questo programma non puo' essere eseguito
      -              " !"                 to   v-nt2                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "#"                  to   OK                     .
           go to     pre-exe-pgm-999.
       pre-exe-pgm-100.
      *              *-------------------------------------------------*
      *              * Test che la fase di generazione sia eseguita    *
      *              *-------------------------------------------------*
           if        rn-dat-bil-flg-bil   =    "G"
                     go to pre-exe-pgm-150.
      *                  *---------------------------------------------*
      *                  * Se no : messaggio di avviso                 *
      *                  *---------------------------------------------*
           move      "ME"                 to   v-ope                  .
           move      "Attenzione : non e' ancora stata eseguita la fase 
      -              "di generazione"     to   v-nt1                  .
           move      "automatica dei movimenti di bilancio !            
      -              "              "     to   v-nt2                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Si prosegue                                 *
      *                  *---------------------------------------------*
           go to     pre-exe-pgm-150.
       pre-exe-pgm-150.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazioni relative alla chiusura*
      *              * bilanci                                         *
      *              *-------------------------------------------------*
           move      "R:"                 to   s-ope                  .
           move      "pgm/cge/bil[cau-stc]"
                                          to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Test su esito operazione                    *
      *                  *---------------------------------------------*
           if        s-ves                =    spaces
                     move  s-alf          to   w-ref-cau-stc
                     go to pre-exe-pgm-200.
      *                      *-----------------------------------------*
      *                      * Se errata : messaggio                   *
      *                      *-----------------------------------------*
           move      "ME"                 to   v-ope                  .
           move      "Mancano le referenze relative alla chiusura bilanc
      -              "i."                 to   v-nt1                  .
           move      "Il programma pertanto non puo' essere eseguito !"
                                          to   v-nt2                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "#"                  to   OK                     .
           go to     pre-exe-pgm-999.
       pre-exe-pgm-200.
      *                  *---------------------------------------------*
      *                  * Controllo valori letti                      *
      *                  *---------------------------------------------*
       pre-exe-pgm-300.
      *                      *-----------------------------------------*
      *                      * Sottoconto utile d'esercizio            *
      *                      *-----------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofpdc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdc                 .
      *
           move      "RK"                 to   f-ope                  .
           move      "CODSTC"             to   f-key                  .
           move      w-ref-stc-ute        to   rf-pdc-cod-pdc         .
           move      "pgm/cge/fls/ioc/obj/iofpdc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdc                 .
           if        f-sts                =    e-not-err
                     go to  pre-exe-pgm-320.
      *
           move      "ME"                 to   v-ope                  .
           move      "Sottoconto utile d'esercizio non esistente in arch
      -              "ivio."              to   v-nt1                  .
           move      "Il programma pertanto non puo' essere eseguito !"
                                          to   v-nt2                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "#"                  to   OK                     .
           go to     pre-exe-pgm-900.
       pre-exe-pgm-320.
           if        rf-pdc-tip-cnt   =    "P"
                     go to  pre-exe-pgm-340.
           move      "ME"                 to   v-ope                  .
           move      "Sottoconto utile d'esercizio non codificato di tip
      -              "o 'Patrimoniale'."
                                          to   v-nt1                  .
           move      "Il programma pertanto non puo' essere eseguito !"
                                          to   v-nt2                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "#"                  to   OK                     .
           go to     pre-exe-pgm-900.
       pre-exe-pgm-340.
      *                      *-----------------------------------------*
      *                      * Sottoconto perdita d'esercizio          *
      *                      *-----------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODSTC"             to   f-key                  .
           move      w-ref-stc-pee        to   rf-pdc-cod-pdc         .
           move      "pgm/cge/fls/ioc/obj/iofpdc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdc                 .
           if        f-sts                =    e-not-err
                     go to  pre-exe-pgm-360.
           move      "ME"                 to   v-ope                  .
           move      "Sottoconto perdita d'esercizio non esistente in ar
      -              "chivio."            to   v-nt1                  .
           move      "Il programma pertanto non puo' essere eseguito !"
                                          to   v-nt2                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "#"                  to   OK                     .
           go to     pre-exe-pgm-900.
       pre-exe-pgm-360.
           if        rf-pdc-tip-cnt   =    "P"
                     go to  pre-exe-pgm-900.
           move      "ME"                 to   v-ope                  .
           move      "Sottoconto perdita d'esercizio non codificato di t
      -              "ipo 'Patrimoniale'."
                                          to   v-nt1                  .
           move      "Il programma pertanto non puo' essere eseguito !"
                                          to   v-nt2                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "#"                  to   OK                     .
       pre-exe-pgm-900.
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofpdc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdc                 .
       pre-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts richieste                         *
      *    *-----------------------------------------------------------*
       pmt-ric-sel-000.
      *              *-------------------------------------------------*
      *              * Literal                                         *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      34                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Chiusura bilancio per l'esercizio "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Anno di esercizio                               *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      12                   to   v-lin                  .
           move      36                   to   v-pos                  .
           move      rn-dat-bil-ese-bil   to   v-num                  .
           add       1900                 to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione record richieste                          *
      *    *-----------------------------------------------------------*
       nor-ric-sel-000.
       nor-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Accettazione record richieste                             *
      *    *-----------------------------------------------------------*
       acc-ric-sel-000.
      *              *-------------------------------------------------*
      *              * Conferma esecuzione del programma               *
      *              *-------------------------------------------------*
           move      "MX"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      "Conferma esecuzione del programma (S/N) ?"
                                          to   v-not                  .
           move      spaces               to   v-alf                  .
           move      "SN"                 to   v-msk                  .
           move      "DO  "               to   v-pfk (1)              .
           move      "EXIT"               to   v-pfk (3)              .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Normalizzazione risposta dell'utente            *
      *              *-------------------------------------------------*
           if        v-key              =    spaces
                     if      v-alf          =    "S"
                             move  "DO  " to   v-key
                             go to acc-ric-sel-999
                     else    move  "EXIT" to   v-key
                             go to acc-ric-sel-999.
       acc-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Regolarizzazione record richieste                         *
      *    *-----------------------------------------------------------*
       reg-ric-sel-000.
       reg-ric-sel-999.
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
      *              * Messaggio di programma in esecuzione            *
      *              *-------------------------------------------------*
           move      "PE"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Esecuzione programma vero e proprio             *
      *              *-------------------------------------------------*
           perform   chi-fas-bil-000      thru chi-fas-bil-999        .
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
      *    * Chiusura fase bilanci                                     *
      *    *-----------------------------------------------------------*
       chi-fas-bil-000.
      *              *-------------------------------------------------*
      *              * Apertura file [mgs]                             *
      *              *-------------------------------------------------*
           perform   opn-fil-con-000      thru opn-fil-con-999        .
      *              *-------------------------------------------------*
      *              * Apertura file [pdc]                             *
      *              *-------------------------------------------------*
           perform   opn-fil-pdc-000      thru opn-fil-pdc-999        .
      *              *-------------------------------------------------*
      *              * Apertura file [cli]                             *
      *              *-------------------------------------------------*
           perform   opn-fil-cli-000      thru opn-fil-cli-999        .
      *              *-------------------------------------------------*
      *              * Apertura file [fnt]                             *
      *              *-------------------------------------------------*
           perform   opn-fil-fnt-000      thru opn-fil-fnt-999        .
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di elaborazione            *
      *              *-------------------------------------------------*
           move      spaces               to   w-wrk-flg-ela          .
       chi-fas-bil-050.
      *              *-------------------------------------------------*
      *              * Normalizzazioni iniziali                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ultima chiave letta                         *
      *                  *---------------------------------------------*
           move      spaces               to   w-sav-mgs-tip          .
           move      zero                 to   w-sav-mgs-cod          .
      *                  *---------------------------------------------*
      *                  * Numero records trattati                     *
      *                  *---------------------------------------------*
           move      zero                 to   w-num-rec-tra          .
      *                  *---------------------------------------------*
      *                  * Numero errori riscontrati                   *
      *                  *---------------------------------------------*
           move      zero                 to   w-num-err-ris          .
      *                  *---------------------------------------------*
      *                  * Utile o perdita d'esercizio                 *
      *                  *---------------------------------------------*
           move      zero                 to   w-wrk-uop-ese          .
       chi-fas-bil-100.
      *              *-------------------------------------------------*
      *              * Start su file [mgs]                             *
      *              *-------------------------------------------------*
           perform   str-fil-con-000      thru str-fil-con-999        .
      *                  *---------------------------------------------*
      *                  * Test se errore di start                     *
      *                  *---------------------------------------------*
           if        OK                   not  = spaces
                     move  spaces         to   OK
                     go to chi-fas-bil-300.
       chi-fas-bil-200.
      *              *-------------------------------------------------*
      *              * Lettura sequenziale file [mgs]                  *
      *              *-------------------------------------------------*
           perform   nxt-fil-con-000      thru nxt-fil-con-999        .
      *                  *---------------------------------------------*
      *                  * Test se at end                              *
      *                  *---------------------------------------------*
           if        OK                   not  = spaces
                     move  spaces         to   OK
                     go to chi-fas-bil-300.
      *              *-------------------------------------------------*
      *              * Test sul max su file [mgs]                      *
      *              *-------------------------------------------------*
           perform   max-fil-con-000      thru max-fil-con-999        .
      *                  *---------------------------------------------*
      *                  * Test se oltre il max                        *
      *                  *---------------------------------------------*
           if        OK                   not  = spaces
                     move  spaces         to   OK
                     go to chi-fas-bil-300.
      *              *-------------------------------------------------*
      *              * Memorizzazione ultima chiave letta da [mgs]     *
      *              *-------------------------------------------------*
           perform   mem-key-con-000      thru mem-key-con-999        .
      *              *-------------------------------------------------*
      *              * Selezione record letto da [mgs]                 *
      *              *-------------------------------------------------*
           perform   sel-fil-con-000      thru sel-fil-con-999        .
      *                  *---------------------------------------------*
      *                  * Test se record da ignorare                  *
      *                  *---------------------------------------------*
           if        OK                   not  = spaces
                     move  spaces         to   OK
                     go to chi-fas-bil-200.
      *              *-------------------------------------------------*
      *              * Incremento numero records trattati              *
      *              *-------------------------------------------------*
           add       1                    to   w-num-rec-tra          .
      *              *-------------------------------------------------*
      *              * Riporto saldo su esercizio successivo           *
      *              *-------------------------------------------------*
           perform   rip-sdo-esu-000      thru rip-sdo-esu-999        .
      *              *-------------------------------------------------*
      *              * Riciclo su prossimo conto                       *
      *              *-------------------------------------------------*
           go to     chi-fas-bil-100.
       chi-fas-bil-300.
      *              *-------------------------------------------------*
      *              * Test se numero records trattati : zero          *
      *              *-------------------------------------------------*
           if        w-num-rec-tra        =    zero
                     go to  chi-fas-bil-800.
      *              *-------------------------------------------------*
      *              * Se nessun errore si puo' procedere all'elabora- *
      *              * zione vera e propria                            *
      *              *-------------------------------------------------*
           if        w-num-err-ris        =    zero and
                     w-wrk-flg-ela        =    spaces
                     move   "#"           to   w-wrk-flg-ela
                     go to  chi-fas-bil-050
           else      go to  chi-fas-bil-900.
       chi-fas-bil-800.
      *              *-------------------------------------------------*
      *              * Messaggio per nessun record trattato            *
      *              *-------------------------------------------------*
           move      "WR"                 to   m-ope                  .
           move      "Non esiste alcuna scheda contabile in archivio; l'
      -              "elaborazione pertanto non puo'"
                                          to   m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
           move      "WR"                 to   m-ope                  .
           move      "essere eseguita."
                                          to   m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
       chi-fas-bil-900.
      *              *-------------------------------------------------*
      *              * Se nessun errore ed elaborazione eseguita si    *
      *              * procede, altrimenti a uscita                    *
      *              *-------------------------------------------------*
           if        w-num-err-ris        =    zero and
                     w-wrk-flg-ela        not  = spaces
                     go to  chi-fas-bil-925
           else      go to  chi-fas-bil-950.
       chi-fas-bil-925.
      *              *-------------------------------------------------*
      *              * Aggiustamento saldo a inizio anno successivo    *
      *              * dei sottoconti utile o perdita d'esercizio      *
      *              *-------------------------------------------------*
           perform   ags-sdo-uop-000      thru ags-sdo-uop-999        .
      *              *-------------------------------------------------*
      *              * Aggiornamento record numerazione                *
      *              *-------------------------------------------------*
           perform   agg-rec-num-000      thru agg-rec-num-999        .
       chi-fas-bil-950.
      *              *-------------------------------------------------*
      *              * Chiusura file [mgs]                             *
      *              *-------------------------------------------------*
           perform   cls-fil-con-000      thru cls-fil-con-999        .
      *              *-------------------------------------------------*
      *              * Chiusura file [pdc]                             *
      *              *-------------------------------------------------*
           perform   cls-fil-pdc-000      thru cls-fil-pdc-999        .
      *              *-------------------------------------------------*
      *              * Chiusura file [cli]                             *
      *              *-------------------------------------------------*
           perform   cls-fil-cli-000      thru cls-fil-cli-999        .
      *              *-------------------------------------------------*
      *              * Chiusura file [fnt]                             *
      *              *-------------------------------------------------*
           perform   cls-fil-fnt-000      thru cls-fil-fnt-999        .
       chi-fas-bil-999.
           exit.

      *    *===========================================================*
      *    * Apertura file [mgs]                                       *
      *    *-----------------------------------------------------------*
       opn-fil-con-000.
      *              *-------------------------------------------------*
      *              * Open                                            *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmgs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgs                 .
       opn-fil-con-999.
           exit.

      *    *===========================================================*
      *    * Chiusura file [mgs]                                       *
      *    *-----------------------------------------------------------*
       cls-fil-con-000.
      *              *-------------------------------------------------*
      *              * Close                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmgs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgs                 .
       cls-fil-con-999.
           exit.

      *    *===========================================================*
      *    * Apertura file [pdc]                                       *
      *    *-----------------------------------------------------------*
       opn-fil-pdc-000.
      *              *-------------------------------------------------*
      *              * Open                                            *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofpdc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdc                 .
       opn-fil-pdc-999.
           exit.

      *    *===========================================================*
      *    * Chiusura file [pdc]                                       *
      *    *-----------------------------------------------------------*
       cls-fil-pdc-000.
      *              *-------------------------------------------------*
      *              * Close                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofpdc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdc                 .
       cls-fil-pdc-999.
           exit.

      *    *===========================================================*
      *    * Apertura file [cli]                                       *
      *    *-----------------------------------------------------------*
       opn-fil-cli-000.
      *              *-------------------------------------------------*
      *              * Open                                            *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofcli"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cli                 .
       opn-fil-cli-999.
           exit.

      *    *===========================================================*
      *    * Chiusura file [cli]                                       *
      *    *-----------------------------------------------------------*
       cls-fil-cli-000.
      *              *-------------------------------------------------*
      *              * Close                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofcli"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cli                 .
       cls-fil-cli-999.
           exit.

      *    *===========================================================*
      *    * Apertura file [fnt]                                       *
      *    *-----------------------------------------------------------*
       opn-fil-fnt-000.
      *              *-------------------------------------------------*
      *              * Open                                            *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/ioffnt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fnt                 .
       opn-fil-fnt-999.
           exit.

      *    *===========================================================*
      *    * Chiusura file [fnt]                                       *
      *    *-----------------------------------------------------------*
       cls-fil-fnt-000.
      *              *-------------------------------------------------*
      *              * Close                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/ioffnt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fnt                 .
       cls-fil-fnt-999.
           exit.

      *    *===========================================================*
      *    * Memorizzazione ultima chiave letta da [mgs]               *
      *    *-----------------------------------------------------------*
       mem-key-con-000.
      *              *-------------------------------------------------*
      *              * Ultima chiave letta                             *
      *              *-------------------------------------------------*
           move      rf-mgs-tip-rec       to   w-sav-mgs-tip          .
           move      rf-mgs-cod-con       to   w-sav-mgs-cod          .
       mem-key-con-999.
           exit.

      *    *===========================================================*
      *    * Start su file [mgs]                                       *
      *    *-----------------------------------------------------------*
       str-fil-con-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   OK                     .
      *              *-------------------------------------------------*
      *              * Start                                           *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "ESECOD    "         to   f-key                  .
           move      "GT"                 to   f-cfr                  .
           move      rn-dat-bil-ese-bil   to   rf-mgs-ann-ese         .
           move      w-sav-mgs-tip        to   rf-mgs-tip-rec         .
           move      w-sav-mgs-cod        to   rf-mgs-cod-con         .
           move      "pgm/cge/fls/ioc/obj/iofmgs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgs                 .
      *              *-------------------------------------------------*
      *              * Test se errore di start                         *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     move  "#"            to   OK                     .
       str-fil-con-999.
           exit.

      *    *===========================================================*
      *    * Lettura sequenziale file [mgs]                            *
      *    *-----------------------------------------------------------*
       nxt-fil-con-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   OK                     .
      *              *-------------------------------------------------*
      *              * Start                                           *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmgs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgs                 .
      *              *-------------------------------------------------*
      *              * Test se at end                                  *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     move  "#"            to   OK                     .
       nxt-fil-con-999.
           exit.

      *    *===========================================================*
      *    * Test max su file [mgs]                                    *
      *    *-----------------------------------------------------------*
       max-fil-con-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   OK                     .
      *              *-------------------------------------------------*
      *              * Test se oltre il max                            *
      *              *-------------------------------------------------*
           if        rf-mgs-ann-ese       not  = rn-dat-bil-ese-bil
                     move  "#"            to   OK                     .
       max-fil-con-999.
           exit.

      *    *===========================================================*
      *    * Selezione record letto da [mgs]                           *
      *    *-----------------------------------------------------------*
       sel-fil-con-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   OK                     .
      *              *-------------------------------------------------*
      *              * Selezione su tipo record                        *
      *              *-------------------------------------------------*
           if        rf-mgs-tip-rec       not  = "G" and
                     rf-mgs-tip-rec       not  = "C" and
                     rf-mgs-tip-rec       not  = "F"
                     move  "#"            to   OK                     .
       sel-fil-con-999.
           exit.

      *    *===========================================================*
      *    * Riporto saldo su esercizio successivo                     *
      *    *-----------------------------------------------------------*
       rip-sdo-esu-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   OK                     .
      *              *-------------------------------------------------*
      *              * Test su tipo conto                              *
      *              *-------------------------------------------------*
           if        rf-mgs-tip-rec       =    "G"
                     go to rip-sdo-esu-100
           else if   rf-mgs-tip-rec       =    "C"
                     go to rip-sdo-esu-200
           else      go to rip-sdo-esu-300.
       rip-sdo-esu-100.
      *              *-------------------------------------------------*
      *              * Se conto di tipo 'G'                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura file [pdc]                          *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODSTC    "         to   f-key                  .
           move      rf-mgs-cod-con       to   rf-pdc-cod-pdc         .
           move      "pgm/cge/fls/ioc/obj/iofpdc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdc                 .
      *                  *---------------------------------------------*
      *                  * Test se errore di lettura                   *
      *                  *---------------------------------------------*
           if        f-sts                =    e-not-err
                     go to rip-sdo-esu-400.
      *                  *---------------------------------------------*
      *                  * Incremento numero errori riscontrati        *
      *                  *---------------------------------------------*
           add       1                    to   w-num-err-ris          .
      *                  *---------------------------------------------*
      *                  * Emissione messaggio di errore               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Editing codice sottoconto con appoggio  *
      *                      * a sinistra                              *
      *                      *-----------------------------------------*
           if        rf-mgs-cod-con       >    99999
                     move   3             to   w-edt-cod-pdc-liv
           else      move   2             to   w-edt-cod-pdc-liv      .
           move      rf-mgs-cod-con       to   w-edt-cod-pdc-cod      .
           move      spaces               to   w-edt-cod-pdc-edm      .
           perform   edt-pdc-asx-000      thru edt-pdc-asx-999        .
      *                      *-----------------------------------------*
      *                      * Composizione messaggio completo         *
      *                      *-----------------------------------------*
           move      spaces               to   w-err-msg-err          .
           string    "Non esiste l'anagrafica per il sottoconto "
                                delimited by   size
                     w-edt-cod-pdc-edt
                                delimited by   spaces
                                          into w-err-msg-err          .
      *                      *-----------------------------------------*
      *                      * Output messaggio                        *
      *                      *-----------------------------------------*
           perform   out-msg-err-000      thru out-msg-err-999        .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     rip-sdo-esu-999.
       rip-sdo-esu-200.
      *              *-------------------------------------------------*
      *              * Se conto di tipo 'C'                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura file [cli]                          *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCLI    "         to   f-key                  .
           move      rf-mgs-cod-con       to   rf-cli-cod-cli         .
           move      "pgm/cge/fls/ioc/obj/iofcli"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cli                 .
      *                  *---------------------------------------------*
      *                  * Test se errore di lettura                   *
      *                  *---------------------------------------------*
           if        f-sts                =    e-not-err
                     go to rip-sdo-esu-400.
      *                  *---------------------------------------------*
      *                  * Incremento numero errori riscontrati        *
      *                  *---------------------------------------------*
           add       1                    to   w-num-err-ris          .
      *                  *---------------------------------------------*
      *                  * Emissione messaggio di errore               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Editing codice cliente                  *
      *                      *-----------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      7                    to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<"                  to   v-edm                  .
           move      rf-mgs-cod-con       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Composizione messaggio completo         *
      *                      *-----------------------------------------*
           move      spaces               to   w-err-msg-err          .
           string    "Non esiste l'anagrafica per il cliente "
                                delimited by   size
                     v-edt      delimited by   spaces
                                          into w-err-msg-err          .
      *                      *-----------------------------------------*
      *                      * Output messaggio                        *
      *                      *-----------------------------------------*
           perform   out-msg-err-000      thru out-msg-err-999        .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     rip-sdo-esu-999.
       rip-sdo-esu-300.
      *              *-------------------------------------------------*
      *              * Se conto di tipo 'F'                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura file [fnt]                          *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODFNT    "         to   f-key                  .
           move      rf-mgs-cod-con       to   rf-fnt-cod-fnt         .
           move      "pgm/cge/fls/ioc/obj/ioffnt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fnt                 .
      *                  *---------------------------------------------*
      *                  * Test se errore di lettura                   *
      *                  *---------------------------------------------*
           if        f-sts                =    e-not-err
                     go to rip-sdo-esu-400.
      *                  *---------------------------------------------*
      *                  * Incremento numero errori riscontrati        *
      *                  *---------------------------------------------*
           add       1                    to   w-num-err-ris          .
      *                  *---------------------------------------------*
      *                  * Emissione messaggio di errore               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Editing codice fornitore                *
      *                      *-----------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      7                    to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<"                  to   v-edm                  .
           move      rf-mgs-cod-con       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Composizione messaggio completo         *
      *                      *-----------------------------------------*
           move      spaces               to   w-err-msg-err          .
           string    "Non esiste l'anagrafica per il fornitore "
                                delimited by   size
                     v-edt      delimited by   spaces
                                          into w-err-msg-err          .
      *                      *-----------------------------------------*
      *                      * Output messaggio                        *
      *                      *-----------------------------------------*
           perform   out-msg-err-000      thru out-msg-err-999        .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     rip-sdo-esu-999.
       rip-sdo-esu-400.
      *              *-------------------------------------------------*
      *              * Test su flag di elaborazione                    *
      *              *-------------------------------------------------*
           if        w-wrk-flg-ela        =    spaces
                     go to  rip-sdo-esu-999.
      *              *-------------------------------------------------*
      *              * Se conto non patrimoniale : uscita              *
      *              *-------------------------------------------------*
           if        rf-mgs-tip-rec       =    "G" and
                     rf-pdc-tip-cnt       =    "E"
                     go to rip-sdo-esu-999.
      *              *-------------------------------------------------*
      *              * Determinazione saldo di fine bilancio           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Saldo a inizio anno                         *
      *                  *---------------------------------------------*
           move      rf-mgs-sdo-ini       to   w-wrk-sdo-bil          .
      *                  *---------------------------------------------*
      *                  * Ciclo di scansione sui progressivi mensili  *
      *                  *---------------------------------------------*
           move      zero                 to   I                      .
       rip-sdo-esu-420.
           add       1                    to   I                      .
           if        I                    >    12
                     go to rip-sdo-esu-440.
           add       rf-mgs-dar-mes (I)   to   w-wrk-sdo-bil          .
           subtract  rf-mgs-ave-mes (I)   from w-wrk-sdo-bil          .
           go to     rip-sdo-esu-420.
       rip-sdo-esu-440.
      *                  *---------------------------------------------*
      *                  * Rettifiche di bilancio                      *
      *                  *---------------------------------------------*
           add       rf-mgs-dar-bil       to   w-wrk-sdo-bil          .
           subtract  rf-mgs-ave-bil       from w-wrk-sdo-bil          .
      *              *-------------------------------------------------*
      *              * Se saldo di fine bilancio a zero : uscita       *
      *              *-------------------------------------------------*
           if        w-wrk-sdo-bil        =    zero
                     go to  rip-sdo-esu-999.
      *              *-------------------------------------------------*
      *              * Aggiornamento utile o perdita dell'esercizio    *
      *              *-------------------------------------------------*
           if        rf-mgs-tip-rec       not  = "G" or
                     rf-pdc-tip-cnt       =    "E"
                     go to rip-sdo-esu-500.
           add       w-wrk-sdo-bil        to   w-wrk-uop-ese          .
       rip-sdo-esu-500.
      *              *-------------------------------------------------*
      *              * Riporto su [mgs] anno successivo                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione record [mgs]                *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmgs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgs                 .
      *                  *---------------------------------------------*
      *                  * Preparazione chiave                         *
      *                  *---------------------------------------------*
           move      rn-dat-bil-ese-bil   to   rf-mgs-ann-ese         .
           add       1                    to   rf-mgs-ann-ese         .
           move      w-sav-mgs-tip        to   rf-mgs-tip-rec         .
           move      w-sav-mgs-cod        to   rf-mgs-cod-con         .
      *                  *---------------------------------------------*
      *                  * Ottenimento record                          *
      *                  *---------------------------------------------*
           move      "GK"                 to   f-ope                  .
           move      "ESECOD    "         to   f-key                  .
           move      "pgm/cge/fls/ioc/obj/iofmgs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgs                 .
      *                      *-----------------------------------------*
      *                      * Test su esito lettura                   *
      *                      *-----------------------------------------*
           if        f-sts                =    e-not-err
                     go to  rip-sdo-esu-520.
      *                          *-------------------------------------*
      *                          * Se invalid key : scrittura record   *
      *                          *-------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmgs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgs                 .
      *                              *---------------------------------*
      *                              * Riciclo a inizio                *
      *                              *---------------------------------*
           go to     rip-sdo-esu-500.
       rip-sdo-esu-520.
      *                  *---------------------------------------------*
      *                  * Aggiornamento saldo a inizio anno           *
      *                  *---------------------------------------------*
           move      w-wrk-sdo-bil        to   rf-mgs-sdo-ini         .
      *                  *---------------------------------------------*
      *                  * Aggiornamento record file                   *
      *                  *---------------------------------------------*
           move      "UP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmgs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgs                 .
      *                      *-----------------------------------------*
      *                      * Test su esito operazione                *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to  rip-sdo-esu-500.
      *                  *---------------------------------------------*
      *                  * Rilascio record                             *
      *                  *---------------------------------------------*
           move      "RL"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmgs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgs                 .
       rip-sdo-esu-999.
           exit.

      *    *===========================================================*
      *    * Aggiustamento saldo a inizio anno successivo del sotto-   *
      *    * conto utile o perdita d'esercizio                         *
      *    *-----------------------------------------------------------*
       ags-sdo-uop-000.
      *              *-------------------------------------------------*
      *              * Test se valore a zero                           *
      *              *-------------------------------------------------*
           if        w-wrk-uop-ese        =    zero
                     go to ags-sdo-uop-999.
       ags-sdo-uop-500.
      *              *-------------------------------------------------*
      *              * Riporto su [mgs] anno successivo                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione record [mgs]                *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmgs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgs                 .
      *                  *---------------------------------------------*
      *                  * Preparazione chiave                         *
      *                  *---------------------------------------------*
           move      rn-dat-bil-ese-bil   to   rf-mgs-ann-ese         .
           add       1                    to   rf-mgs-ann-ese         .
           move      "G"                  to   rf-mgs-tip-rec         .
           if        w-wrk-uop-ese        >    zero
                     move  w-ref-stc-ute  to   rf-mgs-cod-con
           else      move  w-ref-stc-pee  to   rf-mgs-cod-con         .
      *                  *---------------------------------------------*
      *                  * Ottenimento record                          *
      *                  *---------------------------------------------*
           move      "GK"                 to   f-ope                  .
           move      "ESECOD    "         to   f-key                  .
           move      "pgm/cge/fls/ioc/obj/iofmgs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgs                 .
      *                      *-----------------------------------------*
      *                      * Test su esito lettura                   *
      *                      *-----------------------------------------*
           if        f-sts                =    e-not-err
                     go to  ags-sdo-uop-520.
      *                          *-------------------------------------*
      *                          * Se invalid key : scrittura record   *
      *                          *-------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmgs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgs                 .
      *                              *---------------------------------*
      *                              * Riciclo a inizio                *
      *                              *---------------------------------*
           go to     ags-sdo-uop-500.
       ags-sdo-uop-520.
      *                  *---------------------------------------------*
      *                  * Aggiornamento saldo a inizio anno           *
      *                  *---------------------------------------------*
           move      w-wrk-uop-ese        to   rf-mgs-sdo-ini         .
           multiply  -1                   by   rf-mgs-sdo-ini         .
      *                  *---------------------------------------------*
      *                  * Aggiornamento record file                   *
      *                  *---------------------------------------------*
           move      "UP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmgs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgs                 .
      *                      *-----------------------------------------*
      *                      * Test su esito operazione                *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to  ags-sdo-uop-500.
      *                  *---------------------------------------------*
      *                  * Rilascio record                             *
      *                  *---------------------------------------------*
           move      "RL"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmgs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgs                 .
       ags-sdo-uop-999.
           exit.

      *    *===========================================================*
      *    * Aggiornamento record numerazione                          *
      *    *-----------------------------------------------------------*
       agg-rec-num-000.
      *              *-------------------------------------------------*
      *              * Open file numerazione [datbil]                  *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/num/ioc/obj/indatbil"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-dat-bil             .
      *              *-------------------------------------------------*
      *              * Normalizzazione record numerazione [datbil]     *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/cge/num/ioc/obj/indatbil"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-dat-bil             .
      *              *-------------------------------------------------*
      *              * Ottenimento record numerazione [datbil]         *
      *              *-------------------------------------------------*
           move      "GT"                 to   f-ope                  .
           move      "pgm/cge/num/ioc/obj/indatbil"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-dat-bil             .
      *              *-------------------------------------------------*
      *              * Aggiornamento dati record                       *
      *              *-------------------------------------------------*
           move      spaces               to   rn-dat-bil-flg-bil     .
           move      w-wrk-dat-att        to   rn-dat-bil-fin-bil     .
      *              *-------------------------------------------------*
      *              * Aggiornamento record file [datbil]              *
      *              *-------------------------------------------------*
           move      "UP"                 to   f-ope                  .
           move      "pgm/cge/num/ioc/obj/indatbil"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-dat-bil             .
      *              *-------------------------------------------------*
      *              * Rilascio record file numerazione [datbil]       *
      *              *-------------------------------------------------*
           move      "RL"                 to   f-ope                  .
           move      "pgm/cge/num/ioc/obj/indatbil"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-dat-bil             .
      *              *-------------------------------------------------*
      *              * Close file numerazione [datbil]                 *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/num/ioc/obj/indatbil"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-dat-bil             .
       agg-rec-num-999.
           exit.

      *    *===========================================================*
      *    * Scrittura messaggio di errore                             *
      *    *-----------------------------------------------------------*
       out-msg-err-000.
      *              *-------------------------------------------------*
      *              * Test su contatore numero errori riscontrati     *
      *              *-------------------------------------------------*
           if        w-num-err-ris        not  = 1
                     go to  out-msg-err-100.
      *              *-------------------------------------------------*
      *              * Intestazione messaggi                           *
      *              *-------------------------------------------------*
           move      "WR"                 to   m-ope                  .
           move      "Attenzione: durante l'esecuzione del programma son
      -              "o stati riscontrati gli errori"
                                          to   m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
      *
           move      "WR"                 to   m-ope                  .
           move      "sottoelencati e l'elaborazione pertanto non ha avu
      -              "to luogo; sara' quindi neces-"
                                          to   m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
      *
           move      "WR"                 to   m-ope                  .
           move      "sario ripeterla dopo la loro correzione."
                                          to   m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
      *
           move      "WR"                 to   m-ope                  .
           move      spaces               to   m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
      *
           move      "WR"                 to   m-ope                  .
           move      all "*"              to   m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
      *
           move      "WR"                 to   m-ope                  .
           move      "N.B.: per i sottoconti senza anagrafica, verificar
      -              "e con 'cge950' la scheda saldi"
                                          to   m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
      *
           move      "WR"                 to   m-ope                  .
           move      "      e, se a zero, cancellarla"
                                          to   m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
      *
           move      "WR"                 to   m-ope                  .
           move      all "*"              to   m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
      *
           move      "WR"                 to   m-ope                  .
           move      spaces               to   m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
       out-msg-err-100.
      *              *-------------------------------------------------*
      *              * Scrittura messaggio                             *
      *              *-------------------------------------------------*
           move      "WR"                 to   m-ope                  .
           move      w-err-msg-err        to   m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
       out-msg-err-999.
           exit.

      *    *===========================================================*
      *    * Editing del codice sottoconto con appoggio a sx o dx      *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/wedtpdc0.wks"                   .
