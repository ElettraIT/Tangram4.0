       Identification Division.
       Program-Id.                                 pcge8000           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    cge                 *
      *                                Settore:    bil                 *
      *                                   Fase:    cge800              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 14/04/91    *
      *                       Ultima revisione:    NdK del 30/04/10    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Dichiarazione di inizio fase di bilancio    *
      *                                                                *
      *                    Il programma scrive in [datbil] la data     *
      *                    di inizio fase di bilancio e l'esercizio    *
      *                    a cui si riferisce                          *
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
                     "PERIODO DI BILANCIO ESERCIZIO PRECEDENTE"       .

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
      *        * Flags di controllo su modalita' di funzionamento      *
      *        *-------------------------------------------------------*
           05  w-cnt-mfu.
      *            *---------------------------------------------------*
      *            * Visualizzazione forzata da segreteria             *
      *            *---------------------------------------------------*
               10  w-cnt-mfu-vis-sgr      pic  x(01)                  .
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
      *        *-------------------------------------------------------*
      *        * Area di controllo per funzionamento print-routine     *
      *        *-------------------------------------------------------*
           05  w-cnt-prn.
      *            *---------------------------------------------------*
      *            * Flag di primo giro                                *
      *            *---------------------------------------------------*
               10  w-cnt-prn-mrk-uno      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Flag di interruzione forzata                      *
      *            *---------------------------------------------------*
               10  w-cnt-prn-flg-int      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Flag di uscita da subroutines principali          *
      *            *---------------------------------------------------*
               10  w-cnt-prn-flg-sub      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Area per salvataggio parametri rottura livello    *
      *            *---------------------------------------------------*
               10  w-cnt-prn-sav-liv.
                   15  w-cnt-prn-sav-l05  pic  x(64)                  .
                   15  w-cnt-prn-sav-l04  pic  x(64)                  .
                   15  w-cnt-prn-sav-l03  pic  x(64)                  .
                   15  w-cnt-prn-sav-l02  pic  x(64)                  .
                   15  w-cnt-prn-sav-l01  pic  x(64)                  .
      *            *---------------------------------------------------*
      *            * Area per salvataggio area di rottura              *
      *            *---------------------------------------------------*
               10  w-cnt-prn-sav-rot.
                   15  filler occurs 320  pic  x(01)                  .

      *    *===========================================================*
      *    * Records files                                             *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [datbil]                                              *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/num/rec/rndatbil"                       .
      *        *-------------------------------------------------------*
      *        * [giocon]                                              *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/num/rec/rngiocon"                       .

      *    *===========================================================*
      *    * Work-area richieste per programma                         *
      *    *-----------------------------------------------------------*
       01  rr.
      *        *-------------------------------------------------------*
      *        * Data bilancio d'esercizio                             *
      *        *-------------------------------------------------------*
           05  rr-dat-bil                 pic  9(07)                  .

      *    *===========================================================*
      *    * Work per dati bilancio                                    *
      *    *-----------------------------------------------------------*
       01  w-bil.
           05  w-bil-dat-att              pic  9(07)                  .
           05  w-bil-ese-att              pic  9(03)                  .
           05  w-bil-dat-bil              pic  9(07)                  .
           05  w-bil-ese-bil              pic  9(03)                  .
           05  w-bil-mes-bil              pic  9(02)                  .
           05  w-bil-gio-bil              pic  9(02)                  .
               
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
      *    * Work per subroutines di Err                               *
      *    *-----------------------------------------------------------*
       01  w-err.
      *        *-------------------------------------------------------*
      *        * Work per Err con box centrale                         *
      *        *-------------------------------------------------------*
           05  w-err-box-err.
               10  w-err-box-err-msg      pic  x(65)                  .
               10  w-err-box-err-m02      pic  x(65)                  .

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
      *              * Esecuzione routine pre-esecuzione programma     *
      *              *-------------------------------------------------*
           perform   pre-exe-pgm-000      thru pre-exe-pgm-999        .
           if        w-cnt-pre-exe-pgm    not  = spaces
                     go to main-900.
      *              *-------------------------------------------------*
      *              * Accettazione conferma esecuzione                *
      *              *-------------------------------------------------*
           perform   acc-ric-sel-000      thru acc-ric-sel-999        .
      *                  *---------------------------------------------*
      *                  * Se uscita per Exit                          *
      *                  *---------------------------------------------*
           if        w-cnt-acc-ric-sel    =    "E"
                     go to main-900.
      *              *-------------------------------------------------*
      *              * Regolarizzazione richieste di selezione         *
      *              *-------------------------------------------------*
           perform   reg-ric-sel-000      thru reg-ric-sel-999        .
      *              *-------------------------------------------------*
      *              * Esecuzione in foreground                        *
      *              *-------------------------------------------------*
           perform   exe-pgm-frg-000      thru exe-pgm-frg-999        .
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
      *              * Esecuzione del programma                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Dichiarazione di inizio bilancio            *
      *                  *---------------------------------------------*
           perform   dic-ini-bil-000      thru dic-ini-bil-999        .
      *                  *---------------------------------------------*
      *                  * Visual. eventuali errori di esecuzione      *
      *                  *---------------------------------------------*
           move      "VE"                 to   b-ope                  .
           move      "F"                  to   b-tfe                  .
           move      i-ide-des            to   b-chr                  .
           call      "swd/mod/prg/obj/mbckgv"
                                         using b                      .
           cancel    "swd/mod/prg/obj/mbckgv"                         .
       exe-pgm-frg-999.
           exit.

      *    *===========================================================*
      *    * Routine pre-esecuzione programma                          *
      *    *-----------------------------------------------------------*
       pre-exe-pgm-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-pre-exe-pgm      .
       pre-exe-pgm-010.
      *              *-------------------------------------------------*
      *              * Test se programma eseguibile                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su flag di eventuale visualizzazione   *
      *                  *---------------------------------------------*
           if        w-cnt-mfu-vis-sgr    not  = "V"
                     go to pre-exe-pgm-020.
      *                  *---------------------------------------------*
      *                  * Messaggio di errore                         *
      *                  *---------------------------------------------*
           move      "Programma non eseguibile dall'utente !            
      -              "               "    to   w-err-box-err-msg      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione messaggio di errore         *
      *                  *---------------------------------------------*
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                  *---------------------------------------------*
      *                  * Flag di uscita ad errore                    *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cnt-pre-exe-pgm      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     pre-exe-pgm-999.
       pre-exe-pgm-020.
      *              *-------------------------------------------------*
      *              * Visualizzazione titolo programma                *
      *              *-------------------------------------------------*
           perform   vis-tit-pgm-000      thru vis-tit-pgm-999        .
      *              *-------------------------------------------------*
      *              * Lettura scelta mese di chiusura esercizio       *
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
      *              * Lettura dati relativi alla chiusura bilanci     *
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
      *              * Test se fase di bilancio gia' in atto           *
      *              *-------------------------------------------------*
           if        rn-dat-bil-flg-bil   =    spaces
                     go to pre-exe-pgm-100.
      *                  *---------------------------------------------*
      *                  * Messaggio di errore                         *
      *                  *---------------------------------------------*
           move      "ME"                 to   v-ope                  .
           move      spaces               to   v-not                  .
           move      "Attenzione : deve essere ancora dichiarata la chiu
      -              "sura fase bilancio relativa"
                                          to   v-nt1                  .
           move      rn-dat-bil-ese-bil   to   v-saa                  .
           string    "all'esercizio "
                                delimited by   size
                     v-ann      delimited by   size
                     "; il programma pertanto non e' eseguibile !"
                                delimited by   size
                                          into v-nt2                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Uscita con errore                           *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cnt-pre-exe-pgm      .
           go to     pre-exe-pgm-999.
       pre-exe-pgm-100.
      *              *-------------------------------------------------*
      *              * Determinazione data attuale                     *
      *              *-------------------------------------------------*
           move      "DT"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-dat                to   w-bil-dat-att          .
      *              *-------------------------------------------------*
      *              * Determinazione esercizio data attuale           *
      *              *-------------------------------------------------*
           move      w-bil-dat-att        to   w-ese-cge-dtr          .
           perform   det-ese-cge-000      thru det-ese-cge-999        .
           move      w-ese-cge-esa        to   w-bil-ese-att          .
      *              *-------------------------------------------------*
      *              * Determinazione esercizio per bilancio           *
      *              *-------------------------------------------------*
           subtract  1                    from w-bil-ese-att
                                        giving w-bil-ese-bil          .
      *              *-------------------------------------------------*
      *              * Determinazione mese per bilancio                *
      *              *-------------------------------------------------*
           move      w-ese-cge-mce        to   w-bil-mes-bil          .
      *              *-------------------------------------------------*
      *              * Determinazione giorno chiusura bilancio         *
      *              *-------------------------------------------------*
           move      zero                 to   s-dat                  .
           move      w-bil-ese-bil        to   s-saa                  .
           if        w-ese-cge-mce        not  = 12
                     add   1              to   s-saa                  .
           move      w-bil-mes-bil        to   s-mes                  .
           move      31                   to   s-gio                  .
       pre-exe-pgm-120.
           move      "CD"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-sts                not  = spaces
                     subtract 1           from s-gio
                     go to pre-exe-pgm-120.
           move      s-gio                to   w-bil-gio-bil          .
      *              *-------------------------------------------------*
      *              * Determinazione data chiusura bilancio           *
      *              *-------------------------------------------------*
           move      zero                 to   s-dat                  .
           move      w-bil-gio-bil        to   s-gio                  .
           move      w-bil-mes-bil        to   s-mes                  .
           move      w-bil-ese-bil        to   s-saa                  .
           if        w-ese-cge-mce        not  = 12
                     add   1              to   s-saa                  .
           move      s-dat                to   w-bil-dat-bil          .
       pre-exe-pgm-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     pre-exe-pgm-999.
       pre-exe-pgm-999.
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
      *                  * Data bilancio                               *
      *                  *---------------------------------------------*
           perform   acc-dat-bil-000      thru acc-dat-bil-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
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
           move      "Si conferma l'esecuzione del programma (S/N) ?"
                                          to   v-not                  .
           move      "S"                  to   v-alf                  .
           move      "SN"                 to   v-msk                  .
           move      "DO  "               to   v-pfk (05)             .
           move      "EXIT"               to   v-pfk (20)             .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           if        v-key                not  = spaces
                     go to acc-ric-sel-920.
           if        v-alf                =    "S"
                     move   "DO  "        to   v-key
           else if   v-alf                =    "N"
                     move   "EXIT"        to   v-key                  .
       acc-ric-sel-920.
      *                  *---------------------------------------------*
      *                  * Test su risposta dell'utente                *
      *                  *---------------------------------------------*
           if        v-key                =    "DO  "
                     go to acc-ric-sel-930
           else if   v-key                =    "EXIT"
                     go to acc-ric-sel-940
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
       acc-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per richieste di selezione        *
      *    *-----------------------------------------------------------*
       pmt-ric-sel-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      65                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Dichiarazione periodo di bilancio dell'esercizio d
      -              "a chiudere il :"    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo selezione : Data bilancio              *
      *    *-----------------------------------------------------------*
       acc-dat-bil-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione valore di default              *
      *                  *---------------------------------------------*
           if        rr-dat-bil           =    zero
                     move  w-bil-dat-bil  to   rr-dat-bil             .
       acc-dat-bil-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      07                   to   v-lin                  .
           move      67                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-ric    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      rr-dat-bil           to   v-dat                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-dat-bil-999.
       acc-dat-bil-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-dat                to   rr-dat-bil             .
       acc-dat-bil-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Valore a zero non ammesso                   *
      *                  *---------------------------------------------*
           if        rr-dat-bil           =    zero
                     go to acc-dat-bil-100.
      *                  *---------------------------------------------*
      *                  * Data impostata in work di comodo ridefinita *
      *                  *---------------------------------------------*
           move      rr-dat-bil           to   s-dat                  .
      *                  *---------------------------------------------*
      *                  * Controllo formale su giorno e mese          *
      *                  *---------------------------------------------*
           if        s-gio                =    w-bil-gio-bil and
                     s-mes                =    w-bil-mes-bil
                     go to acc-dat-bil-420.
      *                      *-----------------------------------------*
      *                      * Messaggio di errore                     *
      *                      *-----------------------------------------*
           move      "La data impostata non e' di fine esercizio !      
      -              "               "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                      *-----------------------------------------*
      *                      * Reimpostazione                          *
      *                      *-----------------------------------------*
           go to     acc-dat-bil-100.
       acc-dat-bil-420.
      *                  *---------------------------------------------*
      *                  * Controllo stampa data ultima giornale       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Determinazione                          *
      *                      *-----------------------------------------*
           perform   acc-dat-bil-usg-000  thru acc-dat-bil-usg-999    .
      *                      *-----------------------------------------*
      *                      * Test                                    *
      *                      *-----------------------------------------*
           if        rn-gio-con-dat-stp   not  < rr-dat-bil
                     go to acc-dat-bil-440.
      *                      *-----------------------------------------*
      *                      * Editing data                            *
      *                      *-----------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      rr-dat-bil           to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Messaggio di errore                     *
      *                      *-----------------------------------------*
           move      "Attenzione : deve essere ancora stampato il giorna
      -              "le contabile al"    to   w-err-box-err-msg      .
      *
           move      spaces               to   w-err-box-err-m02      .
      *
           string    v-edt      delimited by   spaces
                     ". "       delimited by   size
                     "Il programma pertanto non e' eseguibile !"
                                delimited by   size
                                          into w-err-box-err-m02      .
      *
           perform   box-msg-e02-000      thru box-msg-e02-999        .
      *                      *-----------------------------------------*
      *                      * Reimpostazione                          *
      *                      *-----------------------------------------*
           go to     acc-dat-bil-100.
       acc-dat-bil-440.
      *                  *---------------------------------------------*
      *                  * Controllo su anno di esercizio del bilancio *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test su chiusura esercizio              *
      *                      *-----------------------------------------*
           if        w-ese-cge-mce        not  = 12
                     go to acc-dat-bil-500.
      *                      *-----------------------------------------*
      *                      * Confronto                               *
      *                      *-----------------------------------------*
           move      rr-dat-bil           to   s-dat                  .
           if        rn-dat-bil-ese-bil   <    s-saa
                     go to acc-dat-bil-600.
      *                      *-----------------------------------------*
      *                      * Editing anno di esercizio               *
      *                      *-----------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      s-saa                to   v-num                  .
           add       1900                 to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Messaggio di errore                     *
      *                      *-----------------------------------------*
           move      spaces               to   w-err-box-err-msg      .
      *
           string    "Attenzione : bilancio gia' chiuso per l'anno "
                                delimited by   size
                     v-edt      delimited by   spaces
                     "."        delimited by   size
                                          into w-err-box-err-msg      .
      *
           move      "             Il programma pertanto non e' eseguibi
      -              "le !         "      to   w-err-box-err-m02      .
      *
           perform   box-msg-e02-000      thru box-msg-e02-999        .
      *                      *-----------------------------------------*
      *                      * Reimpostazione                          *
      *                      *-----------------------------------------*
           go to     acc-dat-bil-100.
       acc-dat-bil-500.
      *                  *---------------------------------------------*
      *                  * Controllo su anno di esercizio del bilancio *
      *                  * se esercizio con chiusura diversa mese 12   *
      *                  *---------------------------------------------*
       acc-dat-bil-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-dat-bil-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-dat-bil-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-dat-bil-100.
       acc-dat-bil-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo selezione : Data bilancio              *
      *    *                                                           *
      *    * Subroutine di determinazione data ultima stampa giornale  *
      *    *-----------------------------------------------------------*
       acc-dat-bil-usg-000.
      *              *-------------------------------------------------*
      *              * Lettura dati relativi alla data ultima stampa   *
      *              * giornale contabile                              *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/num/ioc/obj/ingiocon"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-gio-con             .
      *
           move      "NO"                 to   f-ope                  .
           move      "pgm/cge/num/ioc/obj/ingiocon"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-gio-con             .
      *
           move      "RD"                 to   f-ope                  .
           move      "pgm/cge/num/ioc/obj/ingiocon"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-gio-con             .
      *
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/num/ioc/obj/ingiocon"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-gio-con             .
       acc-dat-bil-usg-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     acc-dat-bil-usg-999.
       acc-dat-bil-usg-999.
           exit.

      *    *===========================================================*
      *    * Regolarizzazione dei parametri di selezione               *
      *    *-----------------------------------------------------------*
       reg-ric-sel-000.
      *              *-------------------------------------------------*
      *              * Data di chiusura del bilancio                   *
      *              *-------------------------------------------------*
           move      rr-dat-bil           to   w-bil-dat-bil          .
      *              *-------------------------------------------------*
      *              * Scomposizione data                              *
      *              *-------------------------------------------------*
           move      w-bil-dat-bil        to   s-dat                  .
      *              *-------------------------------------------------*
      *              * Se il mese di chiusura esercizio e' dicembre    *
      *              * l'esercizio attuale e' quello relativo all'anno *
      *              * in corso                                        *
      *              *-------------------------------------------------*
           if        w-ese-cge-mce        =    12
                     move  s-saa          to   w-bil-ese-bil
                     go to reg-ric-sel-999.
      *              *-------------------------------------------------*
      *              * Se il mese di chiusura esercizio non e' dicem-  *
      *              * bre ed il mese di chiusura esercizio e' maggio- *
      *              * re del mese attuale : l'esercizio attuale       *
      *              * e' quello relativo all'anno in corso            *
      *              *-------------------------------------------------*
           if        s-mes                >    w-ese-cge-mce
                     move  s-saa          to   w-bil-ese-bil
                     go to reg-ric-sel-999.
      *              *-------------------------------------------------*
      *              * Decremento di un anno per determinare l'eserci- *
      *              * zio in corso                                    *
      *              *-------------------------------------------------*
           subtract  1                    from s-saa                  .
           move      "NS"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-saa                to   w-bil-ese-bil          .
       reg-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione richieste di selezione                    *
      *    *-----------------------------------------------------------*
       nor-ric-sel-000.
           move      zero                 to   rr-dat-bil             .
       nor-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Dichiarazione di inizio bilancio                          *
      *    *-----------------------------------------------------------*
       dic-ini-bil-000.
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
      *              * Ottenimento record [datbil]                     *
      *              *-------------------------------------------------*
           move      "GT"                 to   f-ope                  .
           move      "pgm/cge/num/ioc/obj/indatbil"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-dat-bil             .
      *                  *---------------------------------------------*
      *                  * Normalizzazione record [datbil]             *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/cge/num/ioc/obj/indatbil"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-dat-bil             .
      *                  *---------------------------------------------*
      *                  * Aggiornamento record [datbil]               *
      *                  *---------------------------------------------*
           move      w-bil-dat-att        to   rn-dat-bil-ini-bil     .
           move      w-bil-ese-bil        to   rn-dat-bil-ese-bil     .
      *                  *---------------------------------------------*
      *                  * Update record [datbil]                      *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/num/ioc/obj/indatbil"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-dat-bil             .
      *              *-------------------------------------------------*
      *              * Open tabella numerazioni [datbil]               *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/num/ioc/obj/indatbil"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-dat-bil             .
       dic-ini-bil-100.
      *              *-------------------------------------------------*
      *              * Lettura tabella numerazioni [datbil]            *
      *              *-------------------------------------------------*
           move      "GT"                 to   f-ope                  .
           move      "pgm/cge/num/ioc/obj/indatbil"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-dat-bil             .
      *              *-------------------------------------------------*
      *              * Test su esito operazione                        *
      *              *-------------------------------------------------*
           if        f-sts                =    e-not-err
                     go to dic-ini-bil-200.
      *                  *---------------------------------------------*
      *                  * Record non trovato                          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazione record                  *
      *                      *-----------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/cge/num/ioc/obj/indatbil"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-dat-bil             .
      *                      *-----------------------------------------*
      *                      * Scrittura record normalizzato           *
      *                      *-----------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      "pgm/cge/num/ioc/obj/indatbil"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-dat-bil             .
      *                      *-----------------------------------------*
      *                      * Ripetizione dell'intera operazione      *
      *                      *-----------------------------------------*
           go to     dic-ini-bil-100.
       dic-ini-bil-200.
      *                  *---------------------------------------------*
      *                  * Record trovato                              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Aggiornamento record                    *
      *                      *-----------------------------------------*
           move      "#"                  to   rn-dat-bil-flg-bil     .
           move      w-bil-dat-att        to   rn-dat-bil-ini-bil     .
           move      zero                 to   rn-dat-bil-fin-bil     .
           move      w-bil-ese-bil        to   rn-dat-bil-ese-bil     .
      *                      *-----------------------------------------*
      *                      * Update record                           *
      *                      *-----------------------------------------*
           move      "UP"                 to   f-ope                  .
           move      "pgm/cge/num/ioc/obj/indatbil"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-dat-bil             .
      *                      *-----------------------------------------*
      *                      * Se errori ripete l'intera operazione    *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to dic-ini-bil-100.
      *                      *-----------------------------------------*
      *                      * Unlock                                  *
      *                      *-----------------------------------------*
           move      "RL"                 to   f-ope                  .
           move      "pgm/cge/num/ioc/obj/indatbil"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-dat-bil             .
      *              *-------------------------------------------------*
      *              * Close tabella numerazioni [datbil]              *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/num/ioc/obj/indatbil"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-dat-bil             .
       dic-ini-bil-999.
           exit.

      *    *===========================================================*
      *    * Controllo su tasto Do in parametri di selezione           *
      *    *-----------------------------------------------------------*
       tdo-ric-sel-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-tdo-ric-flg      .
       tdo-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Determinazione anno di esercizio per contabilita'         *
      *    *-----------------------------------------------------------*
       det-ese-cge-000.
      *              *-------------------------------------------------*
      *              * Scomposizione data attuale                      *
      *              *-------------------------------------------------*
           move      w-ese-cge-dtr        to   s-dat                  .
      *              *-------------------------------------------------*
      *              * Se il mese di chiusura esercizio e' dicembre    *
      *              * l'esercizio attuale e' quello relativo all'anno *
      *              * in corso                                        *
      *              *-------------------------------------------------*
           if        w-ese-cge-mce        =    12
                     move  s-saa          to   w-ese-cge-esa
                     go to det-ese-cge-500.
      *              *-------------------------------------------------*
      *              * Se il mese di chiusura esercizio non e' dicem-  *
      *              * bre ed il mese di chiusura esercizio e' maggio- *
      *              * re del mese attuale : l'esercizio attuale       *
      *              * e' quello relativo all'anno in corso            *
      *              *-------------------------------------------------*
           if        s-mes                >    w-ese-cge-mce
                     move  s-saa          to   w-ese-cge-esa
                     go to det-ese-cge-500.
      *              *-------------------------------------------------*
      *              * Decremento di un anno per determinare l'eserci- *
      *              * zio in corso                                    *
      *              *-------------------------------------------------*
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

