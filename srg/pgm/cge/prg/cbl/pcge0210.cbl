       Identification division.
       Program-id.                                 pcge0210          .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    cge                 *
      *                                Settore:    tab                 *
      *                                   Fase:    cge021              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 17/09/89    *
      *                       Ultima revisione:    NdK del 07/07/03    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Interrogazione su archivio conti del piano  *
      *                    dei conti                                   *
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
                     "tab"                                            .
      *        *-------------------------------------------------------*
      *        * Fase gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-fas                  pic  x(06) value
                     "cge021"                                         .
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10) value
                     "pcge0210"                                       .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  i-ide-des                  pic  x(40) value
                     "    INTERROGAZIONE SU ARCHIVIO CONTI    "       .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "mvideo" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/v"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per moduli di input-output          *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/f"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "msegrt"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/s"                                  .

      *    *===========================================================*
      *    * Area per definizione codici di errore di i-o              *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/e"                                  .

      *    *===========================================================*
      *    * Record files                                              *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [zma]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfzma"                          .
      *        *-------------------------------------------------------*
      *        * [zcn]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfzcn"                          .

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
      *        * Flag di primo giro esecuzione                         *
      *        *-------------------------------------------------------*
           05  y-flg-prm-gir              pic  x(01)                  .

      *    *===========================================================*
      *    * Work-area controllo modalita' di funzionamento            *
      *    *-----------------------------------------------------------*
       01  w-fun.
           05  w-fun-ric                  pic  x(01)                  .
           05  w-fun-cic                  pic  x(01)                  .
           05  w-fun-aut                  pic  x(01)                  .

      *    *===========================================================*
      *    * Work-area personalizzazioni                               *
      *    *-----------------------------------------------------------*
       01  w-prs.
           05  w-prs-liv-pdc              pic  9(01)                  .

      *    *===========================================================*
      *    * Work-area per variabili di i.p.c.                         *
      *    *-----------------------------------------------------------*
       01  w-ipc.
           05  w-ipc-cod-mas              pic  9(02)                  .

      *    *===========================================================*
      *    * Work-area richieste per interrogazione                    *
      *    *-----------------------------------------------------------*
       01  w-ric.
           05  w-ric-cod-mas              pic  9(02)                  .
           05  w-ric-des-mas              pic  x(40)                  .

      *    *===========================================================*
      *    * Work-area per ridefinizione contenuto del mark-point      *
      *    *-----------------------------------------------------------*
       01  w-mpn.
           05  w-mpn-cod-con              pic  9(02)                  .

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
      *              * Preparazioni iniziali per il programma          *
      *              *-------------------------------------------------*
           move      spaces               to   OK                     .
           perform   z-pre-ini-pgm-000    thru z-pre-ini-pgm-999      .
           if        OK                   not  = spaces
                     go to main-900.
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
       main-300.
      *              *-------------------------------------------------*
      *              * Esecuzione interrogazione                       *
      *              *-------------------------------------------------*
           perform   z-qry-rou-pri-000   thru z-qry-rou-pri-999       .
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
      *    Preparazioni iniziali per il programma                      *
      *----------------------------------------------------------------*
       z-pre-ini-pgm-000.
      *              *-------------------------------------------------*
      *              * Esecuzione routine pre-esecuzione programma     *
      *              *-------------------------------------------------*
           move      spaces               to   OK                     .
           perform   z-pre-exe-pgm-000    thru z-pre-exe-pgm-999      .
           if        OK                   not  = spaces
                     go to  z-pre-ini-pgm-999.
      *              *-------------------------------------------------*
      *              * Preparazione tipo funzionamento                 *
      *              *-------------------------------------------------*
           perform   z-pre-tip-fun-000    thru z-pre-tip-fun-999      .
       z-pre-ini-pgm-999.
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
                     go to  z-qry-rou-pri-900.
      *              *-------------------------------------------------*
      *              * Start iniziale                                  *
      *              *-------------------------------------------------*
           perform   z-qry-str-ini-000    thru z-qry-str-ini-999      .
           if        v-mrk                not  = spaces
                     move   spaces        to   v-mrk
                     go to  z-qry-rou-pri-800.
       z-qry-rou-pri-100.
      *              *-------------------------------------------------*
      *              * Lettura sequenziale                             *
      *              *-------------------------------------------------*
           perform   z-qry-let-seq-000    thru z-qry-let-seq-999      .
           if        v-mrk                not  = spaces
                     move   spaces        to   v-mrk
                     move   "ST"          to   v-ope
                     call   "swd/mod/prg/obj/mvideo"
                                         using v
                     if     v-pag         =    zero
                            go to z-qry-rou-pri-800
                     else   go to z-qry-rou-pri-200.
      *              *-------------------------------------------------*
      *              * Test se superamento limiti massimi              *
      *              *-------------------------------------------------*
           perform   z-qry-tst-max-000    thru z-qry-tst-max-999      .
           if        v-mrk                not  = spaces
                     move   spaces        to   v-mrk
                     move   "ST"          to   v-ope
                     call   "swd/mod/prg/obj/mvideo"
                                         using v
                     if     v-pag         =    zero
                            go to z-qry-rou-pri-800
                     else   go to z-qry-rou-pri-200.
      *              *-------------------------------------------------*
      *              * Selezione su record letto                       *
      *              *-------------------------------------------------*
           perform   z-qry-sel-rec-000    thru z-qry-sel-rec-999      .
           if        v-mrk                not  = spaces
                     move   spaces        to   v-mrk
                     go to  z-qry-rou-pri-100.
      *              *-------------------------------------------------*
      *              * Test se primo passaggio                         *
      *              *-------------------------------------------------*
           if        v-pag                not  = zero
                     go to z-qry-rou-pri-400.
       z-qry-rou-pri-200.
      *              *-------------------------------------------------*
      *              * Subroutine di avanzamento pagina                *
      *              *-------------------------------------------------*
           perform   z-qry-pag-adv-000    thru z-qry-pag-adv-999      .
      *              *-------------------------------------------------*
      *              * Test se interruzione forzata da operatore       *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     go to z-qry-rou-pri-900.
       z-qry-rou-pri-400.
      *              *-------------------------------------------------*
      *              * Test se linee residue sufficienti               *
      *              *-------------------------------------------------*
           perform   z-qry-lin-res-000    thru z-qry-lin-res-999      .
           if        v-mrk                not  = spaces
                     move   spaces        to   v-mrk
                     go to  z-qry-rou-pri-200.
      *              *-------------------------------------------------*
      *              * Memorizzazione mark-point eventuale             *
      *              *-------------------------------------------------*
           perform   z-qry-mem-mpn-000   thru z-qry-mem-mpn-999       .
      *              *-------------------------------------------------*
      *              * Riga di dettaglio                               *
      *              *-------------------------------------------------*
           perform   z-qry-rig-det-000    thru z-qry-rig-det-999      .
           if        v-key                =    spaces
                     go to z-qry-rou-pri-100
           else      go to z-qry-rou-pri-900.
       z-qry-rou-pri-800.
           move      "ME"                 to   v-ope                  .
           move      "Nessuna registrazione entro i limiti impostati"
                                          to   v-not                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
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
                     go to z-qry-pag-adv-500.
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata da operatore        *
      *                  *---------------------------------------------*
           if        v-key                =    "EXIT"
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
      *                          * Se fine ciclo interrogazione        *
      *                          *-------------------------------------*
           if        v-mrk                not  = spaces
                     move  "EXIT"         to   v-key
                     go to z-qry-pag-adv-999.
      *                          *-------------------------------------*
      *                          * Se rientro a ciclo interrogazione   *
      *                          *-------------------------------------*
           move      spaces               to   v-key                  .
           go to     z-qry-pag-adv-000.
       z-qry-pag-adv-500.
      *              *-------------------------------------------------*
      *              * Intestazione video                              *
      *              *-------------------------------------------------*
           perform   z-qry-int-vid-000    thru z-qry-int-vid-999      .
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

      *================================================================*
      *       Routines variabili                                       *
      *================================================================*

      *================================================================*
      *       Routine pre-esecuzione programma                         *
      *                                                                *
      *       Uscita  : OK = spaces : continua l'esecuzione            *
      *                      "#"    : terminazione programma           *
      *----------------------------------------------------------------*
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
      *              *-------------------------------------------------*
      *              * Se piano dei conti a due livelli si esce        *
      *              *-------------------------------------------------*
           if        w-prs-liv-pdc        not  = 3
                     move   zero          to   w-ipc-cod-mas
                     go to  z-pre-exe-pgm-999.
      *              *-------------------------------------------------*
      *              * Determinazione eventuale variabile di i.p.c.    *
      *              * relativa al codice mastro                       *
      *              *-------------------------------------------------*
           move      "GV"                 to   s-ope                  .
           move      "cod-mas"            to   s-var                  .
           move      "-"                  to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                not  = spaces
                     move   zero          to   w-ipc-cod-mas
                     go to  z-pre-exe-pgm-999
           else      move   s-num         to   w-ipc-cod-mas          .
      *              *-------------------------------------------------*
      *              * Determinazione descrizione mastro nel caso la   *
      *              * variabile suddetta abbia valore positivo        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Apertura [zma]                              *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofzma"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zma                 .
      *                  *---------------------------------------------*
      *                  * Lettura tabella mastri                      *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODMAS    "         to   f-key                  .
           move      w-ipc-cod-mas        to   rf-zma-cod-mas         .
           move      "pgm/cge/fls/ioc/obj/iofzma"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zma                 .
      *
           if        f-sts                =    e-not-err
                     move  rf-zma-des-mas to   w-ric-des-mas
           else      move  zero           to   w-ipc-cod-mas          .
      *                  *---------------------------------------------*
      *                  * Chiusura [zma]                              *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofzma"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zma                 .
       z-pre-exe-pgm-999.
           exit.

      *================================================================*
      *       Preparazione tipo funzionamento programma                *
      *----------------------------------------------------------------*
       z-pre-tip-fun-000.
      *              *-------------------------------------------------*
      *              * Si/No richieste ad utente                       *
      *              *                                                 *
      *              * S : Verra' eseguita la routine ric-prm-000/999  *
      *              * N : Esecuzione immediata senza richieste        *
      *              *-------------------------------------------------*
           if        w-prs-liv-pdc        =    3
                     if    w-ipc-cod-mas  =    zero
                           move  "S"      to   w-fun-ric
                     else  move  "N"      to   w-fun-ric
                           move  w-ipc-cod-mas
                                          to   w-ric-cod-mas
           else      move  "N"            to   w-fun-ric              .
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

      *================================================================*
      *       Azzeramento iniziale work-area richieste                 *
      *                                                                *
      *       A disposizione y-flg-prm-gir : S = Primo giro            *
      *                                      N = Giro successivo al    *
      *                                          primo                 *
      *----------------------------------------------------------------*
       z-azz-ini-ric-000.
           move      zero                 to   w-ric-cod-mas          .
       z-azz-ini-ric-999.
           exit.

      *================================================================*
      *       Impostazione richieste per interrogazione                *
      *                                                                *
      *       Uscita  : OK = spaces : continua l'esecuzione            *
      *                      "#"    : terminazione programma           *
      *----------------------------------------------------------------*
       z-imp-ric-int-000.
      *              *-------------------------------------------------*
      *              * Visualizzazione prompts per impostazione        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Apertura [zma]                              *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofzma"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zma                 .
      *                  *---------------------------------------------*
      *                  * Prompt 'Codice mastro'                      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      16                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice mastro  :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       z-imp-ric-int-100.
      *              *-------------------------------------------------*
      *              * Accettazione codice mastro                      *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      "9B"                 to   v-edm                  .
           move      12                   to   v-lin                  .
           move      18                   to   v-pos                  .
           move      "EXIT"               to   v-pfk(1)               .
           move      "FIND"               to   v-pfk(2)               .
           move      "DO  "               to   v-pfk(3)               .
           move      w-ric-cod-mas        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-num                to   w-ric-cod-mas          .
       z-imp-ric-int-200.
      *                  *---------------------------------------------*
      *                  * Se premuta function-key 'EXIT' si esce      *
      *                  *---------------------------------------------*
           if        v-key                =    "EXIT"
                     move   "#"           to   OK
                     go to  z-imp-ric-int-900.
           if        v-key                =    "FIND"
                     go to  z-imp-ric-int-500.
       z-imp-ric-int-300.
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione mastro          *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODMAS    "         to   f-key                  .
           move      w-ric-cod-mas        to   rf-zma-cod-mas         .
           move      "pgm/cge/fls/ioc/obj/iofzma"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zma                 .
      *
           if        f-sts                =    e-not-err
                     move  rf-zma-des-mas to   w-ric-des-mas
           else      move  all "."        to   w-ric-des-mas          .
      *                      *-----------------------------------------*
      *                      * Visualizzazione descrizione             *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      24                   to   v-pos                  .
           move      w-ric-des-mas        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Test di congruenza impostazione             *
      *                  *---------------------------------------------*
           if        f-sts                not  =  e-not-err
                     go to  z-imp-ric-int-100.
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     z-imp-ric-int-900. 
       z-imp-ric-int-500.
      *                  *---------------------------------------------*
      *                  * Se premuta function-key 'FIND' si esegue un *
      *                  * ciclo di interrogazione sui mastri          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se programma di interrogazione     *
      *                      * gia' attivo                             *
      *                      *-----------------------------------------*
           move      "P?"                 to   s-ope                  .
           move      "pcge0110"           to   s-pro                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-liv                not  = zero
                     go to  z-imp-ric-int-100.
      *                      *-----------------------------------------*
      *                      * Preparazione variabile di i.p.c. per    *
      *                      * possibilita' di function-key "SLCT"     *
      *                      * durante l'interrogazione                *
      *                      *-----------------------------------------*
           move      "PV"                 to   s-ope                  .
           move      "fkselect"           to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      04                   to   s-car                  .
           move      "SLCT"               to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                      *-----------------------------------------*
      *                      * Richiamo programma di interrogazione    *
      *                      *-----------------------------------------*
           move      "pgm/cge/prg/obj/pcge0110"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat                                            .
           cancel    s-pat                                            .
      *                      *-----------------------------------------*
      *                      * Estrazione di eventuale variabile i.p.c.*
      *                      *-----------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "select mas"         to   s-var                  .
           move      "+"                  to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                not  = spaces
                     go to  z-imp-ric-int-100.
           move      s-num                to   w-ric-cod-mas          .
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      "9B"                 to   v-edm                  .
           move      12                   to   v-lin                  .
           move      18                   to   v-pos                  .
           move      w-ric-cod-mas        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           go to     z-imp-ric-int-300.
       z-imp-ric-int-900.
      *                  *---------------------------------------------*
      *                  * Chiusura [zma]                              *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofzma"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zma                 .
       z-imp-ric-int-999.
           exit.

      *================================================================*
      *       Regolarizzazione richieste per interrogazione            *
      *                                                                *
      *       A disposizione routine z-reg-cam-alf-000/999 per la re-  *
      *       golarizzazione campo alfanumerico con padding di "z".    *
      *       Variabile di input/output per suddetta routine:          *
      *       - y-reg-alf (max 20 caratteri)                           *
      *----------------------------------------------------------------*
       z-reg-ric-int-000.
       z-reg-ric-int-999.
           exit.

      *================================================================*
      *       Determinazione function-keys previste in Mark-points     *
      *----------------------------------------------------------------*
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

      *================================================================*
      *       Open files                                               *
      *----------------------------------------------------------------*
       z-qry-opn-fls-000.
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
       z-qry-opn-fls-999.
           exit.

      *================================================================*
      *       Close files                                              *
      *----------------------------------------------------------------*
       z-qry-cls-fls-000.
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
       z-qry-cls-fls-999.
           exit.

      *================================================================*
      *       Start iniziale - se non valida v-mrk:= "*"               *
      *----------------------------------------------------------------*
       z-qry-str-ini-000.
      *              *-------------------------------------------------*
      *              * Start                                           *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "CODCON    "         to   f-key                  .
      *
           if        w-prs-liv-pdc        =    3
                     move  w-ric-cod-mas  to   rf-zcn-cod-mas
           else      move  zero           to   rf-zcn-cod-mas         .
      *
           move      zero                 to   rf-zcn-cod-con         .
           move      "pgm/cge/fls/ioc/obj/iofzcn"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zcn                 .
      *              *-------------------------------------------------*
      *              * Test su successo operazione                     *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     move   "*"           to   v-mrk                  .
       z-qry-str-ini-999.
           exit.

      *================================================================*
      *       Lettura sequenziale - se fine v-mrk:= "*"                *
      *----------------------------------------------------------------*
       z-qry-let-seq-000.
      *              *-------------------------------------------------*
      *              * Tipo operazione: Read Next                      *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofzcn"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zcn                 .
      *              *-------------------------------------------------*
      *              * Test se 'At End'                                *
      *              *-------------------------------------------------*
           if        f-sts                =    e-end-fil
                     move   "*"           to   v-mrk                  .
       z-qry-let-seq-999.
           exit.

      *================================================================*
      *       Test superamento limiti massimi - se si' v-mrk:= "*"     *
      *----------------------------------------------------------------*
       z-qry-tst-max-000.
           if        w-prs-liv-pdc        not  = 3
                     go to z-qry-tst-max-999.
           if        rf-zcn-cod-mas       not  = w-ric-cod-mas
                     move   "*"           to   v-mrk                  .
       z-qry-tst-max-999.
           exit.

      *================================================================*
      *       Selezione su record letto - se da ignorare v-mrk:= "*"   *
      *----------------------------------------------------------------*
       z-qry-sel-rec-000.
           if        w-prs-liv-pdc        not  = 3
                     go to z-qry-sel-rec-999.
           if        rf-zcn-cod-mas       not  = w-ric-cod-mas
                     move   "*"           to   v-mrk                  .
       z-qry-sel-rec-999.
           exit.

      *================================================================*
      *    Intestazione video                                          *
      *----------------------------------------------------------------*
       z-qry-int-vid-000.
      *              *-------------------------------------------------*
      *              * Trattini a linea 01                             *
      *              *-------------------------------------------------*
           move      "WR"                 to   v-ope                  .
           move      all    "="           to   v-cnt                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Titolo : 'CONTI DEL PIANO DEI CONTI'            *
      *              *-------------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      25                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      28                   to   v-pos                  .
           move      "CONTI DEL PIANO DEI CONTI"
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
      *              * Se piano dei conti a due livelli non si visua-  *
      *              * lizzano codice e descrizione mastro             *
      *              *-------------------------------------------------*
           if        w-prs-liv-pdc        not  = 3
                     go to  z-qry-int-vid-100.
      *              *-------------------------------------------------*
      *              * Visualizzazione codice e descrizione mastro     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Literal 'Mastro :'                          *
      *                  *---------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      11                   to   v-pos                  .
           move      "Mastro :"           to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Codice mastro                               *
      *                  *---------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      v-lnr                to   v-lin                  .
           move      20                   to   v-pos                  .
           move      w-ric-cod-mas        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Descrizione mastro                          *
      *                  *---------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      26                   to   v-pos                  .
           move      w-ric-des-mas        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       z-qry-int-vid-100.
      *              *-------------------------------------------------*
      *              * Titolo di suddivisione campi                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Parametri fissi                             *
      *                  *---------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      v-lnr                to   v-lin                  .
      *                  *---------------------------------------------*
      *                  * Literal 'Conto'                             *
      *                  *---------------------------------------------*
           move      05                   to   v-car                  .
           move      17                   to   v-pos                  .
           move      "Conto"              to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Literal 'Descrizione'                       *
      *                  *---------------------------------------------*
           move      11                   to   v-car                  .
           move      26                   to   v-pos                  .
           move      "Descrizione"        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Lineette                                        *
      *              *-------------------------------------------------*
           move      "WR"                 to   v-ope                  .
           move      all    "-"           to   v-cnt                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       z-qry-int-vid-999.
           exit.

      *================================================================*
      *    Test se righe residue sufficienti - se no v-mrk:= "*"       *
      *    A disposizione : v-res = numero linee ancora disponibili    *
      *    Preparare      : v-mrk = "*" se numero linee insufficiente  *
      *----------------------------------------------------------------*
       z-qry-lin-res-000.
      *              *-------------------------------------------------*
      *              * Sufficiente una riga residua                    *
      *              *-------------------------------------------------*
           if        v-res                <    1
                     move   "#"           to   v-mrk                  .
       z-qry-lin-res-999.
           exit.

      *================================================================*
      *    Memorizzazione mark-point eventuale                         *
      *    Preparare      : v-ope = "MK"                               *
      *                     v-pnr = posizione del mark-point           *
      *                     v-cnt = parametri associati al mark-point  *
      *    e poi richiamare la call "swd/mod/prg/obj/mvideo"           *
      *----------------------------------------------------------------*
       z-qry-mem-mpn-000.
       z-qry-mem-mpn-999.
           exit.

      *================================================================*
      *    Riga di dettaglio                                           *
      *                                                                *
      *    Preparare      : v-ope = "WR"                               *
      *                     v-cnt = immagine linea da scrivere         *
      *                                                                *
      *            oppure : v-ope = "LF"                               *
      *                                                                *
      *            oppure : v-ope = "VP"                               *
      *                     v-lnr = numero linea cui posizionarsi      *
      *                                                                *
      *    e poi richiamare la call "swd/mod/prg/obj/mvideo"           *
      *----------------------------------------------------------------*
       z-qry-rig-det-000.
      *              *-------------------------------------------------*
      *              * Codice conto                                    *
      *              *-------------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      v-lnr                to   v-lin                  .
           move      20                   to   v-pos                  .
           move      rf-zcn-cod-con       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Descrizione                                     *
      *              *-------------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      26                   to   v-pos                  .
           move      rf-zcn-des-con       to   v-alf                  .
      *                  *---------------------------------------------*
      *                  * Mark-Point                                  *
      *                  *---------------------------------------------*
           move      "+"                  to   v-edm                  .
           move      rf-zcn-cod-con       to   w-mpn-cod-con          .
           move      w-mpn                to   v-cnt                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Interlinea                                      *
      *              *-------------------------------------------------*
           move      "LF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       z-qry-rig-det-999.
           exit.

      *================================================================*
      *    Trattamento tasto di funzione selezionato da utente         *
      *    A disposizione : v-key = function-key selezionata           *
      *                     v-cnt = parametri associati al mark-point  *
      *                     v-lin = mark-point line     01-21          *
      *                     v-pos = mark-point position 01-80          *
      *    Preparare      : v-mrk = "*" se poi si deve interrompere il *
      *                                 ciclo di interrogazione        *
      *----------------------------------------------------------------*
       z-qry-trt-fun-000.
      *              *-------------------------------------------------*
      *              * Function-key 'SLCT'                             *
      *              *-------------------------------------------------*
           if        v-key                not  = "SLCT"
                     go to  z-qry-trt-fun-500.
      *                  *---------------------------------------------*
      *                  * Put della variabile di i.p.c.: 'select con' *
      *                  *---------------------------------------------*
           move      "PV"                 to   s-ope                  .
           move      "select con"         to   s-var                  .
           move      "-"                  to   s-dop                  .
           move      "N"                  to   s-tip                  .
           move      02                   to   s-car                  .
           move      zero                 to   s-dec                  .
           move      spaces               to   s-sgn                  .
           move      v-cnt                to   w-mpn                  .
           move      w-mpn-cod-con        to   s-num                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Segnale di interruzione interrogazione      *
      *                  *---------------------------------------------*
           move      "*"                  to   v-mrk                  .
      *                  *---------------------------------------------*
      *                  * Flag di richieste a No                      *
      *                  *---------------------------------------------*
           move      "N"                  to   w-fun-ric              .
      *                  *---------------------------------------------*
      *                  * Flag di funzionamento ciclico a No          *
      *                  *---------------------------------------------*
           move      "N"                  to   w-fun-cic              .
           go to     z-qry-trt-fun-999.
       z-qry-trt-fun-500.
      *              *-------------------------------------------------*
      *              * Function-key 'INSR'                             *
      *              *-------------------------------------------------*
           if        v-key                not  = "INSR"
                     go to  z-qry-trt-fun-999.
      *              *-------------------------------------------------*
      *              * Test se programma di gestione file gia' attivo  *
      *              *-------------------------------------------------*
           move      "P?"                 to   s-ope                  .
           move      "pcge2000"           to   s-pro                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-liv                not  = zero
                     go to  z-qry-trt-fun-999.
      *                  *---------------------------------------------*
      *                  * Richiamo programma di gestione archivio     *
      *                  *---------------------------------------------*
           move      "pgm/cge/prg/obj/pcge2000"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat                                            .
           cancel    s-pat                                            .
       z-qry-trt-fun-999.
           exit.
