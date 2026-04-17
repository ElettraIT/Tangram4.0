       Identification Division.
       Program-Id.                                 mbckgv             .
      *================================================================*
      *                                                                *
      * Visualizzazione errori di esecuzione del rullino messaggi      *
      *                                                                *
      * ============================================================== *
      *                                                                *
      * Tipi operazione previsti :                                     *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Visual      Visualizzazione errori .                           *
      *                                                                *
      *             Input  : b-ope = "VE"                              *
      *                                                                *
      *                      b-tfe = Tipo di funzionamento del pro-    *
      *                              gramma di esecuzione              *
      *                              - F : Foreground                  *
      *                              - B : Background                  *
      *                                                                *
      *                      b-chr = Descrizione del programma ter-    *
      *                              minato, solo se parametro pre-    *
      *                              cedente a "F"                     *
      *                                                                *
      *             Output : nessuno                                   *
      *                                                                *
      *             -------------------------------------------------- *
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
      *    * Area per definizione codici di errore di i-o              *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/e"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "mvideo"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/v"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "mmessg"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/m"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "msegrt"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/s"                                  .

      *    *===========================================================*
      *    * Work-area locale                                          *
      *    *-----------------------------------------------------------*
       01  w.
      *        *-------------------------------------------------------*
      *        * Work per parametri di selezione stampa da ultima e-   *
      *        * secuzione di 'mprint'                                 *
      *        *-------------------------------------------------------*
           05  w-psm.
      *            *---------------------------------------------------*
      *            * Ultimo tipo di stampa selezionato, preparato dal  *
      *            * modulo 'mprint' e gestito poi da 'mbckgv'         *
      *            *  - I      : Immediata                             *
      *            *  - D      : Su disco                              *
      *            *  - V      : A video                               *
      *            *---------------------------------------------------*
               10  w-psm-uts              pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Numero di print-file relativo all'ultimo tipo di  *
      *            * stampa selezionato, solo se su disco o a video,   *
      *            * preparato dal modulo 'mprint' e gestito poi dal   *
      *            * modulo 'mbckgv'                                   *
      *            *---------------------------------------------------*
               10  w-psm-upf              pic  9(12)                  .
      *        *-------------------------------------------------------*
      *        * Status di uscita tra routines                         *
      *        *-------------------------------------------------------*
           05  w-sts                      pic  x(02)                  .
      *        *-------------------------------------------------------*
      *        * Tipo funzionamento del programma di esecuzione        *
      *        *-------------------------------------------------------*
           05  w-tfe                      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma di esecuzione               *
      *        *-------------------------------------------------------*
           05  w-dpe                      pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma di esecuzione allineata a   *
      *        * sinistra                                              *
      *        *-------------------------------------------------------*
           05  w-dps                      pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Puntatore per string-unstring                         *
      *        *-------------------------------------------------------*
           05  w-pnt                      pic  9(05)                  .

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Area di comunicazione per moduli                "mbckgx"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/b"                                  .

      ******************************************************************
       Procedure Division                using b                      .
      ******************************************************************

      *    *===========================================================*
      *    * Main program                                              *
      *    *-----------------------------------------------------------*
       main-000.
      *              *-------------------------------------------------*
      *              * Visualizzazione errori                          *
      *              *-------------------------------------------------*
           if        b-ope                =    "VE"
                     perform vis-err-000  thru vis-err-999            .
       main-999.
           exit      program.

      *    *===========================================================*
      *    * Visualizzazione errori                                    *
      *    *-----------------------------------------------------------*
       vis-err-000.
      *              *-------------------------------------------------*
      *              * Se visualizzazione di foreground                *
      *              *-------------------------------------------------*
           if        b-tfe                not  = "B"
                     perform vis-erf-000  thru vis-erf-999
      *              *-------------------------------------------------*
      *              * Se visualizzazione di background                *
      *              *-------------------------------------------------*
           else      perform vis-erb-000  thru vis-erb-999            .
       vis-err-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione errori di foreground                      *
      *    *-----------------------------------------------------------*
       vis-erf-000.
      *              *-------------------------------------------------*
      *              * Memorizzazione tipo funzionamento               *
      *              *-------------------------------------------------*
           move      "F"                  to   w-tfe                  .
      *              *-------------------------------------------------*
      *              * Memorizzazione descrizione del programma di e-  *
      *              * secuzione                                       *
      *              *-------------------------------------------------*
           move      b-chr                to   w-dpe                  .
           move      spaces               to   w-dps                  .
           move      zero                 to   w-pnt                  .
           inspect   w-dpe            tallying w-pnt
                     for               leading spaces                 .
           add       1                    to   w-pnt                  .
           unstring  w-dpe                into w-dps
                                  with pointer w-pnt                  .
      *              *-------------------------------------------------*
      *              * Richiesta parametri di tipo stampa memorizzati  *
      *              * dall'ultima esecuzione di 'mprint', e loro me-  *
      *              * morizzazione                                    *
      *              *-------------------------------------------------*
           move      "T<"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-alf                to   w-psm-uts              .
           move      s-num                to   w-psm-upf              .
      *              *-------------------------------------------------*
      *              * Test se esiste almeno un messaggio di errore    *
      *              *-------------------------------------------------*
           perform   vis-ert-000          thru vis-ert-999            .
      *              *-------------------------------------------------*
      *              * Se errore grave durante il test                 *
      *              *-------------------------------------------------*
           if        w-sts                =    "##"
                     go to vis-erf-500.
      *              *-------------------------------------------------*
      *              * Se nessun messaggio da visualizzare             *
      *              *-------------------------------------------------*
           if        w-sts                =    e-end-fil
                     go to vis-erf-500.
      *              *-------------------------------------------------*
      *              * Visualizzazione messaggi                        *
      *              *-------------------------------------------------*
           perform   qry-msg-000          thru qry-msg-999            .
       vis-erf-500.
      *              *-------------------------------------------------*
      *              * Visualizzazione print-file se stampa a video    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se l'ultima stampa non e' stata a video :   *
      *                  * nessuna azione                              *
      *                  *---------------------------------------------*
           if        w-psm-uts            not  = "V"
                     go to vis-erf-900.
      *                  *---------------------------------------------*
      *                  * Se il numero ultimo print-file e' a zero :  *
      *                  * nessuna azione                              *
      *                  *---------------------------------------------*
           if        w-psm-upf            =    zero
                     go to vis-erf-900.
      *                  *---------------------------------------------*
      *                  * Preparazione variabile di i.p.c. per il ri- *
      *                  * chiamo del sottoprogramma di gestione dei   *
      *                  * print-files                                 *
      *                  *---------------------------------------------*
           move      "PV"                 to   s-ope                  .
           move      "pxpg0006"           to   s-var                  .
           move      "G"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      80                   to   s-car                  .
           move      zero                 to   s-dec                  .
           move      spaces               to   s-sgn                  .
           move      w-psm                to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Richiamo del sottoprogramma di gestione dei *
      *                  * print-files                                 *
      *                  *---------------------------------------------*
           call      "swd/xpg/prg/obj/pxpg0006"                       .
           cancel    "swd/xpg/prg/obj/pxpg0006"                       .
       vis-erf-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     vis-erf-999.
       vis-erf-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione errori di background                      *
      *    *-----------------------------------------------------------*
       vis-erb-000.
      *              *-------------------------------------------------*
      *              * Memorizzazione tipo funzionamento               *
      *              *-------------------------------------------------*
           move      "B"                  to   w-tfe                  .
      *              *-------------------------------------------------*
      *              * Memorizzazione descrizione del programma di e-  *
      *              * secuzione                                       *
      *              *-------------------------------------------------*
           move      "IG"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-alf                to   w-dpe                  .
           move      spaces               to   w-dps                  .
           move      zero                 to   w-pnt                  .
           inspect   w-dpe            tallying w-pnt
                     for               leading spaces                 .
           add       1                    to   w-pnt                  .
           unstring  w-dpe                into w-dps
                                  with pointer w-pnt                  .
      *              *-------------------------------------------------*
      *              * Test se esiste almeno un messaggio di errore    *
      *              *-------------------------------------------------*
           perform   vis-ert-000          thru vis-ert-999            .
      *              *-------------------------------------------------*
      *              * Se errore grave durante il test                 *
      *              *-------------------------------------------------*
           if        w-sts                =    "##"
                     go to vis-erb-900.
      *              *-------------------------------------------------*
      *              * Se nessun messaggio da visualizzare             *
      *              *-------------------------------------------------*
           if        w-sts                not  = e-end-fil
                     go to vis-erb-500.
      *                  *---------------------------------------------*
      *                  * Box segnalazione terminazione corretta      *
      *                  *---------------------------------------------*
           perform   box-tok-000          thru box-tok-999            .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     vis-erb-900.
       vis-erb-500.
      *              *-------------------------------------------------*
      *              * Se messaggi di errore da visualizzare           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Box segnalazione terminazione con errori    *
      *                  *---------------------------------------------*
           perform   box-ter-000          thru box-ter-999            .
      *                  *---------------------------------------------*
      *                  * Visualizzazione messaggi                    *
      *                  *---------------------------------------------*
           perform   qry-msg-000          thru qry-msg-999            .
       vis-erb-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     vis-erb-999.
       vis-erb-999.
           exit.

      *    *===========================================================*
      *    * Test se esiste almeno un messaggio di errore              *
      *    *-----------------------------------------------------------*
       vis-ert-000.
      *              *-------------------------------------------------*
      *              * Open input file messaggi                        *
      *              *-------------------------------------------------*
           if        w-tfe                =    "B"
                     move  "IB"           to   m-ope
           else      move  "IF"           to   m-ope                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
      *              *-------------------------------------------------*
      *              * Se errore : uscita con status a "##"            *
      *              *-------------------------------------------------*
           if        m-rsc                not  = spaces
                     perform err-fms-000  thru err-fms-999
                     move    "##"         to   w-sts
                     go to   vis-ert-900.
      *              *-------------------------------------------------*
      *              * Lettura riga di messaggio                       *
      *              *-------------------------------------------------*
           move      "RD"                 to   m-ope                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
      *              *-------------------------------------------------*
      *              * Se errore diverso da e-end-fil : uscita con     *
      *              * status a "##"                                   *
      *              *-------------------------------------------------*
           if        m-rsc                not  = e-end-fil and
                     m-rsc                not  = spaces
                     perform err-fms-000  thru err-fms-999
                     move    "##"         to   w-sts
                     go to   vis-ert-900.
      *              *-------------------------------------------------*
      *              * Se errore uguale a e-end-fil                    *
      *              *-------------------------------------------------*
           if        m-rsc                =    spaces
                     go to vis-ert-500.
      *                  *---------------------------------------------*
      *                  * Chiusura file messaggi                      *
      *                  *---------------------------------------------*
           move      "CI"                 to   m-ope                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
      *                      *-----------------------------------------*
      *                      * Se errori di chiusura                   *
      *                      *-----------------------------------------*
           if        m-rsc                not  = spaces
                     perform err-fms-000  thru err-fms-999
                     move    "##"         to   w-sts
                     go to   vis-ert-900.
      *                      *-----------------------------------------*
      *                      * Altrimenti                              *
      *                      *-----------------------------------------*
           move      e-end-fil            to   w-sts                  .
           go to     vis-ert-900.
       vis-ert-500.
      *              *-------------------------------------------------*
      *              * Se nessun errore, cioe' esiste almeno una riga  *
      *              * di messaggi                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Chiusura file messaggi                      *
      *                  *---------------------------------------------*
           move      "CI"                 to   m-ope                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
      *                      *-----------------------------------------*
      *                      * Se errori di chiusura                   *
      *                      *-----------------------------------------*
           if        m-rsc                not  = spaces
                     perform err-fms-000  thru err-fms-999
                     display m-rsc line 03 position 01
                     move    "##"         to   w-sts
                     go to   vis-ert-900.
      *                      *-----------------------------------------*
      *                      * Altrimenti                              *
      *                      *-----------------------------------------*
           move      spaces               to   w-sts                  .
       vis-ert-900.
      *              *-------------------------------------------------*
      *              * Cancellazione modulo "mmessg"                   *
      *              *-------------------------------------------------*
           cancel    "swd/mod/prg/obj/mmessg"                         .
       vis-ert-999.
           exit.

      *    *===========================================================*
      *    * Box per terminazione senza errori                         *
      *    *-----------------------------------------------------------*
       box-tok-000.
      *              *-------------------------------------------------*
      *              * Video in Off                                    *
      *              *-------------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Box da linea 07 pos 06 a linea 18 pos 75        *
      *              *-------------------------------------------------*
           move      "BX"                 to   v-ope                  .
           move      07                   to   v-lin                  .
           move      06                   to   v-pos                  .
           move      18                   to   v-lto                  .
           move      75                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Messaggi all'interno del box                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Titolo box                                  *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      36                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      23                   to   v-pos                  .
           move      "Terminazione programma in background"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Sottolineatura titolo box                   *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      36                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      23                   to   v-pos                  .
           move      "------------------------------------"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Literal per il programma                    *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      14                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      08                   to   v-pos                  .
           move      "- Programma : "     to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Programma                                   *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      22                   to   v-pos                  .
           move      w-dps                to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Literal per l'esito                         *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      14                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      08                   to   v-pos                  .
           move      "- Esito     : "     to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Esito                                       *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      22                   to   v-pos                  .
           move      "Nessuna anomalia da segnalare           "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Dicitura di presa visione                   *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      34                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      40                   to   v-pos                  .
           move      "Digitare OK per presa visione :   "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Video in On                                     *
      *              *-------------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Accettazione OK di presa visione                *
      *              *-------------------------------------------------*
           move      spaces               to   v-alf                  .
       box-tok-500.
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      72                   to   v-pos                  .
           move      "DO  "               to   v-pfk (05)             .
           move      "EXIT"               to   v-pfk (20)             .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           if        v-key                =    "DO  " or
                     v-key                =    "EXIT"
                     go to box-tok-999.
           if        v-alf                not  = "OK"
                     go to box-tok-500.
       box-tok-999.
           exit.

      *    *===========================================================*
      *    * Box per terminazione con errori                           *
      *    *-----------------------------------------------------------*
       box-ter-000.
      *              *-------------------------------------------------*
      *              * Video in Off                                    *
      *              *-------------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Box da linea 07 pos 06 a linea 18 pos 75        *
      *              *-------------------------------------------------*
           move      "BX"                 to   v-ope                  .
           move      07                   to   v-lin                  .
           move      06                   to   v-pos                  .
           move      18                   to   v-lto                  .
           move      75                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Messaggi all'interno del box                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Titolo box                                  *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      36                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      23                   to   v-pos                  .
           move      "Terminazione programma in background"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Sottolineatura titolo box                   *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      36                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      23                   to   v-pos                  .
           move      "------------------------------------"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Literal per il programma                    *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      14                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      08                   to   v-pos                  .
           move      "- Programma : "     to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Programma                                   *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      22                   to   v-pos                  .
           move      w-dps                to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Literal per l'esito                         *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      14                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      08                   to   v-pos                  .
           move      "- Esito     : "     to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Esito                                       *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      45                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      22                   to   v-pos                  .
           move      "Attenzione : Ci sono messaggi di esecuzione !"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Dicitura di presa visione                   *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      34                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      40                   to   v-pos                  .
           move      "Digitare OK per presa visione :   "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Video in On                                     *
      *              *-------------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Accettazione OK di presa visione                *
      *              *-------------------------------------------------*
           move      spaces               to   v-alf                  .
       box-ter-500.
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      72                   to   v-pos                  .
           move      "DO  "               to   v-pfk (05)             .
           move      "EXIT"               to   v-pfk (20)             .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           if        v-key                =    "DO  " or
                     v-key                =    "EXIT"
                     go to box-ter-999.
           if        v-alf                not  = "OK"
                     go to box-ter-500.
       box-ter-999.
           exit.

      *    *===========================================================*
      *    *  Routine di interrogazione su file messaggi               *
      *    *-----------------------------------------------------------*
       qry-msg-000.
      *              *-------------------------------------------------*
      *              * Determinazione function-keys in Mark-points     *
      *              *-------------------------------------------------*
           move      "PRNT"               to   v-pfk(01)              .
           move      spaces               to   v-pfk(02)              .
           move      spaces               to   v-pfk(03)              .
           move      spaces               to   v-pfk(04)              .
           move      spaces               to   v-pfk(05)              .
           move      spaces               to   v-pfk(06)              .
           move      spaces               to   v-pfk(07)              .
           move      spaces               to   v-pfk(08)              .
           move      spaces               to   v-pfk(09)              .
           move      spaces               to   v-pfk(10)              .
      *              *-------------------------------------------------*
      *              * Begin Automatico                                *
      *              *-------------------------------------------------*
           move      "BA"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Open file messaggi                              *
      *              *-------------------------------------------------*
           if        w-tfe                =    "B"
                     move  "IB"           to   m-ope
           else      move  "IF"           to   m-ope                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
      *              *-------------------------------------------------*
      *              * Se errore su open                               *
      *              *-------------------------------------------------*
           if        m-rsc                =    spaces
                     go to qry-msg-300.
      *                  *---------------------------------------------*
      *                  * Messaggio di errore                         *
      *                  *---------------------------------------------*
           perform   err-fms-000          thru err-fms-999            .
      *                  *---------------------------------------------*
      *                  * Close file messaggi                         *
      *                  *---------------------------------------------*
           move      "CI"                 to   m-ope                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
      *                  *---------------------------------------------*
      *                  * Cancellazione modulo "mmessg"               *
      *                  *---------------------------------------------*
           cancel    "swd/mod/prg/obj/mmessg"                         .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     qry-msg-900.
       qry-msg-300.
      *              *-------------------------------------------------*
      *              * Lettura riga di messaggio                       *
      *              *-------------------------------------------------*
           move      "RD"                 to   m-ope                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
      *              *-------------------------------------------------*
      *              * Se nessun errore : continuazione                *
      *              *-------------------------------------------------*
           if        m-rsc                =    spaces
                     go to qry-msg-500.
      *              *-------------------------------------------------*
      *              * Se fine file                                    *
      *              *-------------------------------------------------*
           if        m-rsc                not  = e-end-fil
                     go to qry-msg-400.
      *                  *---------------------------------------------*
      *                  * Close file messaggi                         *
      *                  *---------------------------------------------*
           move      "CI"                 to   m-ope                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
      *                  *---------------------------------------------*
      *                  * Cancellazione modulo "mmessg"               *
      *                  *---------------------------------------------*
           cancel    "swd/mod/prg/obj/mmessg"                         .
      *                  *---------------------------------------------*
      *                  * Funzione Stop per interrogazione            *
      *                  *---------------------------------------------*
           move      "ST"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda se zero pagine visua-  *
      *                  * lizate o no                                 *
      *                  *---------------------------------------------*
           if        v-pag                =    zero
                     go to qry-msg-900
           else      go to qry-msg-600.
       qry-msg-400.
      *              *-------------------------------------------------*
      *              * Se errore grave                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Messaggio di errore                         *
      *                  *---------------------------------------------*
           perform   err-fms-000          thru err-fms-999            .
      *                  *---------------------------------------------*
      *                  * Close file messaggi                         *
      *                  *---------------------------------------------*
           move      "CI"                 to   m-ope                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
      *                  *---------------------------------------------*
      *                  * Cancellazione modulo "mmessg"               *
      *                  *---------------------------------------------*
           cancel    "swd/mod/prg/obj/mmessg"                         .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     qry-msg-900.
       qry-msg-500.
      *              *-------------------------------------------------*
      *              * Test se primo passaggio                         *
      *              *-------------------------------------------------*
           if        v-pag                not  = zero
                     go to qry-msg-700.
       qry-msg-600.
      *              *-------------------------------------------------*
      *              * Subroutine di avanzamento pagina                *
      *              *-------------------------------------------------*
           perform   qry-adp-000          thru qry-adp-999            .
      *              *-------------------------------------------------*
      *              * Test se interruzione forzata da operatore       *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     go to qry-msg-900.
       qry-msg-700.
      *              *-------------------------------------------------*
      *              * Test se linee residue sufficienti               *
      *              *-------------------------------------------------*
           if        v-res                not  > 1
                     go to  qry-msg-600.
      *              *-------------------------------------------------*
      *              * Riga di dettaglio                               *
      *              *-------------------------------------------------*
           perform   qry-det-000          thru qry-det-999            .
      *              *-------------------------------------------------*
      *              * Riciclo a lettura messaggio successivo          *
      *              *-------------------------------------------------*
           go to     qry-msg-300.
       qry-msg-900.
      *              *-------------------------------------------------*
      *              * End                                             *
      *              *-------------------------------------------------*
           move      "EN"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       qry-msg-999.
           exit.

      *    *===========================================================*
      *    *  Subroutine di avanzamento pagina in interrogazione       *
      *    *-----------------------------------------------------------*
       qry-adp-000.
      *              *-------------------------------------------------*
      *              * Avanzamento pagina                              *
      *              *-------------------------------------------------*
           move      "PA"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       qry-adp-100.
      *              *-------------------------------------------------*
      *              * Deviazione secondo la function-key              *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     go to qry-adp-200
           else if   v-key                =    "PRNT"
                     go to qry-adp-300
           else      go to qry-adp-400.
       qry-adp-200.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     qry-adp-999.
       qry-adp-300.
      *              *-------------------------------------------------*
      *              * Se Prnt                                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Stampa errori rullino messaggi              *
      *                  *---------------------------------------------*
           move      "PE"                 to   b-ope                  .
           move      w-tfe                to   b-tfe                  .
           move      w-dpe                to   b-chr                  .
           call      "swd/mod/prg/obj/mbckgp"
                                         using b                      .
           cancel    "swd/mod/prg/obj/mbckgp"                         .
      *                  *---------------------------------------------*
      *                  * Normalizzazione function-key                *
      *                  *---------------------------------------------*
           move      spaces               to   v-key                  .
      *                  *---------------------------------------------*
      *                  * Riciclo ad avanzamento pagina               *
      *                  *---------------------------------------------*
           go to     qry-adp-000.
       qry-adp-400.
      *              *-------------------------------------------------*
      *              * Se ne' Exit ne' Prnt                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Trattini a linea 01                         *
      *                  *---------------------------------------------*
           move      "WR"                 to   v-ope                  .
           move      all    "="           to   v-cnt                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Descrizione programma terminato             *
      *                  *---------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      21                   to   v-pos                  .
           move      w-dpe                to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Dicitura per lista errori                   *
      *                  *---------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      61                   to   v-pos                  .
           move      "            Messaggi"
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
      *                  * Trattini a linea 03                         *
      *                  *---------------------------------------------*
           move      "WR"                 to   v-ope                  .
           move      all    "="           to   v-cnt                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       qry-adp-999.
           exit.

      *    *===========================================================*
      *    *  Subroutine di Riga di dettaglio in interrogazione        *
      *    *-----------------------------------------------------------*
       qry-det-000.
      *                  *---------------------------------------------*
      *                  * Riga del messaggio                          *
      *                  *---------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      01                   to   v-pos                  .
           move      m-msg                to   v-alf                  .
           move      "+"                  to   v-edm                  .
           move      "*"                  to   v-cnt                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       qry-det-999.
           exit.

      *    *===========================================================*
      *    * Box per terminazione con errore di i-o su file messaggi   *
      *    *-----------------------------------------------------------*
       err-fms-000.
      *              *-------------------------------------------------*
      *              * Video in Off                                    *
      *              *-------------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Box da linea 07 pos 06 a linea 18 pos 75        *
      *              *-------------------------------------------------*
           move      "BX"                 to   v-ope                  .
           move      07                   to   v-lin                  .
           move      06                   to   v-pos                  .
           move      18                   to   v-lto                  .
           move      75                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Messaggi all'interno del box                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Titolo box                                  *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      36                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      23                   to   v-pos                  .
           move      "Terminazione programma in background"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Sottolineatura titolo box                   *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      36                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      23                   to   v-pos                  .
           move      "------------------------------------"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Literal per il programma                    *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      14                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      08                   to   v-pos                  .
           move      "- Programma : "     to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Programma                                   *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      22                   to   v-pos                  .
           move      w-dps                to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Literal per l'esito                         *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      14                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      08                   to   v-pos                  .
           move      "- Esito     : "     to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Esito                                       *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      22                   to   v-pos                  .
           move      "ERRORE GRAVE IN ANALISI MESSAGGI !!     "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Dicitura di presa visione                   *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      34                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      40                   to   v-pos                  .
           move      "Digitare OK per presa visione :   "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Video in On                                     *
      *              *-------------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Accettazione OK di presa visione                *
      *              *-------------------------------------------------*
           move      spaces               to   v-alf                  .
       err-fms-500.
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      72                   to   v-pos                  .
           move      "DO  "               to   v-pfk (05)             .
           move      "EXIT"               to   v-pfk (20)             .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           if        v-key                =    "DO  " or
                     v-key                =    "EXIT"
                     go to err-fms-999.
           if        v-alf                not  = "OK"
                     go to err-fms-500.
       err-fms-999.
           exit.
