       Identification Division.
       Program-Id.                                 pbfo9000           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    bfo                 *
      *                                Settore:    pul                 *
      *                                   Fase:    bfo900              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 05/10/96    *
      *                       Ultima revisione:    NdK del 05/08/23    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Pulizia bolle fornitori chiuse              *
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
                     "bfo"                                            .
      *        *-------------------------------------------------------*
      *        * Settore gestionale                                    *
      *        *-------------------------------------------------------*
           05  i-ide-set                  pic  x(03) value
                     "pul"                                            .
      *        *-------------------------------------------------------*
      *        * Fase gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-fas                  pic  x(06) value
                     "bfo900"                                         .
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10) value
                     "pbfo9000"                                       .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  i-ide-des                  pic  x(40) value
                     "   PULIZIA MOVIMENTI FORNITORI CHIUSI   "       .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "mvideo" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/v"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "msegrt"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/s"                                  .

      *    *===========================================================*
      *    * Area per definizione codici di errore di i-o              *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/e"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per moduli di input-output          *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/f"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per moduli                 "mbckgx" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/b"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "mmessg" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/m"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per moduli di input-output su files *
      *    * di tipo line sequential                                   *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/g"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "mopsys" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/o"                                  .

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
      *        * Area di controllo per funzionamento pul-routine       *
      *        *-------------------------------------------------------*
           05  w-cnt-pul.
      *            *---------------------------------------------------*
      *            * Flag di uscita da subroutines principali          *
      *            *---------------------------------------------------*
               10  w-cnt-pul-flg-sub      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Literals                                              *
      *        *-------------------------------------------------------*
           05  w-cnt-lit.
               10  w-cnt-lit-t80          pic  x(80) value all "="    .

      *    *===========================================================*
      *    * Record files                                              *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [bft]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/bfo/fls/rec/rfbft"                          .
      *        *-------------------------------------------------------*
      *        * [bfr]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/bfo/fls/rec/rfbfr"                          .
      *        *-------------------------------------------------------*
      *        * [bfx]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/bfo/fls/rec/rfbfx"                          .

      *    *===========================================================*
      *    * Work-area richieste per pulizie                           *
      *    *-----------------------------------------------------------*
       01  rr.
      *        *-------------------------------------------------------*
      *        * Data massima impostabile, a seconda della lettura     *
      *        * delle numerazioni e delle personalizzazioni           *
      *        *-------------------------------------------------------*
           05  rr-dat-max                 pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Data massima effettiva, fino alla quale vengono pu-   *
      *        * liti i documenti.                                     *
      *        *                                                       *
      *        * Inizialmente viene proposta la data di fine eserci-   *
      *        * zio relativa all'esercizio precedente quello attua-   *
      *        * le.                                                   *
      *        *-------------------------------------------------------*
           05  rr-dat-pul                 pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Flag indicatore di sottoposizione a cancellazione     *
      *        * di documenti non chiusi                               *
      *        *                                                       *
      *        *  - N : No                                             *
      *        *  - S : Si                                             *
      *        *                                                       *
      *        * Inizialmente viene proposto il valore 'N'.            *
      *        *-------------------------------------------------------*
           05  rr-snx-cnc                 pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Conferma esecuzione                                   *
      *        *-------------------------------------------------------*
           05  rr-cnf-ese                 pic  x(01)                  .

      *    *===========================================================*
      *    * Work per accettazioni campi espansi                       *
      *    *-----------------------------------------------------------*
       01  w-exp.
      *        *-------------------------------------------------------*
      *        * Work per : Si/No cancellazione                        *
      *        *-------------------------------------------------------*
           05  w-exp-snx-cnc.
               10  w-exp-snx-cnc-num      pic  9(02)       value 2    .
               10  w-exp-snx-cnc-lun      pic  9(02)       value 02   .
               10  w-exp-snx-cnc-tbl.
                   15  filler             pic  x(02) value "No"       .
                   15  filler             pic  x(02) value "Si"       .

      *    *===========================================================*
      *    * Work-area contatori                                       *
      *    *-----------------------------------------------------------*
       01  w-ctr-rec.
           05  w-ctr-rec-let              pic  9(09)                  .
           05  w-ctr-rec-eli              pic  9(09)                  .
           05  w-ctr-rec-scr              pic  9(09)                  .
           05  w-ctr-rec-cen              pic  9(02)                  .
           05  w-ctr-rec-dec              pic  9(01)                  .

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

      *    *===========================================================*
      *    * Work per subroutine pul-put-app-000/999                   *
      *    *-----------------------------------------------------------*
       01  w-pul-put-app.
           05  w-pul-put-app-int          pic  x(01)                  .
           05  w-pul-put-app-ctr          pic  9(05)                  .

      *    *===========================================================*
      *    * File area generica                                        *
      *    *-----------------------------------------------------------*
       01  f-xxx.
           05  f-xxx-nam                  pic  x(04)   value "bft "   .
           05  f-xxx-pfo                  pic  x(40)                  .

      *    *===========================================================*
      *    * Work per subroutines di Det                               *
      *    *-----------------------------------------------------------*
       01  w-det.
      *        *-------------------------------------------------------*
      *        * Work per Det numero records                           *
      *        *-------------------------------------------------------*
           05  w-det-rec-fil.
               10  w-det-rec-fil-flg      pic  x(01)                  .
               10  w-det-rec-fil-alf.
                   15  w-det-rec-fil-tst  pic  x(13)                  .
                   15  w-det-rec-fil-ler  pic  x(05)                  .
                   15  w-det-rec-fil-dat  pic  x(13)                  .
               10  w-det-rec-fil-rec      pic  9(11)                  .
               10  w-det-rec-fil-s15      pic s9(15)                  .
               10  w-det-rec-fil-v02      pic s9(03)                  .
               10  w-det-rec-fil-tpt      pic  x(40)                  .

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
      *              * Open files per richieste                        *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-rou-opn-fls      .
           perform   rou-opn-fls-000      thru rou-opn-fls-999        .
           if        w-cnt-rou-opn-fls    not  = spaces
                     go to main-900.
      *              *-------------------------------------------------*
      *              * Accettazione richieste di selezione             *
      *              *-------------------------------------------------*
           perform   acc-ric-sel-000      thru acc-ric-sel-999        .
      *              *-------------------------------------------------*
      *              * Close files                                     *
      *              *-------------------------------------------------*
           perform   rou-cls-fls-000      thru rou-cls-fls-999        .
      *                  *---------------------------------------------*
      *                  * Se uscita per Exit                          *
      *                  *---------------------------------------------*
           if        w-cnt-acc-ric-sel    =    "E"
                     go to main-900.
      *              *-------------------------------------------------*
      *              * Inizializzazione rullino messaggi               *
      *              *-------------------------------------------------*
           perform   ini-rul-msg-000      thru ini-rul-msg-999        .
      *                  *---------------------------------------------*
      *                  * Se errore : uscita                          *
      *                  *---------------------------------------------*
            if       m-rsc                not  = spaces
                     go to main-900.
      *              *-------------------------------------------------*
      *              * Messaggio di elaborazione in corso              *
      *              *-------------------------------------------------*
           perform   msg-ela-crs-000      thru msg-ela-crs-999        .
      *              *-------------------------------------------------*
      *              * Esecuzione programma di pulizia                 *
      *              *-------------------------------------------------*
           perform   pul-rou-pri-000      thru pul-rou-pri-999        .
      *              *-------------------------------------------------*
      *              * Messaggio di fine elaborazione                  *
      *              *-------------------------------------------------*
           perform   msg-fin-ela-000      thru msg-fin-ela-999        .
       main-900.
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
      *    * Inizializzazione rullino messaggi                         *
      *    *-----------------------------------------------------------*
       ini-rul-msg-000.
      *              *-------------------------------------------------*
      *              * Inizializzazione rullino messaggi di foreground *
      *              *-------------------------------------------------*
           move      "OF"                 to   m-ope                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
       ini-rul-msg-999.
           exit.

      *    *===========================================================*
      *    * Messaggio per elaborazione in corso                       *
      *    *-----------------------------------------------------------*
       msg-ela-crs-000.
      *              *-------------------------------------------------*
      *              * Messaggio di programma in esecuzione            *
      *              *-------------------------------------------------*
           move      "PE"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Prompt per contatori elementi                   *
      *              *-------------------------------------------------*
           perform   pmt-ctr-ele-000      thru pmt-ctr-ele-999        .
       msg-ela-crs-999.
           exit.

      *    *===========================================================*
      *    * Messaggio per fine elaborazione                           *
      *    *-----------------------------------------------------------*
       msg-fin-ela-000.
      *              *-------------------------------------------------*
      *              * Messaggi a video                                *
      *              *-------------------------------------------------*
       msg-fin-ela-120.
      *                  *---------------------------------------------*
      *                  * Composizione riga                           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Editing data di pulizia                 *
      *                      *-----------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      rr-dat-pul           to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * String riga                             *
      *                      *-----------------------------------------*
           move      spaces               to   m-msg                  .
           string    "Pulizia documenti fino al .: "
                                delimited by   size
                     v-edt      delimited by   spaces
                                          into m-msg                  .
      *                  *---------------------------------------------*
      *                  * Scrittura riga                              *
      *                  *---------------------------------------------*
           move      "WR"                 to   m-ope                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
       msg-fin-ela-130.
      *                  *---------------------------------------------*
      *                  * Composizione riga                           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Dati da segreteria                      *
      *                      *-----------------------------------------*
           move      "IG"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                      *-----------------------------------------*
      *                      * String riga                             *
      *                      *-----------------------------------------*
           move      spaces               to   m-msg                  .
           string    "Utente ....................: "
                                delimited by   size
                     s-ute      delimited by   spaces
                                          into m-msg                  .
      *                  *---------------------------------------------*
      *                  * Scrittura riga                              *
      *                  *---------------------------------------------*
           move      "WR"                 to   m-ope                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
       msg-fin-ela-140.
      *                  *---------------------------------------------*
      *                  * Composizione riga                           *
      *                  *---------------------------------------------*
           move      all "-"              to   m-msg                  .
      *                  *---------------------------------------------*
      *                  * Scrittura riga                              *
      *                  *---------------------------------------------*
           move      "WR"                 to   m-ope                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
       msg-fin-ela-220.
      *                  *---------------------------------------------*
      *                  * Composizione riga                           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Editing contatore elementi letti        *
      *                      *-----------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      09                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BG"                to   v-edm                  .
           move      w-ctr-rec-let        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * String riga                             *
      *                      *-----------------------------------------*
           move      spaces               to   m-msg                  .
           string    "Elementi letti ............: "
                                delimited by   size
                     v-edt      delimited by   spaces
                                          into m-msg                  .
      *                  *---------------------------------------------*
      *                  * Scrittura riga                              *
      *                  *---------------------------------------------*
           move      "WR"                 to   m-ope                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
       msg-fin-ela-230.
      *                  *---------------------------------------------*
      *                  * Composizione riga                           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Editing contatore elementi eliminati    *
      *                      *-----------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      09                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BG"                to   v-edm                  .
           move      w-ctr-rec-eli        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * String riga                             *
      *                      *-----------------------------------------*
           move      spaces               to   m-msg                  .
           string    "Elementi eliminati ........: "
                                delimited by   size
                     v-edt      delimited by   spaces
                                          into m-msg                  .
      *                  *---------------------------------------------*
      *                  * Scrittura riga                              *
      *                  *---------------------------------------------*
           move      "WR"                 to   m-ope                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
       msg-fin-ela-240.
      *                  *---------------------------------------------*
      *                  * Composizione riga                           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Editing contatore elementi riportati    *
      *                      *-----------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      09                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BG"                to   v-edm                  .
           move      w-ctr-rec-scr        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * String riga                             *
      *                      *-----------------------------------------*
           move      spaces               to   m-msg                  .
           string    "Elementi riportati ........: "
                                delimited by   size
                     v-edt      delimited by   spaces
                                          into m-msg                  .
      *                  *---------------------------------------------*
      *                  * Composizione riga                           *
      *                  *---------------------------------------------*
           move      "WR"                 to   m-ope                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
       msg-fin-ela-250.
      *                  *---------------------------------------------*
      *                  * Composizione riga                           *
      *                  *---------------------------------------------*
           move      all "-"              to   m-msg                  .
      *                  *---------------------------------------------*
      *                  * Scrittura riga                              *
      *                  *---------------------------------------------*
           move      "WR"                 to   m-ope                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
       msg-fin-ela-300.
      *                  *---------------------------------------------*
      *                  * Composizione riga                           *
      *                  *---------------------------------------------*
           if        w-pul-put-app-int    =    spaces
                     go to msg-fin-ela-800.
           move      "ATTENZIONE : elaborazione interrotta - Gli archivi
      -              " rimangono invariati."
                                          to   m-msg                  .
      *                  *---------------------------------------------*
      *                  * Scrittura riga                              *
      *                  *---------------------------------------------*
           move      "WR"                 to   m-ope                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
       msg-fin-ela-800.
      *                  *---------------------------------------------*
      *                  * Visualizzazione del rullino messaggi        *
      *                  *---------------------------------------------*
           move      "VE"                 to   b-ope                  .
           move      "F"                  to   b-tfe                  .
           move      i-ide-des            to   b-chr                  .
           call      "swd/mod/prg/obj/mbckgv"
                                         using b                      .
           cancel    "swd/mod/prg/obj/mbckgv"                         .
       msg-fin-ela-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     msg-fin-ela-999.
       msg-fin-ela-999.
           exit.

      *    *===========================================================*
      *    * Pulizie : Routine principale                              *
      *    *-----------------------------------------------------------*
       pul-rou-pri-000.
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
           perform   pul-opn-fls-000      thru pul-opn-fls-999        .
           if        w-cnt-pul-flg-sub    not  = spaces
                     go to pul-rou-pri-999.
      *              *-------------------------------------------------*
      *              * Open files di appoggio per pulizia e riorganiz- *
      *              * zazione                                         *
      *              *-------------------------------------------------*
           perform   pul-opn-app-000      thru pul-opn-app-999        .
      *              *-------------------------------------------------*
      *              * Start iniziale                                  *
      *              *-------------------------------------------------*
           perform   pul-str-ini-000      thru pul-str-ini-999        .
           if        w-cnt-pul-flg-sub    not  = spaces
                     move   spaces        to   w-cnt-pul-flg-sub
                     go to  pul-rou-pri-900.
       pul-rou-pri-200.
      *              *-------------------------------------------------*
      *              * Lettura sequenziale                             *
      *              *-------------------------------------------------*
           perform   pul-let-seq-000      thru pul-let-seq-999        .
           if        w-cnt-pul-flg-sub    not  = spaces
                     move   spaces        to   w-cnt-pul-flg-sub
                     go to  pul-rou-pri-900.
      *              *-------------------------------------------------*
      *              * Test se superamento limiti massimi              *
      *              *-------------------------------------------------*
           perform   pul-tst-max-000      thru pul-tst-max-999        .
           if        w-cnt-pul-flg-sub    not  = spaces
                     move   spaces        to   w-cnt-pul-flg-sub
                     go to  pul-rou-pri-900.
      *              *-------------------------------------------------*
      *              * Test se record da eliminare                     *
      *              *-------------------------------------------------*
           perform   pul-sel-rec-000      thru pul-sel-rec-999        .
           if        w-cnt-pul-flg-sub    not  = spaces
                     move   spaces        to   w-cnt-pul-flg-sub
                     go to  pul-rou-pri-200.
      *              *-------------------------------------------------*
      *              * Scrittura record in file di appoggio            *
      *              *-------------------------------------------------*
           perform   pul-put-app-000      thru pul-put-app-999        .
      *              *-------------------------------------------------*
      *              * Riciclo                                         *
      *              *-------------------------------------------------*
           go to     pul-rou-pri-200.
       pul-rou-pri-900.
      *              *-------------------------------------------------*
      *              * Close files                                     *
      *              *-------------------------------------------------*
           perform   pul-cls-fls-000      thru pul-cls-fls-999        .
      *              *-------------------------------------------------*
      *              * Close files di appoggio per pulizia e riorga-   *
      *              * nizzazione                                      *
      *              *-------------------------------------------------*
           perform   pul-cls-app-000      thru pul-cls-app-999        .
      *              *-------------------------------------------------*
      *              * Rename sostitutivo per far si' che i file di    *
      *              * appoggio per pulizia/riorganizzazione sostitui- *
      *              * scano i files originali                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se esecuzione interrotta               *
      *                  *---------------------------------------------*
           if        w-pul-put-app-int    not  = spaces
                     go to pul-rou-pri-999.
      *                  *---------------------------------------------*
      *                  * Rename                                      *
      *                  *---------------------------------------------*
           perform   pul-ren-sos-000      thru pul-ren-sos-999        .
       pul-rou-pri-999.
           exit.

      *    *===========================================================*
      *    * Routine pre-esecuzione programma                          *
      *    *-----------------------------------------------------------*
       pre-exe-pgm-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-pre-exe-pgm      .
      *              *-------------------------------------------------*
      *              * Visualizzazione titolo programma                *
      *              *-------------------------------------------------*
           perform   vis-tit-pgm-000      thru vis-tit-pgm-999        .
      *              *-------------------------------------------------*
      *              * Determinazione data massima per pulizie         *
      *              *-------------------------------------------------*
           move      "DT"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-dat                to   rr-dat-max             .
       pre-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Open files per richieste                                  *
      *    *-----------------------------------------------------------*
       rou-opn-fls-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-rou-opn-fls      .
       rou-opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Close files per richieste                                 *
      *    *-----------------------------------------------------------*
       rou-cls-fls-000.
       rou-cls-fls-999.
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
      *                  * Conferma esecuzione programma               *
      *                  *---------------------------------------------*
           perform   acc-cnf-ese-000      thru acc-cnf-ese-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
       acc-ric-sel-200.
      *                  *---------------------------------------------*
      *                  * Data pulizia                                *
      *                  *---------------------------------------------*
           perform   acc-dat-pul-000      thru acc-dat-pul-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-100.
       acc-ric-sel-300.
      *                  *---------------------------------------------*
      *                  * Si/No cancellazione ordini non chiusi       *
      *                  *---------------------------------------------*
           perform   acc-snx-cnc-000      thru acc-snx-cnc-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-200.
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
           move      "#CNF"               to   v-not                  .
           move      spaces               to   v-alf                  .
           move      "SNE"                to   v-msk                  .
           move      "DO  "               to   v-pfk (05)             .
           move      "UP  "               to   v-pfk (01)             .
           move      "EXIT"               to   v-pfk (20)             .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           if        v-key                not  = spaces
                     go to acc-ric-sel-920.
           if        v-alf                =    "S"
                     move   "DO  "        to   v-key
           else if   v-alf                =    "E"
                     move   "EXIT"        to   v-key
           else if   v-alf                =    "N"
                     move   "UP  "        to   v-key                  .
       acc-ric-sel-920.
      *                  *---------------------------------------------*
      *                  * Test su risposta dell'utente                *
      *                  *---------------------------------------------*
           if        v-key                =    "DO  "
                     go to acc-ric-sel-930
           else if   v-key                =    "EXIT"
                     go to acc-ric-sel-940
           else if   v-key                =    "UP  "
                     go to acc-ric-sel-950
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
       acc-ric-sel-950.
      *                  *---------------------------------------------*
      *                  * Se Up                                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Ad accettazioni                         *
      *                      *-----------------------------------------*
           go to     acc-ric-sel-100.
       acc-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per richieste di selezione        *
      *    *-----------------------------------------------------------*
       pmt-ric-sel-000.
      *              *-------------------------------------------------*
      *              * Data pulizia                                    *
      *              *-------------------------------------------------*
           perform   pmt-dat-pul-000      thru pmt-dat-pul-999        .
      *              *-------------------------------------------------*
      *              * Si/No cancellazione ordini non chiusi           *
      *              *-------------------------------------------------*
           perform   pmt-snx-cnc-000      thru pmt-snx-cnc-999        .
      *              *-------------------------------------------------*
      *              * Nota bene                                       *
      *              *-------------------------------------------------*
           perform   pmt-not-ben-000      thru pmt-not-ben-999        .
       pmt-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts : Data pulizia                    *
      *    *-----------------------------------------------------------*
       pmt-dat-pul-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      68                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Pulizia documenti con data di registrazione fino a
      -              "l .............. :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-dat-pul-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts : Si/No cancellazione ordini non  *
      *    *                           chiusi                          *
      *    *-----------------------------------------------------------*
       pmt-snx-cnc-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      68                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Cancellazione anche di documenti non chiusi ......
      -              "................ :" to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-snx-cnc-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts : Nota bene                       *
      *    *-----------------------------------------------------------*
       pmt-not-ben-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "N.B.: Il programma cancella solo i documenti conso
      -              "lidati, cioe' chiusi, fino al-"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "      la data richiesta.                          
      -              "                              "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-not-ben-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts : Contatori elementi              *
      *    *-----------------------------------------------------------*
       pmt-ctr-ele-000.
      *              *-------------------------------------------------*
      *              * Linea di trattini                               *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      all "-"              to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Contatore 1                                     *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Elementi letti ............:"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Determinazione numero di record                 *
      *              *-------------------------------------------------*
           perform   det-rec-fil-000      thru det-rec-fil-999        .
      *              *-------------------------------------------------*
      *              * Contatore elementi totali                       *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      17                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      54                   to   v-pos                  .
           move      "Elementi totali :"  to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Elementi totali                                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      09                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BG"                to   v-edm                  .
           move      18                   to   v-lin                  .
           move      72                   to   v-pos                  .
           move      w-det-rec-fil-rec    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Contatore 2                                     *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Elementi eliminati ........:"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Contatore 3                                     *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      21                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Elementi riportati ........:"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-ctr-ele-999.
           exit.

      *    *===========================================================*
      *    * Accettazione Box di conferma esecuzione                   *
      *    *-----------------------------------------------------------*
       acc-cnf-ese-000.
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
           move      04                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      21                   to   v-lto                  .
           move      80                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Attenzione                                      *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      78                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      02                   to   v-pos                  .
           move      "     =============================================
      -              "=======================     "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      78                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      02                   to   v-pos                  .
           move      "     ***                     A T T E N Z I O N E  
      -              "                    ***     "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      78                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      02                   to   v-pos                  .
           move      "     +============================================
      -              "======================+     "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Messaggio                                       *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      78                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      02                   to   v-pos                  .
           move      "     |                                            
      -              "                      |     "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      78                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      02                   to   v-pos                  .
           move      "     |             QUESTA OPERAZIONE E' IRREVERSIB
      -              "ILE !!!               |     "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      78                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      02                   to   v-pos                  .
           move      "     |             -------------------------------
      -              "-------               |     "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      78                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      02                   to   v-pos                  .
           move      "     |           TUTTI GLI UTENTI DEVONO ESSERE SC
      -              "OLLEGATI              |     "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      78                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      02                   to   v-pos                  .
           move      "     |        I DATI CANCELLATI NON POSSONO ESSERE
      -              " RECUPERATI           |     "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      78                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      02                   to   v-pos                  .
           move      "     |        ------------------------------------
      -              "-----------           |     "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      78                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      02                   to   v-pos                  .
           move      "     |        SI VUOLE COMUNQUE PROCEDERE ALL' ESE
      -              "CUZIONE DEL           |     "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      78                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      02                   to   v-pos                  .
           move      "     |        PROGRAMMA?                          
      -              "                      |     "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      78                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      02                   to   v-pos                  .
           move      "     |                                            
      -              "                      |     "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      78                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      02                   to   v-pos                  .
           move      "     +============================================
      -              "======================+     "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-cnf-ese-200.
      *              *-------------------------------------------------*
      *              * Video in On                                     *
      *              *-------------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-cnf-ese-600.
      *              *-------------------------------------------------*
      *              * Conferma                                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Default per accettazione                    *
      *                  *---------------------------------------------*
           move      spaces               to   rr-cnf-ese             .
       acc-cnf-ese-650.
      *                  *---------------------------------------------*
      *                  * Visualizzazione prompt per accettazione     *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      55                   to   v-pos                  .
           move      "Conferma (S N E) ?  "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-cnf-ese-700.
      *                  *---------------------------------------------*
      *                  * Accettazione carattere di conferma          *
      *                  *---------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      74                   to   v-pos                  .
           move      "DO  "               to   v-pfk (05)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      rr-cnf-ese           to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-cnf-ese-725.
      *                  *---------------------------------------------*
      *                  * Test se function key                        *
      *                  *---------------------------------------------*
           if        v-key                =    "DO  "
                     move  "S"            to   v-alf
                     go to acc-cnf-ese-750.
           if        v-key                =    "UP  "
                     move  "N"            to   v-alf
                     go to acc-cnf-ese-750.
           if        v-key                =    "EXIT"
                     move  "E"            to   v-alf
                     go to acc-cnf-ese-750.
       acc-cnf-ese-750.
      *                  *---------------------------------------------*
      *                  * Valore impostato in campo di destinazione   *
      *                  *---------------------------------------------*
           move      v-alf                to   rr-cnf-ese             .
       acc-cnf-ese-775.
      *                  *---------------------------------------------*
      *                  * Controllo valore impostato                  *
      *                  *---------------------------------------------*
           if        rr-cnf-ese           not  = "S" and
                     rr-cnf-ese           not  = "N" and
                     rr-cnf-ese           not  = "E"
                     go to acc-cnf-ese-700.
       acc-cnf-ese-800.
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda della scelta           *
      *                  *---------------------------------------------*
           if        rr-cnf-ese           =    "S"
                     go to acc-cnf-ese-825
           else if   rr-cnf-ese           =    "N"
                     go to acc-cnf-ese-850
           else      go to acc-cnf-ese-875.
       acc-cnf-ese-825.
      *                  *---------------------------------------------*
      *                  * Se scelta 'S'                               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     acc-cnf-ese-950.
       acc-cnf-ese-850.
      *                  *---------------------------------------------*
      *                  * Se scelta 'N'                               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Flag di uscita                          *
      *                      *-----------------------------------------*
           move      "E"                  to   w-cnt-acc-ric-sel      .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     acc-cnf-ese-950.
       acc-cnf-ese-875.
      *                  *---------------------------------------------*
      *                  * Se scelta 'E'                               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Flag di uscita                          *
      *                      *-----------------------------------------*
           move      "E"                  to   w-cnt-acc-ric-sel      .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     acc-cnf-ese-950.
       acc-cnf-ese-950.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ripristino immagine video                   *
      *                  *---------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-cnf-ese-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo selezione : Data pulizia               *
      *    *-----------------------------------------------------------*
       acc-dat-pul-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-dat-pul-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      06                   to   v-lin                  .
           move      70                   to   v-pos                  .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-dat-pul           to   v-dat                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-dat-pul-999.
       acc-dat-pul-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-dat                to   rr-dat-pul             .
       acc-dat-pul-300.
      *              *-------------------------------------------------*
      *              * Se Find                                         *
      *              *-------------------------------------------------*
           if        v-key                not  = "FIND"
                     go to acc-dat-pul-400.
      *                  *---------------------------------------------*
      *                  * Find su bolle                               *
      *                  *---------------------------------------------*
           perform   fnd-arc-bft-000      thru fnd-arc-bft-999        .
      *                  *---------------------------------------------*
      *                  * A reimpostazione                            *
      *                  *---------------------------------------------*
           go to     acc-dat-pul-100.
       acc-dat-pul-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
           if        rr-dat-pul           >   rr-dat-max
                     go to acc-dat-pul-100.
           if        rr-dat-pul           =   zero
                     go to acc-dat-pul-100.
       acc-dat-pul-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-dat-pul-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-dat-pul-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-dat-pul-100.
       acc-dat-pul-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo selezione : Si/No cancellazione ordini *
      *    *                                non chiusi                 *
      *    *-----------------------------------------------------------*
       acc-snx-cnc-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-snx-cnc-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-snx-cnc-lun    to   v-car                  .
           move      w-exp-snx-cnc-num    to   v-ldt                  .
           move      "NS#"                to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      09                   to   v-lin                  .
           move      70                   to   v-pos                  .
           move      w-exp-snx-cnc-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           if        rr-snx-cnc           =    "N"
                     move  01             to   v-num
           else if   rr-snx-cnc           =    "S"
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-snx-cnc-999.
       acc-snx-cnc-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        v-num                =    01
                     move  "N"            to   rr-snx-cnc
           else if   v-num                =    02
                     move  "S"            to   rr-snx-cnc
           else      move  spaces         to   rr-snx-cnc             .
       acc-snx-cnc-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
           if        rr-snx-cnc           not  = "N" and
                     rr-snx-cnc           not  = "S"
                     go to acc-snx-cnc-100.
       acc-snx-cnc-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-snx-cnc-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-snx-cnc-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-snx-cnc-100.
       acc-snx-cnc-999.
           exit.

      *    *===========================================================*
      *    * Controllo su tasto Do in parametri di selezione           *
      *    *-----------------------------------------------------------*
       tdo-ric-sel-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-tdo-ric-flg      .
       tdo-ric-sel-100.
      *              *-------------------------------------------------*
      *              * Test su data pulizia                            *
      *              *-------------------------------------------------*
       tdo-ric-sel-110.
           if        rr-dat-pul           not  = zero
                     go to tdo-ric-sel-120.
           move      "Manca la data di pulizia !                        
      -              "               "    to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-120.
           if        rr-dat-pul           not  > rr-dat-max
                     go to tdo-ric-sel-200.
           move      "Data pulizia superiore al massimo consentito !    
      -              "               "    to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-200.
       tdo-ric-sel-300.
      *              *-------------------------------------------------*
      *              * Uscita per controlli superati                   *
      *              *-------------------------------------------------*
           go to     tdo-ric-sel-999.
       tdo-ric-sel-900.
      *              *-------------------------------------------------*
      *              * Se errore                                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Messaggio di errore                         *
      *                  *---------------------------------------------*
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                  *---------------------------------------------*
      *                  * Flag di errore                              *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cnt-tdo-ric-flg      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     tdo-ric-sel-999.
       tdo-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione richieste di selezione                    *
      *    *-----------------------------------------------------------*
       nor-ric-sel-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Data massima effettiva                      *
      *                  *---------------------------------------------*
           move      zero                 to   rr-dat-pul             .
      *                  *---------------------------------------------*
      *                  * Si/No cancellazione documenti non chiusi    *
      *                  *---------------------------------------------*
           move      spaces               to   rr-snx-cnc             .
       nor-ric-sel-100.
      *              *-------------------------------------------------*
      *              * Preparazione valori di default                  *
      *              *-------------------------------------------------*
       nor-ric-sel-300.
      *                  *---------------------------------------------*
      *                  * Data effettiva pulizia                      *
      *                  *                                             *
      *                  * Se la data attuale e' entro i primi 6 mesi  *
      *                  * dell'anno : data di default = fine anno di  *
      *                  * due anni prima, altrimenti fine anno prece- *
      *                  * dente                                       *
      *                  *---------------------------------------------*
           move      rr-dat-max           to   s-dat                  .
           if        s-mes                >    6
                     subtract 1           from s-saa
                     move     12          to   s-mes
                     move     31          to   s-gio
                     move     s-dat       to   rr-dat-pul
           else      subtract 2           from s-saa
                     move     12          to   s-mes
                     move     31          to   s-gio
                     move     s-dat       to   rr-dat-pul             .
       nor-ric-sel-400.
      *                  *---------------------------------------------*
      *                  * Si/No cancellazione documenti non chiusi    *
      *                  *---------------------------------------------*
           move      "N"                  to   rr-snx-cnc             .
       nor-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Pulizie : Open files                                      *
      *    *-----------------------------------------------------------*
       pul-opn-fls-000.
      *              *-------------------------------------------------*
      *              * [bft]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofbft"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bft                 .
      *              *-------------------------------------------------*
      *              * [bfr]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofbfr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bfr                 .
      *              *-------------------------------------------------*
      *              * [bfx]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofbfx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bfx                 .
       pul-opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Pulizie : Close files                                     *
      *    *-----------------------------------------------------------*
       pul-cls-fls-000.
      *              *-------------------------------------------------*
      *              * [bft]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofbft"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bft                 .
      *              *-------------------------------------------------*
      *              * [bfr]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofbfr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bfr                 .
      *              *-------------------------------------------------*
      *              * [bfx]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofbfx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bfx                 .
       pul-cls-fls-999.
           exit.

      *    *===========================================================*
      *    * Pulizie : Open files di appoggio                          *
      *    *-----------------------------------------------------------*
       pul-opn-app-000.
      *              *-------------------------------------------------*
      *              * [bft]                                           *
      *              *-------------------------------------------------*
           move      "Op"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofbft"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bft                 .
      *              *-------------------------------------------------*
      *              * [bfr]                                           *
      *              *-------------------------------------------------*
           move      "Op"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofbfr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bfr                 .
      *              *-------------------------------------------------*
      *              * [bfx]                                           *
      *              *-------------------------------------------------*
           move      "Op"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofbfx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bfx                 .
       pul-opn-app-999.
           exit.

      *    *===========================================================*
      *    * Pulizie : Close files di appoggio                         *
      *    *-----------------------------------------------------------*
       pul-cls-app-000.
      *              *-------------------------------------------------*
      *              * [bft]                                           *
      *              *-------------------------------------------------*
           move      "Cp"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofbft"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bft                 .
      *              *-------------------------------------------------*
      *              * [bfr]                                           *
      *              *-------------------------------------------------*
           move      "Cp"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofbfr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bfr                 .
      *              *-------------------------------------------------*
      *              * [bfx]                                           *
      *              *-------------------------------------------------*
           move      "Cp"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofbfx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bfx                 .
       pul-cls-app-999.
           exit.

      *    *===========================================================*
      *    * Pulizie : Rename sostitutivo file di appoggio             *
      *    *-----------------------------------------------------------*
       pul-ren-sos-000.
      *              *-------------------------------------------------*
      *              * [bft]                                           *
      *              *-------------------------------------------------*
           move      "Xp"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofbft"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bft                 .
      *              *-------------------------------------------------*
      *              * [bfr]                                           *
      *              *-------------------------------------------------*
           move      "Xp"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofbfr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bfr                 .
      *              *-------------------------------------------------*
      *              * [bfx]                                           *
      *              *-------------------------------------------------*
           move      "Xp"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofbfx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bfx                 .
       pul-ren-sos-999.
           exit.

      *    *===========================================================*
      *    * Pulizie : Start iniziale                                  *
      *    *-----------------------------------------------------------*
       pul-str-ini-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-pul-flg-sub      .
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione            *
      *              *-------------------------------------------------*
           move      spaces               to   w-pul-put-app-int      .
      *              *-------------------------------------------------*
      *              * Azzeramento contatori records                   *
      *              *-------------------------------------------------*
           move      zero                 to   w-ctr-rec-let          .
           move      zero                 to   w-ctr-rec-eli          .
           move      zero                 to   w-ctr-rec-scr          .
      *              *-------------------------------------------------*
      *              * Nota per interruzione                           *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      23                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "[F4] - Per interrompere l'esecuzione              
      -              "                              "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Start su archivio [bft]                         *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "IDEDOC    "         to   f-key                  .
           move      zero                 to   rf-bft-dat-reg         .
           move      zero                 to   rf-bft-cod-dpz         .
           move      spaces               to   rf-bft-cod-tmb         .
           move      zero                 to   rf-bft-num-prt         .
           move      "pgm/bfo/fls/ioc/obj/iofbft"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bft                 .
      *              *-------------------------------------------------*
      *              * Se Start errata                                 *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     move  "#"            to   w-cnt-pul-flg-sub
                     go to pul-str-ini-999.
       pul-str-ini-999.
           exit.

      *    *===========================================================*
      *    * Interrogazione : Lettura sequenziale                      *
      *    *-----------------------------------------------------------*
       pul-let-seq-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-pul-flg-sub      .
       pul-let-seq-010.
      *              *-------------------------------------------------*
      *              * Eventuale interruzione                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Accettazione                                *
      *                  *---------------------------------------------*
           move      spaces               to   v-key                  .
           move      "AA"                 to   v-ope                  .
           move      21                   to   v-lin                  .
           move      80                   to   v-pos                  .
           move      "[4] "               to   v-pfk (16)             .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Se attesa disattivata : oltre               *
      *                  *---------------------------------------------*
           if        v-key                =    spaces
                     go to pul-let-seq-100.
      *                  *---------------------------------------------*
      *                  * Se 'Pf4'                                    *
      *                  *---------------------------------------------*
           if        v-key                not  = "[4] "
                     go to pul-let-seq-100.
      *                  *---------------------------------------------*
      *                  * Flags                                       *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cnt-pul-flg-sub      .
           move      "#"                  to   w-pul-put-app-int      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     pul-let-seq-999.
       pul-let-seq-100.
      *              *-------------------------------------------------*
      *              * Lettura sequenziale archivio [bft]              *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofbft"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bft                 .
      *                  *---------------------------------------------*
      *                  * Se fine file                                *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     move  "#"            to   w-cnt-pul-flg-sub
                     go to pul-let-seq-999.
       pul-let-seq-200.
      *              *-------------------------------------------------*
      *              * Incremento contatore records [bft] letti        *
      *              *-------------------------------------------------*
           perform   inc-rec-let-000      thru inc-rec-let-999        .
       pul-let-seq-999.
           exit.

      *    *===========================================================*
      *    * Interrogazione : Test se superamento limiti massimi       *
      *    *-----------------------------------------------------------*
       pul-tst-max-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-pul-flg-sub      .
       pul-tst-max-999.
           exit.

      *    *===========================================================*
      *    * Pulizie : Test se record da eliminare                     *
      *    *-----------------------------------------------------------*
       pul-sel-rec-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-pul-flg-sub      .
       pul-sel-rec-100.
      *              *-------------------------------------------------*
      *              * Test su data registrazione : se maggiore di     *
      *              * quella per pulizie il record e' sicuramente da  *
      *              * riportare                                       *
      *              *-------------------------------------------------*
           if        rf-bft-dat-reg       >    rr-dat-pul
                     go to pul-sel-rec-800.
       pul-sel-rec-200.
      *              *-------------------------------------------------*
      *              * Se pulizia anche di documenti non chiusi,       *
      *              * il movimento va' eliminato                      *
      *              *-------------------------------------------------*
           if        rr-snx-cnc           not  = "S"
                     go to pul-sel-rec-300.
           go to     pul-sel-rec-600.
       pul-sel-rec-300.
           if        rf-bft-flg-bch       =    spaces
                     go to pul-sel-rec-800.
       pul-sel-rec-600.
      *              *-------------------------------------------------*
      *              * In tutti gli altri casi il movimento e' da      *
      *              * eliminare                                       *
      *              *-------------------------------------------------*
           go to     pul-sel-rec-900.
       pul-sel-rec-800.
      *              *-------------------------------------------------*
      *              * Uscita per movimento da riportare               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Incremento contatore records [bft] scritti  *
      *                  *---------------------------------------------*
           perform   inc-rec-scr-000      thru inc-rec-scr-999        .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     pul-sel-rec-999.
       pul-sel-rec-900.
      *              *-------------------------------------------------*
      *              * Uscita per movimento da eliminare               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Flag di selezione non superata              *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cnt-pul-flg-sub      .
      *                  *---------------------------------------------*
      *                  * Incremento contatore records [bft] elimina- *
      *                  * ti                                          *
      *                  *---------------------------------------------*
           perform   inc-rec-eli-000      thru inc-rec-eli-999        .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     pul-sel-rec-999.
       pul-sel-rec-999.
           exit.

      *    *===========================================================*
      *    * Pulizie : Put record su file di appoggio per pulizia      *
      *    *-----------------------------------------------------------*
       pul-put-app-000.
      *              *-------------------------------------------------*
      *              * [bfr]                                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione contatore records [bfr]     *
      *                  * scritti                                     *
      *                  *---------------------------------------------*
           move      zero                 to   w-pul-put-app-ctr      .
      *                  *---------------------------------------------*
      *                  * Start su file [bfr]                         *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "NUMPRT    "         to   f-key                  .
           move      rf-bft-num-prt       to   rf-bfr-num-prt         .
           move      zero                 to   rf-bfr-num-prg         .
           move      "pgm/bfo/fls/ioc/obj/iofbfr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bfr                 .
      *                      *-----------------------------------------*
      *                      * Test su esito operazione                *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to pul-put-app-200.
       pul-put-app-100.
      *                  *---------------------------------------------*
      *                  * Lettura sequenziale record [bfr]            *
      *                  *---------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofbfr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bfr                 .
      *                      *-----------------------------------------*
      *                      * Test su esito operazione                *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to pul-put-app-200.
      *                  *---------------------------------------------*
      *                  * Test sul massimo                            *
      *                  *---------------------------------------------*
           if        rf-bfr-num-prt       not  = rf-bft-num-prt
                     go to pul-put-app-200.
      *                  *---------------------------------------------*
      *                  * Incremento contatore records [bfr] scritti  *
      *                  *---------------------------------------------*
           add       1                    to   w-pul-put-app-ctr      .
      *                  *---------------------------------------------*
      *                  * Put record in file di appoggio              *
      *                  *---------------------------------------------*
           move      "Pp"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofbfr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bfr                 .
      *                  *---------------------------------------------*
      *                  * Riciclo su lettura sequenziale record [bfr] *
      *                  *---------------------------------------------*
           go to     pul-put-app-100.
       pul-put-app-200.
      *              *-------------------------------------------------*
      *              * Test su contatore records [bfr] scritti : se a  *
      *              * zero si esce                                    *
      *              *-------------------------------------------------*
           if        w-pul-put-app-ctr    =    zero
                     go to pul-put-app-999.
       pul-put-app-300.
      *              *-------------------------------------------------*
      *              * [bfx]                                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Start su file [bfx]                         *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "NUMPRT    "         to   f-key                  .
           move      rf-bft-num-prt       to   rf-bfx-num-prt         .
           move      zero                 to   rf-bfx-num-prg         .
           move      zero                 to   rf-bfx-tip-rec         .
           move      "pgm/bfo/fls/ioc/obj/iofbfx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bfx                 .
      *                      *-----------------------------------------*
      *                      * Test su esito operazione                *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to pul-put-app-600.
       pul-put-app-400.
      *                  *---------------------------------------------*
      *                  * Lettura sequenziale record [bfx]            *
      *                  *---------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofbfx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bfx                 .
      *                      *-----------------------------------------*
      *                      * Test su esito operazione                *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to pul-put-app-600.
      *                  *---------------------------------------------*
      *                  * Test sul massimo                            *
      *                  *---------------------------------------------*
           if        rf-bfx-num-prt       not  = rf-bft-num-prt
                     go to pul-put-app-600.
      *                  *---------------------------------------------*
      *                  * Put record in file di appoggio              *
      *                  *---------------------------------------------*
           move      "Pp"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofbfx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bfx                 .
      *                  *---------------------------------------------*
      *                  * Riciclo su lettura sequenziale record [bfx] *
      *                  *---------------------------------------------*
           go to     pul-put-app-400.
       pul-put-app-600.
      *              *-------------------------------------------------*
      *              * [bft]                                           *
      *              *-------------------------------------------------*
           move      "Pp"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofbft"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bft                 .
       pul-put-app-999.
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

      *    *===========================================================*
      *    * Incremento numero records letti                           *
      *    *-----------------------------------------------------------*
       inc-rec-let-000.
      *              *-------------------------------------------------*
      *              * Incremento                                      *
      *              *-------------------------------------------------*
           add       1                    to   w-ctr-rec-let          .
      *              *-------------------------------------------------*
      *              * Deviazione                                      *
      *              *-------------------------------------------------*
           if        w-ctr-rec-let        >    100
                     go to inc-rec-let-100
           else if   w-ctr-rec-let        >    10
                     go to inc-rec-let-200
           else      go to inc-rec-let-500.
       inc-rec-let-100.
      *              *-------------------------------------------------*
      *              * Se maggiore di 100                              *
      *              *-------------------------------------------------*
           move      w-ctr-rec-let        to   w-ctr-rec-cen          .
           if        w-ctr-rec-cen        =    zero
                     go to inc-rec-let-500
           else      go to inc-rec-let-999.
       inc-rec-let-200.
      *              *-------------------------------------------------*
      *              * Se maggiore di 10                               *
      *              *-------------------------------------------------*
           move      w-ctr-rec-let        to   w-ctr-rec-dec          .
           if        w-ctr-rec-dec        =    zero
                     go to inc-rec-let-500
           else      go to inc-rec-let-999.
       inc-rec-let-500.
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
           perform   vis-rec-let-000      thru vis-rec-let-999        .
       inc-rec-let-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione numero records letti                      *
      *    *-----------------------------------------------------------*
       vis-rec-let-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      09                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<"                  to   v-edm                  .
           move      18                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-ctr-rec-let        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-rec-let-999.
           exit.

      *    *===========================================================*
      *    * Incremento numero records scritti                         *
      *    *-----------------------------------------------------------*
       inc-rec-scr-000.
      *              *-------------------------------------------------*
      *              * Incremento                                      *
      *              *-------------------------------------------------*
           add       1                    to   w-ctr-rec-scr          .
      *              *-------------------------------------------------*
      *              * Deviazione                                      *
      *              *-------------------------------------------------*
           if        w-ctr-rec-scr        >    100
                     go to inc-rec-scr-100
           else if   w-ctr-rec-scr        >    10
                     go to inc-rec-scr-200
           else      go to inc-rec-scr-500.
       inc-rec-scr-100.
      *              *-------------------------------------------------*
      *              * Se maggiore di 100                              *
      *              *-------------------------------------------------*
           move      w-ctr-rec-scr        to   w-ctr-rec-cen          .
           if        w-ctr-rec-cen        =    zero
                     go to inc-rec-scr-500
           else      go to inc-rec-scr-999.
       inc-rec-scr-200.
      *              *-------------------------------------------------*
      *              * Se maggiore di 10                               *
      *              *-------------------------------------------------*
           move      w-ctr-rec-scr        to   w-ctr-rec-dec          .
           if        w-ctr-rec-dec        =    zero
                     go to inc-rec-scr-500
           else      go to inc-rec-scr-999.
       inc-rec-scr-500.
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
           perform   vis-rec-scr-000      thru vis-rec-scr-999        .
       inc-rec-scr-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione numero records scritti                    *
      *    *-----------------------------------------------------------*
       vis-rec-scr-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      09                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<"                  to   v-edm                  .
           move      21                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-ctr-rec-scr        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-rec-scr-999.
           exit.

      *    *===========================================================*
      *    * Incremento numero records eliminati                       *
      *    *-----------------------------------------------------------*
       inc-rec-eli-000.
      *              *-------------------------------------------------*
      *              * Incremento                                      *
      *              *-------------------------------------------------*
           add       1                    to   w-ctr-rec-eli          .
      *              *-------------------------------------------------*
      *              * Deviazione                                      *
      *              *-------------------------------------------------*
           if        w-ctr-rec-eli        >    100
                     go to inc-rec-eli-100
           else if   w-ctr-rec-eli        >    10
                     go to inc-rec-eli-200
           else      go to inc-rec-eli-500.
       inc-rec-eli-100.
      *              *-------------------------------------------------*
      *              * Se maggiore di 100                              *
      *              *-------------------------------------------------*
           move      w-ctr-rec-eli        to   w-ctr-rec-cen          .
           if        w-ctr-rec-cen        =    zero
                     go to inc-rec-eli-500
           else      go to inc-rec-eli-999.
       inc-rec-eli-200.
      *              *-------------------------------------------------*
      *              * Se maggiore di 10                               *
      *              *-------------------------------------------------*
           move      w-ctr-rec-eli        to   w-ctr-rec-dec          .
           if        w-ctr-rec-dec        =    zero
                     go to inc-rec-eli-500
           else      go to inc-rec-eli-999.
       inc-rec-eli-500.
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
           perform   vis-rec-eli-000      thru vis-rec-eli-999        .
       inc-rec-eli-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione numero records eliminati                  *
      *    *-----------------------------------------------------------*
       vis-rec-eli-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      09                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<"                  to   v-edm                  .
           move      20                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-ctr-rec-eli        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-rec-eli-999.
           exit.

      *    *===========================================================*
      *    * Find su archivio [bft]                                    *
      *    *-----------------------------------------------------------*
       fnd-arc-bft-000.
      *              *-------------------------------------------------*
      *              * Test se programma di interrogazione gia' attivo *
      *              *-------------------------------------------------*
           move      "P?"                 to   s-ope                  .
           move      "pbfo3010"           to   s-pro                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-liv                not  = zero
                     go to  fnd-arc-bft-999.
      *              *-------------------------------------------------*
      *              * Richiamo programma di interrogazione            *
      *              *-------------------------------------------------*
           move      "pgm/bfo/prg/obj/pbfo3010"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat                                            .
           cancel    s-pat                                            .
       fnd-arc-bft-999.
           exit.

      *    *===========================================================*
      *    * Determinazione numero records                             *
      *    *-----------------------------------------------------------*
       det-rec-fil-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-det-rec-fil-flg      .
      *              *-------------------------------------------------*
      *              * Normalizzazione record determinati              *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-rec-fil-rec      .
      *              *-------------------------------------------------*
      *              * Salvataggio immagine video                      *
      *              *-------------------------------------------------*
           move      "SV"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Estrazione pathname file originale              *
      *              *-------------------------------------------------*
           move      "PG"                 to   s-ope                  .
           move      f-xxx-nam            to   s-nam                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-pat                to   f-xxx-pfo              .
      *              *-------------------------------------------------*
      *              * Richiesta alla segreteria di un pathname unico  *
      *              * per file temporaneo, e sua memorizzazione       *
      *              *-------------------------------------------------*
           move      "UP"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-pat                to   w-det-rec-fil-tpt      .
      *              *-------------------------------------------------*
      *              * Comando di determinazione                       *
      *              *-------------------------------------------------*
           move      80                   to   w-all-str-lun          .
           move      04                   to   w-all-str-num          .
           move      "vutil -info"        to   w-all-str-cat (1)      .
           move      f-xxx-pfo            to   w-all-str-cat (2)      .
           move      "| cat > "           to   w-all-str-cat (3)      .
           move      w-det-rec-fil-tpt    to   w-all-str-cat (4)      .
           perform   all-str-csb-000      thru all-str-csb-999        .
           move      w-all-str-alf        to   o-shs                  .
      *              *-------------------------------------------------*
      *              * Richiamo del modulo 'mopsys'                    *
      *              *-------------------------------------------------*
           move      "SH"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *              *-------------------------------------------------*
      *              * Erase video                                     *
      *              *-------------------------------------------------*
           move      "ER"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Ripristino immagine video                       *
      *              *-------------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       det-rec-fil-300.
      *              *-------------------------------------------------*
      *              * Apertura del file temporaneo                    *
      *              *-------------------------------------------------*
           move      "OI"                 to   g-ope                  .
           move      "siz "               to   g-nam                  .
           move      w-det-rec-fil-tpt    to   g-pat                  .
           call      "swd/mod/prg/obj/mcvinp"
                                         using g                      .
      *              *-------------------------------------------------*
      *              * Se errore : uscita con flag di errore           *
      *              *-------------------------------------------------*
           if        g-sts                =    e-not-err
                     go to det-rec-fil-400.
      *              *-------------------------------------------------*
      *              * Chiusura del file in input                      *
      *              *-------------------------------------------------*
           move      "CL"                 to   g-ope                  .
           call      "swd/mod/prg/obj/mcvinp"
                                         using g                      .
      *              *-------------------------------------------------*
      *              * Cancellazione modulo utilizzato                 *
      *              *-------------------------------------------------*
           cancel    "swd/mod/prg/obj/mcvinp"                         .
      *              *-------------------------------------------------*
      *              * Flag di uscita                                  *
      *              *-------------------------------------------------*
           move      "N"                  to   w-det-rec-fil-flg      .
      *              *-------------------------------------------------*
      *              * Ad uscita                                       *
      *              *-------------------------------------------------*
           go to     det-rec-fil-900.
       det-rec-fil-400.
      *              *-------------------------------------------------*
      *              * Read file sequenziale - riga 1                  *
      *              *-------------------------------------------------*
           move      "GN"                 to   g-ope                  .
           call      "swd/mod/prg/obj/mcvinp"
                                         using g                      .
      *              *-------------------------------------------------*
      *              * Se fine file : uscita con flag                  *
      *              *-------------------------------------------------*
           if        g-sts                not  = e-not-err
                     go to det-rec-fil-800.
      *              *-------------------------------------------------*
      *              * Read file sequenziale - riga 2                  *
      *              *-------------------------------------------------*
           move      "GN"                 to   g-ope                  .
           call      "swd/mod/prg/obj/mcvinp"
                                         using g                      .
      *              *-------------------------------------------------*
      *              * Se fine file : uscita con flag                  *
      *              *-------------------------------------------------*
           if        g-sts                not  = e-not-err
                     go to det-rec-fil-800.
      *              *-------------------------------------------------*
      *              * Read file sequenziale - riga 3                  *
      *              *-------------------------------------------------*
           move      "GN"                 to   g-ope                  .
           call      "swd/mod/prg/obj/mcvinp"
                                         using g                      .
      *              *-------------------------------------------------*
      *              * Se fine file : uscita con flag                  *
      *              *-------------------------------------------------*
           if        g-sts                not  = e-not-err
                     go to det-rec-fil-800.
       det-rec-fil-500.
      *              *-------------------------------------------------*
      *              * Comodo per lettura file                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * In campo di comodo                          *
      *                  *---------------------------------------------*
           move      g-rec                to   w-det-rec-fil-alf      .
      *                  *---------------------------------------------*
      *                  * Test su contenuto                           *
      *                  *---------------------------------------------*
           if        w-det-rec-fil-tst    not  = "# of records:"
                     go to det-rec-fil-800.
      *                  *---------------------------------------------*
      *                  * Conversione numero letto                    *
      *                  *---------------------------------------------*
           move      "CV"                 to   v-ope                  .
           move      w-det-rec-fil-dat    to   v-alf                  .
           move      13                   to   v-car                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-num                to   w-det-rec-fil-rec      .
      *                  *---------------------------------------------*
      *                  * Flag di uscita                              *
      *                  *---------------------------------------------*
           move      "S"                  to   w-det-rec-fil-flg      .
      *                  *---------------------------------------------*
      *                  * A close                                     *
      *                  *---------------------------------------------*
           go to     det-rec-fil-800.
       det-rec-fil-800.
      *              *-------------------------------------------------*
      *              * Chiusura del file in input                      *
      *              *-------------------------------------------------*
           move      "CL"                 to   g-ope                  .
           call      "swd/mod/prg/obj/mcvinp"
                                         using g                      .
      *              *-------------------------------------------------*
      *              * Cancellazione modulo utilizzato                 *
      *              *-------------------------------------------------*
           cancel    "swd/mod/prg/obj/mcvinp"                         .
       det-rec-fil-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-rec-fil-999.
       det-rec-fil-999.
           exit.

      *    *===========================================================*
      *    * Subroutines per allineamenti a destra o a sinistra oppure *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cps"                   .

