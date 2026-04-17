       Identification Division.
       Program-Id.                                 piiczat0           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    iic                 *
      *                                Settore:    pul                 *
      *                                   Fase:    iiczat              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 05/10/96    *
      *                       Ultima revisione:    NdK del 02/03/99    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Pulizia bolle fornitori                     *
      *                                                                *
      *================================================================*

      ******************************************************************
       Environment Division.
      ******************************************************************

      *================================================================*
       Configuration Section.
      *================================================================*

       Source-Computer.        N-d-K-Sia-PD .
       Object-Computer.        N-d-K-Sia-PD .

       Special-Names.          Decimal-Point     Is Comma .

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
                     "iic"                                            .
      *        *-------------------------------------------------------*
      *        * Settore gestionale                                    *
      *        *-------------------------------------------------------*
           05  i-ide-set                  pic  x(03) value
                     "pul"                                            .
      *        *-------------------------------------------------------*
      *        * Fase gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-fas                  pic  x(06) value
                     "iiczat"                                         .
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10) value
                     "piiczat0"                                       .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  i-ide-des                  pic  x(40) value
                     "           Sistemazione date IIC        "       .

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
      *        *-------------------------------------------------------*
      *        * Work per padding campi alfanumerici con 'z'           *
      *        *-------------------------------------------------------*
           05  w-pad-zzz.
               10  w-pad-zzz-alf.
                   15  w-pad-zzz-alf-chr
                                   occurs 20       
                                          pic  x(01)                  .
               10  w-pad-zzz-ctr          pic  9(02)                  .

      *    *===========================================================*
      *    * Work-area per test se blanks embedded                     *
      *    *-----------------------------------------------------------*
       01  w-bla-emb.
           05  w-bla-emb-flg              pic  x(01)                  .
           05  w-bla-emb-str.
               10  w-bla-emb-chr occurs 40
                                          pic  x(01)                  .
           05  w-bla-emb-ctr              pic  9(02)                  .

      *    *===========================================================*
      *    * Record files                                              *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [iit]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/iic/fls/rec/rfiit"                          .
      *        *-------------------------------------------------------*
      *        * [iir]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/iic/fls/rec/rfiir"                          .

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

      *    *===========================================================*
      *    * Work per subroutines di Find                              *
      *    *-----------------------------------------------------------*
       01  w-fnd.
           05  filler                     pic  x(01)                  .
               
      *    *===========================================================*
      *    * Work per subroutines di Let                               *
      *    *-----------------------------------------------------------*
       01  w-let.
           05  filler                     pic  x(01)                  .

      *    *===========================================================*
      *    * Work-area per salvataggi                                  *
      *    *-----------------------------------------------------------*
       01  w-sav.
           05  filler                     pic  x(01)                  .

      *    *===========================================================*
      *    * Work per accettazioni campi espansi                       *
      *    *-----------------------------------------------------------*
       01  w-exp.
           05  filler                     pic  x(01)                  .

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
           05  w-pul-put-app-ctr          pic  9(05)                  .

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
      *    * Regolarizzazione campo alfanumerico con padding di "z"    *
      *    *-----------------------------------------------------------*
       pad-alf-zzz-000.
           move      20                   to   w-pad-zzz-ctr          .
       pad-alf-zzz-100.
           if        w-pad-zzz-ctr        >    zero
                     if    w-pad-zzz-alf-chr
                          (w-pad-zzz-ctr) =    spaces
                           move    "z"    to   w-pad-zzz-alf-chr
                                              (w-pad-zzz-ctr)
                           subtract 1     from w-pad-zzz-ctr
                           go to    pad-alf-zzz-100.
       pad-alf-zzz-999.
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
      *                  * Data pulizia                                *
      *                  *---------------------------------------------*
           perform   acc-dat-pul-000      thru acc-dat-pul-999        .
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
           move      "Conferma impostazioni (S/N/E) ?"
                                          to   v-not                  .
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
           move      "Sistem. documenti con data di registrazione fino a
      -              "l .............. :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-dat-pul-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts : Nota bene                       *
      *    *-----------------------------------------------------------*
       pmt-not-ben-000.
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
           perform   fnd-arc-iit-000      thru fnd-arc-iit-999        .
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
       nor-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Pulizie : Open files                                      *
      *    *-----------------------------------------------------------*
       pul-opn-fls-000.
      *              *-------------------------------------------------*
      *              * [iit]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/iic/fls/ioc/obj/iofiit"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-iit                 .
      *              *-------------------------------------------------*
      *              * [iir]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/iic/fls/ioc/obj/iofiir"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-iir                 .
       pul-opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Pulizie : Close files                                     *
      *    *-----------------------------------------------------------*
       pul-cls-fls-000.
      *              *-------------------------------------------------*
      *              * [iit]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/iic/fls/ioc/obj/iofiit"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-iit                 .
      *              *-------------------------------------------------*
      *              * [iir]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/iic/fls/ioc/obj/iofiir"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-iir                 .
       pul-cls-fls-999.
           exit.

      *    *===========================================================*
      *    * Pulizie : Open files di appoggio                          *
      *    *-----------------------------------------------------------*
       pul-opn-app-000.
      *              *-------------------------------------------------*
      *              * [iit]                                           *
      *              *-------------------------------------------------*
           move      "Op"                 to   f-ope                  .
           move      "pgm/iic/fls/ioc/obj/iofiit"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-iit                 .
      *              *-------------------------------------------------*
      *              * [iir]                                           *
      *              *-------------------------------------------------*
           move      "Op"                 to   f-ope                  .
           move      "pgm/iic/fls/ioc/obj/iofiir"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-iir                 .
       pul-opn-app-999.
           exit.

      *    *===========================================================*
      *    * Pulizie : Close files di appoggio                         *
      *    *-----------------------------------------------------------*
       pul-cls-app-000.
      *              *-------------------------------------------------*
      *              * [iit]                                           *
      *              *-------------------------------------------------*
           move      "Cp"                 to   f-ope                  .
           move      "pgm/iic/fls/ioc/obj/iofiit"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-iit                 .
      *              *-------------------------------------------------*
      *              * [iir]                                           *
      *              *-------------------------------------------------*
           move      "Cp"                 to   f-ope                  .
           move      "pgm/iic/fls/ioc/obj/iofiir"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-iir                 .
       pul-cls-app-999.
           exit.

      *    *===========================================================*
      *    * Pulizie : Rename sostitutivo file di appoggio             *
      *    *-----------------------------------------------------------*
       pul-ren-sos-000.
      *              *-------------------------------------------------*
      *              * [iit]                                           *
      *              *-------------------------------------------------*
           move      "Xp"                 to   f-ope                  .
           move      "pgm/iic/fls/ioc/obj/iofiit"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-iit                 .
      *              *-------------------------------------------------*
      *              * [iir]                                           *
      *              *-------------------------------------------------*
           move      "Xp"                 to   f-ope                  .
           move      "pgm/iic/fls/ioc/obj/iofiir"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-iir                 .
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
      *              * Azzeramento contatori records                   *
      *              *-------------------------------------------------*
           move      zero                 to   w-ctr-rec-let          .
           move      zero                 to   w-ctr-rec-eli          .
           move      zero                 to   w-ctr-rec-scr          .
      *              *-------------------------------------------------*
      *              * Start su archivio [iit]                         *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "DATREG    "         to   f-key                  .
           move      zero                 to   rf-iit-dat-reg         .
           move      zero                 to   rf-iit-num-prt         .
           move      "pgm/iic/fls/ioc/obj/iofiit"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-iit                 .
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
       pul-let-seq-100.
      *              *-------------------------------------------------*
      *              * Lettura sequenziale archivio [iit]              *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/iic/fls/ioc/obj/iofiit"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-iit                 .
      *                  *---------------------------------------------*
      *                  * Se fine file                                *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     move  "#"            to   w-cnt-pul-flg-sub
                     go to pul-let-seq-999.
       pul-let-seq-200.
      *              *-------------------------------------------------*
      *              * Incremento contatore records [iit] letti        *
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
       pul-sel-rec-800.
      *              *-------------------------------------------------*
      *              * Uscita per movimento da riportare               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Incremento contatore records [iit] scritti  *
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
      *                  * Incremento contatore records [iit] elimina- *
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
      *              * [iir]                                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione contatore records [iir]     *
      *                  * scritti                                     *
      *                  *---------------------------------------------*
           move      zero                 to   w-pul-put-app-ctr      .
      *                  *---------------------------------------------*
      *                  * Start su file [iir]                         *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "DATREG    "         to   f-key                  .
           move      rf-iit-dat-reg       to   rf-iir-dat-reg         .
           move      rf-iit-num-prt       to   rf-iir-num-prt         .
           move      zero                 to   rf-iir-num-prg         .
           move      "pgm/iic/fls/ioc/obj/iofiir"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-iir                 .
      *                      *-----------------------------------------*
      *                      * Test su esito operazione                *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to pul-put-app-200.
       pul-put-app-100.
      *                  *---------------------------------------------*
      *                  * Lettura sequenziale record [iir]            *
      *                  *---------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/iic/fls/ioc/obj/iofiir"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-iir                 .
      *                      *-----------------------------------------*
      *                      * Test su esito operazione                *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to pul-put-app-200.
      *                  *---------------------------------------------*
      *                  * Test sul massimo                            *
      *                  *---------------------------------------------*
           if        rf-iir-dat-reg       not  = rf-iit-dat-reg
                     go to pul-put-app-200.
           if        rf-iir-num-prt       not  = rf-iit-num-prt
                     go to pul-put-app-200.
      *                  *---------------------------------------------*
      *                  * Aggiornamento data                          *
      *                  *---------------------------------------------*
           if        rf-iir-dat-reg       >    0990101
                     move  0990331        to   rf-iir-dat-reg         .
      *                  *---------------------------------------------*
      *                  * Incremento contatore records [iir] scritti  *
      *                  *---------------------------------------------*
           add       1                    to   w-pul-put-app-ctr      .
      *                  *---------------------------------------------*
      *                  * Put record in file di appoggio              *
      *                  *---------------------------------------------*
           move      "Pp"                 to   f-ope                  .
           move      "pgm/iic/fls/ioc/obj/iofiir"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-iir                 .
      *                  *---------------------------------------------*
      *                  * Riciclo su lettura sequenziale record [iir] *
      *                  *---------------------------------------------*
           go to     pul-put-app-100.
       pul-put-app-200.
      *              *-------------------------------------------------*
      *              * Test su contatore records [iir] scritti : se a  *
      *              * zero si esce                                    *
      *              *-------------------------------------------------*
           if        w-pul-put-app-ctr    =    zero
                     go to pul-put-app-999.
       pul-put-app-300.
       pul-put-app-600.
      *              *-------------------------------------------------*
      *              * Aggiornamento data                              *
      *              *-------------------------------------------------*
           if        rf-iit-dat-reg       >    0990101
                     move  0990331        to   rf-iit-dat-reg         .
      *              *-------------------------------------------------*
      *              * [iit]                                           *
      *              *-------------------------------------------------*
           move      "Pp"                 to   f-ope                  .
           move      "pgm/iic/fls/ioc/obj/iofiit"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-iit                 .
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
      *    * Find su archivio [iit]                                    *
      *    *-----------------------------------------------------------*
       fnd-arc-iit-000.
      *              *-------------------------------------------------*
      *              * Test se programma di interrogazione gia' attivo *
      *              *-------------------------------------------------*
           move      "P?"                 to   s-ope                  .
           move      "piic3010"           to   s-pro                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-liv                not  = zero
                     go to  fnd-arc-iit-999.
      *              *-------------------------------------------------*
      *              * Richiamo programma di interrogazione            *
      *              *-------------------------------------------------*
           move      "pgm/iic/prg/obj/piic3010"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat                                            .
           cancel    s-pat                                            .
       fnd-arc-iit-999.
           exit.

