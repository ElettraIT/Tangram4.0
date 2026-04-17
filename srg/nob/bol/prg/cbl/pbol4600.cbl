       Identification Division.
       Program-Id.                                 pbol4600           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    nob                 *
      *                        Area gestionale:    bol                 *
      *                                Settore:    spd                 *
      *                                   Fase:    bol460              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 14/05/92    *
      *                       Ultima revisione:    NdK del 02/10/13    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Richieste per il programma pbol4601:        *
      *                                                                *
      *                    Stampa distinta spedizione per vettore      *
      *                                                                *
      *                    SU MISURA PER NOBILI                        *
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
                     "nob"                                            .
      *        *-------------------------------------------------------*
      *        * Area gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-arg                  pic  x(03) value
                     "bol"                                            .
      *        *-------------------------------------------------------*
      *        * Settore gestionale                                    *
      *        *-------------------------------------------------------*
           05  i-ide-set                  pic  x(03) value
                     "spd"                                            .
      *        *-------------------------------------------------------*
      *        * Fase gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-fas                  pic  x(06) value
                     "bol460"                                         .
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10) value
                     "pbol4600"                                       .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  i-ide-des                  pic  x(40) value
                     " STAMPA DISTINTA SPEDIZIONE PER VETTORE "       .

      *    *===========================================================*
      *    * Area per il programma di esecuzione                       *
      *    *-----------------------------------------------------------*
       01  i-exe.
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma di esecuzione             *
      *        *-------------------------------------------------------*
           05  i-exe-pro                  pic  x(10) value
                     "pbol4601  "                                     .
      *        *-------------------------------------------------------*
      *        * Pathname del programma di esecuzione                  *
      *        *-------------------------------------------------------*
           05  i-exe-pat                  pic  x(40) value
                     "nob/bol/prg/obj/pbol4601                "       .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "mvideo" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/v"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "msegrt" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/s"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "mpslct" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/r"                                  .

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
      *            *---------------------------------------------------*
      *            * Per routine sel-prm-stp-000                       *
      *            *---------------------------------------------------*
               10  w-cnt-sel-prm-stp      pic  x(01)                  .
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
      *        * Flags di controllo per tipo funzionamento             *
      *        *-------------------------------------------------------*
           05  w-cnt-fun.
      *            *---------------------------------------------------*
      *            * Si/No richieste per programma di esecuzione       *
      *            *---------------------------------------------------*
               10  w-cnt-fun-snx-ric      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Si/No richiesta di selezione stampa               *
      *            *---------------------------------------------------*
               10  w-cnt-fun-snx-stp      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Area per preparazione parametri selezione stampa      *
      *        *-------------------------------------------------------*
           05  w-cnt-stp.
               10  w-cnt-stp-tip-sel      pic  x(10)                  .
               10  w-cnt-stp-cod-stp      pic  x(08)                  .
               10  w-cnt-stp-tip-sta      pic  x(01)                  .
               10  w-cnt-stp-cod-mod      pic  x(08)                  .
               10  w-cnt-stp-tip-mod      pic  x(01)                  .
               10  w-cnt-stp-amp-lin      pic  9(03)                  .
               10  w-cnt-stp-top-lin      pic  9(04)                  .
               10  w-cnt-stp-lin-min      pic  9(02)                  .
               10  w-cnt-stp-bot-lin      pic  9(04)                  .
               10  w-cnt-stp-amp-car      pic  9(02)v9(02)            .
               10  w-cnt-stp-alt-int      pic  9(02)v9(02)            .
               10  w-cnt-stp-esp-fut      pic  x(99)                  .
               10  w-cnt-stp-fnz-spc      pic  x(99)                  .
      *        *-------------------------------------------------------*
      *        * Work per string-unstring record richieste             *
      *        *-------------------------------------------------------*
           05  w-stu-rrr.
               10  w-stu-rrr-pnt-stu      pic  9(05)                  .
               10  w-stu-rrr-255-byt.
                   15  filler occurs 255  pic  x(01)                  .
               10  w-stu-rrr-sav-pnt      pic  9(05)                  .

      *    *===========================================================*
      *    * Records files                                             *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [vet]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/bol/fls/rec/rfvet"                          .

      *    *===========================================================*
      *    * Work-area richieste per stampa                            *
      *    *-----------------------------------------------------------*
       01  rr.
           05  rr-cod-dpz                 pic  9(02)                  .
           05  rr-dat-max                 pic  9(07)                  .
           05  rr-cod-vet                 pic  9(07)                  .
           05  rr-cod-vet-rag             pic  x(40)                  .
           05  rr-cod-vet-via             pic  x(40)                  .
           05  rr-cod-vet-loc             pic  x(40)                  .
           05  rr-ana-cli                 pic  9(02)                  .
           05  rr-tip-stp                 pic  9(02)                  .
           05  rr-dat-cns                 pic  9(02)                  .

      *    *===========================================================*
      *    * Work per subroutines di Find                              *
      *    *-----------------------------------------------------------*
       01  w-fnd.
      *        *-------------------------------------------------------*
      *        * Work per Find su archivio [bit]                       *
      *        *-------------------------------------------------------*
           05  w-fnd-arc-bit.
               10  w-fnd-arc-bit-sel      pic  x(01)                  .
               10  w-fnd-arc-bit-tmb      pic  x(05)                  .
               10  w-fnd-arc-bit-dat      pic  9(07)                  .
               10  w-fnd-arc-bit-num      pic  9(11)                  .
               
      *    *===========================================================*
      *    * Work per subroutines di Let                               *
      *    *-----------------------------------------------------------*
       01  w-let.
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [vet]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-vet.
               10  w-let-arc-vet-flg      pic  x(01)                  .
               10  w-let-arc-vet-cod      pic  9(07)                  .
               10  w-let-arc-vet-rag      pic  x(40)                  .
               10  w-let-arc-vet-via      pic  x(40)                  .
               10  w-let-arc-vet-loc      pic  x(40)                  .

      *    *===========================================================*
      *    * Work per accettazioni campi espansi                       *
      *    *-----------------------------------------------------------*
       01  w-exp.
      *        *-------------------------------------------------------*
      *        * Work per : Anagrafica clienti                         *
      *        *-------------------------------------------------------*
           05  w-exp-ana-cli.
               10  w-exp-ana-cli-num      pic  9(02)       value 2    .
               10  w-exp-ana-cli-lun      pic  9(02)       value 40   .
               10  w-exp-ana-cli-tbl.
                   15  filler             pic  x(40) value
                            "solo Ragione sociale                    ".
                   15  filler             pic  x(40) value
                            "Indirizzo di spedizione completo        ".
      *        *-------------------------------------------------------*
      *        * Work per : Tipo stampa                                *
      *        *-------------------------------------------------------*
           05  w-exp-tip-stp.
               10  w-exp-tip-stp-num      pic  9(02)       value 2    .
               10  w-exp-tip-stp-lun      pic  9(02)       value 40   .
               10  w-exp-tip-stp-tbl.
                   15  filler             pic  x(40) value
                            "con Totale documento                    ".
                   15  filler             pic  x(40) value
                            "con Numero colli e peso                 ".
      *        *-------------------------------------------------------*
      *        * Work per : Data consegna                              *
      *        *-------------------------------------------------------*
           05  w-exp-dat-cns.
               10  w-exp-dat-cns-num      pic  9(02)       value 2    .
               10  w-exp-dat-cns-lun      pic  9(02)       value 40   .
               10  w-exp-dat-cns-tbl.
                   15  filler             pic  x(40) value
                            "data di Inizio trasporto                ".
                   15  filler             pic  x(40) value
                            "da Annotazioni in bolla                 ".

      *    *===========================================================*
      *    * Link-area per accettazione codice vettore                 *
      *    *-----------------------------------------------------------*
           copy      "pgm/bol/prg/cpy/acmnvet0.acl"                   .

      *    *===========================================================*
      *    * Work per subroutines di Err                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/werrbox0.cpw"                   .

      *    *===========================================================*
      *    * Area di interfaccia per sottoprogramma         "pazi000d" *
      *    *-----------------------------------------------------------*
           copy      "pgm/azi/prg/cpy/pazi000d.pgl"                   .

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
           perform   dic-ini-pgm-000      thru dic-ini-pgm-999        .
           if        w-cnt-dic-ini-pgm    not  = spaces
                     go to main-999.
      *              *-------------------------------------------------*
      *              * Esecuzione routine pre-esecuzione programma     *
      *              *-------------------------------------------------*
           perform   pre-exe-pgm-000      thru pre-exe-pgm-999        .
           if        w-cnt-pre-exe-pgm    not  = spaces
                     go to main-900.
      *              *-------------------------------------------------*
      *              * Preparazione tipo funzionamento programma       *
      *              *-------------------------------------------------*
           perform   pre-tip-fun-000      thru pre-tip-fun-999        .
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
           perform   rou-opn-fls-000      thru rou-opn-fls-999        .
           if        w-cnt-rou-opn-fls    not  = spaces
                     go to main-750.
      *              *-------------------------------------------------*
      *              * Se no richieste : a selezione stampante         *
      *              *-------------------------------------------------*
           if        w-cnt-fun-snx-ric    not  = "S"
                     go to main-350.
       main-250.
      *              *-------------------------------------------------*
      *              * Accettazione richieste di selezione             *
      *              *-------------------------------------------------*
           perform   acc-ric-sel-000      thru acc-ric-sel-999        .
      *                  *---------------------------------------------*
      *                  * Se uscita per Exit                          *
      *                  *---------------------------------------------*
           if        w-cnt-acc-ric-sel    =    "E"
                     go to main-750.
      *              *-------------------------------------------------*
      *              * Regolarizzazione richieste di selezione         *
      *              *-------------------------------------------------*
           perform   reg-ric-sel-000      thru reg-ric-sel-999        .
       main-350.
      *              *-------------------------------------------------*
      *              * Se no stampa : ad esecuzione                    *
      *              *-------------------------------------------------*
           if        w-cnt-fun-snx-stp    not  = "S"
                     go to main-450.
      *              *-------------------------------------------------*
      *              * Preparazione defaults per parametri di selezio- *
      *              * ne stampa                                       *
      *              *-------------------------------------------------*
           perform   pre-prm-stp-000      thru pre-prm-stp-999        .
      *              *-------------------------------------------------*
      *              * Selezione parametri stampa                      *
      *              *-------------------------------------------------*
           perform   sel-prm-stp-000      thru sel-prm-stp-999        .
      *                  *---------------------------------------------*
      *                  * Test se uscita                              *
      *                  *---------------------------------------------*
           if        w-cnt-sel-prm-stp    not  = spaces
                     go to main-750.
       main-450.
      *                  *---------------------------------------------*
      *                  * Richiesta alla segreteria se funzionamento  *
      *                  * in background o foreground                  *
      *                  *---------------------------------------------*
           move      "BF"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Se esecuzione in foreground                 *
      *                  *---------------------------------------------*
           if        s-snb                =    "B"
                     go to main-500.
           perform   exe-pgm-frg-000      thru exe-pgm-frg-999        .
           go to     main-750.
       main-500.
      *                  *---------------------------------------------*
      *                  * Se esecuzione in background                 *
      *                  *---------------------------------------------*
           perform   exe-pgm-bkg-000      thru exe-pgm-bkg-999        .
       main-750.
      *              *-------------------------------------------------*
      *              * Close files                                     *
      *              *-------------------------------------------------*
           perform   rou-cls-fls-000      thru rou-cls-fls-999        .
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
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-dic-ini-pgm      .
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
      *              * Scrittura record richieste per foreground       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Inizio scrittura record richieste           *
      *                  *---------------------------------------------*
           move      "OO"                 to   b-ope                  .
           move      "F"                  to   b-tfe                  .
           call      "swd/mod/prg/obj/mbckgr"
                                         using b                      .
      *                      *-----------------------------------------*
      *                      * Se errore : uscita                      *
      *                      *-----------------------------------------*
            if       b-rsc                not  = spaces
                     go to exe-pgm-frg-999.
      *                  *---------------------------------------------*
      *                  * Estrazione segmenti da 255  bytes da record *
      *                  * richieste                                   *
      *                  *---------------------------------------------*
           move      1                    to   w-stu-rrr-pnt-stu      .
       exe-pgm-frg-200.
           move      spaces               to   w-stu-rrr-255-byt      .
           move      w-stu-rrr-pnt-stu    to   w-stu-rrr-sav-pnt      .
           unstring  rr                   into w-stu-rrr-255-byt
                                  with pointer w-stu-rrr-pnt-stu      .
           move      w-stu-rrr-255-byt    to   b-chr                  .
           if        w-stu-rrr-pnt-stu    =    w-stu-rrr-sav-pnt
                     go to exe-pgm-frg-400.
           move      "PT"                 to   b-ope                  .
           call      "swd/mod/prg/obj/mbckgr"
                                         using b                      .
           go to     exe-pgm-frg-200.
       exe-pgm-frg-400.
      *                  *---------------------------------------------*
      *                  * Fine scrittura record richieste             *
      *                  *---------------------------------------------*
           move      "CL"                 to   b-ope                  .
           call      "swd/mod/prg/obj/mbckgr"
                                         using b                      .
      *                      *-----------------------------------------*
      *                      * Se errore : uscita                      *
      *                      *-----------------------------------------*
            if       b-rsc                not  = spaces
                     go to exe-pgm-frg-999.
      *              *-------------------------------------------------*
      *              * Messaggio di programma in esecuzione            *
      *              *-------------------------------------------------*
           move      "PE"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Lancio del programma di esecuzione              *
      *              *-------------------------------------------------*
           call      i-exe-pat                                        .
      *              *-------------------------------------------------*
      *              * Cancel del programma di esecuzione              *
      *              *-------------------------------------------------*
           cancel    i-exe-pat                                        .
      *              *-------------------------------------------------*
      *              * Cancel del modulo "mbckgr"                      *
      *              *-------------------------------------------------*
           cancel    "swd/mod/prg/obj/mbckgr"                         .
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
      *    *  Selezione parametri stampa                               *
      *    *-----------------------------------------------------------*
       sel-prm-stp-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-sel-prm-stp      .
      *              *-------------------------------------------------*
      *              * Test se selezione stampa da eseguire            *
      *              *-------------------------------------------------*
           if        w-cnt-fun-snx-stp    not  = "S"
                     go to sel-prm-stp-999.
      *              *-------------------------------------------------*
      *              * Preparazione parametri per richiamo selezione   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Informazioni generali da segreteria         *
      *                  *---------------------------------------------*
           move      "IG"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                      *-----------------------------------------*
      *                      * Codice azienda                          *
      *                      *-----------------------------------------*
           move      s-azi                to   r-env-cod-azi          .
      *                      *-----------------------------------------*
      *                      * Codice terminale                        *
      *                      *-----------------------------------------*
           move      s-ter                to   r-env-cod-ter          .
      *                      *-----------------------------------------*
      *                      * Codice utente                           *
      *                      *-----------------------------------------*
           move      s-ute                to   r-env-cod-ute          .
      *                      *-----------------------------------------*
      *                      * Date and time da segreteria             *
      *                      *-----------------------------------------*
           move      "DT"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                      *-----------------------------------------*
      *                      * Date and time                           *
      *                      *-----------------------------------------*
           move      s-sdt                to   r-env-dat-tim          .
      *                  *---------------------------------------------*
      *                  * Informazioni da identificazione programma   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Sistema applicativo                     *
      *                      *-----------------------------------------*
           move      i-ide-sap            to   r-ide-sis-app          .
      *                      *-----------------------------------------*
      *                      * Area gestionale                         *
      *                      *-----------------------------------------*
           move      i-ide-arg            to   r-ide-are-ges          .
      *                      *-----------------------------------------*
      *                      * Settore gestionale                      *
      *                      *-----------------------------------------*
           move      i-ide-set            to   r-ide-set-ges          .
      *                      *-----------------------------------------*
      *                      * Fase gestionale                         *
      *                      *-----------------------------------------*
           move      i-ide-fas            to   r-ide-fas-ges          .
      *                  *---------------------------------------------*
      *                  * Informazioni da preparazione param. stampa  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Flags di tipo selezione                 *
      *                      *-----------------------------------------*
           move      w-cnt-stp-tip-sel    to   r-fix-tip-sel          .
      *                      *-----------------------------------------*
      *                      * Codice stampante                        *
      *                      *-----------------------------------------*
           move      w-cnt-stp-cod-stp    to   r-fix-cod-stp          .
      *                      *-----------------------------------------*
      *                      * Tipo di stampa                          *
      *                      *-----------------------------------------*
           move      w-cnt-stp-tip-sta    to   r-fix-tip-sta          .
      *                      *-----------------------------------------*
      *                      * Codice modulo                           *
      *                      *-----------------------------------------*
           move      w-cnt-stp-cod-mod    to   r-fix-cod-mod          .
      *                      *-----------------------------------------*
      *                      * Tipo modulo                             *
      *                      *-----------------------------------------*
           move      w-cnt-stp-tip-mod    to   r-fix-tip-mod          .
      *                      *-----------------------------------------*
      *                      * Ampiezza linea di stampa in caratteri   *
      *                      *-----------------------------------------*
           move      w-cnt-stp-amp-lin    to   r-fix-amp-lin          .
      *                      *-----------------------------------------*
      *                      * Top margin in linee                     *
      *                      *-----------------------------------------*
           move      w-cnt-stp-top-lin    to   r-fix-top-lin          .
      *                      *-----------------------------------------*
      *                      * Numero linee di stampa minimo           *
      *                      *-----------------------------------------*
           move      w-cnt-stp-lin-min    to   r-fix-lin-min          .
      *                      *-----------------------------------------*
      *                      * Bottom margin in linee                  *
      *                      *-----------------------------------------*
           move      w-cnt-stp-bot-lin    to   r-fix-bot-lin          .
      *                      *-----------------------------------------*
      *                      * Ampiezza caratteri                      *
      *                      *-----------------------------------------*
           move      w-cnt-stp-amp-car    to   r-fix-amp-car          .
      *                      *-----------------------------------------*
      *                      * Altezza interlinea                      *
      *                      *-----------------------------------------*
           move      w-cnt-stp-alt-int    to   r-fix-alt-int          .
      *                      *-----------------------------------------*
      *                      * Area riservata per espansioni future    *
      *                      *-----------------------------------------*
           move      w-cnt-stp-esp-fut    to   r-fix-esp-fut          .
      *                      *-----------------------------------------*
      *                      * Area riservata per funzioni speciali    *
      *                      *-----------------------------------------*
           move      w-cnt-stp-fnz-spc    to   r-fix-fnz-spc          .
      *              *-------------------------------------------------*
      *              * Richiamo modulo di selezione stampa             *
      *              *-------------------------------------------------*
           call      "swd/mod/prg/obj/mpslct"
                                         using r                      .
           cancel    "swd/mod/prg/obj/mpslct"                         .
      *              *-------------------------------------------------*
      *              * Status di uscita                                *
      *              *-------------------------------------------------*
           if        r-rsc                not  = spaces
                     move  "#"            to   w-cnt-sel-prm-stp      .
       sel-prm-stp-999.
           exit.

      *    *===========================================================*
      *    * Programma di esecuzione in background                     *
      *    *-----------------------------------------------------------*
       exe-pgm-bkg-000.
      *              *-------------------------------------------------*
      *              * Inizializzazione rullino messaggi di background *
      *              *-------------------------------------------------*
           move      "OB"                 to   m-ope                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
      *                  *---------------------------------------------*
      *                  * Se errore : uscita                          *
      *                  *---------------------------------------------*
            if       m-rsc                not  = spaces
                     go to exe-pgm-bkg-900.
      *              *-------------------------------------------------*
      *              * Scrittura record richieste per background       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Inizio scrittura record richieste           *
      *                  *---------------------------------------------*
           move      "OO"                 to   b-ope                  .
           move      "B"                  to   b-tfe                  .
           call      "swd/mod/prg/obj/mbckgr"
                                         using b                      .
      *                      *-----------------------------------------*
      *                      * Se errore : uscita                      *
      *                      *-----------------------------------------*
            if       b-rsc                not  = spaces
                     go to exe-pgm-bkg-900.
      *                  *---------------------------------------------*
      *                  * Estrazione segmenti da 255  bytes da record *
      *                  * richieste                                   *
      *                  *---------------------------------------------*
           move      1                    to   w-stu-rrr-pnt-stu      .
       exe-pgm-bkg-200.
           move      spaces               to   w-stu-rrr-255-byt      .
           move      w-stu-rrr-pnt-stu    to   w-stu-rrr-sav-pnt      .
           unstring  rr                   into w-stu-rrr-255-byt
                                  with pointer w-stu-rrr-pnt-stu      .
           move      w-stu-rrr-255-byt    to   b-chr                  .
           if        w-stu-rrr-pnt-stu    =    w-stu-rrr-sav-pnt
                     go to exe-pgm-bkg-400.
           move      "PT"                 to   b-ope                  .
           call      "swd/mod/prg/obj/mbckgr"
                                         using b                      .
           go to     exe-pgm-bkg-200.
       exe-pgm-bkg-400.
      *                  *---------------------------------------------*
      *                  * Fine scrittura record richieste             *
      *                  *---------------------------------------------*
           move      "CL"                 to   b-ope                  .
           call      "swd/mod/prg/obj/mbckgr"
                                         using b                      .
      *                      *-----------------------------------------*
      *                      * Se errore : uscita                      *
      *                      *-----------------------------------------*
            if       b-rsc                not  = spaces
                     go to exe-pgm-bkg-900.
      *              *-------------------------------------------------*
      *              * Lancio del programma di esecuzione  background  *
      *              * tramite chiamata al modulo di segreteria        *
      *              *-------------------------------------------------*
           move      "B+"                 to   s-ope                  .
           move      i-exe-pro            to   s-npb                  .
           move      i-exe-pat            to   s-pmo                  .
           move      i-ide-des            to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
       exe-pgm-bkg-900.
      *              *-------------------------------------------------*
      *              * Cancel del modulo "mbckgr"                      *
      *              *-------------------------------------------------*
           cancel    "swd/mod/prg/obj/mbckgr"                         .
      *              *-------------------------------------------------*
      *              * Cancel del modulo "mmessg"                      *
      *              *-------------------------------------------------*
           cancel    "swd/mod/prg/obj/mmessg"                         .
       exe-pgm-bkg-999.
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
      *              * Determinazione codici dipendenze per l'azienda  *
      *              *-------------------------------------------------*
           move      "IG"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      "DA"                 to   w-dpz-tip-ope          .
           move      s-ter                to   w-dpz-ide-ter          .
           move      s-ute                to   w-dpz-ide-ute          .
           move      s-azi                to   w-dpz-ide-azi          .
           move      s-sap                to   w-dpz-ide-sap          .
           move      s-arg                to   w-dpz-ide-arg          .
           move      s-set                to   w-dpz-ide-set          .
           move      s-fas                to   w-dpz-ide-fas          .
           move      i-ide-des            to   w-dpz-ide-des          .
           move      s-pro                to   w-dpz-ide-pro          .
           call      "pgm/azi/prg/obj/pazi000d"
                                         using w-dpz                  .
           cancel    "pgm/azi/prg/obj/pazi000d"                       .
      *              *-------------------------------------------------*
      *              * Se zero dipendenze : errore ed uscita           *
      *              *-------------------------------------------------*
           if        w-dpz-ctr-dpz        >    zero
                     go to pre-exe-pgm-120.
           move      "EN"                 to   w-dpz-tip-ope          .
           call      "pgm/azi/prg/obj/pazi000d"
                                         using w-dpz                  .
           cancel    "pgm/azi/prg/obj/pazi000d"                       .
           move      "#"                  to   w-cnt-pre-exe-pgm      .
           go to     pre-exe-pgm-999.
       pre-exe-pgm-120.
      *              *-------------------------------------------------*
      *              * Selezione codice dipendenza per il programma    *
      *              *-------------------------------------------------*
           move      "SD"                 to   w-dpz-tip-ope          .
           call      "pgm/azi/prg/obj/pazi000d"
                                         using w-dpz                  .
           cancel    "pgm/azi/prg/obj/pazi000d"                       .
       pre-exe-pgm-180.
      *              *-------------------------------------------------*
      *              * Se scelta non effettuata : uscita               *
      *              *-------------------------------------------------*
           if        w-dpz-cod-prg        =    zero
                     move  "#"            to   w-cnt-pre-exe-pgm
                     go to pre-exe-pgm-999.
      *              *-------------------------------------------------*
      *              * Altrimenti memorizzazione in campo selezione    *
      *              *-------------------------------------------------*
           move      w-dpz-cod-prg        to   rr-cod-dpz             .
       pre-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Preparazione tipo funzionamento programma                 *
      *    *-----------------------------------------------------------*
       pre-tip-fun-000.
      *              *-------------------------------------------------*
      *              * Si/No richieste ad utente                       *
      *              *-------------------------------------------------*
           move      "S"                  to   w-cnt-fun-snx-ric      .
      *              *-------------------------------------------------*
      *              * Si/No richiesta di selezione stampa             *
      *              *-------------------------------------------------*
           move      "S"                  to   w-cnt-fun-snx-stp      .
       pre-tip-fun-999.
           exit.

      *    *===========================================================*
      *    * Open files per richieste                                  *
      *    *-----------------------------------------------------------*
       rou-opn-fls-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-rou-opn-fls      .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice vettore         *
      *              *-------------------------------------------------*
           perform   cod-mne-vet-opn-000  thru cod-mne-vet-opn-999    .
      *              *-------------------------------------------------*
      *              * [vet]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/bol/fls/ioc/obj/iofvet"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-vet                 .
       rou-opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Close files per richieste                                 *
      *    *-----------------------------------------------------------*
       rou-cls-fls-000.
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice vettore        *
      *              *-------------------------------------------------*
           perform   cod-mne-vet-cls-000  thru cod-mne-vet-cls-999    .
      *              *-------------------------------------------------*
      *              * [vet]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/bol/fls/ioc/obj/iofvet"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-vet                 .
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
      *                  * Visualizzazione titolo programma            *
      *                  *---------------------------------------------*
           perform   vis-tit-pgm-000      thru vis-tit-pgm-999        .
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
      *                  * Data max                                    *
      *                  *---------------------------------------------*
           perform   acc-dat-max-000      thru acc-dat-max-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
       acc-ric-sel-200.
      *                  *---------------------------------------------*
      *                  * Codice vettore                              *
      *                  *---------------------------------------------*
           perform   acc-cod-vet-000      thru acc-cod-vet-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-100.
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
           move      "S"                  to   v-alf                  .
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
      *              * Data max                                        *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Data documenti             :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Codice vettore                                  *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice vettore             :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo selezione : Data max                   *
      *    *-----------------------------------------------------------*
       acc-dat-max-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione eventuale default              *
      *                  *---------------------------------------------*
           if        rr-dat-max           not  = zero
                     go to acc-dat-max-100.
      *                  *---------------------------------------------*
      *                  * Date and time da segreteria                 *
      *                  *---------------------------------------------*
           move      "DT"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-dat                to   rr-dat-max             .
       acc-dat-max-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      05                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           if        w-cnt-sts-imp-ric    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      rr-dat-max           to   v-dat                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-dat-max-999.
       acc-dat-max-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-dat                to   rr-dat-max             .
      *              *-------------------------------------------------*
      *              * Se Find                                         *
      *              *-------------------------------------------------*
           if        v-key                not  = "FIND"
                     go to acc-dat-max-400.
      *                  *---------------------------------------------*
      *                  * Find su archivio [bit]                      *
      *                  *---------------------------------------------*
           perform   fnd-arc-bit-000      thru fnd-arc-bit-999        .
           go to     acc-dat-max-100.
       acc-dat-max-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-dat-max-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-dat-max-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-dat-max-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-dat-max-100.
       acc-dat-max-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo selezione : Codice vettore             *
      *    *-----------------------------------------------------------*
       acc-cod-vet-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cod-vet-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-vet-ope      .
           move      rr-cod-vet           to   w-cod-mne-vet-cod      .
           move      07                   to   w-cod-mne-vet-lin      .
           move      30                   to   w-cod-mne-vet-pos      .
           move      07                   to   w-cod-mne-vet-rln      .
           move      41                   to   w-cod-mne-vet-rps      .
           move      08                   to   w-cod-mne-vet-vln      .
           move      41                   to   w-cod-mne-vet-vps      .
           move      09                   to   w-cod-mne-vet-lln      .
           move      41                   to   w-cod-mne-vet-lps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           if        w-cnt-sts-imp-ric    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           perform   cod-mne-vet-cll-000  thru cod-mne-vet-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-mne-vet-foi-000  thru cod-mne-vet-foi-999    .
       acc-cod-vet-110.
           perform   cod-mne-vet-cll-000  thru cod-mne-vet-cll-999    .
           if        w-cod-mne-vet-ope    =    "F+"
                     go to acc-cod-vet-115.
           if        w-cod-mne-vet-ope    =    "AC"
                     go to acc-cod-vet-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-vet-115.
           perform   cod-mne-vet-foi-000  thru cod-mne-vet-foi-999    .
           go to     acc-cod-vet-110.
       acc-cod-vet-120.
           move      w-cod-mne-vet-cod    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-cod-vet-999.
       acc-cod-vet-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-cod-vet             .
       acc-cod-vet-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura archivio [vet]                      *
      *                  *---------------------------------------------*
           move      rr-cod-vet           to   w-let-arc-vet-cod      .
           perform   let-arc-vet-000      thru let-arc-vet-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione dati vettore                 *
      *                  *---------------------------------------------*
           move      w-let-arc-vet-rag    to   rr-cod-vet-rag
           move      w-let-arc-vet-via    to   rr-cod-vet-via
           move      w-let-arc-vet-loc    to   rr-cod-vet-loc         .
      *                  *---------------------------------------------*
      *                  * Visualizzazione dati vettore                *
      *                  *---------------------------------------------*
           perform   vis-rag-vet-000      thru vis-rag-vet-999        .
      *                  *---------------------------------------------*
      *                  * Se codice errato : reimpostazione           *
      *                  *---------------------------------------------*
           if        w-let-arc-vet-flg    not  = spaces
                     go to acc-cod-vet-100.
      *                  *---------------------------------------------*
      *                  * Se codice a zero : reimpostazione           *
      *                  *---------------------------------------------*
           if        rr-cod-vet           =    zero
                     go to acc-cod-vet-100.
       acc-cod-vet-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cod-vet-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-cod-vet-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-cod-vet-100.
       acc-cod-vet-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Codice vettore            *
      *    *-----------------------------------------------------------*
       vis-cod-vet-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      07                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-cod-vet           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-vet-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Ragione sociale vettore   *
      *    *-----------------------------------------------------------*
       vis-rag-vet-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      rr-cod-vet-rag       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      rr-cod-vet-via       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      rr-cod-vet-loc       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-rag-vet-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Anagrafica cliente                   *
      *    *-----------------------------------------------------------*
       acc-ana-cli-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-ana-cli-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-ana-cli-lun    to   v-car                  .
           move      w-exp-ana-cli-num    to   v-ldt                  .
           move      "RI#"                to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      11                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-ana-cli-tbl    to   v-txt                  .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-ric    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      rr-ana-cli           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-ana-cli-999.
       acc-ana-cli-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-ana-cli             .
       acc-ana-cli-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
           if        rr-ana-cli           =    zero
                     go to acc-ana-cli-100.
       acc-ana-cli-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-ana-cli-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-ana-cli-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-ana-cli-100.
       acc-ana-cli-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Tipo stampa                          *
      *    *-----------------------------------------------------------*
       acc-tip-stp-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-tip-stp-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-stp-lun    to   v-car                  .
           move      w-exp-tip-stp-num    to   v-ldt                  .
           move      "TN#"                to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      13                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-tip-stp-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-ric    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      rr-tip-stp           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-tip-stp-999.
       acc-tip-stp-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-tip-stp             .
       acc-tip-stp-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
           if        rr-tip-stp           =    zero
                     go to acc-tip-stp-100.
       acc-tip-stp-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-tip-stp-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-tip-stp-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-tip-stp-100.
       acc-tip-stp-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Data consegna                        *
      *    *-----------------------------------------------------------*
       acc-dat-cns-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-dat-cns-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-dat-cns-lun    to   v-car                  .
           move      w-exp-dat-cns-num    to   v-ldt                  .
           move      "IA#"                to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      15                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-dat-cns-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-ric    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      rr-dat-cns           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-dat-cns-999.
       acc-dat-cns-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-dat-cns             .
       acc-dat-cns-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
           if        rr-dat-cns           =    zero
                     go to acc-dat-cns-100.
       acc-dat-cns-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-dat-cns-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-dat-cns-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-dat-cns-100.
       acc-dat-cns-999.
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
      *              * Test su data max                                *
      *              *-------------------------------------------------*
           if        rr-dat-max           not  = zero
                     go to tdo-ric-sel-200.
           move      "Manca la data dei documenti !"
                                          to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-200.
      *              *-------------------------------------------------*
      *              * Test su codice vettore                          *
      *              *-------------------------------------------------*
           if        rr-cod-vet           not  = zero
                     go to tdo-ric-sel-800.
           move      "Manca il vettore !"
                                          to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-800.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     tdo-ric-sel-999.
       tdo-ric-sel-900.
      *              *-------------------------------------------------*
      *              * Trattamento errore                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione messaggio di errore         *
      *                  *---------------------------------------------*
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                  *---------------------------------------------*
      *                  * Flag di uscita ad errore                    *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cnt-tdo-ric-flg      .
       tdo-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Regolarizzazione dei parametri di selezione               *
      *    *-----------------------------------------------------------*
       reg-ric-sel-000.
      *              *-------------------------------------------------*
      *              * Anagrafica clienti                              *
      *              *-------------------------------------------------*
           if        rr-ana-cli           =    zero
                     move  01             to   rr-ana-cli             .
      *              *-------------------------------------------------*
      *              * Tipo stampa                                     *
      *              *-------------------------------------------------*
           if        rr-tip-stp           =    zero
                     move  01             to   rr-tip-stp             .
      *              *-------------------------------------------------*
      *              * Data consegna                                   *
      *              *-------------------------------------------------*
           if        rr-dat-cns           =    zero
                     move  01             to   rr-dat-cns             .
       reg-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione richieste di selezione                    *
      *    *-----------------------------------------------------------*
       nor-ric-sel-000.
           move      zero                 to   rr-dat-max             .
           move      zero                 to   rr-cod-vet             .
           move      spaces               to   rr-cod-vet-rag         .
           move      spaces               to   rr-cod-vet-via         .
           move      spaces               to   rr-cod-vet-loc         .
           move      zero                 to   rr-ana-cli             .
           move      zero                 to   rr-tip-stp             .
           move      zero                 to   rr-dat-cns             .
       nor-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Find su archivio [bit]                                    *
      *    *-----------------------------------------------------------*
       fnd-arc-bit-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-fnd-arc-bit-sel      .
      *              *-------------------------------------------------*
      *              * Test se programma di interrogazione gia' attivo *
      *              *-------------------------------------------------*
           move      "P?"                 to   s-ope                  .
           move      "pbol3010"           to   s-pro                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-liv                not  = zero
                     move  "#"            to   w-fnd-arc-bit-sel
                     go to  fnd-arc-bit-999.
      *              *-------------------------------------------------*
      *              * Preparazione variabile di i.p.c. per codice di- *
      *              * pendenza                                        *
      *              *-------------------------------------------------*
           move      "PV"                 to   s-ope                  .
           move      "cod-dpz"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "N"                  to   s-tip                  .
           move      02                   to   s-car                  .
           move      zero                 to   s-dec                  .
           move      spaces               to   s-sgn                  .
           move      rr-cod-dpz           to   s-num                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Richiamo programma di interrogazione            *
      *              *-------------------------------------------------*
           move      "pgm/bol/prg/obj/pbol3010"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat                                            .
           cancel    s-pat                                            .
       fnd-arc-bit-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [vet]                         *
      *    *-----------------------------------------------------------*
       let-arc-vet-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-vet-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice a zero                           *
      *              *-------------------------------------------------*
           if        w-let-arc-vet-cod    =    zero
                     go to let-arc-vet-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODVET"             to   f-key                  .
           move      w-let-arc-vet-cod    to   rf-vet-cod-vet         .
           move      "pgm/bol/fls/ioc/obj/iofvet"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-vet                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-vet-400.
       let-arc-vet-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-vet-rag-soc       to   w-let-arc-vet-rag      .
           move      rf-vet-via-vet       to   w-let-arc-vet-via      .
           move      rf-vet-loc-vet       to   w-let-arc-vet-loc      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-vet-999.
       let-arc-vet-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-vet-flg      .
           move      all   "."            to   w-let-arc-vet-rag      .
           go to     let-arc-vet-600.
       let-arc-vet-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-vet-rag      .
       let-arc-vet-600.
           move      spaces               to   w-let-arc-vet-via      .
           move      spaces               to   w-let-arc-vet-loc      .
       let-arc-vet-999.
           exit.

      *    *===========================================================*
      *    * Preparazione parametri per selezione stampa               *
      *    *-----------------------------------------------------------*
       pre-prm-stp-000.
      *              *-------------------------------------------------*
      *              * Flags di tipo selezione                         *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-stp-tip-sel      .
      *              *-------------------------------------------------*
      *              * Codice stampante                                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-stp-cod-stp      .
      *              *-------------------------------------------------*
      *              * Tipo di stampa                                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-stp-tip-sta      .
      *              *-------------------------------------------------*
      *              * Codice modulo                                   *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-stp-cod-mod      .
      *              *-------------------------------------------------*
      *              * Tipo modulo                                     *
      *              *   - L : Libero                                  *
      *              *   - T : Tipografico                             *
      *              *-------------------------------------------------*
           move      "L"                  to   w-cnt-stp-tip-mod      .
      *              *-------------------------------------------------*
      *              * Ampiezza linea di stampa in caratteri           *
      *              *-------------------------------------------------*
           move      120                  to   w-cnt-stp-amp-lin      .
      *              *-------------------------------------------------*
      *              * Top margin in linee                             *
      *              *-------------------------------------------------*
           move      1                    to   w-cnt-stp-top-lin      .
      *              *-------------------------------------------------*
      *              * Numero linee di stampa minimo                   *
      *              *-------------------------------------------------*
           move      30                   to   w-cnt-stp-lin-min      .
      *              *-------------------------------------------------*
      *              * Bottom margin in linee                          *
      *              *-------------------------------------------------*
           move      1                    to   w-cnt-stp-bot-lin      .
      *              *-------------------------------------------------*
      *              * Ampiezza caratteri                              *
      *              *-------------------------------------------------*
           move      zero                 to   w-cnt-stp-amp-car      .
      *              *-------------------------------------------------*
      *              * Altezza interlinea                              *
      *              *-------------------------------------------------*
           move      zero                 to   w-cnt-stp-alt-int      .
      *              *-------------------------------------------------*
      *              * Area riservata per espansioni future            *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-stp-esp-fut      .
      *              *-------------------------------------------------*
      *              * Area riservata per espansioni speciali          *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-stp-fnz-spc      .
       pre-prm-stp-999.
           exit.

      *    *===========================================================*
      *    * Box per messaggio di errore                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/werrbox0.cps"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice vettore         *
      *    *-----------------------------------------------------------*
           copy      "pgm/bol/prg/cpy/acmnvet0.acs"                   .

