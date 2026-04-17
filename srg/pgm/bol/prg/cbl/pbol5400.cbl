       Identification Division.
       Program-Id.                                 pbol5400           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    bol                 *
      *                                Settore:    con                 *
      *                                   Fase:    bol540              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 20/02/19    *
      *                       Ultima revisione:    NdK del 11/06/24    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Richieste per il programma di stampa bol540 *
      *                    Stampa distinte di invio                    *
      *                                                                *
      *                    Le overlay eseguono le seguenti funzioni :  *
      *                    ------------------------------------------  *
      *                    pbol5400 - Main                             *
      *                    pbol540a - Creazione Distinta di invio      *
      *                    pbol540b - Stampa Distinta                  *
      *                    pbol540m - Manutenzione distinta            *
      *                    pbol540c - Generazione documenti PDF        *
      *                    pbol540d - Invio o re-invio documenti PDF   *
      *                    pbol540e - Cancellazione Distinta           *
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
                     "bol"                                            .
      *        *-------------------------------------------------------*
      *        * Settore gestionale                                    *
      *        *-------------------------------------------------------*
           05  i-ide-set                  pic  x(03) value
                     "con"                                            .
      *        *-------------------------------------------------------*
      *        * Fase gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-fas                  pic  x(06) value
                     "bol540"                                         .
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10) value
                     "pbol5400"                                       .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  i-ide-des                  pic  x(40) value
                     " GESTIONE INVIO 'DDT' IN FORMATO 'PDF'  "       .

      *    *===========================================================*
      *    * Area per il programma di esecuzione                       *
      *    *-----------------------------------------------------------*
       01  i-exe.
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma di esecuzione             *
      *        *-------------------------------------------------------*
           05  i-exe-pro                  pic  x(10)                  .
      *        *-------------------------------------------------------*
      *        * Pathname del programma di esecuzione                  *
      *        *-------------------------------------------------------*
           05  i-exe-pat                  pic  x(40)                  .

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
      *    * Area di definizione della valuta base                     *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/c"                                  .

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
      *        * [bol]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/bol/fls/rec/rfbol"                          .
      *        *-------------------------------------------------------*
      *        * [dbe]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/bol/fls/rec/rfdbe"                          .
      *        *-------------------------------------------------------*
      *        * [zbi]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/bol/fls/rec/rfzbi"                          .
      *        *-------------------------------------------------------*
      *        * [cli]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfcli"                          .
      *        *-------------------------------------------------------*
      *        * [dcc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfdcc"                          .

      *    *===========================================================*
      *    * Work-area richieste per stampa serie 'bol540'             *
      *    *-----------------------------------------------------------*
           copy      "pgm/bol/prg/cpy/pbol5400.pgl"                   .

      *    *===========================================================*
      *    * Work-area per ridefinizione record file [bol]             *
      *    *-----------------------------------------------------------*
       01  w-fil-bol.
      *        *-------------------------------------------------------*
      *        * Ridefinizione tipo record 01                          *
      *        *-------------------------------------------------------*
           05  w-fil-bol-rec.
      *            *---------------------------------------------------*
      *            * Modulo d'invio                                    *
      *            *---------------------------------------------------*
               10  w-fil-mdl-inv          pic  x(20)                  .
      *            *---------------------------------------------------*
      *            * Codice azienda, se unica                          *
      *            *---------------------------------------------------*
               10  w-fil-cod-azi          pic  x(10)                  .
      *            *---------------------------------------------------*
      *            * Server SMTP per l'inoltro                         *
      *            *---------------------------------------------------*
               10  w-fil-srv-smt          pic  x(80)                  .
      *            *---------------------------------------------------*
      *            * Mittente mail aziendale per l'inoltro             *
      *            *---------------------------------------------------*
               10  w-fil-mit-azi          pic  x(80)                  .
      *            *---------------------------------------------------*
      *            * Username di accesso SMTP                          *
      *            *---------------------------------------------------*
               10  w-fil-usr-smt          pic  x(40)                  .
      *            *---------------------------------------------------*
      *            * Password di accesso SMTP                          *
      *            *---------------------------------------------------*
               10  w-fil-pwd-smt          pic  x(40)                  .
      *            *---------------------------------------------------*
      *            * Destinatario mail - SDI                           *
      *            *                                                   *
      *            * N.B. solo per fatture XML                         *
      *            *---------------------------------------------------*
               10  w-fil-dst-sdi          pic  x(80)                  .
      *            *---------------------------------------------------*
      *            * Estensione allegati                               *
      *            *                                                   *
      *            * N.B. solo per fatture XML                         *
      *            *---------------------------------------------------*
               10  w-fil-est-all          pic  x(03)                  .
      *            *---------------------------------------------------*
      *            * Tipo di invio                                     *
      *            *                                                   *
      *            * N.B. solo per fatture PDF                         *
      *            *---------------------------------------------------*
               10  w-fil-tip-inv          pic  x(03)                  .
      *            *---------------------------------------------------*
      *            * Dati trasmittente facoltativi                     *
      *            *                                                   *
      *            * N.B. solo per fatture XML                         *
      *            *---------------------------------------------------*
               10  w-fil-piv-trs          pic  x(16)                  .
               10  w-fil-tel-trs          pic  x(20)                  .
               10  w-fil-eml-trs          pic  x(40)                  .
      *            *---------------------------------------------------*
      *            * Area libera non utilizzata                        *
      *            *---------------------------------------------------*
               10  w-fil-alx-exp.
                   15  filler occurs 368  pic  x(01)                  .

      *    *===========================================================*
      *    * Work-area referenze                                       *
      *    *-----------------------------------------------------------*
       01  w-ref.
      *        *-------------------------------------------------------*
      *        * Referenze relative relative ai parametri di invio     *
      *        * tramite mail delle bolle in formato PDF               *
      *        *-------------------------------------------------------*
           05  w-ref-prm-inv.
               10  w-ref-prm-inv-ctr      pic  9(01)                  .
               10  w-ref-prm-inv-max      pic  9(01)      value 6     .
               10  w-ref-prm-inv-azi      pic  x(04)                  .
               10  w-ref-prm-inv-dms      pic  x(80)                  .
               10  w-ref-prm-inv-dpm      pic  x(80)                  .
               10  w-ref-prm-inv-usr      pic  x(80)                  .
               10  w-ref-prm-inv-pwd      pic  x(80)                  .
               10  w-ref-prm-inv-cst   occurs  7.
                   15  w-ref-prm-inv-cpr.
                       20  w-ref-prm-inv-prm
                                          pic  x(80)                  .

      *    *===========================================================*
      *    * Work area per Det                                         *
      *    *-----------------------------------------------------------*
       01  w-det.
      *        *-------------------------------------------------------*
      *        * Determinazione ultimo numero distinta                 *
      *        *-------------------------------------------------------*
           05  w-det-ult-num-dst          pic  9(09)                  .

      *    *===========================================================*
      *    * Work per subroutines di Find                              *
      *    *-----------------------------------------------------------*
       01  w-fnd.
      *        *-------------------------------------------------------*
      *        * Work per Find su archivio [dbe]                       *
      *        *-------------------------------------------------------*
           05  w-fnd-arc-dst.
               10  w-fnd-arc-dst-sel      pic  x(01)                  .
               10  w-fnd-arc-dst-prt      pic  9(09)                  .
               
      *    *===========================================================*
      *    * Work per subroutines di Let                               *
      *    *-----------------------------------------------------------*
       01  w-let.
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [dbe]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-dst.
               10  w-let-arc-dst-flg      pic  x(01)                  .
               10  w-let-arc-dst-prt      pic  9(09)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [zbi]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-zbi.
               10  w-let-arc-zbi-flg      pic  x(01)                  .
               10  w-let-arc-zbi-cod      pic  x(05)                  .
               10  w-let-arc-zbi-dpz      pic  9(02)                  .
               10  w-let-arc-zbi-des      pic  x(30)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [cli]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-cli.
               10  w-let-arc-cli-flg      pic  x(01)                  .
               10  w-let-arc-cli-cod      pic  9(07)                  .
               10  w-let-arc-cli-rag      pic  x(40)                  .
               10  w-let-arc-cli-via      pic  x(40)                  .
               10  w-let-arc-cli-loc      pic  x(40)                  .
               10  w-let-arc-cli-ass      pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [dcc]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-dcc.
               10  w-let-arc-dcc-flg      pic  x(01)                  .
               10  w-let-arc-dcc-cli      pic  9(07)                  .
               10  w-let-arc-dcc-dpz      pic  x(04)                  .
               10  w-let-arc-dcc-rag      pic  x(40)                  .
               10  w-let-arc-dcc-via      pic  x(40)                  .
               10  w-let-arc-dcc-loc      pic  x(40)                  .

      *    *===========================================================*
      *    * Work per accettazioni campi espansi                       *
      *    *-----------------------------------------------------------*
       01  w-exp.
      *        *-------------------------------------------------------*
      *        * Work per : Tipo di operazione                         *
      *        *-------------------------------------------------------*
           05  w-exp-tip-ope.
               10  w-exp-tip-ope-num      pic  9(02)       value 8    .
               10  w-exp-tip-ope-lun      pic  9(02)       value 40   .
               10  w-exp-tip-ope-tbl.
                   15  filler             pic  x(40) value
                            "Creazione distinta documenti da inviare ".
                   15  filler             pic  x(40) value
                            "Stampa distinta                         ".
                   15  filler             pic  x(40) value
                            "Manutenzione distinta                   ".
                   15  filler             pic  x(40) value
                            "Generazione documenti PDF               ".
                   15  filler             pic  x(40) value
                            "Invio documenti via e-mail              ".
                   15  filler             pic  x(40) value
                            "Re-invio distinta                       ".
                   15  filler             pic  x(40) value
                            "re-invio documenti con esito Negativo   ".
                   15  filler             pic  x(40) value
                            "Eliminazione distinta                   ".
      *        *-------------------------------------------------------*
      *        * Work per : Esclusione documenti                       *
      *        *-------------------------------------------------------*
           05  w-exp-sne-doc.
               10  w-exp-sne-doc-num      pic  9(02)       value 2    .
               10  w-exp-sne-doc-lun      pic  9(02)       value 2    .
               10  w-exp-sne-doc-tbl.
                   15  filler             pic  x(02) value "Si"       .
                   15  filler             pic  x(02) value "No"       .
      *        *-------------------------------------------------------*
      *        * Work per : Messaggio ratifica cancellazione           *
      *        *-------------------------------------------------------*
           05  w-exp-rat-can.
               10  w-exp-rat-can-num      pic  9(02)       value 2    .
               10  w-exp-rat-can-lun      pic  9(02)       value 50   .
               10  w-exp-rat-can-tbl.
                   15  filler             pic  x(50) value
                  "Confermare la cancellazione                       ".
                   15  filler             pic  x(50) value
                  "Rinunciare all'operazione                         ".
               10  w-exp-rat-can-sce      pic  9(02)                  .

      *    *===========================================================*
      *    * Work-area locale                                          *
      *    *-----------------------------------------------------------*
       01  w-wrk.
      *        *-------------------------------------------------------*
      *        * Numero documento per accettazione                     *
      *        *-------------------------------------------------------*
           05  w-wrk-num-acc              pic  9(09)                  .
           05  w-wrk-num-acc-r            redefines
               w-wrk-num-acc.
               10  w-wrk-num-saa          pic  9(03)                  .
               10  w-wrk-num-npg          pic  9(06)                  .
      *        *-------------------------------------------------------*
      *        * Numero documento interno                              *
      *        *-------------------------------------------------------*
           05  w-wrk-doc-int              pic  9(11)                  .
           05  w-wrk-doc-int-r            redefines
               w-wrk-doc-int.
               10  w-wrk-doc-saa          pic  9(03)                  .
               10  w-wrk-doc-dpz          pic  9(02)                  .
               10  w-wrk-doc-npg          pic  9(06)                  .

      *    *===========================================================*
      *    * Work-area per salvataggi                                  *
      *    *-----------------------------------------------------------*
       01  w-sav.
           05  w-sav-cod-cli              pic  9(07)                  .

      *    *===========================================================*
      *    * Work-area per comodi di accettazione                      *
      *    *-----------------------------------------------------------*
       01  w-acc.
      *        *-------------------------------------------------------*
      *        * Area per impostazione serie elementi da selezionare   *
      *        *-------------------------------------------------------*
           05  w-acc-ser-edd.
               10  w-acc-ser-edd-max      pic  9(02) value 36         .
               10  w-acc-ser-edd-pev      pic  9(02)                  .
               10  w-acc-ser-edd-nel      pic  9(02)                  .
               10  w-acc-ser-edd-n1v      pic  9(02)                  .
               10  w-acc-ser-edd-nev      pic  9(02)                  .
               10  w-acc-ser-edd-nec      pic  9(02)                  .
               10  w-acc-ser-edd-fce      pic  x(01)                  .
               10  w-acc-ser-edd-led.
                   15  w-acc-ser-edd-rig  pic  x(03)                  .
                   15  w-acc-ser-edd-dpu  pic  x(01)                  .
                   15  filler             pic  x(02)                  .
                   15  w-acc-ser-edd-cod  pic  x(07)                  .
                   15  filler             pic  x(05)                  .
                   15  w-acc-ser-edd-des  pic  x(40)                  .
                   15  filler             pic  x(18)                  .
               10  w-acc-ser-edd-c01      pic  9(02)                  .
               10  w-acc-ser-edd-c02      pic  9(02)                  .
               10  w-acc-ser-edd-c0a      pic  9(02)                  .
               10  w-acc-ser-edd-c0b      pic  9(02)                  .
               10  w-acc-ser-edd-c0c      pic  9(02)                  .
               10  w-acc-ser-edd-c0p      pic  9(02)                  .
               10  w-acc-ser-edd-c0q      pic  9(02)                  .
               10  w-acc-ser-edd-c0r      pic  9(02)                  .
               10  w-acc-ser-edd-spe      pic  9(07)                  .
               10  w-acc-ser-edd-svk      pic  x(04)                  .
               10  w-acc-ser-edd-stu      pic  x(01)                  .
               10  w-acc-ser-edd-fcl.
                   15  filler             pic  x(08)                  .
                   15  w-acc-ser-edd-070  pic  x(70)                  .
                   15  filler             pic  x(02)                  .
               10  w-acc-ser-edd-txt.
                   15  w-acc-ser-edd-rtr  occurs 03
                                          pic  x(40)                  .
               10  w-acc-ser-edd-ltp.
                   15  filler             pic  x(07) value "Pagina "  .
                   15  w-acc-ser-edd-lt1  pic  9(01)                  .
                   15  filler             pic  x(04) value " di "     .
                   15  w-acc-ser-edd-lt2  pic  9(01)                  .

      *    *===========================================================*
      *    * Area di interfaccia per sottoprogramma         "pazi000d" *
      *    *-----------------------------------------------------------*
           copy      "pgm/azi/prg/cpy/pazi000d.pgl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice tipo movimento per la   *
      *    * bollettazione                                             *
      *    *-----------------------------------------------------------*
           copy      "pgm/bol/prg/cpy/acdezbi0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice cliente commerciale     *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcc/prg/cpy/acmndcc0.acl"                   .

      *    *===========================================================*
      *    * Work per subroutines di Err                               *
      *    *-----------------------------------------------------------*
       01  w-err.
      *        *-------------------------------------------------------*
      *        * Work per Err con box centrale, o per messaggi centra- *
      *        * li circondati da un box                               *
      *        *-------------------------------------------------------*
           05  w-err-box-err.
               10  w-err-box-err-msg      pic  x(65)                  .
               10  w-err-box-err-m02      pic  x(65)                  .

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
      *                      *-----------------------------------------*
      *                      * Test se foreground                      *
      *                      *-----------------------------------------*
           if        s-snb                =    "B"
                     go to main-500.
      *                      *-----------------------------------------*
      *                      * Esecuzione in foreground                *
      *                      *-----------------------------------------*
           perform   exe-pgm-frg-000      thru exe-pgm-frg-999        .
      *                      *-----------------------------------------*
      *                      * Ad accettazione richieste               *
      *                      *-----------------------------------------*
           go to     main-250.
       main-500.
      *                  *---------------------------------------------*
      *                  * Se esecuzione in background                 *
      *                  *                                             *
      *                  * Disabilitato : forzatura foreground         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Esecuzione in foreground                *
      *                      *-----------------------------------------*
           perform   exe-pgm-frg-000      thru exe-pgm-frg-999        .
      *                      *-----------------------------------------*
      *                      * Ad accettazione richieste               *
      *                      *-----------------------------------------*
           go to     main-250.
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
           if        rr-tip-ope           =    01
                     move  "pgm/bol/prg/obj/pbol540a                "
                                          to   i-exe-pat
           else if   rr-tip-ope           =    02
                     move  "pgm/bol/prg/obj/pbol540b                "
                                          to   i-exe-pat
           else if   rr-tip-ope           =    03
                     move  "pgm/bol/prg/obj/pbol540m                "
                                          to   i-exe-pat
           else if   rr-tip-ope           =    04
                     move  "pgm/bol/prg/obj/pbol540c                "
                                          to   i-exe-pat
           else if   rr-tip-ope           =    05
                     move  "pgm/bol/prg/obj/pbol540d                "
                                          to   i-exe-pat
           else if   rr-tip-ope           =    06
                     move  "pgm/bol/prg/obj/pbol540d                "
                                          to   i-exe-pat
           else if   rr-tip-ope           =    07
                     move  "pgm/bol/prg/obj/pbol540d                "
                                          to   i-exe-pat
           else if   rr-tip-ope           =    08
                     move  "pgm/bol/prg/obj/pbol540e                "
                                          to   i-exe-pat              .
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
      *    * Routine pre-esecuzione programma                          *
      *    *-----------------------------------------------------------*
       pre-exe-pgm-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-pre-exe-pgm      .
       pre-exe-pgm-020.
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
      *              * Altrimenti memorizzazione in campo richieste    *
      *              *-------------------------------------------------*
           move      w-dpz-cod-prg        to   rr-dpz-inu             .
       pre-exe-pgm-400.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazioni                       *
      *              *-------------------------------------------------*
       pre-exe-pgm-500.
      *              *-------------------------------------------------*
      *              * Lettura referenze                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Parametri per l'invio                       *
      *                  *---------------------------------------------*
           perform   ref-prm-inv-000      thru ref-prm-inv-999        .
      *                  *---------------------------------------------*
      *                  * Test se valori esistenti                    *
      *                  *---------------------------------------------*
           if        w-ref-prm-inv-prm (3)
                                          not  = spaces and
                     w-ref-prm-inv-prm (4)
                                          not  = spaces
                     go to pre-exe-pgm-800.
      *                  *---------------------------------------------*
      *                  * Messaggio di errore                         *
      *                  *---------------------------------------------*
           move      "ATTENZIONE : mancano le referenze dei parametri pe
      -              "r la posta.   "     to   w-err-box-err-msg      .
      *
           move      "Il programma pertanto non e' eseguibile!          
      -              "            "       to   w-err-box-err-m02      .
           perform   box-msg-e02-000      thru box-msg-e02-999        .
      *                  *---------------------------------------------*
      *                  * Uscita con errore                           *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cnt-pre-exe-pgm      .
           go to     pre-exe-pgm-999.
       pre-exe-pgm-800.
      *              *-------------------------------------------------*
      *              * Parametri letti in area comune                  *
      *              *-------------------------------------------------*
           move      w-ref-prm-inv-prm (1)
                                          to   rr-prm-inv (1)         .
           move      w-ref-prm-inv-prm (2)
                                          to   rr-prm-inv (2)         .
           move      w-ref-prm-inv-prm (3)
                                          to   rr-prm-inv (3)         .
           move      w-ref-prm-inv-prm (4)
                                          to   rr-prm-inv (4)         .
           move      w-ref-prm-inv-prm (5)
                                          to   rr-prm-inv (5)         .
           move      w-ref-prm-inv-prm (6)
                                          to   rr-prm-inv (6)         .
           move      w-ref-prm-inv-prm (7)
                                          to   rr-prm-inv (7)         .
       pre-exe-pgm-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     pre-exe-pgm-999.
       pre-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Lettura delle referenze relative ai parametri di invio    *
      *    * tramite mail delle fatture in formato PDF                 *
      *    *-----------------------------------------------------------*
       ref-prm-inv-000.
      *              *-------------------------------------------------*
      *              * Informazioni da segreteria                      *
      *              *-------------------------------------------------*
           move      "IG"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-azi                to   w-ref-prm-inv-azi      .
      *              *-------------------------------------------------*
      *              * Lettura della personalizzazione relativa al     *
      *              * mail server                                     *
      *              *-------------------------------------------------*
           move      "PS"                 to   s-ope                  .
           move      "msa-isp"            to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-alf                to   w-ref-prm-inv-dms      .
      *              *-------------------------------------------------*
      *              * Lettura della personalizzazione relativa al     *
      *              * postmaster                                      *
      *              *-------------------------------------------------*
           move      "PS"                 to   s-ope                  .
           move      "eml-psm"            to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-alf                to   w-ref-prm-inv-dpm      .
      *              *-------------------------------------------------*
      *              * Lettura della personalizzazione relativa allo   *
      *              * User ID di account Server SMTP                  *
      *              *-------------------------------------------------*
           move      "PS"                 to   s-ope                  .
           move      "uid-ssm"            to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-alf                to   w-ref-prm-inv-usr      .
      *              *-------------------------------------------------*
      *              * Lettura della personalizzazione relativa alla   *
      *              * password di account Server SMTP                 *
      *              *-------------------------------------------------*
           move      "PS"                 to   s-ope                  .
           move      "pwd-ssm"            to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-alf                to   w-ref-prm-inv-pwd      .
       ref-prm-inv-100.
      *              *-------------------------------------------------*
      *              * Caricamento iniziale delle personalizzazioni    *
      *              * relative ai parametri di invio                  *
      *              *-------------------------------------------------*
           perform   prs-prm-bol-000      thru prs-prm-bol-999        .
      *              *-------------------------------------------------*
      *              * Normalizzazione dei valori letti                *
      *              *-------------------------------------------------*
           if        w-fil-mdl-inv        =    spaces
                     move  "t_sender"     to   w-ref-prm-inv-prm (1)
           else      move  w-fil-mdl-inv  to   w-ref-prm-inv-prm (1)  .
      *
           if        w-fil-cod-azi        =    spaces
                     move  w-ref-prm-inv-azi
                                          to   w-ref-prm-inv-prm (2)
           else      move  w-fil-cod-azi  to   w-ref-prm-inv-prm (2)  .
      *
           if        w-fil-srv-smt        =    spaces
                     move  w-ref-prm-inv-dms
                                          to   w-ref-prm-inv-prm (3)
           else      move  w-fil-srv-smt  to   w-ref-prm-inv-prm (3)  .
      *
           if        w-fil-mit-azi        =    spaces
                     move  w-ref-prm-inv-dpm
                                          to   w-ref-prm-inv-prm (4)
           else      move  w-fil-mit-azi  to   w-ref-prm-inv-prm (4)  .
      *
           if        w-fil-usr-smt        =    spaces
                     move  w-ref-prm-inv-usr
                                          to   w-ref-prm-inv-prm (5)
           else      move  w-fil-usr-smt  to   w-ref-prm-inv-prm (5)  .
      *
           if        w-fil-pwd-smt        =    spaces
                     move  w-ref-prm-inv-pwd
                                          to   w-ref-prm-inv-prm (6)
           else      move  w-fil-pwd-smt  to   w-ref-prm-inv-prm (6)  .
      *
           if        w-fil-tip-inv        =    spaces
                     move  "eml"          to   w-ref-prm-inv-prm (7)
           else      move  w-fil-tip-inv  to   w-ref-prm-inv-prm (7)  .
       ref-prm-inv-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     ref-prm-inv-999.
       ref-prm-inv-999.
           exit.

      *    *===========================================================*
      *    * Caricamento iniziale della personalizzazione per si/no    *
      *    * aggiornamenti contabili per movimenti di portafoglio      *
      *    *-----------------------------------------------------------*
       prs-prm-bol-000.
      *              *-------------------------------------------------*
      *              * Open file [bol]                                 *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/bol/fls/ioc/obj/iofbol"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bol                 .
       prs-prm-bol-100.
      *              *-------------------------------------------------*
      *              * Lettura record personalizzazioni                *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODBOL    "         to   f-key                  .
           move      01                   to   rf-bol-tip-rec         .
           move      "bol540    "         to   rf-bol-cod-rec         .
           move      "pgm/bol/fls/ioc/obj/iofbol"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bol                 .
      *              *-------------------------------------------------*
      *              * Deviazione secondo l'esito della lettura        *
      *              *-------------------------------------------------*
           if        f-sts                =    e-not-err
                     go to prs-prm-bol-200
           else      go to prs-prm-bol-400.
       prs-prm-bol-200.
      *              *-------------------------------------------------*
      *              * Se record personalizzazioni esistente           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Dati record in area per ridefinizione delle *
      *                  * personalizzazioni                           *
      *                  *---------------------------------------------*
           move      rf-bol-dat-rec       to   w-fil-bol-rec          .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     prs-prm-bol-900.
       prs-prm-bol-400.
      *              *-------------------------------------------------*
      *              * Se record personalizzazioni non esistente       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     prs-prm-bol-900.
       prs-prm-bol-900.
      *              *-------------------------------------------------*
      *              * Close file [bol]                                *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/bol/fls/ioc/obj/iofbol"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bol                 .
       prs-prm-bol-999.
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
      *              * Open modulo accettazione tipo movimento per la  *
      *              * fatturazione                                    *
      *              *-------------------------------------------------*
           perform   cod-des-zbi-opn-000  thru cod-des-zbi-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice cliente commer- *
      *              * ciale                                           *
      *              *-------------------------------------------------*
           perform   cod-mne-dcc-opn-000  thru cod-mne-dcc-opn-999    .
      *              *-------------------------------------------------*
      *              * [dbe]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/bol/fls/ioc/obj/iofdbe"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dbe                 .
      *              *-------------------------------------------------*
      *              * [zbi]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/bol/fls/ioc/obj/iofzbi"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zbi                 .
       rou-opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Close files per richieste                                 *
      *    *-----------------------------------------------------------*
       rou-cls-fls-000.
      *              *-------------------------------------------------*
      *              * Close modulo accettazione tipo movimento per la *
      *              * fatturazione                                    *
      *              *-------------------------------------------------*
           perform   cod-des-zbi-cls-000  thru cod-des-zbi-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice cliente com-   *
      *              * merciale                                        *
      *              *-------------------------------------------------*
           perform   cod-mne-dcc-cls-000  thru cod-mne-dcc-cls-999    .
      *              *-------------------------------------------------*
      *              * [dbe]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/bol/fls/ioc/obj/iofdbe"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dbe                 .
      *              *-------------------------------------------------*
      *              * [zbi]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/bol/fls/ioc/obj/iofzbi"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zbi                 .
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
      *                  * Tipo di operazione                          *
      *                  *---------------------------------------------*
           perform   acc-tip-ope-000      thru acc-tip-ope-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
       acc-ric-sel-150.
      *                  *---------------------------------------------*
      *                  * Numero distinta                             *
      *                  *---------------------------------------------*
           perform   acc-num-dst-000      thru acc-num-dst-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
       acc-ric-sel-200.
      *                  *---------------------------------------------*
      *                  * Tipo movimento da stampare                  *
      *                  *---------------------------------------------*
           perform   acc-tip-mov-000      thru acc-tip-mov-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-100.
       acc-ric-sel-250.
      *                  *---------------------------------------------*
      *                  * Tipi documento                              *
      *                  *---------------------------------------------*
           perform   acc-tip-doc-000      thru acc-tip-doc-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-150.
       acc-ric-sel-300.
      *                  *---------------------------------------------*
      *                  * Data documento minima da stampare           *
      *                  *---------------------------------------------*
           perform   acc-dat-min-000      thru acc-dat-min-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-250.
       acc-ric-sel-350.
      *                  *---------------------------------------------*
      *                  * Data documento massima da stampare          *
      *                  *---------------------------------------------*
           perform   acc-dat-max-000      thru acc-dat-max-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-300.
       acc-ric-sel-400.
      *                  *---------------------------------------------*
      *                  * Numero bolla minimo                         *
      *                  *---------------------------------------------*
           perform   acc-num-min-000      thru acc-num-min-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-350.
       acc-ric-sel-450.
      *                  *---------------------------------------------*
      *                  * Numero bolla massimo                        *
      *                  *---------------------------------------------*
           perform   acc-num-max-000      thru acc-num-max-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-400.
       acc-ric-sel-500.
      *                  *---------------------------------------------*
      *                  * Codice cliente                              *
      *                  *---------------------------------------------*
           perform   acc-cod-cli-000      thru acc-cod-cli-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-450.
       acc-ric-sel-550.
      *                  *---------------------------------------------*
      *                  * Esclusione documenti                        *
      *                  *---------------------------------------------*
           perform   acc-sne-doc-000      thru acc-sne-doc-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-500.
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
      *              * Tipo di operazione                              *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tipo di operazione         :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Numero distinta                                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Numero distinta            :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Prompt di selezione sui documenti               *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "---------------------------- Selezioni sui documen
      -              "ti ---------------------------"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Prompt tipo movimento da stampare               *
      *              *-------------------------------------------------*
           perform   pmt-tip-mov-000      thru pmt-tip-mov-999        .
      *              *-------------------------------------------------*
      *              * Visualizzazione tipo movimento da stampare      *
      *              *-------------------------------------------------*
           perform   vis-tip-mov-000      thru vis-tip-mov-999        .
      *              *-------------------------------------------------*
      *              * Visualizzazione tipo movimento da stampare,     *
      *              * descrizione                                     *
      *              *-------------------------------------------------*
           perform   vis-tip-mov-des-000  thru vis-tip-mov-des-999    .
      *              *-------------------------------------------------*
      *              * Prompt tipi documento                           *
      *              *-------------------------------------------------*
           perform   pmt-tip-doc-000      thru pmt-tip-doc-999        .
      *              *-------------------------------------------------*
      *              * Visualizzazione tipi documento                  *
      *              *-------------------------------------------------*
           perform   vis-tip-doc-000      thru vis-tip-doc-999        .
      *              *-------------------------------------------------*
      *              * Prompt data documento min - max                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      45                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Data emissione         dal :             al :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Visualizzazione data documento minima da ricer- *
      *              * care                                            *
      *              *-------------------------------------------------*
           perform   vis-dat-min-000      thru vis-dat-min-999        .
      *              *-------------------------------------------------*
      *              * Visualizzazione data documento massima da ricer-*
      *              * care                                            *
      *              *-------------------------------------------------*
           perform   vis-dat-max-000      thru vis-dat-max-999        .
      *              *-------------------------------------------------*
      *              * Numero documento min - max                      *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      45                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Numero DDT             dal :             al :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Codice cliente                                  *
      *              *-------------------------------------------------*
           perform   pmt-cod-cli-000      thru pmt-cod-cli-999        .
      *              *-------------------------------------------------*
      *              * Esclusione documenti                            *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Solo se ancora da inviare  :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Documenti da verificare                         *
      *              *-------------------------------------------------*
       pmt-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Prompt tipo movimento da stampare                         *
      *    *-----------------------------------------------------------*
       pmt-tip-mov-000.
      *              *-------------------------------------------------*
      *              * Prompt                                          *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tipo movimento             :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Tipo documento : codice                         *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-tip-mov           to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Tipo documento : descrizione                    *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      37                   to   v-pos                  .
           move      rr-tip-mov-des       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-tip-mov-999.
           exit.

      *    *===========================================================*
      *    * Prompt tipi documento                                     *
      *    *-----------------------------------------------------------*
       pmt-tip-doc-000.
       pmt-tip-doc-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per Codice cliente                *
      *    *-----------------------------------------------------------*
       pmt-cod-cli-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice cliente             :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cod-cli-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Tipo di operazione                   *
      *    *-----------------------------------------------------------*
       acc-tip-ope-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-tip-ope-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-ope-lun    to   v-car                  .
           move      w-exp-tip-ope-num    to   v-ldt                  .
           move      "CSMGIRNE#"          to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      05                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-tip-ope-tbl    to   v-txt                  .
           move      rr-tip-ope           to   v-num                  .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-tip-ope-999.
       acc-tip-ope-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-tip-ope             .
       acc-tip-ope-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se zero : reimpostazione                    *
      *                  *---------------------------------------------*
           if        rr-tip-ope           not  = zero
                     go to acc-tip-ope-600.
           if        v-key                =    "UP  "
                     go to acc-tip-ope-600
           else      go to acc-tip-ope-100.
       acc-tip-ope-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-tip-ope-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-tip-ope-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-tip-ope-100.
       acc-tip-ope-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo selezione : Numero distinta            *
      *    *-----------------------------------------------------------*
       acc-num-dst-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        rr-tip-ope           =    01
                     go to acc-num-dst-999.
      *                  *---------------------------------------------*
      *                  * Eventuale default                           *
      *                  *---------------------------------------------*
           if        rr-num-dst           not  = zero
                     go to acc-num-dst-100.
      *                  *---------------------------------------------*
      *                  * Estrazione di eventuale variabile di i.p.c. *
      *                  *---------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "num-prt"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-num          to   rr-num-dst             .
       acc-num-dst-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "P"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      "<B"                 to   v-edm                  .
           move      07                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
      *
           if        w-cnt-sts-imp-ric    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
      *
           move      "PRSC"               to   v-pfk (07)             .
           move      rr-num-dst           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Cancellazione delle note operative              *
      *              *-------------------------------------------------*
           move      "NT"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-num-dst-999.
       acc-num-dst-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-num-dst             .
       acc-num-dst-300.
      *              *-------------------------------------------------*
      *              * Se Find                                         *
      *              *-------------------------------------------------*
           if        v-key                not  = "FIND"
                     go to acc-num-dst-350.
      *                  *---------------------------------------------*
      *                  * Find su archivio [dbe]                      *
      *                  *---------------------------------------------*
           perform   fnd-arc-dst-000      thru fnd-arc-dst-999        .
           if        w-fnd-arc-dst-sel    not  = spaces
                     go to acc-num-dst-100.
           move      w-fnd-arc-dst-prt    to   rr-num-dst             .
      *                  *---------------------------------------------*
      *                  * Visualizzazione numero distinta             *
      *                  *---------------------------------------------*
           perform   vis-num-dst-000      thru vis-num-dst-999        .
       acc-num-dst-350.
      *              *-------------------------------------------------*
      *              * Se Previous screen                              *
      *              *-------------------------------------------------*
           if        v-key                not  = "PRSC"
                     go to acc-num-dst-400.
      *                  *---------------------------------------------*
      *                  * Determinazione ultimo numero distinta       *
      *                  *---------------------------------------------*
           perform   det-ult-num-000      thru det-ult-num-999        .
           move      w-det-ult-num-dst    to   rr-num-dst             .
      *                  *---------------------------------------------*
      *                  * Visualizzazione ultimo numero distinta      *
      *                  *---------------------------------------------*
           perform   vis-num-dst-000      thru vis-num-dst-999        .
      *                  *---------------------------------------------*
      *                  * Ad accettazione                             *
      *                  *---------------------------------------------*
           go to     acc-num-dst-100.
       acc-num-dst-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura file [dbe]                          *
      *                  *---------------------------------------------*
           move      rr-num-dst           to   w-let-arc-dst-prt      .
           perform   let-arc-dst-000      thru let-arc-dst-999        .
       acc-num-dst-500.
      *                  *---------------------------------------------*
      *                  * Se numero distinta errato : reimpostazione  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test su flag                            *
      *                      *-----------------------------------------*
           if        w-let-arc-dst-flg    =    spaces
                     go to acc-num-dst-600.
      *                      *-----------------------------------------*
      *                      * Messaggio di errore                     *
      *                      *-----------------------------------------*
           move      "Distinta non trovata !"
                                          to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                      *-----------------------------------------*
      *                      * A reimpostazione                        *
      *                      *-----------------------------------------*
           if        v-key                not  = "UP  "
                     go to acc-num-dst-100.
       acc-num-dst-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Eventuale ratifica per Cancellazione        *
      *                  *---------------------------------------------*
           if        rr-tip-ope           not  = 08
                     go to acc-num-dst-800.
      *                  *---------------------------------------------*
      *                  * Box di avvertimento                         *
      *                  *---------------------------------------------*
           perform   acc-num-dst-rat-000  thru acc-num-dst-rat-999    .
      *                  *---------------------------------------------*
      *                  * Test su scelta                              *
      *                  *---------------------------------------------*
           if        w-exp-rat-can-sce    =    02
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-num-dst-999.
       acc-num-dst-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-num-dst-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-num-dst-100.
       acc-num-dst-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo selezione : Numero distinta         *
      *    *-----------------------------------------------------------*
       vis-num-dst-000.
           move      "DS"                 to   v-ope                  .
           move      "P"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      "<B"                 to   v-edm                  .
           move      07                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-num-dst           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-num-dst-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo selezione : Numero distinta            *
      *    *                                                           *
      *    * Routine di ratifica cancellazione                         *
      *    *-----------------------------------------------------------*
       acc-num-dst-rat-000.
      *              *-------------------------------------------------*
      *              * Test se da eseguire                             *
      *              *-------------------------------------------------*
       acc-num-dst-rat-100.
      *              *-------------------------------------------------*
      *              * Box di avviso con conferma                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Salvataggio immagine video                  *
      *                  *---------------------------------------------*
           move      "SV"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Video in Off                                *
      *                  *---------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Box                                         *
      *                  *---------------------------------------------*
           move      "BX"                 to   v-ope                  .
           move      08                   to   v-lin                  .
           move      07                   to   v-pos                  .
           move      18                   to   v-lto                  .
           move      74                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Messaggi nel box                            *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      64                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      09                   to   v-pos                  .
           move      "                       A T T E N Z I O N E        
      -              "              "     to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      64                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      09                   to   v-pos                  .
           move      "Operazione di cancellazione della distinta !      
      -              "              "     to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      13                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      09                   to   v-pos                  .
           move      "Si desidera :"      to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Video in On                                 *
      *                  *---------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Preparazione default per risposta           *
      *                  *---------------------------------------------*
           move      spaces               to   v-alf                  .
           move      zero                 to   w-exp-rat-can-sce      .
       acc-num-dst-rat-120.
      *                  *---------------------------------------------*
      *                  * Accettazione risposta                       *
      *                  *---------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-rat-can-lun    to   v-car                  .
           move      w-exp-rat-can-num    to   v-ldt                  .
           move      "AP#"                to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      15                   to   v-lin                  .
           move      23                   to   v-pos                  .
           move      w-exp-rat-can-tbl    to   v-txt                  .
           move      "EXIT"               to   v-pfk (20)             .
           move      w-exp-rat-can-sce    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Controllo risposta operatore                *
      *                  *---------------------------------------------*
           if        v-key                =    "EXIT"
                     move  01             to   v-num
                     go to acc-num-dst-rat-800.
           if        v-num                not  = 01 and
                     v-num                not  = 02
                     go to acc-num-dst-rat-120.
      *                  *---------------------------------------------*
      *                  * Bufferizzazione risposta operatore          *
      *                  *---------------------------------------------*
           move      v-num                to   w-exp-rat-can-sce      .
       acc-num-dst-rat-800.
      *              *-------------------------------------------------*
      *              * Ripristino immagine video                       *
      *              *-------------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-num-dst-rat-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     acc-num-dst-rat-999.
       acc-num-dst-rat-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo selezione : Tipo movimento             *
      *    *-----------------------------------------------------------*
       acc-tip-mov-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        rr-tip-ope           not  = 01
                     go to acc-tip-mov-999.
       acc-tip-mov-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-des-zbi-ope      .
           move      rr-tip-mov           to   w-cod-des-zbi-cod      .
           move      11                   to   w-cod-des-zbi-lin      .
           move      30                   to   w-cod-des-zbi-pos      .
           move      11                   to   w-cod-des-zbi-dln      .
           move      37                   to   w-cod-des-zbi-dps      .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           if        w-cnt-sts-imp-ric    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           perform   cod-des-zbi-cll-000  thru cod-des-zbi-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-des-zbi-foi-000  thru cod-des-zbi-foi-999    .
       acc-tip-mov-110.
           perform   cod-des-zbi-cll-000  thru cod-des-zbi-cll-999    .
           if        w-cod-des-zbi-ope    =    "F+"
                     go to acc-tip-mov-115.
           if        w-cod-des-zbi-ope    =    "AC"
                     go to acc-tip-mov-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-tip-mov-115.
           perform   cod-des-zbi-foi-000  thru cod-des-zbi-foi-999    .
           go to     acc-tip-mov-110.
       acc-tip-mov-120.
           move      w-cod-des-zbi-cod    to   v-alf                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-tip-mov-999.
       acc-tip-mov-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   rr-tip-mov             .
       acc-tip-mov-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura archivio [zbi]                      *
      *                  *---------------------------------------------*
           move      rr-tip-mov           to   w-let-arc-zbi-cod      .
           move      zero                 to   w-let-arc-zbi-dpz      .
           perform   let-arc-zbi-000      thru let-arc-zbi-999        .
      *                  *---------------------------------------------*
      *                  * Se codice a spaces : normalizzazione della  *
      *                  * descrizione e degli altri parametri         *
      *                  *---------------------------------------------*
           if        rr-tip-mov           =    spaces
                     move  "Tutti i tipi documento        "
                                          to   w-let-arc-zbi-des
                     move  zero           to   w-let-arc-zbi-dpz      .
      *                  *---------------------------------------------*
      *                  * Memorizzazione dati                         *
      *                  *---------------------------------------------*
           move      w-let-arc-zbi-des    to   rr-tip-mov-des         .
           move      w-let-arc-zbi-dpz    to   rr-tip-mov-dpz         .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione                 *
      *                  *---------------------------------------------*
           perform   vis-tip-mov-des-000  thru vis-tip-mov-des-999    .
      *                  *---------------------------------------------*
      *                  * Se codice errato : reimpostazione           *
      *                  *---------------------------------------------*
           if        w-let-arc-zbi-flg    not  = spaces
                     go to acc-tip-mov-100.
       acc-tip-mov-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-tip-mov-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-tip-mov-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-tip-mov-100.
       acc-tip-mov-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : tipo movimento da stampare              *
      *    *-----------------------------------------------------------*
       vis-tip-mov-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-tip-mov           to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tip-mov-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : descrizione tipo movimento da stampare  *
      *    *-----------------------------------------------------------*
       vis-tip-mov-des-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      rr-tip-mov-des       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tip-mov-des-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Tipi documento                       *
      *    *-----------------------------------------------------------*
       acc-tip-doc-000.
       acc-tip-doc-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Tipi documento                    *
      *    *-----------------------------------------------------------*
       vis-tip-doc-000.
       vis-tip-doc-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo selezione : Data fattura minima        *
      *    *-----------------------------------------------------------*
       acc-dat-min-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        rr-tip-ope           not  = 01
                     go to acc-dat-min-999.
       acc-dat-min-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      ">"                  to   v-edm                  .
           move      15                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-dat-min           to   v-dat                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-dat-min-999.
       acc-dat-min-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-dat                to   rr-dat-min             .
       acc-dat-min-300.
      *              *-------------------------------------------------*
      *              * Se Find                                         *
      *              *-------------------------------------------------*
           if        v-key                not = "FIND"
                     go to acc-dat-min-400.
           perform   fnd-arc-bit-000      thru fnd-arc-bit-999        .
           go to     acc-dat-min-100.
       acc-dat-min-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-dat-min-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-dat-min-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-dat-min-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-dat-min-100.
       acc-dat-min-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Data minima                             *
      *    *-----------------------------------------------------------*
       vis-dat-min-000.
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      15                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-dat-min           to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       vis-dat-min-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo selezione : Data fattura massima       *
      *    *-----------------------------------------------------------*
       acc-dat-max-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        rr-tip-ope           not  = 01
                     go to acc-dat-max-999.
       acc-dat-max-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      ">"                  to   v-edm                  .
           move      15                   to   v-lin                  .
           move      47                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "DO  "               to   v-pfk (05)             .
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
       acc-dat-max-300.
      *              *-------------------------------------------------*
      *              * Se Find                                         *
      *              *-------------------------------------------------*
           if        v-key                not = "FIND"
                     go to acc-dat-max-400.
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
      *    * Visualizzazione : Data massima                            *
      *    *-----------------------------------------------------------*
       vis-dat-max-000.
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      15                   to   v-lin                  .
           move      47                   to   v-pos                  .
           move      rr-dat-max           to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       vis-dat-max-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo selezione : Numero documento min       *
      *    *-----------------------------------------------------------*
       acc-num-min-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        rr-tip-ope           not  = 01
                     go to acc-num-min-999.
           if        rr-tip-mov           =    spaces
                     go to acc-num-min-999.
       acc-num-min-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "P"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      "<B"                 to   v-edm                  .
           move      16                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-num-min           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-num-min-999.
       acc-num-min-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-num-min             .
       acc-num-min-300.
      *              *-------------------------------------------------*
      *              * Se Find                                         *
      *              *-------------------------------------------------*
           if        v-key                not  = "FIND"
                     go to acc-num-min-400.
      *                  *---------------------------------------------*
      *                  * Find su archivio [bit]                      *
      *                  *---------------------------------------------*
           perform   fnd-arc-bit-000      thru fnd-arc-bit-999        .
      *                  *---------------------------------------------*
      *                  * A reimpostazione                            *
      *                  *---------------------------------------------*
           move      spaces               to   v-key                  .
           go to     acc-num-min-100.
       acc-num-min-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-num-min-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-num-min-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-num-min-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-num-min-100.
       acc-num-min-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Numero documento min                    *
      *    *-----------------------------------------------------------*
       vis-num-min-000.
           move      "DS"                 to   v-ope                  .
           move      "P"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      "<B"                 to   v-edm                  .
           move      16                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-num-min           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       vis-num-min-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo selezione : Numero documento max       *
      *    *-----------------------------------------------------------*
       acc-num-max-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        rr-tip-ope           not  = 01
                     go to acc-num-max-999.
           if        rr-tip-mov           =    spaces
                     go to acc-num-max-999.
       acc-num-max-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "P"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      "<B"                 to   v-edm                  .
           move      16                   to   v-lin                  .
           move      47                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-num-max           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-num-max-999.
       acc-num-max-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-num-max             .
       acc-num-max-300.
      *              *-------------------------------------------------*
      *              * Se Find                                         *
      *              *-------------------------------------------------*
           if        v-key                not  = "FIND"
                     go to acc-num-max-400.
      *                  *---------------------------------------------*
      *                  * Find su archivio [bit]                      *
      *                  *---------------------------------------------*
           perform   fnd-arc-bit-000      thru fnd-arc-bit-999        .
      *                  *---------------------------------------------*
      *                  * A reimpostazione                            *
      *                  *---------------------------------------------*
           move      spaces               to   v-key                  .
           go to     acc-num-max-100.
        acc-num-max-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-num-max-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-num-max-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-num-max-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-num-max-100.
       acc-num-max-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Numero documento max                    *
      *    *-----------------------------------------------------------*
       vis-num-max-000.
           move      "DS"                 to   v-ope                  .
           move      "P"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      "<B"                 to   v-edm                  .
           move      16                   to   v-lin                  .
           move      47                   to   v-pos                  .
           move      rr-num-max           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       vis-num-max-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo selezione : Codice cliente             *
      *    *-----------------------------------------------------------*
       acc-cod-cli-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        rr-tip-ope           not  = 01
                     go to acc-cod-cli-999.
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      rr-cod-cli           to   w-sav-cod-cli          .
       acc-cod-cli-100.
      *              *-------------------------------------------------*
      *              * Note operative                                  *
      *              *-------------------------------------------------*
           move      "NT"                 to   v-ope                  .
           move      "Tasto funzione SELEZIONE per scegliere piu' client
      -              "i (fino ad un massimo di 36)  "
                                          to   v-nt1                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-dcc-ope      .
           move      rr-cod-cli           to   w-cod-mne-dcc-cod      .
           move      17                   to   w-cod-mne-dcc-lin      .
           move      30                   to   w-cod-mne-dcc-pos      .
           move      17                   to   w-cod-mne-dcc-rln      .
           move      41                   to   w-cod-mne-dcc-rps      .
           move      zero                 to   w-cod-mne-dcc-vln      .
           move      zero                 to   w-cod-mne-dcc-vps      .
           move      zero                 to   w-cod-mne-dcc-lln      .
           move      zero                 to   w-cod-mne-dcc-lps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "SLCT"               to   v-pfk (11)             .
           perform   cod-mne-dcc-cll-000  thru cod-mne-dcc-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-mne-dcc-foi-000  thru cod-mne-dcc-foi-999    .
       acc-cod-cli-110.
           perform   cod-mne-dcc-cll-000  thru cod-mne-dcc-cll-999    .
           if        w-cod-mne-dcc-ope    =    "F+"
                     go to acc-cod-cli-115.
           if        w-cod-mne-dcc-ope    =    "AC"
                     go to acc-cod-cli-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-cli-115.
           perform   cod-mne-dcc-foi-000  thru cod-mne-dcc-foi-999    .
           go to     acc-cod-cli-110.
       acc-cod-cli-120.
           move      w-cod-mne-dcc-cod    to   v-num                  .
       acc-cod-cli-150.
      *              *-------------------------------------------------*
      *              * Cancellazione note operative                    *
      *              *-------------------------------------------------*
           move      "NT"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-cod-cli-999.
       acc-cod-cli-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-cod-cli             .
       acc-cod-cli-300.
      *              *-------------------------------------------------*
      *              * Se 'Select'                                     *
      *              *-------------------------------------------------*
           if        v-key                not  = "SLCT"
                     go to acc-cod-cli-400.
      *                  *---------------------------------------------*
      *                  * Select su codici cliente                    *
      *                  *---------------------------------------------*
           perform   acc-cli-esl-000      thru acc-cli-esl-999        .
      *                  *---------------------------------------------*
      *                  * Normalizzazione function key                *
      *                  *---------------------------------------------*
           move      spaces               to   v-key                  .
      *                  *---------------------------------------------*
      *                  * Verifica se elementi selezionati per atti-  *
      *                  * vazione del segnale di elementi selezionati *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazione segnale di selezione    *
      *                      * effettuata                              *
      *                      *-----------------------------------------*
           move      zero                 to   rr-cod-cli-sns         .
           move      zero                 to   w-acc-ser-edd-c01      .
       acc-cod-cli-310.
           add       1                    to   w-acc-ser-edd-c01      .
           if        w-acc-ser-edd-c01    >    w-acc-ser-edd-max
                     go to acc-cod-cli-320.
           if        rr-cod-cli-eco
                    (w-acc-ser-edd-c01)   =    zero
                     go to acc-cod-cli-320.
      *                      *-----------------------------------------*
      *                      * Attivazione segnale di selezione effet- *
      *                      * tuata                                   *
      *                      *-----------------------------------------*
           move      1                    to   rr-cod-cli-sns         .
       acc-cod-cli-320.
      *                  *---------------------------------------------*
      *                  * Normalizzazione singolo codice cliente      *
      *                  *---------------------------------------------*
           move      zero                 to   rr-cod-cli             .
           move      spaces               to   rr-cod-cli-rag         .
           move      spaces               to   rr-cod-cli-via         .
           move      spaces               to   rr-cod-cli-loc         .
      *                  *---------------------------------------------*
      *                  * Preparazione visualizzazione elementi sele- *
      *                  * zionati                                     *
      *                  *---------------------------------------------*
           perform   pcs-cli-esl-000      thru pcs-cli-esl-999        .
      *                  *---------------------------------------------*
      *                  * Visualizzazione codici selezionati          *
      *                  *---------------------------------------------*
           perform   vcs-cli-esl-000      thru vcs-cli-esl-999        .
      *                  *---------------------------------------------*
      *                  * Proseguimento                               *
      *                  *---------------------------------------------*
           go to     acc-cod-cli-600.
       acc-cod-cli-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-cod-cli-410.
      *                  *---------------------------------------------*
      *                  * Lettura file [cli]                          *
      *                  *---------------------------------------------*
           move      rr-cod-cli           to   w-let-arc-cli-cod      .
           perform   let-arc-cli-000      thru let-arc-cli-999        .
      *                  *---------------------------------------------*
      *                  * Se cliente non esistente : reimpostazione   *
      *                  *---------------------------------------------*
           if        w-let-arc-cli-flg    not  = spaces
                     go to acc-cod-cli-100.
      *                  *---------------------------------------------*
      *                  * Lettura record [dcc]                        *
      *                  *---------------------------------------------*
           move      rr-cod-cli           to   w-let-arc-dcc-cli      .
           move      spaces               to   w-let-arc-dcc-dpz      .
           perform   let-arc-dcc-000      thru let-arc-dcc-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione anagrafica cliente           *
      *                  *---------------------------------------------*
           if        rr-cod-cli           =    zero
                     move  "Tutti               "
                                          to   rr-cod-cli-rag
           else      move  w-let-arc-dcc-rag
                                          to   rr-cod-cli-rag         .
           move      w-let-arc-dcc-via    to   rr-cod-cli-via         .
           move      w-let-arc-dcc-loc    to   rr-cod-cli-loc         .
      *                  *---------------------------------------------*
      *                  * Visualizzazione anagrafica cliente          *
      *                  *---------------------------------------------*
           perform   vis-des-cli-000      thru vis-des-cli-999        .
      *                  *---------------------------------------------*
      *                  * Se cliente esistente : oltre                *
      *                  *---------------------------------------------*
           if        w-let-arc-dcc-flg    =    spaces
                     go to acc-cod-cli-600.
      *                  *---------------------------------------------*
      *                  * Se cliente a zero : oltre                   *
      *                  *---------------------------------------------*
           if        rr-cod-cli           =    zero
                     go to acc-cod-cli-600.
       acc-cod-cli-500.
      *                  *---------------------------------------------*
      *                  * Se anagrafica commerciale non esistente     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Bufferizzazione anagrafica cliente con- *
      *                      * tabile                                  *
      *                      *-----------------------------------------*
           move      w-let-arc-cli-rag    to   rr-cod-cli-rag         .
           move      w-let-arc-cli-via    to   rr-cod-cli-via         .
           move      w-let-arc-cli-loc    to   rr-cod-cli-loc         .
      *                      *-----------------------------------------*
      *                      * Visualizzazione anagrafica cliente      *
      *                      *-----------------------------------------*
           perform   vis-des-cli-000      thru vis-des-cli-999        .
      *                      *-----------------------------------------*
      *                      * Messaggio di errore                     *
      *                      *-----------------------------------------*
           move      "Anagrafica commerciale del cliente non esistente !
      -              ""
                                          to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                      *-----------------------------------------*
      *                      * Reimpostazione                          *
      *                      *-----------------------------------------*
           go to     acc-cod-cli-100.
       acc-cod-cli-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore attuale come precedente : oltre   *
      *                  *---------------------------------------------*
           if        rr-cod-cli           =    w-sav-cod-cli
                     go to acc-cod-cli-800.
      *                  *---------------------------------------------*
      *                  * Se e' solo stato cambiato il codice cli-    *
      *                  * ente : oltre                                *
      *                  *---------------------------------------------*
           if        rr-cod-cli           not  = zero and
                     w-sav-cod-cli        not  = zero
                     go to acc-cod-cli-800.
       acc-cod-cli-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-cod-cli-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-cod-cli-100.
       acc-cod-cli-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo selezione : Anagrafica cliente      *
      *    *-----------------------------------------------------------*
       vis-des-cli-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      rr-cod-cli-rag       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-des-cli-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Serie elementi da selezionare              *
      *    *-----------------------------------------------------------*
       acc-cli-esl-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Salvataggio immagine video                  *
      *                  *---------------------------------------------*
           move      "SV"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Video in Off                                *
      *                  *---------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Box                                         *
      *                  *---------------------------------------------*
           move      "BX"                 to   v-ope                  .
           move      04                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      21                   to   v-lto                  .
           move      80                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Titoli all'interno del box                  *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      76                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      "                       Codici cliente da seleziona
      -              "re                        "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Sottolineatura titoli                       *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      76                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      all "-"              to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Sottolineatura inferiore titoli             *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      76                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      all "-"              to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione pagina a partire dall'ele-  *
      *                  * mento numero 1                              *
      *                  *---------------------------------------------*
           move      1                    to   w-acc-ser-edd-n1v      .
           perform   acc-cli-esl-900      thru acc-cli-esl-909        .
      *                  *---------------------------------------------*
      *                  * Video in On                                 *
      *                  *---------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Primo elemento visualizzato : 1             *
      *                  *---------------------------------------------*
           move      1                    to   w-acc-ser-edd-pev      .
      *                  *---------------------------------------------*
      *                  * Numero elemento in accettazione : 1         *
      *                  *---------------------------------------------*
           move      1                    to   w-acc-ser-edd-nel      .
      *                  *---------------------------------------------*
      *                  * Ad accettazione                             *
      *                  *---------------------------------------------*
           go to     acc-cli-esl-100.
       acc-cli-esl-080.
      *              *-------------------------------------------------*
      *              * Visualizzazione pagina se necessario            *
      *              *-------------------------------------------------*
       acc-cli-esl-082.
           move      w-acc-ser-edd-pev    to   w-acc-ser-edd-c0a      .
           move      w-acc-ser-edd-c0a    to   w-acc-ser-edd-c0b      .
           add       11                   to   w-acc-ser-edd-c0b      .
           if        w-acc-ser-edd-nel    <    w-acc-ser-edd-c0a or
                     w-acc-ser-edd-nel    >    w-acc-ser-edd-c0b
                     go to acc-cli-esl-084
           else      go to acc-cli-esl-086.
       acc-cli-esl-084.
           move      w-acc-ser-edd-nel    to   w-acc-ser-edd-n1v      .
           subtract  1                    from w-acc-ser-edd-n1v      .
           divide    12                   into w-acc-ser-edd-n1v      .
           multiply  12                   by   w-acc-ser-edd-n1v      .
           add       1                    to   w-acc-ser-edd-n1v      .
           move      w-acc-ser-edd-n1v    to   w-acc-ser-edd-pev      .
           perform   acc-cli-esl-900      thru acc-cli-esl-909        .
       acc-cli-esl-086.
           go to     acc-cli-esl-100.
       acc-cli-esl-100.
      *              *-------------------------------------------------*
      *              * Accettazione linea                              *
      *              *-------------------------------------------------*
       acc-cli-esl-120.
      *              *-------------------------------------------------*
      *              * Visualizzazione numero di pagina                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Calcolo del numero di pagina in corso di    *
      *                  * trattamento                                 *
      *                  *---------------------------------------------*
           if        w-acc-ser-edd-nel    >    24
                     move  3              to   w-acc-ser-edd-lt1
           else if   w-acc-ser-edd-nel    >    12
                     move  2              to   w-acc-ser-edd-lt1
           else      move  1              to   w-acc-ser-edd-lt1      .
      *                  *---------------------------------------------*
      *                  * Calcolo del numero di pagina totale         *
      *                  *---------------------------------------------*
           if        rr-cod-cli-els       >    24
                     move  3              to   w-acc-ser-edd-lt2
           else if   rr-cod-cli-els       >    12
                     move  2              to   w-acc-ser-edd-lt2
           else      move  1              to   w-acc-ser-edd-lt2      .
      *
           move      3                    to   w-acc-ser-edd-lt2      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione                             *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      13                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      34                   to   v-pos                  .
           move      w-acc-ser-edd-ltp    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       acc-cli-esl-150.
      *                  *---------------------------------------------*
      *                  * Salvataggio valori precedenti linea         *
      *                  *---------------------------------------------*
           move      rr-cod-cli-eco
                    (w-acc-ser-edd-nel)   to   w-acc-ser-edd-spe      .
       acc-cli-esl-200.
      *                  *---------------------------------------------*
      *                  * Accettazione codice elemento                *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione parametri                  *
      *                      *-----------------------------------------*
           move      "AC"                 to   w-cod-mne-dcc-ope      .
           move      rr-cod-cli-eco
                    (w-acc-ser-edd-nel)   to   w-cod-mne-dcc-cod      .
           move      "<B"                 to   v-edm                  .
      *                          *-------------------------------------*
      *                          * Coordinate di posizionamento        *
      *                          *-------------------------------------*
           move      w-acc-ser-edd-nel    to   w-acc-ser-edd-c0r      .
           subtract  w-acc-ser-edd-pev    from w-acc-ser-edd-c0r      .
           add       7                    to   w-acc-ser-edd-c0r      .
           move      w-acc-ser-edd-c0r    to   w-cod-mne-dcc-lin      .
           move      09                   to   w-cod-mne-dcc-pos      .
           move      w-cod-mne-dcc-lin    to   w-cod-mne-dcc-rln      .
           move      19                   to   w-cod-mne-dcc-rps      .
      *                          *-------------------------------------*
      *                          * Tasto 'Up'                          *
      *                          *-------------------------------------*
           move      "UP  "               to   v-pfk (01)             .
      *                          *-------------------------------------*
      *                          * Tasto 'Down'                        *
      *                          *-------------------------------------*
           move      "DOWN"               to   v-pfk (02)             .
      *                          *-------------------------------------*
      *                          * Tasto 'Find'                        *
      *                          *-------------------------------------*
           move      "FIND"               to   v-pfk (03)             .
      *                          *-------------------------------------*
      *                          * Tasto 'Insr' : disattivato          *
      *                          *-------------------------------------*
           move      spaces               to   v-pfk (04)             .
      *                          *-------------------------------------*
      *                          * Tasto 'Do'                          *
      *                          *-------------------------------------*
           move      "DO  "               to   v-pfk (05)             .
      *                          *-------------------------------------*
      *                          * Tasto 'Remove'                      *
      *                          *-------------------------------------*
           if        rr-cod-cli-eco
                    (w-acc-ser-edd-nel)   not  = zero
                     go to acc-cli-esl-202.
           move      w-acc-ser-edd-nel    to   w-acc-ser-edd-c0a      .
           add       1                    to   w-acc-ser-edd-c0a      .
           if        w-acc-ser-edd-c0a    >    w-acc-ser-edd-max
                     go to acc-cli-esl-204.
           if        rr-cod-cli-eco
                    (w-acc-ser-edd-c0a)   =    zero
                     go to acc-cli-esl-204.
       acc-cli-esl-202.
           move      "REMV"               to   v-pfk (06)             .
       acc-cli-esl-204.
      *                          *-------------------------------------*
      *                          * Tasto 'Previous screen'             *
      *                          *-------------------------------------*
           move      "PRSC"               to   v-pfk (07)             .
      *                          *-------------------------------------*
      *                          * Tasto 'Next screen'                 *
      *                          *-------------------------------------*
           move      "NXSC"               to   v-pfk (08)             .
      *                          *-------------------------------------*
      *                          * Tasto 'Back'                        *
      *                          *-------------------------------------*
           if        w-acc-ser-edd-nel    >    1
                     move  "BACK"         to   v-pfk (09)             .
           if        w-acc-ser-edd-nel    =    w-acc-ser-edd-max
                     go to acc-cli-esl-206.
           move      w-acc-ser-edd-nel    to   w-acc-ser-edd-c0a      .
           add       1                    to   w-acc-ser-edd-c0a      .
           if        rr-cod-cli-eco
                    (w-acc-ser-edd-nel)   =    zero and
                     rr-cod-cli-eco
                    (w-acc-ser-edd-c0a)   =    zero
                     go to acc-cli-esl-206.
      *                          *-------------------------------------*
      *                          * Tasto 'Tab'                         *
      *                          *-------------------------------------*
           move      "TAB "               to   v-pfk (10)             .
       acc-cli-esl-206.
      *                      *-----------------------------------------*
      *                      * Accettazione                            *
      *                      *-----------------------------------------*
           perform   cod-mne-dcc-cll-000  thru cod-mne-dcc-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-mne-dcc-foi-000  thru cod-mne-dcc-foi-999    .
       acc-cli-esl-208.
           perform   cod-mne-dcc-cll-000  thru cod-mne-dcc-cll-999    .
           if        w-cod-mne-dcc-ope    =    "F+"
                     go to acc-cli-esl-210.
           if        w-cod-mne-dcc-ope    =    "AC"
                     go to acc-cli-esl-212.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cli-esl-210.
           perform   cod-mne-dcc-foi-000  thru cod-mne-dcc-foi-999    .
           go to     acc-cli-esl-208.
       acc-cli-esl-212.
           move      w-cod-mne-dcc-cod    to   v-num                  .
       acc-cli-esl-215.
      *                      *-----------------------------------------*
      *                      * Valore in campo di destinazione         *
      *                      *-----------------------------------------*
           move      v-num                to   rr-cod-cli-eco
                                              (w-acc-ser-edd-nel)     .
      *                      *-----------------------------------------*
      *                      * Se Exit                                 *
      *                      *-----------------------------------------*
           if        v-key                =    "EXIT"
                     move  spaces         to   v-key
                     go to acc-cli-esl-800.
      *                      *-----------------------------------------*
      *                      * Se Return                               *
      *                      *-----------------------------------------*
           if        v-key                =    spaces
                     go to acc-cli-esl-425.
       acc-cli-esl-220.
      *                      *-----------------------------------------*
      *                      * Se Up                                   *
      *                      *-----------------------------------------*
           if        v-key                not  = "UP  "
                     go to acc-cli-esl-225.
      *                          *-------------------------------------*
      *                          * Compattamento se necessario         *
      *                          *-------------------------------------*
           move      w-acc-ser-edd-nel    to   w-acc-ser-edd-nec      .
           perform   acc-cli-esl-920      thru acc-cli-esl-929        .
      *                          *-------------------------------------*
      *                          * Se sul primo : uscita               *
      *                          *-------------------------------------*
           subtract  1                    from w-acc-ser-edd-nel      .
           if        w-acc-ser-edd-nel    =    zero
                     move  "UP  "         to   v-key
                     go to acc-cli-esl-800
           else      go to acc-cli-esl-080.
       acc-cli-esl-225.
      *                      *-----------------------------------------*
      *                      * Se Down                                 *
      *                      *-----------------------------------------*
           if        v-key                not  = "DOWN"
                     go to acc-cli-esl-250.
      *                          *-------------------------------------*
      *                          * Compattamento se necessario         *
      *                          *-------------------------------------*
           move      w-acc-ser-edd-nel    to   w-acc-ser-edd-nec      .
           perform   acc-cli-esl-920      thru acc-cli-esl-929        .
      *                          *-------------------------------------*
      *                          * Se sull'ultimo : uscita             *
      *                          *-------------------------------------*
           if        w-acc-ser-edd-fce    =    spaces
                     add   1              to   w-acc-ser-edd-nel      .
           if        w-acc-ser-edd-nel    >    w-acc-ser-edd-max
                     move  "DOWN"         to   v-key
                     go to acc-cli-esl-800.
           if        w-acc-ser-edd-nel    =    1
                     go to acc-cli-esl-230
           else      go to acc-cli-esl-235.
       acc-cli-esl-230.
           if        rr-cod-cli-eco (1)    =    zero and
                     rr-cod-cli-eco (2)    =    zero
                     move  "DOWN"         to   v-key
                     go to acc-cli-esl-800
           else      go to acc-cli-esl-080.
       acc-cli-esl-235.
           move      w-acc-ser-edd-nel    to   w-acc-ser-edd-c0a      .
           move      w-acc-ser-edd-c0a    to   w-acc-ser-edd-c0b      .
           subtract  1                    from w-acc-ser-edd-c0b      .
           if        rr-cod-cli-eco
                    (w-acc-ser-edd-c0a)   =    zero and
                     rr-cod-cli-eco
                    (w-acc-ser-edd-c0b)   =    zero
                     move  "DOWN"         to   v-key
                     go to acc-cli-esl-800
           else      go to acc-cli-esl-080.
       acc-cli-esl-250.
      *                      *-----------------------------------------*
      *                      * Se Insr                                 *
      *                      *-----------------------------------------*
           if        v-key                not  = "INSR"
                     go to acc-cli-esl-275.
           move      w-acc-ser-edd-nel    to   w-acc-ser-edd-nec      .
           perform   acc-cli-esl-940      thru acc-cli-esl-949        .
           go to     acc-cli-esl-080.
       acc-cli-esl-275.
      *                      *-----------------------------------------*
      *                      * Se Do                                   *
      *                      *-----------------------------------------*
           if        v-key                not  = "DO  "
                     go to acc-cli-esl-300.
      *                          *-------------------------------------*
      *                          * Controlli per tasto Do              *
      *                          *-------------------------------------*
           perform   tdo-ric-sel-000      thru tdo-ric-sel-999        .
           if        w-cnt-tdo-ric-flg    =    spaces
                     go to acc-cli-esl-280
           else      move  spaces         to   w-cnt-tdo-ric-flg
                     go to acc-cli-esl-200.
       acc-cli-esl-280.
      *                          *-------------------------------------*
      *                          * Compattamento se necessario         *
      *                          *-------------------------------------*
           move      w-acc-ser-edd-nel    to   w-acc-ser-edd-nec      .
           perform   acc-cli-esl-920      thru acc-cli-esl-929        .
      *                          *-------------------------------------*
      *                          * Uscita                              *
      *                          *-------------------------------------*
           move      "S"                  to   w-cnt-acc-ric-sel      .
           go to     acc-cli-esl-800.
       acc-cli-esl-300.
      *                      *-----------------------------------------*
      *                      * Se Remv                                 *
      *                      *-----------------------------------------*
           if        v-key                not  = "REMV"
                     go to acc-cli-esl-325.
      *                          *-------------------------------------*
      *                          * Compattamento in ogni caso          *
      *                          *-------------------------------------*
           move      w-acc-ser-edd-nel    to   w-acc-ser-edd-nec      .
           perform   acc-cli-esl-930      thru acc-cli-esl-939        .
      *                          *-------------------------------------*
      *                          * A reimpostare                       *
      *                          *-------------------------------------*
           go to     acc-cli-esl-080.
       acc-cli-esl-325.
      *                      *-----------------------------------------*
      *                      * Se Prsc                                 *
      *                      *-----------------------------------------*
           if        v-key                not  = "PRSC"
                     go to acc-cli-esl-350.
      *                          *-------------------------------------*
      *                          * Compattamento se necessario         *
      *                          *-------------------------------------*
           move      w-acc-ser-edd-nel    to   w-acc-ser-edd-nec      .
           perform   acc-cli-esl-920      thru acc-cli-esl-929        .
      *                          *-------------------------------------*
      *                          * Se su prima facciata : uscita       *
      *                          *-------------------------------------*
           if        w-acc-ser-edd-nel    not  > 14
                     move  "UP  "         to   v-key
                     go to acc-cli-esl-800.
      *                          *-------------------------------------*
      *                          * Al primo elemento della facciata    *
      *                          * precedente                          *
      *                          *-------------------------------------*
           subtract  1                    from w-acc-ser-edd-nel      .
           divide    14                   into w-acc-ser-edd-nel      .
           subtract  1                    from w-acc-ser-edd-nel      .
           multiply  14                   by   w-acc-ser-edd-nel      .
           add       1                    to   w-acc-ser-edd-nel      .
           go to     acc-cli-esl-080.
       acc-cli-esl-350.
      *                      *-----------------------------------------*
      *                      * Se Nxsc                                 *
      *                      *-----------------------------------------*
           if        v-key                not  = "NXSC"
                     go to acc-cli-esl-375.
      *                          *-------------------------------------*
      *                          * Compattamento se necessario         *
      *                          *-------------------------------------*
           move      w-acc-ser-edd-nel    to   w-acc-ser-edd-nec      .
           perform   acc-cli-esl-920      thru acc-cli-esl-929        .
      *                          *-------------------------------------*
      *                          * Se su ultima facciata : uscita      *
      *                          *-------------------------------------*
           move      w-acc-ser-edd-nel    to   w-acc-ser-edd-c0a      .
           subtract  1                    from w-acc-ser-edd-c0a      .
           divide    12                   into w-acc-ser-edd-c0a      .
           add       1                    to   w-acc-ser-edd-c0a      .
           multiply  12                   by   w-acc-ser-edd-c0a      .
           add       1                    to   w-acc-ser-edd-c0a      .
           if        w-acc-ser-edd-c0a    >    w-acc-ser-edd-max
                     move  "DOWN"         to   v-key
                     go to acc-cli-esl-800.
           if        rr-cod-cli-eco
                    (w-acc-ser-edd-c0a)   =    zero
                     move  "DOWN"         to   v-key
                     go to acc-cli-esl-800.
      *                          *-------------------------------------*
      *                          * Al primo elemento della facciata    *
      *                          * successiva                          *
      *                          *-------------------------------------*
           move      w-acc-ser-edd-c0a    to   w-acc-ser-edd-nel      .
           go to     acc-cli-esl-080.
       acc-cli-esl-375.
      *                      *-----------------------------------------*
      *                      * Se Back                                 *
      *                      *-----------------------------------------*
           if        v-key                not  = "BACK"
                     go to acc-cli-esl-400.
      *                          *-------------------------------------*
      *                          * Compattamento se necessario         *
      *                          *-------------------------------------*
           move      w-acc-ser-edd-nel    to   w-acc-ser-edd-nec      .
           perform   acc-cli-esl-920      thru acc-cli-esl-929        .
      *                          *-------------------------------------*
      *                          * Al primo elemento                   *
      *                          *-------------------------------------*
           move      1                    to   w-acc-ser-edd-nel      .
           go to     acc-cli-esl-080.
       acc-cli-esl-400.
      *                      *-----------------------------------------*
      *                      * Se Tab                                  *
      *                      *-----------------------------------------*
           if        v-key                not  = "TAB "
                     go to acc-cli-esl-425.
      *                          *-------------------------------------*
      *                          * Compattamento se necessario         *
      *                          *-------------------------------------*
           move      w-acc-ser-edd-nel    to   w-acc-ser-edd-nec      .
           perform   acc-cli-esl-920      thru acc-cli-esl-929        .
      *                          *-------------------------------------*
      *                          * Dopo l'ultimo elemento inserito     *
      *                          *-------------------------------------*
           if        rr-cod-cli-eco
                    (w-acc-ser-edd-max)   not  = zero
                     move  w-acc-ser-edd-max
                                          to   w-acc-ser-edd-nel
                     go to acc-cli-esl-080.
           move      w-acc-ser-edd-max    to   w-acc-ser-edd-nel      .
       acc-cli-esl-405.
           if        w-acc-ser-edd-nel    =    zero
                     go to acc-cli-esl-410.
           if        rr-cod-cli-eco
                    (w-acc-ser-edd-nel)   =    zero
                     subtract  1          from w-acc-ser-edd-nel
                     go to     acc-cli-esl-405.
       acc-cli-esl-410.
           add       1                    to   w-acc-ser-edd-nel      .
           go to     acc-cli-esl-080.
       acc-cli-esl-425.
      *                      *-----------------------------------------*
      *                      * Se Return                               *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Lettura archivio [dcc]              *
      *                          *-------------------------------------*
           move      rr-cod-cli-eco
                    (w-acc-ser-edd-nel)   to   w-let-arc-dcc-cli      .
           move      spaces               to   w-let-arc-dcc-dpz      .
           perform   let-arc-dcc-000      thru let-arc-dcc-999        .
      *                          *-------------------------------------*
      *                          * Trattamento descrizione             *
      *                          *-------------------------------------*
           move      w-let-arc-dcc-rag    to   rr-cod-cli-ers
                                              (w-acc-ser-edd-nel)     .
      *                          *-------------------------------------*
      *                          * Visualizzazione                     *
      *                          *-------------------------------------*
           move      w-acc-ser-edd-nel    to   w-acc-ser-edd-nev      .
           move      w-acc-ser-edd-nel    to   w-acc-ser-edd-c0r      .
           subtract  w-acc-ser-edd-pev    from w-acc-ser-edd-c0r      .
           add       7                    to   w-acc-ser-edd-c0r      .
           perform   acc-cli-esl-910      thru acc-cli-esl-919        .
      *                          *-------------------------------------*
      *                          * Se record non trovato : reimposta-  *
      *                          * zione                               *
      *                          *-------------------------------------*
           if        w-let-arc-dcc-flg    not  = spaces
                     go to acc-cli-esl-200.
      *                          *-------------------------------------*
      *                          * Controllo valore impostato          *
      *                          *-------------------------------------*
           if        rr-cod-cli-eco
                    (w-acc-ser-edd-nel)   =    zero
                     go to acc-cli-esl-450
           else      go to acc-cli-esl-500.
       acc-cli-esl-450.
      *                          *-------------------------------------*
      *                          * Se impostato zero                   *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Se il valore precedente era a   *
      *                              * zero : come per Down            *
      *                              *---------------------------------*
           if        w-acc-ser-edd-spe    =    zero
                     move  "DOWN"         to   v-key
                     go to acc-cli-esl-225.
      *                              *---------------------------------*
      *                              * Altrimenti come per Remv        *
      *                              *---------------------------------*
           move      "REMV"               to   v-key                  .
           go to     acc-cli-esl-300.
       acc-cli-esl-500.
      *                  *---------------------------------------------*
      *                  * Passaggio ad elemento successivo            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Visualizzazione elementi selezionati    *
      *                      *-----------------------------------------*
           perform   acc-cli-esl-960      thru acc-cli-esl-969        .
      *                      *-----------------------------------------*
      *                      * Incremento numero elemento              *
      *                      *-----------------------------------------*
           add       1                    to   w-acc-ser-edd-nel      .
      *                      *-----------------------------------------*
      *                      * Se fine elementi : uscita               *
      *                      *-----------------------------------------*
           if        w-acc-ser-edd-nel    >    w-acc-ser-edd-max
                     move  spaces         to   v-key
                     go to acc-cli-esl-800.
      *                      *-----------------------------------------*
      *                      * Altrimenti : ad impostazione linea      *
      *                      *-----------------------------------------*
           go to     acc-cli-esl-080.
       acc-cli-esl-800.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Salvataggio v-key e w-cnt-acc-ric-sel       *
      *                  *---------------------------------------------*
           move      v-key                to   w-acc-ser-edd-svk      .
           move      w-cnt-acc-ric-sel    to   w-acc-ser-edd-stu      .
      *                  *---------------------------------------------*
      *                  * Video in Off                                *
      *                  *---------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Ripristino  immagine video                  *
      *                  *---------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Video in On                                 *
      *                  *---------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Ripristino v-key e w-cnt-acc-ric-sel        *
      *                  *---------------------------------------------*
           move      w-acc-ser-edd-svk    to   v-key                  .
           move      w-acc-ser-edd-stu    to   w-cnt-acc-ric-sel      .
      *                  *---------------------------------------------*
      *                  * Fine routine                                *
      *                  *---------------------------------------------*
           go to     acc-cli-esl-999.
       acc-cli-esl-890.
      *              *-------------------------------------------------*
      *              * Subroutines interne                             *
      *              *-------------------------------------------------*
       acc-cli-esl-900.
      *                  *---------------------------------------------*
      *                  * Visualizzazione pagina a partire dall'ele-  *
      *                  * mento numero w-acc-ser-edd-n1v              *
      *                  *---------------------------------------------*
           move      w-acc-ser-edd-n1v    to   w-acc-ser-edd-c0p      .
           subtract  1                    from w-acc-ser-edd-c0p      .
           move      w-acc-ser-edd-c0p    to   w-acc-ser-edd-c0q      .
           add       12                   to   w-acc-ser-edd-c0q      .
       acc-cli-esl-901.
           add       1                    to   w-acc-ser-edd-c0p      .
           if        w-acc-ser-edd-c0p    >    w-acc-ser-edd-max
                     go to acc-cli-esl-905.
           if        w-acc-ser-edd-c0p    >    w-acc-ser-edd-c0q
                     go to acc-cli-esl-909.
           move      w-acc-ser-edd-c0p    to   w-acc-ser-edd-nev      .
           move      w-acc-ser-edd-nev    to   w-acc-ser-edd-c0r      .
       acc-cli-esl-903.
           if        w-acc-ser-edd-c0r    >    12
                     subtract  12         from w-acc-ser-edd-c0r
                     go to     acc-cli-esl-903.
           add       6                    to   w-acc-ser-edd-c0r      .
           perform   acc-cli-esl-910      thru acc-cli-esl-919        .
           go to     acc-cli-esl-901.
       acc-cli-esl-905.
           if        w-acc-ser-edd-c0p    >    w-acc-ser-edd-c0q
                     go to acc-cli-esl-909.
           add       1                    to   w-acc-ser-edd-c0r      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      70                   to   v-car                  .
           move      w-acc-ser-edd-c0r    to   v-lin                  .
           move      09                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           add       1                    to   w-acc-ser-edd-c0p      .
           go to     acc-cli-esl-905.
       acc-cli-esl-909.
           exit.
       acc-cli-esl-910.
      *                  *---------------------------------------------*
      *                  * Visualizzazione elemento indirizzato da     *
      *                  * w-acc-ser-edd-nev a linea w-acc-ser-edd-c0r *
      *                  *---------------------------------------------*
           move      spaces               to   w-acc-ser-edd-led      .
      *                      *-----------------------------------------*
      *                      * Composizione linea da visualizzare      *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Editing numero riga                 *
      *                          *-------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "B"                  to   v-edm                  .
           move      w-acc-ser-edd-nev    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-acc-ser-edd-rig      .
      *                          *-------------------------------------*
      *                          * Editing carattere  ':'              *
      *                          *-------------------------------------*
           move      ":"                  to   w-acc-ser-edd-dpu      .
      *                          *-------------------------------------*
      *                          * Editing codice cliente              *
      *                          *-------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      rr-cod-cli-eco
                    (w-acc-ser-edd-nev)   to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-acc-ser-edd-cod      .
      *                          *-------------------------------------*
      *                          * Ragione sociale                     *
      *                          *-------------------------------------*
           move      rr-cod-cli-ers
                    (w-acc-ser-edd-nev)   to   w-acc-ser-edd-des      .
      *                      *-----------------------------------------*
      *                      * Visualizzazione                         *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      76                   to   v-car                  .
           move      w-acc-ser-edd-c0r    to   v-lin                  .
           move      03                   to   v-pos                  .
           move      w-acc-ser-edd-led    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-cli-esl-919.
           exit.
       acc-cli-esl-920.
      *                  *---------------------------------------------*
      *                  * Compattamento dell' elemento indirizzato da *
      *                  * w-acc-ser-edd-nec se necessario             *
      *                  *---------------------------------------------*
           move      spaces               to   w-acc-ser-edd-fce      .
           if        rr-cod-cli-eco
                    (w-acc-ser-edd-nec)   not  = zero
                     go to acc-cli-esl-929.
           move      w-acc-ser-edd-nec    to   w-acc-ser-edd-c0a      .
           move      w-acc-ser-edd-c0a    to   w-acc-ser-edd-c0b      .
           add       1                    to   w-acc-ser-edd-c0b      .
           if        w-acc-ser-edd-c0b    >    w-acc-ser-edd-max
                     go to acc-cli-esl-929.
           if        rr-cod-cli-eco
                    (w-acc-ser-edd-c0b)   =    zero
                     go to acc-cli-esl-929.
           perform   acc-cli-esl-930      thru acc-cli-esl-939        .
           move      "#"                  to   w-acc-ser-edd-fce      .
       acc-cli-esl-929.
           exit.
       acc-cli-esl-930.
      *                  *---------------------------------------------*
      *                  * Compattamento dell' elemento indirizzato da *
      *                  * w-acc-ser-edd-nec in ogni caso              *
      *                  *---------------------------------------------*
           move      w-acc-ser-edd-nec    to   w-acc-ser-edd-c0a      .
           move      w-acc-ser-edd-c0a    to   w-acc-ser-edd-c0b      .
           add       1                    to   w-acc-ser-edd-c0b      .
       acc-cli-esl-931.
           if        w-acc-ser-edd-c0b    >    w-acc-ser-edd-max
                     go to acc-cli-esl-932.
           move      rr-cod-cli-ele
                    (w-acc-ser-edd-c0b)   to   rr-cod-cli-ele
                                              (w-acc-ser-edd-c0a)     .
           if        rr-cod-cli-eco
                    (w-acc-ser-edd-c0b)   =    zero
                     go to acc-cli-esl-933.
           add       1                    to   w-acc-ser-edd-c0a      .
           add       1                    to   w-acc-ser-edd-c0b      .
           go to     acc-cli-esl-931.
       acc-cli-esl-932.
           move      zero                 to   rr-cod-cli-eco
                                              (w-acc-ser-edd-c0a)     .
           move      spaces               to   rr-cod-cli-ers
                                              (w-acc-ser-edd-c0a)     .
       acc-cli-esl-933.
           move      w-acc-ser-edd-nec    to   w-acc-ser-edd-c0a      .
       acc-cli-esl-934.
           if        w-acc-ser-edd-c0a    >    12
                     subtract  12         from w-acc-ser-edd-c0a
                     go to     acc-cli-esl-934.
           move      w-acc-ser-edd-c0a    to   w-acc-ser-edd-c0b      .
           add       6                    to   w-acc-ser-edd-c0b      .
           move      w-acc-ser-edd-c0b    to   w-acc-ser-edd-c0c      .
           add       1                    to   w-acc-ser-edd-c0c      .
       acc-cli-esl-935.
           if        w-acc-ser-edd-c0a    =    12
                     go to acc-cli-esl-936.
           move      "FL"                 to   v-ope                  .
           move      w-acc-ser-edd-c0c    to   v-lin                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-alf                to   w-acc-ser-edd-fcl      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      70                   to   v-car                  .
           move      w-acc-ser-edd-c0b    to   v-lin                  .
           move      09                   to   v-pos                  .
           move      w-acc-ser-edd-070    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           add       1                    to   w-acc-ser-edd-c0a      .
           add       1                    to   w-acc-ser-edd-c0b      .
           add       1                    to   w-acc-ser-edd-c0c      .
           go to     acc-cli-esl-935.
       acc-cli-esl-936.
           move      w-acc-ser-edd-nec    to   w-acc-ser-edd-c0a      .
       acc-cli-esl-937.
           if        w-acc-ser-edd-c0a    >    12
                     subtract  12         from w-acc-ser-edd-c0a
                     go to     acc-cli-esl-937.
           subtract  w-acc-ser-edd-c0a    from 12
                                        giving w-acc-ser-edd-c0b      .
           add       w-acc-ser-edd-nec
                     w-acc-ser-edd-c0b  giving w-acc-ser-edd-c0c      .
           if        w-acc-ser-edd-c0c    >    w-acc-ser-edd-max
                     go to acc-cli-esl-938.
           if        rr-cod-cli-eco
                    (w-acc-ser-edd-c0c)   =    zero
                     go to acc-cli-esl-938.
           move      w-acc-ser-edd-c0c    to   w-acc-ser-edd-nev      .
           move      18                   to   w-acc-ser-edd-c0r      .
           perform   acc-cli-esl-910      thru acc-cli-esl-919        .
           go to     acc-cli-esl-939.
       acc-cli-esl-938.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      70                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      09                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-cli-esl-939.
           exit.
       acc-cli-esl-940.
      *                  *---------------------------------------------*
      *                  * Inserimento dell' elemento indirizzato da   *
      *                  * w-acc-ser-edd-nec                           *
      *                  *---------------------------------------------*
           move      w-acc-ser-edd-max    to   w-acc-ser-edd-c0b      .
           move      w-acc-ser-edd-c0b    to   w-acc-ser-edd-c0a      .
           subtract  1                    from w-acc-ser-edd-c0a      .
       acc-cli-esl-941.
           move      rr-cod-cli-ele
                    (w-acc-ser-edd-c0a)   to   rr-cod-cli-ele
                                              (w-acc-ser-edd-c0b)     .
           if        w-acc-ser-edd-c0a    =    w-acc-ser-edd-nec
                     go to acc-cli-esl-942.
           subtract  1                    from w-acc-ser-edd-c0a      .
           subtract  1                    from w-acc-ser-edd-c0b      .
           go to     acc-cli-esl-941.
       acc-cli-esl-942.
           move      zero                 to   rr-cod-cli-eco
                                              (w-acc-ser-edd-c0a)     .
           move      spaces               to   rr-cod-cli-ers
                                              (w-acc-ser-edd-c0a)     .
           move      w-acc-ser-edd-nec    to   w-acc-ser-edd-c0a      .
       acc-cli-esl-943.
           if        w-acc-ser-edd-c0a    >    12
                     subtract  12         from w-acc-ser-edd-c0a
                     go to     acc-cli-esl-943.
           add       6                    to   w-acc-ser-edd-c0a      .
           if        w-acc-ser-edd-c0a    =    18
                     go to acc-cli-esl-945.
           move      17                   to   w-acc-ser-edd-c0b      .
           move      18                   to   w-acc-ser-edd-c0c      .
       acc-cli-esl-944.
           move      "FL"                 to   v-ope                  .
           move      w-acc-ser-edd-c0b    to   v-lin                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-alf                to   w-acc-ser-edd-fcl      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      70                   to   v-car                  .
           move      w-acc-ser-edd-c0c    to   v-lin                  .
           move      09                   to   v-pos                  .
           move      w-acc-ser-edd-070    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           if        w-acc-ser-edd-c0b    =    w-acc-ser-edd-c0a
                     go to acc-cli-esl-945.
           subtract  1                    from w-acc-ser-edd-c0b      .
           subtract  1                    from w-acc-ser-edd-c0c      .
           go to     acc-cli-esl-944.
       acc-cli-esl-945.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      70                   to   v-car                  .
           move      w-acc-ser-edd-c0a    to   v-lin                  .
           move      09                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-cli-esl-949.
           exit.
       acc-cli-esl-960.
      *                  *---------------------------------------------*
      *                  * Visualizzazione elementi da trattare        *
      *                  *---------------------------------------------*
           move      zero                 to   w-acc-ser-edd-c0a      .
       acc-cli-esl-962.
           add       1                    to   w-acc-ser-edd-c0a      .
           if        w-acc-ser-edd-c0a    >    w-acc-ser-edd-max
                     go to acc-cli-esl-964.
           if        rr-cod-cli-eco
                    (w-acc-ser-edd-c0a)   =    zero
                     go to acc-cli-esl-964.
           go to     acc-cli-esl-962.
       acc-cli-esl-964.
           subtract  1                    from w-acc-ser-edd-c0a      .
           move      w-acc-ser-edd-c0a    to   rr-cod-cli-els         .
       acc-cli-esl-969.
           exit.
       acc-cli-esl-999.
           exit.

      *    *===========================================================*
      *    * Preparazione codici selezionati                           *
      *    *-----------------------------------------------------------*
       pcs-cli-esl-000.
      *              *-------------------------------------------------*
      *              * Preparazione di un campo text di massimo 3 li-  *
      *              * nee da 40 caratteri ciascuna                    *
      *              *-------------------------------------------------*
           move      spaces               to   w-acc-ser-edd-txt      .
      *              *-------------------------------------------------*
      *              * Test se da eseguire                             *
      *              *-------------------------------------------------*
           if        rr-cod-cli-sns       =    zero
                     go to pcs-cli-esl-900.
           move      zero                 to   w-acc-ser-edd-c01      .
       pcs-cli-esl-100.
      *              *-------------------------------------------------*
      *              * Ciclo di preparazione                           *
      *              *-------------------------------------------------*
           if        w-acc-ser-edd-txt
                    (119:1)               not  = spaces
                     go to pcs-cli-esl-900.
           add       1                    to   w-acc-ser-edd-c01      .
           if        w-acc-ser-edd-c01    >    w-acc-ser-edd-max
                     go to pcs-cli-esl-900.
           if        rr-cod-cli-eco
                    (w-acc-ser-edd-c01)   =    zero
                     go to pcs-cli-esl-900.
      *              *-------------------------------------------------*
      *              * Editing codice elemento                         *
      *              *-------------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      rr-cod-cli-eco
                    (w-acc-ser-edd-c01)   to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Concatenamento                                  *
      *              *-------------------------------------------------*
           move      120                  to   w-all-str-lun          .
           move      06                   to   w-all-str-num          .
           if        w-acc-ser-edd-txt    =    spaces
                     move  "("            to   w-all-str-cat (1)
           else      move  spaces         to   w-all-str-cat (1)      .
           move      w-acc-ser-edd-rtr (1)
                                          to   w-all-str-cat (2)      .
           move      w-acc-ser-edd-rtr (2)
                                          to   w-all-str-cat (3)      .
           move      w-acc-ser-edd-rtr (3)
                                          to   w-all-str-cat (4)      .
           if        w-acc-ser-edd-txt    =    spaces
                     move  spaces         to   w-all-str-cat (5)
           else      move  ","            to   w-all-str-cat (5)      .
           move      v-edt                to   w-all-str-cat (6)      .
           perform   all-str-cat-000      thru all-str-cat-999        .
           move      w-all-str-alf        to   w-acc-ser-edd-txt      .
       pcs-cli-esl-800.
      *              *-------------------------------------------------*
      *              * Riciclo                                         *
      *              *-------------------------------------------------*
           go to     pcs-cli-esl-100.
       pcs-cli-esl-900.
      *              *-------------------------------------------------*
      *              * Valore in uscita                                *
      *              *-------------------------------------------------*
           if        w-acc-ser-edd-txt    =    spaces
                     go to pcs-cli-esl-999.
      *              *-------------------------------------------------*
      *              * Completamento valore in uscita                  *
      *              *-------------------------------------------------*
           move      120                  to   w-all-str-lun          .
           move      04                   to   w-all-str-num          .
           move      w-acc-ser-edd-rtr (1)
                                          to   w-all-str-cat (1)      .
           move      w-acc-ser-edd-rtr (2)
                                          to   w-all-str-cat (2)      .
           move      w-acc-ser-edd-rtr (3)
                                          to   w-all-str-cat (3)      .
           move      ")"                  to   w-all-str-cat (4)      .   
           perform   all-str-cat-000      thru all-str-cat-999        .
           move      w-all-str-alf        to   w-acc-ser-edd-txt      .
           if        w-acc-ser-edd-txt
                    (120:1)               not  = ")"  and
                     w-acc-ser-edd-txt
                    (120:1)               not  = spaces
                     move  "...)"         to   w-acc-ser-edd-txt
                                              (117:4)                 .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     pcs-cli-esl-999.
       pcs-cli-esl-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione codici selezionati                        *
      *    *-----------------------------------------------------------*
       vcs-cli-esl-000.
      *              *-------------------------------------------------*
      *              * Pulizia della prima riga e segnale di selezione *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      10                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      30                   to   v-pos                  .
           if        w-acc-ser-edd-txt    =    spaces
                     move  "Tutti     "   to   v-alf
           else      move  "+         "   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vcs-cli-esl-100.
      *              *-------------------------------------------------*
      *              * Preparazione contatore                          *
      *              *-------------------------------------------------*
           move      zero                 to   w-acc-ser-edd-c01      .
       vcs-cli-esl-200.
      *              *-------------------------------------------------*
      *              * Ciclo di preparazione                           *
      *              *-------------------------------------------------*
           add       1                    to   w-acc-ser-edd-c01      .
           if        w-acc-ser-edd-c01    >    1
                     go to vcs-cli-esl-900.
       vcs-cli-esl-300.
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      16                   to   v-lin                  .
           add       w-acc-ser-edd-c01    to   v-lin                  .
           move      41                   to   v-pos                  .
           move      w-acc-ser-edd-rtr
                    (w-acc-ser-edd-c01)   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vcs-cli-esl-400.
      *              *-------------------------------------------------*
      *              * Riciclo                                         *
      *              *-------------------------------------------------*
           go to     vcs-cli-esl-200.
       vcs-cli-esl-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     vcs-cli-esl-999.
       vcs-cli-esl-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Esclusione documenti                 *
      *    *-----------------------------------------------------------*
       acc-sne-doc-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        rr-tip-ope           not  = 01
                     go to acc-sne-doc-999.
       acc-sne-doc-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-sne-doc-lun    to   v-car                  .
           move      w-exp-sne-doc-num    to   v-ldt                  .
           move      "SN#"                to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      19                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-sne-doc-tbl    to   v-txt                  .
           move      rr-sne-doc           to   v-num                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-sne-doc-999.
       acc-sne-doc-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-sne-doc             .
       acc-sne-doc-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se zero : reimpostazione                    *
      *                  *---------------------------------------------*
           if        rr-sne-doc           not  = zero
                     go to acc-sne-doc-600.
           if        v-key                =    "UP  "
                     go to acc-sne-doc-600
           else      go to acc-sne-doc-100.
       acc-sne-doc-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-sne-doc-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-sne-doc-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-sne-doc-100.
       acc-sne-doc-999.
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
      *              * Controllo su data scadenza minima e massima     *
      *              *-------------------------------------------------*
           if        rr-dat-min           =    zero or
                     rr-dat-max           =    zero
                     go to tdo-ric-sel-130.
           if        rr-dat-max           not  < rr-dat-min
                     go to tdo-ric-sel-130.
           move      "Data documenti massima inferiore alla data emissio
      -              "ne  minima     "    to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-130.
      *              *-------------------------------------------------*
      *              * Test su numero fattura minimo e massimo         *
      *              *-------------------------------------------------*
           if        rr-num-min           =    zero or
                     rr-num-max           =    zero
                     go to tdo-ric-sel-200.
           if        rr-num-min           not  > rr-num-max
                     go to tdo-ric-sel-200.
           move      "Numero fattura iniziale maggiore di quella finale 
      -              "!              "    to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-200.
      *              *-------------------------------------------------*
      *              * Controllo su stampabilita' documento            *
      *              *-------------------------------------------------*
       tdo-ric-sel-300.
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
      *                  * Visualizzazione messaggio                   *
      *                  *---------------------------------------------*
           perform   box-msg-err-000      thru box-msg-err-999        .
       tdo-ric-sel-920.
      *                  *---------------------------------------------*
      *                  * Uscita con errore                           *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cnt-tdo-ric-flg      .
       tdo-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Regolarizzazione dei parametri di selezione               *
      *    *-----------------------------------------------------------*
       reg-ric-sel-000.
      *              *-------------------------------------------------*
      *              * Data fattura massima                            *
      *              *-------------------------------------------------*
           if        rr-dat-max           not  = zero
                     go to reg-ric-sel-100.
           if        rr-dat-min           =    zero
                     move  9999999        to   rr-dat-max
           else      move  rr-dat-min     to   rr-dat-max             .
       reg-ric-sel-100.
      *              *-------------------------------------------------*
      *              * Numero fattura massimo                          *
      *              *-------------------------------------------------*
           if        rr-num-max           not  = zero
                     go to reg-ric-sel-200.
           if        rr-num-min           =    zero
                     move  999999999      to   rr-num-max
           else      move  rr-num-min     to   rr-num-max             .
       reg-ric-sel-200.
      *              *-------------------------------------------------*
      *              * Regolarizzazione numero documento interno min   *
      *              *-------------------------------------------------*
           move      rr-num-min           to   w-wrk-num-acc          .
           move      w-wrk-num-saa        to   w-wrk-doc-saa          .
           move      rr-dpz-inu           to   w-wrk-doc-dpz          .
           move      w-wrk-num-npg        to   w-wrk-doc-npg          .
           move      w-wrk-doc-int        to   rr-nin-min             .
      *              *-------------------------------------------------*
      *              * Regolarizzazione numero documento interno max   *
      *              *-------------------------------------------------*
           move      rr-num-max           to   w-wrk-num-acc          .
           move      w-wrk-num-saa        to   w-wrk-doc-saa          .
           move      rr-dpz-inu           to   w-wrk-doc-dpz          .
           move      w-wrk-num-npg        to   w-wrk-doc-npg          .
           move      w-wrk-doc-int        to   rr-nin-max             .
      *              *-------------------------------------------------*
      *              * Regolarizzazione codice cliente singolo o mul-  *
      *              * tiplo                                           *
      *              *-------------------------------------------------*
           if        rr-cod-cli-sns       not  = zero
           move      zero                 to   rr-cod-cli             .
           move      spaces               to   rr-cod-cli-rag         .
           move      spaces               to   rr-cod-cli-via         .
           move      spaces               to   rr-cod-cli-loc         .
                     go to reg-ric-sel-300.
           move      zero                 to   rr-cod-cli-els         .
       reg-ric-sel-250.
           add       1                    to   rr-cod-cli-els         .
           if        rr-cod-cli-els       >    36
                     go to reg-ric-sel-260.
           move      zero                 to   rr-cod-cli-eco
                                              (rr-cod-cli-els)        .
           move      spaces               to   rr-cod-cli-ers
                                              (rr-cod-cli-els)        .
           go to     reg-ric-sel-250.
       reg-ric-sel-260.
           move      zero                 to   rr-cod-cli-els         .
       reg-ric-sel-300.
      *              *-------------------------------------------------*
      *              * Eventuale ritaratura segnale di si/no stampa    *
      *              *-------------------------------------------------*
           if        rr-tip-ope           =    02
                     move  "S"            to   w-cnt-fun-snx-stp
           else      move  "N"            to   w-cnt-fun-snx-stp      .
      *              *-------------------------------------------------*
      *              * Esclusione documenti                            *
      *              *-------------------------------------------------*
           if        rr-sne-doc           =    zero
                     move  01             to   rr-sne-doc             .
      *              *-------------------------------------------------*
      *              * Documenti da verificare                         *
      *              *-------------------------------------------------*
           if        rr-snx-fav           =    zero
                     move  01             to   rr-snx-fav             .
       reg-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione richieste di selezione                    *
      *    *-----------------------------------------------------------*
       nor-ric-sel-000.
           perform   nor-ric-sel-cli-000  thru nor-ric-sel-cli-999    .
           move      zero                 to   rr-tip-ope             .
           move      zero                 to   rr-num-dst             .
           move      spaces               to   rr-tip-mov             .
           move      spaces               to   rr-tip-mov-des         .
           move      zero                 to   rr-tip-mov-vld         .
           move      zero                 to   rr-tip-mov-dpz         .
           move      zero                 to   rr-tip-mov-ngi         .
           move      zero                 to   rr-dat-min             .
           move      zero                 to   rr-dat-max             .
      *
           move      zero                 to   rr-num-min             .
           move      zero                 to   rr-num-max             .
      *
           move      zero                 to   rr-nin-min             .
           move      zero                 to   rr-nin-max             .
      *
           move      zero                 to   rr-sne-doc             .
           move      zero                 to   rr-snx-fav             .
       nor-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione richieste di selezione                    *
      *    *                                                           *
      *    * Normalizzazione specifica per codici cliente              *
      *    *-----------------------------------------------------------*
       nor-ric-sel-cli-000.
           move      zero                 to   rr-cod-cli             .
           move      spaces               to   rr-cod-cli-rag         .
           move      spaces               to   rr-cod-cli-via         .
           move      spaces               to   rr-cod-cli-loc         .
           move      zero                 to   rr-cod-cli-sns         .
           move      zero                 to   rr-cod-cli-els         .
       nor-ric-sel-cli-100.
           add       1                    to   rr-cod-cli-els         .
           if        rr-cod-cli-els       >    36
                     go to nor-ric-sel-cli-200.
           move      zero                 to   rr-cod-cli-eco
                                              (rr-cod-cli-els)        .
           move      spaces               to   rr-cod-cli-ers
                                              (rr-cod-cli-els)        .
           go to     nor-ric-sel-cli-100.
       nor-ric-sel-cli-200.
           move      zero                 to   rr-cod-cli-els         .
           go to     nor-ric-sel-cli-999.
       nor-ric-sel-cli-999.
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
           move      15                   to   v-lto                  .
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
      *              *                                                 *
      *              *   - L : Libero                                  *
      *              *   - T : Tipografico                             *
      *              *-------------------------------------------------*
           move      "L"                  to   w-cnt-stp-tip-mod      .
      *              *-------------------------------------------------*
      *              * Ampiezza linea di stampa in caratteri           *
      *              *-------------------------------------------------*
           move      132                  to   w-cnt-stp-amp-lin      .
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
      *                  *---------------------------------------------*
      *                  * Flag destinato al controllo del Tipo Stampa *
      *                  * per inibire la possibilita' della stampa a  *
      *                  * Video                                       *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cnt-stp-esp-fut
                                              (98 : 1)                .
      *              *-------------------------------------------------*
      *              * Area riservata per espansioni speciali          *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-stp-fnz-spc      .
       pre-prm-stp-999.
           exit.

      *    *===========================================================*
      *    * Determinazione ultimo numero distinta                     *
      *    *-----------------------------------------------------------*
       det-ult-num-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione valore in uscita                *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-ult-num-dst      .
       det-ult-num-100.
      *              *-------------------------------------------------*
      *              * Start su file [dbe]                             *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NG"                 to   f-cfr                  .
           move      "NUMDST    "         to   f-key                  .
           move      999999999            to   rf-dbe-num-dst         .
           move      99999                to   rf-dbe-num-prg         .
           move      "pgm/bol/fls/ioc/obj/iofdbe"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dbe                 .
      *                  *---------------------------------------------*
      *                  * Se Start ad uscita                          *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-ult-num-900.
       det-ult-num-200.
      *              *-------------------------------------------------*
      *              * Read Previous su [dbe]                          *
      *              *-------------------------------------------------*
           move      "RP"                 to   f-ope                  .
           move      "pgm/bol/fls/ioc/obj/iofdbe"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dbe                 .
      *                  *---------------------------------------------*
      *                  * Se fine file il test di congruenza con il   *
      *                  * numero successivo si considera superato, e  *
      *                  * si esce                                     *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-ult-num-900.
       det-ult-num-500.
      *              *-------------------------------------------------*
      *              * Valore determinato                              *
      *              *-------------------------------------------------*
           move      rf-dbe-num-dst       to   w-det-ult-num-dst      .
       det-ult-num-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-ult-num-999.
       det-ult-num-999.
           exit.

      *    *===========================================================*
      *    * Find su archivio [dbe]                                    *
      *    *-----------------------------------------------------------*
       fnd-arc-dst-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di selezione               *
      *              *-------------------------------------------------*
           move      spaces               to   w-fnd-arc-dst-sel      .
      *              *-------------------------------------------------*
      *              * Test se programma di interrogazione gia' attivo *
      *              *-------------------------------------------------*
           move      "P?"                 to   s-ope                  .
           move      "pbol5410"           to   s-pro                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-liv                not  = zero
                     move  "#"            to   w-fnd-arc-dst-sel
                     go to  fnd-arc-dst-999.
      *              *-------------------------------------------------*
      *              * Preparazione variabile di i.p.c. per possibili- *
      *              * ta' di function-key "SLCT" durante l'interroga- *
      *              * zione                                           *
      *              *-------------------------------------------------*
           move      "PV"                 to   s-ope                  .
           move      "fkselect"           to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      04                   to   s-car                  .
           move      "SLCT"               to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Richiamo programma di interrogazione            *
      *              *-------------------------------------------------*
           move      "pgm/bol/prg/obj/pbol5410"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat                                            .
           cancel    s-pat                                            .
      *              *-------------------------------------------------*
      *              * Estrazione di eventuale variabile di i.p.c. de- *
      *              * terminata da function-key "SLCT" durante l'in-  *
      *              * terrogazione                                    *
      *              *-------------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "num-prt"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  spaces         to   w-fnd-arc-dst-sel
                     move  s-num          to   w-fnd-arc-dst-prt
           else      move  "#"            to   w-fnd-arc-dst-sel      .
       fnd-arc-dst-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [dbe]                         *
      *    *-----------------------------------------------------------*
       let-arc-dst-000.
      *              *-------------------------------------------------*
      *              * Preparazione flag di uscita                     *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-dst-flg      .
       let-arc-dst-100.
      *              *-------------------------------------------------*
      *              * Start su file [dbe]                             *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "NUMDST    "         to   f-key                  .
           move      w-let-arc-dst-prt    to   rf-dbe-num-dst         .
           move      "pgm/bol/fls/ioc/obj/iofdbe"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dbe                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata : ad uscita                 *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to let-arc-dst-900.
       let-arc-dst-200.
      *              *-------------------------------------------------*
      *              * Next su [dbe]                                   *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/bol/fls/ioc/obj/iofdbe"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dbe                 .
      *                  *---------------------------------------------*
      *                  * Se 'at end' : ad uscita                     *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to let-arc-dst-900.
       let-arc-dst-300.
      *              *-------------------------------------------------*
      *              * Max su [dbe], se non superato : ad uscita       *
      *              *-------------------------------------------------*
           if        rf-dbe-num-dst       not  = w-let-arc-dst-prt
                     go to let-arc-dst-900.
       let-arc-dst-400.
      *              *-------------------------------------------------*
      *              * Preparazione flag di uscita                     *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-dst-flg      .
       let-arc-dst-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-dst-999. 
       let-arc-dst-999.
           exit.

      *    *===========================================================*
      *    * Find su archivio [bit]                                    *
      *    *-----------------------------------------------------------*
       fnd-arc-bit-000.
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
           move      rr-dpz-inu           to   s-num                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Test se programma di interrogazione gia' attivo *
      *              *-------------------------------------------------*
           move      "P?"                 to   s-ope                  .
           move      "pbol3010"           to   s-pro                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-liv                not  = zero
                     go to  fnd-arc-bit-999.
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
      *    * Routine di lettura archivio [zbi]                         *
      *    *-----------------------------------------------------------*
       let-arc-zbi-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zbi-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice a spazi                          *
      *              *-------------------------------------------------*
           if        w-let-arc-zbi-cod    =    spaces
                     go to let-arc-zbi-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODTMB"             to   f-key                  .
           move      w-let-arc-zbi-cod    to   rf-zbi-cod-tmb         .
           move      w-let-arc-zbi-dpz    to   rf-zbi-cod-dpz         .
           move      "pgm/bol/fls/ioc/obj/iofzbi"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zbi                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-zbi-400.
       let-arc-zbi-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-zbi-des-tmb       to   w-let-arc-zbi-des      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-zbi-999.
       let-arc-zbi-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-zbi-flg      .
           move      all   "."            to   w-let-arc-zbi-des      .
           go to     let-arc-zbi-999.
       let-arc-zbi-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zbi-des      .
       let-arc-zbi-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [cli]                         *
      *    *-----------------------------------------------------------*
       let-arc-cli-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-cli-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice cliente a zero                   *
      *              *-------------------------------------------------*
           if        w-let-arc-cli-cod    =    zero
                     go to let-arc-cli-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCLI    "         to   f-key                  .
           move      w-let-arc-cli-cod    to   rf-cli-cod-cli         .
           move      "pgm/cge/fls/ioc/obj/iofcli"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cli                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-cli-400.
       let-arc-cli-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-cli-rag-soc       to   w-let-arc-cli-rag      .
           move      rf-cli-via-cli       to   w-let-arc-cli-via      .
           move      rf-cli-loc-cli       to   w-let-arc-cli-loc      .
           move      rf-cli-cod-iva       to   w-let-arc-cli-ass      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-cli-999.
       let-arc-cli-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-cli-flg      .
           move      all   "."            to   w-let-arc-cli-rag      .
           go to     let-arc-cli-600.
       let-arc-cli-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-cli-rag      .
       let-arc-cli-600.
           move      spaces               to   w-let-arc-cli-via      .
           move      spaces               to   w-let-arc-cli-loc      .
           move      zero                 to   w-let-arc-cli-ass      .
       let-arc-cli-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [dcc]                         *
      *    *-----------------------------------------------------------*
       let-arc-dcc-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-dcc-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice a zero                           *
      *              *-------------------------------------------------*
           if        w-let-arc-dcc-cli    =    zero
                     go to let-arc-dcc-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCLI"             to   f-key                  .
           move      w-let-arc-dcc-cli    to   rf-dcc-cod-cli         .
           move      w-let-arc-dcc-dpz    to   rf-dcc-dpz-cli         .
           move      "pgm/dcc/fls/ioc/obj/iofdcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-dcc-400.
       let-arc-dcc-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-dcc-rag-soc       to   w-let-arc-dcc-rag      .
           move      rf-dcc-via-dcc       to   w-let-arc-dcc-via      .
           move      rf-dcc-loc-dcc       to   w-let-arc-dcc-loc      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-dcc-999.
       let-arc-dcc-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-dcc-flg      .
           move      all"."               to   w-let-arc-dcc-rag      .
           go to     let-arc-dcc-600.
       let-arc-dcc-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-dcc-rag      .
       let-arc-dcc-600.
           move      spaces               to   w-let-arc-dcc-via      .
           move      spaces               to   w-let-arc-dcc-loc      .
       let-arc-dcc-999.
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
      *    * Subroutines per l'accettazione tipo movimento per la fat- *
      *    * turazione                                                 *
      *    *-----------------------------------------------------------*
           copy      "pgm/bol/prg/cpy/acdezbi0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione codice cliente commerciale *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcc/prg/cpy/acmndcc0.acs"                   .

