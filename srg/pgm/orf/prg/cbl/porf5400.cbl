       Identification Division.
       Program-Id.                                 porf5400           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    orf                 *
      *                                Settore:    con                 *
      *                                   Fase:    orf540              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 27/06/05    *
      *                       Ultima revisione:    NdK del 31/03/21    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Richieste per il programma di stampa orf540 *
      *                    Stampa distinte di invio                    *
      *                                                                *
      *                    Le overlay eseguono le seguenti funzioni :  *
      *                    ------------------------------------------  *
      *                    porf5400 - Main                             *
      *                    porf540a - Creazione Distinta di invio      *
      *                    porf540b - Stampa Distinta                  *
      *                    porf540m - Manutenzione distinta            *
      *                    porf540c - Generazione documenti PDF        *
      *                    porf540d - Invio o re-invio documenti PDF   *
      *                    porf540e - Cancellazione Distinta           *
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
                     "orf"                                            .
      *        *-------------------------------------------------------*
      *        * Settore gestionale                                    *
      *        *-------------------------------------------------------*
           05  i-ide-set                  pic  x(03) value
                     "con"                                            .
      *        *-------------------------------------------------------*
      *        * Fase gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-fas                  pic  x(06) value
                     "orf540"                                         .
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10) value
                     "porf5400"                                       .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  i-ide-des                  pic  x(40) value
                     "   GESTIONE INVIO ELETTRONICO ORDINI    "       .

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
      *        * [ofe]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/orf/fls/rec/rfofe"                          .
      *        *-------------------------------------------------------*
      *        * [yof]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/orf/fls/rec/rfyof"                          .
      *        *-------------------------------------------------------*
      *        * [fnt]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rffnt"                          .
      *        *-------------------------------------------------------*
      *        * [dcf]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcf/fls/rec/rfdcf"                          .

      *    *===========================================================*
      *    * Work-area richieste per stampa serie 'orf540'             *
      *    *-----------------------------------------------------------*
           copy      "pgm/orf/prg/cpy/porf5400.pgl"                   .

      *    *===========================================================*
      *    * Work-area referenze                                       *
      *    *-----------------------------------------------------------*
       01  w-ref.
      *        *-------------------------------------------------------*
      *        * Referenze relative relative ai parametri di invio     *
      *        * tramite mail delle conferme ordine in formato PDF     *
      *        *-------------------------------------------------------*
           05  w-ref-prm-inv.
               10  w-ref-prm-inv-ctr      pic  9(01)                  .
               10  w-ref-prm-inv-max      pic  9(01)      value 6     .
               10  w-ref-prm-inv-azi      pic  x(04)                  .
               10  w-ref-prm-inv-dms      pic  x(80)                  .
               10  w-ref-prm-inv-dpm      pic  x(80)                  .
               10  w-ref-prm-inv-usr      pic  x(80)                  .
               10  w-ref-prm-inv-pwd      pic  x(80)                  .
               10  w-ref-prm-inv-cst   occurs  6.
                   15  w-ref-prm-inv-cpr.
                       20  w-ref-prm-inv-prm
                                          pic  x(80)                  .
      *        *-------------------------------------------------------*
      *        * Referenza relativa default per il tipo movimento      *
      *        *-------------------------------------------------------*
           05  w-ref-cod-tmo.
      *            *---------------------------------------------------*
      *            * Modalita' di trattamento default                  *
      *            *                                                   *
      *            *  - ' ' : Nessun default                           *
      *            *  - 'I' : Default iniziale in entrata programma    *
      *            *  - 'C' : Default continuo, anche cioe' se valore  *
      *            *          successivamente variato                  *
      *            *  - 'O' : Default obbligatorio, cioe' valore non   *
      *            *          modificabile per nessun motivo           *
      *            *---------------------------------------------------*
               10  w-ref-cod-tmo-trt      pic  x(01)                  .
               10  filler                 pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Codice tipo movimento di default                  *
      *            *---------------------------------------------------*
               10  w-ref-cod-tmo-cod      pic  x(05)                  .

      *    *===========================================================*
      *    * Work per valori di default                                *
      *    *-----------------------------------------------------------*
       01  w-def.
      *        *-------------------------------------------------------*
      *        * Tipo movimento per ordine fornitore                   *
      *        *-------------------------------------------------------*
           05  w-def-tmo-orf              pic  x(05)                  .
           05  w-def-tmo-orf-flg          pic  x(01)                  .

      *    *===========================================================*
      *    * Work-area per bufferizzazione tipi movimento              *
      *    *-----------------------------------------------------------*
       01  w-tmo.
      *        *-------------------------------------------------------*
      *        * Contatore di elementi                                 *
      *        *-------------------------------------------------------*
           05  w-tmo-ctr-ele              pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Codice di default                                     *
      *        *-------------------------------------------------------*
           05  w-tmo-cod-tmo              pic  x(05)                  .
      *        *-------------------------------------------------------*
      *        * Descrizione di default                                *
      *        *-------------------------------------------------------*
           05  w-tmo-des-tmo              pic  x(30)                  .

      *    *===========================================================*
      *    * Work per subroutines di Find                              *
      *    *-----------------------------------------------------------*
       01  w-fnd.
      *        *-------------------------------------------------------*
      *        * Work per Find su archivio [ofe]                       *
      *        *-------------------------------------------------------*
           05  w-fnd-arc-dst.
               10  w-fnd-arc-dst-sel      pic  x(01)                  .
               10  w-fnd-arc-dst-prt      pic  9(09)                  .
               
      *    *===========================================================*
      *    * Work per subroutines di Let                               *
      *    *-----------------------------------------------------------*
       01  w-let.
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [ofe]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-dst.
               10  w-let-arc-dst-flg      pic  x(01)                  .
               10  w-let-arc-dst-prt      pic  9(09)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [yof]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-yof.
               10  w-let-arc-yof-flg      pic  x(01)                  .
               10  w-let-arc-yof-cod      pic  x(05)                  .
               10  w-let-arc-yof-des      pic  x(30)                  .
               10  w-let-arc-yof-vld      pic  9(02)                  .
               10  w-let-arc-yof-dpz      pic  9(02)                  .
               10  w-let-arc-yof-ord      pic  9(02)                  .
               10  w-let-arc-yof-prd      pic  9(02)                  .
               10  w-let-arc-yof-sgl      pic  x(03)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [fnt]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-fnt.
               10  w-let-arc-fnt-flg      pic  x(01)                  .
               10  w-let-arc-fnt-cod      pic  9(07)                  .
               10  w-let-arc-fnt-rag      pic  x(40)                  .
               10  w-let-arc-fnt-via      pic  x(40)                  .
               10  w-let-arc-fnt-loc      pic  x(40)                  .
               10  w-let-arc-fnt-ass      pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [dcf]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-dcf.
               10  w-let-arc-dcf-flg      pic  x(01)                  .
               10  w-let-arc-dcf-fnt      pic  9(07)                  .
               10  w-let-arc-dcf-dpz      pic  x(04)                  .
               10  w-let-arc-dcf-rag      pic  x(40)                  .
               10  w-let-arc-dcf-via      pic  x(40)                  .
               10  w-let-arc-dcf-loc      pic  x(40)                  .

      *    *===========================================================*
      *    * Work per accettazioni campi espansi                       *
      *    *-----------------------------------------------------------*
       01  w-exp.
      *        *-------------------------------------------------------*
      *        * Work per : Tipo di operazione                         *
      *        *-------------------------------------------------------*
           05  w-exp-tip-ope.
               10  w-exp-tip-ope-num      pic  9(02)       value 7    .
               10  w-exp-tip-ope-lun      pic  9(02)       value 40   .
               10  w-exp-tip-ope-tbl.
                   15  filler             pic  x(40) value
                            "1. Creazione distinta ordini a fornitori".
                   15  filler             pic  x(40) value
                            "2. Stampa distinta                      ".
                   15  filler             pic  x(40) value
                            "3. Manutenzione distinta                ".
                   15  filler             pic  x(40) value
                            "4. Generazione documenti - PDF          ".
                   15  filler             pic  x(40) value
                            "5. Invio ordini a fornitori             ".
                   15  filler             pic  x(40) value
                            "6. Re-invio ordini a fornitori          ".
                   15  filler             pic  x(40) value
                            "7. Eliminazione distinta                ".
      *        *-------------------------------------------------------*
      *        * Work per : Esclusione documenti senza mail            *
      *        *-------------------------------------------------------*
           05  w-exp-sne-eml.
               10  w-exp-sne-eml-num      pic  9(02)       value 2    .
               10  w-exp-sne-eml-lun      pic  9(02)       value 2    .
               10  w-exp-sne-eml-tbl.
                   15  filler             pic  x(02) value "Si"       .
                   15  filler             pic  x(02) value "No"       .
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
      *        * Work per : Documenti da verificare                    *
      *        *-------------------------------------------------------*
           05  w-exp-snx-oav.
               10  w-exp-snx-oav-num      pic  9(02)       value 2    .
               10  w-exp-snx-oav-lun      pic  9(02)       value 2    .
               10  w-exp-snx-oav-tbl.
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
      *    * Work area locale                                          *
      *    *-----------------------------------------------------------*
       01  w-wrk.
      *        *-------------------------------------------------------*
      *        * Numero ordine per accettazione                        *
      *        *-------------------------------------------------------*
           05  w-wrk-nft-acc              pic  9(09)                  .
           05  w-wrk-nft-acc-r            redefines
               w-wrk-nft-acc.
               10  w-wrk-num-saa          pic  9(03)                  .
               10  w-wrk-num-npg          pic  9(06)                  .
      *        *-------------------------------------------------------*
      *        * Numero ordine internamente                            *
      *        *-------------------------------------------------------*
           05  w-wrk-nft-int              pic  9(11)                  .
           05  w-wrk-nft-int-r            redefines
               w-wrk-nft-int.
               10  w-wrk-noc-saa          pic  9(03)                  .
               10  w-wrk-noc-dpz          pic  9(02)                  .
               10  w-wrk-noc-npg          pic  9(06)                  .

      *    *===========================================================*
      *    * Work-area per salvataggi                                  *
      *    *-----------------------------------------------------------*
       01  w-sav.
           05  w-sav-cod-fnt              pic  9(07)                  .

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
      *    * Link-area per accettazione codice tipo movimento          *
      *    *-----------------------------------------------------------*
           copy      "pgm/orf/prg/cpy/acdeyof0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice fornitore commerciale   *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcf/prg/cpy/acmndcf0.acl"                   .

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
                     move  "pgm/orf/prg/obj/porf540a                "
                                          to   i-exe-pat
           else if   rr-tip-ope           =    02
                     move  "pgm/orf/prg/obj/porf540b                "
                                          to   i-exe-pat
           else if   rr-tip-ope           =    03
                     move  "pgm/orf/prg/obj/porf540m                "
                                          to   i-exe-pat
           else if   rr-tip-ope           =    04
                     move  "pgm/orf/prg/obj/porf540c                "
                                          to   i-exe-pat
           else if   rr-tip-ope           =    05
                     move  "pgm/orf/prg/obj/porf540d                "
                                          to   i-exe-pat
           else if   rr-tip-ope           =    06
                     move  "pgm/orf/prg/obj/porf540d                "
                                          to   i-exe-pat
           else if   rr-tip-ope           =    07
                     move  "pgm/orf/prg/obj/porf540e                "
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
       pre-exe-pgm-010.
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
       pre-exe-pgm-800.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazioni                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Si/no ordine in attesa di verifica          *
      *                  *---------------------------------------------*
           perform   prs-snx-oav-000      thru prs-snx-oav-999        .
       pre-exe-pgm-820.
      *              *-------------------------------------------------*
      *              * Referenza per default tipo movimento            *
      *              *-------------------------------------------------*
           perform   ref-cod-tmo-000      thru ref-cod-tmo-999        .
      *              *-------------------------------------------------*
      *              * Normalizzazione default per tipo movimento      *
      *              *-------------------------------------------------*
           move      w-ref-cod-tmo-cod    to   w-def-tmo-orf          .
      *              *-------------------------------------------------*
      *              * Precaricamento tabella tipi movimento per de-   *
      *              * faults                                          *
      *              *-------------------------------------------------*
           perform   pre-tbl-tmo-000      thru pre-tbl-tmo-999        .
       pre-exe-pgm-840.
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
                     go to pre-exe-pgm-860.
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
       pre-exe-pgm-860.
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
       pre-exe-pgm-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     pre-exe-pgm-999.
       pre-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Lettura personalizzazione : Si/no ordine in attesa di     *
      *    *                             verifica                      *
      *    *                             ___ DA IMPLEMENTARE ___       *
      *    *-----------------------------------------------------------*
       prs-snx-oav-000.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione                       *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/orf/mov/orf300[snx-oav]"
                                          to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-alf          to   w-prs-snx-oav
           else      move  spaces         to   w-prs-snx-oav          .
      *              *-------------------------------------------------*
      *              * Normalizzazione personalizzazione               *
      *              *-------------------------------------------------*
           if        w-prs-snx-oav        not   = "S"
                     move  "N"            to   w-prs-snx-oav          .
       prs-snx-oav-999.
           exit.

      *    *===========================================================*
      *    * Lettura delle referenze relative al default per il tipo   *
      *    * movimento                                                 *
      *    *-----------------------------------------------------------*
       ref-cod-tmo-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione work-area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-ref-cod-tmo-cod      .
           move      spaces               to   w-ref-cod-tmo-trt      .
       ref-cod-tmo-200.
      *              *-------------------------------------------------*
      *              * Lettura referenza                               *
      *              *-------------------------------------------------*
           move      "R:"                 to   s-ope                  .
           move      "pgm/orf/mov/orf300[def-tmo]"
                                          to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-alf          to   w-ref-cod-tmo
           else      move  spaces         to   w-ref-cod-tmo          .
       ref-cod-tmo-400.
      *              *-------------------------------------------------*
      *              * Normalizzazione valori letti                    *
      *              *-------------------------------------------------*
           if        w-ref-cod-tmo-trt    not  = spaces and
                     w-ref-cod-tmo-trt    not  = "I"    and
                     w-ref-cod-tmo-trt    not  = "C"    and
                     w-ref-cod-tmo-trt    not  = "O"
                     move  spaces         to   w-ref-cod-tmo-trt      .
       ref-cod-tmo-420.
           if        w-ref-cod-tmo-trt    =    spaces
                     move  spaces         to   w-ref-cod-tmo-cod      .
       ref-cod-tmo-999.
           exit.

      *    *===========================================================*
      *    * Precaricamento tabella tipi movimento                     *
      *    *-----------------------------------------------------------*
       pre-tbl-tmo-000.
      *              *-------------------------------------------------*
      *              * Normalizzazioni iniziali                        *
      *              *-------------------------------------------------*
           move      zero                 to   w-tmo-ctr-ele          .
           move      spaces               to   w-tmo-cod-tmo          .
           move      spaces               to   w-tmo-des-tmo          .
       pre-tbl-tmo-100.
      *              *-------------------------------------------------*
      *              * Open file [yof]                                 *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/orf/fls/ioc/obj/iofyof"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-yof                 .
      *              *-------------------------------------------------*
      *              * Inizio ciclo di caricamento                     *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "CODTOF    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      spaces               to   rf-yof-cod-tof         .
           move      "pgm/orf/fls/ioc/obj/iofyof"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-yof                 .
      *                  *---------------------------------------------*
      *                  * Se errore di start : uscita con flag di er- *
      *                  * rore                                        *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to  pre-tbl-tmo-900.
       pre-tbl-tmo-200.
      *              *-------------------------------------------------*
      *              * Read-next su file [yof]                         *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/orf/fls/ioc/obj/iofyof"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-yof                 .
      *                  *---------------------------------------------*
      *                  * Se errore di start : uscita                 *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to pre-tbl-tmo-900.
      *              *-------------------------------------------------*
      *              * Incremento contatore                            *
      *              *-------------------------------------------------*
           add       1                    to   w-tmo-ctr-ele          .
      *              *-------------------------------------------------*
      *              * Bufferizzazioni                                 *
      *              *-------------------------------------------------*
           move      rf-yof-cod-tof       to   w-tmo-cod-tmo          .
           move      rf-yof-des-tof       to   w-tmo-des-tmo          .
      *              *-------------------------------------------------*
      *              * Riciclo a riga successiva                       *
      *              *-------------------------------------------------*
           go to     pre-tbl-tmo-200.
       pre-tbl-tmo-900.
      *              *-------------------------------------------------*
      *              * Close file [yof]                                *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/orf/fls/ioc/obj/iofyof"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-yof                 .
       pre-tbl-tmo-999.
           exit.

      *    *===========================================================*
      *    * Lettura delle referenze relative ai parametri di invio    *
      *    * tramite mail delle conferme ordine in formato PDF         *
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
      *              * Ciclo per 'max' referenze                       *
      *              *-------------------------------------------------*
           move      zero                 to   w-ref-prm-inv-ctr      .
       ref-prm-inv-200.
           add       1                    to   w-ref-prm-inv-ctr      .
           if        w-ref-prm-inv-ctr    >    w-ref-prm-inv-max
                     go to ref-prm-inv-900.
      *                  *---------------------------------------------*
      *                  * Lettura referenza multipla                  *
      *                  *---------------------------------------------*
           move      "R:"                 to   s-ope                  .
           move      w-ref-prm-inv-ctr    to   s-num                  .
           move      "pgm/orf/con/orf540[prm-inv]"
                                          to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Bufferizzazione                             *
      *                  *---------------------------------------------*
           move      s-alf                to   w-ref-prm-inv-prm
                                              (w-ref-prm-inv-ctr)     .
      *                  *---------------------------------------------*
      *                  * Normalizzazione in funzione del progressivo *
      *                  *---------------------------------------------*
           go to     ref-prm-inv-310
                     ref-prm-inv-320
                     ref-prm-inv-330
                     ref-prm-inv-340
                     ref-prm-inv-350
                     ref-prm-inv-360
                                depending on   w-ref-prm-inv-ctr      .
       ref-prm-inv-310.
      *                  *---------------------------------------------*
      *                  * 1. Modulo esterno di invio                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazione                         *
      *                      *-----------------------------------------*
           if        w-ref-prm-inv-prm (1)
                                          =    spaces
                     move  "t_sender"     to   w-ref-prm-inv-prm (1)  .
      *                      *-----------------------------------------*
      *                      * A riciclo                               *
      *                      *-----------------------------------------*
           go to     ref-prm-inv-800.
       ref-prm-inv-320.
      *                  *---------------------------------------------*
      *                  * 2. Codice azienda per direttorio documenti  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazione                         *
      *                      *-----------------------------------------*
           if        w-ref-prm-inv-prm (2)
                                          =    spaces
                     move  "doc "         to   w-ref-prm-inv-prm (2)  .
      *                      *-----------------------------------------*
      *                      * Test                                    *
      *                      *-----------------------------------------*
           if        w-ref-prm-inv-prm (2)
                                          not  = "#   "
                     go to ref-prm-inv-200.
      *                      *-----------------------------------------*
      *                      * Codice azienda in uso                   *
      *                      *-----------------------------------------*
           move      w-ref-prm-inv-azi    to   w-ref-prm-inv-prm (2)  .
      *                      *-----------------------------------------*
      *                      * A riciclo                               *
      *                      *-----------------------------------------*
           go to     ref-prm-inv-800.
       ref-prm-inv-330.
      *                  *---------------------------------------------*
      *                  * 3. Server SMTP per l'inoltro                *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazione                         *
      *                      *-----------------------------------------*
           if        w-ref-prm-inv-prm (3)
                                          =    spaces
                     move  w-ref-prm-inv-dms
                                          to   w-ref-prm-inv-prm (3)  .
      *                      *-----------------------------------------*
      *                      * A riciclo                               *
      *                      *-----------------------------------------*
           go to     ref-prm-inv-800.
       ref-prm-inv-340.
      *                  *---------------------------------------------*
      *                  * 4. Mittente per l'inoltro                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazione                         *
      *                      *-----------------------------------------*
           if        w-ref-prm-inv-prm (4)
                                          =    spaces
                     move  w-ref-prm-inv-dpm
                                          to   w-ref-prm-inv-prm (4)  .
      *                      *-----------------------------------------*
      *                      * A riciclo                               *
      *                      *-----------------------------------------*
           go to     ref-prm-inv-800.
       ref-prm-inv-350.
      *                  *---------------------------------------------*
      *                  * 5. Username per l'inoltro                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazione                         *
      *                      *-----------------------------------------*
           if        w-ref-prm-inv-prm (5)
                                          =    spaces
                     move  w-ref-prm-inv-usr
                                          to   w-ref-prm-inv-prm (5)  .
      *                      *-----------------------------------------*
      *                      * A riciclo                               *
      *                      *-----------------------------------------*
           go to     ref-prm-inv-800.
       ref-prm-inv-360.
      *                  *---------------------------------------------*
      *                  * 6. Password per l'inoltro                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazione                         *
      *                      *-----------------------------------------*
           if        w-ref-prm-inv-prm (6)
                                          =    spaces
                     move  w-ref-prm-inv-pwd
                                          to   w-ref-prm-inv-prm (6)  .
      *                      *-----------------------------------------*
      *                      * A riciclo                               *
      *                      *-----------------------------------------*
           go to     ref-prm-inv-800.
       ref-prm-inv-800.
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     ref-prm-inv-200.
       ref-prm-inv-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     ref-prm-inv-999.
       ref-prm-inv-999.
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
      *              * Open modulo accettazione tipo movimento         *
      *              *-------------------------------------------------*
           perform   cod-des-yof-opn-000  thru cod-des-yof-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice fornitore com-  *
      *              * merciale                                        *
      *              *-------------------------------------------------*
           perform   cod-mne-dcf-opn-000  thru cod-mne-dcf-opn-999    .
      *              *-------------------------------------------------*
      *              * [ofe]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/orf/fls/ioc/obj/iofofe"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ofe                 .
      *              *-------------------------------------------------*
      *              * [yof]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/orf/fls/ioc/obj/iofyof"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-yof                 .
       rou-opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Close files per richieste                                 *
      *    *-----------------------------------------------------------*
       rou-cls-fls-000.
      *              *-------------------------------------------------*
      *              * Close modulo accettazione tipo movimento        *
      *              *-------------------------------------------------*
           perform   cod-des-yof-cls-000  thru cod-des-yof-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice fornitore com- *
      *              * merciale                                        *
      *              *-------------------------------------------------*
           perform   cod-mne-dcf-cls-000  thru cod-mne-dcf-cls-999    .
      *              *-------------------------------------------------*
      *              * [ofe]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/orf/fls/ioc/obj/iofofe"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ofe                 .
      *              *-------------------------------------------------*
      *              * [yof]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/orf/fls/ioc/obj/iofyof"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-yof                 .
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
           perform   acc-tmo-orf-000      thru acc-tmo-orf-999        .
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
                     go to acc-ric-sel-200.
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
      *                  * Numero ordine minimo                        *
      *                  *---------------------------------------------*
           perform   acc-num-min-000      thru acc-num-min-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-350.
       acc-ric-sel-450.
      *                  *---------------------------------------------*
      *                  * Numero ordine massimo                       *
      *                  *---------------------------------------------*
           perform   acc-num-max-000      thru acc-num-max-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-400.
       acc-ric-sel-500.
      *                  *---------------------------------------------*
      *                  * Codice fornitore                            *
      *                  *---------------------------------------------*
           perform   acc-cod-fnt-000      thru acc-cod-fnt-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-450.
       acc-ric-sel-550.
      *                  *---------------------------------------------*
      *                  * Esclusione documenti senza mail             *
      *                  *---------------------------------------------*
           perform   acc-sne-eml-000      thru acc-sne-eml-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-500.
       acc-ric-sel-600.
      *                  *---------------------------------------------*
      *                  * Esclusione documenti                        *
      *                  *---------------------------------------------*
           perform   acc-sne-doc-000      thru acc-sne-doc-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-550.
       acc-ric-sel-650.
      *                  *---------------------------------------------*
      *                  * Documenti da verificare                     *
      *                  *---------------------------------------------*
           perform   acc-snx-oav-000      thru acc-snx-oav-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-600.
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
           perform   vis-tmo-orf-000      thru vis-tmo-orf-999        .
      *              *-------------------------------------------------*
      *              * Visualizzazione tipo movimento da stampare,     *
      *              * descrizione                                     *
      *              *-------------------------------------------------*
           perform   vis-tmo-orf-des-000  thru vis-tmo-orf-des-999    .
      *              *-------------------------------------------------*
      *              * Prompt data documento min - max                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      45                   to   v-car                  .
           move      13                   to   v-lin                  .
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
           move      14                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Numero ordine          dal :             al :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Codice fornitore                                *
      *              *-------------------------------------------------*
           perform   pmt-cod-fnt-000      thru pmt-cod-fnt-999        .
      *              *-------------------------------------------------*
      *              * Esclusione documenti senza mail                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Solo ordini con mail       :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
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
      *                  *---------------------------------------------*
      *                  * Test su personalizzazioni                   *
      *                  *---------------------------------------------*
           if        w-prs-snx-oav        not  = "S"
                     go to pmt-ric-sel-999.
      *                  *---------------------------------------------*
      *                  * Visualizzazione propmt                      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Solo documenti verificati  :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Prompt tipo movimento da stampare                         *
      *    *-----------------------------------------------------------*
       pmt-tip-mov-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tipo movimento             :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-tip-mov-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per Codice fornitore              *
      *    *-----------------------------------------------------------*
       pmt-cod-fnt-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice fornitore           :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cod-fnt-999.
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
           move      "CSMGIRE#"           to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      05                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-tip-ope-tbl    to   v-txt                  .
           move      rr-tip-ope           to   v-num                  .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-ric    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
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
           if        w-cnt-sts-imp-ric    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
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
      *              *-------------------------------------------------*
      *              * Se Find                                         *
      *              *-------------------------------------------------*
           if        v-key                not  = "FIND"
                     go to acc-num-dst-400.
      *                  *---------------------------------------------*
      *                  * Find su archivio [ofe]                      *
      *                  *---------------------------------------------*
           perform   fnd-arc-dst-000      thru fnd-arc-dst-999        .
           if        w-fnd-arc-dst-sel    not  = spaces
                     go to acc-num-dst-100.
           move      w-fnd-arc-dst-prt    to   rr-num-dst             .
      *                  *---------------------------------------------*
      *                  * Visualizzazione numero distinta             *
      *                  *---------------------------------------------*
           perform   vis-num-dst-000      thru vis-num-dst-999        .
       acc-num-dst-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura file [ofe]                          *
      *                  *---------------------------------------------*
           move      rr-num-dst           to   w-let-arc-dst-prt      .
           perform   let-arc-dst-000      thru let-arc-dst-999        .
       acc-num-dst-500.
      *                  *---------------------------------------------*
      *                  * Se numero distinta errato : reimpostazione  *
      *                  *---------------------------------------------*
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
           go to     acc-num-dst-100.
       acc-num-dst-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Eventuale ratifica per Cancellazione        *
      *                  *---------------------------------------------*
           if        rr-tip-ope           not  = 07
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
       acc-tmo-orf-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        rr-tip-ope           not  = 01
                     go to acc-tmo-orf-999.
      *                  *---------------------------------------------*
      *                  * Preparazione valore di default              *
      *                  *---------------------------------------------*
           perform   acc-tmo-orf-def-000  thru acc-tmo-orf-def-999    .
           if        w-def-tmo-orf-flg    not  = spaces
                     go to acc-tmo-orf-400.
       acc-tmo-orf-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-des-yof-ope      .
           move      rr-tip-mov           to   w-cod-des-yof-cod      .
           move      11                   to   w-cod-des-yof-lin      .
           move      30                   to   w-cod-des-yof-pos      .
           move      11                   to   w-cod-des-yof-dln      .
           move      37                   to   w-cod-des-yof-dps      .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           if        w-cnt-sts-imp-ric    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           perform   cod-des-yof-cll-000  thru cod-des-yof-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-des-yof-foi-000  thru cod-des-yof-foi-999    .
       acc-tmo-orf-110.
           perform   cod-des-yof-cll-000  thru cod-des-yof-cll-999    .
           if        w-cod-des-yof-ope    =    "F+"
                     go to acc-tmo-orf-115.
           if        w-cod-des-yof-ope    =    "AC"
                     go to acc-tmo-orf-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-tmo-orf-115.
           perform   cod-des-yof-foi-000  thru cod-des-yof-foi-999    .
           go to     acc-tmo-orf-110.
       acc-tmo-orf-120.
           move      w-cod-des-yof-cod    to   v-alf                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-tmo-orf-999.
       acc-tmo-orf-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   rr-tip-mov             .
       acc-tmo-orf-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura tabella [yof]                       *
      *                  *---------------------------------------------*
           move      rr-tip-mov           to   w-let-arc-yof-cod      .
           perform   let-arc-yof-000      thru let-arc-yof-999        .
           move      w-let-arc-yof-des    to   rr-tip-mov-des         .
           move      w-let-arc-yof-vld    to   rr-tip-mov-vld         .
           move      w-let-arc-yof-dpz    to   rr-tip-mov-dpz         .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione                 *
      *                  *---------------------------------------------*
           perform   vis-tmo-orf-des-000  thru vis-tmo-orf-des-999    .
      *                  *---------------------------------------------*
      *                  * Se codice errato : reimpostazione           *
      *                  *---------------------------------------------*
           if        w-let-arc-yof-flg    not  = spaces
                     go to acc-tmo-orf-100.
      *                  *---------------------------------------------*
      *                  * Se a spaces : reimpostazione                *
      *                  *---------------------------------------------*
           if        rr-tip-mov           =    spaces
                     go to acc-tmo-orf-100.
      *                  *---------------------------------------------*
      *                  * Test su codice dipendenza                   *
      *                  *---------------------------------------------*
           if        rr-tip-mov-vld       not  = 02
                     go to acc-tmo-orf-600.
           if        rr-dpz-inu           =    rr-tip-mov-dpz
                     go to acc-tmo-orf-600.
      *                      *-----------------------------------------*
      *                      * Messaggio di errore                     *
      *                      *-----------------------------------------*
           move      "Tipo movimento incompatibile con il codice dipende
      -              "nza   "             to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                      *-----------------------------------------*
      *                      * A reimpostazione                        *
      *                      *-----------------------------------------*
           go to     acc-tmo-orf-100.
       acc-tmo-orf-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-tmo-orf-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-tmo-orf-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-tmo-orf-100.
       acc-tmo-orf-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo chiave : Tipo movimento                *
      *    *                                                           *
      *    * Subroutine di trattamento del default                     *
      *    *-----------------------------------------------------------*
       acc-tmo-orf-def-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di obbligatorieta'         *
      *              *-------------------------------------------------*
           move      spaces               to   w-def-tmo-orf-flg      .
       acc-tmo-orf-def-100.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo di trattamento  *
      *              * da referenza                                    *
      *              *-------------------------------------------------*
           if        w-ref-cod-tmo-trt    =    spaces
                     go to acc-tmo-orf-def-200
           else if   w-ref-cod-tmo-trt    =    "I"
                     go to acc-tmo-orf-def-300
           else if   w-ref-cod-tmo-trt    =    "C"
                     go to acc-tmo-orf-def-400
           else if   w-ref-cod-tmo-trt    =    "O"
                     go to acc-tmo-orf-def-500
           else      go to acc-tmo-orf-def-900.
       acc-tmo-orf-def-200.
      *              *-------------------------------------------------*
      *              * Se nessun default                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se default da utilizzare               *
      *                  *---------------------------------------------*
           if        rr-tip-mov           not  = spaces
                     go to acc-tmo-orf-def-900.
      *                  *---------------------------------------------*
      *                  * Utilizzo del default                        *
      *                  *---------------------------------------------*
           move      w-def-tmo-orf        to   rr-tip-mov             .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     acc-tmo-orf-def-900.
       acc-tmo-orf-def-300.
      *              *-------------------------------------------------*
      *              * Se default iniziale                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se default da utilizzare               *
      *                  *---------------------------------------------*
           if        rr-tip-mov           not  = spaces
                     go to acc-tmo-orf-def-900.
      *                  *---------------------------------------------*
      *                  * Utilizzo del default                        *
      *                  *---------------------------------------------*
           move      w-def-tmo-orf        to   rr-tip-mov             .
      *                  *---------------------------------------------*
      *                  * A lettura e visualizzazione                 *
      *                  *---------------------------------------------*
           go to     acc-tmo-orf-def-600.
       acc-tmo-orf-def-400.
      *              *-------------------------------------------------*
      *              * Se default continuo                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Utilizzo del default                        *
      *                  *---------------------------------------------*
           move      w-ref-cod-tmo-cod    to   rr-tip-mov             .
      *                  *---------------------------------------------*
      *                  * A lettura e visualizzazione                 *
      *                  *---------------------------------------------*
           go to     acc-tmo-orf-def-600.
       acc-tmo-orf-def-500.
      *              *-------------------------------------------------*
      *              * Se default obbligatorio                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Utilizzo del default                        *
      *                  *---------------------------------------------*
           move      w-def-tmo-orf        to   rr-tip-mov             .
      *                  *---------------------------------------------*
      *                  * Forzatura del flag di obbligatorieta'       *
      *                  *---------------------------------------------*
           move      "#"                  to   w-def-tmo-orf-flg      .
      *                  *---------------------------------------------*
      *                  * A lettura e visualizzazione                 *
      *                  *---------------------------------------------*
           go to     acc-tmo-orf-def-600.
       acc-tmo-orf-def-600.
      *              *-------------------------------------------------*
      *              * Lettura descrizione tipo movimento              *
      *              *-------------------------------------------------*
           move      rr-tip-mov           to   w-let-arc-yof-cod      .
           perform   let-arc-yof-000      thru let-arc-yof-999        .
      *              *-------------------------------------------------*
      *              * Bufferizzazione descrizione tipo movimento      *
      *              *-------------------------------------------------*
           move      w-let-arc-yof-des    to   rr-tip-mov-des         .
      *              *-------------------------------------------------*
      *              * Visualizzazione tipo movimento                  *
      *              *-------------------------------------------------*
           perform   vis-tmo-orf-000      thru vis-tmo-orf-999        .
      *              *-------------------------------------------------*
      *              * Visualizzazione descrizione tipo movimento      *
      *              *-------------------------------------------------*
           perform   vis-tmo-orf-des-000  thru vis-tmo-orf-des-999    .
       acc-tmo-orf-def-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     acc-tmo-orf-def-999.
       acc-tmo-orf-def-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Codice tipo movimento             *
      *    *-----------------------------------------------------------*
       vis-tmo-orf-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-tip-mov           to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tmo-orf-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : descrizione tipo movimento        *
      *    *-----------------------------------------------------------*
       vis-tmo-orf-des-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      37                   to   v-pos                  .
           move      rr-tip-mov-des       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tmo-orf-des-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo selezione : Data minima                *
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
           move      13                   to   v-lin                  .
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
           perform   fnd-arc-fit-000      thru fnd-arc-fit-999        .
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
           move      13                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-dat-min           to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       vis-dat-min-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo selezione : Data massima               *
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
           move      13                   to   v-lin                  .
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
           perform   fnd-arc-fit-000      thru fnd-arc-fit-999        .
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
           move      13                   to   v-lin                  .
           move      47                   to   v-pos                  .
           move      rr-dat-max           to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       vis-dat-max-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo selezione : Numero minimo              *
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
       acc-num-min-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "P"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      "<B"                 to   v-edm                  .
           move      14                   to   v-lin                  .
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
           if        v-key                not = "FIND"
                     go to acc-num-min-400.
           perform   fnd-arc-fit-000      thru fnd-arc-fit-999        .
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
      *    * Visualizzazione : Numero minimo                           *
      *    *-----------------------------------------------------------*
       vis-num-min-000.
           move      "DS"                 to   v-ope                  .
           move      "P"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      "<B"                 to   v-edm                  .
           move      14                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-num-min           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-num-min-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Numero massimo                       *
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
       acc-num-max-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "P"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      "<B"                 to   v-edm                  .
           move      14                   to   v-lin                  .
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
           if        v-key                not = "FIND"
                     go to acc-num-max-400.
           perform   fnd-arc-fit-000      thru fnd-arc-fit-999        .
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
      *    * Visualizzazione : Numero minimo                           *
      *    *-----------------------------------------------------------*
       vis-num-max-000.
           move      "DS"                 to   v-ope                  .
           move      "P"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      "<B"                 to   v-edm                  .
           move      14                   to   v-lin                  .
           move      47                   to   v-pos                  .
           move      rr-num-max           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-num-max-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo selezione : Codice fornitore           *
      *    *-----------------------------------------------------------*
       acc-cod-fnt-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        rr-tip-ope           not  = 01
                     go to acc-cod-fnt-999.
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      rr-cod-fnt           to   w-sav-cod-fnt          .
       acc-cod-fnt-100.
      *              *-------------------------------------------------*
      *              * Note operative                                  *
      *              *-------------------------------------------------*
           move      "NT"                 to   v-ope                  .
           move      "Tasto funzione SELEZIONE per scegliere piu' fornit
      -              "ori (fino ad un massimo di 36)"
                                          to   v-nt1                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-dcf-ope      .
           move      rr-cod-fnt           to   w-cod-mne-dcf-cod      .
           move      16                   to   w-cod-mne-dcf-lin      .
           move      30                   to   w-cod-mne-dcf-pos      .
           move      16                   to   w-cod-mne-dcf-rln      .
           move      41                   to   w-cod-mne-dcf-rps      .
           move      zero                 to   w-cod-mne-dcf-vln      .
           move      zero                 to   w-cod-mne-dcf-vps      .
           move      zero                 to   w-cod-mne-dcf-lln      .
           move      zero                 to   w-cod-mne-dcf-lps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "SLCT"               to   v-pfk (11)             .
           perform   cod-mne-dcf-cll-000  thru cod-mne-dcf-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-mne-dcf-foi-000  thru cod-mne-dcf-foi-999    .
       acc-cod-fnt-110.
           perform   cod-mne-dcf-cll-000  thru cod-mne-dcf-cll-999    .
           if        w-cod-mne-dcf-ope    =    "F+"
                     go to acc-cod-fnt-115.
           if        w-cod-mne-dcf-ope    =    "AC"
                     go to acc-cod-fnt-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-fnt-115.
           perform   cod-mne-dcf-foi-000  thru cod-mne-dcf-foi-999    .
           go to     acc-cod-fnt-110.
       acc-cod-fnt-120.
           move      w-cod-mne-dcf-cod    to   v-num                  .
       acc-cod-fnt-150.
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
                     go to acc-cod-fnt-999.
       acc-cod-fnt-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-cod-fnt             .
       acc-cod-fnt-300.
      *              *-------------------------------------------------*
      *              * Se 'Select'                                     *
      *              *-------------------------------------------------*
           if        v-key                not  = "SLCT"
                     go to acc-cod-fnt-400.
      *                  *---------------------------------------------*
      *                  * Select su codici fornitore                  *
      *                  *---------------------------------------------*
           perform   acc-fnt-esl-000      thru acc-fnt-esl-999        .
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
           move      zero                 to   rr-cod-fnt-sns         .
           move      zero                 to   w-acc-ser-edd-c01      .
       acc-cod-fnt-310.
           add       1                    to   w-acc-ser-edd-c01      .
           if        w-acc-ser-edd-c01    >    w-acc-ser-edd-max
                     go to acc-cod-fnt-320.
           if        rr-cod-fnt-eco
                    (w-acc-ser-edd-c01)   =    zero
                     go to acc-cod-fnt-320.
      *                      *-----------------------------------------*
      *                      * Attivazione segnale di selezione effet- *
      *                      * tuata                                   *
      *                      *-----------------------------------------*
           move      1                    to   rr-cod-fnt-sns         .
       acc-cod-fnt-320.
      *                  *---------------------------------------------*
      *                  * Normalizzazione singolo codice fornitore    *
      *                  *---------------------------------------------*
           move      zero                 to   rr-cod-fnt             .
           move      spaces               to   rr-cod-fnt-rag         .
           move      spaces               to   rr-cod-fnt-via         .
           move      spaces               to   rr-cod-fnt-loc         .
      *                  *---------------------------------------------*
      *                  * Preparazione visualizzazione elementi sele- *
      *                  * zionati                                     *
      *                  *---------------------------------------------*
           perform   pcs-fnt-esl-000      thru pcs-fnt-esl-999        .
      *                  *---------------------------------------------*
      *                  * Visualizzazione codici selezionati          *
      *                  *---------------------------------------------*
           perform   vcs-fnt-esl-000      thru vcs-fnt-esl-999        .
      *                  *---------------------------------------------*
      *                  * Proseguimento                               *
      *                  *---------------------------------------------*
           go to     acc-cod-fnt-600.
       acc-cod-fnt-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-cod-fnt-410.
      *                  *---------------------------------------------*
      *                  * Lettura file [fnt]                          *
      *                  *---------------------------------------------*
           move      rr-cod-fnt           to   w-let-arc-fnt-cod      .
           perform   let-arc-fnt-000      thru let-arc-fnt-999        .
      *                  *---------------------------------------------*
      *                  * Se fornitore non esistente : reimpostazione *
      *                  *---------------------------------------------*
           if        w-let-arc-fnt-flg    not  = spaces
                     go to acc-cod-fnt-100.
      *                  *---------------------------------------------*
      *                  * Lettura record [dcf]                        *
      *                  *---------------------------------------------*
           move      rr-cod-fnt           to   w-let-arc-dcf-fnt      .
           move      spaces               to   w-let-arc-dcf-dpz      .
           perform   let-arc-dcf-000      thru let-arc-dcf-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione anagrafica fornitore         *
      *                  *---------------------------------------------*
           if        rr-cod-fnt           =    zero
                     move  "Tutti               "
                                          to   rr-cod-fnt-rag
           else      move  w-let-arc-dcf-rag
                                          to   rr-cod-fnt-rag         .
           move      w-let-arc-dcf-via    to   rr-cod-fnt-via         .
           move      w-let-arc-dcf-loc    to   rr-cod-fnt-loc         .
      *                  *---------------------------------------------*
      *                  * Visualizzazione anagrafica fornitore        *
      *                  *---------------------------------------------*
           perform   vis-des-fnt-000      thru vis-des-fnt-999        .
      *                  *---------------------------------------------*
      *                  * Se fornitore esistente : oltre              *
      *                  *---------------------------------------------*
           if        w-let-arc-dcf-flg    =    spaces
                     go to acc-cod-fnt-600.
      *                  *---------------------------------------------*
      *                  * Se fornitore a zero : oltre                 *
      *                  *---------------------------------------------*
           if        rr-cod-fnt           =    zero
                     go to acc-cod-fnt-600.
       acc-cod-fnt-500.
      *                  *---------------------------------------------*
      *                  * Se anagrafica commerciale non esistente     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Bufferizzazione anagrafica fornitore    *
      *                      * contabile                               *
      *                      *-----------------------------------------*
           move      w-let-arc-fnt-rag    to   rr-cod-fnt-rag         .
           move      w-let-arc-fnt-via    to   rr-cod-fnt-via         .
           move      w-let-arc-fnt-loc    to   rr-cod-fnt-loc         .
      *                      *-----------------------------------------*
      *                      * Visualizzazione anagrafica fornitore    *
      *                      *-----------------------------------------*
           perform   vis-des-fnt-000      thru vis-des-fnt-999        .
      *                      *-----------------------------------------*
      *                      * Messaggio di errore                     *
      *                      *-----------------------------------------*
           move      "Anagrafica commerciale del fornitore non esistente
      -              " !"                 to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                      *-----------------------------------------*
      *                      * Reimpostazione                          *
      *                      *-----------------------------------------*
           go to     acc-cod-fnt-100.
       acc-cod-fnt-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore attuale come precedente : oltre   *
      *                  *---------------------------------------------*
           if        rr-cod-fnt           =    w-sav-cod-fnt
                     go to acc-cod-fnt-800.
      *                  *---------------------------------------------*
      *                  * Se e' solo stato cambiato il codice cli-    *
      *                  * ente : oltre                                *
      *                  *---------------------------------------------*
           if        rr-cod-fnt           not  = zero and
                     w-sav-cod-fnt        not  = zero
                     go to acc-cod-fnt-800.
       acc-cod-fnt-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-cod-fnt-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-cod-fnt-100.
       acc-cod-fnt-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo selezione : Anagrafica fornitore    *
      *    *-----------------------------------------------------------*
       vis-des-fnt-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      rr-cod-fnt-rag       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-des-fnt-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Serie elementi da selezionare              *
      *    *-----------------------------------------------------------*
       acc-fnt-esl-000.
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
           move      "                      Codici fornitore da selezion
      -              "are                       "
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
           perform   acc-fnt-esl-900      thru acc-fnt-esl-909        .
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
           go to     acc-fnt-esl-100.
       acc-fnt-esl-080.
      *              *-------------------------------------------------*
      *              * Visualizzazione pagina se necessario            *
      *              *-------------------------------------------------*
       acc-fnt-esl-082.
           move      w-acc-ser-edd-pev    to   w-acc-ser-edd-c0a      .
           move      w-acc-ser-edd-c0a    to   w-acc-ser-edd-c0b      .
           add       11                   to   w-acc-ser-edd-c0b      .
           if        w-acc-ser-edd-nel    <    w-acc-ser-edd-c0a or
                     w-acc-ser-edd-nel    >    w-acc-ser-edd-c0b
                     go to acc-fnt-esl-084
           else      go to acc-fnt-esl-086.
       acc-fnt-esl-084.
           move      w-acc-ser-edd-nel    to   w-acc-ser-edd-n1v      .
           subtract  1                    from w-acc-ser-edd-n1v      .
           divide    12                   into w-acc-ser-edd-n1v      .
           multiply  12                   by   w-acc-ser-edd-n1v      .
           add       1                    to   w-acc-ser-edd-n1v      .
           move      w-acc-ser-edd-n1v    to   w-acc-ser-edd-pev      .
           perform   acc-fnt-esl-900      thru acc-fnt-esl-909        .
       acc-fnt-esl-086.
           go to     acc-fnt-esl-100.
       acc-fnt-esl-100.
      *              *-------------------------------------------------*
      *              * Accettazione linea                              *
      *              *-------------------------------------------------*
       acc-fnt-esl-120.
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
           if        rr-cod-fnt-els       >    24
                     move  3              to   w-acc-ser-edd-lt2
           else if   rr-cod-fnt-els       >    12
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
       acc-fnt-esl-150.
      *                  *---------------------------------------------*
      *                  * Salvataggio valori precedenti linea         *
      *                  *---------------------------------------------*
           move      rr-cod-fnt-eco
                    (w-acc-ser-edd-nel)   to   w-acc-ser-edd-spe      .
       acc-fnt-esl-200.
      *                  *---------------------------------------------*
      *                  * Accettazione codice elemento                *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione parametri                  *
      *                      *-----------------------------------------*
           move      "AC"                 to   w-cod-mne-dcf-ope      .
           move      rr-cod-fnt-eco
                    (w-acc-ser-edd-nel)   to   w-cod-mne-dcf-cod      .
           move      "<B"                 to   v-edm                  .
      *                          *-------------------------------------*
      *                          * Coordinate di posizionamento        *
      *                          *-------------------------------------*
           move      w-acc-ser-edd-nel    to   w-acc-ser-edd-c0r      .
           subtract  w-acc-ser-edd-pev    from w-acc-ser-edd-c0r      .
           add       7                    to   w-acc-ser-edd-c0r      .
           move      w-acc-ser-edd-c0r    to   w-cod-mne-dcf-lin      .
           move      09                   to   w-cod-mne-dcf-pos      .
           move      w-cod-mne-dcf-lin    to   w-cod-mne-dcf-rln      .
           move      19                   to   w-cod-mne-dcf-rps      .
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
           if        rr-cod-fnt-eco
                    (w-acc-ser-edd-nel)   not  = zero
                     go to acc-fnt-esl-202.
           move      w-acc-ser-edd-nel    to   w-acc-ser-edd-c0a      .
           add       1                    to   w-acc-ser-edd-c0a      .
           if        w-acc-ser-edd-c0a    >    w-acc-ser-edd-max
                     go to acc-fnt-esl-204.
           if        rr-cod-fnt-eco
                    (w-acc-ser-edd-c0a)   =    zero
                     go to acc-fnt-esl-204.
       acc-fnt-esl-202.
           move      "REMV"               to   v-pfk (06)             .
       acc-fnt-esl-204.
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
                     go to acc-fnt-esl-206.
           move      w-acc-ser-edd-nel    to   w-acc-ser-edd-c0a      .
           add       1                    to   w-acc-ser-edd-c0a      .
           if        rr-cod-fnt-eco
                    (w-acc-ser-edd-nel)   =    zero and
                     rr-cod-fnt-eco
                    (w-acc-ser-edd-c0a)   =    zero
                     go to acc-fnt-esl-206.
      *                          *-------------------------------------*
      *                          * Tasto 'Tab'                         *
      *                          *-------------------------------------*
           move      "TAB "               to   v-pfk (10)             .
       acc-fnt-esl-206.
      *                      *-----------------------------------------*
      *                      * Accettazione                            *
      *                      *-----------------------------------------*
           perform   cod-mne-dcf-cll-000  thru cod-mne-dcf-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-mne-dcf-foi-000  thru cod-mne-dcf-foi-999    .
       acc-fnt-esl-208.
           perform   cod-mne-dcf-cll-000  thru cod-mne-dcf-cll-999    .
           if        w-cod-mne-dcf-ope    =    "F+"
                     go to acc-fnt-esl-210.
           if        w-cod-mne-dcf-ope    =    "AC"
                     go to acc-fnt-esl-212.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-fnt-esl-210.
           perform   cod-mne-dcf-foi-000  thru cod-mne-dcf-foi-999    .
           go to     acc-fnt-esl-208.
       acc-fnt-esl-212.
           move      w-cod-mne-dcf-cod    to   v-num                  .
       acc-fnt-esl-215.
      *                      *-----------------------------------------*
      *                      * Valore in campo di destinazione         *
      *                      *-----------------------------------------*
           move      v-num                to   rr-cod-fnt-eco
                                              (w-acc-ser-edd-nel)     .
      *                      *-----------------------------------------*
      *                      * Se Exit                                 *
      *                      *-----------------------------------------*
           if        v-key                =    "EXIT"
                     move  spaces         to   v-key
                     go to acc-fnt-esl-800.
      *                      *-----------------------------------------*
      *                      * Se Return                               *
      *                      *-----------------------------------------*
           if        v-key                =    spaces
                     go to acc-fnt-esl-425.
       acc-fnt-esl-220.
      *                      *-----------------------------------------*
      *                      * Se Up                                   *
      *                      *-----------------------------------------*
           if        v-key                not  = "UP  "
                     go to acc-fnt-esl-225.
      *                          *-------------------------------------*
      *                          * Compattamento se necessario         *
      *                          *-------------------------------------*
           move      w-acc-ser-edd-nel    to   w-acc-ser-edd-nec      .
           perform   acc-fnt-esl-920      thru acc-fnt-esl-929        .
      *                          *-------------------------------------*
      *                          * Se sul primo : uscita               *
      *                          *-------------------------------------*
           subtract  1                    from w-acc-ser-edd-nel      .
           if        w-acc-ser-edd-nel    =    zero
                     move  "UP  "         to   v-key
                     go to acc-fnt-esl-800
           else      go to acc-fnt-esl-080.
       acc-fnt-esl-225.
      *                      *-----------------------------------------*
      *                      * Se Down                                 *
      *                      *-----------------------------------------*
           if        v-key                not  = "DOWN"
                     go to acc-fnt-esl-250.
      *                          *-------------------------------------*
      *                          * Compattamento se necessario         *
      *                          *-------------------------------------*
           move      w-acc-ser-edd-nel    to   w-acc-ser-edd-nec      .
           perform   acc-fnt-esl-920      thru acc-fnt-esl-929        .
      *                          *-------------------------------------*
      *                          * Se sull'ultimo : uscita             *
      *                          *-------------------------------------*
           if        w-acc-ser-edd-fce    =    spaces
                     add   1              to   w-acc-ser-edd-nel      .
           if        w-acc-ser-edd-nel    >    w-acc-ser-edd-max
                     move  "DOWN"         to   v-key
                     go to acc-fnt-esl-800.
           if        w-acc-ser-edd-nel    =    1
                     go to acc-fnt-esl-230
           else      go to acc-fnt-esl-235.
       acc-fnt-esl-230.
           if        rr-cod-fnt-eco (1)    =    zero and
                     rr-cod-fnt-eco (2)    =    zero
                     move  "DOWN"         to   v-key
                     go to acc-fnt-esl-800
           else      go to acc-fnt-esl-080.
       acc-fnt-esl-235.
           move      w-acc-ser-edd-nel    to   w-acc-ser-edd-c0a      .
           move      w-acc-ser-edd-c0a    to   w-acc-ser-edd-c0b      .
           subtract  1                    from w-acc-ser-edd-c0b      .
           if        rr-cod-fnt-eco
                    (w-acc-ser-edd-c0a)   =    zero and
                     rr-cod-fnt-eco
                    (w-acc-ser-edd-c0b)   =    zero
                     move  "DOWN"         to   v-key
                     go to acc-fnt-esl-800
           else      go to acc-fnt-esl-080.
       acc-fnt-esl-250.
      *                      *-----------------------------------------*
      *                      * Se Insr                                 *
      *                      *-----------------------------------------*
           if        v-key                not  = "INSR"
                     go to acc-fnt-esl-275.
           move      w-acc-ser-edd-nel    to   w-acc-ser-edd-nec      .
           perform   acc-fnt-esl-940      thru acc-fnt-esl-949        .
           go to     acc-fnt-esl-080.
       acc-fnt-esl-275.
      *                      *-----------------------------------------*
      *                      * Se Do                                   *
      *                      *-----------------------------------------*
           if        v-key                not  = "DO  "
                     go to acc-fnt-esl-300.
      *                          *-------------------------------------*
      *                          * Controlli per tasto Do              *
      *                          *-------------------------------------*
           perform   tdo-ric-sel-000      thru tdo-ric-sel-999        .
           if        w-cnt-tdo-ric-flg    =    spaces
                     go to acc-fnt-esl-280
           else      move  spaces         to   w-cnt-tdo-ric-flg
                     go to acc-fnt-esl-200.
       acc-fnt-esl-280.
      *                          *-------------------------------------*
      *                          * Compattamento se necessario         *
      *                          *-------------------------------------*
           move      w-acc-ser-edd-nel    to   w-acc-ser-edd-nec      .
           perform   acc-fnt-esl-920      thru acc-fnt-esl-929        .
      *                          *-------------------------------------*
      *                          * Uscita                              *
      *                          *-------------------------------------*
           move      "S"                  to   w-cnt-acc-ric-sel      .
           go to     acc-fnt-esl-800.
       acc-fnt-esl-300.
      *                      *-----------------------------------------*
      *                      * Se Remv                                 *
      *                      *-----------------------------------------*
           if        v-key                not  = "REMV"
                     go to acc-fnt-esl-325.
      *                          *-------------------------------------*
      *                          * Compattamento in ogni caso          *
      *                          *-------------------------------------*
           move      w-acc-ser-edd-nel    to   w-acc-ser-edd-nec      .
           perform   acc-fnt-esl-930      thru acc-fnt-esl-939        .
      *                          *-------------------------------------*
      *                          * A reimpostare                       *
      *                          *-------------------------------------*
           go to     acc-fnt-esl-080.
       acc-fnt-esl-325.
      *                      *-----------------------------------------*
      *                      * Se Prsc                                 *
      *                      *-----------------------------------------*
           if        v-key                not  = "PRSC"
                     go to acc-fnt-esl-350.
      *                          *-------------------------------------*
      *                          * Compattamento se necessario         *
      *                          *-------------------------------------*
           move      w-acc-ser-edd-nel    to   w-acc-ser-edd-nec      .
           perform   acc-fnt-esl-920      thru acc-fnt-esl-929        .
      *                          *-------------------------------------*
      *                          * Se su prima facciata : uscita       *
      *                          *-------------------------------------*
           if        w-acc-ser-edd-nel    not  > 14
                     move  "UP  "         to   v-key
                     go to acc-fnt-esl-800.
      *                          *-------------------------------------*
      *                          * Al primo elemento della facciata    *
      *                          * precedente                          *
      *                          *-------------------------------------*
           subtract  1                    from w-acc-ser-edd-nel      .
           divide    14                   into w-acc-ser-edd-nel      .
           subtract  1                    from w-acc-ser-edd-nel      .
           multiply  14                   by   w-acc-ser-edd-nel      .
           add       1                    to   w-acc-ser-edd-nel      .
           go to     acc-fnt-esl-080.
       acc-fnt-esl-350.
      *                      *-----------------------------------------*
      *                      * Se Nxsc                                 *
      *                      *-----------------------------------------*
           if        v-key                not  = "NXSC"
                     go to acc-fnt-esl-375.
      *                          *-------------------------------------*
      *                          * Compattamento se necessario         *
      *                          *-------------------------------------*
           move      w-acc-ser-edd-nel    to   w-acc-ser-edd-nec      .
           perform   acc-fnt-esl-920      thru acc-fnt-esl-929        .
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
                     go to acc-fnt-esl-800.
           if        rr-cod-fnt-eco
                    (w-acc-ser-edd-c0a)   =    zero
                     move  "DOWN"         to   v-key
                     go to acc-fnt-esl-800.
      *                          *-------------------------------------*
      *                          * Al primo elemento della facciata    *
      *                          * successiva                          *
      *                          *-------------------------------------*
           move      w-acc-ser-edd-c0a    to   w-acc-ser-edd-nel      .
           go to     acc-fnt-esl-080.
       acc-fnt-esl-375.
      *                      *-----------------------------------------*
      *                      * Se Back                                 *
      *                      *-----------------------------------------*
           if        v-key                not  = "BACK"
                     go to acc-fnt-esl-400.
      *                          *-------------------------------------*
      *                          * Compattamento se necessario         *
      *                          *-------------------------------------*
           move      w-acc-ser-edd-nel    to   w-acc-ser-edd-nec      .
           perform   acc-fnt-esl-920      thru acc-fnt-esl-929        .
      *                          *-------------------------------------*
      *                          * Al primo elemento                   *
      *                          *-------------------------------------*
           move      1                    to   w-acc-ser-edd-nel      .
           go to     acc-fnt-esl-080.
       acc-fnt-esl-400.
      *                      *-----------------------------------------*
      *                      * Se Tab                                  *
      *                      *-----------------------------------------*
           if        v-key                not  = "TAB "
                     go to acc-fnt-esl-425.
      *                          *-------------------------------------*
      *                          * Compattamento se necessario         *
      *                          *-------------------------------------*
           move      w-acc-ser-edd-nel    to   w-acc-ser-edd-nec      .
           perform   acc-fnt-esl-920      thru acc-fnt-esl-929        .
      *                          *-------------------------------------*
      *                          * Dopo l'ultimo elemento inserito     *
      *                          *-------------------------------------*
           if        rr-cod-fnt-eco
                    (w-acc-ser-edd-max)   not  = zero
                     move  w-acc-ser-edd-max
                                          to   w-acc-ser-edd-nel
                     go to acc-fnt-esl-080.
           move      w-acc-ser-edd-max    to   w-acc-ser-edd-nel      .
       acc-fnt-esl-405.
           if        w-acc-ser-edd-nel    =    zero
                     go to acc-fnt-esl-410.
           if        rr-cod-fnt-eco
                    (w-acc-ser-edd-nel)   =    zero
                     subtract  1          from w-acc-ser-edd-nel
                     go to     acc-fnt-esl-405.
       acc-fnt-esl-410.
           add       1                    to   w-acc-ser-edd-nel      .
           go to     acc-fnt-esl-080.
       acc-fnt-esl-425.
      *                      *-----------------------------------------*
      *                      * Se Return                               *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Lettura archivio [dcf]              *
      *                          *-------------------------------------*
           move      rr-cod-fnt-eco
                    (w-acc-ser-edd-nel)   to   w-let-arc-dcf-fnt      .
           move      spaces               to   w-let-arc-dcf-dpz      .
           perform   let-arc-dcf-000      thru let-arc-dcf-999        .
      *                          *-------------------------------------*
      *                          * Trattamento descrizione             *
      *                          *-------------------------------------*
           move      w-let-arc-dcf-rag    to   rr-cod-fnt-ers
                                              (w-acc-ser-edd-nel)     .
      *                          *-------------------------------------*
      *                          * Visualizzazione                     *
      *                          *-------------------------------------*
           move      w-acc-ser-edd-nel    to   w-acc-ser-edd-nev      .
           move      w-acc-ser-edd-nel    to   w-acc-ser-edd-c0r      .
           subtract  w-acc-ser-edd-pev    from w-acc-ser-edd-c0r      .
           add       7                    to   w-acc-ser-edd-c0r      .
           perform   acc-fnt-esl-910      thru acc-fnt-esl-919        .
      *                          *-------------------------------------*
      *                          * Se record non trovato : reimposta-  *
      *                          * zione                               *
      *                          *-------------------------------------*
           if        w-let-arc-dcf-flg    not  = spaces
                     go to acc-fnt-esl-200.
      *                          *-------------------------------------*
      *                          * Controllo valore impostato          *
      *                          *-------------------------------------*
           if        rr-cod-fnt-eco
                    (w-acc-ser-edd-nel)   =    zero
                     go to acc-fnt-esl-450
           else      go to acc-fnt-esl-500.
       acc-fnt-esl-450.
      *                          *-------------------------------------*
      *                          * Se impostato zero                   *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Se il valore precedente era a   *
      *                              * zero : come per Down            *
      *                              *---------------------------------*
           if        w-acc-ser-edd-spe    =    zero
                     move  "DOWN"         to   v-key
                     go to acc-fnt-esl-225.
      *                              *---------------------------------*
      *                              * Altrimenti come per Remv        *
      *                              *---------------------------------*
           move      "REMV"               to   v-key                  .
           go to     acc-fnt-esl-300.
       acc-fnt-esl-500.
      *                  *---------------------------------------------*
      *                  * Passaggio ad elemento successivo            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Visualizzazione elementi selezionati    *
      *                      *-----------------------------------------*
           perform   acc-fnt-esl-960      thru acc-fnt-esl-969        .
      *                      *-----------------------------------------*
      *                      * Incremento numero elemento              *
      *                      *-----------------------------------------*
           add       1                    to   w-acc-ser-edd-nel      .
      *                      *-----------------------------------------*
      *                      * Se fine elementi : uscita               *
      *                      *-----------------------------------------*
           if        w-acc-ser-edd-nel    >    w-acc-ser-edd-max
                     move  spaces         to   v-key
                     go to acc-fnt-esl-800.
      *                      *-----------------------------------------*
      *                      * Altrimenti : ad impostazione linea      *
      *                      *-----------------------------------------*
           go to     acc-fnt-esl-080.
       acc-fnt-esl-800.
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
           go to     acc-fnt-esl-999.
       acc-fnt-esl-890.
      *              *-------------------------------------------------*
      *              * Subroutines interne                             *
      *              *-------------------------------------------------*
       acc-fnt-esl-900.
      *                  *---------------------------------------------*
      *                  * Visualizzazione pagina a partire dall'ele-  *
      *                  * mento numero w-acc-ser-edd-n1v              *
      *                  *---------------------------------------------*
           move      w-acc-ser-edd-n1v    to   w-acc-ser-edd-c0p      .
           subtract  1                    from w-acc-ser-edd-c0p      .
           move      w-acc-ser-edd-c0p    to   w-acc-ser-edd-c0q      .
           add       12                   to   w-acc-ser-edd-c0q      .
       acc-fnt-esl-901.
           add       1                    to   w-acc-ser-edd-c0p      .
           if        w-acc-ser-edd-c0p    >    w-acc-ser-edd-max
                     go to acc-fnt-esl-905.
           if        w-acc-ser-edd-c0p    >    w-acc-ser-edd-c0q
                     go to acc-fnt-esl-909.
           move      w-acc-ser-edd-c0p    to   w-acc-ser-edd-nev      .
           move      w-acc-ser-edd-nev    to   w-acc-ser-edd-c0r      .
       acc-fnt-esl-903.
           if        w-acc-ser-edd-c0r    >    12
                     subtract  12         from w-acc-ser-edd-c0r
                     go to     acc-fnt-esl-903.
           add       6                    to   w-acc-ser-edd-c0r      .
           perform   acc-fnt-esl-910      thru acc-fnt-esl-919        .
           go to     acc-fnt-esl-901.
       acc-fnt-esl-905.
           if        w-acc-ser-edd-c0p    >    w-acc-ser-edd-c0q
                     go to acc-fnt-esl-909.
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
           go to     acc-fnt-esl-905.
       acc-fnt-esl-909.
           exit.
       acc-fnt-esl-910.
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
      *                          * Editing codice fornitore            *
      *                          *-------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      rr-cod-fnt-eco
                    (w-acc-ser-edd-nev)   to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-acc-ser-edd-cod      .
      *                          *-------------------------------------*
      *                          * Ragione sociale                     *
      *                          *-------------------------------------*
           move      rr-cod-fnt-ers
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
       acc-fnt-esl-919.
           exit.
       acc-fnt-esl-920.
      *                  *---------------------------------------------*
      *                  * Compattamento dell' elemento indirizzato da *
      *                  * w-acc-ser-edd-nec se necessario             *
      *                  *---------------------------------------------*
           move      spaces               to   w-acc-ser-edd-fce      .
           if        rr-cod-fnt-eco
                    (w-acc-ser-edd-nec)   not  = zero
                     go to acc-fnt-esl-929.
           move      w-acc-ser-edd-nec    to   w-acc-ser-edd-c0a      .
           move      w-acc-ser-edd-c0a    to   w-acc-ser-edd-c0b      .
           add       1                    to   w-acc-ser-edd-c0b      .
           if        w-acc-ser-edd-c0b    >    w-acc-ser-edd-max
                     go to acc-fnt-esl-929.
           if        rr-cod-fnt-eco
                    (w-acc-ser-edd-c0b)   =    zero
                     go to acc-fnt-esl-929.
           perform   acc-fnt-esl-930      thru acc-fnt-esl-939        .
           move      "#"                  to   w-acc-ser-edd-fce      .
       acc-fnt-esl-929.
           exit.
       acc-fnt-esl-930.
      *                  *---------------------------------------------*
      *                  * Compattamento dell' elemento indirizzato da *
      *                  * w-acc-ser-edd-nec in ogni caso              *
      *                  *---------------------------------------------*
           move      w-acc-ser-edd-nec    to   w-acc-ser-edd-c0a      .
           move      w-acc-ser-edd-c0a    to   w-acc-ser-edd-c0b      .
           add       1                    to   w-acc-ser-edd-c0b      .
       acc-fnt-esl-931.
           if        w-acc-ser-edd-c0b    >    w-acc-ser-edd-max
                     go to acc-fnt-esl-932.
           move      rr-cod-fnt-ele
                    (w-acc-ser-edd-c0b)   to   rr-cod-fnt-ele
                                              (w-acc-ser-edd-c0a)     .
           if        rr-cod-fnt-eco
                    (w-acc-ser-edd-c0b)   =    zero
                     go to acc-fnt-esl-933.
           add       1                    to   w-acc-ser-edd-c0a      .
           add       1                    to   w-acc-ser-edd-c0b      .
           go to     acc-fnt-esl-931.
       acc-fnt-esl-932.
           move      zero                 to   rr-cod-fnt-eco
                                              (w-acc-ser-edd-c0a)     .
           move      spaces               to   rr-cod-fnt-ers
                                              (w-acc-ser-edd-c0a)     .
       acc-fnt-esl-933.
           move      w-acc-ser-edd-nec    to   w-acc-ser-edd-c0a      .
       acc-fnt-esl-934.
           if        w-acc-ser-edd-c0a    >    12
                     subtract  12         from w-acc-ser-edd-c0a
                     go to     acc-fnt-esl-934.
           move      w-acc-ser-edd-c0a    to   w-acc-ser-edd-c0b      .
           add       6                    to   w-acc-ser-edd-c0b      .
           move      w-acc-ser-edd-c0b    to   w-acc-ser-edd-c0c      .
           add       1                    to   w-acc-ser-edd-c0c      .
       acc-fnt-esl-935.
           if        w-acc-ser-edd-c0a    =    12
                     go to acc-fnt-esl-936.
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
           go to     acc-fnt-esl-935.
       acc-fnt-esl-936.
           move      w-acc-ser-edd-nec    to   w-acc-ser-edd-c0a      .
       acc-fnt-esl-937.
           if        w-acc-ser-edd-c0a    >    12
                     subtract  12         from w-acc-ser-edd-c0a
                     go to     acc-fnt-esl-937.
           subtract  w-acc-ser-edd-c0a    from 12
                                        giving w-acc-ser-edd-c0b      .
           add       w-acc-ser-edd-nec
                     w-acc-ser-edd-c0b  giving w-acc-ser-edd-c0c      .
           if        w-acc-ser-edd-c0c    >    w-acc-ser-edd-max
                     go to acc-fnt-esl-938.
           if        rr-cod-fnt-eco
                    (w-acc-ser-edd-c0c)   =    zero
                     go to acc-fnt-esl-938.
           move      w-acc-ser-edd-c0c    to   w-acc-ser-edd-nev      .
           move      18                   to   w-acc-ser-edd-c0r      .
           perform   acc-fnt-esl-910      thru acc-fnt-esl-919        .
           go to     acc-fnt-esl-939.
       acc-fnt-esl-938.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      70                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      09                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-fnt-esl-939.
           exit.
       acc-fnt-esl-940.
      *                  *---------------------------------------------*
      *                  * Inserimento dell' elemento indirizzato da   *
      *                  * w-acc-ser-edd-nec                           *
      *                  *---------------------------------------------*
           move      w-acc-ser-edd-max    to   w-acc-ser-edd-c0b      .
           move      w-acc-ser-edd-c0b    to   w-acc-ser-edd-c0a      .
           subtract  1                    from w-acc-ser-edd-c0a      .
       acc-fnt-esl-941.
           move      rr-cod-fnt-ele
                    (w-acc-ser-edd-c0a)   to   rr-cod-fnt-ele
                                              (w-acc-ser-edd-c0b)     .
           if        w-acc-ser-edd-c0a    =    w-acc-ser-edd-nec
                     go to acc-fnt-esl-942.
           subtract  1                    from w-acc-ser-edd-c0a      .
           subtract  1                    from w-acc-ser-edd-c0b      .
           go to     acc-fnt-esl-941.
       acc-fnt-esl-942.
           move      zero                 to   rr-cod-fnt-eco
                                              (w-acc-ser-edd-c0a)     .
           move      spaces               to   rr-cod-fnt-ers
                                              (w-acc-ser-edd-c0a)     .
           move      w-acc-ser-edd-nec    to   w-acc-ser-edd-c0a      .
       acc-fnt-esl-943.
           if        w-acc-ser-edd-c0a    >    12
                     subtract  12         from w-acc-ser-edd-c0a
                     go to     acc-fnt-esl-943.
           add       6                    to   w-acc-ser-edd-c0a      .
           if        w-acc-ser-edd-c0a    =    18
                     go to acc-fnt-esl-945.
           move      17                   to   w-acc-ser-edd-c0b      .
           move      18                   to   w-acc-ser-edd-c0c      .
       acc-fnt-esl-944.
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
                     go to acc-fnt-esl-945.
           subtract  1                    from w-acc-ser-edd-c0b      .
           subtract  1                    from w-acc-ser-edd-c0c      .
           go to     acc-fnt-esl-944.
       acc-fnt-esl-945.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      70                   to   v-car                  .
           move      w-acc-ser-edd-c0a    to   v-lin                  .
           move      09                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-fnt-esl-949.
           exit.
       acc-fnt-esl-960.
      *                  *---------------------------------------------*
      *                  * Visualizzazione elementi da trattare        *
      *                  *---------------------------------------------*
           move      zero                 to   w-acc-ser-edd-c0a      .
       acc-fnt-esl-962.
           add       1                    to   w-acc-ser-edd-c0a      .
           if        w-acc-ser-edd-c0a    >    w-acc-ser-edd-max
                     go to acc-fnt-esl-964.
           if        rr-cod-fnt-eco
                    (w-acc-ser-edd-c0a)   =    zero
                     go to acc-fnt-esl-964.
           go to     acc-fnt-esl-962.
       acc-fnt-esl-964.
           subtract  1                    from w-acc-ser-edd-c0a      .
           move      w-acc-ser-edd-c0a    to   rr-cod-fnt-els         .
       acc-fnt-esl-969.
           exit.
       acc-fnt-esl-999.
           exit.

      *    *===========================================================*
      *    * Preparazione codici selezionati                           *
      *    *-----------------------------------------------------------*
       pcs-fnt-esl-000.
      *              *-------------------------------------------------*
      *              * Preparazione di un campo text di massimo 3 li-  *
      *              * nee da 40 caratteri ciascuna                    *
      *              *-------------------------------------------------*
           move      spaces               to   w-acc-ser-edd-txt      .
      *              *-------------------------------------------------*
      *              * Test se da eseguire                             *
      *              *-------------------------------------------------*
           if        rr-cod-fnt-sns       =    zero
                     go to pcs-fnt-esl-900.
           move      zero                 to   w-acc-ser-edd-c01      .
       pcs-fnt-esl-100.
      *              *-------------------------------------------------*
      *              * Ciclo di preparazione                           *
      *              *-------------------------------------------------*
           if        w-acc-ser-edd-txt
                    (119:1)               not  = spaces
                     go to pcs-fnt-esl-900.
           add       1                    to   w-acc-ser-edd-c01      .
           if        w-acc-ser-edd-c01    >    w-acc-ser-edd-max
                     go to pcs-fnt-esl-900.
           if        rr-cod-fnt-eco
                    (w-acc-ser-edd-c01)   =    zero
                     go to pcs-fnt-esl-900.
      *              *-------------------------------------------------*
      *              * Editing codice elemento                         *
      *              *-------------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      rr-cod-fnt-eco
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
       pcs-fnt-esl-800.
      *              *-------------------------------------------------*
      *              * Riciclo                                         *
      *              *-------------------------------------------------*
           go to     pcs-fnt-esl-100.
       pcs-fnt-esl-900.
      *              *-------------------------------------------------*
      *              * Valore in uscita                                *
      *              *-------------------------------------------------*
           if        w-acc-ser-edd-txt    =    spaces
                     go to pcs-fnt-esl-999.
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
           go to     pcs-fnt-esl-999.
       pcs-fnt-esl-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione codici selezionati                        *
      *    *-----------------------------------------------------------*
       vcs-fnt-esl-000.
      *              *-------------------------------------------------*
      *              * Pulizia della prima riga e segnale di selezione *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      10                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      30                   to   v-pos                  .
           if        w-acc-ser-edd-txt    =    spaces
                     move  "Tutti     "   to   v-alf
           else      move  "+         "   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vcs-fnt-esl-100.
      *              *-------------------------------------------------*
      *              * Preparazione contatore                          *
      *              *-------------------------------------------------*
           move      zero                 to   w-acc-ser-edd-c01      .
       vcs-fnt-esl-200.
      *              *-------------------------------------------------*
      *              * Ciclo di preparazione                           *
      *              *-------------------------------------------------*
           add       1                    to   w-acc-ser-edd-c01      .
           if        w-acc-ser-edd-c01    >    1
                     go to vcs-fnt-esl-900.
       vcs-fnt-esl-300.
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
       vcs-fnt-esl-400.
      *              *-------------------------------------------------*
      *              * Riciclo                                         *
      *              *-------------------------------------------------*
           go to     vcs-fnt-esl-200.
       vcs-fnt-esl-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     vcs-fnt-esl-999.
       vcs-fnt-esl-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Esclusione documenti senza mail      *
      *    *-----------------------------------------------------------*
       acc-sne-eml-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        rr-tip-ope           not  = 01
                     go to acc-sne-eml-999.
       acc-sne-eml-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-sne-eml-lun    to   v-car                  .
           move      w-exp-sne-eml-num    to   v-ldt                  .
           move      "SN#"                to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      18                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-sne-eml-tbl    to   v-txt                  .
           move      rr-sne-eml           to   v-num                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-sne-eml-999.
       acc-sne-eml-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-sne-eml             .
       acc-sne-eml-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se zero : reimpostazione                    *
      *                  *---------------------------------------------*
           if        rr-sne-eml           not  = zero
                     go to acc-sne-eml-600.
           if        v-key                =    "UP  "
                     go to acc-sne-eml-600
           else      go to acc-sne-eml-100.
       acc-sne-eml-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-sne-eml-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-sne-eml-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-sne-eml-100.
       acc-sne-eml-999.
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
      *    * Accettazione campo : Documenti da verificare              *
      *    *-----------------------------------------------------------*
       acc-snx-oav-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se personalizzazione lo prevede        *
      *                  *---------------------------------------------*
           if        w-prs-snx-oav        not  = "S"
                     go to acc-snx-oav-999.
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        rr-tip-ope           not  = 01
                     go to acc-snx-oav-999.
       acc-snx-oav-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-snx-oav-lun    to   v-car                  .
           move      w-exp-snx-oav-num    to   v-ldt                  .
           move      "SN#"                to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      20                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-snx-oav-tbl    to   v-txt                  .
           move      rr-snx-oav           to   v-num                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-snx-oav-999.
       acc-snx-oav-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-snx-oav             .
       acc-snx-oav-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se zero : reimpostazione                    *
      *                  *---------------------------------------------*
           if        rr-snx-oav           not  = zero
                     go to acc-snx-oav-600.
           if        v-key                =    "UP  "
                     go to acc-snx-oav-600
           else      go to acc-snx-oav-100.
       acc-snx-oav-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-snx-oav-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-snx-oav-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-snx-oav-100.
       acc-snx-oav-999.
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
      *              * Test su numero ordine minimo e massimo          *
      *              *-------------------------------------------------*
           if        rr-num-min           =    zero or
                     rr-num-max           =    zero
                     go to tdo-ric-sel-200.
           if        rr-num-min           not  > rr-num-max
                     go to tdo-ric-sel-200.
           move      "Numero ordine iniziale maggiore di quella finale !
      -              "               "    to   w-err-box-err-msg      .
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
      *              * Data ordine massima                             *
      *              *-------------------------------------------------*
           if        rr-dat-max           not  = zero
                     go to reg-ric-sel-100.
           if        rr-dat-min           =    zero
                     move  9999999        to   rr-dat-max
           else      move  rr-dat-min     to   rr-dat-max             .
       reg-ric-sel-100.
      *              *-------------------------------------------------*
      *              * Numero ordine massimo                           *
      *              *-------------------------------------------------*
           if        rr-num-max           not  = zero
                     go to reg-ric-sel-200.
           if        rr-num-min           =    zero
                     move  999999999      to   rr-num-max
           else      move  rr-num-min     to   rr-num-max             .
       reg-ric-sel-200.
      *              *-------------------------------------------------*
      *              * Regolarizzazione numero ordine interno minimo   *
      *              *-------------------------------------------------*
           move      rr-num-min           to   w-wrk-nft-acc          .
           move      w-wrk-num-saa        to   w-wrk-noc-saa          .
           move      rr-dpz-inu           to   w-wrk-noc-dpz          .
           move      w-wrk-num-npg        to   w-wrk-noc-npg          .
           move      w-wrk-nft-int        to   rr-noc-min             .
      *              *-------------------------------------------------*
      *              * Regolarizzazione numero ordine interno massimo  *
      *              *-------------------------------------------------*
           move      rr-num-max           to   w-wrk-nft-acc          .
           move      w-wrk-num-saa        to   w-wrk-noc-saa          .
           move      rr-dpz-inu           to   w-wrk-noc-dpz          .
           move      w-wrk-num-npg        to   w-wrk-noc-npg          .
           move      w-wrk-nft-int        to   rr-noc-max             .
      *              *-------------------------------------------------*
      *              * Regolarizzazione codice fornitore singolo o     *
      *              * multiplo                                        *
      *              *-------------------------------------------------*
           if        rr-cod-fnt-sns       not  = zero
           move      zero                 to   rr-cod-fnt             .
           move      spaces               to   rr-cod-fnt-rag         .
           move      spaces               to   rr-cod-fnt-via         .
           move      spaces               to   rr-cod-fnt-loc         .
                     go to reg-ric-sel-300.
           move      zero                 to   rr-cod-fnt-els         .
       reg-ric-sel-250.
           add       1                    to   rr-cod-fnt-els         .
           if        rr-cod-fnt-els       >    36
                     go to reg-ric-sel-260.
           move      zero                 to   rr-cod-fnt-eco
                                              (rr-cod-fnt-els)        .
           move      spaces               to   rr-cod-fnt-ers
                                              (rr-cod-fnt-els)        .
           go to     reg-ric-sel-250.
       reg-ric-sel-260.
           move      zero                 to   rr-cod-fnt-els         .
       reg-ric-sel-300.
      *              *-------------------------------------------------*
      *              * Eventuale ritaratura segnale di si/no stampa    *
      *              *-------------------------------------------------*
           if        rr-tip-ope           =    02
                     move  "S"            to   w-cnt-fun-snx-stp
           else      move  "N"            to   w-cnt-fun-snx-stp      .
      *              *-------------------------------------------------*
      *              * Esclusione documenti senza mail                 *
      *              *-------------------------------------------------*
           if        rr-sne-eml           =    zero
                     move  01             to   rr-sne-eml             .
      *              *-------------------------------------------------*
      *              * Esclusione documenti                            *
      *              *-------------------------------------------------*
           if        rr-sne-doc           =    zero
                     move  01             to   rr-sne-doc             .
      *              *-------------------------------------------------*
      *              * Documenti da verificare                         *
      *              *-------------------------------------------------*
           if        rr-snx-oav           =    zero
                     move  01             to   rr-snx-oav             .
       reg-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione richieste di selezione                    *
      *    *-----------------------------------------------------------*
       nor-ric-sel-000.
           perform   nor-ric-sel-fnt-000  thru nor-ric-sel-fnt-999    .
           move      zero                 to   rr-tip-ope             .
           move      zero                 to   rr-num-dst             .
           move      spaces               to   rr-tip-mov             .
           move      spaces               to   rr-tip-mov-des         .
           move      zero                 to   rr-tip-mov-vld         .
           move      zero                 to   rr-tip-mov-dpz         .
      *
           move      zero                 to   rr-dat-min             .
           move      zero                 to   rr-dat-max             .
      *
           move      zero                 to   rr-num-min             .
           move      zero                 to   rr-num-max             .
      *
           move      zero                 to   rr-noc-min             .
           move      zero                 to   rr-noc-max             .
      *
           move      zero                 to   rr-sne-eml             .
           move      zero                 to   rr-sne-doc             .
           move      zero                 to   rr-snx-oav             .
       nor-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione richieste di selezione                    *
      *    *                                                           *
      *    * Normalizzazione specifica per codici fornitore            *
      *    *-----------------------------------------------------------*
       nor-ric-sel-fnt-000.
           move      zero                 to   rr-cod-fnt             .
           move      spaces               to   rr-cod-fnt-rag         .
           move      spaces               to   rr-cod-fnt-via         .
           move      spaces               to   rr-cod-fnt-loc         .
           move      zero                 to   rr-cod-fnt-sns         .
           move      zero                 to   rr-cod-fnt-els         .
       nor-ric-sel-fnt-100.
           add       1                    to   rr-cod-fnt-els         .
           if        rr-cod-fnt-els       >    36
                     go to nor-ric-sel-fnt-200.
           move      zero                 to   rr-cod-fnt-eco
                                              (rr-cod-fnt-els)        .
           move      spaces               to   rr-cod-fnt-ers
                                              (rr-cod-fnt-els)        .
           go to     nor-ric-sel-fnt-100.
       nor-ric-sel-fnt-200.
           move      zero                 to   rr-cod-fnt-els         .
           go to     nor-ric-sel-fnt-999.
       nor-ric-sel-fnt-999.
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
           move      096                  to   w-cnt-stp-amp-lin      .
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
      *    * Find su archivio [ofe]                                    *
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
           move      "porf5410"           to   s-pro                  .
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
           move      "pgm/orf/prg/obj/porf5410"
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
      *    * Routine di lettura archivio [ofe]                         *
      *    *-----------------------------------------------------------*
       let-arc-dst-000.
      *              *-------------------------------------------------*
      *              * Preparazione flag di uscita                     *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-dst-flg      .
       let-arc-dst-100.
      *              *-------------------------------------------------*
      *              * Start su file [ofe]                             *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "NUMDST    "         to   f-key                  .
           move      w-let-arc-dst-prt    to   rf-ofe-num-dst         .
           move      "pgm/orf/fls/ioc/obj/iofofe"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ofe                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata : ad uscita                 *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to let-arc-dst-900.
       let-arc-dst-200.
      *              *-------------------------------------------------*
      *              * Next su [ofe]                                   *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/orf/fls/ioc/obj/iofofe"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ofe                 .
      *                  *---------------------------------------------*
      *                  * Se 'at end' : ad uscita                     *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to let-arc-dst-900.
       let-arc-dst-300.
      *              *-------------------------------------------------*
      *              * Max su [ofe], se non superato : ad uscita       *
      *              *-------------------------------------------------*
           if        rf-ofe-num-dst       not  = w-let-arc-dst-prt
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
      *    * Tasto funzione "FIND" su archivio ordini                  *
      *    *-----------------------------------------------------------*
       fnd-arc-fit-000.
      *              *-------------------------------------------------*
      *              * Test se programma di interrogazione gia' attivo *
      *              *-------------------------------------------------*
           move      "P?"                 to   s-ope                  .
           move      "porf3010"           to   s-pro                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-liv                not  = zero
                     go to  fnd-arc-fit-999.
      *              *-------------------------------------------------*
      *              * Richiamo programma di interrogazione            *
      *              *-------------------------------------------------*
           move      "pgm/orf/prg/obj/porf3010"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat                                            .
           cancel    s-pat                                            .
       fnd-arc-fit-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [yof]                         *
      *    *-----------------------------------------------------------*
       let-arc-yof-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-yof-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice a spazi                          *
      *              *-------------------------------------------------*
           if        w-let-arc-yof-cod    =    spaces
                     go to let-arc-yof-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODTOF"             to   f-key                  .
           move      w-let-arc-yof-cod    to   rf-yof-cod-tof         .
           move      "pgm/orf/fls/ioc/obj/iofyof"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-yof                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-yof-400.
       let-arc-yof-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-yof-des-tof       to   w-let-arc-yof-des      .
           move      rf-yof-vld-dpz       to   w-let-arc-yof-vld      .
           move      rf-yof-cod-dpz       to   w-let-arc-yof-dpz      .
           move      rf-yof-org-doc       to   w-let-arc-yof-ord      .
           move      rf-yof-prv-doc       to   w-let-arc-yof-prd      .
           move      rf-yof-sgl-num       to   w-let-arc-yof-sgl      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-yof-999.
       let-arc-yof-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-yof-flg      .
           move      all   "."            to   w-let-arc-yof-des      .
           go to     let-arc-yof-520.
       let-arc-yof-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-yof-des      .
       let-arc-yof-520.
           move      zero                 to   w-let-arc-yof-vld      .
           move      zero                 to   w-let-arc-yof-dpz      .
           move      zero                 to   w-let-arc-yof-ord      .
           move      zero                 to   w-let-arc-yof-prd      .
           move      spaces               to   w-let-arc-yof-sgl      .
       let-arc-yof-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [fnt]                         *
      *    *-----------------------------------------------------------*
       let-arc-fnt-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-fnt-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice fornitore a zero                 *
      *              *-------------------------------------------------*
           if        w-let-arc-fnt-cod    =    zero
                     go to let-arc-fnt-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODFNT    "         to   f-key                  .
           move      w-let-arc-fnt-cod    to   rf-fnt-cod-fnt         .
           move      "pgm/cge/fls/ioc/obj/ioffnt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fnt                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-fnt-400.
       let-arc-fnt-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-fnt-rag-soc       to   w-let-arc-fnt-rag      .
           move      rf-fnt-via-fnt       to   w-let-arc-fnt-via      .
           move      rf-fnt-loc-fnt       to   w-let-arc-fnt-loc      .
           move      rf-fnt-cod-iva       to   w-let-arc-fnt-ass      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-fnt-999.
       let-arc-fnt-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-fnt-flg      .
           move      all   "."            to   w-let-arc-fnt-rag      .
           go to     let-arc-fnt-600.
       let-arc-fnt-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-fnt-rag      .
       let-arc-fnt-600.
           move      spaces               to   w-let-arc-fnt-via      .
           move      spaces               to   w-let-arc-fnt-loc      .
           move      zero                 to   w-let-arc-fnt-ass      .
       let-arc-fnt-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [dcf]                         *
      *    *-----------------------------------------------------------*
       let-arc-dcf-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-dcf-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice a zero                           *
      *              *-------------------------------------------------*
           if        w-let-arc-dcf-fnt    =    zero
                     go to let-arc-dcf-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCLI"             to   f-key                  .
           move      w-let-arc-dcf-fnt    to   rf-dcf-cod-fnt         .
           move      w-let-arc-dcf-dpz    to   rf-dcf-dpz-fnt         .
           move      "pgm/dcf/fls/ioc/obj/iofdcf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcf                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-dcf-400.
       let-arc-dcf-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-dcf-rag-soc       to   w-let-arc-dcf-rag      .
           move      rf-dcf-via-dcf       to   w-let-arc-dcf-via      .
           move      rf-dcf-loc-dcf       to   w-let-arc-dcf-loc      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-dcf-999.
       let-arc-dcf-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-dcf-flg      .
           move      all"."               to   w-let-arc-dcf-rag      .
           go to     let-arc-dcf-600.
       let-arc-dcf-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-dcf-rag      .
       let-arc-dcf-600.
           move      spaces               to   w-let-arc-dcf-via      .
           move      spaces               to   w-let-arc-dcf-loc      .
       let-arc-dcf-999.
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
      *    * Subroutines per l'accettazione tipo movimento             *
      *    *-----------------------------------------------------------*
           copy      "pgm/orf/prg/cpy/acdeyof0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per accettazione codice fornitore commerciale *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcf/prg/cpy/acmndcf0.acs"                   .

