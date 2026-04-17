       Identification Division.
       Program-Id.                                 pdcc4100           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    dcc                 *
      *                                Settore:    com                 *
      *                                   Fase:    dcc410              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 19/09/94    *
      *                       Ultima revisione:    NdK del 07/06/22    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Richieste per il programma pdcc4101:        *
      *                                                                *
      *                    Stampa etichette da anagrafiche commercia-  *
      *                    li clienti.                                 *
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
                     "dcc"                                            .
      *        *-------------------------------------------------------*
      *        * Settore gestionale                                    *
      *        *-------------------------------------------------------*
           05  i-ide-set                  pic  x(03) value
                     "com"                                            .
      *        *-------------------------------------------------------*
      *        * Fase gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-fas                  pic  x(06) value
                     "dcc410"                                         .
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10) value
                     "pdcc4100"                                       .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  i-ide-des                  pic  x(40) value
                     "        STAMPA ETICHETTE CLIENTI        "       .

      *    *===========================================================*
      *    * Area per il programma di esecuzione                       *
      *    *-----------------------------------------------------------*
       01  i-exe.
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma di esecuzione             *
      *        *-------------------------------------------------------*
           05  i-exe-pro                  pic  x(10) value
                     "pdcc4101  "                                     .
      *        *-------------------------------------------------------*
      *        * Pathname del programma di esecuzione                  *
      *        *-------------------------------------------------------*
           05  i-exe-pat                  pic  x(40) value
                     "pgm/dcc/prg/obj/pdcc4101                "       .

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
      *    * Record files                                              *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [age]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/age/fls/rec/rfage"                          .
      *        *-------------------------------------------------------*
      *        * [zst]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfzst"                          .

      *    *===========================================================*
      *    * Work-area richieste                                       *
      *    *-----------------------------------------------------------*
       01  rr.
      *        *-------------------------------------------------------*
      *        * Tipo ordinamento stampa                               *
      *        *  - 01 : Per cliente                                   *
      *        *  - 02 : Per agente                                    *
      *        *  - 03 : Per zona                                      *
      *        *  - 04 : Per categoria                                 *
      *        *  - 05 : Per codice statistico                         *
      *        *-------------------------------------------------------*
           05  rr-tip-ord                 pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Tipo etichette                                        *
      *        *  - 01 : Modulo cont. 12''   - 1 c. x 8 etichette      *
      *        *  - 02 : Modulo cont. 12''   - 3 c. x 8 etichette      *
      *        *-------------------------------------------------------*
           05  rr-tip-eti                 pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Tipo anagrafica da stampare                           *
      *        *  - 01 : Anagrafica commerciale cliente                *
      *        *  - 02 : Anagrafica contabile cliente                  *
      *        *-------------------------------------------------------*
           05  rr-tip-ana                 pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Ragione sociale iniziale                              *
      *        *                                                       *
      *        * Solo se tipo ordinamento stampa 01                    *
      *        *-------------------------------------------------------*
           05  rr-rag-min                 pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Ragione sociale finale                                *
      *        *                                                       *
      *        * Solo se tipo ordinamento stampa 01                    *
      *        *-------------------------------------------------------*
           05  rr-rag-max                 pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Codice agente iniziale                                *
      *        *                                                       *
      *        * Solo se tipo ordinamento stampa 02                    *
      *        *-------------------------------------------------------*
           05  rr-age-min                 pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Codice agente iniziale, descrizione                   *
      *        *                                                       *
      *        * Solo se tipo ordinamento stampa 02                    *
      *        *-------------------------------------------------------*
           05  rr-age-min-des             pic  x(20)                  .
      *        *-------------------------------------------------------*
      *        * Codice agente finale                                  *
      *        *                                                       *
      *        * Solo se tipo ordinamento stampa 02                    *
      *        *-------------------------------------------------------*
           05  rr-age-max                 pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Codice agente finale, descrizione                     *
      *        *                                                       *
      *        * Solo se tipo ordinamento stampa 02                    *
      *        *-------------------------------------------------------*
           05  rr-age-max-des             pic  x(20)                  .
      *        *-------------------------------------------------------*
      *        * Codice zona iniziale                                  *
      *        *                                                       *
      *        * Solo se tipo ordinamento stampa 03                    *
      *        *-------------------------------------------------------*
           05  rr-zon-min                 pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Codice zona iniziale, descrizione                     *
      *        *                                                       *
      *        * Solo se tipo ordinamento stampa 03                    *
      *        *-------------------------------------------------------*
           05  rr-zon-min-des             pic  x(20)                  .
      *        *-------------------------------------------------------*
      *        * Codice zona finale                                    *
      *        *                                                       *
      *        * Solo se tipo ordinamento stampa 03                    *
      *        *-------------------------------------------------------*
           05  rr-zon-max                 pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Codice zona finale, descrizione                       *
      *        *                                                       *
      *        * Solo se tipo ordinamento stampa 03                    *
      *        *-------------------------------------------------------*
           05  rr-zon-max-des             pic  x(20)                  .
      *        *-------------------------------------------------------*
      *        * Codice categoria iniziale                             *
      *        *                                                       *
      *        * Solo se tipo ordinamento stampa 04                    *
      *        *-------------------------------------------------------*
           05  rr-cat-min                 pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Codice categoria iniziale, descrizione                *
      *        *                                                       *
      *        * Solo se tipo ordinamento stampa 04                    *
      *        *-------------------------------------------------------*
           05  rr-cat-min-des             pic  x(20)                  .
      *        *-------------------------------------------------------*
      *        * Codice categoria finale                               *
      *        *                                                       *
      *        * Solo se tipo ordinamento stampa 04                    *
      *        *-------------------------------------------------------*
           05  rr-cat-max                 pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Codice categoria finale, descrizione                  *
      *        *                                                       *
      *        * Solo se tipo ordinamento stampa 04                    *
      *        *-------------------------------------------------------*
           05  rr-cat-max-des             pic  x(20)                  .
      *        *-------------------------------------------------------*
      *        * Codice statistico iniziale                            *
      *        *                                                       *
      *        * Solo se tipo ordinamento stampa 05                    *
      *        *-------------------------------------------------------*
           05  rr-stt-min                 pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Codice statistico iniziale, descrizione               *
      *        *                                                       *
      *        * Solo se tipo ordinamento stampa 05                    *
      *        *-------------------------------------------------------*
           05  rr-stt-min-des             pic  x(20)                  .
      *        *-------------------------------------------------------*
      *        * Codice statistico finale                              *
      *        *                                                       *
      *        * Solo se tipo ordinamento stampa 05                    *
      *        *-------------------------------------------------------*
           05  rr-stt-max                 pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Codice statistico finale, descrizione                 *
      *        *                                                       *
      *        * Solo se tipo ordinamento stampa 05                    *
      *        *-------------------------------------------------------*
           05  rr-stt-max-des             pic  x(20)                  .
      *        *-------------------------------------------------------*
      *        * Si/No stampa nome interlocutore                       *
      *        *  - 01 : Si                                            *
      *        *  - 02 : No                                            *
      *        *-------------------------------------------------------*
           05  rr-snx-int                 pic  9(02)                  .

      *    *===========================================================*
      *    * Link-area per accettazione codice agente                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/age/prg/cpy/acmnage0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice zona                    *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcc/prg/cpy/acmnzst1.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice categoria               *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcc/prg/cpy/acmnzst2.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice statistico              *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcc/prg/cpy/acmnzst3.acl"                   .

      *    *===========================================================*
      *    * Work per subroutines di Let                               *
      *    *-----------------------------------------------------------*
       01  w-let.
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [age]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-age.
               10  w-let-arc-age-flg      pic  x(01)                  .
               10  w-let-arc-age-cod      pic  9(07)                  .
               10  w-let-arc-age-nom      pic  x(20)                  .
               10  w-let-arc-age-sup      pic  9(07)                  .
               10  w-let-arc-age-cat      pic  9(05)                  .
               10  w-let-arc-age-per occurs 03 
                                          pic  9(02)v9(01)            .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [zst]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-zst.
               10  w-let-arc-zst-flg      pic  x(01)                  .
               10  w-let-arc-zst-tip      pic  9(02)                  .
               10  w-let-arc-zst-cod      pic  9(05)                  .
               10  w-let-arc-zst-des      pic  x(20)                  .
              
      *    *===========================================================*
      *    * Work per accettazioni campi espansi                       *
      *    *-----------------------------------------------------------*
       01  w-exp.
      *        *-------------------------------------------------------*
      *        * Work per : Tipo ordinamento                           *
      *        *-------------------------------------------------------*
           05  w-exp-tip-ord.
               10  w-exp-tip-ord-num      pic  9(02)       value 5    .
               10  w-exp-tip-ord-lun      pic  9(02)       value 40   .
               10  w-exp-tip-ord-tbl.
                   15  filler             pic  x(40) value
                         "per Cliente                             "   .
                   15  filler             pic  x(40) value
                         "per Agente                              "   .
                   15  filler             pic  x(40) value
                         "per Zona cliente                        "   .
                   15  filler             pic  x(40) value
                         "per caTegoria cliente                   "   .
                   15  filler             pic  x(40) value
                         "per codice Statistico cliente           "   .
      *        *-------------------------------------------------------*
      *        * Work per : Tipo etichette                             *
      *        *-------------------------------------------------------*
           05  w-exp-tip-eti.
               10  w-exp-tip-eti-num      pic  9(02)       value 2    .
               10  w-exp-tip-eti-lun      pic  9(02)       value 40   .
               10  w-exp-tip-eti-tbl.
                   15  filler             pic  x(40) value
                         "Modulo cont. 12''   - 1 c. x 8 etichette"   .
                   15  filler             pic  x(40) value
                         "Laser A4            - 3 c. x 8 etichette"   .
      *        *-------------------------------------------------------*
      *        * Work per : Tipo anagrafica cliente                    *
      *        *-------------------------------------------------------*
           05  w-exp-tip-ana.
               10  w-exp-tip-ana-num      pic  9(02)       value 2    .
               10  w-exp-tip-ana-lun      pic  9(02)       value 40   .
               10  w-exp-tip-ana-tbl.
                   15  filler             pic  x(40) value
                         "Anagrafica commerciale del cliente      "   .
                   15  filler             pic  x(40) value
                         "Anagrafica contabile del cliente        "   .
      *        *-------------------------------------------------------*
      *        * Work per : Si/No stampa nome interlocutore            *
      *        *-------------------------------------------------------*
           05  w-exp-snx-int.
               10  w-exp-snx-int-num      pic  9(02)       value 2    .
               10  w-exp-snx-int-lun      pic  9(02)       value 02   .
               10  w-exp-snx-int-tbl.
                   15  filler             pic  x(02) value "No"       .
                   15  filler             pic  x(02) value "Si"       .

      *    *===========================================================*
      *    * Work-area per salvataggi                                  *
      *    *-----------------------------------------------------------*
       01  w-sav.
      *        *-------------------------------------------------------*
      *        * Salvataggio per tipo ordinamento stampa               *
      *        *-------------------------------------------------------*
           05  w-sav-tip-ord              pic  9(02)                  .

      *    *===========================================================*
      *    * Work per subroutines di Err                               *
      *    *-----------------------------------------------------------*
       01  w-err.
      *        *-------------------------------------------------------*
      *        * Work per Err con box centrale                         *
      *        *-------------------------------------------------------*
           05  w-err-box-err.
               10  w-err-box-err-msg      pic  x(65)                  .

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
      *              * Open modulo accettazione codice agente          *
      *              *-------------------------------------------------*
           perform   cod-mne-age-opn-000  thru cod-mne-age-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice zona            *
      *              *-------------------------------------------------*
           perform   cmn-zst-001-opn-000  thru cmn-zst-001-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice categoria       *
      *              *-------------------------------------------------*
           perform   cmn-zst-002-opn-000  thru cmn-zst-002-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice statistico      *
      *              *-------------------------------------------------*
           perform   cmn-zst-003-opn-000  thru cmn-zst-003-opn-999    .
      *              *-------------------------------------------------*
      *              * [age]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/age/fls/ioc/obj/iofage"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-age                 .
      *              *-------------------------------------------------*
      *              * [zst]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofzst"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zst                 .
       rou-opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Close files per richieste                                 *
      *    *-----------------------------------------------------------*
       rou-cls-fls-000.
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice agente         *
      *              *-------------------------------------------------*
           perform   cod-mne-age-cls-000  thru cod-mne-age-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice zona           *
      *              *-------------------------------------------------*
           perform   cmn-zst-001-cls-000  thru cmn-zst-001-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice categoria      *
      *              *-------------------------------------------------*
           perform   cmn-zst-002-cls-000  thru cmn-zst-002-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice statistico     *
      *              *-------------------------------------------------*
           perform   cmn-zst-003-cls-000  thru cmn-zst-003-cls-999    .
      *              *-------------------------------------------------*
      *              * [age]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/age/fls/ioc/obj/iofage"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-age                 .
      *              *-------------------------------------------------*
      *              * [zst]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofzst"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zst                 .
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
      *                  * Abilitazione tasto Do                       *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cnt-sts-imp-ric      .
       acc-ric-sel-200.
      *                  *---------------------------------------------*
      *                  * Tipo ordinamento stampa                     *
      *                  *---------------------------------------------*
           perform   acc-tip-ord-000      thru acc-tip-ord-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
       acc-ric-sel-300.
      *                  *---------------------------------------------*
      *                  * Tipo etichette                              *
      *                  *---------------------------------------------*
           perform   acc-tip-eti-000      thru acc-tip-eti-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-200.
       acc-ric-sel-400.
      *                  *---------------------------------------------*
      *                  * Tipo anagrafica da stampare                 *
      *                  *---------------------------------------------*
           perform   acc-tip-ana-000      thru acc-tip-ana-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-300.
       acc-ric-sel-500.
      *                  *---------------------------------------------*
      *                  * Valore iniziale e finale                    *
      *                  *---------------------------------------------*
       acc-ric-sel-501.
      *                      *-----------------------------------------*
      *                      * Deviazione a seconda del tipo di ordi-  *
      *                      * namento della stampa                    *
      *                      *-----------------------------------------*
           if        rr-tip-ord           =    01
                     go to acc-ric-sel-510
           else if   rr-tip-ord           =    02
                     go to acc-ric-sel-520
           else if   rr-tip-ord           =    03
                     go to acc-ric-sel-530
           else if   rr-tip-ord           =    04
                     go to acc-ric-sel-540
           else if   rr-tip-ord           =    05
                     go to acc-ric-sel-550.
       acc-ric-sel-502.
      *                      *-----------------------------------------*
      *                      * Se tipo di ordinamento 00               *
      *                      *-----------------------------------------*
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-300
           else      go to acc-ric-sel-600.
       acc-ric-sel-510.
      *                      *-----------------------------------------*
      *                      * Se tipo di ordinamento 01               *
      *                      *-----------------------------------------*
       acc-ric-sel-511.
      *                          *-------------------------------------*
      *                          * Ragione sociale iniziale            *
      *                          *-------------------------------------*
           perform   acc-rag-min-000      thru acc-rag-min-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-400.
       acc-ric-sel-512.
      *                          *-------------------------------------*
      *                          * Ragione sociale finale              *
      *                          *-------------------------------------*
           perform   acc-rag-max-000      thru acc-rag-max-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-511
           else      go to acc-ric-sel-600.
       acc-ric-sel-520.
      *                      *-----------------------------------------*
      *                      * Se tipo di ordinamento 02               *
      *                      *-----------------------------------------*
       acc-ric-sel-521.
      *                          *-------------------------------------*
      *                          * Codice agente iniziale              *
      *                          *-------------------------------------*
           perform   acc-age-min-000      thru acc-age-min-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-400.
       acc-ric-sel-522.
      *                          *-------------------------------------*
      *                          * Codice agente finale                *
      *                          *-------------------------------------*
           perform   acc-age-max-000      thru acc-age-max-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-521
           else      go to acc-ric-sel-600.
       acc-ric-sel-530.
      *                      *-----------------------------------------*
      *                      * Se tipo di ordinamento 03               *
      *                      *-----------------------------------------*
       acc-ric-sel-531.
      *                          *-------------------------------------*
      *                          * Codice zona iniziale                *
      *                          *-------------------------------------*
           perform   acc-zon-min-000      thru acc-zon-min-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-400.
       acc-ric-sel-532.
      *                          *-------------------------------------*
      *                          * Codice zona finale                  *
      *                          *-------------------------------------*
           perform   acc-zon-max-000      thru acc-zon-max-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-531
           else      go to acc-ric-sel-600.
       acc-ric-sel-540.
      *                      *-----------------------------------------*
      *                      * Se tipo di ordinamento 04               *
      *                      *-----------------------------------------*
       acc-ric-sel-541.
      *                          *-------------------------------------*
      *                          * Codice categoria iniziale           *
      *                          *-------------------------------------*
           perform   acc-cat-min-000      thru acc-cat-min-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-400.
       acc-ric-sel-542.
      *                          *-------------------------------------*
      *                          * Codice categoria finale             *
      *                          *-------------------------------------*
           perform   acc-cat-max-000      thru acc-cat-max-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-541
           else      go to acc-ric-sel-600.
       acc-ric-sel-550.
      *                      *-----------------------------------------*
      *                      * Se tipo di ordinamento 05               *
      *                      *-----------------------------------------*
       acc-ric-sel-551.
      *                          *-------------------------------------*
      *                          * Codice statistico iniziale          *
      *                          *-------------------------------------*
           perform   acc-stt-min-000      thru acc-stt-min-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-400.
       acc-ric-sel-552.
      *                          *-------------------------------------*
      *                          * Codice statistico finale            *
      *                          *-------------------------------------*
           perform   acc-stt-max-000      thru acc-stt-max-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-551
           else      go to acc-ric-sel-600.
       acc-ric-sel-600.
      *                  *---------------------------------------------*
      *                  * Si/No stampa nome interlocutore             *
      *                  *---------------------------------------------*
           perform   acc-snx-int-000      thru acc-snx-int-999        .
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
           go to     acc-ric-sel-600.
       acc-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per richieste di selezione        *
      *    *-----------------------------------------------------------*
       pmt-ric-sel-000.
      *              *-------------------------------------------------*
      *              * Tipo ordinamento stampa                         *
      *              *-------------------------------------------------*
           perform   pmt-tip-ord-000      thru pmt-tip-ord-999        .
      *              *-------------------------------------------------*
      *              * Tipo etichette                                  *
      *              *-------------------------------------------------*
           perform   pmt-tip-eti-000      thru pmt-tip-eti-999        .
      *              *-------------------------------------------------*
      *              * Tipo anagrafica da stampare                     *
      *              *-------------------------------------------------*
           perform   pmt-tip-ana-000      thru pmt-tip-ana-999        .
      *              *-------------------------------------------------*
      *              * Valore iniziale e finale                        *
      *              *-------------------------------------------------*
           perform   pmt-min-max-000      thru pmt-min-max-999        .
      *              *-------------------------------------------------*
      *              * Si/No stampa nome interlocutore                 *
      *              *-------------------------------------------------*
           perform   pmt-snx-int-000      thru pmt-snx-int-999        .
       pmt-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Tipo ordinamento stampa                          *
      *    *-----------------------------------------------------------*
       pmt-tip-ord-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tipo ordinamento stampa    :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-tip-ord-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Tipo etichette                                   *
      *    *-----------------------------------------------------------*
       pmt-tip-eti-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tipo etichette             :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-tip-eti-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Tipo anagrafica da stampare                      *
      *    *-----------------------------------------------------------*
       pmt-tip-ana-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Anagrafica da stampare     :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-tip-ana-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Valore iniziale e finale                         *
      *    *-----------------------------------------------------------*
       pmt-min-max-000.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda del tipo di ordinamento    *
      *              * della stampa                                    *
      *              *-------------------------------------------------*
           if        rr-tip-ord           =    01
                     go to pmt-min-max-100
           else if   rr-tip-ord           =    02
                     go to pmt-min-max-200
           else if   rr-tip-ord           =    03
                     go to pmt-min-max-300
           else if   rr-tip-ord           =    04
                     go to pmt-min-max-400
           else if   rr-tip-ord           =    05
                     go to pmt-min-max-500.
       pmt-min-max-050.
      *              *-------------------------------------------------*
      *              * Se tipo di ordinamento 00                       *
      *              *-------------------------------------------------*
           perform   pmt-mmx-nul-000      thru pmt-mmx-nul-999        .
           go to     pmt-min-max-999.
       pmt-min-max-100.
      *              *-------------------------------------------------*
      *              * Se tipo di ordinamento 01                       *
      *              *-------------------------------------------------*
           perform   pmt-mmx-cli-000      thru pmt-mmx-cli-999        .
           go to     pmt-min-max-999.
       pmt-min-max-200.
      *              *-------------------------------------------------*
      *              * Se tipo di ordinamento 02                       *
      *              *-------------------------------------------------*
           perform   pmt-mmx-age-000      thru pmt-mmx-age-999        .
           go to     pmt-min-max-999.
       pmt-min-max-300.
      *              *-------------------------------------------------*
      *              * Se tipo di ordinamento 03                       *
      *              *-------------------------------------------------*
           perform   pmt-mmx-zon-000      thru pmt-mmx-zon-999        .
           go to     pmt-min-max-999.
       pmt-min-max-400.
      *              *-------------------------------------------------*
      *              * Se tipo di ordinamento 04                       *
      *              *-------------------------------------------------*
           perform   pmt-mmx-cat-000      thru pmt-mmx-cat-999        .
           go to     pmt-min-max-999.
       pmt-min-max-500.
      *              *-------------------------------------------------*
      *              * Se tipo di ordinamento 05                       *
      *              *-------------------------------------------------*
           perform   pmt-mmx-stt-000      thru pmt-mmx-stt-999        .
           go to     pmt-min-max-999.
       pmt-min-max-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Valore iniziale e finale, in caso di tipo ordi-  *
      *    *          namento stampa 00                                *
      *    *-----------------------------------------------------------*
       pmt-mmx-nul-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-mmx-nul-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Valore iniziale e finale, in caso di tipo ordi-  *
      *    *          namento stampa 01                                *
      *    *-----------------------------------------------------------*
       pmt-mmx-cli-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Ragione sociale iniziale   :                      
      -              "                              "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Ragione sociale finale     :                      
      -              "                              "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-mmx-cli-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Valore iniziale e finale, in caso di tipo ordi-  *
      *    *          namento stampa 02                                *
      *    *-----------------------------------------------------------*
       pmt-mmx-age-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice agente iniziale     :                      
      -              "                              "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice agente finale       :                      
      -              "                              "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-mmx-age-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Valore iniziale e finale, in caso di tipo ordi-  *
      *    *          namento stampa 03                                *
      *    *-----------------------------------------------------------*
       pmt-mmx-zon-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice zona iniziale       :                      
      -              "                              "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice zona finale         :                      
      -              "                              "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-mmx-zon-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Valore iniziale e finale, in caso di tipo ordi-  *
      *    *          namento stampa 04                                *
      *    *-----------------------------------------------------------*
       pmt-mmx-cat-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice categoria iniziale  :                      
      -              "                              "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice categoria finale    :                      
      -              "                              "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-mmx-cat-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Valore iniziale e finale, in caso di tipo ordi-  *
      *    *          namento stampa 05                                *
      *    *-----------------------------------------------------------*
       pmt-mmx-stt-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice statistico iniziale :                      
      -              "                              "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice statistico finale   :                      
      -              "                              "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-mmx-stt-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Si/No stampa nome interlocutore                  *
      *    *-----------------------------------------------------------*
       pmt-snx-int-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Stampa nome interlocutore  :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-snx-int-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Tipo ordinamento                           *
      *    *-----------------------------------------------------------*
       acc-tip-ord-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-tip-ord-025.
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      rr-tip-ord           to   w-sav-tip-ord          .
       acc-tip-ord-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-ord-lun    to   v-car                  .
           move      w-exp-tip-ord-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      "CAZTS#"             to   v-msk                  .
           move      05                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-tip-ord-tbl    to   v-txt                  .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-tip-ord           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-tip-ord-999.
       acc-tip-ord-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-tip-ord             .
       acc-tip-ord-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-tip-ord-425.
      *                  *---------------------------------------------*
      *                  * Che il valore non sia mancante, a meno che  *
      *                  * non si sia in Up                            *
      *                  *---------------------------------------------*
           if        rr-tip-ord           =    zero   and
                     v-key                =    "UP  "
                     go to acc-tip-ord-600.
       acc-tip-ord-450.
      *                  *---------------------------------------------*
      *                  * Che il valore sia consentito                *
      *                  *---------------------------------------------*
           if        rr-tip-ord           =    zero           or
                     rr-tip-ord           >    w-exp-tip-ord-num
                     go to acc-tip-ord-100.
       acc-tip-ord-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-tip-ord-625.
      *                  *---------------------------------------------*
      *                  * Se il valore e' immutato rispetto al valore *
      *                  * precedente : nessuna dipendenza             *
      *                  *---------------------------------------------*
           if        rr-tip-ord           =    w-sav-tip-ord
                     go to acc-tip-ord-800.
      *                  *---------------------------------------------*
      *                  * Normalizzazione dati relativi ai valori     *
      *                  * minimi e massimi                            *
      *                  *---------------------------------------------*
           perform   nor-min-max-000      thru nor-min-max-999        .
      *                  *---------------------------------------------*
      *                  * Prompt relativo ai valori minimi e massimi  *
      *                  *---------------------------------------------*
           perform   pmt-min-max-000      thru pmt-min-max-999        .
       acc-tip-ord-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-tip-ord-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-tip-ord-100.
       acc-tip-ord-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Tipo etichette                             *
      *    *-----------------------------------------------------------*
       acc-tip-eti-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-tip-eti-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-eti-lun    to   v-car                  .
           move      w-exp-tip-eti-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      "ML#"                to   v-msk                  .
           move      08                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-tip-eti-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-tip-eti           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-tip-eti-999.
       acc-tip-eti-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-tip-eti             .
       acc-tip-eti-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-tip-eti-425.
      *                  *---------------------------------------------*
      *                  * Che il valore non sia mancante, a meno che  *
      *                  * non si sia in Up                            *
      *                  *---------------------------------------------*
           if        rr-tip-eti           =    zero   and
                     v-key                =    "UP  "
                     go to acc-tip-eti-600.
       acc-tip-eti-450.
      *                  *---------------------------------------------*
      *                  * Che il valore sia consentito                *
      *                  *---------------------------------------------*
           if        rr-tip-eti           =    zero           or
                     rr-tip-eti           >    w-exp-tip-eti-num
                     go to acc-tip-eti-100.
       acc-tip-eti-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-tip-eti-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-tip-eti-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-tip-eti-100.
       acc-tip-eti-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Tipo anagrafica da stampare                *
      *    *-----------------------------------------------------------*
       acc-tip-ana-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-tip-ana-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-ana-lun    to   v-car                  .
           move      w-exp-tip-ana-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      spaces               to   v-msk                  .
           move      11                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-tip-ana-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-tip-ana           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-tip-ana-999.
       acc-tip-ana-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-tip-ana             .
       acc-tip-ana-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-tip-ana-425.
      *                  *---------------------------------------------*
      *                  * Che il valore non sia mancante, a meno che  *
      *                  * non si sia in Up                            *
      *                  *---------------------------------------------*
           if        rr-tip-ana           =    zero   and
                     v-key                =    "UP  "
                     go to acc-tip-ana-600.
       acc-tip-ana-450.
      *                  *---------------------------------------------*
      *                  * Che il valore sia consentito                *
      *                  *---------------------------------------------*
           if        rr-tip-ana           =    zero           or
                     rr-tip-ana           >    w-exp-tip-ana-num
                     go to acc-tip-ana-100.
       acc-tip-ana-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-tip-ana-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-tip-ana-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-tip-ana-100.
       acc-tip-ana-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Ragione sociale iniziale                   *
      *    *-----------------------------------------------------------*
       acc-rag-min-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-rag-min-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-rag-min           to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-rag-min-999.
       acc-rag-min-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   rr-rag-min             .
       acc-rag-min-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-rag-min-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-rag-min-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-rag-min-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-rag-min-100.
       acc-rag-min-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Ragione sociale finale                     *
      *    *-----------------------------------------------------------*
       acc-rag-max-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-rag-max-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-rag-max           to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-rag-max-999.
       acc-rag-max-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   rr-rag-max             .
       acc-rag-max-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-rag-max-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-rag-max-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-rag-max-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-rag-max-100.
       acc-rag-max-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Codice agente iniziale                     *
      *    *-----------------------------------------------------------*
       acc-age-min-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-age-min-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-age-ope      .
           move      rr-age-min           to   w-cod-mne-age-cod      .
           move      14                   to   w-cod-mne-age-lin      .
           move      30                   to   w-cod-mne-age-pos      .
           move      14                   to   w-cod-mne-age-nln      .
           move      41                   to   w-cod-mne-age-nps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-mne-age-cll-000  thru cod-mne-age-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-mne-age-foi-000  thru cod-mne-age-foi-999    .
       acc-age-min-110.
           perform   cod-mne-age-cll-000  thru cod-mne-age-cll-999    .
           if        w-cod-mne-age-ope    =    "F+"
                     go to acc-age-min-115.
           if        w-cod-mne-age-ope    =    "AC"
                     go to acc-age-min-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-age-min-115.
           perform   cod-mne-age-foi-000  thru cod-mne-age-foi-999    .
           go to     acc-age-min-110.
       acc-age-min-120.
           move      w-cod-mne-age-cod    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-age-min-999.
       acc-age-min-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-age-min             .
       acc-age-min-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura archivio [age]                      *
      *                  *---------------------------------------------*
           move      rr-age-min           to   w-let-arc-age-cod      .
           perform   let-arc-age-000      thru let-arc-age-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione dati agente                  *
      *                  *---------------------------------------------*
           move      w-let-arc-age-nom    to   rr-age-min-des         .
      *                  *---------------------------------------------*
      *                  * Visualizzazione dati agente                 *
      *                  *---------------------------------------------*
           perform   vis-age-min-des-000  thru vis-age-min-des-999    .
       acc-age-min-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-age-min-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-age-min-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-age-min-100.
       acc-age-min-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Codice agente iniziale, descrizione     *
      *    *-----------------------------------------------------------*
       vis-age-min-des-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      rr-age-min-des       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-age-min-des-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Codice agente finale                       *
      *    *-----------------------------------------------------------*
       acc-age-max-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-age-max-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-age-ope      .
           move      rr-age-max           to   w-cod-mne-age-cod      .
           move      15                   to   w-cod-mne-age-lin      .
           move      30                   to   w-cod-mne-age-pos      .
           move      15                   to   w-cod-mne-age-nln      .
           move      41                   to   w-cod-mne-age-nps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-mne-age-cll-000  thru cod-mne-age-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-mne-age-foi-000  thru cod-mne-age-foi-999    .
       acc-age-max-110.
           perform   cod-mne-age-cll-000  thru cod-mne-age-cll-999    .
           if        w-cod-mne-age-ope    =    "F+"
                     go to acc-age-max-115.
           if        w-cod-mne-age-ope    =    "AC"
                     go to acc-age-max-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-age-max-115.
           perform   cod-mne-age-foi-000  thru cod-mne-age-foi-999    .
           go to     acc-age-max-110.
       acc-age-max-120.
           move      w-cod-mne-age-cod    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-age-max-999.
       acc-age-max-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-age-max             .
       acc-age-max-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura archivio [age]                      *
      *                  *---------------------------------------------*
           move      rr-age-max           to   w-let-arc-age-cod      .
           perform   let-arc-age-000      thru let-arc-age-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione dati agente                  *
      *                  *---------------------------------------------*
           move      w-let-arc-age-nom    to   rr-age-max-des         .
      *                  *---------------------------------------------*
      *                  * Visualizzazione dati agente                 *
      *                  *---------------------------------------------*
           perform   vis-age-max-des-000  thru vis-age-max-des-999    .
       acc-age-max-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-age-max-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-age-max-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-age-max-100.
       acc-age-max-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Codice agente finale, descrizione       *
      *    *-----------------------------------------------------------*
       vis-age-max-des-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      rr-age-max-des       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-age-max-des-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Codice zona iniziale                       *
      *    *-----------------------------------------------------------*
       acc-zon-min-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-zon-min-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cmn-zst-001-ope      .
           move      rr-zon-min           to   w-cmn-zst-001-cod      .
           move      14                   to   w-cmn-zst-001-lin      .
           move      30                   to   w-cmn-zst-001-pos      .
           move      14                   to   w-cmn-zst-001-dln      .
           move      41                   to   w-cmn-zst-001-dps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cmn-zst-001-cll-000  thru cmn-zst-001-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cmn-zst-001-foi-000  thru cmn-zst-001-foi-999    .
       acc-zon-min-110.
           perform   cmn-zst-001-cll-000  thru cmn-zst-001-cll-999    .
           if        w-cmn-zst-001-ope    =    "F+"
                     go to acc-zon-min-115.
           if        w-cmn-zst-001-ope    =    "AC"
                     go to acc-zon-min-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-zon-min-115.
           perform   cmn-zst-001-foi-000  thru cmn-zst-001-foi-999    .
           go to     acc-zon-min-110.
       acc-zon-min-120.
           move      w-cmn-zst-001-cod    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-zon-min-999.
       acc-zon-min-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-zon-min             .
       acc-zon-min-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura tabella                             *
      *                  *---------------------------------------------*
           move      01                   to   w-let-arc-zst-tip      .
           move      rr-zon-min           to   w-let-arc-zst-cod      .
           perform   let-arc-zst-000      thru let-arc-zst-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione descrizione                  *
      *                  *---------------------------------------------*
           move      w-let-arc-zst-des    to   rr-zon-min-des         .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione                 *
      *                  *---------------------------------------------*
           perform   vis-zon-min-des-000  thru vis-zon-min-des-999    .
       acc-zon-min-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-zon-min-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-zon-min-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-zon-min-100.
       acc-zon-min-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Codice zona iniziale, descrizione       *
      *    *-----------------------------------------------------------*
       vis-zon-min-des-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      rr-zon-min-des       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-zon-min-des-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Codice zona finale                         *
      *    *-----------------------------------------------------------*
       acc-zon-max-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-zon-max-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cmn-zst-001-ope      .
           move      rr-zon-max           to   w-cmn-zst-001-cod      .
           move      15                   to   w-cmn-zst-001-lin      .
           move      30                   to   w-cmn-zst-001-pos      .
           move      15                   to   w-cmn-zst-001-dln      .
           move      41                   to   w-cmn-zst-001-dps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cmn-zst-001-cll-000  thru cmn-zst-001-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cmn-zst-001-foi-000  thru cmn-zst-001-foi-999    .
       acc-zon-max-110.
           perform   cmn-zst-001-cll-000  thru cmn-zst-001-cll-999    .
           if        w-cmn-zst-001-ope    =    "F+"
                     go to acc-zon-max-115.
           if        w-cmn-zst-001-ope    =    "AC"
                     go to acc-zon-max-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-zon-max-115.
           perform   cmn-zst-001-foi-000  thru cmn-zst-001-foi-999    .
           go to     acc-zon-max-110.
       acc-zon-max-120.
           move      w-cmn-zst-001-cod    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-zon-max-999.
       acc-zon-max-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-zon-max             .
       acc-zon-max-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura tabella                             *
      *                  *---------------------------------------------*
           move      01                   to   w-let-arc-zst-tip      .
           move      rr-zon-max           to   w-let-arc-zst-cod      .
           perform   let-arc-zst-000      thru let-arc-zst-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione descrizione                  *
      *                  *---------------------------------------------*
           move      w-let-arc-zst-des    to   rr-zon-max-des         .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione                 *
      *                  *---------------------------------------------*
           perform   vis-zon-max-des-000  thru vis-zon-max-des-999    .
       acc-zon-max-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-zon-max-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-zon-max-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-zon-max-100.
       acc-zon-max-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Codice zona finale, descrizione         *
      *    *-----------------------------------------------------------*
       vis-zon-max-des-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      rr-zon-max-des       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-zon-max-des-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Codice categoria iniziale                  *
      *    *-----------------------------------------------------------*
       acc-cat-min-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cat-min-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cmn-zst-002-ope      .
           move      rr-cat-min           to   w-cmn-zst-002-cod      .
           move      14                   to   w-cmn-zst-002-lin      .
           move      30                   to   w-cmn-zst-002-pos      .
           move      14                   to   w-cmn-zst-002-dln      .
           move      41                   to   w-cmn-zst-002-dps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cmn-zst-002-cll-000  thru cmn-zst-002-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cmn-zst-002-foi-000  thru cmn-zst-002-foi-999    .
       acc-cat-min-110.
           perform   cmn-zst-002-cll-000  thru cmn-zst-002-cll-999    .
           if        w-cmn-zst-002-ope    =    "F+"
                     go to acc-cat-min-115.
           if        w-cmn-zst-002-ope    =    "AC"
                     go to acc-cat-min-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cat-min-115.
           perform   cmn-zst-002-foi-000  thru cmn-zst-002-foi-999    .
           go to     acc-cat-min-110.
       acc-cat-min-120.
           move      w-cmn-zst-002-cod    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-cat-min-999.
       acc-cat-min-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-cat-min             .
       acc-cat-min-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura tabella                             *
      *                  *---------------------------------------------*
           move      02                   to   w-let-arc-zst-tip      .
           move      rr-cat-min           to   w-let-arc-zst-cod      .
           perform   let-arc-zst-000      thru let-arc-zst-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione descrizione                  *
      *                  *---------------------------------------------*
           move      w-let-arc-zst-des    to   rr-cat-min-des         .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione                 *
      *                  *---------------------------------------------*
           perform   vis-cat-min-des-000  thru vis-cat-min-des-999    .
       acc-cat-min-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cat-min-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-cat-min-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-cat-min-100.
       acc-cat-min-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Codice categoria iniziale, descrizione  *
      *    *-----------------------------------------------------------*
       vis-cat-min-des-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      rr-cat-min-des       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cat-min-des-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Codice categoria finale                    *
      *    *-----------------------------------------------------------*
       acc-cat-max-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cat-max-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cmn-zst-002-ope      .
           move      rr-cat-max           to   w-cmn-zst-002-cod      .
           move      15                   to   w-cmn-zst-002-lin      .
           move      30                   to   w-cmn-zst-002-pos      .
           move      15                   to   w-cmn-zst-002-dln      .
           move      41                   to   w-cmn-zst-002-dps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cmn-zst-002-cll-000  thru cmn-zst-002-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cmn-zst-002-foi-000  thru cmn-zst-002-foi-999    .
       acc-cat-max-110.
           perform   cmn-zst-002-cll-000  thru cmn-zst-002-cll-999    .
           if        w-cmn-zst-002-ope    =    "F+"
                     go to acc-cat-max-115.
           if        w-cmn-zst-002-ope    =    "AC"
                     go to acc-cat-max-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cat-max-115.
           perform   cmn-zst-002-foi-000  thru cmn-zst-002-foi-999    .
           go to     acc-cat-max-110.
       acc-cat-max-120.
           move      w-cmn-zst-002-cod    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-cat-max-999.
       acc-cat-max-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-cat-max             .
       acc-cat-max-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura tabella                             *
      *                  *---------------------------------------------*
           move      02                   to   w-let-arc-zst-tip      .
           move      rr-cat-max           to   w-let-arc-zst-cod      .
           perform   let-arc-zst-000      thru let-arc-zst-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione descrizione                  *
      *                  *---------------------------------------------*
           move      w-let-arc-zst-des    to   rr-cat-max-des         .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione                 *
      *                  *---------------------------------------------*
           perform   vis-cat-max-des-000  thru vis-cat-max-des-999    .
       acc-cat-max-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cat-max-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-cat-max-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-cat-max-100.
       acc-cat-max-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Codice categoria finale, descrizione    *
      *    *-----------------------------------------------------------*
       vis-cat-max-des-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      rr-cat-max-des       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cat-max-des-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Codice statistico iniziale                 *
      *    *-----------------------------------------------------------*
       acc-stt-min-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-stt-min-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cmn-zst-003-ope      .
           move      rr-stt-min           to   w-cmn-zst-003-cod      .
           move      14                   to   w-cmn-zst-003-lin      .
           move      30                   to   w-cmn-zst-003-pos      .
           move      14                   to   w-cmn-zst-003-dln      .
           move      41                   to   w-cmn-zst-003-dps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cmn-zst-003-cll-000  thru cmn-zst-003-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cmn-zst-003-foi-000  thru cmn-zst-003-foi-999    .
       acc-stt-min-110.
           perform   cmn-zst-003-cll-000  thru cmn-zst-003-cll-999    .
           if        w-cmn-zst-003-ope    =    "F+"
                     go to acc-stt-min-115.
           if        w-cmn-zst-003-ope    =    "AC"
                     go to acc-stt-min-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-stt-min-115.
           perform   cmn-zst-003-foi-000  thru cmn-zst-003-foi-999    .
           go to     acc-stt-min-110.
       acc-stt-min-120.
           move      w-cmn-zst-003-cod    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-stt-min-999.
       acc-stt-min-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-stt-min             .
       acc-stt-min-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura tabella                             *
      *                  *---------------------------------------------*
           move      03                   to   w-let-arc-zst-tip      .
           move      rr-stt-min           to   w-let-arc-zst-cod      .
           perform   let-arc-zst-000      thru let-arc-zst-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione descrizione                  *
      *                  *---------------------------------------------*
           move      w-let-arc-zst-des    to   rr-stt-min-des         .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione                 *
      *                  *---------------------------------------------*
           perform   vis-stt-min-des-000  thru vis-stt-min-des-999    .
       acc-stt-min-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-stt-min-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-stt-min-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-stt-min-100.
       acc-stt-min-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Codice statistico iniziale, descrizione *
      *    *-----------------------------------------------------------*
       vis-stt-min-des-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      rr-stt-min-des       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-stt-min-des-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Codice statistico finale                   *
      *    *-----------------------------------------------------------*
       acc-stt-max-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-stt-max-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cmn-zst-003-ope      .
           move      rr-stt-max           to   w-cmn-zst-003-cod      .
           move      15                   to   w-cmn-zst-003-lin      .
           move      30                   to   w-cmn-zst-003-pos      .
           move      15                   to   w-cmn-zst-003-dln      .
           move      41                   to   w-cmn-zst-003-dps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cmn-zst-003-cll-000  thru cmn-zst-003-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cmn-zst-003-foi-000  thru cmn-zst-003-foi-999    .
       acc-stt-max-110.
           perform   cmn-zst-003-cll-000  thru cmn-zst-003-cll-999    .
           if        w-cmn-zst-003-ope    =    "F+"
                     go to acc-stt-max-115.
           if        w-cmn-zst-003-ope    =    "AC"
                     go to acc-stt-max-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-stt-max-115.
           perform   cmn-zst-003-foi-000  thru cmn-zst-003-foi-999    .
           go to     acc-stt-max-110.
       acc-stt-max-120.
           move      w-cmn-zst-003-cod    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-stt-max-999.
       acc-stt-max-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-stt-max             .
       acc-stt-max-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura tabella                             *
      *                  *---------------------------------------------*
           move      03                   to   w-let-arc-zst-tip      .
           move      rr-stt-max           to   w-let-arc-zst-cod      .
           perform   let-arc-zst-000      thru let-arc-zst-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione descrizione                  *
      *                  *---------------------------------------------*
           move      w-let-arc-zst-des    to   rr-stt-max-des         .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione                 *
      *                  *---------------------------------------------*
           perform   vis-stt-max-des-000  thru vis-stt-max-des-999    .
       acc-stt-max-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-stt-max-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-stt-max-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-stt-max-100.
       acc-stt-max-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Codice statistico finale, descrizione   *
      *    *-----------------------------------------------------------*
       vis-stt-max-des-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      rr-stt-max-des       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-stt-max-des-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo selezione : Si/No stampa nome interlo- *
      *    *                                cutore                     *
      *    *-----------------------------------------------------------*
       acc-snx-int-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-snx-int-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-snx-int-lun    to   v-car                  .
           move      w-exp-snx-int-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      "NS#"                to   v-msk                  .
           move      18                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-snx-int-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-snx-int           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-snx-int-999.
       acc-snx-int-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-snx-int             .
       acc-snx-int-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-snx-int-425.
      *                  *---------------------------------------------*
      *                  * Che il valore non sia mancante, a meno che  *
      *                  * non si sia in Up                            *
      *                  *---------------------------------------------*
           if        rr-snx-int           =    zero   and
                     v-key                =    "UP  "
                     go to acc-snx-int-600.
       acc-snx-int-450.
      *                  *---------------------------------------------*
      *                  * Che il valore sia consentito                *
      *                  *---------------------------------------------*
           if        rr-snx-int           =    zero           or
                     rr-snx-int           >    w-exp-snx-int-num
                     go to acc-snx-int-100.
       acc-snx-int-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-snx-int-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-snx-int-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-snx-int-100.
       acc-snx-int-999.
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
      *              * Controlli                                       *
      *              *-------------------------------------------------*
       tdo-ric-sel-125.
      *                  *---------------------------------------------*
      *                  * Tipo ordinamento stampa                     *
      *                  *---------------------------------------------*
           if        rr-tip-ord           not  = zero
                     go to tdo-ric-sel-126.
           move      "Manca il tipo ordinamento stampa !                
      -              "               "    to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-126.
           if        rr-tip-ord           not  > w-exp-tip-ord-num
                     go to tdo-ric-sel-127.
           move      "Tipo ordinamento stampa errato !                  
      -              "               "    to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-127.
           go to     tdo-ric-sel-150.
       tdo-ric-sel-150.
      *                  *---------------------------------------------*
      *                  * Tipo etichette                              *
      *                  *---------------------------------------------*
           if        rr-tip-eti           not  = zero
                     go to tdo-ric-sel-151.
           move      "Manca il tipo di etichette !                      
      -              "               "    to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-151.
           if        rr-tip-eti           not  > w-exp-tip-eti-num
                     go to tdo-ric-sel-152.
           move      "Tipo di etichette errato !                        
      -              "               "    to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-152.
           go to     tdo-ric-sel-175.
       tdo-ric-sel-175.
      *                  *---------------------------------------------*
      *                  * Tipo di anagrafica cliente                  *
      *                  *---------------------------------------------*
           if        rr-tip-ana           not  = zero
                     go to tdo-ric-sel-176.
           move      "Manca il tipo di anagrafica cliente !             
      -              "               "    to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-176.
           if        rr-tip-eti           not  > w-exp-tip-eti-num
                     go to tdo-ric-sel-177.
           move      "Tipo di anagrafica cliente errato !               
      -              "               "    to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-177.
           go to     tdo-ric-sel-200.
       tdo-ric-sel-200.
      *                  *---------------------------------------------*
      *                  * Ragione sociale min-max                     *
      *                  *---------------------------------------------*
           if        rr-tip-ord           not  = 01
                     go to tdo-ric-sel-225.
           if        rr-rag-max           =    spaces
                     go to tdo-ric-sel-225.
           if        rr-rag-max           not  < rr-rag-min
                     go to tdo-ric-sel-225.
           move      "Ragione sociale finale minore di quella iniziale !
      -              "               "    to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-225.
      *                  *---------------------------------------------*
      *                  * Codice agente min-max                       *
      *                  *---------------------------------------------*
           if        rr-tip-ord           not  = 02
                     go to tdo-ric-sel-250.
           if        rr-age-max           =    zero
                     go to tdo-ric-sel-250.
           if        rr-age-max           not  < rr-age-min
                     go to tdo-ric-sel-250.
           move      "Codice agente finale minore di quello iniziale !  
      -              "               "    to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-250.
      *                  *---------------------------------------------*
      *                  * Codice zona min-max                         *
      *                  *---------------------------------------------*
           if        rr-tip-ord           not  = 03
                     go to tdo-ric-sel-275.
           if        rr-zon-max           =    zero
                     go to tdo-ric-sel-275.
           if        rr-zon-max           not  < rr-zon-min
                     go to tdo-ric-sel-275.
           move      "Codice zona finale minore di quello iniziale !    
      -              "               "    to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-275.
      *                  *---------------------------------------------*
      *                  * Codice categoria min-max                    *
      *                  *---------------------------------------------*
           if        rr-tip-ord           not  = 04
                     go to tdo-ric-sel-300.
           if        rr-cat-max           =    zero
                     go to tdo-ric-sel-300.
           if        rr-cat-max           not  < rr-cat-min
                     go to tdo-ric-sel-300.
           move      "Codice categoria finale minore di quello iniziale 
      -              "!              "    to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-300.
      *                  *---------------------------------------------*
      *                  * Codice statistico min-max                   *
      *                  *---------------------------------------------*
           if        rr-tip-ord           not  = 05
                     go to tdo-ric-sel-325.
           if        rr-stt-max           =    zero
                     go to tdo-ric-sel-325.
           if        rr-stt-max           not  < rr-stt-min
                     go to tdo-ric-sel-325.
           move      "Codice statistico finale minore di quello iniziale
      -              " !             "    to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-325.
      *                  *---------------------------------------------*
      *                  * Si/No stampa nome interlocutore             *
      *                  *---------------------------------------------*
           if        rr-snx-int           not  = zero
                     go to tdo-ric-sel-326.
           move      "Manca la scelta di stampa del nome interlocutore !
      -              "               "    to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-326.
           if        rr-snx-int           not  > w-exp-snx-int-num
                     go to tdo-ric-sel-327.
           move      "scelta di stampa del nome interlocutore errata !  
      -              "               "    to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-327.
           go to     tdo-ric-sel-400.
       tdo-ric-sel-400.
      *              *-------------------------------------------------*
      *              * Regolarizzazioni                                *
      *              *-------------------------------------------------*
       tdo-ric-sel-410.
      *                  *---------------------------------------------*
      *                  * Ragione sociale min-max                     *
      *                  *---------------------------------------------*
           if        rr-tip-ord           not  = 01
                     move  spaces         to   rr-rag-min
                     move  all "Z"        to   rr-rag-max
                     go to tdo-ric-sel-420.
           if        rr-rag-max           not  = spaces
                     go to tdo-ric-sel-420.
           move      rr-rag-min           to   rr-rag-max             .
           inspect   rr-rag-max      replacing all spaces by "Z"      .
       tdo-ric-sel-420.
      *                  *---------------------------------------------*
      *                  * Codice agente min-max                       *
      *                  *---------------------------------------------*
           if        rr-tip-ord           not  = 02
                     move  zero           to   rr-age-min
                     move  spaces         to   rr-age-min-des
                     move  9999999        to   rr-age-max
                     move  spaces         to   rr-age-max-des
                     go to tdo-ric-sel-430.
           if        rr-age-max           not  = zero
                     go to tdo-ric-sel-430.
           if        rr-age-min           =    zero
                     move  9999999        to   rr-age-max
           else      move  rr-age-min     to   rr-age-max             .
       tdo-ric-sel-430.
      *                  *---------------------------------------------*
      *                  * Codice zona min-max                         *
      *                  *---------------------------------------------*
           if        rr-tip-ord           not  = 03
                     move  zero           to   rr-zon-min
                     move  spaces         to   rr-zon-min-des
                     move  99999          to   rr-zon-max
                     move  spaces         to   rr-zon-max-des
                     go to tdo-ric-sel-440.
           if        rr-zon-max           not  = zero
                     go to tdo-ric-sel-440.
           if        rr-zon-min           =    zero
                     move  99999          to   rr-zon-max
           else      move  rr-zon-min     to   rr-zon-max             .
       tdo-ric-sel-440.
      *                  *---------------------------------------------*
      *                  * Codice categoria min-max                    *
      *                  *---------------------------------------------*
           if        rr-tip-ord           not  = 04
                     move  zero           to   rr-cat-min
                     move  spaces         to   rr-cat-min-des
                     move  99999          to   rr-cat-max
                     move  spaces         to   rr-cat-max-des
                     go to tdo-ric-sel-450.
           if        rr-cat-max           not  = zero
                     go to tdo-ric-sel-450.
           if        rr-cat-min           =    zero
                     move  99999          to   rr-cat-max
           else      move  rr-cat-min     to   rr-cat-max             .
       tdo-ric-sel-450.
      *                  *---------------------------------------------*
      *                  * Codice statistico min-max                   *
      *                  *---------------------------------------------*
           if        rr-tip-ord           not  = 05
                     move  zero           to   rr-stt-min
                     move  spaces         to   rr-stt-min-des
                     move  99999          to   rr-stt-max
                     move  spaces         to   rr-stt-max-des
                     go to tdo-ric-sel-460.
           if        rr-stt-max           not  = zero
                     go to tdo-ric-sel-460.
           if        rr-stt-min           =    zero
                     move  99999          to   rr-stt-max
           else      move  rr-stt-min     to   rr-stt-max             .
       tdo-ric-sel-460.
      *                  *---------------------------------------------*
      *                  * Fine regolarizzazioni                       *
      *                  *---------------------------------------------*
           go to     tdo-ric-sel-800.
       tdo-ric-sel-800.
      *              *-------------------------------------------------*
      *              * Uscita per controlli tutti superati             *
      *              *-------------------------------------------------*
           go to     tdo-ric-sel-999.
       tdo-ric-sel-900.
      *              *-------------------------------------------------*
      *              * Emissione messaggio di errore e set del flag di *
      *              * uscita ad errore                                *
      *              *-------------------------------------------------*
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                  *---------------------------------------------*
      *                  * Flag di errore in uscita                    *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cnt-tdo-ric-flg      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     tdo-ric-sel-999.
       tdo-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Regolarizzazione dei parametri di selezione               *
      *    *-----------------------------------------------------------*
       reg-ric-sel-000.
       reg-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione richieste di selezione                    *
      *    *-----------------------------------------------------------*
       nor-ric-sel-000.
      *              *-------------------------------------------------*
      *              * Tipo ordinamento                                *
      *              *-------------------------------------------------*
           move      zero                 to   rr-tip-ord             .
      *              *-------------------------------------------------*
      *              * Tipo etichette                                  *
      *              *-------------------------------------------------*
           move      zero                 to   rr-tip-eti             .
      *              *-------------------------------------------------*
      *              * Tipo anagrafica da stampare                     *
      *              *-------------------------------------------------*
           move      zero                 to   rr-tip-ana             .
      *              *-------------------------------------------------*
      *              * Valori minimi e massimi                         *
      *              *-------------------------------------------------*
           perform   nor-min-max-000      thru nor-min-max-999        .
      *              *-------------------------------------------------*
      *              * Si/No stampa nome interlocutore                 *
      *              *-------------------------------------------------*
           move      zero                 to   rr-snx-int             .
       nor-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione dati relativi ai valori minimi e massimi  *
      *    *-----------------------------------------------------------*
       nor-min-max-000.
      *              *-------------------------------------------------*
      *              * Cliente                                         *
      *              *-------------------------------------------------*
           move      spaces               to   rr-rag-min             .
           move      spaces               to   rr-rag-max             .
      *              *-------------------------------------------------*
      *              * Agente                                          *
      *              *-------------------------------------------------*
           move      zero                 to   rr-age-min             .
           move      spaces               to   rr-age-min-des         .
           move      zero                 to   rr-age-max             .
           move      spaces               to   rr-age-max-des         .
      *              *-------------------------------------------------*
      *              * Zona                                            *
      *              *-------------------------------------------------*
           move      zero                 to   rr-zon-min             .
           move      spaces               to   rr-zon-min-des         .
           move      zero                 to   rr-zon-max             .
           move      spaces               to   rr-zon-max-des         .
      *              *-------------------------------------------------*
      *              * Categoria                                       *
      *              *-------------------------------------------------*
           move      zero                 to   rr-cat-min             .
           move      spaces               to   rr-cat-min-des         .
           move      zero                 to   rr-cat-max             .
           move      spaces               to   rr-cat-max-des         .
      *              *-------------------------------------------------*
      *              * Codice statistico                               *
      *              *-------------------------------------------------*
           move      zero                 to   rr-stt-min             .
           move      spaces               to   rr-stt-min-des         .
           move      zero                 to   rr-stt-max             .
           move      spaces               to   rr-stt-max-des         .
       nor-min-max-999.
           exit.

      *    *===========================================================*
      *    * Preparazione parametri per selezione stampa               *
      *    *-----------------------------------------------------------*
       pre-prm-stp-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo di etichette    *
      *              *-------------------------------------------------*
           if        rr-tip-eti           =    01
                     perform  pre-eti-001-000
                                          thru pre-eti-001-999
           else if   rr-tip-eti           =    02
                     perform  pre-eti-002-000
                                          thru pre-eti-002-999
           else      perform  pre-eti-001-000
                                          thru pre-eti-001-999        .
       pre-prm-stp-999.
           exit.

      *    *===========================================================*
      *    * Preparazione parametri per stampa etichette tipo 01       *
      *    *-----------------------------------------------------------*
      *    *                                                           *
      *    *  - Modulo stampa  : continuo                              *
      *    *  - Lunghezza mod. : 12''          (864 72.mi)             *
      *    *  - Larghezza mod. : 4''           (288 72.mi)             *
      *    *  - Numero colonne : 01                                    *
      *    *  - Lunghezza  mm. : 100                                   *
      *    *  - Altezza    mm. : 036                                   *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       pre-eti-001-000.
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
           move      "eticli  "           to   w-cnt-stp-cod-mod      .
      *              *-------------------------------------------------*
      *              * Tipo modulo                                     *
      *              *   - L : Libero                                  *
      *              *   - T : Tipografico                             *
      *              *-------------------------------------------------*
           move      "T"                  to   w-cnt-stp-tip-mod      .
      *              *-------------------------------------------------*
      *              * Ampiezza linea di stampa in caratteri           *
      *              *-------------------------------------------------*
           move      48                   to   w-cnt-stp-amp-lin      .
      *              *-------------------------------------------------*
      *              * Top margin in linee                             *
      *              *-------------------------------------------------*
           move      0                    to   w-cnt-stp-top-lin      .
      *              *-------------------------------------------------*
      *              * Numero linee di stampa minimo                   *
      *              *-------------------------------------------------*
           move      72                   to   w-cnt-stp-lin-min      .
      *              *-------------------------------------------------*
      *              * Bottom margin in linee                          *
      *              *-------------------------------------------------*
           move      0                    to   w-cnt-stp-bot-lin      .
      *              *-------------------------------------------------*
      *              * Ampiezza caratteri                              *
      *              *-------------------------------------------------*
           move      12,00                to   w-cnt-stp-amp-car      .
      *              *-------------------------------------------------*
      *              * Altezza interlinea                              *
      *              *-------------------------------------------------*
           move      06,00                to   w-cnt-stp-alt-int      .
      *              *-------------------------------------------------*
      *              * Area riservata per espansioni future            *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-stp-esp-fut      .
      *              *-------------------------------------------------*
      *              * Area riservata per espansioni speciali          *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-stp-fnz-spc      .
       pre-eti-001-999.
           exit.

      *    *===========================================================*
      *    * Preparazione parametri per stampa etichette tipo 02       *
      *    *-----------------------------------------------------------*
      *    *                                                           *
      *    *  - Modulo stampa  : Laser A4                              *
      *    *  - Lunghezza mod. :                                       *
      *    *  - Larghezza mod. :                                       *
      *    *  - Numero colonne : 03                                    *
      *    *  - Lunghezza  mm. : 070                                   *
      *    *  - Altezza    mm. : 036                                   *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       pre-eti-002-000.
      *              *-------------------------------------------------*
      *              * Flags di tipo selezione                         *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-stp-tip-sel      .
      *              *-------------------------------------------------*
      *              * Codice stampante                                *
      *              *-------------------------------------------------*
           move      "etilaser"           to   w-cnt-stp-cod-stp      .
      *              *-------------------------------------------------*
      *              * Tipo di stampa                                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-stp-tip-sta      .
      *              *-------------------------------------------------*
      *              * Codice modulo                                   *
      *              *-------------------------------------------------*
           move      "etilaser"           to   w-cnt-stp-cod-mod      .
      *              *-------------------------------------------------*
      *              * Tipo modulo                                     *
      *              *   - L : Libero                                  *
      *              *   - T : Tipografico                             *
      *              *-------------------------------------------------*
           move      "T"                  to   w-cnt-stp-tip-mod      .
      *              *-------------------------------------------------*
      *              * Ampiezza linea di stampa in caratteri           *
      *              *-------------------------------------------------*
           move      132                  to   w-cnt-stp-amp-lin      .
      *              *-------------------------------------------------*
      *              * Top margin in linee                             *
      *              *-------------------------------------------------*
           move      0                    to   w-cnt-stp-top-lin      .
      *              *-------------------------------------------------*
      *              * Numero linee di stampa minimo                   *
      *              *-------------------------------------------------*
           move      72                   to   w-cnt-stp-lin-min      .
      *              *-------------------------------------------------*
      *              * Bottom margin in linee                          *
      *              *-------------------------------------------------*
           move      0                    to   w-cnt-stp-bot-lin      .
      *              *-------------------------------------------------*
      *              * Ampiezza caratteri                              *
      *              *-------------------------------------------------*
           move      20,00                to   w-cnt-stp-amp-car      .
      *              *-------------------------------------------------*
      *              * Altezza interlinea                              *
      *              *-------------------------------------------------*
           move      06,00                to   w-cnt-stp-alt-int      .
      *              *-------------------------------------------------*
      *              * Area riservata per espansioni future            *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-stp-esp-fut      .
      *              *-------------------------------------------------*
      *              * Area riservata per espansioni speciali          *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-stp-fnz-spc      .
       pre-eti-002-999.
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
      *    * Routine di lettura archivio [age]                         *
      *    *-----------------------------------------------------------*
       let-arc-age-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-age-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice a zero                           *
      *              *-------------------------------------------------*
           if        w-let-arc-age-cod    =    zero
                     go to let-arc-age-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODAGE"             to   f-key                  .
           move      w-let-arc-age-cod    to   rf-age-cod-age         .
           move      "pgm/age/fls/ioc/obj/iofage"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-age                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-age-400.
       let-arc-age-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-age-nom-age       to   w-let-arc-age-nom      .
           move      rf-age-sup-age       to   w-let-arc-age-sup      .
           move      rf-age-cat-pvg       to   w-let-arc-age-cat      .
           move      rf-age-per-pvg (1)   to   w-let-arc-age-per (1)  .
           move      rf-age-per-pvg (2)   to   w-let-arc-age-per (2)  .
           move      rf-age-per-pvg (3)   to   w-let-arc-age-per (3)  .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-age-999.
       let-arc-age-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-age-flg      .
           move      all   "."            to   w-let-arc-age-nom      .
           go to     let-arc-age-600.
       let-arc-age-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-age-nom      .
       let-arc-age-600.
           move      zero                 to   w-let-arc-age-sup      .
           move      zero                 to   w-let-arc-age-cat      .
           move      zero                 to   w-let-arc-age-per (1)  .
           move      zero                 to   w-let-arc-age-per (2)  .
           move      zero                 to   w-let-arc-age-per (3)  .
       let-arc-age-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [zst]                         *
      *    *-----------------------------------------------------------*
       let-arc-zst-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zst-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice sconto a zero                    *
      *              *-------------------------------------------------*
           if        w-let-arc-zst-cod    =    zero
                     go to let-arc-zst-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCLS    "         to   f-key                  .
           move      w-let-arc-zst-tip    to   rf-zst-tip-cls         .
           move      w-let-arc-zst-cod    to   rf-zst-cod-cls         .
           move      "pgm/dcc/fls/ioc/obj/iofzst"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zst                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-zst-400.
       let-arc-zst-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-zst-des-cls       to   w-let-arc-zst-des      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-zst-999.
       let-arc-zst-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-zst-flg      .
           move      all   "."            to   w-let-arc-zst-des      .
           go to     let-arc-zst-999.
       let-arc-zst-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zst-des      .
       let-arc-zst-999.
           exit.

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice agente          *
      *    *-----------------------------------------------------------*
           copy      "pgm/age/prg/cpy/acmnage0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice zona            *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcc/prg/cpy/acmnzst1.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice categoria       *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcc/prg/cpy/acmnzst2.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice statistico      *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcc/prg/cpy/acmnzst3.acs"                   .
