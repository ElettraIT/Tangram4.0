       Identification Division.
       Program-Id.                                 page3000           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    age                 *
      *                                Settore:    con                 *
      *                                   Fase:    age300              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 01/06/94    *
      *                       Ultima revisione:    NdK del 30/04/10    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Generazione automatica dei conteggi provvi- *
      *                    gionali a fronte delle fatture emesse.      *
      *                                                                *
      *                    Main program e richieste                    *
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
                     "age"                                            .
      *        *-------------------------------------------------------*
      *        * Settore gestionale                                    *
      *        *-------------------------------------------------------*
           05  i-ide-set                  pic  x(03) value
                     "con"                                            .
      *        *-------------------------------------------------------*
      *        * Fase gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-fas                  pic  x(06) value
                     "age300"                                         .
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10) value
                     "page3000"                                       .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  i-ide-des                  pic  x(40) value
                     "   GENERAZIONE CONTEGGI PROVVIGIONALI   "       .

      *    *===========================================================*
      *    * Area per il programma di esecuzione                       *
      *    *-----------------------------------------------------------*
       01  i-exe.
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma di esecuzione             *
      *        *-------------------------------------------------------*
           05  i-exe-pro                  pic  x(10) value
                     "page300?  "                                     .
      *        *-------------------------------------------------------*
      *        * Pathname del programma di esecuzione                  *
      *        *-------------------------------------------------------*
           05  i-exe-pat                  pic  x(40) value
                     "pgm/age/prg/obj/page300?                "       .

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
      *        * [age]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/age/fls/rec/rfage"                          .
      *        *-------------------------------------------------------*
      *        * [zfi]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/fat/fls/rec/rfzfi"                          .

      *    *===========================================================*
      *    * Work-area richieste                                       *
      *    *-----------------------------------------------------------*
       01  rr.
      *        *-------------------------------------------------------*
      *        * Tipo di esecuzione                                    *
      *        *                                                       *
      *        *  - 01 : Solo simulazione e stampa                     *
      *        *  - 02 : Solo generazione effettiva dei conteggi       *
      *        *  - 03 : Generazione effettiva dei conteggi + stampa   *
      *        *-------------------------------------------------------*
           05  rr-tip-exe                 pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Codice agente min                                     *
      *        *-------------------------------------------------------*
           05  rr-age-min                 pic  9(07)                  .
           05  rr-age-min-nom             pic  x(20)                  .
      *        *-------------------------------------------------------*
      *        * Codice agente max                                     *
      *        *-------------------------------------------------------*
           05  rr-age-max                 pic  9(07)                  .
           05  rr-age-max-nom             pic  x(20)                  .
      *        *-------------------------------------------------------*
      *        * Tipo di ordinamento per agenti                        *
      *        *                                                       *
      *        *  - 01 : Ordinamento per nominativo agente             *
      *        *  - 02 : Ordinamento per codice agente                 *
      *        *  - 03 : Ordinamento per mnemonico agente              *
      *        *                                                       *
      *        * Non significativo se selezionato un solo agente       *
      *        *-------------------------------------------------------*
           05  rr-tor-age                 pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Data minima dei documenti relativi ai documenti fat-  *
      *        * tura su cui eseguire i nuovi conteggi                 *
      *        *-------------------------------------------------------*
           05  rr-dat-min                 pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Data massima dei documenti relativi ai documenti fat- *
      *        * tura su cui eseguire i nuovi conteggi                 *
      *        *-------------------------------------------------------*
           05  rr-dat-max                 pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Tipo documento fattura                                *
      *        *-------------------------------------------------------*
           05  rr-cod-zfi                 pic  x(05)                  .
           05  rr-cod-zfi-des             pic  x(30)                  .
      *        *-------------------------------------------------------*
      *        * Tipo di vendita per l'agente                          *
      *        *                                                       *
      *        *  - 01 : Nessuna selezione                             *
      *        *  - 02 : Solo vendite dirette                          *
      *        *  - 03 : Solo vendite indirette                        *
      *        *-------------------------------------------------------*
           05  rr-tip-vpa                 pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Tipo esecuzione programma, classe                     *
      *        *-------------------------------------------------------*
           05  rr-age-300-ctf             pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Tipo esecuzione programma, sottoclasse                *
      *        *-------------------------------------------------------*
           05  rr-age-300-stf             pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Tipo esecuzione programma, si/no abbattimento prov-   *
      *        * vigioni in riga                                       *
      *        *-------------------------------------------------------*
           05  rr-age-300-abr             pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Tipo esecuzione programma, si/no abbattimento su to-  *
      *        * tale provvigioni                                      *
      *        *-------------------------------------------------------*
           05  rr-age-300-abt             pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Tipo esecuzione programma, si/no stampa               *
      *        *-------------------------------------------------------*
           05  rr-age-300-sns             pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Tipo esecuzione programma, ampiezza pagina stampa     *
      *        *-------------------------------------------------------*
           05  rr-age-300-aps             pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Tipo esecuzione programma, tipo maturazione           *
      *        *-------------------------------------------------------*
           05  rr-age-300-tma             pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Tipo esecuzione programma, tipo data maturazione      *
      *        *-------------------------------------------------------*
           05  rr-age-300-tdm             pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Opzioni di stampa per anomalie                        *
      *        *-------------------------------------------------------*
           05  rr-age-300-ops             pic  x(10)                  .
      *        *-------------------------------------------------------*
      *        * Tipo esecuzione programma, flags per elaborazione     *
      *        *                                                       *
      *        *  - (1) : spaces = non significativo                   *
      *        *  - (1) : '*'    = stampa la riga anche se non con-    *
      *        *                   tribuisce al conteggio              *
      *        *                                                       *
      *        *  - (2) : spaces = non significativo                   *
      *        *  - (2) : '*'    = stampa il riferimento all'ordine    *
      *        *                   cliente nel campo note in riga      *
      *        *                                                       *
      *        *  - (3) : spaces = non significativo                   *
      *        *  - (3) : '*'    = Imponibile provvigionale abbattuto  *
      *        *                   dallo sconto incondizionato         *
      *        *                                                       *
      *        *  - (4) : spaces = non significativo                   *
      *        *  - (4) : '*'    = stampa il documento anche se non    *
      *        *                   contribuisce al conteggio           *
      *        *                                                       *
      *        *  - (5) : spaces = non significativo                   *
      *        *  - (5) : '*'    = stampa delta % tra lordo standard   *
      *        *                   e imponibile provvigionale totali   *
      *        *                                                       *
      *        *-------------------------------------------------------*
           05  rr-age-300-fpe             pic  x(05)                  .
      *        *-------------------------------------------------------*
      *        * Tipo esecuzione programma, parametro per elaborazione *
      *        *                                                       *
      *        *  - (1) : spaces = non significativo                   *
      *        *  - (1) : 'C'    = stampa del codice statistico 1      *
      *        *                   insieme al codice prodotto          *
      *        *  - (1) : 'N'    = stampa del codice statistico 1      *
      *        *                   nelle note in riga                  *
      *        *  - (1) : 'M'    = stampa mnemonico codice stat. 1     *
      *        *                   insieme al codice prodotto          *
      *        *  - (1) : 'X'    = stampa mnemonico codice stat. 1     *
      *        *                   nelle note in riga                  *
      *        *                                                       *
      *        *  - (2) : spaces = non significativo                   *
      *        *  - (2) : 'C'    = stampa del codice statistico 2      *
      *        *                   insieme al codice prodotto          *
      *        *  - (2) : 'N'    = stampa del codice statistico 2      *
      *        *                   nelle note in riga                  *
      *        *  - (2) : 'M'    = stampa mnemonico codice stat. 2     *
      *        *                   insieme al codice prodotto          *
      *        *  - (2) : 'X'    = stampa mnemonico codice stat. 2     *
      *        *                   nelle note in riga                  *
      *        *                                                       *
      *        *  - (3) : spaces = non significativo                   *
      *        *  - (3) : 'C'    = stampa del codice statistico 3      *
      *        *                   insieme al codice prodotto          *
      *        *  - (3) : 'N'    = stampa del codice statistico 3      *
      *        *                   nelle note in riga                  *
      *        *  - (3) : 'M'    = stampa mnemonico codice stat. 3     *
      *        *                   insieme al codice prodotto          *
      *        *  - (3) : 'X'    = stampa mnemonico codice stat. 3     *
      *        *                   nelle note in riga                  *
      *        *                                                       *
      *        *  - (4..10)      = liberi                              *
      *        *-------------------------------------------------------*
           05  rr-age-300-ppe             pic  x(10)                  .

      *    *===========================================================*
      *    * Work per personalizzazione 'pgm/age/age300[tip-fun]'      *
      *    *-----------------------------------------------------------*
       01  w-prs-age-300.
      *        *-------------------------------------------------------*
      *        * Classe di tipo funzionamento, ovvero suffisso per la  *
      *        * overlay 'page300X', a..z                              *
      *        *-------------------------------------------------------*
           05  w-prs-age-300-ctf          pic  x(01)                  .
           05  filler                     pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Sottoclasse di tipo funzionamento                     *
      *        *-------------------------------------------------------*
           05  w-prs-age-300-stf          pic  9(03)                  .
           05  filler                     pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Si/No abbattimento provvigioni in riga                *
      *        *-------------------------------------------------------*
           05  w-prs-age-300-abr          pic  x(01)                  .
           05  filler                     pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Si/No abbattimento su totale provvigioni              *
      *        *-------------------------------------------------------*
           05  w-prs-age-300-abt          pic  x(01)                  .
           05  filler                     pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Si/No stampa di documentazione                        *
      *        *-------------------------------------------------------*
           05  w-prs-age-300-sns          pic  x(01)                  .
           05  filler                     pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Ampiezza pagina per stampa                            *
      *        *-------------------------------------------------------*
           05  w-prs-age-300-aps          pic  9(03)                  .
           05  filler                     pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Tipo maturazione prevista                             *
      *        *-------------------------------------------------------*
           05  w-prs-age-300-tma          pic  9(02)                  .
           05  filler                     pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Tipo data di maturazione minima                       *
      *        *-------------------------------------------------------*
           05  w-prs-age-300-tdm          pic  9(02)                  .
           05  filler                     pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Opzioni di stampa anomalie                            *
      *        *-------------------------------------------------------*
           05  w-prs-age-300-ops          pic  x(10)                  .
           05  filler                     pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Flags per l'elaborazione                              *
      *        *-------------------------------------------------------*
           05  w-prs-age-300-fpe          pic  x(05)                  .
           05  filler                     pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Parametro per l'elaborazione                          *
      *        *-------------------------------------------------------*
           05  w-prs-age-300-ppe          pic  x(10)                  .

      *    *===========================================================*
      *    * Work per subroutines di Let                               *
      *    *-----------------------------------------------------------*
       01  w-let.
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio anagrafica agenti            *
      *        *-------------------------------------------------------*
           05  w-let-arc-age.
               10  w-let-arc-age-flg      pic  x(01)     value spaces .
               10  w-let-arc-age-cod      pic  9(07)     value zero   .
               10  w-let-arc-age-nom      pic  x(20)     value spaces .
               10  w-let-arc-age-rag      pic  x(40)     value spaces .
               10  w-let-arc-age-via      pic  x(40)     value spaces .
               10  w-let-arc-age-loc      pic  x(40)     value spaces .
               10  w-let-arc-age-mne      pic  x(10)     value spaces .
               10  w-let-arc-age-exc      pic  9(07)     value zero   .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [zfi]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-zfi.
               10  w-let-arc-zfi-flg      pic  x(01)     value spaces .
               10  w-let-arc-zfi-cod      pic  x(05)     value spaces .
               10  w-let-arc-zfi-des      pic  x(30)     value spaces .
               10  w-let-arc-zfi-exc      pic  x(05)     value spaces .

      *    *===========================================================*
      *    * Work per accettazioni campi espansi                       *
      *    *-----------------------------------------------------------*
       01  w-exp.
      *        *-------------------------------------------------------*
      *        * Work per : Tipo di conteggio                          *
      *        *-------------------------------------------------------*
           05  w-exp-tip-exe.
               10  w-exp-tip-exe-num      pic  9(02)       value 03   .
               10  w-exp-tip-exe-lun      pic  9(02)       value 50   .
               10  w-exp-tip-exe-tbl.
                   15  filler             pic  x(50) value
                  "solo Simulazione e stampa                         ".
                   15  filler             pic  x(50) value
                  "solo Generazione effettiva dei conteggi           ".
                   15  filler             pic  x(50) value
                  "generazione Effettiva dei conteggi e stampa       ".
      *        *-------------------------------------------------------*
      *        * Work per : Tipo ordinamento agenti                    *
      *        *-------------------------------------------------------*
           05  w-exp-tor-age.
               10  w-exp-tor-age-num      pic  9(02)       value 03   .
               10  w-exp-tor-age-lun      pic  9(02)       value 50   .
               10  w-exp-tor-age-tbl.
                   15  filler             pic  x(50) value
                  "per Nominativo agente                             ".
                   15  filler             pic  x(50) value
                  "per Codice agente                                 ".
                   15  filler             pic  x(50) value
                  "per Mnemonico agente                              ".
      *        *-------------------------------------------------------*
      *        * Work per : Tipo vendita per l'agente                  *
      *        *-------------------------------------------------------*
           05  w-exp-tip-vpa.
               10  w-exp-tip-vpa-num      pic  9(02)       value 03   .
               10  w-exp-tip-vpa-lun      pic  9(02)       value 50   .
               10  w-exp-tip-vpa-tbl.
                   15  filler             pic  x(50) value
                  "Nessuna selezione                                 ".
                   15  filler             pic  x(50) value
                  "solo vendite Dirette                              ".
                   15  filler             pic  x(50) value
                  "solo vendite Indirette                            ".

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
      *    * Link-area per accettazione codice agente                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/age/prg/cpy/acmnage0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione tipo movimento fatturazione    *
      *    *-----------------------------------------------------------*
           copy      "pgm/fat/prg/cpy/acdezfi0.acl"                   .

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
      *              *-------------------------------------------------*
      *              * Flag di eventuale visualizzazione forzata       *
      *              *-------------------------------------------------*
           move      s-sts                to   w-cnt-mfu-vis-sgr      .
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
       pre-exe-pgm-010.
      *              *-------------------------------------------------*
      *              * Test se programma eseguibile                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su flag di eventuale visualizzazione   *
      *                  *---------------------------------------------*
           if        w-cnt-mfu-vis-sgr    not  = "V"
                     go to pre-exe-pgm-100.
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
       pre-exe-pgm-100.
      *              *-------------------------------------------------*
      *              * Lettura della personalizzazione per il tipo di  *
      *              * funzionamento previsto per il programma         *
      *              *-------------------------------------------------*
           perform   prs-age-300-000      thru prs-age-300-999        .
      *              *-------------------------------------------------*
      *              * Preparazione name e pathname del programma di   *
      *              * esecuzione vera e propria                       *
      *              *-------------------------------------------------*
           perform   nam-pat-exe-000      thru nam-pat-exe-999        .
       pre-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Lettura della personalizzazione per il trattamento del    *
      *    * tipo di funzionamento del programma 'age300'              *
      *    *-----------------------------------------------------------*
       prs-age-300-000.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione                       *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/age/age300[tip-fun]"
                                          to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Deviazione a seconda dell'esito della lettura   *
      *              *-------------------------------------------------*
           if        s-ves                =    spaces
                     go to prs-age-300-300.
       prs-age-300-100.
      *              *-------------------------------------------------*
      *              * Se personalizzazione non esistente              *
      *              *-------------------------------------------------*
       prs-age-300-150.
      *                  *---------------------------------------------*
      *                  * Normalizzazione parametri                   *
      *                  *---------------------------------------------*
           move      "a"                  to   w-prs-age-300-ctf      .
           move      001                  to   w-prs-age-300-stf      .
           move      "N"                  to   w-prs-age-300-abr      .
           move      "N"                  to   w-prs-age-300-abt      .
           move      "S"                  to   w-prs-age-300-sns      .
           move      132                  to   w-prs-age-300-aps      .
           move      01                   to   w-prs-age-300-tma      .
           move      01                   to   w-prs-age-300-tdm      .
           move      "X         "         to   w-prs-age-300-ops      .
           move      spaces               to   w-prs-age-300-fpe      .
           move      spaces               to   w-prs-age-300-ppe      .
       prs-age-300-200.
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     prs-age-300-900.
       prs-age-300-300.
      *              *-------------------------------------------------*
      *              * Se personalizzazione esistente                  *
      *              *-------------------------------------------------*
       prs-age-300-350.
      *                  *---------------------------------------------*
      *                  * Spostamento valore personalizzazione in a-  *
      *                  * rea di destinazione                         *
      *                  *---------------------------------------------*
           move      s-alf                to   w-prs-age-300          .
       prs-age-300-400.
      *                  *---------------------------------------------*
      *                  * Controllo parametri                         *
      *                  *---------------------------------------------*
           if        w-prs-age-300-ctf    <    "a" or
                     w-prs-age-300-ctf    >    "z"
                     move  "a"            to   w-prs-age-300-ctf      .
      *
           if        w-prs-age-300-stf    not  numeric
                     move  001            to   w-prs-age-300-stf      .
           if        w-prs-age-300-stf    <    001 or
                     w-prs-age-300-stf    >    999
                     move  001            to   w-prs-age-300-stf      .
      *
           if        w-prs-age-300-abr    not  = "S" and
                     w-prs-age-300-abr    not  = "N"
                     move  "N"            to   w-prs-age-300-abr      .
      *
           if        w-prs-age-300-abt    not  = "S" and
                     w-prs-age-300-abt    not  = "-" and
                     w-prs-age-300-abt    not  = "N"
                     move  "N"            to   w-prs-age-300-abt      .
      *
           if        w-prs-age-300-stf    >    500   and
                     w-prs-age-300-sns    =    "T"
                     move  "S"            to   w-prs-age-300-sns      .
           if        w-prs-age-300-sns    not  = "S" and
                     w-prs-age-300-sns    not  = "T" and
                     w-prs-age-300-sns    not  = "N"
                     move  "S"            to   w-prs-age-300-sns      .
      *
           if        w-prs-age-300-aps    not  numeric
                     move  000            to   w-prs-age-300-aps      .
           if        w-prs-age-300-aps    <    080
                     move  080            to   w-prs-age-300-aps      .
           if        w-prs-age-300-aps    >    240
                     move  240            to   w-prs-age-300-aps      .
      *
           if        w-prs-age-300-tma    not  numeric
                     move  01             to   w-prs-age-300-tma      .
           if        w-prs-age-300-tma    not  = 01  and
                     w-prs-age-300-tma    not  = 02  and
                     w-prs-age-300-tma    not  = 03
                     move  01             to   w-prs-age-300-tma      .
      *
           if        w-prs-age-300-tdm    not  numeric
                     move  01             to   w-prs-age-300-tdm      .
           if        w-prs-age-300-tdm    not  = 01
                     move  01             to   w-prs-age-300-tdm      .
      *
           if        w-prs-age-300-ops    =    spaces
                     move  "X         "   to   w-prs-age-300-ops      .
       prs-age-300-450.
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     prs-age-300-900.
       prs-age-300-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     prs-age-300-999.
       prs-age-300-999.
           exit.

      *    *===========================================================*
      *    * Preparazione name e pathname del programma di esecuzione  *
      *    * vera e propria                                            *
      *    *-----------------------------------------------------------*
       nam-pat-exe-000.
      *              *-------------------------------------------------*
      *              * Name                                            *
      *              *-------------------------------------------------*
           inspect   i-exe-pro replacing  all  "?"
                                          by   w-prs-age-300-ctf      .
      *              *-------------------------------------------------*
      *              * Pathname                                        *
      *              *-------------------------------------------------*
           inspect   i-exe-pat replacing  all  "?"
                                          by   w-prs-age-300-ctf      .
      *              *-------------------------------------------------*
      *              * Filtro del pathname                             *
      *              *-------------------------------------------------*
           move      i-exe-pat            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           move      s-pat                to   i-exe-pat              .
       nam-pat-exe-999.
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
      *              * Open modulo accettazione tipo movimento         *
      *              *-------------------------------------------------*
           perform   cod-des-zfi-opn-000  thru cod-des-zfi-opn-999    .
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
      *              * [zfi]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/fat/fls/ioc/obj/iofzfi"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zfi                 .
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
      *              * Close modulo accettazione tipo movimento        *
      *              *-------------------------------------------------*
           perform   cod-des-zfi-cls-000  thru cod-des-zfi-cls-999    .
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
      *              * [zfi]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/fat/fls/ioc/obj/iofzfi"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zfi                 .
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
      *                  * Tipo di esecuzione                          *
      *                  *---------------------------------------------*
           perform   acc-tip-exe-000      thru acc-tip-exe-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
       acc-ric-sel-200.
      *                  *---------------------------------------------*
      *                  * Codice agente min                           *
      *                  *---------------------------------------------*
           perform   acc-age-min-000      thru acc-age-min-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-100.
       acc-ric-sel-250.
      *                  *---------------------------------------------*
      *                  * Codice agente max                           *
      *                  *---------------------------------------------*
           perform   acc-age-max-000      thru acc-age-max-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-200.
       acc-ric-sel-300.
      *                  *---------------------------------------------*
      *                  * Tipo di ordinamento per agenti              *
      *                  *---------------------------------------------*
           perform   acc-tor-age-000      thru acc-tor-age-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-250.
       acc-ric-sel-400.
      *                  *---------------------------------------------*
      *                  * Data minima dei documenti                   *
      *                  *---------------------------------------------*
           perform   acc-dat-min-000      thru acc-dat-min-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-300.
       acc-ric-sel-500.
      *                  *---------------------------------------------*
      *                  * Data massima dei documenti                  *
      *                  *---------------------------------------------*
           perform   acc-dat-max-000      thru acc-dat-max-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-400.
       acc-ric-sel-600.
      *                  *---------------------------------------------*
      *                  * Tipo movimento fattura                      *
      *                  *---------------------------------------------*
           perform   acc-cod-zfi-000      thru acc-cod-zfi-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-500.
       acc-ric-sel-700.
      *                  *---------------------------------------------*
      *                  * Tipo vendita                                *
      *                  *---------------------------------------------*
           perform   acc-tip-vpa-000      thru acc-tip-vpa-999        .
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
      *              * Tipo di esecuzione                              *
      *              *-------------------------------------------------*
           perform   pmt-tip-exe-000      thru pmt-tip-exe-999        .
      *              *-------------------------------------------------*
      *              * Codice agente min                               *
      *              *-------------------------------------------------*
           perform   pmt-age-min-000      thru pmt-age-min-999        .
      *              *-------------------------------------------------*
      *              * Codice agente max                               *
      *              *-------------------------------------------------*
           perform   pmt-age-max-000      thru pmt-age-max-999        .
      *              *-------------------------------------------------*
      *              * Tipo di ordinamento per agenti                  *
      *              *-------------------------------------------------*
           perform   pmt-tor-age-000      thru pmt-tor-age-999        .
      *              *-------------------------------------------------*
      *              * Data minima dei documenti                       *
      *              *-------------------------------------------------*
           perform   pmt-dat-min-000      thru pmt-dat-min-999        .
      *              *-------------------------------------------------*
      *              * Data massima dei documenti                      *
      *              *-------------------------------------------------*
           perform   pmt-dat-max-000      thru pmt-dat-max-999        .
      *              *-------------------------------------------------*
      *              * Tipo movimento fattura                          *
      *              *-------------------------------------------------*
           perform   pmt-cod-zfi-000      thru pmt-cod-zfi-999        .
      *              *-------------------------------------------------*
      *              * Tipo vendita                                    *
      *              *-------------------------------------------------*
           perform   pmt-tip-vpa-000      thru pmt-tip-vpa-999        .
       pmt-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Tipo di esecuzione                               *
      *    *-----------------------------------------------------------*
       pmt-tip-exe-000.
           if        w-prs-age-300-sns    not  = "S" and
                     w-prs-age-300-sns    not  = "T"
                     go to pmt-tip-exe-999.
       pmt-tip-exe-100.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tipo di esecuzione         :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-tip-exe-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Codice agente min                                *
      *    *-----------------------------------------------------------*
       pmt-age-min-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      08                   to   v-lin                  .
           if        w-prs-age-300-sns    not  = "S" and
                     w-prs-age-300-sns    not  = "T"
                     subtract  3          from v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice agente iniziale     :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-age-min-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Codice agente max                                *
      *    *-----------------------------------------------------------*
       pmt-age-max-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      09                   to   v-lin                  .
           if        w-prs-age-300-sns    not  = "S" and
                     w-prs-age-300-sns    not  = "T"
                     subtract  3          from v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice agente finale       :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-age-max-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Tipo di ordinamento per agenti                   *
      *    *-----------------------------------------------------------*
       pmt-tor-age-000.
           if        w-prs-age-300-sns    not  = "S" and
                     w-prs-age-300-sns    not  = "T"
                     go to pmt-tor-age-999.
       pmt-tor-age-100.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      11                   to   v-lin                  .
           if        w-prs-age-300-sns    not  = "S" and
                     w-prs-age-300-sns    not  = "T"
                     subtract  3          from v-lin                  .
           move      01                   to   v-pos                  .
           move      "Ordinamento agenti         :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-tor-age-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Data minima dei documenti                        *
      *    *-----------------------------------------------------------*
       pmt-dat-min-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      66                   to   v-car                  .
           move      13                   to   v-lin                  .
           if        w-prs-age-300-sns    not  = "S" and
                     w-prs-age-300-sns    not  = "T"
                     subtract  5          from v-lin                  .
           move      01                   to   v-pos                  .
           move      "Data documento minima  delle fatture per cui gener
      -              "are i conteggi :"   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-dat-min-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Data massima dei documenti                       *
      *    *-----------------------------------------------------------*
       pmt-dat-max-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      66                   to   v-car                  .
           move      15                   to   v-lin                  .
           if        w-prs-age-300-sns    not  = "S" and
                     w-prs-age-300-sns    not  = "T"
                     subtract  5          from v-lin                  .
           move      01                   to   v-pos                  .
           move      "Data documento massima delle fatture per cui gener
      -              "are i conteggi :"   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-dat-max-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Tipo movimento fattura                           *
      *    *-----------------------------------------------------------*
       pmt-cod-zfi-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      17                   to   v-lin                  .
           if        w-prs-age-300-sns    not  = "S" and
                     w-prs-age-300-sns    not  = "T"
                     subtract  5          from v-lin                  .
           move      01                   to   v-pos                  .
           move      "Selezioni opzionali su fatture"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      19                   to   v-lin                  .
           if        w-prs-age-300-sns    not  = "S" and
                     w-prs-age-300-sns    not  = "T"
                     subtract  5          from v-lin                  .
           move      01                   to   v-pos                  .
           move      " - Tipo documento          :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cod-zfi-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Tipo vendita                                     *
      *    *-----------------------------------------------------------*
       pmt-tip-vpa-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      20                   to   v-lin                  .
           if        w-prs-age-300-sns    not  = "S" and
                     w-prs-age-300-sns    not  = "T"
                     subtract  5          from v-lin                  .
           move      01                   to   v-pos                  .
           move      " - Tipo di vendita         :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-tip-vpa-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Tipo di esecuzione                         *
      *    *-----------------------------------------------------------*
       acc-tip-exe-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-tip-exe-025.
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-prs-age-300-sns    not  = "S" and
                     w-prs-age-300-sns    not  = "T"
                     go to acc-tip-exe-999.
       acc-tip-exe-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-exe-lun    to   v-car                  .
           move      w-exp-tip-exe-num    to   v-ldt                  .
           move      "SGE#"               to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      w-exp-tip-exe-tbl    to   v-txt                  .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      05                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-tip-exe           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-tip-exe-999.
       acc-tip-exe-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-tip-exe             .
       acc-tip-exe-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-tip-exe-410.
      *                  *---------------------------------------------*
      *                  * Che sia un valore previsto                  *
      *                  *---------------------------------------------*
           if        rr-tip-exe           not  = 01 and
                     rr-tip-exe           not  = 02 and
                     rr-tip-exe           not  = 03
                     go to acc-tip-exe-100.
       acc-tip-exe-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-tip-exe-625.
      *                  *---------------------------------------------*
      *                  * Se non si tratta di una simulazione si nor- *
      *                  * malizza la data minima dei documenti        *
      *                  *---------------------------------------------*
           if        rr-tip-exe           =    01
                     go to acc-tip-exe-650.
           move      zero                 to   rr-dat-min             .
           perform   vis-dat-min-000      thru vis-dat-min-999        .
       acc-tip-exe-650.
      *                  *---------------------------------------------*
      *                  * Se si tratta di sola generazione si norma-  *
      *                  * lizza il tipo ordinamento agenti            *
      *                  *---------------------------------------------*
           if        rr-tip-exe           not  = 02
                     go to acc-tip-exe-675.
           move      zero                 to   rr-tor-age             .
           perform   vis-tor-age-000      thru vis-tor-age-999        .
       acc-tip-exe-675.
      *                  *---------------------------------------------*
      *                  * Fine dipendenze da impostazione             *
      *                  *---------------------------------------------*
           go to     acc-tip-exe-800.
       acc-tip-exe-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-tip-exe-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-tip-exe-100.
       acc-tip-exe-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Codice agente min                          *
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
           move      08                   to   w-cod-mne-age-lin      .
           if        w-prs-age-300-sns    not  = "S" and
                     w-prs-age-300-sns    not  = "T"
                     subtract  3          from w-cod-mne-age-lin      .
           move      30                   to   w-cod-mne-age-pos      .
           move      08                   to   w-cod-mne-age-nln      .
           if        w-prs-age-300-sns    not  = "S" and
                     w-prs-age-300-sns    not  = "T"
                     subtract  3          from w-cod-mne-age-nln      .
           move      41                   to   w-cod-mne-age-nps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
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
       acc-age-min-150.
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
      *                  * Memorizzazione descrizione                  *
      *                  *---------------------------------------------*
           move      w-let-arc-age-nom    to   rr-age-min-nom         .
      *                  *---------------------------------------------*
      *                  * Visualizzazione nominativo agente           *
      *                  *---------------------------------------------*
           perform   vis-age-min-nom-000  thru vis-age-min-nom-999    .
      *                  *---------------------------------------------*
      *                  * Se codice errato : reimpostazione           *
      *                  *---------------------------------------------*
           if        w-let-arc-age-flg    not  = spaces
                     go to acc-age-min-100.
       acc-age-min-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-age-min-625.
      *                  *---------------------------------------------*
      *                  * Se impostato un codice diverso da zero si   *
      *                  * normalizza il tipo ordinamento agenti       *
      *                  *---------------------------------------------*
           if        rr-age-min           =    zero and
                     rr-age-max           =    zero
                     go to acc-age-min-650.
           if        rr-age-min           not  = rr-age-max
                     go to acc-age-min-650.
       acc-age-min-650.
      *                  *---------------------------------------------*
      *                  * Fine dipendenze dall'impostazione           *
      *                  *---------------------------------------------*
           go to     acc-age-min-800.
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
      *    * Visualizzazione : Codice agente min                       *
      *    *-----------------------------------------------------------*
       vis-age-min-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      08                   to   v-lin                  .
           if        w-prs-age-300-sns    not  = "S" and
                     w-prs-age-300-sns    not  = "T"
                     subtract  3          from v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-age-min           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-age-min-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Nominativo agente min                   *
      *    *-----------------------------------------------------------*
       vis-age-min-nom-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      08                   to   v-lin                  .
           if        w-prs-age-300-sns    not  = "S" and
                     w-prs-age-300-sns    not  = "T"
                     subtract  3          from v-lin                  .
           move      41                   to   v-pos                  .
           move      rr-age-min-nom       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-age-min-nom-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Codice agente max                          *
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
           move      09                   to   w-cod-mne-age-lin      .
           if        w-prs-age-300-sns    not  = "S" and
                     w-prs-age-300-sns    not  = "T"
                     subtract  3          from w-cod-mne-age-lin      .
           move      30                   to   w-cod-mne-age-pos      .
           move      09                   to   w-cod-mne-age-nln      .
           if        w-prs-age-300-sns    not  = "S" and
                     w-prs-age-300-sns    not  = "T"
                     subtract  3          from w-cod-mne-age-nln      .
           move      41                   to   w-cod-mne-age-nps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
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
       acc-age-max-150.
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
      *                  * Memorizzazione descrizione                  *
      *                  *---------------------------------------------*
           if        rr-age-min           =    zero and
                     rr-age-max           =    zero
                     move  "Tutti gli agenti                        "
                                          to   rr-age-max-nom
           else      move  w-let-arc-age-nom
                                          to   rr-age-max-nom         .
      *                  *---------------------------------------------*
      *                  * Visualizzazione nominativo agente           *
      *                  *---------------------------------------------*
           perform   vis-age-max-nom-000  thru vis-age-max-nom-999    .
      *                  *---------------------------------------------*
      *                  * Se codice errato : reimpostazione           *
      *                  *---------------------------------------------*
           if        w-let-arc-age-flg    not  = spaces
                     go to acc-age-max-100.
       acc-age-max-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-age-max-625.
      *                  *---------------------------------------------*
      *                  * Se impostato un codice diverso da zero si   *
      *                  * normalizza il tipo ordinamento agenti       *
      *                  *---------------------------------------------*
           if        rr-age-min           =    zero and
                     rr-age-max           =    zero
                     go to acc-age-max-650.
           if        rr-age-min           not  = rr-age-max
                     go to acc-age-max-650.
           move      zero                 to   rr-tor-age             .
           perform   vis-tor-age-000      thru vis-tor-age-999        .
       acc-age-max-650.
      *                  *---------------------------------------------*
      *                  * Fine dipendenze dall'impostazione           *
      *                  *---------------------------------------------*
           go to     acc-age-max-800.
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
      *    * Visualizzazione : Codice agente max                       *
      *    *-----------------------------------------------------------*
       vis-age-max-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      09                   to   v-lin                  .
           if        w-prs-age-300-sns    not  = "S" and
                     w-prs-age-300-sns    not  = "T"
                     subtract  3          from v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-age-max           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-age-max-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Nominativo agente max                   *
      *    *-----------------------------------------------------------*
       vis-age-max-nom-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      09                   to   v-lin                  .
           if        w-prs-age-300-sns    not  = "S" and
                     w-prs-age-300-sns    not  = "T"
                     subtract  3          from v-lin                  .
           move      41                   to   v-pos                  .
           move      rr-age-max-nom       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-age-max-nom-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Tipo ordinamento per agenti                *
      *    *-----------------------------------------------------------*
       acc-tor-age-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-tor-age-025.
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se non c'e' stampa non si accetta il    *
      *                      * campo                                   *
      *                      *-----------------------------------------*
           if        w-prs-age-300-sns    not  = "S" and
                     w-prs-age-300-sns    not  = "T"
                     go to acc-tor-age-999.
      *                      *-----------------------------------------*
      *                      * Se il tipo esecuzione non lo prevede    *
      *                      *-----------------------------------------*
           if        rr-tip-exe           =    02
                     go to acc-tor-age-999.
      *                      *-----------------------------------------*
      *                      * Se selezionato un solo agente non si    *
      *                      * accetta il campo                        *
      *                      *-----------------------------------------*
           if        rr-age-min           =    zero and
                     rr-age-max           =    zero
                     go to acc-tor-age-100.
           if        rr-age-min           not  = zero and
                     rr-age-max           =    zero
                     go to acc-tor-age-999.
           if        rr-age-min           =    rr-age-max
                     go to acc-tor-age-999.
       acc-tor-age-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tor-age-lun    to   v-car                  .
           move      w-exp-tor-age-num    to   v-ldt                  .
           move      "NCM#"               to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      11                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-tor-age-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-tor-age           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-tor-age-999.
       acc-tor-age-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-tor-age             .
       acc-tor-age-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-tor-age-425.
      *                  *---------------------------------------------*
      *                  * Che sia un valore consentito                *
      *                  *---------------------------------------------*
           if        rr-tor-age           not  = 01 and
                     rr-tor-age           not  = 02 and
                     rr-tor-age           not  = 03
                     go to acc-tor-age-100.
       acc-tor-age-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-tor-age-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-tor-age-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-tor-age-100.
       acc-tor-age-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Tipo ordinamento per agenti             *
      *    *-----------------------------------------------------------*
       vis-tor-age-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tor-age-lun    to   v-car                  .
           move      w-exp-tor-age-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      11                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-tor-age-tbl    to   v-txt                  .
           move      rr-tor-age           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tor-age-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Data minima documenti                      *
      *    *-----------------------------------------------------------*
       acc-dat-min-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-dat-min-025.
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        rr-tip-exe           not  = 01
                     go to acc-dat-min-999.
       acc-dat-min-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      13                   to   v-lin                  .
           if        w-prs-age-300-sns    not  = "S" and
                     w-prs-age-300-sns    not  = "T"
                     subtract  5          from v-lin                  .
           move      68                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
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
      *    * Visualizzazione : Data minima documenti                   *
      *    *-----------------------------------------------------------*
       vis-dat-min-000.
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      13                   to   v-lin                  .
           if        w-prs-age-300-sns    not  = "S" and
                     w-prs-age-300-sns    not  = "T"
                     subtract  5          from v-lin                  .
           move      68                   to   v-pos                  .
           move      rr-dat-min           to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dat-min-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Data massima documenti                     *
      *    *-----------------------------------------------------------*
       acc-dat-max-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-dat-max-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      15                   to   v-lin                  .
           if        w-prs-age-300-sns    not  = "S" and
                     w-prs-age-300-sns    not  = "T"
                     subtract  5          from v-lin                  .
           move      68                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
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
       acc-dat-max-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-dat-max-425.
      *                  *---------------------------------------------*
      *                  * Valore a zero non accettabile               *
      *                  *---------------------------------------------*
           if        rr-dat-max           not  = zero
                     go to acc-dat-max-450.
           if        v-key                =    "UP  "
                     go to acc-dat-max-450
           else      go to acc-dat-max-100.
       acc-dat-max-450.
      *                  *---------------------------------------------*
      *                  * Che non sia inferiore alla data minima      *
      *                  *---------------------------------------------*
           if        rr-dat-max           not  < rr-dat-min
                     go to acc-dat-max-475.
           if        v-key                =    "UP  "
                     go to acc-dat-max-475
           else      go to acc-dat-max-100.
       acc-dat-max-475.
      *                  *---------------------------------------------*
      *                  * Fine controlli                              *
      *                  *---------------------------------------------*
           go to     acc-dat-max-600.
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
      *    * Visualizzazione : Data massima documenti                  *
      *    *-----------------------------------------------------------*
       vis-dat-max-000.
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      15                   to   v-lin                  .
           if        w-prs-age-300-sns    not  = "S" and
                     w-prs-age-300-sns    not  = "T"
                     subtract  5          from v-lin                  .
           move      68                   to   v-pos                  .
           move      rr-dat-max           to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dat-max-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Codice tipo movimento fattura              *
      *    *-----------------------------------------------------------*
       acc-cod-zfi-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cod-zfi-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-des-zfi-ope      .
           move      rr-cod-zfi           to   w-cod-des-zfi-cod      .
           move      19                   to   w-cod-des-zfi-lin      .
           if        w-prs-age-300-sns    not  = "S" and
                     w-prs-age-300-sns    not  = "T"
                     subtract  5          from w-cod-des-zfi-lin      .
           move      30                   to   w-cod-des-zfi-pos      .
           move      19                   to   w-cod-des-zfi-dln      .
           if        w-prs-age-300-sns    not  = "S" and
                     w-prs-age-300-sns    not  = "T"
                     subtract  5          from w-cod-des-zfi-dln      .
           move      41                   to   w-cod-des-zfi-dps      .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-des-zfi-cll-000  thru cod-des-zfi-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-des-zfi-foi-000  thru cod-des-zfi-foi-999    .
       acc-cod-zfi-110.
           perform   cod-des-zfi-cll-000  thru cod-des-zfi-cll-999    .
           if        w-cod-des-zfi-ope    =    "F+"
                     go to acc-cod-zfi-115.
           if        w-cod-des-zfi-ope    =    "AC"
                     go to acc-cod-zfi-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-zfi-115.
           perform   cod-des-zfi-foi-000  thru cod-des-zfi-foi-999    .
           go to     acc-cod-zfi-110.
       acc-cod-zfi-120.
           move      w-cod-des-zfi-cod    to   v-alf                  .
       acc-cod-zfi-150.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-cod-zfi-999.
       acc-cod-zfi-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   rr-cod-zfi             .
       acc-cod-zfi-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura archivio [zfi]                      *
      *                  *---------------------------------------------*
           move      rr-cod-zfi           to   w-let-arc-zfi-cod      .
           perform   let-arc-zfi-000      thru let-arc-zfi-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione descrizione                  *
      *                  *---------------------------------------------*
           if        rr-cod-zfi           =    spaces
                     move  "Tutti i tipi documento        "
                                          to   rr-cod-zfi-des
           else      move  w-let-arc-zfi-des
                                          to   rr-cod-zfi-des         .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione tipo movimento  *
      *                  *---------------------------------------------*
           perform   vis-cod-zfi-des-000  thru vis-cod-zfi-des-999    .
      *                  *---------------------------------------------*
      *                  * Se codice errato : reimpostazione           *
      *                  *---------------------------------------------*
           if        w-let-arc-zfi-flg    not  = spaces
                     go to acc-cod-zfi-100.
       acc-cod-zfi-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cod-zfi-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-cod-zfi-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-cod-zfi-100.
       acc-cod-zfi-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Codice tipo movimento fattura           *
      *    *-----------------------------------------------------------*
       vis-cod-zfi-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      19                   to   v-lin                  .
           if        w-prs-age-300-sns    not  = "S" and
                     w-prs-age-300-sns    not  = "T"
                     subtract  5          from v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-cod-zfi           to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-zfi-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Descrizione tipo movimento fattura      *
      *    *-----------------------------------------------------------*
       vis-cod-zfi-des-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      19                   to   v-lin                  .
           if        w-prs-age-300-sns    not  = "S" and
                     w-prs-age-300-sns    not  = "T"
                     subtract  5          from v-lin                  .
           move      41                   to   v-pos                  .
           move      rr-cod-zfi-des       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-zfi-des-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Tipo vendita                               *
      *    *-----------------------------------------------------------*
       acc-tip-vpa-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se non c'e' stampa non si accetta il    *
      *                      * campo                                   *
      *                      *-----------------------------------------*
           if        w-prs-age-300-sns    not  = "S" and
                     w-prs-age-300-sns    not  = "T"
                     go to acc-tip-vpa-999.
       acc-tip-vpa-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-vpa-lun    to   v-car                  .
           move      w-exp-tip-vpa-num    to   v-ldt                  .
           move      "NDI#"               to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      20                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-tip-vpa-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-tip-vpa           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-tip-vpa-999.
       acc-tip-vpa-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-tip-vpa             .
       acc-tip-vpa-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-tip-vpa-425.
      *                  *---------------------------------------------*
      *                  * Che sia un valore consentito                *
      *                  *---------------------------------------------*
           if        rr-tip-vpa           not  = 01 and
                     rr-tip-vpa           not  = 02 and
                     rr-tip-vpa           not  = 03
                     go to acc-tip-vpa-100.
       acc-tip-vpa-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-tip-vpa-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-tip-vpa-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-tip-vpa-100.
       acc-tip-vpa-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Tipo ordinamento per agenti             *
      *    *-----------------------------------------------------------*
       vis-tip-vpa-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-vpa-lun    to   v-car                  .
           move      w-exp-tip-vpa-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      11                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-tip-vpa-tbl    to   v-txt                  .
           move      rr-tip-vpa           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tip-vpa-999.
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
      *              * Controllo su tipo di esecuzione                 *
      *              *-------------------------------------------------*
       tdo-ric-sel-105.
           if        w-prs-age-300-sns    not  = "S" and
                     w-prs-age-300-sns    not  = "T"
                     go to tdo-ric-sel-115.
           if        rr-tip-exe           not  = zero
                     go to tdo-ric-sel-110.
           move      "Manca il tipo di esecuzione !                     
      -              "               "    to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-110.
           if        rr-tip-exe           =    01 or
                     rr-tip-exe           =    02 or
                     rr-tip-exe           =    03
           go to     tdo-ric-sel-115.
           move      "Tipo di esecuzione errato !                       
      -              "               "    to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-115.
           go to     tdo-ric-sel-200.
       tdo-ric-sel-200.
      *              *-------------------------------------------------*
      *              * Controllo su Codice min e max                   *
      *              *-------------------------------------------------*
           if        rr-age-max           =    zero
                     go to tdo-ric-sel-300.
           if        rr-age-max           not  < rr-age-min
                     go to tdo-ric-sel-300.
           move      "Il codice massimo non puo' essere inferiore al cod
      -              "ice minimo !   "    to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-300.
      *              *-------------------------------------------------*
      *              * Controllo : Tipo di ordinamento per agenti      *
      *              *-------------------------------------------------*
       tdo-ric-sel-305.
           if        rr-tip-exe           =    02
                     go to tdo-ric-sel-315.
           if        w-prs-age-300-sns    not  = "S" and
                     w-prs-age-300-sns    not  = "T"
                     go to tdo-ric-sel-315.
           if        rr-age-min           not  = zero or
                     rr-age-max           not  = zero
                     go to tdo-ric-sel-315.
           if        rr-tor-age           not  = zero
                     go to tdo-ric-sel-310.
           move      "Manca il tipo di ordinamento per gli agenti !     
      -              "               "    to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-310.
           if        rr-tor-age           =    01 or
                     rr-tor-age           =    02 or
                     rr-tor-age           =    03
                     go to tdo-ric-sel-315.
           move      "Tipo di ordinamento per gli agenti errato !       
      -              "               "    to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-315.
           go to     tdo-ric-sel-400.
       tdo-ric-sel-400.
      *              *-------------------------------------------------*
      *              * Controllo su data minima documenti              *
      *              *-------------------------------------------------*
           go to     tdo-ric-sel-500.
       tdo-ric-sel-500.
      *              *-------------------------------------------------*
      *              * Controllo su data massima documenti             *
      *              *-------------------------------------------------*
       tdo-ric-sel-505.
           if        rr-dat-max           not  = zero
                     go to tdo-ric-sel-510.
           move      "Manca la data massima documenti !                 
      -              "               "    to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-510.
           if        rr-dat-max           not  < rr-dat-min
                     go to tdo-ric-sel-515.
           move      "Data massima documenti inferiore alla data minima 
      -              "!              "    to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-515.
           go to     tdo-ric-sel-600.
       tdo-ric-sel-600.
      *              *-------------------------------------------------*
      *              * Controllo su tipo movimento fattura             *
      *              *-------------------------------------------------*
           go to     tdo-ric-sel-700.
       tdo-ric-sel-700.
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
      *              * Se non e' previsto alcun tipo di stampa si for- *
      *              * za il tipo esecuzione 02                        *
      *              *-------------------------------------------------*
           if        w-prs-age-300-sns    not  = "S" and
                     w-prs-age-300-sns    not  = "T"
                     move  02             to   rr-tip-exe             .
      *              *-------------------------------------------------*
      *              * Se non e' previsto alcun tipo di stampa si for- *
      *              * za il tipo ordinamento agenti per codice        *
      *              *-------------------------------------------------*
           if        w-prs-age-300-sns    not  = "S" and
                     w-prs-age-300-sns    not  = "T"
                     move  02             to   rr-tor-age             .
      *              *-------------------------------------------------*
      *              * Se non e' previsto alcun tipo di stampa si for- *
      *              * za il tipo ordinamento agenti per codice        *
      *              *-------------------------------------------------*
           if        rr-tip-exe           =    02
                     move  02             to   rr-tor-age             .
      *              *-------------------------------------------------*
      *              * Se selezionato solo agente min si forza agente  *
      *              * max uguale ad agente min                        *
      *              *-------------------------------------------------*
           if        rr-age-min           not  = zero and
                     rr-age-max           =    zero
                     move  rr-age-min     to   rr-age-max             .
      *              *-------------------------------------------------*
      *              * Se selezionato un solo agente si forza il tipo  *
      *              * ordinamento agenti per codice                   *
      *              *-------------------------------------------------*
           if        rr-age-min           not  = zero and
                     rr-age-min           =    rr-age-max
                     move  02             to   rr-tor-age             .
      *              *-------------------------------------------------*
      *              * Assestamento del parametro di si/no stampa a    *
      *              * seconda del tipo di esecuzione                  *
      *              *-------------------------------------------------*
           if        rr-tip-exe           =    01 or
                     rr-tip-exe           =    03
                     move  "S"            to   w-cnt-fun-snx-stp
           else      move  "N"            to   w-cnt-fun-snx-stp      .
      *              *-------------------------------------------------*
      *              * Normalizzazione tipo vendita                    *
      *              *-------------------------------------------------*
           if        rr-tip-vpa           not  = 01 and
                     rr-tip-vpa           not  = 02 and
                     rr-tip-vpa           not  = 03
                     move  01             to   rr-tip-vpa             .
           subtract  01                   from rr-tip-vpa             .
      *              *-------------------------------------------------*
      *              * Assestamento parametri tipo funzionamento pro-  *
      *              * gramma provenienti dalle personalizzazioni      *
      *              *-------------------------------------------------*
           move      w-prs-age-300-ctf    to   rr-age-300-ctf         .
           move      w-prs-age-300-stf    to   rr-age-300-stf         .
           move      w-prs-age-300-abr    to   rr-age-300-abr         .
           move      w-prs-age-300-abt    to   rr-age-300-abt         .
           move      w-prs-age-300-sns    to   rr-age-300-sns         .
           move      w-prs-age-300-aps    to   rr-age-300-aps         .
           move      w-prs-age-300-tma    to   rr-age-300-tma         .
           move      w-prs-age-300-tdm    to   rr-age-300-tdm         .
           move      w-prs-age-300-ops    to   rr-age-300-ops         .
           move      w-prs-age-300-fpe    to   rr-age-300-fpe         .
           move      w-prs-age-300-ppe    to   rr-age-300-ppe         .
       reg-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione richieste di selezione                    *
      *    *-----------------------------------------------------------*
       nor-ric-sel-000.
      *              *-------------------------------------------------*
      *              * Tipo di esecuzione                              *
      *              *-------------------------------------------------*
           move      zero                 to   rr-tip-exe             .
      *              *-------------------------------------------------*
      *              * Codice agente min                               *
      *              *-------------------------------------------------*
           move      zero                 to   rr-age-min             .
           move      spaces               to   rr-age-min-nom         .
      *              *-------------------------------------------------*
      *              * Codice agente max                               *
      *              *-------------------------------------------------*
           move      zero                 to   rr-age-max             .
           move      spaces               to   rr-age-max-nom         .
      *              *-------------------------------------------------*
      *              * Tipo di ordinamento per agenti                  *
      *              *-------------------------------------------------*
           move      zero                 to   rr-tor-age             .
      *              *-------------------------------------------------*
      *              * Data minima documenti                           *
      *              *-------------------------------------------------*
           move      zero                 to   rr-dat-min             .
      *              *-------------------------------------------------*
      *              * Data massima documenti                          *
      *              *-------------------------------------------------*
           move      zero                 to   rr-dat-max             .
      *              *-------------------------------------------------*
      *              * Codice tipo movimento fattura e dati correlati  *
      *              *-------------------------------------------------*
           move      spaces               to   rr-cod-zfi             .
           move      spaces               to   rr-cod-zfi-des         .
      *              *-------------------------------------------------*
      *              * Tipo vendita                                    *
      *              *-------------------------------------------------*
           move      zero                 to   rr-tip-vpa             .
       nor-ric-sel-999.
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
           move      w-prs-age-300-aps    to   w-cnt-stp-amp-lin      .
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
      *    * Routine di lettura archivio anagrafica agenti             *
      *    *-----------------------------------------------------------*
       let-arc-age-000.
      *              *-------------------------------------------------*
      *              * Test se codice agente a zero                    *
      *              *-------------------------------------------------*
           if        w-let-arc-age-cod    =    zero
                     go to let-arc-age-800.
      *              *-------------------------------------------------*
      *              * Test se codice pari al valore precedente        *
      *              *-------------------------------------------------*
           if        w-let-arc-age-cod    =    w-let-arc-age-exc
                     go to let-arc-age-900.
       let-arc-age-100.
      *              *-------------------------------------------------*
      *              * Lettura archivio [age] relativamente all'agente *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODAGE    "         to   f-key                  .
           move      w-let-arc-age-cod    to   rf-age-cod-age         .
           move      "pgm/age/fls/ioc/obj/iofage"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-age                 .
      *              *-------------------------------------------------*
      *              * Deviazione secondo l'esito della lettura        *
      *              *-------------------------------------------------*
           if        f-sts                =    e-not-err
                     go to let-arc-age-200
           else      go to let-arc-age-300.
       let-arc-age-200.
      *              *-------------------------------------------------*
      *              * Se anagrafica agente esistente                  *
      *              *-------------------------------------------------*
       let-arc-age-225.
      *                  *---------------------------------------------*
      *                  * Memorizzazione flag e valori                *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Flag di uscita                          *
      *                      *-----------------------------------------*
           move      spaces               to   w-let-arc-age-flg      .
      *                      *-----------------------------------------*
      *                      * Nome                                    *
      *                      *-----------------------------------------*
           move      rf-age-nom-age       to   w-let-arc-age-nom      .
      *                      *-----------------------------------------*
      *                      * Ragione sociale                         *
      *                      *-----------------------------------------*
           move      rf-age-rag-soc       to   w-let-arc-age-rag      .
      *                      *-----------------------------------------*
      *                      * Via                                     *
      *                      *-----------------------------------------*
           move      rf-age-via-age       to   w-let-arc-age-via      .
      *                      *-----------------------------------------*
      *                      * Localita'                               *
      *                      *-----------------------------------------*
           move      rf-age-loc-age       to   w-let-arc-age-loc      .
      *                      *-----------------------------------------*
      *                      * Mnemonico                               *
      *                      *-----------------------------------------*
           move      rf-age-cod-mne       to   w-let-arc-age-mne      .
       let-arc-age-250.
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     let-arc-age-900.
       let-arc-age-300.
      *              *-------------------------------------------------*
      *              * Se anagrafica agente non esistente              *
      *              *-------------------------------------------------*
       let-arc-age-325.
      *                  *---------------------------------------------*
      *                  * Memorizzazione flag e valori                *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Flag di uscita                          *
      *                      *-----------------------------------------*
           move      "#"                  to   w-let-arc-age-flg      .
      *                      *-----------------------------------------*
      *                      * Nome                                    *
      *                      *-----------------------------------------*
           move      all   "."            to   w-let-arc-age-nom      .
      *                      *-----------------------------------------*
      *                      * Ragione sociale                         *
      *                      *-----------------------------------------*
           move      all   "."            to   w-let-arc-age-rag      .
      *                      *-----------------------------------------*
      *                      * Via                                     *
      *                      *-----------------------------------------*
           move      all   "."            to   w-let-arc-age-via      .
      *                      *-----------------------------------------*
      *                      * Localita'                               *
      *                      *-----------------------------------------*
           move      all   "."            to   w-let-arc-age-loc      .
      *                      *-----------------------------------------*
      *                      * Mnemonico                               *
      *                      *-----------------------------------------*
           move      spaces               to   w-let-arc-age-mne      .
       let-arc-age-350.
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     let-arc-age-900.
       let-arc-age-800.
      *              *-------------------------------------------------*
      *              * Se codice agente a zero                         *
      *              *-------------------------------------------------*
       let-arc-age-825.
      *                  *---------------------------------------------*
      *                  * Memorizzazione flag e valori                *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Flag di uscita                          *
      *                      *-----------------------------------------*
           move      spaces               to   w-let-arc-age-flg      .
      *                      *-----------------------------------------*
      *                      * Nome                                    *
      *                      *-----------------------------------------*
           move      spaces               to   w-let-arc-age-nom      .
      *                      *-----------------------------------------*
      *                      * Ragione sociale                         *
      *                      *-----------------------------------------*
           move      spaces               to   w-let-arc-age-rag      .
      *                      *-----------------------------------------*
      *                      * Via                                     *
      *                      *-----------------------------------------*
           move      spaces               to   w-let-arc-age-via      .
      *                      *-----------------------------------------*
      *                      * Localita'                               *
      *                      *-----------------------------------------*
           move      spaces               to   w-let-arc-age-loc      .
      *                      *-----------------------------------------*
      *                      * Mnemonico                               *
      *                      *-----------------------------------------*
           move      spaces               to   w-let-arc-age-mne      .
       let-arc-age-850.
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     let-arc-age-900.
       let-arc-age-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Codice agente su valore precedente          *
      *                  *---------------------------------------------*
           move      w-let-arc-age-cod    to   w-let-arc-age-exc      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     let-arc-age-999.
       let-arc-age-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio tipi movimento fattura        *
      *    *-----------------------------------------------------------*
       let-arc-zfi-000.
      *              *-------------------------------------------------*
      *              * Test se codice a spaces                         *
      *              *-------------------------------------------------*
           if        w-let-arc-zfi-cod    =    spaces
                     go to let-arc-zfi-800.
      *              *-------------------------------------------------*
      *              * Test se codice pari al valore precedente        *
      *              *-------------------------------------------------*
           if        w-let-arc-zfi-cod    =    w-let-arc-zfi-exc
                     go to let-arc-zfi-900.
       let-arc-zfi-100.
      *              *-------------------------------------------------*
      *              * Lettura archivio [zfi]                          *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODTMO"             to   f-key                  .
           move      w-let-arc-zfi-cod    to   rf-zfi-cod-tmo         .
           move      "pgm/fat/fls/ioc/obj/iofzfi"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zfi                 .
      *              *-------------------------------------------------*
      *              * Deviazione secondo l'esito della lettura        *
      *              *-------------------------------------------------*
           if        f-sts                =    e-not-err
                     go to let-arc-zfi-200
           else      go to let-arc-zfi-300.
       let-arc-zfi-200.
      *              *-------------------------------------------------*
      *              * Se anagrafica esistente                         *
      *              *-------------------------------------------------*
       let-arc-zfi-225.
      *                  *---------------------------------------------*
      *                  * Memorizzazione flag e valori                *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Flag di uscita                          *
      *                      *-----------------------------------------*
           move      spaces               to   w-let-arc-zfi-flg      .
      *                      *-----------------------------------------*
      *                      * Descrizione                             *
      *                      *-----------------------------------------*
           move      rf-zfi-des-tmo       to   w-let-arc-zfi-des      .
       let-arc-zfi-250.
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     let-arc-zfi-900.
       let-arc-zfi-300.
      *              *-------------------------------------------------*
      *              * Se anagrafica non esistente                     *
      *              *-------------------------------------------------*
       let-arc-zfi-325.
      *                  *---------------------------------------------*
      *                  * Memorizzazione flag e valori                *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Flag di uscita                          *
      *                      *-----------------------------------------*
           move      "#"                  to   w-let-arc-zfi-flg      .
      *                      *-----------------------------------------*
      *                      * Descrizione                             *
      *                      *-----------------------------------------*
           move      all   "."            to   w-let-arc-zfi-des      .
       let-arc-zfi-350.
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     let-arc-zfi-900.
       let-arc-zfi-800.
      *              *-------------------------------------------------*
      *              * Se codice a spaces                              *
      *              *-------------------------------------------------*
       let-arc-zfi-825.
      *                  *---------------------------------------------*
      *                  * Memorizzazione flag e valori                *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Flag di uscita                          *
      *                      *-----------------------------------------*
           move      spaces               to   w-let-arc-zfi-flg      .
      *                      *-----------------------------------------*
      *                      * Descrizione                             *
      *                      *-----------------------------------------*
           move      spaces               to   w-let-arc-zfi-des      .
       let-arc-zfi-850.
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     let-arc-zfi-900.
       let-arc-zfi-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Codice su valore precedente                 *
      *                  *---------------------------------------------*
           move      w-let-arc-zfi-cod    to   w-let-arc-zfi-exc      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     let-arc-zfi-999.
       let-arc-zfi-999.
           exit.

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice agente          *
      *    *-----------------------------------------------------------*
           copy      "pgm/age/prg/cpy/acmnage0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice tipo movimento  *
      *    * per fatturazione                                          *
      *    *-----------------------------------------------------------*
           copy      "pgm/fat/prg/cpy/acdezfi0.acs"                   .

