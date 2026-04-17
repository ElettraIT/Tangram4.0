       Identification Division.
       Program-Id.                                 eurfbg05           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    cnv                 *
      *                        Area gestionale:    cnv                 *
      *                                Settore:    eur                 *
      *                                   Fase:    eurfbg01            *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 07/09/00    *
      *                       Ultima revisione:    NdK del 17/10/01    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Conversione per EURO - Foralberg            *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      *                    Assestamento fattura FPF 32                 *
      *                                                                *
      * {FAT}   [fir] v  Fatture clienti, righe                        *
      * {FAT}   [fix] v  Fatture clienti, estensioni righe             *
      * {FAT}   [fit] v  Fatture clienti, testate                      *
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

      *================================================================*
       Input-Output Section.
      *================================================================*

       File-Control.

      *    *===========================================================*
      *    * File Control [kk1]                                        *
      *    *-----------------------------------------------------------*
           select  optional  kk1   assign to disk           f-kk1-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is kk1-key
                             file status  is                f-kk1-sts .

      ******************************************************************
       Data Division.
      ******************************************************************

      *================================================================*
       File Section.
      *================================================================*

      *    *===========================================================*
      *    * File Description [kk1]                                    *
      *    *-----------------------------------------------------------*
       fd  kk1       label record standard                            .

      *    *===========================================================*
      *    * Record file di appoggio [kk1]                             *
      *    *-----------------------------------------------------------*
       01  kk1-rec.
      *        *-------------------------------------------------------*
      *        * Chiave di ordinamento                                 *
      *        *-------------------------------------------------------*
           05  kk1-key.
               10  kk1-num-prt            pic  9(11)                  .
               10  kk1-tip-rec            pic  x(01)                  .
               10  kk1-num-prg            pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  kk1-dat.
               10  filler                 pic  x(01)                  .

      *================================================================*
       Working-Storage Section.
      *================================================================*

      *    *===========================================================*
      *    * File area per [kk1]                                       *
      *    *-----------------------------------------------------------*
       01  f-kk1.
           05  f-kk1-nam                  pic  x(04)                  .
           05  f-kk1-pat                  pic  x(40)                  .
           05  f-kk1-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * Area di identificazione                                   *
      *    *-----------------------------------------------------------*
       01  i-ide.
      *        *-------------------------------------------------------*
      *        * Sistema applicativo                                   *
      *        *-------------------------------------------------------*
           05  i-ide-sap                  pic  x(03) value
                     "cnv"                                            .
      *        *-------------------------------------------------------*
      *        * Area gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-arg                  pic  x(03) value
                     "cnv"                                            .
      *        *-------------------------------------------------------*
      *        * Settore gestionale                                    *
      *        *-------------------------------------------------------*
           05  i-ide-set                  pic  x(03) value
                     "eur"                                            .
      *        *-------------------------------------------------------*
      *        * Fase gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-fas                  pic  x(06) value
                     "eurfbg"                                         .
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10) value
                     "eurfbg05"                                       .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  i-ide-des                  pic  x(40) value
                     "    CONVERSIONI PER EURO - Foralberg    "       .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "mvideo" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/v"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "msegrt"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/s"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "mmessg" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/m"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per moduli                 "mbckgx" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/b"                                  .

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
      *    * Area di comunicazione per modulo                "mopsys"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/o"                                  .

      *    *===========================================================*
      *    * File area generica                                        *
      *    *-----------------------------------------------------------*
       01  f-xxx.
           05  f-xxx-nam                  pic  x(04)                  .
           05  f-xxx-pat                  pic  x(40)                  .
           05  f-xxx-des                  pic  x(46)                  .
           05  f-xxx-sts                  pic  x(02)                  .
           05  f-xxx-obj                  pic  x(40)                  .
           05  f-xxx-nrl                  pic  9(09)                  .
           05  f-xxx-nrs                  pic  9(09)                  .
           05  f-xxx-nrc                  pic  9(02)                  .
           05  f-xxx-nrd                  pic  9(01)                  .
           05  f-xxx-ope                  pic  x(02)                  .
           05  f-xxx-fas                  pic  x(01)                  .

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
      *            * Per routine qry-rou-pri-000                       *
      *            *---------------------------------------------------*
               10  w-cnt-qry-rou-pri      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Per routine exe-rou-srt-000                       *
      *            *---------------------------------------------------*
               10  w-cnt-exe-rou-srt      pic  x(01)                  .
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
      *            * Si/No richieste pre esecuzione interrogazione     *
      *            *---------------------------------------------------*
               10  w-cnt-fun-snx-ric      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Si/No funzionamento ciclico interrogazione        *
      *            *---------------------------------------------------*
               10  w-cnt-fun-snx-cic      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Si/No funzionamento automatico interrogazione     *
      *            *---------------------------------------------------*
               10  w-cnt-fun-snx-aut      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Flag di Si/No primo giro di esecuzione            *
      *            *---------------------------------------------------*
               10  w-cnt-fun-prm-gir      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Area di controllo per funzionamento qry-routine       *
      *        *-------------------------------------------------------*
           05  w-cnt-qry.
      *            *---------------------------------------------------*
      *            * Flag di primo giro                                *
      *            *---------------------------------------------------*
               10  w-cnt-qry-mrk-uno      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Flag di interruzione forzata                      *
      *            *---------------------------------------------------*
               10  w-cnt-qry-flg-int      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Flag di uscita da subroutines principali          *
      *            *---------------------------------------------------*
               10  w-cnt-qry-flg-sub      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Area per salvataggio parametri rottura livello    *
      *            *---------------------------------------------------*
               10  w-cnt-qry-sav-liv.
                   15  w-cnt-qry-sav-l05  pic  x(64)                  .
                   15  w-cnt-qry-sav-l04  pic  x(64)                  .
                   15  w-cnt-qry-sav-l03  pic  x(64)                  .
                   15  w-cnt-qry-sav-l02  pic  x(64)                  .
                   15  w-cnt-qry-sav-l01  pic  x(64)                  .
      *            *---------------------------------------------------*
      *            * Area per salvataggio area di rottura              *
      *            *---------------------------------------------------*
               10  w-cnt-qry-sav-rot.
                   15  filler occurs 320  pic  x(01)                  .
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
      *    * Work-area per trasformazioni in uppercase                 *
      *    *-----------------------------------------------------------*
       01  w-upp.
           05  w-upp-des.
               10  w-upp-chr occurs 40    pic  x(01)                  .
           05  w-ctr                      pic  9(02)                  .
           05  w-upp-car.
               10  filler                 pic  x(26) value
                     "ABCDEFGHIJKLMNOPQRSTUVWXYZ"                     .
           05  w-upp-crr redefines w-upp-car.
               10  w-upc occurs 26        pic  x(01)                  .
           05  w-low.
               10  filler                 pic  x(26) value
                     "abcdefghijklmnopqrstuvwxyz"                     .
           05  w-lor redefines w-low.
               10  w-loc occurs 26        pic  x(01)                  .
           05  w-ulc                      pic  9(02)                  .

      *    *===========================================================*
      *    * Record files                                              *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [fir]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/fat/fls/rec/rffir"                          .
      *        *-------------------------------------------------------*
      *        * [fit]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/fat/fls/rec/rffit"                          .
      *        *-------------------------------------------------------*
      *        * [fix]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/fat/fls/rec/rffix"                          .
      *        *-------------------------------------------------------*
      *        * [ftiprt]                                              *
      *        *-------------------------------------------------------*
           copy      "pgm/fat/num/rec/rnftiprt"                       .

      *    *===========================================================*
      *    * Work-area richieste per interrogazione                    *
      *    *-----------------------------------------------------------*
       01  rr.
      *        *-------------------------------------------------------*
      *        * Conversione automatica o manuale                      *
      *        *  - A1 : Automatica                                    *
      *        *  - M1 : Manuale                                       *
      *        *  - A2 : Automatica, riscrittura                       *
      *        *  - M2 : Manuale, riscrittura                          *
      *        *-------------------------------------------------------*
           05  rr-aut-man                 pic  x(02)                  .
      *        *-------------------------------------------------------*
      *        * Codice azienda                                        *
      *        *-------------------------------------------------------*
           05  rr-cod-azi                 pic  x(04)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per presa visione                              *
      *        *-------------------------------------------------------*
           05  rr-pre-vis                 pic  x(02)                  .

      *    *===========================================================*
      *    * Work per accettazioni campi espansi                       *
      *    *-----------------------------------------------------------*
       01  w-exp.
      *        *-------------------------------------------------------*
      *        * Work per : Tipo conversione                           *
      *        *-------------------------------------------------------*
           05  w-exp-aut-man.
               10  w-exp-aut-man-num      pic  9(02)       value 04   .
               10  w-exp-aut-man-lun      pic  9(02)       value 40   .
               10  w-exp-aut-man-tbl.
                   15  filler             pic  x(40) value
                          "Automatica, per tutti gli archivi       "  .
                   15  filler             pic  x(40) value
                          "Manuale, un archivio alla volta         "  .
                   15  filler             pic  x(40) value
                          "Automatica, riscrittura archivi         "  .
                   15  filler             pic  x(40) value
                          "Manuale, riscrittura archivi            "  .
      *        *-------------------------------------------------------*
      *        * Work per : Risposta Si/No                             *
      *        *-------------------------------------------------------*
           05  w-exp-ris-snx.
               10  w-exp-ris-snx-num      pic  9(02)       value 02   .
               10  w-exp-ris-snx-lun      pic  9(02)       value 02   .
               10  w-exp-ris-snx-tbl.
                   15  filler             pic  x(02) value "Si"       .
                   15  filler             pic  x(02) value "No"       .

      *    *===========================================================*
      *    * Work-area specifica del modulo di i-o                     *
      *    *-----------------------------------------------------------*
       01  w.
      *        *-------------------------------------------------------*
      *        * Indici di comodo                                      *
      *        *-------------------------------------------------------*
           05  w-inx                      pic  9(03)                  .
           05  w-c01                      pic  9(03)                  .
           05  w-c02                      pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Work per messaggi errore                              *
      *        *-------------------------------------------------------*
           05  w-ide-dat                  pic  x(08)                  .
           05  w-ide-saa                  pic  x(03)                  .
           05  w-ide-mes                  pic  x(02)                  .
           05  w-ide-arc                  pic  x(07)                  .
           05  w-ide-mag                  pic  x(07)                  .
           05  w-ide-prt                  pic  x(11)                  .
           05  w-ide-prg                  pic  x(09)                  .

      *    *===========================================================*
      *    * Area di comunicazione per determinazione stato ordine     *
      *    * cliente                                                   *
      *    *-----------------------------------------------------------*
           copy      "pgm/orc/prg/cpy/dstsorc0.dtl"                   .

      *    *===========================================================*
      *    * Work per conversione in Euro                              *
      *    *-----------------------------------------------------------*
           copy      "cnv/cnv/prg/cpy/cnveur01.wkl"                   .

      *    *===========================================================*
      *    * Work area di comodo                                       *
      *    *-----------------------------------------------------------*
       01  w-wrk.
      *        *-------------------------------------------------------*
      *        * Contatori generici                                    *
      *        *-------------------------------------------------------*
           05  w-wrk-ctr-001              pic  9(06)v9(03)            .
           05  w-wrk-ctr-002              pic  9(03)v9(05)            .
           05  w-wrk-ctr-003              pic  9(06)                  .
           05  w-wrk-cmd-prz              pic  9(11)                  .

      *    *===========================================================*
      *    * Work area per tabella archivi                             *
      *    *-----------------------------------------------------------*
       01  w-arc.
           05  w-arc-ele.
               10  w-arc-ele-max          pic  9(03) value 005        .
               10  w-arc-ele-tbl.
______*            15  filler             pic  x(55) value
______*      "[hbx]      File di confronto per l'esportazione A      ".
______*            15  filler             pic  x(55) value
______*      "[hex]      File di confronto per l'esportazione B      ".
                   15  filler             pic  x(55) value
             "[hnu]      Numerazioni                                 ".
                   15  filler             pic  x(55) value
             "[htn]      Tabella note di stampa listini              ".
                   15  filler             pic  x(55) value
             "[hts]      Note di stampa listini                      ".
______*            15  filler             pic  x(55) value
______*      "[hwx]      File di controllo transazioni               ".
                   15  filler             pic  x(55) value
             "[hyx]      File di appoggio generico                   ".
                   15  filler             pic  x(55) value
             "[hzt]      Tracciati di stampa listini                 ".
               10  w-arc-ele-tbr redefines
                   w-arc-ele-tbl.
                   15  w-arc-ele-ele occurs 005.
                       20  filler         pic  x(01)                  .
                       20  w-arc-ele-nam  pic  x(03)                  .
                       20  filler         pic  x(05)                  .
                       20  w-arc-ele-des  pic  x(46)                  .

      *    *===========================================================*
      *    * Work per subroutines di Att                               *
      *    *-----------------------------------------------------------*
       01  w-att.
      *        *-------------------------------------------------------*
      *        * Work per Att numero protocollo                        *
      *        *-------------------------------------------------------*
           05  w-att-fti-prt.
               10  w-att-fti-prt-ddo      pic  9(07)                  .
               10  w-att-fti-prt-saa      pic  9(03)                  .
               10  w-att-fti-prt-num      pic  9(09)                  .
               10  w-att-fti-prt-wnu      pic  9(09)                  .
               10  w-att-fti-prt-wnu-r    redefines
                   w-att-fti-prt-wnu.
                   15  w-att-fti-prt-wsa  pic  9(03)                  .
                   15  w-att-fti-prt-wpr  pic  9(06)                  .

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
      *              * Preparazione tipo funzionamento                 *
      *              *-------------------------------------------------*
           perform   pre-tip-fun-000      thru pre-tip-fun-999        .
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-rou-opn-fls      .
           perform   rou-opn-fls-000      thru rou-opn-fls-999        .
           if        w-cnt-rou-opn-fls    not  = spaces
                     go to main-900.
      *              *-------------------------------------------------*
      *              * Segnale primo giro di esecuzione                *
      *              *-------------------------------------------------*
           move      "S"                  to   w-cnt-fun-prm-gir      .
      *              *-------------------------------------------------*
      *              * Se no richieste : a esecuzione conversione      *
      *              *-------------------------------------------------*
           if        w-cnt-fun-snx-ric    not  = "S"
                     go to main-500.
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
       main-500.
      *              *-------------------------------------------------*
      *              * Esecuzione conversione file                     *
      *              *-------------------------------------------------*
           perform   exe-cnv-fil-000      thru exe-cnv-fil-999        .
      *              *-------------------------------------------------*
      *              * Se no richieste : a fine programma              *
      *              *-------------------------------------------------*
           if        w-cnt-fun-snx-ric    not  = "S"
                     go to main-750.
      *              *-------------------------------------------------*
      *              * Test se fine esecuzione da operatore            *
      *              *-------------------------------------------------*
           if        w-cnt-qry-rou-pri    not  = spaces
                     go to main-750.
      *              *-------------------------------------------------*
      *              * Segnale non piu' primo giro di esecuzione       *
      *              *-------------------------------------------------*
           move      "N"                  to   w-cnt-fun-prm-gir      .
      *              *-------------------------------------------------*
      *              * Test se tipo esecuzione ciclico                 *
      *              *-------------------------------------------------*
           if        w-cnt-fun-snx-ric    not  = "S" or
                     w-cnt-fun-snx-cic    not  = "S"
                     go to main-750
           else      go to main-250.
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
      *    * Trasformazione in uppercase                               *
      *    *-----------------------------------------------------------*
       trf-des-upp-000.
           move      zero                 to   w-ctr                  .
       trf-des-upp-100.
           add       1                    to   w-ctr                  .
           if        w-ctr                >    40
                     go to  trf-des-upp-999.
           move      zero                 to   w-ulc                  .
           inspect   w-low            tallying w-ulc
                     for characters     before initial w-upp-chr
                                                      (w-ctr)         .
           if        w-ulc                <    26
                     add     1            to   w-ulc
                     move    w-upc(w-ulc) to   w-upp-chr(w-ctr)       .
           go to     trf-des-upp-100.
       trf-des-upp-999.
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
      *              * Si/No funzionamento ciclico                     *
      *              *-------------------------------------------------*
           move      "N"                  to   w-cnt-fun-snx-cic      .
      *              *-------------------------------------------------*
      *              * Si/No funzionamento automatico                  *
      *              *-------------------------------------------------*
           move      "S"                  to   w-cnt-fun-snx-aut      .
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
      *                  *---------------------------------------------*
      *                  * Tipo conversione                            *
      *                  *---------------------------------------------*
           perform   acc-aut-man-000      thru acc-aut-man-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
       acc-ric-sel-200.
      *                  *---------------------------------------------*
      *                  * Codice azienda                              *
      *                  *---------------------------------------------*
           perform   acc-cod-azi-000      thru acc-cod-azi-999        .
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
           move      "Conferma esecuzione del programma (S/E) ?"
                                          to   v-not                  .
           move      " "                  to   v-alf                  .
           move      "SE"                 to   v-msk                  .
           move      "DO  "               to   v-pfk (05)             .
           move      "EXIT"               to   v-pfk (20)             .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           if        v-key                not  = spaces
                     go to acc-ric-sel-920.
           if        v-alf                =    "S"
                     move   "DO  "        to   v-key
           else if   v-alf                =    "E"
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
      *              *-------------------------------------------------*
      *              * Tipo conversione                                *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tipo conversione           :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Codice azienda                                  *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice azienda precedente  :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Preparazione linea di separazione               *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      all   "-"            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo selezione : Tipo conversione           *
      *    *-----------------------------------------------------------*
       acc-aut-man-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
           move      "A1"                 to   rr-aut-man             .
       acc-aut-man-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-aut-man-lun    to   v-car                  .
           move      w-exp-aut-man-num    to   v-ldt                  .
           move      "1234#"              to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      w-exp-aut-man-tbl    to   v-txt                  .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-ric    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      05                   to   v-lin                  .
           move      30                   to   v-pos                  .
           if        rr-aut-man           =    "A1"
                     move  01             to   v-num
           else if   rr-aut-man           =    "M1"
                     move  02             to   v-num
           else if   rr-aut-man           =    "A2"
                     move  03             to   v-num
           else if   rr-aut-man           =    "M2"
                     move  04             to   v-num
           else      move  zero           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-aut-man-999.
       acc-aut-man-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        v-num                =    01
                     move  "A1"           to   rr-aut-man
           else if   v-num                =    02
                     move  "M1"           to   rr-aut-man
           else if   v-num                =    03
                     move  "A2"           to   rr-aut-man
           else if   v-num                =    04
                     move  "M2"           to   rr-aut-man
           else      move  spaces         to   rr-aut-man             .
       acc-aut-man-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
           if        rr-aut-man           =    spaces
                     go to acc-aut-man-100.
       acc-aut-man-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-aut-man-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-aut-man-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-aut-man-100.
       acc-aut-man-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Codice azienda                             *
      *    *-----------------------------------------------------------*
       acc-cod-azi-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
           go to     acc-cod-azi-999.
       acc-cod-azi-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "L"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-cod-azi           to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-cod-azi-999.
       acc-cod-azi-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   rr-cod-azi             .
       acc-cod-azi-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
           if        rr-cod-azi           =    spaces
                     go to acc-cod-azi-100.
       acc-cod-azi-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cod-azi-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-cod-azi-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-cod-azi-100.
       acc-cod-azi-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Codice azienda                          *
      *    *-----------------------------------------------------------*
       vis-cod-azi-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-cod-azi           to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-azi-999.
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
      *              * Controllo su tipo conversione                   *
      *              *-------------------------------------------------*
           if        rr-aut-man           =    "A1" or
                     rr-aut-man           =    "M1" or
                     rr-aut-man           =    "A2" or
                     rr-aut-man           =    "M2"
                     go to tdo-ric-sel-200.
           move      "ME"                 to   v-ope                  .
           move      "Tipo conversione errato !"
                                          to   v-not                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "#"                  to   w-cnt-tdo-ric-flg      .
           go to     tdo-ric-sel-999.
       tdo-ric-sel-200.
      *              *-------------------------------------------------*
      *              * Controllo su Codice azienda                     *
      *              *-------------------------------------------------*
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
           move      spaces               to   rr-cod-azi             .
       nor-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Esecuzione routine di conversione                         *
      *    *-----------------------------------------------------------*
       exe-cnv-fil-000.
      *              *-------------------------------------------------*
      *              * Inizializzazione rullino messaggi di foreground *
      *              *-------------------------------------------------*
           move      "OF"                 to   m-ope                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
       exe-cnv-fil-100.
      *              *-------------------------------------------------*
      *              * Inizializzazione tipo operazione da eseguire    *
      *              *-------------------------------------------------*
           if        rr-aut-man (2 : 1)   =    "1"
                     move  "PT"           to   f-xxx-ope
           else      move  "FP"           to   f-xxx-ope              .
       exe-cnv-fil-200.
       exe-cnv-fil-300.
      *              *-------------------------------------------------*
      *              * Tipo fase                                       *
      *              *-------------------------------------------------*
           move      "X"                  to   f-xxx-fas              .
      *              *-------------------------------------------------*
      *              * Record letti                                    *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Record letti               :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Record scritti                                  *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Record scritti             :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Assestamento fattura FPF                        *
      *              *-------------------------------------------------*
           perform   exe-ass-fpf-000      thru exe-ass-fpf-999        .
       exe-cnv-fil-800.
      *              *-------------------------------------------------*
      *              * Visualizzazione rullino messaggi                *
      *              *-------------------------------------------------*
           move      "VE"                 to   b-ope                  .
           move      "F"                  to   b-tfe                  .
           move      i-ide-des            to   b-chr                  .
           call      "swd/mod/prg/obj/mbckgv"
                                         using b                      .
           cancel    "swd/mod/prg/obj/mbckgv"                         .
       exe-cnv-fil-999.
           exit.

      *    *===========================================================*
      *    * Assestamento bolle clienti inevase                        *
      *    *-----------------------------------------------------------*
       exe-ass-fpf-000.
      *              *-------------------------------------------------*
      *              * Preparazione file area generica                 *
      *              *-------------------------------------------------*
           move      "fat "               to   f-xxx-nam              .
      *              *-------------------------------------------------*
      *              * Richiesta di conferma, se manuale               *
      *              *-------------------------------------------------*
           perform   ric-cnf-man-000      thru ric-cnf-man-999        .
      *              *-------------------------------------------------*
      *              * Se non confermato : ad uscita                   *
      *              *-------------------------------------------------*
           if        f-xxx-sts            not  = "S"
                     go to exe-ass-fpf-999.
      *              *-------------------------------------------------*
      *              * Messaggio di programma in esecuzione            *
      *              *-------------------------------------------------*
           move      "PE"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Numero records letti e scritti a zero           *
      *              *-------------------------------------------------*
           move      zero                 to   f-xxx-nrl              .
           move      zero                 to   f-xxx-nrs              .
       exe-ass-fpf-100.
      *              *-------------------------------------------------*
      *              * [kk1]                                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiesta di un pathname unico alla segre-  *
      *                  * teria                                       *
      *                  *---------------------------------------------*
           move      "UP"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-pat                to   f-kk1-pat              .
      *                  *---------------------------------------------*
      *                  * Open file                                   *
      *                  *---------------------------------------------*
           open      i-o    kk1                                       .
       exe-ass-fpf-120.
      *              *-------------------------------------------------*
      *              * [fit]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/fat/fls/ioc/obj/ioffit"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fit                 .
      *              *-------------------------------------------------*
      *              * [fir]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/fat/fls/ioc/obj/ioffir"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fir                 .
      *              *-------------------------------------------------*
      *              * [fix]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/fat/fls/ioc/obj/ioffix"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fix                 .
      *              *-------------------------------------------------*
      *              * [ftiprt]                                        *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/fat/num/ioc/obj/inftiprt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-fti-prt             .
       exe-ass-fpf-150.
      *              *-------------------------------------------------*
      *              * Subroutine di bufferizzazione testate e righe   *
      *              * ordini inevasi                                  *
      *              *-------------------------------------------------*
           perform   exe-ass-fpf-buf-000  thru exe-ass-fpf-buf-999    .
      *              *-------------------------------------------------*
      *              * Attribuzione numero protocollo fatture Italia   *
      *              * 'w-att-fti-prt-num'                             *
      *              *-------------------------------------------------*
           move      1020416              to   w-att-fti-prt-ddo      .
           perform   att-fti-prt-000      thru att-fti-prt-999        .
       exe-ass-fpf-180.
      *              *-------------------------------------------------*
      *              * Start su file [kk1]                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione chiave                      *
      *                  *---------------------------------------------*
           move      zero                 to   kk1-num-prt            .
           move      spaces               to   kk1-tip-rec            .
           move      zero                 to   kk1-num-prg            .
      *                  *---------------------------------------------*
      *                  * Operazione di Start                         *
      *                  *---------------------------------------------*
           start     kk1    key not less
                            kk1-key
                            invalid key
                            go to exe-ass-fpf-800.
       exe-ass-fpf-200.
      *              *-------------------------------------------------*
      *              * Lettura sequenziale file di appoggio [kk1]      *
      *              *-------------------------------------------------*
           read      kk1    next
                            with no lock
                            at end
                            go to exe-ass-fpf-800.
      *              *-------------------------------------------------*
      *              * Incremento numero records letti                 *
      *              *-------------------------------------------------*
           perform   inc-rec-let-000      thru inc-rec-let-999        .
       exe-ass-fpf-300.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo record          *
      *              *-------------------------------------------------*
           if        kk1-tip-rec          =    "R"
                     go to exe-ass-fpf-400
           else if   kk1-tip-rec          =    "T"
                     go to exe-ass-fpf-500
           else if   kk1-tip-rec          =    "X"
                     go to exe-ass-fpf-600
           else      go to exe-ass-fpf-700.
       exe-ass-fpf-400.
      *              *-------------------------------------------------*
      *              * Se record di riga                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Subroutine                                  *
      *                  *---------------------------------------------*
           perform   exe-ass-fpf-fir-000  thru exe-ass-fpf-fir-999    .
      *                  *---------------------------------------------*
      *                  * A riciclo                                   *
      *                  *---------------------------------------------*
           go to     exe-ass-fpf-700.
       exe-ass-fpf-500.
      *              *-------------------------------------------------*
      *              * Se record di testata                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Subroutine                                  *
      *                  *---------------------------------------------*
           perform   exe-ass-fpf-fit-000  thru exe-ass-fpf-fit-999    .
      *                  *---------------------------------------------*
      *                  * A riciclo                                   *
      *                  *---------------------------------------------*
           go to     exe-ass-fpf-700.
       exe-ass-fpf-600.
      *              *-------------------------------------------------*
      *              * Se record di estensione                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Subroutine                                  *
      *                  *---------------------------------------------*
           perform   exe-ass-fpf-fix-000  thru exe-ass-fpf-fix-999    .
      *                  *---------------------------------------------*
      *                  * A riciclo                                   *
      *                  *---------------------------------------------*
           go to     exe-ass-fpf-700.
       exe-ass-fpf-700.
      *              *-------------------------------------------------*
      *              * Riciclo a Next su [kk1]                         *
      *              *-------------------------------------------------*
           go to     exe-ass-fpf-200.
       exe-ass-fpf-800.
      *              *-------------------------------------------------*
      *              * [kk1]                                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Close file                                  *
      *                  *---------------------------------------------*
           close     kk1                                              .
      *                  *---------------------------------------------*
      *                  * Richiesta di delete di un pathname alla se- *
      *                  * greteria                                    *
      *                  *---------------------------------------------*
           move      "PD"                 to   s-ope                  .
           move      f-kk1-pat            to   s-pat                  .
           move      "K"                  to   s-sts                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
       exe-ass-fpf-820.
      *              *-------------------------------------------------*
      *              * [fit]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/fat/fls/ioc/obj/ioffit"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fit                 .
      *              *-------------------------------------------------*
      *              * [fir]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/fat/fls/ioc/obj/ioffir"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fir                 .
      *              *-------------------------------------------------*
      *              * [fix]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/fat/fls/ioc/obj/ioffix"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fix                 .
      *              *-------------------------------------------------*
      *              * [ftiprt]                                        *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/fat/num/ioc/obj/inftiprt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-fti-prt             .
       exe-ass-fpf-850.
      *              *-------------------------------------------------*
      *              * Visualizzazione nr records letti e scritti      *
      *              *-------------------------------------------------*
           perform   vis-rec-let-000      thru vis-rec-let-999        .
           perform   vis-rec-scr-000      thru vis-rec-scr-999        .
      *              *-------------------------------------------------*
      *              * Accettazione presa visione                      *
      *              *-------------------------------------------------*
           perform   acc-pre-vis-000      thru acc-pre-vis-999        .
       exe-ass-fpf-900.
      *              *-------------------------------------------------*
      *              * Scrittura rullino messaggi                      *
      *              *-------------------------------------------------*
           perform   wrt-rou-msg-000      thru wrt-rou-msg-999        .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-ass-fpf-999.
       exe-ass-fpf-999.
           exit.

      *    *===========================================================*
      *    * Assestamento bolle clienti inevase                        *
      *    *                                                           *
      *    * Subroutine di bufferizzazione testate                     *
      *    *-----------------------------------------------------------*
       exe-ass-fpf-buf-000.
      *              *-------------------------------------------------*
      *              * Start su [fit]                                  *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "IDEDOC     "        to   f-key                  .
           move      1010709              to   rf-fit-dat-doc         .
           move      01                   to   rf-fit-cod-dpz         .
           move      10100000032          to   rf-fit-num-doc         .
           move      "FPF  "              to   rf-fit-cod-tmo         .
           move      zero                 to   rf-fit-num-prt         .
           move      "pgm/fat/fls/ioc/obj/ioffit"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fit                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata                             *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-ass-fpf-buf-900.
       exe-ass-fpf-buf-200.
      *              *-------------------------------------------------*
      *              * Next su [fit]                                   *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/fat/fls/ioc/obj/ioffit"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fit                 .
      *                  *---------------------------------------------*
      *                  * Se fine file                                *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-ass-fpf-buf-900.
       exe-ass-fpf-buf-300.
      *              *-------------------------------------------------*
      *              * Test Max                                        *
      *              *-------------------------------------------------*
           if        rf-fit-dat-doc       not  = 1010709     or
                     rf-fit-cod-dpz       not  = 01          or
                     rf-fit-num-doc       not  = 10100000032 or
                     rf-fit-cod-tmo       not  = "FPF  "
                     go to exe-ass-fpf-buf-900.
       exe-ass-fpf-buf-400.
      *              *-------------------------------------------------*
      *              * Selezioni                                       *
      *              *-------------------------------------------------*
           if        rf-fit-cod-cli       not  = 0003873
                     go to exe-ass-fpf-buf-200.
       exe-ass-fpf-buf-600.
      *              *-------------------------------------------------*
      *              * Bufferizzazione                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Testata bolla                               *
      *                  *---------------------------------------------*
           move      rf-fit-num-prt       to   kk1-num-prt            .
           move      "T"                  to   kk1-tip-rec            .
           move      zero                 to   kk1-num-prg            .
      *                  *---------------------------------------------*
      *                  * Scrittura                                   *
      *                  *---------------------------------------------*
           write     kk1-rec                                          .
       exe-ass-fpf-buf-610.
      *                  *---------------------------------------------*
      *                  * Righe bolla                                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Start su [fir]                          *
      *                      *-----------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "NUMPRT    "         to   f-key                  .
           move      rf-fit-num-prt       to   rf-fir-num-prt         .
           move      zero                 to   rf-fir-num-prg         .
           move      "pgm/fat/fls/ioc/obj/ioffir"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fir                 .
      *                          *-------------------------------------*
      *                          * Se Start errata                     *
      *                          *-------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-ass-fpf-buf-750.
       exe-ass-fpf-buf-620.
      *                      *-----------------------------------------*
      *                      * Next su [fir]                           *
      *                      *-----------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/fat/fls/ioc/obj/ioffir"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fir                 .
      *                          *-------------------------------------*
      *                          * Se fine file                        *
      *                          *-------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-ass-fpf-buf-750.
       exe-ass-fpf-buf-630.
      *                      *-----------------------------------------*
      *                      * Test Max su [fir]                       *
      *                      *-----------------------------------------*
           if        rf-fir-num-prt       not  = rf-fit-num-prt
                     go to exe-ass-fpf-buf-750.
       exe-ass-fpf-buf-640.
      *                      *-----------------------------------------*
      *                      * Selezioni su [fir]                      *
      *                      *-----------------------------------------*
       exe-ass-fpf-buf-660.
      *                      *-----------------------------------------*
      *                      * Bufferizzazione                         *
      *                      *-----------------------------------------*
           move      rf-fir-num-prt       to   kk1-num-prt            .
           move      "R"                  to   kk1-tip-rec            .
           move      rf-fir-num-prg       to   kk1-num-prg            .
      *                      *-----------------------------------------*
      *                      * Scrittura                               *
      *                      *-----------------------------------------*
           write     kk1-rec                                          .
       exe-ass-fpf-buf-700.
      *              *-------------------------------------------------*
      *              * Riciclo su [fir]                                *
      *              *-------------------------------------------------*
           go to     exe-ass-fpf-buf-620.
       exe-ass-fpf-buf-750.
      *              *-------------------------------------------------*
      *              * [fix]                                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Righe aggiuntive                            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Start su [fix]                          *
      *                      *-----------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "NUMPRT    "         to   f-key                  .
           move      rf-fit-num-prt       to   rf-fix-num-prt         .
           move      zero                 to   rf-fix-num-prg         .
           move      zero                 to   rf-fix-tip-rec         .
           move      "pgm/fat/fls/ioc/obj/ioffix"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fix                 .
      *                          *-------------------------------------*
      *                          * Se Start errata                     *
      *                          *-------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-ass-fpf-buf-800.
       exe-ass-fpf-buf-760.
      *                      *-----------------------------------------*
      *                      * Next su [fix]                           *
      *                      *-----------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/fat/fls/ioc/obj/ioffix"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fix                 .
      *                          *-------------------------------------*
      *                          * Se fine file                        *
      *                          *-------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-ass-fpf-buf-800.
      *                      *-----------------------------------------*
      *                      * Test Max su [fix]                       *
      *                      *-----------------------------------------*
           if        rf-fix-num-prt       not  = rf-fit-num-prt
                     go to exe-ass-fpf-buf-800.
      *                      *-----------------------------------------*
      *                      * Bufferizzazione                         *
      *                      *-----------------------------------------*
           move      rf-fix-num-prt       to   kk1-num-prt            .
           move      "X"                  to   kk1-tip-rec            .
           move      rf-fix-num-prg       to   kk1-num-prg            .
      *                      *-----------------------------------------*
      *                      * Scrittura                               *
      *                      *-----------------------------------------*
           write     kk1-rec                                          .
      *              *-------------------------------------------------*
      *              * Riciclo su [fix]                                *
      *              *-------------------------------------------------*
           go to     exe-ass-fpf-buf-760.
       exe-ass-fpf-buf-800.
      *              *-------------------------------------------------*
      *              * Riciclo su [fit]                                *
      *              *-------------------------------------------------*
           go to     exe-ass-fpf-buf-200.
       exe-ass-fpf-buf-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-ass-fpf-buf-999.
       exe-ass-fpf-buf-999.
           exit.

      *    *===========================================================*
      *    * Assestamento bolle clienti inevase                        *
      *    *                                                           *
      *    * Subroutine di conversione righe                           *
      *    *-----------------------------------------------------------*
       exe-ass-fpf-fir-000.
      *              *-------------------------------------------------*
      *              * Operazioni preliminari                          *
      *              *-------------------------------------------------*
       exe-ass-fpf-fir-200.
      *              *-------------------------------------------------*
      *              * Lettura record [fir]                            *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      kk1-num-prt          to   rf-fir-num-prt         .
           move      kk1-num-prg          to   rf-fir-num-prg         .
           move      "pgm/fat/fls/ioc/obj/ioffir"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fir                 .
      *                  *---------------------------------------------*
      *                  * Se non trovato : uscita                     *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-ass-fpf-fir-900.
       exe-ass-fpf-fir-400.
      *              *-------------------------------------------------*
      *              * Conversioni                                     *
      *              *-------------------------------------------------*
           if        rf-fir-tip-rig
                    (01 : 01)             =   "C"
                     go to exe-ass-fpf-fir-700.
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione della valuta per la  *
      *                  * fatturazione                                *
      *                  *---------------------------------------------*
           if        rf-fir-sgl-vpf       =    "LIT"
                     go to exe-ass-fpf-fir-420
           else if   rf-fir-sgl-vpf       =    "EUR"
                     go to exe-ass-fpf-fir-430
           else      go to exe-ass-fpf-fir-450.
       exe-ass-fpf-fir-420.
      *                  *---------------------------------------------*
      *                  * Se Lire                                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Subroutine                              *
      *                      *-----------------------------------------*
           perform   exe-ass-fir-lit-000  thru exe-ass-fir-lit-999    .
      *                      *-----------------------------------------*
      *                      * A fasi comuni                           *
      *                      *-----------------------------------------*
           go to     exe-ass-fpf-fir-700.
       exe-ass-fpf-fir-430.
      *                  *---------------------------------------------*
      *                  * Se Euro                                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Subroutine                              *
      *                      *-----------------------------------------*
           perform   exe-ass-fir-eur-000  thru exe-ass-fir-eur-999    .
      *                      *-----------------------------------------*
      *                      * A fasi comuni                           *
      *                      *-----------------------------------------*
           go to     exe-ass-fpf-fir-700.
       exe-ass-fpf-fir-450.
      *                  *---------------------------------------------*
      *                  * Se Altro                                    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     exe-ass-fpf-fir-900.
       exe-ass-fpf-fir-700.
      *              *-------------------------------------------------*
      *              * Fasi comuni                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Protocollo fattura                          *
      *                  *---------------------------------------------*
           move      w-att-fti-prt-num    to   rf-fir-num-prt         .
           move      1020416              to   rf-fir-dat-doc         .
           move      10200000432          to   rf-fir-num-doc         .
       exe-ass-fpf-fir-800.
      *              *-------------------------------------------------*
      *              * Scrittura                                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Incremento numero records scritti           *
      *                  *---------------------------------------------*
           perform   inc-rec-scr-000      thru inc-rec-scr-999        .
      *                  *---------------------------------------------*
      *                  * Put record [fir]                            *
      *                  *---------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      "pgm/fat/fls/ioc/obj/ioffir"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fir                 .
       exe-ass-fpf-fir-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-ass-fpf-fir-999.
       exe-ass-fpf-fir-999.
           exit.

      *    *===========================================================*
      *    * Assestamento bolle clienti inevase                        *
      *    *                                                           *
      *    * Subroutine di conversione righe                           *
      *    *                                                           *
      *    * Subroutine di conversione righe in Lire                   *
      *    *-----------------------------------------------------------*
       exe-ass-fir-lit-000.
      *              *-------------------------------------------------*
      *              * Valuta per la fatturazione                      *
      *              *-------------------------------------------------*
           move      "EUR"                to   rf-fir-sgl-vpf         .
           move      2                    to   rf-fir-dec-vpf         .
           move      "*"                  to   rf-fir-tdc-vpf         .
           move      000001,00000         to   rf-fir-cdc-vpf         .
       exe-ass-fir-lit-100.
      *              *-------------------------------------------------*
      *              * Valuta per l'espressione dei prezzi standard    *
      *              *-------------------------------------------------*
           move      "EUR"                to   rf-fir-sgl-vps         .
           move      2                    to   rf-fir-dec-vps         .
           move      "*"                  to   rf-fir-tdc-vps         .
           move      000001,00000         to   rf-fir-cdc-vps         .
       exe-ass-fir-lit-300.
      *              *-------------------------------------------------*
      *              * Prezzi                                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Prezzo lordo standard                       *
      *                  *---------------------------------------------*
           move      rf-fir-prz-lrs       to   w-wrk-cmd-prz          .
      *
           move      w-wrk-cmd-prz        to   w-cal-imp-eur-imi      .
           move      zero                 to   w-cal-imp-eur-dec      .
           perform   cal-imp-eur-000      thru cal-imp-eur-999        .
           move      w-cal-imp-eur-ime    to   rf-fir-prz-lrs         .
      *                  *---------------------------------------------*
      *                  * Prezzo netto standard                       *
      *                  *---------------------------------------------*
           move      rf-fir-prz-nts       to   w-wrk-cmd-prz          .
      *
           move      w-wrk-cmd-prz        to   w-cal-imp-eur-imi      .
           move      zero                 to   w-cal-imp-eur-dec      .
           perform   cal-imp-eur-000      thru cal-imp-eur-999        .
           move      w-cal-imp-eur-ime    to   rf-fir-prz-nts         .
      *                  *---------------------------------------------*
      *                  * Prezzo unitario di vendita                  *
      *                  *---------------------------------------------*
           move      rf-fir-prz-ven       to   w-wrk-cmd-prz          .
      *
           move      w-wrk-cmd-prz        to   w-cal-imp-eur-imi      .
           move      zero                 to   w-cal-imp-eur-dec      .
           perform   cal-imp-eur-000      thru cal-imp-eur-999        .
           move      w-cal-imp-eur-ime    to   rf-fir-prz-ven         .
      *                  *---------------------------------------------*
      *                  * Prezzo netto effettivo                      *
      *                  *---------------------------------------------*
           move      rf-fir-prz-net       to   w-wrk-cmd-prz          .
      *
           move      w-wrk-cmd-prz        to   w-cal-imp-eur-imi      .
           move      zero                 to   w-cal-imp-eur-dec      .
           perform   cal-imp-eur-000      thru cal-imp-eur-999        .
           move      w-cal-imp-eur-ime    to   rf-fir-prz-net         .
       exe-ass-fir-lit-400.
      *              *-------------------------------------------------*
      *              * Valuta per l'espressione dei prezzi di vendita  *
      *              *-------------------------------------------------*
           move      "EUR"                to   rf-fir-sgl-vpp         .
           move      2                    to   rf-fir-dec-vpp         .
           move      "*"                  to   rf-fir-tdc-vpp         .
           move      000001,00000         to   rf-fir-cdc-vpp         .
       exe-ass-fir-lit-500.
      *              *-------------------------------------------------*
      *              * Legame valutario                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se presente                            *
      *                  *---------------------------------------------*
           if        rf-fir-sgl-vpl       =    spaces
                     go to exe-ass-fir-lit-600.
      *                  *---------------------------------------------*
      *                  * Prezzo di riferimento (quello netto)        *
      *                  *---------------------------------------------*
           move      rf-fir-prz-net       to   rf-fir-prz-vpl         .
       exe-ass-fir-lit-600.
      *              *-------------------------------------------------*
      *              * Valuta per l'espressione del costo riferimento  *
      *              *-------------------------------------------------*
           move      "EUR"                to   rf-fir-sgl-vpc         .
           move      2                    to   rf-fir-dec-vpc         .
           move      "*"                  to   rf-fir-tdc-vpc         .
           move      000001,00000         to   rf-fir-cdc-vpc         .
       exe-ass-fir-lit-700.
      *              *-------------------------------------------------*
      *              * Costo di riferimento                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Prezzo di riferimento                       *
      *                  *---------------------------------------------*
           move      rf-fir-cos-rif       to   w-cal-imp-eur-imi      .
           move      zero                 to   w-cal-imp-eur-dec      .
           perform   cal-imp-eur-000      thru cal-imp-eur-999        .
           move      w-cal-imp-eur-ime    to   rf-fir-cos-rif         .
       exe-ass-fir-lit-800.
      *              *-------------------------------------------------*
      *              * Importi                                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Importo definitivo per la riga              *
      *                  *---------------------------------------------*
           multiply  rf-fir-qta-ven       by   rf-fir-prz-net
                                        giving rf-fir-imp-rig         .
      *
           if        rf-fir-dec-prz       =    1
                     divide 10            into rf-fir-imp-rig
           else if   rf-fir-dec-prz       =    2
                     divide 100           into rf-fir-imp-rig         .
      *                  *---------------------------------------------*
      *                  * Importo ausiliario per la riga              *
      *                  *---------------------------------------------*
           move      rf-fir-imp-rig       to   rf-fir-iau-rig         .
       exe-ass-fir-lit-850.
      *              *-------------------------------------------------*
      *              * Flag di prezzo unitario                         *
      *              *-------------------------------------------------*
       exe-ass-fir-lit-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-ass-fir-lit-999.
       exe-ass-fir-lit-999.
           exit.

      *    *===========================================================*
      *    * Assestamento bolle clienti inevase                        *
      *    *                                                           *
      *    * Subroutine di conversione righe                           *
      *    *                                                           *
      *    * Subroutine di conversione righe in Euro                   *
      *    *-----------------------------------------------------------*
       exe-ass-fir-eur-000.
      *              *-------------------------------------------------*
      *              * Valuta per la fatturazione                      *
      *              *-------------------------------------------------*
           move      000001,00000         to   rf-fir-cdc-vpf         .
       exe-ass-fir-eur-100.
      *              *-------------------------------------------------*
      *              * Valuta per l'espressione dei prezzi standard    *
      *              *-------------------------------------------------*
           move      000001,00000         to   rf-fir-cdc-vps         .
       exe-ass-fir-eur-300.
      *              *-------------------------------------------------*
      *              * Prezzi                                          *
      *              *-------------------------------------------------*
       exe-ass-fir-eur-400.
      *              *-------------------------------------------------*
      *              * Valuta per l'espressione dei prezzi di vendita  *
      *              *-------------------------------------------------*
           move      000001,00000         to   rf-fir-cdc-vpp         .
       exe-ass-fir-eur-500.
      *              *-------------------------------------------------*
      *              * Legame valutario                                *
      *              *-------------------------------------------------*
       exe-ass-fir-eur-600.
      *              *-------------------------------------------------*
      *              * Valuta per l'espressione del costo riferimento  *
      *              *-------------------------------------------------*
           move      000001,00000         to   rf-fir-cdc-vpc         .
       exe-ass-fir-eur-700.
      *              *-------------------------------------------------*
      *              * Costo di riferimento                            *
      *              *-------------------------------------------------*
       exe-ass-fir-eur-800.
      *              *-------------------------------------------------*
      *              * Importi                                         *
      *              *-------------------------------------------------*
       exe-ass-fir-eur-850.
       exe-ass-fir-eur-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-ass-fir-eur-999.
       exe-ass-fir-eur-999.
           exit.

      *    *===========================================================*
      *    * Assestamento bolle clienti inevase                        *
      *    *                                                           *
      *    * Subroutine di conversione righe                           *
      *    *-----------------------------------------------------------*
       exe-ass-fpf-fix-000.
      *              *-------------------------------------------------*
      *              * Operazioni preliminari                          *
      *              *-------------------------------------------------*
       exe-ass-fpf-fix-200.
      *              *-------------------------------------------------*
      *              * Lettura record [fix]                            *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      kk1-num-prt          to   rf-fix-num-prt         .
           move      kk1-num-prg          to   rf-fix-num-prg         .
           move      01                   to   rf-fix-tip-rec         .
           move      "pgm/fat/fls/ioc/obj/ioffix"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fix                 .
      *                  *---------------------------------------------*
      *                  * Se non trovato : uscita                     *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-ass-fpf-fix-900.
       exe-ass-fpf-fix-400.
      *              *-------------------------------------------------*
      *              * Conversioni                                     *
      *              *-------------------------------------------------*
       exe-ass-fpf-fix-700.
      *              *-------------------------------------------------*
      *              * Fasi comuni                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Protocollo fattura                          *
      *                  *---------------------------------------------*
           move      w-att-fti-prt-num    to   rf-fix-num-prt         .
       exe-ass-fpf-fix-800.
      *              *-------------------------------------------------*
      *              * Scrittura                                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Incremento numero records scritti           *
      *                  *---------------------------------------------*
           perform   inc-rec-scr-000      thru inc-rec-scr-999        .
      *                  *---------------------------------------------*
      *                  * Put record [fix]                            *
      *                  *---------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      "pgm/fat/fls/ioc/obj/ioffix"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fix                 .
       exe-ass-fpf-fix-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-ass-fpf-fix-999.
       exe-ass-fpf-fix-999.
           exit.

      *    *===========================================================*
      *    * Assestamento bolle clienti inevase                        *
      *    *                                                           *
      *    * Subroutine di conversione testate                         *
      *    *-----------------------------------------------------------*
       exe-ass-fpf-fit-000.
      *              *-------------------------------------------------*
      *              * Lettura record [fit]                            *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      kk1-num-prt          to   rf-fit-num-prt         .
           move      "pgm/fat/fls/ioc/obj/ioffit"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fit                 .
      *                  *---------------------------------------------*
      *                  * Se non trovato : uscita                     *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-ass-fpf-fit-900.
       exe-ass-fpf-fit-400.
      *              *-------------------------------------------------*
      *              * Conversioni                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione della valuta per la  *
      *                  * fatturazione                                *
      *                  *---------------------------------------------*
           if        rf-fit-sgl-vpf       =    "LIT"
                     go to exe-ass-fpf-fit-420
           else if   rf-fit-sgl-vpf       =    "EUR"
                     go to exe-ass-fpf-fit-430
           else      go to exe-ass-fpf-fit-450.
       exe-ass-fpf-fit-420.
      *                  *---------------------------------------------*
      *                  * Se Lire                                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Subroutine                              *
      *                      *-----------------------------------------*
           perform   exe-ass-fit-lit-000  thru exe-ass-fit-lit-999    .
      *                      *-----------------------------------------*
      *                      * A fasi comuni                           *
      *                      *-----------------------------------------*
           go to     exe-ass-fpf-fit-700.
       exe-ass-fpf-fit-430.
      *                  *---------------------------------------------*
      *                  * Se Euro                                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Subroutine                              *
      *                      *-----------------------------------------*
           perform   exe-ass-fit-eur-000  thru exe-ass-fit-eur-999    .
      *                      *-----------------------------------------*
      *                      * A fasi comuni                           *
      *                      *-----------------------------------------*
           go to     exe-ass-fpf-fit-700.
       exe-ass-fpf-fit-450.
      *                  *---------------------------------------------*
      *                  * Se Altro                                    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     exe-ass-fpf-fit-900.
       exe-ass-fpf-fit-700.
      *              *-------------------------------------------------*
      *              * Fasi comuni                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Protocollo fattura                          *
      *                  *---------------------------------------------*
           move      w-att-fti-prt-num    to   rf-fit-num-prt         .
           move      1020416              to   rf-fit-dat-doc         .
           move      10200000432          to   rf-fit-num-doc         .
       exe-ass-fpf-fit-800.
      *              *-------------------------------------------------*
      *              * Scrittura                                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Incremento numero records scritti           *
      *                  *---------------------------------------------*
           perform   inc-rec-scr-000      thru inc-rec-scr-999        .
      *                  *---------------------------------------------*
      *                  * Scrittura record [fit]                      *
      *                  *---------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      "pgm/fat/fls/ioc/obj/ioffit"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fit                 .
       exe-ass-fpf-fit-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-ass-fpf-fit-999.
       exe-ass-fpf-fit-999.
           exit.

      *    *===========================================================*
      *    * Assestamento bolle clienti inevase                        *
      *    *                                                           *
      *    * Subroutine di conversione testate                         *
      *    *                                                           *
      *    * Subroutine di conversione testate in Lire                 *
      *    *-----------------------------------------------------------*
       exe-ass-fit-lit-000.
      *              *-------------------------------------------------*
      *              * Valuta per la fatturazione                      *
      *              *-------------------------------------------------*
           move      "EUR"                to   rf-fit-sgl-vpf         .
           move      2                    to   rf-fit-dec-vpf         .
           move      "*"                  to   rf-fit-tdc-vpf         .
           move      000001,00000         to   rf-fit-cdc-vpf         .
      *              *-------------------------------------------------*
      *              * Ammontare della quota a forfait                 *
      *              *-------------------------------------------------*
           move      rf-fit-pag-qaf       to   w-cal-imp-eur-imi      .
           move      zero                 to   w-cal-imp-eur-dec      .
           perform   cal-imp-eur-000      thru cal-imp-eur-999        .
           move      w-cal-imp-eur-ime    to   rf-fit-pag-qaf         .
      *              *-------------------------------------------------*
      *              * Ammontare acconto                               *
      *              *-------------------------------------------------*
           move      rf-fit-pag-act       to   w-cal-imp-eur-imi      .
           move      zero                 to   w-cal-imp-eur-dec      .
           perform   cal-imp-eur-000      thru cal-imp-eur-999        .
           move      w-cal-imp-eur-ime    to   rf-fit-pag-act         .
       exe-ass-fit-lit-100.
           move      zero                 to   w-inx                  .
       exe-ass-fit-lit-200.
           add       1                    to   w-inx                  .
           if        w-inx                >    9
                     go to exe-ass-fit-lit-300.
      *              *-------------------------------------------------*
      *              * Totale                                          *
      *              *-------------------------------------------------*
           move      rf-fit-tot-rig (w-inx)
                                          to   w-cal-imp-eur-imi      .
           move      zero                 to   w-cal-imp-eur-dec      .
           perform   cal-imp-eur-000      thru cal-imp-eur-999        .
           move      w-cal-imp-eur-ime    to   rf-fit-tot-rig (w-inx) .
           go to     exe-ass-fit-lit-200.
       exe-ass-fit-lit-300.
      *              *-------------------------------------------------*
      *              * Importo sconto in chiusura                      *
      *              *-------------------------------------------------*
           move      rf-fit-tot-scc       to   w-cal-imp-eur-imi      .
           move      zero                 to   w-cal-imp-eur-dec      .
           perform   cal-imp-eur-000      thru cal-imp-eur-999        .
           move      w-cal-imp-eur-ime    to   rf-fit-tot-scc         .
      *              *-------------------------------------------------*
      *              * Importo sconto pagamento                        *
      *              *-------------------------------------------------*
           move      rf-fit-tot-scp       to   w-cal-imp-eur-imi      .
           move      zero                 to   w-cal-imp-eur-dec      .
           perform   cal-imp-eur-000      thru cal-imp-eur-999        .
           move      w-cal-imp-eur-ime    to   rf-fit-tot-scp         .
       exe-ass-fit-lit-400.
           move      zero                 to   w-inx                  .
       exe-ass-fit-lit-500.
           add       1                    to   w-inx                  .
           if        w-inx                >    6
                     go to exe-ass-fit-lit-600.
      *              *-------------------------------------------------*
      *              * Spesa                                           *
      *              *-------------------------------------------------*
           move      rf-fit-spe-imp (w-inx)
                                          to   w-cal-imp-eur-imi      .
           move      zero                 to   w-cal-imp-eur-dec      .
           perform   cal-imp-eur-000      thru cal-imp-eur-999        .
           move      w-cal-imp-eur-ime    to   rf-fit-spe-imp (w-inx) .
           go to     exe-ass-fit-lit-500.
       exe-ass-fit-lit-600.
      *              *-------------------------------------------------*
      *              * Castelletto iva                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ciclo 1..6                                  *
      *                  *---------------------------------------------*
           move      zero                 to   w-inx                  .
       exe-ass-fit-lit-620.
           add       1                    to   w-inx                  .
           if        w-inx                >    6
                     go to exe-ass-fit-lit-700.
      *                  *---------------------------------------------*
      *                  * Imponibile iva, in valuta                   *
      *                  *---------------------------------------------*
           move      rf-fit-iva-ibl (w-inx)
                                          to   w-cal-imp-eur-imi      .
           move      zero                 to   w-cal-imp-eur-dec      .
           perform   cal-imp-eur-000      thru cal-imp-eur-999        .
           move      w-cal-imp-eur-ime    to   rf-fit-iva-ibl (w-inx) .
      *                  *---------------------------------------------*
      *                  * Imposta iva, in valuta                      *
      *                  *---------------------------------------------*
           move      rf-fit-iva-imp (w-inx)
                                          to   w-cal-imp-eur-imi      .
           move      zero                 to   w-cal-imp-eur-dec      .
           perform   cal-imp-eur-000      thru cal-imp-eur-999        .
           move      w-cal-imp-eur-ime    to   rf-fit-iva-imp (w-inx) .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     exe-ass-fit-lit-620.
       exe-ass-fit-lit-700.
      *              *-------------------------------------------------*
      *              * Totale documento                                *
      *              *-------------------------------------------------*
           move      rf-fit-iva-tdo       to   w-cal-imp-eur-imi      .
           move      zero                 to   w-cal-imp-eur-dec      .
           perform   cal-imp-eur-000      thru cal-imp-eur-999        .
           move      w-cal-imp-eur-ime    to   rf-fit-iva-tdo         .
       exe-ass-fit-lit-800.
      *              *-------------------------------------------------*
      *              * Castelletto contropartite                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ciclo 1..10                                 *
      *                  *---------------------------------------------*
           move      zero                 to   w-inx                  .
       exe-ass-fit-lit-820.
           add       1                    to   w-inx                  .
           if        w-inx                >    10
                     go to exe-ass-fit-lit-900.
      *                  *---------------------------------------------*
      *                  * Importo contropartita, in valuta            *
      *                  *---------------------------------------------*
           move      rf-fit-ctp-imp (w-inx)
                                          to   w-cal-imp-eur-imi      .
           move      zero                 to   w-cal-imp-eur-dec      .
           perform   cal-imp-eur-000      thru cal-imp-eur-999        .
           move      w-cal-imp-eur-ime    to   rf-fit-ctp-imp (w-inx) .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     exe-ass-fit-lit-820.
       exe-ass-fit-lit-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-ass-fit-lit-999.
       exe-ass-fit-lit-999.
           exit.

      *    *===========================================================*
      *    * Assestamento bolle clienti inevase                        *
      *    *                                                           *
      *    * Subroutine di conversione testate                         *
      *    *                                                           *
      *    * Subroutine di conversione testate in Euro                 *
      *    *-----------------------------------------------------------*
       exe-ass-fit-eur-000.
      *              *-------------------------------------------------*
      *              * Coefficiente di cambio                          *
      *              *-------------------------------------------------*
           move      000001,00000         to   rf-fit-cdc-vpf         .
       exe-ass-fit-eur-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-ass-fit-eur-999.
       exe-ass-fit-eur-999.
           exit.

      *    *===========================================================*
      *    * Richiesta di conferma, se manuale                         *
      *    *-----------------------------------------------------------*
       ric-cnf-man-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione della fase               *
      *              *-------------------------------------------------*
           if        f-xxx-fas            =    "X"
                     go to ric-cnf-man-100.
       ric-cnf-man-050.
      *              *-------------------------------------------------*
      *              * Visualizzazione copia in corso                  *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      spaces               to   v-alf
           string    "Copia archivio ["
                                delimited by   size
                     f-xxx-nam  delimited by   spaces
                     "] "       delimited by   size
                     f-xxx-des  delimited by   size
                                          into v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           go to     ric-cnf-man-120.
       ric-cnf-man-100.
      *              *-------------------------------------------------*
      *              * Visualizzazione conversione in corso            *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      spaces               to   v-alf
           string    "Conversione relativa all'archivio ["
                                delimited by   size
                     f-xxx-nam  delimited by   spaces
                     "]"        delimited by   size
                                          into v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           go to     ric-cnf-man-120.
       ric-cnf-man-120.
      *              *-------------------------------------------------*
      *              * Visualizzazione a spaces area per conferma      *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Se funzionamento automatico : uscita con Ok     *
      *              *-------------------------------------------------*
           if        rr-aut-man (1 : 1)   not  = "M"
                     move  "S"            to   f-xxx-sts
                     go to ric-cnf-man-999.
      *              *-------------------------------------------------*
      *              * Visualizzazione prompt per conferma             *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Conferma conversione       :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Normalizzazione risposta                        *
      *              *-------------------------------------------------*
           move      spaces               to   f-xxx-sts              .
       ric-cnf-man-200.
      *              *-------------------------------------------------*
      *              * Accettazione risposta Si/No                     *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-ris-snx-lun    to   v-car                  .
           move      w-exp-ris-snx-num    to   v-ldt                  .
           move      "SN#"                to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      w-exp-ris-snx-tbl    to   v-txt                  .
           move      12                   to   v-lin                  .
           move      30                   to   v-pos                  .
           if        f-xxx-sts             =    "S"
                     move  01             to   v-num
           else if   f-xxx-sts            =    "N"
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       ric-cnf-man-300.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        v-num                =    01
                     move  "S"            to   f-xxx-sts
           else if   v-num                =    02
                     move  "N"            to   f-xxx-sts
           else      move  spaces         to   f-xxx-sts              .
       ric-cnf-man-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
           if        f-xxx-sts            not  = "S" and
                     f-xxx-sts            not  = "N"
                     go to ric-cnf-man-200.
       ric-cnf-man-999.
           exit.

      *    *===========================================================*
      *    * Scrittura rullino messaggi                                *
      *    *-----------------------------------------------------------*
       wrt-rou-msg-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione della fase               *
      *              *-------------------------------------------------*
           if        f-xxx-fas            =    "X"
                     go to wrt-rou-msg-200.
       wrt-rou-msg-100.
      *              *-------------------------------------------------*
      *              * Rullino messaggi                                *
      *              *-------------------------------------------------*
           move      "WR"                 to   m-ope                  .
           move      spaces               to   m-msg                  .
           string    "Copia archivio ["
                                delimited by   size
                     f-xxx-nam  delimited by   spaces
                     "] "       delimited by   size
                     f-xxx-des  delimited by   size
                                          into m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     wrt-rou-msg-999.
       wrt-rou-msg-200.
      *              *-------------------------------------------------*
      *              * Rullino messaggi                                *
      *              *-------------------------------------------------*
           move      "WR"                 to   m-ope                  .
           move      spaces               to   m-msg                  .
           string    "Conversione archivio ["
                                delimited by   size
                     f-xxx-nam  delimited by   spaces
                     "]"        delimited by   size
                                          into m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
      *
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      09                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<"                  to   v-edm                  .
           move      f-xxx-nrl            to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "WR"                 to   m-ope                  .
           move      spaces               to   m-msg                  .
           string    "  - Numero records letti   : "
                                delimited by   size
                     v-edt      delimited by   spaces
                                          into m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
      *
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      09                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<"                  to   v-edm                  .
           move      f-xxx-nrs            to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "WR"                 to   m-ope                  .
           move      spaces               to   m-msg                  .
           string    "  - Numero records scritti : "
                                delimited by   size
                     v-edt      delimited by   spaces
                                          into m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
      *
           move      "WR"                 to   m-ope                  .
           move      spaces               to   m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
       wrt-rou-msg-999.
           exit.

      *    *===========================================================*
      *    * Accettazione presa visione                                *
      *    *-----------------------------------------------------------*
       acc-pre-vis-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione della fase               *
      *              *-------------------------------------------------*
           if        f-xxx-fas            =    "X"
                     go to acc-pre-vis-200.
       acc-pre-vis-100.
      *              *-------------------------------------------------*
      *              * Messaggio di fine copia archivio                *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      spaces               to   v-alf
           string    "Fine copia relativa all'archivio ["
                                delimited by   size
                     f-xxx-nam  delimited by   spaces
                     "]"                  delimited by   size
                                          into v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           go to     acc-pre-vis-300.
       acc-pre-vis-200.
      *              *-------------------------------------------------*
      *              * Messaggio di fine conversione archivio          *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      spaces               to   v-alf
           string    "Fine conversione relativa all'archivio ["
                                delimited by   size
                     f-xxx-nam  delimited by   spaces
                     "]"                  delimited by   size
                                          into v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           go to     acc-pre-vis-300.
       acc-pre-vis-300.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda se funzionamento automa-   *
      *              * tico o manuale                                  *
      *              *-------------------------------------------------*
           if        rr-aut-man (1 : 1)   =    "M"
                     go to acc-pre-vis-600
           else      go to acc-pre-vis-400.
       acc-pre-vis-400.
      *              *-------------------------------------------------*
      *              * Se funzionamento automatico                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Attesa di 1 secondo                         *
      *                  *---------------------------------------------*
           call      "swd/mod/prg/obj/mwait0"                         .
      *                  *---------------------------------------------*
      *                  * Pulizia linea 20                            *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Cancellazione messaggio di programma in     *
      *                  * esecuzione                                  *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      24                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      spaces               to   v-alf
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     acc-pre-vis-999.
       acc-pre-vis-600.
      *              *-------------------------------------------------*
      *              * Se funzionamento manuale                        *
      *              *-------------------------------------------------*
       acc-pre-vis-610.
      *                  *---------------------------------------------*
      *                  * Cancellazione messaggio di programma in     *
      *                  * esecuzione                                  *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      24                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      spaces               to   v-alf
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Normalizzazione presa visione               *
      *                  *---------------------------------------------*
           move      spaces               to   rr-pre-vis             .
      *                  *---------------------------------------------*
      *                  * Prompt per presa visione                    *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      35                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      46                   to   v-pos                  .
           move      " - Digitare OK : [  ]              "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-pre-vis-620.
      *                  *---------------------------------------------*
      *                  * Accettazione presa visione                  *
      *                  *---------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      64                   to   v-pos                  .
           move      rr-pre-vis           to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-alf                to   rr-pre-vis             .
           if        rr-pre-vis           not  = "OK"
                     go to acc-pre-vis-620.
       acc-pre-vis-630.
      *                  *---------------------------------------------*
      *                  * Pulizia linea 20                            *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     acc-pre-vis-999.
       acc-pre-vis-999.
           exit.

      *    *===========================================================*
      *    * Incremento numero records letti                           *
      *    *-----------------------------------------------------------*
       inc-rec-let-000.
      *              *-------------------------------------------------*
      *              * Incremento                                      *
      *              *-------------------------------------------------*
           add       1                    to   f-xxx-nrl              .
      *              *-------------------------------------------------*
      *              * Deviazione                                      *
      *              *-------------------------------------------------*
           if        f-xxx-nrl            >    100
                     go to inc-rec-let-100
           else if   f-xxx-nrl            >    10
                     go to inc-rec-let-200
           else      go to inc-rec-let-500.
       inc-rec-let-100.
      *              *-------------------------------------------------*
      *              * Se maggiore di 100                              *
      *              *-------------------------------------------------*
           move      f-xxx-nrl            to   f-xxx-nrc              .
           if        f-xxx-nrc            =    zero
                     go to inc-rec-let-500
           else      go to inc-rec-let-999.
       inc-rec-let-200.
      *              *-------------------------------------------------*
      *              * Se maggiore di 10                               *
      *              *-------------------------------------------------*
           move      f-xxx-nrl            to   f-xxx-nrd              .
           if        f-xxx-nrd            =    zero
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
      *    * Incremento numero records scritti                         *
      *    *-----------------------------------------------------------*
       inc-rec-scr-000.
      *              *-------------------------------------------------*
      *              * Incremento                                      *
      *              *-------------------------------------------------*
           add       1                    to   f-xxx-nrs              .
      *              *-------------------------------------------------*
      *              * Deviazione                                      *
      *              *-------------------------------------------------*
           if        f-xxx-nrs            >    100
                     go to inc-rec-scr-100
           else if   f-xxx-nrs            >    10
                     go to inc-rec-scr-200
           else      go to inc-rec-scr-500.
       inc-rec-scr-100.
      *              *-------------------------------------------------*
      *              * Se maggiore di 100                              *
      *              *-------------------------------------------------*
           move      f-xxx-nrs            to   f-xxx-nrc              .
           if        f-xxx-nrc            =    zero
                     go to inc-rec-scr-500
           else      go to inc-rec-scr-999.
       inc-rec-scr-200.
      *              *-------------------------------------------------*
      *              * Se maggiore di 10                               *
      *              *-------------------------------------------------*
           move      f-xxx-nrs            to   f-xxx-nrd              .
           if        f-xxx-nrd            =    zero
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
      *    * Visualizzazione numero records letti                      *
      *    *-----------------------------------------------------------*
       vis-rec-let-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      09                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<"                  to   v-edm                  .
           move      16                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      f-xxx-nrl            to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-rec-let-999.
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
           move      18                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      f-xxx-nrs            to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-rec-scr-999.
           exit.

      *    *===========================================================*
      *    * Routine di attribuzione numero protocollo fatture Italia  *
      *    *-----------------------------------------------------------*
       att-fti-prt-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione record                          *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/fat/num/ioc/obj/inftiprt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-fti-prt             .
      *              *-------------------------------------------------*
      *              * Preparazione chiave per lettura tabella numera- *
      *              * zioni [ftiprt]                                  *
      *              *-------------------------------------------------*
           move      w-att-fti-prt-ddo    to   s-dat                  .
           move      s-saa                to   w-att-fti-prt-saa      .
           move      w-att-fti-prt-saa    to   rn-fti-prt-scl-ann     .
      *              *-------------------------------------------------*
      *              * Lettura tabella numerazioni [ftiprt]            *
      *              *-------------------------------------------------*
           move      "GT"                 to   f-ope                  .
           move      "pgm/fat/num/ioc/obj/inftiprt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-fti-prt             .
      *              *-------------------------------------------------*
      *              * Test su esito operazione                        *
      *              *-------------------------------------------------*
           if        f-sts                =    e-not-err
                     go to att-fti-prt-100.
      *                  *---------------------------------------------*
      *                  * Record non trovato                          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Composizione record normalizzato        *
      *                      *-----------------------------------------*
           move      zero                 to   rn-fti-prt-num-prt     .
      *                      *-----------------------------------------*
      *                      * Scrittura record normalizzato           *
      *                      *-----------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      "pgm/fat/num/ioc/obj/inftiprt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-fti-prt             .
      *                      *-----------------------------------------*
      *                      * Ripetizione dell'intera operazione      *
      *                      *-----------------------------------------*
           go to     att-fti-prt-000.
       att-fti-prt-100.
      *                  *---------------------------------------------*
      *                  * Record trovato                              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Numero protocollo in work di comodo     *
      *                      *-----------------------------------------*
           move      rn-fti-prt-num-prt   to   w-att-fti-prt-wnu      .
      *                      *-----------------------------------------*
      *                      * Aggiornamento numero protocollo         *
      *                      *-----------------------------------------*
           add       1                    to   w-att-fti-prt-wpr      .
           if        w-att-fti-prt-wpr    =    zero
                     move  1              to   w-att-fti-prt-wpr      .
      *                      *-----------------------------------------*
      *                      * Forzatura secolo/anno                   *
      *                      *-----------------------------------------*
           move      w-att-fti-prt-saa    to   w-att-fti-prt-wsa      .
      *                      *-----------------------------------------*
      *                      * Bufferizzazione in record [ftiprt]      *
      *                      *-----------------------------------------*
           move      w-att-fti-prt-wnu    to   rn-fti-prt-num-prt     .
      *                      *-----------------------------------------*
      *                      * Update record                           *
      *                      *-----------------------------------------*
           move      "UP"                 to   f-ope                  .
           move      "pgm/fat/num/ioc/obj/inftiprt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-fti-prt             .
      *                      *-----------------------------------------*
      *                      * Unlock record                           *
      *                      *-----------------------------------------*
           move      "RL"                 to   f-ope                  .
           move      "pgm/fat/num/ioc/obj/inftiprt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-fti-prt             .
      *                      *-----------------------------------------*
      *                      * Se errori ripete l'intera operazione    *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to att-fti-prt-000.
      *                      *-----------------------------------------*
      *                      * Valore in uscita                        *
      *                      *-----------------------------------------*
           move      rn-fti-prt-num-prt   to   w-att-fti-prt-num      .
       att-fti-prt-999.
           exit.

      *    *===========================================================*
      *    * Routines per la conversione                               *
      *    *-----------------------------------------------------------*
           copy      "cnv/cnv/prg/cpy/cnveur01.wks"                   .

