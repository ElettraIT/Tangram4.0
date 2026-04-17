       Identification Division.
       Program-Id.                                 pmag4600           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    mag                 *
      *                                Settore:    sit                 *
      *                                   Fase:    mag460              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 04/12/93    *
      *                       Ultima revisione:    NdK del 08/02/22    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Richieste per il programma pmag4601:        *
      *                                                                *
      *                    Situazione giacenze di proprieta'           *
      *                    valorizzate prodotti di vendita             *
      *                                                                *
      *                    ------------------------------------------- *
      *                                                                *
      *                    File di esportazione :                      *
      *                                                                *
      *                    /abd/asc/UTENTE/exp/AZI_mag460.csv          *
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
                     "mag"                                            .
      *        *-------------------------------------------------------*
      *        * Settore gestionale                                    *
      *        *-------------------------------------------------------*
           05  i-ide-set                  pic  x(03) value
                     "sit"                                            .
      *        *-------------------------------------------------------*
      *        * Fase gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-fas                  pic  x(06) value
                     "mag460"                                         .
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10) value
                     "pmag4600"                                       .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  i-ide-des                  pic  x(40) value
                     "GIACENZE VALORIZZATE PRODOTTI DI VENDITA"       .

      *    *===========================================================*
      *    * Area per il programma di esecuzione                       *
      *    *-----------------------------------------------------------*
       01  i-exe.
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma di esecuzione             *
      *        *-------------------------------------------------------*
           05  i-exe-pro                  pic  x(10) value
                     "pmag4601  "                                     .
      *        *-------------------------------------------------------*
      *        * Pathname del programma di esecuzione                  *
      *        *-------------------------------------------------------*
           05  i-exe-pat                  pic  x(40) value
                     "pgm/mag/prg/obj/pmag4601                "       .

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
      *    * Record files                                              *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [zos]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/azi/fls/rec/rfzos"                          .
      *        *-------------------------------------------------------*
      *        * [dcp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfdcp"                          .
      *        *-------------------------------------------------------*
      *        * [zls]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfzls"                          .
      *        *-------------------------------------------------------*
      *        * [zvl]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfzvl"                          .
      *        *-------------------------------------------------------*
      *        * [duccmc]                                              *
      *        *-------------------------------------------------------*
           copy      "pgm/mag/num/rec/rnduccmc"                       .

      *    *===========================================================*
      *    * Work generica per tutto il programma                      *
      *    *-----------------------------------------------------------*
       01  w-gen.
      *        *-------------------------------------------------------*
      *        * Data attuale                                          *
      *        *-------------------------------------------------------*
           05  w-gen-dat-att              pic  9(07)                  .
               
      *    *===========================================================*
      *    * Work-area richieste                                       *
      *    *-----------------------------------------------------------*
       01  rr.
      *        *-------------------------------------------------------*
      *        * Codice dipendenza in uso                              *
      *        *-------------------------------------------------------*
           05  rr-dpz-inu                 pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Denominazione dipendenza in uso                       *
      *        *-------------------------------------------------------*
           05  rr-dpz-inu-den             pic  x(20)                  .
      *        *-------------------------------------------------------*
      *        * Numero globale dipendenze                             *
      *        *-------------------------------------------------------*
           05  rr-dpz-ctr-dpz             pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Numero dipendenze selezionate                         *
      *        *-------------------------------------------------------*
           05  rr-dpz-ctr-sel             pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Tabella dipendenze selezionate                        *
      *        *-------------------------------------------------------*
           05  rr-dpz-tbl-dpz.
      *            *---------------------------------------------------*
      *            * Elementi della tabella dipendenze selezionate     *
      *            *---------------------------------------------------*
               10  rr-dpz-ele-dpz occurs 99.
      *                *-----------------------------------------------*
      *                * Codice dipendenza selezionato                 *
      *                *-----------------------------------------------*
                   15  rr-dpz-ele-cod     pic  9(02)                  .
      *                *-----------------------------------------------*
      *                * Denominazione dipendenza                      *
      *                *-----------------------------------------------*
                   15  rr-dpz-ele-den     pic  x(20)                  .
      *        *-------------------------------------------------------*
      *        * Codice filtro di selezione                            *
      *        *-------------------------------------------------------*
           05  rr-fso-dcp                 pic  9(08)                  .
           05  rr-fso-dcp-alf redefines
               rr-fso-dcp                 pic  x(08)                  .
           05  rr-fso-dcp-des             pic  x(40)                  .
           05  rr-fso-dcp-ord             pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Tipo salto pagina                                     *
      *        *                                                       *
      *        * - 01 : No salto pagina                                *
      *        * - 02 : Ad ogni classe                                 *
      *        * - 03 : Ad ogni gruppo                                 *
      *        * - 04 : Ad ogni sottogruppo                            *
      *        *-------------------------------------------------------*
           05  rr-tip-slp                 pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Si/No dettaglio prodotti                              *
      *        *                                                       *
      *        * - 01 : Si                                             *
      *        * - 02 : No                                             *
      *        *                                                       *
      *        * N.B.: solo se ordinamento per classe, gruppo o sotto- *
      *        *       gruppo e no salto pagina                        *
      *        *-------------------------------------------------------*
           05  rr-snx-dtp                 pic  9(01)                  .
      *        *-------------------------------------------------------*
      *        * Data situazione di magazzino                          *
      *        *-------------------------------------------------------*
           05  rr-dat-sit                 pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Tipo di giacenza da valorizzare                       *
      *        *                                                       *
      *        * - 01 : Di proprieta'                                  *
      *        * - 02 : Fisica                                         *
      *        * - 03 : Fisica di proprieta'                           *
      *        * - 04 : Presso terzi                                   *
      *        * - 05 : Di terzi                                       *
      *        *-------------------------------------------------------*
           05  rr-tip-gia                 pic  9(01)                  .
      *        *-------------------------------------------------------*
      *        * Si/No solo se saldo diverso da zero                   *
      *        *                                                       *
      *        * - 1 : Si                                              *
      *        * - 2 : Si, ma solo se negativo                         *
      *        * - 3 : No, nessuna selezione                           *
      *        *-------------------------------------------------------*
           05  rr-snx-zer                 pic  9(01)                  .
      *        *-------------------------------------------------------*
      *        * Tipo valorizzazione                                   *
      *        *                                                       *
      *        * - 01 : Costo medio ponderato annuale                  *
      *        * - 02 : Ultimo costo d'acquisto                        *
      *        * - 03 : Costo standard                                 *
      *        * - 04 : Costo medio continuo                           *
      *        * - 05 : Prezzo di listino di vendita                   *
      *        * - 06 : Listino di acquisto fornitore preferenziale    *
      *        *-------------------------------------------------------*
           05  rr-tip-val                 pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Codice listino                                        *
      *        *-------------------------------------------------------*
           05  rr-cod-lst                 pic  x(03)                  .
           05  rr-cod-lst-des             pic  x(20)                  .
           05  rr-cod-lst-vlt             pic  x(03)                  .
           05  rr-cod-lst-vlt-des         pic  x(20)                  .
           05  rr-cod-lst-vlt-dec         pic  9(01)                  .
           05  rr-cod-lst-vlt-tdc         pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Prodotti non valorizzati                              *
      *        *                                                       *
      *        * - 01 : Inclusi                                        *
      *        * - 02 : Esclusi                                        *
      *        * - 03 : Solo non valorizzati                           *
      *        *-------------------------------------------------------*
           05  rr-snx-nvl                 pic  9(01)                  .
      *        *-------------------------------------------------------*
      *        * Si/No esportazione archivio                           *
      *        *                                                       *
      *        * - 01 : No                                             *
      *        * - 02 : Si                                             *
      *        * - 03 : Si, ma senza stampa                            *
      *        *-------------------------------------------------------*
           05  rr-snx-exp                 pic  9(01)                  .
      *        *-------------------------------------------------------*
      *        * Data valorizzazione di magazzino                      *
      *        *-------------------------------------------------------*
           05  rr-dat-val                 pic  9(07)                  .

      *    *===========================================================*
      *    * Work per subroutines di Let                               *
      *    *-----------------------------------------------------------*
       01  w-let.
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [zls]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-zls.
               10  w-let-arc-zls-flg      pic  x(01)                  .
               10  w-let-arc-zls-cod      pic  x(03)                  .
               10  w-let-arc-zls-des      pic  x(20)                  .
               10  w-let-arc-zls-vlt      pic  x(03)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [zvl]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-zvl.
               10  w-let-arc-zvl-flg      pic  x(01)                  .
               10  w-let-arc-zvl-cod      pic  x(03)                  .
               10  w-let-arc-zvl-des      pic  x(20)                  .
               10  w-let-arc-zvl-din      pic  x(20)                  .
               10  w-let-arc-zvl-dec      pic  9(01)                  .
               10  w-let-arc-zvl-tdc      pic  x(01)                  .

      *    *===========================================================*
      *    * Work per Let su archivio [zos] per [dcp]                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/lzosdcp0.ltw"                   .

      *    *===========================================================*
      *    * Work-area per salvataggi                                  *
      *    *-----------------------------------------------------------*
       01  w-sav.
      *        *-------------------------------------------------------*
      *        * Data situazione                                       *
      *        *-------------------------------------------------------*
           05  w-sav-dat-sit              pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Tipo valorizzazione                                   *
      *        *-------------------------------------------------------*
           05  w-sav-tip-val              pic  9(02)                  .

      *    *===========================================================*
      *    * Link-area per accettazione codice filtro selezione e or-  *
      *    * dinamento per file [dcp]                                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/azosdcp0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice listino                 *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/acodzls0.acl"                   .

      *    *===========================================================*
      *    * Work per accettazioni campi espansi                       *
      *    *-----------------------------------------------------------*
       01  w-exp.
      *        *-------------------------------------------------------*
      *        * Work per : Tipo salto pagina                          *
      *        *-------------------------------------------------------*
           05  w-exp-tip-slp.
               10  w-exp-tip-slp-num      pic  9(02)       value 4    .
               10  w-exp-tip-slp-lun      pic  9(02)       value 20   .
               10  w-exp-tip-slp-tbl.
                   15  filler             pic  x(20) value
                            "No                  "                    .
                   15  filler             pic  x(20) value
                            "ad ogni Classe      "                    .
                   15  filler             pic  x(20) value
                            "ad ogni Gruppo      "                    .
                   15  filler             pic  x(20) value
                            "ad ogni Sottogruppo "                    .
      *        *-------------------------------------------------------*
      *        * Work per : Si/no dettaglio prodotti                   *
      *        *-------------------------------------------------------*
           05  w-exp-snx-dtp.
               10  w-exp-snx-dtp-num      pic  9(02)       value 2    .
               10  w-exp-snx-dtp-lun      pic  9(02)       value 02   .
               10  w-exp-snx-dtp-tbl.
                   15  filler             pic  x(02) value "Si"       .
                   15  filler             pic  x(02) value "No"       .
      *        *-------------------------------------------------------*
      *        * Work per : Tipo di giacenza                           *
      *        *-------------------------------------------------------*
           05  w-exp-tip-gia.
               10  w-exp-tip-gia-num      pic  9(02)       value 05   .
               10  w-exp-tip-gia-lun      pic  9(02)       value 40   .
               10  w-exp-tip-gia-tbl.
                   15  filler             pic  x(40) value
                            "Di proprieta'                           ".
                   15  filler             pic  x(40) value
                            "Fisica                                  ".
                   15  filler             pic  x(40) value
                            "fisica di proprieta'                    ".
                   15  filler             pic  x(40) value
                            "Presso terzi                            ".
                   15  filler             pic  x(40) value
                            "di Terzi                                ".
      *        *-------------------------------------------------------*
      *        * Work per : Si/no solo saldo diverso da zero           *
      *        *-------------------------------------------------------*
           05  w-exp-snx-zer.
               10  w-exp-snx-zer-num      pic  9(02)       value 3    .
               10  w-exp-snx-zer-lun      pic  9(02)       value 30   .
               10  w-exp-snx-zer-tbl.
                   15  filler             pic  x(30) value
                            "Si                            "          .
                   15  filler             pic  x(30) value
                            "Si, ma solo se negativo       "          .
                   15  filler             pic  x(30) value
                            "No, nessuna selezione         "          .
      *        *-------------------------------------------------------*
      *        * Work per : Tipo valorizzazione                        *
      *        *-------------------------------------------------------*
           05  w-exp-tip-val.
               10  w-exp-tip-val-num      pic  9(02)       value 6    .
               10  w-exp-tip-val-lun      pic  9(02)       value 40   .
               10  w-exp-tip-val-tbl.
                   15  filler             pic  x(40) value
                            "costo Medio ponderato annuale           ".
                   15  filler             pic  x(40) value
                            "Ultimo costo d'acquisto                 ".
                   15  filler             pic  x(40) value
                            "costo Standard                          ".
                   15  filler             pic  x(40) value
                            "costo medio Continuo                    ".
                   15  filler             pic  x(40) value
                            "Prezzo di listino di vendita            ".
                   15  filler             pic  x(40) value
                            "Listino acquisto fornitore preferenziale".
      *        *-------------------------------------------------------*
      *        * Work per : Prodotti non valorizzati                   *
      *        *-------------------------------------------------------*
           05  w-exp-snx-nvl.
               10  w-exp-snx-nvl-num      pic  9(02)       value 3    .
               10  w-exp-snx-nvl-lun      pic  9(02)       value 20   .
               10  w-exp-snx-nvl-tbl.
                   15  filler             pic  x(20) value
                            "Inclusi             "                    .
                   15  filler             pic  x(20) value
                            "Esclusi             "                    .
                   15  filler             pic  x(20) value
                            "Solo non valorizzati"                    .
      *        *-------------------------------------------------------*
      *        * Work per : Si/no esportazione archivio                *
      *        *-------------------------------------------------------*
           05  w-exp-snx-exp.
               10  w-exp-snx-exp-num      pic  9(02)       value 3    .
               10  w-exp-snx-exp-lun      pic  9(02)       value 20   .
               10  w-exp-snx-exp-tbl.
                   15  filler             pic  x(20) value
                            "No                  "                    .
                   15  filler             pic  x(20) value
                            "Si                  "                    .
                   15  filler             pic  x(20) value
                            "si, senza La stampa "                    .

      *    *===========================================================*
      *    * Work area per contatori ed indici                         *
      *    *-----------------------------------------------------------*
       01  w-cix.
      *        *-------------------------------------------------------*
      *        * Contatore generico                                    *
      *        *-------------------------------------------------------*
           05  w-cix-ctr-001              pic  9(03)                  .

      *    *===========================================================*
      *    * Work per subroutines di Err                               *
      *    *-----------------------------------------------------------*
       01  w-err.
      *        *-------------------------------------------------------*
      *        * Work per Err con box centrale                         *
      *        *-------------------------------------------------------*
           05  w-err-box-err.
               10  w-err-box-err-msg      pic  x(60)                  .

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
           move      "S+"                 to   w-dpz-tip-ope          .
           call      "pgm/azi/prg/obj/pazi000d"
                                         using w-dpz                  .
           cancel    "pgm/azi/prg/obj/pazi000d"                       .
       pre-exe-pgm-180.
      *              *-------------------------------------------------*
      *              * Se scelta non effettuata : uscita               *
      *              *-------------------------------------------------*
           if        w-dpz-ctr-dpz        =    zero
                     move  "#"            to   w-cnt-pre-exe-pgm
                     go to pre-exe-pgm-999.
      *              *-------------------------------------------------*
      *              * Altrimenti memorizzazione in campo selezione    *
      *              *                                                 *
      *              * zero = tutte le dipendenze                      *
      *              * nn   = codice dipendenza selezionata            *
      *              *-------------------------------------------------*
           move      w-dpz-cod-prg        to   rr-dpz-inu             .
      *              *-------------------------------------------------*
      *              * Memorizzazione denominazione dipendenza in uso  *
      *              *-------------------------------------------------*
           if        rr-dpz-inu           =    zero
                     move  spaces         to   rr-dpz-inu-den
           else      move  w-dpz-ele-den
                          (rr-dpz-inu)    to   rr-dpz-inu-den         .
      *              *-------------------------------------------------*
      *              * Determinazione data attuale                     *
      *              *-------------------------------------------------*
           move      "DT"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-dat                to   w-gen-dat-att          .
      *              *-------------------------------------------------*
      *              * Lettura numerazione relativa al costo medio     *
      *              * continuo                                        *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/mag/num/ioc/obj/induccmc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-duc-cmc             .
           move      "NO"                 to   f-ope                  .
           move      "pgm/mag/num/ioc/obj/induccmc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-duc-cmc             .
           move      "RD"                 to   f-ope                  .
           move      "pgm/mag/num/ioc/obj/induccmc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-duc-cmc             .
           move      "CL"                 to   f-ope                  .
           move      "pgm/mag/num/ioc/obj/induccmc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-duc-cmc             .
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
      *              * Open modulo accettazione codice filtro ordina-  *
      *              * mento e selezione per file [dcp]                *
      *              *-------------------------------------------------*
           perform   cod-zos-dcp-opn-000  thru cod-zos-dcp-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice listino         *
      *              *-------------------------------------------------*
           perform   cod-cod-zls-opn-000  thru cod-cod-zls-opn-999    .
      *              *-------------------------------------------------*
      *              * [zos]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/azi/fls/ioc/obj/iofzos"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zos                 .
      *              *-------------------------------------------------*
      *              * [zls]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofzls"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zls                 .
      *              *-------------------------------------------------*
      *              * [zvl]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofzvl"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zvl                 .
      *              *-------------------------------------------------*
      *              * Apertura filtro per selezione ed ordinamento    *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcp/prg/obj/bzosdcp0"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
       rou-opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Close files per richieste                                 *
      *    *-----------------------------------------------------------*
       rou-cls-fls-000.
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice filtro ordina- *
      *              * mento e selezione per file [dcp]                *
      *              *-------------------------------------------------*
           perform   cod-zos-dcp-cls-000  thru cod-zos-dcp-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice listino        *
      *              *-------------------------------------------------*
           perform   cod-cod-zls-cls-000  thru cod-cod-zls-cls-999    .
      *              *-------------------------------------------------*
      *              * [zos]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/azi/fls/ioc/obj/iofzos"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zos                 .
      *              *-------------------------------------------------*
      *              * [zls]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofzls"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zls                 .
      *              *-------------------------------------------------*
      *              * [zvl]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofzvl"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zvl                 .
      *              *-------------------------------------------------*
      *              * Close filtro per selezione ed ordinamento       *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcp/prg/obj/bzosdcp0"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
      *                  *---------------------------------------------*
      *                  * Cancel del modulo utilizzato                *
      *                  *---------------------------------------------*
           move      "pgm/dcp/prg/obj/bzosdcp0"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           cancel    s-pat                                            .
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
      *                  * Accettazione codice filtro di ordinamento e *
      *                  * selezione per file [dcp]                    *
      *                  *---------------------------------------------*
           perform   acc-fso-dcp-000      thru acc-fso-dcp-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
       acc-ric-sel-200.
      *                  *---------------------------------------------*
      *                  * Tipo salto pagina                           *
      *                  *---------------------------------------------*
           perform   acc-tip-slp-000      thru acc-tip-slp-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-100.
       acc-ric-sel-250.
      *                  *---------------------------------------------*
      *                  * Si/no dettaglio prodotti                    *
      *                  *---------------------------------------------*
           perform   acc-snx-dtp-000      thru acc-snx-dtp-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-200.
       acc-ric-sel-300.
      *                  *---------------------------------------------*
      *                  * Data situazione                             *
      *                  *---------------------------------------------*
           perform   acc-dat-sit-000      thru acc-dat-sit-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-250.
       acc-ric-sel-400.
      *                  *---------------------------------------------*
      *                  * Tipo di giacenza                            *
      *                  *---------------------------------------------*
           perform   acc-tip-gia-000      thru acc-tip-gia-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-300.
       acc-ric-sel-500.
      *                  *---------------------------------------------*
      *                  * Si/no solo se saldo diverso da zero         *
      *                  *---------------------------------------------*
           perform   acc-snx-zer-000      thru acc-snx-zer-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-400.
       acc-ric-sel-600.
      *                  *---------------------------------------------*
      *                  * Tipo valorizzazione                         *
      *                  *---------------------------------------------*
           perform   acc-tip-val-000      thru acc-tip-val-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-500.
       acc-ric-sel-650.
      *                  *---------------------------------------------*
      *                  * Codice listino                              *
      *                  *---------------------------------------------*
           perform   acc-cod-lst-000      thru acc-cod-lst-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-600.
       acc-ric-sel-700.
      *                  *---------------------------------------------*
      *                  * Si/no solo se non valorizzati               *
      *                  *---------------------------------------------*
           perform   acc-snx-nvl-000      thru acc-snx-nvl-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-650.
       acc-ric-sel-750.
      *                  *---------------------------------------------*
      *                  * Si/no esportazione archivio                 *
      *                  *---------------------------------------------*
           perform   acc-snx-exp-000      thru acc-snx-exp-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-700.
       acc-ric-sel-800.
      *                  *---------------------------------------------*
      *                  * Data valorizzazione                         *
      *                  *---------------------------------------------*
           perform   acc-dat-val-000      thru acc-dat-val-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-750.
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
      *              * Prompts                                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Codice filtro per selezione ed ordinamento  *
      *                  * per file [dcp]                              *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice filtro di selezione :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "       anagrafica prodotti  "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Salto pagina da effettuare                  *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Salto pagina da effettuare :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Dettaglio prodotti                          *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      57                   to   v-pos                  .
           move      "Dettaglio prodotti :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Data situazione                             *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Situazione di magazzino al :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Tipo giacenza                               *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Giacenza da valorizzare    :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Si/No solo saldo diverso da zero            *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Solo saldo diverso da zero :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Tipo di valorizzazione                      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tipo di valorizzazione     :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Codice listino                              *
      *                  *---------------------------------------------*
           perform   pmt-cod-lst-000      thru pmt-cod-lst-999        .
      *                  *---------------------------------------------*
      *                  * Prodotti non valorizzati                    *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Prodotti non valorizzati   :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Si/no esportazione archivio                 *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Esportazione archivio      :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Data valorizzazione                         *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      21                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Valorizzazione relativa al :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Preparazione e visualizzazione defaults         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Tipo salto pagina                           *
      *                  *---------------------------------------------*
           move      01                   to   rr-tip-slp             .
           perform   vis-tip-slp-000      thru vis-tip-slp-999        .
      *                  *---------------------------------------------*
      *                  * Si/no dettaglio prodotti                    *
      *                  *---------------------------------------------*
           move      01                   to   rr-snx-dtp             .
           perform   vis-snx-dtp-000      thru vis-snx-dtp-999        .
      *                  *---------------------------------------------*
      *                  * Data situazione                             *
      *                  *---------------------------------------------*
           move      w-gen-dat-att        to   rr-dat-sit             .
           perform   vis-dat-sit-000      thru vis-dat-sit-999        .
      *                  *---------------------------------------------*
      *                  * Tipo di giacenza                            *
      *                  *---------------------------------------------*
           move      01                   to   rr-tip-gia             .
           perform   vis-tip-gia-000      thru vis-tip-gia-999        .
      *                  *---------------------------------------------*
      *                  * Si/no solo se saldo diverso da zero         *
      *                  *---------------------------------------------*
           move      01                   to   rr-snx-zer             .
           perform   vis-snx-zer-000      thru vis-snx-zer-999        .
      *                  *---------------------------------------------*
      *                  * Tipo di valorizzazione                      *
      *                  *---------------------------------------------*
           move      01                   to   rr-tip-val             .
           perform   vis-tip-val-000      thru vis-tip-val-999        .
      *                  *---------------------------------------------*
      *                  * Si/no solo se non valorizzati               *
      *                  *---------------------------------------------*
           move      01                   to   rr-snx-nvl             .
           perform   vis-snx-nvl-000      thru vis-snx-nvl-999        .
      *                  *---------------------------------------------*
      *                  * Si/no esportazione archivio                 *
      *                  *---------------------------------------------*
           move      01                   to   rr-snx-exp             .
           perform   vis-snx-exp-000      thru vis-snx-exp-999        .
      *                  *---------------------------------------------*
      *                  * Data valorizzazione                         *
      *                  *---------------------------------------------*
           move      w-gen-dat-att        to   rr-dat-val             .
           perform   vis-dat-val-000      thru vis-dat-val-999        .
       pmt-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Codice listino                                   *
      *    *-----------------------------------------------------------*
       pmt-cod-lst-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      01                   to   v-pos                  .
           if        rr-tip-val           =    05
                     move  "Codice listino             :"
                                          to   v-alf
           else      move  spaces         to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cod-lst-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo selezione : Codice filtro per selezio- *
      *    *                                ne ed ordinamento per il   *
      *    *                                file [dcp]                 *
      *    *-----------------------------------------------------------*
       acc-fso-dcp-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-fso-dcp-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-zos-dcp-ope      .
           move      rr-fso-dcp           to   w-cod-zos-dcp-cod      .
           move      05                   to   w-cod-zos-dcp-lin      .
           move      30                   to   w-cod-zos-dcp-pos      .
           move      05                   to   w-cod-zos-dcp-dln      .
           move      41                   to   w-cod-zos-dcp-dps      .
           move      "<B"                 to   v-edm                  .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-zos-dcp-cll-000  thru cod-zos-dcp-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-zos-dcp-foi-000  thru cod-zos-dcp-foi-999    .
       acc-fso-dcp-110.
           perform   cod-zos-dcp-cll-000  thru cod-zos-dcp-cll-999    .
           if        w-cod-zos-dcp-ope    =    "F+"
                     go to acc-fso-dcp-115.
           if        w-cod-zos-dcp-ope    =    "AC"
                     go to acc-fso-dcp-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-fso-dcp-115.
           perform   cod-zos-dcp-foi-000  thru cod-zos-dcp-foi-999    .
           go to     acc-fso-dcp-110.
       acc-fso-dcp-120.
           move      w-cod-zos-dcp-cod    to   v-num                  .
       acc-fso-dcp-150.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-fso-dcp-999.
       acc-fso-dcp-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-fso-dcp             .
       acc-fso-dcp-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura codice filtro selezione e ordina-   *
      *                  * mento per file [dcp]                        *
      *                  *---------------------------------------------*
           move      rr-fso-dcp           to   w-let-fso-dcp-cod      .
           perform   let-fso-dcp-000      thru let-fso-dcp-999        .
           move      w-let-fso-dcp-des    to   rr-fso-dcp-des         .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione filtro          *
      *                  *---------------------------------------------*
           perform   vis-des-fso-000      thru vis-des-fso-999        .
      *                  *---------------------------------------------*
      *                  * Se codice filtro non esistente : a reimpo-  *
      *                  * stazione                                    *
      *                  *---------------------------------------------*
           if        w-let-fso-dcp-flg    not  = spaces
                     go to acc-fso-dcp-100.
       acc-fso-dcp-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura tipo ordinamento dal codice filtro  *
      *                  *---------------------------------------------*
       acc-fso-dcp-620.
      *                      *-----------------------------------------*
      *                      * Richiesta tipo ordinamento              *
      *                      *-----------------------------------------*
           move      "TO"                 to   f-ope                  .
           move      rr-fso-dcp-alf       to   f-key                  .
           move      "pgm/dcp/prg/obj/bzosdcp0"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
           if        f-sts                =    "01"
                     move  01             to   rr-fso-dcp-ord
           else if   f-sts                =    "02"
                     move  02             to   rr-fso-dcp-ord
           else if   f-sts                =    "03"
                     move  03             to   rr-fso-dcp-ord
           else if   f-sts                =    "04"
                     move  04             to   rr-fso-dcp-ord
           else      move  01             to   rr-fso-dcp-ord         .
       acc-fso-dcp-640.
      *                      *-----------------------------------------*
      *                      * Se ordinamento per codice o descrizione *
      *                      *-----------------------------------------*
           if        rr-fso-dcp-ord       <    3
                     go to acc-fso-dcp-800.
      *                      *-----------------------------------------*
      *                      * Normalizzazione tipo salto pagina       *
      *                      *-----------------------------------------*
           move      1                    to   rr-tip-slp             .
      *                      *-----------------------------------------*
      *                      * Visualizzazione tipo salto pagina       *
      *                      *-----------------------------------------*
           perform   vis-tip-slp-000      thru vis-tip-slp-999        .
      *                      *-----------------------------------------*
      *                      * Normalizzazione si/no dettaglio prodot- *
      *                      * ti                                      *
      *                      *-----------------------------------------*
           move      1                    to   rr-snx-dtp             .
      *                      *-----------------------------------------*
      *                      * Visualizzazione si/no dettaglio prodot- *
      *                      * ti                                      *
      *                      *-----------------------------------------*
           perform   vis-snx-dtp-000      thru vis-snx-dtp-999        .
       acc-fso-dcp-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-fso-dcp-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-fso-dcp-100.
       acc-fso-dcp-999.
           exit.

      *    *===========================================================*
      *    * Visualizzaz. campo selezione : Codice filtro per selezio- *
      *    *                                ne ed ordinamento per il   *
      *    *                                file [dcp]                 *
      *    *-----------------------------------------------------------*
       vis-fso-dcp-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      05                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-fso-dcp           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-fso-dcp-999.
           exit.

      *    *===========================================================*
      *    * Visualizzaz. campo selezione : Descrizione codice filtro  *
      *    *                                per selezione ed ordina-   *
      *    *                                mento per il [dcp]         *
      *    *-----------------------------------------------------------*
       vis-des-fso-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      rr-fso-dcp-des       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-des-fso-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo selezione : Tipo salto pagina          *
      *    *-----------------------------------------------------------*
       acc-tip-slp-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        rr-fso-dcp-ord       >    2
                     go to acc-tip-slp-999.
       acc-tip-slp-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-slp-lun    to   v-car                  .
           move      w-exp-tip-slp-num    to   v-ldt                  .
           move      "NCGS#"              to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      08                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-tip-slp-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-ric    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      rr-tip-slp           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-tip-slp-999.
       acc-tip-slp-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-tip-slp             .
       acc-tip-slp-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Valore zero non ammesso, a meno che non si  *
      *                  * sia in Up                                   *
      *                  *---------------------------------------------*
           if        rr-tip-slp           not  = zero
                     go to acc-tip-slp-600.
           if        v-key                =    "UP  "
                     go to acc-tip-slp-600
           else      go to acc-tip-slp-999.
       acc-tip-slp-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se no salto pagina                          *
      *                  *---------------------------------------------*
           if        rr-tip-slp           =    01
                     go to acc-tip-slp-800.
      *                  *---------------------------------------------*
      *                  * Normalizzazione si/no dettaglio prodotti    *
      *                  *---------------------------------------------*
           move      1                    to   rr-snx-dtp             .
      *                  *---------------------------------------------*
      *                  * Visualizzazione si/no dettaglio prodotti    *
      *                  *---------------------------------------------*
           perform   vis-snx-dtp-000      thru vis-snx-dtp-999        .
       acc-tip-slp-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-tip-slp-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-tip-slp-100.
       acc-tip-slp-999.
           exit.

      *    *===========================================================*
      *    * Visualizzaz. campo selezione : Tipo salto pagina          *
      *    *-----------------------------------------------------------*
       vis-tip-slp-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-slp-lun    to   v-car                  .
           move      w-exp-tip-slp-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      08                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-tip-slp-tbl    to   v-txt                  .
           move      rr-tip-slp           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tip-slp-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Si/No dettaglio prodotti                   *
      *    *-----------------------------------------------------------*
       acc-snx-dtp-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        rr-fso-dcp-ord       >    02
                     go to acc-snx-dtp-999.
           if        rr-tip-slp           not  = 01
                     go to acc-snx-dtp-999.
       acc-snx-dtp-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-snx-dtp-lun    to   v-car                  .
           move      w-exp-snx-dtp-num    to   v-ldt                  .
           move      "SN#"                to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      08                   to   v-lin                  .
           move      78                   to   v-pos                  .
           move      w-exp-snx-dtp-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-ric    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      rr-snx-dtp           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-snx-dtp-999.
       acc-snx-dtp-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-snx-dtp             .
       acc-snx-dtp-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Valore zero non ammesso, a meno che non si  *
      *                  * sia in Up                                   *
      *                  *---------------------------------------------*
           if        rr-snx-dtp           not  = zero
                     go to acc-snx-dtp-600.
           if        v-key                =    "UP  "
                     go to acc-snx-dtp-600
           else      go to acc-snx-dtp-999.
       acc-snx-dtp-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-snx-dtp-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-snx-dtp-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-snx-dtp-100.
       acc-snx-dtp-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Si/No dettaglio prodotti                *
      *    *-----------------------------------------------------------*
       vis-snx-dtp-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-snx-dtp-lun    to   v-car                  .
           move      w-exp-snx-dtp-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      08                   to   v-lin                  .
           move      78                   to   v-pos                  .
           move      w-exp-snx-dtp-tbl    to   v-txt                  .
           move      rr-snx-dtp           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-snx-dtp-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Data situazione                            *
      *    *-----------------------------------------------------------*
       acc-dat-sit-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      rr-dat-sit           to   w-sav-dat-sit          .
       acc-dat-sit-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-ric    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      10                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-dat-sit           to   v-dat                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-dat-sit-150.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-dat-sit-999.
       acc-dat-sit-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-dat                to   rr-dat-sit             .
       acc-dat-sit-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Valore zero non ammesso, ameno che non si   *
      *                  * sia in Up                                   *
      *                  *---------------------------------------------*
           if        rr-dat-sit           not  = zero
                     go to acc-dat-sit-600.
           if        v-key                =    "UP  "
                     go to acc-dat-sit-600
           else      go to acc-dat-sit-100.
       acc-dat-sit-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se valore variato                      *
      *                  *---------------------------------------------*
           if        rr-dat-sit           =    w-sav-dat-sit
                     go to acc-dat-sit-800.
      *                  *---------------------------------------------*
      *                  * Data valorizzazione                         *
      *                  *---------------------------------------------*
           move      rr-dat-sit           to   rr-dat-val             .
           perform   vis-dat-val-000      thru vis-dat-val-999        .
       acc-dat-sit-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-dat-sit-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-dat-sit-100.
       acc-dat-sit-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Data situazione                         *
      *    *-----------------------------------------------------------*
       vis-dat-sit-000.
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      10                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-dat-sit           to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dat-sit-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Tipo di giacenza                           *
      *    *-----------------------------------------------------------*
       acc-tip-gia-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-tip-gia-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-gia-lun    to   v-car                  .
           move      w-exp-tip-gia-num    to   v-ldt                  .
           move      "DFATP#"             to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      12                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-tip-gia-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-ric    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      rr-tip-gia           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-tip-gia-999.
       acc-tip-gia-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-tip-gia             .
       acc-tip-gia-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Valore zero non ammesso, a meno che non si  *
      *                  * sia in Up                                   *
      *                  *---------------------------------------------*
           if        rr-tip-gia           not  = zero
                     go to acc-tip-gia-500.
           if        v-key                =    "UP  "
                     go to acc-tip-gia-600
           else      go to acc-tip-gia-999.
       acc-tip-gia-500.
      *                  *---------------------------------------------*
      *                  * Test se valore ammesso                      *
      *                  *---------------------------------------------*
           if        rr-tip-gia           >    w-exp-tip-gia-num
                     go to acc-tip-gia-100.
       acc-tip-gia-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-tip-gia-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-tip-gia-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-tip-gia-100.
       acc-tip-gia-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Tipo di giacenza                        *
      *    *-----------------------------------------------------------*
       vis-tip-gia-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-gia-lun    to   v-car                  .
           move      w-exp-tip-gia-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      12                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-tip-gia-tbl    to   v-txt                  .
           move      rr-tip-gia           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tip-gia-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Si/No solo saldo diverso da zero           *
      *    *-----------------------------------------------------------*
       acc-snx-zer-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-snx-zer-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-snx-zer-lun    to   v-car                  .
           move      w-exp-snx-zer-num    to   v-ldt                  .
           move      "SNX#"               to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      14                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-snx-zer-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-ric    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      rr-snx-zer           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-snx-zer-999.
       acc-snx-zer-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-snx-zer             .
       acc-snx-zer-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Valore zero non ammesso, a meno che non si  *
      *                  * sia in Up                                   *
      *                  *---------------------------------------------*
           if        rr-snx-zer           not  = zero
                     go to acc-snx-zer-600.
           if        v-key                =    "UP  "
                     go to acc-snx-zer-600
           else      go to acc-snx-zer-999.
       acc-snx-zer-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-snx-zer-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-snx-zer-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-snx-zer-100.
       acc-snx-zer-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Si/No solo saldo diverso da zero        *
      *    *-----------------------------------------------------------*
       vis-snx-zer-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-snx-zer-lun    to   v-car                  .
           move      w-exp-snx-zer-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      14                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-snx-zer-tbl    to   v-txt                  .
           move      rr-snx-zer           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-snx-zer-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Tipo valorizzazione                        *
      *    *-----------------------------------------------------------*
       acc-tip-val-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      rr-tip-val           to   w-sav-tip-val          .
       acc-tip-val-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-val-lun    to   v-car                  .
           move      w-exp-tip-val-num    to   v-ldt                  .
           move      "MUSCPL#"            to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      16                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-tip-val-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-ric    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      rr-tip-val           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-tip-val-999.
       acc-tip-val-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-tip-val             .
       acc-tip-val-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Valore zero non ammesso, a meno che non si  *
      *                  * sia in Up                                   *
      *                  *---------------------------------------------*
           if        rr-tip-val           not  = zero
                     go to acc-tip-val-600.
           if        v-key                =    "UP  "
                     go to acc-tip-val-600
           else      go to acc-tip-val-999.
       acc-tip-val-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore attuale come precedente : oltre   *
      *                  *---------------------------------------------*
           if        rr-tip-val           =    w-sav-tip-val
                     go to acc-tip-val-800.
      *                  *---------------------------------------------*
      *                  * Trattamento codice listino                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Prompt codice listino                   *
      *                      *-----------------------------------------*
           perform   pmt-cod-lst-000      thru pmt-cod-lst-999        .
      *                      *-----------------------------------------*
      *                      * Se valore attuale, 05 : oltre           *
      *                      *-----------------------------------------*
           if        rr-tip-val           =    05
                     go to acc-tip-val-800.
      *                      *-----------------------------------------*
      *                      * Normalizzazione e visualizzazione       *
      *                      *-----------------------------------------*
           move      spaces               to   rr-cod-lst             .
           move      spaces               to   rr-cod-lst-des         .
           move      spaces               to   rr-cod-lst-vlt         .
           move      spaces               to   rr-cod-lst-vlt-des     .
           move      zero                 to   rr-cod-lst-vlt-dec     .
           move      spaces               to   rr-cod-lst-vlt-tdc     .
           perform   vis-cod-lst-000      thru vis-cod-lst-999        .
           perform   vis-cod-lst-des-000  thru vis-cod-lst-des-999    .
           perform   vis-cod-lst-vlt-000  thru vis-cod-lst-vlt-999    .
           perform   vis-cod-lst-vlt-des-000
                                          thru vis-cod-lst-vlt-des-999.
       acc-tip-val-700.
      *                  *---------------------------------------------*
      *                  * Trattamento data ultimo consolidamento      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se necessario                      *
      *                      *-----------------------------------------*
           if        rr-tip-val           not  = 04
                     go to acc-tip-val-800.
           if        rn-duc-cmc-dat-con   =    zero
                     go to acc-tip-val-800.
      *                          *-------------------------------------*
      *                          * Prompt                              *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Data ultimo consolidamento :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                          *-------------------------------------*
      *                          * Valore                              *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      19                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rn-duc-cmc-dat-con   to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-tip-val-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-tip-val-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-tip-val-100.
       acc-tip-val-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Tipo valorizzazione                     *
      *    *-----------------------------------------------------------*
       vis-tip-val-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-val-lun    to   v-car                  .
           move      w-exp-tip-val-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      16                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-tip-val-tbl    to   v-txt                  .
           move      rr-tip-val           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tip-val-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo selezione : Codice listino             *
      *    *-----------------------------------------------------------*
       acc-cod-lst-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        rr-tip-val           not  = 05
                     go to acc-cod-lst-999.
       acc-cod-lst-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-cod-lst-ope      .
           move      rr-cod-lst           to   w-cod-cod-lst-cod      .
           move      17                   to   w-cod-cod-lst-lin      .
           move      30                   to   w-cod-cod-lst-pos      .
           move      spaces               to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-cod-zls-cll-000  thru cod-cod-zls-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-cod-zls-foi-000  thru cod-cod-zls-foi-999    .
       acc-cod-lst-110.
           perform   cod-cod-zls-cll-000  thru cod-cod-zls-cll-999    .
           if        w-cod-cod-lst-ope    =    "F+"
                     go to acc-cod-lst-115.
           if        w-cod-cod-lst-ope    =    "AC"
                     go to acc-cod-lst-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-lst-115.
           perform   cod-cod-zls-foi-000  thru cod-cod-zls-foi-999    .
           go to     acc-cod-lst-110.
       acc-cod-lst-120.
           move      w-cod-cod-lst-cod    to   v-alf                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-cod-lst-999.
       acc-cod-lst-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   rr-cod-lst             .
       acc-cod-lst-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura tabella [zls]                       *
      *                  *---------------------------------------------*
           move      rr-cod-lst           to   w-let-arc-zls-cod      .
           perform   let-arc-zls-000      thru let-arc-zls-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione descrizione                  *
      *                  *---------------------------------------------*
           move      w-let-arc-zls-des    to   rr-cod-lst-des         .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione listino         *
      *                  *---------------------------------------------*
           perform   vis-cod-lst-des-000  thru vis-cod-lst-des-999    .
      *                  *---------------------------------------------*
      *                  * Se codice errato : reimpostazione           *
      *                  *---------------------------------------------*
           if        w-let-arc-zls-flg    not  = spaces
                     go to acc-cod-lst-100.
       acc-cod-lst-500.
      *                  *---------------------------------------------*
      *                  * Lettura archivio [zvl]                      *
      *                  *---------------------------------------------*
           move      w-let-arc-zls-vlt    to   w-let-arc-zvl-cod      .
           perform   let-arc-zvl-000      thru let-arc-zvl-999        .
           if        w-let-arc-zvl-flg    =    spaces
                     go to acc-cod-lst-550.
      *                      *-----------------------------------------*
      *                      * Messaggio di errore                     *
      *                      *-----------------------------------------*
           move      "Sigla valuta associato al listino non esistente   
      -              "          "         to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                      *-----------------------------------------*
      *                      * Reimpostazione                          *
      *                      *-----------------------------------------*
           go to     acc-cod-lst-100.
       acc-cod-lst-550.
      *                  *---------------------------------------------*
      *                  * Bufferizzazione valori associati alla sigla *
      *                  * valuta                                      *
      *                  *---------------------------------------------*
           move      w-let-arc-zls-vlt    to   rr-cod-lst-vlt         .
           move      w-let-arc-zvl-des    to   rr-cod-lst-vlt-des     .
           move      w-let-arc-zvl-dec    to   rr-cod-lst-vlt-dec     .
           move      w-let-arc-zvl-tdc    to   rr-cod-lst-vlt-tdc     .
      *                  *---------------------------------------------*
      *                  * Visualizzazione sigla valuta                *
      *                  *---------------------------------------------*
           perform   vis-cod-lst-vlt-000  thru vis-cod-lst-vlt-999    .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione sigla valuta    *
      *                  *---------------------------------------------*
           perform   vis-cod-lst-vlt-des-000
                                          thru vis-cod-lst-vlt-des-999.
       acc-cod-lst-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cod-lst-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-cod-lst-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-cod-lst-100.
       acc-cod-lst-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo selezione : Codice listino          *
      *    *-----------------------------------------------------------*
       vis-cod-lst-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-cod-lst           to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-lst-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo selezione : Descrizione listino     *
      *    *-----------------------------------------------------------*
       vis-cod-lst-des-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      34                   to   v-pos                  .
           move      rr-cod-lst-des       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-lst-des-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo selezione : Sigla valuta associata  *
      *    *                                   al listino              *
      *    *-----------------------------------------------------------*
       vis-cod-lst-vlt-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      57                   to   v-pos                  .
           move      rr-cod-lst-vlt       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-lst-vlt-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo selezione : Descizione sigla valuta *
      *    *                                   associata al listino    *
      *    *-----------------------------------------------------------*
       vis-cod-lst-vlt-des-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      61                   to   v-pos                  .
           move      rr-cod-lst-vlt-des   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-lst-vlt-des-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Si/No solo non valorizzati                 *
      *    *-----------------------------------------------------------*
       acc-snx-nvl-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-snx-nvl-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-snx-nvl-lun    to   v-car                  .
           move      w-exp-snx-nvl-num    to   v-ldt                  .
           move      "IES#"               to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      19                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-snx-nvl-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-ric    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      rr-snx-nvl           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-snx-nvl-999.
       acc-snx-nvl-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-snx-nvl             .
       acc-snx-nvl-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Valore zero non ammesso, a meno che non si  *
      *                  * sia in Up                                   *
      *                  *---------------------------------------------*
           if        rr-snx-nvl           not  = zero
                     go to acc-snx-nvl-600.
           if        v-key                =    "UP  "
                     go to acc-snx-nvl-600
           else      go to acc-snx-nvl-999.
       acc-snx-nvl-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-snx-nvl-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-snx-nvl-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-snx-nvl-100.
       acc-snx-nvl-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Si/No solo non valorizzati              *
      *    *-----------------------------------------------------------*
       vis-snx-nvl-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-snx-nvl-lun    to   v-car                  .
           move      w-exp-snx-nvl-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      19                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-snx-nvl-tbl    to   v-txt                  .
           move      rr-snx-nvl           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-snx-nvl-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Si/No esportazione archivio                *
      *    *-----------------------------------------------------------*
       acc-snx-exp-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        rr-dpz-inu           =    zero
                     go to acc-snx-exp-999.
       acc-snx-exp-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-snx-exp-lun    to   v-car                  .
           move      w-exp-snx-exp-num    to   v-ldt                  .
           move      "NSL#"               to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      20                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-snx-exp-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-ric    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      rr-snx-exp           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-snx-exp-999.
       acc-snx-exp-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-snx-exp             .
       acc-snx-exp-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Valore zero non ammesso, a meno che non si  *
      *                  * sia in Up                                   *
      *                  *---------------------------------------------*
           if        rr-snx-exp           not  = zero
                     go to acc-snx-exp-600.
           if        v-key                =    "UP  "
                     go to acc-snx-exp-600
           else      go to acc-snx-exp-999.
       acc-snx-exp-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-snx-exp-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-snx-exp-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-snx-exp-100.
       acc-snx-exp-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Si/No esportazione archivio             *
      *    *-----------------------------------------------------------*
       vis-snx-exp-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-snx-exp-lun    to   v-car                  .
           move      w-exp-snx-exp-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      20                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-snx-exp-tbl    to   v-txt                  .
           move      rr-snx-exp           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-snx-exp-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Data valorizzazione                        *
      *    *-----------------------------------------------------------*
       acc-dat-val-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        rr-tip-val           =    05
                     go to acc-dat-val-999.
      *                  *---------------------------------------------*
      *                  * Eventuale default                           *
      *                  *---------------------------------------------*
           if        rr-dat-val           =    zero
                     move  rr-dat-sit     to   rr-dat-val             .
       acc-dat-val-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-ric    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      21                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-dat-val           to   v-dat                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-dat-val-150.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-dat-val-999.
       acc-dat-val-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-dat                to   rr-dat-val             .
       acc-dat-val-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Valore zero non ammesso, ameno che non si   *
      *                  * sia in Up                                   *
      *                  *---------------------------------------------*
           if        rr-dat-val           not  = zero
                     go to acc-dat-val-600.
           if        v-key                =    "UP  "
                     go to acc-dat-val-600
           else      go to acc-dat-val-100.
       acc-dat-val-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-dat-val-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-dat-val-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-dat-val-100.
       acc-dat-val-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Data valorizzazione                     *
      *    *-----------------------------------------------------------*
       vis-dat-val-000.
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      21                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-dat-val           to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dat-val-999.
           exit.

      *    *===========================================================*
      *    * Controllo su tasto Do in parametri di selezione           *
      *    *-----------------------------------------------------------*
       tdo-ric-sel-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-tdo-ric-flg      .
      *              *-------------------------------------------------*
      *              * Test su data situazione                         *
      *              *-------------------------------------------------*
           if        rr-dat-sit           not  = zero
                     go to tdo-ric-sel-100.
           move      "Manca la data situazione !"
                                          to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-100.
      *              *-------------------------------------------------*
      *              * Uscita per controlli superati                   *
      *              *-------------------------------------------------*
           go to     tdo-ric-sel-999.
       tdo-ric-sel-900.
      *              *-------------------------------------------------*
      *              * Trattamento errore                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Emissione messaggio d'errore                *
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
      *              * Normalizzazione tipo salto pagina               *
      *              *-------------------------------------------------*
           if        rr-tip-slp           =    zero
                     move  01             to   rr-tip-slp             .
      *              *-------------------------------------------------*
      *              * Normalizzazione Si/No dettaglio prodotti        *
      *              *-------------------------------------------------*
           if        rr-snx-dtp           =    zero
                     move  01             to   rr-snx-dtp             .
      *              *-------------------------------------------------*
      *              * Normalizzazione tipo giacenza                   *
      *              *-------------------------------------------------*
           if        rr-tip-gia           =    zero
                     move  01             to   rr-tip-gia             .
      *              *-------------------------------------------------*
      *              * Normalizzazione Si/No solo se saldo diverso da  *
      *              * zero                                            *
      *              *-------------------------------------------------*
           if        rr-snx-zer           =    zero
                     move  01             to   rr-snx-zer             .
      *              *-------------------------------------------------*
      *              * Normalizzazione tipo di valorizzazione          *
      *              *-------------------------------------------------*
           if        rr-tip-val           =    zero
                     move  01             to   rr-tip-val             .
      *              *-------------------------------------------------*
      *              * Normalizzazione Si/No solo non valorizzati      *
      *              *-------------------------------------------------*
           if        rr-snx-nvl           =    zero
                     move  01             to   rr-snx-nvl             .
      *              *-------------------------------------------------*
      *              * Normalizzazione Si/No esportazione archivio     *
      *              *-------------------------------------------------*
           if        rr-snx-exp           =    zero
                     move  01             to   rr-snx-exp             .
      *              *-------------------------------------------------*
      *              * Normalizzazione data di valorizzazione          *
      *              *-------------------------------------------------*
           if        rr-dat-val           =    zero
                     move  rr-dat-sit     to   rr-dat-val             .
       reg-ric-sel-400.
      *              *-------------------------------------------------*
      *              * Si/No richiesta di selezione stampa             *
      *              *-------------------------------------------------*
           if        rr-snx-exp           =    03
                     move  "N"            to   w-cnt-fun-snx-stp      .
       reg-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione richieste di selezione                    *
      *    *-----------------------------------------------------------*
       nor-ric-sel-000.
      *              *-------------------------------------------------*
      *              * Precaricamento tabella dipendenze selezionate   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Contatore globale dipendenze letto in pre-  *
      *                  * esecuzione del programma                    *
      *                  *---------------------------------------------*
           move      w-dpz-ctr-dpz        to   rr-dpz-ctr-dpz         .
      *                  *---------------------------------------------*
      *                  * Test se selezionata una sola dipendenza     *
      *                  *---------------------------------------------*
           if        rr-dpz-inu           not  = zero
                     go to nor-ric-sel-100.
      *                  *---------------------------------------------*
      *                  * Preparazione contatori                      *
      *                  *---------------------------------------------*
           move      zero                 to   rr-dpz-ctr-sel         .
           move      zero                 to   w-cix-ctr-001          .
       nor-ric-sel-010.
           add       1                    to   w-cix-ctr-001          .
           if        w-cix-ctr-001        >    99
                     go to nor-ric-sel-100.
           if        w-dpz-ele-flg
                    (w-cix-ctr-001)       =    spaces
                     go to nor-ric-sel-010.
           add       1                    to   rr-dpz-ctr-sel         .
           if        rr-dpz-ctr-sel       >    99
                     go to nor-ric-sel-100.
           move      w-cix-ctr-001        to   rr-dpz-ele-cod
                                              (rr-dpz-ctr-sel)        .
           move      w-dpz-ele-den
                    (w-cix-ctr-001)       to   rr-dpz-ele-den
                                              (rr-dpz-ctr-sel)        .
      *                  *---------------------------------------------*
      *                  * Riciclo su elemento successivo              *
      *                  *---------------------------------------------*
           go to     nor-ric-sel-010.
       nor-ric-sel-100.
      *              *-------------------------------------------------*
      *              * Normalizzazioni altri campi                     *
      *              *-------------------------------------------------*
           move      zero                 to   rr-fso-dcp             .
           move      spaces               to   rr-fso-dcp-des         .
           move      zero                 to   rr-fso-dcp-ord         .
           move      zero                 to   rr-tip-slp             .
           move      zero                 to   rr-snx-dtp             .
           move      zero                 to   rr-dat-sit             .
           move      zero                 to   rr-tip-gia             .
           move      zero                 to   rr-snx-zer             .
           move      zero                 to   rr-tip-val             .
           move      zero                 to   rr-snx-nvl             .
           move      zero                 to   rr-snx-exp             .
           move      spaces               to   rr-cod-lst             .
           move      spaces               to   rr-cod-lst-des         .
           move      spaces               to   rr-cod-lst-vlt         .
           move      spaces               to   rr-cod-lst-vlt-des     .
           move      zero                 to   rr-cod-lst-vlt-dec     .
           move      spaces               to   rr-cod-lst-vlt-tdc     .
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
           move      07                   to   v-pos                  .
           move      14                   to   v-lto                  .
           move      74                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Messaggio nel box                               *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      60                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      09                   to   v-pos                  .
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
           move      70                   to   v-pos                  .
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
           move      71                   to   v-pos                  .
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
      *              *-------------------------------------------------*
      *              * Area riservata per espansioni speciali          *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-stp-fnz-spc      .
       pre-prm-stp-999.
           exit.

      *    *===========================================================*
      *    * Routine lettura archivio [zos] per [dcp]                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/lzosdcp0.lts"                   .

      *    *===========================================================*
      *    * Routine lettura tabella [zls]                             *
      *    *-----------------------------------------------------------*
       let-arc-zls-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zls-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice listino a Spaces                 *
      *              *-------------------------------------------------*
           if        w-let-arc-zls-cod    not  = spaces
                     go to let-arc-zls-400.
       let-arc-zls-200.
      *              *-------------------------------------------------*
      *              * Se codice listino a Spaces                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Descrizione listino                         *
      *                  *---------------------------------------------*
           move      "Listino Base        "
                                          to   w-let-arc-zls-des      .
      *                  *---------------------------------------------*
      *                  * Valuta per il listino, valuta base          *
      *                  *---------------------------------------------*
           move      c-sgl                to   w-let-arc-zls-vlt      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     let-arc-zls-999.
       let-arc-zls-400.
      *              *-------------------------------------------------*
      *              * Se codice listino a non Spaces                  *
      *              *-------------------------------------------------*
       let-arc-zls-450.
      *                  *---------------------------------------------*
      *                  * Lettura per codice                          *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODLST    "         to   f-key                  .
           move      w-let-arc-zls-cod    to   rf-zls-cod-lst         .
           move      "pgm/dcp/fls/ioc/obj/iofzls"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zls                 .
      *                  *---------------------------------------------*
      *                  * Deviazione secondo l'esito della lettura    *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to let-arc-zls-550.
       let-arc-zls-500.
      *                  *---------------------------------------------*
      *                  * Se record esistente                         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Descrizione listino                     *
      *                      *-----------------------------------------*
           move      rf-zls-des-lst       to   w-let-arc-zls-des      .
      *                      *-----------------------------------------*
      *                      * Valuta per il listino                   *
      *                      *-----------------------------------------*
           move      rf-zls-cod-vlt       to   w-let-arc-zls-vlt      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     let-arc-zls-999.
       let-arc-zls-550.
      *                  *---------------------------------------------*
      *                  * Se record non esistente                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Flag di uscita ad errore                *
      *                      *-----------------------------------------*
           move      "#"                  to   w-let-arc-zls-flg      .
      *                      *-----------------------------------------*
      *                      * Descrizione listino                     *
      *                      *-----------------------------------------*
           move      all   "."            to   w-let-arc-zls-des      .
      *                      *-----------------------------------------*
      *                      * Valuta per il listino                   *
      *                      *-----------------------------------------*
           move      spaces               to   w-let-arc-zls-vlt      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     let-arc-zls-999.
       let-arc-zls-999.
           exit.

      *    *===========================================================*
      *    * Routine lettura tabella [zvl]                             *
      *    *-----------------------------------------------------------*
       let-arc-zvl-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zvl-flg      .
      *              *-------------------------------------------------*
      *              * Deviazione a seconda del valore della sigla va- *
      *              * luta                                            *
      *              *-------------------------------------------------*
           if        w-let-arc-zvl-cod    =    c-sgl
                     go to let-arc-zvl-200
           else if   w-let-arc-zvl-cod    =    spaces
                     go to let-arc-zvl-400
           else      go to let-arc-zvl-600.
       let-arc-zvl-200.
      *              *-------------------------------------------------*
      *              * Se sigla valuta pari alla valuta base           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Descrizione valuta in lingua italiana       *
      *                  *---------------------------------------------*
           move      c-des                to   w-let-arc-zvl-des      .
      *                  *---------------------------------------------*
      *                  * Descrizione valuta internazionale           *
      *                  *---------------------------------------------*
           move      c-din                to   w-let-arc-zvl-din      .
      *                  *---------------------------------------------*
      *                  * Numero decimali valuta                      *
      *                  *---------------------------------------------*
           move      c-dec                to   w-let-arc-zvl-dec      .
      *                  *---------------------------------------------*
      *                  * Tipo di coefficiente per cambio valuta      *
      *                  *---------------------------------------------*
           move      c-tdc                to   w-let-arc-zvl-tdc      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     let-arc-zvl-999.
       let-arc-zvl-400.
      *              *-------------------------------------------------*
      *              * Se sigla valuta a Spaces                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Descrizione valuta in lingua italiana       *
      *                  *---------------------------------------------*
           move      spaces               to   w-let-arc-zvl-des      .
      *                  *---------------------------------------------*
      *                  * Descrizione valuta internazionale           *
      *                  *---------------------------------------------*
           move      spaces               to   w-let-arc-zvl-din      .
      *                  *---------------------------------------------*
      *                  * Numero decimali valuta                      *
      *                  *---------------------------------------------*
           move      zero                 to   w-let-arc-zvl-dec      .
      *                  *---------------------------------------------*
      *                  * Tipo di coefficiente per cambio valuta      *
      *                  *---------------------------------------------*
           move      spaces               to   w-let-arc-zvl-tdc      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     let-arc-zvl-999.
       let-arc-zvl-600.
      *              *-------------------------------------------------*
      *              * Se sigla valuta diversa dalla valuta base ed    *
      *              * anche diversa da Spaces                         *
      *              *-------------------------------------------------*
       let-arc-zvl-650.
      *                  *---------------------------------------------*
      *                  * Lettura per sigla valuta                    *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODVLT    "         to   f-key                  .
           move      w-let-arc-zvl-cod    to   rf-zvl-sgl-vlt         .
           move      "pgm/dcc/fls/ioc/obj/iofzvl"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zvl                 .
      *                  *---------------------------------------------*
      *                  * Deviazione secondo l'esito della lettura    *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to let-arc-zvl-750.
       let-arc-zvl-700.
      *                  *---------------------------------------------*
      *                  * Se record esistente                         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Descrizione valuta in lingua italiana   *
      *                      *-----------------------------------------*
           move      rf-zvl-des-ita       to   w-let-arc-zvl-des      .
      *                      *-----------------------------------------*
      *                      * Descrizione valuta internazionale       *
      *                      *-----------------------------------------*
           move      rf-zvl-des-int       to   w-let-arc-zvl-din      .
      *                      *-----------------------------------------*
      *                      * Numero decimali valuta                  *
      *                      *-----------------------------------------*
           move      rf-zvl-num-dec       to   w-let-arc-zvl-dec      .
      *                      *-----------------------------------------*
      *                      * Tipo di coefficiente per cambio valuta  *
      *                      *-----------------------------------------*
           move      rf-zvl-def-tdc       to   w-let-arc-zvl-tdc      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     let-arc-zvl-999.
       let-arc-zvl-750.
      *                  *---------------------------------------------*
      *                  * Se record non esistente                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Flag di uscita ad errore                *
      *                      *-----------------------------------------*
           move      "#"                  to   w-let-arc-zvl-flg      .
      *                      *-----------------------------------------*
      *                      * Descrizione valuta in lingua italiana   *
      *                      *-----------------------------------------*
           move      all   "."            to   w-let-arc-zvl-des      .
      *                      *-----------------------------------------*
      *                      * Descrizione valuta internazionale       *
      *                      *-----------------------------------------*
           move      all   "."            to   w-let-arc-zvl-din      .
      *                      *-----------------------------------------*
      *                      * Numero decimali valuta                  *
      *                      *-----------------------------------------*
           move      zero                 to   w-let-arc-zvl-dec      .
      *                      *-----------------------------------------*
      *                      * Tipo di coefficiente per cambio valuta  *
      *                      *-----------------------------------------*
           move      spaces               to   w-let-arc-zvl-tdc      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     let-arc-zvl-999.
       let-arc-zvl-999.
           exit.

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice filtro ordina-  *
      *    * mento e selezione per file [dcp]                          *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/azosdcp0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione codice listino             *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/acodzls0.acs"                   .
