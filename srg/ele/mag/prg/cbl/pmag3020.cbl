       Identification Division.
       Program-Id.                                 pmag3020           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    mag                 *
      *                                Settore:    mov                 *
      *                                   Fase:    mag302              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 15/09/93    *
      *                       Ultima revisione:    NdK del 30/04/10    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Richieste per il programma pmag3021:        *
      *                                                                *
      *                    Stampa brogliaccio movimenti di magazzino   *
      *                                                                *
      *                    SOLO PER ESTRAPOLARE MOVIMENTI CAUSALE 50   *
      *                    SENZA UNA DELLE DUE UBICAZIONI              *
      *                    Giorgio 15/11/24                            *
      *                                                                *
      *                    Comando in ELETTRA 'mag309'                 *
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
                     "mov"                                            .
      *        *-------------------------------------------------------*
      *        * Fase gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-fas                  pic  x(06) value
                     "mag302"                                         .
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10) value
                     "pmag3020"                                       .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  i-ide-des                  pic  x(40) value
                     "   BROGLIACCIO MOVIMENTI DI MAGAZZINO   "       .

      *    *===========================================================*
      *    * Area per il programma di esecuzione                       *
      *    *-----------------------------------------------------------*
       01  i-exe.
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma di esecuzione             *
      *        *-------------------------------------------------------*
           05  i-exe-pro                  pic  x(10) value
                     "pmag3021  "                                     .
      *        *-------------------------------------------------------*
      *        * Pathname del programma di esecuzione                  *
      *        *-------------------------------------------------------*
           05  i-exe-pat                  pic  x(40) value
                     "ele/mag/prg/obj/pmag3021                "       .

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
      *        * [zmc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/mag/fls/rec/rfzmc"                          .
      *        *-------------------------------------------------------*
      *        * [zmm]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/mag/fls/rec/rfzmm"                          .
      *        *-------------------------------------------------------*
      *        * [zos]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/azi/fls/rec/rfzos"                          .

      *    *===========================================================*
      *    * Work-area richieste per stampa                            *
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
      *        * Numero totale dipendenze                              *
      *        *-------------------------------------------------------*
           05  rr-dpz-ctr-dpz             pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Tipo stampa                                           *
      *        *                                                       *
      *        * - 01 : Per data registrazione                         *
      *        * - 02 : Per data immissione/ultima modifica            *
      *        *-------------------------------------------------------*
           05  rr-tip-stp                 pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Data iniziale                                         *
      *        *-------------------------------------------------------*
           05  rr-dat-ini                 pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Data finale                                           *
      *        *-------------------------------------------------------*
           05  rr-dat-fin                 pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Opzioni di stampa                                     *
      *        *                                                       *
      *        *  - Spaces : No                                        *
      *        *  - X      : Si                                        *
      *        *-------------------------------------------------------*
           05  rr-opz-stp.
      *            *---------------------------------------------------*
      *            * Costi e valori                                    *
      *            *---------------------------------------------------*
               10  rr-opz-stp-val         pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Intestazione stampa                               *
      *            *---------------------------------------------------*
               10  rr-opz-stp-int         pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Causale di magazzino da selezionare                   *
      *        *-------------------------------------------------------*
           05  rr-cau-mag                 pic  9(05)                  .
           05  rr-cau-mag-des             pic  x(30)                  .
      *        *-------------------------------------------------------*
      *        * Codice conto merce                                    *
      *        *-------------------------------------------------------*
           05  rr-cod-mic                 pic  x(03)                  .
           05  rr-cod-mic-des             pic  x(20)                  .
      *        *-------------------------------------------------------*
      *        * Codice archivio                                       *
      *        *-------------------------------------------------------*
           05  rr-cod-arc                 pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Si/no elementi selezionati per causale                *
      *        *-------------------------------------------------------*
           05  rr-cau-mag-sns             pic  9(01)                  .
      *        *-------------------------------------------------------*
      *        * Numero elementi selezionati per causale               *
      *        *-------------------------------------------------------*
           05  rr-cau-mag-els             pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Elenco elementi selezionati per causale               *
      *        *-------------------------------------------------------*
           05  rr-cau-mag-edd.
               10  rr-cau-mag-ele occurs 36.
                   15  rr-cau-mag-cod     pic  9(05)                  .
                   15  rr-cau-mag-dsc     pic  x(30)                  .
                   15  rr-cau-mag-mne     pic  x(05)                  .
      *        *-------------------------------------------------------*
      *        * Numero protocollo per accettazione min - max          *
      *        *-------------------------------------------------------*
           05  rr-num-min                 pic  9(09)                  .
           05  rr-num-max                 pic  9(09)                  .
      *        *-------------------------------------------------------*
      *        * Numero protocollo interno min - max                   *
      *        *-------------------------------------------------------*
           05  rr-npt-min                 pic  9(11)                  .
           05  rr-npt-max                 pic  9(11)                  .
      *        *-------------------------------------------------------*
      *        * Codice utente da selezionare                          *
      *        *-------------------------------------------------------*
           05  rr-cod-ute                 pic  x(08)                  .
      *        *-------------------------------------------------------*
      *        * Codice fase da selezionare                            *
      *        *-------------------------------------------------------*
           05  rr-cod-fas                 pic  x(06)                  .
      *        *-------------------------------------------------------*
      *        * Tipo magazzino                                        *
      *        *-------------------------------------------------------*
           05  rr-tip-mag                 pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Codice filtro di selezione per il tipo di magazzino   *
      *        *-------------------------------------------------------*
           05  rr-fso-mag                 pic  9(08)                  .
           05  rr-fso-mag-alf redefines
               rr-fso-mag                 pic  x(08)                  .
           05  rr-fso-mag-des             pic  x(40)                  .

      *    *===========================================================*
      *    * Work-area personalizzazioni                               *
      *    *-----------------------------------------------------------*
       01  w-prs.
      *        *-------------------------------------------------------*
      *        * Si/No gestione semilavorati attiva                    *
      *        *-------------------------------------------------------*
           05  w-prs-dps-snx              pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Si/No gestione materie prime attiva                   *
      *        *-------------------------------------------------------*
           05  w-prs-dpm-snx              pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Si/No gestione materiali vari attiva                  *
      *        *-------------------------------------------------------*
           05  w-prs-mtv-snx              pic  x(01)                  .

      *    *===========================================================*
      *    * Area di interfaccia per sottoprogramma         "pazi000d" *
      *    *-----------------------------------------------------------*
           copy      "pgm/azi/prg/cpy/pazi000d.pgl"                   .

      *    *===========================================================*
      *    * Work per salvataggi                                       *
      *    *-----------------------------------------------------------*
       01  w-sav.
           05  w-sav-tip-stp              pic  9(02)                  .
           05  w-sav-tip-mag              pic  9(02)                  .

      *    *===========================================================*
      *    * Work-area per comodi di accettazione                      *
      *    *-----------------------------------------------------------*
       01  w-acc.
      *        *-------------------------------------------------------*
      *        * Area per impostazione serie elementi da selezionare   *
      *        *-------------------------------------------------------*
           05  w-acc-ser-edd.
               10  w-acc-ser-edd-max      pic  9(02) value 36         .
               10  w-acc-ser-edd-els      pic  9(02)                  .
               10  w-acc-ser-edd-pev      pic  9(02)                  .
               10  w-acc-ser-edd-nel      pic  9(02)                  .
               10  w-acc-ser-edd-n1v      pic  9(02)                  .
               10  w-acc-ser-edd-nev      pic  9(02)                  .
               10  w-acc-ser-edd-nec      pic  9(02)                  .
               10  w-acc-ser-edd-fce      pic  x(01)                  .
               10  w-acc-ser-edd-led.
                   15  w-acc-ser-edd-rig  pic  x(03)                  .
                   15  filler             pic  x(03)                  .
                   15  w-acc-ser-edd-cod  pic  x(05)                  .
                   15  filler             pic  x(03)                  .
                   15  w-acc-ser-edd-des  pic  x(30)                  .
                   15  filler             pic  x(15)                  .
                   15  w-acc-ser-edd-mne  pic  x(05)                  .
                   15  filler             pic  x(12)                  .
               10  w-acc-ser-edd-c01      pic  9(02)                  .
               10  w-acc-ser-edd-c02      pic  9(02)                  .
               10  w-acc-ser-edd-c0a      pic  9(02)                  .
               10  w-acc-ser-edd-c0b      pic  9(02)                  .
               10  w-acc-ser-edd-c0c      pic  9(02)                  .
               10  w-acc-ser-edd-c0p      pic  9(02)                  .
               10  w-acc-ser-edd-c0q      pic  9(02)                  .
               10  w-acc-ser-edd-c0r      pic  9(02)                  .
               10  w-acc-ser-edd-spe      pic  9(05)                  .
               10  w-acc-ser-edd-svk      pic  x(08)                  .
               10  w-acc-ser-edd-stu      pic  x(08)                  .
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
      *    * Work per subroutines di Let                               *
      *    *-----------------------------------------------------------*
       01  w-let.
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [zmc]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-zmc.
               10  w-let-arc-zmc-flg      pic  x(01)                  .
               10  w-let-arc-zmc-cod      pic  9(05)                  .
               10  w-let-arc-zmc-des      pic  x(30)                  .
               10  w-let-arc-zmc-mne      pic  x(05)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [zmm]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-zmm.
               10  w-let-arc-zmm-flg      pic  x(01)                  .
               10  w-let-arc-zmm-cod      pic  x(03)                  .
               10  w-let-arc-zmm-des      pic  x(20)                  .

      *    *===========================================================*
      *    * Work per Let su archivio [zos] per [dcp]                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/lzosdcp0.ltw"                   .
              
      *    *===========================================================*
      *    * Work per Let su archivio [zos] per [dps]                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/dps/prg/cpy/lzosdps0.ltw"                   .
              
      *    *===========================================================*
      *    * Work per Let su archivio [zos] per [dpm]                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/dpm/prg/cpy/lzosdpm0.ltw"                   .
              
      *    *===========================================================*
      *    * Work per Let su archivio [zos] per [mtv]                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/mtv/prg/cpy/lzosmtv0.ltw"                   .
              
      *    *===========================================================*
      *    * Work per accettazioni campi espansi                       *
      *    *-----------------------------------------------------------*
       01  w-exp.
      *        *-------------------------------------------------------*
      *        * Work per : Tipo stampa                                *
      *        *-------------------------------------------------------*
           05  w-exp-tip-stp.
               10  w-exp-tip-stp-num      pic  9(02)       value 2    .
               10  w-exp-tip-stp-lun      pic  9(02)       value 35   .
               10  w-exp-tip-stp-tbl.
                   15  filler             pic  x(35) value
                            "Per data registrazione             "     .
                   15  filler             pic  x(35) value
                            "Per data immissione/ultima modifica"     .
      *        *-------------------------------------------------------*
      *        * Work per : Opzioni di stampa                          *
      *        *-------------------------------------------------------*
           05  w-exp-opz-stp.
               10  w-exp-opz-stp-num      pic  9(02)       value 2    .
               10  w-exp-opz-stp-lun      pic  9(02)       value 40   .
               10  w-exp-opz-stp-tbl.
                   15  filler             pic  x(40) value
                            "[ ] Stampa costi e valori               ".
                   15  filler             pic  x(40) value
                            "[ ] Stampa intestazione pagina          ".
      *        *-------------------------------------------------------*
      *        * Work per : Tipo prodotto                              *
      *        *-------------------------------------------------------*
           05  w-exp-tip-mag.
               10  w-exp-tip-mag-num      pic  9(02)       value 05   .
               10  w-exp-tip-mag-lun      pic  9(02)       value 25   .
               10  w-exp-tip-mag-tbl.
                   15  filler             pic  x(25) value
                            "Tutti                    "               .
                   15  filler             pic  x(25) value
                            "Prodotti di vendita      "               .
                   15  filler             pic  x(25) value
                            "Semilavorati             "               .
                   15  filler             pic  x(25) value
                            "Materie prime            "               .
                   15  filler             pic  x(25) value
                            "Materiali vari           "               .
               10  w-exp-tip-mag-tbr redefines
                   w-exp-tip-mag-tbl.
                   15  w-exp-tip-mag-ele occurs 05
                                          pic  x(25)                  .
               10  w-exp-tip-mag-ast.
                   15  filler             pic  x(10) value
                            "9901020304"                              .
               10  w-exp-tip-mag-ass redefines
                   w-exp-tip-mag-ast.
                   15  w-exp-tip-mag-tpm occurs 05
                                          pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Work per : Tipo magazzino limitato ai soli tipi am-   *
      *        *            messi                                      *
      *        *-------------------------------------------------------*
           05  w-exp-tpm-amm.
               10  w-exp-tpm-amm-num      pic  9(02)                  .
               10  w-exp-tpm-amm-lun      pic  9(02) value 25         .
               10  w-exp-tpm-amm-tbl.
                   15  w-exp-tpm-amm-ele occurs 05
                                          pic  x(25)                  .
               10  w-exp-tpm-amm-ass.
                   15  w-exp-tpm-amm-tpm occurs 05
                                          pic  9(02)                  .
               10  w-exp-tpm-amm-i01      pic  9(02)                  .
               10  w-exp-tpm-amm-c01      pic  9(02)                  .
               10  w-exp-tpm-amm-c02      pic  9(02)                  .

      *    *===========================================================*
      *    * Work area locale                                          *
      *    *-----------------------------------------------------------*
       01  w-wrk.
      *        *-------------------------------------------------------*
      *        * Numero protocollo per accettazione                    *
      *        *-------------------------------------------------------*
           05  w-wrk-npt-acc              pic  9(09)                  .
           05  w-wrk-npt-acc-r            redefines
               w-wrk-npt-acc.
               10  w-wrk-num-saa          pic  9(03)                  .
               10  w-wrk-num-npg          pic  9(06)                  .
      *        *-------------------------------------------------------*
      *        * Numero protocollo internamente                        *
      *        *-------------------------------------------------------*
           05  w-wrk-npt-int              pic  9(11)                  .
           05  w-wrk-npt-int-r            redefines
               w-wrk-npt-int.
               10  w-wrk-npm-saa          pic  9(03)                  .
               10  w-wrk-npm-dpz          pic  9(02)                  .
               10  w-wrk-npm-npg          pic  9(06)                  .

      *    *===========================================================*
      *    * Work-area per allineamenti a destra o a sinistra oppure   *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cpw"                   .

      *    *===========================================================*
      *    * Work per subroutines di Err                               *
      *    *-----------------------------------------------------------*
       01  w-err.
      *        *-------------------------------------------------------*
      *        * Work per Err con box centrale                         *
      *        *-------------------------------------------------------*
           05  w-err-box-err.
               10  w-err-box-err-msg      pic  x(65)                  .

      *    *===========================================================*
      *    * Link-area per accettazione codice causale di magazzino    *
      *    *-----------------------------------------------------------*
           copy      "pgm/mag/prg/cpy/acmnzmc0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice conto merce             *
      *    *-----------------------------------------------------------*
           copy      "pgm/mag/prg/cpy/acdezmm0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice filtro selezione e or-  *
      *    * dinamento per file [dcp]                                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/azosdcp0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice filtro selezione e or-  *
      *    * dinamento per file [dps]                                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/dps/prg/cpy/azosdps0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice filtro selezione e or-  *
      *    * dinamento per file [dpm]                                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/dpm/prg/cpy/azosdpm0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice filtro selezione e or-  *
      *    * dinamento per file [mtv]                                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/mtv/prg/cpy/azosmtv0.acl"                   .

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
      *              * Altrimenti memorizzazione in campo richieste    *
      *              *-------------------------------------------------*
           move      w-dpz-cod-prg        to   rr-dpz-inu             .
           move      w-dpz-den-prg        to   rr-dpz-inu-den         .
           move      w-dpz-ctr-dpz        to   rr-dpz-ctr-dpz         .
      *              *-------------------------------------------------*
      *              * Lettura personalizzazioni                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Si/No gestione semilavorati attiva          *
      *                  *---------------------------------------------*
           perform   prs-dps-snx-000      thru prs-dps-snx-999        .
      *                  *---------------------------------------------*
      *                  * Si/No gestione materie prime attiva         *
      *                  *---------------------------------------------*
           perform   prs-dpm-snx-000      thru prs-dpm-snx-999        .
      *                  *---------------------------------------------*
      *                  * Si/No gestione materiali vari attiva        *
      *                  *---------------------------------------------*
           perform   prs-mtv-snx-000      thru prs-mtv-snx-999        .
      *              *-------------------------------------------------*
      *              * Determinazione tipi magazzino ammessi           *
      *              *-------------------------------------------------*
           perform   det-tpm-amm-000      thru det-tpm-amm-999        .
       pre-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Lettura personalizzazione : Si/No gestione semilavorati   *
      *    *                             attiva                        *
      *    *-----------------------------------------------------------*
       prs-dps-snx-000.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione                       *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/dps[snx]"       to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-alf          to   w-prs-dps-snx
           else      move  spaces         to   w-prs-dps-snx          .
      *              *-------------------------------------------------*
      *              * Normalizzazione personalizzazione               *
      *              *-------------------------------------------------*
           if        w-prs-dps-snx        =    "S" or
                     w-prs-dps-snx        =    "N"
                     go to prs-dps-snx-999.
           move      "N"                  to   w-prs-dps-snx          .
       prs-dps-snx-999.
           exit.

      *    *===========================================================*
      *    * Lettura personalizzazione : Si/No gestione materie prime  *
      *    *                             attiva                        *
      *    *-----------------------------------------------------------*
       prs-dpm-snx-000.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione                       *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/dpm[snx]"       to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-alf          to   w-prs-dpm-snx
           else      move  spaces         to   w-prs-dpm-snx          .
      *              *-------------------------------------------------*
      *              * Normalizzazione personalizzazione               *
      *              *-------------------------------------------------*
           if        w-prs-dpm-snx        =    "S" or
                     w-prs-dpm-snx        =    "N"
                     go to prs-dpm-snx-999.
           move      "N"                  to   w-prs-dpm-snx          .
       prs-dpm-snx-999.
           exit.

      *    *===========================================================*
      *    * Lettura personalizzazione : Si/No gestione materiali vari *
      *    *                             attiva                        *
      *    *-----------------------------------------------------------*
       prs-mtv-snx-000.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione                       *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/mtv[snx]"       to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-alf          to   w-prs-mtv-snx
           else      move  spaces         to   w-prs-mtv-snx          .
      *              *-------------------------------------------------*
      *              * Normalizzazione personalizzazione               *
      *              *-------------------------------------------------*
           if        w-prs-mtv-snx        =    "S" or
                     w-prs-mtv-snx        =    "N"
                     go to prs-mtv-snx-999.
           move      "N"                  to   w-prs-mtv-snx          .
       prs-mtv-snx-999.
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
      *              * [zmc]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofzmc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zmc                 .
      *              *-------------------------------------------------*
      *              * [zmm]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofzmm"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zmm                 .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione causale di magazzino   *
      *              *-------------------------------------------------*
           perform   cod-mne-zmc-opn-000  thru cod-mne-zmc-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice conto merce     *
      *              *-------------------------------------------------*
           perform   cod-des-zmm-opn-000  thru cod-des-zmm-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice filtro ordina-  *
      *              * mento e selezione per file [dcp]                *
      *              *-------------------------------------------------*
           perform   cod-zos-dcp-opn-000  thru cod-zos-dcp-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice filtro ordina-  *
      *              * mento e selezione per file [dps]                *
      *              *-------------------------------------------------*
           perform   cod-zos-dps-opn-000  thru cod-zos-dps-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice filtro ordina-  *
      *              * mento e selezione per file [dpm]                *
      *              *-------------------------------------------------*
           perform   cod-zos-dpm-opn-000  thru cod-zos-dpm-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice filtro ordina-  *
      *              * mento e selezione per file [mtv]                *
      *              *-------------------------------------------------*
           perform   cod-zos-mtv-opn-000  thru cod-zos-mtv-opn-999    .
       rou-opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Close files per richieste                                 *
      *    *-----------------------------------------------------------*
       rou-cls-fls-000.
      *              *-------------------------------------------------*
      *              * [zmc]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofzmc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zmc                 .
      *              *-------------------------------------------------*
      *              * [zmm]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofzmm"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zmm                 .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione causale di magazzino  *
      *              *-------------------------------------------------*
           perform   cod-mne-zmc-cls-000  thru cod-mne-zmc-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice conto merce    *
      *              *-------------------------------------------------*
           perform   cod-des-zmm-cls-000  thru cod-des-zmm-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice filtro ordina- *
      *              * mento e selezione per file [dcp]                *
      *              *-------------------------------------------------*
           perform   cod-zos-dcp-cls-000  thru cod-zos-dcp-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice filtro ordina- *
      *              * mento e selezione per file [dps]                *
      *              *-------------------------------------------------*
           perform   cod-zos-dps-cls-000  thru cod-zos-dps-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice filtro ordina- *
      *              * mento e selezione per file [dpm]                *
      *              *-------------------------------------------------*
           perform   cod-zos-dpm-cls-000  thru cod-zos-dpm-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice filtro ordina- *
      *              * mento e selezione per file [mtv]                *
      *              *-------------------------------------------------*
           perform   cod-zos-mtv-cls-000  thru cod-zos-mtv-cls-999    .
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
      *                  * Tipo stampa                                 *
      *                  *---------------------------------------------*
           perform   acc-tip-stp-000      thru acc-tip-stp-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
       acc-ric-sel-150.
      *                  *---------------------------------------------*
      *                  * Data iniziale                               *
      *                  *---------------------------------------------*
           perform   acc-dat-ini-000      thru acc-dat-ini-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-100.
       acc-ric-sel-200.
      *                  *---------------------------------------------*
      *                  * Data finale                                 *
      *                  *---------------------------------------------*
           perform   acc-dat-fin-000      thru acc-dat-fin-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-150.
       acc-ric-sel-225.
      *                  *---------------------------------------------*
      *                  * Opzioni di stampa                           *
      *                  *---------------------------------------------*
           perform   acc-opz-stp-000      thru acc-opz-stp-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-200.
       acc-ric-sel-250.
      *                  *---------------------------------------------*
      *                  * Causale di magazzino                        *
      *                  *---------------------------------------------*
           perform   acc-cau-mag-000      thru acc-cau-mag-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-225.
       acc-ric-sel-300.
      *                  *---------------------------------------------*
      *                  * Codice merce in conto                       *
      *                  *---------------------------------------------*
           perform   acc-cod-mic-000      thru acc-cod-mic-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-250.
       acc-ric-sel-350.
      *                  *---------------------------------------------*
      *                  * Codice archivio                             *
      *                  *---------------------------------------------*
           perform   acc-cod-arc-000      thru acc-cod-arc-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-300.
       acc-ric-sel-400.
      *                  *---------------------------------------------*
      *                  * Numero protocollo min                       *
      *                  *---------------------------------------------*
           perform   acc-npt-min-000      thru acc-npt-min-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-350.
       acc-ric-sel-450.
      *                  *---------------------------------------------*
      *                  * Numero protocollo max                       *
      *                  *---------------------------------------------*
           perform   acc-npt-max-000      thru acc-npt-max-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-400.
       acc-ric-sel-500.
      *                  *---------------------------------------------*
      *                  * Codice utente                               *
      *                  *---------------------------------------------*
           perform   acc-cod-ute-000      thru acc-cod-ute-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-450.
       acc-ric-sel-550.
      *                  *---------------------------------------------*
      *                  * Codice fase                                 *
      *                  *---------------------------------------------*
           perform   acc-cod-fas-000      thru acc-cod-fas-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-500.
       acc-ric-sel-600.
      *                  *---------------------------------------------*
      *                  * Tipo magazzino                              *
      *                  *---------------------------------------------*
           perform   acc-tip-mag-000      thru acc-tip-mag-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-550.
       acc-ric-sel-650.
      *                  *---------------------------------------------*
      *                  * Codice filtro di selezione                  *
      *                  *---------------------------------------------*
           perform   acc-fso-mag-000      thru acc-fso-mag-999        .
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
      *              * Tipo stampa                                     *
      *              *-------------------------------------------------*
           perform   pmt-tip-stp-000      thru pmt-tip-stp-999        .
      *              *-------------------------------------------------*
      *              * Data iniziale                                   *
      *              *-------------------------------------------------*
           perform   pmt-dat-ini-000      thru pmt-dat-ini-999        .
      *              *-------------------------------------------------*
      *              * Data finale                                     *
      *              *-------------------------------------------------*
           perform   pmt-dat-fin-000      thru pmt-dat-fin-999        .
      *              *-------------------------------------------------*
      *              * Opzioni di stampa                               *
      *              *-------------------------------------------------*
           perform   pmt-opz-stp-000      thru pmt-opz-stp-999        .
      *              *-------------------------------------------------*
      *              * Prompt selezioni                                *
      *              *-------------------------------------------------*
           perform   pmt-sel-stp-000      thru pmt-sel-stp-999        .
      *              *-------------------------------------------------*
      *              * Causale di magazzino                            *
      *              *-------------------------------------------------*
           perform   pmt-cau-mag-000      thru pmt-cau-mag-999        .
      *              *-------------------------------------------------*
      *              * Codice conto merce                              *
      *              *-------------------------------------------------*
           perform   pmt-cod-mic-000      thru pmt-cod-mic-999        .
      *              *-------------------------------------------------*
      *              * Codice archivio                                 *
      *              *-------------------------------------------------*
           perform   pmt-cod-arc-000      thru pmt-cod-arc-999        .
      *              *-------------------------------------------------*
      *              * Numero protocollo min                           *
      *              *-------------------------------------------------*
           perform   pmt-npt-min-000      thru pmt-npt-min-999        .
      *              *-------------------------------------------------*
      *              * Numero protocollo max                           *
      *              *-------------------------------------------------*
           perform   pmt-npt-max-000      thru pmt-npt-max-999        .
      *              *-------------------------------------------------*
      *              * Codice utente                                   *
      *              *-------------------------------------------------*
           perform   pmt-cod-ute-000      thru pmt-cod-ute-999        .
      *              *-------------------------------------------------*
      *              * Codice fase                                     *
      *              *-------------------------------------------------*
           perform   pmt-cod-fas-000      thru pmt-cod-fas-999        .
      *              *-------------------------------------------------*
      *              * Tipo magazzino                                  *
      *              *-------------------------------------------------*
           perform   pmt-tip-mag-000      thru pmt-tip-mag-999        .
      *              *-------------------------------------------------*
      *              * Codice filtro di selezione                      *
      *              *-------------------------------------------------*
           perform   pmt-fso-mag-000      thru pmt-fso-mag-999        .
       pmt-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Tipo stampa                      *
      *    *-----------------------------------------------------------*
       pmt-tip-stp-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tipo stampa                :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-tip-stp-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Data iniziale                    *
      *    *-----------------------------------------------------------*
       pmt-dat-ini-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      01                   to   v-pos                  .
      *
           if        rr-tip-stp           =    02
                     move  "Data immissione        dal :"
                                          to   v-alf
           else      move  "Data registrazione     dal :"
                                          to   v-alf                  .
      *
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-dat-ini-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Data finale                      *
      *    *-----------------------------------------------------------*
       pmt-dat-fin-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      "al :"               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-dat-fin-999.
           exit.

      *    *===========================================================*
      *    * Prompt per opzioni di stampa                              *
      *    *-----------------------------------------------------------*
       pmt-opz-stp-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Opzioni di stampa          :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-opz-stp-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Prompt selezioni                 *
      *    *-----------------------------------------------------------*
       pmt-sel-stp-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Selezioni su :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-sel-stp-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Causale di magazzino             *
      *    *-----------------------------------------------------------*
       pmt-cau-mag-000.
      *              *-------------------------------------------------*
      *              * Test se da visualizzare                         *
      *              *-------------------------------------------------*
           if        rr-tip-stp           =    02
                     go to pmt-cau-mag-999.
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "- Causale di magazzino     :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cau-mag-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Codice conto merce               *
      *    *-----------------------------------------------------------*
       pmt-cod-mic-000.
      *              *-------------------------------------------------*
      *              * Test se da visualizzare                         *
      *              *-------------------------------------------------*
           if        rr-tip-stp           =    02
                     go to pmt-cod-mic-999.
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "- Codice conto merce       :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cod-mic-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Codice archivio                  *
      *    *-----------------------------------------------------------*
       pmt-cod-arc-000.
      *              *-------------------------------------------------*
      *              * Test se da visualizzare                         *
      *              *-------------------------------------------------*
           if        rr-tip-stp           =    02
                     go to pmt-cod-arc-999.
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "- Codice archivio          :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cod-arc-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Protocollo di magazzino min      *
      *    *-----------------------------------------------------------*
       pmt-npt-min-000.
      *              *-------------------------------------------------*
      *              * Test se da visualizzare                         *
      *              *-------------------------------------------------*
           if        rr-tip-stp           =    02
                     go to pmt-npt-min-999.
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "- Numero protocollo     da :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-npt-min-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Protocollo di magazzino max      *
      *    *-----------------------------------------------------------*
       pmt-npt-max-000.
      *              *-------------------------------------------------*
      *              * Test se da visualizzare                         *
      *              *-------------------------------------------------*
           if        rr-tip-stp           =    02
                     go to pmt-npt-max-999.
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      "a :"                to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-npt-max-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Codice utente                    *
      *    *-----------------------------------------------------------*
       pmt-cod-ute-000.
      *              *-------------------------------------------------*
      *              * Test se da visualizzare                         *
      *              *-------------------------------------------------*
           if        rr-tip-stp           not  = 02
                     go to pmt-cod-ute-999.
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "- Utente                   :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cod-ute-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Codice fase                      *
      *    *-----------------------------------------------------------*
       pmt-cod-fas-000.
      *              *-------------------------------------------------*
      *              * Test se da visualizzare                         *
      *              *-------------------------------------------------*
           if        rr-tip-stp           not  = 02
                     go to pmt-cod-fas-999.
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "- Programma                :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cod-fas-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Tipo magazzino                   *
      *    *-----------------------------------------------------------*
       pmt-tip-mag-000.
      *              *-------------------------------------------------*
      *              * Test se prompt da visualizzare                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se nella tabella tipi magazzino ammessi e'  *
      *                  * stato rilevato un solo elemento il prompt   *
      *                  * non si visualizza                           *
      *                  *---------------------------------------------*
           if        w-exp-tpm-amm-num    =    1
                     go to pmt-tip-mag-999.
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tipo prodotto              :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-tip-mag-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Codice filtro di selezione       *
      *    *-----------------------------------------------------------*
       pmt-fso-mag-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice filtro di selezione :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-fso-mag-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo selezione : Tipo stampa                *
      *    *-----------------------------------------------------------*
       acc-tip-stp-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      rr-tip-stp           to   w-sav-tip-stp          .
       acc-tip-stp-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-stp-lun    to   v-car                  .
           move      w-exp-tip-stp-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      05                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-tip-stp-tbl    to   v-txt                  .
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
      *                  *---------------------------------------------*
      *                  * Se valore attuale come precedente : oltre   *
      *                  *---------------------------------------------*
           if        rr-tip-stp           =    w-sav-tip-stp
                     go to acc-tip-stp-800.
      *                  *---------------------------------------------*
      *                  * Se valore attuale 01 e precedente a zero :  *
      *                  * oltre                                       *
      *                  *---------------------------------------------*
           if        rr-tip-stp           =    01   and
                     w-sav-tip-stp        =    zero
                     go to acc-tip-stp-800.
      *                  *---------------------------------------------*
      *                  * Erase linee impegnate                       *
      *                  *---------------------------------------------*
           move      "EL"                 to   v-ope                  .
           move      12                   to   v-lin                  .
           move      17                   to   v-lto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Rivisualizzazione prompt per data iniziale  *
      *                  *---------------------------------------------*
           perform   pmt-dat-ini-000      thru pmt-dat-ini-999        .
      *                  *---------------------------------------------*
      *                  * Se valore attuale : 01                      *
      *                  *---------------------------------------------*
           if        rr-tip-stp           not  = 01
                     go to acc-tip-stp-700.
      *                      *-----------------------------------------*
      *                      * Prompt per Fase                         *
      *                      *-----------------------------------------*
           perform   pmt-cod-fas-000      thru pmt-cod-fas-999        .
      *                      *-----------------------------------------*
      *                      * Prompt per Causale di magazzino         *
      *                      *-----------------------------------------*
           perform   pmt-cau-mag-000      thru pmt-cau-mag-999        .
      *                      *-----------------------------------------*
      *                      * Prompt per Codice conto merce           *
      *                      *-----------------------------------------*
           perform   pmt-cod-mic-000      thru pmt-cod-mic-999        .
      *                      *-----------------------------------------*
      *                      * Prompt per numero protocollo min        *
      *                      *-----------------------------------------*
           perform   pmt-npt-min-000      thru pmt-npt-min-999        .
      *                      *-----------------------------------------*
      *                      * Prompt per numero protocollo max        *
      *                      *-----------------------------------------*
           perform   pmt-npt-max-000      thru pmt-npt-max-999        .
      *                      *-----------------------------------------*
      *                      * Codice Utente                           *
      *                      *-----------------------------------------*
           if        rr-cod-ute           not  = spaces
                     move  spaces         to   rr-cod-ute
                     perform vis-cod-ute-000
                                          thru vis-cod-ute-999        .
      *                      *-----------------------------------------*
      *                      * Codice Fase                             *
      *                      *-----------------------------------------*
           if        rr-cod-fas           not  = spaces
                     move  spaces         to   rr-cod-fas
                     perform vis-cod-fas-000
                                          thru vis-cod-fas-999        .
      *                      *-----------------------------------------*
      *                      * Oltre                                   *
      *                      *-----------------------------------------*
           go to     acc-tip-stp-800.
       acc-tip-stp-700.
      *                  *---------------------------------------------*
      *                  * Se valore attuale : 02                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Prompt per Utente                       *
      *                      *-----------------------------------------*
           perform   pmt-cod-ute-000      thru pmt-cod-ute-999        .
      *                      *-----------------------------------------*
      *                      * Prompt per Fase                         *
      *                      *-----------------------------------------*
           perform   pmt-cod-fas-000      thru pmt-cod-fas-999        .
      *                      *-----------------------------------------*
      *                      * Causale di magazzino normalizzata       *
      *                      *-----------------------------------------*
           perform   nor-ric-sel-cau-000  thru nor-ric-sel-cau-999    .
           perform   vis-cau-mag-000      thru vis-cau-mag-999        .
           perform   vis-cau-mag-des-000  thru vis-cau-mag-des-999    .
      *                      *-----------------------------------------*
      *                      * Codice conto merce normalizzato         *
      *                      *-----------------------------------------*
           perform   nor-ric-sel-mic-000  thru nor-ric-sel-mic-999    .
           perform   vis-cod-mic-000      thru vis-cod-mic-999        .
           perform   vis-cod-mic-des-000  thru vis-cod-mic-des-999    .
      *                      *-----------------------------------------*
      *                      * Protocolli min - max normalizzati       *
      *                      *-----------------------------------------*
           move      zero                 to   rr-num-min             .
           move      zero                 to   rr-num-max             .
           move      zero                 to   rr-npt-min             .
           move      zero                 to   rr-npt-max             .
           perform   vis-npt-min-000      thru vis-npt-min-999        .
           perform   vis-npt-max-000      thru vis-npt-max-999        .
      *                      *-----------------------------------------*
      *                      * Oltre                                   *
      *                      *-----------------------------------------*
           go to     acc-tip-stp-800.
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
      *    * Accettazione campo selezione : Data iniziale              *
      *    *-----------------------------------------------------------*
       acc-dat-ini-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-dat-ini-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      07                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           if        w-cnt-sts-imp-ric    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      rr-dat-ini           to   v-dat                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-dat-ini-999.
       acc-dat-ini-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-dat                to   rr-dat-ini             .
       acc-dat-ini-300.
      *              *-------------------------------------------------*
      *              * Se Find                                         *
      *              *-------------------------------------------------*
           if        v-key                not  = "FIND"
                     go to acc-dat-ini-400.
      *                  *---------------------------------------------*
      *                  * Find su magazzino                           *
      *                  *---------------------------------------------*
           perform   fnd-arc-mmt-000      thru fnd-arc-mmt-999        .
      *                  *---------------------------------------------*
      *                  * A reimpostazione                            *
      *                  *---------------------------------------------*
           go to     acc-dat-ini-100.
       acc-dat-ini-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-dat-ini-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-dat-ini-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-dat-ini-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-dat-ini-100.
       acc-dat-ini-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo selezione : Data finale                *
      *    *-----------------------------------------------------------*
       acc-dat-fin-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-dat-fin-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      07                   to   v-lin                  .
           move      46                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           if        w-cnt-sts-imp-ric    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      rr-dat-fin           to   v-dat                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-dat-fin-999.
       acc-dat-fin-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-dat                to   rr-dat-fin             .
       acc-dat-fin-300.
      *              *-------------------------------------------------*
      *              * Se Find                                         *
      *              *-------------------------------------------------*
           if        v-key                not  = "FIND"
                     go to acc-dat-fin-400.
      *                  *---------------------------------------------*
      *                  * Find su magazzino                           *
      *                  *---------------------------------------------*
           perform   fnd-arc-mmt-000      thru fnd-arc-mmt-999        .
      *                  *---------------------------------------------*
      *                  * A reimpostazione                            *
      *                  *---------------------------------------------*
           go to     acc-dat-fin-100.
       acc-dat-fin-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-dat-fin-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-dat-fin-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-dat-fin-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-dat-fin-100.
       acc-dat-fin-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Opzioni di stampa                    *
      *    *-----------------------------------------------------------*
       acc-opz-stp-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-opz-stp-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "C"                  to   v-tip                  .
           move      w-exp-opz-stp-lun    to   v-car                  .
           move      w-exp-opz-stp-num    to   v-ldt                  .
           move      "X"                  to   v-msk                  .
           move      "BX"                 to   v-edm                  .
           move      09                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-opz-stp-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-opz-stp           to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-opz-stp-999.
       acc-opz-stp-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   rr-opz-stp             .
       acc-opz-stp-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-opz-stp-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-opz-stp-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-opz-stp-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-opz-stp-100.
       acc-opz-stp-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo selezione : Causale di magazzino       *
      *    *-----------------------------------------------------------*
       acc-cau-mag-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
           move      50                   to   rr-cau-mag             .
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        rr-tip-stp           not  = 01
                     go to acc-cau-mag-999.
       acc-cau-mag-100.
      *              *-------------------------------------------------*
      *              * Note operative                                  *
      *              *-------------------------------------------------*
           move      "NT"                 to   v-ope                  .
           move      "Tasto funzione SELEZIONE per scegliere piu' causal
      -              "i (fino ad un massimo di 36)  "
                                          to   v-nt1                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-zmc-ope      .
           move      rr-cau-mag           to   w-cod-mne-zmc-cod      .
           move      13                   to   w-cod-mne-zmc-lin      .
           move      30                   to   w-cod-mne-zmc-pos      .
           move      13                   to   w-cod-mne-zmc-dln      .
           move      40                   to   w-cod-mne-zmc-dps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           if        w-cnt-sts-imp-ric    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      "SLCT"               to   v-pfk (11)             .
           perform   cod-mne-zmc-cll-000  thru cod-mne-zmc-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-mne-zmc-foi-000  thru cod-mne-zmc-foi-999    .
       acc-cau-mag-110.
           perform   cod-mne-zmc-cll-000  thru cod-mne-zmc-cll-999    .
           if        w-cod-mne-zmc-ope    =    "F+"
                     go to acc-cau-mag-115.
           if        w-cod-mne-zmc-ope    =    "AC"
                     go to acc-cau-mag-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cau-mag-115.
           perform   cod-mne-zmc-foi-000  thru cod-mne-zmc-foi-999    .
           go to     acc-cau-mag-110.
       acc-cau-mag-120.
           move      w-cod-mne-zmc-cod    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Cancellazione eventuali note operative          *
      *              *-------------------------------------------------*
           move      "NT"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-cau-mag-999.
       acc-cau-mag-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-cau-mag             .
       acc-cau-mag-300.
      *              *-------------------------------------------------*
      *              * Se 'Select'                                     *
      *              *-------------------------------------------------*
           if        v-key                not  = "SLCT"
                     go to acc-cau-mag-400.
      *                  *---------------------------------------------*
      *                  * Select su causali di magazzino              *
      *                  *---------------------------------------------*
           move      rr-cau-mag-els       to   w-acc-ser-edd-els      .
           perform   acc-zcc-esl-000      thru acc-zcc-esl-999        .
      *                  *---------------------------------------------*
      *                  * Verifica se elementi selezionati per atti-  *
      *                  * vazione del segnale di elementi selezionati *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazione segnale di selezione    *
      *                      * effettuata                              *
      *                      *-----------------------------------------*
           move      zero                 to   rr-cau-mag-sns         .
           move      zero                 to   w-acc-ser-edd-c01      .
       acc-cau-mag-310.
           add       1                    to   w-acc-ser-edd-c01      .
           if        w-acc-ser-edd-c01    >    w-acc-ser-edd-max
                     go to acc-cau-mag-320.
           if        rr-cau-mag-cod
                    (w-acc-ser-edd-c01)   =    zero
                     go to acc-cau-mag-320.
      *                      *-----------------------------------------*
      *                      * Attivazione segnale di selezione effet- *
      *                      * tuata                                   *
      *                      *-----------------------------------------*
           if        rr-cau-mag-cod
                    (w-acc-ser-edd-c01)   not  = zero
                     move  1              to   rr-cau-mag-sns         .
      *                  *---------------------------------------------*
      *                  * Normalizzazione function key                *
      *                  *---------------------------------------------*
           move      spaces               to   v-key                  .
       acc-cau-mag-320.
      *                  *---------------------------------------------*
      *                  * Normalizzazione singolo codice causale      *
      *                  *---------------------------------------------*
           move      zero                 to   rr-cau-mag             .
      *                  *---------------------------------------------*
      *                  * Preparazione visualizzazione elementi sele- *
      *                  * zionati                                     *
      *                  *---------------------------------------------*
           perform   pcs-zcc-esl-000      thru pcs-zcc-esl-999        .
      *                  *---------------------------------------------*
      *                  * Visualizzazione codici selezionati          *
      *                  *---------------------------------------------*
           perform   vcs-zcc-esl-000      thru vcs-zcc-esl-999        .
      *                  *---------------------------------------------*
      *                  * Proseguimento                               *
      *                  *---------------------------------------------*
           go to     acc-cau-mag-600.
       acc-cau-mag-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione della selezione ef-  *
      *                  * fettuata eventualmente                      *
      *                  *---------------------------------------------*
           if        rr-cau-mag-sns       =    zero
                     go to acc-cau-mag-440.
      *                  *---------------------------------------------*
      *                  * A preparazione visualizzazione elementi     *
      *                  * selezionati                                 *
      *                  *---------------------------------------------*
           go to     acc-cau-mag-320.
       acc-cau-mag-440.
      *                  *---------------------------------------------*
      *                  * Lettura archivio [zmc]                      *
      *                  *---------------------------------------------*
           move      rr-cau-mag           to   w-let-arc-zmc-cod      .
           perform   let-arc-zmc-000      thru let-arc-zmc-999        .
      *                  *---------------------------------------------*
      *                  * Trattamento descrizione                     *
      *                  *---------------------------------------------*
           if        rr-cau-mag           not  = zero
                     move  w-let-arc-zmc-des
                                          to   rr-cau-mag-des
           else      move  "Tutte"        to   rr-cau-mag-des         .
           perform   vis-cau-mag-des-000  thru vis-cau-mag-des-999    .
      *                  *---------------------------------------------*
      *                  * Se record non trovato : reimpostazione      *
      *                  *---------------------------------------------*
           if        w-let-arc-zmc-flg    not  = spaces
                     go to acc-cau-mag-100.
       acc-cau-mag-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cau-mag-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-cau-mag-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-cau-mag-100.
       acc-cau-mag-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo selezione : Causale di magazzino    *
      *    *-----------------------------------------------------------*
       vis-cau-mag-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      13                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-cau-mag           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cau-mag-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo selezione : Descrizione causale di  *
      *    * magazzino                                                 *
      *    *-----------------------------------------------------------*
       vis-cau-mag-des-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      40                   to   v-pos                  .
           move      rr-cau-mag-des       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cau-mag-des-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Serie elementi da selezionare              *
      *    *-----------------------------------------------------------*
       acc-zcc-esl-000.
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
           move      "                       Codici causale da seleziona
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
           perform   acc-zcc-esl-900      thru acc-zcc-esl-909        .
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
           go to     acc-zcc-esl-100.
       acc-zcc-esl-080.
      *              *-------------------------------------------------*
      *              * Visualizzazione pagina se necessario            *
      *              *-------------------------------------------------*
       acc-zcc-esl-082.
           move      w-acc-ser-edd-pev    to   w-acc-ser-edd-c0a      .
           move      w-acc-ser-edd-c0a    to   w-acc-ser-edd-c0b      .
           add       11                   to   w-acc-ser-edd-c0b      .
           if        w-acc-ser-edd-nel    <    w-acc-ser-edd-c0a or
                     w-acc-ser-edd-nel    >    w-acc-ser-edd-c0b
                     go to acc-zcc-esl-084
           else      go to acc-zcc-esl-086.
       acc-zcc-esl-084.
           move      w-acc-ser-edd-nel    to   w-acc-ser-edd-n1v      .
           subtract  1                    from w-acc-ser-edd-n1v      .
           divide    12                   into w-acc-ser-edd-n1v      .
           multiply  12                   by   w-acc-ser-edd-n1v      .
           add       1                    to   w-acc-ser-edd-n1v      .
           move      w-acc-ser-edd-n1v    to   w-acc-ser-edd-pev      .
           perform   acc-zcc-esl-900      thru acc-zcc-esl-909        .
       acc-zcc-esl-086.
           go to     acc-zcc-esl-100.
       acc-zcc-esl-100.
      *              *-------------------------------------------------*
      *              * Visualizzazione elementi selezionati            *
      *              *-------------------------------------------------*
           perform   acc-zcc-esl-950      thru acc-zcc-esl-959        .
      *              *-------------------------------------------------*
      *              * Accettazione linea                              *
      *              *-------------------------------------------------*
       acc-zcc-esl-120.
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
           if        rr-cau-mag-els       >    24
                     move  3              to   w-acc-ser-edd-lt2
           else if   rr-cau-mag-els       >    12
                     move  2              to   w-acc-ser-edd-lt2
           else      move  1              to   w-acc-ser-edd-lt2      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione                             *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      13                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      w-acc-ser-edd-ltp    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       acc-zcc-esl-150.
      *                  *---------------------------------------------*
      *                  * Salvataggio valori precedenti linea         *
      *                  *---------------------------------------------*
           move      rr-cau-mag-cod
                    (w-acc-ser-edd-nel)   to   w-acc-ser-edd-spe      .
       acc-zcc-esl-200.
      *                  *---------------------------------------------*
      *                  * Accettazione codice elemento                *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione parametri                  *
      *                      *-----------------------------------------*
           move      "AC"                 to   w-cod-mne-zmc-ope      .
           move      rr-cau-mag-cod
                    (w-acc-ser-edd-nel)   to   w-cod-mne-zmc-cod      .
           move      "<B"                 to   v-edm                  .
      *                          *-------------------------------------*
      *                          * Coordinate di posizionamento        *
      *                          *-------------------------------------*
           move      w-acc-ser-edd-nel    to   w-acc-ser-edd-c0r      .
           subtract  w-acc-ser-edd-pev    from w-acc-ser-edd-c0r      .
           add       7                    to   w-acc-ser-edd-c0r      .
           move      w-acc-ser-edd-c0r    to   w-cod-mne-zmc-lin      .
           move      09                   to   w-cod-mne-zmc-pos      .
           move      w-cod-mne-zmc-lin    to   w-cod-mne-zmc-dln      .
           move      w-cod-mne-zmc-pos    to   w-cod-mne-zmc-dps      .
           add       08                   to   w-cod-mne-zmc-dps      .
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
           if        rr-cau-mag-cod
                    (w-acc-ser-edd-nel)   not  = zero
                     go to acc-zcc-esl-202.
           move      w-acc-ser-edd-nel    to   w-acc-ser-edd-c0a      .
           add       1                    to   w-acc-ser-edd-c0a      .
           if        w-acc-ser-edd-c0a    >    w-acc-ser-edd-max
                     go to acc-zcc-esl-204.
           if        rr-cau-mag-cod
                    (w-acc-ser-edd-c0a)   =    zero
                     go to acc-zcc-esl-204.
       acc-zcc-esl-202.
           move      "REMV"               to   v-pfk (06)             .
       acc-zcc-esl-204.
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
                     go to acc-zcc-esl-206.
           move      w-acc-ser-edd-nel    to   w-acc-ser-edd-c0a      .
           add       1                    to   w-acc-ser-edd-c0a      .
           if        rr-cau-mag-cod
                    (w-acc-ser-edd-nel)   =    zero and
                     rr-cau-mag-cod
                    (w-acc-ser-edd-c0a)   =    zero
                     go to acc-zcc-esl-206.
      *                          *-------------------------------------*
      *                          * Tasto 'Tab'                         *
      *                          *-------------------------------------*
           move      "TAB "               to   v-pfk (10)             .
       acc-zcc-esl-206.
      *                      *-----------------------------------------*
      *                      * Accettazione                            *
      *                      *-----------------------------------------*
           perform   cod-mne-zmc-cll-000  thru cod-mne-zmc-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-mne-zmc-foi-000  thru cod-mne-zmc-foi-999    .
       acc-zcc-esl-208.
           perform   cod-mne-zmc-cll-000  thru cod-mne-zmc-cll-999    .
           if        w-cod-mne-zmc-ope    =    "F+"
                     go to acc-zcc-esl-210.
           if        w-cod-mne-zmc-ope    =    "AC"
                     go to acc-zcc-esl-212.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-zcc-esl-210.
           perform   cod-mne-zmc-foi-000  thru cod-mne-zmc-foi-999    .
           go to     acc-zcc-esl-208.
       acc-zcc-esl-212.
           move      w-cod-mne-zmc-cod    to   v-num                  .
       acc-zcc-esl-215.
      *                      *-----------------------------------------*
      *                      * Valore in campo di destinazione         *
      *                      *-----------------------------------------*
           move      v-num                to   rr-cau-mag-cod
                                              (w-acc-ser-edd-nel)     .
      *                      *-----------------------------------------*
      *                      * Se Exit                                 *
      *                      *-----------------------------------------*
           if        v-key                =    "EXIT"
                     move  spaces         to   v-key
                     go to acc-zcc-esl-800.
      *                      *-----------------------------------------*
      *                      * Se Return                               *
      *                      *-----------------------------------------*
           if        v-key                =    spaces
                     go to acc-zcc-esl-425.
       acc-zcc-esl-220.
      *                      *-----------------------------------------*
      *                      * Se Up                                   *
      *                      *-----------------------------------------*
           if        v-key                not  = "UP  "
                     go to acc-zcc-esl-225.
      *                          *-------------------------------------*
      *                          * Compattamento se necessario         *
      *                          *-------------------------------------*
           move      w-acc-ser-edd-nel    to   w-acc-ser-edd-nec      .
           perform   acc-zcc-esl-920      thru acc-zcc-esl-929        .
      *                          *-------------------------------------*
      *                          * Se sul primo : uscita               *
      *                          *-------------------------------------*
           subtract  1                    from w-acc-ser-edd-nel      .
           if        w-acc-ser-edd-nel    =    zero
                     move  "UP  "         to   v-key
                     go to acc-zcc-esl-800
           else      go to acc-zcc-esl-080.
       acc-zcc-esl-225.
      *                      *-----------------------------------------*
      *                      * Se Down                                 *
      *                      *-----------------------------------------*
           if        v-key                not  = "DOWN"
                     go to acc-zcc-esl-250.
      *                          *-------------------------------------*
      *                          * Compattamento se necessario         *
      *                          *-------------------------------------*
           move      w-acc-ser-edd-nel    to   w-acc-ser-edd-nec      .
           perform   acc-zcc-esl-920      thru acc-zcc-esl-929        .
      *                          *-------------------------------------*
      *                          * Se sull'ultimo : uscita             *
      *                          *-------------------------------------*
           if        w-acc-ser-edd-fce    =    spaces
                     add   1              to   w-acc-ser-edd-nel      .
           if        w-acc-ser-edd-nel    >    w-acc-ser-edd-max
                     move  "DOWN"         to   v-key
                     go to acc-zcc-esl-800.
           if        w-acc-ser-edd-nel    =    1
                     go to acc-zcc-esl-230
           else      go to acc-zcc-esl-235.
       acc-zcc-esl-230.
           if        rr-cau-mag-cod (1)    =    zero and
                     rr-cau-mag-cod (2)    =    zero
                     move  "DOWN"         to   v-key
                     go to acc-zcc-esl-800
           else      go to acc-zcc-esl-080.
       acc-zcc-esl-235.
           move      w-acc-ser-edd-nel    to   w-acc-ser-edd-c0a      .
           move      w-acc-ser-edd-c0a    to   w-acc-ser-edd-c0b      .
           subtract  1                    from w-acc-ser-edd-c0b      .
           if        rr-cau-mag-cod
                    (w-acc-ser-edd-c0a)   =    zero and
                     rr-cau-mag-cod
                    (w-acc-ser-edd-c0b)   =    zero
                     move  "DOWN"         to   v-key
                     go to acc-zcc-esl-800
           else      go to acc-zcc-esl-080.
       acc-zcc-esl-250.
      *                      *-----------------------------------------*
      *                      * Se Insr                                 *
      *                      *-----------------------------------------*
           if        v-key                not  = "INSR"
                     go to acc-zcc-esl-275.
           move      w-acc-ser-edd-nel    to   w-acc-ser-edd-nec      .
           perform   acc-zcc-esl-940      thru acc-zcc-esl-949        .
           go to     acc-zcc-esl-080.
       acc-zcc-esl-275.
      *                      *-----------------------------------------*
      *                      * Se Do                                   *
      *                      *-----------------------------------------*
           if        v-key                not  = "DO  "
                     go to acc-zcc-esl-300.
      *                          *-------------------------------------*
      *                          * Controlli per tasto Do              *
      *                          *-------------------------------------*
           perform   tdo-ric-sel-000      thru tdo-ric-sel-999        .
           if        w-cnt-tdo-ric-flg    =    spaces
                     go to acc-zcc-esl-280
           else      move  spaces         to   w-cnt-tdo-ric-flg
                     go to acc-zcc-esl-200.
       acc-zcc-esl-280.
      *                          *-------------------------------------*
      *                          * Compattamento se necessario         *
      *                          *-------------------------------------*
           move      w-acc-ser-edd-nel    to   w-acc-ser-edd-nec      .
           perform   acc-zcc-esl-920      thru acc-zcc-esl-929        .
      *                          *-------------------------------------*
      *                          * Uscita                              *
      *                          *-------------------------------------*
           move      "S"                  to   w-cnt-acc-ric-sel      .
           go to     acc-zcc-esl-800.
       acc-zcc-esl-300.
      *                      *-----------------------------------------*
      *                      * Se Remv                                 *
      *                      *-----------------------------------------*
           if        v-key                not  = "REMV"
                     go to acc-zcc-esl-325.
      *                          *-------------------------------------*
      *                          * Compattamento in ogni caso          *
      *                          *-------------------------------------*
           move      w-acc-ser-edd-nel    to   w-acc-ser-edd-nec      .
           perform   acc-zcc-esl-930      thru acc-zcc-esl-939        .
      *                          *-------------------------------------*
      *                          * A reimpostare                       *
      *                          *-------------------------------------*
           go to     acc-zcc-esl-080.
       acc-zcc-esl-325.
      *                      *-----------------------------------------*
      *                      * Se Prsc                                 *
      *                      *-----------------------------------------*
           if        v-key                not  = "PRSC"
                     go to acc-zcc-esl-350.
      *                          *-------------------------------------*
      *                          * Compattamento se necessario         *
      *                          *-------------------------------------*
           move      w-acc-ser-edd-nel    to   w-acc-ser-edd-nec      .
           perform   acc-zcc-esl-920      thru acc-zcc-esl-929        .
      *                          *-------------------------------------*
      *                          * Se su prima facciata : uscita       *
      *                          *-------------------------------------*
           if        w-acc-ser-edd-nel    not  > 14
                     move  "UP  "         to   v-key
                     go to acc-zcc-esl-800.
      *                          *-------------------------------------*
      *                          * Al primo elemento della facciata    *
      *                          * precedente                          *
      *                          *-------------------------------------*
           subtract  1                    from w-acc-ser-edd-nel      .
           divide    14                   into w-acc-ser-edd-nel      .
           subtract  1                    from w-acc-ser-edd-nel      .
           multiply  14                   by   w-acc-ser-edd-nel      .
           add       1                    to   w-acc-ser-edd-nel      .
           go to     acc-zcc-esl-080.
       acc-zcc-esl-350.
      *                      *-----------------------------------------*
      *                      * Se Nxsc                                 *
      *                      *-----------------------------------------*
           if        v-key                not  = "NXSC"
                     go to acc-zcc-esl-375.
      *                          *-------------------------------------*
      *                          * Compattamento se necessario         *
      *                          *-------------------------------------*
           move      w-acc-ser-edd-nel    to   w-acc-ser-edd-nec      .
           perform   acc-zcc-esl-920      thru acc-zcc-esl-929        .
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
                     go to acc-zcc-esl-800.
           if        rr-cau-mag-cod
                    (w-acc-ser-edd-c0a)   =    zero
                     move  "DOWN"         to   v-key
                     go to acc-zcc-esl-800.
      *                          *-------------------------------------*
      *                          * Al primo elemento della facciata    *
      *                          * successiva                          *
      *                          *-------------------------------------*
           move      w-acc-ser-edd-c0a    to   w-acc-ser-edd-nel      .
           go to     acc-zcc-esl-080.
       acc-zcc-esl-375.
      *                      *-----------------------------------------*
      *                      * Se Back                                 *
      *                      *-----------------------------------------*
           if        v-key                not  = "BACK"
                     go to acc-zcc-esl-400.
      *                          *-------------------------------------*
      *                          * Compattamento se necessario         *
      *                          *-------------------------------------*
           move      w-acc-ser-edd-nel    to   w-acc-ser-edd-nec      .
           perform   acc-zcc-esl-920      thru acc-zcc-esl-929        .
      *                          *-------------------------------------*
      *                          * Al primo elemento                   *
      *                          *-------------------------------------*
           move      1                    to   w-acc-ser-edd-nel      .
           go to     acc-zcc-esl-080.
       acc-zcc-esl-400.
      *                      *-----------------------------------------*
      *                      * Se Tab                                  *
      *                      *-----------------------------------------*
           if        v-key                not  = "TAB "
                     go to acc-zcc-esl-425.
      *                          *-------------------------------------*
      *                          * Compattamento se necessario         *
      *                          *-------------------------------------*
           move      w-acc-ser-edd-nel    to   w-acc-ser-edd-nec      .
           perform   acc-zcc-esl-920      thru acc-zcc-esl-929        .
      *                          *-------------------------------------*
      *                          * Dopo l'ultimo elemento inserito     *
      *                          *-------------------------------------*
           if        rr-cau-mag-cod
                    (w-acc-ser-edd-max)   not  = zero
                     move  w-acc-ser-edd-max
                                          to   w-acc-ser-edd-nel
                     go to acc-zcc-esl-080.
           move      w-acc-ser-edd-max    to   w-acc-ser-edd-nel      .
       acc-zcc-esl-405.
           if        w-acc-ser-edd-nel    =    zero
                     go to acc-zcc-esl-410.
           if        rr-cau-mag-cod
                    (w-acc-ser-edd-nel)   =    zero
                     subtract  1          from w-acc-ser-edd-nel
                     go to     acc-zcc-esl-405.
       acc-zcc-esl-410.
           add       1                    to   w-acc-ser-edd-nel      .
           go to     acc-zcc-esl-080.
       acc-zcc-esl-425.
      *                      *-----------------------------------------*
      *                      * Se Return                               *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Lettura archivio [zmc]              *
      *                          *-------------------------------------*
           move      rr-cau-mag-cod
                    (w-acc-ser-edd-nel)   to   w-let-arc-zmc-cod      .
           perform   let-arc-zmc-000      thru let-arc-zmc-999        .
      *                          *-------------------------------------*
      *                          * Trattamento descrizione             *
      *                          *-------------------------------------*
           move      w-let-arc-zmc-des    to   rr-cau-mag-dsc
                                              (w-acc-ser-edd-nel)     .
      *                          *-------------------------------------*
      *                          * Trattamento mnemonico               *
      *                          *-------------------------------------*
           move      w-let-arc-zmc-mne    to   rr-cau-mag-mne
                                              (w-acc-ser-edd-nel)     .
      *                          *-------------------------------------*
      *                          * Visualizzazione                     *
      *                          *-------------------------------------*
           move      w-acc-ser-edd-nel    to   w-acc-ser-edd-nev      .
           move      w-acc-ser-edd-nel    to   w-acc-ser-edd-c0r      .
           subtract  w-acc-ser-edd-pev    from w-acc-ser-edd-c0r      .
           add       7                    to   w-acc-ser-edd-c0r      .
           perform   acc-zcc-esl-910      thru acc-zcc-esl-919        .
      *                          *-------------------------------------*
      *                          * Se record non trovato : reimposta-  *
      *                          * zione                               *
      *                          *-------------------------------------*
           if        w-let-arc-zmc-flg    not  = spaces
                     go to acc-zcc-esl-200.
      *                          *-------------------------------------*
      *                          * Controllo valore impostato          *
      *                          *-------------------------------------*
           if        rr-cau-mag-cod
                    (w-acc-ser-edd-nel)   =    zero
                     go to acc-zcc-esl-450
           else      go to acc-zcc-esl-500.
       acc-zcc-esl-450.
      *                          *-------------------------------------*
      *                          * Se impostato zero                   *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Se il valore precedente era a   *
      *                              * zero : come per Down            *
      *                              *---------------------------------*
           if        w-acc-ser-edd-spe    =    zero
                     move  "DOWN"         to   v-key
                     go to acc-zcc-esl-225.
      *                              *---------------------------------*
      *                              * Altrimenti come per Remv        *
      *                              *---------------------------------*
           move      "REMV"               to   v-key                  .
           go to     acc-zcc-esl-300.
       acc-zcc-esl-500.
      *                  *---------------------------------------------*
      *                  * Passaggio ad elemento successivo            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Visualizzazione elementi selezionati    *
      *                      *-----------------------------------------*
           perform   acc-zcc-esl-960      thru acc-zcc-esl-969        .
      *                      *-----------------------------------------*
      *                      * Incremento numero elemento              *
      *                      *-----------------------------------------*
           add       1                    to   w-acc-ser-edd-nel      .
      *                      *-----------------------------------------*
      *                      * Se fine elementi : uscita               *
      *                      *-----------------------------------------*
           if        w-acc-ser-edd-nel    >    w-acc-ser-edd-max
                     move  spaces         to   v-key
                     go to acc-zcc-esl-800.
      *                      *-----------------------------------------*
      *                      * Altrimenti : ad impostazione linea      *
      *                      *-----------------------------------------*
           go to     acc-zcc-esl-080.
       acc-zcc-esl-800.
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
           go to     acc-zcc-esl-999.
       acc-zcc-esl-890.
      *              *-------------------------------------------------*
      *              * Subroutines interne                             *
      *              *-------------------------------------------------*
       acc-zcc-esl-900.
      *                  *---------------------------------------------*
      *                  * Visualizzazione pagina a partire dall'ele-  *
      *                  * mento numero w-acc-ser-edd-n1v              *
      *                  *---------------------------------------------*
           move      w-acc-ser-edd-n1v    to   w-acc-ser-edd-c0p      .
           subtract  1                    from w-acc-ser-edd-c0p      .
           move      w-acc-ser-edd-c0p    to   w-acc-ser-edd-c0q      .
           add       12                   to   w-acc-ser-edd-c0q      .
       acc-zcc-esl-901.
           add       1                    to   w-acc-ser-edd-c0p      .
           if        w-acc-ser-edd-c0p    >    w-acc-ser-edd-max
                     go to acc-zcc-esl-905.
           if        w-acc-ser-edd-c0p    >    w-acc-ser-edd-c0q
                     go to acc-zcc-esl-909.
           move      w-acc-ser-edd-c0p    to   w-acc-ser-edd-nev      .
           move      w-acc-ser-edd-nev    to   w-acc-ser-edd-c0r      .
       acc-zcc-esl-903.
           if        w-acc-ser-edd-c0r    >    12
                     subtract  12         from w-acc-ser-edd-c0r
                     go to     acc-zcc-esl-903.
           add       6                    to   w-acc-ser-edd-c0r      .
           perform   acc-zcc-esl-910      thru acc-zcc-esl-919        .
           go to     acc-zcc-esl-901.
       acc-zcc-esl-905.
           if        w-acc-ser-edd-c0p    >    w-acc-ser-edd-c0q
                     go to acc-zcc-esl-909.
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
           go to     acc-zcc-esl-905.
       acc-zcc-esl-909.
           exit.
       acc-zcc-esl-910.
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
      *                          * Editing codice causale              *
      *                          *-------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "B"                  to   v-edm                  .
           move      rr-cau-mag-cod
                    (w-acc-ser-edd-nev)   to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-acc-ser-edd-cod      .
      *                          *-------------------------------------*
      *                          * Descrizione                         *
      *                          *-------------------------------------*
           move      rr-cau-mag-dsc
                    (w-acc-ser-edd-nev)   to   w-acc-ser-edd-des      .
      *                          *-------------------------------------*
      *                          * Mnemonico                           *
      *                          *-------------------------------------*
           move      rr-cau-mag-mne
                    (w-acc-ser-edd-nev)   to   w-acc-ser-edd-mne      .
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
       acc-zcc-esl-919.
           exit.
       acc-zcc-esl-920.
      *                  *---------------------------------------------*
      *                  * Compattamento dell' elemento indirizzato da *
      *                  * w-acc-ser-edd-nec se necessario             *
      *                  *---------------------------------------------*
           move      spaces               to   w-acc-ser-edd-fce      .
           if        rr-cau-mag-cod
                    (w-acc-ser-edd-nec)   not  = zero
                     go to acc-zcc-esl-929.
           move      w-acc-ser-edd-nec    to   w-acc-ser-edd-c0a      .
           move      w-acc-ser-edd-c0a    to   w-acc-ser-edd-c0b      .
           add       1                    to   w-acc-ser-edd-c0b      .
           if        w-acc-ser-edd-c0b    >    w-acc-ser-edd-max
                     go to acc-zcc-esl-929.
           if        rr-cau-mag-cod
                    (w-acc-ser-edd-c0b)   =    zero
                     go to acc-zcc-esl-929.
           perform   acc-zcc-esl-930      thru acc-zcc-esl-939        .
           move      "#"                  to   w-acc-ser-edd-fce      .
       acc-zcc-esl-929.
           exit.
       acc-zcc-esl-930.
      *                  *---------------------------------------------*
      *                  * Compattamento dell' elemento indirizzato da *
      *                  * w-acc-ser-edd-nec in ogni caso              *
      *                  *---------------------------------------------*
           move      w-acc-ser-edd-nec    to   w-acc-ser-edd-c0a      .
           move      w-acc-ser-edd-c0a    to   w-acc-ser-edd-c0b      .
           add       1                    to   w-acc-ser-edd-c0b      .
       acc-zcc-esl-931.
           if        w-acc-ser-edd-c0b    >    w-acc-ser-edd-max
                     go to acc-zcc-esl-932.
           move      rr-cau-mag-ele
                    (w-acc-ser-edd-c0b)   to   rr-cau-mag-ele
                                              (w-acc-ser-edd-c0a)     .
           if        rr-cau-mag-cod
                    (w-acc-ser-edd-c0b)   =    zero
                     go to acc-zcc-esl-933.
           add       1                    to   w-acc-ser-edd-c0a      .
           add       1                    to   w-acc-ser-edd-c0b      .
           go to     acc-zcc-esl-931.
       acc-zcc-esl-932.
           move      zero                 to   rr-cau-mag-cod
                                              (w-acc-ser-edd-c0a)     .
           move      spaces               to   rr-cau-mag-dsc
                                              (w-acc-ser-edd-c0a)     .
           move      spaces               to   rr-cau-mag-mne
                                              (w-acc-ser-edd-c0a)     .
       acc-zcc-esl-933.
           move      w-acc-ser-edd-nec    to   w-acc-ser-edd-c0a      .
       acc-zcc-esl-934.
           if        w-acc-ser-edd-c0a    >    12
                     subtract  12         from w-acc-ser-edd-c0a
                     go to     acc-zcc-esl-934.
           move      w-acc-ser-edd-c0a    to   w-acc-ser-edd-c0b      .
           add       6                    to   w-acc-ser-edd-c0b      .
           move      w-acc-ser-edd-c0b    to   w-acc-ser-edd-c0c      .
           add       1                    to   w-acc-ser-edd-c0c      .
       acc-zcc-esl-935.
           if        w-acc-ser-edd-c0a    =    12
                     go to acc-zcc-esl-936.
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
           go to     acc-zcc-esl-935.
       acc-zcc-esl-936.
           move      w-acc-ser-edd-nec    to   w-acc-ser-edd-c0a      .
       acc-zcc-esl-937.
           if        w-acc-ser-edd-c0a    >    12
                     subtract  12         from w-acc-ser-edd-c0a
                     go to     acc-zcc-esl-937.
           subtract  w-acc-ser-edd-c0a    from 12
                                        giving w-acc-ser-edd-c0b      .
           add       w-acc-ser-edd-nec
                     w-acc-ser-edd-c0b  giving w-acc-ser-edd-c0c      .
           if        w-acc-ser-edd-c0c    >    w-acc-ser-edd-max
                     go to acc-zcc-esl-938.
           if        rr-cau-mag-cod
                    (w-acc-ser-edd-c0c)   =    zero
                     go to acc-zcc-esl-938.
           move      w-acc-ser-edd-c0c    to   w-acc-ser-edd-nev      .
           move      18                   to   w-acc-ser-edd-c0r      .
           perform   acc-zcc-esl-910      thru acc-zcc-esl-919        .
           go to     acc-zcc-esl-939.
       acc-zcc-esl-938.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      70                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      09                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-zcc-esl-939.
           exit.
       acc-zcc-esl-940.
      *                  *---------------------------------------------*
      *                  * Inserimento dell' elemento indirizzato da   *
      *                  * w-acc-ser-edd-nec                           *
      *                  *---------------------------------------------*
           move      w-acc-ser-edd-max    to   w-acc-ser-edd-c0b      .
           move      w-acc-ser-edd-c0b    to   w-acc-ser-edd-c0a      .
           subtract  1                    from w-acc-ser-edd-c0a      .
       acc-zcc-esl-941.
           move      rr-cau-mag-ele
                    (w-acc-ser-edd-c0a)   to   rr-cau-mag-ele
                                              (w-acc-ser-edd-c0b)     .
           if        w-acc-ser-edd-c0a    =    w-acc-ser-edd-nec
                     go to acc-zcc-esl-942.
           subtract  1                    from w-acc-ser-edd-c0a      .
           subtract  1                    from w-acc-ser-edd-c0b      .
           go to     acc-zcc-esl-941.
       acc-zcc-esl-942.
           move      zero                 to   rr-cau-mag-cod
                                              (w-acc-ser-edd-c0a)     .
           move      spaces               to   rr-cau-mag-dsc
                                              (w-acc-ser-edd-c0a)     .
           move      spaces               to   rr-cau-mag-mne
                                              (w-acc-ser-edd-c0a)     .
           move      w-acc-ser-edd-nec    to   w-acc-ser-edd-c0a      .
       acc-zcc-esl-943.
           if        w-acc-ser-edd-c0a    >    12
                     subtract  12         from w-acc-ser-edd-c0a
                     go to     acc-zcc-esl-943.
           add       6                    to   w-acc-ser-edd-c0a      .
           if        w-acc-ser-edd-c0a    =    18
                     go to acc-zcc-esl-945.
           move      17                   to   w-acc-ser-edd-c0b      .
           move      18                   to   w-acc-ser-edd-c0c      .
       acc-zcc-esl-944.
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
                     go to acc-zcc-esl-945.
           subtract  1                    from w-acc-ser-edd-c0b      .
           subtract  1                    from w-acc-ser-edd-c0c      .
           go to     acc-zcc-esl-944.
       acc-zcc-esl-945.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      70                   to   v-car                  .
           move      w-acc-ser-edd-c0a    to   v-lin                  .
           move      09                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-zcc-esl-949.
           exit.
       acc-zcc-esl-950.
      *                  *---------------------------------------------*
      *                  * Visualizzazione elementi selezionati        *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      20                   to   v-lin                  .
           move      76                   to   v-pos                  .
           move      rr-cau-mag-els       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-zcc-esl-959.
           exit.
       acc-zcc-esl-960.
      *                  *---------------------------------------------*
      *                  * Visualizzazione elementi da trattare        *
      *                  *---------------------------------------------*
           move      zero                 to   w-acc-ser-edd-c0a      .
       acc-zcc-esl-962.
           add       1                    to   w-acc-ser-edd-c0a      .
           if        w-acc-ser-edd-c0a    >    w-acc-ser-edd-max
                     go to acc-zcc-esl-964.
           if        rr-cau-mag-cod
                    (w-acc-ser-edd-c0a)   =    zero
                     go to acc-zcc-esl-964.
           go to     acc-zcc-esl-962.
       acc-zcc-esl-964.
           subtract  1                    from w-acc-ser-edd-c0a      .
           move      w-acc-ser-edd-c0a    to   rr-cau-mag-els         .
      *              *-------------------------------------------------*
      *              * Visualizzazione elementi selezionati            *
      *              *-------------------------------------------------*
           perform   acc-zcc-esl-950      thru acc-zcc-esl-959        .
       acc-zcc-esl-969.
           exit.
       acc-zcc-esl-999.
           exit.

      *    *===========================================================*
      *    * Preparazione codici selezionati                           *
      *    *-----------------------------------------------------------*
       pcs-zcc-esl-000.
      *              *-------------------------------------------------*
      *              * Preparazione di un campo text di massimo 3 li-  *
      *              * nee da 40 caratteri ciascuna                    *
      *              *-------------------------------------------------*
           move      spaces               to   w-acc-ser-edd-txt      .
      *              *-------------------------------------------------*
      *              * Test se da eseguire                             *
      *              *-------------------------------------------------*
           if        rr-cau-mag-sns       =    zero
                     go to pcs-zcc-esl-900.
           move      zero                 to   w-acc-ser-edd-c01      .
       pcs-zcc-esl-100.
      *              *-------------------------------------------------*
      *              * Ciclo di preparazione                           *
      *              *-------------------------------------------------*
           if        w-acc-ser-edd-txt
                    (119:1)               not  = spaces
                     go to pcs-zcc-esl-900.
           add       1                    to   w-acc-ser-edd-c01      .
           if        w-acc-ser-edd-c01    >    w-acc-ser-edd-max
                     go to pcs-zcc-esl-900.
           if        rr-cau-mag-cod
                    (w-acc-ser-edd-c01)   =    zero
                     go to pcs-zcc-esl-900.
      *              *-------------------------------------------------*
      *              * Editing codice causale                          *
      *              *-------------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      rr-cau-mag-cod
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
       pcs-zcc-esl-800.
      *              *-------------------------------------------------*
      *              * Riciclo                                         *
      *              *-------------------------------------------------*
           go to     pcs-zcc-esl-100.
       pcs-zcc-esl-900.
      *              *-------------------------------------------------*
      *              * Valore in uscita                                *
      *              *-------------------------------------------------*
           if        w-acc-ser-edd-txt    =    spaces
                     go to pcs-zcc-esl-999.
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
           go to     pcs-zcc-esl-999.
       pcs-zcc-esl-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione codici selezionati                        *
      *    *-----------------------------------------------------------*
       vcs-zcc-esl-000.
      *              *-------------------------------------------------*
      *              * Preparazione contatore                          *
      *              *-------------------------------------------------*
           move      zero                 to   w-acc-ser-edd-c01      .
       vcs-zcc-esl-100.
      *              *-------------------------------------------------*
      *              * Ciclo di preparazione                           *
      *              *-------------------------------------------------*
           add       1                    to   w-acc-ser-edd-c01      .
           if        w-acc-ser-edd-c01    >    3
                     go to vcs-zcc-esl-900.
       vcs-zcc-esl-200.
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      12                   to   v-lin                  .
           add       w-acc-ser-edd-c01    to   v-lin                  .
           move      41                   to   v-pos                  .
           move      w-acc-ser-edd-rtr
                    (w-acc-ser-edd-c01)   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vcs-zcc-esl-800.
      *              *-------------------------------------------------*
      *              * Riciclo                                         *
      *              *-------------------------------------------------*
           go to     vcs-zcc-esl-100.
       vcs-zcc-esl-900.
      *              *-------------------------------------------------*
      *              * Pulizia della prima riga e segnale di selezione *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      30                   to   v-pos                  .
           if        w-acc-ser-edd-txt    =    spaces
                     move  "Tutte      "  to   v-alf
           else      move  "+          "  to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     vcs-zcc-esl-999.
       vcs-zcc-esl-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Codice conto merce                   *
      *    *-----------------------------------------------------------*
       acc-cod-mic-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        rr-tip-stp           not  = 01
                     go to acc-cod-mic-999.
       acc-cod-mic-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-des-zmm-ope      .
           move      rr-cod-mic           to   w-cod-des-zmm-cod      .
           move      14                   to   w-cod-des-zmm-lin      .
           move      30                   to   w-cod-des-zmm-pos      .
           move      14                   to   w-cod-des-zmm-dln      .
           move      41                   to   w-cod-des-zmm-dps      .
           move      spaces               to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-des-zmm-cll-000  thru cod-des-zmm-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-des-zmm-foi-000  thru cod-des-zmm-foi-999    .
       acc-cod-mic-110.
           perform   cod-des-zmm-cll-000  thru cod-des-zmm-cll-999    .
           if        w-cod-des-zmm-ope    =    "F+"
                     go to acc-cod-mic-115.
           if        w-cod-des-zmm-ope    =    "AC"
                     go to acc-cod-mic-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-mic-115.
           perform   cod-des-zmm-foi-000  thru cod-des-zmm-foi-999    .
           go to     acc-cod-mic-110.
       acc-cod-mic-120.
           move      w-cod-des-zmm-cod    to   v-alf                  .
       acc-cod-mic-150.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-cod-mic-999.
       acc-cod-mic-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   rr-cod-mic             .
       acc-cod-mic-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura archivio [zmm]                      *
      *                  *---------------------------------------------*
           move      rr-cod-mic           to   w-let-arc-zmm-cod      .
           perform   let-arc-zmm-000      thru let-arc-zmm-999        .
      *                   *--------------------------------------------*
      *                   * Bufferizzazione descrizione                *
      *                   *--------------------------------------------*
           if        rr-cod-mic           =    spaces
                     move  "Tutti"        to   rr-cod-mic-des
           else      move  w-let-arc-zmm-des
                                          to   rr-cod-mic-des         .
      *                   *--------------------------------------------*
      *                   * Visualizzazione descrizione                *
      *                   *--------------------------------------------*
           perform   vis-cod-mic-des-000  thru vis-cod-mic-des-999    .
      *                  *---------------------------------------------*
      *                  * Se lettura errata : reimpostazione          *
      *                  *---------------------------------------------*
           if        w-let-arc-zmm-flg    not  = spaces
                     go to acc-cod-mic-100.
       acc-cod-mic-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cod-mic-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-cod-mic-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-cod-mic-100.
       acc-cod-mic-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Codice conto merce                *
      *    *-----------------------------------------------------------*
       vis-cod-mic-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-cod-mic           to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-mic-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Descrizione conto merce           *
      *    *-----------------------------------------------------------*
       vis-cod-mic-des-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      rr-cod-mic-des       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-mic-des-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Codice archivio                      *
      *    *-----------------------------------------------------------*
       acc-cod-arc-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        rr-tip-stp           not  = 01
                     go to acc-cod-arc-999.
       acc-cod-arc-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      15                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-cod-arc           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-cod-arc-999.
       acc-cod-arc-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-cod-arc             .
       acc-cod-arc-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-cod-arc-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cod-arc-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-cod-arc-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-cod-arc-100.
       acc-cod-arc-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Codice archivio                   *
      *    *-----------------------------------------------------------*
       vis-cod-arc-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      15                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-cod-arc           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-arc-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo selezione : Numero protocollo min      *
      *    *-----------------------------------------------------------*
       acc-npt-min-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        rr-tip-stp           not  = 01
                     go to acc-npt-min-999.
       acc-npt-min-100.
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
                     go to acc-npt-min-999.
       acc-npt-min-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-num-min             .
       acc-npt-min-300.
      *              *-------------------------------------------------*
      *              * Se Find                                         *
      *              *-------------------------------------------------*
           if        v-key                not  = "FIND"
                     go to acc-npt-min-400.
      *                  *---------------------------------------------*
      *                  * Find su magazzino                           *
      *                  *---------------------------------------------*
           perform   fnd-arc-mmt-000      thru fnd-arc-mmt-999        .
      *                  *---------------------------------------------*
      *                  * A reimpostazione                            *
      *                  *---------------------------------------------*
           go to     acc-npt-min-100.
       acc-npt-min-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-npt-min-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-npt-min-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-npt-min-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-npt-min-100.
       acc-npt-min-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Numero protocollo min             *
      *    *-----------------------------------------------------------*
       vis-npt-min-000.
           move      "DS"                 to   v-ope                  .
           move      "P"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      "<B"                 to   v-edm                  .
           move      16                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-num-min           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-npt-min-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo selezione : Numero protocollo max      *
      *    *-----------------------------------------------------------*
       acc-npt-max-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        rr-tip-stp           not  = 01
                     go to acc-npt-max-999.
       acc-npt-max-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "P"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      "<B"                 to   v-edm                  .
           move      16                   to   v-lin                  .
           move      45                   to   v-pos                  .
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
                     go to acc-npt-max-999.
       acc-npt-max-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-num-max             .
       acc-npt-max-300.
      *              *-------------------------------------------------*
      *              * Se Find                                         *
      *              *-------------------------------------------------*
           if        v-key                not  = "FIND"
                     go to acc-npt-max-400.
      *                  *---------------------------------------------*
      *                  * Find su magazzino                           *
      *                  *---------------------------------------------*
           perform   fnd-arc-mmt-000      thru fnd-arc-mmt-999        .
      *                  *---------------------------------------------*
      *                  * A reimpostazione                            *
      *                  *---------------------------------------------*
           go to     acc-npt-max-100.
       acc-npt-max-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-npt-max-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-npt-max-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-npt-max-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-npt-max-100.
       acc-npt-max-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Numero protocollo min             *
      *    *-----------------------------------------------------------*
       vis-npt-max-000.
           move      "DS"                 to   v-ope                  .
           move      "P"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      "<B"                 to   v-edm                  .
           move      16                   to   v-lin                  .
           move      45                   to   v-pos                  .
           move      rr-num-max           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-npt-max-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo selezione : Codice utente              *
      *    *-----------------------------------------------------------*
       acc-cod-ute-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        rr-tip-stp           not  = 02
                     go to acc-cod-ute-999.
       acc-cod-ute-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "L"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-ric    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      rr-cod-ute           to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-cod-ute-999.
       acc-cod-ute-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   rr-cod-ute             .
       acc-cod-ute-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore a spaces                          *
      *                  *---------------------------------------------*
           if        rr-cod-ute           not  = spaces
                     go to acc-cod-ute-600.
      *                      *-----------------------------------------*
      *                      * Visualizzazione literal 'Tutti'         *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "Tutti"              to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-cod-ute-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cod-ute-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-cod-ute-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-cod-ute-100.
       acc-cod-ute-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo selezione : Codice utente           *
      *    *-----------------------------------------------------------*
       vis-cod-ute-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-cod-ute           to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-ute-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo selezione : Codice fase                *
      *    *-----------------------------------------------------------*
       acc-cod-fas-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        rr-tip-stp           not  = 02
                     go to acc-cod-fas-999.
       acc-cod-fas-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "L"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-ric    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      rr-cod-fas           to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-cod-fas-999.
       acc-cod-fas-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   rr-cod-fas             .
       acc-cod-fas-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore a spaces                          *
      *                  *---------------------------------------------*
           if        rr-cod-fas           not  = spaces
                     go to acc-cod-fas-600.
      *                      *-----------------------------------------*
      *                      * Visualizzazione literal 'Tutti'         *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "Tutti"              to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-cod-fas-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cod-fas-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-cod-fas-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-cod-fas-100.
       acc-cod-fas-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo selezione : Codice fase             *
      *    *-----------------------------------------------------------*
       vis-cod-fas-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-cod-fas           to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-fas-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo chiave : Tipo prodotto                 *
      *    *-----------------------------------------------------------*
       acc-tip-mag-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se la tabella dei tipi magazzino ammessi e' *
      *                  * formata da un solo elemento : si forza il   *
      *                  * valore corrispondente e si esce             *
      *                  *---------------------------------------------*
           if        w-exp-tpm-amm-num    not  = 1
                     go to acc-tip-mag-050.
           move      w-exp-tip-mag-tpm (1)
                                          to   rr-tip-mag             .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     acc-tip-mag-999.
       acc-tip-mag-050.
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      rr-tip-mag           to   w-sav-tip-mag          .
       acc-tip-mag-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tpm-amm-lun    to   v-car                  .
           move      w-exp-tpm-amm-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-exp-tpm-amm-tbl    to   v-txt                  .
           move      18                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      zero                 to   w-exp-tpm-amm-c01      .
       acc-tip-mag-102.
           add       1                    to   w-exp-tpm-amm-c01      .
           if        w-exp-tpm-amm-c01    >    w-exp-tpm-amm-num
                     move  zero           to   v-num
                     go to acc-tip-mag-104.
           if        rr-tip-mag           =    w-exp-tpm-amm-tpm
                                              (w-exp-tpm-amm-c01)
                     move  w-exp-tpm-amm-c01
                                          to   v-num
                     go to acc-tip-mag-104.
           go to     acc-tip-mag-102.
       acc-tip-mag-104.
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-tip-mag-999.
       acc-tip-mag-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-exp-tpm-amm-i01      .
           if        w-exp-tpm-amm-i01    =    zero
                     move  zero           to   rr-tip-mag
           else      move  w-exp-tpm-amm-tpm
                          (w-exp-tpm-amm-i01)
                                          to   rr-tip-mag             .
       acc-tip-mag-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test che il valore sia accettabile          *
      *                  *---------------------------------------------*
           if        rr-tip-mag           =    zero
                     go to acc-tip-mag-100.
       acc-tip-mag-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
           if        rr-tip-mag           =    w-sav-tip-mag
                     go to acc-tip-mag-800.
      *                  *---------------------------------------------*
      *                  * Normalizzazione codice e descrizione filtro *
      *                  * di selezione                                *
      *                  *---------------------------------------------*
           move      zero                 to   rr-fso-mag             .
           move      spaces               to   rr-fso-mag-des         .
      *                  *---------------------------------------------*
      *                  * Visualizzazione codice e descrizione filtro *
      *                  * di selezione                                *
      *                  *---------------------------------------------*
           perform   vis-fso-mag-000      thru vis-fso-mag-999        .
           perform   vis-fso-mag-des-000  thru vis-fso-mag-des-999    .
       acc-tip-mag-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-tip-mag-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-tip-mag-100.
       acc-tip-mag-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo chiave : Tipo prodotto              *
      *    *-----------------------------------------------------------*
       vis-tip-mag-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-mag-lun    to   v-car                  .
           move      w-exp-tip-mag-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-exp-tip-mag-tbl    to   v-txt                  .
           move      18                   to   v-lin                  .
           move      30                   to   v-pos                  .
           if        rr-tip-mag           =    99
                     move  01             to   v-num
           else if   rr-tip-mag           =    01
                     move  02             to   v-num
           else if   rr-tip-mag           =    02
                     move  03             to   v-num
           else if   rr-tip-mag           =    03
                     move  04             to   v-num
           else if   rr-tip-mag           =    04
                     move  05             to   v-num
           else      move  zero           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tip-mag-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo chiave : Codice filtro di selezione    *
      *    *-----------------------------------------------------------*
       acc-fso-mag-000.
      *              *-------------------------------------------------*
      *              * Visualizzazione del prompt in funzione del tipo *
      *              * magazzino                                       *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      01                   to   v-pos                  .
           if        rr-tip-mag           =    01
                     move  "       anagrafica prodotti  "
                                          to   v-alf
           else if   rr-tip-mag           =    02
                     move  "   anagrafica semilavorati  "
                                          to   v-alf
           else if   rr-tip-mag           =    03
                     move  "  anagrafica materie prime  "
                                          to   v-alf
           else if   rr-tip-mag           =    04
                     move  " anagrafica materiali vari  "
                                          to   v-alf
           else      move  spaces         to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo di prodotto     *
      *              *-------------------------------------------------*
           if        rr-tip-mag           =    01
                     go to acc-fso-mag-100
           else if   rr-tip-mag           =    02
                     go to acc-fso-mag-200
           else if   rr-tip-mag           =    03
                     go to acc-fso-mag-300
           else if   rr-tip-mag           =    04
                     go to acc-fso-mag-400
           else      go to acc-fso-mag-999.
       acc-fso-mag-100.
      *                  *---------------------------------------------*
      *                  * Se Prodotto di vendita                      *
      *                  *---------------------------------------------*
           perform   acc-fso-dcp-000      thru acc-fso-dcp-999        .
           go to     acc-fso-mag-999.
       acc-fso-mag-200.
      *                  *---------------------------------------------*
      *                  * Se Semilavorato                             *
      *                  *---------------------------------------------*
           perform   acc-fso-dps-000      thru acc-fso-dps-999        .
           go to     acc-fso-mag-999.
       acc-fso-mag-300.
      *                  *---------------------------------------------*
      *                  * Se Materia prima                            *
      *                  *---------------------------------------------*
           perform   acc-fso-dpm-000      thru acc-fso-dpm-999        .
           go to     acc-fso-mag-999.
       acc-fso-mag-400.
      *                  *---------------------------------------------*
      *                  * Se Materiale vario                          *
      *                  *---------------------------------------------*
           perform   acc-fso-mtv-000      thru acc-fso-mtv-999        .
           go to     acc-fso-mag-999.
       acc-fso-mag-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Codice filtro di selezione [dcp]     *
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
           move      rr-fso-mag           to   w-cod-zos-dcp-cod      .
           move      19                   to   w-cod-zos-dcp-lin      .
           move      30                   to   w-cod-zos-dcp-pos      .
           move      19                   to   w-cod-zos-dcp-dln      .
           move      41                   to   w-cod-zos-dcp-dps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
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
           move      v-num                to   rr-fso-mag             .
       acc-fso-dcp-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura codice filtro selezione e ordina-   *
      *                  * mento per file [dcp]                        *
      *                  *---------------------------------------------*
           move      rr-fso-mag           to   w-let-fso-dcp-cod      .
           perform   let-fso-dcp-000      thru let-fso-dcp-999        .
           move      w-let-fso-dcp-des    to   rr-fso-mag-des         .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione filtro          *
      *                  *---------------------------------------------*
           perform   vis-fso-mag-des-000  thru vis-fso-mag-des-999    .
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
      *    * Accettazione campo : Codice filtro di selezione [dps]     *
      *    *-----------------------------------------------------------*
       acc-fso-dps-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-fso-dps-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-zos-dps-ope      .
           move      rr-fso-mag           to   w-cod-zos-dps-cod      .
           move      19                   to   w-cod-zos-dps-lin      .
           move      30                   to   w-cod-zos-dps-pos      .
           move      19                   to   w-cod-zos-dps-dln      .
           move      41                   to   w-cod-zos-dps-dps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-zos-dps-cll-000  thru cod-zos-dps-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-zos-dps-foi-000  thru cod-zos-dps-foi-999    .
       acc-fso-dps-110.
           perform   cod-zos-dps-cll-000  thru cod-zos-dps-cll-999    .
           if        w-cod-zos-dps-ope    =    "F+"
                     go to acc-fso-dps-115.
           if        w-cod-zos-dps-ope    =    "AC"
                     go to acc-fso-dps-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-fso-dps-115.
           perform   cod-zos-dps-foi-000  thru cod-zos-dps-foi-999    .
           go to     acc-fso-dps-110.
       acc-fso-dps-120.
           move      w-cod-zos-dps-cod    to   v-num                  .
       acc-fso-dps-150.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-fso-dps-999.
       acc-fso-dps-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-fso-mag             .
       acc-fso-dps-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura codice filtro selezione e ordina-   *
      *                  * mento per file [dps]                        *
      *                  *---------------------------------------------*
           move      rr-fso-mag           to   w-let-fso-dps-cod      .
           perform   let-fso-dps-000      thru let-fso-dps-999        .
           move      w-let-fso-dps-des    to   rr-fso-mag-des         .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione filtro          *
      *                  *---------------------------------------------*
           perform   vis-fso-mag-des-000  thru vis-fso-mag-des-999    .
      *                  *---------------------------------------------*
      *                  * Se codice filtro non esistente : a reimpo-  *
      *                  * stazione                                    *
      *                  *---------------------------------------------*
           if        w-let-fso-dps-flg    not  = spaces
                     go to acc-fso-dps-100.
       acc-fso-dps-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-fso-dps-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-fso-dps-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-fso-dps-100.
       acc-fso-dps-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Codice filtro di selezione [dpm]     *
      *    *-----------------------------------------------------------*
       acc-fso-dpm-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-fso-dpm-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-zos-dpm-ope      .
           move      rr-fso-mag           to   w-cod-zos-dpm-cod      .
           move      19                   to   w-cod-zos-dpm-lin      .
           move      30                   to   w-cod-zos-dpm-pos      .
           move      19                   to   w-cod-zos-dpm-dln      .
           move      41                   to   w-cod-zos-dpm-dps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-zos-dpm-cll-000  thru cod-zos-dpm-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-zos-dpm-foi-000  thru cod-zos-dpm-foi-999    .
       acc-fso-dpm-110.
           perform   cod-zos-dpm-cll-000  thru cod-zos-dpm-cll-999    .
           if        w-cod-zos-dpm-ope    =    "F+"
                     go to acc-fso-dpm-115.
           if        w-cod-zos-dpm-ope    =    "AC"
                     go to acc-fso-dpm-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-fso-dpm-115.
           perform   cod-zos-dpm-foi-000  thru cod-zos-dpm-foi-999    .
           go to     acc-fso-dpm-110.
       acc-fso-dpm-120.
           move      w-cod-zos-dpm-cod    to   v-num                  .
       acc-fso-dpm-150.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-fso-dpm-999.
       acc-fso-dpm-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-fso-mag             .
       acc-fso-dpm-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura codice filtro selezione e ordina-   *
      *                  * mento per file [dpm]                        *
      *                  *---------------------------------------------*
           move      rr-fso-mag           to   w-let-fso-dpm-cod      .
           perform   let-fso-dpm-000      thru let-fso-dpm-999        .
           move      w-let-fso-dpm-des    to   rr-fso-mag-des         .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione filtro          *
      *                  *---------------------------------------------*
           perform   vis-fso-mag-des-000  thru vis-fso-mag-des-999    .
      *                  *---------------------------------------------*
      *                  * Se codice filtro non esistente : a reimpo-  *
      *                  * stazione                                    *
      *                  *---------------------------------------------*
           if        w-let-fso-dpm-flg    not  = spaces
                     go to acc-fso-dpm-100.
       acc-fso-dpm-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-fso-dpm-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-fso-dpm-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-fso-dpm-100.
       acc-fso-dpm-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Codice filtro di selezione [mtv]     *
      *    *-----------------------------------------------------------*
       acc-fso-mtv-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-fso-mtv-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-zos-mtv-ope      .
           move      rr-fso-mag           to   w-cod-zos-mtv-cod      .
           move      19                   to   w-cod-zos-mtv-lin      .
           move      30                   to   w-cod-zos-mtv-pos      .
           move      19                   to   w-cod-zos-mtv-dln      .
           move      41                   to   w-cod-zos-mtv-dps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-zos-mtv-cll-000  thru cod-zos-mtv-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-zos-mtv-foi-000  thru cod-zos-mtv-foi-999    .
       acc-fso-mtv-110.
           perform   cod-zos-mtv-cll-000  thru cod-zos-mtv-cll-999    .
           if        w-cod-zos-mtv-ope    =    "F+"
                     go to acc-fso-mtv-115.
           if        w-cod-zos-mtv-ope    =    "AC"
                     go to acc-fso-mtv-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-fso-mtv-115.
           perform   cod-zos-mtv-foi-000  thru cod-zos-mtv-foi-999    .
           go to     acc-fso-mtv-110.
       acc-fso-mtv-120.
           move      w-cod-zos-mtv-cod    to   v-num                  .
       acc-fso-mtv-150.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-fso-mtv-999.
       acc-fso-mtv-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-fso-mag             .
       acc-fso-mtv-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura codice filtro selezione e ordina-   *
      *                  * mento per file [mtv]                        *
      *                  *---------------------------------------------*
           move      rr-fso-mag           to   w-let-fso-mtv-cod      .
           perform   let-fso-mtv-000      thru let-fso-mtv-999        .
           move      w-let-fso-mtv-des    to   rr-fso-mag-des         .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione filtro          *
      *                  *---------------------------------------------*
           perform   vis-fso-mag-des-000  thru vis-fso-mag-des-999    .
      *                  *---------------------------------------------*
      *                  * Se codice filtro non esistente : a reimpo-  *
      *                  * stazione                                    *
      *                  *---------------------------------------------*
           if        w-let-fso-mtv-flg    not  = spaces
                     go to acc-fso-mtv-100.
       acc-fso-mtv-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-fso-mtv-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-fso-mtv-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-fso-mtv-100.
       acc-fso-mtv-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Codice del filtro di selezione    *
      *    *-----------------------------------------------------------*
       vis-fso-mag-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      19                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-fso-mag           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-fso-mag-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Descrizione filtro selezione      *
      *    *-----------------------------------------------------------*
       vis-fso-mag-des-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      rr-fso-mag-des       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-fso-mag-des-999.
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
      *              * Test su tipo stampa                             *
      *              *-------------------------------------------------*
           if        rr-tip-stp           not  = zero
                     go to tdo-ric-sel-200.
           move      "Tipo stampa indefinito !                          
      -              "               "    to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-200.
      *              *-------------------------------------------------*
      *              * Controllo su data iniziale e finale             *
      *              *-------------------------------------------------*
           if        rr-dat-fin           =    zero
                     go to tdo-ric-sel-300.
           if        rr-dat-fin           not  < rr-dat-ini
                     go to tdo-ric-sel-300.
           move      "La data finale non puo' essere inferiore alla data
      -              " iniziale !    "    to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-300.
      *              *-------------------------------------------------*
      *              * Controllo se elementi selezionati che compaiono *
      *              * piu' di una volta                               *
      *              *-------------------------------------------------*
           move      zero                 to   w-acc-ser-edd-c01      .
       tdo-ric-sel-322.
           add       1                    to   w-acc-ser-edd-c01      .
           if        w-acc-ser-edd-c01    >    w-acc-ser-edd-max
                     go to tdo-ric-sel-800.
           if        rr-cau-mag-cod
                    (w-acc-ser-edd-c01)   =    zero
                     go to tdo-ric-sel-800.
           move      rr-cau-mag-cod
                    (w-acc-ser-edd-c01)   to   w-acc-ser-edd-spe      .
           move      zero                 to   w-acc-ser-edd-c02      .
       tdo-ric-sel-324.
           add       1                    to   w-acc-ser-edd-c02      .
           if        w-acc-ser-edd-c02    >    w-acc-ser-edd-max
                     go to tdo-ric-sel-322.
           if        w-acc-ser-edd-c02    =    w-acc-ser-edd-c01
                     go to tdo-ric-sel-324.
           if        w-acc-ser-edd-spe    not = rr-cau-mag-cod
                                               (w-acc-ser-edd-c02)
                     go to tdo-ric-sel-324.
      *                  *---------------------------------------------*
      *                  * Editing                                     *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      rr-cau-mag-cod
                    (w-acc-ser-edd-c02)   to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Messaggio di errore                         *
      *                  *---------------------------------------------*
           move      spaces               to   w-err-box-err-msg      .
           string    "Attenzione : la causale "
                                delimited by   size
                     v-edt      delimited by   spaces
                     " risulta selezionata piu' di una volta"
                                delimited by   size
                                          into w-err-box-err-msg      .
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
      *                  * Emissione messaggio                         *
      *                  *---------------------------------------------*
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                  *---------------------------------------------*
      *                  * Flag di uscita ad errore                    *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cnt-tdo-ric-flg      .
       tdo-ric-sel-999.
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
      *    * Regolarizzazione dei parametri di selezione               *
      *    *-----------------------------------------------------------*
       reg-ric-sel-000.
      *              *-------------------------------------------------*
      *              * Regolarizzazione data finale                    *
      *              *-------------------------------------------------*
           if        rr-dat-fin           not  = zero
                     go to reg-ric-sel-200.
           if        rr-dat-ini           =    zero
                     move 9999999         to   rr-dat-fin
           else      move rr-dat-ini      to   rr-dat-fin             .
       reg-ric-sel-200.
      *              *-------------------------------------------------*
      *              * Regolarizzazione tipo magazzino                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se tipo magazzino '99' : a zero             *
      *                  *---------------------------------------------*
           if        rr-tip-mag           =    99
                     move  zero           to   rr-tip-mag
                     go to reg-ric-sel-300.
      *                  *---------------------------------------------*
      *                  * Se la tabella dei tipi magazzino ammessi e' *
      *                  * formata da un solo elemento : si forza il   *
      *                  * valore corrispondente                       *
      *                  *---------------------------------------------*
           if        w-exp-tpm-amm-num    not  = 1
                     go to reg-ric-sel-300.
           move      w-exp-tip-mag-tpm (1)
                                          to   rr-tip-mag             .
       reg-ric-sel-300.
      *              *-------------------------------------------------*
      *              * Regolarizzazione codice filtro di selezione     *
      *              *-------------------------------------------------*
           if        rr-tip-mag           =    zero
                     move  zero           to   rr-fso-mag             .
       reg-ric-sel-400.
      *              *-------------------------------------------------*
      *              * Regolarizzazione causale di magazzino singola   *
      *              *-------------------------------------------------*
           if        rr-cau-mag-sns       not  = zero
                     move  zero           to   rr-cau-mag             .
       reg-ric-sel-500.
      *              *-------------------------------------------------*
      *              * Numero protocollo per accettazione massimo      *
      *              *-------------------------------------------------*
           if        rr-num-max           not  = zero
                     go to reg-ric-sel-600.
           if        rr-num-min           =    zero
                     move  999999999      to   rr-num-max
           else      move  rr-num-min     to   rr-num-max             .
       reg-ric-sel-600.
      *              *-------------------------------------------------*
      *              * Numero protocollo interno min                   *
      *              *-------------------------------------------------*
           move      rr-num-min           to   w-wrk-npt-acc          .
           move      w-wrk-num-saa        to   w-wrk-npm-saa          .
           move      rr-dpz-inu           to   w-wrk-npm-dpz          .
           move      w-wrk-num-npg        to   w-wrk-npm-npg          .
           move      w-wrk-npt-int        to   rr-npt-min             .
      *              *-------------------------------------------------*
      *              * Numero protocollo interno max                   *
      *              *-------------------------------------------------*
           move      rr-num-max           to   w-wrk-npt-acc          .
           move      w-wrk-num-saa        to   w-wrk-npm-saa          .
           move      rr-dpz-inu           to   w-wrk-npm-dpz          .
           move      w-wrk-num-npg        to   w-wrk-npm-npg          .
           move      w-wrk-npt-int        to   rr-npt-max             .
       reg-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione richieste di selezione                    *
      *    *-----------------------------------------------------------*
       nor-ric-sel-000.
           move      zero                 to   rr-tip-stp             .
           move      zero                 to   rr-dat-ini             .
           move      zero                 to   rr-dat-fin             .
           move      "XX"                 to   rr-opz-stp             .
           move      spaces               to   rr-cod-ute             .
           move      spaces               to   rr-cod-fas             .
           move      zero                 to   rr-tip-mag             .
           move      zero                 to   rr-fso-mag             .
           move      spaces               to   rr-fso-mag-des         .
           perform   nor-ric-sel-cau-000  thru nor-ric-sel-cau-999    .
           perform   nor-ric-sel-mic-000  thru nor-ric-sel-mic-999    .
           move      zero                 to   rr-cod-arc             .
           move      zero                 to   rr-num-min             .
           move      zero                 to   rr-num-max             .
           move      zero                 to   rr-npt-min             .
           move      zero                 to   rr-npt-max             .
       nor-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione richieste di selezione                    *
      *    *                                                           *
      *    * Normalizzazione specifica per causale di magazzino        *
      *    *-----------------------------------------------------------*
       nor-ric-sel-cau-000.
           move      zero                 to   rr-cau-mag             .
           move      spaces               to   rr-cau-mag-des         .
           move      zero                 to   rr-cau-mag-sns         .
           move      zero                 to   rr-cau-mag-els         .
           move      zero                 to   w-acc-ser-edd-c01      .
       nor-ric-sel-cau-100.
           add       1                    to   w-acc-ser-edd-c01      .
           if        w-acc-ser-edd-c01    >    w-acc-ser-edd-max
                     go to nor-ric-sel-cau-900.
           move      zero                 to   rr-cau-mag-cod
                                              (w-acc-ser-edd-c01)     .
           move      spaces               to   rr-cau-mag-dsc
                                              (w-acc-ser-edd-c01)     .
           move      spaces               to   rr-cau-mag-mne
                                              (w-acc-ser-edd-c01)     .
           go to     nor-ric-sel-cau-100.
       nor-ric-sel-cau-900.
           go to     nor-ric-sel-cau-999.
       nor-ric-sel-cau-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione richieste di selezione                    *
      *    *                                                           *
      *    * Normalizzazione specifica per codice conto merce          *
      *    *-----------------------------------------------------------*
       nor-ric-sel-mic-000.
           move      spaces               to   rr-cod-mic             .
           move      spaces               to   rr-cod-mic-des         .
       nor-ric-sel-mic-999.
           exit.

      *    *===========================================================*
      *    * Find su archivio [mmt]                                    *
      *    *-----------------------------------------------------------*
       fnd-arc-mmt-000.
      *              *-------------------------------------------------*
      *              * Test se programma di interrogazione gia' attivo *
      *              *-------------------------------------------------*
           move      "P?"                 to   s-ope                  .
           move      "pmag3010"           to   s-pro                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-liv                not  = zero
                     go to  fnd-arc-mmt-999.
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
      *              * Richiamo programma di interrogazione            *
      *              *-------------------------------------------------*
           move      "pgm/mag/prg/obj/pmag3010"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat                                            .
           cancel    s-pat                                            .
       fnd-arc-mmt-999.
           exit.

      *    *===========================================================*
      *    * Routine lettura tabella [zmc]                             *
      *    *-----------------------------------------------------------*
       let-arc-zmc-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zmc-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice causale a zero                   *
      *              *-------------------------------------------------*
           if        w-let-arc-zmc-cod    =    zero
                     go to let-arc-zmc-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCAU    "         to   f-key                  .
           move      w-let-arc-zmc-cod    to   rf-zmc-cod-cau         .
           move      "pgm/mag/fls/ioc/obj/iofzmc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zmc                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-zmc-400.
       let-arc-zmc-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-zmc-des-cau       to   w-let-arc-zmc-des      .
           move      rf-zmc-mne-cau       to   w-let-arc-zmc-mne      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-zmc-999.
       let-arc-zmc-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-zmc-flg      .
           move      all   "."            to   w-let-arc-zmc-des      .
           go to     let-arc-zmc-600.
       let-arc-zmc-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zmc-des      .
       let-arc-zmc-600.
           move      spaces               to   w-let-arc-zmc-mne      .
       let-arc-zmc-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [zmm]                         *
      *    *-----------------------------------------------------------*
       let-arc-zmm-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zmm-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice conto merce a spaces             *
      *              *-------------------------------------------------*
           if        w-let-arc-zmm-cod    =    spaces
                     go to let-arc-zmm-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCTM    "         to   f-key                  .
           move      w-let-arc-zmm-cod    to   rf-zmm-cod-ctm         .
           move      "pgm/mag/fls/ioc/obj/iofzmm"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zmm                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-zmm-400.
       let-arc-zmm-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-zmm-des-ctm       to   w-let-arc-zmm-des      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-zmm-999.
       let-arc-zmm-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-zmm-flg      .
           move      all   "."            to   w-let-arc-zmm-des      .
           go to     let-arc-zmm-999.
       let-arc-zmm-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zmm-des      .
       let-arc-zmm-999.
           exit.

      *    *===========================================================*
      *    * Routine lettura archivio [zos] per [dcp]                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/lzosdcp0.lts"                   .

      *    *===========================================================*
      *    * Routine lettura archivio [zos] per [dps]                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/dps/prg/cpy/lzosdps0.lts"                   .

      *    *===========================================================*
      *    * Routine lettura archivio [zos] per [dpm]                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/dpm/prg/cpy/lzosdpm0.lts"                   .

      *    *===========================================================*
      *    * Routine lettura archivio [zos] per [mtv]                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/mtv/prg/cpy/lzosmtv0.lts"                   .
          
      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice causale di ge-  *
      *    * stione magazzino                                          *
      *    *-----------------------------------------------------------*
           copy      "pgm/mag/prg/cpy/acmnzmc0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice conto merce     *
      *    *-----------------------------------------------------------*
           copy      "pgm/mag/prg/cpy/acdezmm0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice filtro ordina-  *
      *    * mento e selezione per file [dcp]                          *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/azosdcp0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice filtro ordina-  *
      *    * mento e selezione per file [dps]                          *
      *    *-----------------------------------------------------------*
           copy      "pgm/dps/prg/cpy/azosdps0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice filtro ordina-  *
      *    * mento e selezione per file [dpm]                          *
      *    *-----------------------------------------------------------*
           copy      "pgm/dpm/prg/cpy/azosdpm0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice filtro ordina-  *
      *    * mento e selezione per file [mtv]                          *
      *    *-----------------------------------------------------------*
           copy      "pgm/mtv/prg/cpy/azosmtv0.acs"                   .

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
      *    * Determinazione tipi magazzino ammessi                     *
      *    *-----------------------------------------------------------*
       det-tpm-amm-000.
      *              *-------------------------------------------------*
      *              * Preparazione area w-exp limitata ai soli tipi   *
      *              * magazzino previsti                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se e' attiva solamente la gestione dei      *
      *                  * Prodotti di vendita si forza come unico     *
      *                  * elemento nella lista                        *
      *                  *---------------------------------------------*
           if        w-prs-dps-snx        not  = "S" and
                     w-prs-dpm-snx        not  = "S" and
                     w-prs-mtv-snx        not  = "S"
                     move  01             to   w-exp-tip-mag-tpm (1)
                     move  01             to   w-exp-tpm-amm-num
                     go to det-tpm-amm-999.
      *                  *---------------------------------------------*
      *                  * Tipo magazzino 'Tutti' :                    *
      *                  * - Se attiva solo la gestione Prodotti di    *
      *                  *   di vendita non si inserisce nella lista   *
      *                  *---------------------------------------------*
           if        w-prs-dps-snx        =    "S" or
                     w-prs-dpm-snx        =    "S" or
                     w-prs-mtv-snx        =    "S"
                     move  99             to   w-exp-tpm-amm-tpm (1)
           else      move  zero           to   w-exp-tpm-amm-tpm (1)  .
      *                  *---------------------------------------------*
      *                  * Tipo magazzino 'Prodotto di vendita' :      *
      *                  * - Sempre ammesso                            *
      *                  *---------------------------------------------*
           move      01                   to   w-exp-tpm-amm-tpm (2)  .
      *                  *---------------------------------------------*
      *                  * Tipo magazzino 'Semilavorato' :             *
      *                  * - Se gestione semilavorati attiva lo si     *
      *                  *   inserisce nella lista, altrimenti no      *
      *                  *---------------------------------------------*
           if        w-prs-dps-snx        =    "S"
                     move  02             to   w-exp-tpm-amm-tpm (3)
           else      move  zero           to   w-exp-tpm-amm-tpm (3)  .
      *                  *---------------------------------------------*
      *                  * Tipo magazzino 'Materia Prima' :            *
      *                  * - Se gestione materie prime attiva lo si    *
      *                  *   inserisce nella lista, altrimenti no      *
      *                  *---------------------------------------------*
           if        w-prs-dpm-snx        =    "S"
                     move  03             to   w-exp-tpm-amm-tpm (4)
           else      move  zero           to   w-exp-tpm-amm-tpm (4)  .
      *                  *---------------------------------------------*
      *                  * Tipo magazzino 'Materiale Vario' :          *
      *                  * - Se gestione materiale vario attiva lo si  *
      *                  *   inserisce nella lista, altrimenti no      *
      *                  *---------------------------------------------*
           if        w-prs-mtv-snx        =    "S"
                     move  04             to   w-exp-tpm-amm-tpm (5)
           else      move  zero           to   w-exp-tpm-amm-tpm (5)  .
       det-tpm-amm-010.
      *              *-------------------------------------------------*
      *              * Compattamento della lista dei tipi magazzino    *
      *              * ammessi                                         *
      *              *-------------------------------------------------*
           if        w-exp-tpm-amm-tpm (1)
                                          =    zero
                     move  w-exp-tpm-amm-tpm (2)
                                          to   w-exp-tpm-amm-tpm (1)
                     move  w-exp-tpm-amm-tpm (3)
                                          to   w-exp-tpm-amm-tpm (2)
                     move  w-exp-tpm-amm-tpm (4)
                                          to   w-exp-tpm-amm-tpm (3)
                     move  w-exp-tpm-amm-tpm (5)
                                          to   w-exp-tpm-amm-tpm (4)
                     move  zero           to   w-exp-tpm-amm-tpm (5)  .
           if        w-exp-tpm-amm-tpm (2)
                                          =    zero
                     move  w-exp-tpm-amm-tpm (3)
                                          to   w-exp-tpm-amm-tpm (2)
                     move  w-exp-tpm-amm-tpm (4)
                                          to   w-exp-tpm-amm-tpm (3)
                     move  w-exp-tpm-amm-tpm (5)
                                          to   w-exp-tpm-amm-tpm (4)
                     move  zero           to   w-exp-tpm-amm-tpm (5)  .
           if        w-exp-tpm-amm-tpm (3)
                                          =    zero
                     move  w-exp-tpm-amm-tpm (4)
                                          to   w-exp-tpm-amm-tpm (3)
                     move  w-exp-tpm-amm-tpm (5)
                                          to   w-exp-tpm-amm-tpm (4)
                     move  zero           to   w-exp-tpm-amm-tpm (5)  .
           if        w-exp-tpm-amm-tpm (4)
                                          =    zero
                     move  w-exp-tpm-amm-tpm (5)
                                          to   w-exp-tpm-amm-tpm (4)
                     move  zero           to   w-exp-tpm-amm-tpm (5)  .
      *              *-------------------------------------------------*
      *              * Preparazione del numero di elementi in tabella  *
      *              * tipi magazzino ammessi                          *
      *              *-------------------------------------------------*
           move      zero                 to   w-exp-tpm-amm-num      .
       det-tpm-amm-020.
           add       1                    to   w-exp-tpm-amm-num      .
           if        w-exp-tpm-amm-num    >    5
                     move  5              to   w-exp-tpm-amm-num
                     go to det-tpm-amm-040.
           if        w-exp-tpm-amm-tpm
                    (w-exp-tpm-amm-num)   not  = zero
                     go to det-tpm-amm-020.
           subtract  1                    from w-exp-tpm-amm-num      .
       det-tpm-amm-040.
      *              *-------------------------------------------------*
      *              * Preparazione delle descrizioni relative ai tipi *
      *              * magazzino ammessi                               *
      *              *-------------------------------------------------*
           move      zero                 to   w-exp-tpm-amm-c01      .
       det-tpm-amm-100.
           add       1                    to   w-exp-tpm-amm-c01      .
           if        w-exp-tpm-amm-c01    >    w-exp-tip-mag-num
                     go to det-tpm-amm-999.
           move      zero                 to   w-exp-tpm-amm-c02      .
       det-tpm-amm-120.
           add       1                    to   w-exp-tpm-amm-c02      .
           if        w-exp-tpm-amm-c02    >    5
                     go to det-tpm-amm-100.
           if        w-exp-tpm-amm-tpm
                    (w-exp-tpm-amm-c01)   not  = w-exp-tip-mag-tpm
                                                (w-exp-tpm-amm-c02)
                     go to det-tpm-amm-120.
           move      w-exp-tip-mag-ele
                    (w-exp-tpm-amm-c02)   to   w-exp-tpm-amm-ele
                                              (w-exp-tpm-amm-c01)     .
           go to     det-tpm-amm-100.
       det-tpm-amm-999.
           exit.

      *    *===========================================================*
      *    * Subroutines per allineamenti a destra o a sinistra oppure *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cps"                   .

