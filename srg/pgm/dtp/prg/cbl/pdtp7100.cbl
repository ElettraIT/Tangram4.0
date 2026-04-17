       Identification Division.
       Program-Id.                                 pdtp7100           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    dtp                 *
      *                                Settore:    val                 *
      *                                   Fase:    dtp710              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 04/12/93    *
      *                       Ultima revisione:    NdK del 24/05/00    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Valorizzazione scalare distinte base        *
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

      *================================================================*
       Input-Output Section.
      *================================================================*

       File-Control.

      *    *===========================================================*
      *    * File Control [srt]                                        *
      *    *-----------------------------------------------------------*
           select  srt       assign       to sort                     .

      ******************************************************************
       Data Division.
      ******************************************************************

      *================================================================*
       File Section.
      *================================================================*

      *    *===========================================================*
      *    * File Description [srt]                                    *
      *    *-----------------------------------------------------------*
       sd  srt.
      *    *-----------------------------------------------------------*
      *    * Sort record                                               *
      *    *-----------------------------------------------------------*
       01  srt-rec.
      *        *-------------------------------------------------------*
      *        * Chiave di ordinamento                                 *
      *        *-------------------------------------------------------*
           05  srt-key.
      *            *---------------------------------------------------*
      *            * Filler                                            *
      *            *---------------------------------------------------*
               10  filler                 pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  srt-dat.
      *            *---------------------------------------------------*
      *            * Filler                                            *
      *            *---------------------------------------------------*
               10  filler                 pic  x(01)                  .

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
                     "dtp"                                            .
      *        *-------------------------------------------------------*
      *        * Settore gestionale                                    *
      *        *-------------------------------------------------------*
           05  i-ide-set                  pic  x(03) value
                     "val"                                            .
      *        *-------------------------------------------------------*
      *        * Fase gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-fas                  pic  x(06) value
                     "dtp710"                                         .
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10) value
                     "pdtp7100"                                       .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  i-ide-des                  pic  x(40) value
                     "  VALORIZZAZIONE SCALARE DISTINTA BASE  "       .

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
      *    * Area di comunicazione per modulo                "mprint"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/p"                                  .

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
      *        * Flags di controllo su scelta stampante                *
      *        *-------------------------------------------------------*
           05  w-cnt-ccl.
      *            *---------------------------------------------------*
      *            * Contatore cicli gia' eseguiti                     *
      *            *---------------------------------------------------*
               10  w-cnt-ccl-gia-ese      pic  9(11)                  .
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
      *            *---------------------------------------------------*
      *            * Per routine let-sel-stp-000                       *
      *            *---------------------------------------------------*
               10  w-cnt-let-sel-stp      pic  x(01)                  .
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
      *            * Si/No richieste per programma di esecuzione       *
      *            *---------------------------------------------------*
               10  w-cnt-fun-snx-ric      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Si/No funzionamento ciclico                       *
      *            *---------------------------------------------------*
               10  w-cnt-fun-snx-cic      pic  x(01)                  .
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
      *        * Area di controllo per funzionamento print-routine     *
      *        *-------------------------------------------------------*
           05  w-cnt-prn.
      *            *---------------------------------------------------*
      *            * Flag di primo giro                                *
      *            *---------------------------------------------------*
               10  w-cnt-prn-mrk-uno      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Segnale di 'begin' eseguito                       *
      *            *---------------------------------------------------*
               10  w-cnt-prn-mrk-beg      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Flag di interruzione forzata                      *
      *            *---------------------------------------------------*
               10  w-cnt-prn-flg-int      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Flag di uscita da subroutines principali          *
      *            *---------------------------------------------------*
               10  w-cnt-prn-flg-sub      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Area per salvataggio parametri rottura livello    *
      *            *---------------------------------------------------*
               10  w-cnt-prn-sav-liv.
                   15  w-cnt-prn-sav-l05  pic  x(64)                  .
                   15  w-cnt-prn-sav-l04  pic  x(64)                  .
                   15  w-cnt-prn-sav-l03  pic  x(64)                  .
                   15  w-cnt-prn-sav-l02  pic  x(64)                  .
                   15  w-cnt-prn-sav-l01  pic  x(64)                  .
      *            *---------------------------------------------------*
      *            * Area per salvataggio area di rottura              *
      *            *---------------------------------------------------*
               10  w-cnt-prn-sav-rot.
                   15  filler occurs 320  pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Work per manipolazione titolo stampato                *
      *        *-------------------------------------------------------*
           05  w-cnt-tit.
               10  w-cnt-tit-des-tit.
                   15  w-cnt-tit-chr-tit  occurs 80
                                          pic  x(01)                  .
               10  w-cnt-tit-num-pag      pic  9(05)                  .
               10  w-cnt-tit-dat-stp      pic  9(07)                  .
               10  w-cnt-tit-des-azi.
                   15  w-cnt-tit-chr-azi  occurs 40
                                          pic  x(01)                  .
               10  w-cnt-tit-ctr-wrk      pic  9(02)                  .
               10  w-cnt-tit-ctr-azi      pic  9(02)                  .
               10  w-cnt-tit-ctr-tit      pic  9(02)                  .
               10  w-cnt-tit-pos-tit      pic  9(03)                  .
               10  w-cnt-tit-ctr-dep      pic  9(02)                  .
               10  w-cnt-tit-ctr-cif      pic  9(02)                  .
               10  w-cnt-tit-pos-dep      pic  9(03)                  .
               10  w-cnt-tit-num-lin      pic  9(01)                  .
      *        *-------------------------------------------------------*
      *        * Work per string-unstring                              *
      *        *-------------------------------------------------------*
           05  w-cnt-stu.
               10  w-cnt-stu-num-seg      pic  9(05)                  .
               10  w-cnt-stu-pnt-stu      pic  9(05)                  .
               10  w-cnt-stu-255-byt.
                   15  filler occurs 255  pic  x(01)                  .
               10  w-cnt-stu-sav-pnt      pic  9(05)                  .

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
      *    * Records files                                             *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [dcp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfdcp"                          .
      *        *-------------------------------------------------------*
      *        * [dps]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dps/fls/rec/rfdps"                          .
      *        *-------------------------------------------------------*
      *        * [dpm]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dpm/fls/rec/rfdpm"                          .
      *        *-------------------------------------------------------*
      *        * [lgt]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dtp/fls/rec/rflgt"                          .
      *        *-------------------------------------------------------*
      *        * [lgr]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dtp/fls/rec/rflgr"                          .
      *        *-------------------------------------------------------*
      *        * [lgv]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dtp/fls/rec/rflgv"                          .

      *    *===========================================================*
      *    * Work-area richieste                                       *
      *    *-----------------------------------------------------------*
       01  rr.
      *        *-------------------------------------------------------*
      *        * Data per valorizzazione                               *
      *        *-------------------------------------------------------*
           05  rr-dat-val                 pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Tipo di distinta                                      *
      *        *  - 01 : Per prodotti di vendita                       *
      *        *  - 02 : Per semilavorati                              *
      *        *  - 99 : Sub-distinta virtuale                         *
      *        *-------------------------------------------------------*
           05  rr-tip-dis                 pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Codice numerico per l'assieme                         *
      *        *-------------------------------------------------------*
           05  rr-num-ass                 pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Codice alfanumerico per l'assieme                     *
      *        *-------------------------------------------------------*
           05  rr-alf-ass                 pic  x(14)                  .
      *        *-------------------------------------------------------*
      *        * Descrizione per l'assieme                             *
      *        *-------------------------------------------------------*
           05  rr-des-ass                 pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Unita' di misura per l'assieme                        *
      *        *-------------------------------------------------------*
           05  rr-umi-ass                 pic  x(03)                  .
      *        *-------------------------------------------------------*
      *        * Tipo di valorizzazione per le materie prime incluse   *
      *        * in distinta                                           *
      *        * - 01 : Costo medio ponderato annuale                  *
      *        * - 02 : Ultimo costo d'acquisto                        *
      *        * - 03 : Costo standard                                 *
      *        *-------------------------------------------------------*
           05  rr-tva-map                 pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Tipo di valorizzazione per i semilavorati finali in-  *
      *        * clusi in distinta                                     *
      *        * - 01 : Costo medio ponderato annuale                  *
      *        * - 02 : Ultimo costo d'acquisto                        *
      *        * - 03 : Costo standard                                 *
      *        * - 99 : In funzione dei componenti                     *
      *        *                                                       *
      *        * Nota : Questa valorizzazione vale solamente per i se- *
      *        *        milavorati finali di un ciclo di lavorazione,  *
      *        *        ovvero quei semilavorati che hanno la voce a-  *
      *        *        nagrafica 'rf-dps-tip-sem' pari a 01.          *
      *        *        Gli altri semilavorati, ovvero quelli inter-   *
      *        *        medi di un ciclo di lavorazione, sono equipa-  *
      *        *        rati alle subdistinte virtuali, per cui assu-  *
      *        *        meranno il loro valore sempre in funzione del  *
      *        *        valore dei loro componenti.                    *
      *        *-------------------------------------------------------*
           05  rr-tva-sem                 pic  9(02)                  .

      *    *===========================================================*
      *    * Work per subroutines di Let                               *
      *    *-----------------------------------------------------------*
       01  w-let.
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [dcp]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-dcp.
               10  w-let-arc-dcp-flg      pic  x(01)                  .
               10  w-let-arc-dcp-num      pic  9(07)                  .
               10  w-let-arc-dcp-alf      pic  x(14)                  .
               10  w-let-arc-dcp-des      pic  x(40)                  .
               10  w-let-arc-dcp-tpr      pic  x(01)                  .
               10  w-let-arc-dcp-umi      pic  x(03)                  .
               10  w-let-arc-dcp-deq      pic  9(01)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [dps]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-dps.
               10  w-let-arc-dps-flg      pic  x(01)                  .
               10  w-let-arc-dps-num      pic  9(07)                  .
               10  w-let-arc-dps-alf      pic  x(14)                  .
               10  w-let-arc-dps-des      pic  x(40)                  .
               10  w-let-arc-dps-umi      pic  x(03)                  .
               10  w-let-arc-dps-deq      pic  9(01)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [lgv]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-lgv.
               10  w-let-arc-lgv-flg      pic  x(01)                  .
               10  w-let-arc-lgv-num      pic  9(07)                  .
               10  w-let-arc-lgv-alf      pic  x(14)                  .
               10  w-let-arc-lgv-des      pic  x(40)                  .
               10  w-let-arc-lgv-umi      pic  x(03)                  .
               10  w-let-arc-lgv-deq      pic  9(01)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [dpm]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-dpm.
               10  w-let-arc-dpm-flg      pic  x(01)                  .
               10  w-let-arc-dpm-num      pic  9(07)                  .
               10  w-let-arc-dpm-alf      pic  x(14)                  .
               10  w-let-arc-dpm-des      pic  x(40)                  .
               10  w-let-arc-dpm-umi      pic  x(03)                  .
               10  w-let-arc-dpm-deq      pic  9(01)                  .

      *    *===========================================================*
      *    * Work-area per salvataggi                                  *
      *    *-----------------------------------------------------------*
       01  w-sav.
      *        *-------------------------------------------------------*
      *        * Salvataggio per tipo distinta                         *
      *        *-------------------------------------------------------*
           05  w-sav-tip-dis              pic  9(02)                  .

      *    *===========================================================*
      *    * Work per accettazioni campi espansi                       *
      *    *-----------------------------------------------------------*
       01  w-exp.
      *        *-------------------------------------------------------*
      *        * Work per : Tipo distinta                              *
      *        *-------------------------------------------------------*
           05  w-exp-tip-dis.
               10  w-exp-tip-dis-num      pic  9(02)       value 3    .
               10  w-exp-tip-dis-lun      pic  9(02)       value 40   .
               10  w-exp-tip-dis-tbl.
                   15  filler             pic  x(40) value
                            "Distinta per prodotto di vendita        ".
                   15  filler             pic  x(40) value
                            "Distinta per semilavorato               ".
                   15  filler             pic  x(40) value
                            "Sub-distinta virtuale                   ".
      *        *-------------------------------------------------------*
      *        * Work per : Tipo valorizzazione per le materie prime   *
      *        *-------------------------------------------------------*
           05  w-exp-tva-map.
               10  w-exp-tva-map-num      pic  9(02)       value 3    .
               10  w-exp-tva-map-lun      pic  9(02)       value 30   .
               10  w-exp-tva-map-tbl.
                   15  filler             pic  x(30) value
                            "Costo medio ponderato annuale "          .
                   15  filler             pic  x(30) value
                            "Ultimo costo d'acquisto       "          .
                   15  filler             pic  x(30) value
                            "Costo standard                "          .
      *        *-------------------------------------------------------*
      *        * Work per : Tipo valorizzazione per i semilavorati     *
      *        *-------------------------------------------------------*
           05  w-exp-tva-sem.
               10  w-exp-tva-sem-num      pic  9(02)       value 4    .
               10  w-exp-tva-sem-lun      pic  9(02)       value 30   .
               10  w-exp-tva-sem-tbl.
                   15  filler             pic  x(30) value
                            "In funzione dei componenti    "          .
                   15  filler             pic  x(30) value
                            "Costo medio ponderato annuale "          .
                   15  filler             pic  x(30) value
                            "Ultimo costo d'acquisto       "          .
                   15  filler             pic  x(30) value
                            "Costo standard                "          .

      *    *===========================================================*
      *    * Link-area per accettazione codice prodotto 'dcp'          *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/acoddcp0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice semilavorato 'dps'      *
      *    *-----------------------------------------------------------*
           copy      "pgm/dps/prg/cpy/acoddps0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice legame virtuale         *
      *    *-----------------------------------------------------------*
           copy      "pgm/dtp/prg/cpy/acodlgv0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice materia prima           *
      *    *-----------------------------------------------------------*
           copy      "pgm/dpm/prg/cpy/acoddpm0.acl"                   .

      *    *===========================================================*
      *    * Work per subroutine di esplosione scalare distinta base   *
      *    *-----------------------------------------------------------*
       01  w-esp-scl-dtp.
      *        *-------------------------------------------------------*
      *        * Parametri in input                                    *
      *        *-------------------------------------------------------*
           05  w-esp-scl-dtp-inp.
      *            *---------------------------------------------------*
      *            * Funzione da eseguire                              *
      *            *---------------------------------------------------*
               10  w-esp-scl-dtp-fun      pic  x(05)                  .
      *            *---------------------------------------------------*
      *            * Tipo magazzino per l'assieme da esplodere         *
      *            *---------------------------------------------------*
               10  w-esp-scl-dtp-tde      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Codice numerico magazzino da esplodere            *
      *            *---------------------------------------------------*
               10  w-esp-scl-dtp-nde      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Codice alfanumerico magazzino da esplodere        *
      *            *---------------------------------------------------*
               10  w-esp-scl-dtp-ade      pic  x(14)                  .
      *            *---------------------------------------------------*
      *            * Quantita' da esplodere per l'assieme              *
      *            *---------------------------------------------------*
               10  w-esp-scl-dtp-qde      pic s9(08)v9(03)            .
      *        *-------------------------------------------------------*
      *        * Parametri in output                                   *
      *        *-------------------------------------------------------*
           05  w-esp-scl-dtp-out.
      *            *---------------------------------------------------*
      *            * Status di uscita dalle funzioni                   *
      *            *---------------------------------------------------*
               10  w-esp-scl-dtp-sts      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Tipo di elemento trovato nella scansione          *
      *            *---------------------------------------------------*
               10  w-esp-scl-dtp-tip      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Codice numerico elemento trovato                  *
      *            *---------------------------------------------------*
               10  w-esp-scl-dtp-num      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Codice alfanumerico elemento trovato              *
      *            *---------------------------------------------------*
               10  w-esp-scl-dtp-alf      pic  x(14)                  .
      *            *---------------------------------------------------*
      *            * Livello di profondita' elemento trovato           *
      *            *---------------------------------------------------*
               10  w-esp-scl-dtp-liv      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Quantita' totale relativa all'elemento trovato    *
      *            *---------------------------------------------------*
               10  w-esp-scl-dtp-qta      pic s9(08)v9(03)            .
      *            *---------------------------------------------------*
      *            * Flag di anagrafica elemento esistente             *
      *            *  - Spaces : Si                                    *
      *            *  - #      : No                                    *
      *            *---------------------------------------------------*
               10  w-esp-scl-dtp-ana      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Descrizione da anagrafica elemento                *
      *            *---------------------------------------------------*
               10  w-esp-scl-dtp-des      pic  x(40)                  .
      *            *---------------------------------------------------*
      *            * Unita' di misura da anagrafica elemento           *
      *            *---------------------------------------------------*
               10  w-esp-scl-dtp-umi      pic  x(03)                  .
      *            *---------------------------------------------------*
      *            * Numero decimali quantita' da anagrafica elemento  *
      *            *---------------------------------------------------*
               10  w-esp-scl-dtp-dec      pic  9(01)                  .
      *        *-------------------------------------------------------*
      *        * Area di lavoro                                        *
      *        *-------------------------------------------------------*
           05  w-esp-scl-dtp-war.
      *            *---------------------------------------------------*
      *            * Livello di profondita' interno                    *
      *            *---------------------------------------------------*
               10  w-esp-scl-dtp-wlp      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Salvataggio livello di profondita' interno attu-  *
      *            * ale                                               *
      *            *---------------------------------------------------*
               10  w-esp-scl-dtp-wls      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Segnale di prossima operazione da eseguire        *
      *            *  - S : Start sul livello di profondita' attuale   *
      *            *  - U : Incremento di un livello e quindi 'start'  *
      *            *        sul livello di profondita' aumentato       *
      *            *  - N : Read Next sul livello di profondita' attu- *
      *            *        ale                                        *
      *            *---------------------------------------------------*
               10  w-esp-scl-dtp-wpo      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Tabella per ogni livello, max 50                  *
      *            *---------------------------------------------------*
               10  w-esp-scl-dtp-wtb occurs 50.
      *                *-----------------------------------------------*
      *                * Tipo di elemento                              *
      *                *  - 01 : Prodotto finito                       *
      *                *  - 02 : Semilavorato                          *
      *                *  - 03 : Materia prima                         *
      *                *  - 99 : Subdistinta virtuale                  *
      *                *-----------------------------------------------*
                   15  w-esp-scl-dtp-wti  pic  9(02)                  .
      *                *-----------------------------------------------*
      *                * Codice numerico elemento                      *
      *                *-----------------------------------------------*
                   15  w-esp-scl-dtp-wnu  pic  9(07)                  .
      *                *-----------------------------------------------*
      *                * Codice alfanumerico elemento                  *
      *                *-----------------------------------------------*
                   15  w-esp-scl-dtp-wal  pic  x(14)                  .
      *                *-----------------------------------------------*
      *                * Numero progressivo di riga distinta           *
      *                *-----------------------------------------------*
                   15  w-esp-scl-dtp-wrg  pic  9(05)                  .
      *                *-----------------------------------------------*
      *                * Quantita' relativa all'elemento               *
      *                *-----------------------------------------------*
                   15  w-esp-scl-dtp-wqt  pic s9(08)v9(03)            .
      *                *-----------------------------------------------*
      *                * Elemento anagraficamente esistente            *
      *                *  - Spaces : Si                                *
      *                *  - #      : No                                *
      *                *-----------------------------------------------*
                   15  w-esp-scl-dtp-wes  pic  x(01)                  .
      *                *-----------------------------------------------*
      *                * Descrizione anagrafica per l'elemento         *
      *                *-----------------------------------------------*
                   15  w-esp-scl-dtp-wde  pic  x(40)                  .
      *                *-----------------------------------------------*
      *                * Unita' di misura per l'elemento               *
      *                *-----------------------------------------------*
                   15  w-esp-scl-dtp-wum  pic  x(03)                  .
      *                *-----------------------------------------------*
      *                * Numero decimali per quantita'                 *
      *                *-----------------------------------------------*
                   15  w-esp-scl-dtp-wnd  pic  9(01)                  .

      *    *===========================================================*
      *    * Area di comunicazione per determinazione valore unitario  *
      *    * di magazzino                                              *
      *    *-----------------------------------------------------------*
           copy      "pgm/mag/prg/cpy/dvunmag0.dtl"                   .

      *    *===========================================================*
      *    * Work-area per routine di valorizzazione scalare           *
      *    *-----------------------------------------------------------*
       01  w-vlz-scl.
      *        *-------------------------------------------------------*
      *        * Flag per per forzatura esecuzione tutta a fine ciclo  *
      *        *-------------------------------------------------------*
           05  w-vlz-scl-flg-end          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per numero pagina di stampa                    *
      *        *-------------------------------------------------------*
           05  w-vlz-scl-num-pag          pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Flag di determinazione valore unitario                *
      *        *  - Spaces : Determinato correttamente                 *
      *        *  - #      : Determinazione non possibile              *
      *        *-------------------------------------------------------*
           05  w-vlz-scl-flg-val          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per determinazione valore unitario             *
      *        *-------------------------------------------------------*
           05  w-vlz-scl-det-val          pic  9(11)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per determinazione sub-valori parziali         *
      *        *-------------------------------------------------------*
           05  w-vlz-scl-det-sub          pic  9(11)                  .
      *        *-------------------------------------------------------*
      *        * Livello di profondita' da stampare                    *
      *        *-------------------------------------------------------*
           05  w-vlz-scl-liv-stp          pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Tipo di elemento da stampare                          *
      *        *-------------------------------------------------------*
           05  w-vlz-scl-tel-stp          pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Codice numerico elemento da stampare                  *
      *        *-------------------------------------------------------*
           05  w-vlz-scl-num-stp          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Codice alfanumerico elemento da stampare              *
      *        *-------------------------------------------------------*
           05  w-vlz-scl-alf-stp          pic  x(14)                  .
      *        *-------------------------------------------------------*
      *        * Descrizione elemento da stampare                      *
      *        *-------------------------------------------------------*
           05  w-vlz-scl-des-stp          pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Unita' di misura elemento da stampare                 *
      *        *-------------------------------------------------------*
           05  w-vlz-scl-umi-stp          pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Numero decimali quantita' elemento da stampare        *
      *        *-------------------------------------------------------*
           05  w-vlz-scl-ndq-stp          pic  9(01)                  .
      *        *-------------------------------------------------------*
      *        * Quantita' elemento da stampare                        *
      *        *-------------------------------------------------------*
           05  w-vlz-scl-qta-stp          pic s9(08)v9(03)            .
      *        *-------------------------------------------------------*
      *        * Si/No costo e valore elemento da stampare             *
      *        *  - S : Si                                             *
      *        *  - N : No                                             *
      *        *-------------------------------------------------------*
           05  w-vlz-scl-snx-cev          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Costo unitario elemento da stampare                   *
      *        *-------------------------------------------------------*
           05  w-vlz-scl-cun-stp          pic  9(11)                  .
      *        *-------------------------------------------------------*
      *        * Valore elemento da stampare                           *
      *        *-------------------------------------------------------*
           05  w-vlz-scl-val-stp          pic s9(11)                  .
      *        *-------------------------------------------------------*
      *        * Work per displacement di stampa                       *
      *        *-------------------------------------------------------*
           05  w-vlz-scl-wds-stp          pic  9(03)                  .

      *    *===========================================================*
      *    * Work area per controllo rotture di livello                *
      *    *-----------------------------------------------------------*
       01  w-rot.
      *        *-------------------------------------------------------*
      *        * 5. livello di rottura                                 *
      *        *-------------------------------------------------------*
           05  w-rot-l05.
               10  filler                 pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * 4. livello di rottura                                 *
      *        *-------------------------------------------------------*
           05  w-rot-l04.
               10  filler                 pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * 3. livello di rottura                                 *
      *        *-------------------------------------------------------*
           05  w-rot-l03.
               10  filler                 pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * 2. livello di rottura                                 *
      *        *-------------------------------------------------------*
           05  w-rot-l02.
               10  filler                 pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * 1. livello di rottura                                 *
      *        *-------------------------------------------------------*
           05  w-rot-l01.
               10  filler                 pic  x(01)                  .

      ******************************************************************
       Procedure Division.
      ******************************************************************

      *================================================================*
      *       Main                                                     *
      *================================================================*
       main-000.
      *              *-------------------------------------------------*
      *              * Contatore cicli gia' eseguiti a zero            *
      *              *-------------------------------------------------*
           move      zero                 to   w-cnt-ccl-gia-ese      .
      *              *-------------------------------------------------*
      *              * Inizializzazione marker Begin di stampa         *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-mrk-beg      .
      *              *-------------------------------------------------*
      *              * Dichiarazione di inizio programma               *
      *              *-------------------------------------------------*
           perform   dic-ini-pgm-000      thru dic-ini-pgm-999        .
           if        w-cnt-dic-ini-pgm    not  = spaces
                     go to main-999.
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
           perform   rou-opn-fls-000      thru rou-opn-fls-999        .
           if        w-cnt-rou-opn-fls    not  = spaces
                     go to main-800.
      *              *-------------------------------------------------*
      *              * Esecuzione routine pre-esecuzione programma     *
      *              *-------------------------------------------------*
           perform   pre-exe-pgm-000      thru pre-exe-pgm-999        .
           if        w-cnt-pre-exe-pgm    not  = spaces
                     go to main-800.
      *              *-------------------------------------------------*
      *              * Preparazione tipo funzionamento programma       *
      *              *-------------------------------------------------*
           perform   pre-tip-fun-000      thru pre-tip-fun-999        .
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
                     go to main-800.
      *                  *---------------------------------------------*
      *                  * Se uscita per 'N'                           *
      *                  *---------------------------------------------*
           if        w-cnt-acc-ric-sel    =    "N"
                     go to main-250.
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
      *              * ne stampa, purche' si sia al primo ciclo di e-  *
      *              * secuzione                                       *
      *              *-------------------------------------------------*
           perform   pre-prm-stp-000      thru pre-prm-stp-999        .
      *              *-------------------------------------------------*
      *              * Selezione parametri stampa, purche' si sia al   *
      *              * primo ciclo di esecuzione, ed il programma pre- *
      *              * veda la stampa                                  *
      *              *-------------------------------------------------*
           perform   sel-prm-stp-000      thru sel-prm-stp-999        .
      *                  *---------------------------------------------*
      *                  * Test se uscita                              *
      *                  *---------------------------------------------*
           if        w-cnt-sel-prm-stp    not  = spaces
                     go to main-800.
       main-450.
      *              *-------------------------------------------------*
      *              * Esecuzione in foreground                        *
      *              *-------------------------------------------------*
           perform   exe-pgm-frg-000      thru exe-pgm-frg-999        .
      *              *-------------------------------------------------*
      *              * Test se tipo esecuzione ciclico                 *
      *              *-------------------------------------------------*
           if        w-cnt-fun-snx-ric    not  = "S" or
                     w-cnt-fun-snx-cic    not  = "S"
                     go to main-800.
      *              *-------------------------------------------------*
      *              * Incremento numero ciclo gia' eseguiti           *
      *              *-------------------------------------------------*
           add       1                    to   w-cnt-ccl-gia-ese      .
      *              *-------------------------------------------------*
      *              * Riciclo                                         *
      *              *-------------------------------------------------*
           go to     main-250.
       main-800.
      *              *-------------------------------------------------*
      *              * Close files                                     *
      *              *-------------------------------------------------*
           perform   rou-cls-fls-000      thru rou-cls-fls-999        .
       main-810.
      *              *-------------------------------------------------*
      *              * Se eseguiti zero cicli : ad uscita              *
      *              *-------------------------------------------------*
           if        w-cnt-ccl-gia-ese    =    zero
                     go to main-900.
       main-820.
      *              *-------------------------------------------------*
      *              * Se eseguito Begin di stampa : si esegue End di  *
      *              * stampa                                          *
      *              *-------------------------------------------------*
           if        w-cnt-prn-mrk-beg    =    spaces
                     go to main-830.
           move      "EN"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       main-830.
      *                  *---------------------------------------------*
      *                  * Cancellazione modulo stampa                 *
      *                  *---------------------------------------------*
           cancel    "swd/mod/prg/obj/mprint"                         .
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
      *    * Programma di esecuzione in foreground                     *
      *    *-----------------------------------------------------------*
       exe-pgm-frg-000.
      *              *-------------------------------------------------*
      *              * Inizializzazione rullino messaggi di foreground *
      *              *-------------------------------------------------*
           move      "OF"                 to   m-ope                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
      *              *-------------------------------------------------*
      *              * Se errore : uscita                              *
      *              *-------------------------------------------------*
            if       m-rsc                not  = spaces
                     go to exe-pgm-frg-999.
      *              *-------------------------------------------------*
      *              * Messaggio di programma in esecuzione            *
      *              *-------------------------------------------------*
           move      "PE"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       exe-pgm-frg-300.
      *              *-------------------------------------------------*
      *              * Esecuzione del programma                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura parametri di selezione stampa, pur- *
      *                  * che' si sia al primo ciclo di esecuzione, e *
      *                  * il programma preveda la stampa              *
      *                  *---------------------------------------------*
           perform   let-sel-stp-000      thru let-sel-stp-999        .
           if        w-cnt-let-sel-stp    not  = spaces
                     go to exe-pgm-frg-999.
      *                  *---------------------------------------------*
      *                  * Esecuzione eventuale sort preliminare       *
      *                  *---------------------------------------------*
           perform   exe-rou-srt-000      thru exe-rou-srt-999        .
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda se sort eseguito       *
      *                  *---------------------------------------------*
           if        w-cnt-exe-rou-srt    =    spaces
                     go to exe-pgm-frg-400
           else      go to exe-pgm-frg-500.
       exe-pgm-frg-400.
      *                  *---------------------------------------------*
      *                  * Se sort non eseguito                        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Ciclo di report-program                 *
      *                      *-----------------------------------------*
           perform   prn-rou-pri-000      thru prn-rou-pri-999        .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     exe-pgm-frg-600.
       exe-pgm-frg-500.
      *                  *---------------------------------------------*
      *                  * Se sort eseguito                            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     exe-pgm-frg-600.
       exe-pgm-frg-600.
      *                  *---------------------------------------------*
      *                  * Visual. eventuali errori di esecuzione      *
      *                  *---------------------------------------------*
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
      *              * Se il contatore di cicli gia' eseguiti e' mag-  *
      *              * giore di zero : uscita                          *
      *              *-------------------------------------------------*
           if        w-cnt-ccl-gia-ese    >    zero
                     go to sel-prm-stp-999.
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
      *    * Lettura parametri di selezione stampa da segreteria       *
      *    *-----------------------------------------------------------*
       let-sel-stp-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-let-sel-stp      .
      *              *-------------------------------------------------*
      *              * Se il contatore di cicli gia' eseguiti e' mag-  *
      *              * giore di zero : uscita                          *
      *              *-------------------------------------------------*
           if        w-cnt-ccl-gia-ese    >    zero
                     go to let-sel-stp-999.
      *              *-------------------------------------------------*
      *              * Test se programma senza stampa                  *
      *              *-------------------------------------------------*
           if        w-cnt-fun-snx-stp    not  = "S"
                     go to let-sel-stp-999.
      *              *-------------------------------------------------*
      *              * Inizializzazione area parametri stampa          *
      *              *-------------------------------------------------*
           move      spaces               to   p-sel                  .
      *              *-------------------------------------------------*
      *              * Inizializzazione numero progressivo segmento    *
      *              *-------------------------------------------------*
           move      zero                 to   w-cnt-stu-num-seg      .
       let-sel-stp-100.
      *              *-------------------------------------------------*
      *              * Incremento numero progressivo segmento          *
      *              *-------------------------------------------------*
           add       1                    to   w-cnt-stu-num-seg      .
      *              *-------------------------------------------------*
      *              * Richiamo del modulo di segreteria per l'estra-  *
      *              * zione del segmento di parametri stampa          *
      *              *-------------------------------------------------*
           move      "S<"                 to   s-ope                  .
           move      w-cnt-stu-num-seg    to   s-num                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Concatenazione del segmento in area parametri   *
      *              * di stampa selezionati                           *
      *              *-------------------------------------------------*
           move      w-cnt-stu-num-seg    to   w-cnt-stu-pnt-stu      .
           multiply  80                   by   w-cnt-stu-pnt-stu      .
           subtract  79                   from w-cnt-stu-pnt-stu      .
           move      w-cnt-stu-pnt-stu    to   w-cnt-stu-sav-pnt      .
           string    s-alf
                     delimited by size    into p-sel
                                  with pointer w-cnt-stu-pnt-stu      .
      *              *-------------------------------------------------*
      *              * Se non si e' alla fine del record si ricicla    *
      *              *-------------------------------------------------*
           if        w-cnt-stu-pnt-stu    not  = w-cnt-stu-sav-pnt
                     go to let-sel-stp-100.
       let-sel-stp-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di report-program                                   *
      *    *-----------------------------------------------------------*
       prn-rou-pri-000.
      *              *-------------------------------------------------*
      *              * Inizializzazione markers                        *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-mrk-uno      .
      *              *-------------------------------------------------*
      *              * Inizializzazione area per rotture di livello    *
      *              *-------------------------------------------------*
           move      spaces               to   w-rot                  .
      *              *-------------------------------------------------*
      *              * Inizializzazione flag di interruzione forzata   *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
      *              *-------------------------------------------------*
      *              * Start iniziale                                  *
      *              *-------------------------------------------------*
           perform   prn-str-ini-000      thru prn-str-ini-999        .
           if        w-cnt-prn-flg-sub    not  = spaces
                     move   spaces        to   w-cnt-prn-flg-sub
                     go to  prn-rou-pri-600.
       prn-rou-pri-100.
      *              *-------------------------------------------------*
      *              * Salvataggio area rottura in area precedente     *
      *              *-------------------------------------------------*
           move      w-rot-l05            to   w-cnt-prn-sav-l05      .
           move      w-rot-l04            to   w-cnt-prn-sav-l04      .
           move      w-rot-l03            to   w-cnt-prn-sav-l03      .
           move      w-rot-l02            to   w-cnt-prn-sav-l02      .
           move      w-rot-l01            to   w-cnt-prn-sav-l01      .
       prn-rou-pri-200.
      *              *-------------------------------------------------*
      *              * Lettura sequenziale                             *
      *              *-------------------------------------------------*
           perform   prn-let-seq-000      thru prn-let-seq-999        .
           if        w-cnt-prn-flg-sub    not  = spaces
                     move   spaces        to   w-cnt-prn-flg-sub
                     go to  prn-rou-pri-500.
      *              *-------------------------------------------------*
      *              * Test se superamento limiti massimi              *
      *              *-------------------------------------------------*
           perform   prn-tst-max-000      thru prn-tst-max-999        .
           if        w-cnt-prn-flg-sub    not  = spaces
                     move   spaces        to   w-cnt-prn-flg-sub
                     go to  prn-rou-pri-500.
      *              *-------------------------------------------------*
      *              * Selezione su record letto                       *
      *              *-------------------------------------------------*
           perform   prn-sel-rec-000      thru prn-sel-rec-999        .
           if        w-cnt-prn-flg-sub    not  = spaces
                     move   spaces        to   w-cnt-prn-flg-sub
                     go to  prn-rou-pri-200.
      *              *-------------------------------------------------*
      *              * Composizione area per tests di rottura          *
      *              *-------------------------------------------------*
           perform   prn-cmp-rot-000      thru prn-cmp-rot-999        .
      *              *-------------------------------------------------*
      *              * Test se primo passaggio                         *
      *              *-------------------------------------------------*
           if        w-cnt-prn-mrk-uno    not  = spaces
                     go to prn-rou-pri-300.
      *                  *---------------------------------------------*
      *                  * Test se programma senza stampa              *
      *                  *---------------------------------------------*
           if        w-cnt-fun-snx-stp    not  = "S"
                     go to prn-rou-pri-250.
      *                  *---------------------------------------------*
      *                  * Test se Begin gia' eseguito                 *
      *                  *---------------------------------------------*
           if        w-cnt-prn-mrk-beg    not  = spaces
                     go to prn-rou-pri-250.
      *                      *-----------------------------------------*
      *                      * Begin                                   *
      *                      *-----------------------------------------*
           move      "BE"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Se errori                               *
      *                      *-----------------------------------------*
           if        p-rsc                not  = spaces
                     go to prn-rou-pri-900.
      *                      *-----------------------------------------*
      *                      * Marker di Begin eseguito                *
      *                      *-----------------------------------------*
           move      "#"                  to   w-cnt-prn-mrk-beg      .
       prn-rou-pri-250.
      *                  *---------------------------------------------*
      *                  * Inizio di tutti i livelli                   *
      *                  *---------------------------------------------*
           perform   prn-rou-pri-790      thru prn-rou-pri-791        .
           perform   prn-rou-pri-750      thru prn-rou-pri-751        .
           perform   prn-rou-pri-740      thru prn-rou-pri-741        .
           perform   prn-rou-pri-730      thru prn-rou-pri-731        .
           perform   prn-rou-pri-720      thru prn-rou-pri-721        .
           perform   prn-rou-pri-710      thru prn-rou-pri-711        .
           go to     prn-rou-pri-400.
       prn-rou-pri-300.
      *              *-------------------------------------------------*
      *              * Se rottura del 5. livello                       *
      *              *-------------------------------------------------*
           if        w-rot-l05            =    w-cnt-prn-sav-l05
                     go to prn-rou-pri-310.
           move      w-rot                to   w-cnt-prn-sav-rot      .
           move      w-cnt-prn-sav-l05    to   w-rot-l05              .
           move      w-cnt-prn-sav-l04    to   w-rot-l04              .
           move      w-cnt-prn-sav-l03    to   w-rot-l03              .
           move      w-cnt-prn-sav-l02    to   w-rot-l02              .
           move      w-cnt-prn-sav-l01    to   w-rot-l01              .
           perform   prn-rou-pri-810      thru prn-rou-pri-811        .
           perform   prn-rou-pri-820      thru prn-rou-pri-821        .
           perform   prn-rou-pri-830      thru prn-rou-pri-831        .
           perform   prn-rou-pri-840      thru prn-rou-pri-841        .
           perform   prn-rou-pri-850      thru prn-rou-pri-851        .
           move      w-cnt-prn-sav-rot    to   w-rot                  .
           perform   prn-rou-pri-750      thru prn-rou-pri-751        .
           perform   prn-rou-pri-740      thru prn-rou-pri-741        .
           perform   prn-rou-pri-730      thru prn-rou-pri-731        .
           perform   prn-rou-pri-720      thru prn-rou-pri-721        .
           perform   prn-rou-pri-710      thru prn-rou-pri-711        .
           go to     prn-rou-pri-400.
       prn-rou-pri-310.
      *              *-------------------------------------------------*
      *              * Se rottura del 4. livello                       *
      *              *-------------------------------------------------*
           if        w-rot-l04            =    w-cnt-prn-sav-l04
                     go to prn-rou-pri-320.
           move      w-rot                to   w-cnt-prn-sav-rot      .
           move      w-cnt-prn-sav-l04    to   w-rot-l04              .
           move      w-cnt-prn-sav-l03    to   w-rot-l03              .
           move      w-cnt-prn-sav-l02    to   w-rot-l02              .
           move      w-cnt-prn-sav-l01    to   w-rot-l01              .
           perform   prn-rou-pri-810      thru prn-rou-pri-811        .
           perform   prn-rou-pri-820      thru prn-rou-pri-821        .
           perform   prn-rou-pri-830      thru prn-rou-pri-831        .
           perform   prn-rou-pri-840      thru prn-rou-pri-841        .
           move      w-cnt-prn-sav-rot    to   w-rot                  .
           perform   prn-rou-pri-740      thru prn-rou-pri-741        .
           perform   prn-rou-pri-730      thru prn-rou-pri-731        .
           perform   prn-rou-pri-720      thru prn-rou-pri-721        .
           perform   prn-rou-pri-710      thru prn-rou-pri-711        .
           go to     prn-rou-pri-400.
       prn-rou-pri-320.
      *              *-------------------------------------------------*
      *              * Se rottura del 3. livello                       *
      *              *-------------------------------------------------*
           if        w-rot-l03            =    w-cnt-prn-sav-l03
                     go to prn-rou-pri-330.
           move      w-rot                to   w-cnt-prn-sav-rot      .
           move      w-cnt-prn-sav-l03    to   w-rot-l03              .
           move      w-cnt-prn-sav-l02    to   w-rot-l02              .
           move      w-cnt-prn-sav-l01    to   w-rot-l01              .
           perform   prn-rou-pri-810      thru prn-rou-pri-811        .
           perform   prn-rou-pri-820      thru prn-rou-pri-821        .
           perform   prn-rou-pri-830      thru prn-rou-pri-831        .
           move      w-cnt-prn-sav-rot    to   w-rot                  .
           perform   prn-rou-pri-730      thru prn-rou-pri-731        .
           perform   prn-rou-pri-720      thru prn-rou-pri-721        .
           perform   prn-rou-pri-710      thru prn-rou-pri-711        .
           go to     prn-rou-pri-400.
       prn-rou-pri-330.
      *              *-------------------------------------------------*
      *              * Se rottura del 2. livello                       *
      *              *-------------------------------------------------*
           if        w-rot-l02            =    w-cnt-prn-sav-l02
                     go to prn-rou-pri-340.
           move      w-rot                to   w-cnt-prn-sav-rot      .
           move      w-cnt-prn-sav-l02    to   w-rot-l02              .
           move      w-cnt-prn-sav-l01    to   w-rot-l01              .
           perform   prn-rou-pri-810      thru prn-rou-pri-811        .
           perform   prn-rou-pri-820      thru prn-rou-pri-821        .
           move      w-cnt-prn-sav-rot    to   w-rot                  .
           perform   prn-rou-pri-720      thru prn-rou-pri-721        .
           perform   prn-rou-pri-710      thru prn-rou-pri-711        .
           go to     prn-rou-pri-400.
       prn-rou-pri-340.
      *              *-------------------------------------------------*
      *              * Se rottura del 1. livello                       *
      *              *-------------------------------------------------*
           if        w-rot-l01            =    w-cnt-prn-sav-l01
                     go to prn-rou-pri-400.
           move      w-rot                to   w-cnt-prn-sav-rot      .
           move      w-cnt-prn-sav-l01    to   w-rot-l01              .
           perform   prn-rou-pri-810      thru prn-rou-pri-811        .
           move      w-cnt-prn-sav-rot    to   w-rot                  .
           perform   prn-rou-pri-710      thru prn-rou-pri-711        .
           go to     prn-rou-pri-400.
       prn-rou-pri-400.
      *              *-------------------------------------------------*
      *              * Se segnale di interruzione attivo : fine ciclo  *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-rou-pri-900.
      *              *-------------------------------------------------*
      *              * Esecuzione per il livello di dettaglio          *
      *              *-------------------------------------------------*
           perform   prn-liv-det-000      thru prn-liv-det-999        .
      *              *-------------------------------------------------*
      *              * Se segnale di interruzione attivo : fine ciclo  *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-rou-pri-900.
      *              *-------------------------------------------------*
      *              * Segnale di passaggio successivo al primo        *
      *              *-------------------------------------------------*
           move      "#"                  to   w-cnt-prn-mrk-uno      .
      *              *-------------------------------------------------*
      *              * Riciclo a lettura sequenziale file principale   *
      *              *-------------------------------------------------*
           go to     prn-rou-pri-100.
       prn-rou-pri-500.
      *              *-------------------------------------------------*
      *              * Test se almeno un passaggio                     *
      *              *-------------------------------------------------*
           if        w-cnt-prn-mrk-uno    =    spaces
                     go to prn-rou-pri-600.
           perform   prn-rou-pri-810      thru prn-rou-pri-811        .
           perform   prn-rou-pri-820      thru prn-rou-pri-821        .
           perform   prn-rou-pri-830      thru prn-rou-pri-831        .
           perform   prn-rou-pri-840      thru prn-rou-pri-841        .
           perform   prn-rou-pri-850      thru prn-rou-pri-851        .
           perform   prn-rou-pri-890      thru prn-rou-pri-891        .
           go to     prn-rou-pri-900.
       prn-rou-pri-600.
      *              *-------------------------------------------------*
      *              * Esecuzione per nessuna registrazione da elab.   *
      *              *-------------------------------------------------*
           perform   prn-nes-ela-000      thru prn-nes-ela-999        .
           go to     prn-rou-pri-900.
       prn-rou-pri-710.
      *              *-------------------------------------------------*
      *              * Esecuzione per inizio 1. livello di rottura     *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-rou-pri-711.
           perform   prn-ini-lr1-000      thru prn-ini-lr1-999        .
       prn-rou-pri-711.
           exit.
       prn-rou-pri-720.
      *              *-------------------------------------------------*
      *              * Esecuzione per inizio 2. livello di rottura     *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-rou-pri-721.
           perform   prn-ini-lr2-000      thru prn-ini-lr2-999        .
       prn-rou-pri-721.
           exit.
       prn-rou-pri-730.
      *              *-------------------------------------------------*
      *              * Esecuzione per inizio 3. livello di rottura     *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-rou-pri-731.
           perform   prn-ini-lr3-000      thru prn-ini-lr3-999        .
       prn-rou-pri-731.
           exit.
       prn-rou-pri-740.
      *              *-------------------------------------------------*
      *              * Esecuzione per inizio 4. livello di rottura     *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-rou-pri-741.
           perform   prn-ini-lr4-000      thru prn-ini-lr4-999        .
       prn-rou-pri-741.
           exit.
       prn-rou-pri-750.
      *              *-------------------------------------------------*
      *              * Esecuzione per inizio 5. livello di rottura     *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-rou-pri-751.
           perform   prn-ini-lr5-000      thru prn-ini-lr5-999        .
       prn-rou-pri-751.
           exit.
       prn-rou-pri-790.
      *              *-------------------------------------------------*
      *              * Esecuzione per inizio ciclo                     *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-rou-pri-791.
           perform   prn-ini-cic-000      thru prn-ini-cic-999        .
       prn-rou-pri-791.
           exit.
       prn-rou-pri-810.
      *              *-------------------------------------------------*
      *              * Esecuzione per fine 1. livello di rottura       *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-rou-pri-811.
           perform   prn-fin-lr1-000      thru prn-fin-lr1-999        .
       prn-rou-pri-811.
           exit.
       prn-rou-pri-820.
      *              *-------------------------------------------------*
      *              * Esecuzione per fine 2. livello di rottura       *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-rou-pri-821.
           perform   prn-fin-lr2-000      thru prn-fin-lr2-999        .
       prn-rou-pri-821.
           exit.
       prn-rou-pri-830.
      *              *-------------------------------------------------*
      *              * Esecuzione per fine 3. livello di rottura       *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-rou-pri-831.
           perform   prn-fin-lr3-000      thru prn-fin-lr3-999        .
       prn-rou-pri-831.
           exit.
       prn-rou-pri-840.
      *              *-------------------------------------------------*
      *              * Esecuzione per fine 4. livello di rottura       *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-rou-pri-841.
           perform   prn-fin-lr4-000      thru prn-fin-lr4-999        .
       prn-rou-pri-841.
           exit.
       prn-rou-pri-850.
      *              *-------------------------------------------------*
      *              * Esecuzione per fine 5. livello di rottura       *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-rou-pri-851.
           perform   prn-fin-lr5-000      thru prn-fin-lr5-999        .
       prn-rou-pri-851.
           exit.
       prn-rou-pri-890.
      *              *-------------------------------------------------*
      *              * Esecuzione per fine ciclo                       *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-rou-pri-891.
           perform   prn-fin-cic-000      thru prn-fin-cic-999        .
       prn-rou-pri-891.
           exit.
       prn-rou-pri-900.
      *              *-------------------------------------------------*
      *              * Uscita, senza End in quanto verra' eseguita e-  *
      *              * ventualmente nel main                           *
      *              *-------------------------------------------------*
           go to     prn-rou-pri-999.
       prn-rou-pri-999.
           exit.

      *    *===========================================================*
      *    * Intestazione pagina standard                              *
      *    *-----------------------------------------------------------*
       int-pag-std-000.
      *              *-------------------------------------------------*
      *              * Elaborazioni preliminari su aree titolo         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Determinazione lunghezza area nome azienda  *
      *                  *---------------------------------------------*
           move      "IA"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-asx                to   w-cnt-tit-des-azi      .
           move      40                   to   w-cnt-tit-ctr-azi      .
       int-pag-std-100.
           if        w-cnt-tit-chr-azi
                    (w-cnt-tit-ctr-azi)   =    spaces
                     if     w-cnt-tit-ctr-azi
                                          >    1
                            subtract  1   from w-cnt-tit-ctr-azi
                            go to     int-pag-std-100.
      *                  *---------------------------------------------*
      *                  * Determinazione lunghezza titolo stampato    *
      *                  *---------------------------------------------*
           move      80                   to   w-cnt-tit-ctr-tit      .
       int-pag-std-200.
           if        w-cnt-tit-chr-tit
                    (w-cnt-tit-ctr-tit)   =    spaces
                     if     w-cnt-tit-ctr-tit
                                          >    1
                            subtract  1   from w-cnt-tit-ctr-tit
                            go to     int-pag-std-200.
      *                  *---------------------------------------------*
      *                  * Determinazione posizione iniziale titolo    *
      *                  *---------------------------------------------*
           move      p-sel-als-sel        to   w-cnt-tit-pos-tit      .
           subtract  w-cnt-tit-ctr-tit    from w-cnt-tit-pos-tit      .
           divide    2                    into w-cnt-tit-pos-tit      .
           add       1                    to   w-cnt-tit-pos-tit      .
      *                  *---------------------------------------------*
      *                  * Determinazione lunghezza area data e pagina *
      *                  *---------------------------------------------*
           if        w-cnt-tit-num-pag    =    zero
                     move  4              to   w-cnt-tit-ctr-wrk
                     go to int-pag-std-300.
           move      zero                 to   w-cnt-tit-ctr-wrk      .
           inspect   w-cnt-tit-num-pag
                                      tallying w-cnt-tit-ctr-wrk
                                   for leading "0"                    .
       int-pag-std-300.
           subtract  w-cnt-tit-ctr-wrk    from 27
                                        giving w-cnt-tit-ctr-dep      .
           subtract  w-cnt-tit-ctr-wrk    from 5
                                        giving w-cnt-tit-ctr-cif      .
      *                  *---------------------------------------------*
      *                  * Determinazione posizione iniziale data e p. *
      *                  *---------------------------------------------*
           move      p-sel-als-sel        to   w-cnt-tit-pos-dep      .
           subtract  w-cnt-tit-ctr-dep    from w-cnt-tit-pos-dep      .
           add       1                    to   w-cnt-tit-pos-dep      .
      *                  *---------------------------------------------*
      *                  * Determinazione se titolo su una o due linee *
      *                  *---------------------------------------------*
           move      w-cnt-tit-ctr-azi    to   w-cnt-tit-ctr-wrk      .
           add       2                    to   w-cnt-tit-ctr-wrk      .
           if        w-cnt-tit-ctr-wrk    not  < w-cnt-tit-pos-tit
                     move  2              to   w-cnt-tit-num-lin
                     go to int-pag-std-500.
           move      w-cnt-tit-pos-tit    to   w-cnt-tit-ctr-wrk      .
           add       w-cnt-tit-ctr-tit    to   w-cnt-tit-ctr-wrk      .
           add       1                    to   w-cnt-tit-ctr-wrk      .
           if        w-cnt-tit-ctr-wrk    not  < w-cnt-tit-pos-dep
                     move  2              to   w-cnt-tit-num-lin
                     go to int-pag-std-500.
           move      1                    to   w-cnt-tit-num-lin      .
       int-pag-std-500.
      *              *-------------------------------------------------*
      *              * Avanzamento pagina                              *
      *              *-------------------------------------------------*
           move      "PA"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Se errore grave di i-o su stampa si esce    *
      *                  * con status di errore                        *
      *                  *---------------------------------------------*
           if        p-rsc                not  = spaces
                     move   "#"           to   w-cnt-prn-flg-int
                     go to  int-pag-std-999.
      *              *-------------------------------------------------*
      *              * Linea di '=' iniziale                           *
      *              *-------------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      p-sel-als-sel        to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           move      all   "="            to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *              *-------------------------------------------------*
      *              * Prima linea titolo                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Nome azienda                                *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      w-cnt-tit-ctr-azi    to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           move      w-cnt-tit-des-azi    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Titolo stampato                             *
      *                  *---------------------------------------------*
           if        w-cnt-tit-num-lin    =    2
                     go to int-pag-std-600.
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      w-cnt-tit-ctr-tit    to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      w-cnt-tit-pos-tit    to   p-pos                  .
           move      w-cnt-tit-des-tit    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       int-pag-std-600.
      *                  *---------------------------------------------*
      *                  * Literal per Data                            *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      06                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      w-cnt-tit-pos-dep    to   p-pos                  .
           move      "Data :"             to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Data                                        *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "D"                  to   p-tip                  .
           move      p-lnr                to   p-lin                  .
           move      w-cnt-tit-pos-dep    to   p-pos                  .
           add       7                    to   p-pos                  .
           move      w-cnt-tit-dat-stp    to   p-dat                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Literal per Pag.                            *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      04                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      w-cnt-tit-pos-dep    to   p-pos                  .
           add       17                   to   p-pos                  .
           move      "Pag."               to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Numero pagina                               *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      w-cnt-tit-ctr-cif    to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      p-lnr                to   p-lin                  .
           move      w-cnt-tit-pos-dep    to   p-pos                  .
           add       22                   to   p-pos                  .
           move      w-cnt-tit-num-pag    to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *              *-------------------------------------------------*
      *              * Seconda linea titolo                            *
      *              *-------------------------------------------------*
           if        w-cnt-tit-num-lin    not  = 2
                     go to int-pag-std-900.
      *                  *---------------------------------------------*
      *                  * Titolo stampato                             *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      w-cnt-tit-ctr-tit    to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      w-cnt-tit-pos-tit    to   p-pos                  .
           move      w-cnt-tit-des-tit    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       int-pag-std-900.
      *              *-------------------------------------------------*
      *              * Linea di '-' finale                             *
      *              *-------------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      p-sel-als-sel        to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           move      all   "-"            to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "VP"                 to   p-ope                  .
           add       2
                     p-lnr              giving p-lin                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       int-pag-std-999.
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
      *              * Si/No funzionamento ciclico stampa              *
      *              *-------------------------------------------------*
           move      "S"                  to   w-cnt-fun-snx-cic      .
      *              *-------------------------------------------------*
      *              * Si/No richiesta di selezione stampa             *
      *              *-------------------------------------------------*
           move      "S"                  to   w-cnt-fun-snx-stp      .
       pre-tip-fun-999.
           exit.

      *    *===========================================================*
      *    * Open files per richieste ed esecuzione                    *
      *    *-----------------------------------------------------------*
       rou-opn-fls-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-rou-opn-fls      .
       rou-opn-fls-200.
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice prodotto 'dcp'  *
      *              *-------------------------------------------------*
           perform   cod-cod-dcp-opn-000  thru cod-cod-dcp-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice semilavorato    *
      *              *-------------------------------------------------*
           perform   cod-cod-dps-opn-000  thru cod-cod-dps-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice sub-distinta    *
      *              *-------------------------------------------------*
           perform   cod-cod-lgv-opn-000  thru cod-cod-lgv-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice materia prima   *
      *              *-------------------------------------------------*
           perform   cod-cod-dpm-opn-000  thru cod-cod-dpm-opn-999    .
       rou-opn-fls-400.
      *              *-------------------------------------------------*
      *              * [lgt]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dtp/fls/ioc/obj/ioflgt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lgt                 .
      *              *-------------------------------------------------*
      *              * [lgr]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dtp/fls/ioc/obj/ioflgr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lgr                 .
      *              *-------------------------------------------------*
      *              * [dcp]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
      *              *-------------------------------------------------*
      *              * [dps]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dps/fls/ioc/obj/iofdps"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dps                 .
      *              *-------------------------------------------------*
      *              * [lgv]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dtp/fls/ioc/obj/ioflgv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lgv                 .
      *              *-------------------------------------------------*
      *              * [dpm]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dpm/fls/ioc/obj/iofdpm"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dpm                 .
       rou-opn-fls-800.
      *              *-------------------------------------------------*
      *              * Open modulo di determinazione valore unitario   *
      *              * di magazzino                                    *
      *              *-------------------------------------------------*
           move      "OP"                 to   d-vun-mag-tip-ope      .
           move      "pgm/mag/prg/obj/pmag300v"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using d-vun-mag              .
       rou-opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Close files per richieste ed esecuzione                   *
      *    *-----------------------------------------------------------*
       rou-cls-fls-000.
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice prodotto 'dcp' *
      *              *-------------------------------------------------*
           perform   cod-cod-dcp-cls-000  thru cod-cod-dcp-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice semilavorato   *
      *              *-------------------------------------------------*
           perform   cod-cod-dps-cls-000  thru cod-cod-dps-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice sub-distinta   *
      *              *-------------------------------------------------*
           perform   cod-cod-lgv-cls-000  thru cod-cod-lgv-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice materia prima  *
      *              *-------------------------------------------------*
           perform   cod-cod-dpm-cls-000  thru cod-cod-dpm-cls-999    .
       rou-cls-fls-400.
      *              *-------------------------------------------------*
      *              * [lgt]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dtp/fls/ioc/obj/ioflgt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lgt                 .
      *              *-------------------------------------------------*
      *              * [lgr]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dtp/fls/ioc/obj/ioflgr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lgr                 .
      *              *-------------------------------------------------*
      *              * [dcp]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
      *              *-------------------------------------------------*
      *              * [dps]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dps/fls/ioc/obj/iofdps"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dps                 .
      *              *-------------------------------------------------*
      *              * [lgv]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dtp/fls/ioc/obj/ioflgv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lgv                 .
      *              *-------------------------------------------------*
      *              * [dpm]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dpm/fls/ioc/obj/iofdpm"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dpm                 .
       rou-cls-fls-800.
      *              *-------------------------------------------------*
      *              * Close modulo di determinazione valore di magazz.*
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Chiusura modulo                             *
      *                  *---------------------------------------------*
           move      "CL"                 to   d-vun-mag-tip-ope      .
           move      "pgm/mag/prg/obj/pmag300v"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using d-vun-mag              .
      *                  *---------------------------------------------*
      *                  * Test se cancellabile                        *
      *                  *---------------------------------------------*
           move      "C?"                 to   d-vun-mag-tip-ope      .
           move      "pgm/mag/prg/obj/pmag300v"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using d-vun-mag              .
           if        d-vun-mag-exi-sts    not  = spaces
                     go to rou-cls-fls-999.
      *                  *---------------------------------------------*
      *                  * Cancellazione modulo                        *
      *                  *---------------------------------------------*
           move      "pgm/mag/prg/obj/pmag300v"
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
      *                  * Data per valorizzazione                     *
      *                  *---------------------------------------------*
           perform   acc-dat-val-000      thru acc-dat-val-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
       acc-ric-sel-200.
      *                  *---------------------------------------------*
      *                  * Tipo distinta                               *
      *                  *---------------------------------------------*
           perform   acc-tip-dis-000      thru acc-tip-dis-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-100.
       acc-ric-sel-300.
      *                  *---------------------------------------------*
      *                  * Codice assieme                              *
      *                  *---------------------------------------------*
           perform   acc-cod-ass-000      thru acc-cod-ass-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-200.
       acc-ric-sel-400.
      *                  *---------------------------------------------*
      *                  * Tipo valorizzazione per materie prime       *
      *                  *---------------------------------------------*
           perform   acc-tva-map-000      thru acc-tva-map-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-300.
       acc-ric-sel-500.
      *                  *---------------------------------------------*
      *                  * Tipo valorizzazione per semilavorati        *
      *                  *---------------------------------------------*
           perform   acc-tva-sem-000      thru acc-tva-sem-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-400.
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
      *    * Normalizzazione richieste di selezione                    *
      *    *-----------------------------------------------------------*
       nor-ric-sel-000.
      *              *-------------------------------------------------*
      *              * Test se primo ciclo di esecuzione               *
      *              *-------------------------------------------------*
           if        w-cnt-ccl-gia-ese    >    zero
                     go to nor-ric-sel-999.
      *              *-------------------------------------------------*
      *              * Normalizzazione richieste                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Data per valorizzazione                     *
      *                  *---------------------------------------------*
           move      zero                 to   rr-dat-val             .
      *                  *---------------------------------------------*
      *                  * Tipo di distinta                            *
      *                  *---------------------------------------------*
           move      zero                 to   rr-tip-dis             .
      *                  *---------------------------------------------*
      *                  * Codice assieme                              *
      *                  *---------------------------------------------*
           move      zero                 to   rr-num-ass             .
           move      spaces               to   rr-alf-ass             .
           move      spaces               to   rr-des-ass             .
           move      spaces               to   rr-umi-ass             .
      *                  *---------------------------------------------*
      *                  * Tipo di valorizzazione per le materie prime *
      *                  * incluse in distinta                         *
      *                  *---------------------------------------------*
           move      zero                 to   rr-tva-map             .
      *                  *---------------------------------------------*
      *                  * Tipo di valorizzazione per i semilavorati   *
      *                  * finali inclusi in distinta                  *
      *                  *---------------------------------------------*
           move      zero                 to   rr-tva-sem             .
       nor-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per richieste di selezione        *
      *    *-----------------------------------------------------------*
       pmt-ric-sel-000.
      *              *-------------------------------------------------*
      *              * Prompts                                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Data per valorizzazione                     *
      *                  *---------------------------------------------*
           perform   pmt-dat-val-000      thru pmt-dat-val-999        .
      *                  *---------------------------------------------*
      *                  * Tipo distinta                               *
      *                  *---------------------------------------------*
           perform   pmt-tip-dis-000      thru pmt-tip-dis-999        .
      *                  *---------------------------------------------*
      *                  * Codice assieme                              *
      *                  *---------------------------------------------*
           perform   pmt-cod-ass-000      thru pmt-cod-ass-999        .
      *                  *---------------------------------------------*
      *                  * Tipo di valorizzazione per le materie prime *
      *                  *---------------------------------------------*
           perform   pmt-tva-map-000      thru pmt-tva-map-999        .
      *                  *---------------------------------------------*
      *                  * Tipo di valorizzazione per i semilavorati   *
      *                  *---------------------------------------------*
           perform   pmt-tva-sem-000      thru pmt-tva-sem-999        .
       pmt-ric-sel-200.
      *              *-------------------------------------------------*
      *              * Se questo e' il primo ciclo di esecuzione : si  *
      *              * esce                                            *
      *              *-------------------------------------------------*
           if        w-cnt-ccl-gia-ese    =    zero
                     go to pmt-ric-sel-999.
       pmt-ric-sel-400.
      *              *-------------------------------------------------*
      *              * Visualizzazione valori                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Data per valorizzazione                     *
      *                  *---------------------------------------------*
           perform   vis-dat-val-000      thru vis-dat-val-999        .
      *                  *---------------------------------------------*
      *                  * Tipo distinta                               *
      *                  *---------------------------------------------*
           perform   vis-tip-dis-000      thru vis-tip-dis-999        .
      *                  *---------------------------------------------*
      *                  * Codice assieme                              *
      *                  *---------------------------------------------*
           perform   vis-cod-ass-000      thru vis-cod-ass-999        .
      *                  *---------------------------------------------*
      *                  * Tipo di valorizzazione per le materie prime *
      *                  *---------------------------------------------*
           perform   vis-tva-map-000      thru vis-tva-map-999        .
      *                  *---------------------------------------------*
      *                  * Tipo di valorizzazione per i semilavorati   *
      *                  *---------------------------------------------*
           perform   vis-tva-sem-000      thru vis-tva-sem-999        .
       pmt-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Data per valorizzazione                          *
      *    *-----------------------------------------------------------*
       pmt-dat-val-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      24                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Valorizzazione al      :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-dat-val-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Tipo di distinta                                 *
      *    *-----------------------------------------------------------*
       pmt-tip-dis-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      24                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tipo di distinta       :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-tip-dis-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Codice assieme                                   *
      *    *-----------------------------------------------------------*
       pmt-cod-ass-000.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda del tipo di distinta       *
      *              *-------------------------------------------------*
           if        rr-tip-dis           =    01
                     go to pmt-cod-ass-100
           else if   rr-tip-dis           =    02
                     go to pmt-cod-ass-200
           else if   rr-tip-dis           =    99
                     go to pmt-cod-ass-300
           else      go to pmt-cod-ass-900.
       pmt-cod-ass-100.
      *              *-------------------------------------------------*
      *              * Se distinta per prodotto finito                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      24                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice prodotto finito :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           go to     pmt-cod-ass-999.
       pmt-cod-ass-200.
      *              *-------------------------------------------------*
      *              * Se distinta per semilavorato                    *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      24                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice semilavorato    :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           go to     pmt-cod-ass-999.
       pmt-cod-ass-300.
      *              *-------------------------------------------------*
      *              * Se sub-distinta virtuale                        *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      24                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice sub-distinta    :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           go to     pmt-cod-ass-999.
       pmt-cod-ass-900.
      *              *-------------------------------------------------*
      *              * Se tipo distinta indeterminato                  *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      24                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           go to     pmt-cod-ass-999.
       pmt-cod-ass-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Tipo di valorizzazione per le materie prime      *
      *    *-----------------------------------------------------------*
       pmt-tva-map-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      24                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tipo di valorizzazione :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      24                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "  per le materie prime  "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      24                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "   incluse in distinta  "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-tva-map-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Tipo di valorizzazione per i semilavorati        *
      *    *-----------------------------------------------------------*
       pmt-tva-sem-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      24                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tipo di valorizzazione :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      24                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "    per i semilavorati  "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      24                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "     finali inclusi in  "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      24                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "              distinta  "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-tva-sem-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Data per valorizzazione                    *
      *    *-----------------------------------------------------------*
       acc-dat-val-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-dat-val-025.
      *                  *---------------------------------------------*
      *                  * Eventuale preparazione del valore di de-    *
      *                  * fault                                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se valore attuale a zero           *
      *                      *-----------------------------------------*
           if        rr-dat-val           not  zero
                     go to acc-dat-val-100.
      *                      *-----------------------------------------*
      *                      * Data da segreteria                      *
      *                      *-----------------------------------------*
           move      "DT"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-dat                to   rr-dat-val             .
       acc-dat-val-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      05                   to   v-lin                  .
           move      26                   to   v-pos                  .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-dat-val           to   v-dat                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
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
       acc-dat-val-425.
      *                  *---------------------------------------------*
      *                  * Valore zero non ammesso, a meno che non si  *
      *                  * sia in Up                                   *
      *                  *---------------------------------------------*
           if        rr-dat-val           not  = zero
                     go to acc-dat-val-450.
           if        v-key                =    "UP  "
                     go to acc-dat-val-450
           else      go to acc-dat-val-600.
       acc-dat-val-450.
      *                  *---------------------------------------------*
      *                  * Fine controlli                              *
      *                  *---------------------------------------------*
           go to     acc-dat-val-600.
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
      *    * Visualizzazzione : Data per valorizzazione                *
      *    *-----------------------------------------------------------*
       vis-dat-val-000.
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      05                   to   v-lin                  .
           move      26                   to   v-pos                  .
           move      rr-dat-val           to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dat-val-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo selezione : Tipo di distinta           *
      *    *-----------------------------------------------------------*
       acc-tip-dis-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-tip-dis-025.
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      rr-tip-dis           to   w-sav-tip-dis          .
       acc-tip-dis-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-dis-lun    to   v-car                  .
           move      w-exp-tip-dis-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      07                   to   v-lin                  .
           move      26                   to   v-pos                  .
           move      w-exp-tip-dis-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           if        rr-tip-dis           =    01
                     move  01             to   v-num
           else if   rr-tip-dis           =    02
                     move  02             to   v-num
           else if   rr-tip-dis           =    99
                     move  03             to   v-num
           else      move  zero           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-tip-dis-999.
       acc-tip-dis-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        v-num                =    01
                     move  01             to   rr-tip-dis
           else if   v-num                =    02
                     move  02             to   rr-tip-dis
           else if   v-num                =    03
                     move  99             to   rr-tip-dis
           else      move  zero           to   rr-tip-dis             .
       acc-tip-dis-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-tip-dis-425.
      *                  *---------------------------------------------*
      *                  * Valore zero non ammesso, a meno che non si  *
      *                  * sia in Up                                   *
      *                  *---------------------------------------------*
           if        rr-tip-dis           not  = zero
                     go to acc-tip-dis-450.
           if        v-key                =    "UP  "
                     go to acc-tip-dis-450
           else      go to acc-tip-dis-600.
       acc-tip-dis-450.
      *                  *---------------------------------------------*
      *                  * Fine controlli                              *
      *                  *---------------------------------------------*
           go to     acc-tip-dis-600.
       acc-tip-dis-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-tip-dis-620.
      *                  *---------------------------------------------*
      *                  * Se valore immutato rispetto al valore pre-  *
      *                  * cedente : nessuna dipendenza                *
      *                  *---------------------------------------------*
           if        rr-tip-dis           =    w-sav-tip-dis
                     go to acc-tip-dis-800.
       acc-tip-dis-640.
      *                  *---------------------------------------------*
      *                  * Se valore precedente indeterminato : solo   *
      *                  * visualizzazione prompt                      *
      *                  *---------------------------------------------*
           if        w-sav-tip-dis        not  = zero
                     go to acc-tip-dis-660.
           perform   pmt-cod-ass-000      thru pmt-cod-ass-999        .
           go to     acc-tip-dis-800.
       acc-tip-dis-660.
      *                  *---------------------------------------------*
      *                  * Normalizzazione per codice assieme          *
      *                  *---------------------------------------------*
           move      zero                 to   rr-num-ass             .
           move      spaces               to   rr-alf-ass             .
           move      spaces               to   rr-des-ass             .
           move      spaces               to   rr-umi-ass             .
      *                  *---------------------------------------------*
      *                  * Video in Off                                *
      *                  *---------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione prompt                      *
      *                  *---------------------------------------------*
           perform   pmt-cod-ass-000      thru pmt-cod-ass-999        .
      *                  *---------------------------------------------*
      *                  * Visualizzazioni per codice assieme          *
      *                  *---------------------------------------------*
           perform   vis-cod-ass-000      thru vis-cod-ass-999        .
      *                  *---------------------------------------------*
      *                  * Video in On                                 *
      *                  *---------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-tip-dis-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-tip-dis-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-tip-dis-100.
       acc-tip-dis-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazzione : Tipo di distinta                       *
      *    *-----------------------------------------------------------*
       vis-tip-dis-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-dis-lun    to   v-car                  .
           move      w-exp-tip-dis-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      07                   to   v-lin                  .
           move      26                   to   v-pos                  .
           move      w-exp-tip-dis-tbl    to   v-txt                  .
           if        rr-tip-dis           =    01
                     move  01             to   v-num
           else if   rr-tip-dis           =    02
                     move  02             to   v-num
           else if   rr-tip-dis           =    99
                     move  03             to   v-num
           else      move  zero           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tip-dis-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Codice assieme                             *
      *    *-----------------------------------------------------------*
       acc-cod-ass-000.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda del tipo di distinta       *
      *              *-------------------------------------------------*
           if        rr-tip-dis           =    01
                     go to acc-cod-ass-100
           else if   rr-tip-dis           =    02
                     go to acc-cod-ass-200
           else if   rr-tip-dis           =    99
                     go to acc-cod-ass-300
           else      go to acc-cod-ass-900.
       acc-cod-ass-100.
      *              *-------------------------------------------------*
      *              * Se distinta per prodotto finito                 *
      *              *-------------------------------------------------*
           perform   acc-cod-dcp-000      thru acc-cod-dcp-999        .
           go to     acc-cod-ass-999.
       acc-cod-ass-200.
      *              *-------------------------------------------------*
      *              * Se distinta per semilavorato                    *
      *              *-------------------------------------------------*
           perform   acc-cod-dps-000      thru acc-cod-dps-999        .
           go to     acc-cod-ass-999.
       acc-cod-ass-300.
      *              *-------------------------------------------------*
      *              * Se sub-distinta virtuale                        *
      *              *-------------------------------------------------*
           perform   acc-cod-lgv-000      thru acc-cod-lgv-999        .
           go to     acc-cod-ass-999.
       acc-cod-ass-900.
      *              *-------------------------------------------------*
      *              * Se tipo distinta indeterminato                  *
      *              *-------------------------------------------------*
           go to     acc-cod-ass-999.
       acc-cod-ass-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Codice prodotto finito                     *
      *    *-----------------------------------------------------------*
       acc-cod-dcp-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cod-dcp-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-cod-dcp-ope      .
           move      "A"                  to   w-cod-cod-dcp-tac      .
           move      rr-num-ass           to   w-cod-cod-dcp-num      .
           move      rr-alf-ass           to   w-cod-cod-dcp-alf      .
           move      09                   to   w-cod-cod-dcp-lin      .
           move      26                   to   w-cod-cod-dcp-pos      .
           move      09                   to   w-cod-cod-dcp-dln      .
           move      41                   to   w-cod-cod-dcp-dps      .
           move      spaces               to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-cod-dcp-cll-000  thru cod-cod-dcp-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-cod-dcp-foi-000  thru cod-cod-dcp-foi-999    .
       acc-cod-dcp-110.
           perform   cod-cod-dcp-cll-000  thru cod-cod-dcp-cll-999    .
           if        w-cod-cod-dcp-ope    =    "F+"
                     go to acc-cod-dcp-115.
           if        w-cod-cod-dcp-ope    =    "AC"
                     go to acc-cod-dcp-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-dcp-115.
           perform   cod-cod-dcp-foi-000  thru cod-cod-dcp-foi-999    .
           go to     acc-cod-dcp-110.
       acc-cod-dcp-120.
           move      w-cod-cod-dcp-num    to   v-num                  .
           move      w-cod-cod-dcp-alf    to   v-alf                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-cod-dcp-999.
       acc-cod-dcp-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-num-ass             .
           move      v-alf                to   rr-alf-ass             .
       acc-cod-dcp-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-cod-dcp-420.
      *                  *---------------------------------------------*
      *                  * Lettura archivio [dcp]                      *
      *                  *---------------------------------------------*
           move      rr-num-ass           to   w-let-arc-dcp-num      .
           perform   let-arc-dcp-000      thru let-arc-dcp-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione descrizione e unita' di mi-  *
      *                  * sura assieme                                *
      *                  *---------------------------------------------*
           move      w-let-arc-dcp-des    to   rr-des-ass             .
           move      w-let-arc-dcp-umi    to   rr-umi-ass             .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione                 *
      *                  *---------------------------------------------*
           perform   vis-des-ass-000      thru vis-des-ass-999        .
      *                  *---------------------------------------------*
      *                  * Se valore non trovato : reimpostazione      *
      *                  *---------------------------------------------*
           if        w-let-arc-dcp-flg    not  = spaces
                     go to acc-cod-dcp-100.
      *                  *---------------------------------------------*
      *                  * Se valore a spaces : reimpostazione a meno  *
      *                  * che non sia stato premuto 'Up'              *
      *                  *---------------------------------------------*
           if        rr-alf-ass           not  = spaces
                     go to acc-cod-dcp-440.
           if        v-key                =    "UP  "
                     go to acc-cod-dcp-600
           else      go to acc-cod-dcp-100.
       acc-cod-dcp-440.
      *                  *---------------------------------------------*
      *                  * Test che la distinta base per il prodotto   *
      *                  * finito esista in archivio [lgt]             *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Lettura anagrafica [lgt]                *
      *                      *-----------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "TNMASS"             to   f-key                  .
           move      01                   to   rf-lgt-tpm-ass         .
           move      rr-num-ass           to   rf-lgt-nrm-ass         .
           move      "pgm/dtp/fls/ioc/obj/ioflgt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lgt                 .
      *                      *-----------------------------------------*
      *                      * Test di esistenza                       *
      *                      *-----------------------------------------*
           if        f-sts                =    e-not-err
                     go to acc-cod-dcp-460.
      *                      *-----------------------------------------*
      *                      * Messaggio di errore                     *
      *                      *-----------------------------------------*
           move      "Non esiste una distinta base per il prodotto finit
      -              "o !            "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                      *-----------------------------------------*
      *                      * A reimpostazione                        *
      *                      *-----------------------------------------*
           go to     acc-cod-dcp-100.
       acc-cod-dcp-460.
      *                  *---------------------------------------------*
      *                  * Fine controlli                              *
      *                  *---------------------------------------------*
           go to     acc-cod-dcp-600.
       acc-cod-dcp-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cod-dcp-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-cod-dcp-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-cod-dcp-100.
       acc-cod-dcp-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Codice semilavorato                        *
      *    *-----------------------------------------------------------*
       acc-cod-dps-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cod-dps-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-cod-dps-ope      .
           move      "A"                  to   w-cod-cod-dps-tac      .
           move      rr-num-ass           to   w-cod-cod-dps-num      .
           move      rr-alf-ass           to   w-cod-cod-dps-alf      .
           move      09                   to   w-cod-cod-dps-lin      .
           move      26                   to   w-cod-cod-dps-pos      .
           move      09                   to   w-cod-cod-dps-dln      .
           move      41                   to   w-cod-cod-dps-dps      .
           move      spaces               to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-cod-dps-cll-000  thru cod-cod-dps-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-cod-dps-foi-000  thru cod-cod-dps-foi-999    .
       acc-cod-dps-110.
           perform   cod-cod-dps-cll-000  thru cod-cod-dps-cll-999    .
           if        w-cod-cod-dps-ope    =    "F+"
                     go to acc-cod-dps-115.
           if        w-cod-cod-dps-ope    =    "AC"
                     go to acc-cod-dps-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-dps-115.
           perform   cod-cod-dps-foi-000  thru cod-cod-dps-foi-999    .
           go to     acc-cod-dps-110.
       acc-cod-dps-120.
           move      w-cod-cod-dps-num    to   v-num                  .
           move      w-cod-cod-dps-alf    to   v-alf                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-cod-dps-999.
       acc-cod-dps-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-num-ass             .
           move      v-alf                to   rr-alf-ass             .
       acc-cod-dps-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-cod-dps-420.
      *                  *---------------------------------------------*
      *                  * Lettura archivio [dps]                      *
      *                  *---------------------------------------------*
           move      rr-num-ass           to   w-let-arc-dps-num      .
           perform   let-arc-dps-000      thru let-arc-dps-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione descrizione e unita' di mi-  *
      *                  * sura assieme                                *
      *                  *---------------------------------------------*
           move      w-let-arc-dps-des    to   rr-des-ass             .
           move      w-let-arc-dps-umi    to   rr-umi-ass             .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione                 *
      *                  *---------------------------------------------*
           perform   vis-des-ass-000      thru vis-des-ass-999        .
      *                  *---------------------------------------------*
      *                  * Se valore non trovato : reimpostazione      *
      *                  *---------------------------------------------*
           if        w-let-arc-dps-flg    not  = spaces
                     go to acc-cod-dps-100.
      *                  *---------------------------------------------*
      *                  * Se valore a spaces : reimpostazione a meno  *
      *                  * che non sia stato premuto 'Up'              *
      *                  *---------------------------------------------*
           if        rr-alf-ass           not  = spaces
                     go to acc-cod-dps-440.
           if        v-key                =    "UP  "
                     go to acc-cod-dps-600
           else      go to acc-cod-dps-100.
       acc-cod-dps-440.
      *                  *---------------------------------------------*
      *                  * Test che la distinta base per il semilavo-  *
      *                  * rato esista in archivio [lgt]               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Lettura anagrafica [lgt]                *
      *                      *-----------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "TNMASS"             to   f-key                  .
           move      02                   to   rf-lgt-tpm-ass         .
           move      rr-num-ass           to   rf-lgt-nrm-ass         .
           move      "pgm/dtp/fls/ioc/obj/ioflgt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lgt                 .
      *                      *-----------------------------------------*
      *                      * Test di esistenza                       *
      *                      *-----------------------------------------*
           if        f-sts                =    e-not-err
                     go to acc-cod-dps-460.
      *                      *-----------------------------------------*
      *                      * Messaggio di errore                     *
      *                      *-----------------------------------------*
           move      "Non esiste una distinta base per il semilavorato !
      -              "               "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                      *-----------------------------------------*
      *                      * A reimpostazione                        *
      *                      *-----------------------------------------*
           go to     acc-cod-dps-100.
       acc-cod-dps-460.
      *                  *---------------------------------------------*
      *                  * Fine controlli                              *
      *                  *---------------------------------------------*
           go to     acc-cod-dps-600.
       acc-cod-dps-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cod-dps-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-cod-dps-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-cod-dps-100.
       acc-cod-dps-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Codice sub-distinta virtuale               *
      *    *-----------------------------------------------------------*
       acc-cod-lgv-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cod-lgv-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-cod-lgv-ope      .
           move      "A"                  to   w-cod-cod-lgv-tac      .
           move      rr-num-ass           to   w-cod-cod-lgv-num      .
           move      rr-alf-ass           to   w-cod-cod-lgv-alf      .
           move      09                   to   w-cod-cod-lgv-lin      .
           move      26                   to   w-cod-cod-lgv-pos      .
           move      09                   to   w-cod-cod-lgv-dln      .
           move      41                   to   w-cod-cod-lgv-dps      .
           move      spaces               to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-cod-lgv-cll-000  thru cod-cod-lgv-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-cod-lgv-foi-000  thru cod-cod-lgv-foi-999    .
       acc-cod-lgv-110.
           perform   cod-cod-lgv-cll-000  thru cod-cod-lgv-cll-999    .
           if        w-cod-cod-lgv-ope    =    "F+"
                     go to acc-cod-lgv-115.
           if        w-cod-cod-lgv-ope    =    "AC"
                     go to acc-cod-lgv-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-lgv-115.
           perform   cod-cod-lgv-foi-000  thru cod-cod-lgv-foi-999    .
           go to     acc-cod-lgv-110.
       acc-cod-lgv-120.
           move      w-cod-cod-lgv-num    to   v-num                  .
           move      w-cod-cod-lgv-alf    to   v-alf                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-cod-lgv-999.
       acc-cod-lgv-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-num-ass             .
           move      v-alf                to   rr-alf-ass             .
       acc-cod-lgv-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-cod-lgv-420.
      *                  *---------------------------------------------*
      *                  * Lettura archivio [lgv]                      *
      *                  *---------------------------------------------*
           move      rr-num-ass           to   w-let-arc-lgv-num      .
           perform   let-arc-lgv-000      thru let-arc-lgv-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione descrizione e unita' di mi-  *
      *                  * sura assieme                                *
      *                  *---------------------------------------------*
           move      w-let-arc-lgv-des    to   rr-des-ass             .
           move      w-let-arc-lgv-umi    to   rr-umi-ass             .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione                 *
      *                  *---------------------------------------------*
           perform   vis-des-ass-000      thru vis-des-ass-999        .
      *                  *---------------------------------------------*
      *                  * Se valore non trovato : reimpostazione      *
      *                  *---------------------------------------------*
           if        w-let-arc-lgv-flg    not  = spaces
                     go to acc-cod-lgv-100.
      *                  *---------------------------------------------*
      *                  * Se valore a spaces : reimpostazione a meno  *
      *                  * che non sia stato premuto 'Up'              *
      *                  *---------------------------------------------*
           if        rr-alf-ass           not  = spaces
                     go to acc-cod-lgv-440.
           if        v-key                =    "UP  "
                     go to acc-cod-lgv-600
           else      go to acc-cod-lgv-100.
       acc-cod-lgv-440.
      *                  *---------------------------------------------*
      *                  * Test che la distinta base per la subdistin- *
      *                  * ta virtuale esista in archivio [lgt]        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Lettura anagrafica [lgt]                *
      *                      *-----------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "TNMASS"             to   f-key                  .
           move      99                   to   rf-lgt-tpm-ass         .
           move      rr-num-ass           to   rf-lgt-nrm-ass         .
           move      "pgm/dtp/fls/ioc/obj/ioflgt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lgt                 .
      *                      *-----------------------------------------*
      *                      * Test di esistenza                       *
      *                      *-----------------------------------------*
           if        f-sts                =    e-not-err
                     go to acc-cod-lgv-460.
      *                      *-----------------------------------------*
      *                      * Messaggio di errore                     *
      *                      *-----------------------------------------*
           move      "Non esiste una distinta base per la sub-distinta v
      -              "irtuale !      "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                      *-----------------------------------------*
      *                      * A reimpostazione                        *
      *                      *-----------------------------------------*
           go to     acc-cod-lgv-100.
       acc-cod-lgv-460.
      *                  *---------------------------------------------*
      *                  * Fine controlli                              *
      *                  *---------------------------------------------*
           go to     acc-cod-lgv-600.
       acc-cod-lgv-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cod-lgv-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-cod-lgv-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-cod-lgv-100.
       acc-cod-lgv-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Codice assieme                          *
      *    *-----------------------------------------------------------*
       vis-cod-ass-000.
      *              *-------------------------------------------------*
      *              * Visualizzazione codice alfanumerico             *
      *              *-------------------------------------------------*
           perform   vis-alf-ass-000      thru vis-alf-ass-999        .
      *              *-------------------------------------------------*
      *              * Visualizzazione descrizione                     *
      *              *-------------------------------------------------*
           perform   vis-des-ass-000      thru vis-des-ass-999        .
       vis-cod-ass-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Codice alfanumerico assieme             *
      *    *-----------------------------------------------------------*
       vis-alf-ass-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      14                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      26                   to   v-pos                  .
           move      rr-alf-ass           to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-alf-ass-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Descrizione assieme                     *
      *    *-----------------------------------------------------------*
       vis-des-ass-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      rr-des-ass           to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-des-ass-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Tipo valorizzazione per materie prime      *
      *    *-----------------------------------------------------------*
       acc-tva-map-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-tva-map-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tva-map-lun    to   v-car                  .
           move      w-exp-tva-map-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      13                   to   v-lin                  .
           move      26                   to   v-pos                  .
           move      w-exp-tva-map-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           if        rr-tva-map           =    01
                     move  01             to   v-num
           else if   rr-tva-map           =    02
                     move  02             to   v-num
           else if   rr-tva-map           =    03
                     move  03             to   v-num
           else      move  zero           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-tva-map-999.
       acc-tva-map-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        v-num                =    01
                     move  01             to   rr-tva-map
           else if   v-num                =    02
                     move  02             to   rr-tva-map
           else if   v-num                =    03
                     move  03             to   rr-tva-map             .
       acc-tva-map-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-tva-map-425.
      *                  *---------------------------------------------*
      *                  * Valore zero non ammesso, a meno che non si  *
      *                  * sia in Up                                   *
      *                  *---------------------------------------------*
           if        rr-tva-map           not  = zero
                     go to acc-tva-map-450.
           if        v-key                =    "UP  "
                     go to acc-tva-map-450
           else      go to acc-tva-map-600.
       acc-tva-map-450.
      *                  *---------------------------------------------*
      *                  * Fine controlli                              *
      *                  *---------------------------------------------*
           go to     acc-tva-map-600.
       acc-tva-map-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-tva-map-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-tva-map-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-tva-map-100.
       acc-tva-map-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Tipo valorizzazione materie prime       *
      *    *-----------------------------------------------------------*
       vis-tva-map-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tva-map-lun    to   v-car                  .
           move      w-exp-tva-map-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      13                   to   v-lin                  .
           move      26                   to   v-pos                  .
           move      w-exp-tva-map-tbl    to   v-txt                  .
           if        rr-tva-map           =    01
                     move  01             to   v-num
           else if   rr-tva-map           =    02
                     move  02             to   v-num
           else if   rr-tva-map           =    03
                     move  03             to   v-num
           else      move  zero           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tva-map-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Tipo valorizzazione per semilavorati       *
      *    *-----------------------------------------------------------*
       acc-tva-sem-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-tva-sem-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tva-sem-lun    to   v-car                  .
           move      w-exp-tva-sem-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      17                   to   v-lin                  .
           move      26                   to   v-pos                  .
           move      w-exp-tva-sem-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           if        rr-tva-sem           =    99
                     move  01             to   v-num
           else if   rr-tva-sem           =    01
                     move  02             to   v-num
           else if   rr-tva-sem           =    02
                     move  03             to   v-num
           else if   rr-tva-sem           =    03
                     move  04             to   v-num
           else      move  zero           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-tva-sem-999.
       acc-tva-sem-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        v-num                =    01
                     move  99             to   rr-tva-sem
           else if   v-num                =    02
                     move  01             to   rr-tva-sem
           else if   v-num                =    03
                     move  02             to   rr-tva-sem
           else if   v-num                =    04
                     move  03             to   rr-tva-sem             .
       acc-tva-sem-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-tva-sem-425.
      *                  *---------------------------------------------*
      *                  * Valore zero non ammesso, a meno che non si  *
      *                  * sia in Up                                   *
      *                  *---------------------------------------------*
           if        rr-tva-sem           not  = zero
                     go to acc-tva-sem-450.
           if        v-key                =    "UP  "
                     go to acc-tva-sem-450
           else      go to acc-tva-sem-600.
       acc-tva-sem-450.
      *                  *---------------------------------------------*
      *                  * Fine controlli                              *
      *                  *---------------------------------------------*
           go to     acc-tva-sem-600.
       acc-tva-sem-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-tva-sem-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-tva-sem-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-tva-sem-100.
       acc-tva-sem-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Tipo valorizzazione semilavorati        *
      *    *-----------------------------------------------------------*
       vis-tva-sem-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tva-sem-lun    to   v-car                  .
           move      w-exp-tva-sem-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      17                   to   v-lin                  .
           move      26                   to   v-pos                  .
           move      w-exp-tva-sem-tbl    to   v-txt                  .
           if        rr-tva-sem           =    99
                     move  01             to   v-num
           else if   rr-tva-sem           =    01
                     move  02             to   v-num
           else if   rr-tva-sem           =    02
                     move  03             to   v-num
           else if   rr-tva-sem           =    03
                     move  04             to   v-num
           else      move  zero           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tva-sem-999.
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
      *              * Controllo su data per valorizzazione            *
      *              *-------------------------------------------------*
       tdo-ric-sel-105.
           if        rr-dat-val           not  = zero
                     go to tdo-ric-sel-200.
           move      "Manca la data per la valorizzazione               
      -              "               "    to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-200.
      *              *-------------------------------------------------*
      *              * Controllo su tipo distinta                      *
      *              *-------------------------------------------------*
       tdo-ric-sel-201.
           if        rr-tip-dis           not  = zero
                     go to tdo-ric-sel-202.
           move      "Manca il tipo di distinta                         
      -              "               "    to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-202.
           if        rr-tip-dis           =    01 or
                     rr-tip-dis           =    02 or
                     rr-tip-dis           =    99
                     go to tdo-ric-sel-300.
           move      "Tipo di distinta errato                           
      -              "               "    to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-300.
      *              *-------------------------------------------------*
      *              * Controllo su codice assieme                     *
      *              *-------------------------------------------------*
           if        rr-num-ass           not  = zero
                     go to tdo-ric-sel-400.
           move      "Manca il tipo di distinta                         
      -              "               "    to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-302.
           if        rr-tip-dis           =    01
                     go to tdo-ric-sel-310
           else if   rr-tip-dis           =    02
                     go to tdo-ric-sel-320
           else if   rr-tip-dis           =    99
                     go to tdo-ric-sel-330.
       tdo-ric-sel-310.
           move      "Manca il codice del prodotto finito               
      -              "               "    to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-320.
           move      "Manca il codice del semilavorato                  
      -              "               "    to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-330.
           move      "Manca il codice della sub-distinta virtuale       
      -              "               "    to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-400.
      *              *-------------------------------------------------*
      *              * Controllo su valorizzazione materie prime       *
      *              *-------------------------------------------------*
       tdo-ric-sel-405.
           if        rr-tva-map           not  = zero
                     go to tdo-ric-sel-410.
           move      "Manca il tipo di valorizzazione per le materie pri
      -              "me             "    to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-410.
           if        rr-tva-map           =    01 or
                     rr-tva-map           =    02 or
                     rr-tva-map           =    03
                     go to tdo-ric-sel-500.
           move      "Tipo di valorizzazione per le materie prime errato
      -              "               "    to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-500.
      *              *-------------------------------------------------*
      *              * Controllo su valorizzazione semilavorati        *
      *              *-------------------------------------------------*
       tdo-ric-sel-505.
           if        rr-tva-sem           not  = zero
                     go to tdo-ric-sel-510.
           move      "Manca il tipo di valorizzazione per i semilavorati
      -              "               "    to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-510.
           if        rr-tva-sem           =    01 or
                     rr-tva-sem           =    02 or
                     rr-tva-sem           =    03 or
                     rr-tva-sem           =    99
                     go to tdo-ric-sel-600.
           move      "Tipo di valorizzazione per i semilavorati errato  
      -              "               "    to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-600.
      *              *-------------------------------------------------*
      *              * Uscita per controlli superati                   *
      *              *-------------------------------------------------*
           go to     tdo-ric-sel-999.
       tdo-ric-sel-900.
      *              *-------------------------------------------------*
      *              * Trattamento errore                              *
      *              *-------------------------------------------------*
       tdo-ric-sel-905.
      *                  *---------------------------------------------*
      *                  * Emissione messaggio d'errore                *
      *                  *---------------------------------------------*
           perform   box-msg-err-000      thru box-msg-err-999        .
       tdo-ric-sel-910.
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
       reg-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Preparazione parametri per selezione stampa               *
      *    *-----------------------------------------------------------*
       pre-prm-stp-000.
      *              *-------------------------------------------------*
      *              * Se il contatore di cicli gia' eseguiti e' mag-  *
      *              * giore di zero : nessuna preparazione            *
      *              *-------------------------------------------------*
           if        w-cnt-ccl-gia-ese    >    zero
                     go to pre-prm-stp-999.
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
      *    * Routine di sort preliminare                               *
      *    *-----------------------------------------------------------*
       exe-rou-srt-000.
      *              *-------------------------------------------------*
      *              * Flag di sort eseguito a : No                    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-exe-rou-srt      .
       exe-rou-srt-999.
           exit.

      *    *===========================================================*
      *    * Input procedure per sort                                  *
      *    *-----------------------------------------------------------*
       stp-srt-inp-000.
       stp-srt-inp-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Start iniziale                     *
      *    *-----------------------------------------------------------*
       prn-str-ini-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-sub      .
      *              *-------------------------------------------------*
      *              * Inizializzazione flag per forzatura esecuzione  *
      *              * tutta a fine ciclo                              *
      *              *-------------------------------------------------*
           move      spaces               to   w-vlz-scl-flg-end      .
       prn-str-ini-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Messaggio per nessuna registraz.   *
      *    *-----------------------------------------------------------*
       prn-nes-ela-000.
       prn-nes-ela-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Lettura sequenziale                *
      *    *-----------------------------------------------------------*
       prn-let-seq-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-sub      .
      *              *-------------------------------------------------*
      *              * Controllo del flag per forzatura esecuzione     *
      *              * tutta a fine ciclo                              *
      *              *-------------------------------------------------*
           if        w-vlz-scl-flg-end    =    spaces
                     move  "#"            to   w-vlz-scl-flg-end
           else      move  "#"            to   w-cnt-prn-flg-sub
                     go to prn-let-seq-999.
       prn-let-seq-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Test se superamento limiti massimi *
      *    *-----------------------------------------------------------*
       prn-tst-max-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-sub      .
       prn-tst-max-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Selezione su record letto          *
      *    *-----------------------------------------------------------*
       prn-sel-rec-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-sub      .
       prn-sel-rec-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Composizione area per rotture      *
      *    *-----------------------------------------------------------*
       prn-cmp-rot-000.
       prn-cmp-rot-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Esecuzione per inizio ciclo        *
      *    *-----------------------------------------------------------*
       prn-ini-cic-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
       prn-ini-cic-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Esecuzione per fine ciclo          *
      *    *-----------------------------------------------------------*
       prn-fin-cic-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
      *              *-------------------------------------------------*
      *              * Esecuzione stampa valorizzazione scalare        *
      *              *-------------------------------------------------*
           perform   sta-vlz-scl-000      thru sta-vlz-scl-999        .
       prn-fin-cic-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Inizio 5. livello di rottura       *
      *    *-----------------------------------------------------------*
       prn-ini-lr5-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
       prn-ini-lr5-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Fine 5. livello di rottura         *
      *    *-----------------------------------------------------------*
       prn-fin-lr5-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
       prn-fin-lr5-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Inizio 4. livello di rottura       *
      *    *-----------------------------------------------------------*
       prn-ini-lr4-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
       prn-ini-lr4-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Fine 4. livello di rottura         *
      *    *-----------------------------------------------------------*
       prn-fin-lr4-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
       prn-fin-lr4-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Inizio 3. livello di rottura       *
      *    *-----------------------------------------------------------*
       prn-ini-lr3-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
       prn-ini-lr3-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Fine 3. livello di rottura         *
      *    *-----------------------------------------------------------*
       prn-fin-lr3-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
       prn-fin-lr3-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Inizio 2. livello di rottura       *
      *    *-----------------------------------------------------------*
       prn-ini-lr2-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
       prn-ini-lr2-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Fine 2. livello di rottura         *
      *    *-----------------------------------------------------------*
       prn-fin-lr2-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
       prn-fin-lr2-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Inizio 1. livello di rottura       *
      *    *-----------------------------------------------------------*
       prn-ini-lr1-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
       prn-ini-lr1-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Fine 1. livello di rottura         *
      *    *-----------------------------------------------------------*
       prn-fin-lr1-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
       prn-fin-lr1-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Livello di dettaglio               *
      *    *-----------------------------------------------------------*
       prn-liv-det-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
       prn-liv-det-999.
           exit.

      *    *===========================================================*
      *    * Esecuzione stampa valorizzazione scalare                  *
      *    *-----------------------------------------------------------*
       sta-vlz-scl-000.
      *              *-------------------------------------------------*
      *              * Inizializzazioni                                *
      *              *-------------------------------------------------*
       sta-vlz-scl-010.
      *                  *---------------------------------------------*
      *                  * Numero pagina di stampa a zero              *
      *                  *---------------------------------------------*
           move      zero                 to   w-vlz-scl-num-pag      .
      *                  *---------------------------------------------*
      *                  * Flag per determinazione valore unitario :   *
      *                  * a spaces                                    *
      *                  *---------------------------------------------*
           move      spaces               to   w-vlz-scl-flg-val      .
      *                  *---------------------------------------------*
      *                  * Comodo per determinazione valore unitario : *
      *                  * a zero                                      *
      *                  *---------------------------------------------*
           move      zero                 to   w-vlz-scl-det-val      .
       sta-vlz-scl-100.
      *              *-------------------------------------------------*
      *              * Intestazione foglio                             *
      *              *-------------------------------------------------*
           perform   int-pag-sta-000      thru int-pag-sta-999        .
      *              *-------------------------------------------------*
      *              * Se interruzione forzata : a fine                *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to sta-vlz-scl-999.
       sta-vlz-scl-200.
      *              *-------------------------------------------------*
      *              * Esplosione scalare della distinta base con de-  *
      *              * terminazione del valore unitario                *
      *              *-------------------------------------------------*
       sta-vlz-scl-220.
      *                  *---------------------------------------------*
      *                  * Inizializzazione subroutine di esplosione   *
      *                  * scalare distinta base                       *
      *                  *---------------------------------------------*
           move      "E-INI"              to   w-esp-scl-dtp-fun      .
           move      rr-tip-dis           to   w-esp-scl-dtp-tde      .
           move      rr-num-ass           to   w-esp-scl-dtp-nde      .
           move      rr-alf-ass           to   w-esp-scl-dtp-ade      .
           move      1,000                to   w-esp-scl-dtp-qde      .
           perform   esp-scl-dtp-000      thru esp-scl-dtp-999        .
       sta-vlz-scl-240.
      *                  *---------------------------------------------*
      *                  * Preparazione lettura elemento successivo da *
      *                  * esplosione scalare distinta base            *
      *                  *---------------------------------------------*
           move      "E-GET"              to   w-esp-scl-dtp-fun      .
       sta-vlz-scl-260.
      *                  *---------------------------------------------*
      *                  * Lettura elemento successivo da esplosione   *
      *                  * scalare distinta base                       *
      *                  *---------------------------------------------*
           perform   esp-scl-dtp-000      thru esp-scl-dtp-999        .
      *                  *---------------------------------------------*
      *                  * Se fine scansione distinta : a fine deter-  *
      *                  * minazione valore unitario                   *
      *                  *---------------------------------------------*
           if        w-esp-scl-dtp-sts    not  = spaces
                     go to sta-vlz-scl-600.
       sta-vlz-scl-280.
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda del tipo di elemento   *
      *                  * trovato nella scansione                     *
      *                  *---------------------------------------------*
           if        w-esp-scl-dtp-tip    =    01
                     go to sta-vlz-scl-300
           else if   w-esp-scl-dtp-tip    =    02
                     go to sta-vlz-scl-320
           else if   w-esp-scl-dtp-tip    =    03
                     go to sta-vlz-scl-420
           else if   w-esp-scl-dtp-tip    =    99
                     go to sta-vlz-scl-500.
       sta-vlz-scl-300.
      *              *-------------------------------------------------*
      *              * Se prodotto finito                              *
      *              *-------------------------------------------------*
       sta-vlz-scl-305.
      *                  *---------------------------------------------*
      *                  * Preparazione per stampa elemento            *
      *                  *---------------------------------------------*
           move      w-esp-scl-dtp-liv    to   w-vlz-scl-liv-stp      .
           move      01                   to   w-vlz-scl-tel-stp      .
           move      w-esp-scl-dtp-num    to   w-vlz-scl-num-stp      .
           move      w-esp-scl-dtp-alf    to   w-vlz-scl-alf-stp      .
           move      w-esp-scl-dtp-des    to   w-vlz-scl-des-stp      .
           move      w-esp-scl-dtp-umi    to   w-vlz-scl-umi-stp      .
           if        w-esp-scl-dtp-ana    =    spaces
                     move  rf-dcp-dec-qta to   w-vlz-scl-ndq-stp
           else      move  w-esp-scl-dtp-dec
                                          to   w-vlz-scl-ndq-stp      .
           move      w-esp-scl-dtp-qta    to   w-vlz-scl-qta-stp      .
           move      "N"                  to   w-vlz-scl-snx-cev      .
           move      zero                 to   w-vlz-scl-cun-stp      .
           move      zero                 to   w-vlz-scl-val-stp      .
       sta-vlz-scl-310.
      *                  *---------------------------------------------*
      *                  * Richiamo stampa elemento                    *
      *                  *---------------------------------------------*
           perform   sta-ele-dis-000      thru sta-ele-dis-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : a fine            *
      *                  *---------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to sta-vlz-scl-999.
       sta-vlz-scl-315.
      *                  *---------------------------------------------*
      *                  * Entrata nel livello successivo              *
      *                  *---------------------------------------------*
           go to     sta-vlz-scl-240.
       sta-vlz-scl-320.
      *              *-------------------------------------------------*
      *              * Se semilavorato                                 *
      *              *-------------------------------------------------*
       sta-vlz-scl-325.
      *                  *---------------------------------------------*
      *                  * Se anagrafica non esistente si entra nel    *
      *                  * livello successivo                          *
      *                  *---------------------------------------------*
           if        w-esp-scl-dtp-ana    =    spaces
                     go to sta-vlz-scl-330.
       sta-vlz-scl-326.
      *                      *-----------------------------------------*
      *                      * Preparazione per stampa elemento        *
      *                      *-----------------------------------------*
           move      w-esp-scl-dtp-liv    to   w-vlz-scl-liv-stp      .
           move      02                   to   w-vlz-scl-tel-stp      .
           move      w-esp-scl-dtp-num    to   w-vlz-scl-num-stp      .
           move      w-esp-scl-dtp-alf    to   w-vlz-scl-alf-stp      .
           move      w-esp-scl-dtp-des    to   w-vlz-scl-des-stp      .
           move      w-esp-scl-dtp-umi    to   w-vlz-scl-umi-stp      .
           if        w-esp-scl-dtp-ana    =    spaces
                     move  rf-dps-dec-qta to   w-vlz-scl-ndq-stp
           else      move  w-esp-scl-dtp-dec
                                          to   w-vlz-scl-ndq-stp      .
           move      w-esp-scl-dtp-qta    to   w-vlz-scl-qta-stp      .
           move      "N"                  to   w-vlz-scl-snx-cev      .
           move      zero                 to   w-vlz-scl-cun-stp      .
           move      zero                 to   w-vlz-scl-val-stp      .
       sta-vlz-scl-327.
      *                      *-----------------------------------------*
      *                      * Richiamo stampa elemento                *
      *                      *-----------------------------------------*
           perform   sta-ele-dis-000      thru sta-ele-dis-999        .
      *                      *-----------------------------------------*
      *                      * Se interruzione forzata : a fine        *
      *                      *-----------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to sta-vlz-scl-999.
       sta-vlz-scl-328.
      *                      *-----------------------------------------*
      *                      * Entrata nel livello successivo          *
      *                      *-----------------------------------------*
           go to     sta-vlz-scl-240.
       sta-vlz-scl-330.
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda del tipo di semilavo-  *
      *                  * rato                                        *
      *                  *---------------------------------------------*
           if        rf-dps-tip-sem       =    01
                     go to sta-vlz-scl-345.
       sta-vlz-scl-335.
      *                  *---------------------------------------------*
      *                  * Se semilavorato intermedio di un ciclo di   *
      *                  * lavorazione si entra nel livello successivo *
      *                  *---------------------------------------------*
       sta-vlz-scl-340.
      *                      *-----------------------------------------*
      *                      * Preparazione per stampa elemento        *
      *                      *-----------------------------------------*
           move      w-esp-scl-dtp-liv    to   w-vlz-scl-liv-stp      .
           move      02                   to   w-vlz-scl-tel-stp      .
           move      w-esp-scl-dtp-num    to   w-vlz-scl-num-stp      .
           move      w-esp-scl-dtp-alf    to   w-vlz-scl-alf-stp      .
           move      w-esp-scl-dtp-des    to   w-vlz-scl-des-stp      .
           move      w-esp-scl-dtp-umi    to   w-vlz-scl-umi-stp      .
           if        w-esp-scl-dtp-ana    =    spaces
                     move  rf-dps-dec-qta to   w-vlz-scl-ndq-stp
           else      move  w-esp-scl-dtp-dec
                                          to   w-vlz-scl-ndq-stp      .
           move      w-esp-scl-dtp-qta    to   w-vlz-scl-qta-stp      .
           move      "N"                  to   w-vlz-scl-snx-cev      .
           move      zero                 to   w-vlz-scl-cun-stp      .
           move      zero                 to   w-vlz-scl-val-stp      .
       sta-vlz-scl-341.
      *                      *-----------------------------------------*
      *                      * Richiamo stampa elemento                *
      *                      *-----------------------------------------*
           perform   sta-ele-dis-000      thru sta-ele-dis-999        .
      *                      *-----------------------------------------*
      *                      * Se interruzione forzata : a fine        *
      *                      *-----------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to sta-vlz-scl-999.
       sta-vlz-scl-342.
      *                      *-----------------------------------------*
      *                      * Entrata nel livello successivo          *
      *                      *-----------------------------------------*
           go to     sta-vlz-scl-240.
       sta-vlz-scl-345.
      *                  *---------------------------------------------*
      *                  * Se semilavorato finale di un ciclo di lavo- *
      *                  * razione                                     *
      *                  *---------------------------------------------*
       sta-vlz-scl-350.
      *                      *-----------------------------------------*
      *                      * Se il tipo di valorizzazione richiesta  *
      *                      * per i semilavorati finali di un ciclo   *
      *                      * di lavorazione indica di calcolare in   *
      *                      * base ai componenti : si entra nel li-   *
      *                      * vello successivo                        *
      *                      *-----------------------------------------*
           if        rr-tva-sem           not  = 99
                     go to sta-vlz-scl-355.
       sta-vlz-scl-351.
      *                          *-------------------------------------*
      *                          * Preparazione per stampa elemento    *
      *                          *-------------------------------------*
           move      w-esp-scl-dtp-liv    to   w-vlz-scl-liv-stp      .
           move      02                   to   w-vlz-scl-tel-stp      .
           move      w-esp-scl-dtp-num    to   w-vlz-scl-num-stp      .
           move      w-esp-scl-dtp-alf    to   w-vlz-scl-alf-stp      .
           move      w-esp-scl-dtp-des    to   w-vlz-scl-des-stp      .
           move      w-esp-scl-dtp-umi    to   w-vlz-scl-umi-stp      .
           if        w-esp-scl-dtp-ana    =    spaces
                     move  rf-dps-dec-qta to   w-vlz-scl-ndq-stp
           else      move  w-esp-scl-dtp-dec
                                          to   w-vlz-scl-ndq-stp      .
           move      w-esp-scl-dtp-qta    to   w-vlz-scl-qta-stp      .
           move      "N"                  to   w-vlz-scl-snx-cev      .
           move      zero                 to   w-vlz-scl-cun-stp      .
           move      zero                 to   w-vlz-scl-val-stp      .
       sta-vlz-scl-352.
      *                          *-------------------------------------*
      *                          * Richiamo stampa elemento            *
      *                          *-------------------------------------*
           perform   sta-ele-dis-000      thru sta-ele-dis-999        .
      *                          *-------------------------------------*
      *                          * Se interruzione forzata : a fine    *
      *                          *-------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to sta-vlz-scl-999.
       sta-vlz-scl-353.
      *                          *-------------------------------------*
      *                          * Entrata nel livello successivo      *
      *                          *-------------------------------------*
           go to     sta-vlz-scl-240.
       sta-vlz-scl-355.
      *                      *-----------------------------------------*
      *                      * Altrimenti si devia in funzione del ti- *
      *                      * po di valorizzazione richiesto          *
      *                      *-----------------------------------------*
           if        rr-tva-sem           =    01
                     go to sta-vlz-scl-360
           else if   rr-tva-sem           =    02
                     go to sta-vlz-scl-380
           else if   rr-tva-sem           =    03
                     go to sta-vlz-scl-400.
       sta-vlz-scl-360.
      *                      *-----------------------------------------*
      *                      * Se valorizzazione al costo medio ponde- *
      *                      * rato                                    *
      *                      *-----------------------------------------*
       sta-vlz-scl-361.
      *                          *-------------------------------------*
      *                          * Richiamo modulo di magazzino        *
      *                          *-------------------------------------*
           move      "VM"                 to   d-vun-mag-tip-ope      .
           move      0001                 to   d-vun-mag-tip-val      .
           move      rr-dat-val           to   d-vun-mag-dat-val      .
           move      02                   to   d-vun-mag-tip-mag      .
           move      rf-dps-num-sem       to   d-vun-mag-num-mag      .
           move      "pgm/mag/prg/obj/pmag300v"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using d-vun-mag              .
       sta-vlz-scl-362.
      *                          *-------------------------------------*
      *                          * Se esito operazione negativo si     *
      *                          * prova con la valorizzazione a costo *
      *                          * standard                            *
      *                          *-------------------------------------*
           if        d-vun-mag-exi-sts    not  = spaces
                     go to sta-vlz-scl-400.
       sta-vlz-scl-363.
      *                          *-------------------------------------*
      *                          * Se valore determinato pari a zero : *
      *                          * si pone in On il flag di determi-   *
      *                          * nazione non eseguibile, si stampa   *
      *                          * l'elemento, e si esegue uno Skip    *
      *                          *-------------------------------------*
           if        d-vun-mag-val-uni    not  = zero
                     go to sta-vlz-scl-368.
       sta-vlz-scl-364.
      *                              *---------------------------------*
      *                              * Flag di determinazione non e-   *
      *                              * seguibile                       *
      *                              *---------------------------------*
           move      "#"                  to   w-vlz-scl-flg-val      .
       sta-vlz-scl-365.
      *                              *---------------------------------*
      *                              * Preparazione per stampa elemento*
      *                              *---------------------------------*
           move      w-esp-scl-dtp-liv    to   w-vlz-scl-liv-stp      .
           move      02                   to   w-vlz-scl-tel-stp      .
           move      w-esp-scl-dtp-num    to   w-vlz-scl-num-stp      .
           move      w-esp-scl-dtp-alf    to   w-vlz-scl-alf-stp      .
           move      w-esp-scl-dtp-des    to   w-vlz-scl-des-stp      .
           move      w-esp-scl-dtp-umi    to   w-vlz-scl-umi-stp      .
           if        w-esp-scl-dtp-ana    =    spaces
                     move  rf-dps-dec-qta to   w-vlz-scl-ndq-stp
           else      move  w-esp-scl-dtp-dec
                                          to   w-vlz-scl-ndq-stp      .
           move      w-esp-scl-dtp-qta    to   w-vlz-scl-qta-stp      .
           move      "S"                  to   w-vlz-scl-snx-cev      .
           move      zero                 to   w-vlz-scl-cun-stp      .
           move      zero                 to   w-vlz-scl-val-stp      .
       sta-vlz-scl-366.
      *                              *---------------------------------*
      *                              * Richiamo stampa elemento        *
      *                              *---------------------------------*
           perform   sta-ele-dis-000      thru sta-ele-dis-999        .
      *                              *---------------------------------*
      *                              * Se interruzione forzata : a fine*
      *                              *---------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to sta-vlz-scl-999.
       sta-vlz-scl-367.
      *                              *---------------------------------*
      *                              * Preparazione funzione Skip      *
      *                              *---------------------------------*
           move      "E-SKP"              to   w-esp-scl-dtp-fun      .
      *                              *---------------------------------*
      *                              * Riciclo su scansione distinta   *
      *                              * base                            *
      *                              *---------------------------------*
           go to     sta-vlz-scl-260.
       sta-vlz-scl-368.
      *                          *-------------------------------------*
      *                          * Se valore determinato diverso da    *
      *                          * zero                                *
      *                          *-------------------------------------*
       sta-vlz-scl-369.
      *                              *---------------------------------*
      *                              * Sommatoria valore               *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Determinazione sub-valore   *
      *                                  * parziale per l'elemento in  *
      *                                  * esame                       *
      *                                  *-----------------------------*
           move      d-vun-mag-val-uni    to   w-vlz-scl-det-sub      .
      *                                  *-----------------------------*
      *                                  * Applicazione decimali ges-  *
      *                                  * tione magazzino             *
      *                                  *-----------------------------*
           divide    100                  into w-vlz-scl-det-sub      .
      *                                  *-----------------------------*
      *                                  * Calcolo                     *
      *                                  *-----------------------------*
           multiply  w-esp-scl-dtp-qta    by   w-vlz-scl-det-sub      .
      *                                  *-----------------------------*
      *                                  * Sommatoria vera e propria   *
      *                                  *-----------------------------*
           add       w-vlz-scl-det-sub    to   w-vlz-scl-det-val      .
       sta-vlz-scl-370.
      *                                  *-----------------------------*
      *                                  * Preparazione per stampa e-  *
      *                                  * lemento                     *
      *                                  *-----------------------------*
           move      w-esp-scl-dtp-liv    to   w-vlz-scl-liv-stp      .
           move      02                   to   w-vlz-scl-tel-stp      .
           move      w-esp-scl-dtp-num    to   w-vlz-scl-num-stp      .
           move      w-esp-scl-dtp-alf    to   w-vlz-scl-alf-stp      .
           move      w-esp-scl-dtp-des    to   w-vlz-scl-des-stp      .
           move      w-esp-scl-dtp-umi    to   w-vlz-scl-umi-stp      .
           if        w-esp-scl-dtp-ana    =    spaces
                     move  rf-dps-dec-qta to   w-vlz-scl-ndq-stp
           else      move  w-esp-scl-dtp-dec
                                          to   w-vlz-scl-ndq-stp      .
           move      w-esp-scl-dtp-qta    to   w-vlz-scl-qta-stp      .
           move      "S"                  to   w-vlz-scl-snx-cev      .
           move      d-vun-mag-val-uni    to   w-vlz-scl-cun-stp      .
           move      w-vlz-scl-det-sub    to   w-vlz-scl-val-stp      .
       sta-vlz-scl-371.
      *                                  *-----------------------------*
      *                                  * Richiamo stampa elemento    *
      *                                  *-----------------------------*
           perform   sta-ele-dis-000      thru sta-ele-dis-999        .
      *                                  *-----------------------------*
      *                                  * Se interruzione forzata : a *
      *                                  * fine                        *
      *                                  *-----------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to sta-vlz-scl-999.
       sta-vlz-scl-372.
      *                                  *-----------------------------*
      *                                  * Preparazione funzione Skip  *
      *                                  *-----------------------------*
           move      "E-SKP"              to   w-esp-scl-dtp-fun      .
      *                                  *-----------------------------*
      *                                  * Riciclo su scansione di-    *
      *                                  * stinta base                 *
      *                                  *-----------------------------*
           go to     sta-vlz-scl-260.
       sta-vlz-scl-380.
      *                      *-----------------------------------------*
      *                      * Se valorizzazione al costo ultimo       *
      *                      *-----------------------------------------*
       sta-vlz-scl-381.
      *                          *-------------------------------------*
      *                          * Richiamo modulo di magazzino        *
      *                          *-------------------------------------*
           move      "VM"                 to   d-vun-mag-tip-ope      .
           move      0002                 to   d-vun-mag-tip-val      .
           move      rr-dat-val           to   d-vun-mag-dat-val      .
           move      02                   to   d-vun-mag-tip-mag      .
           move      rf-dps-num-sem       to   d-vun-mag-num-mag      .
           move      "pgm/mag/prg/obj/pmag300v"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using d-vun-mag              .
       sta-vlz-scl-382.
      *                          *-------------------------------------*
      *                          * Riaggancio al trattamento gia' ese- *
      *                          * guito per il costo medio ponderato  *
      *                          *-------------------------------------*
           go to     sta-vlz-scl-362.
       sta-vlz-scl-400.
      *                      *-----------------------------------------*
      *                      * Se valorizzazione al costo standard     *
      *                      *-----------------------------------------*
       sta-vlz-scl-401.
      *                          *-------------------------------------*
      *                          * Richiamo modulo di magazzino        *
      *                          *-------------------------------------*
           move      "VM"                 to   d-vun-mag-tip-ope      .
           move      0003                 to   d-vun-mag-tip-val      .
           move      rr-dat-val           to   d-vun-mag-dat-val      .
           move      02                   to   d-vun-mag-tip-mag      .
           move      rf-dps-num-sem       to   d-vun-mag-num-mag      .
           move      "pgm/mag/prg/obj/pmag300v"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using d-vun-mag              .
      *                          *-------------------------------------*
      *                          * Riaggancio al trattamento gia' ese- *
      *                          * guito per il costo medio ponderato  *
      *                          * ad esclusione della prova per il    *
      *                          * costo standard                      *
      *                          *-------------------------------------*
           go to     sta-vlz-scl-363.
       sta-vlz-scl-420.
      *              *-------------------------------------------------*
      *              * Se materia prima                                *
      *              *-------------------------------------------------*
       sta-vlz-scl-430.
      *                      *-----------------------------------------*
      *                      * Deviazione in funzione del tipo di va-  *
      *                      * lorizzazione richiesto                  *
      *                      *-----------------------------------------*
           if        rr-tva-map           =    01
                     go to sta-vlz-scl-440
           else if   rr-tva-map           =    02
                     go to sta-vlz-scl-460
           else if   rr-tva-map           =    03
                     go to sta-vlz-scl-480.
       sta-vlz-scl-440.
      *                      *-----------------------------------------*
      *                      * Se valorizzazione al costo medio ponde- *
      *                      * rato                                    *
      *                      *-----------------------------------------*
       sta-vlz-scl-441.
      *                          *-------------------------------------*
      *                          * Richiamo modulo di magazzino        *
      *                          *-------------------------------------*
           move      "VM"                 to   d-vun-mag-tip-ope      .
           move      0001                 to   d-vun-mag-tip-val      .
           move      rr-dat-val           to   d-vun-mag-dat-val      .
           move      03                   to   d-vun-mag-tip-mag      .
           move      rf-dpm-num-map       to   d-vun-mag-num-mag      .
           move      "pgm/mag/prg/obj/pmag300v"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using d-vun-mag              .
       sta-vlz-scl-442.
      *                          *-------------------------------------*
      *                          * Se esito operazione negativo si     *
      *                          * prova con la valorizzazione a costo *
      *                          * standard                            *
      *                          *-------------------------------------*
           if        d-vun-mag-exi-sts    not  = spaces
                     go to sta-vlz-scl-480.
       sta-vlz-scl-443.
      *                          *-------------------------------------*
      *                          * Se valore determinato pari a zero : *
      *                          * si pone in On il flag di determi-   *
      *                          * nazione non eseguibile, si stampa   *
      *                          * l'elemento, e si ricicla all'ele-   *
      *                          * mento successivo in distinta        *
      *                          *-------------------------------------*
           if        d-vun-mag-val-uni    not  = zero
                     go to sta-vlz-scl-448.
       sta-vlz-scl-444.
      *                              *---------------------------------*
      *                              * Flag di determinazione non e-   *
      *                              * seguibile                       *
      *                              *---------------------------------*
           move      "#"                  to   w-vlz-scl-flg-val      .
       sta-vlz-scl-445.
      *                              *---------------------------------*
      *                              * Preparazione per stampa elemento*
      *                              *---------------------------------*
           move      w-esp-scl-dtp-liv    to   w-vlz-scl-liv-stp      .
           move      03                   to   w-vlz-scl-tel-stp      .
           move      w-esp-scl-dtp-num    to   w-vlz-scl-num-stp      .
           move      w-esp-scl-dtp-alf    to   w-vlz-scl-alf-stp      .
           move      w-esp-scl-dtp-des    to   w-vlz-scl-des-stp      .
           move      w-esp-scl-dtp-umi    to   w-vlz-scl-umi-stp      .
           if        w-esp-scl-dtp-ana    =    spaces
                     move  rf-dpm-dec-qta to   w-vlz-scl-ndq-stp
           else      move  w-esp-scl-dtp-dec
                                          to   w-vlz-scl-ndq-stp      .
           move      w-esp-scl-dtp-qta    to   w-vlz-scl-qta-stp      .
           move      "S"                  to   w-vlz-scl-snx-cev      .
           move      zero                 to   w-vlz-scl-cun-stp      .
           move      zero                 to   w-vlz-scl-val-stp      .
       sta-vlz-scl-446.
      *                              *---------------------------------*
      *                              * Richiamo stampa elemento        *
      *                              *---------------------------------*
           perform   sta-ele-dis-000      thru sta-ele-dis-999        .
      *                              *---------------------------------*
      *                              * Se interruzione forzata : a fine*
      *                              *---------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to sta-vlz-scl-999.
       sta-vlz-scl-447.
      *                              *---------------------------------*
      *                              * Riciclo su scansione distinta   *
      *                              * base                            *
      *                              *---------------------------------*
           go to     sta-vlz-scl-240.
       sta-vlz-scl-448.
      *                          *-------------------------------------*
      *                          * Se valore determinato diverso da    *
      *                          * zero                                *
      *                          *-------------------------------------*
       sta-vlz-scl-449.
      *                              *---------------------------------*
      *                              * Sommatoria valore               *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Determinazione sub-valore   *
      *                                  * parziale per l'elemento in  *
      *                                  * esame                       *
      *                                  *-----------------------------*
           move      d-vun-mag-val-uni    to   w-vlz-scl-det-sub      .
      *                                  *-----------------------------*
      *                                  * Applicazione decimali ges-  *
      *                                  * tione magazzino             *
      *                                  *-----------------------------*
           divide    100                  into w-vlz-scl-det-sub      .
      *                                  *-----------------------------*
      *                                  * Calcolo                     *
      *                                  *-----------------------------*
           multiply  w-esp-scl-dtp-qta    by   w-vlz-scl-det-sub      .
      *                                  *-----------------------------*
      *                                  * Sommatoria vera e propria   *
      *                                  *-----------------------------*
           add       w-vlz-scl-det-sub    to   w-vlz-scl-det-val      .
       sta-vlz-scl-450.
      *                                  *-----------------------------*
      *                                  * Preparazione per stampa e-  *
      *                                  * lemento                     *
      *                                  *-----------------------------*
           move      w-esp-scl-dtp-liv    to   w-vlz-scl-liv-stp      .
           move      03                   to   w-vlz-scl-tel-stp      .
           move      w-esp-scl-dtp-num    to   w-vlz-scl-num-stp      .
           move      w-esp-scl-dtp-alf    to   w-vlz-scl-alf-stp      .
           move      w-esp-scl-dtp-des    to   w-vlz-scl-des-stp      .
           move      w-esp-scl-dtp-umi    to   w-vlz-scl-umi-stp      .
           if        w-esp-scl-dtp-ana    =    spaces
                     move  rf-dpm-dec-qta to   w-vlz-scl-ndq-stp
           else      move  w-esp-scl-dtp-dec
                                          to   w-vlz-scl-ndq-stp      .
           move      w-esp-scl-dtp-qta    to   w-vlz-scl-qta-stp      .
           move      "S"                  to   w-vlz-scl-snx-cev      .
           move      d-vun-mag-val-uni    to   w-vlz-scl-cun-stp      .
           move      w-vlz-scl-det-sub    to   w-vlz-scl-val-stp      .
       sta-vlz-scl-451.
      *                                  *-----------------------------*
      *                                  * Richiamo stampa elemento    *
      *                                  *-----------------------------*
           perform   sta-ele-dis-000      thru sta-ele-dis-999        .
      *                                  *-----------------------------*
      *                                  * Se interruzione forzata : a *
      *                                  * fine                        *
      *                                  *-----------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to sta-vlz-scl-999.
       sta-vlz-scl-452.
      *                                  *-----------------------------*
      *                                  * Riciclo su scansione di-    *
      *                                  * stinta base                 *
      *                                  *-----------------------------*
           go to     sta-vlz-scl-240.
       sta-vlz-scl-460.
      *                      *-----------------------------------------*
      *                      * Se valorizzazione al costo ultimo       *
      *                      *-----------------------------------------*
       sta-vlz-scl-461.
      *                          *-------------------------------------*
      *                          * Richiamo modulo di magazzino        *
      *                          *-------------------------------------*
           move      "VM"                 to   d-vun-mag-tip-ope      .
           move      0002                 to   d-vun-mag-tip-val      .
           move      rr-dat-val           to   d-vun-mag-dat-val      .
           move      03                   to   d-vun-mag-tip-mag      .
           move      rf-dpm-num-map       to   d-vun-mag-num-mag      .
           move      "pgm/mag/prg/obj/pmag300v"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using d-vun-mag              .
       sta-vlz-scl-462.
      *                          *-------------------------------------*
      *                          * Riaggancio al trattamento gia' ese- *
      *                          * guito per il costo medio ponderato  *
      *                          *-------------------------------------*
           go to     sta-vlz-scl-442.
       sta-vlz-scl-480.
      *                      *-----------------------------------------*
      *                      * Se valorizzazione al costo standard     *
      *                      *-----------------------------------------*
       sta-vlz-scl-481.
      *                          *-------------------------------------*
      *                          * Richiamo modulo di magazzino        *
      *                          *-------------------------------------*
           move      "VM"                 to   d-vun-mag-tip-ope      .
           move      0003                 to   d-vun-mag-tip-val      .
           move      rr-dat-val           to   d-vun-mag-dat-val      .
           move      03                   to   d-vun-mag-tip-mag      .
           move      rf-dpm-num-map       to   d-vun-mag-num-mag      .
           move      "pgm/mag/prg/obj/pmag300v"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using d-vun-mag              .
       sta-vlz-scl-482.
      *                          *-------------------------------------*
      *                          * Riaggancio al trattamento gia' ese- *
      *                          * guito per il costo medio ponderato  *
      *                          * ad esclusione della prova per il    *
      *                          * costo standard                      *
      *                          *-------------------------------------*
           go to     sta-vlz-scl-443.
       sta-vlz-scl-500.
      *              *-------------------------------------------------*
      *              * Se subdistinta virtuale                         *
      *              *-------------------------------------------------*
       sta-vlz-scl-505.
      *                  *---------------------------------------------*
      *                  * Preparazione per stampa elemento            *
      *                  *---------------------------------------------*
           move      w-esp-scl-dtp-liv    to   w-vlz-scl-liv-stp      .
           move      99                   to   w-vlz-scl-tel-stp      .
           move      w-esp-scl-dtp-num    to   w-vlz-scl-num-stp      .
           move      w-esp-scl-dtp-alf    to   w-vlz-scl-alf-stp      .
           move      w-esp-scl-dtp-des    to   w-vlz-scl-des-stp      .
           move      w-esp-scl-dtp-umi    to   w-vlz-scl-umi-stp      .
           if        w-esp-scl-dtp-ana    =    spaces
                     move  rf-lgv-dec-qta to   w-vlz-scl-ndq-stp
           else      move  w-esp-scl-dtp-dec
                                          to   w-vlz-scl-ndq-stp      .
           move      w-esp-scl-dtp-qta    to   w-vlz-scl-qta-stp      .
           move      "N"                  to   w-vlz-scl-snx-cev      .
           move      zero                 to   w-vlz-scl-cun-stp      .
           move      zero                 to   w-vlz-scl-val-stp      .
       sta-vlz-scl-510.
      *                  *---------------------------------------------*
      *                  * Richiamo stampa elemento                    *
      *                  *---------------------------------------------*
           perform   sta-ele-dis-000      thru sta-ele-dis-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : a fine            *
      *                  *---------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to sta-vlz-scl-999.
       sta-vlz-scl-515.
      *                  *---------------------------------------------*
      *                  * Entrata nel livello successivo              *
      *                  *---------------------------------------------*
           go to     sta-vlz-scl-240.
       sta-vlz-scl-600.
      *              *-------------------------------------------------*
      *              * Fine esplosione scalare della distinta base con *
      *              * determinazione del valore unitario              *
      *              *-------------------------------------------------*
       sta-vlz-scl-610.
      *                  *---------------------------------------------*
      *                  * Test se linee residue sufficienti per la    *
      *                  * stampa del totale                           *
      *                  *---------------------------------------------*
           if        p-res                >    3
                     go to sta-vlz-scl-620.
      *                  *---------------------------------------------*
      *                  * Intestazione pagina                         *
      *                  *---------------------------------------------*
           perform   int-pag-sta-000      thru int-pag-sta-999        .
      *                  *---------------------------------------------*
      *                  * Test se interruzione forzata                *
      *                  *---------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to sta-vlz-scl-999.
       sta-vlz-scl-620.
      *                  *---------------------------------------------*
      *                  * Linea di sopralineatura del totale          *
      *                  *---------------------------------------------*
       sta-vlz-scl-621.
      *                      *-----------------------------------------*
      *                      * Interlinea                              *
      *                      *-----------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       sta-vlz-scl-622.
      *                      *-----------------------------------------*
      *                      * Stampa                                  *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      14                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      119                  to   p-pos                  .
           move      "--------------"     to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       sta-vlz-scl-630.
      *                  *---------------------------------------------*
      *                  * Linea del totale                            *
      *                  *---------------------------------------------*
       sta-vlz-scl-631.
      *                      *-----------------------------------------*
      *                      * Interlinea                              *
      *                      *-----------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       sta-vlz-scl-632.
      *                      *-----------------------------------------*
      *                      * Stampa literal                          *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      15                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      102                  to   p-pos                  .
           move      "Valore totale :"    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       sta-vlz-scl-633.
      *                      *-----------------------------------------*
      *                      * Stampa segnale di valore a zero o non   *
      *                      * perfettamente determinato               *
      *                      *-----------------------------------------*
           if        w-vlz-scl-det-val    not  = zero and
                     w-vlz-scl-flg-val    =    spaces
                     go to sta-vlz-scl-634.
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      01                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      118                  to   p-pos                  .
           move      "*"                  to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       sta-vlz-scl-634.
      *                      *-----------------------------------------*
      *                      * Stampa valore totale                    *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      11                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "G"                  to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      119                  to   p-pos                  .
           move      w-vlz-scl-det-val    to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       sta-vlz-scl-640.
      *                  *---------------------------------------------*
      *                  * Linea di sottolineatura del totale          *
      *                  *---------------------------------------------*
       sta-vlz-scl-641.
      *                      *-----------------------------------------*
      *                      * Interlinea                              *
      *                      *-----------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       sta-vlz-scl-642.
      *                      *-----------------------------------------*
      *                      * Stampa                                  *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      14                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      119                  to   p-pos                  .
           move      "--------------"     to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       sta-vlz-scl-650.
      *                  *---------------------------------------------*
      *                  * Eject finale                                *
      *                  *---------------------------------------------*
           move      "EJ"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       sta-vlz-scl-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     sta-vlz-scl-999.
       sta-vlz-scl-999.
           exit.

      *    *===========================================================*
      *    * Intestazione foglio                                       *
      *    *-----------------------------------------------------------*
       int-pag-sta-000.
      *              *-------------------------------------------------*
      *              * Preparazione parametri intestazione standard    *
      *              *-------------------------------------------------*
           move      "VALORIZZAZIONE SCALARE DISTINTA BASE"
                                          to   w-cnt-tit-des-tit      .
           move      "DT"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-dat                to   w-cnt-tit-dat-stp      .
           add       1                    to   w-vlz-scl-num-pag      .
           move      w-vlz-scl-num-pag    to   w-cnt-tit-num-pag      .
      *              *-------------------------------------------------*
      *              * Intestazione standard                           *
      *              *-------------------------------------------------*
           perform   int-pag-std-000      thru int-pag-std-999        .
      *              *-------------------------------------------------*
      *              * Se interruzione forzata : uscita                *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to int-pag-sta-999.
       int-pag-sta-100.
      *              *-------------------------------------------------*
      *              * Sub-intestazione                                *
      *              *-------------------------------------------------*
       int-pag-sta-150.
      *                  *---------------------------------------------*
      *                  * Test se pagina 1                            *
      *                  *---------------------------------------------*
           if        w-cnt-tit-num-pag    not  = 1
                     go to int-pag-sta-600.
       int-pag-sta-200.
      *                  *---------------------------------------------*
      *                  * Codice e descrizione del prodotto           *
      *                  *---------------------------------------------*
       int-pag-sta-210.
      *                      *-----------------------------------------*
      *                      * Posizionamento verticale subordinato    *
      *                      *-----------------------------------------*
           move      "VS"                 to   p-ope                  .
           add       3
                     p-lnr              giving p-lin                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       int-pag-sta-220.
      *                      *-----------------------------------------*
      *                      * Literal                                 *
      *                      *-----------------------------------------*
       int-pag-sta-230.
      *                          *-------------------------------------*
      *                          * Deviazione a seconda del tipo di    *
      *                          * distinta base                       *
      *                          *-------------------------------------*
           if        rr-tip-dis           =    01
                     go to int-pag-sta-260
           else if   rr-tip-dis           =    02
                     go to int-pag-sta-270
           else if   rr-tip-dis           =    99
                     go to int-pag-sta-280
           else      go to int-pag-sta-260.
       int-pag-sta-260.
      *                          *-------------------------------------*
      *                          * Se prodotto finito                  *
      *                          *-------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      56                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           move      "Codice e descrizione del prodotto finito .........
      -              ".....:"             to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           go to     int-pag-sta-300.
       int-pag-sta-270.
      *                          *-------------------------------------*
      *                          * Se semilavorato                     *
      *                          *-------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      56                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           move      "Codice e descrizione del semilavorato ............
      -              ".....:"             to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           go to     int-pag-sta-300.
       int-pag-sta-280.
      *                          *-------------------------------------*
      *                          * Se sub-distinta virtuale            *
      *                          *-------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      56                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           move      "Codice e descrizione della sub-distinta virtuale .
      -              ".....:"             to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           go to     int-pag-sta-300.
       int-pag-sta-300.
      *                      *-----------------------------------------*
      *                      * Codice alfanumerico                     *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      14                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      058                  to   p-pos                  .
           move      rr-alf-ass           to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       int-pag-sta-320.
      *                      *-----------------------------------------*
      *                      * Descrizione                             *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      074                  to   p-pos                  .
           move      rr-des-ass           to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       int-pag-sta-330.
      *                      *-----------------------------------------*
      *                      * Unita' di misura                        *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se a spaces non si stampa           *
      *                          *-------------------------------------*
           if        rr-umi-ass           =    spaces
                     go to int-pag-sta-400
      *                          *-------------------------------------*
      *                          * Literal                             *
      *                          *-------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      05                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      117                  to   p-pos                  .
           move      "Udm :"              to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                          *-------------------------------------*
      *                          * Valore                              *
      *                          *-------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      03                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      123                  to   p-pos                  .
           move      rr-umi-ass           to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       int-pag-sta-400.
      *                  *---------------------------------------------*
      *                  * Tipo valorizzazione semilavorati finali     *
      *                  *---------------------------------------------*
       int-pag-sta-410.
      *                      *-----------------------------------------*
      *                      * Posizionamento verticale subordinato    *
      *                      *-----------------------------------------*
           move      "VS"                 to   p-ope                  .
           add       2
                     p-lnr              giving p-lin                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       int-pag-sta-420.
      *                      *-----------------------------------------*
      *                      * Literal prima linea                     *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      56                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           move      "Tipo di valorizzazione per i semilavorati finali .
      -              ".....:"             to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       int-pag-sta-430.
      *                      *-----------------------------------------*
      *                      * Valore                                  *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      30                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      058                  to   p-pos                  .
           if        rr-tva-sem           =    01
                     move  "Costo medio ponderato annuale "
                                          to   p-alf
           else if   rr-tva-sem           =    02
                     move  "Ultimo costo d'acquisto       "
                                          to   p-alf
           else if   rr-tva-sem           =    03
                     move  "Costo standard                "
                                          to   p-alf
           else if   rr-tva-sem           =    99
                     move  "In funzione dei componenti    "
                                          to   p-alf
           else      move  all  "."       to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       int-pag-sta-440.
      *                      *-----------------------------------------*
      *                      * Interlinea                              *
      *                      *-----------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       int-pag-sta-450.
      *                      *-----------------------------------------*
      *                      * Literal seconda linea                   *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      56                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           move      "                  inclusi nella distinta scalare  
      -              "      "             to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       int-pag-sta-500.
      *                  *---------------------------------------------*
      *                  * Tipo valorizzazione materie prime           *
      *                  *---------------------------------------------*
       int-pag-sta-510.
      *                      *-----------------------------------------*
      *                      * Posizionamento verticale subordinato    *
      *                      *-----------------------------------------*
           move      "VS"                 to   p-ope                  .
           add       2
                     p-lnr              giving p-lin                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       int-pag-sta-520.
      *                      *-----------------------------------------*
      *                      * Literal prima linea                     *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      56                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           move      "Tipo di valorizzazione per le materie prime ......
      -              ".....:"             to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       int-pag-sta-530.
      *                      *-----------------------------------------*
      *                      * Valore                                  *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      30                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      058                  to   p-pos                  .
           if        rr-tva-map           =    01
                     move  "Costo medio ponderato annuale "
                                          to   p-alf
           else if   rr-tva-map           =    02
                     move  "Ultimo costo d'acquisto       "
                                          to   p-alf
           else if   rr-tva-map           =    03
                     move  "Costo standard                "
                                          to   p-alf
           else      move  all  "."       to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       int-pag-sta-540.
      *                      *-----------------------------------------*
      *                      * Interlinea                              *
      *                      *-----------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       int-pag-sta-550.
      *                      *-----------------------------------------*
      *                      * Literal seconda linea                   *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      56                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           move      "             incluse nella distinta scalare       
      -              "      "             to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       int-pag-sta-600.
      *              *-------------------------------------------------*
      *              * Fincatura                                       *
      *              *-------------------------------------------------*
       int-pag-sta-610.
      *                  *---------------------------------------------*
      *                  * Posizionamento verticale subordinato        *
      *                  *---------------------------------------------*
           move      "VS"                 to   p-ope                  .
           add       3
                     p-lnr              giving p-lin                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       int-pag-sta-620.
      *                  *---------------------------------------------*
      *                  * Fincatura generica                          *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      132                  to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           move      "                                     Descrizione d
      -              "istinta base                      Udm    Quantita'
      -              "   Costo unitario     Valore    "
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       int-pag-sta-630.
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       int-pag-sta-640.
      *                  *---------------------------------------------*
      *                  * Sottolineatura fincatura                    *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      132                  to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           move      "--------------------------------------------------
      -              "--------------------------------- ---  -----------
      -              "-  -------------- --------------"
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       int-pag-sta-650.
      *                  *---------------------------------------------*
      *                  * Posizionamento verticale di separazione     *
      *                  *---------------------------------------------*
           move      "VP"                 to   p-ope                  .
           add       1
                     p-lnr              giving p-lin                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       int-pag-sta-999.
           exit.

      *    *===========================================================*
      *    * Stampa elemento di una distinta                           *
      *    *-----------------------------------------------------------*
       sta-ele-dis-000.
      *              *-------------------------------------------------*
      *              * Calcolo del displacement di stampa, facendo at- *
      *              * tenzione a non superare il massimo livello di   *
      *              * indentazione consentito                         *
      *              *-------------------------------------------------*
           move      w-vlz-scl-liv-stp    to   w-vlz-scl-wds-stp      .
           if        w-vlz-scl-wds-stp    >    06
                     move  06             to   w-vlz-scl-wds-stp      .
           subtract  1                    from w-vlz-scl-wds-stp      .
           multiply  4                    by   w-vlz-scl-wds-stp      .
       sta-ele-dis-100.
      *              *-------------------------------------------------*
      *              * Test se linee residue sufficienti               *
      *              *-------------------------------------------------*
           if        p-res                >    1
                     go to sta-ele-dis-200.
      *              *-------------------------------------------------*
      *              * Intestazione pagina                             *
      *              *-------------------------------------------------*
           perform   int-pag-sta-000      thru int-pag-sta-999        .
      *              *-------------------------------------------------*
      *              * Test se interruzione forzata                    *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to sta-ele-dis-999.
       sta-ele-dis-200.
      *              *-------------------------------------------------*
      *              * Interlinea                                      *
      *              *-------------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       sta-ele-dis-300.
      *              *-------------------------------------------------*
      *              * Livello di profondita                           *
      *              *-------------------------------------------------*
       sta-ele-dis-301.
      *                  *---------------------------------------------*
      *                  * Livello                                     *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      02                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "9"                  to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           add       w-vlz-scl-wds-stp    to   p-pos                  .
           move      w-vlz-scl-liv-stp    to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       sta-ele-dis-302.
      *                  *---------------------------------------------*
      *                  * Parentesi tonda chiusa                      *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      01                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      003                  to   p-pos                  .
           add       w-vlz-scl-wds-stp    to   p-pos                  .
           move      ")"                  to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       sta-ele-dis-325.
      *              *-------------------------------------------------*
      *              * Tipo di elemento da stampare                    *
      *              *-------------------------------------------------*
       sta-ele-dis-326.
      *                  *---------------------------------------------*
      *                  * Parantesi quadra aperta                     *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      01                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      005                  to   p-pos                  .
           add       w-vlz-scl-wds-stp    to   p-pos                  .
           move      "["                  to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       sta-ele-dis-327.
      *                  *---------------------------------------------*
      *                  * Tipo di elemento                            *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      01                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      006                  to   p-pos                  .
           add       w-vlz-scl-wds-stp    to   p-pos                  .
           if        w-vlz-scl-tel-stp    =    01
                     move  "P"            to   p-alf
           else if   w-vlz-scl-tel-stp    =    02
                     move  "S"            to   p-alf
           else if   w-vlz-scl-tel-stp    =    03
                     move  "M"            to   p-alf
           else if   w-vlz-scl-tel-stp    =    99
                     move  "D"            to   p-alf
           else      move  "?"            to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       sta-ele-dis-328.
      *                  *---------------------------------------------*
      *                  * Parantesi quadra chiusa                     *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      01                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      007                  to   p-pos                  .
           add       w-vlz-scl-wds-stp    to   p-pos                  .
           move      "]"                  to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       sta-ele-dis-350.
      *              *-------------------------------------------------*
      *              * Codice elemento da stampare                     *
      *              *-------------------------------------------------*
       sta-ele-dis-351.
      *                  *---------------------------------------------*
      *                  * Se codice alfanumerico diverso da spaces    *
      *                  * si stampa il codice alfanumerico, altri-    *
      *                  * menti quello numerico                       *
      *                  *---------------------------------------------*
           if        w-vlz-scl-alf-stp    =    spaces
                     go to sta-ele-dis-353.
       sta-ele-dis-352.
      *                  *---------------------------------------------*
      *                  * Stampa codice alfanumerico                  *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      14                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      009                  to   p-pos                  .
           add       w-vlz-scl-wds-stp    to   p-pos                  .
           move      w-vlz-scl-alf-stp    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           go to     sta-ele-dis-375.
       sta-ele-dis-353.
      *                  *---------------------------------------------*
      *                  * Stampa codice numerico                      *
      *                  *---------------------------------------------*
       sta-ele-dis-354.
      *                      *-----------------------------------------*
      *                      * Editing                                 *
      *                      *-----------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      07                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      spaces               to   p-edm                  .
           move      w-vlz-scl-num-stp    to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       sta-ele-dis-355.
      *                      *-----------------------------------------*
      *                      * Stampa tra parentesi tonde              *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      09                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      009                  to   p-pos                  .
           add       w-vlz-scl-wds-stp    to   p-pos                  .
           move      spaces               to   p-alf                  .
           string    "("        delimited by   size
                     p-edt      delimited by   spaces
                     ")"        delimited by   size
                                          into p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           go to     sta-ele-dis-375.
       sta-ele-dis-375.
      *              *-------------------------------------------------*
      *              * Descrizione elemento da stampare                *
      *              *-------------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      024                  to   p-pos                  .
           add       w-vlz-scl-wds-stp    to   p-pos                  .
           move      w-vlz-scl-des-stp    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       sta-ele-dis-400.
      *              *-------------------------------------------------*
      *              * Unita' di misura elemento da stampare           *
      *              *-------------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      03                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      085                  to   p-pos                  .
           move      w-vlz-scl-umi-stp    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       sta-ele-dis-425.
      *              *-------------------------------------------------*
      *              * Quantita' elemento da stampare                  *
      *              *-------------------------------------------------*
       sta-ele-dis-426.
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda del tipo di editing    *
      *                  * che deve essere effettuato                  *
      *                  *---------------------------------------------*
           if        w-vlz-scl-qta-stp    >    999999,000 or
                     w-vlz-scl-qta-stp    <   -999999,000
                     go to sta-ele-dis-428.
       sta-ele-dis-427.
      *                  *---------------------------------------------*
      *                  * Se editing raggruppato a tre a tre          *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      06                   to   p-car                  .
           if        w-vlz-scl-ndq-stp    =    zero or
                     w-vlz-scl-ndq-stp    =    1    or
                     w-vlz-scl-ndq-stp    =    2    or
                     w-vlz-scl-ndq-stp    =    3
                     move  "GB"           to   p-edm
                     move  w-vlz-scl-ndq-stp
                                          to   p-dec
           else      move  "GBD"          to   p-edm
                     move  3              to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      p-lnr                to   p-lin                  .
           move      091                  to   p-pos                  .
           move      w-vlz-scl-qta-stp    to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           go to     sta-ele-dis-450.
       sta-ele-dis-428.
      *                  *---------------------------------------------*
      *                  * Se editing non raggruppato                  *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      08                   to   p-car                  .
           if        w-vlz-scl-ndq-stp    =    zero or
                     w-vlz-scl-ndq-stp    =    1    or
                     w-vlz-scl-ndq-stp    =    2    or
                     w-vlz-scl-ndq-stp    =    3
                     move  "B"            to   p-edm
                     move  w-vlz-scl-ndq-stp
                                          to   p-dec
           else      move  "BD"           to   p-edm
                     move  3              to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      p-lnr                to   p-lin                  .
           move      090                  to   p-pos                  .
           move      w-vlz-scl-qta-stp    to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           go to     sta-ele-dis-450.
       sta-ele-dis-450.
      *              *-------------------------------------------------*
      *              * Se costo e valore non sono da stampare si salta *
      *              * la stampa relativa a questi valori              *
      *              *-------------------------------------------------*
           if        w-vlz-scl-snx-cev    not  = "S"
                     go to sta-ele-dis-600.
       sta-ele-dis-475.
      *              *-------------------------------------------------*
      *              * Costo unitario elemento da stampare             *
      *              *-------------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "V"                  to   p-tip                  .
           move      11                   to   p-car                  .
           move      c-dec                to   p-dec                  .
           add       02                   to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "GD"                 to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      104                  to   p-pos                  .
           move      w-vlz-scl-cun-stp    to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       sta-ele-dis-500.
      *              *-------------------------------------------------*
      *              * Segnale di valore a zero                        *
      *              *-------------------------------------------------*
           if        w-vlz-scl-val-stp    not  = zero
                     go to sta-ele-dis-525.
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      01                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      118                  to   p-pos                  .
           move      "*"                  to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       sta-ele-dis-525.
      *              *-------------------------------------------------*
      *              * Valore elemento da stampare                     *
      *              *-------------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      11                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "G"                  to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      119                  to   p-pos                  .
           move      w-vlz-scl-val-stp    to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       sta-ele-dis-600.
      *              *-------------------------------------------------*
      *              * Fine stampa elemento                            *
      *              *-------------------------------------------------*
           go to     sta-ele-dis-999.
       sta-ele-dis-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [dcp]                         *
      *    *-----------------------------------------------------------*
       let-arc-dcp-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-dcp-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice a zero                           *
      *              *-------------------------------------------------*
           if        w-let-arc-dcp-num    =    zero  
                     go to let-arc-dcp-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRO"             to   f-key                  .
           move      w-let-arc-dcp-num    to   rf-dcp-num-pro         .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-dcp-400.
       let-arc-dcp-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           if        rf-dcp-tip-pro       =    01
                     move  "M"            to   w-let-arc-dcp-tpr
           else if   rf-dcp-tip-pro       =    02
                     move  "S"            to   w-let-arc-dcp-tpr
           else if   rf-dcp-tip-pro       =    03
                     move  "I"            to   w-let-arc-dcp-tpr
           else if   rf-dcp-tip-pro       =    09
                     move  "X"            to   w-let-arc-dcp-tpr
           else      move  spaces         to   w-let-arc-dcp-tpr      .
           move      rf-dcp-alf-pro       to   w-let-arc-dcp-alf      .
           move      rf-dcp-des-pro       to   w-let-arc-dcp-des      .
           move      rf-dcp-umi-ven       to   w-let-arc-dcp-umi      .
           move      rf-dcp-dec-qta       to   w-let-arc-dcp-deq      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-dcp-999.
       let-arc-dcp-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-dcp-flg      .
           move      all   "."            to   w-let-arc-dcp-des      .
           go to     let-arc-dcp-600.
       let-arc-dcp-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-dcp-des      .
       let-arc-dcp-600.
           move      spaces               to   w-let-arc-dcp-alf      .
           move      spaces               to   w-let-arc-dcp-tpr      .
           move      spaces               to   w-let-arc-dcp-umi      .
           move      zero                 to   w-let-arc-dcp-deq      .
       let-arc-dcp-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [dps]                         *
      *    *-----------------------------------------------------------*
       let-arc-dps-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-dps-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice a zero                           *
      *              *-------------------------------------------------*
           if        w-let-arc-dps-num    =    zero  
                     go to let-arc-dps-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMSEM"             to   f-key                  .
           move      w-let-arc-dps-num    to   rf-dps-num-sem         .
           move      "pgm/dps/fls/ioc/obj/iofdps"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dps                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-dps-400.
       let-arc-dps-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-dps-alf-sem       to   w-let-arc-dps-alf      .
           move      rf-dps-des-sem       to   w-let-arc-dps-des      .
           move      rf-dps-umi-prd       to   w-let-arc-dps-umi      .
           move      rf-dps-dec-qta       to   w-let-arc-dps-deq      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-dps-999.
       let-arc-dps-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-dps-flg      .
           move      all   "."            to   w-let-arc-dps-des      .
           go to     let-arc-dps-600.
       let-arc-dps-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-dps-des      .
       let-arc-dps-600.
           move      spaces               to   w-let-arc-dps-alf      .
           move      spaces               to   w-let-arc-dps-umi      .
           move      zero                 to   w-let-arc-dps-deq      .
       let-arc-dps-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [lgv]                         *
      *    *-----------------------------------------------------------*
       let-arc-lgv-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-lgv-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice a zero                           *
      *              *-------------------------------------------------*
           if        w-let-arc-lgv-num    =    zero  
                     go to let-arc-lgv-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMLGV"             to   f-key                  .
           move      w-let-arc-lgv-num    to   rf-lgv-num-lgv         .
           move      "pgm/dtp/fls/ioc/obj/ioflgv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lgv                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-lgv-400.
       let-arc-lgv-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-lgv-alf-lgv       to   w-let-arc-lgv-alf      .
           move      rf-lgv-des-lgv       to   w-let-arc-lgv-des      .
           move      rf-lgv-umi-prd       to   w-let-arc-lgv-umi      .
           move      rf-lgv-dec-qta       to   w-let-arc-lgv-deq      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-lgv-999.
       let-arc-lgv-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-lgv-flg      .
           move      all   "."            to   w-let-arc-lgv-des      .
           go to     let-arc-lgv-600.
       let-arc-lgv-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-lgv-des      .
       let-arc-lgv-600.
           move      spaces               to   w-let-arc-lgv-alf      .
           move      spaces               to   w-let-arc-lgv-umi      .
           move      zero                 to   w-let-arc-lgv-deq      .
       let-arc-lgv-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [dpm]                         *
      *    *-----------------------------------------------------------*
       let-arc-dpm-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-dpm-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice a zero                           *
      *              *-------------------------------------------------*
           if        w-let-arc-dpm-num    =    zero  
                     go to let-arc-dpm-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMMAP"             to   f-key                  .
           move      w-let-arc-dpm-num    to   rf-dpm-num-map         .
           move      "pgm/dpm/fls/ioc/obj/iofdpm"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dpm                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-dpm-400.
       let-arc-dpm-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-dpm-alf-map       to   w-let-arc-dpm-alf      .
           move      rf-dpm-des-map       to   w-let-arc-dpm-des      .
           move      rf-dpm-umi-prd       to   w-let-arc-dpm-umi      .
           move      rf-dpm-dec-qta       to   w-let-arc-dpm-deq      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-dpm-999.
       let-arc-dpm-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-dpm-flg      .
           move      all   "."            to   w-let-arc-dpm-des      .
           go to     let-arc-dpm-600.
       let-arc-dpm-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-dpm-des      .
       let-arc-dpm-600.
           move      spaces               to   w-let-arc-dpm-alf      .
           move      spaces               to   w-let-arc-dpm-umi      .
           move      zero                 to   w-let-arc-dpm-deq      .
       let-arc-dpm-999.
           exit.

      *    *===========================================================*
      *    * Subroutines per accettazione codice prodotto 'dcp'        *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/acoddcp0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per accettazione codice semilavorato 'dps'    *
      *    *-----------------------------------------------------------*
           copy      "pgm/dps/prg/cpy/acoddps0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per accettazione codice sub-distinta          *
      *    *-----------------------------------------------------------*
           copy      "pgm/dtp/prg/cpy/acodlgv0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per accettazione codice materia prima         *
      *    *-----------------------------------------------------------*
           copy      "pgm/dpm/prg/cpy/acoddpm0.acs"                   .

      *    *===========================================================*
      *    * Subroutine di esplosione scalare distinta base            *
      *    *                                                           *
      *    * ----------------                                          *
      *    *                                                           *
      *    * Attenzione : Questa routine utilizza numerosi file, che   *
      *    *              sono di seguito elencati. Essi vengono tut-  *
      *    *              ti trattati in lettura, ma nel far questo    *
      *    *              vengono rovinate le Start eventualmente ese- *
      *    *              guite su di essi.                            *
      *    *                                                           *
      *    *              Pertanto i programmi che intendono utilizza- *
      *    *              re la seguente routine non devono assoluta-  *
      *    *              mente utilizzare come files principali i fi- *
      *    *              les utilizzati :                             *
      *    *                                                           *
      *    *                  - [lgt]                                  *
      *    *                  - [lgr]                                  *
      *    *                  - [dcp]                                  *
      *    *                  - [dps]                                  *
      *    *                  - [dpm]                                  *
      *    *                  - [lgv]                                  *
      *    *                                                           *
      *    *              Inoltre non e' possibile utilizzare nel ci-  *
      *    *              clo principale nemmeno i moduli di filtro    *
      *    *              per ordinamento e/o selezione anagrafiche,   *
      *    *              in quanto questi, in alcune circostanze,     *
      *    *              utilizzano a loro volta come files guida i   *
      *    *              files :                                      *
      *    *                                                           *
      *    *                  - [dcp]                                  *
      *    *                  - [dps]                                  *
      *    *                  - [dpm]                                  *
      *    *                                                           *
      *    *              Nei casi in cui si dovesse assolutamente a-  *
      *    *              vere un file guida tra quelli non consentiti *
      *    *              da questa routine, sara' necessario :        *
      *    *                                                           *
      *    *                  - Preparare preventivamente un file tem- *
      *    *                    poraneo di appoggio come file guida    *
      *    *                                                           *
      *    *                  - Utilizzare tale file temporaneo di ap- *
      *    *                    poggio come file guida                 *
      *    *                                                           *
      *    *                  - Cancellare tale file temporaneo di ap- *
      *    *                    poggio alla fine dell'esecuzione       *
      *    *                                                           *
      *    *                  Nota : Il file di appoggio temporaneo    *
      *    *                         puo' essere costituito da un fi-  *
      *    *                         le di sort, in quanto questa ro-  *
      *    *                         utine non utilizza il sort.       *
      *    *                                                           *
      *    * ----------------                                          *
      *    *                                                           *
      *    * Presume che preventivamente siano stati definiti ed aper- *
      *    * ti i seguenti files:                                      *
      *    *                                                           *
      *    *  - [lgt] : Testate distinte base                          *
      *    *  - [lgr] : Righe distinte base                            *
      *    *  - [dcp] : Anagrafica prodotti finiti                     *
      *    *  - [dps] : Anagrafica semilavorati                        *
      *    *  - [dpm] : Anagrafica materie prime                       *
      *    *  - [lgv] : Anagrafica subdistinte virtuali                *
      *    *                                                           *
      *    * ----------------                                          *
      *    *                                                           *
      *    * - Funzione 'E-INI'                                        *
      *    *                                                           *
      *    *   Deve necessariamente essere richiamata, per ogni esplo- *
      *    *   sione da eseguire, prima delle altre funzioni.          *
      *    *                                                           *
      *    *   - Input  : w-esp-scl-dtp-fun = 'E-INI'                  *
      *    *                                                           *
      *    *              w-esp-scl-dtp-tde = Tipo codice magazzino da *
      *    *                                  esplodere                *
      *    *                                   - 01 : Prodotto finito  *
      *    *                                   - 02 : Semilavorato     *
      *    *                                   - 99 : Subdistinta vir- *
      *    *                                          tuale            *
      *    *                                                           *
      *    *              w-esp-scl-dtp-nde = Codice numerico magazzi- *
      *    *                                  no da esplodere o codice *
      *    *                                  numerico della subdis-   *
      *    *                                  tinta virtuale           *
      *    *                                                           *
      *    *              w-esp-scl-dtp-ade = Codice alfanumerico cor- *
      *    *                                  rispondente al codice    *
      *    *                                  numerico precedente      *
      *    *                                                           *
      *    *              w-esp-scl-dtp-qde = Quantita' da esplodere   *
      *    *                                                           *
      *    *   - Output : w-esp-scl-dtp-tip = Tipo di elemento         *
      *    *                                                           *
      *    *              w-esp-scl-dtp-num = Codice numerico dell'e-  *
      *    *                                  lemento                  *
      *    *                                                           *
      *    *              w-esp-scl-dtp-alf = Codice alfanumerico del- *
      *    *                                  l'elemento               *
      *    *                                                           *
      *    *              w-esp-scl-dtp-liv = Livello di profondita'   *
      *    *                                  relativo all'elemento,   *
      *    *                                  forzato a zero           *
      *    *                                                           *
      *    *              w-esp-scl-dtp-qta = Quantita' totale relati- *
      *    *                                  va all'elemento          *
      *    *                                                           *
      *    *              w-esp-scl-dtp-ana = Segnale di anagrafica e- *
      *    *                                  sistente                 *
      *    *                                   - Spaces : Si           *
      *    *                                   - '#'    : No           *
      *    *                                                           *
      *    *              w-esp-scl-dtp-des = Descrizione da anagrafi- *
      *    *                                  ca elemento              *
      *    *                                                           *
      *    *              w-esp-scl-dtp-umi = Unita' di misura da ana- *
      *    *                                  grafica elemento         *
      *    *                                                           *
      *    *              w-esp-scl-dtp-dec = Numero decimali quantita'*
      *    *                                  da anagrafica elemento   *
      *    *                                                           *
      *    *              rf-dcp, oppure    = Anagrafica elemento, a   *
      *    *              rf-dps, oppure      seconda del tipo ele-    *
      *    *              rf-dpm, oppure      mento, se anagrafica e-  *
      *    *              rf-lgv              sistente                 *
      *    *                                                           *
      *    * ----------------                                          *
      *    *                                                           *
      *    * - Funzione 'E-GET'                                        *
      *    *                                                           *
      *    *   Ottenimento dell'elemento successivo relativo all'es-   *
      *    *   plosione scalare.                                       *
      *    *                                                           *
      *    *   - Input  : w-esp-scl-dtp-fun = 'E-GET'                  *
      *    *                                                           *
      *    *   - Output : w-esp-scl-dtp-sts = Status di uscita         *
      *    *                                   - Spaces : Ok, elemento *
      *    *                                              successivo a *
      *    *                                              disposizione *
      *    *                                   - '#'    : Fine elemen- *
      *    *                                              ti nella di- *
      *    *                                              stinta       *
      *    *                                                           *
      *    *              w-esp-scl-dtp-tip = Tipo di elemento trovato *
      *    *                                  nella scansione          *
      *    *                                   - 01 : Prodotto finito  *
      *    *                                   - 02 : Semilavorato     *
      *    *                                   - 03 : Materia prima    *
      *    *                                   - 99 : Subdistinta vir- *
      *    *                                          tuale            *
      *    *                                                           *
      *    *              w-esp-scl-dtp-num = Codice numerico dell'e-  *
      *    *                                  lemento trovato nella    *
      *    *                                  scansione                *
      *    *                                                           *
      *    *              w-esp-scl-dtp-alf = Codice alfanumerico del- *
      *    *                                  l'elemento               *
      *    *                                                           *
      *    *              w-esp-scl-dtp-liv = Livello di profondita'   *
      *    *                                  relativo all'elemento    *
      *    *                                  trovato nella scansione, *
      *    *                                  assumendo come base il   *
      *    *                                  livello zero relativo    *
      *    *                                  all'assieme da esplodere *
      *    *                                                           *
      *    *              w-esp-scl-dtp-qta = Quantita' totale relati- *
      *    *                                  va all'elemento trovato  *
      *    *                                  nella scansione          *
      *    *                                                           *
      *    *              w-esp-scl-dtp-ana = Segnale di anagrafica e- *
      *    *                                  sistente                 *
      *    *                                   - Spaces : Si           *
      *    *                                   - '#'    : No           *
      *    *                                                           *
      *    *              w-esp-scl-dtp-des = Descrizione da anagrafi- *
      *    *                                  ca elemento              *
      *    *                                                           *
      *    *              w-esp-scl-dtp-umi = Unita' di misura da ana- *
      *    *                                  grafica elemento         *
      *    *                                                           *
      *    *              w-esp-scl-dtp-dec = Numero decimali quantita'*
      *    *                                  da anagrafica elemento   *
      *    *                                                           *
      *    *              rf-dcp, oppure    = Anagrafica elemento, a   *
      *    *              rf-dps, oppure      seconda del tipo ele-    *
      *    *              rf-dpm, oppure      mento, se anagrafica e-  *
      *    *              rf-lgv              sistente                 *
      *    *                                                           *
      *    * ----------------                                          *
      *    *                                                           *
      *    * - Funzione 'E-SKP'                                        *
      *    *                                                           *
      *    *   Ottenimento dell'elemento successivo dello stesso li-   *
      *    *   vello dell'ultimo elemento trovato, evitando in tal     *
      *    *   modo di entrare piu' in profondita' per quanto riguar-  *
      *    *   da i semilavorati e le subdistinte virtuali.            *
      *    *                                                           *
      *    *   - Input  : w-esp-scl-dtp-fun = 'E-SKP'                  *
      *    *                                                           *
      *    *   - Output : Esattamente come per funzione 'E-GET'        *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       esp-scl-dtp-000.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda della funzione richiesta   *
      *              *-------------------------------------------------*
           if        w-esp-scl-dtp-fun    =    "E-INI"
                     go to esp-scl-dtp-050
           else if   w-esp-scl-dtp-fun    =    "E-GET"
                     go to esp-scl-dtp-100
           else if   w-esp-scl-dtp-fun    =    "E-SKP"
                     go to esp-scl-dtp-250
           else      go to esp-scl-dtp-999.
       esp-scl-dtp-050.
      *              *-------------------------------------------------*
      *              * Funzione 'E-INI'                                *
      *              *-------------------------------------------------*
       esp-scl-dtp-055.
      *                  *---------------------------------------------*
      *                  * Livello di lavoro a : 01                    *
      *                  *---------------------------------------------*
           move      01                   to   w-esp-scl-dtp-wlp      .
      *                  *---------------------------------------------*
      *                  * Segnale di prossima operazione da eseguire  *
      *                  * a : Start sul livello di profondita interno *
      *                  * attuale                                     *
      *                  *---------------------------------------------*
           move      "S"                  to   w-esp-scl-dtp-wpo      .
       esp-scl-dtp-060.
      *                  *---------------------------------------------*
      *                  * Preparazione valori per il 1. livello       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Tipo elemento                           *
      *                      *-----------------------------------------*
           move      w-esp-scl-dtp-tde    to   w-esp-scl-dtp-wti (01) .
      *                      *-----------------------------------------*
      *                      * Codice numerico elemento                *
      *                      *-----------------------------------------*
           move      w-esp-scl-dtp-nde    to   w-esp-scl-dtp-wnu (01) .
      *                      *-----------------------------------------*
      *                      * Codice alfanumerico elemento            *
      *                      *-----------------------------------------*
           move      w-esp-scl-dtp-ade    to   w-esp-scl-dtp-wal (01) .
      *                      *-----------------------------------------*
      *                      * Numero progressivo per riga distinta    *
      *                      *-----------------------------------------*
           move      zero                 to   w-esp-scl-dtp-wrg (01) .
      *                      *-----------------------------------------*
      *                      * Lettura anagrafica elemento e determi-  *
      *                      * nazione dei seguenti valori:            *
      *                      *  - Elemento anagraficamente esistente   *
      *                      *  - Descrizione anagrafica elemento      *
      *                      *  - Unita' di misura elemento            *
      *                      *  - Numero decimali per quantita'        *
      *                      *-----------------------------------------*
           perform   esp-scl-dtp-800      thru esp-scl-dtp-809        .
      *                      *-----------------------------------------*
      *                      * Quantita' relativa all'elemento         *
      *                      *-----------------------------------------*
           move      w-esp-scl-dtp-qde    to   w-esp-scl-dtp-wqt (01) .
           if        w-esp-scl-dtp-wnd (01)
                                          =    zero
                     add     0,499        to   w-esp-scl-dtp-wqt (01)
                     divide   1000        into w-esp-scl-dtp-wqt (01)
                                                         rounded
                     multiply 1000        by   w-esp-scl-dtp-wqt (01)
           else if   w-esp-scl-dtp-wnd (01)
                                          =    1
                     add     0,049        to   w-esp-scl-dtp-wqt (01)
                     divide    100        into w-esp-scl-dtp-wqt (01)
                                                         rounded
                     multiply  100        by   w-esp-scl-dtp-wqt (01)
           else if   w-esp-scl-dtp-wnd (01)
                                          =    2
                     add     0,004        to   w-esp-scl-dtp-wqt (01)
                     divide     10        into w-esp-scl-dtp-wqt (01)
                                                         rounded
                     multiply   10        by   w-esp-scl-dtp-wqt (01) .
       esp-scl-dtp-065.
      *                  *---------------------------------------------*
      *                  * Preparazione valori in uscita, escluso lo   *
      *                  * status, relativi all'elemento               *
      *                  *---------------------------------------------*
           perform   esp-scl-dtp-820      thru esp-scl-dtp-829        .
       esp-scl-dtp-070.
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     esp-scl-dtp-999.
       esp-scl-dtp-100.
      *              *-------------------------------------------------*
      *              * Funzione 'E-GET'                                *
      *              *-------------------------------------------------*
       esp-scl-dtp-105.
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda del;valore del segnale *
      *                  * di prossima operazione da eseguire          *
      *                  *---------------------------------------------*
           if        w-esp-scl-dtp-wpo    =    "S"
                     go to esp-scl-dtp-110
           else if   w-esp-scl-dtp-wpo    =    "U"
                     go to esp-scl-dtp-140
           else      go to esp-scl-dtp-145.
       esp-scl-dtp-110.
      *                  *---------------------------------------------*
      *                  * Se operazione da eseguire : Start sul li-   *
      *                  * vello di profondita' interno attuale        *
      *                  *---------------------------------------------*
       esp-scl-dtp-115.
      *                      *-----------------------------------------*
      *                      * Esecuzione 'Start'                      *
      *                      *-----------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "GT"                 to   f-cfr                  .
           move      "TNMASS    "         to   f-key                  .
           move      w-esp-scl-dtp-wti
                    (w-esp-scl-dtp-wlp)   to   rf-lgr-tpm-ass         .
           move      w-esp-scl-dtp-wnu
                    (w-esp-scl-dtp-wlp)   to   rf-lgr-nrm-ass         .
           move      w-esp-scl-dtp-wrg
                    (w-esp-scl-dtp-wlp)   to   rf-lgr-num-prg         .
           move      "pgm/dtp/fls/ioc/obj/ioflgr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lgr                 .
      *                      *-----------------------------------------*
      *                      * Deviazione a seconda dell'esito della   *
      *                      * 'Start'                                 *
      *                      *-----------------------------------------*
           if        f-sts                =    e-not-err
                     go to esp-scl-dtp-130.
       esp-scl-dtp-120.
      *                      *-----------------------------------------*
      *                      * Se 'Start' errata : non ci sono altri   *
      *                      * elementi nella distinta o subdistinta   *
      *                      *-----------------------------------------*
       esp-scl-dtp-125.
      *                          *-------------------------------------*
      *                          * Decremento livello di profondita'   *
      *                          * interno                             *
      *                          *-------------------------------------*
           subtract  1                    from w-esp-scl-dtp-wlp      .
      *                          *-------------------------------------*
      *                          * Se raggiunto il livello di profon-  *
      *                          * dita' interno zero : uscita con se- *
      *                          * gnale di fine scansione             *
      *                          *-------------------------------------*
           if        w-esp-scl-dtp-wlp    =    zero
                     move  "#"            to   w-esp-scl-dtp-sts
                     go to esp-scl-dtp-999.
      *                          *-------------------------------------*
      *                          * Segnale di prossima operazione da   *
      *                          * eseguire : Start sul livello di     *
      *                          * profondita' attuale                 *
      *                          *-------------------------------------*
           move      "S"                  to   w-esp-scl-dtp-wpo      .
      *                          *-------------------------------------*
      *                          * Riciclo per esecuzione Start        *
      *                          *-------------------------------------*
           go to     esp-scl-dtp-105.
       esp-scl-dtp-130.
      *                      *-----------------------------------------*
      *                      * Se 'Start' andata a buon fine : possono *
      *                      * esserci altri elementi nella distinta o *
      *                      * subdistinta                             *
      *                      *-----------------------------------------*
       esp-scl-dtp-135.
      *                          *-------------------------------------*
      *                          * Segnale di prossima operazione da   *
      *                          * eseguire : Read Next sul livello di *
      *                          * profondita' attuale                 *
      *                          *-------------------------------------*
           move      "N"                  to   w-esp-scl-dtp-wpo      .
      *                          *-------------------------------------*
      *                          * Riciclo per esecuzione Read Next    *
      *                          *-------------------------------------*
           go to     esp-scl-dtp-105.
       esp-scl-dtp-140.
      *                  *---------------------------------------------*
      *                  * Se operazione da eseguire : Incremento di   *
      *                  * un livello e Start sul livello di profondi- *
      *                  * ta' interno aumentato                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Incremento livello di profondita' in-   *
      *                      * terno                                   *
      *                      *-----------------------------------------*
           add       1                    to   w-esp-scl-dtp-wlp      .
      *                      *-----------------------------------------*
      *                      * Segnale di prossima operazione da ese-  *
      *                      * guire : Start sul livello di profondi-  *
      *                      * ta' interno attuale                     *
      *                      *-----------------------------------------*
           move      "S"                  to   w-esp-scl-dtp-wpo      .
      *                      *-----------------------------------------*
      *                      * Riciclo per esecuzione Start            *
      *                      *-----------------------------------------*
           go to     esp-scl-dtp-105.
       esp-scl-dtp-145.
      *                  *---------------------------------------------*
      *                  * Se operazione da eseguire : Read Next sul   *
      *                  * livello di profondita' attuale              *
      *                  *---------------------------------------------*
       esp-scl-dtp-150.
      *                      *-----------------------------------------*
      *                      * Esecuzione 'Read Next'                  *
      *                      *-----------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dtp/fls/ioc/obj/ioflgr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lgr                 .
      *                      *-----------------------------------------*
      *                      * Deviazione a seconda dell'esito della   *
      *                      * 'Read Next'                             *
      *                      *-----------------------------------------*
           if        f-sts                =    e-not-err
                     go to esp-scl-dtp-160.
       esp-scl-dtp-155.
      *                      *-----------------------------------------*
      *                      * Se 'At end' : non ci sono altri elemen- *
      *                      * ti nella distinta o subdistinta         *
      *                      *-----------------------------------------*
           go to     esp-scl-dtp-120.
       esp-scl-dtp-160.
      *                      *-----------------------------------------*
      *                      * Se 'Read Next' andata a buon fine       *
      *                      *-----------------------------------------*
       esp-scl-dtp-165.
      *                          *-------------------------------------*
      *                          * Test max per verificare se si e'    *
      *                          * raggiunta la fine della distinta o  *
      *                          * subdistinta, e deviazione a seconda *
      *                          * dell'esito del test                 *
      *                          *-------------------------------------*
           if        rf-lgr-tpm-ass       =    w-esp-scl-dtp-wti
                                              (w-esp-scl-dtp-wlp) and
                     rf-lgr-nrm-ass       =    w-esp-scl-dtp-wnu
                                              (w-esp-scl-dtp-wlp)
                     go to esp-scl-dtp-175.
       esp-scl-dtp-170.
      *                          *-------------------------------------*
      *                          * Se si e' raggiunta la fine della    *
      *                          * distinta o subdistinta              *
      *                          *-------------------------------------*
           go to     esp-scl-dtp-120.
       esp-scl-dtp-175.
      *                          *-------------------------------------*
      *                          * Se si e' ancora nell'ambito della   *
      *                          * distinta o subdistinta del livello  *
      *                          * di profondita' interno attuale      *
      *                          *-------------------------------------*
       esp-scl-dtp-180.
      *                              *---------------------------------*
      *                              * Aggiornamento del numero pro-   *
      *                              * gressivo riga relativo al li-   *
      *                              * vello di profondita' interno    *
      *                              * attuale                         *
      *                              *---------------------------------*
           move      rf-lgr-num-prg       to   w-esp-scl-dtp-wrg
                                              (w-esp-scl-dtp-wlp)     .
       esp-scl-dtp-185.
      *                              *---------------------------------*
      *                              * Salvataggio livello di profon-  *
      *                              * dita' interno attuale           *
      *                              *---------------------------------*
           move      w-esp-scl-dtp-wlp    to   w-esp-scl-dtp-wls      .
      *                              *---------------------------------*
      *                              * Incremento livello di profondi- *
      *                              * ta' interno attuale             *
      *                              *---------------------------------*
           add       1                    to   w-esp-scl-dtp-wlp      .
       esp-scl-dtp-190.
      *                              *---------------------------------*
      *                              * Preparazione valori per il li-  *
      *                              * vello di profondita' interno    *
      *                              * attuale aumentato di 1          *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Tipo elemento               *
      *                                  *-----------------------------*
           move      rf-lgr-tpm-cpt       to   w-esp-scl-dtp-wti
                                              (w-esp-scl-dtp-wlp)     .
      *                                  *-----------------------------*
      *                                  * Codice numerico elemento    *
      *                                  *-----------------------------*
           move      rf-lgr-nrm-cpt       to   w-esp-scl-dtp-wnu
                                              (w-esp-scl-dtp-wlp)     .
      *                                  *-----------------------------*
      *                                  * Codice alfanumerico elemen- *
      *                                  * to                          *
      *                                  *-----------------------------*
           move      rf-lgr-afm-cpt       to   w-esp-scl-dtp-wal
                                              (w-esp-scl-dtp-wlp)     .
      *                                  *-----------------------------*
      *                                  * Numero progressivo di riga  *
      *                                  * distinta                    *
      *                                  *-----------------------------*
           move      zero                to   w-esp-scl-dtp-wrg
                                             (w-esp-scl-dtp-wlp)      .
      *                                  *-----------------------------*
      *                                  * Lettura anagrafica elemento *
      *                                  * e determinazione dei se-    *
      *                                  * guenti valori :             *
      *                                  *  - Elemento anagraficamente *
      *                                  *    esistente                *
      *                                  *  - Descrizione anagrafica   *
      *                                  *    elemento                 *
      *                                  *  - Unita' di misura elemen- *
      *                                  *    to                       *
      *                                  *  - Numero decimali per la   *
      *                                  *    quantita'                *
      *                                  *-----------------------------*
           perform   esp-scl-dtp-800      thru esp-scl-dtp-809        .
      *                                  *-----------------------------*
      *                                  * Quantita' relativa all'ele- *
      *                                  * mento                       *
      *                                  *-----------------------------*
           move      w-esp-scl-dtp-wqt
                    (w-esp-scl-dtp-wls)  to   w-esp-scl-dtp-wqt
                                             (w-esp-scl-dtp-wlp)      .
           multiply  rf-lgr-qta-ipm      by   w-esp-scl-dtp-wqt
                                             (w-esp-scl-dtp-wlp)      .
           if        rf-lgr-qta-ipd      not  = zero
                     divide   rf-lgr-qta-ipd
                                         into w-esp-scl-dtp-wqt
                                             (w-esp-scl-dtp-wlp)      .
           if        w-esp-scl-dtp-wnd
                    (w-esp-scl-dtp-wlp)   =    zero
                     add     0,499        to   w-esp-scl-dtp-wqt
                                              (w-esp-scl-dtp-wlp)
                     divide   1000        into w-esp-scl-dtp-wqt
                                              (w-esp-scl-dtp-wlp)
                                                         rounded
                     multiply 1000        by   w-esp-scl-dtp-wqt
                                              (w-esp-scl-dtp-wlp)
           else if   w-esp-scl-dtp-wnd
                    (w-esp-scl-dtp-wlp)   =    1
                     add     0,049        to   w-esp-scl-dtp-wqt
                                              (w-esp-scl-dtp-wlp)
                     divide    100        into w-esp-scl-dtp-wqt
                                              (w-esp-scl-dtp-wlp)
                                                         rounded
                     multiply  100        by   w-esp-scl-dtp-wqt
                                              (w-esp-scl-dtp-wlp)
           else if   w-esp-scl-dtp-wnd
                    (w-esp-scl-dtp-wlp)   =    2
                     add     0,004        to   w-esp-scl-dtp-wqt
                                              (w-esp-scl-dtp-wlp)
                     divide     10        into w-esp-scl-dtp-wqt
                                              (w-esp-scl-dtp-wlp)
                                                         rounded
                     multiply   10        by   w-esp-scl-dtp-wqt
                                              (w-esp-scl-dtp-wlp)     .
       esp-scl-dtp-195.
      *                              *---------------------------------*
      *                              * Preparazione valori in uscita,  *
      *                              * escluso lo status, relativi     *
      *                              * all'elemento                    *
      *                              *---------------------------------*
           perform   esp-scl-dtp-820      thru esp-scl-dtp-829        .
       esp-scl-dtp-200.
      *                              *---------------------------------*
      *                              * Preparazione della prossima o-  *
      *                              * perazione da eseguire, a se-    *
      *                              * conda del tipo di elemento      *
      *                              *                                 *
      *                              *   - Prodotto finito       : 'U' *
      *                              *                                 *
      *                              *   - Semilavorato          : 'U' *
      *                              *                                 *
      *                              *   - Materia prima         : 'N' *
      *                              *                                 *
      *                              *   - Subdistinta virtuale  : 'U' *
      *                              *                                 *
      *                              *   - Tipo non riconosciuto : 'N' *
      *                              *                                 *
      *                              *---------------------------------*
           if        w-esp-scl-dtp-wti
                    (w-esp-scl-dtp-wlp)   =    01 or
                     w-esp-scl-dtp-wti
                    (w-esp-scl-dtp-wlp)   =    02 or
                     w-esp-scl-dtp-wti
                    (w-esp-scl-dtp-wlp)   =    99
                     move  "U"            to   w-esp-scl-dtp-wpo
           else      move  "N"            to   w-esp-scl-dtp-wpo      .
       esp-scl-dtp-205.
      *                              *---------------------------------*
      *                              * Ripristino livello di profon-   *
      *                              * dita' interno attuale           *
      *                              *---------------------------------*
           move      w-esp-scl-dtp-wls    to   w-esp-scl-dtp-wlp      .
       esp-scl-dtp-210.
      *                              *---------------------------------*
      *                              * Status di uscita a : Ok         *
      *                              *---------------------------------*
           move      spaces               to   w-esp-scl-dtp-sts      .
       esp-scl-dtp-215.
      *                              *---------------------------------*
      *                              * Uscita                          *
      *                              *---------------------------------*
           go to     esp-scl-dtp-999.
       esp-scl-dtp-250.
      *              *-------------------------------------------------*
      *              * Funzione 'E-SKP'                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se il segnale di prossima operazione da e-  *
      *                  * seguire e' pari a 'U' lo si porta a 'N'     *
      *                  *---------------------------------------------*
           if        w-esp-scl-dtp-wpo    =    "U"
                     move  "N"            to   w-esp-scl-dtp-wpo      .
      *                  *---------------------------------------------*
      *                  * Dopodiche' si esegue la funzione 'E-GET'    *
      *                  *---------------------------------------------*
           go to     esp-scl-dtp-100.
       esp-scl-dtp-800.
      *              *-------------------------------------------------*
      *              * Subroutine per la lettura dell'anagrafica rela- *
      *              * tiva all'elemento di tipo e codice numerico di  *
      *              * cui al livello di profondita' interna definito  *
      *              * in : w-esp-scl-dtp-wlp                          *
      *              *-------------------------------------------------*
       esp-scl-dtp-801.
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda del tipo elemento      *
      *                  *---------------------------------------------*
           if        w-esp-scl-dtp-wti
                    (w-esp-scl-dtp-wlp)   =    01
                     go to esp-scl-dtp-802
           else if   w-esp-scl-dtp-wti
                    (w-esp-scl-dtp-wlp)   =    02
                     go to esp-scl-dtp-803
           else if   w-esp-scl-dtp-wti
                    (w-esp-scl-dtp-wlp)   =    03
                     go to esp-scl-dtp-804
           else if   w-esp-scl-dtp-wti
                    (w-esp-scl-dtp-wlp)   =    99
                     go to esp-scl-dtp-805
           else      go to esp-scl-dtp-808.
       esp-scl-dtp-802.
      *                  *---------------------------------------------*
      *                  * Se tipo elemento 01 : Prodotto finito       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Lettura anagrafica [dcp]                *
      *                      *-----------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRO"             to   f-key                  .
           move      w-esp-scl-dtp-wnu
                    (w-esp-scl-dtp-wlp)   to   rf-dcp-num-pro         .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
      *                      *-----------------------------------------*
      *                      * Se anagrafica non esistente             *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     move  "#"            to   w-esp-scl-dtp-wes
                                              (w-esp-scl-dtp-wlp)
                     move  all "."        to   w-esp-scl-dtp-wde
                                              (w-esp-scl-dtp-wlp)
                     move  all "."        to   w-esp-scl-dtp-wum
                                              (w-esp-scl-dtp-wlp)
                     move  zero           to   w-esp-scl-dtp-wnd
                                              (w-esp-scl-dtp-wlp)
                     go to esp-scl-dtp-809.
      *                      *-----------------------------------------*
      *                      * Se anagrafica esistente                 *
      *                      *-----------------------------------------*
           move      spaces               to   w-esp-scl-dtp-wes
                                              (w-esp-scl-dtp-wlp)     .
           move      rf-dcp-des-pro       to   w-esp-scl-dtp-wde
                                              (w-esp-scl-dtp-wlp)     .
           move      rf-dcp-umi-ven       to   w-esp-scl-dtp-wum
                                              (w-esp-scl-dtp-wlp)     .
           move      rf-dcp-dec-qta       to   w-esp-scl-dtp-wnd
                                              (w-esp-scl-dtp-wlp)     .
           go to     esp-scl-dtp-809.
       esp-scl-dtp-803.
      *                  *---------------------------------------------*
      *                  * Se tipo elemento 02 : Semilavorato          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Lettura anagrafica [dps]                *
      *                      *-----------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMSEM"             to   f-key                  .
           move      w-esp-scl-dtp-wnu
                    (w-esp-scl-dtp-wlp)   to   rf-dps-num-sem         .
           move      "pgm/dps/fls/ioc/obj/iofdps"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dps                 .
      *                      *-----------------------------------------*
      *                      * Se anagrafica non esistente             *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     move  "#"            to   w-esp-scl-dtp-wes
                                              (w-esp-scl-dtp-wlp)
                     move  all "."        to   w-esp-scl-dtp-wde
                                              (w-esp-scl-dtp-wlp)
                     move  all "."        to   w-esp-scl-dtp-wum
                                              (w-esp-scl-dtp-wlp)
                     move  zero           to   w-esp-scl-dtp-wnd
                                              (w-esp-scl-dtp-wlp)
                     go to esp-scl-dtp-809.
      *                      *-----------------------------------------*
      *                      * Se anagrafica esistente                 *
      *                      *-----------------------------------------*
           move      spaces               to   w-esp-scl-dtp-wes
                                              (w-esp-scl-dtp-wlp)     .
           move      rf-dps-des-sem       to   w-esp-scl-dtp-wde
                                              (w-esp-scl-dtp-wlp)     .
           move      rf-dps-umi-prd       to   w-esp-scl-dtp-wum
                                              (w-esp-scl-dtp-wlp)     .
           move      rf-dps-dec-qta       to   w-esp-scl-dtp-wnd
                                              (w-esp-scl-dtp-wlp)     .
           go to     esp-scl-dtp-809.
       esp-scl-dtp-804.
      *                  *---------------------------------------------*
      *                  * Se tipo elemento 03 : Materia prima         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Lettura anagrafica [dpm]                *
      *                      *-----------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMMAP"             to   f-key                  .
           move      w-esp-scl-dtp-wnu
                    (w-esp-scl-dtp-wlp)   to   rf-dpm-num-map         .
           move      "pgm/dpm/fls/ioc/obj/iofdpm"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dpm                 .
      *                      *-----------------------------------------*
      *                      * Se anagrafica non esistente             *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     move  "#"            to   w-esp-scl-dtp-wes
                                              (w-esp-scl-dtp-wlp)
                     move  all "."        to   w-esp-scl-dtp-wde
                                              (w-esp-scl-dtp-wlp)
                     move  all "."        to   w-esp-scl-dtp-wum
                                              (w-esp-scl-dtp-wlp)
                     move  zero           to   w-esp-scl-dtp-wnd
                                              (w-esp-scl-dtp-wlp)
                     go to esp-scl-dtp-809.
      *                      *-----------------------------------------*
      *                      * Se anagrafica esistente                 *
      *                      *-----------------------------------------*
           move      spaces               to   w-esp-scl-dtp-wes
                                              (w-esp-scl-dtp-wlp)     .
           move      rf-dpm-des-map       to   w-esp-scl-dtp-wde
                                              (w-esp-scl-dtp-wlp)     .
           move      rf-dpm-umi-prd       to   w-esp-scl-dtp-wum
                                              (w-esp-scl-dtp-wlp)     .
           move      rf-dpm-dec-qta       to   w-esp-scl-dtp-wnd
                                              (w-esp-scl-dtp-wlp)     .
           go to     esp-scl-dtp-809.
       esp-scl-dtp-805.
      *                  *---------------------------------------------*
      *                  * Se tipo elemento 99 : Subdistinta virtuale  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Lettura anagrafica [lgv]                *
      *                      *-----------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMLGV"             to   f-key                  .
           move      w-esp-scl-dtp-wnu
                    (w-esp-scl-dtp-wlp)   to   rf-lgv-num-lgv         .
           move      "pgm/dtp/fls/ioc/obj/ioflgv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lgv                 .
      *                      *-----------------------------------------*
      *                      * Se anagrafica non esistente             *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     move  "#"            to   w-esp-scl-dtp-wes
                                              (w-esp-scl-dtp-wlp)
                     move  all "."        to   w-esp-scl-dtp-wde
                                              (w-esp-scl-dtp-wlp)
                     move  all "."        to   w-esp-scl-dtp-wum
                                              (w-esp-scl-dtp-wlp)
                     move  zero           to   w-esp-scl-dtp-wnd
                                              (w-esp-scl-dtp-wlp)
                     go to esp-scl-dtp-809.
      *                      *-----------------------------------------*
      *                      * Se anagrafica esistente                 *
      *                      *-----------------------------------------*
           move      spaces               to   w-esp-scl-dtp-wes
                                              (w-esp-scl-dtp-wlp)     .
           move      rf-lgv-des-lgv       to   w-esp-scl-dtp-wde
                                              (w-esp-scl-dtp-wlp)     .
           move      rf-lgv-umi-prd       to   w-esp-scl-dtp-wum
                                              (w-esp-scl-dtp-wlp)     .
           move      rf-lgv-dec-qta       to   w-esp-scl-dtp-wnd
                                              (w-esp-scl-dtp-wlp)     .
           go to     esp-scl-dtp-809.
       esp-scl-dtp-808.
      *                  *---------------------------------------------*
      *                  * Se tipo elemento di tipo non riconosciuto   *
      *                  *---------------------------------------------*
           move      "#"                  to   w-esp-scl-dtp-wes
                                              (w-esp-scl-dtp-wlp)     .
           move      all  "."             to   w-esp-scl-dtp-wde
                                              (w-esp-scl-dtp-wlp)     .
           move      all  "."             to   w-esp-scl-dtp-wum
                                              (w-esp-scl-dtp-wlp)     .
           move      zero                 to   w-esp-scl-dtp-wnd
                                              (w-esp-scl-dtp-wlp)     .
           go to     esp-scl-dtp-809.
       esp-scl-dtp-809.
           exit.
       esp-scl-dtp-820.
      *              *-------------------------------------------------*
      *              * Subroutine per la preparazione dei valori in u- *
      *              * scita relativi all'elemento di cui al livello   *
      *              * di profondita' di : w-esp-scl-dtp-wlp.          *
      *              * Ad esclusione dello status di uscita            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Tipo elemento                               *
      *                  *---------------------------------------------*
           move      w-esp-scl-dtp-wti
                    (w-esp-scl-dtp-wlp)   to   w-esp-scl-dtp-tip      .
      *                  *---------------------------------------------*
      *                  * Codice numerico elemento                    *
      *                  *---------------------------------------------*
           move      w-esp-scl-dtp-wnu
                    (w-esp-scl-dtp-wlp)   to   w-esp-scl-dtp-num      .
      *                  *---------------------------------------------*
      *                  * Codice alfanumerico elemento                *
      *                  *---------------------------------------------*
           move      w-esp-scl-dtp-wal
                    (w-esp-scl-dtp-wlp)   to   w-esp-scl-dtp-alf      .
      *                  *---------------------------------------------*
      *                  * Livello di profondita' esterno, pari al li- *
      *                  * vello di profondita' interno diminuito di 1 *
      *                  *---------------------------------------------*
           move      w-esp-scl-dtp-wlp    to   w-esp-scl-dtp-liv      .
           subtract  1                    from w-esp-scl-dtp-liv      .
      *                  *---------------------------------------------*
      *                  * Quantita' relativa all'elemento             *
      *                  *---------------------------------------------*
           move      w-esp-scl-dtp-wqt
                    (w-esp-scl-dtp-wlp)   to   w-esp-scl-dtp-qta      .
      *                  *---------------------------------------------*
      *                  * Flag di anagrafica elemento esistente       *
      *                  *---------------------------------------------*
           move      w-esp-scl-dtp-wes
                    (w-esp-scl-dtp-wlp)   to   w-esp-scl-dtp-ana      .
      *                  *---------------------------------------------*
      *                  * Descrizione da anagrafica elemento          *
      *                  *---------------------------------------------*
           move      w-esp-scl-dtp-wde
                    (w-esp-scl-dtp-wlp)   to   w-esp-scl-dtp-des      .
      *                  *---------------------------------------------*
      *                  * Unita' di misura da anagrafica elemento     *
      *                  *---------------------------------------------*
           move      w-esp-scl-dtp-wum
                    (w-esp-scl-dtp-wlp)   to   w-esp-scl-dtp-umi      .
      *                  *---------------------------------------------*
      *                  * Numero decimali quantita' da anagrafica e-  *
      *                  * lemento                                     *
      *                  *---------------------------------------------*
           move      w-esp-scl-dtp-wnd
                    (w-esp-scl-dtp-wlp)   to   w-esp-scl-dtp-dec      .
      *                  *---------------------------------------------*
      *                  * Se anagrafica esistente si esce, altrimenti *
      *                  * si prepara la descrizione con il codice nu- *
      *                  * merico dell'elemento e con puntini          *
      *                  *---------------------------------------------*
           if        w-esp-scl-dtp-ana    =    spaces
                     go to esp-scl-dtp-829.
           move      all   "."            to   w-esp-scl-dtp-des      .
           string    "("
                                delimited by   size
                     w-esp-scl-dtp-num
                                delimited by   size
                     ")"
                                delimited by   size
                                          into w-esp-scl-dtp-des      .
       esp-scl-dtp-829.
           exit.
       esp-scl-dtp-999.
           exit.
