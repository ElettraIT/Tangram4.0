       Identification Division.
       Program-Id.                                 page2000           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    age                 *
      *                                Settore:    arc                 *
      *                                   Fase:    age200              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 04/07/94    *
      *                       Ultima revisione:    NdK del 07/06/22    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Gestione archivio agenti                    *
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
                     "arc"                                            .
      *        *-------------------------------------------------------*
      *        * Fase gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-fas                  pic  x(06) value
                     "age200"                                         .
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10) value
                     "page2000"                                       .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  i-ide-des                  pic  x(40) value
                     "        GESTIONE ARCHIVIO AGENTI        "       .

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
      *            * Per routine pos-exe-pgm-000                       *
      *            *---------------------------------------------------*
               10  w-cnt-pos-exe-pgm      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Per routine rou-opn-fls-000                       *
      *            *---------------------------------------------------*
               10  w-cnt-rou-opn-fls      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Per routine rou-let-reg-000                       *
      *            *---------------------------------------------------*
               10  w-cnt-rou-let-reg      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Per routine pre-acc-ins-000                       *
      *            *---------------------------------------------------*
               10  w-cnt-pre-acc-ins      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Per routine pre-acc-mod-000                       *
      *            *---------------------------------------------------*
               10  w-cnt-pre-acc-mod      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Per routine pre-acc-vis-000                       *
      *            *---------------------------------------------------*
               10  w-cnt-pre-acc-vis      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Flags di tipo uscita da routines di accettazione      *
      *        *-------------------------------------------------------*
           05  w-cnt-tus.
      *            *---------------------------------------------------*
      *            * Da accettazione campi chiave                      *
      *            *---------------------------------------------------*
               10  w-cnt-tus-acc-key      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Da accettazione campi non chiave                  *
      *            *---------------------------------------------------*
               10  w-cnt-tus-acc-nok      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Da accettazione testata                           *
      *            *---------------------------------------------------*
               10  w-cnt-tus-acc-tes      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Flags di uscita da controlli su tasto Do              *
      *        *-------------------------------------------------------*
           05  w-cnt-tdo.
      *            *---------------------------------------------------*
      *            * Per tasto Do su campi chiave                      *
      *            *---------------------------------------------------*
               10  w-cnt-tdo-key-flg      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Per controllo se chiave vuota                     *
      *            *---------------------------------------------------*
               10  w-cnt-key-vuo-flg      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Per tasto Do su campi non chiave                  *
      *            *---------------------------------------------------*
               10  w-cnt-tdo-nok-flg      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Flags di controllo su modalita' di funzionamento      *
      *        *-------------------------------------------------------*
           05  w-cnt-mfu.
      *            *---------------------------------------------------*
      *            * Tipo impostazione                                 *
      *            * - K : Impostazione campi chiave                   *
      *            * - T : Impostazione campi testata                  *
      *            *---------------------------------------------------*
               10  w-cnt-mfu-tip-imp      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Tipo funzione                                     *
      *            * - I : Inserimento                                 *
      *            * - M : Modifica                                    *
      *            * - V : Visualizzazione                             *
      *            *---------------------------------------------------*
               10  w-cnt-mfu-tip-fun      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Visualizzazione forzata da segreteria             *
      *            *---------------------------------------------------*
               10  w-cnt-mfu-vis-sgr      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Flag di controllo su status visualizzazione titolo    *
      *        *-------------------------------------------------------*
           05  w-cnt-sts-vis-tit          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Flags di controllo su status impostazioni             *
      *        *-------------------------------------------------------*
           05  w-cnt-sts-imp.
      *            *---------------------------------------------------*
      *            * Impostazione chiave                               *
      *            *---------------------------------------------------*
               10  w-cnt-sts-imp-key      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Numero pagina testata in corso di trattamento     *
      *            *---------------------------------------------------*
               10  w-cnt-sts-imp-npt      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Salvataggio per il campo precedente               *
      *            *---------------------------------------------------*
               10  w-cnt-sts-imp-svp      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Si/No pagina testata in corso di trattamento      *
      *            *---------------------------------------------------*
               10  w-cnt-sts-imp-snp      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Numero pagine componenti la testata               *
      *            *---------------------------------------------------*
               10  w-cnt-sts-imp-mpt      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Impostazione testata                              *
      *            *---------------------------------------------------*
               10  w-cnt-sts-imp-tes      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Impostazione pagine di testata                    *
      *            *---------------------------------------------------*
               10  w-cnt-sts-imp-pte.
                   15  w-cnt-sts-imp-ptx  occurs 9
                                          pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Ingresso in pagine di testata                     *
      *            *---------------------------------------------------*
               10  w-cnt-sts-ing-pte.
                   15  w-cnt-sts-ing-ptx  occurs 9
                                          pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Numero relativo pagina testata visualizzata       *
      *            *---------------------------------------------------*
               10  w-cnt-sts-imp-nrt      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Numero relativo pagine testata gestite            *
      *            *---------------------------------------------------*
               10  w-cnt-sts-imp-ntt      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Comodi per numeri pagine testata gestite          *
      *            *---------------------------------------------------*
               10  w-cnt-sts-imp-ntw      pic  9(02)                  .
               10  w-cnt-sts-imp-nts      pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Flags di controllo su status visualizzazione prompts  *
      *        *-------------------------------------------------------*
           05  w-cnt-sts-pmt.
      *            *---------------------------------------------------*
      *            * Visualizzazione prompts chiave                    *
      *            *---------------------------------------------------*
               10  w-cnt-sts-pmt-key      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Visualizzazione prompts testata                   *
      *            *---------------------------------------------------*
               10  w-cnt-sts-pmt-tes      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Visualizzazione prompts pagine di testata         *
      *            *---------------------------------------------------*
               10  w-cnt-sts-pmt-pte.
                   15  w-cnt-sts-pmt-ptx  occurs 9
                                          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Flags di controllo su status visualizzazione dati     *
      *        *-------------------------------------------------------*
           05  w-cnt-sts-vis.
      *            *---------------------------------------------------*
      *            * Visualizzazione dati chiave                       *
      *            *---------------------------------------------------*
               10  w-cnt-sts-vis-key      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Visualizzazione dati testata                      *
      *            *---------------------------------------------------*
               10  w-cnt-sts-vis-tes      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Visualizzazione dati pagine di testata            *
      *            *---------------------------------------------------*
               10  w-cnt-sts-vis-pte.
                   15  w-cnt-sts-vis-ptx  occurs 9
                                          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Area di controllo per accettazioni singoli campi      *
      *        *-------------------------------------------------------*
           05  w-cnt-acc.
               10  w-cnt-acc-flg-aum      pic  x(01)                  .
               10  w-cnt-acc-sav-tip      pic  x(01)                  .
               10  w-cnt-acc-sav-car      pic  9(02)                  .
               10  w-cnt-acc-sav-ldt      pic  9(02)                  .
               10  w-cnt-acc-sav-dec      pic  9(01)                  .
               10  w-cnt-acc-sav-sgn      pic  x(01)                  .
               10  w-cnt-acc-sav-edm      pic  x(10)                  .
               10  w-cnt-acc-sav-msk      pic  x(24)                  .
               10  w-cnt-acc-sav-lin      pic  9(02)                  .
               10  w-cnt-acc-sav-pos      pic  9(02)                  .
               10  w-cnt-acc-sav-alf      pic  x(80)                  .
               10  w-cnt-acc-sav-txt.
                   15  filler occurs 400  pic  x(01)                  .
               10  w-cnt-acc-sav-num      pic s9(13)v9(05)  trailing
                                                            separate
                                                            character .
               10  w-cnt-acc-sav-dat      pic  9(07)                  .
               10  w-cnt-acc-sav-ufk      pic  x(80)                  .
               10  w-cnt-acc-sav-mod      pic  x(01)                  .
               10  w-cnt-acc-sav-l23      pic  x(80)                  .
               10  w-cnt-acc-sav-l24      pic  x(80)                  .
      *            *---------------------------------------------------*
      *            * Flag di uscita per il tasto 'Exit'                *
      *            *---------------------------------------------------*
               10  w-cnt-acc-flg-exi      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Work area                                             *
      *        *-------------------------------------------------------*
           05  w-cnt-wrk.
               10  w-cnt-wrk-ctr-001      pic  9(05)                  .
               10  w-cnt-wrk-ctr-002      pic  9(05)                  .
               10  w-cnt-wrk-ctr-008      pic  9(05)                  .
               10  w-cnt-wrk-ctr-009      pic  9(05)                  .

      *    *===========================================================*
      *    * Record files                                              *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [ada]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/azi/fls/rec/rfada"                          .
      *        *-------------------------------------------------------*
      *        * [age]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/age/fls/rec/rfage"                          .
      *        *-------------------------------------------------------*
      *        * [ags]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/age/fls/rec/rfags"                          .
      *        *-------------------------------------------------------*
      *        * [gxn]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/geo/fls/rec/rfgxn"                          .
      *        *-------------------------------------------------------*
      *        * [gxc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/geo/fls/rec/rfgxc"                          .
      *        *-------------------------------------------------------*
      *        * [zpv]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/age/fls/rec/rfzpv"                          .
      *        *-------------------------------------------------------*
      *        * [pdc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfpdc"                         .
      *        *-------------------------------------------------------*
      *        * [adc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/azi/fls/rec/rfadc"                          .

      *    *===========================================================*
      *    * Work-area per bufferizzazione testata                     *
      *    *-----------------------------------------------------------*
       01  w-tes.
      *        *-------------------------------------------------------*
      *        * Valori chiave                                         *
      *        *-------------------------------------------------------*
           05  w-tes-val-key.
               10  w-tes-cod-age          pic  9(07)                  .
               10  w-tes-cod-age-aut      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Valori attuali e precedenti                           *
      *        *-------------------------------------------------------*
           05  w-tes-val-aep occurs 2.
      *            *---------------------------------------------------*
      *            * Valori gestiti dal programma                      *
      *            *---------------------------------------------------*
               10  w-tes-ide-dat          pic  9(07)                  .
               10  w-tes-ide-ute          pic  x(08)                  .
               10  w-tes-ide-fas          pic  x(06)                  .
               10  w-tes-cod-mne          pic  x(10)                  .
               10  w-tes-nom-key          pic  x(20)                  .
               10  w-tes-nom-age          pic  x(20)                  .
               10  w-tes-rag-key          pic  x(40)                  .
               10  w-tes-rag-soc          pic  x(40)                  .
               10  w-tes-via-age          pic  x(40)                  .
               10  w-tes-loc-age          pic  x(40)                  .
               10  w-tes-cod-naz          pic  x(03)                  .
               10  w-tes-cod-naz-des      pic  x(20)                  .
               10  w-tes-cod-cmn          pic  9(05)                  .
               10  w-tes-cod-cmn-des      pic  x(30)                  .
               10  w-tes-cod-cmn-prv      pic  x(02)                  .
               10  w-tes-cod-fzn          pic  9(03)                  .
               10  w-tes-cod-fzn-des      pic  x(30)                  .
               10  w-tes-cod-lct          pic  9(03)                  .
               10  w-tes-cod-lct-des      pic  x(30)                  .
      *            *---------------------------------------------------*
      *            * Numeri utenza obsoleti                            *
      *            *---------------------------------------------------*
               10  w-tes-num-tel          pic  x(20)                  .
               10  w-tes-num-fax          pic  x(20)                  .
               10  w-tes-num-tlx          pic  x(20)                  .
      *
               10  w-tes-num-tlx-r redefines
                   w-tes-num-tlx.
                   15  w-tes-num-trg      pic  x(15)                  .
                   15  filler             pic  x(01)                  .
                   15  w-tes-dpz-op1      pic  9(02)                  .
                   15  w-tes-dpz-op2      pic  9(02)                  .
               10  w-tes-dpz-op1-des      pic  x(20)                  .
               10  w-tes-dpz-op2-des      pic  x(20)                  .
      *
               10  w-tes-nom-int          pic  x(30)                  .
      *            *---------------------------------------------------*
      *            * Contatti                                          *
      *            *---------------------------------------------------*
               10  w-tes-con-arc occurs 50.
                   15  w-tes-tip-con      pic  x(03)                  .
                   15  w-tes-num-con      pic  x(80)                  .
                   15  w-tes-int-con      pic  x(40)                  .
      *            *---------------------------------------------------*
      *            * Partita Iva                                       *
      *            *---------------------------------------------------*
               10  w-tes-prt-iva          pic  9(11)                  .
               10  w-tes-cod-fis          pic  x(16)                  .
               10  w-tes-sup-age          pic  9(07)                  .
               10  w-tes-sup-age-nom      pic  x(20)                  .
               10  w-tes-cat-pvg          pic  9(05)                  .
               10  w-tes-cat-pvg-des      pic  x(20)                  .
               10  w-tes-per-pvg occurs 03
                                          pic  9(02)v9(01)            .
               10  w-tes-tip-mat          pic  x(01)                  .
               10  w-tes-flg-cpv.
                   15  w-tes-flg-ele occurs 09
                                          pic  x(01)                  .
               10  w-tes-ctp-ven          pic  9(07)                  .
               10  w-tes-ctp-ven-des      pic  x(40)                  .
               10  w-tes-ctp-rsv          pic  9(07)                  .
               10  w-tes-ctp-rsv-des      pic  x(40)                  .
      *            *---------------------------------------------------*
      *            * Campi non gestiti dal programma                   *
      *            *---------------------------------------------------*
               10  w-tes-cod-cge          pic  9(07)                  .
               10  w-tes-cla-bdg          pic  9(05)                  .
      *            *---------------------------------------------------*
      *            * Campi aggiunti                                    *
      *            *---------------------------------------------------*
               10  w-tes-sta-tus          pic  9(02)                  .
               10  w-tes-sta-tud          pic  9(07)                  .
               10  w-tes-sta-tuc          pic  9(07)                  .
               10  w-tes-sta-tuc-nom      pic  x(20)                  .
               10  w-tes-sta-tux          pic  9(02)                  .
               10  w-tes-dat-iat          pic  9(07)                  .
               10  w-tes-alx-exp.
                   15  filler  occurs 55  pic  x(01)                  .

      *    *===========================================================*
      *    * Work per subroutines di Insr                              *
      *    *-----------------------------------------------------------*
       01  w-ins.
      *        *-------------------------------------------------------*
      *        * Work per Insr su archivio [ags]                       *
      *        *-------------------------------------------------------*
           05  w-ins-arc-ags.
               10  w-ins-arc-ags-age      pic  9(07)                  .

      *    *===========================================================*
      *    * Work per subroutines di Let                               *
      *    *-----------------------------------------------------------*
       01  w-let.
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [gxn]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-gxn.
               10  w-let-arc-gxn-flg      pic  x(01)                  .
               10  w-let-arc-gxn-cod      pic  x(03)                  .
               10  w-let-arc-gxn-des      pic  x(20)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [gxc]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-gxc.
               10  w-let-arc-gxc-flg      pic  x(01)                  .
               10  w-let-arc-gxc-tip      pic  x(01)                  .
               10  w-let-arc-gxc-cmn      pic  9(05)                  .
               10  w-let-arc-gxc-fzn      pic  9(03)                  .
               10  w-let-arc-gxc-lct      pic  9(03)                  .
               10  w-let-arc-gxc-des      pic  x(30)                  .
               10  w-let-arc-gxc-prv      pic  x(02)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [zpv]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-zpv.
               10  w-let-arc-zpv-flg      pic  x(01)                  .
               10  w-let-arc-zpv-tip      pic  9(02)                  .
               10  w-let-arc-zpv-cod      pic  9(05)                  .
               10  w-let-arc-zpv-des      pic  x(20)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [age]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-age.
               10  w-let-arc-age-flg      pic  x(01)                  .
               10  w-let-arc-age-cod      pic  9(07)                  .
               10  w-let-arc-age-nom      pic  x(20)                  .
               10  w-let-arc-age-sup      pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [ags]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-ags.
               10  w-let-arc-ags-age      pic  9(07)                  .
               10  w-let-arc-ags-sup      pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [pdc]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-pdc.
               10  w-let-arc-pdc-flg      pic  x(01)                  .
               10  w-let-arc-pdc-cod      pic  9(07)                  .
               10  w-let-arc-pdc-des      pic  x(40)                  .

      *    *===========================================================*
      *    * Work per Let su archivio [ada]                            *
      *    *-----------------------------------------------------------*
           copy      "pgm/azi/prg/cpy/larcada0.ltw"                   .

      *    *===========================================================*
      *    * Work per accettazioni campi espansi                       *
      *    *-----------------------------------------------------------*
       01  w-exp.
      *        *-------------------------------------------------------*
      *        * Work per : errore formale partita iva                 *
      *        *-------------------------------------------------------*
           05  w-exp-err-piv.
               10  w-exp-err-piv-num      pic  9(02)       value 2    .
               10  w-exp-err-piv-lun      pic  9(02)       value 40   .
               10  w-exp-err-piv-tbl.
                   15  filler             pic  x(40) value
                            "Reimpostazione della partita iva        ".
                   15  filler             pic  x(40) value
                            "Forzatura del valore impostato          ".
               10  w-exp-err-piv-sce      pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Work per : errore formale codice fiscale              *
      *        *-------------------------------------------------------*
           05  w-exp-err-cfi.
               10  w-exp-err-cfi-num      pic  9(02)       value 2    .
               10  w-exp-err-cfi-lun      pic  9(02)       value 40   .
               10  w-exp-err-cfi-tbl.
                   15  filler             pic  x(40) value
                            "Reimpostazione del codice fiscale       ".
                   15  filler             pic  x(40) value
                            "Forzatura del valore impostato          ".
               10  w-exp-err-cfi-sce      pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Work per : Tipo maturazione                           *
      *        *-------------------------------------------------------*
           05  w-exp-tip-mat.
               10  w-exp-tip-mat-num      pic  9(02)       value 2    .
               10  w-exp-tip-mat-lun      pic  9(02)       value 40   .
               10  w-exp-tip-mat-tbl.
                   15  filler             pic  x(40) value
                            "Maturazione immediata dei conteggi      ".
                   15  filler             pic  x(40) value
                            "Maturazione conteggi a fronte pagamenti ".
      *        *-------------------------------------------------------*
      *        * Work per : Status commerciale agente                  *
      *        *-------------------------------------------------------*
           05  w-exp-sta-tus.
               10  w-exp-sta-tus-num      pic  9(02)       value 9    .
               10  w-exp-sta-tus-lun      pic  9(02)       value 40   .
               10  w-exp-sta-tus-tbl.
                   15  filler             pic  x(40) value
                           "Normale                                 " .
                   15  filler             pic  x(40) value
                           "Esauriti i rapporti commerciali         " .
                   15  filler             pic  x(40) value
                           "Sostituito da ns. nuovo agente          " .
                   15  filler             pic  x(40) value
                           "Cessata attivita'                       " .
                   15  filler             pic  x(40) value
                           "Cessata attivita', ma sost. da nuovo ag." .
                   15  filler             pic  x(40) value
                           "In contenzioso                          " .
                   15  filler             pic  x(40) value
                           "In contenzioso, ma sost. da nuovo agente" .
                   15  filler             pic  x(40) value
                           "Fallito                                 " .
                   15  filler             pic  x(40) value
                           "Fallito, ma sostituito da nuovo agente  " .
      *        *-------------------------------------------------------*
      *        * Work per : Modalita' di trattamento nelle ststistiche *
      *        *            per i documenti relativi al nuovo status   *
      *        *-------------------------------------------------------*
           05  w-exp-sta-tux.
               10  w-exp-sta-tux-num      pic  9(02)       value 2    .
               10  w-exp-sta-tux-lun      pic  9(02)       value 40   .
               10  w-exp-sta-tux-tbl.
                   15  filler             pic  x(40) value
                           "statistiche Distinte                    " .
                   15  filler             pic  x(40) value
                           "statistiche Girate sul nuovo agente     " .

      *    *===========================================================*
      *    * Area di interfaccia per sottoprogramma         "pazi000d" *
      *    *-----------------------------------------------------------*
           copy      "pgm/azi/prg/cpy/pazi000d.pgl"                   .

      *    *===========================================================*
      *    * Work per subroutines di controllo formale Partita Iva e   *
      *    * Codici Fiscale                                            *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/wpivcfi0.wkl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice dipendenza dell'azienda *
      *    *-----------------------------------------------------------*
           copy      "pgm/azi/prg/cpy/acoddpz0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice agente                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/age/prg/cpy/acmnage0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione categoria provvigioni          *
      *    *-----------------------------------------------------------*
           copy      "pgm/age/prg/cpy/acmnzpv1.acl"                   .
           
      *    *===========================================================*
      *    * Link-area per accettazione codice nazione                 *
      *    *-----------------------------------------------------------*
           copy      "pgm/geo/prg/cpy/acdenaz0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione Indirizzo, C.a.p. e citta'     *
      *    *-----------------------------------------------------------*
           copy      "pgm/geo/prg/cpy/aiplgeo0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione C.a.p. e citta'                *
      *    *-----------------------------------------------------------*
           copy      "pgm/geo/prg/cpy/acecgeo0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione comune                         *
      *    *-----------------------------------------------------------*
           copy      "pgm/geo/prg/cpy/acomgeo0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione frazione                       *
      *    *-----------------------------------------------------------*
           copy      "pgm/geo/prg/cpy/afrageo0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione localita'                      *
      *    *-----------------------------------------------------------*
           copy      "pgm/geo/prg/cpy/alocgeo0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice sottoconto              *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/acmnpdc0.acl"                   .

      *    *===========================================================*
      *    * Area di comunicazione per determinazione contatti         *
      *    *-----------------------------------------------------------*
           copy      "pgm/azi/prg/cpy/dconarc0.dtl"                   .

      *    *===========================================================*
      *    * Work per ridefinizioni partita iva / codice fiscale       *
      *    *-----------------------------------------------------------*
       01  w-piv-cfi.
      *        *-------------------------------------------------------*
      *        * Codice fiscale in rappresentazione alfanumerica       *
      *        *-------------------------------------------------------*
           05  w-piv-cfi-cfi-alf          pic  x(16)                  .
      *        *-------------------------------------------------------*
      *        * Ridefinizione del codice fiscale come due porzioni,   *
      *        * la prima di 11 caratteri, la seconda di 5 caratteri   *
      *        *-------------------------------------------------------*
           05  w-piv-cfi-cfi-016 redefines
               w-piv-cfi-cfi-alf.
               10  w-piv-cfi-cfi-11a      pic  x(11)                  .
               10  w-piv-cfi-cfi-11n redefines
                   w-piv-cfi-cfi-11a      pic  9(11)                  .
               10  w-piv-cfi-cfi-05a      pic  x(05)                  .
               10  w-piv-cfi-cfi-05n redefines
                   w-piv-cfi-cfi-05a      pic  9(05)                  .

      *    *===========================================================*
      *    * Work-area personalizzazioni                               *
      *    *-----------------------------------------------------------*
       01  w-prs.
      *        *-------------------------------------------------------*
      *        * Numero livelli del piano dei conti                    *
      *        *-------------------------------------------------------*
           05  w-prs-liv-pdc              pic  9(01)                  .
      *        *-------------------------------------------------------*
      *        * Si/no contropartita vendite e resi su vendite in ana- *
      *        * grafica agente                                        *
      *        *-------------------------------------------------------*
           05  w-prs-snx-ctp              pic  x(01)                  .

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
      *        * Flags per l'elaborazione                              *
      *        *-------------------------------------------------------*
           05  w-prs-age-300-fpe          pic  x(05)                  .
           05  filler                     pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Parametro per l'elaborazione                          *
      *        *-------------------------------------------------------*
           05  w-prs-age-300-ppe          pic  x(10)                  .

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
      *    * Work per salvataggi valori precedenti                     *
      *    *-----------------------------------------------------------*
       01  w-sav.
      *        *-------------------------------------------------------*
      *        * Work per salvataggio codice comune                    *
      *        *-------------------------------------------------------*
           05  w-sav-cod-cmn              pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Work per salvataggio codice frazione                  *
      *        *-------------------------------------------------------*
           05  w-sav-cod-fzn              pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Work per salvataggio partita iva                      *
      *        *-------------------------------------------------------*
           05  w-sav-prt-iva              pic  9(11)                  .
      *        *-------------------------------------------------------*
      *        * Work per salvataggio codice fiscale                   *
      *        *-------------------------------------------------------*
           05  w-sav-cod-fis              pic  x(16)                  .
      *        *-------------------------------------------------------*
      *        * Work per salvataggio codice super-agente              *
      *        *-------------------------------------------------------*
           05  w-sav-sup-age              pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Status commerciale                                    *
      *        *-------------------------------------------------------*
           05  w-sav-sta-tus              pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Codice agente di riferimento                          *
      *        *-------------------------------------------------------*
           05  w-sav-sta-tuc              pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Modalita' di trattamento nelle ststistiche per i      *
      *        * documenti relativi al nuovo status                    *
      *        *-------------------------------------------------------*
           05  w-sav-sta-tux              pic  9(02)                  .

      *    *===========================================================*
      *    * Work per accettazione contatti e utenze                   *
      *    *-----------------------------------------------------------*
           copy      "pgm/azi/prg/cpy/aconarc0.acl"                   .

      *    *===========================================================*
      *    * Work per attribuzione e ripristino codice automatico      *
      *    *-----------------------------------------------------------*
       01  w-enc-age.
      *        *-------------------------------------------------------*
      *        * Massimo valore accettabile                            *
      *        *-------------------------------------------------------*
           05  w-enc-age-val-max          pic  9(07) value 9999999    .
      *        *-------------------------------------------------------*
      *        * Valore pre incremento                                 *
      *        *-------------------------------------------------------*
           05  w-enc-age-val-pre          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Valore post incremento                                *
      *        *-------------------------------------------------------*
           05  w-enc-age-val-pos          pic  9(07)                  .

      *    *===========================================================*
      *    * Work per subroutines di editing codice sottoconto         *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/wedtpdc0.wkl"                   .

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
      *              * Normalizzazione flag di visualizzazione titolo  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-sts-vis-tit      .
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
       main-050.
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-rou-opn-fls      .
           perform   rou-opn-fls-000      thru rou-opn-fls-999        .
           if        w-cnt-rou-opn-fls    not  = spaces
                     go to main-800.
       main-100.
      *              *-------------------------------------------------*
      *              * Accettazione campi chiave                       *
      *              *-------------------------------------------------*
           perform   acc-key-reg-000      thru acc-key-reg-999        .
      *                  *---------------------------------------------*
      *                  * Se tipo uscita "E" : fine programma         *
      *                  *---------------------------------------------*
           if        w-cnt-tus-acc-key    =    "E"
                     go to main-800.
      *              *-------------------------------------------------*
      *              * Lettura registrazione pre-esistente             *
      *              *-------------------------------------------------*
           perform   rou-let-reg-000      thru rou-let-reg-999        .
      *                  *---------------------------------------------*
      *                  * Se esito negativo : riciclo su chiave       *
      *                  *---------------------------------------------*
           if        w-cnt-rou-let-reg    not  = spaces
                     go to main-100.
      *              *-------------------------------------------------*
      *              * Routine pre-accettazione campi non chiave       *
      *              *-------------------------------------------------*
           if        w-cnt-mfu-tip-fun    =    "I"
                     go to main-130
           else if   w-cnt-mfu-tip-fun    =    "M"
                     go to main-140
           else if   w-cnt-mfu-tip-fun    =    "V"
                     go to main-150
           else      go to main-100.
       main-130.
           move      spaces               to   w-cnt-pre-acc-ins      .
           perform   pre-acc-ins-000      thru pre-acc-ins-999        .
           if        w-cnt-pre-acc-ins    =    spaces
                     go to main-180
           else      go to main-100.
       main-140.
           move      spaces               to   w-cnt-pre-acc-mod      .
           perform   pre-acc-mod-000      thru pre-acc-mod-999        .
           if        w-cnt-pre-acc-mod    =    spaces
                     go to main-180
           else      go to main-100.
       main-150.
           move      spaces               to   w-cnt-pre-acc-vis      .
           perform   pre-acc-vis-000      thru pre-acc-vis-999        .
           if        w-cnt-pre-acc-vis    =    spaces
                     go to main-180
           else      go to main-100.
       main-180.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di modifica di almeno un   *
      *              * campo non chiave                                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-acc-flg-aum      .
      *              *-------------------------------------------------*
      *              * Accettazione campi non chiave                   *
      *              *-------------------------------------------------*
           perform   acc-nok-reg-000      thru acc-nok-reg-999        .
      *                  *---------------------------------------------*
      *                  * Se uscita per Exit                          *
      *                  *---------------------------------------------*
           if        w-cnt-tus-acc-nok    not  = "E"
                     go to main-200.
      *                      *-----------------------------------------*
      *                      * Routine post-exit su campi non chiave   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se Inserimento                      *
      *                          *-------------------------------------*
           if        w-cnt-mfu-tip-fun    =    "I"
                     perform   pos-exi-ins-000
                                          thru pos-exi-ins-999
      *                          *-------------------------------------*
      *                          * Se Modifica                         *
      *                          *-------------------------------------*
           else if   w-cnt-mfu-tip-fun    =    "M"
                     perform   pos-exi-mod-000
                                          thru pos-exi-mod-999
      *                          *-------------------------------------*
      *                          * Se Visualizzazione                  *
      *                          *-------------------------------------*
           else if   w-cnt-mfu-tip-fun    =    "V"
                     perform   pos-exi-vis-000
                                          thru pos-exi-vis-999        .
      *                      *-----------------------------------------*
      *                      * Riciclo                                 *
      *                      *-----------------------------------------*
           go to     main-100.
       main-200.
      *                  *---------------------------------------------*
      *                  * Se uscita per annullamento                  *
      *                  *---------------------------------------------*
           if        w-cnt-tus-acc-nok    not  = "X"
                     go to main-300.
      *                      *-----------------------------------------*
      *                      * Routine post-conferma di annullamento   *
      *                      *-----------------------------------------*
           perform   pos-cnf-ann-000      thru pos-cnf-ann-999        .
      *                      *-----------------------------------------*
      *                      * Riciclo                                 *
      *                      *-----------------------------------------*
           go to     main-100.
       main-300.
      *                  *---------------------------------------------*
      *                  * Se uscita per conferma                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Routine post-conferma                   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se post-conferma di Inserimento     *
      *                          *-------------------------------------*
           if        w-cnt-mfu-tip-fun    =    "I"
                     perform   pos-cnf-ins-000
                                          thru pos-cnf-ins-999
      *                          *-------------------------------------*
      *                          * Se post-conferma di Modifica        *
      *                          *-------------------------------------*
           else if   w-cnt-mfu-tip-fun    =    "M"
                     perform   pos-cnf-mod-000
                                          thru pos-cnf-mod-999        .
      *                      *-----------------------------------------*
      *                      * Riciclo                                 *
      *                      *-----------------------------------------*
           go to     main-100.
       main-800.
      *              *-------------------------------------------------*
      *              * Close files                                     *
      *              *-------------------------------------------------*
           perform   rou-cls-fls-000      thru rou-cls-fls-999        .
      *              *-------------------------------------------------*
      *              * Esecuzione routine post-esecuzione programma    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-pos-exe-pgm      .
           perform   pos-exe-pgm-000      thru pos-exe-pgm-999        .
           if        w-cnt-pos-exe-pgm    not  = spaces
                     go to main-050.
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
      *              * Tasto di funzione Prsc :                        *
      *              * - se in tipo impostazione testata e su pagina   *
      *              *   maggiore di 1 : abilitato                     *
      *              * - altrimenti    : invariato                     *
      *              *-------------------------------------------------*
           if        w-cnt-mfu-tip-imp    =    "T" and
                     w-cnt-sts-imp-npt    >    1
                     move  "PRSC"         to   v-pfk (07)             .
      *              *-------------------------------------------------*
      *              * Tasto di funzione Nxsc :                        *
      *              * - se in tipo impostazione testata e se pagina   *
      *              *   gia' completamente impostata o testata gia'   *
      *              *   completamente impostata : abilitato           *
      *              *  - altrimenti             : invariato           *
      *              *-------------------------------------------------*
           if        w-cnt-mfu-tip-imp    not  = "T"
                     go to exe-acc-cmp-050.
           if        w-cnt-sts-imp-tes    not  = spaces or
                     w-cnt-sts-imp-ptx
                    (w-cnt-sts-imp-npt)   not  = spaces
                     move  "NXSC"         to   v-pfk (08)             .
       exe-acc-cmp-050.
      *              *-------------------------------------------------*
      *              * Tasto di funzione Delt :                        *
      *              *  - se Impostazione chiave    : non abilitato    *
      *              *  - se Inserimento            : non abilitato    *
      *              *  - se Visualizzazione        : non abilitato    *
      *              *  - se Almeno una modifica    : non abilitato    *
      *              *  - altrimenti                : abilitato        *
      *              *-------------------------------------------------*
           if        w-cnt-mfu-tip-imp    =    "K"   or
                     w-cnt-mfu-tip-fun    =    "I"   or
                     w-cnt-mfu-tip-fun    =    "V"   or
                     w-cnt-acc-flg-aum    not  = spaces
                     go to exe-acc-cmp-100.
           move      "DELT"               to   v-pfk (19)             .
       exe-acc-cmp-100.
      *              *-------------------------------------------------*
      *              * Tasto di funzione Do :                          *
      *              *  - se Visualizzazione     : disabilitati        *
      *              *  - altrimenti             : inalterati          *
      *              *-------------------------------------------------*
           if        w-cnt-mfu-tip-fun    =    "V"
                     move  spaces         to   v-pfk (05)             .
      *              *-------------------------------------------------*
      *              * Salvataggio parametri di accettazione           *
      *              *-------------------------------------------------*
           move      v-tip                to   w-cnt-acc-sav-tip      .
           move      v-car                to   w-cnt-acc-sav-car      .
           move      v-lin                to   w-cnt-acc-sav-lin      .
           move      v-pos                to   w-cnt-acc-sav-pos      .
           if        v-tip                =    "A" or
                     v-tip                =    "U" or
                     v-tip                =    "L" or
                     v-tip                =    "E"
                     move  v-alf          to   w-cnt-acc-sav-alf      .
           if        v-tip                =    "N" or
                     v-tip                =    "V" or
                     v-tip                =    "P" or
                     v-tip                =    "E"
                     move  v-dec          to   w-cnt-acc-sav-dec
                     move  v-sgn          to   w-cnt-acc-sav-sgn
                     move  v-edm          to   w-cnt-acc-sav-edm
                     move  v-msk          to   w-cnt-acc-sav-msk
                     move  v-num          to   w-cnt-acc-sav-num      .
           if        v-tip                =    "D"
                     move  v-dat          to   w-cnt-acc-sav-dat      .
           if        v-tip                =    "T" or
                     v-tip                =    "E"
                     move  v-ldt          to   w-cnt-acc-sav-ldt
                     move  v-txt          to   w-cnt-acc-sav-txt      .
           move      v-ufk                to   w-cnt-acc-sav-ufk      .
       exe-acc-cmp-200.
      *              *-------------------------------------------------*
      *              * Accettazione                                    *
      *              *-------------------------------------------------*
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Memorizzazione segnale di campo modificato      *
      *              *-------------------------------------------------*
           move      v-mod                to   w-cnt-acc-sav-mod      .
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                not  = "DELT"
                     go to exe-acc-cmp-600.
      *              *-------------------------------------------------*
      *              * Se il campo impostato ha modificato il valore   *
      *              * precedente : ripetizione impostazione           *
      *              *-------------------------------------------------*
           if        w-cnt-acc-sav-mod    not  = spaces
                     go to exe-acc-cmp-400.
      *              *-------------------------------------------------*
      *              * Salvataggio linee 23 e 24 di note operative     *
      *              *-------------------------------------------------*
           move      "GL"                 to   v-ope                  .
           move      23                   to   v-lin                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-alf                to   w-cnt-acc-sav-l23      .
           move      "GL"                 to   v-ope                  .
           move      24                   to   v-lin                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-alf                to   w-cnt-acc-sav-l24      .
      *              *-------------------------------------------------*
      *              * Cancellazione note operative attuali            *
      *              *-------------------------------------------------*
           move      "NT"                 to   v-ope                  .
           move      spaces               to   v-not                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Richiesta conferma annullamento                 *
      *              *-------------------------------------------------*
           move      "MX"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      "#DEL"               to   v-not                  .
           move      "N"                  to   v-alf                  .
           move      "SN"                 to   v-msk                  .
           move      "DELT"               to   v-pfk (19)             .
           move      "UP  "               to   v-pfk (02)             .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           if        v-key                =    spaces
                     if      v-alf        =    "S"
                             move  "DELT" to   v-key
                     else    move  "UP  " to   v-key                  .
      *              *-------------------------------------------------*
      *              * Se function key DELT si esce                    *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     go to exe-acc-cmp-999.
      *              *-------------------------------------------------*
      *              * Ripristino linee 23 e 24 di note operative      *
      *              *-------------------------------------------------*
           move      "NT"                 to   v-ope                  .
           move      w-cnt-acc-sav-l23    to   v-nt1                  .
           move      w-cnt-acc-sav-l24    to   v-nt2                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       exe-acc-cmp-400.
      *              *-------------------------------------------------*
      *              * Ripristino parametri di impostazione            *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      w-cnt-acc-sav-tip    to   v-tip                  .
           move      w-cnt-acc-sav-car    to   v-car                  .
           move      w-cnt-acc-sav-lin    to   v-lin                  .
           move      w-cnt-acc-sav-pos    to   v-pos                  .
           if        v-tip                =    "A" or
                     v-tip                =    "U" or
                     v-tip                =    "L" or
                     v-tip                =    "E"
                     move  w-cnt-acc-sav-alf
                                          to   v-alf                  .
           if        v-tip                =    "N" or
                     v-tip                =    "V" or
                     v-tip                =    "P" or
                     v-tip                =    "E"
                     move  w-cnt-acc-sav-dec
                                          to   v-dec
                     move  w-cnt-acc-sav-sgn
                                          to   v-sgn
                     move  w-cnt-acc-sav-edm
                                          to   v-edm
                     move  w-cnt-acc-sav-msk
                                          to   v-msk
                     move  w-cnt-acc-sav-num
                                          to   v-num                  .
           if        v-tip                =    "D"
                     move  w-cnt-acc-sav-dat
                                          to   v-dat                  .
           if        v-tip                =    "T" or
                     v-tip                =    "E"
                     move  w-cnt-acc-sav-ldt
                                          to   v-ldt
                     move  w-cnt-acc-sav-txt
                                          to   v-txt                  .
           move      w-cnt-acc-sav-ufk    to   v-ufk                  .
      *              *-------------------------------------------------*
      *              * Ritorno alla impostazione                       *
      *              *-------------------------------------------------*
           go to     exe-acc-cmp-200.
       exe-acc-cmp-600.
      *              *-------------------------------------------------*
      *              * Flag globale di avvenuta almeno una modifica    *
      *              *-------------------------------------------------*
           if        w-cnt-acc-sav-mod    not  = spaces
                     move  "#"            to   w-cnt-acc-flg-aum      .
      *              *-------------------------------------------------*
      *              * Visualizzazione tipo funzionamento              *
      *              *-------------------------------------------------*
           perform   vis-tip-fun-000      thru vis-tip-fun-999        .
       exe-acc-cmp-800.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                not  = "EXIT"
                     go to exe-acc-cmp-850.
      *                  *---------------------------------------------*
      *                  * Subroutine di controllo per Exit            *
      *                  *---------------------------------------------*
           perform   exe-acc-cmp-exi-000  thru exe-acc-cmp-exi-999    .
      *                  *---------------------------------------------*
      *                  * Se Exit non attivato : a ripristino para-   *
      *                  * metri di impostazione                       *
      *                  *---------------------------------------------*
           if        w-cnt-acc-flg-exi    not  = spaces
                     go to exe-acc-cmp-400.
      *                  *---------------------------------------------*
      *                  * Altrimenti : ad uscita                      *
      *                  *---------------------------------------------*
           go to     exe-acc-cmp-900.
       exe-acc-cmp-850.
       exe-acc-cmp-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-acc-cmp-999.
       exe-acc-cmp-999.
           exit.

      *    *===========================================================*
      *    * Esecuzione accettazione di un campo                       *
      *    *                                                           *
      *    * Subroutine di controllo tasto 'Exit'                      *
      *    *-----------------------------------------------------------*
       exe-acc-cmp-exi-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-acc-flg-exi      .
       exe-acc-cmp-exi-050.
      *              *-------------------------------------------------*
      *              * Se impostazione campi chiave : si ignora        *
      *              *-------------------------------------------------*
           if        w-cnt-mfu-tip-imp    =    "K"
                     go to exe-acc-cmp-exi-920.
      *              *-------------------------------------------------*
      *              * Se in visualizzazione : si ignora               *
      *              *-------------------------------------------------*
           if        w-cnt-mfu-tip-fun    =    "V"
                     go to exe-acc-cmp-exi-920.
      *              *-------------------------------------------------*
      *              * Se in impostazione altri campi riga corpo : si  *
      *              * ignora                                          *
      *              *-------------------------------------------------*
           if        w-cnt-mfu-tip-imp    =    "R"
                     go to exe-acc-cmp-exi-920.
      *              *-------------------------------------------------*
      *              * Se non avvenuta alcuna modifica : si ignora     *
      *              *-------------------------------------------------*
           if        w-cnt-acc-flg-aum    =    spaces
                     go to exe-acc-cmp-exi-920.
       exe-acc-cmp-exi-100.
      *              *-------------------------------------------------*
      *              * Salvataggio linee 23 e 24 di note operative     *
      *              *-------------------------------------------------*
           move      "GL"                 to   v-ope                  .
           move      23                   to   v-lin                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-alf                to   w-cnt-acc-sav-l23      .
           move      "GL"                 to   v-ope                  .
           move      24                   to   v-lin                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-alf                to   w-cnt-acc-sav-l24      .
      *              *-------------------------------------------------*
      *              * Cancellazione note operative attuali            *
      *              *-------------------------------------------------*
           move      "NT"                 to   v-ope                  .
           move      spaces               to   v-not                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Richiesta conferma di uscita                    *
      *              *-------------------------------------------------*
           move      "MX"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      "#EXI"               to   v-not                  .
           move      "N"                  to   v-alf                  .
           move      "SN"                 to   v-msk                  .
           move      "EXIT"               to   v-pfk (20)             .
           move      "UP  "               to   v-pfk (02)             .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           if        v-key                =    spaces
                     if      v-alf        =    "S"
                             move  "EXIT" to   v-key
                     else    move  "UP  " to   v-key                  .
      *              *-------------------------------------------------*
      *              * Se non function key EXIT si ripristina          *
      *              *-------------------------------------------------*
           if        v-key                not  = "EXIT"
                     go to exe-acc-cmp-exi-600.
      *              *-------------------------------------------------*
      *              * Altrimenti : ad Exit attivato                   *
      *              *-------------------------------------------------*
           go to     exe-acc-cmp-exi-800.
       exe-acc-cmp-exi-600.
      *              *-------------------------------------------------*
      *              * Se uscita senza Exit                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Flag di Exit non attivato                   *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cnt-acc-flg-exi      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     exe-acc-cmp-exi-900.
       exe-acc-cmp-exi-800.
      *              *-------------------------------------------------*
      *              * Se uscita con Exit                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     exe-acc-cmp-exi-900.
       exe-acc-cmp-exi-900.
      *              *-------------------------------------------------*
      *              * Ripristino linee 23 e 24 di note operative      *
      *              *-------------------------------------------------*
           move      "NT"                 to   v-ope                  .
           move      w-cnt-acc-sav-l23    to   v-nt1                  .
           move      w-cnt-acc-sav-l24    to   v-nt2                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       exe-acc-cmp-exi-920.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-acc-cmp-exi-999.
       exe-acc-cmp-exi-999.
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
      *              * Test su flag di visualizzazione titolo          *
      *              *-------------------------------------------------*
           if        w-cnt-sts-vis-tit    not  = spaces
                     move  spaces         to   w-cnt-sts-vis-tit
                     go to vis-tit-pgm-999.
       vis-tit-pgm-100.
      *              *-------------------------------------------------*
      *              * Erase video                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se impostazione chiave effettuata      *
      *                  *---------------------------------------------*
           if        w-cnt-tus-acc-key    not  = spaces
                     go to vis-tit-pgm-200.
      *                  *---------------------------------------------*
      *                  * Erase video                                 *
      *                  *---------------------------------------------*
           move      "ER"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tit-pgm-200.
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
           perform   vis-tit-pgm-des-000  thru vis-tit-pgm-des-999    .
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
      *    * Visualizzazione titolo programma                          *
      *    *                                                           *
      *    * Trattamento descrizione del programma                     *
      *    *-----------------------------------------------------------*
       vis-tit-pgm-des-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del numero pagina        *
      *              *-------------------------------------------------*
           if        w-cnt-sts-imp-npt    >    1
                     go to vis-tit-pgm-des-500.
       vis-tit-pgm-des-200.
      *              *-------------------------------------------------*
      *              * Prima pagina                                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Descrizione del programma                   *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      02                   to   v-lin                  .
           move      21                   to   v-pos                  .
           move      i-ide-des            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     vis-tit-pgm-des-900.
       vis-tit-pgm-des-500.
      *              *-------------------------------------------------*
      *              * Pagine successive                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Numero di pagine gestite                    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Concatenamento iniziale                 *
      *                      *-----------------------------------------*
           move      40                   to   w-all-str-lun          .
           move      02                   to   w-all-str-num          .
           move      i-ide-des            to   w-all-str-cat (1)      .
           move      "- ["                to   w-all-str-cat (2)      .
           perform   all-str-csb-000      thru all-str-csb-999        .
      *                      *-----------------------------------------*
      *                      * Editing numero pagina relativa          *
      *                      *-----------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      w-cnt-sts-imp-nrt    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-edt                to   w-all-str-cat (2)      .
      *                      *-----------------------------------------*
      *                      * Editing numero totale pagine relative   *
      *                      *-----------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      w-cnt-sts-imp-ntt    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-edt                to   w-all-str-cat (4)      .
      *                      *-----------------------------------------*
      *                      * Concatenamento finale                   *
      *                      *-----------------------------------------*
           move      40                   to   w-all-str-lun          .
           move      05                   to   w-all-str-num          .
           move      w-all-str-alf        to   w-all-str-cat (1)      .
           move      "/"                  to   w-all-str-cat (3)      .
           move      "]"                  to   w-all-str-cat (5)      .
           perform   all-str-cat-000      thru all-str-cat-999        .
      *                      *-----------------------------------------*
      *                      * Allineamento al centro descrizione      *
      *                      *-----------------------------------------*
           move      40                   to   w-all-str-lun          .
           perform   all-str-cen-000      thru all-str-cen-999        .
      *                      *-----------------------------------------*
      *                      * Visualizzazione                         *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      02                   to   v-lin                  .
           move      21                   to   v-pos                  .
           move      w-all-str-alf        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     vis-tit-pgm-des-900.
       vis-tit-pgm-des-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     vis-tit-pgm-des-999.
       vis-tit-pgm-des-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione tipo funzionamento                        *
      *    *-----------------------------------------------------------*
       vis-tip-fun-000.
           move      "TF"                 to   v-ope                  .
           move      w-cnt-mfu-tip-fun    to   v-tfu                  .
           move      w-cnt-acc-flg-aum    to   v-tfm                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tip-fun-999.
           exit.

      *    *===========================================================*
      *    * Routine pre-esecuzione programma                          *
      *    *-----------------------------------------------------------*
       pre-exe-pgm-000.
      *              *-------------------------------------------------*
      *              * Visualizzazione titolo programma                *
      *              *-------------------------------------------------*
           perform   vis-tit-pgm-000      thru vis-tit-pgm-999        .
           move      "#"                  to   w-cnt-sts-vis-tit      .
       pre-exe-pgm-100.
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
       pre-exe-pgm-200.
      *              *-------------------------------------------------*
      *              * Lettura della personalizzazione per il tipo di  *
      *              * funzionamento previsto per il programma         *
      *              *-------------------------------------------------*
           perform   prs-age-300-000      thru prs-age-300-999        .
      *              *-------------------------------------------------*
      *              * Lettura della personalizzazione per si/no ac-   *
      *              * cettazione contropartite vendite e resi su ven- *
      *              * dite in anagrafica agente                       *
      *              *-------------------------------------------------*
           perform   prs-snx-ctp-000      thru prs-snx-ctp-999        .
      *              *-------------------------------------------------*
      *              * Lettura della personalizzazione per numero li-  *
      *              * velli piano dei conti                           *
      *              *-------------------------------------------------*
           perform   prs-liv-pdc-000      thru prs-liv-pdc-999        .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice dipendenza del- *
      *              * l'azienda                                       *
      *              *-------------------------------------------------*
           perform   cod-cod-dpz-opn-000  thru cod-cod-dpz-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice agente          *
      *              *-------------------------------------------------*
           perform   cod-mne-age-opn-000  thru cod-mne-age-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice nazione         *
      *              *-------------------------------------------------*
           perform   cod-des-naz-opn-000  thru cod-des-naz-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione indirizzo + citta'     *
      *              *-------------------------------------------------*
           perform   ind-cec-geo-opn-000  thru ind-cec-geo-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione c.a.p. e citta'        *
      *              *-------------------------------------------------*
           perform   cap-cit-geo-opn-000  thru cap-cit-geo-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione comune                 *
      *              *-------------------------------------------------*
           perform   cod-com-geo-opn-000  thru cod-com-geo-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione frazione               *
      *              *-------------------------------------------------*
           perform   cod-fra-geo-opn-000  thru cod-fra-geo-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione localita'              *
      *              *-------------------------------------------------*
           perform   cod-loc-geo-opn-000  thru cod-loc-geo-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice cat. provvig.   *
      *              *-------------------------------------------------*
           perform   cmn-zpv-001-opn-000  thru cmn-zpv-001-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice sottoconto      *
      *              *-------------------------------------------------*
           perform   cod-mne-pdc-opn-000  thru cod-mne-pdc-opn-999    .
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
                     w-prs-age-300-abt    not  = "N"
                     move  "N"            to   w-prs-age-300-abt      .
      *
           if        w-prs-age-300-sns    not  = "S" and
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
      *    * Lettura della personalizzazione per si/no accettazione    *
      *    * contropartite vendite e resi su vendite in anagrafica     *
      *    * agente                                                    *
      *    *-----------------------------------------------------------*
       prs-snx-ctp-000.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione                       *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/age[snx-ctp]"   to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-alf          to   w-prs-snx-ctp
           else      move  spaces         to   w-prs-snx-ctp          .
      *                  *---------------------------------------------*
      *                  * Normalizzazione personalizzazione           *
      *                  *---------------------------------------------*
           if        w-prs-snx-ctp        not   = "S"
                     move  "N"            to   w-prs-snx-ctp          .
       prs-snx-ctp-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     prs-snx-ctp-999.
       prs-snx-ctp-999.
           exit.

      *    *===========================================================*
      *    * Lettura della personalizzazione per numero livelli piano  *
      *    * dei conti                                                 *
      *    *-----------------------------------------------------------*
       prs-liv-pdc-000.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione                       *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/cge[liv-pdc]"   to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-num          to   w-prs-liv-pdc
           else      move  3              to   w-prs-liv-pdc          .
       prs-liv-pdc-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     prs-liv-pdc-999.
       prs-liv-pdc-999.
           exit.

      *    *===========================================================*
      *    * Routine post-esecuzione programma                         *
      *    *-----------------------------------------------------------*
       pos-exe-pgm-000.
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice dipendenza del-*
      *              * l'azienda                                       *
      *              *-------------------------------------------------*
           perform   cod-cod-dpz-cls-000  thru cod-cod-dpz-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice agente         *
      *              *-------------------------------------------------*
           perform   cod-mne-age-cls-000  thru cod-mne-age-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice nazione        *
      *              *-------------------------------------------------*
           perform   cod-des-naz-cls-000  thru cod-des-naz-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione indirizzo + citta'    *
      *              *-------------------------------------------------*
           perform   ind-cec-geo-cls-000  thru ind-cec-geo-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione c.a.p. e citta'       *
      *              *-------------------------------------------------*
           perform   cap-cit-geo-cls-000  thru cap-cit-geo-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione comune                *
      *              *-------------------------------------------------*
           perform   cod-com-geo-cls-000  thru cod-com-geo-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione frazione              *
      *              *-------------------------------------------------*
           perform   cod-fra-geo-cls-000  thru cod-fra-geo-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione localita'             *
      *              *-------------------------------------------------*
           perform   cod-loc-geo-cls-000  thru cod-loc-geo-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice cat. provvig.  *
      *              *-------------------------------------------------*
           perform   cmn-zpv-001-cls-000  thru cmn-zpv-001-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice sottoconto     *
      *              *-------------------------------------------------*
           perform   cod-mne-pdc-cls-000  thru cod-mne-pdc-cls-999    .
       pos-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Open files                                                *
      *    *-----------------------------------------------------------*
       rou-opn-fls-000.
      *              *-------------------------------------------------*
      *              * [ada]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/azi/fls/ioc/obj/iofada"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ada                 .
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
      *              * [ags]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/age/fls/ioc/obj/iofags"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ags                 .
      *              *-------------------------------------------------*
      *              * [gxn]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/geo/fls/ioc/obj/iofgxn"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxn                 .
      *              *-------------------------------------------------*
      *              * [gxc]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/geo/fls/ioc/obj/iofgxc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxc                 .
      *              *-------------------------------------------------*
      *              * [zpv]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/age/fls/ioc/obj/iofzpv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zpv                 .
      *              *-------------------------------------------------*
      *              * [pdc]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofpdc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdc                 .
      *              *-------------------------------------------------*
      *              * Open modulo di determinazione contatti          *
      *              *-------------------------------------------------*
           perform   con-arc-tip-opn-000  thru con-arc-tip-opn-999    .
       rou-opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Close files                                               *
      *    *-----------------------------------------------------------*
       rou-cls-fls-000.
      *              *-------------------------------------------------*
      *              * [ada]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/azi/fls/ioc/obj/iofada"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ada                 .
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
      *              * [ags]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/age/fls/ioc/obj/iofags"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ags                 .
      *              *-------------------------------------------------*
      *              * [gxn]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/geo/fls/ioc/obj/iofgxn"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxn                 .
      *              *-------------------------------------------------*
      *              * [gxc]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/geo/fls/ioc/obj/iofgxc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxc                 .
      *              *-------------------------------------------------*
      *              * [zpv]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/age/fls/ioc/obj/iofzpv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zpv                 .
      *              *-------------------------------------------------*
      *              * [pdc]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofpdc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdc                 .
      *              *-------------------------------------------------*
      *              * Close modulo di determinazione contatti         *
      *              *-------------------------------------------------*
           perform   con-arc-tip-cls-000  thru con-arc-tip-cls-999    .
       rou-cls-fls-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campi chiave della registrazione             *
      *    *-----------------------------------------------------------*
       acc-key-reg-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-tus-acc-key      .
      *              *-------------------------------------------------*
      *              * Normalizzazione tipo funzionamento              *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-mfu-tip-fun      .
      *              *-------------------------------------------------*
      *              * Normalizzazione tipo impostazione               *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-mfu-tip-imp      .
      *              *-------------------------------------------------*
      *              * Normalizzazione status impostazione             *
      *              *-------------------------------------------------*
           move      1                    to   w-cnt-sts-imp-npt      .
           move      1                    to   w-cnt-sts-imp-nrt      .
      *
           move      spaces               to   w-cnt-sts-imp-key
                                               w-cnt-sts-imp-tes
                                               w-cnt-sts-imp-pte
                                               w-cnt-sts-ing-pte      .
      *              *-------------------------------------------------*
      *              * Normalizzazione status visualizzazione prompts  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-sts-pmt-key
                                               w-cnt-sts-pmt-tes
                                               w-cnt-sts-pmt-pte      .
      *              *-------------------------------------------------*
      *              * Normalizzazione status visualizzazione dati     *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-sts-vis-key
                                               w-cnt-sts-vis-tes
                                               w-cnt-sts-vis-pte      .
      *              *-------------------------------------------------*
      *              * Normalizzazione registrazione                   *
      *              *-------------------------------------------------*
           perform   nor-key-nok-000      thru nor-key-nok-999        .
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
      *                  * Prompts per campi chiave                    *
      *                  *---------------------------------------------*
           perform   pmt-key-reg-000      thru pmt-key-reg-999        .
           move      "#"                  to   w-cnt-sts-pmt-key      .
      *                  *---------------------------------------------*
      *                  * Prompts per prima pagina testata            *
      *                  *---------------------------------------------*
           perform   pmt-tes-reg-000      thru pmt-tes-reg-999        .
           move      "#"                  to   w-cnt-sts-pmt-ptx
                                              (w-cnt-sts-imp-npt)     .
      *                  *---------------------------------------------*
      *                  * Video in 'ON'                               *
      *                  *---------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Tipo impostazione : chiave                      *
      *              *-------------------------------------------------*
           move      "K"                  to   w-cnt-mfu-tip-imp      .
       acc-key-reg-100.
      *              *-------------------------------------------------*
      *              * Accettazioni                                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione func-key di impostazione    *
      *                  *---------------------------------------------*
           move      spaces               to   v-key                  .
      *                  *---------------------------------------------*
      *                  * Codice agente                               *
      *                  *---------------------------------------------*
           perform   acc-cod-age-000      thru acc-cod-age-999        .
           if        w-cnt-tus-acc-key    not  = spaces
                     go to acc-key-reg-999.
       acc-key-reg-900.
      *              *-------------------------------------------------*
      *              * Flag di controllo status impostazioni chiave    *
      *              *-------------------------------------------------*
           move      "#"                  to   w-cnt-sts-imp-key      .
      *              *-------------------------------------------------*
      *              * Flag di controllo status visual. dati chiave    *
      *              *-------------------------------------------------*
           move      "#"                  to   w-cnt-sts-vis-key      .
      *              *-------------------------------------------------*
      *              * Test se chiave vuota                            *
      *              *-------------------------------------------------*
           perform   cnt-key-vuo-000      thru cnt-key-vuo-999        .
           if        w-cnt-key-vuo-flg    not  = spaces
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-key-reg-999.
      *              *-------------------------------------------------*
      *              * Controllo globale su tasto Do su chiave         *
      *              *-------------------------------------------------*
           perform   cnt-tdo-key-000      thru cnt-tdo-key-999        .
           if        w-cnt-tdo-key-flg    not  = spaces
                     go to acc-key-reg-100.
           move      "S"                  to   w-cnt-tus-acc-key      .
       acc-key-reg-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campi chiave della registrazione          *
      *    *-----------------------------------------------------------*
       vis-key-reg-000.
      *              *-------------------------------------------------*
      *              * Codice agente                                   *
      *              *-------------------------------------------------*
           perform   vis-cod-age-000      thru vis-cod-age-999        .
       vis-key-reg-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per campi chiave                  *
      *    *-----------------------------------------------------------*
       pmt-key-reg-000.
      *              *-------------------------------------------------*
      *              * Erase linee impegnate                           *
      *              *-------------------------------------------------*
           move      "EL"                 to   v-ope                  .
           move      04                   to   v-lin                  .
           move      05                   to   v-lto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Codice agente                                   *
      *              *-------------------------------------------------*
           perform   pmt-cod-age-000      thru pmt-cod-age-999        .
      *              *-------------------------------------------------*
      *              * Linea di trattini                               *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      all "-"              to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-key-reg-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per Codice agente                 *
      *    *-----------------------------------------------------------*
       pmt-cod-age-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice agente              :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-cod-age-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo chiave : Codice agente                 *
      *    *-----------------------------------------------------------*
       acc-cod-age-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cod-age-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-age-ope      .
           move      w-tes-cod-age        to   w-cod-mne-age-cod      .
           move      04                   to   w-cod-mne-age-lin      .
           move      30                   to   w-cod-mne-age-pos      .
           move      08                   to   w-cod-mne-age-nln      .
           move      30                   to   w-cod-mne-age-nps      .
           move      "<B"                 to   v-edm                  .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-mne-age-cll-000  thru cod-mne-age-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-mne-age-foi-000  thru cod-mne-age-foi-999    .
       acc-cod-age-110.
           perform   cod-mne-age-cll-000  thru cod-mne-age-cll-999    .
           if        w-cod-mne-age-ope    =    "F+"
                     go to acc-cod-age-115.
           if        w-cod-mne-age-ope    =    "AC"
                     go to acc-cod-age-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-age-115.
           perform   cod-mne-age-foi-000  thru cod-mne-age-foi-999    .
           go to     acc-cod-age-110.
       acc-cod-age-120.
           move      w-cod-mne-age-cod    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-cod-age-999.
       acc-cod-age-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-cod-age          .
       acc-cod-age-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
           if        w-tes-cod-age        not  = zero
                     go to acc-cod-age-600.
      *                  *---------------------------------------------*
      *                  * Se il codice impostato e' zero              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se inserimento consentito          *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test                                *
      *                          *-------------------------------------*
           if        w-cnt-mfu-vis-sgr    not  = "V"
                     go to acc-cod-age-412.
      *                          *-------------------------------------*
      *                          * Messaggio                           *
      *                          *-------------------------------------*
           move      "Inserimento non consentito !                      
      -              "               "    to   w-err-box-err-msg      .
      *                          *-------------------------------------*
      *                          * Box di errore                       *
      *                          *-------------------------------------*
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                          *-------------------------------------*
      *                          * A reimpostazione                    *
      *                          *-------------------------------------*
           go to     acc-cod-age-100.
       acc-cod-age-412.
      *                      *-----------------------------------------*
      *                      * Attribuzione codice automatico          *
      *                      *-----------------------------------------*
           perform   att-cod-aut-000      thru att-cod-aut-999        .
      *                      *-----------------------------------------*
      *                      * Codice automatico in campo di destina-  *
      *                      * zione                                   *
      *                      *-----------------------------------------*
           move      w-enc-age-val-pos    to   w-tes-cod-age          .
           move      "#"                  to   w-tes-cod-age-aut      .
      *                      *-----------------------------------------*
      *                      * Visualizzazione del codice              *
      *                      *-----------------------------------------*
           perform   vis-cod-age-000      thru vis-cod-age-999        .
       acc-cod-age-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cod-age-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                not  = "DO  "
                     go to acc-cod-age-999.
           perform   cnt-key-vuo-000      thru cnt-key-vuo-999        .
           if        w-cnt-key-vuo-flg    not  = spaces
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-cod-age-999.
           perform   cnt-tdo-key-000      thru cnt-tdo-key-999        .
           if        w-cnt-tdo-key-flg    not  = spaces
                     go to acc-cod-age-100
           else      move  "S"            to   w-cnt-tus-acc-key
                     go to acc-cod-age-999.
       acc-cod-age-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo chiave : Codice agente              *
      *    *-----------------------------------------------------------*
       vis-cod-age-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      04                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "<B"                 to   v-edm                  .
           move      w-tes-cod-age        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-age-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campi non chiave della registrazione         *
      *    *-----------------------------------------------------------*
       acc-nok-reg-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-tus-acc-nok      .
      *              *-------------------------------------------------*
      *              * Visualizzazione tipo funzionamento              *
      *              *-------------------------------------------------*
           perform   vis-tip-fun-000      thru vis-tip-fun-999        .
      *              *-------------------------------------------------*
      *              * Assestamento status di impostazione se in modi- *
      *              * fica o visualizzazione, per testata             *
      *              *-------------------------------------------------*
           if        w-cnt-mfu-tip-fun    =    "M" or
                     w-cnt-mfu-tip-fun    =    "V"
                     move  "#"            to   w-cnt-sts-imp-tes
                     move  all "#"        to   w-cnt-sts-imp-pte
                     move  all "#"        to   w-cnt-sts-ing-pte      .
       acc-nok-reg-200.
      *              *-------------------------------------------------*
      *              * Trattamento testata                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Determinazione del numero di pagine che     *
      *                  * compongono la testata                       *
      *                  *---------------------------------------------*
           perform   dmp-tes-reg-000      thru dmp-tes-reg-999        .
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
      *                  * Prompts per campi chiave                    *
      *                  *---------------------------------------------*
           if        w-cnt-sts-pmt-key    =    spaces
                     perform pmt-key-reg-000
                                          thru pmt-key-reg-999
                     move    "#"          to   w-cnt-sts-pmt-key      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione dati campi chiave           *
      *                  *---------------------------------------------*
           if        w-cnt-sts-vis-key    =    spaces
                     perform vis-key-reg-000
                                          thru vis-key-reg-999
                     move    "#"          to   w-cnt-sts-vis-key      .
      *                  *---------------------------------------------*
      *                  * Prompts per pagina di testata               *
      *                  *---------------------------------------------*
           if        w-cnt-sts-pmt-ptx
                    (w-cnt-sts-imp-npt)   =    spaces
                     perform pmt-tes-reg-000
                                          thru pmt-tes-reg-999
                     move    "#"          to   w-cnt-sts-pmt-ptx
                                              (w-cnt-sts-imp-npt)     .
      *                  *---------------------------------------------*
      *                  * Visualizzazione dati pagina testata         *
      *                  *---------------------------------------------*
           if        w-cnt-mfu-tip-fun    =    "M"      or
                     w-cnt-mfu-tip-fun    =    "V"      or
                     w-cnt-sts-imp-ptx
                    (w-cnt-sts-imp-npt)   not  = spaces or
                     w-cnt-sts-ing-ptx
                    (w-cnt-sts-imp-npt)   not  = spaces
                     if    w-cnt-sts-vis-ptx
                          (w-cnt-sts-imp-npt)
                                          =    spaces
                           perform vis-tes-reg-000
                                          thru vis-tes-reg-999
                           move    "#"    to   w-cnt-sts-vis-ptx
                                              (w-cnt-sts-imp-npt)     .
      *                  *---------------------------------------------*
      *                  * Flag di ingresso in pagina di testata       *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cnt-sts-ing-ptx
                                              (w-cnt-sts-imp-npt)     .
      *                  *---------------------------------------------*
      *                  * Video in 'ON'                               *
      *                  *---------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Accettazione dati testata                   *
      *                  *---------------------------------------------*
           perform   acc-tes-reg-000      thru acc-tes-reg-999        .
      *                  *---------------------------------------------*
      *                  * Se tipo uscita definitivo                   *
      *                  *---------------------------------------------*
           if        w-cnt-tus-acc-tes    =    "S" or
                     w-cnt-tus-acc-tes    =    "X" or
                     w-cnt-tus-acc-tes    =    "E"
                     move  w-cnt-tus-acc-tes
                                          to   w-cnt-tus-acc-nok
                     go to acc-nok-reg-999.
      *                  *---------------------------------------------*
      *                  * Se spostamento di pagina                    *
      *                  *---------------------------------------------*
           if        w-cnt-tus-acc-tes    =    "-"
                     go to acc-nok-reg-300
           else      go to acc-nok-reg-400.
       acc-nok-reg-300.
      *                      *-----------------------------------------*
      *                      * Se spostamento a pagina precedente      *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se la pagina attuale e' 1 si ignora *
      *                          *-------------------------------------*
           if        w-cnt-sts-imp-npt    not  > 1
                     go to acc-nok-reg-200.
      *                          *-------------------------------------*
      *                          * Status di visualizzazione prompts e *
      *                          * dati della chiave a : no            *
      *                          *-------------------------------------*
           move      spaces               to   w-cnt-sts-pmt-key      .
           move      spaces               to   w-cnt-sts-vis-key      .
      *                          *-------------------------------------*
      *                          * Status di visualizzazione prompts e *
      *                          * dati della pagina attuale a : no    *
      *                          *-------------------------------------*
           move      spaces               to   w-cnt-sts-pmt-ptx
                                              (w-cnt-sts-imp-npt)     .
           move      spaces               to   w-cnt-sts-vis-ptx
                                              (w-cnt-sts-imp-npt)     .
       acc-nok-reg-325.
      *                          *-------------------------------------*
      *                          * Decremento numero pagina attuale    *
      *                          *-------------------------------------*
           subtract  1                    from w-cnt-sts-imp-npt      .
      *                          *-------------------------------------*
      *                          * Test se pagina da trattare          *
      *                          *-------------------------------------*
           perform   snp-tes-reg-000      thru snp-tes-reg-999        .
      *                          *-------------------------------------*
      *                          * Se no : decremento ulteriore        *
      *                          *-------------------------------------*
           if        w-cnt-sts-imp-snp    not  = spaces and
                     w-cnt-sts-imp-npt    >    1
                     go to acc-nok-reg-325.
      *                          *-------------------------------------*
      *                          * Decremento numero pagina relativa   *
      *                          *-------------------------------------*
           if        w-cnt-sts-imp-nrt    >    1
                     subtract  1          from w-cnt-sts-imp-nrt      .
      *                          *-------------------------------------*
      *                          * Riciclo ad impostazione testata     *
      *                          *-------------------------------------*
           go to     acc-nok-reg-200.
       acc-nok-reg-400.
      *                      *-----------------------------------------*
      *                      * Se spostamento a pagina successiva      *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Salvataggio numero pagina attuale   *
      *                          *-------------------------------------*
           move      w-cnt-sts-imp-npt    to   w-cnt-sts-imp-svp      .
       acc-nok-reg-425.
      *                          *-------------------------------------*
      *                          * Status di impostazione dati della   *
      *                          * della pagina attuale a : si'        *
      *                          *-------------------------------------*
           move      "#"                  to   w-cnt-sts-imp-ptx
                                              (w-cnt-sts-imp-npt)     .
      *                          *-------------------------------------*
      *                          * Se la pagina attuale e' la massima  *
      *                          * si pone lo status di impostazione   *
      *                          * dati generale testata a si'         *
      *                          *-------------------------------------*
           if        w-cnt-sts-imp-npt    not  < w-cnt-sts-imp-mpt
                     move  "#"            to   w-cnt-sts-imp-tes      .
      *                          *-------------------------------------*
      *                          * Se la pagina attuale e' la massima  *
      *                          * si va' a conferma impostazioni      *
      *                          *-------------------------------------*
           if        w-cnt-sts-imp-npt    not  < w-cnt-sts-imp-mpt
                     move  w-cnt-sts-imp-svp
                                          to   w-cnt-sts-imp-npt
                     go to acc-nok-reg-800.
      *                          *-------------------------------------*
      *                          * Incremento numero pagina attuale    *
      *                          *-------------------------------------*
           add       1                    to   w-cnt-sts-imp-npt      .
      *                          *-------------------------------------*
      *                          * Test se pagina da trattare          *
      *                          *-------------------------------------*
           perform   snp-tes-reg-000      thru snp-tes-reg-999        .
      *                          *-------------------------------------*
      *                          * Se no : a re-incremento             *
      *                          *-------------------------------------*
           if        w-cnt-sts-imp-snp    not  = spaces
                     go to acc-nok-reg-425.
      *                          *-------------------------------------*
      *                          * Incremento numero pagina relativa   *
      *                          *-------------------------------------*
           add       1                    to   w-cnt-sts-imp-nrt      .
      *                          *-------------------------------------*
      *                          * Status di visualizzazione prompts e *
      *                          * dati della chiave a : no            *
      *                          *-------------------------------------*
           move      spaces               to   w-cnt-sts-pmt-key      .
           move      spaces               to   w-cnt-sts-vis-key      .
      *                          *-------------------------------------*
      *                          * Status di visualizzazione prompts e *
      *                          * dati della pagina impostata a : no  *
      *                          *-------------------------------------*
           move      spaces               to   w-cnt-sts-pmt-ptx
                                              (w-cnt-sts-imp-svp)     .
           move      spaces               to   w-cnt-sts-vis-ptx
                                              (w-cnt-sts-imp-svp)     .
      *                          *-------------------------------------*
      *                          * Riciclo ad impostazione testata     *
      *                          *-------------------------------------*
           go to     acc-nok-reg-200.
       acc-nok-reg-800.
      *              *-------------------------------------------------*
      *              * Conferma impostazioni                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Cancellazione eventuali note operative      *
      *                  *---------------------------------------------*
           move      "NT"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-nok-reg-810.
      *                  *---------------------------------------------*
      *                  * Test se sola visualizzazione                *
      *                  *---------------------------------------------*
           if        w-cnt-mfu-tip-fun    =    "V"
                     go to acc-nok-reg-820
           else      go to acc-nok-reg-830.
       acc-nok-reg-820.
      *                      *-----------------------------------------*
      *                      * Accettazione se sola visualizzazione    *
      *                      *-----------------------------------------*
           move      "MX"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      "#VIS"               to   v-not                  .
           move      spaces               to   v-alf                  .
           move      "E"                  to   v-msk                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "EXIT"               to   v-pfk (20)             .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           if        v-key                not  = spaces
                     go to acc-nok-reg-840.
           if        v-alf                =    "E"
                     move  "EXIT"         to   v-key                  .
           go to     acc-nok-reg-840.
       acc-nok-reg-830.
      *                      *-----------------------------------------*
      *                      * Accettazione se inserimento o modifica  *
      *                      *-----------------------------------------*
           move      "MX"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      "#SAV"               to   v-not                  .
           move      spaces               to   v-alf                  .
           move      "SNE"                to   v-msk                  .
           move      "DO  "               to   v-pfk (05)             .
           move      "UP  "               to   v-pfk (01)             .
           move      "EXIT"               to   v-pfk (20)             .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           if        v-key                not  = spaces
                     go to acc-nok-reg-840.
           if        v-alf                =    "S"
                     move   "DO  "        to   v-key
           else if   v-alf                =    "E"
                     move   "EXIT"        to   v-key
           else if   v-alf                =    "N"
                     move   "UP  "        to   v-key                  .
       acc-nok-reg-840.
      *                  *---------------------------------------------*
      *                  * Test su risposta dell'utente                *
      *                  *---------------------------------------------*
           if        v-key                =    "DO  "
                     go to acc-nok-reg-850
           else if   v-key                =    "EXIT"
                     go to acc-nok-reg-860
           else if   v-key                =    "UP  "
                     go to acc-nok-reg-870
           else      go to acc-nok-reg-810.
       acc-nok-reg-850.
      *                  *---------------------------------------------*
      *                  * Se Do                                       *
      *                  *---------------------------------------------*
           perform   cnt-tdo-nok-000      thru cnt-tdo-nok-999        .
           if        w-cnt-tdo-nok-flg    =    spaces
                     move  "S"            to   w-cnt-tus-acc-nok
                     go to acc-nok-reg-999
           else      move  spaces         to   w-cnt-tdo-nok-flg
                     go to acc-nok-reg-800.
       acc-nok-reg-860.
      *                  *---------------------------------------------*
      *                  * Se Exit                                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Subroutine di controllo per Exit        *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se Exit non attivato : come 'Up'    *
      *                          *-------------------------------------*
           perform   exe-acc-cmp-exi-000  thru exe-acc-cmp-exi-999    .
           if        w-cnt-acc-flg-exi    not  = spaces
                     go to acc-nok-reg-870.
      *                          *-------------------------------------*
      *                          * Se Exit attivato                    *
      *                          *-------------------------------------*
           move      "E"                  to   w-cnt-tus-acc-nok      .
           go to     acc-nok-reg-999.
       acc-nok-reg-870.
      *                  *---------------------------------------------*
      *                  * Se Up                                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Ad accettazione testata                 *
      *                      *-----------------------------------------*
           move      spaces               to   v-key                  .
           go to     acc-nok-reg-200.
       acc-nok-reg-999.
           exit.

      *    *===========================================================*
      *    * Determinazione numero pagine che compongono la testata    *
      *    *-----------------------------------------------------------*
       dmp-tes-reg-000.
      *              *-------------------------------------------------*
      *              * La testata e' composta di nr. 5 pagine          *
      *              *-------------------------------------------------*
           move      05                   to   w-cnt-sts-imp-mpt      .
      *              *-------------------------------------------------*
      *              * Determinazione delle pagine relative gestite    *
      *              *-------------------------------------------------*
           move      zero                 to   w-cnt-sts-imp-ntt      .
           move      zero                 to   w-cnt-sts-imp-ntw      .
      *              *-------------------------------------------------*
      *              * Salvataggio numero pagina in trattamento        *
      *              *-------------------------------------------------*
           move      w-cnt-sts-imp-npt    to   w-cnt-sts-imp-nts      .
       dmp-tes-reg-200.
      *              *-------------------------------------------------*
      *              * Calcolo numero relativo pagine                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Incremento contatore di comodo              *
      *                  *---------------------------------------------*
           add       1                    to   w-cnt-sts-imp-ntw      .
           if        w-cnt-sts-imp-ntw    >    w-cnt-sts-imp-mpt
                     go to dmp-tes-reg-900.
      *                  *---------------------------------------------*
      *                  * Test se pagina da trattare                  *
      *                  *---------------------------------------------*
           move      w-cnt-sts-imp-ntw    to   w-cnt-sts-imp-npt      .
           perform   snp-tes-reg-000      thru snp-tes-reg-999        .
      *                  *---------------------------------------------*
      *                  * Test su flag di uscita                      *
      *                  *---------------------------------------------*
           if        w-cnt-sts-imp-snp    not  = spaces
                     go to dmp-tes-reg-200.
      *                  *---------------------------------------------*
      *                  * Incremento numero pagine relative           *
      *                  *---------------------------------------------*
           add       1                    to   w-cnt-sts-imp-ntt      .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     dmp-tes-reg-200.
       dmp-tes-reg-900.
      *              *-------------------------------------------------*
      *              * Ripristino numero pagina in trattamento         *
      *              *-------------------------------------------------*
           move      w-cnt-sts-imp-nts    to   w-cnt-sts-imp-npt      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     dmp-tes-reg-999.
       dmp-tes-reg-999.
           exit.

      *    *===========================================================*
      *    * Determinazione si/no pagina w-cnt-sts-imp-npt da trattare *
      *    *-----------------------------------------------------------*
       snp-tes-reg-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-sts-imp-snp      .
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del numero pagina        *
      *              *-------------------------------------------------*
           go to     snp-tes-reg-100
                     snp-tes-reg-200
                     snp-tes-reg-300
                     snp-tes-reg-400
                     depending            on   w-cnt-sts-imp-npt      .
           go to     snp-tes-reg-999.
       snp-tes-reg-100.
      *              *-------------------------------------------------*
      *              * Test per la pagina 1                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Sempre attiva                               *
      *                  *---------------------------------------------*
           go to     snp-tes-reg-999.
       snp-tes-reg-200.
      *              *-------------------------------------------------*
      *              * Test per la pagina 2                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Sempre attiva                               *
      *                  *---------------------------------------------*
           go to     snp-tes-reg-999.
       snp-tes-reg-300.
      *              *-------------------------------------------------*
      *              * Test per la pagina 3                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Sempre attiva                               *
      *                  *---------------------------------------------*
           go to     snp-tes-reg-999.
       snp-tes-reg-400.
      *              *-------------------------------------------------*
      *              * Test per la pagina 4                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Sempre attiva                               *
      *                  *---------------------------------------------*
           go to     snp-tes-reg-999.
       snp-tes-reg-500.
      *              *-------------------------------------------------*
      *              * Test per la pagina 5                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se le personalizzazioni lo consentono       *
      *                  *---------------------------------------------*
           if        w-prs-snx-ctp        not  = "S"
                     move  "#"            to   w-cnt-sts-imp-snp
                     go to snp-tes-reg-999.
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     snp-tes-reg-999.
       snp-tes-reg-999.
           exit.

      *    *===========================================================*
      *    * Accettazione testata registrazione                        *
      *    *-----------------------------------------------------------*
       acc-tes-reg-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione tipo uscita                     *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-tus-acc-tes      .
      *              *-------------------------------------------------*
      *              * Normalizzazione function-key                    *
      *              *-------------------------------------------------*
           move      spaces               to   v-key                  .
      *              *-------------------------------------------------*
      *              * Tipo impostazione : testata                     *
      *              *-------------------------------------------------*
           move      "T"                  to   w-cnt-mfu-tip-imp      .
      *              *-------------------------------------------------*
      *              * Accettazioni                                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del numero pagina    *
      *                  *---------------------------------------------*
           go to     acc-tes-reg-100
                     acc-tes-reg-200
                     acc-tes-reg-300
                     acc-tes-reg-400
                     acc-tes-reg-500
                     depending            on   w-cnt-sts-imp-npt      .
           go to     acc-tes-reg-999.
       acc-tes-reg-100.
      *              *-------------------------------------------------*
      *              * Pagina 1                                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione func-key di impostazione    *
      *                  *---------------------------------------------*
           move      spaces               to   v-key                  .
      *                  *---------------------------------------------*
      *                  * Ad accettazione nome agente                 *
      *                  *---------------------------------------------*
           go to     acc-tes-reg-110.
       acc-tes-reg-105.
      *                  *---------------------------------------------*
      *                  * Codice nazione                              *
      *                  *---------------------------------------------*
           perform   acc-cod-naz-000      thru acc-cod-naz-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
       acc-tes-reg-110.
      *                  *---------------------------------------------*
      *                  * Nome agente                                 *
      *                  *---------------------------------------------*
           perform   acc-nom-age-000      thru acc-nom-age-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-105.
       acc-tes-reg-120.
      *                  *---------------------------------------------*
      *                  * Ragione sociale                             *
      *                  *---------------------------------------------*
           perform   acc-rag-soc-000      thru acc-rag-soc-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-110.
       acc-tes-reg-130.
      *                  *---------------------------------------------*
      *                  * Indirizzo                                   *
      *                  *---------------------------------------------*
           perform   acc-via-age-000      thru acc-via-age-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-120.
       acc-tes-reg-140.
      *                  *---------------------------------------------*
      *                  * C.a.p. e citta'                             *
      *                  *---------------------------------------------*
           perform   acc-loc-age-000      thru acc-loc-age-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-130.
       acc-tes-reg-150.
      *                  *---------------------------------------------*
      *                  * Codice comune                               *
      *                  *---------------------------------------------*
           perform   acc-cod-cmn-000      thru acc-cod-cmn-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-140.
       acc-tes-reg-155.
      *                  *---------------------------------------------*
      *                  * Codice frazione                             *
      *                  *---------------------------------------------*
           perform   acc-cod-fzn-000      thru acc-cod-fzn-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-150.
       acc-tes-reg-160.
      *                  *---------------------------------------------*
      *                  * Codice localita'                            *
      *                  *---------------------------------------------*
           perform   acc-cod-lct-000      thru acc-cod-lct-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-155.
       acc-tes-reg-170.
      *                  *---------------------------------------------*
      *                  * Partita iva                                 *
      *                  *---------------------------------------------*
           perform   acc-prt-iva-000      thru acc-prt-iva-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-160.
       acc-tes-reg-180.
      *                  *---------------------------------------------*
      *                  * Codice fiscale                              *
      *                  *---------------------------------------------*
           perform   acc-cod-fis-000      thru acc-cod-fis-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-170.
       acc-tes-reg-190.
      *                  *---------------------------------------------*
      *                  * Codice mnemonico                            *
      *                  *---------------------------------------------*
           perform   acc-cod-mne-000      thru acc-cod-mne-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-180.
      *                  *---------------------------------------------*
      *                  * Presa visione per pagina 1                  *
      *                  *---------------------------------------------*
           perform   acc-pre-vpg-000      thru acc-pre-vpg-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-190.
      *                  *---------------------------------------------*
      *                  * Fine Pagina                                 *
      *                  *---------------------------------------------*
           move      "+"                  to   w-cnt-tus-acc-tes      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     acc-tes-reg-999.
       acc-tes-reg-200.
      *                  *---------------------------------------------*
      *                  * Contatti e utenze                           *
      *                  *---------------------------------------------*
           perform   acc-con-arc-000      thru acc-con-arc-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     move  "-"            to   w-cnt-tus-acc-tes
                     go to acc-tes-reg-999.
      *                  *---------------------------------------------*
      *                  * Fine Pagina                                 *
      *                  *---------------------------------------------*
           move      "+"                  to   w-cnt-tus-acc-tes      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     acc-tes-reg-999.
       acc-tes-reg-300.
      *              *-------------------------------------------------*
      *              * Pagina 3                                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Categoria di provvigioni                    *
      *                  *---------------------------------------------*
           perform   acc-cat-pvg-000      thru acc-cat-pvg-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     move  "-"            to   w-cnt-tus-acc-tes
                     go to acc-tes-reg-999.
       acc-tes-reg-310.
      *                  *---------------------------------------------*
      *                  * % di provvigione                            *
      *                  *---------------------------------------------*
           perform   acc-per-pvg-000      thru acc-per-pvg-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-300.
       acc-tes-reg-320.
      *                  *---------------------------------------------*
      *                  * Codice agente supervisore                   *
      *                  *---------------------------------------------*
           perform   acc-sup-age-000      thru acc-sup-age-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-310.
       acc-tes-reg-330.
      *                  *---------------------------------------------*
      *                  * Targa                                       *
      *                  *---------------------------------------------*
           perform   acc-num-trg-000      thru acc-num-trg-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-320.
       acc-tes-reg-335.
      *                  *---------------------------------------------*
      *                  * Dipendenza operativa                        *
      *                  *---------------------------------------------*
           perform   acc-dpz-ope-000      thru acc-dpz-ope-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-330.
       acc-tes-reg-340.
      *                  *---------------------------------------------*
      *                  * Tipo maturazione                            *
      *                  *---------------------------------------------*
           perform   acc-tip-mat-000      thru acc-tip-mat-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-335.
       acc-tes-reg-350.
      *                  *---------------------------------------------*
      *                  * Indicatori per conteggi provvigionali       *
      *                  *---------------------------------------------*
           perform   acc-flg-cpv-000      thru acc-flg-cpv-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-340.
       acc-tes-reg-390.
      *                  *---------------------------------------------*
      *                  * Presa visione per pagina 3                  *
      *                  *---------------------------------------------*
           perform   acc-pre-vpg-000      thru acc-pre-vpg-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-350.
      *                  *---------------------------------------------*
      *                  * Fine Pagina                                 *
      *                  *---------------------------------------------*
           move      "+"                  to   w-cnt-tus-acc-tes      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     acc-tes-reg-999.
       acc-tes-reg-400.
      *              *-------------------------------------------------*
      *              * Pagina 4                                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Data inizio attivita'                       *
      *                  *---------------------------------------------*
           perform   acc-dat-iat-000      thru acc-dat-iat-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     move  "-"            to   w-cnt-tus-acc-tes
                     go to acc-tes-reg-999.
       acc-tes-reg-410.
      *                  *---------------------------------------------*
      *                  * Status commerciale                          *
      *                  *---------------------------------------------*
           perform   acc-sta-tus-000      thru acc-sta-tus-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-400.
       acc-tes-reg-420.
      *                  *---------------------------------------------*
      *                  * Data determinazione status                  *
      *                  *---------------------------------------------*
           perform   acc-sta-tud-000      thru acc-sta-tud-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-410.
       acc-tes-reg-430.
      *                  *---------------------------------------------*
      *                  * Codice agente di riferimento                *
      *                  *---------------------------------------------*
           perform   acc-sta-tuc-000      thru acc-sta-tuc-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-420.
       acc-tes-reg-440.
      *                  *---------------------------------------------*
      *                  * Modalita' di trattamento statistiche        *
      *                  *---------------------------------------------*
           perform   acc-sta-tux-000      thru acc-sta-tux-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-430.
       acc-tes-reg-490.
      *                  *---------------------------------------------*
      *                  * Presa visione per pagina 4                  *
      *                  *---------------------------------------------*
           perform   acc-pre-vpg-000      thru acc-pre-vpg-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-440.
      *                  *---------------------------------------------*
      *                  * Fine Pagina                                 *
      *                  *---------------------------------------------*
           move      "+"                  to   w-cnt-tus-acc-tes      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     acc-tes-reg-999.
       acc-tes-reg-500.
      *              *-------------------------------------------------*
      *              * Pagina 5                                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Contropartita vendite                       *
      *                  *---------------------------------------------*
           perform   acc-ctp-ven-000      thru acc-ctp-ven-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     move  "-"            to   w-cnt-tus-acc-tes
                     go to acc-tes-reg-999.
       acc-tes-reg-510.
      *                  *---------------------------------------------*
      *                  * Contropartita resi su vendite               *
      *                  *---------------------------------------------*
           perform   acc-ctp-rsv-000      thru acc-ctp-rsv-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-500.
      *                  *---------------------------------------------*
      *                  * Fine Pagina                                 *
      *                  *---------------------------------------------*
           move      "+"                  to   w-cnt-tus-acc-tes      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     acc-tes-reg-999.
       acc-tes-reg-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione testata registrazione                     *
      *    *-----------------------------------------------------------*
       vis-tes-reg-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del numero pagina        *
      *              *-------------------------------------------------*
           go to     vis-tes-reg-100
                     vis-tes-reg-200
                     vis-tes-reg-300
                     vis-tes-reg-400
                     vis-tes-reg-500
                     depending            on   w-cnt-sts-imp-npt      .
           go to     vis-tes-reg-999.
       vis-tes-reg-100.
      *              *-------------------------------------------------*
      *              * Codice nazione                                  *
      *              *-------------------------------------------------*
           perform   vis-cod-naz-000      thru vis-cod-naz-999        .
      *              *-------------------------------------------------*
      *              * Descrizione nazione                             *
      *              *-------------------------------------------------*
           perform   vis-des-naz-000      thru vis-des-naz-999        .
      *              *-------------------------------------------------*
      *              * Nome dell'agente                                *
      *              *-------------------------------------------------*
           perform   vis-nom-age-000      thru vis-nom-age-999        .
      *              *-------------------------------------------------*
      *              * Ragione sociale                                 *
      *              *-------------------------------------------------*
           perform   vis-rag-soc-000      thru vis-rag-soc-999        .
      *              *-------------------------------------------------*
      *              * Indirizzo                                       *
      *              *-------------------------------------------------*
           perform   vis-via-age-000      thru vis-via-age-999        .
      *              *-------------------------------------------------*
      *              * C.a.p. e citta'                                 *
      *              *-------------------------------------------------*
           perform   vis-loc-age-000      thru vis-loc-age-999        .
      *              *-------------------------------------------------*
      *              * Codice comune                                   *
      *              *-------------------------------------------------*
           perform   vis-cod-cmn-000      thru vis-cod-cmn-999        .
      *              *-------------------------------------------------*
      *              * Descrizione comune                              *
      *              *-------------------------------------------------*
           perform   vis-des-cmn-000      thru vis-des-cmn-999        .
      *              *-------------------------------------------------*
      *              * Codice provincia                                *
      *              *-------------------------------------------------*
           perform   vis-cod-prv-000      thru vis-cod-prv-999        .
      *              *-------------------------------------------------*
      *              * Codice frazione                                 *
      *              *-------------------------------------------------*
           perform   vis-cod-fzn-000      thru vis-cod-fzn-999        .
      *              *-------------------------------------------------*
      *              * Descrizione frazione                            *
      *              *-------------------------------------------------*
           perform   vis-des-fzn-000      thru vis-des-fzn-999        .
      *              *-------------------------------------------------*
      *              * Codice localita'                                *
      *              *-------------------------------------------------*
           perform   vis-cod-lct-000      thru vis-cod-lct-999        .
      *              *-------------------------------------------------*
      *              * Descrizione localita'                           *
      *              *-------------------------------------------------*
           perform   vis-des-lct-000      thru vis-des-lct-999        .
      *              *-------------------------------------------------*
      *              * Partita iva                                     *
      *              *-------------------------------------------------*
           perform   vis-prt-iva-000      thru vis-prt-iva-999        .
      *              *-------------------------------------------------*
      *              * Codice fiscale                                  *
      *              *-------------------------------------------------*
           perform   vis-cod-fis-000      thru vis-cod-fis-999        .
      *              *-------------------------------------------------*
      *              * Codice mnemonico                                *
      *              *-------------------------------------------------*
           perform   vis-cod-mne-000      thru vis-cod-mne-999        .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     vis-tes-reg-999.
       vis-tes-reg-200.
      *              *-------------------------------------------------*
      *              * Contatti e utenze                               *
      *              *-------------------------------------------------*
           perform   vis-con-arc-000      thru vis-con-arc-999        .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     vis-tes-reg-999.
       vis-tes-reg-300.
      *              *-------------------------------------------------*
      *              * Categoria di provvigioni                        *
      *              *-------------------------------------------------*
           perform   vis-cat-pvg-000      thru vis-cat-pvg-999        .
      *              *-------------------------------------------------*
      *              * Descrizione categoria di provvigioni            *
      *              *-------------------------------------------------*
           perform   vis-des-pvg-000      thru vis-des-pvg-999        .
      *              *-------------------------------------------------*
      *              * % di provvigione                                *
      *              *-------------------------------------------------*
           perform   vis-per-pvg-000      thru vis-per-pvg-999        .
      *              *-------------------------------------------------*
      *              * Codice agente supervisore                       *
      *              *-------------------------------------------------*
           perform   vis-sup-age-000      thru vis-sup-age-999        .
      *              *-------------------------------------------------*
      *              * Nome agente supervisore                         *
      *              *-------------------------------------------------*
           perform   vis-sup-nom-000      thru vis-sup-nom-999        .
      *              *-------------------------------------------------*
      *              * Targa                                           *
      *              *-------------------------------------------------*
           perform   vis-num-trg-000      thru vis-num-trg-999        .
      *              *-------------------------------------------------*
      *              * Dipendenza operativa                            *
      *              *-------------------------------------------------*
           perform   vis-dpz-ope-000      thru vis-dpz-ope-999        .
           perform   vis-dpz-ope-des-000  thru vis-dpz-ope-des-999    .
      *              *-------------------------------------------------*
      *              * Tipo maturazione                                *
      *              *-------------------------------------------------*
           perform   vis-tip-mat-000      thru vis-tip-mat-999        .
      *              *-------------------------------------------------*
      *              * Indicatori per conteggi provvigionali           *
      *              *-------------------------------------------------*
           perform   vis-flg-cpv-000      thru vis-flg-cpv-999        .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     vis-tes-reg-999.
       vis-tes-reg-400.
      *              *-------------------------------------------------*
      *              * Data inizio attivita'                           *
      *              *-------------------------------------------------*
           perform   vis-dat-iat-000      thru vis-dat-iat-999        .
      *              *-------------------------------------------------*
      *              * Status commerciale                              *
      *              *-------------------------------------------------*
           perform   vis-sta-tus-000      thru vis-sta-tus-999        .
      *              *-------------------------------------------------*
      *              * Data determinazione status                      *
      *              *-------------------------------------------------*
           perform   vis-sta-tud-000      thru vis-sta-tud-999        .
      *              *-------------------------------------------------*
      *              * Codice agente di riferimento                    *
      *              *-------------------------------------------------*
           perform   vis-sta-tuc-000      thru vis-sta-tuc-999        .
           perform   vis-sta-tuc-nom-000  thru vis-sta-tuc-nom-999    .
      *              *-------------------------------------------------*
      *              * Modalita' di trattamento statistiche            *
      *              *-------------------------------------------------*
           perform   vis-sta-tux-000      thru vis-sta-tux-999        .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     vis-tes-reg-999.
       vis-tes-reg-500.
      *              *-------------------------------------------------*
      *              * Contropartita vendite                           *
      *              *-------------------------------------------------*
           perform   vis-ctp-ven-000      thru vis-ctp-ven-999        .
           perform   vis-ctp-ven-des-000  thru vis-ctp-ven-des-999    .
      *              *-------------------------------------------------*
      *              * Contropartita resi su vendite                   *
      *              *-------------------------------------------------*
           perform   vis-ctp-rsv-000      thru vis-ctp-rsv-999        .
           perform   vis-ctp-rsv-des-000  thru vis-ctp-rsv-des-999    .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     vis-tes-reg-999.
       vis-tes-reg-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts testata                           *
      *    *-----------------------------------------------------------*
       pmt-tes-reg-000.
      *              *-------------------------------------------------*
      *              * Erase linee impegnate                           *
      *              *-------------------------------------------------*
           move      "EL"                 to   v-ope                  .
           move      06                   to   v-lin                  .
           move      21                   to   v-lto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del numero pagina        *
      *              *-------------------------------------------------*
           go to     pmt-tes-reg-100
                     pmt-tes-reg-200
                     pmt-tes-reg-300
                     pmt-tes-reg-400
                     pmt-tes-reg-500
                     depending            on   w-cnt-sts-imp-npt      .
           go to     pmt-tes-reg-999.
       pmt-tes-reg-100.
      *              *-------------------------------------------------*
      *              * Codice nazione                                  *
      *              *-------------------------------------------------*
           perform   pmt-cod-naz-000      thru pmt-cod-naz-999        .
      *              *-------------------------------------------------*
      *              * Nome dell'agente                                *
      *              *-------------------------------------------------*
           perform   pmt-nom-age-000      thru pmt-nom-age-999        .
      *              *-------------------------------------------------*
      *              * Ragione sociale                                 *
      *              *-------------------------------------------------*
           perform   pmt-rag-age-000      thru pmt-rag-age-999        .
      *              *-------------------------------------------------*
      *              * Indirizzo                                       *
      *              *-------------------------------------------------*
           perform   pmt-via-age-000      thru pmt-via-age-999        .
      *              *-------------------------------------------------*
      *              * C.a.p. e citta'                                 *
      *              *-------------------------------------------------*
           perform   pmt-loc-age-000      thru pmt-loc-age-999        .
      *              *-------------------------------------------------*
      *              * Codice comune                                   *
      *              *-------------------------------------------------*
           perform   pmt-cod-cmn-000      thru pmt-cod-cmn-999        .
      *              *-------------------------------------------------*
      *              * Codice frazione                                 *
      *              *-------------------------------------------------*
           perform   pmt-cod-fzn-000      thru pmt-cod-fzn-999        .
      *              *-------------------------------------------------*
      *              * Codice localita'                                *
      *              *-------------------------------------------------*
           perform   pmt-cod-lct-000      thru pmt-cod-lct-999        .
      *              *-------------------------------------------------*
      *              * Partita iva                                     *
      *              *-------------------------------------------------*
           perform   pmt-prt-iva-000      thru pmt-prt-iva-999        .
      *              *-------------------------------------------------*
      *              * Codice fiscale                                  *
      *              *-------------------------------------------------*
           perform   pmt-cod-fis-000      thru pmt-cod-fis-999        .
      *              *-------------------------------------------------*
      *              * Codice mnemonico                                *
      *              *-------------------------------------------------*
           perform   pmt-cod-mne-000      thru pmt-cod-mne-999        .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     pmt-tes-reg-999.
       pmt-tes-reg-200.
      *              *-------------------------------------------------*
      *              * Contatti e utenze                               *
      *              *-------------------------------------------------*
           perform   pmt-con-arc-000      thru pmt-con-arc-999        .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     pmt-tes-reg-999.
       pmt-tes-reg-300.
      *              *-------------------------------------------------*
      *              * Categoria di provvigioni                        *
      *              *-------------------------------------------------*
           perform   pmt-cat-pvg-000      thru pmt-cat-pvg-999        .
      *              *-------------------------------------------------*
      *              * % di provvigione                                *
      *              *-------------------------------------------------*
           perform   pmt-per-pvg-000      thru pmt-per-pvg-999        .
      *              *-------------------------------------------------*
      *              * Codice agente supervisore                       *
      *              *-------------------------------------------------*
           perform   pmt-sup-age-000      thru pmt-sup-age-999        .
      *              *-------------------------------------------------*
      *              * Targa                                           *
      *              *-------------------------------------------------*
           perform   pmt-num-trg-000      thru pmt-num-trg-999        .
      *              *-------------------------------------------------*
      *              * Dipendenza operativa                            *
      *              *-------------------------------------------------*
           perform   pmt-dpz-ope-000      thru pmt-dpz-ope-999        .
      *              *-------------------------------------------------*
      *              * Tipo maturazione                                *
      *              *-------------------------------------------------*
           perform   pmt-tip-mat-000      thru pmt-tip-mat-999        .
      *              *-------------------------------------------------*
      *              * Indicatori per conteggi provvigionali           *
      *              *-------------------------------------------------*
           perform   pmt-flg-cpv-000      thru pmt-flg-cpv-999        .
           go to     pmt-tes-reg-999.
       pmt-tes-reg-400.
      *              *-------------------------------------------------*
      *              * Data inizio attivita'                           *
      *              *-------------------------------------------------*
           perform   pmt-dat-iat-000      thru pmt-dat-iat-999        .
      *              *-------------------------------------------------*
      *              * Status commerciale                              *
      *              *-------------------------------------------------*
           perform   pmt-sta-tus-000      thru pmt-sta-tus-999        .
      *              *-------------------------------------------------*
      *              * Data determinazione status                      *
      *              *-------------------------------------------------*
           perform   pmt-sta-tud-000      thru pmt-sta-tud-999        .
      *              *-------------------------------------------------*
      *              * Codice cliente di riferimento                   *
      *              *-------------------------------------------------*
           perform   pmt-sta-tuc-000      thru pmt-sta-tuc-999        .
      *              *-------------------------------------------------*
      *              * Modalita' di trattamento statistiche            *
      *              *-------------------------------------------------*
           perform   pmt-sta-tux-000      thru pmt-sta-tux-999        .
           go to     pmt-tes-reg-999.
       pmt-tes-reg-500.
      *              *-------------------------------------------------*
      *              * Contropartita vendite                           *
      *              *-------------------------------------------------*
           perform   pmt-ctp-ven-000      thru pmt-ctp-ven-999        .
      *              *-------------------------------------------------*
      *              * Contropartita resi su vendite                   *
      *              *-------------------------------------------------*
           perform   pmt-ctp-rsv-000      thru pmt-ctp-rsv-999        .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     pmt-tes-reg-999.
       pmt-tes-reg-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Codice nazione                   *
      *    *-----------------------------------------------------------*
       pmt-cod-naz-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice nazione             :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cod-naz-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Nome agente                      *
      *    *-----------------------------------------------------------*
       pmt-nom-age-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Nominativo agente          :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-nom-age-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Ragione sociale                  *
      *    *-----------------------------------------------------------*
       pmt-rag-age-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Ragione sociale            :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-rag-age-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Indirizzo                        *
      *    *-----------------------------------------------------------*
       pmt-via-age-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Indirizzo                  :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-via-age-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : C.a.p. e citta'                  *
      *    *-----------------------------------------------------------*
       pmt-loc-age-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "C.a.p. e citta'            :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-loc-age-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Codice comune                    *
      *    *-----------------------------------------------------------*
       pmt-cod-cmn-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice comune              :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cod-cmn-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Codice frazione                  *
      *    *-----------------------------------------------------------*
       pmt-cod-fzn-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice frazione            :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cod-fzn-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Codice localita'                 *
      *    *-----------------------------------------------------------*
       pmt-cod-lct-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice localita'           :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cod-lct-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Partita iva                      *
      *    *-----------------------------------------------------------*
       pmt-prt-iva-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      01                   to   v-pos                  .
           if        w-tes-cod-naz (1)    =    spaces or
                     w-tes-cod-naz (1)    =    "IT"
                     move  "Partita iva                :"
                                          to   v-alf
           else      move  spaces         to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-prt-iva-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Codice fiscale                   *
      *    *-----------------------------------------------------------*
       pmt-cod-fis-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      01                   to   v-pos                  .
           if        w-tes-cod-naz (1)    =    spaces or
                     w-tes-cod-naz (1)    =    "IT"
                     move  "Codice fiscale             :"
                                          to   v-alf
           else      move  "Codice iva                 :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cod-fis-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Codice mnemonico                 *
      *    *-----------------------------------------------------------*
       pmt-cod-mne-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      21                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice mnemonico           :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cod-mne-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Contatti e utenze                *
      *    *-----------------------------------------------------------*
       pmt-con-arc-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tipo     Interlocutore / Reparto                  
      -              "      Utenza                  "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "----  ------------------------------   -----------
      -              "------------------------------"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-con-arc-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Categoria di provvigioni         *
      *    *-----------------------------------------------------------*
       pmt-cat-pvg-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Categoria di provvigioni   :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cat-pvg-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : % di provvigioni                 *
      *    *-----------------------------------------------------------*
       pmt-per-pvg-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "% di provvigione           :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-per-pvg-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Codice agente supervisore        *
      *    *-----------------------------------------------------------*
       pmt-sup-age-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Agente a cui fa capo       :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-sup-age-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Targa                            *
      *    *-----------------------------------------------------------*
       pmt-num-trg-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Targa veicolo              :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-num-trg-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Dipendenza operativa             *
      *    *-----------------------------------------------------------*
       pmt-dpz-ope-000.
      *              *-------------------------------------------------*
      *              * Test se prompt da visualizzare                  *
      *              *-------------------------------------------------*
           if        w-dpz-ctr-dpz        not  > 1
                     go to pmt-dpz-ope-999.
       pmt-dpz-ope-100.
      *              *-------------------------------------------------*
      *              * Visualizzazione prompt                          *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Dipendenza operativa       :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-dpz-ope-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Tipo maturazione                 *
      *    *-----------------------------------------------------------*
       pmt-tip-mat-000.
      *              *-------------------------------------------------*
      *              * Test se prompt da visualizzare                  *
      *              *-------------------------------------------------*
           if        w-prs-age-300-tma    not  = 03
                     go to pmt-tip-mat-999.
       pmt-tip-mat-100.
      *              *-------------------------------------------------*
      *              * Visualizzazione prompt                          *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tipo maturazione conteggi  :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "            provvigionali   "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-tip-mat-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Indicatori per conteggi prov-    *
      *    *                          vigionali                        *
      *    *-----------------------------------------------------------*
       pmt-flg-cpv-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Indicatori per conteggi    :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      21                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "          provvigionali     "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-flg-cpv-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Data inizio attivita'            *
      *    *-----------------------------------------------------------*
       pmt-dat-iat-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "In attivita' dal           :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-dat-iat-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Status commerciale               *
      *    *-----------------------------------------------------------*
       pmt-sta-tus-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Status commerciale         :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-sta-tus-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Data determinazione status       *
      *    *-----------------------------------------------------------*
       pmt-sta-tud-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Rilevazione status al      :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-sta-tud-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Codice agente di riferimento     *
      *    *-----------------------------------------------------------*
       pmt-sta-tuc-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Agente di riferimento      :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-sta-tuc-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Modalita' di trattamento statis- *
      *    *                          tiche                            *
      *    *-----------------------------------------------------------*
       pmt-sta-tux-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Trattamento statistiche    :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-sta-tux-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Contropartita vendite            *
      *    *-----------------------------------------------------------*
       pmt-ctp-ven-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Contropartita vendite      :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-ctp-ven-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Contropartita resi su vendite    *
      *    *-----------------------------------------------------------*
       pmt-ctp-rsv-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Contropartita per resi su  :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "                  vendite   "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-ctp-rsv-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Presa visione per pagina     *
      *    *-----------------------------------------------------------*
       acc-pre-vpg-000.
      *              *-------------------------------------------------*
      *              * Se non esiste alcuna pagina attiva successiva   *
      *              * alla pagina attuale : uscita                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Salvataggio numero pagina attuale           *
      *                  *---------------------------------------------*
           move      w-cnt-sts-imp-npt    to   w-cnt-sts-imp-svp      .
       acc-pre-vpg-100.
      *                  *---------------------------------------------*
      *                  * Se all'ultima pagina : ripristino pagina    *
      *                  * salvata ed uscita                           *
      *                  *---------------------------------------------*
           if        w-cnt-sts-imp-npt    not  < w-cnt-sts-imp-mpt
                     move  w-cnt-sts-imp-svp
                                          to   w-cnt-sts-imp-npt
                     go to acc-pre-vpg-999.
      *                  *---------------------------------------------*
      *                  * Incremento numero pagina                    *
      *                  *---------------------------------------------*
           add       1                    to   w-cnt-sts-imp-npt      .
      *                  *---------------------------------------------*
      *                  * Test se pagina da trattare                  *
      *                  *---------------------------------------------*
           perform   snp-tes-reg-000      thru snp-tes-reg-999        .
      *                  *---------------------------------------------*
      *                  * Se no : a re-incremento                     *
      *                  *---------------------------------------------*
           if        w-cnt-sts-imp-snp    not  = spaces
                     go to acc-pre-vpg-100.
      *                  *---------------------------------------------*
      *                  * Se si : ripristino pagina salvata e accet-  *
      *                  * tazione presa visione                       *
      *                  *---------------------------------------------*
           move      w-cnt-sts-imp-svp    to   w-cnt-sts-imp-npt      .
       acc-pre-vpg-200.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "MX"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      spaces               to   v-msk                  .
           move      spaces               to   v-alf                  .
           move      spaces               to   v-not                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-tes    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-pre-vpg-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-pre-vpg-999.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-pre-vpg-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-pre-vpg-000.
       acc-pre-vpg-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Codice nazione               *
      *    *-----------------------------------------------------------*
       acc-cod-naz-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione eventuale default              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se valore gia' diverso da spaces non si *
      *                      * prepara alcun default                   *
      *                      *-----------------------------------------*
           if        w-tes-cod-naz (1)    not  = spaces
                     go to acc-cod-naz-100.
      *                      *-----------------------------------------*
      *                      * Preparazione del valore di default      *
      *                      *-----------------------------------------*
           move      "IT "                to   w-tes-cod-naz (1)      .
      *                      *-----------------------------------------*
      *                      * Preparazione della descrizione relativa *
      *                      * al valore di default                    *
      *                      *-----------------------------------------*
           move      w-tes-cod-naz (1)    to   w-let-arc-gxn-cod      .
           perform   let-arc-gxn-000      thru let-arc-gxn-999        .
           move      w-let-arc-gxn-des    to   w-tes-cod-naz-des (1)  .
      *                      *-----------------------------------------*
      *                      * Visualizzazione valore di default       *
      *                      *-----------------------------------------*
           perform   vis-cod-naz-000      thru vis-cod-naz-999        .
      *                      *-----------------------------------------*
      *                      * Visualizzazione descrizione relativa al *
      *                      * valore di default                       *
      *                      *-----------------------------------------*
           perform   vis-des-naz-000      thru vis-des-naz-999        .
       acc-cod-naz-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-des-naz-ope      .
           move      w-tes-cod-naz (1)    to   w-cod-des-naz-cod      .
           move      06                   to   w-cod-des-naz-lin      .
           move      30                   to   w-cod-des-naz-pos      .
           move      06                   to   w-cod-des-naz-dln      .
           move      36                   to   w-cod-des-naz-dps      .
           move      spaces               to   v-edm                  .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-des-naz-cll-000  thru cod-des-naz-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-des-naz-foi-000  thru cod-des-naz-foi-999    .
       acc-cod-naz-110.
           perform   cod-des-naz-cll-000  thru cod-des-naz-cll-999    .
           if        w-cod-des-naz-ope    =    "F+"
                     go to acc-cod-naz-115.
           if        w-cod-des-naz-ope    =    "AC"
                     go to acc-cod-naz-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-naz-115.
           perform   cod-des-naz-foi-000  thru cod-des-naz-foi-999    .
           go to     acc-cod-naz-110.
       acc-cod-naz-120.
           move      w-cod-des-naz-cod    to   v-alf                  .
       acc-cod-naz-150.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-cod-naz-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-cod-naz-999.
       acc-cod-naz-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-cod-naz (1)      .
       acc-cod-naz-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura tabella                             *
      *                  *---------------------------------------------*
           move      w-tes-cod-naz (1)    to   w-let-arc-gxn-cod      .
           perform   let-arc-gxn-000      thru let-arc-gxn-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione descrizione                  *
      *                  *---------------------------------------------*
           move      w-let-arc-gxn-des    to   w-tes-cod-naz-des (1)  .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione                 *
      *                  *---------------------------------------------*
           perform   vis-des-naz-000      thru vis-des-naz-999        .
      *                  *---------------------------------------------*
      *                  * Se codice errato : reimpostazione           *
      *                  *---------------------------------------------*
           if        w-let-arc-gxn-flg    not  = spaces
                     go to acc-cod-naz-100.
      *                  *---------------------------------------------*
      *                  * Se a spaces : reimpostazione                *
      *                  *---------------------------------------------*
           if        w-tes-cod-naz (1)    =    spaces
                     go to acc-cod-naz-100.
       acc-cod-naz-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda se Italia o Estero     *
      *                  *---------------------------------------------*
           if        w-tes-cod-naz (1)    =    "IT "
                     go to acc-cod-naz-620
           else      go to acc-cod-naz-640.
       acc-cod-naz-620.
      *                  *---------------------------------------------*
      *                  * Se Italia                                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Visualizzazione prompt per :            *
      *                      * - Partita iva                           *
      *                      *-----------------------------------------*
           perform   pmt-prt-iva-000      thru pmt-prt-iva-999        .
      *                      *-----------------------------------------*
      *                      * Visualizzazione prompt per :            *
      *                      * - Codice fiscale                        *
      *                      *-----------------------------------------*
           perform   pmt-cod-fis-000      thru pmt-cod-fis-999        .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     acc-cod-naz-800.
       acc-cod-naz-640.
      *                  *---------------------------------------------*
      *                  * Se Estero                                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazione e visualizzazione, se   *
      *                      * necessario di :                         *
      *                      * - Comune                                *
      *                      * - Frazione                              *
      *                      * - Localita'                             *
      *                      *-----------------------------------------*
       acc-cod-naz-642.
      *                          *-------------------------------------*
      *                          * Comune                              *
      *                          *-------------------------------------*
           if        w-tes-cod-cmn (1)    =    zero
                     go to acc-cod-naz-644.
           move      zero                 to   w-tes-cod-cmn (1)      .
           move      spaces               to   w-tes-cod-cmn-des (1)  .
           move      spaces               to   w-tes-cod-cmn-prv (1)  .
           perform   vis-cod-cmn-000      thru vis-cod-cmn-999        .
           perform   vis-des-cmn-000      thru vis-des-cmn-999        .
           perform   vis-cod-prv-000      thru vis-cod-prv-999        .
       acc-cod-naz-644.
      *                          *-------------------------------------*
      *                          * Frazione                            *
      *                          *-------------------------------------*
           if        w-tes-cod-fzn (1)    =    zero
                     go to acc-cod-naz-646.
           move      zero                 to   w-tes-cod-fzn (1)      .
           move      spaces               to   w-tes-cod-fzn-des (1)  .
           perform   vis-cod-fzn-000      thru vis-cod-fzn-999        .
           perform   vis-des-fzn-000      thru vis-des-fzn-999        .
       acc-cod-naz-646.
      *                          *-------------------------------------*
      *                          * Localita'                           *
      *                          *-------------------------------------*
           if        w-tes-cod-lct (1)    =    zero
                     go to acc-cod-naz-648.
           move      zero                 to   w-tes-cod-lct (1)      .
           move      spaces               to   w-tes-cod-lct-des (1)  .
           perform   vis-cod-lct-000      thru vis-cod-lct-999        .
           perform   vis-des-lct-000      thru vis-des-lct-999        .
       acc-cod-naz-648.
      *                      *-----------------------------------------*
      *                      * Normalizzazione e visualizzazione, se   *
      *                      * necessario di :                         *
      *                      * - Partita iva                           *
      *                      *-----------------------------------------*
           if        w-tes-prt-iva (1)    =    zero
                     go to acc-cod-naz-650.
           move      zero                 to   w-tes-prt-iva (1)      .
           perform   vis-prt-iva-000      thru vis-prt-iva-999        .
       acc-cod-naz-650.
      *                      *-----------------------------------------*
      *                      * Visualizzazione prompt per :            *
      *                      * - Partita iva                           *
      *                      *-----------------------------------------*
           perform   pmt-prt-iva-000      thru pmt-prt-iva-999        .
      *                      *-----------------------------------------*
      *                      * Visualizzazione prompt per :            *
      *                      * - Codice fiscale                        *
      *                      *-----------------------------------------*
           perform   pmt-cod-fis-000      thru pmt-cod-fis-999        .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     acc-cod-naz-800.
       acc-cod-naz-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-cod-naz-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-cod-naz-100.
       acc-cod-naz-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Codice nazione            *
      *    *-----------------------------------------------------------*
       vis-cod-naz-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-cod-naz (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-naz-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Descrizione nazione       *
      *    *-----------------------------------------------------------*
       vis-des-naz-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      36                   to   v-pos                  .
           move      w-tes-cod-naz-des (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-des-naz-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Nome agente                  *
      *    *-----------------------------------------------------------*
       acc-nom-age-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-nom-age-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-tes-nom-age (1)    to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-nom-age-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-nom-age-999.
       acc-nom-age-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-nom-age (1)      .
       acc-nom-age-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore a spaces : reimpostazione, a meno *
      *                  * che non sia su tasto Up                     *
      *                  *---------------------------------------------*
           if        w-tes-nom-age (1)    not  = spaces
                     go to acc-nom-age-450.
           if        v-key                =    "UP  "
                     go to acc-nom-age-600
           else      go to acc-nom-age-100.
       acc-nom-age-450.
      *                  *---------------------------------------------*
      *                  * Se valore a non spaces il primo carattere   *
      *                  * non deve essere a spaces                    *
      *                  *---------------------------------------------*
           if        w-tes-nom-age (1)
                    (01 : 01)             =    spaces
                     go to acc-nom-age-100.
       acc-nom-age-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione nome agente in uppercase       *
      *                  *---------------------------------------------*
           move      w-tes-nom-age (1)    to   w-all-str-alf          .
           move      20                   to   w-all-str-lun          .
           perform   all-str-upp-000      thru all-str-upp-999        .
           move      w-all-str-alf        to   w-tes-nom-key (1)      .
       acc-nom-age-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-nom-age-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-nom-age-100.
       acc-nom-age-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Nome agente               *
      *    *-----------------------------------------------------------*
       vis-nom-age-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-nom-age (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-nom-age-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Ragione sociale              *
      *    *-----------------------------------------------------------*
       acc-rag-soc-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Eventuale default da nominativo agente      *
      *                  *---------------------------------------------*
           if        w-tes-rag-soc (1)    =    spaces
                     move  w-tes-nom-age (1)
                                          to   w-tes-rag-soc (1)      .
       acc-rag-soc-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-tes-rag-soc (1)    to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-rag-soc-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-rag-soc-999.
       acc-rag-soc-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-rag-soc (1)      .
       acc-rag-soc-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore a spaces : reimpostazione, a meno *
      *                  * che non sia su tasto Up                     *
      *                  *---------------------------------------------*
           if        w-tes-rag-soc (1)    not  = spaces
                     go to acc-rag-soc-450.
           if        v-key                =    "UP  "
                     go to acc-rag-soc-600
           else      go to acc-rag-soc-100.
       acc-rag-soc-450.
      *                  *---------------------------------------------*
      *                  * Se valore a non spaces il primo carattere   *
      *                  * non deve essere a spaces                    *
      *                  *---------------------------------------------*
           if        w-tes-rag-soc (1)
                    (01 : 01)             =    spaces
                     go to acc-rag-soc-100.
       acc-rag-soc-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione ragione sociale in uppercase   *
      *                  *---------------------------------------------*
           move      w-tes-rag-soc (1)    to   w-all-str-alf          .
           move      40                   to   w-all-str-lun          .
           perform   all-str-upp-000      thru all-str-upp-999        .
           move      w-all-str-alf        to   w-tes-rag-key (1)      .
       acc-rag-soc-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-rag-soc-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-rag-soc-100.
       acc-rag-soc-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Ragione sociale           *
      *    *-----------------------------------------------------------*
       vis-rag-soc-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-rag-soc (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-rag-soc-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Indirizzo                    *
      *    *-----------------------------------------------------------*
       acc-via-age-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-via-age-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-ind-cec-geo-ope      .
           move      w-tes-cod-naz (1)    to   w-ind-cec-geo-naz      .
           move      w-tes-via-age (1)    to   w-ind-cec-geo-ind      .
           move      11                   to   w-ind-cec-geo-lin      .
           move      30                   to   w-ind-cec-geo-pos      .
           move      w-tes-loc-age (1)    to   w-ind-cec-geo-cec      .
           move      w-tes-cod-cmn (1)    to   w-ind-cec-geo-cmn      .
           move      w-tes-cod-cmn-des (1)
                                          to   w-ind-cec-geo-dco      .
           move      w-tes-cod-cmn-prv (1)
                                          to   w-ind-cec-geo-prv      .
           move      w-tes-cod-fzn (1)    to   w-ind-cec-geo-fzn      .
           move      w-tes-cod-fzn-des (1)
                                          to   w-ind-cec-geo-dfr      .
           move      w-tes-cod-lct (1)    to   w-ind-cec-geo-lct      .
           move      w-tes-cod-lct-des (1)
                                          to   w-ind-cec-geo-dlo      .
           move      spaces               to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   ind-cec-geo-cll-000  thru ind-cec-geo-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   ind-cec-geo-foi-000  thru ind-cec-geo-foi-999    .
       acc-via-age-110.
           perform   ind-cec-geo-cll-000  thru ind-cec-geo-cll-999    .
           if        w-ind-cec-geo-ope    =    "F+"
                     go to acc-via-age-115.
           if        w-ind-cec-geo-ope    =    "AC"
                     go to acc-via-age-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-via-age-115.
           perform   ind-cec-geo-foi-000  thru ind-cec-geo-foi-999    .
           go to     acc-via-age-110.
       acc-via-age-120.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-via-age-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-via-age-999.
       acc-via-age-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      w-ind-cec-geo-ind    to   w-tes-via-age (1)      .
      *              *-------------------------------------------------*
      *              * Altri valori se automatismo eseguito            *
      *              *-------------------------------------------------*
           if        w-ind-cec-geo-aut    =    spaces
                     go to acc-via-age-400.
      *                  *---------------------------------------------*
      *                  * C.a.p. e citta'                             *
      *                  *---------------------------------------------*
           move      w-ind-cec-geo-cec    to   w-tes-loc-age (1)      .
           perform   vis-loc-age-000      thru vis-loc-age-999        .
      *                  *---------------------------------------------*
      *                  * Comune                                      *
      *                  *---------------------------------------------*
           move      w-ind-cec-geo-cmn    to   w-tes-cod-cmn (1)      .
           move      w-ind-cec-geo-dco    to   w-tes-cod-cmn-des (1)  .
           move      w-ind-cec-geo-prv    to   w-tes-cod-cmn-prv (1)  .
           perform   vis-cod-cmn-000      thru vis-cod-cmn-999        .
           perform   vis-des-cmn-000      thru vis-des-cmn-999        .
           perform   vis-cod-prv-000      thru vis-cod-prv-999        .
      *                  *---------------------------------------------*
      *                  * Frazione                                    *
      *                  *---------------------------------------------*
           move      w-ind-cec-geo-fzn    to   w-tes-cod-fzn (1)      .
           move      w-ind-cec-geo-dfr    to   w-tes-cod-fzn-des (1)  .
           perform   vis-cod-fzn-000      thru vis-cod-fzn-999        .
           perform   vis-des-fzn-000      thru vis-des-fzn-999        .
      *                  *---------------------------------------------*
      *                  * Localita'                                   *
      *                  *---------------------------------------------*
           move      w-ind-cec-geo-lct    to   w-tes-cod-lct (1)      .
           move      w-ind-cec-geo-dlo    to   w-tes-cod-lct-des (1)  .
           perform   vis-cod-lct-000      thru vis-cod-lct-999        .
           perform   vis-des-lct-000      thru vis-des-lct-999        .
       acc-via-age-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore a non spaces il primo carattere   *
      *                  * non deve essere a spaces                    *
      *                  *---------------------------------------------*
           if        w-tes-via-age (1)    =    spaces
                     go to acc-via-age-600.
      *
           if        w-tes-via-age (1)
                    (01 : 01)             =    spaces
                     go to acc-via-age-100.
       acc-via-age-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-via-age-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-via-age-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-via-age-100.
       acc-via-age-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Indirizzo                 *
      *    *-----------------------------------------------------------*
       vis-via-age-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-via-age (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-via-age-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : C.a.p. e citta'              *
      *    *-----------------------------------------------------------*
       acc-loc-age-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-loc-age-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cap-cit-geo-ope      .
           move      w-tes-cod-naz (1)    to   w-cap-cit-geo-naz      .
           move      w-tes-loc-age (1)    to   w-cap-cit-geo-cec      .
           move      12                   to   w-cap-cit-geo-lin      .
           move      30                   to   w-cap-cit-geo-pos      .
           move      w-tes-cod-cmn (1)    to   w-cap-cit-geo-cmn      .
           move      w-tes-cod-cmn-des (1)
                                          to   w-cap-cit-geo-dco      .
           move      w-tes-cod-cmn-prv (1)
                                          to   w-cap-cit-geo-prv      .
           move      w-tes-cod-fzn (1)    to   w-cap-cit-geo-fzn      .
           move      w-tes-cod-fzn-des (1)
                                          to   w-cap-cit-geo-dfr      .
           move      w-tes-cod-lct (1)    to   w-cap-cit-geo-lct      .
           move      w-tes-cod-lct-des (1)
                                          to   w-cap-cit-geo-dlo      .
           move      spaces               to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cap-cit-geo-cll-000  thru cap-cit-geo-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cap-cit-geo-foi-000  thru cap-cit-geo-foi-999    .
       acc-loc-age-110.
           perform   cap-cit-geo-cll-000  thru cap-cit-geo-cll-999    .
           if        w-cap-cit-geo-ope    =    "F+"
                     go to acc-loc-age-115.
           if        w-cap-cit-geo-ope    =    "AC"
                     go to acc-loc-age-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-loc-age-115.
           perform   cap-cit-geo-foi-000  thru cap-cit-geo-foi-999    .
           go to     acc-loc-age-110.
       acc-loc-age-120.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-loc-age-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-loc-age-999.
       acc-loc-age-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      w-cap-cit-geo-cec    to   w-tes-loc-age (1)      .
      *              *-------------------------------------------------*
      *              * Altri valori se automatismo eseguito            *
      *              *-------------------------------------------------*
           if        w-cap-cit-geo-aut    =    spaces
                     go to acc-loc-age-400.
       acc-loc-age-300.
      *                  *---------------------------------------------*
      *                  * Indirizzo, solo se l'elemento selezionato   *
      *                  * e' una localita'                            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se non si tratta di una localita' non   *
      *                      * si esegue alcun automatismo sull'indi-  *
      *                      * rizzo                                   *
      *                      *-----------------------------------------*
           if        w-cap-cit-geo-lct    =    zero
                     go to acc-loc-age-350.
      *                      *-----------------------------------------*
      *                      * Concatenamento della localita' selezio- *
      *                      * nata con l'indirizzo precedente         *
      *                      *-----------------------------------------*
           move      "IL"                 to   w-ind-cec-geo-ope      .
           move      w-tes-via-age (1)    to   w-ind-cec-geo-ind      .
           move      w-cap-cit-geo-dlo    to   w-ind-cec-geo-dlo      .
           perform   ind-cec-geo-cll-000  thru ind-cec-geo-cll-999    .
           move      w-ind-cec-geo-ind    to   w-tes-via-age (1)      .
      *                      *-----------------------------------------*
      *                      * Visualizzazione indirizzo concatenato   *
      *                      *-----------------------------------------*
           perform   vis-via-age-000      thru vis-via-age-999        .
       acc-loc-age-350.
      *                  *---------------------------------------------*
      *                  * Comune                                      *
      *                  *---------------------------------------------*
           move      w-cap-cit-geo-cmn    to   w-tes-cod-cmn (1)      .
           move      w-cap-cit-geo-dco    to   w-tes-cod-cmn-des (1)  .
           move      w-cap-cit-geo-prv    to   w-tes-cod-cmn-prv (1)  .
           perform   vis-cod-cmn-000      thru vis-cod-cmn-999        .
           perform   vis-des-cmn-000      thru vis-des-cmn-999        .
           perform   vis-cod-prv-000      thru vis-cod-prv-999        .
      *                  *---------------------------------------------*
      *                  * Frazione                                    *
      *                  *---------------------------------------------*
           move      w-cap-cit-geo-fzn    to   w-tes-cod-fzn (1)      .
           move      w-cap-cit-geo-dfr    to   w-tes-cod-fzn-des (1)  .
           perform   vis-cod-fzn-000      thru vis-cod-fzn-999        .
           perform   vis-des-fzn-000      thru vis-des-fzn-999        .
      *                  *---------------------------------------------*
      *                  * Localita'                                   *
      *                  *---------------------------------------------*
           move      w-cap-cit-geo-lct    to   w-tes-cod-lct (1)      .
           move      w-cap-cit-geo-dlo    to   w-tes-cod-lct-des (1)  .
           perform   vis-cod-lct-000      thru vis-cod-lct-999        .
           perform   vis-des-lct-000      thru vis-des-lct-999        .
       acc-loc-age-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore a non spaces il primo carattere   *
      *                  * non deve essere a spaces                    *
      *                  *---------------------------------------------*
           if        w-tes-loc-age (1)    =    spaces
                     go to acc-loc-age-600.
      *
           if        w-tes-loc-age (1)
                    (01 : 01)             =    spaces
                     go to acc-loc-age-100.
       acc-loc-age-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-loc-age-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-loc-age-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-loc-age-100.
       acc-loc-age-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : C.a.p. e citta'           *
      *    *-----------------------------------------------------------*
       vis-loc-age-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-loc-age (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-loc-age-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Codice comune                *
      *    *-----------------------------------------------------------*
       acc-cod-cmn-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cod-cmn-025.
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-tes-cod-naz (1)    not  = "IT "
                     go to acc-cod-cmn-999.
       acc-cod-cmn-050.
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      w-tes-cod-cmn (1)    to   w-sav-cod-cmn          .
       acc-cod-cmn-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-com-geo-ope      .
           move      w-tes-cod-cmn (1)    to   w-cod-com-geo-cmn      .
           move      14                   to   w-cod-com-geo-lin      .
           move      30                   to   w-cod-com-geo-pos      .
           move      14                   to   w-cod-com-geo-dln      .
           move      36                   to   w-cod-com-geo-dps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-com-geo-cll-000  thru cod-com-geo-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-com-geo-foi-000  thru cod-com-geo-foi-999    .
       acc-cod-cmn-110.
           perform   cod-com-geo-cll-000  thru cod-com-geo-cll-999    .
           if        w-cod-com-geo-ope    =    "F+"
                     go to acc-cod-cmn-115.
           if        w-cod-com-geo-ope    =    "AC"
                     go to acc-cod-cmn-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-cmn-115.
           perform   cod-com-geo-foi-000  thru cod-com-geo-foi-999    .
           go to     acc-cod-cmn-110.
       acc-cod-cmn-120.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-cod-cmn-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-cod-cmn-999.
       acc-cod-cmn-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      w-cod-com-geo-cmn    to   w-tes-cod-cmn (1)      .
       acc-cod-cmn-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura di controllo                        *
      *                  *---------------------------------------------*
           move      "C"                  to   w-let-arc-gxc-tip      .
           move      w-tes-cod-cmn (1)    to   w-let-arc-gxc-cmn      .
           move      zero                 to   w-let-arc-gxc-fzn      .
           move      zero                 to   w-let-arc-gxc-lct      .
           perform   let-arc-gxc-000      thru let-arc-gxc-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione dati letti                   *
      *                  *---------------------------------------------*
           move      w-let-arc-gxc-des    to   w-tes-cod-cmn-des (1)  .
           move      w-let-arc-gxc-prv    to   w-tes-cod-cmn-prv (1)  .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione                 *
      *                  *---------------------------------------------*
           perform   vis-des-cmn-000      thru vis-des-cmn-999        .
      *                  *---------------------------------------------*
      *                  * Visualizzazione provincia                   *
      *                  *---------------------------------------------*
           perform   vis-cod-prv-000      thru vis-cod-prv-999        .
      *                  *---------------------------------------------*
      *                  * Se codice errato : reimpostazione           *
      *                  *---------------------------------------------*
           if        w-let-arc-gxc-flg    not  = spaces
                     go to acc-cod-cmn-100.
       acc-cod-cmn-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se valore cambiato oppure no           *
      *                  *---------------------------------------------*
           if        w-tes-cod-cmn (1)    =    w-sav-cod-cmn
                     go to acc-cod-cmn-625
           else      go to acc-cod-cmn-650.
       acc-cod-cmn-625.
      *                  *---------------------------------------------*
      *                  * Se valore inalterato                        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     acc-cod-cmn-800.
       acc-cod-cmn-650.
      *                  *---------------------------------------------*
      *                  * Se valore cambiato rispetto al precedente   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazione e visualizzazione, se   *
      *                      * necessario di :                         *
      *                      * - Frazione                              *
      *                      * - Localita'                             *
      *                      *-----------------------------------------*
       acc-cod-cmn-652.
      *                          *-------------------------------------*
      *                          * Frazione                            *
      *                          *-------------------------------------*
           if        w-tes-cod-fzn (1)    =    zero
                     go to acc-cod-cmn-654.
           move      zero                 to   w-tes-cod-fzn (1)      .
           move      spaces               to   w-tes-cod-fzn-des (1)  .
           perform   vis-cod-fzn-000      thru vis-cod-fzn-999        .
           perform   vis-des-fzn-000      thru vis-des-fzn-999        .
       acc-cod-cmn-654.
      *                          *-------------------------------------*
      *                          * Localita'                           *
      *                          *-------------------------------------*
           if        w-tes-cod-lct (1)    =    zero
                     go to acc-cod-cmn-656.
           move      zero                 to   w-tes-cod-lct (1)      .
           move      spaces               to   w-tes-cod-lct-des (1)  .
           perform   vis-cod-lct-000      thru vis-cod-lct-999        .
           perform   vis-des-lct-000      thru vis-des-lct-999        .
       acc-cod-cmn-656.
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     acc-cod-cmn-800.
       acc-cod-cmn-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-cod-cmn-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-cod-cmn-100.
       acc-cod-cmn-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Codice Comune             *
      *    *-----------------------------------------------------------*
       vis-cod-cmn-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      14                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-cod-cmn (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-cmn-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Descrizione Comune        *
      *    *-----------------------------------------------------------*
       vis-des-cmn-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      36                   to   v-pos                  .
           move      w-tes-cod-cmn-des (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-des-cmn-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Codice provincia          *
      *    *-----------------------------------------------------------*
       vis-cod-prv-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      69                   to   v-pos                  .
           move      w-tes-cod-cmn-prv (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-prv-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Codice frazione              *
      *    *-----------------------------------------------------------*
       acc-cod-fzn-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cod-fzn-025.
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-tes-cod-naz (1)    not  = "IT "
                     go to acc-cod-fzn-999.
           if        w-tes-cod-cmn (1)    =    zero
                     go to acc-cod-fzn-999.
       acc-cod-fzn-050.
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      w-tes-cod-fzn (1)    to   w-sav-cod-fzn          .
       acc-cod-fzn-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-fra-geo-ope      .
           move      w-tes-cod-cmn (1)    to   w-cod-fra-geo-cmn      .
           move      w-tes-cod-fzn (1)    to   w-cod-fra-geo-fzn      .
           move      15                   to   w-cod-fra-geo-lin      .
           move      30                   to   w-cod-fra-geo-pos      .
           move      15                   to   w-cod-fra-geo-dln      .
           move      36                   to   w-cod-fra-geo-dps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-fra-geo-cll-000  thru cod-fra-geo-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-fra-geo-foi-000  thru cod-fra-geo-foi-999    .
       acc-cod-fzn-110.
           perform   cod-fra-geo-cll-000  thru cod-fra-geo-cll-999    .
           if        w-cod-fra-geo-ope    =    "F+"
                     go to acc-cod-fzn-115.
           if        w-cod-fra-geo-ope    =    "AC"
                     go to acc-cod-fzn-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-fzn-115.
           perform   cod-fra-geo-foi-000  thru cod-fra-geo-foi-999    .
           go to     acc-cod-fzn-110.
       acc-cod-fzn-120.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-cod-fzn-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-cod-fzn-999.
       acc-cod-fzn-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      w-cod-fra-geo-fzn    to   w-tes-cod-fzn (1)      .
       acc-cod-fzn-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura di controllo                        *
      *                  *---------------------------------------------*
           move      "F"                  to   w-let-arc-gxc-tip      .
           move      w-tes-cod-cmn (1)    to   w-let-arc-gxc-cmn      .
           move      w-tes-cod-fzn (1)    to   w-let-arc-gxc-fzn      .
           move      zero                 to   w-let-arc-gxc-lct      .
           perform   let-arc-gxc-000      thru let-arc-gxc-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione dati letti                   *
      *                  *---------------------------------------------*
           move      w-let-arc-gxc-des    to   w-tes-cod-fzn-des (1)  .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione                 *
      *                  *---------------------------------------------*
           perform   vis-des-fzn-000      thru vis-des-fzn-999        .
      *                  *---------------------------------------------*
      *                  * Se codice errato : reimpostazione           *
      *                  *---------------------------------------------*
           if        w-let-arc-gxc-flg    not  = spaces
                     go to acc-cod-fzn-100.
       acc-cod-fzn-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se valore cambiato oppure no           *
      *                  *---------------------------------------------*
           if        w-tes-cod-fzn (1)    =    w-sav-cod-fzn
                     go to acc-cod-fzn-625
           else      go to acc-cod-fzn-650.
       acc-cod-fzn-625.
      *                  *---------------------------------------------*
      *                  * Se valore inalterato                        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     acc-cod-fzn-800.
       acc-cod-fzn-650.
      *                  *---------------------------------------------*
      *                  * Se valore cambiato rispetto al precedente   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazione e visualizzazione, se   *
      *                      * necessario di :                         *
      *                      * - Localita'                             *
      *                      *-----------------------------------------*
       acc-cod-fzn-652.
      *                          *-------------------------------------*
      *                          * Localita'                           *
      *                          *-------------------------------------*
           if        w-tes-cod-lct (1)    =    zero
                     go to acc-cod-fzn-654.
           move      zero                 to   w-tes-cod-lct (1)      .
           move      spaces               to   w-tes-cod-lct-des (1)  .
           perform   vis-cod-lct-000      thru vis-cod-lct-999        .
           perform   vis-des-lct-000      thru vis-des-lct-999        .
       acc-cod-fzn-654.
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     acc-cod-fzn-800.
       acc-cod-fzn-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-cod-fzn-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-cod-fzn-100.
       acc-cod-fzn-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Codice frazione           *
      *    *-----------------------------------------------------------*
       vis-cod-fzn-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      15                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-cod-fzn (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-fzn-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Descrizione frazione      *
      *    *-----------------------------------------------------------*
       vis-des-fzn-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      36                   to   v-pos                  .
           move      w-tes-cod-fzn-des (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-des-fzn-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Codice localita'             *
      *    *-----------------------------------------------------------*
       acc-cod-lct-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cod-lct-025.
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-tes-cod-naz (1)    not  = "IT "
                     go to acc-cod-lct-999.
           if        w-tes-cod-cmn (1)    =    zero
                     go to acc-cod-lct-999.
       acc-cod-lct-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-loc-geo-ope      .
           move      w-tes-cod-cmn (1)    to   w-cod-loc-geo-cmn      .
           move      w-tes-cod-fzn (1)    to   w-cod-loc-geo-fzn      .
           move      w-tes-cod-lct (1)    to   w-cod-loc-geo-lct      .
           move      16                   to   w-cod-loc-geo-lin      .
           move      30                   to   w-cod-loc-geo-pos      .
           move      16                   to   w-cod-loc-geo-dln      .
           move      36                   to   w-cod-loc-geo-dps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-loc-geo-cll-000  thru cod-loc-geo-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-loc-geo-foi-000  thru cod-loc-geo-foi-999    .
       acc-cod-lct-110.
           perform   cod-loc-geo-cll-000  thru cod-loc-geo-cll-999    .
           if        w-cod-loc-geo-ope    =    "F+"
                     go to acc-cod-lct-115.
           if        w-cod-loc-geo-ope    =    "AC"
                     go to acc-cod-lct-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-lct-115.
           perform   cod-loc-geo-foi-000  thru cod-loc-geo-foi-999    .
           go to     acc-cod-lct-110.
       acc-cod-lct-120.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-cod-lct-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-cod-lct-999.
       acc-cod-lct-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      w-cod-loc-geo-lct    to   w-tes-cod-lct (1)      .
       acc-cod-lct-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura di controllo                        *
      *                  *---------------------------------------------*
           move      "L"                  to   w-let-arc-gxc-tip      .
           move      w-tes-cod-cmn (1)    to   w-let-arc-gxc-cmn      .
           move      w-tes-cod-fzn (1)    to   w-let-arc-gxc-fzn      .
           move      w-tes-cod-lct (1)    to   w-let-arc-gxc-lct      .
           perform   let-arc-gxc-000      thru let-arc-gxc-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione dati letti                   *
      *                  *---------------------------------------------*
           move      w-let-arc-gxc-des    to   w-tes-cod-lct-des (1)  .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione                 *
      *                  *---------------------------------------------*
           perform   vis-des-lct-000      thru vis-des-lct-999        .
      *                  *---------------------------------------------*
      *                  * Se codice errato : reimpostazione           *
      *                  *---------------------------------------------*
           if        w-let-arc-gxc-flg    not  = spaces
                     go to acc-cod-lct-100.
       acc-cod-lct-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cod-lct-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-cod-lct-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-cod-lct-100.
       acc-cod-lct-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Codice localita'          *
      *    *-----------------------------------------------------------*
       vis-cod-lct-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      16                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-cod-lct (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-lct-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Descrizione localita'     *
      *    *-----------------------------------------------------------*
       vis-des-lct-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      36                   to   v-pos                  .
           move      w-tes-cod-lct-des (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-des-lct-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Partita iva                  *
      *    *-----------------------------------------------------------*
       acc-prt-iva-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-tes-cod-naz (1)    not  = "IT "
                     go to acc-prt-iva-999.
      *                  *---------------------------------------------*
      *                  * Salvataggio partita iva e codice fiscale    *
      *                  *---------------------------------------------*
           move      w-tes-prt-iva (1)    to   w-sav-prt-iva          .
           move      w-tes-cod-fis (1)    to   w-sav-cod-fis          .
      *                  *---------------------------------------------*
      *                  * Partita iva in rappresentazione alfanumeri- *
      *                  * ca                                          *
      *                  *---------------------------------------------*
           move      w-tes-prt-iva (1)    to   w-ctl-prt-iva-num      .
           if        w-ctl-prt-iva-num    =    zero
                     move  spaces         to   w-ctl-prt-iva-alf      .
       acc-prt-iva-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-ctl-prt-iva-alf    to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-prt-iva-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-prt-iva-999.
       acc-prt-iva-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione con   *
      *              * rappresentazione alfanumerica                   *
      *              *-------------------------------------------------*
           move      v-alf                to   w-ctl-prt-iva-alf      .
       acc-prt-iva-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore a spaces : forzatura di zero      *
      *                  *---------------------------------------------*
           if        w-ctl-prt-iva-alf    =    spaces
                     move  zero           to   w-ctl-prt-iva-num      .
      *                  *---------------------------------------------*
      *                  * Se valore non-numerico : reimpostazione     *
      *                  *---------------------------------------------*
           if        w-ctl-prt-iva-num    not  numeric
                     go to acc-prt-iva-100.
      *                  *---------------------------------------------*
      *                  * Valore impostato in campo definitivo        *
      *                  *---------------------------------------------*
           move      w-ctl-prt-iva-num    to   w-tes-prt-iva (1)      .
      *                  *---------------------------------------------*
      *                  * Se la partita iva e' a zero non si esegue   *
      *                  * il controllo                                *
      *                  *---------------------------------------------*
           if        w-tes-prt-iva (1)    =    "000000000000"
                     go to acc-prt-iva-600.
      *                  *---------------------------------------------*
      *                  * Se la partita iva e' pari al valore prece-  *
      *                  * dente non si esegue il controllo            *
      *                  *---------------------------------------------*
           if        w-tes-prt-iva (1)    =    w-sav-prt-iva
                     go to acc-prt-iva-600.
      *                  *---------------------------------------------*
      *                  * Controllo formale sulla partita iva         *
      *                  *---------------------------------------------*
           move      w-tes-prt-iva (1)    to   w-ctl-prt-iva-piv      .
           perform   ctl-prt-iva-000      thru ctl-prt-iva-999        .
      *                  *---------------------------------------------*
      *                  * Se controllo formale superato si prosegue   *
      *                  *---------------------------------------------*
           if        w-ctl-prt-iva-flg    =    spaces
                     go to  acc-prt-iva-600.
       acc-prt-iva-420.
      *                  *---------------------------------------------*
      *                  * Se controllo formale non superato           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Salvataggio immagine video              *
      *                      *-----------------------------------------*
           move      "SV"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Video in Off                            *
      *                      *-----------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Box                                     *
      *                      *-----------------------------------------*
           move      "BX"                 to   v-ope                  .
           move      08                   to   v-lin                  .
           move      07                   to   v-pos                  .
           move      17                   to   v-lto                  .
           move      74                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Literals nel box                        *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      64                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      09                   to   v-pos                  .
           move      "Attenzione :  La partita iva risulta formalmente s
      -              "corretta.     "     to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      12                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      09                   to   v-pos                  .
           move      "Scelta     :"       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Video in On                             *
      *                      *-----------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-prt-iva-425.
      *                      *-----------------------------------------*
      *                      * Accettazione risposta                   *
      *                      *-----------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-err-piv-lun    to   v-car                  .
           move      w-exp-err-piv-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      13                   to   v-lin                  .
           move      23                   to   v-pos                  .
           move      w-exp-err-piv-tbl    to   v-txt                  .
           move      zero                 to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-num                to   w-exp-err-piv-sce      .
      *                      *-----------------------------------------*
      *                      * Controllo risposta                      *
      *                      *-----------------------------------------*
           if        w-exp-err-piv-sce    not  = 01 and
                     w-exp-err-piv-sce    not  = 02
                     go to acc-prt-iva-425.
      *                      *-----------------------------------------*
      *                      * Ripristino immagine video               *
      *                      *-----------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Deviazione a seconda della risposta     *
      *                      *-----------------------------------------*
           if        w-exp-err-piv-sce    =    01
                     go to acc-prt-iva-100.
       acc-prt-iva-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore variato rispetto al valore prece- *
      *                  * dente si ricalcola automaticamente, se pos- *
      *                  * sibile, il codice fiscale in funzione della *
      *                  * partita iva                                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se avvenuta variazione             *
      *                      *-----------------------------------------*
           if        w-tes-prt-iva (1)    =    w-sav-prt-iva
                     go to acc-prt-iva-612.
      *                      *-----------------------------------------*
      *                      * Se il codice fiscale attuale e' a spa-  *
      *                      * ces si esegue comunque l'automatismo    *
      *                      *-----------------------------------------*
           if        w-tes-cod-fis (1)    =    spaces
                     go to acc-prt-iva-603.
      *                      *-----------------------------------------*
      *                      * Se precedentemente alla variazione il   *
      *                      * codice fiscale era pari alla partita    *
      *                      * iva, si esegue l'automatismo            *
      *                      *-----------------------------------------*
           move      spaces               to   w-piv-cfi-cfi-alf      .
           move      w-sav-prt-iva        to   w-piv-cfi-cfi-11n      .
           if        w-piv-cfi-cfi-alf    =    w-sav-cod-fis
                     go to acc-prt-iva-603.
      *                      *-----------------------------------------*
      *                      * Altrimenti l'automatismo non viene ese- *
      *                      * guito                                   *
      *                      *-----------------------------------------*
           go to     acc-prt-iva-612.
       acc-prt-iva-603.
      *                      *-----------------------------------------*
      *                      * Esecuzione automatismo                  *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Codice fiscale attuale pari alla    *
      *                          * partita iva attuale                 *
      *                          *-------------------------------------*
           move      spaces               to   w-piv-cfi-cfi-alf      .
           if        w-tes-prt-iva (1)    =    zero
                     go to acc-prt-iva-606.
           move      w-tes-prt-iva (1)    to   w-piv-cfi-cfi-11n      .
       acc-prt-iva-606.
           move      w-piv-cfi-cfi-alf    to   w-tes-cod-fis (1)      .
       acc-prt-iva-609.
      *                          *-------------------------------------*
      *                          * Visualizzazione codice fiscale      *
      *                          *-------------------------------------*
           perform   vis-cod-fis-000      thru vis-cod-fis-999        .
       acc-prt-iva-612.
           go to     acc-prt-iva-800.
       acc-prt-iva-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-prt-iva-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-prt-iva-100.
       acc-prt-iva-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Partita iva               *
      *    *-----------------------------------------------------------*
       vis-prt-iva-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9B"                 to   v-edm                  .
           move      18                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-prt-iva (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-prt-iva-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Codice fiscale                       *
      *    *-----------------------------------------------------------*
       acc-cod-fis-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Salvataggio codice fiscale                  *
      *                  *---------------------------------------------*
           move      w-tes-cod-fis (1)    to   w-sav-cod-fis          .
       acc-cod-fis-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      16                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-tes-cod-fis (1)    to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-cod-fis-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-cod-fis-999.
      *              *-------------------------------------------------*
      *              * Valore impostato                                *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-cod-fis (1)      .
       acc-cod-fis-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se l'agente e' estero non si esegue il con- *
      *                  * trollo                                      *
      *                  *---------------------------------------------*
           if        w-tes-cod-naz (1)    not  = "IT "
                     go to acc-cod-fis-415.
      *                  *---------------------------------------------*
      *                  * Se il codice fiscale e' completamente a     *
      *                  * spaces non si esegue il controllo           *
      *                  *---------------------------------------------*
           if        w-tes-cod-fis (1)    =    spaces
                     go to acc-cod-fis-415.
      *                  *---------------------------------------------*
      *                  * Se il codice fiscale e' pari al valore pre- *
      *                  * cedente non si esegue il controllo          *
      *                  *---------------------------------------------*
           if        w-tes-cod-fis (1)    =    w-sav-cod-fis
                     go to acc-cod-fis-415.
      *                  *---------------------------------------------*
      *                  * Se il codice fiscale e' pari alla partita   *
      *                  * iva non si esegue il controllo              *
      *                  *---------------------------------------------*
           move      w-tes-cod-fis (1)    to   w-piv-cfi-cfi-alf      .
           if        w-piv-cfi-cfi-11n    =    w-tes-prt-iva (1) and
                     w-piv-cfi-cfi-05a    =    spaces
                     go to acc-cod-fis-415.
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione della lunghezza ef-  *
      *                  * fettiva del valore                          *
      *                  *---------------------------------------------*
           if        w-piv-cfi-cfi-05a    not  = spaces
                     go to acc-cod-fis-406.
       acc-cod-fis-403.
      *                  *---------------------------------------------*
      *                  * Se la lunghezza effettiva del valore non    *
      *                  * supera gli 11 caratteri                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se il valore non e' numerico : errore   *
      *                      *-----------------------------------------*
           if        w-piv-cfi-cfi-11n    not  numeric
                     go to acc-cod-fis-409.
      *                      *-----------------------------------------*
      *                      * Controllo formale come partita iva      *
      *                      *-----------------------------------------*
           move      w-piv-cfi-cfi-11n    to   w-ctl-prt-iva-piv      .
           perform   ctl-prt-iva-000      thru ctl-prt-iva-999        .
      *                      *-----------------------------------------*
      *                      * Se controllo formale superato si prose- *
      *                      * gue, altrimenti si emette la segnala-   *
      *                      * zione di errore                         *
      *                      *-----------------------------------------*
           if        w-ctl-prt-iva-flg    =    spaces
                     go to  acc-cod-fis-415
           else      go to  acc-cod-fis-409.
       acc-cod-fis-406.
      *                  *---------------------------------------------*
      *                  * Se la lunghezza effettiva del valore supera *
      *                  * gli 11 caratteri                            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Controllo formale come codice fiscale   *
      *                      *-----------------------------------------*
           move      w-tes-cod-fis (1)    to   w-ctl-cod-fis-cof      .
           perform   ctl-cod-fis-000      thru ctl-cod-fis-999        .
      *                      *-----------------------------------------*
      *                      * Se controllo formale superato si prose- *
      *                      * gue, altrimenti si emette la segnala-   *
      *                      * zione di errore                         *
      *                      *-----------------------------------------*
           if        w-ctl-cod-fis-flg    =    spaces
                     go to  acc-cod-fis-415
           else      go to  acc-cod-fis-409.
       acc-cod-fis-409.
      *                  *---------------------------------------------*
      *                  * Se controllo formale non superato           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Salvataggio immagine video              *
      *                      *-----------------------------------------*
           move      "SV"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Video in Off                            *
      *                      *-----------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Box                                     *
      *                      *-----------------------------------------*
           move      "BX"                 to   v-ope                  .
           move      08                   to   v-lin                  .
           move      07                   to   v-pos                  .
           move      17                   to   v-lto                  .
           move      74                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Literals nel box                        *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      64                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      09                   to   v-pos                  .
           move      "Attenzione :  Il codice fiscale risulta formalment
      -              "e scorretto.  "     to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      12                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      09                   to   v-pos                  .
           move      "Scelta     :"       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Video in On                             *
      *                      *-----------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-cod-fis-412.
      *                      *-----------------------------------------*
      *                      * Accettazione risposta                   *
      *                      *-----------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-err-cfi-lun    to   v-car                  .
           move      w-exp-err-cfi-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      13                   to   v-lin                  .
           move      23                   to   v-pos                  .
           move      w-exp-err-cfi-tbl    to   v-txt                  .
           move      zero                 to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-num                to   w-exp-err-cfi-sce      .
      *                      *-----------------------------------------*
      *                      * Controllo risposta                      *
      *                      *-----------------------------------------*
           if        w-exp-err-cfi-sce    not  = 01 and
                     w-exp-err-cfi-sce    not  = 02
                     go to acc-cod-fis-412.
      *                      *-----------------------------------------*
      *                      * Ripristino immagine video               *
      *                      *-----------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Deviazione a seconda della risposta     *
      *                      *-----------------------------------------*
           if        w-exp-err-cfi-sce    =    01
                     go to acc-cod-fis-100
           else      go to acc-cod-fis-415.
       acc-cod-fis-415.
      *                  *---------------------------------------------*
      *                  * Prosecuzione dopo controlli                 *
      *                  *---------------------------------------------*
           go to     acc-cod-fis-600.
       acc-cod-fis-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cod-fis-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-cod-fis-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-cod-fis-100.
       acc-cod-fis-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Codice fiscale                    *
      *    *-----------------------------------------------------------*
       vis-cod-fis-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      16                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-cod-fis (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-fis-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Codice mnemonico             *
      *    *-----------------------------------------------------------*
       acc-cod-mne-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cod-mne-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      10                   to   v-car                  .
           move      21                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-tes-cod-mne (1)    to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-cod-mne-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-cod-mne-999.
       acc-cod-mne-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-cod-mne (1)      .
       acc-cod-mne-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test che non ci siano blanks embedded       *
      *                  *---------------------------------------------*
           move      w-tes-cod-mne (1)    to   w-all-str-alf          .
           move      10                   to   w-all-str-lun          .
           perform   all-str-ble-000      thru all-str-ble-999        .
           if        w-all-str-flg        not  = spaces
                     go to acc-cod-mne-100.
       acc-cod-mne-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cod-mne-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-cod-mne-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-cod-mne-100.
       acc-cod-mne-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Codice mnemonico          *
      *    *-----------------------------------------------------------*
       vis-cod-mne-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      10                   to   v-car                  .
           move      21                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-cod-mne (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-mne-999.
           exit.

      *    *===========================================================*
      *    * Accettazione e visualizzazione : Contatti e utenze        *
      *    *                                                           *
      *    * Subroutine in copy di acc-con-arc-000 e vis-con-arc-000   *
      *    *-----------------------------------------------------------*
           copy      "pgm/azi/prg/cpy/aconarc0.acs"                   .

      *    *===========================================================*
      *    * Accettazione campo testata : Categoria provvigioni        *
      *    *-----------------------------------------------------------*
       acc-cat-pvg-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cat-pvg-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cmn-zpv-001-ope      .
           move      w-tes-cat-pvg (1)    to   w-cmn-zpv-001-cod      .
           move      07                   to   w-cmn-zpv-001-lin      .
           move      30                   to   w-cmn-zpv-001-pos      .
           move      07                   to   w-cmn-zpv-001-dln      .
           move      41                   to   w-cmn-zpv-001-dps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cmn-zpv-001-cll-000  thru cmn-zpv-001-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cmn-zpv-001-foi-000  thru cmn-zpv-001-foi-999    .
       acc-cat-pvg-110.
           perform   cmn-zpv-001-cll-000  thru cmn-zpv-001-cll-999    .
           if        w-cmn-zpv-001-ope    =    "F+"
                     go to acc-cat-pvg-115.
           if        w-cmn-zpv-001-ope    =    "AC"
                     go to acc-cat-pvg-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cat-pvg-115.
           perform   cmn-zpv-001-foi-000  thru cmn-zpv-001-foi-999    .
           go to     acc-cat-pvg-110.
       acc-cat-pvg-120.
           move      w-cmn-zpv-001-cod    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-cat-pvg-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-cat-pvg-999.
       acc-cat-pvg-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-cat-pvg (1)      .
       acc-cat-pvg-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura tabella                             *
      *                  *---------------------------------------------*
           move      01                   to   w-let-arc-zpv-tip      .
           move      w-tes-cat-pvg (1)    to   w-let-arc-zpv-cod      .
           perform   let-arc-zpv-000      thru let-arc-zpv-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione descrizione                  *
      *                  *---------------------------------------------*
           move      w-let-arc-zpv-des    to   w-tes-cat-pvg-des (1)  .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione                 *
      *                  *---------------------------------------------*
           perform   vis-des-pvg-000      thru vis-des-pvg-999        .
      *                  *---------------------------------------------*
      *                  * Se codice errato : reimpostazione           *
      *                  *---------------------------------------------*
           if        w-let-arc-zpv-flg    not  = spaces
                     go to acc-cat-pvg-100.
       acc-cat-pvg-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cat-pvg-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-cat-pvg-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-cat-pvg-100.
       acc-cat-pvg-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Categoria provvigioni     *
      *    *-----------------------------------------------------------*
       vis-cat-pvg-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      07                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-cat-pvg (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cat-pvg-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Descrizione categoria     *
      *    *-----------------------------------------------------------*
       vis-des-pvg-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      w-tes-cat-pvg-des (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-des-pvg-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Percentuale di provvigione   *
      *    *                              legata all'agente            *
      *    *-----------------------------------------------------------*
       acc-per-pvg-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-per-pvg-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      01                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BD"                to   v-edm                  .
           move      08                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-tes-per-pvg (1, 1) to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-per-pvg-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-per-pvg-999.
       acc-per-pvg-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-per-pvg (1, 1)   .
       acc-per-pvg-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-per-pvg-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-per-pvg-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-per-pvg-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-per-pvg-100.
       acc-per-pvg-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Percentuale provvigione   *
      *    *-----------------------------------------------------------*
       vis-per-pvg-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      01                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BD"                to   v-edm                  .
           move      08                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-per-pvg (1, 1) to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-per-pvg-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Codice agente supervisore            *
      *    *-----------------------------------------------------------*
       acc-sup-age-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Salvataggio del valore precedente           *
      *                  *---------------------------------------------*
           move      w-tes-sup-age (1)    to   w-sav-sup-age          .
       acc-sup-age-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "K"                  to   v-tip                  .
           move      10                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-sup-age-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-sup-age-999.
       acc-sup-age-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
       acc-sup-age-300.
      *              *-------------------------------------------------*
      *              * Se Insr                                         *
      *              *-------------------------------------------------*
           if        v-key                not  = "INSR"
                     go to acc-sup-age-400.
      *                  *---------------------------------------------*
      *                  * Routine di inserimento del super-agente     *
      *                  *---------------------------------------------*
           move      w-tes-cod-age        to   w-ins-arc-ags-age      .
           perform   ins-arc-ags-000      thru ins-arc-ags-999        .
      *                  *---------------------------------------------*
      *                  * Lettura legame tra agente e super-agente    *
      *                  * per controllo se il valore attuale e' anco- *
      *                  * ra quello valido                            *
      *                  *---------------------------------------------*
           move      w-tes-cod-age        to   w-let-arc-ags-age      .
           perform   let-arc-ags-000      thru let-arc-ags-999        .
           if        w-let-arc-ags-sup    =    w-sav-sup-age
                     go to acc-sup-age-400.
       acc-sup-age-320.
      *                  *---------------------------------------------*
      *                  * Se la lettura del legame ha trovato un      *
      *                  * nuovo valore : lettura e visualizzazione    *
      *                  * del nuovo super-agente                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Aggiornamento con il nuovo valore       *
      *                      *-----------------------------------------*
           move      w-let-arc-ags-sup    to   w-tes-sup-age (1)      .
      *                      *-----------------------------------------*
      *                      * Lettura archivio [age]                  *
      *                      *-----------------------------------------*
           move      w-tes-sup-age (1)    to   w-let-arc-age-cod      .
           perform   let-arc-age-000      thru let-arc-age-999        .
      *                      *-----------------------------------------*
      *                      * Memorizzazione nome agente              *
      *                      *-----------------------------------------*
           move      w-let-arc-age-nom    to   w-tes-sup-age-nom (1)  .
      *                      *-----------------------------------------*
      *                      * Visualizzazione codice agente           *
      *                      *-----------------------------------------*
           perform   vis-sup-age-000      thru vis-sup-age-999        .
      *                      *-----------------------------------------*
      *                      * Visualizzazione nome agente             *
      *                      *-----------------------------------------*
           perform   vis-sup-nom-000      thru vis-sup-nom-999        .
       acc-sup-age-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-sup-age-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-sup-age-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-sup-age-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-sup-age-100.
       acc-sup-age-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Codice agente supervisore         *
      *    *-----------------------------------------------------------*
       vis-sup-age-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      10                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "<B"                 to   v-edm                  .
           move      w-tes-sup-age (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-sup-age-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Nome agente supervisore           *
      *    *-----------------------------------------------------------*
       vis-sup-nom-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      w-tes-sup-age-nom (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-sup-nom-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Targa                        *
      *    *-----------------------------------------------------------*
       acc-num-trg-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-num-trg-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      15                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-tes-num-trg (1)    to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-num-trg-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-num-trg-999.
       acc-num-trg-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-num-trg (1)      .
       acc-num-trg-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-num-trg-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-num-trg-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-num-trg-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-num-trg-100.
       acc-num-trg-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Targa                     *
      *    *-----------------------------------------------------------*
       vis-num-trg-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-num-trg (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-num-trg-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Codice dipendenza                          *
      *    *-----------------------------------------------------------*
       acc-dpz-ope-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-dpz-ctr-dpz        not  > 1
                     go to acc-dpz-ope-999.
       acc-dpz-ope-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-cod-dpz-ope      .
           move      w-tes-dpz-op1 (1)    to   w-cod-cod-dpz-cod      .
           move      15                   to   w-cod-cod-dpz-lin      .
           move      30                   to   w-cod-cod-dpz-pos      .
           move      15                   to   w-cod-cod-dpz-dln      .
           move      41                   to   w-cod-cod-dpz-dps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-cod-dpz-cll-000  thru cod-cod-dpz-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-cod-dpz-foi-000  thru cod-cod-dpz-foi-999    .
       acc-dpz-ope-110.
           perform   cod-cod-dpz-cll-000  thru cod-cod-dpz-cll-999    .
           if        w-cod-cod-dpz-ope    =    "F+"
                     go to acc-dpz-ope-115.
           if        w-cod-cod-dpz-ope    =    "AC"
                     go to acc-dpz-ope-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-dpz-ope-115.
           perform   cod-cod-dpz-foi-000  thru cod-cod-dpz-foi-999    .
           go to     acc-dpz-ope-110.
       acc-dpz-ope-120.
           move      w-cod-cod-dpz-cod    to   v-num                  .
       acc-dpz-ope-150.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-dpz-ope-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-dpz-ope-999.
       acc-dpz-ope-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-dpz-op1 (1)      .
       acc-dpz-ope-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-dpz-ope-410.
      *                  *---------------------------------------------*
      *                  * Lettura archivio [ada]                      *
      *                  *---------------------------------------------*
           move      w-tes-dpz-op1 (1)    to   w-let-arc-dpz-cod      .
           perform   let-arc-dpz-000      thru let-arc-dpz-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione descrizione                  *
      *                  *---------------------------------------------*
           move      w-let-arc-dpz-des    to   w-tes-dpz-op1-des (1)  .
       acc-dpz-ope-420.
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione                 *
      *                  *---------------------------------------------*
           perform   vis-dpz-ope-des-000  thru vis-dpz-ope-des-999    .
       acc-dpz-ope-430.
      *                  *---------------------------------------------*
      *                  * Se lettura errata : reimpostazione          *
      *                  *---------------------------------------------*
           if        w-let-arc-dpz-flg    not  = spaces
                     go to acc-dpz-ope-100.
       acc-dpz-ope-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-dpz-ope-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-dpz-ope-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-dpz-ope-100.
       acc-dpz-ope-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Dipendenza operativa                    *
      *    *-----------------------------------------------------------*
       vis-dpz-ope-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      15                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-dpz-op1 (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dpz-ope-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Dipendenza operativa, descrizione       *
      *    *-----------------------------------------------------------*
       vis-dpz-ope-des-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      w-tes-dpz-op1-des (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dpz-ope-des-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Tipo maturazione             *
      *    *-----------------------------------------------------------*
       acc-tip-mat-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-prs-age-300-tma    not  = 03
                     go to acc-tip-mat-999.
       acc-tip-mat-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-mat-lun    to   v-car                  .
           move      w-exp-tip-mat-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      17                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-tip-mat-tbl    to   v-txt                  .
           if        w-tes-tip-mat (1)    =    "I"
                     move  01             to   v-num
           else if   w-tes-tip-mat (1)    =    "P"
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-tip-mat-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-tip-mat-999.
       acc-tip-mat-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        v-num                =    01
                     move  "I"            to   w-tes-tip-mat (1)
           else if   v-num                =    02
                     move  "P"            to   w-tes-tip-mat (1)
           else      move  spaces         to   w-tes-tip-mat (1)      .
       acc-tip-mat-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se a zero : reimpostazione                  *
      *                  *---------------------------------------------*
           if        v-num                not  = zero
                     go to acc-tip-mat-600.
           if        v-key                not  = "UP  "
                     go to acc-tip-mat-100.
           if        w-cnt-sts-imp-tes    =    spaces
                     go to acc-tip-mat-600
           else      go to acc-tip-mat-100.
       acc-tip-mat-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-tip-mat-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-tip-mat-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-tip-mat-100.
       acc-tip-mat-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Tipo maturazione          *
      *    *-----------------------------------------------------------*
       vis-tip-mat-000.
      *              *-------------------------------------------------*
      *              * Test se campo da visualizzare                   *
      *              *-------------------------------------------------*
           if        w-prs-age-300-tma    not  = 03
                     go to vis-tip-mat-999.
       vis-tip-mat-100.
      *              *-------------------------------------------------*
      *              * Visualizzazione campo                           *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-mat-lun    to   v-car                  .
           move      w-exp-tip-mat-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      17                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-tip-mat-tbl    to   v-txt                  .
           if        w-tes-tip-mat (1)    =    "I"
                     move  01             to   v-num
           else if   w-tes-tip-mat (1)    =    "P"
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tip-mat-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Indicatori per conteggi      *
      *    *                              provvigionali                *
      *    *-----------------------------------------------------------*
       acc-flg-cpv-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-flg-cpv-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      09                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-tes-flg-cpv (1)    to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-flg-cpv-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-flg-cpv-999.
       acc-flg-cpv-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-flg-cpv (1)      .
       acc-flg-cpv-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-flg-cpv-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-flg-cpv-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-flg-cpv-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-flg-cpv-100.
       acc-flg-cpv-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Indicatori per conteggi   *
      *    *                                 provvigionali             *
      *    *-----------------------------------------------------------*
       vis-flg-cpv-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      09                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-flg-cpv (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-flg-cpv-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Data inizio attivita'                *
      *    *-----------------------------------------------------------*
       acc-dat-iat-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-dat-iat-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      07                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "PRSC"               to   v-pfk (07)             .
           move      "NXSC"               to   v-pfk (08)             .
           move      w-tes-dat-iat (1)    to   v-dat                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-dat-iat-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-dat-iat-999.
       acc-dat-iat-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-dat                to   w-tes-dat-iat (1)      .
       acc-dat-iat-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-dat-iat-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-dat-iat-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-dat-iat-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-dat-iat-100.
       acc-dat-iat-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Data inizio attivita'             *
      *    *-----------------------------------------------------------*
       vis-dat-iat-000.
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      07                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-dat-iat (1)    to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dat-iat-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Status commerciale           *
      *    *-----------------------------------------------------------*
       acc-sta-tus-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      w-tes-sta-tus (1)    to   w-sav-sta-tus          .
       acc-sta-tus-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-sta-tus-lun    to   v-car                  .
           move      w-exp-sta-tus-num    to   v-ldt                  .
           move      spaces               to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      09                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-sta-tus-tbl    to   v-txt                  .
      *
           if        w-tes-sta-tus (1)    =    01
                     move  01             to   v-num
           else if   w-tes-sta-tus (1)    =    11
                     move  02             to   v-num
           else if   w-tes-sta-tus (1)    =    21
                     move  03             to   v-num
           else if   w-tes-sta-tus (1)    =    51
                     move  04             to   v-num
           else if   w-tes-sta-tus (1)    =    52
                     move  05             to   v-num
           else if   w-tes-sta-tus (1)    =    61
                     move  06             to   v-num
           else if   w-tes-sta-tus (1)    =    62
                     move  07             to   v-num
           else if   w-tes-sta-tus (1)    =    71
                     move  08             to   v-num
           else if   w-tes-sta-tus (1)    =    72
                     move  09             to   v-num
           else      move  zero           to   v-num                  .
      *
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "PRSC"               to   v-pfk (07)             .
           move      "NXSC"               to   v-pfk (08)             .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-sta-tus-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-sta-tus-999.
       acc-sta-tus-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        v-num                =    01
                     move  01             to   w-tes-sta-tus (1)
           else if   v-num                =    02
                     move  11             to   w-tes-sta-tus (1)
           else if   v-num                =    03
                     move  21             to   w-tes-sta-tus (1)
           else if   v-num                =    04
                     move  51             to   w-tes-sta-tus (1)
           else if   v-num                =    05
                     move  52             to   w-tes-sta-tus (1)
           else if   v-num                =    06
                     move  61             to   w-tes-sta-tus (1)
           else if   v-num                =    07
                     move  62             to   w-tes-sta-tus (1)
           else if   v-num                =    08
                     move  71             to   w-tes-sta-tus (1)
           else if   v-num                =    09
                     move  72             to   w-tes-sta-tus (1)
           else      move zero            to   w-tes-sta-tus (1)      .
       acc-sta-tus-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se zero : reimpostazione, a meno che non si *
      *                  * sia in Up, e che il valore precedente non   *
      *                  * fosse gia' a zero                           *
      *                  *---------------------------------------------*
           if        w-tes-sta-tus (1)    not  = zero
                     go to acc-sta-tus-600.
           if        v-key                not  = "UP  "
                     go to acc-sta-tus-100.
           if        w-sav-sta-tus        =    zero
                     go to acc-sta-tus-600
           else      go to acc-sta-tus-100.
       acc-sta-tus-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Eventuale normalizzazione fornitore di ri-  *
      *                  * ferimento e modalita' di trattamento per le *
      *                  * statistiche                                 *
      *                  *---------------------------------------------*
           if        w-tes-sta-tuc (1)    =    zero
                     go to acc-sta-tus-650.
           if        w-tes-sta-tus (1)    =    w-sav-sta-tus
                     go to acc-sta-tus-650.
           if        w-tes-sta-tus (1)    =    21  or
                     w-tes-sta-tus (1)    =    52  or
                     w-tes-sta-tus (1)    =    62  or
                     w-tes-sta-tus (1)    =    72
                     go to acc-sta-tus-650.
      *                  *---------------------------------------------*
      *                  * Normalizzazione agente di riferimento       *
      *                  *---------------------------------------------*
           move      zero                 to   w-tes-sta-tuc (1)      .
           move      spaces               to   w-tes-sta-tuc-nom (1)  .
      *                  *---------------------------------------------*
      *                  * Normalizzazione modalita' di trattamento    *
      *                  * per le statistiche                          *
      *                  *---------------------------------------------*
           move      zero                 to   w-tes-sta-tux (1)      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione agente di riferimento       *
      *                  *---------------------------------------------*
           perform   vis-sta-tuc-000      thru vis-sta-tuc-999        .
           perform   vis-sta-tuc-nom-000  thru vis-sta-tuc-nom-999    .
      *                  *---------------------------------------------*
      *                  * Visualizzazione modalita' di trattamento    *
      *                  * per le statistiche                          *
      *                  *---------------------------------------------*
           perform   vis-sta-tux-000      thru vis-sta-tux-999        .
       acc-sta-tus-650.
      *                  *---------------------------------------------*
      *                  * Eventuale normalizzazione data di rileva-   *
      *                  * mento status                                *
      *                  *---------------------------------------------*
           if        w-tes-sta-tud (1)    =    zero
                     go to acc-sta-tus-800.
           if        w-tes-sta-tus (1)    =    w-sav-sta-tus
                     go to acc-sta-tus-800.
           if        w-tes-sta-tus (1)    not  = 01
                     go to acc-sta-tus-800.
      *                  *---------------------------------------------*
      *                  * Normalizzazione data di rilevamento status  *
      *                  *---------------------------------------------*
           move      zero                 to   w-tes-sta-tud (1)      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione data di rilevamento status  *
      *                  *---------------------------------------------*
           perform   vis-sta-tud-000      thru vis-sta-tud-999        .
       acc-sta-tus-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-sta-tus-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-sta-tus-100.
       acc-sta-tus-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Status commerciale        *
      *    *-----------------------------------------------------------*
       vis-sta-tus-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-sta-tus-lun    to   v-car                  .
           move      w-exp-sta-tus-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      09                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-sta-tus-tbl    to   v-txt                  .
      *
           if        w-tes-sta-tus (1)    =    01
                     move  01             to   v-num
           else if   w-tes-sta-tus (1)    =    11
                     move  02             to   v-num
           else if   w-tes-sta-tus (1)    =    21
                     move  03             to   v-num
           else if   w-tes-sta-tus (1)    =    51
                     move  04             to   v-num
           else if   w-tes-sta-tus (1)    =    52
                     move  05             to   v-num
           else if   w-tes-sta-tus (1)    =    61
                     move  06             to   v-num
           else if   w-tes-sta-tus (1)    =    62
                     move  07             to   v-num
           else if   w-tes-sta-tus (1)    =    71
                     move  08             to   v-num
           else if   w-tes-sta-tus (1)    =    72
                     move  09             to   v-num
           else      move  zero           to   v-num                  .
      *
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-sta-tus-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Data determinazione status   *
      *    *-----------------------------------------------------------*
       acc-sta-tud-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *                                             *
      *                  * Solo se status diverso da 01                *
      *                  *---------------------------------------------*
           if        w-tes-sta-tus (1)    =    01
                     go to acc-sta-tud-999.
      *                  *---------------------------------------------*
      *                  * Preparazione eventuale default              *
      *                  *---------------------------------------------*
           if        w-tes-sta-tud (1)    not  = zero
                     go to acc-sta-tud-100.
      *                  *---------------------------------------------*
      *                  * System date da segreteria                   *
      *                  *---------------------------------------------*
           move      "DT"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-dat                to   w-tes-sta-tud (1)      .
       acc-sta-tud-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      11                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "PRSC"               to   v-pfk (07)             .
           move      "NXSC"               to   v-pfk (08)             .
           move      w-tes-sta-tud (1)    to   v-dat                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-sta-tud-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-sta-tud-999.
       acc-sta-tud-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-dat                to   w-tes-sta-tud (1)      .
       acc-sta-tud-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-sta-tud-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-sta-tud-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-sta-tud-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-sta-tud-100.
       acc-sta-tud-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Data determinazione sta-  *
      *    *                                 tus                       *
      *    *-----------------------------------------------------------*
       vis-sta-tud-000.
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      11                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-sta-tud (1)    to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-sta-tud-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Codice agente di riferimento         *
      *    *-----------------------------------------------------------*
       acc-sta-tuc-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *                                             *
      *                  * Solo se status a 21,52,62,72                *
      *                  *---------------------------------------------*
           if        w-tes-sta-tus (1)    not  = 21  and
                     w-tes-sta-tus (1)    not  = 52  and
                     w-tes-sta-tus (1)    not  = 62  and
                     w-tes-sta-tus (1)    not  = 72
                     go to acc-sta-tuc-999.
       acc-sta-tuc-100.
      *              *-------------------------------------------------*
      *              * Salvataggio valore precedente                   *
      *              *-------------------------------------------------*
           move      w-tes-sta-tuc (1)    to   w-sav-sta-tuc          .
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-age-ope      .
           move      w-tes-sta-tuc (1)    to   w-cod-mne-age-cod      .
           move      13                   to   w-cod-mne-age-lin      .
           move      30                   to   w-cod-mne-age-pos      .
           move      13                   to   w-cod-mne-age-nln      .
           move      41                   to   w-cod-mne-age-nps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "PRSC"               to   v-pfk (07)             .
           move      "NXSC"               to   v-pfk (08)             .
           perform   cod-mne-age-cll-000  thru cod-mne-age-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-mne-age-foi-000  thru cod-mne-age-foi-999    .
       acc-sta-tuc-110.
           perform   cod-mne-age-cll-000  thru cod-mne-age-cll-999    .
           if        w-cod-mne-age-ope    =    "F+"
                     go to acc-sta-tuc-115.
           if        w-cod-mne-age-ope    =    "AC"
                     go to acc-sta-tuc-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-sta-tuc-115.
           perform   cod-mne-age-foi-000  thru cod-mne-age-foi-999    .
           go to     acc-sta-tuc-110.
       acc-sta-tuc-120.
           move      w-cod-mne-age-cod    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-sta-tuc-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-sta-tuc-999.
       acc-sta-tuc-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-sta-tuc (1)      .
       acc-sta-tuc-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-sta-tuc-410.
      *                  *---------------------------------------------*
      *                  * Lettura file [age]                          *
      *                  *---------------------------------------------*
           move      w-tes-sta-tuc (1)    to   w-let-arc-age-cod      .
           perform   let-arc-age-000      thru let-arc-age-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione nominativo                   *
      *                  *---------------------------------------------*
           move      w-let-arc-age-nom    to   w-tes-sta-tuc-nom (1)  .
       acc-sta-tuc-420.
      *                  *---------------------------------------------*
      *                  * Visualizzazione nominativo                  *
      *                  *---------------------------------------------*
           perform   vis-sta-tuc-nom-000  thru vis-sta-tuc-nom-999    .
       acc-sta-tuc-430.
      *                  *---------------------------------------------*
      *                  * Se agente non esistente                     *
      *                  *---------------------------------------------*
           if        w-let-arc-age-flg    =    spaces
                     go to acc-sta-tuc-440.
       acc-sta-tuc-434.
      *                      *-----------------------------------------*
      *                      * A reimpostazione                        *
      *                      *-----------------------------------------*
           go to     acc-sta-tuc-100.
       acc-sta-tuc-440.
      *                      *-----------------------------------------*
      *                      * Controllo che il codice agente imposta- *
      *                      * to non sia pari a quello di testata     *
      *                      *-----------------------------------------*
           if        w-tes-sta-tuc (1)    =    w-tes-cod-age
                     go to acc-sta-tuc-100.
       acc-sta-tuc-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Eventuale normalizzazione modalita' di      *
      *                  * trattamento per le statistiche              *
      *                  *---------------------------------------------*
           if        w-tes-sta-tuc (1)    not  = zero
                     go to acc-sta-tuc-800.
           if        w-tes-sta-tuc (1)    =    w-sav-sta-tuc
                     go to acc-sta-tuc-800.
           if        w-tes-sta-tux (1)    =    zero
                     go to acc-sta-tuc-800.
      *                  *---------------------------------------------*
      *                  * Normalizzazione modalita' di trattamento    *
      *                  * per le statistiche                          *
      *                  *---------------------------------------------*
           move      zero                 to   w-tes-sta-tux (1)      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione modalita' di trattamento    *
      *                  * per le statistiche                          *
      *                  *---------------------------------------------*
           perform   vis-sta-tux-000      thru vis-sta-tux-999        .
       acc-sta-tuc-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-sta-tuc-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-sta-tuc-100.
       acc-sta-tuc-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Codice agente di riferimento      *
      *    *-----------------------------------------------------------*
       vis-sta-tuc-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      13                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-sta-tuc (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-sta-tuc-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Codice agente di riferimento,     *
      *    *                         nominativo                        *
      *    *-----------------------------------------------------------*
       vis-sta-tuc-nom-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      w-tes-sta-tuc-nom (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-sta-tuc-nom-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Modalita' di trattamento     *
      *    *                              statistiche                  *
      *    *-----------------------------------------------------------*
       acc-sta-tux-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *                                             *
      *                  * Solo se agente di riferimento diverso da    *
      *                  * zero                                        *
      *                  *---------------------------------------------*
           if        w-tes-sta-tuc (1)    =    zero
                     go to acc-sta-tux-999.
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      w-tes-sta-tux (1)    to   w-sav-sta-tux          .
      *                  *---------------------------------------------*
      *                  * Preparazione eventuale default              *
      *                  *---------------------------------------------*
           if        w-tes-sta-tux (1)    not  = zero
                     go to acc-sta-tux-100.
           move      01                   to   w-tes-sta-tux (1)      .
       acc-sta-tux-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-sta-tux-lun    to   v-car                  .
           move      w-exp-sta-tux-num    to   v-ldt                  .
           move      "DG#"                to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      15                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-sta-tux-tbl    to   v-txt                  .
           move      w-tes-sta-tux (1)    to   v-num                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "PRSC"               to   v-pfk (07)             .
           move      "NXSC"               to   v-pfk (08)             .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-sta-tux-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-sta-tux-999.
       acc-sta-tux-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-sta-tux (1)      .
       acc-sta-tux-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se zero : reimpostazione, a meno che non si *
      *                  * sia in Up, e che il valore precedente non   *
      *                  * fosse gia' a zero                           *
      *                  *---------------------------------------------*
           if        w-tes-sta-tux (1)    not  = zero
                     go to acc-sta-tux-600.
           if        v-key                not  = "UP  "
                     go to acc-sta-tux-100.
           if        w-sav-sta-tux        =    zero
                     go to acc-sta-tux-600
           else      go to acc-sta-tux-100.
       acc-sta-tux-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-sta-tux-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-sta-tux-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-sta-tux-100.
       acc-sta-tux-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Modalita' di trattamento  *
      *    *                                 statistiche               *
      *    *-----------------------------------------------------------*
       vis-sta-tux-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-sta-tux-lun    to   v-car                  .
           move      w-exp-sta-tux-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      15                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-sta-tux-tbl    to   v-txt                  .
           move      w-tes-sta-tux (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-sta-tux-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Contropartita vendite        *
      *    *-----------------------------------------------------------*
       acc-ctp-ven-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-ctp-ven-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-pdc-ope      .
           move      w-prs-liv-pdc        to   w-cod-mne-pdc-liv      .
           move      w-tes-ctp-ven (1)    to   w-cod-mne-pdc-cod      .
           move      07                   to   w-cod-mne-pdc-lin      .
           move      30                   to   w-cod-mne-pdc-pos      .
           move      07                   to   w-cod-mne-pdc-dln      .
           move      41                   to   w-cod-mne-pdc-dps      .
           move      "B"                  to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-mne-pdc-cll-000  thru cod-mne-pdc-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-mne-pdc-foi-000  thru cod-mne-pdc-foi-999    .
       acc-ctp-ven-110.
           perform   cod-mne-pdc-cll-000  thru cod-mne-pdc-cll-999    .
           if        w-cod-mne-pdc-ope    =    "F+"
                     go to acc-ctp-ven-115.
           if        w-cod-mne-pdc-ope    =    "AC"
                     go to acc-ctp-ven-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-ctp-ven-115.
           perform   cod-mne-pdc-foi-000  thru cod-mne-pdc-foi-999    .
           go to     acc-ctp-ven-110.
       acc-ctp-ven-120.
           move      w-cod-mne-pdc-cod    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-ctp-ven-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-ctp-ven-999.
       acc-ctp-ven-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-ctp-ven (1)      .
       acc-ctp-ven-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura archivio piano dei conti            *
      *                  *---------------------------------------------*
           move      w-tes-ctp-ven (1)    to   w-let-arc-pdc-cod      .
           perform   let-arc-pdc-000      thru let-arc-pdc-999        .
           move      w-let-arc-pdc-des    to   w-tes-ctp-ven-des (1)  .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione sottoconto      *
      *                  *---------------------------------------------*
           perform   vis-ctp-ven-des-000  thru vis-ctp-ven-des-999    .
      *                  *---------------------------------------------*
      *                  * Se codice sottoconto non esistente : reim-  *
      *                  * postazione                                  *
      *                  *---------------------------------------------*
           if        w-let-arc-pdc-flg    not  = spaces
                     go to acc-ctp-ven-100.
       acc-ctp-ven-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-ctp-ven-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-ctp-ven-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-ctp-ven-100.
       acc-ctp-ven-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Contropartita vendite     *
      *    *-----------------------------------------------------------*
       vis-ctp-ven-000.
      *              *-------------------------------------------------*
      *              * Editing con appoggio a sinistra                 *
      *              *-------------------------------------------------*
           move      w-prs-liv-pdc        to   w-edt-cod-pdc-liv      .
           move      w-tes-ctp-ven (1)    to   w-edt-cod-pdc-cod      .
           move      "B"                  to   w-edt-cod-pdc-edm      .
           perform   edt-pdc-asx-000      thru edt-pdc-asx-999        .
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      09                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-edt-cod-pdc-edt    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-ctp-ven-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Descrizione sottoconto    *
      *    *-----------------------------------------------------------*
       vis-ctp-ven-des-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      w-tes-ctp-ven-des (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-ctp-ven-des-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Contropartita vendite        *
      *    *-----------------------------------------------------------*
       acc-ctp-rsv-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-ctp-rsv-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-pdc-ope      .
           move      w-prs-liv-pdc        to   w-cod-mne-pdc-liv      .
           move      w-tes-ctp-rsv (1)    to   w-cod-mne-pdc-cod      .
           move      09                   to   w-cod-mne-pdc-lin      .
           move      30                   to   w-cod-mne-pdc-pos      .
           move      09                   to   w-cod-mne-pdc-dln      .
           move      41                   to   w-cod-mne-pdc-dps      .
           move      "B"                  to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-mne-pdc-cll-000  thru cod-mne-pdc-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-mne-pdc-foi-000  thru cod-mne-pdc-foi-999    .
       acc-ctp-rsv-110.
           perform   cod-mne-pdc-cll-000  thru cod-mne-pdc-cll-999    .
           if        w-cod-mne-pdc-ope    =    "F+"
                     go to acc-ctp-rsv-115.
           if        w-cod-mne-pdc-ope    =    "AC"
                     go to acc-ctp-rsv-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-ctp-rsv-115.
           perform   cod-mne-pdc-foi-000  thru cod-mne-pdc-foi-999    .
           go to     acc-ctp-rsv-110.
       acc-ctp-rsv-120.
           move      w-cod-mne-pdc-cod    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-ctp-rsv-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-ctp-rsv-999.
       acc-ctp-rsv-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-ctp-rsv (1)      .
       acc-ctp-rsv-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura archivio piano dei conti            *
      *                  *---------------------------------------------*
           move      w-tes-ctp-rsv (1)    to   w-let-arc-pdc-cod      .
           perform   let-arc-pdc-000      thru let-arc-pdc-999        .
           move      w-let-arc-pdc-des    to   w-tes-ctp-rsv-des (1)  .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione sottoconto      *
      *                  *---------------------------------------------*
           perform   vis-ctp-rsv-des-000  thru vis-ctp-rsv-des-999    .
      *                  *---------------------------------------------*
      *                  * Se codice sottoconto non esistente : reim-  *
      *                  * postazione                                  *
      *                  *---------------------------------------------*
           if        w-let-arc-pdc-flg    not  = spaces
                     go to acc-ctp-rsv-100.
       acc-ctp-rsv-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-ctp-rsv-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-ctp-rsv-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-ctp-rsv-100.
       acc-ctp-rsv-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Contropartita vendite     *
      *    *-----------------------------------------------------------*
       vis-ctp-rsv-000.
      *              *-------------------------------------------------*
      *              * Editing con appoggio a sinistra                 *
      *              *-------------------------------------------------*
           move      w-prs-liv-pdc        to   w-edt-cod-pdc-liv      .
           move      w-tes-ctp-rsv (1)    to   w-edt-cod-pdc-cod      .
           move      "B"                  to   w-edt-cod-pdc-edm      .
           perform   edt-pdc-asx-000      thru edt-pdc-asx-999        .
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      09                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-edt-cod-pdc-edt    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-ctp-rsv-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Descrizione sottoconto    *
      *    *-----------------------------------------------------------*
       vis-ctp-rsv-des-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      w-tes-ctp-rsv-des (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-ctp-rsv-des-999.
           exit.

      *    *===========================================================*
      *    * Controllo su impostazione tasto Do campi chiave           *
      *    *-----------------------------------------------------------*
       cnt-tdo-key-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-tdo-key-flg      .
       cnt-tdo-key-999.
           exit.
           
      *    *===========================================================*
      *    * Controllo se chiave vuota                                 *
      *    *-----------------------------------------------------------*
       cnt-key-vuo-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-key-vuo-flg      .
      *              *-------------------------------------------------*
      *              * Test                                            *
      *              *-------------------------------------------------*
           if        w-tes-cod-age        =    zero
                     move  "#"            to   w-cnt-key-vuo-flg      .
       cnt-key-vuo-999.
           exit.

      *    *===========================================================*
      *    * Controllo su impostazione tasto Do campi non chiave       *
      *    *-----------------------------------------------------------*
       cnt-tdo-nok-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-tdo-nok-flg      .
       cnt-tdo-nok-050.
      *              *-------------------------------------------------*
      *              * Controllo su Codice nazione                     *
      *              *-------------------------------------------------*
           if        w-tes-cod-naz (1)    not  = spaces
                     go to cnt-tdo-nok-100.
           move      "Manca il codice nazione                         "
                                          to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-100.
      *              *-------------------------------------------------*
      *              * Controllo su Ragione sociale                    *
      *              *-------------------------------------------------*
           if        w-tes-rag-soc (1)    not  = spaces
                     go to cnt-tdo-nok-150.
           move      "Manca la ragione sociale                        "
                                          to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-150.
      *              *-------------------------------------------------*
      *              * Controllo su Nome agente                        *
      *              *-------------------------------------------------*
           if        w-tes-nom-age (1)    not  = spaces
                     go to cnt-tdo-nok-200.
           move      "Manca il nominativo dell'agente                 "
                                          to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-200.
       cnt-tdo-nok-400.
      *              *-------------------------------------------------*
      *              * Controllo su Data rilevamento status            *
      *              *-------------------------------------------------*
           if        w-tes-sta-tus (1)    =    01 or
                     w-tes-sta-tus (1)    =    99
                     go to cnt-tdo-nok-434.
           if        w-tes-sta-tud (1)    not  = zero
                     go to cnt-tdo-nok-434.
           move      "Manca la data di rilevamento dello status !       
      -              "          "         to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-434.
      *              *-------------------------------------------------*
      *              * Controllo su Codice agente di riferimento       *
      *              *-------------------------------------------------*
           if        w-tes-sta-tus (1)    not  = 21  and
                     w-tes-sta-tus (1)    not  = 52  and
                     w-tes-sta-tus (1)    not  = 62  and
                     w-tes-sta-tus (1)    not  = 72
                     go to cnt-tdo-nok-440.
           if        w-tes-sta-tuc (1)    =    zero
                     go to cnt-tdo-nok-436.
           move      w-tes-sta-tuc (1)    to   w-let-arc-age-cod      .
           perform   let-arc-age-000      thru let-arc-age-999        .
           if        w-let-arc-age-flg    =    spaces
                     go to cnt-tdo-nok-440.
           move      "Codice Agente di riferimento non esistente !      
      -              "          "         to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-436.
           move      "Manca il codice Agente di riferimento !           
      -              "          "         to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-440.
       cnt-tdo-nok-500.
      *              *-------------------------------------------------*
      *              * Normalizzazioni                                 *
      *              *-------------------------------------------------*
       cnt-tdo-nok-520.
      *                  *---------------------------------------------*
      *                  * Tipo maturazione                            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test sul tipo trattamento della matura- *
      *                      * zione delle provvigioni letto dalle     *
      *                      * personalizzazioni                       *
      *                      *-----------------------------------------*
           if        w-prs-age-300-tma    not  = 03
                     go to cnt-tdo-nok-524.
       cnt-tdo-nok-522.
      *                          *-------------------------------------*
      *                          * Se maturazione a seconda dell'agen- *
      *                          * te                                  *
      *                          *-------------------------------------*
           if        w-tes-tip-mat (1)    =    spaces
                     move  "I"            to   w-tes-tip-mat (1)      .
      *                          *-------------------------------------*
      *                          * Ad uscita                           *
      *                          *-------------------------------------*
           go to     cnt-tdo-nok-800.
       cnt-tdo-nok-524.
      *                          *-------------------------------------*
      *                          * Se maturazione indipendentemente    *
      *                          * dall'agente                         *
      *                          *-------------------------------------*
           if        w-tes-tip-mat (1)    not  = spaces
                     move  spaces         to   w-tes-tip-mat (1)      .
      *                          *-------------------------------------*
      *                          * Ad uscita                           *
      *                          *-------------------------------------*
           go to     cnt-tdo-nok-800.
       cnt-tdo-nok-800.
      *              *-------------------------------------------------*
      *              * Uscita per controlli tutti superati             *
      *              *-------------------------------------------------*
           go to     cnt-tdo-nok-999.
       cnt-tdo-nok-900.
      *              *-------------------------------------------------*
      *              * Emissione messaggio di errore e set del flag di *
      *              * uscita ad errore                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Box di errore                               *
      *                  *---------------------------------------------*
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                  *---------------------------------------------*
      *                  * Flag di errore in uscita                    *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cnt-tdo-nok-flg      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     cnt-tdo-nok-999.
       cnt-tdo-nok-999.
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
      *    * Normalizzazione dati chiave e non chiave                  *
      *    *-----------------------------------------------------------*
       nor-key-nok-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione dati chiave                     *
      *              *-------------------------------------------------*
           perform   nor-key-reg-000      thru nor-key-reg-999        .
      *              *-------------------------------------------------*
      *              * Normalizzazione dati non chiave testata         *
      *              *-------------------------------------------------*
           perform   nor-nok-tes-000      thru nor-nok-tes-999        .
       nor-key-nok-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione dati chiave                               *
      *    *-----------------------------------------------------------*
       nor-key-reg-000.
           move      zero                 to   w-tes-cod-age          .
           move      spaces               to   w-tes-cod-age-aut      .
       nor-key-reg-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione dati non chiave testata                   *
      *    *-----------------------------------------------------------*
       nor-nok-tes-000.
           move      zero                 to   w-tes-ide-dat (1)      .
           move      spaces               to   w-tes-ide-ute (1)      .
           move      spaces               to   w-tes-ide-fas (1)      .
           move      spaces               to   w-tes-cod-mne (1)      .
           move      spaces               to   w-tes-nom-key (1)      .
           move      spaces               to   w-tes-nom-age (1)      .
           move      spaces               to   w-tes-rag-key (1)      .
           move      spaces               to   w-tes-rag-soc (1)      .
           move      spaces               to   w-tes-via-age (1)      .
           move      spaces               to   w-tes-loc-age (1)      .
           move      spaces               to   w-tes-cod-naz (1)      .
           move      spaces               to   w-tes-cod-naz-des (1)  .
           move      zero                 to   w-tes-cod-cmn (1)      .
           move      spaces               to   w-tes-cod-cmn-des (1)  .
           move      spaces               to   w-tes-cod-cmn-prv (1)  .
           move      zero                 to   w-tes-cod-fzn (1)      .
           move      spaces               to   w-tes-cod-fzn-des (1)  .
           move      zero                 to   w-tes-cod-lct (1)      .
           move      spaces               to   w-tes-cod-lct-des (1)  .
           move      zero                 to   w-tes-prt-iva (1)      .
           move      spaces               to   w-tes-cod-fis (1)      .
           move      zero                 to   w-tes-sup-age (1)      .
           move      spaces               to   w-tes-sup-age-nom (1)  .
           move      zero                 to   w-tes-cat-pvg (1)      .
           move      spaces               to   w-tes-cat-pvg-des (1)  .
           move      zero                 to   w-tes-per-pvg (1, 1)   .
           move      zero                 to   w-tes-per-pvg (1, 2)   .
           move      zero                 to   w-tes-per-pvg (1, 3)   .
           move      spaces               to   w-tes-tip-mat (1)      .
           move      spaces               to   w-tes-flg-cpv (1)      .
           move      zero                 to   w-tes-ctp-ven (1)      .
           move      spaces               to   w-tes-ctp-ven-des (1)  .
           move      zero                 to   w-tes-ctp-rsv (1)      .
           move      spaces               to   w-tes-ctp-rsv-des (1)  .
      *              *-------------------------------------------------*
      *              * Valori in obsolescenza                          *
      *              *-------------------------------------------------*
           move      spaces               to   w-tes-num-tel (1)      .
           move      spaces               to   w-tes-num-fax (1)      .
           move      spaces               to   w-tes-num-tlx (1)      .
      *
           move      spaces               to   w-tes-num-trg (1)      .
           move      zero                 to   w-tes-dpz-op1 (1)      .
           move      spaces               to   w-tes-dpz-op1-des (1)  .
           move      zero                 to   w-tes-dpz-op2 (1)      .
           move      spaces               to   w-tes-dpz-op2-des (1)  .
      *
           move      spaces               to   w-tes-nom-int (1)      .
      *              *-------------------------------------------------*
      *              * Contatti                                        *
      *              *-------------------------------------------------*
           move      zero                 to   w-acc-con-arc-c0w      .
       nor-nok-tes-600.
           add       1                    to   w-acc-con-arc-c0w      .
           if        w-acc-con-arc-c0w    >    w-acc-con-arc-max
                     go to nor-nok-tes-620.
           move      spaces               to   w-tes-tip-con
                                              (1, w-acc-con-arc-c0w)  .
           move      spaces               to   w-tes-num-con
                                              (1, w-acc-con-arc-c0w)  .
           move      spaces               to   w-tes-int-con
                                              (1, w-acc-con-arc-c0w)  .
           go to     nor-nok-tes-600.
       nor-nok-tes-620.
      *              *-------------------------------------------------*
      *              * Campi non gestiti dal programma                 *
      *              *-------------------------------------------------*
           move      zero                 to   w-tes-cod-cge (1)      .
           move      zero                 to   w-tes-cla-bdg (1)      .
      *              *-------------------------------------------------*
      *              * Campi aggiunti                                  *
      *              *-------------------------------------------------*
           move      zero                 to   w-tes-sta-tus (1)      .
           move      zero                 to   w-tes-sta-tud (1)      .
           move      zero                 to   w-tes-sta-tuc (1)      .
           move      spaces               to   w-tes-sta-tuc-nom (1)  .
           move      zero                 to   w-tes-sta-tux (1)      .
           move      zero                 to   w-tes-dat-iat (1)      .
           move      spaces               to   w-tes-alx-exp (1)      .
       nor-nok-tes-999.
           exit.

      *    *===========================================================*
      *    * Lettura registrazione pre-esistente                       *
      *    *-----------------------------------------------------------*
       rou-let-reg-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-rou-let-reg      .
      *              *-------------------------------------------------*
      *              * Lettura                                         *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODAGE    "         to   f-key                  .
           move      w-tes-cod-age        to   rf-age-cod-age         .
           move      "pgm/age/fls/ioc/obj/iofage"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-age                 .
           if        f-sts                =    e-not-err
                     go to rou-let-reg-100.
       rou-let-reg-050.
      *                  *---------------------------------------------*
      *                  * Se anagrafica non trovata                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se inserimento consentito          *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test                                *
      *                          *-------------------------------------*
           if        w-cnt-mfu-vis-sgr    not  = "V"
                     go to rou-let-reg-060.
      *                          *-------------------------------------*
      *                          * Flag di uscita                      *
      *                          *-------------------------------------*
           move      "#"                  to   w-cnt-rou-let-reg      .
      *                          *-------------------------------------*
      *                          * Messaggio                           *
      *                          *-------------------------------------*
           move      "Inserimento non consentito !                      
      -              "               "    to   w-err-box-err-msg      .
      *                          *-------------------------------------*
      *                          * Box di errore                       *
      *                          *-------------------------------------*
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                          *-------------------------------------*
      *                          * Ad uscita                           *
      *                          *-------------------------------------*
           go to     rou-let-reg-900.
       rou-let-reg-060.
      *                      *-----------------------------------------*
      *                      * Tipo funzionamento : Inserimento        *
      *                      *-----------------------------------------*
           move      "I"                  to   w-cnt-mfu-tip-fun      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     rou-let-reg-999.
       rou-let-reg-100.
      *                  *---------------------------------------------*
      *                  * Se anagrafica trovata                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Tipo funzionamento : Modifica           *
      *                      *-----------------------------------------*
           move      "M"                  to   w-cnt-mfu-tip-fun      .
      *                      *-----------------------------------------*
      *                      * Determinazione valori attuali           *
      *                      *-----------------------------------------*
           move      rf-age-ide-dat       to   w-tes-ide-dat (1)      .
           move      rf-age-ide-ute       to   w-tes-ide-ute (1)      .
           move      rf-age-ide-fas       to   w-tes-ide-fas (1)      .
           move      rf-age-cod-mne       to   w-tes-cod-mne (1)      .
           move      rf-age-rag-key       to   w-tes-rag-key (1)      .
           move      rf-age-rag-soc       to   w-tes-rag-soc (1)      .
           move      rf-age-nom-key       to   w-tes-nom-key (1)      .
           move      rf-age-nom-age       to   w-tes-nom-age (1)      .
           move      rf-age-via-age       to   w-tes-via-age (1)      .
           move      rf-age-loc-age       to   w-tes-loc-age (1)      .
           move      rf-age-cod-naz       to   w-tes-cod-naz (1)      .
           move      rf-age-cod-cmn       to   w-tes-cod-cmn (1)      .
           move      rf-age-cod-fzn       to   w-tes-cod-fzn (1)      .
           move      rf-age-cod-lct       to   w-tes-cod-lct (1)      .
           move      rf-age-num-tel       to   w-tes-num-tel (1)      .
           move      rf-age-num-fax       to   w-tes-num-fax (1)      .
           move      rf-age-num-tlx       to   w-tes-num-tlx (1)      .
           move      rf-age-nom-int       to   w-tes-nom-int (1)      .
           move      rf-age-prt-iva       to   w-tes-prt-iva (1)      .
           move      rf-age-cod-fis       to   w-tes-cod-fis (1)      .
           move      rf-age-sup-age       to   w-tes-sup-age (1)      .
           move      rf-age-cat-pvg       to   w-tes-cat-pvg (1)      .
           move      rf-age-per-pvg (1)   to   w-tes-per-pvg (1, 1)   .
           move      rf-age-per-pvg (2)   to   w-tes-per-pvg (1, 2)   .
           move      rf-age-per-pvg (3)   to   w-tes-per-pvg (1, 3)   .
           move      rf-age-tip-mat       to   w-tes-tip-mat (1)      .
           move      rf-age-flg-cpv       to   w-tes-flg-cpv (1)      .
           move      rf-age-ctp-ven       to   w-tes-ctp-ven (1)      .
           move      rf-age-ctp-rsv       to   w-tes-ctp-rsv (1)      .
      *                      *-----------------------------------------*
      *                      * Campi non gestiti dal programma         *
      *                      *-----------------------------------------*
           move      rf-age-cod-cge       to   w-tes-cod-cge (1)      .
           move      rf-age-cla-bdg       to   w-tes-cla-bdg (1)      .
      *                      *-----------------------------------------*
      *                      * Campi aggiunti                          *
      *                      *-----------------------------------------*
           move      rf-age-sta-tus       to   w-tes-sta-tus (1)      .
           move      rf-age-sta-tud       to   w-tes-sta-tud (1)      .
           move      rf-age-sta-tuc       to   w-tes-sta-tuc (1)      .
           move      rf-age-sta-tux       to   w-tes-sta-tux (1)      .
           move      rf-age-dat-iat       to   w-tes-dat-iat (1)      .
      *                      *-----------------------------------------*
      *                      * Normalizzazione campi aggiunti          *
      *                      *-----------------------------------------*
           if        rf-age-sta-tus       not  numeric
                     move  01             to   w-tes-sta-tus (1)      .
           if        rf-age-sta-tud       not  numeric
                     move  zero           to   w-tes-sta-tud (1)      .
           if        rf-age-sta-tuc       not  numeric
                     move  zero           to   w-tes-sta-tuc (1)      .
           if        rf-age-sta-tux       not  numeric
                     move  zero           to   w-tes-sta-tux (1)      .
           if        rf-age-dat-iat       not  numeric
                     move  zero           to   w-tes-dat-iat (1)      .
      *
           if        w-tes-dpz-op1 (1)    not  numeric
                     move  zero           to   w-tes-dpz-op1 (1)      .
           if        w-tes-dpz-op2 (1)    not  numeric
                     move  zero           to   w-tes-dpz-op2 (1)      .
      *                      *-----------------------------------------*
      *                      * Area per espansioni speciali            *
      *                      *-----------------------------------------*
           move      rf-age-alx-exp       to   w-tes-alx-exp (1)      .
       rou-let-reg-200.
      *                          *-------------------------------------*
      *                          * Valori contenuti indirettamente in  *
      *                          * record [age]                        *
      *                          *-------------------------------------*
       rou-let-reg-205.
      *                              *---------------------------------*
      *                              * Valori dipendenti dal codice    *
      *                              * nazione                         *
      *                              *---------------------------------*
           move      w-tes-cod-naz (1)    to   w-let-arc-gxn-cod      .
           perform   let-arc-gxn-000      thru let-arc-gxn-999        .
           move      w-let-arc-gxn-des    to   w-tes-cod-naz-des (1)  .
       rou-let-reg-210.
      *                              *---------------------------------*
      *                              * Valori dipendenti dal Codice    *
      *                              * Comune                          *
      *                              *---------------------------------*
           move      "C"                  to   w-let-arc-gxc-tip      .
           move      w-tes-cod-cmn (1)    to   w-let-arc-gxc-cmn      .
           move      zero                 to   w-let-arc-gxc-fzn      .
           move      zero                 to   w-let-arc-gxc-lct      .
           perform   let-arc-gxc-000      thru let-arc-gxc-999        .
           move      w-let-arc-gxc-des    to   w-tes-cod-cmn-des (1)  .
           move      w-let-arc-gxc-prv    to   w-tes-cod-cmn-prv (1)  .
       rou-let-reg-215.
      *                              *---------------------------------*
      *                              * Valori dipendenti dal Codice    *
      *                              * Frazione                        *
      *                              *---------------------------------*
           move      "F"                  to   w-let-arc-gxc-tip      .
           move      w-tes-cod-cmn (1)    to   w-let-arc-gxc-cmn      .
           move      w-tes-cod-fzn (1)    to   w-let-arc-gxc-fzn      .
           move      zero                 to   w-let-arc-gxc-lct      .
           perform   let-arc-gxc-000      thru let-arc-gxc-999        .
           move      w-let-arc-gxc-des    to   w-tes-cod-fzn-des (1)  .
       rou-let-reg-220.
      *                              *---------------------------------*
      *                              * Valori dipendenti dal Codice    *
      *                              * Localita'                       *
      *                              *---------------------------------*
           move      "L"                  to   w-let-arc-gxc-tip      .
           move      w-tes-cod-cmn (1)    to   w-let-arc-gxc-cmn      .
           move      w-tes-cod-fzn (1)    to   w-let-arc-gxc-fzn      .
           move      w-tes-cod-lct (1)    to   w-let-arc-gxc-lct      .
           perform   let-arc-gxc-000      thru let-arc-gxc-999        .
           move      w-let-arc-gxc-des    to   w-tes-cod-lct-des (1)  .
       rou-let-reg-240.
      *                              *---------------------------------*
      *                              * Valori dipendenti dal codice    *
      *                              * categoria provvigioni           *
      *                              *---------------------------------*
           move      01                   to   w-let-arc-zpv-tip      .
           move      w-tes-cat-pvg (1)    to   w-let-arc-zpv-cod      .
           perform   let-arc-zpv-000      thru let-arc-zpv-999        .
           move      w-let-arc-zpv-des    to   w-tes-cat-pvg-des (1)  .
      *                              *---------------------------------*
      *                              * Valori dipendenti dal codice    *
      *                              * agente supervisore              *
      *                              *---------------------------------*
           move      w-tes-sup-age (1)    to   w-let-arc-age-cod      .
           perform   let-arc-age-000      thru let-arc-age-999        .
           move      w-let-arc-age-nom    to   w-tes-sup-age-nom (1)  .
      *                              *---------------------------------*
      *                              * Valori dipendenti dal codice    *
      *                              * contropartita vendite           *
      *                              *---------------------------------*
           move      w-tes-ctp-ven (1)    to   w-let-arc-pdc-cod      .
           perform   let-arc-pdc-000      thru let-arc-pdc-999        .
           move      w-let-arc-pdc-des    to   w-tes-ctp-ven-des (1)  .
      *                              *---------------------------------*
      *                              * Valori dipendenti dal codice    *
      *                              * contropartita resi su vendite   *
      *                              *---------------------------------*
           move      w-tes-ctp-rsv (1)    to   w-let-arc-pdc-cod      .
           perform   let-arc-pdc-000      thru let-arc-pdc-999        .
           move      w-let-arc-pdc-des    to   w-tes-ctp-rsv-des (1)  .
      *                              *---------------------------------*
      *                              * Lettura contatti [adc]          *
      *                              *---------------------------------*
           perform   rou-let-reg-adc-000  thru rou-let-reg-adc-999    .
      *                              *---------------------------------*
      *                              * Valori dipendenti dal codice    *
      *                              * agente sostitutivo              *
      *                              *---------------------------------*
           move      w-tes-sta-tuc (1)    to   w-let-arc-age-cod      .
           perform   let-arc-age-000      thru let-arc-age-999        .
           move      w-let-arc-age-nom    to   w-tes-sta-tuc-nom (1)  .
      *                              *---------------------------------*
      *                              * Valori dipendenti dal codice    *
      *                              * dipendenza                      *
      *                              *---------------------------------*
           move      w-tes-dpz-op1 (1)    to   w-let-arc-dpz-cod      .
           perform   let-arc-dpz-000      thru let-arc-dpz-999        .
           move      w-let-arc-dpz-des    to   w-tes-dpz-op1-des (1)  .
      *                      *-----------------------------------------*
      *                      * Valori precedenti anagrafica            *
      *                      *-----------------------------------------*
           move      w-tes-val-aep (1)    to   w-tes-val-aep (2)      .
       rou-let-reg-800.
      *              *-------------------------------------------------*
      *              * Test per visualizzazione                        *
      *              *-------------------------------------------------*
           if        w-cnt-mfu-vis-sgr    =    "V"
                     move  "V"            to   w-cnt-mfu-tip-fun      .
       rou-let-reg-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     rou-let-reg-999.
       rou-let-reg-999.
           exit.

      *    *===========================================================*
      *    * Lettura registrazione pre-esistente                       *
      *    *                                                           *
      *    * Contatti                                                  *
      *    *-----------------------------------------------------------*
       rou-let-reg-adc-000.
      *              *-------------------------------------------------*
      *              * Determinazione contatti agente                  *
      *              *-------------------------------------------------*
           move      "DT"                 to   d-con-arc-tip-ope      .
           move      31                   to   d-con-arc-tip-arc      .
           move      w-tes-cod-age        to   d-con-arc-cod-arc      .
           move      spaces               to   d-con-arc-dpz-arc      .
           move      spaces               to   d-con-arc-tip-sel      .
           perform   con-arc-tip-cll-000  thru con-arc-tip-cll-999    .
      *              *-------------------------------------------------*
      *              * Ciclo di bufferizzazione                        *
      *              *-------------------------------------------------*
           move      zero                 to   w-acc-con-arc-c0w      .
       rou-let-reg-adc-200.
           add       1                    to   w-acc-con-arc-c0w      .
           if        w-acc-con-arc-c0w    >    d-con-arc-num-ele
                     go to rou-let-reg-adc-900.
           move      d-con-arc-tip-con
                    (w-acc-con-arc-c0w)   to   w-tes-tip-con
                                              (1, w-acc-con-arc-c0w)  .
           move      d-con-arc-num-con
                    (w-acc-con-arc-c0w)   to   w-tes-num-con
                                              (1, w-acc-con-arc-c0w)  .
           move      d-con-arc-int-con
                    (w-acc-con-arc-c0w)   to   w-tes-int-con
                                              (1, w-acc-con-arc-c0w)  .
           go to     rou-let-reg-adc-200.
       rou-let-reg-adc-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     rou-let-reg-adc-999.
       rou-let-reg-adc-999.
           exit.

      *    *===========================================================*
      *    * Routine pre-accettazioni per inserimento                  *
      *    *-----------------------------------------------------------*
       pre-acc-ins-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-pre-acc-ins      .
       pre-acc-ins-100.
      *              *-------------------------------------------------*
      *              * Preparazione defaults                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Eventuale default Codice nazione            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione del valore di default      *
      *                      *-----------------------------------------*
           move      "IT "                to   w-tes-cod-naz (1)      .
      *                      *-----------------------------------------*
      *                      * Preparazione della descrizione relativa *
      *                      * al valore di default                    *
      *                      *-----------------------------------------*
           move      w-tes-cod-naz (1)    to   w-let-arc-gxn-cod      .
           perform   let-arc-gxn-000      thru let-arc-gxn-999        .
           move      w-let-arc-gxn-des    to   w-tes-cod-naz-des (1)  .
      *                      *-----------------------------------------*
      *                      * Visualizzazione valore di default       *
      *                      *-----------------------------------------*
           perform   vis-cod-naz-000      thru vis-cod-naz-999        .
      *                      *-----------------------------------------*
      *                      * Visualizzazione descrizione relativa al *
      *                      * valore di default                       *
      *                      *-----------------------------------------*
           perform   vis-des-naz-000      thru vis-des-naz-999        .
      *                  *---------------------------------------------*
      *                  * Eventuale default status agente             *
      *                  *---------------------------------------------*
           move      01                   to   w-tes-sta-tus (1)      .
       pre-acc-ins-999.
           exit.

      *    *===========================================================*
      *    * Routine pre-accettazioni per modifica                     *
      *    *-----------------------------------------------------------*
       pre-acc-mod-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-pre-acc-mod      .
       pre-acc-mod-999.
           exit.

      *    *===========================================================*
      *    * Routine pre-accettazioni per visualizzazione              *
      *    *-----------------------------------------------------------*
       pre-acc-vis-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-pre-acc-vis      .
       pre-acc-vis-999.
           exit.

      *    *===========================================================*
      *    * Routine post-exit su inserimento                          *
      *    *-----------------------------------------------------------*
       pos-exi-ins-000.
      *              *-------------------------------------------------*
      *              * Se e' stata eseguita l'attribuzione del codice  *
      *              * in automatico, si ripristina, se possibile, il  *
      *              * codice al valore precedente l'incremento        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se non attribuzione automatica : uscita     *
      *                  *---------------------------------------------*
           if        w-tes-cod-age-aut    =    spaces
                     go to pos-exi-ins-999.
      *                  *---------------------------------------------*
      *                  * Ripristino codice automatico progressivo    *
      *                  *---------------------------------------------*
           perform   rip-cod-aut-000      thru rip-cod-aut-999        .
       pos-exi-ins-999.
           exit.

      *    *===========================================================*
      *    * Routine post-exit su modifica                             *
      *    *-----------------------------------------------------------*
       pos-exi-mod-000.
       pos-exi-mod-999.
           exit.

      *    *===========================================================*
      *    * Routine post-exit su visualizzazione                      *
      *    *-----------------------------------------------------------*
       pos-exi-vis-000.
       pos-exi-vis-999.
           exit.

      *    *===========================================================*
      *    * Routine post-conferma di inserimento                      *
      *    *-----------------------------------------------------------*
       pos-cnf-ins-000.
      *              *-------------------------------------------------*
      *              * Scrittura movimento su files                    *
      *              *-------------------------------------------------*
           perform   scr-mov-fil-000      thru scr-mov-fil-999        .
       pos-cnf-ins-999.
           exit.

      *    *===========================================================*
      *    * Routine post-conferma di modifica                         *
      *    *-----------------------------------------------------------*
       pos-cnf-mod-000.
      *              *-------------------------------------------------*
      *              * Scrittura movimento su files                    *
      *              *-------------------------------------------------*
           perform   scr-mov-fil-000      thru scr-mov-fil-999        .
       pos-cnf-mod-999.
           exit.

      *    *===========================================================*
      *    * Routine post-conferma di annullamento                     *
      *    *-----------------------------------------------------------*
       pos-cnf-ann-000.
      *              *-------------------------------------------------*
      *              * Delete movimento da files                       *
      *              *-------------------------------------------------*
           perform   del-mov-fil-000      thru del-mov-fil-999        .
       pos-cnf-ann-999.
           exit.

      *    *===========================================================*
      *    * Scrittura movimento su file                               *
      *    *-----------------------------------------------------------*
       scr-mov-fil-000.
      *              *-------------------------------------------------*
      *              * Trattamento file [age]                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se inserimento                              *
      *                  *---------------------------------------------*
           if        w-cnt-mfu-tip-fun    not  = "I"
                     go to scr-mov-fil-500.
      *                      *-----------------------------------------*
      *                      * Write record [age]                      *
      *                      *-----------------------------------------*
           perform   wrt-rec-age-000      thru wrt-rec-age-999        .
      *                      *-----------------------------------------*
      *                      * Write record [adc]                      *
      *                      *-----------------------------------------*
           perform   wrt-rec-adc-000      thru wrt-rec-adc-999        .
           go to     scr-mov-fil-999.
       scr-mov-fil-500.
      *                  *---------------------------------------------*
      *                  * Se modifica                                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Rewrite record [age]                    *
      *                      *-----------------------------------------*
           perform   rew-rec-age-000      thru rew-rec-age-999        .
      *                      *-----------------------------------------*
      *                      * Rewrite record [adc]                    *
      *                      *-----------------------------------------*
           perform   rew-rec-adc-000      thru rew-rec-adc-999        .
       scr-mov-fil-999.
           exit.

      *    *===========================================================*
      *    * Delete movimento da file                                  *
      *    *-----------------------------------------------------------*
       del-mov-fil-000.
      *              *-------------------------------------------------*
      *              * Delete record [age]                             *
      *              *-------------------------------------------------*
           perform   del-rec-age-000      thru del-rec-age-999        .
      *              *-------------------------------------------------*
      *              * Delete record [adc]                             *
      *              *-------------------------------------------------*
           perform   del-rec-adc-000      thru del-rec-adc-999        .
       del-mov-fil-999.
           exit.

      *    *===========================================================*
      *    * Composizione record [age]                                 *
      *    *-----------------------------------------------------------*
       cmp-rec-age-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione record                          *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/age/fls/ioc/obj/iofage"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-age                 .
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Campi chiave                                *
      *                  *---------------------------------------------*
           move      w-tes-cod-age        to   rf-age-cod-age         .
      *                  *---------------------------------------------*
      *                  * Campi non chiave                            *
      *                  *---------------------------------------------*
           move      "IG"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-dat                to   rf-age-ide-dat         .
           move      s-ute                to   rf-age-ide-ute         .
           move      s-fas                to   rf-age-ide-fas         .
           move      w-tes-cod-mne (1)    to   rf-age-cod-mne         .
           move      w-tes-rag-key (1)    to   rf-age-rag-key         .
           move      w-tes-rag-soc (1)    to   rf-age-rag-soc         .
           move      w-tes-nom-key (1)    to   rf-age-nom-key         .
           move      w-tes-nom-age (1)    to   rf-age-nom-age         .
           move      w-tes-via-age (1)    to   rf-age-via-age         .
           move      w-tes-loc-age (1)    to   rf-age-loc-age         .
           move      w-tes-cod-naz (1)    to   rf-age-cod-naz         .
           move      w-tes-cod-cmn (1)    to   rf-age-cod-cmn         .
           move      w-tes-cod-fzn (1)    to   rf-age-cod-fzn         .
           move      w-tes-cod-lct (1)    to   rf-age-cod-lct         .
           move      w-tes-num-tel (1)    to   rf-age-num-tel         .
           move      w-tes-num-fax (1)    to   rf-age-num-fax         .
           move      w-tes-num-tlx (1)    to   rf-age-num-tlx         .
           move      w-tes-nom-int (1)    to   rf-age-nom-int         .
           move      w-tes-prt-iva (1)    to   rf-age-prt-iva         .
           move      w-tes-cod-fis (1)    to   rf-age-cod-fis         .
           move      w-tes-sup-age (1)    to   rf-age-sup-age         .
           move      w-tes-cat-pvg (1)    to   rf-age-cat-pvg         .
           move      w-tes-per-pvg (1, 1) to   rf-age-per-pvg (1)     .
           move      w-tes-per-pvg (1, 2) to   rf-age-per-pvg (2)     .
           move      w-tes-per-pvg (1, 3) to   rf-age-per-pvg (3)     .
           move      w-tes-tip-mat (1)    to   rf-age-tip-mat         .
           move      w-tes-flg-cpv (1)    to   rf-age-flg-cpv         .
           move      w-tes-ctp-ven (1)    to   rf-age-ctp-ven         .
           move      w-tes-ctp-rsv (1)    to   rf-age-ctp-rsv         .
      *                  *---------------------------------------------*
      *                  * Campi non gestiti dal programma             *
      *                  *---------------------------------------------*
           move      w-tes-cod-cge (1)    to   rf-age-cod-cge         .
           move      w-tes-cla-bdg (1)    to   rf-age-cla-bdg         .
      *                  *---------------------------------------------*
      *                  * Campi aggiunti                              *
      *                  *---------------------------------------------*
           move      w-tes-sta-tus (1)    to   rf-age-sta-tus         .
           move      w-tes-sta-tud (1)    to   rf-age-sta-tud         .
           move      w-tes-sta-tuc (1)    to   rf-age-sta-tuc         .
           move      w-tes-sta-tux (1)    to   rf-age-sta-tux         .
           move      w-tes-dat-iat (1)    to   rf-age-dat-iat         .
           move      w-tes-alx-exp (1)    to   rf-age-alx-exp         .
       cmp-rec-age-999.
           exit.

      *    *===========================================================*
      *    * Scrittura record [age]                                    *
      *    *-----------------------------------------------------------*
       wrt-rec-age-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-age-000      thru cmp-rec-age-999        .
      *              *-------------------------------------------------*
      *              * Put record                                      *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      "pgm/age/fls/ioc/obj/iofage"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-age                 .
       wrt-rec-age-999.
           exit.

      *    *===========================================================*
      *    * Riscrittura record [age]                                  *
      *    *-----------------------------------------------------------*
       rew-rec-age-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-age-000      thru cmp-rec-age-999        .
      *              *-------------------------------------------------*
      *              * Forced put record                               *
      *              *-------------------------------------------------*
           move      "FP"                 to   f-ope                  .
           move      "pgm/age/fls/ioc/obj/iofage"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-age                 .
       rew-rec-age-999.
           exit.

      *    *===========================================================*
      *    * Cancellazione record [age]                                *
      *    *-----------------------------------------------------------*
       del-rec-age-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-age-000      thru cmp-rec-age-999        .
      *              *-------------------------------------------------*
      *              * Delete record                                   *
      *              *-------------------------------------------------*
           move      "DE"                 to   f-ope                  .
           move      "pgm/age/fls/ioc/obj/iofage"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-age                 .
       del-rec-age-999.
           exit.

      *    *===========================================================*
      *    * Riscrittura record [adc]                                  *
      *    *-----------------------------------------------------------*
       rew-rec-adc-000.
      *              *-------------------------------------------------*
      *              * Cancellazione preventiva delle righe            *
      *              *-------------------------------------------------*
           perform   del-rec-adc-000      thru del-rec-adc-999        .
       rew-rec-adc-400.
      *              *-------------------------------------------------*
      *              * Scrittura delle righe                           *
      *              *-------------------------------------------------*
           perform   wrt-rec-adc-000      thru wrt-rec-adc-999        .
       rew-rec-adc-999.
           exit.

      *    *===========================================================*
      *    * Composizione record [adc]                                 *
      *    *-----------------------------------------------------------*
       cmp-rec-adc-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione record                          *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/azi/fls/ioc/obj/iofadc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-adc                 .
       cmp-rec-adc-100.
      *              *-------------------------------------------------*
      *              * Composizione                                    *
      *              *-------------------------------------------------*
           move      "IG"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-dat                to   rf-adc-ide-dat         .
           move      s-ute                to   rf-adc-ide-ute         .
           move      s-fas                to   rf-adc-ide-fas         .
           move      31                   to   rf-adc-tip-arc         .
           move      w-tes-cod-age        to   rf-adc-cod-arc         .
           move      spaces               to   rf-adc-dpz-arc         .
      *
           move      w-tes-rag-key (1)    to   w-all-str-alf          .
           move      40                   to   w-all-str-lun          .
           perform   all-str-upp-000      thru all-str-upp-999        .
           move      w-all-str-alf        to   rf-adc-des-key         .
      *
           move      w-tes-tip-con
                    (1, w-acc-con-arc-c0w)
                                          to   rf-adc-tip-con         .
           move      w-acc-con-arc-c0w    to   rf-adc-num-prg         .
           move      w-tes-int-con
                    (1, w-acc-con-arc-c0w)
                                          to   rf-adc-int-con         .
           move      spaces               to   rf-adc-rep-con         .
           move      spaces               to   rf-adc-pri-con         .
           move      spaces               to   rf-adc-pre-con         .
           move      w-tes-num-con
                    (1, w-acc-con-arc-c0w)
                                          to   rf-adc-num-con         .
           move      rf-adc-ide-dat       to   rf-adc-dat-agg         .
           move      spaces               to   rf-adc-alx-exp         .
       cmp-rec-adc-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     cmp-rec-adc-999.
       cmp-rec-adc-999.
           exit.

      *    *===========================================================*
      *    * Scrittura record [adc]                                    *
      *    *-----------------------------------------------------------*
       wrt-rec-adc-000.
      *              *-------------------------------------------------*
      *              * Scrittura delle righe                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ciclo                                       *
      *                  *---------------------------------------------*
           move      zero                 to   w-acc-con-arc-c0w      .
       wrt-rec-adc-200.
           add       1                    to   w-acc-con-arc-c0w      .
           if        w-acc-con-arc-c0w    >    w-acc-con-arc-max
                     go to wrt-rec-adc-999.
      *                  *---------------------------------------------*
      *                  * Test se riga da scrivere                    *
      *                  *---------------------------------------------*
           if        w-tes-tip-con
                    (1, w-acc-con-arc-c0w)
                                          =    spaces
                     go to wrt-rec-adc-999.
      *                  *---------------------------------------------*
      *                  * Composizione record                         *
      *                  *---------------------------------------------*
           perform   cmp-rec-adc-000      thru cmp-rec-adc-999        .
      *                  *---------------------------------------------*
      *                  * Put record                                  *
      *                  *---------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      "pgm/azi/fls/ioc/obj/iofadc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-adc                 .
      *                  *---------------------------------------------*
      *                  * Riciclo a lettura riga successiva           *
      *                  *---------------------------------------------*
           go to     wrt-rec-adc-200.
       wrt-rec-adc-999.
           exit.

      *    *===========================================================*
      *    * Cancellazione record [adc]                                *
      *    *-----------------------------------------------------------*
       del-rec-adc-000.
      *              *-------------------------------------------------*
      *              * Cancellazione delle righe                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ciclo                                       *
      *                  *---------------------------------------------*
           move      zero                 to   w-acc-con-arc-c0w      .
       del-rec-adc-200.
           add       1                    to   w-acc-con-arc-c0w      .
           if        w-acc-con-arc-c0w    >    w-acc-con-arc-max
                     go to del-rec-adc-999.
      *                  *---------------------------------------------*
      *                  * Composizione record                         *
      *                  *---------------------------------------------*
           perform   cmp-rec-adc-000      thru cmp-rec-adc-999        .
      *                  *---------------------------------------------*
      *                  * Delete record                               *
      *                  *---------------------------------------------*
           move      "DE"                 to   f-ope                  .
           move      "pgm/azi/fls/ioc/obj/iofadc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-adc                 .
      *                  *---------------------------------------------*
      *                  * Riciclo a lettura riga successiva           *
      *                  *---------------------------------------------*
           go to     del-rec-adc-200.
       del-rec-adc-999.
           exit.

      *    *===========================================================*
      *    * Insr su archivio [ags]                                    *
      *    *-----------------------------------------------------------*
       ins-arc-ags-000.
      *              *-------------------------------------------------*
      *              * Test se programma di gestione file gia' attivo  *
      *              *-------------------------------------------------*
           move      "P?"                 to   s-ope                  .
           move      "page2200"           to   s-pro                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-liv                not  = zero
                     go to  ins-arc-ags-999.
      *              *-------------------------------------------------*
      *              * Variabile di i.p.c. per il codice agente da     *
      *              * passare al livello successivo                   *
      *              *-------------------------------------------------*
           move      "PV"                 to   s-ope                  .
           move      "cod-age"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "N"                  to   s-tip                  .
           move      07                   to   s-car                  .
           move      zero                 to   s-dec                  .
           move      spaces               to   s-sgn                  .
           move      w-ins-arc-ags-age    to   s-num                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Richiamo programma di gestione archivio         *
      *              *-------------------------------------------------*
           move      "pgm/age/prg/obj/page2200"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat                                            .
           cancel    s-pat                                            .
       ins-arc-ags-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [ada]                         *
      *    *-----------------------------------------------------------*
           copy      "pgm/azi/prg/cpy/larcada0.lts"                   .

      *    *===========================================================*
      *    * Routine lettura tabella [gxn]                             *
      *    *-----------------------------------------------------------*
       let-arc-gxn-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-gxn-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice a spazi                          *
      *              *-------------------------------------------------*
           if        w-let-arc-gxn-cod    =    spaces
                     go to let-arc-gxn-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODNAZ    "         to   f-key                  .
           move      w-let-arc-gxn-cod    to   rf-gxn-cod-naz         .
           move      "pgm/geo/fls/ioc/obj/iofgxn"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxn                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-gxn-400.
       let-arc-gxn-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-gxn-des-naz       to   w-let-arc-gxn-des      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-gxn-999.
       let-arc-gxn-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-gxn-flg      .
           move      all   "."            to   w-let-arc-gxn-des      .
           go to     let-arc-gxn-999.
       let-arc-gxn-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-gxn-des      .
       let-arc-gxn-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [gxc]                         *
      *    *-----------------------------------------------------------*
       let-arc-gxc-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-gxc-flg      .
      *              *-------------------------------------------------*
      *              * Deviazione secondo il tipo elemento             *
      *              *-------------------------------------------------*
           if        w-let-arc-gxc-tip    =    "C"
                     go to let-arc-gxc-100
           else if   w-let-arc-gxc-tip    =    "F"
                     go to let-arc-gxc-200
           else if   w-let-arc-gxc-tip    =    "L"
                     go to let-arc-gxc-300.
       let-arc-gxc-100.
      *              *-------------------------------------------------*
      *              * Se tipo elemento : Comune                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se codice a zero                       *
      *                  *---------------------------------------------*
           if        w-let-arc-gxc-cmn    =    zero
                     go to let-arc-gxc-400.
      *                  *---------------------------------------------*
      *                  * Lettura Comune                              *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCFL    "         to   f-key                  .
           move      w-let-arc-gxc-cmn    to   rf-gxc-cod-cmn         .
           move      zero                 to   rf-gxc-cod-fzn         .
           move      zero                 to   rf-gxc-cod-lct         .
           move      "pgm/geo/fls/ioc/obj/iofgxc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxc                 .
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda se record esistente    *
      *                  * oppure no                                   *
      *                  *---------------------------------------------*
           if        f-sts                =    e-not-err
                     go to let-arc-gxc-600
           else      go to let-arc-gxc-500.
       let-arc-gxc-200.
      *              *-------------------------------------------------*
      *              * Se tipo elemento : Frazione                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se codice a zero                       *
      *                  *---------------------------------------------*
           if        w-let-arc-gxc-cmn    =    zero or
                     w-let-arc-gxc-fzn    =    zero
                     go to let-arc-gxc-400.
      *                  *---------------------------------------------*
      *                  * Lettura Frazione                            *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCFL    "         to   f-key                  .
           move      w-let-arc-gxc-cmn    to   rf-gxc-cod-cmn         .
           move      w-let-arc-gxc-fzn    to   rf-gxc-cod-fzn         .
           move      zero                 to   rf-gxc-cod-lct         .
           move      "pgm/geo/fls/ioc/obj/iofgxc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxc                 .
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda se record esistente    *
      *                  * oppure no                                   *
      *                  *---------------------------------------------*
           if        f-sts                =    e-not-err
                     go to let-arc-gxc-600
           else      go to let-arc-gxc-500.
       let-arc-gxc-300.
      *              *-------------------------------------------------*
      *              * Se tipo elemento : Localita'                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se codice a zero                       *
      *                  *---------------------------------------------*
           if        w-let-arc-gxc-cmn    =    zero or
                     w-let-arc-gxc-lct    =    zero
                     go to let-arc-gxc-400.
      *                  *---------------------------------------------*
      *                  * Lettura Localita'                           *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCFL    "         to   f-key                  .
           move      w-let-arc-gxc-cmn    to   rf-gxc-cod-cmn         .
           move      w-let-arc-gxc-fzn    to   rf-gxc-cod-fzn         .
           move      w-let-arc-gxc-lct    to   rf-gxc-cod-lct         .
           move      "pgm/geo/fls/ioc/obj/iofgxc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxc                 .
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda se record esistente    *
      *                  * oppure no                                   *
      *                  *---------------------------------------------*
           if        f-sts                =    e-not-err
                     go to let-arc-gxc-600
           else      go to let-arc-gxc-500.
       let-arc-gxc-400.
      *              *-------------------------------------------------*
      *              * Se codice elemento a zero                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione work area                   *
      *                  *---------------------------------------------*
           move      spaces               to   w-let-arc-gxc-des      .
           move      spaces               to   w-let-arc-gxc-prv      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     let-arc-gxc-999.
       let-arc-gxc-500.
      *              *-------------------------------------------------*
      *              * Se codice elemento non esistente                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione work area                   *
      *                  *---------------------------------------------*
           move      all   "."            to   w-let-arc-gxc-des      .
           move      spaces               to   w-let-arc-gxc-prv      .
      *                  *---------------------------------------------*
      *                  * Flag di uscita a non trovato                *
      *                  *---------------------------------------------*
           move      "#"                  to   w-let-arc-gxc-flg      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     let-arc-gxc-999.
       let-arc-gxc-600.
      *              *-------------------------------------------------*
      *              * Se codice elemento esistente                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Bufferizzazione in work area                *
      *                  *---------------------------------------------*
           move      rf-gxc-des-cfl       to   w-let-arc-gxc-des      .
           move      rf-gxc-cod-prv       to   w-let-arc-gxc-prv      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     let-arc-gxc-999.
       let-arc-gxc-999.
           exit.

      *    *===========================================================*
      *    * Routine lettura archivio [age]                            *
      *    *-----------------------------------------------------------*
       let-arc-age-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-age-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice agente a zero                    *
      *              *-------------------------------------------------*
           if        w-let-arc-age-cod    =    zero
                     go to let-arc-age-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
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
           if        f-sts                not  = e-not-err
                     go to let-arc-age-400.
       let-arc-age-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-age-nom-age       to   w-let-arc-age-nom      .
           move      rf-age-sup-age       to   w-let-arc-age-sup      .
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
       let-arc-age-999.
           exit.

      *    *===========================================================*
      *    * Lettura archivio [ags]                                    *
      *    *-----------------------------------------------------------*
       let-arc-ags-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione valore di uscita                *
      *              *-------------------------------------------------*
           move      zero                 to   w-let-arc-ags-sup      .
      *              *-------------------------------------------------*
      *              * Start su file [ags]                             *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "AGESUP    "         to   f-key                  .
           move      w-let-arc-ags-age    to   rf-ags-cod-age         .
           move      zero                 to   rf-ags-num-prg         .
           move      "pgm/age/fls/ioc/obj/iofags"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ags                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata : ad uscita                 *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to let-arc-ags-900.
       let-arc-ags-100.
      *              *-------------------------------------------------*
      *              * Next su [ags]                                   *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/age/fls/ioc/obj/iofags"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ags                 .
      *                  *---------------------------------------------*
      *                  * Se 'at end' : ad uscita                     *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to let-arc-ags-900.
       let-arc-ags-200.
      *              *-------------------------------------------------*
      *              * Max su [ags], se non superato : ad uscita       *
      *              *-------------------------------------------------*
           if        rf-ags-cod-age       not  = w-let-arc-ags-age
                     go to let-arc-ags-900.
       let-arc-ags-400.
      *              *-------------------------------------------------*
      *              * Bufferizzazione dell'elemento letto             *
      *              *-------------------------------------------------*
           move      rf-ags-sup-age       to   w-let-arc-ags-sup      .
       let-arc-ags-500.
      *              *-------------------------------------------------*
      *              * Riciclo a record [ags] successivo               *
      *              *-------------------------------------------------*
           go to     let-arc-ags-100.
       let-arc-ags-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-ags-999.
       let-arc-ags-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [zpv]                         *
      *    *-----------------------------------------------------------*
       let-arc-zpv-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zpv-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice sconto a zero                    *
      *              *-------------------------------------------------*
           if        w-let-arc-zpv-cod    =    zero
                     go to let-arc-zpv-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCPV    "         to   f-key                  .
           move      w-let-arc-zpv-tip    to   rf-zpv-tip-cpv         .
           move      w-let-arc-zpv-cod    to   rf-zpv-cod-cpv         .
           move      "pgm/age/fls/ioc/obj/iofzpv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zpv                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-zpv-400.
       let-arc-zpv-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-zpv-des-cpv       to   w-let-arc-zpv-des      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-zpv-999.
       let-arc-zpv-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-zpv-flg      .
           move      all   "."            to   w-let-arc-zpv-des      .
           go to     let-arc-zpv-999.
       let-arc-zpv-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zpv-des      .
       let-arc-zpv-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [pdc]                         *
      *    *-----------------------------------------------------------*
       let-arc-pdc-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-pdc-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice sottoconto a zero                *
      *              *-------------------------------------------------*
           if        w-let-arc-pdc-cod    =    zero
                     go to let-arc-pdc-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODPDC"             to   f-key                  .
           move      w-let-arc-pdc-cod    to   rf-pdc-cod-pdc         .
           move      "pgm/cge/fls/ioc/obj/iofpdc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdc                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-pdc-400.
       let-arc-pdc-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-pdc-des-pdc       to   w-let-arc-pdc-des      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-pdc-999.
       let-arc-pdc-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-pdc-flg      .
           move      all   "."            to   w-let-arc-pdc-des      .
           go to     let-arc-pdc-999.
       let-arc-pdc-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-pdc-des      .
       let-arc-pdc-999.
           exit.

      *    *===========================================================*
      *    * Editing del codice sottoconto con appoggio a sx o dx      *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/wedtpdc0.wks"                   .

      *    *===========================================================*
      *    * Subroutines di controllo formale Partita Iva e Codice     *
      *    * Fiscale                                                   *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/wpivcfi0.wks"                   .

      *    *===========================================================*
      *    * Routine di attribuzione codice automatico progressivo     *
      *    *-----------------------------------------------------------*
       att-cod-aut-000.
      *              *-------------------------------------------------*
      *              * Lettura codice automatico per [age]             *
      *              *-------------------------------------------------*
           move      "Eg"                 to   s-ope                  .
           move      "age "               to   s-nam                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-sts                =    spaces
                     go to att-cod-aut-400.
       att-cod-aut-200.
      *              *-------------------------------------------------*
      *              * Record non esistente                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Scrittura record normalizzato               *
      *                  *---------------------------------------------*
           move      "Ep"                 to   s-ope                  .
           move      "age "               to   s-nam                  .
           move      zero                 to   s-num                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Ripetizione dell'intera operazione          *
      *                  *---------------------------------------------*
           go to     att-cod-aut-000.
       att-cod-aut-400.
      *              *-------------------------------------------------*
      *              * Record esistente                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Memorizzazione del valore pre incremento    *
      *                  *---------------------------------------------*
           move      s-num                to   w-enc-age-val-pre      .
      *                  *---------------------------------------------*
      *                  * Incremento del valore                       *
      *                  *---------------------------------------------*
           move      w-enc-age-val-pre    to   w-enc-age-val-pos      .
           add       1                    to   w-enc-age-val-pos      .
       att-cod-aut-500.
      *                  *---------------------------------------------*
      *                  * Se l'incremento porta a zero si forza il    *
      *                  * valore a 1                                  *
      *                  *---------------------------------------------*
           if        w-enc-age-val-pos    =    zero
                     move  1              to   w-enc-age-val-pos      .
      *                  *---------------------------------------------*
      *                  * Se raggiunto il massimo valore impostabile  *
      *                  * si ricicla da 1                             *
      *                  *---------------------------------------------*
           if        w-enc-age-val-pos    >    w-enc-age-val-max
                     move  1              to   w-enc-age-val-pos      .
      *                  *---------------------------------------------*
      *                  * Controllo se esiste gia' un record con il   *
      *                  * codice pari al valore incrementato          *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODAGE    "         to   f-key                  .
           move      w-enc-age-val-pos    to   rf-age-cod-age         .
           move      "pgm/age/fls/ioc/obj/iofage"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-age                 .
           if        f-sts                =    e-not-err
                     go to att-cod-aut-600
           else      go to att-cod-aut-700.
       att-cod-aut-600.
      *                  *---------------------------------------------*
      *                  * Se esiste gia' un record con il codice pari *
      *                  * al valore incrementato                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Ulteriore incremento del valore         *
      *                      *-----------------------------------------*
           add       1                    to   w-enc-age-val-pos      .
      *                      *-----------------------------------------*
      *                      * Riciclo a controllo di esistenza        *
      *                      *-----------------------------------------*
           go to     att-cod-aut-500.
       att-cod-aut-700.
      *                  *---------------------------------------------*
      *                  * Se non esiste gia' un record con il codice  *
      *                  * pari al valore incrementato                 *
      *                  *---------------------------------------------*
           move      "Eu"                 to   s-ope                  .
           move      "age "               to   s-nam                  .
           move      w-enc-age-val-pos    to   s-num                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                      *-----------------------------------------*
      *                      * Se errori ripete l'intera operazione di *
      *                      * attribuzione                            *
      *                      *-----------------------------------------*
           if        s-sts                not  = spaces
                     go to att-cod-aut-000.
       att-cod-aut-999.
           exit.

      *    *===========================================================*
      *    * Routine di ripristino codice automatico progressivo       *
      *    *-----------------------------------------------------------*
       rip-cod-aut-000.
      *              *-------------------------------------------------*
      *              * Lettura codice automatico per [age]             *
      *              *-------------------------------------------------*
           move      "Eg"                 to   s-ope                  .
           move      "age "               to   s-nam                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-sts                =    spaces
                     go to rip-cod-aut-400.
       rip-cod-aut-200.
      *              *-------------------------------------------------*
      *              * Se record non esistente                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     rip-cod-aut-999.
       rip-cod-aut-400.
      *              *-------------------------------------------------*
      *              * Se record esistente                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Confronto tra il valore attuale ed il valo- *
      *                  * re post incremento                          *
      *                  *---------------------------------------------*
           if        s-num                =    w-enc-age-val-pos
                     go to rip-cod-aut-600.
       rip-cod-aut-500.
      *                  *---------------------------------------------*
      *                  * Se il valore attuale non e' uguale al valo- *
      *                  * re post incremento                          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Unlock                                  *
      *                      *-----------------------------------------*
           move      "Er"                 to   s-ope                  .
           move      "age "               to   s-nam                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     rip-cod-aut-999.
       rip-cod-aut-600.
      *                  *---------------------------------------------*
      *                  * Se il valore attuale e' uguale al valore    *
      *                  * post incremento                             *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Aggiornamento del codice automatico con *
      *                      * il valore pre incremento                *
      *                      *-----------------------------------------*
           move      "Eu"                 to   s-ope                  .
           move      "age "               to   s-nam                  .
           move      w-enc-age-val-pre    to   s-num                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                      *-----------------------------------------*
      *                      * Se errori ripete l'intera operazione di *
      *                      * attribuzione                            *
      *                      *-----------------------------------------*
           if        s-sts                not  = spaces
                     go to rip-cod-aut-000.
       rip-cod-aut-999.
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
      *    * Subroutines per accettazione codice dipendenza dell'a-    *
      *    * zienda                                                    *
      *    *-----------------------------------------------------------*
           copy      "pgm/azi/prg/cpy/acoddpz0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione codice agente              *
      *    *-----------------------------------------------------------*
           copy      "pgm/age/prg/cpy/acmnage0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice nazione         *
      *    *-----------------------------------------------------------*
           copy      "pgm/geo/prg/cpy/acdenaz0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione Indirizzo + Citta'         *
      *    *-----------------------------------------------------------*
           copy      "pgm/geo/prg/cpy/aiplgeo0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione C.a.p. e citta'            *
      *    *-----------------------------------------------------------*
           copy      "pgm/geo/prg/cpy/acecgeo0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione codice Comune 'geo'        *
      *    *-----------------------------------------------------------*
           copy      "pgm/geo/prg/cpy/acomgeo0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione codice Frazione 'geo'      *
      *    *-----------------------------------------------------------*
           copy      "pgm/geo/prg/cpy/afrageo0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione codice Localita' 'geo'     *
      *    *-----------------------------------------------------------*
           copy      "pgm/geo/prg/cpy/alocgeo0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice categoria prov- *
      *    * vigioni                                                   *
      *    *-----------------------------------------------------------*
           copy      "pgm/age/prg/cpy/acmnzpv1.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice sottoconto      *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/acmnpdc0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per determinazione contatti                   *
      *    *-----------------------------------------------------------*
           copy      "pgm/azi/prg/cpy/dconarc0.dts"                   .

