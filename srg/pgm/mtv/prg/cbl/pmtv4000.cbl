       Identification Division.
       Program-Id.                                 pmtv4000           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    mtv                 *
      *                                Settore:    gst                 *
      *                                   Fase:    mtv400              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 03/11/92    *
      *                       Ultima revisione:    NdK del 29/04/10    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Gestione anagrafica materie varie           *
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
                     "mtv"                                            .
      *        *-------------------------------------------------------*
      *        * Settore gestionale                                    *
      *        *-------------------------------------------------------*
           05  i-ide-set                  pic  x(03) value
                     "gst"                                            .
      *        *-------------------------------------------------------*
      *        * Fase gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-fas                  pic  x(06) value
                     "mtv400"                                         .
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10) value
                     "pmtv4000"                                       .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  i-ide-des                  pic  x(40) value
                     "     DATI ANAGRAFICI MATERIALI VARI     "       .

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
      *            *---------------------------------------------------*
      *            * Per routine pre-snx-del-000                       *
      *            *---------------------------------------------------*
               10  w-cnt-pre-snx-del      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Per routine pos-snx-del-000                       *
      *            *---------------------------------------------------*
               10  w-cnt-pos-snx-del      pic  x(01)                  .
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
      *        *-------------------------------------------------------*
      *        * Area di controllo per duplicazione record             *
      *        *-------------------------------------------------------*
           05  w-cnt-dup.
               10  w-cnt-dup-rec-flg      pic  x(01)                  .

      *    *===========================================================*
      *    * Record files                                              *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [mtv]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/mtv/fls/rec/rfmtv"                          .
      *        *-------------------------------------------------------*
      *        * [zv1]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/mtv/fls/rec/rfzv1"                          .
      *        *-------------------------------------------------------*
      *        * [zv2]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/mtv/fls/rec/rfzv2"                          .
      *        *-------------------------------------------------------*
      *        * [zv3]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/mtv/fls/rec/rfzv3"                          .
      *        *-------------------------------------------------------*
      *        * [zum]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfzum"                          .
      *        *-------------------------------------------------------*
      *        * [zvs]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/mtv/fls/rec/rfzvs"                          .

      *    *===========================================================*
      *    * Work-area per bufferizzazione testata                     *
      *    *-----------------------------------------------------------*
       01  w-tes.
      *        *-------------------------------------------------------*
      *        * Valori chiave                                         *
      *        *-------------------------------------------------------*
           05  w-tes-val-key.
               10  w-tes-num-mtv          pic  9(07)                  .
               10  w-tes-num-mtv-aut      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Valori attuali e precedenti                           *
      *        *-------------------------------------------------------*
           05  w-tes-val-aep occurs 2.
               10  w-tes-ide-dat          pic  9(07)                  .
               10  w-tes-ide-ute          pic  x(08)                  .
               10  w-tes-ide-fas          pic  x(06)                  .
               10  w-tes-alf-mtv          pic  x(14)                  .
               10  w-tes-syn-mtv          pic  x(13)                  .
               10  w-tes-des-key          pic  x(40)                  .
               10  w-tes-des-int          pic  x(40)                  .
               10  w-tes-cla-mtv          pic  9(05)                  .
               10  w-tes-cla-mtv-des      pic  x(40)                  .
               10  w-tes-cla-mtv-sud      pic  9(02)                  .
               10  w-tes-cla-mtv-umi      pic  x(03)                  .
               10  w-tes-gru-mtv          pic  9(05)                  .
               10  w-tes-gru-mtv-des      pic  x(40)                  .
               10  w-tes-gru-mtv-sud      pic  9(02)                  .
               10  w-tes-gru-mtv-umi      pic  x(03)                  .
               10  w-tes-sgr-mtv          pic  9(05)                  .
               10  w-tes-sgr-mtv-des      pic  x(40)                  .
               10  w-tes-sgr-mtv-sud      pic  9(02)                  .
               10  w-tes-sgr-mtv-umi      pic  x(03)                  .
               10  w-tes-tip-mtv          pic  9(02)                  .
               10  w-tes-snx-mag          pic  x(01)                  .
               10  w-tes-snx-mac          pic  x(01)                  .
               10  w-tes-tip-cfz          pic  9(02)                  .
               10  w-tes-qta-cfz          pic  9(06)v9(03)            .
               10  w-tes-pes-uni          pic  9(06)v9(03)            .
               10  w-tes-pes-tar          pic  9(06)v9(03)            .
               10  w-tes-vol-uni          pic  9(06)v9(03)            .
               10  w-tes-dim-mtv.
                   15  w-tes-dim-lar      pic  9(06)v9(03)            .
                   15  w-tes-dim-alt      pic  9(06)v9(03)            .
                   15  w-tes-dim-prf      pic  9(06)v9(03)            .
               10  w-tes-pcl-fis          pic  x(10)                  .
               10  w-tes-coe-mol          pic  9(04)v9(03)            .
               10  w-tes-coe-div          pic  9(04)v9(03)            .
               10  w-tes-umi-gst          pic  x(03)                  .
               10  w-tes-umi-gst-des      pic  x(20)                  .
               10  w-tes-dec-qta          pic  9(01)                  .
               10  w-tes-snx-2qt          pic  9(01)                  .
               10  w-tes-dec-2qt          pic  9(01)                  .
               10  w-tes-snx-3qt          pic  9(01)                  .
               10  w-tes-dec-3qt          pic  9(01)                  .
               10  w-tes-tip-vpr          pic  x(03)                  .
               10  w-tes-tip-vpr-des      pic  x(10)                  .
               10  w-tes-cod-s01          pic  9(05)                  .
               10  w-tes-cod-s01-des      pic  x(20)                  .
               10  w-tes-cod-s02          pic  9(05)                  .
               10  w-tes-cod-s02-des      pic  x(20)                  .
               10  w-tes-cod-s03          pic  9(05)                  .
               10  w-tes-cod-s03-des      pic  x(20)                  .
               10  w-tes-cla-bdg          pic  9(05)                  .
               10  w-tes-alx-exp.
                   15  filler occurs 80   pic  x(01)                  .

      *    *===========================================================*
      *    * Work per subroutines di Let                               *
      *    *-----------------------------------------------------------*
       01  w-let.
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [zv1]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-zv1.
               10  w-let-arc-zv1-flg      pic  x(01)                  .
               10  w-let-arc-zv1-cla      pic  9(05)                  .
               10  w-let-arc-zv1-des      pic  x(40)                  .
               10  w-let-arc-zv1-sud      pic  9(02)                  .
               10  w-let-arc-zv1-umi      pic  x(03)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [zv2]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-zv2.
               10  w-let-arc-zv2-flg      pic  x(01)                  .
               10  w-let-arc-zv2-cla      pic  9(05)                  .
               10  w-let-arc-zv2-gru      pic  9(05)                  .
               10  w-let-arc-zv2-des      pic  x(40)                  .
               10  w-let-arc-zv2-sud      pic  9(02)                  .
               10  w-let-arc-zv2-umi      pic  x(03)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [zv3]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-zv3.
               10  w-let-arc-zv3-flg      pic  x(01)                  .
               10  w-let-arc-zv3-cla      pic  9(05)                  .
               10  w-let-arc-zv3-gru      pic  9(05)                  .
               10  w-let-arc-zv3-sgr      pic  9(05)                  .
               10  w-let-arc-zv3-des      pic  x(40)                  .
               10  w-let-arc-zv3-sud      pic  9(02)                  .
               10  w-let-arc-zv3-umi      pic  x(03)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [zum]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-zum.
               10  w-let-arc-zum-flg      pic  x(01)                  .
               10  w-let-arc-zum-cod      pic  x(03)                  .
               10  w-let-arc-zum-des      pic  x(20)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [zvs]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-zvs.
               10  w-let-arc-zvs-flg      pic  x(01)                  .
               10  w-let-arc-zvs-tip      pic  9(02)                  .
               10  w-let-arc-zvs-cod      pic  9(05)                  .
               10  w-let-arc-zvs-des      pic  x(20)                  .

      *    *===========================================================*
      *    * Work-area personalizzazioni                               *
      *    *-----------------------------------------------------------*
       01  w-prs.
      *        *-------------------------------------------------------*
      *        * Si/No gestione magazzino fisico e contabile           *
      *        *-------------------------------------------------------*
           05  w-prs-snx-mfc.
               10  w-prs-snx-mfm          pic  x(01)                  .
               10  w-prs-snx-mfd          pic  x(01)                  .
               10  w-prs-snx-mcm          pic  x(01)                  .
               10  w-prs-snx-mcd          pic  x(01)                  .

      *    *===========================================================*
      *    * Work per subroutines di Ctl                               *
      *    *-----------------------------------------------------------*
       01  w-ctl.
      *        *-------------------------------------------------------*
      *        * Work per Ctl su unicita' codice materia varia alfa-   *
      *        * numerica                                              *
      *        *-------------------------------------------------------*
           05  w-ctl-uni-alf.
               10  w-ctl-uni-alf-flg      pic  x(01)                  .
               10  w-ctl-uni-alf-alf      pic  x(14)                  .
               10  w-ctl-uni-alf-num      pic  9(07)                  .
               10  w-ctl-uni-alf-ctr      pic  9(02)                  .

      *    *===========================================================*
      *    * Work-area per contatori e indici                          *
      *    *-----------------------------------------------------------*
       01  w-cix.
      *        *-------------------------------------------------------*
      *        * Contatore per percentuali                             *
      *        *-------------------------------------------------------*
           05  w-cix-ctr-001              pic  9(03)                  .

      *    *===========================================================*
      *    * Work-area per salvataggi                                  *
      *    *-----------------------------------------------------------*
       01  w-sav.
      *        *-------------------------------------------------------*
      *        * Salvataggio per : Descrizione interna                 *
      *        *-------------------------------------------------------*
           05  w-sav-des-int              pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Salvataggio per : Classe merceologica                 *
      *        *-------------------------------------------------------*
           05  w-sav-cla-mtv              pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Salvataggio per : Gruppo merceologico                 *
      *        *-------------------------------------------------------*
           05  w-sav-gru-mtv              pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Salvataggio per : Sottoruppo merceologico             *
      *        *-------------------------------------------------------*
           05  w-sav-sgr-mtv              pic  9(05)                  .

      *    *===========================================================*
      *    * Work per accettazioni campi espansi                       *
      *    *-----------------------------------------------------------*
       01  w-exp.
      *        *-------------------------------------------------------*
      *        * Work per : Si/no in confezione                        *
      *        *-------------------------------------------------------*
           05  w-exp-tip-cfz.
               10  w-exp-tip-cfz-num      pic  9(02)       value 02   .
               10  w-exp-tip-cfz-lun      pic  9(02)       value 02   .
               10  w-exp-tip-cfz-tbl.
                   15  filler             pic  x(02) value "No"       .
                   15  filler             pic  x(02) value "Si"       .
      *        *-------------------------------------------------------*
      *        * Work per : Si/no gestione magazzino fisico            *
      *        *-------------------------------------------------------*
           05  w-exp-snx-mag.
               10  w-exp-snx-mag-num      pic  9(02)       value 02   .
               10  w-exp-snx-mag-lun      pic  9(02)       value 02   .
               10  w-exp-snx-mag-tbl.
                   15  filler             pic  x(02) value "Si"       .
                   15  filler             pic  x(02) value "No"       .
      *        *-------------------------------------------------------*
      *        * Work per : Si/no gestione magazzino contabile         *
      *        *-------------------------------------------------------*
           05  w-exp-snx-mac.
               10  w-exp-snx-mac-num      pic  9(02)       value 02   .
               10  w-exp-snx-mac-lun      pic  9(02)       value 02   .
               10  w-exp-snx-mac-tbl.
                   15  filler             pic  x(02) value "Si"       .
                   15  filler             pic  x(02) value "No"       .

      *    *===========================================================*
      *    * Link-area per accettazione codice materia varia 'mtv'     *
      *    *-----------------------------------------------------------*
           copy      "pgm/mtv/prg/cpy/acodmtv0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice classe materia varia    *
      *    *-----------------------------------------------------------*
           copy      "pgm/mtv/prg/cpy/acmnzv10.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice gruppo materia varia    *
      *    *-----------------------------------------------------------*
           copy      "pgm/mtv/prg/cpy/acmnzv20.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice sottogruppo mat. varia  *
      *    *-----------------------------------------------------------*
           copy      "pgm/mtv/prg/cpy/acmnzv30.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione unita' di misura               *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/acodzum0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione tipo variante                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/acodztv0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice statistico 1            *
      *    *-----------------------------------------------------------*
           copy      "pgm/mtv/prg/cpy/acmnzvs1.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice statistico 2            *
      *    *-----------------------------------------------------------*
           copy      "pgm/mtv/prg/cpy/acmnzvs2.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice statistico 3            *
      *    *-----------------------------------------------------------*
           copy      "pgm/mtv/prg/cpy/acmnzvs3.acl"                   .

      *    *===========================================================*
      *    * Work per subroutines di Err                               *
      *    *-----------------------------------------------------------*
       01  w-err.
      *        *-------------------------------------------------------*
      *        * Work per Err su controllo tasto Do non chiave         *
      *        *-------------------------------------------------------*
           05  w-err-tdo-nok.
               10  w-err-box-err-msg      pic  x(56)                  .

      *    *===========================================================*
      *    * Work per attribuzione e ripristino codice automatico      *
      *    *-----------------------------------------------------------*
       01  w-enc-mtv.
      *        *-------------------------------------------------------*
      *        * Valore pre incremento                                 *
      *        *-------------------------------------------------------*
           05  w-enc-mtv-val-pre          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Valore post incremento                                *
      *        *-------------------------------------------------------*
           05  w-enc-mtv-val-pos          pic  9(07)                  .

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
      *              * Tasto di funzione Pf4 :                         *
      *              *  - se Impostazione chiave    : non abilitato    *
      *              *  - se Inserimento            : non abilitato    *
      *              *  - se Visualizzazione        : abilitato        *
      *              *  - se Almeno una modifica    : non abilitato    *
      *              *  - altrimenti                : abilitato        *
      *              *-------------------------------------------------*
           if        w-cnt-mfu-tip-imp    =    "K"   or
                     w-cnt-mfu-tip-fun    =    "I"   or
                     w-cnt-acc-flg-aum    not  = spaces
                     go to exe-acc-cmp-080.
           move      "[4] "               to   v-pfk (18)             .
       exe-acc-cmp-080.
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
      *              * Routine pre-richiesta di ratifica tasto Delete  *
      *              *-------------------------------------------------*
           perform   pre-snx-del-000      thru pre-snx-del-999        .
      *              *-------------------------------------------------*
      *              * Se errore : a ripristino                        *
      *              *-------------------------------------------------*
           if        w-cnt-pre-snx-del    not  = spaces
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
      *              * Se non function key DELT si ripristina          *
      *              *-------------------------------------------------*
           if        v-key                not  = "DELT"
                     go to exe-acc-cmp-300.
      *              *-------------------------------------------------*
      *              * Routine post-richiesta di ratifica tasto Delete *
      *              *-------------------------------------------------*
           perform   pos-snx-del-000      thru pos-snx-del-999        .
      *              *-------------------------------------------------*
      *              * Se errore : a ripristino                        *
      *              *-------------------------------------------------*
           if        w-cnt-pre-snx-del    not  = spaces
                     go to exe-acc-cmp-300.
      *              *-------------------------------------------------*
      *              * Altrimenti : uscita                             *
      *              *-------------------------------------------------*
           go to     exe-acc-cmp-999.
       exe-acc-cmp-300.
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
      *              *-------------------------------------------------*
      *              * Set del flag di tasto Pf4                       *
      *              *-------------------------------------------------*
           if        v-key                not  = "[4] "
                     go to exe-acc-cmp-900.
      *                  *---------------------------------------------*
      *                  * Attivazione segnale di duplicazione         *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cnt-dup-rec-flg      .
      *                  *---------------------------------------------*
      *                  * Forzatura tasto 'Exit'                      *
      *                  *---------------------------------------------*
           move      "EXIT"               to   v-key                  .
       exe-acc-cmp-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
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
      *              *-------------------------------------------------*
      *              * Normalizzazione segnale di duplicazione         *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-dup-rec-flg      .
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione si/no gestione magaz- *
      *              * zino fisico o contabile                         *
      *              *-------------------------------------------------*
           perform   prs-snx-mfc-000      thru prs-snx-mfc-999        .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice materia varia   *
      *              *-------------------------------------------------*
           perform   cod-cod-mtv-opn-000  thru cod-cod-mtv-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice classe          *
      *              *-------------------------------------------------*
           perform   cod-mne-zv1-opn-000  thru cod-mne-zv1-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice gruppo          *
      *              *-------------------------------------------------*
           perform   cod-mne-zv2-opn-000  thru cod-mne-zv2-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice sottogruppo     *
      *              *-------------------------------------------------*
           perform   cod-mne-zv3-opn-000  thru cod-mne-zv3-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione unita' di misura       *
      *              *-------------------------------------------------*
           perform   cod-cod-zum-opn-000  thru cod-cod-zum-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice statistico 1    *
      *              *-------------------------------------------------*
           perform   cmn-zvs-001-opn-000  thru cmn-zvs-001-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice statistico 2    *
      *              *-------------------------------------------------*
           perform   cmn-zvs-002-opn-000  thru cmn-zvs-002-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice statistico 3    *
      *              *-------------------------------------------------*
           perform   cmn-zvs-003-opn-000  thru cmn-zvs-003-opn-999    .
       pre-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Routine post-esecuzione programma                         *
      *    *-----------------------------------------------------------*
       pos-exe-pgm-000.
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice materia varia  *
      *              *-------------------------------------------------*
           perform   cod-cod-mtv-cls-000  thru cod-cod-mtv-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice classe         *
      *              *-------------------------------------------------*
           perform   cod-mne-zv1-cls-000  thru cod-mne-zv1-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice gruppo         *
      *              *-------------------------------------------------*
           perform   cod-mne-zv2-cls-000  thru cod-mne-zv2-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice sottogruppo    *
      *              *-------------------------------------------------*
           perform   cod-mne-zv3-cls-000  thru cod-mne-zv3-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione unita' di misura      *
      *              *-------------------------------------------------*
           perform   cod-cod-zum-cls-000  thru cod-cod-zum-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice statistico 1   *
      *              *-------------------------------------------------*
           perform   cmn-zvs-001-cls-000  thru cmn-zvs-001-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice statistico 2   *
      *              *-------------------------------------------------*
           perform   cmn-zvs-002-cls-000  thru cmn-zvs-002-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice statistico 3   *
      *              *-------------------------------------------------*
           perform   cmn-zvs-003-cls-000  thru cmn-zvs-003-cls-999    .
       pos-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Lettura delle personalizzazioni per si/no gestione magaz- *
      *    * zino fisico e contabile                                   *
      *    *-----------------------------------------------------------*
       prs-snx-mfc-000.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione                       *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/mtv/mtv400[snx-mfc]"
                                          to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Deviazione in funzione dell'esito della lettura *
      *              *-------------------------------------------------*
           if        s-ves                =    spaces
                     move  s-alf          to   w-prs-snx-mfc
                     go to prs-snx-mfc-100.
      *                  *---------------------------------------------*
      *                  * Normalizzazione personalizzazione           *
      *                  *---------------------------------------------*
           move      "MNMN"               to   w-prs-snx-mfc          .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     prs-snx-mfc-999.
       prs-snx-mfc-100.
      *                  *---------------------------------------------*
      *                  * Se personalizzazione trovata : controlli e  *
      *                  * normalizzazioni                             *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se 'no' gestione magazzino fisico si    *
      *                      * forza a 'no' la gestione magazzino con- *
      *                      * tabile solo se il valore e' fisso       *
      *                      *-----------------------------------------*
           if        w-prs-snx-mcm        =    "M"
                     go to prs-snx-mfc-999.
           if        w-prs-snx-mfd        =    "N"
                     move  "N"            to   w-prs-snx-mcd          .
       prs-snx-mfc-999.
           exit.

      *    *===========================================================*
      *    * Open files                                                *
      *    *-----------------------------------------------------------*
       rou-opn-fls-000.
      *              *-------------------------------------------------*
      *              * [mtv]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/mtv/fls/ioc/obj/iofmtv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mtv                 .
      *              *-------------------------------------------------*
      *              * [zv1]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/mtv/fls/ioc/obj/iofzv1"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zv1                 .
      *              *-------------------------------------------------*
      *              * [zv2]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/mtv/fls/ioc/obj/iofzv2"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zv2                 .
      *              *-------------------------------------------------*
      *              * [zv3]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/mtv/fls/ioc/obj/iofzv3"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zv3                 .
      *              *-------------------------------------------------*
      *              * [zum]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofzum"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zum                 .
      *              *-------------------------------------------------*
      *              * [zvs]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/mtv/fls/ioc/obj/iofzvs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zvs                 .
       rou-opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Close files                                               *
      *    *-----------------------------------------------------------*
       rou-cls-fls-000.
      *              *-------------------------------------------------*
      *              * [mtv]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/mtv/fls/ioc/obj/iofmtv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mtv                 .
      *              *-------------------------------------------------*
      *              * [zv1]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/mtv/fls/ioc/obj/iofzv1"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zv1                 .
      *              *-------------------------------------------------*
      *              * [zv2]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/mtv/fls/ioc/obj/iofzv2"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zv2                 .
      *              *-------------------------------------------------*
      *              * [zv3]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/mtv/fls/ioc/obj/iofzv3"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zv3                 .
      *              *-------------------------------------------------*
      *              * [zum]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofzum"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zum                 .
      *              *-------------------------------------------------*
      *              * [zvs]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/mtv/fls/ioc/obj/iofzvs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zvs                 .
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
      *                  * Numero progressivo interno                  *
      *                  *---------------------------------------------*
           perform   acc-prg-mtv-000      thru acc-prg-mtv-999        .
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
      *              * Numero progressivo interno                      *
      *              *-------------------------------------------------*
           perform   vis-prg-mtv-000      thru vis-prg-mtv-999        .
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
      *              * Numero progressivo interno                      *
      *              *-------------------------------------------------*
           perform   pmt-prg-mtv-000      thru pmt-prg-mtv-999        .
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
      *    * Visualizzazione prompts per Numero progressivo interno    *
      *    *-----------------------------------------------------------*
       pmt-prg-mtv-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Numero progressivo interno :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-prg-mtv-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo chiave : Numero progressivo interno    *
      *    *                             per la materia varia          *
      *    *-----------------------------------------------------------*
       acc-prg-mtv-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-prg-mtv-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-cod-mtv-ope      .
           move      "N"                  to   w-cod-cod-mtv-tac      .
           move      w-tes-num-mtv        to   w-cod-cod-mtv-num      .
           move      spaces               to   w-cod-cod-mtv-alf      .
           move      04                   to   w-cod-cod-mtv-lin      .
           move      30                   to   w-cod-cod-mtv-pos      .
           move      08                   to   w-cod-cod-mtv-dln      .
           move      30                   to   w-cod-cod-mtv-dps      .
           move      spaces               to   v-edm                  .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-cod-mtv-cll-000  thru cod-cod-mtv-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-cod-mtv-foi-000  thru cod-cod-mtv-foi-999    .
       acc-prg-mtv-110.
           perform   cod-cod-mtv-cll-000  thru cod-cod-mtv-cll-999    .
           if        w-cod-cod-mtv-ope    =    "F+"
                     go to acc-prg-mtv-115.
           if        w-cod-cod-mtv-ope    =    "AC"
                     go to acc-prg-mtv-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-prg-mtv-115.
           perform   cod-cod-mtv-foi-000  thru cod-cod-mtv-foi-999    .
           go to     acc-prg-mtv-110.
       acc-prg-mtv-120.
           move      w-cod-cod-mtv-num    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-prg-mtv-999.
       acc-prg-mtv-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-num-mtv          .
       acc-prg-mtv-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda se il codice impostato *
      *                  * e' zero oppure diverso da zero              *
      *                  *---------------------------------------------*
           if        w-tes-num-mtv        =    zero
                     go to acc-prg-mtv-450
           else      go to acc-prg-mtv-600.
       acc-prg-mtv-450.
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
                     go to acc-prg-mtv-452.
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
           go to     acc-prg-mtv-100.
       acc-prg-mtv-452.
      *                      *-----------------------------------------*
      *                      * Attribuzione codice automatico progres- *
      *                      * sivo                                    *
      *                      *-----------------------------------------*
           perform   att-cod-aut-000      thru att-cod-aut-999        .
      *                      *-----------------------------------------*
      *                      * Codice automatico in campo di destina-  *
      *                      *-----------------------------------------*
           move      w-enc-mtv-val-pos    to   w-tes-num-mtv          .
      *                      *-----------------------------------------*
      *                      * Segnale di attribuzione codice esegui-  *
      *                      * ta automaticamente                      *
      *                      *-----------------------------------------*
           move      "#"                  to   w-tes-num-mtv-aut      .
      *                      *-----------------------------------------*
      *                      * Visualizzazione del codice              *
      *                      *-----------------------------------------*
           perform   vis-prg-mtv-000      thru vis-prg-mtv-999        .
      *                      *-----------------------------------------*
      *                      * Prosecuzione                            *
      *                      *-----------------------------------------*
           go to     acc-prg-mtv-600.
       acc-prg-mtv-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-prg-mtv-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                not  = "DO  "
                     go to acc-prg-mtv-999.
           perform   cnt-key-vuo-000      thru cnt-key-vuo-999        .
           if        w-cnt-key-vuo-flg    not  = spaces
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-prg-mtv-999.
           perform   cnt-tdo-key-000      thru cnt-tdo-key-999        .
           if        w-cnt-tdo-key-flg    not  = spaces
                     go to acc-prg-mtv-100
           else      move  "S"            to   w-cnt-tus-acc-key
                     go to acc-prg-mtv-999.
       acc-prg-mtv-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo chiave : Numero progressivo interno *
      *    *                                per la materia varia       *
      *    *-----------------------------------------------------------*
       vis-prg-mtv-000.
      *              *-------------------------------------------------*
      *              * Se valore a zero : tutto a Spaces               *
      *              *-------------------------------------------------*
           if        w-tes-num-mtv        not  = zero
                     go to vis-prg-mtv-500.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      14                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      04                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           go to     vis-prg-mtv-999.
       vis-prg-mtv-500.
      *              *-------------------------------------------------*
      *              * Se valore a non-zero : editing tra ()           *
      *              *-------------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<"                  to   v-edm                  .
           move      w-tes-num-mtv        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      14                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      04                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           string    "("        delimited by   size
                     v-edt      delimited by   spaces
                     ")"        delimited by   size
                                          into v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-prg-mtv-999.
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
      *              * La testata e' composta di nr. 3 pagine          *
      *              *-------------------------------------------------*
           move      3                    to   w-cnt-sts-imp-mpt      .
       dmp-tes-reg-999.
           exit.

      *    *===========================================================*
      *    * Determinazione si/no pagina w-cnt-sts-imp-npt da trattare *
      *    *-----------------------------------------------------------*
       snp-tes-reg-000.
      *              *-------------------------------------------------*
      *              * Flag di uscita a Si                             *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-sts-imp-snp      .
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del numero pagina        *
      *              *-------------------------------------------------*
           go to     snp-tes-reg-100
                     snp-tes-reg-200
                     snp-tes-reg-300
                     depending            on   w-cnt-sts-imp-npt      .
           go to     snp-tes-reg-999.
       snp-tes-reg-100.
      *              *-------------------------------------------------*
      *              * Pagina numero 1                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Sempre gestita                              *
      *                  *---------------------------------------------*
           go to     snp-tes-reg-999.
       snp-tes-reg-200.
      *              *-------------------------------------------------*
      *              * Pagina numero 2                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Sempre gestita                              *
      *                  *---------------------------------------------*
           go to     snp-tes-reg-999.
       snp-tes-reg-300.
      *              *-------------------------------------------------*
      *              * Pagina numero 3                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Sempre gestita                              *
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
                     depending            on   w-cnt-sts-imp-npt      .
           go to     acc-tes-reg-999.
       acc-tes-reg-100.
      *                  *---------------------------------------------*
      *                  * Normalizzazione func-key di impostazione    *
      *                  *---------------------------------------------*
           move      spaces               to   v-key                  .
      *                  *---------------------------------------------*
      *                  * Codice materia varia                        *
      *                  *---------------------------------------------*
           perform   acc-cod-mtv-000      thru acc-cod-mtv-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
       acc-tes-reg-110.
      *                  *---------------------------------------------*
      *                  * Descrizione interna                         *
      *                  *---------------------------------------------*
           perform   acc-des-int-000      thru acc-des-int-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-100.
       acc-tes-reg-120.
      *                  *---------------------------------------------*
      *                  * Classe merceologica                         *
      *                  *---------------------------------------------*
           perform   acc-cla-mtv-000      thru acc-cla-mtv-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-110.
       acc-tes-reg-130.
      *                  *---------------------------------------------*
      *                  * Gruppo merceologico                         *
      *                  *---------------------------------------------*
           perform   acc-gru-mtv-000      thru acc-gru-mtv-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-120.
       acc-tes-reg-140.
      *                  *---------------------------------------------*
      *                  * Sottogruppo merceologico                    *
      *                  *---------------------------------------------*
           perform   acc-sgr-mtv-000      thru acc-sgr-mtv-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-130.
       acc-tes-reg-150.
      *                  *---------------------------------------------*
      *                  * Unita' di misura                            *
      *                  *---------------------------------------------*
           perform   acc-umi-gst-000      thru acc-umi-gst-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-140.
       acc-tes-reg-160.
      *                  *---------------------------------------------*
      *                  * Sinonimo                                    *
      *                  *---------------------------------------------*
           perform   acc-syn-mtv-000      thru acc-syn-mtv-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-150.
       acc-tes-reg-170.
      *                  *---------------------------------------------*
      *                  * Si/no magazzino fisico                      *
      *                  *---------------------------------------------*
           perform   acc-snx-mag-000      thru acc-snx-mag-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-160.
       acc-tes-reg-180.
      *                  *---------------------------------------------*
      *                  * Si/no magazzino contabile                   *
      *                  *---------------------------------------------*
           perform   acc-snx-mac-000      thru acc-snx-mac-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-170.
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
                     go to acc-tes-reg-180.
      *                  *---------------------------------------------*
      *                  * Fine Pagina                                 *
      *                  *---------------------------------------------*
           move      "+"                  to   w-cnt-tus-acc-tes      .
           go to     acc-tes-reg-999.
       acc-tes-reg-200.
      *                  *---------------------------------------------*
      *                  * Si/no in confezione                         *
      *                  *---------------------------------------------*
           perform   acc-tip-cfz-000      thru acc-tip-cfz-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     move  "-"            to   w-cnt-tus-acc-tes
                     go to acc-tes-reg-999.
       acc-tes-reg-210.
      *                  *---------------------------------------------*
      *                  * Quantita' per confezione                    *
      *                  *---------------------------------------------*
           perform   acc-qta-cfz-000      thru acc-qta-cfz-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-200.
       acc-tes-reg-220.
      *                  *---------------------------------------------*
      *                  * Peso unitario lordo                         *
      *                  *---------------------------------------------*
           perform   acc-pes-uni-000      thru acc-pes-uni-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-210.
       acc-tes-reg-230.
      *                  *---------------------------------------------*
      *                  * Peso tara                                   *
      *                  *---------------------------------------------*
           perform   acc-pes-tar-000      thru acc-pes-tar-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-220.
       acc-tes-reg-240.
      *                  *---------------------------------------------*
      *                  * Volume unitario                             *
      *                  *---------------------------------------------*
           perform   acc-vol-uni-000      thru acc-vol-uni-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-230.
       acc-tes-reg-250.
      *                  *---------------------------------------------*
      *                  * Larghezza                                   *
      *                  *---------------------------------------------*
           perform   acc-dim-lar-000      thru acc-dim-lar-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-240.
       acc-tes-reg-260.
      *                  *---------------------------------------------*
      *                  * Altezza                                     *
      *                  *---------------------------------------------*
           perform   acc-dim-alt-000      thru acc-dim-alt-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-250.
       acc-tes-reg-270.
      *                  *---------------------------------------------*
      *                  * Profondita'                                 *
      *                  *---------------------------------------------*
           perform   acc-dim-prf-000      thru acc-dim-prf-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-260.
       acc-tes-reg-280.
      *                  *---------------------------------------------*
      *                  * Peculiarita' fisiche                        *
      *                  *---------------------------------------------*
           perform   acc-pcl-fis-000      thru acc-pcl-fis-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-270.
       acc-tes-reg-290.
      *                  *---------------------------------------------*
      *                  * Coefficiente moltiplicatore                 *
      *                  *---------------------------------------------*
           perform   acc-coe-mol-000      thru acc-coe-mol-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-280.
       acc-tes-reg-295.
      *                  *---------------------------------------------*
      *                  * Coefficiente divisore                       *
      *                  *---------------------------------------------*
           perform   acc-coe-div-000      thru acc-coe-div-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-290.
      *                  *---------------------------------------------*
      *                  * Presa visione per pagina 2                  *
      *                  *---------------------------------------------*
           perform   acc-pre-vpg-000      thru acc-pre-vpg-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-295.
      *                  *---------------------------------------------*
      *                  * Fine Pagina                                 *
      *                  *---------------------------------------------*
           move      "+"                  to   w-cnt-tus-acc-tes      .
           go to     acc-tes-reg-999.
       acc-tes-reg-300.
      *                  *---------------------------------------------*
      *                  * Decimali quantita'                          *
      *                  *---------------------------------------------*
           perform   acc-dec-qta-000      thru acc-dec-qta-999        .
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
      *                  * Tipo variante                               *
      *                  *---------------------------------------------*
           perform   acc-tip-vpr-000      thru acc-tip-vpr-999        .
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
      *                  * Codice statistico 1                         *
      *                  *---------------------------------------------*
           perform   acc-cod-s01-000      thru acc-cod-s01-999        .
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
      *                  * Codice statistico 2                         *
      *                  *---------------------------------------------*
           perform   acc-cod-s02-000      thru acc-cod-s02-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-320.
       acc-tes-reg-340.
      *                  *---------------------------------------------*
      *                  * Codice statistico 3                         *
      *                  *---------------------------------------------*
           perform   acc-cod-s03-000      thru acc-cod-s03-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-330.
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
                     depending            on   w-cnt-sts-imp-npt      .
           go to     vis-tes-reg-999.
       vis-tes-reg-100.
      *              *-------------------------------------------------*
      *              * Codice materia varia                            *
      *              *-------------------------------------------------*
           perform   vis-cod-mtv-000      thru vis-cod-mtv-999        .
      *              *-------------------------------------------------*
      *              * Descrizione interna                             *
      *              *-------------------------------------------------*
           perform   vis-des-int-000      thru vis-des-int-999        .
      *              *-------------------------------------------------*
      *              * Classe merceologica                             *
      *              *-------------------------------------------------*
           perform   vis-cla-mtv-000      thru vis-cla-mtv-999        .
           perform   vis-des-cla-000      thru vis-des-cla-999        .
      *              *-------------------------------------------------*
      *              * Gruppo merceologico                             *
      *              *-------------------------------------------------*
           perform   vis-gru-mtv-000      thru vis-gru-mtv-999        .
           perform   vis-des-gru-000      thru vis-des-gru-999        .
      *              *-------------------------------------------------*
      *              * Sottogruppo merceologico                        *
      *              *-------------------------------------------------*
           perform   vis-sgr-mtv-000      thru vis-sgr-mtv-999        .
           perform   vis-des-sgr-000      thru vis-des-sgr-999        .
      *              *-------------------------------------------------*
      *              * Unita' di misura                                *
      *              *-------------------------------------------------*
           perform   vis-umi-gst-000      thru vis-umi-gst-999        .
           perform   vis-des-umi-000      thru vis-des-umi-999        .
      *              *-------------------------------------------------*
      *              * Sinonimo                                        *
      *              *-------------------------------------------------*
           perform   vis-syn-mtv-000      thru vis-syn-mtv-999        .
      *              *-------------------------------------------------*
      *              * Si/no magazzino fisico                          *
      *              *-------------------------------------------------*
           perform   vis-snx-mag-000      thru vis-snx-mag-999        .
      *              *-------------------------------------------------*
      *              * Si/no magazzino contabile                       *
      *              *-------------------------------------------------*
           perform   vis-snx-mac-000      thru vis-snx-mac-999        .
           go to     vis-tes-reg-999.
       vis-tes-reg-200.
      *              *-------------------------------------------------*
      *              * Si/no in confezione                             *
      *              *-------------------------------------------------*
           perform   vis-tip-cfz-000      thru vis-tip-cfz-999        .
      *              *-------------------------------------------------*
      *              * Quantita' per confezione                        *
      *              *-------------------------------------------------*
           perform   vis-qta-cfz-000      thru vis-qta-cfz-999        .
      *              *-------------------------------------------------*
      *              * Peso unitario lordo                             *
      *              *-------------------------------------------------*
           perform   vis-pes-uni-000      thru vis-pes-uni-999        .
      *              *-------------------------------------------------*
      *              * Peso tara                                       *
      *              *-------------------------------------------------*
           perform   vis-pes-tar-000      thru vis-pes-tar-999        .
      *              *-------------------------------------------------*
      *              * Volume unitario                                 *
      *              *-------------------------------------------------*
           perform   vis-vol-uni-000      thru vis-vol-uni-999        .
      *              *-------------------------------------------------*
      *              * Larghezza                                       *
      *              *-------------------------------------------------*
           perform   vis-dim-lar-000      thru vis-dim-lar-999        .
      *              *-------------------------------------------------*
      *              * Altezza                                         *
      *              *-------------------------------------------------*
           perform   vis-dim-alt-000      thru vis-dim-alt-999        .
      *              *-------------------------------------------------*
      *              * Profondita'                                     *
      *              *-------------------------------------------------*
           perform   vis-dim-prf-000      thru vis-dim-prf-999        .
      *              *-------------------------------------------------*
      *              * Peculiarita' fisiche                            *
      *              *-------------------------------------------------*
           perform   vis-pcl-fis-000      thru vis-pcl-fis-999        .
      *              *-------------------------------------------------*
      *              * Coefficiente moltiplicatore                     *
      *              *-------------------------------------------------*
           perform   vis-coe-mol-000      thru vis-coe-mol-999        .
      *              *-------------------------------------------------*
      *              * Coefficiente divisore                           *
      *              *-------------------------------------------------*
           perform   vis-coe-div-000      thru vis-coe-div-999        .
           go to     vis-tes-reg-999.
       vis-tes-reg-300.
      *              *-------------------------------------------------*
      *              * Decimali quantita'                              *
      *              *-------------------------------------------------*
           perform   vis-dec-qta-000      thru vis-dec-qta-999        .
      *              *-------------------------------------------------*
      *              * Tipo variante                                   *
      *              *-------------------------------------------------*
           perform   vis-tip-vpr-000      thru vis-tip-vpr-999        .
           perform   vis-des-tvv-000      thru vis-des-tvv-999        .
      *              *-------------------------------------------------*
      *              * Codice statistico 1                             *
      *              *-------------------------------------------------*
           perform   vis-cod-s01-000      thru vis-cod-s01-999        .
           perform   vis-des-cs1-000      thru vis-des-cs1-999        .
      *              *-------------------------------------------------*
      *              * Codice statistico 2                             *
      *              *-------------------------------------------------*
           perform   vis-cod-s02-000      thru vis-cod-s02-999        .
           perform   vis-des-cs2-000      thru vis-des-cs2-999        .
      *              *-------------------------------------------------*
      *              * Codice statistico 3                             *
      *              *-------------------------------------------------*
           perform   vis-cod-s03-000      thru vis-cod-s03-999        .
           perform   vis-des-cs3-000      thru vis-des-cs3-999        .
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
                     depending            on   w-cnt-sts-imp-npt      .
           go to     pmt-tes-reg-999.
       pmt-tes-reg-100.
      *              *-------------------------------------------------*
      *              * Codice materia varia                            *
      *              *-------------------------------------------------*
           perform   pmt-cod-mtv-000      thru pmt-cod-mtv-999        .
      *              *-------------------------------------------------*
      *              * Descrizione interna                             *
      *              *-------------------------------------------------*
           perform   pmt-des-int-000      thru pmt-des-int-999        .
      *              *-------------------------------------------------*
      *              * Classe merceologica                             *
      *              *-------------------------------------------------*
           perform   pmt-cla-mtv-000      thru pmt-cla-mtv-999        .
      *              *-------------------------------------------------*
      *              * Gruppo merceologico                             *
      *              *-------------------------------------------------*
           perform   pmt-gru-mtv-000      thru pmt-gru-mtv-999        .
      *              *-------------------------------------------------*
      *              * Sottogruppo merceologico                        *
      *              *-------------------------------------------------*
           perform   pmt-sgr-mtv-000      thru pmt-sgr-mtv-999        .
      *              *-------------------------------------------------*
      *              * Unita' di misura                                *
      *              *-------------------------------------------------*
           perform   pmt-umi-gst-000      thru pmt-umi-gst-999        .
      *              *-------------------------------------------------*
      *              * Sinonimo                                        *
      *              *-------------------------------------------------*
           perform   pmt-syn-mtv-000      thru pmt-syn-mtv-999        .
      *              *-------------------------------------------------*
      *              * Si/no magazzino fisico                          *
      *              *-------------------------------------------------*
           perform   pmt-snx-mag-000      thru pmt-snx-mag-999        .
      *              *-------------------------------------------------*
      *              * Si/no magazzino contabile                       *
      *              *-------------------------------------------------*
           perform   pmt-snx-mac-000      thru pmt-snx-mac-999        .
           go to     pmt-tes-reg-999.
       pmt-tes-reg-200.
      *              *-------------------------------------------------*
      *              * Si/no in confezione                             *
      *              *-------------------------------------------------*
           perform   pmt-tip-cfz-000      thru pmt-tip-cfz-999        .
      *              *-------------------------------------------------*
      *              * Quantita' per confezione                        *
      *              *-------------------------------------------------*
           perform   pmt-qta-cfz-000      thru pmt-qta-cfz-999        .
      *              *-------------------------------------------------*
      *              * Peso unitario lordo                             *
      *              *-------------------------------------------------*
           perform   pmt-pes-uni-000      thru pmt-pes-uni-999        .
      *              *-------------------------------------------------*
      *              * Peso tara                                       *
      *              *-------------------------------------------------*
           perform   pmt-pes-tar-000      thru pmt-pes-tar-999        .
      *              *-------------------------------------------------*
      *              * Volume unitario                                 *
      *              *-------------------------------------------------*
           perform   pmt-vol-uni-000      thru pmt-vol-uni-999        .
      *              *-------------------------------------------------*
      *              * Larghezza                                       *
      *              *-------------------------------------------------*
           perform   pmt-dim-lar-000      thru pmt-dim-lar-999        .
      *              *-------------------------------------------------*
      *              * Altezza                                         *
      *              *-------------------------------------------------*
           perform   pmt-dim-alt-000      thru pmt-dim-alt-999        .
      *              *-------------------------------------------------*
      *              * Profondita'                                     *
      *              *-------------------------------------------------*
           perform   pmt-dim-prf-000      thru pmt-dim-prf-999        .
      *              *-------------------------------------------------*
      *              * Peculiarita' fisiche                            *
      *              *-------------------------------------------------*
           perform   pmt-pcl-fis-000      thru pmt-pcl-fis-999        .
      *              *-------------------------------------------------*
      *              * Coefficiente moltiplicatore                     *
      *              *-------------------------------------------------*
           perform   pmt-coe-mol-000      thru pmt-coe-mol-999        .
      *              *-------------------------------------------------*
      *              * Coefficiente divisore                           *
      *              *-------------------------------------------------*
           perform   pmt-coe-div-000      thru pmt-coe-div-999        .
           go to     pmt-tes-reg-999.
       pmt-tes-reg-300.
      *              *-------------------------------------------------*
      *              * Decimali quantita'                              *
      *              *-------------------------------------------------*
           perform   pmt-dec-qta-000      thru pmt-dec-qta-999        .
      *              *-------------------------------------------------*
      *              * Tipo variante                                   *
      *              *-------------------------------------------------*
           perform   pmt-tip-vpr-000      thru pmt-tip-vpr-999        .
      *              *-------------------------------------------------*
      *              * Codice statistico 1                             *
      *              *-------------------------------------------------*
           perform   pmt-cod-s01-000      thru pmt-cod-s01-999        .
      *              *-------------------------------------------------*
      *              * Codice statistico 2                             *
      *              *-------------------------------------------------*
           perform   pmt-cod-s02-000      thru pmt-cod-s02-999        .
      *              *-------------------------------------------------*
      *              * Codice statistico 3                             *
      *              *-------------------------------------------------*
           perform   pmt-cod-s03-000      thru pmt-cod-s03-999        .
           go to     pmt-tes-reg-999.
       pmt-tes-reg-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Codice materia varia             *
      *    *-----------------------------------------------------------*
       pmt-cod-mtv-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice materiale           :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cod-mtv-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Descrizione interna              *
      *    *-----------------------------------------------------------*
       pmt-des-int-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Descrizione ad uso interno :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-des-int-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Classe merceologica              *
      *    *-----------------------------------------------------------*
       pmt-cla-mtv-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Classe merceologica        :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cla-mtv-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Gruppo merceologico              *
      *    *-----------------------------------------------------------*
       pmt-gru-mtv-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Gruppo merceologico        :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-gru-mtv-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Sottogruppo merceologico         *
      *    *-----------------------------------------------------------*
       pmt-sgr-mtv-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Sottogruppo merceologico   :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-sgr-mtv-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Unita' di misura                 *
      *    *-----------------------------------------------------------*
       pmt-umi-gst-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Unita' di misura           :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-umi-gst-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Sinonimo per la materia varia    *
      *    *-----------------------------------------------------------*
       pmt-syn-mtv-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Sinonimo per il materiale  :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-syn-mtv-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Si/no gestione magazzino fisico  *
      *    *-----------------------------------------------------------*
       pmt-snx-mag-000.
      *              *-------------------------------------------------*
      *              * Test se campo da visualizzare                   *
      *              *-------------------------------------------------*
           if        w-prs-snx-mfm        =    "F"
                     go to pmt-snx-mag-999.
      *              *-------------------------------------------------*
      *              * Visualizzazione prompt                          *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Si/no gestione magazzino   :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-snx-mag-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Si/no gestione magazzino conta-  *
      *    *                          bile                             *
      *    *-----------------------------------------------------------*
       pmt-snx-mac-000.
      *              *-------------------------------------------------*
      *              * Test se campo da visualizzare                   *
      *              *-------------------------------------------------*
           if        w-prs-snx-mcm        =    "F"
                     go to pmt-snx-mac-999.
      *              *-------------------------------------------------*
      *              * Visualizzazione prompt                          *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Si/no gestione magazzino   :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      21                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "               contabile    "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-snx-mac-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Si/no in confezione              *
      *    *-----------------------------------------------------------*
       pmt-tip-cfz-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Materiale in confezione    :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-tip-cfz-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Quantita' per confezione         *
      *    *-----------------------------------------------------------*
       pmt-qta-cfz-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Q.ta' per   confezione     :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-qta-cfz-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Peso unitario                    *
      *    *-----------------------------------------------------------*
       pmt-pes-uni-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Peso unitario lordo        :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-pes-uni-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Peso tara                        *
      *    *-----------------------------------------------------------*
       pmt-pes-tar-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Peso tara                  :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-pes-tar-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Volume unitario                  *
      *    *-----------------------------------------------------------*
       pmt-vol-uni-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Volume unitario            :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-vol-uni-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Larghezza                        *
      *    *-----------------------------------------------------------*
       pmt-dim-lar-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Larghezza                  :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-dim-lar-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Altezza                          *
      *    *-----------------------------------------------------------*
       pmt-dim-alt-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Altezza                    :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-dim-alt-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Profondita'                      *
      *    *-----------------------------------------------------------*
       pmt-dim-prf-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Profondita'                :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-dim-prf-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Peculiarita' fisiche             *
      *    *-----------------------------------------------------------*
       pmt-pcl-fis-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Peculiarita' fisiche       :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-pcl-fis-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Coefficiente moltiplicatore      *
      *    *-----------------------------------------------------------*
       pmt-coe-mol-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Coefficiente moltiplicatore:"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-coe-mol-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Coefficiente divisore            *
      *    *-----------------------------------------------------------*
       pmt-coe-div-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Coefficiente divisore      :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-coe-div-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Numero decimali quantita'        *
      *    *-----------------------------------------------------------*
       pmt-dec-qta-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Numero decimali quantita'  :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-dec-qta-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Tipo variante di produzione      *
      *    *-----------------------------------------------------------*
       pmt-tip-vpr-000.
       pmt-tip-vpr-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Codice statistico 1              *
      *    *-----------------------------------------------------------*
       pmt-cod-s01-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice statistico 1        :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cod-s01-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Codice statistico 2              *
      *    *-----------------------------------------------------------*
       pmt-cod-s02-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice statistico 2        :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cod-s02-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Codice statistico 3              *
      *    *-----------------------------------------------------------*
       pmt-cod-s03-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice statistico 3        :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cod-s03-999.
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
      *    * Accettazione campo testata : Codice materia varia alfa-   *
      *    *                              numerico                     *
      *    *-----------------------------------------------------------*
       acc-cod-mtv-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cod-mtv-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-cod-mtv-ope      .
           move      "L"                  to   w-cod-cod-mtv-tac      .
           move      w-tes-num-mtv        to   w-cod-cod-mtv-num      .
           move      w-tes-alf-mtv (1)    to   w-cod-cod-mtv-alf      .
           move      06                   to   w-cod-cod-mtv-lin      .
           move      30                   to   w-cod-cod-mtv-pos      .
           move      zero                 to   w-cod-cod-mtv-dln      .
           move      zero                 to   w-cod-cod-mtv-dps      .
           move      spaces               to   v-edm                  .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "NXSC"               to   v-pfk (08)             .
           perform   cod-cod-mtv-cll-000  thru cod-cod-mtv-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-cod-mtv-foi-000  thru cod-cod-mtv-foi-999    .
       acc-cod-mtv-110.
           perform   cod-cod-mtv-cll-000  thru cod-cod-mtv-cll-999    .
           if        w-cod-cod-mtv-ope    =    "F+"
                     go to acc-cod-mtv-115.
           if        w-cod-cod-mtv-ope    =    "AC"
                     go to acc-cod-mtv-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-mtv-115.
           perform   cod-cod-mtv-foi-000  thru cod-cod-mtv-foi-999    .
           go to     acc-cod-mtv-110.
       acc-cod-mtv-120.
           move      w-cod-cod-mtv-alf    to   v-alf                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-cod-mtv-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-cod-mtv-999.
       acc-cod-mtv-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-alf-mtv (1)      .
       acc-cod-mtv-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore a spaces : reimpostazione         *
      *                  *---------------------------------------------*
           if        w-tes-alf-mtv (1)    =    spaces
                     go to acc-cod-mtv-100.
       acc-cod-mtv-450.
      *                  *---------------------------------------------*
      *                  * Controllo che il valore impostato sia uni-  *
      *                  * co in archivio                              *
      *                  *---------------------------------------------*
           move      w-tes-num-mtv        to   w-ctl-uni-alf-num      .
           move      w-tes-alf-mtv (1)    to   w-ctl-uni-alf-alf      .
           perform   ctl-uni-alf-000      thru ctl-uni-alf-999        .
           if        w-ctl-uni-alf-flg    =    spaces
                     go to acc-cod-mtv-600.
      *                  *---------------------------------------------*
      *                  * Se controllo non superato                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Messaggio di errore                     *
      *                      *-----------------------------------------*
           move      "Codice materiale vario gia' utilizzato !"
                                          to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                      *-----------------------------------------*
      *                      * A reimpostazione                        *
      *                      *-----------------------------------------*
           go to     acc-cod-mtv-100.
       acc-cod-mtv-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cod-mtv-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-cod-mtv-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-cod-mtv-100.
       acc-cod-mtv-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Codice materia varia alfa *
      *    *-----------------------------------------------------------*
       vis-cod-mtv-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      14                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-alf-mtv (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-mtv-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Descrizione interna          *
      *    *-----------------------------------------------------------*
       acc-des-int-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      w-tes-des-int (1)    to   w-sav-des-int          .
       acc-des-int-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "NXSC"               to   v-pfk (08)             .
           move      w-tes-des-int (1)    to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-des-int-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-des-int-999.
       acc-des-int-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-des-int (1)      .
       acc-des-int-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore a spaces : reimpostazione, a meno *
      *                  * che non sia su tasto Up                     *
      *                  *---------------------------------------------*
           if        w-tes-des-int (1)    not  = spaces
                     go to acc-des-int-450.
           if        v-key                =    "UP  "
                     go to acc-des-int-600
           else      go to acc-des-int-100.
       acc-des-int-450.
      *                  *---------------------------------------------*
      *                  * Se valore a non spaces il primo carattere   *
      *                  * della prima riga non deve essere a spaces   *
      *                  *---------------------------------------------*
           if        w-tes-des-int (1)
                    (01 : 01)             =    spaces
                     go to acc-des-int-100.
       acc-des-int-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione riga descrizione in uppercase  *
      *                  *---------------------------------------------*
           move      w-tes-des-int (1)    to   w-all-str-alf          .
           move      40                   to   w-all-str-lun          .
           perform   all-str-upp-000      thru all-str-upp-999        .
           move      w-all-str-alf        to   w-tes-des-key (1)      .
       acc-des-int-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-des-int-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-des-int-100.
       acc-des-int-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Descrizione interna       *
      *    *-----------------------------------------------------------*
       vis-des-int-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-des-int (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-des-int-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Classe merceologica          *
      *    *-----------------------------------------------------------*
       acc-cla-mtv-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      w-tes-cla-mtv (1)    to   w-sav-cla-mtv          .
       acc-cla-mtv-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-zv1-ope      .
           move      w-tes-cla-mtv (1)    to   w-cod-mne-zv1-cla      .
           move      zero                 to   w-cod-mne-zv1-gru      .
           move      zero                 to   w-cod-mne-zv1-sgr      .
           move      11                   to   w-cod-mne-zv1-lin      .
           move      30                   to   w-cod-mne-zv1-pos      .
           move      11                   to   w-cod-mne-zv1-dln      .
           move      36                   to   w-cod-mne-zv1-dps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "NXSC"               to   v-pfk (08)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-mne-zv1-cll-000  thru cod-mne-zv1-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-mne-zv1-foi-000  thru cod-mne-zv1-foi-999    .
       acc-cla-mtv-110.
           perform   cod-mne-zv1-cll-000  thru cod-mne-zv1-cll-999    .
           if        w-cod-mne-zv1-ope    =    "F+"
                     go to acc-cla-mtv-115.
           if        w-cod-mne-zv1-ope    =    "AC"
                     go to acc-cla-mtv-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cla-mtv-115.
           perform   cod-mne-zv1-foi-000  thru cod-mne-zv1-foi-999    .
           go to     acc-cla-mtv-110.
       acc-cla-mtv-120.
           move      w-cod-mne-zv1-cla    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-cla-mtv-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-cla-mtv-999.
       acc-cla-mtv-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-cla-mtv (1)      .
       acc-cla-mtv-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura archivio [zv1]                      *
      *                  *---------------------------------------------*
           move      w-tes-cla-mtv (1)    to   w-let-arc-zv1-cla      .
           perform   let-arc-zv1-000      thru let-arc-zv1-999        .
      *                   *--------------------------------------------*
      *                   * Bufferizzazione di :                       *
      *                   * - Descrizione                              *
      *                   * - Ulteriore suddivisione                   *
      *                   * - Unita' di misura da proporre             *
      *                   *--------------------------------------------*
           move      w-let-arc-zv1-des    to   w-tes-cla-mtv-des (1)  .
           move      w-let-arc-zv1-sud    to   w-tes-cla-mtv-sud (1)  .
           move      w-let-arc-zv1-umi    to   w-tes-cla-mtv-umi (1)  .
      *                   *--------------------------------------------*
      *                   * Visualizzazione descrizione                *
      *                   *--------------------------------------------*
           perform   vis-des-cla-000      thru vis-des-cla-999        .
      *                  *---------------------------------------------*
      *                  * Se lettura errata : reimpostazione          *
      *                  *---------------------------------------------*
           if        w-let-arc-zv1-flg    not  = spaces
                     go to acc-cla-mtv-100.
       acc-cla-mtv-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore impostato uguale a precedente :   *
      *                  * oltre                                       *
      *                  *---------------------------------------------*
           if        w-tes-cla-mtv (1)    =    w-sav-cla-mtv
                     go to acc-cla-mtv-800.
      *                      *-----------------------------------------*
      *                      * Normalizzazione e visualizzazione dati  *
      *                      * gruppo merceologico                     *
      *                      *-----------------------------------------*
           if        w-tes-gru-mtv (1)    =    zero
                     go to acc-cla-mtv-620.
           move      zero                 to   w-tes-gru-mtv (1)      .
           move      spaces               to   w-tes-gru-mtv-des (1)  .
           move      zero                 to   w-tes-gru-mtv-sud (1)  .
           move      spaces               to   w-tes-gru-mtv-umi (1)  .
           perform   vis-gru-mtv-000      thru vis-gru-mtv-999        .
           perform   vis-des-gru-000      thru vis-des-gru-999        .
       acc-cla-mtv-620.
      *                      *-----------------------------------------*
      *                      * Normalizzazione e visualizzazione dati  *
      *                      * sottogruppo merceologico                *
      *                      *-----------------------------------------*
           if        w-tes-sgr-mtv (1)    =    zero
                     go to acc-cla-mtv-640.
           move      zero                 to   w-tes-sgr-mtv (1)      .
           move      spaces               to   w-tes-sgr-mtv-des (1)  .
           move      zero                 to   w-tes-sgr-mtv-sud (1)  .
           move      spaces               to   w-tes-sgr-mtv-umi (1)  .
           perform   vis-sgr-mtv-000      thru vis-sgr-mtv-999        .
           perform   vis-des-sgr-000      thru vis-des-sgr-999        .
       acc-cla-mtv-640.
       acc-cla-mtv-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-cla-mtv-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-cla-mtv-100.
       acc-cla-mtv-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Classe merceologica       *
      *    *-----------------------------------------------------------*
       vis-cla-mtv-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      11                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-cla-mtv (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cla-mtv-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Descrizione classe        *
      *    *-----------------------------------------------------------*
       vis-des-cla-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      36                   to   v-pos                  .
           move      w-tes-cla-mtv-des (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-des-cla-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Gruppo merceologico          *
      *    *-----------------------------------------------------------*
       acc-gru-mtv-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se valore da accettare                 *
      *                  *---------------------------------------------*
           if        w-tes-cla-mtv (1)    =    zero
                     go to acc-gru-mtv-999.
           if        w-tes-cla-mtv-sud (1)
                                          =    01
                     go to acc-gru-mtv-999.
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      w-tes-gru-mtv (1)    to   w-sav-gru-mtv          .
       acc-gru-mtv-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-zv2-ope      .
           move      w-tes-cla-mtv (1)    to   w-cod-mne-zv2-cla      .
           move      w-tes-cla-mtv-des (1)
                                          to   w-cod-mne-zv2-dcl      .
           move      w-tes-gru-mtv (1)    to   w-cod-mne-zv2-gru      .
           move      zero                 to   w-cod-mne-zv2-sgr      .
           move      12                   to   w-cod-mne-zv2-lin      .
           move      30                   to   w-cod-mne-zv2-pos      .
           move      12                   to   w-cod-mne-zv2-dln      .
           move      36                   to   w-cod-mne-zv2-dps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "NXSC"               to   v-pfk (08)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-mne-zv2-cll-000  thru cod-mne-zv2-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-mne-zv2-foi-000  thru cod-mne-zv2-foi-999    .
       acc-gru-mtv-110.
           perform   cod-mne-zv2-cll-000  thru cod-mne-zv2-cll-999    .
           if        w-cod-mne-zv2-ope    =    "F+"
                     go to acc-gru-mtv-115.
           if        w-cod-mne-zv2-ope    =    "AC"
                     go to acc-gru-mtv-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-gru-mtv-115.
           perform   cod-mne-zv2-foi-000  thru cod-mne-zv2-foi-999    .
           go to     acc-gru-mtv-110.
       acc-gru-mtv-120.
           move      w-cod-mne-zv2-gru    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-gru-mtv-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-gru-mtv-999.
       acc-gru-mtv-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-gru-mtv (1)      .
       acc-gru-mtv-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura archivio [zv2]                      *
      *                  *---------------------------------------------*
           move      w-tes-cla-mtv (1)    to   w-let-arc-zv2-cla      .
           move      w-tes-gru-mtv (1)    to   w-let-arc-zv2-gru      .
           perform   let-arc-zv2-000      thru let-arc-zv2-999        .
      *                   *--------------------------------------------*
      *                   * Bufferizzazione di :                       *
      *                   * - Descrizione                              *
      *                   * - Ulteriore suddivisione                   *
      *                   * - Unita' di misura da proporre             *
      *                   *--------------------------------------------*
           move      w-let-arc-zv2-des    to   w-tes-gru-mtv-des (1)  .
           move      w-let-arc-zv2-sud    to   w-tes-gru-mtv-sud (1)  .
           move      w-let-arc-zv2-umi    to   w-tes-gru-mtv-umi (1)  .
      *                   *--------------------------------------------*
      *                   * Visualizzazione descrizione                *
      *                   *--------------------------------------------*
           perform   vis-des-gru-000      thru vis-des-gru-999        .
      *                  *---------------------------------------------*
      *                  * Se lettura errata : reimpostazione          *
      *                  *---------------------------------------------*
           if        w-let-arc-zv2-flg    not  = spaces
                     go to acc-gru-mtv-100.
      *                  *---------------------------------------------*
      *                  * Se valore a zero : reimpostazione, a meno   *
      *                  * che non sia su tasto Up                     *
      *                  *---------------------------------------------*
           if        w-tes-gru-mtv (1)    not  = zero
                     go to acc-gru-mtv-450.
           if        v-key                =    "UP  "
                     go to acc-gru-mtv-600
           else      go to acc-gru-mtv-100.
       acc-gru-mtv-450.
       acc-gru-mtv-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore impostato uguale a precedente :   *
      *                  * oltre                                       *
      *                  *---------------------------------------------*
           if        w-tes-gru-mtv (1)    =    w-sav-gru-mtv
                     go to acc-gru-mtv-800.
      *                      *-----------------------------------------*
      *                      * Normalizzazione e visualizzazione dati  *
      *                      * sottogruppo merceologico                *
      *                      *-----------------------------------------*
           if        w-tes-sgr-mtv (1)    =    zero
                     go to acc-gru-mtv-640.
           move      zero                 to   w-tes-sgr-mtv (1)      .
           move      spaces               to   w-tes-sgr-mtv-des (1)  .
           move      zero                 to   w-tes-sgr-mtv-sud (1)  .
           move      spaces               to   w-tes-sgr-mtv-umi (1)  .
           perform   vis-sgr-mtv-000      thru vis-sgr-mtv-999        .
           perform   vis-des-sgr-000      thru vis-des-sgr-999        .
       acc-gru-mtv-640.
       acc-gru-mtv-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-gru-mtv-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-gru-mtv-100.
       acc-gru-mtv-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Gruppo merceologico       *
      *    *-----------------------------------------------------------*
       vis-gru-mtv-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      12                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-gru-mtv (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-gru-mtv-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Descrizione gruppo        *
      *    *-----------------------------------------------------------*
       vis-des-gru-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      36                   to   v-pos                  .
           move      w-tes-gru-mtv-des (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-des-gru-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Sottogruppo merceologico     *
      *    *-----------------------------------------------------------*
       acc-sgr-mtv-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se valore da accettare                 *
      *                  *---------------------------------------------*
           if        w-tes-gru-mtv (1)    =    zero
                     go to acc-sgr-mtv-999.
           if        w-tes-gru-mtv-sud (1)
                                          =    01
                     go to acc-sgr-mtv-999.
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      w-tes-sgr-mtv (1)    to   w-sav-sgr-mtv          .
       acc-sgr-mtv-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-zv3-ope      .
           move      w-tes-cla-mtv (1)    to   w-cod-mne-zv3-cla      .
           move      w-tes-cla-mtv-des (1)
                                          to   w-cod-mne-zv3-dcl      .
           move      w-tes-gru-mtv (1)    to   w-cod-mne-zv3-gru      .
           move      w-tes-gru-mtv-des (1)
                                          to   w-cod-mne-zv3-dgr      .
           move      w-tes-sgr-mtv (1)    to   w-cod-mne-zv3-sgr      .
           move      13                   to   w-cod-mne-zv3-lin      .
           move      30                   to   w-cod-mne-zv3-pos      .
           move      13                   to   w-cod-mne-zv3-dln      .
           move      36                   to   w-cod-mne-zv3-dps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "NXSC"               to   v-pfk (08)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-mne-zv3-cll-000  thru cod-mne-zv3-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-mne-zv3-foi-000  thru cod-mne-zv3-foi-999    .
       acc-sgr-mtv-110.
           perform   cod-mne-zv3-cll-000  thru cod-mne-zv3-cll-999    .
           if        w-cod-mne-zv3-ope    =    "F+"
                     go to acc-sgr-mtv-115.
           if        w-cod-mne-zv3-ope    =    "AC"
                     go to acc-sgr-mtv-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-sgr-mtv-115.
           perform   cod-mne-zv3-foi-000  thru cod-mne-zv3-foi-999    .
           go to     acc-sgr-mtv-110.
       acc-sgr-mtv-120.
           move      w-cod-mne-zv3-sgr    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-sgr-mtv-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-sgr-mtv-999.
       acc-sgr-mtv-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-sgr-mtv (1)      .
       acc-sgr-mtv-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura archivio [zv3]                      *
      *                  *---------------------------------------------*
           move      w-tes-cla-mtv (1)    to   w-let-arc-zv3-cla      .
           move      w-tes-gru-mtv (1)    to   w-let-arc-zv3-gru      .
           move      w-tes-sgr-mtv (1)    to   w-let-arc-zv3-sgr      .
           perform   let-arc-zv3-000      thru let-arc-zv3-999        .
      *                   *--------------------------------------------*
      *                   * Bufferizzazione di :                       *
      *                   * - Descrizione                              *
      *                   * - Ulteriore suddivisione                   *
      *                   * - Unita' di misura da proporre             *
      *                   *--------------------------------------------*
           move      w-let-arc-zv3-des    to   w-tes-sgr-mtv-des (1)  .
           move      w-let-arc-zv3-sud    to   w-tes-sgr-mtv-sud (1)  .
           move      w-let-arc-zv3-umi    to   w-tes-sgr-mtv-umi (1)  .
      *                   *--------------------------------------------*
      *                   * Visualizzazione descrizione                *
      *                   *--------------------------------------------*
           perform   vis-des-sgr-000      thru vis-des-sgr-999        .
      *                  *---------------------------------------------*
      *                  * Se lettura errata : reimpostazione          *
      *                  *---------------------------------------------*
           if        w-let-arc-zv3-flg    not  = spaces
                     go to acc-sgr-mtv-100.
      *                  *---------------------------------------------*
      *                  * Se valore a zero : reimpostazione, a meno   *
      *                  * che non sia su tasto Up                     *
      *                  *---------------------------------------------*
           if        w-tes-sgr-mtv (1)    not  = zero
                     go to acc-sgr-mtv-450.
           if        v-key                =    "UP  "
                     go to acc-sgr-mtv-600
           else      go to acc-sgr-mtv-100.
       acc-sgr-mtv-450.
       acc-sgr-mtv-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-sgr-mtv-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-sgr-mtv-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-sgr-mtv-100.
       acc-sgr-mtv-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Sottogruppo merceologico  *
      *    *-----------------------------------------------------------*
       vis-sgr-mtv-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      13                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-sgr-mtv (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-sgr-mtv-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Descrizione sottogruppo   *
      *    *-----------------------------------------------------------*
       vis-des-sgr-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      36                   to   v-pos                  .
           move      w-tes-sgr-mtv-des (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-des-sgr-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Unita' di misura             *
      *    *-----------------------------------------------------------*
       acc-umi-gst-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione valore di default              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Solo in inserimento e con valori a spazi*
      *                      *-----------------------------------------*
           if        w-cnt-mfu-tip-fun    not  = "I"
                     go to acc-umi-gst-100.
           if        w-tes-umi-gst (1)    not  = spaces
                     go to acc-umi-gst-100.
      *                      *-----------------------------------------*
      *                      * Se contenuta nel sottogruppo            *
      *                      *-----------------------------------------*
           if        w-tes-sgr-mtv-umi (1)
                                          not  = spaces
                     move  w-tes-sgr-mtv-umi (1)
                                          to   w-tes-umi-gst (1)
                     go to acc-umi-gst-100.
      *                      *-----------------------------------------*
      *                      * Se contenuta nel gruppo                 *
      *                      *-----------------------------------------*
           if        w-tes-gru-mtv-sud (1)
                                          =    01 and
                     w-tes-gru-mtv-umi (1)
                                          not  = spaces
                     move  w-tes-gru-mtv-umi (1)
                                          to   w-tes-umi-gst (1)
                     go to acc-umi-gst-100.
      *                      *-----------------------------------------*
      *                      * Se contenuta nella classe               *
      *                      *-----------------------------------------*
           if        w-tes-cla-mtv-sud (1)
                                          =    01
                     move  w-tes-cla-mtv-umi (1)
                                          to   w-tes-umi-gst (1)
                     go to acc-umi-gst-100.
       acc-umi-gst-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-cod-umi-ope      .
           move      w-tes-umi-gst (1)    to   w-cod-cod-umi-cod      .
           move      15                   to   w-cod-cod-umi-lin      .
           move      30                   to   w-cod-cod-umi-pos      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "NXSC"               to   v-pfk (08)             .
           perform   cod-cod-zum-cll-000  thru cod-cod-zum-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-cod-zum-foi-000  thru cod-cod-zum-foi-999    .
       acc-umi-gst-110.
           perform   cod-cod-zum-cll-000  thru cod-cod-zum-cll-999    .
           if        w-cod-cod-umi-ope    =    "F+"
                     go to acc-umi-gst-115.
           if        w-cod-cod-umi-ope    =    "AC"
                     go to acc-umi-gst-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-umi-gst-115.
           perform   cod-cod-zum-foi-000  thru cod-cod-zum-foi-999    .
           go to     acc-umi-gst-110.
       acc-umi-gst-120.
           move      w-cod-cod-umi-cod    to   v-alf                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-umi-gst-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-umi-gst-999.
       acc-umi-gst-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-umi-gst (1)      .
       acc-umi-gst-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura tabella unita' di misura            *
      *                  *---------------------------------------------*
           move      w-tes-umi-gst (1)    to   w-let-arc-zum-cod      .
           perform   let-arc-zum-000      thru let-arc-zum-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione dati associati al codice     *
      *                  * unita' di misura                            *
      *                  *---------------------------------------------*
           move      w-let-arc-zum-des    to   w-tes-umi-gst-des (1)  .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione unita' di mi-   *
      *                  * sura                                        *
      *                  *---------------------------------------------*
           perform   vis-des-umi-000      thru vis-des-umi-999        .
      *                  *---------------------------------------------*
      *                  * Se unita' di misura non esistente : reimpo- *
      *                  * stazione                                    *
      *                  *---------------------------------------------*
           if        w-let-arc-zum-flg    not  = spaces
                     go to acc-umi-gst-100.
       acc-umi-gst-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-umi-gst-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-umi-gst-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-umi-gst-100.
       acc-umi-gst-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Unita' di misura                  *
      *    *-----------------------------------------------------------*
       vis-umi-gst-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-umi-gst (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-umi-gst-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Descrizione unita' di misura      *
      *    *-----------------------------------------------------------*
       vis-des-umi-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      36                   to   v-pos                  .
           move      w-tes-umi-gst-des (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-des-umi-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Sinonimo materia varia       *
      *    *-----------------------------------------------------------*
       acc-syn-mtv-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-syn-mtv-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-cod-mtv-ope      .
           move      "S"                  to   w-cod-cod-mtv-tac      .
           move      w-tes-num-mtv        to   w-cod-cod-mtv-num      .
           move      w-tes-syn-mtv (1)    to   w-cod-cod-mtv-alf      .
           move      17                   to   w-cod-cod-mtv-lin      .
           move      30                   to   w-cod-cod-mtv-pos      .
           move      zero                 to   w-cod-cod-mtv-dln      .
           move      zero                 to   w-cod-cod-mtv-dps      .
           move      spaces               to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "NXSC"               to   v-pfk (08)             .
           perform   cod-cod-mtv-cll-000  thru cod-cod-mtv-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-cod-mtv-foi-000  thru cod-cod-mtv-foi-999    .
       acc-syn-mtv-110.
           perform   cod-cod-mtv-cll-000  thru cod-cod-mtv-cll-999    .
           if        w-cod-cod-mtv-ope    =    "F+"
                     go to acc-syn-mtv-115.
           if        w-cod-cod-mtv-ope    =    "AC"
                     go to acc-syn-mtv-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-syn-mtv-115.
           perform   cod-cod-mtv-foi-000  thru cod-cod-mtv-foi-999    .
           go to     acc-syn-mtv-110.
       acc-syn-mtv-120.
           move      w-cod-cod-mtv-alf    to   v-alf                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-syn-mtv-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-syn-mtv-999.
       acc-syn-mtv-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-syn-mtv (1)      .
       acc-syn-mtv-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-syn-mtv-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-syn-mtv-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-syn-mtv-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-syn-mtv-100.
       acc-syn-mtv-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Sinonimo materia varia    *
      *    *-----------------------------------------------------------*
       vis-syn-mtv-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      13                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-syn-mtv (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-syn-mtv-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Si/no magazzino fisico       *
      *    *-----------------------------------------------------------*
       acc-snx-mag-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-prs-snx-mfm        =    "F"
                     move  w-prs-snx-mfd  to   w-tes-snx-mag (1)
                     go to acc-snx-mag-999.
      *                  *---------------------------------------------*
      *                  * Preparazione eventuale default              *
      *                  *---------------------------------------------*
           if        w-tes-snx-mag (1)    not  = spaces
                     go to acc-snx-mag-100.
           move      w-prs-snx-mfd        to   w-tes-snx-mag (1)      .
       acc-snx-mag-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-snx-mag-lun    to   v-car                  .
           move      w-exp-snx-mag-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      19                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-snx-mag-tbl    to   v-txt                  .
           if        w-tes-snx-mag (1)    =    "S"
                     move  01             to   v-num
           else if   w-tes-snx-mag (1)    =    "N"
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
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
                     go to acc-snx-mag-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-snx-mag-999.
       acc-snx-mag-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        v-num                =    01
                     move  "S"            to   w-tes-snx-mag (1)
           else if   v-num                =    02
                     move  "N"            to   w-tes-snx-mag (1)
           else      move  zero           to   w-tes-snx-mag (1)      .
       acc-snx-mag-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-snx-mag-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-snx-mag-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-snx-mag-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-snx-mag-100.
       acc-snx-mag-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Si/no magazzino fiscale   *
      *    *-----------------------------------------------------------*
       vis-snx-mag-000.
      *              *-------------------------------------------------*
      *              * Test se campo da visualizzare                   *
      *              *-------------------------------------------------*
           if        w-prs-snx-mfm        =    "F"
                     go to vis-snx-mag-999.
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-snx-mag-lun    to   v-car                  .
           move      w-exp-snx-mag-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      19                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-snx-mag-tbl    to   v-txt                  .
           if        w-tes-snx-mag (1)    =    "S"
                     move  01             to   v-num
           else if   w-tes-snx-mag (1)    =    "N"
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-snx-mag-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Si/no magazzino contabile    *
      *    *-----------------------------------------------------------*
       acc-snx-mac-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-prs-snx-mcm        =    "F"
                     move  w-prs-snx-mcd  to   w-tes-snx-mac (1)
                     go to acc-snx-mac-999.
      *                  *---------------------------------------------*
      *                  * Preparazione eventuale default              *
      *                  *---------------------------------------------*
           if        w-tes-snx-mac (1)    not  = spaces
                     go to acc-snx-mac-100.
           move      w-prs-snx-mcd        to   w-tes-snx-mac (1)      .
       acc-snx-mac-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-snx-mac-lun    to   v-car                  .
           move      w-exp-snx-mac-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      20                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-snx-mac-tbl    to   v-txt                  .
           if        w-tes-snx-mac (1)    =    "S"
                     move  01             to   v-num
           else if   w-tes-snx-mac (1)    =    "N"
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
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
                     go to acc-snx-mac-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-snx-mac-999.
       acc-snx-mac-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        v-num                =    01
                     move  "S"            to   w-tes-snx-mac (1)
           else if   v-num                =    02
                     move  "N"            to   w-tes-snx-mac (1)
           else      move  zero           to   w-tes-snx-mac (1)      .
       acc-snx-mac-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-snx-mac-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-snx-mac-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-snx-mac-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-snx-mac-100.
       acc-snx-mac-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Si/no magazzino contabile *
      *    *-----------------------------------------------------------*
       vis-snx-mac-000.
      *              *-------------------------------------------------*
      *              * Test se campo da visualizzare                   *
      *              *-------------------------------------------------*
           if        w-prs-snx-mcm        =    "F"
                     go to vis-snx-mac-999.
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-snx-mac-lun    to   v-car                  .
           move      w-exp-snx-mac-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      20                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-snx-mac-tbl    to   v-txt                  .
           if        w-tes-snx-mac (1)    =    "S"
                     move  01             to   v-num
           else if   w-tes-snx-mac (1)    =    "N"
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-snx-mac-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Si/no in confezione          *
      *    *-----------------------------------------------------------*
       acc-tip-cfz-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-tip-cfz-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-cfz-lun    to   v-car                  .
           move      w-exp-tip-cfz-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      07                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-tip-cfz-tbl    to   v-txt                  .
           if        w-tes-tip-cfz (1)    =    zero
                     move  01             to   v-num
           else if   w-tes-tip-cfz (1)    =    01
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
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
                     go to acc-tip-cfz-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-tip-cfz-999.
       acc-tip-cfz-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        v-num                =    01
                     move  zero           to   w-tes-tip-cfz (1)
           else if   v-num                =    02
                     move  01             to   w-tes-tip-cfz (1)
           else      move  zero           to   w-tes-tip-cfz (1)      .
       acc-tip-cfz-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-tip-cfz-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se 'no' in confezione                       *
      *                  *---------------------------------------------*
           if        w-tes-tip-cfz (1)    not  = zero
                     go to acc-tip-cfz-800.
      *                  *---------------------------------------------*
      *                  * Normalizzazione e visualizzazione quantita' *
      *                  *---------------------------------------------*
           move      zero                 to   w-tes-qta-cfz (1)      .
           perform   vis-qta-cfz-000      thru vis-qta-cfz-999        .
       acc-tip-cfz-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-tip-cfz-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-tip-cfz-100.
       acc-tip-cfz-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Si/no in confezione       *
      *    *-----------------------------------------------------------*
       vis-tip-cfz-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-cfz-lun    to   v-car                  .
           move      w-exp-tip-cfz-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      07                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-tip-cfz-tbl    to   v-txt                  .
           if        w-tes-tip-cfz (1)    =    zero
                     move  01             to   v-num
           else if   w-tes-tip-cfz (1)    =    01
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tip-cfz-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Quantita' per confezione     *
      *    *-----------------------------------------------------------*
       acc-qta-cfz-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-tes-tip-cfz (1)    =    zero
                     go to acc-qta-cfz-999.
       acc-qta-cfz-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      03                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BGD"               to   v-edm                  .
           move      08                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "PRSC"               to   v-pfk (07)             .
           move      "NXSC"               to   v-pfk (08)             .
           move      w-tes-qta-cfz (1)    to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-qta-cfz-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-qta-cfz-999.
       acc-qta-cfz-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-qta-cfz (1)      .
       acc-qta-cfz-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-qta-cfz-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-qta-cfz-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-qta-cfz-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-qta-cfz-100.
       acc-qta-cfz-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Quantita' per confezione  *
      *    *-----------------------------------------------------------*
       vis-qta-cfz-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      03                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BGD"               to   v-edm                  .
           move      08                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-qta-cfz (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-qta-cfz-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Peso unitario lordo          *
      *    *-----------------------------------------------------------*
       acc-pes-uni-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-pes-uni-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      03                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BGD"               to   v-edm                  .
           move      10                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "PRSC"               to   v-pfk (07)             .
           move      "NXSC"               to   v-pfk (08)             .
           move      w-tes-pes-uni (1)    to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-pes-uni-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-pes-uni-999.
       acc-pes-uni-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-pes-uni (1)      .
       acc-pes-uni-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-pes-uni-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-pes-uni-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-pes-uni-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-pes-uni-100.
       acc-pes-uni-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Peso unitario lordo       *
      *    *-----------------------------------------------------------*
       vis-pes-uni-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      03                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BGD"               to   v-edm                  .
           move      10                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-pes-uni (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-pes-uni-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Peso tara                    *
      *    *-----------------------------------------------------------*
       acc-pes-tar-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-pes-tar-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      03                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BGD"               to   v-edm                  .
           move      11                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "PRSC"               to   v-pfk (07)             .
           move      "NXSC"               to   v-pfk (08)             .
           move      w-tes-pes-tar (1)    to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-pes-tar-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-pes-tar-999.
       acc-pes-tar-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-pes-tar (1)      .
       acc-pes-tar-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-pes-tar-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-pes-tar-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-pes-tar-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-pes-tar-100.
       acc-pes-tar-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Peso tara                 *
      *    *-----------------------------------------------------------*
       vis-pes-tar-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      03                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BGD"               to   v-edm                  .
           move      11                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-pes-tar (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-pes-tar-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Volume unitario              *
      *    *-----------------------------------------------------------*
       acc-vol-uni-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-vol-uni-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      03                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BGD"               to   v-edm                  .
           move      12                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "PRSC"               to   v-pfk (07)             .
           move      "NXSC"               to   v-pfk (08)             .
           move      w-tes-vol-uni (1)    to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-vol-uni-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-vol-uni-999.
       acc-vol-uni-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-vol-uni (1)      .
       acc-vol-uni-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-vol-uni-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-vol-uni-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-vol-uni-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-vol-uni-100.
       acc-vol-uni-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Volume unitario           *
      *    *-----------------------------------------------------------*
       vis-vol-uni-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      03                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BGD"               to   v-edm                  .
           move      12                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-vol-uni (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-vol-uni-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Larghezza                    *
      *    *-----------------------------------------------------------*
       acc-dim-lar-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-dim-lar-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      03                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BGD"               to   v-edm                  .
           move      14                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "PRSC"               to   v-pfk (07)             .
           move      "NXSC"               to   v-pfk (08)             .
           move      w-tes-dim-lar (1)    to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-dim-lar-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-dim-lar-999.
       acc-dim-lar-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-dim-lar (1)      .
       acc-dim-lar-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-dim-lar-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-dim-lar-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-dim-lar-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-dim-lar-100.
       acc-dim-lar-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Larghezza                 *
      *    *-----------------------------------------------------------*
       vis-dim-lar-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      03                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BGD"               to   v-edm                  .
           move      14                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-dim-lar (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dim-lar-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Altezza                      *
      *    *-----------------------------------------------------------*
       acc-dim-alt-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-dim-alt-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      03                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BGD"               to   v-edm                  .
           move      15                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "PRSC"               to   v-pfk (07)             .
           move      "NXSC"               to   v-pfk (08)             .
           move      w-tes-dim-alt (1)    to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-dim-alt-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-dim-alt-999.
       acc-dim-alt-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-dim-alt (1)      .
       acc-dim-alt-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-dim-alt-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-dim-alt-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-dim-alt-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-dim-alt-100.
       acc-dim-alt-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Altezza                   *
      *    *-----------------------------------------------------------*
       vis-dim-alt-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      03                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BGD"               to   v-edm                  .
           move      15                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-dim-alt (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dim-alt-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Profondita'                  *
      *    *-----------------------------------------------------------*
       acc-dim-prf-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-dim-prf-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      03                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BGD"               to   v-edm                  .
           move      16                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "PRSC"               to   v-pfk (07)             .
           move      "NXSC"               to   v-pfk (08)             .
           move      w-tes-dim-prf (1)    to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-dim-prf-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-dim-prf-999.
       acc-dim-prf-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-dim-prf (1)      .
       acc-dim-prf-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-dim-prf-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-dim-prf-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-dim-prf-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-dim-prf-100.
       acc-dim-prf-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Profondita'               *
      *    *-----------------------------------------------------------*
       vis-dim-prf-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      03                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BGD"               to   v-edm                  .
           move      16                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-dim-prf (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dim-prf-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Peculiarita' fisiche         *
      *    *-----------------------------------------------------------*
       acc-pcl-fis-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-pcl-fis-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      10                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "PRSC"               to   v-pfk (07)             .
           move      "NXSC"               to   v-pfk (08)             .
           move      w-tes-pcl-fis (1)    to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-pcl-fis-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-pcl-fis-999.
       acc-pcl-fis-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-pcl-fis (1)      .
       acc-pcl-fis-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-pcl-fis-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-pcl-fis-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-pcl-fis-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-pcl-fis-100.
       acc-pcl-fis-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Peculiarita' fisiche      *
      *    *-----------------------------------------------------------*
       vis-pcl-fis-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      10                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-pcl-fis (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-pcl-fis-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Coefficiente moltiplicatore  *
      *    *-----------------------------------------------------------*
       acc-coe-mol-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-coe-mol-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      03                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BGD"               to   v-edm                  .
           move      19                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "PRSC"               to   v-pfk (07)             .
           move      "NXSC"               to   v-pfk (08)             .
           move      w-tes-coe-mol (1)    to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-coe-mol-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-coe-mol-999.
       acc-coe-mol-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-coe-mol (1)      .
       acc-coe-mol-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-coe-mol-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-coe-mol-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-coe-mol-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-coe-mol-100.
       acc-coe-mol-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Coefficiente moltiplic.   *
      *    *-----------------------------------------------------------*
       vis-coe-mol-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      03                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BGD"               to   v-edm                  .
           move      19                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-coe-mol (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-coe-mol-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Coefficiente divisore        *
      *    *-----------------------------------------------------------*
       acc-coe-div-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-coe-div-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      03                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BGD"               to   v-edm                  .
           move      20                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "PRSC"               to   v-pfk (07)             .
           move      "NXSC"               to   v-pfk (08)             .
           move      w-tes-coe-div (1)    to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-coe-div-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-coe-div-999.
       acc-coe-div-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-coe-div (1)      .
       acc-coe-div-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-coe-div-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-coe-div-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-coe-div-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-coe-div-100.
       acc-coe-div-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Coefficiente divisore     *
      *    *-----------------------------------------------------------*
       vis-coe-div-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      03                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BGD"               to   v-edm                  .
           move      20                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-coe-div (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-coe-div-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Decimali quantita'           *
      *    *-----------------------------------------------------------*
       acc-dec-qta-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-dec-qta-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      07                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "PRSC"               to   v-pfk (07)             .
           move      "NXSC"               to   v-pfk (08)             .
           move      w-tes-dec-qta (1)    to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-dec-qta-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-dec-qta-999.
       acc-dec-qta-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-dec-qta (1)      .
       acc-dec-qta-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore non ammesso : reimpostazione      *
      *                  *---------------------------------------------*
           if        w-tes-dec-qta (1)    >    3
                     go to acc-dec-qta-100.
       acc-dec-qta-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-dec-qta-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-dec-qta-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-dec-qta-100.
       acc-dec-qta-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Decimali quantita'        *
      *    *-----------------------------------------------------------*
       vis-dec-qta-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      07                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-dec-qta (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dec-qta-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Tipo variante                *
      *    *-----------------------------------------------------------*
       acc-tip-vpr-000.
       acc-tip-vpr-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Tipo variante             *
      *    *-----------------------------------------------------------*
       vis-tip-vpr-000.
       vis-tip-vpr-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Descrizione variante      *
      *    *-----------------------------------------------------------*
       vis-des-tvv-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      10                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      37                   to   v-pos                  .
           move      w-tes-tip-vpr-des (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-des-tvv-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Codice statistico 1          *
      *    *-----------------------------------------------------------*
       acc-cod-s01-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cod-s01-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cmn-zvs-001-ope      .
           move      w-tes-cod-s01 (1)    to   w-cmn-zvs-001-cod      .
           move      12                   to   w-cmn-zvs-001-lin      .
           move      30                   to   w-cmn-zvs-001-pos      .
           move      12                   to   w-cmn-zvs-001-dln      .
           move      37                   to   w-cmn-zvs-001-dps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "PRSC"               to   v-pfk (07)             .
           perform   cmn-zvs-001-cll-000  thru cmn-zvs-001-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cmn-zvs-001-foi-000  thru cmn-zvs-001-foi-999    .
       acc-cod-s01-110.
           perform   cmn-zvs-001-cll-000  thru cmn-zvs-001-cll-999    .
           if        w-cmn-zvs-001-ope    =    "F+"
                     go to acc-cod-s01-115.
           if        w-cmn-zvs-001-ope    =    "AC"
                     go to acc-cod-s01-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-s01-115.
           perform   cmn-zvs-001-foi-000  thru cmn-zvs-001-foi-999    .
           go to     acc-cod-s01-110.
       acc-cod-s01-120.
           move      w-cmn-zvs-001-cod    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-cod-s01-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-cod-s01-999.
       acc-cod-s01-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-cod-s01 (1)      .
       acc-cod-s01-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura tabella                             *
      *                  *---------------------------------------------*
           move      01                   to   w-let-arc-zvs-tip      .
           move      w-tes-cod-s01 (1)    to   w-let-arc-zvs-cod      .
           perform   let-arc-zvs-000      thru let-arc-zvs-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione descrizione                  *
      *                  *---------------------------------------------*
           move      w-let-arc-zvs-des    to   w-tes-cod-s01-des (1)  .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione                 *
      *                  *---------------------------------------------*
           perform   vis-des-cs1-000      thru vis-des-cs1-999        .
      *                  *---------------------------------------------*
      *                  * Se codice errato : reimpostazione           *
      *                  *---------------------------------------------*
           if        w-let-arc-zvs-flg    not  = spaces
                     go to acc-cod-s01-100.
       acc-cod-s01-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cod-s01-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-cod-s01-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-cod-s01-100.
       acc-cod-s01-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Codice statistico 1       *
      *    *-----------------------------------------------------------*
       vis-cod-s01-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      12                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-cod-s01 (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-s01-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Descrizione codice sta-   *
      *    * tistico 1                                                 *
      *    *-----------------------------------------------------------*
       vis-des-cs1-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      37                   to   v-pos                  .
           move      w-tes-cod-s01-des (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-des-cs1-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Codice statistico 2          *
      *    *-----------------------------------------------------------*
       acc-cod-s02-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cod-s02-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cmn-zvs-002-ope      .
           move      w-tes-cod-s02 (1)    to   w-cmn-zvs-002-cod      .
           move      13                   to   w-cmn-zvs-002-lin      .
           move      30                   to   w-cmn-zvs-002-pos      .
           move      13                   to   w-cmn-zvs-002-dln      .
           move      37                   to   w-cmn-zvs-002-dps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "PRSC"               to   v-pfk (07)             .
           perform   cmn-zvs-002-cll-000  thru cmn-zvs-002-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cmn-zvs-002-foi-000  thru cmn-zvs-002-foi-999    .
       acc-cod-s02-110.
           perform   cmn-zvs-002-cll-000  thru cmn-zvs-002-cll-999    .
           if        w-cmn-zvs-002-ope    =    "F+"
                     go to acc-cod-s02-115.
           if        w-cmn-zvs-002-ope    =    "AC"
                     go to acc-cod-s02-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-s02-115.
           perform   cmn-zvs-002-foi-000  thru cmn-zvs-002-foi-999    .
           go to     acc-cod-s02-110.
       acc-cod-s02-120.
           move      w-cmn-zvs-002-cod    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-cod-s02-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-cod-s02-999.
       acc-cod-s02-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-cod-s02 (1)      .
       acc-cod-s02-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura tabella                             *
      *                  *---------------------------------------------*
           move      02                   to   w-let-arc-zvs-tip      .
           move      w-tes-cod-s02 (1)    to   w-let-arc-zvs-cod      .
           perform   let-arc-zvs-000      thru let-arc-zvs-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione descrizione                  *
      *                  *---------------------------------------------*
           move      w-let-arc-zvs-des    to   w-tes-cod-s02-des (1)  .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione                 *
      *                  *---------------------------------------------*
           perform   vis-des-cs2-000      thru vis-des-cs2-999        .
      *                  *---------------------------------------------*
      *                  * Se codice errato : reimpostazione           *
      *                  *---------------------------------------------*
           if        w-let-arc-zvs-flg    not  = spaces
                     go to acc-cod-s02-100.
       acc-cod-s02-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cod-s02-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-cod-s02-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-cod-s02-100.
       acc-cod-s02-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Codice statistico 2       *
      *    *-----------------------------------------------------------*
       vis-cod-s02-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      13                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-cod-s02 (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-s02-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Descrizione codice sta-   *
      *    * tistico 2                                                 *
      *    *-----------------------------------------------------------*
       vis-des-cs2-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      37                   to   v-pos                  .
           move      w-tes-cod-s02-des (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-des-cs2-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Codice statistico 3          *
      *    *-----------------------------------------------------------*
       acc-cod-s03-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cod-s03-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cmn-zvs-003-ope      .
           move      w-tes-cod-s03 (1)    to   w-cmn-zvs-003-cod      .
           move      14                   to   w-cmn-zvs-003-lin      .
           move      30                   to   w-cmn-zvs-003-pos      .
           move      14                   to   w-cmn-zvs-003-dln      .
           move      37                   to   w-cmn-zvs-003-dps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "PRSC"               to   v-pfk (07)             .
           perform   cmn-zvs-003-cll-000  thru cmn-zvs-003-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cmn-zvs-003-foi-000  thru cmn-zvs-003-foi-999    .
       acc-cod-s03-110.
           perform   cmn-zvs-003-cll-000  thru cmn-zvs-003-cll-999    .
           if        w-cmn-zvs-003-ope    =    "F+"
                     go to acc-cod-s03-115.
           if        w-cmn-zvs-003-ope    =    "AC"
                     go to acc-cod-s03-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-s03-115.
           perform   cmn-zvs-003-foi-000  thru cmn-zvs-003-foi-999    .
           go to     acc-cod-s03-110.
       acc-cod-s03-120.
           move      w-cmn-zvs-003-cod    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-cod-s03-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-cod-s03-999.
       acc-cod-s03-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-cod-s03 (1)      .
       acc-cod-s03-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura tabella                             *
      *                  *---------------------------------------------*
           move      03                   to   w-let-arc-zvs-tip      .
           move      w-tes-cod-s03 (1)    to   w-let-arc-zvs-cod      .
           perform   let-arc-zvs-000      thru let-arc-zvs-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione descrizione                  *
      *                  *---------------------------------------------*
           move      w-let-arc-zvs-des    to   w-tes-cod-s03-des (1)  .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione                 *
      *                  *---------------------------------------------*
           perform   vis-des-cs3-000      thru vis-des-cs3-999        .
      *                  *---------------------------------------------*
      *                  * Se codice errato : reimpostazione           *
      *                  *---------------------------------------------*
           if        w-let-arc-zvs-flg    not  = spaces
                     go to acc-cod-s03-100.
       acc-cod-s03-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cod-s03-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-cod-s03-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-cod-s03-100.
       acc-cod-s03-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Codice statistico 3       *
      *    *-----------------------------------------------------------*
       vis-cod-s03-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      14                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-cod-s03 (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-s03-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Descrizione codice sta-   *
      *    * tistico 3                                                 *
      *    *-----------------------------------------------------------*
       vis-des-cs3-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      37                   to   v-pos                  .
           move      w-tes-cod-s03-des (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-des-cs3-999.
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
           if        w-tes-num-mtv        =    zero
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
      *              *-------------------------------------------------*
      *              * Controllo che il valore impostato sia unico in  *
      *              * archivio                                        *
      *              *-------------------------------------------------*
           move      w-tes-num-mtv        to   w-ctl-uni-alf-num      .
           move      w-tes-alf-mtv (1)    to   w-ctl-uni-alf-alf      .
           perform   ctl-uni-alf-000      thru ctl-uni-alf-999        .
           if        w-ctl-uni-alf-flg    =    spaces
                     go to cnt-tdo-nok-002.
           move      "Codice materiale vario gia' utilizzato !"
                                          to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-002.
      *              *-------------------------------------------------*
      *              * Controllo su descrizione per uso interno        *
      *              *-------------------------------------------------*
           if        w-tes-des-int (1)    not  = spaces
                     go to cnt-tdo-nok-010.
           move      "Manca la descrizione per uso interno"
                                          to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-010.
      *              *-------------------------------------------------*
      *              * Controllo su gruppo merceologico                *
      *              *-------------------------------------------------*
           if        w-tes-cla-mtv (1)    =    zero
                     go to cnt-tdo-nok-020.
           if        w-tes-cla-mtv-sud (1)
                                          =    01
                     go to cnt-tdo-nok-020.
           if        w-tes-gru-mtv (1)    not  = zero
                     go to cnt-tdo-nok-020.
           move      "Manca il codice gruppo merceologico"
                                          to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-020.
      *              *-------------------------------------------------*
      *              * Controllo su sottogruppo merceologico           *
      *              *-------------------------------------------------*
           if        w-tes-cla-mtv (1)    =    zero
                     go to cnt-tdo-nok-500.
           if        w-tes-gru-mtv (1)    =    zero
                     go to cnt-tdo-nok-500.
           if        w-tes-cla-mtv-sud (1)
                                          =    01
                     go to cnt-tdo-nok-500.
           if        w-tes-gru-mtv-sud (1)
                                          =    01
                     go to cnt-tdo-nok-500.
           if        w-tes-sgr-mtv (1)    not  = zero
                     go to cnt-tdo-nok-500.
           move      "Manca il codice sottogruppo merceologico"
                                          to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-500.
      *              *-------------------------------------------------*
      *              * Normalizzazioni                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Si/no gestione magazzino fisico             *
      *                  *---------------------------------------------*
           if        w-tes-snx-mag (1)    =    spaces
                     move  "N"            to   w-tes-snx-mag (1)      .
      *                  *---------------------------------------------*
      *                  * Si/no gestione magazzino contabile          *
      *                  *---------------------------------------------*
           if        w-tes-snx-mac (1)    =    spaces
                     move  "N"            to   w-tes-snx-mac (1)      .
      *              *-------------------------------------------------*
      *              * Uscita per controlli tutti superati             *
      *              *-------------------------------------------------*
           go to     cnt-tdo-nok-999.
       cnt-tdo-nok-900.
      *              *-------------------------------------------------*
      *              * Emissione messaggio di errore e set del flag di *
      *              * uscita ad errore                                *
      *              *-------------------------------------------------*
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
           move      zero                 to   w-tes-num-mtv          .
           move      spaces               to   w-tes-num-mtv-aut      .
       nor-key-reg-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione dati non chiave testata                   *
      *    *-----------------------------------------------------------*
       nor-nok-tes-000.
           move      zero                 to   w-tes-ide-dat (1)      .
           move      spaces               to   w-tes-ide-ute (1)      .
           move      spaces               to   w-tes-ide-fas (1)      .
           move      spaces               to   w-tes-alf-mtv (1)      .
           move      spaces               to   w-tes-syn-mtv (1)      .
           move      spaces               to   w-tes-des-key (1)      .
           move      spaces               to   w-tes-des-int (1)      .
           move      zero                 to   w-tes-cla-mtv (1)      .
           move      spaces               to   w-tes-cla-mtv-des (1)  .
           move      zero                 to   w-tes-cla-mtv-sud (1)  .
           move      spaces               to   w-tes-cla-mtv-umi (1)  .
           move      zero                 to   w-tes-gru-mtv (1)      .
           move      spaces               to   w-tes-gru-mtv-des (1)  .
           move      zero                 to   w-tes-gru-mtv-sud (1)  .
           move      spaces               to   w-tes-gru-mtv-umi (1)  .
           move      zero                 to   w-tes-sgr-mtv (1)      .
           move      spaces               to   w-tes-sgr-mtv-des (1)  .
           move      zero                 to   w-tes-sgr-mtv-sud (1)  .
           move      spaces               to   w-tes-sgr-mtv-umi (1)  .
           move      zero                 to   w-tes-tip-mtv (1)      .
           move      spaces               to   w-tes-snx-mag (1)      .
           move      spaces               to   w-tes-snx-mac (1)      .
           move      zero                 to   w-tes-tip-cfz (1)      .
           move      zero                 to   w-tes-qta-cfz (1)      .
           move      zero                 to   w-tes-pes-uni (1)      .
           move      zero                 to   w-tes-pes-tar (1)      .
           move      zero                 to   w-tes-vol-uni (1)      .
           move      zero                 to   w-tes-dim-lar (1)      .
           move      zero                 to   w-tes-dim-alt (1)      .
           move      zero                 to   w-tes-dim-prf (1)      .
           move      spaces               to   w-tes-pcl-fis (1)      .
           move      zero                 to   w-tes-coe-mol (1)      .
           move      zero                 to   w-tes-coe-div (1)      .
           move      spaces               to   w-tes-umi-gst (1)      .
           move      spaces               to   w-tes-umi-gst-des (1)  .
           move      zero                 to   w-tes-dec-qta (1)      .
           move      zero                 to   w-tes-snx-2qt (1)      .
           move      zero                 to   w-tes-dec-2qt (1)      .
           move      zero                 to   w-tes-snx-3qt (1)      .
           move      zero                 to   w-tes-dec-3qt (1)      .
           move      spaces               to   w-tes-tip-vpr (1)      .
           move      spaces               to   w-tes-tip-vpr-des (1)  .
           move      zero                 to   w-tes-cod-s01 (1)      .
           move      spaces               to   w-tes-cod-s01-des (1)  .
           move      zero                 to   w-tes-cod-s02 (1)      .
           move      spaces               to   w-tes-cod-s02-des (1)  .
           move      zero                 to   w-tes-cod-s03 (1)      .
           move      spaces               to   w-tes-cod-s03-des (1)  .
           move      zero                 to   w-tes-cla-bdg (1)      .
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
           move      "NUMMTV    "         to   f-key                  .
           move      w-tes-num-mtv        to   rf-mtv-num-mtv         .
           move      "pgm/mtv/fls/ioc/obj/iofmtv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mtv                 .
      *                  *---------------------------------------------*
      *                  * Test su esito lettura                       *
      *                  *---------------------------------------------*
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
      *                          *-------------------------------------*
      *                          * Valori contenuti direttamente in    *
      *                          * record [mtv]                        *
      *                          *-------------------------------------*
           move      rf-mtv-ide-dat       to   w-tes-ide-dat (1)      .
           move      rf-mtv-ide-ute       to   w-tes-ide-ute (1)      .
           move      rf-mtv-ide-fas       to   w-tes-ide-fas (1)      .
           move      rf-mtv-alf-mtv       to   w-tes-alf-mtv (1)      .
           move      rf-mtv-syn-mtv       to   w-tes-syn-mtv (1)      .
           move      rf-mtv-des-key       to   w-tes-des-key (1)      .
           move      rf-mtv-des-mtv       to   w-tes-des-int (1)      .
           move      rf-mtv-cla-mtv       to   w-tes-cla-mtv (1)      .
           move      rf-mtv-gru-mtv       to   w-tes-gru-mtv (1)      .
           move      rf-mtv-sgr-mtv       to   w-tes-sgr-mtv (1)      .
           move      rf-mtv-tip-mtv       to   w-tes-tip-mtv (1)      .
           move      rf-mtv-snx-mag       to   w-tes-snx-mag (1)      .
           move      rf-mtv-snx-mac       to   w-tes-snx-mac (1)      .
           move      rf-mtv-tip-cfz       to   w-tes-tip-cfz (1)      .
           move      rf-mtv-qta-cfz       to   w-tes-qta-cfz (1)      .
           move      rf-mtv-pes-uni       to   w-tes-pes-uni (1)      .
           move      rf-mtv-pes-tar       to   w-tes-pes-tar (1)      .
           move      rf-mtv-vol-uni       to   w-tes-vol-uni (1)      .
           move      rf-mtv-dim-lar       to   w-tes-dim-lar (1)      .
           move      rf-mtv-dim-alt       to   w-tes-dim-alt (1)      .
           move      rf-mtv-dim-prf       to   w-tes-dim-prf (1)      .
           move      rf-mtv-pcl-fis       to   w-tes-pcl-fis (1)      .
           move      rf-mtv-coe-mol       to   w-tes-coe-mol (1)      .
           move      rf-mtv-coe-div       to   w-tes-coe-div (1)      .
           move      rf-mtv-umi-gst       to   w-tes-umi-gst (1)      .
           move      rf-mtv-dec-qta       to   w-tes-dec-qta (1)      .
           move      rf-mtv-snx-2qt       to   w-tes-snx-2qt (1)      .
           move      rf-mtv-dec-2qt       to   w-tes-dec-2qt (1)      .
           move      rf-mtv-snx-3qt       to   w-tes-snx-3qt (1)      .
           move      rf-mtv-dec-3qt       to   w-tes-dec-3qt (1)      .
           move      rf-mtv-tip-vpr       to   w-tes-tip-vpr (1)      .
           move      rf-mtv-cod-s01       to   w-tes-cod-s01 (1)      .
           move      rf-mtv-cod-s02       to   w-tes-cod-s02 (1)      .
           move      rf-mtv-cod-s03       to   w-tes-cod-s03 (1)      .
           move      rf-mtv-cla-bdg       to   w-tes-cla-bdg (1)      .
           move      rf-mtv-alx-exp       to   w-tes-alx-exp (1)      .
       rou-let-reg-250.
      *                          *-------------------------------------*
      *                          * Valori contenuti indirettamente in  *
      *                          * record [mtv]                        *
      *                          *-------------------------------------*
       rou-let-reg-300.
      *                              *---------------------------------*
      *                              * Lettura archivio [zv1]          *
      *                              *---------------------------------*
           move      w-tes-cla-mtv (1)    to   w-let-arc-zv1-cla      .
           perform   let-arc-zv1-000      thru let-arc-zv1-999        .
           move      w-let-arc-zv1-des    to   w-tes-cla-mtv-des (1)  .
           move      w-let-arc-zv1-sud    to   w-tes-cla-mtv-sud (1)  .
           move      w-let-arc-zv1-umi    to   w-tes-cla-mtv-umi (1)  .
      *                              *---------------------------------*
      *                              * Lettura archivio [zv2]          *
      *                              *---------------------------------*
           move      w-tes-cla-mtv (1)    to   w-let-arc-zv2-cla      .
           move      w-tes-gru-mtv (1)    to   w-let-arc-zv2-gru      .
           perform   let-arc-zv2-000      thru let-arc-zv2-999        .
           move      w-let-arc-zv2-des    to   w-tes-gru-mtv-des (1)  .
           move      w-let-arc-zv2-sud    to   w-tes-gru-mtv-sud (1)  .
           move      w-let-arc-zv2-umi    to   w-tes-gru-mtv-umi (1)  .
      *                              *---------------------------------*
      *                              * Lettura archivio [zv3]          *
      *                              *---------------------------------*
           move      w-tes-cla-mtv (1)    to   w-let-arc-zv3-cla      .
           move      w-tes-gru-mtv (1)    to   w-let-arc-zv3-gru      .
           move      w-tes-sgr-mtv (1)    to   w-let-arc-zv3-sgr      .
           perform   let-arc-zv3-000      thru let-arc-zv3-999        .
           move      w-let-arc-zv3-des    to   w-tes-sgr-mtv-des (1)  .
           move      w-let-arc-zv3-sud    to   w-tes-sgr-mtv-sud (1)  .
           move      w-let-arc-zv3-umi    to   w-tes-sgr-mtv-umi (1)  .
      *                              *---------------------------------*
      *                              * Lettura archivio [zum]          *
      *                              *---------------------------------*
           move      w-tes-umi-gst (1)    to   w-let-arc-zum-cod      .
           perform   let-arc-zum-000      thru let-arc-zum-999        .
           move      w-let-arc-zum-des    to   w-tes-umi-gst-des (1)  .
      *                              *---------------------------------*
      *                              * Lettura archivio [zvs]          *
      *                              *---------------------------------*
           move      01                   to   w-let-arc-zvs-tip      .
           move      w-tes-cod-s01 (1)    to   w-let-arc-zvs-cod      .
           perform   let-arc-zvs-000      thru let-arc-zvs-999        .
           move      w-let-arc-zvs-des    to   w-tes-cod-s01-des (1)  .
      *                              *---------------------------------*
      *                              * Lettura archivio [zvs]          *
      *                              *---------------------------------*
           move      02                   to   w-let-arc-zvs-tip      .
           move      w-tes-cod-s02 (1)    to   w-let-arc-zvs-cod      .
           perform   let-arc-zvs-000      thru let-arc-zvs-999        .
           move      w-let-arc-zvs-des    to   w-tes-cod-s02-des (1)  .
      *                              *---------------------------------*
      *                              * Lettura archivio [zvs]          *
      *                              *---------------------------------*
           move      03                   to   w-let-arc-zvs-tip      .
           move      w-tes-cod-s03 (1)    to   w-let-arc-zvs-cod      .
           perform   let-arc-zvs-000      thru let-arc-zvs-999        .
           move      w-let-arc-zvs-des    to   w-tes-cod-s03-des (1)  .
      *                          *-------------------------------------*
      *                          * Valori precedenti anagrafica        *
      *                          *-------------------------------------*
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
      *    * Routine pre-accettazioni per inserimento                  *
      *    *-----------------------------------------------------------*
       pre-acc-ins-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-pre-acc-ins      .
      *              *-------------------------------------------------*
      *              * Duplicazione record precedente se richiesto     *
      *              *-------------------------------------------------*
           if        w-cnt-dup-rec-flg    =    spaces
                     go to pre-acc-ins-999.
      *                  *---------------------------------------------*
      *                  * Routine di duplicazione record              *
      *                  *---------------------------------------------*
           perform   rou-dup-rec-000      thru rou-dup-rec-999        .
       pre-acc-ins-999.
           exit.

      *    *===========================================================*
      *    * Routine duplicazione record                               *
      *    *-----------------------------------------------------------*
       rou-dup-rec-000.
      *              *-------------------------------------------------*
      *              * Copia dei valori di testata precedenti          *
      *              *-------------------------------------------------*
           move      w-tes-val-aep (2)    to   w-tes-val-aep (1)      .
      *              *-------------------------------------------------*
      *              * Normalizzazione flags di controllo              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Flag di impostazione pagine di testata      *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cnt-sts-imp-tes      .
      *                  *---------------------------------------------*
      *                  * Flags di impostazione pagine di testata     *
      *                  * eseguita                                    *
      *                  *---------------------------------------------*
           move      all "#"              to   w-cnt-sts-imp-pte      .
      *                  *---------------------------------------------*
      *                  * Flags di ingresso in pagine di testata      *
      *                  * eseguito                                    *
      *                  *---------------------------------------------*
           move      all "#"              to   w-cnt-sts-ing-pte      .
      *                  *---------------------------------------------*
      *                  * Flags di visualizzazione per le pagine      *
      *                  *---------------------------------------------*
           move      spaces               to   w-cnt-sts-vis-ptx (2)  .
           move      spaces               to   w-cnt-sts-vis-ptx (3)  .
           move      spaces               to   w-cnt-sts-vis-ptx (4)  .
           move      spaces               to   w-cnt-sts-vis-ptx (5)  .
           move      spaces               to   w-cnt-sts-vis-ptx (6)  .
           move      spaces               to   w-cnt-sts-vis-ptx (7)  .
           move      spaces               to   w-cnt-sts-vis-ptx (8)  .
           move      spaces               to   w-cnt-sts-vis-ptx (9)  .
      *              *-------------------------------------------------*
      *              * Normalizzazione segnale di duplicazione         *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-dup-rec-flg      .
      *              *-------------------------------------------------*
      *              * Visualizzazione pagine di testata               *
      *              *-------------------------------------------------*
           perform   vis-tes-reg-000      thru vis-tes-reg-999        .
       rou-dup-rec-999.
           exit.

      *    *===========================================================*
      *    * Routine pre-accettazioni per modifica                     *
      *    *-----------------------------------------------------------*
       pre-acc-mod-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-pre-acc-mod      .
      *              *-------------------------------------------------*
      *              * Normalizzazione segnale di duplicazione         *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-dup-rec-flg      .
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
      *    * Routine pre-richiesta di ratifica tasto Delete            *
      *    *-----------------------------------------------------------*
       pre-snx-del-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-pre-snx-del      .
       pre-snx-del-999.
           exit.

      *    *===========================================================*
      *    * Routine post-richiesta di ratifica tasto Delete           *
      *    *-----------------------------------------------------------*
       pos-snx-del-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-pos-snx-del      .
       pos-snx-del-999.
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
           if        w-tes-num-mtv-aut    =    spaces
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
      *              * Trattamento file [mtv]                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se inserimento                              *
      *                  *---------------------------------------------*
           if        w-cnt-mfu-tip-fun    not  = "I"
                     go to scr-mov-fil-500.
      *                      *-----------------------------------------*
      *                      * Write record [mtv]                      *
      *                      *-----------------------------------------*
           perform   wrt-rec-mtv-000      thru wrt-rec-mtv-999        .
           go to     scr-mov-fil-999.
       scr-mov-fil-500.
      *                  *---------------------------------------------*
      *                  * Se modifica                                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Rewrite record [mtv]                    *
      *                      *-----------------------------------------*
           perform   rew-rec-mtv-000      thru rew-rec-mtv-999        .
       scr-mov-fil-999.
           exit.

      *    *===========================================================*
      *    * Delete movimento da file                                  *
      *    *-----------------------------------------------------------*
       del-mov-fil-000.
      *              *-------------------------------------------------*
      *              * Delete record [mtv]                             *
      *              *-------------------------------------------------*
           perform   del-rec-mtv-000      thru del-rec-mtv-999        .
       del-mov-fil-999.
           exit.

      *    *===========================================================*
      *    * Composizione record [mtv]                                 *
      *    *-----------------------------------------------------------*
       cmp-rec-mtv-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione record                          *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/mtv/fls/ioc/obj/iofmtv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mtv                 .
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Campi chiave                                *
      *                  *---------------------------------------------*
           move      w-tes-num-mtv        to   rf-mtv-num-mtv         .
      *                  *---------------------------------------------*
      *                  * Campi non chiave                            *
      *                  *---------------------------------------------*
           move      "IG"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-dat                to   rf-mtv-ide-dat         .
           move      s-ute                to   rf-mtv-ide-ute         .
           move      s-fas                to   rf-mtv-ide-fas         .
           move      w-tes-alf-mtv (1)    to   rf-mtv-alf-mtv         .
           move      w-tes-syn-mtv (1)    to   rf-mtv-syn-mtv         .
           move      w-tes-des-key (1)    to   rf-mtv-des-key         .
           move      w-tes-des-int (1)    to   rf-mtv-des-mtv         .
           move      w-tes-cla-mtv (1)    to   rf-mtv-cla-mtv         .
           move      w-tes-gru-mtv (1)    to   rf-mtv-gru-mtv         .
           move      w-tes-sgr-mtv (1)    to   rf-mtv-sgr-mtv         .
           move      w-tes-tip-mtv (1)    to   rf-mtv-tip-mtv         .
           move      w-tes-snx-mag (1)    to   rf-mtv-snx-mag         .
           move      w-tes-snx-mac (1)    to   rf-mtv-snx-mac         .
           move      w-tes-tip-cfz (1)    to   rf-mtv-tip-cfz         .
           move      w-tes-qta-cfz (1)    to   rf-mtv-qta-cfz         .
           move      w-tes-pes-uni (1)    to   rf-mtv-pes-uni         .
           move      w-tes-pes-tar (1)    to   rf-mtv-pes-tar         .
           move      w-tes-vol-uni (1)    to   rf-mtv-vol-uni         .
           move      w-tes-dim-lar (1)    to   rf-mtv-dim-lar         .
           move      w-tes-dim-alt (1)    to   rf-mtv-dim-alt         .
           move      w-tes-dim-prf (1)    to   rf-mtv-dim-prf         .
           move      w-tes-pcl-fis (1)    to   rf-mtv-pcl-fis         .
           move      w-tes-coe-mol (1)    to   rf-mtv-coe-mol         .
           move      w-tes-coe-div (1)    to   rf-mtv-coe-div         .
           move      w-tes-umi-gst (1)    to   rf-mtv-umi-gst         .
           move      w-tes-dec-qta (1)    to   rf-mtv-dec-qta         .
           move      w-tes-snx-2qt (1)    to   rf-mtv-snx-2qt         .
           move      w-tes-dec-2qt (1)    to   rf-mtv-dec-2qt         .
           move      w-tes-snx-3qt (1)    to   rf-mtv-snx-3qt         .
           move      w-tes-dec-3qt (1)    to   rf-mtv-dec-3qt         .
           move      w-tes-tip-vpr (1)    to   rf-mtv-tip-vpr         .
           move      w-tes-cod-s01 (1)    to   rf-mtv-cod-s01         .
           move      w-tes-cod-s02 (1)    to   rf-mtv-cod-s02         .
           move      w-tes-cod-s03 (1)    to   rf-mtv-cod-s03         .
           move      w-tes-cla-bdg (1)    to   rf-mtv-cla-bdg         .
           move      w-tes-alx-exp (1)    to   rf-mtv-alx-exp         .
       cmp-rec-mtv-999.
           exit.

      *    *===========================================================*
      *    * Scrittura record [mtv]                                    *
      *    *-----------------------------------------------------------*
       wrt-rec-mtv-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-mtv-000      thru cmp-rec-mtv-999        .
      *              *-------------------------------------------------*
      *              * Put record                                      *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      "pgm/mtv/fls/ioc/obj/iofmtv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mtv                 .
       wrt-rec-mtv-999.
           exit.

      *    *===========================================================*
      *    * Riscrittura record [mtv]                                  *
      *    *-----------------------------------------------------------*
       rew-rec-mtv-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-mtv-000      thru cmp-rec-mtv-999        .
      *              *-------------------------------------------------*
      *              * Forced put record                               *
      *              *-------------------------------------------------*
           move      "FP"                 to   f-ope                  .
           move      "pgm/mtv/fls/ioc/obj/iofmtv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mtv                 .
       rew-rec-mtv-999.
           exit.

      *    *===========================================================*
      *    * Cancellazione record [mtv]                                *
      *    *-----------------------------------------------------------*
       del-rec-mtv-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-mtv-000      thru cmp-rec-mtv-999        .
      *              *-------------------------------------------------*
      *              * Delete record                                   *
      *              *-------------------------------------------------*
           move      "DE"                 to   f-ope                  .
           move      "pgm/mtv/fls/ioc/obj/iofmtv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mtv                 .
       del-rec-mtv-999.
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
           move      13                   to   v-pos                  .
           move      14                   to   v-lto                  .
           move      68                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Messaggio nel box                               *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      48                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      15                   to   v-pos                  .
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
           move      64                   to   v-pos                  .
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
           move      65                   to   v-pos                  .
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
      *    * Routine lettura tabella [zv1]                             *
      *    *-----------------------------------------------------------*
       let-arc-zv1-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zv1-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice classe a zero                    *
      *              *-------------------------------------------------*
           if        w-let-arc-zv1-cla    =    zero
                     go to let-arc-zv1-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCLA    "         to   f-key                  .
           move      w-let-arc-zv1-cla    to   rf-zv1-cod-cla         .
           move      "pgm/mtv/fls/ioc/obj/iofzv1"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zv1                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-zv1-400.
       let-arc-zv1-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-zv1-des-cla       to   w-let-arc-zv1-des      .
           move      rf-zv1-ult-sud       to   w-let-arc-zv1-sud      .
           move      rf-zv1-umi-def       to   w-let-arc-zv1-umi      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-zv1-999.
       let-arc-zv1-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-zv1-flg      .
           move      all   "."            to   w-let-arc-zv1-des      .
           go to     let-arc-zv1-600.
       let-arc-zv1-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zv1-des      .
       let-arc-zv1-600.
           move      zero                 to   w-let-arc-zv1-sud      .
           move      spaces               to   w-let-arc-zv1-umi      .
       let-arc-zv1-999.
           exit.

      *    *===========================================================*
      *    * Routine lettura tabella [zv2]                             *
      *    *-----------------------------------------------------------*
       let-arc-zv2-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zv2-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice gruppo a zero                    *
      *              *-------------------------------------------------*
           if        w-let-arc-zv2-gru    =    zero
                     go to let-arc-zv2-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODGRU    "         to   f-key                  .
           move      w-let-arc-zv2-cla    to   rf-zv2-cod-cla         .
           move      w-let-arc-zv2-gru    to   rf-zv2-cod-gru         .
           move      "pgm/mtv/fls/ioc/obj/iofzv2"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zv2                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-zv2-400.
       let-arc-zv2-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-zv2-des-gru       to   w-let-arc-zv2-des      .
           move      rf-zv2-ult-sud       to   w-let-arc-zv2-sud      .
           move      rf-zv2-umi-def       to   w-let-arc-zv2-umi      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-zv2-999.
       let-arc-zv2-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-zv2-flg      .
           move      all   "."            to   w-let-arc-zv2-des      .
           go to     let-arc-zv2-600.
       let-arc-zv2-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zv2-des      .
       let-arc-zv2-600.
           move      zero                 to   w-let-arc-zv2-sud      .
           move      spaces               to   w-let-arc-zv2-umi      .
       let-arc-zv2-999.
           exit.

      *    *===========================================================*
      *    * Routine lettura tabella [zv3]                             *
      *    *-----------------------------------------------------------*
       let-arc-zv3-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zv3-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice sottogruppo a zero               *
      *              *-------------------------------------------------*
           if        w-let-arc-zv3-sgr    =    zero
                     go to let-arc-zv3-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODSGR    "         to   f-key                  .
           move      w-let-arc-zv3-cla    to   rf-zv3-cod-cla         .
           move      w-let-arc-zv3-gru    to   rf-zv3-cod-gru         .
           move      w-let-arc-zv3-sgr    to   rf-zv3-cod-sgr         .
           move      "pgm/mtv/fls/ioc/obj/iofzv3"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zv3                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-zv3-400.
       let-arc-zv3-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-zv3-des-sgr       to   w-let-arc-zv3-des      .
           move      rf-zv3-ult-sud       to   w-let-arc-zv3-sud      .
           move      rf-zv3-umi-def       to   w-let-arc-zv3-umi      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-zv3-999.
       let-arc-zv3-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-zv3-flg      .
           move      all   "."            to   w-let-arc-zv3-des      .
           go to     let-arc-zv3-600.
       let-arc-zv3-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zv3-des      .
       let-arc-zv3-600.
           move      zero                 to   w-let-arc-zv3-sud      .
           move      spaces               to   w-let-arc-zv3-umi      .
       let-arc-zv3-999.
           exit.

      *    *===========================================================*
      *    * Routine lettura tabella [zum]                             *
      *    *-----------------------------------------------------------*
       let-arc-zum-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zum-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice unita' di misura a spaces        *
      *              *-------------------------------------------------*
           if        w-let-arc-zum-cod    =    spaces
                     go to let-arc-zum-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODUMI    "         to   f-key                  .
           move      w-let-arc-zum-cod    to   rf-zum-cod-umi         .
           move      "pgm/dcp/fls/ioc/obj/iofzum"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zum                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-zum-400.
       let-arc-zum-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-zum-des-umi       to   w-let-arc-zum-des      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-zum-999.
       let-arc-zum-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-zum-flg      .
           move      all   "."            to   w-let-arc-zum-des      .
           go to     let-arc-zum-999.
       let-arc-zum-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zum-des      .
       let-arc-zum-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [zvs]                         *
      *    *-----------------------------------------------------------*
       let-arc-zvs-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zvs-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice statistico a zero                *
      *              *-------------------------------------------------*
           if        w-let-arc-zvs-cod    =    zero
                     go to let-arc-zvs-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCLS    "         to   f-key                  .
           move      w-let-arc-zvs-tip    to   rf-zvs-tip-cls         .
           move      w-let-arc-zvs-cod    to   rf-zvs-cod-cls         .
           move      "pgm/mtv/fls/ioc/obj/iofzvs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zvs                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-zvs-400.
       let-arc-zvs-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-zvs-des-cls       to   w-let-arc-zvs-des      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-zvs-999.
       let-arc-zvs-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-zvs-flg      .
           move      all   "."            to   w-let-arc-zvs-des      .
           go to     let-arc-zvs-999.
       let-arc-zvs-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zvs-des      .
       let-arc-zvs-999.
           exit.

      *    *===========================================================*
      *    * Controllo unicita' codice materia varia alfanumerico      *
      *    *-----------------------------------------------------------*
       ctl-uni-alf-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-ctl-uni-alf-flg      .
      *              *-------------------------------------------------*
      *              * Contatore records letti con chiave alfanumerica *
      *              * pari a quella passata, ma con chiave numerica   *
      *              * diversa da quella passata : a zero              *
      *              *-------------------------------------------------*
           move      zero                 to   w-ctl-uni-alf-ctr      .
      *              *-------------------------------------------------*
      *              * Start su file [mtv]                             *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "ALFMTV    "         to   f-key                  .
           move      w-ctl-uni-alf-alf    to   rf-mtv-alf-mtv         .
           move      zero                 to   rf-mtv-num-mtv         .
           move      "pgm/mtv/fls/ioc/obj/iofmtv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mtv                 .
      *              *-------------------------------------------------*
      *              * Se errata : a trattamento finale                *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to ctl-uni-alf-600.
       ctl-uni-alf-100.
      *              *-------------------------------------------------*
      *              * Lettura sequenziale file [mtv]                  *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/mtv/fls/ioc/obj/iofmtv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mtv                 .
      *              *-------------------------------------------------*
      *              * Se 'at end' : a trattamento finale              *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to ctl-uni-alf-600.
      *              *-------------------------------------------------*
      *              * Test sul massimo                                *
      *              *-------------------------------------------------*
           if        rf-mtv-alf-mtv       not  = w-ctl-uni-alf-alf
                     go to ctl-uni-alf-600.
      *              *-------------------------------------------------*
      *              * Se valore numerico letto diverso da quello pas- *
      *              * sato incremento il contatore                    *
      *              *-------------------------------------------------*
           if        rf-mtv-num-mtv       not  = w-ctl-uni-alf-num
                     add   1              to   w-ctl-uni-alf-ctr      .
      *              *-------------------------------------------------*
      *              * Riciclo in lettura                              *
      *              *-------------------------------------------------*
           go to     ctl-uni-alf-100.
       ctl-uni-alf-600.
      *              *-------------------------------------------------*
      *              * Trattamento finale                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se il contatore e' a zero : uscita Ok       *
      *                  *---------------------------------------------*
           if        w-ctl-uni-alf-ctr    =    zero
                     go to ctl-uni-alf-999.
      *                  *---------------------------------------------*
      *                  * Altrimenti : uscita per errore              *
      *                  *---------------------------------------------*
           move      "#"                  to   w-ctl-uni-alf-flg      .
       ctl-uni-alf-999.
           exit.

      *    *===========================================================*
      *    * Subroutines per accettazione codice materia varia         *
      *    *-----------------------------------------------------------*
           copy      "pgm/mtv/prg/cpy/acodmtv0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice classe          *
      *    *-----------------------------------------------------------*
           copy      "pgm/mtv/prg/cpy/acmnzv10.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice gruppo          *
      *    *-----------------------------------------------------------*
           copy      "pgm/mtv/prg/cpy/acmnzv20.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice sottogruppo     *
      *    *-----------------------------------------------------------*
           copy      "pgm/mtv/prg/cpy/acmnzv30.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione unita' di misura           *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/acodzum0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione tipo variante              *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/acodztv0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice statistico 1    *
      *    *-----------------------------------------------------------*
           copy      "pgm/mtv/prg/cpy/acmnzvs1.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice statistico 2    *
      *    *-----------------------------------------------------------*
           copy      "pgm/mtv/prg/cpy/acmnzvs2.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice statistico 3    *
      *    *-----------------------------------------------------------*
           copy      "pgm/mtv/prg/cpy/acmnzvs3.acs"                   .

      *    *===========================================================*
      *    * Routine di attribuzione codice automatico progressivo     *
      *    *-----------------------------------------------------------*
       att-cod-aut-000.
      *              *-------------------------------------------------*
      *              * Lettura codice automatico per [mtv]             *
      *              *-------------------------------------------------*
           move      "Eg"                 to   s-ope                  .
           move      "mtv "               to   s-nam                  .
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
           move      "mtv "               to   s-nam                  .
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
           move      s-num                to   w-enc-mtv-val-pre      .
      *                  *---------------------------------------------*
      *                  * Incremento del valore                       *
      *                  *---------------------------------------------*
           move      w-enc-mtv-val-pre    to   w-enc-mtv-val-pos      .
           add       1                    to   w-enc-mtv-val-pos      .
       att-cod-aut-500.
      *                  *---------------------------------------------*
      *                  * Se l'incremento porta a zero si forza il    *
      *                  * valore a 1                                  *
      *                  *---------------------------------------------*
           if        w-enc-mtv-val-pos    =    zero
                     move  1              to   w-enc-mtv-val-pos      .
      *                  *---------------------------------------------*
      *                  * Controllo se esiste gia' un record con il   *
      *                  * codice pari al valore incrementato          *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMMTV    "         to   f-key                  .
           move      w-enc-mtv-val-pos    to   rf-mtv-num-mtv         .
           move      "pgm/mtv/fls/ioc/obj/iofmtv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mtv                 .
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
           add       1                    to   w-enc-mtv-val-pos      .
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
           move      "mtv "               to   s-nam                  .
           move      w-enc-mtv-val-pos    to   s-num                  .
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
      *              * Lettura codice automatico per [mtv]             *
      *              *-------------------------------------------------*
           move      "Eg"                 to   s-ope                  .
           move      "mtv "               to   s-nam                  .
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
           if        s-num                =    w-enc-mtv-val-pos
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
           move      "mtv "               to   s-nam                  .
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
           move      "mtv "               to   s-nam                  .
           move      w-enc-mtv-val-pre    to   s-num                  .
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

