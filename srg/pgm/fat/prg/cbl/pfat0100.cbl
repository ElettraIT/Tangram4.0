       Identification Division.
       Program-Id.                                 pfat0100           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    fat                 *
      *                                Settore:    prs                 *
      *                                   Fase:    fat010              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 30/07/91    *
      *                       Ultima revisione:    NdK del 20/04/24    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Gestione tabella tipi movimento documenti   *
      *                    di vendita - fatture                        *
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
                     "fat"                                            .
      *        *-------------------------------------------------------*
      *        * Settore gestionale                                    *
      *        *-------------------------------------------------------*
           05  i-ide-set                  pic  x(03) value
                     "prs"                                            .
      *        *-------------------------------------------------------*
      *        * Fase gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-fas                  pic  x(06) value
                     "fat010"                                         .
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10) value
                     "pfat0100"                                       .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  i-ide-des                  pic  x(40) value
                     "GESTIONE TIPI MOVIMENTO PER FATTURAZIONE"       .

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
      *    * Area di comunicazione per modulo                "mpslct"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/r"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per moduli                "mppssx"  *
      *    *-----------------------------------------------------------*
       01  j.
      *        *-------------------------------------------------------*
      *        * Tipo operazione                                       *
      *        *-------------------------------------------------------*
           05  j-ope                      pic  x(02)                  .
      *        *-------------------------------------------------------*
      *        * Tipo record                                           *
      *        *-------------------------------------------------------*
           05  j-tre                      pic  x(04)                  .
      *        *-------------------------------------------------------*
      *        * Chiave record                                         *
      *        *-------------------------------------------------------*
           05  j-kre                      pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Dati record                                           *
      *        *-------------------------------------------------------*
           05  j-dat.
               10  j-chr occurs 2048      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Return status code                                    *
      *        *-------------------------------------------------------*
           05  j-rsc                      pic  x(02)                  .
      *        *-------------------------------------------------------*
      *        * Return message                                        *
      *        *-------------------------------------------------------*
           05  j-msg                      pic  x(80)                  .

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
      *        * [zfi]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/fat/fls/rec/rfzfi"                          .
      *        *-------------------------------------------------------*
      *        * [zac]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/fat/fls/rec/rfzac"                          .
      *        *-------------------------------------------------------*
      *        * [pdc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfpdc"                          .
      *        *-------------------------------------------------------*
      *        * [zcc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfzcc"                          .
      *        *-------------------------------------------------------*
      *        * [cli]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfcli"                          .
      *        *-------------------------------------------------------*
      *        * [ada]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/azi/fls/rec/rfada"                          .

      *    *===========================================================*
      *    * Work-area per bufferizzazione testata                     *
      *    *-----------------------------------------------------------*
       01  w-tes.
      *        *-------------------------------------------------------*
      *        * Valori chiave                                         *
      *        *-------------------------------------------------------*
           05  w-tes-val-key.
               10  w-tes-cod-tmo          pic  x(05)                  .
      *        *-------------------------------------------------------*
      *        * Valori attuali e precedenti                           *
      *        *-------------------------------------------------------*
           05  w-tes-val-aep occurs 2.
               10  w-tes-des-key          pic  x(30)                  .
               10  w-tes-des-tmo          pic  x(30)                  .
               10  w-tes-pwd-tmo          pic  x(08)                  .
               10  w-tes-vld-dpz          pic  9(02)                  .
               10  w-tes-cod-dpz          pic  9(02)                  .
               10  w-tes-cod-dpz-den      pic  x(20)                  .
               10  w-tes-tip-doc          pic  9(02)                  .
               10  w-tes-tdo-fel          pic  x(05)                  .
               10  w-tes-org-doc          pic  9(02)                  .
               10  w-tes-prv-doc          pic  9(02)                  .
               10  w-tes-sgl-num          pic  x(03)                  .
               10  w-tes-num-giv          pic  9(02)                  .
               10  w-tes-des-stp          pic  x(25)                  .
               10  w-tes-cau-cge          pic  9(03)                  .
               10  w-tes-cau-cge-des      pic  x(30)                  .
               10  w-tes-cau-cge-tmi      pic  x(01)                  .
               10  w-tes-ctp-ivv          pic  9(07)                  .
               10  w-tes-ctp-ivv-des      pic  x(40)                  .
               10  w-tes-ctp-ven          pic  9(07)                  .
               10  w-tes-ctp-ven-des      pic  x(40)                  .
               10  w-tes-def-tpr          pic  x(05)                  .
               10  w-tes-snx-age          pic  x(01)                  .
               10  w-tes-snx-rco          pic  x(01)                  .
               10  w-tes-snx-rdt          pic  x(01)                  .
               10  w-tes-cod-dct          pic  9(03)                  .
               10  w-tes-txt-dct.
                   15  w-tes-dct-rig occurs 10
                                          pic  x(40)                  .
               10  w-tes-cli-aft          pic  9(07)                  .
               10  w-tes-cli-aft-rag      pic  x(40)                  .
               10  w-tes-cod-mod          pic  x(08)                  .
               10  w-tes-cod-mod-des      pic  x(40)                  .
               10  w-tes-alx-exp.
                   15  filler occurs 60   pic  x(01)                  .

      *    *===========================================================*
      *    * Work-area personalizzazioni                               *
      *    *-----------------------------------------------------------*
       01  w-prs.
      *        *-------------------------------------------------------*
      *        * Numero livelli del piano dei conti                    *
      *        *-------------------------------------------------------*
           05  w-prs-liv-pdc              pic  9(01)                  .

      *    *===========================================================*
      *    * Work-area per salvataggi                                  *
      *    *-----------------------------------------------------------*
       01  w-sav.
      *        *-------------------------------------------------------*
      *        * Causale contabile                                     *
      *        *-------------------------------------------------------*
           05  w-sav-cau-cge              pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Validita' per le dipendenze                           *
      *        *-------------------------------------------------------*
           05  w-sav-vld-dpz              pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Codice dicitura                                       *
      *        *-------------------------------------------------------*
           05  w-sav-cod-dct              pic  9(03)                  .

      *    *===========================================================*
      *    * Work per accettazioni campi espansi                       *
      *    *-----------------------------------------------------------*
       01  w-exp.
      *        *-------------------------------------------------------*
      *        * Work per : Validita' per le dipendenze                *
      *        *-------------------------------------------------------*
           05  w-exp-vld-dpz.
               10  w-exp-vld-dpz-num      pic  9(02)       value 2    .
               10  w-exp-vld-dpz-lun      pic  9(02)       value 30   .
               10  w-exp-vld-dpz-tbl.
                   15  filler             pic  x(30) value
                            "valido per Tutte le dipendenze"          .
                   15  filler             pic  x(30) value
                            "valido per Una sola dipendenza"          .
      *        *-------------------------------------------------------*
      *        * Work per : Tipo documento                             *
      *        *-------------------------------------------------------*
           05  w-exp-tip-doc.
               10  w-exp-tip-doc-num      pic  9(02)       value 6    .
               10  w-exp-tip-doc-lun      pic  9(02)       value 20   .
               10  w-exp-tip-doc-tbl.
                   15  filler             pic  x(20) value
                            "Fattura             "                    .
                   15  filler             pic  x(20) value
                            "nota Di addebito    "                    .
                   15  filler             pic  x(20) value
                            "nota di Credito     "                    .
                   15  filler             pic  x(20) value
                            "fattura Pro-forma   "                    .
                   15  filler             pic  x(20) value
                            "Autofattura         "                    .
                   15  filler             pic  x(20) value
                            "acconto Su fattura  "                    .
      *        *-------------------------------------------------------*
      *        * Work per : Tipi fattura elettronica                   *
      *        *-------------------------------------------------------*
           05  w-exp-tdo-fel.
               10  w-exp-tdo-fel-num      pic  9(02)       value 18   .
               10  w-exp-tdo-fel-lun      pic  9(02)       value 40   .
               10  w-exp-tdo-fel-tbl.
                   15  filler             pic  x(40) value
                            "TD01 - Fattura                          ".
                   15  filler             pic  x(40) value
                            "TD02 - acconto/anticipo su fattura      ".
                   15  filler             pic  x(40) value
                            "TD03 - acconto/anticipo su parcella     ".
                   15  filler             pic  x(40) value
                            "TD04 - nota di credito                  ".
                   15  filler             pic  x(40) value
                            "TD05 - nota di debito                   ".
                   15  filler             pic  x(40) value
                            "TD06 - parcella                         ".
                   15  filler             pic  x(40) value
                            "TD16 - integrazione fattura rev. ch. int".
                   15  filler             pic  x(40) value
                            "TD17 - integrazione/autofatt. acq. s.es.".
                   15  filler             pic  x(40) value
                            "TD18 - acquisto beni intracomunitari    ".
                   15  filler             pic  x(40) value
                            "TD19 - integrazione acquisto beni ex 17 ".
                   15  filler             pic  x(40) value
                            "TD20 - autofattura per regolariz. ex 6  ".
                   15  filler             pic  x(40) value
                            "TD21 - autofattura per splafonamento    ".
                   15  filler             pic  x(40) value
                            "TD22 - estrazione beni da Deposito IVA  ".
                   15  filler             pic  x(40) value
                            "TD23 - estrazione beni da Dep.IVA con v.".
                   15  filler             pic  x(40) value
                            "TD24 - fattura differita di cui art. 21.".
                   15  filler             pic  x(40) value
                            "TD25 - fattura differita art. 21,co4,3p.".
                   15  filler             pic  x(40) value
                            "TD26 - cessione di beni ammortizz.      ".
                   15  filler             pic  x(40) value
                            "TD27 - fattura per autoconsumo o per c. ".
      *        *-------------------------------------------------------*
      *        * Work per : Origine del documento                      *
      *        *-------------------------------------------------------*
           05  w-exp-org-doc.
               10  w-exp-org-doc-num      pic  9(02)       value 7    .
               10  w-exp-org-doc-lun      pic  9(02)       value 40   .
               10  w-exp-org-doc-tbl.
                   15  filler             pic  x(40) value
                       "Manuale, fattura                        "     .
                   15  filler             pic  x(40) value
                       "Manuale, fattura + bolla di consegna    "     .
                   15  filler             pic  x(40) value
                       "Manuale, fattura + ricevuta fiscale     "     .
                   15  filler             pic  x(40) value
                       "Automatica, a fronte bolla di uscita    "     .
                   15  filler             pic  x(40) value
                       "Automatica, a fronte bolla di entrata   "     .
                   15  filler             pic  x(40) value
                       "Automatica, a fronte ordine             "     .
                   15  filler             pic  x(40) value
                       "Automatica, a fronte ordine spedizione  "     .
      *        *-------------------------------------------------------*
      *        * Work per : Tipi riga da proporre                      *
      *        *-------------------------------------------------------*
           05  w-exp-def-tpr.
               10  w-exp-def-tpr-num      pic  9(02)       value 7    .
               10  w-exp-def-tpr-lun      pic  9(02)       value 20   .
               10  w-exp-def-tpr-tbl.
                   15  filler             pic  x(20) value
                            "(Nessuna proposta)  "                    .
                   15  filler             pic  x(20) value
                            "Prodotto di vendita "                    .
                   15  filler             pic  x(20) value
                            "Prodotto similare   "                    .
                   15  filler             pic  x(20) value
                            "Materia prima       "                    .
                   15  filler             pic  x(20) value
                            "Semilavorato        "                    .
                   15  filler             pic  x(20) value
                            "Commento            "                    .
                   15  filler             pic  x(20) value
                            "Addebito            "                    .
      *        *-------------------------------------------------------*
      *        * Work per : Si/No riferimenti all'Agente               *
      *        *-------------------------------------------------------*
           05  w-exp-snx-age.
               10  w-exp-snx-age-num      pic  9(02)       value 4    .
               10  w-exp-snx-age-lun      pic  9(02)       value 40   .
               10  w-exp-snx-age-tbl.
                   15  filler             pic  x(40) value
                            "Si, codice e nominativo                 ".
                   15  filler             pic  x(40) value
                            "No, nessun riferimento                  ".
                   15  filler             pic  x(40) value
                            "Si, ma solo il codice                   ".
                   15  filler             pic  x(40) value
                            "Si, ma solo il nominativo               ".
      *        *-------------------------------------------------------*
      *        * Work per : Si/No riferimenti alla conferma ordine     *
      *        *-------------------------------------------------------*
           05  w-exp-snx-rco.
               10  w-exp-snx-rco-num      pic  9(02)       value 2    .
               10  w-exp-snx-rco-lun      pic  9(02)       value 40   .
               10  w-exp-snx-rco-tbl.
                   15  filler             pic  x(40) value
                            "Si, stampa dei riferimenti alla conferma".
                   15  filler             pic  x(40) value
                            "No, nessun riferimento                  ".
      *        *-------------------------------------------------------*
      *        * Work per : Si/No riferimenti al documento trasporto   *
      *        *-------------------------------------------------------*
           05  w-exp-snx-rdt.
               10  w-exp-snx-rdt-num      pic  9(02)       value 2    .
               10  w-exp-snx-rdt-lun      pic  9(02)       value 40   .
               10  w-exp-snx-rdt-tbl.
                   15  filler             pic  x(40) value
                            "Si, stampa dei riferimenti al d.d.t.    ".
                   15  filler             pic  x(40) value
                            "No, nessun riferimento                  ".

      *    *===========================================================*
      *    * Work per subroutines di Ctl                               *
      *    *-----------------------------------------------------------*
       01  w-ctl.
      *        *-------------------------------------------------------*
      *        * Work per Ctl su tipo fattura elettronica              *
      *        *-------------------------------------------------------*
           05  w-ctl-tdo-fel.
               10  w-ctl-tdo-fel-flg      pic  x(01)                  .
               10  w-ctl-tdo-fel-tdo      pic  x(05)                  .
               10  w-ctl-tdo-fel-tdo-r redefines
                   w-ctl-tdo-fel-tdo.
                   15  w-ctl-tdo-fel-chr occurs 5
                                          pic  x(01)                  .
               10  w-ctl-tdo-fel-inx      pic  9(02)                  .

      *    *===========================================================*
      *    * Work per subroutines di Find                              *
      *    *-----------------------------------------------------------*
       01  w-fnd.
      *        *-------------------------------------------------------*
      *        * Work per Find su archivio [ada]                       *
      *        *-------------------------------------------------------*
           05  w-fnd-arc-ada.
               10  w-fnd-arc-ada-sel      pic  x(01)                  .
               10  w-fnd-arc-ada-cod      pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Work per Find su tipi documento fattura elettronica   *
      *        *-------------------------------------------------------*
           05  w-fnd-tdo-fel.
               10  w-fnd-tdo-fel-sel      pic  x(01)                  .
               10  w-fnd-tdo-fel-tdo      pic  x(05)                  .
      *        *-------------------------------------------------------*
      *        * Work per Find su codici modulo di [pss]               *
      *        *-------------------------------------------------------*
           05  w-fnd-pss-mod.
               10  w-fnd-pss-mod-sel      pic  x(01)                  .
               10  w-fnd-pss-mod-cod      pic  x(08)                  .

      *    *===========================================================*
      *    * Work per subroutines di Let                               *
      *    *-----------------------------------------------------------*
       01  w-let.
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [ada]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-ada.
               10  w-let-arc-ada-flg      pic  x(01)                  .
               10  w-let-arc-ada-cod      pic  9(02)                  .
               10  w-let-arc-ada-den      pic  x(20)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [zcc]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-zcc.
               10  w-let-arc-zcc-flg      pic  x(01)                  .
               10  w-let-arc-zcc-cod      pic  9(03)                  .
               10  w-let-arc-zcc-des      pic  x(30)                  .
               10  w-let-arc-zcc-tmi      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [pdc]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-pdc.
               10  w-let-arc-pdc-flg      pic  x(01)                  .
               10  w-let-arc-pdc-cod      pic  9(07)                  .
               10  w-let-arc-pdc-des      pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [zac]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-zac.
               10  w-let-arc-zac-flg      pic  x(01)                  .
               10  w-let-arc-zac-tip      pic  9(02)                  .
               10  w-let-arc-zac-cod      pic  9(03)                  .
               10  w-let-arc-zac-des.
                   15  w-let-arc-zac-drg occurs 10
                                          pic  x(40)                  .
               10  w-let-arc-zac-civ      pic  9(05)                  .
               10  w-let-arc-zac-ccp      pic  9(07)                  .
               10  w-let-arc-zac-tot      pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [cli]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-cli.
               10  w-let-arc-cli-flg      pic  x(01)                  .
               10  w-let-arc-cli-cod      pic  9(07)                  .
               10  w-let-arc-cli-rag      pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su sub-archivio [mod]                    *
      *        *-------------------------------------------------------*
           05  w-let-arc-mod.
               10  w-let-arc-mod-flg      pic  x(01)                  .
               10  w-let-arc-mod-cod      pic  x(08)                  .
               10  w-let-arc-mod-des      pic  x(40)                  .

      *    *===========================================================*
      *    * Link-area per accettazione codice tipo movimento per la   *
      *    * fatturazione                                              *
      *    *-----------------------------------------------------------*
           copy      "pgm/fat/prg/cpy/acdezfi0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice causale contabile       *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/acmnzcc0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice sottoconto              *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/acmnpdc0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice addebito o commento     *
      *    *-----------------------------------------------------------*
           copy      "pgm/fat/prg/cpy/acmnzac0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice cliente contabile       *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/acmncli0.acl"                   .

      *    *===========================================================*
      *    * Work per subroutines di editing codice sottoconto         *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/wedtpdc0.wkl"                   .

      *    *===========================================================*
      *    * Work per records di [pss] 'mod'                           *
      *    *-----------------------------------------------------------*
           copy      "swd/xpg/prg/cpy/wpssmod0.cpw"                   .

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
      *              *  - se Impostazione corpo     : non abilitato    *
      *              *  - se Inserimento            : non abilitato    *
      *              *  - se Visualizzazione        : abilitato        *
      *              *  - se Almeno una modifica    : non abilitato    *
      *              *  - altrimenti                : abilitato        *
      *              *-------------------------------------------------*
           if        w-cnt-mfu-tip-imp    =    "K"   or
                     w-cnt-mfu-tip-imp    =    "C"   or
                     w-cnt-mfu-tip-fun    =    "I"   or
                     w-cnt-acc-flg-aum    not  = spaces
                     go to exe-acc-cmp-080.
           move      "[4] "               to   v-pfk (17)             .
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
           if        w-cnt-pos-snx-del    not  = spaces
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
      *              * Lettura personalizzazione numero livelli del    *
      *              * piano dei conti                                 *
      *              *-------------------------------------------------*
           perform   prs-liv-pdc-000      thru prs-liv-pdc-999        .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione tipo movimento         *
      *              *-------------------------------------------------*
           perform   cod-des-zfi-opn-000  thru cod-des-zfi-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice causale         *
      *              *-------------------------------------------------*
           perform   cod-mne-zcc-opn-000  thru cod-mne-zcc-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice sottoconto      *
      *              *-------------------------------------------------*
           perform   cod-mne-pdc-opn-000  thru cod-mne-pdc-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione addebito o commento    *
      *              *-------------------------------------------------*
           perform   cod-mne-zac-opn-000  thru cod-mne-zac-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice cliente         *
      *              *-------------------------------------------------*
           perform   cod-mne-cli-opn-000  thru cod-mne-cli-opn-999    .
       pre-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Lettura delle personalizzazioni relative al numero di li- *
      *    * velli del piano dei conti                                 *
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
      *              *-------------------------------------------------*
      *              * Normalizzazione personalizzazione               *
      *              *-------------------------------------------------*
           if        w-prs-liv-pdc        not  = 2
                     move  3              to   w-prs-liv-pdc          .
       prs-liv-pdc-999.
           exit.

      *    *===========================================================*
      *    * Routine post-esecuzione programma                         *
      *    *-----------------------------------------------------------*
       pos-exe-pgm-000.
      *              *-------------------------------------------------*
      *              * Close modulo accettazione tipo movimento        *
      *              *-------------------------------------------------*
           perform   cod-des-zfi-cls-000  thru cod-des-zfi-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice causale        *
      *              *-------------------------------------------------*
           perform   cod-mne-zcc-cls-000  thru cod-mne-zcc-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice sottoconto     *
      *              *-------------------------------------------------*
           perform   cod-mne-pdc-cls-000  thru cod-mne-pdc-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione addebito o commento   *
      *              *-------------------------------------------------*
           perform   cod-mne-zac-cls-000  thru cod-mne-zac-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice cliente        *
      *              *-------------------------------------------------*
           perform   cod-mne-cli-cls-000  thru cod-mne-cli-cls-999    .
       pos-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Open files                                                *
      *    *-----------------------------------------------------------*
       rou-opn-fls-000.
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
      *              *-------------------------------------------------*
      *              * [zac]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/fat/fls/ioc/obj/iofzac"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zac                 .
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
      *              * [zcc]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofzcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zcc                 .
      *              *-------------------------------------------------*
      *              * [cli]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofcli"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cli                 .
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
      *              * Funzione Open  per modulo              "mppssf" *
      *              *-------------------------------------------------*
           move      "OP"                 to   j-ope                  .
           call      "swd/mod/prg/obj/mppssf"
                                        using  r
                                               j                      .
       rou-opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Close files                                               *
      *    *-----------------------------------------------------------*
       rou-cls-fls-000.
      *              *-------------------------------------------------*
      *              * [zfi]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/fat/fls/ioc/obj/iofzfi"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
      *              *-------------------------------------------------*
      *              * [zac]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/fat/fls/ioc/obj/iofzac"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zac                 .
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
      *              * [zcc]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofzcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zcc                 .
      *              *-------------------------------------------------*
      *              * [cli]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofcli"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cli                 .
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
      *              * Funzione Close per modulo              "mppssf" *
      *              *-------------------------------------------------*
           move      "CL"                 to   j-ope                  .
           call      "swd/mod/prg/obj/mppssf"
                                        using  r
                                               j                      .
      *              *-------------------------------------------------*
      *              * Test di cancellabilita' per modulo     "mppssf" *
      *              *-------------------------------------------------*
           move      "X?"                 to   j-ope                  .
           call      "swd/mod/prg/obj/mppssf"
                                        using  r
                                               j                      .
           if        j-rsc                not  = spaces
                     go to rou-cls-fls-999.
      *              *-------------------------------------------------*
      *              * Cancellazione modulo                   "mppssf" *
      *              *-------------------------------------------------*
           cancel    "swd/mod/prg/obj/mppssf"                         .
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
      *                  * Codice tipo movimento                       *
      *                  *---------------------------------------------*
           perform   acc-cod-tmo-000      thru acc-cod-tmo-999        .
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
      *              * Codice tipo movimento                           *
      *              *-------------------------------------------------*
           perform   vis-cod-tmo-000      thru vis-cod-tmo-999        .
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
      *              * Codice tipo movimento                           *
      *              *-------------------------------------------------*
           perform   pmt-cod-tmo-000      thru pmt-cod-tmo-999        .
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
      *    * Visualizzazione prompts per Codice tipo movimento         *
      *    *-----------------------------------------------------------*
       pmt-cod-tmo-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice tipo movimento      :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-cod-tmo-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo chiave : Codice tipo movimento         *
      *    *-----------------------------------------------------------*
       acc-cod-tmo-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cod-tmo-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-des-zfi-ope      .
           move      w-tes-cod-tmo        to   w-cod-des-zfi-cod      .
           move      04                   to   w-cod-des-zfi-lin      .
           move      30                   to   w-cod-des-zfi-pos      .
           move      07                   to   w-cod-des-zfi-dln      .
           move      30                   to   w-cod-des-zfi-dps      .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-des-zfi-cll-000  thru cod-des-zfi-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-des-zfi-foi-000  thru cod-des-zfi-foi-999    .
       acc-cod-tmo-110.
           perform   cod-des-zfi-cll-000  thru cod-des-zfi-cll-999    .
           if        w-cod-des-zfi-ope    =    "F+"
                     go to acc-cod-tmo-115.
           if        w-cod-des-zfi-ope    =    "AC"
                     go to acc-cod-tmo-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-tmo-115.
           perform   cod-des-zfi-foi-000  thru cod-des-zfi-foi-999    .
           go to     acc-cod-tmo-110.
       acc-cod-tmo-120.
           move      w-cod-des-zfi-cod    to   v-alf                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-cod-tmo-999.
       acc-cod-tmo-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-cod-tmo          .
       acc-cod-tmo-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test che non ci siano blanks embedded       *
      *                  *---------------------------------------------*
           move      w-tes-cod-tmo        to   w-all-str-alf          .
           move      05                   to   w-all-str-lun          .
           perform   all-str-ble-000      thru all-str-ble-999        .
           if        w-all-str-flg        not  = spaces
                     go to acc-cod-tmo-100.
       acc-cod-tmo-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cod-tmo-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                not  = "DO  "
                     go to acc-cod-tmo-999.
           perform   cnt-key-vuo-000      thru cnt-key-vuo-999        .
           if        w-cnt-key-vuo-flg    not  = spaces
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-cod-tmo-999.
           perform   cnt-tdo-key-000      thru cnt-tdo-key-999        .
           if        w-cnt-tdo-key-flg    not  = spaces
                     go to acc-cod-tmo-100
           else      move  "S"            to   w-cnt-tus-acc-key
                     go to acc-cod-tmo-999.
       acc-cod-tmo-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo chiave : Codice tipo movimento      *
      *    *-----------------------------------------------------------*
       vis-cod-tmo-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-cod-tmo        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-tmo-999.
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
      *              * La testata e' composta di nr. 2 pagine          *
      *              *-------------------------------------------------*
           move      3                    to   w-cnt-sts-imp-mpt      .
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
                     depending            on   w-cnt-sts-imp-npt      .
           go to     snp-tes-reg-999.
       snp-tes-reg-100.
      *              *-------------------------------------------------*
      *              * Test per pagina 1                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     snp-tes-reg-999.
       snp-tes-reg-200.
      *              *-------------------------------------------------*
      *              * Test per pagina 2                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     snp-tes-reg-999.
       snp-tes-reg-300.
      *              *-------------------------------------------------*
      *              * Test per pagina 3                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Solo se autofattura                         *
      *                  *---------------------------------------------*
           if        w-tes-tip-doc (1)    not  = 05
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
                     depending            on   w-cnt-sts-imp-npt      .
           go to     acc-tes-reg-999.
       acc-tes-reg-100.
      *                  *---------------------------------------------*
      *                  * Normalizzazione func-key di impostazione    *
      *                  *---------------------------------------------*
           move      spaces               to   v-key                  .
      *                  *---------------------------------------------*
      *                  * Descrizione tipo movimento                  *
      *                  *---------------------------------------------*
           perform   acc-des-tmo-000      thru acc-des-tmo-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
       acc-tes-reg-110.
      *                  *---------------------------------------------*
      *                  * Descrizione per la stampa                   *
      *                  *---------------------------------------------*
           perform   acc-des-stp-000      thru acc-des-stp-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-100.
       acc-tes-reg-115.
      *                  *---------------------------------------------*
      *                  * Password per il movimento                   *
      *                  *---------------------------------------------*
           perform   acc-pwd-tmo-000      thru acc-pwd-tmo-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-110.
       acc-tes-reg-120.
      *                  *---------------------------------------------*
      *                  * Validita' per le dipendenze                 *
      *                  *---------------------------------------------*
           perform   acc-vld-dpz-000      thru acc-vld-dpz-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-115.
       acc-tes-reg-125.
      *                  *---------------------------------------------*
      *                  * Codice dipendenza                           *
      *                  *---------------------------------------------*
           perform   acc-cod-dpz-000      thru acc-cod-dpz-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-120.
       acc-tes-reg-130.
      *                  *---------------------------------------------*
      *                  * Tipo documento                              *
      *                  *---------------------------------------------*
           perform   acc-tip-doc-000      thru acc-tip-doc-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-125.
       acc-tes-reg-133.
      *                  *---------------------------------------------*
      *                  * Tipo fattura elettronica                    *
      *                  *---------------------------------------------*
           perform   acc-tdo-fel-000      thru acc-tdo-fel-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-130.
       acc-tes-reg-135.
      *                  *---------------------------------------------*
      *                  * Origine del documento                       *
      *                  *---------------------------------------------*
           perform   acc-org-doc-000      thru acc-org-doc-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-133.
       acc-tes-reg-140.
      *                  *---------------------------------------------*
      *                  * Provenienza del documento                   *
      *                  *---------------------------------------------*
           perform   acc-prv-doc-000      thru acc-prv-doc-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-135.
       acc-tes-reg-145.
      *                  *---------------------------------------------*
      *                  * Sigla numerazione                           *
      *                  *---------------------------------------------*
           perform   acc-sgl-num-000      thru acc-sgl-num-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-140.
       acc-tes-reg-150.
      *                  *---------------------------------------------*
      *                  * Numero giornale iva                         *
      *                  *---------------------------------------------*
           perform   acc-num-giv-000      thru acc-num-giv-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-145.
       acc-tes-reg-155.
      *                  *---------------------------------------------*
      *                  * Codice causale contabile                    *
      *                  *---------------------------------------------*
           perform   acc-cau-cge-000      thru acc-cau-cge-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-150.
       acc-tes-reg-160.
      *                  *---------------------------------------------*
      *                  * Sottoconto iva vendite                      *
      *                  *---------------------------------------------*
           perform   acc-ctp-ivv-000      thru acc-ctp-ivv-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-155.
       acc-tes-reg-165.
      *                  *---------------------------------------------*
      *                  * Sottoconto vendite                          *
      *                  *---------------------------------------------*
           perform   acc-ctp-ven-000      thru acc-ctp-ven-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-160.
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
                     go to acc-tes-reg-165.
      *                  *---------------------------------------------*
      *                  * Fine Pagina                                 *
      *                  *---------------------------------------------*
           move      "+"                  to   w-cnt-tus-acc-tes      .
           go to     acc-tes-reg-999.
       acc-tes-reg-200.
      *                  *---------------------------------------------*
      *                  * Default per accettazione tipo riga corpo    *
      *                  *---------------------------------------------*
           perform   acc-def-tpr-000      thru acc-def-tpr-999        .
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
      *                  * Si/no stampa Agente                         *
      *                  *---------------------------------------------*
           perform   acc-snx-age-000      thru acc-snx-age-999        .
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
      *                  * Si/no stampa riferimento conferma ordine    *
      *                  *---------------------------------------------*
           perform   acc-snx-rco-000      thru acc-snx-rco-999        .
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
      *                  * Si/no stampa riferimento documento trasp.   *
      *                  *---------------------------------------------*
           perform   acc-snx-rdt-000      thru acc-snx-rdt-999        .
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
      *                  * Codice dicitura                             *
      *                  *---------------------------------------------*
           perform   acc-cod-dct-000      thru acc-cod-dct-999        .
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
      *                  * Codice modulo                               *
      *                  *---------------------------------------------*
           perform   acc-cod-mod-000      thru acc-cod-mod-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-240.
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
                     go to acc-tes-reg-240.
      *                  *---------------------------------------------*
      *                  * Fine Pagina                                 *
      *                  *---------------------------------------------*
           move      "+"                  to   w-cnt-tus-acc-tes      .
           go to     acc-tes-reg-999.
       acc-tes-reg-300.
      *                  *---------------------------------------------*
      *                  * Codice Cliente per autofattura              *
      *                  *---------------------------------------------*
           perform   acc-cli-aft-000      thru acc-cli-aft-999        .
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
                     go to acc-tes-reg-240.
      *                  *---------------------------------------------*
      *                  * Fine Pagina                                 *
      *                  *---------------------------------------------*
           move      "+"                  to   w-cnt-tus-acc-tes      .
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
                     depending            on   w-cnt-sts-imp-npt      .
           go to     vis-tes-reg-999.
       vis-tes-reg-100.
      *              *-------------------------------------------------*
      *              * Descrizione tipo movimento                      *
      *              *-------------------------------------------------*
           perform   vis-des-tmo-000      thru vis-des-tmo-999        .
      *              *-------------------------------------------------*
      *              * Descrizione per la stampa                       *
      *              *-------------------------------------------------*
           perform   vis-des-stp-000      thru vis-des-stp-999        .
      *              *-------------------------------------------------*
      *              * Password per il movimento                       *
      *              *-------------------------------------------------*
           perform   vis-pwd-tmo-000      thru vis-pwd-tmo-999        .
      *              *-------------------------------------------------*
      *              * Validita' per le dipendenze                     *
      *              *-------------------------------------------------*
           perform   vis-vld-dpz-000      thru vis-vld-dpz-999        .
      *              *-------------------------------------------------*
      *              * Codice dipendenza                               *
      *              *-------------------------------------------------*
           perform   vis-cod-dpz-000      thru vis-cod-dpz-999        .
           perform   vis-den-dpz-000      thru vis-den-dpz-999        .
      *              *-------------------------------------------------*
      *              * Tipo documento                                  *
      *              *-------------------------------------------------*
           perform   vis-tip-doc-000      thru vis-tip-doc-999        .
      *              *-------------------------------------------------*
      *              * Tipo fattura elettronica                        *
      *              *-------------------------------------------------*
           perform   vis-tdo-fel-000      thru vis-tdo-fel-999        .
      *              *-------------------------------------------------*
      *              * Origine del documento                           *
      *              *-------------------------------------------------*
           perform   vis-org-doc-000      thru vis-org-doc-999        .
      *              *-------------------------------------------------*
      *              * Provenienza del documento                       *
      *              *-------------------------------------------------*
           perform   vis-prv-doc-000      thru vis-prv-doc-999        .
      *              *-------------------------------------------------*
      *              * Sigla numerazione                               *
      *              *-------------------------------------------------*
           perform   vis-sgl-num-000      thru vis-sgl-num-999        .
      *              *-------------------------------------------------*
      *              * Numero giornale iva                             *
      *              *-------------------------------------------------*
           perform   vis-num-giv-000      thru vis-num-giv-999        .
      *              *-------------------------------------------------*
      *              * Codice causale contabile                        *
      *              *-------------------------------------------------*
           perform   vis-cau-cge-000      thru vis-cau-cge-999        .
           perform   vis-des-cau-000      thru vis-des-cau-999        .
      *              *-------------------------------------------------*
      *              * Sottoconto iva vendite                          *
      *              *-------------------------------------------------*
           perform   vis-ctp-ivv-000      thru vis-ctp-ivv-999        .
           perform   vis-des-civ-000      thru vis-des-civ-999        .
      *              *-------------------------------------------------*
      *              * Sottoconto vendite                              *
      *              *-------------------------------------------------*
           perform   vis-ctp-ven-000      thru vis-ctp-ven-999        .
           perform   vis-des-ctv-000      thru vis-des-ctv-999        .
           go to     vis-tes-reg-999.
       vis-tes-reg-200.
      *              *-------------------------------------------------*
      *              * Default per accettazione tipo riga corpo        *
      *              *-------------------------------------------------*
           perform   vis-def-tpr-000      thru vis-def-tpr-999        .
      *              *-------------------------------------------------*
      *              * Si/no stampa Agente                             *
      *              *-------------------------------------------------*
           perform   vis-snx-age-000      thru vis-snx-age-999        .
      *              *-------------------------------------------------*
      *              * Si/no stampa riferimento conferma ordine        *
      *              *-------------------------------------------------*
           perform   vis-snx-rco-000      thru vis-snx-rco-999        .
      *              *-------------------------------------------------*
      *              * Si/no stampa riferimento documento di trasporto *
      *              *-------------------------------------------------*
           perform   vis-snx-rdt-000      thru vis-snx-rdt-999        .
      *              *-------------------------------------------------*
      *              * Codice dicitura                                 *
      *              *-------------------------------------------------*
           perform   vis-cod-dct-000      thru vis-cod-dct-999        .
           perform   vis-txt-dct-000      thru vis-txt-dct-999        .
      *              *-------------------------------------------------*
      *              * Codice modulo                                   *
      *              *-------------------------------------------------*
           perform   vis-cod-mod-000      thru vis-cod-mod-999        .
           perform   vis-cod-mod-des-000  thru vis-cod-mod-des-999    .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     vis-tes-reg-999.
       vis-tes-reg-300.
      *              *-------------------------------------------------*
      *              * Codice Cliente per autofattura                  *
      *              *-------------------------------------------------*
           perform   vis-cli-aft-000      thru vis-cli-aft-999        .
           perform   vis-cli-aft-rag-000  thru vis-cli-aft-rag-999    .
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
                     depending            on   w-cnt-sts-imp-npt      .
           go to     pmt-tes-reg-999.
       pmt-tes-reg-100.
      *              *-------------------------------------------------*
      *              * Descrizione tipo movimento                      *
      *              *-------------------------------------------------*
           perform   pmt-des-tmo-000      thru pmt-des-tmo-999        .
      *              *-------------------------------------------------*
      *              * Descrizione per la stampa                       *
      *              *-------------------------------------------------*
           perform   pmt-des-stp-000      thru pmt-des-stp-999        .
      *              *-------------------------------------------------*
      *              * Password per il movimento                       *
      *              *-------------------------------------------------*
           perform   pmt-pwd-tmo-000      thru pmt-pwd-tmo-999        .
      *              *-------------------------------------------------*
      *              * Validita' per le dipendenze                     *
      *              *-------------------------------------------------*
           perform   pmt-vld-dpz-000      thru pmt-vld-dpz-999        .
      *              *-------------------------------------------------*
      *              * Codice dipendenza                               *
      *              *-------------------------------------------------*
           perform   pmt-cod-dpz-000      thru pmt-cod-dpz-999        .
      *              *-------------------------------------------------*
      *              * Tipo documento                                  *
      *              *-------------------------------------------------*
           perform   pmt-tip-doc-000      thru pmt-tip-doc-999        .
      *              *-------------------------------------------------*
      *              * Tipo fattura elettronica                        *
      *              *-------------------------------------------------*
           perform   pmt-tdo-fel-000      thru pmt-tdo-fel-999        .
      *              *-------------------------------------------------*
      *              * Origine del documento                           *
      *              *-------------------------------------------------*
           perform   pmt-org-doc-000      thru pmt-org-doc-999        .
      *              *-------------------------------------------------*
      *              * Sigla numerazione                               *
      *              *-------------------------------------------------*
           perform   pmt-sgl-num-000      thru pmt-sgl-num-999        .
      *              *-------------------------------------------------*
      *              * Numero giornale iva                             *
      *              *-------------------------------------------------*
           perform   pmt-num-giv-000      thru pmt-num-giv-999        .
      *              *-------------------------------------------------*
      *              * Codice causale contabile                        *
      *              *-------------------------------------------------*
           perform   pmt-cau-cge-000      thru pmt-cau-cge-999        .
      *              *-------------------------------------------------*
      *              * Sottoconto iva vendite                          *
      *              *-------------------------------------------------*
           perform   pmt-ctp-ivv-000      thru pmt-ctp-ivv-999        .
      *              *-------------------------------------------------*
      *              * Sottoconto vendite                              *
      *              *-------------------------------------------------*
           perform   pmt-ctp-ven-000      thru pmt-ctp-ven-999        .
           go to     pmt-tes-reg-999.
       pmt-tes-reg-200.
      *              *-------------------------------------------------*
      *              * Default per accettazione tipo riga corpo        *
      *              *-------------------------------------------------*
           perform   pmt-def-tpr-000      thru pmt-def-tpr-999        .
      *              *-------------------------------------------------*
      *              * Si/no stampa Agente                             *
      *              *-------------------------------------------------*
           perform   pmt-snx-age-000      thru pmt-snx-age-999        .
      *              *-------------------------------------------------*
      *              * Si/no stampa riferimento conferma ordine        *
      *              *-------------------------------------------------*
           perform   pmt-snx-rco-000      thru pmt-snx-rco-999        .
      *              *-------------------------------------------------*
      *              * Si/no stampa riferimento documento di trasporto *
      *              *-------------------------------------------------*
           perform   pmt-snx-rdt-000      thru pmt-snx-rdt-999        .
      *              *-------------------------------------------------*
      *              * Codice dicitura                                 *
      *              *-------------------------------------------------*
           perform   pmt-cod-dct-000      thru pmt-cod-dct-999        .
      *              *-------------------------------------------------*
      *              * Codice modulo                                   *
      *              *-------------------------------------------------*
           perform   pmt-cod-mod-000      thru pmt-cod-mod-999        .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     pmt-tes-reg-999.
       pmt-tes-reg-300.
      *              *-------------------------------------------------*
      *              * Codice Cliente per autofattura                  *
      *              *-------------------------------------------------*
           perform   pmt-cli-aft-000      thru pmt-cli-aft-999        .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     pmt-tes-reg-999.
       pmt-tes-reg-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Descrizione tipo movimento       *
      *    *-----------------------------------------------------------*
       pmt-des-tmo-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Descrizione tipo movimento :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-des-tmo-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Descrizione per la stampa        *
      *    *-----------------------------------------------------------*
       pmt-des-stp-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Descrizione per la stampa  :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-des-stp-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Password per il movimento        *
      *    *-----------------------------------------------------------*
       pmt-pwd-tmo-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "                            "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-pwd-tmo-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Validita' per le dipendenze      *
      *    *-----------------------------------------------------------*
       pmt-vld-dpz-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Validita' per le dipendenze:"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-vld-dpz-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Codice dipendenza                *
      *    *-----------------------------------------------------------*
       pmt-cod-dpz-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice dipendenza          :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cod-dpz-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Tipo documento                   *
      *    *-----------------------------------------------------------*
       pmt-tip-doc-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tipo documento associato   :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-tip-doc-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Tipo fattura elettronica         *
      *    *-----------------------------------------------------------*
       pmt-tdo-fel-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      16                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      60                   to   v-pos                  .
           move      "Tipo per 'sdi' :"   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-tdo-fel-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Origine del documento            *
      *    *-----------------------------------------------------------*
       pmt-org-doc-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Origine del documento      :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-org-doc-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Sigla numerazione                *
      *    *-----------------------------------------------------------*
       pmt-sgl-num-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Sigla numerazione          :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-sgl-num-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Numero giornale iva              *
      *    *-----------------------------------------------------------*
       pmt-num-giv-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Nr. giornale iva           :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-num-giv-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Codice causale contabile         *
      *    *-----------------------------------------------------------*
       pmt-cau-cge-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Causale contabile          :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cau-cge-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Sottoconto iva vendite           *
      *    *-----------------------------------------------------------*
       pmt-ctp-ivv-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Sottoconto iva vendite     :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-ctp-ivv-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Sottoconto vendite               *
      *    *-----------------------------------------------------------*
       pmt-ctp-ven-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      21                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Contropartita vendite      :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-ctp-ven-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Default per accettazione tipo    *
      *    *                          riga corpo                       *
      *    *-----------------------------------------------------------*
       pmt-def-tpr-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tipo di riga proposto      :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-def-tpr-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Si/no stampa Agente              *
      *    *-----------------------------------------------------------*
       pmt-snx-age-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Stampa agente              :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-snx-age-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Si/no riferimenti conferma       *
      *    *-----------------------------------------------------------*
       pmt-snx-rco-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Stampa riferimenti ordine  :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-snx-rco-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Si/no riferimenti d.d.t.         *
      *    *-----------------------------------------------------------*
       pmt-snx-rdt-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Stampa riferimenti al      :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      " documento di trasporto     "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-snx-rdt-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Codice dicitura                  *
      *    *-----------------------------------------------------------*
       pmt-cod-dct-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Dicitura a fine righe      :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "    in stampa fattura       "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "            differita       "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cod-dct-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Codice modulo                    *
      *    *-----------------------------------------------------------*
       pmt-cod-mod-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice modulo proposto     :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cod-mod-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Codice Cliente Autofattura       *
      *    *-----------------------------------------------------------*
       pmt-cli-aft-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice Cliente Autofattura :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cli-aft-999.
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
      *    * Accettazione campo testata : Descrizione tipo movimento   *
      *    *-----------------------------------------------------------*
       acc-des-tmo-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-des-tmo-100.
      *              *-------------------------------------------------*
      *              * Note operative                                  *
      *              *-------------------------------------------------*
           move      "NT"                 to   v-ope                  .
           move      "[F4] per copiare il tipo movimento"
                                          to   v-not                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-tes    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      w-tes-des-tmo (1)    to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Cancellazione note operative attuali            *
      *              *-------------------------------------------------*
           move      "NT"                 to   v-ope                  .
           move      spaces               to   v-not                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-des-tmo-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-des-tmo-999.
       acc-des-tmo-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-des-tmo (1)      .
       acc-des-tmo-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore a spaces : reimpostazione         *
      *                  *---------------------------------------------*
           if        w-tes-des-tmo (1)    =    spaces
                     go to acc-des-tmo-100.
       acc-des-tmo-450.
      *                  *---------------------------------------------*
      *                  * Se valore a non spaces il primo carattere   *
      *                  * non deve essere a spaces                    *
      *                  *---------------------------------------------*
           if        w-tes-des-tmo (1)
                    (01 : 01)             =    spaces
                     go to acc-des-tmo-100.
       acc-des-tmo-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione descrizione in uppercase       *
      *                  *---------------------------------------------*
           move      w-tes-des-tmo (1)    to   w-all-str-alf          .
           move      30                   to   w-all-str-lun          .
           perform   all-str-upp-000      thru all-str-upp-999        .
           move      w-all-str-alf        to   w-tes-des-key (1)      .
       acc-des-tmo-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-des-tmo-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-des-tmo-100.
       acc-des-tmo-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Descrizione tipo movimento*
      *    *-----------------------------------------------------------*
       vis-des-tmo-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-des-tmo (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-des-tmo-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Descrizione per la stampa    *
      *    *-----------------------------------------------------------*
       acc-des-stp-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione valore default                 *
      *                  *---------------------------------------------*
           if        w-tes-des-stp (1)    not  = spaces
                     go to acc-des-stp-100.
           move      w-tes-des-tmo (1)    to   w-tes-des-stp (1)      .
       acc-des-stp-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      25                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-tes    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      w-tes-des-stp (1)    to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-des-stp-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-des-stp-999.
       acc-des-stp-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-des-stp (1)      .
       acc-des-stp-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-des-stp-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-des-stp-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-des-stp-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-des-stp-100.
       acc-des-stp-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Descrizione tipo movimento*
      *    *-----------------------------------------------------------*
       vis-des-stp-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      25                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-des-stp (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-des-stp-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Password per tipo movimento  *
      *    *-----------------------------------------------------------*
       acc-pwd-tmo-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Campo momentaneamente non accettato         *
      *                  *---------------------------------------------*
           go to     acc-pwd-tmo-999.
       acc-pwd-tmo-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Descrizione tipo movimento*
      *    *-----------------------------------------------------------*
       vis-pwd-tmo-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-pwd-tmo (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-pwd-tmo-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Validita' per le dipendenze  *
      *    *-----------------------------------------------------------*
       acc-vld-dpz-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      w-tes-vld-dpz (1)    to   w-sav-vld-dpz          .
       acc-vld-dpz-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-vld-dpz-lun    to   v-car                  .
           move      w-exp-vld-dpz-num    to   v-ldt                  .
           move      "TU#"                to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      10                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-vld-dpz-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-tes    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      w-tes-vld-dpz (1)    to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-vld-dpz-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-vld-dpz-999.
       acc-vld-dpz-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-vld-dpz (1)      .
       acc-vld-dpz-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se zero : reimpostazione                    *
      *                  *---------------------------------------------*
           if        w-tes-vld-dpz (1)    not  = zero
                     go to acc-vld-dpz-600.
           if        v-key                =    "UP  "
                     go to acc-vld-dpz-600
           else      go to acc-vld-dpz-100.
       acc-vld-dpz-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore attuale uguale a precedente :     *
      *                  * oltre                                       *
      *                  *---------------------------------------------*
           if        w-tes-vld-dpz (1)    =    w-sav-vld-dpz
                     go to acc-vld-dpz-800.
      *                      *-----------------------------------------*
      *                      * Se validita' per una sola dipendenza :  *
      *                      * oltre                                   *
      *                      *-----------------------------------------*
           if        w-tes-vld-dpz (1)    =    02
                     go to acc-vld-dpz-800.
      *                          *-------------------------------------*
      *                          * Normalizzazione codice dipendenza   *
      *                          *-------------------------------------*
           move      zero                 to   w-tes-cod-dpz (1)      .
      *                          *-------------------------------------*
      *                          * Visualizzazione codice dipendenza   *
      *                          *-------------------------------------*
           perform   vis-cod-dpz-000      thru vis-cod-dpz-999        .
      *                          *-------------------------------------*
      *                          * Normalizzazione descrizione dipend. *
      *                          *-------------------------------------*
           move      spaces               to   w-tes-cod-dpz-den (1)  .
      *                          *-------------------------------------*
      *                          * Visualizzazione descrizione dipend. *
      *                          *-------------------------------------*
           perform   vis-den-dpz-000      thru vis-den-dpz-999        .
       acc-vld-dpz-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-vld-dpz-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-vld-dpz-100.
       acc-vld-dpz-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Validita' per le dipend.  *
      *    *-----------------------------------------------------------*
       vis-vld-dpz-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-vld-dpz-lun    to   v-car                  .
           move      w-exp-vld-dpz-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      10                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-vld-dpz-tbl    to   v-txt                  .
           move      w-tes-vld-dpz (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-vld-dpz-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Codice dipendenza            *
      *    *-----------------------------------------------------------*
       acc-cod-dpz-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-tes-vld-dpz (1)    =    01
                     go to acc-cod-dpz-999.
       acc-cod-dpz-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      11                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           if        w-cnt-sts-imp-tes    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      w-tes-cod-dpz (1)    to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-cod-dpz-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-cod-dpz-999.
       acc-cod-dpz-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-cod-dpz (1)      .
      *              *-------------------------------------------------*
      *              * Se Find                                         *
      *              *-------------------------------------------------*
           if        v-key                not  = "FIND"
                     go to acc-cod-dpz-400.
      *                  *---------------------------------------------*
      *                  * Find su archivio [ada]                      *
      *                  *---------------------------------------------*
           perform   fnd-arc-ada-000      thru fnd-arc-ada-999        .
           if        w-fnd-arc-ada-sel    not  = spaces
                     go to acc-cod-dpz-100.
           move      w-fnd-arc-ada-cod    to   w-tes-cod-dpz (1)      .
      *                  *---------------------------------------------*
      *                  * Visualizza codice selezionato               *
      *                  *---------------------------------------------*
           perform   vis-cod-dpz-000      thru vis-cod-dpz-999        .
           move      spaces               to   v-key                  .
           go to     acc-cod-dpz-400.
       acc-cod-dpz-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura archivio [ada]                      *
      *                  *---------------------------------------------*
           move      w-tes-cod-dpz (1)    to   w-let-arc-ada-cod      .
           perform   let-arc-ada-000      thru let-arc-ada-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione descrizione                  *
      *                  *---------------------------------------------*
           move      w-let-arc-ada-den    to   w-tes-cod-dpz-den (1)  .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione                 *
      *                  *---------------------------------------------*
           perform   vis-den-dpz-000      thru vis-den-dpz-999        .
      *                  *---------------------------------------------*
      *                  * Se lettura errata : reimpostazione          *
      *                  *---------------------------------------------*
           if        w-let-arc-ada-flg    not  = spaces
                     go to acc-cod-dpz-100.
      *                  *---------------------------------------------*
      *                  * Se valore non ammesso : reimpostazione      *
      *                  *---------------------------------------------*
           if        w-tes-cod-dpz (1)    =    zero
                     go to acc-cod-dpz-100.
       acc-cod-dpz-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cod-dpz-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-cod-dpz-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-cod-dpz-100.
       acc-cod-dpz-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Codice dipendenza         *
      *    *-----------------------------------------------------------*
       vis-cod-dpz-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      11                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-cod-dpz (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-dpz-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Denominazione dipendenza  *
      *    *-----------------------------------------------------------*
       vis-den-dpz-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      35                   to   v-pos                  .
           move      w-tes-cod-dpz-den (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-den-dpz-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Tipo documento               *
      *    *-----------------------------------------------------------*
       acc-tip-doc-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-tip-doc-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-doc-lun    to   v-car                  .
           move      w-exp-tip-doc-num    to   v-ldt                  .
           move      "FDCPAS#"            to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      13                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-tip-doc-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-tes    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      w-tes-tip-doc (1)    to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-tip-doc-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-tip-doc-999.
       acc-tip-doc-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-tip-doc (1)      .
       acc-tip-doc-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se zero : reimpostazione                    *
      *                  *---------------------------------------------*
           if        w-tes-tip-doc (1)    not  = zero
                     go to acc-tip-doc-600.
           if        v-key                =    "UP  "
                     go to acc-tip-doc-600
           else      go to acc-tip-doc-100.
       acc-tip-doc-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se tipo documento : fattura Pro-Forma  *
      *                  *---------------------------------------------*
           if        w-tes-tip-doc (1)    not  = 04
                     go to acc-tip-doc-800.
      *                  *---------------------------------------------*
      *                  * Normalizzazione causale contabile           *
      *                  *---------------------------------------------*
           move      zero                 to   w-tes-cau-cge (1)      .
           perform   vis-cau-cge-000      thru vis-cau-cge-999        .
      *                  *---------------------------------------------*
      *                  * Normalizzazione numero giornale iva         *
      *                  *---------------------------------------------*
           move      zero                 to   w-tes-num-giv (1)      .
           perform   vis-num-giv-000      thru vis-num-giv-999        .
      *                  *---------------------------------------------*
      *                  * Normalizzazione sigla numerazione           *
      *                  *---------------------------------------------*
           if        w-tes-sgl-num (1)    not  = spaces
                     go to acc-tip-doc-800.
           move      "FPF"                to   w-tes-sgl-num (1)      .
           perform   vis-sgl-num-000      thru vis-sgl-num-999        .
       acc-tip-doc-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-tip-doc-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-tip-doc-100.
       acc-tip-doc-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Tipo documento            *
      *    *-----------------------------------------------------------*
       vis-tip-doc-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-doc-lun    to   v-car                  .
           move      w-exp-tip-doc-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      13                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-tip-doc-tbl    to   v-txt                  .
           move      w-tes-tip-doc (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tip-doc-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Tipo fattura elettronica             *
      *    *-----------------------------------------------------------*
       acc-tdo-fel-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-tes-tip-doc (1)    =    04
                     go to acc-tdo-fel-999.
      *                  *---------------------------------------------*
      *                  * Preparazione eventuale default              *
      *                  *---------------------------------------------*
           if        w-tes-tdo-fel (1)    not  = spaces
                     go to acc-tdo-fel-100.
           if        w-tes-tip-doc (1)    =    01
                     move  "TD01"         to   w-tes-tdo-fel (1)
           else if   w-tes-tip-doc (1)    =    02
                     move  "TD05"         to   w-tes-tdo-fel (1)
           else if   w-tes-tip-doc (1)    =    03
                     move  "TD04"         to   w-tes-tdo-fel (1)
           else if   w-tes-tip-doc (1)    =    05
                     move  "TD16"         to   w-tes-tdo-fel (1)
           else if   w-tes-tip-doc (1)    =    06
                     move  "TD02"         to   w-tes-tdo-fel (1)      .
       acc-tdo-fel-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      77                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           if        w-cnt-sts-imp-tes    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      w-tes-tdo-fel (1)    to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-tdo-fel-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-tdo-fel-999.
       acc-tdo-fel-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-tdo-fel (1)      .
       acc-tdo-fel-300.
      *              *-------------------------------------------------*
      *              * Se Find                                         *
      *              *-------------------------------------------------*
           if        v-key                not  = "FIND"
                     go to acc-tdo-fel-400.
      *                  *---------------------------------------------*
      *                  * Find su tipi fattura elettronica            *
      *                  *---------------------------------------------*
           perform   fnd-tdo-fel-000      thru fnd-tdo-fel-999        .
           if        w-fnd-tdo-fel-sel    not  = spaces
                     go to acc-tdo-fel-100.
           move      w-fnd-tdo-fel-tdo    to   w-tes-tdo-fel (1)      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione tipo fattura elettronica    *
      *                  *---------------------------------------------*
           perform   vis-tdo-fel-000      thru vis-tdo-fel-999        .
           move      spaces               to   v-key                  .
       acc-tdo-fel-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Controlli su tipo documento impostato       *
      *                  *---------------------------------------------*
           move      w-tes-tdo-fel (1)    to   w-ctl-tdo-fel-tdo      .
           perform   ctl-tdo-fel-000      thru ctl-tdo-fel-999        .
           if        w-ctl-tdo-fel-flg    = spaces
                     go to acc-tdo-fel-600.
      *                  *---------------------------------------------*
      *                  * Messaggio di errore                         *
      *                  *---------------------------------------------*
           move      "Tipo documento per 'sdi' non accettabile !       "
                                          to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                  *---------------------------------------------*
      *                  * A reimpostazione                            *
      *                  *---------------------------------------------*
           go to     acc-tdo-fel-100.
       acc-tdo-fel-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-tdo-fel-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-tdo-fel-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-tdo-fel-100.
       acc-tdo-fel-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Tipo fattura elettronica          *
      *    *-----------------------------------------------------------*
       vis-tdo-fel-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      77                   to   v-pos                  .
           move      w-tes-tdo-fel (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tdo-fel-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Origine del documento        *
      *    *-----------------------------------------------------------*
       acc-org-doc-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-org-doc-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-org-doc-lun    to   v-car                  .
           move      w-exp-org-doc-num    to   v-ldt                  .
           move      spaces               to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      14                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-org-doc-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-tes    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
      *
           if        w-tes-org-doc (1)    =    01
                     move  01             to   v-num
           else if   w-tes-org-doc (1)    =    02
                     move  02             to   v-num
           else if   w-tes-org-doc (1)    =    03
                     move  03             to   v-num
           else if   w-tes-org-doc (1)    =    11
                     move  04             to   v-num
           else if   w-tes-org-doc (1)    =    31
                     move  05             to   v-num
           else if   w-tes-org-doc (1)    =    41
                     move  06             to   v-num
           else if   w-tes-org-doc (1)    =    51
                     move  07             to   v-num
           else      move  zero           to   v-num                  .
      *
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-org-doc-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-org-doc-999.
       acc-org-doc-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        v-num                =    01
                     move  01             to   w-tes-org-doc (1)
           else if   v-num                =    02
                     move  02             to   w-tes-org-doc (1)
           else if   v-num                =    03
                     move  03             to   w-tes-org-doc (1)
           else if   v-num                =    04
                     move  11             to   w-tes-org-doc (1)
           else if   v-num                =    05
                     move  31             to   w-tes-org-doc (1)
           else if   v-num                =    06
                     move  41             to   w-tes-org-doc (1)
           else if   v-num                =    07
                     move  51             to   w-tes-org-doc (1)
           else      move  zero           to   w-tes-org-doc (1)      .
       acc-org-doc-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se zero : reimpostazione                    *
      *                  *---------------------------------------------*
           if        w-tes-org-doc (1)    not  = zero
                     go to acc-org-doc-500.
           if        v-key                =    "UP  "
                     go to acc-org-doc-600
           else      go to acc-org-doc-100.
       acc-org-doc-500.
      *                  *---------------------------------------------*
      *                  * Se tipo documento Fattura Pro-Forma : si    *
      *                  * accetta solo origine del documento manuale  *
      *                  * o automatica a fronte ordine /ordine di     *
      *                  * spedizione                                  *
      *                  *---------------------------------------------*
           if        w-tes-tip-doc (1)    not  = 04
                     go to acc-org-doc-600.
           if        w-tes-org-doc (1)    not  = 01 and
                     w-tes-org-doc (1)    not  = 41 and
                     w-tes-org-doc (1)    not  = 51
                     go to acc-org-doc-100.
       acc-org-doc-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-org-doc-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-org-doc-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-org-doc-100.
       acc-org-doc-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Origine del documento     *
      *    *-----------------------------------------------------------*
       vis-org-doc-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-org-doc-lun    to   v-car                  .
           move      w-exp-org-doc-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      14                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-org-doc-tbl    to   v-txt                  .
      *
           if        w-tes-org-doc (1)    =    01
                     move  01             to   v-num
           else if   w-tes-org-doc (1)    =    02
                     move  02             to   v-num
           else if   w-tes-org-doc (1)    =    03
                     move  03             to   v-num
           else if   w-tes-org-doc (1)    =    11
                     move  04             to   v-num
           else if   w-tes-org-doc (1)    =    31
                     move  05             to   v-num
           else if   w-tes-org-doc (1)    =    41
                     move  06             to   v-num
           else if   w-tes-org-doc (1)    =    51
                     move  07             to   v-num
           else      move  zero           to   v-num                  .
      *
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-org-doc-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Provenienza del documento    *
      *    *-----------------------------------------------------------*
       acc-prv-doc-000.
      *              *-------------------------------------------------*
      *              * Il campo non viene momentaneamente gestito : il *
      *              * suo valore viene attualmente determinato in     *
      *              * cnt-tdo-nok-000/999                             *
      *              *-------------------------------------------------*
       acc-prv-doc-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Provenienza del documento *
      *    *-----------------------------------------------------------*
       vis-prv-doc-000.
      *              *-------------------------------------------------*
      *              * Il campo non viene momentaneamente gestito : il *
      *              * suo valore viene attualmente determinato in     *
      *              * cnt-tdo-nok-000/999                             *
      *              *-------------------------------------------------*
       vis-prv-doc-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Sigla numerazione            *
      *    *-----------------------------------------------------------*
       acc-sgl-num-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-sgl-num-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-tes    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      w-tes-sgl-num (1)    to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-sgl-num-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-sgl-num-999.
       acc-sgl-num-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-sgl-num (1)      .
       acc-sgl-num-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test che non ci siano blanks embedded       *
      *                  *---------------------------------------------*
           move      w-tes-sgl-num (1)    to   w-all-str-alf          .
           move      03                   to   w-all-str-lun          .
           perform   all-str-ble-000      thru all-str-ble-999        .
           if        w-all-str-flg        not  = spaces
                     go to acc-sgl-num-100.
       acc-sgl-num-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-sgl-num-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-sgl-num-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-sgl-num-100.
       acc-sgl-num-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Sigla numerazione         *
      *    *-----------------------------------------------------------*
       vis-sgl-num-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-sgl-num (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-sgl-num-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Numero giornale iva          *
      *    *-----------------------------------------------------------*
       acc-num-giv-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-tes-tip-doc (1)    =    04
                     go to acc-num-giv-999.
       acc-num-giv-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      17                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-tes    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      w-tes-num-giv (1)    to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-num-giv-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-num-giv-999.
       acc-num-giv-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-num-giv (1)      .
       acc-num-giv-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-num-giv-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-num-giv-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-num-giv-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-num-giv-100.
       acc-num-giv-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Numero giornale iva       *
      *    *-----------------------------------------------------------*
       vis-num-giv-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      17                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-num-giv (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-num-giv-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata: codice causale contabile      *
      *    *-----------------------------------------------------------*
       acc-cau-cge-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-tes-tip-doc (1)    =    04
                     go to acc-cau-cge-999.
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      w-tes-cau-cge (1)    to   w-sav-cau-cge          .
       acc-cau-cge-100.
      *              *-------------------------------------------------*
      *              * Visualizzazione note operative                  *
      *              *-------------------------------------------------*
           move      "NT"                 to   v-ope                  .
           move      "Se la causale viene posta a zero il documento non 
      -              "aggiorna la contabilita' e  "
                                          to   v-nt1                  .
           move      "nemmeno il portafoglio clienti                    
      -              "                          "
                                          to   v-nt2                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-zcc-ope      .
           move      w-tes-cau-cge (1)    to   w-cod-mne-zcc-cod      .
           move      19                   to   w-cod-mne-zcc-lin      .
           move      30                   to   w-cod-mne-zcc-pos      .
           move      19                   to   w-cod-mne-zcc-dln      .
           move      35                   to   w-cod-mne-zcc-dps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "[1] "               to   v-pfk (15)             .
           if        w-cnt-sts-imp-tes    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           perform   cod-mne-zcc-cll-000  thru cod-mne-zcc-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-mne-zcc-foi-000  thru cod-mne-zcc-foi-999    .
       acc-cau-cge-110.
           perform   cod-mne-zcc-cll-000  thru cod-mne-zcc-cll-999    .
           if        w-cod-mne-zcc-ope    =    "F+"
                     go to acc-cau-cge-115.
           if        w-cod-mne-zcc-ope    =    "AC"
                     go to acc-cau-cge-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cau-cge-115.
           perform   cod-mne-zcc-foi-000  thru cod-mne-zcc-foi-999    .
           go to     acc-cau-cge-110.
       acc-cau-cge-120.
           move      w-cod-mne-zcc-cod    to   v-num                  .
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
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-cau-cge-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-cau-cge-999.
       acc-cau-cge-200.
      *              *-------------------------------------------------*
      *              * Valore impostato                                *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-cau-cge (1)      .
       acc-cau-cge-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura tabella causali                     *
      *                  *---------------------------------------------*
           move      w-tes-cau-cge (1)    to   w-let-arc-zcc-cod      .
           perform   let-arc-zcc-000      thru let-arc-zcc-999        .
           move      w-let-arc-zcc-des    to   w-tes-cau-cge-des (1)  .
           move      w-let-arc-zcc-tmi    to   w-tes-cau-cge-tmi (1)  .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione causale         *
      *                  *---------------------------------------------*
           perform   vis-des-cau-000      thru vis-des-cau-999        .
      *                  *---------------------------------------------*
      *                  * Se lettura errata : reimpostazione          *
      *                  *---------------------------------------------*
           if        w-let-arc-zcc-flg    not  = spaces
                     go to acc-cau-cge-100.
      *                  *---------------------------------------------*
      *                  * Causale a zero : ammessa                    *
      *                  *---------------------------------------------*
           if        w-tes-cau-cge (1)    not  = zero
                     go to acc-cau-cge-450.
      *                      *-----------------------------------------*
      *                      * Nessun ulteriore controllo ed oltre     *
      *                      *-----------------------------------------*
           go to     acc-cau-cge-600.
       acc-cau-cge-450.
      *                  *---------------------------------------------*
      *                  * Test di compatibilita' tra tipo movimento   *
      *                  * iva associato alla causale e tipo documento *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se tipo documento : fattura o nota di   *
      *                      * addebito                                *
      *                      *-----------------------------------------*
           if        w-tes-tip-doc (1)    not  = 01 and
                     w-tes-tip-doc (1)    not  = 02
                     go to acc-cau-cge-500.
           if        w-tes-cau-cge-tmi (1)
                                          =    "1" or
                     w-tes-cau-cge-tmi (1)
                                          =    "A"
                     go to acc-cau-cge-600.
           if        w-tes-cau-cge (1)    =    w-sav-cau-cge
                     go to acc-cau-cge-600.
           if       (w-tes-cau-cge-tmi (1)
                                          =    "0"    or
                     w-tes-cau-cge-tmi (1)
                                          =    spaces  ) and
                     v-key                =    "[1] "
                     go to acc-cau-cge-600.
           go to     acc-cau-cge-590.
       acc-cau-cge-500.
      *                      *-----------------------------------------*
      *                      * Se tipo documento : nota di accredito   *
      *                      *-----------------------------------------*
           if        w-tes-tip-doc (1)    not  = 03
                     go to acc-cau-cge-600.
           if        w-tes-cau-cge-tmi (1)
                                          =    "2" or
                     w-tes-cau-cge-tmi (1)
                                          =    "B"
                     go to acc-cau-cge-600.
           if        w-tes-cau-cge (1)    =    w-sav-cau-cge
                     go to acc-cau-cge-600.
           if       (w-tes-cau-cge-tmi (1)
                                          =    "0"    or
                     w-tes-cau-cge-tmi (1)
                                          =    spaces  ) and
                     v-key                =    "[1] "
                     go to acc-cau-cge-600.
           go to     acc-cau-cge-590.
       acc-cau-cge-590.
      *                      *-----------------------------------------*
      *                      * Messaggio di errore                     *
      *                      *-----------------------------------------*
           move      "Causale scorretta sotto il profilo iva !        "
                                          to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                      *-----------------------------------------*
      *                      * A reimpostazione                        *
      *                      *-----------------------------------------*
           go to     acc-cau-cge-100.
       acc-cau-cge-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se valore variato                      *
      *                  *---------------------------------------------*
           if        w-tes-cau-cge (1)    =    w-sav-cau-cge
                     go to acc-cau-cge-800.
      *                  *---------------------------------------------*
      *                  * Se causale a zero                           *
      *                  *---------------------------------------------*
           if        w-tes-cau-cge (1)    not  = zero
                     go to acc-cau-cge-800.
      *                      *-----------------------------------------*
      *                      * Normalizzazione sottoconti              *
      *                      *-----------------------------------------*
           move      zero                 to   w-tes-ctp-ivv (1)      .
           move      spaces               to   w-tes-ctp-ivv-des (1)  .
           move      zero                 to   w-tes-ctp-ven (1)      .
           move      spaces               to   w-tes-ctp-ven-des (1)  .
      *                      *-----------------------------------------*
      *                      * Visualizzazione sottoconti              *
      *                      *-----------------------------------------*
           perform   vis-ctp-ivv-000      thru vis-ctp-ivv-999        .
           perform   vis-des-civ-000      thru vis-des-civ-999        .
           perform   vis-ctp-ven-000      thru vis-ctp-ven-999        .
           perform   vis-des-ctv-000      thru vis-des-ctv-999        .
       acc-cau-cge-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-cau-cge-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-cau-cge-100.
       acc-cau-cge-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Codice causale contabile          *
      *    *-----------------------------------------------------------*
       vis-cau-cge-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      19                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "<B"                 to   v-edm                  .
           move      w-tes-cau-cge (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       vis-cau-cge-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Descrizione causale contabile     *
      *    *-----------------------------------------------------------*
       vis-des-cau-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      35                   to   v-pos                  .
           move      w-tes-cau-cge-des (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       vis-des-cau-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Sottoconto iva vendite       *
      *    *-----------------------------------------------------------*
       acc-ctp-ivv-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-tes-cau-cge (1)    =    zero
                     go to acc-ctp-ivv-999.
       acc-ctp-ivv-100.
      *              *-------------------------------------------------*
      *              * Visualizzazione note operative                  *
      *              *-------------------------------------------------*
           move      "NT"                 to   v-ope                  .
           move      "Digitare un codice sottoconto solo nel caso in cui
      -              " si voglia che il sottoconto"
                                          to   v-nt1                  .
           move      "iva movimentato sia diverso da quello di default d
      -              "ella contabilita' generale"
                                          to   v-nt2                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-pdc-ope      .
           move      w-prs-liv-pdc        to   w-cod-mne-pdc-liv      .
           move      w-tes-ctp-ivv (1)    to   w-cod-mne-pdc-cod      .
           move      20                   to   w-cod-mne-pdc-lin      .
           move      30                   to   w-cod-mne-pdc-pos      .
           move      20                   to   w-cod-mne-pdc-dln      .
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
       acc-ctp-ivv-110.
           perform   cod-mne-pdc-cll-000  thru cod-mne-pdc-cll-999    .
           if        w-cod-mne-pdc-ope    =    "F+"
                     go to acc-ctp-ivv-115.
           if        w-cod-mne-pdc-ope    =    "AC"
                     go to acc-ctp-ivv-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-ctp-ivv-115.
           perform   cod-mne-pdc-foi-000  thru cod-mne-pdc-foi-999    .
           go to     acc-ctp-ivv-110.
       acc-ctp-ivv-120.
           move      w-cod-mne-pdc-cod    to   v-num                  .
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
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-ctp-ivv-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-ctp-ivv-999.
       acc-ctp-ivv-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-ctp-ivv (1)      .
       acc-ctp-ivv-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura archivio piano dei conti            *
      *                  *---------------------------------------------*
           move      w-tes-ctp-ivv (1)    to   w-let-arc-pdc-cod      .
           perform   let-arc-pdc-000      thru let-arc-pdc-999        .
           move      w-let-arc-pdc-des    to   w-tes-ctp-ivv-des (1)  .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione sottoconto      *
      *                  *---------------------------------------------*
           perform   vis-des-civ-000      thru vis-des-civ-999        .
      *                  *---------------------------------------------*
      *                  * Se codice sottoconto non esistente : reim-  *
      *                  * postazione                                  *
      *                  *---------------------------------------------*
           if        w-let-arc-pdc-flg    not  = spaces
                     go to acc-ctp-ivv-100.
       acc-ctp-ivv-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-ctp-ivv-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-ctp-ivv-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-ctp-ivv-100.
       acc-ctp-ivv-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Sottoconto iva vendite    *
      *    *-----------------------------------------------------------*
       vis-ctp-ivv-000.
      *              *-------------------------------------------------*
      *              * Editing con appoggio a sinistra                 *
      *              *-------------------------------------------------*
           move      w-prs-liv-pdc        to   w-edt-cod-pdc-liv      .
           move      w-tes-ctp-ivv (1)    to   w-edt-cod-pdc-cod      .
           move      "B"                  to   w-edt-cod-pdc-edm      .
           perform   edt-pdc-asx-000      thru edt-pdc-asx-999        .
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      09                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-edt-cod-pdc-edt    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-ctp-ivv-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Descrizione sottoconto    *
      *    * iva vendite                                               *
      *    *-----------------------------------------------------------*
       vis-des-civ-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      w-tes-ctp-ivv-des (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-des-civ-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Sottoconto vendite           *
      *    *-----------------------------------------------------------*
       acc-ctp-ven-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-tes-cau-cge (1)    =    zero
                     go to acc-ctp-ven-999.
       acc-ctp-ven-100.
      *              *-------------------------------------------------*
      *              * Visualizzazione note operative                  *
      *              *-------------------------------------------------*
           move      "NT"                 to   v-ope                  .
           move      "Digitare un codice sottoconto solo nel caso in cui
      -              " si voglia che la contropar-"
                                          to   v-nt1                  .
           move      "tita contabile sia forzata a questo valore"
                                          to   v-nt2                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-pdc-ope      .
           move      w-prs-liv-pdc        to   w-cod-mne-pdc-liv      .
           move      w-tes-ctp-ven (1)    to   w-cod-mne-pdc-cod      .
           move      21                   to   w-cod-mne-pdc-lin      .
           move      30                   to   w-cod-mne-pdc-pos      .
           move      21                   to   w-cod-mne-pdc-dln      .
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
      *              * Cancellazione note operative                    *
      *              *-------------------------------------------------*
           move      "NT"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
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
           perform   vis-des-ctv-000      thru vis-des-ctv-999        .
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
      *    * Visualizzazione campo testata : Sottoconto vendite        *
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
           move      21                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-edt-cod-pdc-edt    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-ctp-ven-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Descrizione sottoconto    *
      *    * vendite                                                   *
      *    *-----------------------------------------------------------*
       vis-des-ctv-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      21                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      w-tes-ctp-ven-des (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-des-ctv-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Default per accettazione tipo riga   *
      *    *-----------------------------------------------------------*
       acc-def-tpr-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-def-tpr-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-def-tpr-lun    to   v-car                  .
           move      w-exp-def-tpr-num    to   v-ldt                  .
           move      spaces               to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      07                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-def-tpr-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-tes    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
      *
           if        w-tes-def-tpr (1)    =    spaces
                     move  01             to   v-num
           else if   w-tes-def-tpr (1)    =    "P    "
                     move  02             to   v-num
           else if   w-tes-def-tpr (1)    =    "PN   "
                     move  03             to   v-num
           else if   w-tes-def-tpr (1)    =    "M    "
                     move  04             to   v-num
           else if   w-tes-def-tpr (1)    =    "S    "
                     move  05             to   v-num
           else if   w-tes-def-tpr (1)    =    "C    "
                     move  06             to   v-num
           else if   w-tes-def-tpr (1)    =    "A    "
                     move  07             to   v-num
           else      move  zero           to   v-num                  .
      *
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-def-tpr-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-def-tpr-999.
       acc-def-tpr-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        v-num                =    01
                     move  spaces         to   w-tes-def-tpr (1)
           else if   v-num                =    02
                     move  "P    "        to   w-tes-def-tpr (1)
           else if   v-num                =    03
                     move  "PN   "        to   w-tes-def-tpr (1)
           else if   v-num                =    04
                     move  "M    "        to   w-tes-def-tpr (1)
           else if   v-num                =    05
                     move  "S    "        to   w-tes-def-tpr (1)
           else if   v-num                =    06
                     move  "C    "        to   w-tes-def-tpr (1)
           else if   v-num                =    07
                     move  "A    "        to   w-tes-def-tpr (1)
           else      move  spaces         to   w-tes-def-tpr (1)      .
       acc-def-tpr-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-def-tpr-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-def-tpr-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-def-tpr-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-def-tpr-100.
       acc-def-tpr-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Default accettazione tipo riga    *
      *    *-----------------------------------------------------------*
       vis-def-tpr-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-def-tpr-lun    to   v-car                  .
           move      w-exp-def-tpr-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      07                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-def-tpr-tbl    to   v-txt                  .
      *
           if        w-tes-def-tpr (1)    =    spaces
                     move  01             to   v-num
           else if   w-tes-def-tpr (1)    =    "P    "
                     move  02             to   v-num
           else if   w-tes-def-tpr (1)    =    "PN   "
                     move  03             to   v-num
           else if   w-tes-def-tpr (1)    =    "M    "
                     move  04             to   v-num
           else if   w-tes-def-tpr (1)    =    "S    "
                     move  05             to   v-num
           else if   w-tes-def-tpr (1)    =    "C    "
                     move  06             to   v-num
           else if   w-tes-def-tpr (1)    =    "A    "
                     move  07             to   v-num
           else      move  zero           to   v-num                  .
      *
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-def-tpr-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Si/No stampa Agente          *
      *    *-----------------------------------------------------------*
       acc-snx-age-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione eventuale default              *
      *                  *---------------------------------------------*
           if        w-tes-snx-age (1)    not  = spaces
                     go to acc-snx-age-100.
           move      "N"                  to   w-tes-snx-age (1)      .
       acc-snx-age-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-snx-age-lun    to   v-car                  .
           move      w-exp-snx-age-num    to   v-ldt                  .
           move      spaces               to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      09                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-snx-age-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-tes    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
      *
           if        w-tes-snx-age (1)    =    "S"
                     move  01             to   v-num
           else if   w-tes-snx-age (1)    =    "N"
                     move  02             to   v-num
           else if   w-tes-snx-age (1)    =    "C"
                     move  03             to   v-num
           else if   w-tes-snx-age (1)    =    "T"
                     move  04             to   v-num
           else      move  zero           to   v-num                  .
      *
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-snx-age-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-snx-age-999.
       acc-snx-age-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        v-num                =    01
                     move  "S"            to   w-tes-snx-age (1)
           else if   v-num                =    02
                     move  "N"            to   w-tes-snx-age (1)
           else if   v-num                =    03
                     move  "C"            to   w-tes-snx-age (1)
           else if   v-num                =    04
                     move  "T"            to   w-tes-snx-age (1)
           else      move  spaces         to   w-tes-snx-age (1)      .
       acc-snx-age-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se spaces : reimpostazione, a meno che non  *
      *                  * si sia in Up                                *
      *                  *---------------------------------------------*
           if        w-tes-snx-age (1)    not  = spaces
                     go to acc-snx-age-600.
           if        v-key                =    "UP  "
                     go to acc-snx-age-600
           else      go to acc-snx-age-100.
       acc-snx-age-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-snx-age-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-snx-age-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-snx-age-100.
       acc-snx-age-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Si/No stampa Agente       *
      *    *-----------------------------------------------------------*
       vis-snx-age-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-snx-age-lun    to   v-car                  .
           move      w-exp-snx-age-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      09                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-snx-age-tbl    to   v-txt                  .
      *
           if        w-tes-snx-age (1)    =    "S"
                     move  01             to   v-num
           else if   w-tes-snx-age (1)    =    "N"
                     move  02             to   v-num
           else if   w-tes-snx-age (1)    =    "C"
                     move  03             to   v-num
           else if   w-tes-snx-age (1)    =    "T"
                     move  04             to   v-num
           else      move  zero           to   v-num                  .
      *
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-snx-age-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo  : Si/No riferimenti conferma ordine   *
      *    *-----------------------------------------------------------*
       acc-snx-rco-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione eventuale default              *
      *                  *---------------------------------------------*
           if        w-tes-snx-rco (1)    not  = spaces
                     go to acc-snx-rco-100.
           move      "S"                  to   w-tes-snx-rco (1)      .
       acc-snx-rco-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-snx-rco-lun    to   v-car                  .
           move      w-exp-snx-rco-num    to   v-ldt                  .
           move      spaces               to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      11                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-snx-rco-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-tes    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
      *
           if        w-tes-snx-rco (1)    =    spaces
                     move  01             to   v-num
           else if   w-tes-snx-rco (1)    =    "S"
                     move  01             to   v-num
           else if   w-tes-snx-rco (1)    =    "N"
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
      *
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-snx-rco-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-snx-rco-999.
       acc-snx-rco-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        v-num                =    01
                     move  "S"            to   w-tes-snx-rco (1)
           else if   v-num                =    02
                     move  "N"            to   w-tes-snx-rco (1)
           else      move  spaces         to   w-tes-snx-rco (1)      .
       acc-snx-rco-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se spaces : reimpostazione, a meno che non  *
      *                  * si sia in Up                                *
      *                  *---------------------------------------------*
           if        w-tes-snx-rco (1)    not  = spaces
                     go to acc-snx-rco-600.
           if        v-key                =    "UP  "
                     go to acc-snx-rco-600
           else      go to acc-snx-rco-100.
       acc-snx-rco-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-snx-rco-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-snx-rco-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-snx-rco-100.
       acc-snx-rco-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Si/No riferimenti conferma ordine *
      *    *-----------------------------------------------------------*
       vis-snx-rco-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-snx-rco-lun    to   v-car                  .
           move      w-exp-snx-rco-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      11                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-snx-rco-tbl    to   v-txt                  .
      *
           if        w-tes-snx-rco (1)    =    spaces
                     move  01             to   v-num
           else if   w-tes-snx-rco (1)    =    "S"
                     move  01             to   v-num
           else if   w-tes-snx-rco (1)    =    "N"
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
      *
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-snx-rco-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo  : Si/No riferimenti docum. trasporto  *
      *    *-----------------------------------------------------------*
       acc-snx-rdt-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione eventuale default              *
      *                  *---------------------------------------------*
           if        w-tes-snx-rdt (1)    not  = spaces
                     go to acc-snx-rdt-100.
           move      "S"                  to   w-tes-snx-rdt (1)      .
       acc-snx-rdt-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-snx-rdt-lun    to   v-car                  .
           move      w-exp-snx-rdt-num    to   v-ldt                  .
           move      spaces               to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      13                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-snx-rdt-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-tes    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
      *
           if        w-tes-snx-rdt (1)    =    spaces
                     move  01             to   v-num
           else if   w-tes-snx-rdt (1)    =    "S"
                     move  01             to   v-num
           else if   w-tes-snx-rdt (1)    =    "N"
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
      *
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-snx-rdt-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-snx-rdt-999.
       acc-snx-rdt-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        v-num                =    01
                     move  "S"            to   w-tes-snx-rdt (1)
           else if   v-num                =    02
                     move  "N"            to   w-tes-snx-rdt (1)
           else      move  spaces         to   w-tes-snx-rdt (1)      .
       acc-snx-rdt-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se spaces : reimpostazione, a meno che non  *
      *                  * si sia in Up                                *
      *                  *---------------------------------------------*
           if        w-tes-snx-rdt (1)    not  = spaces
                     go to acc-snx-rdt-600.
           if        v-key                =    "UP  "
                     go to acc-snx-rdt-600
           else      go to acc-snx-rdt-100.
       acc-snx-rdt-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-snx-rdt-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-snx-rdt-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-snx-rdt-100.
       acc-snx-rdt-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Si/No riferimenti docum. trasp.   *
      *    *-----------------------------------------------------------*
       vis-snx-rdt-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-snx-rdt-lun    to   v-car                  .
           move      w-exp-snx-rdt-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      13                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-snx-rdt-tbl    to   v-txt                  .
      *
           if        w-tes-snx-rdt (1)    =    spaces
                     move  01             to   v-num
           else if   w-tes-snx-rdt (1)    =    "S"
                     move  01             to   v-num
           else if   w-tes-snx-rdt (1)    =    "N"
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
      *
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-snx-rdt-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Codice dicitura                      *
      *    *-----------------------------------------------------------*
       acc-cod-dct-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-tes-org-doc (1)    =    02 or
                     w-tes-org-doc (1)    =    03
                     go to acc-cod-dct-999.
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      w-tes-cod-dct (1)    to   w-sav-cod-dct          .
       acc-cod-dct-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-zac-ope      .
           move      03                   to   w-cod-mne-zac-tip      .
           move      w-tes-cod-dct (1)    to   w-cod-mne-zac-cod      .
           move      16                   to   w-cod-mne-zac-lin      .
           move      30                   to   w-cod-mne-zac-pos      .
           move      16                   to   w-cod-mne-zac-dln      .
           move      40                   to   w-cod-mne-zac-dps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           if        w-cnt-sts-imp-tes    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           perform   cod-mne-zac-cll-000  thru cod-mne-zac-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-mne-zac-foi-000  thru cod-mne-zac-foi-999    .
       acc-cod-dct-110.
           perform   cod-mne-zac-cll-000  thru cod-mne-zac-cll-999    .
           if        w-cod-mne-zac-ope    =    "F+"
                     go to acc-cod-dct-115.
           if        w-cod-mne-zac-ope    =    "AC"
                     go to acc-cod-dct-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-dct-115.
           perform   cod-mne-zac-foi-000  thru cod-mne-zac-foi-999    .
           go to     acc-cod-dct-110.
       acc-cod-dct-120.
           move      w-cod-mne-zac-cod    to   v-num                  .
       acc-cod-dct-150.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-cod-dct-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-cod-dct-999.
       acc-cod-dct-200.
      *              *-------------------------------------------------*
      *              * Valore impostato                                *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-cod-dct (1)      .
       acc-cod-dct-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura archivio [zac]                      *
      *                  *---------------------------------------------*
           move      03                   to   w-let-arc-zac-tip      .
           move      w-tes-cod-dct (1)    to   w-let-arc-zac-cod      .
           perform   let-arc-zac-000      thru let-arc-zac-999        .
      *                  *---------------------------------------------*
      *                  * Se codice errato                            *
      *                  *---------------------------------------------*
           if        w-let-arc-zac-flg    =    spaces
                     go to acc-cod-dct-600.
      *                      *-----------------------------------------*
      *                      * Messaggio                               *
      *                      *-----------------------------------------*
           move      "Codice dicitura non esistente !                   
      -              "               "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                      *-----------------------------------------*
      *                      * A reimpostazione                        *
      *                      *-----------------------------------------*
           go to     acc-cod-dct-100.
       acc-cod-dct-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore attuale come precedente : oltre   *
      *                  *---------------------------------------------*
           if        w-tes-cod-dct (1)    =    w-sav-cod-dct
                     go to acc-cod-dct-800.
      *                  *---------------------------------------------*
      *                  * Descrizione dicitura                        *
      *                  *---------------------------------------------*
           move      w-let-arc-zac-des    to   w-tes-txt-dct (1)      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione                 *
      *                  *---------------------------------------------*
           perform   vis-txt-dct-000      thru vis-txt-dct-999        .
       acc-cod-dct-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-cod-dct-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-cod-dct-100.
       acc-cod-dct-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Codice dicitura                   *
      *    *-----------------------------------------------------------*
       vis-cod-dct-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      16                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-cod-dct (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-dct-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Testo dicitura                    *
      *    *-----------------------------------------------------------*
       vis-txt-dct-000.
      *              *-------------------------------------------------*
      *              * Riga 1                                          *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      40                   to   v-pos                  .
           move      w-tes-dct-rig (1, 1) to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Riga 2                                          *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      40                   to   v-pos                  .
           move      w-tes-dct-rig (1, 2) to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Riga 3                                          *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      40                   to   v-pos                  .
           move      w-tes-dct-rig (1, 3) to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Se righe oltre                                  *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      77                   to   v-pos                  .
      *
           if        w-tes-txt-dct (1)
                    (121 : 280)           =    spaces
                     move  spaces         to   v-alf
           else      move  "..."          to   v-alf                  .
      *
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-txt-dct-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Codice modulo                              *
      *    *-----------------------------------------------------------*
       acc-cod-mod-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cod-mod-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "L"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           if        w-cnt-sts-imp-tes    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      w-tes-cod-mod (1)    to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
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
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-cod-mod-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-cod-mod-999.
       acc-cod-mod-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-cod-mod (1)      .
      *              *-------------------------------------------------*
      *              * Se Find                                         *
      *              *-------------------------------------------------*
           if        v-key                not  = "FIND"
                     go to acc-cod-mod-400.
      *                  *---------------------------------------------*
      *                  * Find su codici modulo                       *
      *                  *---------------------------------------------*
           move      w-tes-cod-mod (1)    to   w-fnd-pss-mod-cod      .
           perform   fnd-pss-mod-000      thru fnd-pss-mod-999        .
      *                  *---------------------------------------------*
      *                  * Se nessuna selezione : reimpostazione       *
      *                  *---------------------------------------------*
           if        w-fnd-pss-mod-sel    not  = spaces
                     go to acc-cod-mod-100.
      *                  *---------------------------------------------*
      *                  * Memorizzazione codice selezionato           *
      *                  *---------------------------------------------*
           move      w-fnd-pss-mod-cod    to   w-tes-cod-mod (1)      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione codice selezionato          *
      *                  *---------------------------------------------*
           perform   vis-cod-mod-000      thru vis-cod-mod-999        .
      *                  *---------------------------------------------*
      *                  * Normalizzazione function key                *
      *                  *---------------------------------------------*
           move      spaces               to   v-key                  .
       acc-cod-mod-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura codice modulo per descrizione       *
      *                  *---------------------------------------------*
           if        w-tes-cod-mod (1)    =    spaces
                     go to acc-cod-mod-600.
           move      w-tes-cod-mod (1)    to   w-let-arc-mod-cod      .
           perform   let-arc-mod-000      thru let-arc-mod-999        .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione tipo stampante  *
      *                  *---------------------------------------------*
           move      w-let-arc-mod-des    to   w-tes-cod-mod-des (1)  .
           perform   vis-cod-mod-des-000  thru vis-cod-mod-des-999    .
      *                  *---------------------------------------------*
      *                  * Test che il tipo stampante esista           *
      *                  *---------------------------------------------*
           if        w-let-arc-mod-flg    not  = spaces
                     go to acc-cod-mod-100.
       acc-cod-mod-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cod-mod-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-cod-mod-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-cod-mod-100.
       acc-cod-mod-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Codice modulo                           *
      *    *-----------------------------------------------------------*
       vis-cod-mod-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-cod-mod (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-mod-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Codice modulo, descrizione              *
      *    *-----------------------------------------------------------*
       vis-cod-mod-des-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      40                   to   v-pos                  .
           move      w-tes-cod-mod-des (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-mod-des-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Codice cliente per autofattura       *
      *    *-----------------------------------------------------------*
       acc-cli-aft-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cli-aft-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-cli-ope      .
           move      w-tes-cli-aft (1)    to   w-cod-mne-cli-cod      .
           move      07                   to   w-cod-mne-cli-lin      .
           move      30                   to   w-cod-mne-cli-pos      .
           move      07                   to   w-cod-mne-cli-rln      .
           move      41                   to   w-cod-mne-cli-rps      .
           move      zero                 to   w-cod-mne-cli-vln      .
           move      zero                 to   w-cod-mne-cli-vps      .
           move      zero                 to   w-cod-mne-cli-lln      .
           move      zero                 to   w-cod-mne-cli-lps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           if        w-cnt-sts-imp-tes    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           perform   cod-mne-cli-cll-000  thru cod-mne-cli-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-mne-cli-foi-000  thru cod-mne-cli-foi-999    .
       acc-cli-aft-110.
           perform   cod-mne-cli-cll-000  thru cod-mne-cli-cll-999    .
           if        w-cod-mne-cli-ope    =    "F+"
                     go to acc-cli-aft-115.
           if        w-cod-mne-cli-ope    =    "AC"
                     go to acc-cli-aft-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cli-aft-115.
           perform   cod-mne-cli-foi-000  thru cod-mne-cli-foi-999    .
           go to     acc-cli-aft-110.
       acc-cli-aft-120.
           move      w-cod-mne-cli-cod    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-cli-aft-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-cli-aft-999.
       acc-cli-aft-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-cli-aft (1)      .
       acc-cli-aft-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura archivio [cli]                      *
      *                  *---------------------------------------------*
           move      w-tes-cli-aft (1)    to   w-let-arc-cli-cod      .
           perform   let-arc-cli-000      thru let-arc-cli-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione dati cliente                 *
      *                  *---------------------------------------------*
           move      w-let-arc-cli-rag    to   w-tes-cli-aft-rag (1)  .
      *                  *---------------------------------------------*
      *                  * Visualizzazione dati cliente                *
      *                  *---------------------------------------------*
           perform   vis-cli-aft-rag-000  thru vis-cli-aft-rag-999    .
      *                  *---------------------------------------------*
      *                  * Se codice errato : reimpostazione           *
      *                  *---------------------------------------------*
           if        w-let-arc-cli-flg    not  = spaces
                     go to acc-cli-aft-100.
       acc-cli-aft-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cli-aft-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-cli-aft-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-cli-aft-100.
       acc-cli-aft-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Codice cliente autofattura        *
      *    *-----------------------------------------------------------*
       vis-cli-aft-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      07                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-cli-aft (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cli-aft-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Ragione sociale cliente           *
      *    *-----------------------------------------------------------*
       vis-cli-aft-rag-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      w-tes-cli-aft-rag (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cli-aft-rag-999.
           exit.

      *    *===========================================================*
      *    * Editing del codice sottoconto con appoggio a sx o dx      *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/wedtpdc0.wks"                   .

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
      *              * Test su Codice tipo movimento                   *
      *              *-------------------------------------------------*
           if        w-tes-cod-tmo        =    spaces
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
       cnt-tdo-nok-100.
      *              *-------------------------------------------------*
      *              * Controlli preliminari comuni                    *
      *              *-------------------------------------------------*
       cnt-tdo-nok-110.
      *                  *---------------------------------------------*
      *                  * Descrizione                                 *
      *                  *---------------------------------------------*
           if        w-tes-des-tmo (1)    not  = spaces
                     go to cnt-tdo-nok-120.
           move      "Manca la descrizione !                          "
                                          to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-120.
      *                  *---------------------------------------------*
      *                  * Validita' per le dipendenze                 *
      *                  *---------------------------------------------*
           if        w-tes-vld-dpz (1)    not  = zero
                     go to cnt-tdo-nok-121.
           move      "Manca la validita' per le dipendenze !          "
                                          to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-121.
           if        w-tes-vld-dpz (1)    =    01 or
                     w-tes-vld-dpz (1)    =    02
                     go to cnt-tdo-nok-130.
           move      "Validita' per le dipendenze errata !            "
                                          to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-130.
      *                  *---------------------------------------------*
      *                  * Codice dipendenza                           *
      *                  *---------------------------------------------*
           if        w-tes-vld-dpz (1)    not  = 02
                     go to cnt-tdo-nok-140.
           if        w-tes-cod-dpz (1)    not  = zero
                     go to cnt-tdo-nok-140.
           move      "Manca il codice della dipendenza di validita' ! "
                                          to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-140.
      *                  *---------------------------------------------*
      *                  * Tipo documento                              *
      *                  *---------------------------------------------*
           if        w-tes-tip-doc (1)    not  = zero
                     go to cnt-tdo-nok-141.
           move      "Manca il tipo documento associato !             "
                                          to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-141.
           if        w-tes-tip-doc (1)    =    01 or
                     w-tes-tip-doc (1)    =    02 or
                     w-tes-tip-doc (1)    =    03 or
                     w-tes-tip-doc (1)    =    04 or
                     w-tes-tip-doc (1)    =    05 or
                     w-tes-tip-doc (1)    =    06
                     go to cnt-tdo-nok-145.
           move      "Tipo documento associato errato !               "
                                          to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-145.
      *                  *---------------------------------------------*
      *                  * Tipo fattura elettronica                    *
      *                  *---------------------------------------------*
           if        w-tes-tip-doc (1)    =    04
                     go to cnt-tdo-nok-150.
           if        w-tes-tdo-fel (1)    not  = spaces
                     go to cnt-tdo-nok-150.
           move      "Manca il tipo documento per 'sdi' !             "
                                          to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-150.
      *                  *---------------------------------------------*
      *                  * Origine del documento                       *
      *                  *---------------------------------------------*
           if        w-tes-tip-doc (1)    not  = 01
                     go to cnt-tdo-nok-160.
           if        w-tes-org-doc (1)    not  = zero
                     go to cnt-tdo-nok-151.
           move      "Manca l'origine del documento !                 "
                                          to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-151.
           if        w-tes-org-doc (1)    =    01 or
                     w-tes-org-doc (1)    =    02 or
                     w-tes-org-doc (1)    =    03 or
                     w-tes-org-doc (1)    =    11 or
                     w-tes-org-doc (1)    =    31 or
                     w-tes-org-doc (1)    =    41 or
                     w-tes-org-doc (1)    =    51
                     go to cnt-tdo-nok-160.
           move      "Origine del documento errata !                  "
                                          to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-160.
      *                  *---------------------------------------------*
      *                  * Causale contabile                           *
      *                  *---------------------------------------------*
           if        w-tes-cau-cge (1)    not  = zero
                     go to cnt-tdo-nok-170.
      *                      *-----------------------------------------*
      *                      * Se la causale contabile e' a zero : si  *
      *                      * saltano i controlli successivi          *
      *                      *-----------------------------------------*
           go to     cnt-tdo-nok-400.
       cnt-tdo-nok-170.
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda se tipo movimento      *
      *                  * 'normale o per 'giroconto'                  *
      *                  *---------------------------------------------*
           if        w-tes-tip-doc     (1)
                                          =    03      and
                    (w-tes-cau-cge-tmi (1)
                                          =    "0"  or
                     w-tes-cau-cge-tmi (1)
                                          =    spaces)
                     go to cnt-tdo-nok-300.
       cnt-tdo-nok-200.
      *              *-------------------------------------------------*
      *              * Controlli addizionali specifici per i tipi mo-  *
      *              * vimento 'normali'                               *
      *              *-------------------------------------------------*
       cnt-tdo-nok-210.
      *                  *---------------------------------------------*
      *                  * Tipo movimento iva della causale contabile  *
      *                  *---------------------------------------------*
           if        w-tes-tip-doc (1)    =    01 or
                     w-tes-tip-doc (1)    =    02 or
                     w-tes-tip-doc (1)    =    05 or
                     w-tes-tip-doc (1)    =    06
                     go to cnt-tdo-nok-211
           else      go to cnt-tdo-nok-212.
       cnt-tdo-nok-211.
           if        w-tes-cau-cge-tmi (1)
                                          =    "1" or
                     w-tes-cau-cge-tmi (1)
                                          =    "A"
                     go to cnt-tdo-nok-220
           else      go to cnt-tdo-nok-213.
       cnt-tdo-nok-212.
           if        w-tes-cau-cge-tmi (1)
                                          =    "2" or
                     w-tes-cau-cge-tmi (1)
                                          =    "B"
                     go to cnt-tdo-nok-220
           else      go to cnt-tdo-nok-213.
       cnt-tdo-nok-213.
           move      "Causale scorretta sotto il profilo i.v.a. !     "
                                          to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-220.
      *                  *---------------------------------------------*
      *                  * Test se default tipo riga accettabile       *
      *                  *---------------------------------------------*
           if        w-tes-def-tpr (1)    =    spaces   or
                     w-tes-def-tpr (1)    =    "P    "  or
                     w-tes-def-tpr (1)    =    "PN   "  or
                     w-tes-def-tpr (1)    =    "M    "  or
                     w-tes-def-tpr (1)    =    "S    "  or
                     w-tes-def-tpr (1)    =    "C    "  or
                     w-tes-def-tpr (1)    =    "A    "
                     go to cnt-tdo-nok-290.
      *                  *---------------------------------------------*
      *                  * Se valore errato                            *
      *                  *---------------------------------------------*
           move      "Tipo di riga proposto non accettabile !           
      -              "               "    to   w-err-box-err-msg      .
      *                  *---------------------------------------------*
      *                  * Ad errore                                   *
      *                  *---------------------------------------------*
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-290.
      *                  *---------------------------------------------*
      *                  * Fine controlli addizionali per tipi movi-   *
      *                  * mento 'normali'                             *
      *                  *---------------------------------------------*
           go to     cnt-tdo-nok-400.
       cnt-tdo-nok-300.
      *              *-------------------------------------------------*
      *              * Controlli addizionali specifici per i tipi mo-  *
      *              * vimento 'giroconto'                             *
      *              *-------------------------------------------------*
       cnt-tdo-nok-310.
      *                  *---------------------------------------------*
      *                  * Sigla numerazione                           *
      *                  *---------------------------------------------*
           if        w-tes-sgl-num (1)    not  = spaces
                     go to cnt-tdo-nok-320.
           move      "Manca la sigla della numerazione !              "
                                          to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-320.
      *                  *---------------------------------------------*
      *                  * Numero giornale iva                         *
      *                  *---------------------------------------------*
           if        w-tes-num-giv (1)    =    zero
                     go to cnt-tdo-nok-330.
           move      "Il numero giornale i.v.a. deve essere a zero !  "
                                          to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-330.
      *                  *---------------------------------------------*
      *                  * Fine controlli addizionali per tipi movi-   *
      *                  * mento 'giroconto'                           *
      *                  *---------------------------------------------*
           go to     cnt-tdo-nok-400.
       cnt-tdo-nok-400.
      *              *-------------------------------------------------*
      *              * Normalizzazioni comuni                          *
      *              *-------------------------------------------------*
       cnt-tdo-nok-410.
      *                  *---------------------------------------------*
      *                  * Provenienza del documento                   *
      *                  *---------------------------------------------*
           if       (w-tes-tip-doc (1)    =    01 or
                     w-tes-tip-doc (1)    =    03  ) and
                     w-tes-org-doc (1)    =    11
                     move  01             to   w-tes-prv-doc (1)
           else      move  zero           to   w-tes-prv-doc (1)      .
       cnt-tdo-nok-420.
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda se tipo movimento      *
      *                  * 'normale o per 'giroconto'                  *
      *                  *---------------------------------------------*
           if        w-tes-tip-doc     (1)
                                          =    03      and
                    (w-tes-cau-cge-tmi (1)
                                          =    "0"  or
                     w-tes-cau-cge-tmi (1)
                                          =    spaces)
                     go to cnt-tdo-nok-600.
       cnt-tdo-nok-500.
      *              *-------------------------------------------------*
      *              * Normalizzazioni addizionali specifiche per i    *
      *              * tipi movimento 'normali'                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Si/no stampa Agente                         *
      *                  *---------------------------------------------*
           if        w-tes-snx-age (1)    =    spaces
                     move  "N"            to   w-tes-snx-age (1)      .
      *                  *---------------------------------------------*
      *                  * Si/no stampa riferimenti conferma ordine    *
      *                  *---------------------------------------------*
           if        w-tes-snx-rco (1)    =    spaces
                     move  "S"            to   w-tes-snx-rco (1)      .
      *                  *---------------------------------------------*
      *                  * Si/no stampa riferimenti documento trasp.   *
      *                  *---------------------------------------------*
           if        w-tes-snx-rdt (1)    =    spaces
                     move  "S"            to   w-tes-snx-rdt (1)      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     cnt-tdo-nok-700.
       cnt-tdo-nok-600.
      *              *-------------------------------------------------*
      *              * Normalizzazioni addizionali specifiche per i    *
      *              * tipi movimento 'giroconto'                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su codice causale                      *
      *                  *---------------------------------------------*
           if        w-tes-cau-cge (1)    =    zero
                     go to cnt-tdo-nok-700.
      *                  *---------------------------------------------*
      *                  * Descrizione per la stampa                   *
      *                  *---------------------------------------------*
           move      "*"                  to   w-tes-des-stp (1)      .
      *                  *---------------------------------------------*
      *                  * Numero giornale iva                         *
      *                  *---------------------------------------------*
           move      zero                 to   w-tes-num-giv (1)      .
      *                  *---------------------------------------------*
      *                  * Fine normalizzazioni specifiche per i tipi  *
      *                  * movimento 'giroconto'                       *
      *                  *---------------------------------------------*
           go to     cnt-tdo-nok-700.
       cnt-tdo-nok-700.
      *              *-------------------------------------------------*
      *              * Normalizzazioni in relazione al tipo documento  *
      *              *-------------------------------------------------*
           if        w-tes-tip-doc (1)    not  = 04
                     go to cnt-tdo-nok-720.
      *                  *---------------------------------------------*
      *                  * Normalizzazione causale contabile           *
      *                  *---------------------------------------------*
           move      zero                 to   w-tes-cau-cge (1)      .
      *                  *---------------------------------------------*
      *                  * Normalizzazione numero giornale iva         *
      *                  *---------------------------------------------*
           move      zero                 to   w-tes-num-giv (1)      .
      *                  *---------------------------------------------*
      *                  * Normalizzazione sigla numerazione           *
      *                  *---------------------------------------------*
           if        w-tes-sgl-num (1)    not  = spaces
                     go to cnt-tdo-nok-720.
           move      "FPF"                to   w-tes-sgl-num (1)      .
       cnt-tdo-nok-720.
      *              *-------------------------------------------------*
      *              * Normalizzazioni in relazione al codice causale  *
      *              *-------------------------------------------------*
           if        w-tes-cau-cge (1)    not  = zero
                     go to cnt-tdo-nok-800.
      *                  *---------------------------------------------*
      *                  * Normalizzazione sottoconti                  *
      *                  *---------------------------------------------*
           move      zero                 to   w-tes-ctp-ivv (1)      .
           move      spaces               to   w-tes-ctp-ivv-des (1)  .
           move      zero                 to   w-tes-ctp-ven (1)      .
           move      spaces               to   w-tes-ctp-ven-des (1)  .
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
           move      spaces               to   w-tes-cod-tmo          .
       nor-key-reg-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione dati non chiave testata                   *
      *    *-----------------------------------------------------------*
       nor-nok-tes-000.
           move      spaces               to   w-tes-des-key (1)      .
           move      spaces               to   w-tes-des-tmo (1)      .
           move      spaces               to   w-tes-pwd-tmo (1)      .
           move      zero                 to   w-tes-vld-dpz (1)      .
           move      zero                 to   w-tes-cod-dpz (1)      .
           move      spaces               to   w-tes-cod-dpz-den (1)  .
           move      zero                 to   w-tes-tip-doc (1)      .
           move      spaces               to   w-tes-tdo-fel (1)      .
           move      zero                 to   w-tes-org-doc (1)      .
           move      zero                 to   w-tes-prv-doc (1)      .
           move      spaces               to   w-tes-sgl-num (1)      .
           move      zero                 to   w-tes-num-giv (1)      .
           move      spaces               to   w-tes-des-stp (1)      .
           move      zero                 to   w-tes-cau-cge (1)      .
           move      spaces               to   w-tes-cau-cge-des (1)  .
           move      spaces               to   w-tes-cau-cge-tmi (1)  .
           move      zero                 to   w-tes-ctp-ivv (1)      .
           move      spaces               to   w-tes-ctp-ivv-des (1)  .
           move      zero                 to   w-tes-ctp-ven (1)      .
           move      spaces               to   w-tes-ctp-ven-des (1)  .
           move      spaces               to   w-tes-def-tpr (1)      .
           move      spaces               to   w-tes-snx-age (1)      .
           move      spaces               to   w-tes-snx-rco (1)      .
           move      spaces               to   w-tes-snx-rdt (1)      .
           move      zero                 to   w-tes-cod-dct (1)      .
           move      spaces               to   w-tes-txt-dct (1)      .
           move      zero                 to   w-tes-cli-aft (1)      .
           move      spaces               to   w-tes-cli-aft-rag (1)  .
           move      spaces               to   w-tes-cod-mod (1)      .
           move      spaces               to   w-tes-cod-mod-des (1)  .
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
           move      "CODTMO    "         to   f-key                  .
           move      w-tes-cod-tmo        to   rf-zfi-cod-tmo         .
           move      "pgm/fat/fls/ioc/obj/iofzfi"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zfi                 .
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
      *                      *-----------------------------------------*
      *                      * Normalizzazione segnale di duplicazione *
      *                      *-----------------------------------------*
           move      spaces               to   w-cnt-dup-rec-flg      .
      *                      *-----------------------------------------*
      *                      * Determinazione valori attuali           *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Valori contenuti direttamente in    *
      *                          * record [zfi]                        *
      *                          *-------------------------------------*
           move      rf-zfi-des-key       to   w-tes-des-key (1)      .
           move      rf-zfi-des-tmo       to   w-tes-des-tmo (1)      .
           move      rf-zfi-pwd-tmo       to   w-tes-pwd-tmo (1)      .
           move      rf-zfi-vld-dpz       to   w-tes-vld-dpz (1)      .
           move      rf-zfi-cod-dpz       to   w-tes-cod-dpz (1)      .
           move      rf-zfi-tip-doc       to   w-tes-tip-doc (1)      .
           move      rf-zfi-org-doc       to   w-tes-org-doc (1)      .
           move      rf-zfi-prv-doc       to   w-tes-prv-doc (1)      .
           move      rf-zfi-sgl-num       to   w-tes-sgl-num (1)      .
           move      rf-zfi-num-giv       to   w-tes-num-giv (1)      .
           move      rf-zfi-des-stp       to   w-tes-des-stp (1)      .
           move      rf-zfi-cau-cge       to   w-tes-cau-cge (1)      .
           move      rf-zfi-ctp-ivv       to   w-tes-ctp-ivv (1)      .
           move      rf-zfi-ctp-ven       to   w-tes-ctp-ven (1)      .
           move      rf-zfi-def-tpr       to   w-tes-def-tpr (1)      .
           move      rf-zfi-snx-age       to   w-tes-snx-age (1)      .
           move      rf-zfi-snx-rco       to   w-tes-snx-rco (1)      .
           move      rf-zfi-snx-rdt       to   w-tes-snx-rdt (1)      .
           move      rf-zfi-cod-dct       to   w-tes-cod-dct (1)      .
           move      rf-zfi-cli-aft       to   w-tes-cli-aft (1)      .
      *
           if        rf-zfi-cli-aft       not  numeric
                     move  zero           to   w-tes-cli-aft (1)      .
      *
           move      rf-zfi-tdo-fel       to   w-tes-tdo-fel (1)      .
           move      rf-zfi-cod-mod       to   w-tes-cod-mod (1)      .
           move      rf-zfi-alx-exp       to   w-tes-alx-exp (1)      .
      *                          *-------------------------------------*
      *                          * Eventuali normalizzazioni           *
      *                          *-------------------------------------*
           if        rf-zfi-cod-dct       not  numeric
                     move  zero           to   w-tes-cod-dct (1)      .
      *                          *-------------------------------------*
      *                          * Valori contenuti indirettamente in  *
      *                          * record [zfi]                        *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Lettura archivio [ada]          *
      *                              *---------------------------------*
           move      w-tes-cod-dpz (1)    to   w-let-arc-ada-cod      .
           perform   let-arc-ada-000      thru let-arc-ada-999        .
           move      w-let-arc-ada-den    to   w-tes-cod-dpz-den (1)  .
      *                              *---------------------------------*
      *                              * Lettura archivio [zcc]          *
      *                              *---------------------------------*
           move      w-tes-cau-cge (1)    to   w-let-arc-zcc-cod      .
           perform   let-arc-zcc-000      thru let-arc-zcc-999        .
           move      w-let-arc-zcc-des    to   w-tes-cau-cge-des (1)  .
           move      w-let-arc-zcc-tmi    to   w-tes-cau-cge-tmi (1)  .
      *                              *---------------------------------*
      *                              * Lettura archivio [pdc]          *
      *                              *---------------------------------*
           move      w-tes-ctp-ivv (1)    to   w-let-arc-pdc-cod      .
           perform   let-arc-pdc-000      thru let-arc-pdc-999        .
           move      w-let-arc-pdc-des    to   w-tes-ctp-ivv-des (1)  .
      *                              *---------------------------------*
      *                              * Lettura archivio [pdc]          *
      *                              *---------------------------------*
           move      w-tes-ctp-ven (1)    to   w-let-arc-pdc-cod      .
           perform   let-arc-pdc-000      thru let-arc-pdc-999        .
           move      w-let-arc-pdc-des    to   w-tes-ctp-ven-des (1)  .
      *                              *---------------------------------*
      *                              * Lettura archivio [zac]          *
      *                              *---------------------------------*
           if        w-tes-cod-dct (1)    =    zero
                     go to rou-let-reg-600.
           move      03                   to   w-let-arc-zac-tip      .
           move      w-tes-cod-dct (1)    to   w-let-arc-zac-cod      .
           perform   let-arc-zac-000      thru let-arc-zac-999        .
           move      w-let-arc-zac-des    to   w-tes-txt-dct (1)      .
       rou-let-reg-600.
      *                              *---------------------------------*
      *                              * Lettura archivio [cli]          *
      *                              *---------------------------------*
           move      w-tes-cli-aft (1)    to   w-let-arc-cli-cod      .
           perform   let-arc-cli-000      thru let-arc-cli-999        .
           move      w-let-arc-cli-rag    to   w-tes-cli-aft-rag (1)  .
      *                              *---------------------------------*
      *                              * Lettura archivio [cli]          *
      *                              *---------------------------------*
           move      w-tes-cli-aft (1)    to   w-let-arc-cli-cod      .
           perform   let-arc-cli-000      thru let-arc-cli-999        .
           move      w-let-arc-cli-rag    to   w-tes-cli-aft-rag (1)  .
      *                              *---------------------------------*
      *                              * Lettura codice modulo           *
      *                              *---------------------------------*
           move      w-tes-cod-mod (1)    to   w-let-arc-mod-cod      .
           perform   let-arc-mod-000      thru let-arc-mod-999        .
           move      w-let-arc-mod-des    to   w-tes-cod-mod-des (1)  .
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
                     go to pre-acc-ins-500.
       pre-acc-ins-200.
      *                  *---------------------------------------------*
      *                  * Se richiesta la duplicazione                *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Da buffer a 'w-tes'                     *
      *                      *-----------------------------------------*
           move      w-tes-val-aep (2)    to   w-tes-val-aep (1)      .
      *                      *-----------------------------------------*
      *                      * Normalizzazione flags di controllo      *
      *                      *-----------------------------------------*
           move      "#"                  to   w-cnt-sts-imp-tes      .
           move      all "#"              to   w-cnt-sts-imp-pte      .
           move      all "#"              to   w-cnt-sts-ing-pte      .
      *                      *-----------------------------------------*
      *                      * Normalizzazione flags di visualizzazio- *
      *                      * ne per le pagine                        *
      *                      *-----------------------------------------*
           move      spaces               to   w-cnt-sts-vis-ptx (2)  .
           move      spaces               to   w-cnt-sts-vis-ptx (3)  .
           move      spaces               to   w-cnt-sts-vis-ptx (4)  .
           move      spaces               to   w-cnt-sts-vis-ptx (5)  .
           move      spaces               to   w-cnt-sts-vis-ptx (6)  .
           move      spaces               to   w-cnt-sts-vis-ptx (7)  .
           move      spaces               to   w-cnt-sts-vis-ptx (8)  .
           move      spaces               to   w-cnt-sts-vis-ptx (9)  .
      *                  *---------------------------------------------*
      *                  * Normalizzazione segnale di duplicazione     *
      *                  *---------------------------------------------*
           move      spaces               to   w-cnt-dup-rec-flg      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione pagine di testata           *
      *                  *---------------------------------------------*
           perform   vis-tes-reg-000      thru vis-tes-reg-999        .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     pre-acc-ins-999.
       pre-acc-ins-500.
      *                  *---------------------------------------------*
      *                  * Se non richiesta la duplicazione            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazione segnale di duplicazione *
      *                      *-----------------------------------------*
           move      spaces               to   w-cnt-dup-rec-flg      .
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
      *              * Trattamento file [zfi]                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se inserimento                              *
      *                  *---------------------------------------------*
           if        w-cnt-mfu-tip-fun    not  = "I"
                     go to scr-mov-fil-500.
      *                      *-----------------------------------------*
      *                      * Write record [zfi]                      *
      *                      *-----------------------------------------*
           perform   wrt-rec-zfi-000      thru wrt-rec-zfi-999        .
           go to     scr-mov-fil-999.
       scr-mov-fil-500.
      *                  *---------------------------------------------*
      *                  * Se modifica                                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Rewrite record [zfi]                    *
      *                      *-----------------------------------------*
           perform   rew-rec-zfi-000      thru rew-rec-zfi-999        .
       scr-mov-fil-999.
           exit.

      *    *===========================================================*
      *    * Delete movimento da file                                  *
      *    *-----------------------------------------------------------*
       del-mov-fil-000.
      *              *-------------------------------------------------*
      *              * Delete record [zfi]                             *
      *              *-------------------------------------------------*
           perform   del-rec-zfi-000      thru del-rec-zfi-999        .
       del-mov-fil-999.
           exit.

      *    *===========================================================*
      *    * Composizione record [zfi]                                 *
      *    *-----------------------------------------------------------*
       cmp-rec-zfi-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione record                          *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/fat/fls/ioc/obj/iofzfi"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zfi                 .
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Campi chiave                                *
      *                  *---------------------------------------------*
           move      w-tes-cod-tmo        to   rf-zfi-cod-tmo         .
      *                  *---------------------------------------------*
      *                  * Campi non chiave                            *
      *                  *---------------------------------------------*
           move      w-tes-des-key (1)    to   rf-zfi-des-key         .
           move      w-tes-des-tmo (1)    to   rf-zfi-des-tmo         .
           move      w-tes-pwd-tmo (1)    to   rf-zfi-pwd-tmo         .
           move      w-tes-vld-dpz (1)    to   rf-zfi-vld-dpz         .
           move      w-tes-cod-dpz (1)    to   rf-zfi-cod-dpz         .
           move      w-tes-tip-doc (1)    to   rf-zfi-tip-doc         .
           move      w-tes-org-doc (1)    to   rf-zfi-org-doc         .
           move      w-tes-prv-doc (1)    to   rf-zfi-prv-doc         .
           move      w-tes-sgl-num (1)    to   rf-zfi-sgl-num         .
           move      w-tes-num-giv (1)    to   rf-zfi-num-giv         .
           move      w-tes-des-stp (1)    to   rf-zfi-des-stp         .
           move      w-tes-cau-cge (1)    to   rf-zfi-cau-cge         .
           move      w-tes-ctp-ivv (1)    to   rf-zfi-ctp-ivv         .
           move      w-tes-ctp-ven (1)    to   rf-zfi-ctp-ven         .
           move      w-tes-def-tpr (1)    to   rf-zfi-def-tpr         .
           move      w-tes-snx-age (1)    to   rf-zfi-snx-age         .
           move      w-tes-snx-rco (1)    to   rf-zfi-snx-rco         .
           move      w-tes-snx-rdt (1)    to   rf-zfi-snx-rdt         .
           move      w-tes-cod-dct (1)    to   rf-zfi-cod-dct         .
           move      w-tes-cli-aft (1)    to   rf-zfi-cli-aft         .
           move      w-tes-tdo-fel (1)    to   rf-zfi-tdo-fel         .
           move      w-tes-cod-mod (1)    to   rf-zfi-cod-mod         .
           move      w-tes-alx-exp (1)    to   rf-zfi-alx-exp         .
       cmp-rec-zfi-999.
           exit.

      *    *===========================================================*
      *    * Scrittura record [zfi]                                    *
      *    *-----------------------------------------------------------*
       wrt-rec-zfi-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-zfi-000      thru cmp-rec-zfi-999        .
      *              *-------------------------------------------------*
      *              * Put record                                      *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      "pgm/fat/fls/ioc/obj/iofzfi"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zfi                 .
       wrt-rec-zfi-999.
           exit.

      *    *===========================================================*
      *    * Riscrittura record [zfi]                                  *
      *    *-----------------------------------------------------------*
       rew-rec-zfi-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-zfi-000      thru cmp-rec-zfi-999        .
      *              *-------------------------------------------------*
      *              * Forced put record                               *
      *              *-------------------------------------------------*
           move      "FP"                 to   f-ope                  .
           move      "pgm/fat/fls/ioc/obj/iofzfi"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zfi                 .
       rew-rec-zfi-999.
           exit.

      *    *===========================================================*
      *    * Cancellazione record [zfi]                                *
      *    *-----------------------------------------------------------*
       del-rec-zfi-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-zfi-000      thru cmp-rec-zfi-999        .
      *              *-------------------------------------------------*
      *              * Delete record                                   *
      *              *-------------------------------------------------*
           move      "DE"                 to   f-ope                  .
           move      "pgm/fat/fls/ioc/obj/iofzfi"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zfi                 .
       del-rec-zfi-999.
           exit.

      *    *===========================================================*
      *    * Find su archivio [ada]                                    *
      *    *-----------------------------------------------------------*
       fnd-arc-ada-000.
      *              *-------------------------------------------------*
      *              * Test se programma di interrogazione gia' attivo *
      *              *-------------------------------------------------*
           move      "P?"                 to   s-ope                  .
           move      "pazi0010"           to   s-pro                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-liv                not  = zero
                     move  "#"            to   w-fnd-arc-ada-sel
                     go to  fnd-arc-ada-999.
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
           move      "pgm/azi/prg/obj/pazi0010"
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
           move      "cod-dpz"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  spaces         to   w-fnd-arc-ada-sel
                     move  s-num          to   w-fnd-arc-ada-cod
           else      move  "#"            to   w-fnd-arc-ada-sel      .
       fnd-arc-ada-999.
           exit.

      *    *===========================================================*
      *    * Find su tipi fattura elettronica                          *
      *    *-----------------------------------------------------------*
       fnd-tdo-fel-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di selezione               *
      *              *-------------------------------------------------*
           move      spaces               to   w-fnd-tdo-fel-sel      .
      *              *-------------------------------------------------*
      *              * Salvataggio del video attuale                   *
      *              *-------------------------------------------------*
           move      "SV"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Accettazione provvisoria di un pop-up per il    *
      *              * tipo fattura elettronica                        *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tdo-fel-lun    to   v-car                  .
           move      w-exp-tdo-fel-num    to   v-ldt                  .
           move      spaces               to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      13                   to   v-lin                  .
           move      35                   to   v-pos                  .
           move      w-exp-tdo-fel-tbl    to   v-txt                  .
           move      "EXIT"               to   v-pfk (20)             .
           move      zero                 to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Valore in campo di destinazione                 *
      *              *-------------------------------------------------*
           if        v-num                =    01
                     move  "TD01"         to   w-fnd-tdo-fel-tdo
           else if   v-num                =    02
                     move  "TD02"         to   w-fnd-tdo-fel-tdo
           else if   v-num                =    03
                     move  "TD03"         to   w-fnd-tdo-fel-tdo
           else if   v-num                =    04
                     move  "TD04"         to   w-fnd-tdo-fel-tdo
           else if   v-num                =    05
                     move  "TD05"         to   w-fnd-tdo-fel-tdo
           else if   v-num                =    06
                     move  "TD06"         to   w-fnd-tdo-fel-tdo
           else if   v-num                =    07
                     move  "TD16"         to   w-fnd-tdo-fel-tdo
           else if   v-num                =    08
                     move  "TD17"         to   w-fnd-tdo-fel-tdo
           else if   v-num                =    09
                     move  "TD18"         to   w-fnd-tdo-fel-tdo
           else if   v-num                =    10
                     move  "TD19"         to   w-fnd-tdo-fel-tdo
           else if   v-num                =    11
                     move  "TD20"         to   w-fnd-tdo-fel-tdo
           else if   v-num                =    12
                     move  "TD21"         to   w-fnd-tdo-fel-tdo
           else if   v-num                =    13
                     move  "TD22"         to   w-fnd-tdo-fel-tdo
           else if   v-num                =    14
                     move  "TD23"         to   w-fnd-tdo-fel-tdo
           else if   v-num                =    15
                     move  "TD24"         to   w-fnd-tdo-fel-tdo
           else if   v-num                =    16
                     move  "TD25"         to   w-fnd-tdo-fel-tdo
           else if   v-num                =    17
                     move  "TD26"         to   w-fnd-tdo-fel-tdo
           else if   v-num                =    18
                     move  "TD27"         to   w-fnd-tdo-fel-tdo
           else      move  spaces         to   w-fnd-tdo-fel-tdo      .
      *              *-------------------------------------------------*
      *              * Ripristino immagine video                       *
      *              *-------------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       fnd-tdo-fel-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     fnd-tdo-fel-999.
       fnd-tdo-fel-999.
           exit.

      *    *===========================================================*
      *    * Controllo tipo fattura elettronica                        *
      *    *-----------------------------------------------------------*
       ctl-tdo-fel-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-ctl-tdo-fel-flg      .
      *              *-------------------------------------------------*
      *              * Se a spazi, nessun controllo                    *
      *              *-------------------------------------------------*
           if        w-ctl-tdo-fel-tdo    =    spaces
                     go to ctl-tdo-fel-999.
       ctl-tdo-fel-100.
      *              *-------------------------------------------------*
      *              * Test che non ci siano blanks embedded           *
      *              *-------------------------------------------------*
           move      w-ctl-tdo-fel-tdo    to   w-all-str-alf          .
           move      04                   to   w-all-str-lun          .
           perform   all-str-ble-000      thru all-str-ble-999        .
           if        w-all-str-flg        not  = spaces
                     go to ctl-tdo-fel-900.
       ctl-tdo-fel-200.
      *              *-------------------------------------------------*
      *              * Test che i primi due caratteri siano 'TD'       *
      *              *-------------------------------------------------*
           if        w-ctl-tdo-fel-chr (1)
                                          =    "T" and
                     w-ctl-tdo-fel-chr (2)
                                          =    "D"
                     go to ctl-tdo-fel-300
           else      go to ctl-tdo-fel-900.
       ctl-tdo-fel-300.
      *              *-------------------------------------------------*
      *              * Test che il terzo carattere sia '0', '1' o '2'  *
      *              *-------------------------------------------------*
           if        w-ctl-tdo-fel-chr (3)
                                          =    "0" or
                     w-ctl-tdo-fel-chr (3)
                                          =    "1" or
                     w-ctl-tdo-fel-chr (3)
                                          =    "2"
                     go to ctl-tdo-fel-400
           else      go to ctl-tdo-fel-900.
       ctl-tdo-fel-400.
      *              *-------------------------------------------------*
      *              * Test di congruenza del quarto carattere         *
      *              *-------------------------------------------------*
           if        w-ctl-tdo-fel-chr (3)
                                          =    "0"
                     go to ctl-tdo-fel-420
           else if   w-ctl-tdo-fel-chr (3)
                                          =    "1"
                     go to ctl-tdo-fel-440
           else if   w-ctl-tdo-fel-chr (3)
                                          =    "2"
                     go to ctl-tdo-fel-460
           else      go to ctl-tdo-fel-900.
       ctl-tdo-fel-420.
      *              *-------------------------------------------------*
      *              * Test se 'TD01' --> 'TD06'                       *
      *              *-------------------------------------------------*
           if        w-ctl-tdo-fel-chr (4)
                                          =    "1" or
                     w-ctl-tdo-fel-chr (4)
                                          =    "2" or
                     w-ctl-tdo-fel-chr (4)
                                          =    "3" or
                     w-ctl-tdo-fel-chr (4)
                                          =    "4" or
                     w-ctl-tdo-fel-chr (4)
                                          =    "5" or
                     w-ctl-tdo-fel-chr (4)
                                          =    "6"
                     go to ctl-tdo-fel-800
           else      go to ctl-tdo-fel-900.
       ctl-tdo-fel-440.
      *              *-------------------------------------------------*
      *              * Test se 'TD16' --> 'TD19'                       *
      *              *-------------------------------------------------*
           if        w-ctl-tdo-fel-chr (4)
                                          =    "6" or
                     w-ctl-tdo-fel-chr (4)
                                          =    "7" or
                     w-ctl-tdo-fel-chr (4)
                                          =    "8" or
                     w-ctl-tdo-fel-chr (4)
                                          =    "9"
                     go to ctl-tdo-fel-800
           else      go to ctl-tdo-fel-900.
       ctl-tdo-fel-460.
      *              *-------------------------------------------------*
      *              * Test se 'TD20' --> 'TD27'                       *
      *              *-------------------------------------------------*
           if        w-ctl-tdo-fel-chr (4)
                                          =    "0" or
                     w-ctl-tdo-fel-chr (4)
                                          =    "1" or
                     w-ctl-tdo-fel-chr (4)
                                          =    "2" or
                     w-ctl-tdo-fel-chr (4)
                                          =    "3" or
                     w-ctl-tdo-fel-chr (4)
                                          =    "4" or
                     w-ctl-tdo-fel-chr (4)
                                          =    "5" or
                     w-ctl-tdo-fel-chr (4)
                                          =    "6" or
                     w-ctl-tdo-fel-chr (4)
                                          =    "7"
                     go to ctl-tdo-fel-800
           else      go to ctl-tdo-fel-900.
       ctl-tdo-fel-800.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     ctl-tdo-fel-999.
       ctl-tdo-fel-900.
      *              *-------------------------------------------------*
      *              * Uscita con errore                               *
      *              *-------------------------------------------------*
           move      "#"                  to   w-ctl-tdo-fel-flg      .
       ctl-tdo-fel-999.
           exit.

      *    *===========================================================*
      *    * Find su codici modulo di [pss]                            *
      *    *-----------------------------------------------------------*
       fnd-pss-mod-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di selezione               *
      *              *-------------------------------------------------*
           move      spaces               to   w-fnd-pss-mod-sel      .
      *              *-------------------------------------------------*
      *              * Test se programma di interrogazione gia' attivo *
      *              *-------------------------------------------------*
           move      "P?"                 to   s-ope                  .
           move      "pxpg4310"           to   s-pro                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-liv                not  = zero
                     move  "#"            to   w-fnd-pss-mod-sel
                     go to  fnd-pss-mod-999.
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
      *              * Preparazione variabile di i.p.c. 'cod-mod'      *
      *              *-------------------------------------------------*
           move      "PV"                 to   s-ope                  .
           move      "cod-mod"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      08                   to   s-car                  .
           move      w-fnd-pss-mod-cod    to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Richiamo programma di interrogazione            *
      *              *-------------------------------------------------*
           call      "swd/xpg/prg/obj/pxpg4310"                       .
           cancel    "swd/xpg/prg/obj/pxpg4310"                       .
      *              *-------------------------------------------------*
      *              * Estrazione di eventuale variabile di i.p.c. de- *
      *              * terminata da function-key "SLCT" durante l'in-  *
      *              * terrogazione                                    *
      *              *-------------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "pss-mod"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  spaces         to   w-fnd-pss-mod-sel
                     move  s-alf          to   w-fnd-pss-mod-cod
           else      move  "#"            to   w-fnd-pss-mod-sel      .
       fnd-pss-mod-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [ada]                         *
      *    *-----------------------------------------------------------*
       let-arc-ada-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-ada-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice a zero                           *
      *              *-------------------------------------------------*
           if        w-let-arc-ada-cod    =    zero
                     go to let-arc-ada-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODDPZ    "         to   f-key                  .
           move      w-let-arc-ada-cod    to   rf-ada-cod-dpz         .
           move      "pgm/azi/fls/ioc/obj/iofada"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ada                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-ada-400.
       let-arc-ada-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-ada-cod-mne       to   w-let-arc-ada-den      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-ada-999.
       let-arc-ada-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-ada-flg      .
           move      all   "."            to   w-let-arc-ada-den      .
           go to     let-arc-ada-999.
       let-arc-ada-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-ada-den      .
       let-arc-ada-999.
           exit.

      *    *===========================================================*
      *    * Routine lettura archivio [zcc]                            *
      *    *-----------------------------------------------------------*
       let-arc-zcc-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zcc-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice causale a zero                   *
      *              *-------------------------------------------------*
           if        w-let-arc-zcc-cod    =    zero
                     go to let-arc-zcc-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCAU    "         to   f-key                  .
           move      w-let-arc-zcc-cod    to   rf-zcc-cod-cau         .
           move      "pgm/cge/fls/ioc/obj/iofzcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zcc                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-zcc-400.
       let-arc-zcc-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-zcc-des-cau       to   w-let-arc-zcc-des      .
           move      rf-zcc-tip-moi       to   w-let-arc-zcc-tmi      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-zcc-999.
       let-arc-zcc-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-zcc-flg      .
           move      all   "."            to   w-let-arc-zcc-des      .
           go to     let-arc-zcc-600.
       let-arc-zcc-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zcc-des      .
       let-arc-zcc-600.
           move      spaces               to   w-let-arc-zcc-tmi      .
       let-arc-zcc-999.
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
      *    * Routine di lettura archivio [zac]                         *
      *    *-----------------------------------------------------------*
       let-arc-zac-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zac-flg      .
      *              *-------------------------------------------------*
      *              * Test se codici nulli                            *
      *              *-------------------------------------------------*
           if        w-let-arc-zac-tip    =    zero or
                     w-let-arc-zac-cod    =    zero
                     go to let-arc-zac-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODAOC    "         to   f-key                  .
           move      w-let-arc-zac-tip    to   rf-zac-tip-rec         .
           move      w-let-arc-zac-cod    to   rf-zac-cod-aoc         .
           move      "pgm/fat/fls/ioc/obj/iofzac"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zac                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-zac-400.
       let-arc-zac-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-zac-des-aoc       to   w-let-arc-zac-des      .
           move      rf-zac-cod-iva       to   w-let-arc-zac-civ      .
           move      rf-zac-cod-ctp       to   w-let-arc-zac-ccp      .
           move      rf-zac-tip-tot       to   w-let-arc-zac-tot      .
           if        rf-zac-tip-tot       not  numeric
                     move  zero           to   w-let-arc-zac-tot      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-zac-999.
       let-arc-zac-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-zac-flg      .
           move      spaces               to   w-let-arc-zac-des      .
           move      all   "."            to   w-let-arc-zac-drg (1)  .
           go to     let-arc-zac-600.
       let-arc-zac-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zac-des      .
       let-arc-zac-600.
           move      zero                 to   w-let-arc-zac-civ      .
           move      zero                 to   w-let-arc-zac-ccp      .
           move      zero                 to   w-let-arc-zac-tot      .
       let-arc-zac-999.
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
      *              * Test se codice a zero                           *
      *              *-------------------------------------------------*
           if        w-let-arc-cli-cod    =    zero
                     go to let-arc-cli-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCLI"             to   f-key                  .
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
       let-arc-cli-290.
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
           go to     let-arc-cli-999.
       let-arc-cli-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-cli-rag      .
       let-arc-cli-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura sub-archivio [mod]                     *
      *    *-----------------------------------------------------------*
       let-arc-mod-000.
      *              *-------------------------------------------------*
      *              * Se codice modulo a spaces                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           if        w-let-arc-mod-cod    not  = spaces
                     go to let-arc-mod-100.
      *                  *---------------------------------------------*
      *                  * Flag di uscita                              *
      *                  *---------------------------------------------*
           move      spaces               to   w-let-arc-mod-flg      .
      *                  *---------------------------------------------*
      *                  * Descrizione                                 *
      *                  *---------------------------------------------*
           move      spaces               to   w-let-arc-mod-des      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     let-arc-mod-999.
       let-arc-mod-100.
      *              *-------------------------------------------------*
      *              * Lettura record, e deviazione a seconda dell'e-  *
      *              * sito della lettura                              *
      *              *-------------------------------------------------*
           move      "RD"                 to   j-ope                  .
           move      "MOD "               to   j-tre                  .
           move      w-let-arc-mod-cod    to   j-kre                  .
           call      "swd/mod/prg/obj/mppssf"
                                        using  r
                                               j                      .
           if        j-rsc                not  = spaces
                     go to let-arc-mod-300.
       let-arc-mod-200.
      *              *-------------------------------------------------*
      *              * Se record esistente                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Record letto in area di ridefinizione       *
      *                  *---------------------------------------------*
           move      j-dat                to   w-mod                  .
      *                  *---------------------------------------------*
      *                  * Flag di uscita                              *
      *                  *---------------------------------------------*
           move      spaces               to   w-let-arc-mod-flg      .
      *                  *---------------------------------------------*
      *                  * Descrizione                                 *
      *                  *---------------------------------------------*
           move      w-mod-des-mod        to   w-let-arc-mod-des      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     let-arc-mod-999.
       let-arc-mod-300.
      *              *-------------------------------------------------*
      *              * Se record non esistente                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Flag di uscita                              *
      *                  *---------------------------------------------*
           move      "#"                  to   w-let-arc-mod-flg      .
      *                  *---------------------------------------------*
      *                  * Descrizione                                 *
      *                  *---------------------------------------------*
           move      all   "."            to   w-let-arc-mod-des      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     let-arc-mod-999.
       let-arc-mod-999.
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
           copy      "pgm/fat/prg/cpy/acdezfi0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione della causale contabile    *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/acmnzcc0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice sottoconto      *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/acmnpdc0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione addebito o commento        *
      *    *-----------------------------------------------------------*
           copy      "pgm/fat/prg/cpy/acmnzac0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice cliente         *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/acmncli0.acs"                   .

