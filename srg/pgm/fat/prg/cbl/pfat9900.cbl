       Identification Division.
       Program-Id.                                 pfat9900           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    fat                 *
      *                                Settore:    uti                 *
      *                                   Fase:    fat990              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 01/09/91    *
      *                       Ultima revisione:    NdK del 31/01/25    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Update fatture                              *
      *                                                                *
      *                    ___ DA COMPLETARE pagina 3 ___              *
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
                     "uti"                                            .
      *        *-------------------------------------------------------*
      *        * Fase gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-fas                  pic  x(06) value
                     "fat990"                                         .
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10) value
                     "pfat9900"                                       .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  i-ide-des                  pic  x(40) value
                     "   Update testata documenti 'fattura'   "       .

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
      *        * [fit]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/fat/fls/rec/rffit"                          .
      *        *-------------------------------------------------------*
      *        * [fir]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/fat/fls/rec/rffir"                          .
      *        *-------------------------------------------------------*
      *        * [fix]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/fat/fls/rec/rffix"                          .
      *        *-------------------------------------------------------*
      *        * [zsf]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfzsf"                          .

      *    *===========================================================*
      *    * Work-area personalizzazioni                               *
      *    *-----------------------------------------------------------*
       01  w-prs.
      *        *-------------------------------------------------------*
      *        * Personalizzazioni relative alle spese per la fattura- *
      *        * zione                                                 *
      *        *-------------------------------------------------------*
           05  w-prs-spe-fat.
      *            *---------------------------------------------------*
      *            * Tipo accettazione                                 *
      *            *---------------------------------------------------*
               10  w-prs-spe-fat-tac      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Numero di spese personalizzate totale             *
      *            *---------------------------------------------------*
               10  w-prs-spe-fat-nst      pic  9(01)                  .
      *            *---------------------------------------------------*
      *            * Tabella spese personalizzate, impaccate in alto,  *
      *            * le spese non personalizzate non sono contenute    *
      *            * nella tabella                                     *
      *            *---------------------------------------------------*
               10  w-prs-spe-fat-tbl occurs 06.
      *                *-----------------------------------------------*
      *                * Numero spesa : 1..6                           *
      *                *-----------------------------------------------*
                   15  w-prs-spe-fat-npt  pic  9(01)                  .
      *                *-----------------------------------------------*
      *                * Descrizione per il video estesa               *
      *                *-----------------------------------------------*
                   15  w-prs-spe-fat-dve  pic  x(25)                  .
      *                *-----------------------------------------------*
      *                * Tipo funzionamento spesa                      *
      *                * - 01 : A percentuale fissa per tutti i clien- *
      *                *        ti cui viene addebitata la spesa       *
      *                * - 02 : A percentuale variabile per ogni cli-  *
      *                *        ente cui viene addebitata la spesa,    *
      *                *        con la percentuale di spesa espressa   *
      *                *        nel record dati commerciali cliente    *
      *                * - 03 : Ad importo fisso per tutti i clienti   *
      *                *        cui viene addebitata la spesa          *
      *                * - 04 : Ad importo variabile per tutti i cli-  *
      *                *        enti cui viene addebitata la spesa,    *
      *                *        con l'importo della spesa espresso nel *
      *                *        record dati commerciali cliente        *
      *                * - 05 : A seconda dei clienti, per ognuno dei  *
      *                *        quali si esprimera', nel record dati   *
      *                *        commerciali, se la spesa :             *
      *                *        - non va' addebitata                   *
      *                *        - va' addebitata in percentuale        *
      *                *        - va' addebitata ad importo            *
      *                *        e dove inoltre si esprimera' l'even-   *
      *                *        tuale percentuale o importo da addebi- *
      *                *        tare                                   *
      *                *-----------------------------------------------*
                   15  w-prs-spe-fat-tfs  pic  9(02)                  .
      *                *-----------------------------------------------*
      *                * Percentuale fissa per la spesa, in caso che   *
      *                * il tipo funzionamento spesa sia pari a 01,    *
      *                * oppure di default per l'anagrafica dati com-  *
      *                * merciali cliente nel caso che il tipo fun-    *
      *                * zionamento spesa sia pari a 02                *
      *                *-----------------------------------------------*
                   15  w-prs-spe-fat-per  pic  9(02)v9(01)            .
      *                *-----------------------------------------------*
      *                * Imponibile per la percentuale della spesa,    *
      *                * solo se il tipo funzionamento spesa e' pari a *
      *                * 01 o 02 o 05                                  *
      *                * - 00 : Non significativo                      *
      *                * - 01 : Totale merce                           *
      *                * - 02 : Totale netto                           *
      *                * - 03 : Su una combinazione delle voci seguenti*
      *                * - 11 : Totale merci                           *
      *                * - 12 : Totale servizi                         *
      *                * - 13 : Totale imballi                         *
      *                * - 14 : Totale libero 4                        *
      *                * - 15 : Totale libero 5                        *
      *                * - 16 : Totale libero 6                        *
      *                * - 17 : Totale libero 7                        *
      *                * - 18 : Totale libero 8                        *
      *                * - 19 : Totale extra                           *
      *                *-----------------------------------------------*
                   15  w-prs-spe-fat-ibl  pic  9(02)                  .
      *                *-----------------------------------------------*
      *                * Totalizzatori da considerarsi per la determi- *
      *                * nazione dell'imponibile per la percentuale    *
      *                * della spesa                                   *
      *                *                                               *
      *                * Solo se l'imponibile per la percentuale e'    *
      *                * pari a 03                                     *
      *                *                                               *
      *                * Per ogni totalizzatore deve essere specifica- *
      *                * to un valore di un carattere, dove :          *
      *                *                                               *
      *                * - N : Non concorre a formare l'imponibile     *
      *                * - S : Concorre a formare l'imponibile         *
      *                *                                               *
      *                * Il significato dei totalizzatori e' di tipo   *
      *                * posizionale, con la seguente codifica :       *
      *                *                                               *
      *                * 1 : Totale merci                              *
      *                * 2 : Totale servizi                            *
      *                * 3 : Totale imballi                            *
      *                * 4 : Totale libero 4                           *
      *                * 5 : Totale libero 5                           *
      *                * 6 : Totale libero 6                           *
      *                * 7 : Totale libero 7                           *
      *                * 8 : Totale libero 8                           *
      *                * 9 : Totale extra                              *
      *                *-----------------------------------------------*
                   15  w-prs-spe-fat-ibt.
                       20  w-prs-spe-fat-ibx occurs 09
                                          pic  x(01)                  .
      *                *-----------------------------------------------*
      *                * Importo fisso per la spesa in caso che il ti- *
      *                * po funzionamento spesa e' pari a 03, oppure   *
      *                * di default per l'anagrafica dati commerciali  *
      *                * cliente nel caso che il tipo funzionamento    *
      *                * spesa sia pari a 04                           *
      *                *-----------------------------------------------*
                   15  w-prs-spe-fat-imp  pic  9(09)                  .
      *                *-----------------------------------------------*
      *                * Codice iva per la spesa. Se zero significa che*
      *                * la spesa non e' soggetta ad una aliquota iva  *
      *                * fissa, bensi' che l'importo della spesa va'   *
      *                * sventagliato in proporzione agli altri imponi-*
      *                * bili di fattura                               *
      *                *-----------------------------------------------*
                   15  w-prs-spe-fat-civ  pic  9(05)                  .
      *                *-----------------------------------------------*
      *                * Codice contropartita per la spesa. Se zero si-*
      *                * gnifica che la spesa non ha una contropartita *
      *                * fissa, bensi' che l'importo della spesa va'   *
      *                * sventagliato in proporzione alle altre contro-*
      *                * partite di fattura                            *
      *                *-----------------------------------------------*
                   15  w-prs-spe-fat-ccp  pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Contatori, indici, e comodi locali                *
      *            *---------------------------------------------------*
               10  w-prs-spe-fat-i01      pic  9(01)                  .
               10  w-prs-spe-fat-i02      pic  9(02)                  .

      *    *===========================================================*
      *    * Work per subroutines di Pmt                               *
      *    *-----------------------------------------------------------*
       01  w-pmt.
      *        *-------------------------------------------------------*
      *        * Work per Pmt : Importi e percentuali spese in fattura *
      *        *-------------------------------------------------------*
           05  w-pmt-iep-spe.
               10  w-pmt-iep-spe-ctr      pic  9(02)                  .

      *    *===========================================================*
      *    * Work-area per accettazioni                                *
      *    *-----------------------------------------------------------*
       01  w-acc.
      *        *-------------------------------------------------------*
      *        * Codice cliente                                        *
      *        *-------------------------------------------------------*
           05  w-acc-cod-arc              pic  9(07)                  .
           05  w-acc-dpz-arc              pic  x(07)                  .
      *        *-------------------------------------------------------*
      *        * Data documento                                        *
      *        *-------------------------------------------------------*
           05  w-acc-dat-doc              pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Numero documento                                      *
      *        *-------------------------------------------------------*
           05  w-acc-num-doc              pic  9(11)                  .
           05  w-acc-num-doc-r redefines
               w-acc-num-doc.
               10  w-acc-num-doc-saa      pic  9(03)                  .
               10  w-acc-num-doc-ngi      pic  9(02)                  .
               10  w-acc-num-doc-prg      pic  9(06)                  .
      *        *-------------------------------------------------------*
      *        * Tipo documento                                        *
      *        *-------------------------------------------------------*
           05  w-acc-cod-tmo              pic  x(05)                  .
      *        *-------------------------------------------------------*
      *        * Codice dipendenza                                     *
      *        *-------------------------------------------------------*
           05  w-acc-cod-dpz              pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Castelletto iva                                       *
      *        *-------------------------------------------------------*
           05  w-acc-iva-cst.
               10  w-acc-iva-rig occurs 06.
                   15  w-acc-iva-cod      pic  9(05)                  .
                   15  w-acc-iva-ibv      pic s9(11)                  .
                   15  w-acc-iva-ibl      pic s9(11)                  .
                   15  w-acc-iva-imp      pic s9(11)                  .
               10  w-acc-iva-tiv          pic s9(11)                  .
               10  w-acc-iva-tib          pic s9(11)                  .
               10  w-acc-iva-tim          pic s9(11)                  .
               10  w-acc-iva-tnd          pic s9(11)                  .
               10  w-acc-iva-tdo          pic s9(11)                  .
      *        *-------------------------------------------------------*
      *        * Work per Acc : Importi e percentuali spese in fattura *
      *        *-------------------------------------------------------*
           05  w-acc-iep-spe.
               10  w-acc-iep-spe-ctr      pic  9(02)                  .
               10  w-acc-iep-spe-dir      pic  x(01)                  .
               10  w-acc-iep-spe-inx      pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Comodi per cancellazione                              *
      *        *-------------------------------------------------------*
           05  w-acc-odc.
               10  w-acc-odc-tes          pic  x(01)                  .
               10  w-acc-odc-rig          pic  x(01)                  .

      *    *===========================================================*
      *    * Work per subroutines di Vis                               *
      *    *-----------------------------------------------------------*
       01  w-vis.
      *        *-------------------------------------------------------*
      *        * Work per Vis : Importi e percentuali spese in fattura *
      *        *-------------------------------------------------------*
           05  w-vis-iep-spe.
               10  w-vis-iep-spe-ctr      pic  9(02)                  .
               10  w-vis-iep-spe-inx      pic  9(02)                  .

      *    *===========================================================*
      *    * Work per accettazioni campi espansi                       *
      *    *-----------------------------------------------------------*
       01  w-exp.
      *        *-------------------------------------------------------*
      *        * Work per : Opzioni di cancellazione                   *
      *        *-------------------------------------------------------*
           05  w-exp-odc-doc.
               10  w-exp-odc-doc-num      pic  9(02)       value 2    .
               10  w-exp-odc-doc-lun      pic  9(02)       value 40   .
               10  w-exp-odc-doc-tbl.
                   15  filler             pic  x(40) value
                            "[ ] Cancella testata documento          ".
                   15  filler             pic  x(40) value
                            "[ ] Cancella righe documento            ".
      *        *-------------------------------------------------------*
      *        * Work per : No/Si addebito spese in fattura            *
      *        *-------------------------------------------------------*
           05  w-exp-spe-snx.
               10  w-exp-spe-snx-num      pic  9(02)       value 2    .
               10  w-exp-spe-snx-lun      pic  9(02)       value 02   .
               10  w-exp-spe-snx-tbl.
                   15  filler             pic  x(02) value
                            "No"                                      .
                   15  filler             pic  x(02) value
                            "Si"                                      .

      *    *===========================================================*
      *    * Work-area per bufferizzazione righe                       *
      *    *-----------------------------------------------------------*
       01  w-rig.
      *        *-------------------------------------------------------*
      *        * Numero massimo di elementi                            *
      *        *-------------------------------------------------------*
           05  w-rig-max-ele              pic  9(05)     value 999    .
      *        *-------------------------------------------------------*
      *        * Contatore elementi                                    *
      *        *-------------------------------------------------------*
           05  w-rig-num-ele              pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Contatori di comodo                                   *
      *        *-------------------------------------------------------*
           05  w-rig-ctr-ele              pic  9(05)                  .
           05  w-rig-ctr-001              pic  9(05)                  .
           05  w-rig-ctr-002              pic  9(05)                  .
           05  w-rig-ctr-003              pic  9(05)                  .
           05  w-rig-ctr-004              pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Protocollo                                            *
      *        *-------------------------------------------------------*
           05  w-rig-num-prt              pic  9(09)                  .
      *        *-------------------------------------------------------*
      *        * Castelletto progressivi riga salvati                  *
      *        *-------------------------------------------------------*
           05  w-rig-cst-rig.
      *            *---------------------------------------------------*
      *            * Singolo elemento                                  *
      *            *---------------------------------------------------*
               10  w-rig-sng-ele  occurs 999.
      *                *-----------------------------------------------*
      *                * Chiave di ordinamento                         *
      *                *-----------------------------------------------*
                   15  w-rig-key-ord.
      *                    *-------------------------------------------*
      *                    * Chiave                                    *
      *                    *-------------------------------------------*
                       20  w-rig-key-prg  pic  9(05)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Comodo per salvataggio chiave di ordinamento          *
      *        *-------------------------------------------------------*
           05  w-rig-sav-key              pic  x(60)                  .

      *    *===========================================================*
      *    * Work area per accettazione castelletto iva in w-tes       *
      *    *-----------------------------------------------------------*
       01  w-cst.
           05  w-cst-flg-exi              pic  x(01)                  .
           05  w-cst-ctr-rig              pic  9(02)                  .
           05  w-cst-cts-rig              pic  9(02)                  .
           05  w-cst-ctx-rig              pic  9(02)                  .
           05  w-cst-cty-rig              pic  9(02)                  .
           05  w-cst-ctz-rig              pic  9(02)                  .
           05  w-cst-wrk-s13              pic s9(13)                  .
           05  w-cst-cod-iva              pic  9(05)                  .
           05  w-cst-cod-ivr redefines
               w-cst-cod-iva.
               10  w-cst-cod-001          pic  9(01)                  .
               10  w-cst-cod-002          pic  9(01)                  .
               10  w-cst-cod-003          pic  9(01)                  .
               10  w-cst-cod-045          pic  9(02)                  .
           05  w-cst-tot-rig              pic s9(13)                  .
           05  w-cst-des-iva              pic  x(15)                  .
           05  w-cst-ctr-wrk              pic  9(02)                  .

      *    *===========================================================*
      *    * Work area per salvataggi                                  *
      *    *-----------------------------------------------------------*
       01  w-sav.
      *        *-------------------------------------------------------*
      *        * Codice iva                                            *
      *        *-------------------------------------------------------*
           05  w-sav-iva-cod              pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Imponibile iva in valuta base                         *
      *        *-------------------------------------------------------*
           05  w-sav-iva-ibl              pic s9(11)                  .
      *        *-------------------------------------------------------*
      *        * Imposta iva                                           *
      *        *-------------------------------------------------------*
           05  w-sav-iva-imp              pic s9(11)                  .
      *        *-------------------------------------------------------*
      *        * Totale documento                                      *
      *        *-------------------------------------------------------*
           05  w-sav-iva-tdo              pic s9(11)                  .
      *        *-------------------------------------------------------*
      *        * Si/No addebito spesa                                  *
      *        *-------------------------------------------------------*
           05  w-sav-spe-snx              pic  9(01)                  .
      *        *-------------------------------------------------------*
      *        * Importo spese                                         *
      *        *-------------------------------------------------------*
           05  w-sav-spe-imp              pic s9(09)                  .
      *        *-------------------------------------------------------*
      *        * % spesa                                               *
      *        *-------------------------------------------------------*
           05  w-sav-spe-per              pic  9(02)v9(01)            .

      *    *===========================================================*
      *    * Work per subroutines di Find                              *
      *    *-----------------------------------------------------------*
       01  w-fnd.
      *        *-------------------------------------------------------*
      *        * Work per Find su codici iva                           *
      *        *-------------------------------------------------------*
           05  w-fnd-cod-iva.
               10  w-fnd-cod-iva-sel      pic  x(01)                  .
               10  w-fnd-cod-iva-cod      pic  9(05)                  .

      *    *===========================================================*
      *    * Area di comunicazione per determinazione imposta in base  *
      *    * ad un imponibile                                          *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/dimpiva0.dtl"                   .

      *    *===========================================================*
      *    * Area di interfaccia per sottoprogramma         "pazi000d" *
      *    *-----------------------------------------------------------*
           copy      "pgm/azi/prg/cpy/pazi000d.pgl"                   .

      *    *===========================================================*
      *    * Work per subroutines di Err                               *
      *    *-----------------------------------------------------------*
       01  w-err.
      *        *-------------------------------------------------------*
      *        * Work per Err su controllo tasto Do non chiave         *
      *        *-------------------------------------------------------*
           05  w-err-box-err.
               10  w-err-box-err-msg      pic  x(56)                  .

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
      *              *  - se Altri campi corpo      : non abilitato    *
      *              *  - se Inserimento            : non abilitato    *
      *              *  - se Visualizzazione        : non abilitato    *
      *              *  - se Disabilitato manualm.  : non abilitato    *
      *              *  - se Almeno una modifica    : non abilitato    *
      *              *  - altrimenti                : abilitato        *
      *              *-------------------------------------------------*
           if        w-cnt-mfu-tip-imp    =    "K"      or
                     w-cnt-mfu-tip-imp    =    "R"      or
                     w-cnt-mfu-tip-fun    =    "I"      or
                     w-cnt-mfu-tip-fun    =    "V"      or
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
                     go to exe-acc-cmp-800.
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
       exe-acc-cmp-800.
      *              *-------------------------------------------------*
      *              * Flag globale di avvenuta almeno una modifica    *
      *              *-------------------------------------------------*
           if        w-cnt-acc-sav-mod    not  = spaces
                     move  "#"            to   w-cnt-acc-flg-aum      .
      *              *-------------------------------------------------*
      *              * Visualizzazione tipo funzionamento              *
      *              *-------------------------------------------------*
           perform   vis-tip-fun-000      thru vis-tip-fun-999        .
       exe-acc-cmp-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
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
      *              * Test se programma eseguibile                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su flag di eventuale visualizzazione   *
      *                  *---------------------------------------------*
           if        w-cnt-mfu-vis-sgr    not  = "V"
                     go to pre-exe-pgm-050.
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
       pre-exe-pgm-050.
      *              *-------------------------------------------------*
      *              * Visualizzazione titolo programma                *
      *              *-------------------------------------------------*
           perform   vis-tit-pgm-000      thru vis-tit-pgm-999        .
           move      "#"                  to   w-cnt-sts-vis-tit      .
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
      *              *-------------------------------------------------*
      *              * Altrimenti memorizzazione in campo testata      *
      *              *-------------------------------------------------*
           move      w-dpz-cod-prg        to   w-acc-cod-dpz          .
       pre-exe-pgm-200.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazioni                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Tabella [zsf] : Spese per la fatturazione e *
      *                  *                 referenze per sconti a      *
      *                  *                 piede fattura               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Open tabella [zsf]                      *
      *                      *-----------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofzsf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zsf                 .
      *                      *-----------------------------------------*
      *                      * Spese per la fatturazione               *
      *                      *-----------------------------------------*
           perform   prs-spe-fat-000      thru prs-spe-fat-999        .
       pre-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Lettura personalizzazione : Spese per la fatturazione     *
      *    *-----------------------------------------------------------*
       prs-spe-fat-000.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione : Tipo accettazione   *
      *              * spesa                                           *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/fat/mov/fat300[tac-spf]"
                                          to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Test su esito operazione                    *
      *                  *---------------------------------------------*
           if        s-ves                not  = spaces
                     move  zero           to   w-prs-spe-fat-tac
           else      move  s-num          to   w-prs-spe-fat-tac      .
      *                  *---------------------------------------------*
      *                  * Normalizzazione personalizzazione           *
      *                  *---------------------------------------------*
           if        w-prs-spe-fat-tac    not  = 00 and
                     w-prs-spe-fat-tac    not  = 01
                     move  00             to   w-prs-spe-fat-tac      .
      *              *-------------------------------------------------*
      *              * Numero di spese personalizzate caricate : zero  *
      *              *-------------------------------------------------*
           move      zero                 to   w-prs-spe-fat-nst      .
       prs-spe-fat-200.
      *              *-------------------------------------------------*
      *              * Tentativo di lettura spese da 1 a 6 con carica- *
      *              * mento in tabella delle spese trovate            *
      *              *-------------------------------------------------*
       prs-spe-fat-250.
      *                  *---------------------------------------------*
      *                  * Indice 1..6 a zero                          *
      *                  *---------------------------------------------*
           move      zero                 to   w-prs-spe-fat-i01      .
       prs-spe-fat-300.
      *                  *---------------------------------------------*
      *                  * Incremento indice 1..6                      *
      *                  *---------------------------------------------*
           add       1                    to   w-prs-spe-fat-i01      .
      *                  *---------------------------------------------*
      *                  * Se oltre il max : a chiusura                *
      *                  *---------------------------------------------*
           if        w-prs-spe-fat-i01    >    6
                     go to prs-spe-fat-900.
       prs-spe-fat-400.
      *                  *---------------------------------------------*
      *                  * Tentativo di lettura della tabella per la   *
      *                  * spesa corrispondente all'indice, e con co-  *
      *                  * dice lingua per Italia                      *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODSPF    "         to   f-key                  .
           move      w-prs-spe-fat-i01    to   rf-zsf-num-spf         .
           move      "I  "                to   rf-zsf-cod-lng         .
           move      "pgm/dcc/fls/ioc/obj/iofzsf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zsf                 .
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione dell'esito della     *
      *                  * lettura                                     *
      *                  *---------------------------------------------*
           if        f-sts                =    e-not-err
                     go to prs-spe-fat-500
           else      go to prs-spe-fat-700.
       prs-spe-fat-500.
      *                  *---------------------------------------------*
      *                  * Se record spesa esistente                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Controllo di correttezza, e se non e'   *
      *                      * superato si ricicla a spesa successiva  *
      *                      *-----------------------------------------*
           if        rf-zsf-tfu-spe       <    01 or
                     rf-zsf-tfu-spe       >    05
                     go to prs-spe-fat-800.
      *                      *-----------------------------------------*
      *                      * Bufferizzazione spesa                   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Incremento numero di spese persona- *
      *                          * lizzate caricate                    *
      *                          *-------------------------------------*
           add       1                    to   w-prs-spe-fat-nst      .
      *                          *-------------------------------------*
      *                          * Numero spesa                        *
      *                          *-------------------------------------*
           move      w-prs-spe-fat-i01    to   w-prs-spe-fat-npt
                                              (w-prs-spe-fat-nst)     .
      *                          *-------------------------------------*
      *                          * Descrizione per il video estesa     *
      *                          *-------------------------------------*
           move      rf-zsf-des-ves       to   w-prs-spe-fat-dve
                                              (w-prs-spe-fat-nst)     .
      *                          *-------------------------------------*
      *                          * Tipo funzionamento spesa            *
      *                          *-------------------------------------*
           move      rf-zsf-tfu-spe       to   w-prs-spe-fat-tfs
                                              (w-prs-spe-fat-nst)     .
      *                          *-------------------------------------*
      *                          * Percentuale per la spesa            *
      *                          *-------------------------------------*
           move      rf-zsf-per-spe       to   w-prs-spe-fat-per
                                              (w-prs-spe-fat-nst)     .
      *                          *-------------------------------------*
      *                          * Tipo di imponibile per la spesa     *
      *                          *-------------------------------------*
           move      rf-zsf-ibl-spe       to   w-prs-spe-fat-ibl
                                              (w-prs-spe-fat-nst)     .
      *                          *-------------------------------------*
      *                          * Totalizzatori imponibile spesa      *
      *                          *-------------------------------------*
           move      rf-zsf-ibt-spe       to   w-prs-spe-fat-ibt
                                              (w-prs-spe-fat-nst)     .
      *                          *-------------------------------------*
      *                          * Importo spesa                       *
      *                          *-------------------------------------*
           move      rf-zsf-imp-spe       to   w-prs-spe-fat-imp
                                              (w-prs-spe-fat-nst)     .
      *                          *-------------------------------------*
      *                          * Codice iva spesa                    *
      *                          *-------------------------------------*
           move      rf-zsf-civ-spe       to   w-prs-spe-fat-civ
                                              (w-prs-spe-fat-nst)     .
      *                          *-------------------------------------*
      *                          * Contropartita spesa                 *
      *                          *-------------------------------------*
           move      rf-zsf-ccp-spe       to   w-prs-spe-fat-ccp
                                              (w-prs-spe-fat-nst)     .
      *                      *-----------------------------------------*
      *                      * Riciclo su spesa successiva             *
      *                      *-----------------------------------------*
           go to     prs-spe-fat-800.
       prs-spe-fat-700.
      *                  *---------------------------------------------*
      *                  * Se record spesa non esistente               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Riciclo a spesa successiva              *
      *                      *-----------------------------------------*
           go to     prs-spe-fat-800.
       prs-spe-fat-800.
      *                  *---------------------------------------------*
      *                  * Riciclo a spesa successiva                  *
      *                  *---------------------------------------------*
           go to     prs-spe-fat-300.
       prs-spe-fat-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     prs-spe-fat-999.
       prs-spe-fat-999.
           exit.

      *    *===========================================================*
      *    * Routine post-esecuzione programma                         *
      *    *-----------------------------------------------------------*
       pos-exe-pgm-000.
       pos-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Open files                                                *
      *    *-----------------------------------------------------------*
       rou-opn-fls-000.
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
       rou-opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Close files                                               *
      *    *-----------------------------------------------------------*
       rou-cls-fls-000.
      *              *-------------------------------------------------*
      *              * [zsf] : chiusura file aperto in pre-esecuzione  *
      *              *         per personalizzazioni e referenze       *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofzsf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zsf                 .
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
      *                  * Tipo movimento                              *
      *                  *---------------------------------------------*
           perform   acc-cod-tmo-000      thru acc-cod-tmo-999        .
           if        w-cnt-tus-acc-key    not  = spaces
                     go to acc-key-reg-999.
       acc-key-reg-200.
      *                  *---------------------------------------------*
      *                  * Data documento                              *
      *                  *---------------------------------------------*
           perform   acc-dat-doc-000      thru acc-dat-doc-999        .
           if        w-cnt-tus-acc-key    not  = spaces
                     go to acc-key-reg-999.
           if        v-key                =    "UP  "
                     go to acc-key-reg-100.
       acc-key-reg-300.
      *                  *---------------------------------------------*
      *                  * Numero documento                            *
      *                  *---------------------------------------------*
           perform   acc-num-doc-000      thru acc-num-doc-999        .
           if        w-cnt-tus-acc-key    not  = spaces
                     go to acc-key-reg-999.
           if        v-key                =    "UP  "
                     go to acc-key-reg-200.
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
      *              * Tipo movimento                                  *
      *              *-------------------------------------------------*
           perform   vis-tip-mpb-000      thru vis-tip-mpb-999        .
      *              *-------------------------------------------------*
      *              * Data documento                                  *
      *              *-------------------------------------------------*
           perform   vis-dat-doc-000      thru vis-dat-doc-999        .
      *              *-------------------------------------------------*
      *              * Numero documento                                *
      *              *-------------------------------------------------*
           perform   vis-num-doc-000      thru vis-num-doc-999        .
       vis-key-reg-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per campi chiave                  *
      *    *-----------------------------------------------------------*
       pmt-key-reg-000.
      *              *-------------------------------------------------*
      *              * Erase linee impegnate dalla chiave              *
      *              *-------------------------------------------------*
           move      "EL"                 to   v-ope                  .
           move      04                   to   v-lin                  .
           move      06                   to   v-lto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Visualizzazioni prompts                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Tipo movimento                              *
      *                  *---------------------------------------------*
           perform   pmt-tip-mpb-000      thru pmt-tip-mpb-999        .
      *                  *---------------------------------------------*
      *                  * Data documento                              *
      *                  *---------------------------------------------*
           perform   pmt-dat-doc-000      thru pmt-dat-doc-999        .
      *                  *---------------------------------------------*
      *                  * Numero documento                            *
      *                  *---------------------------------------------*
           perform   pmt-num-doc-000      thru pmt-num-doc-999        .
      *              *-------------------------------------------------*
      *              * Linea di trattini di separazione                *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      all "-"              to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-key-reg-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per Tipo movimento                *
      *    *-----------------------------------------------------------*
       pmt-tip-mpb-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      16                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tipo movimento :"   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-tip-mpb-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per data documento                *
      *    *-----------------------------------------------------------*
       pmt-dat-doc-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      16                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      56                   to   v-pos                  .
           move      "Data documento :"   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-dat-doc-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per numero documento              *
      *    *-----------------------------------------------------------*
       pmt-num-doc-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      13                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      56                   to   v-pos                  .
           move      "Nr.         :"      to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-num-doc-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo chiave : Tipo movimento                *
      *    *-----------------------------------------------------------*
       acc-cod-tmo-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cod-tmo-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      18                   to   v-pos                  .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rf-fit-cod-tmo       to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
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
           move      v-alf                to   rf-fit-cod-tmo         .
       acc-cod-tmo-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
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
      *    * Visualizzazione campo chiave : tipo movimento per bolla   *
      *    *-----------------------------------------------------------*
       vis-tip-mpb-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      18                   to   v-pos                  .
           move      rf-fit-cod-tmo       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tip-mpb-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo chiave : Data documento                *
      *    *-----------------------------------------------------------*
       acc-dat-doc-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-dat-doc-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      ">"                  to   v-edm                  .
           move      04                   to   v-lin                  .
           move      73                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rf-fit-dat-doc       to   v-dat                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-dat-doc-999.
       acc-dat-doc-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-dat                to   rf-fit-dat-doc         .
       acc-dat-doc-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-dat-doc-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-dat-doc-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                not  = "DO  "
                     go to acc-dat-doc-999.
           perform   cnt-key-vuo-000      thru cnt-key-vuo-999        .
           if        w-cnt-key-vuo-flg    not  = spaces
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-dat-doc-999.
           perform   cnt-tdo-key-000      thru cnt-tdo-key-999        .
           if        w-cnt-tdo-key-flg    not  = spaces
                     go to acc-dat-doc-100
           else      move  "S"            to   w-cnt-tus-acc-key
                     go to acc-dat-doc-999.
       acc-dat-doc-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo chiave : Data documento             *
      *    *-----------------------------------------------------------*
       vis-dat-doc-000.
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      04                   to   v-lin                  .
           move      73                   to   v-pos                  .
           move      rf-fit-dat-doc       to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dat-doc-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo chiave : Numero documento              *
      *    *-----------------------------------------------------------*
       acc-num-doc-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-num-doc-100.
      *              *-------------------------------------------------*
      *              * Note operative                                  *
      *              *-------------------------------------------------*
           move      "NT"                 to   v-ope                  .
           move      "Formato di accettazione saa.gg.nnnnnn dove 'gg' e'
      -              "il nr. giornale iva"
                                          to   v-not                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      05                   to   v-lin                  .
           move      70                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rf-fit-num-doc       to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Cancellazione note operative                    *
      *              *-------------------------------------------------*
           move      "NT"                 to   v-ope                  .
           move      spaces               to   v-not                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-num-doc-999.
       acc-num-doc-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rf-fit-num-doc         .
       acc-num-doc-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-num-doc-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-num-doc-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                not  = "DO  "
                     go to acc-num-doc-999.
           perform   cnt-key-vuo-000      thru cnt-key-vuo-999        .
           if        w-cnt-key-vuo-flg    not  = spaces
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-num-doc-999.
           perform   cnt-tdo-key-000      thru cnt-tdo-key-999        .
           if        w-cnt-tdo-key-flg    not  = spaces
                     go to acc-num-doc-100
           else      move  "S"            to   w-cnt-tus-acc-key
                     go to acc-num-doc-999.
       acc-num-doc-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo chiave : Numero documento           *
      *    *-----------------------------------------------------------*
       vis-num-doc-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      05                   to   v-lin                  .
           move      70                   to   v-pos                  .
           move      rf-fit-num-doc       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-num-doc-999.
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
      *              *-------------------------------------------------*
      *              * Pagina 1                                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione func-key di impostazione    *
      *                  *---------------------------------------------*
           move      spaces               to   v-key                  .
      *                  *---------------------------------------------*
      *                  * Codice archivio                             *
      *                  *---------------------------------------------*
           perform   acc-cod-arc-000      thru acc-cod-arc-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
       acc-tes-reg-110.
      *                  *---------------------------------------------*
      *                  * Codice dipendenza archivio                  *
      *                  *---------------------------------------------*
           perform   acc-dpz-arc-000      thru acc-dpz-arc-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-100.
       acc-tes-reg-120.
      *                  *---------------------------------------------*
      *                  * Data di contabilita'                        *
      *                  *---------------------------------------------*
           perform   acc-dri-mgd-000      thru acc-dri-mgd-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-110.
       acc-tes-reg-130.
      *                  *---------------------------------------------*
      *                  * Numero di contabilita'                      *
      *                  *---------------------------------------------*
           perform   acc-prt-mgd-000      thru acc-prt-mgd-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-120.
       acc-tes-reg-140.
      *                  *---------------------------------------------*
      *                  * Numero di registrazioni                     *
      *                  *---------------------------------------------*
           perform   acc-nrg-mgd-000      thru acc-nrg-mgd-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-130.
       acc-tes-reg-150.
      *                  *---------------------------------------------*
      *                  * Flag di regime fiscale                      *
      *                  *---------------------------------------------*
           perform   acc-flg-rfp-000      thru acc-flg-rfp-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-140.
       acc-tes-reg-160.
      *                  *---------------------------------------------*
      *                  * Flag di elaborazione provvigioni            *
      *                  *---------------------------------------------*
           perform   acc-flg-epv-000      thru acc-flg-epv-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-150.
       acc-tes-reg-190.
      *                  *---------------------------------------------*
      *                  * Fine Pagina                                 *
      *                  *---------------------------------------------*
           go to     acc-tes-reg-999.
       acc-tes-reg-200.
      *              *-------------------------------------------------*
      *              * Pagina 2                                        *
      *              *-------------------------------------------------*
       acc-tes-reg-220.
      *                  *---------------------------------------------*
      *                  * Castelletto iva                             *
      *                  *---------------------------------------------*
           perform   acc-cst-iva-000      thru acc-cst-iva-999        .
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
           go to     acc-tes-reg-999.
       acc-tes-reg-300.
      *              *-------------------------------------------------*
      *              * Pagina 3                                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * ___ DA COMPLETARE ___                       *
      *                  *---------------------------------------------*
      
           go to     acc-tes-reg-999.
      
      
      *                  *---------------------------------------------*
      *                  * Inizializzazione contatore per tabella spese*
      *                  *---------------------------------------------*
           move      1                    to   w-acc-iep-spe-ctr      .
       acc-tes-reg-305.
      *                  *---------------------------------------------*
      *                  * Si/No addebito, importo e percentuale spesa *
      *                  *---------------------------------------------*
       acc-tes-reg-310.
      *                      *-----------------------------------------*
      *                      * Accettazione Si/No addebito             *
      *                      *-----------------------------------------*
           perform   acc-spe-snx-000      thru acc-spe-snx-999        .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
      *                      *-----------------------------------------*
      *                      * Se Up                                   *
      *                      *-----------------------------------------*
           if        v-key                not  = "UP  "
                     go to acc-tes-reg-320.
           subtract  1                    from w-acc-iep-spe-ctr      .
           if        w-acc-iep-spe-ctr    >    zero
                     go to acc-tes-reg-310.
           go to     acc-tes-reg-395.
       acc-tes-reg-320.
      *                      *-----------------------------------------*
      *                      * Se Down                                 *
      *                      *-----------------------------------------*
           if        v-key                =    "DOWN"
                     go to acc-tes-reg-340.
      *                      *-----------------------------------------*
      *                      * Accettazione importo spesa              *
      *                      *-----------------------------------------*
           perform   acc-spe-imp-000      thru acc-spe-imp-999        .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-310.
       acc-tes-reg-330.
      *                      *-----------------------------------------*
      *                      * Accettazione percentuale spesa          *
      *                      *-----------------------------------------*
           perform   acc-spe-per-000      thru acc-spe-per-999        .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-320.
       acc-tes-reg-340.
      *                      *-----------------------------------------*
      *                      * Se Down o Return                        *
      *                      *-----------------------------------------*
           add       1                    to   w-acc-iep-spe-ctr      .
           if        w-acc-iep-spe-ctr    >    w-prs-spe-fat-nst
                     go to acc-tes-reg-390
           else      go to acc-tes-reg-310.
       acc-tes-reg-390.
      *                  *---------------------------------------------*
      *                  * Inizializzazione contatore per tabella spese*
      *                  *---------------------------------------------*
           move      w-prs-spe-fat-nst    to   w-acc-iep-spe-ctr      .
       acc-tes-reg-395.
      *                  *---------------------------------------------*
      *                  * Importo spese                               *
      *                  *---------------------------------------------*
           perform   acc-spe-imp-000      thru acc-spe-imp-999        .
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
      *              * Codice archivio                                 *
      *              *-------------------------------------------------*
           perform   vis-cod-arc-000      thru vis-cod-arc-999        .
      *              *-------------------------------------------------*
      *              * Codice dipendenza archivio                      *
      *              *-------------------------------------------------*
           perform   vis-dpz-arc-000      thru vis-dpz-arc-999        .
      *              *-------------------------------------------------*
      *              * Data di contabilita'                            *
      *              *-------------------------------------------------*
           perform   vis-dri-mgd-000      thru vis-dri-mgd-999        .
      *              *-------------------------------------------------*
      *              * Numero di contabilita'                          *
      *              *-------------------------------------------------*
           perform   vis-prt-mgd-000      thru vis-prt-mgd-999        .
      *              *-------------------------------------------------*
      *              * Nr. di registrazioni                            *
      *              *-------------------------------------------------*
           perform   vis-nrg-mgd-000      thru vis-nrg-mgd-999        .
      *              *-------------------------------------------------*
      *              * Flag di regime fiscale                          *
      *              *-------------------------------------------------*
           perform   vis-flg-rfp-000      thru vis-flg-rfp-999        .
      *              *-------------------------------------------------*
      *              * Flag di elaborazione provvigioni                *
      *              *-------------------------------------------------*
           perform   vis-flg-epv-000      thru vis-flg-epv-999        .
      *              *-------------------------------------------------*
      *              * Data ultima modifica                            *
      *              *-------------------------------------------------*
           perform   vis-ide-dat-000      thru vis-ide-dat-999        .
      *              *-------------------------------------------------*
      *              * Utente ultima modifica                          *
      *              *-------------------------------------------------*
           perform   vis-ide-ute-000      thru vis-ide-ute-999        .
      *              *-------------------------------------------------*
      *              * Fase ultima modifica                            *
      *              *-------------------------------------------------*
           perform   vis-ide-fas-000      thru vis-ide-fas-999        .
      *              *-------------------------------------------------*
      *              * Protocollo                                      *
      *              *-------------------------------------------------*
           perform   vis-num-prt-000      thru vis-num-prt-999        .
      *              *-------------------------------------------------*
      *              * Totali                                          *
      *              *-------------------------------------------------*
           perform   vis-tot-rig-000      thru vis-tot-rig-999        .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     vis-tes-reg-999.
       vis-tes-reg-200.
      *              *-------------------------------------------------*
      *              * Castelletto iva                                 *
      *              *-------------------------------------------------*
           perform   vis-cst-iva-000      thru vis-cst-iva-999        .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     vis-tes-reg-999.
       vis-tes-reg-300.


      *              *-------------------------------------------------*
      *              * ___ DA COMPLETARE ___                           *
      *              *-------------------------------------------------*
           go to     vis-tes-reg-999.



      *              *-------------------------------------------------*
      *              * Si/No addebito spese                            *
      *              *-------------------------------------------------*
           perform   vis-spe-snx-000      thru vis-spe-snx-999        .
      *              *-------------------------------------------------*
      *              * Importi spese                                   *
      *              *-------------------------------------------------*
           perform   vis-spe-imp-000      thru vis-spe-imp-999        .
      *              *-------------------------------------------------*
      *              * Percentuali spese                               *
      *              *-------------------------------------------------*
           perform   vis-spe-per-000      thru vis-spe-per-999        .
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
           move      07                   to   v-lin                  .
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
      *              * Codice archivio                                 *
      *              *-------------------------------------------------*
           perform   pmt-cod-arc-000      thru pmt-cod-arc-999        .
      *              *-------------------------------------------------*
      *              * Codice dipendenza archivio                      *
      *              *-------------------------------------------------*
           perform   pmt-dpz-arc-000      thru pmt-dpz-arc-999        .
      *              *-------------------------------------------------*
      *              * Data di contabilita'                            *
      *              *-------------------------------------------------*
           perform   pmt-dri-mgd-000      thru pmt-dri-mgd-999        .
      *              *-------------------------------------------------*
      *              * Numero di contabilita'                          *
      *              *-------------------------------------------------*
           perform   pmt-prt-mgd-000      thru pmt-prt-mgd-999        .
      *              *-------------------------------------------------*
      *              * Nr. di registrazioni                            *
      *              *-------------------------------------------------*
           perform   pmt-nrg-mgd-000      thru pmt-nrg-mgd-999        .
      *              *-------------------------------------------------*
      *              * Flag di regime fiscale                          *
      *              *-------------------------------------------------*
           perform   pmt-flg-rfp-000      thru pmt-flg-rfp-999        .
      *              *-------------------------------------------------*
      *              * Flag elaborazione provvigioni                   *
      *              *-------------------------------------------------*
           perform   pmt-flg-epv-000      thru pmt-flg-epv-999        .
      *              *-------------------------------------------------*
      *              * Data ultima modifica                            *
      *              *-------------------------------------------------*
           perform   pmt-ide-dat-000      thru pmt-ide-dat-999        .
      *              *-------------------------------------------------*
      *              * Utente ultima modifica                          *
      *              *-------------------------------------------------*
           perform   pmt-ide-ute-000      thru pmt-ide-ute-999        .
      *              *-------------------------------------------------*
      *              * Fase ultima modifica                            *
      *              *-------------------------------------------------*
           perform   pmt-ide-fas-000      thru pmt-ide-fas-999        .
      *              *-------------------------------------------------*
      *              * Protocollo                                      *
      *              *-------------------------------------------------*
           perform   pmt-num-prt-000      thru pmt-num-prt-999        .
      *              *-------------------------------------------------*
      *              * Totali                                          *
      *              *-------------------------------------------------*
           perform   pmt-tot-rig-000      thru pmt-tot-rig-999        .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     pmt-tes-reg-999.
       pmt-tes-reg-200.
      *              *-------------------------------------------------*
      *              * Castelletto iva                                 *
      *              *-------------------------------------------------*
           perform   pmt-cst-iva-000      thru pmt-cst-iva-999        .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     pmt-tes-reg-999.
       pmt-tes-reg-300.
      *              *-------------------------------------------------*
      *              * Spese                                           *
      *              *-------------------------------------------------*
           perform   pmt-spe-imp-000      thru pmt-spe-imp-999        .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     pmt-tes-reg-999.
       pmt-tes-reg-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per Codice archivio               *
      *    *-----------------------------------------------------------*
       pmt-cod-arc-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice cliente             :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cod-arc-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per Codice dipendenza archivio    *
      *    *-----------------------------------------------------------*
       pmt-dpz-arc-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice dipendenza cliente  :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-dpz-arc-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per Data di contabilita'          *
      *    *-----------------------------------------------------------*
       pmt-dri-mgd-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "------------------------- Riferimenti per la prima
      -              "nota -------------------------"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Data di riferimento        :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-dri-mgd-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per Numero di contabilita'        *
      *    *-----------------------------------------------------------*
       pmt-prt-mgd-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Nr.  protocollo            :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-prt-mgd-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per Nr. di registrazioni          *
      *    *-----------------------------------------------------------*
       pmt-nrg-mgd-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Nr.  di registrazioni      :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-nrg-mgd-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per Flag di regime fiscale        *
      *    *-----------------------------------------------------------*
       pmt-flg-rfp-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      43                   to   v-pos                  .
           move      "Flag regime fiscale        :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-flg-rfp-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per Flag elaborazione provvigioni *
      *    *-----------------------------------------------------------*
       pmt-flg-epv-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "---------------------- Riferimenti per la gestione
      -              " agenti ----------------------"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Flag di elaborazione       :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-flg-epv-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per Data ultima modifica          *
      *    *-----------------------------------------------------------*
       pmt-ide-dat-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      all "-"              to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Data   ultima modifica     :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-ide-dat-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per Utente ultima modifica        *
      *    *-----------------------------------------------------------*
       pmt-ide-ute-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Utente ultima modifica     :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-ide-ute-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per Fase ultima modifica          *
      *    *-----------------------------------------------------------*
       pmt-ide-fas-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Fase   ultima modifica     :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-ide-fas-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per Protocollo                    *
      *    *-----------------------------------------------------------*
       pmt-num-prt-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      21                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Protocollo                 :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-num-prt-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per Totali                        *
      *    *-----------------------------------------------------------*
       pmt-tot-rig-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      24                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      42                   to   v-pos                  .
           move      "|Tot. 1:              6:"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      24                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      42                   to   v-pos                  .
           move      "|     2:              7:"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      24                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      42                   to   v-pos                  .
           move      "|     3:              8:"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      24                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      42                   to   v-pos                  .
           move      "|     4:              9:"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      24                   to   v-car                  .
           move      21                   to   v-lin                  .
           move      42                   to   v-pos                  .
           move      "|     5:              t:"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-tot-rig-999.
           exit.

      *    *===========================================================*
      *    * Prompt per campo : Castelletto iva                        *
      *    *-----------------------------------------------------------*
       pmt-cst-iva-000.
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
       pmt-cst-iva-100.
      *                  *---------------------------------------------*
      *                  * Se sigla valuta pari a quella base          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Fincatura                               *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "                   Codice iva     Imponibile      
      -              "   Imposta          Totale    "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Sottolineatura fincatura                *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "                   ----------   --------------   -
      -              "-------------   --------------"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Trattini per totale                     *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "                   ----------   --------------   -
      -              "-------------   --------------"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Literal per totali                      *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      22                   to   v-pos                  .
           move      "Totali :"           to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     pmt-cst-iva-999.
       pmt-cst-iva-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per importi spese                 *
      *    *-----------------------------------------------------------*
       pmt-spe-imp-000.
      *              *-------------------------------------------------*
      *              * Se nessuna personalizzazione : uscita           *
      *              *-------------------------------------------------*
           if        w-prs-spe-fat-nst    =    zero
                     go to pmt-spe-imp-999.
      *              *-------------------------------------------------*
      *              * Ciclo di scansione su tabella spese             *
      *              *-------------------------------------------------*
           move      zero                 to   w-pmt-iep-spe-ctr      .
       pmt-spe-imp-100.
           add       1                    to   w-pmt-iep-spe-ctr      .
           if        w-pmt-iep-spe-ctr    >    w-prs-spe-fat-nst
                     go to pmt-spe-imp-999.
      *                  *---------------------------------------------*
      *                  * Visualizzazione                             *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      25                   to   v-car                  .
           add       09
                     w-pmt-iep-spe-ctr  giving v-lin                  .
           move      01                   to   v-pos                  .
           move      w-prs-spe-fat-dve
                    (w-pmt-iep-spe-ctr)   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      01                   to   v-car                  .
           add       09
                     w-pmt-iep-spe-ctr  giving v-lin                  .
           move      26                   to   v-pos                  .
           move      ":"                  to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     pmt-spe-imp-100.
       pmt-spe-imp-999.
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
      *    * Accettazione campo : Codice archivio                      *
      *    *-----------------------------------------------------------*
       acc-cod-arc-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
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
           move      07                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-acc-cod-arc        to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-cod-arc-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-cod-arc-999.
       acc-cod-arc-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-acc-cod-arc          .
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
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-cod-arc-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
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
           move      07                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-acc-cod-arc        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-arc-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Codice dipendenza archivio   *
      *    *-----------------------------------------------------------*
       acc-dpz-arc-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-dpz-arc-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-acc-dpz-arc        to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-dpz-arc-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-dpz-arc-999.
       acc-dpz-arc-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-acc-dpz-arc          .
       acc-dpz-arc-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-dpz-arc-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-dpz-arc-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-dpz-arc-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-dpz-arc-100.
       acc-dpz-arc-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Codice dipendenza archivio        *
      *    *-----------------------------------------------------------*
       vis-dpz-arc-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-acc-dpz-arc        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dpz-arc-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Data di contabilita'         *
      *    *-----------------------------------------------------------*
       acc-dri-mgd-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-dri-mgd-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      ">"                  to   v-edm                  .
           move      11                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rf-fit-dri-mgd       to   v-dat                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-dri-mgd-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-dri-mgd-999.
       acc-dri-mgd-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-dat                to   rf-fit-dri-mgd         .
       acc-dri-mgd-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-dri-mgd-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-dri-mgd-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-dri-mgd-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-dri-mgd-100.
       acc-dri-mgd-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Data di contabilita'      *
      *    *-----------------------------------------------------------*
       vis-dri-mgd-000.
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      11                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rf-fit-dri-mgd       to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dri-mgd-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Numero di contabilita'       *
      *    *-----------------------------------------------------------*
       acc-prt-mgd-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-prt-mgd-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      12                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rf-fit-prt-mgd       to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-prt-mgd-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-prt-mgd-999.
       acc-prt-mgd-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rf-fit-prt-mgd         .
       acc-prt-mgd-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-prt-mgd-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-prt-mgd-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-prt-mgd-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-prt-mgd-100.
       acc-prt-mgd-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Numero di contabilita'    *
      *    *-----------------------------------------------------------*
       vis-prt-mgd-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      12                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rf-fit-prt-mgd       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-prt-mgd-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Numero di registrazioni      *
      *    *-----------------------------------------------------------*
       acc-nrg-mgd-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-nrg-mgd-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      13                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rf-fit-nrg-mgd       to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-nrg-mgd-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-nrg-mgd-999.
       acc-nrg-mgd-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rf-fit-nrg-mgd         .
       acc-nrg-mgd-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-nrg-mgd-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-nrg-mgd-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-nrg-mgd-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-nrg-mgd-100.
       acc-nrg-mgd-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Numero di registrazioni   *
      *    *-----------------------------------------------------------*
       vis-nrg-mgd-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      13                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rf-fit-nrg-mgd       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-nrg-mgd-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Flag di regime fiscale               *
      *    *-----------------------------------------------------------*
       acc-flg-rfp-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-flg-rfp-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      72                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rf-fit-flg-rfp       to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-flg-rfp-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-flg-rfp-999.
       acc-flg-rfp-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   rf-fit-flg-rfp         .
       acc-flg-rfp-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-flg-rfp-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-flg-rfp-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-flg-rfp-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-flg-rfp-100.
       acc-flg-rfp-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Flag di regime fiscale            *
      *    *-----------------------------------------------------------*
       vis-flg-rfp-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      72                   to   v-pos                  .
           move      rf-fit-flg-rfp       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-flg-rfp-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Flag di elaborazione provvigioni     *
      *    *-----------------------------------------------------------*
       acc-flg-epv-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-flg-epv-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rf-fit-flg-blx (2)   to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-flg-epv-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-flg-epv-999.
       acc-flg-epv-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   rf-fit-flg-blx (2)     .
       acc-flg-epv-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-flg-epv-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-flg-epv-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-flg-epv-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-flg-epv-100.
       acc-flg-epv-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Flag di elaborazione provvigioni  *
      *    *-----------------------------------------------------------*
       vis-flg-epv-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rf-fit-flg-blx (2)   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-flg-epv-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Data ultima modifica              *
      *    *-----------------------------------------------------------*
       vis-ide-dat-000.
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      17                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rf-fit-ide-dat       to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-ide-dat-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Utente ultima modifica            *
      *    *-----------------------------------------------------------*
       vis-ide-ute-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rf-fit-ide-ute       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-ide-ute-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Fase ultima modifica              *
      *    *-----------------------------------------------------------*
       vis-ide-fas-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rf-fit-ide-fas       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-ide-fas-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Protocollo                        *
      *    *-----------------------------------------------------------*
       vis-num-prt-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      09                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "M"                  to   v-edm                  .
           move      "XXX.XXXXXX"         to   v-msk                  .
           move      21                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rf-fit-num-prt       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-num-prt-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Totali                            *
      *    *-----------------------------------------------------------*
       vis-tot-rig-000.
           move      "DS"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      17                   to   v-lin                  .
           move      51                   to   v-pos                  .
           move      rf-fit-tot-rig (1)   to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      18                   to   v-lin                  .
           move      51                   to   v-pos                  .
           move      rf-fit-tot-rig (2)   to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      19                   to   v-lin                  .
           move      51                   to   v-pos                  .
           move      rf-fit-tot-rig (3)   to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      20                   to   v-lin                  .
           move      51                   to   v-pos                  .
           move      rf-fit-tot-rig (4)   to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      21                   to   v-lin                  .
           move      51                   to   v-pos                  .
           move      rf-fit-tot-rig (5)   to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      17                   to   v-lin                  .
           move      67                   to   v-pos                  .
           move      rf-fit-tot-rig (6)   to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      18                   to   v-lin                  .
           move      67                   to   v-pos                  .
           move      rf-fit-tot-rig (7)   to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      19                   to   v-lin                  .
           move      67                   to   v-pos                  .
           move      rf-fit-tot-rig (8)   to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      20                   to   v-lin                  .
           move      67                   to   v-pos                  .
           move      rf-fit-tot-rig (9)   to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      21                   to   v-lin                  .
           move      67                   to   v-pos                  .
           move      rf-fit-iva-tdo       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tot-rig-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Castelletto iva                      *
      *    *-----------------------------------------------------------*
       acc-cst-iva-000.
       acc-cst-iva-010.
      *              *-------------------------------------------------*
      *              * Inizializzazione contatore 1..6                 *
      *              *-------------------------------------------------*
           move      1                    to   w-cst-ctr-rig          .
       acc-cst-iva-020.
      *              *-------------------------------------------------*
      *              * Codice iva                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      w-acc-iva-cod 
                    (w-cst-ctr-rig)       to   w-sav-iva-cod          .
       acc-cst-iva-040.
      *                  *---------------------------------------------*
      *                  * Accettazione                                *
      *                  *---------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "B"                  to   v-edm                  .
           add       12
                     w-cst-ctr-rig      giving v-lin                  .
           move      22                   to   v-pos                  .
      *                      *-----------------------------------------*
      *                      * Tasti funzione                          *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Determinazione numero riga + 1      *
      *                          *-------------------------------------*
           add       1
                     w-cst-ctr-rig      giving w-cst-cts-rig          .
      *                          *-------------------------------------*
      *                          * Up   : sempre ammesso               *
      *                          *-------------------------------------*
           move      "UP  "               to   v-pfk (01)             .
      *                          *-------------------------------------*
      *                          * Down : sempre ammesso               *
      *                          *-------------------------------------*
           move      "DOWN"               to   v-pfk (02)             .
      *                          *-------------------------------------*
      *                          * Find : sempre ammesso               *
      *                          *-------------------------------------*
           move      "FIND"               to   v-pfk (03)             .
      *                          *-------------------------------------*
      *                          * Insr : sempre ammesso, a meno che   *
      *                          *        non si sia in una riga new   *
      *                          *        oppure manchi spazio per     *
      *                          *        l'inserimento                *
      *                          *-------------------------------------*
           if        w-acc-iva-cod (6)    not  = zero
                     go to acc-cst-iva-060.
           if        w-cst-ctr-rig        =    6
                     go to acc-cst-iva-060.
           if        w-sav-iva-cod        =    zero
                     go to acc-cst-iva-060.
           move      "INSR"               to   v-pfk (04)             .
       acc-cst-iva-060.
      *                          *-------------------------------------*
      *                          * Do   : sempre ammesso, purche' sia  *
      *                          *        concesso dallo status delle  *
      *                          *        impostazioni                 *
      *                          *-------------------------------------*
           if        w-cnt-sts-imp-tes    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
      *                          *-------------------------------------*
      *                          * Remv : sempre ammesso               *
      *                          *-------------------------------------*
           move      "REMV"               to   v-pfk (06)             .
      *                          *-------------------------------------*
      *                          * Back : sempre ammesso, a meno che   *
      *                          *        si sia gia' sulla prima riga *
      *                          *-------------------------------------*
           if        w-cst-ctr-rig        not  = 1
                     move  "BACK"         to   v-pfk (09)             .
      *                          *-------------------------------------*
      *                          * Tab  : sempre ammesso               *
      *                          *-------------------------------------*
           move      "TAB "               to   v-pfk (10)             .
      *                      *-----------------------------------------*
      *                      * Valore di accettazione                  *
      *                      *-----------------------------------------*
           move      w-acc-iva-cod 
                    (w-cst-ctr-rig)       to   v-num                  .
      *                      *-----------------------------------------*
      *                      * Richiamo subroutine di accettazione     *
      *                      *-----------------------------------------*
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *                  *---------------------------------------------*
      *                  * Se Return                                   *
      *                  *---------------------------------------------*
           if        v-key                =    spaces
                     go to acc-cst-iva-300.
      *                  *---------------------------------------------*
      *                  * Se Exit                                     *
      *                  *---------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-cst-iva-999.
      *                  *---------------------------------------------*
      *                  * Se Delt                                     *
      *                  *---------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-cst-iva-999.
      *                  *---------------------------------------------*
      *                  * Se premuto un altro tasto funzione non deve *
      *                  * essere avvenuta variazione del campo        *
      *                  *---------------------------------------------*
           if        v-num                not  = w-sav-iva-cod
                     go to acc-cst-iva-020.
      *                  *---------------------------------------------*
      *                  * Se Up                                       *
      *                  *---------------------------------------------*
           if        v-key                not  = "UP  "
                     go to acc-cst-iva-080.
      *                      *-----------------------------------------*
      *                      * Compattamento controllato               *
      *                      *-----------------------------------------*
           perform   acc-cst-iva-800      thru acc-cst-iva-809        .
      *                      *-----------------------------------------*
      *                      * Se su riga > 1 : a riga precedente      *
      *                      *-----------------------------------------*
           if        w-cst-ctr-rig        >    1
                     subtract  1          from w-cst-ctr-rig
                     go to     acc-cst-iva-020.
      *                      *-----------------------------------------*
      *                      * Se su riga 1 : uscita, a meno che non   *
      *                      * si sia in variazione e il castelletto   *
      *                      * non sia a posto                         *
      *                      *-----------------------------------------*
           if        w-cnt-sts-imp-tes    =    spaces
                     move  "UP  "         to   v-key
                     go to acc-cst-iva-999.
           perform   acc-cst-iva-980      thru acc-cst-iva-989        .
           if        w-cst-flg-exi        =    spaces
                     move  "UP  "         to   v-key
                     go to acc-cst-iva-999.
           go to     acc-cst-iva-040.
       acc-cst-iva-080.
      *                  *---------------------------------------------*
      *                  * Se Down                                     *
      *                  *---------------------------------------------*
           if        v-key                not  = "DOWN"
                     go to acc-cst-iva-140.
       acc-cst-iva-100.
      *                      *-----------------------------------------*
      *                      * Compattamento controllato               *
      *                      *-----------------------------------------*
           perform   acc-cst-iva-800      thru acc-cst-iva-809        .
      *                      *-----------------------------------------*
      *                      * Se il codice iva precedentemente salva- *
      *                      * to era a zero                           *
      *                      *-----------------------------------------*
           if        w-sav-iva-cod        not  = zero
                     go to acc-cst-iva-120.
      *                          *-------------------------------------*
      *                          * Se anche dopo il compattamento ci   *
      *                          * si trova su di un codice iva a ze-  *
      *                          * ro : a impostazione totale docum.   *
      *                          *-------------------------------------*
           if        w-acc-iva-cod
                    (w-cst-ctr-rig)       =    zero
                     go to acc-cst-iva-600.
      *                          *-------------------------------------*
      *                          * Altrimenti si ricicla sulla stessa  *
      *                          * riga, in pratica sulla successiva   *
      *                          *-------------------------------------*
           go to     acc-cst-iva-020.
       acc-cst-iva-120.
      *                      *-----------------------------------------*
      *                      * Se il codice iva precedentemente salva- *
      *                      * to era diverso da zero                  *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se si e' all'ultima riga : a impo-  *
      *                          * stazione totale documento           *
      *                          *-------------------------------------*
           if        w-cst-ctr-rig        =    6
                     go to acc-cst-iva-600.
      *                      *-----------------------------------------*
      *                      * Altrimenti : a riga successiva          *
      *                      *-----------------------------------------*
           add       1                    to   w-cst-ctr-rig          .
           go to     acc-cst-iva-020.
       acc-cst-iva-140.
      *                  *---------------------------------------------*
      *                  * Se Find                                     *
      *                  *---------------------------------------------*
           if        v-key                not  = "FIND"
                     go to acc-cst-iva-160.
      *                      *-----------------------------------------*
      *                      * Find su archivio                        *
      *                      *-----------------------------------------*
           perform   fnd-cod-iva-000      thru fnd-cod-iva-999        .
           if        w-fnd-cod-iva-sel    not  = spaces
                     go to acc-cst-iva-020.
           move      w-fnd-cod-iva-cod    to   w-acc-iva-cod
                                              (w-cst-ctr-rig)         .
      *                      *-----------------------------------------*
      *                      * Visualizza codice selezionato           *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "B"                  to   v-edm                  .
           add       12
                     w-cst-ctr-rig      giving v-lin                  .
           move      22                   to   v-pos                  .
           move      w-acc-iva-cod
                    (w-cst-ctr-rig)       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      spaces               to   v-key                  .
           go to     acc-cst-iva-300.
       acc-cst-iva-160.
      *                  *---------------------------------------------*
      *                  * Se Insr                                     *
      *                  *---------------------------------------------*
           if        v-key                not  = "INSR"
                     go to acc-cst-iva-180.
      *                      *-----------------------------------------*
      *                      * Inserimento riga in castelletto         *
      *                      *-----------------------------------------*
           perform   acc-cst-iva-820      thru acc-cst-iva-829        .
      *                      *-----------------------------------------*
      *                      * Riciclo sulla stessa riga               *
      *                      *-----------------------------------------*
           go to     acc-cst-iva-020.
       acc-cst-iva-180.
      *                  *---------------------------------------------*
      *                  * Se Do                                       *
      *                  *---------------------------------------------*
           if        v-key                not  = "DO  "
                     go to acc-cst-iva-200.
      *                      *-----------------------------------------*
      *                      * Compattamento controllato               *
      *                      *-----------------------------------------*
           perform   acc-cst-iva-800      thru acc-cst-iva-809        .
      *                      *-----------------------------------------*
      *                      * Controllo castelletto iva               *
      *                      *-----------------------------------------*
           perform   acc-cst-iva-980      thru acc-cst-iva-989        .
           if        w-cst-flg-exi        not  = spaces
                     go to acc-cst-iva-040.
      *                      *-----------------------------------------*
      *                      * Controllo globali                       *
      *                      *-----------------------------------------*
           perform   cnt-tdo-nok-000      thru cnt-tdo-nok-999        .
      *                      *-----------------------------------------*
      *                      * Test su esito controllo                 *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se positivo : uscita                *
      *                          *-------------------------------------*
           if        w-cnt-tdo-nok-flg    =    spaces
                     move  "S"            to   w-cnt-tus-acc-tes
                     go to acc-cst-iva-999.
      *                          *-------------------------------------*
      *                          * Altrimenti si torna alla reimposta- *
      *                          * zione della stessa riga             *
      *                          *-------------------------------------*
           go to     acc-cst-iva-040.
       acc-cst-iva-200.
      *                  *---------------------------------------------*
      *                  * Se Remv                                     *
      *                  *---------------------------------------------*
           if        v-key                not  = "REMV"
                     go to acc-cst-iva-220.
       acc-cst-iva-210.
      *                      *-----------------------------------------*
      *                      * Compattamento non controllato           *
      *                      *-----------------------------------------*
           perform   acc-cst-iva-810      thru acc-cst-iva-819        .
      *                      *-----------------------------------------*
      *                      * Reimpostazione della stessa riga        *
      *                      *-----------------------------------------*
           go to     acc-cst-iva-020.
       acc-cst-iva-220.
      *                  *---------------------------------------------*
      *                  * Se Nxsc                                     *
      *                  *---------------------------------------------*
           if        v-key                not  = "NXSC"
                     go to acc-cst-iva-230.
      *                      *-----------------------------------------*
      *                      * Compattamento controllato               *
      *                      *-----------------------------------------*
           perform   acc-cst-iva-800      thru acc-cst-iva-809        .
      *                      *-----------------------------------------*
      *                      * Controllo castelletto iva               *
      *                      *-----------------------------------------*
           perform   acc-cst-iva-980      thru acc-cst-iva-989        .
           if        w-cst-flg-exi        not  = spaces
                     go to acc-cst-iva-040.
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           move      "+"                  to   w-cnt-tus-acc-tes      .
           go to     acc-cst-iva-999.
       acc-cst-iva-230.
      *                  *---------------------------------------------*
      *                  * Se Prsc                                     *
      *                  *---------------------------------------------*
           if        v-key                not  = "PRSC"
                     go to acc-cst-iva-240.
      *                      *-----------------------------------------*
      *                      * Compattamento controllato               *
      *                      *-----------------------------------------*
           perform   acc-cst-iva-800      thru acc-cst-iva-809        .
      *                      *-----------------------------------------*
      *                      * Controllo castelletto iva               *
      *                      *-----------------------------------------*
           perform   acc-cst-iva-980      thru acc-cst-iva-989        .
           if        w-cst-flg-exi        not  = spaces
                     go to acc-cst-iva-040.
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           move      "-"                  to   w-cnt-tus-acc-tes      .
           go to     acc-cst-iva-999.
       acc-cst-iva-240.
      *                  *---------------------------------------------*
      *                  * Se Back                                     *
      *                  *---------------------------------------------*
           if        v-key                not  = "BACK"
                     go to acc-cst-iva-260.
      *                      *-----------------------------------------*
      *                      * Compattamento controllato               *
      *                      *-----------------------------------------*
           perform   acc-cst-iva-800      thru acc-cst-iva-809        .
      *                      *-----------------------------------------*
      *                      * Ad impostazione della prima riga        *
      *                      *-----------------------------------------*
           move      1                    to   w-cst-ctr-rig          .
           go to     acc-cst-iva-020.
       acc-cst-iva-260.
      *                  *---------------------------------------------*
      *                  * Se Tab                                      *
      *                  *---------------------------------------------*
           if        v-key                not  = "TAB "
                     go to acc-cst-iva-340.
      *                      *-----------------------------------------*
      *                      * Compattamento controllato               *
      *                      *-----------------------------------------*
           perform   acc-cst-iva-800      thru acc-cst-iva-809        .
      *                      *-----------------------------------------*
      *                      * Se il codice iva precedentemente salva- *
      *                      * to era a zero                           *
      *                      *-----------------------------------------*
           if        w-sav-iva-cod        not  = zero
                     go to acc-cst-iva-280.
      *                          *-------------------------------------*
      *                          * Se anche dopo il compattamento ci   *
      *                          * si trova su di un codice iva a ze-  *
      *                          * ro : a impostazione totale docum.   *
      *                          *-------------------------------------*
           if        w-acc-iva-cod
                    (w-cst-ctr-rig)       =    zero
                     go to acc-cst-iva-600.
       acc-cst-iva-280.
      *                      *-----------------------------------------*
      *                      * Se il codice iva precedentemente salva- *
      *                      * to era diverso da zero ci si posiziona  *
      *                      * dopo l'ultima riga                      *
      *                      *-----------------------------------------*
           add       1                    to   w-cst-ctr-rig          .
      *                          *-------------------------------------*
      *                          * Se sono presenti tutte le righe si  *
      *                          * va' al totale documento             *
      *                          *-------------------------------------*
           if        w-cst-ctr-rig        >    6
                     go to acc-cst-iva-600.
      *                          *-------------------------------------*
      *                          * Se la riga e' vuota si va ad impo-  *
      *                          * starla                              *
      *                          *-------------------------------------*
           if        w-acc-iva-cod
                    (w-cst-ctr-rig)       =    zero
                     go to acc-cst-iva-020.
      *                          *-------------------------------------*
      *                          * Altrimenti si ricicla per trovare   *
      *                          * una riga vuota                      *
      *                          *-------------------------------------*
           go to     acc-cst-iva-280.
       acc-cst-iva-300.
      *                  *---------------------------------------------*
      *                  * Se Return                                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Memorizzazione valore impostato         *
      *                      *-----------------------------------------*
           move      v-num                to   w-acc-iva-cod
                                              (w-cst-ctr-rig)         .
      *                      *-----------------------------------------*
      *                      * Se campo vuoto : come Down              *
      *                      *-----------------------------------------*
           if        v-num                =    zero
                     go to acc-cst-iva-100.
      *                  *---------------------------------------------*
      *                  * Controllo impostazione                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Controllo codice iva                    *
      *                      *-----------------------------------------*
       acc-cst-iva-320.
      *                      *-----------------------------------------*
      *                      * Deviazione a seconda del tipo di codice *
      *                      * iva impostato                           *
      *                      *-----------------------------------------*
           if        w-acc-iva-cod
                    (w-cst-ctr-rig)       <    100
                     go to acc-cst-iva-340.
           if        w-acc-iva-cod
                    (w-cst-ctr-rig)       >    300 and
                     w-acc-iva-cod
                    (w-cst-ctr-rig)       <    700
                     go to acc-cst-iva-460.
           if        w-acc-iva-cod
                    (w-cst-ctr-rig)       >    700 and
                     w-acc-iva-cod
                    (w-cst-ctr-rig)       <    800
                     go to acc-cst-iva-340.
           if        w-acc-iva-cod
                    (w-cst-ctr-rig)       >    900
                     go to acc-cst-iva-520.
       acc-cst-iva-340.
      *                      *-----------------------------------------*
      *                      * Se codice iva 001..099                  *
      *                      *               701..799                  *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Accettazione imponibile in valuta   *
      *                          *-------------------------------------*
       acc-cst-iva-350.
      *                          *-------------------------------------*
      *                          * Accettazione imponibile in valuta   *
      *                          * base                                *
      *                          *-------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "GB"                 to   v-edm                  .
           add       12
                     w-cst-ctr-rig      giving v-lin                  .
           move      32                   to   v-pos                  .
      *                              *---------------------------------*
      *                              * Tasti funzione                  *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Determinazione nr riga + 1  *
      *                                  *-----------------------------*
           add       1
                     w-cst-ctr-rig      giving w-cst-cts-rig          .
      *                                  *-----------------------------*
      *                                  * Up   : Ammesso solo se ac-  *
      *                                  *        cettazione castel-   *
      *                                  *        letto in valuta      *
      *                                  *-----------------------------*
      *                                  *-----------------------------*
      *                                  * Down : sempre ammesso       *
      *                                  *-----------------------------*
           move      "DOWN"               to   v-pfk (02)             .
      *                                  *-----------------------------*
      *                                  * Remv : sempre ammesso       *
      *                                  *-----------------------------*
           move      "REMV"               to   v-pfk (06)             .
      *                                  *-----------------------------*
      *                                  * Pf2 : sempre ammesso        *
      *                                  *-----------------------------*
           move      "[2] "               to   v-pfk (15)             .
      *                                  *-----------------------------*
      *                                  * Valore di accettazione      *
      *                                  *-----------------------------*
           move      w-acc-iva-ibl 
                    (w-cst-ctr-rig)       to   v-num                  .
      *                                  *-----------------------------*
      *                                  * Richiamo sub. accettazione  *
      *                                  *-----------------------------*
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *                          *-------------------------------------*
      *                          * Se Return                           *
      *                          *-------------------------------------*
           if        v-key                =    spaces
                     go to acc-cst-iva-370.
      *                          *-------------------------------------*
      *                          * Se Exit o Remv                      *
      *                          *-------------------------------------*
           if        v-key                =    "EXIT" or
                     v-key                =    "REMV"
                     go to acc-cst-iva-210.
      *                          *-------------------------------------*
      *                          * Se Delt                             *
      *                          *-------------------------------------*
           if        v-key                =    "DELT"
                     go to acc-cst-iva-350.
      *                          *-------------------------------------*
      *                          * Se Next Screen                      *
      *                          *-------------------------------------*
           if        v-key                =    "NXSC"
                     go to acc-cst-iva-350.
      *                          *-------------------------------------*
      *                          * Se Previous Screen                  *
      *                          *-------------------------------------*
           if        v-key                =    "PRSC"
                     go to acc-cst-iva-350.
      *                          *-------------------------------------*
      *                          * Se Up                               *
      *                          *-------------------------------------*
           if        v-key                =    "UP  "
                     go to acc-cst-iva-340.
       acc-cst-iva-360.
      *                          *-------------------------------------*
      *                          * Se Pf2                              *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Test                            *
      *                              *---------------------------------*
           if        v-key                not  = "[2] "
                     go to acc-cst-iva-370.
      *                              *---------------------------------*
      *                              * Valore impostato in campo di    *
      *                              * destinazione                    *
      *                              *---------------------------------*
           move      v-num                to   w-cst-tot-rig          .
      *                              *---------------------------------*
      *                              * Totale riga a zero non ammesso  *
      *                              *---------------------------------*
           if        w-cst-tot-rig        =    zero
                     go to acc-cst-iva-350.
      *                              *---------------------------------*
      *                              * Scorporo imponibile ed imposta  *
      *                              *---------------------------------*
           perform   acc-cst-iva-930      thru acc-cst-iva-939        .
      *                              *---------------------------------*
      *                              * Visualizzazione imponibile      *
      *                              *---------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "GB"                 to   v-edm                  .
           add       12
                     w-cst-ctr-rig      giving v-lin                  .
           move      32                   to   v-pos                  .
           move      w-acc-iva-ibl 
                    (w-cst-ctr-rig)       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                              *---------------------------------*
      *                              * Visualizzazione imposta         *
      *                              *---------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "GB"                 to   v-edm                  .
           add       12
                     w-cst-ctr-rig      giving v-lin                  .
           move      49                   to   v-pos                  .
           move      w-acc-iva-imp 
                    (w-cst-ctr-rig)       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                              *---------------------------------*
      *                              * Visualizzazione totale riga     *
      *                              *---------------------------------*
           perform   acc-cst-iva-840      thru acc-cst-iva-849        .
      *                              *---------------------------------*
      *                              * Determinazione totali castell.  *
      *                              *---------------------------------*
           perform   acc-cst-iva-830      thru acc-cst-iva-839        .
      *                              *---------------------------------*
      *                              * Visualizzazione tot. castell.   *
      *                              *---------------------------------*
           perform   acc-cst-iva-860      thru acc-cst-iva-869        .
      *                              *---------------------------------*
      *                              * Imponibile scorporato a zero o  *
      *                              * imposta scorporata a zero non   *
      *                              * ammessi                         *
      *                              *---------------------------------*
           if        w-acc-iva-ibl
                    (w-cst-ctr-rig)       =    zero or
                     w-acc-iva-imp
                    (w-cst-ctr-rig)       =    zero
                     go to acc-cst-iva-350.
      *                              *---------------------------------*
      *                              * Ad incremento numero riga       *
      *                              *---------------------------------*
           go to     acc-cst-iva-580.
       acc-cst-iva-370.
      *                          *-------------------------------------*
      *                          * Se Return o Down                    *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Valore impostato in campo di    *
      *                              * destinazione                    *
      *                              *---------------------------------*
           move      v-num                to   w-acc-iva-ibl
                                              (w-cst-ctr-rig)         .
      *                              *---------------------------------*
      *                              * Imponibile a zero non ammesso   *
      *                              *---------------------------------*
           if        w-acc-iva-ibl
                    (w-cst-ctr-rig)       =    zero
                     go to acc-cst-iva-350.
      *                              *---------------------------------*
      *                              * Visualizzazione totale riga     *
      *                              *---------------------------------*
           perform   acc-cst-iva-840      thru acc-cst-iva-849        .
      *                              *---------------------------------*
      *                              * Determinazione totali castell.  *
      *                              *---------------------------------*
           perform   acc-cst-iva-830      thru acc-cst-iva-839        .
      *                              *---------------------------------*
      *                              * Visualizzazione tot. imponibile *
      *                              *---------------------------------*
           perform   acc-cst-iva-870      thru acc-cst-iva-879        .
      *                              *---------------------------------*
      *                              * Visualizzazione tot. documento  *
      *                              *---------------------------------*
           perform   acc-cst-iva-890      thru acc-cst-iva-899        .
      *                              *---------------------------------*
      *                              * Preparazione default imposta    *
      *                              *---------------------------------*
           perform   acc-cst-iva-910      thru acc-cst-iva-919        .
       acc-cst-iva-380.
      *                          *-------------------------------------*
      *                          * Accettazione imposta                *
      *                          *-------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "GB"                 to   v-edm                  .
           add       12
                     w-cst-ctr-rig      giving v-lin                  .
           move      49                   to   v-pos                  .
      *                              *---------------------------------*
      *                              * Tasti funzione                  *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Determinazione nr riga + 1  *
      *                                  *-----------------------------*
           add       1
                     w-cst-ctr-rig      giving w-cst-cts-rig          .
      *                                  *-----------------------------*
      *                                  * Up   : sempre ammesso       *
      *                                  *-----------------------------*
           move      "UP  "               to   v-pfk (01)             .
      *                                  *-----------------------------*
      *                                  * Down : sempre ammesso       *
      *                                  *-----------------------------*
           move      "DOWN"               to   v-pfk (02)             .
      *                                  *-----------------------------*
      *                                  * Remv : sempre ammesso       *
      *                                  *-----------------------------*
           move      "REMV"               to   v-pfk (06)             .
      *                                  *-----------------------------*
      *                                  * Valore di accettazione      *
      *                                  *-----------------------------*
           move      w-acc-iva-imp 
                    (w-cst-ctr-rig)       to   v-num                  .
      *                                  *-----------------------------*
      *                                  * Richiamo sub. accettazione  *
      *                                  *-----------------------------*
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *                          *-------------------------------------*
      *                          * Se Exit o Remv                      *
      *                          *-------------------------------------*
           if        v-key                =    "EXIT" or
                     v-key                =    "REMV"
                     go to acc-cst-iva-210.
      *                          *-------------------------------------*
      *                          * Se Delt                             *
      *                          *-------------------------------------*
           if        v-key                =    "DELT"
                     go to acc-cst-iva-380.
      *                          *-------------------------------------*
      *                          * Se Next Screen                      *
      *                          *-------------------------------------*
           if        v-key                =    "NXSC"
                     go to acc-cst-iva-380.
      *                          *-------------------------------------*
      *                          * Se Previous Screen                  *
      *                          *-------------------------------------*
           if        v-key                =    "PRSC"
                     go to acc-cst-iva-380.
      *                          *-------------------------------------*
      *                          * Se Up                               *
      *                          *-------------------------------------*
           if        v-key                =    "UP  "
                     go to acc-cst-iva-340.
      *                          *-------------------------------------*
      *                          * Se Return o Down                    *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Valore impostato in campo di    *
      *                              * destinazione                    *
      *                              *---------------------------------*
           move      v-num                to   w-acc-iva-imp
                                              (w-cst-ctr-rig)         .
      *                              *---------------------------------*
      *                              * Imposta a zero non ammessa      *
      *                              *---------------------------------*
           if        w-acc-iva-imp
                    (w-cst-ctr-rig)       =    zero
                     go to acc-cst-iva-380.
      *                              *---------------------------------*
      *                              * Visualizzazione totale riga     *
      *                              *---------------------------------*
           perform   acc-cst-iva-840      thru acc-cst-iva-849        .
      *                              *---------------------------------*
      *                              * Determinazione totali castell.  *
      *                              *---------------------------------*
           perform   acc-cst-iva-830      thru acc-cst-iva-839        .
      *                              *---------------------------------*
      *                              * Visualizzazione tot. imposta    *
      *                              *---------------------------------*
           perform   acc-cst-iva-880      thru acc-cst-iva-889        .
      *                              *---------------------------------*
      *                              * Visualizzazione tot. documento  *
      *                              *---------------------------------*
           perform   acc-cst-iva-890      thru acc-cst-iva-899        .
      *                              *---------------------------------*
      *                              * Ad incremento numero riga       *
      *                              *---------------------------------*
           go to     acc-cst-iva-580.
       acc-cst-iva-460.
      *                      *-----------------------------------------*
      *                      * Se codice iva 301..399                  *
      *                      *               401..499                  *
      *                      *               501..599                  *
      *                      *               601..699                  *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Salvataggio valore imposta          *
      *                          *-------------------------------------*
           move      w-acc-iva-imp
                    (w-cst-ctr-rig)       to   w-sav-iva-imp          .
      *                          *-------------------------------------*
      *                          * Azzeramento imposta                 *
      *                          *-------------------------------------*
           move      zero                 to   w-acc-iva-imp
                                              (w-cst-ctr-rig)         .
      *                          *-------------------------------------*
      *                          * Visualizzazione descrizione titolo  *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      15                   to   v-car                  .
           add       12
                     w-cst-ctr-rig      giving v-lin                  .
           move      49                   to   v-pos                  .
           move      w-cst-des-iva        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                          *-------------------------------------*
      *                          * Se imposta precedentemente diversa  *
      *                          * da zero : ricalcolo e rivisualizza- *
      *                          * zione castelletto                   *
      *                          *-------------------------------------*
           if        w-sav-iva-imp        =    zero
                     go to acc-cst-iva-480.
      *                              *---------------------------------*
      *                              * Visualizzazione totale riga     *
      *                              *---------------------------------*
           perform   acc-cst-iva-840      thru acc-cst-iva-849        .
      *                              *---------------------------------*
      *                              * Determinazione totali castell.  *
      *                              *---------------------------------*
           perform   acc-cst-iva-830      thru acc-cst-iva-839        .
      *                              *---------------------------------*
      *                              * Visualizzazione tot. imposta    *
      *                              *---------------------------------*
           perform   acc-cst-iva-880      thru acc-cst-iva-889        .
      *                              *---------------------------------*
      *                              * Visualizzazione tot. documento  *
      *                              *---------------------------------*
           perform   acc-cst-iva-890      thru acc-cst-iva-899        .
       acc-cst-iva-480.
      *                          *-------------------------------------*
      *                          * Accettazione imponibile in valuta   *
      *                          *-------------------------------------*
       acc-cst-iva-490.
      *                          *-------------------------------------*
      *                          * Accettazione imponibile in valuta   *
      *                          * base                                *
      *                          *-------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "GB"                 to   v-edm                  .
           add       12
                     w-cst-ctr-rig      giving v-lin                  .
           move      32                   to   v-pos                  .
      *                              *---------------------------------*
      *                              * Tasti funzione                  *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Determinazione nr riga + 1  *
      *                                  *-----------------------------*
           add       1
                     w-cst-ctr-rig      giving w-cst-cts-rig          .
      *                                  *-----------------------------*
      *                                  * Up   : Ammesso solo se ac-  *
      *                                  *        cettazione castel-   *
      *                                  *        letto in valuta      *
      *                                  *-----------------------------*
      *                                  *-----------------------------*
      *                                  * Down : sempre ammesso       *
      *                                  *-----------------------------*
           move      "DOWN"               to   v-pfk (02)             .
      *                                  *-----------------------------*
      *                                  * Remv : sempre ammesso       *
      *                                  *-----------------------------*
           move      "REMV"               to   v-pfk (06)             .
      *                                  *-----------------------------*
      *                                  * Valore di accettazione      *
      *                                  *-----------------------------*
           move      w-acc-iva-ibl 
                    (w-cst-ctr-rig)       to   v-num                  .
      *                                  *-----------------------------*
      *                                  * Richiamo sub. accettazione  *
      *                                  *-----------------------------*
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *                          *-------------------------------------*
      *                          * Se Exit o Remv                      *
      *                          *-------------------------------------*
           if        v-key                =    "EXIT" or
                     v-key                =    "REMV"
                     go to acc-cst-iva-210.
      *                          *-------------------------------------*
      *                          * Se Delt                             *
      *                          *-------------------------------------*
           if        v-key                =    "DELT"
                     go to acc-cst-iva-490.
      *                          *-------------------------------------*
      *                          * Se Next Screen                      *
      *                          *-------------------------------------*
           if        v-key                =    "NXSC"
                     go to acc-cst-iva-490.
      *                          *-------------------------------------*
      *                          * Se Previous Screen                  *
      *                          *-------------------------------------*
           if        v-key                =    "PRSC"
                     go to acc-cst-iva-490.
      *                          *-------------------------------------*
      *                          * Se Up                               *
      *                          *-------------------------------------*
           if        v-key                =    "UP  "
                     go to acc-cst-iva-340.
      *                          *-------------------------------------*
      *                          * Se Return o Down                    *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Valore impostato in campo di    *
      *                              * destinazione                    *
      *                              *---------------------------------*
           move      v-num                to   w-acc-iva-ibl
                                              (w-cst-ctr-rig)         .
      *                              *---------------------------------*
      *                              * Imponibile a zero non ammesso   *
      *                              *---------------------------------*
           if        w-acc-iva-ibl
                    (w-cst-ctr-rig)       =    zero
                     go to acc-cst-iva-350.
      *                              *---------------------------------*
      *                              * Visualizzazione totale riga     *
      *                              *---------------------------------*
           perform   acc-cst-iva-840      thru acc-cst-iva-849        .
      *                              *---------------------------------*
      *                              * Determinazione totali castell.  *
      *                              *---------------------------------*
           perform   acc-cst-iva-830      thru acc-cst-iva-839        .
      *                              *---------------------------------*
      *                              * Visualizzazione tot. imponibile *
      *                              *---------------------------------*
           perform   acc-cst-iva-870      thru acc-cst-iva-879        .
      *                              *---------------------------------*
      *                              * Visualizzazione tot. documento  *
      *                              *---------------------------------*
           perform   acc-cst-iva-890      thru acc-cst-iva-899        .
      *                              *---------------------------------*
      *                              * Ad incremento numero riga       *
      *                              *---------------------------------*
           go to     acc-cst-iva-580.
       acc-cst-iva-520.
      *                      *-----------------------------------------*
      *                      * Se codice iva 901..999                  *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Salvataggio valore imponibile       *
      *                          *-------------------------------------*
           move      w-acc-iva-ibl
                    (w-cst-ctr-rig)       to   w-sav-iva-ibl          .
      *                          *-------------------------------------*
      *                          * Azzeramento imponibile              *
      *                          *-------------------------------------*
           move      zero                 to   w-acc-iva-ibl
                                              (w-cst-ctr-rig)         .
      *                          *-------------------------------------*
      *                          * Se imponibile precedente diverso da *
      *                          * zero : ricalcolo e rivisualizzazio- *
      *                          * ne castelletto                      *
      *                          *-------------------------------------*
           if        w-sav-iva-ibl        =    zero
                     go to acc-cst-iva-540.
      *                              *---------------------------------*
      *                              * Visualizzazione totale riga     *
      *                              *---------------------------------*
           perform   acc-cst-iva-840      thru acc-cst-iva-849        .
      *                              *---------------------------------*
      *                              * Determinazione totali castell.  *
      *                              *---------------------------------*
           perform   acc-cst-iva-830      thru acc-cst-iva-839        .
      *                              *---------------------------------*
      *                              * Visualizzazione tot. imponibile *
      *                              *---------------------------------*
           perform   acc-cst-iva-870      thru acc-cst-iva-879        .
      *                              *---------------------------------*
      *                              * Visualizzazione tot. documento  *
      *                              *---------------------------------*
           perform   acc-cst-iva-890      thru acc-cst-iva-899        .
       acc-cst-iva-540.
      *                          *-------------------------------------*
      *                          * Accettazione imposta                *
      *                          *-------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "GB"                 to   v-edm                  .
           add       12
                     w-cst-ctr-rig      giving v-lin                  .
           move      49                   to   v-pos                  .
      *                              *---------------------------------*
      *                              * Tasti funzione                  *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Determinazione nr riga + 1  *
      *                                  *-----------------------------*
           add       1
                     w-cst-ctr-rig      giving w-cst-cts-rig          .
      *                                  *-----------------------------*
      *                                  * Down : sempre ammesso       *
      *                                  *-----------------------------*
           move      "DOWN"               to   v-pfk (02)             .
      *                                  *-----------------------------*
      *                                  * Remv : sempre ammesso       *
      *                                  *-----------------------------*
           move      "REMV"               to   v-pfk (06)             .
      *                                  *-----------------------------*
      *                                  * Valore di accettazione      *
      *                                  *-----------------------------*
           move      w-acc-iva-imp 
                    (w-cst-ctr-rig)       to   v-num                  .
      *                                  *-----------------------------*
      *                                  * Richiamo sub. accettazione  *
      *                                  *-----------------------------*
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *                          *-------------------------------------*
      *                          * Se Return                           *
      *                          *-------------------------------------*
           if        v-key                =    spaces
                     go to acc-cst-iva-560.
      *                          *-------------------------------------*
      *                          * Se Exit o Remv                      *
      *                          *-------------------------------------*
           if        v-key                =    "EXIT" or
                     v-key                =    "REMV"
                     go to acc-cst-iva-210.
      *                          *-------------------------------------*
      *                          * Se Delt                             *
      *                          *-------------------------------------*
           if        v-key                =    "DELT"
                     go to acc-cst-iva-540.
      *                          *-------------------------------------*
      *                          * Se Next Screen                      *
      *                          *-------------------------------------*
           if        v-key                =    "NXSC"
                     go to acc-cst-iva-540.
      *                          *-------------------------------------*
      *                          * Se Previous Screen                  *
      *                          *-------------------------------------*
           if        v-key                =    "PRSC"
                     go to acc-cst-iva-540.
       acc-cst-iva-560.
      *                          *-------------------------------------*
      *                          * Se Return o Down                    *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Valore impostato in campo di    *
      *                              * destinazione                    *
      *                              *---------------------------------*
           move      v-num                to   w-acc-iva-imp
                                              (w-cst-ctr-rig)         .
      *                              *---------------------------------*
      *                              * Imposta a zero non ammessa      *
      *                              *---------------------------------*
           if        w-acc-iva-imp
                    (w-cst-ctr-rig)       =    zero
                     go to acc-cst-iva-540.
      *                              *---------------------------------*
      *                              * Visualizzazione totale riga     *
      *                              *---------------------------------*
           perform   acc-cst-iva-840      thru acc-cst-iva-849        .
      *                              *---------------------------------*
      *                              * Determinazione totali castell.  *
      *                              *---------------------------------*
           perform   acc-cst-iva-830      thru acc-cst-iva-839        .
      *                              *---------------------------------*
      *                              * Visualizzazione tot. imposta    *
      *                              *---------------------------------*
           perform   acc-cst-iva-880      thru acc-cst-iva-889        .
      *                              *---------------------------------*
      *                              * Visualizzazione tot. documento  *
      *                              *---------------------------------*
           perform   acc-cst-iva-890      thru acc-cst-iva-899        .
      *                              *---------------------------------*
      *                              * Ad incremento numero riga       *
      *                              *---------------------------------*
           go to     acc-cst-iva-580.
       acc-cst-iva-580.
      *              *-------------------------------------------------*
      *              * Incremento numero riga                          *
      *              *-------------------------------------------------*
           add       1                    to   w-cst-ctr-rig          .
      *                  *---------------------------------------------*
      *                  * Se fine righe : a totale documento          *
      *                  *---------------------------------------------*
           if        w-cst-ctr-rig        >    6
                     go to acc-cst-iva-600.
      *                  *---------------------------------------------*
      *                  * Altrimenti : a prossimo codice iva          *
      *                  *---------------------------------------------*
           go to     acc-cst-iva-020.
       acc-cst-iva-600.
      *              *-------------------------------------------------*
      *              * Totale documento                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      w-acc-iva-tdo        to   w-sav-iva-tdo          .
       acc-cst-iva-620.
      *                  *---------------------------------------------*
      *                  * Accettazione                                *
      *                  *---------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "GB"                 to   v-edm                  .
           move      20                   to   v-lin                  .
           move      66                   to   v-pos                  .
      *                      *-----------------------------------------*
      *                      * Tasti funzione                          *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Up   : sempre ammesso               *
      *                          *-------------------------------------*
           move      "UP  "               to   v-pfk (01)             .
      *                          *-------------------------------------*
      *                          * Down : sempre ammesso               *
      *                          *-------------------------------------*
           move      "DOWN"               to   v-pfk (02)             .
      *                          *-------------------------------------*
      *                          * Do   : sempre ammesso, purche' sia  *
      *                          *        concesso dallo status delle  *
      *                          *        impostazioni                 *
      *                          *-------------------------------------*
           move      "DO  "               to   v-pfk (05)             .
      *                      *-----------------------------------------*
      *                      * Valore di accettazione                  *
      *                      *-----------------------------------------*
           move      w-acc-iva-tdo        to   v-num                  .
      *                      *-----------------------------------------*
      *                      * Richiamo subroutine di accettazione     *
      *                      *-----------------------------------------*
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *                      *-----------------------------------------*
      *                      * Valore in campo destinazione            *
      *                      *-----------------------------------------*
           move      v-num                to   w-acc-iva-tdo          .
      *                  *---------------------------------------------*
      *                  * Se Exit                                     *
      *                  *---------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-cst-iva-999.
      *                  *---------------------------------------------*
      *                  * Se Delt                                     *
      *                  *---------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-cst-iva-999.
      *                  *---------------------------------------------*
      *                  * Se premuto un altro tasto funzione non deve *
      *                  * essere avvenuta variazione del campo        *
      *                  *---------------------------------------------*
           if        w-acc-iva-tdo        not  = w-sav-iva-tdo
                     go to acc-cst-iva-620.
      *                  *---------------------------------------------*
      *                  * Se Return                                   *
      *                  *---------------------------------------------*
           if        v-key                =    spaces
                     go to acc-cst-iva-700.
      *                  *---------------------------------------------*
      *                  * Se Up                                       *
      *                  *---------------------------------------------*
           if        v-key                not  = "UP  "
                     go to acc-cst-iva-660.
      *                      *-----------------------------------------*
      *                      * Allineamento all'ultima riga non vuota  *
      *                      * oppure alla prima riga                  *
      *                      *-----------------------------------------*
           move      6                    to   w-cst-ctr-rig          .
       acc-cst-iva-640.
           if        w-acc-iva-cod
                    (w-cst-ctr-rig)       =    zero
                     if    w-cst-ctr-rig  >    1
                           subtract  1    from w-cst-ctr-rig
                           go to     acc-cst-iva-640.
           go to     acc-cst-iva-020.
       acc-cst-iva-660.
      *                  *---------------------------------------------*
      *                  * Totale documento a zero non ammesso         *
      *                  *---------------------------------------------*
           if        w-acc-iva-tdo        =    zero
                     go to acc-cst-iva-620.
      *                  *---------------------------------------------*
      *                  * Se Down : come per Return                   *
      *                  *---------------------------------------------*
           if        v-key                =    "DOWN"
                     move  spaces         to   v-key
                     go to acc-cst-iva-700.
      *                  *---------------------------------------------*
      *                  * Se Do                                       *
      *                  *---------------------------------------------*
           if        v-key                not  = "DO  "
                     go to acc-cst-iva-680.
      *                      *-----------------------------------------*
      *                      * Controllo castelletto iva               *
      *                      *-----------------------------------------*
           perform   acc-cst-iva-980      thru acc-cst-iva-989        .
           if        w-cst-flg-exi        not  = spaces
                     go to acc-cst-iva-620.
      *                      *-----------------------------------------*
      *                      * Controllo globali                       *
      *                      *-----------------------------------------*
           perform   cnt-tdo-nok-000      thru cnt-tdo-nok-999        .
      *                      *-----------------------------------------*
      *                      * Test su esito controllo                 *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se positivo : uscita                *
      *                          *-------------------------------------*
           if        w-cnt-tdo-nok-flg    =    spaces
                     move  "S"            to   w-cnt-tus-acc-tes
                     go to acc-cst-iva-999.
      *                          *-------------------------------------*
      *                          * Altrimenti si torna alla reimposta- *
      *                          * zione del totale documento          *
      *                          *-------------------------------------*
           go to     acc-cst-iva-620.
       acc-cst-iva-680.
      *                  *---------------------------------------------*
      *                  * Se Nxsc                                     *
      *                  *---------------------------------------------*
           if        v-key                not  = "NXSC"
                     go to acc-cst-iva-690.
      *                      *-----------------------------------------*
      *                      * Controllo castelletto iva               *
      *                      *-----------------------------------------*
           perform   acc-cst-iva-980      thru acc-cst-iva-989        .
           if        w-cst-flg-exi        not  = spaces
                     go to acc-cst-iva-620.
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           move      "+"                  to   w-cnt-tus-acc-tes      .
           go to     acc-cst-iva-999.
       acc-cst-iva-690.
      *                  *---------------------------------------------*
      *                  * Se Prsc                                     *
      *                  *---------------------------------------------*
           if        v-key                not  = "PRSC"
                     go to acc-cst-iva-700.
      *                      *-----------------------------------------*
      *                      * Controllo castelletto iva               *
      *                      *-----------------------------------------*
           perform   acc-cst-iva-980      thru acc-cst-iva-989        .
           if        w-cst-flg-exi        not  = spaces
                     go to acc-cst-iva-620.
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           move      "-"                  to   w-cnt-tus-acc-tes      .
           go to     acc-cst-iva-999.
       acc-cst-iva-700.
      *                  *---------------------------------------------*
      *                  * Se Return                                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Controllo castelletto iva               *
      *                      *-----------------------------------------*
           perform   acc-cst-iva-980      thru acc-cst-iva-989        .
           if        w-cst-flg-exi        not  = spaces
                     go to acc-cst-iva-620.
           move      spaces               to   v-key                  .
           go to     acc-cst-iva-999.
       acc-cst-iva-800.
      *              *-------------------------------------------------*
      *              * Subroutine interna di compattamento controllato *
      *              * castelletto : solo se il codice iva e' a zero   *
      *              *-------------------------------------------------*
           if        w-acc-iva-cod
                    (w-cst-ctr-rig)       not  = zero
                     go to acc-cst-iva-809.
           if        w-cst-ctr-rig        =    6
                     go to acc-cst-iva-809.
           if        w-acc-iva-cod
                    (w-cst-cts-rig)       =    zero
                     go to acc-cst-iva-809.
           perform   acc-cst-iva-810      thru acc-cst-iva-819        .
       acc-cst-iva-809.
           exit.
       acc-cst-iva-810.
      *              *-------------------------------------------------*
      *              * Subroutine interna di compattamento righe del   *
      *              * castelletto escludendo la riga w-cst-ctr-rig    *
      *              * con rideterminazione e rivisualizzazione totali *
      *              *-------------------------------------------------*
           move      w-cst-ctr-rig        to   w-cst-ctx-rig          .
           add       1
                     w-cst-ctx-rig    giving   w-cst-cty-rig          .
       acc-cst-iva-811.
           if        w-cst-ctx-rig        =    6
                     move  zero           to   w-acc-iva-ibv (6)   
                     move  zero           to   w-acc-iva-ibl (6)   
                     move  zero           to   w-acc-iva-cod (6)   
                     move  zero           to   w-acc-iva-imp (6)   
                     go to acc-cst-iva-812.
           move      w-acc-iva-rig
                    (w-cst-cty-rig)       to   w-acc-iva-rig
                                              (w-cst-ctx-rig)         .
           add       1                    to   w-cst-ctx-rig          .
           add       1                    to   w-cst-cty-rig          .
           go to     acc-cst-iva-811.
       acc-cst-iva-812.
           move      w-cst-ctr-rig        to   w-cst-ctx-rig          .
           add       12                   to   w-cst-ctx-rig          .
           add       1
                     w-cst-ctx-rig    giving   w-cst-cty-rig          .
       acc-cst-iva-813.
           if        w-cst-ctx-rig        =    18
                     go to acc-cst-iva-814.
           move      "FL"                 to   v-ope                  .
           move      w-cst-cty-rig        to   v-lin                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      w-cst-ctx-rig        to   v-lin                  .
           move      01                   to   v-pos                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           if        v-alf                =    spaces
                     go to acc-cst-iva-815.
           add       1                    to   w-cst-ctx-rig          .
           add       1                    to   w-cst-cty-rig          .
           go to     acc-cst-iva-813.
       acc-cst-iva-814.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       acc-cst-iva-815.
            perform  acc-cst-iva-830      thru acc-cst-iva-839        .
            perform  acc-cst-iva-860      thru acc-cst-iva-869        .
       acc-cst-iva-819.
           exit.
       acc-cst-iva-820.
      *              *-------------------------------------------------*
      *              * Subroutine interna di inserimento della riga    *
      *              * numero w-cst-ctr-rig nel castelletto iva        *
      *              *-------------------------------------------------*
           move      5                    to   w-cst-cty-rig          .
       acc-cst-iva-821.
           if        w-acc-iva-cod
                    (w-cst-cty-rig)       =    zero
                     subtract  1          from w-cst-cty-rig
                     go to     acc-cst-iva-821.
           move      w-cst-cty-rig        to   w-cst-ctx-rig          .
           add       1                    to   w-cst-cty-rig          .
       acc-cst-iva-822.
           move      w-acc-iva-rig
                    (w-cst-ctx-rig)       to   w-acc-iva-rig
                                              (w-cst-cty-rig)         .
           if        w-cst-ctx-rig        not  = w-cst-ctr-rig
                     subtract  1          from w-cst-ctx-rig
                     subtract  1          from w-cst-cty-rig
                     go to acc-cst-iva-822.
           move      zero                 to   w-acc-iva-ibl
                                              (w-cst-ctr-rig)         .
           move      zero                 to   w-acc-iva-cod
                                              (w-cst-ctr-rig)         .
           move      zero                 to   w-acc-iva-imp
                                              (w-cst-ctr-rig)         .
           move      5                    to   w-cst-cty-rig          .
       acc-cst-iva-823.
           if        w-cst-cty-rig        =    w-cst-ctr-rig
                     go to     acc-cst-iva-824.
           if        w-acc-iva-cod
                    (w-cst-cty-rig)       =    zero
                     subtract  1          from w-cst-cty-rig
                     go to     acc-cst-iva-823.
       acc-cst-iva-824.
           move      w-cst-cty-rig        to   w-cst-ctx-rig          .
           add       1                    to   w-cst-cty-rig          .
       acc-cst-iva-825.
           move      "FL"                 to   v-ope                  .
           add       12
                     w-cst-ctx-rig      giving v-lin                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           add       12
                     w-cst-cty-rig      giving v-lin                  .
           move      01                   to   v-pos                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           if        w-cst-ctx-rig        not  = w-cst-ctr-rig
                     subtract  1          from w-cst-ctx-rig
                     subtract  1          from w-cst-cty-rig
                     go to acc-cst-iva-825.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           add       12
                     w-cst-ctr-rig      giving v-lin                  .
           move      01                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       acc-cst-iva-829.
           exit.
       acc-cst-iva-830.
      *              *-------------------------------------------------*
      *              * Subroutine interna di determinazione totali per *
      *              * castelletto iva                                 *
      *              *-------------------------------------------------*
           move      zero                 to   w-acc-iva-tiv          .
           move      zero                 to   w-acc-iva-tib          .
           move      zero                 to   w-acc-iva-tim          .
           move      zero                 to   w-acc-iva-tnd          .
           move      zero                 to   w-cst-ctx-rig          .
       acc-cst-iva-831.
           add       1                    to   w-cst-ctx-rig          .
           if        w-cst-ctx-rig        >    6
                     go to acc-cst-iva-832.
           if        w-acc-iva-cod
                    (w-cst-ctx-rig)       =    zero
                     go to acc-cst-iva-831.
           add       w-acc-iva-ibv
                    (w-cst-ctx-rig)       to   w-acc-iva-tiv          .
           add       w-acc-iva-ibl
                    (w-cst-ctx-rig)       to   w-acc-iva-tib          .
           add       w-acc-iva-imp
                    (w-cst-ctx-rig)       to   w-acc-iva-tim          .
           if        w-acc-iva-cod
                    (w-cst-ctx-rig)       >    700 and
                     w-acc-iva-cod
                    (w-cst-ctx-rig)       <    800
                     add   w-acc-iva-ibl
                          (w-cst-ctx-rig)   
                                          to   w-acc-iva-tnd          .
           go to     acc-cst-iva-831.
       acc-cst-iva-832.
           add       w-acc-iva-tib    
                     w-acc-iva-tim      giving w-acc-iva-tdo          .
       acc-cst-iva-839.
           exit.
       acc-cst-iva-840.
      *              *-------------------------------------------------*
      *              * Subroutine interna di visualizzazione totale    *
      *              * riga castelletto nr w-cst-ctr-rig               *
      *              *-------------------------------------------------*
           perform   acc-cst-iva-850      thru acc-cst-iva-859        .
           move      "DS"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "GB"                 to   v-edm                  .
           add       12
                     w-cst-ctr-rig      giving v-lin                  .
           move      66                   to   v-pos                  .
           move      w-cst-tot-rig        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-cst-iva-849.
           exit.
       acc-cst-iva-850.
      *              *-------------------------------------------------*
      *              * Subroutine interna di determinazione totale ri- *
      *              * ga castelletto w-cst-ctr-rig in w-cst-tot-rig   *
      *              *-------------------------------------------------*
           add       w-acc-iva-ibl
                    (w-cst-ctr-rig)   
                     w-acc-iva-imp
                    (w-cst-ctr-rig)     giving w-cst-tot-rig          .
       acc-cst-iva-859.
           exit.
       acc-cst-iva-860.
      *              *-------------------------------------------------*
      *              * Subroutine interna di visualizzazione totali    *
      *              * castelletto iva                                 *
      *              *-------------------------------------------------*
           perform   acc-cst-iva-870      thru acc-cst-iva-879        .
           perform   acc-cst-iva-880      thru acc-cst-iva-889        .
           perform   acc-cst-iva-890      thru acc-cst-iva-899        .
       acc-cst-iva-869.
           exit.
       acc-cst-iva-870.
      *              *-------------------------------------------------*
      *              * Subroutine interna di visualizzazione del tota- *
      *              * le imponibile sia in valuta che non             *
      *              *-------------------------------------------------*
       acc-cst-iva-875.
      *                  *---------------------------------------------*
      *                  * Imponibile in valuta base                   *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "GB"                 to   v-edm                  .
           move      20                   to   v-lin                  .
           move      32                   to   v-pos                  .
           move      w-acc-iva-tib        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-cst-iva-879.
           exit.
       acc-cst-iva-880.
      *              *-------------------------------------------------*
      *              * Subroutine interna di visualizzazione del tota- *
      *              * le imposta                                      *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "GB"                 to   v-edm                  .
           move      20                   to   v-lin                  .
           move      49                   to   v-pos                  .
           move      w-acc-iva-tim        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-cst-iva-889.
           exit.
       acc-cst-iva-890.
      *              *-------------------------------------------------*
      *              * Subroutine interna di visualizzazione del tota- *
      *              * le documento                                    *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "GB"                 to   v-edm                  .
           move      20                   to   v-lin                  .
           move      66                   to   v-pos                  .
           move      w-acc-iva-tdo        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-cst-iva-899.
           exit.
       acc-cst-iva-910.
      *              *-------------------------------------------------*
      *              * Subroutine interna di determinazione imposta a  *
      *              * fronte imponibile per riga w-cst-ctr-rig        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione                                *
      *                  *---------------------------------------------*
           move      w-acc-iva-cod
                    (w-cst-ctr-rig)       to   w-cst-cod-iva          .
           move      zero                 to   w-cst-cod-001          .
           move      zero                 to   w-cst-cod-002          .
           move      zero                 to   w-cst-cod-003          .
      *                  *---------------------------------------------*
      *                  * Determinazione                              *
      *                  *---------------------------------------------*
           move      w-acc-iva-ibl 
                    (w-cst-ctr-rig)       to   d-imp-iva-ibl-iva      .
           move      w-cst-cod-iva        to   d-imp-iva-cod-iva      .
           perform   det-imp-iva-000      thru det-imp-iva-999        .
           move      d-imp-iva-ims-iva    to   w-acc-iva-imp 
                                              (w-cst-ctr-rig)         .
       acc-cst-iva-919.
           exit.
       acc-cst-iva-920.
      *              *-------------------------------------------------*
      *              * Subroutine interna di scorporo imponibile e im- *
      *              * posta per riga w-cst-ctr-rig da w-cst-tot-rig   *
      *              * per aliquote di corrispettivi                   *
      *              *-------------------------------------------------*
           move      "SI"                 to   d-imp-iva-tip-ope      .
      *
           move      w-cst-tot-rig        to   d-imp-iva-ibl-iva      .
           move      w-acc-iva-cod
                    (w-cst-ctr-rig)       to   d-imp-iva-cod-iva      .
      *
           perform   det-imp-iva-cll-000  thru det-imp-iva-cll-999    .
      *
           move      d-imp-iva-ibl-iva    to   w-acc-iva-ibl 
                                              (w-cst-ctr-rig)         .
           move      d-imp-iva-ims-iva    to   w-acc-iva-imp 
                                              (w-cst-ctr-rig)         .
       acc-cst-iva-929.
           exit.
       acc-cst-iva-930.
      *              *-------------------------------------------------*
      *              * Subroutine interna di scorporo imponibile e im- *
      *              * posta per riga w-cst-ctr-rig da w-cst-tot-rig   *
      *              * per aliquote non di corrispettivi               *
      *              *-------------------------------------------------*
           move      "SI"                 to   d-imp-iva-tip-ope      .
      *
           move      w-cst-tot-rig        to   d-imp-iva-ibl-iva      .
           move      w-acc-iva-cod
                    (w-cst-ctr-rig)       to   d-imp-iva-cod-iva      .
           add       100                  to   d-imp-iva-cod-iva      .
      *
           perform   det-imp-iva-cll-000  thru det-imp-iva-cll-999    .
      *
           move      d-imp-iva-ibl-iva    to   w-acc-iva-ibl 
                                              (w-cst-ctr-rig)         .
           move      d-imp-iva-ims-iva    to   w-acc-iva-imp 
                                              (w-cst-ctr-rig)         .
       acc-cst-iva-939.
           exit.
       acc-cst-iva-980.
      *              *-------------------------------------------------*
      *              * Subroutine interna di esecuzione operazioni per *
      *              * fine impostazione castelletto iva               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione status di uscita            *
      *                  *---------------------------------------------*
           move      spaces               to   w-cst-flg-exi          .
      *                  *---------------------------------------------*
      *                  * Controllo che almeno una riga sia presente  *
      *                  *---------------------------------------------*
           if        w-acc-iva-cod (1)    =    zero
                     move  "#"            to   w-cst-flg-exi
                     go to acc-cst-iva-989.
      *                  *---------------------------------------------*
      *                  * Controllo che il totale documento sia di-   *
      *                  * verso da zero                               *
      *                  *---------------------------------------------*
           if        w-acc-iva-tdo        =    zero
                     move  "#"            to   w-cst-flg-exi
                     go to acc-cst-iva-989.
      *                  *---------------------------------------------*
      *                  * Determinazione del totale imposta non dedu- *
      *                  * cibile                                      *
      *                  *---------------------------------------------*
           move      zero                 to   w-acc-iva-tnd          .
           move      zero                 to   w-cst-ctz-rig          .
       acc-cst-iva-984.
           add       1                    to   w-cst-ctz-rig          .
           if        w-cst-ctz-rig        >    6
                     go to acc-cst-iva-986.
           if        w-acc-iva-cod
                    (w-cst-ctz-rig)       >    700 and
                     w-acc-iva-cod
                    (w-cst-ctz-rig)       <    800
                     add   w-acc-iva-imp
                          (w-cst-ctz-rig) to   w-acc-iva-tnd          .
           go to     acc-cst-iva-984.
       acc-cst-iva-986.
       acc-cst-iva-989.
           exit.
       acc-cst-iva-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Castelletto iva                   *
      *    *-----------------------------------------------------------*
       vis-cst-iva-000.
      *              *-------------------------------------------------*
      *              * Inizializzazione contatore 1..6                 *
      *              *-------------------------------------------------*
           move      zero                 to   w-cst-ctr-rig          .
      *              *-------------------------------------------------*
      *              * Ciclo 1..6                                      *
      *              *-------------------------------------------------*
       vis-cst-iva-100.
           add       1                    to   w-cst-ctr-rig          .
           if        w-cst-ctr-rig        >    6
                     go to  vis-cst-iva-400.
           if        w-acc-iva-cod 
                    (w-cst-ctr-rig)       =    zero and
                     w-acc-iva-ibl 
                    (w-cst-ctr-rig)       =    zero and
                     w-acc-iva-imp 
                    (w-cst-ctr-rig)       =    zero
                     go to  vis-cst-iva-400.
      *                  *---------------------------------------------*
      *                  * Codice iva                                  *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "B"                  to   v-edm                  .
           add       12
                     w-cst-ctr-rig      giving v-lin                  .
           move      22                   to   v-pos                  .
           move      w-acc-iva-cod
                    (w-cst-ctr-rig)       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Imponibile in valuta                    *
      *                      *-----------------------------------------*
       vis-cst-iva-120.
      *                      *-----------------------------------------*
      *                      * Imponibile in valuta base               *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "GB"                 to   v-edm                  .
           add       12
                     w-cst-ctr-rig      giving v-lin                  .
           move      32                   to   v-pos                  .
           move      w-acc-iva-ibl
                    (w-cst-ctr-rig)       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Imposta o esenzione                     *
      *                      *-----------------------------------------*
           move      w-acc-iva-cod
                    (w-cst-ctr-rig)       to   w-cst-cod-iva          .
           move      zero                 to   w-cst-cod-001
                                               w-cst-cod-002          .
           if        w-cst-cod-iva        >    300   and
                     w-cst-cod-iva        <    700
                     go to  vis-cst-iva-200.
      *                      *-----------------------------------------*
      *                      * Caso imposta                            *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "GB"                 to   v-edm                  .
           add       12
                     w-cst-ctr-rig      giving v-lin                  .
           move      49                   to   v-pos                  .
           move      w-acc-iva-imp
                    (w-cst-ctr-rig)       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           go to     vis-cst-iva-300.
       vis-cst-iva-200.
      *                      *-----------------------------------------*
      *                      * Caso esenzione                          *
      *                      *-----------------------------------------*
       vis-cst-iva-300.
      *                      *-----------------------------------------*
      *                      * Totale riga                             *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           add       12
                     w-cst-ctr-rig      giving v-lin                  .
           move      "GB"                 to   v-edm                  .
           move      66                   to   v-pos                  .
           add       w-acc-iva-ibl
                    (w-cst-ctr-rig)   
                     w-acc-iva-imp
                    (w-cst-ctr-rig)     giving v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Riciclo a riga castelletto successiva   *
      *                      *-----------------------------------------*
           go to     vis-cst-iva-100.
       vis-cst-iva-400.
      *                  *---------------------------------------------*
      *                  * Visualizzazione riga totali                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Totale imponibile in valuta             *
      *                      *-----------------------------------------*
       vis-cst-iva-420.
      *                      *-----------------------------------------*
      *                      * Totale imponibile                       *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "GB"                 to   v-edm                  .
           move      20                   to   v-lin                  .
           move      32                   to   v-pos                  .
           move      w-acc-iva-tib        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Totale imposta                          *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "GB"                 to   v-edm                  .
           move      20                   to   v-lin                  .
           move      49                   to   v-pos                  .
           move      w-acc-iva-tim        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Totale documento                        *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "GB"                 to   v-edm                  .
           move      20                   to   v-lin                  .
           move      66                   to   v-pos                  .
           move      w-acc-iva-tdo        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cst-iva-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo riga : Si/No addebito spesa in funzio- *
      *    * ne di w-acc-iep-spe-ctr                                   *
      *    *-----------------------------------------------------------*
       acc-spe-snx-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-prs-spe-fat-nst    =    zero
                     go to acc-spe-snx-999.
      *                  *---------------------------------------------*
      *                  * Determinazione indice per la spesa          *
      *                  *---------------------------------------------*
           move      w-prs-spe-fat-npt
                    (w-acc-iep-spe-ctr)   to   w-acc-iep-spe-inx      .
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      rf-fit-spe-snx
                    (w-acc-iep-spe-inx)   to   w-sav-spe-snx          .
       acc-spe-snx-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-spe-snx-lun    to   v-car                  .
           move      w-exp-spe-snx-num    to   v-ldt                  .
           move      "NS#"                to   v-msk                  .
           move      spaces               to   v-edm                  .
           add       09
                     w-acc-iep-spe-ctr  giving v-lin                  .
           move      28                   to   v-pos                  .
           move      w-exp-spe-snx-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-tes    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           if        rf-fit-spe-snx
                    (w-acc-iep-spe-inx)
                                          =    0
                     move  1              to   v-num
           else if   rf-fit-spe-snx
                    (w-acc-iep-spe-inx)
                                          =    1
                     move  2              to   v-num
           else      move  zero           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-spe-snx-999.
      *              *-------------------------------------------------*
      *              * Valore impostato                                *
      *              *-------------------------------------------------*
           if        v-num                =    1
                     move  0              to   rf-fit-spe-snx
                                              (w-acc-iep-spe-inx)
           else if   v-num                =    2
                     move  1              to   rf-fit-spe-snx
                                              (w-acc-iep-spe-inx)
           else      move  zero           to   rf-fit-spe-snx
                                              (w-acc-iep-spe-inx)     .
       acc-spe-snx-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-spe-snx-425.
      *                  *---------------------------------------------*
      *                  * Se in rettifica dati provvigionali in fat-  *
      *                  * tura non si accetta la modifica del campo   *
      *                  *---------------------------------------------*
           if        rf-fit-spe-snx
                    (w-acc-iep-spe-inx)
                                          =    w-sav-spe-snx
                     go to acc-spe-snx-450.
           move      w-sav-spe-snx        to   rf-fit-spe-snx
                                              (w-acc-iep-spe-inx)     .
           go to     acc-spe-snx-100.
       acc-spe-snx-450.
      *                  *---------------------------------------------*
      *                  * Fine controlli su valore impostato          *
      *                  *---------------------------------------------*
           go to     acc-spe-snx-600.
       acc-spe-snx-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore attuale diverso dal precedente    *
      *                  *---------------------------------------------*
           if        rf-fit-spe-snx
                    (w-acc-iep-spe-inx)
                                          =    w-sav-spe-snx
                     go to acc-spe-snx-800.
      *                      *-----------------------------------------*
      *                      * Trattamento importo e percentuale spesa *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se si addebito : oltre              *
      *                          *-------------------------------------*
           if        rf-fit-spe-snx
                    (w-acc-iep-spe-inx)
                                          =    1
                     go to acc-spe-snx-650.
      *                          *-------------------------------------*
      *                          * Test su personalizzazione per tipo  *
      *                          * accettazione                        *
      *                          *-------------------------------------*
           if        w-prs-spe-fat-tac    =    01
                     go to acc-spe-snx-650.
      *                          *-------------------------------------*
      *                          * Normalizzazione importo e percen-   *
      *                          * tuale                               *
      *                          *-------------------------------------*
           move      zero                 to   rf-fit-spe-imp
                                              (w-acc-iep-spe-inx)     .
           move      zero                 to   rf-fit-spe-per
                                              (w-acc-iep-spe-inx)     .
       acc-spe-snx-650.
      *                      *-----------------------------------------*
      *                      * Determinazione totali riepilogo docum.  *
      *                      *-----------------------------------------*
______*    perform   det-tri-doc-000      thru det-tri-doc-999        .
       acc-spe-snx-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-spe-snx-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-spe-snx-100.
       acc-spe-snx-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo campo riga : Si/No addebito spese   *
      *    *-----------------------------------------------------------*
       vis-spe-snx-000.
      *              *-------------------------------------------------*
      *              * Ciclo di scansione                              *
      *              *-------------------------------------------------*
           move      zero                 to   w-vis-iep-spe-ctr      .
       vis-spe-snx-100.
           add       1                    to   w-vis-iep-spe-ctr      .
           if        w-vis-iep-spe-ctr    >    w-prs-spe-fat-nst
                     go to vis-spe-snx-999.
      *                  *---------------------------------------------*
      *                  * Determinazione indice per la spesa          *
      *                  *---------------------------------------------*
           move      w-prs-spe-fat-npt
                    (w-vis-iep-spe-ctr)   to   w-vis-iep-spe-inx      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione                             *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-spe-snx-lun    to   v-car                  .
           move      w-exp-spe-snx-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           add       09
                     w-vis-iep-spe-ctr  giving v-lin                  .
           move      28                   to   v-pos                  .
           move      w-exp-spe-snx-tbl    to   v-txt                  .
           if        rf-fit-spe-snx
                    (w-vis-iep-spe-inx)
                                          =    0
                     move  1              to   v-num
           else if   rf-fit-spe-snx
                    (w-vis-iep-spe-inx)
                                          =    1
                     move  2              to   v-num
           else      move  zero           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Riciclo su scansione                        *
      *                  *---------------------------------------------*
           go to     vis-spe-snx-100.
       vis-spe-snx-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo riga : Importo spesa in funzione di    *
      *    * w-acc-iep-spe-ctr                                         *
      *    *-----------------------------------------------------------*
       acc-spe-imp-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Determinazione indice per la spesa          *
      *                  *---------------------------------------------*
           move      w-prs-spe-fat-npt
                    (w-acc-iep-spe-ctr)   to   w-acc-iep-spe-inx      .
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se numero spese personalizzate a zero : *
      *                      * il campo non e' da accettare            *
      *                      *-----------------------------------------*
           if        w-prs-spe-fat-nst    =    zero
                     go to acc-spe-imp-999.
      *                      *-----------------------------------------*
      *                      * Se spesa da addebitare ma in rettifica  *
      *                      * dati provvigionali in fattura : no ac-  *
      *                      * cettazione                              *
      *                      *-----------------------------------------*
           if        rf-fit-spe-snx
                    (w-acc-iep-spe-inx)
                                          =    1
                     go to acc-spe-imp-999.
      *                      *-----------------------------------------*
      *                      * Se spesa da addebitare : oltre          *
      *                      *-----------------------------------------*
           if        rf-fit-spe-snx
                    (w-acc-iep-spe-inx)
                                          =    1
                     go to acc-spe-imp-050.
      *                      *-----------------------------------------*
      *                      * Test su personalizzazione per tipo ac-  *
      *                      * cettazione                              *
      *                      *-----------------------------------------*
           if        w-prs-spe-fat-tac    =    00
                     go to acc-spe-imp-999.
       acc-spe-imp-050.
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      rf-fit-spe-imp
                    (w-acc-iep-spe-inx)
                                          to   w-sav-spe-imp          .
       acc-spe-imp-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      09                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "GB"                 to   v-edm                  .
           add       09
                     w-acc-iep-spe-ctr  giving v-lin                  .
           move      31                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-tes    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      rf-fit-spe-imp
                    (w-acc-iep-spe-inx)
                                          to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-spe-imp-999.
      *              *-------------------------------------------------*
      *              * Valore impostato                                *
      *              *-------------------------------------------------*
           move      v-num                to   rf-fit-spe-imp
                                              (w-acc-iep-spe-inx)  .
       acc-spe-imp-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-spe-imp-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore attuale diverso dal precedente    *
      *                  *---------------------------------------------*
           if        rf-fit-spe-imp
                    (w-acc-iep-spe-inx)
                                          =    w-sav-spe-imp
                     go to acc-spe-imp-800.
      *                      *-----------------------------------------*
      *                      * Normalizzazione percentuale spesa       *
      *                      *-----------------------------------------*
           move      zero                 to   rf-fit-spe-per
                                              (w-acc-iep-spe-inx)  .
      *                      *-----------------------------------------*
      *                      * Determinazione totali riepilogo docum.  *
      *                      *-----------------------------------------*
______*    perform   det-tri-doc-000      thru det-tri-doc-999        .
       acc-spe-imp-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-spe-imp-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-spe-imp-100.
       acc-spe-imp-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo campo riga : Importi spese          *
      *    *-----------------------------------------------------------*
       vis-spe-imp-000.
      *              *-------------------------------------------------*
      *              * Ciclo di scansione                              *
      *              *-------------------------------------------------*
           move      zero                 to   w-vis-iep-spe-ctr      .
       vis-spe-imp-100.
           add       1                    to   w-vis-iep-spe-ctr      .
           if        w-vis-iep-spe-ctr    >    w-prs-spe-fat-nst
                     go to vis-spe-imp-999.
      *                  *---------------------------------------------*
      *                  * Determinazione indice per la spesa          *
      *                  *---------------------------------------------*
           move      w-prs-spe-fat-npt
                    (w-vis-iep-spe-ctr)   to   w-vis-iep-spe-inx      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione                             *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      09                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "GB"                 to   v-edm                  .
           add       09
                     w-vis-iep-spe-ctr  giving v-lin                  .
           move      31                   to   v-pos                  .
           move      rf-fit-spe-imp
                    (w-vis-iep-spe-inx)
                                          to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Riciclo su scansione                        *
      *                  *---------------------------------------------*
           go to     vis-spe-imp-100.
       vis-spe-imp-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo riga : % spesa in funzione di          *
      *    * w-acc-iep-spe-ctr                                         *
      *    *-----------------------------------------------------------*
       acc-spe-per-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Determinazione indice per la spesa          *
      *                  *---------------------------------------------*
           move      w-prs-spe-fat-npt
                    (w-acc-iep-spe-ctr)   to   w-acc-iep-spe-inx      .
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se numero spese personalizzate a zero : *
      *                      * il campo non e' da accettare            *
      *                      *-----------------------------------------*
           if        w-prs-spe-fat-nst    =    zero
                     go to acc-spe-per-999.
      *                      *-----------------------------------------*
      *                      * Se tipo di imponibile per la spesa a    *
      *                      * zero : no                               *
      *                      *-----------------------------------------*
           if        rf-fit-spe-ibl
                    (w-acc-iep-spe-inx)
                                          =    zero
                     go to acc-spe-per-999.
      *                      *-----------------------------------------*
      *                      * Se spesa da addebitare ma in rettifica  *
      *                      * dati provvigionali in fattura : no ac-  *
      *                      * cettazione                              *
      *                      *-----------------------------------------*
           if        rf-fit-spe-snx
                    (w-acc-iep-spe-inx)
                                          =    1
                     go to acc-spe-per-999.
      *                      *-----------------------------------------*
      *                      * Se spesa da addebitare : oltre          *
      *                      *-----------------------------------------*
           if        rf-fit-spe-snx
                    (w-acc-iep-spe-inx)
                                          =    1
                     go to acc-spe-per-050.
      *                      *-----------------------------------------*
      *                      * Test su personalizzazione per tipo ac-  *
      *                      * cettazione                              *
      *                      *-----------------------------------------*
           if        w-prs-spe-fat-tac    =    00
                     go to acc-spe-per-999.
       acc-spe-per-050.
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      rf-fit-spe-per
                    (w-acc-iep-spe-inx)
                                          to   w-sav-spe-per          .
       acc-spe-per-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      01                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "BD"                 to   v-edm                  .
           add       09
                     w-acc-iep-spe-ctr  giving v-lin                  .
           move      45                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-tes    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      rf-fit-spe-per
                    (w-acc-iep-spe-inx)
                                          to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-spe-per-999.
      *              *-------------------------------------------------*
      *              * Valore impostato                                *
      *              *-------------------------------------------------*
           move      v-num                to   rf-fit-spe-per
                                              (w-acc-iep-spe-inx)  .
       acc-spe-per-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-spe-per-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore attuale diverso dal precedente    *
      *                  *---------------------------------------------*
           if        rf-fit-spe-per
                    (w-acc-iep-spe-inx)
                                          =    w-sav-spe-per
                     go to acc-spe-per-800.
      *                      *-----------------------------------------*
      *                      * Determinazione totali riepilogo docum.  *
      *                      *-----------------------------------------*
______*    perform   det-tri-doc-000      thru det-tri-doc-999        .
       acc-spe-per-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-spe-per-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-spe-per-100.
       acc-spe-per-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo campo riga : % spese                *
      *    *-----------------------------------------------------------*
       vis-spe-per-000.
      *              *-------------------------------------------------*
      *              * Ciclo di scansione                              *
      *              *-------------------------------------------------*
           move      zero                 to   w-vis-iep-spe-ctr      .
       vis-spe-per-100.
           add       1                    to   w-vis-iep-spe-ctr      .
           if        w-vis-iep-spe-ctr    >    w-prs-spe-fat-nst
                     go to vis-spe-per-999.
      *                  *---------------------------------------------*
      *                  * Determinazione indice per la spesa          *
      *                  *---------------------------------------------*
           move      w-prs-spe-fat-npt
                    (w-vis-iep-spe-ctr)   to   w-vis-iep-spe-inx      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione                             *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      01                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "BD"                 to   v-edm                  .
           add       09
                     w-vis-iep-spe-ctr  giving v-lin                  .
           move      45                   to   v-pos                  .
           move      rf-fit-spe-per
                    (w-vis-iep-spe-inx)
                                          to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Riciclo su scansione                        *
      *                  *---------------------------------------------*
           go to     vis-spe-per-100.
       vis-spe-per-999.
           exit.

      *    *===========================================================*
      *    * Controllo su impostazione tasto Do campi chiave           *
      *    *-----------------------------------------------------------*
       cnt-tdo-key-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-tdo-key-flg      .
      *              *-------------------------------------------------*
      *              * Test                                            *
      *              *-------------------------------------------------*
           if        rf-fit-cod-tmo       =    spaces or
                     rf-fit-dat-doc       =    zero   or
                     rf-fit-num-doc       =    zero
                     move  "#"            to   w-cnt-tdo-key-flg      .
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
           if        rf-fit-cod-tmo       =    spaces and
                     rf-fit-dat-doc       =    zero   and
                     rf-fit-num-doc       =    zero
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
      *              * Normalizzazione                                 *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/fat/fls/ioc/obj/ioffit"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fit                 .
       nor-key-nok-999.
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
      *              * Salvataggio valori per la start                 *
      *              *-------------------------------------------------*
           move      rf-fit-dat-doc       to   w-acc-dat-doc          .
           move      rf-fit-num-doc       to   w-acc-num-doc          .
           move      rf-fit-cod-tmo       to   w-acc-cod-tmo          .
      *              *-------------------------------------------------*
      *              * Start su file [fit]                             *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "IDEDOC    "         to   f-key                  .
           move      w-acc-cod-dpz        to   rf-fit-cod-dpz         .
           move      zero                 to   rf-fit-num-prt         .
           move      "pgm/fat/fls/ioc/obj/ioffit"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fit                 .
      *              *-------------------------------------------------*
      *              * Se Start errata : a documento non esistente     *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     move  "#"            to   w-cnt-rou-let-reg
                     go to rou-let-reg-999.
       rou-let-reg-100.
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
      *                  * Se Read Next : a controllo                  *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     move  "#"            to   w-cnt-rou-let-reg
                     go to rou-let-reg-999.
       rou-let-reg-300.
      *              *-------------------------------------------------*
      *              * Max su [fit], se non superato : a determinazio- *
      *              * ne finale                                       *
      *              *-------------------------------------------------*
           if        rf-fit-dat-doc       not  = w-acc-dat-doc or
                     rf-fit-cod-dpz       not  = w-acc-cod-dpz or
                     rf-fit-cod-tmo       not  = w-acc-cod-tmo or
                     rf-fit-num-doc       not  = w-acc-num-doc
                     move  "#"            to   w-cnt-rou-let-reg
                     go to rou-let-reg-999.
       rou-let-reg-800.
      *              *-------------------------------------------------*
      *              * Se record trovato                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Tipo funzionamento : Modifica               *
      *                  *---------------------------------------------*
           move      "M"                  to   w-cnt-mfu-tip-fun      .
      *                  *---------------------------------------------*
      *                  * Preparazione codice cliente                 *
      *                  *---------------------------------------------*
           move      rf-fit-cod-cli       to   w-acc-cod-arc          .
           move      rf-fit-dpz-cli       to   w-acc-dpz-arc          .
       rou-let-reg-810.
      *              *-------------------------------------------------*
      *              * Normalizzazione castelletto iva                 *
      *              *-------------------------------------------------*
           move      zero                 to   w-cst-ctr-wrk          .
       rou-let-reg-820.
           add       1                    to   w-cst-ctr-wrk          .
           if        w-cst-ctr-wrk        >    6
                     go to rou-let-reg-830.
           move      zero                 to   w-acc-iva-cod
                                              (w-cst-ctr-wrk)         .
           move      zero                 to   w-acc-iva-ibv
                                              (w-cst-ctr-wrk)         .
           move      zero                 to   w-acc-iva-ibl
                                              (w-cst-ctr-wrk)         .
           move      zero                 to   w-acc-iva-imp
                                              (w-cst-ctr-wrk)         .
           go to     rou-let-reg-820.
       rou-let-reg-830.
      *              *-------------------------------------------------*
      *              * Caricamento castelletto iva                     *
      *              *-------------------------------------------------*
           move      zero                 to   w-cst-ctr-wrk          .
       rou-let-reg-840.
           add       1                    to   w-cst-ctr-wrk          .
           if        w-cst-ctr-wrk        >    6
                     go to rou-let-reg-850.
           move      rf-fit-iva-cod
                    (w-cst-ctr-wrk)       to   w-acc-iva-cod
                                              (w-cst-ctr-wrk)         .
           move      rf-fit-iva-ibl
                    (w-cst-ctr-wrk)       to   w-acc-iva-ibv
                                              (w-cst-ctr-wrk)         .
           move      rf-fit-iva-ibl
                    (w-cst-ctr-wrk)       to   w-acc-iva-ibl
                                              (w-cst-ctr-wrk)         .
           move      rf-fit-iva-imp
                    (w-cst-ctr-wrk)       to   w-acc-iva-imp
                                              (w-cst-ctr-wrk)         .
           go to     rou-let-reg-840.
       rou-let-reg-850.
      *              *-------------------------------------------------*
      *              * Determinazione totali castelletto iva           *
      *              *-------------------------------------------------*
           perform   acc-cst-iva-830      thru acc-cst-iva-839        .
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
      *              * Trattamento file [fit]                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se inserimento                              *
      *                  *---------------------------------------------*
           if        w-cnt-mfu-tip-fun    not  = "I"
                     go to scr-mov-fil-500.
           go to     scr-mov-fil-999.
       scr-mov-fil-500.
      *                  *---------------------------------------------*
      *                  * Se modifica                                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Rewrite record [fit]                    *
      *                      *-----------------------------------------*
           perform   rew-rec-fit-000      thru rew-rec-fit-999        .
      *                      *-----------------------------------------*
      *                      * Rewrite record [fir]                    *
      *                      *-----------------------------------------*
           perform   rew-rec-fir-000      thru rew-rec-fir-999        .
       scr-mov-fil-999.
           exit.

      *    *===========================================================*
      *    * Delete movimento da file                                  *
      *    *-----------------------------------------------------------*
       del-mov-fil-000.
      *              *-------------------------------------------------*
      *              * Preparazioni preliminari                        *
      *              *-------------------------------------------------*
           move      rf-fit-num-prt       to   w-rig-num-prt          .
      *              *-------------------------------------------------*
      *              * Richiesta di conferma esecuzione                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Box di avvertimento                         *
      *                  *---------------------------------------------*
           perform   box-cnf-exe-000      thru box-cnf-exe-999        .
       del-mov-fil-100.
      *              *-------------------------------------------------*
      *              * Trattamento righe                               *
      *              *-------------------------------------------------*
           if        w-acc-odc-rig        =    spaces
                     go to del-mov-fil-300.
      *                  *---------------------------------------------*
      *                  * Preparazione castelletto righe in memoria   *
      *                  *---------------------------------------------*
           perform   del-mov-fil-pcr-000  thru del-mov-fil-pcr-999    .
      *                  *---------------------------------------------*
      *                  * Cancellazione castelletto righe in memoria  *
      *                  *---------------------------------------------*
           perform   del-mov-fil-crg-000  thru del-mov-fil-crg-999    .
       del-mov-fil-300.
      *              *-------------------------------------------------*
      *              * Trattamento testata                             *
      *              *-------------------------------------------------*
           if        w-acc-odc-tes        =    spaces
                     go to del-mov-fil-900.
      *                  *---------------------------------------------*
      *                  * Delete record [fit]                         *
      *                  *---------------------------------------------*
           perform   del-rec-fit-000      thru del-rec-fit-999        .
       del-mov-fil-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     del-mov-fil-999.
       del-mov-fil-999.
           exit.

      *    *===========================================================*
      *    * Delete movimento da file                                  *
      *    *                                                           *
      *    * Subroutine di preparazione castelletto righe in memoria   *
      *    *-----------------------------------------------------------*
       del-mov-fil-pcr-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione contatore elementi              *
      *              *-------------------------------------------------*
           move      zero                 to   w-rig-num-ele          .
       del-mov-fil-pcr-100.
      *              *-------------------------------------------------*
      *              * Start su righe documento [fir]                  *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      w-rig-num-prt        to   rf-fir-num-prt         .
           move      zero                 to   rf-fir-num-prg         .
           move      "pgm/fat/fls/ioc/obj/ioffir"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fir                 .
      *                  *---------------------------------------------*
      *                  * Test su esito della start                   *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to del-mov-fil-pcr-900.
       del-mov-fil-pcr-200.
      *              *-------------------------------------------------*
      *              * Lettura sequenziale file [fir]                  *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/fat/fls/ioc/obj/ioffir"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fir                 .
      *                  *---------------------------------------------*
      *                  * Test se 'at end'                            *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to del-mov-fil-pcr-900.
       del-mov-fil-pcr-300.
      *              *-------------------------------------------------*
      *              * Test sul massimo                                *
      *              *-------------------------------------------------*
           if        rf-fir-num-prt       not  = w-rig-num-prt
                     go to del-mov-fil-pcr-900.
       del-mov-fil-pcr-400.
      *              *-------------------------------------------------*
      *              * Incremento contatore                            *
      *              *-------------------------------------------------*
           add       1                    to   w-rig-num-ele          .
      *              *-------------------------------------------------*
      *              * Test sul contatore                              *
      *              *-------------------------------------------------*
           if        w-rig-num-ele        >    w-rig-max-ele
                     go to del-mov-fil-pcr-900.
      *              *-------------------------------------------------*
      *              * Preparazione chiave                             *
      *              *-------------------------------------------------*
           move      rf-fir-num-prg       to   w-rig-key-prg
                                              (w-rig-num-ele)         .
       del-mov-fil-pcr-600.
      *              *-------------------------------------------------*
      *              * Riciclo a riga successiva                       *
      *              *-------------------------------------------------*
           go to     del-mov-fil-pcr-200.
       del-mov-fil-pcr-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     del-mov-fil-pcr-999.
       del-mov-fil-pcr-999.
           exit.

      *    *===========================================================*
      *    * Delete movimento da file                                  *
      *    *                                                           *
      *    * Subroutine di cancellazione righe                         *
      *    *-----------------------------------------------------------*
       del-mov-fil-crg-000.
      *              *-------------------------------------------------*
      *              * Test preliminare                                *
      *              *-------------------------------------------------*
           if        w-rig-num-ele        =    zero
                     go to del-mov-fil-crg-900.
      *              *-------------------------------------------------*
      *              * Normalizzazione contatore elementi              *
      *              *-------------------------------------------------*
           move      zero                 to   w-rig-ctr-ele          .
       del-mov-fil-crg-100.
       del-mov-fil-crg-200.
      *              *-------------------------------------------------*
      *              * Incremento contatore righe                      *
      *              *-------------------------------------------------*
           add       1                    to   w-rig-ctr-ele          .
      *              *-------------------------------------------------*
      *              * Test su contatore righe                         *
      *              *-------------------------------------------------*
           if        w-rig-ctr-ele        >    w-rig-num-ele
                     go to del-mov-fil-crg-900.
           if        w-rig-ctr-ele        >    w-rig-max-ele
                     go to del-mov-fil-crg-900.
           if        w-rig-key-prg
                    (w-rig-ctr-ele)       =    zero
                     go to del-mov-fil-crg-200.
       del-mov-fil-crg-400.
      *              *-------------------------------------------------*
      *              * Cancellazione                                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Delete record file righe [fir]              *
      *                  *---------------------------------------------*
           perform   del-rec-fir-000      thru del-rec-fir-999        .
      *                  *---------------------------------------------*
      *                  * Delete record file righe [fix]              *
      *                  *---------------------------------------------*
           perform   del-rec-fix-000      thru del-rec-fix-999        .
       del-mov-fil-crg-600.
      *              *-------------------------------------------------*
      *              * Riciclo a riga successiva                       *
      *              *-------------------------------------------------*
           go to     del-mov-fil-crg-200.
       del-mov-fil-crg-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     del-mov-fil-crg-999.
       del-mov-fil-crg-999.
           exit.

      *    *===========================================================*
      *    * Scrittura record [fit]                                    *
      *    *-----------------------------------------------------------*
       wrt-rec-fit-000.
       wrt-rec-fit-999.
           exit.

      *    *===========================================================*
      *    * Riscrittura record [fit]                                  *
      *    *-----------------------------------------------------------*
       rew-rec-fit-000.
      *              *-------------------------------------------------*
      *              * Identificativi                                  *
      *              *-------------------------------------------------*
           move      "IG"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-dat                to   rf-fit-ide-dat         .
           move      s-ute                to   rf-fit-ide-ute         .
           move      s-fas                to   rf-fit-ide-fas         .
      *              *-------------------------------------------------*
      *              * Codice cliente                                  *
      *              *-------------------------------------------------*
           move      w-acc-cod-arc        to   rf-fit-cod-cli         .
           move      w-acc-dpz-arc        to   rf-fit-dpz-cli         .
      *              *-------------------------------------------------*
      *              * Castelletto IVA                                 *
      *              *-------------------------------------------------*
           move      zero                 to   w-cst-ctr-wrk          .
       rew-rec-bit-200.
           add       1                    to   w-cst-ctr-wrk          .
           if        w-cst-ctr-wrk        >    6
                     go to rew-rec-bit-220.
           move      w-acc-iva-cod
                    (w-cst-ctr-wrk)       to   rf-fit-iva-cod
                                              (w-cst-ctr-wrk)         .
           move      w-acc-iva-ibl
                    (w-cst-ctr-wrk)       to   rf-fit-iva-ibl
                                              (w-cst-ctr-wrk)         .
           move      w-acc-iva-imp
                    (w-cst-ctr-wrk)       to   rf-fit-iva-imp
                                              (w-cst-ctr-wrk)         .
           go to     rew-rec-bit-200.
       rew-rec-bit-220.
           move      w-acc-iva-tdo        to   rf-fit-iva-tdo         .
      *              *-------------------------------------------------*
      *              * Forced put record                               *
      *              *-------------------------------------------------*
           move      "FP"                 to   f-ope                  .
           move      "pgm/fat/fls/ioc/obj/ioffit"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fit                 .
       rew-rec-fit-999.
           exit.

      *    *===========================================================*
      *    * Riscrittura record [fir]                                  *
      *    *-----------------------------------------------------------*
       rew-rec-fir-000.
      *              *-------------------------------------------------*
      *              * Test se record da aggiornare                    *
      *              *-------------------------------------------------*
           if        rf-fit-cod-cli       =    w-acc-cod-arc and
                     rf-fit-dpz-cli       =    w-acc-dpz-arc
                     go to rew-rec-fir-900.
      *              *-------------------------------------------------*
      *              * Preparazioni preliminari                        *
      *              *-------------------------------------------------*
           move      rf-fit-num-prt       to   w-rig-num-prt          .
       rew-rec-fir-100.
      *              *-------------------------------------------------*
      *              * Start su righe documento [fir]                  *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      w-rig-num-prt        to   rf-fir-num-prt         .
           move      zero                 to   rf-fir-num-prg         .
           move      "pgm/fat/fls/ioc/obj/ioffir"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fir                 .
      *                  *---------------------------------------------*
      *                  * Test su esito della start                   *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to rew-rec-fir-900.
       rew-rec-fir-200.
      *              *-------------------------------------------------*
      *              * Lettura sequenziale file [fir]                  *
      *              *-------------------------------------------------*
           move      "GN"                 to   f-ope                  .
           move      "pgm/fat/fls/ioc/obj/ioffir"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fir                 .
      *                  *---------------------------------------------*
      *                  * Test su flag                                *
      *                  *---------------------------------------------*
           if        f-sts                =    e-not-err
                     go to rew-rec-fir-300.
      *                  *---------------------------------------------*
      *                  * Rilascio del record                         *
      *                  *---------------------------------------------*
           move      "RL"                 to   f-ope                  .
           move      "pgm/fat/fls/ioc/obj/ioffir"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fir                 .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     rew-rec-fir-900.
       rew-rec-fir-300.
      *              *-------------------------------------------------*
      *              * Test sul massimo                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su protocollo                          *
      *                  *---------------------------------------------*
           if        rf-fir-num-prt       =    w-rig-num-prt
                     go to rew-rec-fir-400.
      *                  *---------------------------------------------*
      *                  * Rilascio del record                         *
      *                  *---------------------------------------------*
           move      "RL"                 to   f-ope                  .
           move      "pgm/fat/fls/ioc/obj/ioffir"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fir                 .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     rew-rec-fir-900.
       rew-rec-fir-400.
      *              *-------------------------------------------------*
      *              * Aggiornamento valori riga                       *
      *              *-------------------------------------------------*
           move      w-acc-cod-arc        to   rf-fir-cod-cli         .
           move      w-acc-dpz-arc        to   rf-fir-dpz-cli         .
       rew-rec-fir-600.
      *              *-------------------------------------------------*
      *              * Update record [fir]                             *
      *              *-------------------------------------------------*
           move      "UP"                 to   f-ope                  .
           move      "pgm/fat/fls/ioc/obj/ioffir"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fir                 .
      *              *-------------------------------------------------*
      *              * Rilascio del record                             *
      *              *-------------------------------------------------*
           move      "RL"                 to   f-ope                  .
           move      "pgm/fat/fls/ioc/obj/ioffir"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fir                 .
       rew-rec-fir-800.
      *              *-------------------------------------------------*
      *              * Riciclo a riga successiva                       *
      *              *-------------------------------------------------*
           go to     rew-rec-fir-200.
       rew-rec-fir-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     rew-rec-fir-999.
       rew-rec-fir-999.
           exit.

      *    *===========================================================*
      *    * Cancellazione record testata [fit]                        *
      *    *-----------------------------------------------------------*
       del-rec-fit-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-fit-000      thru cmp-rec-fit-999        .
      *              *-------------------------------------------------*
      *              * Delete record                                   *
      *              *-------------------------------------------------*
           move      "DE"                 to   f-ope                  .
           move      "pgm/fat/fls/ioc/obj/ioffit"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fit                 .
       del-rec-fit-999.
           exit.

      *    *===========================================================*
      *    * Composizione record testata [fit]                         *
      *    *-----------------------------------------------------------*
       cmp-rec-fit-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione record                          *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/fat/fls/ioc/obj/ioffit"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fit                 .
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           move      w-rig-num-prt        to   rf-fit-num-prt         .
       cmp-rec-fit-999.
           exit.

      *    *===========================================================*
      *    * Cancellazione record righe [fir]                          *
      *    *-----------------------------------------------------------*
       del-rec-fir-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-fir-000      thru cmp-rec-fir-999        .
      *              *-------------------------------------------------*
      *              * Delete record                                   *
      *              *-------------------------------------------------*
           move      "DE"                 to   f-ope                  .
           move      "pgm/fat/fls/ioc/obj/ioffir"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fir                 .
       del-rec-fir-999.
           exit.

      *    *===========================================================*
      *    * Composizione record righe [fir]                           *
      *    *-----------------------------------------------------------*
       cmp-rec-fir-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione record                          *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/fat/fls/ioc/obj/ioffir"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fir                 .
      *              *-------------------------------------------------*
      *              * Composizione chiave                             *
      *              *-------------------------------------------------*
           move      w-rig-num-prt        to   rf-fir-num-prt         .
           move      w-rig-key-prg
                    (w-rig-ctr-ele)       to   rf-fir-num-prg         .
       cmp-rec-fir-999.
           exit.

      *    *===========================================================*
      *    * Cancellazione record righe [fix]                          *
      *    *-----------------------------------------------------------*
       del-rec-fix-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-fix-000      thru cmp-rec-fix-999        .
      *              *-------------------------------------------------*
      *              * Delete record                                   *
      *              *-------------------------------------------------*
           move      "DE"                 to   f-ope                  .
           move      "pgm/fat/fls/ioc/obj/ioffix"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fix                 .
       del-rec-fix-999.
           exit.
       
      *    *===========================================================*
      *    * Composizione record righe [fix]                           *
      *    *-----------------------------------------------------------*
       cmp-rec-fix-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione record                          *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/fat/fls/ioc/obj/ioffix"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fix                 .
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           move      w-rig-num-prt        to   rf-fix-num-prt         .
           move      w-rig-key-prg
                    (w-rig-ctr-ele)       to   rf-fix-num-prg         .
           move      01                   to   rf-fix-tip-rec         .
       cmp-rec-fix-999.
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
      *    * Box di avviso per la cancellazione del documento          *
      *    *-----------------------------------------------------------*
       box-cnf-exe-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione della risposta                  *
      *              *-------------------------------------------------*
           move      "  "                 to   w-acc-odc              .
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
       box-cnf-exe-100.
      *              *-------------------------------------------------*
      *              * Box                                             *
      *              *-------------------------------------------------*
           move      "BX"                 to   v-ope                  .
           move      08                   to   v-lin                  .
           move      07                   to   v-pos                  .
           move      17                   to   v-lto                  .
           move      74                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       box-cnf-exe-200.
      *              *-------------------------------------------------*
      *              * Literals nel box                                *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      64                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      09                   to   v-pos                  .
           move      "ATTENZIONE :  E' stata richiesta la cancellazione 
      -              "del documento."     to   v-alf                  .
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
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      64                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      09                   to   v-pos                  .
           move      "N.B. : La cancellazione non coinvolge contabilita'
      -              " e portafoglio"     to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       box-cnf-exe-300.
      *              *-------------------------------------------------*
      *              * Video in On                                     *
      *              *-------------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       box-cnf-exe-400.
      *              *-------------------------------------------------*
      *              * Accettazione risposta                           *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "C"                  to   v-tip                  .
           move      w-exp-odc-doc-lun    to   v-car                  .
           move      w-exp-odc-doc-num    to   v-ldt                  .
           move      "BX"                 to   v-edm                  .
           move      13                   to   v-lin                  .
           move      23                   to   v-pos                  .
           move      w-exp-odc-doc-tbl    to   v-txt                  .
           move      w-acc-odc            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-alf                to   w-acc-odc              .
       box-cnf-exe-600.
      *              *-------------------------------------------------*
      *              * Ripristino immagine video                       *
      *              *-------------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       box-cnf-exe-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     box-cnf-exe-999.
       box-cnf-exe-999.
           exit.

      *    *===========================================================*
      *    * Find su codici iva                                        *
      *    *-----------------------------------------------------------*
       fnd-cod-iva-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-fnd-cod-iva-sel      .
      *              *-------------------------------------------------*
      *              * Test se programma di interrogazione gia' attivo *
      *              *-------------------------------------------------*
           move      "P?"                 to   s-ope                  .
           move      "pcge0410"           to   s-pro                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-liv                not  = zero
                     move  "#"            to   w-fnd-cod-iva-sel
                     go to  fnd-cod-iva-999.
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
           move      "pgm/cge/prg/obj/pcge0410"
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
           move      "cod-iva"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                not  = spaces
                     move  "#"            to   w-fnd-cod-iva-sel
                     go to fnd-cod-iva-999.
           move      s-num                to   w-fnd-cod-iva-cod      .
       fnd-cod-iva-999.
           exit.

      *    *===========================================================*
      *    * Determinazione imposta                                    *
      *    *-----------------------------------------------------------*
       det-imp-iva-000.
      *              *-------------------------------------------------*
      *              * Determinazione                                  *
      *              *-------------------------------------------------*
           move      "DI"                 to   d-imp-iva-tip-ope      .
           perform   det-imp-iva-cll-000  thru det-imp-iva-cll-999    .
       det-imp-iva-999.
           exit.

      *    *===========================================================*
      *    * Subroutines per determinazione imposta Iva in base ad un  *
      *    * imponibile                                                *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/dimpiva0.dts"                   .

