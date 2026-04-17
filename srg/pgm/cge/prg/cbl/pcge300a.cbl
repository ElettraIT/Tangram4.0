       Identification Division.
       Program-Id.                                 pcge300a           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    cge                 *
      *                                Settore:    mov                 *
      *                                   Fase:    cge300              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 01/12/89    *
      *                       Ultima revisione:    NdK del 14/11/22    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Primanota di contabilita' generale          *
      *                                                                *
      *                    Saldaconto cliente                          *
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
      *            * Per routine rou-opn-fls-000                       *
      *            *---------------------------------------------------*
               10  w-cnt-rou-opn-fls      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Per routine rou-let-reg-000                       *
      *            *---------------------------------------------------*
               10  w-cnt-rou-let-reg      pic  x(01)                  .
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
      *            *---------------------------------------------------*
      *            * Per tasto Do su saldaconto                        *
      *            *---------------------------------------------------*
               10  w-cnt-tdo-sdc-flg      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Flags di controllo su modalita' di funzionamento      *
      *        *-------------------------------------------------------*
           05  w-cnt-mfu.
      *            *---------------------------------------------------*
      *            * Tipo impostazione                                 *
      *            * - K : Impostazione campi chiave                   *
      *            * - T : Impostazione campi testata                  *
      *            * - S : Saldaconto                                  *
      *            *---------------------------------------------------*
               10  w-cnt-mfu-tip-imp      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Tipo funzione                                     *
      *            * - I : Inserimento                                 *
      *            * - M : Modifica                                    *
      *            * - V : Visualizzazione                             *
      *            *---------------------------------------------------*
               10  w-cnt-mfu-tip-fun      pic  x(01)                  .
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
      *            * Impostazione testata                              *
      *            *---------------------------------------------------*
               10  w-cnt-sts-imp-tes      pic  x(01)                  .
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
      *        * [mgr]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfmgr"                          .
      *        *-------------------------------------------------------*
      *        * [pdc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfpdc"                          .

      *    *===========================================================*
      *    * Work-area per bufferizzazione testata                     *
      *    *-----------------------------------------------------------*
       01  w-tes.
      *        *-------------------------------------------------------*
      *        * Valori attuali e precedenti                           *
      *        *-------------------------------------------------------*
           05  w-tes-val-aep occurs 2.
               10  w-tes-tip-sdc          pic  9(01)                  .
               10  w-tes-dat-ini          pic  9(07)                  .
               10  w-tes-dat-fin          pic  9(07)                  .
               10  w-tes-mes-sel          pic  9(02)                  .
               10  w-tes-imp-sdc          pic  x(01)                  .
               10  w-tes-imp-mov          pic s9(13)                  .

      *    *===========================================================*
      *    * Work per accettazioni campi espansi                       *
      *    *-----------------------------------------------------------*
       01  w-exp.
      *        *-------------------------------------------------------*
      *        * Work per : Tipo saldaconto                            *
      *        *-------------------------------------------------------*
           05  w-exp-tip-sdc.
               10  w-exp-tip-sdc-num      pic  9(02)       value 4    .
               10  w-exp-tip-sdc-lun      pic  9(02)       value 27   .
               10  w-exp-tip-sdc-tbl.
                   15  filler             pic  x(27) value
                            "Selezione manuale partite  "             .
                   15  filler             pic  x(27) value
                            "Saldo partite piu' vecchie "             .
                   15  filler             pic  x(27) value
                            "Saldo partite di un periodo"             .
                   15  filler             pic  x(27) value
                            "Saldo partite di un mese   "             .
      *        *-------------------------------------------------------*
      *        * Work per : Mese da selezionare                        *
      *        *-------------------------------------------------------*
           05  w-exp-mes-sel.
               10  w-exp-mes-sel-num      pic  9(02)       value 7    .
               10  w-exp-mes-sel-lun      pic  9(02)       value 10   .
               10  w-exp-mes-sel-tbl.
                   15  w-exp-mes-sel-tbx  occurs 7.
                       20  w-exp-mes-sel-ele  
                                          pic  x(10)                  .
                       20  w-exp-mes-sel-elx redefines
                           w-exp-mes-sel-ele.
                           25  w-exp-mes-sel-mes
                                          pic  x(03)                  .
                           25  filler     pic  x(01)                  .
                           25  w-exp-mes-sel-ann
                                          pic  9(02)                  .
                           25  filler     pic  x(04)                  .

      *    *===========================================================*
      *    * Work per subroutines di Det                               *
      *    *-----------------------------------------------------------*
       01  w-det.
      *        *-------------------------------------------------------*
      *        * Work per Det partite aperte cliente                   *
      *        *-------------------------------------------------------*
           05  w-det-pap-cli.
               10  w-det-pap-cli-cod      pic  9(07)                  .
               10  w-det-pap-cli-flg      pic  x(01)                  .
               10  w-det-pap-cli-msg      pic  x(76)                  .
      *        *-------------------------------------------------------*
      *        * Work per Det residuo operazione                       *
      *        *-------------------------------------------------------*
           05  w-det-res-ope.
               10  w-det-res-ope-par      pic s9(11)                  .
               10  w-det-res-ope-ris      pic s9(11)                  .
               10  w-det-res-ope-res      pic s9(11)                  .

      *    *===========================================================*
      *    * Work-area per salvataggi                                  *
      *    *-----------------------------------------------------------*
       01  w-sav.
           05  w-sav-tip-sdc              pic  9(01)                  .
           05  w-sav-imp-ris              pic s9(11)                  .
           05  w-sav-cto-sdo              pic  x(01)                  .

      *    *===========================================================*
      *    * Work-area per tabella partite                             *
      *    *-----------------------------------------------------------*
       01  w-par.
      *        *-------------------------------------------------------*
      *        * Saldo cliente                                         *
      *        *-------------------------------------------------------*
           05  w-par-sdo-cli              pic s9(13)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per rottura data partita                       *
      *        *-------------------------------------------------------*
           05  w-par-rot-dat              pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per rottura numero partita                     *
      *        *-------------------------------------------------------*
           05  w-par-rot-num              pic  x(10)                  .
      *        *-------------------------------------------------------*
      *        * Flag di primo passaggio                               *
      *        *-------------------------------------------------------*
           05  w-par-flg-uno              pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Numero massimo elementi in tabella                    *
      *        *-------------------------------------------------------*
           05  w-par-max-ele              pic  9(03) value 900        .
      *        *-------------------------------------------------------*
      *        * Numero elementi in tabella                            *
      *        *-------------------------------------------------------*
           05  w-par-num-ele              pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Tabella                                               *
      *        *-------------------------------------------------------*
           05  w-par-tbl-ele occurs 901.
               10  w-par-tip-doc          pic  x(03)                  .
               10  w-par-dat-rif          pic  9(07)       comp-3     .
               10  w-par-num-rif          pic  x(10)                  .
               10  w-par-res-par          pic s9(13)       comp-3     .
               10  w-par-imp-ris          pic s9(13)       comp-3     .
               10  w-par-cto-sdo          pic  x(01)                  .
               10  w-par-prg-rig          pic s9(13)       comp-3     .

      *    *===========================================================*
      *    * Work-area per tabella progressivi mensili                 *
      *    *-----------------------------------------------------------*
       01  w-prm.
           05  w-prm-tbl-ele occurs 7.
               10  w-prm-sam-prm.
                   15  w-prm-saa-prm      pic  9(03)                  .
                   15  w-prm-mes-prm      pic  9(02)                  .
               10  w-prm-sdo-prm          pic s9(13)                  .
           05  w-prm-wrk-sam.
               10  w-prm-wrk-saa          pic  9(03)                  .
               10  w-prm-wrk-mes          pic  9(02)                  .

      *    *===========================================================*
      *    * Work per trattamento tabelle                              *
      *    *-----------------------------------------------------------*
       01  w-tbl.
      *        *-------------------------------------------------------*
      *        * Work per trattamento tabella mesi                     *
      *        *-------------------------------------------------------*
           05  w-tbl-mes.
               10  filler                 pic  x(03) value "GEN"      .
               10  filler                 pic  x(03) value "FEB"      .
               10  filler                 pic  x(03) value "MAR"      .
               10  filler                 pic  x(03) value "APR"      .
               10  filler                 pic  x(03) value "MAG"      .
               10  filler                 pic  x(03) value "GIU"      .
               10  filler                 pic  x(03) value "LUG"      .
               10  filler                 pic  x(03) value "AGO"      .
               10  filler                 pic  x(03) value "SET"      .
               10  filler                 pic  x(03) value "OTT"      .
               10  filler                 pic  x(03) value "NOV"      .    
               10  filler                 pic  x(03) value "DIC"      .     
           05  w-tbl-mex redefines
               w-tbl-mes.
               10  w-tbl-mes-nox occurs 12.
                   15  w-tbl-mes-nom      pic  x(03)                  .
           05  w-tbl-mes-i01              pic  9(02)                  .
           05  w-tbl-mes-i02              pic  9(02)                  .

      *    *===========================================================*
      *    * Work-area per funzione saldaconto                         *
      *    *-----------------------------------------------------------*
       01  w-sdc.
           05  w-sdc-npg-max              pic  9(03)                  .
           05  w-sdc-npg-vis              pic  9(03)                  .
           05  w-sdc-npg-dat              pic  9(03)                  .
           05  w-sdc-ctr-rig              pic  9(03)                  .
           05  w-sdc-rig-ini              pic  9(03)                  .
           05  w-sdc-tot-ris              pic s9(11)                  .
           05  w-sdc-tot-prg              pic s9(11)                  .
           05  w-sdc-wrk-rig              pic  9(03)                  .
           05  w-sdc-wrk-lin              pic  9(03)                  .
           05  w-sdc-wrk-rem              pic  9(03)                  .
           05  w-sdc-wrk-s11              pic s9(11)                  .
           05  w-sdc-flg-imr              pic  x(01)                  .

      *    *===========================================================*
      *    * Work-area generica                                        *
      *    *-----------------------------------------------------------*
       01  w-wrk.
      *        *-------------------------------------------------------*
      *        * Flag per primo append                                 *
      *        *-------------------------------------------------------*
           05  w-wrk-flg-app              pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per residuo operazione                         *
      *        *-------------------------------------------------------*
           05  w-wrk-res-ope              pic s9(11)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per progressivo anticipi                       *
      *        *-------------------------------------------------------*
           05  w-wrk-prg-ant              pic s9(11)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per progressivo abbuoni                        *
      *        *-------------------------------------------------------*
           05  w-wrk-prg-abb              pic s9(11)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per numero progressivo riga                    *
      *        *-------------------------------------------------------*
           05  w-wrk-num-rig              pic s9(11)                  .
      *        *-------------------------------------------------------*
      *        * Contatore generico 'I'                                *
      *        *-------------------------------------------------------*
           05  I                          pic  9(03)                  .

      *    *===========================================================*
      *    * Work-area per allineamenti a destra o a sinistra oppure   *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cpw"                   .

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Work-area per bufferizzazione corpo singolo movimento     *
      *    *-----------------------------------------------------------*
       01  w-rig.
      *        *-------------------------------------------------------*
      *        * Area valori attuali e precedenti                      *
      *        *-------------------------------------------------------*
           05  w-rig-val-aep     occurs 2.
               10  w-rig-num-prg          pic  9(05)       comp-3     .
               10  w-rig-tip-con          pic  x(01)                  .
               10  w-rig-cod-con          pic  9(07)                  .
               10  w-rig-des-con          pic  x(40)                  .
               10  w-rig-cod-pdc          pic  9(07)       comp-3     .
               10  w-rig-tip-arc          pic  x(01)                  .
               10  w-rig-cod-arc          pic  9(07)       comp-3     .
               10  w-rig-com-rig          pic  x(40)                  .
               10  w-rig-dat-rif          pic  9(07)       comp-3     .
               10  w-rig-num-rif          pic  x(10)                  .
               10  w-rig-dar-ave          pic  x(01)                  .
               10  w-rig-imp-mov          pic s9(13)       comp-3     .
               10  w-rig-flg-pge          pic  x(01)                  .
               10  w-rig-flg-pcf          pic  x(01)                  .

      *    *===========================================================*
      *    * Area di comunicazione per gestione catena rig             *
      *    *-----------------------------------------------------------*
       01  w-cat-rig.
           05  w-cat-rig-ope              pic  x(02)                  .
           05  w-cat-rig-exs              pic  x(01)                  .
           05  w-cat-rig-num              pic  9(05)                  .
           05  w-cat-rig-cur              pic  9(05)                  .
           05  w-cat-rig-prg              pic  9(05)                  .
           05  w-cat-rig-max              pic  9(05)                  .
           05  w-cat-rig-app              pic  x(01)                  .
           05  w-cat-rig-ins              pic  x(01)                  .
           05  w-cat-rig-new              pic  x(01)                  .
           05  w-cat-rig-lst              pic  x(01)                  .
           05  w-cat-rig-buf.
               10  filler occurs 400      pic  x(01)                  .

      *    *===========================================================*
      *    * Work-area referenze                                       *
      *    *-----------------------------------------------------------*
       01  w-ref.
           05  w-ref-iva-ven              pic  9(07)                  .
           05  w-ref-iva-dai              pic  9(07)                  .
           05  w-ref-iva-acq              pic  9(07)                  .
           05  w-ref-iva-cai              pic  9(07)                  .
           05  w-ref-iva-cor              pic  9(07)                  .
           05  w-ref-stc-ven              pic  9(07)                  .
           05  w-ref-stc-acq              pic  9(07)                  .
           05  w-ref-stc-cor              pic  9(07)                  .
           05  w-ref-res-ven              pic  9(07)                  .
           05  w-ref-res-acq              pic  9(07)                  .
           05  w-ref-stc-cas              pic  9(07)                  .
           05  w-ref-abb-pas              pic  9(07)                  .
           05  w-ref-abb-att              pic  9(07)                  .

      *    *===========================================================*
      *    * Work-area per linkage programma gestione riferimenti      *
      *    *-----------------------------------------------------------*
       01  w-lnk.
           05  w-lnk-cod-cof              pic  9(07)                  .
           05  w-lnk-rag-cof              pic  x(40)                  .
           05  w-lnk-dat-reg              pic  9(07)                  .
           05  w-lnk-stc-cof              pic  9(07)                  .
           05  w-lnk-nrg-dac              pic  9(05)                  .
           05  w-lnk-sdo-reg              pic s9(13)                  .
           05  w-lnk-exi-sts              pic  x(01)                  .

      ******************************************************************
       Procedure Division                using w-rig
                                               w-cat-rig
                                               w-ref
                                               w-lnk                  .
      ******************************************************************

      *================================================================*
      *       Main                                                     *
      *================================================================*
       main-000.
      *              *-------------------------------------------------*
      *              * Esecuzione routine pre-esecuzione programma     *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-pre-exe-pgm      .
           perform   pre-exe-pgm-000      thru pre-exe-pgm-999        .
           if        w-cnt-pre-exe-pgm    not  = spaces
                     go to main-900.
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
      *                  * Se esito negativo : uscita                  *
      *                  *---------------------------------------------*
           if        w-cnt-rou-let-reg    not  = spaces
                     go to main-800.
      *              *-------------------------------------------------*
      *              * Routine pre-accettazione campi non chiave       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se Inserimento                              *
      *                  *---------------------------------------------*
           if        w-cnt-mfu-tip-fun    =    "I"
                     perform   pre-acc-ins-000
                                          thru pre-acc-ins-999
      *                  *---------------------------------------------*
      *                  * Se Modifica                                 *
      *                  *---------------------------------------------*
           else if   w-cnt-mfu-tip-fun    =    "M"
                     perform   pre-acc-mod-000
                                          thru pre-acc-mod-999
      *                  *---------------------------------------------*
      *                  * Se Visualizzazione                          *
      *                  *---------------------------------------------*
           else if   w-cnt-mfu-tip-fun    =    "V"
                     perform   pre-acc-vis-000
                                          thru pre-acc-vis-999
      *                  *---------------------------------------------*
      *                  * Se tipo funzionamento errato : uscita       *
      *                  *---------------------------------------------*
           else      go to main-800.
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
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     main-800.
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
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     main-800.
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
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     main-800.
       main-800.
      *              *-------------------------------------------------*
      *              * Close files                                     *
      *              *-------------------------------------------------*
           perform   rou-cls-fls-000      thru rou-cls-fls-999        .
      *              *-------------------------------------------------*
      *              * Esecuzione routine post-esecuzione programma    *
      *              *-------------------------------------------------*
           perform   pos-exe-pgm-000      thru pos-exe-pgm-999        .
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
      *              * Tasto di funzione Pf3 : sempre abilitato        *
      *              *-------------------------------------------------*
           move      "[3] "               to   v-pfk (17)             .
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
      *              * Se Pf3                                          *
      *              *-------------------------------------------------*
           if        v-key                not  = "[3] "
                     go to exe-acc-cmp-800.
           perform   pf3-cge-300-000      thru pf3-cge-300-999        .
           go to     exe-acc-cmp-400.
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
       exe-acc-cmp-999.
           exit.

      *    *===========================================================*
      *    * Tasto funzione "Pf3" disponibile ad ogni impostazione     *
      *    *-----------------------------------------------------------*
       pf3-cge-300-000.
      *              *-------------------------------------------------*
      *              * Test se programma di interrogazione gia' attivo *
      *              *-------------------------------------------------*
           move      "P?"                 to   s-ope                  .
           move      "pcge3010"           to   s-pro                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-liv                not  = zero
                     go to  pf3-cge-300-999.
      *              *-------------------------------------------------*
      *              * Richiamo programma di interrogazione            *
      *              *-------------------------------------------------*
           move      "pgm/cge/prg/obj/pcge3010"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat                                            .
           cancel    s-pat                                            .
       pf3-cge-300-999.
           exit.

      *    *===========================================================*
      *    * Dichiarazione di inizio programma                         *
      *    *-----------------------------------------------------------*
       dic-ini-pgm-000.
       dic-ini-pgm-999.
           exit.

      *    *===========================================================*
      *    * Dichiarazione di fine programma                           *
      *    *-----------------------------------------------------------*
       dic-fin-pgm-000.
       dic-fin-pgm-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione titolo programma                          *
      *    *-----------------------------------------------------------*
       vis-tit-pgm-000.
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
      *              * Preparazione tabella progressivi mensili        *
      *              *-------------------------------------------------*
           move      w-lnk-dat-reg        to   s-dat                  .
           move      s-mes                to   w-prm-wrk-mes          .
           move      s-saa                to   w-prm-wrk-saa          .
           move      7                    to   I                      .
       pre-exe-pgm-100.
           move      w-prm-wrk-saa        to   w-prm-saa-prm (I)      .
           move      w-prm-wrk-mes        to   w-prm-mes-prm (I)      .
           move      zero                 to   w-prm-sdo-prm (I)      .
           subtract  1                    from I                      .
           if        I                    =    zero
                     go to  pre-exe-pgm-200.
           subtract  1                    from w-prm-wrk-mes          .
           if        w-prm-wrk-mes        =    zero
                     move     12          to   w-prm-wrk-mes
                     subtract 1           from w-prm-wrk-saa          .
           go to     pre-exe-pgm-100.
       pre-exe-pgm-200.
      *              *-------------------------------------------------*
      *              * Preparazione tabella per accettazione pop-up me-*
      *              * se da selezionare                               *
      *              *-------------------------------------------------*
           move      zero                 to   I                      .
       pre-exe-pgm-300.
           add       1                    to   I                      .
           if        I                    >    7
                     go to  pre-exe-pgm-400.
           move      spaces               to   w-exp-mes-sel-ele (I)  .
           if        I                    =    1
                     move   "Anteriori "  to   w-exp-mes-sel-ele (I)
                     go to  pre-exe-pgm-300.
           move      w-prm-mes-prm (I)    to   w-prm-wrk-mes          .
           move      w-tbl-mes-nom
                    (w-prm-wrk-mes)       to   w-exp-mes-sel-mes (I)  .
           move      w-prm-saa-prm (I)    to   w-exp-mes-sel-ann (I)  .
           go to     pre-exe-pgm-300.
       pre-exe-pgm-400.
      *                  *---------------------------------------------*
      *                  * Determinazione partite aperte cliente       *
      *                  *---------------------------------------------*
           move      w-lnk-cod-cof        to   w-det-pap-cli-cod      .
           perform   det-pap-cli-000      thru det-pap-cli-999        .
           if        w-det-pap-cli-flg    =    spaces
                     go to pre-exe-pgm-500.
      *                      *-----------------------------------------*
      *                      * Messaggio di errore                     *
      *                      *-----------------------------------------*
           move      "ME"                 to   v-ope                  .
           move      w-det-pap-cli-msg    to   v-not                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Uscita con errore                       *
      *                      *-----------------------------------------*
           move      "#"                  to   w-cnt-pre-exe-pgm      .
           go to     pre-exe-pgm-999.
       pre-exe-pgm-500.
      *              *-------------------------------------------------*
      *              * Intestazione cliente                            *
      *              *-------------------------------------------------*
           move      "EL"                 to   v-ope                  .
           move      04                   to   v-lin                  .
           move      05                   to   v-lto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      22                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Saldaconto cliente   :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      w-lnk-cod-cof        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      48                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      24                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           string    v-edt
                                delimited by   spaces
                     " "
                                delimited by   size
                     w-lnk-rag-cof
                                delimited by   size
                                          into v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Trattini a linea 05                             *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      all   "-"            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pre-exe-pgm-999.
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
       rou-opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Close files                                               *
      *    *-----------------------------------------------------------*
       rou-cls-fls-000.
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
           move      spaces               to   w-cnt-sts-imp-key
                                               w-cnt-sts-imp-tes      .
      *              *-------------------------------------------------*
      *              * Normalizzazione status visualizzazione prompts  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-sts-pmt-key
                                               w-cnt-sts-pmt-tes      .
      *              *-------------------------------------------------*
      *              * Normalizzazione status visualizzazione dati     *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-sts-vis-key
                                               w-cnt-sts-vis-tes      .
      *              *-------------------------------------------------*
      *              * Normalizzazione registrazione                   *
      *              *-------------------------------------------------*
           perform   nor-key-nok-000      thru nor-key-nok-999        .
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
                     go to acc-key-reg-000.
           move      "S"                  to   w-cnt-tus-acc-key      .
       acc-key-reg-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campi chiave della registrazione          *
      *    *-----------------------------------------------------------*
       vis-key-reg-000.
       vis-key-reg-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per campi chiave                  *
      *    *-----------------------------------------------------------*
       pmt-key-reg-000.
       pmt-key-reg-999.
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
                     move  "#"            to   w-cnt-sts-imp-tes      .
       acc-nok-reg-200.
      *              *-------------------------------------------------*
      *              * Trattamento testata                             *
      *              *-------------------------------------------------*
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
      *                  * Prompts per testata                         *
      *                  *---------------------------------------------*
           if        w-cnt-sts-pmt-tes    =    spaces
                     perform pmt-tes-reg-000
                                          thru pmt-tes-reg-999
                     move    "#"          to   w-cnt-sts-pmt-tes      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione dati testata                *
      *                  *---------------------------------------------*
           if        w-cnt-mfu-tip-fun    =    "M"   or
                     w-cnt-mfu-tip-fun    =    "V"   or
                     w-cnt-sts-imp-tes    not  = spaces
                     if    w-cnt-sts-vis-tes
                                          =    spaces
                           perform vis-tes-reg-000
                                          thru vis-tes-reg-999
                           move    "#"    to   w-cnt-sts-vis-tes      .
      *                  *---------------------------------------------*
      *                  * Video in 'ON'                               *
      *                  *---------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-nok-reg-300.
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
           perform   cnt-tdo-sdc-000      thru cnt-tdo-sdc-999        .
           if        w-cnt-tdo-sdc-flg    =    spaces
                     move  "S"            to   w-cnt-tus-acc-nok
                     go to acc-nok-reg-999
           else      move  spaces         to   w-cnt-tdo-sdc-flg
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
           go to     acc-nok-reg-300.
       acc-nok-reg-999.
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
      *              * Se tipo impostazione saldaconto ad accettazione *
      *              * saldaconto                                      *
      *              *-------------------------------------------------*
           if        w-cnt-mfu-tip-imp    =    "S"
                     go to acc-tes-reg-800.
      *              *-------------------------------------------------*
      *              * Tipo impostazione : testata                     *
      *              *-------------------------------------------------*
           move      "T"                  to   w-cnt-mfu-tip-imp      .
       acc-tes-reg-100.
      *              *-------------------------------------------------*
      *              * Accettazioni                                    *
      *              *-------------------------------------------------*
           move      spaces               to   v-key                  .
      *              *-------------------------------------------------*
      *              * Visualizzazione progressivi mensili             *
      *              *-------------------------------------------------*
           perform   vis-prg-men-000      thru vis-prg-men-999        .
      *              *-------------------------------------------------*
      *              * Visualizzazione totale progressivi mensili      *
      *              *-------------------------------------------------*
           perform   vis-prm-tot-000      thru vis-prm-tot-999        .
       acc-tes-reg-200.
      *              *-------------------------------------------------*
      *              * Tipo saldaconto                                 *
      *              *-------------------------------------------------*
           perform   acc-tip-sdc-000      thru acc-tip-sdc-999        .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
       acc-tes-reg-300.
      *              *-------------------------------------------------*
      *              * Importo da assegnare                            *
      *              *-------------------------------------------------*
           perform   acc-imp-das-000      thru acc-imp-das-999        .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-200.
       acc-tes-reg-400.
      *              *-------------------------------------------------*
      *              * Data iniziale                                   *
      *              *-------------------------------------------------*
           perform   acc-dat-ini-000      thru acc-dat-ini-999        .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-300.
       acc-tes-reg-500.
      *              *-------------------------------------------------*
      *              * Data finale                                     *
      *              *-------------------------------------------------*
           perform   acc-dat-fin-000      thru acc-dat-fin-999        .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-400.
       acc-tes-reg-600.
      *              *-------------------------------------------------*
      *              * Mese saldaconto                                 *
      *              *-------------------------------------------------*
           perform   acc-mes-sel-000      thru acc-mes-sel-999        .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-500.
       acc-tes-reg-700.
      *              *-------------------------------------------------*
      *              * Conferma impostazioni pagina richieste          *
      *              *-------------------------------------------------*
           perform   acc-cnf-imp-000      thru acc-cnf-imp-999        .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-100.
      *              *-------------------------------------------------*
      *              * Tipo impostazione : saldaconto                  *
      *              *-------------------------------------------------*
           move      "S"                  to   w-cnt-mfu-tip-imp      .
       acc-tes-reg-800.
      *              *-------------------------------------------------*
      *              * Accettazione funzione saldaconto                *
      *              *-------------------------------------------------*
           perform   acc-fun-sdc-000      thru acc-fun-sdc-999        .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
       acc-tes-reg-900.
      *              *-------------------------------------------------*
      *              * Assestamento status di uscita                   *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     move   "S"           to   w-cnt-tus-acc-tes
           else if   v-key                =    "DELT"
                     move   "X"           to   w-cnt-tus-acc-tes
           else if   v-key                =    "EXIT"
                     move   "E"           to   w-cnt-tus-acc-tes
           else      move   "+"           to   w-cnt-tus-acc-tes      .
       acc-tes-reg-990.
      *              *-------------------------------------------------*
      *              * Flag di status impostazione testata             *
      *              *-------------------------------------------------*
           move      "#"                  to   w-cnt-sts-imp-tes      .
      *              *-------------------------------------------------*
      *              * Flag di status visualizzazione dati testata     *
      *              *-------------------------------------------------*
           move      "#"                  to   w-cnt-sts-vis-tes      .
       acc-tes-reg-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione testata registrazione                     *
      *    *-----------------------------------------------------------*
       vis-tes-reg-000.
      *              *-------------------------------------------------*
      *              * Tipo saldaconto                                 *
      *              *-------------------------------------------------*
           perform   vis-tip-sdc-000      thru vis-tip-sdc-999        .
      *              *-------------------------------------------------*
      *              * Importo da assegnare                            *
      *              *-------------------------------------------------*
           perform   vis-imp-das-000      thru vis-imp-das-999        .
      *              *-------------------------------------------------*
      *              * Data iniziale                                   *
      *              *-------------------------------------------------*
           perform   vis-dat-ini-000      thru vis-dat-ini-999        .
      *              *-------------------------------------------------*
      *              * Data finale                                     *
      *              *-------------------------------------------------*
           perform   vis-dat-fin-000      thru vis-dat-fin-999        .
      *              *-------------------------------------------------*
      *              * Mese saldaconto da selezionare                  *
      *              *-------------------------------------------------*
           perform   vis-mes-sel-000      thru vis-mes-sel-999        .
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
      *              * Tabella progressivi mensili                     *
      *              *-------------------------------------------------*
           perform   pmt-tbl-prm-000      thru pmt-tbl-prm-999        .
      *              *-------------------------------------------------*
      *              * Tipo saldaconto                                 *
      *              *-------------------------------------------------*
           perform   pmt-tip-sdc-000      thru pmt-tip-sdc-999        .
      *              *-------------------------------------------------*
      *              * Importo da assegnare                            *
      *              *-------------------------------------------------*
           perform   pmt-imp-das-000      thru pmt-imp-das-999        .
      *              *-------------------------------------------------*
      *              * Data iniziale                                   *
      *              *-------------------------------------------------*
           perform   pmt-dat-ini-000      thru pmt-dat-ini-999        .
      *              *-------------------------------------------------*
      *              * Data finale                                     *
      *              *-------------------------------------------------*
           perform   pmt-dat-fin-000      thru pmt-dat-fin-999        .
      *              *-------------------------------------------------*
      *              * Mese da selezionare                             *
      *              *-------------------------------------------------*
           perform   pmt-mes-sel-000      thru pmt-mes-sel-999        .
       pmt-tes-reg-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Tabella progressivi mensili      *
      *    *-----------------------------------------------------------*
       pmt-tbl-prm-000.
      *              *-------------------------------------------------*
      *              * Intestazione                                    *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      26                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      55                   to   v-pos                  .
           move      "--------------------------"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      26                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      55                   to   v-pos                  .
           move      " Situazione delle partite "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      26                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      55                   to   v-pos                  .
           move      "--------------------------"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Tabella mesi                                    *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      55                   to   v-pos                  .
           move      "Anteriori :"        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      1                    to   I                      .
       pmt-tbl-prm-100.
           add       1                    to   I                      .
           if        I                    >    7
                     go to  pmt-tbl-prm-200.
      *                  *---------------------------------------------*
      *                  * Nome del mese                               *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           add       10
                     I                  giving v-lin                  .
           move      55                   to   v-pos                  .
           move      w-prm-mes-prm (I)    to   w-tbl-mes-i01          .
           move      w-tbl-mes-nom
                    (w-tbl-mes-i01)       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Anno                                        *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           add       10
                     I                  giving v-lin                  .
           move      59                   to   v-pos                  .
           move      w-prm-saa-prm (I)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Due punti                                   *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      01                   to   v-car                  .
           add       10
                     I                  giving v-lin                  .
           move      65                   to   v-pos                  .
           move      ":"                  to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Riciclo su elemento tabella successivo      *
      *                  *---------------------------------------------*
           go to     pmt-tbl-prm-100.
       pmt-tbl-prm-200.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      26                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      55                   to   v-pos                  .
           move      "--------------------------"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      55                   to   v-pos                  .
           move      "Totale    :"        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-tbl-prm-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Tipo saldaconto                  *
      *    *-----------------------------------------------------------*
       pmt-tip-sdc-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      22                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tipo di saldaconto   :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-tip-sdc-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Importo da assegnare             *
      *    *-----------------------------------------------------------*
       pmt-imp-das-000.
           if        w-tes-tip-sdc (1)    not  = 2
                     go to pmt-imp-das-999.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      22                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Importo da assegnare :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-imp-das-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Data iniziale                    *
      *    *-----------------------------------------------------------*
       pmt-dat-ini-000.
           if        w-tes-tip-sdc (1)    not  = 3
                     go to pmt-dat-ini-999.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      22                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Data iniziale        :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-dat-ini-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Data finale                      *
      *    *-----------------------------------------------------------*
       pmt-dat-fin-000.
           if        w-tes-tip-sdc (1)    not  = 3
                     go to pmt-dat-fin-999.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      22                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Data finale          :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-dat-fin-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Mese da selezionare              *
      *    *-----------------------------------------------------------*
       pmt-mes-sel-000.
           if        w-tes-tip-sdc (1)    not  = 4
                     go to pmt-mes-sel-999.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      22                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Mese da selezionare  :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-mes-sel-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Funzione saldaconto              *
      *    *-----------------------------------------------------------*
       pmt-fun-sdc-000.
      *              *-------------------------------------------------*
      *              * Erase linee impegnate                           *
      *              *-------------------------------------------------*
           move      "EL"                 to   v-ope                  .
           move      06                   to   v-lin                  .
           move      21                   to   v-lto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Intestazione colonne                            *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tipo  Data   Nr. docum. | Residuo partita |Importo
      -              " riscosso |C/S|    Progressivo"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Colonne per saldaconto                          *
      *              *-------------------------------------------------*
           move      zero                 to   I                      .
       pmt-fun-sdc-100.
           add       1                    to   I                      .
           if        I                    >    13
                     go to pmt-fun-sdc-200.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      41                   to   v-car                  .
           add       6
                     I                  giving v-lin                  .
           move      25                   to   v-pos                  .
           move      "|                 |                 |   |"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           go to     pmt-fun-sdc-100.
       pmt-fun-sdc-200.
      *              *-------------------------------------------------*
      *              * Trattini a linea 20                             *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      all   "-"            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Colonne per totali a pie' video                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      23                   to   v-car                  .
           move      21                   to   v-lin                  .
           move      43                   to   v-pos                  .
           move      "|                 |   |"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-fun-sdc-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione progressivi mensili                       *
      *    *-----------------------------------------------------------*
       vis-prg-men-000.
           move      zero                 to   I                      .
       vis-prg-men-100.
           add       1                    to   I                      .
           if        I                    >    7
                     go to  vis-prg-men-999.
           move      "DS"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "G"                  to   v-edm                  .
           add       10
                     I                  giving v-lin                  .
           move      66                   to   v-pos                  .
           move      w-prm-sdo-prm (I)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           go to     vis-prg-men-100.
       vis-prg-men-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione totale progressivi mensili                *
      *    *-----------------------------------------------------------*
       vis-prm-tot-000.
           move      "DS"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "G"                  to   v-edm                  .
           move      19                   to   v-lin                  .
           move      66                   to   v-pos                  .
           move      w-par-sdo-cli        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-prm-tot-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Tipo saldaconto              *
      *    *-----------------------------------------------------------*
       acc-tip-sdc-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      w-tes-tip-sdc (1)    to   w-sav-tip-sdc          .
       acc-tip-sdc-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-sdc-lun    to   v-car                  .
           move      w-exp-tip-sdc-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      10                   to   v-lin                  .
           move      24                   to   v-pos                  .
           move      w-exp-tip-sdc-tbl    to   v-txt                  .
           move      w-tes-tip-sdc (1)    to   v-num                  .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-tes    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-tip-sdc-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-tip-sdc-999.
       acc-tip-sdc-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-tip-sdc (1)      .
       acc-tip-sdc-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se zero : reimpostazione                    *
      *                  *---------------------------------------------*
           if        w-tes-tip-sdc (1)    =    zero
                     go to acc-tip-sdc-100.
       acc-tip-sdc-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
           if        w-sav-tip-sdc        =    w-tes-tip-sdc (1)
                     go to  acc-tip-sdc-800.
      *                  *---------------------------------------------*
      *                  * Abblencamento area prompt di selezione      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione prompt di selezione         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazione valori di selezione     *
      *                      *-----------------------------------------*
           move      zero                 to   w-tes-imp-mov (1)      .
           move      zero                 to   w-tes-dat-ini (1)      .
           move      zero                 to   w-tes-dat-fin (1)      .
           move      zero                 to   w-tes-mes-sel (1)      .
      *                      *-----------------------------------------*
      *                      * Importo da assegnare                    *
      *                      *-----------------------------------------*
           perform   pmt-imp-das-000      thru pmt-imp-das-999        .
      *                      *-----------------------------------------*
      *                      * Data iniziale                           *
      *                      *-----------------------------------------*
           perform   pmt-dat-ini-000      thru pmt-dat-ini-999        .
      *                      *-----------------------------------------*
      *                      * Data finale                             *
      *                      *-----------------------------------------*
           perform   pmt-dat-fin-000      thru pmt-dat-fin-999        .
      *                      *-----------------------------------------*
      *                      * Mese da selezionare                     *
      *                      *-----------------------------------------*
           perform   pmt-mes-sel-000      thru pmt-mes-sel-999        .
       acc-tip-sdc-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-tip-sdc-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-tip-sdc-100.
       acc-tip-sdc-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Tipo saldaconto           *
      *    *-----------------------------------------------------------*
       vis-tip-sdc-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-sdc-lun    to   v-car                  .
           move      w-exp-tip-sdc-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      10                   to   v-lin                  .
           move      24                   to   v-pos                  .
           move      w-exp-tip-sdc-tbl    to   v-txt                  .
           move      w-tes-tip-sdc (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tip-sdc-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Importo da assegnare         *
      *    *-----------------------------------------------------------*
       acc-imp-das-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-tes-tip-sdc (1)    not  = 2
                     go to acc-imp-das-999.
       acc-imp-das-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "<GB"                to   v-edm                  .
           move      12                   to   v-lin                  .
           move      24                   to   v-pos                  .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-tes    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      w-tes-imp-mov (1)    to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-imp-das-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-imp-das-999.
       acc-imp-das-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-imp-mov (1)      .
       acc-imp-das-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore impostato zero : reimpostazione   *
      *                  *---------------------------------------------*
           if        w-tes-imp-mov (1)    =    zero
                     go to acc-imp-das-100.
       acc-imp-das-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-imp-das-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-imp-das-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-imp-das-100.
       acc-imp-das-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Importo riscosso          *
      *    *-----------------------------------------------------------*
       vis-imp-das-000.
           move      "DS"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "<GB"                to   v-edm                  .
           move      12                   to   v-lin                  .
           move      24                   to   v-pos                  .
           move      w-tes-imp-mov (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-imp-das-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Data iniziale                *
      *    *-----------------------------------------------------------*
       acc-dat-ini-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-tes-tip-sdc (1)    not  = 3
                     go to acc-dat-ini-999.
       acc-dat-ini-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      12                   to   v-lin                  .
           move      24                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           if        w-cnt-sts-imp-tes    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      w-tes-dat-ini (1)    to   v-dat                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-dat-ini-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-dat-ini-999.
       acc-dat-ini-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-dat                to   w-tes-dat-ini (1)      .
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
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-dat-ini-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-dat-ini-100.
       acc-dat-ini-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Data iniziale             *
      *    *-----------------------------------------------------------*
       vis-dat-ini-000.
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      12                   to   v-lin                  .
           move      24                   to   v-pos                  .
           move      w-tes-dat-ini (1)    to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dat-ini-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Data finale                  *
      *    *-----------------------------------------------------------*
       acc-dat-fin-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-tes-tip-sdc (1)    not  = 3
                     go to acc-dat-fin-999.
       acc-dat-fin-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      13                   to   v-lin                  .
           move      24                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           if        w-cnt-sts-imp-tes    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      w-tes-dat-fin (1)    to   v-dat                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-dat-fin-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-dat-fin-999.
       acc-dat-fin-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-dat                to   w-tes-dat-fin (1)      .
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
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-dat-fin-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-dat-fin-100.
       acc-dat-fin-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Data finale               *
      *    *-----------------------------------------------------------*
       vis-dat-fin-000.
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      13                   to   v-lin                  .
           move      24                   to   v-pos                  .
           move      w-tes-dat-fin (1)    to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dat-fin-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Mese da selezionare          *
      *    *-----------------------------------------------------------*
       acc-mes-sel-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-tes-tip-sdc (1)    not  = 4
                     go to acc-mes-sel-999.
       acc-mes-sel-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-mes-sel-lun    to   v-car                  .
           move      w-exp-mes-sel-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      11                   to   v-lin                  .
           move      55                   to   v-pos                  .
           move      w-exp-mes-sel-tbl    to   v-txt                  .
           move      zero                 to   v-num                  .
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
                     go to acc-mes-sel-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-mes-sel-999.
       acc-mes-sel-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-mes-sel (1)      .
       acc-mes-sel-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se zero : reimpostazione                    *
      *                  *---------------------------------------------*
           if        w-tes-mes-sel (1)    =    zero
                     go to acc-mes-sel-100.
       acc-mes-sel-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ripristino prima riga progressivi mensili   *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      55                   to   v-pos                  .
           move      "Anteriori :"        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione mese selezionato dopo prompt*
      *                  *---------------------------------------------*
           perform   vis-mes-sel-000      thru vis-mes-sel-999        .
       acc-mes-sel-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-mes-sel-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-mes-sel-100.
       acc-mes-sel-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Mese da selezionare       *
      *    *-----------------------------------------------------------*
       vis-mes-sel-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      10                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      24                   to   v-pos                  .
           move      w-tes-mes-sel (1)    to   w-prm-wrk-mes          .
           move      w-exp-mes-sel-ele
                    (w-prm-wrk-mes)       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-mes-sel-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo conferma impostazione richieste        *
      *    *-----------------------------------------------------------*
       acc-cnf-imp-000.
       acc-cnf-imp-100.
      *              *-------------------------------------------------*
      *              * Accettazione                                    *
      *              *-------------------------------------------------*
           move      "MX"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      "#SAV"               to   v-not                  .
           move      "S"                  to   v-alf                  .
           move      "SNE"                to   v-msk                  .
           move      "DO  "               to   v-pfk (05)             .
           move      "UP  "               to   v-pfk (01)             .
           move      "EXIT"               to   v-pfk (20)             .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Normalizzazione risposta dell'utente            *
      *              *-------------------------------------------------*
           if        v-key                not  = spaces
                     go to  acc-cnf-imp-200.
           if        v-alf                =    "S"
                     move   "DO  "        to   v-key
           else if   v-alf                =    "E"
                     move   "EXIT"        to   v-key
           else if   v-alf                =    "N"
                     move   "UP  "        to   v-key                  .
       acc-cnf-imp-200.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-cnf-imp-999.
      *              *-------------------------------------------------*
      *              * Se Up : uscita                                  *
      *              *-------------------------------------------------*
           if        v-key                =    "UP  "
                     go to acc-cnf-imp-999.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           perform   cnt-tdo-nok-000      thru cnt-tdo-nok-999        .
           if        w-cnt-tdo-nok-flg    =    spaces
                     move  "S"            to   w-cnt-tus-acc-nok
                     go to acc-cnf-imp-999
           else      move  spaces         to   w-cnt-tdo-nok-flg
                     go to acc-cnf-imp-100.
       acc-cnf-imp-999.
           exit.

      *    *===========================================================*
      *    * Accettazioni per funzione saldaconto                      *
      *    *-----------------------------------------------------------*
       acc-fun-sdc-000.
      *              *-------------------------------------------------*
      *              * Se impostazione saldaconto gia' avvenuta        *
      *              *-------------------------------------------------*
           if        w-tes-imp-sdc (1)    =    spaces
                     go to  acc-fun-sdc-100.
           multiply  12                   by   w-sdc-npg-dat
                                        giving w-sdc-ctr-rig          .
           subtract  11                   from w-sdc-ctr-rig          .
           go to     acc-fun-sdc-300.
       acc-fun-sdc-100.
      *              *-------------------------------------------------*
      *              * Determinazione numero massimo pagine            *
      *              *-------------------------------------------------*
           move      w-par-num-ele        to   w-sdc-npg-max          .
           divide    12                   into w-sdc-npg-max
                                        giving w-sdc-npg-max
                                     remainder w-sdc-wrk-rem          .
           if        w-sdc-wrk-rem        >    zero
                     add    1             to   w-sdc-npg-max          .
      *              *-------------------------------------------------*
      *              * Selezione preventiva : solo se in inserimento   *
      *              *-------------------------------------------------*
           if        w-cnt-mfu-tip-fun    =    "I"
                     perform sel-pre-sdc-000
                                          thru sel-pre-sdc-999        .
      *              *-------------------------------------------------*
      *              * Visualizzazione prompts                         *
      *              *-------------------------------------------------*
           perform   pmt-fun-sdc-000      thru pmt-fun-sdc-999        .
      *              *-------------------------------------------------*
      *              * Numero pagina visualizzata                      *
      *              *-------------------------------------------------*
           move      zero                 to   w-sdc-npg-vis          .
      *              *-------------------------------------------------*
      *              * Numero riga da trattare                         *
      *              *-------------------------------------------------*
           move      w-sdc-rig-ini        to   w-sdc-ctr-rig          .
       acc-fun-sdc-200.
           add       1                    to   w-sdc-ctr-rig          .
           if        w-sdc-ctr-rig        >    w-par-num-ele
                     go to  acc-fun-sdc-900.
       acc-fun-sdc-300.
      *              *-------------------------------------------------*
      *              * Determinazione numero pagina da trattare        *
      *              *-------------------------------------------------*
           divide    12                   into w-sdc-ctr-rig
                                        giving w-sdc-npg-dat
                                     remainder w-sdc-wrk-rem          .
           if        w-sdc-wrk-rem        >    zero
                     add    1             to   w-sdc-npg-dat          .
      *              *-------------------------------------------------*
      *              * Visualizzazione totale riscosso e totale pro-   *
      *              * gressivo                                        *
      *              *-------------------------------------------------*
           if        w-sdc-npg-vis        =    zero
                     perform vis-tot-ris-000
                                          thru vis-tot-ris-999
                     perform vis-tot-prg-000
                                          thru vis-tot-prg-999        .
      *              *-------------------------------------------------*
      *              * Visualizzazione pagina da trattare              *
      *              *-------------------------------------------------*
           if        w-sdc-npg-dat        not  = w-sdc-npg-vis
                     perform vis-pag-sdc-000
                                          thru vis-pag-sdc-999        .
      *              *-------------------------------------------------*
      *              * Determinazione numero linea                     *
      *              *-------------------------------------------------*
           subtract  1                    from w-sdc-npg-dat
                                        giving w-sdc-wrk-rig          .
           multiply  12                   by   w-sdc-wrk-rig          .
           subtract  w-sdc-wrk-rig        from w-sdc-ctr-rig
                                        giving w-sdc-wrk-lin          .
           add       7                    to   w-sdc-wrk-lin          .
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di impostazione riga       *
      *              *-------------------------------------------------*
           move      spaces               to   w-sdc-flg-imr          .
       acc-fun-sdc-400.
      *              *-------------------------------------------------*
      *              * Accettazione riga saldaconto                    *
      *              *-------------------------------------------------*
           move      spaces               to   v-key                  .
      *                  *---------------------------------------------*
      *                  * Importo riscosso                            *
      *                  *---------------------------------------------*
           perform   acc-imp-ris-000      thru acc-imp-ris-999        .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-fun-sdc-900.
      *                  *---------------------------------------------*
      *                  * Test su tipo uscita                         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se Up                                   *
      *                      *-----------------------------------------*
           if        v-key                =    "UP  "
                     subtract 1           from w-sdc-ctr-rig
                     go to  acc-fun-sdc-300.
      *                      *-----------------------------------------*
      *                      * Se Down                                 *
      *                      *-----------------------------------------*
           if        v-key                =    "DOWN"
                     go to  acc-fun-sdc-200.
      *                      *-----------------------------------------*
      *                      * Se Next Screen                          *
      *                      *-----------------------------------------*
           if        v-key                not  = "NXSC"
                     go to  acc-fun-sdc-500.
           multiply  12                   by   w-sdc-npg-dat
                                        giving w-sdc-ctr-rig          .
           add       1                    to   w-sdc-ctr-rig          .
           go to     acc-fun-sdc-300.
       acc-fun-sdc-500.
      *                      *-----------------------------------------*
      *                      * Se Prev Screen                          *
      *                      *-----------------------------------------*
           if        v-key                not  = "PRSC"
                     go to  acc-fun-sdc-600.
           subtract  1                    from w-sdc-npg-dat          .
           multiply  12                   by   w-sdc-npg-dat
                                        giving w-sdc-ctr-rig          .
           subtract  11                   from w-sdc-ctr-rig          .
           go to     acc-fun-sdc-300.
       acc-fun-sdc-600.
      *                      *-----------------------------------------*
      *                      * Se Tab                                  *
      *                      *-----------------------------------------*
           if        v-key                =    "TAB "
                     move   w-par-num-ele to   w-sdc-ctr-rig
                     go to  acc-fun-sdc-300.
      *                      *-----------------------------------------*
      *                      * Se Back                                 *
      *                      *-----------------------------------------*
           if        v-key                =    "BACK"
                     move   1             to   w-sdc-ctr-rig
                     go to  acc-fun-sdc-300.
      *              *-------------------------------------------------*
      *              * Attivazione flag di impostazione riga           *
      *              *-------------------------------------------------*
           move      "#"                  to   w-sdc-flg-imr          .
      *              *-------------------------------------------------*
      *              * Flag di in conto o a saldo                      *
      *              *-------------------------------------------------*
           perform   acc-cto-sdo-000      thru acc-cto-sdo-999        .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-fun-sdc-900.
           if        v-key                =    "UP  "
                     go to acc-fun-sdc-400.
      *              *-------------------------------------------------*
      *              * Riciclo a riga successiva                       *
      *              *-------------------------------------------------*
           go to     acc-fun-sdc-200.
       acc-fun-sdc-900.
      *              *-------------------------------------------------*
      *              * Attivazione flag di saldaconto impostato        *
      *              *-------------------------------------------------*
           move      "#"                  to   w-tes-imp-sdc (1)      .
       acc-fun-sdc-999.
           exit.

      *    *===========================================================*
      *    * Selezione preventiva per funzione saldaconto              *
      *    *-----------------------------------------------------------*
       sel-pre-sdc-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione totale riscosso                 *
      *              *-------------------------------------------------*
           move      zero                 to   w-sdc-tot-ris          .
      *              *-------------------------------------------------*
      *              * Normalizzazione totale progressivo              *
      *              *-------------------------------------------------*
           move      zero                 to   w-sdc-tot-prg          .
      *              *-------------------------------------------------*
      *              * Determinazione numero riga iniziale in funzione *
      *              * del tipo saldaconto                             *
      *              *-------------------------------------------------*
           go to     sel-pre-sdc-100
                     sel-pre-sdc-200
                     sel-pre-sdc-300
                     sel-pre-sdc-400
                     depending            on   w-tes-tip-sdc (1)      .
       sel-pre-sdc-100.
      *              *-------------------------------------------------*
      *              * Tipo saldaconto : manuale                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Riga iniziale : zero                        *
      *                  *---------------------------------------------*
           move      zero                 to   w-sdc-rig-ini          .
      *                  *---------------------------------------------*
      *                  * Ciclo di preparazione colonna progressivi   *
      *                  *---------------------------------------------*
           move      zero                 to   I                      .
       sel-pre-sdc-110.
           add       1                    to   I                      .
           if        I                    >    w-par-num-ele
                     go to  sel-pre-sdc-120.
           add       w-par-res-par (I)    to   w-sdc-tot-prg          .
           move      w-sdc-tot-prg        to   w-par-prg-rig (I)      .
           go to     sel-pre-sdc-110.
       sel-pre-sdc-120.
           go to     sel-pre-sdc-999.
       sel-pre-sdc-200.
      *              *-------------------------------------------------*
      *              * Tipo saldaconto : partite piu' vecchie          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Riga iniziale : zero                        *
      *                  *---------------------------------------------*
           move      zero                 to   w-sdc-rig-ini          .
      *                  *---------------------------------------------*
      *                  * Ciclo di preselezione partite               *
      *                  *---------------------------------------------*
           move      zero                 to   I                      .
       sel-pre-sdc-210.
           add       1                    to   I                      .
           if        I                    >    w-par-num-ele
                     go to  sel-pre-sdc-220.
           add       w-par-res-par (I)    to   w-sdc-tot-ris          .
           if        w-sdc-tot-ris        not  > w-tes-imp-mov (1)
                     move   I             to   w-sdc-wrk-rig          .
           go to     sel-pre-sdc-210.
       sel-pre-sdc-220.
      *                  *---------------------------------------------*
      *                  * Assestamento contatore di riga              *
      *                  *---------------------------------------------*
           if        w-sdc-wrk-rig        <    w-par-num-ele
                     add    1             to   w-sdc-wrk-rig          .
      *                  *---------------------------------------------*
      *                  * Ciclo di selezione partite                  *
      *                  *---------------------------------------------*
           move      zero                 to   w-sdc-tot-ris          .
           move      zero                 to   I                      .
       sel-pre-sdc-230.
           add       1                    to   I                      .
           if        I                    >    w-sdc-wrk-rig
                     go to  sel-pre-sdc-240.
           add       w-par-res-par (I)    to   w-sdc-tot-ris          .
           move      w-par-res-par (I)    to   w-par-imp-ris (I)      .
           move      "S"                  to   w-par-cto-sdo (I)      .
           move      w-sdc-tot-ris        to   w-par-prg-rig (I)      .
           go to     sel-pre-sdc-230.
       sel-pre-sdc-240.
      *                      *-----------------------------------------*
      *                      * Determinazione totale progressivo       *
      *                      *-----------------------------------------*
           move      w-sdc-tot-ris        to   w-sdc-tot-prg          .
      *                      *-----------------------------------------*
      *                      * Se importo assegnato uguale alla somma  *
      *                      * delle partite selezionate : uscita      *
      *                      *-----------------------------------------*
           if        w-sdc-tot-ris        =    w-tes-imp-mov (1)
                     go to  sel-pre-sdc-999.
      *                      *-----------------------------------------*
      *                      * Se importo assegnato maggiore della     *
      *                      * somma delle partite selezionate         *
      *                      *-----------------------------------------*
           if        w-sdc-tot-ris        >    w-tes-imp-mov (1)
                     go to  sel-pre-sdc-260.
           subtract  w-sdc-tot-ris        from w-tes-imp-mov (1)
                                        giving w-sdc-wrk-s11          .
           add       w-sdc-wrk-s11        to   w-sdc-tot-ris          .
           add       w-sdc-wrk-s11        to   w-par-imp-ris
                                              (w-par-num-ele)         .
           move      "C"                  to   w-par-cto-sdo 
                                              (w-par-num-ele)         .
           go to     sel-pre-sdc-999.
       sel-pre-sdc-260.
      *                      *-----------------------------------------*
      *                      * Se importo assegnato minore della       *
      *                      * somma delle partite selezionate         *
      *                      *-----------------------------------------*
           subtract  w-sdc-tot-ris        from w-tes-imp-mov (1)
                                        giving w-sdc-wrk-s11          .
           add       w-sdc-wrk-s11        to   w-sdc-tot-ris          .
           add       w-sdc-wrk-s11        to   w-par-imp-ris
                                              (w-sdc-wrk-rig)         .
           move      "C"                  to   w-par-cto-sdo
                                              (w-sdc-wrk-rig)         .
           go to     sel-pre-sdc-999.
       sel-pre-sdc-300.
      *              *-------------------------------------------------*
      *              * Tipo saldaconto : per periodo                   *
      *              *-------------------------------------------------*
           move      zero                 to   w-sdc-rig-ini          .
           move      zero                 to   I                      .
       sel-pre-sdc-320.
           add       1                    to   I                      .
           if        I                    >    w-par-num-ele
                     go to  sel-pre-sdc-340.
           if        w-par-dat-rif (I)    <    w-tes-dat-ini (1)
                     go to  sel-pre-sdc-320.
           if        w-par-dat-rif (I)    >    w-tes-dat-fin (1)
                     go to  sel-pre-sdc-340.
           if        w-sdc-rig-ini        =    zero
                     move     I           to   w-sdc-rig-ini
                     subtract 1           from w-sdc-rig-ini          .
           add       w-par-res-par (I)    to   w-sdc-tot-ris          .
           move      w-par-res-par (I)    to   w-par-imp-ris (I)      .
           move      "S"                  to   w-par-cto-sdo (I)      .
           move      w-sdc-tot-ris        to   w-par-prg-rig (I)      .
           go to     sel-pre-sdc-320.
       sel-pre-sdc-340.
      *                      *-----------------------------------------*
      *                      * Determinazione totale progressivo       *
      *                      *-----------------------------------------*
           move      w-sdc-tot-ris        to   w-sdc-tot-prg          .
           go to     sel-pre-sdc-999.
       sel-pre-sdc-400.
      *              *-------------------------------------------------*
      *              * Tipo saldaconto : mensile                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Come per tipo saldaconto per periodo        *
      *                  *---------------------------------------------*
           go to     sel-pre-sdc-300.
       sel-pre-sdc-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione pagina saldaconto                         *
      *    *-----------------------------------------------------------*
       vis-pag-sdc-000.
      *              *-------------------------------------------------*
      *              * Aggiornamento numero pagina visualizzata        *
      *              *-------------------------------------------------*
           move      w-sdc-npg-dat        to   w-sdc-npg-vis          .
      *              *-------------------------------------------------*
      *              * Video in Off                                    *
      *              *-------------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Determinazione contatore righe iniziale         *
      *              *-------------------------------------------------*
           multiply  12                   by   w-sdc-npg-dat
                                      giving   w-sdc-wrk-rig          .
           subtract  12                   from w-sdc-wrk-rig          .
           move      zero                 to   I                      .
       vis-pag-sdc-100.
           add       1                    to   I                      .
           if        I                    >    12
                     go to  vis-pag-sdc-900.
           add       1                    to   w-sdc-wrk-rig          .
           if        w-sdc-wrk-rig        >    w-par-num-ele
                     go to  vis-pag-sdc-120
           else      go to  vis-pag-sdc-140.
       vis-pag-sdc-120.
           if        w-sdc-npg-dat        =    1
                     go to  vis-pag-sdc-900.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           add       7
                     I                  giving v-lin                  .
           move      01                   to   v-pos                  .
           move      "                        |                 |       
      -              "          |   |               "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           go to     vis-pag-sdc-100.
       vis-pag-sdc-140.
      *              *-------------------------------------------------*
      *              * Visualizzazione riga                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Tipo documento                              *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           add       7
                     I                  giving v-lin                  .
           move      01                   to   v-pos                  .
           move      w-par-tip-doc
                    (w-sdc-wrk-rig)       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Data riferimento                            *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           add       7
                     I                  giving v-lin                  .
           move      05                   to   v-pos                  .
           move      w-par-dat-rif
                    (w-sdc-wrk-rig)       to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Numero riferimento                          *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      10                   to   v-car                  .
           add       7
                     I                  giving v-lin                  .
           move      14                   to   v-pos                  .
      *
           move      w-par-num-rif
                    (w-sdc-wrk-rig)       to   w-all-str-alf          .
           move      10                   to   w-all-str-lun          .
           perform   all-str-adx-000      thru all-str-adx-999        .
      *
           move      w-all-str-alf        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Residuo partita                             *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "G"                  to   v-edm                  .
           add       7
                     I                  giving v-lin                  .
           move      27                   to   v-pos                  .
           move      w-par-res-par
                    (w-sdc-wrk-rig)       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Importo riscosso                            *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "G"                  to   v-edm                  .
           add       7
                     I                  giving v-lin                  .
           move      45                   to   v-pos                  .
           move      w-par-imp-ris
                    (w-sdc-wrk-rig)       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Flag di in conto o a saldo                  *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      01                   to   v-car                  .
           add       7
                     I                  giving v-lin                  .
           move      63                   to   v-pos                  .
           move      w-par-cto-sdo
                    (w-sdc-wrk-rig)       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Progressivo di riga                         *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "GB"                 to   v-edm                  .
           add       7
                     I                  giving v-lin                  .
           move      66                   to   v-pos                  .
           move      w-par-prg-rig
                    (w-sdc-wrk-rig)       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Riciclo su riga successiva                  *
      *                  *---------------------------------------------*
           go to     vis-pag-sdc-100.
       vis-pag-sdc-900.
      *              *-------------------------------------------------*
      *              * Video in On                                     *
      *              *-------------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-pag-sdc-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Importo riscosso             *
      *    *-----------------------------------------------------------*
       acc-imp-ris-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Salvataggio valori riga precedenti          *
      *                  *---------------------------------------------*
           move      w-par-imp-ris
                    (w-sdc-ctr-rig)       to   w-sav-imp-ris          .
           move      w-par-cto-sdo
                    (w-sdc-ctr-rig)       to   w-sav-cto-sdo          .
       acc-imp-ris-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "G"                  to   v-edm                  .
           move      w-sdc-wrk-lin        to   v-lin                  .
           move      45                   to   v-pos                  .
      *                  *---------------------------------------------*
      *                  * Se flag di impostazione riga attivo non si  *
      *                  * accettano tasti funzione se non Slct o Remv *
      *                  *---------------------------------------------*
           if        w-sdc-flg-imr        not  = spaces
                     go to  acc-imp-ris-150.
           if        w-sdc-ctr-rig        >    1      
                     move   "UP  "        to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           if        w-sdc-npg-dat        <    w-sdc-npg-max
                     move   "NXSC"        to   v-pfk (06)             .
           if        w-sdc-npg-dat        >    1
                     move   "PRSC"        to   v-pfk (07)             .
           if        w-sdc-ctr-rig        <    w-par-num-ele
                     move   "TAB "        to   v-pfk (08)             .
           if        w-sdc-ctr-rig        >    1
                     move   "BACK"        to   v-pfk (09)             .
       acc-imp-ris-150.
           move      "SLCT"               to   v-pfk (10)             .
           move      "REMV"               to   v-pfk (11)             .
           move      w-par-imp-ris
                    (w-sdc-ctr-rig)       to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-imp-ris-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-imp-ris-999.
      *              *-------------------------------------------------*
      *              * Se Remv                                         *
      *              *-------------------------------------------------*
           if        v-key                not  = "REMV"
                     go to acc-imp-ris-200.
      *                  *---------------------------------------------*
      *                  * Controaggiornamento totale riscosso         *
      *                  *---------------------------------------------*
           subtract  w-par-imp-ris
                    (w-sdc-ctr-rig)        from w-sdc-tot-ris         .
           perform   vis-tot-ris-000      thru vis-tot-ris-999        .
      *                  *---------------------------------------------*
      *                  * Normalizzazione valori riga                 *
      *                  *---------------------------------------------*
           move      zero                 to   w-par-imp-ris
                                              (w-sdc-ctr-rig)         .
           move      spaces               to   w-par-cto-sdo
                                              (w-sdc-ctr-rig)         .
           perform vis-imp-ris-000        thru vis-imp-ris-999        .
           perform vis-cto-sdo-000        thru vis-cto-sdo-999        .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     acc-imp-ris-999.
       acc-imp-ris-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-par-imp-ris
                                              (w-sdc-ctr-rig)         .
      *              *-------------------------------------------------*
      *              * Se Up, Down, Prsc, Nxsc, Tab e Back             *
      *              *-------------------------------------------------*
           if        v-key                not  = "UP  " and
                     v-key                not  = "DOWN" and
                     v-key                not  = "PRSC" and
                     v-key                not  = "NXSC" and
                     v-key                not  = "TAB " and
                     v-key                not  = "BACK"
                     go to  acc-imp-ris-300.
      *                  *---------------------------------------------*
      *                  * Se campo modificato : ripristino valore pre-*
      *                  * cedente e reimpostazione                    *
      *                  *---------------------------------------------*
           if        w-par-imp-ris
                    (w-sdc-ctr-rig)       not  = w-sav-imp-ris
                     move   w-sav-imp-ris to   w-par-imp-ris
                                              (w-sdc-ctr-rig)
                     go to  acc-imp-ris-100.
      *                  *---------------------------------------------*
      *                  * Altrimenti : uscita                         *
      *                  *---------------------------------------------*
           go to     acc-imp-ris-999.
       acc-imp-ris-300.
      *              *-------------------------------------------------*
      *              * Se Slct                                         *
      *              *-------------------------------------------------*
           if        v-key                not  = "SLCT"
                     go to  acc-imp-ris-400.
      *                  *---------------------------------------------*
      *                  * Se campo modificato : ripristino valore pre-*
      *                  * cedente e reimpostazione                    *
      *                  *---------------------------------------------*
           if        w-par-imp-ris
                    (w-sdc-ctr-rig)       not  = w-sav-imp-ris
                     move   w-sav-imp-ris to   w-par-imp-ris
                                              (w-sdc-ctr-rig)
                     go to  acc-imp-ris-100.
      *                  *---------------------------------------------*
      *                  * Azioni per Slct                             *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se importo gia' esistente               *
      *                      *-----------------------------------------*
           if        w-par-imp-ris
                    (w-sdc-ctr-rig)       =    zero
                     go to  acc-imp-ris-320.
      *                          *-------------------------------------*
      *                          * Aggiornamento totale riscosso       *
      *                          *-------------------------------------*
           subtract  w-par-imp-ris
                    (w-sdc-ctr-rig)       from w-sdc-tot-ris          .
      *                          *-------------------------------------*
      *                          * Aggiornamento elementi riga         *
      *                          *-------------------------------------*
           move      zero                 to   w-par-imp-ris
                                              (w-sdc-ctr-rig)         .
           move      spaces               to   w-par-cto-sdo
                                              (w-sdc-ctr-rig)         .
           go to     acc-imp-ris-340.
       acc-imp-ris-320.
      *                      *-----------------------------------------*
      *                      * Se importo non esistente                *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Aggiornamento elementi riga         *
      *                          *-------------------------------------*
           move      w-par-res-par
                    (w-sdc-ctr-rig)       to   w-par-imp-ris
                                              (w-sdc-ctr-rig)         .
           move      "S"                  to   w-par-cto-sdo
                                              (w-sdc-ctr-rig)         .
      *                          *-------------------------------------*
      *                          * Aggiornamento totale riscosso       *
      *                          *-------------------------------------*
           add       w-par-imp-ris
                    (w-sdc-ctr-rig)       to   w-sdc-tot-ris          .
       acc-imp-ris-340.
           perform   vis-imp-ris-000      thru vis-imp-ris-999        .
           perform   vis-cto-sdo-000      thru vis-cto-sdo-999        .
           perform   vis-tot-ris-000      thru vis-tot-ris-999        .
           move      "DOWN"               to   v-key                  .
           go to     acc-imp-ris-999.
       acc-imp-ris-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore impostato zero                    *
      *                  *---------------------------------------------*
           if        w-par-imp-ris
                    (w-sdc-ctr-rig)       not  = zero
                     go to acc-imp-ris-600.
           move      spaces               to   w-par-cto-sdo
                                              (w-sdc-ctr-rig)         .
           perform   vis-cto-sdo-000      thru vis-cto-sdo-999        .
           go to     acc-imp-ris-700.
       acc-imp-ris-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se importo riscosso diverso da residuo par- *
      *                  * tita                                        *
      *                  *---------------------------------------------*
           if        w-par-imp-ris
                    (w-sdc-ctr-rig)       =    w-par-res-par
                                              (w-sdc-ctr-rig)
                     go to  acc-imp-ris-620.
      *                      *-----------------------------------------*
      *                      * Se importo riscosso precedente uguale a *
      *                      * residuo partita : abblencamento flag di *
      *                      * in conto o a saldo                      *
      *                      *-----------------------------------------*
           if        w-sav-imp-ris        =    w-par-res-par
                                              (w-sdc-ctr-rig)
                     move   spaces        to   w-par-cto-sdo
                                              (w-sdc-ctr-rig)         .
           go to     acc-imp-ris-700.
       acc-imp-ris-620.
      *                  *---------------------------------------------*
      *                  * Se importo riscosso uguale a residuo parti- *
      *                  * ta                                          *
      *                  *---------------------------------------------*
           move      "S"                  to   w-par-cto-sdo
                                              (w-sdc-ctr-rig)         .
           perform   vis-cto-sdo-000      thru vis-cto-sdo-999        .
       acc-imp-ris-700.
      *                  *---------------------------------------------*
      *                  * Aggiornamento totale riscosso               *
      *                  *---------------------------------------------*
           subtract  w-sav-imp-ris        from w-sdc-tot-ris          .
           add       w-par-imp-ris
                    (w-sdc-ctr-rig)       to   w-sdc-tot-ris          .
           perform   vis-tot-ris-000      thru vis-tot-ris-999        .
       acc-imp-ris-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-sdc-000
                                          thru cnt-tdo-sdc-999
                     if      w-cnt-tdo-sdc-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-imp-ris-999
                     else    move  spaces to   w-cnt-tdo-sdc-flg
                             go to acc-imp-ris-100.
       acc-imp-ris-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Importo riscosso          *
      *    *-----------------------------------------------------------*
       vis-imp-ris-000.
           move      "DS"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "G"                  to   v-edm                  .
           move      w-sdc-wrk-lin        to   v-lin                  .
           move      45                   to   v-pos                  .
           move      w-par-imp-ris
                    (w-sdc-ctr-rig)       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-imp-ris-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Flag di in conto o a saldo   *
      *    *-----------------------------------------------------------*
       acc-cto-sdo-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-par-imp-ris
                    (w-sdc-ctr-rig)       =    zero       or
                     w-par-imp-ris
                    (w-sdc-ctr-rig)       =    w-par-res-par
                                              (w-sdc-ctr-rig)
                     go to  acc-cto-sdo-999.
       acc-cto-sdo-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      w-sdc-wrk-lin        to   v-lin                  .
           move      63                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "REMV"               to   v-pfk (06)             .
           move      w-par-cto-sdo
                    (w-sdc-ctr-rig)       to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                not  = "EXIT"
                     go to  acc-cto-sdo-120.
      *                  *---------------------------------------------*
      *                  * Aggiornamento totale riscosso               *
      *                  *---------------------------------------------*
           subtract  w-par-imp-ris
                    (w-sdc-ctr-rig)       from w-sdc-tot-ris          .
           add       w-sav-imp-ris        to   w-sdc-tot-ris          .
           perform   vis-tot-ris-000      thru vis-tot-ris-999        .
      *                  *---------------------------------------------*
      *                  * Ripristino valori precedentemente salvati   *
      *                  *---------------------------------------------*
            move     w-sav-imp-ris        to   w-par-imp-ris
                                              (w-sdc-ctr-rig)         .
            move     w-sav-cto-sdo        to   w-par-cto-sdo
                                              (w-sdc-ctr-rig)         .
            perform  vis-cto-sdo-000      thru vis-cto-sdo-999        .
      *                  *---------------------------------------------*
      *                  * Uscita come per Up                          *
      *                  *---------------------------------------------*
           move      "UP  "               to   v-key                  .
           go to     acc-cto-sdo-999.
       acc-cto-sdo-120.
      *              *-------------------------------------------------*
      *              * Se Remv                                         *
      *              *-------------------------------------------------*
           if        v-key                not  = "REMV"
                     go to  acc-cto-sdo-140.
      *                  *---------------------------------------------*
      *                  * Controaggiornamento totale riscosso         *
      *                  *---------------------------------------------*
           subtract  w-par-imp-ris
                    (w-sdc-ctr-rig)        from w-sdc-tot-ris          .
           perform   vis-tot-ris-000      thru vis-tot-ris-999        .
      *                  *---------------------------------------------*
      *                  * Normalizzazione valori riga                 *
      *                  *---------------------------------------------*
           move      zero                 to   w-par-imp-ris
                                              (w-sdc-ctr-rig)         .
           move      spaces               to   w-par-cto-sdo
                                              (w-sdc-ctr-rig)         .
           perform vis-cto-sdo-000        thru vis-cto-sdo-999
      *                  *---------------------------------------------*
      *                  * Uscita come per Up                          *
      *                  *---------------------------------------------*
           move      "UP  "               to   v-key                  .
           go to     acc-cto-sdo-999.
       acc-cto-sdo-140.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-cto-sdo-999.
       acc-cto-sdo-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-par-cto-sdo
                                              (w-sdc-ctr-rig)         .
      *              *-------------------------------------------------*
      *              * Se Up                                           *
      *              *-------------------------------------------------*
           if        v-key                not  = "UP  "
                     go to  acc-cto-sdo-400.
           if        w-par-cto-sdo
                    (w-sdc-ctr-rig)       =    "C" or
                     w-par-cto-sdo
                    (w-sdc-ctr-rig)       =    "S" or
                     w-par-cto-sdo
                    (w-sdc-ctr-rig)       =    " "
                     go to  acc-cto-sdo-999
           else      go to  acc-cto-sdo-100.
       acc-cto-sdo-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
           if        w-par-cto-sdo
                    (w-sdc-ctr-rig)       not  = "C" and
                     w-par-cto-sdo
                    (w-sdc-ctr-rig)       not  = "S"
                     go to  acc-cto-sdo-100.
       acc-cto-sdo-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cto-sdo-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-sdc-000
                                          thru cnt-tdo-sdc-999
                     if      w-cnt-tdo-sdc-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-cto-sdo-999
                     else    move  spaces to   w-cnt-tdo-sdc-flg
                             go to acc-cto-sdo-100.
       acc-cto-sdo-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Flag di in conto o a saldo*
      *    *-----------------------------------------------------------*
       vis-cto-sdo-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      w-sdc-wrk-lin        to   v-lin                  .
           move      63                   to   v-pos                  .
           move      w-par-cto-sdo
                    (w-sdc-ctr-rig)       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cto-sdo-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Totale riscosso           *
      *    *-----------------------------------------------------------*
       vis-tot-ris-000.
           move      "DS"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "G"                  to   v-edm                  .
           move      21                   to   v-lin                  .
           move      45                   to   v-pos                  .
           move      w-sdc-tot-ris        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tot-ris-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Totale progressivo        *
      *    *-----------------------------------------------------------*
       vis-tot-prg-000.
           move      "DS"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "G"                  to   v-edm                  .
           move      21                   to   v-lin                  .
           move      66                   to   v-pos                  .
           move      w-sdc-tot-prg        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tot-prg-999.
           exit.

      *    *===========================================================*
      *    * Determinazione partite aperte cliente                     *
      *    *-----------------------------------------------------------*
       det-pap-cli-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-det-pap-cli-flg      .
      *              *-------------------------------------------------*
      *              * Flag di primo passaggio                         *
      *              *-------------------------------------------------*
           move      "#"                  to   w-par-flg-uno          .
      *              *-------------------------------------------------*
      *              * Inizializzazione saldo cliente                  *
      *              *-------------------------------------------------*
           move      zero                 to   w-par-sdo-cli          .
      *              *-------------------------------------------------*
      *              * Normalizzazione contatore numero partite        *
      *              *-------------------------------------------------*
           move      zero                 to   w-par-num-ele          .
      *              *-------------------------------------------------*
      *              * Normalizzazione area di comodo per rottura par- *
      *              * tita                                            *
      *              *-------------------------------------------------*
           move      zero                 to   w-par-rot-dat          .
           move      spaces               to   w-par-rot-num          .
      *              *-------------------------------------------------*
      *              * Start su file [mgr]                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione record file [mgr]           *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmgr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgr                 .
      *                  *---------------------------------------------*
      *                  * Preparazione chiave                         *
      *                  *---------------------------------------------*
           move      "C"                  to   rf-mgr-tip-arc         .
           move      w-det-pap-cli-cod    to   rf-mgr-cod-arc         .
      *                  *---------------------------------------------*
      *                  * Operazione di 'Start'                       *
      *                  *---------------------------------------------*
           move      "ARCRIF"             to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      "SK"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmgr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgr                 .
      *              *-------------------------------------------------*
      *              * Test su esito operazione                        *
      *              *-------------------------------------------------*
           if        f-sts                =    e-not-err
                     go to  det-pap-cli-300.
       det-pap-cli-200.
      *                  *---------------------------------------------*
      *                  * Se errore                                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Composizione messaggio                  *
      *                      *-----------------------------------------*
           move      "Attenzione : nessuna partita aperta per il cliente
      -              " !"                to   w-det-pap-cli-msg      .
      *                      *-----------------------------------------*
      *                      * Uscita con segnale di errore            *
      *                      *-----------------------------------------*
           move      "#"                  to   w-det-pap-cli-flg      .
           go to     det-pap-cli-999.
       det-pap-cli-300.
      *              *-------------------------------------------------*
      *              * Lettura sequenziale file [mgr]                  *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmgr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgr                 .
      *              *-------------------------------------------------*
      *              * Test su esito operazione                        *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to  det-pap-cli-800.
      *              *-------------------------------------------------*
      *              * Test sul massimo                                *
      *              *-------------------------------------------------*
           if        rf-mgr-tip-arc       not  = "C"            or
                     rf-mgr-cod-arc       not  = w-det-pap-cli-cod
                     go to  det-pap-cli-800.
      *              *-------------------------------------------------*
      *              * Se rottura data e numero partita                *
      *              *-------------------------------------------------*
           if        rf-mgr-dat-rif       =    w-par-rot-dat and
                     rf-mgr-num-rif       =    w-par-rot-num
                     go to  det-pap-cli-500.
      *                  *---------------------------------------------*
      *                  * Fine partita precedente                     *
      *                  *---------------------------------------------*
           perform   det-pap-cli-910      thru det-pap-cli-919        .
           if        w-par-num-ele        not  > w-par-max-ele
                     go to  det-pap-cli-400.
       det-pap-cli-350.
      *                      *-----------------------------------------*
      *                      * Se esubero numero partite per cliente   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Composizione messaggio              *
      *                          *-------------------------------------*
           move      "Attenzione : cliente con partite aperte oltre il m
      -              "assimo consentito !"
                                          to   w-det-pap-cli-msg      .
      *                          *-------------------------------------*
      *                          * Uscita con segnale di errore        *
      *                          *-------------------------------------*
           move      "#"                  to   w-det-pap-cli-flg      .
           go to     det-pap-cli-999.
       det-pap-cli-400.
      *                  *---------------------------------------------*
      *                  * Inizio partita successiva                   *
      *                  *---------------------------------------------*
           perform   det-pap-cli-900      thru det-pap-cli-909        .
      *                  *---------------------------------------------*
      *                  * Aggiornamento area rottura data e numero    *
      *                  * partita                                     *
      *                  *---------------------------------------------*
           move      rf-mgr-dat-rif       to   w-par-rot-dat          .
           move      rf-mgr-num-rif       to   w-par-rot-num          .
       det-pap-cli-500.
      *              *-------------------------------------------------*
      *              * Aggiornamento residuo partita                   *
      *              *-------------------------------------------------*
           if        rf-mgr-dar-ave       =    "A"
                     subtract rf-mgr-imp-mov
                                          from w-par-res-par (901)
           else      add      rf-mgr-imp-mov
                                          to   w-par-res-par (901)    .
      *              *-------------------------------------------------*
      *              * Riciclo su lettura file [mgr]                   *
      *              *-------------------------------------------------*
           go to     det-pap-cli-300.
       det-pap-cli-800.
      *              *-------------------------------------------------*
      *              * Fine partita precedente                         *
      *              *-------------------------------------------------*
           perform   det-pap-cli-910      thru det-pap-cli-919        .
           if        w-par-num-ele        >    w-par-max-ele
                     go to  det-pap-cli-350.
      *              *-------------------------------------------------*
      *              * Test se esiste almeno una partita per il cliente*
      *              *-------------------------------------------------*
           if        w-par-num-ele        =    zero
                     go to  det-pap-cli-200.
           go to     det-pap-cli-999.
       det-pap-cli-900.
      *              *-------------------------------------------------*
      *              * Subroutine interna di inizio partita            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Memorizzazione dati partita nell'elemento   *
      *                  * 901esimo                                    *
      *                  *---------------------------------------------*
           if        rf-mgr-tip-iva       =    "1" or
                     rf-mgr-tip-iva       =    "A"
                     move   "FAT"         to   w-par-tip-doc (901)
           else if   rf-mgr-tip-iva       =    "2" or
                     rf-mgr-tip-iva       =    "B"
                     move   "NCR"         to   w-par-tip-doc (901)
           else      move   "ANT"         to   w-par-tip-doc (901)    .
           move      rf-mgr-dat-rif       to   w-par-dat-rif (901)    .
           move      rf-mgr-num-rif       to   w-par-num-rif (901)    .
           move      zero                 to   w-par-res-par (901)    .
           move      zero                 to   w-par-imp-ris (901)    .
           move      spaces               to   w-par-cto-sdo (901)    .
           move      zero                 to   w-par-prg-rig (901)    .
       det-pap-cli-909.
           exit.
       det-pap-cli-910.
      *              *-------------------------------------------------*
      *              * Subroutine interna di fine partita              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se primo passaggio : uscita                 *
      *                  *---------------------------------------------*
           if        w-par-flg-uno        not  = spaces
                     move   spaces        to   w-par-flg-uno
                     go to  det-pap-cli-919.
      *                  *---------------------------------------------*
      *                  * Se saldo partita zero : uscita              *
      *                  *---------------------------------------------*
           if        w-par-res-par (901)  =    zero
                     go to  det-pap-cli-919.
      *                  *---------------------------------------------*
      *                  * Incremento contatore numero partite         *
      *                  *---------------------------------------------*
           add       1                    to   w-par-num-ele          .
           if        w-par-num-ele        >    w-par-max-ele
                     go to  det-pap-cli-919.
      *                  *---------------------------------------------*
      *                  * Aggiornamento tabella partite               *
      *                  *---------------------------------------------*
           move      w-par-tbl-ele (901)  to   w-par-tbl-ele
                                              (w-par-num-ele)         .
      *                  *---------------------------------------------*
      *                  * Aggiornamento saldo cliente                 *
      *                  *---------------------------------------------*
           add       w-par-res-par (901)  to   w-par-sdo-cli          .
      *                  *---------------------------------------------*
      *                  * Aggiornamento tabella progressivi mensili   *
      *                  *---------------------------------------------*
           move      w-par-dat-rif (901)  to   s-dat                  .
           move      s-saa                to   w-prm-wrk-saa          .
           move      s-mes                to   w-prm-wrk-mes          .
           move      zero                 to   I                      .
       det-pap-cli-911.
           add       1                    to   I                      .
           if        I                    >    7
                     go to  det-pap-cli-919.
           if        w-prm-wrk-sam        >    w-prm-sam-prm (I)
                     go to  det-pap-cli-911.
           add       w-par-res-par (901)  to   w-prm-sdo-prm (I)      .
       det-pap-cli-919.
           exit.
       det-pap-cli-999.
           exit.

      *    *===========================================================*
      *    * Determinazione residuo operazione                         *
      *    *-----------------------------------------------------------*
       det-res-ope-000.
           subtract  w-det-res-ope-ris    from w-det-res-ope-par
                                        giving w-det-res-ope-res      .
       det-res-ope-999.
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
      *              * Se tipo saldaconto per periodo                  *
      *              *-------------------------------------------------*
           if        w-tes-tip-sdc (1)    not  =  3
                     go to cnt-tdo-nok-300.
      *                  *---------------------------------------------*
      *                  * Controllo su data minima e massima          *
      *                  *---------------------------------------------*
           if        w-tes-dat-fin (1)    =    zero
                     go to cnt-tdo-nok-200.
           if        w-tes-dat-fin (1)    not  < w-tes-dat-ini (1)
                     go to cnt-tdo-nok-200.
           move      "ME"                 to   v-ope                  .
           move      "La data finale non puo' essere inferiore alla data
      -              " iniziale !"        to   v-not                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "#"                  to   w-cnt-tdo-nok-flg      .
           go to     cnt-tdo-nok-999.
       cnt-tdo-nok-200.
      *                  *---------------------------------------------*
      *                  * Regolarizzazione data iniziale e finale     *
      *                  *---------------------------------------------*
           if        w-tes-dat-fin (1)    not  = zero
                     go to cnt-tdo-nok-300.
           if        w-tes-dat-ini (1)    =    zero
                     move  9999999        to   w-tes-dat-fin (1)
           else      move  w-tes-dat-ini (1)
                                          to   w-tes-dat-fin (1)      .
       cnt-tdo-nok-300.
      *              *-------------------------------------------------*
      *              * Se tipo saldaconto mensile                      *
      *              *-------------------------------------------------*
           if        w-tes-tip-sdc (1)    not  =  4
                     go to cnt-tdo-nok-999.
      *                  *---------------------------------------------*
      *                  * Regolarizzazione data iniziale              *
      *                  *---------------------------------------------*
           move      w-tes-mes-sel (1)    to   w-prm-wrk-mes          .
           move      zero                 to   s-dat                  .
           move      1                    to   s-gio                  .
           move      w-prm-mes-prm
                    (w-prm-wrk-mes)       to   s-mes                  .
           move      w-prm-saa-prm
                    (w-prm-wrk-mes)       to   s-saa                  .
           move      s-dat                to   w-tes-dat-ini (1)      .
       cnt-tdo-nok-320.
      *                  *---------------------------------------------*
      *                  * Regolarizzazione data finale                *
      *                  *---------------------------------------------*
           move      w-tes-dat-ini (1)    to   s-dat                  .
           move      31                   to   s-gio                  .
           move      "CD"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-sts                =    spaces
                     go to  cnt-tdo-nok-340.
           move      30                   to   s-gio                  .
           move      "CD"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-sts                =    spaces
                     go to  cnt-tdo-nok-340.
           move      29                   to   s-gio                  .
           move      "CD"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-sts                =    spaces
                     go to  cnt-tdo-nok-340.
           move      28                   to   s-gio                  .
       cnt-tdo-nok-340.
           move      s-dat                to   w-tes-dat-fin (1)      .
      *                  *---------------------------------------------*
      *                  * Se selezionato 'anteriori'                  *
      *                  *---------------------------------------------*
           if        w-prm-wrk-mes        =    1
                     move   zero          to   w-tes-dat-ini (1)      .
       cnt-tdo-nok-999.
           exit.

      *    *===========================================================*
      *    * Controllo su impostazione tasto Do saldaconto             *
      *    *-----------------------------------------------------------*
       cnt-tdo-sdc-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-tdo-sdc-flg      .
      *              *-------------------------------------------------*
      *              * Test su totale riscosso                         *
      *              *-------------------------------------------------*
           if        w-sdc-tot-ris        =    zero
                     move   "#"           to   w-cnt-tdo-sdc-flg      .
      *              *-------------------------------------------------*
      *              * Regolarizzazione importo movimento              *
      *              *-------------------------------------------------*
           move      w-sdc-tot-ris        to   w-tes-imp-mov (1)      .
       cnt-tdo-sdc-999.
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
       nor-key-reg-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione dati non chiave testata                   *
      *    *-----------------------------------------------------------*
       nor-nok-tes-000.
           move      zero                 to   w-tes-tip-sdc (1)      .
           move      zero                 to   w-tes-dat-ini (1)      .
           move      zero                 to   w-tes-dat-fin (1)      .
           move      zero                 to   w-tes-mes-sel (1)      .
           move      spaces               to   w-tes-imp-sdc (1)      .
           move      zero                 to   w-tes-imp-mov (1)      .
       nor-nok-tes-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione dati non chiave riga corpo                *
      *    *-----------------------------------------------------------*
       nor-nok-rig-000.
           move      zero                 to   w-rig-num-prg (1)      .
           move      spaces               to   w-rig-tip-con (1)      .
           move      zero                 to   w-rig-cod-con (1)      .
           move      spaces               to   w-rig-des-con (1)      .
           move      zero                 to   w-rig-cod-pdc (1)      .
           move      spaces               to   w-rig-tip-arc (1)      .
           move      zero                 to   w-rig-cod-arc (1)      .
           move      spaces               to   w-rig-com-rig (1)      .
           move      zero                 to   w-rig-dat-rif (1)      .
           move      spaces               to   w-rig-num-rif (1)      .
           move      spaces               to   w-rig-dar-ave (1)      .
           move      zero                 to   w-rig-imp-mov (1)      .
           move      spaces               to   w-rig-flg-pge (1)      .
           move      spaces               to   w-rig-flg-pcf (1)      .
       nor-nok-rig-999.
           exit.

      *    *===========================================================*
      *    * Lettura registrazione pre-esistente                       *
      *    *-----------------------------------------------------------*
       rou-let-reg-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-rou-let-reg      .
      *                  *---------------------------------------------*
      *                  * Tipo funzionamento : impostazione           *
      *                  *---------------------------------------------*
           move      "I"                  to   w-cnt-mfu-tip-fun      .
       rou-let-reg-999.
           exit.

      *    *===========================================================*
      *    * Routine pre-accettazioni per inserimento                  *
      *    *-----------------------------------------------------------*
       pre-acc-ins-000.
       pre-acc-ins-999.
           exit.

      *    *===========================================================*
      *    * Routine pre-accettazioni per modifica                     *
      *    *-----------------------------------------------------------*
       pre-acc-mod-000.
       pre-acc-mod-999.
           exit.

      *    *===========================================================*
      *    * Routine pre-accettazioni per visualizzazione              *
      *    *-----------------------------------------------------------*
       pre-acc-vis-000.
       pre-acc-vis-999.
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
      *              * Determinazione status uscita da saldaconto      *
      *              *-------------------------------------------------*
           move      "#"                  to   w-lnk-exi-sts          .
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
       pos-cnf-mod-999.
           exit.

      *    *===========================================================*
      *    * Routine post-conferma di annullamento                     *
      *    *-----------------------------------------------------------*
       pos-cnf-ann-000.
       pos-cnf-ann-999.
           exit.

      *    *===========================================================*
      *    * Scrittura movimento su file                               *
      *    *-----------------------------------------------------------*
       scr-mov-fil-000.
      *              *-------------------------------------------------*
      *              * Write catena righe                              *
      *              *-------------------------------------------------*
           perform   wrt-cat-rig-000      thru wrt-cat-rig-999        .
       scr-mov-fil-999.
           exit.

      *    *===========================================================*
      *    * Delete movimento da file                                  *
      *    *-----------------------------------------------------------*
       del-mov-fil-000.
       del-mov-fil-999.
           exit.

      *    *===========================================================*
      *    * Scrittura catena righe movimento                          *
      *    *-----------------------------------------------------------*
       wrt-cat-rig-000.
      *              *-------------------------------------------------*
      *              * Inizializzazione accumulatori abbuoni e anticipi*
      *              *-------------------------------------------------*
           move      zero                 to   w-wrk-prg-abb          .
           move      zero                 to   w-wrk-prg-ant          .
      *              *-------------------------------------------------*
      *              * Attivazione flag primo append                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-wrk-flg-app          .
      *              *-------------------------------------------------*
      *              * Ciclo di scansione righe partita                *
      *              *-------------------------------------------------*
           move      zero                 to   I                      .
       wrt-cat-rig-100.
           add       1                    to   I                      .
           if        I                    >    w-par-num-ele
                     go to  wrt-cat-rig-400.
           if        w-par-imp-ris (I)    =    zero
                     go to  wrt-cat-rig-100.
      *                  *---------------------------------------------*
      *                  * Append                                      *
      *                  *---------------------------------------------*
           if        w-wrk-flg-app        not  = spaces
                     move  spaces         to   w-wrk-flg-app
                     go to wrt-cat-rig-110.
           if        w-cat-rig-app        not  = spaces
                     go to  wrt-cat-rig-900.
           move      "AP"                 to   w-cat-rig-ope          .
           call      "pgm/cge/prg/obj/pcge3002"
                                         using w-cat-rig              .
       wrt-cat-rig-110.
      *                  *---------------------------------------------*
      *                  * Determinazione residuo operazione di riga   *
      *                  *---------------------------------------------*
           move      w-par-imp-ris (I)    to   w-det-res-ope-ris      .
           move      w-par-res-par (I)    to   w-det-res-ope-par      .
           perform   det-res-ope-000      thru det-res-ope-999        .
      *                  *---------------------------------------------*
      *                  * Normalizzazione riga                        *
      *                  *---------------------------------------------*
           perform   nor-nok-rig-000      thru nor-nok-rig-999        .
      *                  *---------------------------------------------*
      *                  * Composizione riga                           *
      *                  *---------------------------------------------*
           move      "C"                  to   w-rig-tip-con (1)      .
           move      w-lnk-cod-cof        to   w-rig-cod-con (1)      .
           move      w-lnk-rag-cof        to   w-rig-des-con (1)      .
           move      w-lnk-stc-cof        to   w-rig-cod-pdc (1)      .
           move      "C"                  to   w-rig-tip-arc (1)      .
           move      w-lnk-cod-cof        to   w-rig-cod-arc (1)      .
           move      w-par-dat-rif (I)    to   w-rig-dat-rif (1)      .
           move      w-par-num-rif (I)    to   w-rig-num-rif (1)      .
      *                  *---------------------------------------------*
      *                  * Se registrazione a saldo                    *
      *                  *---------------------------------------------*
           if        w-par-cto-sdo (I)    not  = "S"
                     go to  wrt-cat-rig-200.
           move      w-par-res-par (I)    to   w-rig-imp-mov (1)      .
           if        w-det-res-ope-res    not  = zero
                     add    w-det-res-ope-res
                                          to   w-wrk-prg-abb          .
           go to     wrt-cat-rig-300.
       wrt-cat-rig-200.
      *                  *---------------------------------------------*
      *                  * Se registrazione in conto                   *
      *                  *---------------------------------------------*
           if        w-det-res-ope-res    <    zero
                     move   w-par-res-par (I)
                                          to   w-rig-imp-mov (1)
                     add    w-det-res-ope-res
                                          to   w-wrk-prg-ant
           else      move   w-par-imp-ris (I)
                                          to   w-rig-imp-mov (1)      .
       wrt-cat-rig-300.
           if        w-rig-imp-mov (1)    >    zero
                     move     "A"         to   w-rig-dar-ave (1)
           else      move     "D"         to   w-rig-dar-ave (1)
                     multiply -1          by   w-rig-imp-mov (1)      .
      *                  *---------------------------------------------*
      *                  * Update catena                               *
      *                  *---------------------------------------------*
           move      "UP"                 to   w-cat-rig-ope          .
           move      w-cat-rig-max        to   w-cat-rig-num          .
           move      w-rig                to   w-cat-rig-buf          .
           call      "pgm/cge/prg/obj/pcge3002"
                                         using w-cat-rig              .
      *                  *---------------------------------------------*
      *                  * Aggiornamento numero riga                   *
      *                  *---------------------------------------------*
           add       1                    to   w-lnk-nrg-dac          .
      *                  *---------------------------------------------*
      *                  * Aggiornamento saldo registrazione           *
      *                  *---------------------------------------------*
           if        w-rig-dar-ave (1)    =    "A"
                     subtract  w-rig-imp-mov (1)
                                          from w-lnk-sdo-reg
           else      add        w-rig-imp-mov (1)
                                          to   w-lnk-sdo-reg          .
      *              *-------------------------------------------------*
      *              * Riciclo su riga partita successiva              *
      *              *-------------------------------------------------*
           go to     wrt-cat-rig-100.
       wrt-cat-rig-400.
      *              *-------------------------------------------------*
      *              * Registrazione eventuale abbuono                 *
      *              *-------------------------------------------------*
           if        w-wrk-prg-abb        =    zero
                     go to  wrt-cat-rig-500.
      *                  *---------------------------------------------*
      *                  * Se registrazione abbuono passivo            *
      *                  *---------------------------------------------*
           if        w-wrk-prg-abb        <    zero
                     go to  wrt-cat-rig-420.
      *                      *-----------------------------------------*
      *                      * Scrittura riga : riga 'abbuono passivo' *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Lettura descrizione sottoconto ab-  *
      *                          * buoni passivi                       *
      *                          *-------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODSTC"             to   f-key                  .
           move      w-ref-abb-pas        to   rf-pdc-cod-pdc         .
           move      "pgm/cge/fls/ioc/obj/iofpdc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdc                 .
           if        f-sts                not  = e-not-err
                     move   all "."       to   rf-pdc-des-pdc         .
      *                          *-------------------------------------*
      *                          * Append catena                       *
      *                          *-------------------------------------*
           if        w-cat-rig-app        not  = spaces
                     go to  wrt-cat-rig-900.
           move      "AP"                 to   w-cat-rig-ope          .
           call      "pgm/cge/prg/obj/pcge3002"
                                         using w-cat-rig              .
      *                          *-------------------------------------*
      *                          * Normalizzazione riga                *
      *                          *-------------------------------------*
           perform   nor-nok-rig-000      thru nor-nok-rig-999        .
      *                          *-------------------------------------*
      *                          * Composizione riga                   *
      *                          *-------------------------------------*
           move      "G"                  to   w-rig-tip-con (1)      .
           move      w-ref-abb-pas        to   w-rig-cod-con (1)      .
           move      rf-pdc-des-pdc       to   w-rig-des-con (1)      .
           move      w-ref-abb-pas        to   w-rig-cod-pdc (1)      .
           move      "G"                  to   w-rig-tip-arc (1)      .
           move      "D"                  to   w-rig-dar-ave (1)      .
           move      w-wrk-prg-abb        to   w-rig-imp-mov (1)      .
      *                          *-------------------------------------*
      *                          * Update catena                       *
      *                          *-------------------------------------*
           move      "UP"                 to   w-cat-rig-ope          .
           move      w-cat-rig-max        to   w-cat-rig-num          .
           move      w-rig                to   w-cat-rig-buf          .
           call      "pgm/cge/prg/obj/pcge3002"
                                         using w-cat-rig              .
      *                          *-------------------------------------*
      *                          * Aggiornamento numero riga           *
      *                          *-------------------------------------*
           add       1                    to   w-lnk-nrg-dac          .
      *                          *-------------------------------------*
      *                          * Aggiornamento saldo registrazione   *
      *                          *-------------------------------------*
           if        w-rig-dar-ave (1)    =    "A"
                     subtract  w-rig-imp-mov (1)
                                          from w-lnk-sdo-reg
           else      add        w-rig-imp-mov (1)
                                          to   w-lnk-sdo-reg          .
           go to     wrt-cat-rig-500.
       wrt-cat-rig-420.
      *                  *---------------------------------------------*
      *                  * Se registrazione abbuono attivo             *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Scrittura riga : riga 'abbuono attivo'  *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Lettura descrizione sottoconto ab-  *
      *                          * buoni attivi                        *
      *                          *-------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODSTC"             to   f-key                  .
           move      w-ref-abb-att        to   rf-pdc-cod-pdc         .
           move      "pgm/cge/fls/ioc/obj/iofpdc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdc                 .
           if        f-sts                not  = e-not-err
                     move   all "."       to   rf-pdc-des-pdc         .
      *                          *-------------------------------------*
      *                          * Append catena                       *
      *                          *-------------------------------------*
           if        w-cat-rig-app        not  = spaces
                     go to  wrt-cat-rig-900.
           move      "AP"                 to   w-cat-rig-ope          .
           call      "pgm/cge/prg/obj/pcge3002"
                                         using w-cat-rig              .
      *                          *-------------------------------------*
      *                          * Normalizzazione riga                *
      *                          *-------------------------------------*
           perform   nor-nok-rig-000      thru nor-nok-rig-999        .
      *                          *-------------------------------------*
      *                          * Composizione riga                   *
      *                          *-------------------------------------*
           move      "G"                  to   w-rig-tip-con (1)      .
           move      w-ref-abb-att        to   w-rig-cod-con (1)      .
           move      rf-pdc-des-pdc       to   w-rig-des-con (1)      .
           move      w-ref-abb-att        to   w-rig-cod-pdc (1)      .
           move      "G"                  to   w-rig-tip-arc (1)      .
           move      "A"                  to   w-rig-dar-ave (1)      .
           move      w-wrk-prg-abb        to   w-rig-imp-mov (1)      .
           multiply  -1                   by   w-rig-imp-mov (1)      .
      *                          *-------------------------------------*
      *                          * Update catena                       *
      *                          *-------------------------------------*
           move      "UP"                 to   w-cat-rig-ope          .
           move      w-cat-rig-max        to   w-cat-rig-num          .
           move      w-rig                to   w-cat-rig-buf          .
           call      "pgm/cge/prg/obj/pcge3002"
                                         using w-cat-rig              .
      *                          *-------------------------------------*
      *                          * Aggiornamento numero riga           *
      *                          *-------------------------------------*
           add       1                    to   w-lnk-nrg-dac          .
      *                          *-------------------------------------*
      *                          * Aggiornamento saldo registrazione   *
      *                          *-------------------------------------*
           if        w-rig-dar-ave (1)    =    "A"
                     subtract  w-rig-imp-mov (1)
                                          from w-lnk-sdo-reg
           else      add        w-rig-imp-mov (1)
                                          to   w-lnk-sdo-reg          .
       wrt-cat-rig-500.
      *              *-------------------------------------------------*
      *              * Registrazione eventuale anticipo                *
      *              *-------------------------------------------------*
           if        w-wrk-prg-ant        =    zero
                     go to  wrt-cat-rig-999.
      *                  *---------------------------------------------*
      *                  * Scrittura riga : riga 'anticipo'            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Append catena                           *
      *                      *-----------------------------------------*
           if        w-cat-rig-app        not  = spaces
                     go to  wrt-cat-rig-900.
           move      "AP"                 to   w-cat-rig-ope          .
           call      "pgm/cge/prg/obj/pcge3002"
                                         using w-cat-rig              .
      *                      *-----------------------------------------*
      *                      * Normalizzazione riga                    *
      *                      *-----------------------------------------*
           perform   nor-nok-rig-000      thru nor-nok-rig-999        .
      *                      *-----------------------------------------*
      *                      * Composizione riga                       *
      *                      *-----------------------------------------*
           move      "C"                  to   w-rig-tip-con (1)      .
           move      w-lnk-cod-cof        to   w-rig-cod-con (1)      .
           move      w-lnk-rag-cof        to   w-rig-des-con (1)      .
           move      w-lnk-stc-cof        to   w-rig-cod-pdc (1)      .
           move      "C"                  to   w-rig-tip-arc (1)      .
           move      w-lnk-cod-cof        to   w-rig-cod-arc (1)      .
           move      "A titolo di anticipo"
                                          to   w-rig-com-rig (1)      .
           move      "A"                  to   w-rig-dar-ave (1)      .
           move      w-wrk-prg-ant        to   w-rig-imp-mov (1)      .
           multiply  -1                   by   w-rig-imp-mov (1)      .
      *                      *-----------------------------------------*
      *                      * Update catena                           *
      *                      *-----------------------------------------*
           move      "UP"                 to   w-cat-rig-ope          .
           move      w-cat-rig-max        to   w-cat-rig-num          .
           move      w-rig                to   w-cat-rig-buf          .
           call      "pgm/cge/prg/obj/pcge3002"
                                         using w-cat-rig              .
      *                      *-----------------------------------------*
      *                      * Aggiornamento numero riga               *
      *                      *-----------------------------------------*
           add       1                    to   w-lnk-nrg-dac          .
      *                      *-----------------------------------------*
      *                      * Aggiornamento saldo registrazione       *
      *                      *-----------------------------------------*
           if        w-rig-dar-ave (1)    =    "A"
                     subtract  w-rig-imp-mov (1)
                                          from w-lnk-sdo-reg
           else      add        w-rig-imp-mov (1)
                                          to   w-lnk-sdo-reg          .
           go to     wrt-cat-rig-999.
       wrt-cat-rig-900.
      *              *-------------------------------------------------*
      *              * Messaggio di errore per esubero numero righe    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Salvataggio immagine video                  *
      *                  *---------------------------------------------*
           move      "SV"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione box internamente vuoto      *
      *                  *---------------------------------------------*
           move      "BX"                 to   v-ope                  .
           move      09                   to   v-lin                  .
           move      14                   to   v-pos                  .
           move      16                   to   v-lto                  .
           move      67                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione messaggio interno al box    *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      50                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      16                   to   v-pos                  .
           move      "Attenzione: la registrazione del saldaconto non e'
      -              " "                  to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      50                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      16                   to   v-pos                  .
           move      "stata completata per numero eccessivo di righe !"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Accettazione della risposta                 *
      *                  *---------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      65                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Ripristino immagine video                   *
      *                  *---------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       wrt-cat-rig-999.
           exit.

      *    *===========================================================*
      *    * Subroutines per allineamenti a destra o a sinistra oppure *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cps"                   .
