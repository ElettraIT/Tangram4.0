       Identification Division.
       Program-Id.                                 pscf3000           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    scf                 *
      *                                Settore:    mov                 *
      *                                   Fase:    scf300              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 16/09/93    *
      *                       Ultima revisione:    NdK del 20/12/22    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Movimenti per scadenze fornitori            *
      *                                                                *
      *                    Main                                        *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      *                            FUNZIONI                            *
      *                                                                *
      * La fase scf300, nella sua globalita', esegue le funzioni elen- *
      * cate nella tabella seguente :                                  *
      *                                                                *
      *                                                                *
      *   Codice                                                       *
      *    tipo                                                        *
      * operazione          Descrizione Per il tipo operazione         *
      * ----------  -------------------------------------------------- *
      *                                                                *
      *     Overlay pscf300a                                           *
      *                                                                *
      *   1101      Registrazione Rimessa Diretta                      *
      *   1102      Registrazione Incasso Elettronico                  *
      *   1103      Registrazione Ri.Ba.                               *
      *   1104      Registrazione C.d.O.                               *
      *   1105      Registrazione M.Av.                                *
      *   1106      Registrazione R.I.D.                               *
      *   1107      Registrazione Bonifico Bancario                    *
      *   1108      Registrazione C/C Postale                          *
      *   1109      Registrazione Ricevuta Bancaria                    *
      *   1110      Registrazione Tratta                               *
      *   1111      Registrazione Paghero' Cambiario                   *
      *                                                                *
      *   1127      Registrazione Bonifico Bancario su Estero          *
      *                                                                *
      *   1161      Cessione Paghero' Cambiario a Fornitore            *
      *                                                                *
      *                                                                *
      *     Overlay pscf300b                                           *
      *                                                                *
      *   1200      Storno Scadenza                                    *
      *                                                                *
      *                                                                *
      *     Overlay pscf300c                                           *
      *                                                                *
      *   1510      Riscossione per Contanti ___ DA IMPLEMENTARE ___   *
      *   1520      Riscossione con Assegno  ___ DA IMPLEMENTARE ___   *
      *   1530      Riscossione accr. CC     ___ DA IMPLEMENTARE ___   *
      *                                                                *
      *   1610      Pagamento per Contanti                             *
      *   1620      Pagamento con Assegno                              *
      *   1630      Pagamento contro Addebito in C/C Bancario          *
      *   1640      Pagamento contro Addebito in C/C Postale           *
      *   1650      Compensazione scadenze   ___ DA IMPLEMENTARE ___   *
      *                                                                *
      *   1730      Ordine di Bonifico Bancario                        *
      *   1731      Ordine di Bonifico Bancario su Estero              *
      *   1740      Bollettino di C/C Postale                          *
      *   1750      Incarico di pagamento Avviso di Scadenza           *
      *                                                                *
      *                                                                *
      *     Overlay pscf300d                                           *
      *                                                                *
      *   1830      Addebito Bonifici Bancari                          *
      *   1831      Addebito Bonifici Bancari su Estero                *
      *   1840      Addebito Bollettini di C/C Postale                 *
      *   1850      Addebito Avvisi di Scadenza                        *
      *                                                                *
      *                                                                *
      *     Overlay pscf300j                                           *
      *                                                                *
      *   0001      Registrazione Fattura                              *
      *   0002      Registrazione Nota di Addebito                     *
      *   0003      Registrazione Nota di Accredito                    *
      *                                                                *
      *   0007      Registrazione Fattura           Reverse Charge     *
      *   0008      Registrazione Nota di Addebito  Reverse Charge     *
      *   0009      Registrazione Nota di Accredito Reverse Charge     *
      *                                                                *
      *   0011      Registrazione Fattura CEE (Merce)                  *
      *   0012      Registrazione Fattura CEE (Servizi)                *
      *   0022      Registrazione Nota di Addebito CEE                 *
      *   0033      Registrazione Nota di Accredito CEE                *
      *                                                                *
      *   0111      Registrazione Fattura Estero (Merce)               *
      *   0112      Registrazione Fattura Estero (Servizi)             *
      *   0222      Registrazione Nota di Addebito Estero              *
      *   0333      Registrazione Nota di Accredito Estero             *
      *                                                                *
      *   0444      Registrazione Bolla doganale                       *
      *                                                                *
      *                                                                *
      *     Overlay pscf300m                                           *
      *                                                                *
      *   4730      Stampa Ordini di Bonifico Bancario                 *
      *                                                                *
      *                                                                *
      *     Overlay pscf300n                                           *
      *                                                                *
      *   4731      Stampa Ordini di Bonifico Bancario su Estero       *
      *                                                                *
      *                                                                *
      *     Overlay pscf300p                                           *
      *                                                                *
      *   4735      Stampa conferma a Fornitore di avvenuto Bonifico   *
      *   4736      Stampa conferma a Fornitore per Bonifico Estero    *
      *                                                                *
      *                                                                *
      *     Overlay pscf300o                                           *
      *                                                                *
      *   4750      Stampa Incarichi di pagamento Avvisi di Scadenza   *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      *                    AGGIORNAMENTI CONTABILI                     *
      *                                                                *
      *             (Vedi: 'abd/not/Varie/Tangram_scf300)              *
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
      *    * Work-area per variabili di i.p.c.                         *
      *    *-----------------------------------------------------------*
       01  w-ipc.
      *        *-------------------------------------------------------*
      *        * Variabile 'scf300-sub-pgm'                            *
      *        *-------------------------------------------------------*
           05  w-ipc-sub-pgm.
      *            *---------------------------------------------------*
      *            * Valore della variabile                            *
      *            *---------------------------------------------------*
               10  w-ipc-sub-pgm-val.
      *                *-----------------------------------------------*
      *                * Dati relativi all'identificazione             *
      *                *-----------------------------------------------*
                   15  w-ipc-sub-pgm-ide.
      *                    *-------------------------------------------*
      *                    * Sistema applicativo                       *
      *                    *-------------------------------------------*
                       20  w-ipc-sub-pgm-sap
                                          pic  x(03)                  .
      *                    *-------------------------------------------*
      *                    * Area gestionale                           *
      *                    *-------------------------------------------*
                       20  w-ipc-sub-pgm-arg
                                          pic  x(03)                  .
      *                    *-------------------------------------------*
      *                    * Settore gestionale                        *
      *                    *-------------------------------------------*
                       20  w-ipc-sub-pgm-set
                                          pic  x(03)                  .
      *                    *-------------------------------------------*
      *                    * Fase gestionale                           *
      *                    *-------------------------------------------*
                       20  w-ipc-sub-pgm-fas
                                          pic  x(06)                  .
      *                    *-------------------------------------------*
      *                    * Sigla interna del programma               *
      *                    *-------------------------------------------*
                       20  w-ipc-sub-pgm-pro
                                          pic  x(10)                  .
      *                    *-------------------------------------------*
      *                    * Descrizione del programma                 *
      *                    *-------------------------------------------*
                       20  w-ipc-sub-pgm-des
                                          pic  x(40)                  .
      *                *-----------------------------------------------*
      *                * Indicatore                                    *
      *                *  - Spaces : Nessun significato                *
      *                *  - NA     : Il programma e' eseguito come     *
      *                *             sottoprogramma per la movimenta-  *
      *                *             zione scadenze passive senza ag-  *
      *                *             giornamenti contabili             *
      *                *-----------------------------------------------*
                   15  w-ipc-sub-pgm-ind  pic  x(02)                  .

      *    *===========================================================*
      *    * Area di Link per programmi della fase 'pscf3000'          *
      *    *-----------------------------------------------------------*
           copy      "pgm/scf/prg/cpy/pscf3000.pgl"                   .

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
      *        *-------------------------------------------------------*
      *        * Flags di tipo uscita da routines di accettazione      *
      *        *-------------------------------------------------------*
           05  w-cnt-tus.
      *            *---------------------------------------------------*
      *            * Da accettazione campi chiave                      *
      *            *---------------------------------------------------*
               10  w-cnt-tus-acc-key      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Flags di uscita da controlli su tasto Do              *
      *        *-------------------------------------------------------*
           05  w-cnt-tdo.
      *            *---------------------------------------------------*
      *            * Per tasto Do su campi chiave                      *
      *            *---------------------------------------------------*
               10  w-cnt-tdo-key-flg      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Flags di controllo su modalita' di funzionamento      *
      *        *-------------------------------------------------------*
           05  w-cnt-mfu.
      *            *---------------------------------------------------*
      *            * Visualizzazione forzata da segreteria             *
      *            *---------------------------------------------------*
               10  w-cnt-mfu-vis-sgr      pic  x(01)                  .

      *    *===========================================================*
      *    * Record files                                              *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [ada]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/azi/fls/rec/rfada"                          .
      *        *-------------------------------------------------------*
      *        * [sff]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/scf/fls/rec/rfsff"                          .
      *        *-------------------------------------------------------*
      *        * [sfs]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/scf/fls/rec/rfsfs"                          .
      *        *-------------------------------------------------------*
      *        * [sfx]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/scf/fls/rec/rfsfx"                          .
      *        *-------------------------------------------------------*
      *        * [sfp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/scf/fls/rec/rfsfp"                          .
      *        *-------------------------------------------------------*
      *        * [sfa]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/scf/fls/rec/rfsfa"                          .
      *        *-------------------------------------------------------*
      *        * [bef]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/scf/fls/rec/rfbef"                          .
      *        *-------------------------------------------------------*
      *        * [yop]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/scf/fls/rec/rfyop"                          .
      *        *-------------------------------------------------------*
      *        * [xft]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/scf/fls/rec/rfxft"                          .
      *        *-------------------------------------------------------*
      *        * [xfr]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/scf/fls/rec/rfxfr"                          .
      *        *-------------------------------------------------------*
      *        * [fnt]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rffnt"                          .
      *        *-------------------------------------------------------*
      *        * [cli]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfcli"                          .
      *        *-------------------------------------------------------*
      *        * [pdc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfpdc"                          .
      *        *-------------------------------------------------------*
      *        * [mgr]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfmgr"                          .
      *        *-------------------------------------------------------*
      *        * [mgi]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfmgi"                          .
      *        *-------------------------------------------------------*
      *        * [zcc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfzcc"                          .
      *        *-------------------------------------------------------*
      *        * [zci]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfzci"                          .
      *        *-------------------------------------------------------*
      *        * [dcf]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcf/fls/rec/rfdcf"                          .
      *        *-------------------------------------------------------*
      *        * [yfp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcf/fls/rec/rfyfp"                          .
      *        *-------------------------------------------------------*
      *        * [zvl]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfzvl"                          .
      *        *-------------------------------------------------------*
      *        * [cbp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/gep/fls/rec/rfcbp"                          .
      *        *-------------------------------------------------------*
      *        * [obp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/gep/fls/rec/rfobp"                          .
      *        *-------------------------------------------------------*
      *        * [zpg]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/gep/fls/rec/rfzpg"                          .
      *        *-------------------------------------------------------*
      *        * [gep]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/gep/fls/rec/rfgep"                          .
      *        *-------------------------------------------------------*
      *        * [axi]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/abi/fls/rec/rfaxi"                          .
      *        *-------------------------------------------------------*
      *        * [axs]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/abi/fls/rec/rfaxs"                          .
      *        *-------------------------------------------------------*
      *        * [raa]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/rda/fls/rec/rfraa"                          .
      *        *-------------------------------------------------------*
      *        * [ram]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/rda/fls/rec/rfram"                          .
      *        *-------------------------------------------------------*
      *        * [ztr]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/rda/fls/rec/rfztr"                          .
      *        *-------------------------------------------------------*
      *        * [rdaprt]                                              *
      *        *-------------------------------------------------------*
           copy      "pgm/rda/num/rec/rnrdaprt"                       .
      *        *-------------------------------------------------------*
      *        * [sffnum]                                              *
      *        *-------------------------------------------------------*
           copy      "pgm/scf/num/rec/rnsffnum"                       .
      *        *-------------------------------------------------------*
      *        * [sfsnum]                                              *
      *        *-------------------------------------------------------*
           copy      "pgm/scf/num/rec/rnsfsnum"                       .
      *        *-------------------------------------------------------*
      *        * [sfpnum]                                              *
      *        *-------------------------------------------------------*
           copy      "pgm/scf/num/rec/rnsfpnum"                       .
      *        *-------------------------------------------------------*
      *        * [sfanum]                                              *
      *        *-------------------------------------------------------*
           copy      "pgm/scf/num/rec/rnsfanum"                       .
      *        *-------------------------------------------------------*
      *        * [sfonum]                                              *
      *        *-------------------------------------------------------*
           copy      "pgm/scf/num/rec/rnsfonum"                       .
      *        *-------------------------------------------------------*
      *        * [prtivf]                                              *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/num/rec/rnprtivf"                       .
      *        *-------------------------------------------------------*
      *        * [prtcge]                                              *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/num/rec/rnprtcge"                       .

      *    *===========================================================*
      *    * Area di comunicazione per gestione catena rig, con buffer *
      *    * dati in grado di ospitare l'area w-rig di ogni overlay    *
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
               10  filler occurs 1024     pic  x(01)                  .

      *    *===========================================================*
      *    * Area di comunicazione per gestione catena rg2, con buffer *
      *    * dati in grado di ospitare l'area w-rg2 di ogni overlay    *
      *    *-----------------------------------------------------------*
       01  w-cat-rg2.
           05  w-cat-rg2-ope              pic  x(02)                  .
           05  w-cat-rg2-exs              pic  x(01)                  .
           05  w-cat-rg2-num              pic  9(05)                  .
           05  w-cat-rg2-cur              pic  9(05)                  .
           05  w-cat-rg2-prg              pic  9(05)                  .
           05  w-cat-rg2-max              pic  9(05)                  .
           05  w-cat-rg2-app              pic  x(01)                  .
           05  w-cat-rg2-ins              pic  x(01)                  .
           05  w-cat-rg2-new              pic  x(01)                  .
           05  w-cat-rg2-lst              pic  x(01)                  .
           05  w-cat-rg2-buf.
               10  filler occurs 1024     pic  x(01)                  .

      *    *===========================================================*
      *    * Link-area per accettazione codice banca estera fornitore  *
      *    *-----------------------------------------------------------*
           copy      "pgm/scf/prg/cpy/acmnbef0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione tipo operazione per scadenze   *
      *    * fornitori                                                 *
      *    *-----------------------------------------------------------*
           copy      "pgm/scf/prg/cpy/acmnyop0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice fornitore               *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/acmnfnt0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice cliente                 *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/acmncli0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice sottoconto              *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/acmnpdc0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice commerciale fornitore   *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcf/prg/cpy/acmndcf0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice dipendenza fornitore    *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcf/prg/cpy/acoddcf0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione forma di pagamento             *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcf/prg/cpy/acmnyfp0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice valuta                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcc/prg/cpy/acodzvl0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione coefficiente cambio valuta     *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcc/prg/cpy/acoecmb0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice nostra cassa, banca, o  *
      *    * c/c postale                                               *
      *    *-----------------------------------------------------------*
           copy      "pgm/gep/prg/cpy/acdecbp0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione del codice ABI                 *
      *    *-----------------------------------------------------------*
           copy      "pgm/abi/prg/cpy/acmnabi0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione del codice CAB                 *
      *    *-----------------------------------------------------------*
           copy      "pgm/abi/prg/cpy/acmncab0.acl"                   .

      *    *===========================================================*
      *    * Link-area per aggiornamento contabilita' generale, clien- *
      *    * ti, fornitori, iva                                        *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/pcge300z.pgl"                   .

      *    *===========================================================*
      *    * Work per subroutines di Let                               *
      *    *-----------------------------------------------------------*
       01  w-let.
      *        *-------------------------------------------------------*
      *        * Work per lettura Tipo operazione                      *
      *        *-------------------------------------------------------*
           05  w-let-tip-ope.
      *            *---------------------------------------------------*
      *            * Flag di lettura                                   *
      *            *---------------------------------------------------*
               10  w-let-tip-ope-flg      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Codice tipo operazione                            *
      *            *---------------------------------------------------*
               10  w-let-tip-ope-cod      pic  9(04)                  .
      *            *---------------------------------------------------*
      *            * Descrizione per codice tipo operazione            *
      *            *---------------------------------------------------*
               10  w-let-tip-ope-des      pic  x(50)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [pdc]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-pdc.
               10  w-let-arc-pdc-flg      pic  x(01)                  .
               10  w-let-arc-pdc-cod      pic  9(07)                  .
               10  w-let-arc-pdc-des      pic  x(40)                  .

      *    *===========================================================*
      *    * Work per subroutines di Acc                               *
      *    *-----------------------------------------------------------*
       01  w-acc.
      *        *-------------------------------------------------------*
      *        * Work per accettazione primo campo della chiave        *
      *        *-------------------------------------------------------*
           05  w-acc-acc-uno.
      *            *---------------------------------------------------*
      *            * Selettore primo campo da accettare                *
      *            * - D : Data registrazione                          *
      *            * - T : Tipo movimento                              *
      *            *---------------------------------------------------*
               10  w-acc-acc-uno-sel      pic  x(01)                  .

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
      *    * Area di comunicazione per determinazione imposta in base  *
      *    * ad un imponibile                                          *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/dimpiva0.dtl"                   .

      ******************************************************************
       Procedure Division.
      ******************************************************************

      *================================================================*
      *       Main                                                     *
      *================================================================*
       main-000.
      *              *-------------------------------------------------*
      *              * Lettura della variabile di i.p.c. di tipo glo-  *
      *              * bale 'fat300-sub-pgm'                           *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-dic-ini-pgm      .
           perform   ipc-sub-pgm-000      thru ipc-sub-pgm-999        .
      *              *-------------------------------------------------*
      *              * Preparazione parametri di identificazione       *
      *              *-------------------------------------------------*
           perform   pre-par-ide-000      thru pre-par-ide-999        .
      *              *-------------------------------------------------*
      *              * Dichiarazione di inizio programma               *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-dic-ini-pgm      .
           perform   dic-ini-pgm-000      thru dic-ini-pgm-999        .
           if        w-cnt-dic-ini-pgm    not  = spaces
                     go to main-999.
      *              *-------------------------------------------------*
      *              * Visualizzazione titolo programma                *
      *              *-------------------------------------------------*
           perform   vis-tit-pgm-000      thru vis-tit-pgm-999        .
      *              *-------------------------------------------------*
      *              * Esecuzione routine pre-esecuzione programma     *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-pre-exe-pgm      .
           perform   pre-exe-pgm-000      thru pre-exe-pgm-999        .
           if        w-cnt-pre-exe-pgm    not  = spaces
                     go to main-900.
      *              *-------------------------------------------------*
      *              * Esecuzione ciclo per la fase 'scf300'           *
      *              *-------------------------------------------------*
           perform   fas-scf-300-000      thru fas-scf-300-999        .
       main-900.
      *              *-------------------------------------------------*
      *              * Esecuzione routine post-esecuzione programma    *
      *              *-------------------------------------------------*
           perform   pos-exe-pgm-000      thru pos-exe-pgm-999        .
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
      *    * Lettura della variabile di i.p.c. 'scf300-sub-pgm'        *
      *    *-----------------------------------------------------------*
       ipc-sub-pgm-000.
      *              *-------------------------------------------------*
      *              * Normalizzazioni valore della variabile          *
      *              *-------------------------------------------------*
           move      spaces               to   w-ipc-sub-pgm-val      .
       ipc-sub-pgm-100.
      *              *-------------------------------------------------*
      *              * Lettura e cancellazione della variabile globale *
      *              *-------------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "scf300-sub-pgm"     to   s-var                  .
           move      "G"                  to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Se variabile non esistente : uscita             *
      *              *-------------------------------------------------*
           if        s-ves                not  = spaces
                     go to ipc-sub-pgm-999.
      *              *-------------------------------------------------*
      *              * Se variabile di formato inaspettato : uscita    *
      *              *-------------------------------------------------*
           if        s-tip                not  = "A" or
                     s-car                not  = 80
                     go to ipc-sub-pgm-999.
       ipc-sub-pgm-200.
      *              *-------------------------------------------------*
      *              * Valore della variabile in area di bufferizza-   *
      *              * zione                                           *
      *              *-------------------------------------------------*
           move      s-alf                to   w-ipc-sub-pgm-val      .
       ipc-sub-pgm-300.
      *              *-------------------------------------------------*
      *              * Se indicatore di tipo riconosciuto : uscita     *
      *              *-------------------------------------------------*
           if        w-ipc-sub-pgm-ind    =    "NA"
                     go to ipc-sub-pgm-999.
      *              *-------------------------------------------------*
      *              * Normalizzazione valore della variabile          *
      *              *-------------------------------------------------*
           move      spaces               to   w-ipc-sub-pgm-val      .
       ipc-sub-pgm-999.
           exit.

      *    *===========================================================*
      *    * Preparazione parametri di identificazione                 *
      *    *-----------------------------------------------------------*
       pre-par-ide-000.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda che esista o meno la va-   *
      *              * riabile di i.p.c. 'scf300-sub-pgm'              *
      *              *-------------------------------------------------*
           if        w-ipc-sub-pgm-ind    =    "NA"
                     go to pre-par-ide-100
           else      go to pre-par-ide-200.
       pre-par-ide-100.
      *              *-------------------------------------------------*
      *              * Se variabile di i.p.c. 'scf300-sub-pgm' esi-    *
      *              * stente : preparazione area di identificazione   *
      *              * programma secondo la variabile di i.p.c.        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Sistema applicativo                         *
      *                  *---------------------------------------------*
           move      w-ipc-sub-pgm-sap    to   i-ide-sap              .
      *                  *---------------------------------------------*
      *                  * Area gestionale                             *
      *                  *---------------------------------------------*
           move      w-ipc-sub-pgm-arg    to   i-ide-arg              .
      *                  *---------------------------------------------*
      *                  * Settore gestionale                          *
      *                  *---------------------------------------------*
           move      w-ipc-sub-pgm-set    to   i-ide-set              .
      *                  *---------------------------------------------*
      *                  * Fase gestionale                             *
      *                  *---------------------------------------------*
           move      w-ipc-sub-pgm-fas    to   i-ide-fas              .
      *                  *---------------------------------------------*
      *                  * Sigla interna del programma                 *
      *                  *---------------------------------------------*
           move      w-ipc-sub-pgm-pro    to   i-ide-pro              .
      *                  *---------------------------------------------*
      *                  * Descrizione del programma                   *
      *                  *---------------------------------------------*
           move      w-ipc-sub-pgm-des    to   i-ide-des              .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     pre-par-ide-999.
       pre-par-ide-200.
      *              *-------------------------------------------------*
      *              * Se variabile di i.p.c. 'scf300-sub-pgm' non e-  *
      *              * sistente : preparazione area di identificazione *
      *              * programma in modo normale                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Sistema applicativo                         *
      *                  *---------------------------------------------*
           move      "pgm"                to   i-ide-sap              .
      *                  *---------------------------------------------*
      *                  * Area gestionale                             *
      *                  *---------------------------------------------*
           move      "scf"                to   i-ide-arg              .
      *                  *---------------------------------------------*
      *                  * Settore gestionale                          *
      *                  *---------------------------------------------*
           move      "mov"                to   i-ide-set              .
      *                  *---------------------------------------------*
      *                  * Fase gestionale                             *
      *                  *---------------------------------------------*
           move      "scf300"             to   i-ide-fas              .
      *                  *---------------------------------------------*
      *                  * Sigla interna del programma                 *
      *                  *---------------------------------------------*
           move      "pscf3000"           to   i-ide-pro              .
      *                  *---------------------------------------------*
      *                  * Descrizione del programma                   *
      *                  *---------------------------------------------*
           move      "    MOVIMENTI PER SCADENZE FORNITORI    "
                                          to   i-ide-des              .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     pre-par-ide-999.
       pre-par-ide-999.
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
      *    * Routine pre-esecuzione programma                          *
      *    *-----------------------------------------------------------*
       pre-exe-pgm-000.
      *              *-------------------------------------------------*
      *              * Caricamento iniziale dei valori di defaults ge- *
      *              * nerali                                          *
      *              *-------------------------------------------------*
           perform   loa-def-gen-000      thru loa-def-gen-999        .
      *              *-------------------------------------------------*
      *              * Caricamento iniziale della tabella dei tipi o-  *
      *              * perazione                                       *
      *              *-------------------------------------------------*
           perform   loa-tbl-top-000      thru loa-tbl-top-999        .
      *              *-------------------------------------------------*
      *              * Open moduli di accettazione                     *
      *              *-------------------------------------------------*
           perform   opn-mod-acc-000      thru opn-mod-acc-999        .
      *              *-------------------------------------------------*
      *              * Open moduli di aggiornamento                    *
      *              *-------------------------------------------------*
           perform   opn-mod-agg-000      thru opn-mod-agg-999        .
      *              *-------------------------------------------------*
      *              * Open files utilizzati dalla fase in tutte le    *
      *              * sue funzioni                                    *
      *              *-------------------------------------------------*
           perform   opn-fls-fas-000      thru opn-fls-fas-999        .
      *              *-------------------------------------------------*
      *              * Caricamento iniziale delle personalizzazioni    *
      *              *-------------------------------------------------*
           perform   loa-prs-scf-000      thru loa-prs-scf-999        .
      *              *-------------------------------------------------*
      *              * Forzatura al valore 'N' della personalizzazione *
      *              * per aggiornamenti contabili in presenza della   *
      *              * variabile di i.p.c. 'scf300-sub-pgm'            *
      *              *-------------------------------------------------*
           if        w-ipc-sub-pgm-ind    =    "NA"
                     move  "N"            to   w-prs-scf-snx-cge      .
      *              *-------------------------------------------------*
      *              * Caricamento iniziale delle referenze            *
      *              *-------------------------------------------------*
           perform   loa-ref-scf-000      thru loa-ref-scf-999        .
      *              *-------------------------------------------------*
      *              * Determinazione della data di registrazione mi-  *
      *              * nima per i movimenti di contabilita' generale   *
      *              *-------------------------------------------------*
           perform   det-drc-min-000      thru det-drc-min-999        .
      *              *-------------------------------------------------*
      *              * Test se programma eseguibile                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su flag di eventuale visualizzazione   *
      *                  *---------------------------------------------*
           if        w-cnt-mfu-vis-sgr    not  = "V"
                     go to pre-exe-pgm-999.
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
       pre-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Routine post-esecuzione programma                         *
      *    *-----------------------------------------------------------*
       pos-exe-pgm-000.
      *              *-------------------------------------------------*
      *              * Close moduli di accettazione                    *
      *              *-------------------------------------------------*
           perform   cls-mod-acc-000      thru cls-mod-acc-999        .
      *              *-------------------------------------------------*
      *              * Close moduli di aggiornamento                   *
      *              *-------------------------------------------------*
           perform   cls-mod-agg-000      thru cls-mod-agg-999        .
      *              *-------------------------------------------------*
      *              * Close files utilizzati dalla fase in tutte le   *
      *              * sue funzioni                                    *
      *              *-------------------------------------------------*
           perform   cls-fls-fas-000      thru cls-fls-fas-999        .
       pos-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Esecuzione ciclo per la fase 'scf300'                     *
      *    *-----------------------------------------------------------*
       fas-scf-300-000.
      *              *-------------------------------------------------*
      *              * Video in Off                                    *
      *              *-------------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di richiesta registrazione *
      *              * di una nuova scadenza a fronte dell'operazione  *
      *              * appena completata                               *
      *              *-------------------------------------------------*
           move      spaces               to   w-key-ric-rns          .
      *              *-------------------------------------------------*
      *              * Selettore primo campo da accettare della chia-  *
      *              * ve : data di registrazione                      *
      *              *-------------------------------------------------*
           move      "D"                  to   w-acc-acc-uno-sel      .
      *              *-------------------------------------------------*
      *              * Normalizzazione valori di accettazione chiave   *
      *              *-------------------------------------------------*
           perform   nor-val-key-000      thru nor-val-key-999        .
      *              *-------------------------------------------------*
      *              * Preparazione valori pre-cablati per accettazio- *
      *              * ne da valori di default generali                *
      *              *-------------------------------------------------*
           perform   pre-key-def-000      thru pre-key-def-999        .
      *              *-------------------------------------------------*
      *              * Visualizzazione prompts per impostazione campi  *
      *              * chiave                                          *
      *              *-------------------------------------------------*
           perform   pmt-key-reg-000      thru pmt-key-reg-999        .
      *              *-------------------------------------------------*
      *              * Video in On                                     *
      *              *-------------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       fas-scf-300-100.
      *              *-------------------------------------------------*
      *              * Assestamento flags di controllo per registra-   *
      *              * zione nuova scadenza a fronte dell'operazione   *
      *              * appena completata                               *
      *              *-------------------------------------------------*
           if        w-key-ric-rns        =    spaces
                     move  spaces         to   w-key-flg-rns
                     move  zero           to   w-key-dtr-rns
                     move  zero           to   w-key-top-rns
                     move  zero           to   w-key-imp-rns
                     move  zero           to   w-key-tvo-rns
                     move  zero           to   w-key-nvs-rns
                     move  zero           to   w-key-nvp-rns
           else      move  spaces         to   w-key-ric-rns
                     move  "#"            to   w-key-flg-rns          .
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare a Spaces per il ti- *
      *              * po uscita da impostazione campi chiave          *
      *              *-------------------------------------------------*
           move      spaces               to   w-key-tus-ack          .
      *              *-------------------------------------------------*
      *              * Accettazione campi chiave                       *
      *              *-------------------------------------------------*
           perform   acc-key-reg-000      thru acc-key-reg-999        .
      *              *-------------------------------------------------*
      *              * Se tipo uscita "E" : fine programma             *
      *              *-------------------------------------------------*
           if        w-cnt-tus-acc-key    =    "E"
                     go to fas-scf-300-999.
      *              *-------------------------------------------------*
      *              * Richiamo sottoprogramma per esecuzione del tipo *
      *              * operazione                                      *
      *              *-------------------------------------------------*
           perform   ric-spg-top-000      thru ric-spg-top-999        .
       fas-scf-300-200.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo uscita da im-   *
      *              * postazione campi chiave                         *
      *              *-------------------------------------------------*
           if        w-key-tus-ack        =    spaces
                     go to fas-scf-300-300
           else if   w-key-tus-ack        =    "U"
                     go to fas-scf-300-400
           else if   w-key-tus-ack        =    "E"
                     go to fas-scf-300-500
           else      go to fas-scf-300-300.
       fas-scf-300-300.
      *              *-------------------------------------------------*
      *              * Se tipo uscita da impostazione campi chiave al  *
      *              * valore Spaces                                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Video in Off                                *
      *                  *---------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione titolo programma            *
      *                  *---------------------------------------------*
           perform   vis-tit-pgm-000      thru vis-tit-pgm-999        .
      *                  *---------------------------------------------*
      *                  * Normalizzazione valori di accettazione      *
      *                  * chiave                                      *
      *                  *---------------------------------------------*
           perform   nor-val-key-000      thru nor-val-key-999        .
      *                  *---------------------------------------------*
      *                  * Preparazione valori pre-cablati per accet-  *
      *                  * tazione da valori di default generali       *
      *                  *---------------------------------------------*
           perform   pre-key-def-000      thru pre-key-def-999        .
      *                  *---------------------------------------------*
      *                  * Visualizzazione prompts per impostazione    *
      *                  * campi chiave                                *
      *                  *---------------------------------------------*
           perform   pmt-key-reg-000      thru pmt-key-reg-999        .
      *                  *---------------------------------------------*
      *                  * Selettore primo campo da accettare della    *
      *                  * chiave : data di registrazione              *
      *                  *---------------------------------------------*
           move      "D"                  to   w-acc-acc-uno-sel      .
      *                  *---------------------------------------------*
      *                  * Video in On                                 *
      *                  *---------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Riciclo ad impostazione campi chiave        *
      *                  *---------------------------------------------*
           go to     fas-scf-300-100.
       fas-scf-300-400.
      *              *-------------------------------------------------*
      *              * Se tipo uscita da im postazione campi chiave al *
      *              * valore 'U'                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Video in Off                                *
      *                  *---------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione titolo programma            *
      *                  *---------------------------------------------*
           perform   vis-tit-pgm-000      thru vis-tit-pgm-999        .
      *                  *---------------------------------------------*
      *                  * Normalizzazione valori di accettazione      *
      *                  * chiave                                      *
      *                  *---------------------------------------------*
           perform   nor-val-key-000      thru nor-val-key-999        .
      *                  *---------------------------------------------*
      *                  * Preparazione valori pre-cablati per accet-  *
      *                  * tazione da valori di default generali       *
      *                  *---------------------------------------------*
           perform   pre-key-def-000      thru pre-key-def-999        .
      *                  *---------------------------------------------*
      *                  * Visualizzazione prompts per impostazione    *
      *                  * campi chiave                                *
      *                  *---------------------------------------------*
           perform   pmt-key-reg-000      thru pmt-key-reg-999        .
      *                  *---------------------------------------------*
      *                  * Selettore primo campo da accettare della    *
      *                  * chiave : tipo movimento                     *
      *                  *---------------------------------------------*
           move      "T"                  to   w-acc-acc-uno-sel      .
      *                  *---------------------------------------------*
      *                  * Video in On                                 *
      *                  *---------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Riciclo ad impostazione campi chiave        *
      *                  *---------------------------------------------*
           go to     fas-scf-300-100.
       fas-scf-300-500.
      *              *-------------------------------------------------*
      *              * Se tipo uscita da im postazione campi chiave al *
      *              * valore 'E'                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Video in Off                                *
      *                  *---------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione titolo programma            *
      *                  *---------------------------------------------*
           perform   vis-tit-pgm-000      thru vis-tit-pgm-999        .
      *                  *---------------------------------------------*
      *                  * Normalizzazione valori di accettazione      *
      *                  * chiave                                      *
      *                  *---------------------------------------------*
           perform   nor-val-key-000      thru nor-val-key-999        .
      *                  *---------------------------------------------*
      *                  * Preparazione valori pre-cablati per accet-  *
      *                  * tazione da valori di default generali       *
      *                  *---------------------------------------------*
           perform   pre-key-def-000      thru pre-key-def-999        .
      *                  *---------------------------------------------*
      *                  * Visualizzazione prompts per impostazione    *
      *                  * campi chiave                                *
      *                  *---------------------------------------------*
           perform   pmt-key-reg-000      thru pmt-key-reg-999        .
      *                  *---------------------------------------------*
      *                  * Selettore primo campo da accettare della    *
      *                  * chiave : tipo movimento                     *
      *                  *---------------------------------------------*
           move      "T"                  to   w-acc-acc-uno-sel      .
      *                  *---------------------------------------------*
      *                  * Video in On                                 *
      *                  *---------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Riciclo ad impostazione campi chiave        *
      *                  *---------------------------------------------*
           go to     fas-scf-300-100.
       fas-scf-300-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione valori di accettazione chiave             *
      *    *-----------------------------------------------------------*
       nor-val-key-000.
      *              *-------------------------------------------------*
      *              * Data di registrazione                           *
      *              *-------------------------------------------------*
           move      zero                 to   w-key-dat-reg          .
      *              *-------------------------------------------------*
      *              * Codice tipo operazione                          *
      *              *-------------------------------------------------*
           move      zero                 to   w-key-tip-ope          .
      *              *-------------------------------------------------*
      *              * Descrizione per codice tipo operazione          *
      *              *-------------------------------------------------*
           move      spaces               to   w-key-tip-ope-des      .
       nor-val-key-999.
           exit.

      *    *===========================================================*
      *    * Preparazione valori pre-cablati per accettazione campi    *
      *    * chiave, assumendo dai valori di default generali          *
      *    *-----------------------------------------------------------*
       pre-key-def-000.
      *              *-------------------------------------------------*
      *              * Data di registrazione                           *
      *              *-------------------------------------------------*
           move      w-def-dat-reg        to   w-key-dat-reg          .
      *              *-------------------------------------------------*
      *              * Codice tipo operazione                          *
      *              *-------------------------------------------------*
           move      w-def-tip-ope        to   w-key-tip-ope          .
      *              *-------------------------------------------------*
      *              * Lettura codice tipo operazione                  *
      *              *-------------------------------------------------*
           move      w-key-tip-ope        to   w-let-tip-ope-cod      .
           perform   let-tip-ope-000      thru let-tip-ope-999        .
      *              *-------------------------------------------------*
      *              * Descrizione tipo operazione                     *
      *              *-------------------------------------------------*
           move      w-let-tip-ope-des    to   w-key-tip-ope-des      .
       pre-key-def-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per accettazione campi chiave, e  *
      *    * dei valori relativi ai campi pre-cablati per defaults     *
      *    *-----------------------------------------------------------*
       pmt-key-reg-000.
      *              *-------------------------------------------------*
      *              * Erase linee impegnate dalla chiave              *
      *              *-------------------------------------------------*
           move      "EL"                 to   v-ope                  .
           move      04                   to   v-lin                  .
           move      07                   to   v-lto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Prompt per data di registrazione                *
      *              *-------------------------------------------------*
           perform   pmt-dat-reg-000      thru pmt-dat-reg-999        .
      *              *-------------------------------------------------*
      *              * Data di registrazione                           *
      *              *-------------------------------------------------*
           perform   vis-dat-reg-000      thru vis-dat-reg-999        .
      *              *-------------------------------------------------*
      *              * Prompt per tipo operazione                      *
      *              *-------------------------------------------------*
           perform   pmt-tip-ope-000      thru pmt-tip-ope-999        .
      *              *-------------------------------------------------*
      *              * Codice tipo operazione                          *
      *              *-------------------------------------------------*
           perform   vis-tip-ope-000      thru vis-tip-ope-999        .
      *              *-------------------------------------------------*
      *              * Codice tipo operazione, descrizione             *
      *              *-------------------------------------------------*
           perform   vis-tip-ope-des-000  thru vis-tip-ope-des-999    .
      *              *-------------------------------------------------*
      *              * Trattini di separazione                         *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      all   "-"            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-key-reg-999.
           exit.

      *    *===========================================================*
      *    * Prompt per data di registrazione                          *
      *    *-----------------------------------------------------------*
       pmt-dat-reg-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Data registrazione :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-dat-reg-999.
           exit.

      *    *===========================================================*
      *    * Prompt per tipo operazione                                *
      *    *-----------------------------------------------------------*
       pmt-tip-ope-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tipo operazione    :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-tip-ope-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campi chiave                                 *
      *    *-----------------------------------------------------------*
       acc-key-reg-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-tus-acc-key      .
      *              *-------------------------------------------------*
      *              * Se si e' in fase di registrazione nuova scaden- *
      *              * za a fronte dell'operazione appena completata,  *
      *              * l'accettazione inizia dalla data di registra-   *
      *              * zione                                           *
      *              *-------------------------------------------------*
           if        w-key-flg-rns        not  = spaces
                     go to acc-key-reg-100.
      *              *-------------------------------------------------*
      *              * Altrimenti, caso normale, l'accettazione ha i-  *
      *              * nizio a seconda del valore del Selettore primo  *
      *              * campo da accettare della chiave                 *
      *              *-------------------------------------------------*
           if        w-acc-acc-uno-sel    =    "T"
                     go to acc-key-reg-200
           else      go to acc-key-reg-100.
       acc-key-reg-100.
      *              *-------------------------------------------------*
      *              * Accettazione Data registrazione                 *
      *              *-------------------------------------------------*
           perform   acc-dat-reg-000      thru acc-dat-reg-999        .
           if        w-cnt-tus-acc-key    not  = spaces
                     go to acc-key-reg-999.
       acc-key-reg-200.
      *              *-------------------------------------------------*
      *              * Accettazione Tipo operazione                    *
      *              *-------------------------------------------------*
           perform   acc-tip-ope-000      thru acc-tip-ope-999        .
           if        w-cnt-tus-acc-key    not  = spaces
                     go to acc-key-reg-999.
           if        v-key                =    "UP  "
                     go to acc-key-reg-100.
       acc-key-reg-999.
           exit.

      *    *===========================================================*
      *    * Accettazione Data registrazione                           *
      *    *-----------------------------------------------------------*
       acc-dat-reg-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-dat-reg-025.
      *                  *---------------------------------------------*
      *                  * Se in fase di emissione nuova scadenza a    *
      *                  * fronte dell'operazione appena completata    *
      *                  * si forza come data di registrazione la data *
      *                  * di registrazione per emissione nuova sca-   *
      *                  * denza, la si visualizza, e si continua      *
      *                  * dopo l'accettazione stessa.                 *
      *                  * Se la data di registrazione per emissione   *
      *                  * nuova scadenza e' a zero si forza la data   *
      *                  * di sistema.                                 *
      *                  *---------------------------------------------*
       acc-dat-reg-030.
      *                      *-----------------------------------------*
      *                      * Se non in fase di registrazione nuova   *
      *                      * scadenza a fronte dell'operazione appe- *
      *                      * na completata : ad accettazione         *
      *                      *-----------------------------------------*
           if        w-key-flg-rns        =    spaces
                     go to acc-dat-reg-100.
      *                      *-----------------------------------------*
      *                      * Se la data di registrazione per regi-   *
      *                      * strazione nuova scadenza e' diversa da  *
      *                      * zero, la si pone e si omette la lettura *
      *                      * della data di sistema                   *
      *                      *-----------------------------------------*
           if        w-key-dtr-rns        not  = zero
                     move  w-key-dtr-rns  to   w-key-dat-reg
                     go to acc-dat-reg-040.
       acc-dat-reg-035.
      *                      *-----------------------------------------*
      *                      * Lettura data di sistema da segreteria   *
      *                      *-----------------------------------------*
           move      "DT"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                      *-----------------------------------------*
      *                      * Data di sistema in data di registrazio- *
      *                      * ne                                      *
      *                      *-----------------------------------------*
           move      s-dat                to   w-key-dat-reg          .
       acc-dat-reg-040.
      *                      *-----------------------------------------*
      *                      * Visualizzazione data di registrazione   *
      *                      *-----------------------------------------*
           perform   vis-dat-reg-000      thru vis-dat-reg-999        .
      *                      *-----------------------------------------*
      *                      * Normalizzazione function-key di impo-   *
      *                      * stazione                                *
      *                      *-----------------------------------------*
           move      spaces               to   v-key                  .
      *                      *-----------------------------------------*
      *                      * A post accettazione                     *
      *                      *-----------------------------------------*
           go to     acc-dat-reg-400.
       acc-dat-reg-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se viene eseguita l'accettazione effettiva  *
      *                  * si pone comunque in Off il flag di regi-    *
      *                  * strazione nuova scadenza a fronte dell'ope- *
      *                  * razione appena completata                   *
      *                  *---------------------------------------------*
           move      spaces               to   w-key-flg-rns          .
      *                  *---------------------------------------------*
      *                  * Accettazione effettiva                      *
      *                  *---------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      04                   to   v-lin                  .
           move      22                   to   v-pos                  .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      w-key-dat-reg        to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-dat-reg-999.
       acc-dat-reg-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-dat                to   w-key-dat-reg          .
       acc-dat-reg-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test che la data di registrazione sia di-   *
      *                  * versa da zero                               *
      *                  *---------------------------------------------*
           if        w-key-dat-reg        =    zero
                     go to acc-dat-reg-100.
       acc-dat-reg-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-dat-reg-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                not  = "DO  "
                     go to acc-dat-reg-999.
           perform   cnt-tdo-key-000      thru cnt-tdo-key-999        .
           if        w-cnt-tdo-key-flg    not  = spaces
                     go to acc-dat-reg-100
           else      move  "S"            to   w-cnt-tus-acc-key
                     go to acc-dat-reg-999.
       acc-dat-reg-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione Data registrazione                        *
      *    *-----------------------------------------------------------*
       vis-dat-reg-000.
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      04                   to   v-lin                  .
           move      22                   to   v-pos                  .
           move      w-key-dat-reg        to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       vis-dat-reg-999.
           exit.

      *    *===========================================================*
      *    * Accettazione Codice tipo operazione                       *
      *    *-----------------------------------------------------------*
       acc-tip-ope-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se in fase di emissione nuova scadenza a    *
      *                  * fronte dell'operazione appena completata    *
      *                  * si forza come tipo operazione quello cor-   *
      *                  * rispondente al tipo della nuova scadenza    *
      *                  * da emettere, lo si visualizza, e si conti-  *
      *                  * nua dopo l'accettazione stessa              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se non in fase di registrazione nuova   *
      *                      * scadenza a fronte dell'operazione appe- *
      *                      * na completata : ad accettazione         *
      *                      *-----------------------------------------*
           if        w-key-flg-rns        =    spaces
                     go to acc-tip-ope-100.
      *                      *-----------------------------------------*
      *                      * Determinazione tipo operazione          *
      *                      *-----------------------------------------*
           move      w-key-top-rns        to   w-key-tip-ope          .
      *                      *-----------------------------------------*
      *                      * Visualizzazione tipo operazione         *
      *                      *-----------------------------------------*
           perform   vis-tip-ope-000      thru vis-tip-ope-999        .
      *                      *-----------------------------------------*
      *                      * Normalizzazione function-key di impo-   *
      *                      * stazione                                *
      *                      *-----------------------------------------*
           move      spaces               to   v-key                  .
      *                      *-----------------------------------------*
      *                      * A post accettazione                     *
      *                      *-----------------------------------------*
           go to     acc-tip-ope-400.
       acc-tip-ope-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se viene eseguita l'accettazione effettiva  *
      *                  * si pone comunque in Off il flag di regi-    *
      *                  * strazione nuova scadenza a fronte dell'ope- *
      *                  * razione appena completata                   *
      *                  *---------------------------------------------*
           move      spaces               to   w-key-flg-rns          .
      *                  *---------------------------------------------*
      *                  * Accettazione effettiva                      *
      *                  *---------------------------------------------*
           move      "AC"                 to   w-cod-mne-yop-ope      .
           move      w-key-tip-ope        to   w-cod-mne-yop-cod      .
           move      05                   to   w-cod-mne-yop-lin      .
           move      22                   to   w-cod-mne-yop-pos      .
           move      05                   to   w-cod-mne-yop-dln      .
           move      27                   to   w-cod-mne-yop-dps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "EXIT"               to   v-pfk (20)             .
           perform   cod-mne-yop-cll-000  thru cod-mne-yop-cll-999    .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           perform   cod-mne-yop-foi-000  thru cod-mne-yop-foi-999    .
       acc-tip-ope-110.
           perform   cod-mne-yop-cll-000  thru cod-mne-yop-cll-999    .
           if        w-cod-mne-yop-ope    =    "F+"
                     go to acc-tip-ope-115.
           if        w-cod-mne-yop-ope    =    "AC"
                     go to acc-tip-ope-120.
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       acc-tip-ope-115.
           perform   cod-mne-yop-foi-000  thru cod-mne-yop-foi-999    .
           go to     acc-tip-ope-110.
       acc-tip-ope-120.
           move      w-cod-mne-yop-cod    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-tip-ope-999.
       acc-tip-ope-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-key-tip-ope          .
       acc-tip-ope-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura codice tipo operazione              *
      *                  *---------------------------------------------*
           move      w-key-tip-ope        to   w-let-tip-ope-cod      .
           perform   let-tip-ope-000      thru let-tip-ope-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione descrizione tipo operazione  *
      *                  *---------------------------------------------*
           move      w-let-tip-ope-des    to   w-key-tip-ope-des      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione tipo operazione *
      *                  *---------------------------------------------*
           perform   vis-tip-ope-des-000  thru vis-tip-ope-des-999    .
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda se tipo operazione e-  *
      *                  * sistente o non esistente                    *
      *                  *---------------------------------------------*
           if        w-let-tip-ope-flg    =    spaces
                     go to acc-tip-ope-450.
       acc-tip-ope-425.
      *                  *---------------------------------------------*
      *                  * Se tipo operazione non esistente            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * A reimpostazione                        *
      *                      *-----------------------------------------*
           go to     acc-tip-ope-100.
       acc-tip-ope-450.
      *                  *---------------------------------------------*
      *                  * Se tipo operazione esistente                *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Deviazione a seconda del valore         *
      *                      *-----------------------------------------*
           if        w-key-tip-ope        =    zero
                     go to acc-tip-ope-500
           else      go to acc-tip-ope-550.
       acc-tip-ope-500.
      *                      *-----------------------------------------*
      *                      * Se valore a zero                        *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se non si e' in Up : reimpostazione *
      *                          *-------------------------------------*
           if        v-key                not  = "UP  "
                     go to acc-tip-ope-100.
      *                          *-------------------------------------*
      *                          * Se si e' in Up : continuazione      *
      *                          *-------------------------------------*
           go to     acc-tip-ope-600.
       acc-tip-ope-550.
      *                      *-----------------------------------------*
      *                      * Se valore diverso da zero               *
      *                      *-----------------------------------------*
           go to     acc-tip-ope-600.
       acc-tip-ope-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-tip-ope-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                not  = "DO  "
                     go to acc-tip-ope-999.
           perform   cnt-tdo-key-000      thru cnt-tdo-key-999        .
           if        w-cnt-tdo-key-flg    not  = spaces
                     go to acc-tip-ope-100
           else      move  "S"            to   w-cnt-tus-acc-key
                     go to acc-tip-ope-999.
       acc-tip-ope-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione Codice tipo operazione                    *
      *    *-----------------------------------------------------------*
       vis-tip-ope-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      05                   to   v-lin                  .
           move      22                   to   v-pos                  .
           move      w-key-tip-ope        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tip-ope-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione Descrizione per codice tipo operazione    *
      *    *-----------------------------------------------------------*
       vis-tip-ope-des-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      50                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      27                   to   v-pos                  .
           move      w-key-tip-ope-des    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       vis-tip-ope-des-999.
           exit.

      *    *===========================================================*
      *    * Lettura Tipo operazione                                   *
      *    *-----------------------------------------------------------*
       let-tip-ope-000.
      *              *-------------------------------------------------*
      *              * Test se codice tipo operazione a zero           *
      *              *-------------------------------------------------*
           if        w-let-tip-ope-cod    =    zero
                     go to let-tip-ope-600.
       let-tip-ope-100.
      *              *-------------------------------------------------*
      *              * Ricerca codice tipo operazione in tabella       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Indice di comodo a zero                     *
      *                  *---------------------------------------------*
           move      zero                 to   w-top-ele-inx          .
       let-tip-ope-200.
      *                  *---------------------------------------------*
      *                  * Incremento indice di comodo                 *
      *                  *---------------------------------------------*
           add       1                    to   w-top-ele-inx          .
      *                  *---------------------------------------------*
      *                  * Test se superato il massimo numero di ele-  *
      *                  * menti in tabella tipi operazione; se supe-  *
      *                  * rato si va ad elemento non trovato          *
      *                  *---------------------------------------------*
           if        w-top-ele-inx        >    w-top-ele-num
                     go to let-tip-ope-400.
      *                  *---------------------------------------------*
      *                  * Test se l'elemento in esame e' quello cer-  *
      *                  * cato; se non lo e' si ricicla all'elemento  *
      *                  * successivo                                  *
      *                  *---------------------------------------------*
           if        w-top-cod-top
                    (w-top-ele-inx)       not  = w-let-tip-ope-cod
                     go to let-tip-ope-200.
       let-tip-ope-300.
      *                  *---------------------------------------------*
      *                  * Se elemento trovato                         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione parametri in uscita        *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Descrizione per tipo operazione     *
      *                          *-------------------------------------*
           move      w-top-des-top
                    (w-top-ele-inx)       to   w-let-tip-ope-des      .
      *                          *-------------------------------------*
      *                          * Flag di uscita a trovato            *
      *                          *-------------------------------------*
           move      spaces               to   w-let-tip-ope-flg      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     let-tip-ope-999.
       let-tip-ope-400.
      *                  *---------------------------------------------*
      *                  * Se elemento non trovato                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione parametri in uscita        *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Descrizione per tipo operazione     *
      *                          *-------------------------------------*
           move      all   "."            to   w-let-tip-ope-des      .
      *                          *-------------------------------------*
      *                          * Flag di uscita a non trovato        *
      *                          *-------------------------------------*
           move      "#"                  to   w-let-tip-ope-flg      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     let-tip-ope-999.
       let-tip-ope-600.
      *              *-------------------------------------------------*
      *              * Se codice tipo operazione a zero                *
      *              *-------------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione parametri in uscita        *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Descrizione per tipo operazione     *
      *                          *-------------------------------------*
           move      spaces               to   w-let-tip-ope-des      .
      *                          *-------------------------------------*
      *                          * Flag di uscita a trovato            *
      *                          *-------------------------------------*
           move      spaces               to   w-let-tip-ope-flg      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     let-tip-ope-999.
       let-tip-ope-999.
           exit.

      *    *===========================================================*
      *    * Controllo su impostazione tasto Do                        *
      *    *-----------------------------------------------------------*
       cnt-tdo-key-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-tdo-key-flg      .
       cnt-tdo-key-100.
      *              *-------------------------------------------------*
      *              * Test su data di registrazione, che non manchi   *
      *              *-------------------------------------------------*
           if        w-key-dat-reg        =    zero
                     move  "#"            to   w-cnt-tdo-key-flg
                     go to cnt-tdo-key-999.
       cnt-tdo-key-200.
      *              *-------------------------------------------------*
      *              * Test su tipo operazione, che non manchi         *
      *              *-------------------------------------------------*
           if        w-key-tip-ope        =    zero
                     move  "#"            to   w-cnt-tdo-key-flg
                     go to cnt-tdo-key-999.
       cnt-tdo-key-999.
           exit.
           
      *    *===========================================================*
      *    * Richiamo sottoprogramma per esecuzione Tipo Operazione    *
      *    *-----------------------------------------------------------*
       ric-spg-top-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo operazione      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se tipo operazione  0001                    *
      *                  *---------------------------------------------*
           if        w-key-tip-ope        =    0001 or
                     w-key-tip-ope        =    0002 or
                     w-key-tip-ope        =    0003 or
                     w-key-tip-ope        =    0007 or
                     w-key-tip-ope        =    0008 or
                     w-key-tip-ope        =    0009 or
                     w-key-tip-ope        =    0011 or
                     w-key-tip-ope        =    0012 or
                     w-key-tip-ope        =    0022 or
                     w-key-tip-ope        =    0033 or
                     w-key-tip-ope        =    0111 or
                     w-key-tip-ope        =    0112 or
                     w-key-tip-ope        =    0222 or
                     w-key-tip-ope        =    0333 or
                     w-key-tip-ope        =    0444
                     perform   ric-000-001-000
                                          thru ric-000-001-999
      *                  *---------------------------------------------*
      *                  * Se tipo operazione  1101                    *
      *                  *---------------------------------------------*
           else if   w-key-tip-ope        =    1101
                     perform   ric-001-101-000
                                          thru ric-001-101-999
      *                  *---------------------------------------------*
      *                  * Se tipo operazione  1102                    *
      *                  *---------------------------------------------*
           else if   w-key-tip-ope        =    1102
                     perform   ric-001-102-000
                                          thru ric-001-102-999
      *                  *---------------------------------------------*
      *                  * Se tipo operazione  1103                    *
      *                  *---------------------------------------------*
           else if   w-key-tip-ope        =    1103
                     perform   ric-001-103-000
                                          thru ric-001-103-999
      *                  *---------------------------------------------*
      *                  * Se tipo operazione  1104                    *
      *                  *---------------------------------------------*
           else if   w-key-tip-ope        =    1104
                     perform   ric-001-104-000
                                          thru ric-001-104-999
      *                  *---------------------------------------------*
      *                  * Se tipo operazione  1105                    *
      *                  *---------------------------------------------*
           else if   w-key-tip-ope        =    1105
                     perform   ric-001-105-000
                                          thru ric-001-105-999
      *                  *---------------------------------------------*
      *                  * Se tipo operazione  1106                    *
      *                  *---------------------------------------------*
           else if   w-key-tip-ope        =    1106
                     perform   ric-001-106-000
                                          thru ric-001-106-999
      *                  *---------------------------------------------*
      *                  * Se tipo operazione  1107                    *
      *                  *---------------------------------------------*
           else if   w-key-tip-ope        =    1107
                     perform   ric-001-107-000
                                          thru ric-001-107-999
      *                  *---------------------------------------------*
      *                  * Se tipo operazione  1108                    *
      *                  *---------------------------------------------*
           else if   w-key-tip-ope        =    1108
                     perform   ric-001-108-000
                                          thru ric-001-108-999
      *                  *---------------------------------------------*
      *                  * Se tipo operazione  1109                    *
      *                  *---------------------------------------------*
           else if   w-key-tip-ope        =    1109
                     perform   ric-001-109-000
                                          thru ric-001-109-999
      *                  *---------------------------------------------*
      *                  * Se tipo operazione  1110                    *
      *                  *---------------------------------------------*
           else if   w-key-tip-ope        =    1110
                     perform   ric-001-110-000
                                          thru ric-001-110-999
      *                  *---------------------------------------------*
      *                  * Se tipo operazione  1111                    *
      *                  *---------------------------------------------*
           else if   w-key-tip-ope        =    1111
                     perform   ric-001-111-000
                                          thru ric-001-111-999
      *                  *---------------------------------------------*
      *                  * Se tipo operazione  1127                    *
      *                  *---------------------------------------------*
           else if   w-key-tip-ope        =    1127
                     perform   ric-001-127-000
                                          thru ric-001-127-999
      *                  *---------------------------------------------*
      *                  * Se tipo operazione  1161                    *
      *                  *---------------------------------------------*
           else if   w-key-tip-ope        =    1161
                     perform   ric-001-161-000
                                          thru ric-001-161-999
      *                  *---------------------------------------------*
      *                  * Se tipo operazione  1200                    *
      *                  *---------------------------------------------*
           else if   w-key-tip-ope        =    1200
                     perform   ric-001-200-000
                                          thru ric-001-200-999
      *                  *---------------------------------------------*
      *                  * Se tipo operazione  1510                    *
      *                  *---------------------------------------------*
           else if   w-key-tip-ope        =    1510
                     perform   ric-001-510-000
                                          thru ric-001-510-999
      *                  *---------------------------------------------*
      *                  * Se tipo operazione  1520                    *
      *                  *---------------------------------------------*
           else if   w-key-tip-ope        =    1520
                     perform   ric-001-520-000
                                          thru ric-001-520-999
      *                  *---------------------------------------------*
      *                  * Se tipo operazione  1530                    *
      *                  *---------------------------------------------*
           else if   w-key-tip-ope        =    1530
                     perform   ric-001-530-000
                                          thru ric-001-530-999
      *                  *---------------------------------------------*
      *                  * Se tipo operazione  1610                    *
      *                  *---------------------------------------------*
           else if   w-key-tip-ope        =    1610
                     perform   ric-001-610-000
                                          thru ric-001-610-999
      *                  *---------------------------------------------*
      *                  * Se tipo operazione  1620                    *
      *                  *---------------------------------------------*
           else if   w-key-tip-ope        =    1620
                     perform   ric-001-620-000
                                          thru ric-001-620-999
      *                  *---------------------------------------------*
      *                  * Se tipo operazione  1630                    *
      *                  *---------------------------------------------*
           else if   w-key-tip-ope        =    1630
                     perform   ric-001-630-000
                                          thru ric-001-630-999
      *                  *---------------------------------------------*
      *                  * Se tipo operazione  1640                    *
      *                  *---------------------------------------------*
           else if   w-key-tip-ope        =    1640
                     perform   ric-001-640-000
                                          thru ric-001-640-999
      *                  *---------------------------------------------*
      *                  * Se tipo operazione  1650                    *
      *                  *---------------------------------------------*
______*    else if   w-key-tip-ope        =    1650
______*              perform   ric-001-650-000
______*                                   thru ric-001-650-999
      *                  *---------------------------------------------*
      *                  * Se tipo operazione  1730                    *
      *                  *---------------------------------------------*
           else if   w-key-tip-ope        =    1730
                     perform   ric-001-730-000
                                          thru ric-001-730-999
      *                  *---------------------------------------------*
      *                  * Se tipo operazione  1731                    *
      *                  *---------------------------------------------*
           else if   w-key-tip-ope        =    1731
                     perform   ric-001-731-000
                                          thru ric-001-731-999
      *                  *---------------------------------------------*
      *                  * Se tipo operazione  1740                    *
      *                  *---------------------------------------------*
           else if   w-key-tip-ope        =    1740
                     perform   ric-001-740-000
                                          thru ric-001-740-999
      *                  *---------------------------------------------*
      *                  * Se tipo operazione  1750                    *
      *                  *---------------------------------------------*
           else if   w-key-tip-ope        =    1750
                     perform   ric-001-750-000
                                          thru ric-001-750-999
      *                  *---------------------------------------------*
      *                  * Se tipo operazione  1830                    *
      *                  *---------------------------------------------*
           else if   w-key-tip-ope        =    1830
                     perform   ric-001-830-000
                                          thru ric-001-830-999
      *                  *---------------------------------------------*
      *                  * Se tipo operazione  1831                    *
      *                  *---------------------------------------------*
           else if   w-key-tip-ope        =    1831
                     perform   ric-001-831-000
                                          thru ric-001-831-999
      *                  *---------------------------------------------*
      *                  * Se tipo operazione  1840                    *
      *                  *---------------------------------------------*
           else if   w-key-tip-ope        =    1840
                     perform   ric-001-840-000
                                          thru ric-001-840-999
      *                  *---------------------------------------------*
      *                  * Se tipo operazione  1850                    *
      *                  *---------------------------------------------*
           else if   w-key-tip-ope        =    1850
                     perform   ric-001-850-000
                                          thru ric-001-850-999
      *                  *---------------------------------------------*
      *                  * Se tipo operazione  4730                    *
      *                  *---------------------------------------------*
           else if   w-key-tip-ope        =    4730
                     perform   ric-004-730-000
                                          thru ric-004-730-999
      *                  *---------------------------------------------*
      *                  * Se tipo operazione  4731                    *
      *                  *---------------------------------------------*
           else if   w-key-tip-ope        =    4731
                     perform   ric-004-731-000
                                          thru ric-004-731-999
      *                  *---------------------------------------------*
      *                  * Se tipo operazione  4735                    *
      *                  *---------------------------------------------*
           else if   w-key-tip-ope        =    4735
                     perform   ric-004-735-000
                                          thru ric-004-735-999
      *                  *---------------------------------------------*
      *                  * Se tipo operazione  4736                    *
      *                  *---------------------------------------------*
           else if   w-key-tip-ope        =    4736
                     perform   ric-004-736-000
                                          thru ric-004-736-999
      *                  *---------------------------------------------*
      *                  * Se tipo operazione  4750                    *
      *                  *---------------------------------------------*
           else if   w-key-tip-ope        =    4750
                     perform   ric-004-750-000
                                          thru ric-004-750-999
      *                  *---------------------------------------------*
      *                  * Se tipo operazione  non riconosciuto        *
      *                  *---------------------------------------------*
           else      go to ric-spg-top-999.
       ric-spg-top-999.
           exit.

      *    *===========================================================*
      *    * Richiamo sottoprogramma per tipi operazione fatture       *
      *    *-----------------------------------------------------------*
       ric-000-001-000.
      *              *-------------------------------------------------*
      *              * Richiamo sottoprogramma di esecuzione           *
      *              *-------------------------------------------------*
           move      spaces               to   w-key-tip-fun          .
           move      "pgm/scf/prg/obj/pscf300j"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using i-ide
                                               w-prs
                                               w-ref
                                               w-top
                                               w-def
                                               w-key                  .
      *              *-------------------------------------------------*
      *              * Cancellazione sottoprogramma di esecuzione      *
      *              *-------------------------------------------------*
           cancel    s-pat                                            .
       ric-000-001-999.
           exit.

      *    *===========================================================*
      *    * Richiamo sottoprogramma per tipo operazione  1101         *
      *    *-----------------------------------------------------------*
       ric-001-101-000.
      *              *-------------------------------------------------*
      *              * Richiamo sottoprogramma di esecuzione           *
      *              *-------------------------------------------------*
           move      spaces               to   w-key-tip-fun          .
           move      "pgm/scf/prg/obj/pscf300a"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using i-ide
                                               w-prs
                                               w-ref
                                               w-top
                                               w-def
                                               w-key                  .
      *              *-------------------------------------------------*
      *              * Cancellazione sottoprogramma di esecuzione      *
      *              *-------------------------------------------------*
           cancel    s-pat                                            .
       ric-001-101-999.
           exit.

      *    *===========================================================*
      *    * Richiamo sottoprogramma per tipo operazione  1102         *
      *    *-----------------------------------------------------------*
       ric-001-102-000.
      *              *-------------------------------------------------*
      *              * Richiamo sottoprogramma di esecuzione           *
      *              *-------------------------------------------------*
           move      spaces               to   w-key-tip-fun          .
           move      "pgm/scf/prg/obj/pscf300a"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using i-ide
                                               w-prs
                                               w-ref
                                               w-top
                                               w-def
                                               w-key                  .
      *              *-------------------------------------------------*
      *              * Cancellazione sottoprogramma di esecuzione      *
      *              *-------------------------------------------------*
           cancel    s-pat                                            .
       ric-001-102-999.
           exit.

      *    *===========================================================*
      *    * Richiamo sottoprogramma per tipo operazione  1103         *
      *    *-----------------------------------------------------------*
       ric-001-103-000.
      *              *-------------------------------------------------*
      *              * Richiamo sottoprogramma di esecuzione           *
      *              *-------------------------------------------------*
           move      spaces               to   w-key-tip-fun          .
           move      "pgm/scf/prg/obj/pscf300a"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using i-ide
                                               w-prs
                                               w-ref
                                               w-top
                                               w-def
                                               w-key                  .
      *              *-------------------------------------------------*
      *              * Cancellazione sottoprogramma di esecuzione      *
      *              *-------------------------------------------------*
           cancel    s-pat                                            .
       ric-001-103-999.
           exit.

      *    *===========================================================*
      *    * Richiamo sottoprogramma per tipo operazione  1104         *
      *    *-----------------------------------------------------------*
       ric-001-104-000.
      *              *-------------------------------------------------*
      *              * Richiamo sottoprogramma di esecuzione           *
      *              *-------------------------------------------------*
           move      spaces               to   w-key-tip-fun          .
           move      "pgm/scf/prg/obj/pscf300a"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using i-ide
                                               w-prs
                                               w-ref
                                               w-top
                                               w-def
                                               w-key                  .
      *              *-------------------------------------------------*
      *              * Cancellazione sottoprogramma di esecuzione      *
      *              *-------------------------------------------------*
           cancel    s-pat                                            .
       ric-001-104-999.
           exit.

      *    *===========================================================*
      *    * Richiamo sottoprogramma per tipo operazione  1105         *
      *    *-----------------------------------------------------------*
       ric-001-105-000.
      *              *-------------------------------------------------*
      *              * Richiamo sottoprogramma di esecuzione           *
      *              *-------------------------------------------------*
           move      spaces               to   w-key-tip-fun          .
           move      "pgm/scf/prg/obj/pscf300a"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using i-ide
                                               w-prs
                                               w-ref
                                               w-top
                                               w-def
                                               w-key                  .
      *              *-------------------------------------------------*
      *              * Cancellazione sottoprogramma di esecuzione      *
      *              *-------------------------------------------------*
           cancel    s-pat                                            .
       ric-001-105-999.
           exit.

      *    *===========================================================*
      *    * Richiamo sottoprogramma per tipo operazione  1106         *
      *    *-----------------------------------------------------------*
       ric-001-106-000.
      *              *-------------------------------------------------*
      *              * Richiamo sottoprogramma di esecuzione           *
      *              *-------------------------------------------------*
           move      spaces               to   w-key-tip-fun          .
           move      "pgm/scf/prg/obj/pscf300a"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using i-ide
                                               w-prs
                                               w-ref
                                               w-top
                                               w-def
                                               w-key                  .
      *              *-------------------------------------------------*
      *              * Cancellazione sottoprogramma di esecuzione      *
      *              *-------------------------------------------------*
           cancel    s-pat                                            .
       ric-001-106-999.
           exit.

      *    *===========================================================*
      *    * Richiamo sottoprogramma per tipo operazione  1107         *
      *    *-----------------------------------------------------------*
       ric-001-107-000.
      *              *-------------------------------------------------*
      *              * Richiamo sottoprogramma di esecuzione           *
      *              *-------------------------------------------------*
           move      spaces               to   w-key-tip-fun          .
           move      "pgm/scf/prg/obj/pscf300a"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using i-ide
                                               w-prs
                                               w-ref
                                               w-top
                                               w-def
                                               w-key                  .
      *              *-------------------------------------------------*
      *              * Cancellazione sottoprogramma di esecuzione      *
      *              *-------------------------------------------------*
           cancel    s-pat                                            .
       ric-001-107-999.
           exit.

      *    *===========================================================*
      *    * Richiamo sottoprogramma per tipo operazione  1108         *
      *    *-----------------------------------------------------------*
       ric-001-108-000.
      *              *-------------------------------------------------*
      *              * Richiamo sottoprogramma di esecuzione           *
      *              *-------------------------------------------------*
           move      spaces               to   w-key-tip-fun          .
           move      "pgm/scf/prg/obj/pscf300a"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using i-ide
                                               w-prs
                                               w-ref
                                               w-top
                                               w-def
                                               w-key                  .
      *              *-------------------------------------------------*
      *              * Cancellazione sottoprogramma di esecuzione      *
      *              *-------------------------------------------------*
           cancel    s-pat                                            .
       ric-001-108-999.
           exit.

      *    *===========================================================*
      *    * Richiamo sottoprogramma per tipo operazione  1109         *
      *    *-----------------------------------------------------------*
       ric-001-109-000.
      *              *-------------------------------------------------*
      *              * Richiamo sottoprogramma di esecuzione           *
      *              *-------------------------------------------------*
           move      spaces               to   w-key-tip-fun          .
           move      "pgm/scf/prg/obj/pscf300a"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using i-ide
                                               w-prs
                                               w-ref
                                               w-top
                                               w-def
                                               w-key                  .
      *              *-------------------------------------------------*
      *              * Cancellazione sottoprogramma di esecuzione      *
      *              *-------------------------------------------------*
           cancel    s-pat                                            .
       ric-001-109-999.
           exit.

      *    *===========================================================*
      *    * Richiamo sottoprogramma per tipo operazione  1110         *
      *    *-----------------------------------------------------------*
       ric-001-110-000.
      *              *-------------------------------------------------*
      *              * Richiamo sottoprogramma di esecuzione           *
      *              *-------------------------------------------------*
           move      spaces               to   w-key-tip-fun          .
           move      "pgm/scf/prg/obj/pscf300a"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using i-ide
                                               w-prs
                                               w-ref
                                               w-top
                                               w-def
                                               w-key                  .
      *              *-------------------------------------------------*
      *              * Cancellazione sottoprogramma di esecuzione      *
      *              *-------------------------------------------------*
           cancel    s-pat                                            .
       ric-001-110-999.
           exit.

      *    *===========================================================*
      *    * Richiamo sottoprogramma per tipo operazione  1111         *
      *    *-----------------------------------------------------------*
       ric-001-111-000.
      *              *-------------------------------------------------*
      *              * Richiamo sottoprogramma di esecuzione           *
      *              *-------------------------------------------------*
           move      spaces               to   w-key-tip-fun          .
           move      "pgm/scf/prg/obj/pscf300a"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using i-ide
                                               w-prs
                                               w-ref
                                               w-top
                                               w-def
                                               w-key                  .
      *              *-------------------------------------------------*
      *              * Cancellazione sottoprogramma di esecuzione      *
      *              *-------------------------------------------------*
           cancel    s-pat                                            .
       ric-001-111-999.
           exit.

      *    *===========================================================*
      *    * Richiamo sottoprogramma per tipo operazione  1127         *
      *    *-----------------------------------------------------------*
       ric-001-127-000.
      *              *-------------------------------------------------*
      *              * Richiamo sottoprogramma di esecuzione           *
      *              *-------------------------------------------------*
           move      spaces               to   w-key-tip-fun          .
           move      "pgm/scf/prg/obj/pscf300a"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using i-ide
                                               w-prs
                                               w-ref
                                               w-top
                                               w-def
                                               w-key                  .
      *              *-------------------------------------------------*
      *              * Cancellazione sottoprogramma di esecuzione      *
      *              *-------------------------------------------------*
           cancel    s-pat                                            .
       ric-001-127-999.
           exit.

      *    *===========================================================*
      *    * Richiamo sottoprogramma per tipo operazione  1161         *
      *    *-----------------------------------------------------------*
       ric-001-161-000.
      *              *-------------------------------------------------*
      *              * Richiamo sottoprogramma di esecuzione           *
      *              *-------------------------------------------------*
           move      spaces               to   w-key-tip-fun          .
           move      "pgm/scf/prg/obj/pscf300a"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using i-ide
                                               w-prs
                                               w-ref
                                               w-top
                                               w-def
                                               w-key                  .
      *              *-------------------------------------------------*
      *              * Cancellazione sottoprogramma di esecuzione      *
      *              *-------------------------------------------------*
           cancel    s-pat                                            .
       ric-001-161-999.
           exit.

      *    *===========================================================*
      *    * Richiamo sottoprogramma per tipo operazione  1200         *
      *    *-----------------------------------------------------------*
       ric-001-200-000.
      *              *-------------------------------------------------*
      *              * Richiamo sottoprogramma di esecuzione           *
      *              *-------------------------------------------------*
           move      spaces               to   w-key-tip-fun          .
           move      "pgm/scf/prg/obj/pscf300b"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using i-ide
                                               w-prs
                                               w-ref
                                               w-top
                                               w-def
                                               w-key                  .
      *              *-------------------------------------------------*
      *              * Cancellazione sottoprogramma di esecuzione      *
      *              *-------------------------------------------------*
           cancel    s-pat                                            .
       ric-001-200-999.
           exit.

      *    *===========================================================*
      *    * Richiamo sottoprogramma per tipo operazione 1510          *
      *    *-----------------------------------------------------------*
       ric-001-510-000.
      *              *-------------------------------------------------*
      *              * Richiamo sottoprogramma di esecuzione           *
      *              *-------------------------------------------------*
           move      spaces               to   w-key-tip-fun          .
           move      "pgm/scf/prg/obj/pscf300c"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using i-ide
                                               w-prs
                                               w-ref
                                               w-top
                                               w-def
                                               w-key                  .
      *              *-------------------------------------------------*
      *              * Cancellazione sottoprogramma di esecuzione      *
      *              *-------------------------------------------------*
           cancel    s-pat                                            .
       ric-001-510-999.
           exit.

      *    *===========================================================*
      *    * Richiamo sottoprogramma per tipo operazione 1520          *
      *    *-----------------------------------------------------------*
       ric-001-520-000.
      *              *-------------------------------------------------*
      *              * Richiamo sottoprogramma di esecuzione           *
      *              *-------------------------------------------------*
           move      spaces               to   w-key-tip-fun          .
           move      "pgm/scf/prg/obj/pscf300c"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using i-ide
                                               w-prs
                                               w-ref
                                               w-top
                                               w-def
                                               w-key                  .
      *              *-------------------------------------------------*
      *              * Cancellazione sottoprogramma di esecuzione      *
      *              *-------------------------------------------------*
           cancel    s-pat                                            .
       ric-001-520-999.
           exit.

      *    *===========================================================*
      *    * Richiamo sottoprogramma per tipo operazione 1530          *
      *    *-----------------------------------------------------------*
       ric-001-530-000.
      *              *-------------------------------------------------*
      *              * Richiamo sottoprogramma di esecuzione           *
      *              *-------------------------------------------------*
           move      spaces               to   w-key-tip-fun          .
           move      "pgm/scf/prg/obj/pscf300c"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using i-ide
                                               w-prs
                                               w-ref
                                               w-top
                                               w-def
                                               w-key                  .
      *              *-------------------------------------------------*
      *              * Cancellazione sottoprogramma di esecuzione      *
      *              *-------------------------------------------------*
           cancel    s-pat                                            .
       ric-001-530-999.
           exit.

      *    *===========================================================*
      *    * Richiamo sottoprogramma per tipo operazione  1610         *
      *    *-----------------------------------------------------------*
       ric-001-610-000.
      *              *-------------------------------------------------*
      *              * Richiamo sottoprogramma di esecuzione           *
      *              *-------------------------------------------------*
           move      spaces               to   w-key-tip-fun          .
           move      "pgm/scf/prg/obj/pscf300c"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using i-ide
                                               w-prs
                                               w-ref
                                               w-top
                                               w-def
                                               w-key                  .
      *              *-------------------------------------------------*
      *              * Cancellazione sottoprogramma di esecuzione      *
      *              *-------------------------------------------------*
           cancel    s-pat                                            .
       ric-001-610-999.
           exit.

      *    *===========================================================*
      *    * Richiamo sottoprogramma per tipo operazione  1620         *
      *    *-----------------------------------------------------------*
       ric-001-620-000.
      *              *-------------------------------------------------*
      *              * Richiamo sottoprogramma di esecuzione           *
      *              *-------------------------------------------------*
           move      spaces               to   w-key-tip-fun          .
           move      "pgm/scf/prg/obj/pscf300c"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using i-ide
                                               w-prs
                                               w-ref
                                               w-top
                                               w-def
                                               w-key                  .
      *              *-------------------------------------------------*
      *              * Cancellazione sottoprogramma di esecuzione      *
      *              *-------------------------------------------------*
           cancel    s-pat                                            .
       ric-001-620-999.
           exit.

      *    *===========================================================*
      *    * Richiamo sottoprogramma per tipo operazione  1630         *
      *    *-----------------------------------------------------------*
       ric-001-630-000.
      *              *-------------------------------------------------*
      *              * Richiamo sottoprogramma di esecuzione           *
      *              *-------------------------------------------------*
           move      spaces               to   w-key-tip-fun          .
           move      "pgm/scf/prg/obj/pscf300c"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using i-ide
                                               w-prs
                                               w-ref
                                               w-top
                                               w-def
                                               w-key                  .
      *              *-------------------------------------------------*
      *              * Cancellazione sottoprogramma di esecuzione      *
      *              *-------------------------------------------------*
           cancel    s-pat                                            .
       ric-001-630-999.
           exit.

      *    *===========================================================*
      *    * Richiamo sottoprogramma per tipo operazione  1640         *
      *    *-----------------------------------------------------------*
       ric-001-640-000.
      *              *-------------------------------------------------*
      *              * Richiamo sottoprogramma di esecuzione           *
      *              *-------------------------------------------------*
           move      spaces               to   w-key-tip-fun          .
           move      "pgm/scf/prg/obj/pscf300c"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using i-ide
                                               w-prs
                                               w-ref
                                               w-top
                                               w-def
                                               w-key                  .
      *              *-------------------------------------------------*
      *              * Cancellazione sottoprogramma di esecuzione      *
      *              *-------------------------------------------------*
           cancel    s-pat                                            .
       ric-001-640-999.
           exit.

      *    *===========================================================*
      *    * Richiamo sottoprogramma per tipo operazione 1650          *
      *    *-----------------------------------------------------------*
       ric-001-650-000.
      *              *-------------------------------------------------*
      *              * Richiamo sottoprogramma di esecuzione           *
      *              *-------------------------------------------------*
           move      spaces               to   w-key-tip-fun          .
           move      "pgm/scf/prg/obj/pscf300c"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using i-ide
                                               w-prs
                                               w-ref
                                               w-top
                                               w-def
                                               w-key                  .
      *              *-------------------------------------------------*
      *              * Cancellazione sottoprogramma di esecuzione      *
      *              *-------------------------------------------------*
           cancel    s-pat                                            .
       ric-001-650-999.
           exit.

      *    *===========================================================*
      *    * Richiamo sottoprogramma per tipo operazione  1730         *
      *    *-----------------------------------------------------------*
       ric-001-730-000.
      *              *-------------------------------------------------*
      *              * Richiamo sottoprogramma di esecuzione           *
      *              *-------------------------------------------------*
           move      spaces               to   w-key-tip-fun          .
           move      "pgm/scf/prg/obj/pscf300c"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using i-ide
                                               w-prs
                                               w-ref
                                               w-top
                                               w-def
                                               w-key                  .
      *              *-------------------------------------------------*
      *              * Cancellazione sottoprogramma di esecuzione      *
      *              *-------------------------------------------------*
           cancel    s-pat                                            .
       ric-001-730-999.
           exit.

      *    *===========================================================*
      *    * Richiamo sottoprogramma per tipo operazione  1731         *
      *    *-----------------------------------------------------------*
       ric-001-731-000.
      *              *-------------------------------------------------*
      *              * Richiamo sottoprogramma di esecuzione           *
      *              *-------------------------------------------------*
           move      spaces               to   w-key-tip-fun          .
           move      "pgm/scf/prg/obj/pscf300c"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using i-ide
                                               w-prs
                                               w-ref
                                               w-top
                                               w-def
                                               w-key                  .
      *              *-------------------------------------------------*
      *              * Cancellazione sottoprogramma di esecuzione      *
      *              *-------------------------------------------------*
           cancel    s-pat                                            .
       ric-001-731-999.
           exit.

      *    *===========================================================*
      *    * Richiamo sottoprogramma per tipo operazione  1740         *
      *    *-----------------------------------------------------------*
       ric-001-740-000.
      *              *-------------------------------------------------*
      *              * Richiamo sottoprogramma di esecuzione           *
      *              *-------------------------------------------------*
           move      spaces               to   w-key-tip-fun          .
           move      "pgm/scf/prg/obj/pscf300c"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using i-ide
                                               w-prs
                                               w-ref
                                               w-top
                                               w-def
                                               w-key                  .
      *              *-------------------------------------------------*
      *              * Cancellazione sottoprogramma di esecuzione      *
      *              *-------------------------------------------------*
           cancel    s-pat                                            .
       ric-001-740-999.
           exit.

      *    *===========================================================*
      *    * Richiamo sottoprogramma per tipo operazione  1750         *
      *    *-----------------------------------------------------------*
       ric-001-750-000.
      *              *-------------------------------------------------*
      *              * Richiamo sottoprogramma di esecuzione           *
      *              *-------------------------------------------------*
           move      spaces               to   w-key-tip-fun          .
           move      "pgm/scf/prg/obj/pscf300c"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using i-ide
                                               w-prs
                                               w-ref
                                               w-top
                                               w-def
                                               w-key                  .
      *              *-------------------------------------------------*
      *              * Cancellazione sottoprogramma di esecuzione      *
      *              *-------------------------------------------------*
           cancel    s-pat                                            .
       ric-001-750-999.
           exit.

      *    *===========================================================*
      *    * Richiamo sottoprogramma per tipo operazione  1830         *
      *    *-----------------------------------------------------------*
       ric-001-830-000.
      *              *-------------------------------------------------*
      *              * Richiamo sottoprogramma di esecuzione           *
      *              *-------------------------------------------------*
           move      spaces               to   w-key-tip-fun          .
           move      "pgm/scf/prg/obj/pscf300d"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using i-ide
                                               w-prs
                                               w-ref
                                               w-top
                                               w-def
                                               w-key                  .
      *              *-------------------------------------------------*
      *              * Cancellazione sottoprogramma di esecuzione      *
      *              *-------------------------------------------------*
           cancel    s-pat                                            .
       ric-001-830-999.
           exit.

      *    *===========================================================*
      *    * Richiamo sottoprogramma per tipo operazione  1831         *
      *    *-----------------------------------------------------------*
       ric-001-831-000.
      *              *-------------------------------------------------*
      *              * Richiamo sottoprogramma di esecuzione           *
      *              *-------------------------------------------------*
           move      spaces               to   w-key-tip-fun          .
           move      "pgm/scf/prg/obj/pscf300d"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using i-ide
                                               w-prs
                                               w-ref
                                               w-top
                                               w-def
                                               w-key                  .
      *              *-------------------------------------------------*
      *              * Cancellazione sottoprogramma di esecuzione      *
      *              *-------------------------------------------------*
           cancel    s-pat                                            .
       ric-001-831-999.
           exit.

      *    *===========================================================*
      *    * Richiamo sottoprogramma per tipo operazione  1840         *
      *    *-----------------------------------------------------------*
       ric-001-840-000.
      *              *-------------------------------------------------*
      *              * Richiamo sottoprogramma di esecuzione           *
      *              *-------------------------------------------------*
           move      spaces               to   w-key-tip-fun          .
           move      "pgm/scf/prg/obj/pscf300d"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using i-ide
                                               w-prs
                                               w-ref
                                               w-top
                                               w-def
                                               w-key                  .
      *              *-------------------------------------------------*
      *              * Cancellazione sottoprogramma di esecuzione      *
      *              *-------------------------------------------------*
           cancel    s-pat                                            .
       ric-001-840-999.
           exit.

      *    *===========================================================*
      *    * Richiamo sottoprogramma per tipo operazione  1850         *
      *    *-----------------------------------------------------------*
       ric-001-850-000.
      *              *-------------------------------------------------*
      *              * Richiamo sottoprogramma di esecuzione           *
      *              *-------------------------------------------------*
           move      spaces               to   w-key-tip-fun          .
           move      "pgm/scf/prg/obj/pscf300d"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using i-ide
                                               w-prs
                                               w-ref
                                               w-top
                                               w-def
                                               w-key                  .
      *              *-------------------------------------------------*
      *              * Cancellazione sottoprogramma di esecuzione      *
      *              *-------------------------------------------------*
           cancel    s-pat                                            .
       ric-001-850-999.
           exit.

      *    *===========================================================*
      *    * Richiamo sottoprogramma per tipo operazione  4730         *
      *    *-----------------------------------------------------------*
       ric-004-730-000.
      *              *-------------------------------------------------*
      *              * Richiamo sottoprogramma di esecuzione           *
      *              *-------------------------------------------------*
           move      spaces               to   w-key-tip-fun          .
           move      "pgm/scf/prg/obj/pscf300m"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using i-ide
                                               w-prs
                                               w-ref
                                               w-top
                                               w-def
                                               w-key                  .
      *              *-------------------------------------------------*
      *              * Cancellazione sottoprogramma di esecuzione      *
      *              *-------------------------------------------------*
           cancel    s-pat                                            .
       ric-004-730-999.
           exit.

      *    *===========================================================*
      *    * Richiamo sottoprogramma per tipo operazione  4731         *
      *    *-----------------------------------------------------------*
       ric-004-731-000.
      *              *-------------------------------------------------*
      *              * Richiamo sottoprogramma di esecuzione           *
      *              *-------------------------------------------------*
           move      spaces               to   w-key-tip-fun          .
           move      "pgm/scf/prg/obj/pscf300n"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using i-ide
                                               w-prs
                                               w-ref
                                               w-top
                                               w-def
                                               w-key                  .
      *              *-------------------------------------------------*
      *              * Cancellazione sottoprogramma di esecuzione      *
      *              *-------------------------------------------------*
           cancel    s-pat                                            .
       ric-004-731-999.
           exit.

      *    *===========================================================*
      *    * Richiamo sottoprogramma per tipo operazione  4735         *
      *    *-----------------------------------------------------------*
       ric-004-735-000.
      *              *-------------------------------------------------*
      *              * Richiamo sottoprogramma di esecuzione           *
      *              *-------------------------------------------------*
           move      spaces               to   w-key-tip-fun          .
           move      "pgm/scf/prg/obj/pscf300p"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using i-ide
                                               w-prs
                                               w-ref
                                               w-top
                                               w-def
                                               w-key                  .
      *              *-------------------------------------------------*
      *              * Cancellazione sottoprogramma di esecuzione      *
      *              *-------------------------------------------------*
           cancel    s-pat                                            .
       ric-004-735-999.
           exit.

      *    *===========================================================*
      *    * Richiamo sottoprogramma per tipo operazione  4736         *
      *    *-----------------------------------------------------------*
       ric-004-736-000.
      *              *-------------------------------------------------*
      *              * Richiamo sottoprogramma di esecuzione           *
      *              *-------------------------------------------------*
           move      spaces               to   w-key-tip-fun          .
           move      "pgm/scf/prg/obj/pscf300p"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using i-ide
                                               w-prs
                                               w-ref
                                               w-top
                                               w-def
                                               w-key                  .
      *              *-------------------------------------------------*
      *              * Cancellazione sottoprogramma di esecuzione      *
      *              *-------------------------------------------------*
           cancel    s-pat                                            .
       ric-004-736-999.
           exit.

      *    *===========================================================*
      *    * Richiamo sottoprogramma per tipo operazione  4750         *
      *    *-----------------------------------------------------------*
       ric-004-750-000.
      *              *-------------------------------------------------*
      *              * Richiamo sottoprogramma di esecuzione           *
      *              *-------------------------------------------------*
           move      spaces               to   w-key-tip-fun          .
           move      "pgm/scf/prg/obj/pscf300o"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using i-ide
                                               w-prs
                                               w-ref
                                               w-top
                                               w-def
                                               w-key                  .
      *              *-------------------------------------------------*
      *              * Cancellazione sottoprogramma di esecuzione      *
      *              *-------------------------------------------------*
           cancel    s-pat                                            .
       ric-004-750-999.
           exit.

      *    *===========================================================*
      *    * Caricamento iniziale delle personalizzazioni              *
      *    *-----------------------------------------------------------*
       loa-prs-scf-000.
      *              *-------------------------------------------------*
      *              * Numero livelli del piano dei conti              *
      *              *-------------------------------------------------*
           perform   prs-liv-pdc-000      thru prs-liv-pdc-999        .
      *              *-------------------------------------------------*
      *              * Multinumerazione giornale iva acquisti          *
      *              *-------------------------------------------------*
           perform   prs-mul-gia-000      thru prs-mul-gia-999        .
      *              *-------------------------------------------------*
      *              * Si/No partite zoppe in primanota                *
      *              *-------------------------------------------------*
           perform   prs-snx-pzo-000      thru prs-snx-pzo-999        .
      *              *-------------------------------------------------*
      *              * Protocollo iva fornitori automatico             *
      *              *-------------------------------------------------*
           perform   prs-pif-aut-000      thru prs-pif-aut-999        .
      *              *-------------------------------------------------*
      *              * Data registrazione contabile futura             *
      *              *-------------------------------------------------*
           perform   prs-dat-dfu-000      thru prs-dat-dfu-999        .
      *              *-------------------------------------------------*
      *              * Si/no aggiornamenti contabili per movimenti di  *
      *              * scadenze fornitori                              *
      *              *-------------------------------------------------*
           perform   snx-agg-cge-000      thru snx-agg-cge-999        .
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione : Si/No ragione so-   *
      *              * ciale fornitore in commento in riga             *
      *              *-------------------------------------------------*
           perform   prs-rsf-icr-000      thru prs-rsf-icr-999        .
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione per il tipo di file   *
      *              * da preparare per il supporto magnetico          *
      *              *-------------------------------------------------*
           perform   prs-tip-fsm-000      thru prs-tip-fsm-999        .
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione per si/no gestione    *
      *              * archivio elettronico documenti fornitori        *
      *              *-------------------------------------------------*
           perform   prs-snx-pdf-000      thru prs-snx-pdf-999        .
       loa-prs-scf-999.
           exit.

      *    *===========================================================*
      *    * Lettura personalizzazione : Numero di livelli del piano   *
      *    *                             dei conti                     *
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
      *    * Lettura personalizzazione : Multinumerazione del giornale *
      *    *                             iva acquisti                  *
      *    *-----------------------------------------------------------*
       prs-mul-gia-000.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione                       *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/cge/iva[mul-gia]"
                                          to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-alf          to   w-prs-mul-gia
           else      move  spaces         to   w-prs-mul-gia          .
      *              *-------------------------------------------------*
      *              * Normalizzazione personalizzazione               *
      *              *-------------------------------------------------*
           if        w-prs-mul-gia-snx    not  = "S"
                     move  "N"            to   w-prs-mul-gia-snx      .
           if        w-prs-mul-gia-snx    =    "N"
                     move  00             to   w-prs-mul-gia-max      .
           if        w-prs-mul-gia-max    not  numeric or
                     w-prs-mul-gia-max    >    29
                     move  29             to   w-prs-mul-gia-max      .
           if        w-prs-mul-gia-snd    not  = "S"
                     move  "N"            to   w-prs-mul-gia-snd      .
       prs-mul-gia-999.
           exit.

      *    *===========================================================*
      *    * Lettura personalizzazione : Si/No ammissibilita' delle    *
      *    *                             partite zoppe in primanota    *
      *    *-----------------------------------------------------------*
       prs-snx-pzo-000.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione                       *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/cge/mov/cge300[snx-pzo]"
                                          to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-alf          to   w-prs-snx-pzo
           else      move  spaces         to   w-prs-snx-pzo          .
      *              *-------------------------------------------------*
      *              * Normalizzazione personalizzazione               *
      *              *-------------------------------------------------*
           if        w-prs-snx-pzo        not  = "N"
                     move  "S"            to   w-prs-snx-pzo          .
       prs-snx-pzo-999.
           exit.

      *    *===========================================================*
      *    * Lettura personalizzazione : Numero protocollo iva forni-  *
      *    *                             tori automatico               *
      *    *-----------------------------------------------------------*
       prs-pif-aut-000.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione                       *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/cge/mov/cge300[pif-aut]"
                                          to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-alf          to   w-prs-pif-aut
           else      move  spaces         to   w-prs-pif-aut          .
      *              *-------------------------------------------------*
      *              * Normalizzazione personalizzazione               *
      *              *-------------------------------------------------*
           if        w-prs-pif-aut        not  = "S"
                     move  spaces         to   w-prs-pif-aut          .
       prs-pif-aut-999.
           exit.

      *    *===========================================================*
      *    * Lettura personalizzazione : Data documento futura         *
      *    *-----------------------------------------------------------*
       prs-dat-dfu-000.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione                       *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/cge/mov/cge300[dat-dfu]"
                                          to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-num          to   w-prs-dat-dfu
           else      move  zero           to   w-prs-dat-dfu          .
      *              *-------------------------------------------------*
      *              * Normalizzazione personalizzazione               *
      *              *-------------------------------------------------*
           if        w-prs-dat-dfu        =    01
                     go to prs-dat-dfu-999.
           move      00                   to   w-prs-dat-dfu          .
       prs-dat-dfu-999.
           exit.

      *    *===========================================================*
      *    * Lettura personalizzazione : Aggiornamenti contabili per   *
      *    *                             movimenti scadenze fornitori  *
      *    *-----------------------------------------------------------*
       snx-agg-cge-000.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione                       *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/scf/mov/scf300[snx-cge]"
                                          to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-alf          to   w-prs-scf-snx-cge
           else      move  spaces         to   w-prs-scf-snx-cge      .
      *              *-------------------------------------------------*
      *              * Normalizzazione personalizzazione               *
      *              *-------------------------------------------------*
           if        w-prs-scf-snx-cge    =    "S" or
                     w-prs-scf-snx-cge    =    "N"
                     go to snx-agg-cge-999.
           move      "S"                  to   w-prs-scf-snx-cge      .
       snx-agg-cge-999.
           exit.

      *    *===========================================================*
      *    * Lettura personalizzazione : Si/No ragione sociale forni-  *
      *    *                             tore in commento in riga      *
      *    *-----------------------------------------------------------*
       prs-rsf-icr-000.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione                       *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/scf/mov/scf300[rsf-icr]"
                                          to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-alf          to   w-prs-scf-rsf-icr
           else      move  spaces         to   w-prs-scf-rsf-icr      .
      *              *-------------------------------------------------*
      *              * Normalizzazione personalizzazione               *
      *              *-------------------------------------------------*
           if        w-prs-scf-rsf-icr    =    "S" or
                     w-prs-scf-rsf-icr    =    "N"
                     go to prs-rsf-icr-999.
           move      "N"                  to   w-prs-scf-rsf-icr      .
       prs-rsf-icr-999.
           exit.

      *    *===========================================================*
      *    * Lettura personalizzazione per il tipo di file da prepara- *
      *    * re per il supporto magnetico                              *
      *    *-----------------------------------------------------------*
       prs-tip-fsm-000.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione                       *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/scf/mov/scf300m[tip-fsm]"
                                          to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-alf          to   w-prs-tip-fsm
           else      move  spaces         to   w-prs-tip-fsm          .
      *              *-------------------------------------------------*
      *              * Normalizzazione personalizzazione               *
      *              *-------------------------------------------------*
           if        w-prs-tip-fsm        not   = "U" and
                     w-prs-tip-fsm        not   = "T"
                     move  "M"            to   w-prs-tip-fsm          .
       prs-tip-fsm-999.
           exit.

      *    *===========================================================*
      *    * Lettura personalizzazione per si/no gestione archivio     *
      *    * elettronico documenti fornitori                           *
      *    *                                                           *
      *    * N.B.: in obsolescenza                                     *
      *    *-----------------------------------------------------------*
       prs-snx-pdf-000.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione                       *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/scf[snx-pdf]"
                                          to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-alf          to   w-prs-snx-pdf
           else      move  spaces         to   w-prs-snx-pdf          .
      *              *-------------------------------------------------*
      *              * Normalizzazione personalizzazione               *
      *              *-------------------------------------------------*
           if        w-prs-snx-pdf        not   = "S"
                     move  "N"            to   w-prs-snx-pdf          .
       prs-snx-pdf-999.
           exit.

      *    *===========================================================*
      *    * Caricamento iniziale delle referenze                      *
      *    *-----------------------------------------------------------*
       loa-ref-scf-000.
      *              *-------------------------------------------------*
      *              * Referenze per sottoconto iva acquisti           *
      *              *-------------------------------------------------*
           perform   ref-iva-acq-000      thru ref-iva-acq-999        .
      *              *-------------------------------------------------*
      *              * Referenze per sottoconto iva a debito su ac-    *
      *              * quisti intracomunitari                          *
      *              *-------------------------------------------------*
           perform   ref-iva-dai-000      thru ref-iva-dai-999        .
      *              *-------------------------------------------------*
      *              * Referenze per sottoconto iva a credito su ac-   *
      *              * quisti intracomunitari                          *
      *              *-------------------------------------------------*
           perform   ref-iva-cai-000      thru ref-iva-cai-999        .
      *              *-------------------------------------------------*
      *              * Referenze per sottoconto iva a debito su ac-    *
      *              * quisti in Reverse Charge                        *
      *              *-------------------------------------------------*
           perform   ref-iva-dar-000      thru ref-iva-dar-999        .
      *              *-------------------------------------------------*
      *              * Referenze per sottoconto iva a credito su ac-   *
      *              * quisti in Reverse Charge                        *
      *              *-------------------------------------------------*
           perform   ref-iva-car-000      thru ref-iva-car-999        .
      *              *-------------------------------------------------*
      *              * Referenze per sottoconto acquisti               *
      *              *-------------------------------------------------*
           perform   ref-stc-acq-000      thru ref-stc-acq-999        .
      *              *-------------------------------------------------*
      *              * Referenze per sottoconto resi su acquisti       *
      *              *-------------------------------------------------*
           perform   ref-res-acq-000      thru ref-res-acq-999        .
       loa-ref-scf-999.
           exit.

      *    *===========================================================*
      *    * Lettura referenza : Sottoconto iva acquisti               *
      *    *-----------------------------------------------------------*
       ref-iva-acq-000.
      *              *-------------------------------------------------*
      *              * Lettura referenza                               *
      *              *-------------------------------------------------*
           move      "R:"                 to   s-ope                  .
           move      "pgm/cge/mov/cge300[iva-acq]"
                                          to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-num          to   w-ref-iva-acq
           else      move  zero           to   w-ref-iva-acq          .
      *              *-------------------------------------------------*
      *              * Lettura descrizione                             *
      *              *-------------------------------------------------*
           move      w-ref-iva-acq        to   w-let-arc-pdc-cod      .
           perform   let-arc-pdc-000      thru let-arc-pdc-999        .
           move      w-let-arc-pdc-des    to   w-ref-iva-acq-des      .
       ref-iva-acq-999.
           exit.

      *    *===========================================================*
      *    * Lettura referenza : Sottoconto iva a debito su acquisti   *
      *    *                     intracomunitari                       *
      *    *-----------------------------------------------------------*
       ref-iva-dai-000.
      *              *-------------------------------------------------*
      *              * Lettura referenza                               *
      *              *-------------------------------------------------*
           move      "R:"                 to   s-ope                  .
           move      "pgm/cge/mov/cge300[iva-dai]"
                                          to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-num          to   w-ref-iva-dai
                     go to ref-iva-dai-100.
      *              *-------------------------------------------------*
      *              * Se referenza non esistente : si prova con quel- *
      *              * la dell'iva su vendite                          *
      *              *-------------------------------------------------*
           move      "R:"                 to   s-ope                  .
           move      "pgm/cge/mov/cge300[iva-ven]"
                                          to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-num          to   w-ref-iva-dai
                     go to ref-iva-dai-100.
      *              *-------------------------------------------------*
      *              * Altrimenti si normalizza il valore              *
      *              *-------------------------------------------------*
           move      zero                 to   w-ref-iva-dai          .
       ref-iva-dai-100.
      *              *-------------------------------------------------*
      *              * Lettura descrizione                             *
      *              *-------------------------------------------------*
           move      w-ref-iva-dai        to   w-let-arc-pdc-cod      .
           perform   let-arc-pdc-000      thru let-arc-pdc-999        .
           move      w-let-arc-pdc-des    to   w-ref-iva-dai-des      .
       ref-iva-dai-999.
           exit.

      *    *===========================================================*
      *    * Lettura referenza : Sottoconto iva a credito su acquisti  *
      *    *                     intracomunitari                       *
      *    *-----------------------------------------------------------*
       ref-iva-cai-000.
      *              *-------------------------------------------------*
      *              * Lettura referenza                               *
      *              *-------------------------------------------------*
           move      "R:"                 to   s-ope                  .
           move      "pgm/cge/mov/cge300[iva-cai]"
                                          to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-num          to   w-ref-iva-cai
                     go to ref-iva-cai-100.
      *              *-------------------------------------------------*
      *              * Se referenza non esistente : si prova con quel- *
      *              * la dell'iva su acquisti                         *
      *              *-------------------------------------------------*
           move      "R:"                 to   s-ope                  .
           move      "pgm/cge/mov/cge300[iva-acq]"
                                          to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-num          to   w-ref-iva-cai
                     go to ref-iva-cai-100.
      *              *-------------------------------------------------*
      *              * Altrimenti si normalizza il valore              *
      *              *-------------------------------------------------*
           move      zero                 to   w-ref-iva-cai          .
       ref-iva-cai-100.
      *              *-------------------------------------------------*
      *              * Lettura descrizione                             *
      *              *-------------------------------------------------*
           move      w-ref-iva-cai        to   w-let-arc-pdc-cod      .
           perform   let-arc-pdc-000      thru let-arc-pdc-999        .
           move      w-let-arc-pdc-des    to   w-ref-iva-cai-des      .
       ref-iva-cai-999.
           exit.

      *    *===========================================================*
      *    * Lettura referenza : Sottoconto iva a debito su acquisti   *
      *    *                     in Reverse Charge                     *
      *    *-----------------------------------------------------------*
       ref-iva-dar-000.
      *              *-------------------------------------------------*
      *              * Lettura referenza                               *
      *              *-------------------------------------------------*
           move      "R:"                 to   s-ope                  .
           move      "pgm/cge/mov/cge300[iva-dar]"
                                          to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-num          to   w-ref-iva-dar
                     go to ref-iva-dar-100.
      *              *-------------------------------------------------*
      *              * Se referenza non esistente : si prova con quel- *
      *              * la dell'iva su vendite                          *
      *              *-------------------------------------------------*
           move      "R:"                 to   s-ope                  .
           move      "pgm/cge/mov/cge300[iva-ven]"
                                          to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-num          to   w-ref-iva-dar
                     go to ref-iva-dar-100.
      *              *-------------------------------------------------*
      *              * Altrimenti si normalizza il valore              *
      *              *-------------------------------------------------*
           move      zero                 to   w-ref-iva-dar          .
       ref-iva-dar-100.
      *              *-------------------------------------------------*
      *              * Lettura descrizione                             *
      *              *-------------------------------------------------*
           move      w-ref-iva-dar        to   w-let-arc-pdc-cod      .
           perform   let-arc-pdc-000      thru let-arc-pdc-999        .
           move      w-let-arc-pdc-des    to   w-ref-iva-dar-des      .
       ref-iva-dar-999.
           exit.

      *    *===========================================================*
      *    * Lettura referenza : Sottoconto iva a credito su acquisti  *
      *    *                     in Reverse Charge                     *
      *    *-----------------------------------------------------------*
       ref-iva-car-000.
      *              *-------------------------------------------------*
      *              * Lettura referenza                               *
      *              *-------------------------------------------------*
           move      "R:"                 to   s-ope                  .
           move      "pgm/cge/mov/cge300[iva-car]"
                                          to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-num          to   w-ref-iva-car
                     go to ref-iva-car-100.
      *              *-------------------------------------------------*
      *              * Se referenza non esistente : si prova con quel- *
      *              * la dell'iva su acquisti                         *
      *              *-------------------------------------------------*
           move      "R:"                 to   s-ope                  .
           move      "pgm/cge/mov/cge300[iva-acq]"
                                          to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-num          to   w-ref-iva-car
                     go to ref-iva-car-100.
      *              *-------------------------------------------------*
      *              * Altrimenti si normalizza il valore              *
      *              *-------------------------------------------------*
           move      zero                 to   w-ref-iva-car          .
       ref-iva-car-100.
      *              *-------------------------------------------------*
      *              * Lettura descrizione                             *
      *              *-------------------------------------------------*
           move      w-ref-iva-car        to   w-let-arc-pdc-cod      .
           perform   let-arc-pdc-000      thru let-arc-pdc-999        .
           move      w-let-arc-pdc-des    to   w-ref-iva-car-des      .
       ref-iva-car-999.
           exit.

      *    *===========================================================*
      *    * Lettura referenza : Sottoconto acquisti                   *
      *    *-----------------------------------------------------------*
       ref-stc-acq-000.
      *              *-------------------------------------------------*
      *              * Lettura referenza                               *
      *              *-------------------------------------------------*
           move      "R:"                 to   s-ope                  .
           move      "pgm/cge/mov/cge300[stc-acq]"
                                          to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-num          to   w-ref-stc-acq
           else      move  zero           to   w-ref-stc-acq          .
      *              *-------------------------------------------------*
      *              * Lettura descrizione                             *
      *              *-------------------------------------------------*
           move      w-ref-stc-acq        to   w-let-arc-pdc-cod      .
           perform   let-arc-pdc-000      thru let-arc-pdc-999        .
           move      w-let-arc-pdc-des    to   w-ref-stc-acq-des      .
       ref-stc-acq-999.
           exit.

      *    *===========================================================*
      *    * Lettura referenza : Sottoconto resi su acquisti           *
      *    *-----------------------------------------------------------*
       ref-res-acq-000.
      *              *-------------------------------------------------*
      *              * Lettura referenza                               *
      *              *-------------------------------------------------*
           move      "R:"                 to   s-ope                  .
           move      "pgm/cge/mov/cge300[res-acq]"
                                          to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-num          to   w-ref-res-acq
                     go to ref-res-acq-100.
      *              *-------------------------------------------------*
      *              * Se referenza non esistente : si prova con quel- *
      *              * la del sottoconto acquisti                      *
      *              *-------------------------------------------------*
           move      "R:"                 to   s-ope                  .
           move      "pgm/cge/mov/cge300[stc-acq]"
                                          to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-num          to   w-ref-res-acq
                     go to ref-res-acq-100.
      *              *-------------------------------------------------*
      *              * Altrimenti si normalizza il valore              *
      *              *-------------------------------------------------*
           move      zero                 to   w-ref-res-acq          .
       ref-res-acq-100.
      *              *-------------------------------------------------*
      *              * Lettura descrizione                             *
      *              *-------------------------------------------------*
           move      w-ref-res-acq        to   w-let-arc-pdc-cod      .
           perform   let-arc-pdc-000      thru let-arc-pdc-999        .
           move      w-let-arc-pdc-des    to   w-ref-res-acq-des      .
       ref-res-acq-999.
           exit.

      *    *===========================================================*
      *    * Caricamento iniziale dei valori di defaults generali      *
      *    *-----------------------------------------------------------*
       loa-def-gen-000.
      *              *-------------------------------------------------*
      *              * Data di registrazione                           *
      *              *-------------------------------------------------*
           move      "DT"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-dat                to   w-def-dat-reg          .
      *              *-------------------------------------------------*
      *              * Codice tipo operazione di default               *
      *              *-------------------------------------------------*
           move      zero                 to   w-def-tip-ope          .
      *              *-------------------------------------------------*
      *              * Data di registrazione per contabilita'          *
      *              *-------------------------------------------------*
           move      zero                 to   w-def-drg-cge          .
      *              *-------------------------------------------------*
      *              * Numero giornale Iva per contabilita'            *
      *              *-------------------------------------------------*
           move      zero                 to   w-def-num-giv          .
       loa-def-gen-999.
           exit.

      *    *===========================================================*
      *    * Caricamento iniziale della tabella dei tipi operazione    *
      *    *-----------------------------------------------------------*
       loa-tbl-top-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione iniziale della tabella          *
      *              *-------------------------------------------------*
       loa-tbl-top-010.
      *                  *---------------------------------------------*
      *                  * Massimo numero di elementi in tabella       *
      *                  * ti in tabella                               *
      *                  *---------------------------------------------*
           move      50                   to   w-top-ele-max          .
       loa-tbl-top-015.
      *                  *---------------------------------------------*
      *                  * Normalizzazione numero effettivo di elemen- *
      *                  * ti in tabella                               *
      *                  *---------------------------------------------*
           move      zero                 to   w-top-ele-num          .
       loa-tbl-top-020.
      *                  *---------------------------------------------*
      *                  * Normalizzazione degli elementi in tabella   *
      *                  *---------------------------------------------*
           move      zero                 to   w-top-ele-inx          .
       loa-tbl-top-022.
           add       1                    to   w-top-ele-inx          .
           if        w-top-ele-inx        >    w-top-ele-max
                     go to loa-tbl-top-030.
           move      zero                 to   w-top-cod-top
                                              (w-top-ele-inx)         .
           move      spaces               to   w-top-cod-mne
                                              (w-top-ele-inx)         .
           move      spaces               to   w-top-des-top
                                              (w-top-ele-inx)         .
           move      zero                 to   w-top-cau-cge
                                              (w-top-ele-inx)         .
           move      zero                 to   w-top-stc-cge
                                              (w-top-ele-inx)         .
           move      zero                 to   w-top-ctp-cge
                                              (w-top-ele-inx)         .
           move      zero                 to   w-top-f01-top
                                              (w-top-ele-inx)         .
           move      zero                 to   w-top-f02-top
                                              (w-top-ele-inx)         .
           move      zero                 to   w-top-f03-top
                                              (w-top-ele-inx)         .
           move      zero                 to   w-top-f04-top
                                              (w-top-ele-inx)         .
           move      zero                 to   w-top-f05-top
                                              (w-top-ele-inx)         .
           go to     loa-tbl-top-022.
       loa-tbl-top-030.
      *                  *---------------------------------------------*
      *                  * Fine normalizzazione tabella                *
      *                  *---------------------------------------------*
           go to     loa-tbl-top-100.
       loa-tbl-top-100.
      *              *-------------------------------------------------*
      *              * Caricamento della tabella                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Apertura [yop]                              *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/scf/fls/ioc/obj/iofyop"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-yop                 .
      *                  *---------------------------------------------*
      *                  * Start su [yop]                              *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "CODTOP    "         to   f-key                  .
           move      zero                 to   rf-yop-cod-top         .
           move      "pgm/scf/fls/ioc/obj/iofyop"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-yop                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata : fine lettura              *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to loa-tbl-top-180.
       loa-tbl-top-120.
      *                  *---------------------------------------------*
      *                  * Lettura sequenziale [yop]                   *
      *                  *---------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/scf/fls/ioc/obj/iofyop"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-yop                 .
      *                  *---------------------------------------------*
      *                  * Se fine file : fine lettura                 *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to loa-tbl-top-180.
       loa-tbl-top-140.
      *                  *---------------------------------------------*
      *                  * Bufferizzazione elemento in tabella         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Incremento numero elementi memorizzati  *
      *                      *-----------------------------------------*
           add       1                    to   w-top-ele-num          .
      *                      *-----------------------------------------*
      *                      * Bufferizzazione dati associati al tipo  *
      *                      * operazione                              *
      *                      *-----------------------------------------*
           move      rf-yop-cod-top       to   w-top-cod-top
                                              (w-top-ele-num)         .
           move      rf-yop-cod-mne       to   w-top-cod-mne
                                              (w-top-ele-num)         .
           move      rf-yop-des-top       to   w-top-des-top
                                              (w-top-ele-num)         .
           move      rf-yop-cau-cge       to   w-top-cau-cge
                                              (w-top-ele-num)         .
           move      rf-yop-stc-cge       to   w-top-stc-cge
                                              (w-top-ele-num)         .
           move      rf-yop-ctp-cge       to   w-top-ctp-cge
                                              (w-top-ele-num)         .
           move      rf-yop-f01-top       to   w-top-f01-top
                                              (w-top-ele-num)         .
           move      rf-yop-f02-top       to   w-top-f02-top
                                              (w-top-ele-num)         .
           move      rf-yop-f03-top       to   w-top-f03-top
                                              (w-top-ele-num)         .
           move      rf-yop-f04-top       to   w-top-f04-top
                                              (w-top-ele-num)         .
           move      rf-yop-f05-top       to   w-top-f05-top
                                              (w-top-ele-num)         .
       loa-tbl-top-160.
      *                  *---------------------------------------------*
      *                  * Riciclo a lettura elemento successivo       *
      *                  *---------------------------------------------*
           go to     loa-tbl-top-120.
       loa-tbl-top-180.
      *                  *---------------------------------------------*
      *                  * Chiusura [yop]                              *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/scf/fls/ioc/obj/iofyop"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-yop                 .
       loa-tbl-top-999.
           exit.

      *    *===========================================================*
      *    * Open moduli di accettazione                               *
      *    *-----------------------------------------------------------*
       opn-mod-acc-000.
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice banca estera    *
      *              * fornitore                                       *
      *              *-------------------------------------------------*
           perform   cod-mne-bef-opn-000  thru cod-mne-bef-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione tipo operazione per    *
      *              * scadenze fornitori                              *
      *              *-------------------------------------------------*
           perform   cod-mne-yop-opn-000  thru cod-mne-yop-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice fornitore       *
      *              *-------------------------------------------------*
           perform   cod-mne-fnt-opn-000  thru cod-mne-fnt-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice cliente         *
      *              *-------------------------------------------------*
           perform   cod-mne-cli-opn-000  thru cod-mne-cli-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice sottoconto      *
      *              *-------------------------------------------------*
           perform   cod-mne-pdc-opn-000  thru cod-mne-pdc-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione fornitore commerciale  *
      *              *-------------------------------------------------*
           perform   cod-mne-dcf-opn-000  thru cod-mne-dcf-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione dipendenza fornitore   *
      *              *-------------------------------------------------*
           perform   cod-cod-dcf-opn-000  thru cod-cod-dcf-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione forma di pagamento     *
      *              *-------------------------------------------------*
           perform   cod-mne-yfp-opn-000  thru cod-mne-yfp-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice valuta          *
      *              *-------------------------------------------------*
           perform   cod-cod-zvl-opn-000  thru cod-cod-zvl-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione coefficiente di cambio *
      *              * valuta                                          *
      *              *-------------------------------------------------*
           perform   coe-cmb-vlt-opn-000  thru coe-cmb-vlt-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice nostra cassa,   *
      *              * banca, o c/c postale                            *
      *              *-------------------------------------------------*
           perform   cod-des-cbp-opn-000  thru cod-des-cbp-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice ABI             *
      *              *-------------------------------------------------*
           perform   cod-mne-abi-opn-000  thru cod-mne-abi-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice CAB             *
      *              *-------------------------------------------------*
           perform   cod-mne-cab-opn-000  thru cod-mne-cab-opn-999    .
       opn-mod-acc-999.
           exit.

      *    *===========================================================*
      *    * Close moduli di accettazione                              *
      *    *-----------------------------------------------------------*
       cls-mod-acc-000.
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice banca estera   *
      *              * fornitore                                       *
      *              *-------------------------------------------------*
           perform   cod-mne-bef-cls-000  thru cod-mne-bef-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione tipo operazione per   *
      *              * scadenze fornitori                              *
      *              *-------------------------------------------------*
           perform   cod-mne-yop-cls-000  thru cod-mne-yop-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice fornitore      *
      *              *-------------------------------------------------*
           perform   cod-mne-fnt-cls-000  thru cod-mne-fnt-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice cliente        *
      *              *-------------------------------------------------*
           perform   cod-mne-cli-cls-000  thru cod-mne-cli-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice sottoconto     *
      *              *-------------------------------------------------*
           perform   cod-mne-pdc-cls-000  thru cod-mne-pdc-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione fornitore commerciale *
      *              *-------------------------------------------------*
           perform   cod-mne-dcf-cls-000  thru cod-mne-dcf-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione dipendenza fornitore  *
      *              *-------------------------------------------------*
           perform   cod-cod-dcf-cls-000  thru cod-cod-dcf-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione forma di pagamento    *
      *              *-------------------------------------------------*
           perform   cod-mne-yfp-cls-000  thru cod-mne-yfp-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice valuta         *
      *              *-------------------------------------------------*
           perform   cod-cod-zvl-cls-000  thru cod-cod-zvl-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione coefficiente di cam-  *
      *              * bio valuta                                      *
      *              *-------------------------------------------------*
           perform   coe-cmb-vlt-cls-000  thru coe-cmb-vlt-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice nostra cassa,  *
      *              * banca, o c/c postale                            *
      *              *-------------------------------------------------*
           perform   cod-des-cbp-cls-000  thru cod-des-cbp-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice ABI            *
      *              *-------------------------------------------------*
           perform   cod-mne-abi-cls-000  thru cod-mne-abi-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice CAB            *
      *              *-------------------------------------------------*
           perform   cod-mne-cab-cls-000  thru cod-mne-cab-cls-999    .
       cls-mod-acc-999.
           exit.

      *    *===========================================================*
      *    * Open moduli di aggiornamento                              *
      *    *-----------------------------------------------------------*
       opn-mod-agg-000.
      *              *-------------------------------------------------*
      *              * Open modulo aggiornamento contabilita' genera-  *
      *              * le, clienti, fornitori, iva                     *
      *              *-------------------------------------------------*
           perform   mdl-agg-cge-opn-000  thru mdl-agg-cge-opn-999    .
       opn-mod-agg-999.
           exit.

      *    *===========================================================*
      *    * Close moduli di aggiornamento                             *
      *    *-----------------------------------------------------------*
       cls-mod-agg-000.
      *              *-------------------------------------------------*
      *              * Close modulo aggiornamento contabilita' genera- *
      *              * le, clienti, fornitori, iva                     *
      *              *-------------------------------------------------*
           perform   mdl-agg-cge-cls-000  thru mdl-agg-cge-cls-999    .
       cls-mod-agg-999.
           exit.

      *    *===========================================================*
      *    * Open files utilizzati dalla fase in tutte le sue funzioni *
      *    *-----------------------------------------------------------*
       opn-fls-fas-000.
      *              *-------------------------------------------------*
      *              * Open sottoprogramma gestione catena righe       *
      *              *-------------------------------------------------*
           move      "OP"                 to   w-cat-rig-ope          .
           perform   cll-sub-cat-000      thru cll-sub-cat-999        .
      *              *-------------------------------------------------*
      *              * Open sottoprogramma gestione catena righe 2     *
      *              *-------------------------------------------------*
           move      "OP"                 to   w-cat-rg2-ope          .
           perform   cll-sub-ct2-000      thru cll-sub-ct2-999        .
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
      *              * [sff]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/scf/fls/ioc/obj/iofsff"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-sff                 .
      *              *-------------------------------------------------*
      *              * [sfs]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/scf/fls/ioc/obj/iofsfs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-sfs                 .
      *              *-------------------------------------------------*
      *              * [sfx]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/scf/fls/ioc/obj/iofsfx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-sfx                 .
      *              *-------------------------------------------------*
      *              * [sfp]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/scf/fls/ioc/obj/iofsfp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-sfp                 .
      *              *-------------------------------------------------*
      *              * [sfa]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/scf/fls/ioc/obj/iofsfa"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-sfa                 .
      *              *-------------------------------------------------*
      *              * [bef]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/scf/fls/ioc/obj/iofbef"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bef                 .
      *              *-------------------------------------------------*
      *              * [yop]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/scf/fls/ioc/obj/iofyop"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-yop                 .
      *              *-------------------------------------------------*
      *              * [xft]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/scf/fls/ioc/obj/iofxft"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-xft                 .
      *              *-------------------------------------------------*
      *              * [xfr]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/scf/fls/ioc/obj/iofxfr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-xfr                 .
      *              *-------------------------------------------------*
      *              * [fnt]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/ioffnt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fnt                 .
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
      *              * [mgr]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmgr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgr                 .
      *              *-------------------------------------------------*
      *              * [mgi]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmgi"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgi                 .
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
      *              * [zci]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofzci"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zci                 .
      *              *-------------------------------------------------*
      *              * [dcf]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofdcf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcf                 .
      *              *-------------------------------------------------*
      *              * [yfp]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofyfp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-yfp                 .
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
      *              * [cbp]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofcbp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cbp                 .
      *              *-------------------------------------------------*
      *              * [obp]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofobp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-obp                 .
      *              *-------------------------------------------------*
      *              * [zpg]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofzpg"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zpg                 .
      *              *-------------------------------------------------*
      *              * [gep]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofgep"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gep                 .
      *              *-------------------------------------------------*
      *              * [axi]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/abi/fls/ioc/obj/iofaxi"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-axi                 .
      *              *-------------------------------------------------*
      *              * [axs]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/abi/fls/ioc/obj/iofaxs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-axs                 .
      *              *-------------------------------------------------*
      *              * [raa]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/rda/fls/ioc/obj/iofraa"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-raa                 .
      *              *-------------------------------------------------*
      *              * [ram]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/rda/fls/ioc/obj/iofram"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ram                 .
      *              *-------------------------------------------------*
      *              * [ztr]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/rda/fls/ioc/obj/iofztr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ztr                 .
      *              *-------------------------------------------------*
      *              * [rdaprt]                                        *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/rda/num/ioc/obj/inrdaprt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-rda-prt             .
      *              *-------------------------------------------------*
      *              * [sffnum]                                        *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/scf/num/ioc/obj/insffnum"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-sff-num             .
      *              *-------------------------------------------------*
      *              * [sfsnum]                                        *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/scf/num/ioc/obj/insfsnum"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-sfs-num             .
      *              *-------------------------------------------------*
      *              * [sfpnum]                                        *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/scf/num/ioc/obj/insfpnum"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-sfp-num             .
      *              *-------------------------------------------------*
      *              * [sfanum]                                        *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/scf/num/ioc/obj/insfanum"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-sfa-num             .
      *              *-------------------------------------------------*
      *              * [sfonum]                                        *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/scf/num/ioc/obj/insfonum"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-sfo-num             .
      *              *-------------------------------------------------*
      *              * [prtivf]                                        *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/num/ioc/obj/inprtivf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-prt-ivf             .
      *              *-------------------------------------------------*
      *              * [prtcge]                                        *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/num/ioc/obj/inprtcge"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-prt-cge             .
      *              *-------------------------------------------------*
      *              * Open modulo di determinazione imposta           *
      *              *-------------------------------------------------*
           perform   det-imp-iva-opn-000  thru det-imp-iva-opn-999    .
       opn-fls-fas-999.
           exit.

      *    *===========================================================*
      *    * Close files utilizzati dalla fase in tutte le sue fun-    *
      *    * zioni                                                     *
      *    *-----------------------------------------------------------*
       cls-fls-fas-000.
      *              *-------------------------------------------------*
      *              * Close sottoprogramma gestione catena righe con  *
      *              * cancellazione del sottoprogramma stesso         *
      *              *-------------------------------------------------*
           move      "CL"                 to   w-cat-rig-ope          .
           perform   cll-sub-cat-000      thru cll-sub-cat-999        .
           perform   cnc-sub-cat-000      thru cnc-sub-cat-999        .
      *              *-------------------------------------------------*
      *              * Close sottoprogramma gestione catena righe 2    *
      *              * con cancellazione del sottoprogramma stesso     *
      *              *-------------------------------------------------*
           move      "CL"                 to   w-cat-rg2-ope          .
           perform   cll-sub-ct2-000      thru cll-sub-ct2-999        .
           perform   cnc-sub-ct2-000      thru cnc-sub-ct2-999        .
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
      *              * [sff]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/scf/fls/ioc/obj/iofsff"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-sff                 .
      *              *-------------------------------------------------*
      *              * [sfs]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/scf/fls/ioc/obj/iofsfs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-sfs                 .
      *              *-------------------------------------------------*
      *              * [sfx]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/scf/fls/ioc/obj/iofsfx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-sfx                 .
      *              *-------------------------------------------------*
      *              * [sfp]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/scf/fls/ioc/obj/iofsfp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-sfp                 .
      *              *-------------------------------------------------*
      *              * [sfa]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/scf/fls/ioc/obj/iofsfa"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-sfa                 .
      *              *-------------------------------------------------*
      *              * [bef]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/scf/fls/ioc/obj/iofbef"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bef                 .
      *              *-------------------------------------------------*
      *              * [yop]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/scf/fls/ioc/obj/iofyop"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-yop                 .
      *              *-------------------------------------------------*
      *              * [xft]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/scf/fls/ioc/obj/iofxft"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-xft                 .
      *              *-------------------------------------------------*
      *              * [xfr]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/scf/fls/ioc/obj/iofxfr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-xfr                 .
      *              *-------------------------------------------------*
      *              * [fnt]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/ioffnt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fnt                 .
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
      *              * [mgr]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmgr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgr                 .
      *              *-------------------------------------------------*
      *              * [mgi]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmgi"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgi                 .
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
      *              * [zci]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofzci"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zci                 .
      *              *-------------------------------------------------*
      *              * [dcf]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofdcf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcf                 .
      *              *-------------------------------------------------*
      *              * [yfp]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofyfp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-yfp                 .
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
      *              * [cbp]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofcbp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cbp                 .
      *              *-------------------------------------------------*
      *              * [obp]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofobp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-obp                 .
      *              *-------------------------------------------------*
      *              * [zpg]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofzpg"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zpg                 .
      *              *-------------------------------------------------*
      *              * [gep]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofgep"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gep                 .
      *              *-------------------------------------------------*
      *              * [axi]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/abi/fls/ioc/obj/iofaxi"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-axi                 .
      *              *-------------------------------------------------*
      *              * [axs]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/abi/fls/ioc/obj/iofaxs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-axs                 .
      *              *-------------------------------------------------*
      *              * [raa]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/rda/fls/ioc/obj/iofraa"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-raa                 .
      *              *-------------------------------------------------*
      *              * [ram]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/rda/fls/ioc/obj/iofram"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ram                 .
      *              *-------------------------------------------------*
      *              * [ztr]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/rda/fls/ioc/obj/iofztr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ztr                 .
      *              *-------------------------------------------------*
      *              * [rdaprt]                                        *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/rda/num/ioc/obj/inrdaprt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-rda-prt             .
      *              *-------------------------------------------------*
      *              * [sffnum]                                        *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/scf/num/ioc/obj/insffnum"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-sff-num             .
      *              *-------------------------------------------------*
      *              * [sfsnum]                                        *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/scf/num/ioc/obj/insfsnum"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-sfs-num             .
      *              *-------------------------------------------------*
      *              * [sfpnum]                                        *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/scf/num/ioc/obj/insfpnum"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-sfp-num             .
      *              *-------------------------------------------------*
      *              * [sfanum]                                        *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/scf/num/ioc/obj/insfanum"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-sfa-num             .
      *              *-------------------------------------------------*
      *              * [sfonum]                                        *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/scf/num/ioc/obj/insfonum"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-sfo-num             .
      *              *-------------------------------------------------*
      *              * [prtivf]                                        *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/num/ioc/obj/inprtivf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-prt-ivf             .
      *              *-------------------------------------------------*
      *              * [prtcge]                                        *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/num/ioc/obj/inprtcge"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-prt-cge             .
      *              *-------------------------------------------------*
      *              * Close modulo di determinazione imposta          *
      *              *-------------------------------------------------*
           perform   det-imp-iva-cls-000  thru det-imp-iva-cls-999    .
       cls-fls-fas-999.
           exit.

      *    *===========================================================*
      *    * Richiamo sottoprogramma per gestione catena righe         *
      *    *-----------------------------------------------------------*
       cll-sub-cat-000.
           move      "pgm/scf/prg/obj/pscf3002"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using w-cat-rig              .
       cll-sub-cat-999.
           exit.

      *    *===========================================================*
      *    * Cancellazione sottoprogramma per gestione catena righe    *
      *    *-----------------------------------------------------------*
       cnc-sub-cat-000.
           move      "pgm/scf/prg/obj/pscf3002"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           cancel    s-pat                                            .
       cnc-sub-cat-999.
           exit.

      *    *===========================================================*
      *    * Richiamo sottoprogramma per gestione catena righe 2       *
      *    *-----------------------------------------------------------*
       cll-sub-ct2-000.
           move      "pgm/scf/prg/obj/pscf3003"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using w-cat-rg2              .
       cll-sub-ct2-999.
           exit.

      *    *===========================================================*
      *    * Cancellazione sottoprogramma per gestione catena righe 2  *
      *    *-----------------------------------------------------------*
       cnc-sub-ct2-000.
           move      "pgm/scf/prg/obj/pscf3003"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           cancel    s-pat                                            .
       cnc-sub-ct2-999.
           exit.

      *    *===========================================================*
      *    * Determinazione della data di registrazione minima per i   *
      *    * movimenti di contabilita' generale                        *
      *    *-----------------------------------------------------------*
       det-drc-min-000.
      *              *-------------------------------------------------*
      *              * Richiamo sottoprogramma per aggiornamenti con-  *
      *              * tabili per la determinazione della data di re-  *
      *              * gistrazione minima per la contabilita' genera-  *
      *              * le, clienti, fornitori, ma non iva              *
      *              *-------------------------------------------------*
           move      "DN"                 to   l-cge-300-tip-ope      .
           perform   mdl-agg-cge-cll-000  thru mdl-agg-cge-cll-999    .
      *              *-------------------------------------------------*
      *              * Memorizzazione della data di registrazione mi-  *
      *              * nima appena determinata                         *
      *              *-------------------------------------------------*
           move      l-cge-300-dat-reg    to   w-prs-scf-drc-min      .
       det-drc-min-999.
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
      *    * Subroutines per accettazione codice banca estera per il   *
      *    * fornitore                                                 *
      *    *-----------------------------------------------------------*
           copy      "pgm/scf/prg/cpy/acmnbef0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione codice operazione per sca- *
      *    * denze fornitori                                           *
      *    *-----------------------------------------------------------*
           copy      "pgm/scf/prg/cpy/acmnyop0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice fornitore       *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/acmnfnt0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice cliente         *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/acmncli0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice sottoconto      *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/acmnpdc0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice fornitore com-  *
      *    * merciale                                                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcf/prg/cpy/acmndcf0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per accettazione codice dipendenza fornitore  *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcf/prg/cpy/acoddcf0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione della forma di pagamento   *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcf/prg/cpy/acmnyfp0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice valuta          *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcc/prg/cpy/acodzvl0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del coefficiente di cambio *
      *    * valuta                                                    *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcc/prg/cpy/acoecmb0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice nostra cassa, o *
      *    * nostra banca, o nostro c/c postale                        *
      *    *-----------------------------------------------------------*
           copy      "pgm/gep/prg/cpy/acdecbp0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice ABI             *
      *    *-----------------------------------------------------------*
           copy      "pgm/abi/prg/cpy/acmnabi0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione codice C.A.B.              *
      *    *-----------------------------------------------------------*
           copy      "pgm/abi/prg/cpy/acmncab0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per modulo aggiornamento contabilita' genera- *
      *    * le, clienti, fornitori, iva                               *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/pcge300z.pgs"                   .

      *    *===========================================================*
      *    * Subroutines per determinazione imposta Iva in base ad un  *
      *    * imponibile                                                *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/dimpiva0.dts"                   .

