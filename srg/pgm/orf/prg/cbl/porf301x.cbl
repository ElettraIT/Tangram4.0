       Identification Division.
       Program-Id.                                 porf301x           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    orf                 *
      *                                Settore:                        *
      *                                   Fase:                        *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 28/02/94    *
      *                       Ultima revisione:    NdK del 25/04/25    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      * ============================================================== *
      *                                                                *
      * Descrizione pgm:   Modulo di espansione ordine a fornitore per *
      *                    visualizzare le righe                       *
      *                                                                *
      * ============================================================== *
      *                                                                *
      * Tipi operazione :                                              *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "OP"  Open modulo                                              *
      *                                                                *
      *              Input  : w-esp-rig-ofr-ope : "OP"                 *
      *                                                                *
      *                                                                *
      *              Output : nessuno                                  *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "CL"  Close modulo                                             *
      *                                                                *
      *              Input  : w-esp-rig-ofr-ope : "CL"                 *
      *                                                                *
      *                                                                *
      *              Output : nessuno                                  *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "C?"  Test di cancellabilita' del modulo                       *
      *                                                                *
      *              Input  : w-esp-rig-ofr-ope : "C?"                 *
      *                                                                *
      *                                                                *
      *              Output : w-esp-rig-ofr-ope : spaces = Si          *
      *                                           "C?"   = No          *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "ES"  Espansione                                               *
      *                                                                *
      *              Input  : w-esp-rig-ofr-ope : "ES"                 *
      *                                                                *
      *                       w-esp-rig-ofr-prt : protocollo ordine    *
      *                       w-esp-rig-ofr-num : numero documento     *
      *                       w-esp-rig-ofr-dat : data documento       *
      *                       w-esp-rig-ofr-tmo : tipo documento       *
      *                       w-esp-rig-ofr-pro : codice alfanumerico  *
      *                                           prodotto (opzionale) *
      *                                                                *
      *              Output : nessuno                                  *
      *                                                                *
      *                                                                *
      *       N.B.: L'operazione di espansione non salva la 'start'    *
      *             sulle righe ordine nel caso in cui venga effet-    *
      *             tuata una interrogazione sulle righe [ofr]         *
      *                                                                *
      *       -------------------------------------------------------- *
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
      *    * Record files                                              *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [ofr]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/orf/fls/rec/rfofr"                          .
      *        *-------------------------------------------------------*
      *        * [ofx]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/orf/fls/rec/rfofx"                          .
      *        *-------------------------------------------------------*
      *        * [dcf]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcf/fls/rec/rfdcf"                          .
      *        *-------------------------------------------------------*
      *        * [aaf]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcf/fls/rec/rfaaf"                          .
      *        *-------------------------------------------------------*
      *        * [dcp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfdcp"                          .
      *        *-------------------------------------------------------*
      *        * [pdx]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfpdx"                          .

      *    *===========================================================*
      *    * Area di comunicazione per determinazione quantita' da e-  *
      *    * vadere riga ordine fornitore                              *
      *    *-----------------------------------------------------------*
           copy      "pgm/orf/prg/cpy/dqevrof0.dtl"                   .

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
      *        * Contatore del numero di Open in corso per il modulo   *
      *        *-------------------------------------------------------*
           05  w-cnt-ctr-opn              pic  9(03)       value zero .
      *        *-------------------------------------------------------*
      *        * Deviatori per l'expand                                *
      *        *-------------------------------------------------------*
           05  w-cnt-swc-esp.
      *            *---------------------------------------------------*
      *            * Deviatore per l'expand relativo a tutte le righe  *
      *            *---------------------------------------------------*
               10  w-cnt-swc-esp-gen     pic  9(01)       value zero .
      *            *---------------------------------------------------*
      *            * Deviatore per l'expand relativo alla singola riga *
      *            *---------------------------------------------------*
               10  w-cnt-swc-esp-rig     pic  9(01)       value zero .
      *            *---------------------------------------------------*
      *            * Deviatore per l'expand relativo al 'find' su pro- *
      *            * dotto                                             *
      *            *                                                   *
      *            *  - 'zero' : Reset                                 *
      *            *  - '1'    : Utilizzo                              *
      *            *  - '>1'   : Disattivo                             *
      *            *---------------------------------------------------*
               10  w-cnt-swc-fnd-pro     pic  9(05)                  .

      *    *===========================================================*
      *    * Work-area personalizzazioni                               *
      *    *-----------------------------------------------------------*
       01  w-prs.
      *        *-------------------------------------------------------*
      *        * Numero livelli del piano dei conti                    *
      *        *-------------------------------------------------------*
           05  w-prs-liv-pdc              pic  9(01)                  .
      *        *-------------------------------------------------------*
      *        * Tipo visualizzazione note nelle righe documento       *
      *        *-------------------------------------------------------*
           05  w-prs-tvn-rgd              pic  9(02)                  .

      *    *===========================================================*
      *    * Work per subroutines di Let                               *
      *    *-----------------------------------------------------------*
       01  w-let.
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [ofx]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-ofx.
               10  w-let-arc-ofx-flg      pic  x(01)                  .
               10  w-let-arc-ofx-prt      pic  9(11)                  .
               10  w-let-arc-ofx-prg      pic  9(05)                  .
               10  w-let-arc-ofx-trc      pic  9(02)                  .
               10  w-let-arc-ofx-des.
                   15  w-let-arc-ofx-drg occurs 10
                                          pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [aaf]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-aaf.
               10  w-let-arc-aaf-flg      pic  x(01)                  .
               10  w-let-arc-aaf-tpm      pic  9(02)                  .
               10  w-let-arc-aaf-cdm      pic  9(07)                  .
               10  w-let-arc-aaf-fnt      pic  9(07)                  .
               10  w-let-arc-aaf-fda      pic  x(14)                  .
               10  w-let-arc-aaf-csf      pic  x(14)                  .
               10  w-let-arc-aaf-dmg.
                   15  w-let-arc-aaf-drm occurs 10
                                          pic  x(40)                  .
               10  w-let-arc-aaf-des.
                   15  w-let-arc-aaf-drg occurs 10
                                          pic  x(40)                  .
               10  w-let-arc-aaf-snu      pic  x(01)                  .
               10  w-let-arc-aaf-umf      pic  x(03)                  .
               10  w-let-arc-aaf-ndu      pic  9(01)                  .
               10  w-let-arc-aaf-cmu      pic  9(06)v9(03)            .
               10  w-let-arc-aaf-cdu      pic  9(06)v9(03)            .
               10  w-let-arc-aaf-tpa      pic  9(02)                  .
               10  w-let-arc-aaf-vlt      pic  x(03)                  .
               10  w-let-arc-aaf-dcv      pic  9(01)                  .
               10  w-let-arc-aaf-ndp      pic  9(01)                  .
               10  w-let-arc-aaf-lda      pic  9(06)v9(03)            .
               10  w-let-arc-aaf-tap      pic  9(02)                  .
               10  w-let-arc-aaf-lvv      pic  x(03)                  .
               10  w-let-arc-aaf-lvd      pic  9(01)                  .
               10  w-let-arc-aaf-lvt      pic  x(01)                  .
               10  w-let-arc-aaf-lvc      pic  9(06)v9(05)            .
               10  w-let-arc-aaf-lvp      pic  9(01)v9(02)            .
               10  w-let-arc-aaf-tbp.
                   15  w-let-arc-aaf-etp occurs 06.
                       20  w-let-arc-aaf-qtp
                                          pic  9(06)v9(03)            .
                       20  w-let-arc-aaf-pzp
                                          pic  9(09)                  .
                       20  w-let-arc-aaf-csp
                                          pic  9(05)                  .
                       20  w-let-arc-aaf-psp occurs 05
                                          pic  9(02)v9(01)            .
               10  w-let-arc-aaf-ctr      pic  9(02)                  .
               10  w-let-arc-aaf-apv      pic  9(07)                  .
               10  w-let-arc-aaf-rif      pic  x(20)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [dcp]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-dcp.
               10  w-let-arc-dcp-flg      pic  x(01)                  .
               10  w-let-arc-dcp-cod      pic  9(07)                  .
               10  w-let-arc-dcp-alf      pic  x(14)                  .
               10  w-let-arc-dcp-des.
                   15  w-let-arc-dcp-drg occurs 10
                                          pic  x(40)                  .
               10  w-let-arc-dcp-dui      pic  x(40)                  .
               10  w-let-arc-dcp-tpr      pic  9(02)                  .
               10  w-let-arc-dcp-civ      pic  9(05)                  .
               10  w-let-arc-dcp-umi      pic  x(03)                  .
               10  w-let-arc-dcp-deq      pic  9(01)                  .
               10  w-let-arc-dcp-plb      pic  9(09)                  .
               10  w-let-arc-dcp-ctr      pic  9(02)                  .
               10  w-let-arc-dcp-lng      pic  x(03)                  .

      *    *===========================================================*
      *    * Work per Let su archivio [dcp] e [pdx]                    *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/ldcppdx0.ltw"                   .

      *    *===========================================================*
      *    * Work per linea corpo a video                              *
      *    *-----------------------------------------------------------*
       01  w-lin.
      *        *-------------------------------------------------------*
      *        * Immagine di una riga di corpo su video                *
      *        *-------------------------------------------------------*
           05  w-lin-imm.
      *            *---------------------------------------------------*
      *            * Linee di display per ogni riga corpo in scroll    *
      *            *---------------------------------------------------*
               10  w-lin-imm-dsp.
                   15  w-lin-imm-dsp-lin occurs 01
                                          pic  x(78)                  .
      *            *---------------------------------------------------*
      *            * Riga corpo in scroll rappresentata linearmente    *
      *            *---------------------------------------------------*
               10  w-lin-imm-scr redefines
                   w-lin-imm-dsp.
      *                *-----------------------------------------------*
      *                * Numero linea                                  *
      *                *-----------------------------------------------*
                   15  w-lin-imm-num-lin  pic  x(03)                  .
      *                *-----------------------------------------------*
      *                * Altri dati visualizzati                       *
      *                *-----------------------------------------------*
                   15  w-lin-imm-seg-tr1  pic  x(01)                  .
                   15  w-lin-imm-alf-000.
                       20  w-lin-imm-alf-001.
                           25  w-lin-imm-alf-rig
                                          pic  x(14)                  .
                   15  w-lin-imm-seg-tr2  pic  x(01)                  .
                   15  w-lin-imm-des-000.
                       20  w-lin-imm-des-001.
                           25  w-lin-imm-des-rig
                                          pic  x(32)                  .
                   15  w-lin-imm-seg-tr3  pic  x(01)                  .
                   15  w-lin-imm-qep-000.
                       20  w-lin-imm-qep-001.
                           25  w-lin-imm-qta-rig
                                          pic  x(11)                  .
                   15  w-lin-imm-seg-tr4  pic  x(01)                  .
                   15  w-lin-imm-imp-000.
                       20  w-lin-imm-imp-001.
                           25  w-lin-imm-imp-rig
                                          pic  x(14)                  .

      *    *===========================================================*
      *    * Work-area locale                                          *
      *    *-----------------------------------------------------------*
       01  w-wrk.
      *        *-------------------------------------------------------*
      *        * Work per trattamento numero documento                 *
      *        *-------------------------------------------------------*
           05  w-wrk-num-doc              pic  9(11)                  .
           05  w-wrk-num-doc-r            redefines
               w-wrk-num-doc                                          .
               10  w-wrk-ndo-saa          pic  9(03)                  .
               10  w-wrk-ndo-dpz          pic  9(02)                  .
               10  w-wrk-ndo-prg          pic  9(06)                  .
      *        *-------------------------------------------------------*
      *        * Work per contatori generici                           *
      *        *-------------------------------------------------------*
           05  w-wrk-ctr-001              pic  9(05)                  .
           05  w-wrk-ctr-002              pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Work per eventuale espansione descrizione in riga     *
      *        *-------------------------------------------------------*
           05  w-wrk-snx-ext              pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Work per descrizione in riga                          *
      *        *-------------------------------------------------------*
           05  w-wrk-des-rig.
               10  w-wrk-des-drg occurs 10
                                          pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Work per posizionamento progressivo                   *
      *        *-------------------------------------------------------*
           05  w-wrk-prg-alf              pic  x(05)                  .
           05  w-wrk-prg-pos              pic  9(03)                  .
           05  w-wrk-prg-car              pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Work per quantita' secondaria                         *
      *        *-------------------------------------------------------*
           05  w-wrk-qta-cfr              pic s9(06)v9(03)            .
           05  w-wrk-umi-sec              pic  x(03)                  .
      *        *-------------------------------------------------------*
      *        * Work per ricerca codice prodotto                      *
      *        *-------------------------------------------------------*
           05  w-wrk-fnd-pro.
               10  w-wrk-fnd-pro-alf      pic  x(14)                  .

      *    *===========================================================*
      *    * Work per subroutines di Ctl                               *
      *    *-----------------------------------------------------------*
       01  w-ctl.
      *        *-------------------------------------------------------*
      *        * Work per Ctl su tipo riga                             *
      *        *-------------------------------------------------------*
           05  w-ctl-tip-rig.
               10  w-ctl-tip-rig-flg      pic  x(01)                  .
               10  w-ctl-tip-rig-tri      pic  x(05)                  .
               10  w-ctl-tip-rig-tri-r redefines
                   w-ctl-tip-rig-tri.
                   15  w-ctl-tip-rig-chr occurs 5
                                          pic  x(01)                  .
               10  w-ctl-tip-rig-tri-c redefines
                   w-ctl-tip-rig-tri.
                   15  w-ctl-tip-rig-fil  pic  x(01)                  .
                   15  w-ctl-tip-rig-aoc  pic  x(03)                  .
                   15  w-ctl-tip-rig-fil  pic  x(01)                  .
               10  w-ctl-tip-rig-inx      pic  9(02)                  .

      *    *===========================================================*
      *    * Work per routine di visualizzazione note nelle righe do-  *
      *    * cumento                                                   *
      *    *-----------------------------------------------------------*
       01  w-vis-not-rgd.
      *        *-------------------------------------------------------*
      *        * Tipo operazione da effettuare                         *
      *        * - 'NO' : Abblencamento area note                      *
      *        * - 'PM' : Visualizzazione prompts                      *
      *        * - 'NT' : Visualizzazione note complete                *
      *        *-------------------------------------------------------*
           05  w-vis-not-rgd-top          pic  x(02)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per stringa da 80 caratteri                    *
      *        *-------------------------------------------------------*
           05  w-vis-not-rgd-x80          pic  x(80)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per puntatore di stringa                       *
      *        *-------------------------------------------------------*
           05  w-vis-not-rgd-pnt          pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per ridefinizione numero protocollo            *
      *        *-------------------------------------------------------*
           05  w-vis-not-rgd-npt          pic  9(11)                  .
           05  w-vis-not-rgd-npt-r redefines
               w-vis-not-rgd-npt.
               10  w-vis-not-rgd-saa      pic  9(03)                  .
               10  w-vis-not-rgd-dpz      pic  9(02)                  .
               10  w-vis-not-rgd-prg      pic  9(06)                  .
      *        *-------------------------------------------------------*
      *        * Comodi per note da 77 caratteri                       *
      *        *-------------------------------------------------------*
           05  w-vis-not-rgd-a77          pic  x(77)                  .
           05  w-vis-not-rgd-b77          pic  x(77)                  .

      *    *===========================================================*
      *    * Link-area per accettazione codice prodotto 'dcp'          *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/acoddcp0.acl"                   .

      *    *===========================================================*
      *    * Work per subroutine di accettazione                       *
      *    *-----------------------------------------------------------*
       01  w-aux.
      *        *-------------------------------------------------------*
      *        * Work per espansione record righe di ordine            *
      *        *-------------------------------------------------------*
           05  w-aux-rec-rig.
               10  w-aux-rec-rig-rtr      pic  x(01)                  .
               10  w-aux-rec-rig-c01      pic  9(05)                  .
               10  w-aux-rec-rig-c02      pic  9(03)                  .
               10  w-aux-rec-rig-c03      pic  9(05)                  .
               10  w-aux-rec-rig-c04      pic  9(05)                  .
               10  w-aux-rec-rig-c05      pic  9(05)                  .
               10  w-aux-rec-rig-c06      pic  9(05)                  .
               10  w-aux-rec-rig-cix      pic  9(03)                  .
               10  w-aux-rec-rig-nli      pic  9(05)                  .
               10  w-aux-rec-rig-crb      pic  9(05)                  .
               10  w-aux-rec-rig-max      pic  9(05) value 800        .
               10  w-aux-rec-rig-cpb      pic  9(05)                  .
               10  w-aux-rec-rig-cpa      pic  9(05)                  .
               10  w-aux-rec-rig-lt1      pic  x(03)                  .
               10  w-aux-rec-rig-lt2      pic  x(03)                  .
               10  w-aux-rec-rig-ltp      pic  x(17)                  .
               10  w-aux-rec-rig-svk.
                   15  w-aux-rec-rig-spg
                                          pic  9(05)                  .
                   15  w-aux-rec-rig-sal
                                          pic  x(14)                  .
                   15  w-aux-rec-rig-sde
                                          pic  x(20)                  .
                   15  w-aux-rec-rig-sqt
                                          pic  9(08)                  .
                   15  w-aux-rec-rig-sim
                                          pic  9(11)                  .
               10  w-aux-rec-rig-tor      pic  x(01)                  .
               10  w-aux-rec-rig-dor      pic  x(01)                  .
               10  w-aux-rec-rig-buf
                               occurs 800.
                   15  w-aux-rec-rig-key.
                       20  w-aux-rec-rig-kpg
                                          pic  9(05)                  .
                       20  w-aux-rec-rig-kal
                                          pic  x(14)                  .
                       20  w-aux-rec-rig-kde
                                          pic  x(20)                  .
                       20  w-aux-rec-rig-kqt
                                          pic  9(08)                  .
                       20  w-aux-rec-rig-kim
                                          pic  9(11)                  .
                   15  w-aux-rec-rig-prt  pic  9(11)       comp-3     .
                   15  w-aux-rec-rig-prg  pic  9(05)       comp-3     .
                   15  w-aux-rec-rig-alf  pic  x(14)                  .

      *    *===========================================================*
      *    * Work per subroutines di editing codice sottoconto         *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/wedtpdc0.wkl"                   .

      *    *===========================================================*
      *    * Work per subroutines di editing codice iva                *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/wedtzci0.wkl"                   .

      *    *===========================================================*
      *    * Work per subroutines di editing quantita' da incolonnare  *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wedtqta0.wkl"                   .

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
      *    * Link-area per espansione su righe ordine                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/orf/prg/cpy/porf301x.pgl"                   .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "mvideo" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/v"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "msegrt"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/s"                                  .

      ******************************************************************
       Procedure Division                using w-esp-rig-ofr
                                               v
                                               s                      .
      ******************************************************************

      *================================================================*
      *       Main                                                     *
      *================================================================*
       main-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo operazione      *
      *              *-------------------------------------------------*
           if        w-esp-rig-ofr-ope    =    "OP"
                     perform   opn-000    thru opn-999
           else if   w-esp-rig-ofr-ope    =    "CL"
                     perform   cls-000    thru cls-999
           else if   w-esp-rig-ofr-ope    =    "C?"
                     perform   tcm-000    thru tcm-999
           else if   w-esp-rig-ofr-ope    =    "ES"
                     perform   esp-000    thru esp-999                .
       main-999.
           exit      program                                          .

      *================================================================*
      *       Routines                                                 *
      *================================================================*

      *    *===========================================================*
      *    * Open                                                      *
      *    *-----------------------------------------------------------*
       opn-000.
      *              *-------------------------------------------------*
      *              * Incremento contatore Open in corso per il modu- *
      *              * lo                                              *
      *              *-------------------------------------------------*
           add       1                    to   w-cnt-ctr-opn          .
      *              *-------------------------------------------------*
      *              * Se questa non e' la prima Open per il modulo si *
      *              * esce                                            *
      *              *-------------------------------------------------*
           if        w-cnt-ctr-opn        >    1
                     go to opn-999.
       opn-100.
      *              *-------------------------------------------------*
      *              * Se questa e' la prima Open per il modulo        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura personalizzazioni                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Numero livelli del piano dei conti      *
      *                      *-----------------------------------------*
           perform   prs-liv-pdc-000      thru prs-liv-pdc-999        .
      *                      *-----------------------------------------*
      *                      * Tipo visualizzazione note nelle righe   *
      *                      *-----------------------------------------*
           perform   prs-tvn-rgd-000      thru prs-tvn-rgd-999        .
       opn-120.
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * [ofr]                                       *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/orf/fls/ioc/obj/iofofr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ofr                 .
      *                  *---------------------------------------------*
      *                  * [ofx]                                       *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/orf/fls/ioc/obj/iofofx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ofx                 .
      *                  *---------------------------------------------*
      *                  * [dcf]                                       *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofdcf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcf                 .
      *                  *---------------------------------------------*
      *                  * [aaf]                                       *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofaaf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaf                 .
      *                  *---------------------------------------------*
      *                  * [dcp]                                       *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
      *                  *---------------------------------------------*
      *                  * [pdx]                                       *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofpdx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdx                 .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice prodotto [dcp]  *
      *              *-------------------------------------------------*
           perform   cod-cod-dcp-opn-000  thru cod-cod-dcp-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo di determinazione quantita' da e-   *
      *              * vadere riga ordine fornitore                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se necessario                          *
      *                  *---------------------------------------------*
           if        w-prs-tvn-rgd        not  = 01 and
                     w-prs-tvn-rgd        not  = 11
                     go to opn-999.
      *                  *---------------------------------------------*
      *                  * Open modulo                                 *
      *                  *---------------------------------------------*
           perform   det-qev-rof-opn-000  thru det-qev-rof-opn-999    .
       opn-999.
           exit.

      *    *===========================================================*
      *    * Close                                                     *
      *    *-----------------------------------------------------------*
       cls-000.
      *              *-------------------------------------------------*
      *              * Decremento contatore Open in corso per il modu- *
      *              * lo                                              *
      *              *-------------------------------------------------*
           subtract  1                    from w-cnt-ctr-opn          .
      *              *-------------------------------------------------*
      *              * Se questa non e' l'ultima Close per il modulo   *
      *              * si esce                                         *
      *              *-------------------------------------------------*
           if        w-cnt-ctr-opn        >    zero
                     go to cls-999.
       cls-100.
      *              *-------------------------------------------------*
      *              * Se questa e' l'ultima Close per il modulo       *
      *              *-------------------------------------------------*
       cls-120.
      *              *-------------------------------------------------*
      *              * Close files                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * [ofr]                                       *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/orf/fls/ioc/obj/iofofr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ofr                 .
      *                  *---------------------------------------------*
      *                  * [ofx]                                       *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/orf/fls/ioc/obj/iofofx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ofx                 .
      *                  *---------------------------------------------*
      *                  * [dcf]                                       *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofdcf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcf                 .
      *                  *---------------------------------------------*
      *                  * [aaf]                                       *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofaaf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaf                 .
      *                  *---------------------------------------------*
      *                  * [dcp]                                       *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
      *                  *---------------------------------------------*
      *                  * [pdx]                                       *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofpdx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdx                 .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice prodotto [dcp] *
      *              *-------------------------------------------------*
           perform   cod-cod-dcp-cls-000  thru cod-cod-dcp-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo di determinazione quantita' da e-  *
      *              * vadere riga ordine fornitore                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se necessario                          *
      *                  *---------------------------------------------*
           if        w-prs-tvn-rgd        not  = 01 and
                     w-prs-tvn-rgd        not  = 11
                     go to cls-999.
      *                  *---------------------------------------------*
      *                  * Chiusura modulo                             *
      *                  *---------------------------------------------*
           perform   det-qev-rof-cls-000  thru det-qev-rof-cls-999    .
       cls-999.
           exit.

      *    *===========================================================*
      *    * Test di cancellabilita' per il modulo                     *
      *    *-----------------------------------------------------------*
       tcm-000.
      *              *-------------------------------------------------*
      *              * Se il contatore delle Open in corso per il mo-  *
      *              * dulo e' pari a zero si dichiara che e' cancel-  *
      *              * labile, altrimento che non lo e'                *
      *              *-------------------------------------------------*
           if        w-cnt-ctr-opn        =    zero
                     move  spaces         to   w-esp-rig-ofr-ope      .
       tcm-999.
           exit.

      *    *===========================================================*
      *    * Routine di Expand su righe ordine                         *
      *    *-----------------------------------------------------------*
       esp-000.
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
      *              * Default tipo ordinamento                        *
      *              *-------------------------------------------------*
           move      "R"                  to   w-aux-rec-rig-tor      .
           move      ">"                  to   w-aux-rec-rig-dor      .
       esp-050.
      *              *-------------------------------------------------*
      *              * Normalizzazione contatore righe                 *
      *              *-------------------------------------------------*
           move      zero                 to   w-aux-rec-rig-crb      .
      *              *-------------------------------------------------*
      *              * Normalizzazione deviatore per 'find' su pro-    *
      *              * dotto                                           *
      *              *-------------------------------------------------*
           move      zero                 to   w-cnt-swc-fnd-pro      .
      *              *-------------------------------------------------*
      *              * Normalizzazione comodo di accetazione           *
      *              *-------------------------------------------------*
           move      spaces               to   w-wrk-fnd-pro-alf      .
      *              *-------------------------------------------------*
      *              * Normalizzazione comodi di visualizzazione       *
      *              *-------------------------------------------------*
           move      "NO"                 to   w-edt-qta-inc-ope      .
           perform   edt-qta-inc-000      thru edt-qta-inc-999        .
       esp-100.
      *              *-------------------------------------------------*
      *              * Lettura righe record [ofr]                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Start                                       *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      w-esp-rig-ofr-prt    to   rf-ofr-num-prt         .
           move      zero                 to   rf-ofr-num-prg         .
           move      "pgm/orf/fls/ioc/obj/iofofr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ofr                 .
      *                  *---------------------------------------------*
      *                  * Se errore di start : uscita                 *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to esp-999.
       esp-200.
      *              *-------------------------------------------------*
      *              * Lettura righe [ofr]                             *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/orf/fls/ioc/obj/iofofr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ofr                 .
      *                  *---------------------------------------------*
      *                  * Se at end : a controllo contatore           *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to esp-500.
       esp-300.
      *              *-------------------------------------------------*
      *              * Se oltre il massimo : a controllo contatore     *
      *              *-------------------------------------------------*
           if        rf-ofr-num-prt       not  = w-esp-rig-ofr-prt
                     go to esp-500.
       esp-320.
      *              *-------------------------------------------------*
      *              * Selezioni sul record                            *
      *              *-------------------------------------------------*
       esp-400.
      *              *-------------------------------------------------*
      *              * Incremento numero records nel buffer            *
      *              *-------------------------------------------------*
           add       1                    to   w-aux-rec-rig-crb      .
      *              *-------------------------------------------------*
      *              * Test se buffer oltre il numero previsto         *
      *              *-------------------------------------------------*
           if        w-aux-rec-rig-crb    >    w-aux-rec-rig-max
                     go to esp-500.
       esp-410.
      *              *-------------------------------------------------*
      *              * Aggiornamento comodi di visualizzazione         *
      *              *-------------------------------------------------*
           move      "AG"                 to   w-edt-qta-inc-ope      .
           move      rf-ofr-qta-ord       to   w-edt-qta-inc-qta      .
           move      rf-ofr-dec-qta       to   w-edt-qta-inc-dec      .
           perform   edt-qta-inc-000      thru edt-qta-inc-999        .
       esp-420.
      *              *-------------------------------------------------*
      *              * Bufferizzazione riga in corso di trattamento    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Chiave di ordinamento                       *
      *                  *---------------------------------------------*
           perform   esp-key-000          thru esp-key-999            .
      *                  *---------------------------------------------*
      *                  * Numero protocollo                           *
      *                  *---------------------------------------------*
           move      rf-ofr-num-prt       to   w-aux-rec-rig-prt
                                              (w-aux-rec-rig-crb)     .
      *                  *---------------------------------------------*
      *                  * Numero progressivo                          *
      *                  *---------------------------------------------*
           move      rf-ofr-num-prg       to   w-aux-rec-rig-prg
                                              (w-aux-rec-rig-crb)     .
      *                  *---------------------------------------------*
      *                  * Codice alfanumerico  prodotto               *
      *                  *---------------------------------------------*
           if        rf-ofr-tip-rig       =    "P    "
                     move  rf-ofr-alf-mag to   w-aux-rec-rig-alf
                                              (w-aux-rec-rig-crb)
           else      move  spaces         to   w-aux-rec-rig-alf
                                              (w-aux-rec-rig-crb)     .
       esp-430.
      *              *-------------------------------------------------*
      *              * Riciclo a lettura                               *
      *              *-------------------------------------------------*
           go to     esp-200.
       esp-500.
      *              *-------------------------------------------------*
      *              * Ordinamento                                     *
      *              *-------------------------------------------------*
           perform   esp-srt-000          thru esp-srt-999            .
       esp-505.
      *              *-------------------------------------------------*
      *              * Controllo numero records letti con lo stesso    *
      *              * valore                                          *
      *              *-------------------------------------------------*
           if        w-aux-rec-rig-crb    =    zero
                     go to esp-999.
      *                  *---------------------------------------------*
      *                  * Determinazione numero pagine nel buffer     *
      *                  *---------------------------------------------*
           move      w-aux-rec-rig-crb    to   w-aux-rec-rig-cpb      .
           subtract  1                    from w-aux-rec-rig-cpb      .
           divide    7                    into w-aux-rec-rig-cpb      .
           add       1                    to   w-aux-rec-rig-cpb      .
      *                  *---------------------------------------------*
      *                  * Inizializzazione numero record nel buffer   *
      *                  * attualmente trattato                        *
      *                  *---------------------------------------------*
           move      1                    to   w-aux-rec-rig-c01      .
       esp-520.
      *              *-------------------------------------------------*
      *              * Salvataggio immagine video                      *
      *              *-------------------------------------------------*
           move      "SV"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Video in 'OFF'                                  *
      *              *-------------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Costruzione box di Expand                       *
      *              *-------------------------------------------------*
           move      "BX"                 to   v-ope                  .
           move      07                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      19                   to   v-lto                  .
           move      80                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Fincatura                                       *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      78                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      02                   to   v-pos                  .
           move      "N.R|    Codice    |           Descrizione         
      -              " | Quantita' |    Importo   "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Sottolineatura fincatura                        *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      78                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      02                   to   v-pos                  .
           move      "---+--------------+-------------------------------
      -              "-+-----------+--------------"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Indicatore colonna di ordinamento               *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      09                   to   v-lin                  .
      *
           if        w-aux-rec-rig-tor    =    "R"
                     move  03             to   v-pos
           else if   w-aux-rec-rig-tor    =    "P"
                     move  12             to   v-pos
           else if   w-aux-rec-rig-tor    =    "D"
                     move  36             to   v-pos
           else if   w-aux-rec-rig-tor    =    "Q"
                     move  58             to   v-pos
           else if   w-aux-rec-rig-tor    =    "I"
                     move  72             to   v-pos
           else      move  03             to   v-pos                  .
      *
           if        w-aux-rec-rig-dor    =    "<"
                     move  "/\"           to   v-alf
           else      move  "\/"           to   v-alf                  .
      *
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Griglia 1                                       *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      78                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      02                   to   v-pos                  .
           move      "   |              |                               
      -              " |           |              "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Griglia 2                                       *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      78                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      02                   to   v-pos                  .
           move      "   |              |                               
      -              " |           |              "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Griglia 3                                       *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      78                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      02                   to   v-pos                  .
           move      "   |              |                               
      -              " |           |              "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Griglia 4                                       *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      78                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      02                   to   v-pos                  .
           move      "   |              |                               
      -              " |           |              "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Griglia 5                                       *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      78                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      02                   to   v-pos                  .
           move      "   |              |                               
      -              " |           |              "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Griglia 6                                       *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      78                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      02                   to   v-pos                  .
           move      "   |              |                               
      -              " |           |              "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Griglia 7                                       *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      78                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      02                   to   v-pos                  .
           move      "   |              |                               
      -              " |           |              "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Sottolineatura di chiusura                      *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      78                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      02                   to   v-pos                  .
           move      "---+--------------+-------------------------------
      -              "-+-----------+--------------"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Estremi 'Documento'                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Literal documento                           *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      38                   to   v-pos                  .
           move      "Documento :"        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Codice tipo documento                       *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      50                   to   v-pos                  .
           move      w-esp-rig-ofr-tmo    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Literal 'nr.'                               *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      57                   to   v-pos                  .
           move      "nr."                to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Numero documento                            *
      *                  *---------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      18                   to   v-lin                  .
           move      60                   to   v-pos                  .
           move      w-esp-rig-ofr-num    to   w-wrk-num-doc          .
           move      w-wrk-ndo-prg        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Literal 'del'                               *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      67                   to   v-pos                  .
           move      "del"                to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Data documento                              *
      *                  *---------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      18                   to   v-lin                  .
           move      71                   to   v-pos                  .
           move      w-esp-rig-ofr-dat    to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       esp-540.
      *              *-------------------------------------------------*
      *              * Visualizzazione pagina video contenente         *
      *              * il record attualmente trattato                  *
      *              *-------------------------------------------------*
           perform   esp-vis-000          thru esp-vis-999            .
      *              *-------------------------------------------------*
      *              * Video in On                                     *
      *              *-------------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       esp-550.
      *              *-------------------------------------------------*
      *              * Determinazione numero linea a video             *
      *              *-------------------------------------------------*
           move      w-aux-rec-rig-c01    to   w-aux-rec-rig-nli      .
       esp-555.
           if        w-aux-rec-rig-nli    >    7
                     subtract  7          from w-aux-rec-rig-nli
                     go to esp-555.
      *                  *---------------------------------------------*
      *                  * Incremento numero linea a video per posi-   *
      *                  * zionamento verticale                        *
      *                  *---------------------------------------------*
           add       09                   to   w-aux-rec-rig-nli      .
       esp-560.
      *                  *---------------------------------------------*
      *                  * Espansione record attualmente trattato      *
      *                  *---------------------------------------------*
       esp-575.
      *                  *---------------------------------------------*
      *                  * Normalizzazione function-key                *
      *                  *---------------------------------------------*
           move      spaces               to   v-key                  .
      *                  *---------------------------------------------*
      *                  * Se il programma chiamante ha passato in     *
      *                  * link il codice prodotto e questa e' la      *
      *                  * prima chiamata di espansione, si esegue una *
      *                  * ricerca precablata                          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se passato il codice prodotto il   *
      *                      * link                                    *
      *                      *-----------------------------------------*
           if        w-esp-rig-ofr-pro    =    spaces
                     go to esp-577.
      *                      *-----------------------------------------*
      *                      * Test se gia' fatto un giro di ricerca   *
      *                      *-----------------------------------------*
           if        w-cnt-swc-fnd-pro    >    1
                     go to esp-577.
      *                      *-----------------------------------------*
      *                      * Incremento deviatore per il 'find'      *
      *                      *-----------------------------------------*
           add       1                    to   w-cnt-swc-fnd-pro      .
      *                      *-----------------------------------------*
      *                      * Valore passato dal chiamante            *
      *                      *-----------------------------------------*
           move      w-esp-rig-ofr-pro    to   w-wrk-fnd-pro-alf      .
      *                      *-----------------------------------------*
      *                      * A 'find'                                *
      *                      *-----------------------------------------*
           go to     esp-620.
       esp-577.
      *                  *---------------------------------------------*
      *                  * Accettazione del mark-point                 *
      *                  *---------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "K"                  to   v-tip                  .
           move      spaces               to   v-ufk                  .
      *
           if        w-aux-rec-rig-c01    >    1
                     move  "UP  "         to   v-pfk (01)             .
      *
           if        w-aux-rec-rig-c01    <    w-aux-rec-rig-crb
                     move  "DOWN"         to   v-pfk (02)             .
      *
           move      "FIND"               to   v-pfk (03)             .
      *
           if        w-aux-rec-rig-cpa    >    1
                     move  "PRSC"         to   v-pfk (07)             .
      *
           if        w-aux-rec-rig-cpa    <    w-aux-rec-rig-cpb
                     move  "NXSC"         to   v-pfk (08)             .
      *
           if        w-aux-rec-rig-cpa    >    1
                     move  "BACK"         to   v-pfk (09)             .
      *
           if        w-aux-rec-rig-cpa    <    w-aux-rec-rig-cpb
                     move  "TAB "         to   v-pfk (10)             .
      *
           move      "EXPD"               to   v-pfk (15)             .
           move      "[1] "               to   v-pfk (16)             .
      *
           if        w-aux-rec-rig-tor    not  = "R"
                     move  "LEFT"         to   v-pfk (17)             .
      *
           if        w-aux-rec-rig-tor    not  = "I"
                     move  "RGHT"         to   v-pfk (18)             .
      *
           move      "EXIT"               to   v-pfk (20)             .
      *
           move      w-aux-rec-rig-nli    to   v-lin                  .
           move      21                   to   v-pos                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       esp-580.
           if        v-key                =    spaces or
                     v-key                =    "RTRN"
                     go to esp-582
           else if   v-key                =    "UP  "
                     go to esp-584
           else if   v-key                =    "DOWN"
                     go to esp-586
           else if   v-key                =    "EXIT"
                     go to esp-602
           else if   v-key                =    "NXSC"
                     go to esp-592
           else if   v-key                =    "PRSC"
                     go to esp-594
           else if   v-key                =    "BACK"
                     go to esp-598
           else if   v-key                =    "TAB "
                     go to esp-600
           else if   v-key                =    "EXPD"
                     go to esp-606
           else if   v-key                =    "FIND"
                     go to esp-620
           else if   v-key                =    "[1] "
                     go to esp-700
           else if   v-key                =    "LEFT"
                     go to esp-710
           else if   v-key                =    "RGHT"
                     go to esp-720
           else      go to esp-575.
       esp-582.
      *              *=================================================*
      *              * Se Return                                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Inibito                                     *
      *                  *---------------------------------------------*
           go to     esp-575.
       esp-584.
      *              *=================================================*
      *              * Se Up                                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Decremento contatore                        *
      *                  *---------------------------------------------*
           subtract  1                    from w-aux-rec-rig-c01      .
      *                  *---------------------------------------------*
      *                  * Normalizzazione deviatore per Expand        *
      *                  *---------------------------------------------*
           move      0                    to   w-cnt-swc-esp-gen      .
      *                  *---------------------------------------------*
      *                  * Controllo su linea assoluta a video : la    *
      *                  * prima                                       *
      *                  *---------------------------------------------*
           if        w-aux-rec-rig-nli    =    10
                     go to esp-590
           else      go to esp-550.
       esp-586.
      *              *=================================================*
      *              * Se Down                                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione deviatore per Expand        *
      *                  *---------------------------------------------*
           move      0                    to   w-cnt-swc-esp-gen      .
      *                  *---------------------------------------------*
      *                  * Eventuale incremento contatore              *
      *                  *---------------------------------------------*
           if        w-aux-rec-rig-c01    <    w-aux-rec-rig-crb
                     add   1              to   w-aux-rec-rig-c01
                     go to esp-588
           else      go to esp-575.
       esp-588.
      *                  *---------------------------------------------*
      *                  * Controllo su linea assoluta a video :       *
      *                  * l'ultima                                    *
      *                  *---------------------------------------------*
           if        w-aux-rec-rig-nli    =    16
                     go to esp-590
           else      go to esp-550.
       esp-590.
      *                  *---------------------------------------------*
      *                  * Visualizzazione pagina video contenente     *
      *                  * il record attualmente trattato              *
      *                  *---------------------------------------------*
           perform   esp-vis-000          thru esp-vis-999            .
      *                  *---------------------------------------------*
      *                  * A determinazione numero linea               *
      *                  *---------------------------------------------*
           go to     esp-550.
       esp-592.
      *              *=================================================*
      *              * Se Next screen                                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione deviatore per Expand        *
      *                  *---------------------------------------------*
           move      0                    to   w-cnt-swc-esp-gen      .
      *                  *---------------------------------------------*
      *                  * Incremento del contatore per le pagine      *
      *                  *---------------------------------------------*
           add       1                    to   w-aux-rec-rig-cpa      .
      *                  *---------------------------------------------*
      *                  * A trattamento contatore per le pagine       *
      *                  *---------------------------------------------*
           go to     esp-596.
       esp-594.
      *              *=================================================*
      *              * Se Previous screen                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione deviatore per Expand        *
      *                  *---------------------------------------------*
           move      0                    to   w-cnt-swc-esp-gen      .
      *                  *---------------------------------------------*
      *                  * Decremento del contatore per le pagine      *
      *                  *---------------------------------------------*
           subtract  1                    from w-aux-rec-rig-cpa      .
      *                  *---------------------------------------------*
      *                  * A trattamento contatore per le pagine       *
      *                  *---------------------------------------------*
           go to     esp-596.
       esp-596.
      *              *-------------------------------------------------*
      *              * Trattamento contatore per le pagine             *
      *              *-------------------------------------------------*
           move      w-aux-rec-rig-cpa    to   w-aux-rec-rig-c01      .
           multiply  7                    by   w-aux-rec-rig-c01      .
           subtract  6                    from w-aux-rec-rig-c01      .
           go to     esp-590.
       esp-598.
      *              *=================================================*
      *              * Se Back                                         *
      *              *-------------------------------------------------*
           move      1                    to   w-aux-rec-rig-cpa      .
           go to     esp-596.
       esp-600.
      *              *=================================================*
      *              * Se Tab                                          *
      *              *-------------------------------------------------*
           move      w-aux-rec-rig-cpb    to   w-aux-rec-rig-cpa      .
           go to     esp-596.
       esp-602.
      *              *=================================================*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     esp-900.
       esp-606.
      *              *=================================================*
      *              * Se Expand                                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test sul deviatore per l'Expand             *
      *                  *---------------------------------------------*
           if        w-cnt-swc-esp-gen    =    0
                     go to esp-608
           else      go to esp-610.
       esp-608.
      *                  *---------------------------------------------*
      *                  * Se deviatore a '0'                          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Routine di espansione della riga        *
      *                      *-----------------------------------------*
           perform   esp-rig-000          thru esp-rig-999            .
      *                      *-----------------------------------------*
      *                      * Normalizzazione deviatore               *
      *                      *-----------------------------------------*
           move      1                    to   w-cnt-swc-esp-gen      .
      *                      *-----------------------------------------*
      *                      * Ad accettazione carattere               *
      *                      *-----------------------------------------*
           go to     esp-575.
       esp-610.
      *                  *---------------------------------------------*
      *                  * Se deviatore a '1'                          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazione deviatore               *
      *                      *-----------------------------------------*
           move      0                    to   w-cnt-swc-esp-gen      .
      *                      *-----------------------------------------*
      *                      * A trattamento Exit                      *
      *                      *-----------------------------------------*
           go to     esp-602.
       esp-620.
      *              *=================================================*
      *              * Se Find                                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Routine di ricerca per codice articolo      *
      *                  *---------------------------------------------*
           perform   esp-fnd-000          thru esp-fnd-999            .
      *                  *---------------------------------------------*
      *                  * Incremento deviatore per il 'find'          *
      *                  *---------------------------------------------*
           if        w-esp-rig-ofr-pro    not  = spaces
                     add   1              to   w-cnt-swc-fnd-pro      .
      *                  *---------------------------------------------*
      *                  * Test su esito accettazione                  *
      *                  *---------------------------------------------*
           if        w-aux-rec-rig-cix    =    zero
                     go to esp-575.
      *                  *---------------------------------------------*
      *                  * Posizionamento alla pagina contenente la    *
      *                  * riga trovata                                *
      *                  *---------------------------------------------*
           move      w-aux-rec-rig-cix    to   w-aux-rec-rig-c01      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione pagina video contenente il  *
      *                  * record attualmente trattato                 *
      *                  *---------------------------------------------*
           perform   esp-vis-000          thru esp-vis-999            .
      *                  *---------------------------------------------*
      *                  * A visualizzazione della pagina contenente   *
      *                  * il record trovato                           *
      *                  *---------------------------------------------*
           go to     esp-550.
       esp-700.
      *              *=================================================*
      *              * Se [1]                                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ordinamento crescente - decrescente         *
      *                  *---------------------------------------------*
           if        w-aux-rec-rig-dor    =    ">"
                     move  "<"            to   w-aux-rec-rig-dor
           else      move  ">"            to   w-aux-rec-rig-dor      .
      *                  *---------------------------------------------*
      *                  * A caricamento buffer                        *
      *                  *---------------------------------------------*
           go to     esp-050.
       esp-710.
      *              *=================================================*
      *              * Se Left                                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Colonna a sinistra                          *
      *                  *---------------------------------------------*
           if        w-aux-rec-rig-tor    =    "I"
                     move  "Q"            to   w-aux-rec-rig-tor
           else if   w-aux-rec-rig-tor    =    "Q"
                     move  "D"            to   w-aux-rec-rig-tor
           else if   w-aux-rec-rig-tor    =    "D"
                     move  "P"            to   w-aux-rec-rig-tor
           else if   w-aux-rec-rig-tor    =    "P"
                     move  "R"            to   w-aux-rec-rig-tor      .
      *                  *---------------------------------------------*
      *                  * A caricamento buffer                        *
      *                  *---------------------------------------------*
           go to     esp-050.
       esp-720.
      *              *=================================================*
      *              * Se Right                                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Colonna a destra                            *
      *                  *---------------------------------------------*
           if        w-aux-rec-rig-tor    =    "R"
                     move  "P"            to   w-aux-rec-rig-tor
           else if   w-aux-rec-rig-tor    =    "P"
                     move  "D"            to   w-aux-rec-rig-tor
           else if   w-aux-rec-rig-tor    =    "D"
                     move  "Q"            to   w-aux-rec-rig-tor
           else if   w-aux-rec-rig-tor    =    "Q"
                     move  "I"            to   w-aux-rec-rig-tor      .
      *                  *---------------------------------------------*
      *                  * A caricamento buffer                        *
      *                  *---------------------------------------------*
           go to     esp-050.
       esp-900.
      *              *=================================================*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     esp-999.
       esp-999.
           exit.
      *    *===========================================================*
      *    * Routine di visualizzazione pagina video contenente il     *
      *    * record attualmente trattato                               *
      *    *-----------------------------------------------------------*
       esp-vis-000.
      *              *-------------------------------------------------*
      *              * Preparazione contatori                          *
      *              *-------------------------------------------------*
           move      w-aux-rec-rig-c01    to   w-aux-rec-rig-c02      .
           add       6                    to   w-aux-rec-rig-c02      .
           divide    7                    into w-aux-rec-rig-c02      .
           move      w-aux-rec-rig-c02    to   w-aux-rec-rig-cpa      .
           subtract  1                    from w-aux-rec-rig-c02      .
           multiply  7                    by   w-aux-rec-rig-c02      .
           add       1                    to   w-aux-rec-rig-c02      .
           add       6
                     w-aux-rec-rig-c02  giving w-aux-rec-rig-c03      .
           move      w-aux-rec-rig-c03    to   w-aux-rec-rig-c04      .
           if        w-aux-rec-rig-c03    >    w-aux-rec-rig-crb
                     move  w-aux-rec-rig-crb
                                          to   w-aux-rec-rig-c03      .
      *              *-------------------------------------------------*
      *              * Posizionamento iniziale                         *
      *              *-------------------------------------------------*
           move      10                   to   w-aux-rec-rig-c05      .
       esp-vis-200.
      *              *-------------------------------------------------*
      *              * Preparazione riga da visualizzare               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura record [ofr]                        *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      w-aux-rec-rig-prt
                    (w-aux-rec-rig-c02)   to   rf-ofr-num-prt         .
           move      w-aux-rec-rig-prg
                    (w-aux-rec-rig-c02)   to   rf-ofr-num-prg         .
           move      "pgm/orf/fls/ioc/obj/iofofr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ofr                 .
      *                  *---------------------------------------------*
      *                  * Editing Numero d'ordine riga                *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      spaces               to   v-edm                  .
           move      w-aux-rec-rig-c02    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-lin-imm-num-lin      .
      *                  *---------------------------------------------*
      *                  * Separatore 1                                *
      *                  *---------------------------------------------*
           move      "|"                  to   w-lin-imm-seg-tr1      .
      *                  *---------------------------------------------*
      *                  * Scomposizione tipo riga                     *
      *                  *---------------------------------------------*
           move      rf-ofr-tip-rig       to   w-ctl-tip-rig-tri      .
      *                  *---------------------------------------------*
      *                  * Codice prodotto                             *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Assemblaggio                            *
      *                      *-----------------------------------------*
           move      14                   to   w-all-str-lun          .
           move      03                   to   w-all-str-num          .
      *
           if        w-ctl-tip-rig-chr (1)
                                          =    "C"
                     move  "(Comm."       to   w-all-str-cat (1)
                     move  w-ctl-tip-rig-aoc
                                          to   w-all-str-cat (2)
                     move  ")"            to   w-all-str-cat (3)
           else if   w-ctl-tip-rig-chr (1)
                                          =    "A"
                     move  "(Add."        to   w-all-str-cat (1)
                     move  w-ctl-tip-rig-aoc
                                          to   w-all-str-cat (2)
                     move  ")"            to   w-all-str-cat (3)
           else      move  spaces         to   w-all-str-cat (1)
                     move  rf-ofr-alf-mag to   w-all-str-cat (2)
                     move  spaces         to   w-all-str-cat (3)      .
      *
           perform   all-str-cat-000      thru all-str-cat-999        .
      *                      *-----------------------------------------*
      *                      * Allineamento a destra per commenti o    *
      *                      * addebiti                                *
      *                      *-----------------------------------------*
           if        w-ctl-tip-rig-chr (1)
                                          =    "C" or
                     w-ctl-tip-rig-chr (1)
                                          =    "A"
                     perform   all-str-adx-000
                                          thru all-str-adx-999        .
      *                      *-----------------------------------------*
      *                      * In campo di destinazione                *
      *                      *-----------------------------------------*
           move      w-all-str-alf        to   w-lin-imm-alf-rig      .
      *                  *---------------------------------------------*
      *                  * Separatore 2                                *
      *                  *---------------------------------------------*
           move      "|"                  to   w-lin-imm-seg-tr2      .
       esp-vis-300.
      *                  *---------------------------------------------*
      *                  * Descrizione per la riga                     *
      *                  *---------------------------------------------*
       esp-vis-310.
      *                      *-----------------------------------------*
      *                      * Deviazione in funzione del tipo di      *
      *                      * estensione : nel caso di estensione     *
      *                      * tipo 2 si utilizza la descrizione in    *
      *                      * riga comunque                           *
      *                      *-----------------------------------------*
           if        rf-ofr-des-ext       =    0
                     go to esp-vis-320
           else if   rf-ofr-des-ext       =    1
                     go to esp-vis-330
           else if   rf-ofr-des-ext       =    2
                     go to esp-vis-340
           else if   rf-ofr-des-ext       =    3
                     go to esp-vis-350.
       esp-vis-320.
      *                      *-----------------------------------------*
      *                      * Se descrizione in riga                  *
      *                      *-----------------------------------------*
           move      rf-ofr-des-rig       to   w-lin-imm-des-rig      .
      *                          *-------------------------------------*
      *                          * Oltre                               *
      *                          *-------------------------------------*
           go to     esp-vis-400.
       esp-vis-330.
      *                      *-----------------------------------------*
      *                      * Se estensione nel file [ofx]            *
      *                      *-----------------------------------------*
           move      rf-ofr-num-prt       to   w-let-arc-ofx-prt      .
           move      rf-ofr-num-prg       to   w-let-arc-ofx-prg      .
           move      11                   to   w-let-arc-ofx-trc      .
           perform   let-arc-ofx-000      thru let-arc-ofx-999        .
           move      w-let-arc-ofx-des    to   w-lin-imm-des-rig      .
      *                          *-------------------------------------*
      *                          * Oltre                               *
      *                          *-------------------------------------*
           go to     esp-vis-400.
       esp-vis-340.
      *                      *-----------------------------------------*
      *                      * Se estensione nel file [pdx] : solo per *
      *                      * riga di tipo 'P'                        *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Lettura archivio [dcp]              *
      *                          *-------------------------------------*
           move      rf-ofr-num-mag       to   w-let-dcp-pdx-cod      .
           move      spaces               to   w-let-dcp-pdx-tar      .
           move      zero                 to   w-let-dcp-pdx-arc      .
           move      rf-ofr-cod-lng       to   w-let-dcp-pdx-lng      .
           perform   let-dcp-pdx-000      thru let-dcp-pdx-999        .
           move      w-let-dcp-pdx-des    to   w-lin-imm-des-rig      .
      *                          *-------------------------------------*
      *                          * Oltre                               *
      *                          *-------------------------------------*
           go to     esp-vis-400.
       esp-vis-350.
      *                      *-----------------------------------------*
      *                      * Se estensione nel file [aaf] per il     *
      *                      * fornitore : solo per riga di tipo 'P'   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Lettura archivio [dcp]              *
      *                          *-------------------------------------*
           move      rf-ofr-num-mag       to   w-let-arc-dcp-cod      .
           move      rf-ofr-cod-lng       to   w-let-arc-dcp-lng      .
           perform   let-arc-dcp-000      thru let-arc-dcp-999        .
      *                          *-------------------------------------*
      *                          * Lettura record [aaf]                *
      *                          *-------------------------------------*
           move      rf-ofr-tip-mag       to   w-let-arc-aaf-tpm      .
           move      rf-ofr-num-mag       to   w-let-arc-aaf-cdm      .
           move      rf-ofr-cod-arc       to   w-let-arc-aaf-fnt      .
           move      rf-ofr-fda-pif       to   w-let-arc-aaf-fda      .
           move      w-let-arc-dcp-des    to   w-let-arc-aaf-dmg      .
           perform   let-arc-aaf-000      thru let-arc-aaf-999        .
           move      w-let-arc-aaf-des    to   w-lin-imm-des-rig      .
      *                          *-------------------------------------*
      *                          * Oltre                               *
      *                          *-------------------------------------*
           go to     esp-vis-400.
       esp-vis-400.
      *                  *---------------------------------------------*
      *                  * Separatore 3                                *
      *                  *---------------------------------------------*
           move      "|"                  to   w-lin-imm-seg-tr3      .
      *                  *---------------------------------------------*
      *                  * Editing quantita'                           *
      *                  *---------------------------------------------*
           move      "ED"                 to   w-edt-qta-inc-ope      .
      *
           if        rf-ofr-snx-tum       =    "P"
                     move  rf-ofr-qta-ord to   w-edt-qta-inc-qta
           else if   rf-ofr-snx-tum       =    "S"
                     move  rf-ofr-qta-ord to   w-edt-qta-inc-qta
           else      move  rf-ofr-qta-fda to   w-edt-qta-inc-qta      .
      *
           if        rf-ofr-snx-tum       =    "P"
                     move  rf-ofr-dec-qta to   w-edt-qta-inc-dec
           else if   rf-ofr-snx-tum       =    "S"
                     move  rf-ofr-nde-tum to   w-edt-qta-inc-dec
           else      move  rf-ofr-dec-qta to   w-edt-qta-inc-dec      .
      *
           move      "S"                  to   w-edt-qta-inc-sgn      .
           move      11                   to   w-edt-qta-inc-car      .
           perform   edt-qta-inc-000      thru edt-qta-inc-999        .
           move      w-edt-qta-inc-edt    to   w-lin-imm-qta-rig      .
      *                  *---------------------------------------------*
      *                  * Separatore 4                                *
      *                  *---------------------------------------------*
           move      "|"                  to   w-lin-imm-seg-tr4      .
      *                  *---------------------------------------------*
      *                  * Editing importo                             *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           if        rf-ofr-imp-rig       >    9999999999 or
                     rf-ofr-imp-rig       <   -9999999999
                     if    rf-ofr-dec-vpf =    0
                           move  13       to   v-car
                     else  move  12       to   v-car
           else      move  10             to   v-car                  .
           subtract  rf-ofr-dec-vpf       from v-car                  .
           move      rf-ofr-dec-vpf       to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           if        rf-ofr-imp-rig       >    9999999999 or
                     rf-ofr-imp-rig       <   -9999999999
                     move  "B"            to   v-edm
           else      move  "GB"           to   v-edm                  .
           move      rf-ofr-imp-rig       to   v-num                  .
           if        rf-ofr-dec-vpf       =    1
                     divide 10            into v-num
           else if   rf-ofr-dec-vpf       =    2
                     divide 100           into v-num
           else if   rf-ofr-dec-vpf       =    3
                     divide 1000          into v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-lin-imm-imp-rig      .
       esp-vis-600.
      *              *-------------------------------------------------*
      *              * Visualizzazione riga                            *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      78                   to   v-car                  .
           move      w-aux-rec-rig-c05    to   v-lin                  .
           move      02                   to   v-pos                  .
           move      w-lin-imm-dsp        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Incremento contatori                            *
      *              *-------------------------------------------------*
           add       1                    to   w-aux-rec-rig-c02      .
           add       1                    to   w-aux-rec-rig-c05      .
           if        w-aux-rec-rig-c02    not  > w-aux-rec-rig-c03
                     go to esp-vis-200.
       esp-vis-700.
           if        w-aux-rec-rig-c02    >    w-aux-rec-rig-c04
                     go to esp-vis-800.
           if        w-aux-rec-rig-crb    not  > 6
                     go to esp-vis-800.
      *                  *---------------------------------------------*
      *                  * Riga vuota con separatori                   *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      78                   to   v-car                  .
           move      w-aux-rec-rig-c05    to   v-lin                  .
           move      02                   to   v-pos                  .
           move      "   |              |                               
      -              " |           |              "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Incremento contatori                        *
      *                  *---------------------------------------------*
           add       1                    to   w-aux-rec-rig-c02      .
           add       1                    to   w-aux-rec-rig-c05      .
           go to    esp-vis-700.
       esp-vis-800.
      *              *-------------------------------------------------*
      *              * Literal 'pagina'                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Editing 'numero pagina'                     *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      w-aux-rec-rig-cpa    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-aux-rec-rig-lt1      .
      *                  *---------------------------------------------*
      *                  * Editing 'di pagine'                         *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      w-aux-rec-rig-cpb    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-aux-rec-rig-lt2      .
      *                  *---------------------------------------------*
      *                  * String                                      *
      *                  *---------------------------------------------*
           move      spaces               to   w-aux-rec-rig-ltp      .
           string    "Pagina "  delimited by   size
                     w-aux-rec-rig-lt1
                                delimited by   spaces
                     " di "     delimited by   size
                     w-aux-rec-rig-lt2
                                delimited by   spaces
                                          into w-aux-rec-rig-ltp      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione stringa composta            *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      17                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      w-aux-rec-rig-ltp    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       esp-vis-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     esp-vis-999.
       esp-vis-999.
           exit.

      *    *===========================================================*
      *    * Routine di preparazione chiavi di ordinamento delle righe *
      *    *-----------------------------------------------------------*
       esp-key-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare                     *
      *              *-------------------------------------------------*
           move      zero                 to   w-aux-rec-rig-kpg
                                              (w-aux-rec-rig-crb)     .
           move      spaces               to   w-aux-rec-rig-kal
                                              (w-aux-rec-rig-crb)     .
           move      spaces               to   w-aux-rec-rig-kde
                                              (w-aux-rec-rig-crb)     .
           move      zero                 to   w-aux-rec-rig-kqt
                                              (w-aux-rec-rig-crb)     .
           move      zero                 to   w-aux-rec-rig-kim
                                              (w-aux-rec-rig-crb)     .
      *              *-------------------------------------------------*
      *              * Progressivo                                     *
      *              *-------------------------------------------------*
           if        w-aux-rec-rig-tor    =    "R"
                     move  rf-ofr-num-prg to   w-aux-rec-rig-kpg
                                              (w-aux-rec-rig-crb)     .
      *              *-------------------------------------------------*
      *              * Codice alfanumerico prodotto                    *
      *              *-------------------------------------------------*
           if        w-aux-rec-rig-tor    =    "P"
                     move  rf-ofr-alf-mag to   w-aux-rec-rig-kal
                                              (w-aux-rec-rig-crb)     .
      *              *-------------------------------------------------*
      *              * Descrizione prodotto                            *
      *              *-------------------------------------------------*
           if        w-aux-rec-rig-tor    =    "D"
                     move  rf-ofr-des-rig to   w-aux-rec-rig-kde
                                              (w-aux-rec-rig-crb)     .
      *              *-------------------------------------------------*
      *              * Quantita' prodotto                              *
      *              *-------------------------------------------------*
           if        w-aux-rec-rig-tor    =    "Q"
                     move  rf-ofr-qta-ord to   w-aux-rec-rig-kqt
                                              (w-aux-rec-rig-crb)     .
      *              *-------------------------------------------------*
      *              * Importo in riga                                 *
      *              *-------------------------------------------------*
           if        w-aux-rec-rig-tor    =    "I"
                     move  rf-ofr-imp-rig to   w-aux-rec-rig-kim
                                              (w-aux-rec-rig-crb)     .
       esp-key-999.
           exit.

      *    *===========================================================*
      *    * Routine di ordinamento, crescente o decrescente, righe    *
      *    *-----------------------------------------------------------*
       esp-srt-000.
      *              *-------------------------------------------------*
      *              * Test se almeno due codici da ordinare           *
      *              *-------------------------------------------------*
           if        w-aux-rec-rig-crb < 2
                     go to esp-srt-999.
       esp-srt-050.
      *              *-------------------------------------------------*
      *              * Ciclo di ordinamento                            *
      *              *-------------------------------------------------*
           move      zero                 to   w-aux-rec-rig-c01      .
       esp-srt-100.
           add       1                    to   w-aux-rec-rig-c01      .
           if        w-aux-rec-rig-c01    =    w-aux-rec-rig-crb
                     go to esp-srt-999.
           move      w-aux-rec-rig-c01    to   w-aux-rec-rig-c02
                                               w-aux-rec-rig-c03      .
           move      w-aux-rec-rig-key
                    (w-aux-rec-rig-c01)   to   w-aux-rec-rig-svk      .
       esp-srt-200.
           add       1                    to   w-aux-rec-rig-c02      .
           if        w-aux-rec-rig-c02    >    w-aux-rec-rig-crb
                     go to esp-srt-300.

           if       (w-aux-rec-rig-dor    =    ">"                and
                     w-aux-rec-rig-key
                    (w-aux-rec-rig-c02)   >    w-aux-rec-rig-svk) or
                    (w-aux-rec-rig-dor    =    "<"                and
                     w-aux-rec-rig-key
                    (w-aux-rec-rig-c02)   <    w-aux-rec-rig-svk)
                     go to esp-srt-200.

           move      w-aux-rec-rig-c02    to   w-aux-rec-rig-c03      .
           move      w-aux-rec-rig-key
                    (w-aux-rec-rig-c02)   to   w-aux-rec-rig-svk      .
           go to     esp-srt-200.
       esp-srt-300.
           move      w-aux-rec-rig-c01    to   w-aux-rec-rig-c04      .
           if       (w-aux-rec-rig-dor    =    ">"                 and 
                     w-aux-rec-rig-svk    >    w-aux-rec-rig-key
                                              (w-aux-rec-rig-c04)) or
                    (w-aux-rec-rig-dor    =    "<"                 and 
                     w-aux-rec-rig-svk    <    w-aux-rec-rig-key
                                              (w-aux-rec-rig-c04))
                     go to esp-srt-100.

           move      w-aux-rec-rig-buf
                    (w-aux-rec-rig-c03)   to   w-aux-rec-rig-buf
                                              (w-aux-rec-rig-max)     .
           move      w-aux-rec-rig-buf
                    (w-aux-rec-rig-c04)   to   w-aux-rec-rig-buf
                                              (w-aux-rec-rig-c03)     .
           move      w-aux-rec-rig-buf
                    (w-aux-rec-rig-max)   to   w-aux-rec-rig-buf
                                              (w-aux-rec-rig-c04)     .
           go to     esp-srt-100.
       esp-srt-999.
           exit.

      *    *===========================================================*
      *    * Routine di Expand della singola riga                      *
      *    *-----------------------------------------------------------*
       esp-rig-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di espansione per la de-   *
      *              * scrizione effettuabile                          *
      *              *-------------------------------------------------*
           move      spaces               to   w-wrk-snx-ext          .
      *              *-------------------------------------------------*
      *              * Salvataggio immagine video                      *
      *              *-------------------------------------------------*
           move      "SV"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Video in 'OFF'                                  *
      *              *-------------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Costruzione box di Expand per la singola riga   *
      *              *-------------------------------------------------*
           move      "BX"                 to   v-ope                  .
           move      07                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      17                   to   v-lto                  .
      *                  *---------------------------------------------*
      *                  * Eventuale aggiustamento in presenza delle   *
      *                  * note in riga                                *
      *                  *---------------------------------------------*
           if        w-prs-tvn-rgd        not  = zero
                     add  03              to   v-lto                  .
           move      80                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Abblencamento note in righe documento           *
      *              *-------------------------------------------------*
           move      "NO"                 to   w-vis-not-rgd-top      .
           perform   vis-not-rgd-000      thru vis-not-rgd-999        .
       esp-rig-100.
      *              *-------------------------------------------------*
      *              * Lettura riga per espansione                     *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      w-aux-rec-rig-prt
                    (w-aux-rec-rig-c01)   to   rf-ofr-num-prt         .
           move      w-aux-rec-rig-prg
                    (w-aux-rec-rig-c01)   to   rf-ofr-num-prg         .
           move      "pgm/orf/fls/ioc/obj/iofofr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ofr                 .
       esp-rig-200.
      *              *-------------------------------------------------*
      *              * Visualizzazione prompts per riga corpo espansa  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Tipo riga                                   *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      19                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      "Tipo riga         :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Codice prodotto                             *
      *                  *---------------------------------------------*
           move      rf-ofr-tip-rig       to   w-ctl-tip-rig-tri      .
           perform   pmt-cod-pro-000      thru pmt-cod-pro-999        .
      *                  *---------------------------------------------*
      *                  * Formato di acquisizione                     *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      38                   to   v-pos                  .
           move      "Formato:"           to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Descrizione                                 *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      19                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      "Descrizione       :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Quantita'                                   *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      19                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      "Quantita'         :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Prezzo                                      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      19                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      "Prezzo            :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Legame valutario                            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se effettuare                      *
      *                      *-----------------------------------------*
           if        rf-ofr-sgl-vpl       =    spaces
                     go to esp-rig-210.
      *                      *-----------------------------------------*
      *                      * Visualizzazione                         *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      29                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      39                   to   v-pos                  .
           move      "Legame :                  +/-"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       esp-rig-210.
      *                  *---------------------------------------------*
      *                  * % sconto                                    *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      19                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      "% sconto          :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Importo                                     *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      09                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      55                   to   v-pos                  .
           move      "Importo :"          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Date consegna e flags                       *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      58                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      "Consegna richiesta:          [ ]    Prevista:     
      -              "     [ ]"           to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Codice iva                                  *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      19                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      "Codice iva        :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Contropartita                               *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      19                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      "Contropartita     :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione prompts per note in righe   *
      *                  * documento                                   *
      *                  *---------------------------------------------*
           move      "PM"                 to   w-vis-not-rgd-top      .
           perform   vis-not-rgd-000      thru vis-not-rgd-999        .
       esp-rig-400.
      *              *-------------------------------------------------*
      *              * Visualizzazione campi per riga corpo espansa    *
      *              *-------------------------------------------------*
       esp-rig-420.
      *                  *---------------------------------------------*
      *                  * Tipo riga                                   *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      23                   to   v-pos                  .
           move      w-ctl-tip-rig-chr (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       esp-rig-440.
      *                  *---------------------------------------------*
      *                  * Progressivo riga                            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Editing                                 *
      *                      *-----------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<"                  to   v-edm                  .
           move      w-aux-rec-rig-c01    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Preparazione della stringa              *
      *                      *-----------------------------------------*
           move      spaces               to   w-wrk-prg-alf          .
           string    "("        delimited by   size
                     v-edt      delimited by   spaces
                     ")"        delimited by   size
                                          into w-wrk-prg-alf          .
      *                      *-----------------------------------------*
      *                      * Preparazione posizione giustificata a   *
      *                      * destra                                  *
      *                      *-----------------------------------------*
           move      zero                 to   w-wrk-prg-pos          .
           move      05                   to   w-wrk-prg-car          .
           inspect   w-wrk-prg-alf    tallying w-wrk-prg-pos
                                          for  trailing spaces        .
           subtract  w-wrk-prg-pos        from w-wrk-prg-car          .
      *                      *-----------------------------------------*
      *                      * Visualizzazione della stringa           *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      w-wrk-prg-car        to   v-car                  .
           move      08                   to   v-lin                  .
           move      74                   to   v-pos                  .
           add       w-wrk-prg-pos        to   v-pos                  .
           move      w-wrk-prg-alf        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       esp-rig-460.
      *                  *---------------------------------------------*
      *                  * Codice prodotto                             *
      *                  *---------------------------------------------*
           perform   vis-cod-pro-000      thru vis-cod-pro-999        .
      *                  *---------------------------------------------*
      *                  * Formato d'acquisizione                      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      14                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      47                   to   v-pos                  .
           move      rf-ofr-fda-pif       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Codice prodotto per il fornitore            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se da visualizzare                 *
      *                      *-----------------------------------------*
           if        rf-ofr-cop-sfn       =    spaces
                     go to esp-rig-480.
      *                      *-----------------------------------------*
      *                      * Prompt                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      62                   to   v-pos                  .
           move      "[F]"                to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Valore                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      14                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      66                   to   v-pos                  .
           move      rf-ofr-cop-sfn       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       esp-rig-480.
      *                  *---------------------------------------------*
      *                  * Descrizione prodotto                        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Deviazione in funzione del tipo di      *
      *                      * estensione : nel caso di estensione     *
      *                      * tipo 2 si utilizza la descrizione       *
      *                      * in riga comunque                        *
      *                      *-----------------------------------------*
           if        rf-ofr-des-ext       =    0
                     go to esp-rig-482
           else if   rf-ofr-des-ext       =    1
                     go to esp-rig-483
           else if   rf-ofr-des-ext       =    2
                     go to esp-rig-484
           else if   rf-ofr-des-ext       =    3
                     go to esp-rig-485.
       esp-rig-482.
      *                          *-------------------------------------*
      *                          * Se descrizione in riga              *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      23                   to   v-pos                  .
           move      rf-ofr-des-rig       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                          *-------------------------------------*
      *                          * A trattamento quantita'             *
      *                          *-------------------------------------*
           go to     esp-rig-500.
       esp-rig-483.
      *                          *-------------------------------------*
      *                          * Se estensione nel file [ofx]        *
      *                          *-------------------------------------*
           move      rf-ofr-num-prt       to   w-let-arc-ofx-prt      .
           move      rf-ofr-num-prg       to   w-let-arc-ofx-prg      .
           move      11                   to   w-let-arc-ofx-trc      .
           perform   let-arc-ofx-000      thru let-arc-ofx-999        .
      *                          *-------------------------------------*
      *                          * Visualizzazione della I riga        *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      23                   to   v-pos                  .
           move      w-let-arc-ofx-drg (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                          *-------------------------------------*
      *                          * Attivazione del flag di espansione  *
      *                          * per la descrizione effettuabile     *
      *                          *-------------------------------------*
           move      "#"                  to   w-wrk-snx-ext          .
      *                          *-------------------------------------*
      *                          * A trattamento quantita'             *
      *                          *-------------------------------------*
           go to     esp-rig-500.
       esp-rig-484.
      *                          *-------------------------------------*
      *                          * Se estensione nel file [pdx]        *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Lettura record [dcp]            *
      *                              *---------------------------------*
           move      rf-ofr-num-mag       to   w-let-dcp-pdx-cod      .
           move      spaces               to   w-let-dcp-pdx-tar      .
           move      zero                 to   w-let-dcp-pdx-arc      .
           move      rf-ofr-cod-lng       to   w-let-dcp-pdx-lng      .
           perform   let-dcp-pdx-000      thru let-dcp-pdx-999        .
      *                              *---------------------------------*
      *                              * Visualizzazione della I riga    *
      *                              *---------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      23                   to   v-pos                  .
           move      w-let-dcp-pdx-drg (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                              *---------------------------------*
      *                              * Attivazione del flag di espan-  *
      *                              * sione per la descrizione effet- *
      *                              * tuabile                         *
      *                              *---------------------------------*
           move      "#"                  to   w-wrk-snx-ext          .
      *                          *-------------------------------------*
      *                          * A trattamento quantita'             *
      *                          *-------------------------------------*
           go to     esp-rig-500.
       esp-rig-485.
      *                          *-------------------------------------*
      *                          * Se estensione nel file [pdx]        *
      *                          * per fornitore                       *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Lettura archivio [dcp]          *
      *                              *---------------------------------*
           move      rf-ofr-num-mag       to   w-let-arc-dcp-cod      .
           move      rf-ofr-cod-lng       to   w-let-arc-dcp-lng      .
           perform   let-arc-dcp-000      thru let-arc-dcp-999        .
      *                              *---------------------------------*
      *                              * Lettura archivio [aaf]          *
      *                              *---------------------------------*
           move      rf-ofr-tip-mag       to   w-let-arc-aaf-tpm      .
           move      rf-ofr-num-mag       to   w-let-arc-aaf-cdm      .
           move      rf-ofr-cod-arc       to   w-let-arc-aaf-fnt      .
           move      rf-ofr-fda-pif       to   w-let-arc-aaf-fda      .
           move      w-let-arc-dcp-des    to   w-let-arc-aaf-dmg      .
           perform   let-arc-aaf-000      thru let-arc-aaf-999        .
      *                              *---------------------------------*
      *                              * Visualizzazione della I riga    *
      *                              *---------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      23                   to   v-pos                  .
           move      w-let-arc-aaf-drg (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                              *---------------------------------*
      *                              * Attivazione del flag di espan-  *
      *                              * sione per la descrizione effet- *
      *                              * tuabile                         *
      *                              *---------------------------------*
           move      "#"                  to   w-wrk-snx-ext          .
      *                              *---------------------------------*
      *                              * A trattamento quantita'         *
      *                              *---------------------------------*
           go to     esp-rig-500.
       esp-rig-500.
      *                  *---------------------------------------------*
      *                  * Esposizione o meno delle barrette che stan- *
      *                  * no ad indicare la presenza di ulteriori ri- *
      *                  * ghe di descrizione                          *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      22                   to   v-pos                  .
           if        w-wrk-snx-ext        =    spaces
                     move  spaces         to   v-alf
           else      move  "|"            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      63                   to   v-pos                  .
           if        w-wrk-snx-ext        =    spaces
                     move  spaces         to   v-alf
           else      move  "|"            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Quantita' primaria                          *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
      *
           if        rf-ofr-snx-tum       =    "P"
                     move  rf-ofr-dec-qta to   v-dec
           else if   rf-ofr-snx-tum       =    "S"
                     move  rf-ofr-nde-tum to   v-dec
           else      move  rf-ofr-dec-qta to   v-dec                  .
      *
           move      "S"                  to   v-sgn                  .
           move      "<BG"                to   v-edm                  .
           move      11                   to   v-lin                  .
           move      23                   to   v-pos                  .
      *
           if        rf-ofr-snx-tum       =    "P"
                     move  rf-ofr-qta-ord to   v-num
           else if   rf-ofr-snx-tum       =    "S"
                     move  rf-ofr-qta-fda to   v-num
           else      move  rf-ofr-qta-fda to   v-num                  .
      *
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       esp-rig-510.
      *                  *---------------------------------------------*
      *                  * Quantita' secondaria                        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se da visualizzare                 *
      *                      *-----------------------------------------*
           if        rf-ofr-snx-tum       not  = "S" and
                     rf-ofr-snx-tum       not  = "P"
                     go to esp-rig-520.
      *                      *-----------------------------------------*
      *                      * Visualizzazione                         *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Preparazione valore numerico da e-  *
      *                          * ditare                              *
      *                          *-------------------------------------*
           if        rf-ofr-snx-tum       =    "P"
                     move  rf-ofr-qta-fda to   w-wrk-qta-cfr
           else      move  rf-ofr-qta-ord to   w-wrk-qta-cfr          .
      *                          *-------------------------------------*
      *                          * Deviazione a seconda del valore da  *
      *                          * editare                             *
      *                          *-------------------------------------*
           if        w-wrk-qta-cfr        =    zero
                     go to esp-rig-512
           else      go to esp-rig-514.
       esp-rig-512.
      *                          *-------------------------------------*
      *                          * Se valore a zero                    *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Visualizzazione a spaces        *
      *                              *---------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      18                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      39                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                              *---------------------------------*
      *                              * Oltre                           *
      *                              *---------------------------------*
           go to     esp-rig-520.
       esp-rig-514.
      *                          *-------------------------------------*
      *                          * Se valore diverso da zero           *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Editing quantita'               *
      *                              *---------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           if        rf-ofr-snx-tum       =    "P"
                     move  rf-ofr-nde-tum to   v-dec
           else      move  rf-ofr-dec-qta to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "<GB"                to   v-edm                  .
           move      w-wrk-qta-cfr        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                              *---------------------------------*
      *                              * Preparazione unita' di misura   *
      *                              *---------------------------------*
           if        rf-ofr-snx-tum       =    "P"
                     move  rf-ofr-umf-tum to   w-wrk-umi-sec
           else      move  rf-ofr-umi-acq to   w-wrk-umi-sec          .
      *                              *---------------------------------*
      *                              * Composizione area editata com-  *
      *                              * pleta                           *
      *                              *---------------------------------*
           move      spaces               to   v-alf                  .
           string    "("
                                delimited by   size
                     w-wrk-umi-sec
                                delimited by   size
                     " "
                                delimited by   size
                     v-edt
                                delimited by   spaces
                     ")"
                                delimited by   size
                                          into v-alf                  .
      *                              *---------------------------------*
      *                              * Visualizzazione                 *
      *                              *---------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      18                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      39                   to   v-pos                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                              *---------------------------------*
      *                              * Oltre                           *
      *                              *---------------------------------*
           go to     esp-rig-520.
       esp-rig-520.
      *                  *---------------------------------------------*
      *                  * Sigla valuta per il prezzo                  *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      17                   to   v-pos                  .
           move      rf-ofr-sgl-vpp       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Prezzo                                      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      09                   to   v-car                  .
           move      rf-ofr-dec-vpf       to   v-dec                  .
           add       rf-ofr-dec-prz       to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BG"                to   v-edm                  .
           move      12                   to   v-lin                  .
           move      23                   to   v-pos                  .
           move      rf-ofr-prz-acq       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Legame valutario                            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se da visualizzare                 *
      *                      *-----------------------------------------*
           if        rf-ofr-sgl-vpl       =    spaces
                     go to esp-rig-540.
      *                      *-----------------------------------------*
      *                      * Sigla valuta                            *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      48                   to   v-pos                  .
           move      rf-ofr-sgl-vpl       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Cambio di riferimento                   *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      03                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "B"                  to   v-edm                  .
           move      12                   to   v-lin                  .
           move      52                   to   v-pos                  .
           move      rf-ofr-ccr-vpl       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Percentuale di limitazione              *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      02                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BD"                to   v-edm                  .
           move      12                   to   v-lin                  .
           move      69                   to   v-pos                  .
           move      rf-ofr-plm-vpl       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Tipo di limitazione                     *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      74                   to   v-pos                  .
           move      rf-ofr-tlm-vpl       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       esp-rig-540.
      *                  *---------------------------------------------*
      *                  * % sconto                                    *
      *                  *---------------------------------------------*
           move      zero                 to   w-wrk-ctr-001          .
       esp-rig-550.
           add       1                    to   w-wrk-ctr-001          .
           if        w-wrk-ctr-001        >    5
                     go to esp-rig-560.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      01                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BD"                to   v-edm                  .
           move      13                   to   v-lin                  .
           move      w-wrk-ctr-001        to   v-pos                  .
           multiply  05                   by   v-pos                  .
           add       18                   to   v-pos                  .
           move      rf-ofr-per-scr 
                    (w-wrk-ctr-001)       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           go to     esp-rig-550.
       esp-rig-560.
      *                  *---------------------------------------------*
      *                  * Importo                                     *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      rf-ofr-dec-vpf       to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BG"                to   v-edm                  .
           move      13                   to   v-lin                  .
           move      65                   to   v-pos                  .
           move      rf-ofr-imp-rig       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Consegna richiesta                          *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      14                   to   v-lin                  .
           move      23                   to   v-pos                  .
           move      rf-ofr-dcn-ric       to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Formato stampa data consegna richiesta      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      33                   to   v-pos                  .
           move      rf-ofr-fds-dcr       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Consegna prevista                           *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      14                   to   v-lin                  .
           move      49                   to   v-pos                  .
           move      rf-ofr-dcn-prv       to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Flag di conferma da fornitore               *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      59                   to   v-pos                  .
           move      rf-ofr-flg-cnf       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       esp-rig-580.
      *                  *---------------------------------------------*
      *                  * Codice iva                                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Editing                                 *
      *                      *-----------------------------------------*
           move      rf-ofr-cod-iva       to   w-edt-iva-cod          .
           perform   edt-cod-iva-000      thru edt-cod-iva-999        .
      *                      *-----------------------------------------*
      *                      * Visualizzazione                         *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      02                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BD"                to   v-edm                  .
           move      15                   to   v-lin                  .
           move      23                   to   v-pos                  .
           move      w-edt-iva-cie        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       esp-rig-600.
      *                  *---------------------------------------------*
      *                  * Contropartita                               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Editing con appoggio a sinistra         *
      *                      *-----------------------------------------*
           move      w-prs-liv-pdc        to   w-edt-cod-pdc-liv      .
           move      rf-ofr-ctp-acq       to   w-edt-cod-pdc-cod      .
           move      "B"                  to   w-edt-cod-pdc-edm      .
           perform   edt-pdc-asx-000      thru edt-pdc-asx-999        .
      *                      *-----------------------------------------*
      *                      * Visualizzazione                         *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      09                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      23                   to   v-pos                  .
           move      w-edt-cod-pdc-edt    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione note in righe documento     *
      *                  *---------------------------------------------*
           move      "NT"                 to   w-vis-not-rgd-top      .
           perform   vis-not-rgd-000      thru vis-not-rgd-999        .
       esp-rig-800.
      *              *-------------------------------------------------*
      *              * Video in On                                     *
      *              *-------------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       esp-rig-900.
      *              *-------------------------------------------------*
      *              * Normalizzazione function-key                    *
      *              *-------------------------------------------------*
           move      spaces               to   v-key                  .
      *              *-------------------------------------------------*
      *              * Accettazione carattere di presa visione         *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "K"                  to   v-tip                  .
           move      spaces               to   v-ufk                  .
           move      "DO  "               to   v-pfk (05)             .
           move      "EXPD"               to   v-pfk (15)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      16                   to   v-lin                  .
           move      78                   to   v-pos                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       esp-rig-920.
           if        v-key                =    "DO  " or
                     v-key                =    spaces
                     go to esp-rig-922
           else if   v-key                =    "EXIT"
                     go to esp-rig-924
           else if   v-key                =    "EXPD"
                     go to esp-rig-928
           else      go to esp-rig-900.
       esp-rig-922.
      *              *-------------------------------------------------*
      *              * Se Do o Return                                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Come per Exit                               *
      *                  *---------------------------------------------*
           go to     esp-rig-924.
       esp-rig-924.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ripristino immagine video                   *
      *                  *---------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     esp-rig-999.
       esp-rig-928.
      *              *-------------------------------------------------*
      *              * Se Expand                                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test sul deviatore per l'Expand             *
      *                  *---------------------------------------------*
           if        w-cnt-swc-esp-rig    =    0
                     go to esp-rig-930
           else      go to esp-rig-932.
       esp-rig-930.
      *                  *---------------------------------------------*
      *                  * Se deviatore a '0'                          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se l' espansione della descrizione non  *
      *                      * e' attuabile : a trattamento per Exit   *
      *                      *-----------------------------------------*
           if        w-wrk-snx-ext        =    spaces
                     go to esp-rig-924.
      *                      *-----------------------------------------*
      *                      * Routine di espansione della descrizione *
      *                      *-----------------------------------------*
           perform   esp-rig-des-000      thru esp-rig-des-999        .
      *                      *-----------------------------------------*
      *                      * Normalizzazione deviatore               *
      *                      *-----------------------------------------*
           move      1                    to   w-cnt-swc-esp-rig      .
      *                      *-----------------------------------------*
      *                      * Ad accettazione carattere               *
      *                      *-----------------------------------------*
           go to     esp-rig-900.
       esp-rig-932.
      *                  *---------------------------------------------*
      *                  * Se deviatore a '1'                          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazione deviatore               *
      *                      *-----------------------------------------*
           move      0                    to   w-cnt-swc-esp-rig      .
      *                      *-----------------------------------------*
      *                      * A trattamento Exit                      *
      *                      *-----------------------------------------*
           go to     esp-rig-924.
       esp-rig-999.
           exit.

      *    *===========================================================*
      *    * Routine di Expand della descrizione per la riga           *
      *    *-----------------------------------------------------------*
       esp-rig-des-000.
      *              *-------------------------------------------------*
      *              * Salvataggio immagine video                      *
      *              *-------------------------------------------------*
           move      "SV"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Video in 'OFF'                                  *
      *              *-------------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       esp-rig-des-010.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo estensione      *
      *              *-------------------------------------------------*
           if        rf-ofr-des-ext       =    0
                     go to esp-rig-des-800
           else if   rf-ofr-des-ext       =    1
                     go to esp-rig-des-020
           else if   rf-ofr-des-ext       =    2
                     go to esp-rig-des-030
           else if   rf-ofr-des-ext       =    3
                     go to esp-rig-des-040.
       esp-rig-des-020.
      *              *-------------------------------------------------*
      *              * Lettura record [ofx]                            *
      *              *-------------------------------------------------*
           move      rf-ofr-num-prt       to   w-let-arc-ofx-prt      .
           move      rf-ofr-num-prg       to   w-let-arc-ofx-prg      .
           move      11                   to   w-let-arc-ofx-trc      .
           perform   let-arc-ofx-000      thru let-arc-ofx-999        .
           move      w-let-arc-ofx-des    to   w-wrk-des-rig          .
      *                  *---------------------------------------------*
      *                  * A calcolo del numero di righe utilizzate    *
      *                  *---------------------------------------------*
           go to     esp-rig-des-200.
       esp-rig-des-030.
      *              *-------------------------------------------------*
      *              * Lettura record [pdx]                            *
      *              *-------------------------------------------------*
           move      rf-ofr-num-mag       to   w-let-dcp-pdx-cod      .
           move      spaces               to   w-let-dcp-pdx-tar      .
           move      zero                 to   w-let-dcp-pdx-arc      .
           move      rf-ofr-cod-lng       to   w-let-dcp-pdx-lng      .
           perform   let-dcp-pdx-000      thru let-dcp-pdx-999        .
           move      w-let-dcp-pdx-des    to   w-wrk-des-rig          .
      *                  *---------------------------------------------*
      *                  * A calcolo del numero di righe utilizzate    *
      *                  *---------------------------------------------*
           go to     esp-rig-des-200.
       esp-rig-des-040.
      *              *-------------------------------------------------*
      *              * Lettura record [aaf] per fornitore              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura archivio [dcp]                      *
      *                  *---------------------------------------------*
           move      rf-ofr-num-mag       to   w-let-arc-dcp-cod      .
           move      rf-ofr-cod-lng       to   w-let-arc-dcp-lng      .
           perform   let-arc-dcp-000      thru let-arc-dcp-999        .
      *                  *---------------------------------------------*
      *                  * Lettura archivio [aaf]                      *
      *                  *---------------------------------------------*
           move      rf-ofr-tip-mag       to   w-let-arc-aaf-tpm      .
           move      rf-ofr-num-mag       to   w-let-arc-aaf-cdm      .
           move      rf-ofr-cod-arc       to   w-let-arc-aaf-fnt      .
           move      rf-ofr-fda-pif       to   w-let-arc-aaf-fda      .
           move      w-let-arc-dcp-des    to   w-let-arc-aaf-dmg      .
           perform   let-arc-aaf-000      thru let-arc-aaf-999        .
           move      w-let-arc-aaf-des    to   w-wrk-des-rig          .
      *                  *---------------------------------------------*
      *                  * A calcolo del numero di righe utilizzate    *
      *                  *---------------------------------------------*
           go to     esp-rig-des-200.
       esp-rig-des-200.
      *              *-------------------------------------------------*
      *              * Calcolo del numero di righe utilizzate          *
      *              *-------------------------------------------------*
           move      11                   to   w-wrk-ctr-001          .
       esp-rig-des-220.
           subtract  1                    from w-wrk-ctr-001          .
           if        w-wrk-ctr-001        =    zero
                     go to esp-rig-des-300.
           if        w-wrk-des-drg
                    (w-wrk-ctr-001)       =    spaces
                     go to esp-rig-des-220.
       esp-rig-des-300.
      *              *-------------------------------------------------*
      *              * Costruzione box di Expand in relazione al nume- *
      *              * ro di righe necessarie alla descrizione         *
      *              *-------------------------------------------------*
           move      "BX"                 to   v-ope                  .
           move      09                   to   v-lin                  .
           move      22                   to   v-pos                  .
           move      v-lin                to   v-lto                  .
           add       01                   to   v-lto                  .
           add       w-wrk-ctr-001        to   v-lto                  .
           move      63                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Visualizzazione delle righe di descrizione      *
      *              *-------------------------------------------------*
           move      zero                 to   w-wrk-ctr-002          .
       esp-rig-des-320.
           add       1                    to   w-wrk-ctr-002          .
           if        w-wrk-ctr-002        >    w-wrk-ctr-001
                     go to esp-rig-des-800.
      *                  *---------------------------------------------*
      *                  * Visualizzazione                             *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      09                   to   v-lin                  .
           add       w-wrk-ctr-002        to   v-lin                  .
           move      23                   to   v-pos                  .
           move      w-wrk-des-drg
                    (w-wrk-ctr-002)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     esp-rig-des-320.
       esp-rig-des-800.
      *              *-------------------------------------------------*
      *              * Video in On                                     *
      *              *-------------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       esp-rig-des-900.
      *              *-------------------------------------------------*
      *              * Normalizzazione function-key                    *
      *              *-------------------------------------------------*
           move      spaces               to   v-key                  .
      *              *-------------------------------------------------*
      *              * Accettazione carattere di presa visione         *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "K"                  to   v-tip                  .
           move      spaces               to   v-ufk                  .
           move      10                   to   v-lin                  .
           move      23                   to   v-pos                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Ripristino immagine video                       *
      *              *-------------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       esp-rig-des-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per espansione riga : codice pro- *
      *    * dotto                                                     *
      *    *-----------------------------------------------------------*
       pmt-cod-pro-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      19                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      03                   to   v-pos                  .
           if        w-ctl-tip-rig-chr (1)
                                          =    "A"
                     move  "Codice addebito   :"
                                          to   v-alf
           else if   w-ctl-tip-rig-chr (1)
                                          =    "C"
                     move  "Codice commento   :"
                                          to   v-alf
           else      move  "Codice prodotto   :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-cod-pro-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Codice prodotto                   *
      *    *-----------------------------------------------------------*
       vis-cod-pro-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      14                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      23                   to   v-pos                  .
      *
           if        w-ctl-tip-rig-chr (1)
                                          =    "C" or
                     w-ctl-tip-rig-chr (1)
                                          =    "A"
                     move  w-ctl-tip-rig-aoc
                                          to   v-alf
           else      move  rf-ofr-alf-mag to   v-alf                  .
      *
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-pro-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [ofx]                         *
      *    *-----------------------------------------------------------*
       let-arc-ofx-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-ofx-flg      .
      *              *-------------------------------------------------*
      *              * Test se codici nulli                            *
      *              *-------------------------------------------------*
           if        w-let-arc-ofx-prt    =    zero   or
                     w-let-arc-ofx-prg    =    zero   or
                     w-let-arc-ofx-trc    =    zero
                     go to let-arc-ofx-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      w-let-arc-ofx-prt    to   rf-ofx-num-prt         .
           move      w-let-arc-ofx-prg    to   rf-ofx-num-prg         .
           move      w-let-arc-ofx-trc    to   rf-ofx-tip-rec         .
           move      "pgm/orf/fls/ioc/obj/iofofx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ofx                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-ofx-400.
       let-arc-ofx-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-ofx-des-400       to   w-let-arc-ofx-des      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-ofx-999.
       let-arc-ofx-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-ofx-flg      .
           move      spaces               to   w-let-arc-ofx-des      .
           move      all   "."            to   w-let-arc-ofx-drg (1)  .
           go to     let-arc-ofx-999.
       let-arc-ofx-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-ofx-des      .
       let-arc-ofx-999.
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
      *    * Lettura personalizzazione : Tipo visualizzazione note     *
      *    *                             nelle righe documento         *
      *    *-----------------------------------------------------------*
       prs-tvn-rgd-000.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione                       *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/orf/mov/orf300[tvn-rgd]"
                                          to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-num          to   w-prs-tvn-rgd
           else      move  zero           to   w-prs-tvn-rgd          .
      *              *-------------------------------------------------*
      *              * Normalizzazione personalizzazione               *
      *              *-------------------------------------------------*
           if        w-prs-tvn-rgd        =    01 or
                     w-prs-tvn-rgd        =    11
                     go to prs-tvn-rgd-999.
           move      00                   to   w-prs-tvn-rgd          .
       prs-tvn-rgd-999.
           exit.

      *    *===========================================================*
      *    * Routine lettura archivio [dcp] e [pdx]                    *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/ldcppdx0.lts"                   .

      *    *===========================================================*
      *    * Routine di visualizzazione note durante l'impostazione    *
      *    * delle righe documento                                     *
      *    *-----------------------------------------------------------*
       vis-not-rgd-000.
      *              *-------------------------------------------------*
      *              * Se personalizzazione indica, nessuna visualiz-  *
      *              * zazione : uscita                                *
      *              *-------------------------------------------------*
           if        w-prs-tvn-rgd        =    zero
                     go to vis-not-rgd-999.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda del tipo operazione ri-    *
      *              * chiesto                                         *
      *              *-------------------------------------------------*
           if        w-vis-not-rgd-top    =    "NO"
                     go to vis-not-rgd-100
           else if   w-vis-not-rgd-top    =    "PM"
                     go to vis-not-rgd-200
           else if   w-vis-not-rgd-top    =    "NT"
                     go to vis-not-rgd-300
           else      go to vis-not-rgd-999.
       vis-not-rgd-100.
      *              *-------------------------------------------------*
      *              * Tipo operazione richiesto : Abblencamento area  *
      *              * note                                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Abblencamento totale area note              *
      *                  *---------------------------------------------*
           move      spaces               to   w-vis-not-rgd-a77      .
           move      spaces               to   w-vis-not-rgd-b77      .
      *                  *---------------------------------------------*
      *                  * A visualizzazione note operative            *
      *                  *---------------------------------------------*
           go to     vis-not-rgd-900.
       vis-not-rgd-200.
      *              *-------------------------------------------------*
      *              * Tipo operazione richiesto : Visualizzazione dei *
      *              * soli prompts                                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Linea di separazione                        *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      76                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      all "-"              to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Test se da effettuare                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test su tipo riga; se non superato si   *
      *                      * procede con l'abblencamento note        *
      *                      *-----------------------------------------*
           if        w-ctl-tip-rig-chr (1)
                                          =    "A" or
                     w-ctl-tip-rig-chr (1)
                                          =    "C" or
                     w-ctl-tip-rig-chr (1)
                                          =    "N"
                     go to vis-not-rgd-100.
       vis-not-rgd-210.
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda del tipo visualizza-   *
      *                  * zione                                       *
      *                  *---------------------------------------------*
           if        w-prs-tvn-rgd        =    01 or
                     w-prs-tvn-rgd        =    11
                     go to vis-not-rgd-220
           else      go to vis-not-rgd-999.
       vis-not-rgd-220.
      *                  *---------------------------------------------*
      *                  * Se tipo visualizzazione : 01                *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Composizione prima riga                 *
      *                      *-----------------------------------------*
           move      spaces               to   w-vis-not-rgd-x80      .
           move      1                    to   w-vis-not-rgd-pnt      .
       vis-not-rgd-222.
      *                          *-------------------------------------*
      *                          * Quantita' ricevuta                  *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Prompt                          *
      *                              *---------------------------------*
           string    "Quantita' ricevuta:"
                                delimited by   size
                                          into w-vis-not-rgd-x80
                                  with pointer w-vis-not-rgd-pnt      .
       vis-not-rgd-224.
      *                      *-----------------------------------------*
      *                      * Bufferizzazione prima riga composta     *
      *                      *-----------------------------------------*
           move      w-vis-not-rgd-x80    to   w-vis-not-rgd-a77      .
       vis-not-rgd-230.
      *                      *-----------------------------------------*
      *                      * Composizione seconda riga               *
      *                      *-----------------------------------------*
           move      spaces               to   w-vis-not-rgd-x80      .
           move      1                    to   w-vis-not-rgd-pnt      .
       vis-not-rgd-232.
      *                          *-------------------------------------*
      *                          * Ultima evasione                     *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Prompt                          *
      *                              *---------------------------------*
           string    "Ultima evasione   :"
                                delimited by   size
                                          into w-vis-not-rgd-x80
                                  with pointer w-vis-not-rgd-pnt      .
       vis-not-rgd-234.
      *                      *-----------------------------------------*
      *                      * Bufferizzazione seconda riga composta   *
      *                      *-----------------------------------------*
           move      w-vis-not-rgd-x80    to   w-vis-not-rgd-b77      .
       vis-not-rgd-238.
      *                      *-----------------------------------------*
      *                      * A visualizzazione note operative        *
      *                      *-----------------------------------------*
           go to     vis-not-rgd-280.
       vis-not-rgd-280.
      *                  *---------------------------------------------*
      *                  * A visualizzazione note operative            *
      *                  *---------------------------------------------*
           go to     vis-not-rgd-900.
       vis-not-rgd-300.
      *              *-------------------------------------------------*
      *              * Tipo operazione richiesto : Visualizzazione no- *
      *              * te complete                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se da effettuare                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test su tipo riga                       *
      *                      *-----------------------------------------*
           if        w-ctl-tip-rig-chr (1)
                                          =    "A" or
                     w-ctl-tip-rig-chr (1)
                                          =    "C" or
                     w-ctl-tip-rig-chr (1)
                                          =    "N"
                     go to vis-not-rgd-999.
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda del tipo visualizza-   *
      *                  * zione                                       *
      *                  *---------------------------------------------*
           if        w-prs-tvn-rgd        =    01 or
                     w-prs-tvn-rgd        =    11
                     go to vis-not-rgd-400
           else      go to vis-not-rgd-999.
       vis-not-rgd-400.
      *                  *---------------------------------------------*
      *                  * Se tipo visualizzazione : 01                *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Determinazione quantita' evasa riga or- *
      *                      * dine ed estremi ultima evasione         *
      *                      *-----------------------------------------*
           move      "DT"                 to   d-qev-rof-tip-ope      .
           perform   det-qev-rof-cll-000  thru det-qev-rof-cll-999    .
      *                      *-----------------------------------------*
      *                      * Composizione prima riga                 *
      *                      *-----------------------------------------*
           move      spaces               to   w-vis-not-rgd-x80      .
           move      1                    to   w-vis-not-rgd-pnt      .
       vis-not-rgd-410.
      *                          *-------------------------------------*
      *                          * Quantita' ricevuta                  *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Prompt                          *
      *                              *---------------------------------*
           string    "Quantita' ricevuta:"
                                delimited by   size
                                          into w-vis-not-rgd-x80
                                  with pointer w-vis-not-rgd-pnt      .
      *                              *---------------------------------*
      *                              * Valore                          *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Editing                     *
      *                                  *-----------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      rf-ofr-dec-qta       to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BG"                to   v-edm                  .
           move      d-qev-rof-qta-ord    to   v-num                  .
           if        d-qev-rof-qta-dri    >    zero
                     subtract d-qev-rof-qta-dri
                                          from v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                                  *-----------------------------*
      *                                  * String                      *
      *                                  *-----------------------------*
           string    " "
                                delimited by   size
                     v-edt
                                delimited by   spaces
                                          into w-vis-not-rgd-x80
                                  with pointer w-vis-not-rgd-pnt      .
       vis-not-rgd-420.
      *                      *-----------------------------------------*
      *                      * Bufferizzazione prima riga composta     *
      *                      *-----------------------------------------*
           move      w-vis-not-rgd-x80    to   w-vis-not-rgd-a77      .
       vis-not-rgd-450.
      *                      *-----------------------------------------*
      *                      * Composizione seconda riga               *
      *                      *-----------------------------------------*
           move      spaces               to   w-vis-not-rgd-x80      .
           move      1                    to   w-vis-not-rgd-pnt      .
       vis-not-rgd-460.
      *                          *-------------------------------------*
      *                          * Ultima evasione                     *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Prompt                          *
      *                              *---------------------------------*
           string    "Ultima evasione   :"
                                delimited by   size
                                          into w-vis-not-rgd-x80
                                  with pointer w-vis-not-rgd-pnt      .
      *                              *---------------------------------*
      *                              * Se estermi ultima evasione non  *
      *                              * esistenti : oltre               *
      *                              *---------------------------------*
           if        d-qev-rof-tdo-urc    =    spaces and
                     d-qev-rof-drg-urc    =    zero   and
                     d-qev-rof-prt-urc    =    zero
                     go to vis-not-rgd-470.
      *                              *---------------------------------*
      *                              * Tipo documento                  *
      *                              *---------------------------------*
           string    " "
                                delimited by   size
                     d-qev-rof-tdo-urc
                                delimited by   spaces
                                          into w-vis-not-rgd-x80
                                  with pointer w-vis-not-rgd-pnt      .
      *                              *---------------------------------*
      *                              * Numero documento                *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Numero protocollo in work   *
      *                                  * di comodo ridefinito        *
      *                                  *-----------------------------*
           move      d-qev-rof-prt-urc    to   w-vis-not-rgd-npt      .
      *                                  *-----------------------------*
      *                                  * Editing                     *
      *                                  *-----------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<"                  to   v-edm                  .
           move      w-vis-not-rgd-prg    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                                  *-----------------------------*
      *                                  * String                      *
      *                                  *-----------------------------*
           string    " "
                                delimited by   size
                     v-edt
                                delimited by   spaces
                                          into w-vis-not-rgd-x80
                                  with pointer w-vis-not-rgd-pnt      .
      *                              *---------------------------------*
      *                              * Data documento                  *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Editing                     *
      *                                  *-----------------------------*
           move      "ED"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      "<"                  to   v-edm                  .
           move      d-qev-rof-drg-urc    to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                                  *-----------------------------*
      *                                  * String                      *
      *                                  *-----------------------------*
           string    " del "
                                delimited by   size
                     v-edt
                                delimited by   spaces
                                          into w-vis-not-rgd-x80
                                  with pointer w-vis-not-rgd-pnt      .
       vis-not-rgd-470.
      *                      *-----------------------------------------*
      *                      * Bufferizzazione seconda riga composta   *
      *                      *-----------------------------------------*
           move      w-vis-not-rgd-x80    to   w-vis-not-rgd-b77      .
       vis-not-rgd-480.
      *                      *-----------------------------------------*
      *                      * A visualizzazione note operative        *
      *                      *-----------------------------------------*
           go to     vis-not-rgd-900.
       vis-not-rgd-900.
      *              *-------------------------------------------------*
      *              * Visualizzazione note operative                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Riga 1                                      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      77                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      w-vis-not-rgd-a77    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Riga 1                                      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      77                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      w-vis-not-rgd-b77    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     vis-not-rgd-999.
       vis-not-rgd-999.
           exit.

      *    *===========================================================*
      *    * Routine di Find per codice prodotto                       *
      *    *-----------------------------------------------------------*
       esp-fnd-000.
      *              *-------------------------------------------------*
      *              * Se e' gia' stato preparato il codice da cercare *
      *              * si salta la richiesta a video                   *
      *              *-------------------------------------------------*
           if        w-cnt-swc-fnd-pro    =    1
                     go to esp-fnd-600.
      *              *-------------------------------------------------*
      *              * Normalizzazione indice di posizionamento        *
      *              *-------------------------------------------------*
           move      zero                 to   w-aux-rec-rig-cix      .
      *              *-------------------------------------------------*
      *              * Salvataggio immagine video                      *
      *              *-------------------------------------------------*
           move      "SV"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Video in 'OFF'                                  *
      *              *-------------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Costruzione box di Expand per la visualizzazio- *
      *              * ne di tutte le righe del documento              *
      *              *-------------------------------------------------*
           move      "BX"                 to   v-ope                  .
           move      11                   to   v-lin                  .
           move      06                   to   v-pos                  .
           move      13                   to   v-lto                  .
           move      75                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       esp-fnd-100.
      *              *-------------------------------------------------*
      *              * Fincatura                                       *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      08                   to   v-pos                  .
           move      "Codice :"           to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       esp-fnd-200.
      *              *-------------------------------------------------*
      *              * Video in On                                     *
      *              *-------------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       esp-fnd-205.
      *              *-------------------------------------------------*
      *              * Accettazione codice prodotto                    *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-cod-dcp-ope      .
           move      "A"                  to   w-cod-cod-dcp-tac      .
           move      zero                 to   w-cod-cod-dcp-num      .
           move      w-wrk-fnd-pro-alf    to   w-cod-cod-dcp-alf      .
           move      12                   to   w-cod-cod-dcp-lin      .
           move      17                   to   w-cod-cod-dcp-pos      .
           move      12                   to   w-cod-cod-dcp-dln      .
           move      34                   to   w-cod-cod-dcp-dps      .
           move      spaces               to   v-edm                  .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "EXIT"               to   v-pfk (20)             .
           perform   cod-cod-dcp-cll-000  thru cod-cod-dcp-cll-999    .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           perform   cod-cod-dcp-foi-000  thru cod-cod-dcp-foi-999    .
       esp-fnd-210.
           perform   cod-cod-dcp-cll-000  thru cod-cod-dcp-cll-999    .
           if        w-cod-cod-dcp-ope    =    "F+"
                     go to esp-fnd-215.
           if        w-cod-cod-dcp-ope    =    "AC"
                     go to esp-fnd-220.
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       esp-fnd-215.
           perform   cod-cod-dcp-foi-000  thru cod-cod-dcp-foi-999    .
           go to     esp-fnd-210.
       esp-fnd-220.
           move      w-cod-cod-dcp-alf    to   v-alf                  .
           move      w-cod-cod-dcp-num    to   v-num                  .
       esp-fnd-250.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     go to esp-fnd-900.
       esp-fnd-300.
      *              *-------------------------------------------------*
      *              * Lettura archivio [dcp]                          *
      *              *-------------------------------------------------*
           move      v-num                to   w-let-arc-dcp-cod      .
           move      v-alf                to   w-let-arc-dcp-alf      .
           move      rf-ofr-cod-lng       to   w-let-arc-dcp-lng      .
           perform   let-arc-dcp-000      thru let-arc-dcp-999        .
           move      w-let-arc-dcp-alf    to   w-wrk-fnd-pro-alf      .
      *              *-------------------------------------------------*
      *              * Visualizzazione descrizione prodotto            *
      *              *-------------------------------------------------*
           perform   vis-cod-pro-des-000  thru vis-cod-pro-des-999    .
      *              *-------------------------------------------------*
      *              * Se codice a zero : uscita                       *
      *              *-------------------------------------------------*
           if        w-let-arc-dcp-cod    =    zero or
                     w-let-arc-dcp-alf    =    spaces
                     go to esp-fnd-900.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     go to esp-fnd-600.
       esp-fnd-400.
      *              *-------------------------------------------------*
      *              * Accettazione carattere di presa visione         *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "K"                  to   v-tip                  .
           move      spaces               to   v-ufk                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      12                   to   v-lin                  .
           move      74                   to   v-pos                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       esp-fnd-420.
           if        v-key                =    "DO  " or
                     v-key                =    spaces
                     go to esp-fnd-422
           else if   v-key                =    "EXIT"
                     go to esp-fnd-424
           else if   v-key                =    "UP  "
                     go to esp-fnd-426
           else      go to esp-fnd-900.
       esp-fnd-422.
      *              *-------------------------------------------------*
      *              * Se Do o Return                                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * A ricerca                                   *
      *                  *---------------------------------------------*
           go to     esp-fnd-600.
       esp-fnd-424.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     esp-fnd-900.
       esp-fnd-426.
      *              *-------------------------------------------------*
      *              * Se Up                                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione function-key                *
      *                  *---------------------------------------------*
           move      spaces               to   v-key                  .
      *                  *---------------------------------------------*
      *                  * Ad accettazione prodotto                    *
      *                  *---------------------------------------------*
           go to     esp-fnd-205.
       esp-fnd-600.
      *              *-------------------------------------------------*
      *              * Ricerca vera e propria                          *
      *              *-------------------------------------------------*
           perform   esp-fnd-ric-000      thru esp-fnd-ric-999        .
       esp-fnd-900.
      *              *-------------------------------------------------*
      *              * Test se ripristino da eseguire                  *
      *              *-------------------------------------------------*
           if        w-cnt-swc-fnd-pro    =    1
                     go to esp-fnd-999.
      *              *-------------------------------------------------*
      *              * Ripristino immagine video                       *
      *              *-------------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     esp-fnd-999.
       esp-fnd-999.
           exit.

      *    *===========================================================*
      *    * Routine di Find per codice prodotto                       *
      *    *                                                           *
      *    * Subroutine di ricerca                                     *
      *    *-----------------------------------------------------------*
       esp-fnd-ric-000.
      *              *-------------------------------------------------*
      *              * Inizio scansione                                *
      *              *-------------------------------------------------*
           move      w-aux-rec-rig-c01    to   w-aux-rec-rig-c06      .
           move      zero                 to   w-aux-rec-rig-cix      .
       esp-fnd-ric-200.
      *              *-------------------------------------------------*
      *              * Test max                                        *
      *              *-------------------------------------------------*
           add       1                    to   w-aux-rec-rig-c06      .
           if        w-aux-rec-rig-c06    >    w-aux-rec-rig-crb or
                     w-aux-rec-rig-c06    >    w-aux-rec-rig-max
                     go to esp-fnd-ric-900.
      *              *-------------------------------------------------*
      *              * Test di confronto                               *
      *              *-------------------------------------------------*
           if        w-wrk-fnd-pro-alf    =    w-aux-rec-rig-alf
                                              (w-aux-rec-rig-c06)
                     move  w-aux-rec-rig-c06
                                          to   w-aux-rec-rig-cix
                     go to esp-fnd-ric-900.
       esp-fnd-ric-600.
      *              *-------------------------------------------------*
      *              * Riciclo                                         *
      *              *-------------------------------------------------*
           go to     esp-fnd-ric-200.
       esp-fnd-ric-800.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
       esp-fnd-ric-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     esp-fnd-ric-999.
       esp-fnd-ric-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Descrizione Prodotto              *
      *    *-----------------------------------------------------------*
       vis-cod-pro-des-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      34                   to   v-pos                  .
           move      w-let-arc-dcp-des    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-pro-des-999.
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
           if        w-let-arc-dcp-cod    =    zero  
                     go to let-arc-dcp-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRO"             to   f-key                  .
           move      w-let-arc-dcp-cod    to   rf-dcp-num-pro         .
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
           move      rf-dcp-des-pro       to   w-let-arc-dcp-dui      .
           move      rf-dcp-tip-pro       to   w-let-arc-dcp-tpr      .
           move      rf-dcp-cod-iva       to   w-let-arc-dcp-civ      .
           move      rf-dcp-umi-ven       to   w-let-arc-dcp-umi      .
           move      rf-dcp-dec-qta       to   w-let-arc-dcp-deq      .
       let-arc-dcp-210.
      *                  *---------------------------------------------*
      *                  * Trattamento descrizione                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Bufferizzazione della descrizione       *
      *                      *-----------------------------------------*
           move      rf-dcp-des-pro       to   w-let-arc-dcp-des      .
      *                      *-----------------------------------------*
      *                      * Test se codice lingua diverso da "I  "  *
      *                      *-----------------------------------------*
           if        w-let-arc-dcp-lng    not  = "I  "
                     go to let-arc-dcp-350.
       let-arc-dcp-215.
      *                      *-----------------------------------------*
      *                      * Descrizione contenuta nel record        *
      *                      *-----------------------------------------*
           if        rf-dcp-des-pdx       =    0
                     go to let-arc-dcp-300.
      *                      *-----------------------------------------*
      *                      * Descrizione in file di estensione       *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Normalizzazione descrizione         *
      *                          *-------------------------------------*
           move      spaces               to   w-let-arc-dcp-des      .
      *                          *-------------------------------------*
      *                          * Normalizzazione contatore           *
      *                          *-------------------------------------*
           move      zero                 to   w-let-arc-dcp-ctr      .
       let-arc-dcp-220.
           add       1                    to   w-let-arc-dcp-ctr      .
           if        w-let-arc-dcp-ctr    >    10
                     go to let-arc-dcp-300.
      *                          *-------------------------------------*
      *                          * Lettura archivio [pdx]              *
      *                          *-------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "TRCLNG"             to   f-key                  .
           move      01                   to   rf-pdx-tip-rec         .
           move      zero                 to   rf-pdx-cod-arc         .
           move      "I  "                to   rf-pdx-cod-lng         .
           move      w-let-arc-dcp-cod    to   rf-pdx-cod-num         .
           move      spaces               to   rf-pdx-for-mat         .
           move      w-let-arc-dcp-ctr    to   rf-pdx-num-prg         .
           move      "pgm/dcp/fls/ioc/obj/iofpdx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdx                 .
      *                              *---------------------------------*
      *                              * Se record [pdx] non trovato :   *
      *                              * ad uscita                       *
      *                              *---------------------------------*
           if        f-sts                not  = e-not-err
                     go to let-arc-dcp-300.
      *                          *-------------------------------------*
      *                          * Bufferizzazione riga letta          *
      *                          *-------------------------------------*
           move      rf-pdx-des-pro       to   w-let-arc-dcp-drg
                                              (w-let-arc-dcp-ctr)     .
      *                          *-------------------------------------*
      *                          * Riciclo                             *
      *                          *-------------------------------------*
           go to     let-arc-dcp-220.
       let-arc-dcp-300.
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     let-arc-dcp-999.
       let-arc-dcp-350.
      *                  *---------------------------------------------*
      *                  * Se lingua diversa da "I  "                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Descrizione in lingua                   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Normalizzazione descrizione         *
      *                          *-------------------------------------*
           move      spaces               to   w-let-arc-dcp-des      .
      *                          *-------------------------------------*
      *                          * Normalizzazione contatore           *
      *                          *-------------------------------------*
           move      zero                 to   w-let-arc-dcp-ctr      .
       let-arc-dcp-370.
           add       1                    to   w-let-arc-dcp-ctr      .
           if        w-let-arc-dcp-ctr    >    10
                     go to let-arc-dcp-380.
      *                          *-------------------------------------*
      *                          * Lettura archivio [pdx]              *
      *                          *-------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "TRCLNG"             to   f-key                  .
           move      02                   to   rf-pdx-tip-rec         .
           move      zero                 to   rf-pdx-cod-arc         .
           move      w-let-arc-dcp-lng    to   rf-pdx-cod-lng         .
           move      w-let-arc-dcp-cod    to   rf-pdx-cod-num         .
           move      spaces               to   rf-pdx-for-mat         .
           move      w-let-arc-dcp-ctr    to   rf-pdx-num-prg         .
           move      "pgm/dcp/fls/ioc/obj/iofpdx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdx                 .
      *                              *---------------------------------*
      *                              * Se record [pdx] non trovato :   *
      *                              * ad uscita                       *
      *                              *---------------------------------*
           if        f-sts                not  = e-not-err
                     go to let-arc-dcp-380.
      *                          *-------------------------------------*
      *                          * Bufferizzazione riga letta          *
      *                          *-------------------------------------*
           move      rf-pdx-des-pro       to   w-let-arc-dcp-drg
                                              (w-let-arc-dcp-ctr)     .
      *                          *-------------------------------------*
      *                          * Riciclo                             *
      *                          *-------------------------------------*
           go to     let-arc-dcp-370.
       let-arc-dcp-380.
      *                      *-----------------------------------------*
      *                      * Test in uscita                          *
      *                      *-----------------------------------------*
           if        w-let-arc-dcp-des    =    spaces
                     move  rf-dcp-des-pro to   w-let-arc-dcp-des      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     let-arc-dcp-999.
       let-arc-dcp-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-dcp-flg      .
           move      spaces               to   w-let-arc-dcp-des      .
           move      all   "."            to   w-let-arc-dcp-drg (1)  .
           move      all   "."            to   w-let-arc-dcp-dui      .
           go to     let-arc-dcp-520.
       let-arc-dcp-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-dcp-des      .
           move      spaces               to   w-let-arc-dcp-dui      .
       let-arc-dcp-520.
           move      zero                 to   w-let-arc-dcp-tpr      .
           move      zero                 to   w-let-arc-dcp-civ      .
           move      spaces               to   w-let-arc-dcp-umi      .
           move      zero                 to   w-let-arc-dcp-deq      .
       let-arc-dcp-999.
           exit.

      *    *===========================================================*
      *    * Subroutines per accettazione codice prodotto 'dcp'        *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/acoddcp0.acs"                   .

      *    *===========================================================*
      *    * Routine di lettura archivio [aaf]                         *
      *    *-----------------------------------------------------------*
       let-arc-aaf-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-aaf-flg      .
      *              *-------------------------------------------------*
      *              * Normalizzazione record [aaf]                    *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofaaf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaf                 .
      *              *-------------------------------------------------*
      *              * Lettura record [aaf]                            *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "PRFNFM"             to   f-key                  .
           move      w-let-arc-aaf-tpm    to   rf-aaf-tip-mag         .
           move      w-let-arc-aaf-cdm    to   rf-aaf-num-pro         .
           move      w-let-arc-aaf-fnt    to   rf-aaf-cod-dcf         .
           move      w-let-arc-aaf-fda    to   rf-aaf-fda-pif         .
           move      "pgm/dcf/fls/ioc/obj/iofaaf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaf                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-aaf-400.
       let-arc-aaf-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-aaf-cop-sfn       to   w-let-arc-aaf-csf      .
           move      rf-aaf-snx-tum       to   w-let-arc-aaf-snu      .
           move      rf-aaf-umf-tum       to   w-let-arc-aaf-umf      .
           move      rf-aaf-nde-tum       to   w-let-arc-aaf-ndu      .
           move      rf-aaf-cmo-tum       to   w-let-arc-aaf-cmu      .
           move      rf-aaf-cdi-tum       to   w-let-arc-aaf-cdu      .
           move      rf-aaf-tip-pza       to   w-let-arc-aaf-tpa      .
           move      rf-aaf-sgl-vlt       to   w-let-arc-aaf-vlt      .
           move      rf-aaf-dec-vlt       to   w-let-arc-aaf-dcv      .
           move      rf-aaf-dec-prz       to   w-let-arc-aaf-ndp      .
           move      rf-aaf-lot-acq       to   w-let-arc-aaf-lda      .
           move      rf-aaf-tap-pes       to   w-let-arc-aaf-tap      .
           move      rf-aaf-lgv-vlt       to   w-let-arc-aaf-lvv      .
           move      rf-aaf-lgv-dcv       to   w-let-arc-aaf-lvd      .
           move      rf-aaf-lgv-tdc       to   w-let-arc-aaf-lvt      .
           move      rf-aaf-lgv-cdc       to   w-let-arc-aaf-lvc      .
           move      rf-aaf-lgv-pdt       to   w-let-arc-aaf-lvp      .
           move      rf-aaf-tmp-cns       to   w-let-arc-aaf-apv      .
           move      rf-aaf-rif-lst       to   w-let-arc-aaf-rif      .
           move      zero                 to   w-let-arc-aaf-ctr      .
       let-arc-aaf-202.
           add       1                    to   w-let-arc-aaf-ctr      .
           if        w-let-arc-aaf-ctr    >    6
                     go to let-arc-aaf-204.
           move      rf-aaf-qta-pes
                    (w-let-arc-aaf-ctr)   to   w-let-arc-aaf-qtp
                                              (w-let-arc-aaf-ctr)     .
           move      rf-aaf-prz-pes
                    (w-let-arc-aaf-ctr)   to   w-let-arc-aaf-pzp
                                              (w-let-arc-aaf-ctr)     .
           move      rf-aaf-csr-pes
                    (w-let-arc-aaf-ctr)   to   w-let-arc-aaf-csp
                                              (w-let-arc-aaf-ctr)     .
           move      rf-aaf-psr-pes
                    (w-let-arc-aaf-ctr, 1)
                                          to   w-let-arc-aaf-psp
                                              (w-let-arc-aaf-ctr, 1)  .
           move      rf-aaf-psr-pes
                    (w-let-arc-aaf-ctr, 2)
                                          to   w-let-arc-aaf-psp
                                              (w-let-arc-aaf-ctr, 2)  .
           move      rf-aaf-psr-pes
                    (w-let-arc-aaf-ctr, 3)
                                          to   w-let-arc-aaf-psp
                                              (w-let-arc-aaf-ctr, 3)  .
           move      rf-aaf-psr-pes
                    (w-let-arc-aaf-ctr, 4)
                                          to   w-let-arc-aaf-psp
                                              (w-let-arc-aaf-ctr, 4)  .
           move      rf-aaf-psr-pes
                    (w-let-arc-aaf-ctr, 5)
                                          to   w-let-arc-aaf-psp
                                              (w-let-arc-aaf-ctr, 5)  .
           go to     let-arc-aaf-202.
       let-arc-aaf-204.
      *                  *---------------------------------------------*
      *                  * Trattamento descrizione                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Descrizione contenuta nel record        *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test su flag di estensione          *
      *                          *-------------------------------------*
           if        rf-aaf-xdp-sfn       not  = 0
                     go to let-arc-aaf-210.
      *                          *-------------------------------------*
      *                          * Test se a spaces                    *
      *                          *-------------------------------------*
           if        rf-aaf-dep-sfn       not  = spaces
                     go to let-arc-aaf-208.
           if        w-let-arc-aaf-dmg    =    spaces
                     go to let-arc-aaf-208.
      *                          *-------------------------------------*
      *                          * Se la descrizione e' a spaces si    *
      *                          * utilizza quella letta dalla scheda  *
      *                          * anagrafica del codice magazzino     *
      *                          *-------------------------------------*
           move      w-let-arc-aaf-dmg    to   w-let-arc-aaf-des      .
      *                          *-------------------------------------*
      *                          * Ad uscita                           *
      *                          *-------------------------------------*
           go to     let-arc-aaf-300.
       let-arc-aaf-208.
      *                          *-------------------------------------*
      *                          * Descrizione per il fornitore        *
      *                          *-------------------------------------*
           move      rf-aaf-dep-sfn       to   w-let-arc-aaf-des      .
      *                          *-------------------------------------*
      *                          * Ad uscita                           *
      *                          *-------------------------------------*
           go to     let-arc-aaf-300.
       let-arc-aaf-210.
      *                      *-----------------------------------------*
      *                      * Descrizione in file di estensione       *
      *                      *-----------------------------------------*
           move      zero                 to   w-let-arc-aaf-ctr      .
       let-arc-aaf-220.
           add       1                    to   w-let-arc-aaf-ctr      .
           if        w-let-arc-aaf-ctr    >    10
                     go to let-arc-aaf-300.
      *                          *-------------------------------------*
      *                          * Lettura archivio [pdx]              *
      *                          *-------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "TRCLNG"             to   f-key                  .
           if        w-let-arc-aaf-tpm    =    01
                     move  13             to   rf-pdx-tip-rec
           else if   w-let-arc-aaf-tpm    =    03
                     move  33             to   rf-pdx-tip-rec
           else if   w-let-arc-aaf-tpm    =    04
                     move  43             to   rf-pdx-tip-rec         .
           move      w-let-arc-aaf-fnt    to   rf-pdx-cod-arc         .
           move      spaces               to   rf-pdx-cod-lng         .
           move      w-let-arc-aaf-cdm    to   rf-pdx-cod-num         .
           move      w-let-arc-aaf-fda    to   rf-pdx-for-mat         .
           move      w-let-arc-aaf-ctr    to   rf-pdx-num-prg         .
           move      "pgm/dcp/fls/ioc/obj/iofpdx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdx                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-aaf-300.
           move      rf-pdx-des-pro       to   w-let-arc-aaf-drg
                                              (w-let-arc-aaf-ctr)     .
           go to     let-arc-aaf-220.
       let-arc-aaf-300.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-aaf-999.
       let-arc-aaf-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-aaf-flg      .
           move      spaces               to   w-let-arc-aaf-des      .
           move      all   "."            to   w-let-arc-aaf-drg (1)  .
           go to     let-arc-aaf-520.
       let-arc-aaf-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-aaf-des      .
       let-arc-aaf-520.
           move      spaces               to   w-let-arc-aaf-dmg      .
           move      spaces               to   w-let-arc-aaf-csf      .
           move      spaces               to   w-let-arc-aaf-snu      .
           move      spaces               to   w-let-arc-aaf-umf      .
           move      zero                 to   w-let-arc-aaf-ndu      .
           move      zero                 to   w-let-arc-aaf-cmu      .
           move      zero                 to   w-let-arc-aaf-cdu      .
           move      zero                 to   w-let-arc-aaf-tpa      .
           move      spaces               to   w-let-arc-aaf-vlt      .
           move      zero                 to   w-let-arc-aaf-dcv      .
           move      zero                 to   w-let-arc-aaf-ndp      .
           move      zero                 to   w-let-arc-aaf-lda      .
           move      zero                 to   w-let-arc-aaf-tap      .
           move      spaces               to   w-let-arc-aaf-lvv      .
           move      zero                 to   w-let-arc-aaf-lvd      .
           move      zero                 to   w-let-arc-aaf-lvt      .
           move      zero                 to   w-let-arc-aaf-lvc      .
           move      zero                 to   w-let-arc-aaf-lvp      .
           move      zero                 to   w-let-arc-aaf-apv      .
           move      spaces               to   w-let-arc-aaf-rif      .
      *
           move      zero                 to   w-let-arc-aaf-ctr      .
       let-arc-aaf-522.
           add       1                    to   w-let-arc-aaf-ctr      .
           if        w-let-arc-aaf-ctr    >    6
                     go to let-arc-aaf-524.
           move      zero                 to   w-let-arc-aaf-qtp
                                              (w-let-arc-aaf-ctr)     .
           move      zero                 to   w-let-arc-aaf-pzp
                                              (w-let-arc-aaf-ctr)     .
           move      zero                 to   w-let-arc-aaf-csp
                                              (w-let-arc-aaf-ctr)     .
           move      zero                 to   w-let-arc-aaf-psp
                                              (w-let-arc-aaf-ctr, 1)  .
           move      zero                 to   w-let-arc-aaf-psp
                                              (w-let-arc-aaf-ctr, 2)  .
           move      zero                 to   w-let-arc-aaf-psp
                                              (w-let-arc-aaf-ctr, 3)  .
           move      zero                 to   w-let-arc-aaf-psp
                                              (w-let-arc-aaf-ctr, 4)  .
           move      zero                 to   w-let-arc-aaf-psp
                                              (w-let-arc-aaf-ctr, 5)  .
           go to     let-arc-aaf-522.
       let-arc-aaf-524.
       let-arc-aaf-999.
           exit.

      *    *===========================================================*
      *    * Editing del codice sottoconto con appoggio a sx o dx      *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/wedtpdc0.wks"                   .

      *    *===========================================================*
      *    * Subroutines per editing del codice iva                    *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/wedtzci0.wks"                   .

      *    *===========================================================*
      *    * Editing di una quantita' da incolonnare                   *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wedtqta0.wks"                   .

      *    *===========================================================*
      *    * Subroutines per determinazione quantita' da evadere riga  *
      *    * ordine fornitore                                          *
      *    *-----------------------------------------------------------*
           copy      "pgm/orf/prg/cpy/dqevrof0.dts"                   .

      *    *===========================================================*
      *    * Subroutines per allineamenti a destra o a sinistra oppure *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cps"                   .

