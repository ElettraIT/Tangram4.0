       Identification Division.
       Program-Id.                                 pfat301x           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    fat                 *
      *                                Settore:                        *
      *                                   Fase:                        *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 12/08/93    *
      *                       Ultima revisione:    NdK del 24/04/25    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      * ============================================================== *
      *                                                                *
      * Descrizione pgm:   Modulo di espansione della fattura per      *
      *                    visualizzare le righe                       *
      *                                                                *
      *                                                                *
      *                    ____ uniformare con 'pbol301x' ____         *
      *                    ____ 'exp-vis-000'             ____         *
      *                                                                *
      *                                                                *
      * ============================================================== *
      *                                                                *
      * Tipi operazione :                                              *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "OP"  Open modulo                                              *
      *                                                                *
      *              Input  : w-esp-rig-fir-ope : "OP"                 *
      *                                                                *
      *                                                                *
      *              Output : nessuno                                  *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "CL"  Close modulo                                             *
      *                                                                *
      *              Input  : w-esp-rig-fir-ope : "CL"                 *
      *                                                                *
      *                                                                *
      *              Output : nessuno                                  *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "C?"  Test di cancellabilita' del modulo                       *
      *                                                                *
      *              Input  : w-esp-rig-fir-ope : "C?"                 *
      *                                                                *
      *                                                                *
      *              Output : w-esp-rig-fir-ope : spaces = Si          *
      *                                           "C?"   = No          *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "ES"  Espansione                                               *
      *                                                                *
      *              Input  : w-esp-rig-fir-ope : "ES"                 *
      *                                                                *
      *                       w-esp-rig-fir-prt : protocollo fattura   *
      *                       w-esp-rig-fir-num : numero documento     *
      *                       w-esp-rig-fir-dat : data documento       *
      *                       w-esp-rig-fir-tmo : tipo documento       *
      *                       w-esp-rig-fir-pro : codice alfanumerico  *
      *                                           prodotto (opzionale) *
      *                                                                *
      *              Output : nessuno                                  *
      *                                                                *
      *                                                                *
      *       N.B.: L'operazione di espansione non salva la 'start'    *
      *             sulle righe fattura nel caso in cui venga effet-   *
      *             tuata una interrogazione sulle righe [fir]         *
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
      *        * [age]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/age/fls/rec/rfage"                          .
      *        *-------------------------------------------------------*
      *        * [dcc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfdcc"                          .
      *        *-------------------------------------------------------*
      *        * [zfp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfzfp"                          .
      *        *-------------------------------------------------------*
      *        * [zsf]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfzsf"                          .
      *        *-------------------------------------------------------*
      *        * [dcp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfdcp"                          .
      *        *-------------------------------------------------------*
      *        * [pdx]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfpdx"                          .

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
      *        * Si/No gestione agenti attiva                          *
      *        *-------------------------------------------------------*
           05  w-prs-age-snx              pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Tipo visualizzazione note nelle righe documento       *
      *        *-------------------------------------------------------*
           05  w-prs-tvn-rgd              pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Tipo visualizzazione                                  *
      *        *                                                       *
      *        * - '00' : Normale, solo le righe documento             *
      *        *                                                       *
      *        * - '01' : Con dati di testata e piede (vedi creen)     *
      *        *                                                       *
      *        *-------------------------------------------------------*
           05  w-prs-tip-esp              pic  9(02)                  .
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
      *    * Work per subroutines di Let                               *
      *    *-----------------------------------------------------------*
       01  w-let.
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [fix]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-fix.
               10  w-let-arc-fix-flg      pic  x(01)                  .
               10  w-let-arc-fix-prt      pic  9(09)                  .
               10  w-let-arc-fix-prg      pic  9(05)                  .
               10  w-let-arc-fix-trc      pic  9(02)                  .
               10  w-let-arc-fix-des.
                   15  w-let-arc-fix-drg occurs 10
                                          pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [dcp]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-dcp.
               10  w-let-arc-dcp-flg      pic  x(01)                  .
               10  w-let-arc-dcp-num      pic  9(07)                  .
               10  w-let-arc-dcp-alf      pic  x(14)                  .
               10  w-let-arc-dcp-des      pic  x(40)                  .
               10  w-let-arc-dcp-umi      pic  x(03)                  .
               10  w-let-arc-dcp-ndq      pic  9(01)                  .

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
      *        * Work per ricerca codice prodotto                      *
      *        *-------------------------------------------------------*
           05  w-wrk-fnd-pro.
               10  w-wrk-fnd-pro-alf      pic  x(14)                  .

      *    *===========================================================*
      *    * Work per subroutines di Det                               *
      *    *-----------------------------------------------------------*
       01  w-det.
      *        *-------------------------------------------------------*
      *        * Work per Det totale documento                         *
      *        *-------------------------------------------------------*
           05  w-det-tot-doc.
               10  w-det-tot-doc-ctr      pic  9(02)                  .
               10  w-det-tot-doc-tot      pic s9(11)                  .
               10  w-det-tot-doc-wpa      pic s9(11)                  .
               10  w-det-tot-doc-s11      pic s9(11)                  .
               10  w-det-tot-doc-wpr      pic s9(11)                  .

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
      *    * Work per accettazioni campi espansi                       *
      *    *-----------------------------------------------------------*
       01  w-exp.
      *        *-------------------------------------------------------*
      *        * Work per : Si/No addebito spese in fattura            *
      *        *-------------------------------------------------------*
           05  w-exp-spe-snx.
               10  w-exp-spe-snx-num      pic  9(02)       value 2    .
               10  w-exp-spe-snx-lun      pic  9(02)       value 02   .
               10  w-exp-spe-snx-tbl.
                   15  filler             pic  x(02) value
                            "No"                                      .
                   15  filler             pic  x(02) value
                            "Si"                                      .
      *        *-------------------------------------------------------*
      *        * Work per : Flag di significativita' provvigioni in    *
      *        * riga                                                  *
      *        *-------------------------------------------------------*
           05  w-exp-fsp-rig.
               10  w-exp-fsp-rig-num      pic  9(02)       value 3    .
               10  w-exp-fsp-rig-lun      pic  9(02)       value 10   .
               10  w-exp-fsp-rig-tbl.
                   15  filler             pic  x(10) value
                            "Si        "                              .
                   15  filler             pic  x(10) value
                            "No        "                              .
                   15  filler             pic  x(10) value
                            "Speciali  "                              .

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
      *        * Comodo per valore prezzo nella valuta per il prezzo   *
      *        *-------------------------------------------------------*
           05  w-vis-not-rgd-pvp          pic  9(09)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per valore scostamento                         *
      *        *-------------------------------------------------------*
           05  w-vis-not-rgd-vsc          pic s9(11)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per percentuale scostamento                    *
      *        *-------------------------------------------------------*
           05  w-vis-not-rgd-psc          pic s9(03)v9(01)            .
      *        *-------------------------------------------------------*
      *        * Comodi per note da 77 caratteri                       *
      *        *-------------------------------------------------------*
           05  w-vis-not-rgd-a77          pic  x(77)                  .
           05  w-vis-not-rgd-b77          pic  x(77)                  .

      *    *===========================================================*
      *    * Work per subroutine di accettazione                       *
      *    *-----------------------------------------------------------*
       01  w-aux.
      *        *-------------------------------------------------------*
      *        * Work per espansione record righe di fattura           *
      *        *-------------------------------------------------------*
           05  w-aux-rec-fir.
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
                   15  w-aux-rec-rig-prt  pic  9(09)       comp-3     .
                   15  w-aux-rec-rig-prg  pic  9(05)       comp-3     .
                   15  w-aux-rec-rig-alf  pic  x(14)                  .

      *    *===========================================================*
      *    * Link-area per accettazione codice prodotto 'dcp'          *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/acoddcp0.acl"                   .

      *    *===========================================================*
      *    * Work-area per conversioni rispetto alla valuta base       *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wcvsvlt0.cpw"                   .

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
      *    * Link-area per espansione su righe fattura                 *
      *    *-----------------------------------------------------------*
           copy      "pgm/fat/prg/cpy/pfat301x.pgl"                   .

      ******************************************************************
       Procedure Division                using w-esp-rig-fir          .
      ******************************************************************

      *================================================================*
      *       Main                                                     *
      *================================================================*
       main-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo operazione      *
      *              *-------------------------------------------------*
           if        w-esp-rig-fir-ope    =    "OP"
                     perform   opn-000    thru opn-999
           else if   w-esp-rig-fir-ope    =    "CL"
                     perform   cls-000    thru cls-999
           else if   w-esp-rig-fir-ope    =    "C?"
                     perform   tcm-000    thru tcm-999
           else if   w-esp-rig-fir-ope    =    "ES"
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
      *                      * Si/No gestione agenti attiva            *
      *                      *-----------------------------------------*
           perform   prs-age-snx-000      thru prs-age-snx-999        .
      *                      *-----------------------------------------*
      *                      * Tipo visualizzazione note nelle righe   *
      *                      *-----------------------------------------*
           perform   prs-tvn-rgd-000      thru prs-tvn-rgd-999        .
      *                      *-----------------------------------------*
      *                      * Tipo di espansione                      *
      *                      *-----------------------------------------*
           perform   prs-tip-esp-000      thru prs-tip-esp-999        .
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
      *                      *-----------------------------------------*
      *                      * Close tabella [zsf]                     *
      *                      *-----------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofzsf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zsf                 .
       opn-120.
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * [fit]                                       *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/fat/fls/ioc/obj/ioffit"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fit                 .
      *                  *---------------------------------------------*
      *                  * [fir]                                       *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/fat/fls/ioc/obj/ioffir"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fir                 .
      *                  *---------------------------------------------*
      *                  * [fix]                                       *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/fat/fls/ioc/obj/ioffix"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fix                 .
      *                  *---------------------------------------------*
      *                  * [age]                                       *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/age/fls/ioc/obj/iofage"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-age                 .
      *                  *---------------------------------------------*
      *                  * [dcc]                                       *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofdcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
      *                  *---------------------------------------------*
      *                  * [zfp]                                       *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofzfp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zfp                 .
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
      *                  * [fit]                                       *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/fat/fls/ioc/obj/ioffit"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fit                 .
      *                  *---------------------------------------------*
      *                  * [fir]                                       *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/fat/fls/ioc/obj/ioffir"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fir                 .
      *                  *---------------------------------------------*
      *                  * [fix]                                       *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/fat/fls/ioc/obj/ioffix"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fix                 .
      *                  *---------------------------------------------*
      *                  * [age]                                       *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/age/fls/ioc/obj/iofage"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-age                 .
      *                  *---------------------------------------------*
      *                  * [dcc]                                       *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofdcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
      *                  *---------------------------------------------*
      *                  * [zfp]                                       *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofzfp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zfp                 .
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
                     move  spaces         to   w-esp-rig-fir-ope      .
       tcm-999.
           exit.

      *    *===========================================================*
      *    * Routine di Expand su righe fattura                        *
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
      *              * Lettura righe record [fir]                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Start                                       *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      w-esp-rig-fir-prt    to   rf-fir-num-prt         .
           move      zero                 to   rf-fir-num-prg         .
           move      "pgm/fat/fls/ioc/obj/ioffir"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fir                 .
      *                  *---------------------------------------------*
      *                  * Se errore di start : uscita                 *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to esp-999.
       esp-200.
      *              *-------------------------------------------------*
      *              * Lettura righe [fir]                             *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/fat/fls/ioc/obj/ioffir"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fir                 .
      *                  *---------------------------------------------*
      *                  * Se at end : a controllo contatore           *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to esp-500.
       esp-300.
      *              *-------------------------------------------------*
      *              * Se oltre il massimo : a controllo contatore     *
      *              *-------------------------------------------------*
           if        rf-fir-num-prt       not  = w-esp-rig-fir-prt
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
           move      rf-fir-qta-ven       to   w-edt-qta-inc-qta      .
           move      rf-fir-dec-qta       to   w-edt-qta-inc-dec      .
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
           move      rf-fir-num-prt       to   w-aux-rec-rig-prt
                                              (w-aux-rec-rig-crb)     .
      *                  *---------------------------------------------*
      *                  * Numero progressivo                          *
      *                  *---------------------------------------------*
           move      rf-fir-num-prg       to   w-aux-rec-rig-prg
                                              (w-aux-rec-rig-crb)     .
      *                  *---------------------------------------------*
      *                  * Codice alfanumerico  prodotto               *
      *                  *---------------------------------------------*
           if        rf-fir-tip-rig       =    "P    "
                     move  rf-fir-alf-pro to   w-aux-rec-rig-alf
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
           divide    6                    into w-aux-rec-rig-cpb      .
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
           perform   esp-box-000          thru esp-box-999            .
       esp-540.
      *              *-------------------------------------------------*
      *              * Visualizzazione pagina video contenente         *
      *              * il record attualmente trattato                  *
      *              *-------------------------------------------------*
           perform   esp-930              thru esp-959                .
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
           if        w-aux-rec-rig-nli    >    6
                     subtract  6          from w-aux-rec-rig-nli
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
           if        w-esp-rig-fir-pro    =    spaces
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
           move      w-esp-rig-fir-pro    to   w-wrk-fnd-pro-alf      .
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
           if        w-aux-rec-rig-nli    =    15
                     go to esp-590
           else      go to esp-550.
       esp-590.
      *                  *---------------------------------------------*
      *                  * Visualizzazione pagina video contenente     *
      *                  * il record attualmente trattato              *
      *                  *---------------------------------------------*
           perform   esp-930              thru esp-959                .
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
           multiply  6                    by   w-aux-rec-rig-c01      .
           subtract  5                    from w-aux-rec-rig-c01      .
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
      *                  *---------------------------------------------*
      *                  * Normalizzazione deviatore per Expand        *
      *                  *---------------------------------------------*
           move      0                    to   w-cnt-swc-esp-gen      .
      *                  *---------------------------------------------*
      *                  * Restore                                     *
      *                  *---------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     esp-800.
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
           if        w-esp-rig-fir-pro    not  = spaces
                     add   1              to   w-cnt-swc-fnd-pro      .
      *                  *---------------------------------------------*
      *                  * Normalizzazione comodo di accetazione       *
      *                  *---------------------------------------------*
           move      spaces               to   w-wrk-fnd-pro-alf      .
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
           perform   esp-930              thru esp-959                .
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
       esp-800.
      *              *=================================================*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     esp-999.
       esp-930.
      *              *=================================================*
      *              * Visualizzazione pagina video contenente il re-  *
      *              * cord attualmente trattato                       *
      *              *-------------------------------------------------*
           move      w-aux-rec-rig-c01    to   w-aux-rec-rig-c02      .
           add       5                    to   w-aux-rec-rig-c02      .
           divide    6                    into w-aux-rec-rig-c02      .
           move      w-aux-rec-rig-c02    to   w-aux-rec-rig-cpa      .
           subtract  1                    from w-aux-rec-rig-c02      .
           multiply  6                    by   w-aux-rec-rig-c02      .
           add       1                    to   w-aux-rec-rig-c02      .
           add       5
                     w-aux-rec-rig-c02  giving w-aux-rec-rig-c03      .
           move      w-aux-rec-rig-c03    to   w-aux-rec-rig-c04      .
           if        w-aux-rec-rig-c03    >    w-aux-rec-rig-crb
                     move  w-aux-rec-rig-crb
                                          to   w-aux-rec-rig-c03      .
      *                  *---------------------------------------------*
      *                  * Posizionamento iniziale                     *
      *                  *---------------------------------------------*
           move      10                   to   w-aux-rec-rig-c05      .
       esp-940.
      *                  *---------------------------------------------*
      *                  * Preparazione riga da visualizzare           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Lettura record [fir]                    *
      *                      *-----------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      w-aux-rec-rig-prt
                    (w-aux-rec-rig-c02)   to   rf-fir-num-prt         .
           move      w-aux-rec-rig-prg
                    (w-aux-rec-rig-c02)   to   rf-fir-num-prg         .
           move      "pgm/fat/fls/ioc/obj/ioffir"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fir                 .
      *                      *-----------------------------------------*
      *                      * Editing Numero d'ordine riga            *
      *                      *-----------------------------------------*
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
      *                      *-----------------------------------------*
      *                      * Separatore 1                            *
      *                      *-----------------------------------------*
           move      "|"                  to   w-lin-imm-seg-tr1      .
      *                      *-----------------------------------------*
      *                      * Scomposizione tipo riga                 *
      *                      *-----------------------------------------*
           move      rf-fir-tip-rig       to   w-ctl-tip-rig-tri      .
      *                      *-----------------------------------------*
      *                      * Codice prodotto                         *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Assemblaggio                        *
      *                          *-------------------------------------*
           move      14                   to   w-all-str-lun          .
           move      04                   to   w-all-str-num          .
      *
           if        w-ctl-tip-rig-chr (1)
                                          =    "C"
                     move  "(Commento"    to   w-all-str-cat (1)
                     move  w-ctl-tip-rig-aoc
                                          to   w-all-str-cat (3)
                     move  ")"            to   w-all-str-cat (4)
           else if   w-ctl-tip-rig-chr (1)
                                          =    "A"
                     move  "(Addebito"    to   w-all-str-cat (1)
                     move  w-ctl-tip-rig-aoc
                                          to   w-all-str-cat (3)
                     move  ")"            to   w-all-str-cat (4)
           else      move  spaces         to   w-all-str-cat (1)
                     move  spaces         to   w-all-str-cat (2)
                     move  rf-fir-alf-pro to   w-all-str-cat (3)
                     move  spaces         to   w-all-str-cat (4)      .
      *
           if        w-ctl-tip-rig-aoc    =    spaces
                     move  spaces         to   w-all-str-cat (2)
           else      move  ":"            to   w-all-str-cat (2)      .
      *
           perform   all-str-cat-000      thru all-str-cat-999        .
      *                          *-------------------------------------*
      *                          * Allineamento a destra per commenti  *
      *                          * o addebiti                          *
      *                          *-------------------------------------*
           if        w-ctl-tip-rig-chr (1)
                                          =    "C" or
                     w-ctl-tip-rig-chr (1)
                                          =    "A"
                     perform   all-str-adx-000
                                          thru all-str-adx-999        .
      *                          *-------------------------------------*
      *                          * In campo di destinazione            *
      *                          *-------------------------------------*
           move      w-all-str-alf        to   w-lin-imm-alf-rig      .
      *                      *-----------------------------------------*
      *                      * Separatore 2                            *
      *                      *-----------------------------------------*
           move      "|"                  to   w-lin-imm-seg-tr2      .
      *                      *-----------------------------------------*
      *                      * Descrizione per la riga                 *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Lettura preliminare file [fix]      *
      *                          *-------------------------------------*
           move      rf-fir-num-prt       to   w-let-arc-fix-prt      .
           move      rf-fir-num-prg       to   w-let-arc-fix-prg      .
           move      01                   to   w-let-arc-fix-trc      .
           perform   let-arc-fix-000      thru let-arc-fix-999        .
      *
           if        w-let-arc-fix-flg    not  = spaces
                     go to esp-951.
      *
           move      w-let-arc-fix-des    to   w-lin-imm-des-rig      .
      *
           go to     esp-956.
       esp-951.
      *                          *-------------------------------------*
      *                          * Deviazione in funzione del tipo di  *
      *                          * estensione : nel caso di estensione *
      *                          * tipo 2 si utilizza la descrizione   *
      *                          * in riga comunque                    *
      *                          *-------------------------------------*
           if        rf-fir-des-ext       =    0
                     go to esp-952
           else if   rf-fir-des-ext       =    1
                     go to esp-953
           else if   rf-fir-des-ext       =    2
                     go to esp-954.
       esp-952.
      *                          *-------------------------------------*
      *                          * Se descrizione in riga              *
      *                          *-------------------------------------*
           move      rf-fir-des-rig       to   w-lin-imm-des-rig      .
           go to     esp-956.
       esp-953.
      *                          *-------------------------------------*
      *                          * Se estensione nel file [fix]        *
      *                          *-------------------------------------*
           go to     esp-956.
       esp-954.
      *                          *-------------------------------------*
      *                          * Se estensione nel file [pdx]        *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Lettura record [dcp]            *
      *                              *---------------------------------*
           move      rf-fir-num-pro       to   w-let-dcp-pdx-cod      .
           move      "C"                  to   w-let-dcp-pdx-tar      .
           move      rf-fir-cod-cli       to   w-let-dcp-pdx-arc      .
           move      rf-fir-cod-lng       to   w-let-dcp-pdx-lng      .
           perform   let-dcp-pdx-000      thru let-dcp-pdx-999        .
           move      w-let-dcp-pdx-des    to   w-lin-imm-des-rig      .
           go to     esp-956.
       esp-956.
      *                      *-----------------------------------------*
      *                      * Separatore 3                            *
      *                      *-----------------------------------------*
           move      "|"                  to   w-lin-imm-seg-tr3      .
      *                      *-----------------------------------------*
      *                      * Editing quantita'                       *
      *                      *-----------------------------------------*
           move      "ED"                 to   w-edt-qta-inc-ope      .
           move      rf-fir-qta-ven       to   w-edt-qta-inc-qta      .
           move      rf-fir-dec-qta       to   w-edt-qta-inc-dec      .
           move      "S"                  to   w-edt-qta-inc-sgn      .
           move      11                   to   w-edt-qta-inc-car      .
           perform   edt-qta-inc-000      thru edt-qta-inc-999        .
           move      w-edt-qta-inc-edt    to   w-lin-imm-qta-rig      .
      *                      *-----------------------------------------*
      *                      * Separatore 4                            *
      *                      *-----------------------------------------*
           move      "|"                  to   w-lin-imm-seg-tr4      .
      *                      *-----------------------------------------*
      *                      * Editing importo                         *
      *                      *-----------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           if        rf-fir-imp-rig       >    999999999 or
                     rf-fir-imp-rig       <   -999999999
                     move  11             to   v-car
           else      move  09             to   v-car                  .
           move      rf-fir-dec-vpf       to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           if        rf-fir-imp-rig       >    999999999 or
                     rf-fir-imp-rig       <   -999999999
                     move  "B"            to   v-edm
           else      move  "GB"           to   v-edm                  .
           move      rf-fir-imp-rig       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-lin-imm-imp-rig      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione riga                        *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      78                   to   v-car                  .
           move      w-aux-rec-rig-c05    to   v-lin                  .
           move      02                   to   v-pos                  .
           move      w-lin-imm-dsp        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Incremento contatori                        *
      *                  *---------------------------------------------*
           add       1                    to   w-aux-rec-rig-c02      .
           add       1                    to   w-aux-rec-rig-c05      .
           if        w-aux-rec-rig-c02    not  > w-aux-rec-rig-c03
                     go to esp-940.
       esp-957.
           if        w-aux-rec-rig-c02    >    w-aux-rec-rig-c04
                     go to esp-958.
           if        w-aux-rec-rig-crb    not  > 6
                     go to esp-958.
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
           go to     esp-957.
       esp-958.
      *              *-------------------------------------------------*
      *              * Literal 'pagina'                                *
      *              *-------------------------------------------------*
           perform   esp-ldp-000          thru esp-ldp-999            .
       esp-959.
           exit.
       esp-999.
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
                     move  rf-fir-num-prg to   w-aux-rec-rig-kpg
                                              (w-aux-rec-rig-crb)     .
      *              *-------------------------------------------------*
      *              * Codice alfanumerico prodotto                    *
      *              *-------------------------------------------------*
           if        w-aux-rec-rig-tor    =    "P"
                     move  rf-fir-alf-pro to   w-aux-rec-rig-kal
                                              (w-aux-rec-rig-crb)     .
      *              *-------------------------------------------------*
      *              * Descrizione prodotto                            *
      *              *-------------------------------------------------*
           if        w-aux-rec-rig-tor    =    "D"
                     move  rf-fir-des-rig to   w-aux-rec-rig-kde
                                              (w-aux-rec-rig-crb)     .
      *              *-------------------------------------------------*
      *              * Quantita' prodotto                              *
      *              *-------------------------------------------------*
           if        w-aux-rec-rig-tor    =    "Q"
                     move  rf-fir-qta-ven to   w-aux-rec-rig-kqt
                                              (w-aux-rec-rig-crb)     .
      *              *-------------------------------------------------*
      *              * Importo in riga                                 *
      *              *-------------------------------------------------*
           if        w-aux-rec-rig-tor    =    "I"
                     move  rf-fir-imp-rig to   w-aux-rec-rig-kim
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
      *    * Routine di Expand su righe fattura                        *
      *    *                                                           *
      *    * Subroutine per la costruzione del Box                     *
      *    *-----------------------------------------------------------*
       esp-box-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione delle personalizzazioni  *
      *              *-------------------------------------------------*
           if        w-prs-tip-esp        =    00
                     go to esp-box-100
           else if   w-prs-tip-esp        =    01
                     go to esp-box-200
           else      go to esp-box-100.
       esp-box-100.
      *              *-------------------------------------------------*
      *              * Box per espansione tipo '00'                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Box                                         *
      *                  *---------------------------------------------*
           move      "BX"                 to   v-ope                  .
           move      07                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      18                   to   v-lto                  .
           move      80                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Fincatura superiore righe                   *
      *                  *---------------------------------------------*
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
      *                  *---------------------------------------------*
      *                  * Sottolineatura fincatura superiore          *
      *                  *---------------------------------------------*
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
      *                  *---------------------------------------------*
      *                  * Griglie                                     *
      *                  *---------------------------------------------*
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
      *
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
      *
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
      *
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
      *
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
      *
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
      *                  *---------------------------------------------*
      *                  * Sottolineatura di chiusura                  *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      78                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      02                   to   v-pos                  .
           move      "---+--------------+-------------------------------
      -              "-+-----------+--------------"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Estremi 'Documento'                         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Literal documento                       *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      38                   to   v-pos                  .
           move      "Documento :"        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Codice tipo documento                   *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      50                   to   v-pos                  .
           move      w-esp-rig-fir-tmo    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Literal 'nr.'                           *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      57                   to   v-pos                  .
           move      "nr."                to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Numero documento                        *
      *                      *-----------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      17                   to   v-lin                  .
           move      60                   to   v-pos                  .
           move      w-esp-rig-fir-num    to   w-wrk-num-doc          .
           move      w-wrk-ndo-prg        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Literal 'del'                           *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      67                   to   v-pos                  .
           move      "del"                to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Data documento                          *
      *                      *-----------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      17                   to   v-lin                  .
           move      71                   to   v-pos                  .
           move      w-esp-rig-fir-dat    to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       esp-box-190.
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     esp-box-999.
       esp-box-200.
      *              *-------------------------------------------------*
      *              * Box per espansione tipo '01'                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura record di testata                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazione record [fit]            *
      *                      *-----------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/fat/fls/ioc/obj/ioffit"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fit                 .
      *                      *-----------------------------------------*
      *                      * Lettura record [fit]                    *
      *                      *-----------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      w-esp-rig-fir-prt    to   rf-fit-num-prt         .
           move      "pgm/fat/fls/ioc/obj/ioffit"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fit                 .
       esp-box-220.
      *                  *---------------------------------------------*
      *                  * Box                                         *
      *                  *---------------------------------------------*
           move      "BX"                 to   v-ope                  .
           move      03                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      22                   to   v-lto                  .
           move      80                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Testata                                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Documento, composizione stringa         *
      *                      *-----------------------------------------*
           move      57                   to   w-all-str-lun          .
           move      04                   to   w-all-str-num          .
           if        rf-fit-tip-doc       =    01 and
                     rf-fit-org-doc       =    02
                     move  "Fattura accompagnatoria"
                                          to   w-all-str-cat (1)
           else if   rf-fit-tip-doc       =    01
                     move  "Fattura          "
                                          to   w-all-str-cat (1)
           else if   rf-fit-tip-doc       =    02
                     move  "Nota di addebito "
                                          to   w-all-str-cat (1)
           else if   rf-fit-tip-doc       =    03
                     move  "Nota di accredito"
                                          to   w-all-str-cat (1)
           else if   rf-fit-tip-doc       =    04
                     move  "Fattura Pro-Forma"
                                          to   w-all-str-cat (1)
           else      move  "(?)              "
                                          to   w-all-str-cat (1)      .
           move      "-"                  to   w-all-str-cat (2)      .
           move      rf-fit-cod-tmo       to   w-all-str-cat (3)      .
           move      "- nr."              to   w-all-str-cat (4)      .
           perform   all-str-csb-000      thru all-str-csb-999        .
      *                      *-----------------------------------------*
      *                      * Documento, editing numero               *
      *                      *-----------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      rf-fit-num-doc       to   w-wrk-num-doc          .
           move      w-wrk-ndo-prg        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Documento, composizione stringa - segue *
      *                      *-----------------------------------------*
           move      57                   to   w-all-str-lun          .
           move      03                   to   w-all-str-num          .
           move      w-all-str-alf        to   w-all-str-cat (1)      .
           move      v-edt                to   w-all-str-cat (2)      .
           move      "del"                to   w-all-str-cat (3)      .
           perform   all-str-csb-000      thru all-str-csb-999        .
      *                      *-----------------------------------------*
      *                      * Documento, editing data                 *
      *                      *-----------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      rf-fit-dat-doc       to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Documento, composizione stringa - segue *
      *                      *-----------------------------------------*
           move      57                   to   w-all-str-lun          .
           move      02                   to   w-all-str-num          .
           move      w-all-str-alf        to   w-all-str-cat (1)      .
           move      v-edt                to   w-all-str-cat (2)      .
           perform   all-str-csb-000      thru all-str-csb-999        .
      *                      *-----------------------------------------*
      *                      * Documento, visualizzazione literal      *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      57                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      w-all-str-alf        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Cliente                                 *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      18                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      "Cliente          :" to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Cliente, codice                         *
      *                      *-----------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      05                   to   v-lin                  .
           move      22                   to   v-pos                  .
           move      rf-fit-cod-cli       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Cliente, codice dipendenza              *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      31                   to   v-pos                  .
           move      rf-fit-dpz-cli       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Cliente, ragione sociale                *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Lettura                             *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Normalizzazione record [dcc]    *
      *                              *---------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofdcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
      *                              *---------------------------------*
      *                              * Lettura record [dcc]            *
      *                              *---------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCLI    "         to   f-key                  .
           move      rf-fit-cod-cli       to   rf-dcc-cod-cli         .
           move      rf-fit-dpz-cli       to   rf-dcc-dpz-cli         .
           move      "pgm/dcc/fls/ioc/obj/iofdcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
      *                              *---------------------------------*
      *                              * Se record [dcc] non trovato     *
      *                              *---------------------------------*
           if        f-sts                not  = e-not-err
                     move  all "."        to   rf-dcc-rag-soc         .
      *                          *-------------------------------------*
      *                          * Visualizzazione                     *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      37                   to   v-pos                  .
           move      rf-dcc-rag-soc       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Agente                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      18                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      "Agente           :" to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Agente, codice                          *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test se presente                    *
      *                          *-------------------------------------*
           if        rf-fit-cod-age       =    zero
                     go to esp-box-230.
      *                          *-------------------------------------*
      *                          * Visualizzazione                     *
      *                          *-------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      06                   to   v-lin                  .
           move      22                   to   v-pos                  .
           move      rf-fit-cod-age       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Agente, nominativo                      *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Lettura                             *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Normalizzazione record [age]    *
      *                              *---------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/age/fls/ioc/obj/iofage"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-age                 .
      *                              *---------------------------------*
      *                              * Lettura record [age]            *
      *                              *---------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODFOP    "         to   f-key                  .
           move      rf-fit-cod-age       to   rf-age-cod-age         .
           move      "pgm/age/fls/ioc/obj/iofage"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-age                 .
      *                              *---------------------------------*
      *                              * Se record [age] non trovato     *
      *                              *---------------------------------*
           if        f-sts                not  = e-not-err
                     move  all "."        to   rf-age-nom-age         .
      *                          *-------------------------------------*
      *                          * Visualizzazione                     *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      37                   to   v-pos                  .
           move      rf-age-nom-age       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       esp-box-230.
      *                      *-----------------------------------------*
      *                      * Tipo vendita per provvigioni            *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Si evidenzia solo la vendita 'indi- *
      *                          * retta' mentre in tutti gli altri    *
      *                          * casi si va oltre                    *
      *                          *-------------------------------------*
           if        rf-fit-tip-vpa       not  = 02
                     go to esp-box-235.
      *                          *-------------------------------------*
      *                          * Visualizzazione                     *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      68                   to   v-pos                  .
           move      "(indiretta)"        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       esp-box-235.
      *                      *-----------------------------------------*
      *                      * Forma di pagamento                      *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      18                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      "Pagamento        :" to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Forma di pagamento, codice              *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test se presente                    *
      *                          *-------------------------------------*
           if        rf-fit-cod-fop       =    zero
                     go to esp-box-240.
      *                          *-------------------------------------*
      *                          * Visualizzazione                     *
      *                          *-------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      07                   to   v-lin                  .
           move      22                   to   v-pos                  .
           move      rf-fit-cod-fop       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Forma di pagamento, descrizione         *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Lettura                             *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Normalizzazione record [zfp]    *
      *                              *---------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofzfp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zfp                 .
      *                              *---------------------------------*
      *                              * Lettura record [zfp]            *
      *                              *---------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODFOP    "         to   f-key                  .
           move      rf-fit-cod-fop       to   rf-zfp-cod-fop         .
           move      "pgm/dcc/fls/ioc/obj/iofzfp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zfp                 .
      *                              *---------------------------------*
      *                              * Se record [zfp] non trovato     *
      *                              *---------------------------------*
           if        f-sts                not  = e-not-err
                     move  all "."        to   rf-zfp-des-fop         .
      *                          *-------------------------------------*
      *                          * Visualizzazione                     *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      37                   to   v-pos                  .
           move      rf-zfp-des-fop       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       esp-box-240.
      *                  *---------------------------------------------*
      *                  * Fincatura superiore righe                   *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      78                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      02                   to   v-pos                  .
           move      "---+--------------+-------------------------------
      -              "-+-----------+--------------"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Fincatura righe                             *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      78                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      02                   to   v-pos                  .
           move      "N.R|    Codice    |           Descrizione         
      -              " | Quantita' |    Importo   "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Griglie                                     *
      *                  *---------------------------------------------*
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
      *
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
      *
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
      *
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
      *
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
      *
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
      *                  *---------------------------------------------*
      *                  * Sottolineatura di chiusura                  *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      78                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      02                   to   v-pos                  .
           move      "---+--------------+-------------------------------
      -              "-+-----------+--------------"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       esp-box-260.
      *                  *---------------------------------------------*
      *                  * Piede                                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Piede, griglie                          *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      "|"                  to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      "|"                  to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      "|"                  to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      "|"                  to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      21                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      "|"                  to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Sconto incondizionato                   *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      18                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      "Sconto incondiz. :" to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Sconto incondizionato, valore           *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      rf-fir-dec-vpf       to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "<BG"                to   v-edm                  .
           move      17                   to   v-lin                  .
           move      22                   to   v-pos                  .
           move      rf-fit-tot-scc       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Sconto incondizionato, %                *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      01                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "BGD"                to   v-edm                  .
           move      17                   to   v-lin                  .
           move      37                   to   v-pos                  .
           move      rf-fit-per-scc       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Sconto pagamento                        *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      18                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      "Sconto pagamento :" to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Sconto pagamento, valore                *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      rf-fir-dec-vpf       to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "<BG"                to   v-edm                  .
           move      18                   to   v-lin                  .
           move      22                   to   v-pos                  .
           move      rf-fit-tot-scp       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Sconto pagamento, %                     *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      01                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "BGD"                to   v-edm                  .
           move      18                   to   v-lin                  .
           move      37                   to   v-pos                  .
           move      rf-fit-per-scp       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Totale documento                        *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      21                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      51                   to   v-pos                  .
           move      "Totale     :"       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Totale documento, determinazione        *
      *                      *-----------------------------------------*
           perform   det-tot-doc-000      thru det-tot-doc-999        .
      *                      *-----------------------------------------*
      *                      * Totale documento, valore                *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      rf-fit-dec-vpf       to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "BG"                 to   v-edm                  .
           move      17                   to   v-lin                  .
           move      63                   to   v-pos                  .
           move      w-det-tot-doc-tot    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Castelletto spese                       *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Importi spese, prompts              *
      *                          *-------------------------------------*
           perform   pmt-spe-imp-000      thru pmt-spe-imp-999        .
      *                          *-------------------------------------*
      *                          * Si/No addebito spese                *
      *                          *-------------------------------------*
           perform   vis-spe-snx-000      thru vis-spe-snx-999        .
      *                          *-------------------------------------*
      *                          * Importi spese                       *
      *                          *-------------------------------------*
           perform   vis-spe-imp-000      thru vis-spe-imp-999        .
      *                          *-------------------------------------*
      *                          * Percentuali spese                   *
      *                          *-------------------------------------*
           perform   vis-spe-per-000      thru vis-spe-per-999        .
       esp-box-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     esp-box-999.
       esp-box-999.
           exit.

      *    *===========================================================*
      *    * Routine di Expand su righe fattura                        *
      *    *                                                           *
      *    * Subroutine per il literal di pagina                       *
      *    *-----------------------------------------------------------*
       esp-ldp-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione delle personalizzazioni  *
      *              *-------------------------------------------------*
           if        w-prs-tip-esp        =    00
                     go to esp-ldp-100
           else if   w-prs-tip-esp        =    01
                     go to esp-ldp-200
           else      go to esp-ldp-100.
       esp-ldp-100.
      *              *-------------------------------------------------*
      *              * Literal per espansione tipo '00'                *
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
           move      17                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      w-aux-rec-rig-ltp    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     esp-ldp-900.
       esp-ldp-200.
      *              *-------------------------------------------------*
      *              * Literal per espansione tipo '01'                *
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
           string    "[Pag. "  delimited by   size
                     w-aux-rec-rig-lt1
                                delimited by   spaces
                     " di "     delimited by   size
                     w-aux-rec-rig-lt2
                                delimited by   spaces
                     "]"        delimited by   size
                                          into w-aux-rec-rig-ltp      .
      *                  *---------------------------------------------*
      *                  * Allineamento a destra                       *
      *                  *---------------------------------------------*
           move      17                   to   w-all-str-lun          .
           move      w-aux-rec-rig-ltp    to   w-all-str-alf          .
           perform   all-str-adx-000      thru all-str-adx-999        .
      *                  *---------------------------------------------*
      *                  * Visualizzazione stringa composta            *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      17                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      62                   to   v-pos                  .
           move      w-all-str-alf        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     esp-ldp-900.
       esp-ldp-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     esp-ldp-999.
       esp-ldp-999.
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
           move      06                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      16                   to   v-lto                  .
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
                    (w-aux-rec-rig-c01)   to   rf-fir-num-prt         .
           move      w-aux-rec-rig-prg
                    (w-aux-rec-rig-c01)   to   rf-fir-num-prg         .
           move      "pgm/fat/fls/ioc/obj/ioffir"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fir                 .
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
           move      07                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      "Tipo riga         :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Codice prodotto                             *
      *                  *---------------------------------------------*
           move      rf-fir-tip-rig       to   w-ctl-tip-rig-tri      .
           perform   pmt-cod-pro-000      thru pmt-cod-pro-999        .
      *                  *---------------------------------------------*
      *                  * Descrizione                                 *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      19                   to   v-car                  .
           move      09                   to   v-lin                  .
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
           move      10                   to   v-lin                  .
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
           move      11                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      "Prezzo            :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * % sconto                                    *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      19                   to   v-car                  .
           move      12                   to   v-lin                  .
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
           move      19                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      "Importo           :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Codice iva                                  *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      19                   to   v-car                  .
           move      14                   to   v-lin                  .
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
           move      15                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      "Contropartita     :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Provvigioni in riga                         *
      *                  *---------------------------------------------*
           perform   pmt-ppv-rig-000      thru pmt-ppv-rig-999        .
       esp-rig-300.
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
           move      07                   to   v-lin                  .
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
           move      07                   to   v-lin                  .
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
       esp-rig-480.
      *                  *---------------------------------------------*
      *                  * Descrizione prodotto                        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Lettura preliminare file [fix]          *
      *                      *-----------------------------------------*
           move      rf-fir-num-prt       to   w-let-arc-fix-prt      .
           move      rf-fir-num-prg       to   w-let-arc-fix-prg      .
           move      01                   to   w-let-arc-fix-trc      .
           perform   let-arc-fix-000      thru let-arc-fix-999        .
      *
           if        w-let-arc-fix-flg    not  = spaces
                     go to esp-rig-481.
      *
           go to     esp-rig-483.
       esp-rig-481.
      *                      *-----------------------------------------*
      *                      * Deviazione in funzione del tipo di      *
      *                      * estensione : nel caso di estensione     *
      *                      * tipo 2 si utilizza la descrizione       *
      *                      * in riga comunque                        *
      *                      *-----------------------------------------*
           if        rf-fir-des-ext       =    0
                     go to esp-rig-482
           else if   rf-fir-des-ext       =    1
                     go to esp-rig-483
           else if   rf-fir-des-ext       =    2
                     go to esp-rig-484.
       esp-rig-482.
      *                          *-------------------------------------*
      *                          * Se descrizione in riga              *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      23                   to   v-pos                  .
           move      rf-fir-des-rig       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                          *-------------------------------------*
      *                          * A trattamento quantita'             *
      *                          *-------------------------------------*
           go to     esp-rig-500.
       esp-rig-483.
      *                          *-------------------------------------*
      *                          * Se estensione nel file [fix]        *
      *                          *-------------------------------------*
      *                          *-------------------------------------*
      *                          * Visualizzazione della I riga        *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      23                   to   v-pos                  .
           move      w-let-arc-fix-drg (1)
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
           move      rf-fir-num-pro       to   w-let-dcp-pdx-cod      .
           move      "C"                  to   w-let-dcp-pdx-tar      .
           move      rf-fir-cod-cli       to   w-let-dcp-pdx-arc      .
           move      rf-fir-cod-lng       to   w-let-dcp-pdx-lng      .
           perform   let-dcp-pdx-000      thru let-dcp-pdx-999        .
      *                          *-------------------------------------*
      *                          * Visualizzazione della I riga        *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      23                   to   v-pos                  .
           move      w-let-dcp-pdx-drg (1)
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
       esp-rig-500.
      *                  *---------------------------------------------*
      *                  * Esposizione o meno delle barrette che stan- *
      *                  * no ad indicare la presenza di ulteriori ri- *
      *                  * ghe di descrizione                          *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      22                   to   v-pos                  .
           if        w-wrk-snx-ext        =    spaces
                     move  spaces         to   v-alf
           else      move  "|"            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      63                   to   v-pos                  .
           if        w-wrk-snx-ext        =    spaces
                     move  spaces         to   v-alf
           else      move  "|"            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Quantita'                                   *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      rf-fir-dec-qta       to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "<BD"                to   v-edm                  .
           move      10                   to   v-lin                  .
           move      23                   to   v-pos                  .
           move      rf-fir-qta-ven       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       esp-rig-520.
      *                  *---------------------------------------------*
      *                  * Prezzo                                      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      09                   to   v-car                  .
           move      rf-fir-dec-vpp       to   v-dec                  .
           add       rf-fir-dec-prz       to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BG"                to   v-edm                  .
           move      11                   to   v-lin                  .
           move      23                   to   v-pos                  .
           move      rf-fir-prz-ven       to   v-num                  .
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
           move      12                   to   v-lin                  .
           move      w-wrk-ctr-001        to   v-pos                  .
           multiply  05                   by   v-pos                  .
           add       18                   to   v-pos                  .
           move      rf-fir-per-scr 
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
           move      rf-fir-dec-vpf       to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BG"                to   v-edm                  .
           move      13                   to   v-lin                  .
           move      23                   to   v-pos                  .
           move      rf-fir-imp-rig       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       esp-rig-580.
      *                  *---------------------------------------------*
      *                  * Codice iva                                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Editing                                 *
      *                      *-----------------------------------------*
           move      rf-fir-cod-iva       to   w-edt-iva-cod          .
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
           move      14                   to   v-lin                  .
           move      23                   to   v-pos                  .
           move      w-edt-iva-cie        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       esp-rig-590.
      *                  *---------------------------------------------*
      *                  * Contropartita                               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Editing con appoggio a sinistra         *
      *                      *-----------------------------------------*
           move      w-prs-liv-pdc        to   w-edt-cod-pdc-liv      .
           move      rf-fir-ctp-ven       to   w-edt-cod-pdc-cod      .
           move      "B"                  to   w-edt-cod-pdc-edm      .
           perform   edt-pdc-asx-000      thru edt-pdc-asx-999        .
      *                      *-----------------------------------------*
      *                      * Visualizzazione                         *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      09                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      23                   to   v-pos                  .
           move      w-edt-cod-pdc-edt    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       esp-rig-600.
      *                  *---------------------------------------------*
      *                  * Provvigioni in riga                         *
      *                  *---------------------------------------------*
           perform   vis-ppv-rig-000      thru vis-ppv-rig-999        .
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
           move      15                   to   v-lin                  .
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
      *                  * Normalizzazione deviatore                   *
      *                  *---------------------------------------------*
           move      0                    to   w-cnt-swc-esp-rig      .
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
      *              *-------------------------------------------------*
      *              * Lettura preliminare record [fix]                *
      *              *-------------------------------------------------*
           move      rf-fir-num-prt       to   w-let-arc-fix-prt      .
           move      rf-fir-num-prg       to   w-let-arc-fix-prg      .
           move      01                   to   w-let-arc-fix-trc      .
           perform   let-arc-fix-000      thru let-arc-fix-999        .
      *
           if        w-let-arc-fix-flg    not  = spaces
                     go to esp-rig-des-010.
      *
           move      w-let-arc-fix-des    to   w-wrk-des-rig          .
      *
           go to     esp-rig-des-200.
       esp-rig-des-010.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo estensione      *
      *              *-------------------------------------------------*
           if        rf-fir-des-ext       =    0
                     go to esp-rig-des-800
           else if   rf-fir-des-ext       =    1
                     go to esp-rig-des-020
           else if   rf-fir-des-ext       =    2
                     go to esp-rig-des-040.
       esp-rig-des-020.
      *              *-------------------------------------------------*
      *              * Lettura record [fix]                            *
      *              *-------------------------------------------------*
           go to     esp-rig-des-200.
       esp-rig-des-040.
      *              *-------------------------------------------------*
      *              * Lettura record [pdx]                            *
      *              *-------------------------------------------------*
           move      rf-fir-num-pro       to   w-let-dcp-pdx-cod      .
           move      "C"                  to   w-let-dcp-pdx-tar      .
           move      rf-fir-cod-cli       to   w-let-dcp-pdx-arc      .
           move      rf-fir-cod-lng       to   w-let-dcp-pdx-lng      .
           perform   let-dcp-pdx-000      thru let-dcp-pdx-999        .
           move      w-let-dcp-pdx-des    to   w-wrk-des-rig          .
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
      *              * Costruzione box di Expand per la sola descri-   *
      *              * zione della riga                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Le dimensioni del box dipendono dal numero  *
      *                  * di righe da cui e' composta la descrizione  *
      *                  *---------------------------------------------*
           move      "BX"                 to   v-ope                  .
           move      08                   to   v-lin                  .
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
           move      08                   to   v-lin                  .
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
           move      09                   to   v-lin                  .
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
           move      08                   to   v-lin                  .
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
           move      08                   to   v-lin                  .
           move      23                   to   v-pos                  .
      *
           if        w-ctl-tip-rig-chr (1)
                                          =    "C" or
                     w-ctl-tip-rig-chr (1)
                                          =    "A"
                     move  w-ctl-tip-rig-aoc
                                          to   v-alf
           else      move  rf-fir-alf-pro to   v-alf                  .
      *
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-pro-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per riga corpo di accettazione :  *
      *    * Percentuali di provvigione in riga                        *
      *    *-----------------------------------------------------------*
       pmt-ppv-rig-000.
      *              *-------------------------------------------------*
      *              * Test se da visualizzare                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su personalizzazione gestione agenti   *
      *                  *---------------------------------------------*
           if        w-prs-age-snx        not  = "S"
                     go to pmt-ppv-rig-900.
       pmt-ppv-rig-100.
      *              *-------------------------------------------------*
      *              * Visualizzazione prompts                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Fincatura                                   *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      52                   to   v-pos                  .
           move      "--------Provvigioni---------"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Si/no provvigioni                           *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      12                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      52                   to   v-pos                  .
           move      "Si/No      :"       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * % provvigioni                               *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      12                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      52                   to   v-pos                  .
           move      "Percentuali:"       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * A forfait                                   *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      12                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      52                   to   v-pos                  .
           move      "A forfait  :"       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-ppv-rig-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     pmt-ppv-rig-999.
       pmt-ppv-rig-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione valori per le provvigioni in riga         *
      *    * Percentuali di provvigione in riga                        *
      *    *-----------------------------------------------------------*
       vis-ppv-rig-000.
      *              *-------------------------------------------------*
      *              * Test se da visualizzare                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su personalizzazione gestione agenti   *
      *                  *---------------------------------------------*
           if        w-prs-age-snx        not  = "S"
                     go to vis-ppv-rig-900.
       vis-ppv-rig-100.
      *              *-------------------------------------------------*
      *              * Visualizzazione campi                           *
      *              *-------------------------------------------------*
       vis-ppv-rig-120.
      *                  *---------------------------------------------*
      *                  * Si/no provvigioni                           *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-fsp-rig-lun    to   v-car                  .
           move      w-exp-fsp-rig-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      12                   to   v-lin                  .
           move      65                   to   v-pos                  .
           move      w-exp-fsp-rig-tbl    to   v-txt                  .
           move      rf-fir-fsp-rig       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-ppv-rig-200.
      *                  *---------------------------------------------*
      *                  * % provvigioni                               *
      *                  *---------------------------------------------*
           move      zero                 to   w-wrk-ctr-001          .
       vis-ppv-rig-210.
           add       1                    to   w-wrk-ctr-001          .
           if        w-wrk-ctr-001        >    3
                     go to vis-ppv-rig-300.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      01                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BD"                to   v-edm                  .
           move      13                   to   v-lin                  .
           move      w-wrk-ctr-001        to   v-pos                  .
           multiply  05                   by   v-pos                  .
           add       60                   to   v-pos                  .
           move      rf-fir-ppv-rig 
                    (w-wrk-ctr-001)       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           go to     vis-ppv-rig-210.
       vis-ppv-rig-300.
      *                  *---------------------------------------------*
      *                  * A forfait                                   *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "<BG"                to   v-edm                  .
           move      14                   to   v-lin                  .
           move      65                   to   v-pos                  .
           move      rf-fir-pvf-rig       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-ppv-rig-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     vis-ppv-rig-999.
       vis-ppv-rig-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [fix]                         *
      *    *-----------------------------------------------------------*
       let-arc-fix-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-fix-flg      .
      *              *-------------------------------------------------*
      *              * Test se codici nulli                            *
      *              *-------------------------------------------------*
           if        w-let-arc-fix-prt    =    zero   or
                     w-let-arc-fix-prg    =    zero   or
                     w-let-arc-fix-trc    =    zero
                     go to let-arc-fix-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      w-let-arc-fix-prt    to   rf-fix-num-prt         .
           move      w-let-arc-fix-prg    to   rf-fix-num-prg         .
           move      w-let-arc-fix-trc    to   rf-fix-tip-rec         .
           move      "pgm/fat/fls/ioc/obj/ioffix"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fix                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-fix-400.
       let-arc-fix-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-fix-des-400       to   w-let-arc-fix-des      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-fix-999.
       let-arc-fix-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-fix-flg      .
           move      spaces               to   w-let-arc-fix-des      .
           move      all   "."            to   w-let-arc-fix-drg (1)  .
           go to     let-arc-fix-999.
       let-arc-fix-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-fix-des      .
       let-arc-fix-999.
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
      *    * Lettura personalizzazione : Si/No gestione agenti attiva  *
      *    *-----------------------------------------------------------*
       prs-age-snx-000.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione                       *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/age[snx]"       to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-alf          to   w-prs-age-snx
           else      move  spaces         to   w-prs-age-snx          .
      *              *-------------------------------------------------*
      *              * Normalizzazione personalizzazione               *
      *              *-------------------------------------------------*
           if        w-prs-age-snx        not  = "S" and
                     w-prs-age-snx        not  = "N"
                     move  "N"            to   w-prs-age-snx          .
       prs-age-snx-999.
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
           move      "pgm/fat/mov/fat300[tvn-rgd]"
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
                     w-prs-tvn-rgd        =    02
                     go to prs-tvn-rgd-999.
           move      00                   to   w-prs-tvn-rgd          .
       prs-tvn-rgd-999.
           exit.

      *    *===========================================================*
      *    * Lettura della personalizzazione relativa al tipo di       *
      *    * espansione                                                *
      *    *-----------------------------------------------------------*
       prs-tip-esp-000.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione                       *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/fat/mov/fat301x[tip-esp]"
                                          to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-num          to   w-prs-tip-esp
           else      move  zero           to   w-prs-tip-esp          .
      *              *-------------------------------------------------*
      *              * Normalizzazione personalizzazione               *
      *              *-------------------------------------------------*
           if        w-prs-tip-esp        not  = 01
                     move  00             to   w-prs-tip-esp          .
       prs-tip-esp-999.
           exit.

      *    *===========================================================*
      *    * Lettura delle personalizzazioni relative alle spese per   *
      *    * la fatturazione                                           *
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
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     vis-not-rgd-800.
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
           move      16                   to   v-lin                  .
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
                                          =    "A"   or
                     w-ctl-tip-rig-chr (1)
                                          =    "C"   or
                     w-ctl-tip-rig-chr (2)
                                          =    "N"
                     go to vis-not-rgd-100.
       vis-not-rgd-210.
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda del tipo visualizza-   *
      *                  * zione                                       *
      *                  *---------------------------------------------*
           if        w-prs-tvn-rgd        =   01
                     go to vis-not-rgd-220
           else if   w-prs-tvn-rgd        =   02
                     go to vis-not-rgd-240
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
      *                          * Prezzo netto                        *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Prompt                          *
      *                              *---------------------------------*
           string    "Prezzo netto  "
                                delimited by   size
                     rf-fir-sgl-vpp
                                delimited by   size
                     " :"
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
      *                          * Sconto effettivo                    *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Prompt                          *
      *                              *---------------------------------*
           string    "Sconto effettivo  :"
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
       vis-not-rgd-240.
      *                  *---------------------------------------------*
      *                  * Se tipo visualizzazione : 02                *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Composizione prima riga                 *
      *                      *-----------------------------------------*
           move      spaces               to   w-vis-not-rgd-x80      .
           move      1                    to   w-vis-not-rgd-pnt      .
       vis-not-rgd-242.
      *                          *-------------------------------------*
      *                          * Prezzo netto                        *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Prompt                          *
      *                              *---------------------------------*
           string    "Prezzo netto  "
                                delimited by   size
                     rf-fir-sgl-vpp
                                delimited by   size
                     " :"
                                delimited by   size
                                          into w-vis-not-rgd-x80
                                  with pointer w-vis-not-rgd-pnt      .
       vis-not-rgd-244.
      *                          *-------------------------------------*
      *                          * Posizionamento puntatore            *
      *                          *-------------------------------------*
           move      37                   to   w-vis-not-rgd-pnt      .
       vis-not-rgd-246.
      *                          *-------------------------------------*
      *                          * Prezzo lordo standard               *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Prompt                          *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Inizio                      *
      *                                  *-----------------------------*
           string    "Lordo standard "
                                delimited by   size
                                          into w-vis-not-rgd-x80
                                  with pointer w-vis-not-rgd-pnt      .
      *                                  *-----------------------------*
      *                                  * Sigla valuta per prezzi     *
      *                                  * standard, se diversa da si- *
      *                                  * gla valuta per il prezzo    *
      *                                  *-----------------------------*
      *                                      *-------------------------*
      *                                      * Test                    *
      *                                      *-------------------------*
           if        rf-fir-sgl-vps       =    rf-fir-sgl-vpp
                     go to vis-not-rgd-247.
      *                                      *-------------------------*
      *                                      * String                  *
      *                                      *-------------------------*
           string    rf-fir-sgl-vps
                                delimited by   size
                                          into w-vis-not-rgd-x80
                                  with pointer w-vis-not-rgd-pnt      .
       vis-not-rgd-247.
      *                                  *-----------------------------*
      *                                  * Fine                        *
      *                                  *-----------------------------*
           string    " :"
                                delimited by   size
                                          into w-vis-not-rgd-x80
                                  with pointer w-vis-not-rgd-pnt      .
       vis-not-rgd-248.
      *                      *-----------------------------------------*
      *                      * Bufferizzazione prima riga composta     *
      *                      *-----------------------------------------*
           move      w-vis-not-rgd-x80    to   w-vis-not-rgd-a77      .
       vis-not-rgd-250.
      *                      *-----------------------------------------*
      *                      * Composizione seconda riga               *
      *                      *-----------------------------------------*
           move      spaces               to   w-vis-not-rgd-x80      .
           move      1                    to   w-vis-not-rgd-pnt      .
       vis-not-rgd-252.
      *                          *-------------------------------------*
      *                          * Sconto effettivo                    *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Prompt                          *
      *                              *---------------------------------*
           string    "Sconto effettivo  :"
                                delimited by   size
                                          into w-vis-not-rgd-x80
                                  with pointer w-vis-not-rgd-pnt      .
       vis-not-rgd-254.
      *                          *-------------------------------------*
      *                          * Posizionamento puntatore            *
      *                          *-------------------------------------*
           move      37                   to   w-vis-not-rgd-pnt      .
       vis-not-rgd-256.
      *                          *-------------------------------------*
      *                          * Prezzo netto standard               *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Prompt                          *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Inizio                      *
      *                                  *-----------------------------*
           string    "Netto standard "
                                delimited by   size
                                          into w-vis-not-rgd-x80
                                  with pointer w-vis-not-rgd-pnt      .
      *                                  *-----------------------------*
      *                                  * Sigla valuta per prezzi     *
      *                                  * standard, se diversa da si- *
      *                                  * gla valuta per il prezzo    *
      *                                  *-----------------------------*
      *                                      *-------------------------*
      *                                      * Test                    *
      *                                      *-------------------------*
           if        rf-fir-sgl-vps       =    rf-fir-sgl-vpp
                     go to vis-not-rgd-257.
      *                                      *-------------------------*
      *                                      * String                  *
      *                                      *-------------------------*
           string    rf-fir-sgl-vps
                                delimited by   size
                                          into w-vis-not-rgd-x80
                                  with pointer w-vis-not-rgd-pnt      .
       vis-not-rgd-257.
      *                                  *-----------------------------*
      *                                  * Fine                        *
      *                                  *-----------------------------*
           string    " :"
                                delimited by   size
                                          into w-vis-not-rgd-x80
                                  with pointer w-vis-not-rgd-pnt      .
       vis-not-rgd-258.
      *                      *-----------------------------------------*
      *                      * Bufferizzazione seconda riga composta   *
      *                      *-----------------------------------------*
           move      w-vis-not-rgd-x80    to   w-vis-not-rgd-b77      .
      *                      *-----------------------------------------*
      *                      * A visualizzazione note operative        *
      *                      *-----------------------------------------*
           go to     vis-not-rgd-280.
       vis-not-rgd-280.
      *                  *---------------------------------------------*
      *                  * A visualizzazione note operative            *
      *                  *---------------------------------------------*
           go to     vis-not-rgd-800.
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
           if        w-prs-tvn-rgd        =   01
                     go to vis-not-rgd-400
           else if   w-prs-tvn-rgd        =   02
                     go to vis-not-rgd-500
           else      go to vis-not-rgd-999.
       vis-not-rgd-400.
      *                  *---------------------------------------------*
      *                  * Se tipo visualizzazione : 01                *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Composizione prima riga                 *
      *                      *-----------------------------------------*
           move      spaces               to   w-vis-not-rgd-x80      .
           move      1                    to   w-vis-not-rgd-pnt      .
       vis-not-rgd-410.
      *                          *-------------------------------------*
      *                          * Prezzo netto                        *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Prompt                          *
      *                              *---------------------------------*
           string    "Prezzo netto  "
                                delimited by   size
                     rf-fir-sgl-vpp
                                delimited by   size
                     " :"
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
           move      "V"                  to   v-tip                  .
           move      09                   to   v-car                  .
           move      rf-fir-dec-vpp       to   v-dec                  .
           add       rf-fir-dec-prz       to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BG"                to   v-edm                  .
           move      rf-fir-prz-net       to   v-num                  .
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
      *                          * Sconto effettivo                    *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Prompt                          *
      *                              *---------------------------------*
           string    "Sconto effettivo  :"
                                delimited by   size
                                          into w-vis-not-rgd-x80
                                  with pointer w-vis-not-rgd-pnt      .
      *                              *---------------------------------*
      *                              * Valore                          *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Prezzo lordo standard in    *
      *                                  * work di lavoro              *
      *                                  *-----------------------------*
           move      rf-fir-prz-lrs       to   w-vis-not-rgd-pvp      .
      *                                  *-----------------------------*
      *                                  * Conversione prezzo lordo    *
      *                                  * standard nella valuta per   *
      *                                  * il prezzo                   *
      *                                  *-----------------------------*
      *                                      *-------------------------*
      *                                      * Test se necessario      *
      *                                      *-------------------------*
           if        rf-fir-sgl-vps       =    rf-fir-sgl-vpp
                     go to vis-not-rgd-461.
      *                                      *-------------------------*
      *                                      * Conversione valore da   *
      *                                      * valuta per i prezzi     *
      *                                      * standard a valuta base  *
      *                                      *-------------------------*
           move      rf-fir-sgl-vps       to   w-cvs-vlt-sgl          .
           move      rf-fir-dec-vps       to   w-cvs-vlt-dec          .
           move      rf-fir-tdc-vps       to   w-cvs-vlt-tdc          .
           move      rf-fir-cdc-vps       to   w-cvs-vlt-cdc          .
           move      w-vis-not-rgd-pvp    to   w-cvs-vlt-aav          .
           perform   cvs-alt-vlb-000      thru cvs-alt-vlb-999        .
      *                                      *-------------------------*
      *                                      * Conversione valore da   *
      *                                      * valuta base a valuta    *
      *                                      * per il prezzo           *
      *                                      *-------------------------*
           move      rf-fir-sgl-vpp       to   w-cvs-vlt-sgl          .
           move      rf-fir-dec-vpp       to   w-cvs-vlt-dec          .
           move      rf-fir-tdc-vpp       to   w-cvs-vlt-tdc          .
           move      rf-fir-cdc-vpp       to   w-cvs-vlt-cdc          .
           perform   cvs-vlb-alt-000      thru cvs-vlb-alt-999        .
           move      w-cvs-vlt-aav        to   w-vis-not-rgd-pvp      .
       vis-not-rgd-461.
      *                                  *-----------------------------*
      *                                  * Test se determinabile       *
      *                                  *-----------------------------*
           if        w-vis-not-rgd-pvp    =    zero or
                     rf-fir-prz-net       =    zero
                     go to vis-not-rgd-462
           else      go to vis-not-rgd-464.
       vis-not-rgd-462.
      *                                  *-----------------------------*
      *                                  * Se non determinabile        *
      *                                  *-----------------------------*
      *                                      *-------------------------*
      *                                      * String 'n.d.'           *
      *                                      *-------------------------*
           string    " n.d."
                                delimited by   size
                                          into w-vis-not-rgd-x80
                                  with pointer w-vis-not-rgd-pnt      .
           go to     vis-not-rgd-470.
       vis-not-rgd-464.
      *                                  *-----------------------------*
      *                                  * Se determinabile            *
      *                                  *-----------------------------*
      *                                      *-------------------------*
      *                                      * Determinazione          *
      *                                      *-------------------------*
      *                                          *---------------------*
      *                                          * Determinazione va-  *
      *                                          * lore scostamento    *
      *                                          *---------------------*
           subtract  rf-fir-prz-net       from w-vis-not-rgd-pvp
                                        giving w-vis-not-rgd-vsc      .
      *                                          *---------------------*
      *                                          * Determinazione per- *
      *                                          * centuale scostamen- *
      *                                          * to                  *
      *                                          *---------------------*
           multiply  100                  by   w-vis-not-rgd-vsc      .
           divide    w-vis-not-rgd-pvp    into w-vis-not-rgd-vsc
                                        giving w-vis-not-rgd-psc
                                               rounded                .
      *                                      *-------------------------*
      *                                      * Editing                 *
      *                                      *-------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      01                   to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "<D"                 to   v-edm                  .
           move      w-vis-not-rgd-psc    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                                      *-------------------------*
      *                                      * String                  *
      *                                      *-------------------------*
           string    " "
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
           go to     vis-not-rgd-800.
       vis-not-rgd-500.
      *                  *---------------------------------------------*
      *                  * Se tipo visualizzazione : 02                *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Composizione prima riga                 *
      *                      *-----------------------------------------*
           move      spaces               to   w-vis-not-rgd-x80      .
           move      1                    to   w-vis-not-rgd-pnt      .
       vis-not-rgd-510.
      *                          *-------------------------------------*
      *                          * Prezzo netto                        *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Prompt                          *
      *                              *---------------------------------*
           string    "Prezzo netto  "
                                delimited by   size
                     rf-fir-sgl-vpp
                                delimited by   size
                     " :"
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
           move      "V"                  to   v-tip                  .
           move      09                   to   v-car                  .
           move      rf-fir-dec-vpp       to   v-dec                  .
           add       rf-fir-dec-prz       to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BG"                to   v-edm                  .
           move      rf-fir-prz-net       to   v-num                  .
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
       vis-not-rgd-520.
      *                          *-------------------------------------*
      *                          * Posizionamento puntatore            *
      *                          *-------------------------------------*
           move      37                   to   w-vis-not-rgd-pnt      .
       vis-not-rgd-530.
      *                          *-------------------------------------*
      *                          * Prezzo lordo standard               *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Prompt                          *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Inizio                      *
      *                                  *-----------------------------*
           string    "Lordo standard "
                                delimited by   size
                                          into w-vis-not-rgd-x80
                                  with pointer w-vis-not-rgd-pnt      .
      *                                  *-----------------------------*
      *                                  * Sigla valuta per prezzi     *
      *                                  * standard, se diversa da si- *
      *                                  * gla valuta per il prezzo    *
      *                                  *-----------------------------*
      *                                      *-------------------------*
      *                                      * Test                    *
      *                                      *-------------------------*
           if        rf-fir-sgl-vps       =    rf-fir-sgl-vpp
                     go to vis-not-rgd-535.
      *                                      *-------------------------*
      *                                      * String                  *
      *                                      *-------------------------*
           string    rf-fir-sgl-vps
                                delimited by   size
                                          into w-vis-not-rgd-x80
                                  with pointer w-vis-not-rgd-pnt      .
       vis-not-rgd-535.
      *                                  *-----------------------------*
      *                                  * Fine                        *
      *                                  *-----------------------------*
           string    " :"
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
           move      "V"                  to   v-tip                  .
           move      09                   to   v-car                  .
           move      rf-fir-dec-vps       to   v-dec                  .
           add       rf-fir-dec-prz       to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BG"                to   v-edm                  .
           move      rf-fir-prz-lrs       to   v-num                  .
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
      *                      *-----------------------------------------*
      *                      * Bufferizzazione prima riga composta     *
      *                      *-----------------------------------------*
           move      w-vis-not-rgd-x80    to   w-vis-not-rgd-a77      .
       vis-not-rgd-550.
      *                      *-----------------------------------------*
      *                      * Composizione seconda riga               *
      *                      *-----------------------------------------*
           move      spaces               to   w-vis-not-rgd-x80      .
           move      1                    to   w-vis-not-rgd-pnt      .
       vis-not-rgd-560.
      *                          *-------------------------------------*
      *                          * Sconto effettivo                    *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Prompt                          *
      *                              *---------------------------------*
           string    "Sconto effettivo  :"
                                delimited by   size
                                          into w-vis-not-rgd-x80
                                  with pointer w-vis-not-rgd-pnt      .
      *                              *---------------------------------*
      *                              * Valore                          *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Prezzo lordo standard in    *
      *                                  * work di lavoro              *
      *                                  *-----------------------------*
           move      rf-fir-prz-lrs       to   w-vis-not-rgd-pvp      .
      *                                  *-----------------------------*
      *                                  * Conversione prezzo lordo    *
      *                                  * standard nella valuta per   *
      *                                  * il prezzo                   *
      *                                  *-----------------------------*
      *                                      *-------------------------*
      *                                      * Test se necessario      *
      *                                      *-------------------------*
           if        rf-fir-sgl-vps       =    rf-fir-sgl-vpp
                     go to vis-not-rgd-561.
      *                                      *-------------------------*
      *                                      * Conversione valore da   *
      *                                      * valuta per i prezzi     *
      *                                      * standard a valuta base  *
      *                                      *-------------------------*
           move      rf-fir-sgl-vps       to   w-cvs-vlt-sgl          .
           move      rf-fir-dec-vps       to   w-cvs-vlt-dec          .
           move      rf-fir-tdc-vps       to   w-cvs-vlt-tdc          .
           move      rf-fir-cdc-vps       to   w-cvs-vlt-cdc          .
           move      w-vis-not-rgd-pvp    to   w-cvs-vlt-aav          .
           perform   cvs-alt-vlb-000      thru cvs-alt-vlb-999        .
      *                                      *-------------------------*
      *                                      * Conversione valore da   *
      *                                      * valuta base a valuta    *
      *                                      * per il prezzo           *
      *                                      *-------------------------*
           move      rf-fir-sgl-vpp       to   w-cvs-vlt-sgl          .
           move      rf-fir-dec-vpp       to   w-cvs-vlt-dec          .
           move      rf-fir-tdc-vpp       to   w-cvs-vlt-tdc          .
           move      rf-fir-cdc-vpp       to   w-cvs-vlt-cdc          .
           perform   cvs-vlb-alt-000      thru cvs-vlb-alt-999        .
           move      w-cvs-vlt-aav        to   w-vis-not-rgd-pvp      .
       vis-not-rgd-561.
      *                                  *-----------------------------*
      *                                  * Test se determinabile       *
      *                                  *-----------------------------*
           if        w-vis-not-rgd-pvp    =    zero or
                     rf-fir-prz-net       =    zero
                     go to vis-not-rgd-562
           else      go to vis-not-rgd-564.
       vis-not-rgd-562.
      *                                  *-----------------------------*
      *                                  * Se non determinabile        *
      *                                  *-----------------------------*
      *                                      *-------------------------*
      *                                      * String 'n.d.'           *
      *                                      *-------------------------*
           string    " n.d."
                                delimited by   size
                                          into w-vis-not-rgd-x80
                                  with pointer w-vis-not-rgd-pnt      .
           go to     vis-not-rgd-570.
       vis-not-rgd-564.
      *                                  *-----------------------------*
      *                                  * Se determinabile            *
      *                                  *-----------------------------*
      *                                      *-------------------------*
      *                                      * Determinazione          *
      *                                      *-------------------------*
      *                                          *---------------------*
      *                                          * Determinazione va-  *
      *                                          * lore scostamento    *
      *                                          *---------------------*
           subtract  rf-fir-prz-net       from w-vis-not-rgd-pvp
                                        giving w-vis-not-rgd-vsc      .
      *                                          *---------------------*
      *                                          * Determinazione per- *
      *                                          * centuale scostamen- *
      *                                          * to                  *
      *                                          *---------------------*
           multiply  100                  by   w-vis-not-rgd-vsc      .
           divide    w-vis-not-rgd-pvp    into w-vis-not-rgd-vsc
                                        giving w-vis-not-rgd-psc
                                               rounded                .
      *                                      *-------------------------*
      *                                      * Editing                 *
      *                                      *-------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      01                   to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "<D"                 to   v-edm                  .
           move      w-vis-not-rgd-psc    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                                      *-------------------------*
      *                                      * String                  *
      *                                      *-------------------------*
           string    " "
                                delimited by   size
                     v-edt
                                delimited by   spaces
                                          into w-vis-not-rgd-x80
                                  with pointer w-vis-not-rgd-pnt      .
       vis-not-rgd-570.
      *                          *-------------------------------------*
      *                          * Posizionamento puntatore            *
      *                          *-------------------------------------*
           move      37                   to   w-vis-not-rgd-pnt      .
       vis-not-rgd-580.
      *                          *-------------------------------------*
      *                          * Prezzo netto standard               *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Prompt                          *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Inizio                      *
      *                                  *-----------------------------*
           string    "Netto standard "
                                delimited by   size
                                          into w-vis-not-rgd-x80
                                  with pointer w-vis-not-rgd-pnt      .
      *                                  *-----------------------------*
      *                                  * Sigla valuta per prezzi     *
      *                                  * standard, se diversa da si- *
      *                                  * gla valuta per il prezzo    *
      *                                  *-----------------------------*
      *                                      *-------------------------*
      *                                      * Test                    *
      *                                      *-------------------------*
           if        rf-fir-sgl-vps       =    rf-fir-sgl-vpp
                     go to vis-not-rgd-585.
      *                                      *-------------------------*
      *                                      * String                  *
      *                                      *-------------------------*
           string    rf-fir-sgl-vps
                                delimited by   size
                                          into w-vis-not-rgd-x80
                                  with pointer w-vis-not-rgd-pnt      .
       vis-not-rgd-585.
      *                                  *-----------------------------*
      *                                  * Fine                        *
      *                                  *-----------------------------*
           string    " :"
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
           move      "V"                  to   v-tip                  .
           move      09                   to   v-car                  .
           move      rf-fir-dec-vps       to   v-dec                  .
           add       rf-fir-dec-prz       to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BG"                to   v-edm                  .
           move      rf-fir-prz-nts       to   v-num                  .
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
       vis-not-rgd-600.
      *                          *-------------------------------------*
      *                          * Scostamento tra prezzo netto stan-  *
      *                          * dard e prezzo netto effettivo       *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Valore                          *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Prezzo netto standard in    *
      *                                  * work di lavoro              *
      *                                  *-----------------------------*
           move      rf-fir-prz-nts       to   w-vis-not-rgd-pvp      .
      *                                  *-----------------------------*
      *                                  * Conversione prezzo netto    *
      *                                  * standard nella valuta per   *
      *                                  * il prezzo                   *
      *                                  *-----------------------------*
      *                                      *-------------------------*
      *                                      * Test se necessario      *
      *                                      *-------------------------*
           if        rf-fir-sgl-vps       =    rf-fir-sgl-vpp
                     go to vis-not-rgd-601.
      *                                      *-------------------------*
      *                                      * Conversione valore da   *
      *                                      * valuta per i prezzi     *
      *                                      * standard a valuta base  *
      *                                      *-------------------------*
           move      rf-fir-sgl-vps       to   w-cvs-vlt-sgl          .
           move      rf-fir-dec-vps       to   w-cvs-vlt-dec          .
           move      rf-fir-tdc-vps       to   w-cvs-vlt-tdc          .
           move      rf-fir-cdc-vps       to   w-cvs-vlt-cdc          .
           move      w-vis-not-rgd-pvp    to   w-cvs-vlt-aav          .
           perform   cvs-alt-vlb-000      thru cvs-alt-vlb-999        .
      *                                      *-------------------------*
      *                                      * Conversione valore da   *
      *                                      * valuta base a valuta    *
      *                                      * per il prezzo           *
      *                                      *-------------------------*
           move      rf-fir-sgl-vpp       to   w-cvs-vlt-sgl          .
           move      rf-fir-dec-vpp       to   w-cvs-vlt-dec          .
           move      rf-fir-tdc-vpp       to   w-cvs-vlt-tdc          .
           move      rf-fir-cdc-vpp       to   w-cvs-vlt-cdc          .
           perform   cvs-vlb-alt-000      thru cvs-vlb-alt-999        .
           move      w-cvs-vlt-aav        to   w-vis-not-rgd-pvp      .
       vis-not-rgd-601.
      *                                  *-----------------------------*
      *                                  * Test se determinabile       *
      *                                  *-----------------------------*
           if        w-vis-not-rgd-pvp    =    zero or
                     rf-fir-prz-net       =    zero
                     go to vis-not-rgd-602
           else      go to vis-not-rgd-604.
       vis-not-rgd-602.
      *                                  *-----------------------------*
      *                                  * Se non determinabile        *
      *                                  *-----------------------------*
      *                                      *-------------------------*
      *                                      * Nessuna azione          *
      *                                      *-------------------------*
           go to     vis-not-rgd-610.
       vis-not-rgd-604.
      *                                  *-----------------------------*
      *                                  * Se determinabile            *
      *                                  *-----------------------------*
      *                                      *-------------------------*
      *                                      * Determinazione          *
      *                                      *-------------------------*
      *                                          *---------------------*
      *                                          * Determinazione va-  *
      *                                          * lore scostamento    *
      *                                          *---------------------*
           subtract  w-vis-not-rgd-pvp    from rf-fir-prz-net
                                        giving w-vis-not-rgd-vsc      .
      *                                          *---------------------*
      *                                          * Determinazione per- *
      *                                          * centuale scostamen- *
      *                                          * to                  *
      *                                          *---------------------*
           multiply  100                  by   w-vis-not-rgd-vsc      .
           divide    w-vis-not-rgd-pvp    into w-vis-not-rgd-vsc
                                        giving w-vis-not-rgd-psc
                                               rounded                .
      *                                      *-------------------------*
      *                                      * Editing                 *
      *                                      *-------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      01                   to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "<D"                 to   v-edm                  .
           move      w-vis-not-rgd-psc    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                                      *-------------------------*
      *                                      * String                  *
      *                                      *-------------------------*
           string    " ["
                                delimited by   size
                     v-edt
                                delimited by   spaces
                     "%]"
                                delimited by   size
                                          into w-vis-not-rgd-x80
                                  with pointer w-vis-not-rgd-pnt      .
       vis-not-rgd-610.
      *                      *-----------------------------------------*
      *                      * Bufferizzazione seconda riga composta   *
      *                      *-----------------------------------------*
           move      w-vis-not-rgd-x80    to   w-vis-not-rgd-b77      .
       vis-not-rgd-620.
      *                      *-----------------------------------------*
      *                      * A visualizzazione note operative        *
      *                      *-----------------------------------------*
           go to     vis-not-rgd-800.
       vis-not-rgd-800.
      *              *-------------------------------------------------*
      *              * Visualizzazione note operative                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Riga 1                                      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      77                   to   v-car                  .
           move      17                   to   v-lin                  .
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
           move      18                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      w-vis-not-rgd-b77    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       vis-not-rgd-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     vis-not-rgd-999.
       vis-not-rgd-999.
           exit.

      *    *===========================================================*
      *    * Routine di conversione da altra valuta a valuta base      *
      *    *                                                           *
      *    * Input  : w-cvs-vlt-sgl = Sigla dell'altra valuta          *
      *    *                                                           *
      *    *          w-cvs-vlt-dec = Numero decimali dell'altra valu- *
      *    *                          ta                               *
      *    *                                                           *
      *    *          w-cvs-vlt-tdc = Tipo di coefficiente dell'altra  *
      *    *                          valuta                           *
      *    *                                                           *
      *    *          w-cvs-vlt-cdc = Coefficiente di cambio dell'al-  *
      *    *                          tra valuta                       *
      *    *                                                           *
      *    *          w-cvs-vlt-aav = Ammontare da convertire, espres- *
      *    *                          so nell'altra valuta             *
      *    *                                                           *
      *    * Output : w-cvs-vlt-avb = Ammontare convertito, espresso   *
      *    *                          nella valuta base                *
      *    *                                                           *
      *    *-----------------------------------------------------------*
      *    * Routine di conversione da valuta base ad altra valuta     *
      *    *                                                           *
      *    * Input  : w-cvs-vlt-sgl = Sigla dell'altra valuta          *
      *    *                                                           *
      *    *          w-cvs-vlt-dec = Numero decimali dell'altra valu- *
      *    *                          ta                               *
      *    *                                                           *
      *    *          w-cvs-vlt-tdc = Tipo di coefficiente dell'altra  *
      *    *                          valuta                           *
      *    *                                                           *
      *    *          w-cvs-vlt-cdc = Coefficiente di cambio dell'al-  *
      *    *                          tra valuta                       *
      *    *                                                           *
      *    *          w-cvs-vlt-avb = Ammontare da convertire, espres- *
      *    *                          so nella valuta base             *
      *    *                                                           *
      *    * Output : w-cvs-vlt-aav = Ammontare convertito, espresso   *
      *    *                          nell'altra valuta                *
      *    *                                                           *
      *    *-----------------------------------------------------------*
      *    * Routine di conversione da valuta a valuta                 *
      *    *                                                           *
      *    * Input  : Valuta da trasformare                            *
      *    *                                                           *
      *    *            - w-cvs-vdt-sgl = Sigla valuta                 *
      *    *                                                           *
      *    *            - w-cvs-vdt-dec = Numero decimali              *
      *    *                                                           *
      *    *            - w-cvs-vdt-tdc = Tipo di coefficiente         *
      *    *                                                           *
      *    *            - w-cvs-vdt-cdc = Coefficiente di cambio ri-   *
      *    *                              spetto alla valuta base      *
      *    *                                                           *
      *    *            - w-cvs-vdt-amm = Ammontare da convertire,     *
      *    *                              espresso nella valuta da     *
      *    *                              trasformare                  *
      *    *                                                           *
      *    *          Valuta in cui trasformare                        *
      *    *                                                           *
      *    *            - w-cvs-vnu-sgl = Sigla valuta                 *
      *    *                                                           *
      *    *            - w-cvs-vnu-dec = Numero decimali              *
      *    *                                                           *
      *    *            - w-cvs-vnu-tdc = Tipo di coefficiente         *
      *    *                                                           *
      *    *            - w-cvs-vnu-cdc = Coefficiente di cambio ri-   *
      *    *                              spetto alla valuta base      *
      *    *                                                           *
      *    * Output : Valuta in cui trasformare                        *
      *    *                                                           *
      *    *            - w-cvs-vnu-amm = Ammontare convertito,        *
      *    *                              espresso nella valuta        *
      *    *                              in cui trasformare           *
      *    *                                                           *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wcvsvlt0.cps"                   .

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
      *              * Normalizzazione comodo di accetazione           *
      *              *-------------------------------------------------*
           move      spaces               to   w-wrk-fnd-pro-alf      .
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
           move      v-num                to   w-let-arc-dcp-num      .
           move      v-alf                to   w-let-arc-dcp-alf      .
           perform   let-arc-dcp-000      thru let-arc-dcp-999        .
           move      w-let-arc-dcp-alf    to   w-wrk-fnd-pro-alf      .
      *              *-------------------------------------------------*
      *              * Visualizzazione descrizione prodotto            *
      *              *-------------------------------------------------*
           perform   vis-cod-pro-des-000  thru vis-cod-pro-des-999    .
      *              *-------------------------------------------------*
      *              * Se codice a zero : uscita                       *
      *              *-------------------------------------------------*
           if        w-let-arc-dcp-num    =    zero or
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
      *    * Routine lettura archivio [dcp]                            *
      *    *-----------------------------------------------------------*
       let-arc-dcp-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-dcp-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice Prodotto di vendita a zero       *
      *              *-------------------------------------------------*
           if        w-let-arc-dcp-num    =    zero
                     go to let-arc-dcp-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRO    "         to   f-key                  .
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
           move      rf-dcp-alf-pro       to   w-let-arc-dcp-alf      .
           move      rf-dcp-des-pro       to   w-let-arc-dcp-des      .
           move      rf-dcp-umi-ven       to   w-let-arc-dcp-umi      .
           move      rf-dcp-dec-qta       to   w-let-arc-dcp-ndq      .
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
           move      spaces               to   w-let-arc-dcp-umi      .
           move      zero                 to   w-let-arc-dcp-ndq      .
       let-arc-dcp-999.
           exit.

      *    *===========================================================*
      *    * Determinazione totale documento                           *
      *    *-----------------------------------------------------------*
       det-tot-doc-000.
      *              *-------------------------------------------------*
      *              * Azzeramento totalizzatore                       *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-tot-doc-tot      .
      *              *-------------------------------------------------*
      *              * Ciclo di scansione castelletto iva              *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-tot-doc-ctr      .
       det-tot-doc-100.
           add       1                    to   w-det-tot-doc-ctr      .
           if        w-det-tot-doc-ctr    >    6
                     go to det-tot-doc-999.
           if        rf-fit-iva-cod
                    (w-det-tot-doc-ctr)   =    zero
                     go to det-tot-doc-100.
      *                  *---------------------------------------------*
      *                  * Imponibile                                  *
      *                  *---------------------------------------------*
           add       rf-fit-iva-ibl
                    (w-det-tot-doc-ctr)   to   w-det-tot-doc-tot      .
      *                  *---------------------------------------------*
      *                  * Imposta                                     *
      *                  *---------------------------------------------*
           add       rf-fit-iva-imp
                    (w-det-tot-doc-ctr)   to   w-det-tot-doc-tot      .
       det-tot-doc-800.
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     det-tot-doc-100.
       det-tot-doc-999.
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
           move      zero                 to   w-wrk-ctr-001          .
       pmt-spe-imp-100.
           add       1                    to   w-wrk-ctr-001          .
           if        w-wrk-ctr-001        >    w-prs-spe-fat-nst
                     go to pmt-spe-imp-999.
      *                  *---------------------------------------------*
      *                  * Visualizzazione                             *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      25                   to   v-car                  .
           add       18
                     w-wrk-ctr-001      giving v-lin                  .
           if        w-wrk-ctr-001        >    3
                     move  42             to   v-pos
                     subtract  03         from v-lin
           else      move  03             to   v-pos                  .
           move      w-prs-spe-fat-dve
                    (w-wrk-ctr-001)       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      01                   to   v-car                  .
           add       18
                     w-wrk-ctr-001      giving v-lin                  .
           if        w-wrk-ctr-001        >    3
                     move  59             to   v-pos
                     subtract  03         from v-lin
           else      move  20             to   v-pos                  .
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
      *    * Visualizzazione : Si/No addebito spese                    *
      *    *-----------------------------------------------------------*
       vis-spe-snx-000.
      *              *-------------------------------------------------*
      *              * Ciclo di scansione                              *
      *              *-------------------------------------------------*
           move      zero                 to   w-wrk-ctr-001          .
       vis-spe-snx-100.
           add       1                    to   w-wrk-ctr-001          .
           if        w-wrk-ctr-001        >    w-prs-spe-fat-nst
                     go to vis-spe-snx-999.
      *                  *---------------------------------------------*
      *                  * Determinazione indice per la spesa          *
      *                  *---------------------------------------------*
           move      w-prs-spe-fat-npt
                    (w-wrk-ctr-001)       to   w-wrk-ctr-002          .
      *                  *---------------------------------------------*
      *                  * Visualizzazione                             *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-spe-snx-lun    to   v-car                  .
           move      w-exp-spe-snx-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           add       18
                     w-wrk-ctr-001      giving v-lin                  .
           if        w-wrk-ctr-001        >    3
                     move  61             to   v-pos
                     subtract  03         from v-lin
           else      move  22             to   v-pos                  .
           move      w-exp-spe-snx-tbl    to   v-txt                  .
           if        rf-fit-spe-snx
                    (w-wrk-ctr-002)       =    0
                     move  1              to   v-num
           else if   rf-fit-spe-snx
                    (w-wrk-ctr-002)       =    1
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
      *    * Visualizzazione : Importi spese                           *
      *    *-----------------------------------------------------------*
       vis-spe-imp-000.
      *              *-------------------------------------------------*
      *              * Ciclo di scansione                              *
      *              *-------------------------------------------------*
           move      zero                 to   w-wrk-ctr-001          .
       vis-spe-imp-100.
           add       1                    to   w-wrk-ctr-001          .
           if        w-wrk-ctr-001        >    w-prs-spe-fat-nst
                     go to vis-spe-imp-999.
      *                  *---------------------------------------------*
      *                  * Determinazione indice per la spesa          *
      *                  *---------------------------------------------*
           move      w-prs-spe-fat-npt
                    (w-wrk-ctr-001)       to   w-wrk-ctr-002          .
      *                  *---------------------------------------------*
      *                  * Visualizzazione                             *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      09                   to   v-car                  .
           move      rf-fit-dec-vpf       to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "GB"                 to   v-edm                  .
           add       18
                     w-wrk-ctr-001      giving v-lin                  .
           if        w-wrk-ctr-001        >    3
                     move  64             to   v-pos
                     subtract  03         from v-lin
           else      move  25             to   v-pos                  .
           move      rf-fit-spe-imp
                    (w-wrk-ctr-002)       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Riciclo su scansione                        *
      *                  *---------------------------------------------*
           go to     vis-spe-imp-100.
       vis-spe-imp-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : % spese                                 *
      *    *-----------------------------------------------------------*
       vis-spe-per-000.
      *              *-------------------------------------------------*
      *              * Ciclo di scansione                              *
      *              *-------------------------------------------------*
           move      zero                 to   w-wrk-ctr-001          .
       vis-spe-per-100.
           add       1                    to   w-wrk-ctr-001          .
           if        w-wrk-ctr-001        >    w-prs-spe-fat-nst
                     go to vis-spe-per-999.
      *                  *---------------------------------------------*
      *                  * Determinazione indice per la spesa          *
      *                  *---------------------------------------------*
           move      w-prs-spe-fat-npt
                    (w-wrk-ctr-001)       to   w-wrk-ctr-002          .
      *                  *---------------------------------------------*
      *                  * Visualizzazione                             *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      01                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "BD"                 to   v-edm                  .
           add       18
                     w-wrk-ctr-001      giving v-lin                  .
           if        w-wrk-ctr-001        >    3
                     move  76             to   v-pos
                     subtract  03         from v-lin
           else      move  37             to   v-pos                  .
           move      rf-fit-spe-per
                    (w-wrk-ctr-002)
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
      *    * Subroutines per accettazione codice prodotto 'dcp'        *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/acoddcp0.acs"                   .

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
      *    * Subroutines per allineamenti a destra o a sinistra oppure *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cps"                   .


