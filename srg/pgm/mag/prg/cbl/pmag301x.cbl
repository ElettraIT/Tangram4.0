       Identification Division.
       Program-Id.                                 pmag301x           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    mag                 *
      *                                Settore:                        *
      *                                   Fase:                        *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 31/08/93    *
      *                       Ultima revisione:    NdK del 23/08/01    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      * ============================================================== *
      *                                                                *
      * Descrizione pgm:   Modulo di espansione righe movimenti        *
      *                                                                *
      * ============================================================== *
      *                                                                *
      * Tipi operazione :                                              *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "OP"  Open modulo                                              *
      *                                                                *
      *              Input  : w-esp-rig-mmr-ope : "OP"                 *
      *                                                                *
      *                                                                *
      *              Output : nessuno                                  *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "CL"  Close modulo                                             *
      *                                                                *
      *              Input  : w-esp-rig-mmr-ope : "CL"                 *
      *                                                                *
      *                                                                *
      *              Output : nessuno                                  *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "C?"  Test di cancellabilita' del modulo                       *
      *                                                                *
      *              Input  : w-esp-rig-mmr-ope : "C?"                 *
      *                                                                *
      *                                                                *
      *              Output : w-esp-rig-mmr-ope : spaces = Si          *
      *                                           "C?"   = No          *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "ES"  Espansione                                               *
      *                                                                *
      *              Input  : w-esp-rig-mmr-ope : "ES"                 *
      *                                                                *
      *                       w-esp-rig-mmr-prt : protocollo           *
      *                       w-esp-rig-mmr-dat : data documento       *
      *                       w-esp-rig-mmr-cau : causale              *
      *                       w-esp-rig-mmr-cod : codice alfanumerico  *
      *                                           prodotto (opzionale) *
      *                                                                *
      *              Output : nessuno                                  *
      *                                                                *
      *                                                                *
      *       N.B.: L'operazione di espansione non salva la 'start'    *
      *             sulle righe nel caso in cui venga effettuata       *
      *             una interrogazione sulle righe [mmr]               *
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
      *    * Record files                                              *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [mmt]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/mag/fls/rec/rfmmt"                          .
      *        *-------------------------------------------------------*
      *        * [mmr]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/mag/fls/rec/rfmmr"                          .
      *        *-------------------------------------------------------*
      *        * [dcc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfdcc"                          .
      *        *-------------------------------------------------------*
      *        * [dcp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfdcp"                          .
      *        *-------------------------------------------------------*
      *        * [pdx]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfpdx"                          .
      *        *-------------------------------------------------------*
      *        * [dps]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dps/fls/rec/rfdps"                          .
      *        *-------------------------------------------------------*
      *        * [dpm]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dpm/fls/rec/rfdpm"                          .
      *        *-------------------------------------------------------*
      *        * [mtv]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/mtv/fls/rec/rfmtv"                          .

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
      *        * Si/No righe gia' espanse al dettaglio                 *
      *        *-------------------------------------------------------*
      *            *---------------------------------------------------*
      *            * Comodo per ridefinizione della personalizzazione  *
      *            *---------------------------------------------------*
           05  w-prs-snr-det              pic  x(03)                  .
           05  w-prs-snr-det-r            redefines
               w-prs-snr-det.
      *            *---------------------------------------------------*
      *            * Si/no righe gia' espanse                          *
      *            *---------------------------------------------------*
               10  w-prs-snr-esp          pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Si/no richiesta ad utente                         *
      *            *---------------------------------------------------*
               10  w-prs-snr-ric          pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Si/no visualizzazione costi e valori              *
      *            *---------------------------------------------------*
               10  w-prs-snr-vcv          pic  x(01)                  .
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
      *        * Work per Let su archivio [dpm]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-dpm.
               10  w-let-arc-dpm-flg      pic  x(01)                  .
               10  w-let-arc-dpm-num      pic  9(07)                  .
               10  w-let-arc-dpm-alf      pic  x(14)                  .
               10  w-let-arc-dpm-des      pic  x(40)                  .
               10  w-let-arc-dpm-umi      pic  x(03)                  .
               10  w-let-arc-dpm-deq      pic  9(01)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [mtv]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-mtv.
               10  w-let-arc-mtv-flg      pic  x(01)                  .
               10  w-let-arc-mtv-num      pic  9(07)                  .
               10  w-let-arc-mtv-alf      pic  x(14)                  .
               10  w-let-arc-mtv-des      pic  x(40)                  .
               10  w-let-arc-mtv-umi      pic  x(03)                  .
               10  w-let-arc-mtv-deq      pic  9(01)                  .

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
      *                * Tipo e codice magazzino                       *
      *                *-----------------------------------------------*
                   15  w-lin-imm-seg-tr1  pic  x(01)                  .
                   15  w-lin-imm-alf-000.
                       20  w-lin-imm-tip-rig
                                          pic  x(02)                  .
                       20  w-lin-imm-alf-rig
                                          pic  x(14)                  .
                   15  w-lin-imm-seg-bn1  pic  x(01)                  .
      *                *-----------------------------------------------*
      *                * Descrizione                                   *
      *                *-----------------------------------------------*
                   15  w-lin-imm-seg-tr2  pic  x(01)                  .
                   15  w-lin-imm-seg-bn2  pic  x(01)                  .
                   15  w-lin-imm-des-rig  pic  x(40)                  .
      *                *-----------------------------------------------*
      *                * Quantita'                                     *
      *                *-----------------------------------------------*
                   15  w-lin-imm-seg-tr3  pic  x(01)                  .
                   15  w-lin-imm-seg-bn3  pic  x(01)                  .
                   15  w-lin-imm-qta-rig  pic  x(13)                  .

      *    *===========================================================*
      *    * Work per ridefinizione numeri protocollo                  *
      *    *-----------------------------------------------------------*
       01  w-num.
      *        *-------------------------------------------------------*
      *        * Work per : Numero protocollo saa.dd.nnnnnn            *
      *        *-------------------------------------------------------*
           05  w-num-prt-num.
               10  w-num-prt-num-num      pic  9(11)                  .
               10  w-num-prt-num-num-r    redefines
                   w-num-prt-num-num.
                   15  w-num-prt-num-saa  pic  9(03)                  .
                   15  w-num-prt-num-dpz  pic  9(02)                  .
                   15  w-num-prt-num-prg  pic  9(06)                  .
               10  w-num-prt-prg-num      pic  9(09)                  .
               10  w-num-prt-prg-num-r    redefines
                   w-num-prt-prg-num.
                   15  w-num-prt-prg-saa  pic  9(03)                  .
                   15  w-num-prt-prg-prg  pic  9(06)                  .

      *    *===========================================================*
      *    * Work-area locale                                          *
      *    *-----------------------------------------------------------*
       01  w-wrk.
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
      *        * Work per composizione stringa                         *
      *        *-------------------------------------------------------*
           05  w-wrk-str-x80              pic  x(80)                  .
           05  w-wrk-str-pnt              pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Work per ricerca codice prodotto                      *
      *        *-------------------------------------------------------*
           05  w-wrk-fnd-pro.
               10  w-wrk-fnd-pro-alf      pic  x(14)                  .
      *        *-------------------------------------------------------*
      *        * Work per descrizione                                  *
      *        *-------------------------------------------------------*
           05  w-wrk-des-mag.
               10  w-wrk-des-mag-wca      pic  x(14)                  .
               10  w-wrk-des-mag-wde      pic  x(40)                  .
               10  w-wrk-des-mag-wdq      pic  9(01)                  .

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

      *    *===========================================================*
      *    * Link-area per accettazione codice prodotto 'dcp'          *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/acoddcp0.acl"                   .

      *    *===========================================================*
      *    * Work per subroutine di accettazione                       *
      *    *-----------------------------------------------------------*
       01  w-aux.
      *        *-------------------------------------------------------*
      *        * Work per espansione record righe bolla                *
      *        *-------------------------------------------------------*
           05  w-aux-rec-mmr.
               10  w-aux-rec-mmr-rtr      pic  x(01)                  .
               10  w-aux-rec-mmr-c01      pic  9(05)                  .
               10  w-aux-rec-mmr-c02      pic  9(03)                  .
               10  w-aux-rec-mmr-c03      pic  9(05)                  .
               10  w-aux-rec-mmr-c04      pic  9(05)                  .
               10  w-aux-rec-mmr-c05      pic  9(05)                  .
               10  w-aux-rec-mmr-c06      pic  9(05)                  .
               10  w-aux-rec-mmr-cix      pic  9(03)                  .
               10  w-aux-rec-mmr-nli      pic  9(05)                  .
               10  w-aux-rec-mmr-crb      pic  9(05)                  .
               10  w-aux-rec-mmr-max      pic  9(05) value 999        .
               10  w-aux-rec-mmr-cpb      pic  9(05)                  .
               10  w-aux-rec-mmr-cpa      pic  9(05)                  .
               10  w-aux-rec-mmr-lt1      pic  x(03)                  .
               10  w-aux-rec-mmr-lt2      pic  x(03)                  .
               10  w-aux-rec-mmr-ltp      pic  x(17)                  .
               10  w-aux-rec-mmr-buf
                               occurs 999.
                   15  w-aux-rec-mmr-dat  pic  9(07)       comp-3     .
                   15  w-aux-rec-mmr-prt  pic  9(11)       comp-3     .
                   15  w-aux-rec-mmr-prg  pic  9(05)       comp-3     .
                   15  w-aux-rec-mmr-tip  pic  9(02)                  .
                   15  w-aux-rec-mmr-alf  pic  x(14)                  .

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Link-area per espansione su righe bolla                   *
      *    *-----------------------------------------------------------*
           copy      "pgm/mag/prg/cpy/pmag301x.pgl"                   .

      ******************************************************************
       Procedure Division                using w-esp-rig-mmr          .
      ******************************************************************

      *================================================================*
      *       Main                                                     *
      *================================================================*
       main-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo operazione      *
      *              *-------------------------------------------------*
           if        w-esp-rig-mmr-ope    =    "OP"
                     perform   opn-000    thru opn-999
           else if   w-esp-rig-mmr-ope    =    "CL"
                     perform   cls-000    thru cls-999
           else if   w-esp-rig-mmr-ope    =    "C?"
                     perform   tcm-000    thru tcm-999
           else if   w-esp-rig-mmr-ope    =    "ES"
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
      *                      * Si/No righe gia' espanse al dettaglio   *
      *                      *-----------------------------------------*
           perform   prs-snr-det-000      thru prs-snr-det-999        .
      *                      *-----------------------------------------*
      *                      * Si/No gestione semilavorati attiva      *
      *                      *-----------------------------------------*
           perform   prs-dps-snx-000      thru prs-dps-snx-999        .
      *                      *-----------------------------------------*
      *                      * Si/No gestione materie prime attiva     *
      *                      *-----------------------------------------*
           perform   prs-dpm-snx-000      thru prs-dpm-snx-999        .
      *                      *-----------------------------------------*
      *                      * Si/No gestione materiali vari attiva    *
      *                      *-----------------------------------------*
           perform   prs-mtv-snx-000      thru prs-mtv-snx-999        .
       opn-120.
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * [mmt]                                       *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmmt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mmt                 .
      *                  *---------------------------------------------*
      *                  * [mmr]                                       *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmmr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mmr                 .
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
       opn-200.
      *              *-------------------------------------------------*
      *              * [dps]                                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se gestione semilavorati attiva        *
      *                  *---------------------------------------------*
           if        w-prs-dps-snx        not  = "S"
                     go to opn-220.
      *                  *---------------------------------------------*
      *                  * Apertura file                               *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dps/fls/ioc/obj/iofdps"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dps                 .
       opn-220.
      *              *-------------------------------------------------*
      *              * [dpm]                                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se gestione materie prime attiva       *
      *                  *---------------------------------------------*
           if        w-prs-dpm-snx        not  = "S"
                     go to opn-240.
      *                  *---------------------------------------------*
      *                  * Apertura file                               *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dpm/fls/ioc/obj/iofdpm"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dpm                 .
       opn-240.
      *              *-------------------------------------------------*
      *              * [mtv]                                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se gestione materiali vari attiva      *
      *                  *---------------------------------------------*
           if        w-prs-mtv-snx        not  = "S"
                     go to opn-999.
      *                  *---------------------------------------------*
      *                  * Apertura file                               *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/mtv/fls/ioc/obj/iofmtv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mtv                 .
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
      *                  * [mmt]                                       *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmmt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mmt                 .
      *                  *---------------------------------------------*
      *                  * [mmr]                                       *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmmr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mmr                 .
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
       cls-200.
      *              *-------------------------------------------------*
      *              * [dps]                                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se gestione semilavorati attiva        *
      *                  *---------------------------------------------*
           if        w-prs-dps-snx        not  = "S"
                     go to cls-220.
      *                  *---------------------------------------------*
      *                  * Chiusura file                               *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dps/fls/ioc/obj/iofdps"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dps                 .
       cls-220.
      *              *-------------------------------------------------*
      *              * [dpm]                                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se gestione materie prime attiva       *
      *                  *---------------------------------------------*
           if        w-prs-dpm-snx        not  = "S"
                     go to cls-240.
      *                  *---------------------------------------------*
      *                  * Chiusura file                               *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dpm/fls/ioc/obj/iofdpm"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dpm                 .
       cls-240.
      *              *-------------------------------------------------*
      *              * [mtv]                                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se gestione materiali vari attiva      *
      *                  *---------------------------------------------*
           if        w-prs-mtv-snx        not  = "S"
                     go to cls-999.
      *                  *---------------------------------------------*
      *                  * Chiusura file                               *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/mtv/fls/ioc/obj/iofmtv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mtv                 .
       cls-999.
           exit.

      *    *===========================================================*
      *    * Lettura personalizzazione : Si/No righe gia' espanse al   *
      *    *                             dettaglio                     *
      *    *-----------------------------------------------------------*
       prs-snr-det-000.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione                       *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/mag/mov/mag301[snr-det]"
                                          to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Se personalizzazione non esistente              *
      *              *-------------------------------------------------*
           if        s-ves                =    spaces
                     go to prs-snr-det-050.
      *                  *---------------------------------------------*
      *                  * Normalizzazione al valore di default        *
      *                  *---------------------------------------------*
           move      "N"                  to   w-prs-snr-esp          .
           move      "N"                  to   w-prs-snr-ric          .
           move      "N"                  to   w-prs-snr-vcv          .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     prs-snr-det-999.
       prs-snr-det-050.
      *              *-------------------------------------------------*
      *              * Valore letto in work personalizzazioni          *
      *              *-------------------------------------------------*
           move      s-alf                to   w-prs-snr-det          .
       prs-snr-det-100.
      *              *-------------------------------------------------*
      *              * Controllo valori letti                          *
      *              *-------------------------------------------------*
           if        w-prs-snr-esp        not  = "S" and
                     w-prs-snr-esp        not  = "N"
                     move  "N"            to   w-prs-snr-esp          .
      *
           if        w-prs-snr-ric        not  = "S" and
                     w-prs-snr-ric        not  = "N"
                     move  "N"            to   w-prs-snr-ric          .
      *
           if        w-prs-snr-vcv        not  = "S" and
                     w-prs-snr-vcv        not  = "N"
                     move  "N"            to   w-prs-snr-vcv          .
       prs-snr-det-999.
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
      *    * Test di cancellabilita' per il modulo                     *
      *    *-----------------------------------------------------------*
       tcm-000.
      *              *-------------------------------------------------*
      *              * Se il contatore delle Open in corso per il mo-  *
      *              * dulo e' pari a zero si dichiara che e' cancel-  *
      *              * labile, altrimento che non lo e'                *
      *              *-------------------------------------------------*
           if        w-cnt-ctr-opn        =    zero
                     move  spaces         to   w-esp-rig-mmr-ope      .
       tcm-999.
           exit.

      *    *===========================================================*
      *    * Routine di Expand su righe bolla                          *
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
      *              * Lettura record [mmt]                            *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "DATREG    "         to   f-key                  .
           move      w-esp-rig-mmr-dat    to   rf-mmt-dat-reg         .
           move      w-esp-rig-mmr-prt    to   rf-mmt-num-prt         .
           move      "pgm/mag/fls/ioc/obj/iofmmt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mmt                 .
      *                  *---------------------------------------------*
      *                  * Se record non trovato : uscita              *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to esp-999.
       esp-100.
      *              *-------------------------------------------------*
      *              * Normalizzazione contatore righe                 *
      *              *-------------------------------------------------*
           move      zero                 to   w-aux-rec-mmr-crb      .
      *              *-------------------------------------------------*
      *              * Normalizzazione deviatore per 'find' su pro-    *
      *              * dotto                                           *
      *              *-------------------------------------------------*
           move      zero                 to   w-cnt-swc-fnd-pro      .
      *              *-------------------------------------------------*
      *              * Normalizzazione comodo di accetazione           *
      *              *-------------------------------------------------*
           move      spaces               to   w-wrk-fnd-pro-alf      .
       esp-110.
      *              *-------------------------------------------------*
      *              * Lettura righe record [mmr]                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Start                                       *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "DATREG    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      w-esp-rig-mmr-dat    to   rf-mmr-dat-reg         .
           move      w-esp-rig-mmr-prt    to   rf-mmr-num-prt         .
           move      zero                 to   rf-mmr-num-prg         .
           move      "pgm/mag/fls/ioc/obj/iofmmr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mmr                 .
      *                  *---------------------------------------------*
      *                  * Se errore di start : uscita                 *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to esp-999.
       esp-200.
      *              *-------------------------------------------------*
      *              * Lettura righe [mmr]                             *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmmr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mmr                 .
      *                  *---------------------------------------------*
      *                  * Se at end : a controllo contatore           *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to esp-500.
       esp-300.
      *              *-------------------------------------------------*
      *              * Se oltre il massimo : a controllo contatore     *
      *              *-------------------------------------------------*
           if        rf-mmr-num-prt       not  = w-esp-rig-mmr-prt
                     go to esp-500.
       esp-320.
      *              *-------------------------------------------------*
      *              * Selezioni sul record                            *
      *              *-------------------------------------------------*
       esp-400.
      *              *-------------------------------------------------*
      *              * Incremento numero records nel buffer            *
      *              *-------------------------------------------------*
           add       1                    to   w-aux-rec-mmr-crb      .
      *              *-------------------------------------------------*
      *              * Test se buffer oltre il numero previsto         *
      *              *-------------------------------------------------*
           if        w-aux-rec-mmr-crb    >    w-aux-rec-mmr-max
                     go to esp-500.
       esp-420.
      *              *-------------------------------------------------*
      *              * Bufferizzazione riga in corso di trattamento    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Data registrazione                          *
      *                  *---------------------------------------------*
           move      rf-mmr-dat-reg       to   w-aux-rec-mmr-dat
                                              (w-aux-rec-mmr-crb)     .
      *                  *---------------------------------------------*
      *                  * Numero protocollo                           *
      *                  *---------------------------------------------*
           move      rf-mmr-num-prt       to   w-aux-rec-mmr-prt
                                              (w-aux-rec-mmr-crb)     .
      *                  *---------------------------------------------*
      *                  * Numero progressivo                          *
      *                  *---------------------------------------------*
           move      rf-mmr-num-prg       to   w-aux-rec-mmr-prg
                                              (w-aux-rec-mmr-crb)     .
      *                  *---------------------------------------------*
      *                  * Tipo codice magazzino                       *
      *                  *---------------------------------------------*
           move      rf-mmr-tip-mag       to   w-aux-rec-mmr-tip
                                              (w-aux-rec-mmr-crb)     .
      *                  *---------------------------------------------*
      *                  * Codice alfanumerico magazzino               *
      *                  *---------------------------------------------*
           move      rf-mmr-alf-mag       to   w-aux-rec-mmr-alf
                                              (w-aux-rec-mmr-crb)     .
       esp-430.
      *              *-------------------------------------------------*
      *              * Riciclo a lettura                               *
      *              *-------------------------------------------------*
           go to     esp-200.
       esp-500.
      *              *-------------------------------------------------*
      *              * Controllo numero records letti con lo stesso    *
      *              * valore                                          *
      *              *-------------------------------------------------*
           if        w-aux-rec-mmr-crb    =    zero
                     go to esp-999.
      *                  *---------------------------------------------*
      *                  * Determinazione numero pagine nel buffer     *
      *                  *---------------------------------------------*
           move      w-aux-rec-mmr-crb    to   w-aux-rec-mmr-cpb      .
           subtract  1                    from w-aux-rec-mmr-cpb      .
           divide    6                    into w-aux-rec-mmr-cpb      .
           add       1                    to   w-aux-rec-mmr-cpb      .
      *                  *---------------------------------------------*
      *                  * Inizializzazione numero record nel buffer   *
      *                  * attualmente trattato                        *
      *                  *---------------------------------------------*
           move      1                    to   w-aux-rec-mmr-c01      .
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
      *              * Costruzione box di Expand per la visualizzazio- *
      *              * ne di tutte le righe del documento              *
      *              *-------------------------------------------------*
           move      "BX"                 to   v-ope                  .
           move      07                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      18                   to   v-lto                  .
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
           move      "N.R|     Codice      |               Descrizione  
      -              "             |   Quantita'  "
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
           move      "---+-----------------+----------------------------
      -              "-------------+--------------"
                                          to   v-alf                  .
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
           move      "   |                 |                            
      -              "             |              "
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
           move      "   |                 |                            
      -              "             |              "
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
           move      "   |                 |                            
      -              "             |              "
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
           move      "   |                 |                            
      -              "             |              "
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
           move      "   |                 |                            
      -              "             |              "
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
           move      "   |                 |                            
      -              "             |              "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Sottolineatura di chiusura                      *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      78                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      02                   to   v-pos                  .
           move      "---+-----------------+----------------------------
      -              "-------------+--------------"
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
           move      17                   to   v-lin                  .
           move      28                   to   v-pos                  .
           move      "Movimento :"        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Literal 'nr.'                               *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      40                   to   v-pos                  .
           move      "nr."                to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Numero protocollo                           *
      *                  *---------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      17                   to   v-lin                  .
           move      44                   to   v-pos                  .
           move      w-esp-rig-mmr-prt    to   w-num-prt-num-num      .
           move      w-num-prt-num-prg    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Literal 'del'                               *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      51                   to   v-pos                  .
           move      "del"                to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Data registrazione                          *
      *                  *---------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      17                   to   v-lin                  .
           move      55                   to   v-pos                  .
           move      w-esp-rig-mmr-dat    to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Literal 'causale'                           *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      09                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      64                   to   v-pos                  .
           move      "causale :"          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Codice causale                              *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      17                   to   v-lin                  .
           move      74                   to   v-pos                  .
           move      w-esp-rig-mmr-cau    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       esp-540.
      *              *-------------------------------------------------*
      *              * Visualizzazione pagina video contenente         *
      *              * il record attualmente trattato                  *
      *              *-------------------------------------------------*
           perform   esp-950              thru esp-969                .
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
           move      w-aux-rec-mmr-c01    to   w-aux-rec-mmr-nli      .
       esp-555.
           if        w-aux-rec-mmr-nli    >    6
                     subtract  6          from w-aux-rec-mmr-nli
                     go to esp-555.
      *                  *---------------------------------------------*
      *                  * Incremento numero linea a video per posi-   *
      *                  * zionamento verticale                        *
      *                  *---------------------------------------------*
           add       09                   to   w-aux-rec-mmr-nli      .
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
           if        w-esp-rig-mmr-cod    =    spaces
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
           move      w-esp-rig-mmr-cod    to   w-wrk-fnd-pro-alf      .
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
           if        w-aux-rec-mmr-c01    >    1
                     move  "UP  "         to   v-pfk (01)             .
      *
           if        w-aux-rec-mmr-c01    <    w-aux-rec-mmr-crb
                     move  "DOWN"         to   v-pfk (02)             .
      *
           if        w-aux-rec-mmr-tip
                    (w-aux-rec-mmr-c01)   =    01
                     move  "FIND"         to   v-pfk (03)             .
      *
           if        w-aux-rec-mmr-cpa    >    1
                     move  "PRSC"         to   v-pfk (07)             .
      *
           if        w-aux-rec-mmr-cpa    <    w-aux-rec-mmr-cpb
                     move  "NXSC"         to   v-pfk (08)             .
      *
           if        w-aux-rec-mmr-cpa    >    1
                     move  "BACK"         to   v-pfk (09)             .
      *
           if        w-aux-rec-mmr-cpa    <    w-aux-rec-mmr-cpb
                     move  "TAB "         to   v-pfk (10)             .
      *
           move      "EXPD"               to   v-pfk (15)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      w-aux-rec-mmr-nli    to   v-lin                  .
           move      08                   to   v-pos                  .
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
           else      go to esp-575.
       esp-582.
      *              *-------------------------------------------------*
      *              * Se Return                                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Inimmto                                     *
      *                  *---------------------------------------------*
           go to     esp-575.
       esp-584.
      *              *-------------------------------------------------*
      *              * Se Up                                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Decremento contatore                        *
      *                  *---------------------------------------------*
           subtract  1                    from w-aux-rec-mmr-c01      .
      *                  *---------------------------------------------*
      *                  * Normalizzazione deviatore per Expand        *
      *                  *---------------------------------------------*
           move      0                    to   w-cnt-swc-esp-gen      .
      *                  *---------------------------------------------*
      *                  * Controllo su linea assoluta a video : la    *
      *                  * prima                                       *
      *                  *---------------------------------------------*
           if        w-aux-rec-mmr-nli    =    10
                     go to esp-590
           else      go to esp-550.
       esp-586.
      *              *-------------------------------------------------*
      *              * Se Down                                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione deviatore per Expand        *
      *                  *---------------------------------------------*
           move      0                    to   w-cnt-swc-esp-gen      .
      *                  *---------------------------------------------*
      *                  * Eventuale incremento contatore              *
      *                  *---------------------------------------------*
           if        w-aux-rec-mmr-c01    <    w-aux-rec-mmr-crb
                     add   1              to   w-aux-rec-mmr-c01
                     go to esp-588
           else      go to esp-575.
       esp-588.
      *                  *---------------------------------------------*
      *                  * Controllo su linea assoluta a video :       *
      *                  * l'ultima                                    *
      *                  *---------------------------------------------*
           if        w-aux-rec-mmr-nli    =    15
                     go to esp-590
           else      go to esp-550.
       esp-590.
      *                  *---------------------------------------------*
      *                  * Visualizzazione pagina video contenente     *
      *                  * il record attualmente trattato              *
      *                  *---------------------------------------------*
           perform   esp-950              thru esp-969                .
      *                  *---------------------------------------------*
      *                  * A determinazione numero linea               *
      *                  *---------------------------------------------*
           go to     esp-550.
       esp-592.
      *              *-------------------------------------------------*
      *              * Se Next screen                                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione deviatore per Expand        *
      *                  *---------------------------------------------*
           move      0                    to   w-cnt-swc-esp-gen      .
      *                  *---------------------------------------------*
      *                  * Incremento del contatore per le pagine      *
      *                  *---------------------------------------------*
           add       1                    to   w-aux-rec-mmr-cpa      .
      *                  *---------------------------------------------*
      *                  * A trattamento contatore per le pagine       *
      *                  *---------------------------------------------*
           go to     esp-596.
       esp-594.
      *              *-------------------------------------------------*
      *              * Se Previous screen                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione deviatore per Expand        *
      *                  *---------------------------------------------*
           move      0                    to   w-cnt-swc-esp-gen      .
      *                  *---------------------------------------------*
      *                  * Decremento del contatore per le pagine      *
      *                  *---------------------------------------------*
           subtract  1                    from w-aux-rec-mmr-cpa      .
      *                  *---------------------------------------------*
      *                  * A trattamento contatore per le pagine       *
      *                  *---------------------------------------------*
           go to     esp-596.
       esp-596.
      *              *-------------------------------------------------*
      *              * Trattamento contatore per le pagine             *
      *              *-------------------------------------------------*
           move      w-aux-rec-mmr-cpa    to   w-aux-rec-mmr-c01      .
           multiply  6                    by   w-aux-rec-mmr-c01      .
           subtract  5                    from w-aux-rec-mmr-c01      .
           go to     esp-590.
       esp-598.
      *              *-------------------------------------------------*
      *              * Se Back                                         *
      *              *-------------------------------------------------*
           move      1                    to   w-aux-rec-mmr-cpa      .
           go to     esp-596.
       esp-600.
      *              *-------------------------------------------------*
      *              * Se Tab                                          *
      *              *-------------------------------------------------*
           move      w-aux-rec-mmr-cpb    to   w-aux-rec-mmr-cpa      .
           go to     esp-596.
       esp-602.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     esp-800.
       esp-606.
      *              *-------------------------------------------------*
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
      *              *-------------------------------------------------*
      *              * Se Find                                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Routine di ricerca per codice articolo      *
      *                  *---------------------------------------------*
           perform   esp-fnd-000          thru esp-fnd-999            .
      *                  *---------------------------------------------*
      *                  * Incremento deviatore per il 'find'          *
      *                  *---------------------------------------------*
           if        w-esp-rig-mmr-cod    not  = spaces
                     add   1              to   w-cnt-swc-fnd-pro      .
      *                  *---------------------------------------------*
      *                  * Test su esito accettazione                  *
      *                  *---------------------------------------------*
           if        w-aux-rec-mmr-cix    =    zero
                     go to esp-575.
      *                  *---------------------------------------------*
      *                  * Posizionamento alla pagina contenente la    *
      *                  * riga trovata                                *
      *                  *---------------------------------------------*
           move      w-aux-rec-mmr-cix    to   w-aux-rec-mmr-c01      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione pagina video contenente il  *
      *                  * record attualmente trattato                 *
      *                  *---------------------------------------------*
           perform   esp-950              thru esp-969                .
      *                  *---------------------------------------------*
      *                  * A visualizzazione della pagina contenente   *
      *                  * il record trovato                           *
      *                  *---------------------------------------------*
           go to     esp-550.
       esp-800.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     esp-999.
       esp-950.
      *              *=================================================*
      *              * Visualizzazione pagina video contenente il re-  *
      *              * cord attualmente trattato                       *
      *              *-------------------------------------------------*
           move      w-aux-rec-mmr-c01    to   w-aux-rec-mmr-c02      .
           add       5                    to   w-aux-rec-mmr-c02      .
           divide    6                    into w-aux-rec-mmr-c02      .
           move      w-aux-rec-mmr-c02    to   w-aux-rec-mmr-cpa      .
           subtract  1                    from w-aux-rec-mmr-c02      .
           multiply  6                    by   w-aux-rec-mmr-c02      .
           add       1                    to   w-aux-rec-mmr-c02      .
           add       5
                     w-aux-rec-mmr-c02  giving w-aux-rec-mmr-c03      .
           move      w-aux-rec-mmr-c03    to   w-aux-rec-mmr-c04      .
           if        w-aux-rec-mmr-c03    >    w-aux-rec-mmr-crb
                     move  w-aux-rec-mmr-crb
                                          to   w-aux-rec-mmr-c03      .
      *                  *---------------------------------------------*
      *                  * Posizionamento iniziale                     *
      *                  *---------------------------------------------*
           move      10                   to   w-aux-rec-mmr-c05      .
       esp-951.
      *                  *---------------------------------------------*
      *                  * Preparazione riga da visualizzare           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Lettura record [mmr]                    *
      *                      *-----------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "DATREG    "         to   f-key                  .
           move      w-aux-rec-mmr-dat
                    (w-aux-rec-mmr-c02)   to   rf-mmr-dat-reg         .
           move      w-aux-rec-mmr-prt
                    (w-aux-rec-mmr-c02)   to   rf-mmr-num-prt         .
           move      w-aux-rec-mmr-prg
                    (w-aux-rec-mmr-c02)   to   rf-mmr-num-prg         .
           move      "pgm/mag/fls/ioc/obj/iofmmr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mmr                 .
      *                      *-----------------------------------------*
      *                      * Editing Numero d'ordine riga            *
      *                      *-----------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      spaces               to   v-edm                  .
           move      w-aux-rec-mmr-c02    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-lin-imm-num-lin      .
      *                      *-----------------------------------------*
      *                      * Separatore 1                            *
      *                      *-----------------------------------------*
           move      "|"                  to   w-lin-imm-seg-tr1      .
      *                      *-----------------------------------------*
      *                      * Spazio vuoto 1                          *
      *                      *-----------------------------------------*
           move      " "                  to   w-lin-imm-seg-bn1      .
      *                      *-----------------------------------------*
      *                      * Tipo magazzino                          *
      *                      *-----------------------------------------*
           if        w-prs-dps-snx        not  = "S" and
                     w-prs-dpm-snx        not  = "S" and
                     w-prs-mtv-snx        not  = "S"
                     move  spaces         to   w-lin-imm-tip-rig
           else if   rf-mmr-tip-mag       =    01
                     move  "P/"           to   w-lin-imm-tip-rig
           else if   rf-mmr-tip-mag       =    02
                     move  "S/"           to   w-lin-imm-tip-rig
           else if   rf-mmr-tip-mag       =    03
                     move  "M/"           to   w-lin-imm-tip-rig
           else if   rf-mmr-tip-mag       =    04
                     move  "V/"           to   w-lin-imm-tip-rig
           else      move  "?/"           to   w-lin-imm-tip-rig      .
      *                      *-----------------------------------------*
      *                      * Codice magazzino                        *
      *                      *-----------------------------------------*
           move      rf-mmr-alf-mag       to   w-lin-imm-alf-rig      .
      *                      *-----------------------------------------*
      *                      * Separatore 2                            *
      *                      *-----------------------------------------*
           move      "|"                  to   w-lin-imm-seg-tr2      .
       esp-952.
      *                      *-----------------------------------------*
      *                      * Descrizione per la riga                 *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Normalizzazione preliminare         *
      *                          *-------------------------------------*
           move      spaces               to   w-lin-imm-des-rig      .
      *                          *-------------------------------------*
      *                          * Deviazione in funzione del tipo di  *
      *                          * magazzino                           *
      *                          *-------------------------------------*
           if        rf-mmr-tip-mag       =    01
                     go to esp-954
           else if   rf-mmr-tip-mag       =    02
                     go to esp-956
           else if   rf-mmr-tip-mag       =    03
                     go to esp-958
           else if   rf-mmr-tip-mag       =    04
                     go to esp-960.
       esp-954.
      *                          *-------------------------------------*
      *                          * Prodotti di vendita                 *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Lettura                         *
      *                              *---------------------------------*
           move      rf-mmr-num-mag       to   w-let-arc-dcp-num      .
           perform   let-arc-dcp-000      thru let-arc-dcp-999        .
           move      w-let-arc-dcp-alf    to   w-wrk-des-mag-wca      .
           move      w-let-arc-dcp-des    to   w-wrk-des-mag-wde      .
           move      w-let-arc-dcp-deq    to   w-wrk-des-mag-wdq      .
      *                              *---------------------------------*
      *                              * Oltre                           *
      *                              *---------------------------------*
           go to     esp-964.
       esp-956.
      *                          *-------------------------------------*
      *                          * Semilavorati                        *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Test se gestione attiva         *
      *                              *---------------------------------*
           if        w-prs-dps-snx        not  = "S"
                     move  spaces         to   w-wrk-des-mag-wca
                     move  spaces         to   w-wrk-des-mag-wde
                     move  zero           to   w-wrk-des-mag-wdq
                     go to esp-964.
      *                              *---------------------------------*
      *                              * Lettura                         *
      *                              *---------------------------------*
           move      rf-mmr-num-mag       to   w-let-arc-dps-num      .
           perform   let-arc-dps-000      thru let-arc-dps-999        .
           move      w-let-arc-dps-alf    to   w-wrk-des-mag-wca      .
           move      w-let-arc-dps-des    to   w-wrk-des-mag-wde      .
           move      w-let-arc-dps-deq    to   w-wrk-des-mag-wdq      .
      *                              *---------------------------------*
      *                              * Oltre                           *
      *                              *---------------------------------*
           go to     esp-964.
       esp-958.
      *                          *-------------------------------------*
      *                          * Materie prime                       *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Test se gestione attiva         *
      *                              *---------------------------------*
           if        w-prs-dpm-snx        not  = "S"
                     move  spaces         to   w-wrk-des-mag-wca
                     move  spaces         to   w-wrk-des-mag-wde
                     move  zero           to   w-wrk-des-mag-wdq
                     go to esp-964.
      *                              *---------------------------------*
      *                              * Lettura                         *
      *                              *---------------------------------*
           move      rf-mmr-num-mag       to   w-let-arc-dpm-num      .
           perform   let-arc-dpm-000      thru let-arc-dpm-999        .
           move      w-let-arc-dpm-alf    to   w-wrk-des-mag-wca      .
           move      w-let-arc-dpm-des    to   w-wrk-des-mag-wde      .
           move      w-let-arc-dpm-deq    to   w-wrk-des-mag-wdq      .
      *                              *---------------------------------*
      *                              * Oltre                           *
      *                              *---------------------------------*
           go to     esp-964.
       esp-960.
      *                          *-------------------------------------*
      *                          * Materiali vari                      *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Test se gestione attiva         *
      *                              *---------------------------------*
           if        w-prs-mtv-snx        not  = "S"
                     move  spaces         to   w-wrk-des-mag-wca
                     move  spaces         to   w-wrk-des-mag-wde
                     move  zero           to   w-wrk-des-mag-wdq
                     go to esp-964.
      *                              *---------------------------------*
      *                              * Lettura                         *
      *                              *---------------------------------*
           move      rf-mmr-num-mag       to   w-let-arc-mtv-num      .
           perform   let-arc-mtv-000      thru let-arc-mtv-999        .
           move      w-let-arc-mtv-alf    to   w-wrk-des-mag-wca      .
           move      w-let-arc-mtv-des    to   w-wrk-des-mag-wde      .
           move      w-let-arc-mtv-deq    to   w-wrk-des-mag-wdq      .
      *                              *---------------------------------*
      *                              * Oltre                           *
      *                              *---------------------------------*
           go to     esp-964.
       esp-964.
      *                          *-------------------------------------*
      *                          * Descrizione determinata             *
      *                          *-------------------------------------*
           move      w-wrk-des-mag-wde    to   w-lin-imm-des-rig      .
       esp-966.
      *                      *-----------------------------------------*
      *                      * Separatore 3                            *
      *                      *-----------------------------------------*
           move      "|"                  to   w-lin-imm-seg-tr3      .
      *                      *-----------------------------------------*
      *                      * Editing quantita'                       *
      *                      *-----------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      w-wrk-des-mag-wdq    to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "BGD"                to   v-edm                  .
           move      rf-mmr-qta-mov       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-lin-imm-qta-rig      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione riga                        *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      78                   to   v-car                  .
           move      w-aux-rec-mmr-c05    to   v-lin                  .
           move      02                   to   v-pos                  .
           move      w-lin-imm-dsp        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Incremento contatori                        *
      *                  *---------------------------------------------*
           add       1                    to   w-aux-rec-mmr-c02      .
           add       1                    to   w-aux-rec-mmr-c05      .
           if        w-aux-rec-mmr-c02    not  > w-aux-rec-mmr-c03
                     go to esp-951.
       esp-967.
           if        w-aux-rec-mmr-c02    >    w-aux-rec-mmr-c04
                     go to esp-968.
           if        w-aux-rec-mmr-crb    not  > 6
                     go to esp-968.
      *                  *---------------------------------------------*
      *                  * Riga vuota con separatori                   *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      78                   to   v-car                  .
           move      w-aux-rec-mmr-c05    to   v-lin                  .
           move      02                   to   v-pos                  .
           move      "   |                 |                            
      -              "             |              "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Incremento contatori                        *
      *                  *---------------------------------------------*
           add       1                    to   w-aux-rec-mmr-c02      .
           add       1                    to   w-aux-rec-mmr-c05      .
           go to    esp-967.
       esp-968.
      *                  *---------------------------------------------*
      *                  * Literal 'pagina'                            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Editing 'numero pagina'                 *
      *                      *-----------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      w-aux-rec-mmr-cpa    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-aux-rec-mmr-lt1      .
      *                      *-----------------------------------------*
      *                      * Editing 'di pagine'                     *
      *                      *-----------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      w-aux-rec-mmr-cpb    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-aux-rec-mmr-lt2      .
      *                      *-----------------------------------------*
      *                      * String                                  *
      *                      *-----------------------------------------*
           move      spaces               to   w-aux-rec-mmr-ltp      .
           string    "Pagina "  delimited by   size
                     w-aux-rec-mmr-lt1
                                delimited by   spaces
                     " di "     delimited by   size
                     w-aux-rec-mmr-lt2
                                delimited by   spaces
                                          into w-aux-rec-mmr-ltp      .
      *                      *-----------------------------------------*
      *                      * Visualizzazione stringa composta        *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      17                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      w-aux-rec-mmr-ltp    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       esp-969.
           exit.
       esp-999.
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
           move      12                   to   v-lto                  .
           move      80                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       esp-rig-100.
      *              *-------------------------------------------------*
      *              * Lettura riga per espansione                     *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "DATREG    "         to   f-key                  .
           move      w-aux-rec-mmr-dat
                    (w-aux-rec-mmr-c01)   to   rf-mmr-dat-reg         .
           move      w-aux-rec-mmr-prt
                    (w-aux-rec-mmr-c01)   to   rf-mmr-num-prt         .
           move      w-aux-rec-mmr-prg
                    (w-aux-rec-mmr-c01)   to   rf-mmr-num-prg         .
           move      "pgm/mag/fls/ioc/obj/iofmmr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mmr                 .
       esp-rig-200.
      *              *-------------------------------------------------*
      *              * Visualizzazione prompts per riga corpo espansa  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Tipo codice                                 *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      19                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      "Tipo codice       :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Codice prodotto                             *
      *                  *---------------------------------------------*
           perform   pmt-cod-pro-000      thru pmt-cod-pro-999        .
      *                  *---------------------------------------------*
      *                  * Quantita'                                   *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      19                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      "Quantita'         :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Test su personalizzazione                   *
      *                  *---------------------------------------------*
           if        w-prs-snr-vcv        not  = "S"
                     go to esp-rig-300.
      *                  *---------------------------------------------*
      *                  * Costo unitario                              *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      19                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      "Costo unitario    :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Valore                                      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      19                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      "Valore            :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       esp-rig-300.
       esp-rig-400.
      *              *-------------------------------------------------*
      *              * Visualizzazione campi per riga corpo espansa    *
      *              *-------------------------------------------------*
       esp-rig-420.
      *                  *---------------------------------------------*
      *                  * Tipo codice                                 *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      23                   to   v-pos                  .
           if        rf-mmr-tip-mag       =    01
                     move  "P"            to   v-alf
           else if   rf-mmr-tip-mag       =    02
                     move  "S"            to   v-alf
           else if   rf-mmr-tip-mag       =    03
                     move  "M"            to   v-alf
           else if   rf-mmr-tip-mag       =    04
                     move  "V"            to   v-alf
           else      move  spaces         to   v-alf                  .
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
           move      w-aux-rec-mmr-c01    to   v-num                  .
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
      *                  * Descrizione                                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Determinazione                          *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Deviazione in funzione del tipo di  *
      *                          * magazzino                           *
      *                          *-------------------------------------*
           if        rf-mmr-tip-mag       =    01
                     go to esp-rig-482
           else if   rf-mmr-tip-mag       =    02
                     go to esp-rig-484
           else if   rf-mmr-tip-mag       =    03
                     go to esp-rig-486
           else if   rf-mmr-tip-mag       =    04
                     go to esp-rig-488.
       esp-rig-482.
      *                          *-------------------------------------*
      *                          * Prodotti di vendita                 *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Lettura                         *
      *                              *---------------------------------*
           move      rf-mmr-num-mag       to   w-let-arc-dcp-num      .
           perform   let-arc-dcp-000      thru let-arc-dcp-999        .
           move      w-let-arc-dcp-alf    to   w-wrk-des-mag-wca      .
           move      w-let-arc-dcp-des    to   w-wrk-des-mag-wde      .
           move      w-let-arc-dcp-deq    to   w-wrk-des-mag-wdq      .
      *                              *---------------------------------*
      *                              * Oltre                           *
      *                              *---------------------------------*
           go to     esp-rig-490.
       esp-rig-484.
      *                          *-------------------------------------*
      *                          * Semilavorati                        *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Test se gestione attiva         *
      *                              *---------------------------------*
           if        w-prs-dps-snx        not  = "S"
                     move  spaces         to   w-wrk-des-mag-wca
                     move  spaces         to   w-wrk-des-mag-wde
                     move  zero           to   w-wrk-des-mag-wdq
                     go to esp-rig-490.
      *                              *---------------------------------*
      *                              * Lettura                         *
      *                              *---------------------------------*
           move      rf-mmr-num-mag       to   w-let-arc-dps-num      .
           perform   let-arc-dps-000      thru let-arc-dps-999        .
           move      w-let-arc-dps-alf    to   w-wrk-des-mag-wca      .
           move      w-let-arc-dps-des    to   w-wrk-des-mag-wde      .
           move      w-let-arc-dps-deq    to   w-wrk-des-mag-wdq      .
      *                              *---------------------------------*
      *                              * Oltre                           *
      *                              *---------------------------------*
           go to     esp-rig-490.
       esp-rig-486.
      *                          *-------------------------------------*
      *                          * Materie prime                       *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Test se gestione attiva         *
      *                              *---------------------------------*
           if        w-prs-dpm-snx        not  = "S"
                     move  spaces         to   w-wrk-des-mag-wca
                     move  spaces         to   w-wrk-des-mag-wde
                     move  zero           to   w-wrk-des-mag-wdq
                     go to esp-rig-490.
      *                              *---------------------------------*
      *                              * Lettura                         *
      *                              *---------------------------------*
           move      rf-mmr-num-mag       to   w-let-arc-dpm-num      .
           perform   let-arc-dpm-000      thru let-arc-dpm-999        .
           move      w-let-arc-dpm-alf    to   w-wrk-des-mag-wca      .
           move      w-let-arc-dpm-des    to   w-wrk-des-mag-wde      .
           move      w-let-arc-dpm-deq    to   w-wrk-des-mag-wdq      .
      *                              *---------------------------------*
      *                              * Oltre                           *
      *                              *---------------------------------*
           go to     esp-rig-490.
       esp-rig-488.
      *                          *-------------------------------------*
      *                          * Materiali vari                      *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Test se gestione attiva         *
      *                              *---------------------------------*
           if        w-prs-mtv-snx        not  = "S"
                     move  spaces         to   w-wrk-des-mag-wca
                     move  spaces         to   w-wrk-des-mag-wde
                     move  zero           to   w-wrk-des-mag-wdq
                     go to esp-rig-490.
      *                              *---------------------------------*
      *                              * Lettura                         *
      *                              *---------------------------------*
           move      rf-mmr-num-mag       to   w-let-arc-mtv-num      .
           perform   let-arc-mtv-000      thru let-arc-mtv-999        .
           move      w-let-arc-mtv-alf    to   w-wrk-des-mag-wca      .
           move      w-let-arc-mtv-des    to   w-wrk-des-mag-wde      .
           move      w-let-arc-mtv-deq    to   w-wrk-des-mag-wdq      .
      *                              *---------------------------------*
      *                              * Oltre                           *
      *                              *---------------------------------*
           go to     esp-rig-490.
       esp-rig-490.
      *                          *-------------------------------------*
      *                          * Descrizione determinata             *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      38                   to   v-pos                  .
           move      w-wrk-des-mag-wde    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       esp-rig-500.
      *                  *---------------------------------------------*
      *                  * Unita' di misura                            *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      17                   to   v-pos                  .
           move      rf-mmr-udm-mov       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Quantita'                                   *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      03                   to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "<BD"                to   v-edm                  .
           move      09                   to   v-lin                  .
           move      23                   to   v-pos                  .
           move      rf-mmr-qta-mov       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Test su personalizzazione                   *
      *                  *---------------------------------------------*
           if        w-prs-snr-vcv        not  = "S"
                     go to esp-rig-800.
      *                  *---------------------------------------------*
      *                  * Costo unitario                              *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      09                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           add       02                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BG"                to   v-edm                  .
           move      10                   to   v-lin                  .
           move      23                   to   v-pos                  .
           move      rf-mmr-cun-mov       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Valore                                      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BG"                to   v-edm                  .
           move      11                   to   v-lin                  .
           move      23                   to   v-pos                  .
           move      rf-mmr-val-mov       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
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
           move      11                   to   v-lin                  .
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
      *    * Visualizzazione prompts per espansione riga : codice pro- *
      *    * dotto                                                     *
      *    *-----------------------------------------------------------*
       pmt-cod-pro-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      19                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      03                   to   v-pos                  .
      *
           if        rf-mmr-tip-mag       =    01
                     move  "Codice prodotto   :"
                                          to   v-alf
           else if   rf-mmr-tip-mag       =    02
                     move  "Semilavorato      :"
                                          to   v-alf
           else if   rf-mmr-tip-mag       =    03
                     move  "Materia prima     :"
                                          to   v-alf
           else if   rf-mmr-tip-mag       =    04
                     move  "Materiale vario   :"
                                          to   v-alf
           else      move  "Codice magazzino  :"
                                          to   v-alf                  .
      *
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
           move      rf-mmr-alf-mag       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-pro-999.
           exit.

      *    *===========================================================*
      *    * Routine lettura archivio [dcp] e [pdx]                    *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/ldcppdx0.lts"                   .

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
           move      zero                 to   w-aux-rec-mmr-cix      .
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
           move      w-aux-rec-mmr-c01    to   w-aux-rec-mmr-c06      .
           move      zero                 to   w-aux-rec-mmr-cix      .
       esp-fnd-ric-200.
      *              *-------------------------------------------------*
      *              * Test max                                        *
      *              *-------------------------------------------------*
           add       1                    to   w-aux-rec-mmr-c06      .
           if        w-aux-rec-mmr-c06    >    w-aux-rec-mmr-crb or
                     w-aux-rec-mmr-c06    >    w-aux-rec-mmr-max
                     go to esp-fnd-ric-900.
      *              *-------------------------------------------------*
      *              * Test di confronto                               *
      *              *-------------------------------------------------*
           if        w-wrk-fnd-pro-alf    =    w-aux-rec-mmr-alf
                                              (w-aux-rec-mmr-c06)
                     move  w-aux-rec-mmr-c06
                                          to   w-aux-rec-mmr-cix
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
      *              * Test se codice a zero                           *
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
           move      spaces               to   w-let-arc-dcp-umi      .
           move      zero                 to   w-let-arc-dcp-deq      .
       let-arc-dcp-999.
           exit.

      *    *===========================================================*
      *    * Routine lettura archivio [dps]                            *
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
           move      "NUMSEM    "         to   f-key                  .
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
      *    * Routine lettura archivio [dpm]                            *
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
           move      "NUMMAP    "         to   f-key                  .
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
      *    * Routine lettura archivio [mtv]                            *
      *    *-----------------------------------------------------------*
       let-arc-mtv-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-mtv-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice a zero                           *
      *              *-------------------------------------------------*
           if        w-let-arc-mtv-num    =    zero
                     go to let-arc-mtv-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMMTV    "         to   f-key                  .
           move      w-let-arc-mtv-num    to   rf-mtv-num-mtv         .
           move      "pgm/mtv/fls/ioc/obj/iofmtv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mtv                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-mtv-400.
       let-arc-mtv-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-mtv-alf-mtv       to   w-let-arc-mtv-alf      .
           move      rf-mtv-des-mtv       to   w-let-arc-mtv-des      .
           move      rf-mtv-umi-gst       to   w-let-arc-mtv-umi      .
           move      rf-mtv-dec-qta       to   w-let-arc-mtv-deq      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-mtv-999.
       let-arc-mtv-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-mtv-flg      .
           move      all   "."            to   w-let-arc-mtv-des      .
           go to     let-arc-mtv-600.
       let-arc-mtv-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-mtv-des      .
       let-arc-mtv-600.
           move      spaces               to   w-let-arc-mtv-alf      .
           move      spaces               to   w-let-arc-mtv-umi      .
           move      zero                 to   w-let-arc-mtv-deq      .
       let-arc-mtv-999.
           exit.

      *    *===========================================================*
      *    * Subroutines per accettazione codice prodotto 'dcp'        *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/acoddcp0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per allineamenti a destra o a sinistra oppure *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cps"                   .

